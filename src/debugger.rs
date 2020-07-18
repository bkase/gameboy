use std::borrow::Cow::{self, Borrowed, Owned};

use expr;
use hardware::{self, Hardware};
use headless;
use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::config::OutputStreamType;
use rustyline::error::ReadlineError;
use rustyline::highlight::{Highlighter, MatchingBracketHighlighter};
use rustyline::hint::{Hinter, HistoryHinter};
use rustyline::validate::{self, MatchingBracketValidator, Validator};
use rustyline::{Cmd, CompletionType, Config, Context, EditMode, Editor, Helper, KeyPress};
use shlex;
use std::env;
use std::iter::Extend;
use std::iter::Iterator;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread::sleep;
use std::time::Duration;
use structopt::StructOpt;
use these::These;
use web_utils::Performance;

/*
 * INFO:
 * print [-s --screen | -t --tiles | <expr>]
 * watch [-c --condition <expr> | --name <string>] <expr>
 * backtrace
 * break
 *
 * MUTATE:
 * run <file>
 * break [-d --delete | --name <string>] [$<addr> | (<expr>) | <symbol>]
 * continue
 * step-over
 * step-into
 *
 * EXPR:
 * literal := $[0-9]+ | 0x[0-9]+ | [0-9]+ (u16)
 * binop := =, !=, <, >, +, -, *, >>, <<, &, |
 * registers := A B C D E H L SP PC AF BC DE HL
 * memory load := [ <register> | <literal> ]
 */

mod break_expr {
    use expr;
    use expr::{lit, sp, In};

    use nom::{
        branch::alt,
        bytes::complete::{tag, take_while},
        character::is_space,
        combinator::{all_consuming, map},
        error::{ParseError, VerboseError},
        sequence::{delimited, preceded},
        IResult,
    };

    use mem::Addr;

    #[derive(Debug, Clone)]
    pub enum E {
        Lit(Addr),
        Conditional(expr::E),
        Symbolic(String),
    }

    fn conditional<'a, Er: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, expr::E, Er> {
        delimited(
            preceded(sp, tag("(")),
            preceded(sp, map(expr::expr0, |e0| expr::E(e0))),
            preceded(sp, tag(")")),
        )(input)
    }

    fn symbolic<'a, Er: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, &str, Er> {
        take_while(|c| !is_space(c as u8))(input)
    }

    fn expr<'a, Er: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, E, Er> {
        alt((
            map(lit, |addr| E::Lit(Addr::directly(addr))),
            map(conditional, |e| E::Conditional(e)),
            map(symbolic, |s| E::Symbolic(s.to_string())),
        ))(input)
    }

    fn root<'a, Er: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, E, Er> {
        all_consuming(expr)(input)
    }

    impl std::str::FromStr for E {
        type Err = std::string::String;

        fn from_str(src: &str) -> Result<E, Self::Err> {
            root::<VerboseError<&str>>(src)
                .map(|(_, e)| e)
                .map_err(|e| match e {
                    nom::Err::Error(e) => nom::error::convert_error(src, e),
                    nom::Err::Failure(e) => nom::error::convert_error(src, e),
                    nom::Err::Incomplete(_) => "Incomplete".to_string(),
                })
        }
    }
}

#[derive(Debug, StructOpt)]
#[structopt(about = "Debugger line for the gameboy")]
enum Line {
    Print {
        #[structopt(short, long)]
        screen: bool,
        #[structopt(short, long)]
        tiles: bool,
        #[structopt(parse(try_from_str))]
        expr: Option<expr::E>,
    },
    /// Print the result of the expression after every command
    Watch {
        #[structopt(short, long)]
        condition: Option<String>,
        #[structopt(short, long)]
        name: Option<String>,
        #[structopt(parse(try_from_str))]
        expr: Option<expr::E>,
    },
    Backtrace,
    /// Set or print breakpoints
    Break {
        #[structopt(short, long)]
        delete: bool,
        #[structopt(short, long)]
        name: Option<String>,
        /// Args are $<addr> | (<expr>) | <symbol>
        #[structopt(name = "ARG")]
        arg: Option<break_expr::E>,
    },
    /// Continue running
    Continue,
    /// Step-over
    StepOver,
    /// Step-into
    StepInto,
    /// Disassemble
    Disassemble,
}

struct MyHelper {
    completer: FilenameCompleter,
    highlighter: MatchingBracketHighlighter,
    validator: MatchingBracketValidator,
    hinter: HistoryHinter,
    colored_prompt: String,
}
impl Helper for MyHelper {}

impl Completer for MyHelper {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        ctx: &Context<'_>,
    ) -> Result<(usize, Vec<Pair>), ReadlineError> {
        self.completer.complete(line, pos, ctx)
    }
}

impl Hinter for MyHelper {
    fn hint(&self, line: &str, pos: usize, ctx: &Context<'_>) -> Option<String> {
        self.hinter.hint(line, pos, ctx)
    }
}

impl Highlighter for MyHelper {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> Cow<'b, str> {
        if default {
            Borrowed(&self.colored_prompt)
        } else {
            Borrowed(prompt)
        }
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Owned("\x1b[1m".to_owned() + hint + "\x1b[m")
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        self.highlighter.highlight_char(line, pos)
    }
}

impl Validator for MyHelper {
    fn validate(
        &self,
        ctx: &mut validate::ValidationContext,
    ) -> rustyline::Result<validate::ValidationResult> {
        self.validator.validate(ctx)
    }

    fn validate_while_typing(&self) -> bool {
        self.validator.validate_while_typing()
    }
}

fn exec_until_interrupt(
    performance: &Performance,
    hardware: &mut Hardware,
    running: Arc<AtomicBool>,
) {
    // timing
    let start = performance.now();
    let mut last = start;

    hardware.paused = false;
    running.store(true, Ordering::SeqCst);
    loop {
        if running.load(Ordering::SeqCst) {
            let now = performance.now();
            let diff = now - last;
            last = now;

            hardware.run(diff);
            if hardware.paused {
                break;
            }

            let now_ = performance.now();
            sleep(Duration::from_micros(
                16_667 - u64::min(16_667, ((now_ - now) * 1000.) as u64),
            ))
        } else {
            break;
        }
    }
}

fn icat(img: &Path) {
    let mut process = Command::new("kitty")
        .arg("+kitten")
        .arg("icat")
        .arg(img.to_str().unwrap())
        .spawn();
    match process.iter_mut().next() {
        Some(p) => {
            let _ = p.wait();
        }
        None => (),
    };
}

fn cmd(performance: &Performance, hardware: &mut Hardware, running: Arc<AtomicBool>, line: Line) {
    use self::expr::Eval;

    match line {
        Line::Print {
            screen,
            tiles,
            expr,
        } => {
            if screen {
                let mut dir = env::temp_dir();
                dir.push("screenshot.png");
                let path = Path::new(&dir);
                headless::save_screenshot(&path, &hardware.ppu.screen);
                icat(&path);
            }
            if tiles {
                println!("TODO: Dump tiles");
            }

            match expr {
                Some(e) => {
                    let res: u16 = e.eval(hardware);
                    println!("  ${:x}", res)
                }
                None => (),
            }
        }
        Line::Continue => exec_until_interrupt(performance, hardware, running),
        Line::StepInto => hardware.step(),
        Line::Disassemble => {
            let mut ip1 = hardware.cpu.ip.clone();
            let mut ip2 = hardware.cpu.ip.clone();

            let mut forwards_runner = ip1.instrs_forwards(&hardware.cpu.memory);
            let backwards_runnner = ip2.instrs_backwards(&hardware.cpu.memory);

            let (curr, _, curr_addr) = forwards_runner.next().unwrap();

            // TODO: Clean up this iterator mess
            //
            let mut first_few: Vec<String> = backwards_runnner
                .take(5)
                .map(|(i, _, addr)| format!("    {:}: {:}", addr, i))
                .collect::<Vec<String>>()
                .iter()
                .rev()
                .map(|s| s.clone())
                .collect::<Vec<String>>();

            first_few.push(format!(" => {:}: {:}", curr_addr, curr));

            first_few
                .iter()
                .map(|s| s.clone())
                .chain(
                    forwards_runner
                        .take(5)
                        .map(|(i, _, addr)| format!("    {:}: {:}", addr, i)),
                )
                .for_each(|s| {
                    println!("{:}", s);
                })
        }
        Line::Break { delete, name, arg } => {
            if delete {
                println!("Delete unsupported for now")
            } else {
                match arg {
                    Some(break_expr::E::Lit(addr)) => {
                        let value = name.unwrap_or_else(|| format!("Lit {:}", addr));
                        hardware.breakpoints.insert(addr, value);
                    }
                    Some(break_expr::E::Conditional(expr)) => {
                        let key = name.unwrap_or_else(|| format!("Computed {:?}", expr));
                        hardware.computed_breakpoints.insert(key, expr);
                    }
                    Some(break_expr::E::Symbolic(s)) => {
                        println!("Symbolic breakpoints unsupported for now");
                    }
                    None => {
                        println!("Simple Breakpoints:");
                        for (k, name) in &hardware.breakpoints {
                            println!("{:}: {:}", name, k)
                        }
                        println!("Computed Breakpoints:");
                        for (name, e) in &hardware.computed_breakpoints {
                            println!("{:}: {:?}", name, e)
                        }
                    }
                }
            }
        }
        _ => println!("Line: {:?}", line),
    }
}

pub fn run(rom: &PathBuf) {
    let running = Arc::new(AtomicBool::new(true));
    let r = running.clone();

    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .edit_mode(EditMode::Vi)
        .output_stream(OutputStreamType::Stdout)
        .build();
    let h = MyHelper {
        completer: FilenameCompleter::new(),
        highlighter: MatchingBracketHighlighter::new(),
        hinter: HistoryHinter {},
        colored_prompt: "".to_owned(),
        validator: MatchingBracketValidator::new(),
    };
    let mut rl = Editor::with_config(config);
    rl.set_helper(Some(h));
    rl.bind_sequence(KeyPress::Ctrl('R'), Cmd::ReverseSearchHistory);
    rl.bind_sequence(KeyPress::Tab, Cmd::CompleteHint);

    let performance = Performance::create();

    let cartridge = headless::read_cartridge(rom);

    let mut hardware = Hardware::create(
        &performance,
        hardware::Config {
            trace: false,
            roms: These::That(Cow::Owned(cartridge)),
        },
    );
    hardware.paused = false;

    ctrlc::set_handler(move || {
        println!("Interrupted 1");
        r.store(false, Ordering::SeqCst);
    })
    .expect("Error setting Ctrl-C handler");

    let mut count = 0;
    loop {
        let p = format!("{}> ", count);
        rl.helper_mut().expect("No helper").colored_prompt = format!("\x1b[1;32m{}\x1b[0m", p);
        let readline = rl.readline(&p);

        // let readline = rl.readline("(debug) \x1b[1m𝝺 \x1b[0m");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match shlex::split(line.as_str()) {
                    None => println!(""),
                    Some(ys) => {
                        let mut xs = vec!["gameboy-debugger".to_string()];
                        xs.extend(ys);
                        let l = Line::from_iter_safe(xs);
                        match l {
                            Ok(line) => {
                                cmd(&performance, &mut hardware, running.clone(), line);
                            }
                            Err(e) => println!("Line: {:}", e),
                        }
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("Interrupted");
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
        count += 1;
    }
}
