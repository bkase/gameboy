use std::borrow::Cow::{self, Borrowed, Owned};

use hardware::{self, Hardware};
use headless;
use instr::Instr;
use instr::InstrPointer;
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
use std::sync::mpsc;
use std::sync::mpsc::TryRecvError;
use std::sync::{Arc, Mutex};
use std::thread;
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
 * break [-d --delete | --name <string>] $<addr> | (<expr>) | <symbol>
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

mod expr {
    use register_kind::{RegisterKind16, RegisterKind8};

    #[derive(Debug, Clone, Copy)]
    pub enum Op {
        Equals,
        NotEquals,
        LessThan,
        GreaterThan,
        Plus,
        Minus,
        Times,
        RightShift,
        LeftShift,
        BitAnd,
        BitOr,
    }

    #[derive(Debug, Clone)]
    pub enum Loadable {
        Lit(u16),
        Reg16(RegisterKind16),
    }

    #[derive(Debug, Clone)]
    pub enum Term {
        Reg8(RegisterKind8),
        Load(Loadable),
        Loadable(Loadable),
    }

    #[derive(Debug, Clone)]
    pub struct E0 {
        term: Term,
        e1: Box<E1>,
    }

    #[derive(Debug, Clone)]
    pub enum E1 {
        Epsilon,
        Binop(Op, Box<E0>, Box<E1>),
    }

    use nom::{
        branch::alt,
        bytes::complete::{tag, tag_no_case, take, take_while, take_while1},
        character::{is_digit, is_hex_digit, is_space},
        combinator::{all_consuming, map, map_res, opt},
        error::{ParseError, VerboseError},
        multi::count,
        sequence::{delimited, pair, preceded, terminated, tuple},
        IResult,
    };

    fn from_hex8(input: &str) -> Result<u8, std::num::ParseIntError> {
        u8::from_str_radix(input, 16)
    }

    fn from_hex16(input: &str) -> Result<u16, std::num::ParseIntError> {
        u16::from_str_radix(input, 16)
    }

    fn from_decimal16(input: &str) -> Result<u16, std::num::ParseIntError> {
        u16::from_str_radix(input, 10)
    }

    type In<'a> = &'a str;

    fn sp<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, In<'a>, E> {
        take_while(|c| is_space(c as u8))(input)
    }

    fn op<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, Op, E> {
        preceded(
            sp,
            alt((
                map(tag("="), |_: &str| Op::Equals),
                map(tag("!="), |_: &str| Op::NotEquals),
                map(tag("<"), |_: &str| Op::LessThan),
                map(tag(">"), |_: &str| Op::GreaterThan),
                map(tag("+"), |_: &str| Op::Plus),
                map(tag("-"), |_: &str| Op::Minus),
                map(tag("*"), |_: &str| Op::Times),
                map(tag(">>"), |_: &str| Op::RightShift),
                map(tag("<<"), |_: &str| Op::LeftShift),
                map(tag("&"), |_: &str| Op::BitAnd),
                map(tag("|"), |_: &str| Op::BitOr),
            )),
        )(input)
    }

    fn lit<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, u16, E> {
        preceded(
            sp,
            alt((
                preceded(
                    tag("$"),
                    map_res(take_while1(|c| is_hex_digit(c as u8)), from_hex16),
                ),
                preceded(
                    tag("0x"),
                    map_res(take_while1(|c| is_hex_digit(c as u8)), from_hex16),
                ),
                map_res(take_while1(|c| is_digit(c as u8)), from_decimal16),
            )),
        )(input)
    }

    fn reg16<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, RegisterKind16, E> {
        preceded(
            sp,
            alt((
                map(tag("BC"), |_: &str| RegisterKind16::Bc),
                map(tag("DE"), |_: &str| RegisterKind16::De),
                map(tag("HL"), |_: &str| RegisterKind16::Hl),
                map(tag("SP"), |_: &str| RegisterKind16::Sp),
            )),
        )(input)
    }

    fn loadable<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, Loadable, E> {
        preceded(
            sp,
            alt((
                map(lit, |l: u16| Loadable::Lit(l)),
                map(reg16, |r: RegisterKind16| Loadable::Reg16(r)),
            )),
        )(input)
    }

    fn load<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, Loadable, E> {
        delimited(
            preceded(sp, tag("[")),
            preceded(sp, loadable),
            preceded(sp, tag("]")),
        )(input)
    }

    fn reg8<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, RegisterKind8, E> {
        preceded(
            sp,
            alt((
                map(tag("A"), |_: &str| RegisterKind8::A),
                map(tag("B"), |_: &str| RegisterKind8::B),
                map(tag("C"), |_: &str| RegisterKind8::C),
                map(tag("D"), |_: &str| RegisterKind8::D),
                map(tag("E"), |_: &str| RegisterKind8::E),
                map(tag("H"), |_: &str| RegisterKind8::H),
                map(tag("L"), |_: &str| RegisterKind8::L),
            )),
        )(input)
    }

    fn term<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, Term, E> {
        preceded(
            sp,
            alt((
                map(load, |l: Loadable| Term::Load(l)),
                map(loadable, |l: Loadable| Term::Loadable(l)),
                map(reg8, |r: RegisterKind8| Term::Reg8(r)),
            )),
        )(input)
    }

    fn expr0<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, Box<E0>, E> {
        let (input, term) = term(input)?;
        let (input, e1) = expr1(input)?;
        Ok((input, Box::new(E0 { term, e1 })))
    }

    fn expr1<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, Box<E1>, E> {
        map(
            alt((
                map(tuple((op, expr0, expr1)), |(op, e0, e1)| {
                    E1::Binop(op, e0, e1)
                }),
                map(take(0u8), |_: &str| E1::Epsilon),
            )),
            |e1| Box::new(e1),
        )(input)
    }

    fn root<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, Box<E0>, E> {
        all_consuming(expr0)(input)
    }

    #[derive(Debug, Clone)]
    pub struct E(pub Box<E0>);
    impl std::str::FromStr for E {
        type Err = std::string::String;

        fn from_str(src: &str) -> Result<E, Self::Err> {
            root::<VerboseError<&str>>(src)
                .map(|(_, e0)| E(e0))
                .map_err(|e| match e {
                    nom::Err::Error(e) => nom::error::convert_error(src, e),
                    nom::Err::Failure(e) => nom::error::convert_error(src, e),
                    nom::Err::Incomplete(_) => "Incomplete".to_string(),
                })
        }
    }

    use hardware::Hardware;
    use mem::Addr;

    pub type Output = u16;
    type Context<'a> = &'a mut Hardware;
    pub trait Eval {
        fn eval(&self, ctx: Context) -> Output;
    }

    impl Eval for Loadable {
        fn eval(&self, ctx: Context) -> Output {
            match self {
                Loadable::Lit(lit) => *lit,
                Loadable::Reg16(rk) => ctx.cpu.registers.read16(*rk).0,
            }
        }
    }

    impl Eval for Term {
        fn eval(&self, ctx: Context) -> Output {
            match self {
                Term::Reg8(rk) => u16::from(ctx.cpu.registers.read8(*rk).0),
                Term::Loadable(loadable) => loadable.eval(ctx),
                Term::Load(loadable) => {
                    let res = loadable.eval(ctx);
                    u16::from(ctx.cpu.memory.ld8(Addr::directly(res)))
                }
            }
        }
    }

    impl Eval for E0 {
        fn eval(&self, ctx: Context) -> Output {
            fn f(l: Output, op: &Op, r: Output) -> Output {
                match op {
                    Op::Equals => {
                        if l == r {
                            0
                        } else {
                            1
                        }
                    }
                    Op::NotEquals => {
                        if l != r {
                            0
                        } else {
                            1
                        }
                    }
                    Op::LessThan => {
                        if l < r {
                            0
                        } else {
                            1
                        }
                    }
                    Op::GreaterThan => {
                        if l > r {
                            0
                        } else {
                            1
                        }
                    }
                    Op::Plus => l + r,
                    Op::Minus => l - r,
                    Op::Times => l * r,
                    Op::RightShift => l >> r,
                    Op::LeftShift => l << r,
                    Op::BitAnd => l & r,
                    Op::BitOr => l | r,
                }
            }

            fn go(ctx: Context, acc: Output, e1: &Box<E1>) -> Output {
                match e1 {
                    box E1::Epsilon => acc,
                    box E1::Binop(op, e0, e1) => {
                        let e0_ = e0.eval(ctx);
                        go(ctx, f(acc, op, e0_), e1)
                    }
                }
            }

            let t1 = self.term.eval(ctx);
            go(ctx, t1, &self.e1)
        }
    }

    impl Eval for E {
        fn eval(&self, ctx: Context) -> Output {
            self.0.eval(ctx)
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
        arg: Option<String>,
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

    running.store(true, Ordering::SeqCst);
    loop {
        if running.load(Ordering::SeqCst) {
            let now = performance.now();
            let diff = now - last;
            last = now;

            hardware.run(diff);

            let now_ = performance.now();
            sleep(Duration::from_micros(
                16_667 - (((now_ - now) * 1000.) as u64),
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
