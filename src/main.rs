#![feature(proc_macro_hygiene)]
#![feature(track_caller)]
#![feature(exclusive_range_pattern)]
#![feature(or_patterns)]
#![feature(box_syntax, box_patterns)]
#![recursion_limit = "512"]

extern crate packed_struct;
#[macro_use]
extern crate packed_struct_codegen;
#[macro_use]
extern crate structopt;
extern crate clap;
extern crate nom;
extern crate serde_json;
extern crate these;
#[macro_use]
extern crate serde;
extern crate ansi_escapes;
extern crate rustyline;
#[macro_use]
extern crate rustyline_derive;
extern crate shlex;

#[cfg(test)]
pub mod test {
    pub extern crate proptest;
}
#[cfg(test)]
pub extern crate proptest_derive;

extern crate image;

mod alu;
mod cpu;
mod debugger;
mod hardware;
mod headless;
mod instr;
mod mem;
mod ppu;
mod read_view_u8;
mod register;
mod register_kind;
mod screen;
mod sound;
mod terminal;
mod trace;
mod utils;
mod web_utils;

use headless::run;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process::exit;
use std::process::{Command, Output};
use std::str::from_utf8;
use structopt::StructOpt;

#[derive(Debug, Clone, serde::Deserialize)]
struct Run {
    name: String,
    timeout_millis: u32,
}

#[derive(Debug, Clone, serde::Deserialize)]
struct Spec {
    use_bootrom: Option<String>,
    rom: String,
    run: Vec<Run>,
}

#[derive(Debug, StructOpt)]
#[structopt(about = "Run a gameboy emulator without a graphics frontend")]
enum Config {
    /// Run the emulator. By default, exits successfully (code 0) when a `jr -2` is encountered.
    Run {
        #[structopt(flatten)]
        exec_config: headless::Config,
    },
    /// Run golden master tests
    Golden {
        /// Golden tests folder where the output is stored
        golden_path: PathBuf,
    },
    /// The head is the terminal!!
    Term {
        #[structopt(flatten)]
        exec_config: headless::Config,
    },
    Debug {
        /// Debug this rom
        rom: PathBuf,
    },
}

fn exec(name: &str, command: &mut Command) -> Output {
    let output = command.output().expect(&format!("Failed to run {:}", name));
    if !output.status.success() {
        eprintln!("Failed to run {:}", name);
        eprintln!("OUT: {:}", from_utf8(output.stdout.as_slice()).unwrap());
        eprintln!("ERR: {:}", from_utf8(output.stderr.as_slice()).unwrap());
    }
    output
}

pub fn main() {
    let config = Config::from_args();

    let mut err = false;
    match config {
        Config::Debug { rom } => debugger::run(&rom),
        Config::Term { exec_config } => terminal::run(exec_config),
        Config::Run { exec_config } => exit(run(exec_config)),
        Config::Golden { golden_path } => {
            let specs_path = golden_path.join("golden_tests.dhall");

            let output = exec(
                "dhall-to-json",
                Command::new("bash").arg("-c").arg(format!(
                    "dhall-to-json <<< './{:}'",
                    specs_path.to_str().unwrap()
                )),
            );

            let specs: Vec<Spec> =
                serde_json::from_str(&from_utf8(output.stdout.as_slice()).unwrap())
                    .expect("It parses");

            for spec in specs {
                let rom = &spec.rom.clone();
                let use_bootrom = &spec.use_bootrom.clone();
                for r in &spec.run {
                    let sub_screenshot_dir = Path::new(
                        Path::new(&rom)
                            .file_name()
                            .expect("ROM input should be a file"),
                    )
                    .join(r.name.clone());

                    let out_dir = golden_path.join("out").join(sub_screenshot_dir.clone());
                    let golden_dir = golden_path.join("golden").join(sub_screenshot_dir);

                    fs::create_dir_all(&out_dir).expect("Dir created properly");
                    fs::create_dir_all(&golden_dir).expect("Dir created properly");

                    let _code = run(headless::Config {
                        use_bootrom: use_bootrom
                            .clone()
                            .map(|s| PathBuf::new().join(&golden_path).join(&s)),
                        trace: false,
                        screenshot_directory: Some(out_dir.clone()),
                        timeout: Some(r.timeout_millis),
                        rom: golden_path.join(rom.clone()),
                    });

                    for entry in fs::read_dir(&out_dir).expect("Expect dir to begin") {
                        let diff_file = out_dir.join("$diff.png");
                        let new_file = entry.expect("Entry can load").path();

                        // skip over diffs
                        if diff_file == new_file {
                            continue;
                        }
                        // Remove diff file so we know it's fresh for the run
                        let _ = fs::remove_file(&diff_file);

                        let golden_file = golden_dir.join(Path::new(
                            new_file.file_name().expect("File should have a file_name"),
                        ));
                        // If golden file doesn't exists, there's nothing for us to diff against
                        if !golden_file.exists() {
                            eprintln!("Missing golden file {:?}; cannot diff", golden_file);
                            continue;
                        }

                        // Diff and generate the report
                        let output = exec(
                            "stitch.sh",
                            Command::new("bash")
                                .arg(&golden_path.join("stitch.sh").to_str().unwrap())
                                .arg(&golden_file)
                                .arg(&new_file)
                                .arg(&diff_file),
                        );
                        if !output.status.success() {
                            err = true;
                        } else {
                            // if the diff file exists it means that stitch.sh found a diff
                            if diff_file.exists() {
                                err = true;
                                eprintln!(
                                    "❌ Diff on {:} @ {:} on file {:?}",
                                    r.name, rom, diff_file
                                );

                                // try to cat with kitty first
                                let mut process = Command::new("kitty")
                                    .arg("+kitten")
                                    .arg("icat")
                                    .arg(diff_file.to_str().unwrap())
                                    .spawn();
                                match process.iter_mut().next() {
                                    Some(p) => {
                                        let _ = p.wait();
                                    }
                                    None => (),
                                };

                                // cat with imgcat for iterm2 compat (+ buildkite logs)
                                let mut process =
                                    Command::new(golden_path.join("imgcat").to_str().unwrap())
                                        .arg("-p")
                                        .arg(diff_file.to_str().unwrap())
                                        .spawn()
                                        .expect("imgcat can't run");
                                let _ = process.wait().unwrap();
                            } else {
                                // otherwise there is no diff
                                println!("✅ No diff on {:} @ {:}", r.name, rom)
                            }
                        }
                    }
                }
            }
        }
    }

    if err {
        exit(1);
    }
}
