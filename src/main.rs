#![feature(proc_macro_hygiene)]
#![feature(track_caller)]
#![feature(exclusive_range_pattern)]
#![feature(or_patterns)]
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

#[cfg(test)]
pub mod test {
    pub extern crate proptest;
}
#[cfg(test)]
pub extern crate proptest_derive;

extern crate image;

mod alu;
mod cpu;
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
mod trace;
mod utils;
mod web_utils;

use headless::run;
use std::env::temp_dir;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process::exit;
use std::process::Command;
use std::process::ExitStatus;
use std::str::from_utf8;
use structopt::StructOpt;

#[derive(Debug, Clone, serde::Deserialize)]
struct Run {
    name: String,
    timeout_millis: u32,
}

#[derive(Debug, Clone, serde::Deserialize)]
struct Spec {
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
}

pub fn main() {
    let config = Config::from_args();

    let mut err = false;
    match config {
        Config::Run { exec_config } => exit(run(exec_config)),
        Config::Golden { golden_path } => {
            let specs_path = golden_path.join("golden_tests.dhall");

            let output = Command::new("bash")
                .arg("-c")
                .arg(format!(
                    "dhall-to-json <<< './{:}'",
                    specs_path.to_str().unwrap()
                ))
                .output()
                .expect("Failed to run dhall-to-json");
            if !output.status.success() {
                eprintln!("Failed to run dhall-to-json");
                eprintln!("OUT: {:}", from_utf8(output.stdout.as_slice()).unwrap());
                eprintln!("ERR: {:}", from_utf8(output.stderr.as_slice()).unwrap());
            }

            let specs: Vec<Spec> =
                serde_json::from_str(&from_utf8(output.stdout.as_slice()).unwrap())
                    .expect("It parses");

            for spec in specs {
                let rom = &spec.rom.clone();
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
                        use_bootrom: None,
                        trace: false,
                        screenshot_directory: Some(out_dir.clone()),
                        timeout: Some(r.timeout_millis),
                        rom: golden_path.join(rom.clone()),
                    });

                    for entry in fs::read_dir(&out_dir).expect("Expect dir to begin") {
                        let new_file = entry.expect("Entry can load").path();
                        let golden_file = golden_dir.join(Path::new(
                            new_file.file_name().expect("File should have a file_name"),
                        ));
                        // TODO: Check if golden file exists
                        if !golden_file.exists() {
                            eprintln!("Missing golden file {:?}; cannot diff", golden_file);
                            continue;
                        }

                        let diff_file = out_dir.join("diff.png");
                        // TODO: Diff and generate report
                        let output = Command::new("bash")
                            .arg(&golden_path.join(Path::new("stitch.sh")).to_str().unwrap())
                            .arg(&golden_file)
                            .arg(&new_file)
                            .arg(&diff_file)
                            .output()
                            .expect("Failed to run stitch.sh");

                        if !output.status.success() {
                            eprintln!("Stich.sh command failed!");
                            eprintln!("OUT: {:}", from_utf8(output.stdout.as_slice()).unwrap());
                            eprintln!("ERR: {:}", from_utf8(output.stderr.as_slice()).unwrap());
                        } else {
                            // if the diff file exists it means that stitch.sh found a diff
                            if diff_file.exists() {
                                err = true;
                                eprintln!("❌ Diff on {:} @ {:}", r.name, rom);

                                let output = Command::new("base64")
                                    .arg("-w0")
                                    .arg(diff_file.to_str().unwrap())
                                    // TODO: namespace to a specific build to avoid race
                                    .output()
                                    .expect("Failed to run buildkite-agent");

                                if !output.status.success() {
                                    eprintln!("Base64 failed");
                                    eprintln!(
                                        "OUT: {:}",
                                        from_utf8(output.stdout.as_slice()).unwrap()
                                    );
                                    eprintln!(
                                        "ERR: {:}",
                                        from_utf8(output.stderr.as_slice()).unwrap()
                                    );
                                } else {
                                    // let b64data = from_utf8(output.stdout.as_slice()).unwrap();
                                    let mut process = Command::new("~/imgcat")
                                        .arg(diff_file.to_str().unwrap())
                                        .spawn()
                                        .expect("imgcat can't run");

                                    println!(
                                        "imgcat exited with status {:}",
                                        process.wait().unwrap()
                                    );
                                }
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
