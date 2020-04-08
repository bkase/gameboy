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
extern crate these;

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

use headless::{run, Config};
use structopt::StructOpt;

pub fn main() {
    let config = Config::from_args();
    run(config);
}
