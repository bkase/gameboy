#![feature(proc_macro_hygiene)]
#![feature(track_caller)]
#![feature(exclusive_range_pattern)]
#![feature(or_patterns)]
#![recursion_limit = "512"]

extern crate packed_struct;
#[macro_use]
extern crate packed_struct_codegen;
extern crate clap;
extern crate nom;
extern crate these;

#[cfg(test)]
pub mod test {
    pub extern crate proptest;
}

mod alu;
mod cpu;
mod expr;
mod hardware;
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

use clap::{App, Arg, SubCommand};
use std::collections::BTreeSet;
use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufRead, BufReader};
use trace::Record;

#[cfg(test)]
pub extern crate proptest_derive;

trait BTreeSetExt {
    fn add_record_from_line(&mut self, line: &str);
}

impl BTreeSetExt for BTreeSet<Record> {
    fn add_record_from_line(&mut self, line: &str) {
        if line == "" {
            return;
        }

        // about 50MB/s
        let (_, r) = Record::of_line(line).expect("Trying to read line");

        self.insert(r);
    }
}

fn intersect(left: &str, right: &str) {
    let left = File::open(left).expect("Left file can't be opened");
    let left = BufReader::new(left);

    let right = File::open(right).expect("Right file can't be opened");
    let right = BufReader::new(right);

    let mut s1 = BTreeSet::new();
    let mut s2 = BTreeSet::new();

    for line in left.lines() {
        s1.add_record_from_line(&line.unwrap())
    }
    for line in right.lines() {
        s2.add_record_from_line(&line.unwrap())
    }

    for record in s1.intersection(&s2) {
        println!("{:}", record)
    }
    for record in s2.intersection(&s1) {
        eprintln!("{:}", record)
    }
}

fn dedupe_stdin() {
    let mut s = BTreeSet::new();
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        s.add_record_from_line(&line.unwrap())
    }

    s.iter().for_each(|r| println!("{:}", r))
}

pub fn main() {
    let intersect_name = "intersect";
    let matches = App::new("Gameboy trace records")
        .version("1.0")
        .author("Brandon Kase (@bkase_)")
        .about("Parse, print, and manipulate trace records per instruction gameboy cpu instruction")
        .subcommand(
            SubCommand::with_name(intersect_name)
                .about(
                    "Intersect two traces on program counters -- stdout is left, stderr is right",
                )
                .arg(
                    Arg::with_name("LEFT_INPUT")
                        .help("Sets the left input file to use")
                        .required(true)
                        .index(1),
                )
                .arg(
                    Arg::with_name("RIGHT_INPUT")
                        .help("Sets the right input file to use")
                        .required(true)
                        .index(2),
                ),
        )
        .get_matches();

    if let Some(matches) = matches.subcommand_matches(intersect_name) {
        intersect(
            matches.value_of("LEFT_INPUT").unwrap(),
            matches.value_of("RIGHT_INPUT").unwrap(),
        )
    } else {
        dedupe_stdin()
    }
}
