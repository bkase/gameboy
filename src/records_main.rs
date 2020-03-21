#![feature(proc_macro_hygiene)]
#![feature(track_caller)]
#![feature(exclusive_range_pattern)]
#![feature(or_patterns)]
#![recursion_limit = "512"]

extern crate packed_struct;
#[macro_use]
extern crate packed_struct_codegen;
extern crate nom;

#[cfg(test)]
pub mod test {
    pub extern crate proptest;
}

mod alu;
mod cpu;
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

use std::collections::BTreeSet;
use std::io;
use std::io::prelude::*;
use trace::Record;
#[cfg(test)]
pub extern crate proptest_derive;

pub fn main() {
    let mut s = BTreeSet::new();
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let l = &line.unwrap();
        if l == "" {
            continue;
        }

        // about 50MB/s
        let (_, r) = Record::of_line(l).expect("Trying to read line");

        s.insert(r);
    }

    s.iter().for_each(|r| println!("{:}", r))
}
