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

use hardware::Hardware;
use instr::Instr;
use instr::Jump::*;
use std::thread::sleep;
use std::time::Duration;
use web_utils::*;

pub fn main() {
    let performance = Performance::create();
    let mut last = performance.now();
    let mut hardware = Hardware::create(&performance);
    hardware.paused = false;

    loop {
        match hardware.cpu.ip.peek(&hardware.cpu.memory) {
            Instr::Jump(Jr(-2)) => return,
            _ => (),
        }

        let now = performance.now();
        let diff = now - last;
        last = now;

        hardware.run(diff);

        sleep(Duration::from_micros(16_667))
    }
}
