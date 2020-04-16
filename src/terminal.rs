use ansi_escapes::{CursorTo, EraseScreen};
use hardware;
use hardware::Hardware;
use headless::{read_bootrom, read_cartridge, Config};
use screen::{Coordinate, Screen};
use std::borrow::Cow;
use std::io;
use std::io::prelude::*;
use these::These;
use web_utils::*;

fn draw(screen: &Screen) {
    // from https://github.com/hopey-dishwasher/termpix/blob/master/src/lib.rs
    let mut row = Vec::new();
    // move cursor to top left
    write!(row, "{:}", CursorTo::TopLeft).unwrap();
    for y in 0..screen.height {
        //TODO: inc by 2 instead
        if y % 2 == 1 || y + 1 == screen.height {
            continue;
        }

        for x in 0..screen.width {
            let top = screen.get(Coordinate {
                x: x as u8,
                y: y as u8,
            });
            let bottom = screen.get(Coordinate {
                x: x as u8,
                y: y as u8,
            });
            write!(
                row,
                "\x1b[48;2;{};{};{}m\x1b[38;2;{};{};{}m▄",
                top.r, top.g, top.b, bottom.r, bottom.g, bottom.b
            )
            .unwrap();
        }

        write!(row, "\x1b[m\n").unwrap();
        io::stdout().write(&row).unwrap();
        row.clear();
    }
}

pub fn run(config: Config) {
    let bootrom = read_bootrom(&config.use_bootrom);
    let cartridge = read_cartridge(&config.rom);

    // timing
    let performance = Performance::create();
    let start = performance.now();
    let mut last = start;

    let mut hardware = Hardware::create(
        &performance,
        hardware::Config {
            trace: config.trace,
            roms: match bootrom {
                Some(bootrom) => These::These(Cow::Owned(bootrom), Cow::Owned(cartridge)),
                None => These::That(Cow::Owned(cartridge)),
            },
        },
    );
    hardware.paused = false;

    loop {
        let now = performance.now();
        let diff = now - last;
        last = now;

        hardware.run(diff);

        draw(&hardware.ppu.screen);
    }
}
