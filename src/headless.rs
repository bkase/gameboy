use hardware;
use hardware::Hardware;
use image::png::PNGEncoder;
use instr::Instr;
use instr::Jump::*;
use mem::{Bootrom, Cartridge};
use screen::Screen;
use std::borrow::Cow;
use std::fs::{self, File};
use std::io::prelude::*;
use std::path::Path;
use std::path::PathBuf;
use std::process::exit;
use std::thread::sleep;
use std::time::Duration;
use structopt::StructOpt;
use these::These;
use web_utils::*;

#[derive(Debug, StructOpt)]
#[structopt(
    about = "Run a gameboy emulator without a graphics frontend. By default, exits successfully (code 0) when a `jr -2` is encountered."
)]
pub struct Config {
    /// The 256byte DMG_BOOTROM.bin file
    #[structopt(short, long, parse(from_os_str))]
    pub use_bootrom: Option<PathBuf>,

    /// Timeout in millis after which this program will exit with code 124
    #[structopt(long)]
    pub timeout: Option<u32>,

    /// Saves a screenshot to directory after the timeout or a `jr -2`
    #[structopt(short, long, parse(from_os_str))]
    pub screenshot_directory: Option<PathBuf>,

    /// Emits a trace line on stdout before executing each instruction. Use the trace records tool to manipulate these traces
    #[structopt(short, long)]
    pub trace: bool,

    /// ROM to run in the gameboy
    #[structopt(name = "ROM", parse(from_os_str))]
    pub rom: PathBuf,
}

fn capture_screenshot(config: &Config, name: &str, screen: &Screen) {
    match &config.screenshot_directory {
        Some(dir) => {
            let mut file = File::create(Path::new(dir).join(name.to_owned() + ".png"))
                .expect("Cannot create screenshot");
            let encoder = PNGEncoder::new(file);
            encoder
                .encode(
                    screen.data.as_slice(),
                    screen.width,
                    screen.height,
                    image::ColorType::Rgba8,
                )
                .expect("Couldn't write png to file");
        }
        None => (),
    }
}

fn open_and_validate(path: &PathBuf, length: u64) -> File {
    let file = File::open(path).expect(&format!("Can't open rom file at {:?}", path));
    let metadata = fs::metadata(path).expect("Can't read metadata in rom");
    if metadata.len() != length {
        eprintln!("Rom is not {:} bytes", length);
        exit(1);
    }
    file
}

fn read_bootrom(use_bootrom: &Option<PathBuf>) -> Option<Bootrom> {
    use_bootrom.as_ref().map(|path| {
        let mut bootrom = open_and_validate(path, 0x100);
        let mut buf: Bootrom = [0; 0x100];
        bootrom
            .read(&mut buf)
            .expect("Unexpected buffer overflow, we already checked length");
        buf
    })
}

fn read_cartridge(path: &PathBuf) -> Cartridge {
    let mut cartridge = open_and_validate(path, 0x8000);
    let mut buf: Cartridge = [0; 0x8000];
    cartridge
        .read(&mut buf)
        .expect("Unexpected buffer overflow, we already checked length");
    buf
}

pub fn run(config: Config) -> i32 {
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
        match hardware.cpu.ip.peek(&hardware.cpu.memory) {
            Instr::Jump(Jr(-2)) => {
                capture_screenshot(&config, "final", &hardware.ppu.screen);
                return 0;
            }
            _ => (),
        }

        let now = performance.now();
        let diff = now - last;
        last = now;

        hardware.run(diff);

        match config.timeout {
            Some(millis) => {
                let diff_from_start = now - start;
                if diff_from_start > f64::from(millis) {
                    capture_screenshot(&config, "final", &hardware.ppu.screen);
                    return 124;
                }
            }
            None => (),
        };
        sleep(Duration::from_micros(16_667))
    }
}
