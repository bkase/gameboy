#![allow(dead_code)]

use ppu::{PpuRegisters, ReadViewU8, ViewU8};
use register::R16;
use std::fmt;

/* 5.1. General memory map
 Interrupt Enable Register
 --------------------------- FFFF
 Internal RAM
 --------------------------- FF80
 Empty but unusable for I/O
 --------------------------- FF4C
 I/O ports
 --------------------------- FF00
 Empty but unusable for I/O
 --------------------------- FEA0
 Sprite Attrib Memory (OAM)
 --------------------------- FE00
 Echo of 8kB Internal RAM
 --------------------------- E000
 8kB Internal RAM
 --------------------------- C000
 8kB switchable RAM bank
 --------------------------- A000
 8kB Video RAM
 --------------------------- 8000 --
 16kB switchable ROM bank |
 --------------------------- 4000 |= 32kB Cartrigbe
 16kB ROM bank #0 |
 --------------------------- 0000 --
*/

pub const BOOTROM: &[u8; 0x100] = include_bytes!("../DMG_ROM.bin");

pub struct Memory {
    zero: Vec<u8>,
    main: Vec<u8>,
    video: Vec<u8>,
    rom0: Vec<u8>,
    pub ppu: PpuRegisters,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Addr(u16);
impl fmt::Display for Addr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "${:04x}", self.0)
    }
}

pub enum Direction {
    Pos,
    Neg,
}

impl Addr {
    pub fn indirectly(r: R16) -> Addr {
        Addr(r.0)
    }

    pub fn directly(n: u16) -> Addr {
        Addr(n)
    }

    pub fn io_memory() -> Addr {
        Addr::directly(0xff00)
    }
}

impl Addr {
    pub fn offset(self, by: u16, direction: Direction) -> Addr {
        let new_val = match direction {
            Direction::Pos => self.0.wrapping_add(by),
            Direction::Neg => self.0.wrapping_sub(by),
        };
        Addr(new_val)
    }

    pub fn into_register(self) -> R16 {
        R16(self.0)
    }
}

impl Memory {
    pub fn create() -> Memory {
        Memory {
            zero: vec![0; 0x7f],
            main: vec![0; 0x2000],
            video: vec![0; 0x2000],
            rom0: vec![0; 0x4000],
            ppu: PpuRegisters::create(),
        }
    }

    pub fn ld_lots(&self, Addr(addr): Addr, length: u16) -> Vec<u8> {
        (0..length).map(|i| self.ld8(Addr(addr + i))).collect()
    }

    #[allow(clippy::match_overlapping_arm)]
    pub fn ld8(&self, Addr(addr): Addr) -> u8 {
        match addr {
            0xffff => {
                println!("ie register");
                0
            }
            0xff80...0xfffe => self.zero[(addr - 0xff80) as usize],
            0xff4c...0xff7f => {
                println!("unusable memory");
                0
            }
            0xff11 => {
                println!("Sound mode change, not-implemented for now");
                0
            }
            0xff13 => {
                println!("Sound mode change, not-implemented for now");
                0
            }
            0xff40 => self.ppu.lcdc.read(),
            0xff42 => self.ppu.scy.read(),
            0xff44 => self.ppu.ly.read(),
            0xff47 => self.ppu.bgp.read(),
            0xff00...0xff4b => {
                println!("Passthrough...");
                0
            }
            // panic!("rest of I/O ports"),
            0xfea0...0xfeff => panic!("unusable"),
            0xfe00...0xfe9f => panic!("sprite oam"),
            0xe000...0xfdff => panic!("echo ram"),
            // 0xd000 ... 0xdfff => panic!("(cgb) ram banks 1-7"),
            // 0xc000 ... 0xcfff => panic!("ram bank 0"),
            0xc000...0xdfff => self.main[(addr - 0xc000) as usize],
            0xa000...0xbfff => {
                // cartridge ram
                // not sure what to do here...
                println!("Passthrough...");
                0
            }
            // end video ram
            // 0x9c00 ... 0x9fff => panic!("bg map data 2"),
            // 0x9800 ... 0x9bff => panic!("bg map data 1"),
            // 0x8000 ... 0x97ff => panic!("character ram"),
            0x8000...0x9fff => self.video[(addr - 0x8000) as usize],
            // begin video ram
            0x4000...0x7fff => panic!("switchable rom banks xx"),
            0x0150...0x3fff =>
            // rom bank 0
            {
                self.rom0[addr as usize]
            }
            0x0100...0x014f =>
            // cartridge header
            {
                self.rom0[addr as usize]
            }
            0x0000...0x00ff =>
            // bootrom
            {
                BOOTROM[addr as usize]
            }
        }
    }

    pub fn ld16(&self, Addr(addr): Addr) -> u16 {
        fn u16read(vec: &[u8], addr: u16) -> u16 {
            (u16::from(vec[(addr + 1) as usize]) << 8) | u16::from(vec[addr as usize])
        }

        match addr {
            0xffff => panic!("interrupt enable register"),
            0xff80...0xfffe => u16read(&self.zero, addr - 0xff80),
            0xff4c...0xff7f => panic!("unusable"),
            0xff00...0xff4b => panic!("I/O ports"),
            0xfea0...0xfeff => panic!("unusable"),
            0xfe00...0xfe9f => panic!("sprite oam"),
            0xe000...0xfdff => panic!("echo ram"),
            // 0xd000 ... 0xdfff => panic!("(cgb) ram banks 1-7"),
            // 0xc000 ... 0xcfff => panic!("ram bank 0"),
            0xc000...0xdfff => u16read(&self.main, addr - 0xc000),
            0xa000...0xbfff => panic!("cartridge ram"),
            // end video ram
            // 0x9c00 ... 0x9fff => panic!("bg map data 2"),
            // 0x9800 ... 0x9bff => panic!("bg map data 1"),
            // 0x8000 ... 0x97ff => panic!("character ram"),
            0x8000...0x9fff => u16read(&self.video, addr - 0x8000),
            // begin video ram
            0x4000...0x7fff => panic!("switchable rom banks xx"),
            0x0150...0x3fff => u16read(&self.rom0, addr),

            0x0100...0x014f => u16read(&self.rom0, addr),
            0x0000...0x00ff =>
            // bootrom
            {
                u16read(BOOTROM, addr)
            }
        }
    }

    #[allow(clippy::match_overlapping_arm)]
    pub fn st8(&mut self, Addr(addr): Addr, n: u8) {
        match addr {
            0xffff => panic!("interrupt enable register"),
            0xff80...0xfffe => self.zero[(addr - 0xff80) as usize] = n,

            0xff4c...0xff7f => panic!("unusable"),
            0xff11 => println!("Sound mode change, not-implemented for now"),
            0xff13 => println!("Sound mode change, not-implemented for now"),
            0xff40 => self.ppu.lcdc.set(n),
            0xff42 => self.ppu.scy.set(n),
            0xff44 => panic!("Cannot write to LY register"),
            0xff47 => self.ppu.bgp.set(n),
            0xff00...0xff4b => println!("Passthrough"),
            // panic!("rest of I/O ports"),
            0xfea0...0xfeff => panic!("unusable"),
            0xfe00...0xfe9f => panic!("sprite oam"),
            0xe000...0xfdff => panic!("echo ram"),
            // 0xd000 ... 0xdfff => panic!("(cgb) ram banks 1-7"),
            // 0xc000 ... 0xcfff => panic!("ram bank 0"),
            0xc000...0xdfff => self.main[(addr - 0xc000) as usize] = n,
            0xa000...0xbfff => panic!("cartridge ram"),
            // end video ram
            // 0x9c00 ... 0x9fff => panic!("bg map data 2"),
            // 0x9800 ... 0x9bff => panic!("bg map data 1"),
            // 0x8000 ... 0x97ff => panic!("character ram"),
            0x8000...0x9fff => self.video[(addr - 0x8000) as usize] = n,
            // begin video ram
            0x4000...0x7fff => panic!("switchable rom banks xx"),
            0x0150...0x3fff => panic!("Cannot write to ROM"),
            0x0100...0x014f =>
            // cartridge header
            {
                panic!("Cannot write to ROM")
            }
            0x0000...0x00ff => println!("Cannot write to bootrom"),
        }
    }
}
