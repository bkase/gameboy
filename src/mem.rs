#![allow(dead_code)]

use packed_struct::prelude::*;
use ppu::{OamEntry, PpuRegisters};
use read_view_u8::*;
use register::R16;
use sound;
use std::convert::TryInto;
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

// TODO: Switch to unpatched ROM and figure out why lockup happens
pub const BOOTROM: &[u8; 0x100] = include_bytes!("../DMG_ROM_PATCHED.bin");

// Cartridges
pub type Cartridge = &'static [u8; 0x8000];

pub const TETRIS: Cartridge = include_bytes!("../Tetris.GB");

#[derive(PackedStruct, Debug)]
#[packed_struct(size_bytes = "1", bit_numbering = "lsb0")]
pub struct InterruptRegister {
    #[packed_field(bits = "0")]
    pub vblank: bool,
    #[packed_field(bits = "1")]
    pub lcd_stat: bool,
    #[packed_field(bits = "2")]
    pub timer: bool,
    #[packed_field(bits = "3")]
    pub serial: bool,
    #[packed_field(bits = "4")]
    pub joypad: bool,
}
impl InterruptRegister {
    fn create() -> InterruptRegister {
        InterruptRegister {
            vblank: false,
            lcd_stat: false,
            timer: false,
            serial: false,
            joypad: false,
        }
    }
}
impl ReadViewU8 for InterruptRegister {
    fn read(&self) -> u8 {
        self.pack()[0]
    }
}
impl ViewU8 for InterruptRegister {
    fn set(&mut self, n: u8) {
        *self = InterruptRegister::unpack(&[n]).expect("it's 8bits")
    }
}

#[derive(Debug)]
pub struct Memory {
    booting: bool,
    zero: Vec<u8>,
    sprite_oam: Vec<OamEntry>,
    main: Vec<u8>,
    video: Vec<u8>,
    rom0: Vec<u8>,
    rom1: Vec<u8>,
    pub interrupt_enable: InterruptRegister,
    pub interrupt_flag: InterruptRegister,
    pub ppu: PpuRegisters,
    pub sound: sound::Registers,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd)]
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
    pub fn create(cartridge: Option<Cartridge>) -> Memory {
        let (rom0, rom1) = match cartridge {
            Some(c) => {
                let (bank0, bank1) = c.split_at(0x4000);
                (bank0.to_vec(), bank1.to_vec())
            }
            None => (vec![0; 0x4000], vec![0; 0x4000]),
        };
        Memory {
            booting: true,
            zero: vec![0; 0x7f],
            sprite_oam: vec![OamEntry::create(); 40],
            main: vec![0; 0x2000],
            video: vec![0; 0x2000],
            rom0,
            rom1,
            interrupt_enable: InterruptRegister::create(),
            interrupt_flag: InterruptRegister::create(),
            ppu: PpuRegisters::create(),
            sound: sound::Registers::create(),
        }
    }

    #[inline]
    pub fn done_booting(&mut self) {
        self.booting = false;
    }

    pub fn ld_lots(&self, Addr(addr): Addr, length: u16) -> Vec<u8> {
        (0..length).map(|i| self.ld8(Addr(addr + i))).collect()
    }

    #[allow(clippy::match_overlapping_arm)]
    pub fn ld8(&self, Addr(addr): Addr) -> u8 {
        match addr {
            0xffff => self.interrupt_enable.read(),
            0xff80..=0xfffe => self.zero[(addr - 0xff80) as usize],
            0xff4c..=0xff7f => {
                println!("unusable memory");
                0
            }
            0xff0f => self.interrupt_flag.read(),
            0xff10 => self.sound.pulse_a.sweep.read(),
            0xff11 => self.sound.pulse_a.length.read(),
            0xff12 => self.sound.pulse_a.volume.read(),
            0xff13 => self.sound.pulse_a.frequency.read(),
            0xff14 => self.sound.pulse_a.control.read(),
            0xff40 => self.ppu.lcdc.read(),
            0xff42 => self.ppu.scy.read(),
            0xff43 => self.ppu.scx.read(),
            0xff44 => self.ppu.ly.read(),
            0xff47 => self.ppu.bgp.read(),
            0xff00..=0xff4b => {
                println!("Passthrough...");
                0
            }
            // panic!("rest of I/O ports"),
            0xfea0..=0xfeff => 0, // unusable memory
            0xfe00..=0xfe9f => {
                let addr_ = addr - 0xfe00;
                self.sprite_oam[(addr_ / 4) as usize].pack()[(addr_ % 4) as usize]
            }
            0xe000..=0xfdff => panic!("echo ram"),
            // 0xd000 ... 0xdfff => panic!("(cgb) ram banks 1-7"),
            // 0xc000 ... 0xcfff => panic!("ram bank 0"),
            0xc000..=0xdfff => self.main[(addr - 0xc000) as usize],
            0xa000..=0xbfff => {
                // cartridge ram
                // not sure what to do here...
                println!("Passthrough...");
                0
            }
            // end video ram
            // 0x9c00 ... 0x9fff => panic!("bg map data 2"),
            // 0x9800 ... 0x9bff => panic!("bg map data 1"),
            // 0x8000 ... 0x97ff => panic!("character ram"),
            0x8000..=0x9fff => self.video[(addr - 0x8000) as usize],
            // begin video ram
            0x4000..=0x7fff => self.rom1[(addr - 0x4000) as usize],
            0x0150..=0x3fff =>
            // rom bank 0
            {
                self.rom0[addr as usize]
            }
            // HACK: cartridge header with Nintendo logo
            0x0104..=0x0134 => {
                // redirect to nintendo logo inside bootrom
                BOOTROM[(addr as usize) - 0x0104 + 0x00a8]
            }
            0x0100..=0x014f =>
            // cartridge header
            {
                self.rom0[addr as usize]
            }
            0x0000..=0x00ff =>
            // bootrom
            {
                if self.booting {
                    BOOTROM[addr as usize]
                } else {
                    self.rom0[addr as usize]
                }
            }
        }
    }

    pub fn ld16(&self, Addr(addr): Addr) -> u16 {
        fn u16read(vec: &[u8], addr: u16) -> u16 {
            (u16::from(vec[(addr + 1) as usize]) << 8) | u16::from(vec[addr as usize])
        }

        match addr {
            0xffff => panic!("interrupt enable register"),
            0xff80..=0xfffe => u16read(&self.zero, addr - 0xff80),
            0xff4c..=0xff7f => panic!("unusable"),
            0xff00..=0xff4b => panic!("I/O ports"),
            0xfea0..=0xfeff => panic!("unusable"),
            0xfe00..=0xfe9f => {
                u16::from(self.ld8(Addr(addr + 1))) << 8 | u16::from(self.ld8(Addr(addr)))
            }
            0xe000..=0xfdff => panic!("echo ram"),
            // 0xd000 ... 0xdfff => panic!("(cgb) ram banks 1-7"),
            // 0xc000 ... 0xcfff => panic!("ram bank 0"),
            0xc000..=0xdfff => u16read(&self.main, addr - 0xc000),
            0xa000..=0xbfff => panic!("cartridge ram"),
            // end video ram
            // 0x9c00 ... 0x9fff => panic!("bg map data 2"),
            // 0x9800 ... 0x9bff => panic!("bg map data 1"),
            // 0x8000 ... 0x97ff => panic!("character ram"),
            0x8000..=0x9fff => u16read(&self.video, addr - 0x8000),
            // begin video ram
            0x4000..=0x7fff => u16read(&self.rom1, addr - 0x4000),
            0x0150..=0x3fff => u16read(&self.rom0, addr),

            0x0100..=0x014f => u16read(&self.rom0, addr),
            0x0000..=0x00ff =>
            // bootrom
            {
                if self.booting {
                    u16read(BOOTROM, addr)
                } else {
                    u16read(&self.rom0, addr)
                }
            }
        }
    }

    fn start_dma(&mut self, n: u8) {
        use web_utils::*;
        log(&format!("Initiating DMA from {:x}", u16::from(n) * 0x100));
        let base_addr = Addr::directly(u16::from(n) * 0x100);
        // TODO: Don't instantly DMA transfer, actually take the 160us
        // Right now this code instantly does the full transfer
        let bytes = self.ld_lots(base_addr, 0xa0);
        for (i, chunk) in bytes.chunks(4).enumerate() {
            self.sprite_oam[i] = OamEntry::unpack(chunk.try_into().expect("chunks are 4"))
                .expect("oam entry expected");
        }
    }

    fn st_lots(&mut self, addr: Addr, bytes: Vec<u8>) {
        bytes.iter().enumerate().for_each(|(i, byte)| {
            self.st8(addr.offset(i as u16, Direction::Pos), *byte);
        });
    }

    #[allow(clippy::match_overlapping_arm)]
    pub fn st8(&mut self, Addr(addr): Addr, n: u8) {
        match addr {
            0xffff => self.interrupt_enable.set(n),
            0xff80..=0xfffe => self.zero[(addr - 0xff80) as usize] = n,

            0xff4c..=0xff7f => {
                // TODO: Why does tetris write to "unusable" memory?
                // panic!("unusable"),
                ()
            }
            0xff0f => self.interrupt_flag.set(n),
            0xff10 => self.sound.pulse_a.sweep.set(n),
            0xff11 => self.sound.pulse_a.length.set(n),
            0xff12 => self.sound.pulse_a.volume.set(n),
            0xff13 => self.sound.pulse_a.frequency.set(n),
            0xff14 => self.sound.pulse_a.control.set(n),
            0xff40 => self.ppu.lcdc.set(n),
            0xff42 => self.ppu.scy.set(n),
            0xff43 => self.ppu.scx.set(n),
            0xff44 => panic!("Cannot write to LY register"),
            0xff46 => self.start_dma(n),
            0xff47 => self.ppu.bgp.set(n),
            0xff00..=0xff4b => println!("Passthrough"),
            // panic!("rest of I/O ports"),
            0xfea0..=0xfeff => {
                // TODO: Why does tetris write to this "unusable" memory
                ()
            }
            0xfe00..=0xfe9f => {
                let addr_ = addr - 0xfe00;
                self.sprite_oam[(addr_ / 4) as usize].pack()[(addr_ % 4) as usize] = n;
            }
            0xe000..=0xfdff => panic!("echo ram"),
            // 0xd000 ... 0xdfff => panic!("(cgb) ram banks 1-7"),
            // 0xc000 ... 0xcfff => panic!("ram bank 0"),
            0xc000..=0xdfff => self.main[(addr - 0xc000) as usize] = n,
            0xa000..=0xbfff => panic!("cartridge ram"),
            // end video ram
            // 0x9c00 ... 0x9fff => panic!("bg map data 2"),
            // 0x9800 ... 0x9bff => panic!("bg map data 1"),
            // 0x8000 ... 0x97ff => panic!("character ram"),
            0x8000..=0x9fff => self.video[(addr - 0x8000) as usize] = n,
            // begin video ram
            0x4000..=0x7fff => panic!("switchable rom banks xx"),
            0x0150..=0x3fff => {
                println!("(header) ROM write; do we need to implement bank switching here?")
            }
            0x0100..=0x014f =>
            // cartridge header
            {
                println!("(header) ROM write; do we need to implement bank switching here?")
            }
            0x0000..=0x00ff => println!("Cannot write to bootrom"),
        }
    }
}
