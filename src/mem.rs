use ppu::{PpuRegisters, ReadViewU8, ViewU8};
use register::R16;

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

pub const BOOTROM: [u8; 0x100] = [
    /*0000000*/ 0x31, 0xfe, 0xff, 0xaf, 0x21, 0xff, 0x9f, 0x32, 0xcb, 0x7c, 0x20, 0xfb, 0x21,
    0x26, 0xff, 0x0e, /*0000010*/ 0x11, 0x3e, 0x80, 0x32, 0xe2, 0x0c, 0x3e, 0xf3, 0xe2, 0x32,
    0x3e, 0x77, 0x77, 0x3e, 0xfc, 0xe0, /*0000020*/ 0x47, 0x11, 0x04, 0x01, 0x21, 0x10, 0x80,
    0x1a, 0xcd, 0x95, 0x00, 0xcd, 0x96, 0x00, 0x13, 0x7b, /*0000030*/ 0xfe, 0x34, 0x20, 0xf3,
    0x11, 0xd8, 0x00, 0x06, 0x08, 0x1a, 0x13, 0x22, 0x23, 0x05, 0x20, 0xf9, /*0000040*/ 0x3e,
    0x19, 0xea, 0x10, 0x99, 0x21, 0x2f, 0x99, 0x0e, 0x0c, 0x3d, 0x28, 0x08, 0x32, 0x0d, 0x20,
    /*0000050*/ 0xf9, 0x2e, 0x0f, 0x18, 0xf3, 0x67, 0x3e, 0x64, 0x57, 0xe0, 0x42, 0x3e, 0x91,
    0xe0, 0x40, 0x04, /*0000060*/ 0x1e, 0x02, 0x0e, 0x0c, 0xf0, 0x44, 0xfe, 0x90, 0x20, 0xfa,
    0x0d, 0x20, 0xf7, 0x1d, 0x20, 0xf2, /*0000070*/ 0x0e, 0x13, 0x24, 0x7c, 0x1e, 0x83, 0xfe,
    0x62, 0x28, 0x06, 0x1e, 0xc1, 0xfe, 0x64, 0x20, 0x06, /*0000080*/ 0x7b, 0xe2, 0x0c, 0x3e,
    0x87, 0xe2, 0xf0, 0x42, 0x90, 0xe0, 0x42, 0x15, 0x20, 0xd2, 0x05, 0x20, /*0000090*/ 0x4f,
    0x16, 0x20, 0x18, 0xcb, 0x4f, 0x06, 0x04, 0xc5, 0xcb, 0x11, 0x17, 0xc1, 0xcb, 0x11, 0x17,
    /*00000a0*/ 0x05, 0x20, 0xf5, 0x22, 0x23, 0x22, 0x23, 0xc9, 0xce, 0xed, 0x66, 0x66, 0xcc,
    0x0d, 0x00, 0x0b, /*00000b0*/ 0x03, 0x73, 0x00, 0x83, 0x00, 0x0c, 0x00, 0x0d, 0x00, 0x08,
    0x11, 0x1f, 0x88, 0x89, 0x00, 0x0e, /*00000c0*/ 0xdc, 0xcc, 0x6e, 0xe6, 0xdd, 0xdd, 0xd9,
    0x99, 0xbb, 0xbb, 0x67, 0x63, 0x6e, 0x0e, 0xec, 0xcc, /*00000d0*/ 0xdd, 0xdc, 0x99, 0x9f,
    0xbb, 0xb9, 0x33, 0x3e, 0x3c, 0x42, 0xb9, 0xa5, 0xb9, 0xa5, 0x42, 0x3c, /*00000e0*/ 0x21,
    0x04, 0x01, 0x11, 0xa8, 0x00, 0x1a, 0x13, 0xbe, 0x20, 0xfe, 0x23, 0x7d, 0xfe, 0x34, 0x20,
    /*00000f0*/ 0xf5, 0x06, 0x19, 0x78, 0x86, 0x23, 0x05, 0x20, 0xfb, 0x86, 0x20, 0xfe, 0x3e,
    0x01, 0xe0, 0x50,
];

pub struct Memory {
    main: Vec<u8>,
    video: Vec<u8>,
    rom0: Vec<u8>,
    ppu: PpuRegisters,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Addr(u16);

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
    pub fn offset(&self, by: u16, direction: Direction) -> Addr {
        let new_val = match direction {
            Direction::Pos => self.0 + by,
            Direction::Neg => self.0 - by,
        };
        Addr(new_val)
    }

    pub fn into_register(&self) -> R16 {
        R16(self.0)
    }
}

impl Memory {
    pub fn create() -> Memory {
        Memory {
            main: vec![0; 0x2000],
            video: vec![0; 0x2000],
            rom0: vec![0; 0x4000],
            ppu: PpuRegisters::create(),
        }
    }

    pub fn ld8(&self, Addr(addr): Addr) -> u8 {
        match addr {
            0xffff => panic!("interrupt enable register"),
            0xff80...0xfffe => panic!("zero page"),
            0xff4c...0xff7f => panic!("unusable"),
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
            0xff00...0xff4b => panic!("rest of I/O ports"),
            0xfea0...0xfeff => panic!("unusable"),
            0xfe00...0xfe9f => panic!("sprite oam"),
            0xe000...0xfdff => panic!("echo ram"),
            // 0xd000 ... 0xdfff => panic!("(cgb) ram banks 1-7"),
            // 0xc000 ... 0xcfff => panic!("ram bank 0"),
            0xc000...0xdfff => self.main[(addr - 0xc000) as usize],
            0xa000...0xbfff => panic!("cartridge ram"),
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
        fn u16read(vec: &Vec<u8>, addr: u16) -> u16 {
            ((vec[(addr + 1) as usize] as u16) << 8) | vec[addr as usize] as u16
        }

        match addr {
            0xffff => panic!("interrupt enable register"),
            0xff80...0xfffe => panic!("zero page"),
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
                u16read(&BOOTROM.to_vec(), addr)
            }
        }
    }

    pub fn st8(&mut self, Addr(addr): Addr, n: u8) {
        match addr {
            0xffff => panic!("interrupt enable register"),
            0xff80...0xfffe => panic!("zero page"),
            0xff4c...0xff7f => panic!("unusable"),
            0xff11 => println!("Sound mode change, not-implemented for now"),
            0xff13 => println!("Sound mode change, not-implemented for now"),
            0xff40 => self.ppu.lcdc.set(n),
            0xff42 => self.ppu.scy.set(n),
            0xff44 => panic!("Cannot write to LY register"),
            0xff47 => self.ppu.bgp.set(n),
            0xff00...0xff4b => panic!("rest of I/O ports"),
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
            0x0000...0x00ff => panic!("Cannot write to bootrom"),
        }
    }
}
