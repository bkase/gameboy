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

pub struct Memory {
    main: Vec<u8>,
    video: Vec<u8>,
    rom0: Vec<u8>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Addr(u16);

pub enum Direction { Pos, Neg }

impl Addr {
    pub fn indirectly(r : R16) -> Addr {
        Addr(r.0)
    }

    pub fn directly(n : u16) -> Addr {
        Addr(n)
    }

    pub fn io_memory() -> Addr {
        Addr::directly(0xff00)
    }
}

impl Addr {
    pub fn offset(&self, by: u16, direction: Direction) -> Addr {
        let new_val =
            match direction {
                Direction::Pos => self.0 + by,
                Direction::Neg => self.0 - by
            };
        Addr(new_val)
    }
}

impl Memory {
    pub fn create() -> Memory {
        Memory {
            main: vec![0; 0x2000],
            video: vec![0; 0x2000],
            rom0: vec![0; 0x4000]
        }
    }

    pub fn ld8(&self, Addr(addr): Addr) -> u8 {
        match addr {
            0xffff => panic!("interrupt enable register"),
            0xff80 ... 0xfffe => panic!("zero page"),
            0xff4c ... 0xff7f => panic!("unusable"),
            0xff00 ... 0xff4b => panic!("I/O ports"),
            0xfea0 ... 0xfeff => panic!("unusable"),
            0xfe00 ... 0xfe9f => panic!("sprite oam"),
            0xe000 ... 0xfdff => panic!("echo ram"),
            // 0xd000 ... 0xdfff => panic!("(cgb) ram banks 1-7"),
            // 0xc000 ... 0xcfff => panic!("ram bank 0"),
            0xc000 ... 0xdfff => self.main[(addr-0xc000) as usize],
            0xa000 ... 0xbfff => panic!("cartridge ram"),
            // end video ram
            // 0x9c00 ... 0x9fff => panic!("bg map data 2"),
            // 0x9800 ... 0x9bff => panic!("bg map data 1"),
            // 0x8000 ... 0x97ff => panic!("character ram"),
            0x8000 ... 0x9fff => self.video[(addr-0x8000) as usize],
            // begin video ram
            0x4000 ... 0x7fff => panic!("switchable rom banks xx"),
            0x0150 ... 0x3fff =>
                // rom bank 0
                self.rom0[addr as usize],
            0x0100 ... 0x014f =>
                // cartridge header
                self.rom0[addr as usize],
            0x0000 ... 0x00ff => panic!("restart/int vectors"),
        }
    }

    pub fn ld16(&self, Addr(addr): Addr) -> u16 {
        fn u16read(vec : &Vec<u8>, addr: u16) -> u16 {
            ((vec[addr as usize] as u16) << 8) | vec[(addr+1) as usize] as u16
        }

        match addr {
            0xffff => panic!("interrupt enable register"),
            0xff80 ... 0xfffe => panic!("zero page"),
            0xff4c ... 0xff7f => panic!("unusable"),
            0xff00 ... 0xff4b => panic!("I/O ports"),
            0xfea0 ... 0xfeff => panic!("unusable"),
            0xfe00 ... 0xfe9f => panic!("sprite oam"),
            0xe000 ... 0xfdff => panic!("echo ram"),
            // 0xd000 ... 0xdfff => panic!("(cgb) ram banks 1-7"),
            // 0xc000 ... 0xcfff => panic!("ram bank 0"),
            0xc000 ... 0xdfff => u16read(&self.main, addr-0xc000),
            0xa000 ... 0xbfff => panic!("cartridge ram"),
            // end video ram
            // 0x9c00 ... 0x9fff => panic!("bg map data 2"),
            // 0x9800 ... 0x9bff => panic!("bg map data 1"),
            // 0x8000 ... 0x97ff => panic!("character ram"),
            0x8000 ... 0x9fff => u16read(&self.video, addr-0x8000),
            // begin video ram
            0x4000 ... 0x7fff => panic!("switchable rom banks xx"),
            0x0150 ... 0x3fff => u16read(&self.rom0, addr),

            0x0100 ... 0x014f => u16read(&self.rom0, addr),
            0x0000 ... 0x00ff => panic!("restart/int vectors"),
        }
    }

    pub fn st8(&mut self, addr: Addr, n: u8) {
        panic!("TODO");
    }
}

