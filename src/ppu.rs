#![allow(dead_code)]

use mem::Addr;
use packed_struct::prelude::*;

pub trait ReadViewU8 {
    fn read(&self) -> u8;
}

pub trait ViewU8: ReadViewU8 {
    fn set(&mut self, n: u8);
}

impl ReadViewU8 for u8 {
    fn read(&self) -> u8 {
        *self
    }
}
impl ViewU8 for u8 {
    fn set(&mut self, n: u8) {
        *self = n
    }
}

#[derive(PackedStruct)]
#[packed_struct(size_bytes = "1", bit_numbering = "lsb0")]
pub struct Palette {
    #[packed_field(bits = "0:1")]
    pub dot00: Integer<u8, packed_bits::Bits2>,
    #[packed_field(bits = "2:3")]
    pub dot01: Integer<u8, packed_bits::Bits2>,
    #[packed_field(bits = "4:5")]
    pub dot10: Integer<u8, packed_bits::Bits2>,
    #[packed_field(bits = "6:7")]
    pub dot11: Integer<u8, packed_bits::Bits2>,
}
impl Palette {
    pub fn create() -> Palette {
        Palette {
            dot00: 0.into(),
            dot01: 0b01.into(),
            dot10: 0b10.into(),
            dot11: 0b11.into(),
        }
    }
}
impl ReadViewU8 for Palette {
    fn read(&self) -> u8 {
        self.pack()[0]
    }
}
impl ViewU8 for Palette {
    fn set(&mut self, n: u8) {
        *self = Palette::unpack(&[n]).expect("it's 8bits")
    }
}

#[derive(PrimitiveEnum_u8, Clone, Copy, Debug, PartialEq)]
pub enum ObjSize {
    _8x8 = 0,
    _8x16 = 1,
}

#[derive(PrimitiveEnum_u8, Clone, Copy, Debug, PartialEq)]
pub enum TileMapDisplay {
    _9800_9bff = 0,
    _9c00_9fff = 1,
}
impl TileMapDisplay {
    pub fn base_addr(self) -> Addr {
        match self {
            TileMapDisplay::_9800_9bff => Addr::directly(0x9800),
            TileMapDisplay::_9c00_9fff => Addr::directly(0x9c00),
        }
    }
}

#[derive(PrimitiveEnum_u8, Clone, Copy, Debug, PartialEq)]
pub enum BgWindowTileData {
    _8800_97ff = 0,
    _8000_8fff = 1, // same area as OBJ
}
impl BgWindowTileData {
    pub fn base_addr(self) -> Addr {
        match self {
            BgWindowTileData::_8800_97ff => Addr::directly(0x8800),
            BgWindowTileData::_8000_8fff => Addr::directly(0x8000),
        }
    }
}

#[derive(PackedStruct)]
#[packed_struct(size_bytes = "1", bit_numbering = "lsb0")]
pub struct Lcdc {
    #[packed_field(bits = "0")]
    bg_and_window_display: bool,
    #[packed_field(bits = "1")]
    obj_display: bool,
    #[packed_field(bits = "2", ty = "enum")]
    obj_size: ObjSize,
    #[packed_field(bits = "3", ty = "enum")]
    bg_tile_map_display: TileMapDisplay,
    #[packed_field(bits = "4", ty = "enum")]
    bg_window_tile_data: BgWindowTileData,
    #[packed_field(bits = "5")]
    window_display: bool,
    #[packed_field(bits = "6", ty = "enum")]
    window_tile_map_data: TileMapDisplay,
    #[packed_field(bits = "7")]
    lcd_control_operation: bool,
}
// 0x91
// 0101 0001
impl Lcdc {
    fn create() -> Lcdc {
        Lcdc::unpack(&[0x00]).expect("Fits within a u8")
    }
}

impl ReadViewU8 for Lcdc {
    fn read(&self) -> u8 {
        self.pack()[0]
    }
}

impl ViewU8 for Lcdc {
    fn set(&mut self, n: u8) {
        *self = Lcdc::unpack(&[n]).expect("it's 8bits")
    }
}

pub struct PpuRegisters {
    pub lcdc: Lcdc,
    pub scy: u8,
    pub ly: u8, // read/only
    pub bgp: Palette,
}

impl PpuRegisters {
    pub fn create() -> PpuRegisters {
        PpuRegisters {
            lcdc: Lcdc::create(),
            scy: 0,
            ly: 0,
            bgp: Palette::create(),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum PixelSource {
    Bg,
    Sprite(u8),
}

#[derive(Copy, Clone, Debug)]
pub struct Pixel {
    value: Integer<u8, packed_bits::Bits2>,
    source: PixelSource,
}

#[derive(Copy, Clone, Debug)]
pub struct Position {
    row: u8,
    col: u8,
}

#[derive(Copy, Clone, Debug)]
pub struct Draw {
    pixel: Pixel,
    position: Position,
}

/*
struct PixelFifo {
    fifo : VecDeque<Pixel>
    row : u8 // 0 - 143
    col : u8 // 0 - 159
}
impl PixelFifo {
    // TODO: Use a pallette
    fn fill(&mut self, pixels: [Pixel; 8]) {
        self.fifo.append(pixels.into())
    }

    // once every 4MHz clock tick (during Pixel Transfer)
    // TODO: Should this stop at 160 or should the caller do this?
    fn pop(&mut self) -> Option<Draw> {
        if self.fifo.len() < 8 {
            None
        } else {
            let x = self.fifo.pop_front().expect("we already checked length");
            let ret = Draw { color: x, position: Position { row: self.row, col: self.col } };
            self.col += 1;
            ret
        }
    }
}

#[derive(PartialEq)]
enum PixelFetcherMode {
    ReadTile,
    ReadData0(u8), // the tile_number
    ReadData1(u8, u8), // the tile_number, the first byte
    Sleep,
}*/

/*
 *
 *  TileMap Data (256 tiles total)
 *  (some of them are bg some are fg)
 *  -----------------------------
 *  | 16bytes (2bytes per line) |
 *  | 8x8 pixels                |
 *  |            ...            |
 *
 *
 *
 *  Background map (32x32) (1024)
 *  ------------------------------
 *  |ptrs                        |
 *  |     (SCX, SCY)             |
 *  |    ---------------         |
 *  |    |xxx          |         |
 *  |    |             |         |
 *  |    |             |         |
 *  |    ---------------         |
 *  |                            |
 *  |                            |
 *  ------------------------------
 */

/*struct PixelFetcher<'a> {
    memory: &'a Memory,

    // increments each round of the pixel fetching
    tile_map_col: u16,
    tile_map_row:

    // increments each step
    mode: PixelFetcherMode
}
// clocked at 2MHz
impl<'a> PixelFetcher<'a> {
    fn create<'b>(memory: &'b Memory) -> PixelFetcher<'b> {
        PixelFetcher {
            memory,
            tile_map_offset: 0,
            mode: PixelFetcherMode::ReadTile,
            tile_number: None,
            data0: None,
        }
    }

    fn read_tile(&mut self) {
        assert_eq!(self.mode, PixelFetcherMode::ReadTile);

        let base_addr = self.memory.ppu.lcdc.bg_window_tile_data.base_addr();
        let tile_number =
            self.memory.ld8(base_addr.offset_by(tile_map_offset, Direction::Pos));
        self.mode = PixelFetcherMode::ReadData0(tile_number);
    }

    fn step(&mut self) -> Option<[Pixel; 8]> {
        match self.mode {
            PixelFetcherMode::ReadTile =>  {
                let base_addr = self.memory.ppu.lcdc.bg_window_tile_data.base_addr();
                let tile_number =
                    self.memory.ld8(base_addr.offset_by(tile_map_offset, Direction::Pos));
                self.mode = PixelFetcherMode::ReadData0(tile_number);
            },
            PixelFetcherMode::ReadData0(tile_number) => {
                // TODO: How do we get tile_number to an address?
                let base_addr = self.memory.ppu.lcdc.bg_tile_map_display.base_addr();
                let data0 = self.memory.ld8(base_addr.offset_by(tile_number * 16, Direction::Pos));
                self.mode = PixelFetcherMode::ReadData1(tile_number, data0);
            },
        }
    }
}


pub struct Ppu {
    pixel_fifo: PixelFifo,
    pixel_fetcher: PixelFetcher,

    ppu_section_ticks: u32
}
impl Ppu {
    pub fn forwards(&mut self, duration: u32) {
        self.ppu_section_ticks += duration*4;
        self.ppu_section_ticks %= 17_556*4; // for now hardcoded
    }
}*/

#[cfg(test)]
mod test {
    use ppu::*;
    #[test]
    fn i_understand_lsb() {
        let bgp = Palette::create();
        let x = bgp.pack()[0];
        assert_eq!(x, 0b1110_0100 as u8)
    }

}
