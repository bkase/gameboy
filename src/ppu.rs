#![allow(dead_code)]
#![allow(clippy::range_minus_one)]

use mem::{Addr, Direction, Memory};
use packed_struct::prelude::*;
use read_view_u8::*;
use screen::{Coordinate, Rgb, Screen};

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
    pub scx: u8,
    pub ly: u8, // read/only
    pub bgp: Palette,
}

impl PpuRegisters {
    pub fn create() -> PpuRegisters {
        PpuRegisters {
            lcdc: Lcdc::create(),
            scy: 0,
            scx: 0,
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
 *       114 cols      20                  63          114
 *       --------------------------------------------
 *       | OAM search   | Pixel Transfer   | Hblank
 *  144  |
 *  lines| 20clocks     | 43+ clocks       | 51- clocks
 *       |
 *       |
 *       --------------------------------------------
 * 10lines          Vblank (1140 clocks total)
 *       -------------------------------------------
 *
 */

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Mode {
    OamSearch,
    PixelTransfer,
    Hblank,
    Vblank,
}
impl Mode {
    fn working(self) -> bool {
        match self {
            Mode::OamSearch => true,
            Mode::PixelTransfer => true,
            Mode::Hblank => false,
            Mode::Vblank => false,
        }
    }
}

const COLS: u8 = 114;
const ROWS: u8 = 154;
const SCREEN_ROWS: u8 = 144;
const VBLANK_ROWS: u8 = 10;

#[derive(Copy, Clone, Debug)]
/// Invariant: .0 <= COLS * ROWS
struct Moment(u16);

impl Moment {
    /// Returns number between 0 and ROWS exclusive
    fn line(self) -> u8 {
        (self.0 / u16::from(COLS)) as u8
    }

    fn mode(self) -> Mode {
        assert!(self.0 <= u16::from(COLS) * u16::from(ROWS));

        if self.0 < u16::from(COLS) * u16::from(SCREEN_ROWS) {
            let pos = self.0 % u16::from(COLS);
            match pos {
                0..20 => Mode::OamSearch,
                20..63 => Mode::PixelTransfer,
                63..114 => Mode::Hblank,
                _ => panic!("Unexpected"),
            }
        } else {
            Mode::Vblank
        }
    }

    fn advance(&mut self, amount: u32) {
        *self = Moment(
            (u32::from(self.0).wrapping_add(amount) % (u32::from(COLS) * u32::from(ROWS))) as u16,
        );
    }
}

#[cfg(test)]
mod moments_test {
    use ppu::*;

    const BIG: u32 = (ROWS as u32) * (COLS as u32);

    #[test]
    fn moment_advance() {
        let mut moment = Moment(2 * u16::from(COLS) + 24);
        assert_eq!(moment.line(), 2);
        assert_eq!(moment.mode(), Mode::PixelTransfer);
        moment.advance(BIG);
        assert_eq!(moment.line(), 2);
        assert_eq!(moment.mode(), Mode::PixelTransfer);
    }
}

pub struct Ppu {
    pub screen: Screen,
    moment: Moment,
    dirty: bool,
}

const TILES_PER_ROW: u8 = 32;

impl Ppu {
    pub fn create() -> Ppu {
        Ppu {
            screen: Screen::create(160, 144),
            moment: Moment(0),
            dirty: false,
        }
    }

    fn compute_tile_row(b0: u8, b1: u8) -> impl Iterator<Item = u8> {
        (0..=7)
            .map(move |i| (b0 >> (7 - i)) & 0x1)
            .zip((0..=7).map(move |i| (b1 >> (7 - i)) & 0x1))
            .map(|(bit0, bit1)| match (bit0, bit1) {
                (0, 0) => 0,
                (1, 0) => 1,
                (0, 1) => 2,
                (1, 1) => 3,
                (_, _) => panic!("impossible"),
            })
    }

    // TODO: There's probably some bit hacks I can do to make this faster
    //       if necessary
    fn tile_row(&self, b0: u8, b1: u8) -> Vec<Pixel> {
        Self::compute_tile_row(b0, b1)
            .map(|num| Pixel {
                value: num.into(),
                source: PixelSource::Bg, // TODO: determine if sprite or not
            })
            .collect()
    }

    pub fn paint(&mut self, memory: &Memory, row: u8) {
        let scx = memory.ppu.scx;
        let scy = memory.ppu.scy;
        let pallette = &memory.ppu.bgp;
        let effective_row = row.wrapping_add(scy);

        let tiles_base_addr = memory.ppu.lcdc.bg_window_tile_data.base_addr();
        let map_base_addr = memory.ppu.lcdc.bg_tile_map_display.base_addr();
        // for each of the 18 tiles on screen on the row
        (scx..(18 + scx)).for_each(|i| {
            let tile_number = memory.ld8(map_base_addr.offset(
                u16::from(effective_row / 8) * u16::from(TILES_PER_ROW) + u16::from(i),
                Direction::Pos,
            ));

            let b0 = memory.ld8(tiles_base_addr.offset(
                u16::from(tile_number) * 16 + 2 * (u16::from(effective_row % 8)),
                Direction::Pos,
            ));
            let b1 = memory.ld8(tiles_base_addr.offset(
                u16::from(tile_number) * 16 + 2 * (u16::from(effective_row % 8)) + 1,
                Direction::Pos,
            ));

            let pixels = self.tile_row(b0, b1);
            let pixel_lut: [(u8, u8, u8); 4] =
                [(255, 255, 255), (98, 78, 81), (220, 176, 181), (0, 0, 0)];

            pixels.into_iter().enumerate().for_each(|(j, pixel)| {
                let idx: u8 = match pixel.value.into() {
                    0b00 => pallette.dot00,
                    0b01 => pallette.dot01,
                    0b10 => pallette.dot10,
                    0b11 => pallette.dot11,
                    _ => panic!("Invariant violation"),
                }
                .into();

                let (r, g, b) = pixel_lut[idx as usize];
                self.screen.bang(
                    Rgb { r, g, b },
                    Coordinate {
                        x: ((i as u8) * 8 + (j as u8)) as u8,
                        y: row,
                    },
                )
            });
        })
    }

    pub fn advance(&mut self, memory: &mut Memory, duration: u32) {
        self.moment.advance(duration);
        memory.ppu.ly = self.moment.line();
        self.dirty = true;
    }

    pub fn repaint(&mut self, memory: &Memory) {
        if self.dirty {
            self.dirty = false;
            (0..=SCREEN_ROWS - 1).for_each(|row| self.paint(memory, row));
        }
    }

    pub fn force_repaint(&mut self, memory: &Memory) {
        self.dirty = true;
        self.repaint(memory);
    }
}

#[cfg(test)]
mod tiles {
    use ppu::*;

    // b0: 0001_1100b
    // b1: 0101_1001b
    // -------------
    //     0203_3102
    #[test]
    fn compute_tile_row() {
        let got: Vec<u8> = Ppu::compute_tile_row(0b0001_1100, 0b0101_1001).collect();
        println!("GOT {:?}", got);
        assert_eq!(got, vec![0, 2, 0, 3, 3, 1, 0, 2])
    }

}

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
