#![allow(dead_code)]
#![allow(clippy::range_minus_one)]

use mem::{Addr, Direction, Memory};
use monoid::Monoid;
use packed_struct::prelude::*;
use screen::{Coordinate, Rgb, Screen};
use std::ops::RangeInclusive;
use web_utils::log;

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
            // HACK: do it wrong on purpose
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

#[derive(Clone, Debug, PartialEq)]
enum WrappingRange {
    Regular(RangeInclusive<u8>),
    Inverted(u8, RangeInclusive<u8>),
    Empty,
}
impl Monoid for WrappingRange {
    fn combine(self, other: WrappingRange) -> WrappingRange {
        match (self, other) {
            (WrappingRange::Empty, v) | (v, WrappingRange::Empty) => v,
            (WrappingRange::Regular(r1), WrappingRange::Regular(r2)) => {
                WrappingRange::Regular(*r1.start()..=*r2.end())
            }
            (WrappingRange::Inverted(m1, r1), WrappingRange::Inverted(m2, r2)) => {
                assert_eq!(m1, m2);
                WrappingRange::Inverted(m1, *r1.start()..=*r2.end())
            }
            // be lazy and just invalidate everything when we hit an inverted and a regular
            (WrappingRange::Regular(_), WrappingRange::Inverted(_, _))
            | (WrappingRange::Inverted(_, _), WrappingRange::Regular(_)) => {
                WrappingRange::Regular(0..=(SCREEN_ROWS - 1))
            }
        }
    }
}

impl WrappingRange {
    fn entrypoint(&self) -> u8 {
        match self {
            WrappingRange::Empty => 0,
            WrappingRange::Regular(r) => *r.start(),
            WrappingRange::Inverted(_, r) => *r.end() + 1,
        }
    }

    fn of(max: u8, lower: Option<u8>, upper: Option<u8>) -> WrappingRange {
        assert!(lower.unwrap_or_else(|| 0) <= max);
        assert!(upper.unwrap_or_else(|| 0) <= max);

        match (lower, upper) {
            (None, None) => WrappingRange::Empty,
            (Some(lower), None) => WrappingRange::Regular(lower..=max),
            (None, Some(upper)) => WrappingRange::Regular(0..=upper),
            (Some(lower), Some(upper)) => {
                if upper < lower {
                    if lower - upper > 1 {
                        WrappingRange::Inverted(max, (upper + 1)..=(lower - 1))
                    } else {
                        WrappingRange::Regular(0..=max)
                    }
                } else {
                    WrappingRange::Regular(lower..=upper)
                }
            }
        }
    }
}

struct WrappingRangeIterator {
    cursor: u8,
    v: WrappingRange,
    wrapped: bool,
}
impl Iterator for WrappingRangeIterator {
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        match &self.v {
            WrappingRange::Empty => None,
            WrappingRange::Regular(r) => {
                if self.cursor <= *r.end() {
                    let last = self.cursor;
                    self.cursor += 1;
                    Some(last)
                } else {
                    None
                }
            }
            WrappingRange::Inverted(max, r) => {
                // -1 is safe because we +1 when we create from `of`
                if self.cursor < *r.start() || !self.wrapped {
                    let last = self.cursor;
                    if self.cursor < *max {
                        self.cursor += 1;
                        Some(last)
                    } else {
                        assert_eq!(self.cursor, *max);
                        if *r.start() == 0 {
                            None
                        } else {
                            self.wrapped = true;
                            self.cursor = 0;
                            Some(last)
                        }
                    }
                } else {
                    None
                }
            }
        }
    }
}

impl IntoIterator for WrappingRange {
    type Item = u8;
    type IntoIter = WrappingRangeIterator;
    fn into_iter(self) -> WrappingRangeIterator {
        let entry = self.entrypoint();
        WrappingRangeIterator {
            wrapped: false,
            cursor: entry,
            v: self,
        }
    }
}

#[cfg(test)]
mod wrapping_test {
    use ppu::*;
    #[test]
    fn wrapping_of() {
        // simple
        assert_eq!(
            WrappingRange::of(10, Some(1), Some(4)),
            WrappingRange::Regular(1..=4)
        );
        // overflow/underflow upper
        assert_eq!(
            WrappingRange::of(10, Some(1), None),
            WrappingRange::Regular(1..=10)
        );
        // overflow lower
        assert_eq!(
            WrappingRange::of(10, None, Some(4)),
            WrappingRange::Regular(0..=4)
        );
        // overflow both
        assert_eq!(WrappingRange::of(10, None, None), WrappingRange::Empty);
        // inverted
        assert_eq!(
            WrappingRange::of(10, Some(4), Some(1)),
            WrappingRange::Inverted(10, 2..=3)
        );
        // oneline-inversion is empty
        assert_eq!(
            WrappingRange::of(10, Some(3), Some(2)),
            WrappingRange::Regular(0..=10)
        );
    }

    #[test]
    fn wrapping_iterator() {
        // empty is empty
        let empty: Vec<u8> = WrappingRange::Empty.into_iter().collect();
        assert_eq!(empty, vec![]);

        // regular
        let regular: Vec<u8> = WrappingRange::Regular(3..=10).into_iter().collect();
        let expected: Vec<u8> = (3..=10).into_iter().collect();
        assert_eq!(regular, expected);

        // inverted
        let inverted: Vec<u8> = WrappingRange::Inverted(10, 2..=3).into_iter().collect();
        assert_eq!(inverted, vec![4, 5, 6, 7, 8, 9, 10, 0, 1]);
    }

}

#[derive(Copy, Clone, Debug)]
/// Invariant: .0 <= COLS * ROWS
struct Moment(u16);

impl Moment {
    /// Returns number between 0 and ROWS exclusive
    fn line(self) -> u8 {
        (self.0 % u16::from(ROWS)) as u8
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

    // I wrote this on an airplane, sorry for the grossness
    //
    // TODO: there's an off-by-one in this case, but worst cast we accidentally just draw a frame twice. The off-by-one is because we can start at the row before blank and end the row after and only sometimes do we want to redraw all.
    fn determine_repaints(
        amount: u32,
        start_line: u8,
        start_mode: Mode,
        end_line: u8,
        end_mode: Mode,
    ) -> WrappingRange {
        // sanity
        assert!(start_line < ROWS);
        assert!(end_line < ROWS);

        // we need to repaint everything
        // either because the amount is enormous
        if amount >= u32::from(COLS) * u32::from(ROWS) ||
            // or the amount isn't quite the whole screen but it's bigger than vblank and
            (amount >= u32::from(VBLANK_ROWS) * u32::from(COLS) &&
             // we start in or near vblank
             (start_mode == Mode::Vblank ||
                (start_line == SCREEN_ROWS-1 && start_mode == Mode::Hblank)) &&
             // we end in or near vlank
             (end_mode == Mode::Vblank ||
                (end_line == 0 && end_mode.working())))
        {
            WrappingRange::Regular(0..=(SCREEN_ROWS - 1))
        // we need to repaint either zero or one lines
        } else if amount < u32::from(COLS) && start_line == end_line {
            match (start_mode, end_mode) {
                (Mode::PixelTransfer, Mode::Hblank) | (Mode::OamSearch, Mode::Hblank) => {
                    WrappingRange::Regular(start_line..=start_line)
                }
                (_, _) => WrappingRange::Empty,
            }
        // we started and ended at the same line, but we wrapped around
        } else if start_line == end_line {
            match start_mode {
                // if we started before hblank, we can repaint everything
                Mode::OamSearch | Mode::PixelTransfer => {
                    WrappingRange::Regular(0..=(SCREEN_ROWS - 1))
                }
                // if we're in hblank, we need to skip just this line
                Mode::Hblank => WrappingRange::Inverted(SCREEN_ROWS - 1, start_line..=start_line),
                // if we're in vblank we should do nothing
                Mode::Vblank => WrappingRange::Empty,
            }
        } else {
            // we started and stopped at different lines
            assert_ne!(start_line, end_line);
            // start either on this line or on the next depending on which mode we're in
            let start_point = match start_mode {
                Mode::OamSearch | Mode::PixelTransfer => Some(start_line),
                Mode::Hblank => {
                    if start_line + 1 < SCREEN_ROWS {
                        Some(start_line + 1)
                    } else {
                        None // overflow
                    }
                }
                Mode::Vblank => {
                    if start_line == ROWS - 1 {
                        Some(0)
                    } else {
                        None // overflow
                    }
                }
            };
            // end on either this line or the prior depending on which mode we're in
            let end_point = match end_mode {
                Mode::OamSearch | Mode::PixelTransfer => {
                    if end_line == 0 {
                        None // underflow
                    } else {
                        Some(end_line - 1)
                    }
                }
                Mode::Hblank => Some(end_line),
                Mode::Vblank => {
                    if end_line == SCREEN_ROWS {
                        Some(SCREEN_ROWS - 1)
                    } else {
                        None // overflow
                    }
                }
            };

            // if start_line and end_line were next to each other
            // but the point edge cases causes them to cross, we want empty
            if start_line + 1 == end_line && start_point > end_point {
                WrappingRange::Empty
            } else {
                WrappingRange::of(SCREEN_ROWS - 1, start_point, end_point)
            }
        }
    }

    /// Returns a range of lines onto which we need to repaint
    /// Bounds: 0..SCREEN_ROWS
    fn advance(&mut self, amount: u32) -> WrappingRange {
        let start_line = self.line();
        // TODO: Fix my mode calculation
        let start_mode = if start_line > 143 {
            Mode::Vblank
        } else {
            self.mode()
        };

        *self = Moment(
            (u32::from(self.0).wrapping_add(amount) % (u32::from(COLS) * u32::from(ROWS))) as u16,
        );
        let end_line = self.line();
        let end_mode =
            // TODO: Fix my mode calculation
            if end_line > 143 {
                Mode::Vblank
            } else {
                self.mode()
            };

        Self::determine_repaints(amount, start_line, start_mode, end_line, end_mode)
    }
}

#[cfg(test)]
mod moments_test {
    use ppu::*;

    const BIG: u32 = (ROWS as u32) * (COLS as u32);

    #[test]
    fn large_simple() {
        assert_eq!(
            Moment::determine_repaints(BIG, 0, Mode::Hblank, 3, Mode::Hblank),
            WrappingRange::Regular(0..=SCREEN_ROWS - 1)
        );
        assert_eq!(
            Moment::determine_repaints(BIG, 0, Mode::OamSearch, 100, Mode::Hblank),
            WrappingRange::Regular(0..=SCREEN_ROWS - 1)
        );
    }

    #[test]
    fn vblank_larges() {
        // normal
        assert_eq!(
            Moment::determine_repaints(BIG - 2, 146, Mode::Vblank, 148, Mode::Vblank),
            WrappingRange::Regular(0..=SCREEN_ROWS - 1)
        );

        // inverted
        assert_eq!(
            Moment::determine_repaints(BIG - 2, 148, Mode::Vblank, 146, Mode::Vblank),
            WrappingRange::Regular(0..=SCREEN_ROWS - 1)
        );

        // near vblank lower
        assert_eq!(
            Moment::determine_repaints(BIG - 2, SCREEN_ROWS - 1, Mode::Hblank, 146, Mode::Vblank),
            WrappingRange::Regular(0..=SCREEN_ROWS - 1)
        );

        // near vblank upper
        assert_eq!(
            Moment::determine_repaints(BIG - 2, 148, Mode::Vblank, 0, Mode::OamSearch),
            WrappingRange::Regular(0..=SCREEN_ROWS - 1)
        );

        // near vblank upper/lower
        assert_eq!(
            Moment::determine_repaints(BIG - 1, SCREEN_ROWS - 1, Mode::Hblank, 0, Mode::OamSearch),
            WrappingRange::Regular(0..=SCREEN_ROWS - 1)
        );
    }

    #[test]
    fn zero_or_one() {
        // same region (working)
        assert_eq!(
            Moment::determine_repaints(5, 135, Mode::OamSearch, 135, Mode::OamSearch),
            WrappingRange::Empty
        );

        // same region (not working)
        assert_eq!(
            Moment::determine_repaints(5, 135, Mode::Hblank, 135, Mode::Hblank),
            WrappingRange::Empty
        );

        // different region
        assert_eq!(
            Moment::determine_repaints(5, 135, Mode::OamSearch, 135, Mode::Hblank),
            WrappingRange::Regular(135..=135)
        );
    }

    // TODO: More of these tests
    #[test]
    fn different_lines() {
        // begin oam, end oam
        assert_eq!(
            Moment::determine_repaints(
                (COLS as u32) * 2,
                136,
                Mode::OamSearch,
                138,
                Mode::OamSearch
            ),
            WrappingRange::Regular(136..=137)
        );

        // begin hblank, end oam
        assert_eq!(
            Moment::determine_repaints((COLS as u32) * 2, 136, Mode::Hblank, 138, Mode::OamSearch),
            WrappingRange::Regular(137..=137)
        );

        // begin hblank, end oam, oneline
        assert_eq!(
            Moment::determine_repaints(COLS as u32 - 50, 137, Mode::Hblank, 138, Mode::OamSearch),
            WrappingRange::Empty
        );

        // begin hblank, end hblank
        assert_eq!(
            Moment::determine_repaints((COLS as u32) * 2, 136, Mode::Hblank, 138, Mode::Hblank),
            WrappingRange::Regular(137..=138)
        );
    }

    #[test]
    fn inversion() {
        // begin oam, end hblank
        assert_eq!(
            Moment::determine_repaints((COLS as u32) * 30, 136, Mode::OamSearch, 26, Mode::Hblank),
            WrappingRange::Inverted(SCREEN_ROWS - 1, 27..=135)
        );
    }
}

pub struct Ppu {
    pub screen: Screen,
    moment: Moment,
    dirty: Option<WrappingRange>,
}

const TILES_PER_ROW: u8 = 32;

impl Ppu {
    pub fn create() -> Ppu {
        Ppu {
            screen: Screen::create(160, 144),
            moment: Moment(0),
            dirty: None,
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
        let effective_row = row.wrapping_add(scy);

        let tiles_base_addr = memory.ppu.lcdc.bg_window_tile_data.base_addr();
        let map_base_addr = memory.ppu.lcdc.bg_tile_map_display.base_addr();
        // for each of the 18 tiles on screen on the row
        ((0 + scx)..(18 + scx)).for_each(|i| {
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

            // TODO: Don't recreate pallette here
            let pallette = Palette::create();

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
        let range = self.moment.advance(duration);
        // HACK: we need this to be more accurate to have bootrom to be happy, but we can just set
        // to 144 whenever it's around 144 and we should be good
        memory.ppu.ly = if self.moment.line() >= 144 && self.moment.line() <= 149 {
            144
        } else {
            self.moment.line()
        };

        let mut hole = None;
        std::mem::swap(&mut self.dirty, &mut hole);
        self.dirty = hole.combine(Some(range));
    }

    pub fn repaint(&mut self, memory: &Memory) {
        let mut dirty = None;
        std::mem::swap(&mut self.dirty, &mut dirty);
        match dirty {
            None => (),
            Some(_range) => {
                WrappingRange::Regular(0..=SCREEN_ROWS - 1)
                    .into_iter()
                    .for_each(|row| self.paint(memory, row));
            }
        }
    }

    pub fn force_repaint(&mut self, memory: &Memory) {
        self.dirty = Some(WrappingRange::Regular(0..=SCREEN_ROWS - 1));
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
