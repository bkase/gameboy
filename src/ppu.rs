#![allow(dead_code)]
#![allow(clippy::range_minus_one)]

use mem::{Addr, Direction, Memory};
use packed_struct::prelude::*;
use read_view_u8::*;
use screen::{Coordinate, Rgb, Screen};

pub const DEBUG: bool = false;

#[derive(PackedStruct, Debug, Clone, Copy, PartialEq)]
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

#[derive(PackedStruct, Debug, PartialEq)]
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

    pub fn display(&self) -> bool {
        self.lcd_control_operation
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

#[derive(Debug, PartialEq)]
pub struct PpuRegisters {
    pub lcdc: Lcdc,
    pub scy: u8,
    pub scx: u8,
    pub bgp: Palette,
    pub obp0: Palette,
    pub obp1: Palette,
    moment: Moment,
}

impl PpuRegisters {
    pub fn create() -> PpuRegisters {
        PpuRegisters {
            lcdc: Lcdc::create(),
            scy: 0,
            scx: 0,
            bgp: Palette::create(),
            obp0: Palette::create(),
            obp1: Palette::create(),
            moment: Moment(0),
        }
    }

    // TODO: Flesh this out so writes work and more data is sent
    pub fn lcdc_stat_controller_mode(&self) -> u8 {
        let result = match self.moment.mode() {
            Mode::Hblank => 0,
            Mode::Vblank => 1,
            Mode::OamSearch => 2,
            Mode::PixelTransfer => 3,
        };

        assert!(result <= 3);

        result
    }

    #[inline]
    pub fn ly(&self) -> u8 {
        self.moment.line()
    }
}

#[derive(Copy, Clone, Debug)]
pub enum PixelSource {
    Bg,
    Sprite(usize),
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
const BG_ROWS_SUB_ONE: u8 = 255;
const BG_COLS_SUB_ONE: u8 = 255;
const TILE_DEBUG_ROWS: u8 = 192;
const VBLANK_ROWS: u8 = 10;

#[derive(Copy, Clone, Debug, PartialEq)]
/// Invariant: .0 <= COLS * ROWS
struct Moment(u16);

#[derive(Copy, Clone, Debug)]
pub struct TriggeredVblank(pub bool);

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

    fn advance(&mut self, amount: u32) -> TriggeredVblank {
        let pre_mod = u32::from(self.0).wrapping_add(amount * 4);
        *self = Moment((pre_mod % (u32::from(COLS) * u32::from(ROWS))) as u16);
        if pre_mod >= u32::from(COLS) * u32::from(SCREEN_ROWS) {
            TriggeredVblank(true)
        } else {
            TriggeredVblank(false)
        }
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
        let TriggeredVblank(b) = moment.advance(BIG);
        assert!(b);
        assert_eq!(moment.line(), 2);
        assert_eq!(moment.mode(), Mode::PixelTransfer);
    }
}

#[derive(Debug)]
pub struct Ppu {
    pub screen: Screen,
    pub debug_wide_screen: Screen,
    pub debug_tile_screen: Screen,
    dirty: bool,
    visible_sprites_mask: [Option<usize>; 10],
}

const TILES_PER_ROW: u8 = 32;
const TILES_PER_ROW_TILE_VIEW: u8 = 16;

#[derive(PrimitiveEnum_u8, Clone, Copy, Debug, PartialEq)]
pub enum OamEntryPriority {
    AboveEverything = 0,
    BelowMost = 1, // still above 00 on bg tiles
}

#[derive(PrimitiveEnum_u8, Clone, Copy, Debug, PartialEq)]
pub enum OamEntryPalette {
    ObjectPalette0 = 0,
    ObjectPalette1 = 1,
}

#[derive(PackedStruct, Debug, Clone, Copy, PartialEq)]
#[packed_struct(size_bytes = "4", bit_numbering = "msb0")]
pub struct OamEntry {
    #[packed_field(bytes = "0")]
    pos_y: u8,
    #[packed_field(bytes = "1")]
    pos_x: u8,
    #[packed_field(bytes = "2")]
    tile_number: u8,
    #[packed_field(bits = "24", ty = "enum")]
    priority: OamEntryPriority,
    #[packed_field(bits = "25")]
    flip_x: bool,
    #[packed_field(bits = "26")]
    flip_y: bool,
    #[packed_field(bits = "27", ty = "enum")]
    palette: OamEntryPalette,
}
impl OamEntry {
    pub fn create() -> OamEntry {
        OamEntry::unpack(&[0x00, 0x00, 0x00, 0x00]).expect("Fits within a u8;4")
    }

    fn get_palette(&self, memory: &Memory) -> Palette {
        match self.palette {
            OamEntryPalette::ObjectPalette0 => memory.ppu.obp0,
            OamEntryPalette::ObjectPalette1 => memory.ppu.obp1,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ScreenChoice {
    FullDebug,
    Real,
    TileDebug,
}

impl Ppu {
    pub fn create() -> Ppu {
        Ppu {
            screen: Screen::create(160, 144),
            debug_wide_screen: Screen::create(256, 256),
            debug_tile_screen: Screen::create(16 * 8, 24 * 8),
            dirty: false,
            visible_sprites_mask: [None; 10],
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
    fn tile_row(&self, b0: u8, b1: u8, sprite_index_option: Option<usize>) -> Vec<Pixel> {
        Self::compute_tile_row(b0, b1)
            .map(|num| Pixel {
                value: num.into(),
                source: match sprite_index_option {
                    None => PixelSource::Bg,
                    Some(i) => PixelSource::Sprite(i),
                },
            })
            .collect()
    }

    fn sprite_mux(&self, memory: &Memory, row: u8, row_pixels: &mut Vec<Pixel>) {
        self.visible_sprites_mask
            .iter()
            .filter_map(|x| *x)
            .for_each(|i| {
                let entry = memory.sprite_oam[i];

                // TODO: Implement flip_x and flip_y
                let sprite_pixels = self.pixels_for_tile_number(
                    memory,
                    entry.tile_number,
                    Addr::directly(0x8000),
                    row,
                    Some(i),
                );

                let width = row_pixels.len();
                (0..8).for_each(|j| {
                    let idx = usize::from(entry.pos_x) + j;
                    // at the offset, we're still on the screen
                    if idx >= 8 && idx < width - 8 &&
                    // we only overwrite when the priority is above everything
                    // or the existing value is transparent (0x00)
                      (entry.priority == OamEntryPriority::AboveEverything
                        || row_pixels[idx-8].value == 0.into())
                    {
                        row_pixels[idx - 8] = sprite_pixels[j];
                    }
                })
            });
    }

    fn pixels_for_tile_number(
        &self,
        memory: &Memory,
        tile_number: u8,
        tiles_base_addr: Addr,
        row: u8,
        sprite_index_option: Option<usize>,
    ) -> Vec<Pixel> {
        let b0 = memory.ld8(tiles_base_addr.offset(
            u16::from(tile_number) * 16 + 2 * (u16::from(row % 8)),
            Direction::Pos,
        ));
        let b1 = memory.ld8(tiles_base_addr.offset(
            u16::from(tile_number) * 16 + 2 * (u16::from(row % 8)) + 1,
            Direction::Pos,
        ));

        self.tile_row(b0, b1, sprite_index_option)
    }

    fn paint_row(&mut self, memory: &Memory, row: u8, screen_choice: ScreenChoice) {
        let scx = match screen_choice {
            ScreenChoice::Real => memory.ppu.scx,
            ScreenChoice::FullDebug => 0,
            ScreenChoice::TileDebug => 0,
        };
        let scy = match screen_choice {
            ScreenChoice::Real => memory.ppu.scy,
            ScreenChoice::FullDebug => 0,
            ScreenChoice::TileDebug => 0,
        };
        let effective_row = row.wrapping_add(scy);
        self.oam_search(memory, effective_row);

        let tiles_base_addr = match screen_choice {
            ScreenChoice::Real | ScreenChoice::FullDebug => {
                memory.ppu.lcdc.bg_window_tile_data.base_addr()
            }
            ScreenChoice::TileDebug => {
                Addr::directly(if row >= (16 * 8) { 0x9000 } else { 0x8000 })
            }
        };

        let map_base_addr = memory.ppu.lcdc.bg_tile_map_display.base_addr();

        let total_tiles = match screen_choice {
            ScreenChoice::Real => (self.screen.width / 8) as u8,
            ScreenChoice::FullDebug => (self.debug_wide_screen.width / 8) as u8,
            ScreenChoice::TileDebug => (self.debug_tile_screen.width / 8) as u8,
        };

        // for each of the tiles on screen on the row
        // get all the bg pixels
        let mut pixels: Vec<Pixel> = ((scx / 8)..(total_tiles + (scx / 8)))
            .flat_map(|i| {
                let tile_number = match screen_choice {
                    ScreenChoice::Real | ScreenChoice::FullDebug => {
                        memory.ld8(map_base_addr.offset(
                            u16::from(effective_row / 8) * u16::from(TILES_PER_ROW) + u16::from(i),
                            Direction::Pos,
                        ))
                    }
                    ScreenChoice::TileDebug => {
                        // when this overflows, it coincides with shifting to the
                        // 0x9000 base addr
                        (row / 8) * TILES_PER_ROW_TILE_VIEW + i
                    }
                };

                self.pixels_for_tile_number(
                    memory,
                    tile_number,
                    tiles_base_addr,
                    effective_row,
                    None,
                )
            })
            .collect();

        // mux in the sprites
        match screen_choice {
            ScreenChoice::Real => self.sprite_mux(memory, effective_row, &mut pixels),
            ScreenChoice::TileDebug | ScreenChoice::FullDebug => (),
        };

        // color and blit
        let pixel_lut: [(u8, u8, u8); 4] =
            [(255, 255, 255), (98, 78, 81), (220, 176, 181), (0, 0, 0)];
        pixels.into_iter().enumerate().for_each(|(j, pixel)| {
            let pallette = match pixel.source {
                PixelSource::Bg => memory.ppu.bgp,
                PixelSource::Sprite(i) => memory.sprite_oam[i].get_palette(memory),
            };
            let idx: u8 = match pixel.value.into() {
                0b00 => pallette.dot00,
                0b01 => pallette.dot01,
                0b10 => pallette.dot10,
                0b11 => pallette.dot11,
                _ => panic!("Invariant violation"),
            }
            .into();

            let (r, g, b) = pixel_lut[idx as usize];
            let rgb = Rgb { r, g, b };
            let coord = Coordinate { x: j as u8, y: row };

            match screen_choice {
                ScreenChoice::Real => {
                    if memory.ppu.lcdc.lcd_control_operation {
                        self.screen.bang(rgb, coord)
                    } else {
                        self.screen.bang(Rgb::white(), coord)
                    }
                }
                ScreenChoice::FullDebug => {
                    if memory.ppu.lcdc.lcd_control_operation {
                        self.debug_wide_screen.bang(rgb, coord)
                    } else {
                        self.debug_wide_screen.bang(Rgb::white(), coord)
                    }
                }
                ScreenChoice::TileDebug => self.debug_tile_screen.bang(rgb, coord),
            };
        });
    }

    pub fn advance(&mut self, memory: &mut Memory, duration: u32) -> TriggeredVblank {
        let triggered_vblank = memory.ppu.moment.advance(duration);
        self.dirty = true;
        triggered_vblank
    }

    // Fills the visible_sprites_mask
    pub fn oam_search(&mut self, memory: &Memory, row: u8) {
        let h = match memory.ppu.lcdc.obj_size {
            ObjSize::_8x8 => 8,
            ObjSize::_8x16 => 16,
        };
        // TODO: Should we pre-allocate this extra buffer
        let mut entries: Vec<(usize, &OamEntry)> = memory
            .sprite_oam
            .iter()
            .enumerate()
            // keep those that are visible on this row
            .filter(|(_, e)| e.pos_x != 0 && e.pos_y <= row + 16 && e.pos_y + h > row + 16)
            .collect();
        // lower x earlier, for ties delegate to the earlier entry
        entries.sort_by(|(ai, a), (bi, b)| a.pos_x.cmp(&b.pos_x).then(ai.cmp(bi)));

        // start fresh so we don't have to worry about straggling visible_sprites
        (0..self.visible_sprites_mask.len()).for_each(|i| self.visible_sprites_mask[i] = None);

        // reversing so we can put the ones that are drawn earlier first
        // in the visible mask
        let len = self.visible_sprites_mask.len();
        entries.iter().rev().enumerate().for_each(|(i, (ei, _))| {
            if i < len {
                self.visible_sprites_mask[i] = Some(*ei)
            }
        });
    }

    pub fn repaint(&mut self, memory: &Memory) {
        if self.dirty {
            self.dirty = false;

            (0..=SCREEN_ROWS - 1).for_each(|row| self.paint_row(memory, row, ScreenChoice::Real));
            if DEBUG {
                (0..=BG_ROWS_SUB_ONE)
                    .for_each(|row| self.paint_row(memory, row, ScreenChoice::FullDebug));
                (0..=TILE_DEBUG_ROWS - 1)
                    .for_each(|row| self.paint_row(memory, row, ScreenChoice::TileDebug));
            }
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

    #[test]
    fn oam_entry_pack_unpack() {
        let bytes: &[u8; 4] = &[0x80, 0x10, 0x58, 0];
        let entry = OamEntry::unpack(bytes).expect("four bytes");
        println!("entry {:?}", entry);
        assert!(entry.pos_y == 0x80);
        assert!(entry.pos_x == 0x10);
        assert!(entry.tile_number == 0x58);
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
