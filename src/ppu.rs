use packed_struct::prelude::*;

pub trait ReadViewU8 {
    fn read(&self) -> u8;
}

pub trait ViewU8 : ReadViewU8 {
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
#[packed_struct(size_bytes="1", bit_numbering="lsb0")]
pub struct Bgp {
    #[packed_field(bits="0:1")]
    dot00: Integer<u8, packed_bits::Bits2>,
    #[packed_field(bits="2:3")]
    dot01: Integer<u8, packed_bits::Bits2>,
    #[packed_field(bits="4:5")]
    dot10: Integer<u8, packed_bits::Bits2>,
    #[packed_field(bits="6:7")]
    dot11: Integer<u8, packed_bits::Bits2>,
}
impl Bgp {
    pub fn create() -> Bgp {
        Bgp { dot00: 0.into(), dot01: 0b01.into(), dot10: 0b10.into(), dot11: 0b11.into() }
    }
}
impl ReadViewU8 for Bgp {
    fn read(&self) -> u8 {
        self.pack()[0]
    }
}
impl ViewU8 for Bgp {
    fn set(&mut self, n: u8) {
        *self = Bgp::unpack(&[n]).expect("it's 8bits")
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

#[derive(PrimitiveEnum_u8, Clone, Copy, Debug, PartialEq)]
pub enum BgWindowTileData {
    _8800_97ff = 0,
    _8000_8fff = 1, // same area as OBJ
}

#[derive(PackedStruct)]
#[packed_struct(size_bytes="1", bit_numbering="lsb0")]
pub struct Lcdc {
    #[packed_field(bits="0")]
    bg_and_window_display: bool,
    #[packed_field(bits="1")]
    obj_display: bool,
    #[packed_field(bits="2", ty="enum")]
    obj_size: ObjSize,
    #[packed_field(bits="3", ty="enum")]
    bg_tile_map_display: TileMapDisplay,
    #[packed_field(bits="4", ty="enum")]
    bg_window_tile_data: BgWindowTileData,
    #[packed_field(bits="5")]
    window_display: bool,
    #[packed_field(bits="6", ty="enum")]
    window_tile_map_data: TileMapDisplay,
    #[packed_field(bits="7")]
    lcd_control_operation: bool,
}
impl Lcdc {
    fn create() -> Lcdc {
        Lcdc::unpack(&[0x91]).expect("Fits within a u8")
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

pub struct Ppu {
    pub lcdc: Lcdc,
    pub scy: u8,
    pub ly: u8, // read/only
    pub bgp: Bgp,
}

impl Ppu {
    pub fn create() -> Ppu {
        Ppu {
            lcdc: Lcdc::create(),
            scy: 0,
            ly: 0,
            bgp: Bgp::create(),
        }
    }
}

#[cfg(test)]
mod test {
    use ppu::*;
  #[test]
  fn i_understand_lsb() {
    let bgp = Bgp::create();
    let x = bgp.pack()[0];
    assert_eq!(x, 0b11100100 as u8)
  }

}
