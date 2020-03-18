#![allow(dead_code)]

use packed_struct::prelude::*;
use ppu::{OamEntry, PpuRegisters};
use read_view_u8::*;
use register::R16;
use sound;
use std::convert::TryInto;
use std::fmt;
use std::string::String;

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
pub const BOOTROM: &[u8; 0x100] = include_bytes!("../DMG_ROM.bin");

// Cartridges
pub type Cartridge = &'static [u8; 0x8000];

pub const TETRIS: Cartridge = include_bytes!("../Tetris.GB");

pub const TEST_01: Cartridge =
    include_bytes!("../gb-test-roms/cpu_instrs/individual/01-special.gb");
// include_bytes!("../../mooneye-gb/tests/build/acceptance/instr/daa.gb");

pub const TIC_TAC_TOE: Cartridge = include_bytes!("../tictactoe.gb");
pub const DR_MARIO: Cartridge = include_bytes!("../drmario.gb");

#[derive(Copy, Clone, Debug)]
pub struct TriggeredTimer(pub bool);

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

// emulate the physical hardware, ie. hook this up to the actual events
// reading from the IO register will poll the hardware
#[derive(PackedStruct, Debug)]
#[packed_struct(size_bytes = "1", bit_numbering = "lsb0")]
pub struct JoypadHardware {
    #[packed_field(bits = "0", ty = "enum")]
    a: JoypadSelect,
    #[packed_field(bits = "1", ty = "enum")]
    b: JoypadSelect,
    #[packed_field(bits = "2", ty = "enum")]
    start: JoypadSelect,
    #[packed_field(bits = "3", ty = "enum")]
    select: JoypadSelect,
    #[packed_field(bits = "4", ty = "enum")]
    right: JoypadSelect,
    #[packed_field(bits = "5", ty = "enum")]
    left: JoypadSelect,
    #[packed_field(bits = "6", ty = "enum")]
    up: JoypadSelect,
    #[packed_field(bits = "7", ty = "enum")]
    down: JoypadSelect,
}
impl JoypadHardware {
    fn create() -> JoypadHardware {
        JoypadHardware {
            a: JoypadSelect::Unselected,
            b: JoypadSelect::Unselected,
            start: JoypadSelect::Unselected,
            select: JoypadSelect::Unselected,
            right: JoypadSelect::Unselected,
            left: JoypadSelect::Unselected,
            up: JoypadSelect::Unselected,
            down: JoypadSelect::Unselected,
        }
    }
}

#[derive(PrimitiveEnum_u8, Clone, Copy, Debug, PartialEq)]
pub enum JoypadSelect {
    Selected = 0,
    Unselected = 1,
}

impl JoypadSelect {
    fn or(&self, other: JoypadSelect) -> JoypadSelect {
        match (self, other) {
            (JoypadSelect::Selected, _) => JoypadSelect::Selected,
            (_, JoypadSelect::Selected) => JoypadSelect::Selected,
            _ => JoypadSelect::Unselected,
        }
    }
}

#[derive(PrimitiveEnum_u8, Clone, Copy, Debug, PartialEq)]
pub enum JoypadButton {
    A = 0,
    B = 1,
    Start = 2,
    Select = 3,
}

#[derive(PrimitiveEnum_u8, Clone, Copy, Debug, PartialEq)]
pub enum JoypadDpad {
    Right = 0,
    Left = 1,
    Up = 2,
    Down = 3,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum JoypadKey {
    Dpad(JoypadDpad),
    Button(JoypadButton),
}

#[derive(PackedStruct, Debug, Clone, Copy)]
#[packed_struct(size_bytes = "1", bit_numbering = "lsb0")]
pub struct JoypadRegister {
    #[packed_field(bits = "0", ty = "enum")]
    input_right_or_a: JoypadSelect,
    #[packed_field(bits = "1", ty = "enum")]
    input_left_or_b: JoypadSelect,
    #[packed_field(bits = "2", ty = "enum")]
    input_up_or_select: JoypadSelect,
    #[packed_field(bits = "3", ty = "enum")]
    input_down_or_start: JoypadSelect,
    #[packed_field(bits = "4", ty = "enum")]
    select_direction_keys: JoypadSelect,
    #[packed_field(bits = "5", ty = "enum")]
    select_button_keys: JoypadSelect,
}

impl JoypadRegister {
    fn create() -> JoypadRegister {
        JoypadRegister::unpack(&[0xFF]).expect("Fits within a u8")
    }

    fn unselect_all(&mut self) {
        self.input_down_or_start = JoypadSelect::Unselected;
        self.input_up_or_select = JoypadSelect::Unselected;
        self.input_left_or_b = JoypadSelect::Unselected;
        self.input_right_or_a = JoypadSelect::Unselected;
    }
}

#[derive(Debug)]
pub struct Joypad {
    pub register: JoypadRegister,
    hardware: JoypadHardware,
}

impl Joypad {
    fn create() -> Joypad {
        Joypad {
            register: JoypadRegister::create(),
            hardware: JoypadHardware::create(),
        }
    }

    fn handle_key(&mut self, key: JoypadKey, is_down: bool) {
        let select_button_keys = self.register.select_button_keys;
        let select_direction_keys = self.register.select_direction_keys;
        use web_utils::*;
        log(&format!(
            "Try handling {:?}, is_down: {:}, select_direction_keys {:?}, select_button_keys {:?}",
            key, is_down, select_direction_keys, select_button_keys
        ));

        let perform_set = |_key_bit: u8| {
            // TODO: Perform interrupt here
            ()
        };

        let select_state = if is_down {
            JoypadSelect::Selected
        } else {
            JoypadSelect::Unselected
        };

        match key {
            JoypadKey::Dpad(dpad) => {
                match dpad {
                    JoypadDpad::Right => self.hardware.right = select_state,
                    JoypadDpad::Left => self.hardware.left = select_state,
                    JoypadDpad::Up => self.hardware.up = select_state,
                    JoypadDpad::Down => self.hardware.down = select_state,
                };

                if select_direction_keys == JoypadSelect::Selected {
                    perform_set(dpad.to_primitive())
                }
            }
            JoypadKey::Button(button) => {
                match button {
                    JoypadButton::A => self.hardware.a = select_state,
                    JoypadButton::B => self.hardware.b = select_state,
                    JoypadButton::Start => self.hardware.start = select_state,
                    JoypadButton::Select => self.hardware.select = select_state,
                };

                if select_button_keys == JoypadSelect::Selected {
                    perform_set(button.to_primitive())
                }
            }
        }
    }

    pub fn handle_key_down(&mut self, key: JoypadKey) {
        self.handle_key(key, true);
    }

    pub fn handle_key_up(&mut self, key: JoypadKey) {
        self.handle_key(key, false);
    }

    pub fn poll_and_read(&self) -> u8 {
        // TODO: Should we change this to not need to allocate?
        // it will require making ld8 on mem &mut self I think
        // but since register is a PrimitiveEnum_u8, it may be free to Copy
        let mut reg = self.register;

        let dpad_on = reg.select_direction_keys;
        let button_on = reg.select_button_keys;

        let mux = |dpad, button| match (dpad_on, button_on) {
            (JoypadSelect::Unselected, JoypadSelect::Unselected) => JoypadSelect::Unselected,
            (JoypadSelect::Unselected, JoypadSelect::Selected) => button,
            (JoypadSelect::Selected, JoypadSelect::Unselected) => dpad,
            (JoypadSelect::Selected, JoypadSelect::Selected) => dpad.or(button),
        };

        reg.input_down_or_start = mux(self.hardware.down, self.hardware.start);
        reg.input_up_or_select = mux(self.hardware.up, self.hardware.select);
        reg.input_right_or_a = mux(self.hardware.right, self.hardware.a);
        reg.input_left_or_b = mux(self.hardware.left, self.hardware.b);

        reg.pack()[0]
    }

    // only sets the "Writable" bits of this register (which keys to care about)
    pub fn partial_set(&mut self, n: u8) {
        let curr = self.poll_and_read();
        let mask = 0b00110000;
        let masked = n & mask;
        // first clear those bits, then set them as required
        let new_reg = JoypadRegister::unpack(&[(curr & !mask) | masked]).expect("it's 8bits");
        self.register = new_reg;
    }
}

#[derive(Debug)]
struct SerialIo {
    serial_byte: u8,
    last_serial_byte_dumped: u8,
    buffer: String,
}

impl SerialIo {
    fn create() -> SerialIo {
        SerialIo {
            serial_byte: 0,
            last_serial_byte_dumped: 0,
            buffer: String::with_capacity(121),
        }
    }

    fn clock_set(&mut self, n: u8) {
        use web_utils::log;
        if n == 0x81 {
            if self.last_serial_byte_dumped != self.serial_byte {
                self.buffer.push(self.serial_byte as char);
                self.last_serial_byte_dumped = self.serial_byte;
                if self.serial_byte == ('\n' as u8) {
                    log(&format!("Serial: {:}", self.buffer));
                    self.buffer.clear();
                }
                if self.buffer.len() > 120 {
                    self.buffer.drain(0..1);
                }
            }
        }
    }
}

#[derive(PackedStruct, Debug, Clone, Copy)]
#[packed_struct(size_bytes = "1", bit_numbering = "lsb0")]
pub struct TimerControl {
    #[packed_field(bits = "0:1")]
    input_clock_select: Integer<u8, packed_bits::Bits2>,
    #[packed_field(bits = "2")]
    timer_enable: bool,
}
impl ReadViewU8 for TimerControl {
    fn read(&self) -> u8 {
        self.pack()[0]
    }
}
impl ViewU8 for TimerControl {
    fn set(&mut self, n: u8) {
        *self = TimerControl::unpack(&[n]).expect("it's 8bits")
    }
}

#[derive(Debug)]
pub struct Timer {
    div: u8,
    tima: u8,
    tma: u8,
    tac: TimerControl,
}
impl Timer {
    fn create() -> Timer {
        Timer {
            div: 0,
            tima: 0,
            tma: 0,
            tac: TimerControl::unpack(&[0]).unwrap(),
        }
    }
}
impl fmt::Display for Timer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "div:${:x} tima:${:x} tma:${:x} tac:${:x}",
            self.div,
            self.tima,
            self.tma,
            self.tac.read()
        )
    }
}

#[derive(Debug)]
pub struct Memory {
    pub booting: bool,
    zero: Vec<u8>,
    pub sprite_oam: Vec<OamEntry>,
    main: Vec<u8>,
    video: Vec<u8>,
    rom0: Vec<u8>,
    rom1: Vec<u8>,
    cartridge_ram: Vec<u8>,
    pub timer: Timer,
    serial: SerialIo,
    pub interrupt_enable: InterruptRegister,
    pub interrupt_flag: InterruptRegister,
    pub ppu: PpuRegisters,
    pub sound: sound::Registers,
    pub joypad: Joypad,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Addr(u16);
impl fmt::Display for Addr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "${:04x}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

    pub fn offset_signed(self, by: i8) -> Addr {
        if by < 0 {
            self.offset((-by) as u16, Direction::Neg)
        } else {
            self.offset(by as u16, Direction::Pos)
        }
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
            timer: Timer::create(),
            serial: SerialIo::create(),
            rom0,
            rom1,
            cartridge_ram: vec![0; 0x2000],
            interrupt_enable: InterruptRegister::create(),
            interrupt_flag: InterruptRegister::create(),
            ppu: PpuRegisters::create(),
            sound: sound::Registers::create(),
            joypad: Joypad::create(),
        }
    }

    pub fn ld_lots(&self, Addr(addr): Addr, length: u16) -> Vec<u8> {
        (0..length).map(|i| self.ld8(Addr(addr + i))).collect()
    }

    pub fn advance_timers(&mut self, cpu_ticks_elapsed: u32) -> TriggeredTimer {
        self.timer.div = (cpu_ticks_elapsed / 0xff) as u8;
        if !self.timer.tac.timer_enable {
            TriggeredTimer(false)
        } else {
            let old_tima = self.timer.tima;
            let divisor = match self.timer.tac.input_clock_select.into() {
                0b00 => 1024,
                0b01 => 16,
                0b10 => 64,
                0b11 => 256,
                x => panic!("Unexpected input clock select value {:}", x),
            };
            self.timer.tima = (cpu_ticks_elapsed / divisor) as u8;
            // if tima is smaller than old_tima, we overflowed
            if self.timer.tima < old_tima {
                self.timer.tima = self.timer.tma;
                TriggeredTimer(true)
            } else {
                TriggeredTimer(false)
            }
        }
    }

    #[allow(clippy::match_overlapping_arm)]
    #[allow(overlapping_patterns)]
    pub fn ld8(&self, Addr(addr): Addr) -> u8 {
        match addr {
            0xffff => self.interrupt_enable.read(),
            0xff80..=0xfffe => self.zero[(addr - 0xff80) as usize],
            0xff4c..=0xff7f => 0, // TODO: panic!("unusable memory"),
            0xff00 => self.joypad.poll_and_read(),
            0xff01 => self.serial.serial_byte,
            0xff0f => self.interrupt_flag.read(),
            0xff10 => self.sound.pulse_a.sweep.read(),
            0xff11 => self.sound.pulse_a.length.read(),
            0xff12 => self.sound.pulse_a.volume.read(),
            0xff13 => self.sound.pulse_a.frequency.read(),
            0xff14 => self.sound.pulse_a.control.read(),
            0xff40 => self.ppu.lcdc.read(),
            0xff41 => self.ppu.lcdc_stat_controller_mode(),
            0xff42 => self.ppu.scy.read(),
            0xff43 => self.ppu.scx.read(),
            0xff44 => self.ppu.ly(),
            0xff47 => self.ppu.bgp.read(),
            0xff48 => self.ppu.obp0.read(),
            0xff49 => self.ppu.obp1.read(),
            0xff00..=0xff4b => 0,
            // panic!("rest of I/O ports"),
            0xfea0..=0xfeff => 0xff, // unusable memory
            0xfe00..=0xfe9f => {
                let addr_ = addr - 0xfe00;
                self.sprite_oam[(addr_ / 4) as usize].pack()[(addr_ % 4) as usize]
            }
            0xe000..=0xfdff => {
                // echo ram
                self.main[(addr - 0xe000) as usize]
            }
            // 0xd000 ... 0xdfff => panic!("(cgb) ram banks 1-7"),
            // 0xc000 ... 0xcfff => panic!("ram bank 0"),
            0xc000..=0xdfff => self.main[(addr - 0xc000) as usize],
            0xa000..=0xbfff => self.cartridge_ram[(addr - 0xa000) as usize],
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

    pub fn ld16(&self, addr: Addr) -> u16 {
        let lo = self.ld8(addr);
        let hi = self.ld8(addr.offset(1, Direction::Pos));
        (u16::from(hi) << 8) | u16::from(lo)
    }

    fn start_dma(&mut self, n: u8) {
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
        use web_utils::log;
        match addr {
            0xffff => self.interrupt_enable.set(n),
            0xff80..=0xfffe => self.zero[(addr - 0xff80) as usize] = n,

            0xff00 => self.joypad.partial_set(n),
            0xff01 => self.serial.serial_byte = n,
            0xff02 => {
                // TODO: Implement serial clock properly
                self.serial.clock_set(n);
            }
            0xff04 => self.timer.div = 0,
            0xff06 => self.timer.tma = n,
            0xff07 => self.timer.tac.set(n),
            0xff0f => self.interrupt_flag.set(n),
            0xff10 => self.sound.pulse_a.sweep.set(n),
            0xff11 => self.sound.pulse_a.length.set(n),
            0xff12 => self.sound.pulse_a.volume.set(n),
            0xff13 => self.sound.pulse_a.frequency.set(n),
            0xff14 => self.sound.pulse_a.control.set(n),
            0xff40 => self.ppu.lcdc.set(n),
            0xff41 => {
                if n != 0 {
                    log(&format!(
                        "I assumed 0xff41 is always set to 0, but it's set to {:}",
                        n
                    ))
                }
            }
            0xff42 => self.ppu.scy.set(n),
            0xff43 => self.ppu.scx.set(n),
            0xff44 => log(&format!(
                "unusable addr ${:x} (LY) attempting to write ${:x}",
                addr, n
            )),
            0xff46 => self.start_dma(n),
            0xff47 => self.ppu.bgp.set(n),
            0xff48 => self.ppu.obp0.set(n),
            0xff49 => self.ppu.obp1.set(n),
            0xff50 => {
                if n == 0x01 {
                    self.booting = false
                } else if n == 0x00 {
                    // TODO: What does 0x00 to ff50 mean? Mooneye-gb tests do it
                    ()
                } else {
                    panic!("Unexpected value write to 0xff50 {:x}", n)
                }
            }
            0xff4c..=0xff7f => {
                // TODO: Why does tetris write to "unusable" memory?
                log(&format!(
                    "unusable addr ${:x} attempting to write ${:x}",
                    addr, n
                ))
            }
            0xff03..=0xff4b => (),
            // panic!("rest of I/O ports"),
            0xfea0..=0xfeff => {
                // TODO: Why does tetris write to this "unusable" memory
                log(&format!(
                    "unusable ${:x} attempting to write ${:x}",
                    addr, n
                ))
            }
            0xfe00..=0xfe9f => {
                let addr_ = addr - 0xfe00;
                self.sprite_oam[(addr_ / 4) as usize].pack()[(addr_ % 4) as usize] = n;
            }
            0xe000..=0xfdff => self.st8(Addr::directly(addr - (0xe000 - 0xc000)), n),
            // 0xd000 ... 0xdfff => panic!("(cgb) ram banks 1-7"),
            // 0xc000 ... 0xcfff => panic!("ram bank 0"),
            0xc000..=0xdfff => self.main[(addr - 0xc000) as usize] = n,
            0xa000..=0xbfff => self.cartridge_ram[(addr - 0xa000) as usize] = n,
            // end video ram
            // 0x9c00 ... 0x9fff => panic!("bg map data 2"),
            // 0x9800 ... 0x9bff => panic!("bg map data 1"),
            // 0x8000 ... 0x97ff => panic!("character ram"),
            0x8000..=0x9fff => self.video[(addr - 0x8000) as usize] = n,
            // begin video ram
            0x4000..=0x7fff => (),
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
