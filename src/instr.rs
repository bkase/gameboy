use register_kind::{RegisterKind8, RegisterKind16};
use std::error::Error;
use std::fmt;
use mem::{Memory, Addr, Direction};

/*   mneumonic       opcode clocks flag explanation
 *
     ld   r,r         xx         4 ---- r=r
     ld   r,n         xx nn      8 ---- r=n
     ld   r,(HL)      xx         8 ---- r=(HL)
     ld   (HL),r      7x         8 ---- (HL)=r
     ld   (HL),n      36 nn     12 ----
     ld   A,(BC)      0A         8 ----
     ld   A,(DE)      1A         8 ----
     ld   A,(nn)      FA        16 ----
     ld   (BC),A      02         8 ----
     ld   (DE),A      12         8 ----
     ld   (nn),A      EA        16 ----
     ld   A,(FF00+n)  F0 nn     12 ---- read from io-port n (memory FF00+n)
     ld   (FF00+n),A  E0 nn     12 ---- write to io-port n (memory FF00+n)
     ld   A,(FF00+C)  F2         8 ---- read from io-port C (memory FF00+C)
     ld   (FF00+C),A  E2         8 ---- write to io-port C (memory FF00+C)
     ldi  (HL),A      22         8 ---- (HL)=A, HL=HL+1
     ldi  A,(HL)      2A         8 ---- A=(HL), HL=HL+1
     ldd  (HL),A      32         8 ---- (HL)=A, HL=HL-1
     ldd  A,(HL)      3A         8 ---- A=(HL), HL=HL-1
*/
#[derive(Debug, Clone)]
pub struct BadOpcode;
impl fmt::Display for BadOpcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "bad opcode")
    }
}

impl Error for BadOpcode {
    fn description(&self) -> &str {
        "Bad opcode"
    }

    fn cause(&self) -> Option<&Error> {
        // Generic error, underlying cause isn't tracked.
        None
    }
}

pub trait HasDuration {
    // The left of the tuple is the duration if the conditional is taken
    // The right is if the conditional is not taken (or None if N/A)
    fn duration(&self) -> (u32, Option<u32>);
}

pub enum Ld {
    RGetsR(RegisterKind8, RegisterKind8),
    RGetsN(RegisterKind8, u8),
    RGetsHlInd(RegisterKind8),
    HlIndGetsR(RegisterKind8),
    HlIndGetsN(u8),
    AGetsBcInd,
    AGetsDeInd,
    AGetsNnInd(Addr),
    BcIndGetsA,
    DeIndGetsA,
    NnIndGetsA(Addr),
    AGetsIOOffset(u8),
    IOOffsetGetsA(u8),
    AGetsIOOffsetByC,
    IOOffsetByCGetsA,
    // special instructions
    HlIndGetsAInc,
    AGetsHlIndInc,
    HlIndGetsADec,
    AGetsHlIndDec,
}
use self::Ld::*;


impl HasDuration for Ld {
    fn duration(&self) -> (u32, Option<u32>) {
        match self {
            RGetsR(_, _) => (1, None),
            RGetsN(_, _) => (2, None),
            RGetsHlInd(_) => (2, None),
            HlIndGetsR(_) => (2, None),
            HlIndGetsN(_) => (3, None),
            AGetsBcInd => (2, None),
            AGetsDeInd => (2, None),
            AGetsNnInd(_) => (4, None),
            BcIndGetsA => (2, None),
            DeIndGetsA => (2, None),
            NnIndGetsA(_) => (4, None),
            AGetsIOOffset(_) => (3, None),
            IOOffsetGetsA(_) => (3, None),
            AGetsIOOffsetByC => (4, None),
            IOOffsetByCGetsA => (4, None),
            HlIndGetsAInc => (4, None),
            AGetsHlIndInc => (4, None),
            HlIndGetsADec => (4, None),
            AGetsHlIndDec => (4, None),
        }
    }
}

pub enum Arith {
    Xor(RegisterKind8), XorHlInd,
    Sub(RegisterKind8), SubHlInd,
    Inc8(RegisterKind8), Inc16(RegisterKind16), IncHlInd,
    Dec8(RegisterKind8), Dec16(RegisterKind16), DecHlInd,
    // Add / and / etc
}
use self::Arith::*;

impl HasDuration for Arith {
    fn duration(&self) -> (u32, Option<u32>) {
        match self {
            Xor(_) => (1, None),
            XorHlInd => (2, None),
            Sub(_) => (1, None),
            SubHlInd => (2, None),
            Inc8(_) => (1, None),
            Inc16(_) => (2, None),
            IncHlInd => (3, None),
            Dec8(_) => (1, None),
            Dec16(_) => (2, None),
            DecHlInd => (3, None),
        }
    }
}

pub enum Rotate {
    Rla,
    Rl(RegisterKind8)
}
use self::Rotate::*;

impl HasDuration for Rotate {
    fn duration(&self) -> (u32, Option<u32>) {
        match self {
            Rla => (1, None),
            Rl(_) => (2, None)
        }
    }
}

pub enum Jump {
    Jr(i8),
    JrNz(i8),
    JrZ(i8)
}
use self::Jump::*;

impl HasDuration for Jump {
    fn duration(&self) -> (u32, Option<u32>) {
        match self {
            Jr(_) => (3, None),
            JrNz(_) => (3, Some(2)),
            JrZ(_) => (3, Some(2)),
        }
    }
}

// Minimal instructions to execute the bootrom
pub enum Instr {
    Ld(Ld),
    Arith(Arith),
    Rotate(Rotate),
    Jump(Jump),
    Bit7h,
    CpHlInd, Cp(u8),
    PopBc,
    PushBc,
    Ret,
}
use self::Instr::*;

impl HasDuration for Instr {
    fn duration(&self) -> (u32, Option<u32>) {
        match self {
            Ld(ld) => ld.duration(),
            Arith(arith) => arith.duration(),
            Rotate(rotate) => rotate.duration(),
            Jump(jump) => jump.duration(),
            Bit7h => (2, None),
            CpHlInd => (2, None),
            Cp(_) => (2, None),
            PopBc => (3, None),
            PushBc => (4, None),
            Ret => (4, None),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct InstrPointer(Addr);
impl InstrPointer {
    pub fn create() -> InstrPointer {
        InstrPointer(Addr::directly(0x0100))
    }

    fn rewind(&mut self, n : u16) {
        let new_val = self.0.offset(n, Direction::Neg);
        (*self).0 = new_val
    }

    fn inc(&mut self) {
        let new_val = self.0.offset(1, Direction::Pos);
        (*self).0 = new_val
    }

    fn inc_by(&mut self, n : u16) {
        let new_val = self.0.offset(n, Direction::Pos);
        (*self).0 = new_val;
    }

    pub fn jump(&mut self, addr: Addr) {
        (*self).0 = addr
    }

    pub fn offset_by(&mut self, n : i8) {
        let (offset, direction) =
            if n > 0 { (n as u16, Direction::Pos) } else { ((n*(-1)) as u16, Direction::Neg) };

        let new_val = self.0.offset(offset, direction);
        (*self).0 = new_val;
    }
}

struct LiveInstrPointer<'a> {
    ptr : &'a mut InstrPointer,
    memory : &'a Memory
}
impl<'a> LiveInstrPointer<'a> {
    fn create<'b>(ptr : &'b mut InstrPointer, memory: &'b Memory) -> LiveInstrPointer<'b> {
        LiveInstrPointer {
            ptr,
            memory
        }
    }

    fn peek8(&self) -> u8 {
        self.memory.ld8(self.ptr.0)
    }

    fn read8(&mut self) -> u8 {
        let v = self.peek8();
        self.ptr.inc();
        v
    }

    fn peek16(&self) -> u16 {
        self.memory.ld16(self.ptr.0)
    }

    fn read16(&mut self) -> u16 {
        let v = self.peek16();
        self.ptr.inc_by(2);
        v
    }
}

impl<'a> LiveInstrPointer<'a> {
    // returns instruction and bytes read by the PC
    fn read_(&mut self) -> (Instr, u16) {
        let pos0 = self.read8();
        match pos0 {
            0x00 => panic!(format!("unimplemented instruction {}", pos0)),
            0x01 => panic!(format!("unimplemented instruction {}", pos0)),
            0x02 => (Ld(BcIndGetsA), 1),
            0x03 => (Arith(Inc16(RegisterKind16::Bc)), 1),
            0x04 => (Arith(Inc8(RegisterKind8::B)), 1),
            0x05 => (Arith(Dec8(RegisterKind8::B)), 1),
            0x06 =>
                (Ld(RGetsN(RegisterKind8::B, self.read8())), 2),
            0x07 => panic!(format!("unimplemented instruction {}", pos0)),
            0x08 => panic!(format!("unimplemented instruction {}", pos0)),
            0x09 => panic!(format!("unimplemented instruction {}", pos0)),
            0x0a => (Ld(AGetsBcInd), 1),
            0x0b => panic!(format!("unimplemented instruction {}", pos0)),
            0x0c => (Arith(Inc8(RegisterKind8::C)), 1),
            0x0d => (Arith(Dec8(RegisterKind8::C)), 1),
            0x0e =>
                (Ld(RGetsN(RegisterKind8::C, self.read8())), 2),
            0x0f => panic!(format!("unimplemented instruction {}", pos0)),
            0x10 => panic!(format!("unimplemented instruction {}", pos0)),
            0x11 => panic!(format!("unimplemented instruction {}", pos0)),
            0x12 => (Ld(DeIndGetsA), 1),
            0x13 => (Arith(Inc16(RegisterKind16::De)), 1),
            0x14 => (Arith(Inc8(RegisterKind8::D)), 1),
            0x15 => (Arith(Dec8(RegisterKind8::D)), 1),
            0x16 => (Ld(RGetsN(RegisterKind8::D, self.read8())), 2),
            0x17 => (Rotate(Rla), 1),
            0x18 => (Jump(Jr(self.read8() as i8)), 2),
            0x19 => panic!(format!("unimplemented instruction {}", pos0)),
            0x1a => (Ld(AGetsDeInd), 1),
            0x1b => panic!(format!("unimplemented instruction {}", pos0)),
            0x1c => (Arith(Inc8(RegisterKind8::E)), 1),
            0x1d => (Arith(Dec8(RegisterKind8::E)), 1),
            0x1e =>
                (Ld(RGetsN(RegisterKind8::E, self.read8())), 2),
            0x1f => panic!(format!("unimplemented instruction {}", pos0)),
            0x20 => (Jump(JrNz(self.read8() as i8)), 2),
            0x21 => panic!(format!("unimplemented instruction {}", pos0)),
            0x22 => (Ld(HlIndGetsAInc), 1),
            0x23 => (Arith(Inc16(RegisterKind16::Hl)), 1),
            0x24 => (Arith(Inc8(RegisterKind8::H)), 1),
            0x25 => (Arith(Dec8(RegisterKind8::H)), 1),
            0x26 => (Ld(RGetsN(RegisterKind8::H, self.read8())), 2),
            0x27 => panic!(format!("unimplemented instruction {}", pos0)),
            0x28 => (Jump(JrZ(self.read8() as i8)), 2),
            0x29 => panic!(format!("unimplemented instruction {}", pos0)),
            0x2a => (Ld(AGetsHlIndInc), 1),
            0x2b => panic!(format!("unimplemented instruction {}", pos0)),
            0x2c => (Arith(Inc8(RegisterKind8::L)), 1),
            0x2d => (Arith(Dec8(RegisterKind8::L)), 1),
            0x2e =>
                (Ld(RGetsN(RegisterKind8::L, self.read8())), 2),
            0x2f => panic!(format!("unimplemented instruction {}", pos0)),
            0x30 => panic!(format!("unimplemented instruction {}", pos0)),
            0x31 => panic!(format!("unimplemented instruction {}", pos0)),
            0x32 => (Ld(HlIndGetsADec), 1),
            0x33 => (Arith(Inc16(RegisterKind16::Sp)), 1),
            0x34 => (Arith(IncHlInd), 1),
            0x35 => (Arith(DecHlInd), 1),
            0x36 => (Ld(HlIndGetsN(self.read8())), 2),
            0x37 => panic!(format!("unimplemented instruction {}", pos0)),
            0x38 => panic!(format!("unimplemented instruction {}", pos0)),
            0x39 => panic!(format!("unimplemented instruction {}", pos0)),
            0x3a => (Ld(AGetsHlIndDec), 1),
            0x3b => panic!(format!("unimplemented instruction {}", pos0)),
            0x3c => (Arith(Inc8(RegisterKind8::A)), 1),
            0x3d => (Arith(Dec8(RegisterKind8::A)), 1),
            0x3e =>
                (Ld(RGetsN(RegisterKind8::A, self.read8())), 2),
            0x3f => panic!(format!("unimplemented instruction {}", pos0)),
            0x40 =>
                (Ld(RGetsR(RegisterKind8::B, RegisterKind8::B)), 1),
            0x41 => 
                (Ld(RGetsR(RegisterKind8::B, RegisterKind8::C)), 1),
            0x42 => 
                (Ld(RGetsR(RegisterKind8::B, RegisterKind8::D)), 1),
            0x43 => 
                (Ld(RGetsR(RegisterKind8::B, RegisterKind8::E)), 1),
            0x44 => 
                (Ld(RGetsR(RegisterKind8::B, RegisterKind8::H)), 1),
            0x45 => 
                (Ld(RGetsR(RegisterKind8::B, RegisterKind8::L)), 1),
            0x46 =>
                (Ld(RGetsHlInd(RegisterKind8::B)), 1),
            0x47 =>
                (Ld(RGetsR(RegisterKind8::B, RegisterKind8::A)), 1),
            0x48 =>
                (Ld(RGetsR(RegisterKind8::C, RegisterKind8::B)), 1),
            0x49 => 
                (Ld(RGetsR(RegisterKind8::C, RegisterKind8::C)), 1),
            0x4a => 
                (Ld(RGetsR(RegisterKind8::C, RegisterKind8::D)), 1),
            0x4b => 
                (Ld(RGetsR(RegisterKind8::C, RegisterKind8::E)), 1),
            0x4c => 
                (Ld(RGetsR(RegisterKind8::C, RegisterKind8::H)), 1),
            0x4d => 
                (Ld(RGetsR(RegisterKind8::C, RegisterKind8::L)), 1),
            0x4e =>
                (Ld(RGetsHlInd(RegisterKind8::C)), 1),
            0x4f =>
                (Ld(RGetsR(RegisterKind8::C, RegisterKind8::A)), 1),
            0x50 =>
                (Ld(RGetsR(RegisterKind8::D, RegisterKind8::B)), 1),
            0x51 => 
                (Ld(RGetsR(RegisterKind8::D, RegisterKind8::C)), 1),
            0x52 => 
                (Ld(RGetsR(RegisterKind8::D, RegisterKind8::D)), 1),
            0x53 => 
                (Ld(RGetsR(RegisterKind8::D, RegisterKind8::E)), 1),
            0x54 => 
                (Ld(RGetsR(RegisterKind8::D, RegisterKind8::H)), 1),
            0x55 => 
                (Ld(RGetsR(RegisterKind8::D, RegisterKind8::L)), 1),
            0x56 =>
                (Ld(RGetsHlInd(RegisterKind8::D)), 1),
            0x57 =>
                (Ld(RGetsR(RegisterKind8::D, RegisterKind8::A)), 1),
            0x58 =>
                (Ld(RGetsR(RegisterKind8::E, RegisterKind8::B)), 1),
            0x59 => 
                (Ld(RGetsR(RegisterKind8::E, RegisterKind8::C)), 1),
            0x5a => 
                (Ld(RGetsR(RegisterKind8::E, RegisterKind8::D)), 1),
            0x5b => 
                (Ld(RGetsR(RegisterKind8::E, RegisterKind8::E)), 1),
            0x5c => 
                (Ld(RGetsR(RegisterKind8::E, RegisterKind8::H)), 1),
            0x5d => 
                (Ld(RGetsR(RegisterKind8::E, RegisterKind8::L)), 1),
            0x5e =>
                (Ld(RGetsHlInd(RegisterKind8::E)), 1),
            0x5f =>
                (Ld(RGetsR(RegisterKind8::E, RegisterKind8::A)), 1),
            0x60 =>
                (Ld(RGetsR(RegisterKind8::H, RegisterKind8::B)), 1),
            0x61 => 
                (Ld(RGetsR(RegisterKind8::H, RegisterKind8::C)), 1),
            0x62 => 
                (Ld(RGetsR(RegisterKind8::H, RegisterKind8::D)), 1),
            0x63 => 
                (Ld(RGetsR(RegisterKind8::H, RegisterKind8::E)), 1),
            0x64 => 
                (Ld(RGetsR(RegisterKind8::H, RegisterKind8::H)), 1),
            0x65 => 
                (Ld(RGetsR(RegisterKind8::H, RegisterKind8::L)), 1),
            0x66 =>
                (Ld(RGetsHlInd(RegisterKind8::H)), 1),
            0x67 =>
                (Ld(RGetsR(RegisterKind8::H, RegisterKind8::A)), 1),
            0x68 =>
                (Ld(RGetsR(RegisterKind8::L, RegisterKind8::B)), 1),
            0x69 => 
                (Ld(RGetsR(RegisterKind8::L, RegisterKind8::C)), 1),
            0x6a => 
                (Ld(RGetsR(RegisterKind8::L, RegisterKind8::D)), 1),
            0x6b => 
                (Ld(RGetsR(RegisterKind8::L, RegisterKind8::E)), 1),
            0x6c => 
                (Ld(RGetsR(RegisterKind8::L, RegisterKind8::H)), 1),
            0x6d => 
                (Ld(RGetsR(RegisterKind8::L, RegisterKind8::L)), 1),
            0x6e =>
                (Ld(RGetsHlInd(RegisterKind8::L)), 1),
            0x6f =>
                (Ld(RGetsR(RegisterKind8::L, RegisterKind8::A)), 1),
            0x70 =>
                (Ld(HlIndGetsR(RegisterKind8::B)), 1),
            0x71 => 
                (Ld(HlIndGetsR(RegisterKind8::C)), 1),
            0x72 => 
                (Ld(HlIndGetsR(RegisterKind8::D)), 1),
            0x73 => 
                (Ld(HlIndGetsR(RegisterKind8::E)), 1),
            0x74 => 
                (Ld(HlIndGetsR(RegisterKind8::H)), 1),
            0x75 => 
                (Ld(HlIndGetsR(RegisterKind8::L)), 1),
            0x76 => panic!(format!("unimplemented instruction {}", pos0)),
            0x77 =>
                (Ld(HlIndGetsR(RegisterKind8::A)), 1),
            0x78 =>
                (Ld(RGetsR(RegisterKind8::A, RegisterKind8::B)), 1),
            0x79 => 
                (Ld(RGetsR(RegisterKind8::A, RegisterKind8::C)), 1),
            0x7a => 
                (Ld(RGetsR(RegisterKind8::A, RegisterKind8::D)), 1),
            0x7b => 
                (Ld(RGetsR(RegisterKind8::A, RegisterKind8::E)), 1),
            0x7c => 
                (Ld(RGetsR(RegisterKind8::A, RegisterKind8::H)), 1),
            0x7d => 
                (Ld(RGetsR(RegisterKind8::A, RegisterKind8::L)), 1),
            0x7e =>
                (Ld(RGetsHlInd(RegisterKind8::A)), 1),
            0x7f =>
                (Ld(RGetsR(RegisterKind8::A, RegisterKind8::A)), 1),
            0x80 => panic!(format!("unimplemented instruction {}", pos0)),
            0x81 => panic!(format!("unimplemented instruction {}", pos0)),
            0x82 => panic!(format!("unimplemented instruction {}", pos0)),
            0x83 => panic!(format!("unimplemented instruction {}", pos0)),
            0x84 => panic!(format!("unimplemented instruction {}", pos0)),
            0x85 => panic!(format!("unimplemented instruction {}", pos0)),
            0x86 => panic!(format!("unimplemented instruction {}", pos0)),
            0x87 => panic!(format!("unimplemented instruction {}", pos0)),
            0x88 => panic!(format!("unimplemented instruction {}", pos0)),
            0x89 => panic!(format!("unimplemented instruction {}", pos0)),
            0x8a => panic!(format!("unimplemented instruction {}", pos0)),
            0x8b => panic!(format!("unimplemented instruction {}", pos0)),
            0x8c => panic!(format!("unimplemented instruction {}", pos0)),
            0x8d => panic!(format!("unimplemented instruction {}", pos0)),
            0x8e => panic!(format!("unimplemented instruction {}", pos0)),
            0x8f => panic!(format!("unimplemented instruction {}", pos0)),
            0x90 => (Arith(Sub(RegisterKind8::B)), 1),
            0x91 => (Arith(Sub(RegisterKind8::C)), 1),
            0x92 => (Arith(Sub(RegisterKind8::D)), 1),
            0x93 => (Arith(Sub(RegisterKind8::E)), 1),
            0x94 => (Arith(Sub(RegisterKind8::H)), 1),
            0x95 => (Arith(Sub(RegisterKind8::L)), 1),
            0x96 => (Arith(SubHlInd), 1),
            0x97 => (Arith(Sub(RegisterKind8::A)), 1),
            0x98 => panic!(format!("unimplemented instruction {}", pos0)),
            0x99 => panic!(format!("unimplemented instruction {}", pos0)),
            0x9a => panic!(format!("unimplemented instruction {}", pos0)),
            0x9b => panic!(format!("unimplemented instruction {}", pos0)),
            0x9c => panic!(format!("unimplemented instruction {}", pos0)),
            0x9d => panic!(format!("unimplemented instruction {}", pos0)),
            0x9e => panic!(format!("unimplemented instruction {}", pos0)),
            0x9f => panic!(format!("unimplemented instruction {}", pos0)),
            0xa0 => panic!(format!("unimplemented instruction {}", pos0)),
            0xa1 => panic!(format!("unimplemented instruction {}", pos0)),
            0xa2 => panic!(format!("unimplemented instruction {}", pos0)),
            0xa3 => panic!(format!("unimplemented instruction {}", pos0)),
            0xa4 => panic!(format!("unimplemented instruction {}", pos0)),
            0xa5 => panic!(format!("unimplemented instruction {}", pos0)),
            0xa6 => panic!(format!("unimplemented instruction {}", pos0)),
            0xa7 => panic!(format!("unimplemented instruction {}", pos0)),
            0xa8 => (Arith(Xor(RegisterKind8::B)), 1),
            0xa9 =>
                (Arith(Xor(RegisterKind8::C)), 1),
            0xaa =>
                (Arith(Xor(RegisterKind8::D)), 1),
            0xab =>
                (Arith(Xor(RegisterKind8::E)), 1),
            0xac =>
                (Arith(Xor(RegisterKind8::H)), 1),
            0xad =>
                (Arith(Xor(RegisterKind8::L)), 1),
            0xae =>
                (Arith(XorHlInd), 1),
            0xaf =>
                (Arith(Xor(RegisterKind8::A)), 1),
            0xb0 => panic!(format!("unimplemented instruction {}", pos0)),
            0xb1 => panic!(format!("unimplemented instruction {}", pos0)),
            0xb2 => panic!(format!("unimplemented instruction {}", pos0)),
            0xb3 => panic!(format!("unimplemented instruction {}", pos0)),
            0xb4 => panic!(format!("unimplemented instruction {}", pos0)),
            0xb5 => panic!(format!("unimplemented instruction {}", pos0)),
            0xb6 => panic!(format!("unimplemented instruction {}", pos0)),
            0xb7 => panic!(format!("unimplemented instruction {}", pos0)),
            0xb8 => panic!(format!("unimplemented instruction {}", pos0)),
            0xb9 => panic!(format!("unimplemented instruction {}", pos0)),
            0xba => panic!(format!("unimplemented instruction {}", pos0)),
            0xbb => panic!(format!("unimplemented instruction {}", pos0)),
            0xbc => panic!(format!("unimplemented instruction {}", pos0)),
            0xbd => panic!(format!("unimplemented instruction {}", pos0)),
            0xbe => (Instr::CpHlInd, 1),
            0xbf => panic!(format!("unimplemented instruction {}", pos0)),
            0xc0 => panic!(format!("unimplemented instruction {}", pos0)),
            0xc1 => (Instr::PopBc, 1),
            0xc2 => panic!(format!("unimplemented instruction {}", pos0)),
            0xc3 => panic!(format!("unimplemented instruction {}", pos0)),
            0xc4 => panic!(format!("unimplemented instruction {}", pos0)),
            0xc5 => (Instr::PushBc, 1),
            0xc6 => panic!(format!("unimplemented instruction {}", pos0)),
            0xc7 => panic!(format!("unimplemented instruction {}", pos0)),
            0xc8 => panic!(format!("unimplemented instruction {}", pos0)),
            0xc9 => (Instr::Ret, 1),
            0xca => panic!(format!("unimplemented instruction {}", pos0)),
            0xcb => {
                let pos1 = self.read8();
                match pos1 {
                    0x11 => (Rotate(Rl(RegisterKind8::C)), 2),
                    0x7c => (Bit7h, 2),
                    _ => panic!(format!("unimplemented instruction {}", pos1)),
                }
            }
            0xcc => panic!(format!("unimplemented instruction {}", pos0)),
            0xcd => panic!(format!("unimplemented instruction {}", pos0)),
            0xce => panic!(format!("unimplemented instruction {}", pos0)),
            0xcf => panic!(format!("unimplemented instruction {}", pos0)),
            0xd0 => panic!(format!("unimplemented instruction {}", pos0)),
            0xd1 => panic!(format!("unimplemented instruction {}", pos0)),
            0xd2 => panic!(format!("unimplemented instruction {}", pos0)),
            0xd3 => panic!(format!("unimplemented instruction {}", pos0)),
            0xd4 => panic!(format!("unimplemented instruction {}", pos0)),
            0xd5 => panic!(format!("unimplemented instruction {}", pos0)),
            0xd6 => panic!(format!("unimplemented instruction {}", pos0)),
            0xd7 => panic!(format!("unimplemented instruction {}", pos0)),
            0xd8 => panic!(format!("unimplemented instruction {}", pos0)),
            0xd9 => panic!(format!("unimplemented instruction {}", pos0)),
            0xda => panic!(format!("unimplemented instruction {}", pos0)),
            0xdb => panic!(format!("unimplemented instruction {}", pos0)),
            0xdc => panic!(format!("unimplemented instruction {}", pos0)),
            0xdd => panic!(format!("unimplemented instruction {}", pos0)),
            0xde => panic!(format!("unimplemented instruction {}", pos0)),
            0xdf => panic!(format!("unimplemented instruction {}", pos0)),
            0xe0 =>
                (Ld(IOOffsetGetsA(self.read8())), 2),
            0xe1 => panic!(format!("unimplemented instruction {}", pos0)),
            0xe2 => (Ld(IOOffsetByCGetsA), 1),
            0xe3 => panic!(format!("unimplemented instruction {}", pos0)),
            0xe4 => panic!(format!("unimplemented instruction {}", pos0)),
            0xe5 => panic!(format!("unimplemented instruction {}", pos0)),
            0xe6 => panic!(format!("unimplemented instruction {}", pos0)),
            0xe7 => panic!(format!("unimplemented instruction {}", pos0)),
            0xe8 => panic!(format!("unimplemented instruction {}", pos0)),
            0xe9 => panic!(format!("unimplemented instruction {}", pos0)),
            0xea =>
                (Ld(NnIndGetsA(Addr::directly(self.read16()))), 3),
            0xeb => panic!(format!("unimplemented instruction {}", pos0)),
            0xec => panic!(format!("unimplemented instruction {}", pos0)),
            0xed => panic!(format!("unimplemented instruction {}", pos0)),
            0xee => panic!(format!("unimplemented instruction {}", pos0)),
            0xef => panic!(format!("unimplemented instruction {}", pos0)),
            0xf0 =>
                (Ld(AGetsIOOffset(self.read8())), 2),
            0xf1 => panic!(format!("unimplemented instruction {}", pos0)),
            0xf2 =>
                (Ld(AGetsIOOffsetByC), 1),
            0xf3 => panic!(format!("unimplemented instruction {}", pos0)),
            0xf4 => panic!(format!("unimplemented instruction {}", pos0)),
            0xf5 => panic!(format!("unimplemented instruction {}", pos0)),
            0xf6 => panic!(format!("unimplemented instruction {}", pos0)),
            0xf7 => panic!(format!("unimplemented instruction {}", pos0)),
            0xf8 => panic!(format!("unimplemented instruction {}", pos0)),
            0xf9 => panic!(format!("unimplemented instruction {}", pos0)),
            0xfa =>
                (Ld(AGetsNnInd(Addr::directly(self.read16()))), 3),
            0xfb => panic!(format!("unimplemented instruction {}", pos0)),
            0xfc => panic!(format!("unimplemented instruction {}", pos0)),
            0xfd => panic!(format!("unimplemented instruction {}", pos0)),
            0xfe => (Instr::Cp(self.read8()), 2),
            0xff => panic!(format!("unimplemented instruction {}", pos0))
        }
    }

    fn read(&'a mut self) -> Instr {
        let (i, _) = self.read_();
        i
    }

    fn peek(&'a mut self) -> Instr {
        let (i, n) = {
            self.read_()
        };
        self.ptr.rewind(n);
        i
    }
}

impl InstrPointer {
    pub fn read(&mut self, memory : &Memory) -> Instr {
        LiveInstrPointer::create(self, memory).read()
    }
    pub fn peek(&mut self, memory : &Memory) -> Instr {
        LiveInstrPointer::create(self, memory).peek()
    }
}


