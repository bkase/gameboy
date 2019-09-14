#![allow(dead_code)]

use mem::{Addr, Direction, Memory};
use register::R8;
use register_kind::{RegisterKind16, RegisterKind8};
use std::error::Error;
use std::fmt;

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

#[derive(Debug, Clone)]
pub enum Ld {
    RGetsN(RegisterKind8, u8),
    RGetsR(RegisterKind8, RegisterKind8),
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

    SpGetsAddr(Addr),
    HlGetsAddr(Addr),
    DeGetsAddr(Addr),
}
use self::Ld::*;

impl fmt::Display for Ld {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RGetsN(rk, v) => write!(f, "(LD) {:} <- {:}", rk, v),
            RGetsR(rk1, rk2) => write!(f, "(LD) {:} <- {:}", rk1, rk2),
            RGetsHlInd(rk) => write!(f, "(LD) {:} <- [HL]", rk),
            HlIndGetsR(rk) => write!(f, "(LD) [HL] <- {:}", rk),
            HlIndGetsN(n) => write!(f, "(LD) [HL] <- {:}", R8(*n)),
            AGetsBcInd => write!(f, "(LD) A <- [BC]"),
            AGetsDeInd => write!(f, "(LD) A <- [DE]"),
            AGetsNnInd(nn) => write!(f, "(LD) A <- [{:}]", nn),
            BcIndGetsA => write!(f, "(LD) [BC] <- A"),
            DeIndGetsA => write!(f, "(LD) [DE] <- A"),
            NnIndGetsA(addr) => write!(f, "(LD) [{:}] <- A", addr),
            AGetsIOOffset(n) => write!(f, "(LD) A <- [$ff00+{:}]", R8(*n)),
            IOOffsetGetsA(n) => write!(f, "(LD) [$ff00+{:}] <- A", R8(*n)),
            AGetsIOOffsetByC => write!(f, "(LD) A <- [$ff00+C]"),
            IOOffsetByCGetsA => write!(f, "(LD) [$ff00+C] <- A"),
            // special instructions
            HlIndGetsAInc => write!(f, "(LD) [HL]++ <- A"),
            AGetsHlIndInc => write!(f, "(LD) A <- [HL]++"),
            HlIndGetsADec => write!(f, "(LD) [HL]-- <- A"),
            AGetsHlIndDec => write!(f, "(LD) A <- [HL]++"),

            SpGetsAddr(addr) => write!(f, "(LD) SP <- {:}", addr),
            HlGetsAddr(addr) => write!(f, "(LD) HL <- {:}", addr),
            DeGetsAddr(addr) => write!(f, "(LD) DE <- {:}", addr),
        }
    }
}

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
            SpGetsAddr(_) => (3, None),
            HlGetsAddr(_) => (3, None),
            DeGetsAddr(_) => (3, None),
        }
    }
}

// TODO: Hlist for the variants so we can reuse these better across all instrs
#[derive(Debug, Clone)]
pub enum RegsHlN {
    HlInd,
    N(u8),
    Reg(RegisterKind8),
}
impl fmt::Display for RegsHlN {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RegsHlN::HlInd => write!(f, "[HL]"),
            RegsHlN::N(n) => write!(f, "{:}", R8(*n)),
            RegsHlN::Reg(rk) => write!(f, "{:}", rk),
        }
    }
}
#[derive(Debug, Clone)]
pub enum RegsHl {
    HlInd,
    Reg(RegisterKind8),
}
impl fmt::Display for RegsHl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RegsHl::HlInd => write!(f, "[HL]"),
            RegsHl::Reg(rk) => write!(f, "{:}", rk),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Arith {
    Add(RegsHlN),
    Adc(RegsHlN),
    Sub(RegsHlN),
    Sbc(RegsHl),
    And(RegsHlN),
    Or(RegsHlN),
    Xor(RegsHlN),
    Cp(RegsHlN),
    Inc(RegsHl),
    Dec(RegsHl),

    AddHl(RegisterKind16),
    AddSp(i8),
    Inc16(RegisterKind16),
    Dec16(RegisterKind16),
}
use self::Arith::*;

impl fmt::Display for Arith {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            // 8 bit
            Add(x) => write!(f, "(ADD) A <- A + {:}", x),
            Adc(x) => write!(f, "(ADC) A <- A + {:} + c", x),
            Sub(x) => write!(f, "(SUB) A <- A - {:}", x),
            Sbc(x) => write!(f, "(SBC) A <- A - {:} + c", x),
            And(x) => write!(f, "(AND) A <- A & {:}", x),
            Or(x) => write!(f, "(OR) A <- A | {:}", x),
            Xor(x) => write!(f, "(OR) A <- A ^ {:}", x),
            Cp(x) => write!(f, "(CP) A ==? {:}", x),
            Inc(x) => write!(f, "(INC) {:}++", x),
            Dec(x) => write!(f, "(DEC) {:}--", x),

            // 16 bit
            AddHl(rk) => write!(f, "(ADD) HL <- HL + {:}", rk),
            AddSp(x) => write!(f, "(ADD) SP <- SP + {:}", x),
            Inc16(rk) => write!(f, "(INC) {:}++", rk),
            Dec16(rk) => write!(f, "(DEC) {:}--", rk),
        }
    }
}

impl HasDuration for Arith {
    fn duration(&self) -> (u32, Option<u32>) {
        match self {
            Add(x) | Adc(x) | Sub(x) | And(x) | Or(x) | Xor(x) | Cp(x) => match x {
                RegsHlN::HlInd => (2, None),
                RegsHlN::N(_) => (2, None),
                RegsHlN::Reg(_) => (1, None),
            },
            Sbc(RegsHl::HlInd) => (2, None),
            Sbc(RegsHl::Reg(_)) => (1, None),
            Inc(x) | Dec(x) => match x {
                RegsHl::HlInd => (3, None),
                RegsHl::Reg(_) => (1, None),
            },
            AddHl(_) => (2, None),
            AddSp(_) => (4, None),
            Inc16(_) | Dec16(_) => (2, None),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Rotate {
    Rla,
    Rl(RegisterKind8),
}
use self::Rotate::*;

impl fmt::Display for Rotate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Rla => write!(f, "(RLA) A <- A << 1"),
            Rl(rk) => write!(f, "(RL n) {:} <- {:} << 1", rk, rk),
        }
    }
}

impl HasDuration for Rotate {
    fn duration(&self) -> (u32, Option<u32>) {
        match self {
            Rla => (1, None),
            Rl(_) => (2, None),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Jump {
    Jp(Addr),
    Jr(i8),
    JrNz(i8),
    JrZ(i8),
    Call(Addr),
    CallZ(Addr),
}
use self::Jump::*;

impl fmt::Display for Jump {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Jp(addr) => write!(f, "JP {:}", addr),
            Jr(n) => write!(f, "JR {:}", n),
            JrNz(n) => write!(f, "JRNZ {:} (if != 0)", n),
            JrZ(n) => write!(f, "JRZ {:} (if == 0)", n),
            Call(addr) => write!(f, "CALL {:}", addr),
            CallZ(addr) => write!(f, "CALLZ {:}", addr),
        }
    }
}

impl HasDuration for Jump {
    fn duration(&self) -> (u32, Option<u32>) {
        match self {
            Jp(_) => (16, None),
            Jr(_) => (3, None),
            JrNz(_) => (3, Some(2)),
            JrZ(_) => (3, Some(2)),
            Call(_) => (6, None),
            CallZ(_) => (6, Some(3)),
        }
    }
}

// Minimal instructions to execute the bootrom
#[derive(Debug, Clone)]
pub enum Instr {
    Ld(Ld),
    Arith(Arith),
    Rotate(Rotate),
    Jump(Jump),
    Bit7h,
    CpHlInd,
    PopBc,
    PushBc,
    Ret,
    Nop,
}
use self::Instr::*;

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ld(ld) => write!(f, "{:}", ld),
            Arith(a) => write!(f, "{:}", a),
            Rotate(r) => write!(f, "{:}", r),
            Jump(j) => write!(f, "{:}", j),
            Bit7h => write!(f, "BIT 7, H"),
            CpHlInd => write!(f, "(CP) A ==? [HL]"),
            PopBc => write!(f, "POP BC"),
            PushBc => write!(f, "PUSH BC"),
            Ret => write!(f, "RET"),
            Nop => write!(f, "NOP"),
        }
    }
}

impl HasDuration for Instr {
    fn duration(&self) -> (u32, Option<u32>) {
        match self {
            Ld(ld) => ld.duration(),
            Arith(arith) => arith.duration(),
            Rotate(rotate) => rotate.duration(),
            Jump(jump) => jump.duration(),
            Bit7h => (2, None),
            CpHlInd => (2, None),
            PopBc => (3, None),
            PushBc => (4, None),
            Ret => (4, None),
            Nop => (1, None),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct InstrPointer(pub Addr);
impl InstrPointer {
    pub fn create() -> InstrPointer {
        InstrPointer(Addr::directly(0x00))
    }

    fn rewind(&mut self, n: u16) {
        let new_val = self.0.offset(n, Direction::Neg);
        (*self).0 = new_val
    }

    fn inc(&mut self) {
        let new_val = self.0.offset(1, Direction::Pos);
        (*self).0 = new_val
    }

    fn inc_by(&mut self, n: u16) {
        let new_val = self.0.offset(n, Direction::Pos);
        (*self).0 = new_val;
    }

    pub fn jump(&mut self, addr: Addr) {
        (*self).0 = addr
    }

    pub fn offset_by(&mut self, n: i8) {
        let (offset, direction) = if n > 0 {
            (n as u16, Direction::Pos)
        } else {
            ((-n) as u16, Direction::Neg)
        };

        let new_val = self.0.offset(offset, direction);
        (*self).0 = new_val;
    }
}

struct LiveInstrPointer<'a> {
    ptr: &'a mut InstrPointer,
    memory: &'a Memory,
}
impl<'a> LiveInstrPointer<'a> {
    fn create<'b>(ptr: &'b mut InstrPointer, memory: &'b Memory) -> LiveInstrPointer<'b> {
        LiveInstrPointer { ptr, memory }
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

use register_kind::RegisterKind16::*;
use register_kind::RegisterKind8::*;
impl<'a> LiveInstrPointer<'a> {
    // returns instruction and bytes read by the PC
    fn read_(&mut self) -> (Instr, Vec<u8>) {
        fn hi_lo_decompose(x: u16) -> (u8, u8) {
            (((x & 0xff00) >> 8) as u8, (x & 0xff) as u8)
        }

        let pos0 = self.read8();
        match pos0 {
            0x00 => (Nop, vec![pos0]),
            0x01 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0x02 => (Ld(BcIndGetsA), vec![pos0]),
            0x03 => (Arith(Inc16(Bc)), vec![pos0]),
            0x04 => (Arith(Inc(RegsHl::Reg(B))), vec![pos0]),
            0x05 => (Arith(Dec(RegsHl::Reg(B))), vec![pos0]),
            0x06 => {
                let pos1 = self.read8();
                (Ld(RGetsN(B, pos1)), vec![pos0, pos1])
            }
            0x07 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0x08 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0x09 => (Arith(AddHl(Bc)), vec![pos0]),
            0x0a => (Ld(AGetsBcInd), vec![pos0]),
            0x0b => (Arith(Dec16(Bc)), vec![pos0]),
            0x0c => (Arith(Inc(RegsHl::Reg(C))), vec![pos0]),
            0x0d => (Arith(Dec(RegsHl::Reg(C))), vec![pos0]),
            0x0e => {
                let pos1 = self.read8();
                (Ld(RGetsN(C, pos1)), vec![pos0, pos1])
            }
            0x0f => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0x10 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0x11 => {
                let addr = self.read16();
                let (hi, lo) = hi_lo_decompose(addr);
                (Ld(DeGetsAddr(Addr::directly(addr))), vec![pos0, lo, hi])
            }
            0x12 => (Ld(DeIndGetsA), vec![pos0]),
            0x13 => (Arith(Inc16(De)), vec![pos0]),
            0x14 => (Arith(Inc(RegsHl::Reg(D))), vec![pos0]),
            0x15 => (Arith(Dec(RegsHl::Reg(D))), vec![pos0]),
            0x16 => {
                let pos1 = self.read8();
                (Ld(RGetsN(RegisterKind8::D, pos1)), vec![pos0, pos1])
            }
            0x17 => (Rotate(Rla), vec![pos0]),
            0x18 => {
                let pos1 = self.read8();
                (Jump(Jr(pos1 as i8)), vec![pos0, pos1])
            }
            0x19 => (Arith(AddHl(De)), vec![pos0]),
            0x1a => (Ld(AGetsDeInd), vec![pos0]),
            0x1b => (Arith(Dec16(De)), vec![pos0]),
            0x1c => (Arith(Inc(RegsHl::Reg(E))), vec![pos0]),
            0x1d => (Arith(Dec(RegsHl::Reg(E))), vec![pos0]),
            0x1e => {
                let pos1 = self.read8();
                (Ld(RGetsN(E, pos1)), vec![pos0, pos1])
            }
            0x1f => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0x20 => {
                let pos1 = self.read8();
                (Jump(JrNz(pos1 as i8)), vec![pos0, pos1])
            }
            0x21 => {
                let addr = self.read16();
                let (hi, lo) = hi_lo_decompose(addr);
                (Ld(HlGetsAddr(Addr::directly(addr))), vec![pos0, lo, hi])
            }
            0x22 => (Ld(HlIndGetsAInc), vec![pos0]),
            0x23 => (Arith(Inc16(Hl)), vec![pos0]),
            0x24 => (Arith(Inc(RegsHl::Reg(H))), vec![pos0]),
            0x25 => (Arith(Dec(RegsHl::Reg(H))), vec![pos0]),
            0x26 => {
                let pos1 = self.read8();
                (Ld(RGetsN(H, pos1)), vec![pos0, pos1])
            }
            0x27 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0x28 => {
                let pos1 = self.read8();
                (Jump(JrZ(pos1 as i8)), vec![pos0, pos1])
            }
            0x29 => (Arith(AddHl(Hl)), vec![pos0]),
            0x2a => (Ld(AGetsHlIndInc), vec![pos0]),
            0x2b => (Arith(Dec16(Hl)), vec![pos0]),
            0x2c => (Arith(Inc(RegsHl::Reg(L))), vec![pos0]),
            0x2d => (Arith(Dec(RegsHl::Reg(L))), vec![pos0]),
            0x2e => {
                let pos1 = self.read8();
                (Ld(RGetsN(L, pos1)), vec![pos0, pos1])
            }
            0x2f => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0x30 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0x31 => {
                let addr = self.read16();
                let (hi, lo) = hi_lo_decompose(addr);
                (Ld(SpGetsAddr(Addr::directly(addr))), vec![pos0, lo, hi])
            }
            0x32 => (Ld(HlIndGetsADec), vec![pos0]),
            0x33 => (Arith(Inc16(Sp)), vec![pos0]),
            0x34 => (Arith(Inc(RegsHl::HlInd)), vec![pos0]),
            0x35 => (Arith(Dec(RegsHl::HlInd)), vec![pos0]),
            0x36 => {
                let pos1 = self.read8();
                (Ld(HlIndGetsN(pos1)), vec![pos0, pos1])
            }
            0x37 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0x38 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0x39 => (Arith(AddHl(Sp)), vec![pos0]),
            0x3a => (Ld(AGetsHlIndDec), vec![pos0]),
            0x3b => (Arith(Dec16(Sp)), vec![pos0]),
            0x3c => (Arith(Inc(RegsHl::Reg(A))), vec![pos0]),
            0x3d => (Arith(Dec(RegsHl::Reg(A))), vec![pos0]),
            0x3e => {
                let pos1 = self.read8();
                (Ld(RGetsN(RegisterKind8::A, pos1)), vec![pos0, pos1])
            }
            0x3f => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0x40 => (Ld(RGetsR(B, B)), vec![pos0]),
            0x41 => (Ld(RGetsR(B, C)), vec![pos0]),
            0x42 => (Ld(RGetsR(B, D)), vec![pos0]),
            0x43 => (Ld(RGetsR(B, E)), vec![pos0]),
            0x44 => (Ld(RGetsR(B, H)), vec![pos0]),
            0x45 => (Ld(RGetsR(B, L)), vec![pos0]),
            0x46 => (Ld(RGetsHlInd(B)), vec![pos0]),
            0x47 => (Ld(RGetsR(B, A)), vec![pos0]),
            0x48 => (Ld(RGetsR(C, B)), vec![pos0]),
            0x49 => (Ld(RGetsR(C, C)), vec![pos0]),
            0x4a => (Ld(RGetsR(C, D)), vec![pos0]),
            0x4b => (Ld(RGetsR(C, E)), vec![pos0]),
            0x4c => (Ld(RGetsR(C, H)), vec![pos0]),
            0x4d => (Ld(RGetsR(C, L)), vec![pos0]),
            0x4e => (Ld(RGetsHlInd(C)), vec![pos0]),
            0x4f => (Ld(RGetsR(C, A)), vec![pos0]),
            0x50 => (Ld(RGetsR(D, B)), vec![pos0]),
            0x51 => (Ld(RGetsR(D, C)), vec![pos0]),
            0x52 => (Ld(RGetsR(D, D)), vec![pos0]),
            0x53 => (Ld(RGetsR(D, E)), vec![pos0]),
            0x54 => (Ld(RGetsR(D, H)), vec![pos0]),
            0x55 => (Ld(RGetsR(D, L)), vec![pos0]),
            0x56 => (Ld(RGetsHlInd(D)), vec![pos0]),
            0x57 => (Ld(RGetsR(D, A)), vec![pos0]),
            0x58 => (Ld(RGetsR(E, B)), vec![pos0]),
            0x59 => (Ld(RGetsR(E, C)), vec![pos0]),
            0x5a => (Ld(RGetsR(E, D)), vec![pos0]),
            0x5b => (Ld(RGetsR(E, E)), vec![pos0]),
            0x5c => (Ld(RGetsR(E, H)), vec![pos0]),
            0x5d => (Ld(RGetsR(E, L)), vec![pos0]),
            0x5e => (Ld(RGetsHlInd(E)), vec![pos0]),
            0x5f => (Ld(RGetsR(E, A)), vec![pos0]),
            0x60 => (Ld(RGetsR(H, B)), vec![pos0]),
            0x61 => (Ld(RGetsR(H, C)), vec![pos0]),
            0x62 => (Ld(RGetsR(H, D)), vec![pos0]),
            0x63 => (Ld(RGetsR(H, E)), vec![pos0]),
            0x64 => (Ld(RGetsR(H, H)), vec![pos0]),
            0x65 => (Ld(RGetsR(H, L)), vec![pos0]),
            0x66 => (Ld(RGetsHlInd(H)), vec![pos0]),
            0x67 => (Ld(RGetsR(H, A)), vec![pos0]),
            0x68 => (Ld(RGetsR(L, B)), vec![pos0]),
            0x69 => (Ld(RGetsR(L, C)), vec![pos0]),
            0x6a => (Ld(RGetsR(L, D)), vec![pos0]),
            0x6b => (Ld(RGetsR(L, E)), vec![pos0]),
            0x6c => (Ld(RGetsR(L, H)), vec![pos0]),
            0x6d => (Ld(RGetsR(L, L)), vec![pos0]),
            0x6e => (Ld(RGetsHlInd(L)), vec![pos0]),
            0x6f => (Ld(RGetsR(L, A)), vec![pos0]),
            0x70 => (Ld(HlIndGetsR(B)), vec![pos0]),
            0x71 => (Ld(HlIndGetsR(C)), vec![pos0]),
            0x72 => (Ld(HlIndGetsR(D)), vec![pos0]),
            0x73 => (Ld(HlIndGetsR(E)), vec![pos0]),
            0x74 => (Ld(HlIndGetsR(H)), vec![pos0]),
            0x75 => (Ld(HlIndGetsR(L)), vec![pos0]),
            0x76 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0x77 => (Ld(HlIndGetsR(A)), vec![pos0]),
            0x78 => (Ld(RGetsR(A, B)), vec![pos0]),
            0x79 => (Ld(RGetsR(A, C)), vec![pos0]),
            0x7a => (Ld(RGetsR(A, D)), vec![pos0]),
            0x7b => (Ld(RGetsR(A, E)), vec![pos0]),
            0x7c => (Ld(RGetsR(A, H)), vec![pos0]),
            0x7d => (Ld(RGetsR(A, L)), vec![pos0]),
            0x7e => (Ld(RGetsHlInd(A)), vec![pos0]),
            0x7f => (Ld(RGetsR(A, A)), vec![pos0]),
            0x80 => (Arith(Add(RegsHlN::Reg(B))), vec![pos0]),
            0x81 => (Arith(Add(RegsHlN::Reg(C))), vec![pos0]),
            0x82 => (Arith(Add(RegsHlN::Reg(D))), vec![pos0]),
            0x83 => (Arith(Add(RegsHlN::Reg(E))), vec![pos0]),
            0x84 => (Arith(Add(RegsHlN::Reg(H))), vec![pos0]),
            0x85 => (Arith(Add(RegsHlN::Reg(L))), vec![pos0]),
            0x86 => (Arith(Add(RegsHlN::HlInd)), vec![pos0]),
            0x87 => (Arith(Add(RegsHlN::Reg(A))), vec![pos0]),
            0x88 => (Arith(Adc(RegsHlN::Reg(B))), vec![pos0]),
            0x89 => (Arith(Adc(RegsHlN::Reg(C))), vec![pos0]),
            0x8a => (Arith(Adc(RegsHlN::Reg(D))), vec![pos0]),
            0x8b => (Arith(Adc(RegsHlN::Reg(E))), vec![pos0]),
            0x8c => (Arith(Adc(RegsHlN::Reg(H))), vec![pos0]),
            0x8d => (Arith(Adc(RegsHlN::Reg(L))), vec![pos0]),
            0x8e => (Arith(Adc(RegsHlN::HlInd)), vec![pos0]),
            0x8f => {
                let pos1 = self.read8();
                (Arith(Adc(RegsHlN::N(pos1))), vec![pos0, pos1])
            }
            0x90 => (Arith(Sub(RegsHlN::Reg(B))), vec![pos0]),
            0x91 => (Arith(Sub(RegsHlN::Reg(C))), vec![pos0]),
            0x92 => (Arith(Sub(RegsHlN::Reg(D))), vec![pos0]),
            0x93 => (Arith(Sub(RegsHlN::Reg(E))), vec![pos0]),
            0x94 => (Arith(Sub(RegsHlN::Reg(H))), vec![pos0]),
            0x95 => (Arith(Sub(RegsHlN::Reg(L))), vec![pos0]),
            0x96 => (Arith(Sub(RegsHlN::HlInd)), vec![pos0]),
            0x97 => (Arith(Sub(RegsHlN::Reg(A))), vec![pos0]),
            0x98 => (Arith(Sbc(RegsHl::Reg(B))), vec![pos0]),
            0x99 => (Arith(Sbc(RegsHl::Reg(C))), vec![pos0]),
            0x9a => (Arith(Sbc(RegsHl::Reg(D))), vec![pos0]),
            0x9b => (Arith(Sbc(RegsHl::Reg(E))), vec![pos0]),
            0x9c => (Arith(Sbc(RegsHl::Reg(H))), vec![pos0]),
            0x9d => (Arith(Sbc(RegsHl::Reg(L))), vec![pos0]),
            0x9e => (Arith(Sbc(RegsHl::HlInd)), vec![pos0]),
            0x9f => (Arith(Sbc(RegsHl::Reg(A))), vec![pos0]),
            0xa0 => (Arith(And(RegsHlN::Reg(B))), vec![pos0]),
            0xa1 => (Arith(And(RegsHlN::Reg(C))), vec![pos0]),
            0xa2 => (Arith(And(RegsHlN::Reg(D))), vec![pos0]),
            0xa3 => (Arith(And(RegsHlN::Reg(E))), vec![pos0]),
            0xa4 => (Arith(And(RegsHlN::Reg(H))), vec![pos0]),
            0xa5 => (Arith(And(RegsHlN::Reg(L))), vec![pos0]),
            0xa6 => (Arith(And(RegsHlN::HlInd)), vec![pos0]),
            0xa7 => (Arith(And(RegsHlN::Reg(A))), vec![pos0]),
            0xa8 => (Arith(Xor(RegsHlN::Reg(B))), vec![pos0]),
            0xa9 => (Arith(Xor(RegsHlN::Reg(C))), vec![pos0]),
            0xaa => (Arith(Xor(RegsHlN::Reg(D))), vec![pos0]),
            0xab => (Arith(Xor(RegsHlN::Reg(E))), vec![pos0]),
            0xac => (Arith(Xor(RegsHlN::Reg(H))), vec![pos0]),
            0xad => (Arith(Xor(RegsHlN::Reg(L))), vec![pos0]),
            0xae => (Arith(Xor(RegsHlN::HlInd)), vec![pos0]),
            0xaf => (Arith(Xor(RegsHlN::Reg(A))), vec![pos0]),
            0xb0 => (Arith(Or(RegsHlN::Reg(B))), vec![pos0]),
            0xb1 => (Arith(Or(RegsHlN::Reg(C))), vec![pos0]),
            0xb2 => (Arith(Or(RegsHlN::Reg(D))), vec![pos0]),
            0xb3 => (Arith(Or(RegsHlN::Reg(E))), vec![pos0]),
            0xb4 => (Arith(Or(RegsHlN::Reg(H))), vec![pos0]),
            0xb5 => (Arith(Or(RegsHlN::Reg(L))), vec![pos0]),
            0xb6 => (Arith(Or(RegsHlN::HlInd)), vec![pos0]),
            0xb7 => (Arith(Or(RegsHlN::Reg(A))), vec![pos0]),
            0xb8 => (Arith(Cp(RegsHlN::Reg(B))), vec![pos0]),
            0xb9 => (Arith(Cp(RegsHlN::Reg(C))), vec![pos0]),
            0xba => (Arith(Cp(RegsHlN::Reg(D))), vec![pos0]),
            0xbb => (Arith(Cp(RegsHlN::Reg(E))), vec![pos0]),
            0xbc => (Arith(Cp(RegsHlN::Reg(H))), vec![pos0]),
            0xbd => (Arith(Cp(RegsHlN::Reg(L))), vec![pos0]),
            0xbe => (Arith(Cp(RegsHlN::HlInd)), vec![pos0]),
            0xbf => (Arith(Cp(RegsHlN::Reg(A))), vec![pos0]),
            0xc0 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xc1 => (Instr::PopBc, vec![pos0]),
            0xc2 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xc3 => {
                let addr = self.read16();
                let (hi, lo) = hi_lo_decompose(addr);
                (Jump(Jp(Addr::directly(addr))), vec![pos0, lo, hi])
            }
            0xc4 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xc5 => (Instr::PushBc, vec![pos0]),
            0xc6 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xc7 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xc8 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xc9 => (Instr::Ret, vec![pos0]),
            0xca => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xcb => {
                let pos1 = self.read8();
                match pos1 {
                    0x11 => (Rotate(Rl(RegisterKind8::C)), vec![pos0, pos1]),
                    0x7c => (Bit7h, vec![pos0, pos1]),
                    _ => panic!(format!("unimplemented instruction {}", pos1)),
                }
            }
            0xcc => {
                let addr = self.read16();
                let (hi, lo) = hi_lo_decompose(addr);
                (Jump(CallZ(Addr::directly(addr))), vec![pos0, lo, hi])
            }
            0xcd => {
                let addr = self.read16();
                let (hi, lo) = hi_lo_decompose(addr);
                (Jump(Call(Addr::directly(addr))), vec![pos0, lo, hi])
            }
            0xce => {
                let pos1 = self.read8();
                (Arith(Add(RegsHlN::N(pos1))), vec![pos0, pos1])
            }
            0xcf => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xd0 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xd1 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xd2 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xd3 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xd4 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xd5 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xd6 => {
                let pos1 = self.read8();
                (Arith(Sub(RegsHlN::N(pos1))), vec![pos0, pos1])
            }

            0xd7 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xd8 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xd9 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xda => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xdb => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xdc => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xdd => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xde => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xdf => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xe0 => {
                let pos1 = self.read8();
                (Ld(IOOffsetGetsA(pos1)), vec![pos0, pos1])
            }
            0xe1 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xe2 => (Ld(IOOffsetByCGetsA), vec![pos0]),
            0xe3 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xe4 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xe5 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xe6 => {
                let pos1 = self.read8();
                (Arith(And(RegsHlN::N(pos1))), vec![pos0, pos1])
            }
            0xe7 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xe8 => {
                let pos1 = self.read8();
                (Arith(AddSp(pos1 as i8)), vec![pos0, pos1])
            }
            0xe9 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xea => {
                let addr = self.read16();
                let (hi, lo) = hi_lo_decompose(addr);
                (Ld(NnIndGetsA(Addr::directly(addr))), vec![pos0, lo, hi])
            }
            0xeb => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xec => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xed => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xee => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xef => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xf0 => {
                let pos1 = self.read8();
                (Ld(AGetsIOOffset(pos1)), vec![pos0, pos1])
            }
            0xf1 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xf2 => (Ld(AGetsIOOffsetByC), vec![pos0]),
            0xf3 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xf4 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xf5 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xf6 => {
                let pos1 = self.read8();
                (Arith(Or(RegsHlN::N(pos1))), vec![pos0, pos1])
            }
            0xf7 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xf8 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xf9 => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xfa => {
                let addr = self.read16();
                let (hi, lo) = hi_lo_decompose(addr);
                (Ld(AGetsNnInd(Addr::directly(addr))), vec![pos0, lo, hi])
            }
            0xfb => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xfc => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xfd => panic!(format!("unimplemented instruction ${:x}", pos0)),
            0xfe => {
                let pos1 = self.read8();
                (Arith(Cp(RegsHlN::N(pos1))), vec![pos0, pos1])
            }
            0xff => panic!(format!("unimplemented instruction ${:x}", pos0)),
        }
    }

    fn read(&'a mut self) -> Instr {
        let (i, _) = self.read_();
        i
    }

    fn peek(&'a mut self) -> Instr {
        let (i, n) = { self.read_() };
        self.ptr.rewind(n.len() as u16);
        i
    }
}

impl InstrPointer {
    pub fn read(&mut self, memory: &Memory) -> Instr {
        LiveInstrPointer::create(self, memory).read()
    }

    pub fn read_(&mut self, memory: &Memory) -> (Instr, Vec<u8>) {
        LiveInstrPointer::create(self, memory).read_()
    }

    pub fn peek(&mut self, memory: &Memory) -> Instr {
        LiveInstrPointer::create(self, memory).peek()
    }
}

#[cfg(test)]
mod tests {
    use instr::InstrPointer;
    use mem::{Addr, Memory, BOOTROM};
    //use test::proptest::prelude::*;

    #[test]
    fn bootrom_roundtrip() {
        let memory = Memory::create();
        let mut ptr = InstrPointer::create();

        let mut acc = Vec::new();

        println!("Starting... {:?}", ptr);
        while ptr.0 != Addr::directly(0x100) {
            // skip the Nintendo logo data
            if ptr.0 == Addr::directly(0xa8) {
                ptr.jump(Addr::directly(0xe0));
                // the data
                acc.append(&mut vec![
                    0xce, 0xed, 0x66, 0x66, 0xcc, 0x0d, 0x00, 0x0b, /*00000b0*/ 0x03, 0x73,
                    0x00, 0x83, 0x00, 0x0c, 0x00, 0x0d, 0x00, 0x08, 0x11, 0x1f, 0x88, 0x89, 0x00,
                    0x0e, /*00000c0*/ 0xdc, 0xcc, 0x6e, 0xe6, 0xdd, 0xdd, 0xd9, 0x99, 0xbb,
                    0xbb, 0x67, 0x63, 0x6e, 0x0e, 0xec, 0xcc, /*00000d0*/ 0xdd, 0xdc, 0x99,
                    0x9f, 0xbb, 0xb9, 0x33, 0x3e, 0x3c, 0x42, 0xb9, 0xa5, 0xb9, 0xa5, 0x42, 0x3c,
                ]);
                continue;
            }

            let addr = ptr.0;
            let (i, mut bytes) = ptr.read_(&memory);
            println!("{:?} @ 0x{:x}", i, addr.into_register().0);
            acc.append(&mut bytes);
        }

        for (i, (x, y)) in BOOTROM.to_vec().iter().zip(acc.iter()).enumerate() {
            println!("Testing at offset ${:x}", i);
            assert_eq!(x, y);
        }
    }
}
