#![allow(dead_code)]

use mem::{Addr, Direction, Memory};
#[cfg(test)]
use proptest_derive::Arbitrary;
use register::R8;
use register_kind::{RegisterKind16, RegisterKind8};
use std::error::Error;
use std::fmt;

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

    fn cause(&self) -> Option<&dyn Error> {
        // Generic error, underlying cause isn't tracked.
        None
    }
}

pub trait HasDuration {
    // The left of the tuple is the duration if the conditional is taken
    // The right is if the conditional is not taken (or None if N/A)
    fn duration(&self) -> (u32, Option<u32>);
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PutGet {
    Put,
    Get,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OffsetBy {
    C,
    N(u8),
}
impl fmt::Display for OffsetBy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OffsetBy::C => write!(f, "c"),
            OffsetBy::N(x) => write!(f, "{:02x}", x),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ld {
    Word(RegsHl, RegsHlN),
    AOpInd(RegisterKind16, PutGet),
    AOpNnInd(Addr, PutGet),
    NnIndGetsSp(Addr),
    AOpIOOffset(OffsetBy, PutGet),
    AOpHlInd(Direction, PutGet),
    DwordGetsAddr(RegisterKind16, Addr),
    SpGetsHl,
    HlGetsSpOffset(i8),
}
use self::Ld::*;

impl fmt::Display for Ld {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Word(r1, r2) => write!(f, "ld {:},{:}", r1, r2),
            AOpInd(r, PutGet::Get) => write!(f, "ld {:},[{:}]", RegisterKind8::A, r),
            AOpInd(r, PutGet::Put) => write!(f, "ld [{:}],{:}", r, RegisterKind8::A),
            AOpNnInd(addr, PutGet::Get) => write!(f, "ld {:},[{:}]", RegisterKind8::A, addr),
            AOpNnInd(addr, PutGet::Put) => write!(f, "ld [{:}],{:}", addr, RegisterKind8::A),
            NnIndGetsSp(addr) => write!(f, "ld [{:}],{:}", addr, RegisterKind16::Sp),
            AOpIOOffset(offset_by, putget) => {
                let arg = match offset_by {
                    OffsetBy::C => format!("[$ff00+c]"),
                    OffsetBy::N(x) => format!("[$ff{:02x}]", x),
                };
                match putget {
                    PutGet::Get => write!(f, "ldh {:},{:}", RegisterKind8::A, arg),
                    PutGet::Put => write!(f, "ldh {:},{:}", arg, RegisterKind8::A),
                }
            }
            AOpHlInd(dir, putget) => {
                let incdec = match dir {
                    Direction::Pos => "+",
                    Direction::Neg => "-",
                };
                match putget {
                    PutGet::Put => write!(
                        f,
                        "ld [{:}{:}],{:}",
                        RegisterKind16::Hl,
                        incdec,
                        RegisterKind8::A
                    ),
                    PutGet::Get => write!(
                        f,
                        "ld {:},[{:}{:}]",
                        RegisterKind8::A,
                        RegisterKind16::Hl,
                        incdec
                    ),
                }
            }
            DwordGetsAddr(r, addr) => write!(f, "ld {:},{:}", r, addr),
            SpGetsHl => write!(f, "ld {:},{:}", RegisterKind16::Sp, RegisterKind16::Hl),
            HlGetsSpOffset(x) => write!(
                f,
                "ld {:},{:}+{:02x}",
                RegisterKind16::Hl,
                RegisterKind16::Sp,
                x
            ),
        }
    }
}

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
impl HasDuration for Ld {
    fn duration(&self) -> (u32, Option<u32>) {
        match self {
            Word(RegsHl::Reg(_), RegsHlN::Reg(_)) => (1, None),
            Word(RegsHl::Reg(_), RegsHlN::N(_) | RegsHlN::HlInd) => (2, None),
            Word(RegsHl::HlInd, RegsHlN::Reg(_)) => (2, None),
            Word(RegsHl::HlInd, RegsHlN::N(_)) => (3, None),
            Word(RegsHl::HlInd, RegsHlN::HlInd) => {
                panic!("ld [hl], [hl] isn't a valid instruction")
            }
            AOpInd(_, _) => (2, None),
            AOpNnInd(_, _) => (4, None),
            NnIndGetsSp(_) => (5, None),
            AOpIOOffset(OffsetBy::N(_), _) => (3, None),
            AOpIOOffset(OffsetBy::C, _) => (2, None),
            AOpHlInd(_, _) => (2, None),
            DwordGetsAddr(_, _) => (3, None),
            SpGetsHl => (2, None),
            HlGetsSpOffset(_) => (3, None),
        }
    }
}

// TODO: Hlist for the variants so we can reuse these better across all instrs
#[derive(Debug, Clone, PartialEq)]
pub enum RegsHlN {
    HlInd,
    N(u8),
    Reg(RegisterKind8),
}
impl fmt::Display for RegsHlN {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RegsHlN::HlInd => write!(f, "[hl]"),
            RegsHlN::N(n) => write!(f, "{:}", R8(*n)),
            RegsHlN::Reg(rk) => write!(f, "{:}", rk),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum RegsHl {
    HlInd,
    Reg(RegisterKind8),
}
impl RegsHl {
    fn upcast(&self) -> RegsHlN {
        match self {
            RegsHl::HlInd => RegsHlN::HlInd,
            RegsHl::Reg(r) => RegsHlN::Reg(*r),
        }
    }
}
impl fmt::Display for RegsHl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RegsHl::HlInd => write!(f, "[hl]"),
            RegsHl::Reg(rk) => write!(f, "{:}", rk),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Arith {
    Add(RegsHlN),
    Adc(RegsHlN),
    Sub(RegsHlN),
    Sbc(RegsHlN),
    And(RegsHlN),
    Or(RegsHlN),
    Xor(RegsHlN),
    Cp(RegsHlN),
    Inc(RegsHl),
    Dec(RegsHl),
    Swap(RegsHl),
    Sla(RegsHl),
    Sra(RegsHl),
    Srl(RegsHl),
    Cpl,

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
            Add(x) => write!(f, "add a,{:}", x),
            Adc(x) => write!(f, "adc a,{:}", x),
            Sub(x) => write!(f, "sub a,{:}", x),
            Sbc(x) => write!(f, "sbc a,{:}", x),
            And(x) => write!(f, "and a,{:}", x),
            Or(x) => write!(f, "or a,{:}", x),
            Xor(x) => write!(f, "xor a,{:}", x),
            Cp(x) => write!(f, "cp a,{:}", x),
            Inc(x) => write!(f, "inc {:}", x),
            Dec(x) => write!(f, "dec {:}", x),
            Swap(x) => write!(f, "swap {:}", x),
            Sla(x) => write!(f, "sla {:}", x),
            Sra(x) => write!(f, "sra {:}", x),
            Srl(x) => write!(f, "srl {:}", x),
            Cpl => write!(f, "cpl"),

            // 16 bit
            AddHl(rk) => write!(f, "add hl,{:}", rk),
            AddSp(x) => write!(f, "add sp,{:}", x),
            Inc16(rk) => write!(f, "inc {:}", rk),
            Dec16(rk) => write!(f, "dec {:}", rk),
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
            Sbc(RegsHlN::N(_)) => (2, None),
            Sbc(RegsHlN::HlInd) => (2, None),
            Sbc(RegsHlN::Reg(_)) => (1, None),
            Inc(x) | Dec(x) => match x {
                RegsHl::HlInd => (3, None),
                RegsHl::Reg(_) => (1, None),
            },
            AddHl(_) => (2, None),
            AddSp(_) => (4, None),
            Inc16(_) | Dec16(_) => (2, None),
            Cpl => (1, None),
            Swap(x) => match x {
                RegsHl::Reg(_) => (2, None),
                RegsHl::HlInd => (4, None),
            },
            Sla(x) => match x {
                RegsHl::Reg(_) => (2, None),
                RegsHl::HlInd => (4, None),
            },
            Sra(x) => match x {
                RegsHl::Reg(_) => (2, None),
                RegsHl::HlInd => (4, None),
            },
            Srl(x) => match x {
                RegsHl::Reg(_) => (2, None),
                RegsHl::HlInd => (4, None),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Rotate {
    Rla,
    Rra,
    Rlca,
    Rrca,
    Rl(RegsHl),
    Rlc(RegsHl),
    Rrc(RegsHl),
    Rr(RegsHl),
}
use self::Rotate::*;

impl fmt::Display for Rotate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Rla => write!(f, "rla"),
            Rra => write!(f, "rra"),
            Rlca => write!(f, "rlca"),
            Rrca => write!(f, "rrca"),
            Rl(r) => write!(f, "rl {:}", r),
            Rlc(r) => write!(f, "rlc {:}", r),
            Rrc(r) => write!(f, "rrc {:}", r),
            Rr(r) => write!(f, "rr {:}", r),
        }
    }
}

impl HasDuration for Rotate {
    fn duration(&self) -> (u32, Option<u32>) {
        match self {
            Rla => (1, None),
            Rra => (1, None),
            Rlca => (1, None),
            Rrca => (1, None),
            Rl(RegsHl::HlInd) => (4, None),
            Rl(RegsHl::Reg(_)) => (2, None),
            Rlc(RegsHl::HlInd) => (4, None),
            Rlc(RegsHl::Reg(_)) => (2, None),
            Rrc(RegsHl::HlInd) => (4, None),
            Rrc(RegsHl::Reg(_)) => (2, None),
            Rr(RegsHl::HlInd) => (4, None),
            Rr(RegsHl::Reg(_)) => (2, None),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Jump {
    Jp(Addr),
    JpCc(RetCondition, Addr),
    JpHlInd,
    Jr(i8),
    JrCc(RetCondition, i8),
    Call(Addr),
    CallCc(RetCondition, Addr),
    Rst(u8),
}
use self::Jump::*;

impl fmt::Display for Jump {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Jp(addr) => write!(f, "jp {:}", addr),
            JpCc(c, addr) => write!(f, "jp {:},{:}", c, addr),
            JpHlInd => write!(f, "jp hl"),
            Jr(n) => write!(f, "jr {:02x}", n),
            JrCc(c, i) => write!(f, "jr {:},{:}", c, i),
            Call(addr) => write!(f, "call {:}", addr),
            CallCc(c, addr) => write!(f, "call {:},{:}", c, addr),
            Rst(n) => write!(f, "RST ${:02x}", n),
        }
    }
}

impl HasDuration for Jump {
    fn duration(&self) -> (u32, Option<u32>) {
        match self {
            Jp(_) => (4, None),
            JpCc(_, _) => (4, Some(3)),
            JpHlInd => (1, None),
            Jr(_) => (3, None),
            JrCc(_, _) => (3, Some(2)),
            Call(_) => (6, None),
            CallCc(_, _) => (6, Some(3)),
            Rst(_) => (8, None),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Bits {
    Bit(u8, RegsHl),
    Res(u8, RegsHl),
    Set(u8, RegsHl),
}
use self::Bits::*;

impl fmt::Display for Bits {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Bit(b, r) => write!(f, "bit {:},{:}", b, r),
            Res(b, r) => write!(f, "res {:},{:}", b, r),
            Set(b, r) => write!(f, "set {:},{:}", b, r),
        }
    }
}

impl HasDuration for Bits {
    fn duration(&self) -> (u32, Option<u32>) {
        match self {
            Set(_, x) | Bit(_, x) | Res(_, x) => match x {
                RegsHl::Reg(_) => (2, None),
                RegsHl::HlInd => (4, None),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RetCondition {
    Nz,
    Z,
    Nc,
    C,
}

impl fmt::Display for RetCondition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RetCondition::Nz => write!(f, "nz"),
            RetCondition::Z => write!(f, "z"),
            RetCondition::Nc => write!(f, "nc"),
            RetCondition::C => write!(f, "c"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instr {
    Ld(Ld),
    Arith(Arith),
    Rotate(Rotate),
    Jump(Jump),
    Bits(Bits),
    CpHlInd,
    Pop(RegisterKind16),
    PopAf,
    Push(RegisterKind16),
    PushAf,
    Ret,
    RetCc(RetCondition),
    Reti,
    Nop,
    Di,
    Ei,
    Scf,
    Daa,
    Halt,
    Stop,
    Ccf,
    InvalidOp(u8),
}
use self::Instr::*;

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ld(ld) => write!(f, "{:}", ld),
            Arith(a) => write!(f, "{:}", a),
            Rotate(r) => write!(f, "{:}", r),
            Jump(j) => write!(f, "{:}", j),
            Bits(b) => write!(f, "{:}", b),
            CpHlInd => write!(f, "cp a,[hl]"),
            Pop(r16) => write!(f, "pop {:}", r16),
            PopAf => write!(f, "pop af"),
            Push(r16) => write!(f, "push {:}", r16),
            PushAf => write!(f, "push af"),
            Ret => write!(f, "ret"),
            RetCc(cond) => write!(f, "ret {:}", cond),
            Reti => write!(f, "reti"),
            Nop => write!(f, "nop"),
            Ei => write!(f, "ei"),
            Di => write!(f, "di"),
            Scf => write!(f, "scf"),
            Ccf => write!(f, "ccf"),
            Daa => write!(f, "daa"),
            Halt => write!(f, "halt"),
            Stop => write!(f, "stop"),
            InvalidOp(n) => write!(f, "bad {:x}", n),
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
            Bits(bits) => bits.duration(),
            CpHlInd => (2, None),
            Pop(_) => (3, None),
            PopAf => (3, None),
            Push(_) => (4, None),
            PushAf => (4, None),
            Ret | Reti => (4, None),
            RetCc(_) => (5, Some(2)),
            Nop => (1, None),
            InvalidOp(_) => (1, None),
            Di | Ei | Scf | Ccf | Daa | Halt | Stop => (1, None),
        }
    }
}

// This was an artifact of a time when it seemed like it would be necessary to
// keep a cache of seen instructions in order to print the stream backwards.
// Keeping this here because it's interesting and maybe will become a blog post.
//
// A "well-behaved" lens, but not a "very well-behaved" lens
//
// PutGet -- jumping to what you are at doesn't change anything
// forall t. t = t.jump( t.into_addr() )
//
// GetPut -- you get what you've jumped to
// forall t a. a = t.jump(a).into_addr()
//
// The very well case
// ( Not in this case!! )
// PutPut -- jumping twice is the same as jumping once
// forall t a b. t.jump(b) = t.jump(a).jump(b)
//
// Why not? Because we want to admit the "trace" computation
// so we can print instructions backwards.
//
// This is not that easy, we can't just walk backwards in memory:
// Consider the following example:
//
// ... $c4 $cb $2f $00 ...
//                  ^
//
// We are currently at $00, the `nop` instruction. If we look one byte back we
// see we have $2f or the bitwise complement instruction. But if we look
// another byte back we see $cb which is the prefix opcode for 2-byte
// instructions -- so we should have interpreted the $2f as $cb $2f or the
// shift-right into carry instruction. But then we look another byte back: $c4
// or the call if the zero flag is not set instruction which uses the following
// two bytes for the address! So we should have interpreted it as $c4 $cb $2f or
// "call nz, $2fcb" some function in bank 0 of a gameboy ROM.
//
// Three bytes is the longest an instruction can take on this processor so you
// always need to look three bytes behind the earliest instruction you want to
// print. This leads to a heuristic so you know when to stop walking backwards.
//
// Knowing this, we can induce a backwards iterator on the instruction stream
// carefully.
//
/*
pub trait InstrPointerTrait {
    fn create() -> Self;
    fn into_addr(&self) -> Addr;
    fn jump(&mut self, addr: Addr);

    fn into_u16(&self) -> u16 {
        self.into_addr().into_register().0
    }
    fn offset_by(&mut self, n: i8) {
        self.jump(self.into_addr().offset_signed(n));
    }
    fn inc_by(&mut self, n: u16) {
        let res = self.into_u16();
        self.jump(Addr::directly(res + n));
    }

    fn inc(&mut self) {
        self.inc_by(1);
    }
}
*/

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct InstrPointer(pub Addr);
impl InstrPointer {
    pub fn create() -> InstrPointer {
        InstrPointer(Addr::directly(0x00))
    }

    fn into_addr(&self) -> Addr {
        self.0
    }

    pub fn jump(&mut self, addr: Addr) {
        (*self).0 = addr
    }
}

// bonus functions:
impl InstrPointer {
    pub fn into_u16(&self) -> u16 {
        self.into_addr().into_register().0
    }
    pub fn offset_by(&mut self, n: i8) {
        self.jump(self.into_addr().offset_signed(n));
    }
    fn inc_by(&mut self, n: u16) {
        let res = self.into_u16();
        self.jump(Addr::directly(res + n));
    }

    fn inc(&mut self) {
        self.inc_by(1);
    }
}

pub trait ReadOnlyTape {
    fn peek8_offset(&self, by: i8) -> u8;
    fn peek16_offset(&self, by: i8) -> u16;

    fn peek8(&self) -> u8 {
        self.peek8_offset(0)
    }

    fn peek16(&self) -> u16 {
        self.peek16_offset(0)
    }

    fn peek(&self) -> Instr {
        let (i, _) = self.peek_();
        i
    }

    // returns instruction and bytes read by the PC
    fn peek_(&self) -> (Instr, Vec<u8>) {
        fn word_regshl(x: u8) -> RegsHl {
            match x {
                0 => RegsHl::Reg(B),
                1 => RegsHl::Reg(C),
                2 => RegsHl::Reg(D),
                3 => RegsHl::Reg(E),
                4 => RegsHl::Reg(H),
                5 => RegsHl::Reg(L),
                6 => RegsHl::HlInd,
                7 => RegsHl::Reg(A),
                _ => panic!("Unexpected input for word_regshl: {:}", x),
            }
        }

        let pos0 = self.peek8();
        //use web_utils::log;
        //log(&format!("Decode ${:x}", pos0));
        match pos0 {
            0x00 => (Nop, vec![pos0]),
            // 8bit loads
            0x02 | 0x0a | 0x12 | 0x1a => {
                let reg = match pos0 / 0x10 {
                    0 => Bc,
                    1 => De,
                    x => panic!("Unexpected reg value {:}", x),
                };
                let put_get = match pos0 % 0x10 {
                    2 => PutGet::Put,
                    0xa => PutGet::Get,
                    x => panic!("Unexpected putget value {:}", x),
                };
                (Ld(AOpInd(reg, put_get)), vec![pos0])
            }
            0x22 | 0x2a | 0x32 | 0x3a => {
                let dir = match pos0 / 0x10 {
                    2 => Direction::Pos,
                    3 => Direction::Neg,
                    x => panic!("Unexpected dir value {:}", x),
                };
                let put_get = match pos0 % 0x10 {
                    2 => PutGet::Put,
                    0xa => PutGet::Get,
                    x => panic!("Unexpected putget value {:}", x),
                };
                (Ld(AOpHlInd(dir, put_get)), vec![pos0])
            }
            0x06 | 0x16 | 0x26 | 0x36 | 0x0e | 0x1e | 0x2e | 0x3e => {
                let reg = word_regshl(pos0 / 8);
                let pos1 = self.peek8_offset(1);
                (Ld(Word(reg, RegsHlN::N(pos1))), vec![pos0, pos1])
            }
            0x40..0x80 => {
                let r1 = word_regshl((pos0 - 0x40) / 8);
                let r2 = word_regshl(pos0 % 8);
                if r1 == RegsHl::HlInd && r2 == RegsHl::HlInd {
                    // halt is instead of (HL), (HL)
                    (Halt, vec![pos0])
                } else {
                    (Ld(Word(r1, r2.upcast())), vec![pos0])
                }
            }
            0xe0 | 0xf0 | 0xe2 | 0xf2 => {
                let put_get = match pos0 / 0x10 {
                    0xe => PutGet::Put,
                    0xf => PutGet::Get,
                    x => panic!("Unexpected putget value {:}", x),
                };
                let (offset_by, instrs) = match pos0 % 0x10 {
                    0 => {
                        let pos1 = self.peek8_offset(1);
                        (OffsetBy::N(pos1), vec![pos0, pos1])
                    }
                    2 => (OffsetBy::C, vec![pos0]),
                    x => panic!("Unexpected offset_by value {:}", x),
                };
                (Ld(AOpIOOffset(offset_by, put_get)), instrs)
            }
            0xea | 0xfa => {
                let put_get = match pos0 / 0x10 {
                    0xe => PutGet::Put,
                    0xf => PutGet::Get,
                    x => panic!("Unexpected putget value {:}", x),
                };
                let addr = self.peek16_offset(1);
                let (hi, lo) = hi_lo_decompose(addr);
                (
                    Ld(AOpNnInd(Addr::directly(addr), put_get)),
                    vec![pos0, lo, hi],
                )
            }
            // 16bit loads
            0x01 | 0x11 | 0x21 | 0x31 => {
                let reg = match pos0 / 0x10 {
                    0 => Bc,
                    1 => De,
                    2 => Hl,
                    3 => Sp,
                    x => panic!("Unexpected reg value {:}", x),
                };

                let addr = self.peek16_offset(1);
                let (hi, lo) = hi_lo_decompose(addr);
                (
                    Ld(DwordGetsAddr(reg, Addr::directly(addr))),
                    vec![pos0, lo, hi],
                )
            }
            0x08 => {
                let addr = self.peek16_offset(1);
                let (hi, lo) = hi_lo_decompose(addr);
                (Ld(NnIndGetsSp(Addr::directly(addr))), vec![pos0, lo, hi])
            }
            0xf8 => {
                let pos1 = self.peek8_offset(1);
                (Ld(HlGetsSpOffset(pos1 as i8)), vec![pos0, pos1])
            }
            0xf9 => (Ld(SpGetsHl), vec![pos0]),
            // rest
            0x03 => (Arith(Inc16(Bc)), vec![pos0]),
            0x04 => (Arith(Inc(RegsHl::Reg(B))), vec![pos0]),
            0x05 => (Arith(Dec(RegsHl::Reg(B))), vec![pos0]),
            0x07 => (Rotate(Rlca), vec![pos0]),
            0x09 => (Arith(AddHl(Bc)), vec![pos0]),
            0x0b => (Arith(Dec16(Bc)), vec![pos0]),
            0x0c => (Arith(Inc(RegsHl::Reg(C))), vec![pos0]),
            0x0d => (Arith(Dec(RegsHl::Reg(C))), vec![pos0]),
            0x0f => (Rotate(Rrca), vec![pos0]),
            0x10 => {
                let pos1 = self.peek8_offset(1);
                (Stop, vec![pos0, pos1])
            }
            0x13 => (Arith(Inc16(De)), vec![pos0]),
            0x14 => (Arith(Inc(RegsHl::Reg(D))), vec![pos0]),
            0x15 => (Arith(Dec(RegsHl::Reg(D))), vec![pos0]),
            0x17 => (Rotate(Rla), vec![pos0]),
            0x18 => {
                let pos1 = self.peek8_offset(1);
                (Jump(Jr(pos1 as i8)), vec![pos0, pos1])
            }
            0x19 => (Arith(AddHl(De)), vec![pos0]),
            0x1b => (Arith(Dec16(De)), vec![pos0]),
            0x1c => (Arith(Inc(RegsHl::Reg(E))), vec![pos0]),
            0x1d => (Arith(Dec(RegsHl::Reg(E))), vec![pos0]),
            0x1f => (Rotate(Rra), vec![pos0]),
            0x20 => {
                let pos1 = self.peek8_offset(1);
                (Jump(JrCc(RetCondition::Nz, pos1 as i8)), vec![pos0, pos1])
            }
            0x23 => (Arith(Inc16(Hl)), vec![pos0]),
            0x24 => (Arith(Inc(RegsHl::Reg(H))), vec![pos0]),
            0x25 => (Arith(Dec(RegsHl::Reg(H))), vec![pos0]),
            0x27 => (Daa, vec![pos0]),
            0x28 => {
                let pos1 = self.peek8_offset(1);
                (Jump(JrCc(RetCondition::Z, pos1 as i8)), vec![pos0, pos1])
            }
            0x29 => (Arith(AddHl(Hl)), vec![pos0]),
            0x2b => (Arith(Dec16(Hl)), vec![pos0]),
            0x2c => (Arith(Inc(RegsHl::Reg(L))), vec![pos0]),
            0x2d => (Arith(Dec(RegsHl::Reg(L))), vec![pos0]),
            0x2f => (Arith(Cpl), vec![pos0]),
            0x30 => {
                let pos1 = self.peek8_offset(1);
                (Jump(JrCc(RetCondition::Nc, pos1 as i8)), vec![pos0, pos1])
            }
            0x33 => (Arith(Inc16(Sp)), vec![pos0]),
            0x34 => (Arith(Inc(RegsHl::HlInd)), vec![pos0]),
            0x35 => (Arith(Dec(RegsHl::HlInd)), vec![pos0]),
            0x37 => (Scf, vec![pos0]),
            0x38 => {
                let pos1 = self.peek8_offset(1);
                (Jump(JrCc(RetCondition::C, pos1 as i8)), vec![pos0, pos1])
            }
            0x39 => (Arith(AddHl(Sp)), vec![pos0]),
            0x3b => (Arith(Dec16(Sp)), vec![pos0]),
            0x3c => (Arith(Inc(RegsHl::Reg(A))), vec![pos0]),
            0x3d => (Arith(Dec(RegsHl::Reg(A))), vec![pos0]),
            0x3f => (Ccf, vec![pos0]),
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
            0x8f => (Arith(Adc(RegsHlN::Reg(A))), vec![pos0]),
            0x90 => (Arith(Sub(RegsHlN::Reg(B))), vec![pos0]),
            0x91 => (Arith(Sub(RegsHlN::Reg(C))), vec![pos0]),
            0x92 => (Arith(Sub(RegsHlN::Reg(D))), vec![pos0]),
            0x93 => (Arith(Sub(RegsHlN::Reg(E))), vec![pos0]),
            0x94 => (Arith(Sub(RegsHlN::Reg(H))), vec![pos0]),
            0x95 => (Arith(Sub(RegsHlN::Reg(L))), vec![pos0]),
            0x96 => (Arith(Sub(RegsHlN::HlInd)), vec![pos0]),
            0x97 => (Arith(Sub(RegsHlN::Reg(A))), vec![pos0]),
            0x98 => (Arith(Sbc(RegsHlN::Reg(B))), vec![pos0]),
            0x99 => (Arith(Sbc(RegsHlN::Reg(C))), vec![pos0]),
            0x9a => (Arith(Sbc(RegsHlN::Reg(D))), vec![pos0]),
            0x9b => (Arith(Sbc(RegsHlN::Reg(E))), vec![pos0]),
            0x9c => (Arith(Sbc(RegsHlN::Reg(H))), vec![pos0]),
            0x9d => (Arith(Sbc(RegsHlN::Reg(L))), vec![pos0]),
            0x9e => (Arith(Sbc(RegsHlN::HlInd)), vec![pos0]),
            0x9f => (Arith(Sbc(RegsHlN::Reg(A))), vec![pos0]),
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
            0xc0 => (RetCc(RetCondition::Nz), vec![pos0]),
            0xc1 => (Pop(RegisterKind16::Bc), vec![pos0]),
            0xc2 => {
                let addr = self.peek16_offset(1);
                let (hi, lo) = hi_lo_decompose(addr);
                (
                    Jump(JpCc(RetCondition::Nz, Addr::directly(addr))),
                    vec![pos0, lo, hi],
                )
            }
            0xc3 => {
                let addr = self.peek16_offset(1);
                let (hi, lo) = hi_lo_decompose(addr);
                (Jump(Jp(Addr::directly(addr))), vec![pos0, lo, hi])
            }
            0xc4 => {
                let addr = self.peek16_offset(1);
                let (hi, lo) = hi_lo_decompose(addr);
                (
                    Jump(CallCc(RetCondition::Nz, Addr::directly(addr))),
                    vec![pos0, lo, hi],
                )
            }
            0xc5 => (Push(RegisterKind16::Bc), vec![pos0]),
            0xc6 => {
                let pos1 = self.peek8_offset(1);
                (Arith(Add(RegsHlN::N(pos1))), vec![pos0, pos1])
            }
            0xc7 => (Jump(Rst(0x00)), vec![pos0]),
            0xc8 => (RetCc(RetCondition::Z), vec![pos0]),
            0xc9 => (Instr::Ret, vec![pos0]),
            0xca => {
                let addr = self.peek16_offset(1);
                let (hi, lo) = hi_lo_decompose(addr);
                (
                    Jump(JpCc(RetCondition::Z, Addr::directly(addr))),
                    vec![pos0, lo, hi],
                )
            }
            0xcb => {
                let reg_lookup = |index| match index {
                    0x0 => RegsHl::Reg(B),
                    0x1 => RegsHl::Reg(C),
                    0x2 => RegsHl::Reg(D),
                    0x3 => RegsHl::Reg(E),
                    0x4 => RegsHl::Reg(H),
                    0x5 => RegsHl::Reg(L),
                    0x6 => RegsHl::HlInd,
                    0x7 => RegsHl::Reg(A),
                    x => panic!("Unexpected match value {:}", x),
                };
                let pos1 = self.peek8_offset(1);
                match pos1 {
                    0x00..=0x07 => {
                        let r = reg_lookup(pos1);
                        (Rotate(Rlc(r)), vec![pos0, pos1])
                    }
                    0x08..=0x0f => {
                        let r = reg_lookup(pos1 - 0x08);
                        (Rotate(Rrc(r)), vec![pos0, pos1])
                    }
                    0x10..=0x17 => {
                        let r = reg_lookup(pos1 - 0x10);
                        (Rotate(Rl(r)), vec![pos0, pos1])
                    }
                    0x18..=0x1f => {
                        let r = reg_lookup(pos1 - 0x18);
                        (Rotate(Rr(r)), vec![pos0, pos1])
                    }
                    0x20..=0x27 => {
                        let r = reg_lookup(pos1 - 0x20);
                        (Arith(Sla(r)), vec![pos0, pos1])
                    }
                    0x28..=0x2f => {
                        let r = reg_lookup(pos1 - 0x28);
                        (Arith(Sra(r)), vec![pos0, pos1])
                    }
                    0x30 => (Arith(Swap(RegsHl::Reg(B))), vec![pos0, pos1]),
                    0x31 => (Arith(Swap(RegsHl::Reg(C))), vec![pos0, pos1]),
                    0x32 => (Arith(Swap(RegsHl::Reg(D))), vec![pos0, pos1]),
                    0x33 => (Arith(Swap(RegsHl::Reg(E))), vec![pos0, pos1]),
                    0x34 => (Arith(Swap(RegsHl::Reg(H))), vec![pos0, pos1]),
                    0x35 => (Arith(Swap(RegsHl::Reg(L))), vec![pos0, pos1]),
                    0x36 => (Arith(Swap(RegsHl::HlInd)), vec![pos0, pos1]),
                    0x37 => (Arith(Swap(RegsHl::Reg(A))), vec![pos0, pos1]),
                    0x38..=0x3f => {
                        let r = reg_lookup(pos1 - 0x38);
                        (Arith(Srl(r)), vec![pos0, pos1])
                    }
                    // bits, res, set
                    0x40..=0xff => {
                        let num = ((pos1 - 0x40) % 0x40) / 8;
                        assert!(num < 8);
                        let reg = match pos1 % 0x8 {
                            0x0 => RegsHl::Reg(B),
                            0x1 => RegsHl::Reg(C),
                            0x2 => RegsHl::Reg(D),
                            0x3 => RegsHl::Reg(E),
                            0x4 => RegsHl::Reg(H),
                            0x5 => RegsHl::Reg(L),
                            0x6 => RegsHl::HlInd,
                            0x7 => RegsHl::Reg(A),
                            _ => panic!("Unexpected num greater than 0x7"),
                        };
                        let cmd = match pos1 / 0x40 {
                            1 => Bit(num, reg),
                            2 => Res(num, reg),
                            3 => Set(num, reg),
                            _ => panic!("Unexpected division result"),
                        };
                        (Bits(cmd), vec![pos0, pos1])
                    }
                    _ => {
                        use web_utils::log;
                        log(&format!("Decode ${:x}", pos1));
                        panic!(format!("unimplemented instruction {}", pos1))
                    }
                }
            }
            0xcc => {
                let addr = self.peek16_offset(1);
                let (hi, lo) = hi_lo_decompose(addr);
                (
                    Jump(CallCc(RetCondition::Z, Addr::directly(addr))),
                    vec![pos0, lo, hi],
                )
            }
            0xcd => {
                let addr = self.peek16_offset(1);
                let (hi, lo) = hi_lo_decompose(addr);
                (Jump(Call(Addr::directly(addr))), vec![pos0, lo, hi])
            }
            0xce => {
                let pos1 = self.peek8_offset(1);
                (Arith(Adc(RegsHlN::N(pos1))), vec![pos0, pos1])
            }
            0xcf => (Jump(Rst(0x08)), vec![pos0]),
            0xd0 => (RetCc(RetCondition::Nc), vec![pos0]),
            0xd1 => (Pop(RegisterKind16::De), vec![pos0]),
            0xd2 => {
                let addr = self.peek16_offset(1);
                let (hi, lo) = hi_lo_decompose(addr);
                (
                    Jump(JpCc(RetCondition::Nc, Addr::directly(addr))),
                    vec![pos0, lo, hi],
                )
            }
            0xd3 => (InvalidOp(pos0), vec![pos0]),
            0xd4 => {
                let addr = self.peek16_offset(1);
                let (hi, lo) = hi_lo_decompose(addr);
                (
                    Jump(CallCc(RetCondition::Nc, Addr::directly(addr))),
                    vec![pos0, lo, hi],
                )
            }
            0xd5 => (Push(RegisterKind16::De), vec![pos0]),
            0xd6 => {
                let pos1 = self.peek8_offset(1);
                (Arith(Sub(RegsHlN::N(pos1))), vec![pos0, pos1])
            }

            0xd7 => (Jump(Rst(0x10)), vec![pos0]),
            0xd8 => (RetCc(RetCondition::C), vec![pos0]),
            0xd9 => (Reti, vec![pos0]),
            0xda => {
                let addr = self.peek16_offset(1);
                let (hi, lo) = hi_lo_decompose(addr);
                (
                    Jump(JpCc(RetCondition::C, Addr::directly(addr))),
                    vec![pos0, lo, hi],
                )
            }
            0xdb => (InvalidOp(pos0), vec![pos0]),
            0xdc => {
                let addr = self.peek16_offset(1);
                let (hi, lo) = hi_lo_decompose(addr);
                (
                    Jump(CallCc(RetCondition::C, Addr::directly(addr))),
                    vec![pos0, lo, hi],
                )
            }
            0xdd => (InvalidOp(pos0), vec![pos0]),
            0xde => {
                let pos1 = self.peek8_offset(1);
                (Arith(Sbc(RegsHlN::N(pos1))), vec![pos0, pos1])
            }
            0xdf => (Jump(Rst(0x18)), vec![pos0]),
            0xe1 => (Pop(RegisterKind16::Hl), vec![pos0]),
            0xe3 => (InvalidOp(pos0), vec![pos0]),
            0xe4 => (InvalidOp(pos0), vec![pos0]),
            0xe5 => (Push(RegisterKind16::Hl), vec![pos0]),
            0xe6 => {
                let pos1 = self.peek8_offset(1);
                (Arith(And(RegsHlN::N(pos1))), vec![pos0, pos1])
            }
            0xe7 => (Jump(Rst(0x20)), vec![pos0]),
            0xe8 => {
                let pos1 = self.peek8_offset(1);
                (Arith(AddSp(pos1 as i8)), vec![pos0, pos1])
            }
            0xe9 => (Jump(JpHlInd), vec![pos0]),
            0xeb => (InvalidOp(pos0), vec![pos0]),
            0xec => (InvalidOp(pos0), vec![pos0]),
            0xed => (InvalidOp(pos0), vec![pos0]),
            0xee => {
                let pos1 = self.peek8_offset(1);
                (Arith(Xor(RegsHlN::N(pos1))), vec![pos0, pos1])
            }
            0xef => (Jump(Rst(0x28)), vec![pos0]),
            0xf1 => (PopAf, vec![pos0]),
            0xf3 => (Di, vec![pos0]),
            0xf4 => (InvalidOp(pos0), vec![pos0]),
            0xf5 => (PushAf, vec![pos0]),
            0xf6 => {
                let pos1 = self.peek8_offset(1);
                (Arith(Or(RegsHlN::N(pos1))), vec![pos0, pos1])
            }
            0xf7 => (Jump(Rst(0x30)), vec![pos0]),
            0xfb => (Ei, vec![pos0]),
            0xfc => (InvalidOp(pos0), vec![pos0]),
            0xfd => (InvalidOp(pos0), vec![pos0]),
            0xfe => {
                let pos1 = self.peek8_offset(1);
                (Arith(Cp(RegsHlN::N(pos1))), vec![pos0, pos1])
            }
            0xff => (Jump(Rst(0x38)), vec![pos0]),
        }
    }
}

// Memory-like tape
pub trait Tape: ReadOnlyTape {
    fn read8(&mut self) -> u8;
    fn read16(&mut self) -> u16;

    fn advance(&mut self, by: i8);

    fn read(&mut self) -> Instr {
        let (i, _) = self.read_();
        i
    }

    fn read_(&mut self) -> (Instr, Vec<u8>) {
        let (i, n) = { self.peek_() };
        self.advance(n.len() as i8);
        (i, n)
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
}

impl<'a> ReadOnlyTape for LiveInstrPointer<'a> {
    fn peek8_offset(&self, by: i8) -> u8 {
        self.memory.ld8(self.ptr.0.offset_signed(by))
    }

    fn peek16_offset(&self, by: i8) -> u16 {
        self.memory.ld16(self.ptr.0.offset_signed(by))
    }
}

impl<'a> Tape for LiveInstrPointer<'a> {
    fn read8(&mut self) -> u8 {
        let v = self.peek8();
        self.ptr.inc();
        v
    }

    fn read16(&mut self) -> u16 {
        let v = self.peek16();
        self.ptr.inc_by(2);
        v
    }

    fn advance(&mut self, by: i8) {
        self.ptr.offset_by(by)
    }
}

pub fn hi_lo_decompose(x: u16) -> (u8, u8) {
    (((x & 0xff00) >> 8) as u8, (x & 0xff) as u8)
}

// See the following chart for inspiration on organization
// https://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
use register_kind::RegisterKind16::*;
use register_kind::RegisterKind8::*;
impl<'a> LiveInstrPointer<'a> {}

pub struct InstrPointerForwards<'a>(LiveInstrPointer<'a>);
impl<'a> Iterator for InstrPointerForwards<'a> {
    type Item = (Instr, Vec<u8>);

    fn next(&mut self) -> Option<(Instr, Vec<u8>)> {
        // TODO: Don't let this crash when it reads off the edge of the world
        Some(self.0.read_())
    }
}
pub struct InstrPointerBackwards<'a>(LiveInstrPointer<'a>);
impl<'a> Iterator for InstrPointerBackwards<'a> {
    type Item = (Instr, Vec<u8>);

    fn next(&mut self) -> Option<(Instr, Vec<u8>)> {
        let start_addr = self.0.ptr.into_addr();
        // if we're at the start, stop
        if start_addr == Addr::directly(0x00) {
            return None;
        }

        // rewind (up to) 3 bytes
        self.0
            .advance(-1 * (u16::min(3, start_addr.into_register().0) as i8));

        // try moving until we either decode an instruction that ends at the
        // point we started (and emit), or we decode past it (and give up) --
        // this second case can happen if we've jumped into the middle of
        // somewhere in RAM for example
        while self.0.ptr.into_addr() < start_addr {
            let try_addr = self.0.ptr.into_addr();

            let data = self.0.read_();
            // a hit, we decoded an instruction that successfully ended on our
            // start address
            if self.0.ptr.into_addr() == start_addr {
                // start we now are located at this earlier address
                self.0.ptr.jump(try_addr);
                return Some(data);
            }
            // start over 1 byte later
            self.0.ptr.jump(try_addr);
            self.0.advance(1);
        }

        // give up; we couldn't decode anything that is consistent
        None
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

    pub fn peek_(&mut self, memory: &Memory) -> (Instr, Vec<u8>) {
        LiveInstrPointer::create(self, memory).peek_()
    }

    pub fn instrs_forwards<'a>(&'a mut self, memory: &'a Memory) -> InstrPointerForwards<'a> {
        InstrPointerForwards(LiveInstrPointer::create(self, memory))
    }

    pub fn instrs_backwards<'a>(&'a mut self, memory: &'a Memory) -> InstrPointerBackwards<'a> {
        InstrPointerBackwards(LiveInstrPointer::create(self, memory))
    }
}

#[cfg(test)]
mod tests {
    use instr::{hi_lo_decompose, InstrPointer};
    use mem::{Addr, Memory, BOOTROM};
    //use test::proptest::prelude::*;

    #[test]
    fn load_decode() {
        use instr::Ld::*;
        use instr::{Ld, OffsetBy, PutGet, RegsHl, RegsHlN};
        use mem::Direction;
        use register_kind::RegisterKind16::*;
        use register_kind::RegisterKind8::*;

        let extra = Addr::directly(0xdead);
        let (hi, lo) = hi_lo_decompose(extra.into_register().0);

        let slot = Addr::directly(0xff80);
        let slot1 = slot.offset(1, Direction::Pos);
        let slot2 = slot.offset(2, Direction::Pos);

        let mut memory = Memory::create_with_bootrom();
        let mut ptr = InstrPointer::create();
        ptr.jump(slot);

        memory.st8(slot1, lo);
        memory.st8(slot2, hi);
        let mut check = |instr, i| {
            memory.st8(slot, i);

            let instr_ = ptr.peek(&memory);
            assert_eq!(
                instr,
                instr_,
                "Mem: {:x?}, i: {:x}",
                vec![memory.ld8(slot), memory.ld8(slot1), memory.ld8(slot2)],
                i
            );
        };

        let rhl = |r8| RegsHl::Reg(r8);
        let rhln = |r8| RegsHlN::Reg(r8);
        let n = |x| RegsHlN::N(x);

        (0..=255).for_each(|i| match i {
            // 8bit
            0x02 => check(Ld(AOpInd(Bc, PutGet::Put)), i),
            0x12 => check(Ld(AOpInd(De, PutGet::Put)), i),
            0x0a => check(Ld(AOpInd(Bc, PutGet::Get)), i),
            0x1a => check(Ld(AOpInd(De, PutGet::Get)), i),

            0x22 => check(Ld(AOpHlInd(Direction::Pos, PutGet::Put)), i),
            0x2a => check(Ld(AOpHlInd(Direction::Pos, PutGet::Get)), i),
            0x32 => check(Ld(AOpHlInd(Direction::Neg, PutGet::Put)), i),
            0x3a => check(Ld(AOpHlInd(Direction::Neg, PutGet::Get)), i),

            0x06 => check(Ld(Word(rhl(B), n(lo))), i),
            0x0e => check(Ld(Word(rhl(C), n(lo))), i),
            0x16 => check(Ld(Word(rhl(D), n(lo))), i),
            0x1e => check(Ld(Word(rhl(E), n(lo))), i),
            0x26 => check(Ld(Word(rhl(H), n(lo))), i),
            0x2e => check(Ld(Word(rhl(L), n(lo))), i),
            0x36 => check(Ld(Word(RegsHl::HlInd, n(lo))), i),
            0x3e => check(Ld(Word(rhl(A), n(lo))), i),

            0x40 => check(Ld(Word(rhl(B), rhln(B))), i),
            0x41 => check(Ld(Word(rhl(B), rhln(C))), i),
            0x42 => check(Ld(Word(rhl(B), rhln(D))), i),
            0x43 => check(Ld(Word(rhl(B), rhln(E))), i),
            0x44 => check(Ld(Word(rhl(B), rhln(H))), i),
            0x45 => check(Ld(Word(rhl(B), rhln(L))), i),
            0x46 => check(Ld(Word(rhl(B), RegsHlN::HlInd)), i),
            0x47 => check(Ld(Word(rhl(B), rhln(A))), i),
            0x48 => check(Ld(Word(rhl(C), rhln(B))), i),
            0x49 => check(Ld(Word(rhl(C), rhln(C))), i),
            0x4a => check(Ld(Word(rhl(C), rhln(D))), i),
            0x4b => check(Ld(Word(rhl(C), rhln(E))), i),
            0x4c => check(Ld(Word(rhl(C), rhln(H))), i),
            0x4d => check(Ld(Word(rhl(C), rhln(L))), i),
            0x4e => check(Ld(Word(rhl(C), RegsHlN::HlInd)), i),
            0x4f => check(Ld(Word(rhl(C), rhln(A))), i),
            0x50 => check(Ld(Word(rhl(D), rhln(B))), i),
            0x51 => check(Ld(Word(rhl(D), rhln(C))), i),
            0x52 => check(Ld(Word(rhl(D), rhln(D))), i),
            0x53 => check(Ld(Word(rhl(D), rhln(E))), i),
            0x54 => check(Ld(Word(rhl(D), rhln(H))), i),
            0x55 => check(Ld(Word(rhl(D), rhln(L))), i),
            0x56 => check(Ld(Word(rhl(D), RegsHlN::HlInd)), i),
            0x57 => check(Ld(Word(rhl(D), rhln(A))), i),
            0x58 => check(Ld(Word(rhl(E), rhln(B))), i),
            0x59 => check(Ld(Word(rhl(E), rhln(C))), i),
            0x5a => check(Ld(Word(rhl(E), rhln(D))), i),
            0x5b => check(Ld(Word(rhl(E), rhln(E))), i),
            0x5c => check(Ld(Word(rhl(E), rhln(H))), i),
            0x5d => check(Ld(Word(rhl(E), rhln(L))), i),
            0x5e => check(Ld(Word(rhl(E), RegsHlN::HlInd)), i),
            0x5f => check(Ld(Word(rhl(E), rhln(A))), i),
            0x60 => check(Ld(Word(rhl(H), rhln(B))), i),
            0x61 => check(Ld(Word(rhl(H), rhln(C))), i),
            0x62 => check(Ld(Word(rhl(H), rhln(D))), i),
            0x63 => check(Ld(Word(rhl(H), rhln(E))), i),
            0x64 => check(Ld(Word(rhl(H), rhln(H))), i),
            0x65 => check(Ld(Word(rhl(H), rhln(L))), i),
            0x66 => check(Ld(Word(rhl(H), RegsHlN::HlInd)), i),
            0x67 => check(Ld(Word(rhl(H), rhln(A))), i),
            0x68 => check(Ld(Word(rhl(L), rhln(B))), i),
            0x69 => check(Ld(Word(rhl(L), rhln(C))), i),
            0x6a => check(Ld(Word(rhl(L), rhln(D))), i),
            0x6b => check(Ld(Word(rhl(L), rhln(E))), i),
            0x6c => check(Ld(Word(rhl(L), rhln(H))), i),
            0x6d => check(Ld(Word(rhl(L), rhln(L))), i),
            0x6e => check(Ld(Word(rhl(L), RegsHlN::HlInd)), i),
            0x6f => check(Ld(Word(rhl(L), rhln(A))), i),
            0x70 => check(Ld(Word(RegsHl::HlInd, rhln(B))), i),
            0x71 => check(Ld(Word(RegsHl::HlInd, rhln(C))), i),
            0x72 => check(Ld(Word(RegsHl::HlInd, rhln(D))), i),
            0x73 => check(Ld(Word(RegsHl::HlInd, rhln(E))), i),
            0x74 => check(Ld(Word(RegsHl::HlInd, rhln(H))), i),
            0x75 => check(Ld(Word(RegsHl::HlInd, rhln(L))), i),
            0x77 => check(Ld(Word(RegsHl::HlInd, rhln(A))), i),
            0x78 => check(Ld(Word(rhl(A), rhln(B))), i),
            0x79 => check(Ld(Word(rhl(A), rhln(C))), i),
            0x7a => check(Ld(Word(rhl(A), rhln(D))), i),
            0x7b => check(Ld(Word(rhl(A), rhln(E))), i),
            0x7c => check(Ld(Word(rhl(A), rhln(H))), i),
            0x7d => check(Ld(Word(rhl(A), rhln(L))), i),
            0x7e => check(Ld(Word(rhl(A), RegsHlN::HlInd)), i),
            0x7f => check(Ld(Word(rhl(A), rhln(A))), i),

            0xe0 => check(Ld(AOpIOOffset(OffsetBy::N(lo), PutGet::Put)), i),
            0xf0 => check(Ld(AOpIOOffset(OffsetBy::N(lo), PutGet::Get)), i),

            0xe2 => check(Ld(AOpIOOffset(OffsetBy::C, PutGet::Put)), i),
            0xf2 => check(Ld(AOpIOOffset(OffsetBy::C, PutGet::Get)), i),

            0xea => check(Ld(AOpNnInd(extra, PutGet::Put)), i),
            0xfa => check(Ld(AOpNnInd(extra, PutGet::Get)), i),
            // 16bit
            0x01 => check(Ld(DwordGetsAddr(Bc, extra)), i),
            0x11 => check(Ld(DwordGetsAddr(De, extra)), i),
            0x21 => check(Ld(DwordGetsAddr(Hl, extra)), i),
            0x31 => check(Ld(DwordGetsAddr(Sp, extra)), i),
            0x08 => check(Ld(NnIndGetsSp(extra)), i),
            0xf9 => check(Ld(SpGetsHl), i),
            _ => (),
        })
    }

    #[test]
    fn bootrom_roundtrip() {
        let memory = Memory::create_with_bootrom();
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
