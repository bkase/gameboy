#![allow(dead_code)]

use alu;
use instr::{
    Arith, Bits, HasDuration, Instr, InstrPointer, Jump, Ld, OffsetBy, PutGet, RegsHl, RegsHlN,
    RetCondition, Rotate,
};
use mem::{Addr, Bootrom, Cartridge, Direction, Memory, Roms, BOOTROM};
use register::{Flags, Registers, R16, R8};
use register_kind::{RegisterKind16, RegisterKind8};
use std::borrow::Cow;
use these::These;

#[derive(Debug, PartialEq)]
pub struct Cpu {
    pub registers: Registers,
    pub memory: Memory,
    pub ip: InstrPointer,
    // a secret state bit that's controlled by interrupt-enabling/disabling instrs
    pub interrupt_master_enable: bool,
}

enum BranchAction {
    Take,
    Skip,
}

impl Cpu {
    pub fn create(roms: Roms) -> Cpu {
        let mut r = Registers::create();
        // adjust to state after bootrom
        // skipping the zeros because we default to zero naturally
        //
        // see http://bgb.bircd.org/pandocs.htm#powerupsequence
        if !roms.is_here() {
            r.a = R8(0x01);
            r.flags.z = true;
            r.flags.n = false;
            r.flags.h = true;
            r.flags.c = true;
            r.bc = R16(0x0013);
            r.de = R16(0x00d8);
            r.hl = R16(0x014d);
            r.sp = R16(0xfffe);
        }
        Cpu {
            registers: r,
            memory: Memory::create(roms),
            ip: InstrPointer::create(),
            interrupt_master_enable: true,
        }
    }

    pub fn create_with_registers(registers: Registers) -> Cpu {
        Cpu {
            registers,
            memory: Memory::create(These::This(Cow::Borrowed(BOOTROM))),
            ip: InstrPointer::create(),
            interrupt_master_enable: true,
        }
    }

    fn indirect_ld(&mut self, k: RegisterKind16) -> (u8, Addr) {
        let r = self.registers.read16(k);
        let addr = Addr::indirectly(r);
        (self.memory.ld8(addr), addr)
    }

    fn indirect_st(&mut self, k: RegisterKind16, n: u8) {
        let r = self.registers.read16(k);
        self.memory.st8(Addr::indirectly(r), n)
    }

    fn store_regshl(&mut self, x: RegsHl, n: u8) {
        match x {
            RegsHl::Reg(r) => self.registers.write8n(r, n),
            RegsHl::HlInd => self.indirect_st(RegisterKind16::Hl, n),
        }
    }

    fn execute_ld(&mut self, ld: Ld) -> BranchAction {
        use self::Ld::*;

        match ld {
            Word(r1, r2) => {
                let n = self.operand_regshln(r2);
                self.store_regshl(r1, n);
            }
            AOpInd(r16, PutGet::Get) => {
                let (n, _) = self.indirect_ld(r16);
                self.registers.write8n(RegisterKind8::A, n);
            }
            AOpInd(r16, PutGet::Put) => {
                let n = self.registers.read8(RegisterKind8::A);
                self.indirect_st(r16, n.0);
            }
            AOpNnInd(nn, PutGet::Get) => {
                let n = self.memory.ld8(nn);
                self.registers.write8n(RegisterKind8::A, n);
            }
            AOpNnInd(nn, PutGet::Put) => {
                let n = self.registers.a;
                self.memory.st8(nn, n.0);
            }
            NnIndGetsSp(nn) => {
                let n = self.registers.sp;
                self.memory.st8(nn, n.lo().0);
                self.memory.st8(nn.offset(1, Direction::Pos), n.hi().0);
            }
            AOpIOOffset(offset_by, put_get) => {
                let offset = match offset_by {
                    OffsetBy::N(n) => n,
                    OffsetBy::C => self.registers.read8(RegisterKind8::C).0,
                };
                let addr = Addr::io_memory().offset(u16::from(offset), Direction::Pos);
                match put_get {
                    PutGet::Get => {
                        let n = self.memory.ld8(addr);
                        self.registers.write8n(RegisterKind8::A, n)
                    }
                    PutGet::Put => {
                        let n = self.registers.read8(RegisterKind8::A);
                        self.memory.st8(addr, n.0);
                    }
                }
            }
            AOpHlInd(dir, put_get) => {
                match put_get {
                    PutGet::Get => {
                        let (n, _) = self.indirect_ld(RegisterKind16::Hl);
                        self.registers.write8n(RegisterKind8::A, n);
                    }
                    PutGet::Put => {
                        let n = self.registers.read8(RegisterKind8::A);
                        self.indirect_st(RegisterKind16::Hl, n.0);
                    }
                }

                match dir {
                    Direction::Pos => self.registers.hl.inc(),
                    Direction::Neg => self.registers.hl.dec(),
                }
            }
            DwordGetsAddr(r16, addr) => self.registers.write16r(r16, addr.into_register()),
            SpGetsHl => self.registers.sp = self.registers.hl,
            HlGetsSpOffset(r8) => {
                self.registers.hl = Addr::indirectly(self.registers.sp)
                    .offset_signed(r8)
                    .into_register()
            }
        };
        BranchAction::Take
    }

    fn execute_alu_binop<F>(&mut self, f: F, operand: u8) -> u8
    where
        F: FnOnce(&mut Flags, u8, u8) -> u8,
    {
        let old_a = self.registers.a.0;
        let result = f(&mut self.registers.flags, old_a, operand);
        self.registers.write8n(RegisterKind8::A, result);
        result
    }

    fn operand_regshln(&mut self, x: RegsHlN) -> u8 {
        match x {
            RegsHlN::Reg(r) => self.registers.read8(r).0,
            RegsHlN::HlInd => {
                let (operand, _) = self.indirect_ld(RegisterKind16::Hl);
                operand
            }
            RegsHlN::N(n) => n,
        }
    }

    fn operand_regshl(&mut self, x: RegsHl) -> u8 {
        match x {
            RegsHl::Reg(r) => self.registers.read8(r).0,
            RegsHl::HlInd => {
                let (operand, _) = self.indirect_ld(RegisterKind16::Hl);
                operand
            }
        }
    }

    fn handle_alu_regshln<F>(&mut self, f: F, x: RegsHlN)
    where
        F: FnOnce(&mut Flags, u8, u8) -> u8,
    {
        let operand = self.operand_regshln(x);
        self.execute_alu_binop(f, operand);
    }
    fn handle_alu_regshl<F>(&mut self, f: F, x: RegsHl)
    where
        F: FnOnce(&mut Flags, u8, u8) -> u8,
    {
        let operand = self.operand_regshl(x);
        self.execute_alu_binop(f, operand);
    }

    fn execute_arith(&mut self, arith: Arith) -> BranchAction {
        use self::Arith::*;

        match arith {
            Add(x) => self.handle_alu_regshln(alu::add, x),
            Adc(x) => self.handle_alu_regshln(alu::adc, x),
            Sub(x) => self.handle_alu_regshln(alu::sub, x),
            Sbc(x) => self.handle_alu_regshln(alu::sbc, x),
            And(x) => self.handle_alu_regshln(alu::and, x),
            Or(x) => self.handle_alu_regshln(alu::or, x),
            Xor(x) => self.handle_alu_regshln(alu::xor, x),
            Cp(x) => {
                let old_a = self.registers.a.0;
                let operand = self.operand_regshln(x);
                let _ = alu::sub(&mut self.registers.flags, old_a, operand);
            }
            Inc(RegsHl::Reg(r)) => {
                let operand = self.registers.read8(r).0;
                let result = alu::inc(&mut self.registers.flags, operand);
                self.registers.write8n(r, result);
            }
            Inc(RegsHl::HlInd) => {
                let operand = self.indirect_ld(RegisterKind16::Hl);
                let result = alu::inc(&mut self.registers.flags, operand.0);
                self.indirect_st(RegisterKind16::Hl, result);
            }
            Dec(RegsHl::Reg(r)) => {
                let operand = self.registers.read8(r).0;
                let result = alu::inc(&mut self.registers.flags, operand);
                self.registers.write8n(r, result);
            }
            Dec(RegsHl::HlInd) => {
                let operand = self.indirect_ld(RegisterKind16::Hl);
                let result = alu::dec(&mut self.registers.flags, operand.0);
                self.indirect_st(RegisterKind16::Hl, result);
            }
            AddHl(r) => {
                let old_hl = self.registers.hl.0;
                let operand = self.registers.read16(r).0;
                let result = alu::addhl(&mut self.registers.flags, old_hl, operand);
                self.registers.write16n(RegisterKind16::Hl, result);
            }
            AddSp(operand) => {
                let old_sp = self.registers.sp.0;
                let result = alu::addsp(&mut self.registers.flags, old_sp, operand);
                self.registers.write16n(RegisterKind16::Sp, result);
            }
            Inc16(r16) => {
                let operand = self.registers.read16(r16);
                let result = alu::inc16(&mut self.registers.flags, operand.0);
                self.registers.write16n(r16, result);
            }
            Dec16(r16) => {
                let operand = self.registers.read16(r16);
                let result = alu::dec16(&mut self.registers.flags, operand.0);
                self.registers.write16n(r16, result);
            }
            Cpl => {
                let result = alu::complement(&mut self.registers.flags, self.registers.a.0);
                self.registers.write8n(RegisterKind8::A, result);
            }
            Swap(RegsHl::Reg(r)) => {
                let operand = self.registers.read8(r).0;
                let result = alu::swap_nibbles(&mut self.registers.flags, operand);
                self.registers.write8n(r, result);
            }
            Swap(RegsHl::HlInd) => {
                let operand = self.indirect_ld(RegisterKind16::Hl);
                let result = alu::swap_nibbles(&mut self.registers.flags, operand.0);
                self.indirect_st(RegisterKind16::Hl, result);
            }
            Sla(RegsHl::Reg(r)) => {
                let operand = self.registers.read8(r).0;
                let result = alu::shift_left_carry(&mut self.registers.flags, operand);
                self.registers.write8n(r, result);
            }
            Sla(RegsHl::HlInd) => {
                let operand = self.indirect_ld(RegisterKind16::Hl);
                let result = alu::shift_left_carry(&mut self.registers.flags, operand.0);
                self.indirect_st(RegisterKind16::Hl, result);
            }
            Srl(RegsHl::Reg(r)) => {
                let operand = self.registers.read8(r).0;
                let result = alu::shift_right_carry(&mut self.registers.flags, operand);
                self.registers.write8n(r, result);
            }
            Srl(RegsHl::HlInd) => {
                let operand = self.indirect_ld(RegisterKind16::Hl);
                let result = alu::shift_right_carry(&mut self.registers.flags, operand.0);
                self.indirect_st(RegisterKind16::Hl, result);
            }
        };
        BranchAction::Take
    }

    fn execute_rotate(&mut self, rotate: Rotate) -> BranchAction {
        use self::Rotate::*;

        match rotate {
            Rla => {
                let n = self.registers.read8(RegisterKind8::A);
                let result = alu::rl(&mut self.registers.flags, n.0);
                self.registers.flags.z = false;
                self.registers.flags.n = false;
                self.registers.flags.h = false;
                self.registers.write8n(RegisterKind8::A, result);
            }
            Rlca => {
                let n = self.registers.read8(RegisterKind8::A);
                let result = alu::rlc(&mut self.registers.flags, n.0);
                self.registers.write8n(RegisterKind8::A, result);
            }
            Rra => {
                let n = self.registers.read8(RegisterKind8::A);
                let result = alu::rr(&mut self.registers.flags, n.0);
                self.registers.flags.z = false;
                self.registers.flags.n = false;
                self.registers.flags.h = false;
                self.registers.write8n(RegisterKind8::A, result);
            }
            Rl(RegsHl::Reg(r)) => {
                let n = self.registers.read8(r);
                let result = alu::rl(&mut self.registers.flags, n.0);
                self.registers.write8n(r, result);
            }
            Rl(RegsHl::HlInd) => {
                let n = self.indirect_ld(RegisterKind16::Hl);
                let result = alu::rl(&mut self.registers.flags, n.0);
                self.indirect_st(RegisterKind16::Hl, result);
            }
            Rlc(RegsHl::Reg(r)) => {
                let n = self.registers.read8(r);
                let result = alu::rlc(&mut self.registers.flags, n.0);
                self.registers.write8n(r, result);
            }
            Rlc(RegsHl::HlInd) => {
                let n = self.indirect_ld(RegisterKind16::Hl);
                let result = alu::rlc(&mut self.registers.flags, n.0);
                self.indirect_st(RegisterKind16::Hl, result);
            }
            Rr(RegsHl::Reg(r)) => {
                let n = self.registers.read8(r);
                let result = alu::rr(&mut self.registers.flags, n.0);
                self.registers.write8n(r, result);
            }
            Rr(RegsHl::HlInd) => {
                let n = self.indirect_ld(RegisterKind16::Hl);
                let result = alu::rr(&mut self.registers.flags, n.0);
                self.indirect_st(RegisterKind16::Hl, result);
            }
        };
        BranchAction::Take
    }

    fn pop(&mut self) -> R8 {
        let (v, _) = self.indirect_ld(RegisterKind16::Sp);
        self.registers.sp.inc();
        R8(v)
    }

    fn pop16(&mut self) -> R16 {
        let lo = self.pop();
        let hi = self.pop();
        hi.concat(lo)
    }

    fn push16(&mut self, n: R16) {
        self.registers.sp.dec();
        self.indirect_st(RegisterKind16::Sp, n.hi().0);
        self.registers.sp.dec();
        self.indirect_st(RegisterKind16::Sp, n.lo().0);
    }
}

// for tests
impl Cpu {
    // TODO: Accept parsed instrs instead of just raw bytes
    fn inject_instrs_update_ip(&mut self, raw: Vec<u8>) {
        self.ip.jump(Addr::directly(0xc000));
        raw.iter()
            .enumerate()
            .for_each(|(i, n)| self.memory.st8(Addr::directly(0xc000 + i as u16), *n))
    }
}

#[cfg(test)]
mod push_pop_tests {
    use cpu::Cpu;
    use mem::BOOTROM;
    use register::Flags;
    use register::{R16, R8};
    use std::borrow::Cow;
    use test::proptest::prelude::*;
    use these::These;

    proptest! {
        #[test]
        fn push_pop_self_inverse(x : u16) {
            let mut cpu = Cpu::create(These::This(Cow::Borrowed(BOOTROM)));
            cpu.registers.sp = R16(0xff90);

            let r16 = R16(x);
            cpu.push16(r16);
            let res = cpu.pop16();
            assert_eq!(res, r16);
        }

        #[test]
        fn push_pop_af_self_inverse_cpu(a: R8, flags: Flags) {
            let mut cpu = Cpu::create(These::This(Cow::Borrowed(BOOTROM)));
            cpu.registers.sp = R16(0xff90);
            cpu.registers.a = a;
            cpu.registers.flags = flags.clone();
            // PushAf, PopAf
            cpu.inject_instrs_update_ip(vec![0xf5, 0xf1]);

            cpu.execute();
            cpu.execute();

            assert_eq!(cpu.registers.a, a, "The A register");
            assert_eq!(cpu.registers.flags, flags, "The flags");
        }
    }
}

pub enum InterruptKind {
    Vblank,
    LcdStat,
    Timer,
    Serial,
    Joypad,
}

impl Cpu {
    fn do_call(&mut self, addr: Addr) {
        let r16 = self.ip.0.into_register();
        self.push16(r16);
        self.ip.jump(addr);
    }

    fn execute_jump(&mut self, jump: Jump) -> BranchAction {
        use self::Jump::*;

        match jump {
            Jp(addr) => {
                self.ip.jump(addr);
                BranchAction::Take
            }
            JpCc(cond, addr) => match cond {
                RetCondition::Nz => {
                    if !self.registers.flags.z {
                        self.ip.jump(addr);
                        BranchAction::Take
                    } else {
                        BranchAction::Skip
                    }
                }
                RetCondition::Z => {
                    if self.registers.flags.z {
                        self.ip.jump(addr);
                        BranchAction::Take
                    } else {
                        BranchAction::Skip
                    }
                }
                RetCondition::Nc => {
                    if !self.registers.flags.c {
                        self.ip.jump(addr);
                        BranchAction::Take
                    } else {
                        BranchAction::Skip
                    }
                }
                RetCondition::C => {
                    if self.registers.flags.c {
                        self.ip.jump(addr);
                        BranchAction::Take
                    } else {
                        BranchAction::Skip
                    }
                }
            },
            JpHlInd => {
                let operand = self.registers.read16(RegisterKind16::Hl);
                self.ip.jump(Addr::indirectly(operand));
                BranchAction::Take
            }
            Jr(offset) => {
                self.ip.offset_by(offset);
                BranchAction::Take
            }
            JrCc(cond, offset) => match cond {
                RetCondition::Nz => {
                    if !self.registers.flags.z {
                        self.ip.offset_by(offset);
                        BranchAction::Take
                    } else {
                        BranchAction::Skip
                    }
                }
                RetCondition::Z => {
                    if self.registers.flags.z {
                        self.ip.offset_by(offset);
                        BranchAction::Take
                    } else {
                        BranchAction::Skip
                    }
                }
                RetCondition::Nc => {
                    if !self.registers.flags.c {
                        self.ip.offset_by(offset);
                        BranchAction::Take
                    } else {
                        BranchAction::Skip
                    }
                }
                RetCondition::C => {
                    if self.registers.flags.c {
                        self.ip.offset_by(offset);
                        BranchAction::Take
                    } else {
                        BranchAction::Skip
                    }
                }
            },
            Call(addr) => {
                self.do_call(addr);
                BranchAction::Take
            }
            CallCc(cond, addr) => match cond {
                RetCondition::Nz => {
                    if !self.registers.flags.z {
                        self.do_call(addr);
                        BranchAction::Take
                    } else {
                        BranchAction::Skip
                    }
                }
                RetCondition::Z => {
                    if self.registers.flags.z {
                        self.do_call(addr);
                        BranchAction::Take
                    } else {
                        BranchAction::Skip
                    }
                }
                RetCondition::Nc => {
                    if !self.registers.flags.c {
                        self.do_call(addr);
                        BranchAction::Take
                    } else {
                        BranchAction::Skip
                    }
                }
                RetCondition::C => {
                    if self.registers.flags.c {
                        self.do_call(addr);
                        BranchAction::Take
                    } else {
                        BranchAction::Skip
                    }
                }
            },
            Rst(n) => {
                self.do_call(Addr::directly(u16::from(n)));
                BranchAction::Take
            }
        }
    }

    fn execute_bits(&mut self, bits: Bits) -> BranchAction {
        use self::Bits::*;

        match bits {
            Bit(b, RegsHl::Reg(reg)) => {
                let operand = self.registers.read8(reg);
                alu::bit(&mut self.registers.flags, operand.0, b);
                BranchAction::Take
            }
            Bit(b, RegsHl::HlInd) => {
                let operand = self.indirect_ld(RegisterKind16::Hl);
                alu::bit(&mut self.registers.flags, operand.0, b);
                BranchAction::Take
            }
            Res(b, RegsHl::Reg(reg)) => {
                let operand = self.registers.read8(reg);
                let result = alu::reset_bit(operand.0, b);
                self.registers.write8n(reg, result);
                BranchAction::Take
            }
            Res(b, RegsHl::HlInd) => {
                let operand = self.indirect_ld(RegisterKind16::Hl);
                let result = alu::reset_bit(operand.0, b);
                self.indirect_st(RegisterKind16::Hl, result);
                BranchAction::Take
            }
            Set(b, RegsHl::Reg(reg)) => {
                let operand = self.registers.read8(reg);
                let result = alu::set_bit(operand.0, b);
                self.registers.write8n(reg, result);
                BranchAction::Take
            }
            Set(b, RegsHl::HlInd) => {
                let operand = self.indirect_ld(RegisterKind16::Hl);
                let result = alu::set_bit(operand.0, b);
                self.indirect_st(RegisterKind16::Hl, result);
                BranchAction::Take
            }
        }
    }

    fn do_ret(&mut self) -> BranchAction {
        let reg = self.pop16();
        self.ip.jump(Addr::directly(reg.0));
        BranchAction::Take
    }

    fn execute_instr(&mut self, instr: Instr) -> BranchAction {
        use self::Instr::*;

        match instr {
            Nop => BranchAction::Take,
            Ld(ld) => self.execute_ld(ld),
            Arith(arith) => self.execute_arith(arith),
            Rotate(rotate) => self.execute_rotate(rotate),
            Jump(jump) => self.execute_jump(jump),
            Bits(bits) => self.execute_bits(bits),
            CpHlInd => {
                let x = self.registers.read8(RegisterKind8::A);
                let (operand, _) = self.indirect_ld(RegisterKind16::Hl);
                let _ = alu::sub(&mut self.registers.flags, x.0, operand);
                BranchAction::Take
            }
            Pop(reg) => {
                let new16 = self.pop16();
                self.registers.write16r(reg, new16);
                BranchAction::Take
            }
            PopAf => {
                let new16 = self.pop16();
                self.registers.a = new16.hi();
                let low = new16.lo().0;
                self.registers.flags.z = (low & 0b1000_0000) > 0;
                self.registers.flags.n = (low & 0b0100_0000) > 0;
                self.registers.flags.h = (low & 0b0010_0000) > 0;
                self.registers.flags.c = (low & 0b0001_0000) > 0;
                BranchAction::Take
            }
            Push(reg) => {
                let operand = self.registers.read16(reg);
                self.push16(operand);
                BranchAction::Take
            }
            PushAf => {
                let a = self.registers.a;

                let btoi = |b| if b { 1 } else { 0 };

                let f = R8((btoi(self.registers.flags.z) << 7)
                    | (btoi(self.registers.flags.n) << 6)
                    | (btoi(self.registers.flags.h) << 5)
                    | (btoi(self.registers.flags.c) << 4));

                let mut r16 = R16(0);
                r16.set_hi(a);
                r16.set_lo(f);
                self.push16(r16);
                BranchAction::Take
            }
            Ret => self.do_ret(),
            Reti => {
                self.interrupt_master_enable = true;
                self.do_ret()
            }
            RetCc(cond) => match cond {
                RetCondition::Nz => {
                    if !self.registers.flags.z {
                        self.do_ret();
                        BranchAction::Take
                    } else {
                        BranchAction::Skip
                    }
                }
                RetCondition::Z => {
                    if self.registers.flags.z {
                        self.do_ret();
                        BranchAction::Take
                    } else {
                        BranchAction::Skip
                    }
                }
                RetCondition::Nc => {
                    if !self.registers.flags.c {
                        self.do_ret();
                        BranchAction::Take
                    } else {
                        BranchAction::Skip
                    }
                }
                RetCondition::C => {
                    if self.registers.flags.c {
                        self.do_ret();
                        BranchAction::Take
                    } else {
                        BranchAction::Skip
                    }
                }
            },
            InvalidOp(_) => BranchAction::Take,
            Di => {
                self.interrupt_master_enable = false;
                BranchAction::Take
            }
            Ei => {
                self.interrupt_master_enable = true;
                BranchAction::Take
            }
            Scf => {
                self.registers.flags.n = false;
                self.registers.flags.h = false;
                self.registers.flags.c = true;
                BranchAction::Take
            }
            Ccf => {
                self.registers.flags.n = false;
                self.registers.flags.h = false;
                self.registers.flags.c = !self.registers.flags.c;
                BranchAction::Take
            }
            Daa => {
                let x = self.registers.read8(RegisterKind8::A);
                let result = alu::daa(&mut self.registers.flags, x.0);
                self.registers.write8n(RegisterKind8::A, result);
                BranchAction::Take
            }
            Halt => panic!("Halt instruction unimplemented in CPU"),
        }
    }

    /// Peek at the next instruction to see how long it will take
    pub fn peek_next(&mut self) -> u32 {
        //use web_utils::log;
        let instr = self.ip.peek(&self.memory);
        //log(&format!("{:}", instr));
        // Assumption: Take duration is longer than skip duration (this seems to be true for all
        // Gameboy instructions
        let (take_duration, _) = instr.duration();
        take_duration
    }

    /// Execute the current instruction returning the duration it did take. Note: This can be
    /// different from the peeked duration as time taken differs based on branch takes or skips.
    pub fn execute(&mut self) -> u32 {
        let instr = self.ip.read(&self.memory);
        let (take_duration, skip_duration) = instr.duration();
        let action = self.execute_instr(instr);
        match action {
            BranchAction::Take => take_duration,
            BranchAction::Skip => skip_duration.unwrap_or_else(|| take_duration),
        }
    }

    /// Attempt an interrupt of a certain kind
    /// Returns None if the interrupt is not enabled
    /// Returns Some(addr to call) otherwise
    fn attempt_interrupt_(&self, kind: InterruptKind) -> Option<Addr> {
        if self.interrupt_master_enable {
            let ie = &self.memory.interrupt_enable;
            match kind {
                InterruptKind::Vblank => {
                    if ie.vblank {
                        // use web_utils::log;
                        // log("Triggering vblank");
                        Some(Addr::directly(0x40))
                    } else {
                        None
                    }
                }
                InterruptKind::LcdStat => {
                    if ie.lcd_stat {
                        Some(Addr::directly(0x48))
                    } else {
                        None
                    }
                }
                InterruptKind::Timer => {
                    if ie.timer {
                        Some(Addr::directly(0x50))
                    } else {
                        None
                    }
                }
                InterruptKind::Serial => {
                    if ie.serial {
                        Some(Addr::directly(0x58))
                    } else {
                        None
                    }
                }
                InterruptKind::Joypad => {
                    if ie.joypad {
                        Some(Addr::directly(0x58))
                    } else {
                        None
                    }
                }
            }
        } else {
            None
        }
    }

    /// Attempts to fire an interrupt; it can be ignored
    /// If it fires, interrupts are disabled and we push ip + jump to the addr
    pub fn attempt_interrupt(&mut self, kind: InterruptKind) {
        match self.attempt_interrupt_(kind) {
            None => (),
            Some(addr) => {
                self.interrupt_master_enable = false;
                self.do_call(addr);
            }
        }
    }
}

// Tests each instruction against the Gekkio specs
// https://gekkio.fi/files/gb-docs/gbctr.pdf (pg. 8)
// ALU info is missing from these docs. In those cases, the spec is taken from
// the official Gameboy manual
// https://ia801906.us.archive.org/19/items/GameBoyProgManVer1.1/GameBoyProgManVer1.1.pdf
//
// At first glance these tests seem redundant w.r.t. the implementation
// However, see https://bkase.dev/posts/gameboy-debugging-parable
// I'd much rather duplicate myself than run into one of these again
//
// Think of it as a form of double-accounting to catch any other dumb mistakes
#[cfg(test)]
mod cpu_to_spec {
    use cpu::Cpu;
    use instr::{
        Arith, Bits, HasDuration, Instr, InstrPointer, Jump, Ld, OffsetBy, PutGet, RegsHl, RegsHlN,
        RetCondition, Rotate,
    };
    use mem::{Addr, Direction};
    use register::{Flags, Registers, R16, R8};
    use register_kind::{RegisterKind16, RegisterKind8};
    use test::proptest::prelude::*;

    struct InstrSpec<F: Fn(&mut Cpu) -> ()> {
        duration: (u32, Option<u32>),
        run: F,
    }

    impl<F: Fn(&mut Cpu) -> ()> InstrSpec<F> {
        fn validate(&self, regs: Registers, instr: Instr) {
            assert_eq!(instr.duration(), self.duration);

            let mut cpu1 = Cpu::create_with_registers(regs.clone());
            cpu1.execute_instr(instr);

            let mut cpu2 = Cpu::create_with_registers(regs.clone());
            (self.run)(&mut cpu2);

            assert_eq!(cpu1, cpu2);
        }
    }

    proptest! {
        #[test]
        fn ld_r_r_(regs: Registers) {
            let rs = [RegisterKind8::B, RegisterKind8::C, RegisterKind8::D, RegisterKind8::H, RegisterKind8::L, RegisterKind8::A];

            rs.iter().for_each(|r1| {
                rs.iter().for_each(|r2| {
                    let spec = InstrSpec {
                        duration: (1, None),
                        run: |cpu| {
                            let old_r2 = cpu.registers.read8(*r2);
                            cpu.registers.write8r(*r1, old_r2);
                        }
                    };
                    let instr = Instr::Ld(Ld::Word(RegsHl::Reg(*r1), RegsHlN::Reg(*r2)));
                    spec.validate(regs.clone(), instr);
                });
            });
        }

        #[test]
        fn ld_r_n(regs: Registers, n: u8) {
            let rs = [RegisterKind8::B, RegisterKind8::C, RegisterKind8::D, RegisterKind8::H, RegisterKind8::L, RegisterKind8::A];

            rs.iter().for_each(|r1| {
                let spec = InstrSpec {
                    duration: (2, None),
                    run: |cpu| {
                        cpu.registers.write8n(*r1, n)
                    }
                };
                let instr = Instr::Ld(Ld::Word(RegsHl::Reg(*r1), RegsHlN::N(n)));
                spec.validate(regs.clone(), instr);
            });
        }

        #[test]
        fn ld_r_hl(regs: Registers) {
            let rs = [RegisterKind8::B, RegisterKind8::C, RegisterKind8::D, RegisterKind8::H, RegisterKind8::L, RegisterKind8::A];

            rs.iter().for_each(|r1| {
                let spec = InstrSpec {
                    duration: (2, None),
                    run: |cpu| {
                        let (n, _) = cpu.indirect_ld(RegisterKind16::Hl);
                        cpu.registers.write8n(*r1, n);
                    }
                };
                let instr = Instr::Ld(Ld::Word(RegsHl::Reg(*r1), RegsHlN::HlInd));
                spec.validate(regs.clone(), instr);
            });
        }

        #[test]
        fn ld_hl_r(regs: Registers) {
            let rs = [RegisterKind8::B, RegisterKind8::C, RegisterKind8::D, RegisterKind8::H, RegisterKind8::L, RegisterKind8::A];

            rs.iter().for_each(|r1| {
                let spec = InstrSpec {
                    duration: (2, None),
                    run: |cpu| {
                        let r1 = cpu.registers.read8(*r1);
                        cpu.indirect_st(RegisterKind16::Hl, r1.0)
                    }
                };
                let instr = Instr::Ld(Ld::Word(RegsHl::HlInd, RegsHlN::Reg(*r1)));
                spec.validate(regs.clone(), instr);
            });
        }

        #[test]
        fn ld_hl_n(regs: Registers, n: u8) {
            let spec = InstrSpec {
                duration: (3, None),
                run: |cpu| {
                    cpu.indirect_st(RegisterKind16::Hl, n)
                }
            };
            let instr = Instr::Ld(Ld::Word(RegsHl::HlInd, RegsHlN::N(n)));
            spec.validate(regs.clone(), instr);
        }

        #[test]
        fn ld_a_bc(regs: Registers) {
            let spec = InstrSpec {
                duration: (2, None),
                run: |cpu| {
                    let (n, _) = cpu.indirect_ld(RegisterKind16::Bc);
                    cpu.registers.write8n(RegisterKind8::A, n);
                }
            };
            let instr = Instr::Ld(Ld::AOpInd(RegisterKind16::Bc, PutGet::Get));
            spec.validate(regs.clone(), instr);
        }

        #[test]
        fn ld_a_de(regs: Registers) {
            let spec = InstrSpec {
                duration: (2, None),
                run: |cpu| {
                    let (n, _) = cpu.indirect_ld(RegisterKind16::De);
                    cpu.registers.write8n(RegisterKind8::A, n);
                }
            };
            let instr = Instr::Ld(Ld::AOpInd(RegisterKind16::De, PutGet::Get));
            spec.validate(regs.clone(), instr);
        }

        #[test]
        fn ld_bc_a(regs: Registers) {
            let spec = InstrSpec {
                duration: (2, None),
                run: |cpu| {
                    let r = cpu.registers.read8(RegisterKind8::A);
                    cpu.indirect_st(RegisterKind16::Bc, r.0);
                }
            };
            let instr = Instr::Ld(Ld::AOpInd(RegisterKind16::Bc, PutGet::Put));
            spec.validate(regs.clone(), instr);
        }

        #[test]
        fn ld_de_a(regs: Registers) {
            let spec = InstrSpec {
                duration: (2, None),
                run: |cpu| {
                    let r = cpu.registers.read8(RegisterKind8::A);
                    cpu.indirect_st(RegisterKind16::De, r.0);
                }
            };
            let instr = Instr::Ld(Ld::AOpInd(RegisterKind16::De, PutGet::Put));
            spec.validate(regs.clone(), instr);
        }

        #[test]
        fn ld_a_nn(regs: Registers, addr: Addr) {
            let spec = InstrSpec {
                duration: (4, None),
                run: |cpu| {
                    let n = cpu.memory.ld8(addr);
                    cpu.registers.write8n(RegisterKind8::A, n);
                }
            };
            let instr = Instr::Ld(Ld::AOpNnInd(addr, PutGet::Get));
            spec.validate(regs.clone(), instr);
        }

        #[test]
        fn ldh_a_c(regs: Registers) {
            let spec = InstrSpec {
                duration: (2, None),
                run: |cpu| {
                    let r = cpu.registers.read8(RegisterKind8::C);
                    let n = cpu.memory.ld8(Addr::directly(0xFF00 + u16::from(r.0)));
                    cpu.registers.write8n(RegisterKind8::A, n);
                }
            };
            let instr = Instr::Ld(Ld::AOpIOOffset(OffsetBy::C, PutGet::Get));
            spec.validate(regs.clone(), instr);
        }

        #[test]
        fn ldh_c_a(regs: Registers) {
            let spec = InstrSpec {
                duration: (2, None),
                run: |cpu| {
                    let r = cpu.registers.read8(RegisterKind8::C);
                    let a = cpu.registers.read8(RegisterKind8::A);
                    cpu.memory.st8(Addr::directly(0xFF00 + u16::from(r.0)), a.0);
                }
            };
            let instr = Instr::Ld(Ld::AOpIOOffset(OffsetBy::C, PutGet::Put));
            spec.validate(regs.clone(), instr);
        }

        #[test]
        fn ldh_a_n(regs: Registers, n: u8) {
            let spec = InstrSpec {
                duration: (3, None),
                run: |cpu| {
                    let n = cpu.memory.ld8(Addr::directly(0xFF00 + u16::from(n)));
                    cpu.registers.write8n(RegisterKind8::A, n);
                }
            };
            let instr = Instr::Ld(Ld::AOpIOOffset(OffsetBy::N(n), PutGet::Get));
            spec.validate(regs.clone(), instr);
        }

        #[test]
        fn ldh_n_a(regs: Registers, n: u8) {
            let spec = InstrSpec {
                duration: (3, None),
                run: |cpu| {
                    let a = cpu.registers.read8(RegisterKind8::A);
                    cpu.memory.st8(Addr::directly(0xFF00 + u16::from(n)), a.0);
                }
            };
            let instr = Instr::Ld(Ld::AOpIOOffset(OffsetBy::N(n), PutGet::Put));
            spec.validate(regs.clone(), instr);
        }

        #[test]
        fn ld_a_hl_minus(regs: Registers) {
            let spec = InstrSpec {
                duration: (2, None),
                run: |cpu| {
                    let (n, addr) = cpu.indirect_ld(RegisterKind16::Hl);
                    cpu.registers.write8n(RegisterKind8::A, n);
                    cpu.registers.write16r(RegisterKind16::Hl, addr.offset_signed(-1).into_register());
                }
            };
            let instr = Instr::Ld(Ld::AOpHlInd(Direction::Neg, PutGet::Get));
            spec.validate(regs.clone(), instr);
        }

        #[test]
        fn ld_hl_minus_a(regs: Registers) {
            let spec = InstrSpec {
                duration: (2, None),
                run: |cpu| {
                    let a = cpu.registers.read8(RegisterKind8::A);
                    cpu.indirect_st(RegisterKind16::Hl, a.0);
                    let hl = cpu.registers.read16(RegisterKind16::Hl);
                    cpu.registers.write16n(RegisterKind16::Hl, hl.0.wrapping_sub(1));
                }
            };
            let instr = Instr::Ld(Ld::AOpHlInd(Direction::Neg, PutGet::Put));
            spec.validate(regs.clone(), instr);
        }

        #[test]
        fn ld_a_hl_plus(regs: Registers) {
            let spec = InstrSpec {
                duration: (2, None),
                run: |cpu| {
                    let (n, addr) = cpu.indirect_ld(RegisterKind16::Hl);
                    cpu.registers.write8n(RegisterKind8::A, n);
                    cpu.registers.write16r(RegisterKind16::Hl, addr.offset_signed(1).into_register());
                }
            };
            let instr = Instr::Ld(Ld::AOpHlInd(Direction::Pos, PutGet::Get));
            spec.validate(regs.clone(), instr);
        }

        #[test]
        fn ld_hl_plus_a(regs: Registers) {
            let spec = InstrSpec {
                duration: (2, None),
                run: |cpu| {
                    let a = cpu.registers.read8(RegisterKind8::A);
                    cpu.indirect_st(RegisterKind16::Hl, a.0);
                    let hl = cpu.registers.read16(RegisterKind16::Hl);
                    cpu.registers.write16n(RegisterKind16::Hl, hl.0.wrapping_add(1));
                }
            };
            let instr = Instr::Ld(Ld::AOpHlInd(Direction::Pos, PutGet::Put));
            spec.validate(regs.clone(), instr);
        }

        #[test]
        fn ld_rr_nn(regs: Registers, nn: u16) {
            let rs = [RegisterKind16::Bc, RegisterKind16::De, RegisterKind16::Hl, RegisterKind16::Sp];

            rs.iter().for_each(|rr| {
                let spec = InstrSpec {
                    duration: (3, None),
                    run: |cpu| {
                        cpu.registers.write16n(*rr, nn);
                    }
                };
                let instr = Instr::Ld(Ld::DwordGetsAddr(*rr, Addr::directly(nn)));
                spec.validate(regs.clone(), instr);
            });
        }

        #[test]
        fn ld_nn_ind_sp(regs: Registers, nn: u16) {
            let spec = InstrSpec {
                duration: (5, None),
                run: |cpu| {
                    let addr = Addr::directly(nn);
                    let sp = cpu.registers.read16(RegisterKind16::Sp);
                    cpu.memory.st8(addr, (sp.0 & 0xff) as u8);
                    cpu.memory.st8(addr.offset_signed(1), ((sp.0 & 0xff00) >> 8) as u8);
                }
            };
            let instr = Instr::Ld(Ld::NnIndGetsSp(Addr::directly(nn)));
            spec.validate(regs.clone(), instr);
        }

        #[test]
        fn ld_sp_hl(regs: Registers) {
            let spec = InstrSpec {
                duration: (2, None),
                run: |cpu| {
                    let hl = cpu.registers.read16(RegisterKind16::Hl);
                    cpu.registers.write16r(RegisterKind16::Sp, hl);
                }
            };
            let instr = Instr::Ld(Ld::SpGetsHl);
            spec.validate(regs.clone(), instr);
        }

        #[test]
        fn push_rr(regs: Registers) {
            let rs = [Some(RegisterKind16::Bc), Some(RegisterKind16::De), Some(RegisterKind16::Hl), None];

            let f = |b| if b { 1 } else { 0 };

            rs.iter().for_each(|rr| {
                let spec = InstrSpec {
                    duration: (4, None),
                    run: |cpu| {
                        let rr =
                            match rr {
                                Some(k) => cpu.registers.read16(*k).0,
                                None => {
                                    let flags = &cpu.registers.flags;
                                    (u16::from(cpu.registers.a.0) << 8) |
                                        (f(flags.z) << 7) |
                                        (f(flags.n) << 6) |
                                        (f(flags.h) << 5) |
                                        (f(flags.c) << 4)
                                }
                            };

                        let sp = cpu.registers.read16(RegisterKind16::Sp);
                        let sp1 = sp.0.wrapping_sub(1);

                        cpu.memory.st8(Addr::directly(sp1), ((rr & 0xff00) >> 8) as u8);
                        let sp2 = sp1.wrapping_sub(1);

                        cpu.memory.st8(Addr::directly(sp2), (rr & 0xff) as u8);

                        cpu.registers.write16n(RegisterKind16::Sp, sp2);
                    }
                };
                let instr = match rr {
                    Some(k) => Instr::Push(*k),
                    None => Instr::PushAf
                };
                spec.validate(regs.clone(), instr);
            });
        }

        #[test]
        fn pop_rr(regs: Registers) {
            let rs = [
                Some(RegisterKind16::Bc),
                Some(RegisterKind16::De),
                Some(RegisterKind16::Hl),
                None,
            ];

            rs.iter().for_each(|rr| {
                let spec = InstrSpec {
                    duration: (3, None),
                    run: |cpu| {
                        let sp = cpu.registers.read16(RegisterKind16::Sp);

                        let sp = sp.0;
                        let lo = cpu.memory.ld8(Addr::directly(sp));
                        let sp1 = sp.wrapping_add(1);

                        let hi = cpu.memory.ld8(Addr::directly(sp1));
                        let sp2 = sp1.wrapping_add(1);

                        match rr {
                            Some(k) => cpu.registers.write16n(*k, (u16::from(hi) << 8) | u16::from(lo)),
                            None => {
                                cpu.registers.a = R8(hi);
                                cpu.registers.flags = Flags {
                                    z: (lo & 0b1000_0000) > 0,
                                    n: (lo & 0b0100_0000) > 0,
                                    h: (lo & 0b0010_0000) > 0,
                                    c: (lo & 0b0001_0000) > 0,
                                }
                            }
                        };

                        cpu.registers.write16n(RegisterKind16::Sp, sp2);
                    },
                };
                let instr = match rr {
                    Some(k) => Instr::Pop(*k),
                    None => Instr::PopAf,
                };
                spec.validate(regs.clone(), instr);
            });
        }
    }
}
