#![allow(dead_code)]

use alu;
use instr::{
    Arith, Bits, HasDuration, Instr, InstrPointer, Jump, Ld, RegsHl, RegsHlN, RetCondition, Rotate,
};
use mem::{Addr, Cartridge, Direction, Memory};
use register::{Flags, Registers, R16, R8};
use register_kind::{RegisterKind16, RegisterKind8};

#[derive(Debug)]
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
    pub fn create(cartridge: Option<Cartridge>) -> Cpu {
        Cpu {
            registers: Registers::create(),
            memory: Memory::create(cartridge),
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

    fn execute_ld(&mut self, ld: Ld) -> BranchAction {
        use self::Ld::*;

        match ld {
            RGetsR(r1, r2) => {
                let n = self.registers.read8(r2);
                self.registers.write8r(r1, n);
            }
            RGetsN(r, n) => self.registers.write8n(r, n),
            RGetsHlInd(r) => {
                let (n, _) = self.indirect_ld(RegisterKind16::Hl);
                self.registers.write8n(r, n);
            }
            HlIndGetsR(r) => {
                let n = self.registers.read8(r);
                self.indirect_st(RegisterKind16::Hl, n.0)
            }
            HlIndGetsN(n) => self.indirect_st(RegisterKind16::Hl, n),
            AGetsBcInd => {
                let (n, _) = self.indirect_ld(RegisterKind16::Bc);
                self.registers.write8n(RegisterKind8::A, n);
            }
            AGetsDeInd => {
                let (n, _) = self.indirect_ld(RegisterKind16::De);
                self.registers.write8n(RegisterKind8::A, n);
            }
            AGetsNnInd(nn) => {
                let n = self.memory.ld8(nn);
                self.registers.write8n(RegisterKind8::A, n);
            }
            BcIndGetsA => {
                let n = self.registers.a;
                self.indirect_st(RegisterKind16::Bc, n.0)
            }
            DeIndGetsA => {
                let n = self.registers.a;
                self.indirect_st(RegisterKind16::De, n.0)
            }
            NnIndGetsA(nn) => {
                let n = self.registers.a;
                self.memory.st8(nn, n.0);
            }
            NnIndGetsSp(nn) => {
                let n = self.registers.sp;
                self.memory.st8(nn, n.lo().0);
                self.memory.st8(nn.offset(1, Direction::Pos), n.hi().0);
            }
            AGetsIOOffset(offset) => {
                let addr = Addr::io_memory().offset(u16::from(offset), Direction::Pos);
                let n = self.memory.ld8(addr);
                self.registers.write8n(RegisterKind8::A, n)
            }
            IOOffsetGetsA(offset) => {
                let addr = Addr::io_memory().offset(u16::from(offset), Direction::Pos);
                let n = self.registers.read8(RegisterKind8::A);
                self.memory.st8(addr, n.0);
            }
            AGetsIOOffsetByC => {
                let offset = self.registers.c();
                let addr = Addr::io_memory().offset(u16::from(offset.0), Direction::Pos);
                let n = self.memory.ld8(addr);
                self.registers.write8n(RegisterKind8::A, n)
            }
            IOOffsetByCGetsA => {
                let offset = self.registers.c();
                let addr = Addr::io_memory().offset(u16::from(offset.0), Direction::Pos);
                let n = self.registers.read8(RegisterKind8::A);
                self.memory.st8(addr, n.0);
            }
            HlIndGetsAInc => {
                let n = self.registers.read8(RegisterKind8::A);
                self.indirect_st(RegisterKind16::Hl, n.0);
                self.registers.hl.inc()
            }
            AGetsHlIndInc => {
                let (n, _) = self.indirect_ld(RegisterKind16::Hl);
                self.registers.write8n(RegisterKind8::A, n);
                self.registers.hl.inc()
            }
            HlIndGetsADec => {
                let n = self.registers.read8(RegisterKind8::A);
                self.indirect_st(RegisterKind16::Hl, n.0);
                self.registers.hl.dec()
            }
            AGetsHlIndDec => {
                let (n, _) = self.indirect_ld(RegisterKind16::Hl);
                self.registers.write8n(RegisterKind8::A, n);
                self.registers.hl.dec()
            }
            SpGetsAddr(addr) => {
                self.registers.sp = addr.into_register();
            }
            SpGetsHl => {
                self.registers.sp = self.registers.hl;
            }
            HlGetsAddr(addr) => {
                self.registers.hl = addr.into_register();
            }
            DeGetsAddr(addr) => {
                self.registers.de = addr.into_register();
            }
            BcGetsAddr(addr) => {
                self.registers.bc = addr.into_register();
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
            Sbc(x) => self.handle_alu_regshl(alu::sbc, x),
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
                let result = alu::dec(&mut self.registers.flags, operand);
                self.registers.write8n(r, result);
            }
            Dec(RegsHl::HlInd) => {
                let operand = self.indirect_ld(RegisterKind16::Hl);
                let result = alu::inc(&mut self.registers.flags, operand.0);
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
            Rla | Rlca => {
                // TODO: figure out how RLA and RLCA are different
                let n = self.registers.read8(RegisterKind8::A);
                let result = alu::rl(&mut self.registers.flags, n.0);
                self.registers.write8n(RegisterKind8::A, result);
            }
            Rra => {
                let n = self.registers.read8(RegisterKind8::A);
                let result = alu::rl(&mut self.registers.flags, n.0);
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

    fn push(&mut self, n: R8) {
        self.registers.sp.dec();
        self.indirect_st(RegisterKind16::Sp, n.0);
    }

    fn push16(&mut self, n: R16) {
        self.push(n.hi());
        self.push(n.lo());
    }
}

#[cfg(test)]
mod instr_tests {
    use cpu::Cpu;
    use register::R16;
    use test::proptest::prelude::*;

    proptest! {
        #[test]
        fn push_pop_self_inverse(x : u16) {
            let mut cpu = Cpu::create(None);
            cpu.registers.sp = R16(0xff90);

            let r16 = R16(x);
            cpu.push16(r16);
            let res = cpu.pop16();
            assert_eq!(res, r16);
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
                self.registers.flags.z = (low & 0b10000000) > 0;
                self.registers.flags.n = (low & 0b01000000) > 0;
                self.registers.flags.h = (low & 0b00100000) > 0;
                self.registers.flags.c = (low & 0b00010000) > 0;
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
            Daa => {
                let old_flags = self.registers.flags.clone();
                let x = self.registers.read8(RegisterKind8::A);
                let result = alu::daa(&mut self.registers.flags, x.0);
                self.registers.write8n(RegisterKind8::A, result);
                BranchAction::Take
            }
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
