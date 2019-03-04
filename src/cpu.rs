use register::{Registers, R16};
use register_kind::{RegisterKind16, RegisterKind8};
use instr::{Instr, HasDuration, InstrPointer, Ld, Arith, Rotate, Jump};
use mem::{Memory, Addr, Direction};

pub struct Cpu {
    registers : Registers,
    memory : Memory,
    ip : InstrPointer,
}

enum BranchAction {
    Take, Skip
}

impl Cpu {
    fn create() -> Cpu {
        Cpu {
            registers : Registers::create(),
            memory : Memory::create(),
            ip : InstrPointer::create(),
        }
    }

    fn indirect_ld(&mut self, k: RegisterKind16) -> u8 {
        let r = self.registers.read16(k);
        self.memory.ld8(Addr::indirectly(r))
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
            },
            RGetsN(r, n) => self.registers.write8n(r, n),
            RGetsHlInd(r) => {
                let n = self.indirect_ld(RegisterKind16::Hl);
                self.registers.write8n(r, n);
            },
            HlIndGetsR(r) => {
                let n = self.registers.read8(r);
                self.indirect_st(RegisterKind16::Hl, n.0)
            },
            HlIndGetsN(n) => {
                self.indirect_st(RegisterKind16::Hl, n)
            },
            AGetsBcInd => {
                let n = self.indirect_ld(RegisterKind16::Bc);
                self.registers.write8n(RegisterKind8::A, n);
            },
            AGetsDeInd => {
                let n = self.indirect_ld(RegisterKind16::De);
                self.registers.write8n(RegisterKind8::A, n);
            },
            AGetsNnInd(nn) => {
                let n = self.memory.ld8(nn);
                self.registers.write8n(RegisterKind8::A, n);
            },
            BcIndGetsA => {
                let n = self.registers.a;
                self.indirect_st(RegisterKind16::Bc, n.0)
            },
            DeIndGetsA => {
                let n = self.registers.a;
                self.indirect_st(RegisterKind16::De, n.0)
            },
            NnIndGetsA(nn) => {
                let n = self.registers.a;
                self.memory.st8(nn, n.0);
            },
            AGetsIOOffset(offset) => {
                let addr = Addr::io_memory().offset(offset as u16, Direction::Pos);
                let n = self.memory.ld8(addr);
                self.registers.write8n(RegisterKind8::A, n)
            },
            IOOffsetGetsA(offset) => {
                let addr = Addr::io_memory().offset(offset as u16, Direction::Pos);
                let n = self.registers.read8(RegisterKind8::A);
                self.memory.st8(addr, n.0);
            },
            AGetsIOOffsetByC => {
                let offset = self.registers.c();
                let addr = Addr::io_memory().offset(offset.0 as u16, Direction::Pos);
                let n = self.memory.ld8(addr);
                self.registers.write8n(RegisterKind8::A, n)
            },
            IOOffsetByCGetsA => {
                let offset = self.registers.c();
                let addr = Addr::io_memory().offset(offset.0 as u16, Direction::Pos);
                let n = self.registers.read8(RegisterKind8::A);
                self.memory.st8(addr, n.0);
            },
            HlIndGetsAInc => {
                let n = self.registers.read8(RegisterKind8::A);
                self.indirect_st(RegisterKind16::Hl, n.0);
                self.registers.hl.inc()
            },
            AGetsHlIndInc => {
                let n = self.indirect_ld(RegisterKind16::Hl);
                self.registers.write8n(RegisterKind8::A, n);
                self.registers.hl.inc()
            },
            HlIndGetsADec => {
                let n = self.indirect_ld(RegisterKind16::Hl);
                self.registers.write8n(RegisterKind8::A, n);
                self.registers.hl.dec()
            },
            AGetsHlIndDec => {
              let n = self.registers.read8(RegisterKind8::A);
              self.indirect_st(RegisterKind16::Hl, n.0);
              self.registers.hl.dec()
            },
        };
        BranchAction::Take
    }

    fn execute_arith(&mut self, arith: Arith) -> BranchAction {
        /*match arith {
            Xor(r) => ,
            XorHlInd,
            Sub(RegisterKind8), SubHlInd,
            Inc8(RegisterKind8), Inc16(RegisterKind16), IncHlInd,
            Dec8(RegisterKind8), Dec16(RegisterKind16), DecHlInd,

        }*/
        panic!("TODO");

    }

    fn execute_rotate(&mut self, rotate: Rotate) -> BranchAction {
        panic!("TODO");
    }

    fn execute_jump(&mut self, jump: Jump) -> BranchAction {
        panic!("TODO");
    }

    fn execute(&mut self, instr: Instr) -> BranchAction {
        use self::Instr::*;

        match instr {
          Ld(ld) => self.execute_ld(ld),
          Arith(arith) => self.execute_arith(arith),
          Rotate(rotate) => self.execute_rotate(rotate),
          Jump(jump) => self.execute_jump(jump),
          Bit7h => panic!("TODO"),
          CpHlInd => panic!("TODO"),
          Cp(n) => panic!("TODO"),
          PopBc => panic!("TODO"),
          PushBc => panic!("TODO"),
          Ret => panic!("TODO"),
        }
    }

    // clock speed is 4.194304 MHz
    // we pretend is div 4 since all instrs are multiple of 4
    // 1.04858 MHz
    // 1048.58 ticks per millisecond
    pub fn run(&mut self, dt : f64) {
        let mut clocks_to_tick = (dt * 1048.58) as u32;

        let mut instr = self.ip.peek(&self.memory);
        let (mut take_duration, mut skip_duration) = instr.duration();

        // TODO: Is this off-by-one frame?
        while clocks_to_tick > take_duration {
            let action = {
                self.execute(instr)
            };
            clocks_to_tick -=
                match action {
                    BranchAction::Take => take_duration,
                    BranchAction::Skip => skip_duration.unwrap_or_else(|| take_duration)
                };

            let _ = self.ip.read(&self.memory);
            instr = self.ip.peek(&self.memory);
            let (take_duration_, skip_duration_) = instr.duration();
            take_duration = take_duration_;
            skip_duration = skip_duration_;
        }
    }
}


