use register::Registers;
use instr::{Instr, HasDuration, InstrPointer};
use mem::Memory;

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

    fn execute(&self, instr: Instr) -> BranchAction {
        panic!("TODO")
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


