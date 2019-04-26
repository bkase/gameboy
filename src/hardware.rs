use cpu::Cpu;
use mem::{Addr, Direction};
use ppu::Ppu;
use std::cell::RefCell;
use std::rc::Rc;

pub struct Hardware {
    pub cpu: Cpu,
    pub ppu: Ppu,
}

impl Hardware {
    pub fn create() -> Hardware {
        Hardware {
            cpu: Cpu::create(),
            ppu: Ppu::create(),
        }
    }

    // clock speed is 4.194304 MHz
    // for CPU we pretend is div 4 since all instrs are multiple of 4
    // 1.04858 MHz
    // 1048.58 ticks per millisecond
    //
    // for PPU at least one part it does run at 4MHz
    pub fn run(&mut self, dt: f64) {
        let mut clocks_to_tick = (dt * 1048.58 / 1000.0) as u32;

        let mut duration = self.cpu.peek_next();

        // TODO: Is this off-by-one frame?
        while clocks_to_tick > duration {
            let elapsed_duration = self.cpu.execute();
            self.ppu.advance(&self.cpu.memory, elapsed_duration);
            clocks_to_tick -= elapsed_duration;

            duration = self.cpu.peek_next();
        }
    }
}
