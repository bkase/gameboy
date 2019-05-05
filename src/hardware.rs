use cpu::Cpu;
use mem::Addr;
use ppu::Ppu;
use std::collections::HashSet;
use web_utils::log;

pub struct Hardware {
    pub cpu: Cpu,
    pub ppu: Ppu,
    pub paused: bool,
    // dirty bit, aka we need to redraw things with this is true
    pub dirty: bool,
    pub breakpoints: HashSet<Addr>,
}

impl Hardware {
    pub fn create() -> Hardware {
        let mut set = HashSet::new();
        set.insert(Addr::directly(0x00e0));
        //set.insert(Addr::directly(0x00fe));
        Hardware {
            cpu: Cpu::create(),
            ppu: Ppu::create(),
            paused: true,
            dirty: false,
            breakpoints: set,
        }
    }

    // step the hardware forwards once
    // useful for step-debugging
    fn step_(&mut self) -> u32 {
        let elapsed_duration = self.cpu.execute();
        self.ppu.advance(&mut self.cpu.memory, elapsed_duration);
        return elapsed_duration;
    }

    /// step the hardware forwards and repaint
    pub fn step(&mut self) {
        log(&format!(
            "Executing {:}",
            self.cpu.ip.peek(&self.cpu.memory)
        ));
        let _ = self.step_();
        self.ppu.repaint(&self.cpu.memory);
        self.dirty = true;
    }

    // clock speed is 4.194304 MHz
    // for CPU we pretend is div 4 since all instrs are multiple of 4
    // 1.04858 MHz
    // 1048.58 ticks per millisecond
    //
    // for PPU at least one part it does run at 4MHz
    pub fn run(&mut self, dt: f64) {
        if self.paused {
            return;
        }

        let mut clocks_to_tick = (dt * 1048.58 / 15.0) as u32;
        let mut duration = self.cpu.peek_next();

        // TODO: Protection for taking too long snowball
        // TODO: Is this off-by-one frame?
        while clocks_to_tick > duration {
            let elapsed_duration = self.step_();
            clocks_to_tick -= elapsed_duration;

            duration = self.cpu.peek_next();
            // hit a breakpoint?
            if self.breakpoints.contains(&self.cpu.ip.0) {
                self.paused = true;
                break;
            }
        }

        self.ppu.repaint(&self.cpu.memory);
        self.dirty = true;
    }

    pub fn force_repaint(&mut self) {
        self.ppu.force_repaint(&self.cpu.memory);
    }
}
