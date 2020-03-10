use cpu::{Cpu, InterruptKind};
use mem::{Addr, TriggeredTimer, DR_MARIO, TEST_01, TETRIS, TIC_TAC_TOE};
use ppu::{Ppu, TriggeredVblank};
use sound::Sound;
use std::collections::HashSet;
use web_utils::log;
use web_utils::*;

#[derive(Debug)]
pub struct Hardware {
    pub cpu: Cpu,
    pub ppu: Ppu,
    pub sound: Sound,
    pub paused: bool,
    // dirty bit, aka we need to redraw things with this is true
    pub dirty: bool,
    pub breakpoints: HashSet<Addr>,
    // between 0 and 4194304
    pub clocks_elapsed_mod_seconds: u32,
    // for vblank measurement
    pub vblanks: usize,
    pub start_time: f64,
}

impl Hardware {
    pub fn create() -> Hardware {
        let mut _set = HashSet::new();
        Hardware {
            cpu: Cpu::create(Some(TETRIS)),
            ppu: Ppu::create(),
            sound: Sound::create(),
            paused: true,
            dirty: false,
            breakpoints: _set,
            clocks_elapsed_mod_seconds: 0,
            vblanks: 0,
            start_time: performance.now(),
        }
    }

    // step the hardware forwards once
    // useful for step-debugging
    fn step_(&mut self) -> u32 {
        let elapsed_duration = self.cpu.execute();
        let TriggeredVblank(triggered_vblank) =
            self.ppu.advance(&mut self.cpu.memory, elapsed_duration);
        // TODO: Is this okay that we ppu advance after the CPU tick? I.e. do we execute that
        // instruction before the vblank interrupt triggers?
        self.sound.advance(&mut self.cpu.memory, elapsed_duration);

        // update timers
        self.clocks_elapsed_mod_seconds =
            (self.clocks_elapsed_mod_seconds + elapsed_duration) % 4194304;
        let TriggeredTimer(triggered_timer) = self
            .cpu
            .memory
            .advance_timers(self.clocks_elapsed_mod_seconds);

        // attempt interrupts
        if triggered_vblank {
            self.vblanks += 1;
            self.cpu.attempt_interrupt(InterruptKind::Vblank);
        }
        if triggered_timer {
            self.cpu.attempt_interrupt(InterruptKind::Timer);
        }

        elapsed_duration
    }

    // html! macro makes this not dead
    #[allow(dead_code)]
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

        let mut clocks_to_tick = (dt * 1048.58) as u32;
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

    // html! macro makes this not dead
    #[allow(dead_code)]
    pub fn force_repaint(&mut self) {
        self.ppu.force_repaint(&self.cpu.memory);
    }
}
