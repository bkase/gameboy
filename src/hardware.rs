use cpu::{Cpu, InterruptKind};
use mem::{Addr, TriggeredTimer, DR_MARIO, TEST_01, TETRIS, TIC_TAC_TOE};
use ppu::{Ppu, TriggeredVblank};
use sound::Sound;
use std::collections::HashSet;
use std::fmt;
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
    pub clocks_elapsed: u64,
    clocks_zero: u64,
    // for vblank measurement
    pub vblanks: usize,
    pub start_time: f64,
}

struct SpacedBytes(Vec<u8>);
impl fmt::Display for SpacedBytes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.iter().map(|b| write!(f, "{:02x} ", b)).collect()
    }
}

#[cfg(target_arch = "wasm32")]
impl Hardware {
    fn trace_state(&mut self) {}
}

#[cfg(not(target_arch = "wasm32"))]
impl Hardware {
    fn trace_state(&mut self) {
        if self.clocks_zero == 1 {
            self.clocks_zero = self.clocks_elapsed;
        }

        let f = |b, s| if b { s } else { "-" };
        let s1 = {
            let regs = &self.cpu.registers;
            format!(
                "A:{:02X} F:{:}{:}{:}{:} BC:{:04X} DE:{:04x} HL:{:04x} SP:{:04x} PC:{:04x}",
                regs.a.0,
                f(regs.flags.z, "Z"),
                f(regs.flags.n, "N"),
                f(regs.flags.h, "H"),
                f(regs.flags.c, "C"),
                regs.bc.0,
                regs.de.0,
                regs.hl.0,
                regs.sp.0,
                (self.cpu.ip.0).into_register().0
            )
        };
        let s2 = format!(" (cy: {:})", (self.clocks_elapsed - self.clocks_zero) * 4);
        let s3 = format!(
            " ppu:{:}",
            if self.cpu.memory.ppu.lcdc.display() {
                "+"
            } else {
                "-"
            }
        );
        let s4 = format!(" |[00]0x{:04x}: {:}", (self.cpu.ip.0).into_register().0, {
            let (i, bs) = self.cpu.ip.peek_(&mut self.cpu.memory);
            let (b1, b2, b3) = {
                let b1 = format!("{:02x}", bs[0]);
                let b2 = {
                    if bs.len() > 1 {
                        format!("{:02x}", bs[1])
                    } else {
                        format!("  ")
                    }
                };
                let b3 = {
                    if bs.len() > 2 {
                        format!("{:02x}", bs[2])
                    } else {
                        format!("  ")
                    }
                };
                (b1, b2, b3)
            };
            format!("{:} {:} {:} {:<15}", b1, b2, b3, i)
        });

        log(&format!("{:}{:}{:}{:}", s1, s2, s3, s4));
        // A:01 F:Z-HC BC:0013 DE:00d8 HL:014d SP:fffe PC:0100 (cy: 0) ppu:+0 |[00]0x0100: 00        nop
    }
}

impl Hardware {
    pub fn create(performance: &Performance) -> Hardware {
        let mut _set = HashSet::new();
        Hardware {
            cpu: Cpu::create(Some(TEST_01)),
            ppu: Ppu::create(),
            sound: Sound::create(),
            paused: true,
            dirty: false,
            breakpoints: _set,
            clocks_elapsed: 0,
            clocks_zero: 1,
            vblanks: 0,
            start_time: performance.now(),
        }
    }

    // step the hardware forwards once
    // useful for step-debugging
    fn step_(&mut self) -> u32 {
        if !self.cpu.memory.booting {
            self.trace_state();
        }

        let elapsed_duration = self.cpu.execute();
        let TriggeredVblank(triggered_vblank) =
            self.ppu.advance(&mut self.cpu.memory, elapsed_duration);
        // TODO: Is this okay that we ppu advance after the CPU tick? I.e. do we execute that
        // instruction before the vblank interrupt triggers?
        self.sound.advance(&mut self.cpu.memory, elapsed_duration);

        // update timers
        self.clocks_elapsed += u64::from(elapsed_duration);
        let TriggeredTimer(triggered_timer) = self
            .cpu
            .memory
            .advance_timers((self.clocks_elapsed % 4194304) as u32);

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
