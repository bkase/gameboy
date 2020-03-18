use instr::{InstrPointer, ReadOnlyTape};
use register::{Registers, R16, R8};
use register_kind;
use std::fmt;
use std::string::String;

#[derive(Debug)]
pub struct Record {
    registers: Registers,
    ip: InstrPointer,
    cy: u64,
    ppu_display: bool,
    bank: Option<u8>,
    instr_bytes: Vec<u8>,
}

impl ReadOnlyTape for Record {
    fn peek8_offset(&self, by: i8) -> u8 {
        self.instr_bytes[by as usize]
    }

    fn peek16_offset(&self, by: i8) -> u16 {
        let lo = self.instr_bytes[by as usize];
        let hi = self.instr_bytes[(1 + by) as usize];
        (u16::from(hi) << 8) | u16::from(lo)
    }
}

impl fmt::Display for Record {
    fn fmt(&self, ft: &mut fmt::Formatter) -> fmt::Result {
        let f = |b, s| if b { s } else { "-" };
        let s1 = {
            let regs = &self.registers;
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
                (self.ip.0).into_register().0
            )
        };
        let s2 = format!(" (cy: {:})", self.cy);
        let s3 = format!(" ppu:{:}0", if self.ppu_display { "+" } else { "-" });
        let s4 = format!(" |[00]0x{:04x}: {:}", (self.ip.0).into_register().0, {
            let (i, bs) = self.peek_();
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

        write!(ft, "{:}{:}{:}{:}", s1, s2, s3, s4)
    }
}
