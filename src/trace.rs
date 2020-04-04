use instr::{InstrPointer, ReadOnlyTape};
use mem::Addr;
use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take, take_until},
    combinator::{map, map_res},
    multi::count,
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
#[cfg(test)]
use proptest_derive::Arbitrary;
use register::{Flags, Registers, R16, R8};
use register_kind::RegisterKind16;
use std::cmp::Ordering;
use std::fmt;

fn from_hex8(input: &str) -> Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input, 16)
}

fn from_hex16(input: &str) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input, 16)
}

fn from_decimal64(input: &str) -> Result<u64, std::num::ParseIntError> {
    u64::from_str_radix(input, 10)
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(Arbitrary))]
pub struct Record {
    registers: Registers,
    ip: InstrPointer,
    cy: u64,
    ppu_display: bool,
    // fixing the value for instructions since not all instrs are implemented yet
    #[cfg_attr(test, proptest(value = "vec![0x18, 0x29, 0x13]"))]
    instr_bytes: Vec<u8>,
}
impl PartialEq for Record {
    fn eq(&self, other: &Self) -> bool {
        self.ip == other.ip
    }
}
impl Eq for Record {}
impl PartialOrd for Record {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Record {
    fn cmp(&self, other: &Self) -> Ordering {
        self.ip.cmp(&other.ip)
    }
}

type In<'a> = &'a str;

// parse
fn a_reg(input: In) -> IResult<In, R8> {
    (preceded(
        tag_no_case("A:"),
        map(map_res(take(2 as usize), from_hex8), |x| R8(x)),
    ))(input)
}

fn labeled_r16(label: &'static str) -> impl Fn(In) -> IResult<In, R16> {
    move |input| {
        (preceded(
            pair(tag_no_case(label), tag(":")),
            map(map_res(take(4 as usize), from_hex16), |x| R16(x)),
        ))(input)
    }
}

fn r16(kind: RegisterKind16) -> impl Fn(In) -> IResult<In, R16> {
    labeled_r16(match kind {
        RegisterKind16::Bc => "bc",
        RegisterKind16::De => "de",
        RegisterKind16::Hl => "hl",
        RegisterKind16::Sp => "sp",
    })
}

fn flag(c: char) -> impl Fn(In) -> IResult<In, bool> {
    move |input| map(take(1 as usize), |c_: &str| c == c_.chars().next().unwrap())(input)
}

fn flags(input: In) -> IResult<In, Flags> {
    preceded(
        tag_no_case("F:"),
        map(
            tuple((flag('Z'), flag('N'), flag('H'), flag('C'))),
            |(z, n, h, c)| Flags { z, n, h, c },
        ),
    )(input)
}

fn word<P, O>(p: P) -> impl Fn(In) -> IResult<In, O>
where
    P: Fn(In) -> IResult<In, O>,
{
    move |input: In| {
        let (input, o) = p(input)?;
        tag(" ")(input).map(|(i, _)| (i, o))
    }
}

fn registers(input: In) -> IResult<In, Registers> {
    map(
        tuple((
            word(a_reg),
            word(flags),
            word(r16(RegisterKind16::Bc)),
            word(r16(RegisterKind16::De)),
            word(r16(RegisterKind16::Hl)),
            word(r16(RegisterKind16::Sp)),
        )),
        |(a, flags, bc, de, hl, sp)| Registers {
            bc,
            de,
            hl,
            a,
            sp,
            flags,
        },
    )(input)
}

fn cy(input: In) -> IResult<In, u64> {
    delimited(
        tag("(cy: "),
        map_res(take_until(")"), from_decimal64),
        tag(")"),
    )(input)
}

fn ppu(input: In) -> IResult<In, bool> {
    delimited(
        tag("ppu:"),
        map(take(1 as usize), |c: In| c.chars().next().unwrap() == '+'),
        take(1 as usize),
    )(input)
}

fn bank(input: In) -> IResult<In, Option<u8>> {
    delimited(
        tag("["),
        map(take(2 as usize), |xs: In| from_hex8(xs).ok()),
        tag("]"),
    )(input)
}

fn pc_addr(input: In) -> IResult<In, InstrPointer> {
    map(
        preceded(tag("0x"), map_res(take(4 as usize), from_hex16)),
        |res: u16| InstrPointer(Addr::directly(res)),
    )(input)
}

fn instr_bytes(input: In) -> IResult<In, Vec<u8>> {
    let p = || map_res(terminated(take(2 as usize), tag(" ")), from_hex8);
    // using alt instead of many1 to assert it's at most 3
    alt((count(p(), 3), count(p(), 2), count(p(), 1)))(input)
}

impl Record {
    pub fn create(
        registers: Registers,
        ip: InstrPointer,
        cy: u64,
        ppu_display: bool,
        instr_bytes: Vec<u8>,
    ) -> Record {
        Record {
            registers,
            ip,
            cy,
            ppu_display,
            instr_bytes,
        }
    }
}

impl Record {
    pub fn of_line(input: In) -> IResult<In, Record> {
        let (input, regs) = registers(input)?;
        let (input, pc) = word(labeled_r16("pc"))(input)?;
        let (input, cy) = word(cy)(input)?;
        let (input, ppu) = terminated(word(ppu), tag("|"))(input)?;
        let (input, bank) = bank(input)?;
        let (input, pc_addr) = terminated(pc_addr, tag(": "))(input)?;
        let (input, instr_bytes) = instr_bytes(input)?;
        assert_eq!(
            (pc_addr.0).into_register(),
            pc,
            "Two pcs need to be the same"
        );

        Ok((input, Self::create(regs, pc_addr, cy, ppu, instr_bytes)))
    }
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
        let bank_raw = if self.ip.into_u16() >= 0xc000 {
            None
        } else {
            Some(0x00)
        };
        let bank = match bank_raw {
            Some(x) => format!("{:02x}", x),
            None => format!("{:}", "??"),
        };
        let s4 = format!(
            " |[{:}]0x{:04x}: {:}",
            bank,
            (self.ip.0).into_register().0,
            {
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
                format!("{:} {:} {:}  {:<15}", b1, b2, b3, i)
            }
        );

        write!(ft, "{:}{:}{:}{:}", s1, s2, s3, s4)
    }
}

#[cfg(test)]
mod tests {
    use test::proptest::prelude::*;
    use trace::*;

    #[test]
    fn parse_print_roundtrip() {
        let line =
            "A:01 F:Z-HC BC:0013 DE:00d8 HL:4000 SP:fffe PC:0216 (cy: 32) ppu:+0 |[00]0x0216: c3 00 02  jp $0200";
        let (_, parsed) = Record::of_line(&line).unwrap();
        let printed = format!("{:}", parsed);
        assert_eq!(line, printed);
    }
    proptest! {
        #[test]
        fn print_parse_partial_iso(r: Record) {
            let printed = format!("{:}", r);
            let (_, parsed) = Record::of_line(&printed).unwrap();
            assert_eq!(r, parsed)
        }
    }
}
