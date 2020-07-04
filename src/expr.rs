use register_kind::{RegisterKind16, RegisterKind8};

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    Plus,
    Minus,
    Times,
    RightShift,
    LeftShift,
    BitAnd,
    BitOr,
}

#[derive(Debug, Clone)]
pub enum Loadable {
    Lit(u16),
    Reg16(RegisterKind16),
}

#[derive(Debug, Clone)]
pub enum Term {
    Reg8(RegisterKind8),
    Load(Loadable),
    Loadable(Loadable),
}

#[derive(Debug, Clone)]
pub struct E0 {
    term: Term,
    e1: Box<E1>,
}

#[derive(Debug, Clone)]
pub enum E1 {
    Epsilon,
    Binop(Op, Box<E0>, Box<E1>),
}

use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_while, take_while1},
    character::{is_digit, is_hex_digit, is_space},
    combinator::{all_consuming, map, map_res},
    error::{ParseError, VerboseError},
    sequence::{delimited, preceded, tuple},
    IResult,
};

fn from_hex8(input: &str) -> Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input, 16)
}

fn from_hex16(input: &str) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input, 16)
}

fn from_decimal16(input: &str) -> Result<u16, std::num::ParseIntError> {
    u16::from_str_radix(input, 10)
}

pub type In<'a> = &'a str;

pub fn sp<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, In<'a>, E> {
    take_while(|c| is_space(c as u8))(input)
}

fn op<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, Op, E> {
    preceded(
        sp,
        alt((
            map(tag("="), |_: &str| Op::Equals),
            map(tag("!="), |_: &str| Op::NotEquals),
            map(tag("<"), |_: &str| Op::LessThan),
            map(tag(">"), |_: &str| Op::GreaterThan),
            map(tag("+"), |_: &str| Op::Plus),
            map(tag("-"), |_: &str| Op::Minus),
            map(tag("*"), |_: &str| Op::Times),
            map(tag(">>"), |_: &str| Op::RightShift),
            map(tag("<<"), |_: &str| Op::LeftShift),
            map(tag("&"), |_: &str| Op::BitAnd),
            map(tag("|"), |_: &str| Op::BitOr),
        )),
    )(input)
}

pub fn lit<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, u16, E> {
    preceded(
        sp,
        alt((
            preceded(
                tag("$"),
                map_res(take_while1(|c| is_hex_digit(c as u8)), from_hex16),
            ),
            preceded(
                tag("0x"),
                map_res(take_while1(|c| is_hex_digit(c as u8)), from_hex16),
            ),
            map_res(take_while1(|c| is_digit(c as u8)), from_decimal16),
        )),
    )(input)
}

fn reg16<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, RegisterKind16, E> {
    preceded(
        sp,
        alt((
            map(tag("BC"), |_: &str| RegisterKind16::Bc),
            map(tag("DE"), |_: &str| RegisterKind16::De),
            map(tag("HL"), |_: &str| RegisterKind16::Hl),
            map(tag("SP"), |_: &str| RegisterKind16::Sp),
        )),
    )(input)
}

fn loadable<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, Loadable, E> {
    preceded(
        sp,
        alt((
            map(lit, |l: u16| Loadable::Lit(l)),
            map(reg16, |r: RegisterKind16| Loadable::Reg16(r)),
        )),
    )(input)
}

fn load<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, Loadable, E> {
    delimited(
        preceded(sp, tag("[")),
        preceded(sp, loadable),
        preceded(sp, tag("]")),
    )(input)
}

fn reg8<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, RegisterKind8, E> {
    preceded(
        sp,
        alt((
            map(tag("A"), |_: &str| RegisterKind8::A),
            map(tag("B"), |_: &str| RegisterKind8::B),
            map(tag("C"), |_: &str| RegisterKind8::C),
            map(tag("D"), |_: &str| RegisterKind8::D),
            map(tag("E"), |_: &str| RegisterKind8::E),
            map(tag("H"), |_: &str| RegisterKind8::H),
            map(tag("L"), |_: &str| RegisterKind8::L),
        )),
    )(input)
}

fn term<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, Term, E> {
    preceded(
        sp,
        alt((
            map(load, |l: Loadable| Term::Load(l)),
            map(loadable, |l: Loadable| Term::Loadable(l)),
            map(reg8, |r: RegisterKind8| Term::Reg8(r)),
        )),
    )(input)
}

pub fn expr0<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, Box<E0>, E> {
    let (input, term) = term(input)?;
    let (input, e1) = expr1(input)?;
    Ok((input, Box::new(E0 { term, e1 })))
}

fn expr1<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, Box<E1>, E> {
    map(
        alt((
            map(tuple((op, expr0, expr1)), |(op, e0, e1)| {
                E1::Binop(op, e0, e1)
            }),
            map(take(0u8), |_: &str| E1::Epsilon),
        )),
        |e1| Box::new(e1),
    )(input)
}

pub fn root<'a, E: ParseError<&'a str>>(input: In<'a>) -> IResult<In<'a>, Box<E0>, E> {
    all_consuming(expr0)(input)
}

#[derive(Debug, Clone)]
pub struct E(pub Box<E0>);
impl std::str::FromStr for E {
    type Err = std::string::String;

    fn from_str(src: &str) -> Result<E, Self::Err> {
        root::<VerboseError<&str>>(src)
            .map(|(_, e0)| E(e0))
            .map_err(|e| match e {
                nom::Err::Error(e) => nom::error::convert_error(src, e),
                nom::Err::Failure(e) => nom::error::convert_error(src, e),
                nom::Err::Incomplete(_) => "Incomplete".to_string(),
            })
    }
}

use hardware::Hardware;
use mem::Addr;

pub type Output = u16;
type Context<'a> = &'a Hardware;
pub trait Eval {
    fn eval(&self, ctx: Context) -> Output;
}

impl Eval for Loadable {
    fn eval(&self, ctx: Context) -> Output {
        match self {
            Loadable::Lit(lit) => *lit,
            Loadable::Reg16(rk) => ctx.cpu.registers.read16(*rk).0,
        }
    }
}

impl Eval for Term {
    fn eval(&self, ctx: Context) -> Output {
        match self {
            Term::Reg8(rk) => u16::from(ctx.cpu.registers.read8(*rk).0),
            Term::Loadable(loadable) => loadable.eval(ctx),
            Term::Load(loadable) => {
                let res = loadable.eval(ctx);
                u16::from(ctx.cpu.memory.ld8(Addr::directly(res)))
            }
        }
    }
}

impl Eval for E0 {
    fn eval(&self, ctx: Context) -> Output {
        fn f(l: Output, op: &Op, r: Output) -> Output {
            match op {
                Op::Equals => {
                    if l == r {
                        0
                    } else {
                        1
                    }
                }
                Op::NotEquals => {
                    if l != r {
                        0
                    } else {
                        1
                    }
                }
                Op::LessThan => {
                    if l < r {
                        0
                    } else {
                        1
                    }
                }
                Op::GreaterThan => {
                    if l > r {
                        0
                    } else {
                        1
                    }
                }
                Op::Plus => l + r,
                Op::Minus => l - r,
                Op::Times => l * r,
                Op::RightShift => l >> r,
                Op::LeftShift => l << r,
                Op::BitAnd => l & r,
                Op::BitOr => l | r,
            }
        }

        fn go(ctx: Context, acc: Output, e1: &Box<E1>) -> Output {
            match e1 {
                box E1::Epsilon => acc,
                box E1::Binop(op, e0, e1) => {
                    let e0_ = e0.eval(ctx);
                    go(ctx, f(acc, op, e0_), e1)
                }
            }
        }

        let t1 = self.term.eval(ctx);
        go(ctx, t1, &self.e1)
    }
}

impl Eval for E {
    fn eval(&self, ctx: Context) -> Output {
        self.0.eval(ctx)
    }
}
