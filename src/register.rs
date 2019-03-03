#[derive(Copy, Clone, Debug, PartialEq)]
pub struct R16(u16);
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct R8(u8);

impl R16 {
    fn unwrap(self) -> u16 { let R16(x) = self; x }

    fn hi(self) -> R8 {
        R8(((self.unwrap() & 0xff00) >> 8) as u8)
    }

    fn lo(self) -> R8 {
        R8((self.unwrap() & 0x00ff) as u8)
    }
}

impl R8 {
    fn unwrap(self) -> u8 { let R8(x) = self; x }

    fn concat(self, lo : R8) -> R16 {
        R16(((self.unwrap() as u16) << 8) | lo.unwrap() as u16)
    }
}

#[cfg(test)]
mod r_tests {
    use register::{R16, R8};
    use test::proptest::prelude::*;

    proptest! {
        #[test]
        fn r16_decompose_recompose(x : u16) {
            let r16 = R16(x);
            assert_eq!(r16.hi().concat(r16.lo()), r16)
        }
    }
}

pub struct Registers {
    af : R16, // acc/flags
    bc : R16,
    de : R16,
    hl : R16,
    sp : R16, // stack pointer
}
// accessors for registers and flags
impl Registers {
    pub fn create() -> Registers {
        Registers {
            af: R16(0x0000),
            bc: R16(0x0000),
            de: R16(0x0000),
            hl: R16(0x0000),
            sp: R16(0x0000),
        }
    }

    pub fn af(self) -> R16 { self.af }
    pub fn a(self) -> R8 { self.af.hi() }
    pub fn f(self) -> R8 { self.af.lo() }

    pub fn bc(self) -> R16 { self.bc }
    pub fn b(self) -> R8 { self.bc.hi() }
    pub fn c(self) -> R8 { self.bc.lo() }

    pub fn de(self) -> R16 { self.de }
    pub fn d(self) -> R8 { self.de.hi() }
    pub fn e(self) -> R8 { self.de.lo() }

    pub fn hl(self) -> R16 { self.hl }
    pub fn h(self) -> R8 { self.hl.hi() }
    pub fn l(self) -> R8 { self.hl.lo() }

    pub fn sp(self) -> R16 { self.sp }
}
