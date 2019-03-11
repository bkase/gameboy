use register_kind::{RegisterKind8, RegisterKind16};

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct R16(pub u16);
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct R8(pub u8);

impl R16 {
    pub fn hi(&self) -> R8 {
        R8(((self.0 & 0xff00) >> 8) as u8)
    }

    pub fn lo(&self) -> R8 {
        R8((self.0 & 0x00ff) as u8)
    }

    pub fn inc(&mut self) {
        let old = self.0;
        (*self).0 = old.wrapping_add(1);
    }

    pub fn dec(&mut self) {
        let old = self.0;
        (*self).0 = old.wrapping_sub(1);
    }
}

impl R8 {
    pub fn concat(self, lo : R8) -> R16 {
        R16(((self.0 as u16) << 8) | lo.0 as u16)
    }

    pub fn inc(&mut self) {
        let old = self.0;
        (*self).0 = old +& 1;
    }

    pub fn dec(&mut self) {
        let old = self.0;
        (*self).0 = old -& 1;
    }
}

impl R16 {
    fn set_hi(&mut self, n: R8) {
        let lo = self.lo();
        *self = n.concat(lo);
    }

    fn set_lo(&mut self, n: R8) {
        let hi = self.hi();
        *self = hi.concat(n);
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

// flags
//===================
// 7 6 5 4 3 2 1 0
// Z N H C 0 0 0 0
//==================
pub struct Flags {
    pub z: bool,
    pub n: bool,
    pub h: bool,
    pub c: bool,
}
impl Flags {
    pub fn reset(&mut self) {
        self.z = false;
        self.n = false;
        self.h = false;
        self.c = false;
    }
}

pub struct Registers {
    pub bc : R16,
    pub de : R16,
    pub hl : R16,
    pub a : R8, // acc
    pub sp : R16, // stack pointer
    pub flags : Flags,
}

// accessors for registers and flags
impl Registers {
    pub fn create() -> Registers {
        Registers {
            bc: R16(0x0000),
            de: R16(0x0000),
            hl: R16(0x0000),
            a: R8(0x0000),
            sp: R16(0x0000),
            flags: Flags{ z: false, n: false, h: false, c: false },
        }
    }

    pub fn b(&self) -> R8 { self.bc.hi() }
    pub fn c(&self) -> R8 { self.bc.lo() }
    pub fn set_b(&mut self, n: R8) { self.bc.set_hi(n); }
    pub fn set_c(&mut self, n: R8) { self.bc.set_lo(n) }

    pub fn d(&self) -> R8 { self.de.hi() }
    pub fn e(&self) -> R8 { self.de.lo() }
    pub fn set_d(&mut self, n: R8) { self.de.set_hi(n); }
    pub fn set_e(&mut self, n: R8) { self.de.set_lo(n) }

    pub fn h(&self) -> R8 { self.hl.hi() }
    pub fn l(&self) -> R8 { self.hl.lo() }
    pub fn set_h(&mut self, n: R8) { self.hl.set_hi(n); }
    pub fn set_l(&mut self, n: R8) { self.hl.set_lo(n) }

    pub fn read8(&self, kind: RegisterKind8) -> R8 {
        match kind {
            RegisterKind8::B => self.b(),
            RegisterKind8::C => self.c(),
            RegisterKind8::D => self.d(),
            RegisterKind8::E => self.e(),
            RegisterKind8::H => self.h(),
            RegisterKind8::L => self.l(),
            RegisterKind8::A => self.a,
        }
    }

    pub fn read16(&self, kind: RegisterKind16) -> R16 {
        match kind {
            RegisterKind16::Bc => self.bc,
            RegisterKind16::De => self.de,
            RegisterKind16::Hl => self.hl,
            RegisterKind16::Sp => self.sp,
        }
    }

    pub fn write8r(&mut self, kind: RegisterKind8, n: R8) {
        match kind {
            RegisterKind8::B => self.set_b(n),
            RegisterKind8::C => self.set_c(n),
            RegisterKind8::D => self.set_d(n),
            RegisterKind8::E => self.set_e(n),
            RegisterKind8::H => self.set_h(n),
            RegisterKind8::L => self.set_l(n),
            RegisterKind8::A => (*self).a = n,
        }
    }

    pub fn write8n(&mut self, kind: RegisterKind8, n: u8) {
        self.write8r(kind, R8(n))
    }

    pub fn write16r(&mut self, kind: RegisterKind16, n: R16) {
        match kind {
            RegisterKind16::Bc => self.bc = n,
            RegisterKind16::De => self.de = n,
            RegisterKind16::Hl => self.hl = n,
            RegisterKind16::Sp => self.sp = n,
        }
    }

    pub fn write16n(&mut self, kind: RegisterKind16, n: u16) {
        self.write16r(kind, R16(n))
    }
}
