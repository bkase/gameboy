use register::Flags;

/*
    b i1 i2   o bo
    0 0  0    0 0
    0 0  1    1 1
    0 1  0    1 0
    0 1  1    0 0
    1 0  0    1 1
    1 0  1    0 1
    1 1  0    0 0
    1 1  1    1 1
*/
// returns result, was there a borrow at 4, and was there a borrow at the end
fn sbc_(left: u8, right: u8, old_carry: bool) -> (u8, bool, bool) {
    let left16 = u16::from(left);
    let right16 = u16::from(right);

    let old_carry_num = if old_carry { 1 } else { 0 };
    let bottom_nibble_approx = ((left16 & 0x0f) | 0x80) - ((right16 + old_carry_num) & 0x0f);

    let borrow_at_4 = (bottom_nibble_approx & 0x10) == 0x10;
    let borrow_at_end = ((left16 | 0x8000) - (right16 + old_carry_num)) & 0x100 == 0x100;
    (
        left.wrapping_sub(right).wrapping_sub(old_carry_num as u8),
        borrow_at_4,
        borrow_at_end,
    )
}

fn sub_(left: u8, right: u8) -> (u8, bool, bool) {
    sbc_(left, right, false)
}

pub fn sub(flags: &mut Flags, left: u8, right: u8) -> u8 {
    let (r, borrow_at_4, borrow_at_end) = sub_(left, right);
    flags.z = r == 0;
    flags.n = true;
    flags.h = borrow_at_4;
    flags.c = borrow_at_end;
    r
}

pub fn sbc(flags: &mut Flags, left: u8, right: u8) -> u8 {
    let (r, borrow_at_4, borrow_at_end) = sbc_(left, right, flags.c);
    flags.z = r == 0;
    flags.n = true;
    flags.h = borrow_at_4;
    flags.c = borrow_at_end;
    r
}

// returns result, was there a carry at 3, and was there a carry at the end
fn adc_(left: u8, right: u8, old_carry: bool) -> (u8, bool, bool) {
    let left16 = u16::from(left);
    let right16 = u16::from(right);

    let old_carry_num = if old_carry { 1 } else { 0 };
    let bottom_nibble = (left16 & 0x0f) + ((right16 + old_carry_num) & 0x0f);
    let carry_at_3 = (bottom_nibble & 0x10) == 0x10;
    let carry_at_end = (left16 + right16 + old_carry_num) & 0x100 == 0x100;
    (
        left.wrapping_add(right).wrapping_add(old_carry_num as u8),
        carry_at_3,
        carry_at_end,
    )
}

fn add_(left: u8, right: u8) -> (u8, bool, bool) {
    adc_(left, right, false)
}

// returns result, carry_at_11, carry_at_end
fn addhl_(left: u16, right: u16) -> (u16, bool, bool) {
    let left32 = u32::from(left);
    let right32 = u32::from(right);

    let bottom_12 = (left32 & 0xfff) + (right32 & 0xfff);
    let carry_at_11 = (bottom_12 & 0x1000) == 0x1000;
    let carry_at_end = (left32 + right32) & 0x10_000 == 0x10_000;
    (left.wrapping_add(right), carry_at_11, carry_at_end)
}

// TODO: This is a little hacky, and the logic could be wrong. Check this.
// returns result, carry_at_3, carry_at_7
fn addsp_(left: u16, right: i8) -> (u16, bool, bool) {
    let (abs, is_sub) = if right > 0 {
        (right as u8, false)
    } else {
        ((-right) as u8, true)
    };

    if is_sub {
        let (res8, borrow_at_4, borrow_at_end) = sub_((left & 0xff) as u8, abs);
        let result =
            ((left & 0xff00).wrapping_sub(if borrow_at_end { 1 } else { 0 })) | (u16::from(res8));
        (result, borrow_at_4, borrow_at_end)
    } else {
        let (res8, carry_at_3, carry_at_end) = add_((left & 0xff) as u8, abs);
        let result = ((left & 0xff00) | (u16::from(res8))).wrapping_add(1);
        (result, carry_at_3, carry_at_end)
    }
}

pub fn addsp(flags: &mut Flags, left: u16, right: i8) -> u16 {
    let (r, carry_at_3, carry_at_7) = addsp_(left, right);
    flags.z = false;
    flags.n = false;
    flags.h = carry_at_3;
    flags.c = carry_at_7;
    r
}

pub fn addhl(flags: &mut Flags, left: u16, right: u16) -> u16 {
    let (r, carry_at_11, carry_at_end) = addhl_(left, right);
    flags.n = false;
    flags.h = carry_at_11;
    flags.c = carry_at_end;
    r
}

pub fn add(flags: &mut Flags, left: u8, right: u8) -> u8 {
    let (r, carry_at_3, carry_at_end) = add_(left, right);
    flags.z = r == 0;
    flags.n = false;
    flags.h = carry_at_3;
    flags.c = carry_at_end;
    r
}

pub fn adc(flags: &mut Flags, left: u8, right: u8) -> u8 {
    let (r, carry_at_3, carry_at_end) = adc_(left, right, flags.c);
    flags.z = r == 0;
    flags.n = false;
    flags.h = carry_at_3;
    flags.c = carry_at_end;
    r
}

pub fn xor(flags: &mut Flags, left: u8, right: u8) -> u8 {
    let r = left ^ right;
    flags.reset();
    flags.z = r == 0;
    r
}

pub fn or(flags: &mut Flags, left: u8, right: u8) -> u8 {
    let r = left | right;
    flags.reset();
    flags.z = r == 0;
    r
}

pub fn and(flags: &mut Flags, left: u8, right: u8) -> u8 {
    let r = left & right;
    flags.reset();
    flags.z = r == 0;
    flags.h = true;
    r
}

pub fn inc(flags: &mut Flags, x: u8) -> u8 {
    let r = x.wrapping_add(1);
    flags.z = r == 0;
    flags.n = false;
    flags.h = x & 0b1111 == 0b1111;
    // c not affected
    r
}

pub fn dec(flags: &mut Flags, x: u8) -> u8 {
    let old_c = flags.c;
    let r = sub(flags, x, 1);
    flags.c = old_c;
    r
}

pub fn inc16(_flags: &mut Flags, x: u16) -> u16 {
    x.wrapping_add(1)
}

pub fn dec16(_flags: &mut Flags, x: u16) -> u16 {
    x.wrapping_sub(1)
}

pub fn rl(flags: &mut Flags, x: u8) -> u8 {
    let r = (x << 1) | (if flags.c { 1 } else { 0 });
    flags.reset();
    flags.z = r == 0;
    flags.c = x & 0x80 == 0x80;
    r
}

pub fn rlc(flags: &mut Flags, x: u8) -> u8 {
    let bit7 = if x & 0x80 == 0x80 { 1 } else { 0 };
    let r = (x << 1) | bit7;
    flags.reset();
    flags.z = r == 0;
    flags.c = bit7 == 0x1;
    r
}

pub fn rr(flags: &mut Flags, x: u8) -> u8 {
    let r = (x >> 1) | (if flags.c { 0x80 } else { 0 });
    flags.reset();
    flags.z = r == 0;
    flags.c = x & 0x1 == 0x1;
    r
}

pub fn bit(flags: &mut Flags, x: u8, b: u8) {
    assert!(b < 8);
    flags.z = x & (1 << b) == 0;
    flags.n = false;
    flags.h = true;
    // c not affected
}

pub fn reset_bit(x: u8, b: u8) -> u8 {
    assert!(b < 8);
    x & !(1 << b)
}

pub fn set_bit(x: u8, b: u8) -> u8 {
    assert!(b < 8);
    x | (1 << b)
}

pub fn complement(flags: &mut Flags, x: u8) -> u8 {
    flags.n = true;
    flags.h = true;
    !x
}

fn swap_nibbles_(x: u8) -> u8 {
    let high_nibble = (x & 0xf0) >> 4;
    let low_nibble = x & 0x0f;

    (low_nibble << 4) | high_nibble
}

pub fn swap_nibbles(flags: &mut Flags, x: u8) -> u8 {
    let r = swap_nibbles_(x);

    flags.reset();
    flags.z = r == 0;
    r
}

pub fn shift_left_carry(flags: &mut Flags, x: u8) -> u8 {
    let r = x << 1;

    flags.reset();
    flags.z = r == 0;
    flags.c = x & 0x80 == 0x80;
    r
}

pub fn shift_right_carry(flags: &mut Flags, x: u8) -> u8 {
    let r = x >> 1;

    flags.reset();
    flags.z = r == 0;
    flags.c = x & 0x1 == 0x1;
    r
}

// DAA instruction is weird
// https://ehaskins.com/2018-01-30%20Z80%20DAA/
pub fn daa(flags: &mut Flags, x: u8) -> u8 {
    let mut correction = 0;

    if flags.h || (!flags.n && (x & 0xf) > 9) {
        correction |= 0x6;
    }

    if flags.c || (!flags.n && x > 0x99) {
        correction |= 0x60;
        flags.c = true;
    } else {
        flags.c = false;
    }

    let result = if flags.n {
        x.wrapping_sub(correction)
    } else {
        x.wrapping_add(correction)
    };

    flags.z = result == 0;
    flags.h = false;
    result
}

#[cfg(test)]
mod tests {
    use alu;
    use register::Flags;
    use test::proptest::prelude::*;

    #[test]
    fn adc_with_carry() {
        let mut flags = Flags::create();
        let r1 = alu::adc(&mut flags, 0xfe, 0x1);
        assert_eq!(r1, 0);
        assert_eq!(flags.c, true);
    }

    #[test]
    fn set_bit() {
        let r1 = alu::set_bit(0b11001111, 4);
        assert_eq!(r1, 0b11011111);
    }

    #[test]
    fn reset_bit() {
        let r1 = alu::reset_bit(0b11011111, 4);
        assert_eq!(r1, 0b11001111);

        // 9 -> 8
        let r2 = alu::reset_bit(9, 0);
        assert_eq!(r2, 8);
    }

    #[test]
    fn swap_nibbles() {
        let r1 = alu::swap_nibbles_(0b10110000);
        let r2 = alu::swap_nibbles_(0b00001101);
        assert_eq!(r1, 0b00001011);
        assert_eq!(r2, 0b11010000);
    }

    #[test]
    fn complement() {
        let mut flags = Flags::create();
        let x = 0b00110011;
        let x_ = alu::complement(&mut flags, x);
        assert_eq!(x_, 0b11001100);
    }

    #[test]
    fn borrow_at_4() {
        let (_, b4, _) = alu::sub_(0b0111, 0b1000);
        assert!(b4);
    }

    #[test]
    fn borrow_at_end() {
        let (_, _, b) = alu::sub_(4, 5);
        assert!(b);
    }

    #[test]
    fn carry_at_3() {
        let (_, c3, _) = alu::add_(0b1000, 0b1000);
        assert!(c3);
    }

    #[test]
    fn carry_at_end() {
        let (_, _, c) = alu::add_(0xff, 0xff);
        assert!(c);
    }

    #[test]
    fn sub_with_flags() {
        let mut flags = Flags::create();
        let x = 0xce;
        let x_ = alu::sub(&mut flags, x, x);
        assert_eq!(x_, 0);
        assert!(flags.z);
        assert!(flags.n);
        assert!(!flags.h);
        assert!(!flags.c);
    }

    #[test]
    fn daa() {
        let mut flags = Flags::create();
        //                               1   9         2   8
        let x = alu::add(&mut flags, 0b0001_1001, 0b0010_1000);
        let y = alu::daa(&mut flags, x);
        //                 4    7
        assert_eq!(y, 0b0100_0111);
    }

    proptest! {
        #[test]
        fn subtraction_works(left : u8, right: u8) {
            let (r, _, _) = alu::sub_(left, right);
            assert_eq!(left.wrapping_sub(right), r)
        }

        #[test]
        fn addition_works(left: u8, right: u8) {
            let (r, _, _) = alu::add_(left, right);
            assert_eq!(left.wrapping_add(right), r)
        }
    }
}
