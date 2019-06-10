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
    (left.wrapping_sub(right), borrow_at_4, borrow_at_end)
}

fn sub_(left: u8, right: u8) -> (u8, bool, bool) {
    sbc_(left, right, false)
}

pub fn sub(flags: &mut Flags, left: u8, right: u8) -> u8 {
    let (r, borrow_at_4, borrow_at_end) = sub_(left, right);
    flags.z = r == 0;
    flags.n = true;
    flags.h = !borrow_at_4;
    flags.c = !borrow_at_end;
    r
}

pub fn sbc(flags: &mut Flags, left: u8, right: u8) -> u8 {
    let (r, borrow_at_4, borrow_at_end) = sbc_(left, right, flags.c);
    flags.z = r == 0;
    flags.n = true;
    flags.h = !borrow_at_4;
    flags.c = !borrow_at_end;
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
    (left.wrapping_add(right), carry_at_3, carry_at_end)
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
        ((right * -1) as u8, true)
    };

    if is_sub {
        let (res8, borrow_at_4, borrow_at_end) = sub_((left & 0xff) as u8, abs);
        let result =
            ((left & 0x00).wrapping_sub(if borrow_at_end { 1 } else { 0 })) | (u16::from(res8));
        (result, borrow_at_4, borrow_at_end)
    } else {
        let (res8, carry_at_3, carry_at_end) = add_((left & 0xff) as u8, abs);
        let result = ((left & 0x00) | (u16::from(res8))).wrapping_add(1);
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
    flags.h = x & 0b111 == 0b111;
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

pub fn bit(flags: &mut Flags, x: u8, b: u8) {
    assert!(b < 8);
    flags.z = x & (1 << b) == 0;
    flags.n = false;
    flags.h = true;
    // c not affected
}

#[cfg(test)]
mod tests {
    use alu;
    use test::proptest::prelude::*;

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
