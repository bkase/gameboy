fn num(b: bool) -> u8 {
    if b { 1 } else { 0 }
}

fn bits(x: u8) -> [bool; 8] {
    [ x & 0x01 == 0x01,
      x & 0x02 == 0x02,
      x & 0x04 == 0x04,
      x & 0x08 == 0x08,
      x & 0x10 == 0x10,
      x & 0x20 == 0x20,
      x & 0x40 == 0x40,
      x & 0x80 == 0x80,
    ]
}

fn unbits(b: [bool; 8]) -> u8 {
    num(b[7]) * 0x80 +
    num(b[6]) * 0x40 +
    num(b[5]) * 0x20 +
    num(b[4]) * 0x10 +
    num(b[3]) * 0x08 +
    num(b[2]) * 0x04 +
    num(b[1]) * 0x02 +
    num(b[0]) * 0x01
}

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
fn sub(left: u8, right: u8) -> (u8, bool, bool) {
    fn lookup(b: bool, i1: bool, i2: bool) -> (bool, bool) {
        let lut =
         [ (false, false)
         , (true, true)
         , (true, false)
         , (false, false)
         , (true, true)
         , (false, true)
         , (false, false)
         , (true, true) ];
        let idx = num(b) * 4 + num(i1) * 2 + num(i2);
        lut[idx as usize]
    }

    let bits_left = bits(left);
    let bits_right = bits(right);
    let mut borrow = false;
    let mut borrow_at_4 = false;
    let mut result = [true, true, true, true, true, true, true, true];

    for i in 0 .. 8 {
        let (o, bo) = lookup(borrow, bits_left[i], bits_right[i]);
        borrow = bo;
        if i == 4 {
            borrow_at_4 = bo;
        };
        result[i] = o;
    };

    (unbits(result), borrow_at_4, borrow)
}

#[cfg(test)]
mod tests {
    use alu;
    use test::proptest::prelude::*;

    #[test]
    fn borrow_at_4() {
        let (_, b4, _) = alu::sub(0b01111, 0b10000);
        assert!(b4);
    }

    #[test]
    fn borrow_at_end() {
        let (_, _, b) = alu::sub(4, 5);
        assert!(b);
    }

    proptest! {
        #[test]
        fn bits_unbits_selfinverse(x : u8) {
            assert_eq!(alu::unbits(alu::bits(x)), x)
        }

        #[test]
        fn subtraction_works(left : u8, right: u8) {
            let (r, _, _) = alu::sub(left, right);
            assert_eq!(left.wrapping_sub(right), r)
        }
    }
}


