pub trait ReadViewU8 {
    fn read(&self) -> u8;
}

pub trait ViewU8: ReadViewU8 {
    fn set(&mut self, n: u8);
}

impl ReadViewU8 for u8 {
    fn read(&self) -> u8 {
        *self
    }
}
impl ViewU8 for u8 {
    fn set(&mut self, n: u8) {
        *self = n
    }
}
