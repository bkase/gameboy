use std::fmt;

#[derive(Debug, Clone, Copy)]
pub enum RegisterKind8 {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}
impl fmt::Display for RegisterKind8 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum RegisterKind16 {
    Bc,
    De,
    Hl,
    Sp,
}

impl fmt::Display for RegisterKind16 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RegisterKind16::Bc => write!(f, "BC"),
            RegisterKind16::De => write!(f, "DE"),
            RegisterKind16::Hl => write!(f, "HL"),
            RegisterKind16::Sp => write!(f, "SP"),
        }
    }
}
