use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
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
        match self {
            RegisterKind8::A => write!(f, "a"),
            RegisterKind8::B => write!(f, "b"),
            RegisterKind8::C => write!(f, "c"),
            RegisterKind8::D => write!(f, "d"),
            RegisterKind8::E => write!(f, "e"),
            RegisterKind8::H => write!(f, "h"),
            RegisterKind8::L => write!(f, "l"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RegisterKind16 {
    Bc,
    De,
    Hl,
    Sp,
}

impl fmt::Display for RegisterKind16 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RegisterKind16::Bc => write!(f, "bc"),
            RegisterKind16::De => write!(f, "de"),
            RegisterKind16::Hl => write!(f, "hl"),
            RegisterKind16::Sp => write!(f, "sp"),
        }
    }
}
