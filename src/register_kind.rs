#[derive(Debug)]
pub enum RegisterKind8 {
    A,
    B, C,
    D, E,
    H, L
}

#[derive(Debug)]
pub enum RegisterKind16 {
    Bc,
    De,
    Hl,
    Sp,
}

