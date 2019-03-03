#[derive(Debug)]
pub enum RegisterKind8 {
    A, F,
    B, C,
    D, E,
    H, L
}

#[derive(Debug)]
pub enum RegisterKind16 {
    Af,
    Bc,
    De,
    Hl,
    Sp,
    Pc
}

