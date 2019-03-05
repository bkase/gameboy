#[derive(Debug, Clone, Copy)]
pub enum RegisterKind8 {
    A,
    B, C,
    D, E,
    H, L
}

#[derive(Debug, Clone, Copy)]
pub enum RegisterKind16 {
    Bc,
    De,
    Hl,
    Sp,
}

