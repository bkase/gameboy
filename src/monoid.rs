pub trait Monoid {
    fn combine(self, other: Self) -> Self;
}

impl<T: Monoid> Monoid for Option<T> {
    fn combine(self, other: Option<T>) -> Option<T> {
        match (self, other) {
            (Some(x), None) | (None, Some(x)) => Some(x),
            (None, None) => None,
            (Some(x), Some(y)) => Some(x.combine(y)),
        }
    }
}
