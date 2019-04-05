use futures_signals::signal::ReadOnlyMutable;

// TODO: Put some impls on this
/// A MutableEffect gives the illusion that you have a signal of something that
/// is mutable, but may not implement Copy. A unit is fired on the
/// `ReadOnlyMutable` every time the state should be read again.
pub struct MutableEffect<T> {
    pub state: T,
    pub trigger: ReadOnlyMutable<()>,
}

impl<T: Clone> MutableEffect<T> {
    pub fn clone_data(&self) -> T {
        self.state.clone()
    }
}
