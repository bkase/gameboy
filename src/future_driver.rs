use futures::task::{Context, Poll};
use futures::Future;
use std::cell::RefCell;
use std::rc::Rc;

pub fn tick<F: Future + Unpin>(f: Rc<RefCell<F>>) -> Poll<F::Output> {
    let waker = futures_util::task::noop_waker();
    // TODO: Is this safe?
    let mut pin = unsafe { std::pin::Pin::new_unchecked(f.borrow_mut()) };
    let mut context = Context::from_waker(&waker);
    pin.as_mut().poll(&mut context)
}
