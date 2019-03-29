use futures_signals::signal::{Signal, SignalExt};
use std::rc::Rc;
use virtual_dom_rs::prelude::*;

pub struct State<S>
where
    S: Signal<Item = ()>,
{
    // let's just test this thing
    pub unit: S,
}

pub fn component<S: Signal<Item = ()>>(state: State<S>) -> impl Signal<Item = Rc<VirtualNode>> {
    state.unit.map(|_| {
        Rc::new(html! {
            <canvas width="160" height="144" id="canvas" style="width: 100%;image-rendering: pixelated;"></canvas>
        })
    })
}
