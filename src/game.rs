use futures::{Future, FutureExt};
use futures_signals::signal::{Signal, SignalExt};
use virtual_dom_rs::prelude::*;

pub struct State<S>
where
    S: Signal<Item = ()>,
{
    // let's just test this thing
    unit: S,
}

pub fn component<S: Signal<Item = ()>>(state: State<S>) -> impl Signal<Item = VirtualNode> {
    state.unit.map(|_| {
        html! {
            <canvas width="160" height="144" id="canvas" style="width: 100%;image-rendering: pixelated;"></canvas>
        }
    })
}
