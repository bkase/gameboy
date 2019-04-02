use futures::{Future, FutureExt};
use futures_signals::signal::{Signal, SignalExt};
use virtual_dom_rs::prelude::*;
use web_utils;

fn sample_view<In: Signal<Item = u32>>(model: In) -> impl Signal<Item = VirtualNode> {
    model.map(|m| {
        let greetings = format!("Hello, World! {}", m);
        html! { <div> { greetings } </div> }
    })
}

pub fn run<In: Signal<Item = u32>>(rx: In) -> impl Future<Output = ()> {
    let start_view = html! { <div> Hello </div> };

    let body = web_utils::document().body().unwrap();

    let mut dom_updater = DomUpdater::new_append_to_mount(start_view, &body);

    sample_view(rx)
        .map(move |vdom| dom_updater.update(vdom))
        .to_future()
        .map(|_| ())
}
