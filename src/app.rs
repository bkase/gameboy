use futures::{Future, FutureExt};
use futures_signals::map_ref;
use futures_signals::signal::{Mutable, Signal, SignalExt};
use game;
use hack_vdom::InjectNode;
use mem::Memory;
use mem_view;
use mutable_effect::MutableEffect;
use std::cell::RefCell;
use std::rc::Rc;
use virtual_dom_rs::prelude::*;
use web_utils;

// the idea is that "global" shared state would go here
// Memory or Key-presses would go here
pub struct Globals {
    // let's just make sure lifetime stuff is solid
    pub unit: Mutable<()>,
    // a simple counter per frame
    pub frames: Mutable<u32>,
}

/*
 * The plan:
 *
 *  * AppState has all atomic pieces of mutable state in `Mutable`s
 *  * Different components grab different mutables as needed
 *  * Mutables that wrap mutable references are updated every frame
 *      * They need cheap PartialEq instances so we can dedupe efficiently
 *  * Each component (at the leaves!), combines the mutables together and
 *      creates a single signal
 *  * Leaf components return Signal<Rc<VirtualNode>>
 *  * Non-leaves combine the children's Signal<Rc<VirtualNode>>s together
 *  * At the end we have a single Signal<VirtualNode> that we patch into the DOM
 *  * AppState owns all the Mutables, but all other components mixin the events they want by
 *  reference. They mixin readonly signals by value (as these are cheap and are created from
 *  mutables)
 */
pub struct AppState {
    pub globals: Globals,
    pub mem: Rc<MutableEffect<Rc<Memory>>>,
    pub mem_view_state: mem_view::LocalState<Rc<RefCell<Mutable<u16>>>>,
    // we could have one field per component that emits events
}

fn component(state: &AppState) -> impl Signal<Item = VirtualNode> {
    let unit = &(state.globals).unit;
    let game = game::component(game::State {
        unit: Box::new(unit.signal()),
    });
    let mem_view = mem_view::component(mem_view::State {
        mem: state.mem.clone(),
        local: state.mem_view_state.clone(),
    });

    map_ref! {
        let _ = unit.signal(),
        let mem_view_dom = mem_view,
        let game_dom = game => {
            let game_dom : InjectNode = InjectNode(game_dom.clone());
            let mem_view_dom : InjectNode = InjectNode(mem_view_dom.clone());
            html! {
                <div class="mw8 ph4 mt2">
                <div class="mw6">
                    { game_dom }
                </div>
                <div class="mw7 mt2">
                    { mem_view_dom }
                </div>
            }
        }
    }
}

pub fn run(app_state: &AppState) -> impl Future<Output = ()> {
    let start_view = html! { <div /> };

    let body = web_utils::document().body().unwrap();

    let mut dom_updater = DomUpdater::new_append_to_mount(start_view, &body);

    component(app_state)
        .map(move |vdom| dom_updater.update(vdom))
        .to_future()
        .map(|_| ())
}
