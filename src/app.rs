use cpu_control_view;
use futures::{Future, FutureExt};
use futures_signals::map_ref;
use futures_signals::signal::{Mutable, Signal, SignalExt};
use game;
use hack_vdom::InjectNode;
use hardware::Hardware;
use mem_view;
use mutable_effect::MutableEffect;
use reg_view;
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
    pub hardware: Rc<MutableEffect<Rc<RefCell<Hardware>>>>,
    pub mem_view_state: Vec<mem_view::LocalState<Rc<RefCell<Mutable<u16>>>>>,
    pub cpu_control_view_state: Rc<RefCell<Mutable<cpu_control_view::Mode>>>,
    // we could have one field per component that emits events
}

fn component(state: &AppState) -> impl Signal<Item = VirtualNode> {
    let unit = &(state.globals).unit;
    let game = game::component(game::State {
        unit: Box::new(unit.signal()),
    });
    //TODO: How do I traverse?
    let mem_view1 = mem_view::component(mem_view::State {
        hardware: state.hardware.clone(),
        local: state.mem_view_state[0].clone(),
    });
    let mem_view2 = mem_view::component(mem_view::State {
        hardware: state.hardware.clone(),
        local: state.mem_view_state[1].clone(),
    });
    let reg_view = reg_view::component(reg_view::State {
        hardware: state.hardware.clone(),
    });
    let cpu_control_view = cpu_control_view::component(cpu_control_view::State {
        hardware: state.hardware.clone(),
        mode: state.cpu_control_view_state.clone(),
    });

    map_ref! {
        let _ = unit.signal(),
        let mem_view_dom1 = mem_view1,
        let mem_view_dom2 = mem_view2,
        let reg_view_dom = reg_view,
        let cpu_control_view_dom = cpu_control_view,
        let game_dom = game => {
            let game_dom : InjectNode = InjectNode(game_dom.clone());
            let mem_view_dom1 : InjectNode = InjectNode(mem_view_dom1.clone());
            let mem_view_dom2 : InjectNode = InjectNode(mem_view_dom2.clone());
            let reg_view_dom : InjectNode = InjectNode(reg_view_dom.clone());
            let cpu_control_view_dom : InjectNode = InjectNode(cpu_control_view_dom.clone());
            html! {
                <div class="mw8 ph4 mt2">
                <div class="flex">
                    <div class="mw6 w-100">
                        { game_dom }
                    </div>
                    <div class="mw4">
                        { cpu_control_view_dom }
                    </div>
                </div>
                <div class="mw5 mt2">
                    { reg_view_dom }
                </div>
                <div class="mw7 mt2">
                    { mem_view_dom1 }
                </div>
                <div class="mw7 mt2">
                    { mem_view_dom2 }
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
