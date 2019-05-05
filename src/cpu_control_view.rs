#![allow(dead_code)]

use futures_signals::map_ref;
use futures_signals::signal::{Mutable, Signal};
use hardware::Hardware;
use instr::InstrPointer;
use mutable_effect::MutableEffect;
use std::cell::RefCell;
use std::rc::Rc;
use virtual_dom_rs::prelude::*;
#[allow(unused_imports)]
use web_sys::MouseEvent;
use web_utils::log;

#[derive(Debug, Copy, Clone)]
pub enum Mode {
    Paused,
    Running,
}
impl Mode {
    fn toggle(self) -> Mode {
        match self {
            Mode::Paused => Mode::Running,
            Mode::Running => Mode::Paused,
        }
    }
}

pub struct State {
    pub hardware: Rc<MutableEffect<Rc<RefCell<Hardware>>>>,
    pub mode: Rc<RefCell<Mutable<Mode>>>,
}

pub fn component(state: State) -> impl Signal<Item = Rc<VirtualNode>> {
    let hardware = state.hardware.clone_data();
    let mode_mutable = state.mode.clone();

    map_ref! {
        let mode = state.mode.borrow().signal(),
        let _ = state.hardware.trigger.signal() => move {
            let disabled1 = match mode {
                Mode::Paused => false,
                Mode::Running => true,
            };
            let disabled2 = !disabled1;

            // Note: Due to a bug in percy, we have to manually write out the onclick twice
            // (onclicks don't get updated by the vdom)
            fn run_button(hardware: Rc<RefCell<Hardware>>, children: VirtualNode, switch_to: Mode, disabled: bool, mode_mutable: Rc<RefCell<Mutable<Mode>>>) -> VirtualNode {
                if disabled {
                        html! { <button disabled="" onclick=
                            move |_e: MouseEvent| {
                                let mode_mutable = mode_mutable.clone();
                                {
                                    let mode_borrow = mode_mutable.borrow_mut();
                                    let mut lock = mode_borrow.lock_mut();
                                    *lock = switch_to;
                                    hardware.borrow_mut().paused = match switch_to {
                                        Mode::Paused => true,
                                        Mode::Running => false,
                                    };
                                }
                                log("Hit the toggle on it");
                            }> { children } </button> }
                } else {
                        html! {
                            <button onclick=move |_e: MouseEvent| {
                                let mode_mutable = mode_mutable.clone();
                                {
                                    let mode_borrow = mode_mutable.borrow_mut();
                                    let mut lock = mode_borrow.lock_mut();
                                    *lock = switch_to;
                                    hardware.borrow_mut().paused = match switch_to {
                                        Mode::Paused => true,
                                        Mode::Running => false,
                                    };
                                }
                                log("Hit the toggle on it");
                            }>
                            { children }
                                </button>
                        }
                }
            }

            fn step_button(hardware: Rc<RefCell<Hardware>>, children: VirtualNode, disabled: bool) -> VirtualNode {
                if disabled {
                    html! {
                        <button disabled="" onclick=move |_: MouseEvent| {
                            hardware.borrow_mut().step();
                            log("Stepping once");
                        }>{children}</button>
                    }
                } else {
                    html! {
                        <button onclick=move |_: MouseEvent| {
                            hardware.borrow_mut().step();
                            log("Stepping once");
                        }>{children}</button>
                    }
                }
            }

            fn repaint_button(hardware: Rc<RefCell<Hardware>>, children: VirtualNode) -> VirtualNode {
                html! {
                <button onclick=move |_: MouseEvent| {
                    hardware.borrow_mut().force_repaint();
                    log("Force repainting");
                }>
                {children}
                </button>
                }
            }

            let instrs: Vec<VirtualNode> = {
                let ip_addr = hardware.borrow().cpu.ip.0;
                let mut new_ip = InstrPointer(ip_addr);

                vec![
                    new_ip.read(&hardware.borrow().cpu.memory),
                    new_ip.read(&hardware.borrow().cpu.memory),
                    new_ip.read(&hardware.borrow().cpu.memory),
                    new_ip.read(&hardware.borrow().cpu.memory),
                    new_ip.read(&hardware.borrow().cpu.memory),
                ].into_iter().map(|i| {
                    html!{ <li> { format!("{:}", i) } </li> }
                }).collect()
            };

            Rc::new(
            html! {
                <div>
                { run_button(hardware.clone(), html! { Run }, Mode::Running, disabled1, mode_mutable.clone()) }
                { run_button(hardware.clone(), html! { Pause }, Mode::Paused, disabled2, mode_mutable.clone()) }
                { step_button(hardware.clone(), html! { Step }, disabled1) }
                { repaint_button(hardware.clone(), html! { Repaint }) }
                <ol style="font-family: PragmataPro, monospace;">
                //{ if disabled1 { vec![VirtualNode::text("")] } else { instrs } }
                { instrs }
                </ol>

                </div>
            })
        }
    }
}
