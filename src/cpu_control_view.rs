use hardware::Hardware;
use instr::InstrPointer;
use mem::Addr;
use moxie_dom::{
    elements::{button, div, li, ol, p},
    prelude::*,
};
use std::cell::RefCell;
use std::rc::Rc;
use web_sys::AudioContext;
use web_utils::log;

#[derive(Debug, Copy, Clone)]
pub enum Mode {
    Paused,
    Running,
}

#[topo::nested]
#[illicit::from_env(hardware: &Key<Rc<RefCell<Hardware>>>, mode: &Key<Rc<RefCell<Mode>>>, audio_ctx: &Key<Rc<AudioContext>>)]
fn run_button(copy: &str, switch_to: Mode, disabled: bool) {
    let hardware = hardware.clone();
    let mode = mode.clone();
    let audio_ctx = audio_ctx.clone();

    let click_handler = move |_: event::Click| {
        {
            *mode.borrow_mut() = switch_to;
            let _ = audio_ctx.resume();
            hardware.borrow_mut().paused = match switch_to {
                Mode::Paused => true,
                Mode::Running => false,
            };
        }
        log("Hit the toggle on it");
    };
    if disabled {
        mox! {
          <button disabled="" on={click_handler}>{text(copy)}</button>
        }
    } else {
        mox! {
          <button on={click_handler}>{text(copy)}</button>
        }
    }
}

#[topo::nested]
#[illicit::from_env(hardware: &Key<Rc<RefCell<Hardware>>>)]
fn step_button(copy: &str, disabled: bool) {
    let hardware = hardware.clone();
    let click_handler = move |_: event::Click| {
        hardware.borrow_mut().step();
        log("Stepping once");
    };

    if disabled {
        mox! {
          <button disabled="" on={click_handler}>{text(copy)}</button>
        }
    } else {
        mox! {
          <button on={click_handler}>{text(copy)}</button>
        }
    }
}

#[topo::nested]
#[illicit::from_env(hardware: &Key<Rc<RefCell<Hardware>>>)]
fn repaint_button(copy: &str) {
    let hardware = hardware.clone();
    let click_handler = move |_: event::Click| {
        hardware.borrow_mut().force_repaint();
        log("Force repainting");
    };
    mox! {
        <button on={click_handler}> {text(copy)} </button>
    }
}

#[topo::nested]
#[illicit::from_env(hardware: &Key<Rc<RefCell<Hardware>>>)]
fn instr(new_ip: &mut InstrPointer) {
    let instr = new_ip.read(&hardware.borrow().cpu.memory);
    mox! { <li> { text(format!("{:}", instr)) } </li> }
}

#[topo::nested]
#[illicit::from_env(hardware: &Key<Rc<RefCell<Hardware>>>)]
fn instrs() {
    let ip_addr = hardware.borrow().cpu.ip.0;
    let timer = &hardware.borrow().cpu.memory.timer;
    let mut new_ip = InstrPointer(ip_addr);

    mox! {
        <div>
            <p style="font-family: PragmataPro, monospace;">
                { text(format!("ip: {:} timer: {:}", ip_addr, timer)) }
            </p>
            <ol style="font-family: PragmataPro, monospace;">
                <instr _=(&mut new_ip) />
                <instr _=(&mut new_ip) />
                <instr _=(&mut new_ip) />
                <instr _=(&mut new_ip) />
                <instr _=(&mut new_ip) />
            </ol>
        </div>
    }
}

#[topo::nested]
#[illicit::from_env(mode: &Key<Rc<RefCell<Mode>>>)]
fn view() {
    let disabled1 = match *(mode.borrow()) {
        Mode::Paused => false,
        Mode::Running => true,
    };
    let disabled2 = !disabled1;

    mox! {
     <div>
       <run_button _=("Run", Mode::Running, disabled1) />
       <run_button _=("Pause", Mode::Paused, disabled2) />
       <step_button _=("Step", disabled1) />
       <repaint_button _=("Repaint") />
       <instrs />
     </div>
    }
}

#[topo::nested]
pub fn cpu_control_view(audio_ctx: Rc<AudioContext>, mode: Rc<RefCell<Mode>>) {
    let audio_ctx = state(|| audio_ctx);
    let mode = state(|| mode);

    illicit::child_env![
      Key<Rc<AudioContext>> => audio_ctx,
      Key<Rc<RefCell<Mode>>> => mode
    ]
    .enter(|| topo::call(|| view()));
}
