use hardware::Hardware;
use instr::InstrPointer;
use moxie_dom::{
    elements::{button, div, li, ol, p},
    prelude::*,
};
use ppu;
use std::cell::RefCell;
use std::rc::Rc;
use web_sys::AudioContext;
use web_utils::log;
use web_utils::*;

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
    let performance = Performance::create();
    let vblanks = {
        let ip_addr = hardware.borrow().cpu.ip.0;
        let timer = &hardware.borrow().cpu.memory.timer;
        let mut new_ip = InstrPointer(ip_addr);

        let vblanks = hardware.borrow().vblanks;
        let start_time = hardware.borrow().start_time;
        let vblankps = vblanks as f64 / ((&performance).now() - start_time);

        mox! {
            <div>
                <p style="font-family: PragmataPro, monospace;">
                    { text(format!("ip: {:} timer: {:} vblankps: {:}", ip_addr, timer, vblankps)) }
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

        vblanks
    };

    if vblanks > 100 {
        {
            hardware.borrow_mut().vblanks = 0;
        }
        {
            hardware.borrow_mut().start_time = (&performance).now();
        }
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
       { if ppu::DEBUG { mox! { <instrs /> } } }
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
