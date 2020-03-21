use hardware::Hardware;
use mem::{JoypadButton, JoypadDpad, JoypadKey};
use moxie_dom::{
    elements::{button, canvas, div},
    prelude::*,
};
use ppu;
use std::cell::RefCell;
use std::rc::Rc;
use std::string::String;
use web_sys::AudioContext;

use web_utils::log;

fn button_of_key(key: &String) -> Option<JoypadKey> {
    log(&format!("Button of key: {:}", key));
    if key == "Enter" {
        Some(JoypadKey::Button(JoypadButton::Start))
    } else if key == "a" {
        Some(JoypadKey::Button(JoypadButton::A))
    } else if key == "ArrowLeft" {
        Some(JoypadKey::Dpad(JoypadDpad::Left))
    } else if key == "ArrowRight" {
        Some(JoypadKey::Dpad(JoypadDpad::Right))
    } else if key == "ArrowUp" {
        Some(JoypadKey::Dpad(JoypadDpad::Up))
    } else if key == "ArrowDown" {
        Some(JoypadKey::Dpad(JoypadDpad::Down))
    } else {
        None
    }
}

#[topo::nested]
#[illicit::from_env(hardware: &Key<Rc<RefCell<Hardware>>>)]
pub fn gameboy_view(audio_ctx: Rc<AudioContext>) {
    let hardware1 = hardware.clone();
    let on_keydown = move |keypress: event::KeyDown| {
        let key = button_of_key(&keypress.key());
        key.iter()
            .for_each(|k| hardware1.borrow_mut().cpu.memory.joypad.handle_key_down(*k));
    };
    let hardware2 = hardware.clone();
    let on_keyup = move |keypress: event::KeyUp| {
        let key = button_of_key(&keypress.key());
        key.iter()
            .for_each(|k| hardware2.borrow_mut().cpu.memory.joypad.handle_key_up(*k));
    };

    let hardware3 = hardware.clone();
    let on_click = move |_: event::Click| {
        if !ppu::DEBUG {
            hardware3.borrow_mut().paused = false;
            log("Starting to play");
        } else {
        }
    };

    let audio_ctx = audio_ctx.clone();
    let on_enable_sound = move |_: event::Click| {
        log(&format!("State: {:?}", audio_ctx.state()));
        let r = audio_ctx.resume();
        log(&format!("State: {:?}, R: {:?}", audio_ctx.state(), r));
    };

    mox! {
           <div>
           { if !ppu::DEBUG {
    mox! {
               <button on={on_enable_sound}>
                   { text("Enable Sound") }
               </button>
               } } }
              <canvas width="160" height="144" id="canvas" style="width: 100%;image-rendering: pixelated;" on={on_keydown} on={on_keyup} on={on_click} tabIndex="0"></canvas>
          </div>
       };
}
