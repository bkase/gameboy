use hardware::Hardware;
use mem::{JoypadButton, JoypadDpad, JoypadKey};
use moxie_dom::{elements::canvas, prelude::*};
use std::cell::RefCell;
use std::rc::Rc;
use std::string::String;

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
pub fn gameboy_view() {
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

    mox! {
       <canvas width="160" height="144" id="canvas" style="width: 100%;image-rendering: pixelated;" on={on_keydown} on={on_keyup} tabIndex="0"></canvas>
    };
}
