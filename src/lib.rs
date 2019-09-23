// hello world

#![feature(proc_macro_hygiene)]
#![feature(exclusive_range_pattern)]

extern crate console_error_panic_hook;
extern crate css_rs_macro;
extern crate futures;
extern crate futures_signals;
extern crate futures_util;
extern crate js_sys;
// #[macro_use]
// extern crate topo;
// extern crate moxie;
// #[macro_use]
// extern crate moxie_dom;
extern crate packed_struct;
extern crate virtual_dom_rs;
extern crate wasm_bindgen;
extern crate web_sys;
#[macro_use]
extern crate packed_struct_codegen;

#[cfg(test)]
pub mod test {
    pub extern crate proptest;
}

mod alu;
mod app;
mod cpu;
mod cpu_control_view;
mod debug_gui;
mod future_driver;
mod game;
mod hack_vdom;
mod hardware;
mod instr;
mod mem;
mod mem_view;
mod monoid;
mod mutable_effect;
mod ppu;
mod read_view_u8;
mod reg_view;
mod register;
mod register_kind;
mod screen;
mod sound;
mod tile_debug;
mod utils;
mod web_utils;

use futures::task::Poll;
use futures_signals::signal::Mutable;
use hardware::Hardware;
use mutable_effect::MutableEffect;
use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::prelude::*;
use wasm_bindgen::{Clamped, JsCast};
use web_utils::*;

// This function is automatically invoked after the wasm module is instantiated.
#[wasm_bindgen(start)]
pub fn run() -> Result<(), JsValue> {
    utils::set_panic_hook();

    // Rc is used to do a closure dance so we can stop the
    // request_animation_frame loop
    let f = Rc::new(RefCell::new(None));
    let g = f.clone();

    // For now hardware has it's own memory
    let hardware = Rc::new(RefCell::new(Hardware::create()));
    // mem is for GUI
    let trigger = Mutable::new(());
    let hardware_effect = MutableEffect {
        state: hardware.clone(),
        trigger: trigger.read_only(),
    };

    // Note: Safari refuses to play any audio unless it's resumed from a
    //       callstack originating at a button press, so we also hook it into
    //       the "run" button press in our GUI.
    let audio_ctx = Rc::new(web_sys::AudioContext::new().unwrap());

    let app_state: app::AppState = app::AppState {
        globals: app::Globals {
            unit: Mutable::new(()),
            frames: Mutable::new(0),
        },
        hardware: Rc::new(hardware_effect),
        mem_view_state: vec![
            mem_view::LocalState {
                focus: Rc::new(RefCell::new(Mutable::new(0x9910))),
                cursor: Rc::new(RefCell::new(Mutable::new(0x9910))),
            },
            mem_view::LocalState {
                focus: Rc::new(RefCell::new(Mutable::new(0x8080))),
                cursor: Rc::new(RefCell::new(Mutable::new(0x8080))),
            },
        ],
        cpu_control_view_state: Rc::new(RefCell::new(Mutable::new(cpu_control_view::Mode::Paused))),
        cpu_control_view_audio_ctx: audio_ctx.clone(),
    };
    let signal_future = Rc::new(RefCell::new(app::run(&app_state)));
    // trigger the initial render
    let _ = future_driver::tick(signal_future.clone());

    let canvas = document().get_element_by_id("canvas").unwrap();
    let canvas: web_sys::HtmlCanvasElement = canvas
        .dyn_into::<web_sys::HtmlCanvasElement>()
        .map_err(|_| ())
        .unwrap();

    /* TODO: Moxie
    let moxie_cell = Rc::new(RefCell::new(None));
    {
        use moxie_dom::{prelude::*, *};
        let moxie_root = document().get_element_by_id("moxie").unwrap();
        moxie_dom::boot(moxie_root, move || {
            let i = state!(|| 0);

            let count = state!(|| 0);

            text!(&format!("Hello world {:}", count));
            text!(&format!("Raf {:}", i));

            element!("button", |e| e
                .attr("type", "button")
                .on(|_: ClickEvent, count| Some(count + 1), count)
                .inner(|| text!("increment")));

            let closure: Closure<dyn FnMut()> = Closure::new(move || {
                let _ = i.update(|i| Some(i + 1));
            });
            *moxie_cell.borrow_mut() = Some(closure);
            window().set_timeout_with_callback_and_timeout_and_arguments_0(
                moxie_cell
                    .borrow()
                    .as_ref()
                    .unwrap()
                    .as_ref()
                    .unchecked_ref(),
                0,
            );
        });
    } */

    let width = canvas.width() as u32;
    let height = canvas.height() as u32;
    // gameboy resolution
    assert_eq!(width, 160);
    assert_eq!(height, 144);

    let ctx = canvas
        .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into::<web_sys::CanvasRenderingContext2d>()
        .unwrap();

    let primary = audio_ctx.create_oscillator().unwrap();
    let gain = audio_ctx.create_gain().unwrap();
    primary.set_type(web_sys::OscillatorType::Square);
    primary.frequency().set_value(440.0); // A4 note
    gain.gain().set_value(0.0); // starts muted
    primary.connect_with_audio_node(&gain).unwrap();
    gain.connect_with_audio_node(&audio_ctx.destination())
        .unwrap();
    primary.start().unwrap();

    let mut i = 0;
    let mut last = performance.now();
    let closure = move || {
        {
            // Change the state
            let mut lock = app_state.globals.frames.lock_mut();
            *lock = i;
        }

        // Stop after 50,000 frames
        if i > 50_000 {
            // Drop our handle to this closure so that it will get cleaned
            // up once we return.
            let _ = f.borrow_mut().take();
            audio_ctx.close().unwrap();
            return;
        }

        // Measure time delta
        let now = performance.now();
        let diff = now - last;
        last = now;

        // Execute our hardware!
        {
            hardware.borrow_mut().run(diff);
        }
        let dirty = { hardware.borrow().dirty };
        {
            hardware.borrow_mut().dirty = false;
            if dirty {
                // Trigger hardware changes
                let mut lock = trigger.lock_mut();
                *lock = ();
            }
        }

        // Blit bytes
        let data = web_sys::ImageData::new_with_u8_clamped_array_and_sh(
            Clamped(&mut hardware.borrow_mut().ppu.screen.data),
            width,
            height,
        )
        .expect("u8 clamped array");
        ctx.put_image_data(&data, 0.0, 0.0).expect("put_image_data");

        // play sound
        match hardware.borrow().sound.audio {
            None => {
                gain.gain().set_value(0.0);
            }
            Some(ref audio) => {
                log(&format!(
                    "Settings audio gain: {:?}, frequency: {:?}",
                    audio.channel.gain, audio.channel.frequency
                ));
                gain.gain().set_value(audio.channel.gain);
                primary.frequency().set_value(audio.channel.frequency);
            }
        }

        // Show fps
        let fps = (1000.0 / diff).ceil();
        ctx.set_font("bold 12px Monaco");
        ctx.fill_text(&format!("FPS {}", fps), 10.0, 50.0)
            .expect("fill_text");

        // Increment once per call
        i += 1;

        // Drive our GUI
        match future_driver::tick(signal_future.clone()) {
            Poll::Pending => (),
            Poll::Ready(()) => panic!("The signal should never end!"),
        };

        // Schedule ourself for another requestAnimationFrame callback.
        request_animation_frame(f.borrow().as_ref().unwrap());
    };
    *g.borrow_mut() = Some(Closure::wrap(Box::new(closure) as Box<dyn FnMut()>));

    request_animation_frame(g.borrow().as_ref().unwrap());

    Ok(())
}
