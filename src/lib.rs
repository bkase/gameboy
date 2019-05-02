// hello world

#![feature(proc_macro_hygiene)]
#![feature(futures_api)]
#![feature(exclusive_range_pattern)]

extern crate console_error_panic_hook;
extern crate css_rs_macro;
extern crate futures;
#[macro_use]
extern crate futures_signals;
extern crate futures_util;
extern crate js_sys;
extern crate virtual_dom_rs;
extern crate wasm_bindgen;
extern crate web_sys;

extern crate packed_struct;
#[macro_use]
extern crate packed_struct_codegen;

#[cfg(test)]
pub mod test {
    pub extern crate proptest;
}

mod alu;
mod app;
mod cpu;
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
mod register;
mod register_kind;
mod screen;
mod tile_debug;
mod utils;
mod web_utils;

use futures::task::Poll;
use futures_signals::signal::Mutable;
use hardware::Hardware;
use mem::Memory;
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
    let mut hardware = Hardware::create();
    // mem is for GUI
    let mem = Rc::new(Memory::create());
    let trigger = Mutable::new(());
    let mem_effect = MutableEffect {
        state: mem,
        trigger: trigger.read_only(),
    };

    let app_state: app::AppState = app::AppState {
        globals: app::Globals {
            unit: Mutable::new(()),
            frames: Mutable::new(0),
        },
        mem: Rc::new(mem_effect),
        mem_view_state: mem_view::LocalState {
            focus: Rc::new(RefCell::new(Mutable::new(0x0040))),
            cursor: Rc::new(RefCell::new(Mutable::new(0x0064))),
        },
    };
    let signal_future = Rc::new(RefCell::new(app::run(&app_state)));
    // trigger the initial render
    let _ = future_driver::tick(signal_future.clone());

    let canvas = document().get_element_by_id("canvas").unwrap();
    let canvas: web_sys::HtmlCanvasElement = canvas
        .dyn_into::<web_sys::HtmlCanvasElement>()
        .map_err(|_| ())
        .unwrap();

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

    let mut i = 0;
    let mut last = performance_now();
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
            return;
        }

        // Measure time delta
        let now = performance_now();
        let diff = now - last;
        last = now;

        // Execute our hardware!
        hardware.run(diff);

        // Blit bytes
        let data = web_sys::ImageData::new_with_u8_clamped_array_and_sh(
            Clamped(&mut hardware.ppu.screen.data),
            width,
            height,
        )
        .expect("u8 clamped array");
        ctx.put_image_data(&data, 0.0, 0.0).expect("put_image_data");

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
    *g.borrow_mut() = Some(Closure::wrap(Box::new(closure) as Box<FnMut()>));

    request_animation_frame(g.borrow().as_ref().unwrap());

    Ok(())
}
