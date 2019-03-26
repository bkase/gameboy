// hello world

#![feature(proc_macro_hygiene)]
#![feature(futures_api)]

extern crate console_error_panic_hook;
extern crate css_rs_macro;
extern crate futures;
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
mod cpu;
mod debug_gui;
mod futures_bridge;
mod instr;
mod mem;
mod ppu;
mod register;
mod register_kind;
mod tile_debug;
mod utils;
mod web_utils;

use std::cell::RefCell;
use std::rc::Rc;

use wasm_bindgen::prelude::*;
use wasm_bindgen::{Clamped, JsCast};

use futures::stream::Stream;
use futures::FutureExt;
use futures_bridge::spawn_local;
use futures_signals::signal::{Mutable, SignalExt, SignalFuture};

use web_utils::*;

fn draw_frame(data: &mut Vec<u8>, width: u32, height: u32, i: u32) {
    for row in 0..height {
        for col in 0..width {
            let idx: usize = (row * width * 4 + col * 4) as usize;
            data[idx] = (i + row + col) as u8;
            data[idx + 1] = (i + row + col) as u8;
            data[idx + 2] = (i + row + col) as u8;
            data[idx + 3] = 255;
        }
    }
}

/*
struct MyClosure {
    app: debug_gui::App,
    tx: mpsc::Sender<u32>,
}

impl FnMut<()> for MyClosure {
    extern "rust-call" fn call_mut(&mut self, args: ()) {
        let _ = self.app;
        let tx_ = self.tx.clone();
    }
}
impl FnOnce<()> for MyClosure {
    type Output = ();
    extern "rust-call" fn call_once(mut self, args: ()) {
        self.call_mut(args)
    }
}
*/

// This function is automatically invoked after the wasm module is instantiated.
#[wasm_bindgen(start)]
pub fn run() -> Result<(), JsValue> {
    utils::set_panic_hook();

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

    // Rc is used to do a closure dance so we can stop the
    // request_animation_frame loop
    let f = Rc::new(RefCell::new(None));
    let g = f.clone();

    let mut i = 0;
    let mut data = vec![0; (height * width * 4) as usize];

    let state: Mutable<u32> = Mutable::new(0);
    let (mut app, signal) = debug_gui::App::new(state.signal());
    //let _ = tx.send(i + 1);
    let mut signal_future = signal.to_future().map(|_| ());
    futures_bridge::spawn_local(signal_future);

    let mut last = performance_now();
    let x = move || {
        // let _ = app;
        // tx_rc.borrow_mut().send(i);
        // tx_rc.borrow_mut().send(i);
        let mut lock = state.lock_mut();
        *lock = i;

        // Stop after 500 frames
        if i > 500 {
            // Drop our handle to this closure so that it will get cleaned
            // up once we return.
            let _ = f.borrow_mut().take();
            return;
        }

        // Measure time delta
        let now = performance_now();
        let diff = now - last;
        last = now;

        draw_frame(&mut data, width, height, i);

        // Blit bytes
        let data =
            web_sys::ImageData::new_with_u8_clamped_array_and_sh(Clamped(&mut data), width, height)
                .expect("u8 clamped array");
        ctx.put_image_data(&data, 0.0, 0.0).expect("put_image_data");

        // Show fps
        ctx.set_font("bold 12px Monaco");
        ctx.fill_text(&format!("FPS {}", (1000.0 / diff).ceil()), 10.0, 50.0)
            .expect("fill_text");

        // Increment once per call
        i += 1;

        // Schedule ourself for another requestAnimationFrame callback.
        request_animation_frame(f.borrow().as_ref().unwrap());
    };
    *g.borrow_mut() = Some(Closure::wrap(Box::new(x) as Box<FnMut()>));

    request_animation_frame(g.borrow().as_ref().unwrap());

    Ok(())
}
