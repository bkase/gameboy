extern crate console_error_panic_hook;
extern crate wasm_bindgen;

extern crate packed_struct;
#[macro_use] extern crate packed_struct_codegen;

#[cfg(test)]
pub mod test {
    pub extern crate proptest;
}

mod register_kind;
mod instr;
mod register;
mod ppu;
mod alu;
mod cpu;
mod mem;
mod utils;
mod tile_debug;
mod web_utils;

use std::cell::RefCell;
use std::rc::Rc;

use wasm_bindgen::prelude::*;
use wasm_bindgen::{JsCast, Clamped};

use web_utils::*;

fn draw_frame(data: &mut Vec<u8>, width: u32, height: u32, i: u32) {
    for row in 0..height {
        for col in 0..width {
            let idx : usize = (row*width*4 + col*4) as usize;
            data[idx+0] = (i + row + col) as u8;
            data[idx+1] = (i + row + col) as u8;
            data[idx+2] = (i + row + col) as u8;
            data[idx+3] = 255;
        }
    }
}

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
    let mut data = vec![0; (height*width*4) as usize];

    let mut last = performance_now();
    *g.borrow_mut() = Some(Closure::wrap(Box::new(move || {
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
        let data = web_sys::ImageData::new_with_u8_clamped_array_and_sh(Clamped(&mut data), width, height).expect("u8 clamped array");
        ctx.put_image_data(&data, 0.0, 0.0).expect("put_image_data");

        // Show fps
        ctx.set_font("bold 12px Monaco");
        ctx.fill_text(&format!("FPS {}", (1000.0 / diff).ceil()), 10.0, 50.0).expect("fill_text");

        // Increment once per call
        i += 1;

        // Schedule ourself for another requestAnimationFrame callback.
        request_animation_frame(f.borrow().as_ref().unwrap());
    }) as Box<FnMut()>));

    request_animation_frame(g.borrow().as_ref().unwrap());
    Ok(())
}

