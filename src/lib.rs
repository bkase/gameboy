use std::cell::RefCell;
use std::rc::Rc;

extern crate console_error_panic_hook;
extern crate wasm_bindgen;

#[cfg(test)]
pub mod test {
    pub extern crate proptest;
}

use wasm_bindgen::prelude::*;
use wasm_bindgen::{JsCast, Clamped};

mod register_kind;
mod instr;
mod register;
mod cpu;
mod mem;
mod utils;

fn window() -> web_sys::Window {
    web_sys::window().expect("no global `window` exists")
}

fn request_animation_frame(f: &Closure<FnMut()>) {
    window()
        .request_animation_frame(f.as_ref().unchecked_ref())
        .expect("should register `requestAnimationFrame` OK");
}

fn document() -> web_sys::Document {
    window()
        .document()
        .expect("should have a document on window")
}

fn draw(data: &mut Vec<u8>, width: u32, height: u32, i: u32) {
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


#[wasm_bindgen]
extern "C" {
    // Use `js_namespace` here to bind `console.log(..)` instead of just
    // `log(..)`
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);

    #[wasm_bindgen(js_namespace = performance, js_name = now)]
    fn performance_now() -> f64;
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

    let _x = instr::Instr::Ld;

    let width = canvas.width() as u32;
    let height = canvas.height() as u32;
    log(&format!("Height {} and width {}", height, width));

    let ctx = canvas
        .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into::<web_sys::CanvasRenderingContext2d>()
        .unwrap();

    // Here we want to call `requestAnimationFrame` in a loop, but only a fixed
    // number of times. After it's done we want all our resources cleaned up. To
    // achieve this we're using an `Rc`. The `Rc` will eventually store the
    // closure we want to execute on each frame, but to start out it contains
    // `None`.
    //
    // After the `Rc` is made we'll actually create the closure, and the closure
    // will reference one of the `Rc` instances. The other `Rc` reference is
    // used to store the closure, request the first frame, and then is dropped
    // by this function.
    //
    // Inside the closure we've got a persistent `Rc` reference, which we use
    // for all future iterations of the loop
    let f = Rc::new(RefCell::new(None));
    let g = f.clone();

    let mut i = 0;
    let mut data = Vec::new();
    for _ in 0 .. height {
        for _ in 0 .. width {
            for _ in 0 .. 4 {
                data.push(0);
            }
        }
    }
    let mut last = performance_now();
    *g.borrow_mut() = Some(Closure::wrap(Box::new(move || {
        if i > 500 {
            // body().set_text_content(Some("All done!"));

            // Drop our handle to this closure so that it will get cleaned
            // up once we return.
            let _ = f.borrow_mut().take();
            let m = mem::Memory::create();
            let _ = mem::Memory::ld8(&m, mem::Addr::directly(0xff00));
            return;
        }

        let now = performance_now();
        let diff = now - last;
        last = now;

        draw(&mut data, width, height, i);
        let data = web_sys::ImageData::new_with_u8_clamped_array_and_sh(Clamped(&mut data), width, height).expect("u8 clamped array");
        ctx.put_image_data(&data, 0.0, 0.0).expect("put_image_data");
        ctx.set_font("bold 12px Monaco");
        ctx.fill_text(&format!("FPS {}", (1000.0 / diff).ceil()), 10.0, 50.0).expect("fill_text");

        // Set the body's text content to how many times this
        // requestAnimationFrame callback has fired.
        i += 1;

        // Schedule ourself for another requestAnimationFrame callback.
        request_animation_frame(f.borrow().as_ref().unwrap());
    }) as Box<FnMut()>));

    request_animation_frame(g.borrow().as_ref().unwrap());
    Ok(())
}

