use std::cell::RefCell;
use std::rc::Rc;

extern crate console_error_panic_hook;
extern crate wasm_bindgen;

extern crate packed_struct;
#[macro_use] extern crate packed_struct_codegen;


#[cfg(test)]
pub mod test {
    pub extern crate proptest;
}

use wasm_bindgen::prelude::*;
use wasm_bindgen::{JsCast, Clamped};

mod register_kind;
mod instr;
mod register;
mod ppu;
mod alu;
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

use packed_struct::prelude::*;

// 8x2 -> 2x8
//
// 
fn bit_transpose(x: u8, y: u8) -> [u8; 8] {
    let p0 = ((x & (1 << 7)) >> 6) | ((y & (1 << 7)) >> 7);
    let p1 = ((x & (1 << 6)) >> 5) | ((y & (1 << 6)) >> 6);
    let p2 = ((x & (1 << 5)) >> 4) | ((y & (1 << 5)) >> 5);
    let p3 = ((x & (1 << 4)) >> 3) | ((y & (1 << 4)) >> 4);
    let p4 = ((x & (1 << 3)) >> 2) | ((y & (1 << 3)) >> 3);
    let p5 = ((x & (1 << 2)) >> 1) | ((y & (1 << 2)) >> 2);
    let p6 = ((x & (1 << 1)) >> 0) | ((y & (1 << 1)) >> 1);
    let p7 = ((x & (1 << 0)) << 1) | ((y & (1 << 0)) >> 0);
    log(&format!("p0 {}, p1 {}, p2 {}, p3 {}, p4 {}, p5 {}, p6 {}, p7 {}", p0, p1, p2, p3, p4, p5, p6, p7));
    [p0, p1, p2, p3,
     p4, p5, p6, p7]
}

#[derive(PackedStruct, Debug)]
#[packed_struct(size_bytes="1", bit_numbering="lsb0")]
pub struct Pixels {
    #[packed_field(bits="0:1")]
    pub p4: Integer<u8, packed_bits::Bits2>,
    #[packed_field(bits="2:3")]
    pub p3: Integer<u8, packed_bits::Bits2>,
    #[packed_field(bits="4:5")]
    pub p2: Integer<u8, packed_bits::Bits2>,
    #[packed_field(bits="6:7")]
    pub p1: Integer<u8, packed_bits::Bits2>,
}


fn draw_tiles(data: &mut Vec<u8>, width: u32, pallette: ppu::Palette, tiles: Vec<[u8; 16]>) {
    // TODO: figure out iterators
    fn pixels(bs: [u8; 16]) -> Vec<u8> {
        let mut out = Vec::with_capacity(64);

        for arr in bs.to_vec().chunks(2) {
            let mut transpose = bit_transpose(arr[0], arr[1]).to_vec();
            log(&format!("For b0 {}, b1 {}", arr[0], arr[1]));
            out.append(&mut transpose);
        }

        out
    }

    let pixel_lut: [(u8, u8, u8); 4] =
        [ (0, 0, 0)
        , (220, 176, 181)
        , (98, 78, 81)
        , (255, 255, 255) ];

    let mut row = 0;
    let mut col = 0;
    for tile in tiles {
        for pixel in pixels(tile) {
            let idx : u8 =
                match pixel {
                    0b00 => pallette.dot00,
                    0b01 => pallette.dot01,
                    0b10 => pallette.dot10,
                    0b11 => pallette.dot11,
                        _ => panic!("unexpected")
                }.into();
            let (r, g, b) = pixel_lut[idx as usize];
            let i = (row*width*4 + col*4) as usize;
            data[i+0] = r;
            data[i+1] = g;
            data[i+2] = b;
            data[i+3] = 255;
            // update
            log(&format!("Drawing {} at row:{}, col: {}", idx, row, col));
            col += 1;
            if col == 8 {
                row += 1;
                col = 0;
            }
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

    // tiles
    let tiles_canvas = document().get_element_by_id("tiles").unwrap();
    let tiles_canvas: web_sys::HtmlCanvasElement = tiles_canvas
        .dyn_into::<web_sys::HtmlCanvasElement>()
        .map_err(|_| ())
        .unwrap();
    let tiles_ctx = tiles_canvas
        .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into::<web_sys::CanvasRenderingContext2d>()
        .unwrap();
    let tiles_width = tiles_canvas.width() as u32;
    let tiles_height = tiles_canvas.height() as u32;

    let mut data_ = Vec::new();
    for _ in 0 .. tiles_height {
        for _ in 0 .. tiles_width {
            for _ in 0 .. 4 {
                data_.push(128);
            }
        }
    }
    // Nintendo logo tiles from bootrom
    /*let tiles: Vec<[u8; 8]> =
        vec![[0xCE,0xED,0x66,0x66,0xCC,0x0D,0x00,0x0B],[0x03,0x73,0x00,0x83,0x00,0x0C,0x00,0x0D],
             [0x00,0x08,0x11,0x1F,0x88,0x89,0x00,0x0E],[0xDC,0xCC,0x6E,0xE6,0xDD,0xDD,0xD9,0x99],
             [0xBB,0xBB,0x67,0x63,0x6E,0x0E,0xEC,0xCC],[0xDD,0xDC,0x99,0x9F,0xBB,0xB9,0x33,0x3E]];

    fn decompress(tiles: &[u8; 8]) -> [u8; 16] {
        fn double_bits(x: u8) -> (u8, u8) {
            let b7 = (x & (1 << 7)) >> 7;
            let b6 = (x & (1 << 6)) >> 6;
            let b5 = (x & (1 << 5)) >> 5;
            let b4 = (x & (1 << 4)) >> 4;
            let b3 = (x & (1 << 3)) >> 3;
            let b2 = (x & (1 << 2)) >> 2;
            let b1 = (x & (1 << 1)) >> 1;
            let b0 = x & 1;

            ( (b7 << 7) | (b7 << 6) | (b6 << 5) | (b6 << 4) | (b5 << 3) | (b5 << 2) | (b4 << 1) | b4
            , (b3 << 7) | (b3 << 6) | (b2 << 5) | (b2 << 4) | (b1 << 3) | (b1 << 2) | (b0 << 1) | b0)
        }

        let mut ret = [0,0,0,0,
                       0,0,0,0,
                       0,0,0,0,
                       0,0,0,0];
        let mut i = 0;
        for tile in tiles.iter() {
            let (o1, o2) = (*tile, *tile);//double_bits(*tile);
            ret[i] = o1;
            ret[i+1] = o2;
            i += 2;
            log(&format!("In {} \n Out {} {}", tile, o1, o2));
        }

        ret
    }*/

    let sample_tile =
        vec![[0x02, 0xFF, 0x73, 0x8c, 0x65, 0x9a, 0x48, 0xb6, 0x12, 0xec, 0x26, 0xd8, 0xce, 0xb0, 0x60, 0x80]];

    /*let pokemon_house =
        vec![[0xFF, 0x00, 0x7E, 0xFF, 0x85, 0x81, 0x89, 0x83, 0x93, 0x85, 0xA5, 0x8B, 0xC9, 0x97, 0x7E, 0xFF]];*/

    draw_tiles(&mut data_, tiles_width as u32, ppu::Palette::create(), sample_tile);
    let imgdata_ = web_sys::ImageData::new_with_u8_clamped_array_and_sh(Clamped(&mut data_), tiles_width, tiles_height).expect("u8 clamped array");
    tiles_ctx.put_image_data(&imgdata_, 0.0, 0.0).expect("put_image_data");

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

