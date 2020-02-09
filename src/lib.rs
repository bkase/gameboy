#![feature(proc_macro_hygiene)]
#![feature(track_caller)]
#![feature(exclusive_range_pattern)]
#![recursion_limit = "512"]

extern crate console_error_panic_hook;
extern crate js_sys;
#[macro_use]
extern crate topo;
extern crate moxie;
#[macro_use]
extern crate moxie_dom;
extern crate packed_struct;
extern crate wasm_bindgen;
extern crate web_sys;
#[macro_use]
extern crate packed_struct_codegen;

#[cfg(test)]
pub mod test {
    pub extern crate proptest;
}

mod alu;
mod cpu;
mod cpu_control_view;
mod hardware;
mod instr;
mod mem;
mod mem_view;
mod monoid;
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

use hardware::Hardware;
use std::cell::RefCell;
use std::rc::Rc;
use wasm_bindgen::prelude::*;
use wasm_bindgen::{Clamped, JsCast};
use web_utils::*;

#[derive(Debug, Clone)]
struct CanvasInfo {
    width: u32,
    height: u32,
    ctx: web_sys::CanvasRenderingContext2d,
}

fn init_graphics() -> CanvasInfo {
    log("Init graphics");
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

    CanvasInfo { width, height, ctx }
}

// This function is automatically invoked after the wasm module is instantiated.
#[wasm_bindgen(start)]
pub fn run() -> Result<(), JsValue> {
    utils::set_panic_hook();

    // For now hardware has it's own memory
    let hardware = Rc::new(RefCell::new(Hardware::create()));

    // Note: Safari refuses to play any audio unless it's resumed from a
    //       callstack originating at a button press, so we also hook it into
    //       the "run" button press in our GUI.
    let audio_ctx = Rc::new(web_sys::AudioContext::new().unwrap());

    let primary = audio_ctx.create_oscillator().unwrap();
    let gain = audio_ctx.create_gain().unwrap();
    primary.set_type(web_sys::OscillatorType::Square);
    primary.frequency().set_value(440.0); // A4 note
    gain.gain().set_value(0.0); // starts muted
    primary.connect_with_audio_node(&gain).unwrap();
    gain.connect_with_audio_node(&audio_ctx.destination())
        .unwrap();
    primary.start().unwrap();

    {
        use cpu_control_view::cpu_control_view;
        use mem_view::mem_view;
        use moxie_dom::{
            elements::{canvas, div},
            prelude::*,
        };
        use reg_view::reg_view;
        let moxie_root = document().get_element_by_id("moxie").unwrap();
        let mut i = 0;
        let mut last = performance.now();
        moxie_dom::embed::WebRuntime::new(moxie_root, move || {
            let hardware_ = state(|| hardware.clone());

            let mode_ = Rc::new(RefCell::new(cpu_control_view::Mode::Paused));

            let audio_ctx_ = audio_ctx.clone();

            illicit::child_env![
                Key<Rc<RefCell<Hardware>>> => hardware_
            ].enter(|| {
                topo::call(|| {

                    mox! {
                        <div class="mw8 ph4 mt2">
                            <div class="flex">
                                <div class="mw6 w-100">
                                    <canvas width="160" height="144" id="canvas" style="width: 100%;image-rendering: pixelated;"></canvas>
                                </div>
                                <div class="mw4">
                                    <cpu_control_view _=(audio_ctx_, mode_) />
                                </div>
                            </div>
                            <div class="mw5 mt2">
                                <reg_view />
                            </div>
                            <div class="mw7 mt2">
                                <mem_view _=(0x9910, 0x9910) />
                            </div>
                            <div class="mw7 mt2">
                                <mem_view _=(0x8080, 0x8080) />
                            </div>
                        </div>
                    };

                    let info = once(|| init_graphics());
                    let ctx = info.ctx;
                    let width = info.width;
                    let height = info.height;

                    // Measure time delta
                    let now = performance.now();
                    let diff = now - last;
                    last = now;

                    // Execute our hardware!
                    {
                        hardware.borrow_mut().run(diff);
                    }
                    {
                        hardware.borrow_mut().dirty = false;
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
            })});
        })
        .animation_frame_scheduler()
        .run_on_every_frame();
    }

    Ok(())
}
