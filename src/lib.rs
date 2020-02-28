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
mod breakpoints_view;
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

#[derive(Debug, Clone)]
struct Canvi {
    real: CanvasInfo,
    full_debug: CanvasInfo,
    tile_debug: CanvasInfo,
}

fn setup_canvas(id: &str, asserted_width: u32, asserted_height: u32) -> CanvasInfo {
    let canvas = document().get_element_by_id(id).unwrap();
    let canvas: web_sys::HtmlCanvasElement = canvas
        .dyn_into::<web_sys::HtmlCanvasElement>()
        .map_err(|_| ())
        .unwrap();

    let width = canvas.width() as u32;
    let height = canvas.height() as u32;
    // gameboy resolution
    assert_eq!(width, asserted_width);
    assert_eq!(height, asserted_height);

    let ctx = canvas
        .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into::<web_sys::CanvasRenderingContext2d>()
        .unwrap();

    CanvasInfo { width, height, ctx }
}

fn init_graphics() -> Canvi {
    log("Init graphics");
    let full_debug = setup_canvas("debug-canvas", 256, 256);
    let real = setup_canvas("canvas", 160, 144);
    let tile_debug = setup_canvas("tile-canvas", 16 * 8, 24 * 8);

    Canvi {
        real,
        full_debug,
        tile_debug,
    }
}

fn blit_bytes(
    hardware: &mut Hardware,
    ctx: &mut web_sys::CanvasRenderingContext2d,
    screen_choice: ppu::ScreenChoice,
) {
    let data = match screen_choice {
        ppu::ScreenChoice::Real => web_sys::ImageData::new_with_u8_clamped_array_and_sh(
            Clamped(&mut hardware.ppu.screen.data),
            hardware.ppu.screen.width,
            hardware.ppu.screen.height,
        ),
        ppu::ScreenChoice::FullDebug => web_sys::ImageData::new_with_u8_clamped_array_and_sh(
            Clamped(&mut hardware.ppu.debug_wide_screen.data),
            hardware.ppu.debug_wide_screen.width,
            hardware.ppu.debug_wide_screen.height,
        ),
        ppu::ScreenChoice::TileDebug => web_sys::ImageData::new_with_u8_clamped_array_and_sh(
            Clamped(&mut hardware.ppu.debug_tile_screen.data),
            hardware.ppu.debug_tile_screen.width,
            hardware.ppu.debug_tile_screen.height,
        ),
    }
    .expect("u8 clamped array");
    ctx.put_image_data(&data, 0.0, 0.0).expect("put_image_data");
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
        use breakpoints_view::breakpoints_view;
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
                                <div class="mw5">
                                    <cpu_control_view _=(audio_ctx_, mode_) />
                                    <breakpoints_view />
                                    <canvas width="128" height="192" id="tile-canvas" style="width: 100%;image-rendering: pixelated;"></canvas>
                                </div>
                            </div>
                            <div class="mw6 w-100">
                                <canvas width="256" height="256" id="debug-canvas" style="width: 100%;image-rendering: pixelated;"></canvas>
                            </div>
                            <div class="mw5 mt2">
                                <reg_view />
                            </div>
                            <div class="mw7 mt2">
                                <mem_view _=(0x9800, 0x9800) />
                            </div>
                            <div class="mw7 mt2">
                                <mem_view _=(0xfe80, 0xfe80) />
                            </div>
                        </div>
                    };

                    let mut canvi = once(|| init_graphics());

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
                    {
                    blit_bytes(&mut hardware.borrow_mut(), &mut canvi.real.ctx, ppu::ScreenChoice::Real);
                    }

                    {
                    blit_bytes(&mut hardware.borrow_mut(), &mut canvi.full_debug.ctx, ppu::ScreenChoice::FullDebug);
                    }

                    {
                    blit_bytes(&mut hardware.borrow_mut(), &mut canvi.tile_debug.ctx, ppu::ScreenChoice::TileDebug);
                    }

                    // Draw extra debug graphics
                    canvi.full_debug.ctx.set_stroke_style(&JsValue::from_str("green"));
                    canvi.full_debug.ctx.stroke_rect(
                        hardware.borrow().cpu.memory.ppu.scx.into(),
                        hardware.borrow().cpu.memory.ppu.scy.into(),
                        canvi.real.width.into(),
                        canvi.real.height.into(),
                        );


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
                    canvi.real.ctx.set_font("bold 12px Monaco");
                    canvi.real.ctx.fill_text(&format!("FPS {}", fps), 10.0, 50.0)
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
