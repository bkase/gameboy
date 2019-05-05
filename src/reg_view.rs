#![allow(dead_code)]

use futures_signals::map_ref;
use futures_signals::signal::Signal;
use hardware::Hardware;
use mutable_effect::MutableEffect;
use ppu::ReadViewU8;
use register::Flags;
use register::{R16, R8};
use std::cell::RefCell;
use std::rc::Rc;
use virtual_dom_rs::prelude::*;

pub struct State {
    pub hardware: Rc<MutableEffect<Rc<RefCell<Hardware>>>>,
}

pub fn component(state: State) -> impl Signal<Item = Rc<VirtualNode>> {
    let hardware = state.hardware.clone_data();

    map_ref! {
        let _ = state.hardware.trigger.signal() => move {
            let ppu_regs_hardware = hardware.clone();
            let ppu_regs = &ppu_regs_hardware.borrow().cpu.memory.ppu;
            let registers = &hardware.borrow().cpu.registers;

            fn draw_r16(name: &'static str, value: R16) -> VirtualNode {
                html! {
                <tr>
                  <th> { name } </th>
                  <td> { format!("${:04x}", value.0) } </td>
                </tr>
                }
            }
            fn draw_r8(name: &'static str, value: R8) -> VirtualNode {
                html! {
                <tr>
                  <th> { name } </th>
                  <td> { format!("${:02x}", value.0) } </td>
                </tr>
                }
            }
            fn draw_flags(name: &'static str, flags: &Flags) -> VirtualNode {
                html! {
                <tr>
                  <th> { name } </th>
                  <td> { format!("z={:} n={:} h={:} c={:}", flags.z, flags.n, flags.h, flags.c) } </td>
                </tr>
                }
            }

            Rc::new(
            html! {
        <div style="font-family: PragmataPro, monospace;">
            <table>
                <thead>
                    <tr>
                        <th> { "Registers" }  </th>
                    </tr>
                </thead>
                <tbody>
                    { draw_r16("bc", registers.bc) }
                    { draw_r16("de", registers.de) }
                    { draw_r16("hl", registers.hl) }
                    { draw_r16("sp", registers.sp) }
                    { draw_r8("a", registers.a) }
                    { draw_flags("flags", &registers.flags) }
                    { draw_r8("scx", R8(ppu_regs.scx)) }
                    { draw_r8("scy", R8(ppu_regs.scy)) }
                    { draw_r8("lcdc", R8(ppu_regs.lcdc.read())) }
                </tbody>
            </table>
        </div>
            })
        }
    }
}
