#![allow(dead_code)]

use futures_signals::map_ref;
use futures_signals::signal::Signal;
use hardware::Hardware;
use mem::InterruptRegister;
use mutable_effect::MutableEffect;
use read_view_u8::*;
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
            let sound_regs_hardware = hardware.clone();
            let pulse_a = &sound_regs_hardware.borrow().cpu.memory.sound.pulse_a;
            let ppu_regs = &ppu_regs_hardware.borrow().cpu.memory.ppu;
            let registers = &hardware.borrow().cpu.registers;

            let interrupt_enable = &hardware.borrow().cpu.memory.interrupt_enable;
            let interrupt_flag = &hardware.borrow().cpu.memory.interrupt_flag;

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
            fn draw_interrupt(name: &'static str, r: &InterruptRegister) -> VirtualNode {
                html! {
                <tr>
                  <th> { name } </th>
                  <td> { format!("vblank={:} lcd_stat={:} timer={:} serial={:} joypad={:}", r.vblank, r.lcd_stat, r.timer, r.serial, r.joypad) } </td>
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
                    { draw_interrupt("ie", &interrupt_enable) }
                    { draw_interrupt("id", &interrupt_flag) }
                    { draw_r8("pa:control", R8(pulse_a.control.read())) }
                    { draw_r8("pa:frequency", R8(pulse_a.frequency.read())) }
                    { draw_r8("pa:volume", R8(pulse_a.volume.read())) }
                    { draw_r8("pa:length", R8(pulse_a.length.read())) }
                    { draw_r8("pa:sweep", R8(pulse_a.sweep.read())) }
                    { draw_r8("scx", R8(ppu_regs.scx)) }
                    { draw_r8("scy", R8(ppu_regs.scy)) }
                    { draw_r8("ly", R8(ppu_regs.ly)) }
                    { draw_r8("lcdc", R8(ppu_regs.lcdc.read())) }
                </tbody>
            </table>
        </div>
            })
        }
    }
}
