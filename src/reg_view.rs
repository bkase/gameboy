use hardware::Hardware;
use mem::InterruptRegister;
use moxie_dom::{
    elements::{div, table, tbody, td, th, thead, tr},
    prelude::*,
};
use read_view_u8::*;
use register::Flags;
use register::{R16, R8};
use std::cell::RefCell;
use std::rc::Rc;

#[topo::nested]
fn draw_row(name: &'static str, msg: std::string::String) {
    mox! {
        <tr>
          <th> { text(name) } </th>
          <td> { text(msg) } </td>
        </tr>
    }
}

#[topo::nested]
fn draw_r16(name: &'static str, value: R16) {
    draw_row(name, format!("${:04x}", value.0))
}

#[topo::nested]
fn draw_r8(name: &'static str, value: R8) {
    draw_row(name, format!("${:02x}", value.0))
}

#[topo::nested]
fn draw_flags(name: &'static str, flags: &Flags) {
    draw_row(
        name,
        format!(
            "z={:} n={:} h={:} c={:}",
            flags.z, flags.n, flags.h, flags.c
        ),
    )
}

#[topo::nested]
fn draw_interrupt(name: &'static str, r: &InterruptRegister) {
    draw_row(
        name,
        format!(
            "vblank={:} lcd_stat={:} timer={:} serial={:} joypad={:}",
            r.vblank, r.lcd_stat, r.timer, r.serial, r.joypad
        ),
    )
}

#[topo::nested]
fn draw_bit(name: &'static str, bit: bool) {
    draw_row(name, format!("{:}", bit))
}

#[topo::nested]
#[illicit::from_env(hardware: &Key<Rc<RefCell<Hardware>>>)]
pub fn reg_view() {
    let ppu_regs_hardware = hardware.clone();
    let sound_regs_hardware = hardware.clone();
    let pulse_a = &sound_regs_hardware.borrow().cpu.memory.sound.pulse_a;
    let ppu_regs = &ppu_regs_hardware.borrow().cpu.memory.ppu;
    let registers = &hardware.borrow().cpu.registers;

    let interrupt_enable = &hardware.borrow().cpu.memory.interrupt_enable;
    let interrupt_flag = &hardware.borrow().cpu.memory.interrupt_flag;
    let interrupt_master_enable = &hardware.borrow().cpu.interrupt_master_enable;

    mox! {
        <div style="font-family: PragmataPro, monospace;">
            <table>
                <thead>
                    <tr>
                        <th> { text("Registers") }  </th>
                    </tr>
                </thead>
                <tbody>
                    <draw_r16 _=("bc", registers.bc) />
                    <draw_r16 _=("de", registers.de) />
                    <draw_r16 _=("hl", registers.hl) />
                    <draw_r16 _=("sp", registers.sp) />
                    <draw_r8 _=("a", registers.a) />
                    <draw_flags _=("flags", &registers.flags) />
                    <draw_interrupt _=("ie", &interrupt_enable) />
                    <draw_interrupt _=("id", &interrupt_flag) />
                    <draw_bit _=("ime", *interrupt_master_enable) />
                    <draw_r8 _=("pa:control", R8(pulse_a.control.read())) />
                    <draw_r8 _=("pa:frequency", R8(pulse_a.frequency.read())) />
                    <draw_r8 _=("pa:volume", R8(pulse_a.volume.read())) />
                    <draw_r8 _=("pa:length", R8(pulse_a.length.read())) />
                    <draw_r8 _=("pa:sweep", R8(pulse_a.sweep.read())) />
                    <draw_r8 _=("scx", R8(ppu_regs.scx)) />
                    <draw_r8 _=("scy", R8(ppu_regs.scy)) />
                    <draw_r8 _=("ly", R8(ppu_regs.ly())) />
                    <draw_r8 _=("lcdc", R8(ppu_regs.lcdc.read())) />
                </tbody>
            </table>
        </div>
    }
}
