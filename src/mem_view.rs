#![allow(dead_code)]

use futures_signals::map_ref;
use futures_signals::signal::{Mutable, Signal, SignalExt};
use mem::{Addr, Memory};
use mutable_effect::MutableEffect;
use std::cell::RefCell;
use std::rc::Rc;
use web_sys::MouseEvent;

use virtual_dom_rs::prelude::*;

// supports only 16 cols for now
#[derive(Debug, Clone)]
struct ViewModel {
    data: Vec<u8>, // assumption: aligned to +0 of the first row
    local_mutable: LocalState<Rc<RefCell<Mutable<u16>>>>,
    local: LocalState<u16>,
}

fn mem_table_view(model: ViewModel) -> Rc<VirtualNode> {
    let top_labels: Vec<VirtualNode> = (0..16)
        .map(|i| html! { <th> { format!("{:02x}", i) } </th> })
        .collect();

    let data_per_row = model.data.chunks(16);

    let local_mutable = model.local_mutable.clone();
    let local = model.local.clone();

    let draw_row = |(i, row): (usize, &[u8])| {
        let cols: Vec<VirtualNode> = row
            .iter()
            .enumerate()
            .map(|(j, byte)| {
                html! {
                <td
                 onclick=move |_event: MouseEvent| {
                   web_sys::console::log_1(&format!("Cursor moved to ({},{})", i, j).into());
                   /*let cursor = local_mutable.cursor.clone();
                   {
                       let cursor_borrow = cursor.borrow_mut();
                       let mut lock = cursor_borrow.lock_mut();
                       *lock = local.focus  - (8*16) + ((i*16) as u16) + (j as u16);
                   }*/
                }> { format!("{:02x}", byte) } </td> }
            })
            .collect();

        let ascii: String = row
            .iter()
            .map(|byte| {
                if *byte >= 32 && *byte < 128 {
                    *byte as char
                } else {
                    '.'
                }
            })
            .collect();

        html! {
        <tr>
            <th> { format!("${:04x}", model.local.focus + ((i*16) as u16) - (8*16)) } </th>
            { cols }
            <td> { ascii } </td>
        </tr>
        }
    };

    let all_data: Vec<VirtualNode> = data_per_row.enumerate().map(draw_row).collect();

    Rc::new(html! {
        <div style="font-family: PragmataPro, monospace;">
            <p>{ format!("Cursor: {}", model.local.cursor) } </p>
            <table>
                <thead>
                    <tr>
                        <th> </th>
                        { top_labels }
                    </tr>
                </thead>
                <tbody>
                    { all_data }
                </tbody>
            </table>
        </div>
    })
}

#[derive(Debug, Clone)]
pub struct LocalState<T> {
    pub focus: T,  // the row which is centered, invariant 0xXXX0 and >= 0x80
    pub cursor: T, // invariant in the vec
}

pub struct State {
    pub mem: Rc<MutableEffect<Rc<Memory>>>,
    pub local: LocalState<Rc<RefCell<Mutable<u16>>>>,
}

pub fn component(state: State) -> impl Signal<Item = Rc<VirtualNode>> {
    let rows = 16;
    let cols = 16;

    let mem = state.mem.clone_data();
    let local = state.local.clone();

    map_ref! {
        let focus = state.local.focus.borrow().signal(),
        let cursor = state.local.cursor.borrow().signal(),
        let _ = state.mem.trigger.signal() => move {
            let start_addr = Addr::directly(focus - ((rows / 2) * cols));
            let data = mem.ld_lots(start_addr, rows * cols);

            mem_table_view(ViewModel { data, local_mutable: local.clone(), local: LocalState { cursor: *cursor, focus: *focus } } )
        }
    }
}
