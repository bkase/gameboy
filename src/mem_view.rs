#![allow(dead_code)]

use futures_signals::map_ref;
use futures_signals::signal::{Mutable, Signal};
use hardware::Hardware;
use mem::{Addr, Direction};
use mutable_effect::MutableEffect;
use std::cell::RefCell;
use std::rc::Rc;
#[allow(unused_imports)]
use web_sys::MouseEvent;
use web_utils::log;

use virtual_dom_rs::prelude::*;

const ROWS: u16 = 16;
const COLS: u16 = 16;

// supports only 16 cols for now
#[derive(Debug, Clone)]
struct ViewModel {
    data: Vec<u8>, // assumption: aligned to +0 of the first row
    local_mutable: LocalState<Rc<RefCell<Mutable<u16>>>>,
    local: LocalState<u16>,
    local_cell: Rc<RefCell<LocalState<u16>>>,
}

fn mem_table_view(model: ViewModel) -> Rc<VirtualNode> {
    fn cursor_of_coord(focus: u16, row: usize, col: usize) -> u16 {
        focus - ((ROWS / 2) * COLS) + ((row as u16) * COLS) + (col as u16)
    }

    fn color(local: &LocalState<u16>, row: usize, col: usize) -> &'static str {
        if local.cursor == cursor_of_coord(local.focus, row, col) {
            "rgba(0,170,170,.2)"
        } else {
            "rgba(0,0,0,0)"
        }
    }

    let top_labels: Vec<VirtualNode> = (0..16)
        .map(|i| html! { <th> { format!("{:02x}", i) } </th> })
        .collect();

    // data for the first closure
    let local = model.local.clone();
    let local_mutable = model.local_mutable.clone();

    let draw_row = move |(i, row): (usize, &[u8])| {
        let cols: Vec<VirtualNode> = row
            .iter()
            .enumerate()
            .map({
                // data for the next closure
                let local = local.clone();
                let local_mutable = local_mutable.clone();

                move |(j, byte)| {
                    let bg_color = color(&local, i, j);

                    // RLS and clippy aren't smart enough to understand html!
                    // data for the next closure
                    #[allow(unused_variables)]
                    let local = local.clone();
                    #[allow(unused_variables)]
                    let local_mutable = local_mutable.clone();

                    html! {
                         <td style={format!("background-color: {};", bg_color)}
                             onclick=move |_event: MouseEvent| {
                        web_sys::console::log_1(&format!("Cursor moved to ({},{})", i, j).into());
                        let cursor = local_mutable.cursor.clone();
                        {
                            let cursor_borrow = cursor.borrow_mut();
                            let mut lock = cursor_borrow.lock_mut();
                            *lock = cursor_of_coord(local.focus, i, j);
                        }
                    }> { format!("{:02x}", byte) } </td>
                    }
                }
            })
            .collect();

        let ascii: Vec<VirtualNode> = row
            .iter()
            .enumerate()
            .map(|(j, byte)| {
                let bg_color = color(&local, i, j);

                let content = if *byte >= 32 && *byte < 128 {
                    (*byte as char).to_string()
                } else {
                    '.'.to_string()
                };

                html! {
                    <span style={format!("background-color: {}", bg_color)}>{content}</span>
                }
            })
            .collect();

        html! {
        <tr>
            <th> { format!("${:04x}", local.focus + ((i as u16) * COLS) - ((ROWS/2)*COLS)) } </th>
            { cols }
            <td> { ascii } </td>
        </tr>
        }
    };

    let data_per_row = model.data.chunks(COLS as usize);
    let all_data: Vec<VirtualNode> = data_per_row.enumerate().map(draw_row).collect();

    fn arrow_button(
        local: Rc<RefCell<LocalState<u16>>>,
        local_mutable: LocalState<Rc<RefCell<Mutable<u16>>>>,
        direction: Direction,
    ) {
        let cursor = local_mutable.cursor.clone();
        let focus = local_mutable.cursor.clone();
        let new_focus = ((local.borrow().focus as i32)
            + (0x40
                * match direction {
                    Direction::Pos => 1,
                    Direction::Neg => -1,
                })) as u16;
        {
            let focus_borrow = focus.borrow_mut();
            let mut lock = focus_borrow.lock_mut();
            log(&format!(
                "Writing new focus {:?} (old focus) {:?}",
                new_focus,
                local.borrow().focus
            ));
            *lock = new_focus;
        }

        let new_cursor = if local.borrow().cursor < cursor_of_coord(new_focus, 0, 0) {
            cursor_of_coord(new_focus, 0, 0)
        } else if local.borrow().cursor
            > cursor_of_coord(new_focus, (ROWS - 1) as usize, (COLS - 1) as usize)
        {
            cursor_of_coord(new_focus, (ROWS - 1) as usize, (COLS - 1) as usize)
        } else {
            local.borrow().cursor
        };

        {
            let cursor_borrow = cursor.borrow_mut();
            let mut lock = cursor_borrow.lock_mut();
            log(&format!("Writing new cursor {:?}", new_cursor));
            *lock = new_cursor;
        }
    }

    let local_mutable_up = model.local_mutable.clone();
    let local_mutable_down = model.local_mutable.clone();
    let local_up = model.local_cell.clone();
    let local_down = model.local_cell.clone();

    Rc::new(html! {
        <div style="font-family: PragmataPro, monospace;">
            <button onclick=move |_: MouseEvent| {
        arrow_button(local_up.clone(), local_mutable_up.clone(), Direction::Pos)
            }>
        { "↑" }
            </button>

            <button onclick=move |_: MouseEvent| {
               arrow_button(local_down.clone(), local_mutable_down.clone(), Direction::Neg)
            }>
            { "↓" }
            </button>

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
    pub focus: T,  // the row which is centered, invariant 0xXXX0 and >= 0x40
    pub cursor: T, // invariant in the vec
}

pub struct State {
    pub hardware: Rc<MutableEffect<Rc<RefCell<Hardware>>>>,
    pub local: LocalState<Rc<RefCell<Mutable<u16>>>>,
}

pub fn component(state: State) -> impl Signal<Item = Rc<VirtualNode>> {
    let hardware = state.hardware.clone_data();
    let local = state.local.clone();

    let local_cell = Rc::new(RefCell::new(LocalState {
        cursor: 0,
        focus: 0,
    }));

    map_ref! {
        let focus = state.local.focus.borrow().signal(),
        let cursor = state.local.cursor.borrow().signal(),
        let _ = state.hardware.trigger.signal() => move {
            let start_addr = Addr::directly(focus - ((ROWS / 2) * COLS));
            let data = hardware.borrow().cpu.memory.ld_lots(start_addr, ROWS * COLS);

            let new_local = LocalState { cursor: *cursor, focus: *focus };
            *local_cell.borrow_mut() = new_local.clone();

            mem_table_view(ViewModel { data, local_mutable: local.clone(), local: new_local, local_cell: local_cell.clone() } )
        }
    }
}
