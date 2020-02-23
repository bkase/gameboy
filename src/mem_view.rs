use hardware::Hardware;
use mem::{Addr, Direction};
use moxie_dom::{
    elements::{button, div, span, table, tbody, td, th, thead, tr},
    prelude::*,
};
use std::cell::RefCell;
use std::rc::Rc;
use web_utils::log;

const ROWS: u16 = 16;
const COLS: u16 = 16;

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Cursor(pub u16);
#[derive(Debug, PartialEq)]
pub struct Focus(pub u16);

fn cursor_of_coord(focus: &Focus, row: usize, col: usize) -> Cursor {
    Cursor(focus.0 - ((ROWS / 2) * COLS) + ((row as u16) * COLS) + (col as u16))
}

fn color(cursor: &Cursor, focus: &Focus, row: usize, col: usize) -> &'static str {
    if *cursor == cursor_of_coord(focus, row, col) {
        "rgba(0,170,170,.2)"
    } else {
        "rgba(0,0,0,0)"
    }
}

#[topo::nested]
fn top_labels() {
    (0..16).for_each(|i| {
        mox! {
            <th> {text(format!("{:02x}", i))} </th>
        }
    })
}

#[topo::nested]
#[illicit::from_env(cursor: &Key<Cursor>, focus: &Key<Focus>)]
fn cols(i: usize, row: &[u8]) {
    row.iter().enumerate().for_each(|(j, byte)| {
        let bg_color = color(cursor, focus, i, j);
        mox! {
            <td style={format!("background-color: {};", bg_color)}
                on={{
                    let focus = focus.clone();
                    let cursor = cursor.clone();

                    move |_: event::Click| {
                    log(&format!("Cursor moved to ({},{})", i, j));
                    let new_cursor = cursor_of_coord(&focus, i, j);
                    cursor.set(new_cursor);
                }}}>
                {text(format!("{:02x}", byte))}
            </td>
        }
    });
}

#[topo::nested]
#[illicit::from_env(cursor: &Key<Cursor>, focus: &Key<Focus>)]
fn ascii(i: usize, row: &[u8]) {
    row.iter().enumerate().for_each(|(j, byte)| {
        let bg_color = color(cursor, focus, i, j);

        let content = if *byte >= 32 && *byte < 128 {
            (*byte as char).to_string()
        } else {
            '.'.to_string()
        };

        mox! {
            <span style={format!("background-color: {}", bg_color)}>{content}</span>
        };
    })
}

#[topo::nested]
#[illicit::from_env(focus: &Key<Focus>)]
fn row((i, row): (usize, &[u8])) {
    mox! {
        <tr>
            <th> {text(format!("${:04x}", focus.0 + ((i as u16) * COLS) - ((ROWS/2)*COLS))) } </th>
            <cols _=(i, row) />

            <td>
                <ascii _=(i, row) />
            </td>
        </tr>
    }
}

#[topo::nested]
fn arrow_press(direction: Direction, cursor: &Key<Cursor>, focus: &Key<Focus>) {
    let new_focus = Focus(
        (i32::from(focus.0)
            + (0x40
                * match direction {
                    Direction::Pos => 1,
                    Direction::Neg => -1,
                })) as u16,
    );

    let new_cursor = if cursor.0 < cursor_of_coord(&new_focus, 0, 0).0 {
        cursor_of_coord(&new_focus, 0, 0)
    } else if cursor.0 > cursor_of_coord(&new_focus, (ROWS - 1) as usize, (COLS - 1) as usize).0 {
        cursor_of_coord(&new_focus, (ROWS - 1) as usize, (COLS - 1) as usize)
    } else {
        Cursor(cursor.0)
    };

    log(&format!(
        "Writing new focus {:?} (old focus) {:?}",
        new_focus, focus
    ));
    focus.set(new_focus);

    log(&format!("Writing new cursor {:?}", new_cursor));
    cursor.set(new_cursor);
}

#[topo::nested]
#[illicit::from_env(cursor: &Key<Cursor>, focus: &Key<Focus>)]
fn mem_table(data: Vec<u8>) {
    let data_per_row = data.chunks(COLS as usize);

    mox! {
        <div style="font-family: PragmataPro, monospace;">
            <button on={{
                let cursor = cursor.clone();
                let focus = focus.clone();
                move |_: event::Click| {
                    arrow_press(Direction::Pos, &cursor, &focus)
                }}}>
        { text("↑") }
            </button>

            <button on={{
                let cursor = cursor.clone();
                let focus = focus.clone();
                move |_: event::Click| {
                   arrow_press(Direction::Neg, &cursor, &focus)
                }}}>
            { text("↓") }
            </button>

            <table>
                <thead>
                    <tr>
                        <th></th>
                        <top_labels />
                    </tr>
                </thead>
                <tbody>
                { data_per_row.enumerate().for_each(row) }
                </tbody>
            </table>
        </div>
    }
}

#[topo::nested]
#[illicit::from_env(hardware: &Key<Rc<RefCell<Hardware>>>)]
pub fn mem_view(start_focus: u16, start_cursor: u16) {
    let cursor = state(|| Cursor(start_cursor));
    let focus = state(|| Focus(start_focus));

    let start_addr = Addr::directly(focus.0 - ((ROWS / 2) * COLS));
    let data = hardware
        .borrow()
        .cpu
        .memory
        .ld_lots(start_addr, ROWS * COLS);

    illicit::child_env![
      Key<Cursor> => cursor,
      Key<Focus> => focus
    ]
    .enter(|| topo::call(|| mem_table(data)));
}
