#![allow(dead_code)]

use futures::{Future, FutureExt};
use futures_signals::signal::{Signal, SignalExt};
use virtual_dom_rs::prelude::*;
use web_utils;

// supports only 16 cols for now
#[derive(Debug, Clone)]
struct MemTableViewModel {
    data: Vec<u8>, // assumption: aligned to +0 of the first row
    focus: u16,    // the row which is centered, invariant 0xXXX0 and >= 0x80
    cursor: u16,   // invariant in the vec
}

fn mem_table_view<In: Signal<Item = MemTableViewModel>>(
    model: In,
) -> impl Signal<Item = VirtualNode> {
    model.map(|m| {
        let top_labels: Vec<VirtualNode> = (0..16)
            .map(|i| html! { <th> { format!("{:02x}", i) } </th> })
            .collect();

        let data_per_row = m.data.chunks(16);

        let draw_row = |(i, row): (usize, &[u8])| {
            let cols: Vec<VirtualNode> = row
                .iter()
                .map(|byte| html! { <td> { format!("{:02x}", byte) } </td> })
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
                <th> { format!("${:04x}", m.focus + ((i*16) as u16) - (8*16)) } </th>
                { cols }
                <td> { ascii } </td>
            </tr>
            }
        };

        let all_data: Vec<VirtualNode> = data_per_row.enumerate().map(draw_row).collect();

        html! {
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
        }
    })
}

fn sample_view<In: Signal<Item = u32>>(model: In) -> impl Signal<Item = VirtualNode> {
    model.map(|m| {
        let greetings = format!("Hello, World! {}", m);
        html! { <div> { greetings } </div> }
    })
}

pub fn run<In: Signal<Item = u32>>(rx: In) -> impl Future<Output = ()> {
    let start_view = html! { <div> Hello </div> };

    let body = web_utils::document().body().unwrap();

    let mut dom_updater = DomUpdater::new_append_to_mount(start_view, &body);

    mem_table_view(rx.map(|_i| MemTableViewModel {
        data: (0..=255).collect(),
        focus: 0xff00,
        cursor: 0xff30,
    }))
    .map(move |vdom| dom_updater.update(vdom))
    .to_future()
    .map(|_| ())

    /*
    sample_view(rx)
        .map(move |vdom| dom_updater.update(vdom))
        .to_future()
        .map(|_| ())
        */
}
