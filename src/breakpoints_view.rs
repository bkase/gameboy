use hardware::Hardware;
use js_sys::JSON;
use mem::Addr;
use moxie_dom::{
    elements::{div, input, li, ul},
    prelude::*,
};
use std::cell::RefCell;
use std::num::ParseIntError;
use std::rc::Rc;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;

#[topo::nested]
pub fn text_input(placeholder: &str, editing: bool, mut on_save: impl FnMut(String) + 'static) {
    let text = state(|| {
        if editing {
            placeholder.to_string()
        } else {
            String::new()
        }
    });

    fn input_value(ev: impl AsRef<sys::Event>) -> String {
        let event: &sys::Event = ev.as_ref();
        let target = event.target().unwrap();
        let input: sys::HtmlInputElement = target.dyn_into().unwrap();
        let val = input.value();
        input.set_value(""); // it's a little weird to clear the text every time, TODO clean up
        val
    }

    let change_text = text.clone();
    let on_change = move |change: event::Change| change_text.set(input_value(change));

    let clear_text = text.clone();
    let on_keydown = move |keypress: event::KeyDown| {
        if keypress.key() == "Enter" {
            let value = input_value(keypress);
            if !value.is_empty() {
                on_save(value);
            }
            clear_text.set("".into());
        }
    };

    mox! {
        <input type="text" placeholder={placeholder} value={&*text} autoFocus="true"
            class={if editing { "edit new-bp" } else { "new-bp"}}
            on={on_change} on={on_keydown} />
    };
}

fn decode_hex(s: &str) -> u16 {
    let bytes: Result<Vec<u8>, ParseIntError> = (0..s.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&s[i..i + 2], 16))
        .collect();

    let bytes = bytes.expect("hex is saved");
    u16::from(bytes[0]) << 8 | u16::from(bytes[1])
}

fn sync_local_storage(bps: &Vec<String>) {
    let window = web_sys::window().expect("window to exist");
    let local_storage: web_sys::Storage = window
        .local_storage()
        .expect("local_storage")
        .expect("Some local_storage");

    let array = js_sys::Array::new();
    for bp in bps.iter() {
        array.push(&JsValue::from(bp));
    }

    let storage_string = JSON::stringify(&JsValue::from(array)).expect("storage string");
    let storage_string: String = storage_string.into();
    local_storage
        .set_item("breakpoints_view_breakpoints", &storage_string)
        .unwrap();
}

#[topo::nested]
#[illicit::from_env(hardware: &Key<Rc<RefCell<Hardware>>>)]
pub fn breakpoints_view() {
    let window = web_sys::window().expect("window to exist");
    let local_storage: web_sys::Storage = window
        .local_storage()
        .expect("local_storage")
        .expect("Some local_storage");

    let mut bps: Vec<String> = Vec::new();

    // load it
    if let Some(value) = local_storage
        .get_item("breakpoints_view_breakpoints")
        .expect("inside localstorage")
    {
        let data = JSON::parse(&value).expect("json");
        let iter = js_sys::try_iter(&data)
            .expect("iterate through it")
            .expect("Not none");
        for bp in iter {
            let bp: std::string::String = bp
                .expect("it's a thing")
                .as_string()
                .expect("it's a string");
            let addr = Addr::directly(decode_hex(&bp));
            bps.push(bp);

            hardware.borrow_mut().breakpoints.insert(addr);
        }
    }

    let bps = state(|| bps);

    let hardware = hardware.clone();
    let bps = bps.clone();
    mox! {
        <div>
            <text_input _=(
                    "014c",
                    true, {
                        let bps = bps.clone();
                    move |value: String| {
                        bps.update(|bps| {
                            let mut bps = bps.to_vec();
                            // parse value
                            let addr = Addr::directly(decode_hex(&value));
                            bps.push(value);
                            // add it to breakpoints
                            hardware.borrow_mut().breakpoints.insert(addr);
                            // update localstorage
                            sync_local_storage(&bps);
                            Some(bps)
                        });
                    }},
                )
            />
            <ul>
            { bps.iter().for_each(|bp| {
                mox! { <li> { text(bp) } </li> }
              })
            }
            </ul>
        </div>
    }
}
