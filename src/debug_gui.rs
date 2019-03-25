use css_rs_macro::css;
use futures::stream::Stream;
use futures::Future;
use std::rc::Rc;
use virtual_dom_rs::prelude::*;
use web_sys;

struct SampleView {}
impl SampleView {
    fn main<In: Stream<Item = u32>>(model: In) -> impl Stream<Item = VirtualNode> {
        return model.map(|m| {
            let greetings = format!("Hello, World! {}", m);
            html! { <div> { greetings } </div> }
        });
    }
}

pub struct App {
    pub count: u32,
}

impl App {
    /*
    pub fn render(&mut self) {
        let greetings = format!("Hello, World! {}", self.count);

        let end_view = html! {
           // Use regular Rust comments within your html
           <div class="big blue">
              /* Interpolate values using braces */
              <strong>{ greetings }</strong>

              <button
                class=MY_COMPONENT_CSS
                onclick=|_event: MouseEvent| {
                   web_sys::console::log_1(&"Button Clicked!".into());
                }
              >
                // No need to wrap text in quotation marks (:
                Click me and check your console
              </button>
           </div>
        };
        self.dom_updater.update(end_view);
    }*/

    pub fn new<In: Stream<Item = u32>>(rx: In) -> (App, impl Stream<Item = ()>) {
        let start_view = html! { <div> Hello </div> };

        let window = web_sys::window().unwrap();
        let document = window.document().unwrap();
        let body = document.body().unwrap();

        let mut dom_updater = DomUpdater::new_append_to_mount(start_view, &body);

        let rx_ = SampleView::main(rx).map(move |vdom| dom_updater.update(vdom));

        (App { count: 0 }, rx_)
    }
}

static MY_COMPONENT_CSS: &'static str = css! {r#"
:host {
    font-size: 24px;
    font-weight: bold;
}
"#};

static _MORE_CSS: &'static str = css! {r#"
.big {
  font-size: 30px;
}

.blue {
  color: blue;
}
"#};
