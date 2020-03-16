#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    pub fn log(s: &str);

    type Performance_;
    static performance: Performance_;

    #[wasm_bindgen(method, js_name = now)]
    fn now(this: &Performance_) -> f64;
}

pub fn window() -> web_sys::Window {
    web_sys::window().expect("no global `window` exists")
}

pub fn document() -> web_sys::Document {
    window()
        .document()
        .expect("should have a document on window")
}

#[cfg(target_arch = "wasm32")]
pub struct Performance;
#[cfg(target_arch = "wasm32")]
impl Performance {
    pub fn create() -> Performance {
        Performance
    }

    pub fn now(&self) -> f64 {
        performance.now()
    }
}

// ==============================
// =========== NATIVE ===========
// ==============================

#[cfg(not(target_arch = "wasm32"))]
use std::time::Instant;

#[cfg(not(target_arch = "wasm32"))]
pub struct Performance {
    instant: Instant,
}
#[cfg(not(target_arch = "wasm32"))]
impl Performance {
    pub fn create() -> Performance {
        Performance {
            instant: Instant::now(),
        }
    }

    pub fn now(&self) -> f64 {
        self.instant.elapsed().as_millis() as f64
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub fn log(s: &str) {
    println!("{:}", s);
}
