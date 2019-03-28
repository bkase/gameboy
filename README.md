# Gameboy

Hopefully a gameboy color emulator. Right now, it's barely anything.

## Video Series

Join me as I understand how a gameboy, Rust, and WASM work. 

* Ep00: https://youtu.be/0ErM1JsWUFk
* Ep01: https://youtu.be/DsZDdDEztIs
* Ep02: https://youtu.be/7ZrjgwKf0HQ

## How to run

1. Acquire the bootrom `DMG_ROM.bin` and stick it in the root of the repo
2. Rustup nightly
3. Install [`wasm-pack`](https://rustwasm.github.io/wasm-pack/)
4. Run `wasm-pack build` and `cd www && npm run start`
5. Open up `localhost:8080` in your browser

Seriously though, don't run this yet -- it's not ready

