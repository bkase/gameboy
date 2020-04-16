# Gameboy

Hopefully a gameboy color emulator. Right now, it's almost a gameboy pocket one.

## What works

* Very primitive sounds
* The Bootrom
* Tetris (modulo sound)
* Many of Blaarg's CPU tests

On top of property-based and unit tests, `headless golden` runs golden master tests for blaarg's ROMs and bootrom and Tetris.

## How to run

I recommend running with `nix` for reproducibility. Rustup nightly will probably still work though.

1. Make a folder `roms/` in the root of the repo with at least the bootrom `DMG_ROM.bin`, and `Tetris.gb`
2. For headless mode, I recommend building with nix: Run `nix-build release.nix`, `./result/bin/headless run --help` for more.
3. For a wasm build, install [`wasm-pack`](https://rustwasm.github.io/wasm-pack/)
4. Inside of `nix-shell shell.nix` run `wasm-pack build` and `cd www && npm run start`
5. Open up `localhost:8080` in your browser

## Relevant media:

### Blog posts

Note that the posts include embedded emulators (snapshotted at the point of post publishing).

* [A Gameboy Emulator Debugging Parable](https://bkase.dev/posts/gameboy-debugging-parable)

### Old Video Series

* Ep00: https://youtu.be/0ErM1JsWUFk
* Ep01: https://youtu.be/DsZDdDEztIs
* Ep02: https://youtu.be/7ZrjgwKf0HQ
* Ep03.1: https://youtu.be/IQBBJGtctIo
* Ep03.2: https://youtu.be/DrN6a_D0XRE
* Ep03.3: https://youtu.be/2PLuz_kFRHQ

At some point, editing videos became very not fun. I will occasionally write blog posts as I go.

### Tweets

In mostly reverse chronological order:

* [Golden master testing to detect regressions](https://twitter.com/bkase_/status/1249091551386632192)
* [Gameboy debugging parable blog post announcement](https://twitter.com/bkase_/status/1241555098423234560)
* [One-line mistake in cpu logic](https://twitter.com/bkase_/status/1240870511237828609)
* [Tetris "mostly" works](https://twitter.com/bkase_/status/1233306000158949376)
* [Gameboy bootrom is basically done](https://twitter.com/bkase_/status/1128213391003213824)
* [Gameboy almost boots properly](https://twitter.com/bkase_/status/1125164504705159168)

