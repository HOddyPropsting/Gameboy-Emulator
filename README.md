# RustyBoy

A Gameboy emulator written in rust. I began this as a way to learn the Rust programming language. It is also my first foray into low-level programming / how small computers like this function. I've used various sources to discover how to build this -
- http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf
- http://otakunozoku.com/nintendo-gameboy-cribsheet/
- [Z80 User Manual](http://z80.info/zip/z80cpu_um.pdf)
- Rodney Zaks How to Program the Z80
- http://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html

## Installation 
1. Make sure you have Rust and Cargo up to date
2. Pull the repo
3. Cargo Run in the main directory

## Design
 I am splitting things up roughly along the lines that they would be separate components on the board. So a separate module for the CPU, MMU, LCD screen, and the System Clock. I'll see how this holds up and may have to adjust due to shared access/dependencies. I'm using SDL2 for the display and input. However this is very early days and it does not yet display correctly.


