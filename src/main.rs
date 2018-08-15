extern crate sdl2;
extern crate rand;
extern crate time;

mod mmu;
mod cpu;
mod lcd;

use rand::Rng;

use cpu::Cpu;
use mmu::Bit;
use lcd::Lcd;

use time::{PreciseTime};

fn main() {

  let mut c = Cpu::default();

  let sdl_context = sdl2::init().unwrap();
  let video_subsystem = sdl_context.video().unwrap();
  let window = video_subsystem
      .window("RustyBoy",
              144,
              160)
      .position_centered()
      .build()
      .unwrap(); 

  let mut canvas = window
        .into_canvas()
        .target_texture()
        .build().unwrap();

  let mut lcd = Lcd{
    canvas : canvas,
    tex : [0; 144*3*160]
  };

  let mut rng = rand::thread_rng();

  c.mmu.set_bit(0xFF40,Bit::One);
  for i in 0..15 {
    c.mmu.save(0x8000 + i,rng.gen::<u8>());
  } 

  let mut prev = PreciseTime::now();
  loop {
    //c.process_next_instruction();
    lcd.render_screen(&c.mmu);
    let now = PreciseTime::now();
    let dt = prev.to(now);
    println!("{:?}",dt.num_milliseconds());
    prev = now;
  }
}



