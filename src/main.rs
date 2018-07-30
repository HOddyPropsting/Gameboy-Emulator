extern crate sdl2;

mod mmu;
mod cpu;

use cpu::Cpu;
use sdl2::rect::Point;
use sdl2::pixels::Color;
use std::time::*;

fn main() {

  let mut c = Cpu::default();

  let sdl_context = sdl2::init().unwrap();
  let video_subsystem = sdl_context.video().unwrap();
  let window = video_subsystem
      .window("rust-sdl2 demo: Game of Life",
              188,
              188)
      .position_centered()
      .build()
      .unwrap();
  let mut canvas = window
        .into_canvas()
        .target_texture()
        .present_vsync()
        .build().unwrap();

  canvas.clear();

  canvas.set_draw_color(Color::RGB(255,0,0));

  canvas.draw_point(Point::new(20,20)).unwrap();
  canvas.draw_point(Point::new(20,21)).unwrap();
  canvas.draw_point(Point::new(28,20)).unwrap();
  canvas.draw_point(Point::new(21,21)).unwrap();
    
  canvas.present();

  canvas.set_draw_color(Color::RGB(0,0,0));        

  loop {
    c.process_next_instruction();
  }
}




struct Clock{

  time : u64,

  past : Instant,

  wait_time : Duration

}


impl Clock {
  
  fn tick(){

    //grab time diff between last time tick executed and now
    //if diff > waitTime
    //log a crystal tick
    //every 4 crystal ticks log a "machine tick"
    //every 456 ticks start rendering a line and then trigger hblank interrupt
    //after rendering 144 lines trigger vblank and wait 4559 ticks before rendering again



    // ok slight difference - allocate a "tick budget" that must be less than an entire loop. EG write the video functions to display the screen
    // work out how long that takes, then execute that many "ticks" stopping at the next screen 
    

    //let end = Instant::now();

  }  

}



/*
Clock

Screen timing:
15.66ms

Vblank time:
1.09ms

Hblank time:
108.7µs (.1087ms)

time per pixel = .1087 / 160 = 0.000679375ms

time per cycle:

basic clock speed = 4.194304 Mhz = 0.00023841857910156ms

clock ticks per screen = 

clock ticks per line = 455.92

clock ticks per vblank = 4559

clock ticks per pixel = 2.8495052280000747

clock ticks per 




*/





