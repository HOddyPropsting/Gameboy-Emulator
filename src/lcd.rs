use sdl2::pixels::Color;
use sdl2::rect::Point;
use sdl2::video::{Window, WindowContext};
use sdl2::surface::Surface;
use sdl2::video::WindowSurfaceRef;
use sdl2::pixels::PixelFormatEnum;
use sdl2::render::Canvas;
use mmu::{Mmu,Bit};


const LCD_CONTROL_REGISTER: u16 = 0xFF40;
const TILE_DATA_OFFSET_1: u16 = 0x8000;
const TILE_DATA_OFFSET_2: u16 = 0x8800;

const BACKGROUND_OFFSET_1: u16 = 0x9800;
const BACKGROUND_OFFSET_2: u16 = 0x9C00;

const BG_PALETTE: u16 = 0xFF47;

const COLOR_WHITE : Color = Color {
    r: 255,
    g: 255,
    b: 255,
    a: 0,
};

const COLOR_L_GREY : Color = Color {
    r: 130,
    g: 130,
    b: 130,
    a: 0,
};

const COLOR_GREY : Color = Color {
    r: 60,
    g: 60,
    b: 60,
    a: 0,
};

const COLOR_BLACK : Color = Color {
    r: 0,
    g: 0,
    b: 0,
    a: 0,
};

pub struct Lcd{
pub canvas : Canvas<Window>,
pub tex : [u8; 144*3*160],
}

impl Lcd {
  fn BG_PALETTE_MASK() -> [u8;4] {[0b00000011, 0b00001100, 0b00110000, 0b11000000]}

  fn BG_COLOR_SHADES() -> [Color;4] {[COLOR_WHITE, COLOR_L_GREY, COLOR_GREY, COLOR_BLACK]}
}

#[derive(Debug)]
enum SpriteSize {
  Square,
  Rect,
}

impl Lcd {

  fn get_lcd_enabled(mmu : &Mmu) -> bool{
    return mmu.get_bit(LCD_CONTROL_REGISTER, Bit::Eight);
  }

  fn get_window_tile_bank(mmu : &Mmu) -> u16{
    if mmu.get_bit(LCD_CONTROL_REGISTER, Bit::Seven){
      return BACKGROUND_OFFSET_2;
    } else {
      return BACKGROUND_OFFSET_1;
    }
  }

  fn get_window_enabled(mmu : &Mmu) -> bool {
    return mmu.get_bit(LCD_CONTROL_REGISTER, Bit::Six);
  }

  fn get_tile_data_offset(mmu : &Mmu) -> u16{
    if mmu.get_bit(LCD_CONTROL_REGISTER, Bit::Five){
      return TILE_DATA_OFFSET_2;
    } else {
      return TILE_DATA_OFFSET_1;
    }    
  }

  fn get_bg_tile_bank(mmu : &Mmu) -> u16{
    if mmu.get_bit(LCD_CONTROL_REGISTER, Bit::Four){
      return BACKGROUND_OFFSET_2;
    } else {
      return BACKGROUND_OFFSET_1;
    }    
  }

  fn get_sprite_size(mmu : &Mmu) -> SpriteSize{
    if mmu.get_bit(LCD_CONTROL_REGISTER, Bit::Three){
      return SpriteSize::Square;
    } else {
      return SpriteSize::Rect;
    }
  }

  fn get_sprite_enabled(mmu : &Mmu) -> bool{
    return mmu.get_bit(LCD_CONTROL_REGISTER, Bit::Two);
  }

  fn get_bg_enabled(mmu : &Mmu) -> bool {
    return mmu.get_bit(LCD_CONTROL_REGISTER, Bit::One);  
  }

  fn get_scroll_y(mmu : &Mmu) -> u8{
    return mmu.fetch(0xFF42);
  }

  fn get_scroll_x(mmu : &Mmu) -> u8{
    return mmu.fetch(0xFF43);
  }

  fn get_window_y(mmu : &Mmu) -> u8{
    return mmu.fetch(0xFF4A);
  }

  fn get_window_x(mmu : &Mmu) -> u8{
    return mmu.fetch(0xFF4B);
  }

  fn get_bg_shade(colour_no : u8, mmu : &Mmu ) -> Color{
    return Lcd::BG_COLOR_SHADES()[colour_no as usize];
  }

  /// Returns the tile id for a given coördinate.
  /// This should be used once the scroll x / y have been added.
  fn get_tile_id(x : u8, y : u8, mmu : &Mmu) -> u8{
    let mx = x/8;
    let my = y/8;
    let offset = Lcd::get_bg_tile_bank(mmu);
    return mmu.fetch(offset + mx as u16 + (my as u16 * 0x10) as u16);
  }

  /// returns the address for the start of the tile data, for a given tile id
  fn get_tile_address(tile_id : u8, mmu : &Mmu) -> u16 {
    let offset = Lcd::get_tile_data_offset(mmu);
    if offset == TILE_DATA_OFFSET_1 {
      return (offset + (tile_id as u16 *0x000F)) as u16;
    } else {
      return (offset as i32 + (tile_id as i32 *0x000F)) as u16;
    }
  }

  /// Returns a 2 bit colour id.
  /// x and y are tile coördinates. i.e 0 < x <= 8, 0 < y <= 8
  fn get_tile_color_id(tile_address : u16, x : u8, y : u8, mmu : &Mmu) -> u8{
    let a = mmu.fetch(tile_address + (y as u16*2));
    let b = mmu.fetch(tile_address + (y as u16*2) + 1);
    let bit = Bit::from(x+1);
    let low = if a & bit as u8 > 0 {1} else {0};
    let high = if b & bit as u8 > 0 {2} else {0};
    return low + high;
  }

  fn get_bg_pixel(x : u8, y : u8, mmu : &Mmu) -> Color{
    if !Lcd::get_bg_enabled(mmu) {
      return COLOR_WHITE;
    }
    let vx = x.wrapping_add(Lcd::get_scroll_x(mmu)); //these wrap if it is over 256
    let vy = y.wrapping_add(Lcd::get_scroll_y(mmu));
    let tx = vx % 0x8;
    let ty = vy % 0x8;
    let address = Lcd::get_tile_address(Lcd::get_tile_id(vx, vy, mmu), mmu);
    let color_id = Lcd::get_tile_color_id(address, tx, ty, mmu);
    Lcd::get_bg_shade(color_id, mmu)
  }

  fn get_window_pixel(x : u8, y : u8, mmu : &Mmu) -> Option<Color>{
    let wx = Lcd::get_window_x(mmu).saturating_sub(7);
    let wy = Lcd::get_window_y(mmu);
    if Lcd::get_window_enabled(mmu) && x >= wx && y >= wy {
      let tx = x % 0x8;
      let ty = y % 0x8;
      let address = Lcd::get_tile_address(Lcd::get_tile_id(x, y, mmu), mmu);
      let color_id = Lcd::get_tile_color_id(address, tx, ty, mmu);
      Some(Lcd::get_bg_shade(color_id,mmu))
    } else {
      None
    }
  }

  fn get_screen_pixel(x : u8, y : u8, mmu : &Mmu) -> Color{
    match Lcd::get_window_pixel(x, y,mmu){
      Some(c) => c,
      None => Lcd::get_bg_pixel(x,y, mmu),
    }
  }

  pub fn render_screen(&mut self, mmu : &Mmu){
    for y in 0u8..160u8 {
      for x in 0u8..144u8{
        let c = Lcd::get_screen_pixel(x as u8,y as u8,mmu);
        let z : usize = ((x as u32 *3) + (y as u32 * 144 * 3)) as usize;
        self.tex[z] = c.b;
        self.tex[z+1] = c.g;
        self.tex[z+2] = c.r;
      }
    }

    let mut surface = Surface::new(144, 160, PixelFormatEnum::RGB24).expect("Failed to create surface");
    surface.with_lock_mut(|data| {
      data.copy_from_slice(&self.tex);
    });

    let ref texture_creator = self.canvas.texture_creator();
    let texture = texture_creator.create_texture_from_surface(&surface).unwrap();
    self.canvas.copy(&texture, None, None).unwrap();
    self.canvas.present();
  }

} 
