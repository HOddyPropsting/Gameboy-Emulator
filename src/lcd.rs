const BIT_0: u8 = 0b00000001;
const BIT_1: u8 = 0b00000010;
const BIT_2: u8 = 0b00000100;
const BIT_3: u8 = 0b00001000;
const BIT_4: u8 = 0b00010000;
const BIT_5: u8 = 0b00100000;
const BIT_6: u8 = 0b01000000;
const BIT_7: u8 = 0b10000000;

const LCD_CONTROL_REGISTER: u16 = 0xFF40;
const MIXED_OFFSET_1: u16 = 0x8800;
const MIXED_OFFSET_2: u16 = 0x8000;
const BACKGROUND_OFFSET_1: u16 = 0x9800;
const BACKGROUND_OFFSET_2: u16 = 0x9C00;

const BG_PALETTE: u16 = 0xFF47;

struct lcd {

}

impl lcd {
  fn BG_PALETTE_MASK(&self) -> [u8;4] {[0b00000011, 0b00001100, 0b00110000, 0b11000000]}

  fn BG_COLOR_SHADES(&self) -> [Color;4] {[Color::RGB(15,56,15), Color::RGB(48,98,48), Color::RGB(139,172,15), Color::RGB(155,188,15)]}
}

#[derive(Debug)]
enum SpriteSize {
  square,
  rect,
}


impl lcd {

  fn get_lcd_enabled(mmu : &Mmu) -> bool{
    return mmu.get_bit(LCD_CONTROL_REGISTER, BIT_7);
  }

  fn get_window_tile_bank(mmu : &Mmu) -> u16{
    if mmu.get_bit(LCD_CONTROL_REGISTER, BIT_6){
      return BACKGROUND_OFFSET_2;
    } else {
      return BACKGROUND_OFFSET_1;
    }
  }

  fn get_window_enabled(mmu : &Mmu) -> bool {
    return mmu.get_bit(LCD_CONTROL_REGISTER, BIT_5);
  }

  fn get_mixed_tile_bank(mmu : &Mmu) -> u16{
    if mmu.get_bit(LCD_CONTROL_REGISTER, BIT_4){
      return MIXED_OFFSET_2;
    } else {
      return MIXED_OFFSET_1;
    }    
  }

  fn get_bg_tile_bank(mmu : &Mmu) -> u16{
    if mmu.get_bit(LCD_CONTROL_REGISTER, BIT_3){
      return BACKGROUND_OFFSET_2;
    } else {
      return BACKGROUND_OFFSET_1;
    }    
  }

  fn get_sprite_size(mmu : &Mmu) -> SpriteSize{
    if mmu.get_bit(LCD_CONTROL_REGISTER, BIT_2){
      return SpriteSize::square;
    } else {
      return SpriteSize::rect;
    }
  }

  fn get_sprite_enabled(mmu : &Mmu) -> bool{
    return mmu.get_bit(LCD_CONTROL_REGISTER, BIT_1);
  }

  fn get_bg_enabled(mmu : &Mmu) -> bool {
    return mmu.get_bit(LCD_CONTROL_REGISTER, BIT_0);  
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

  fn get_bg_shade(&self, mmu : &Mmu, colourNo : u8 ) -> Color{
    return self.BG_COLOR_SHADES()[(mmu.fetch(BG_PALETTE) & self.BG_PALETTE_MASK()[colourNo as usize]) as usize];
  }

}