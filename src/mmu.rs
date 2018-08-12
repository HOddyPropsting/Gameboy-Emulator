pub struct Mmu{

  gb_internal_rom : [u8; 0x100],

  gb_cartridge : [u8; 65536],

  boot_rom_locked : bool,
}

//important locations
const REG_DIV : usize = 0xFF04;
const BOOT_ROM_LOCKOUT: usize = 0xFF50;
pub const INTERRUPT_ENABLED : usize = 0xFFFF;
pub const INTERRUPT_FLAGS : usize = 0xFF0F;

impl Default for Mmu {
  fn default() -> Mmu{
    Mmu{
      gb_internal_rom : 
      [ 0x31, 0xfe, 0xff, 0xaf, 0x21, 0xff, 0x9f, 0x32, 0xcb, 0x7c, 0x20, 0xfb, 0x21, 0x26, 0xff, 0x0e,
        0x11, 0x3e, 0x80, 0x32, 0xe2, 0x0c, 0x3e, 0xf3, 0xe2, 0x32, 0x3e, 0x77, 0x77, 0x3e, 0xfc, 0xe0,
        0x47, 0x11, 0x04, 0x01, 0x21, 0x10, 0x80, 0x1a, 0xcd, 0x95, 0x00, 0xcd, 0x96, 0x00, 0x13, 0x7b,
        0xfe, 0x34, 0x20, 0xf3, 0x11, 0xd8, 0x00, 0x06, 0x08, 0x1a, 0x13, 0x22, 0x23, 0x05, 0x20, 0xf9,
        0x3e, 0x19, 0xea, 0x10, 0x99, 0x21, 0x2f, 0x99, 0x0e, 0x0c, 0x3d, 0x28, 0x08, 0x32, 0x0d, 0x20,
        0xf9, 0x2e, 0x0f, 0x18, 0xf3, 0x67, 0x3e, 0x64, 0x57, 0xe0, 0x42, 0x3e, 0x91, 0xe0, 0x40, 0x04,
        0x1e, 0x02, 0x0e, 0x0c, 0xf0, 0x44, 0xfe, 0x90, 0x20, 0xfa, 0x0d, 0x20, 0xf7, 0x1d, 0x20, 0xf2,
        0x0e, 0x13, 0x24, 0x7c, 0x1e, 0x83, 0xfe, 0x62, 0x28, 0x06, 0x1e, 0xc1, 0xfe, 0x64, 0x20, 0x06,
        0x7b, 0xe2, 0x0c, 0x3e, 0x87, 0xe2, 0xf0, 0x42, 0x90, 0xe0, 0x42, 0x15, 0x20, 0xd2, 0x05, 0x20,
        0x4f, 0x16, 0x20, 0x18, 0xcb, 0x4f, 0x06, 0x04, 0xc5, 0xcb, 0x11, 0x17, 0xc1, 0xcb, 0x11, 0x17,
        0x05, 0x20, 0xf5, 0x22, 0x23, 0x22, 0x23, 0xc9, 0xce, 0xed, 0x66, 0x66, 0xcc, 0x0d, 0x00, 0x0b,
        0x03, 0x73, 0x00, 0x83, 0x00, 0x0c, 0x00, 0x0d, 0x00, 0x08, 0x11, 0x1f, 0x88, 0x89, 0x00, 0x0e,
        0xdc, 0xcc, 0x6e, 0xe6, 0xdd, 0xdd, 0xd9, 0x99, 0xbb, 0xbb, 0x67, 0x63, 0x6e, 0x0e, 0xec, 0xcc,
        0xdd, 0xdc, 0x99, 0x9f, 0xbb, 0xb9, 0x33, 0x3e, 0x3c, 0x42, 0xb9, 0xa5, 0xb9, 0xa5, 0x42, 0x3c,
        0x21, 0x04, 0x01, 0x11, 0xa8, 0x00, 0x1a, 0x13, 0xbe, 0x20, 0xfe, 0x23, 0x7d, 0xfe, 0x34, 0x20,
        0xf5, 0x06, 0x19, 0x78, 0x86, 0x23, 0x05, 0x20, 0xfb, 0x86, 0x20, 0xfe, 0x3e, 0x01, 0xe0, 0x50,],
      gb_cartridge : [0; 0x10000],
      boot_rom_locked : false,
    }
  }
}

impl Mmu {

  pub fn fetch(&self, address : u16) -> u8{
    let (mirror_address,_) = self.mirror_address(address as usize);
    if address < 0x100 && !self.boot_rom_locked{
      return self.gb_internal_rom[mirror_address];
    } else {
      return self.gb_cartridge[mirror_address];  
    }    
  }

  pub fn save(&mut self, address : u16, value:u8){
    let (mirror_address,writable) = self.mirror_address(address as usize);
    if writable {
      if mirror_address == REG_DIV { //writing to the divider register zeros it.
        self.gb_cartridge[mirror_address] = 0x00;
      } else if mirror_address == BOOT_ROM_LOCKOUT {
        self.gb_cartridge[mirror_address] = value;
        self.boot_rom_locked = true;
      } else {
        self.gb_cartridge[mirror_address] = value;
      }
    }
  }

  pub fn mirror_address(&self, address : usize) -> (usize,bool){
    if address >= 0xE000 && address < 0xFE00 {
      (address - 0x2000,true)
    } else if address < 0x8000 || (address >= 0xFEA0 && address < 0xFF00){
      (address,false)
    } else {
      (address,true)
    }
  }

  pub fn set_bit(&mut self, loc : u16, bit : Bit) {
    let temp = self.fetch(loc) | bit as u8;
    self.save(loc, temp);
  }

  pub fn set_bit_usize(&mut self, loc : usize, bit : Bit) {
    let temp = self.fetch(loc as u16) | bit as u8;
    self.save(loc as u16, temp);
  }

  pub fn get_bit(&self, loc : u16, bit : Bit) -> bool{
    return (self.fetch(loc) & bit as u8) > 0;
  }

  pub fn get_bit_usize(&self, loc : usize, bit : Bit) -> bool{
    return (self.fetch(loc as u16) & bit as u8) > 0;
  }

}

#[derive(Clone,Copy)]
pub enum Bit {
  One   = 0b00000001,
  Two   = 0b00000010,
  Three = 0b00000100,
  Four  = 0b00001000,
  Five  = 0b00010000,
  Six   = 0b00100000,
  Seven = 0b01000000,
  Eight = 0b10000000,
}

impl From<u8> for Bit {
    fn from(b: u8) -> Self {
      match b {
        1 => Bit::One,
        2 => Bit::Two,
        3 => Bit::Three,
        4 => Bit::Four,
        5 => Bit::Five,
        6 => Bit::Six,
        7 => Bit::Seven,
        8 => Bit::Eight,
        _ => panic!("Tried to create an out of bounds Bit")
      }       
    }
}


#[cfg(test)]
mod tests {

  use super::*;

  #[test]
  fn fetch_returns_boot_rom_if_flag_set(){
    let mut m : Mmu = Mmu::default();
    assert_eq!(m.fetch(0x00),0x31,"Boot rom data fetched if BOOT_ROM_LOCKOUT is false");
    m.boot_rom_locked = true;
    assert_eq!(m.fetch(0x00),0x00,"ROM data fetched if BOOT_ROM_LOCKOUT is true");
  }

  #[test]
  fn touching_boot_rom_lockout_locks_boot(){
    let mut m : Mmu = Mmu::default();
    assert_eq!(m.boot_rom_locked,false);
    m.save(BOOT_ROM_LOCKOUT as u16,0x00);
    assert_eq!(m.boot_rom_locked,true,"Writing to BOOT_ROM_LOCKOUT sets the boot rom locked flag");
  }

  #[test]
  fn mirror_addresses() {
    let mut m : Mmu = Mmu::default();
    m.gb_cartridge[0xFD99] = 10;
    m.gb_cartridge[0xDD99] = 99;
    assert_eq!(m.fetch(0xFD99),99,"Fetching 0xFD99 returns 0xDD99");
    m.gb_cartridge[0xE000] = 10;
    m.gb_cartridge[0xC000] = 99;
    assert_eq!(m.fetch(0xE000),99,"Fetching 0xE000 returns 0xC000");
  }

  #[test]
  fn unwriteable_address_ranges_0x100() {    
    let mut m : Mmu = Mmu::default();
    m.gb_cartridge[0x0FFF] = 10;
    m.save(0x0FFF,20);
    assert_eq!(m.gb_cartridge[0x0FFF], 10,"0x0FFF is unwriteable");
    m.gb_cartridge[0xFEA0] = 10;
    m.save(0xFEA0,20);
    assert_eq!(m.gb_cartridge[0xFEA0], 10,"0xFEA0 is unwriteable");
    m.gb_cartridge[0xFEFF] = 10;
    m.save(0xFEFF,20);
    assert_eq!(m.gb_cartridge[0xFEFF], 10,"0xFEFF is unwriteable");
  }

  #[test]
  fn set_bit() {
    let mut m : Mmu = Mmu::default();
    m.gb_cartridge[0x8000] = 0x00;
    m.set_bit(0x8000,Bit::One);
    assert_eq!(m.gb_cartridge[0x8000], 0b00000001, "Bit 1 set without changing lower bits" );

    m.set_bit(0x8000,Bit::Two);
    assert_eq!(m.gb_cartridge[0x8000], 0b00000011, "Bit 2 set without changing lower bits" );

    m.set_bit(0x8000,Bit::Three);
    assert_eq!(m.gb_cartridge[0x8000], 0b00000111, "Bit 3 set without changing lower bits" );

    m.set_bit(0x8000,Bit::Four);
    assert_eq!(m.gb_cartridge[0x8000], 0b00001111, "Bit 4 set without changing lower bits" );

    m.set_bit(0x8000,Bit::Five);
    assert_eq!(m.gb_cartridge[0x8000], 0b00011111, "Bit 5 set without changing lower bits" );

    m.set_bit(0x8000,Bit::Six);
    assert_eq!(m.gb_cartridge[0x8000], 0b00111111, "Bit 6 set without changing lower bits" );

    m.set_bit(0x8000,Bit::Seven);
    assert_eq!(m.gb_cartridge[0x8000], 0b01111111, "Bit 7 set without changing lower bits" );

    m.set_bit(0x8000,Bit::Eight);
    assert_eq!(m.gb_cartridge[0x8000], 0b11111111, "Bit 8 set without changing lower bits" );
  }
}