use mmu::INTERRUPT_FLAGS;
use mmu::{Mmu,Bit,INTERRUPT_ENABLED};

const ZERO_FLAG:  u8 = 1 << 7;
const NEG_FLAG:   u8 = 1 << 6;
const HALF_FLAG:  u8 = 1 << 5;
const CARRY_FLAG: u8 = 1 << 4;

#[derive(Clone,Copy)]
#[allow(non_camel_case_types)]
pub enum Register{
  A,
  AF,
  F,
  B,
  BC,
  C,
  D,
  DE,
  E,
  H,
  HL,
  L,
  SP,
  PC,
  HL_address,
}

#[allow(non_snake_case)]
#[derive(Default)]
pub struct Cpu {
  A: u8,
  F: u8,
  B: u8,
  C: u8,
  D: u8,
  E: u8,
  H: u8,
  L: u8,
  SP: u16,
  PC: u16,
  interrupt_enabled: bool,

  pub mmu : Mmu,
}

macro_rules! apply8 {
  ($cpu:ident,$r:ident,$f:ident,$num:expr) => {
    { let reg = $cpu.load_register8(Register::$r);
      let num = $num;
      let tmp = $cpu.$f(reg,num);
      $cpu.save_register8(Register::$r,tmp);
    }
  };
}

macro_rules! ld_8 {
  ($cpu:ident,$r:ident,$l:ident) => {{
      let x = $cpu.load_register8(Register::$l);
      $cpu.save_register8(Register::$r,x);
    }
  };
}

macro_rules! pop {
  ($cpu:ident,$r:ident) => {{ 
    let sp = $cpu.load_register16(Register::SP);
    let l = $cpu.load_address(sp);
    let h = $cpu.load_address(sp.wrapping_add(1));
    //print!(" POP! {:>04x}, {:>02x}, {:>02x},", ((h as u16) << 8) + (l as u16), h as u8, l as u8);
    let hl = ((h as u16) << 8) + (l as u16);
    $cpu.save_register16(Register::$r, hl);
    $cpu.save_register16(Register::SP, sp.wrapping_add(2));
    }
  };
}

macro_rules! push {
  ($cpu:ident,$r:ident) => {{
      let sp = $cpu.load_register16(Register::SP);
      let pc = $cpu.load_register16(Register::$r);
      //print!(" PUSH! {:>04x}, {:>02x}, {:>02x},", pc, pc as u8, (pc>>8) as u8,);

      $cpu.save_address(sp.wrapping_sub(2), pc as u8);
      $cpu.save_address(sp.wrapping_sub(1), (pc >> 8) as u8);
      $cpu.save_register16(Register::SP, sp.wrapping_sub(2));
    }
  };
}


impl Cpu {

  pub fn process(&mut self, count : u16){
    for _ in 0..count { 
      self.process_next_instruction();
    }
  }

  fn fetch_u8(&mut self) -> u8{
    let x = self.mmu.fetch(self.PC);
    self.PC = self.PC + 1;
    return x;
  }

  fn fetch_i8(&mut self) -> i8{
    return self.fetch_u8() as i8;
  }

  fn fetch_u16(&mut self) -> u16{
    let x = self.fetch_u8();
    let y = self.fetch_u8();
    return ((y as u16) << 8) + (x as u16);
  }

  fn zero_flags(&mut self){
    self.F = self.F & 0b00001111
  }

  fn set_flag(&mut self, flag: u8, value: bool){
    if value {
      self.F = self.F | flag;
    } else {
      self.F = self.F & !flag;
    }
  }

  fn get_flag(&self, flag: u8) -> bool
  {
    if (self.F & flag) > 0 {
      true
    } else {
        false
    }
  }

  fn load_address(&self, loc: u16) -> u8{
    return self.mmu.fetch(loc);
  }

  fn load_register_address(&self, reg: Register) -> u8{
    let x = self.load_register16(reg);
    return self.mmu.fetch(x);
  }

  fn save_address(&mut self, loc: u16, value :u8) {
    self.mmu.save(loc, value);
  }

  fn save_register_address(&mut self, reg: Register, value :u8){
    let x = self.load_register16(reg);
    self.mmu.save(x, value);
  }

  fn load_register8(&self, r: Register) -> u8{
    match r {
      Register::A => self.A,
      Register::B => self.B,
      Register::C => self.C,
      Register::D => self.D,
      Register::E => self.E,
      Register::F => self.F,
      Register::H => self.H,
      Register::L => self.L,
      Register::HL_address => self.load_register_address(Register::HL),
      _ => panic!("Attempted to load a 16 bit register as 8 bit"),
    }
  }

  fn load_register16(&self, r: Register) -> u16{
    match r {
      Register::AF => (((self.A as u16) << 8) + self.F as u16) as u16,
      Register::BC => (((self.B as u16) << 8) + self.C as u16) as u16,
      Register::DE => (((self.D as u16) << 8) + self.E as u16) as u16,
      Register::HL => (((self.H as u16) << 8) + self.L as u16) as u16,
      Register::SP => self.SP as u16,
      Register::PC => self.PC as u16,
      _ => panic!("Attempted to load an 8 bit register as 16 bit"),
    }
  }

  fn save_register8(&mut self, r : Register, value : u8){
    match r {
      Register::A => self.A = value,
      Register::B => self.B = value,
      Register::C => self.C = value,
      Register::D => self.D = value,
      Register::E => self.E = value,
      Register::F => self.F = value,
      Register::H => self.H = value,
      Register::L => self.L = value,
      Register::HL_address => self.save_register_address(Register::HL,value),
      _ => panic!("Attempted to save a 16 bit register as 8 bit"),
    }
  }

  fn save_register16(&mut self, r: Register, value : u16){
    match r {
      Register::AF => {self.A = (value >> 8) as u8; self.F = value as u8},
      Register::BC => {self.B = (value >> 8) as u8; self.C = value as u8},
      Register::DE => {self.D = (value >> 8) as u8; self.E = value as u8},
      Register::HL => {self.H = (value >> 8) as u8; self.L = value as u8},
      Register::SP => self.SP = value,
      Register::PC => self.PC = value,
      _ => panic!("Attempted to save an 8 bit register as 16 bit"),
    }
  }  

  fn and8(&mut self, a: u8, b: u8) -> u8{
    self.zero_flags();
    self.set_flag(HALF_FLAG,true);
    let x = a & b;
    self.set_flag(ZERO_FLAG, x == 0);
    return x;
  }

  fn xor8(&mut self, a: u8, b: u8) -> u8{
    self.zero_flags();
    let x = a ^ b;
    self.set_flag(ZERO_FLAG, x == 0);
    return x;    
  }

  fn or8(&mut self, a: u8, b: u8) -> u8{
    self.zero_flags();
    let x = a | b;
    self.set_flag(ZERO_FLAG, x == 0);
    return x;
  }


  fn cp8(&mut self, a: u8, b: u8) -> u8{
    //this is a little weird - since cp doesn't change the accumulator, and just sets the Z flag it is possible to use
    //sub8 to do this. Just ignore the return value and return the accumulator
    //according to the documentation for the Z80 this pretty much uses SUB under the hood and ignores the result.
    let _x = self.sub8(a,b);
    return a;
  }

  fn rlc(&mut self, a : Register){
    let val = self.load_register8(a);
    let highest_bit = val & 0b10000000;
    self.save_register8(a, val.rotate_left(1)); 
    self.zero_flags();
    self.set_flag(CARRY_FLAG, highest_bit > 0);
    self.set_flag(ZERO_FLAG, val == 0);
  }

  fn rrc(&mut self, a: Register){
    let val = self.load_register8(a);
    let lowest_bit = val & 0b00000001;

    self.save_register8(a, val.rotate_right(1));
    self.zero_flags();
    self.set_flag(CARRY_FLAG, lowest_bit > 0);
    self.set_flag(ZERO_FLAG, val == 0);
  }

  fn rl(&mut self, a : Register){
    let val = self.load_register8(a);
    let highest_bit = val & 0b10000000;
    let rot_val = (val.rotate_left(1) & 0b11111110) | (self.get_flag(CARRY_FLAG) as u8);
    self.zero_flags();
    self.set_flag(CARRY_FLAG, highest_bit > 0);
    self.set_flag(ZERO_FLAG, rot_val == 0);
    self.save_register8(a, rot_val);
  }

  fn rr(&mut self, a : Register){
    let val = self.load_register8(a);
    let lowest_bit = val & 0b00000001;
    let rot_val = (val.rotate_right(1) & 0b11111110) | (self.get_flag(CARRY_FLAG) as u8); 
    self.zero_flags();
    self.set_flag(CARRY_FLAG, lowest_bit > 0);
    self.set_flag(ZERO_FLAG, rot_val == 0);
    self.save_register8(a, rot_val);
  }

  fn sla(&mut self, a : Register){
    let val = self.load_register8(a);
    let highest_bit = val & 0b10000000;
    let shift_val = val << 1;
    self.zero_flags();
    self.set_flag(CARRY_FLAG, highest_bit > 0);
    self.set_flag(ZERO_FLAG, shift_val == 0);
    self.save_register8(a, shift_val);
  }

  fn sra(&mut self, a: Register){
    let val = self.load_register8(a);
    let highest_bit = val & 0b10000000;
    let lowest_bit  = val & 0b00000001;
    let shift_val = ((val >> 1) | 0b01111111) & highest_bit;
    self.zero_flags();
    self.set_flag(CARRY_FLAG, lowest_bit > 0);
    self.set_flag(ZERO_FLAG,shift_val == 0);
    self.save_register8(a,shift_val);
  }

  fn swap(&mut self, a: Register){
    let val = self.load_register8(a);
    let left = val >> 4;
    let right = val << 4;
    let both = right & left;
    self.zero_flags();
    self.set_flag(ZERO_FLAG, both == 0);
    self.save_register8(a, both);
  }

  fn srl(&mut self, a : Register){
    let val = self.load_register8(a);
    let lowest_bit = val & 0b00000001;
    let shift_val = val >> 1;
    self.zero_flags();
    self.set_flag(ZERO_FLAG, shift_val == 0);
    self.set_flag(CARRY_FLAG, lowest_bit > 0);
    self.save_register8(a,shift_val);
  }

  fn bit(&mut self, a : Register, bit: Bit){
    let val = self.load_register8(a);
    let complement_bit = (val & bit as u8) == 0;
    self.set_flag(ZERO_FLAG, complement_bit);
    self.set_flag(HALF_FLAG, true);
    self.set_flag(NEG_FLAG, false);

    //print!(" BIT {} VAL:{:>8b} CPL:{} ", bit as u8,  val, complement_bit);

  }

  fn res(&mut self, a : Register, bit : Bit){
    let val = self.load_register8(a);
    let reset_val = val & !(bit as u8);
    self.save_register8(a,reset_val);
  }

  fn set(&mut self, a : Register, bit : Bit){
    let val = self.load_register8(a);
    let set_val = val | (bit as u8);
    self.save_register8(a,set_val);
  }

  fn add8(&mut self, a: u8, b:u8) -> u8{
    let hf = (((a & 0xf) + (b & 0xf)) & 0x10) == 0x10;
    let (tmp,ov) = a.overflowing_add(b);
    self.set_flag(NEG_FLAG, false);
    self.set_flag(ZERO_FLAG, tmp == 0);
    self.set_flag(CARRY_FLAG, ov);
    self.set_flag(HALF_FLAG, hf);
    return tmp;
  }

  fn sub8(&mut self, a: u8, b:u8) -> u8{
    //print!(" {:4x}, {:4x} ", a, b);
    let hf = (a & 0xf) < (b & 0xf);
    let (tmp,ov) = a.overflowing_sub(b);
    self.set_flag(NEG_FLAG, true);
    self.set_flag(ZERO_FLAG, tmp == 0);
    //print!(" Z {}", tmp == 0);
    self.set_flag(CARRY_FLAG, ov);
    self.set_flag(HALF_FLAG, hf);
    return tmp;
  }

  fn add16(&mut self, a:u16, b:u16) -> u16 {
    let hf = (a & 0xfff) + (b &0xfff) == 0x1000;
    let (tmp,ov) = a.overflowing_add(b);
    self.set_flag(NEG_FLAG, false); 
    self.set_flag(HALF_FLAG, hf);
    self.set_flag(CARRY_FLAG, ov);
    return tmp;
  }

  pub fn process_next_instruction(&mut self) {
    //print!("{:4x} ", self.load_register16(Register::PC));
    match self.fetch_u8() {
      0x00 => {
        //print!("NOP");
      } 
      , 
      0x01 => { let x = self.fetch_u16();
                self.save_register16(Register::BC, x);
                //print!("LD BC {:4x}",x)
              },
      0x02 => {  let y = self.load_register8(Register::A);
                self.save_register_address(Register::BC,y);
                //print!("LD (BC) A")
              },
      0x03 => {  let tmp = self.load_register16(Register::BC).wrapping_add(1);
                self.save_register16(Register::BC, tmp);
                //print!("INC BC")
              },
      0x04 => { apply8!(self ,B , add8 , 1);
                //print!("INC B")
              },
      0x05 => { apply8!(self ,B , sub8 , 1);
                //print!("DEC B")
              },
      0x06 => { let x = self.fetch_u8();
                self.save_register8(Register::B, x);
                 //print!("LD B {:2x}", x)
              },
      0x07 => { let x = self.A & 0b10000000;
                self.A = self.A.rotate_left(1); 
                self.zero_flags();
                self.set_flag(CARRY_FLAG, x > 0); 
                //print!("RLCA")
              },
      0x08 => {  let x = self.load_register16(Register::SP);
                let y = self.fetch_u16();
                self.save_address(y, x as u8);
                self.save_address(y+1, (x >> 8) as u8);
                //print!("LD ({:4x}) SP", y)
              },
      0x09 => { let x = self.load_register16(Register::HL);
                let y = self.load_register16(Register::BC);
                let z = self.add16(x,y);
                self.save_register16(Register::HL,z);
                //print!("ADD HL BC")
              },
      0x0A => { let y = self.load_register_address(Register::BC);
                self.save_register8(Register::A, y);
                //print!("LD A (BC)") 
              },
      0x0B => { let tmp = self.load_register16(Register::BC).wrapping_sub(1);
                self.save_register16(Register::BC, tmp);
                //print!("DEC BC")
              },
      0x0C => { apply8!(self ,C , add8 , 1);
                //print!("INC C")
              },
      0x0D => { apply8!(self,C , sub8 , 1);
                //print!("DEC C")
              }, 
      0x0E => { let x = self.fetch_u8();
                self.save_register8(Register::C, x);
                //print!("LD C {:2x}", x)
               },
      0x0F => { let x = self.A & 0b1; 
                self.A = self.A.rotate_right(1);  
                self.zero_flags(); 
                self.set_flag(CARRY_FLAG, x > 0); 
                //print!("RRCA")
              },
      0x10 => {//print!("unimplemented STOP")
              },
      0x11 => { let x = self.fetch_u16();
                self.save_register16(Register::DE, x);
                //print!("LD DE {:4x}", x)
              },
      0x12 => {  let y = self.load_register8(Register::A);
                self.save_register_address(Register::DE,y);
                //print!("LD (DE) A")
              },
      0x13 => { let tmp = self.load_register16(Register::DE).wrapping_add(1);
                self.save_register16(Register::DE, tmp);
                //print!("INC DE")
              },
      0x14 => { apply8!(self,D , add8 , 1);
                //print!("INC D")
              },
      0x15 => { apply8!(self,D , sub8 , 1);
                //print!("DEC D") 
              },
      0x16 => { let x = self.fetch_u8(); 
                self.save_register8(Register::D, x);
                //print!("LD D {:2x}",x)
              },
      0x17 => { let x = self.A & 0b10000000; 
                self.A = self.A.rotate_left(1); 
                self.A = (self.A & 0b11111110) | (self.get_flag(CARRY_FLAG) as u8); 
                self.zero_flags(); 
                self.set_flag(CARRY_FLAG, x > 0); 
                //print!("RLA")
              },
      0x18 => { let x = self.fetch_i8(); 
                self.PC = ((self.PC as i16).wrapping_add(x as i16)) as u16;
                 //print!("JR {:4x}", x)
              },
      0x19 => { let x = self.load_register16(Register::HL);
                let y = self.load_register16(Register::DE);
                let z = self.add16(x,y);
                self.save_register16(Register::HL,z);
                //print!("ADD HL DE")
              },
      0x1A => { let y = self.load_register_address(Register::BC);
                self.save_register8(Register::A,y);
                //print!("LD A (DE)")
              },
      0x1B => { let tmp = self.load_register16(Register::DE).wrapping_sub(1);
                self.save_register16(Register::DE, tmp);
                //print!("DEC DE")
              },
      0x1C => { apply8!(self,E , add8 , 1);
                //print!("INC E")
              },
      0x1D => { apply8!(self,E , sub8 , 1);
                //print!("DEC E")
              },
      0x1E => { let x = self.fetch_u8(); 
                self.save_register8(Register::E, x);
                //print!("LD E {:4x}", x)
              },
       0x1F => { let x = self.A & 0b1; 
                self.A = self.A.rotate_right(1); 
                self.A = (self.A & 0b01111111) | ((self.get_flag(CARRY_FLAG) as u8) << 7); 
                self.zero_flags(); 
                self.set_flag(CARRY_FLAG, x > 0); 
                //print!("RRA")
              },
      0x20 => { let x = self.fetch_i8(); 
                if !(self.get_flag(ZERO_FLAG)) { 
                  self.PC = ((self.PC as i16).wrapping_add(x as i16)) as u16;
                };
                //print!("JR NZ {:4x}", x)
              },
      0x21 => { let x = self.fetch_u16();
                self.save_register16(Register::HL, x);
                //print!("LD HL {:4x}", x) 
              },
      0x22 => { let x = self.load_register16(Register::HL);
                let y = self.load_register8(Register::A);
                self.save_address(x,y);
                self.save_register16(Register::HL, x.wrapping_add(1));
                //print!("LD (HL+) A, {:4x}", y)
              },
      0x23 => { let tmp = self.load_register16(Register::HL).wrapping_add(1);
                self.save_register16(Register::HL, tmp);
                //print!("INC HL")
              },
      0x24 => { apply8!(self,H , add8 , 1);
                //print!("INC H")
              },
      0x25 => { apply8!(self,H , sub8 , 1);
                //print!("DEC H")
              },
      0x26 => { let x = self.fetch_u8();
                self.save_register8(Register::H, x);
                //print!("LD H,{:4x}", x)
              },
      0x27 => { //print!("unimplemented DAA") 
              },
      0x28 => { let x = self.fetch_i8(); 
                if self.get_flag(ZERO_FLAG) { 
                  self.PC = ((self.PC as i16).wrapping_add(x as i16)) as u16;
                };
                //print!("JR Z {:4x}", x)
              },
      0x29 => { let x = self.load_register16(Register::HL);
                let y = self.load_register16(Register::HL);
                let z = self.add16(x,y);
                self.save_register16(Register::HL,z);
                //print!("ADD HL,HL")
              },
      0x2A => { let x = self.load_register16(Register::HL);
                let y = self.load_address(x);
                self.save_register8(Register::A,y);
                self.save_register16(Register::HL, x.wrapping_add(1));
                //print!("LD A (HL+), {:4x}", y)
              }, 
      0x2B => { let tmp = self.load_register16(Register::HL).wrapping_sub(1);
                self.save_register16(Register::BC, tmp);
                //print!("DEC HL")
              },
      0x2C => { apply8!(self,L , add8 , 1);
                //print!("INC L")
              },
      0x2D => { apply8!(self,L , sub8 , 1);
                //print!("DEC L")
              },
      0x2E => { let x = self.fetch_u8(); 
                self.save_register8(Register::L, x);
                //print!("LD L {:4x}", x)
              },
      0x2F => { self.A = !self.A;
                //print!("CPL")
              },
      0x30 => { let x = self.fetch_i8();
                if !self.get_flag(CARRY_FLAG) {
                  self.PC = ((self.PC as i16).wrapping_add(x as i16)) as u16;
                };
                //print!("JR NC {:4x}", x) 
              },
      0x31 => { let x = self.fetch_u16();
                self.save_register16(Register::SP, x);
                //print!("LD SP {:4x}", x) 
              },
      0x32 => { let x = self.load_register16(Register::HL);
                let y = self.load_register8(Register::A);
                self.save_address(x,y);
                self.save_register16(Register::HL, x.wrapping_sub(1));
                //print!("LD (HL-) A, {:4x}", x) 
              },
      0x33 => { let x = self.load_register16(Register::SP).wrapping_add(1);
                self.save_register16(Register::SP, x);
                //print!("INC SP") 
              },
      0x34 => { let x = self.load_register_address(Register::HL);
                let y = self.add8(x,1);
                self.save_register_address(Register::HL,y);
                //print!("INC (HL)") 
              },
      0x35 => { let x = self.load_register_address(Register::HL);
                let y = self.sub8(x,1);
                self.save_register_address(Register::HL,y);
                //print!("DEC (HL)") 
              },
      0x36 => { let x = self.fetch_u8();
                self.save_register_address(Register::HL, x);
                //print!("LD (HL) d8") 
              },
      0x37 => { self.set_flag(CARRY_FLAG, true);
                self.set_flag(NEG_FLAG, false);
                self.set_flag(HALF_FLAG, false);
                //print!("SCF") 
              },
      0x38 => { let x = self.fetch_i8(); 
                if self.get_flag(CARRY_FLAG) { 
                  self.PC = ((self.PC as i16).wrapping_add(x as i16)) as u16;
                };
                //print!("JR C {:4x}", x) 
              },
      0x39 => { let x = self.load_register16(Register::HL);
                let y = self.load_register16(Register::SP);
                let z = self.add16(x,y);
                self.save_register16(Register::HL,z);
                //print!("ADD HL SP") 
              },
      0x3A => { let x = self.load_register16(Register::HL);
                let y = self.load_address(x);
                self.save_register8(Register::A,y);
                self.save_register16(Register::HL, x.wrapping_sub(1));
                //print!("LD A (HL-), {:4x}", y) 
              },
      0x3B => { let tmp = self.load_register16(Register::SP).wrapping_sub(1);
                self.save_register16(Register::BC, tmp);
                //print!("DEC SP") 
              },
      0x3C => { apply8!(self,A, add8, 1);
                //print!("INC A") 
              },
      0x3D => { apply8!(self,A, sub8, 1);
                //print!("DEC A") 
              },
      0x3E => { let x = self.fetch_u8();
                self.save_register8(Register::A,x);
                //print!("LD A d8") 
              },
      0x3F => { let x = !self.get_flag(CARRY_FLAG);
                self.set_flag(CARRY_FLAG, x);
                self.set_flag(NEG_FLAG, false);
                self.set_flag(HALF_FLAG, false);
                //print!("CCF") 
              },
      0x40 => { ld_8!(self,B,B);
                //print!("LD B B") 
              },
      0x41 => { ld_8!(self,B,C);
                //print!("LD B C") 
              },
      0x42 => { ld_8!(self,B,D);
                //print!("LD B D") 
              },
      0x43 => { ld_8!(self,B,E);
                //print!("LD B E") 
              },
      0x44 => { ld_8!(self,B,H);
                //print!("LD B H") 
              },
      0x45 => { ld_8!(self,B,L);
                //print!("LD B L") 
              },
      0x46 => { ld_8!(self,B,HL_address);
                //print!("LD B (HL)") 
              },
      0x47 => { ld_8!(self,B,A);
                //print!("LD B A") 
              },
      0x48 => { ld_8!(self,C,B);
                //print!("LD C B") 
              },
      0x49 => { ld_8!(self,C,C);
                //print!("LD C C") 
              },
      0x4A => { ld_8!(self,C,D);
                //print!("LD C D") 
              },
      0x4B => { ld_8!(self,C,E);
                //print!("LD C E") 
              },
      0x4C => { ld_8!(self,C,H);
                //print!("LD C H") 
              },
      0x4D => { ld_8!(self,C,L);
                //print!("LD C L") 
              },
      0x4E => { ld_8!(self,C,HL_address);
                //print!("LD C (HL)") 
              },
      0x4F => { ld_8!(self,C,A);
                //print!("LD C A") 
              },
      0x50 => { ld_8!(self,D,C);
                //print!("LD D B") 
              },
      0x51 => { ld_8!(self,D,C);
                //print!("LD D C") 
              },
      0x52 => { ld_8!(self,D,D);
                //print!("LD D D") 
              },
      0x53 => { ld_8!(self,D,E);
                //print!("LD D E") 
              },
      0x54 => { ld_8!(self,D,H);
                //print!("LD D H") 
              },
      0x55 => { ld_8!(self,D,L);
                //print!("LD D L") 
              },
      0x56 => { ld_8!(self,D,HL_address);;
                //print!("LD D (HL)") 
              },
      0x57 => { ld_8!(self,D,A);
                //print!("LD D A") 
              },
      0x58 => { ld_8!(self,E,B);
                //print!("LD E B") 
              },
      0x59 => { ld_8!(self,E,C);
                //print!("LD E C") 
              },
      0x5A => { ld_8!(self,E,D);
                //print!("LD E D") 
              },
      0x5B => { ld_8!(self,E,E);
                //print!("LD E E") 
              },
      0x5C => { ld_8!(self,E,H);
                //print!("LD E H") 
              },
      0x5D => { ld_8!(self,E,L);
                //print!("LD E L") 
              },
      0x5E => { ld_8!(self,E,HL_address);
                //print!("LD E (HL)") 
              },
      0x5F => { ld_8!(self,E,A);
                //print!("LD E A") 
              },
      0x60 => { ld_8!(self,H,C);
                //print!("LD H B") 
              },
      0x61 => { ld_8!(self,H,C);
                //print!("LD H C") 
              },
      0x62 => { ld_8!(self,H,D);
                //print!("LD H D") 
              },
      0x63 => { ld_8!(self,H,E);
                //print!("LD H E") 
              },
      0x64 => { ld_8!(self,H,H);
                //print!("LD H H") 
              },
      0x65 => { ld_8!(self,H,L);
                //print!("LD H L") 
              },
      0x66 => { ld_8!(self,H,HL_address);
                //print!("LD H (HL)") 
              },
      0x67 => { ld_8!(self,H,A);
                //print!("LD H A") 
              },
      0x68 => { ld_8!(self,L,B);
                //print!("LD L B") 
              },
      0x69 => { ld_8!(self,L,C);
                //print!("LD L C") 
              },
      0x6A => { ld_8!(self,L,D);
                //print!("LD L D") 
              },
      0x6B => { ld_8!(self,L,E);
                //print!("LD L E") 
              },
      0x6C => { ld_8!(self,L,H);
                //print!("LD L H") 
              },
      0x6D => { ld_8!(self,L,L);
                //print!("LD L L") 
              },
      0x6E => { ld_8!(self,L,HL_address);
                //print!("LD L (HL)") 
              },
      0x6F => { ld_8!(self,L,A); 
                //print!("LD L A") 
              },
      0x70 => { ld_8!(self,HL_address,B);
                //print!("LD (HL) B") 
              },
      0x71 => { ld_8!(self,HL_address,C);
                //print!("LD (HL) C") 
              },
      0x72 => { ld_8!(self,HL_address,D);
                //print!("LD (HL) D") 
              },
      0x73 => { ld_8!(self,HL_address,E);
                //print!("LD (HL) E") 
              },
      0x74 => { ld_8!(self,HL_address,H);
                //print!("LD (HL) H") 
              },
      0x75 => { ld_8!(self,HL_address,L);
                //print!("LD (HL) L") 
              },
      0x76 => { //print!("unimplemented HALT")
       },
      0x77 => { ld_8!(self,HL_address,A);
                //print!("LD (HL) A")
              },
      0x78 => { ld_8!(self,A,B);
                //print!("LD A B") 
              },
      0x79 => { ld_8!(self,A,C);
                //print!("LD A C") 
              },
      0x7A => { ld_8!(self,A,D);
                //print!("LD A D") 
              },
      0x7B => { ld_8!(self,A,E);
                //print!("LD A E") 
              },
      0x7C => { ld_8!(self,A,H);
                //print!("LD A H") 
              },
      0x7D => { ld_8!(self,A,L);
                //print!("LD A L") 
              },
      0x7E => { let x = self.load_register_address(Register::HL);
                self.save_register8(Register::A,x);
                //print!("LD A (HL)") 
              },
      0x7F => { ld_8!(self,A,A);
                //print!("LD A A") 
              },
      0x80 => { apply8!(self,A,add8,self.load_register8(Register::B));
                //print!("ADD A B") 
              },
      0x81 => { apply8!(self,A,add8,self.load_register8(Register::C));
                //print!("ADD A C") 
              },
      0x82 => { apply8!(self,A,add8,self.load_register8(Register::D));
                //print!("ADD A D") 
              },
      0x83 => { apply8!(self,A,add8,self.load_register8(Register::E));
                //print!("ADD A E") 
              },
      0x84 => { apply8!(self,A,add8,self.load_register8(Register::H));
                //print!("ADD A H") 
              },
      0x85 => { apply8!(self,A,add8,self.load_register8(Register::L));
                //print!("ADD A L") 
              },
      0x86 => { apply8!(self,A,add8,self.load_register_address(Register::HL));
                //print!("ADD A (HL)") 
              },
      0x87 => { apply8!(self,A,add8,self.load_register8(Register::A));
                //print!("ADD A A") 
              },
      0x88 => { apply8!(self,A,add8,self.load_register8(Register::B).wrapping_add(self.get_flag(CARRY_FLAG) as u8));
                //print!("ADC A B") 
              },
      0x89 => { apply8!(self,A,add8,self.load_register8(Register::C).wrapping_add(self.get_flag(CARRY_FLAG) as u8));
                //print!("ADC A C") 
              },
      0x8A => { apply8!(self,A,add8,self.load_register8(Register::D).wrapping_add(self.get_flag(CARRY_FLAG) as u8));
                //print!("ADC A D") 
              },
      0x8B => { apply8!(self,A,add8,self.load_register8(Register::E).wrapping_add(self.get_flag(CARRY_FLAG) as u8));
                //print!("ADC A E") 
              },
      0x8C => { apply8!(self,A,add8,self.load_register8(Register::H).wrapping_add(self.get_flag(CARRY_FLAG) as u8));
                //print!("ADC A H") 
              },
      0x8D => { apply8!(self,A,add8,self.load_register8(Register::L).wrapping_add(self.get_flag(CARRY_FLAG) as u8));
                //print!("ADC A L") 
              },
      0x8E => { apply8!(self,A,add8,self.load_register_address(Register::HL).wrapping_add(self.get_flag(CARRY_FLAG) as u8));
                //print!("ADC A (HL)") 
              },
      0x8F => { apply8!(self,A,add8,self.load_register8(Register::A).wrapping_add(self.get_flag(CARRY_FLAG) as u8));
                //print!("ADC A A") 
              },
      0x90 => { apply8!(self,A,sub8,self.load_register8(Register::B));
                //print!("ADD A B") 
              },
      0x91 => { apply8!(self,A,sub8,self.load_register8(Register::C));
                //print!("ADD A C") 
              },
      0x92 => { apply8!(self,A,sub8,self.load_register8(Register::D));
                //print!("ADD A D") 
              },
      0x93 => { apply8!(self,A,sub8,self.load_register8(Register::E));
                //print!("ADD A E") 
              },
      0x94 => { apply8!(self,A,sub8,self.load_register8(Register::H));
                //print!("ADD A H") 
              },
      0x95 => { apply8!(self,A,sub8,self.load_register8(Register::L));
                //print!("ADD A L") 
              },
      0x96 => { apply8!(self,A,sub8,self.load_register_address(Register::HL));
                //print!("ADD A (HL)") 
              },
      0x97 => { apply8!(self,A,sub8,self.load_register8(Register::A));
                //print!("ADD A A") 
              },
      0x98 => { apply8!(self,A,sub8,self.load_register8(Register::B).wrapping_add(self.get_flag(CARRY_FLAG) as u8));
                //print!("ADC A B") 
              },
      0x99 => { apply8!(self,A,sub8,self.load_register8(Register::C).wrapping_add(self.get_flag(CARRY_FLAG) as u8));
                //print!("ADC A C") 
              },
      0x9A => { apply8!(self,A,sub8,self.load_register8(Register::D).wrapping_add(self.get_flag(CARRY_FLAG) as u8));
                //print!("ADC A D") 
              },
      0x9B => { apply8!(self,A,sub8,self.load_register8(Register::E).wrapping_add(self.get_flag(CARRY_FLAG) as u8));
                //print!("ADC A E") 
              },
      0x9C => { apply8!(self,A,sub8,self.load_register8(Register::H).wrapping_add(self.get_flag(CARRY_FLAG) as u8));
                //print!("ADC A H") 
              },
      0x9D => { apply8!(self,A,sub8,self.load_register8(Register::L).wrapping_add(self.get_flag(CARRY_FLAG) as u8));
                //print!("ADC A L") 
              },
      0x9E => { apply8!(self,A,sub8,self.load_register_address(Register::HL).wrapping_add(self.get_flag(CARRY_FLAG) as u8));
                //print!("ADC A (HL)") 
              },
      0x9F => { apply8!(self,A,sub8,self.load_register8(Register::A).wrapping_add(self.get_flag(CARRY_FLAG) as u8));
                //print!("ADC A A") 
              },
      0xA0 => { apply8!(self,A,and8,self.load_register8(Register::B));
                //print!("AND B") 
              },
      0xA1 => { apply8!(self,A,and8,self.load_register8(Register::C));
                //print!("AND C") 
              },
      0xA2 => { apply8!(self,A,and8,self.load_register8(Register::D));
                //print!("AND D") 
              },
      0xA3 => { apply8!(self,A,and8,self.load_register8(Register::E));
                //print!("AND E") 
              },
      0xA4 => { apply8!(self,A,and8,self.load_register8(Register::H));
                //print!("AND H") 
              },
      0xA5 => { apply8!(self,A,and8,self.load_register8(Register::L));
                //print!("AND L") 
              },
      0xA6 => { apply8!(self,A,and8,self.load_register_address(Register::HL));
                //print!("AND (HL)") 
              },
      0xA7 => { apply8!(self,A,and8,self.load_register8(Register::A));
                //print!("AND A") 
              },
      0xA8 => { apply8!(self,A,xor8,self.load_register8(Register::B));
                //print!("XOR B") 
              },
      0xA9 => { apply8!(self,A,xor8,self.load_register8(Register::C));
                //print!("XOR C") 
              },
      0xAA => { apply8!(self,A,xor8,self.load_register8(Register::D));
                //print!("XOR D") 
              },
      0xAB => { apply8!(self,A,xor8,self.load_register8(Register::E));
                //print!("XOR E") 
              },
      0xAC => { apply8!(self,A,xor8,self.load_register8(Register::H));
                //print!("XOR H") 
              },
      0xAD => { apply8!(self,A,xor8,self.load_register8(Register::L));
                //print!("XOR L") 
              },
      0xAE => { apply8!(self,A,xor8,self.load_register_address(Register::HL));
                //print!("XOR (HL)") 
              },
      0xAF => { apply8!(self,A,xor8,self.load_register8(Register::A));
                //print!("XOR A") 
              },
      0xB0 => { apply8!(self,A,or8,self.load_register8(Register::B));
                //print!("OR B") 
              },
      0xB1 => { apply8!(self,A,or8,self.load_register8(Register::C));
                //print!("OR C") 
              },
      0xB2 => { apply8!(self,A,or8,self.load_register8(Register::D));
                //print!("OR D") 
              },
      0xB3 => { apply8!(self,A,or8,self.load_register8(Register::E));
                //print!("OR E") 
              },
      0xB4 => { apply8!(self,A,or8,self.load_register8(Register::H));
                //print!("OR H") 
              },
      0xB5 => { apply8!(self,A,or8,self.load_register8(Register::L));
                //print!("OR L") 
              },
      0xB6 => { apply8!(self,A,or8,self.load_register_address(Register::HL));
                //print!("OR (HL)") 
              },
      0xB7 => { apply8!(self,A,or8,self.load_register8(Register::A));
                //print!("OR A") 
              },
      0xB8 => { apply8!(self,A,cp8,self.load_register8(Register::B));
                //print!("CP B") 
              },
      0xB9 => { apply8!(self,A,cp8,self.load_register8(Register::C));
                //print!("CP C") 
              },
      0xBA => { apply8!(self,A,cp8,self.load_register8(Register::D));
                //print!("CP D") 
              },
      0xBB => { apply8!(self,A,cp8,self.load_register8(Register::E));
                //print!("CP E") 
              },
      0xBC => { apply8!(self,A,cp8,self.load_register8(Register::H));
                //print!("CP H") 
              },
      0xBD => { apply8!(self,A,cp8,self.load_register8(Register::L));
                //print!("CP L") 
              },
      0xBE => { apply8!(self,A,cp8,self.load_register_address(Register::HL));
                //print!("CP (HL)") 
              },
      0xBF => { apply8!(self,A,cp8,self.load_register8(Register::A));
                //print!("CP A") 
              },
      0xC0 => { if !self.get_flag(ZERO_FLAG) {
                pop!(self,PC)
                };
                //print!("RET NZ") 
              },
      0xC1 => { pop!(self,BC);
                //print!("POP BC")
              },
      0xC2 => { let x = self.fetch_u16();
                if !self.get_flag(ZERO_FLAG) {
                  self.save_register16(Register::PC, x);
                };
                //print!("JP NZ {:4x}", x) 
              },
      0xC3 => { let x = self.fetch_u16();
                self.save_register16(Register::PC, x);
                //print!("JP {:4x}", x) 
              }, 
      0xC4 => { let a16 = self.fetch_u16();
                if !self.get_flag(ZERO_FLAG) {
                  push!(self,PC);
                  self.save_register16(Register::PC, a16);
                };
                //print!("CALL NZ {:4x}", a16) 
              },
      0xC5 => { push!(self,BC);
                //print!("PUSH BC") 
              },
      0xC6 => { apply8!(self,A,add8,self.fetch_u8());
                //print!("ADD A d8") 
              },
      0xC7 => { push!(self,PC);
                self.save_register16(Register::PC, 0x00);
                //print!("RST 0x00") 
              },
      0xC8 => { if self.get_flag(ZERO_FLAG) {
                  pop!(self,PC);
                };
                //print!("RET Z") 
              },
      0xC9 => { //print!("{:>02x}{:>02x} ",self.mmu.fetch(self.SP), self.mmu.fetch(self.SP+1));
                pop!(self,PC);
                //print!("RET") 
              },
      0xCA => { let x = self.fetch_u16();
                if self.get_flag(ZERO_FLAG) {
                  self.save_register16(Register::PC, x);
                };
                //print!("JP Z {:4x}", x) 
              },
      0xCB => { self.prefix_cb();
                //print!("PREFIX CB") 
              },
      0xCC => { let a16 = self.fetch_u16();
                if self.get_flag(ZERO_FLAG) {
                  push!(self,PC);
                  self.save_register16(Register::PC, a16);
                };
                //print!("CALL Z {:4x}", a16) 
              },
      0xCD => { push!(self,PC);
                let a16 = self.fetch_u16();
                self.save_register16(Register::PC, a16);
                //print!("CALL PC:{:>04x} {:4x}",self.PC, a16) 
              },
      0xCE => { apply8!(self,A,add8,self.fetch_u8().wrapping_add(self.get_flag(CARRY_FLAG) as u8));
                //print!("ADC d8") 
              },
      0xCF => { push!(self,PC);
                self.save_register16(Register::PC, 0x08);
                //print!("RST 0x08") 
              },
      0xD0 => { if !self.get_flag(CARRY_FLAG) {
                  pop!(self,PC)
                };
                //print!("RET NC") 
              },
      0xD1 => { pop!(self,DE);
                //print!("POP DE")
              },
      0xD2 => { let x = self.fetch_u16();
                if !self.get_flag(CARRY_FLAG) {
                  self.save_register16(Register::PC, x);
                };
                //print!("JP NC {:4x}", x)
              },
      0xD3 => { //print!("0xD3 N/A") 
    },
      0xD4 => { let a16 = self.fetch_u16();
                if !self.get_flag(CARRY_FLAG) {
                  push!(self,PC);
                  self.save_register16(Register::PC, a16);
                };
                //print!("CALL NC {:4x}", a16) 
              },
      0xD5 => { push!(self,DE);
                //print!("PUSH DE") 
              },
      0xD6 => { apply8!(self,A,sub8,self.fetch_u8());
                //print!("SUB d8") 
              },
      0xD7 => { push!(self,PC);
                self.save_register16(Register::PC, 0x10);
                //print!("RST 0x10") 
              },
      0xD8 => { if self.get_flag(CARRY_FLAG) {
                  pop!(self,PC);
                }
                //print!("RET C") 
              },
      0xD9 => { pop!(self,PC);
                self.interrupt_enabled = true;
                //print!("RETI") 
              },
      0xDA => { let x = self.fetch_u16();
                if self.get_flag(CARRY_FLAG) {
                  self.save_register16(Register::PC, x);
                };
                //print!("JP C {:4x}", x) 
              },
      0xDB => { //print!("0xDB N/A") 
    },
      0xDC => { let a16 = self.fetch_u16();
                if self.get_flag(CARRY_FLAG) {
                  push!(self,PC);              
                  self.save_register16(Register::PC, a16);
                };
                //print!("CALL C {:4x}", a16) 
              },
      0xDD => { //print!("0xDD N/A") 
    },
      0xDE => { apply8!(self,A,sub8,self.fetch_u8().wrapping_add(self.get_flag(CARRY_FLAG) as u8));
                //print!("SBC A d8") 
              },
      0xDF => { push!(self,PC);
                self.save_register16(Register::PC, 0x18);
                //print!("RST 0x18") 
              },
      0xE0 => { let x = self.fetch_u8() as u16 + 0xFF00;
                let y = self.load_address(x);
                self.save_register8(Register::A,y);
                //print!("LD (0xFF00 + {:4x}) A", x) 
              },
      0xE1 => { pop!(self,HL);
                //print!("POP HL") 
              },
      0xE2 => { let x = self.load_register8(Register::C) as u16 + 0xFF00;
                let y = self.load_address(x);
                self.save_register8(Register::A,y);
                //print!("LD (0xFF00 + C) A") 
              },
      0xE3 => { //print!("0xE3 N/A") 
              },
      0xE4 => { //print!("0xE4 N/A") 
              },
      0xE5 => { push!(self,HL);
                //print!("PUSH HL") 
              },
      0xE6 => { apply8!(self,A,and8,self.fetch_u8());
                //print!("AND d8") 
              },
      0xE7 => { push!(self,PC);
                self.save_register16(Register::PC, 0x20);
                //print!("RST 0x20") 
              },
      0xE8 => { let x = self.fetch_i8(); 
                self.SP = ((self.SP as i16).wrapping_add(x as i16)) as u16;
                //print!("ADD SP {:4x}", x) 
              },
      0xE9 => { let x = self.load_register16(Register::HL);
                self.save_register16(Register::PC, x);
                //print!("JP (HL)") 
              },
      0xEA => { let x = self.fetch_u16();
                let y = self.load_address(x);
                self.save_register8(Register::A, y);
                //print!("LD ({:4x}) A", x) 
              },
      0xEB => { //print!("0xEB N/A") 
              },
      0xEC => { //print!("0xEC N/A") 
              },
      0xED => { //print!("0xED N/A") 
              },
      0xEE => { apply8!(self,A,xor8,self.fetch_u8());
                //print!("XOR d8")
              },
      0xEF => { push!(self,PC);
                self.save_register16(Register::PC, 0x28);
                //print!("RST 0x28") 
              },
      0xF0 => { let x = self.fetch_u8() as u16 + 0xFF00;
                let y = self.load_address(x);
                self.save_register8(Register::A,y);
                //print!("LD A, (0xFF00 + {:4x})", x) 
              },
      0xF1 => { pop!(self,AF);
                //print!("POP AF") 
              },
      0xF2 => { let x = self.load_register8(Register::C) as u16 + 0xFF00;
                let y = self.load_address(x);
                self.save_register8(Register::A,y);
                //print!("LD A (0xFF00 + C") 
              },
      0xF3 => { self.interrupt_enabled = false;
                //print!("DI") 
              },
      0xF4 => { //print!("0xF4 N/A") 
    },
      0xF5 => { push!(self,AF);
                //print!("PUSH AF") 
              },
      0xF6 => { apply8!(self,A,or8,self.fetch_u8());
                //print!("OR d8") 
              },
      0xF7 => { push!(self,PC);
                self.save_register16(Register::PC, 0x30);
                //print!("RST 0x30") 
              },
      0xF8 => { let x = self.load_register16(Register::SP) + self.fetch_u8() as u16;
                self.save_register16(Register::HL,x);
                //print!("LD HL SP + {:4x}", x) 
              },
      0xF9 => { let x = self.load_register16(Register::HL);
                self.save_register16(Register::SP, x);
                //print!("LD SP HL") 
              },
      0xFA => { let x = self.fetch_u16();
                let _y = self.load_address(x);
                //print!("LD A ({:4x})", x)
              },
      0xFB => { self.interrupt_enabled = true;
                //print!("EI") 
              },
      0xFC => { //print!("0xFC N/A") 
              },
      0xFD => { //print!("0xFD N/A") 
              },
      0xFE => { apply8!(self,A,cp8,self.fetch_u8());
                //print!("CP d8") 
              },
      0xFF => { push!(self,PC);
                self.save_register16(Register::PC, 0x38);
                //print!("RST 0x38") 
              },
    };
    //print!("\n");
  }

  fn prefix_cb(&mut self){  

    let reg_vec = vec![Register::B,Register::C,Register::D,Register::E,Register::H,Register::L,Register::HL_address,Register::A];
    let instruction = self.fetch_u8();

    match instruction{
      0o000...0o007 => self.rlc(reg_vec[(instruction % 8) as usize]),
      0o010...0o017 => self.rrc(reg_vec[(instruction % 8) as usize]),
      0o020...0o027 => self.rl(reg_vec[(instruction % 8) as usize]),
      0o030...0o037 => self.rr(reg_vec[(instruction % 8) as usize]),
      0o040...0o047 => self.sla(reg_vec[(instruction % 8) as usize]),
      0o050...0o057 => self.sra(reg_vec[(instruction % 8) as usize]),
      0o060...0o067 => self.swap(reg_vec[(instruction % 8) as usize]),
      0o070...0o077 => self.srl(reg_vec[(instruction % 8) as usize]),
      0o100...0o107 => self.bit(reg_vec[(instruction % 8) as usize], Bit::One),
      0o110...0o117 => self.bit(reg_vec[(instruction % 8) as usize], Bit::Two),
      0o120...0o127 => self.bit(reg_vec[(instruction % 8) as usize], Bit::Three),
      0o130...0o137 => self.bit(reg_vec[(instruction % 8) as usize], Bit::Four),
      0o140...0o147 => self.bit(reg_vec[(instruction % 8) as usize], Bit::Five),
      0o150...0o157 => self.bit(reg_vec[(instruction % 8) as usize], Bit::Six),
      0o160...0o167 => self.bit(reg_vec[(instruction % 8) as usize], Bit::Seven),
      0o170...0o177 => self.bit(reg_vec[(instruction % 8) as usize], Bit::Eight),
      0o200...0o207 => self.res(reg_vec[(instruction % 8) as usize], Bit::One),
      0o210...0o217 => self.res(reg_vec[(instruction % 8) as usize], Bit::Two),
      0o220...0o227 => self.res(reg_vec[(instruction % 8) as usize], Bit::Three),
      0o230...0o237 => self.res(reg_vec[(instruction % 8) as usize], Bit::Four),
      0o240...0o247 => self.res(reg_vec[(instruction % 8) as usize], Bit::Five),
      0o250...0o257 => self.res(reg_vec[(instruction % 8) as usize], Bit::Six),
      0o260...0o267 => self.res(reg_vec[(instruction % 8) as usize], Bit::Seven),
      0o270...0o277 => self.res(reg_vec[(instruction % 8) as usize], Bit::Eight),
      0o300...0o307 => self.set(reg_vec[(instruction % 8) as usize], Bit::One),
      0o310...0o317 => self.set(reg_vec[(instruction % 8) as usize], Bit::Two),
      0o320...0o327 => self.set(reg_vec[(instruction % 8) as usize], Bit::Three),
      0o330...0o337 => self.set(reg_vec[(instruction % 8) as usize], Bit::Four),
      0o340...0o347 => self.set(reg_vec[(instruction % 8) as usize], Bit::Five),
      0o350...0o357 => self.set(reg_vec[(instruction % 8) as usize], Bit::Six),
      0o360...0o367 => self.set(reg_vec[(instruction % 8) as usize], Bit::Seven),
      0o370...0o377 => self.set(reg_vec[(instruction % 8) as usize], Bit::Eight),
      _ => panic!("invalid instruction given to prefix_cb"),
    }
  }

  pub fn interrupt(&mut self, interrupt : Interrupt){
    self.mmu.set_bit_usize(INTERRUPT_FLAGS, interrupt.get_bit());
    if self.interrupt_enabled && self.mmu.get_bit_usize(INTERRUPT_ENABLED, interrupt.get_bit() ) {
      self.interrupt_enabled = false;
      push!(self,PC);
      self.PC = interrupt as u16;
    }
  }
}

#[allow(non_camel_case_types)]
pub enum Interrupt{
    V_BLANK = 0x0040,
    LCDC = 0x0048,
    TIMER_OVERFLOW = 0x0050,
    SERIAL_TRANSFER = 0x0058,
    INPUT = 0x0060,
  }

impl Interrupt{
  fn get_bit(&self) -> Bit{
    match self {
      Interrupt::V_BLANK => Bit::One,
      Interrupt::LCDC => Bit::Two,
      Interrupt::TIMER_OVERFLOW => Bit::Three,
      Interrupt::SERIAL_TRANSFER => Bit::Four,
      Interrupt::INPUT => Bit::Five,
    }
  }
}