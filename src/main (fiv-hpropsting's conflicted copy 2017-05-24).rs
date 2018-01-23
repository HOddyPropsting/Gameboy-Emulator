// eg. apply8(cpu, Register:A,add8,cpu.load_register8(Register::D));
	macro_rules! apply8 {
		($self_:ident,$r:expr,$f:ident,$num:expr) => {
			{ let reg = $self_.load_register8(&$r);
				let num = $num;
				let tmp = $self_.$f(reg,num);
				$self_.save_register8(&$r,tmp);
			}
		};
	}

	macro_rules! ld_8 {
	    ($self_:ident,$r:ident,$l:ident) => {{
	    	let x = $self_.load_register8(&Register::$l);
	    	$self_.save_register8(&Register::$r,x);
	    }
	  };
	}

	macro_rules! apply16 {
		($self_:ident,$r:expr,$f:ident,$num:expr) => {
			{ let reg = $self_.load_register16(&$r);
				let num = $num;
				let tmp = $self_.$f(reg,num);
				$self_.save_register16(&$r,tmp);
			}
		};
	}	

#[derive(Debug)]
enum Operand {
	Register { r: Register},
	Data8 { data: u8},
	Data16 { data: u16},
	Address { address: u16},
	SignedData { data: i8},
	None,
}

const ZERO_FLAG:  u8 = 1 << 7;
const NEG_FLAG:	  u8 = 1 << 6;
const HALF_FLAG:  u8 = 1 << 5;
const CARRY_FLAG: u8 = 1 << 4;

#[derive(Debug)]
enum Register{
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
}

struct Cpu {
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
	memory : [u8; 65536],
}

fn program(mut cpu : Cpu) {
	let input /*[u8;10] =*/ =  vec![0x41];

	while cpu.PC < input.len() as u16 {

		match cpu.fetch_u8(&input) {
			0x00 => println!("NOP"), 
			0x01 => { let x = cpu.fetch_u16(&input);
								cpu.save_register16(&Register::BC, x);
								println!("LD BC d16")
							},
			0x02 => {	let y = cpu.load_register8(&Register::A);
								cpu.save_register_address(&Register::BC,y);
								println!("LD (BC) A")
							},
			0x03 => {	let tmp = cpu.load_register16(&Register::BC).wrapping_add(1);
								cpu.save_register16(&Register::BC, tmp);
								println!("INC BC")
							},
			0x04 => { apply8!(cpu ,Register::B , add8 , 1);
								println!("INC B")
							},
			0x05 => { apply8!(cpu , Register::B , sub8 , 1);
							  println!("DEC B")
							},
			0x06 => { let x = cpu.fetch_u8(&input);
								cpu.save_register8(&Register::B, x);
					 		  println!("LD B d8")
							},
			0x07 => { let x = cpu.A & 0b10000000;
							  cpu.A = cpu.A.rotate_left(1); 
							  cpu.zero_flags();
							  cpu.set_flag(CARRY_FLAG, x > 0); 
							  println!("RLCA")
							},
			0x08 => {	let x = cpu.load_register16(&Register::SP);
								let y = cpu.fetch_u16(&input);
								cpu.save_address(y, (x as u8));
								cpu.save_address(y+1, (x >> 8) as u8);
								println!("LD (a16) SP")
							},
			0x09 => { let x = cpu.load_register16(&Register::HL);
								let y = cpu.load_register16(&Register::BC);
								let z = cpu.add16(x,y);
								cpu.save_register16(&Register::HL,z);
							  println!("ADD HL BC")
							},
			0x0A => { let y = cpu.load_register_address(&Register::BC);
								cpu.save_register8(&Register::A, y);
							  println!("LD A (BC)") 
							},
			0x0B => { let tmp = cpu.load_register16(&Register::BC).wrapping_sub(1);
								cpu.save_register16(&Register::BC, tmp);
								println!("DEC BC")
							},
			0x0C => { apply8!(cpu ,Register::C , add8 , 1);
								println!("INC C")
							},
			0x0D => { apply8!(cpu ,Register::C , sub8 , 1);
								println!("DEC C")
							},
			0x0E => { let x = cpu.fetch_u8(&input);
								cpu.save_register8(&Register::C, x);
							  println!("LD C d8")
							 },
			0x0F => { let x = cpu.A & 0b1; 
							  cpu.A = cpu.A.rotate_right(1);  
							  cpu.zero_flags(); 
							  cpu.set_flag(CARRY_FLAG, x > 0); 
							  println!("RRCA")
							},
			0x10 => println!("unimplemented STOP"),
			0x11 => { let x = cpu.fetch_u16(&input);
								cpu.save_register16(&Register::DE, x);
								println!("LD DE d16")
							},
			0x12 => {	let y = cpu.load_register8(&Register::A);
								cpu.save_register_address(&Register::DE,y);
								println!("LD (DE) A")
							},
			0x13 => { let tmp = cpu.load_register16(&Register::DE).wrapping_add(1);
								cpu.save_register16(&Register::DE, tmp);
						    println!("INC DE")
						  },
			0x14 => { apply8!(cpu ,Register::D , add8 , 1);
								println!("INC D")
							},
			0x15 => { apply8!(cpu ,Register::D , sub8 , 1);
							  println!("DEC D")
							},
			0x16 => { let x = cpu.fetch_u8(&input); 
								cpu.save_register8(&Register::D, x);
								println!("LD D d8")
							},
			0x17 => { let x = cpu.A & 0b10000000; 
								cpu.A = cpu.A.rotate_left(1); 
								cpu.A = (cpu.A & 0b11111110) | (cpu.get_flag(CARRY_FLAG) as u8); 
								cpu.zero_flags(); 
								cpu.set_flag(CARRY_FLAG, x > 0); 
								println!("RLA")
							},
			0x18 => { let x = cpu.fetch_i8(&input) - 2; 
					  		cpu.PC = ((cpu.PC as i16).wrapping_add(x as i16)) as u16;
					 			println!("JR r8")
							},
			0x19 => { let x = cpu.load_register16(&Register::HL);
								let y = cpu.load_register16(&Register::DE);
								let z = cpu.add16(x,y);
								cpu.save_register16(&Register::HL,z);
								println!("ADD HL DE")
							},
			0x1A => { let y = cpu.load_register_address(&Register::BC);
								cpu.save_register8(&Register::A,y);
								println!("LD A (DE)")
							},
			0x1B => { let tmp = cpu.load_register16(&Register::DE).wrapping_sub(1);
								cpu.save_register16(&Register::DE, tmp);
							  println!("DEC DE")
							},
			0x1C => { apply8!(cpu ,Register::E , add8 , 1);
								println!("INC E")
							},
      0x1D => { apply8!(cpu ,Register::E , sub8 , 1);
      					println!("DEC E")
      				},
  		0x1E => { let x = cpu.fetch_u8(&input); 
  							cpu.save_register8(&Register::E, x);
      					println!("LD E d8")
      				},
   		0x1F => { let x = cpu.A & 0b1; 
      					cpu.A = cpu.A.rotate_right(1); 
      					cpu.A = (cpu.A & 0b01111111) | ((cpu.get_flag(CARRY_FLAG) as u8) << 7); 
      					cpu.zero_flags(); 
      					cpu.set_flag(CARRY_FLAG, x > 0); 
      					println!("RRA")
      				},
			0x20 => { let x = cpu.fetch_i8(&input) - 2; 
								if !(cpu.get_flag(ZERO_FLAG)) { 
									cpu.PC = ((cpu.PC as i16).wrapping_add(x as i16)) as u16;
								};
								println!("JR NZ r8")
							},
			0x21 => { let x = cpu.fetch_u16(&input);
								cpu.save_register16(&Register::HL, x);
								println!("LD HL d16") 
							},
			0x22 => { let x = cpu.load_register16(&Register::HL);
								let y = cpu.load_register8(&Register::A);
								cpu.save_address(x,y);
								cpu.save_register16(&Register::HL, x.wrapping_add(1));
								println!("LD (HL+) A")
							},
			0x23 => { let tmp = cpu.load_register16(&Register::HL).wrapping_add(1);
								cpu.save_register16(&Register::HL, tmp);
								println!("INC HL")
							},
			0x24 => { apply8!(cpu ,Register::H , add8 , 1);
								println!("INC H")
							},
			0x25 => { apply8!(cpu ,Register::H , sub8 , 1);
								println!("DEC H")
							},
			0x26 => { let x = cpu.fetch_u8(&input);
								cpu.save_register8(&Register::H, x);
								println!("LD H,d8")
							},
			0x27 => { println!("unimplemented DAA") },
			0x28 => { let x = cpu.fetch_i8(&input) - 2; 
								if (cpu.get_flag(ZERO_FLAG)) { 
									cpu.PC = ((cpu.PC as i16).wrapping_add(x as i16)) as u16;
								};
								println!("JR Z r8")
							},
			0x29 => { let x = cpu.load_register16(&Register::HL);
								let y = cpu.load_register16(&Register::HL);
								let z = cpu.add16(x,y);
								cpu.save_register16(&Register::HL,z);
								println!("ADD HL,HL")
							},
			0x2A => { let x = cpu.load_register16(&Register::HL);
								let y = cpu.load_address(x);
								cpu.save_register8(&Register::A,y);
								cpu.save_register16(&Register::HL, x.wrapping_add(1));
								println!("LD A,(HL+)")
							}, 
			0x2B => { let tmp = cpu.load_register16(&Register::HL).wrapping_sub(1);
								cpu.save_register16(&Register::BC, tmp);
								println!("DEC HL")
							},
			0x2C => { apply8!(cpu ,Register::L , add8 , 1);
								println!("INC L")
							},
			0x2D => { apply8!(cpu ,Register::L , sub8 , 1);
								println!("DEC L")
							},
			0x2E => { let x = cpu.fetch_u8(&input); 
								cpu.save_register8(&Register::L, x);
								println!("LD L d8")
							},
			0x2F => { cpu.A = !cpu.A;
								println!("CPL")
							},
			0x30 => { let x = cpu.fetch_i8(&input) - 2;
								if !(cpu.get_flag(CARRY_FLAG)) {
									cpu.PC = ((cpu.PC as i16).wrapping_add(x as i16)) as u16;
								};
								println!("JR NC r8") },
			0x31 => { let x = cpu.fetch_u16(&input);
								cpu.save_register16(&Register::SP, x);
								println!("LD SP d16") },
			0x32 => { let x = cpu.load_register16(&Register::HL);
								let y = cpu.load_register8(&Register::A);
								cpu.save_address(x,y);
								cpu.save_register16(&Register::HL, x.wrapping_sub(1));
								println!("LD (HL-) A") },
			0x33 => { let x = cpu.load_register16(&Register::SP).wrapping_add(1);
								cpu.save_register16(&Register::SP, x);
								println!("INC SP") },
			0x34 => { let x = cpu.load_register_address(&Register::HL);
								let y = cpu.add8(x,1);
								cpu.save_register_address(&Register::HL,y);
								println!("INC (HL)") },
			0x35 => { let x = cpu.load_register_address(&Register::HL);
								let y = cpu.sub8(x,1);
								cpu.save_register_address(&Register::HL,y);
								println!("DEC (HL)") },
			0x36 => { let x = cpu.fetch_u8(&input);
								cpu.save_register_address(&Register::HL, x);
								println!("LD (HL) d8") },
			0x37 => { cpu.set_flag(CARRY_FLAG, true);
								cpu.set_flag(NEG_FLAG, false);
								cpu.set_flag(HALF_FLAG, false);
								println!("SCF") 
							},
			0x38 => { let x = cpu.fetch_i8(&input) - 2; 
								if (cpu.get_flag(CARRY_FLAG)) { 
									cpu.PC = ((cpu.PC as i16).wrapping_add(x as i16)) as u16;
								};
								println!("JR C r8") 
							},
			0x39 => { let x = cpu.load_register16(&Register::HL);
								let y = cpu.load_register16(&Register::SP);
								let z = cpu.add16(x,y);
								cpu.save_register16(&Register::HL,z);
								println!("ADD HL SP") 
							},
			0x3A => { let x = cpu.load_register16(&Register::HL);
								let y = cpu.load_address(x);
								cpu.save_register8(&Register::A,y);
								cpu.save_register16(&Register::HL, x.wrapping_sub(1));
								println!("LD A (HL-)") 
							},
			0x3B => { let tmp = cpu.load_register16(&Register::SP).wrapping_sub(1);
								cpu.save_register16(&Register::BC, tmp);
								println!("DEC SP") 
							},
			0x3C => { apply8!(cpu, Register::A, add8, 1);
								println!("INC A") 
							},
			0x3D => { apply8!(cpu, Register::A, sub8, 1);
								println!("DEC A") },
			0x3E => { let x = cpu.fetch_u8(&input);
								cpu.save_register8(&Register::A,x);
								println!("LD A d8") 
							},
			0x3F => { let x = !cpu.get_flag(CARRY_FLAG);
								cpu.set_flag(CARRY_FLAG, x);
								cpu.set_flag(NEG_FLAG, false);
								cpu.set_flag(HALF_FLAG, false);
								println!("CCF") 
							},
			0x40 => { ld_8!(cpu,B,B);
								println!("LD B B") 
							},
			0x41 => { ld_8!(cpu,B,C);
								println!("LD B C") 
							},
			0x42 => { ld_8!(cpu,B,D);
								println!("LD B D") 
							},
			0x43 => { ld_8!(cpu,B,E);
								println!("LD B E") 
							},
			0x44 => { ld_8!(cpu,B,H);
								println!("LD B H") 
							},
			0x45 => { ld_8!(cpu,B,L);
								println!("LD B L") 
							},
			0x46 => { let x = cpu.load_register_address(&Register::HL);
								cpu.save_register8(&Register::B,x);
								println!("LD B (HL)") 
							},
			0x47 => { ld_8!(cpu,B,A);
								println!("LD B A") 
							},
			0x48 => { ld_8!(cpu,C,B);
								println!("LD C B") 
							},
			0x49 => { ld_8!(cpu,C,C);
								println!("LD C C") 
							},
			0x4A => { ld_8!(cpu,C,D);
								println!("LD C D") 
							},
			0x4B => { ld_8!(cpu,C,E);
								println!("LD C E") 
							},
			0x4C => { ld_8!(cpu,C,H);
								println!("LD C H") 
							},
			0x4D => { ld_8!(cpu,C,L);
								println!("LD C L") 
							},
			0x4E => { let x = cpu.load_register_address(&Register::HL);
								cpu.save_register8(&Register::C,x);
								println!("LD C (HL)") 
							},
			0x4F => { ld_8!(cpu,C,A);
								println!("LD C A") 
							},
			0x50 => { ld_8!(cpu,D,C);
								println!("LD D B") 
							},
			0x51 => { ld_8!(cpu,D,C);
								println!("LD D C") 
							},
			0x52 => { ld_8!(cpu,D,D);
								println!("LD D D") 
							},
			0x53 => { ld_8!(cpu,D,E);
								println!("LD D E") 
							},
			0x54 => { ld_8!(cpu,D,H);
								println!("LD D H") 
							},
			0x55 => { ld_8!(cpu,D,L);
								println!("LD D L") 
							},
			0x56 => { let x = cpu.load_register_address(&Register::HL);
								cpu.save_register8(&Register::D,x);
								println!("LD D (HL)") 
							},
			0x57 => { ld_8!(cpu,D,A);
								println!("LD D A") 
							},
			0x58 => { ld_8!(cpu,E,B);
								println!("LD E B") 
							},
			0x59 => { ld_8!(cpu,E,C);
								println!("LD E C") 
							},
			0x5A => { ld_8!(cpu,E,D);
								println!("LD E D") 
							},
			0x5B => { ld_8!(cpu,E,E);
								println!("LD E E") 
							},
			0x5C => { ld_8!(cpu,E,H);
								println!("LD E H") 
							},
			0x5D => { ld_8!(cpu,E,L);
								println!("LD E L") 
							},
			0x5E => { let x = cpu.load_register_address(&Register::HL);
								cpu.save_register8(&Register::E,x);
								println!("LD E (HL)") 
							},
			0x5F => { ld_8!(cpu,E,A);
								println!("LD E A") 
							},
			0x60 => { ld_8!(cpu,H,C);
								println!("LD H B") 
							},
			0x61 => { ld_8!(cpu,H,C);
								println!("LD H C") 
							},
			0x62 => { ld_8!(cpu,H,D);
								println!("LD H D") 
							},
			0x63 => { ld_8!(cpu,H,E);
								println!("LD H E") 
							},
			0x64 => { ld_8!(cpu,H,H);
								println!("LD H H") 
							},
			0x65 => { ld_8!(cpu,H,L);
								println!("LD H L") 
							},
			0x66 => { let x = cpu.load_register_address(&Register::HL);
								cpu.save_register8(&Register::H,x);
								println!("LD H (HL)") 
							},
			0x67 => { ld_8!(cpu,H,A);
								println!("LD H A") 
							},
			0x68 => { ld_8!(cpu,L,B);
								println!("LD L B") 
							},
			0x69 => { ld_8!(cpu,L,C);
								println!("LD L C") 
							},
			0x6A => { ld_8!(cpu,L,D);
								println!("LD L D") 
							},
			0x6B => { ld_8!(cpu,L,E);
								println!("LD L E") 
							},
			0x6C => { ld_8!(cpu,L,H);
								println!("LD L H") 
							},
			0x6D => { ld_8!(cpu,L,L);
								println!("LD L L") 
							},
			0x6E => { let x = cpu.load_register_address(&Register::HL);
								cpu.save_register8(&Register::L,x);
								println!("LD L (HL)") 
							},
			0x6F => { ld_8!(cpu,L,A); 
								println!("LD L A") 
							},
			_ => println!("unimplemented"),
		};

		println!("{:8b}	{:8b}", cpu.B, cpu.C);
		//println!("1: {:8b}	0: {:8b}", cpu.memory[1], cpu.memory[0]);
		//println!("3: {:8b}	2: {:8b}", cpu.memory[3], cpu.memory[2]);
		//println!("5: {:8b}	4: {:8b}", cpu.memory[5], cpu.memory[4]);
	};
}

fn main() {
	let mut c = Cpu{
		A: 0x3B,//0b11100001,
		F: 0b00000,
		B: 0,
		C: 0b11010101,
		D: 0,
		E: 0,
		H: 0,
		L: 0,
		SP: 0,
		PC: 0,

		memory : [0; 65536],
	};

	println!("{:8b}	{:8b}", c.B, c.C);
	program(c);
}

impl Cpu {

	fn fetch_u8(&mut self, input : &[u8]) -> u8{
		let x = input[self.PC as usize];
		self.PC = self.PC + 1;
		return x;
	}

	fn fetch_i8(&mut self, input : &[u8]) -> i8{
		return self.fetch_u8(input) as i8;
	}

	fn fetch_u16(&mut self, input : &[u8]) -> u16{
		let x = self.fetch_u8(input);
		let y = self.fetch_u8(input);
		return ((x as u16) << 8) + (y as u16);
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
		return self.memory[loc as usize];
	}

	fn load_register_address(&self, reg: &Register) -> u8{
		return self.memory[self.load_register16(&reg) as usize];
	}

	fn save_address(&mut self, loc: u16, value :u8) {
		self.memory[loc as usize] = value;
	}

	fn save_register_address(&mut self, reg: &Register, value :u8){
		self.memory[self.load_register16(reg) as usize] = value;
	}

	fn load_register88(&self, r : &Register) -> (u8,u8){
		match *r {
			Register::A => (self.A,0),
			Register::B => (self.B,0),
			Register::C => (self.C,0),
			Register::D => (self.D,0),
			Register::E => (self.E,0),
			Register::F => (self.F,0),
			Register::H => (self.H,0),
			Register::L => (self.L,0),
			Register::AF => (self.A,self.F),
			Register::BC => (self.B,self.C),
			Register::DE => (self.D,self.E),
			Register::HL => (self.H,self.L),
			Register::SP => (self.SP as u8, (self.SP >> 8) as u8),
			Register::PC => (self.PC as u8, (self.PC >> 8) as u8),
		}
	}

	fn load_register8(&self, r: &Register) -> u8{
		match *r {
			Register::A => self.A,
			Register::B => self.B,
			Register::C => self.C,
			Register::D => self.D,
			Register::E => self.E,
			Register::F => self.F,
			Register::H => self.H,
			Register::L => self.L,
			_ => panic!("Attempted to load a 16 bit register as 8 bit"),
		}
	}

	fn load_register16(&self, r: &Register) -> u16{
		match *r {
			Register::AF => (((self.A as u16) << 8) + self.F as u16) as u16,
			Register::BC => (((self.B as u16) << 8) + self.C as u16) as u16,
			Register::DE => (((self.D as u16) << 8) + self.E as u16) as u16,
			Register::HL => (((self.H as u16) << 8) + self.L as u16) as u16,
			Register::SP => self.SP as u16,
			Register::PC => self.PC as u16,
			_ => panic!("Attempted to load an 8 bit register as 16 bit"),
		}
	}

	fn save_register88(&mut self, r : &Register, value1 : u8, value2 : u8){
		match *r {
			Register::A => self.A = value1,
			Register::B => self.B = value1,
			Register::C => self.C = value1,
			Register::D => self.D = value1,
			Register::E => self.E = value1,
			Register::F => self.F = value1,
			Register::H => self.H = value1,
			Register::L => self.L = value1,
			Register::AF => { self.A = value1; self.F = value2 },
			Register::BC => { self.B = value1; self.C = value2 },
			Register::DE => { self.D = value1; self.E = value2 },
			Register::HL => { self.H = value1; self.L = value2 },
			Register::SP => self.SP = (value1 << 8 + value2) as u16,
			Register::PC => self.PC = (value1 << 8 + value2) as u16,
		}
	}

	fn save_register8(&mut self, r : &Register, value : u8){
		match *r {
			Register::A => self.A = value,
			Register::B => self.B = value,
			Register::C => self.C = value,
			Register::D => self.D = value,
			Register::E => self.E = value,
			Register::F => self.F = value,
			Register::H => self.H = value,
			Register::L => self.L = value,
			_ => panic!("Attempted to save a 16 bit register as 8 bit"),
		}
	}

	fn save_register16(&mut self, r: &Register, value : u16){
		match *r {
			Register::AF => {self.A = (value >> 8) as u8; self.F = value as u8},
			Register::BC => {self.B = (value >> 8) as u8; self.C = value as u8},
			Register::DE => {self.D = (value >> 8) as u8; self.E = value as u8},
			Register::HL => {self.H = (value >> 8) as u8; self.L = value as u8},
			Register::SP => self.SP = value,
			Register::PC => self.PC = value,
			_ => panic!("Attempted to save an 8 bit register as 16 bit"),
		}
	}

	/*
	* Adds two u8s
	* sets the neg flag to false
	* zero flag if it overflows
	* half flag if a carry from 4 -> 5
	*/
	fn add8(&mut self, a: u8, b:u8) -> u8{
		let hf = (((a & 0xf) + (b & 0xf)) & 0x10) == 0x10;
		let (tmp,ov) = a.overflowing_add(b);
		self.set_flag(NEG_FLAG, false);
		self.set_flag(ZERO_FLAG, ov);
		self.set_flag(HALF_FLAG, hf);
		return tmp;
	}

	fn sub8(&mut self, a: u8, b:u8) -> u8{
		let hf = ((a & 0xf) < (b & 0xf));
		let (tmp,ov) = a.overflowing_sub(b);
		self.set_flag(NEG_FLAG, true);
		self.set_flag(ZERO_FLAG, ov);
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
}
