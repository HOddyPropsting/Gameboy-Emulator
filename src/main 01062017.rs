// eg. apply8(cpu, Register:A,add8,cpu.load_register8(Register::D));
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
			let h = $cpu.load_address(sp+1);
			let hl = ((h as u16) << 8) + (l as u16);
			$cpu.save_register16(Register::$r, hl);
			$cpu.save_register16(Register::SP, sp+2);
	    }};
	}

	macro_rules! push {
	    ($cpu:ident,$r:ident) => {{
	    	let sp = $cpu.load_register16(Register::SP);
				let pc = $cpu.load_register16(Register::$r);
				$cpu.save_address(sp-1, pc as u8);
				$cpu.save_address(sp-2, (pc >> 8) as u8);
				$cpu.save_register16(Register::SP, sp-2);
	    }};
	}

const ZERO_FLAG:  u8 = 1 << 7;
const NEG_FLAG:	  u8 = 1 << 6;
const HALF_FLAG:  u8 = 1 << 5;
const CARRY_FLAG: u8 = 1 << 4;

#[derive(Clone,Copy)]
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
	HL_address,
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
	interrupt_enabled: bool,

	mmu : Mmu,
}

fn program(mut cpu : Cpu) {

	while true {

		match cpu.fetch_u8() {
			0x00 => println!("NOP"), 
			0x01 => { let x = cpu.fetch_u16();
								cpu.save_register16(Register::BC, x);
								println!("LD BC d16")
							},
			0x02 => {	let y = cpu.load_register8(Register::A);
								cpu.save_register_address(Register::BC,y);
								println!("LD (BC) A")
							},
			0x03 => {	let tmp = cpu.load_register16(Register::BC).wrapping_add(1);
								cpu.save_register16(Register::BC, tmp);
								println!("INC BC")
							},
			0x04 => { apply8!(cpu ,B , add8 , 1);
								println!("INC B")
							},
			0x05 => { apply8!(cpu ,B , sub8 , 1);
							  println!("DEC B")
							},
			0x06 => { let x = cpu.fetch_u8();
								cpu.save_register8(Register::B, x);
					 		  println!("LD B d8")
							},
			0x07 => { let x = cpu.A & 0b10000000;
							  cpu.A = cpu.A.rotate_left(1); 
							  cpu.zero_flags();
							  cpu.set_flag(CARRY_FLAG, x > 0); 
							  println!("RLCA")
							},
			0x08 => {	let x = cpu.load_register16(Register::SP);
								let y = cpu.fetch_u16();
								cpu.save_address(y, (x as u8));
								cpu.save_address(y+1, (x >> 8) as u8);
								println!("LD (a16) SP")
							},
			0x09 => { let x = cpu.load_register16(Register::HL);
								let y = cpu.load_register16(Register::BC);
								let z = cpu.add16(x,y);
								cpu.save_register16(Register::HL,z);
							  println!("ADD HL BC")
							},
			0x0A => { let y = cpu.load_register_address(Register::BC);
								cpu.save_register8(Register::A, y);
							  println!("LD A (BC)") 
							},
			0x0B => { let tmp = cpu.load_register16(Register::BC).wrapping_sub(1);
								cpu.save_register16(Register::BC, tmp);
								println!("DEC BC")
							},
			0x0C => { apply8!(cpu ,C , add8 , 1);
								println!("INC C")
							},
			0x0D => { apply8!(cpu,C , sub8 , 1);
								println!("DEC C")
							}, 
			0x0E => { let x = cpu.fetch_u8();
								cpu.save_register8(Register::C, x);
							  println!("LD C d8")
							 },
			0x0F => { let x = cpu.A & 0b1; 
							  cpu.A = cpu.A.rotate_right(1);  
							  cpu.zero_flags(); 
							  cpu.set_flag(CARRY_FLAG, x > 0); 
							  println!("RRCA")
							},
			0x10 => println!("unimplemented STOP"),
			0x11 => { let x = cpu.fetch_u16();
								cpu.save_register16(Register::DE, x);
								println!("LD DE d16")
							},
			0x12 => {	let y = cpu.load_register8(Register::A);
								cpu.save_register_address(Register::DE,y);
								println!("LD (DE) A")
							},
			0x13 => { let tmp = cpu.load_register16(Register::DE).wrapping_add(1);
								cpu.save_register16(Register::DE, tmp);
						    println!("INC DE")
						  },
			0x14 => { apply8!(cpu,D , add8 , 1);
								println!("INC D")
							},
			0x15 => { apply8!(cpu,D , sub8 , 1);
							  println!("DEC D")
							},
			0x16 => { let x = cpu.fetch_u8(); 
								cpu.save_register8(Register::D, x);
								println!("LD D d8")
							},
			0x17 => { let x = cpu.A & 0b10000000; 
								cpu.A = cpu.A.rotate_left(1); 
								cpu.A = (cpu.A & 0b11111110) | (cpu.get_flag(CARRY_FLAG) as u8); 
								cpu.zero_flags(); 
								cpu.set_flag(CARRY_FLAG, x > 0); 
								println!("RLA")
							},
			0x18 => { let x = cpu.fetch_i8() - 2; 
					  		cpu.PC = ((cpu.PC as i16).wrapping_add(x as i16)) as u16;
					 			println!("JR r8")
							},
			0x19 => { let x = cpu.load_register16(Register::HL);
								let y = cpu.load_register16(Register::DE);
								let z = cpu.add16(x,y);
								cpu.save_register16(Register::HL,z);
								println!("ADD HL DE")
							},
			0x1A => { let y = cpu.load_register_address(Register::BC);
								cpu.save_register8(Register::A,y);
								println!("LD A (DE)")
							},
			0x1B => { let tmp = cpu.load_register16(Register::DE).wrapping_sub(1);
								cpu.save_register16(Register::DE, tmp);
							  println!("DEC DE")
							},
			0x1C => { apply8!(cpu,E , add8 , 1);
								println!("INC E")
							},
      0x1D => { apply8!(cpu,E , sub8 , 1);
      					println!("DEC E")
      				},
  		0x1E => { let x = cpu.fetch_u8(); 
  							cpu.save_register8(Register::E, x);
      					println!("LD E d8")
      				},
   		0x1F => { let x = cpu.A & 0b1; 
      					cpu.A = cpu.A.rotate_right(1); 
      					cpu.A = (cpu.A & 0b01111111) | ((cpu.get_flag(CARRY_FLAG) as u8) << 7); 
      					cpu.zero_flags(); 
      					cpu.set_flag(CARRY_FLAG, x > 0); 
      					println!("RRA")
      				},
			0x20 => { let x = cpu.fetch_i8() - 2; 
								if !(cpu.get_flag(ZERO_FLAG)) { 
									cpu.PC = ((cpu.PC as i16).wrapping_add(x as i16)) as u16;
								};
								println!("JR NZ r8")
							},
			0x21 => { let x = cpu.fetch_u16();
								cpu.save_register16(Register::HL, x);
								println!("LD HL d16") 
							},
			0x22 => { let x = cpu.load_register16(Register::HL);
								let y = cpu.load_register8(Register::A);
								cpu.save_address(x,y);
								cpu.save_register16(Register::HL, x.wrapping_add(1));
								println!("LD (HL+) A")
							},
			0x23 => { let tmp = cpu.load_register16(Register::HL).wrapping_add(1);
								cpu.save_register16(Register::HL, tmp);
								println!("INC HL")
							},
			0x24 => { apply8!(cpu,H , add8 , 1);
								println!("INC H")
							},
			0x25 => { apply8!(cpu,H , sub8 , 1);
								println!("DEC H")
							},
			0x26 => { let x = cpu.fetch_u8();
								cpu.save_register8(Register::H, x);
								println!("LD H,d8")
							},
			0x27 => { println!("unimplemented DAA") },
			0x28 => { let x = cpu.fetch_i8() - 2; 
								if cpu.get_flag(ZERO_FLAG) { 
									cpu.PC = ((cpu.PC as i16).wrapping_add(x as i16)) as u16;
								};
								println!("JR Z r8")
							},
			0x29 => { let x = cpu.load_register16(Register::HL);
								let y = cpu.load_register16(Register::HL);
								let z = cpu.add16(x,y);
								cpu.save_register16(Register::HL,z);
								println!("ADD HL,HL")
							},
			0x2A => { let x = cpu.load_register16(Register::HL);
								let y = cpu.load_address(x);
								cpu.save_register8(Register::A,y);
								cpu.save_register16(Register::HL, x.wrapping_add(1));
								println!("LD A,(HL+)")
							}, 
			0x2B => { let tmp = cpu.load_register16(Register::HL).wrapping_sub(1);
								cpu.save_register16(Register::BC, tmp);
								println!("DEC HL")
							},
			0x2C => { apply8!(cpu,L , add8 , 1);
								println!("INC L")
							},
			0x2D => { apply8!(cpu,L , sub8 , 1);
								println!("DEC L")
							},
			0x2E => { let x = cpu.fetch_u8(); 
								cpu.save_register8(Register::L, x);
								println!("LD L d8")
							},
			0x2F => { cpu.A = !cpu.A;
								println!("CPL")
							},
			0x30 => { let x = cpu.fetch_i8() - 2;
								if !cpu.get_flag(CARRY_FLAG) {
									cpu.PC = ((cpu.PC as i16).wrapping_add(x as i16)) as u16;
								};
								println!("JR NC r8") },
			0x31 => { let x = cpu.fetch_u16();
								cpu.save_register16(Register::SP, x);
								println!("LD SP d16") },
			0x32 => { let x = cpu.load_register16(Register::HL);
								let y = cpu.load_register8(Register::A);
								cpu.save_address(x,y);
								cpu.save_register16(Register::HL, x.wrapping_sub(1));
								println!("LD (HL-) A") },
			0x33 => { let x = cpu.load_register16(Register::SP).wrapping_add(1);
								cpu.save_register16(Register::SP, x);
								println!("INC SP") },
			0x34 => { let x = cpu.load_register_address(Register::HL);
								let y = cpu.add8(x,1);
								cpu.save_register_address(Register::HL,y);
								println!("INC (HL)") },
			0x35 => { let x = cpu.load_register_address(Register::HL);
								let y = cpu.sub8(x,1);
								cpu.save_register_address(Register::HL,y);
								println!("DEC (HL)") },
			0x36 => { let x = cpu.fetch_u8();
								cpu.save_register_address(Register::HL, x);
								println!("LD (HL) d8") },
			0x37 => { cpu.set_flag(CARRY_FLAG, true);
								cpu.set_flag(NEG_FLAG, false);
								cpu.set_flag(HALF_FLAG, false);
								println!("SCF") 
							},
			0x38 => { let x = cpu.fetch_i8() - 2; 
								if cpu.get_flag(CARRY_FLAG) { 
									cpu.PC = ((cpu.PC as i16).wrapping_add(x as i16)) as u16;
								};
								println!("JR C r8") 
							},
			0x39 => { let x = cpu.load_register16(Register::HL);
								let y = cpu.load_register16(Register::SP);
								let z = cpu.add16(x,y);
								cpu.save_register16(Register::HL,z);
								println!("ADD HL SP") 
							},
			0x3A => { let x = cpu.load_register16(Register::HL);
								let y = cpu.load_address(x);
								cpu.save_register8(Register::A,y);
								cpu.save_register16(Register::HL, x.wrapping_sub(1));
								println!("LD A (HL-)") 
							},
			0x3B => { let tmp = cpu.load_register16(Register::SP).wrapping_sub(1);
								cpu.save_register16(Register::BC, tmp);
								println!("DEC SP") 
							},
			0x3C => { apply8!(cpu,A, add8, 1);
								println!("INC A") 
							},
			0x3D => { apply8!(cpu,A, sub8, 1);
								println!("DEC A") },
			0x3E => { let x = cpu.fetch_u8();
								cpu.save_register8(Register::A,x);
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
			0x46 => { ld_8!(cpu,B,HL_address);
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
			0x4E => { ld_8!(cpu,C,HL_address);
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
			0x56 => { ld_8!(cpu,D,HL_address);;
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
			0x5E => { ld_8!(cpu,E,HL_address);
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
			0x66 => { ld_8!(cpu,H,HL_address);
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
			0x6E => { ld_8!(cpu,L,HL_address);
								println!("LD L (HL)") 
							},
			0x6F => { ld_8!(cpu,L,A); 
								println!("LD L A") 
							},
			0x70 => { ld_8!(cpu,HL_address,B);
								println!("LD (HL) B") 
							},
			0x71 => { ld_8!(cpu,HL_address,C);
								println!("LD (HL) C") 
							},
			0x72 => { ld_8!(cpu,HL_address,D);
								println!("LD (HL) D") 
							},
			0x73 => { ld_8!(cpu,HL_address,E);
								println!("LD (HL) E") 
							},
			0x74 => { ld_8!(cpu,HL_address,H);
								println!("LD (HL) H") 
							},
			0x75 => { ld_8!(cpu,HL_address,L);
								println!("LD (HL) L") 
							},
			0x76 => { println!("unimplemented HALT") },
			0x77 => { ld_8!(cpu,HL_address,A);
								println!("LD (HL) A")
						  },
			0x78 => { ld_8!(cpu,A,B);
								println!("LD A B") 
							},
			0x79 => { ld_8!(cpu,A,C);
								println!("LD A C") 
							},
			0x7A => { ld_8!(cpu,A,D);
								println!("LD A D") 
							},
			0x7B => { ld_8!(cpu,A,E);
								println!("LD A E") 
							},
			0x7C => { ld_8!(cpu,A,H);
								println!("LD A H") 
							},
			0x7D => { ld_8!(cpu,A,L);
								println!("LD A L") 
							},
			0x7E => { let x = cpu.load_register_address(Register::HL);
								cpu.save_register8(Register::A,x);
								println!("LD A (HL)") 
							},
			0x7F => { ld_8!(cpu,A,A);
								println!("LD A A") },
			0x80 => { apply8!(cpu,A,add8,cpu.load_register8(Register::B));
								println!("ADD A B") 
							},
			0x81 => { apply8!(cpu,A,add8,cpu.load_register8(Register::C));
								println!("ADD A C") 
							},
			0x82 => { apply8!(cpu,A,add8,cpu.load_register8(Register::D));
								println!("ADD A D") 
							},
			0x83 => { apply8!(cpu,A,add8,cpu.load_register8(Register::E));
								println!("ADD A E") 
							},
			0x84 => { apply8!(cpu,A,add8,cpu.load_register8(Register::H));
								println!("ADD A H") 
							},
			0x85 => { apply8!(cpu,A,add8,cpu.load_register8(Register::L));
								println!("ADD A L") 
							},
			0x86 => { apply8!(cpu,A,add8,cpu.load_register_address(Register::HL));
								println!("ADD A (HL)") 
							},
			0x87 => { apply8!(cpu,A,add8,cpu.load_register8(Register::A));
								println!("ADD A A") 
							},
			0x88 => { apply8!(cpu,A,add8,cpu.load_register8(Register::B).wrapping_add(cpu.get_flag(CARRY_FLAG) as u8));
								println!("ADC A B") 
							},
			0x89 => { apply8!(cpu,A,add8,cpu.load_register8(Register::C).wrapping_add(cpu.get_flag(CARRY_FLAG) as u8));
								println!("ADC A C") 
							},
			0x8A => { apply8!(cpu,A,add8,cpu.load_register8(Register::D).wrapping_add(cpu.get_flag(CARRY_FLAG) as u8));
								println!("ADC A D") 
							},
			0x8B => { apply8!(cpu,A,add8,cpu.load_register8(Register::E).wrapping_add(cpu.get_flag(CARRY_FLAG) as u8));
								println!("ADC A E") 
							},
			0x8C => { apply8!(cpu,A,add8,cpu.load_register8(Register::H).wrapping_add(cpu.get_flag(CARRY_FLAG) as u8));
								println!("ADC A H") 
							},
			0x8D => { apply8!(cpu,A,add8,cpu.load_register8(Register::L).wrapping_add(cpu.get_flag(CARRY_FLAG) as u8));
								println!("ADC A L") 
							},
			0x8E => { apply8!(cpu,A,add8,cpu.load_register_address(Register::HL).wrapping_add(cpu.get_flag(CARRY_FLAG) as u8));
								println!("ADC A (HL)") 
							},
			0x8F => { apply8!(cpu,A,add8,cpu.load_register8(Register::A).wrapping_add(cpu.get_flag(CARRY_FLAG) as u8));
								println!("ADC A A") 
							},
			0x90 => { apply8!(cpu,A,sub8,cpu.load_register8(Register::B));
								println!("ADD A B") 
							},
			0x91 => { apply8!(cpu,A,sub8,cpu.load_register8(Register::C));
								println!("ADD A C") 
							},
			0x92 => { apply8!(cpu,A,sub8,cpu.load_register8(Register::D));
								println!("ADD A D") 
							},
			0x93 => { apply8!(cpu,A,sub8,cpu.load_register8(Register::E));
								println!("ADD A E") 
							},
			0x94 => { apply8!(cpu,A,sub8,cpu.load_register8(Register::H));
								println!("ADD A H") 
							},
			0x95 => { apply8!(cpu,A,sub8,cpu.load_register8(Register::L));
								println!("ADD A L") 
							},
			0x96 => { apply8!(cpu,A,sub8,cpu.load_register_address(Register::HL));
								println!("ADD A (HL)") 
							},
			0x97 => { apply8!(cpu,A,sub8,cpu.load_register8(Register::A));
								println!("ADD A A") 
							},
			0x98 => { apply8!(cpu,A,sub8,cpu.load_register8(Register::B).wrapping_add(cpu.get_flag(CARRY_FLAG) as u8));
								println!("ADC A B") 
							},
			0x99 => { apply8!(cpu,A,sub8,cpu.load_register8(Register::C).wrapping_add(cpu.get_flag(CARRY_FLAG) as u8));
								println!("ADC A C") 
							},
			0x9A => { apply8!(cpu,A,sub8,cpu.load_register8(Register::D).wrapping_add(cpu.get_flag(CARRY_FLAG) as u8));
								println!("ADC A D") 
							},
			0x9B => { apply8!(cpu,A,sub8,cpu.load_register8(Register::E).wrapping_add(cpu.get_flag(CARRY_FLAG) as u8));
								println!("ADC A E") 
							},
			0x9C => { apply8!(cpu,A,sub8,cpu.load_register8(Register::H).wrapping_add(cpu.get_flag(CARRY_FLAG) as u8));
								println!("ADC A H") 
							},
			0x9D => { apply8!(cpu,A,sub8,cpu.load_register8(Register::L).wrapping_add(cpu.get_flag(CARRY_FLAG) as u8));
								println!("ADC A L") 
							},
			0x9E => { apply8!(cpu,A,sub8,cpu.load_register_address(Register::HL).wrapping_add(cpu.get_flag(CARRY_FLAG) as u8));
								println!("ADC A (HL)") 
							},
			0x9F => { apply8!(cpu,A,sub8,cpu.load_register8(Register::A).wrapping_add(cpu.get_flag(CARRY_FLAG) as u8));
								println!("ADC A A") 
							},
			0xA0 => { apply8!(cpu,A,and8,cpu.load_register8(Register::B));
								println!("AND B") 
							},
			0xA1 => { apply8!(cpu,A,and8,cpu.load_register8(Register::C));
								println!("AND C") 
							},
			0xA2 => { apply8!(cpu,A,and8,cpu.load_register8(Register::D));
								println!("AND D") 
							},
			0xA3 => { apply8!(cpu,A,and8,cpu.load_register8(Register::E));
								println!("AND E") 
							},
			0xA4 => { apply8!(cpu,A,and8,cpu.load_register8(Register::H));
								println!("AND H") 
							},
			0xA5 => { apply8!(cpu,A,and8,cpu.load_register8(Register::L));
								println!("AND L") 
							},
			0xA6 => { apply8!(cpu,A,and8,cpu.load_register_address(Register::HL));
								println!("AND (HL)") 
							},
			0xA7 => { apply8!(cpu,A,and8,cpu.load_register8(Register::A));
								println!("AND A") 
							},
			0xA8 => { apply8!(cpu,A,xor8,cpu.load_register8(Register::B));
								println!("XOR B") 
							},
			0xA9 => { apply8!(cpu,A,xor8,cpu.load_register8(Register::C));
								println!("XOR C") 
							},
			0xAA => { apply8!(cpu,A,xor8,cpu.load_register8(Register::D));
								println!("XOR D") 
							},
			0xAB => { apply8!(cpu,A,xor8,cpu.load_register8(Register::E));
								println!("XOR E") 
							},
			0xAC => { apply8!(cpu,A,xor8,cpu.load_register8(Register::H));
								println!("XOR H") 
							},
			0xAD => { apply8!(cpu,A,xor8,cpu.load_register8(Register::L));
								println!("XOR L") 
							},
			0xAE => { apply8!(cpu,A,xor8,cpu.load_register_address(Register::HL));
								println!("XOR (HL)") 
							},
			0xAF => { apply8!(cpu,A,xor8,cpu.load_register8(Register::A));
								println!("XOR A") 
							},
			0xB0 => { apply8!(cpu,A,or8,cpu.load_register8(Register::B));
								println!("OR B") 
							},
			0xB1 => { apply8!(cpu,A,or8,cpu.load_register8(Register::C));
								println!("OR C") 
							},
			0xB2 => { apply8!(cpu,A,or8,cpu.load_register8(Register::D));
								println!("OR D") 
							},
			0xB3 => { apply8!(cpu,A,or8,cpu.load_register8(Register::E));
								println!("OR E") 
							},
			0xB4 => { apply8!(cpu,A,or8,cpu.load_register8(Register::H));
								println!("OR H") 
							},
			0xB5 => { apply8!(cpu,A,or8,cpu.load_register8(Register::L));
								println!("OR L") 
							},
			0xB6 => { apply8!(cpu,A,or8,cpu.load_register_address(Register::HL));
								println!("OR (HL)") 
							},
			0xB7 => { apply8!(cpu,A,or8,cpu.load_register8(Register::A));
								println!("OR A") 
							},
			0xB8 => { apply8!(cpu,A,cp8,cpu.load_register8(Register::B));
								println!("CP B") 
							},
			0xB9 => { apply8!(cpu,A,cp8,cpu.load_register8(Register::C));
								println!("CP C") 
							},
			0xBA => { apply8!(cpu,A,cp8,cpu.load_register8(Register::D));
								println!("CP D") 
							},
			0xBB => { apply8!(cpu,A,cp8,cpu.load_register8(Register::E));
								println!("CP E") 
							},
			0xBC => { apply8!(cpu,A,cp8,cpu.load_register8(Register::H));
								println!("CP H") 
							},
			0xBD => { apply8!(cpu,A,cp8,cpu.load_register8(Register::L));
								println!("CP L") 
							},
			0xBE => { apply8!(cpu,A,cp8,cpu.load_register_address(Register::HL));
								println!("CP (HL)") 
							},
			0xBF => { apply8!(cpu,A,cp8,cpu.load_register8(Register::A));
								println!("CP A") 
							},
			0xC0 => { if !cpu.get_flag(ZERO_FLAG) {
								pop!(cpu,PC)
								};
								println!("RET NZ") 
							},
			0xC1 => { pop!(cpu,BC);
								println!("POP BC")
							},
			0xC2 => { let x = cpu.fetch_u16();
								if !cpu.get_flag(ZERO_FLAG) {
									cpu.save_register16(Register::PC, x);
								};
								println!("JP NZ a16") 
							},
			0xC3 => { let x = cpu.fetch_u16();
								cpu.save_register16(Register::PC, x);
								println!("JP a16") 
							}, 
			0xC4 => { let a16 = cpu.fetch_u16();
								if !cpu.get_flag(ZERO_FLAG) {
									push!(cpu,PC);
									cpu.save_register16(Register::PC, a16);
								};
								println!("CALL NZ a16") 
							},
			0xC5 => { push!(cpu,BC);
								println!("PUSH BC") 
							},
			0xC6 => { apply8!(cpu,A,add8,cpu.fetch_u8());
								println!("ADD A d8") 
							},
			0xC7 => { push!(cpu,PC);
								cpu.save_register16(Register::PC, 0x00);
								println!("RST 0x00") 
							},
			0xC8 => { if cpu.get_flag(ZERO_FLAG) {
									pop!(cpu,PC);
								};
								println!("RET Z") 
							},
			0xC9 => { pop!(cpu,PC);
								println!("RET") 
							},
			0xCA => { let x = cpu.fetch_u16();
								if cpu.get_flag(ZERO_FLAG) {
									cpu.save_register16(Register::PC, x);
								};
								println!("JP Z a16") 
							},
			0xCB => { cpu = prefix_cb(cpu);
								println!("PREFIX CB") },
			0xCC => { let a16 = cpu.fetch_u16();
								if cpu.get_flag(ZERO_FLAG) {
									push!(cpu,PC);
									cpu.save_register16(Register::PC, a16);
								};
								println!("CALL Z a16") 
							},
			0xCD => { push!(cpu,PC);
								let a16 = cpu.fetch_u16();
								cpu.save_register16(Register::PC, a16);
								println!("CALL a16") 
							},
			0xCE => { apply8!(cpu,A,add8,cpu.fetch_u8().wrapping_add(cpu.get_flag(CARRY_FLAG) as u8));
								println!("ADC d8") 
							},
			0xCF => { push!(cpu,PC);
								cpu.save_register16(Register::PC, 0x08);
								println!("RST 0x08") 
							},
			0xD0 => { if !cpu.get_flag(CARRY_FLAG) {
									pop!(cpu,PC)
								};
								println!("RET NC") 
							},
			0xD1 => { pop!(cpu,DE);
								println!("POP DE")
							},
			0xD2 => { let x = cpu.fetch_u16();
								if !cpu.get_flag(CARRY_FLAG) {
									cpu.save_register16(Register::PC, x);
								};
								println!("JP NC a16")
							},
			0xD3 => { println!("0xD3 N/A") },
			0xD4 => { let a16 = cpu.fetch_u16();
								if !cpu.get_flag(CARRY_FLAG) {
									push!(cpu,PC);
									cpu.save_register16(Register::PC, a16);
								};
								println!("CALL NC a16") 
							},
			0xD5 => { push!(cpu,DE);
								println!("PUSH DE") 
							},
			0xD6 => { apply8!(cpu,A,sub8,cpu.fetch_u8());
								println!("SUB d8") 
							},
			0xD7 => { push!(cpu,PC);
								cpu.save_register16(Register::PC, 0x10);
								println!("RST 0x10") 
							},
			0xD8 => { if cpu.get_flag(CARRY_FLAG) {
									pop!(cpu,PC);
								}
								println!("RET C") 
							},
			0xD9 => { pop!(cpu,PC);
								cpu.interrupt_enabled = true;
								println!("RETI") 
							},
			0xDA => { let x = cpu.fetch_u16();
								if cpu.get_flag(CARRY_FLAG) {
									cpu.save_register16(Register::PC, x);
								};
								println!("JP C a16") 
							},
			0xDB => { println!("0xDB N/A") },
			0xDC => { let a16 = cpu.fetch_u16();
								if cpu.get_flag(CARRY_FLAG) {
									push!(cpu,PC);							
									cpu.save_register16(Register::PC, a16);
								};
								println!("CALL C a16") 
							},
			0xDD => { println!("0xDD N/A") },
			0xDE => { apply8!(cpu,A,sub8,cpu.fetch_u8().wrapping_add(cpu.get_flag(CARRY_FLAG) as u8));
								println!("SBC A d8") 
							},
			0xDF => { push!(cpu,PC);
								cpu.save_register16(Register::PC, 0x18);
								println!("RST 0x18") 
							},
			0xE0 => { let x = cpu.fetch_u8() as u16 + 0xFF00;
								let y = cpu.load_address(x);
								cpu.save_register8(Register::A,y);
								println!("LD (0xFF00 + a8) A") 
							},
			0xE1 => { pop!(cpu,HL);
								println!("POP HL") 
							},
			0xE2 => { let x = cpu.load_register8(Register::C) as u16 + 0xFF00;
								let y = cpu.load_address(x);
								cpu.save_register8(Register::A,y);
								println!("LD (0xFF00 + C) A") 
							},
			0xE3 => { println!("0xE3 N/A") },
			0xE4 => { println!("0xE4 N/A") },
			0xE5 => { push!(cpu,HL);
								println!("PUSH HL") 
							},
			0xE6 => { apply8!(cpu,A,and8,cpu.fetch_u8());
								println!("AND d8") 
							},
			0xE7 => { push!(cpu,PC);
								cpu.save_register16(Register::PC, 0x20);
								println!("RST 0x20") 
							},
			0xE8 => { let x = cpu.fetch_i8(); 
					  		cpu.SP = ((cpu.SP as i16).wrapping_add(x as i16)) as u16;
								println!("ADD SP r8") 
							},
			0xE9 => { let x = cpu.load_register16(Register::HL);
								cpu.save_register16(Register::PC, x);
								println!("JP (HL)") 
							},
			0xEA => { let x = cpu.fetch_u16();
								let y = cpu.load_address(x);
								cpu.save_register8(Register::A, y);
								println!("LD (a16) A") },
			0xEB => { println!("0xEB N/A") },
			0xEC => { println!("0xEC N/A") },
			0xED => { println!("0xED N/A") },
			0xEE => { apply8!(cpu,A,xor8,cpu.fetch_u8());
								println!("XOR d8")
						  },
			0xEF => { push!(cpu,PC);
								cpu.save_register16(Register::PC, 0x28);
								println!("RST 0x28") 
							},
			0xF0 => { let x = cpu.fetch_u8() as u16 + 0xFF00;
								let y = cpu.load_address(x);
								cpu.save_register8(Register::A,y);
								println!("LD A, (0xFF00 + a8)") 
							},
			0xF1 => { pop!(cpu,AF);
								println!("POP AF") },
			0xF2 => { let x = cpu.load_register8(Register::C) as u16 + 0xFF00;
								let y = cpu.load_address(x);
								cpu.save_register8(Register::A,y);
								println!("LD A (0xFF00 + C") 
							},
			0xF3 => { cpu.interrupt_enabled = false;
								println!("DI") 
							},
			0xF4 => { println!("0xF4 N/A") },
			0xF5 => { push!(cpu,AF);
								println!("PUSH AF") },
			0xF6 => { apply8!(cpu,A,or8,cpu.fetch_u8());
								println!("OR d8") },
			0xF7 => { push!(cpu,PC);
								cpu.save_register16(Register::PC, 0x30);
								println!("RST 0x30") 
							},
			0xF8 => { let x = cpu.load_register16(Register::SP) + cpu.fetch_u8() as u16;
								cpu.save_register16(Register::HL,x);
								println!("LD HL SP + r8") 
							},
			0xF9 => { let x = cpu.load_register16(Register::HL);
								cpu.save_register16(Register::SP, x);
								println!("LD SP HL") },
			0xFA => { println!("LD A (a16)") },
			0xFB => { cpu.interrupt_enabled = true;
								println!("EI") 
							},
			0xFC => { println!("0xFC N/A") },
			0xFD => { println!("0xFD N/A") },
			0xFE => { apply8!(cpu,A,cp8,cpu.fetch_u8());
								println!("CP d8") 
							},
			0xFF => { push!(cpu,PC);
								cpu.save_register16(Register::PC, 0x38);
								println!("RST 0x38") 
							},
			_ => println!("unimplemented"),
		};


	};

	println!("{:8b}	{:8b}", cpu.B, cpu.F);

}

fn main() {

	let mut m = Mmu{
		gb_internal_rom : [0; 0x100],

		gb_cartridge : [0; 0x10000],
	};

	m.load_cartridge();

	let mut c = Cpu{
		A: 0b0,
		F: 0b0,
		B: 0b0,
		C: 0x00,
		D: 0,
		E: 0,
		H: 0,
		L: 0,
		SP: 0x00,
		PC: 0,

		interrupt_enabled : false,

		mmu : m,
	};
	println!("{:8b}	{:8b}", c.B, c.F);
	program(c);
}

fn prefix_cb(mut cpu : Cpu) -> Cpu{

	let reg_vec = vec![Register::B,Register::C,Register::D,Register::E,Register::H,Register::L,Register::HL_address,Register::A];

	let instruction = cpu.fetch_u8();

	match instruction{
		0o000...0o007 => cpu.rlc(reg_vec[(instruction % 8) as usize]),
		0o010...0o017 => cpu.rrc(reg_vec[(instruction % 8) as usize]),
		0o020...0o027 => cpu.rl(reg_vec[(instruction % 8) as usize]),
		0o030...0o037 => cpu.rr(reg_vec[(instruction % 8) as usize]),
		0o040...0o047 => cpu.sla(reg_vec[(instruction % 8) as usize]),
		0o050...0o057 => cpu.sra(reg_vec[(instruction % 8) as usize]),
		0o060...0o067 => cpu.swap(reg_vec[(instruction % 8) as usize]),
		0o070...0o077 => cpu.srl(reg_vec[(instruction % 8) as usize]),
		0o100...0o107 => cpu.bit(reg_vec[(instruction % 8) as usize], 0),
		0o110...0o117 => cpu.bit(reg_vec[(instruction % 8) as usize], 1),
		0o120...0o127 => cpu.bit(reg_vec[(instruction % 8) as usize], 2),
		0o130...0o137 => cpu.bit(reg_vec[(instruction % 8) as usize], 3),
		0o140...0o147 => cpu.bit(reg_vec[(instruction % 8) as usize], 4),
		0o150...0o157 => cpu.bit(reg_vec[(instruction % 8) as usize], 5),
		0o160...0o167 => cpu.bit(reg_vec[(instruction % 8) as usize], 6),
		0o170...0o177 => cpu.bit(reg_vec[(instruction % 8) as usize], 7),
		0o200...0o207 => cpu.res(reg_vec[(instruction % 8) as usize], 0),
		0o210...0o217 => cpu.res(reg_vec[(instruction % 8) as usize], 1),
		0o220...0o227 => cpu.res(reg_vec[(instruction % 8) as usize], 2),
		0o230...0o237 => cpu.res(reg_vec[(instruction % 8) as usize], 3),
		0o240...0o247 => cpu.res(reg_vec[(instruction % 8) as usize], 4),
		0o250...0o257 => cpu.res(reg_vec[(instruction % 8) as usize], 5),
		0o260...0o267 => cpu.res(reg_vec[(instruction % 8) as usize], 6),
		0o270...0o277 => cpu.res(reg_vec[(instruction % 8) as usize], 7),
		0o300...0o307 => cpu.set(reg_vec[(instruction % 8) as usize], 0),
		0o310...0o317 => cpu.set(reg_vec[(instruction % 8) as usize], 1),
		0o320...0o327 => cpu.set(reg_vec[(instruction % 8) as usize], 2),
		0o330...0o337 => cpu.set(reg_vec[(instruction % 8) as usize], 3),
		0o340...0o347 => cpu.set(reg_vec[(instruction % 8) as usize], 4),
		0o350...0o357 => cpu.set(reg_vec[(instruction % 8) as usize], 5),
		0o360...0o367 => cpu.set(reg_vec[(instruction % 8) as usize], 6),
		0o370...0o377 => cpu.set(reg_vec[(instruction % 8) as usize], 7),
		_ => panic!("invalid instruction given to prefix_cb"),
	}

	return cpu;
}

impl Cpu {

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
		let x = self.sub8(a,b);
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

	fn bit(&mut self, a : Register, bit: u8){
		let val = self.load_register8(a);
		let complement_bit = !(val & (1 << bit));
		self.set_flag(ZERO_FLAG, complement_bit > 0);
		self.set_flag(HALF_FLAG, true);
		self.set_flag(NEG_FLAG, false);
	}

	fn res(&mut self, a : Register, bit : u8){
		let val = self.load_register8(a);
		let reset_val = val & !(1 << bit);
		self.save_register8(a,reset_val);
	}

	fn set(&mut self, a : Register, bit : u8){
		let val = self.load_register8(a);
		let set_val = val | (1 << bit);
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
		let hf = (a & 0xf) < (b & 0xf);
		let (tmp,ov) = a.overflowing_sub(b);
		self.set_flag(NEG_FLAG, true);
		self.set_flag(ZERO_FLAG, tmp == 0);
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
}


struct Mmu{

	gb_internal_rom : [u8; 0x100],

	gb_cartridge : [u8; 65536],
}

impl Mmu {
	
  fn load_cartridge(&mut self){
  	self.gb_internal_rom = 
			[0x31, 0xfe, 0xff, 0xaf, 0x21, 0xff, 0x9f, 0x32, 0xcb, 0x7c, 0x20, 0xfb, 0x21, 0x26, 0xff, 0x0e,
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
 			 0xf5, 0x06, 0x19, 0x78, 0x86, 0x23, 0x05, 0x20, 0xfb, 0x86, 0x20, 0xfe, 0x3e, 0x01, 0xe0, 0x50,];
  }

  fn fetch(&self, address : u16) -> u8{
  	let (mirror_address,_) = self.mirror_address(address);
  	if address < 0x100 && (self.gb_cartridge[0xFF50] & 1) == 0{
  		return self.gb_internal_rom[mirror_address as usize];
  	} else {
  		return self.gb_cartridge[mirror_address as usize];	
  	}  	
  }

  fn save(&mut self, address : u16, value:u8){
  	let (mirror_address,writable) = self.mirror_address(address);
  	if writable {
  		self.gb_cartridge[mirror_address as usize] = value;
  	}
  }

  fn mirror_address(&self, address : u16) -> (u16,bool){
  	if address >= 0xE000 && address < 0xFE00 {
  		(address - 0x2000,true)
  	} else if address < 0xA000 || (address >= 0xFEA0 && address < 0xFF00){
  		(address,false)
  	} else {
  		(address,true)
  	}
  }

}