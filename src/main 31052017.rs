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
	memory : [u8; 65536],
}

fn program(mut cpu : Cpu) {
	let input /*[u8;10] =*/ =  vec![0xCB, 0o020];

	while cpu.PC < input.len() as u16 {

		match cpu.fetch_u8(&input) {
			0x00 => println!("NOP"), 
			0x01 => { let x = cpu.fetch_u16(&input);
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
			0x06 => { let x = cpu.fetch_u8(&input);
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
								let y = cpu.fetch_u16(&input);
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
			0x0E => { let x = cpu.fetch_u8(&input);
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
			0x11 => { let x = cpu.fetch_u16(&input);
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
			0x16 => { let x = cpu.fetch_u8(&input); 
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
			0x18 => { let x = cpu.fetch_i8(&input) - 2; 
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
  		0x1E => { let x = cpu.fetch_u8(&input); 
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
			0x20 => { let x = cpu.fetch_i8(&input) - 2; 
								if !(cpu.get_flag(ZERO_FLAG)) { 
									cpu.PC = ((cpu.PC as i16).wrapping_add(x as i16)) as u16;
								};
								println!("JR NZ r8")
							},
			0x21 => { let x = cpu.fetch_u16(&input);
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
			0x26 => { let x = cpu.fetch_u8(&input);
								cpu.save_register8(Register::H, x);
								println!("LD H,d8")
							},
			0x27 => { println!("unimplemented DAA") },
			0x28 => { let x = cpu.fetch_i8(&input) - 2; 
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
			0x2E => { let x = cpu.fetch_u8(&input); 
								cpu.save_register8(Register::L, x);
								println!("LD L d8")
							},
			0x2F => { cpu.A = !cpu.A;
								println!("CPL")
							},
			0x30 => { let x = cpu.fetch_i8(&input) - 2;
								if !cpu.get_flag(CARRY_FLAG) {
									cpu.PC = ((cpu.PC as i16).wrapping_add(x as i16)) as u16;
								};
								println!("JR NC r8") },
			0x31 => { let x = cpu.fetch_u16(&input);
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
			0x36 => { let x = cpu.fetch_u8(&input);
								cpu.save_register_address(Register::HL, x);
								println!("LD (HL) d8") },
			0x37 => { cpu.set_flag(CARRY_FLAG, true);
								cpu.set_flag(NEG_FLAG, false);
								cpu.set_flag(HALF_FLAG, false);
								println!("SCF") 
							},
			0x38 => { let x = cpu.fetch_i8(&input) - 2; 
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
			0x3E => { let x = cpu.fetch_u8(&input);
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
			0xC2 => { let x = cpu.fetch_u16(&input);
								if !cpu.get_flag(ZERO_FLAG) {
									cpu.save_register16(Register::PC, x);
								};
								println!("JP NZ a16") 
							},
			0xC3 => { let x = cpu.fetch_u16(&input);
								cpu.save_register16(Register::PC, x);
								println!("JP a16") 
							}, 
			0xC4 => { let a16 = cpu.fetch_u16(&input);
								if !cpu.get_flag(ZERO_FLAG) {
									push!(cpu,PC);
									cpu.save_register16(Register::PC, a16);
								};
								println!("CALL NZ a16") 
							},
			0xC5 => { push!(cpu,BC);
								println!("PUSH BC") 
							},
			0xC6 => { apply8!(cpu,A,add8,cpu.fetch_u8(&input));
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
			0xCA => { let x = cpu.fetch_u16(&input);
								if cpu.get_flag(ZERO_FLAG) {
									cpu.save_register16(Register::PC, x);
								};
								println!("JP Z a16") 
							},
			0xCB => { cpu = prefix_cb(cpu, &input);
								println!("PREFIX CB") },
			0xCC => { let a16 = cpu.fetch_u16(&input);
								if cpu.get_flag(ZERO_FLAG) {
									push!(cpu,PC);
									cpu.save_register16(Register::PC, a16);
								};
								println!("CALL Z a16") 
							},
			0xCD => { push!(cpu,PC);
								let a16 = cpu.fetch_u16(&input);
								cpu.save_register16(Register::PC, a16);
								println!("CALL a16") 
							},
			0xCE => { apply8!(cpu,A,add8,cpu.fetch_u8(&input).wrapping_add(cpu.get_flag(CARRY_FLAG) as u8));
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
			0xD2 => { let x = cpu.fetch_u16(&input);
								if !cpu.get_flag(CARRY_FLAG) {
									cpu.save_register16(Register::PC, x);
								};
								println!("JP NC a16")
							},
			0xD3 => { println!("0xD3 N/A") },
			0xD4 => { let a16 = cpu.fetch_u16(&input);
								if !cpu.get_flag(CARRY_FLAG) {
									push!(cpu,PC);
									cpu.save_register16(Register::PC, a16);
								};
								println!("CALL NC a16") 
							},
			0xD5 => { push!(cpu,DE);
								println!("PUSH DE") 
							},
			0xD6 => { apply8!(cpu,A,sub8,cpu.fetch_u8(&input));
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
			0xDA => { let x = cpu.fetch_u16(&input);
								if cpu.get_flag(CARRY_FLAG) {
									cpu.save_register16(Register::PC, x);
								};
								println!("JP C a16") 
							},
			0xDB => { println!("0xDB N/A") },
			0xDC => { let a16 = cpu.fetch_u16(&input);
								if cpu.get_flag(CARRY_FLAG) {
									push!(cpu,PC);							
									cpu.save_register16(Register::PC, a16);
								};
								println!("CALL C a16") 
							},
			0xDD => { println!("0xDD N/A") },
			0xDE => { apply8!(cpu,A,sub8,cpu.fetch_u8(&input).wrapping_add(cpu.get_flag(CARRY_FLAG) as u8));
								println!("SBC A d8") 
							},
			0xDF => { push!(cpu,PC);
								cpu.save_register16(Register::PC, 0x18);
								println!("RST 0x18") 
							},
			0xE0 => { println!("LD (0xFF00 + a8), A") },
			0xE1 => { println!("POP HL") },
			0xE2 => { println!("LD (0xFF00 + C), A") },
			0xE3 => { println!("0xE3 N/A") },
			0xE4 => { println!("0xE4 N/A") },
			0xE5 => { println!("PUSH HL") },
			0xE6 => { println!("AND d8") },
			0xE7 => { println!("RST 0x20") },
			0xE8 => { println!("ADD SP r8") },
			0xE9 => { println!("JP (HL)") },
			0xEA => { println!("LD (a16) A") },
			0xEB => { println!("0xEB N/A") },
			0xEC => { println!("0xEC N/A") },
			0xED => { println!("0xED N/A") },
			0xEE => { println!("XOR d8") },
			0xEF => { println!("RST 0x28") },
			_ => println!("unimplemented"),
		};
	};

	println!("{:8b}	{:8b}", cpu.B, cpu.F);

}

fn main() {
	let mut c = Cpu{
		A: 0b11100001,
		F: 0b10000,
		B: 0b11011011,
		C: 0x00,
		D: 0,
		E: 0,
		H: 0,
		L: 0,
		SP: 0x00,
		PC: 0,

		interrupt_enabled : false,

		memory : [0; 65536],
	};

	c.memory[0] = 0x5;
	c.memory[1] = 0x4;
	println!("{:8b}	{:8b}", c.B, c.F);
	program(c);
}

fn prefix_cb(mut cpu : Cpu, input : &[u8]) -> Cpu{

	let reg_vec = vec![Register::B,Register::C,Register::D,Register::E,Register::H,Register::L,Register::HL_address,Register::A];

	let instruction = cpu.fetch_u8(input);

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
		return self.memory[loc as usize];
	}

	fn load_register_address(&self, reg: Register) -> u8{
		return self.memory[self.load_register16(reg) as usize];
	}

	fn save_address(&mut self, loc: u16, value :u8) {
		self.memory[loc as usize] = value;
	}

	fn save_register_address(&mut self, reg: Register, value :u8){
		self.memory[self.load_register16(reg) as usize] = value;
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

	/*
	* Adds two u8s
	* sets the neg flag to false
	* zero flag if it overflows
	* half flag if a carry from 4 -> 5
	*/

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
 