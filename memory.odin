package main

import "core:math/rand"
import "core:slice"
import inst "instructions"

Address :: distinct u16

MM :: enum u16 {
	Rom,
	Switch_Rom,
	Vram,
	External_Ram,
	WRam,
	Echo_Ram,
	OAM,
	Forbidden,
	IO,
	Hram,
	Interupt,
}

Hardware_Registers :: enum u64 {
	JOYP,
	SB,
	SC,
	DIV,
	TIMA,
	TMA,
	TAC,
	IF,
	NR10,
	NR11,
	NR12,
	NR13,
	NR14,
	NR21,
	NR22,
	NR23,
	NR24,
	NR30,
	NR31,
	NR32,
	NR33,
	NR34,
	NR41,
	NR42,
	NR43,
	NR44,
	NR50,
	NR51,
	NR52,
	Wave_Ram,
	LCDC,
	STAT,
	SCY,
	SCX,
	LY,
	LYC,
	DMA,
	BGP,
	OBP0,
	OBP1,
	WY,
	WX,
	KEY1,
	VBK,
	HDMA1,
	HDMA2,
	HDMA3,
	HDMA4,
	HDMA5,
	RP,
	BCPS,
	BCPD,
	OCPS,
	OCPD,
	OPRI,
	SVBK,
	PCM12,
	PCM34,
	IE,
}

HR_Address :: [?]Address {
	0xFF00,
	0xFF01,
	0xFF02,
	0xFF04,
	0xFF05,
	0xFF06,
	0xFF07,
	0xFF0F,
	0xFF10,
	0xFF11,
	0xFF12,
	0xFF13,
	0xFF14,
	0xFF16,
	0xFF17,
	0xFF18,
	0xFF19,
	0xFF1A,
	0xFF1B,
	0xFF1C,
	0xFF1D,
	0xFF1E,
	0xFF20,
	0xFF21,
	0xFF22,
	0xFF23,
	0xFF24,
	0xFF25,
	0xFF26,
	0xFF30,
	0xFF40,
	0xFF41,
	0xFF42,
	0xFF43,
	0xFF44,
	0xFF45,
	0xFF46,
	0xFF47,
	0xFF48,
	0xFF49,
	0xFF4A,
	0xFF4B,
	0xFF4D,
	0xFF4F,
	0xFF51,
	0xFF52,
	0xFF53,
	0xFF54,
	0xFF55,
	0xFF56,
	0xFF68,
	0xFF69,
	0xFF6A,
	0xFF6B,
	0xFF6C,
	0xFF70,
	0xFF76,
	0xFF77,
	0xFFFF,
}

MM_End :: [?]Address {
	0x3FFF,
	0x7FFF,
	0x9FFF,
	0xBFFF,
	0xDFFF,
	0xFDFF,
	0xFE9F,
	0xFEFF,
	0xFF7F,
	0xFFFE,
	0xFFFF,
}

MM_Start :: [?]Address {
	0x0000,
	0x4000,
	0x8000,
	0xA000,
	0xC000,
	0xE000,
	0xFE00,
	0xFEA0,
	0xFF00,
	0xFF80,
	0xFFFF,
}

@(rodata)
io_reset := [0x100]u8 {
	0x0F,
	0x00,
	0x7C,
	0xFF,
	0x00,
	0x00,
	0x00,
	0xF8,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0x01,
	0x80,
	0xBF,
	0xF3,
	0xFF,
	0xBF,
	0xFF,
	0x3F,
	0x00,
	0xFF,
	0xBF,
	0x7F,
	0xFF,
	0x9F,
	0xFF,
	0xBF,
	0xFF,
	0xFF,
	0x00,
	0x00,
	0xBF,
	0x77,
	0xF3,
	0xF1,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0x00,
	0xFF,
	0x00,
	0xFF,
	0x00,
	0xFF,
	0x00,
	0xFF,
	0x00,
	0xFF,
	0x00,
	0xFF,
	0x00,
	0xFF,
	0x00,
	0xFF,
	0x91,
	0x80,
	0x00,
	0x00,
	0x00,
	0x00,
	0x00,
	0xFC,
	0x00,
	0x00,
	0x00,
	0x00,
	0xFF,
	0x7E,
	0xFF,
	0xFE,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0x3E,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xC0,
	0xFF,
	0xC1,
	0x00,
	0xFE,
	0xFF,
	0xFF,
	0xFF,
	0xF8,
	0xFF,
	0x00,
	0x00,
	0x00,
	0x8F,
	0x00,
	0x00,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xFF,
	0xCE,
	0xED,
	0x66,
	0x66,
	0xCC,
	0x0D,
	0x00,
	0x0B,
	0x03,
	0x73,
	0x00,
	0x83,
	0x00,
	0x0C,
	0x00,
	0x0D,
	0x00,
	0x08,
	0x11,
	0x1F,
	0x88,
	0x89,
	0x00,
	0x0E,
	0xDC,
	0xCC,
	0x6E,
	0xE6,
	0xDD,
	0xDD,
	0xD9,
	0x99,
	0xBB,
	0xBB,
	0x67,
	0x63,
	0x6E,
	0x0E,
	0xEC,
	0xCC,
	0xDD,
	0xDC,
	0x99,
	0x9F,
	0xBB,
	0xB9,
	0x33,
	0x3E,
	0x45,
	0xEC,
	0x52,
	0xFA,
	0x08,
	0xB7,
	0x07,
	0x5D,
	0x01,
	0xFD,
	0xC0,
	0xFF,
	0x08,
	0xFC,
	0x00,
	0xE5,
	0x0B,
	0xF8,
	0xC2,
	0xCE,
	0xF4,
	0xF9,
	0x0F,
	0x7F,
	0x45,
	0x6D,
	0x3D,
	0xFE,
	0x46,
	0x97,
	0x33,
	0x5E,
	0x08,
	0xEF,
	0xF1,
	0xFF,
	0x86,
	0x83,
	0x24,
	0x74,
	0x12,
	0xFC,
	0x00,
	0x9F,
	0xB4,
	0xB7,
	0x06,
	0xD5,
	0xD0,
	0x7A,
	0x00,
	0x9E,
	0x04,
	0x5F,
	0x41,
	0x2F,
	0x1D,
	0x77,
	0x36,
	0x75,
	0x81,
	0xAA,
	0x70,
	0x3A,
	0x98,
	0xD1,
	0x71,
	0x02,
	0x4D,
	0x01,
	0xC1,
	0xFF,
	0x0D,
	0x00,
	0xD3,
	0x05,
	0xF9,
	0x00,
	0x0B,
	0x00,
}

rom: [0x8000]u8
extern_ram: [0x2000]u8
io: [0x100]u8
vram: [0x2000]u8
oam: [0x100]u8
wram: [0x2000]u8
hram: [0x80]u8

get_reg16 :: proc(cpu: ^Cpu, name: inst.Operand_Name) -> (reg: ^u16) {
	#partial switch name {
	case .O_AF:
		return &cpu.registers.AF.full
	case .O_BC:
		return &cpu.registers.BC.full
	case .O_DE:
		return &cpu.registers.DE.full
	case .O_HL:
		return &cpu.registers.HL.full
	case .O_SP:
		return transmute(^u16)&cpu.registers.SP
	}

	panic("reg16 not defind")
}

get_reg8 :: proc(cpu: ^Cpu, name: inst.Operand_Name) -> (reg: ^u8) {
	#partial switch name {
	case .O_A:
		return &cpu.registers.AF.single.upper
	// case "F":
	// 	return &cpu.registers.AF.single.lower
	case .O_B:
		return &cpu.registers.BC.single.upper
	case .O_C:
		return &cpu.registers.BC.single.lower
	case .O_D:
		return &cpu.registers.DE.single.upper
	case .O_E:
		return &cpu.registers.DE.single.lower
	case .O_H:
		return &cpu.registers.HL.single.upper
	case .O_L:
		return &cpu.registers.HL.single.lower
	}

	panic("reg8 not defind")
}

is_reg8 :: proc(name: inst.Operand_Name) -> bool {
	return name in bit_set[inst.Operand_Name]{.O_A, .O_B, .O_C, .O_D, .O_E, .O_H, .O_L}
}

is_reg16 :: proc(name: inst.Operand_Name) -> bool {
	return name in bit_set[inst.Operand_Name]{.O_AF, .O_BC, .O_DE, .O_HL}
}


split_u16 :: proc(val: u16) -> (msb: u8, lsb: u8) {
	return u8(val >> 8), u8(val & 0xFF)
}

is_half_carried_add :: proc {
	is_half_carried_add8,
	is_half_carried_add16,
}


is_half_carried_sub :: proc {
	is_half_carried_sub8,
	is_half_carried_sub16,
}

is_half_carried_add8 :: proc(a, b: u8) -> bool {
	return (((a & 0xF) + (b & 0xF)) & 0x10) == 0x10
}

is_half_carried_add16 :: proc(a, b: u16) -> bool {
	return (((a & 0xFFF) + (b & 0xFFF)) & 0x1000) == 0x1000
}
is_half_carried_sub8 :: proc(a, b: u8) -> bool {
	return (((a & 0xF) - (b & 0xF)) & 0x10) == 0x10
}

is_half_carried_sub16 :: proc(a, b: u16) -> bool {
	return (((a & 0xFFF) - (b & 0xFFF)) & 0x1000) == 0x1000
}

rotate_left_includes_carry :: proc(val: ^u8, flags: ^bit_set[Flags;u8]) {
	msb := val^ >> 7
	c := u8(.C in flags)
	val^ = val^ << 1 + c
	flags^ = flags^ + {.C} if msb == 1 else flags^ - {.C}
}

rotate_left :: proc(val: ^u8) -> (msb: u8) {
	msb = val^ >> 7
	val^ = val^ << 1 + msb
	return
}

rotate_right_includes_carry :: proc(val: ^u8, flags: ^bit_set[Flags;u8]) {
	lsb := val^ & 1
	c := u8(.C in flags)
	val^ = val^ >> 1
	if c == 1 {
		val^ += 0x80
	}
	flags^ = flags^ + {.C} if lsb == 1 else flags^ - {.C}
}

rotate_right :: proc(val: ^u8) -> (lsb: u8) {
	lsb = val^ & 1
	val^ = val^ >> 1
	if lsb == 1 {
		val^ += 0x80
	}
	return
}

check_condition :: proc(flags: ^bit_set[Flags;u8], condtion: Condition) -> bool {
	switch condtion {
	case .NZ:
		return .Z not_in flags
	case .Z:
		return .Z in flags
	case .NC:
		return .C not_in flags
	case .C:
		return .C in flags
	}

	panic("Condition error")
}

get_condition :: proc(condtion: inst.Operand_Name) -> Condition {
	#partial switch condtion {
	case .O_C:
		return .C
	case .O_NC:
		return .NC
	case .O_Z:
		return .Z
	case .O_NZ:
		return .NZ
	}
	return nil
}

@(private)
is_address_in_mm :: #force_inline proc(address: Address, $range: MM) -> bool {
	return MM_Start[range] <= address && address >= MM_End[range]
}

copy :: proc(dest: Address, source: Address, len: u64) {
	for i in 0 ..= len {
		write_u8(dest + Address(i), read_u8(source + Address(i)))
	}
}

read_u8 :: proc(address: Address) -> u8 {
	switch {
	case is_address_in_mm(address, MM.Rom):
		return rom[address]
	case is_address_in_mm(address, MM.Vram):
		return vram[address - MM_End[MM.Vram]]
	case is_address_in_mm(address, MM.External_Ram):
		return extern_ram[address - MM_End[MM.External_Ram]]
	case is_address_in_mm(address, MM.WRam):
		return wram[address - MM_End[MM.WRam]]
	case is_address_in_mm(address, MM.OAM):
		return oam[address - MM_End[MM.OAM]]
	case address == HR_Address[Hardware_Registers.DIV]:
		// Should return a div timer, but a random number works just as well for Tetris
		return u8(rand.int31_max(0xFF))
	case address == HR_Address[Hardware_Registers.LCDC]:
		return gpu.controll
	case address == HR_Address[Hardware_Registers.SCY]:
		return gpu.scroll_y
	case address == HR_Address[Hardware_Registers.SCX]:
		return gpu.scroll_x
	case address == HR_Address[Hardware_Registers.JOYP]:
		assert(false, "todo")
		return 0
	case address == HR_Address[Hardware_Registers.IF]:
		return u8(cpu.interrupt.enable)
	case address == HR_Address[Hardware_Registers.IE]:
		return transmute(u8)cpu.interrupt.flags
	case is_address_in_mm(address, MM.Hram):
		return hram[address - MM_End[MM.Hram]]
	case is_address_in_mm(address, MM.IO):
		return oam[address - MM_End[MM.IO]]
	}

	panic("invalid Address")
}


read_u16 :: proc(address: Address) -> u16 {
	return u16(read_u8(address)) | u16(read_u8(address + 1) << 8)
}


write_u8 :: proc(address: Address, value: u8) {
	switch {
	case is_address_in_mm(address, MM.Rom):
		rom[address] = value
	case is_address_in_mm(address, MM.Vram):
		vram[address - MM_End[MM.Vram]] = value
	case is_address_in_mm(address, MM.WRam):
		wram[address - MM_End[MM.WRam]] = value
	case is_address_in_mm(address, MM.OAM):
		oam[address - MM_End[MM.OAM]] = value
	case is_address_in_mm(address, MM.Hram):
		hram[address - MM_End[MM.Hram]] = value
	case address == HR_Address[Hardware_Registers.DIV]:
		// Should return a div timer, but a random number works just as well for Tetris
		u8(rand.int31_max(0xFF))
	case address == HR_Address[Hardware_Registers.LCDC]:
		gpu.controll = value
	case address == HR_Address[Hardware_Registers.SCY]:
		gpu.scroll_y = value
	case address == HR_Address[Hardware_Registers.SCX]:
		gpu.scroll_x = value
	case address == HR_Address[Hardware_Registers.IE]:
		cpu.interrupt.enable = bool(value)
	case address == HR_Address[Hardware_Registers.IF]:
		cpu.interrupt.flags = transmute(bit_set[Interrupt;u8])value
	case address == HR_Address[Hardware_Registers.DMA]:
		copy(MM_Start[MM.OAM], Address(value << 8), 160)
	case address == HR_Address[Hardware_Registers.BGP]:
		assert(false, "todo")
	case address == HR_Address[Hardware_Registers.OBP0]:
		assert(false, "todo")
	case address == HR_Address[Hardware_Registers.OBP1]:
		assert(false, "todo")
	case is_address_in_mm(address, MM.IO):
		hram[address - MM_End[MM.IO]] = value
	}

	panic("invalid Address")
}

write_u16 :: proc(address: Address, value: u16) {
	write_u8(address, u8(value & 0x00FF))
	write_u8(address + 1, u8((value & 0xFF00) >> 8))
}

