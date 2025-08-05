package main

import "core:fmt"
import "core:log"
import "core:math/rand"

Address :: distinct u16

MM :: enum u8 {
	Rom          = 0,
	Switch_Rom   = 1,
	Vram         = 2,
	External_Ram = 3,
	Wram         = 4,
	Echo_Ram     = 5,
	OAM          = 6,
	Forbidden    = 7,
	IO           = 8,
	Hram         = 9,
	Interupt     = 10,
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


get_reg16 :: proc($r16: R16) -> (reg: ^u16) {
	when r16 == .NONE {
		#panic("get r16 is none")
	}
	switch r16 {
	case .NONE:
	case .AF:
		return &cpu.registers.AF
	case .BC:
		return &cpu.registers.BC
	case .DE:
		return &cpu.registers.DE
	case .HL:
		return &cpu.registers.HL
	case .SP:
		return transmute(^u16)&cpu.SP
	}

	panic("reg16 not defind")
}

get_reg8 :: proc($r8: R8) -> (reg: ^u8) {
	when r8 == .NONE {
		#panic("get r8 is none")
	}
	switch r8 {
	case .NONE:
	case .A:
		return &cpu.registers.A
	case .B:
		return &cpu.registers.B
	case .C:
		return &cpu.registers.C
	case .D:
		return &cpu.registers.D
	case .E:
		return &cpu.registers.E
	case .H:
		return &cpu.registers.H
	case .L:
		return &cpu.registers.L
	}

	panic("reg8 not defind")
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

rotate_left_includes_carry :: proc(val: ^u8) {
	flags := &cpu.registers.F
	msb := val^ >> 7
	c := u8(.C in flags^)
	val^ = val^ << 1 + c
	flags^ = flags^ + {.C} if msb == 1 else flags^ - {.C}
}

rotate_left :: proc(val: ^u8) {
	msb := val^ >> 7
	val^ = val^ << 1 + msb
}

rotate_right_includes_carry :: proc(val: ^u8) {
	flags := &cpu.registers.F
	lsb := val^ & 1
	c := u8(.C in flags)
	val^ = val^ >> 1
	if c == 1 {
		val^ += 0x80
	}
	flags^ = flags^ + {.C} if lsb == 1 else flags^ - {.C}
}


rotate_right :: proc(val: ^u8) {
	lsb := val^ & 1
	val^ = val^ >> 1
	if lsb == 1 {
		val^ += 0x80
	}
}


toggle_flag :: #force_inline proc(cond: bool, flag: Flags) {
	if cond {
		cpu.registers.F += {flag}
	} else {
		cpu.registers.F -= {flag}
	}
}


is_condition_valid :: proc($condtion: ConditionCode) -> bool {
	switch condtion {
	case .NZ:
		return .Z not_in cpu.registers.F
	case .Z:
		return .Z in cpu.registers.F
	case .NC:
		return .C not_in cpu.registers.F
	case .C:
		return .C in cpu.registers.F
	}
	panic("Condition error")
}


@(private)
is_address_in_mm :: #force_inline proc(address: Address, $range: MM) -> bool {
	return MM_Start[range] <= address && address <= MM_End[range]
}

copy :: proc(dest: Address, source: Address, len: u64) {
	for i in 0 ..< len {
		write_u8(dest + Address(i), read_u8(source + Address(i)))
	}
}

read_u8 :: proc(address: Address) -> u8 {
	switch {
	case is_address_in_mm(address, MM.Rom) || is_address_in_mm(address, MM.Switch_Rom):
		return rom[address]
	case is_address_in_mm(address, MM.External_Ram):
		return extern_ram[address - MM_Start[MM.External_Ram]]
	case is_address_in_mm(address, MM.Vram):
		return vram[address - MM_Start[MM.Vram]]
	case is_address_in_mm(address, MM.Wram):
		return wram[address - MM_Start[MM.Wram]]
	case is_address_in_mm(address, MM.OAM):
		return oam[address - MM_Start[MM.OAM]]
	case address == 0xFF04:
		// DIV
		// Should return a div timer, but a random number works just as well for Tetris
		return u8(rand.int31_max(0xFF))
	case address == 0xFF40:
		return transmute(u8)gpu.controll
	case address == 0xFF42:
		return gpu.scroll_y
	case address == 0xFF43:
		return gpu.scroll_x
	case address == 0xFF44:
		return gpu.scanline
	case address == 0xFF00:
		return transmute(u8)cpu.joypad
	// return 0
	case address == 0xFF0F:
		return transmute(u8)cpu.interrupt.flags
	case address == 0xFFFF:
		return transmute(u8)cpu.interrupt.enable
	case is_address_in_mm(address, MM.Hram):
		return hram[address - MM_Start[MM.Hram]]
	case is_address_in_mm(address, MM.IO):
		return io[address - MM_Start[MM.IO]]
	case:
		log.fatal("Reading Adress: 0x%04X is invalid;", address)
		return 0
	}
}


read_u16 :: proc(address: Address) -> u16 {
	return u16(read_u8(address)) | (u16(read_u8(address + 1)) << 8)
}


write_u8 :: proc(address: Address, value: u8, location := #caller_location) {
	switch {
	case is_address_in_mm(address, MM.External_Ram):
		extern_ram[address - MM_Start[MM.External_Ram]] = value
	case is_address_in_mm(address, MM.Vram):
		fmt.printfln("[%v] VRAM($%04X): 0x%02X", location, address, value)
		vram[address - MM_Start[MM.Vram]] = value
		if address <= 0x97FF {
			update_tile(address)
		}
	case is_address_in_mm(address, MM.Wram):
		wram[address - MM_Start[MM.Wram]] = value
	case is_address_in_mm(address, MM.OAM):
		oam[address - MM_Start[MM.OAM]] = value
	case is_address_in_mm(address, MM.Hram):
		hram[address - MM_Start[MM.Hram]] = value
	case address == 0xFF40:
		gpu.controll = transmute(bit_set[LCD_Control;u8])value
	case address == 0xFF42:
		gpu.scroll_y = value
	case address == 0xFF43:
		gpu.scroll_x = value
	case address == 0xFF46:
		copy(MM_Start[MM.OAM], Address(value << 8), 160)
	case address == 0xFF47:
		for &color, i in bg_palette {
			color = DEFAULT_PALETTE[(value >> (u8(i) * 2)) & 3]
		}
	case address == 0xFF48:
		for &color, i in sprite_palettes[0] {
			color = DEFAULT_PALETTE[(value >> (u8(i) * 2)) & 3]
		}
	case address == 0xFF49:
		for &color, i in sprite_palettes[1] {
			color = DEFAULT_PALETTE[(value >> (u8(i) * 2)) & 3]
		}
	case address == 0xFFFF:
		cpu.interrupt.enable = transmute(bit_set[Interrupt;u8])value
	case address == 0xFF0F:
		cpu.interrupt.flags = transmute(bit_set[Interrupt;u8])value
	case is_address_in_mm(address, MM.IO):
		io[address - MM_Start[MM.IO]] = value
	case:
		log.infof("Writing %v Adress: 0x%04X is invalid", value, address)
	}

}

write_u16 :: proc(address: Address, value: u16) {
	write_u8(address, u8(value & 0x00FF))
	write_u8(address + 1, u8((value & 0xFF00) >> 8))
}

push_sp :: proc(value: u16) {
	cpu.SP -= 2
	write_u16(cpu.SP, value)
	log.warnf("Push SP($%04X): 0x%04X", cpu.SP, value)
}
pop_sp :: proc() -> u16 {
	value := read_u16(cpu.SP)
	log.warnf("Pop SP($%04X): 0x%04X", cpu.SP, value)
	cpu.SP += 2
	return value
}

