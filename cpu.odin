package main

import "core:os"
import inst "instructions"

ConditionCode :: enum u8 {
	Z,
	NZ,
	C,
	NC,
}

R8 :: enum u8 {
	A,
	B,
	C,
	D,
	E,
	H,
	L,
}

n8 :: distinct u8
n16 :: distinct u16
e8 :: distinct i8

R16 :: enum u8 {
	AF,
	SP,
	BC,
	HL,
	DE,
}

VEC :: enum {
	V00 = 0x00,
	V08 = 0x08,
	V10 = 0x10,
	V18 = 0x18,
	V20 = 0x20,
	V28 = 0x28,
	V30 = 0x30,
	V38 = 0x38,
}

rotate_left_includes_carry :: proc(val: ^u8) {
	flags := &cpu.registers.AF.single.F
	msb := val^ >> 7
	c := u8(.C in flags^)
	val^ = val^ << 1 + c
	flags^ = flags^ + {.C} if msb == 1 else flags^ - {.C}
}

rotate_left :: proc(val: ^u8) {
	msb := val^ >> 7
	val^ = val^ << 1 + msb
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


rotate_right :: proc(val: ^u8) {
	lsb := val^ & 1
	val^ = val^ >> 1
	if lsb == 1 {
		val^ += 0x80
	}
}


toggle_flag :: #force_inline proc(cond: bool, flag: Flags) {
	if cond {
		cpu.registers.AF.single.F += {flag}
	} else {
		cpu.registers.AF.single.F -= {flag}
	}
}

execute_instruction :: proc(opcode: u8) {
	switch opcode {
	// NOOP
	case 0x0:
		break
	case 0x1:
		ld_r16_n16(.BC)
	case 0x2:
		ld_r16_A(.BC)
	case 0x3:
		inc8(.B)
	case 0x4:
		inc8(.B)
	case 0x5:
		dec8(.B)
	case 0x6:
	case 0x7:
	case 0x8:
	case 0x9:
	case 0xa:
	case 0xb:
	case 0xc:
	case 0xd:
	case 0xe:
	case 0xf:
	case 0x10:
	case 0x11:
	case 0x12:
	case 0x13:
	case 0x14:
	case 0x15:
	case 0x16:
	case 0x17:
	case 0x18:
	case 0x19:
	case 0x1a:
	case 0x1b:
	case 0x1c:
	case 0x1d:
	case 0x1e:
	case 0x1f:
	case 0x20:
	case 0x21:
	case 0x22:
	case 0x23:
	case 0x24:
	case 0x25:
	case 0x26:
	case 0x27:
	case 0x28:
	case 0x29:
	case 0x2a:
	case 0x2b:
	case 0x2c:
	case 0x2d:
	case 0x2e:
	case 0x2f:
	case 0x30:
	case 0x31:
	case 0x32:
	case 0x33:
	case 0x34:
	case 0x35:
	case 0x36:
	case 0x37:
	case 0x38:
	case 0x39:
	case 0x3a:
	case 0x3b:
	case 0x3c:
	case 0x3d:
	case 0x3e:
	case 0x3f:
	case 0x40:
	case 0x41:
	case 0x42:
	case 0x43:
	case 0x44:
	case 0x45:
	case 0x46:
	case 0x47:
	case 0x48:
	case 0x49:
	case 0x4a:
	case 0x4b:
	case 0x4c:
	case 0x4d:
	case 0x4e:
	case 0x4f:
	case 0x50:
	case 0x51:
	case 0x52:
	case 0x53:
	case 0x54:
	case 0x55:
	case 0x56:
	case 0x57:
	case 0x58:
	case 0x59:
	case 0x5a:
	case 0x5b:
	case 0x5c:
	case 0x5d:
	case 0x5e:
	case 0x5f:
	case 0x60:
	case 0x61:
	case 0x62:
	case 0x63:
	case 0x64:
	case 0x65:
	case 0x66:
	case 0x67:
	case 0x68:
	case 0x69:
	case 0x6a:
	case 0x6b:
	case 0x6c:
	case 0x6d:
	case 0x6e:
	case 0x6f:
	case 0x70:
	case 0x71:
	case 0x72:
	case 0x73:
	case 0x74:
	case 0x75:
	case 0x76:
	case 0x77:
	case 0x78:
	case 0x79:
	case 0x7a:
	case 0x7b:
	case 0x7c:
	case 0x7d:
	case 0x7e:
	case 0x7f:
	case 0x80:
	case 0x81:
	case 0x82:
	case 0x83:
	case 0x84:
	case 0x85:
	case 0x86:
	case 0x87:
	case 0x88:
	case 0x89:
	case 0x8a:
	case 0x8b:
	case 0x8c:
	case 0x8d:
	case 0x8e:
	case 0x8f:
	case 0x90:
	case 0x91:
	case 0x92:
	case 0x93:
	case 0x94:
	case 0x95:
	case 0x96:
	case 0x97:
	case 0x98:
	case 0x99:
	case 0x9a:
	case 0x9b:
	case 0x9c:
	case 0x9d:
	case 0x9e:
	case 0x9f:
	case 0xa0:
	case 0xa1:
	case 0xa2:
	case 0xa3:
	case 0xa4:
	case 0xa5:
	case 0xa6:
	case 0xa7:
	case 0xa8:
	case 0xa9:
	case 0xaa:
	case 0xab:
	case 0xac:
	case 0xad:
	case 0xae:
	case 0xaf:
	case 0xb0:
	case 0xb1:
	case 0xb2:
	case 0xb3:
	case 0xb4:
	case 0xb5:
	case 0xb6:
	case 0xb7:
	case 0xb8:
	case 0xb9:
	case 0xba:
	case 0xbb:
	case 0xbc:
	case 0xbd:
	case 0xbe:
	case 0xbf:
	case 0xc0:
	case 0xc1:
	case 0xc2:
	case 0xc3:
	case 0xc4:
	case 0xc5:
	case 0xc6:
	case 0xc7:
	case 0xc8:
	case 0xc9:
	case 0xca:
	case 0xcb:
	case 0xcc:
	case 0xcd:
	case 0xce:
	case 0xcf:
	case 0xd0:
	case 0xd1:
	case 0xd2:
	case 0xd3:
	case 0xd4:
	case 0xd5:
	case 0xd6:
	case 0xd7:
	case 0xd8:
	case 0xd9:
	case 0xda:
	case 0xdb:
	case 0xdc:
	case 0xdd:
	case 0xde:
	case 0xdf:
	case 0xe0:
	case 0xe1:
	case 0xe2:
	case 0xe3:
	case 0xe4:
	case 0xe5:
	case 0xe6:
	case 0xe7:
	case 0xe8:
	case 0xe9:
	case 0xea:
	case 0xeb:
	case 0xec:
	case 0xed:
	case 0xee:
	case 0xef:
	case 0xf0:
	case 0xf1:
	case 0xf2:
	case 0xf3:
	case 0xf4:
	case 0xf5:
	case 0xf6:
	case 0xf7:
	case 0xf8:
	case 0xf9:
	case 0xfa:
	case 0xfb:
	case 0xfc:
	case 0xfd:
	case 0xfe:
	case 0xff:
	}

	cpu.PC += Address(inst.PrefixedInstructions[opcode].bytes)
}


execute_prefixed_instruction :: proc(opcode: u8) {
	switch opcode {
	case 0x00:
	case 0x01:
	case 0x02:
	case 0x03:
	case 0x04:
	case 0x05:
	case 0x06:
	case 0x07:
	case 0x08:
	case 0x09:
	case 0x0a:
	case 0x0b:
	case 0x0c:
	case 0x0d:
	case 0x0e:
	case 0x0f:
	case 0x10:
	case 0x11:
	case 0x12:
	case 0x13:
	case 0x14:
	case 0x15:
	case 0x16:
	case 0x17:
	case 0x18:
	case 0x19:
	case 0x1a:
	case 0x1b:
	case 0x1c:
	case 0x1d:
	case 0x1e:
	case 0x1f:
	case 0x20:
	case 0x21:
	case 0x22:
	case 0x23:
	case 0x24:
	case 0x25:
	case 0x26:
	case 0x27:
	case 0x28:
	case 0x29:
	case 0x2a:
	case 0x2b:
	case 0x2c:
	case 0x2d:
	case 0x2e:
	case 0x2f:
	case 0x30:
	case 0x31:
	case 0x32:
	case 0x33:
	case 0x34:
	case 0x35:
	case 0x36:
	case 0x37:
	case 0x38:
	case 0x39:
	case 0x3a:
	case 0x3b:
	case 0x3c:
	case 0x3d:
	case 0x3e:
	case 0x3f:
	case 0x40:
	case 0x41:
	case 0x42:
	case 0x43:
	case 0x44:
	case 0x45:
	case 0x46:
	case 0x47:
	case 0x48:
	case 0x49:
	case 0x4a:
	case 0x4b:
	case 0x4c:
	case 0x4d:
	case 0x4e:
	case 0x4f:
	case 0x50:
	case 0x51:
	case 0x52:
	case 0x53:
	case 0x54:
	case 0x55:
	case 0x56:
	case 0x57:
	case 0x58:
	case 0x59:
	case 0x5a:
	case 0x5b:
	case 0x5c:
	case 0x5d:
	case 0x5e:
	case 0x5f:
	case 0x60:
	case 0x61:
	case 0x62:
	case 0x63:
	case 0x64:
	case 0x65:
	case 0x66:
	case 0x67:
	case 0x68:
	case 0x69:
	case 0x6a:
	case 0x6b:
	case 0x6c:
	case 0x6d:
	case 0x6e:
	case 0x6f:
	case 0x70:
	case 0x71:
	case 0x72:
	case 0x73:
	case 0x74:
	case 0x75:
	case 0x76:
	case 0x77:
	case 0x78:
	case 0x79:
	case 0x7a:
	case 0x7b:
	case 0x7c:
	case 0x7d:
	case 0x7e:
	case 0x7f:
	case 0x80:
	case 0x81:
	case 0x82:
	case 0x83:
	case 0x84:
	case 0x85:
	case 0x86:
	case 0x87:
	case 0x88:
	case 0x89:
	case 0x8a:
	case 0x8b:
	case 0x8c:
	case 0x8d:
	case 0x8e:
	case 0x8f:
	case 0x90:
	case 0x91:
	case 0x92:
	case 0x93:
	case 0x94:
	case 0x95:
	case 0x96:
	case 0x97:
	case 0x98:
	case 0x99:
	case 0x9a:
	case 0x9b:
	case 0x9c:
	case 0x9d:
	case 0x9e:
	case 0x9f:
	case 0xa0:
	case 0xa1:
	case 0xa2:
	case 0xa3:
	case 0xa4:
	case 0xa5:
	case 0xa6:
	case 0xa7:
	case 0xa8:
	case 0xa9:
	case 0xaa:
	case 0xab:
	case 0xac:
	case 0xad:
	case 0xae:
	case 0xaf:
	case 0xb0:
	case 0xb1:
	case 0xb2:
	case 0xb3:
	case 0xb4:
	case 0xb5:
	case 0xb6:
	case 0xb7:
	case 0xb8:
	case 0xb9:
	case 0xba:
	case 0xbb:
	case 0xbc:
	case 0xbd:
	case 0xbe:
	case 0xbf:
	case 0xc0:
	case 0xc1:
	case 0xc2:
	case 0xc3:
	case 0xc4:
	case 0xc5:
	case 0xc6:
	case 0xc7:
	case 0xc8:
	case 0xc9:
	case 0xca:
	case 0xcb:
	case 0xcc:
	case 0xcd:
	case 0xce:
	case 0xcf:
	case 0xd0:
	case 0xd1:
	case 0xd2:
	case 0xd3:
	case 0xd4:
	case 0xd5:
	case 0xd6:
	case 0xd7:
	case 0xd8:
	case 0xd9:
	case 0xda:
	case 0xdb:
	case 0xdc:
	case 0xdd:
	case 0xde:
	case 0xdf:
	case 0xe0:
	case 0xe1:
	case 0xe2:
	case 0xe3:
	case 0xe4:
	case 0xe5:
	case 0xe6:
	case 0xe7:
	case 0xe8:
	case 0xe9:
	case 0xea:
	case 0xeb:
	case 0xec:
	case 0xed:
	case 0xee:
	case 0xef:
	case 0xf0:
	case 0xf1:
	case 0xf2:
	case 0xf3:
	case 0xf4:
	case 0xf5:
	case 0xf6:
	case 0xf7:
	case 0xf8:
	case 0xf9:
	case 0xfa:
	case 0xfb:
	case 0xfc:
	case 0xfd:
	case 0xfe:
	case 0xff:
	}
}


jump_rel :: proc($cc: ConditionCode) {
	if check_condition_code(cc) {
		cpu.registers.PC = Address(i16(cpu.PC) + i16(read_u8(cpu.registers.SP)))
	}
}

ld_r8_r8 :: proc($r8_0: R8, $r8_1: R8) {
	get_reg8(r8_0)^ = get_reg8(r8_1)^
}

ld_r8_n8 :: proc($r8: R8) {
	get_reg8(r8)^ = read_u8(cpu.PC)
}

ld_r16_n16 :: proc($r16: R16) {
	get_reg16(r16)^ = read_u16(cpu.PC)
}

ld_HL_r8 :: proc($r8: R8) {
	write_u8(cpu.registers.HL.full, get_reg8(r8))
}

ld_HL_n8 :: proc($r8: R8) {
	write_u8(cpu.registers.HL.full, read_u8(cpu.PC))
}

ld_r8_HL :: proc($r8: R8) {
	get_reg8(r8)^ = read_u8(cpu.registers.HL.full)
}

ld_r16_A :: proc($r16: R16) {
	write_u8(Address(get_reg16(r16)^), cpu.registers.AF.single.A)
}

ld_n16_A :: proc($r16: R16) {
	write_u8(Address(read_u16(cpu.PC)), cpu.registers.AF.single.A)
}


ldh_n8_A :: proc() {
	n := u16(read_u8(cpu.PC))
	cpu.registers.AF.single.A = read_u8(Address(0xFF00 + n))
}

ldh_C_A :: proc() {
	write_u8(Address(0xFF00 + u16(cpu.registers.BC.single.C)), cpu.registers.AF.single.A)
}

ld_A_r16 :: proc($r16: R16) {
	cpu.registers.AF.single.A = read_u8(get_reg16(r16)^)
}

ld_A_n16 :: proc() {
	cpu.registers.AF.single.A = read_u8(Address(read_u16(cpu.PC)))
}

ldh_A_n8 :: proc() {
	n := u16(read_u8(cpu.PC))
	write_u8(Address(0xFF00 + n), cpu.registers.AF.single.A)
}

ldh_A_C :: proc() {
	cpu.registers.AF.single.A = read_u8(Address(0xFF00 + u16(cpu.registers.BC.single.C)))
}

ld_sp_n16 :: proc() {
	cpu.SP = Address(read_u16(cpu.PC))
}

ld_n16_SP :: proc() {
	write_u16(Address(read_u16(cpu.PC)), u16(cpu.SP))
}

ld_HL_SP_e8 :: proc() {
	e := i16(read_u8(cpu.PC))
	res := i16(cpu.SP) + e
	cpu.registers.HL.full = u16(res)

	cpu.registers.AF.single.F -= {.Z, .N}

	toggle_flag(is_half_carried_add(u16(cpu.SP), u16(e)), .H)
	toggle_flag(res > 0xFF, .C)
}

ld_SP_HL :: proc() {
	cpu.SP = Address(cpu.registers.HL.full)
}

inc8 :: proc($r8: R8) {
	target := get_reg8(r8)
	target^ += 1

	toggle_flag(target^ == 0, .Z)
	toggle_flag(is_half_carried_add(target^, 1), .H)
	cpu.registers.AF.single.F -= {.N}
}

dec8 :: proc($r8: R8) {
	target := get_reg8(r8)
	target^ -= 1

	toggle_flag(target^ == 0, .Z)
	toggle_flag(is_half_carried_add(target^, 1), .H)
	cpu.registers.AF.single.F += {.N}
}

inc_hl :: proc() {
	target := read_u8(Address(cpu.registers.HL.full))
	target += 1

	write_u8(Address(cpu.registers.HL.full), target)

	toggle_flag(target == 0, .Z)
	toggle_flag(is_half_carried_add(target, 1), .H)
	cpu.registers.AF.single.F -= {.N}
}

dec_hl :: proc($r8: R8) {
	target := read_u8(Address(cpu.registers.HL.full))
	target^ -= 1

	write_u8(Address(cpu.registers.HL.full), target)

	toggle_flag(target == 0, .Z)
	toggle_flag(is_half_carried_add(target, 1), .H)
	cpu.registers.AF.single.F += {.N}
}

inc16 :: proc($r16: R16) {
	target := get_reg8(r16)
	target^ += 1
}

dec16 :: proc($r16: R16) {
	target := get_reg16(r16)
	target^ -= 1
}

pop_r16 :: proc($r16: R16) {
	get_reg16(r16)^ = read_u16(cpu.SP)
	cpu.SP += 2

}


push_r16 :: proc($r16: R16) {
	write_u16(cpu.SP, get_reg16(r16)^)
	cpu.SP -= 2

}


or_A_r8 :: proc($r8: R8) {
	cpu.registers.AF.single.A |= get_reg8(r8)^

	cpu.registers.AF.single.F -= {.N, .C, .H}
	toggle_flag(cpu.registers.AF.single.A == 0, .Z)
}


or_A_HL :: proc() {
	cpu.registers.AF.single.A |= read_u8(Address(cpu.registers.HL.full))

	cpu.registers.AF.single.F -= {.N, .C, .H}
	toggle_flag(cpu.registers.AF.single.A == 0, .Z)
}


or_A_n8 :: proc() {
	cpu.registers.AF.single.A |= read_u8(cpu.PC)

	cpu.registers.AF.single.F -= {.N, .C, .H}
	toggle_flag(cpu.registers.AF.single.A == 0, .Z)
}

res_r8 :: proc($r8: R8) {
	get_reg8(r8)^ &= ~(1 << get_n8(cpu))
}


res_HL :: proc($r8: R8) {
	read_u8(Address(cpu.registers.HL.full))^ &= ~(1 << get_n8(cpu))
}

ret :: proc($cc: ConditionCode) {
	if check_condition_code(cc) {
		cpu.PC = Address(read_u16(cpu.SP))
		cpu.SP += 2
	}
}

reti :: proc() {
	cpu.PC = Address(read_u16(cpu.SP))
	cpu.SP += 2
	cpu.interrupt.enable = true
}

rl_r8 :: proc($r8: R8) {
	reg := get_reg8(r8)
	rotate_left_includes_carry(reg)

	toggle_flag(reg^ == 0, .Z)
	cpu.registers.AF.single.F -= {.N, .H}
}

rl_HL :: proc($r8: R8) {
	mem := read_u8(Address(cpu.registers.HL.full))
	rotate_left_includes_carry(mem)

	toggle_flag(mem^ == 0, .Z)
	cpu.registers.AF.single.F -= {.N, .H}
}

rla :: proc($r8: R8) {
	rotate_left_includes_carry(cpu.registers.AF.single.A)

	cpu.registers.AF.single.F -= {.N, .H, .Z}
}

rlc_r8 :: proc($r8: R8) {
	reg := get_reg8(r8)
	rotate_left(reg)

	toggle_flag(reg^ >> 7 == 1, .C)
	toggle_flag(reg^ == 0, .Z)
	cpu.registers.AF.single.F -= {.N, .H}
}

rlc_HL :: proc($r8: R8) {
	mem := read_u8(Address(cpu.registers.HL.full))
	rotate_left(mem)

	toggle_flag(reg^ >> 7 == 1, .C)
	toggle_flag(mem^ == 0, .Z)
	cpu.registers.AF.single.F -= {.N, .H}
}

rlca :: proc($r8: R8) {
	rotate_left(cpu.registers.AF.single.A)

	toggle_flag(reg^ >> 7 == 1, .C)
	cpu.registers.AF.single.F -= {.N, .H, .Z}
}


rr_r8 :: proc($r8: R8) {
	reg := get_reg8(r8)
	rotate_right_includes_carry(reg)

	toggle_flag(reg^ == 0, .Z)
	cpu.registers.AF.single.F -= {.N, .H}
}

rr_HL :: proc($r8: R8) {
	mem := read_u8(Address(cpu.registers.HL.full))
	rotate_left_includes_carry(mem)

	toggle_flag(mem^ == 0, .Z)
	cpu.registers.AF.single.F -= {.N, .H}
}

rra :: proc($r8: R8) {
	rotate_right_includes_carry(cpu.registers.AF.single.A)

	cpu.registers.AF.single.F -= {.N, .H, .Z}
}

rrc_r8 :: proc($r8: R8) {
	reg := get_reg8(r8)
	rotate_right(reg)

	toggle_flag(reg^ & 0x1 == 1, .C)
	toggle_flag(reg^ == 0, .Z)
	cpu.registers.AF.single.F -= {.N, .H}
}

rrc_HL :: proc($r8: R8) {
	mem := read_u8(Address(cpu.registers.HL.full))
	rotate_right(mem)

	toggle_flag(reg^ & 0x1 == 1, .C)
	toggle_flag(mem^ == 0, .Z)
	cpu.registers.AF.single.F -= {.N, .H}
}

rrca :: proc($r8: R8) {
	rotate_right(cpu.registers.AF.single.A)

	toggle_flag(reg^ & 0x1 == 1, .C)
	cpu.registers.AF.single.F -= {.N, .H, .Z}
}

