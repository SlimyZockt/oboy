package main

import "base:intrinsics"
import "core:log"
import inst "instructions"

ConditionCode :: enum u8 {
	NONE,
	Z,
	NZ,
	C,
	NC,
}

R8 :: enum u8 {
	NONE,
	A,
	B,
	C,
	D,
	E,
	H,
	L,
}

R16 :: enum u8 {
	NONE,
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


load_boot_rom :: proc(cpu: ^Cpu) {
	cpu.registers.A = 0x01
	cpu.registers.F = {.Z, .H, .C}

	cpu.registers.B = 0x00
	cpu.registers.C = 0x13

	cpu.registers.D = 0x00
	cpu.registers.E = 0xD8


	cpu.registers.H = 0x01
	cpu.registers.L = 0x4D

	cpu.PC = 0x0100
	cpu.SP = 0xFFFE

	cpu.interrupt.master = true
	cpu.interrupt.enable = {}
	cpu.interrupt.flags = {}

	cpu.joypad = {.B_Left, .A_Right, .Select_Up, .Start_Down, .Select_DPad, .Select_Buttons}

	io = io_reset

	for &pixel in framebuffer {
		pixel = Color{255, 255, 255}
	}

	gpu.dots = 0
	gpu.mode = .HBlank

	write_u8(0xFF05, 0)
	write_u8(0xFF06, 0)
	write_u8(0xFF07, 0)
	write_u8(0xFF10, 0x80)
	write_u8(0xFF11, 0xBF)
	write_u8(0xFF12, 0xF3)
	write_u8(0xFF14, 0xBF)
	write_u8(0xFF16, 0x3F)
	write_u8(0xFF17, 0x00)
	write_u8(0xFF19, 0xBF)
	write_u8(0xFF1A, 0x7A)
	write_u8(0xFF1B, 0xFF)
	write_u8(0xFF1C, 0x9F)
	write_u8(0xFF1E, 0xBF)
	write_u8(0xFF20, 0xFF)
	write_u8(0xFF21, 0x00)
	write_u8(0xFF22, 0x00)
	write_u8(0xFF23, 0xBF)
	write_u8(0xFF24, 0x77)
	write_u8(0xFF25, 0xF3)
	write_u8(0xFF26, 0xF1)
	write_u8(0xFF40, 0x91)
	write_u8(0xFF42, 0x00)
	write_u8(0xFF43, 0x00)
	write_u8(0xFF45, 0x00)
	write_u8(0xFF47, 0xFC)
	write_u8(0xFF48, 0xFF)
	write_u8(0xFF49, 0xFF)
	write_u8(0xFF4A, 0x00)
	write_u8(0xFF4B, 0x00)
	write_u8(0xFFFF, 0x00)
}

step_cpu :: proc() {
	opcode := rom[cpu.PC]

	instruction := inst.UnprefixedInstructions[opcode]

	// di
	if cpu.pre_opcode == 0xF3 {
		cpu.interrupt.master = false
	}
	if cpu.pre_opcode == 0xFB {
		cpu.interrupt.master = true
	}


	when ODIN_DEBUG {
		operand: Operand
		switch instruction.bytes - 1 {
		case 1:
			operand = read_u8(cpu.PC + 1)
		case 2:
			operand = read_u16(cpu.PC + 1)
		}


		log.infof("At 0x%04X: [%02X; %04X] %s", cpu.PC, opcode, operand, instruction.name)


		append(
			&debug_data,
			Instruction_Debug_Data {
				instruction.mnemonic == .PREFIX,
				opcode,
				operand,
				cpu.PC,
				instruction.mnemonic,
			},
		)
	}

	#partial switch instruction.mnemonic {
	case .PREFIX:
		cpu.PC += 1
		opcode = rom[cpu.PC]
		instruction = inst.PrefixedInstructions[opcode]
		execute_prefixed_instruction(opcode)
	case:
		execute_instruction(opcode)
	}

	#partial switch instruction.mnemonic {
	case .CALL, .HALT, .JR, .JP, .RET, .RETI, .RST:
		break
	case .ILLEGAL_D3,
	     .ILLEGAL_DD,
	     .ILLEGAL_FD,
	     .ILLEGAL_E3,
	     .ILLEGAL_E4,
	     .ILLEGAL_F4,
	     .ILLEGAL_FC,
	     .ILLEGAL_EC,
	     .ILLEGAL_ED,
	     .ILLEGAL_DB,
	     .ILLEGAL_EB:
		log.panic("Illagel instruction:", opcode)
	case:
		cpu.PC += Address(instruction.bytes)
	}

	cpu.pre_opcode = opcode

	for cycle in instruction.cycles {
		cpu.ticks += u64(cycle)
	}

}


execute_instruction :: proc(opcode: u8) {
	switch opcode {
	case 0x0:
		// NOOP
		break
	case 0x1:
		ld_r16_n16(.BC)
	case 0x2:
		ld_r16_A(.BC)
	case 0x3:
		inc16(.BC)
	case 0x4:
		inc8(.B)
	case 0x5:
		dec8(.B)
	case 0x6:
		ld_r8_n8(.B)
	case 0x7:
		rlca()
	case 0x8:
		ld_n16_SP()
	case 0x9:
		add16_HL(.BC)
	case 0xa:
		ld_A_r16(.BC)
	case 0xb:
		dec16(.BC)
	case 0xc:
		inc8(.C)
	case 0xd:
		dec8(.C)
	case 0xe:
		ld_r8_n8(.C)
	case 0xf:
		rrca()
	case 0x10:
		stop()
	case 0x11:
		ld_r16_n16(.DE)
	case 0x12:
		ld_r16_A(.DE)
	case 0x13:
		inc16(.DE)
	case 0x14:
		inc8(.D)
	case 0x15:
		dec8(.D)
	case 0x16:
		ld_r8_n8(.D)
	case 0x17:
		rla()
	case 0x18:
		jr(.NONE)
	case 0x19:
		add16_HL(.DE)
	case 0x1a:
		ld_A_r16(.DE)
	case 0x1b:
		dec16(.DE)
	case 0x1c:
		inc8(.E)
	case 0x1d:
		dec8(.E)
	case 0x1e:
		ld_r8_n8(.E)
	case 0x1f:
		rra()
	case 0x20:
		jr(.NZ)
	case 0x21:
		ld_r16_n16(.HL)
	case 0x22:
		ld_MHL_A(.HLI)
	case 0x23:
		inc16(.HL)
	case 0x24:
		inc8(.H)
	case 0x25:
		dec8(.H)
	case 0x26:
		ld_r8_n8(.H)
	case 0x27:
		daa()
	case 0x28:
		jr(.Z)
	case 0x29:
		add16_HL(.HL)
	case 0x2a:
		ld_A_MHL(.HLI)
	case 0x2b:
		dec16(.HL)
	case 0x2c:
		inc8(.L)
	case 0x2d:
		dec8(.L)
	case 0x2e:
		ld_r8_n8(.L)
	case 0x2f:
		cpl()
	case 0x30:
		jr(.NC)
	case 0x31:
		ld_sp_n16()
	case 0x32:
		ld_MHL_A(.HLD)
	case 0x33:
		inc16(.SP)
	case 0x34:
		inc_hl()
	case 0x35:
		dec_hl()
	case 0x36:
		ld_HL_n8()
	case 0x37:
		scf()
	case 0x38:
		jr(.C)
	case 0x39:
		add16_HL(.SP)
	case 0x3a:
		ld_A_MHL(.HLD)
	case 0x3b:
		dec16(.SP)
	case 0x3c:
		inc8(.A)
	case 0x3d:
		dec8(.A)
	case 0x3e:
		ld_r8_n8(.A)
	case 0x3f:
		ccf()
	case 0x40:
		ld_r8_r8(.B, .B)
	case 0x41:
		ld_r8_r8(.B, .C)
	case 0x42:
		ld_r8_r8(.B, .D)
	case 0x43:
		ld_r8_r8(.B, .E)
	case 0x44:
		ld_r8_r8(.B, .H)
	case 0x45:
		ld_r8_r8(.B, .L)
	case 0x46:
		ld_r8_HL(.B)
	case 0x47:
		ld_r8_r8(.B, .A)
	case 0x48:
		ld_r8_r8(.C, .B)
	case 0x49:
		ld_r8_r8(.C, .C)
	case 0x4a:
		ld_r8_r8(.C, .D)
	case 0x4b:
		ld_r8_r8(.C, .E)
	case 0x4c:
		ld_r8_r8(.C, .H)
	case 0x4d:
		ld_r8_r8(.C, .L)
	case 0x4e:
		ld_r8_HL(.C)
	case 0x4f:
		ld_r8_r8(.C, .A)
	case 0x50:
		ld_r8_r8(.D, .B)
	case 0x51:
		ld_r8_r8(.D, .C)
	case 0x52:
		ld_r8_r8(.D, .D)
	case 0x53:
		ld_r8_r8(.D, .E)
	case 0x54:
		ld_r8_r8(.D, .H)
	case 0x55:
		ld_r8_r8(.D, .L)
	case 0x56:
		ld_r8_HL(.D)
	case 0x57:
		ld_r8_r8(.D, .A)
	case 0x58:
		ld_r8_r8(.E, .B)
	case 0x59:
		ld_r8_r8(.E, .C)
	case 0x5a:
		ld_r8_r8(.E, .D)
	case 0x5b:
		ld_r8_r8(.E, .E)
	case 0x5c:
		ld_r8_r8(.E, .H)
	case 0x5d:
		ld_r8_r8(.E, .L)
	case 0x5e:
		ld_r8_HL(.E)
	case 0x5f:
		ld_r8_r8(.E, .A)
	case 0x60:
		ld_r8_r8(.H, .B)
	case 0x61:
		ld_r8_r8(.H, .C)
	case 0x62:
		ld_r8_r8(.H, .D)
	case 0x63:
		ld_r8_r8(.H, .E)
	case 0x64:
		ld_r8_r8(.H, .H)
	case 0x65:
		ld_r8_r8(.H, .L)
	case 0x66:
		ld_r8_HL(.H)
	case 0x67:
		ld_r8_r8(.H, .A)
	case 0x68:
		ld_r8_r8(.L, .B)
	case 0x69:
		ld_r8_r8(.L, .C)
	case 0x6a:
		ld_r8_r8(.L, .D)
	case 0x6b:
		ld_r8_r8(.L, .E)
	case 0x6c:
		ld_r8_r8(.L, .H)
	case 0x6d:
		ld_r8_r8(.L, .L)
	case 0x6e:
		ld_r8_HL(.L)
	case 0x6f:
		ld_r8_r8(.L, .A)
	case 0x70:
		ld_HL_r8(.B)
	case 0x71:
		ld_HL_r8(.C)
	case 0x72:
		ld_HL_r8(.D)
	case 0x73:
		ld_HL_r8(.E)
	case 0x74:
		ld_HL_r8(.H)
	case 0x75:
		ld_HL_r8(.L)
	case 0x76:
		halt()
	case 0x77:
		ld_HL_r8(.A)
	case 0x78:
		ld_r8_r8(.A, .B)
	case 0x79:
		ld_r8_r8(.A, .C)
	case 0x7a:
		ld_r8_r8(.A, .D)
	case 0x7b:
		ld_r8_r8(.A, .E)
	case 0x7c:
		ld_r8_r8(.A, .H)
	case 0x7d:
		ld_r8_r8(.A, .L)
	case 0x7e:
		ld_r8_HL(.A)
	case 0x7f:
		ld_r8_r8(.A, .A)
	case 0x80:
		add8_A(.R8, .B)
	case 0x81:
		add8_A(.R8, .C)
	case 0x82:
		add8_A(.R8, .D)
	case 0x83:
		add8_A(.R8, .E)
	case 0x84:
		add8_A(.R8, .H)
	case 0x85:
		add8_A(.R8, .L)
	case 0x86:
		add8_A(.HL, .NONE)
	case 0x87:
		add8_A(.R8, .A)
	case 0x88:
		adc_A(.R8, .B)
	case 0x89:
		adc_A(.R8, .C)
	case 0x8a:
		adc_A(.R8, .D)
	case 0x8b:
		adc_A(.R8, .E)
	case 0x8c:
		adc_A(.R8, .H)
	case 0x8d:
		adc_A(.R8, .L)
	case 0x8e:
		adc_A(.HL, .NONE)
	case 0x8f:
		adc_A(.R8, .A)
	case 0x90:
		sub(.R8, .B)
	case 0x91:
		sub(.R8, .C)
	case 0x92:
		sub(.R8, .D)
	case 0x93:
		sub(.R8, .E)
	case 0x94:
		sub(.R8, .H)
	case 0x95:
		sub(.R8, .L)
	case 0x96:
		sub(.HL, .NONE)
	case 0x97:
		sub(.R8, .A)
	case 0x98:
		sbc_A(.R8, .B)
	case 0x99:
		sbc_A(.R8, .C)
	case 0x9a:
		sbc_A(.R8, .D)
	case 0x9b:
		sbc_A(.R8, .E)
	case 0x9c:
		sbc_A(.R8, .H)
	case 0x9d:
		sbc_A(.R8, .L)
	case 0x9e:
		sbc_A(.HL, .NONE)
	case 0x9f:
		sbc_A(.R8, .A)
	case 0xa0:
		and_A(.R8, .B)
	case 0xa1:
		and_A(.R8, .C)
	case 0xa2:
		and_A(.R8, .D)
	case 0xa3:
		and_A(.R8, .E)
	case 0xa4:
		and_A(.R8, .H)
	case 0xa5:
		and_A(.R8, .L)
	case 0xa6:
		and_A(.HL, .NONE)
	case 0xa7:
		and_A(.R8, .A)
	case 0xa8:
		xor_A(.R8, .B)
	case 0xa9:
		xor_A(.R8, .C)
	case 0xaa:
		xor_A(.R8, .D)
	case 0xab:
		xor_A(.R8, .E)
	case 0xac:
		xor_A(.R8, .H)
	case 0xad:
		xor_A(.R8, .L)
	case 0xae:
		xor_A(.HL, .NONE)
	case 0xaf:
		xor_A(.R8, .A)
	case 0xb0:
		or_A(.R8, .B)
	case 0xb1:
		or_A(.R8, .C)
	case 0xb2:
		or_A(.R8, .D)
	case 0xb3:
		or_A(.R8, .E)
	case 0xb4:
		or_A(.R8, .H)
	case 0xb5:
		or_A(.R8, .L)
	case 0xb6:
		or_A(.HL, .NONE)
	case 0xb7:
		or_A(.R8, .A)
	case 0xb8:
		cp_A(.R8, .B)
	case 0xb9:
		cp_A(.R8, .C)
	case 0xba:
		cp_A(.R8, .D)
	case 0xbb:
		cp_A(.R8, .E)
	case 0xbc:
		cp_A(.R8, .H)
	case 0xbd:
		cp_A(.R8, .L)
	case 0xbe:
		cp_A(.HL, .NONE)
	case 0xbf:
		cp_A(.R8, .A)
	case 0xc0:
		ret(.NZ)
	case 0xc1:
		pop_r16(.BC)
	case 0xc2:
		jp(.NZ, .N16)
	case 0xc3:
		jp(.NONE, .N16)
	case 0xc4:
		call_n16(.NZ)
	case 0xc5:
		push_r16(.BC)
	case 0xc6:
		add8_A(.N8, .NONE)
	case 0xc7:
		rst(.V00)
	case 0xc8:
		ret(.Z)
	case 0xc9:
		ret(.NONE)
	case 0xca:
		jp(.Z, .N16)
	case 0xcb:
		panic("prefixed instruction called")
	case 0xcc:
		call_n16(.Z)
	case 0xcd:
		call_n16(.NONE)
	case 0xce:
		adc_A(.N8, .NONE)
	case 0xcf:
		rst(.V08)
	case 0xd0:
		ret(.NZ)
	case 0xd1:
		pop_r16(.DE)
	case 0xd2:
		jp(.NC, .N16)
	case 0xd3:
	case 0xd4:
		call_n16(.NC)
	case 0xd5:
		push_r16(.DE)
	case 0xd6:
		sub(.N8, .NONE)
	case 0xd7:
		rst(.V10)
	case 0xd8:
		ret(.C)
	case 0xd9:
		reti()
	case 0xda:
		jp(.C, .N16)
	case 0xdb:
	case 0xdc:
		call_n16(.C)
	case 0xdd:
	case 0xde:
		sbc_A(.N8, .NONE)
	case 0xdf:
		rst(.V18)
	case 0xe0:
		ldh_n8_A()
	case 0xe1:
		pop_r16(.HL)
	case 0xe2:
		ldh_C_A()
	case 0xe3:
	case 0xe4:
	case 0xe5:
		push_r16(.HL)
	case 0xe6:
		and_A(.N8, .NONE)
	case 0xe7:
		rst(.V20)
	case 0xe8:
		add16_SP_e8()
	case 0xe9:
		jp(.NONE, .HL)
	case 0xea:
		ld_n16_A()
	case 0xeb:
	case 0xec:
	case 0xed:
	case 0xee:
		xor_A(.N8, .NONE)
	case 0xef:
		rst(.V28)
	case 0xf0:
		ldh_A_n8()
	case 0xf1:
		pop_r16(.AF)
	case 0xf2:
		ldh_A_C()
	case 0xf3:
	case 0xf4:
	case 0xf5:
		push_r16(.AF)
	case 0xf6:
		or_A(.N8, .NONE)
	case 0xf7:
		rst(.V30)
	case 0xf8:
		ld_HL_SP_e8()
	case 0xf9:
		ld_SP_HL()
	case 0xfa:
		ld_SP_HL()
	case 0xfb:
	case 0xfc:
	case 0xfd:
	case 0xfe:
		cp_A(.N8, .NONE)
	case 0xff:
		rst(.V38)
	}
}


execute_prefixed_instruction :: proc(opcode: u8) {
	switch opcode {
	case 0x00:
		rlc_r8(.B)
	case 0x01:
		rlc_r8(.C)
	case 0x02:
		rlc_r8(.D)
	case 0x03:
		rlc_r8(.E)
	case 0x04:
		rlc_r8(.H)
	case 0x05:
		rlc_r8(.L)
	case 0x06:
		rlc_HL()
	case 0x07:
		rlc_r8(.A)
	case 0x08:
		rrc_r8(.B)
	case 0x09:
		rrc_r8(.C)
	case 0x0a:
		rrc_r8(.D)
	case 0x0b:
		rrc_r8(.E)
	case 0x0c:
		rrc_r8(.H)
	case 0x0d:
		rrc_r8(.L)
	case 0x0e:
		rrc_HL()
	case 0x0f:
		rrc_r8(.A)
	case 0x10:
		rl_r8(.B)
	case 0x11:
		rl_r8(.C)
	case 0x12:
		rl_r8(.D)
	case 0x13:
		rl_r8(.E)
	case 0x14:
		rl_r8(.H)
	case 0x15:
		rl_r8(.L)
	case 0x16:
		rl_HL()
	case 0x17:
		rl_r8(.A)
	case 0x18:
		rr_r8(.B)
	case 0x19:
		rr_r8(.C)
	case 0x1a:
		rr_r8(.D)
	case 0x1b:
		rr_r8(.E)
	case 0x1c:
		rr_r8(.H)
	case 0x1d:
		rr_r8(.L)
	case 0x1e:
		rr_HL()
	case 0x1f:
		rr_r8(.A)
	case 0x20:
		sla_r8(.B)
	case 0x21:
		sla_r8(.C)
	case 0x22:
		sla_r8(.D)
	case 0x23:
		sla_r8(.E)
	case 0x24:
		sla_r8(.H)
	case 0x25:
		sla_r8(.L)
	case 0x26:
		sla_HL()
	case 0x27:
		sla_r8(.A)
	case 0x28:
		sra_r8(.B)
	case 0x29:
		sra_r8(.C)
	case 0x2a:
		sra_r8(.D)
	case 0x2b:
		sra_r8(.E)
	case 0x2c:
		sra_r8(.H)
	case 0x2d:
		sra_r8(.L)
	case 0x2e:
		sra_HL()
	case 0x2f:
		sra_r8(.A)
	case 0x30:
		swap_r8(.B)
	case 0x31:
		swap_r8(.C)
	case 0x32:
		swap_r8(.D)
	case 0x33:
		swap_r8(.E)
	case 0x34:
		swap_r8(.H)
	case 0x35:
		swap_r8(.L)
	case 0x36:
		swap_hl()
	case 0x37:
		swap_r8(.A)
	case 0x38:
		srl_r8(.B)
	case 0x39:
		srl_r8(.C)
	case 0x3a:
		srl_r8(.D)
	case 0x3b:
		srl_r8(.E)
	case 0x3c:
		srl_r8(.H)
	case 0x3d:
		srl_r8(.L)
	case 0x3e:
		srl_HL()
	case 0x3f:
		srl_r8(.A)
	case 0x40:
		bit_u3(0, .R8, .B)
	case 0x41:
		bit_u3(0, .R8, .C)
	case 0x42:
		bit_u3(0, .R8, .D)
	case 0x43:
		bit_u3(0, .R8, .E)
	case 0x44:
		bit_u3(0, .R8, .H)
	case 0x45:
		bit_u3(0, .R8, .L)
	case 0x46:
		bit_u3(0, .HL, .NONE)
	case 0x47:
		bit_u3(0, .R8, .A)
	case 0x48:
		bit_u3(1, .R8, .B)
	case 0x49:
		bit_u3(1, .R8, .C)
	case 0x4a:
		bit_u3(1, .R8, .D)
	case 0x4b:
		bit_u3(1, .R8, .E)
	case 0x4c:
		bit_u3(1, .R8, .H)
	case 0x4d:
		bit_u3(1, .R8, .L)
	case 0x4e:
		bit_u3(1, .HL, .NONE)
	case 0x4f:
		bit_u3(1, .R8, .A)
	case 0x50:
		bit_u3(2, .R8, .B)
	case 0x51:
		bit_u3(2, .R8, .C)
	case 0x52:
		bit_u3(2, .R8, .D)
	case 0x53:
		bit_u3(2, .R8, .E)
	case 0x54:
		bit_u3(2, .R8, .H)
	case 0x55:
		bit_u3(2, .R8, .L)
	case 0x56:
		bit_u3(2, .HL, .NONE)
	case 0x57:
		bit_u3(2, .R8, .A)
	case 0x58:
		bit_u3(3, .R8, .B)
	case 0x59:
		bit_u3(3, .R8, .C)
	case 0x5a:
		bit_u3(3, .R8, .D)
	case 0x5b:
		bit_u3(3, .R8, .E)
	case 0x5c:
		bit_u3(3, .R8, .H)
	case 0x5d:
		bit_u3(3, .R8, .L)
	case 0x5e:
		bit_u3(3, .HL, .NONE)
	case 0x5f:
		bit_u3(3, .R8, .A)
	case 0x60:
		bit_u3(4, .R8, .B)
	case 0x61:
		bit_u3(4, .R8, .C)
	case 0x62:
		bit_u3(4, .R8, .D)
	case 0x63:
		bit_u3(4, .R8, .E)
	case 0x64:
		bit_u3(4, .R8, .H)
	case 0x65:
		bit_u3(4, .R8, .L)
	case 0x66:
		bit_u3(4, .HL, .NONE)
	case 0x67:
		bit_u3(4, .R8, .A)
	case 0x68:
		bit_u3(5, .R8, .B)
	case 0x69:
		bit_u3(5, .R8, .C)
	case 0x6a:
		bit_u3(5, .R8, .D)
	case 0x6b:
		bit_u3(5, .R8, .E)
	case 0x6c:
		bit_u3(5, .R8, .H)
	case 0x6d:
		bit_u3(5, .R8, .L)
	case 0x6e:
		bit_u3(5, .HL, .NONE)
	case 0x6f:
		bit_u3(5, .R8, .A)
	case 0x70:
		bit_u3(6, .R8, .B)
	case 0x71:
		bit_u3(6, .R8, .C)
	case 0x72:
		bit_u3(6, .R8, .D)
	case 0x73:
		bit_u3(6, .R8, .E)
	case 0x74:
		bit_u3(6, .R8, .H)
	case 0x75:
		bit_u3(6, .R8, .L)
	case 0x76:
		bit_u3(6, .HL, .NONE)
	case 0x77:
		bit_u3(6, .R8, .A)
	case 0x78:
		bit_u3(7, .R8, .B)
	case 0x79:
		bit_u3(7, .R8, .C)
	case 0x7a:
		bit_u3(7, .R8, .D)
	case 0x7b:
		bit_u3(7, .R8, .E)
	case 0x7c:
		bit_u3(7, .R8, .H)
	case 0x7d:
		bit_u3(7, .R8, .L)
	case 0x7e:
		bit_u3(7, .HL, .NONE)
	case 0x7f:
		bit_u3(7, .R8, .A)
	case 0x80:
		res_r8(0, .B)
	case 0x81:
		res_r8(0, .C)
	case 0x82:
		res_r8(0, .D)
	case 0x83:
		res_r8(0, .E)
	case 0x84:
		res_r8(0, .H)
	case 0x85:
		res_r8(0, .L)
	case 0x86:
		res_HL(0)
	case 0x87:
		res_r8(0, .A)
	case 0x88:
		res_r8(1, .B)
	case 0x89:
		res_r8(1, .C)
	case 0x8a:
		res_r8(1, .D)
	case 0x8b:
		res_r8(1, .E)
	case 0x8c:
		res_r8(1, .H)
	case 0x8d:
		res_r8(1, .L)
	case 0x8e:
		res_HL(1)
	case 0x8f:
		res_r8(1, .A)
	case 0x90:
		res_r8(2, .B)
	case 0x91:
		res_r8(2, .C)
	case 0x92:
		res_r8(2, .D)
	case 0x93:
		res_r8(2, .E)
	case 0x94:
		res_r8(2, .H)
	case 0x95:
		res_r8(2, .L)
	case 0x96:
		res_HL(2)
	case 0x97:
		res_r8(2, .A)
	case 0x98:
		res_r8(3, .B)
	case 0x99:
		res_r8(3, .C)
	case 0x9a:
		res_r8(3, .D)
	case 0x9b:
		res_r8(3, .E)
	case 0x9c:
		res_r8(3, .H)
	case 0x9d:
		res_r8(3, .L)
	case 0x9e:
		res_HL(3)
	case 0x9f:
		res_r8(3, .A)
	case 0xa0:
		res_r8(4, .B)
	case 0xa1:
		res_r8(4, .C)
	case 0xa2:
		res_r8(4, .D)
	case 0xa3:
		res_r8(4, .E)
	case 0xa4:
		res_r8(4, .H)
	case 0xa5:
		res_r8(4, .L)
	case 0xa6:
		res_HL(4)
	case 0xa7:
		res_r8(5, .A)
	case 0xa8:
		res_r8(5, .B)
	case 0xa9:
		res_r8(5, .C)
	case 0xaa:
		res_r8(5, .D)
	case 0xab:
		res_r8(5, .E)
	case 0xac:
		res_r8(5, .H)
	case 0xad:
		res_r8(5, .L)
	case 0xae:
		res_HL(5)
	case 0xaf:
		res_r8(5, .A)
	case 0xb0:
		res_r8(6, .B)
	case 0xb1:
		res_r8(6, .C)
	case 0xb2:
		res_r8(6, .D)
	case 0xb3:
		res_r8(6, .E)
	case 0xb4:
		res_r8(6, .H)
	case 0xb5:
		res_r8(6, .L)
	case 0xb6:
		res_HL(6)
	case 0xb7:
		res_r8(7, .A)
	case 0xb8:
		res_r8(7, .B)
	case 0xb9:
		res_r8(7, .C)
	case 0xba:
		res_r8(7, .D)
	case 0xbb:
		res_r8(7, .E)
	case 0xbc:
		res_r8(7, .H)
	case 0xbd:
		res_r8(7, .L)
	case 0xbe:
		res_HL(7)
	case 0xbf:
		res_r8(7, .A)
	case 0xc0:
		set_r8(0, .B)
	case 0xc1:
		set_r8(0, .C)
	case 0xc2:
		set_r8(0, .D)
	case 0xc3:
		set_r8(0, .E)
	case 0xc4:
		set_r8(0, .H)
	case 0xc5:
		set_r8(0, .L)
	case 0xc6:
		set_hl(0)
	case 0xc7:
		set_r8(0, .A)
	case 0xc8:
		set_r8(1, .B)
	case 0xc9:
		set_r8(1, .C)
	case 0xca:
		set_r8(1, .D)
	case 0xcb:
		set_r8(1, .E)
	case 0xcc:
		set_r8(1, .H)
	case 0xcd:
		set_r8(1, .L)
	case 0xce:
		set_hl(1)
	case 0xcf:
		set_r8(1, .A)
	case 0xd0:
		set_r8(2, .B)
	case 0xd1:
		set_r8(2, .C)
	case 0xd2:
		set_r8(2, .D)
	case 0xd3:
		set_r8(2, .E)
	case 0xd4:
		set_r8(2, .H)
	case 0xd5:
		set_r8(2, .L)
	case 0xd6:
		set_hl(2)
	case 0xd7:
		set_r8(2, .A)
	case 0xd8:
		set_r8(3, .B)
	case 0xd9:
		set_r8(3, .C)
	case 0xda:
		set_r8(3, .D)
	case 0xdb:
		set_r8(3, .E)
	case 0xdc:
		set_r8(3, .H)
	case 0xdd:
		set_r8(3, .L)
	case 0xde:
		set_hl(3)
	case 0xdf:
		set_r8(3, .A)
	case 0xe0:
		set_r8(4, .B)
	case 0xe1:
		set_r8(4, .C)
	case 0xe2:
		set_r8(4, .D)
	case 0xe3:
		set_r8(4, .E)
	case 0xe4:
		set_r8(4, .H)
	case 0xe5:
		set_r8(4, .L)
	case 0xe6:
		set_hl(4)
	case 0xe7:
		set_r8(4, .A)
	case 0xe8:
		set_r8(5, .B)
	case 0xe9:
		set_r8(5, .C)
	case 0xea:
		set_r8(5, .D)
	case 0xeb:
		set_r8(5, .E)
	case 0xec:
		set_r8(5, .H)
	case 0xed:
		set_r8(5, .L)
	case 0xee:
		set_hl(5)
	case 0xef:
		set_r8(5, .A)
	case 0xf0:
		set_r8(6, .B)
	case 0xf1:
		set_r8(6, .C)
	case 0xf2:
		set_r8(6, .D)
	case 0xf3:
		set_r8(6, .E)
	case 0xf4:
		set_r8(6, .H)
	case 0xf5:
		set_r8(6, .L)
	case 0xf6:
		set_hl(6)
	case 0xf7:
		set_r8(6, .A)
	case 0xf8:
		set_r8(7, .B)
	case 0xf9:
		set_r8(7, .C)
	case 0xfa:
		set_r8(7, .D)
	case 0xfb:
		set_r8(7, .E)
	case 0xfc:
		set_r8(7, .H)
	case 0xfd:
		set_r8(7, .L)
	case 0xfe:
		set_hl(7)
	case 0xff:
		set_r8(7, .A)
	}
}

U8_ARG_MODE :: enum u8 {
	R8,
	HL,
	N8,
}


U16_ARG_MODE :: enum u8 {
	R16,
	HL,
	N16,
}

get_argument_u8 :: proc($mode: U8_ARG_MODE, $r8: R8) -> u8 {
	when mode == .HL && r8 == .NONE {
		return read_u8(Address(cpu.registers.HL))
	} else when mode == .N8 && r8 == .NONE {
		return read_u8(cpu.PC + 1)
	} else when mode == .R8 && r8 != .NONE {
		return get_reg8(r8)^
	} else {
		#panic("invalid argument")
	}
}


get_argument_u16 :: proc($mode: U16_ARG_MODE, $r16: R16) -> u16 {
	when mode == .HL && r16 == .NONE {
		return cpu.registers.HL
	} else when mode == .N16 && r16 == .NONE {
		return read_u16(cpu.PC + 1)
	} else when mode == .R16 && r16 != .NONE {
		return get_reg16(r16)^
	} else {
		#panic("invalid argument")
	}
}

adc_A :: proc($mode: U8_ARG_MODE, $r8: R8) {
	mem := get_argument_u8(mode, r8)

	reg_a := &cpu.registers.A
	flags := &cpu.registers.F
	carry := u8(.C in flags)
	res := u16(reg_a^) + u16(mem) + u16(carry)
	reg_a^ = u8(res)

	half_carry := ((mem & 0xf) + (reg_a^ & 0xf) + (carry & 0xf)) & 0x10 == 0x10
	toggle_flag(res == 0, .Z)
	toggle_flag(res > 0xFF, .C)
	toggle_flag(half_carry, .H)
	cpu.registers.F -= {.N}

}

add8_A :: proc($mode: U8_ARG_MODE, $r8: R8) {
	mem := get_argument_u8(mode, r8)

	reg_a := &cpu.registers.A
	res := u16(reg_a^) + u16(mem)
	reg_a^ = u8(res)

	toggle_flag(res == 0, .Z)
	toggle_flag(res > 0xFF, .C)
	toggle_flag(is_half_carried_add(reg_a^, mem), .H)
	cpu.registers.F -= {.N}
}

add16_HL :: proc($r16: R16) {
	reg := get_reg16(r16)^

	reg_hl := &cpu.registers.HL
	res := u32(reg_hl^) + u32(reg)
	reg_hl^ = u16(res)

	toggle_flag(res == 0, .Z)
	toggle_flag(res > 0xFFFF, .C)
	toggle_flag(is_half_carried_add(reg_hl^, reg), .H)
	cpu.registers.F -= {.N}
}

add16_SP_e8 :: proc() {
	e8 := cast(i8)read_u8(cpu.PC + 1)

	sp := &cpu.SP
	res := i32(sp^) + i32(e8)
	sp^ = Address(res)

	toggle_flag(res > 0xFF, .C)
	toggle_flag(is_half_carried_add(u8(sp^), u8(e8)), .H)
	cpu.registers.F -= {.N, .Z}
}


and_A :: proc($mode: U8_ARG_MODE, $r8: R8) {
	cpu.registers.A &= get_argument_u8(mode, r8)

	cpu.registers.F -= {.N, .C}
	cpu.registers.F += {.H}
	toggle_flag(cpu.registers.A == 0, .Z)
}


bit_u3 :: proc($bit: u8, $mode: U8_ARG_MODE, $r8: R8) where (0 <= bit && bit <= 7) {
	mem := get_argument_u8(mode, r8)

	toggle_flag((mem << bit) & 0x1 == 0, .Z)
	cpu.registers.F -= {.N}
	cpu.registers.F += {.H}
}

call_n16 :: proc($cc: ConditionCode) {
	if !is_condition_valid(cc) {
		cpu.PC += 3
		return
	}
	n := read_u16(cpu.PC + 1)

	cpu.SP -= 2
	write_u16(cpu.SP, n)

	cpu.PC = Address(n)
}

ccf :: proc() {
	cpu.registers.F -= {.N, .H}
	cpu.registers.F ~= {.C}
}

cp_A :: proc($mode: U8_ARG_MODE, $r8: R8) {
	mem := get_argument_u8(mode, r8)

	diff := (cpu.registers.A) - (mem)
	toggle_flag(diff == 0, .Z)
	toggle_flag(is_half_carried_sub(cpu.registers.A, mem), .H)
	toggle_flag(mem > cpu.registers.A, .C)
	cpu.registers.F += {.N}
}

cpl :: proc() {
	cpu.registers.A = ~cpu.registers.A
	cpu.registers.F += {.N, .H}
}


daa :: proc() {
	adjustment: u8 = 0
	flags := &cpu.registers.F
	a := &cpu.registers.A

	if .H in flags^ || (.N not_in flags^ && (a^ & 0xf) > 9) {
		adjustment |= 0x6
	}

	if .C in flags^ || (.N not_in flags^ && a^ > 0x99) {
		adjustment |= 0x60
		flags^ += {.C}
	}

	a^ += -adjustment if .N in flags^ else adjustment

	toggle_flag(a^ == 0, .Z)
}

dec8 :: proc($r8: R8) {
	target := get_reg8(r8)
	target^ -= 1

	toggle_flag(target^ == 0, .Z)
	toggle_flag(is_half_carried_add(target^, 1), .H)
	cpu.registers.F += {.N}
}

dec_hl :: proc() {
	target := read_u8(Address(cpu.registers.HL))
	target -= 1

	write_u8(Address(cpu.registers.HL), target)

	toggle_flag(target == 0, .Z)
	toggle_flag(is_half_carried_add(target, 1), .H)
	cpu.registers.F += {.N}
}

dec16 :: proc($r16: R16) {
	target := get_reg16(r16)
	target^ -= 1
}

halt :: proc() {
	if !cpu.interrupt.master {
		cpu.PC += 1
	}
}

inc8 :: proc($r8: R8) {
	target := get_reg8(r8)
	target^ += 1

	toggle_flag(target^ == 0, .Z)
	toggle_flag(is_half_carried_add(target^, 1), .H)
	cpu.registers.F -= {.N}
}

inc_hl :: proc() {
	target := read_u8(Address(cpu.registers.HL))
	target += 1

	write_u8(Address(cpu.registers.HL), target)

	toggle_flag(target == 0, .Z)
	toggle_flag(is_half_carried_add(target, 1), .H)
	cpu.registers.F -= {.N}
}

inc16 :: proc($r16: R16) {
	target := get_reg16(r16)
	target^ += 1
}

jp :: proc($cc: ConditionCode, $mode: enum u8 {
		HL,
		N16,
	}) {
	if !is_condition_valid(cc) {
		when mode == .N16 {
			cpu.PC += 3
		} else {
			cpu.PC += 1
		}
		return
	}

	when mode == .HL {
		cpu.PC = Address(cpu.registers.HL)
	} else when mode == .N16 {
		cpu.PC = Address(read_u16(cpu.PC + 1))
	}
}

jr :: proc($cc: ConditionCode) {
	if !is_condition_valid(cc) {
		log.fatal("cc invalid")
		cpu.PC += 2
		return
	}

	cpu.PC = Address(i16(cpu.PC + 2) + i16(i8(read_u8(cpu.PC + 1))))
}

ld_r8_r8 :: proc($r8_0: R8, $r8_1: R8) {
	get_reg8(r8_0)^ = get_reg8(r8_1)^
}

ld_r8_n8 :: proc($r8: R8) {
	get_reg8(r8)^ = read_u8(cpu.PC + 1)
}

ld_r16_n16 :: proc($r16: R16) {
	get_reg16(r16)^ = read_u16(cpu.PC + 1)
}

ld_HL_r8 :: proc($r8: R8) {
	write_u8(Address(cpu.registers.HL), get_reg8(r8)^)
}

ld_HL_n8 :: proc() {
	write_u8(Address(cpu.registers.HL), read_u8(cpu.PC + 1))
}

ld_r8_HL :: proc($r8: R8) {
	get_reg8(r8)^ = read_u8(Address(cpu.registers.HL))
}

ld_r16_A :: proc($r16: R16) {
	write_u8(Address(get_reg16(r16)^), cpu.registers.A)
}

ld_n16_A :: proc() {
	write_u8(Address(read_u16(cpu.PC + 1)), cpu.registers.A)
}


ldh_n8_A :: proc() {
	n := read_u8(cpu.PC + 1)
	write_u8(Address(0xFF00 | u16(n)), cpu.registers.A)
}

ldh_C_A :: proc() {
	write_u8(Address(0xFF00 | u16(cpu.registers.C)), cpu.registers.A)
}

ld_A_r16 :: proc($r16: R16) {
	cpu.registers.A = read_u8(Address(get_reg16(r16)^))
}

ld_A_n16 :: proc() {
	cpu.registers.A = read_u8(Address(read_u16(cpu.PC + 1)))
}

ldh_A_n8 :: proc() {
	n := read_u8(cpu.PC + 1)
	cpu.registers.A = read_u8(Address(0xFF00 | u16(n)))

}

ldh_A_C :: proc() {
	cpu.registers.A = read_u8(Address(0xFF00 | u16(cpu.registers.C)))
}

ld_sp_n16 :: proc() {
	cpu.SP = Address(read_u16(cpu.PC + 1))
}

ld_n16_SP :: proc() {
	write_u16(Address(read_u16(cpu.PC + 1)), u16(cpu.SP))
}

ld_MHL_A :: proc($mode: enum {
		HLI,
		HLD,
	}) {
	write_u8(Address(cpu.registers.HL), cpu.registers.A)


	when mode == .HLI {
		cpu.registers.HL += 1
	} else {
		cpu.registers.HL -= 1
	}
}


ld_A_MHL :: proc($mode: enum {
		HLI,
		HLD,
	}) {
	cpu.registers.A = read_u8(Address(cpu.registers.HL))
	when mode == .HLI {
		cpu.registers.HL += 1
	} else {
		cpu.registers.HL -= 1
	}
}

ld_HL_SP_e8 :: proc() {
	e := i8(read_u8(cpu.PC + 1))
	res := i16(cpu.SP) + i16(e)
	cpu.registers.HL = u16(res)

	cpu.registers.F -= {.Z, .N}

	toggle_flag(is_half_carried_add(u16(cpu.SP), u16(e)), .H)
	toggle_flag(res > 0xFF, .C)
}

ld_SP_HL :: proc() {
	cpu.SP = Address(cpu.registers.HL)
}

// nop :: proc() {}


or_A :: proc($mode: U8_ARG_MODE, $r8: R8) {
	cpu.registers.A |= get_argument_u8(mode, r8)
	cpu.registers.F -= {.N, .C, .H}
	toggle_flag(cpu.registers.A == 0, .Z)
}

pop_r16 :: proc($r16: R16) {
	get_reg16(r16)^ = read_u16(cpu.SP)
	cpu.SP += 2

}

push_r16 :: proc($r16: R16) {
	cpu.SP -= 2
	write_u16(cpu.SP, get_reg16(r16)^)

}

res_r8 :: proc($bit: u8, $r8: R8) where 0 <= bit && bit <= 7 {
	get_reg8(r8)^ &= ~(u8(1) << bit)
}

res_HL :: proc($bit: u8) where 0 <= bit && bit <= 7 {
	write_u8(Address(cpu.registers.HL), read_u8(Address(cpu.registers.HL)) & ~(u8(1) << bit))
}

ret :: proc($cc: ConditionCode) {
	if !is_condition_valid(cc) {
		cpu.PC += 3
		return
	}
	cpu.PC = Address(read_u16(cpu.SP))
	cpu.SP += 2
}

reti :: proc() {
	log.fatal("RETI")
	cpu.interrupt.master = true
	cpu.PC = Address(read_u16(cpu.SP))
	cpu.SP += 2
}

rl_r8 :: proc($r8: R8) {
	reg := get_reg8(r8)
	rotate_left_includes_carry(reg)

	toggle_flag(reg^ == 0, .Z)
	cpu.registers.F -= {.N, .H}
}

rl_HL :: proc() {
	mem := read_u8(Address(cpu.registers.HL))
	rotate_left_includes_carry(&mem)
	write_u8(Address(cpu.registers.HL), mem)

	toggle_flag(mem == 0, .Z)
	cpu.registers.F -= {.N, .H}
}

rla :: proc() {
	rotate_left_includes_carry(&cpu.registers.A)

	cpu.registers.F -= {.N, .H, .Z}
}

rlc_r8 :: proc($r8: R8) {
	reg := get_reg8(r8)
	rotate_left(reg)

	toggle_flag(reg^ >> 7 == 1, .C)
	toggle_flag(reg^ == 0, .Z)
	cpu.registers.F -= {.N, .H}
}

rlc_HL :: proc() {
	mem := read_u8(Address(cpu.registers.HL))
	rotate_left(&mem)
	write_u8(Address(cpu.registers.HL), mem)

	toggle_flag(mem >> 7 == 1, .C)
	toggle_flag(mem == 0, .Z)
	cpu.registers.F -= {.N, .H}
}

rlca :: proc() {
	rotate_left(&cpu.registers.A)

	toggle_flag(cpu.registers.A >> 7 == 1, .C)
	cpu.registers.F -= {.N, .H, .Z}
}


rr_r8 :: proc($r8: R8) {
	reg := get_reg8(r8)
	rotate_right_includes_carry(reg)

	toggle_flag(reg^ == 0, .Z)
	cpu.registers.F -= {.N, .H}
}

rr_HL :: proc() {
	mem := read_u8(Address(cpu.registers.HL))
	rotate_left_includes_carry(&mem)

	write_u8(Address(cpu.registers.HL), mem)

	toggle_flag(mem == 0, .Z)
	cpu.registers.F -= {.N, .H}
}

rra :: proc() {
	rotate_right_includes_carry(&cpu.registers.A)

	cpu.registers.F -= {.N, .H, .Z}
}

rrc_r8 :: proc($r8: R8) {
	reg := get_reg8(r8)
	rotate_right(reg)

	toggle_flag(reg^ & 0x1 == 1, .C)
	toggle_flag(reg^ == 0, .Z)
	cpu.registers.F -= {.N, .H}
}

rrc_HL :: proc() {
	mem := read_u8(Address(cpu.registers.HL))
	rotate_right(&mem)

	write_u8(Address(cpu.registers.HL), mem)

	toggle_flag(mem & 0x1 == 1, .C)
	toggle_flag(mem == 0, .Z)
	cpu.registers.F -= {.N, .H}
}

rrca :: proc() {
	rotate_right(&cpu.registers.A)

	toggle_flag(cpu.registers.A & 0x1 == 1, .C)
	cpu.registers.F -= {.N, .H, .Z}
}

rst :: proc($vec: VEC) {
	cpu.SP -= 2
	write_u16(cpu.SP, u16(cpu.PC))

	cpu.PC = Address(vec)
}

sbc_A :: proc($mode: U8_ARG_MODE, $r8: R8) {
	mem := get_argument_u8(mode, r8)
	c := u8(.C in cpu.registers.F)
	res := i8(cpu.registers.A - mem - c)

	half_carry := ((cpu.registers.A & 0xf) - (mem & 0xf) - (c & 0xf)) & 0x10 == 0x10

	cpu.registers.F += {.N}
	toggle_flag(res == 0, .Z)
	toggle_flag(half_carry, .H)
	toggle_flag(res < 0, .C)

	cpu.registers.A = u8(res)
}

scf :: proc() {
	cpu.registers.F += {.C}
	cpu.registers.F -= {.H, .N}
}

set_r8 :: proc($bit: u8, $r8: R8) where 0 <= bit && bit <= 7 {
	get_reg8(r8)^ |= (u8(1) << bit)
}

set_hl :: proc($bit: u8) where 0 <= bit && bit <= 7 {
	write_u8(Address(cpu.registers.HL), read_u8(Address(cpu.registers.HL)) | (u8(1) << bit))
}

sla_r8 :: proc($r8: R8) {
	reg := get_reg8(r8)

	toggle_flag(reg^ & 0x80 == 1, .C)
	reg^ <<= 1

	cpu.registers.F -= {.N, .H}
	toggle_flag(reg^ == 0, .Z)
}

sla_HL :: proc() {
	mem := read_u8(Address(cpu.registers.HL))

	write_u8(Address(cpu.registers.HL), mem << 1)

	cpu.registers.F -= {.N, .H}
	toggle_flag(mem == 0, .Z)
	toggle_flag(mem & 0x80 == 1, .C)
}

sra_r8 :: proc($r8: R8) {
	reg := get_reg8(r8)

	toggle_flag(reg^ & 0x80 == 1, .C)
	reg^ = (reg^ & 0x80) | (reg^ >> 1)

	cpu.registers.F -= {.N, .H}
	toggle_flag(reg^ == 0, .Z)
}

sra_HL :: proc() {
	mem := read_u8(Address(cpu.registers.HL))

	write_u8(Address(cpu.registers.HL), (mem & 0x80) | (mem >> 1))

	cpu.registers.F -= {.N, .H}
	toggle_flag(mem == 0, .Z)
	toggle_flag(mem & 0x80 == 1, .C)
}


srl_r8 :: proc($r8: R8) {
	reg := get_reg8(r8)

	toggle_flag(reg^ & 0x80 == 1, .C)
	reg^ = (reg^ >> 1)

	cpu.registers.F -= {.N, .H}
	toggle_flag(reg^ == 0, .Z)
}

srl_HL :: proc() {
	mem := read_u8(Address(cpu.registers.HL))

	write_u8(Address(cpu.registers.HL), (mem >> 1))

	cpu.registers.F -= {.N, .H}
	toggle_flag(mem & 0x80 == 1, .C)
	toggle_flag(mem == 0, .Z)
}

stop :: proc() {
	//TODO: implement stop
	panic("stop not implemented yet")

}

sub :: proc($mode: U8_ARG_MODE, $r8: R8) {
	mem := get_argument_u8(mode, r8)
	cpu.registers.A -= mem

	toggle_flag(cpu.registers.A == 0, .Z)
	toggle_flag(is_half_carried_sub(cpu.registers.A, mem), .H)
	toggle_flag(mem > cpu.registers.A, .C)
	cpu.registers.F += {.N}
}


swap_r8 :: proc($r8: R8) {
	reg := get_reg8(r8)

	lower := reg^ & 0xF
	upper := (reg^ >> 0x4) & 0xF
	reg^ = (lower << 0x4) + upper

	toggle_flag(reg^ == 0, .Z)
	cpu.registers.F -= {.N, .H, .C}
}


swap_hl :: proc() {
	mem := read_u8(Address(cpu.registers.HL))

	lower := mem & 0xF
	upper := (mem >> 0x4) & 0xF
	write_u8(Address(cpu.registers.HL), (lower << 0x4) + upper)
	toggle_flag(mem == 0, .Z)
	cpu.registers.F -= {.N, .H, .C}
}


xor_A :: proc($mode: U8_ARG_MODE, $r8: R8) {
	mem := get_argument_u8(mode, r8)

	cpu.registers.A ~= mem
	toggle_flag(mem == 0, .Z)
	cpu.registers.F -= {.N, .H, .C}
}

