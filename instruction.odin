package main

import "base:intrinsics"
import "core:encoding/json"
import "core:fmt"
import "core:log"
import "core:math/bits"
import "core:os"
import "core:slice"
import "core:strconv"
import inst "instructions"

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
		return &cpu.registers.SP
	case:
		assert(false, "reg16 not defind")
		return
	}
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
	case:
		assert(true, "reg8 not defind")
		return
	}
}

is_reg8 :: proc(name: inst.Operand_Name) -> bool {
	return name in bit_set[inst.Operand_Name]{.O_A, .O_B, .O_C, .O_D, .O_E, .O_H, .O_L}
}

is_reg16 :: proc(name: inst.Operand_Name) -> bool {
	return name in bit_set[inst.Operand_Name]{.O_AF, .O_BC, .O_DE, .O_HL}
}

get_n16 :: proc(cpu: ^Cpu, offset: u16 = 1) -> u16 {
	low := cpu.memory[cpu.registers.PC + offset]
	high := cpu.memory[cpu.registers.PC + offset + 1]

	return u16(low) | (u16(high) << 8)

}

get_n8 :: proc(cpu: ^Cpu, offset: u16 = 0) -> u8 {
	return cpu.memory[cpu.registers.PC + offset]
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

	assert(false, "Condition error")
	return false
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

adc :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]

	value: u8

	if is_reg8(operand.name) {
		value = get_reg8(cpu, operand.name)^
	} else if operand.name == .O_HL {
		value = cpu.memory[cpu.registers.HL.full]
	} else if operand.name == .O_n8 {
		value = get_n8(cpu)
	}

	carry := u8(.C in cpu.registers.AF.single.lower)
	a := &cpu.registers.AF.single.upper

	full_res := u32(value) + u32(a^) + u32(carry)
	res := u8(full_res)
	half_carry := ((value & 0xf) + (a^ & 0xf) + (carry & 0xf)) & 0x10 == 0x10

	a^ = res

	flags := &cpu.registers.AF.single.lower
	flags^ = flags^ + {.Z} if res == 0 else flags^ - {.Z}
	flags^ -= {.N}
	flags^ = flags^ + {.H} if half_carry else flags^ - {.H}
	flags^ = flags^ + {.C} if full_res > 0xFF else flags^ - {.C}
}

add :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) <= 2, "Wrong amount of operands")
	if len(instruction.operands) == 1 {
		add8(cpu, instruction)
	} else {
		add16(cpu, instruction)
	}
}


add16 :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 2, "Wrong amount of operands")
	first_operand := instruction.operands[0]
	second_operand := instruction.operands[0]

	full_res: i32
	res: u16
	half_carry := false

	if first_operand.name == .O_HL && is_reg16(second_operand.name) {
		r16 := get_reg16(cpu, second_operand.name)
		hl := cpu.registers.HL.full

		full_res = i32(transmute(i16)hl) + i32(transmute(i16)r16^)
		res = u16(full_res)
		half_carry = is_half_carried_add(hl, r16^)

	} else if first_operand.name == .O_SP && second_operand.name == .O_e8 {
		e8: i8 = transmute(i8)(cpu.memory[cpu.registers.PC + 1])
		full_res = i32(cpu.registers.SP) + i32(transmute(i8)(e8))
		res = u16(full_res)
		half_carry = is_half_carried_add(cpu.registers.SP, u16(e8))
	} else {
		assert(true, "add16: This shoud nerver happen")
	}

	flags := &cpu.registers.AF.single.lower
	flags^ = flags^ + {.Z} if res == 0 else flags^ - {.Z}
	flags^ -= {.N}
	flags^ = flags^ + {.H} if half_carry else flags^ - {.H}
	flags^ = flags^ + {.C} if u32(full_res) > 0xFFFF else flags^ - {.C}
}

add8 :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]

	value: u8

	if is_reg8(operand.name) {
		value = get_reg8(cpu, operand.name)^
	} else if operand.name == .O_HL {
		value = cpu.memory[cpu.registers.HL.full]
	} else if operand.name == .O_n8 {
		value = get_n8(cpu)
	}

	a := &cpu.registers.AF.single.upper
	full_res := u16(value) + u16(a^)
	res := u8(full_res)
	a^ = res

	flags := &cpu.registers.AF.single.lower
	flags^ = flags^ + {.Z} if res == 0 else flags^ - {.Z}
	flags^ -= {.N}
	flags^ = flags^ + {.H} if is_half_carried_add(value, a^) else flags^ - {.H}
	flags^ = flags^ + {.C} if full_res > 0xFF else flags^ - {.C}

}

and :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]

	value: u8

	if is_reg8(operand.name) {
		value = get_reg8(cpu, operand.name)^
	} else if operand.name == .O_HL {
		value = cpu.memory[cpu.registers.HL.full]
	} else if operand.name == .O_n8 {
		value = get_n8(cpu)
	}

	a := &cpu.registers.AF.single.upper
	a^ = a^ & value

	flags := &cpu.registers.AF.single.lower
	flags^ = flags^ + {.Z} if a^ == 0 else flags^ - {.Z}
	flags^ -= {.N, .C}
	flags^ += {.H}

}

bit :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 2, "Wrong amount of operands")
	first_operand := instruction.operands[0]
	second_operand := instruction.operands[1]

	value := cpu.memory[cpu.registers.HL.full]

	if is_reg8(first_operand.name) {
		value = get_reg8(cpu, first_operand.name)^
	} else if first_operand.name == .O_HL {
		value = cpu.memory[cpu.registers.HL.full]
	}

	is_set := (value << get_n8(cpu)) & 0x1

	flags := &cpu.registers.AF.single.lower

	flags^ = flags^ + {.Z} if is_set == 0 else flags^ - {.Z}

	flags^ -= {.N}
	flags^ += {.H}
}

call :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	operand_count := len(instruction.operands)
	assert(operand_count <= 2, "Wrong amount of operands")
	first_operand := instruction.operands[0]

	if operand_count == 2 &&
	   !check_condition(&cpu.registers.AF.single.lower, get_condition(first_operand.name)) {
		return
	}

	n := get_n16(cpu)
	msb, lsb := split_u16(n)

	cpu.registers.SP -= 1
	cpu.memory[cpu.registers.SP] = msb
	cpu.registers.SP -= 1
	cpu.memory[cpu.registers.SP] = lsb

	cpu.registers.PC = n
}

ccf :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 0, "Wrong amount of operands")

	flags := &cpu.registers.AF.single.lower
	flags^ -= {.N, .H}
	flags^ = flags^ - {.C} if .C in flags^ else flags^ + {.C}
}

cp :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 2, "Wrong amount of operands")
	operand := instruction.operands[0]

	value: u8

	if is_reg8(operand.name) {
		value = get_reg8(cpu, operand.name)^
	} else if operand.name == .O_HL {
		value = cpu.memory[cpu.registers.HL.full]
	} else if operand.name == .O_n8 {
		value = get_n8(cpu)
	}

	a := cpu.registers.AF.single.upper

	full_res := i16(transmute(i8)a) - i16(transmute(i8)value)
	res := u8(full_res)
	flags := cpu.registers.AF.single.lower
	flags = flags + {.Z} if res == 0 else flags - {.Z}
	flags -= {.N}
	flags = flags + {.H} if is_half_carried_sub(a, value) else flags - {.H}
	flags = flags + {.C} if full_res < 0 else flags - {.C}
	cpu.registers.AF.single.lower = flags

}

cpl :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 0, "Wrong amount of operands")

	cpu.registers.AF.single.upper = 0xFF - cpu.registers.AF.single.upper

	flags := &cpu.registers.AF.single.lower
	flags^ += {.N, .H}

}

daa :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 0, "Wrong amount of operands")
	adjustment: u8 = 0
	flags := &cpu.registers.AF.single.lower
	a := &cpu.registers.AF.single.upper

	if .H in flags^ || (.N not_in flags^ && (a^ & 0xf) > 9) {
		adjustment |= 0x6
	}

	if .C in flags^ || (.N not_in flags^ && a^ > 0x99) {
		adjustment |= 0x60
		flags^ += {.C}
	}

	a^ += -adjustment if .N in flags^ else adjustment

	flags^ = flags^ + {.Z} if a^ == 0 else flags^ - {.Z}
}

dec :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]

	if is_reg16(operand.name) {
		get_reg16(cpu, operand.name)^ -= 1
	} else {
		dec_8(cpu, instruction)
	}
}

dec_8 :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]
	value: ^u8

	if is_reg8(operand.name) {
		value = get_reg8(cpu, operand.name)
	} else if operand.name == .O_HL {
		value = &cpu.memory[cpu.registers.HL.full]
	}

	full_res := u16(value^) - 1
	res := u8(full_res)
	value^ = res

	flags := cpu.registers.AF.single.lower
	flags += {.N}
	flags = flags + {.Z} if res == 0 else flags - {.Z}
	flags = flags + {.H} if is_half_carried_sub(value^, 1) else flags - {.H}
	cpu.registers.AF.single.lower = flags
}

halt :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	//TODO: maybe implement HALT
}


inc :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]

	if is_reg16(operand.name) {
		get_reg16(cpu, operand.name)^ += 1
	} else {
		inc_8(cpu, instruction)
	}

}

inc_8 :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	operand := instruction.operands[0]
	value: ^u8

	if is_reg8(operand.name) {
		value = get_reg8(cpu, operand.name)
	} else if operand.name == .O_HL {
		value = &cpu.memory[cpu.registers.HL.full]
	}

	full_res := u16(value^) + 1
	res := u8(full_res)
	value^ = res

	flags := &cpu.registers.AF.single.lower
	flags^ -= {.N}
	flags^ = flags^ + {.Z} if res == 0 else flags^ - {.Z}
	flags^ = flags^ + {.H} if is_half_carried_add(value^, 1) else flags^ - {.H}
}

jp :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	operand_count := len(instruction.operands)
	assert(operand_count <= 2, "Wrong amount of operands")

	first_operand := instruction.operands[0]
	flags := &cpu.registers.AF.single.lower
	value: u16

	if operand_count == 2 && !check_condition(flags, get_condition(first_operand.name)) {
		return
	}

	#partial switch first_operand.name {
	case .O_a16:
		value = get_n16(cpu)
	case .O_HL:
		value = cpu.registers.HL.full
	case:
		value = get_n16(cpu)
	}

	cpu.registers.PC = value
}

jr :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	operand_count := len(instruction.operands)
	assert(operand_count <= 2, "Wrong amount of operands")

	first_operand := instruction.operands[0]
	flags := &cpu.registers.AF.single.lower
	value := get_n8(cpu)

	if operand_count == 2 && !check_condition(flags, get_condition(first_operand.name)) {
		cpu.registers.PC += 1
		return
	}

	v := cast(i8)value
	log.debug(v)
	log.debugf("%08b", v)
	log.debugf("%X", cpu.registers.PC)
	cpu.registers.PC = u16(i32(cpu.registers.PC) + i32(v))
	log.debugf("%X", cpu.registers.PC)
}

ld :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) <= 3, "Wrong amount of operands")

	if len(instruction.operands) == 3 {
		e8 := cast(i8)cpu.memory[cpu.registers.PC + 1]

		sp := cpu.registers.SP
		full_res := i32(cast(i16)sp) + i32(e8)
		res := u16(full_res)

		cpu.registers.HL.full = res

		flags := cpu.registers.AF.single.lower
		flags -= {.Z, .N}
		flags = flags + {.H} if is_half_carried_add(sp, u16(e8)) else flags - {.H}
		flags = flags + {.C} if res > 0xFF else flags - {.C}
		cpu.registers.AF.single.lower = flags

		return
	}

	assert(len(instruction.operands) == 2, "Wrong amount of operands")

	first_operand := instruction.operands[0]
	second_operand := instruction.operands[1]

	if is_reg8(first_operand.name) && is_reg8(second_operand.name) {
		get_reg8(cpu, first_operand.name)^ = get_reg8(cpu, second_operand.name)^

	} else if is_reg8(first_operand.name) && second_operand.name == .O_n8 {
		get_reg8(cpu, first_operand.name)^ = get_n8(cpu)

	} else if is_reg8(first_operand.name) && second_operand.name == .O_HL {
		get_reg8(cpu, first_operand.name)^ = cpu.memory[cpu.registers.HL.full]

		switch second_operand.Modifier {
		case .None:
		case .Decrement:
			assert(first_operand.name == .O_A)
			cpu.registers.HL.full -= 1
		case .Increment:
			assert(first_operand.name == .O_A)
			cpu.registers.HL.full += 1
		}

	} else if first_operand.name == .O_HL && is_reg8(second_operand.name) {
		hl_modifier := 0

		cpu.memory[cpu.registers.HL.full] = get_reg8(cpu, second_operand.name)^

		switch first_operand.Modifier {
		case .None:
		case .Decrement:
			assert(second_operand.name == .O_A)
			cpu.registers.HL.full -= 1
		case .Increment:
			assert(second_operand.name == .O_A)
			cpu.registers.HL.full += 1
		}


	} else if first_operand.name == .O_HL && second_operand.name == .O_n8 {
		cpu.memory[cpu.registers.HL.full] = get_n8(cpu)

	} else if first_operand.name == .O_A && second_operand.name == .O_BC {
		cpu.registers.AF.single.upper = cpu.memory[cpu.registers.BC.full]

	} else if first_operand.name == .O_A && second_operand.name == .O_DE {
		cpu.registers.AF.single.upper = cpu.memory[cpu.registers.DE.full]

	} else if first_operand.name == .O_BC && second_operand.name == .O_A {
		cpu.memory[cpu.registers.BC.full] = cpu.registers.AF.single.upper

	} else if first_operand.name == .O_DE && second_operand.name == .O_A {
		cpu.memory[cpu.registers.DE.full] = cpu.registers.AF.single.upper

	} else if first_operand.name == .O_A && second_operand.name == .O_n16 {
		cpu.registers.AF.single.upper = cpu.memory[get_n16(cpu)]

	} else if first_operand.name == .O_n16 && second_operand.name == .O_A {
		cpu.memory[get_n16(cpu)] = cpu.registers.AF.single.upper

	} else if is_reg16(first_operand.name) && second_operand.name == .O_n16 {
		n16 := get_n16(cpu)
		get_reg16(cpu, first_operand.name)^ = n16

	} else if first_operand.name == .O_n16 && second_operand.name == .O_SP {
		n16 := get_n16(cpu)

		msb, lsb := split_u16(cpu.registers.SP)

		cpu.memory[n16] = lsb
		cpu.memory[n16 + 1] = msb

	} else if first_operand.name == .O_SP && second_operand.name == .O_HL {
		cpu.registers.SP = cpu.registers.HL.full
	}

}

ldh :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 2, "Wrong amount of operands")
	first_operand := instruction.operands[0]
	second_operand := instruction.operands[1]

	if first_operand.name == .O_A && second_operand.name == .O_C {
		cpu.registers.AF.single.upper = cpu.memory[0xFF00 + u16(cpu.registers.BC.single.lower)]

	} else if first_operand.name == .O_C && second_operand.name == .O_A {
		cpu.memory[0xFF00 + u16(cpu.registers.BC.single.lower)] = cpu.registers.AF.single.upper

	} else if first_operand.name == .O_A && second_operand.name == .O_n8 {
		cpu.registers.AF.single.upper = cpu.memory[0xFF00 + u16(get_n8(cpu))]

	} else if first_operand.name == .O_n8 && second_operand.name == .O_A {
		cpu.memory[0xFF00 + u16(get_n8(cpu))] = cpu.registers.AF.single.upper

	}
}

or :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]

	value: u8

	if is_reg8(operand.name) {
		value = get_reg8(cpu, operand.name)^
	} else if operand.name == .O_HL {
		value = cpu.memory[cpu.registers.HL.full]
	} else if operand.name == .O_n8 {
		value = get_n8(cpu)
	}

	a := &cpu.registers.AF.single.upper
	a^ = a^ | value

	flags := &cpu.registers.AF.single.lower
	flags^ = flags^ + {.Z} if a^ == 0 else flags^ - {.Z}
	flags^ -= {.N, .C, .H}

}

pop :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]
	assert(is_reg16(operand.name), "Worng operand type")
	lsb := cpu.memory[cpu.registers.SP]
	cpu.registers.SP += 1
	msb := cpu.memory[cpu.registers.SP]
	cpu.registers.SP += 1

	get_reg16(cpu, operand.name)^ = (u16(msb) << 8) + u16(lsb)
}

push :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]
	assert(is_reg16(operand.name), "Worng operand type")
	cpu.registers.SP -= 1

	msb, lsb := split_u16(get_reg16(cpu, operand.name)^)

	cpu.memory[cpu.registers.SP] = msb
	cpu.registers.SP -= 1
	cpu.memory[cpu.registers.SP] = lsb

}

res :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 2, "Wrong amount of operands")
	first_operand := instruction.operands[0]
	second_operand := instruction.operands[1]

	value: ^u8

	if is_reg8(first_operand.name) {
		value = get_reg8(cpu, first_operand.name)
	} else if first_operand.name == .O_HL {
		value = &cpu.memory[cpu.registers.HL.full]
	}

	value^ = value^ & ~(1 << get_n8(cpu))
	flags := &cpu.registers.AF.single.lower

	flags^ = flags^ + {.Z} if value^ == 0 else flags^ - {.Z}

	flags^ -= {.N}
	flags^ += {.H}

}

ret :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	operand_count := len(instruction.operands)
	assert(operand_count <= 1, "Wrong amount of operands")

	if operand_count == 1 &&
	   !check_condition(
			   &cpu.registers.AF.single.lower,
			   get_condition(instruction.operands[0].name),
		   ) {
		return
	}

	lsb := cpu.memory[cpu.registers.SP]
	cpu.registers.SP += 1
	msb := cpu.memory[cpu.registers.SP]
	cpu.registers.SP += 1

	n := (u16(msb) << 8) + u16(lsb)

	cpu.registers.PC = n
}


rl :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")

	target: ^u8
	flags := &cpu.registers.AF.single.lower

	operand := instruction.operands[0]

	if is_reg8(operand.name) {
		target = get_reg8(cpu, operand.name)
	} else if operand.name == .O_HL {
		target = &cpu.memory[cpu.registers.HL.full]
	}

	rotate_left_includes_carry(target, flags)

	if target^ == 0 {
		flags^ += {.Z}
	}

	flags^ -= {.N, .H}
}


rla :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 0, "Wrong amount of operands")

	target := &cpu.registers.AF.single.upper
	flags := &cpu.registers.AF.single.lower

	rotate_left_includes_carry(target, flags)

	flags^ -= {.N, .Z, .H}
}


rlc :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")

	target: ^u8
	flags := &cpu.registers.AF.single.lower

	operand := instruction.operands[0]

	if is_reg8(operand.name) {
		target = get_reg8(cpu, operand.name)
	} else if operand.name == .O_HL {
		target = &cpu.memory[cpu.registers.HL.full]
	}

	msb := rotate_left(target)

	flags^ = flags^ + {.C} if msb == 1 else flags^ - {.C}

	flags^ -= {.N, .Z, .H}
}

rlca :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 0, "Wrong amount of operands")

	target := &cpu.registers.AF.single.upper
	flags := &cpu.registers.AF.single.lower

	msb := rotate_left(target)

	flags^ = flags^ + {.C} if msb == 1 else flags^ - {.C}

	flags^ -= {.N, .Z, .H}
}

rr :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")

	target: ^u8
	flags := &cpu.registers.AF.single.lower

	operand := instruction.operands[0]

	if is_reg8(operand.name) {
		target = get_reg8(cpu, operand.name)
	} else if operand.name == .O_HL {
		target = &cpu.memory[cpu.registers.HL.full]
	}

	rotate_right_includes_carry(target, flags)

	if target^ == 0 {
		flags^ += {.Z}
	}

	flags^ -= {.N, .H}
}

rra :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 0, "Wrong amount of operands")

	target := &cpu.registers.AF.single.upper
	flags := &cpu.registers.AF.single.lower

	rotate_right_includes_carry(target, flags)

	flags^ -= {.N, .Z, .H}
}

rrc :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")

	target: ^u8
	flags := &cpu.registers.AF.single.lower

	operand := instruction.operands[0]

	if is_reg8(operand.name) {
		target = get_reg8(cpu, operand.name)
	} else if operand.name == .O_HL {
		target = &cpu.memory[cpu.registers.HL.full]
	}

	msb := rotate_right(target)

	flags^ = flags^ + {.C} if msb == 1 else flags^ - {.C}

	flags^ -= {.N, .Z, .H}
}

rrca :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 0, "Wrong amount of operands")

	target := &cpu.registers.AF.single.upper
	flags := &cpu.registers.AF.single.lower

	msb := rotate_right(target)

	flags^ = flags^ + {.C} if msb == 1 else flags^ - {.C}

	flags^ -= {.N, .Z, .H}
}

rst :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	msb, lsb := split_u16(cpu.registers.PC)
	cpu.registers.SP -= 1
	cpu.memory[cpu.registers.SP] = msb
	cpu.registers.SP -= 1
	cpu.memory[cpu.registers.SP] = lsb

	addr := 0
	#partial switch instruction.operands[0].name {
	case .O_00:
		addr = 0x00
	case .O_08:
		addr = 0x08
	case .O_10:
		addr = 0x10
	case .O_18:
		addr = 0x18
	case .O_20:
		addr = 0x20
	case .O_28:
		addr = 0x28
	case .O_30:
		addr = 0x30
	case .O_38:
		addr = 0x38
	}
	cpu.registers.PC = Address(addr)
}

sbc :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]

	value: u8

	if is_reg8(operand.name) {
		value = get_reg8(cpu, operand.name)^
	} else if operand.name == .O_HL {
		value = cpu.memory[cpu.registers.HL.full]
	} else if operand.name == .O_n8 {
		value = get_n8(cpu)
	}

	a := &cpu.registers.AF.single.upper

	carry := u8(.C in cpu.registers.AF.single.lower)
	full_res := i32(a^) - i32(value) - i32(carry)
	res := u8(full_res)
	half_carry := ((value & 0xf) - (a^ & 0xf) - (carry & 0xf)) & 0x10 == 0x10

	cpu.registers.AF.single.upper = res

	flags := cpu.registers.AF.single.lower
	flags = flags + {.Z} if res == 0 else flags - {.Z}
	flags -= {.N}
	flags = flags + {.H} if half_carry else flags - {.H}
	flags = flags + {.C} if full_res < 0x0 else flags - {.C}
	cpu.registers.AF.single.lower = flags
}

scf :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 0, "Wrong amount of operands")

	flags := &cpu.registers.AF.single.lower
	flags^ -= {.N, .H}
	flags^ += {.C}
}

set :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {

	assert(len(instruction.operands) == 2, "Wrong amount of operands")
	first_operand := instruction.operands[0]
	second_operand := instruction.operands[1]

	value: ^u8

	if is_reg8(first_operand.name) {
		value = get_reg8(cpu, first_operand.name)
	} else if first_operand.name == .O_HL {
		value = &cpu.memory[cpu.registers.HL.full]
	}

	value^ = value^ | (1 << get_n8(cpu))
	flags := &cpu.registers.AF.single.lower

	flags^ = flags^ + {.Z} if value^ == 0 else flags^ - {.Z}

	flags^ -= {.N}
	flags^ += {.H}

}

sla :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")

	target: ^u8
	flags := &cpu.registers.AF.single.lower

	operand := instruction.operands[0]

	if is_reg8(operand.name) {
		target = get_reg8(cpu, operand.name)
	} else if operand.name == .O_HL {
		target = &cpu.memory[cpu.registers.HL.full]
	}

	msb := target^ >> 7
	target^ = target^ << 1

	if target^ == 0 {
		flags^ += {.Z}
	}

	flags^ = flags^ + {.C} if msb == 1 else flags^ - {.C}
	flags^ -= {.N, .H}

}

sra :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {

	assert(len(instruction.operands) == 1, "Wrong amount of operands")

	target: ^u8
	flags := &cpu.registers.AF.single.lower

	operand := instruction.operands[0]

	if is_reg8(operand.name) {
		target = get_reg8(cpu, operand.name)
	} else if operand.name == .O_HL {
		target = &cpu.memory[cpu.registers.HL.full]
	}

	lsb := target^ & 1
	msb := target^ >> 7
	target^ = target^ >> 1

	if msb == 1 {
		target^ += 0x80
	}

	if target^ == 0 {
		flags^ += {.Z}
	}

	flags^ = flags^ + {.C} if lsb == 1 else flags^ - {.C}
	flags^ -= {.N, .H}

}

srl :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")

	target: ^u8
	flags := &cpu.registers.AF.single.lower

	operand := instruction.operands[0]

	if is_reg8(operand.name) {
		target = get_reg8(cpu, operand.name)
	} else if operand.name == .O_HL {
		target = &cpu.memory[cpu.registers.HL.full]
	}

	lsb := target^ & 1
	target^ = target^ >> 1

	if target^ == 0 {
		flags^ += {.Z}
	}

	flags^ = flags^ + {.C} if lsb == 1 else flags^ - {.C}
	flags^ -= {.N, .H}

}

stop :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	//TODO: maybe implement STOP
}

sub :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 2, "Wrong amount of operands")
	operand := instruction.operands[0]

	value: u8

	if is_reg8(operand.name) {
		value = get_reg8(cpu, operand.name)^
	} else if operand.name == .O_HL {
		value = cpu.memory[cpu.registers.HL.full]
	} else if operand.name == .O_n8 {
		value = get_n8(cpu)
	}

	a := &cpu.registers.AF.single.upper
	res, is_overflown := intrinsics.overflow_sub(a^, value)
	a^ = res

	flags := &cpu.registers.AF.single.lower
	flags^ = flags^ + {.Z} if res == 0 else flags^ - {.Z}
	flags^ -= {.N}
	flags^ = flags^ + {.H} if is_half_carried_sub(a^, value) else flags^ - {.H}
	flags^ = flags^ + {.C} if is_overflown else flags^ - {.C}
}

swap :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]

	target: ^u8

	if is_reg8(operand.name) {
		target = get_reg8(cpu, operand.name)
	} else if operand.name == .O_HL {
		target = &cpu.memory[cpu.registers.HL.full]
	}

	lower := target^ & 0xF
	upper := (target^ >> 0x4) & 0xF

	target^ = (lower << 0x4) + upper
	flags := &cpu.registers.AF.single.lower
	flags^ = flags^ + {.Z} if target^ == 0 else flags^ - {.Z}
	flags^ -= {.N, .H, .C}
}

xor :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
	assert(len(instruction.operands) == 2, "Wrong amount of operands")
	operand := instruction.operands[0]

	value: u8

	if is_reg8(operand.name) {
		value = get_reg8(cpu, operand.name)^
	} else if operand.name == .O_HL {
		value = cpu.memory[cpu.registers.HL.full]
	} else if operand.name == .O_n8 {
		value = get_n8(cpu)
	}

	a := &cpu.registers.AF.single.upper
	a^ = a^ ~ value

	flags := &cpu.registers.AF.single.lower
	flags^ = flags^ + {.Z} if a^ == 0 else flags^ - {.Z}
	flags^ -= {.N, .C, .H}

}

