package main

import "base:intrinsics"
import "core:encoding/json"
import "core:fmt"
import "core:log"
import "core:math/bits"
import "core:os"
import "core:slice"
import "core:strconv"

Address :: u16
Word :: u8

Memory_Map :: enum u16 {
	Rom          = 0x3FFF,
	Switch_Rom   = 0x7FFF,
	Vram         = 0x9FFF,
	External_Ram = 0xBFFF,
	WRam         = 0xDFFF,
	Echo_Ram     = 0xFDFF,
	OAM          = 0xFE9F,
	Forbidden    = 0xFEFF,
	Io           = 0xFF7F,
	Hram         = 0xFFFE,
	Interupt     = 0xFFFF,
}


main :: proc() {
	context.logger = log.create_console_logger(.Debug)

	data, ok := os.read_entire_file_from_filename("./opcodes.json")
	if !ok {
		log.error("Failed to load json file")
		return
	}

	defer delete(data)
	j_data, err := json.parse(data)
	if err != .None {
		log.error("Failed to parse the json file.")
		log.error(err)
		return
	}
	defer json.destroy_value(j_data)

	instruction := new(Instruction)
	root := j_data.(json.Object)

	unprefixed_instructions, unprefixed_err := generate_instruction(
		root["unprefixed"].(json.Object),
	)
	if unprefixed_err != .None {
		log.error(err)
		return
	}

	prefixed_instructions, prefixed_err := generate_instruction(root["cbprefixed"].(json.Object))
	if prefixed_err != .None {
		log.error(err)
		return
	}

	if len(os.args) != 2 {
		log.error(os.args)
		log.error("Not enough args")
		return
	}

	rom_path := os.args[1]
	rom, ok2 := os.read_entire_file_from_filename(rom_path)
	if !ok2 {
		log.error("Can not read file")
		log.error(err)
		return
	}

	rom_size := len(rom)

	cpu := Cpu{}

	for i in 0 ..< u16(Memory_Map.Rom) {
		cpu.memory[i] = rom[i]
	}

	assert(slice.equal(rom[:Memory_Map.Rom], cpu.memory[:Memory_Map.Rom]))

	for false {
		opcode := cpu.memory[cpu.registers.PC]
		instruction := unprefixed_instructions[opcode]

		if instruction.mnemonic == .PREFIX {
			cpu.registers.PC += 1
			opcode = cpu.memory[cpu.registers.PC]
			instruction = prefixed_instructions[opcode]
		}


		execute_instruction(&cpu, instruction)
	}
}

carry_per_bit_add :: proc(first, second: u8) -> (sum: u32, carry: bit_set[0 ..= 32]) {
	return
}

execute_instruction :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	switch instruction.mnemonic {
	case .ADC:
		adc(cpu, instruction)
	case .ADD:
		add(cpu, instruction)
	case .AND:
		and(cpu, instruction)
	case .BIT:
		bit(cpu, instruction)
	case .CALL:
		call(cpu, instruction)
	case .CCF:
		ccf(cpu, instruction)
	case .CP:
		cp(cpu, instruction)
	case .CPL:
		cpl(cpu, instruction)
	case .DAA:
		daa(cpu, instruction)
	case .DEC:
		dec(cpu, instruction)
	case .DI:
		di(cpu, instruction)
	case .EI:
		ei(cpu, instruction)
	case .HALT:
		halt(cpu, instruction)
	case .ILLEGAL:
		#assert(true, "ilLegal instruction")
	case .INC:
		inc(cpu, instruction)
	case .JP:
		jp(cpu, instruction)
	case .JR:
		jr(cpu, instruction)
	case .LD:
		ld(cpu, instruction)
	case .LDH:
		ldh(cpu, instruction)
	case .NOP:
		nop(cpu, instruction)
	case .OR:
		or(cpu, instruction)
	case .POP:
		pop(cpu, instruction)
	case .PREFIX:
		#assert(true, "prefix instruction")
	case .PUSH:
		push(cpu, instruction)
	case .RES:
		res(cpu, instruction)
	case .RET:
		ret(cpu, instruction)
	case .RETI:
		reti(cpu, instruction)
	case .RL:
		rl(cpu, instruction)
	case .RLA:
		rla(cpu, instruction)
	case .RLC:
		rlc(cpu, instruction)
	case .RLCA:
		rlca(cpu, instruction)
	case .RR:
		rr(cpu, instruction)
	case .RRA:
		rra(cpu, instruction)
	case .RRC:
		rrc(cpu, instruction)
	case .RRCA:
		rrca(cpu, instruction)
	case .RST:
		rst(cpu, instruction)
	case .SBC:
		sbc(cpu, instruction)
	case .SCF:
		scf(cpu, instruction)
	case .SET:
		set(cpu, instruction)
	case .SLA:
		sla(cpu, instruction)
	case .SRA:
		sra(cpu, instruction)
	case .SRL:
		srl(cpu, instruction)
	case .STOP:
		stop(cpu, instruction)
	case .SUB:
		sub(cpu, instruction)
	case .SWAP:
		swap(cpu, instruction)
	case .XOR:
		xor(cpu, instruction)
	case:
		log.error("Not implemented yet")
	}

}

get_reg16 :: proc(cpu: ^Cpu, name: ^string) -> (reg: ^u16) {
	switch name^ {
	case "AF":
		return &cpu.registers.AF.full
	case "BC":
		return &cpu.registers.BC.full
	case "DE":
		return &cpu.registers.DE.full
	case "HL":
		return &cpu.registers.HL.full
	case:
		assert(false, "reg16 not defind")
		return
	}
}


get_reg8 :: proc(cpu: ^Cpu, name: ^string) -> (reg: ^u8) {
	switch name^ {
	case "A":
		return &cpu.registers.AF.single.upper
	// case "F":
	// 	return &cpu.registers.AF.single.lower
	case "B":
		return &cpu.registers.BC.single.upper
	case "C":
		return &cpu.registers.BC.single.lower
	case "D":
		return &cpu.registers.DE.single.upper
	case "E":
		return &cpu.registers.DE.single.lower
	case "H":
		return &cpu.registers.HL.single.upper
	case "L":
		return &cpu.registers.HL.single.lower
	case:
		assert(true, "reg8 not defind")
		return
	}
}

is_reg8 :: proc(name: ^string) -> bool {
	return slice.contains(Register8_Names, name^)
}

is_reg16 :: proc(name: ^string) -> bool {
	return slice.contains(Register16_Names, name^)
}

get_n16 :: proc(cpu: ^Cpu, offset: u16 = 0) -> u16 {
	low := cpu.memory[cpu.registers.PC + offset + 1]
	high := cpu.memory[cpu.registers.PC + offset + 2]

	return (u16(high) << 8) + u16(low)
}

get_n8 :: proc(cpu: ^Cpu, offset: u16 = 0) -> u8 {
	return cpu.memory[cpu.registers.PC + offset + 1]
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

get_condition :: proc(condtion: ^string) -> Condition {
	switch condtion^ {
	case "C":
		return .C
	case "NC":
		return .NC
	case "Z":
		return .Z
	case "NZ":
		return .NZ
	}


	assert(false, "Condition error")
	return nil
}

adc :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]

	value: u8

	if is_reg8(&operand.name) {
		value = get_reg8(cpu, &operand.name)^
	} else if operand.name == "HL" {
		value = cpu.memory[cpu.registers.HL.full]
	} else if operand.name == "n8" {
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

add :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) <= 2, "Wrong amount of operands")
	if len(instruction.operands) == 1 {
		add8(cpu, instruction)
	} else {
		add16(cpu, instruction)
	}
}


add16 :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 2, "Wrong amount of operands")
	first_operand := instruction.operands[0]
	second_operand := instruction.operands[0]

	full_res: i32
	res: u16
	half_carry := false

	if first_operand.name == "HL" && is_reg16(&second_operand.name) {
		r16 := get_reg16(cpu, &second_operand.name)
		hl := cpu.registers.HL.full

		full_res = i32(transmute(i16)hl) + i32(transmute(i16)r16^)
		res = u16(full_res)
		half_carry = is_half_carried_add(hl, r16^)

	} else if first_operand.name == "SP" && second_operand.name == "e8" {
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

add8 :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]

	value: u8

	if is_reg8(&operand.name) {
		value = get_reg8(cpu, &operand.name)^
	} else if operand.name == "HL" {
		value = cpu.memory[cpu.registers.HL.full]
	} else if operand.name == "n8" {
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

and :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]

	value: u8

	if is_reg8(&operand.name) {
		value = get_reg8(cpu, &operand.name)^
	} else if operand.name == "HL" {
		value = cpu.memory[cpu.registers.HL.full]
	} else if operand.name == "n8" {
		value = get_n8(cpu)
	}

	a := &cpu.registers.AF.single.upper
	a^ = a^ & value

	flags := &cpu.registers.AF.single.lower
	flags^ = flags^ + {.Z} if a^ == 0 else flags^ - {.Z}
	flags^ -= {.N, .C}
	flags^ += {.H}

}

bit :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 2, "Wrong amount of operands")
	first_operand := instruction.operands[0]
	second_operand := instruction.operands[1]

	value := cpu.memory[cpu.registers.HL.full]

	if is_reg8(&first_operand.name) {
		value = get_reg8(cpu, &first_operand.name)^
	} else if first_operand.name == "HL" {
		value = cpu.memory[cpu.registers.HL.full]
	}

	is_set := (value << get_n8(cpu)) & 0x1

	flags := &cpu.registers.AF.single.lower

	flags^ = flags^ + {.Z} if is_set == 0 else flags^ - {.Z}

	flags^ -= {.N}
	flags^ += {.H}
}

call :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}

ccf :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 0, "Wrong amount of operands")

	flags := &cpu.registers.AF.single.lower
	flags^ -= {.N, .H}
	flags^ = flags^ - {.C} if .C in flags^ else flags^ + {.C}
}

cp :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]

	value: u8

	if is_reg8(&operand.name) {
		value = get_reg8(cpu, &operand.name)^
	} else if operand.name == "HL" {
		value = cpu.memory[cpu.registers.HL.full]
	} else if operand.name == "n8" {
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

cpl :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 0, "Wrong amount of operands")

	cpu.registers.AF.single.upper = 0xFF - cpu.registers.AF.single.upper

	flags := &cpu.registers.AF.single.lower
	flags^ += {.N, .H}

}

daa :: proc(cpu: ^Cpu, instruction: ^Instruction) {
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

dec :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]

	if is_reg16(&operand.name) {
		get_reg16(cpu, &operand.name)^ -= 1
	} else {
		dec_8(cpu, instruction)
	}
}

dec_8 :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]
	value: ^u8

	if is_reg8(&operand.name) {
		value = get_reg8(cpu, &operand.name)
	} else if operand.name == "HL" {
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

di :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}

ei :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}

halt :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}


inc :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]

	if is_reg16(&operand.name) {
		get_reg16(cpu, &operand.name)^ += 1
	} else {
		inc_8(cpu, instruction)
	}

}

inc_8 :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	operand := instruction.operands[0]
	value: ^u8

	if is_reg8(&operand.name) {
		value = get_reg8(cpu, &operand.name)
	} else if operand.name == "HL" {
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

jp :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) <= 2, "Wrong amount of operands")

	if len(instruction.operands) == 2 {
		jump_condional(cpu, instruction)
	} else {

		jump(cpu, instruction)
	}

}

jump :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	first_operand := instruction.operands[0]
	value: u16

	if first_operand.name == "a16" {
		value = get_n16(cpu)
	} else if first_operand.name == "HL" {
		value = cpu.registers.HL.full
	}

	cpu.registers.PC = value
}


jump_condional :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 2, "Wrong amount of operands")
	first_operand := instruction.operands[0]

	value := get_n16(cpu)
	flags := &cpu.registers.AF.single.lower

	if check_condition(flags, get_condition(&first_operand.name)) {
		cpu.registers.PC = value
	}
}

jump_relative :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	first_operand := instruction.operands[0]
	value: u16

	if first_operand.name == "e8" {
		value = get_n16(cpu)
	} else if first_operand.name == "HL" {
		value = cpu.registers.HL.full
	}

	cpu.registers.PC = u16(i16(cpu.registers.PC) - transmute(i16)value)
}


jump_relative_condional :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 2, "Wrong amount of operands")
	first_operand := instruction.operands[0]

	value := get_n16(cpu)
	flags := &cpu.registers.AF.single.lower

	if check_condition(flags, get_condition(&first_operand.name)) {
		cpu.registers.PC = value
	}
}

jr :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) <= 2, "Wrong amount of operands")

	if len(instruction.operands) == 2 {
		jump_relative_condional(cpu, instruction)
	} else {
		jump_relative(cpu, instruction)
	}

}

ld :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) <= 3, "Wrong amount of operands")

	if len(instruction.operands) == 3 {
		e8 := transmute(i8)cpu.memory[cpu.registers.PC + 1]

		sp := cpu.registers.SP
		full_res := i32(transmute(i16)sp) + i32(e8)
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

	if is_reg8(&first_operand.name) && is_reg8(&second_operand.name) {
		get_reg8(cpu, &first_operand.name)^ = get_reg8(cpu, &second_operand.name)^

	} else if is_reg8(&first_operand.name) && second_operand.name == "n8" {
		get_reg8(cpu, &first_operand.name)^ = get_n8(cpu)

	} else if is_reg8(&first_operand.name) && second_operand.name == "HL" {
		get_reg8(cpu, &first_operand.name)^ = cpu.memory[cpu.registers.HL.full]

		switch second_operand.Modifier {
		case .None:
		case .Decrement:
			assert(first_operand.name == "A")
			cpu.registers.HL.full -= 1
		case .Increment:
			assert(first_operand.name == "A")
			cpu.registers.HL.full += 1
		}

	} else if first_operand.name == "HL" && is_reg8(&second_operand.name) {
		hl_modifier := 0

		cpu.memory[cpu.registers.HL.full] = get_reg8(cpu, &second_operand.name)^

		switch first_operand.Modifier {
		case .None:
		case .Decrement:
			assert(second_operand.name == "A")
			cpu.registers.HL.full -= 1
		case .Increment:
			assert(second_operand.name == "A")
			cpu.registers.HL.full += 1
		}


	} else if first_operand.name == "HL" && second_operand.name == "n8" {
		cpu.memory[cpu.registers.HL.full] = get_n8(cpu)

	} else if first_operand.name == "A" && second_operand.name == "BC" {
		cpu.registers.AF.single.upper = cpu.memory[cpu.registers.BC.full]

	} else if first_operand.name == "A" && second_operand.name == "DE" {
		cpu.registers.AF.single.upper = cpu.memory[cpu.registers.DE.full]

	} else if first_operand.name == "BC" && second_operand.name == "A" {
		cpu.memory[cpu.registers.BC.full] = cpu.registers.AF.single.upper

	} else if first_operand.name == "DE" && second_operand.name == "A" {
		cpu.memory[cpu.registers.DE.full] = cpu.registers.AF.single.upper

	} else if first_operand.name == "A" && second_operand.name == "n16" {
		cpu.registers.AF.single.upper = cpu.memory[get_n16(cpu)]

	} else if first_operand.name == "n16" && second_operand.name == "A" {
		cpu.memory[get_n16(cpu)] = cpu.registers.AF.single.upper

	} else if is_reg16(&first_operand.name) && second_operand.name == "n16" {
		n16 := get_n16(cpu)

		get_reg16(cpu, &first_operand.name)^ = n16

	} else if first_operand.name == "n16" && second_operand.name == "SP" {
		n16 := get_n16(cpu)

		msb, lsb := split_u16(cpu.registers.SP)

		cpu.memory[n16] = lsb
		cpu.memory[n16 + 1] = msb

	} else if first_operand.name == "SP" && second_operand.name == "HL" {
		cpu.registers.SP = cpu.registers.HL.full
	}

}

ldh :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 2, "Wrong amount of operands")
	first_operand := instruction.operands[0]
	second_operand := instruction.operands[1]

	if first_operand.name == "A" && second_operand.name == "C" {
		cpu.registers.AF.single.upper = cpu.memory[0xFF00 + u16(cpu.registers.BC.single.lower)]

	} else if first_operand.name == "C" && second_operand.name == "A" {
		cpu.memory[0xFF00 + u16(cpu.registers.BC.single.lower)] = cpu.registers.AF.single.upper

	} else if first_operand.name == "A" && second_operand.name == "n8" {
		cpu.registers.AF.single.upper = cpu.memory[0xFF00 + u16(get_n8(cpu))]

	} else if first_operand.name == "n8" && second_operand.name == "A" {
		cpu.memory[0xFF00 + u16(get_n8(cpu))] = cpu.registers.AF.single.upper

	}
}
nop :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}

or :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]

	value: u8

	if is_reg8(&operand.name) {
		value = get_reg8(cpu, &operand.name)^
	} else if operand.name == "HL" {
		value = cpu.memory[cpu.registers.HL.full]
	} else if operand.name == "n8" {
		value = get_n8(cpu)
	}

	a := &cpu.registers.AF.single.upper
	a^ = a^ | value

	flags := &cpu.registers.AF.single.lower
	flags^ = flags^ + {.Z} if a^ == 0 else flags^ - {.Z}
	flags^ -= {.N, .C, .H}

}

pop :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]
	assert(is_reg16(&operand.name), "Worng operand type")
	lsb := cpu.memory[cpu.registers.SP]
	cpu.registers.SP += 1
	msb := cpu.memory[cpu.registers.SP]
	cpu.registers.SP += 1

	get_reg16(cpu, &operand.name)^ = (u16(msb) << 8) + u16(lsb)
}

push :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]
	assert(is_reg16(&operand.name), "Worng operand type")
	cpu.registers.SP -= 1

	msb, lsb := split_u16(get_reg16(cpu, &operand.name)^)

	cpu.memory[cpu.registers.SP] = msb
	cpu.registers.SP -= 1
	cpu.memory[cpu.registers.SP] = lsb

}

res :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 2, "Wrong amount of operands")
	first_operand := instruction.operands[0]
	second_operand := instruction.operands[1]

	value: ^u8

	if is_reg8(&first_operand.name) {
		value = get_reg8(cpu, &first_operand.name)
	} else if first_operand.name == "HL" {
		value = &cpu.memory[cpu.registers.HL.full]
	}

	value^ = value^ & ~(1 << get_n8(cpu))
	flags := &cpu.registers.AF.single.lower

	flags^ = flags^ + {.Z} if value^ == 0 else flags^ - {.Z}

	flags^ -= {.N}
	flags^ += {.H}

}

ret :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
reti :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
rl :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")

	target: ^u8
	flags := &cpu.registers.AF.single.lower

	operand := instruction.operands[0]

	if is_reg8(&operand.name) {
		target = get_reg8(cpu, &operand.name)
	} else if operand.name == "HL" {
		target = &cpu.memory[cpu.registers.HL.full]
	}

	rotate_left_includes_carry(target, flags)

	if target^ == 0 {
		flags^ += {.Z}
	}


	flags^ -= {.N, .H}
}


rla :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 0, "Wrong amount of operands")

	target := &cpu.registers.AF.single.upper
	flags := &cpu.registers.AF.single.lower

	rotate_left_includes_carry(target, flags)

	flags^ -= {.N, .Z, .H}
}


rlc :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")

	target: ^u8
	flags := &cpu.registers.AF.single.lower

	operand := instruction.operands[0]

	if is_reg8(&operand.name) {
		target = get_reg8(cpu, &operand.name)
	} else if operand.name == "HL" {
		target = &cpu.memory[cpu.registers.HL.full]
	}

	msb := rotate_left(target)

	flags^ = flags^ + {.C} if msb == 1 else flags^ - {.C}

	flags^ -= {.N, .Z, .H}
}

rlca :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 0, "Wrong amount of operands")

	target := &cpu.registers.AF.single.upper
	flags := &cpu.registers.AF.single.lower

	msb := rotate_left(target)

	flags^ = flags^ + {.C} if msb == 1 else flags^ - {.C}

	flags^ -= {.N, .Z, .H}
}

rr :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")

	target: ^u8
	flags := &cpu.registers.AF.single.lower

	operand := instruction.operands[0]

	if is_reg8(&operand.name) {
		target = get_reg8(cpu, &operand.name)
	} else if operand.name == "HL" {
		target = &cpu.memory[cpu.registers.HL.full]
	}

	rotate_right_includes_carry(target, flags)

	if target^ == 0 {
		flags^ += {.Z}
	}


	flags^ -= {.N, .H}
}
rra :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 0, "Wrong amount of operands")

	target := &cpu.registers.AF.single.upper
	flags := &cpu.registers.AF.single.lower

	rotate_right_includes_carry(target, flags)

	flags^ -= {.N, .Z, .H}
}

rrc :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")

	target: ^u8
	flags := &cpu.registers.AF.single.lower

	operand := instruction.operands[0]

	if is_reg8(&operand.name) {
		target = get_reg8(cpu, &operand.name)
	} else if operand.name == "HL" {
		target = &cpu.memory[cpu.registers.HL.full]
	}

	msb := rotate_right(target)

	flags^ = flags^ + {.C} if msb == 1 else flags^ - {.C}

	flags^ -= {.N, .Z, .H}
}

rrca :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 0, "Wrong amount of operands")

	target := &cpu.registers.AF.single.upper
	flags := &cpu.registers.AF.single.lower

	msb := rotate_right(target)

	flags^ = flags^ + {.C} if msb == 1 else flags^ - {.C}

	flags^ -= {.N, .Z, .H}
}

rst :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}

sbc :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]

	value: u8

	if is_reg8(&operand.name) {
		value = get_reg8(cpu, &operand.name)^
	} else if operand.name == "HL" {
		value = cpu.memory[cpu.registers.HL.full]
	} else if operand.name == "n8" {
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

scf :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 0, "Wrong amount of operands")

	flags := &cpu.registers.AF.single.lower
	flags^ -= {.N, .H}
	flags^ += {.C}
}

set :: proc(cpu: ^Cpu, instruction: ^Instruction) {

	assert(len(instruction.operands) == 2, "Wrong amount of operands")
	first_operand := instruction.operands[0]
	second_operand := instruction.operands[1]

	value: ^u8

	if is_reg8(&first_operand.name) {
		value = get_reg8(cpu, &first_operand.name)
	} else if first_operand.name == "HL" {
		value = &cpu.memory[cpu.registers.HL.full]
	}

	value^ = value^ | (1 << get_n8(cpu))
	flags := &cpu.registers.AF.single.lower

	flags^ = flags^ + {.Z} if value^ == 0 else flags^ - {.Z}

	flags^ -= {.N}
	flags^ += {.H}

}

sla :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")

	target: ^u8
	flags := &cpu.registers.AF.single.lower

	operand := instruction.operands[0]

	if is_reg8(&operand.name) {
		target = get_reg8(cpu, &operand.name)
	} else if operand.name == "HL" {
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

sra :: proc(cpu: ^Cpu, instruction: ^Instruction) {

	assert(len(instruction.operands) == 1, "Wrong amount of operands")

	target: ^u8
	flags := &cpu.registers.AF.single.lower

	operand := instruction.operands[0]

	if is_reg8(&operand.name) {
		target = get_reg8(cpu, &operand.name)
	} else if operand.name == "HL" {
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

srl :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")

	target: ^u8
	flags := &cpu.registers.AF.single.lower

	operand := instruction.operands[0]

	if is_reg8(&operand.name) {
		target = get_reg8(cpu, &operand.name)
	} else if operand.name == "HL" {
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

stop :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}

sub :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]

	value: u8

	if is_reg8(&operand.name) {
		value = get_reg8(cpu, &operand.name)^
	} else if operand.name == "HL" {
		value = cpu.memory[cpu.registers.HL.full]
	} else if operand.name == "n8" {
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

swap :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]

	target: ^u8

	if is_reg8(&operand.name) {
		target = get_reg8(cpu, &operand.name)
	} else if operand.name == "HL" {
		target = &cpu.memory[cpu.registers.HL.full]
	}

	lower := target^ & 0xF
	upper := (target^ >> 0x4) & 0xF

	target^ = (lower << 0x4) + upper
	flags := &cpu.registers.AF.single.lower
	flags^ = flags^ + {.Z} if target^ == 0 else flags^ - {.Z}
	flags^ -= {.N, .H, .C}
}

xor :: proc(cpu: ^Cpu, instruction: ^Instruction) {
	assert(len(instruction.operands) == 1, "Wrong amount of operands")
	operand := instruction.operands[0]

	value: u8

	if is_reg8(&operand.name) {
		value = get_reg8(cpu, &operand.name)^
	} else if operand.name == "HL" {
		value = cpu.memory[cpu.registers.HL.full]
	} else if operand.name == "n8" {
		value = get_n8(cpu)
	}

	a := &cpu.registers.AF.single.upper
	a^ = a^ ~ value

	flags := &cpu.registers.AF.single.lower
	flags^ = flags^ + {.Z} if a^ == 0 else flags^ - {.Z}
	flags^ -= {.N, .C, .H}

}


Instruction_Flags :: enum {
	None,
	Zero,
	One,
	C,
	H,
	N,
	Z,
}


Register :: struct #raw_union {
	full:   u16,
	single: struct {
		lower, upper: u8,
	},
}

Cpu :: struct {
	memory:    [0xFFFF]Word,
	registers: struct {
		AF: struct #raw_union {
			full:   u16,
			single: struct {
				lower: bit_set[Flags;u8],
				upper: u8,
			},
		},
		BC: Register,
		DE: Register,
		HL: Register,
		PC: Address,
		SP: Address,
	},
}

Flags :: enum {
	Z,
	N,
	H,
	C,
}

Condition :: enum {
	NZ,
	Z,
	C,
	NC,
}

Operand_Type :: enum {
	None,
	cc,
	vec,
	r8,
	r16,
	a16,
	u3,
	a8,
	e8,
	n16,
	n8,
}

Operand :: struct {
	name:      string,
	// type:      Operand_Type,
	immediate: bool,
	bytes:     Maybe(u8),
	Modifier:  enum {
		None,
		Decrement,
		Increment,
	},
}

Instruction :: struct {
	mnemonic:  Mnemonic,
	bytes:     u8,
	operands:  [dynamic]Operand,
	immediate: Maybe(bool),
	flags:     bit_set[Instruction_Flags],
}

Instruction_Error :: enum {
	None,
}

Mnemonic :: enum {
	ADC,
	ADD,
	AND,
	BIT,
	CALL,
	CCF,
	CP,
	CPL,
	DAA,
	DEC,
	DI,
	EI,
	HALT,
	ILLEGAL,
	INC,
	JP,
	JR,
	LD,
	LDH,
	NOP,
	OR,
	POP,
	PREFIX,
	PUSH,
	RES,
	RET,
	RETI,
	RL,
	RLA,
	RLC,
	RLCA,
	RR,
	RRA,
	RRC,
	RRCA,
	RST,
	SBC,
	SCF,
	SET,
	SLA,
	SRA,
	SRL,
	STOP,
	SUB,
	SWAP,
	XOR,
}

Mnemonic_Map_Type :: struct {
	key: string,
	val: Mnemonic,
}

mnemonic_map: []Mnemonic_Map_Type : {
	{"ADC", .ADC},
	{"ADD", .ADD},
	{"AND", .AND},
	{"BIT", .BIT},
	{"CALL", .CALL},
	{"CCF", .CCF},
	{"CP", .CP},
	{"CPL", .CPL},
	{"DAA", .DAA},
	{"DEC", .DEC},
	{"DI", .DI},
	{"EI", .EI},
	{"HALT", .HALT},
	{"ILLEGAL_D3", .ILLEGAL},
	{"ILLEGAL_DB", .ILLEGAL},
	{"ILLEGAL_DD", .ILLEGAL},
	{"ILLEGAL_E3", .ILLEGAL},
	{"ILLEGAL_E4", .ILLEGAL},
	{"ILLEGAL_EB", .ILLEGAL},
	{"ILLEGAL_EC", .ILLEGAL},
	{"ILLEGAL_ED", .ILLEGAL},
	{"ILLEGAL_F4", .ILLEGAL},
	{"ILLEGAL_FC", .ILLEGAL},
	{"ILLEGAL_FD", .ILLEGAL},
	{"INC", .INC},
	{"JP", .JP},
	{"JR", .JR},
	{"LD", .LD},
	{"LDH", .LDH},
	{"NOP", .NOP},
	{"OR", .OR},
	{"POP", .POP},
	{"PREFIX", .PREFIX},
	{"PUSH", .PUSH},
	{"RES", .RES},
	{"RET", .RET},
	{"RETI", .RETI},
	{"RL", .RL},
	{"RLA", .RLA},
	{"RLC", .RLC},
	{"RLCA", .RLCA},
	{"RR", .RR},
	{"RRA", .RRA},
	{"RRC", .RRC},
	{"RRCA", .RRCA},
	{"RST", .RST},
	{"SBC", .SBC},
	{"SCF", .SCF},
	{"SET", .SET},
	{"SLA", .SLA},
	{"SRA", .SRA},
	{"SRL", .SRL},
	{"STOP", .STOP},
	{"SUB", .SUB},
	{"SWAP", .SWAP},
	{"XOR", .XOR},
}

Register16_Names :: []string{"AF", "BC", "DE", "HL", "SP"}
Register8_Names :: []string{"A", "B", "C", "D", "E", "H", "L"}
Vec_Names :: []string{"$00", "$08", "$10", "$18", "$20", "$28", "$30", "$38"}
Index_Names :: []string{"0", "1", "2", "3", "4", "5", "6", "7"}
Condion_Names :: []string{"Z", "C", "NC", "NZ"}

get_mnemonic_type :: proc(name: string) -> Mnemonic {
	for val in mnemonic_map {
		if val.key == name {
			return val.val
		}
	}

	#assert(true, "wtf")
	return nil
}

generate_instruction :: proc(
	root: json.Object,
) -> (
	instructions: map[u8]^Instruction,
	err: json.Error,
) {
	instructions = make(map[u8]^Instruction, len(root))
	i := 0
	for key in root {
		val := root[key].(json.Object)
		instruction := new(Instruction)

		instruction.bytes = u8(val["bytes"].(json.Float))
		instruction.mnemonic = get_mnemonic_type(val["mnemonic"].(json.String))


		json_operands := val["operands"].(json.Array)
		instruction.operands = make([dynamic]Operand, len(json_operands))
		operands := &instruction.operands
		for operand, i in json_operands {
			operand := operand.(json.Object)

			bytes := operand["bytes"]
			operands[i].bytes = nil if bytes == nil else u8(bytes.(json.Float))

			operands[i].Modifier = .None

			if operand["increment"] != nil {
				operands[i].Modifier = .Increment
			} else if operand["decrement"] != nil {
				operands[i].Modifier = .Decrement
			}

		}

		opcode, ok := strconv.parse_u64_maybe_prefixed(key)
		if !ok {
			log.error("key is not a opcode")
		}

		instructions[u8(opcode)] = instruction
		i += 1
	}


	return instructions, .None
}
