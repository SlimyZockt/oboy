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

Mnemonic_Map :: struct {
	key: string,
	val: Mnemonic,
}

mnemonic_map: []Mnemonic_Map : {
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

get_mnemonic_type :: proc(name: string) -> Mnemonic {
	for val in mnemonic_map {
		if val.key == name {
			return val.val
		}
	}

	assert(false, "mnemonic is not valid ")
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

