package main

import "core:encoding/json"
import "core:fmt"
import "core:log"
import "core:os"
import "core:slice"
import "core:strconv"


Address :: u16
Word :: u8

Memory_Map_Type :: struct {
	Rom:          Address,
	Switch_Rom:   Address,
	Vram:         Address,
	External_Ram: Address,
	WRam:         Address,
	Echo_Ram:     Address,
	OAM:          Address,
	Forbidden:    Address,
	Io:           Address,
	Hram:         Address,
	Interupt:     Address,
}

Memory_Map :: Memory_Map_Type {
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

	log.debugf("%x", rom[:10])
	rom_size := len(rom)

	cpu := Cpu{}

	for i in 0 ..< Memory_Map.Rom {
		cpu.memory[i] = rom[i]
	}

	assert(slice.equal(rom[:Memory_Map.Rom], cpu.memory[:Memory_Map.Rom]))


	for {
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

adc :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
add :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
and :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
bit :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
call :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
ccf :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
cp :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
cpl :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
daa :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
dec :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
di :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
ei :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
halt :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
illegal :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
inc :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
jp :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
jr :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
ld :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
ldh :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
nop :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
or :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
pop :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
prefix :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
push :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
res :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
ret :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
reti :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
rl :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
rla :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
rlc :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
rlca :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
rr :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
rra :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
rrc :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
rrca :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
rst :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
sbc :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
scf :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
set :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
sla :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
sra :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
srl :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
stop :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
sub :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
swap :: proc(cpu: ^Cpu, instruction: ^Instruction) {
}
xor :: proc(cpu: ^Cpu, instruction: ^Instruction) {
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
		AF: Register,
		BC: Register,
		DE: Register,
		HL: Register,
		PC: Address,
		SP: Address,
	},
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

Operand_Map_Type :: []struct {
	name: string,
	type: Operand_Type,
}

Operand_Map :: Operand_Map_Type {
	{"$00", .vec},
	{"$08", .vec},
	{"$10", .vec},
	{"$18", .vec},
	{"$20", .vec},
	{"$28", .vec},
	{"$30", .vec},
	{"$38", .vec},
	{"0", .u3},
	{"1", .u3},
	{"2", .u3},
	{"3", .u3},
	{"4", .u3},
	{"5", .u3},
	{"6", .u3},
	{"7", .u3},
	{"A", .r8},
	{"B", .r8},
	{"C", nil},
	{"D", .r8},
	{"E", .r8},
	{"H", .r8},
	{"L", .r8},
	{"Z", .cc},
	{"AF", .r16},
	{"BC", .r16},
	{"DE", .r16},
	{"HL", .r16},
	{"SP", .r16},
	{"NC", .cc},
	{"NZ", .cc},
	{"a16", .a16},
	{"a8", .a8},
	{"e8", .e8},
	{"n16", .n16},
	{"n8", .n8},
}

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
