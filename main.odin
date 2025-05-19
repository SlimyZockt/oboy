package main

import "base:intrinsics"
import "base:runtime"
import "core:c"
import "core:c/libc"
import "core:encoding/json"
import "core:fmt"
import "core:log"
import "core:math/bits"
import "core:os"
import "core:slice"
import "core:strconv"
import "core:strings"

import "vendor:raylib"
import stbsp "vendor:stb/sprintf"

Address :: u16
Word :: u8

Memory_Map_End :: enum u16 {
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

Hardware_Registers :: enum u8 {
	JOYP     = 0x00,
	SB       = 0x01,
	SC       = 0x02,
	DIV      = 0x04,
	TIMA     = 0x05,
	TMA      = 0x06,
	TAC      = 0x07,
	IF       = 0x0F,
	NR10     = 0x10,
	NR11     = 0x11,
	NR12     = 0x12,
	NR13     = 0x13,
	NR14     = 0x14,
	NR21     = 0x16,
	NR22     = 0x17,
	NR23     = 0x18,
	NR24     = 0x19,
	NR30     = 0x1a,
	NR31     = 0x1b,
	NR32     = 0x1c,
	NR33     = 0x1d,
	NR34     = 0x1e,
	NR41     = 0x20,
	NR42     = 0x21,
	NR43     = 0x22,
	NR44     = 0x23,
	NR50     = 0x24,
	NR51     = 0x25,
	NR52     = 0x26,
	Wave_Ram = 0x30,
	LCDC     = 0x40,
	STAT     = 0x41,
	SCY      = 0x42,
	SCX      = 0x43,
	LY       = 0x44,
	LYC      = 0x45,
	DMA      = 0x46,
	BGP      = 0x47,
	OBP0     = 0x48,
	OBP1     = 0x49,
	WY       = 0x4a,
	WX       = 0x4b,
	KEY1     = 0x4D,
	VBK      = 0x4F,
	HDMA1    = 0x51,
	HDMA2    = 0x52,
	HDMA3    = 0x53,
	HDMA4    = 0x54,
	HDMA5    = 0x55,
	RP       = 0x56,
	BCPS     = 0x68,
	BCPD     = 0x69,
	OCPS     = 0x6a,
	OCPD     = 0x6b,
	OPRI     = 0x6c,
	SVBK     = 0x70,
	PCM12    = 0x76,
	PCM34    = 0x77,
	IE       = 0xFF,
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
	memory:          [0xFFFF]Word,
	interrupt:       bool,
	pre_instruction: Mnemonic,
	registers:       struct {
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

Gameboy :: struct {
	cpu: Cpu,
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

Trace_Data :: struct {
	instruction: ^Instruction,
	pc:          Address,
}

Trace_Log :: struct {
	data: [dynamic]^Trace_Data,
}

trace_log: Trace_Log
g_ctx: runtime.Context
gb: Gameboy

opcode_json :: #load("./opcodes.json")

as_asm :: proc(instruction: ^Instruction) -> string {
	out: strings.Builder
	mnemonic, ok := fmt.enum_value_to_string(instruction.mnemonic)
	if !ok {
		panic("Instruction mnemonic err")
	}

	strings.write_string(&out, mnemonic)
	for operand in instruction.operands {
		strings.write_string(&out, " ")
		strings.write_string(&out, operand.name)
	}

	return strings.to_string(out)
}

sig_handler :: proc "c" (_: libc.int) {
	context = g_ctx
	for data in trace_log.data {
		log.fatalf("At %04X: %s", data.pc, as_asm(data.instruction))
		log.fatalf("Bytes: %X", gb.cpu.memory[data.pc:][:data.instruction.bytes])
	}

	free_all()
	runtime.panic("main.panic")
}

raylib_trace_log :: proc "c" (rl_level: raylib.TraceLogLevel, message: cstring, args: ^c.va_list) {
	context = g_ctx

	level: log.Level
	switch rl_level {
	case .TRACE, .DEBUG:
		level = .Debug
	case .INFO:
		level = .Info
	case .WARNING:
		level = .Warning
	case .ERROR:
		level = .Error
	case .FATAL:
		level = .Fatal
	case .ALL, .NONE:
		fallthrough
	case:
		log.panicf("unexpected log level %v", rl_level)
	}

	@(static) buf: [dynamic]byte
	log_len: i32
	for {
		buf_len := i32(len(buf))
		log_len = stbsp.vsnprintf(raw_data(buf), buf_len, message, args)
		if log_len <= buf_len {
			break
		}

		non_zero_resize(&buf, max(128, len(buf) * 2))
	}

	context.logger.procedure(
		context.logger.data,
		level,
		string(buf[:log_len]),
		context.logger.options,
	)
}


main :: proc() {
	context.logger = log.create_console_logger(.Debug)
	g_ctx = context

	raylib.SetTraceLogLevel(.ALL)
	raylib.SetTraceLogCallback(raylib_trace_log)

	libc.signal(libc.SIGSEGV, sig_handler)
	libc.signal(libc.SIGILL, sig_handler)

	if len(os.args) != 2 {
		log.errorf("Wrong number of args (%d)", len(os.args))
		log.error(os.args)
		return
	}

	rom_path := os.args[1]
	rom, rom_ok := os.read_entire_file_from_filename(rom_path)
	if !rom_ok {
		log.error("Can not read file")
		return
	}

	json_data, err := json.parse(opcode_json)
	if err != .None {
		log.error("Failed to parse the json file.")
		log.error(err)
		return
	}
	defer json.destroy_value(json_data)

	instruction := new(Instruction)
	root := json_data.(json.Object)

	unprefixed_instructions := generate_instruction(root["unprefixed"].(json.Object))
	defer destroy_instructions(unprefixed_instructions)

	prefixed_instructions := generate_instruction(root["cbprefixed"].(json.Object))
	defer destroy_instructions(prefixed_instructions)

	rom_size := len(rom)

	gb.cpu.registers.PC = 0x0100
	gb.cpu.registers.SP = 0xfffe

	for i in 0 ..< u16(Memory_Map_End.Rom) {
		gb.cpu.memory[i] = rom[i]
	}
	assert(slice.equal(rom[:Memory_Map_End.Rom], gb.cpu.memory[:Memory_Map_End.Rom]))

	raylib.InitWindow(160, 144, "oboy")
	raylib.SetTargetFPS(30)

	for !raylib.WindowShouldClose() {

		// for false {
		opcode := gb.cpu.memory[gb.cpu.registers.PC]
		instruction := unprefixed_instructions[opcode]

		if instruction.mnemonic == .PREFIX {
			opcode = gb.cpu.memory[gb.cpu.registers.PC]
			instruction = prefixed_instructions[opcode]
		}

		if instruction.mnemonic == .HALT {
			break
		}

		trace_data := new(Trace_Data)
		trace_data^ = {instruction, gb.cpu.registers.PC}
		append(&trace_log.data, trace_data)

		execute_instruction(&gb.cpu, instruction)
		excute_hardware_register(&gb)

		raylib.BeginDrawing()
		raylib.ClearBackground(raylib.WHITE)
		raylib.EndDrawing()
	}

	raylib.CloseWindow()
	free_all()
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
	case .HALT:
		halt(cpu, instruction)
	case .ILLEGAL:
		#assert(true, "ilLegal instruction gb")
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
		ret(cpu, instruction)
		cpu.interrupt = true
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
	case .NOP:
	case .DI, .EI:
	case:
		log.error("Not implemented yet")
	}

	#partial switch cpu.pre_instruction {
	case .DI:
		cpu.interrupt = false
	case .EI:
		cpu.interrupt = true
	}

	cpu.pre_instruction = instruction.mnemonic

	#partial switch instruction.mnemonic {
	case .CALL, .JP, .JR, .RET, .RETI, .RST:
		return
	}

	cpu.registers.PC += u16(instruction.bytes)
}

excute_hardware_register :: proc(gb: ^Gameboy) {
	handle_lcd(gb)
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

generate_instruction :: proc(root: json.Object) -> (instructions: map[u8]^Instruction) {
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
			immediate := operand["immediate"]
			operands[i].immediate = false if immediate == nil else immediate.(json.Boolean)

			operands[i].name = operand["name"].(json.String)

			operands[i].Modifier = .None

			if operand["increment"] != nil {
				operands[i].Modifier = .Increment
			} else if operand["decrement"] != nil {
				operands[i].Modifier = .Decrement
			}

		}

		opcode, ok := strconv.parse_u64_maybe_prefixed(key)
		if !ok {
			log.panic("key is not a opcode")
		}

		instructions[u8(opcode)] = instruction
		i += 1
	}

	return instructions
}

destroy_instructions :: proc(instructions: map[u8]^Instruction) {
	for _, instruction in instructions {
		delete(instruction.operands)
		free(instruction)
	}
	delete(instructions)
}

