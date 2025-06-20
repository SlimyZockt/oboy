package main

import "base:intrinsics"
import "base:runtime"
import "core:c"
import "core:c/libc"
import "core:encoding/json"
import "core:fmt"
import "core:log"
import "core:math/bits"
import "core:mem"
import "core:os"
import "core:slice"
import "core:strconv"
import "core:strings"
import inst "instructions"

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

Memory :: [0xFFFF]Word

Cpu :: struct {
	memory:          Memory,
	interrupt:       bool,
	pre_instruction: inst.Mnemonic,
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

Data_ID :: u8
Tile_Map :: [1024 * 2]Data_ID
Render_Layer :: raylib.Texture2D

Graphics :: struct {
	render:    struct {
		window:  Render_Layer,
		bg:      Render_Layer,
		objects: Render_Layer,
	},
	tile_data: ^[256]Tile_Data,
	tile_map:  Tile_Map,
}

Emulator :: struct {
	cpu:      Cpu,
	graphics: Graphics,
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

Operand_TD :: struct {
	name: inst.Operand_Name,
	data: union {
		u16,
		u8,
		[]u8,
		i8,
	},
}
Instruction_TD :: struct {
	pc:       Address,
	name:     inst.Mnemonic,
	operands: [dynamic]Operand_TD,
}

Trace_Log :: struct {
	data: [dynamic]^Instruction_TD,
}

CYCLES :: 70224

trace_log: Trace_Log
g_ctx: runtime.Context
emu: Emulator = {}

opcode_json :: #load("./opcodes.json")

sig_handler :: proc "c" (_: libc.int) {
	context = g_ctx
	print_trace_log()

	free_all()
	log.panic("main.panic")
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

print_trace_log :: proc() {
	fmt.println("---Start Instrcution Trace Log---")

	for instruction in trace_log.data {
		out: strings.Builder
		defer delete(out.buf)

		fmt.sbprintf(&out, "0x%04X %s", instruction.pc, instruction.name)
		for &operand in instruction.operands {
			fmt.sbprintf(&out, " %s", operand.name)
			if operand.data == nil do continue
			fmt.sbprintf(&out, "=%X", operand.data)
		}
		str := strings.to_string(out)
		fmt.println(str)
	}
	delete_trace_log(trace_log.data)
	fmt.println("---End Instrcution Trace Log---")
}

assertion_failure_proc :: proc(prefix, message: string, loc: runtime.Source_Code_Location) -> ! {
	print_trace_log()
	log.fatal(message, location = loc)
	os.exit(1)
}

main :: proc() {
	g_ctx = context

	context.logger = log.create_console_logger(.Debug)
	context.assertion_failure_proc = assertion_failure_proc

	raylib.SetTraceLogLevel(.ALL)
	raylib.SetTraceLogCallback(raylib_trace_log)
	libc.signal(libc.SIGSEGV, sig_handler)
	libc.signal(libc.SIGILL, sig_handler)

	when ODIN_DEBUG {
		track: mem.Tracking_Allocator
		mem.tracking_allocator_init(&track, context.allocator)
		context.allocator = mem.tracking_allocator(&track)

		defer {
			if len(track.allocation_map) > 0 {
				fmt.eprintf("=== %v allocations not freed: ===\n", len(track.allocation_map))
				for _, entry in track.allocation_map {
					fmt.eprintf("- %v bytes @ %v\n", entry.size, entry.location)
				}
			}
			if len(track.bad_free_array) > 0 {
				fmt.eprintf("=== %v incorrect frees: ===\n", len(track.bad_free_array))
				for entry in track.bad_free_array {
					fmt.eprintf("- %p @ %v\n", entry.memory, entry.location)
				}
			}
			mem.tracking_allocator_destroy(&track)
		}
	}

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
	defer delete(rom)

	rom_size := len(rom)

	emu.cpu.registers.PC = 0x0100
	emu.cpu.registers.SP = 0xfffe
	emu.cpu.registers.AF.single.lower = {.Z, .H, .C}
	emu.cpu.registers.AF.single.upper = 0x01

	emu.cpu.registers.BC.single.upper = 0x00
	emu.cpu.registers.BC.single.lower = 0x13
	emu.cpu.registers.DE.single.upper = 0x00
	emu.cpu.registers.DE.single.lower = 0xD8
	emu.cpu.registers.HL.single.upper = 0x01
	emu.cpu.registers.HL.single.lower = 0x4D
	emu.cpu.memory[0xFF0F] = 0xE1

	for i in 0 ..< u16(Memory_Map_End.Switch_Rom) {
		emu.cpu.memory[i] = rom[i]
	}
	assert(slice.equal(rom[:Memory_Map_End.Rom], emu.cpu.memory[:Memory_Map_End.Rom]))

	raylib.InitWindow(160, 144, "oboy")
	raylib.SetTargetFPS(30)

	emtpy_image := raylib.GenImageColor(8, 8, raylib.Color{255, 0, 0, 255})

	emu.graphics.render.window = raylib.LoadTextureFromImage(emtpy_image)
	emu.graphics.render.objects = raylib.LoadTextureFromImage(emtpy_image)
	emu.graphics.render.bg = raylib.LoadTextureFromImage(emtpy_image)
	raylib.UnloadImage(emtpy_image)

	emu.graphics.tile_data = new([256]Tile_Data)
	defer free(emu.graphics.tile_data)

	nop_count := 0
	for !raylib.WindowShouldClose() {
		waitticks := CYCLES


		opcode := emu.cpu.memory[emu.cpu.registers.PC]
		instruction := inst.Instructions[opcode]
		if instruction.mnemonic == .PREFIX {
			emu.cpu.registers.PC += 1
			opcode = emu.cpu.memory[emu.cpu.registers.PC]
			instruction = inst.Instructions[0xFE + opcode]
		}

		when ODIN_DEBUG {
			//Generates data for the trace log
			trace_data := new(Instruction_TD)
			trace_data.pc = emu.cpu.registers.PC
			trace_data.name = instruction.mnemonic
			trace_data.operands = make([dynamic]Operand_TD, len(instruction.operands))
			operand_loc := 0

			for operand, i in instruction.operands {
				trace_data.operands[i].name = operand.name
				switch {
				case is_reg8(operand.name):
					trace_data.operands[i].data = get_reg8(&emu.cpu, operand.name)^
				case is_reg16(operand.name):
					trace_data.operands[i].data = get_reg16(&emu.cpu, operand.name)^
				case operand.name == .O_e8:
					trace_data.operands[i].data = i8(emu.cpu.memory[trace_data.pc + 1])
				case operand.name == .O_n8 || operand.name == .O_a8:
					trace_data.operands[i].data = emu.cpu.memory[trace_data.pc + 1]
				case operand.name == .O_n16 || operand.name == .O_a16:
					trace_data.operands[i].data =
						(u16(emu.cpu.memory[trace_data.pc + 2]) << 8) +
						u16(emu.cpu.memory[trace_data.pc + 1])
				case get_condition(operand.name) != nil:
				case operand.bytes != 0:
					trace_data.operands[i].data =
					emu.cpu.memory[int(trace_data.pc) + operand_loc + 1:][:operand.bytes]
				}

				operand_loc += int(operand.bytes)
			}
			append(&trace_log.data, trace_data)
		}

		if instruction.mnemonic == .HALT {
			break
		}

		if instruction.mnemonic != .NOP {
			nop_count = 0
		} else {
			nop_count += 1
		}

		if nop_count == 10 {
			assert(false, "too many nop")
		}

		emu.cpu.memory[0xFF44] = (emu.cpu.memory[0xFF44] + 1) % 154

		//Do the importend thing 
		execute_instruction(&emu.cpu, &instruction)
		handle_graphics(&emu)

		//Cleanup
		raylib.BeginDrawing()
		raylib.ClearBackground(raylib.WHITE)
		raylib.EndDrawing()
	}


	raylib.UnloadTexture(emu.graphics.render.bg)
	raylib.UnloadTexture(emu.graphics.render.objects)
	raylib.UnloadTexture(emu.graphics.render.window)

	raylib.CloseWindow()
	when ODIN_DEBUG {
		print_trace_log()
	}
}

delete_trace_log :: proc(trace_log: [dynamic]^Instruction_TD) {
	for t in trace_log {
		free(t)
		delete(t.operands)
	}
	delete(trace_log)
}

execute_instruction :: proc(cpu: ^Cpu, instruction: ^inst.Instruction) {
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
	case .ILLEGAL_EB,
	     .ILLEGAL_FC,
	     .ILLEGAL_E3,
	     .ILLEGAL_EC,
	     .ILLEGAL_FD,
	     .ILLEGAL_F4,
	     .ILLEGAL_D3,
	     .ILLEGAL_DB,
	     .ILLEGAL_DD,
	     .ILLEGAL_E4,
	     .ILLEGAL_ED:
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
		panic("prefix instruction")
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
	// assert(false, "noop")
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

