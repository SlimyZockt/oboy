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
import "vendor:raylib"

import inst "instructions"
import stbsp "vendor:stb/sprintf"

Register :: struct #raw_union {
	full:   u16,
	single: struct {
		lower, upper: u8,
	},
}

Interrupt :: enum u8 {}

Cpu :: struct {
	interrupt:       struct {
		flags:  bit_set[Interrupt;u8],
		enable: bool,
	},
	PC:              Address,
	SP:              Address,
	pre_instruction: inst.Mnemonic,
	registers:       struct {
		AF: struct #raw_union {
			full:   u16,
			single: struct {
				F: bit_set[Flags;u8],
				A: u8,
			},
		},
		BC: struct #raw_union {
			full:   u16,
			single: struct {
				C: u8,
				B: u8,
			},
		},
		DE: struct #raw_union {
			full:   u16,
			single: struct {
				E: u8,
				D: u8,
			},
		},
		HL: struct #raw_union {
			full:   u16,
			single: struct {
				L: u8,
				H: u8,
			},
		},
	},
}

Gpu :: struct {
	controll: u8,
	scroll_x: u8,
	scroll_y: u8,
	scanline: u8,
	tick:     u8,
}

Data_ID :: u8
Tile_Map :: [1024 * 2]Data_ID
Render_Layer :: raylib.Texture2D

Graphics :: struct {
	render: struct {
		window:  Render_Layer,
		bg:      Render_Layer,
		objects: Render_Layer,
	},
}

Gameboy :: struct {
	cpu: Cpu,
	gpu: Gpu,
}

Flags :: enum {
	Z,
	N,
	H,
	C,
}

Operand_TD :: struct {
	// name: inst.Operand_Name,
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

trace_log: Trace_Log
g_ctx: runtime.Context

cpu: Cpu
gpu: Gpu

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
	cartridge, ok := os.read_entire_file_from_filename(rom_path)
	if !ok {
		log.error("Can not read file")
		return
	}
	defer delete(cartridge)

	for i in 0 ..< 0x8000 {
		rom[i] = cartridge[i]
	}
	assert(slice.equal(rom[:0x8000], cartridge[:0x8000]))
	cartridge_size := len(cartridge)

	{ 	// skip Bootloader
		cpu.PC = 0x0100
		cpu.SP = 0xfffe
		cpu.registers.AF.single.F = {.Z, .H, .C}
		cpu.registers.AF.single.A = 0x01
	}

	raylib.InitWindow(160, 144, "oboy")
	raylib.SetTargetFPS(30)

	emtpy_image := raylib.GenImageColor(8, 8, raylib.Color{255, 0, 0, 255})

	raylib.UnloadImage(emtpy_image)

	nop_count := 0
	for !raylib.WindowShouldClose() {
		opcode := rom[cpu.PC]
		instruction := inst.UnprefixedInstructions[opcode]
		if instruction.mnemonic == .PREFIX {
			cpu.PC += 1
			opcode = rom[cpu.PC]
			instruction = inst.PrefixedInstructions[0xFE + opcode]
		}

		when ODIN_DEBUG {
			//Generates data for the trace log
			trace_data := new(Instruction_TD)
			trace_data.pc = cpu.PC
			trace_data.name = instruction.mnemonic
			operand_loc := 0
			append(&trace_log.data, trace_data)


			if instruction.mnemonic != .NOP {
				nop_count = 0
			} else {
				nop_count += 1
			}

			if nop_count == 10 {
				panic("too many nop")
			}
		}

		if instruction.mnemonic == .HALT {
			break
		}

		// cpu.cpu.memory[0xFF44] = (cpu.cpu.memory[0xFF44] + 1) % 154

		//Do the importend thing 
		execute_instruction(opcode)
		// handle_graphics(&cpu)

		//Cleanup
		raylib.BeginDrawing()
		raylib.ClearBackground(raylib.WHITE)
		raylib.EndDrawing()
	}


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

