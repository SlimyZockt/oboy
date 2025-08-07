package main

import "base:intrinsics"
import "base:runtime"
import "core:c"
import "core:fmt"
import "core:image"
import "core:image/bmp"
import "core:log"
import "core:math"
import "core:mem"
import "core:os"
import "core:strings"
import "core:sync"
import "core:thread"

import vmem "core:mem/virtual"

import rl "vendor:raylib"
import stbsp "vendor:stb/sprintf"

import inst "instructions"

Register :: struct #raw_union {
	full:   u16,
	single: struct {
		lower, upper: u8,
	},
}

Interrupt :: enum u8 {
	VBlank,
	LCD,
	Timer,
	Serial,
	Joypad,
	_,
	_,
	_,
}

Cpu :: struct {
	stopped:    bool,
	PC:         Address,
	SP:         Address,
	pre_opcode: u8,
	// joypad:     bit_set[Joypad;u8],
	input:      struct {
		buttons:   bit_set[Buttons;u8],
		direction: bit_set[Direction;u8],
	},
	ticks:      u64,
	interrupt:  struct {
		master: bool,
		flags:  bit_set[Interrupt;u8],
		enable: bit_set[Interrupt;u8],
	},
	registers:  struct {
		using _: struct #raw_union {
			AF:      u16,
			using _: struct {
				F: bit_set[Flags;u8],
				A: u8,
			},
		},
		using _: struct #raw_union {
			BC:      u16,
			using _: struct {
				C: u8,
				B: u8,
			},
		},
		using _: struct #raw_union {
			DE:      u16,
			using _: struct {
				E: u8,
				D: u8,
			},
		},
		using _: struct #raw_union {
			HL:      u16,
			using _: struct {
				L: u8,
				H: u8,
			},
		},
	},
}

// Joypad :: enum u8 {
// 	A_Right,
// 	B_Left,
// 	Select_Up,
// 	Start_Down,
// 	Select_DPad,
// 	Select_Buttons,
// }

Buttons :: enum u8 {
	A,
	B,
	Select,
	Start,
}

Direction :: enum u8 {
	Right,
	Down,
	Left,
	Up,
}


LCD_Control :: enum u8 {
	Background_Window_Enable,
	OBJ_Enable,
	OBJ_Size,
	Background_Area_Offset,
	Tiledata_Offset,
	Window_Enable,
	Window_Area_Offset,
	LCD_PPU_Enable,
}


Gpu_Mode :: enum u8 {
	HBlank = 0,
	VBlank = 1,
	OAM    = 2,
	Draw   = 3,
}

Gpu :: struct {
	draw:     bool,
	mode:     Gpu_Mode,
	controll: bit_set[LCD_Control;u8],
	scroll_x: u8,
	scroll_y: u8,
	win_x:    u8,
	win_y:    u8,
	scanline: u8,
	dots:     u64,
}

Flags :: enum {
	Z,
	N,
	H,
	C,
}

Operand :: union {
	u8,
	u16,
}

Instruction_Debug_Data :: struct {
	prefixed: bool,
	opcode:   u8,
	operands: Operand,
	pc:       Address,
	kind:     inst.Mnemonic,
}

debug_data: [dynamic]Instruction_Debug_Data

g_ctx: runtime.Context

mutex: sync.Mutex
cpu: Cpu
gpu: Gpu
memory: Memory

Memory :: struct {
	rom:        [0x8000]u8,
	extern_ram: [0x2000]u8,
	vram:       [0x2000]u8,
	oam:        [0x00A0]u8,
	wram:       [0x2000]u8,
	hram:       [0x80]u8,
	io:         [0x100]u8,
}


debug_arena: vmem.Arena

sig_handler :: proc "c" (_: c.int) {
	context = g_ctx
	print_debug_data()

	log.panic("main.panic")
}

rl_trace_log :: proc "c" (rl_level: rl.TraceLogLevel, message: cstring, args: ^c.va_list) {
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


run_instruction_mnemonics: bit_set[inst.Mnemonic]
run_instructions: [0x100]bool
run_cb_instructions: [0x100]bool

print_debug_data :: proc() {
	DEBUG_FOLDER :: "debug"
	PRINT_LENGTH :: 30

	trace_log_builder: strings.Builder
	defer strings.builder_destroy(&trace_log_builder)
	for instruction_data in debug_data {
		lookup :=
			inst.PrefixedInstructions if instruction_data.prefixed else inst.UnprefixedInstructions

		instruction := lookup[instruction_data.opcode]

		fmt.sbprintfln(
			&trace_log_builder,
			"0x%04X, %02X, %04X, %s",
			instruction_data.pc,
			instruction_data.opcode,
			instruction_data.operands,
			instruction.name,
		)
	}

	trace_log := strings.to_string(trace_log_builder)

	if !os.exists(DEBUG_FOLDER) {
		os.make_directory(DEBUG_FOLDER)
	}

	os.write_entire_file(DEBUG_FOLDER + "/trace_log.csv", transmute([]u8)trace_log[:])
	os.write_entire_file(DEBUG_FOLDER + "/vram.bin", memory.vram[:])
	os.write_entire_file(DEBUG_FOLDER + "/hram.bin", memory.hram[:])
	os.write_entire_file(DEBUG_FOLDER + "/oam.bin", memory.oam[:])
	os.write_entire_file(DEBUG_FOLDER + "/io.bin", memory.io[:])
	os.write_entire_file(DEBUG_FOLDER + "/wram.bin", memory.wram[:])

	img, ok := image.pixels_to_image(
		transmute([]image.RGB_Pixel)framebuffer[:],
		SCREEN_WIDTH,
		SCREEN_HEIGHT,
	)
	ensure(ok)

	err := bmp.save_to_file(DEBUG_FOLDER + "/framebuffer.bmp", &img)
	ensure(err == nil)

	log.debug(run_instruction_mnemonics)

	fmt.print("Run instructions: ")
	for used, i in run_instructions {
		if !used do continue
		fmt.printf("0x%02x, ", i)
	}
	fmt.print("\n")

	fmt.print("Run cb instructions: ")
	for used, i in run_cb_instructions {
		if !used do continue
		fmt.printf("0x%02x, ", i)
	}
	fmt.print("\n")
}


assertion_failure_proc :: proc(prefix, message: string, loc: runtime.Source_Code_Location) -> ! {
	print_debug_data()
	log.fatal(message, location = loc)
	os.exit(1)
}


Error :: struct {
	location: runtime.Source_Code_Location,
	massage:  string,
}

Result :: union($T: typeid) #no_nil {
	Error,
	T,
}

new_error :: proc(massage: string, location := #caller_location) -> Error {
	return Error{location, massage}
}

main :: proc() {
	g_ctx = context

	context.logger = log.create_console_logger(.Debug)
	context.assertion_failure_proc = assertion_failure_proc

	rl.SetTraceLogLevel(.ALL)
	rl.SetTraceLogCallback(rl_trace_log)
	// libc.signal(libc.SIGSEGV, sig_handler)
	// libc.signal(libc.SIGILL, sig_handler)

	debug_data = make([dynamic]Instruction_Debug_Data)
	defer delete(debug_data)

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


	{ 	// Setup emulator
		if len(os.args) != 2 {
			log.errorf("Wrong number of args (%d)", len(os.args))
			log.error(os.args)
			return
		}


		rom_path := os.args[1]
		rom_file, err := os.open(rom_path)
		if err != nil {
			log.error("Can not read file")
			return
		}
		defer os.close(rom_file)

		bytes_read, read_err := os.read(rom_file, memory.rom[:])
		if read_err != nil {
			log.error("Can not read file")
			return
		}
		assert(0x8000 == bytes_read)

	}

	load_boot_rom(&cpu)


	t := thread.create_and_start_with_poly_data4(&framebuffer, &gpu.draw, &cpu, &mutex, render)
	assert(t != nil)

	render :: proc(framebuffer: ^Framebuffer, draw: ^bool, cpu: ^Cpu, mutex: ^sync.Mutex) {
		fmt.println("starting Raylib")
		rl.SetConfigFlags({.WINDOW_RESIZABLE, .WINDOW_MAXIMIZED, .BORDERLESS_WINDOWED_MODE})
		rl.InitWindow(SCREEN_WIDTH, SCREEN_HEIGHT, "oboy")
		rl.SetWindowMinSize(SCREEN_WIDTH, SCREEN_HEIGHT)
		defer rl.CloseWindow()

		img := rl.Image{nil, SCREEN_WIDTH, SCREEN_HEIGHT, 1, .UNCOMPRESSED_R8G8B8}
		texture := rl.LoadTextureFromImage(img)

		rl.SetTargetFPS(60)
		for !rl.WindowShouldClose() {

			if draw^ {
				rl.UpdateTexture(texture, framebuffer)

				sync.mutex_lock(mutex)
				draw^ = false
				sync.mutex_unlock(mutex)
			}

			rl.BeginDrawing()

			scale: f32 = math.min(
				f32(rl.GetScreenWidth()) / SCREEN_WIDTH,
				f32(rl.GetScreenHeight()) / SCREEN_HEIGHT,
			)
			offset_x := (f32(rl.GetScreenWidth()) - (SCREEN_WIDTH * scale)) / 2
			offset_y := (f32(rl.GetScreenHeight()) - (SCREEN_HEIGHT * scale)) / 2


			rl.DrawTexturePro(
				texture,
				{0, 0, SCREEN_WIDTH, SCREEN_HEIGHT},
				{offset_x, offset_y, SCREEN_WIDTH * scale, SCREEN_HEIGHT * scale},
				{0, 0},
				0,
				rl.WHITE,
			)

			rl.ClearBackground(rl.BLACK)
			rl.EndDrawing()
		}

		fmt.println("closing Raylib")
	}

	for {

		cpu.input.direction = {.Down, .Up, .Left, .Right}
		cpu.input.buttons = {.Select, .Start, .A, .B}
		if rl.GetKeyPressed() != .KEY_NULL {
			cpu.stopped = false
		}


		if rl.IsKeyDown(.A) {cpu.input.direction -= {.Left}}
		if rl.IsKeyDown(.D) {cpu.input.direction -= {.Right}}
		if rl.IsKeyDown(.W) {cpu.input.direction -= {.Up}}
		if rl.IsKeyDown(.S) {cpu.input.direction -= {.Down}}

		if rl.IsKeyDown(.Q) {cpu.input.buttons -= {.B}}
		if rl.IsKeyDown(.E) {cpu.input.buttons -= {.A}}
		if rl.IsKeyDown(.TAB) {cpu.input.buttons -= {.Select}}
		if rl.IsKeyDown(.SPACE) {cpu.input.buttons -= {.Start}}

		// if (transmute(u8)cpu.joypad != 0x00) {
		// 	cpu.interrupt.flags += {.Joypad}
		// }

		step_cpu()
		step_gpu()
		interrupt_step()


		if thread.is_done(t) {
			thread.destroy(t)
			break
		}
	}


	when ODIN_DEBUG {
		print_debug_data()
	}
}

Render_Data :: struct {
	draw:        ^bool,
	framebuffer: ^Framebuffer,
}

