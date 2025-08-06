package main

import "core:fmt"
import rl "vendor:raylib"

interrupt_step :: proc() {
	if !cpu.interrupt.master do return
	// && !cpu.interrupt.enable && !cpu.interrupt.flags
	fire := cpu.interrupt.enable & cpu.interrupt.flags

	if .VBlank in fire {
		cpu.interrupt.flags -= {.VBlank}

		@(static) start_time: f64
		end_time := rl.GetTime()
		if (start_time - end_time < 1 / 60) {
			rl.WaitTime(1 / 60)
		}

		gpu.draw = true
		// for gpu.draw {}

		cpu.interrupt.master = false
		push_sp(u16(cpu.PC))

		cpu.PC = 0x40
		cpu.ticks += 12
		start_time = rl.GetTime()
	}

	if .LCD in fire {
		cpu.interrupt.flags -= {.LCD}
		cpu.interrupt.master = false

		push_sp(u16(cpu.PC))

		cpu.PC = 0x48

		cpu.ticks += 12
	}

	if .Timer in fire {
		cpu.interrupt.flags -= {.Timer}
		cpu.interrupt.master = false

		push_sp(u16(cpu.PC))

		cpu.PC = 0x50

		cpu.ticks += 12
	}

	if .Serial in fire {
		cpu.interrupt.flags -= {.Serial}
		cpu.interrupt.master = false

		push_sp(u16(cpu.PC))

		cpu.PC = 0x58

		cpu.ticks += 12
	}

	if .Joypad in fire {
		cpu.interrupt.flags -= {.Joypad}
		cpu.interrupt.master = false

		push_sp(u16(cpu.PC))

		cpu.PC = 0x60

		cpu.ticks += 12
	}
}

