package main

import "core:sync"


interrupt_step :: proc() {
	if !cpu.interrupt.master do return
	fire := cpu.interrupt.enable & cpu.interrupt.flags

	if .VBlank in fire {
		cpu.interrupt.flags -= {.VBlank}

		sync.mutex_lock(&mutex)
		if renderer_running {
			gpu.draw = true
			for renderer_running && gpu.draw {
				sync.cond_wait(&cond, &mutex)
			}
		} else {
			gpu.draw = false
		}
		sync.mutex_unlock(&mutex)

		cpu.interrupt.master = false
		push_sp(u16(cpu.PC))

		cpu.PC = 0x40
		cpu.ticks += 12
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
