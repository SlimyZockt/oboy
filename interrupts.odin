package main

import "core:log"
import rl "vendor:raylib"

interrupt_step :: proc(texture: rl.Texture) {
	if !cpu.interrupt.master do return
	// && !cpu.interrupt.enable && !cpu.interrupt.flags
	fire := cpu.interrupt.enable & cpu.interrupt.flags
	if cpu.interrupt.flags != {} {
		log.warn(cpu.interrupt.flags)
	}

	if cpu.interrupt.enable != {} {
		log.warn(cpu.interrupt.enable)
	}

	if .VBlank in fire {
		log.fatal(fire)
		cpu.interrupt.flags -= {.VBlank}
		vblank(texture)
	}

	if .LCD in fire {
		cpu.interrupt.flags -= {.LCD}
		lcd_Stat()
	}

	if .Timer in fire {
		cpu.interrupt.flags -= {.Timer}
		timer()
	}

	if .Serial in fire {
		cpu.interrupt.flags -= {.Serial}
		serial()
	}

	if .Joypad in fire {
		cpu.interrupt.flags -= {.Joypad}
		joypad()
	}

	// log.debug(fire)

}

vblank :: proc(texture: rl.Texture) {

	@(static) start_time: f64
	end_time := rl.GetTime()
	//
	if (start_time - end_time < 1 / 60) {
		rl.WaitTime(1 / 60)
	}


	log.fatal("Vblank render")
	rl.UpdateTexture(texture, &framebuffer)

	cpu.interrupt.master = false
	cpu.SP -= 2
	write_u16(cpu.SP, u16(cpu.PC))
	cpu.PC = 0x40

	cpu.ticks += 12
	start_time = rl.GetTime()
}


lcd_Stat :: proc() {
	cpu.interrupt.master = false

	cpu.SP -= 2
	write_u16(cpu.SP, u16(cpu.PC))

	cpu.PC = 0x48

	cpu.ticks += 12
}

timer :: proc() {
	cpu.interrupt.master = false

	cpu.SP -= 2
	write_u16(cpu.SP, u16(cpu.PC))

	cpu.PC = 0x50

	cpu.ticks += 12
}

serial :: proc() {
	cpu.interrupt.master = false

	cpu.SP -= 2
	write_u16(cpu.SP, u16(cpu.PC))

	cpu.PC = 0x58

	cpu.ticks += 12
}

joypad :: proc() {
	cpu.interrupt.master = false

	cpu.SP -= 2
	write_u16(cpu.SP, u16(cpu.PC))

	cpu.PC = 0x60

	cpu.ticks += 12
}

