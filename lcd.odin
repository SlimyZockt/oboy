package main

import "base:intrinsics"
import "core:math"

LCDC :: enum u8 {
	LCD_Enable,
	Window_Tile_Map_Area,
	Window_ENABLE,
	Tile_Data_Area,
	BG_Tile_Map,
	OBJ_Size,
	OBJ_Enabled,
	BG_Enabled,
}

Color :: enum {
	White,
	Light_Gray,
	Dark_Gray,
	Dark,
}

Tile :: struct {
	pos:     [2]u8,
	texture: [64]Color,
}

handle_graphics :: proc(gb: ^Gameboy) {
	cpu := &gb.cpu
	// LCDC
	lcdc := cast(^bit_set[LCDC])&cpu.memory[Hardware_Registers.LCDC]

}

