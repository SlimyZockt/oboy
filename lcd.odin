package main

LCDC :: enum {
	ENABLE,
	TILE_MAP_EDITOR,
	WINDOW_ENABLE,
	TILE_DATA_AREA,
	BG_TILE_MAP,
	OBJ_SIZE,
	OBJ_ENABLED,
	BG_ENABLED,
}

handle_lcd :: proc(gb: ^Gameboy) {
	cpu := &gb.cpu
	// LCDC
	lcdc: ^bit_set[LCDC] = cast(^bit_set[LCDC])&cpu.memory[Hardware_Registers.LCDC]

}

