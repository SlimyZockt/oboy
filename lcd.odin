#+feature dynamic-literals
package main

import "base:intrinsics"
import "core:c"
import "core:log"
import "core:math"
import "core:mem"
import "vendor:raylib"

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

Vector2 :: [2]u8

cg := raylib.GREEN


img_data := new([160 * 144]raylib.Color)

handle_graphics :: proc(emu: ^Emulator) {
	cpu := &emu.cpu

	lcdc := cast(^bit_set[LCDC])&cpu.memory[Hardware_Registers.LCDC]
	base_addr: Address = 0x8800 if .Tile_Data_Area in lcdc else 0x8800
	update_tile_data(emu.graphics.tile_data, base_addr, &cpu.memory)

	for i in 0 ..< len(emu.graphics.tile_map) {
		emu.graphics.tile_map[i] = cpu.memory[0x9800 + i]
	}

	for y in 0 ..< 18 {
		for x in 0 ..< 20 {
			tile_id := 18 * y + x
			tile := emu.graphics.tile_data[emu.graphics.tile_map[tile_id]]

			for t_x in 0 ..< 8 {
				for t_y in 0 ..< 8 {
					img_data[(8 * y) + (8 * x) + (8 * t_y + t_x)] = tile[8 * t_y + t_x]
				}
			}

		}

	}

	cpu.memory[0xFF44] = 144

	raylib.UpdateTexture(emu.graphics.render.bg, img_data)
	raylib.DrawTexturePro(
		emu.graphics.render.bg,
		{0, 0, 8, 8},
		{0, 0, 160, 144},
		{0, 0},
		0,
		raylib.WHITE,
	)
}

@(rodata)
grey_color_map := [?]raylib.Color {
	raylib.Color{255, 255, 255, 255},
	raylib.Color{169, 169, 169, 255},
	raylib.Color{84, 84, 84, 255},
	raylib.Color{0, 0, 0, 255},
}

Tile_Data :: [64]raylib.Color

update_tile_data :: proc(tile_data: ^[256]Tile_Data, addr: Address, mem: ^Memory) {
	for i in 0 ..< len(tile_data) {
		tile_addr := addr + u16(i * 16)
		data := mem[tile_addr:][:16]

		for y in 0 ..< 8 {
			left := data[y * 2]
			right := data[y * 2 + 1]

			for x in 0 ..< 8 {
				low := (left >> u8(x)) & 1
				high := (right >> u8(x)) & 1

				color := ((high << 1) + low)

				// texture[8 * y + x] = grey_color_map[color]
				log.debug(color)
				tile_data[i][8 * y + x] = grey_color_map[color]
			}
		}

	}

	return
}

get_tile_map :: proc(addr: Address, mem: Memory) {
}

