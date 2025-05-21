#+feature dynamic-literals
package main

import "base:intrinsics"
import "core:c"
import "core:log"
import "core:math"
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

Color :: enum {
	C0,
	C1,
	C2,
	C3,
}

Vector2 :: [2]u8

Tile_Data :: [64]Color

handle_graphics :: proc(emu: ^Emulator) {
	cpu := &emu.cpu
	// LCDC

	lcdc := cast(^bit_set[LCDC])&cpu.memory[Hardware_Registers.LCDC]
	base_addr: Address = 0x8800 if .Tile_Data_Area in lcdc else 0x8800
	update_tile_data(&emu.graphics.map_data_layer, base_addr, &cpu.memory)

	for i in 0x9800 ..< 0x9FFF {
		emu.graphics.tile_map[i - 0x9800] = cpu.memory[i]
	}

	for y in 0 ..< 18 {
		for x in 0 ..< 20 {
			raylib.UpdateTexture(
				emu.graphics.render.bg[20 * y + x],
				emu.graphics.map_data_layer[emu.graphics.tile_map[20 * y + x]].data,
			)


			raylib.DrawTexture(
				emu.graphics.render.bg[20 * y + x],
				c.int(x) * 8,
				c.int(y) * 8,
				raylib.WHITE,
			)

		}
	}

}

grey_color_map := [?]raylib.Color {
	raylib.Color{255, 255, 255, 255},
	raylib.Color{169, 169, 169, 255},
	raylib.Color{84, 84, 84, 255},
	raylib.Color{0, 0, 0, 255},
}

update_tile_data :: proc(data_layer: ^Data_Layer, addr: Address, mem: ^Memory) {
	for i in 0 ..< 255 {
		tile_addr := addr + u16(i * 16)
		data := mem^[tile_addr:][:16]

		texture := new([64]raylib.Color)
		for y in 0 ..< 8 {
			left := data[y * 2]
			right := data[y * 2 + 1]

			for x in 0 ..< 8 {
				low := (left >> u8(x)) & 1
				high := (right >> u8(x)) & 1

				color := Color((high << 1) + low)

				// texture[8 * y + x] = grey_color_map[color]
				texture[8 * y + x] = raylib.GREEN
			}
		}

		data_layer^[i] = {texture, 8, 8, 1, raylib.PixelFormat.UNCOMPRESSED_R8G8B8A8}
	}
}

get_tile_map :: proc(addr: Address, mem: Memory) {
}

