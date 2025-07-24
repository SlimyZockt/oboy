package main

Color :: distinct [3]u8
Palette :: distinct [4]Color

SCREEN_WIIDTH :: 160
SCREEN_HIGHT :: 144
SCANLINES :: 154

@(rodata)
DEFAULT_PALETTE := Palette{{255, 255, 255}, {192, 192, 192}, {96, 96, 96}, {0, 0, 0}}

bg_palette := DEFAULT_PALETTE
sprite_palettes := [2]Palette{DEFAULT_PALETTE, DEFAULT_PALETTE}

framebuffer: [SCREEN_HIGHT * SCREEN_WIIDTH]Color

// dots
ObjectFlags :: enum u8 {
	CGB_Pallet,
	Blank,
	DMG_Pallet,
	X_Filp,
	Y_Filp,
	Priority,
}

Vec2 :: distinct [2]u8

Object :: struct {
	tile_index: u8,
	flags:      bit_set[ObjectFlags;u8],
	pos:        Vec2,
}

step_gpu :: proc() {
	gpu.dots += u16(cpu.ticks - gpu.pre_ticks)

	gpu.pre_ticks = cpu.ticks

	switch gpu.mode {
	case .HBlank:
		if 204 < gpu.dots do break

		// HBlank
		gpu.scanline += 1

		if gpu.scanline == 143 {
			if .VBlank in cpu.interrupt.flags {
				cpu.interrupt.flags -= {.VBlank}
			}

			gpu.mode = .VBlank
		} else {
			gpu.mode = .OAM
		}

		gpu.dots -= 204
	case .VBlank:
		if 456 < gpu.dots do break

		gpu.scanline += 1

		if gpu.scanline == 154 {
			gpu.scanline = 0
			gpu.mode = .OAM
		}

		gpu.dots -= 456

	case .OAM:
		if 80 < gpu.dots do break
		gpu.mode = .Draw
		gpu.dots -= 80
	case .Draw:
		if 172 < gpu.dots do break

		gpu.mode = .HBlank
		draw_scanline(gpu.scanline)

		gpu.dots -= 172
	}
}

draw_scanline :: proc(line: u8) -> (ok: bool) {
	if 0 < line && line < SCANLINES {
		return
	}

	if .LCD_PPU_Enable in gpu.controll do return

	pixels: [SCREEN_WIIDTH]u8
	// bottom := u16(gpu.scroll_y + 143) % u16(256)
	// right := u16(gpu.scroll_x + 159) % u16(256)

	is_drawing_allowed := .Background_Window_Enable in gpu.controll

	if is_drawing_allowed && .Window_Enable in gpu.controll {
		win_offsett := 0x1C00 if .Window_Area_Offset in gpu.controll else 0x1800
		win_tile_ids := vram[u16(win_offsett) + (u16(line) * SCREEN_WIIDTH):][:SCREEN_WIIDTH]
		win_tile_data: [SCREEN_WIIDTH]u8
		for id, i in win_tile_ids {
			if .Tiledata_Offset in gpu.controll {
				win_tile_data[i] = vram[0x1000 + u16(id)]
			} else {
				win_tile_data[i] = vram[i16(0x1000) + i16(id)]
			}
		}
	}


	if .OBJ_Enable in gpu.controll {
		objects: [10]Object
		j := 0
		for i in 0 ..< 40 {
			id := i * 32

			if oam[id] != line do continue

			object := &objects[j]

			object.pos.y = oam[id]
			object.pos.x = oam[id + 1]
			object.tile_index = oam[id + 2]
			object.flags = transmute(bit_set[ObjectFlags;u8])oam[id]

			tile := get_tile(objects[j].tile_index, .UNSIGNED)

			palette := .DMG_Pallet in object.flags ? sprite_palettes[1] : sprite_palettes[0]

			line_data := tile_to_line(tile, line, palette)

			for color, x in line_data {
				framebuffer[object.pos.y * SCREEN_WIIDTH + (object.pos.x + u8(x))] = color
			}

			if j == 9 do break
			j += 1
		}
	}


	is_drawing_allowed = .Background_Window_Enable in gpu.controll
	if is_drawing_allowed {
		// win_tile_map_area := 0x9C00 if .Win_Tile_Map_Area in gpu.lcdc else 0x9800
		bg_y := read_u8(Address(Hardware_Registers.LY)) + gpu.scroll_y

		bg_offset := 0x1C00 if .Background_Area_Offset in gpu.controll else 0x1800
		bg_tile_ids := vram[u16(bg_offset) + (u16(line) * SCREEN_WIIDTH):][:SCREEN_WIIDTH]

		display_x: u8 = SCREEN_WIIDTH - 1

		bg_x := display_x + gpu.scroll_x
		// x >> 3 == x / 8
		idx := vram[u16(bg_offset) + u16(bg_x >> 3)]

		bg_tile_data: [SCREEN_WIIDTH]u8
		for id, i in bg_tile_ids {
			if .Tiledata_Offset in gpu.controll {
				bg_tile_data[i] = vram[0x8000 + u16(id)]
			} else {
				bg_tile_data[i] = vram[i32(0x9000) + i32(id)]
			}
		}

	}

	return true
}

TileData :: distinct [16]u8
Tile :: distinct [64]Color

get_tile :: proc(tile_id: u8, $mode: enum {
		SIGNED,
		UNSIGNED,
	}) -> TileData {
	when mode == .UNSIGNED {
		idx := 0x0000 + u16(tile_id) * 16
	} else when mode == .SIGNED {
		idx := 0x1000 + i16(tile_id) * 16
	}

	return transmute(TileData)(vram[idx:][:16])
}

tile_to_line :: proc(tile: TileData, line: u8, palette: Palette) -> [8]Color {
	left := tile[line * 2]
	right := tile[line * 2 + 1]

	line: [8]Color

	#unroll for x in 0 ..< 8 {
		low := (left >> u8(x)) & 1
		high := (right >> u8(x)) & 1
		line[x] = palette[(high << 1) + low]
	}

	return line
}


create_bitmap :: proc() {}

