package main

import "core:log"

Color :: distinct [3]u8
Palette :: distinct [4]Color

SCREEN_WIDTH :: 160
SCREEN_HIGHT :: 144
SCANLINES :: 154

@(rodata)
DEFAULT_PALETTE := Palette{{255, 255, 255}, {192, 192, 192}, {96, 96, 96}, {0, 0, 0}}

bg_palette := DEFAULT_PALETTE
sprite_palettes := [2]Palette{DEFAULT_PALETTE, DEFAULT_PALETTE}

Framebuffer :: [SCREEN_HIGHT * SCREEN_WIDTH]Color
framebuffer: Framebuffer

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

	@(static) pre_ticks: u64 = 0

	gpu.dots += u64(i64(cpu.ticks) - i64(pre_ticks))

	pre_ticks = cpu.ticks

	// log.warn(gpu.dots)

	switch gpu.mode {
	case .HBlank:
		if gpu.dots < 204 do break
		log.info(gpu.mode)
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
		if gpu.dots < 456 do break
		log.info(gpu.mode)

		gpu.scanline += 1

		if gpu.scanline == 154 {
			gpu.scanline = 0
			gpu.mode = .OAM
		}

		gpu.dots -= 456

	case .OAM:
		if gpu.dots < 80 do break
		log.warn(80 < gpu.dots)
		gpu.mode = .Draw
		gpu.dots -= 80
	case .Draw:
		if gpu.dots < 172 do break
		log.warn(gpu.mode)

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

	// bottom := u16(gpu.scroll_y + 143) % u16(256)
	// right := u16(gpu.scroll_x + 159) % u16(256)

	horizontal_offset: Address = (((Address(line) + Address(gpu.scroll_y)) & 255) >> 3) << 5

	is_drawing_allowed := .Background_Window_Enable in gpu.controll
	if is_drawing_allowed {
		// win_tile_map_area := 0x9C00 if .Win_Tile_Map_Area in gpu.lcdc else 0x9800
		bg_offset: Address = 0x1C00 if .Background_Area_Offset in gpu.controll else 0x1800
		bg_offset += horizontal_offset
		bg_tile_ids := vram[bg_offset:][:SCREEN_WIDTH]

		for id, x in bg_tile_ids {
			tile := get_tile(id, .Tiledata_Offset in gpu.controll)

			display_pos := Vec2{u8((id << 5) + u8(x)), line}

			insert_tile_in_framebuffer(&framebuffer, display_pos, tile, bg_palette)
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

			tile := get_tile(objects[j].tile_index, false)

			palette := .DMG_Pallet in object.flags ? sprite_palettes[1] : sprite_palettes[0]

			insert_tile_in_framebuffer(&framebuffer, object.pos, tile, palette)

			if j == 9 do break
			j += 1
		}
	}

	if is_drawing_allowed && .Window_Enable in gpu.controll {
		win_offset: Address = 0x1C00 if .Window_Area_Offset in gpu.controll else 0x1800
		win_offset += horizontal_offset
		win_tile_ids := vram[win_offset:][:SCREEN_WIDTH]

		for id, x in win_tile_ids {
			tile := get_tile(id, .Tiledata_Offset in gpu.controll)

			display_pos := Vec2{u8((id << 5) + u8(x)), line}

			insert_tile_in_framebuffer(&framebuffer, display_pos, tile, bg_palette)
		}
	}


	return true
}

TileData :: distinct [16]u8
Tile :: distinct [64]Color

get_tile :: proc(tile_id: u8, singed: bool) -> TileData {
	offset: i16 = singed ? 0x0000 : 0x1000

	idx := offset + i16(tile_id) * 16
	return transmute(TileData)(vram[idx:][:16])
}

insert_tile_in_framebuffer :: proc(
	framebuffer: ^Framebuffer,
	pos: Vec2,
	tile: TileData,
	palette: Palette,
) {
	left := tile[pos.y * 2]
	right := tile[pos.y * 2 + 1]
	// line: [8]Color

	#unroll for x in 0 ..< 8 {
		low := (left >> u8(x)) & 1
		high := (right >> u8(x)) & 1
		framebuffer[(u8(x) + pos.x) + (pos.y * SCREEN_WIDTH)] = palette[(high << 1) + low]
	}

}

