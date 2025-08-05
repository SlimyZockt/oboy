package main

import "core:fmt"

Color :: distinct [3]u8
Palette :: distinct [4]Color

SCREEN_WIDTH :: 160
SCREEN_HEIGHT :: 144
SCANLINES :: 154

@(rodata)
DEFAULT_PALETTE := Palette{{255, 255, 255}, {192, 192, 192}, {96, 96, 96}, {0, 0, 0}}

bg_palette := DEFAULT_PALETTE
sprite_palettes := [2]Palette{DEFAULT_PALETTE, DEFAULT_PALETTE}

Framebuffer :: [SCREEN_HEIGHT * SCREEN_WIDTH]Color
framebuffer: Framebuffer

// dots
ObjectFlags :: enum u8 {
	_,
	_,
	_,
	Blank,
	DMG_Pallet,
	X_Filp,
	Y_Filp,
	Priority,
}

Vec2 :: distinct [2]u8

TileData :: distinct [16]u8
Color_Index :: u8
Tile :: distinct [64]Color_Index
Object :: struct {
	tile_index: u8,
	flags:      bit_set[ObjectFlags;u8],
	pos:        Vec2,
}


tiles: [384]Tile

step_gpu :: proc() {

	@(static) pre_ticks: u64 = 0

	gpu.dots += u64(i64(cpu.ticks) - i64(pre_ticks))

	pre_ticks = cpu.ticks

	// log.warn(gpu.dots)

	switch gpu.mode {
	case .HBlank:
		if gpu.dots < 204 do break
		gpu.scanline += 1

		if gpu.scanline == 143 {
			if .VBlank in cpu.interrupt.enable {
				cpu.interrupt.flags += {.VBlank}
				assert(.VBlank in cpu.interrupt.enable)
				assert(.VBlank in cpu.interrupt.flags)
			}


			gpu.mode = .VBlank
		} else {
			gpu.mode = .OAM
		}

		gpu.dots -= 204
	case .VBlank:
		if gpu.dots < 456 do break

		gpu.scanline += 1

		if gpu.scanline > 153 {
			gpu.scanline = 0
			gpu.mode = .OAM
		}

		gpu.dots -= 456

	case .OAM:
		if gpu.dots < 80 do break
		gpu.mode = .Draw
		gpu.dots -= 80
	case .Draw:
		if gpu.dots < 172 do break

		gpu.mode = .HBlank
		draw_scanline(gpu.scanline)


		gpu.dots -= 172
	}
}


update_tile :: proc(address: Address) {
	address := address

	address = address & 0x1ffe
	tile := u16(address >> 4)
	y := (address >> 1) & 7

	#unroll for x in 0 ..< 8 {
		bit_index: u8 = 1 << (7 - u8(x))

		msb: u8 = (vram[address] & bit_index != 0) ? 1 : 0
		lsb: u8 = (vram[address + 1] & bit_index != 0) ? 2 : 0

		// fmt.printfln("Tile Data %v: %v ", u16(y * 8) + u16(x), (msb + lsb))
		tiles[tile][u16(y * 8) + u16(x)] = (msb + lsb)
	}

	fmt.printfln("Tile %v: %v", tile, tiles[tile])
}

draw_scanline :: proc(line: u8) {
	assert(0 <= line && line < SCANLINES)

	// if .LCD_PPU_Enable in gpu.controll do return


	{ 	// get_tiles
		tile_map_offset: u16 = 0x1C00 if .Background_Area_Offset in gpu.controll else 0x1800
		tile_map_offset += ((u16(u16(gpu.scanline) + u16(gpu.scroll_y)) & 0xFF) >> 3) << 5


		line_offset := u16(gpu.scroll_x) >> 3

		y := u8(u16(line + gpu.scroll_y) & 0b0000_0111)
		x := gpu.scroll_x & 0b0000_0111

		tile_index := vram[tile_map_offset + line_offset]
		tile_data_offset: u16 = .Tiledata_Offset in gpu.controll ? 0x0000 : 0x0800

		pixel_offset := int(gpu.scanline) * SCREEN_WIDTH


		for _ in 0 ..< 160 {
			color := tiles[tile_index][y * 8 + x]

			framebuffer[pixel_offset] = bg_palette[color]

			pixel_offset += 1
			x += 1
			if (x == 8) {
				x = 0
				line_offset = (line_offset + 1) & 31
				tile_index = vram[tile_map_offset + line_offset]
			}
		}


	}

	// horizontal_offset: Address = (((Address(line) + Address(gpu.scroll_y)) & 255) >> 3) << 5
	// log.debugf("OFFSET: 0x%04X", horizontal_offset)
	//
	// is_drawing_allowed := .Background_Window_Enable in gpu.controll
	// if is_drawing_allowed {
	// 	// win_tile_map_area := 0x9C00 if .Win_Tile_Map_Area in gpu.lcdc else 0x9800
	// 	bg_offset: Address = 0x1C00 if .Background_Area_Offset in gpu.controll else 0x1800
	// 	bg_offset += horizontal_offset
	// 	log.debugf("BACKGROUND ADDRESS: 0x%04X", bg_offset)
	// 	bg_tile_ids := vram[bg_offset:][:SCREEN_WIDTH >> 3]
	//
	// 	for id, x in bg_tile_ids {
	// 		tile := get_tile(id, .Tiledata_Offset in gpu.controll)
	//
	// 		display_pos := Vec2{u8((id << 5) + u8(x)), line}
	//
	// 		insert_tile_in_framebuffer(&framebuffer, display_pos, tile, bg_palette)
	// 	}
	//
	// 	log.warn(bg_tile_ids)
	// }
	//
	// if .OBJ_Enable in gpu.controll {
	// 	objects: [10]Object
	// 	j := 0
	// 	for i in 0 ..< 40 {
	// 		id := i * 4
	//
	// 		// log.debugf("OAM ADDRESS: 0x%04X", id)
	//
	// 		if oam[id] != line do continue
	//
	// 		object := &objects[j]
	//
	// 		object.pos.y = oam[id]
	// 		object.pos.x = oam[id + 1]
	// 		object.tile_index = oam[id + 2]
	// 		object.flags = transmute(bit_set[ObjectFlags;u8])oam[id + 3]
	//
	// 		tile := get_tile(objects[j].tile_index, false)
	//
	// 		palette := .DMG_Pallet in object.flags ? sprite_palettes[1] : sprite_palettes[0]
	//
	// 		insert_tile_in_framebuffer(&framebuffer, object.pos, tile, palette)
	//
	// 		if j == 9 do break
	// 		j += 1
	// 	}
	//
	// }
	//
	// if is_drawing_allowed || .Window_Enable in gpu.controll {
	// 	win_offset: Address = 0x1C00 if .Window_Area_Offset in gpu.controll else 0x1800
	// 	win_offset += horizontal_offset
	//
	// 	log.debugf("WINDOW ADDRESS: 0x%04X", win_offset)
	// 	win_tile_ids := vram[win_offset:][:SCREEN_WIDTH >> 3]
	//
	// 	for id, x in win_tile_ids {
	// 		tile := get_tile(id, .Tiledata_Offset in gpu.controll)
	//
	// 		display_pos := Vec2{u8((id << 5) + u8(x)), line}
	//
	// 		insert_tile_in_framebuffer(&framebuffer, display_pos, tile, bg_palette)
	// 	}
	//
	// 	log.warn(win_tile_ids)
	// }
	//
	//
	// return true
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

		// log.warn(palette[(high << 1) + low])
		framebuffer[(u8(x) + pos.x) + (pos.y * SCREEN_WIDTH)] = palette[(high << 1) + low]
	}

}

