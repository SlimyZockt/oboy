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
	y:          u8,
	x:          u8,
	tile_index: u8,
	flags:      bit_set[ObjectFlags;u8],
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

		if gpu.scanline >= 144 {
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

		low: u8 = (memory.vram[address] & bit_index != 0) ? 1 : 0
		high: u8 = (memory.vram[address + 1] & bit_index != 0) ? 2 : 0

		// fmt.printfln("Tile Data %v: %v ", u16(y * 8) + u16(x), (msb + lsb))
		tiles[tile][u16(y * 8) + u16(x)] = (low + high)
	}

	// Debug print removed to avoid excessive logging
	// fmt.printfln("Tile %v: %v", tile, tiles[tile])
}

draw_scanline :: proc(line: u8) {
	assert(0 <= line && line < SCANLINES)

	if .LCD_PPU_Enable not_in gpu.controll do return

	// gpu.controll = {.Window_Enable}
	scanline_row: [160]Color_Index

	if .Background_Window_Enable in gpu.controll { 	// draw bg and win
		bg_tilemap: u16 = 0x1C00 if .Background_Area_Offset in gpu.controll else 0x1800
		bg_tilemap += ((u16(u16(gpu.scanline) + u16(gpu.scroll_y)) & 0xFF) >> 3) << 5

		win_tilemap: u16 = 0x1C00 if .Window_Area_Offset in gpu.controll else 0x1800

		line_offset := u16(gpu.scroll_x) >> 3

		bg_y := u8(u16(line + gpu.scroll_y) & 0b0000_0111)
		bg_x := gpu.scroll_x & 0b0000_0111

		use_unsigned_tile_ids := (.Tiledata_Offset in gpu.controll)
		bg_tile_id := memory.vram[bg_tilemap + line_offset]
		bg_tile_index: u16
		if use_unsigned_tile_ids {
			bg_tile_index = u16(bg_tile_id)
		} else {
			bg_tile_index = u16(256 + i16(i8(bg_tile_id)))
		}

		pixel_offset := int(line) * SCREEN_WIDTH

		// Precompute window state for this scanline
		win_active := (.Window_Enable in gpu.controll) && (line >= gpu.win_y)
		wx_start := i16(gpu.win_x) - 7
		if wx_start < 0 do wx_start = 0
		win_line := u16(u16(line) - u16(gpu.win_y))
		win_row := (win_line >> 3) & 31
		win_tilemap_row := (win_tilemap + (win_row << 5))

		for x in 0 ..< 160 {
			color := tiles[bg_tile_index][bg_y * 8 + bg_x]

			scanline_row[x] = color

			framebuffer[pixel_offset] = bg_palette[color]

			pixel_offset += 1
			bg_x += 1
			if (bg_x == 8) {
				bg_x = 0
				line_offset = (line_offset + 1) & 31
				bg_tile_id = memory.vram[bg_tilemap + line_offset]
				if use_unsigned_tile_ids {
					bg_tile_index = u16(bg_tile_id)
				} else {
					bg_tile_index = u16(256 + i16(i8(bg_tile_id)))
				}
			}

			// Window drawing overlays BG
			if !win_active do continue
			if i16(x) < wx_start do continue

			// Compute window tile position
			x_in_win := u16(i16(x) - wx_start)
			win_col := (x_in_win >> 3) & 31
			win_x_in_tile := u8(x_in_win & 7)
			win_y_in_tile := u8(win_line & 7)
			win_tile_id := memory.vram[win_tilemap_row + win_col]
			win_tile_index: u16
			if use_unsigned_tile_ids {
				win_tile_index = u16(win_tile_id)
			} else {
				win_tile_index = u16(256 + i16(i8(win_tile_id)))
			}

			win_color := tiles[win_tile_index][win_y_in_tile * 8 + win_x_in_tile]
			scanline_row[x] = win_color
			framebuffer[pixel_offset - 1] = bg_palette[win_color]
		}
	}

	if .OBJ_Enable not_in gpu.controll do return
	for i in 0 ..< 40 {
		id := i * 4
		object := cast(^Object)(&memory.oam[id])
		sp_x := i16(object.x) - 8
		sp_y := i16(object.y) - 16
		height: i16 = 16 if .OBJ_Size in gpu.controll else 8
		if !(sp_y <= i16(line) && (sp_y + height) > i16(line)) do continue
		palette := sprite_palettes[u8(.DMG_Pallet in object.flags)]
		if .OBJ_Size in gpu.controll {
			// 8x16 sprites
			y_in_sprite: u8 = u8(i16(line) - sp_y)
			if .Y_Filp in object.flags { y_in_sprite = 15 - y_in_sprite }
			tile_number := (object.tile_index & 0xFE) + u8(y_in_sprite >> 3)
			row_in_tile := y_in_sprite & 7
			tile := tiles[tile_number]
			for x in 0 ..< 8 {
				sx := sp_x + i16(x)
				can_draw := (sx >= 0)
				can_draw &&= (sx < SCREEN_WIDTH)
				can_draw &&= (.Priority not_in object.flags || scanline_row[u8(sx)] == 0)
				if !can_draw do continue
				col := u8(7 - x) if .X_Filp in object.flags else u8(x)
				tile_pos := (row_in_tile * 8) + col
				color := tile[tile_pos]
				if color == 0 do continue
				dst := (u32(line) * SCREEN_WIDTH) + u32(sx)
				framebuffer[dst] = palette[color]
			}
			continue
		}
		tile := tiles[object.tile_index]
		y: u8 = u8(i16(line) - sp_y)
		if .Y_Filp in object.flags { y = 7 - y }
		for x in 0 ..< 8 {
			sx := sp_x + i16(x)
			can_draw := (sx >= 0)
			can_draw &&= (sx < SCREEN_WIDTH)
			can_draw &&= (.Priority not_in object.flags || scanline_row[u8(sx)] == 0)
			if !can_draw do continue
			color := tile[(y * 8) + (7 - u8(x))] if .X_Filp in object.flags else tile[(y * 8) + u8(x)]
			if color == 0 do continue
			dst := (u32(line) * SCREEN_WIDTH) + u32(sx)
			framebuffer[dst] = palette[color]
		}
	}

}
