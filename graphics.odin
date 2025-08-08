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

		msb: u8 = (memory.vram[address] & bit_index != 0) ? 1 : 0
		lsb: u8 = (memory.vram[address + 1] & bit_index != 0) ? 2 : 0

		// fmt.printfln("Tile Data %v: %v ", u16(y * 8) + u16(x), (msb + lsb))
		tiles[tile][u16(y * 8) + u16(x)] = (msb + lsb)
	}

	fmt.printfln("Tile %v: %v", tile, tiles[tile])
}

draw_scanline :: proc(line: u8) {
	assert(0 <= line && line < SCANLINES)

	if .LCD_PPU_Enable not_in gpu.controll do return

	// gpu.controll = {.Window_Enable}
	scanline_row: [160]Color_Index

	if .Background_Window_Enable in gpu.controll { 	// draw bg and win
		bg_tilemap: u16 = 0x1C00 if .Background_Area_Offset in gpu.controll else 0x1800
		bg_tilemap += ((u16(u16(gpu.scanline) + u16(gpu.scroll_y)) & 0xFF) >> 3) << 5


		win_tilemap: u16 = 0x1C00 if .Background_Area_Offset in gpu.controll else 0x1800

		line_offset := u16(gpu.scroll_x) >> 3

		bg_y := u8(u16(line + gpu.scroll_y) & 0b0000_0111)
		bg_x := gpu.scroll_x & 0b0000_0111

		tile_data_offset: u8 = .Tiledata_Offset in gpu.controll ? 0 : 128
		bg_tile_index := memory.vram[bg_tilemap + line_offset] + tile_data_offset


		win_tile_index := memory.vram[bg_tilemap + line_offset] + tile_data_offset

		pixel_offset := int(gpu.scanline) * SCREEN_WIDTH

		for x in 0 ..< 160 {
			color := tiles[bg_tile_index][bg_y * 8 + bg_x]

			scanline_row[x] = color

			framebuffer[pixel_offset] = bg_palette[color]

			pixel_offset += 1
			bg_x += 1
			if (bg_x == 8) {
				bg_x = 0
				line_offset = (line_offset + 1) & 31
				tile_data_offset = .Tiledata_Offset in gpu.controll ? 0 : 128
				bg_tile_index = memory.vram[bg_tilemap + line_offset] + tile_data_offset
			}

			if .Window_Enable not_in gpu.controll do continue
		}
	}

	// if .OBJ_Enable not_in gpu.controll do return
	for i in 0 ..< 40 {
		id := i * 4
		// object := Object {
		// 	memory.oam[id],
		// 	memory.oam[id + 1],
		// 	memory.oam[id + 2],
		// 	transmute(bit_set[ObjectFlags;u8])memory.oam[id + 3],
		// }

		object := cast(^Object)(&memory.oam[id])

		sp_x := object.x - 8
		sp_y := object.y - 16

		if !(sp_y <= line && (sp_y + 8) > line) do continue


		palette := sprite_palettes[u8(.DMG_Pallet in object.flags)]


		y: u8 = .Y_Filp in object.flags ? 7 - u8(line - sp_y) : u8(line - sp_y)

		if object.tile_index == 255 do continue
		if object.x == 255 do continue
		if object.y == 255 do continue

		for x in 0 ..< 8 {
			sx := (sp_x + u8(x))

			can_draw := sx >= 0
			can_draw &&= sx < SCREEN_WIDTH
			can_draw &&= (.Priority not_in object.flags || scanline_row[sx] == 0)
			if !can_draw do continue

			tile := tiles[object.tile_index]
			color := .X_Filp in object.flags ? tile[(y * 8) + (7 - u8(x))] : tile[(y * 8) + u8(x)]
			if color == 0 do continue

			dst := (u32(line) * SCREEN_WIDTH) + u32(sx)
			framebuffer[dst] = palette[color]
		}
	}

}

