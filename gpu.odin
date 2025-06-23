package main

Color :: distinct [3]u8
Palette :: distinct [4]Color

@(rodata)
palette := Palette{{255, 255, 255}, {192, 192, 192}, {96, 96, 96}, {0, 0, 0}}

bg_palette := palette

sprite_palettes := [2]Palette{palette, palette}

