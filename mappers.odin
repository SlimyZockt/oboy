package main

Cartridge_Features :: enum {
	Rom,
	MBC1,
	Ram,
	Battery,
	MBC2,
	MMM01,
	Timer,
	MBC3,
	MBC5,
	MBC6,
	MBC7,
	Rumble,
	Sensor,
	Pocket_Camera,
	Bandai_tama5,
	HuC3,
	HuC1,
}

Cartridge_Type :: bit_set[Cartridge_Features;u32]

ROM_Size :: enum u8 {
	Rom_32KiB,
	Rom_64KiB,
	Rom_128KiB,
	Rom_256KiB,
	Rom_512KiB,
	Rom_1MiB,
	Rom_2MiB,
	Rom_4MiB,
	Rom_8MiB,
}

//Don't change the order
RAM_Size :: enum u8 {
	Ram_None,
	Ram_Unused,
	Ram_8KiB,
	Ram_32KiB,
	Ram_128KiB,
	Ram_64KiB,
}

MapperFeatures :: enum u8 {
	RAM_Enabled,
	Advanced_Mode,
}

Mapper :: struct {
	write:              proc(mapper: ^Mapper, address: Address, value: u8),
	read:               proc(mapper: ^Mapper, address: Address) -> u8,
	ram_write:          proc(mapper: ^Mapper, address: Address, value: u8),
	ram_read:           proc(mapper: ^Mapper, addres: Address) -> u8,
	features:           bit_set[MapperFeatures;u8],
	mode:               enum u8 {
		ROM,
		MBC1,
	},
	ram_bank:           u8,
	rom_bank:           u8,
	cartridge_features: Cartridge_Type,
	rom:                []u8,
	rom_size:           ROM_Size,
	ram_size:           RAM_Size,
	ram:                [dynamic]u8,
}

@(rodata)
ram_lookup := [len(RAM_Size)]u32{0, 2048, 8192, 32768, 131072, 65536}

@(rodata)
cartridge_lookup: [28]struct {
	code: u8,
	type: Cartridge_Type,
} =
	{{0x00, {.Rom}}, {0x01, {.MBC1}}, {0x02, {.MBC1, .Ram}}, {0x03, {.MBC1, .Ram, .Battery}}, {0x05, {.MBC2}}, {0x06, {.MBC2, .Battery}}, {0x08, {.Rom, .Ram}}, {0x09, {.Rom, .Ram, .Battery}}, {0x0B, {.MMM01}}, {0x0C, {.MMM01, .Ram}}, {0x0D, {.MMM01, .Ram, .Battery}}, {0x0F, {.MBC3, .Timer, .Battery}}, {0x10, {.MBC3, .Timer, .Ram, .Battery}}, {0x11, {.MBC3}}, {0x12, {.MBC3, .Ram}}, {0x13, {.MBC3, .Ram, .Battery}}, {0x19, {.MBC5}}, {0x1a, {.MBC5, .Ram}}, {0x1b, {.MBC5, .Ram, .Battery}}, {0x1c, {.MBC5, .Rumble}}, {0x1d, {.MBC5, .Rumble, .Ram}}, {0x1d, {.MBC5, .Rumble, .Ram, .Battery}}, {0x1d, {.MBC6}}, {0x1d, {.MBC7, .Sensor, .Rumble, .Ram, .Battery}}, {0x1d, {.Pocket_Camera}}, {0x1d, {.Bandai_tama5}}, {0x1d, {.HuC3}}, {0x1d, {.HuC1, .Ram, .Battery}}}

@(private)
get_cartridge_type :: proc(code: u8) -> Cartridge_Type {
	for type in cartridge_lookup {
		if type.code == code {
			return type.type
		}
	}
	panic("Cartridge type not Supported")
}

new_mapper :: proc(rom: []u8, allocator := context.allocator) -> ^Mapper {
	mapper := new(Mapper, allocator)
	mapper.rom = rom
	mapper.rom_size = ROM_Size(mapper.rom[0x0148])
	mapper.ram_size = RAM_Size(mapper.rom[0x0149])
	mapper.cartridge_features = get_cartridge_type(mapper.rom[0x0147])
	mapper.ram = make([dynamic]u8, ram_lookup[mapper.ram_size])

	switch {
	case .Rom in mapper.cartridge_features:
		{ 	// Setup Roms
			mapper.mode = .ROM
			mapper.read = proc(mapper: ^Mapper, address: Address) -> u8 {
				return mapper.rom[address]
			}

			mapper.write = proc(mapper: ^Mapper, address: Address, value: u8) {}

			mapper.ram_read = proc(mapper: ^Mapper, address: Address) -> u8 {
				return mapper.ram[address - 0xA000]
			}

			mapper.ram_write = proc(mapper: ^Mapper, address: Address, value: u8) {
				mapper.ram[address - 0xA000] = value
			}

		}
	case .MBC1 in mapper.cartridge_features:
		{ 	// Setup MBC1
			mapper.mode = .MBC1

			calc_address :: proc(mapper: ^Mapper, address: Address) -> u32 {
				address := u32(address)
				high_map := u32(mapper.ram_bank)
				switch mapper.rom_size {
				case .Rom_32KiB, .Rom_64KiB, .Rom_128KiB, .Rom_256KiB, .Rom_512KiB:
				case .Rom_1MiB:
					high_map = 0x00
				case .Rom_2MiB:
					high_map &= 0x01
				case .Rom_4MiB:
					if high_map > 0x10 do high_map = 0x10
				case .Rom_8MiB:
				}

				if address <= 0x3FFF {
					if .Advanced_Mode in mapper.features {
						return (address & 0x3fff) | (high_map << 19)
					}

					return address
				}

				address = (address & 0x3fff) | (high_map << 19) | (u32(mapper.rom_bank) << 14)
				if address < 0x4000 do address += 0x4000

				return address
			}

			mapper.read = proc(mapper: ^Mapper, address: Address) -> u8 {
				return mapper.rom[calc_address(mapper, address)]
			}

			mapper.write = proc(mapper: ^Mapper, address: Address, value: u8) {
				value := value
				switch address {
				case 0x0000 ..= 0x1FFF:
					if value & 0x0A == 0x0A {
						mapper.features += {.RAM_Enabled}
					} else {
						mapper.features -= {.RAM_Enabled}
					}
				case 0x2000 ..= 0x3FFF:
					value = value & 0b0001_1111
					if value == 0 {
						mapper.rom_bank = 1
						return
					}
					mapper.rom_bank = value & (u8(mapper.rom_size) + 1)
					if mapper.rom_size >= .Rom_1MiB {
						mapper.rom_bank += (mapper.ram_bank << 5)
					}
					mapper.rom_bank = value
				case 0x4000 ..= 0x5FFF:
					if mapper.rom_size >= .Rom_1MiB || mapper.ram_size >= .Ram_64KiB {
						mapper.ram_bank = value & 0b0000_0011
					}
				case 0x6000 ..= 0x7FFF:
					if value & 0x01 == 0 {
						mapper.features -= {.Advanced_Mode}
					} else {
						mapper.features += {.Advanced_Mode}
					}
				}

			}

			mapper.ram_read = proc(mapper: ^Mapper, address: Address) -> u8 {
				address := u32(address - 0xA000)
				if .RAM_Enabled not_in mapper.features do return 0xFF
				if mapper.ram_size <= .Ram_Unused do return 0xFF
				if .Advanced_Mode not_in mapper.features do return mapper.ram[address]

				high_map := u32(mapper.ram_bank)
				#partial switch mapper.ram_size {
				case .Ram_8KiB:
					high_map = 0x00
				case .Ram_32KiB:
					high_map &= 0x01
				case .Ram_128KiB:
					if high_map > 0x10 do high_map = 0x10
				case .Ram_64KiB:
				}

				address = (address & 0x3fff) | (high_map << 13)

				return mapper.ram[address]
			}

			mapper.ram_write = proc(mapper: ^Mapper, address: Address, value: u8) {
				address := u32(address - 0xA000)
				if .RAM_Enabled not_in mapper.features do return
				if .Advanced_Mode not_in mapper.features do mapper.ram[address] = value

				high_map := u32(mapper.ram_bank)
				#partial switch mapper.ram_size {
				case .Ram_8KiB:
					high_map = 0x00
				case .Ram_32KiB:
					high_map &= 0x01
				case .Ram_128KiB:
					if high_map > 0x10 do high_map = 0x10
				case .Ram_64KiB:
				}

				address = (address & 0x3fff) | (high_map << 13)
				mapper.ram[address] = value
			}
		}
	case:
	}

	return mapper
}
