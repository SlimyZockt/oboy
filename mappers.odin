package main

import "base:intrinsics"

MiB1 :: 1048576

Mapper :: struct {
	write:       proc(mapper: ^Mapper, address: Address, value: u8),
	read:        proc(mapper: ^Mapper, address: Address) -> u8,
	ram_write:   proc(mapper: ^Mapper, address: Address, value: u8),
	ram_read:    proc(mapper: ^Mapper, addres: Address) -> u8,
	huge_mode:   bool,
	ram_enabled: bool,
	upper_bank:  u8,
	lower_bank:  u8,
	ram:         [0x8000]u8,
	mode:        enum {
		ROM,
		MBC1,
	},
}

new_rom :: proc(memory: ^Memory, allocator := context.allocator) -> ^Mapper {
	mapper := new(Mapper, allocator)
	mapper.mode = .ROM
	mapper.read = proc(mapper: ^Mapper, address: Address) -> u8 {
		return memory.cartridge[address]
	}

	mapper.write = proc(mapper: ^Mapper, address: Address, value: u8) {
		// memory.cartridge[address] = value
	}

	mapper.ram_read = proc(mapper: ^Mapper, address: Address) -> u8 {
		return mapper.ram[address - 0xA000]
	}

	mapper.ram_write = proc(mapper: ^Mapper, address: Address, value: u8) {
		mapper.ram[address - 0xA000] = value
	}

	return mapper
}


new_MBC1 :: proc(memory: ^Memory, allocator := context.allocator) -> ^Mapper {
	mapper := new(Mapper, allocator)
	mapper.mode = .MBC1
	if memory.cartridge_size >= MiB1 {
		mapper.huge_mode = true
	}

	calc_address :: proc(address: Address) {

	}

	mapper.read = proc(mapper: ^Mapper, address: Address) -> u8 {
		return 0
	}

	mapper.write = proc(mapper: ^Mapper, address: Address, value: u8) {
		if address <= 0x1FFF && (value & 0x0A) == 0x0A {
			mapper.ram_enabled = true
		}

		if 0x2000 <= address && address <= 0x3FFF {
			bank := value & 0b0001_1111
			if bank == 0 do bank = 1

		}
	}

	mapper.ram_read = proc(mapper: ^Mapper, address: Address) -> u8 {
		return mapper.ram[address - 0xA000]
	}

	mapper.ram_write = proc(mapper: ^Mapper, address: Address, value: u8) {
		if !mapper.ram_enabled do return
	}

	return mapper
}
