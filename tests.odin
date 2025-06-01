#+feature dynamic-literals
package main

import "core:log"
import "core:math/bits"
import "core:testing"


@(test)
test_full_rotate_left_carry :: proc(t: ^testing.T) {

	reg: u8 = 0xF0
	flags: bit_set[Flags;u8]

	for i in 0 ..= 8 {
		rotate_left_includes_carry(&reg, &flags)
	}

	testing.expect(t, reg == 0xF0)
}

@(test)
test_full_rotate_left :: proc(t: ^testing.T) {

	reg: u8 = 0xF0

	for i in 0 ..< 8 {
		rotate_left(&reg)
	}

	testing.expect(t, reg == 0xF0)
}

@(test)
test_full_rotate_right :: proc(t: ^testing.T) {

	reg: u8 = 0xF0

	for i in 0 ..< 8 {
		rotate_right(&reg)
	}

	testing.expect(t, reg == 0xF0)
}

@(test)
test_half_rotate_left_carry :: proc(t: ^testing.T) {
	reg: u8 = 0xF0
	flags: bit_set[Flags;u8]

	for i in 0 ..= 4 {
		rotate_left_includes_carry(&reg, &flags)
	}

	testing.expect(t, reg == 0x0F)
}

@(test)
test_full_rotate_right_carry :: proc(t: ^testing.T) {
	reg: u8 = 0xF0
	flags: bit_set[Flags;u8]

	for i in 0 ..= 8 {
		rotate_right_includes_carry(&reg, &flags)
	}

	testing.expect(t, reg == 0xF0)
}

@(test)
test_half_rotate_right_carry :: proc(t: ^testing.T) {
	reg: u8 = 0xF0
	flags: bit_set[Flags;u8]

	for i in 0 ..= 4 {
		rotate_right_includes_carry(&reg, &flags)
	}

	testing.expect(t, reg == 7)
}

@(test)
test_u16_to_i8 :: proc(t: ^testing.T) {
	u: u8 = 0x80
	i: i16 = 0xFF

	log.infof("%b", u)
	log.info(u16(i - i16(transmute(i8)u)))
}

