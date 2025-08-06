#+feature dynamic-literals
package main

import "core:log"
import "core:testing"

@(test)
test_u16_to_i8 :: proc(t: ^testing.T) {
	u: u8 = 0x80
	i: i16 = 0xFF

	log.infof("%b", u)
	log.info(u16(i - i16(u)))
}


@(test)
test_bit_set :: proc(t: ^testing.T) {
	set := bit_set[TestE]{.A, .B}

	set ~= {.C}

	testing.expect(t, set == {.A, .B, .C})


	set ~= {.C}
	testing.expect(t, set == {.A, .B})
}


TestE :: enum {
	A,
	B,
	C,
}

t_err :: proc() -> (ok: bool) {
	return
}

@(test)
test_t :: proc(t: ^testing.T) {
	log.infof("out: %v", t_err())
	testing.expect(t, t_err() == false)

}

@(test)
test_switch_scope :: proc(t: ^testing.T) {
	e: TestE = .A

	// num := {
	// 	return 0
	// }

	num := 0
	// {
	// 	num := 0
	// }

	log.info(num)
	switch e {
	case .A:
		@(static) i := 0
	case .B:
		i := 0
		log.info(i)
	case .C:
	}
}


T_Struct :: struct {
	using _: struct #raw_union {
		AF:      u16,
		using _: struct {
			F: bit_set[Flags;u8],
			A: u8,
		},
	},
}

@(test)
testing_using :: proc(t: ^testing.T) {
	t: T_Struct
	t.AF = 0xFFAA

	log.debugf("%04X", t.AF)
	log.debugf("%04X", t.F)
	log.debugf("%04X", t.A)
}

