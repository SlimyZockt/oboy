package main

import "core:encoding/json"
import "core:fmt"
import "core:log"
import "core:os"

main :: proc() {
	context.logger = log.create_console_logger(.Debug)

	data, ok := os.read_entire_file_from_filename("./opcodes.json")
	if !ok {
		fmt.eprintln("Failed to load json file")
	}
	defer delete(data)
	j_data, err := json.parse(data)
	if err != .None {
		fmt.eprintln("Failed to parse the json file.")
		fmt.eprintln("Error:", err)
		return
	}
	defer json.destroy_value(j_data)

	instruction := new(Instruction)
	root := j_data.(json.Object)

	unprefixed := root["unprefixed"].(json.Object)

	instructions_json, parse_err := parse_json(unprefixed)

	test := make([dynamic]i32, 10)

	test[0] = 1

}

Instruction_Flags :: enum {
	None,
	Zero,
	One,
	C,
	H,
	N,
	Z,
}

Word :: u8
Address :: u16

Register :: struct #raw_union {
	full:   u16,
	single: struct {
		lower, upper: u8,
	},
}

Cpu :: struct {
	memory:    [0xFFFF]Word,
	registers: struct {
		AF: Register,
		BC: Register,
		DE: Register,
		HL: Register,
		PC: Address,
		SP: Address,
	},
}

Operand :: struct {
	name:      string,
	immediate: bool,
	bytes:     Maybe(u8),
	Modifier:  enum {
		None,
		Decrement,
		Increment,
	},
}

Operand_Names :: enum {
	VEC_00,
	VEC_08,
	VEC_10,
	VEC_18,
	VEC_20,
	VEC_28,
	VEC_30,
	VEC_38,
	// 0 ,
	// 1 ,
	// 2 ,
	// 3 ,
	// 4 ,
	// 5 ,
	// 6 ,
	// 7 ,
	// A ,
	AF,
	B,
	BC,
	C,
	D,
	DE,
	E,
	H,
	HL,
	L,
	NC,
	NZ,
	SP,
	Z,
	a16,
	a8,
	e8,
	n16,
	n8,
}


Instruction :: struct {
	mnemonic:  string,
	bytes:     u8,
	operands:  [dynamic]Operand,
	immediate: Maybe(bool),
	flags:     bit_set[Instruction_Flags],
}

Instruction_Error :: enum {
	None,
}

parse_json :: proc(root: json.Object) -> (instructions: [dynamic]Instruction, err: json.Error) {
	instructions = make([dynamic]Instruction, len(root))
	fmt.printfln("root len : %d", len(root))
	i := 0
	for key in root {
		val := root[key].(json.Object)
		instructions[i].bytes = u8(val["bytes"].(json.Float))
		instructions[i].mnemonic = val["mnemonic"].(json.String)


		json_operands := val["operands"].(json.Array)
		instructions[i].operands = make([dynamic]Operand, len(json_operands))
		operands := &instructions[i].operands
		for operand, i in json_operands {
			operand := operand.(json.Object)

			bytes := operand["bytes"]
			operands[i].bytes = nil if bytes == nil else u8(bytes.(json.Float))

			operands[i].Modifier = .None

			if operand["increment"] != nil {
				operands[i].Modifier = .Increment
			} else if operand["decrement"] != nil {
				operands[i].Modifier = .Decrement
			}


		}


		// log.debug(instructions[i])
		i += 1
	}


	return instructions, .None

}

generate_instruction :: proc(instruction_json: json.Value) -> (^Instruction, Instruction_Error) {


	return nil, .None
}
