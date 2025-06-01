package instruction_codegen

import "core:encoding/json"
import "core:fmt"
import "core:log"
import "core:os"
import "core:slice"
import "core:strings"

OUTPUT_FILE :: "instructions/instruction_definitons.odin"
OPCODE_JSON :: #load("./opcodes.json")


menonics := make([dynamic]string)
operands := make([dynamic]string)
// instructions := make([dynamic]Instruction)

t :: [?]int{3, 3, 5, 7}

main :: proc() {
	json_data, err := json.parse(OPCODE_JSON)
	if err != .None {
		log.error("Failed to parse the json file.")
		log.error(err)
		return
	}
	defer json.destroy_value(json_data)

	root := json_data.(json.Object)

	f, _ := os.open(OUTPUT_FILE, os.O_WRONLY | os.O_CREATE | os.O_TRUNC, 0o644)
	defer os.close(f)


	fmt.fprintln(
		f,
		`package instructions
Operand :: struct {
	name:      Operand_Name,
	immediate: bool,
	bytes:     u8,
	Modifier:  enum {
		None,
		Decrement,
		Increment,
	},
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

Instruction :: struct {
	mnemonic:  Mnemonic,
	opcode:    u16,
	bytes:     u8,
	operands:  []Operand,
	immediate: bool,
	flags:     bit_set[Instruction_Flags],
}`,
	)

	get_instruction_data(root["unprefixed"].(json.Object))
	get_instruction_data(root["cbprefixed"].(json.Object))

	fmt.fprintln(f, `Mnemonic :: enum {`)
	for mnemonic in menonics {
		fmt.fprintfln(f, "%s,", mnemonic)
	}
	fmt.fprintln(f, "}")

	fmt.fprintln(f, `Operand_Name :: enum {`)
	for operand in operands {
		fmt.fprintfln(f, "O_%s,", operand)
	}
	fmt.fprintln(f, "}")

	fmt.fprintln(f, `
@(rodata)
Instructions := [?]Instruction{`)
	gen_instruction_data(f, root["unprefixed"].(json.Object))
	gen_instruction_data(f, root["cbprefixed"].(json.Object))
	fmt.fprintln(f, `}`)
}

get_instruction_data :: proc(root: json.Object) {
	for key in root {
		val := root[key].(json.Object)

		if !slice.contains(menonics[:], val["mnemonic"].(json.String)) {
			append(&menonics, val["mnemonic"].(json.String))
		}
		json_operands := val["operands"].(json.Array)
		for operand in json_operands {
			operand := operand.(json.Object)
			if !slice.contains(operands[:], operand["name"].(json.String)) {
				name := operand["name"].(json.String)
				if name[0] == '$' {
					name = name[1:]
				}
				append(&operands, name)
			}
		}
	}

	return
}

gen_instruction_data :: proc(f: os.Handle, root: json.Object) {
	m, _ := slice.map_keys(root)
	slice.sort(m)
	for key in m {
		val := root[key].(json.Object)
		fmt.fprintln(f, `{`)
		fmt.fprintfln(f, `	.%v,`, val["mnemonic"].(json.String))
		fmt.fprintfln(f, `	0x%v,`, key[2:])
		fmt.fprintfln(f, `	%v,`, u8(val["bytes"].(json.Float)))
		fmt.fprintln(f, `	{`)

		json_operands := val["operands"].(json.Array)
		for operand, i in json_operands {
			fmt.fprintln(f, `	{`)
			operand := operand.(json.Object)


			name := operand["name"].(json.String)
			if name[0] == '$' {
				name = name[1:]
			}
			fmt.fprintfln(f, `		.O_%v,`, name)
			immediate := operand["immediate"]
			fmt.fprintfln(f, `		%v,`, "false" if immediate == nil else "true")

			bytes := operand["bytes"]
			fmt.fprintfln(f, `		%v,`, 0 if bytes == nil else u8(bytes.(json.Float)))


			if operand["increment"] != nil {
				fmt.fprintln(f, `		.Increment,`)
			} else if operand["decrement"] != nil {
				fmt.fprintln(f, `		.Decrement,`)
			} else {
				fmt.fprintln(f, `		.None,`)
			}

			fmt.fprintln(f, `	},`)
		}
		fmt.fprintln(f, `	},`)

		immediate := val["immediate"]
		fmt.fprintfln(f, `		%v,`, "false" if immediate == nil else "true")
		fmt.fprintln(f, `		{},`)
		fmt.fprintln(f, `},`)
	}

}

