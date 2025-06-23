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

Instruction :: struct {
	mnemonic:  Mnemonic,
	bytes:     u8,
	cycles: []u8,
}
`,
	)

	get_instruction_data(root["unprefixed"].(json.Object))
	get_instruction_data(root["cbprefixed"].(json.Object))

	fmt.fprintln(f, `Mnemonic :: enum {`)
	for mnemonic in menonics {
		fmt.fprintfln(f, "	%s,", mnemonic)
	}
	fmt.fprintln(f, "}")

	fmt.fprintln(f, `
@(rodata)
UnprefixedInstructions := [?]Instruction{`)
	gen_instruction_data(f, root["unprefixed"].(json.Object))
	fmt.fprintln(f, `}`)
	fmt.fprintln(f, `
@(rodata)
PrefixedInstructions := [?]Instruction{`)
	gen_instruction_data(f, root["cbprefixed"].(json.Object))
	fmt.fprintln(f, `}`)
}

get_instruction_data :: proc(root: json.Object) {
	for key in root {
		val := root[key].(json.Object)

		if !slice.contains(menonics[:], val["mnemonic"].(json.String)) {
			append(&menonics, val["mnemonic"].(json.String))
		}
	}

	return
}

gen_instruction_data :: proc(f: os.Handle, root: json.Object) {
	m, _ := slice.map_keys(root)
	slice.sort(m)
	for key in m {
		val := root[key].(json.Object)
		fmt.fprint(f, `{`)
		fmt.fprintf(f, `.%v, `, val["mnemonic"].(json.String))
		fmt.fprintf(f, ` %v,`, u8(val["bytes"].(json.Float)))
		fmt.fprint(f, `{`)

		json_cycles := val["cycles"].(json.Array)
		for cycle, i in json_cycles {
			cycle := cycle.(json.Float)
			fmt.fprintf(f, ` %v, `, cycle)
		}

		fmt.fprint(f, `}`)
		fmt.fprintln(f, `},`)
	}

}

