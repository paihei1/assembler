# introduction

# command line interface
Currently, the program expects exactly two arguments, the first one being the input file,
and the second being either the output file or either '-t' or '-T'.
If an output file is specified, the assembler writes an ELF-object file to that location,
assembling to a Windows format is not yet supported.
If '-t' or '-T' is specified, the included [Tests](#tests) are run instead,
'-T' meaning that the output of successful tests is shown and '-t' meaning that it is hidden.
This test execution currently works on both Windows and Linux (but it does rely on marking a
segment writeable and executable at the same time, which may fail on other systems).

# general syntax
The program uses a custom, simplified assembly syntax.
All keywords can be written in either all_lowercase or ALL_CAPS, but no Mixed_Case.
Whitespace is only significant in the following ways:
- spaces and tabs are treated the same, and it only matters if there are any between two words, not how many.
- multiple statements (e.g. instruction with operands) can be on the same line, but one statement can't be spread over multiple lines.
The destination operand of an instruction is generally the first operand,
and the different operands are seperated by whitespace (but there may also be whitespace within one operand), for example `mov rax rdi` would move the content of `RDI` to `RAX`.
If you are unsure about a specific instruction, it is currently best to directly look at the
code in the extension files like `src/assemble/base.rs` directly, in most cases 
[the specification syntax](#the-specification-syntax) is used.
More accessible and browseable documentation is planned in the future.

Other than instructions, the following statements are allowed anywhere in the file:
- A label can be set as `LABEL_NAME:`. Its visibility can be set using `global LABEL_NAME` or in one line `local LABEL_NAME:`, different to most assemblers, labels which are set as neither are not put into the ELF-file at all. Label names take the form `[_A-Za-z][_A-Za-z0-9]*` and are case sensitive. To reference an external label, introduce it as `global` anywhere in the file and don't define it.
- a `#` marks the rest of the current line as a comment, which is ignored completely.
- the syntax `PTR <BYTE/WORD/DWORD/QWORD> LABEL_NAME` directly writes the relative offset to the referenced label in bytes, signed and with a size given by BYTE/WORD/DWORD/QWORD. As opposed to instructions like `call` or `jmp`, the offset is taken from the start of the inserted number, not from after it.
- *deprecated* `<BYTE/WORD/DWORD/QWORD> <integer(/ float if DWORD/QWORD) literal>` directly writes the literal with the given size.
- use instead: directly insert `<sized integer/float literal>` using the new [sized literals](#literals). warning: `0x01 0x23 0x45 0x67` and `0x_01_23_45_67` are not the same, `0x01 0x23 0x45 0x67` is equal to `0x_67_45_23_01`.
- `ALIGN <positive integer literal>` aligns the next statement to the given number of bytes relative to the start or the file, using no-ops.
- `LOCK` directly writes the lock prefix 0xF0. It does not check if it is valid before the next instruction.
- a string literal between double quotes `"string literal"` directly inserts it, using utf-8 encoding. Currently there are no escape sequences read at all, so e.g. newlines should be `"line one" 0x0A "line two"`. If you need the string to be zero terminated, explicitly write a `0x00` after.
- the keywords `_TEST`, `SET`, `DBG`, `ASSERT` and `END` are used for [tests](#tests).

# operands
The following instruction operands are possible:
- low byte registers `AL`,`CL`,`DL`,`BL`,`SPL`,...,`R8L`,...,`R15L`, as always also all lowercase
- high byte registers `AH`,`CH`,`DH` and `BH`
- word (2 bytes) registers `AX`,...,`SP`,...,`R8W`,...
- doubleword (4 bytes) registers `EAX`,...,`ESP`,...,`R8D`,...
- quadword (8 bytes) registers `RAX`,...,`RSP`,...,`R8`,...
- 128 byte SIMD registers `XMM0`,...,`XMM15`
- 256 byte SIMD registers `YMM0`,...,`YMM15`
- immediate values, some instructions only accept some literal types (e.g. the bitshifts will not allow a signed or floating point value as second operand)
- a memory operand, of the form `(BYTE/WORD/DWORD/QWORD) PTR [...]` (parts in `(...)` are optional), where the brackets can contain either a label (in 64-bit mode this is always encoded as RIP-relative, so 'RIP+' doesn't need to be specified), or of the form `[(<base register>)+([1/2/4/8]*<index register>)+(signed offset)]`, where all parts are optional (but if only an offset is specified, it is again interpreted as RIP-relative). The register names must be the quadword variants `RAX`,... . Some additional encoding based restrictions apply. The size specifier `(BYTE/WORD/DWORD/QWORD)` is not always necessary (and sometimes makes no sense at all), but sometimes is the only way to specify the instructions operand size.

# literals
A literal can have the following types: unsigned integers `u8`,`u16`,`u32` and `u64`, 
signed integers `i8`,`i16`,`i32` and `i64`, floating point literals `f/r32` and `f/r64`, or type 'byte string of arbitrary length'.
A type is selected from context (e.g. for `ASSERT rax 1` the size is obvious from the register) and the following table:

decimal literal with any suffix `[-]?[0-9_]+(u8|u16|...)`:                                      type as specified if compatible
floating point literal with suffix `[-]?[0-9_]*.[0-9_]*([eE][-]?[0-9_]+)?(f32|r32|f64|r64)`:    type as specified if compatible
hexadecimal literal with any suffix `0[xX][0-9a-fA-F_]*(u8|u16|...)`:                           type as specified if compatible
string literal `"[^"]*"`, there are no escapes:                                                 byte string of arbitrary length
decimal literal `[-]?[0-9_]+`:                                                                  any number type that fits
floating point literal `[-]?[0-9_]*.[0-9_]*([eE][-]?[0-9_]+)?`:                                 `f/r32` or `f/r64`
hexadecimal literal `0[xX][0-9a-fA-F_]+`:                                                       unsigned integer with size specified by number of digits or byte string of arbitrary length

# tests
The source file can contain test blocks starting with `_TEST <test name>` and ending with `END`.
When the option `-t` or `-T` is specified instead of a target file, all code inside those blocks is executed,
otherwise anything inside the blocks is not included in the output.
Testing also defined three new commands, which can be put anywhere (also outside of tests, but are also not output when not testing):
- `PRINT <whitespace seperated list of literals>`: prints the message, e.g. `PRINT "test " 0xF0 0x9F 0xA5 0xB3 "!"`
- `SET <whitespace seperated list of <position> <value> pairs>`: sets each *position* to the corresponding *value*
- `DBG <whitespace seperated list of <position> <value/type> pairs>`: prints each *position* formatted as type *value*
- `ASSERT <whitespace seperated list of <position> <value> pairs>`: asserts that each *position* has the corresponding *value*, otherwise prints both and fails the test.

A *position* is specified in the same way as an instruction [operand](#operands),
except for that it can't be an immediate value and that if it is a memory operand it can't reference a label.
If the *position* is any general purpose register, the *value* must be a literal of the correct size
(or for `DBG` the *value/type* can also just be one of the literal types `u8`,`u16`,... if an unsigned integer value is specified 
(e.g. `0u32`), the *position* is printed in hexadecimal, otherwise the normal format is decimal).
If the *position* is a SIMD register, the *value* must be a whitespace seperated and `[...]` enclosed list of literals or literal types with sizes adding up to the register size (16 bytes or 32 bytes), if only a literal type is specified, that part is not `SET`/`ASSERT`-ed.
If the *position* is a memory location, the *value* must be a literal, a literal type or a whitespace seperated and `[...]` enclosed list of literals or literal types that can have one `|` between two values that specifies where the actual pointer points, literals and literal types are interpreted like before.

If any `ASSERT` fails, the test is interrupted and counts as failed, printing all previous `DBG` results and the failed `ASSERT`.
If the test runs through, the `DBG`-s are shown if the test flag was `-T`, not for `-t`.

Currently the execution environment for tests can not link any external code, maybe this will change in the future.

# the specification syntax
Many instructions follow a similar encoding scheme, so they are specified in a restricted specification syntax:
The match arm that encodes e.g. the add-with-carry instruction looks like this:
```rust
    b"ADC" | b"adc" => encode_with_operands::<2>(&[
        EncodingArm::new("amr8 imr8 : 14--80/2-10-12-"), 
        EncodingArm::new("amr* i8imr* : -15--83/2 81/2-11--13-")], operands, labels, IMM_INT).map(Some),
```
The specification starts with one block per operand, then the `:`.
Each operand type block specifies what types the operand can have, each being a 2 character code from the following set in order:
The possible first characters are `a`,`b`,`c`,`d`,`i`,`m`,`r`,`v`, and the possible second characters are `8`,`*`,`x` and `y`.
They have to be specified in alphabetical order of the first character and with all `8` before all `*` and so on 
(technically they don't need to, but they get interpreted as if they were, so put them in the right order anyway).
If multiple types have the same second character, it only needs to be specified once after all the first characters.
The second characters specify the operand size:
- `8`: 1 byte
- `*`: the operand can be 2,4 or 8 bytes, differentiated using operand size override prefix for 2 bytes or REX.W for 8 bytes. The specific sizes of all `*` operands must agree.
- `x`: 16 bytes, e.g. an XMM register
- `y`: 32 bytes, e.g. a YMM register

The first characters specify how the operand is encoded, influencing what type it can have:
- `a`: specific opcode if the operand is AL/AX/EAX/RAX (similar for `b`,`c`,`d`), only allowed with second character `8` or `*`.
- `i`: immediate value of the specified size (but for operand size 8 bytes still only 4, except for a special case, see later), also only allowed for `8` and `*`.
- `m`: encoded as MODRM.r/m, meaning it can be a register or a memory operand.
- `r`: encoded as MODRM.reg, meaning it must be a register.
- `v`: encoded as VEX.vvvv, meaning it must be a register, used as a third operand for AVX instructions, so usually with `x` or `y`.

After the `:` comes a list of how to encode all operand type combinations, each element either speciying the combinations opcode or a `-` if this combination is impossible.
The later operands iterate through their types faster, so for example for the first `EncodingArm` for `ADC`:
The `14` stands for types `a8 i8`, the first two dashes mean `a8 m8` and `a8 r8` are impossible, the `80/2` stands for `m8 i8` and so on.
If multiple consecutive combinations are possible, their encodings are seperated by whitespace, like `83/2`and `81/2` in the second `EncodingArm`.
Each encoding consists of an even number of hex-digits specifying the opcode followed by some optional suffixed:
- `/` followed by another hex digit specifies the value of REX.r : MODRM.reg, only possible if no operands have type `r`.
- `+` means that instead of using MODRM.reg, the operand of type `r` is added to the last byte of the opcode. Special case only for `MOV`: if this is used and the operand size is 8 bytes, an `i*` operand will be encoded as a full 8 byte immediate.
- `$` followed by 2 more hex digits, which are read as a byte with the first 2 bits being VEX.pp, the third bit being VEX.L and the rest being VEX.map_select.

# small TODOs
- (remove the `BYTE`,`WORD`,`DWORD` and `QWORD` arms in `assemble::try_assemble`)
- (add missing base instructions: far jumps, `LOOP/LOOPE/LOOPNE`, all special variants of `MOV`)
- use fewer instructions for entering/leaving tests, `FXSAVE64, FXRSTOR64`
- expand buffer for opcode

# roadmap
- add instructions from extensions
- windows target
- redo command line interface
- add performance testing
- searchable index of x86 instructions, documentation
- visualization: dependencies, performance simulation results
- calling convention helper
- project structure, build system, manual compilation steps, change over time, good place for comments, documentation
