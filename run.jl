using Printf
run(`cargo run -- example.s example.o`)
run(`ld -shared -o libexample.so example.o`)
@ccall "./libexample.so".linux_hello_world()::Cvoid
# objdump -d example.o
# readelf -a example.o
# hexedit example.o