global add_one:
    MOV rax rdi
    INC rax
    RET

_TEST test_add_one
    SET rdi 5
    CALL add_one
    ASSERT rax 6
    DBG rdi u64
END

global linux_hello_world:
    xor eax eax
    inc al
    xor edi edi
    inc dil
    lea rsi QWORD PTR [hello_world]
    mov edx 12
    syscall
    ret
hello_world:
    "Hello World" 0x0a 0u8

