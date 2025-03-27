section .text
    global int2str, print

int2str_rec:
        mov rax, [rsp + 8]
        cmp rax, 10
        jge .L1         ; if x >= 10 jump L1
        jmp .L2         ; jump L2

.L1:            ; L1:
        mov rbx, 10
        xor rdx, rdx
        div rbx         ; t1 = x / 10
        mov rcx, [rsp + 16]
        push rcx
        push rax        ; param buf
        call int2str_rec
        add rsp, 16     ; t2 = call int2str_rec
        mov qword [rsp + 16], rax; buf = t2
        jmp .L2         ; jump L2

.L2:            ; L2:
        mov rbx, [rsp + 8]
        mov rcx, 10
        xchg rbx, rax
        xor rdx, rdx
        div rcx         ; t4 = x % 10
        add rdx, '0'     ; t4 = t4 + '0'
        mov rdi, [rsp + 16]
        mov qword [rdi], rdx; *buf = t4
        mov rdi, [rsp + 16]
        add rdi, 1      ; t5 = buf + 1
        mov rax, rdi
        ret             ; return t5

int2str:
    mov rax, [rsp + 8]
    mov rbx, [rsp + 16]
    cmp rax, 0
    jge .L3
    ; Negative numbers: add '-' and flip the sign
    mov byte [rbx], '-'
    add rbx, 1
    mov rcx, rax
    xor rax, rax
    sub rax, rcx
.L3:
    push rbx
    push rax
    call int2str_rec
    add rsp, 16
    ret

print:
        sub rsp, 21
        mov rax, rsp    ; t21 = macro 0
        push rax        ; buf = t21
        push rax        ; param buf
        mov rbx, [rsp + 45]
        push rbx        ; param x
        call int2str
        add rsp, 16     ; t1 = call int2str
        ; buf pointer at [rsp]
        ; buf_after pointer in rax
        mov rdx, rax ; save buf_after
        mov rax, 1 ; write syscall
        mov rdi, 1 ; first arg, stdout
        mov rsi, [rsp] ; second arg, buffer
        sub rdx, rsi ; third arg, length = buf_after - buf
        syscall
        add rsp, 29
        mov rax, 0
        ret
