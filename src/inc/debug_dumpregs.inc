extern _write

[SECTION .data]
	debug_str_myregs db "EDI      ESI      EBP      ESP      EBX      EDX      ECX      EAX", 0x10, 0x0
  debug_str_myregs_len equ $ - debug_str_myregs
	debug_str_myflagso db "ZF=0 CF=0 OF=0 SF=0 PF=0", 0xa
	debug_nl_char db 0xa
	debug_space_char db ' '
__SECT__

[SECTION .bss]
	debug_tmpbytes resb 8
	debug_myflags resb 25
__SECT__

debug_newline:
  pushad
  ccall _write, 1, debug_nl_char, 1
  popad
  ret

debug_space:
  pushad
  ccall _write, 1, debug_space_char, 1
  popad
  ret

debug_asciidigit:
  add al, 0x30  ; add to make 0-9 ascii
  cmp al, 0x39  ; if the value is over 9
  jbe digitdone ; if not skip
  add al, 0x27  ; if so then add to make a-f
  digitdone:
  ret

;
; print the contents of eax in hex
;
debug_printhex:
  pushad ; save a copy of the registers

  mov edi, eax ; save a copy in edit
  mov cl, 28 ; start with 4 bits
  mov esi, 0 ; starting array pointer

  .debug_hexloop1:
    ror eax, cl ; rotate bits in eax right by amount in cl, a multiple of 4
    and al, 00001111b ; clear top 4 bits 
    call debug_asciidigit   ; convert to ascii 
    mov [debug_tmpbytes + esi], al ; copy our value in to the array
    inc esi           ; increment array pointer
    mov eax, edi      ; restore copy
    sub cl, 4         ; decrement four from the bit rotation
    cmp cl, 0         ; compare to 28
    jge .debug_hexloop1 ; if greater then or equal to then continue (signed)

  ccall _write, 1, debug_tmpbytes, 8

  popad ; restore copy of the registers
  ret

;
; print the contents of eax in hex
; ebx = pointer to buffer
;
debug_convhex:
  push ecx ; save a copy of ecx
  push edi
  push esi
  mov edi, eax ; save a copy in edit
  mov cl, 28 ; start with 4 bits
  mov esi, 0 ; starting array pointer

  .debug_convloop1:
    ror eax, cl ; rotate bits in eax right by amount in cl, a multiple of 4
    and al, 00001111b ; clear top 4 bits 
    call debug_asciidigit   ; convert to ascii 
    mov [ebx + esi], al ; copy our value in to the array
    inc esi           ; increment array pointer
    mov eax, edi      ; restore copy
    sub cl, 4         ; decrement four from the bit rotation
    cmp cl, 0         ; compare to 28
    jge .debug_convloop1 ; if greater then or equal to then continue (signed)

  pop esi
  pop edi
  pop ecx ; restore copy of the registers
  ret

;
; dump out the registers
;
debug_dumpregs:
  pushfd
  pushad ; first copy to return
  pushfd ; save a copy of the cpu flags
  pushad ; second copy for us to pop off

  ccall _write, 1, debug_str_myregs, debug_str_myregs_len
  call debug_newline
  mov ecx, 8
  mov edi, 0

  .debug_regsloop:
    pop eax  ; pop the first value off the stack
    push ecx ; push counter onto the stack
    push eax ; push the contents back on because we have to use eax

    mov ecx, debug_str_myregs ; what register?
    add ecx, edi    ; pointer for debug_str_myregs
    pop eax       ; ok now lets get the value to print
    call debug_printhex ; print out the contents of eax in hex 
    call debug_space    ; print a debug_space

    pop ecx       ; lets get the counter back
    add edi, 4
    loop .debug_regsloop

  call debug_newline

  ;now lets print the flags
  mov ecx, 25 ; length to copy
  mov esi, 0  ; offset pointer

  copyloop: ; lets copy the .text to the .bss so we can modify it
  mov al, [debug_str_myflagso + esi] ; move this byte here because we can't move mem to mem
  mov [debug_myflags + esi], al ; copy the byte from the register
  inc esi ; increment the pointer
  loop copyloop

  popfd ; restore the copy of the cpu flags so we can print them out
  jnz nosetzf ; jump if zero flag not set
  mov [debug_myflags+3], byte 0x31 ; set to ascii 1
  nosetzf:
  jnc nosetcf ; jump if carry flag not set
  mov [debug_myflags+8], byte 0x31
  nosetcf:
  jno nosetof ; jump if overflow flag not set
  mov [debug_myflags+13], byte 0x31
  nosetof:
  jns nosetsf ; jump if sign flag not set
  mov [debug_myflags+18], byte 0x31
  nosetsf: 
  jnp nosetpf ; jump if parity flag not set
  mov [debug_myflags+23], byte 0x31
  nosetpf:

  ccall _write, 1, debug_myflags, 25

  ; restore original values
  popad ; restore original copy of the registers
  popfd
  ret

