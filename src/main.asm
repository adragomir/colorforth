%include "macros.inc"
%include "posix.inc"
%include "SDL.inc"

%include "debug.inc"
%include "debug_dumpregs.inc"

section .data
  str_icons_file db 'data/icons', 0
  str_blocks_file db 'data/OkadWork.cf', 0
  str_backup_blocks_file db 'data/OkadBack.cf', 0

section .bss 
  rstruct SDL_Surface, surface  
  rstruct SDL_Event, event
  rstruct stat, icons_file_stat
  rstruct stat, blocks_file_stat
  rstruct stat, backup_blocks_file_stat

; This version of colorforth has three tasks; main (the accept loop),
; draw (user defined), and serve (also user defined).  Each has two
; grows - down stacks.  A suffix of 's' indicates the return stack, 'd'
; indicates the data stack.  Thus 'top_draw_return_stack' and 'top_draw_data_stack' are the tops of
; the return and data stacks, respectively, for the draw task.

return_stack_size equ 256 * 4 * 3   ; size of return stacks (3 blocks)
data_stack_size equ 256 * 4 * 6 ; size of data stacks (6 blocks)
forth_dict_size equ 2048 * 4  ; size of forth dictionary (2048 words, 8 blocks)
        ; we have two arrays like this: names and addrs.
bufsize equ 18 * 1024 ; size of floppy buffer.

;   100000 dictionary
;    a0000 return stack (main)
;    9f400 data stack
;    9dc00 return stack (draw)
;    9d000 data stack
;    9b800 return stack (serve)
;    9ac00 data stack
;    99400 divider (bottom of rstack, top of floppy buffer)
;    94c00 floppy buffer
;    92c00 forth dictionary addrs (room for 2048 entries)
;    90c00 forth dictionary names
;     7c00 BIOS loads boot sector here; we immediately move it to 0
;     4800 source
;     3000 icons
; 0 the colorforth kernel
    times ((3 * (return_stack_size + data_stack_size) + bufsize + 2 * forth_dict_size) / 4) dd 0

;dictionary  equ 0x100000 ; TODO get rid of this

top_main_return_stack equ $; gods
top_main_data_stack equ top_main_return_stack - return_stack_size ; godd
top_draw_return_stack equ top_main_data_stack - data_stack_size
top_draw_data_stack equ top_draw_return_stack - return_stack_size
top_serve_return_stack  equ top_draw_data_stack - data_stack_size
top_serve_data_stack  equ top_serve_return_stack - return_stack_size
end_of_stacks equ top_serve_data_stack - data_stack_size  ; end of stacks
buffer  equ end_of_stacks - bufsize
forth_dictionary_addresses  equ buffer - forth_dict_size
forth_dictionary_names  equ forth_dictionary_addresses - forth_dict_size
; ...

trash_adr times 64 db 'T'
dummy dd  0

section .text

extern _printf, _open, _mmap, _read, _malloc, _write, _realloc

global _main                ; make the main function externally visible

; (r >> format->Rloss) << format->Rshift | (g >> format->Gloss) << format->Gshift | (b >> format->Bloss) << format->Bshift | format->Amask;
; 16 bit: 
; format->Rloss : 3 ; format->Gloss : 2 ; format->Bloss : 3 ; format->Rshift : 11 ; format->Gshift : 5 ; format->Bshift : 0 ; format->Amask: 0
; 32 bit: 
; format->Rloss : 0 ; format->Gloss : 0 ; format->Bloss : 0 ; format->Rshift : 16 ; format->Gshift : 8 ; format->Bshift : 0 ; format->Amask: 0

%define sdl_rgb32(r, g, b) dword (r << 16) | (g << 8) | b | -16777216
%define sdl_rgbsingle32(rgb) (( ((rgb >> 16) & 0xff) << 16) | (((rgb >> 8) & 0xff) << 8) | (rgb & 0xff)  | -16777216)

%define sdl_rgb16(r, g, b) dword ((r >> 3) << 11) | ((g >> 2) << 5) | ((b >> 3)) | -16777216
%define sdl_rgbsingle16(rgb) dword ( (((rgb >> 16) & 0xff) >> 3) << 11) | ( (((rgb >> 8) & 0xff) >> 2) << 5) | ((rgb & 0xff) >> 3)  | -16777216

host_flip_screen:
  push eax
  __SDL_Flip dword [surface]
  pop eax
  ret

host_alloc_display:
  __SDL_Init SDL_INIT_VIDEO
  __SDL_SetVideoMode screen_width, screen_height, screen_depth * 8, SDL_FULLSCREEN
  mov [surface], eax
  mov edx, [eax + 20]
  mov [frame], edx

  __SDL_EnableKeyRepeat 125, 50 
  string color_forth, "colorForth for MacOSX", 0x0
  __SDL_WM_SetCaption str_color_forth, str_color_forth
  ret

bye:
host_exit_ok:
  __SDL_Quit
  syscall SYS_exit, 0
  ret

host_exit_fail:
  __SDL_Quit
  syscall SYS_exit, 1
  ret

icons_fd: dd 0
icons_address: dd 0
blocks_fd: dd 0
blocks_address: dd 0
backup_blocks_fd: dd 0
backup_blocks_address: dd 0

%macro host_read_file 1
  ; open
  syscall SYS_open, str_ %+ %1 %+  _file, O_RDONLY
  mov dword [%1 %+ _fd], dword eax
  ;file stat: find file size
  syscall SYS_fstat, dword[%1 %+ _fd],  %1 %+ _file_stat ; pass the address, NOT the value
  mov ecx, dword [%1 %+ _file_stat.st_size]
  ; open memory that will contain the contents
  syscall SYS_mmap, dword 0, ecx, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_ANON, dword 0, dword 0
  mov dword [%1 %+ _address], eax
  ;read the contents
  syscall SYS_read, dword [%1 %+ _fd], dword [%1 %+ _address], dword [%1 %+ _file_stat.st_size]
  ; close the file
  syscall SYS_close, dword [%1 %+ _fd]
%endmacro

%macro host_write_file 2
  ; open
  syscall SYS_open, str_ %+ %1 %+  _file, O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR
  mov dword [%1 %+ _fd], dword eax
  ;read the contents
  syscall SYS_write, dword [%1 %+ _fd], dword [%2 %+ _address], dword [%2 %+ _file_stat.st_size]
  ; close the file
  syscall SYS_close, dword [%1 %+ _fd]
%endmacro

host_read_icons_file:
  host_read_file icons
  ret

host_read_blocks_file:
  host_read_file blocks
  ; recompute nc
  mov eax, dword [blocks_file_stat.st_size]
  xor edx, edx
  mov ecx, dword 18 * 1024
  div ecx
  mov [nc], eax
  ret

host_write_backup_blocks_file:
  host_write_file backup_blocks, blocks
  ret

host_write_blocks_file:
  host_write_file blocks, blocks
  ret

; in:  ecx - size
; out: eax - address
host_alloc_mem:
  syscall SYS_mmap, dword 0, ecx, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_ANON, dword 0, dword 0
  ret

host_alloc_buffers:
  mov ecx, 1024 * 384        ; dictionary size 1 MB; TODO constant here
  call host_alloc_mem
  mov [H], eax
  ret

_main:
  mov eax, 1
  test eax, 2

  call host_read_icons_file
  call host_read_blocks_file

  call host_alloc_buffers
  call host_alloc_display
  jmp start1

host_notimpl:
    ret

%include "huffman.inc"
%include "icons.inc"

; Boots into 32-bit mode with a flat address space. 
; Segment registers are based at zero; essentially unused. Interrupts off. Protections off. 
; Data are addressed as 32-bit words, not bytes. 
; But esp and esi hold byte addresses, for optimization. 
; Instructions are optimized if agruments are literals. 
; Registers are assigned:
; 0 eax:
;    stack (1st number on Data stack) 
; 1 ecx:
;    string counter, scratch 
; 2 edx:
;    address register A, I/O port, scratch 
; 3 ebx:
;    unused 
; 4 esp:
;    byte pointer to top of Return stack 
; 5 ebp:
;    unused 
; 6 esi:
;    byte pointer to 2nd number on Data stack 
; 7 edi:
;    dword pointer to next word to be interpreted

; the Pentium manual recommends not using "complex instructions"
; like LOOP.  However, it IS used in the boot sector where space
; is at a premium.


; the Pentium manual recommends not using "complex instructions"
; like LOOP.  However, it IS used in the boot sector where space
; is at a premium.
; can't use loopnz in 32-bit mode
%macro NEXT 1
  dec ecx
  jnz %1
%endmacro

; save contents of eax on data stack
; (eax is already a copy of top of data stack)
%macro DUP_ 0
  lea esi, [esi - 4]
  mov [esi], eax
%endmacro

; pop what's at bottom of data stack back into eax
%macro DROP 0
  lodsd ; Load doubleword at address DS:(E)SI into EAX.
%endmacro

op_drop   equ 0xad
op03_dup  equ 0x89fc768d
op4_dup   equ 0x06
op_mov_eax_n  equ 0xb8
op_call   equ 0xe8
op_ret    equ 0xc3

; width, height, BYTES per pixel
screen_width  equ 1024
screen_height equ 768
screen_depth  equ 4
screen_depth_log2 equ 2

_yellow equ sdl_rgbsingle32(0xffff00)
_dkylw  equ sdl_rgbsingle32(0xc0c000)
_orange equ sdl_rgbsingle32(0xe04000)

_green  equ sdl_rgbsingle32(0x00ff00)
_dkgrn  equ sdl_rgbsingle32(0x00c000)

_red  equ sdl_rgbsingle32(0xff0000)
_cyan equ sdl_rgbsingle32(0x00ffff)
_mag  equ sdl_rgbsingle32(0xff00ff)

_black  equ sdl_rgbsingle32(0x000000)
_silver equ sdl_rgbsingle32(0xc0c0c0)
_white  equ sdl_rgbsingle32(0xffffff)

_blue equ sdl_rgbsingle32(0x8080ff)
_dkblue equ sdl_rgbsingle32(0x4040ff)

; icons are 16x24 pixels
icon_width  equ 16
icon_height equ 24
; a character is an icon with 3 pixels of padding all round.
char_padding  equ 3
; character width/height
char_width  equ char_padding + icon_width + char_padding      ; 22
char_height equ char_padding + icon_height + char_padding     ; 30
; horizontal/vertical characters (screen size)
horizontal_chars  equ screen_width / char_width       ; 1024 / 22 = 46 (remainder 12)
vertical_chars  equ screen_height / char_height       ; 768 / 30 = 25 (remainder 18)

hexbit equ 20q  ; that's octal, bit 4 set to indicate hexadecimal display
tagbits equ 17q ; octal again, low 4 bits (0-3) indicate type tag


warm:
  DUP_
  jmp start1.0

start1:
  ; TODO: setup arrays
  ;mov [top_serve_data_stack - 12], dword 8
  ;mov [top_serve_data_stack - 16], dword 90
  mov esi, top_main_data_stack

  .0:
    call noshow
    call noserve
    mov dword [forths], num_of_forth_words
    mov dword [macros], num_of_macros
    mov dword [trash], buffer
    ; copy initial forth dictionary up to its real address
    push esi

    lea esi, [forth_words_names]
    mov edi, forth_dictionary_names
    mov ecx, [forths]
    rep movsd ; esi source, edi destination, ecx times

    lea esi, [forth_words_addresses]
    mov edi, forth_dictionary_addresses
    mov ecx, [forths]
    rep movsd ; esi source edi destination, ecx times

    pop esi
    
    ; load start screen block 18, and start the colorforth system
    mov eax, 18
    call load
    jmp accept


; SWITCHING TASKS
; The current task yields to the other task by calling pause, which preserves
; the state of the current task and jumps into round, which calls unpause to
; reload the state of the other task and to return to that task.
; For each task, there are a number of things that must be preserved:
; The top-of-stack register (TOS) - which in ColorForth is EAX.
; The next-on-stack register (NOS) - which in ColorForth is ESI.
; The return-stack pointer - which in ColorForth is ESP.
; A pointer to where the return-stack pointer is stored, which is me.
; NOTE: Unpause takes its argument via the pointer on the return stack
; that is, its data is expected to be stored inline immediately after the call,
; as it is in round. (Also note that unpause does not return to its caller.)


; When we switch tasks, we need to switch stacks as well.  We do this
; by pushing eax (cached top - of - stack) onto the data stack, pushing
; the data stack pointer onto the return stack, and then saving the
; return stack pointer into the save slot for the task.

; 'me' points to the save slot for the current task
align 4
me: dd main

; user routines for draw and serve.
udraw:  dd 0
userve: dd 0

; these are the save slots - each is followed by code to resume the
; next task - the last one jumps 'round to the first.
; ROUND
; Note the structure of round: For each of the two tasks, there is a call to unpause,
; followed by a variable. Clearly the system cannot simply return to the variable's
; address, and indeed it doesn't: Unpause takes the address on the return stack as an
; argument; it is the address of a variable from which information (in this case the
; return-stack pointer of the incoming task) is to be taken. Unpause also adds four
; (the size of the variable) to the return address, so that it returns to the instruction
; immediately following the variable.
round:  call resume

main: dd 0
  call resume

draw: dd 0
  call resume

serv: dd 0
  jmp round

; pause this task and switch to the next one.
dopause:
  DUP_            ; push TOS onto data stack..
  push esi        ; [RST] save NOS (data-stack pointer) onto return stack, which already has the task's return address
  mov eax, [me]   ; get current task
  mov [eax], esp  ; save return stack pointer (to which task it needs)
  add eax, byte 4 ; get address AFTER the current task (skip storage slot, point to round-robin CALL or JMP)
  jmp eax         ; jump there (into round, which immediately calls resume below)

; resume a task.
; Pause stops the current task and jumps into round, which then calls unpause, which causes
; the other task to run.
; IN: return address points to the save slot.
resume:
  pop eax        ; [RST] return address is always god or main, where return-stack pointer for OTHER task is stored
  mov esp, [eax] ; [RST] load other task's return-stack pointer (this switches return stacks).
  mov [me], eax  ; EAX = pointer to other task's return-stack pointer
  pop esi        ; [RST] restore other task's NOS from the return stack
  DROP           ; load previously dup'd datum back into EAX
  ret            ; and jump to the next task

; Hint in difference between pause and unpause: Pause saves ESP where "me" points.
; Unpause does NOT retrieve ESP from where "me" points
; instead it loads ESP from the address on the return stack.
; (Pause does not alter "me"; unpause does.) Note also that, at startup, "me" already
; contains a pointer to "god".
; So what happens when a ColorForth block calls show?

; set draw task to code following 'call to_draw' (with empty stacks).
to_draw:
  mov edx, top_draw_data_stack - 4   ; data stack is empty.
  mov ecx, top_draw_return_stack - 4 ; return stack contains our return address
  pop dword [ecx]                    ; ecx = address of .0 in show
  lea ecx, [ecx - 4]                 ; and the data stack pointer
  mov [ecx], edx
  mov [draw], ecx                    ; store it in the draw slot
  ret                                ; and return twice (to our caller's caller).

; ditto, for serve task
to_serve:
  mov edx, top_serve_data_stack - 4
  mov ecx, top_serve_return_stack - 4
  pop dword [ecx]
  lea ecx, [ecx - 4] ; TODO XXX PROBLEM
  mov [ecx], edx
  mov [serv], ecx
  ret

; SHOW0
; This sets up both tasks - show sets up show0's code (the RET instruction) as the GOD task
; and show's own code as the MAIN task.
; Note that when show0 is run at startup, the current task is the GOD task.
noshow:
  call show
  ret

; set user drawing routine to code following 'call show'.
; Show changes the code that is run as the foreground or GOD (graphic output display) task.
; Note, however, that whenever show sets up the GOD task to run the caller's code, it also
; resets the MAIN task to run its own code.
; (Note "call [screen]" - if show is called from show0, this just returns to show
; - if show is called from refresh, this runs almost all of refresh)
show:
  pop dword [udraw] ; udraw = address of "ret" in nowhow
  call to_draw
  .0:
    ; the drawing loop.
    call [udraw]
    call switch ; copy frame to screen, then pause.
    jmp .0

freeze:
  pop dword [udraw]
  call to_draw
  .0:
    call [udraw]
    jmp .0

noserve:
  call serve
  ; an empty "server" routine
  ret

; set the user serve routine to code following 'call serve'.
serve:
  pop dword [userve]
  call to_serve
  .0:
    ; the server loop
    call dopause
    call [userve]
    jmp .0

; C
; Change NOS to point to highest cell in return stack for "god" ("godd" marks the address
; one cell HIGHER than the highest-addressed cell in the data stack, so "godd+4" is not in
; the data stack)
; set data stack pointer to highest cell of rstack.
c_:
  mov esi, top_main_return_stack - return_stack_size + 4
  ret

; ADDING TEMPORARY DEFINITIONS
; save current state so we can recover later with 'empty'
mark:
  mov ecx, [macros] ; Save # macros in longword mk
  mov [mk], ecx
  mov ecx, [forths] ; Save # forth words in mk + 1
  mov [mk + 4], ecx
  mov ecx, [H]      ; Save end-of-dictionary pointer in mk + 2
  mov [mk + 8], ecx
  ret

; restore state saved at last 'mark'
empty:
  mov ecx, [mk + 8]    ; restore end-of-dictionary pointer.
  mov [H], ecx
  mov ecx, [mk + 4]    ; restore # words in FORTH wordlist.
  mov [forths], ecx
  mov ecx, [mk]        ; restore # words in MACRO wordlist.
  mov [macros], ecx
  mov dword [class], 0 ; used by C18 compiler, cleared here
  ret

; FINDING WORDS
; These routines return with ECX = offset into wordlist to word if zero flag set(?)
; [note how instructions affect zero flag]

; find pointer to macro code
mfind:
  mov ecx, [macros]                    ; number of macros, 1-based
  push edi                             ; save destination pointer, we need to use it momentarily
  lea edi, [macro_names - 4 + ecx * 4] ; point to last macro
  jmp short ffind                      ; search dictionary

; locate code of high- or low-level Forth word
find:
  mov ecx, [forths]                               ; current number of Forth definitions
  push edi                                        ; save destination pointer so we can use it
  lea edi, [forth_dictionary_names - 4 + ecx * 4] ; point it to last packed Forth word

ffind:
  std         ; search backwards
  repne scasd ; continue moving until we hit a match
  cld         ; clear direction flag again
  pop edi     ; no longer need this, can tell from ECX where match was found
  ret

; execute word (full name on top of stack).
; exword is the default action of 'aword'; find and execute the word
exword:
  dec dword [words] ; from keyboard
  jz execute.0
  DROP
  jmp exword

; execute source word
execute:
  mov dword [lit], alit
  DUP_                                         ; save EAX (top of stack) on data stack
  mov eax, [ -4 + edi * 4]                     ; grab the next pre-parsed word
  and eax, byte -16                            ; Mask out color bits (bits 0..3).
  .0:
    call find                                  ; look for word in the dictionary
    jnz abort                                  ; If not found, abort.
    DROP                                       ; restore EAX from stack
    jmp [forth_dictionary_addresses + ecx * 4] ; jump to low-level code of Forth word or macro

; abort execution or load operation
abort:
  debug_print "called ABORT !!!!!!!!!!!!!", 0xa, 0x0
  mov [curs], edi ; store pointer to last source word encountered

  ; get block number from word pointer
  shl    edi, 2
  sub    edi, [blocks_address] 
  shr    edi, 10
  add    edi, 18

  mov [blk], edi ; update BLK
  ; this way, E will begin editing at point of failure

abort1: 
  mov esp, top_main_return_stack ; [RST] reset return stack pointer
  mov dword [adefine], forthd
  mov dword [adefine + 4], qcompile
  mov dword [adefine + 8], cnum
  mov dword [adefine + 12], cshort
  mov eax, Iquestion
  call echo
  jmp accept

; ADDING WORDS TO THE DICTIONARY
; ColorForth offers two routines to add a word to the dictionary - one routine for each wordlist.
; The address of the routine to use is stored in adefine, the word-definition vector.
; Macro_ uses sdefine to store the address of macrod into this vector, while forth uses sdefine
; to store the address of forthd.
; Sdefine takes the address of a word-definition routine, but it takes the address off the return stack.
; This allows the caller to pass the address simply by calling sdefine
; (as long as the address is intended to be the address of the instruction following the call).
; This leads to simpler code than would be required to pass an address on the data stack.
; Of course this means that (1) the code after the call is not run when sdefine returns, and
; (2) sdefine returns not to its caller but to its caller's caller.

;Sets word-definition vector. Called when current wordlist changes.
sdefine:
  pop dword [adefine] ; [RST]
  ret

;Points word-definition vector at macrod
macro:
  call sdefine
  ; Does NOT fall through to macrod.

;Defines new word and adds it to the MACRO wordlist.
;Used by inter to handle red (color=3, define) words
;  (if the current wordlist is MACRO).
macrod:
  push eax
  mov ecx, [macros]
  inc dword [macros]
  lea ecx, [macro_names + ecx * 4]
  mov eax, macro_addresses - macro_names
  jmp short forthdd

;Points word-definition vector at forthd.
forth:
  call sdefine
  ; Does NOT fall through to forthd.

;Defines new word and adds it to the FORTH wordlist.
;Used by inter to handle red (color=3, define) words
;  (if the current wordlist is FORTH).
forthd:
  push eax
  mov ecx, [forths]    ; current count of Forth words
  inc dword [forths]   ; make it one more
  lea ecx, [forth_dictionary_names + ecx * 4] ; point to the slot for the next definition
  mov eax, forth_dictionary_addresses - forth_dictionary_names

forthdd:
  mov edx, [ - 4 + edi * 4] ; grab pre-parsed word, load the packed word from source block
  and edx, byte -16 ; Clear color bits (bits 0..3).
  mov [ecx], edx ; store the "naked" word in the dictionary
  mov edx, [H] ; HERE pointer, place available for new compiled code
  mov [ecx + eax], edx ;   into wordlist's address array.
  lea edx, [ecx + eax]        ; get address of address-array cell.
  shr edx, 2      ; convert it into DWORD address.
  mov [last], edx      ; store it (to allow optimization).
  pop eax
  mov [list], esp      ; ?
  mov dword [lit], adup      ; ?
  ; for CLASS stuff, search web for moore01a.pdf
  test dword [class], -1      ; is CLASS set for C18 compilation?
  jz .9          ; return if not
  jmp [class] ; otherwise do C18 compilation
  .9:
    ret ; otherwise simply return.

; ColorForth's compiler works by adding bytes of machine code to the end of a buffer called
; the dictionary. (This buffer starts at address 0x100000 - the one-megabyte mark - and
; extends upward in memory.) ColorForth maintains a pointer to the end of this buffer in
; the variable h.
;
; Words in the MACRO wordlist are executed at compile-time in order to add bytes of machine
; code to the definition of the word currently being defined. These words also provide
; opportunities for other macro words to optimize the machine code generated. Sometimes a
; word will leave in list the address of the machine code that the word adds to the
; definition. Another word will check list to see if a specific instruction is in the
; definition, and if so, the word may take the instruction out or substitute another one.
;
; DROP
;
; The word drop removes the top item from the data stack. Specifically, it moves the second
; item on the stack into the TOS register, overwriting the original top item, and adjusts
; the NOS register to point to what was the third item on the stack. The result is one cell
; fewer on the stack.
;
; ColorForth implements drop as the one-byte LODSD instruction (machine-code value: 0xAD).
; Therefore the routine to compile a drop simply adds the machine-code value for LODSD at
; the end of the dictionary and increments the end-of-dictionary pointer by one byte.
; However, this version also stores the old end-of-dictionary pointer (the location of the
; LODSD instruction being added) into list, so that if another routine (such as qdup) wants
; to optimize away the LODSD, it can do so.
;

; compile a DROP instruction
cdrop:
  mov edx, [H]            ; get HERE pointer to newly compiled code
  mov [list], edx         ; store at LIST for tail optimization
  mov byte [edx], op_drop ; compile LODSD=DROP
  inc dword [H]           ; update HERE pointer
  ret

; DUP
;
; The word dup pushes a copy of TOS onto the data stack. Specifically, it adjusts the NOS
; register to make room for an extra cell on the stack and copies TOS into this extra cell.
; The result is one cell more on the stack.
;
; The ColorForth kernel defines two routines for compiling dup: qdup and cdup.
;
; Optimized DUP (qdup)
;
; The word ?dup is often used at the beginning of a macro word to compile a dup only if the
; definition of the word being defined does not already end with a drop (i.e., a LODSD
; instruction). If the drop is there, it is removed, and dup is not compiled.

; compile a DUP if necessary (uses tail optimizer)
qdup:
  mov edx, [H] ; get HERE pointer
  dec edx ; point back to last byte compiled
  cmp [list], edx ; is it subject to tail optimization?
  jne cdup ; if not, go ahead and compile the DUP
  cmp byte [edx], op_drop ; is it a DROP instruction?
  jne cdup ; if not, compile the DUP
  ; so it's a DROP instruction, and we were about to compile a DUP.
  ; DROP followed by DUP is a no-op, so point HERE to before the DROP
  ; was compiled, and we've eliminated a one-byte DROP, a 5-byte DUP,
  ; and the wasted runtime they would have consumed
  mov [H], edx ; move HERE back one byte
  ret

; Non-optimized DUP (cdup)
;
; If the programmer knows that the preceding instruction was not a drop, he can just compile a
; dup directly.
;
; Note that the dword and byte being added to the dictionary correspond to the five bytes (in
; hex) 8D 76 FC 89 06. The first three bytes are the machine code for the instruction lea esi,
; [esi-4]; the last two correspond to mov [esi], eax. (ESI is NOS, the pointer to the next
; [second] item on the stack. The stack grows downward, and the size of each item on the stack
; is four bytes, so lea esi, [esi-4] makes room on the stack for another item. EAX is TOS, the
; register containing the top item on the stack, so mov [esi], eax copies TOS into the new
; space.)
; compile a DUP instruction
cdup:
  mov edx, [H]                ; get HERE pointer
  ; disassembly of DUP:
  ; 8d 76 fc    lea    esi,[esi-4]
  ; 89 06       mov    DWORD PTR [esi],eax
  mov dword [edx], op03_dup   ; compile the DUP
  mov byte [edx + 4], op4_dup
  add dword [H], byte 5       ; adjust HERE accordingly, point past compiled code
  ret

; THE INTERPRETER

; This isn't the entire interpreter; the complete list of routines called by inter is at spaces/adefine.

adup:
  DUP_
  ret

var1:
  DUP_
  mov eax, [forth_dictionary_names + 4 + ecx * 4]
  ret

; VARIABLE
;
; The interpreter calls variable when it encounters a magenta word (a variable to be defined).
;
; Recall that ColorForth defines two wordlists - FORTH and MACRO. If a pre-parsed word is
; green, then what the interpreter does with the word depends on the wordlist in which the
; word is found. If the word is in the MACRO wordlist, then the word's definition (machine
; code) is executed immediately. If the word is in the FORTH wordlist, then a CALL to the
; word's definition is compiled at the end of the dictionary (machine-code buffer).
;
; Recall also that ColorForth defines each wordlist as a pair of arrays - a word array and an
; address array. A pre-parsed word is sought in the word array. If a match is found, then the
; address of the word's definition will be found at the same offset in the address array.
;
; Defining a new word usually creates one new entry - one new word and one new address - in
; one of the two wordlists. Defining a variable is more expensive; a variable definition uses
; four wordlist entries - two entries in each wordlist. The data added to the wordlists are as
; follows:
;
; FORTH wordlist
; Word (name) Address
; word var1
; [4+ecx]
;
; MACRO wordlist
; Word (name) Address
; word var2
; [4+ecx]
;
;
; When the interpreter finds a magenta word, it calls variable to handle the word. Variable
; calls forthd to add the word to the FORTH wordlist, set the ECX register to the address of
; the word-array cell that contains the new word, and set the word's address field to the
; address of the end of the dictionary as usual. Variable stores into this address field the
; address of var1, which does nothing except push [4+forth0+ecx*4] onto the stack. Variable
; then creates a dummy entry in the FORTH wordlist, into which is stored the address of the
; pre-parsed word after the magenta word being handled. This address goes into the word array;
; the corresponding cell in the address array is left unused. (This word-array cell for the
; dummy entry is the address to which "[4+forth0+ecx*4]" refers.)

; Used by inter to handle magenta (color=12, variable) words.

; compile a variable from preparsed source
variable:
  ; note: it doesn't actually compile anything at this point, just adds to
  ; the dictionaries. when the variable is _used_ is when something
  ; gets compiled
  ; first add it to the forth dictionary...
  call forthd
  ; after forthd, ECX is already offset by loadaddr
  mov dword [forth_dictionary_addresses - forth_dictionary_names + ecx], var1
  inc dword [forths] ; dummy entry for source address
  mov [4 + ecx], edi ; EDI is source address containing variable's value

  ; Then variable creates parallel entries in the MACRO wordlist. This operation is the same as
  ; above, except that variable calls macrod instead of forthd, and it stores, not the address of
  ; var1, but the address of the routine following variable (marked in the code with a local
  ; label "@@"), which I choose to call "var2".

  call macrod
  ; after call to macrod, ECX is offset by loadaddr
  mov dword [macro_addresses - macro_names + ecx], .var
  inc dword [macros]
  mov [4 + ecx], edi

  ; The wordlist entries created, variable increments the pre-parsed-word pointer, so that the
  ; interpreter will not interpret the value following the magenta word (which of course is the
  ; value of the variable).
  inc edi ; update source word pointer to following word
  ret

  ; So how is a variable used after it is defined?
  ;
  ; Chuck Moore states that yellow variables are preferred on the Pentium, that is, you use a
  ; variable by having its address pushed onto the stack at compile time. When the interpreter
  ; finds a yellow word, it looks up the word in the FORTH wordlist and executes the code at the
  ; address found in the word's entry. In this case, the address is that of var1, which grabs the
  ; address from the word field of the next (dummy) entry in the wordlist and pushes it onto the
  ; stack at compile time. This address is of course that of the variable's value, stored within
  ; the pre-parsed source code.
  ;
  ; What if a variable name is green? Chuck Moore states: "A green variable compiles a call to
  ; code executed at run time to put the address on the stack." When the interpreter finds a
  ; green word, it looks up the word first in the MACRO wordlist, then in the FORTH wordlist. In
  ; this case, the interpreter will find the variable name in the MACRO wordlist (since the name
  ; was added to both wordlists). The address in the MACRO wordlist is that of "var2" below
  ; which generates code to push the address of the variable's value onto the stack at run time.
  ;
  ; "var2"
  ;
  ; -- routine called if variable is invoked as a MACRO --
  .var:
    call [lit]
    mov eax, [macro_names + 4 + ecx * 4]
    jmp short cshort_

; CNUM
;
; Cnum ("compile number") takes a 32-bit number from the pre-parsed source code and
; compiles it into the machine code needed to push that number onto the stack.
;
; A 32-bit number is stored in the pre-parsed code as two 32-bit cells. Only the
; bottom five bits of the first cell are used the bottom four bits are the "color,"
; in this case color five (a "green" number to be compiled), and the other bit
; indicates whether the number should be displayed as decimal or hexadecimal. The
; second cell contains the 32-bit number.
;
; Whereas most routines called by inter (the interpreter loop) access the current pre-
; parsed word via [-4+edi*4], cnum needs to access the following pre-parsed word (the
; 32-bit number itself) via [edi*4].

; compile a number
cnum:
  call [lit]
  mov eax, [edi * 4] ; get the number from preparsed source
  inc edi
  jmp short cshort_ ; join common code below

; CSHORT
;
; Cshort ("compile short number") takes a 27-bit number from the pre-parsed source
; code and compiles it into the machine code needed to push that number onto the
; stack.
;
; A number small enough to be represented in 27 or fewer bits is stored in the pre-
; parsed source code as a single cell. The bottom four bits contain the "color," in
; this case color six (a "green" number to be compiled), the next bit indicates
; whether to display the number as decimal or hexadecimal, and the other 27 bits
; contain the number.

; used by inter to handle green (color=6, 27-bit number) words.
; compile short (27 bits or less) signed number
cshort:
  call [lit]
  mov eax, [ - 4 + edi * 4] ; get number from pre-parsed source
  sar eax, 5 ; get rid of hexbit and tagbits

cshort_: ; common code for both cnum and cshort
  call literal
  DROP
  ret

alit:
  mov dword [lit], adup

; literal

; Literal takes the 32-bit number at the top of the stack and compiles from it the
; machine code needed to push that number onto the stack later. (In most cases it
; generates a dup followed by a MOV EAX, <32_bit_number> instruction.) Note that
; this routine does not drop the number from the stack after compiling it.

; compile the literal (number) in EAX
literal:
  call qdup                    ; compile code to preserve top of stack
  mov edx, [list]              ; get what was last saved in 'list', preserve previous instruction address
  mov [list + 4], edx          ; save that in the next slot, for optimization
  mov edx, [H]                 ; save address of the "mov eax" instruction about to be compiled
  mov [list], edx              ; and store that in 'list'
  mov byte [edx], op_mov_eax_n ; compile "mov eax, literal"
  mov [edx + 1], eax           ; store the N
  add dword [H], byte 5        ; adjust HERE pointer
  ret
;
; Qcompile retrieves the next pre-parsed word from the source code and looks it up,
; first in the MACRO wordlist, then in the FORTH wordlist. If the word is found in
; the MACRO wordlist, the word's definition is executed. If the word is found in the
; FORTH wordlist, a CALL to the word's definition is compiled into the dictionary.
; If the word is found in neither, the interpreter aborts.

; used by inter to handle green (color=4, compile) words.
qcompile:
  call [lit]                                        ; For info, search for "lit: dd adup"
  mov eax, [ - 4 + edi * 4]                         ; Get next pre-parsed word
  and eax, byte -16                                 ; mask out color bits (bits 0..3)
  call mfind                                        ; locate word in macro dictionary
  jnz .0                                            ; if failed, try in Forth dictionary
  DROP                                              ; if found in MACRO wordlist, restore EAX
  jmp [macro_addresses + ecx * 4]                   ; execute its definition immediately, jump to macro code
  .0:
    call find                                       ; otherwise, find it in FORTH wordlist.
    mov eax, [forth_dictionary_addresses + ecx * 4] ; load code pointer in case there was a match

; Call_ takes the routine address at the top of the stack and compiles from it the
; machine code needed to call that routine.
qcom1:  
  jnz abort               ; abort if no match in dictionary
  mov edx, [H]            ; get HERE pointer to where new compiled code goes
  mov [list], edx         ; save that in 'list' for tail optimization
  mov byte [edx], op_call ; Store CALL opcode into dictionary.
  add edx, byte 5         ; whole instruction including offset is 5 bytes
  sub eax, edx            ; it has to be a 32-bit offset rather than absolute address
  mov [edx - 4], eax      ; store it after the "call" instruction
  mov [H], edx            ; point HERE to end of just-compiled code
  DROP                    ; restore EAX from data stack
  ret

; Used by inter to handle cyan (color=7, compile macro) words.
compile:
  call [lit]
  mov eax, [ - 4 + edi * 4]
  and eax, byte -16 ; mask out tag bits
  call mfind
  mov eax, [macro_addresses + ecx * 4]
  jmp qcom1

; Used by inter to handle yellow (color=8, 27-bit number) words.
short_:
  mov dword [lit], alit
  DUP_
  mov eax, [ - 4 + edi * 4]
  sar eax, 5
  ret

; Used by inter to handle yellow (color=2, 32-bit number) words.
num:
  mov dword [lit], alit
  DUP_
  mov eax, [edi * 4]
  inc edi
  ret

; Writing machine code into the dictionary
;
; These words allow the ColorForth programmer to create MACRO words that write
; machine code into the dictionary, from one to four bytes at a time.
; 
; compile code for: , ,1 ,2 & ,3

; Moves a cell (four bytes) of machine code to the end of the dictionary.
comma:
  mov ecx, 4

dcomma:
  mov edx, [H] ; get HERE pointer to where code is compiled
  mov [edx], eax ; move what's at TOS into that location
  DROP
  lea edx, [ecx + edx] ; adjust HERE for number of bytes compiled
  mov [H], edx ; update HERE
  ret

; Moves one byte of machine code to the end of the dictionary.
comma1:
  mov ecx, 1
  jmp dcomma

; Moves two bytes of machine code to the end of the dictionary.
comma2:
  mov ecx, 2
  jmp dcomma

; Moves three bytes of machine code to the end of the dictionary.
comma3:
  mov ecx, 3
  jmp dcomma

; Macro words

; The semicolon (;) is another optimizing word. It works like this: If the last
; optimizable instruction compiled was a CALL, change this to a JuMP; otherwise add
; a RETurn instruction to the end of the dictionary.
;
; One nice side effect of this, as Chuck Moore has observed, is that this allows
; tail recursion of words in the FORTH wordlist, like this:
;
; word ... condition if word ; then ... ;
;
; If a word calls itself, it would normally fill the return stack with copies of the
; same return address but if it jumps to the beginning of its own definition
; instead, the return stack is not overfilled.
 
; compile code for a semi-colon, end definition
; compiles code to return to caller (either JMP or RET).
semi:
  mov edx, [H]                 ; get HERE pointer
  sub edx, byte 5              ; peek back 5 bytes
  cmp [list], edx
  jne .0
  cmp byte [edx], op_call      ; was it a 'call' instruction? (e8=call, e9=jmp)
  jne .0                       ; skip ahead if not
  inc byte [edx]               ; tail optimization, turn 'call' into 'jmp' (0xe9)
  ret
  .0:
    mov byte [edx + 5], op_ret ; compile a 'ret' at end
    inc dword [H]              ; make 'here' pointer reflect that
    ret

then:
  mov [list], esp
  mov edx, [H]
  sub edx, eax
  mov [eax - 1], dl
  DROP
  ret

begin:
  mov [list], esp

here:
; Returns end-of-dictionary pointer on data stack.
  DUP_
  mov eax, [H]
  ret

h_:
; Returns end-of-dictionary pointer on data stack.
  DUP_
  mov eax, H
  ret

; qlit: If the last instruction in the dictionary is "MOV EAX, literal", move the
; literal onto the data stack and remove the instruction from the dictionary. If the
; instruction was preceded by a DUP, remove that also. Returns with zero flag set if
; nothing placed on stack, cleared if literal on stack.
;
; usage: ... ?lit if do-something-with-literal drop then ... ;
qlit:
  mov edx, [H]
  lea edx, [edx - 5]
  cmp [list], edx
  jne .1
  cmp byte [edx], op_mov_eax_n
  jne .1
  DUP_
  mov eax, [list + 4]
  mov [list], eax
  mov eax, [edx + 1]
  ; disassembly of DUP:
  ; 8d 76 fc    lea    esi,[esi-4]
  ; 89 06       mov    DWORD PTR [esi],eax
  cmp dword [edx - 5], op03_dup
  je .0
  mov [H], edx
  jmp cdrop
  .0:
    add dword [H], byte -10     ; flag nz
    ret
  .1:
    xor edx, edx                ; flag z
    ret

less:
  cmp [esi], eax
  js .9        ; flag nz
  xor ecx, ecx ; flag z
  .9:
    ret

; qignore

; If the interpreter has found the last pre-parsed word (a "null" cell), qignore
; pops the return stack twice. The effect is to return to the caller of load.
; Qignore has to pop two items from the return stack: the address to return to
; load, and the value that was in EDI before load pushed it onto the return stack.
; The RET instruction returns control to the routine that called load.
;
; (MY THEORY: Popping a return address and returning to the caller's caller might
; save a code-cache flush-and-refill, because if you return to a caller, this
; potentially causes the processor to flush the code cache and refill it if the
; caller's code is, for whatever reason, not in the cache -- not likely, but
; possible -- This is especially wasteful if the caller just executes another RET
; immediately afterward, because that's potentially another flush-and-refill. This
; is a VERY small concern in this case, of course, but Chuck Moore has implied that
; you keep improving your software by handling these small concerns because the
; effects of doing so build up eventually, and you end up with smaller, faster
; code.)

; used by inter to handle extension (color=0) words.
qignore:
  ; is this a continuation of the previous word? ignore if so
  test dword [ - 4 + edi * 4], -16  ; valid packed word ? unless word's top 28 bits are null, return to interpreter.
  jnz nul        ;   
  ; otherwise call it a NUL and terminate the LOAD
  pop edi        ; [RST] pop the RET address from the call to qignore, exit interpreter
  pop edi        ; [RST] now pop the saved EDI which was PUSHed by LOAD
nul:
  ret ; return to caller (where LOAD was called if NUL)

; jump

; The word jump introduces a jump table within ColorForth code, like this:

; word ... number jump word0 word1 word2 ... ;

; Here, number causes some number N to be pushed onto the stack, and jump takes that
; number and jumps to wordN (the Nth word afterward).
;
; Note that the code here expects that each entry in the jump table is five bytes
; long, with the address of the word's definition in the latter four bytes which
; is what compiling a word in the FORTH wordlist generates: a CALL to an address. A
; jump table containing MACRO words, generally speaking, will not work.
jump:
  pop edx          ; [RST]
  add edx, eax
  lea edx, [5 + edx + eax * 4]
  add edx, [edx - 4]
  DROP
  jmp edx

; load
;
; Load does not load blocks from disk; the blocks are assumed to be in memory
; already. Load simply multiplies the block number by 256 (0x100) to get the block's
; address. (Thus block 18 (0x12) is at address (0x12 shl 8) or 0x1200.) The
; interpreter begins grabbing the pre-parsed words already in memory and using each
; word's color bits to determine the routine to use to handle that word.

; ( b -- ) Interprets pre-parsed words in the block given.
load:
  sub eax, 18               ; block, delete 18, because we don't have binary content
  shl eax, 10-2             ; multiply by 256 longwords, same as 1024 bytes
  mov ebx, [blocks_address] ; ebx contains the ADDRESS of the block contents
  shr ebx, 2                ; divide the address by 4
  add eax, ebx
  push edi                  ; [RST] save EDI register in case it's being used by interpreter
  mov edi, eax
  DROP                      ; drop block number from data stack

; inter

; Note that inter is an endless loop. The only way to return from the interpreter
; (e.g., when the interpreter has no more pre-parsed words to interpret) is for a
; routine called by the interpreter (such as qignore) to pop an item from the return
; stack before returning (and thus to return to inter's caller). (Only qignore
; checks for the last word in the pre-parsed code, so there is no need for inter to
; do it.)
inter:
  mov edx, [edi * 4]      ; get next longword from block
  inc edi                 ; then point to the following one
  and edx, byte 15        ; get only low 4 bits, the type tag
  call [spaces + edx * 4] ; call the routine appropriate to this type
  jmp inter               ; loop till "nul" reached, which ends the loop

; 16 Interpreter Vectors:
;
; 0 extension
; 1 execute yellow
; 2 execute yellow 32bit number
; 3 define red
; 4 compile green
; 5 compile green 32bit number
; 6 compile green 27bit number
; 7 compile cyan
; 8 execute yellow 27bit number
; 9 comment white
; a Capitalized white
; b all caps white
; c variable magenta
; d null
; e null
; f null

align 4
; these are the compiler actions for the various type tags
spaces:
  dd qignore, execute, num        ; colors 0..2: 0 = extension, 1=execute, 2=executelong
; 3=define, either in macro dictionary or forth dictionary
adefine: ; where definitions go, either in macrod (dictionary) or forthd
  dd forthd          ; color 3 ; TODO is forthd + 5
    ; This is altered by sdefine, which stores the
    ; address of either macrod (to define a MACRO
    ; word) or forthd (to define a FORTH word) here.
  dd qcompile, cnum, cshort, compile    ; colors 4..7: 4 = green, 5 = long, 6 = short, 7 = cyan
  dd short_, nul, nul, nul        ; colors 8..11: 8 = executeshort
  dd variable, nul, nul, nul      ; colors 12..15: 12 = variable

; Other variables
;
; lit
;
; This code vector points to either of two routines:
;
; 1 - adup: The routines forthd/macrod and alit store the address of adup into lit.
; 2 - alit: The routines execute, short_, and num store the address of alit into lit.
; The routines that set lit to point to alit are all routines that handle yellow
; cells (words or numbers), so alit is needed only to handle immediate execution of
; words, and not for defining or compiling. Lit usually points to adup it is reset
; to adup's address not only whenever a word is being defined but also whenever alit
; is called.
;
; (But is [lit] called during handling of current word, or between words? I remember
; something about the transition between yellow and green words, or vice versa,
; causing something special with the compiler.)
;
; It is helpful here to check out what each of the interpreter subroutines does with
; the stack. Nul of course does nothing, and qignore, macrod, forthd, and variable
; all scrupulously avoid altering the stack. Execute uses the stack but then
; restores it to what it was. Num and short_ both leave something on the stack,
; while the remaining routines; qcompile, cnum, cshort, and compile, all execute
; "call [lit]" first thing, use the TOS without first executing a DUP, and then DROP
; later.
;
; Now look at what the two lit routines do. Adup duplicates the TOS and returns,
; while alit, like qignore and the word-definition routines, scrupulously avoids
; altering the stack.
;
; Now we begin to see how these routines work together. Num and short_ both leave
; something on the stack, and they both set lit to alit. The following invocation of
; any of qcompile, cnum, cshort, or compile does a call [lit], which results in alit
; being executed, which avoids altering the stack but does create a mov eax,
; <literal> instruction using the TOS (the number placed on the stack by either num
; and short_) as the literal. Once alit is called, lit is restored to adup, so that
; if qcompile or any other routine that does a call [lit] is called again, adup is
; called, which just preserves the TOS and returns, so that the caller can safely
; overwrite the TOS.
;
; Note that execute does not leave anything on the stack itself, but it is invoked
; when the interpreter finds a yellow word, whose definition is supposed to leave
; something (a single number, per Chuck Moore) on the stack. Other than that, it
; acts like num and short_, in that it sets lit to alit, so that the item left on
; the stack can be compiled into a mov eax, <item> instruction.

lit:  dd adup
mk: dd 0, 0, 0        ; macros, forths, H

; h
;
; This is the pointer to the end of the code space the next byte to be overwritten
; when a word needs to be compiled. The code space begins at address 0x100000 (the
; one-megabyte mark), so if the code space is empty, h is set at 0x100000. As the
; compiler adds bytes of machine code to the code space, h advances.
H:  dd 0x100000 ; here pointer, start compiling here

; last
;
; (forthd stores into "last" the DWORD pointer to last word defined -- the
; "colorless" pre-parsed word stored in the word array of whichever of the two
; wordlists was last updated -- presumably "last" is used for optimizing code)
last: dd 0

; class
;
; From what I can tell, class always contains zero. Empty stores a zero here. Forthd
; will use this as a pointer to code if it is nonzero, but it doesn't store anything
; here. I have found no other references to class anywhere in the kernel, and class
; doesn't seem to be exposed in any way via the two wordlists. I'm guessing that
; this was intended as a way to customize the interpreter by having some custom code
; executed whenever a word is defined, except that I haven't found any way to do
; that except by altering the assembly-language source and reassembling ColorForth.
class:  dd 0 ; used by C18 compiler

; list
;
; This is a list of up to two addresses within the dictionary. Each address is that
; of a previously compiled instruction an instruction that could later be changed
; or removed in order to optimize the machine code.
list: dd 0, 0 ; used by tail optimizer


; Wordlists
;
; ColorForth comes with two wordlists: FORTH and MACRO. A word in the FORTH wordlist
; is compiled as a CALL to the address of the word's definition in the dictionary. A
; word in the FORTH wordlist is executed at compile-time (the word can be green; it
; does not have to be yellow); it is equivalent to an "immediate" word in older
; versions of Forth.
;
; Each wordlist has three parts:
;
; Part FORTH wordlist MACRO wordlist
; Word count forths macros
; Word array forth0 macro0
; Address array forth2 macro2
;
; The word count indicates how many words are defined in the wordlist. The word
; array contains words in pre-parsed format (with color bits set to zero), and the
; address array contains addresses of word definitions. If you can find a given word
; in the word array, the offset to that word will, when applied to the address
; array, yield the address of that word's definition.
;
; In addition to the words provided by the kernel, the wordlists have room for new
; words. The programmer can add up to 512 new words to the FORTH wordlist and up to
; 128 new words to the MACRO wordlist.
;
;
; Example 1: dd 170o shl 25 -- ";" -- A semicolon is 1111 000 (170o) -- seven bits
; wide, so we slide it up by (32-7) or 25 bits
;
; Example 2: dd (24o shl 5+21o) shl 22 -- "lm" -- "l" is 10100 (24o), 5 bits wide --
; "m" is 10001, also 5 bits wide -- slide everything up by (32-5-5) or 22 bits
;
; Example 3: dd ((26o shl 4+3) shl 7+141o) shl 16) -- "fov" -- "f" is 10110, "o" is
; 0011, "v" is 1100 001 -- slide everything up by (32-5-4-7) or 16 bits

forths: dd 0
macros: dd 0
macro_names:  
  dd _semi<<25            ; ;
  dd ((_d<<7|_u)<<7|_p)<<11    ; dup
  dd (((_question<<7|_d)<<7|_u)<<7|_p)<<4   ; ?dup
  dd (((_d<<4|_r)<<4|_o)<<7|_p)<<10  ; drop
  dd (((_t<<7|_h)<<4|_e)<<4|_n)<<13  ; then
  dd ((((_b<<4|_e)<<5|_g)<<4|_i)<<4|_n)<<8  ; begin

num_of_macros equ ($ - macro_names) / 4  ; number of macros in the kernel
  times 128 dd 0

macro_addresses:
  dd semi
  dd cdup
  dd qdup
  dd cdrop
  dd then
  dd begin
  times 128 dd 0

forth_words_names:
  dd (((_b<<4|_o)<<4|_o)<<4|_t)<<13    ; boot
  dd (((_w<<4|_a)<<4|_r)<<5|_m)<<14    ; warm
  dd ((((_p<<4|_a)<<7|_u)<<5|_s)<<4|_e)<<5    ; pause
  dd ((((_m<<4|_a)<<5|_c)<<4|_r)<<4|_o)<<10  ; macro
  dd ((((_f<<4|_o)<<4|_r)<<4|_t)<<7|_h)<<8    ; forth
  dd _c<<27          ; c
  dd (((_s<<4|_t)<<4|_o)<<7|_p)<<12    ; stop
  dd (((_r<<4|_e)<<4|_a)<<7|_d)<<13    ; read
  dd ((((_w<<4|_r)<<4|_i)<<4|_t)<<4|_e)<<11  ; write
  dd (_n<<5|_c)<<23        ; nc
  dd (((((_f<<4|_o)<<4|_r)<<5|_m)<<4|_a)<<4|_t)<<6  ; format
  dd (((_s<<7|_h)<<4|_o)<<5|_w)<<11    ; show
  dd ((((_s<<4|_e)<<4|_r)<<7|_v)<<4|_e)<<8    ; serve
  dd (((_l<<4|_o)<<4|_a)<<7|_d)<<12    ; load
  dd (((_h<<4|_e)<<4|_r)<<4|_e)<<13    ; here
  dd (((_question<<5|_l)<<4|_i)<<4|_t)<<12    ; ?lit
  dd (_3<<7|_comma)<<18          ; 3,
  dd (_2<<7|_comma)<<18          ; 2,
  dd (_1<<7|_comma)<<18          ; 1,
  dd _comma<<25            ; ,
  dd (((_l<<4|_e)<<5|_s)<<5|_s)<<13    ; less
  dd (((_j<<7|_u)<<5|_m)<<7|_p)<<6      ; jump
  dd ((_p<<5|_c)<<4|_i)<<16       ; pci
  dd ((((_d<<4|_e)<<7|_v)<<4|_i)<<5|_c)<<5   ; devic(e)
  dd (((((_a<<5|_c)<<5|_c)<<4|_e)<<7|_p)<<4|_t)<<3  ; accept
  dd ((_p<<4|_a)<<7|_d)<<14      ; pad
  dd ((((_e<<4|_r)<<4|_a)<<5|_s)<<4|_e)<<11  ; erase
  dd (((_c<<4|_o)<<7|_p)<<5|_y)<<11    ; copy
  dd (((_m<<4|_a)<<4|_r)<<7|_k)<<12    ; mark
  dd (((_e<<5|_m)<<7|_p)<<4|_t)<<12    ; empt(y)
  dd (((_e<<5|_m)<<4|_i)<<4|_t)<<15    ; emit
  dd ((((_d<<4|_i)<<5|_g)<<4|_i)<<4|_t)<<8    ; digit
  dd ((((_2<<4|_e)<<5|_m)<<4|_i)<<4|_t)<<8    ; 2emit
  dd _dot<<25              ; .
  dd (_h<<7|_dot)<<18            ; h.
  dd ((_h<<7|_dot)<<4|_n)<<14          ; h.n
  dd (_c<<4|_r)<<23        ; cr
  dd ((((_s<<7|_p)<<4|_a)<<5|_c)<<4|_e)<<7    ; space
  dd (((_d<<4|_o)<<5|_w)<<4|_n)<<12    ; down
  dd (((_e<<7|_d)<<4|_i)<<4|_t)<<13    ; edit
  dd _e<<28          ; e
  dd (_l<<5|_m)<<22        ; lm
  dd (_r<<5|_m)<<23        ; rm
  dd ((((_g<<4|_r)<<4|_a)<<7|_p)<<7|_h)<<5 ; graph
  dd (((((_s<<5|_w)<<4|_i)<<4|_t)<<5|_c)<<7|_h)<<2  ; switch
  dd (((((_f<<4|_r)<<4|_e)<<4|_e)<<7|_z)<<4|_e)<<4  ; freeze
  dd (((_t<<4|_e)<<7|_x)<<4|_t)<<13    ; text
  dd ((_t<<4|_o)<<7|_p)<<17      ; top
  dd ((((_k<<4|_e)<<5|_y)<<7|_b)<<4|_o)<<5    ; keybo(ard)
  dd (((_d<<4|_e)<<7|_b)<<7|_u)<<7      ; debu(g)
  dd (_a<<4|_t)<<24        ; at
  dd ((_plus<<4|_a)<<4|_t)<<17          ; +at
  dd (_x<<5|_y)<<20        ; xy
  dd ((_f<<4|_o)<<7|_v)<<16      ; fov
  dd (((_f<<4|_i)<<5|_f)<<4|_o)<<14    ; fifo
  dd ((_b<<4|_o)<<7|_x)<<14      ; box
  dd (((_l<<4|_i)<<4|_n)<<4|_e)<<15    ; line
  dd ((((_c<<4|_o)<<5|_l)<<4|_o)<<4|_r)<<10  ; color
  dd (((((_o<<5|_c)<<4|_t)<<4|_a)<<4|_n)<<4|_t)<<7  ; octant
  dd (_s<<7|_p)<<20        ; sp
  dd (((_l<<4|_a)<<5|_s)<<4|_t)<<14    ; last
  dd ((((_u<<4|_n)<<7|_p)<<4|_a)<<5|_c)<<5    ; unpac(k)
  dd ((_b<<5|_l)<<7|_k)<<13      ; blk
  dd (((_c<<7|_u)<<4|_r)<<5|_s)<<11    ; curs
  dd (((_w<<4|_o)<<4|_r)<<7|_d)<<12    ; word
  dd ((_e<<7|_k)<<4|_t)<<17      ; ekt
  dd ((((_a<<7|_b)<<4|_o)<<4|_r)<<4|_t)<<9; abort
  dd _h<<25; h
  dd ((((_b<<7|_u)<<5|_f)<<5|_f)<<4|_e)<<4; buffe(r)
  dd (((((_o<<5|_f)<<5|_f)<<5|_s)<<4|_e)<<4|_t)<<5; offset
  dd ((_t<<4|_i)<<5|_c)<<19 ; tic
  dd (((((_w<<4|_i)<<4|_n)<<7|_v)<<4|_e)<<4|_r)<<4; winver (- t | f)
  dd (((_a<<7|_p)<<4|_e)<<4|_r)<<13 ; aper
  dd (((_v<<4|_e)<<5|_s)<<4|_a)<<12 ; vesa
  dd ((((_t<<4|_r)<<4|_a)<<5|_s)<<7|_h)<<8; trash
  dd (((_h<<5|_s)<<7|_v)<<7|_v)<<6 ; hsvv
  dd (((_t<<5|_s)<<4|_i)<<5|_m)<<14 ; tsim
  dd (((((_c<<7|_p)<<4|_o)<<4|_i)<<4|_n)<<4|_t)<<4 ; cpoint
  dd ((((_w<<4|_o)<<4|_r)<<7|_d)<<5|_s)<<7 ; words
  dd (((_q<<7|_k)<<4|_e)<<5|_y)<<9 ; qkey
  dd ((((_d<<4|_i)<<5|_g)<<4|_i)<<4|_n)<<8 ; digin
  dd ((((_q<<5|_w)<<4|_e)<<4|_r)<<4|_t)<<8 ; qwert(y)
  dd (((((_n<<4|_o)<<7|_dash)<<4|_r)<<4|_e)<<4|_t)<<5 ; no-ret
  dd (_r<<7|_question)<<21 ; r?
  dd ((_n<<7|_u)<<5|_l)<<16; nul
  dd ((_c<<4|_a)<<7|_d)<<16; cad
  dd (((_p<<5|_c)<<4|_a)<<7|_d)<<9; pcad
  dd ((((_d<<4|_i)<<5|_s)<<7|_p)<<5|_l)<<4; displ(ay)
  dd (((_a<<5|_c)<<4|_t)<<5|_c)<<14 ; actc
  dd ((((_plus<<5|_l)<<4|_i)<<5|_s)<<4|_t)<<7 ; +list(new)
  dd ((((_i<<4|_t)<<4|_i)<<5|_c)<<7|_k)<<8 ; itick(new)
  dd ((_l<<4|_i)<<5|_s)<<18 ; lis(new)
  dd (_plus<<4|_e)<<21 ; +e(new)
  dd (((((_r<<4|_e)<<4|_t)<<4|_a)<<4|_i)<<4|_n)<<8 ; retain
  dd ((((_k<<4|_e)<<5|_y)<<5|_c)<<7|_h)<<4 ; keych
  dd ((((_u<<4|_t)<<4|_i)<<5|_m)<<4|_e)<<8 ; utime
  dd (((((_e<<4|_n)<<7|_d)<<4|_r)<<4|_a)<<5|_m)<<4; endram
  dd (((_w<<5|_g)<<7|_d)<<5|_s)<<10 ; wgds
  dd (((_o<<5|_l)<<4|_o)<<5|_g)<<14 ; olog
  dd (((_w<<5|_l)<<4|_o)<<5|_g)<<13 ; wlog
  dd (((_c<<5|_l)<<4|_o)<<5|_g)<<13 ; clog
  dd ((_b<<5|_y)<<4|_e)<<16 ; bye
  dd ((((_r<<7|_b)<<4|_a)<<5|_c)<<7|_k)<<5; rback (b – n)
  dd ((((_w<<7|_b)<<4|_a)<<5|_c)<<7|_k)<<4; wback (b n)
  dd ((((((_f<<5|_c)<<4|_r)<<4|_e)<<4|_a)<<4|_t)<<4|_e)<<2 ; fcreate (new)
  dd (((((_f<<5|_c)<<5|_l)<<4|_o)<<5|_s)<<4|_e)<<4 ; fclose (new)
  dd (((((_f<<4|_i)<<4|_o)<<5|_c)<<4|_t)<<5|_l)<<5 ; fioctl (new)
  dd ((((_f<<4|_r)<<4|_e)<<4|_a)<<7|_d)<<8 ; fread (new)
  dd (((((_f<<5|_w)<<4|_r)<<4|_i)<<4|_t)<<4|_e)<<6 ; fwrite (new)
  dd (((_f<<4|_e)<<5|_s)<<5|_c)<<13 ; fesc (new)
  dd ((((_r<<5|_w)<<4|_o)<<4|_r)<<7|_k)<<8 ; rwork (new)
  dd ((((_w<<5|_w)<<4|_o)<<4|_r)<<7|_k)<<7 ; wwork (new)

num_of_forth_words equ ($ - forth_words_names) / 4 ; number of forth words in the kernel
  times 512 dd 0

forth_words_addresses:
  dd host_notimpl; boot
  dd warm
  dd dopause
  dd macro
  dd forth
  dd c_
  dd host_notimpl; stopf; TODO
  dd host_notimpl;readf; TODO
  dd host_notimpl;writef; TODO
  dd nc_
  dd host_notimpl;formatf ; ADR
  dd show
  dd serve
  dd load
  dd here
  dd qlit
  dd comma3
  dd comma2
  dd comma1
  dd comma
  dd less
  dd jump
  dd host_notimpl;north; TODO
  dd host_notimpl;dev; TODO
  dd accept
  dd pad
  dd erase
  dd copy
  dd mark
  dd empty
  dd emit
  dd edig
  dd emit2
  dd dot10
  dd hdot
  dd hdotn
  dd cr
  dd space
  dd down
  dd edit
  dd e
  dd lms
  dd rms
  dd host_notimpl; graph(ic)
  dd switch_
  dd freeze
  dd text1
  dd top
  dd keyboard
  dd debug
  dd at_
  dd pat
  dd xy_
  dd fov_
  dd fifof
  dd box
  dd line
  dd color
  dd octant
  dd sps
  dd last_
  dd unpack
  dd blk_
  dd curs_
  dd _word  ; word
  dd ekeys_ ; ekt

  dd abort; abort
  dd h_ ; h
  dd host_notimpl ; buffe(r)
  dd offset_ ; offset
  dd host_notimpl ; tic
  dd winver; winver (- t | f)
  dd host_notimpl ; aper
  dd host_notimpl ; vesa
  dd trash_ ; trash
  dd host_notimpl ; hsvv
  dd host_notimpl ; tsim
  dd cpoint_ ; cpoint
  dd words_; words
  dd host_notimpl ; qkey
  dd host_notimpl ; digin
  dd qwerty ; qwert(y)
  dd host_notimpl ; no-ret
  dd host_notimpl ; r?
  dd nul; nul
  dd cad_; cad
  dd pcad_; pcad
  dd displ_; displ(ay)
  dd host_notimpl ; actc
  dd host_notimpl ; +list
  dd host_notimpl ; itick
  dd host_notimpl ; lis
  dd host_notimpl ; +e
  dd retain; retain()
  dd keych_; keych
  dd host_notimpl; utime
  dd endram_; endram
  dd host_notimpl ; wgds
  dd host_notimpl ; olog
  dd host_notimpl ; wlog (a n1)
  dd host_notimpl ; clog
  dd bye ; bye
  dd host_notimpl ; rback (b – n)
  dd host_notimpl ; wback (b n)
  dd host_notimpl ; fcreate
  dd host_notimpl ; fclose
  dd host_notimpl ; fioctl
  dd host_notimpl ; fread
  dd host_notimpl ; fwrite
  dd host_notimpl ; fesc
  dd host_notimpl ; rwork
  dd host_notimpl ; wwork
  times 512 dd 0


; Utilities

; General-purpose routines

qwerty:
  ret
 
retain:
  pushad
  call host_write_blocks_file
  popad
  ret

winver:
  DUP_
  mov eax, 1
  or eax, eax
  ret

;( b n -- ) Erase n blocks, starting with block b.
erase:
  mov ecx, eax
  shl ecx, 8
  DROP
  push edi
  mov edi, eax

  ; TODO: check correct access
  sub    edi, 18
  shl    edi, 2+8
  add    edi, [blocks_address]

  xor eax, eax
  rep stosd
  pop edi
  DROP
  ret

;( n -- ) Copy current block to block n, and make that block the current block
copy:
  cmp eax, byte 12          ; can't overwrite machine-code blocks...
  jc abort1                 ; so if we're asked to, abort the operation
  ; push edi                ; TODO
  mov edi, eax              ; get block number into EDI
  sub edi, 18               ; TODO
  shl edi, 2 + 8            ; multiply by 1024 to get physical (byte) address
  push esi                  ; save data stack pointer so we can use it for block move
  mov esi, [blk]            ; get current block number from blk
  sub esi, 18               ; TODO
  shl esi, 2 + 8            ; multiply by 1024 to get address
  mov ecx, 512              ; 256 longwords = 1024 bytes TODO
  add esi, [blocks_address] ; TODO
  add edi, [blocks_address] ; TODO
  rep movsd                 ; move the block from source (ESI) to destination (EDI)
  pop esi                   ; restore data stack pointer
  pop edi
  mov [blk], eax            ; destination block becomes new current block (blk)
  DROP                      ; no longer need the block number
  ret

; move dwords
; ( sa da n -- )    
move:
  mov ecx, eax
  DROP
  mov edi, eax
  shl edi, 2
  DROP
  mov esi, eax
  shl esi, 2
  DROP
  rep movsd
  ret

; (?)Print four numbers --
; show current machine state
debug:
  ; locate character output two lines above bottom
  mov dword [xy], char_padding * 0x10000 + (vertical_chars - 2) * char_height + char_padding
  DUP_
  mov eax, [main]
  push dword [eax]
  call dot
  DUP_
  pop eax
  call dot
  DUP_
  mov eax, [draw]
  call dot
  DUP_
  mov eax, esi ; data stack pointer
  jmp dot

align 4; TODO ? is this needed ?

; Screen variables
;
; Note the relationships among the variables and equates:
;
; The screen width (hp) is 1024 pixels; the screen height (vp) is 768 pixels.
;
; An "icon" is actually a glyph from the ColorForth font a bitmapped image of a
; character. An icon image is actually 16 pixels wide by 24 pixels high, but
; ColorForth puts six pixels of blank space between icons when "emitting" them to
; the screen, so iw (icon width) is 16+6, and ih is 24+6.
;
; The number of characters you can fit on a line on the screen (hc) is thus 46, and
; the number of lines you can fit on the screen (vc) is 25.
;
; The left margin (lm) is 3 pixels from the left edge of the screen. The right
; margin (rm) the X position beyond which characters must not be emitted is the
; character width times the number of characters per line, or 1012.
;
; The code to calculate an address in video memory implies that the video card
; reserves 64KB (0x10000 bytes) per scanline even though a 16-bit color mode
; requires only two bytes per pixel, the resolution requires 1024 pixels per
; scanline, and therefore you don't really need more than 2KB to store a scanline in
; main memory. This is probably another case of Chuck Moore tightly fitting his code
; to his own hardware his video card maps its onboard RAM (displ) to main-memory
; address 0xF0000000 and starts scanlines at addresses 0xF0010000, 0xF0020000, etc.

nc: dd 9
xy: dd char_padding * 0x10000 + char_padding
lm: dd char_padding
rm: dd horizontal_chars * char_width; screen_width + char_padding
xycr: dd 0
fov: dd 10 * (2 * screen_height + screen_height / 2) ;512
cpoint: dd 0

; Getting variable addresses
nc_:
  DUP_
  mov eax, nc
  shr eax, 2
  ret

xy_:
  DUP_
  mov eax, xy
  shr eax, 2
  ret

fov_:
  DUP_
  mov eax, fov
  shr eax, 2
  ret

sps:
  DUP_
  mov eax, spaces
  shr eax, 2
  ret

last_:
  DUP_
  mov eax, last
  shr eax, 2
  ret

blk_:
  DUP_
  mov eax, blk
  shr eax, 2
  ret

curs_:
  DUP_
  mov eax, curs
  shr eax, 2
  ret

ekeys_:
  DUP_
  mov eax, ekeys
  shr eax, 2
  ret

trash_:
  DUP_
  mov eax, trash
  shr eax, 2
  ret

offset_:
  DUP_
  mov eax, offset
  shr eax, 2
  ret

keych_:
  DUP_
  mov eax, keych
  shr eax, 2
  ret

cpoint_:
  DUP_
  mov eax, cpoint
  shr eax, 2
  ret

endram_:
  DUP_
  mov eax, endram
  shr eax, 2
  ret

words_:
  DUP_
  mov eax, words
  shr eax, 2
  ret

pcad_:
  DUP_
  mov eax, pcad
  shr eax, 2
  ret

cad_:
  DUP_
  mov eax, cad
  shr eax, 2
  ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; include 'gen.asm'
align 4; TODO is this needed ?

;displ: dd 0x0
frame: dd 0x0
fore: dd _yellow

xc: dd 0
yc: dd 0

displ_:
  DUP_
  mov eax, display
  shr eax, 2
  ret

; change 8:8:8 bit format to 5:6:5
rgb: 
  ror eax, 8 ; rotate blue bits into upper word
  shr ax, 2 ; drop two low bits of green
  ror eax, 6 ; rotate green into upper word, leaving only red in AX
  shr al, 3 ; drop low 3 bits of red
  rol eax, 6+5 ; now rrrrrggggggbbbbb
  and eax, 0f7deh ; remove low bit of each color 0b1111011111011110
  ret

white:
  DUP_
  mov eax, _white ; 8:8:8 rgb, full brightness

color:
  ;call rgb
  mov [fore], eax
  DROP
  ret

fifof:
  DROP

graphic:
  ret

switch_:
  ;push esi
  ;mov esi, [frame + 4]
  ;push edi
  ;mov edi, [displ]
  ;mov ecx, screen_width * screen_height * screen_depth / 4 ; how many doublewords to move
  ;rep movsd ; move ecx double words from esi to edi
  ;pop edi
  ;pop esi
  pushad
  ;push esi
  ;push edi
  call host_flip_screen
  ;pop edi
  ;pop esi
  popad
  ret

switch:
  call switch_
  jmp dopause

; was called clip
; set EDI, xc, and yc from xy.
; ensure that neither xc nor yc is negative.
; convert xy to pointer (edi) into 'frame' clipped to screen width/height
; ( -- )
pen_addr:
  mov edi, [xy]
  mov ecx, edi
  test cx, cx
  jns .0
  xor ecx, ecx
  .0:
    and ecx, 0xffff
    mov [yc], ecx
    imul ecx, screen_width * screen_depth
    sar edi, 16
    jns .1
    xor edi, edi
  .1:
    mov [xc], edi
    lea edi, [ecx + edi * screen_depth]
    add edi, [frame]
    ret

; display word as 16 pixels
; edi=>location in frame
; edx=color
; currently unused!!!
bit16:
  lodsw
  xchg al, ah
  mov ecx, icon_width
  .0:
    shl ax, 1
    jnc .1
    mov [edi], edx
  .1: 
    add edi, byte screen_depth
    NEXT .0
    ret

; display byte as 32 pixels
; edi=>location in frame
; edx=color

bit32:
  lodsw
  xchg al, ah
  mov ecx, icon_width
  .0:
    shl eax, 1
    jnc .1
    mov [edi], edx
    mov [edi + screen_depth], edx
    mov [edi + screen_width * screen_depth], edx
    mov [edx + (screen_width + 1) * screen_depth], edx
  .1:
    add edi, byte 2 * screen_depth
    NEXT .0
    ret

; ( c -- ) emit
; paint a character on the screen
emit:
  call qcr ; issue CRLF if at end of line
  push esi ; save registers we need...
  push edi
  push edx
  imul eax, byte icon_width * icon_height / 8 ; index into icon table...
  mov esi, [icons_address] ; point to the bit-representation of this character
  add esi, eax
  call pen_addr
  mov edx, [fore] ; get foreground color into EDX:w
  mov ecx, icon_height
  .0:
    push ecx
    call bit16
    add edi, (screen_width - icon_width) * screen_depth
    pop ecx
    NEXT .0
  pop edx ; restore registers...
  pop edi
  pop esi

bl_:  
  DROP

space: 
  add dword [xy], char_width * 0x10000
  ret

; display a double - size character.
; : 2emit ( c -- )
emit2:
  push esi
  push edi
  push edx
  imul eax, byte icon_width * icon_height / 8
  mov    esi, [icons_address]
  add    esi, eax
  call pen_addr
  mov edx, [fore]
  mov ecx, icon_height
  .0:
    push ecx
    call bit32
    add edi, 2 * (screen_width - icon_width) * screen_depth
    pop ecx
    NEXT .0
    pop edx
    pop edi
    pop esi
    add dword [xy], 2 * char_width * 0x10000
    DROP
    ret

text1:
  call white
  mov dword [lm], char_padding
  mov dword [rm], horizontal_chars * char_width
  jmp top

; ( x len -- )
; draw a horizontal line LEN pixels long, starting
; X pixels to the left of the current pen position.
line:
  call pen_addr
  mov ecx, [esi]
  shl ecx, screen_depth_log2  ; ecx = ecx * screen_depth
  sub edi, ecx
  mov ecx, eax
  mov eax, [fore]
  rep stosd ; store ecx doublewords from eax to edi
  inc dword [xy]
  DROP
  DROP
  ret

; ( width height -- ) draw a box and fill with foreground color
box:
  call pen_addr
  cmp eax, screen_height + 1 ; past vertical end of screen?
  js .0 ; continue if not
  mov eax, screen_height  ; else set vertical parameter to end of screen
  .0:
    mov ecx, eax
    sub ecx, [yc]
    jle .9
    cmp dword [esi], screen_width + 1
    js .1
    mov dword [esi], screen_width
  .1:
    mov eax, [xc]
    sub [esi], eax
    jle .9
    mov edx, screen_width
    sub edx, [esi]
    shl edx, screen_depth_log2  ; edx = edx * screen_depth
    mov eax, [fore]
  .2:
    push ecx
    mov ecx, [esi]
    rep stosd
    add edi, edx
    pop ecx
    NEXT .2
  .9:
    DROP
    DROP
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; include 'gen.asm'

cyan:
  DUP_
  mov eax, _cyan
  jmp color

magenta:
  DUP_
  mov eax, _mag
  jmp color

silver:
  DUP_
  mov eax, _silver
  jmp color

blue:
  DUP_
  mov eax, _dkblue
  jmp color

red:
  DUP_
  mov eax, _red
  jmp color

green:
  DUP_
  mov eax, _green
  jmp color

history_size  equ 11
history:
  times history_size db 0

; : echo ( ch -- )
; add character to history
echo:
  push esi
  mov ecx, history_size - 1
  lea edi, [history]
  lea esi, [edi + 1]
  rep movsb
  pop esi
  mov [history + history_size - 1], al
  DROP
  ret

; : right ( -- )
; zero history
right:
  DUP_
  mov ecx, history_size
  lea edi, [history]
  xor eax, eax
  rep stosb        ; store al into [edi] ecx times
; therefore "right" just fills the "history" buffer (11 bytes) with zeroes.
  DROP        
  ret

down:
  DUP_        ; push TOS
  xor edx, edx      ; clear high 32 bits of dividend
  mov ecx, char_height    ; get icon height as divisor
  div ecx    ; TOS/ih = eax:quo edx:rem
  mov eax, edx      ; TOS = rem (overwrite quo)
  add edx, char_padding * 0x10000 + 0x8000 - char_height + char_padding
  mov [xy], edx    ; set xy and fall through to zero...

; if TOS=0, set TOS and ZF to 1; otherwise set both to 0.
zero: 
  test eax, eax
  mov eax, 0
  jnz .9
  inc eax
  .9:
    ret

; blanks the screen (draws a black box over the whole screen).
blank:
  DUP_
  xor eax, eax
  mov [xy], eax
  call color
  DUP_
  mov eax, screen_width
  DUP_
  mov eax, screen_height
  jmp box

; sets cursor position to top-left corner.
top:
  mov ecx, [lm] ; get left margin in pixels
  shl ecx, 16 ; shift to X of XY doubleword
  add ecx, byte 3 ; add 3 pixels for top (Y) margin
  mov [xy], ecx ; update XY
  mov [xycr], ecx ; update XYCR (unused? [jc])
  ret

; insert a carriage return if at end of line
qcr:
  mov cx, [xy + 2] ; get X, the horizontal character pointer
  cmp cx, [rm] ; at or past right margin?
  js cr.9  ; no, return

; insert a carriage return (drop to next line)
cr:
  mov ecx, [lm] ; set X to left margin
  shl ecx, 16 ; move to high 16 bits of XY doubleword...
  mov cx, [xy] ; now get Y word...
  add ecx, byte char_height ; and add height of icon (character) to it
  mov [xy], ecx ; update XY pointer
  .9:
    ret

; Setting screen parameters

; set left margin to new pixel amount (high-level FORTH word LM)
lms:
  mov [lm], eax ; top of stack becomes new left margin
  DROP
  ret

; set right margin to new pixel amount (high-level FORTH word RM)
rms:
  mov [rm], eax
  DROP
  ret

; ( x y -- ) set current screen position
at_:
  mov [xy], ax ; top of stack should be Y position
  DROP ; next on stack should be X position
  mov [xy + 2], ax ; X goes in high 16 bits of XY
  DROP ; clean X off the stack before returning
  ret

; (x y -- ) set screen position relative to current
pat:
  add [xy], ax  ; first add Y update
  DROP ; then get X update into EAX
  add [xy + 2], ax ; update X
  DROP ; clean X update off the stack before returning
  ret

octant:
  DUP_
  mov eax, 0x43        ; poly -last y+ x+ ;23h ; last y+ x+
  mov edx, [esi + 4]    ; (if bit 31 in 2nd arg not set, return)
  test edx, edx
  jns .0
  neg edx        ; (negate 2nd arg)
  mov [esi + 4], edx
  xor al, 1      ; (toggle bit 0)
  .0:
    cmp edx, [esi]        ; (cmp modified 2nd arg, 1st arg)
    jns .9          ; (if edx - [esi] signed,)
    xor al, 4      ;    (toggle bit 2)
  .9:
    ret

; Drawing the user interface

; display one line of the onscreen keyboard (eight icons)
; four chars at addr + 12, space, four chars at addr.
; IN: edi = addr - 4
; OUT: edi = addr
eight:
  add edi, byte 12
  call four
  call space
  sub edi, byte 16

fkeys:
  call four
  call space
  call four
  call space

four:
  mov ecx, 4

; display ECX chars from EDI + 4, incrementing EDI with each char.
nchars:
  push ecx
  DUP_
  xor eax, eax
  mov al, [edi] ; QWERTY [edi]
  inc edi
  call emit
  pop ecx
  NEXT nchars
  ret

; show stack picture at lower left of screen
stack:
  mov edi, top_main_data_stack - 4 ; point to 1st element on god data stack
  ; the top-of-stack (TOS) of the god stack, whose pointer is available in
  ; the save slot labeled 'god', contains 'godd', the god data stack pointer.
  ; we're going to compare it to the godd base, and show anything that's
  ; there.
  .0:
    mov edx, [main]
    cmp [edx], edi
    jae .9 ; return if greater or equal  TODO is jnc in others
    DUP_
    mov eax, [edi]
    sub edi, byte 4
    call qdot
    jmp .0
  .9:
    ret

; display the current keyboard layout
keyboard:
  ; color = keyboard color
  call text1
  DUP_
  mov eax, [keyc]
  call color
  ; left margin = keyboard_hud_x
  mov dword [lm], screen_width - 30 * char_width - 3
  mov dword [rm], horizontal_chars * char_width
  mov dword [xy], (screen_width - 30 * char_width - 3) * 0x10000 + screen_height - 2 * char_height - 3
  ; display finger keys
  mov edi, [board]
  test   edi,edi  ; QWERTY
  jz     .0 ; QWERTY
  call   fkeys ; QWERTY
  .0:
    ; display thumb keys (leave a blank line, move 4 chars in).
    call cr
    add dword [xy], 25 * char_width * 0x10000
    mov edi, [shift]
    add edi, byte 16
    mov ecx, 3
    call nchars
    ; display top element of stack (if any), at left.
    mov dword [lm], char_padding
    mov word [xy + 2], char_padding
    call stack
    ; display input history just to the left of keyboard display
    mov word [xy + 2], screen_width - char_padding - (history_size + keyboard_hud_width) * char_width
    lea edi, [history]
    mov ecx, history_size
    jmp nchars


; Editor
;
; Keyboard handling
;
; ColorForth draws a keyboard map in the lower-right corner of the screen, so that
; the user can see what characters will appear as he types. ColorForth draws the map
; from one of the following four tables. ColorForth also uses the tables to
; determine what character to generate when a particular key is pressed.
;
; Any reference to one of these tables is actually a reference to an address four
; bytes before the table, e.g., "alpha-4", "graphics-4", etc. The reason for this is
; that the key codes 4 through 27 are used as offsets into these tables.
;
; The byte values in these tables are ColorForth character codes (octal values 0
; through 57o).
;
; (Say when each of the four tables is active, e.g., what keys have to be held down
; or pressed.)

keyboard_layout_alpha:
  ; right hand
  db Ig, Ic, Ir, Il         ; top
  db Ih, It, In, Is         ; center
  db Ib, Im, Iw, Iv ; bottom
  ; left hand
  db Ip, Iy, If, Ii ; top
  db Ia, Io, Ie, Iu ; center
  db Iq, Ik, Ix, Id ; bottom

keyboard_layout_graphics:
  db I1, I2, I3, 0
  db I4, I5, I6, I0
  db I7, I8, I9, Iquestion

  db Icolon, Isemi, Istore, Ifetch
  db Iz, Ij, Idot, Icomma
  db Itimes, Islash, Iplus, Iminus

keyboard_layout_numbers:
  db I1, I2, I3, 0
  db I4, I5, I6, I0
  db I7, I8, I9, 0

  db 0, 0, 0, 0
  db 0, 0, 0, 0
  db 0, 0, 0, 0

keyboard_layout_octals:
  db I1, I2, I3, 0
  db I4, I5, I6, I0
  db I7, I8, I9, 0

  db 0, Ia, Ib, Ic
  db 0, Id, Ie, If
  db 0, 0, 0, 0

; -- 6 tables, pointed to by shift -- these deal only with the "shift" keys,
; 0=undefined key, 1=N, 2=space, 3=alt -- db bytes are CF char codes --

; layouts for the thumb (shift) keys
; these sort of go in pairs:
; foo0 is for the first character of a word
; foo1 is used for the rest
alpha0:
  dd nul0, nul0, number, nul0  ; handler routines
  db 0, I9, Itimes, 0   ; and icons for display

alpha1:
  dd word0, x, lj, nul0
  db Ix, Idot, Itimes, 0

;graph0:
;  dd nul0, nul0, nul0, alph0
;  db 0, 0, Ia, 0
;
;graph1:
;  dd word0,   x, lj, alph
;  db Ix, Idot, Ia, 0

numb0:
  dd nul0, minus, alphn, octal
  db Iminus, Ia, If, 0

numb1:
  dd number0, xn,  endn, number0
  db Ix, Idot, 0, 0

; Letter is passed a ColorForth key code (pulled from keys). If the key pressed is
; either an undefined key (code=0), the N key (code=1), the spacebar (code=2), or
; either Alt key (code=3), then letter replaces the key code with [edx+eax] =
; [board+code]. Otherwise, letter returns the key code unchanged.
;letter:
  ;cmp al, 4
  ;js .9 ; if al < 4, that is 0, 1, 2, 3
  ;mov edx, [board]
  ;mov al, [eax + edx]
  ;.9:
    ;ret
letter:
  ; filters 0..9 and a..f for numeric, and returns
  ;  flags according to al
  and al, al
  js .9
  cmp dword [shift], numb0  ; numbers?
  jc .2    ; yes
  cmp dword [current], decimal  ; decimal?
  jz .0    ;  yes
  cmp al, 04h    ;  no check for hex
  jz .2
  cmp al, 05h
  jz .2
  cmp al, 0ah
  jz .2
  cmp al, 0eh
  jz .2
  cmp al, 10h
  jz .2
  cmp al, 13h
  jz .2
  .0:
    cmp al, 18h
    jc .1
    cmp al, 22h
    jc .2
  .1:
    xor eax, eax
  .2:
    and al, al    ; set flag
  .9:
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;; QWERTY
; 4 to 231
sdl_scancode_to_raw:
  times 4 db 0, 
  ;   a -> z = 4 -> 29
  ;  a     b     c     d     e     f     g     h
  db 0x1e, 0x30, 0x2e, 0x20, 0x12, 0x21, 0x22, 0x23; a
  ;  i     j     k     l     m     n     o     p
  db 0x17, 0x24, 0x25, 0x26, 0x32, 0x31, 0x18, 0x19; a
  ;  q     r     s     t     u     v     w     x
  db 0x10, 0x13, 0x1f, 0x14, 0x16, 0x2f, 0x11, 0x2d; a
  ;  y     z
  db 0x15, 0x2c
  ;   1 -> 0 = 30 -> 39
  ; numbers
  ;  1     2     3     4     5     6     7     8
  db 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09 
  ;  9     0
  db 0x0A, 0x0B
  ;   enter = 40
  ;   esc = 41
  ;   backspace = 42
  ;   tab = 43
  ;   space = 44
  ;  cr    ESC   bksp  tab   space
  db 0x1c, 0x01, 0x0e, 0x0f, 0x39
  ;   - = 45
  ;   = = 46
  ;   [ = 47
  ;   ] = 48
  ;   \ = 49
  ;   ; = 51
  ;   ' = 52
  ;   top left weird key - 53
  ;  -_    =+    [{    ]}    \|    na     ;:    '"    weird left
  db 0x0c, 0x0d, 0x1a, 0x1b, 0x2b, 0x00,  0x27, 0x28, 0x00
  ;   , = 54
  ;   . = 55
  ;   / = 56
  ;  ,<    .>    /?
  db 0x33, 0x34, 0x35, 
  ;   f1 -> f12 = 58 -> 69
  ;  na    f1    f2    f3    f4    f5    f6    f7    f8    f9    f10   f11   f12
  db 0x00, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f, 0x40, 0x41, 0x42, 0x43, 0x44, 0x57, 0x58
  ;  na
  db 0x00, 0x00, 0x00, 0x00
  ;   home 74 
  ;   pg up 75
  ;   delete = 76
  ;   end 77 
  ;   pgdn 78
  ;   right = 79
  ;   left = 80
  ;   down = 81
  ;   up = 82
  ;  home  pgup  del   end   pgdn  rt    lt    dn    up
  db 0x47, 0x49, 0x53, 0x4f, 0x51, 0x4d, 0x4b, 0x50, 0x48
  ; 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 
  times 17 db 0
  ;   tilde = 100
  ;  ~
  db 0x29
  times 124 db 0
  ;   lshift 225
  ;   lalt 226 
  ;   lcmd 227 
  ;   rshift 229
  ;   ralt 230
  ;   rcmd 231
  ;  lsh   lalt  lcmd  na    rsh   ralt, rcmd
  db 0x00, 0x38, 0x00, 0x00, 0x00, 0x38, 0x00

getkey:
  __SDL_Delay 30
  __SDL_PollEvent event
  cmp eax, 0
  je getkey ; if there was no event, go forward
  ; here, we are sure to have an event
  cmp byte [event.type], SDL_QUIT 
  je host_exit_ok
  cmp byte [event.type], SDL_KEYDOWN ; was it a key event ? 
  jne getkey
  push ebx
  mov ebx, [event.key.keysym.scancode]
  mov eax, [sdl_scancode_to_raw + ebx]
  mov [keych], eax
  pop ebx
  and eax, 0ffh ; get rid of high bits
  jz getkey
  ret
  
getcfkey:
  call getkey
  push eax
  xor eax,eax
  mov [shifted], eax
  test dword [event.key.keysym.mod], KMOD_LSHIFT | KMOD_RSHIFT
  jz .not_shift
  xor eax,eax
  inc eax
  mov [shifted], eax
  .not_shift:
    pop eax
    ret
  
; Reading the keyboard
;
; The key routine reads Set 1 scan codes from the keyboard controller, subtracts
; sixteen (20o or 0x10), and uses the result as an offset into the keys table, which
; contains ColorForth-specific key codes. ColorForth recognizes only Set 1 scan
; codes in the range 0x10 through 0x39 (20o through 71o in octal), a range
; containing forty-one keys. In fact, ColorForth recognizes only twenty-seven of
; these keys.
 
; our values (1 - 27) for the raw keycodes that we use.
align 2
    ; scan code to colorforth char conversion.
    ; the codes are huffman compressed etc... (the huffman index not the actual value !!!!!!!!!)
    ; -1 for backspace/esc, -2 for return/space
    ; and -3 for alt.

keys
    db 00,00
    dw 0ffffh, 2a19h, 2c1ah, 001bh ; 1   esc    !1      @2   #3
    dw 001ch, 001dh, 001eh, 001fh  ; 5   $4     %5      ^6   &7
    dw 2d20h, 0021h, 0018h, 0023h  ; 9   *8     (9      )0   _-
    dw 2b00h, 0ffffh, 0000h, 1717h ; d   +=     bs      tab  Qq
    dw 0f0fh, 0404h, 0101h, 0202h  ; 11  Ww     Ee      Rr   Tt
    dw 0b0bh, 1616h, 0707h, 0303h  ; 15  Yy     Uu      Ii   Oo
    dw 1212h, 0000h, 0000h, 0fefeh ; 19  Pp     {[      }]   ret
    dw 0000h, 0505h, 0808h, 1010h  ; 1d  Lctrl  Aa      Ss   Dd
    dw 0e0eh, 0d0dh, 1414h, 2222h  ; 21  Ff     Gg      Hh   Jj
    dw 2424h, 0c0ch, 2928h, 0000h  ; 25  Kk     Ll      :;   "'
    dw 0000h, 0000h, 0000h, 2626h  ; 29  ~`     Lshift  |\   Zz
    dw 1515h, 0a0ah, 1111h, 1313h  ; 2d  Xx     Cc      Vv   Bb
    dw 0606h, 0909h, 002eh, 0025h  ; 31  Nn     Mm      <,   >.
    dw 2f27h, 0000h, 2d2dh, 0fdfdh ; 35  ?/     Rshift  *    Lalt
    dw 0fefeh                      ; 39  space  

; There are twenty-eight ColorForth-specific key codes. The blue cells are for keys 
; you press with your right thumb; the green, for keys you press with the four 
; fingers on your right hand; the red, for keys you press with the four fingers on 
; your left hand. 
;
; Key code Key    Key code Key    Key code Key
; 0 Unused key    10  L   20  A
; 1 N     11  ;   21  S
; 2 Space   12  M   22  D
; 3 Alt     13  <   23  F
; 4 U     14  >   24  Z
; 5 I     15  ?   25  X
; 6 O     16  Q   26  C
; 7 P     17  W   27  V
; 8 J     18  E
; 9 K     19  R
;
; So we can see what characters are produced for each of the four keyboard modes. In 
; alpha mode, the keyboard looks like this (the large character is the one produced 
; by the key; the small character is the one printed on the key; the gray cells 
; indicate keys that don't produce any characters): 
;
; y f i     g c r l
; Q W E R T Y U I O P
;
; o e u     h t n s
; A S D F G H J K L ; 
;
; k x d     b m w v
; Z X C V B N M < > ?
; 
; In graphics mode, the keyboard looks like this: 
;
; ; ! @     1 2 3
; Q W E R T Y U I O P
;
; J . ,     4 5 6 0
; A S D F G H J K L ; 
;
; / + -     7 8 9 ?
; Z X C V B N M < > ?
; 
; In numbers mode, the keyboard looks like this: 
;
;    1 2 3
; Q W E R T Y U I O P
;
;    4 5 6 0
; A S D F G H J K L ; 
;
;    7 8 9  
; Z X C V B N M < > ?
; 
; In hexadecimal mode, the keyboard looks like this: 
;
; a b c     1 2 3
; Q W E R T Y U I O P
;
; d e f     4 5 6 0
; A S D F G H J K L ; 
;
;    7 8 9  
; Z X C V B N M < > ?
; 
; Key waits for a key, then returns the key code (a number in the range 0..27). It
; first calls pause to let the other task run, then checks the key port to see if a
; key has been pressed. If not, key keeps calling pause and checking the key port
; until a key is pressed.
;
; If a key is pressed, the scan code must be between 20o (0x10) and 71o (0x39), or
; else the key is ignored key calls pause and checks the key port again. If the
; scan code is acceptable, key uses it to grab the ColorForth key code from the keys
; table.

; returns huffman coded chars or -1 (Esc), -2 (spacebar or Enter) or -3 (Alt)
; zero is filtered
; sets flags according to al
key:
  DUP_
  push esi
  push edi
key0:
  xor eax, eax
  call dopause
  call getcfkey ; returns a virtual scan code - appendix 3
  cmp al, 3ah  ; limit to 39
  jnc key0
  add eax, eax  ; double to account for shifted characters
  add eax, [shifted]  ; +1 if shifted
  mov al, [keys + eax]  ; index into keys
  and al, al
  jz key0  ; repeat if zero
  pop edi
  pop esi
  ret

; programmable keys. Scan code to colorforth 27 key keymapping
; indexes in the array are the virtual scan codes (NOT host ones !!!)
pkeys
  db 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0  ; 0..7     . esc 1 2 3 4 5 6
  db 0 , 0 , 0 , 0 , 0 , 0 , 1 , 0  ; 8..f     7 8 9 0 - = bs tab
  db 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0  ; 10..17   Q W E R T Y U I
  db 0 , 0 , 0 , 0 , 2 , 0 , 0 , 0  ; 18..1f   O P [ ] ret Lctrl A S
  db 0 , 0 , 0 , 0 , 20, 17, 25, 22 ; 20..27   D F G H J K L ; 
  db 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0  ; 28..2f   ' ` Lshift \ Z X C V
  db 0 , 0 , 18, 0 , 0 , 26, 0 , 0  ; 30..37   B N M , . / Rshift *
  db 3 , 2 , 0 , 4 , 5 , 6 , 7 , 8  ; 38..3f   Lalt space caps F1 F2 F3 F4 F5
  db 9 , 10, 11, 12, 13, 0 , 0 , 16 ; 40..47   F6 F7 F8 F9 F10 numlock scrlock home
  db 17, 18, 19, 20, 21, 22, 23, 24 ; 48..4f   up pgup -(KP-) left center(KP5) right +(KP+) end
  db 25, 26, 27, 28, 0 , 0 , 0 , 14 ; 50..57   down pgdn ins del /(kp/) enter(kpenter) ?? F11
  db 15                             ; 58       F12

; key handler for pad. Returns 0..27 for
; the 28 programmable keys.
pkey:
  DUP_
  push esi
  push edi
pkey0:
  xor eax, eax
  call dopause
  call getcfkey ; returns a virtual scan code - appendix 3
  cmp al, 59h ; al - 59h, 58h is the end of one byte raw keyboard scan codes - appendix 3
  jnc pkey0
  mov al, byte [pkeys + eax]
  and al, al
  jz pkey0
  dec al ; values in pkeys are indexes in keys table, which starts with 1. Decrement it to make it start from 0
  pop edi
  pop esi
  ret

; keyboard display is 9 chars wide, 4 high
keyboard_hud_width  equ 9
keyboard_hud_height equ 4

; location of keyboard display (bottom right)
keyboard_hud_x: dd screen_width - keyboard_hud_width * char_width + char_padding 
keyboard_hud_y: dd screen_height - keyboard_hud_height * char_height + char_padding

; pointer to data structure of four offsets to key handling routines
; followed by four bytes: the characters to print as the keyboard
; guide in the lower-right corner of the screen.
; [Refs: keyboard, letter, accept1, decimal, hex, graph, e, pad]
board: dd 0;keyboard_layout_alpha - 4  ; current keyboard (finger keys)
; Shift generally points to one of the following tables:
; alpha0, alpha1, graph0, graph1, numb0, numb1.
shift: dd alpha0;alpha1 ; current shift (thumb) keys
; ColorForth displays numbers in one of two formats â decimal and hexadecimal. 
; Decimal stores 10 here; hex stores 16 here. 
; Therefore routines with "cmp base, 10 : jz base10" fall into handling hexadecimal. 
base: dd 10
; "current" not changed, always points to decimal
current: dd decimal
; current key color?
keyc: dd _yellow
chars:  dd 1
; "after word" - when you finish entering a word, this is called.
aword:  dd exword
; after number
anumber: dd nul
; how many cells on top of the stack hold huffman - encoded characters?
words: dd 1
shifted: dd 0 ; QWERTY

nul0:
  DROP
  jmp short accept2

accept:
  mov dword [shift], alpha0
  ;lea edi, [keyboard_layout_alpha - 4]; TODO NONQWERTY
  xor edi, edi ; QWERTY skip display of command key map

; "Key" returns in AL a number 0..27. If number is 4..27, jump to first -- otherwise
; convert AL into an offset into whichever of the six tables [each with four DD's]
; "shift" is pointing to, and jump to the address in the table.

accept1:
  mov dword [board], edi

accept2:
  call key ; TODOKEY
  ;cmp al, 4
  ;jns near first
  ;mov edx, [shift]
  ;jmp [edx + eax * 4]
  js acmdk
  add dword [shift], 20
  call word_
  call [aword]
  jmp accept
acmdk:
  neg al; QWERTY
  mov edx, [shift]
  jmp dword [edx+eax*4] ; jump to corresponding longword pointer

; Pre-parsing words
;
; Pack takes a single character (first item on stack), converts it into a pre-
; parsed-source bit pattern, and adds the bit pattern to a pre-parsed word (second
; item on stack), if the word is full already, a new word is started.
; for reference, Huffman codes are in groups of 8, the prefixes being:
;  0xxx, 10xxx, 1100xxx, 1101xxx, 1110xxx, 1111xxx
;  "pack" packs one letter (character code at a time, 
;  into a Huffman-coded word at [ESI] (stack item)

bits_:  db 28

; This is part of the "pack" routine.
pack0:
  add eax, byte 120q ; 120q = 0b01010000, make 0b00010000 into 0b01100000, high 2 bits set
  mov cl, 7 ; this is a 7-bit Huffman code
  jmp short pack1 ; continue below

pack:
  cmp al, 20q      ; 0b00010000 ; character code greater than 16?
  jnc pack0        ; if so, it's a 7-bitter, see above
  mov cl, 4        ; otherwise assume it's a 4-bit code
  test al, 10q     ; 10q = 0b00001000, character code more than 7?
  jz pack1     ; if so, it's a 5-bit Huffman code
  inc ecx    ; make the 4 into a 5
  xor al, 30q   ; 30q = 0b00011000, and change prefix to 10xxx

; common entry point for 4, 5, and 7-bit Huffman codes
pack1: 
  mov edx, eax ; copy Huffman code into EDX
  mov ch, cl ; copy Huffman-code bitcount into 8-bit CH register
  .0:
    cmp [bits_], cl ; do we have enough bits left in the word?
    jnc .1 ; if so, continue on
    shr al, 1 ; low bit of character code set?
    jc full ; if so, word is full, need to start an extension word instead
    dec cl ; subtract one from bitcount
    jmp .0 ; keep going; as long as we don't find any set bits, it'll fit
  .1:
    shl dword [esi], cl ; shift over just the amount of bits necessary
    xor [esi], eax ; 'or' or 'add' would have worked as well (and clearer?)
    sub [bits_], cl ; reduce remaining bitcount by what we just used
    ret

; left-justification routine packs Huffman codes into the MSBs of the word, so that the leftmost bit is in bit 31.
lj0:
  mov cl, [bits_] ; bits remaining into CL register
  add cl, 4 ; add to that the 4 reserved bits for type tag
  shl dword [esi], cl ; shift packed word into MSBs
  ret

; this is just the high-level entry point to the above routine
lj:
  call lj0
  DROP
  ret

; the packed word is full, so finish processing
full:
  call lj0 ; left-justify the packed word
  inc dword [words] ; bump the count
  mov byte [bits_], 28 ; 32 - 4 reset bit count, still saving 4 bits for tag
  ; we were processing a character when we found the word full, so add it in
  sub [bits_], ch ; subtract saved bitcount of this Huffman code
  mov eax, edx ; restore top-of-stack with partial packed word
  DUP_
  ret

x:
  call right
  mov eax, [words]
  lea esi, [esi + eax * 4]
  DROP
  jmp accept

word_:
  call right
  mov dword [words], 1
  mov dword [chars], 1
  DUP_
  mov dword [esi], 0
  mov byte [bits_], 28

word1:
  call letter
  jns .0
  neg al; QWERTY
  mov edx, [shift]
  jmp dword [edx + eax * 4]
  .0:
    test al, al
    jz word0
    DUP_
    call echo
    call pack
    inc dword [chars]

word0:
  DROP
  call key
  jmp word1

decimal:
  mov dword [base], 10
  mov dword [shift], numb0
  ;mov dword [board], keyboard_layout_numbers - 4
  ret

hex:
  mov dword [base], 16
  mov dword [shift], numb0
  ;mov dword [board], keyboard_layout_octals - 4
  ret

octal:
  xor dword [current], decimal;(decimal - start1) ^ (hex - start1)
  xor dword [current], hex;(decimal - start1) ^ (hex - start1)
  xor byte [numb0 + 18], 41q ^ 16q ; '9' xor 'f'
  call [current]
  jmp short number0

xn:
  DROP
  DROP
  jmp accept; TODO was acceptn

digit:
  db 14, 10,  0,  0
  db  0,  0, 12,  0,  0,  0, 15,  0
  db 13,  0,  0, 11,  0,  0,  0,  0
  db  0,  1,  2,  3,  4,  5,  6,  7
  db  8,  9

sign: db 0

minus:
  mov [sign], al
  jmp short number2

number0:
  DROP
  jmp short number3

number:
  call [current]
  mov byte [sign], 0
  xor eax, eax ; TOS=entered number(initialized to 0)

number3:
  call key ; TODOKEY
  call letter
  jns .0
  neg al; QWERTY
  mov edx, [shift]
  jmp dword [edx + eax * 4]
  .0:
    test al, al
    jz number0
    mov al, [digit - 4 + eax]
    test byte [sign], 37q
    jz .1
    neg eax
  .1:
    mov edx, [esi]
    imul edx, dword [base]
    add edx, eax
    mov [esi], edx

number2:
  DROP
  mov dword [shift], numb1
  jmp number3

endn:
  DROP
  call [anumber]
  jmp accept; TODO was acceptn

alphn:
  DROP
  DROP
  jmp accept

alph:
  mov dword [shift], alpha1
  jmp word0

;alph0:
;  mov dword [shift], alpha0
;  lea edi, [keyboard_layout_alpha - 4]
;  jmp short star1
;
;star0:
;  mov dword [shift], graph0
;  lea edi, [keyboard_layout_graphics - 4]
;
;star1:
;  DROP
;  jmp accept1
;
;alph:
;  mov dword [shift], alpha1
;  lea edi, [keyboard_layout_alpha - 4]
;  jmp short graphb
;
;graph:
;  mov dword [shift], graph1
;  lea edi, [keyboard_layout_graphics - 4]
;
;graphb: mov dword [board], edi
;  jmp word0

first:
  add dword [shift], byte 4 * 4 + 4   ; move to next shift table
  call word_
  call [aword]
  jmp accept
  
; Printing numbers

; Character codes for the sixteen hexadecimal digits.
hicon:
  db I0, I1, I2, I3, I4, I5, I6, I7
  db I8, I9, Ia, Ib, Ic, Id, Ie, If

; Edig1 ( n -- n ) and edig ( n -- ) emit a hexadecimal digit. The top item on the
; stack is expected to be a number between 0 and 15, which is used as an offset into
; the hicon table.
edig1:
  DUP_

edig:
  push ecx
  mov al, [hicon + eax]
  call emit
  pop ecx
  ret

; Odig ( x -- y n ) converts the next four bits in a 32-bit number into a value
; between 0 and 15 (suitable for being passed to edig). Note that, immediately after
; the call to odig, the caller can test the zero flag to see if the value is zero.
odig:
  rol eax, 4
  DUP_
  and eax, byte 0xf
  ret

; Print 32-bit number onto screen as eight-digit hexadecimal number (pad with
; leading zeroes if necessary).
hdotn:
  mov edx, eax
  neg eax
  lea ecx, [32 + eax * 4]
  DROP
  rol eax, cl
  mov ecx, edx
  jmp short hdot1

hdot:
  mov ecx, 8

hdot1:
  call odig
  call edig
  NEXT hdot1
  DROP
  ret

; Print 32-bit number onto screen as hexadecimal number without leading zeroes.
dot:
  mov ecx, 7
  .0:
    call odig
    jnz .3        ; Branch on first nonzero digit.
    DROP          ; Drop leading 0
    NEXT .0
    inc ecx      ; If number = 0, digit count = 1 (print "0").
  .1:
    call odig    ; Get next hexadecimal digit.
  .2:
    call edig    ; Output digit.
    NEXT .1      ; Continue until there are no more digits.
    call space
    DROP
    ret
  .3:
    inc ecx      ; (Go from handling leading zeroes
    jmp .2        ;    to handling nonzero digits.)

; Print 32-bit number onto screen without leading zeroes as a decimal or a
; hexadecimal number, depending on the current base.
qdot:
  cmp dword [base], byte 10 ; is current base decimal?
  jne dot ; display as hex if not
  ; otherwise fall through to dot10 routine

; (?)Prints a decimal number.
dot10:
  mov edx, eax
  test edx, edx
  jns .0
  neg edx      ; negate number
  DUP_
  mov eax, Iminus     ; Print minus sign.
  call emit
  .0:
    mov ecx, 8
  .1:
    mov eax, edx
    xor edx, edx
    div dword [tens + ecx * 4]  ; NASM "div [tens+ecx*4]
    test eax, eax
    jnz d_1
    dec ecx
    jns .1
    jmp short d_2

d_0:
  mov eax, edx
  xor edx, edx
  div dword [tens + ecx * 4]

d_1: 
  call edig1
  dec ecx
  jns d_0

d_2: 
  mov eax, edx
  call edig1
  call space
  DROP
  ret

; Display routines
;
; unpack
;
; You push a "word" (a 32-bit cell of pre-parsed source code) onto the stack before
; calling unpack. Unpack removes the first character from this word (e.g., "pack"
; becomes "ack") but leaves the word on the stack. Unpack then pushes the removed
; character onto the stack.

unpack:
  DUP_
  test eax, eax      ; Four-bit character begins with 0.
  js .0        ; Branch if character begins with 1.
  shl dword [esi], 4        ; Remove 4-bit character from word.
  rol eax, 4          ; Convert character into character
  and eax, byte 7    ;   code (0..7).
  ret
  .0:
    shl eax, 1          ; Five-bit character begins with 10.
    js .1        ; Branch if character begins with 11.
    shl dword [esi], 5        ; Remove 5-bit character from word.
    rol eax, 4          ; Convert character into character
    and eax, byte 7    ;   code (10o..17o).
    xor al, 8          ; (Flip bit 3 = set bit 3 = add 8.)
    ret
  .1: shl dword [esi], 7        ; Remove 7-bit character from word.
    rol eax, 6          ; Convert character into character
    and eax, byte 77q        ;   code (20o..57o).
    sub al, 20q
    ret

; qring and ring -- I believe the "ring" is character 60o --  -- ColorForth uses
; this instead of a blinking cursor to indicate where the next word will appear in
; the editor
qring:
  DUP_
  inc dword [esi]
  cmp [curs], edi    ; from abort, insert
  jnz .0
  mov [curs], eax
  .0:
    cmp eax, [curs]
    jz ring
    jns .1
    mov [pcad], edi
  .1:
    DROP
    ret

; show "pacman" cursor at current source word (pointed to by EDI)
ring:
  mov [cad], edi ; save as current cursor address
  sub dword [xy], char_width * 0x10000 ; backspace one icon width
  DUP_
  mov eax, _orange ; ochre-colored cursor
  call color
  mov eax, 48 ; cursor character (shift-space, so to speak, in this code)
  mov cx, [xy + 2]
  cmp cx, [rm]
  js .9
  call emit
  sub dword [xy], char_width * 0x10000    ; backspace
  ret
  .9:
    jmp emit

; The refresh routine (actually ref1) relies on thirteen other routines (not
; counting nul, which does nothing) to display pre-parsed words on the screen. These
; routines correspond to the thirteen colors defined in the pre-parsed word specs.
;
; Color number Color Meaning Label Probable label meaning
; 0 N/A Extension TYPE0 Show type-0 word
; 1 Yellow Execute word wW Show word as Word
; 2 Yellow Execute "long" number nW Show number as Word
; 3 Red Define word rW Show red Word
; 4 Green Compile word gW Show green Word
; 5 Green Compile "long" number gnW Show green number as Word
; 6 Green Compile "short" number gsW Show green short number as Word
; 7 Cyan Compile macro word mW Show macro Word
; 8 Yellow Execute "short" number sW Show short number as Word
; 9 White Comment (lowercase) text Show text
; 10 White Comment (Capitalized) Cap Show text Capitalized
; 11 White Comment (ALL CAPS) CAPS Show text in ALL CAPS
; 12 Magenta Variable var Show variable
; 13 nul
; 14 nul
; 15 nul
;
; Displaying words

; Red word
rw:
  mov cx, [xy + 2] ; get current X position
  cmp cx, [lm] ; at left margin?
  jz .0 ; skip if so
  call cr ; insert newline before definition
  .0:
    call red ; set text color to red
    jmp type_ ; type the word

; green word
gw:
  call green
  jmp type_

; deferred macro word
mw:
  call cyan
  jmp type_

; word word: display "white", executable word (actually yellow)
ww:
  DUP_
  mov eax, _yellow
  call color
  jmp short type_

; Displaying word extensions

; display continuation of previous word
type0:
  sub dword [xy], char_width * 0x10000 ; call bspcr
  test dword [ - 4 + edi * 4], -16 ; valid packed word?
  jnz type_ ; display it if so...
  dec edi ; otherwise we've gone past the end of the source, go back one
  mov [lcad], edi ; "last" cursor address is the last source word of screen
  call space
  call qring
  pop edx       ; end of block ; [RST]
  DROP
  jmp keyboard

; Displaying comments

; initial character capitalized comment word (white)
cap:
  call white
  DUP_
  mov eax, [ - 4 + edi * 4]
  and eax, byte -0x10 ; mask out tag bits
  call unpack
  add al, 48
  call emit
  jmp short type2

; all capitals comment word (white)
caps:
  call white
  DUP_
  mov eax, [ - 4 + edi * 4]
  and eax, byte -0x10
  .0:
    call unpack
    jz type3 ; space if it unpacked to nothing
    add al, 48
    call emit
    jmp .0

; Comment word (white)
text:
  call white

; Unpacking and printing characters

type_:
  DUP_
  mov eax, [ - 4 + edi * 4]
  and eax, byte -0x10
type2:
  call unpack
  jz type3
  call emit
  jmp type2
type3:
  call space
  DROP
  DROP
  ret

; Displaying numbers

; Green short word (27 bits)
gsw:
  mov edx, [ - 4 + edi * 4]
  sar edx, 5 ; shift into position
  jmp short gnw1

; Variable word (magenta)
;  falls through to a green number word
var:
  call magenta
  call type_
  ; fall through to next routine to display its value
  ; green (compiled) normal (32 bits) word

; Green number word
gnw:  
  mov edx, [edi * 4]
  inc edi

gnw1:
  DUP_
  mov eax, _green
  cmp dword [bas], dot10 ; is it base 10?
  jz nw2 ; bright green if so
  mov eax, _dkgrn ; else dark green
  jmp short nw2

; Short number executable word (yellow), 27 bits
sw:
  mov edx, [ - 4 + edi * 4]
  sar edx, 5
  jmp short nw1

; Number word (yellow), 32 bits
nw:
  mov edx, [edi * 4]
  inc edi
nw1:
  DUP_
  mov eax, _yellow
  cmp dword [bas], dot10 ; is it base 10?
  jz nw2 ; bright yellow if so
  mov eax, _dkylw ; else dark yellow
nw2:
  call color
  DUP_
  mov eax, edx
  jmp [bas]

; Refresh
refresh:
  call show
  call blank
  call text1
  DUP_                  ; counter
  mov eax, [lcad] ; pointer to end of screen source
  mov [cad], eax        ; for curs beyond end

  ; TODO
  mov    eax, [blk] ; get current block, which is being edited
  sub    eax, 18
  shl    eax, 10-2 ; multiply by 256 longwords per block
  mov    ebx, [blocks_address]
  shr    ebx, 2
  add    eax, ebx
  mov    edi, eax

  xor eax, eax
  mov [pcad], edi  ; for curs=0 (page cursor address)

ref1:
  test dword [edi * 4], 0xf
  jz .0
  call qring
.0:
  mov edx, [edi * 4]
  inc edi
  ; assume decimal number display
  mov dword [bas], dot10
  test dl, 20q ; TODO hexbit
  jz .1 ; not set, so skip ahead
  mov dword [bas], dot ; display as hex
.1:
  and edx, byte 0xf ; get typetag bits as index into display vector
  call [display + edx * 4] ; call the appropriate display routine
  jmp ref1


; The display-routine table

; Tag  Syntax Element      Color
; 
;  15  Commented Number    White
;  14  Display Macro       Blue
;  13  Compiler Feedback   Grey
;  12  Variable            Magenta
; 
;  11  COMMENT (caps)      (White) Obsolete
;  10  Comment             (White) Obsolete
;   9  comment (lower)     White
;   8  Interpreted Number  Yellow
; 
;   7  Compile macro call  Cyan
;   6  Compile number      Green
;   5  Compile big number  Green
;   4  Compile forth word  Green
; 
;   3  Define forth word   Red
;   2  Interp big number   Yellow
;   1  Interp forth word   Yellow
;   0  Word extension      (color of preceding word)
; 
; Offsets to display routines.
align 4
display:
  dd type0, ww, nw, rw
  dd gw, gnw, gsw, mw
  dd sw, text, cap, caps
  dd var, nul, nul, nul

tens:
  dd 10, 100, 1000, 10000, 100000, 1000000
  dd 10000000, 100000000, 1000000000

bas:  dd dot10

; blk is the current block
blk:  dd 18, 18
offset: dd 0
keych: dd 0
endram: dd 0

curs: dd 0
cad:  dd 0 ; cursor (ring) address
pcad: dd 0 ; "previous"/page cursor address
lcad: dd 0 ; "last" / left cursor address of multipart word)
trash:  dd buffer

; Action colors.
; 1-based array of colors used for various action modes
actc: dd _yellow, _black, _red, _dkgrn
  dd _black, _black, _cyan, _black
  dd _white, _white, _white, _blue

vector: dd 0
action: db 1

; These "actxxx" routines and variables wouldn't be connected with act, would they?

act1: 
  mov al, 1    ; 1 = execute yellow word
  jmp short actt

act3: 
  mov al, 3    ; 3 = define red word
  jmp short actt

act4: 
  mov al, 4    ; 4 = compile (green word)
  jmp short actt

act9: 
  mov al, 9    ; comment = white word
  jmp short actt

act10:  
  mov al, 10    ; capitalized comment
  jmp short actt

act11:          
  mov al, 11    ; comment in all caps
  jmp short actt

act7: 
  mov al, 7    ; 7 compile macro cyan word

actt: 
  mov [action], al ; number of action
  mov eax, [actc - 4 + eax * 4] ; load color corresponding to action
  mov dword [aword], insert ; "insert" becomes the active word

; action for number
actn: 
  mov dword [keyc], eax ; store key color
  pop eax      ; RST
  DROP
  jmp accept

; after 'm' pressed, change color and prepare to store variable name
actv: 
  mov byte [action], 12   ; 12 = variable (magenta word)
  mov eax, _mag 
  mov dword [aword], .0
  jmp actn
  .0:
    ; this is the action performed after the variable name is entered
    DUP_ ; save EAX (packed word) on stack
    xor eax, eax ; zero out EAX
    inc dword [words] ; add one to count of words
    jmp insert ; insert packed word into preparsed source

; Cursor and block routines

; minus cursor
mcur: 
  dec dword [curs] ; move left
  jns pcr1 ; just return if it didn't go negative, otherwise undo it...

; plus cursor
pcur: 
  inc dword [curs] ; move right
pcr1: 
  ret

; move up one "row"...
mmcur:  
  sub dword [curs], byte 8 ; ... actually, only 8 words
  jns hcur.9 ; return if it didn't go negative
hcur:
  mov dword [curs], 0 ; otherwise set to 0
  .9:
    ret

; move down one "row"...
ppcur:
  add dword [curs], byte 8 ; ... actually, only 8 words
  ret ; guess it's ok to increment beyond end of screen (?)

; plus one block (+2 since odd are shadows)
pblk: 
  add dword [blk], byte 2
  add dword [esi], byte 2
  ret

; minus block - move editor to previous source (or shadow) block
mblk: 
  cmp dword [blk], byte 20 ; minus one block unless below 20
  js .9 ; (18 is first block available for editing)
  sub dword [blk], byte 2
  sub dword [esi], byte 2
  .9:
    ret

; Jump to previous block edited with 'edit'
; [blk], N <= [blk+4] <= N
popblk: ; ??
  mov ecx, [esi]
  xchg ecx, [blk + 4]
  mov [blk], ecx
  mov [esi], ecx
  ret

; shadow screens in Forth are documentation for corresponding source screens
shadow:
  xor dword [blk], byte 1 ; switch between shadow and source
  xor dword [esi], byte 1 ; change odd to even and vice versa
  ret

; (27 keys in keyboard; 28 offsets in "ekeys" table)
; initial key functions in editor
ekeys:
  dd nul, eout, shadow, act3    ; 0-3 longword
  dd act4, act1, actv, act7     ; 4-7 longword
  dd act9, act10, act11, popblk ; 8-11 longword, F8 should now be 'jump'
  dd nul, nul, nul, hcur        ; 12-15 longword
  dd mmcur, mblk, nul, mcur     ; 16-19 longword
  dd nul, pcur, nul, shadow     ; 20-23 longword

; these are the huffman encodings of the characters to display
ekbd0:
  dd ppcur, pblk, destack, del  ; 24-27 longword, used as dummy execution vectors for ekbd
  ;dd nul, nul, nul, nul; 24-27 longword
  db 0, cap_E,  Is ,  0 ; 28-31, initial control key map in editor (shift)

; note that there are 4 db per line, so it stays word aligned

; characters to display in the key map (keyboard only displays the 12 function keys) (board)
ekbd:
  db Ir, Ig, Iy, Im
  db Ic, It, cap_C, cap_S
  db 0, 0, 0, 0
  ; TODO added by adragomi so it has the same length
;  db 0, 0, 0, 0
;  db 0, 0, 0, 0
;  db 0, 0, 0, 0

;---------old ones
;ekeys:
;  dd nul, del, eout, destack ; n<space><alt>
;  dd act1, act3, act4, shadow ; uiop=yrg*
;  dd mcur, mmcur, ppcur, pcur ; jkl;=ludr
;  dd mblk, actv, act7, pblk ; m,./=-mc+
;  dd nul, act11, act10, act9 ; wer=SCt
;  dd nul, nul, nul, nul ; df=fj (find and jump) not in this version
;
;ekbd0:
;  dd nul, nul, nul, nul
;  db 21, 37,  7,  0  ; x  .  i
;
;ekbd:
;  db 15,  1, 13, 45  ; w  r  g  *
;  db 12, 22, 16,  1  ; l  u  d  r
;  db 35,  9, 10, 43  ; -  m  c  +
;  db  0, 56, 58,  2  ;    S  C  t
;  db  0,  0,  0,  0
;  db  0,  0,  0,  0

e0:
  DROP
  jmp short e_1

edit: 
; Start editor at block given in TOS.
  ;mov ecx, [blk]
  ;mov [blk + 4], ecx
  mov [blk], eax
  DROP

e:  
; Restart editor at current block.
  DUP_
  mov eax, [blk]
  mov dword [anumber], format
  mov byte [alpha0 + 16], Idot
  mov dword [alpha0 + 4], e0
  call refresh
e_1: 
  mov dword [shift], ekbd0
  mov dword [board], ekbd; QWERTY - 4
  mov dword [keyc], _yellow
  .0:
    call pkey ; TODO pkey
    call near [ekeys + eax * 4] ; index into ekeys
    DROP
    jmp .0

eout:
  pop eax    ; [RST]
  DROP
  DROP
  mov dword [aword], exword
  mov dword [anumber], nul
  mov byte [alpha0 + 4 * 4], 0
  mov dword [alpha0 + 4], nul0
  mov dword [keyc], _yellow ; restore key color to yellow
  jmp accept ; revert to command-line processing

; insert, or paste
destack:
  mov edx, [trash] ; grab what was left by last "cut" operation
  cmp edx, buffer ; anything in there?
  jnz .0 ; continue if so...
  ret ; otherwise, 'insert' is already the default action so nothing to do
  .0:
    sub edx, byte 8
    mov ecx, [edx + 4]
    mov [words], ecx
  .1:
    DUP_
    mov eax, [edx]
    sub edx, byte 4
    NEXT .1
    add edx, byte 4
    mov [trash], edx

insert0:
  mov ecx, [lcad]  ; room available? get pointer to last source word
  add ecx, [words] ; add to that the number of 32-bit words to be inserted
  xor ecx, [lcad]
  and ecx, -0x100
  jz insert1
  mov ecx, [words]      ; no
  .0:
    DROP
    NEXT .0
    ret

insert1:
  push esi
  mov esi, [lcad]
  mov ecx, esi
  dec esi
  mov edi, esi
  add edi, [words]
  shl edi, 2
  sub ecx, [cad]
  js .0
  shl esi, 2
  std
  rep movsd
  cld
  .0:
    pop esi
    shr edi, 2
    inc edi
    mov [curs], edi       ; like abort
    mov ecx, [words]
  .1:
    dec edi
    mov [edi * 4], eax
    DROP      ; requires cld
    NEXT .1
    ret

insert:
  call insert0
  mov cl, [action]
  xor [edi * 4], cl
  jmp accept

; read in a word and leave it on the stack
_word:
  mov dword [aword], .0
  jmp accept    ; accept will read a word and then call [aword].
  .0:
    ; restore [aword] to default, and return to caller of _word.
    pop dword [aword]
    mov dword [aword], exword
    ret

format:
  test byte [action], 10 ; ignore 3 and 9
  jz .0
  DROP
  ret
  .0:
    mov edx, eax
    and edx, 0xfc000000 ; check if we have room for tagbits, hexbit, sign bit
    jz .1 ; continue if so
    cmp edx, 0xfc000000
    jne format2
  .1:
    shl eax, 5
    xor al, 2 ; 6
    cmp byte [action], 4
    je .2
    xor al, 13q ; 8
  .2:
    cmp dword [base], byte 10 ; base 10 ?
    je .3 ; continue if so...
    xor al, 20q ; otherwise remove 'hex' bit
  .3:
    mov dword [words], 1
    jmp insert

format2:
  DUP_
  mov eax, 1 ; 5
  cmp byte [action], 4
  jz .0
  mov al, 3 ; 2
  .0:
    cmp dword [base], byte 10
    jz .1
    xor al, 20q
  .1:
    xchg eax, [esi]
    mov dword [words], 2
    jmp insert

; delete, or cut, current word in editor (to the left of pacman cursor)
del:
  call enstack ; copy word(s) to be deleted into "trash" buffer
  mov edi, [pcad] ; get page cursor address into EDI
  mov ecx, [lcad] ; leftmost address of multi-word 'word'
  sub ecx, edi ; subtract to get word count
  shl edi, 2 ; multiply by 4 to get byte offset
  push esi ; save data stack pointer
  mov esi, [cad] ; get current cursor (end of word)
  shl esi, 2 ; byte offset
  rep movsd ; cover what we deleted
  pop esi ; restore data stack pointer
  jmp mcur

; copy source into "trash" buffer
enstack:
  DUP_ ; just to save TOS, we don't actually return anything
  mov eax, [cad] ; get current cursor address
  sub eax, [pcad] ; page cursor address, the first block word
  jz .1 ; one and the same? skip it
  mov ecx, eax ; otherwise, get word count of items to buffer
  xchg eax, edx ; save the count in EDX  safer than a MOV in case EDX is being used for something
  push esi ; save data stack pointer, we need it for "string" operations
  mov esi, [cad] ; source address is current cursor address...
  lea esi, [ - 4 + esi * 4] ; convert to a byte address, point to start of word
  mov edi, [trash] ; destination address is "trash" buffer; get pointer into edit buffer
  .0:
    std ; make string addresses work "backwards" in RAM
    lodsd ; grab a 32-bit word
    cld ; now store "forwards" in "trash" buffer
    stosd ; store a 32-bit word
    NEXT .0 ; loop for word count
    xchg eax, edx ; get the count back...
    stosd ; store it, too, at the end of the trash buffer
    mov [trash], edi ; update the pointer
    pop esi ; restore data stack pointer
  .1:
    DROP ; restore TOS
    ret

; 'pad' is called by a high-level program to define the keypad, followed by
; 28 high-level compiled words that define the key vectors (actions), again
; followed by the Huffman codes for the characters representing the keys

; define keypad actions and representations
pad:
  pop edx ; keypad data must immediately follow call...
  ; there are 28 keys total, 12 on each side plus undefined keys (code 0),
  ; "n", "space" and alt
  ; we're popping the "return" address which is really the address of
  ; the vector table
  mov [vector], edx ; pointer to words for each possible keyhit
  add edx, 28 * 5 + 4 ; + 4 QWERTY, 5 bytes to each "call" instruction
  ; just past that is character data, one byte for the Huffman code of each
  ; character that the physical keyboard represents ("r"=1, "t"=2, etc.)
  ; but remember that "board" contains 4 less than that address, which means
  ; that the first 4 must be those for keycode 0 and the "shift" keys
  mov [board], edx
  ; now, the following makes no sense... subtracting 4*4 from an array of
  ; 5-byte entries should point in the middle of an instruction (which
  ; indeed it does). but the "shift" table is ordinarily an array of 4
  ; longwords followed by the 4 character codes, so there's a method to
  ; CM's madness
  ; the 'keyboard' routine uses this address only for the 4 character codes
  ; at the end of the table
  sub edx, byte 4 * 4 + 4 ; +4 QWERTY, simulate the 4 longwords, can't really use them
  mov [shift], edx ; this is only to point to the character codes
  .0:
    call pkey      ; pkey, QWERTY ; TODOKEY
    ;debug_print "called pad !!!", 0xa, 0x0
    mov edx, [vector] ; load vector table into EDX
    ; the following 3 instructions point to the appropriate vector for the key
    ; it amounts to adding eax*5 to the start of the vector table
    add edx, eax ; add keyvalue once
    lea edx, [5 + eax * 4 + edx] ; add keyvalue 4 more times for total of 5...
    ; plus an extra 5 bytes, which we explain next...
    ; remember that a "call" instruction is e8xxxxxxxx, where the xxxxxxxx is
    ; the offset to the address from the _end_ of the current instruction.
    ; since by adding that extra 5 bytes we _are_ pointing to the end, adding
    ; the 4-byte offset just preceding our address should point us to the
    ; routine to be called. why not just push the address of the "jmp 0b"
    ; below, then "jmp" to the address of the call instead?
    ; guess it wouldn't be any clearer.
    add edx, [edx - 4] ; point to the address of the routine to be called
    DROP ; restore EAX from the data stack
    call edx ; call the routine corresponding to keyhit
    jmp .0 ; loop until "accept" code reached, which exits program

; the kernel gets 12 blocks - fill out to the end
;  times 12 * 1024 - ($ - $$) db 0

; vim:ts=2:sw=2:expandtab

