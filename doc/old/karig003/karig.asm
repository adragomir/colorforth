;==========================================================================
; KARIG.ASM
; Simple 32-bit operating system for Intel PCs
;==========================================================================

; Assemble this file with NASM [http://sourceforge.net/projects/nasm].

%include "macros.asm"
%include "memmap.asm"

%include "boot.asm"
%include "loader.asm"
%include "dump.asm"
%include "screen.asm"

;==========================================================================
