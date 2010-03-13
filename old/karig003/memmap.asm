;==========================================================================
; MEMMAP.ASM
; Locations in memory
;==========================================================================

;boot       equ     0x0800
loader      equ     0x0A00
;bootsector equ     0x7C00

;tbuf       equ    0x996A0 ; Text buffer (80 columns x 30 rows)
stacks      equ    0x9A000 ; Stacks grow downward in memory
maind       equ    0x9C000 ; Main data stack:      2048 cells
mains       equ    0x9D000 ; Main return stack:    1024 return addresses
godd        equ    0x9F000 ; Display data stack:   2048 cells
gods        equ    0xA0000 ; Display return stack: 1024 return addresses

; Memory from 0xA0000..0xFFFFF is off-limits

codespace   equ   0x100000
videobuffer equ   0x200000
videoRAM    equ 0xE0000000

;==========================================================================
