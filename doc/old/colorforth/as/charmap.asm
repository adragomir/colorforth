;# compile character map from data stored as "######.......###"
;# the above should compile to the two bytes 0xfc 0x07
.macro CHR16X24 row
 .equ packed, 0
 .irpc pixel, "\row"
  .equ packed, packed << 1
  .ifeqs "\pixel", "#"
   .equ packed, packed | 1
  .endif
 .endr
 .byte packed >> 8, packed & 0xff
.endm

;# compile character map from data stored as "######........#######..........#"
;# the above should compile to the bytes 0xfc 0x03 0xf8 0x01
.macro CHR32X32 row
 .equ packed, 0
 .equ bitcount, 0
 .irpc pixel, "\row"
  .equ packed, packed << 1
  .ifeqs "\pixel", "#"
   .equ packed, packed | 1
  .endif
  .equ bitcount, bitcount + 1
  .ifeq bitcount - 8
   .byte packed
   .equ packed, 0
   .equ bitcount, 0
  .endif
 .endr
.endm

.org 0x3000  ;# block 12 through block 17
