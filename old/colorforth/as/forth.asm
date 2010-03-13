.equ blockstart, .; .equ blocknumber, -1

;# pad to block boundary
.macro BLOCK number
 .ifb \number
  .print "compiling next block"
  .equ blocknumber, blocknumber + 1
 .else
  .print "compiling block \number"
  .equ blocknumber, \number
 .endif
 .ifdef ABSOLUTE_BLOCKNUMBER
  .org blocknumber * 1024
 .else
  .org blockstart
  .equ blockstart, blockstart + 1024
 .endif
 SET_DEFAULT_TYPETAG
.endm

.macro SET_DEFAULT_TYPETAG
 .if blocknumber % 2  ;# even screens are code, odd are documentation
  .equ default_typetag, 9  ;# type 9 is plain text
 .else
  .equ default_typetag, 4  ;# type 4 is compile
 .endif
.endm

.macro SETTYPE word
 .equ type, 0
 .irp function [EXTENSION], [EXECUTE], [EXECUTELONG], [DEFINE], [COMPILEWORD]
  NEXTTYPE "\word", "\function"
 .endr
 .irp function [COMPILELONG], [COMPILESHORT], [COMPILEMACRO], [EXECUTESHORT]
  NEXTTYPE "\word", "\function"
 .endr
 .irp function [TEXT], [TEXTCAPITALIZED], [TEXTALLCAPS], [VARIABLE]
  NEXTTYPE "\word", "\function"
 .endr
 .irp function [], [], [], [], [], [EXECUTELONGHEX], [], [], [COMPILELONGHEX]
  NEXTTYPE "\word", "\function"
 .endr
 .irp function [COMPILESHORTHEX], [], [EXECUTESHORTHEX], [SKIP], [BINARY]
  NEXTTYPE "\word", "\function"
 .endr
.endm

.macro NEXTTYPE word, function
 .ifdef DEBUG_FORTH
  ;#.print "comparing \"\word\" with \"\function\""
 .endif
 .ifeqs "\word", "\function"
  .equ default_typetag, type
 .else
  .equ type, type + 1
 .endif
.endm

;# compile Forth words with Huffman coding
.macro FORTH words:vararg
 .equ wordcount, 0
 .irp word, \words
 .ifeq wordcount
  .equ typetag, 3  ;# first word is almost always definition
 .else
  .equ typetag, default_typetag
 .endif
 SETTYPE "\word"
 COMPILETYPE "\word"
 .equ wordcount, wordcount + 1
 .endr
.endm

.macro COMPILETYPE word
 .ifeq type - 27  ;# means the SETTYPE macro didn't find a match
  SET_DEFAULT_TYPETAG
  .if typetag == 2 || typetag == 5
   .long typetag, \word
  .elseif typetag == (2 + 16) || typetag == (5 + 16)
   .long typetag, 0x\word
  .elseif typetag == 6 || typetag == 8
   .long typetag | (\word << 5)
  .elseif typetag == (6 + 16) || typetag == (8 + 16)
   .long typetag | (0x\word << 5)
  .elseif typetag == 25  ;# SKIP
   .fill \word, 4, 0
  .elseif typetag == 26  ;# BINARY
   .long 0x\word
  .else
   FORTHWORD "\word"
  .endif
 .endif
.endm

.macro FORTHWORD word
 .equ packed, 0; .equ bits, 28
 ;# only 28 bits are used for a word, the low 4 bits are for the type tag
 .irpc letter, "\word"
  GETCODE "\letter"
  .equ maskbits, (1 << bits) - 1
  .equ testcode, (huffcode >> (shift - bits) & maskbits) << (shift - bits)
  .if bits - shift >= 0
   .equ packed, (packed << shift) | huffcode
   .equ bits, bits - shift
  ;# otherwise will the prefix plus all set bits fit in remaining space?
  .elseif testcode == huffcode
   .equ packed, (packed << bits) | (huffcode >> (shift - bits))
   .equ bits, 0
  .else
   .long (packed << (bits + 4)) | typetag
   .equ packed, huffcode; .equ bits, 28 - shift; .equ typetag, 0
  .endif
 .endr
 .ifne packed
  .long (packed << (bits + 4)) | typetag
 .endif
.endm

.macro GETCODE letter
;# see more documentation on this under 'pack' in color.asm source
 .nolist  ;# don't pollute listing with all these byte comparisons
 .equ huffindex, 0; .equ huffcode, 0
 .irpc huffman, " rtoeanismcylgfwdvpbhxuq0123456789j-k.z/;:!+@*,?"
  .ifeqs "\letter", "\huffman"
   .equ shift, 4 + (huffindex / 8)
   .ifge shift - 6
    .equ shift, 7
   .endif
   .exitm
  .else
   NEXTCODE
  .endif
 .endr
 .list  ;# go ahead and generate listing if enabled
.endm

.macro NEXTCODE
 .equ huffindex, huffindex + 1
 .equ huffcode, huffcode + 1
 .ifeq huffcode - 0b00001000 ;# going from 4-bit to 5-bit code
  .equ huffcode, 0b00010000
 .endif
 .ifeq huffcode - 0b00011000 ;# going from 5-bit to 7-bit code
  .equ huffcode, 0b01100000
 .endif
.endm

/* test cases for common problems
BLOCK 19
FORTH "jul31", "colored", "keypad", "number"
BLOCK
*/
