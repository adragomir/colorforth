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
BLOCK 18
FORTH "[TEXT]", "colorforth",  "[TEXTCAPITALIZED]", "jul31",  "[TEXTCAPITALIZED]", "chuck",  "[TEXTCAPITALIZED]", "moore",  "[TEXTCAPITALIZED]", "public",  "[TEXTCAPITALIZED]", "domain",  "[EXECUTESHORT]", "24",  "[EXECUTE]", "load",  "[EXECUTESHORT]", "26",  "[EXECUTE]", "load",  "[EXECUTESHORT]", "28",  "[EXECUTE]", "load",  "[EXECUTESHORT]", "30",  "[EXECUTE]", "load"
FORTH "dump",  "[COMPILESHORT]", "32",  "load",  ";"
FORTH "icons",  "[COMPILESHORT]", "34",  "load",  ";"
.ifdef FREEDOS_ISO
FORTH mandelbrot, [COMPILESHORT], 38, load, ";"
.else
FORTH "print",  "[COMPILESHORT]", "38",  "load",  ";"
FORTH mandelbrot, [COMPILESHORT], 64, load, ";"
.endif
FORTH "file",  "[COMPILESHORT]", "44",  "load",  ";"
FORTH "north",  "[COMPILESHORT]", "46",  "load",  ";"
FORTH "colors",  "[COMPILESHORT]", "56",  "load",  ";",
FORTH  "[EXECUTE]", "mark",  "[EXECUTE]", "empty"
BLOCK 19
BLOCK 20
BLOCK 21
BLOCK 22
BLOCK 23
BLOCK 24
FORTH "[EXECUTE]", "macro"
FORTH "swap",  "[COMPILESHORTHEX]", "168b",  "2,",  "[COMPILELONGHEX]", "c28b0689",  ",",  ";"
FORTH "0",  "[COMPILEMACRO]", "?dup",  "[COMPILESHORTHEX]", "c031",  "2,",  ";"
FORTH "if",  "[COMPILESHORTHEX]", "74",  "2,",  "here",  ";"
FORTH "-if",  "[COMPILESHORTHEX]", "79",  "2,",  "here",  ";"
FORTH "a",  "[COMPILEMACRO]", "?dup",  "[COMPILESHORTHEX]", "c28b",  "2,",  ";"
FORTH "a!",  "?lit",  "if",  "[COMPILESHORTHEX]", "ba",  "1,",  ",",  ";",  "then",  "[COMPILESHORTHEX]", "d08b",  "2,",  "[COMPILEMACRO]", "drop",  ";"
FORTH "2*",  "[COMPILESHORTHEX]", "e0d1",  "2,",  ";"
FORTH "a,",  "2*",  "2*",  ",",  ";"
FORTH "@",  "?lit",  "if",  "[COMPILEMACRO]", "?dup",  "[COMPILESHORTHEX]", "58b",  "2,",  "[COMPILEMACRO]", "a,",  ";",  "then",  "[COMPILESHORTHEX]", "85048b",  "3,",  "0",  ",",  ";"
FORTH "!",  "?lit",  "if",  "?lit",  "if",  "[COMPILESHORTHEX]", "5c7",  "2,",  "swap",  "[COMPILEMACRO]", "a,",  ",",  ";",  "then",  "[COMPILESHORTHEX]", "589",  "2,",  "[COMPILEMACRO]", "a,",  "[COMPILEMACRO]", "drop",  ";",  "then",  "[COMPILEMACRO]", "a!",  "[COMPILESHORTHEX]", "950489",  "3,",  "0",  ",",  "[COMPILEMACRO]", "drop",  ";"
FORTH "nip",  "[COMPILESHORTHEX]", "4768d",  "3,",  ";"
FORTH "+",  "?lit",  "if",  "[COMPILESHORTHEX]", "5",  "1,",  ",",  ";",  "then",  "[COMPILESHORTHEX]", "603",  "2,",  "[COMPILEMACRO]", "nip",  ";"
FORTH "or",  "[COMPILESHORTHEX]", "633"
FORTH "binary",  "?lit",  "if",  "swap",  "[COMPILESHORT]", "2",  "+",  "1,",  ",",  ";",  "then",  "2,",  "[COMPILEMACRO]", "nip",  ";"
FORTH "and",  "[COMPILESHORTHEX]", "623",  "[COMPILEMACRO]", "binary",  ";"
FORTH "u+",  "?lit",  "if",  "[COMPILESHORTHEX]", "681",  "2,",  ",",  ";",  "then",  "[COMPILESHORTHEX]", "44601",  "3,",  "[COMPILEMACRO]", "drop",  ";"
FORTH "?",  "?lit",  "[COMPILESHORTHEX]", "a9",  "1,",  ",",  ";"
BLOCK 25
FORTH "[TEXTCAPITALIZED]", "pentium",  "macros:",  "1,",  "2,",  "3,",  ",",  "compile",  "1-4",  "bytes"
FORTH "drop",  "lodsd,",  "flags",  "unchanged,",  "why",  "sp",  "is",  "in",  "[TEXTALLCAPS]", "esi"
FORTH "over",  "sp",  "4",  "+",  "@"
FORTH "swap",  "sp",  "xchg"
FORTH "0",  "0",  "0",  "xor,",  "macro",  "0",  "identical",  "to",  "number",  "0"
FORTH "a",  "2",  "0",  "mov,",  "never",  "used?"
FORTH "a!",  "0",  "2",  "mov,",  "unoptimized"
FORTH "@",  "[TEXTALLCAPS]", "eax",  "4",  "*,",  "unoptimized"
FORTH "!",  "[TEXTALLCAPS]", "edx",  "4",  "*"
FORTH "nop",  "used",  "to",  "thwart",  "look-back",  "optimization"
FORTH "-",  "ones-complement"
FORTH "2*"
FORTH "2/"
FORTH "if",  "jz,",  "flags",  "set,",  "max",  "127",  "bytes,",  "leave",  "address"
FORTH "-if",  "jns,",  "same"
FORTH "then",  "fix",  "address",  "-",  "in",  "kernel"
FORTH "push",  "[TEXTALLCAPS]", "eax",  "push"
FORTH "pop",  "[TEXTALLCAPS]", "eax",  "pop"
FORTH "u+",  "add",  "to",  "2nd",  "number,",  "literal",  "or",  "value"
FORTH "?",  "test",  "bits,",  "set",  "flags,",  "literal",  "only!"
BLOCK 26
FORTH "[TEXT]", "macros"
FORTH "over",  "[COMPILEMACRO]", "?dup",  "[COMPILESHORTHEX]", "4468b",  "3,",  ";"
FORTH "push",  "[COMPILESHORTHEX]", "50",  "1,",  "[COMPILEMACRO]", "drop",  ";"
FORTH "pop",  "[COMPILEMACRO]", "?dup",  "[COMPILESHORTHEX]", "58",  "1,",  ";"
FORTH "-",  "[COMPILESHORTHEX]", "d0f7",  "2,",  ";"
FORTH "for",  "[COMPILEMACRO]", "push",  "[COMPILEMACRO]", "begin",  ";"
FORTH "*next",  "swap"
FORTH "next",  "[COMPILELONGHEX]", "75240cff"
FORTH "0next",  ",",  "here",  "-",  "+",  "1,",  "[COMPILESHORTHEX]", "4c483",  "3,",  ";"
FORTH "-next",  "[COMPILELONGHEX]", "79240cff",  "[COMPILEMACRO]", "0next",  ";"
FORTH "i",  "[COMPILEMACRO]", "?dup",  "[COMPILESHORTHEX]", "24048b",  "3,",  ";"
FORTH "*end",  "swap"
FORTH "end",  "[COMPILESHORTHEX]", "eb",  "1,",  "here",  "-",  "+",  "1,",  ";"
FORTH "+!",  "?lit",  "if",  "?lit",  "if",  "[COMPILESHORTHEX]", "581",  "2,",  "swap",  "[COMPILEMACRO]", "a,",  ",",  ";",  "then",  "[COMPILESHORTHEX]", "501",  "2,",  "[COMPILEMACRO]", "a,",  "[COMPILEMACRO]", "drop",  ";",  "then",  "[COMPILEMACRO]", "a!",  "[COMPILESHORTHEX]", "950401",  "3,",  "0",  ",",  "[COMPILEMACRO]", "drop",  ";"
FORTH "nop",  "[COMPILESHORTHEX]", "90",  "1,",  ";"
FORTH "align",  "here",  "-",  "[COMPILESHORT]", "3",  "and",  "drop",  "if",  "[COMPILEMACRO]", "nop",  "[COMPILEMACRO]", "align",  ";",  "then",  ";"
FORTH "or!",  "[COMPILEMACRO]", "a!",  "[COMPILESHORTHEX]", "950409",  "3,",  "0",  ",",  "[COMPILEMACRO]", "drop",  ";"
FORTH "*",  "[COMPILESHORTHEX]", "6af0f",  "3,",  "[COMPILEMACRO]", "nip",  ";"
FORTH "*/",  "[COMPILESHORTHEX]", "c88b",  "2,",  "[COMPILEMACRO]", "drop",  "[COMPILELONGHEX]", "f9f72ef7",  ",",  "[COMPILEMACRO]", "nip",  ";"
FORTH "/mod",  "[COMPILEMACRO]", "swap",  "[COMPILESHORTHEX]", "99",  "1,",  "[COMPILELONGHEX]", "16893ef7",  ",",  ";"
FORTH "/",  "[COMPILEMACRO]", "/mod",  "[COMPILEMACRO]", "nip",  ";"
FORTH "mod",  "[COMPILEMACRO]", "/mod",  "[COMPILEMACRO]", "drop",  ";"
BLOCK 27
FORTH "for",  "[COMPILEWORD]", "n",  "push",  "count",  "onto",  "return",  "stack,",  "falls",  "into",  "[COMPILEWORD]", "begin"
FORTH "begin",  "[COMPILEWORD]", "-a",  "current",  "code",  "address",  "-",  "byte"
FORTH "*next",  "[COMPILEWORD]", "aa-aa",  "swap",  "[COMPILEWORD]", "for",  "and",  "[COMPILEWORD]", "if",  "addresses"
FORTH "next",  "[COMPILEWORD]", "a",  "decrement",  "count,",  "jnz",  "to",  "[COMPILEWORD]", "for,",  "pop",  "return",  "stack",  "when",  "done"
FORTH "-next",  "[COMPILEWORD]", "a",  "same,",  "jns",  "-",  "loop",  "includes",  "0"
FORTH "i",  "[COMPILEWORD]", "-n",  "copy",  "loop",  "index",  "to",  "data",  "stack"
FORTH "end",  "[COMPILEWORD]", "a",  "jmp",  "to",  "[COMPILEWORD]", "begin"
FORTH "+!",  "[COMPILEWORD]", "na",  "add",  "to",  "memory,",  "2",  "literals",  "optimized"
FORTH "align",  "next",  "call",  "to",  "end",  "on",  "word",  "boundary"
FORTH "or!",  "[COMPILEWORD]", "na",  "inclusive-or",  "to",  "memory,",  "unoptimized"
FORTH "*",  "[COMPILEWORD]", "mm-p",  "32-bit",  "product"
FORTH "*/",  "[COMPILEWORD]", "mnd-q",  "64-bit",  "product,",  "then",  "quotient"
FORTH "/mod",  "[COMPILEWORD]", "nd-rq",  "remainder",  "and",  "quotient"
FORTH "/",  "[COMPILEWORD]", "nd-q",  "quotient"
FORTH "mod",  "[COMPILEWORD]", "nd-r",  "remainder"
FORTH "time",  "[COMPILEWORD]", "-n",  "[TEXTCAPITALIZED]", "pentium",  "cycle",  "counter,",  "calibrate",  "to",  "get",  "actual",  "clock",  "rate"
BLOCK 28
FORTH "[TEXTCAPITALIZED]", "compiled",  "[TEXT]", "macros"
FORTH "2/",  "[COMPILESHORTHEX]", "f8d1",  "2,",  ";"
FORTH "time",  "[COMPILEMACRO]", "?dup",  "[COMPILESHORTHEX]", "310f",  "2,",  ";",  "[EXECUTE]", "forth"
FORTH "@",  "@",  ";"
FORTH "!",  "!",  ";"
FORTH "+",  "+",  ";"
FORTH "*/",  "*/",  ";"
FORTH "*",  "*",  ";"
FORTH "/",  "/",  ";"
FORTH "2/",  "2/",  ";"
FORTH "dup",  "dup",  ";",  "[TEXTCAPITALIZED]", "arithmetic"
FORTH "negate",  "-",  "[COMPILESHORT]", "1",  "+",  ";"
FORTH "min",  "less",  "if",  "drop",  ";",  "then",  "swap",  "drop",  ";"
FORTH "abs",  "dup",  "negate"
FORTH "max",  "less",  "if",  "swap",  "then",  "drop",  ";"
FORTH "v+",  "[TEXT]", "vv-v",  "push",  "u+",  "pop",  "+",  ";"
FORTH "writes",  "[TEXT]", "acn",  "for",  "write",  "next",  "drop",  "drop",  ";"
FORTH "reads",  "[TEXT]", "acn",  "for",  "read",  "next",  "drop",  "drop",  ";"
FORTH "oadf",  "[TEXT]", "qwerty"
FORTH "save",  "[COMPILESHORT]", "0",  "dup",  "[EXECUTE]", "nc",  "@",  "writes",  "stop",  ";"
BLOCK 29
FORTH "[TEXTCAPITALIZED]", "these",  "macros",  "may",  "be",  "white,",  "others",  "may",  "not"
FORTH "@",  "[COMPILEWORD]", "etc",  "[TEXTCAPITALIZED]", "arithmetic"
FORTH "negate",  "[COMPILEWORD]", "n-n",  "when",  "you",  "just",  "cant",  "use",  "[COMPILEWORD]", "-"
FORTH "min",  "[COMPILEWORD]", "nn-n",  "minimum"
FORTH "abs",  "[COMPILEWORD]", "n-u",  "absolute",  "value"
FORTH "max",  "[COMPILEWORD]", "nn-n",  "maximum"
FORTH "v+",  "[COMPILEWORD]", "vv-v",  "add",  "2-vectors"
FORTH "nc",  "[COMPILEWORD]", "-a",  "number",  "of",  "cylinders",  "booted"
FORTH "save",  "write",  "colorforth",  "to",  "bootable",  "floppy"
FORTH "oadf",  "save",  "as",  "spelled",  "by",  "qwerty.",  "[TEXTCAPITALIZED]", "for",  "typing",  "with",  "blank",  "screen"
BLOCK 30
FORTH "[TEXTCAPITALIZED]", "colors",  "[TEXT]", "etc"
FORTH "block",  "[COMPILESHORTHEX]", "100",  "*", "off", "+", ";"
FORTH "white",  "[COMPILESHORTHEX]", "ffffff",  "color",  ";"
FORTH "red",  "[COMPILESHORTHEX]", "ff0000",  "color",  ";"
FORTH "green",  "[COMPILESHORTHEX]", "ff00",  "color",  ";"
FORTH "blue",  "[COMPILESHORTHEX]", "ff",  "color",  ";"
FORTH "silver",  "[COMPILESHORTHEX]", "bfbfbf",  "color",  ";"
FORTH "black",  "[COMPILESHORT]", "0",  "color",  ";"
FORTH "screen",  "[COMPILESHORT]", "0",  "dup",  "at",  "hp",  "vp",  "box",  ";"
FORTH "5*",  "[COMPILESHORT]", "5",  "for",  "2emit",  "next",  ";"
FORTH "cf",  "[COMPILESHORT]", "25",  "dup",  "at",  "red",  "[COMPILESHORTHEX]", "1",  "[COMPILESHORTHEX]", "3",  "[COMPILESHORTHEX]", "c",  "[COMPILESHORTHEX]", "3",  "[COMPILESHORTHEX]", "a",  "5*",  "green",  "[COMPILESHORTHEX]", "14",  "[COMPILESHORTHEX]", "2",  "[COMPILESHORTHEX]", "1",  "[COMPILESHORTHEX]", "3",  "[COMPILESHORTHEX]", "3e",  "5*",  ";"
FORTH "logo",  "show",  "black",  "screen",
 FORTH "[COMPILEWORD]", "hp", "iw", "[COMPILESHORT]", "10", "*", "negate", "+",
 FORTH "[COMPILEWORD]", "dup", "dup", ;# copies to compute red and green
 FORTH "[COMPILEWORD]", "vp", "ih", "[COMPILESHORT]", "2", "*", "negate", "+",
 FORTH "[COMPILEWORD]", "blue", "box",
 FORTH "[COMPILESHORT]", "-465", "+",
 FORTH "[COMPILESHORT]", 4, "[COMPILESHORT]", 10, "*/", ;# .4 of difference
 FORTH "[COMPILESHORT]", "465", "+", "dup", ;# save copy to compute green
 FORTH "[COMPILESHORT]", "50", "at",
 FORTH "[COMPILEWORD]", "hp",
 FORTH "[COMPILEWORD]", "vp", "ih", "[COMPILESHORT]", "5", "*", "negate", "+",
 FORTH "[COMPILEWORD]", "red",  "box",
 FORTH "[COMPILESHORT]", "200", "[COMPILESHORT]", "100", "at",
;# FORTH "[COMPILEWORD]", "drop", ;# align green with blue? nope
;# FORTH "[COMPILEWORD]", "swap", "drop", ;# align green with red? testing
 FORTH "[COMPILEWORD]", "+", "2/", ;# halfway between blue and red
 FORTH "[COMPILEWORD]", "vp", "ih", "[COMPILESHORT]", "9", "*", "negate", "+",
 FORTH "[COMPILEWORD]", "green", "box", "text", "cf", "keyboard", ";"
FORTH "empty",  "empt",  "logo",  ";"
BLOCK 31
FORTH "block",  "[COMPILEWORD]", "n-a",  "block",  "number",  "to",  "word",  "address"
FORTH "colors",  "specified",  "as",  "rgb:",  "888"
FORTH "screen",  "fills",  "screen",  "with",  "current",  "color"
FORTH "at",  "[COMPILEWORD]", "xy",  "set",  "current",  "screen",  "position"
FORTH "box",  "[COMPILEWORD]", "xy",  "lower-right",  "of",  "colored",  "rectangle"
FORTH "dump",  "compiles",  "memory",  "display"
FORTH "print",  "compiles",  "screen",  "print"
FORTH "icon",  "compiles",  "icon",  "editor"
FORTH "logo",  "displays",  "colorforth",  "logo"
FORTH "show",  "background",  "task",  "executes",  "following",  "code",  "repeatedly"
FORTH "keyboard",  "displays",  "keypad",  "and",  "stack"
BLOCK 32
FORTH "[TEXTCAPITALIZED]", "dump",  "[VARIABLE]", "x", "[BINARY]", "200000",  "[VARIABLE]", "y", "[BINARY]", "201200"
FORTH "one",  "dup",  "@",  "h.",  "space",  "dup",  "h.",  "cr",  ";"
FORTH "lines",  "for",  "one",  "[COMPILESHORT]", "-1",  "+",  "next",  "drop",  ";"
FORTH "dump",  "x",  "!"
FORTH "r",  "show",  "black",  "screen",  "x",  "@",  "[COMPILESHORT]", "15",  "+",  "[COMPILESHORT]", "16",  "text",  "lines",  "keyboard",  ";"
FORTH "it",  "@",  "+",  "@",  "dup",  "h.",  "space",  ";"
FORTH "lines",  "for",  "white",  "i",  "[EXECUTE]", "x",  "it",  "i",  "[EXECUTE]", "y",  "it",  "or",  "drop",  "if",  "red",  "then",  "i",  ".",  "cr",  "-next",  ";"
FORTH "cmp",  "show",  "blue",  "screen",  "text",  "[COMPILESHORT]", "19",  "lines",  "red",  "[EXECUTE]", "x",  "@",  "h.",  "space",  "[EXECUTE]", "y",  "@",  "h.",  "keyboard",  ";"
FORTH "u",  "[COMPILESHORT]", "16"
FORTH "+xy",  "dup",  "[EXECUTE]", "x",  "+!",  "[EXECUTE]", "y",  "+!",  ";"
FORTH "d",  "[COMPILESHORT]", "-16",  "+xy",  ";"
FORTH "ati",  "[COMPILELONGHEX]", "f4100000",  "[TEXT]", "ff7fc000",  "or"
FORTH "byte",  "[COMPILESHORT]", "4",  "/",  "dump",  ";"
FORTH "fix",  "for",  "[COMPILESHORT]", "0",  "over",  "!",  "[COMPILESHORT]", "1",  "+",  "next",  ";",  "[EXECUTE]", "dump"
BLOCK 33
FORTH "[TEXTCAPITALIZED]", "does",  "not",  "say",  "empty,",  "compiles",  "on",  "top",  "of",  "application"
FORTH "x",  "[COMPILEWORD]", "-a",  "current",  "address"
FORTH "one",  "[COMPILEWORD]", "a-a",  "line",  "of",  "display"
FORTH "lines",  "[COMPILEWORD]", "an"
FORTH "dump",  "[COMPILEWORD]", "a",  "background",  "task",  "continually",  "displays",  "memory"
FORTH "u",  "increment",  "address"
FORTH "d",  "decrement"
FORTH "ati",  "address",  "of",  "[TEXTALLCAPS]", "agp",  "graphic",  "registers"
FORTH "byte",  "[COMPILEWORD]", "a",  "byte",  "address",  "dump"
FORTH "fix",  "[COMPILEWORD]", "an-a",  "test",  "word"
BLOCK 34
FORTH "[TEXTCAPITALIZED]", "icons",  "[EXECUTE]", "empty",  "[EXECUTE]", "macro"
FORTH "@w",  "[COMPILESHORTHEX]", "8b66",  "3,",  ";"
FORTH "!w",  "[COMPILEMACRO]", "a!",  "[COMPILESHORTHEX]", "28966",  "3,",  "[COMPILEMACRO]", "drop",  ";"
FORTH "*byte",  "[COMPILESHORTHEX]", "c486",  "2,",  ";",  "[EXECUTE]", "forth",  "[VARIABLE]", "ic", "[BINARY]", "0",  "[VARIABLE]", "cu", "[BINARY]", "15f"
FORTH "sq",  "[EXECUTE]", "xy",  "@",  "[COMPILESHORTHEX]", "10000",  "/mod",  "[COMPILESHORT]", "16",  "+",  "swap",  "[COMPILESHORT]", "16",  "+",  "box",  "[COMPILESHORT]", "17",  "[COMPILESHORT]", "0",  "+at",  ";"
FORTH "loc",  "[EXECUTE]", "ic",  "@",  "[EXECUTESHORT]", "16",  "[EXECUTESHORT]", "24",  "[EXECUTESHORT]", "8",  "[EXECUTE]", "*/",  "*",  "[EXECUTESHORT]", "12",  "[EXECUTE]", "block",  "[EXECUTESHORT]", "4",  "[EXECUTE]", "*",  "+",  ";"
FORTH "0/1",  "[COMPILESHORTHEX]", "8000",  "?",  "if",  "green",  "sq",  ";",  "then",  "blue",  "sq",  ";"
FORTH "row",  "dup",  "@w",  "*byte",  "[COMPILESHORT]", "16",  "for",  "0/1",  "2*",  "next",  "drop",  "[EXECUTESHORT]", "-17",  "[EXECUTESHORT]", "16",  "[EXECUTE]", "*",  "[COMPILESHORT]", "17",  "+at",  ";"
FORTH "ikon",  "loc",  "[COMPILESHORT]", "24",  "for",  "row",  "[COMPILESHORT]", "2",  "+",  "next",  "drop",  ";"
FORTH "adj",  "[COMPILESHORT]", "17",  "*",  "swap",  ";"
FORTH "cursor",  "[EXECUTE]", "cu",  "@",  "[COMPILESHORT]", "16",  "/mod",  "adj",  "adj",  "over",  "over",  "at",  "red",  "[COMPILESHORT]", "52",  "u+",  "[COMPILESHORT]", "52",  "+",  "box",  ";"
FORTH "ok",  "show",  "black",  "screen",  "cursor",  "[COMPILESHORT]", "18",  "dup",  "at",  "ikon",  "text",  "[EXECUTE]", "ic",  "@",  ".",  "keyboard",  ";",  "[EXECUTESHORT]", "36",  "[EXECUTE]", "load",  "[EXECUTE]", "ok",  "[EXECUTE]", "h"
BLOCK 35
FORTH "[TEXTCAPITALIZED]", "draw",  "big-bits",  "icon"
FORTH "@w",  "[COMPILEWORD]", "a-n",  "fetch",  "16-bit",  "word",  "from",  "byte",  "address"
FORTH "!w",  "[COMPILEWORD]", "na",  "store",  "same"
FORTH "*byte",  "[COMPILEWORD]", "n-n",  "swap",  "bytes"
FORTH "ic",  "[COMPILEWORD]", "-a",  "current",  "icon"
FORTH "cu",  "[COMPILEWORD]", "-a",  "cursor"
FORTH "sq",  "draw",  "small",  "square"
FORTH "xy",  "[COMPILEWORD]", "-a",  "current",  "screen",  "position,",  "set",  "by",  "[COMPILEWORD]", "at"
FORTH "loc",  "[COMPILEWORD]", "-a",  "location",  "of",  "current",  "icons",  "bit-map"
FORTH "0/1",  "[COMPILEWORD]", "n-n",  "color",  "square",  "depending",  "on",  "bit",  "15"
FORTH "row",  "[COMPILEWORD]", "a-a",  "draw",  "row",  "of",  "icon"
FORTH "+at",  "[COMPILEWORD]", "nn",  "relative",  "change",  "to",  "screen",  "position"
FORTH "ikon",  "draw",  "big-bits",  "icon"
FORTH "adj",  "[COMPILEWORD]", "nn-nn",  "magnify",  "cursor",  "position"
FORTH "cursor",  "draw",  "red",  "box",  "for",  "cursor"
FORTH "ok",  "background",  "task",  "to",  "continually",  "draw",  "icon,",  "icon",  "number",  "at",  "top",  "[SKIP]", "136", "[EXTENSION]", "[BINARY]", "8080000",  "[EXECUTESHORT]", "4210752",  "[EXECUTESHORT]", "4210752",  "[EXECUTESHORT]", "4210752"
BLOCK 36
FORTH "[TEXTCAPITALIZED]", "edit"
FORTH "+ic",  "[COMPILESHORT]", "1",  "[EXECUTE]", "ic",  "+!",  ";"
FORTH "-ic",  "[EXECUTE]", "ic",  "@",  "[COMPILESHORT]", "-1",  "+",  "[COMPILESHORT]", "0",  "max",  "[EXECUTE]", "ic",  "!",  ";"
FORTH "bit",  "[EXECUTE]", "cu",  "@",  "2/",  "2/",  "2/",  "2/",  "2*",  "loc",  "+",  "[COMPILESHORTHEX]", "10000",  "[EXECUTE]", "cu",  "@",  "[COMPILESHORTHEX]", "f",  "and",  "[COMPILESHORT]", "1",  "+",  "for",  "2/",  "next",  "*byte",  ";"
FORTH "toggle",  "bit",  "over",  "@w",  "or",  "swap",  "!w",  ";"
FORTH "td",  "toggle"
FORTH "d",  "[COMPILESHORT]", "16"
FORTH "wrap",  "[EXECUTE]", "cu",  "@",  "+",  "[EXECUTESHORT]", "16",  "[EXECUTESHORT]", "24",  "[EXECUTE]", "*",  "dup",  "u+",  "/mod",  "drop",  "[EXECUTE]", "cu",  "!",  ";"
FORTH "tu",  "toggle"
FORTH "u",  "[COMPILESHORT]", "-16",  "wrap",  ";"
FORTH "tr",  "toggle"
FORTH "r",  "[COMPILESHORT]", "1",  "wrap",  ";"
FORTH "tl",  "toggle"
FORTH "l",  "[COMPILESHORT]", "-1",  "wrap",  ";"
FORTH "nul",  ";"
FORTH "h",  "pad",  "nul",  "nul",  "accept",  "nul",  "tl",  "tu",  "td",  "tr",  "l",  "u",  "d",  "r",  "-ic",  "nul",  "nul",  "+ic",  "nul",  "nul",  "nul",  "nul",  "nul",  "nul",  "nul",  "nul",  "nul",  "nul",  "nul",  "nul",  "[EXECUTESHORTHEX]", "2500",  "[EXECUTE]", ",",  "[EXECUTESHORTHEX]", "110160c",  "[EXECUTE]", "dup",  "[EXECUTE]", ",",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "2b000023",  "[EXECUTE]", ",",  "[EXECUTESHORT]", "0",  "[EXECUTE]", ",",  "[EXECUTESHORT]", "0",  "[EXECUTE]", ",",  "[EXECUTESHORT]", "0",  "[EXECUTE]", ","
BLOCK 37
FORTH "[TEXTCAPITALIZED]", "edit",  "icon"
.ifdef FREEDOS_ISO
 .include "apps.asm"
.else
BLOCK 38
FORTH "[TEXTALLCAPS]", "png",  "[EXECUTE]", "empty",  "[VARIABLE]", "w", "[BINARY]", "36",  "[VARIABLE]", "h", "[BINARY]", "20",  "[VARIABLE]", "d", "[BINARY]", "4"
FORTH "frame",  "[COMPILESHORTHEX]", "1e80000",  ";",  "[EXECUTE]", "file",  "[EXECUTESHORT]", "42",  "[EXECUTE]", "load",  "[EXECUTESHORT]", "40",  "[EXECUTE]", "load"
FORTH "-crc",  "[TEXT]", "a",  "here",  "over",  "negate",  "+",  "crc",  ".",  ";"
FORTH "crc",  "-crc",  ";"
FORTH "wd",  "[TEXT]", "-a",  "here",  "[COMPILESHORT]", "3",  "and",  "drop",  "if",  "[COMPILESHORT]", "0",  "1,",  "wd",  ";",  "then",  "here",  "[COMPILESHORT]", "2",  "2/s",  ";"
FORTH "bys",  "[TEXT]", "n-a",  ".",  "here",  "swap",  ",",  ";"
FORTH "plte",  "[COMPILELONGHEX]", "45544c50",  "[COMPILESHORT]", "48",  "bys",  "[COMPILESHORTHEX]", "0",  "3,",  "[COMPILESHORTHEX]", "ff0000",  "3,",  "[COMPILESHORTHEX]", "ff00",  "3,",  "[COMPILESHORTHEX]", "ffff00",  "3,",  "[COMPILESHORTHEX]", "ff",  "3,",  "[COMPILESHORTHEX]", "ff00ff",  "3,",  "[COMPILESHORTHEX]", "ffff",  "3,",  "[COMPILESHORTHEX]", "ffffff",  "3,",  "[COMPILESHORTHEX]", "0",  "3,",  "[COMPILESHORTHEX]", "c00000",  "3,",  "[COMPILESHORTHEX]", "c000",  "3,",  "[COMPILESHORTHEX]", "c0c000",  "3,",  "[COMPILESHORTHEX]", "c0",  "3,",  "[COMPILESHORTHEX]", "c000c0",  "3,",  "[COMPILESHORTHEX]", "c0c0",  "3,",  "[COMPILESHORTHEX]", "c0c0c0",  "3,",  "crc",  ";"
FORTH "png",  "[TEXT]", "awh",  "[EXECUTE]", "d",  "@",  "/",  "[EXECUTE]", "h",  "!",  "[EXECUTE]", "d",  "@",  "/",  "[EXECUTE]", "w",  "!",  "wd",  "swap",  "[COMPILELONGHEX]", "474e5089",  ",",  "[COMPILELONGHEX]", "a1a0a0d",  ",",  "[TEXT]", "ihdr",  "[COMPILELONGHEX]", "52444849",  "[COMPILESHORT]", "13",  "bys",  "[EXECUTE]", "w",  "@",  ".",  "[EXECUTE]", "h",  "@",  ".",  "[COMPILESHORTHEX]", "304",  ",",  "[COMPILESHORTHEX]", "0",  "1,",  "crc",  "plte",  "[TEXT]", "idat",  "[COMPILELONGHEX]", "54414449",  "[COMPILESHORT]", "0",  "bys",  "swap",  "deflate",  "crc",  "[TEXT]", "iend",  "[COMPILELONGHEX]", "444e4549",  "[COMPILESHORT]", "0",  "bys",  "crc",  "wd",  "over",  "negate",  "+",  ";"
FORTH "at",  "[COMPILESHORT]", "1024",  "*",  "+",  "2*",  "[EXECUTE]", "frame",  "+",  ";"
FORTH "full",  "[COMPILESHORT]", "4",  "[EXECUTE]", "d",  "!",  "[COMPILESHORT]", "0",  "dup",  "at",  "[COMPILESHORT]", "1024",  "[COMPILESHORT]", "768",  "png",  ";"
FORTH "pad",  "[COMPILESHORT]", "1",  "[EXECUTE]", "d",  "!",  "[EXECUTESHORT]", "46",  "[EXECUTESHORT]", "-9",  "[EXECUTE]", "+",  "[EXECUTESHORT]", "22",  "[EXECUTE]", "*",  "nop",  "[EXECUTESHORT]", "25",  "[EXECUTESHORT]", "-4",  "[EXECUTE]", "+",  "[EXECUTESHORT]", "30",  "[EXECUTE]", "*",  "at",  "[EXECUTESHORT]", "9",  "[EXECUTESHORT]", "22",  "[EXECUTE]", "*",  "nop",  "[EXECUTESHORT]", "4",  "[EXECUTESHORT]", "30",  "[EXECUTE]", "*",  "png",  ";"
BLOCK 39
BLOCK 40
FORTH "[TEXT]", "lz77",  "[EXECUTE]", "macro"
FORTH "@w",  "[COMPILESHORTHEX]", "8b66",  "3,",  ";"
FORTH "*byte",  "[COMPILESHORTHEX]", "c486",  "2,",  ";"
FORTH "!b",  "[COMPILEMACRO]", "a!",  "[COMPILESHORTHEX]", "289",  "2,",  "[COMPILEMACRO]", "drop",  ";",  "[EXECUTE]", "forth"
FORTH "*bys",  "dup",  "[COMPILESHORT]", "16",  "2/s",  "*byte",  "swap",  "[COMPILESHORTHEX]", "ffff",  "and",  "*byte",  "[COMPILESHORTHEX]", "10000",  "*",  "+",  ";"
FORTH ".",  "*bys",  ",",  ";"
FORTH "+or",  "over",  "-",  "and",  "or",  ";"
FORTH "0/1",  "[COMPILESHORTHEX]", "10",  "?",  "if",  "[COMPILESHORTHEX]", "1e",  "and",  "[COMPILESHORTHEX]", "1e",  "or",  "drop",  "if",  "[COMPILESHORT]", "7",  ";",  "then",  "[COMPILESHORTHEX]", "f",  ";",  "then",  "[COMPILESHORT]", "0",  "and",  ";"
FORTH "4b",  "dup",  "0/1",  "[COMPILESHORT]", "9",  "and",  "over",  "[COMPILESHORT]", "6",  "2/s",  "0/1",  "[COMPILESHORTHEX]", "a",  "and",  "+or",  "swap",  "[COMPILESHORT]", "11",  "2/s",  "0/1",  "[COMPILESHORTHEX]", "c",  "and",  "+or",  "[COMPILESHORTHEX]", "8",  "or",  ";"
FORTH "pix",  "dup",  "@w",  "[EXECUTE]", "d",  "@",  "2*",  "u+",  "4b",  ";"
FORTH "row",  "1,",  "dup",  "[EXECUTE]", "w",  "@",  "2/",  "dup",  "[COMPILESHORT]", "1",  "+",  "dup",  "2,",  "-",  "2,",  "[COMPILESHORT]", "0",  "dup",  "1,",  "+adl",  "for",  "pix",  "[COMPILESHORT]", "16",  "*",  "push",  "pix",  "pop",  "or",  "dup",  "1,",  "+adl",  "next",  "drop",  "+mod",  "[EXECUTE]", "d",  "@",  "[EXECUTESHORT]", "1024",  "[EXECUTESHORT]", "2",  "[EXECUTE]", "*",  "*",  "+",  ";"
FORTH "deflate",  "[COMPILESHORTHEX]", "178",  "2,",  "[COMPILESHORT]", "1",  "[COMPILESHORT]", "0",  "adl!",  "[EXECUTE]", "h",  "@",  "[COMPILESHORT]", "-1",  "+",  "for",  "[COMPILESHORT]", "0",  "row",  "next",  "[COMPILESHORT]", "1",  "row",  "drop",  "[EXECUTE]", "ad2",  "@",  "*byte",  "2,",  "[EXECUTE]", "ad1",  "@",  "*byte",  "2,",  "here",  "over",  "[COMPILESHORT]", "4",  "+",  "negate",  "+",  "*bys",  "over",  "[COMPILESHORT]", "-4",  "+",  "!b",  ";"
BLOCK 41
BLOCK 42
FORTH "[TEXTCAPITALIZED]", "crc",  "[EXECUTE]", "macro"
FORTH "2/s",  "?lit",  "[COMPILESHORTHEX]", "e8c1",  "2,",  "1,",  ";"
FORTH "1@",  "[COMPILESHORTHEX]", "8a",  "2,",  ";",  "[EXECUTE]", "forth",  "[VARIABLE]", "ad1", "[BINARY]", "bda2",  "[VARIABLE]", "ad2", "[BINARY]", "bdd8"
FORTH "array",  "[TEXT]", "-a",  "pop",  "[COMPILESHORT]", "2",  "2/s",  ";"
FORTH "bit",  "[TEXT]", "n-n",  "[COMPILESHORT]", "1",  "?",  "if",  "[COMPILESHORT]", "1",  "2/s",  "[COMPILELONGHEX]", "edb88320",  "or",  ";",  "then",  "[COMPILESHORT]", "1",  "2/s",  ";"
FORTH "fill",  "[TEXT]", "nn",  "for",  "dup",  "[COMPILESHORT]", "8",  "for",  "bit",  "next",  ",",  "[COMPILESHORT]", "1",  "+",  "next",  "drop",  ";"
FORTH "table",  "[TEXT]", "-a",  "align",  "array",  "[EXECUTESHORT]", "0",  "[EXECUTESHORT]", "256",  "[EXECUTE]", "fill"
FORTH "crc",  "[TEXT]", "an-n",  "[COMPILESHORT]", "-1",  "swap",  "for",  "over",  "1@",  "over",  "or",  "[COMPILESHORTHEX]", "ff",  "and",  "[EXECUTE]", "table",  "+",  "@",  "swap",  "[COMPILESHORT]", "8",  "2/s",  "or",  "[COMPILESHORT]", "1",  "u+",  "next",  "-",  "nip",  ";"
FORTH "+adl",  "[TEXT]", "n",  "[COMPILESHORTHEX]", "ff",  "and",  "[EXECUTE]", "ad1",  "@",  "+",  "dup",  "[EXECUTE]", "ad2",  "@",  "+"
FORTH "adl!",  "[EXECUTE]", "ad2",  "!",  "[EXECUTE]", "ad1",  "!",  ";"
FORTH "+mod",  "[EXECUTE]", "ad1",  "@",  "[COMPILESHORT]", "65521",  "mod",  "[EXECUTE]", "ad2",  "@",  "[COMPILESHORT]", "65521",  "mod",  "adl!",  ";"
BLOCK 43
BLOCK 44
.endif
FORTH "[TEXTALLCAPS]", "dos",  "[TEXT]", "file"
FORTH "blks",  "[COMPILESHORT]", "256",  "*",  ";"
FORTH "w/c",  "[EXECUTESHORT]", "18",  "[EXECUTE]", "blks",  ";"
FORTH "size",  "[TEXT]", "-a",  "[EXECUTE]", "buffer",  "[COMPILESHORT]", "0",  "[COMPILESHORT]", "1",  "reads",  "[EXECUTE]", "buffer",  "[EXECUTESHORTHEX]", "98f",  "[EXECUTE]", "+",  ";"
FORTH "set",  "[TEXT]", "n",  "!",  "[EXECUTE]", "buffer",  "[COMPILESHORT]", "0",  "[COMPILESHORT]", "1",  "writes",  ";"
FORTH "cyls",  "[TEXT]", "n-nn",  "[COMPILESHORT]", "1",  "swap",  "[EXECUTE]", "w/c",  "[EXECUTESHORT]", "-1",  "[EXECUTE]", "+",  "+",  "[EXECUTE]", "w/c",  "/",  ";"
FORTH "put",  "[TEXT]", "an",  "dup",  "2*",  "2*",  "size",  "set",  "cyls",  "writes",  "stop",  ";"
FORTH "get",  "[TEXT]", "a",  "size",  "@",  "[COMPILESHORT]", "3",  "+",  "2/",  "2/",  "cyls",  "reads",  "stop",  ";"
FORTH ".com",  "[COMPILESHORT]", "0",  "[COMPILESHORT]", "63",  "blocks",  "put",  ";"
BLOCK 45
FORTH "blks",  "[COMPILEWORD]", "n-n",  "size",  "in",  "blocks",  "to",  "words"
FORTH "w/c",  "[COMPILEWORD]", "-n",  "words",  "per",  "cylinder"
FORTH "size",  "[COMPILEWORD]", "-a",  "locate",  "size",  "of",  "2nd",  "file.",  "[TEXTCAPITALIZED]", "floppy",  "has",  "first",  "[TEXTALLCAPS]", "filler",  "then",  "[TEXTALLCAPS]", "file",  "allocated.",  "[TEXTALLCAPS]", "filler",  "is",  "2048",  "bytes,",  "to",  "fill",  "out",  "cylinder",  "0.",  "[TEXTCAPITALIZED]", "names",  "at",  "most",  "8",  "letters,",  "all",  "caps.",  "[TEXTCAPITALIZED]", "directory",  "starts",  "at",  "[EXECUTE]", "buffer",  "[EXECUTESHORTHEX]", "980",  "[EXECUTE]", "+"
FORTH "set",  "[COMPILEWORD]", "n",  "size.",  "[TEXTALLCAPS]", "file",  "must",  "be",  "larger",  "than",  "your",  "file."
FORTH "cyls",  "[COMPILEWORD]", "n-nn",  "starting",  "cylinder",  "1",  "and",  "number",  "of",  "cylinders"
FORTH "put",  "[COMPILEWORD]", "an",  "write",  "file",  "from",  "address"
FORTH "get",  "[COMPILEWORD]", "a",  "read",  "file",  "to",  "address"
BLOCK 46
FORTH "[TEXTCAPITALIZED]", "north",  "[TEXTCAPITALIZED]", "bridge",  "[EXECUTE]", "empty",  "[EXECUTE]", "macro"
FORTH "4@",  "[COMPILEMACRO]", "dup",  "[COMPILESHORTHEX]", "ed",  "1,",  ";"
FORTH "4!",  "[COMPILESHORTHEX]", "ef",  "1,",  "[COMPILEMACRO]", "drop",  ";",  "[EXECUTE]", "forth",  "[VARIABLE]", "dev", "[BINARY]", "3b00"
FORTH "nb",  "[COMPILESHORTHEX]", "0",  "[EXECUTE]", "dev",  "!",  ";"
FORTH "sb",  "[COMPILESHORTHEX]", "3800",  "[EXECUTE]", "dev",  "!",  ";"
FORTH "agp",  "[COMPILESHORTHEX]", "800",  "[EXECUTE]", "dev",  "!",  ";"
FORTH "ess",  "[COMPILESHORTHEX]", "6800",  "[EXECUTE]", "dev",  "!",  ";"
FORTH "ric",  "[COMPILESHORTHEX]", "7800",  "[EXECUTE]", "dev",  "!",  ";"
FORTH "win",  "[COMPILESHORTHEX]", "8000",  "[EXECUTE]", "dev",  "!",  ";"
FORTH "ati",  "[COMPILESHORTHEX]", "10000",  "[EXECUTE]", "dev",  "!",  ";"
FORTH "add",  "[COMPILESHORTHEX]", "cf8",  "a!",  "4!",  "[COMPILESHORTHEX]", "cfc",  "a!",  ";"
FORTH "q",  "[COMPILELONGHEX]", "80000000",  "+",  "add",  "4@",  ";"
FORTH "en",  "[COMPILESHORTHEX]", "8004",  "q",  "[COMPILESHORT]", "-4",  "and",  "or",  "4!",  ";"
FORTH "dv",  "dup",  "[COMPILESHORTHEX]", "800",  "*",  "q",  "swap",  "[COMPILESHORT]", "1",  "+",  ";"
FORTH "regs",  "[EXECUTE]", "dev",  "@",  "[EXECUTESHORT]", "19",  "[EXECUTESHORT]", "4",  "[EXECUTE]", "*",  "+",  "[COMPILESHORT]", "20",  "for",  "dup",  "q",  "h.",  "space",  "dup",  "h.",  "cr",  "[COMPILESHORT]", "-4",  "+",  "next",  "drop",  ";"
FORTH "devs",  "[COMPILESHORT]", "0",  "[COMPILESHORT]", "33",  "for",  "dup",  "q",  "dup",  "[COMPILESHORT]", "1",  "+",  "drop",  "if",  "dup",  "h.",  "space",  "drop",  "dup",  "[COMPILESHORT]", "8",  "+",  "q",  "dup",  "h.",  "space",  "over",  "h.",  "cr",  "then",  "drop",  "[COMPILESHORTHEX]", "800",  "+",  "next",  "drop",  ";"
FORTH "ok",  "show",  "black",  "screen",  "text",  "regs",  "keyboard",  ";"
FORTH "u",  "[COMPILESHORTHEX]", "40",  "[EXECUTE]", "dev",  "+!",  ";"
FORTH "d",  "[COMPILESHORT]", "-64",  "[EXECUTE]", "dev",  "+!",  ";"
FORTH "test",  "[COMPILESHORTHEX]", "ff00",  "+",  "a!",  "4@",  ";",  "[EXECUTE]", "ok"
BLOCK 47
BLOCK 48
FORTH "[TEXTALLCAPS]", "ascii",  "[EXECUTE]", "macro"
FORTH "1@",  "[COMPILESHORTHEX]", "8a",  "2,",  ";",  "[EXECUTE]", "forth"
FORTH "string",  "pop",  ";"
FORTH "cf-ii",  "string",  "[EXECUTELONGHEX]", "6f747200",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "696e6165",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "79636d73",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "7766676c",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "62707664",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "71757868",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "336a7a6b",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "37363534",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "2d313938",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "2f322e30",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "2b213a3b",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "3f2c2a40",  "[EXECUTE]", ","
FORTH "ch",  "[COMPILESHORTHEX]", "fffffff0",  "and",  "unpack",  "[EXECUTE]", "cf-ii",  "+",  "1@",  "[COMPILESHORTHEX]", "ff",  "and",  ";"
FORTH "ii-cf",  "string",  "[EXECUTESHORTHEX]", "2a00",  "[EXECUTE]", ",",  "[EXECUTESHORT]", "0",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "2b2d0000",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "2725232e",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "1b262224",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "1f1e1d1c",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "28292120",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "2f000000",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "3a43355c",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "3d3e3440",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "484a3744",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "3336393c",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "38314742",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "3f414632",  "[EXECUTE]", ",",  "[EXECUTESHORTHEX]", "493b45",  "[EXECUTE]", ",",  "[EXECUTESHORT]", "0",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "a13052c",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "d0e0410",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "181a0714",  "[EXECUTE]", ",",  "[EXECUTESHORTHEX]", "306090c",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "8011712",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "f111602",  "[EXECUTE]", ",",  "[EXECUTESHORTHEX]", "190b15",  "[EXECUTE]", ","
FORTH "chc",  "[COMPILESHORTHEX]", "ffffffe0",  "+",  "[EXECUTE]", "ii-cf",  "+",  "1@",  "[COMPILESHORTHEX]", "ff",  "and",  ";"
BLOCK 49
BLOCK 50
FORTH "[TEXTCAPITALIZED]", "clock",  "[EXECUTE]", "macro"
FORTH "p@",  "[COMPILESHORTHEX]", "ec",  "1,",  ";"
FORTH "p!",  "[COMPILESHORTHEX]", "ee",  "1,",  "[COMPILEMACRO]", "drop",  ";",  "[EXECUTE]", "forth"
FORTH "ca",  "[COMPILESHORTHEX]", "70",  "a!",  "p!",  "[COMPILESHORTHEX]", "71",  "a!",  ";"
FORTH "c@",  "ca",  "[COMPILESHORT]", "0",  "p@",  ";"
FORTH "c!",  "ca",  "p!",  ";"
FORTH "hi",  "[COMPILESHORT]", "10",  "c@",  "[COMPILESHORTHEX]", "80",  "and",  "drop",  "if",  ";",  "then",  "hi",  ";"
FORTH "lo",  "[COMPILESHORT]", "0",  "p@",  "[COMPILESHORTHEX]", "80",  "and",  "drop",  "if",  "lo",  ";",  "then",  ";"
FORTH "bcd",  "c@",  "[COMPILESHORT]", "16",  "/mod",  "[COMPILESHORT]", "10",  "*",  "+",  ";"
FORTH "hms0",  "[COMPILESHORT]", "4",  "bcd",  "[COMPILESHORT]", "100",  "*",  "[COMPILESHORT]", "2",  "bcd",  "+",  "[COMPILESHORT]", "100",  "*",  "[COMPILESHORT]", "0",  "bcd",  "+",  ";"
FORTH "hms",  "hms0",  "[COMPILESHORT]", "2",  "ms",  "dup",  "hms0",  "or",  "drop",  "if",  "drop",  "hms",  ";",  "then",  ";"
FORTH "ymd",  "[COMPILESHORT]", "9",  "bcd",  "[COMPILESHORT]", "100",  "*",  "[COMPILESHORT]", "8",  "bcd",  "+",  "[COMPILESHORT]", "100",  "*",  "[COMPILESHORT]", "7",  "bcd",  "+",  ";"
FORTH "day",  "[COMPILESHORT]", "6",  "c@",  "[COMPILESHORT]", "-1",  "+",  ";"
FORTH "cal",  "hi",  "lo",  "time",  "-",  "hi",  "lo",  "time",  "+",  "[TEXT]", "748",  ";"
BLOCK 51
BLOCK 52
FORTH "[TEXTALLCAPS]", "lan",  "[EXECUTE]", "empty",  "[EXECUTESHORTHEX]", "3f8",  "[EXECUTESHORT]", "54",  "[EXECUTE]", "load",  "[EXECUTE]", "init"
FORTH "no",  "block",  "[COMPILESHORT]", "4",  "*",  "[COMPILESHORT]", "1024",  ";"
FORTH "send",  "no",  "for",  "dup",  "1@",  "xmit",  "[COMPILESHORT]", "1",  "+",  "next",  "drop",  ";"
FORTH "receive",  "no",  "for",  "rcv",  "over",  "1!",  "[COMPILESHORT]", "1",  "+",  "next",  "drop",  ";"
FORTH "no",  "[COMPILESHORT]", "18",  "[EXECUTESHORT]", "7",  "[EXECUTESHORT]", "18",  "[EXECUTE]", "*",  ";"
FORTH "backup",  "no",  "for",  "dup",  "send",  "[COMPILESHORT]", "1",  "+",  "next",  "drop",  ";"
FORTH "accept",  "no",  "for",  "dup",  "receive",  "[COMPILESHORT]", "1",  "+",  "next",  "drop",  ";"
BLOCK 53
FORTH "[SKIP]", "252", "[EXTENSION]", "[BINARY]", "8080000",  "[EXECUTESHORT]", "4210752",  "[EXECUTESHORT]", "4210752",  "[EXECUTESHORT]", "4210752"
BLOCK 54
FORTH "[TEXTCAPITALIZED]", "serial",  "[TEXT]", "3f8",  "[TEXT]", "2e8",  "[TEXT]", "1050",  "[EXECUTE]", "macro"
FORTH "p@",  "[COMPILEMACRO]", "a!",  "[COMPILEMACRO]", "dup",  "[COMPILESHORTHEX]", "ec",  "1,",  ";"
FORTH "p!",  "[COMPILEMACRO]", "a!",  "[COMPILESHORTHEX]", "ee",  "1,",  "[COMPILEMACRO]", "drop",  ";"
FORTH "1@",  "[COMPILESHORTHEX]", "8a",  "2,",  ";"
FORTH "1!",  "[COMPILEMACRO]", "a!",  "[COMPILESHORTHEX]", "288",  "2,",  "[COMPILEMACRO]", "drop",  ";",  "[EXECUTE]", "forth"
FORTH "r",  "[EXECUTESHORT]", "0",  "[EXECUTE]", "+",  "+",  ";"
FORTH "9600",  "[COMPILESHORT]", "12",  ";"
FORTH "115200",  "[COMPILESHORT]", "1",  ";"
FORTH "b/s",  "[COMPILESHORTHEX]", "83",  "[EXECUTESHORT]", "3",  "[EXECUTE]", "r",  "p!",  "9600",  "[EXECUTESHORT]", "0",  "[EXECUTE]", "r",  "p!",  "[COMPILESHORT]", "0",  "[EXECUTESHORT]", "1",  "[EXECUTE]", "r",  "p!",  "[COMPILESHORT]", "3",  "[EXECUTESHORT]", "3",  "[EXECUTE]", "r",  "p!",  ";"
FORTH "init",  "b/s",  "[TEXT]", "16550",  "[COMPILESHORT]", "1",  "[EXECUTESHORT]", "2",  "[EXECUTE]", "r",  "p!",  "[COMPILESHORT]", "0",  "[EXECUTESHORT]", "4",  "[EXECUTE]", "r",  "p!",  ";"
FORTH "xmit",  "[TEXT]", "n",  "[EXECUTESHORT]", "5",  "[EXECUTE]", "r",  "p@",  "[COMPILESHORTHEX]", "20",  "and",  "drop",  "if",  "[EXECUTESHORT]", "0",  "[EXECUTE]", "r",  "p!",  ";",  "then",  "pause",  "xmit",  ";"
FORTH "cts",  "[EXECUTESHORT]", "6",  "[EXECUTE]", "r",  "p@",  "[COMPILESHORTHEX]", "30",  "and",  "[COMPILESHORTHEX]", "30",  "or",  "drop",  "if",  "cts",  ";",  "then",  "xmit",  ";"
FORTH "st",  "[EXECUTESHORT]", "6",  "[EXECUTE]", "r",  "p@"
FORTH "xbits",  "[COMPILESHORTHEX]", "30",  "and",  "[COMPILESHORTHEX]", "10",  "/",  "dup",  "[COMPILESHORT]", "1",  "and",  "2*",  "2*",  "+",  "2/",  ";"
FORTH "st!",  "[EXECUTESHORT]", "4",  "[EXECUTE]", "r",  "p!",  ";"
FORTH "?rcv",  "[EXECUTESHORT]", "5",  "[EXECUTE]", "r",  "p@",  "[COMPILESHORT]", "1",  "and",  "drop",  "if",  "[EXECUTESHORT]", "0",  "[EXECUTE]", "r",  "p@",  "then",  ";"
FORTH "rcv",  "?rcv",  "if",  ";",  "then",  "pause",  "rcv",  ";"
BLOCK 55
FORTH "p@",  "[COMPILEWORD]", "p-n",  "fetch",  "byte",  "from",  "port"
FORTH "p!",  "[COMPILEWORD]", "np",  "store",  "byte",  "to",  "port"
FORTH "1@",  "[COMPILEWORD]", "a-n",  "fetch",  "byte",  "from",  "byte",  "address"
FORTH "1!",  "[COMPILEWORD]", "na",  "store",  "byte",  "to",  "byte",  "address"
FORTH "r",  "[COMPILEWORD]", "n-p",  "convert",  "relative",  "to",  "absolute",  "port",  "address.",  "[TEXTCAPITALIZED]", "base",  "port",  "on",  "stack",  "at",  "compile",  "time.",  "[TEXTCAPITALIZED]", "compiled",  "as",  "literal",  "at",  "yellow-green",  "transition"
FORTH "9600"
FORTH "115200",  "baud-rate",  "divisors.",  "[TEXTCAPITALIZED]", "these",  "are",  "names,",  "not",  "numbers"
FORTH "b/s",  "set",  "baud",  "rate.",  "[TEXTCAPITALIZED]", "edit",  "to",  "change"
FORTH "init",  "initialize",  "uart"
FORTH "xmit",  "[COMPILEWORD]", "n",  "wait",  "for",  "ready",  "and",  "transmit",  "byte"
FORTH "cts",  "[COMPILEWORD]", "n",  "wait",  "for",  "clear-to-send",  "then",  "xmit"
FORTH "st",  "[COMPILEWORD]", "-n",  "fetch",  "status",  "byte"
FORTH "xbits",  "[COMPILEWORD]", "n-n",  "exchange",  "status",  "bits"
FORTH "st!",  "[COMPILEWORD]", "n",  "store",  "control",  "byte"
FORTH "?rcv",  "fetch",  "byte",  "if",  "ready.",  "[TEXTCAPITALIZED]", "set",  "flag",  "to",  "be",  "tested",  "by",  "[COMPILEWORD]", "if"
FORTH "rcv",  "[COMPILEWORD]", "-n",  "wait",  "for",  "ready",  "and",  "fetch",  "byte"
BLOCK 56
FORTH "[TEXTCAPITALIZED]", "hexagon",  "[EXECUTE]", "empty",  "[VARIABLE]", "col", "[BINARY]", "0",  "[VARIABLE]", "del", "[BINARY]", "202020"
FORTH "lin",  "dup",  "2/",  "2/",  "dup",  "2*",  "line",  ";"
FORTH "hex",  "[EXECUTE]", "xy",  "@",  "[COMPILESHORT]", "7",  "and",  "over",  "2/",  "for",  "lin",  "[COMPILESHORT]", "7",  "+",  "next",  "over",  "for",  "lin",  "next",  "swap",  "2/",  "for",  "[COMPILESHORT]", "-7",  "+",  "lin",  "next",  "drop",  ";"
FORTH "+del",  "[EXECUTE]", "del",  "@",  "nop"
FORTH "petal",  "and",  "[EXECUTE]", "col",  "@",  "+",  "[COMPILESHORTHEX]", "f8f8f8",  "and",  "color",  "[COMPILESHORT]", "100",  "hex",  ";"
FORTH "-del",  "[EXECUTE]", "del",  "@",  "[COMPILESHORTHEX]", "f8f8f8",  "or",  "[COMPILESHORTHEX]", "80808",  "+",  ";"
FORTH "rose",  "[COMPILESHORT]", "0",  "+del",  "[COMPILESHORT]", "-176",  "[COMPILESHORT]", "-200",  "+at",  "[COMPILESHORTHEX]", "f80000",  "-del",  "petal",  "[COMPILESHORT]", "352",  "[COMPILESHORT]", "-200",  "+at",  "[COMPILESHORTHEX]", "f80000",  "+del",  "[COMPILESHORT]", "-264",  "[COMPILESHORT]", "-349",  "+at",  "[COMPILESHORTHEX]", "f800",  "-del",  "petal",  "[COMPILESHORT]", "176",  "[COMPILESHORT]", "-200",  "+at",  "[COMPILESHORTHEX]", "f8",  "+del",  "[COMPILESHORT]", "-176",  "[COMPILESHORT]", "98",  "+at",  "[COMPILESHORTHEX]", "f8",  "-del",  "petal",  "[COMPILESHORT]", "176",  "[COMPILESHORT]", "-200",  "+at",  "[COMPILESHORTHEX]", "f800",  "+del",  ";"
FORTH "ok",  "show",  "black",  "screen",  "[COMPILESHORT]", "512",  "[COMPILESHORT]", "282",  "at",  "rose",  "text",  "[EXECUTE]", "col",  "@",  "h.",  "space",  "[EXECUTE]", "del",  "@",  "[COMPILESHORTHEX]", "ff",  "and",  ".",  "keyboard",  ";",  "[EXECUTESHORT]", "58",  "[EXECUTE]", "load",  "[EXECUTE]", "ok",  "[EXECUTE]", "h"
BLOCK 57
FORTH "[TEXTCAPITALIZED]", "draws",  "7",  "hexagons.",  "[TEXTCAPITALIZED]", "colors",  "differ",  "along",  "red,",  "green",  "and",  "blue",  "axes."
FORTH "col",  "color",  "of",  "center",  "hexagon"
FORTH "del",  "color",  "difference"
FORTH "lin",  "[COMPILEWORD]", "n",  "draws",  "1",  "horizontal",  "line",  "of",  "a",  "hexagon"
FORTH "hex",  "[COMPILEWORD]", "n",  "draws",  "top,",  "center",  "and",  "bottom.",  "[TEXTCAPITALIZED]", "slope",  "7",  "x",  "to",  "4",  "y",  "is",  "1.750",  "compared",  "to",  "1.732"
FORTH "+del",  "[COMPILEWORD]", "n",  "increment",  "color"
FORTH "-del",  "[COMPILEWORD]", "n"
FORTH "petal",  "[COMPILEWORD]", "n",  "draw",  "colored",  "hexagon"
FORTH "rose",  "draw",  "7",  "hexagons"
FORTH "ok",  "describe",  "screen.",  "[TEXTCAPITALIZED]", "center",  "color",  "at",  "top"
BLOCK 58
FORTH "[TEXTCAPITALIZED]", "pan"
FORTH "in",  "[EXECUTE]", "del",  "@",  "2*",  "[COMPILESHORTHEX]", "404040",  "min",  "[EXECUTE]", "del",  "!",  ";"
FORTH "out",  "[EXECUTE]", "del",  "@",  "2/",  "[COMPILESHORTHEX]", "80808",  "max",  "[EXECUTE]", "del",  "!",  ";"
FORTH "r",  "[COMPILESHORTHEX]", "f80000"
FORTH "+del",  "[EXECUTE]", "del",  "@"
FORTH "+col",  "and",  "[EXECUTE]", "col",  "@",  "+",  "[COMPILESHORTHEX]", "f8f8f8",  "and",  "[EXECUTE]", "col",  "!",  ";"
FORTH "g",  "[COMPILESHORTHEX]", "f800",  "+del",  ";"
FORTH "b",  "[COMPILESHORTHEX]", "f8",  "+del",  ";"
FORTH "-r",  "[COMPILESHORTHEX]", "f80000",  "-del",  "+col",  ";"
FORTH "-g",  "[COMPILESHORTHEX]", "f800",  "-del",  "+col",  ";"
FORTH "-b",  "[COMPILESHORTHEX]", "f8",  "-del",  "+col",  ";"
FORTH "nul",  ";"
FORTH "h",  "pad",  "nul",  "nul",  "accept",  "nul",  "-r",  "-g",  "-b",  "nul",  "r",  "g",  "b",  "nul",  "out",  "nul",  "nul",  "in",  "nul",  "nul",  "nul",  "nul",  "nul",  "nul",  "nul",  "nul",  "nul",  "nul",  "nul",  "nul",  "[EXECUTESHORTHEX]", "250000",  "[EXECUTE]", ",",  "[EXECUTESHORTHEX]", "130d01",  "[EXECUTE]", "dup",  "[EXECUTE]", ",",  "[EXECUTE]", ",",  "[EXECUTELONGHEX]", "2b000023",  "[EXECUTE]", ",",  "[EXECUTESHORT]", "0",  "[EXECUTE]", ",",  "[EXECUTESHORT]", "0",  "[EXECUTE]", ",",  "[EXECUTESHORT]", "0",  "[EXECUTE]", ","
BLOCK 59
FORTH "in",  "increment",  "color",  "difference"
FORTH "out",  "decrement",  "it"
FORTH "r"
FORTH "g"
FORTH "b",  "increment",  "center",  "color"
FORTH "-r"
FORTH "-g"
FORTH "-b",  "decrement",  "it"
FORTH "+del",  "redefine",  "with",  ";"
FORTH "+col",  "change",  "center",  "color"
FORTH "nul",  "ignore"
FORTH "h",  "describe",  "keypad"
BLOCK 60
BLOCK 61
BLOCK 62
FORTH "[TEXTCAPITALIZED]", "timing",  "[EXECUTE]", "empty",  "[EXECUTE]", "macro"
FORTH "out",  "[COMPILESHORTHEX]", "e1e6",  "2,",  ";",  "[EXECUTE]", "forth"
FORTH "tare",  "time",  "-",  "[COMPILESHORT]", "1000",  "for",  "next",  "time",  "+",  ";"
FORTH "tare+",  "time",  "-",  "push",  "[COMPILESHORT]", "1000",  "for",  "dup",  "next",  "c",  "pop",  "time",  "+",  ";"
FORTH "test",  "tare",  "time",  "+",  "-",  "[COMPILESHORT]", "1000",  "for",  "out",  "next",  "time",  "+",  ";",  "[TEXT]", "next",  "[TEXT]", "3",  "[TEXT]", "loop",  "[TEXT]", "5.7",  "[TEXT]", "/next",  "[TEXT]", "2",  "[TEXT]", "/swap",  "[TEXT]", "25",  "[TEXT]", "swap",  "[TEXT]", "7.2",  "[EXECUTE]", "macro"
FORTH "c!",  "[COMPILESHORTHEX]", "c88b",  "2,",  "[COMPILEMACRO]", "drop",  "here",  ";"
FORTH "loop",  "[COMPILESHORTHEX]", "49",  "1,",  "[COMPILESHORTHEX]", "75",  "1,",  "[TEXT]", "e2",  "here",  "-",  "+",  "1,",  ";",  "[EXECUTE]", "forth"
FORTH "try",  "time",  "-",  "[COMPILESHORT]", "1000",  "c!",  "loop",  "time",  "+",  ";"
BLOCK 63
BLOCK
