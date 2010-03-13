original := $(wildcard ../masm/*.asm)
sources := $(notdir $(original))
ftpfiles := $(foreach file, $(sources), \
 $(shell echo $(notdir $(file)) | tr 'a-z' 'A-Z'))
# sed recipes (commands) for converting MASM code to GNU 'as' assembler
label_if_any := \(\s*\w\+:\)\?
equ := -e 's/^\s*\(\w\+\)\s\+equ\s\+\(.\+\)/.equ \1, \2/gi'
includes := -e 's/\b\(include\)\(\s\+\)\(\S\+\)\(.*\)/.\1\2"\3"\4/i'
directives := -e 's/\b\(macro\|endm\|org\|end\|align\)\b/.\1/gi'
lowercase := -e 's/^\(.*\)$$/\L\1/'
orgminus := -e 's/\(\s*\.org\s*\.\s*-\s*2\)/;\#\1/'
org0x1fe := -e 's/\(\s*\.org\s*0x1fe\)/;\#\1/'
offset := -e '/^$(label_if_any)\s*\.\w\+/ s/\boffset\b\s//gi'
# need to use \x27 to represent tick "'" in regexp
ascii := -e 's/\.byte\s\+\x27\([^\x27]\+\)\x27/.ascii "\1"/'
dup := -e 's/\(\.\w\+\)\s\+\(\w\+\)\s\+dup\s*(\(\w\+\))/.rept \2 \1 \3; .endr/'
unused := -e 's/^\(\s*\(.model\|.486p\|only\|assume\)\b.*\)/;\#\1/i'
labels := -e 's/^\(\s*\w\+\)\(\s\+\.\w\+\s\+.*\)/\1:\2/'
macro := -e 's/^\(\s*\)\(\w\+\)\(\s\+\)\(macro\)\b/\1\4\3\2/gi'
reg8s := al\|bl\|cl\|dl\|ah\|bh\|ch\|dh
reg16s := ax\|bx\|cx\|dx\|si\|di\|bp\|sp\|cs\|ds\|es\|ss\|ip
reg32s := eax\|ebx\|ecx\|edx\|esi\|edi\|ebp\|esp\|eip
registers := -e 's/\b\($(reg8s)\|$(reg16s)\|$(reg32s)\)/%\1/gi'
shift := -e '/\bdd\b/ s/\bshl\b/<</g'
comment := -e 's/;/;\#/'
here := -e 's/\$$/./'
byte := -e 's/\bdb\b/.byte/i'
word := -e 's/\bdw\b/.word/i'
dword := -e 's/\bdd\b/.long/i'
local := -e 's/^\(\s*\)@@:/\10:/'
reference := -e 's/\B@\([FfBb]\)\b/0\1/'
octal := -e 's/\b\([0-7]\+\)o/0\1/g'
hex := -e 's/\b\([0-9a-fA-f]\{2,\}\)h\b/0x\1/g'
intel := -e '1 s/^/.intel_syntax /'
use32 := -e '/\bcr0\b/I i.code32'  # will mess up the error line no.
use16 := -e '/^start0:/ i.code16gcc' # will mess up the error line no.
code16 := -e '/\.code16/,/\.code32/ s/\be\(\w\{2\}\)\b/\1/gi'
offset_test := -n -e '/$(label_if_any).*offset/p'  # must be used alone
regex_test := sed $(foreach regex, $(REGEX), $($(regex)))
# order of the recipes can be important!
recipes := intel lowercase
recipes += shift comment local reference
recipes += octal hex macro includes directives unused
recipes += byte word dword here equ labels dup ascii
recipes += offset orgminus use16 use32
sed := sed $(foreach regex, $(recipes), $($(regex)))
GAS := as -R -Z
DEFINES := $(shell cat *.asm | grep '\.ifn\?def' | \
 awk '{print $$2}' | sort | uniq)
export
help:
	@echo To build the most universally usable color.com: make new
	@echo To build the closest thing to CM\'s 2001 color.com: make cm2001
	@echo Read Makefile for other options.
%.dat:	%.asm
	$(GAS) $(foreach definition,$(DEFINES),\
	 $(if $($(definition)),--defsym $(definition)=$($(definition)))) \
	 -ahlmns=$*.lst --listing-cont-lines=10 \
	 --reduce-memory-overheads \
	 -o $*.o $<
	objcopy $*.o -O binary $@
color.com: $(sources) chars.asm color.dat
ifdef I_HAVE_AT_LEAST_1GB_RAM
	$(MAKE) newcode.dat
endif
ifdef MANDELBROT
	$(MAKE) new_mandelbrot.blk
endif
	$(GAS) $(foreach definition,$(DEFINES),\
	 $(if $($(definition)),--defsym $(definition)=$($(definition)))) \
	 -ahlns=color.lst --listing-cont-lines=10 -o color.o color.asm
	objcopy color.o -O binary $@
new:	color.asm
	touch $<
	$(MAKE) color.com
newcode: color.asm newcode.dat
	touch $<
	$(MAKE) I_HAVE_AT_LEAST_1GB_RAM=1 color.com
kbd:	color.asm
	touch $<
	$(MAKE) SMALLSCREEN=1 DEBUG_KBD=1 color.com
small:	color.asm
	touch $<
	$(MAKE) SMALLSCREEN=1 color.com
cm2001: color.asm
	touch $<
	$(MAKE) CM2001=1 color.com
jf2005: color.asm
	$(MAKE) DMA=1 JF2005=1 AGP=1 E1_STROBE=1 QUESTIONABLE=1 color.com
$(sources): $(original)
	for file in $+; do \
	 $(sed) $$file | sed $(code16) | \
	 ./as_macro1.py > $$(basename $$file) \
	;done
/tmp/color.dsm: ../color.com
	objdump --target binary \
	 --architecture i386 \
	 --disassemble-all \
	 --no-show-raw-insn \
	 --disassembler-options=intel \
	 $< > $@
/tmp/gas_color.dsm: color.com
	objdump --target binary \
	 --architecture i386 \
	 --disassemble-all \
	 --no-show-raw-insn \
	 --disassembler-options=intel \
	 $< > $@
asmwdiff: /tmp/color.dsm /tmp/gas_color.dsm
	diff -y -W160 $+
asmdiff: /tmp/color.dsm /tmp/gas_color.dsm
	diff $+
set:
	set
%.txt:	%.com cf2text
	cf2text $< > $@
diff:	sources.txt original.txt
	-diff -W180 -y $+
sources.txt: $(sources)
	cat $+ > $@
original.txt: $(original)
	cat $+ > $@
clean:
	rm -f sources.txt original.txt *.{o,bin,out,dmp,lst}
download: $(original) # in case we accidentally overwrote them (shit happens)
	for file in $+; do \
	fetch=$$(echo $$(basename $$file) | tr 'a-z' 'A-Z') && \
	 wget -O $$file http://ftp.ultratechnology.com/$$fetch \
	 ;done
	sed -i 's/\r//' $+ # get rid of embedded CRs from DOS
firstpass: color.asm instruction_fixup.sh
	$(GAS) $< 2>&1 | ./instruction_fixup.sh
	touch $@
secondpass: color.asm firstpass
	$(GAS) $< 2>&1 | ./instruction_fixup.sh 2
	touch $@
regex:
	@echo Use as follows: 'make REGEX="directives offset" regex'
	@echo Full list of recipes: $(recipes)
	@echo Remember that some depend on others to run first!
	[ "$(REGEX)" ] && $(regex_test) $(original)
errors:
	$(GAS) color.asm 2>&1 | ./show_error_lines.sh
errorcount:
	$(GAS) color.asm 2>&1 | wc -l
rcsdiff:
	cvs diff
ci:
	cvs commit
/tmp/color.xxd: ../color.com
	xxd $< > $@
/tmp/gas_color.xxd: color.com
	xxd $< > $@
/tmp/gas_color.old.xxd: color.com.old
	xxd $< > $@
bindiff: /tmp/color.xxd /tmp/gas_color.xxd
	diff -y -W180 $+
macdiff: /tmp/gas_color.xxd /tmp/gas_color.old.xxd # using new macro
	diff -y -W180 $+
test:
	cd ../test && $(MAKE)
debug:
	cd ../test && $(MAKE) BXDEBUG=1
htmldiff: /tmp/color_pydump.html /tmp/color.html
	rm -f $+
	$(MAKE) $+
	diff -y -W180 $+
textdiff: ../color.txt color.txt
	diff -y -W180 $+
/tmp/color_pydump.html: ../color.com
	TIM_NEITZ=1 ./cf2html $< > $@
/tmp/color.html: ../color.com /usr/src/howerd/cf2html
	/usr/src/howerd/cf2html $< > $@
/usr/src/howerd/cf2html: /usr/src/howerd/cf2html.c
	gcc -o $@ $<
chars.asm: charmap.asm Makefile
	cat $< > $@
	./cf2text ../color.com | sed -n '/{block 12}/,/{block 18}/p' | \
	 grep -v '{block [0-9]\+}' >> $@
newcode.asm: forth.asm Makefile
	cat $< > $@
	./cf2text ../color.com | sed -n '/{block 18}/,$$p' | \
	 sed \
	 -e 's/\(.*\)/\L\1/' \
	 -e 's/\(\[\w*\]\)/\U\1/g' \
	 -e 's/{\(block [0-9]\+\)}/\U\1/' \
	 -e '/^[^B]/ s/\(\S\+\)/"\1",/g' \
	 -e 's/^\([^B]\)/FORTH \1/' \
	 -e 's/,\s*$$//' \
	 >> $@
	echo BLOCK >> $@
	echo BLOCK >> $@  # pad out one more block for even number
color.dat: ../color.com empty_block.dat
	dd if=$< of=$@ bs=1024 skip=18
	if [ "$$(expr $$(du $@ | awk '{print $$1}') % 2)" = "0" ]; then \
	 cat $(word 2, $+) >> $@; \
	fi
%.ans:	%.com
	cf2ansi $< > $@
mandelbrot.blk: /usr/src/howerd/MarkSlicker/accept.blk
	dd if=$< of=$@ bs=1024 skip=64 count=4
%.dsm:	%.blk forth.asm
	cat $(word 2, $+) > $@
	./cf2text -h $< | sed \
	 -e 's/\(.*\)/\L\1/' \
	 -e 's/\(\[\w*\]\)/\U\1/g' \
	 -e 's/{\(block [0-9]\+\)}/\U\1/' \
	 -e '/^[^B]/ s/\(\S\+\)/"\1",/g' \
	 -e 's/^\([^B]\)/FORTH \1/' \
	 -e 's/,\s*$$//' \
	 >> $@
	echo BLOCK >> $@
new_%.blk: new_%.dsm
	$(GAS) $(foreach definition,$(DEFINES),\
	 $(if $($(definition)),--defsym $(definition)=$($(definition)))) \
	 -ahlmns=$*.lst --listing-cont-lines=10 \
	 --reduce-memory-overheads \
	 -o $*.o $<
	objcopy $*.o -O binary $@
empty_block.dat: Makefile
	dd if=/dev/zero of=$@ bs=1024 count=1
%.rblk: %.lst  # reverse masm listing into binary
	sed -n 's/^..........\([0-9A-F]\+\).*/\1/p' "$<" | xxd -r -p > "$@"
boot.blk: color.com
	dd if=$< of=$@ bs=512 count=1
boot.dsm: boot.blk
	objdump --target binary \
	 --architecture i386 \
	 --disassemble-all \
	 --no-show-raw-insn \
	 --disassembler-options=intel \
	 $< > $@
