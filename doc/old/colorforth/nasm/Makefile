DMAVERSION := /usr/src/howerd/Josh/chuck05-jg2
original := $(wildcard ../masm/*.asm)
sources := $(notdir $(original))
# sed recipes (commands) for converting MASM code to NASM assembler
label_if_any := \(\s*\w\+:\)\?
opcode := \(\w\+\)
nasm_reserved := -e 's/\b\(bits\|at\|pause\)\b/_\1/g'
equ := -e 's/^\s*\(\w\+\)\s\+equ\s\+\(.\+\)/.equ \1, \2/gi'
includes := -e 's/\b\(include\)\(\s\+\)\(\S\+\)\(.*\)/%\1\2"\3"\4/i'
extended_addressing := -e 's/\[\(\w\+\)\]\[\(\w\+\)\]/[\1+\2]/'
lowercase := -e 's/^\(.*\)$$/\L\1/'
indexed := -e 's/\b\(\w\+\)\[\([^]]*\)\]/[\1+\2]/' # NASM-style
directives := -e 's/\b\(macro\|org\|end\|align\)\b/\L%\1/gi'
orgminus := -e 's/\(\s*org\s*\$$\s*-\s*2\)/;\#\1/'
ptr := -e 's/\(\s\(byte\|word\|dword\)\s\+ptr\)//'
offset := -e '/^$(label_if_any)\s*\.\w\+/ s/\boffset\b\s//gi'
# need to use \x27 to represent tick "'" in regexp
ascii := -e 's/\.byte\s\+\x27\([^\x27]\+\)\x27/.ascii "\1"/'
dup := -e 's/\(\w\+\)\s\+\([0-9]\+\)\s\+dup\s*(\(\w\+\))/times \2 \1 \3/'
unused := -e 's/^\(\s*\(.model\|.486p\|only\|assume\|end\)\b.*\)/;\#\1/i'
labels := -e 's/^\(\s*\w\+\)\(\s\+\.\w\+\s\+.*\)/\1:\2/'
macro0 := -e 's/^\(\s*\)\(\w\+\)\(\s\+\)\(macro\)\s*$$/\L\1%\4\3\2 0/gi'
xor := -e 's/^$(label_if_any)\s*$(opcode)\s\+\(.*\)\(\bxor\b\)/\1 \2 \3 ^/g'
endm := -e 's/^\s*endm\b/%endmacro/i'
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
octal := -e 's/\b\([0-7]\+\)o/\1q/g'
hex := -e 's/\b\([0-9a-fA-f]\+\)h\b/0x\1/g'
intel := -e '1 s/^/.intel_syntax; /'
use32 := -e '/^protected:/ iuse32'  # this rule will mess up the error line no.
use16 := -e '/^start0:/ iuse16' # this rule will mess up the error line no.
offset_test := -n -e '/$(label_if_any).*offset/p'  # must be used alone
regex_test := sed $(foreach regex, $(REGEX), $($(regex)))
# order of the recipes can be important!
recipes := 
recipes += lowercase shift unused nasm_reserved indexed
recipes += octal macro0 endm includes ptr use16 use32 orgminus
recipes += dup xor extended_addressing
sed := sed $(foreach regex, $(recipes), $($(regex)))
NASM := nasm -d offset
export
color.com: $(sources)
	$(NASM) color.asm -o color.com
$(sources): $(original) Makefile
	for file in $(filter-out Makefile, $+); do \
	 $(sed) $$file | \
	 ./nasm_locals.py $$(basename $$file) | \
	 ./nasm_macro1.py > $$(basename $$file) \
	;done
color.dsm: ../color.com
	objdump --target binary \
	 --architecture i386 \
	 --disassemble-all \
	 --disassembler-options=intel \
	 $< > $@
set:
	set
diff:	sources.txt original.txt
	-diff -i -b -B -W180 -y $+
sources.txt: $(sources)
	cat $+ > $@
original.txt: $(original)
	cat $+ > $@
clean:
	rm -f $(sources) sources.txt original.txt
download: $(original) # in case we accidentally overwrote them (shit happens)
	for file in $+; do \
	fetch=$$(echo $$(basename $$file) | tr 'a-z' 'A-Z') && \
	 wget -O $$file http://ftp.ultratechnology.com/$$fetch \
	 ;done
	sed -i 's/\r//' $+ # get rid of embedded CRs from DOS
firstpass: color.asm instruction_fixup.sh
	as $< 2>&1 | ./instruction_fixup.sh
	touch $@
secondpass: color.asm firstpass
	as $< 2>&1 | ./instruction_fixup.sh 2
	touch $@
regex:
	@echo Use as follows: 'make REGEX="directives offset" regex'
	@echo Full list of recipes: $(recipes)
	@echo Remember that some depend on others to run first!
	[ "$(REGEX)" ] && $(regex_test) $(original)
errors:
	$(NASM) color.asm 2>&1 | ./show_error_lines.sh
errorcount:
	$(NASM) color.asm 2>&1 | wc -l
rcsdiff:
	rcsdiff RCS/*
ci:
	-ci -u RCS/*
	-rcs -U RCS/* &>/dev/null
	-ci -u RCS/* &>/dev/null
%.dmadiff: %
	diff -i -b -B -y -W160 $(DMAVERSION)/$< $<
dma:	$(DMAVERSION)
	cd $< && $(NASM) -o color.com color.asm
	touch $@
dmatest: dma
	cp $(DMAVERSION)/color.com ../as
	cd ../test && $(MAKE) oldbxnewcf
