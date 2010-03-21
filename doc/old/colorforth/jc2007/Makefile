sources := $(wildcard *.asm)
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
GAS := as -R -Z -L
DEFINES := $(shell cat *.asm | grep '\.ifn\?def' | \
 awk '{print $$2}' | sort | uniq)
COLINUX := /cygdrive/c/Program\ Files/coLinux
JC2007 := $(shell cygpath -w $(PWD))
BOCHS := /cygdrive/c/Program\ Files/Bochs-2.3
VMWARE := /cygdrive/c/Program\ Files/VMWare/VMware\ Player
BXSHARE := $(shell cygpath -w $(BOCHS))
TIMESTAMP := $(shell date +%Y-%m-%d-%H:%M:%S)
DEBUGGING := 1
CDBOOT := cfboot.iso
WIN98 := win98_img.IMA
LARGEST_CF := /usr/src/howerd/cfdos4/CFDOS4.BLK
LOADBLOCKS := $(shell du $(LARGEST_CF) | awk {'print $$1'})
VIRTUAL_SECTORS := $(shell expr $(LOADBLOCKS) \* 2)
export
default: small bochs23
color.com: $(sources)
ifdef I_HAVE_AT_LEAST_1GB_RAM
	$(MAKE) newcode.dat
endif
	$(MAKE) color.dat
	dd if=color.dat of=$@ bs=1024 count=63
%.dat:	%.asm
	$(GAS) $(foreach definition,$(DEFINES),\
	 $(if $($(definition)),--defsym $(definition)=$($(definition)))) \
	 -ahlns=$*.lst --listing-cont-lines=10 \
	 --reduce-memory-overheads \
	 -o $*.o $<
	objcopy $*.o -O binary $@
newcode: color.asm newcode.dat appcode.dat
	touch $<
	$(MAKE) I_HAVE_AT_LEAST_1GB_RAM=1 color.com
kbd:	color.asm
	touch $<
	$(MAKE) SMALLSCREEN=1 DEBUG_KBD=1 color.com
small:	color.asm
	touch $<
	$(MAKE) SMALLSCREEN=1 newcode
cm2001: color.asm
	touch $<
	$(MAKE) CM2001=1 color.com
jf2005: color.asm
	$(MAKE) DMA=1 AGP=1 E1_STROBE=1 QUESTIONABLE=1 color.com
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
vmclean:
	rm -f *.{log,vm??,lck,nvram}
clean: vmclean
	rm -f *.{o,bin,out,dmp,lst,iso,dat,ima,img}
	rm -rf FDOEMCD
	cd release && $(MAKE) clean
errors:
	$(GAS) color.asm 2>&1 | ./show_error_lines.sh
errorcount:
	$(GAS) color.asm 2>&1 | wc -l
cvsdiff:
	cd .. && cvs diff
commit:
	cd ../ && cvs commit
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
test: ../test/b.img
	cd ../test && $(MAKE)
debug: ../test/b.img
	cd ../test && $(MAKE) BXDEBUG=1
minlog: ../test/b.img
	cd ../test && $(MAKE) $@
bochs23: ../test/b.img
	cd ../test && $(MAKE) oldbxnewcf
vmware: ../test/b.img
	cd ../test && $(MAKE) vmclean vmware
qemu:	../test/b.img
	cd ../test && $(MAKE) vmclean qemu
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
chars.asm: charmap.asm
	cat $< > $@
	./cf2text ../color.com | sed -n '/{block 12}/,/{block 18}/p' | \
	 grep -v '{block [0-9]\+}' >> $@
appcode.asm: forth.asm apps.asm
	cat $+ > $@
newcode.asm:  # it is now in repository, don't make
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
oldcolor.dat: ../color.com empty_block.dat
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
	 -ahlns=$*.lst --listing-cont-lines=10 \
	 --reduce-memory-overheads \
	 -o $*.o $<
	objcopy $*.o -O binary $@
empty_block.dat:
	dd if=/dev/zero of=$@ bs=1024 count=1
%.rblk: %.lst  # reverse masm listing into binary
	sed -n 's/^..........\([0-9A-F]\+\).*/\1/p' "$<" | xxd -r -p > "$@"
boot.blk: color.com
	dd if=$< of=$@ bs=512 count=1
disassembly: ../test/b.img
	objdump --target binary \
	 --architecture i386 \
	 --disassemble-all \
	 --disassembler-options=intel \
	$<
boot.dsm: boot.blk
	objdump --target binary \
	 --architecture i386 \
	 --disassemble-all \
	 --no-show-raw-insn \
	 --disassembler-options=intel \
	 $< > $@
../test/b.img: color.dat
	dd if=/dev/zero of=$@ bs=1024 count=1440
	dd if=$< of=$@ conv=notrunc
pctest: color.com
	scp $< root@192.168.3.4:/tmp/
freedos.iso: ~/shared_documents/FDOEMCD.builder.zip \
	 ~/shared_documents/FDOEMCD.source.zip \
	 color.com
	rm -rf FDOEMCD
	unzip $<
	cp $+ FDOEMCD/CDROOT/
	echo 'COLOR.COM' >> FDOEMCD/CDROOT/AUTORUN.BAT
	cp -f mkisofs.rc FDOEMCD/
	chmod +x FDOEMCD/* FDOEMCD/*/* FDOEMCD/*/*/*
	cd FDOEMCD && cmd /c makeiso.bat
	mv FDOEMCD/FDOEM.ISO $@
isotest: freedos.iso bochsrc cf.vmx
	sed -i 's/^\(ide0:0.fileName = \).*/\1 "$<"/' cf.vmx
	$(VMWARE)/vmplayer.exe cf.vmx
	#IMAGE=$< $(BOCHS)/bochs -q
exclude: color.dat
	find . -maxdepth 1 -type d > $@.lst
	find . -mindepth 2 >> $@.lst
	find . -maxdepth 1 -type f -name '*.iso' >> $@.lst
	find . -maxdepth 1 -type f -name '*.ima' >> $@.lst
	find . -maxdepth 1 -type f -name '*.lst' >> $@.lst
	find . -maxdepth 1 -type f -name '*.dat' >> $@.lst
	find . -maxdepth 1 -type f -name '*.o' >> $@.lst
	find . -maxdepth 1 -type f -name '*.lnk' >> $@.lst
	find . -maxdepth 1 -type f -name '*.pyc' >> $@.lst
	find . -maxdepth 1 -type f -name '*.nvram' >> $@.lst
	find . -maxdepth 1 -type f -name '*vm*' >> $@.lst
cfboot.iso: boot.asm color.asm Makefile
	$(MAKE) EL_TORITO_BOOT=1 newcode vmclean ../test/b.img exclude
	cp -f ../test/b.img boot.img
	mkisofs -b boot.img \
	 -c boot.catalog \
	 -no-emul-boot \
	 -boot-load-seg=0x940 \
	 -boot-load-size=$(VIRTUAL_SECTORS) \
	 --exclude-list=exclude.lst \
	 -o $@ \
	 .
win98.iso: $(WIN98)
	mkdir -p cdboot
	cp $< boot.img
	echo 'test' > cdboot/README
	mkisofs -b boot.img -c boot.catalog -o $@ cdboot
cfisotest: cfboot.iso bochsrc
	IMAGE=$< $(BOCHS)/bochs -q
cdtest: $(CDBOOT)
	if [ -z "$(CDBOOT)" ]; then \
	 echo must define CDBOOT=cfboot.iso >&2; false; fi
	-IMAGE=$< $(BOCHS)/bochs -q
	sed -i 's/^\(ide0:0.fileName =\).*/\1 "$<"/' cf.vmx
	$(VMWARE)/vmplayer.exe cf.vmx
