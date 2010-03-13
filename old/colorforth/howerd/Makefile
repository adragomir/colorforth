TIMESTAMP := $(shell date +%Y%m%d%H%M)
GAS := as -R -Z -L
VMWARE := /cygdrive/c/Program\ Files/VMWare/VMware\ Player
LARGEST_CF := /usr/src/howerd/cfdos4/CFDOS4.BLK
LOADBLOCKS := $(shell du $(LARGEST_CF) | awk {'print $$1'})
VIRTUAL_SECTORS := $(shell expr $(LOADBLOCKS) \* 2)
export
patch_sources := $(wildcard block???.asm)
patches = $(patch_sources:.asm=.dat)
test: ../test/b.img
	cd ../test && $(MAKE) minlog
set:
	set
../test/b.img: cfdos4.blk
	dd if=/dev/zero of=$@ bs=1024 count=1440
	dd if=$< of=$@ conv=notrunc
cfdos4.blk: /usr/src/howerd/cfdos4/CFDOS4.BLK ../jc2007 $(patches)
	cd ../jc2007 && \
	 $(MAKE) EL_TORITO_BOOT=$$EL_TORITO_BOOT newcode # need bigscreen
	dd if=/dev/zero of=$@ bs=1024 count=1440
	dd if=../jc2007/color.com of=$@ conv=notrunc bs=1024 count=12
	dd if=$< of=$@ conv=notrunc skip=12 seek=12 bs=1024
	for patch in $(patches); do \
	 declare block=$${patch:5:3} && \
	 dd if=$$patch of=$@ conv=notrunc seek=$$block bs=1024; \
	done
cfdos4.ima.gz: ../test/b.img
	gzip -c $< > $@
upload: cfdos4.ima.gz cfdos4.iso.gz
	for file in $+; do \
	 ncftpput -C upload.sourceforge.net $$file \
	  incoming/$(TIMESTAMP)-$$file; \
	done
qemu:	../test/b.img
	cd ../test && $(MAKE) vmclean qemu
vmware: ../test/b.img
	cd ../test && $(MAKE) vmclean vmware
clean:
	rm -f *.ima.gz *.blk *.o *.dat *.pyc *.lst newcode.asm
newcode.asm: ../jc2007/forth.asm
	cat $< > $@
	./cf2text cfdos4.blk | sed -n '/{block 18}/,$$p' | \
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
%.dat:	%.asm
	$(GAS) $(foreach definition,$(DEFINES),\
	 $(if $($(definition)),--defsym $(definition)=$($(definition)))) \
	 -ahlns=$*.lst --listing-cont-lines=10 \
	 --reduce-memory-overheads \
	 -o $*.o ../jc2007/forth.asm $<
	objcopy $*.o -O binary $@
exclude: cfdos4.blk
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
cfdos4.iso: exclude
	rm -f cfdos4.blk
	$(MAKE) EL_TORITO_BOOT=1 cfdos4.blk
	mv -f cfdos4.blk boot.img
	mkisofs -b boot.img \
	 -c boot.catalog \
	 -no-emul-boot \
	 -boot-load-seg=0x940 \
	 -boot-load-size=$(VIRTUAL_SECTORS) \
	 --exclude-list=exclude.lst \
	 -o $@ \
	 .
cdtest: $(CDBOOT) vmclean
	if [ -z "$(CDBOOT)" ]; then \
	 echo must define CDBOOT=cfdos4.iso >&2; false; fi
	sed -i 's/^\(ide0:0.fileName =\).*/\1 "$<"/' cf.vmx
	$(MAKE) runcd
runcd:	vmclean
	$(VMWARE)/vmplayer.exe cf-cdrom.vmx
runfloppy: vmclean
	$(VMWARE)/vmplayer.exe cf-floppy.vmx
vmclean:
	rm -f *.{vmss,vmsd} *.log *.vmem *.lck *.nvram *.dbg
%.gz:	%
	gzip -c $< > $@
