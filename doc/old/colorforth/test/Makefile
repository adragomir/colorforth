BOCHS := /cygdrive/c/Program\ Files/Bochs-2.3
BOCHS_SRC := /usr/src/bochs
CF_SRC := ~/src
VMWARE := /cygdrive/c/Program\ Files/VMWare/VMware\ Player
BXSHARE := $(shell cygpath -w $(BOCHS))
QEMU := /cygdrive/c/Program Files/QEMU
TEMP ?= /tmp
oldcf := a.img
newcf := b.img
export
default: newbxnewcf
%.debug.uniq.txt: %.debug.txt
	uniq -f 4 -c $< > $@
%cf.uniq.txt: %cf.txt
	uniq -f 1 -c $< > $@
oldbx%: minlog.bxrc a.img b.img
	-export BXRC=$@ && \
	export BOOTFLOPPY=$($*) && \
	export OTHERFLOPPY=$(filter-out $< $($*),$+) && \
	if [ "$(BXDEBUG)" ]; then \
	 $(BOCHS)/bochsdbg -f logging.bxrc -q; \
	else \
	 $(BOCHS)/bochs -f $< -q; \
	fi
newbx%: logging.bxrc a.img b.img
	-export BXRC=$@ && \
	export BOOTFLOPPY=$($*) && \
	export OTHERFLOPPY=$(filter-out $< $($*),$+) && \
	if [ "$(BXDEBUG)" ]; then \
	 $(BOCHS_SRC)/bochsdbg -f $< -q; \
	else \
	 $(BOCHS_SRC)/bochs -f $< -q; \
	fi
	touch $(TEMP)/$@.txt $(TEMP)/$@.debug.txt
	sed -i 's/\r//' $(TEMP)/$@.txt $(TEMP)/$@.debug.txt
	$(MAKE) $(TEMP)/$@.uniq.txt
	$(MAKE) $(TEMP)/$@.debug.uniq.txt
	less $(TEMP)/$@.uniq.txt
%:	b.img %.bxrc
	-export BXRC=$* && \
	 export BOOTFLOPPY=$< && \
	 $(BOCHS_SRC)/bochs -f $*.bxrc -q;
a.img:	$(CF_SRC)/colorforth/color.com
	dd if=/dev/zero of=$@ bs=1024 count=1440
	dd if=$< of=$@ conv=notrunc
b.img:	$(CF_SRC)/colorforth/as/color.com
	dd if=/dev/zero of=$@ bs=1024 count=1440
	dd if=$< of=$@ conv=notrunc
vmware: cf.vmx b.img
	$(VMWARE)/vmplayer.exe cf.vmx
qemu: cfqemu.bat b.img
	-cp $+ "$(QEMU)/" && cd "$(QEMU)" && cmd /c $<
	cp "$(QEMU)/b.img" .
clean: vmclean
	rm -f new*.txt old*.txt *.img *-core.gz
vmclean:
	rm -f cf.{vmss,vmsd} *.log *.vmem *.lck *.nvram *.dbg
set:
	set
