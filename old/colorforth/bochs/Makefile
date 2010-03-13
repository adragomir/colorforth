SOURCES := $(wildcard *.cc *.h)
BOCHS := /usr/src/bochs
CF_BOCHS := $(PWD)
BOCHS_LOGIN :=  cvs \
 -d:pserver:anonymous@bochs.cvs.sourceforge.net:/cvsroot/bochs login
BOCHS_CHECKOUT :=  cvs -z3 \
 -d:pserver:anonymous@bochs.cvs.sourceforge.net:/cvsroot/bochs co -P bochs
export
all:	$(BOCHS)/bochs.exe
# make bochsdbg.exe first because it builds as bochs.exe!
force:
	$(MAKE) distclean
	$(MAKE) debug_config
	$(MAKE) $(BOCHS)/bochsdbg.exe
	$(MAKE) config
	$(MAKE) $(BOCHS)/bochs.exe
set:
	set
%.wdiff:
	-diff -y -W160 $* $(shell find $(BOCHS) -name "$*.orig")
%.diff:
	-diff $* $(shell find $(BOCHS) -name "$*.orig")
checkin:
	cd .. && cvs commit
cvsdiff:
	cd .. && cvs diff
config:
	cd $(BOCHS) && ./configure \
	 --enable-vbe \
	 --enable-cirrus \
	 --enable-usb \
	 --enable-sb16 \
	 --enable-pci \
	 --enable-ne2000 \
	 --enable-fast-function-calls \
	 --enable-icache \
	 --enable-repeat-speedups \
	 --enable-show-ips \
	 --enable-guest2host-tlb \
	 --enable-sep \
	 --enable-pge \
	 --enable-pae \
	 --enable-sse \
	 --enable-apic \
	 --enable-cpu-level=6 \
	 --enable-readline \
	 $(DEBUG_CONFIG)
debug_config:
	$(MAKE) DEBUG_CONFIG="--enable-debugger --enable-disasm" config
$(BOCHS)/bochs.exe: $(SOURCES)
	cd $(BOCHS) && $(MAKE)
$(BOCHS)/bochsdbg.exe: $(SOURCES)
	cd $(BOCHS) && $(MAKE) && mv bochs.exe bochsdbg.exe
distclean:
	rm -f $(BOCHS)/bochs{,dbg}.exe
test:
	cd ../test && $(MAKE)
debug:
	cd ../test && $(MAKE) BXDEBUG=1
%.link:
	ln -sf floppy.cc.$* floppy.cc
	ln -sf floppy.h.$* floppy.h
	touch floppy.cc floppy.h
links:	$(wildcard *.cc *.h)
	for file in $+; do \
	 declare original=$$(find $(BOCHS) -maxdepth 2 -name $$file); \
	 if [ -f $$original.orig ]; then \
	  rm -f $$original; else \
	  mv -f $$original $$original.orig; \
	 fi; \
	( cd $$(dirname $$original) && \
	  ln -sf $(CF_BOCHS)/$$file $$file) ; \
	done
checkout:
	@[ -d $(BOCHS) ] && echo Already checked out || \
	 cd $(shell dirname $(BOCHS)); $(BOCHS_LOGIN); $(BOCHS_CHECKOUT)
nolinks:
	for file in $$(find $(BOCHS) -name "*.orig"); do \
	 if [ -f "$$(basename $$file .orig)" ]; then \
	  if [ -L "$$(dirname $$file)/$$(basename $$file .orig)" ]; then \
	   rm "$$(dirname $$file)/$$(basename $$file .orig)"; \
	   mv "$$file" "$$(dirname $$file)/$$(basename $$file .orig)"; \
	  else \
	   echo "$$(dirname $$file)/$$(basename $$file .orig)" not a link; \
	  fi; \
	 else \
	  echo No such file "$$(basename $$file .orig)"; \
	 fi; \
	done
cf.patch: $(BOCHS)/bochs.exe
	-cd $(BOCHS) && cvs diff -u > $(CF_BOCHS)/$@
cvsupdate:
	$(MAKE) nolinks
	-cd $(BOCHS) && cvs up
	$(MAKE) links
