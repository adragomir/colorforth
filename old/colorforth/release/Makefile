BOCHS := /cygdrive/c/Program\ Files/Bochs-2.3
BOCHS_SRC := /usr/src/bochs
CF_SRC := ~/src
BXSHARE := $(shell cygpath -w $(BOCHS))
CFIMAGE := a.img
TIMESTAMP := $(shell date +%Y%m%d%H%M)
NSIS := /cygdrive/c/Program\ Files/NSIS/makensis.exe
export
cfbochs.zip: bochs.exe bochsdbg.exe a.img bochsrc.bxrc cfbochs.bat \
	 README.txt a.img.gz
	cp -f $+ cfBochs
	zip -r $@ cfBochs
a.img:	$(CF_SRC)/colorforth/jc2007/color.com
	dd if=/dev/zero of=$@ bs=1024 count=1440
	dd if=$< of=$@ conv=notrunc
a.img.gz: a.img
	gzip -c $< > $@
set:
	set
test:	cfbochs.zip
	cp $< /cygdrive/c/WINDOWS/Temp/
clean:
	rm -f *.exe a.img *.zip *.rtf cfBochs/* LICENSE.txt
%.exe:
	cp -f $(BOCHS_SRC)/$@ .
upload: cfbochs-jc2007.exe a.img.gz
	for file in $+; do \
	 ncftpput -C upload.sourceforge.net $$file \
	  incoming/$(TIMESTAMP)-$$file; \
	done
cfbochs-jc2007.exe: cfbochs.nsi LICENSE.rtf README.rtf cfbochs.zip
	$(NSIS) $<
LICENSE.txt: NOLICENSE.txt /usr/src/bochs/COPYING
	cat $+ > $@
nsiscommands:
	$(NSIS) /CMDHELP
nsishelp:
	$(NSIS) /?
%.rtf: %.txt  # to force association to something other than Notepad
	cp $< $@
