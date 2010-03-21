Any files needed for making Bochs work as a host for colorForth should be
here; rename the same files under your Bochs sources (/usr/src/bochs on my
machine) to .orig, and symlink to these files before building. The Makefile
will do this for you: "make links".

Check the Makefile before trying to guess anything; chances are there's at
least a hint, if not a solution.

Quickstart: "make force" will generate $(BOCHS)/bochs.exe and
$(BOCHS)/bochsdbg.exe suitable for running colorForth. Then "cd ../as", and
"make test". Don't play with it too long that way, it's doing verbose logging
unless you edited the bxrc file. Use the one under ../release instead.
