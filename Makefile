#general project structure
SRC:=src
OUT:=build
OBJS:=$(OUT)/obj

#flags for different builds 
#BASE_FLAGS:=-Wall -I/usr/local/include/SDL -mfpmath=sse -msse3 -mssse3
#BASE_FLAGS:=-Wall -I/usr/local/include/SDL -O3 -ffast-math -ftree-vectorize
BASE_FLAGS:=-I ./src/inc/ -f macho32 -g null
BASE_LFLAGS:=-e _main -arch i386 -lc -framework ApplicationServices
BASE_LFLAGS:= -segprot __TEXT rwx rwx -e _main -arch i386 -L/usr/local/lib -lSDL -lSDL_image -lSDL_ttf -lSDLMain -framework ApplicationServices -lc #-framework OpenGL -lm
DEBUG_FLAGS:=-g null
DEBUG_LFLAGS:=

#the executable to build to
TARGET:=$(OUT)/cf

EXT:=asm
INC_EXT:=inc
NASM:=yasm
LD:=ld
RM:=rm -rf
MKDIR:=mkdir -p

#macros to move files from the source area to the objects area
TO_OBJS=$(strip $(patsubst $(SRC)/%,$(OBJS)/%,$(1)))
TO_SRC=$(strip $(patsubst $(OBJS)/%,$(SRC)/%,$(1)))

SRC_DIRS:=$(strip $(shell find $(SRC) -type d))
OBJS_DIRS:=$(strip $(patsubst $(SRC)/%,$(OBJS)/%,$(SRC_DIRS)))
ALL_CODE_FILES:=$(strip $(foreach DIR,$(SRC_DIRS), $(wildcard $(DIR)/*.$(EXT))))
ALL_OBJECT_FILES:=$(strip $(addsuffix .o, $(basename $(call TO_OBJS,$(ALL_CODE_FILES)))))
ALL_HEADER_FILES:=$(strip $(foreach DIR,$(SRC_DIRS), $(wildcard $(DIR)/*.$(H_EXT))))
ALL_SRC_FILES:=$(ALL_CODE_FILES) $(ALL_HEADER_FILES)

.PHONY: world prebuild postbuild clean final run restore preproc sizes sdl ctest html doc testtool

# Make everything in debug
world: NASMFLAGS:=$(BASE_FLAGS) $(DEBUG_FLAGS)
world: NASMLFLAGS:=$(BASE_LFLAGS) $(DEBUG_LFLAGS)
world: prebuild $(TARGET) postbuild

doc:
	pandoc ./doc/doc.txt > ./doc/doc.html

preproc:
	yasm -e -I ./src/inc/ -f macho32 -g null -g null src/main.asm > build/main.preproc
# Make everything go away (be clean)
clean:
	$(RM) run
	$(RM) $(OUT)
	$(RM) $(TARGET)
	@ls

# This is the actual code dependencies section
$(TARGET): $(ALL_OBJECT_FILES)
	$(LD) $(NASMLFLAGS) $(LDFLAGS) $+ -o $@

%.o:
	$(NASM) $(NASMFLAGS) -l $@.lst -o $@ $(call TO_SRC,$(basename $@).$(EXT))

prebuild:
#	clear
	@$(MKDIR) $(OBJS)
	@for dir in $(OBJS_DIRS); do $(MKDIR) $${dir}; done

sizes:
	gcc -arch i386 -L/usr/local/lib -I/usr/local/include -lSDL -lSDL_image -lSDL_ttf -lSDLMain -o build/sizes src/sizes.c

sdl:
	g++ -arch i386 -L/usr/local/lib -I/usr/local/include -lSDL -lSDL_image -lSDL_ttf -lSDLMain -o build/sdl src/sdl.c

ctest:
	g++ -arch i386 -o build/ctest src/ctest.c

cf2html:
	gcc -arch i386 -o tools/cf2html tools/cf2html.c
html:
	for i in block_files/*.cf; do build/cf2html < $$i > $$i.html; done

testtool:
	./tools/colorforth_block_tool totext data/OkadWork.cf.af-ga4 build/OkadWork.1.txt
	./tools/colorforth_block_tool tocf build/OkadWork.1.txt build/OkadWork.1.cf
	./tools/colorforth_block_tool totext build/OkadWork.1.cf build/OkadWork.2.txt
	md5sum data/OkadWork.cf.af-ga4
	md5sum build/OkadWork.1.cf

postbuild:
ifeq ($(GEN_RUN),yes)
	@ln -sf $(TARGET) run
endif
