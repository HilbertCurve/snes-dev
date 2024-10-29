SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .s .o .smc

# make should be kinda quiet
MAKEFLAGS += --silent

# this our assembler
AS = ca65
ASFLAGS = --cpu 65816
# and our linker
LINK = ld65
# change fella for a different memory map
MEMMAP = ./memmap.cfg
# change for different name
EXE_NAME = sprite
# we be searching src for targets and deps
VPATH = ./src
# we grab just the name of the sources...
SOURCES = $(patsubst src/%.s, %.s, $(wildcard src/*.s))
# to get our object file names
OBJECTS = $(patsubst %.s, %.o, $(SOURCES))

.PHONY: build
build: $(OBJECTS)
	# make build dir if not found
	mkdir -p build
	# for all objects, move em to build dir
	for f in $(OBJECTS) ; do \
		mv ./$$f ./build/$$f ; \
	done
	# go into build dir and link our fella
	cd ./build ; \
	$(LINK) -C ../$(MEMMAP) -o ../$(EXE_NAME).smc $^ ; \

# pattern for matching all buildable files
%.o: %.s
	$(AS) $(ASFLAGS) $< -o $@

# clean out all build stuff
.PHONY: clean
clean:
	rm -r ./build
