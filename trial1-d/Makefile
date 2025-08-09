DMD = dmd

SRC = $(shell find src/evorc -name '*.d')
SRCPP = $(SRC:src/evorc/%.d=build/evorc/%.d)

DFLAGS = -g -debug

all: evorc

evorc: $(SRCPP)
	$(DMD) $(DFLAGS) -of=$@ $^

evorc-unittest: $(SRCPP)
	$(DMD) $(DFLAGS) -unittest -of=evorc-unittest $^

build/evorc/%.d: src/evorc/%.d dpp build/evorc/utils
	./dpp -o $@ $<

dpp: dpp.d
	$(DMD) $(DFLAGS) -of=$@ $^

build/evorc/utils:
	mkdir -p build/evorc/utils

.PHONY: clean
clean:
	rm -rf build/ dpp evorc evorc-unittest
