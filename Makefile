DMD = dmd

SRCS = $(shell find src -name '*.d')

# DFLAGS = -O -inline -release
DFLAGS = -g -debug

evorc: $(SRCS)
	$(DMD) $(DFLAGS) -of=$@ $^

evorc-unittest: $(SRCS)
	$(DMD) $(DFLAGS) -unittest -of=evorc-unittest $^

.PHONY: test
test: evorc-unittest
	./evorc-unittest

.PHONY: docs
docs: $(SRCS)
	$(DMD) $(DFLAGS) -D -Dd=$@ $^

.PHONY: clean
clean:
	rm -f evorc evorc-unittest
