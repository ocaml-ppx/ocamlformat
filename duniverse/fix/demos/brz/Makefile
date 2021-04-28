SHELL := bash
BUILD := ../../_build/default/demos/brz

.PHONY: all
all:
	dune build Main.exe

.PHONY: test
test: all
	$(BUILD)/Main.exe
	@ if command -v dot >/dev/null ; then \
	  for f in *.dot ; do \
	    dot -Tpng $$f -O ; \
	  done ; \
	else \
	  echo "The program dot is required to visualize DFAs. Please install graphviz." ; \
	fi

# This stress test, contributed by Gabriel Radanne, takes about 5 minutes.
.PHONY: testgen
testgen:
	dune build TestGen.exe
	$(BUILD)/TestGen.exe

.PHONY: clean
clean:
	rm -f *~ *.dot.png
	dune clean
