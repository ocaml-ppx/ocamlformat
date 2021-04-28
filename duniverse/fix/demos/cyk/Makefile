BUILD := ../../_build/default/demos/cyk

.PHONY: all
all:
	dune build Bench.exe

.PHONY: test
test: all
	$(BUILD)/Bench.exe

.PHONY: clean
clean:
	rm -f *~
	dune clean
