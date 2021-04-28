BUILD := ../../_build/default/demos/hco

.PHONY: all
all:
	dune build HashConsDemo.exe

.PHONY: test
test: all
	$(BUILD)/HashConsDemo.exe

.PHONY: clean
clean:
	rm -f *~
	dune clean
