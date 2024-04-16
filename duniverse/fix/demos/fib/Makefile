BUILD := ../../_build/default/demos/fib

.PHONY: all
all:
	dune build Fib.exe

.PHONY: test
test: all
	$(BUILD)/Fib.exe

.PHONY: clean
clean:
	rm -f *~
	dune clean
