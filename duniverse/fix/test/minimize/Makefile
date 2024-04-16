BUILD := ../../_build/default/demos/tst

.PHONY: all
all:
	dune build Test.exe

.PHONY: test
test: all
	$(BUILD)/Test.exe

.PHONY: clean
clean:
	rm -f *~
	dune clean
