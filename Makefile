.PHONY: default
default: lib

format_.ml: format.ml
	patch -s -i format.ml.patch -o format_.ml

format_.mli: format.mli
	patch -s -i format.mli.patch -o format_.mli

.PHONY: setup
setup: format_.ml format_.mli

.PHONY: lib
lib: setup
	jbuilder build

format.ml.patch: format.ml format_.ml
	diff -u format.ml format_.ml > $@; [ $$? -eq 1 ]

format.mli.patch: format.mli format_.mli
	diff -u format.mli format_.mli > $@; [ $$? -eq 1 ]

.PHONY: patch
patch: format.ml.patch format.mli.patch

.PHONY: clean
clean:
	rm -rf format_.ml{,i} _build ocamlformat_support.install
