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

.PHONY: clean
clean:
	rm -rf format_.ml{,i} _build ocamlformat_support.install
