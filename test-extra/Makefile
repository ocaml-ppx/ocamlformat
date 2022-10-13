######################################################################
#                                                                    #
#                            OCamlFormat                             #
#                                                                    #
#  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  #
#                                                                    #
#  This source code is licensed under the MIT license found in the   #
#  LICENSE file in the root directory of this source tree.           #
#                                                                    #
######################################################################

JS_DIRS=code/base code/base_bigstring code/base_quickcheck code/core code/core_bench code/core_compat code/core_extended code/core_kernel code/core_profiler code/core_unix

# To test all source files below a directory
#       make DIRS=<directory> test
# By default, test projects used as regression tests
DIRS=code/ocamlformat code/infer code/js_of_ocaml code/dune code/owl code/irmin code/index code/dune-release code/mirage $(JS_DIRS)

# Extra test directories, for which looser checking is done
XDIRS=code/ocaml

# Directories to ignore (given to find, compared literally)
PRUNE_DIRS= \
	code/ocamlformat/test code/ocamlformat/vendor/parser-recovery \
	code/ocaml/experimental code/ocaml/testsuite/tests/parse-errors \
	code/dune/test code/dune/vendor code/dune/otherlibs code/dune/example \
	code/infer/sledge/vendor/llvm-dune

ALL_DIRS=$(DIRS) $(XDIRS)

# To test using the dev or release executable
#       make MODE=<mode>
MODE?=default

code/%:
	@test -d code || mkdir code
	@test -d "$@" || git -C code clone "$(URI)"

code/ocamlformat: URI = ../../../ocamlformat
code/infer: URI = https://github.com/facebook/infer.git
code/js_of_ocaml: URI = https://github.com/ocsigen/js_of_ocaml.git
code/ocaml: URI = https://github.com/ocaml/ocaml.git
code/dune: URI = https://github.com/ocaml/dune.git
code/irmin: URI = https://github.com/mirage/irmin
code/index: URI = https://github.com/mirage/index
code/mirage: URI = https://github.com/mirage/mirage
code/dune-release: URI = https://github.com/ocamllabs/dune-release
code/owl: URI = https://github.com/owlbarn/owl

PRUNE_DIRS += code/mirage/test code/owl

code/base: URI = https://github.com/janestreet/base.git
code/base_bigstring: URI = https://github.com/janestreet/base_bigstring.git
code/base_quickcheck: URI = https://github.com/janestreet/base_quickcheck.git

code/core: URI = https://github.com/janestreet/core.git
code/core_bench: URI = https://github.com/janestreet/core_bench.git
code/core_compat: URI = https://github.com/janestreet/core_compat.git
code/core_extended: URI = https://github.com/janestreet/core_extended.git
code/core_kernel: URI = https://github.com/janestreet/core_kernel.git
code/core_profiler: URI = https://github.com/janestreet/core_profiler.git
code/core_unix: URI = https://github.com/janestreet/core_unix.git

.PHONY: test_setup
test_setup: $(ALL_DIRS)

.PHONY: test
test: test_setup
	@$(MAKE) test_inplace
	@-$(MAKE) test_extra
	@for dir in $(ALL_DIRS); do \
	  test -z "$$(git -C $$dir diff --quiet)" \
	    || (echo FAIL test $$dir; exit 1); \
	done

.PHONY: test_status
test_status:
	@for dir in $(ALL_DIRS); do \
	  echo ; echo $$dir; \
	  git -C $$dir status; \
	done

.PHONY: test_diff
test_diff:
	@for dir in $(ALL_DIRS); do \
	  git -C $$dir diff --no-ext-diff; \
	done

.PHONY: test_stage
test_stage:
	@for dir in $(ALL_DIRS); do \
	  git -C $$dir add .; \
	done

.PHONY: test_unstage
test_unstage:
	@for dir in $(ALL_DIRS); do \
	  git -C $$dir reset HEAD .; \
	done

.PHONY: test_clean
test_clean:
	@for dir in $(ALL_DIRS); do \
	  git -C $$dir checkout -- .; \
	  git -C $$dir clean -f; \
	done

.PHONY: test_pull
test_pull:
	@for dir in $(ALL_DIRS); do \
	  git -C $$dir pull; \
	done

FIND_ARGS= \
	-name _build -not -prune -or \
	$(patsubst %,-path % -not -prune -or,$(PRUNE_DIRS)) \
	-name '*.ml' -and -not -name '*.pp.ml' -or \
	-name '*.mli' -and -not -name '*.pp.mli'

.PHONY: test_inplace
test_inplace:
	@find $(DIRS) $(FIND_ARGS) | parallel "$(OCAMLFORMAT_EXE)" --no-version-check --enable-outside-detected-project -i

.PHONY: test_extra
test_extra:
	@find $(XDIRS) $(FIND_ARGS) | parallel "$(OCAMLFORMAT_EXE)" --no-version-check --enable-outside-detected-project --quiet -i

.PHONY: test_margins
test_margins:
	@for i in {100..40}; do echo $$i; OCAMLFORMAT_MARGIN=$$i $(MAKE) test || break; done
