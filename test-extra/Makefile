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

# To test all source files below a directory
#       make DIRS=<directory> test
# By default, test projects used as regression tests
DIRS=code/ocamlformat code/infer code/js_of_ocaml code/dune

# Extra test directories, for which looser checking is done
XDIRS=code/ocaml

# Directories to ignore (given to find, compared literally)
PRUNE_DIRS= \
	code/ocamlformat/test \
	code/ocaml/experimental code/ocaml/testsuite/tests/parse-errors \
	code/dune/test code/dune/vendor code/dune/otherlibs code/dune/example

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

# DIRS += code/owl code/irmin code/index code/duniverse code/dune-release \
# 	code/coq code/mirage code/alcotest code/citty
# PRUNE_DIRS += code/duniverse/duniverse code/duniverse/examples
# code/owl: URI = https://github.com/owlbarn/owl
# code/irmin: URI = https://github.com/mirage/irmin
# code/index: URI = https://github.com/mirage/index
# code/duniverse: URI = https://github.com/ocamllabs/duniverse
# code/dune-release: URI = https://github.com/ocamllabs/dune-release
# code/coq: URI = https://github.com/coq/coq
# code/mirage: URI = https://github.com/mirage/mirage
# code/alcotest: URI = https://github.com/mirage/alcotest
# code/citty: URI = https://github.com/ocurrent/citty

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
	@find $(DIRS) $(FIND_ARGS) | parallel --bar "$(OCAMLFORMAT_EXE)" --no-version-check -i

.PHONY: test_extra
test_extra:
	@find $(XDIRS) $(FIND_ARGS) | parallel --bar "$(OCAMLFORMAT_EXE)" --no-version-check --quiet -i

.PHONY: test_margins
test_margins:
	@for i in {100..40}; do echo $$i; OCAMLFORMAT_MARGIN=$$i $(MAKE) test || break; done
