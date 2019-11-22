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

FIND_ARGS= \
	-name _build -not -prune -or \
	-path code/ocamlformat/test -not -prune -or \
	-path code/ocaml/experimental -not -prune -or \
	-path code/ocaml/testsuite/tests/parse-errors -not -prune -or \
	-path code/dune/test -not -prune -or \
	-path code/dune/vendor -not -prune -or \
	-path code/dune/otherlibs -not -prune -or \
	-path code/dune/example -not -prune -or \
	-name '*.ml' -and -not -name '*.pp.ml' -or \
	-name '*.mli' -and -not -name '*.pp.mli'

ALL_DIRS=$(DIRS) $(XDIRS)

# To test using the dev or release executable
#       make MODE=<mode>
MODE?=default

OCAMLFORMAT_EXE=../_build/$(MODE)/bin/ocamlformat.exe

.PHONY: test_setup
test_setup:
	@test -d code \
	  || mkdir code
	@test -d code/ocamlformat \
	  || git -C code clone ../../../ocamlformat
	@test -d code/infer \
	  || git -C code clone "https://github.com/facebook/infer.git"
	@test -d code/js_of_ocaml \
	  || git -C code clone "https://github.com/ocsigen/js_of_ocaml.git"
	@test -d code/ocaml \
	  || git -C code clone "https://github.com/ocaml/ocaml.git"
	@test -d code/dune \
		|| git -C code clone "https://github.com/ocaml/dune.git"

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

TEST_FILES:=$(shell find $(DIRS) $(FIND_ARGS) 2>/dev/null)

XTEST_FILES:=$(shell find $(XDIRS) $(FIND_ARGS) 2>/dev/null)

.PHONY: test_inplace
test_inplace: $(OCAMLFORMAT_EXE)
	@parallel --bar $(OCAMLFORMAT_EXE) --no-version-check -i ::: $(TEST_FILES)

.PHONY: test_extra
test_extra: $(OCAMLFORMAT_EXE)
	@parallel --bar $(OCAMLFORMAT_EXE) --no-version-check --quiet -i ::: $(XTEST_FILES)

.PHONY: test_margins
test_margins:
	@for i in {100..40}; do echo $$i; OCAMLFORMAT_MARGIN=$$i $(MAKE) test || break; done

$(OCAMLFORMAT_EXE):
	dune build "$@"

print-%:
	@echo '$*=$($*)'
