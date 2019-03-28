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
DIRS=code/ocamlformat code/infer code/js_of_ocaml

# Extra test directories, for which looser checking is done
XDIRS=code/ocaml

ALL_DIRS=$(DIRS) $(XDIRS)

# To test using the dev or release executable
#       make MODE=<mode>
MODE?=dev

REFMT=refmt

OCAMLFORMAT_EXE=../_build/$(MODE)/src/ocamlformat.exe

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

%.pp.ml: %.ml $(OCAMLFORMAT_EXE)
	@rm -f $*.pp*.ml $*.pp.old.ast $*.pp.new.ast
	$(OCAMLFORMAT_EXE) $< -o $*.pp.ml

%.pp.mli: %.mli $(OCAMLFORMAT_EXE)
	@rm -f $*.pp*.mli $*.old.ast $*.new.ast
	$(OCAMLFORMAT_EXE) $< -o $*.pp.mli

%.ml: %.re $(OCAMLFORMAT_EXE)
	@rm -f $*.pp.ml $*.old.ast $*.new.ast
	$(REFMT) --print=binary_reason $< | $(OCAMLFORMAT_EXE) $< --reason-impl -o $@

%.mli: %.rei $(OCAMLFORMAT_EXE)
	@rm -f $*.pp.mli $*.old.ast $*.new.ast
	$(REFMT) --print=binary_reason $< | $(OCAMLFORMAT_EXE) $< --reason-intf -o $@

TEST_ML:=$(shell find $(DIRS) -name _build -not -prune -or -name gen_version.ml -not -prune -or -path code/ocamlformat/test -not -prune -or -name '*'.ml -and -not -name '*'.pp.ml 2>/dev/null)
TEST_MLI:=$(shell find $(DIRS) -name _build -not -prune -or -path code/ocamlformat/test -not -prune -or -name '*'.mli -and -not -name '*'.pp.mli 2>/dev/null)

XTEST_ML:=$(shell find $(XDIRS) -path code/ocaml/experimental -not -prune -or -path code/ocaml/testsuite/tests/parse-errors -not -prune -or -name '*'.ml -and -not -name '*'.pp.ml 2>/dev/null)
XTEST_MLI:=$(shell find $(XDIRS) -path code/ocaml/experimental -not -prune -or -path code/ocaml/testsuite/tests/parse-errors -not -prune -or -name '*'.mli -and -not -name '*'.pp.mli 2>/dev/null)

TEST_PP_ML:=$(patsubst %.ml,%.pp.ml,$(TEST_ML))
TEST_PP_MLI:=$(patsubst %.mli,%.pp.mli,$(TEST_MLI))

XTEST_PP_ML:=$(patsubst %.ml,%.pp.ml,$(XTEST_ML))
XTEST_PP_MLI:=$(patsubst %.mli,%.pp.mli,$(XTEST_MLI))

.PHONY: test_format
test_format: $(TEST_PP_ML) $(TEST_PP_MLI)

.PHONY: test_inplace
test_inplace:
	@parallel --bar $(OCAMLFORMAT_EXE) --no-version-check -i ::: $(TEST_ML) $(TEST_MLI)

.PHONY: test_inplace
test_extra:
	@parallel --bar $(OCAMLFORMAT_EXE) --no-version-check --quiet -i ::: $(XTEST_ML) $(XTEST_MLI)

.PHONY: test_margins
test_margins:
	@for i in {100..40}; do echo $$i; OCAMLFORMAT_MARGIN=$$i $(MAKE) test || break; done

.PHONY: clean_pp
clean_pp:
	@find $(ALL_DIRS) \
	 \(  -name '*'.pp'*'.ml \
	 -or -name '*'.pp'*'.mli \
	 -or -name '*'.ast \
	 \) -delete

print-%:
	@echo '$*=$($*)'

.PHONY: regtests
regtests:
	@echo running regtests with $(OCAMLFORMAT_EXE)
	@./test.sh --ocamlformat $(OCAMLFORMAT_EXE)
