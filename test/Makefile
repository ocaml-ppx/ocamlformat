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
DIRS=code/ocamlformat code/infer

# To test using the dbg or "default" (opt) executable
#       make MODE=<mode>
MODE?=dbg

REFMT=refmt

OCAMLFORMAT_EXE=../_build/$(MODE)/src/ocamlformat.exe

.PHONY: test
test:
	test -d code \
	  || mkdir code
	test -d code/ocamlformat \
	  || git -C code clone ../../../ocamlformat
	test -d code/infer \
	  || git -C code clone "https://github.com/facebook/infer.git"
	$(MAKE) test_inplace test_convert
	for dir in $(DIRS); do \
	  test -z "$$(git -C $$dir diff --quiet)" \
	    || (echo FAIL test $$dir; exit 1); \
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

TEST_ML:=$(shell find $(DIRS) -name _build -not -prune -or -name failing -not -prune -or -name failing-output -not -prune -or -name '*'.ml -and -not -name '*'.pp.ml 2>/dev/null)
TEST_MLI:=$(shell find $(DIRS) -name _build -not -prune -or -name failing -not -prune -or -name failing-output -not -prune -or -name '*'.mli -and -not -name '*'.pp.mli 2>/dev/null)
TEST_RE:=$(shell find $(DIRS) -name '*'.re 2>/dev/null)
TEST_REI:=$(shell find $(DIRS) -name '*'.rei 2>/dev/null)

CONV_ML:=$(patsubst %.re,%.ml,$(TEST_RE))
CONV_MLI:=$(patsubst %.rei,%.mli,$(TEST_REI))

TEST_PP_ML:=$(patsubst %.ml,%.pp.ml,$(TEST_ML))
TEST_PP_MLI:=$(patsubst %.mli,%.pp.mli,$(TEST_MLI))
TEST_PP_RE:=$(patsubst %.re,%.pp.ml,$(TEST_RE))
TEST_PP_REI:=$(patsubst %.rei,%.pp.mli,$(TEST_REI))

.PHONY: test_format
test_format: $(TEST_PP_ML) $(TEST_PP_MLI) $(TEST_PP_RE) $(TEST_PP_REI)

.PHONY: test_inplace
test_inplace:
	parallel $(OCAMLFORMAT_EXE) --no-version-check -i ::: $(TEST_ML) $(TEST_MLI)

.PHONY: test_convert
test_convert: $(CONV_ML) $(CONV_MLI)

.PHONY: test_convert_inplace
test_convert_inplace:
	parallel -i sh -c "$(REFMT) --print=binary_reason {} | $(OCAMLFORMAT_EXE) {} --reason-impl -o {}" -- $(TEST_RE)
	parallel -i sh -c "$(REFMT) --print=binary_reason {} | $(OCAMLFORMAT_EXE) {} --reason-intf -o {}" -- $(TEST_REI)

.PHONY: test_margins
test_margins:
	@for i in {100..40}; do echo $$i; OCAMLFORMAT_MARGIN=$$i $(MAKE) test || break; done

.PHONY: clean_pp
clean_pp:
	@find $(DIRS) \
	 \(  -name '*'.pp'*'.ml \
	 -or -name '*'.pp'*'.mli \
	 -or -name '*'.ast \
	 \) -delete

print-%:
	@echo '$*=$($*)'

.PHONY: regtests
regtests:
	echo running regtests with $(OCAMLFORMAT_EXE)
	@./test.sh --ocamlformat $(OCAMLFORMAT_EXE)
