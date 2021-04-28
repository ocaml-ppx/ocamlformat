DEMOS := tst cfg cyk fib hco brz

.PHONY: all clean test

all clean test:
	for demo in $(DEMOS) ; do \
	  $(MAKE) -C $$demo $@ ; \
	done
