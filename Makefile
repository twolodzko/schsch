
.PHONY: test
test: unit-test integration-test

.PHONY: unit-test
unit-test:
	racket reader-test.rkt
	racket envir-test.rkt
	racket eval-test.rkt
	racket scheme-test.rkt

.PHONY: integration-test
integration-test: main
	cd examples/the-little-schemer/ && ../../main run-all.scm

.PHONY: repl
repl: main
	@ ./main

main: *.rkt
	raco exe main.rkt

.PHONY: lines
lines:
	@ find . -type f \( -name "*.rkt" -not -name "*-test.rkt" \) -exec cat {} \; | grep . | wc -l
