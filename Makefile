test:
	swipl -s tests/tests.pl -g run_tests,halt -t 'halt(1)'

.PHONY: test
