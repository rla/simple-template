test:
	swipl -s tests/tests.pl -g run_tests,halt -t 'halt(1)'

doc:
	swipl -q -t 'doc_save(prolog/st, [doc_root(doc),format(html),title(simple_template),if(true),recursive(true)])'

.PHONY: test doc
