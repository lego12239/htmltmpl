.PHONY: clean test


htmltmpl.beam: htmltmpl.erl
	erl -compile $^

clean:
	find . -name '*~' -exec rm -f '{}' \+
	rm -f htmltmpl.beam test/test.beam

test: htmltmpl.beam
	cd test && \
	erl -compile test.erl && \
	echo '\n\n' && \
	erl -noinput -pa .. -s test -s init stop
