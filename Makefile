SHELL = bash
EMACS = emacs

.PHONY: test

setup-tests:
	python3 -m venv env
	source env/bin/activate && pip install -r test/requirements-dev.txt

test:
	source env/bin/activate && python test/server.py &
	sleep 0.5
	$(EMACS) -Q \
		 --batch \
		 -L . \
		 -l test/verb-test.el \
		 -f ert-run-tests-batch-and-exit 2> tests.log || true
	kill $$(cat test/server.pid)
	cat tests.log
	! grep FAILED tests.log > /dev/null

load-examples:
	$(EMACS) -l verb.el \
		 --eval "(toggle-debug-on-error)" \
		 --eval "(find-file \"docs/swapi.verb\")" \
		 --eval "(find-file \"req-res.verb\")" \
		 --eval "(delete-other-windows)"

# TODO:
# package-lint (https://github.com/purcell/package-lint)
# checkdoc
# byte-compile-file
