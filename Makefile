SHELL = bash
EMACS = emacs
NOOUTPUT = { ! grep '^'; }

.PHONY: test

setup-tests:
	python3 -m venv env
	source env/bin/activate && pip install -r test/requirements-dev.txt

server:
	source env/bin/activate && python3 test/server.py

test:
	rm -f verb.elc
	source env/bin/activate && python3 test/server.py &
	sleep 0.5
	$(EMACS) -Q \
		 --batch \
		 -L . \
		 -l test/verb-test.el \
		 -f ert-run-tests-batch-and-exit 2> tests.log || true
	kill $$(cat test/server.pid)
	cat tests.log
	! grep FAILED tests.log > /dev/null

# TODO:
# package-lint (https://github.com/purcell/package-lint)

check:
	$(EMACS) -Q --batch --eval '(byte-compile-file "verb.el")' 2>&1 | $(NOOUTPUT)
	yes n | $(EMACS) -Q --batch --eval '(find-file "verb.el")' \
		      	    	    --eval '(checkdoc-current-buffer)'

load-examples:
	$(EMACS) -l verb.el \
		 --eval "(with-current-buffer (get-buffer \"*scratch*\") (verb-mode))" \
		 --eval "(setq verb--debug-enable t)" \
		 --eval "(setq confirm-kill-emacs nil)" \
		 --eval "(setq url-debug t)" \
		 --eval "(toggle-debug-on-error)" \
		 --eval "(dired \"docs\")" \
		 --eval "(delete-other-windows)"
