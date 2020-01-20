SHELL = bash
EMACS ?= emacs
NOOUTPUT = { ! grep '^'; }
PACKAGE_LINT = package-lint
FONT_SIZE ?= 180

.PHONY: test package-lint

setup-tests:
	python3 -m venv env
	source env/bin/activate && pip install -r test/requirements-dev.txt

server:
	source env/bin/activate && python3 test/server.py

test:
	rm -f verb.elc
	source env/bin/activate && python3 test/server.py &
	sleep 0.5
	$(EMACS) --batch \
		 -L . \
		 -l test/verb-test.el \
		 -f ert-run-tests-batch-and-exit 2> tests.log || true
	kill $$(cat test/server.pid)
	cat tests.log
	! grep FAILED tests.log > /dev/null

setup-check:
	git clone https://github.com/purcell/package-lint.git $(PACKAGE_LINT)

byte-compile:
	$(EMACS) -Q --batch --eval '(byte-compile-file "verb.el")' 2>&1 | $(NOOUTPUT)

checkdoc:
	yes n | $(EMACS) -Q --batch --eval '(find-file "verb.el")' \
		      	    	    --eval '(checkdoc-current-buffer)' 2>&1 | $(NOOUTPUT)

package-lint:
	$(EMACS) --batch -l $(PACKAGE_LINT)/package-lint.el \
			 -f package-lint-batch-and-exit verb.el

check: byte-compile checkdoc package-lint

load-examples:
	$(EMACS) -Q -l verb.el \
		 --eval "(add-to-list 'default-frame-alist '(fullscreen . maximized))" \
		 --eval "(set-face-attribute 'default nil :height $(FONT_SIZE))" \
		 --eval "(setq initial-scratch-message nil)" \
		 --eval "(add-hook 'org-mode-hook #'verb-mode)" \
		 --eval "(with-current-buffer (get-buffer \"*scratch*\") (org-mode))" \
		 --eval "(load-theme 'wombat)" \
		 --eval "(setq url-debug t)" \
		 --eval "(toggle-debug-on-error)" \
		 --eval "(dired \"docs\")" \
		 --eval "(delete-other-windows)"
