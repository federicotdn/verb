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

run:
	rm -f verb-autoloads.el verb.elc
	$(EMACS) -Q -L . \
		 --eval "(progn \
			   (require 'package) \
			   (package-initialize) \
		           (package-generate-autoloads \"verb\" \".\") \
			   (load \"verb-autoloads.el\") \
			   (add-to-list 'default-frame-alist '(fullscreen . maximized)) \
			   (set-face-attribute 'default nil :height $(FONT_SIZE)) \
			   (setq initial-scratch-message nil) \
			   (with-eval-after-load 'org (define-key org-mode-map (kbd \"C-c C-r\") verb-mode-prefix-map)) \
			   (with-current-buffer (get-buffer \"*scratch*\") (org-mode)) \
			   (load-theme 'wombat) \
			   (setq url-debug t) \
			   (toggle-debug-on-error) \
			   (dired \"docs\") \
			   (delete-other-windows))"
