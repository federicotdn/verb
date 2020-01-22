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
	$(EMACS) --batch -L . \
		 -l test/verb-test.el \
		 -f ert-run-tests-batch-and-exit 2> tests.log || true
	kill $$(cat test/server.pid)
	cat tests.log
	! grep FAILED tests.log > /dev/null

setup-check:
	git clone https://github.com/purcell/package-lint.git $(PACKAGE_LINT)

lint-file:
	$(EMACS) --batch -L . \
			 --eval '(byte-compile-file "$(filename)")' 2>&1 | $(NOOUTPUT)
	yes n | $(EMACS) --batch \
			 --eval '(find-file "$(filename)")' \
			 --eval '(checkdoc-current-buffer)' 2>&1 | $(NOOUTPUT)
	$(EMACS) --batch -l $(PACKAGE_LINT)/package-lint.el \
			 -f package-lint-batch-and-exit "$(filename)"

check:
	make lint-file filename=verb.el
	make lint-file filename=ob-verb.el

run:
	rm -f verb-autoloads.el verb.elc ob-verb.elc
	$(EMACS) -Q -L . \
		 --eval "(progn \
			   (require 'package) \
		           (package-generate-autoloads \"verb\" \".\") \
			   (load \"verb-autoloads.el\") \
			   (add-to-list 'default-frame-alist '(fullscreen . maximized)) \
			   (set-face-attribute 'default nil :height $(FONT_SIZE)) \
			   (setq initial-scratch-message nil) \
			   (with-eval-after-load 'org (define-key org-mode-map (kbd \"C-c C-r\") verb-mode-prefix-map)) \
			   (org-babel-do-load-languages \
			     'org-babel-load-languages \
			     '((verb . t))) \
			   (setq org-confirm-babel-evaluate nil) \
			   (setq verb-auto-kill-response-buffers t) \
			   (with-current-buffer (get-buffer \"*scratch*\") (org-mode)) \
			   (load-theme 'wombat) \
			   (setq url-debug t) \
			   (toggle-debug-on-error) \
			   (dired \"docs\") \
			   (delete-other-windows))"
