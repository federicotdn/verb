SHELL = bash
EMACS ?= emacs
PORT ?= 8000
NOOUTPUT = grep -v '^Loading' | { ! grep '^'; }
VENDOR = vendor
FONT_SIZE ?= 180
ENV ?= env
ACTIVATE = source $(ENV)/bin/activate
MAX_LINE_LEN = 80

.PHONY: test

help: ## Display this help message.
	@printf 'Summary of available Makefile recipes:\n\n'
	@grep '##' Makefile | grep -v grep | column -t -s '##'
	@echo

setup-tests:  ## Install everything required for testing (Python dependencies).
	test -d $(ENV) || python3 -m venv $(ENV)
	$(ACTIVATE) && \
	pip install -U pip wheel && \
	pip install -r test/requirements-dev.txt

test: ## Run all ERT tests.
test: clean server-bg
	sleep 0.5
	$(EMACS) --batch -L . \
		 -l test/verb-test.el \
		 -f ert-run-tests-batch-and-exit; \
	ret=$$?; \
	make server-kill; \
	exit $$ret

server: ## Run a testing HTTP server on port 8000 (default).
	$(ACTIVATE) && \
	SKIP_PIDFILE=1 PORT=$(PORT) python3 test/server.py

server-bg:
	$(ACTIVATE) && \
	PORT=$(PORT) python3 test/server.py &

server-kill:
	kill $$(cat test/server.pid)

clean: ## Clean up all temporary files created during testing/linting.
	rm -f verb-autoloads.el test/server.pid
	find . -name "*.elc" -type f -delete

setup-check: ## Install everything required for linting (package-lint and relint).
	rm -rf $(VENDOR)
	mkdir $(VENDOR)
	git clone https://github.com/purcell/package-lint.git $(VENDOR)/package-lint
	git clone https://github.com/mattiase/xr.git $(VENDOR)/xr
	git clone https://github.com/mattiase/relint.git $(RELINT) $(VENDOR)/relint

lint-file:
	$(EMACS) --batch -L . \
			 --eval '(byte-compile-file "$(filename)")' 2>&1 | $(NOOUTPUT)
	yes n | $(EMACS) --batch \
			 --eval '(find-file "$(filename)")' \
			 --eval '(checkdoc-current-buffer)' 2>&1 | $(NOOUTPUT)
	$(EMACS) --batch -l $(VENDOR)/package-lint/package-lint.el \
			 -f package-lint-batch-and-exit "$(filename)"
	$(EMACS) --batch -l $(VENDOR)/xr/xr.el -l $(VENDOR)/relint/relint.el \
			 -f relint-batch "$(filename)"
	! grep -n '.\{$(MAX_LINE_LEN)\}' "$(filename)"

check: ## Lint all Emacs Lisp files in the package.
check: clean
	make lint-file filename=verb.el
	make lint-file filename=ob-verb.el
	test $$(cat *.el | grep Package-Version | uniq | wc -l) -eq 1

run: ## Run emacs -Q with the working version of verb.el loaded.
run: clean server-bg
	$(EMACS) -Q -L . \
		 --eval "(progn \
			   (when (eq system-type 'darwin) \
			     (setq mac-command-modifier 'meta)) \
			   (require 'package) \
		           (package-generate-autoloads \"verb\" \".\") \
			   (load \"verb-autoloads.el\") \
			   (add-to-list 'default-frame-alist '(fullscreen . maximized)) \
			   (set-face-attribute 'default nil :height $(FONT_SIZE)) \
			   (setq initial-scratch-message nil) \
			   (with-eval-after-load 'org (define-key org-mode-map (kbd \"C-c C-r\") verb-command-map)) \
			   (org-babel-do-load-languages \
			     'org-babel-load-languages \
			     '((verb . t))) \
			   (setq org-confirm-babel-evaluate nil) \
			   (setq verb-auto-kill-response-buffers t) \
			   (with-current-buffer (get-buffer \"*scratch*\") \
			     (org-mode) \
			     (verb-mode) \
			     (insert \"* Test :verb:\") \
			     (newline) \
			     (insert \"get http://localhost:8000/endpoints\")) \
			   (load-theme 'wombat) \
			   (setq url-debug t) \
			   (toggle-debug-on-error) \
			   (dired \"examples\") \
			   (delete-other-windows))"
	make server-kill
