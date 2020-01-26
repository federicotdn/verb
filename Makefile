SHELL = bash
EMACS ?= emacs
PORT ?= 8000
NOOUTPUT = { ! grep '^'; }
PACKAGE_LINT = package-lint
FONT_SIZE ?= 180
ENV ?= env
ACTIVATE = source $(ENV)/bin/activate

.PHONY: test package-lint

setup-tests:
	test -d $(ENV) || python3 -m venv $(ENV)
	$(ACTIVATE) && \
	pip install -U pip wheel && \
	pip install -r test/requirements-dev.txt

server:
	$(ACTIVATE) && \
	SKIP_PIDFILE=1 PORT=$(PORT) python3 test/server.py

server-bg:
	$(ACTIVATE) && \
	PORT=$(PORT) python3 test/server.py &

server-kill:
	kill $$(cat test/server.pid)

clean:
	rm -f verb-autoloads.el test/server.pid
	find . -name "*.elc" -type f -delete

test: clean server-bg
	sleep 0.5
	$(EMACS) --batch -L . \
		 -l test/verb-test.el \
		 -f ert-run-tests-batch-and-exit; \
	ret=$$?; \
	make server-kill; \
	exit $$ret

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

check: clean
	make lint-file filename=verb.el
	make lint-file filename=ob-verb.el

run: clean server-bg
	$(EMACS) -Q -L . \
		 --eval "(progn \
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
			     (insert \"get http://localhost:8000/\")) \
			   (load-theme 'wombat) \
			   (setq url-debug t) \
			   (toggle-debug-on-error) \
			   (dired \"docs\") \
			   (delete-other-windows))"
	make server-kill
