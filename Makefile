SHELL = bash
EMACS ?= emacs
PORT ?= 8000
PACKAGES = packages
FONT_SIZE ?= 180
MAX_LINE_LEN = 80
WAIT_TIME ?= 0.5
S ?= t
SELECTOR ?= $S

.PHONY: test server

help: ## Display this help message.
	@printf 'Summary of available Makefile recipes:\n\n'
	@grep '##' Makefile | grep -v grep | column -t -s '##'
	@echo

test: ## Run all ERT tests (set SELECTOR to specify only one).
test: clean server-bg
	sleep $(WAIT_TIME)
	$(EMACS) --batch -L . \
		 -l test/verb-test.el \
		 --eval "(ert-run-tests-batch-and-exit '$(SELECTOR))"; \
	ret=$$?; \
	make server-kill; \
	exit $$ret

test-noserver:
	$(EMACS) --batch -L . \
		 -l test/verb-test.el \
		 --eval "(ert-run-tests-batch-and-exit '$(SELECTOR))"; \

server: ## Run a testing HTTP server on port 8000 (default).
	go build test/server.go
	SKIP_PIDFILE=1 PORT=$(PORT) ./server

server-bg:
	go build test/server.go
	PORT=$(PORT) ./server &

server-kill:
	kill $$(cat test/server.pid)
	sleep 1

clean: ## Clean up all temporary files created during testing/runtime.
	rm -f verb-autoloads.el test/server.pid server
	find . -name "*.elc" -type f -delete

setup-check: ## Install packages required for linting.
	rm -rf $(PACKAGES)
	$(EMACS) --batch \
		 --eval "(setq package-user-dir \"$$PWD/$(PACKAGES)\")" \
		 -l test/init-check.el

lint-file:
	@printf "\n<<<------------ Lint file: $(filename) ------------>>>\n"
	@printf "\n--> Step: Byte-compile file\n\n"
	$(EMACS) --batch -L . \
			 --eval "(setq byte-compile-error-on-warn t)" \
			 -f batch-byte-compile "$(filename)"
	@printf "\n--> Step: Run checkdoc\n\n"
	yes n | $(EMACS) --batch \
             -l test/checkdoc-batch.el \
			 -f checkdoc-batch-and-exit "$(filename)"
	@printf "\n--> Step: Run package-lint\n\n"
	$(EMACS) --batch --eval "(setq package-user-dir \"$$PWD/$(PACKAGES)\")" \
			 --eval "(package-initialize)" \
			 --eval "(require 'package-lint)" \
			 -f package-lint-batch-and-exit "$(filename)"
	@printf "\n--> Step: Run relint\n\n"
	$(EMACS) --batch --eval "(setq package-user-dir \"$$PWD/$(PACKAGES)\")" \
			 --eval "(package-initialize)" \
			 --eval "(require 'relint)" \
			 -f relint-batch "$(filename)"
	@printf "\n--> Step: Ensure maximum line length\n\n"
	! grep -n '.\{$(MAX_LINE_LEN)\}' "$(filename)"

check: ## Lint all Emacs Lisp files in the package.
check: clean
	make lint-file filename=verb.el
	make lint-file filename=verb-util.el
	make lint-file filename=ob-verb.el
	test $$(cat *.el | grep Package-Version | uniq | wc -l) -eq 1

update: ## Update the package version number (version=X.Y.Z).
update:
	@test -n "$(version)" || (echo "version not set!" && exit 1)
	@grep -q "$(version)" CHANGELOG.md || (echo "Update changelog first!" && exit 1)
	sed -i -e "s/^;; Package-Version: .*/;; Package-Version: $(version)/g" verb.el ob-verb.el verb-util.el
	sed -i -e "s/defconst verb-version .*/defconst verb-version \"$(version)\"/g" verb.el

run: ## Run emacs -Q with the working version of verb.el loaded.
run: clean server-bg
	make run-noserver; \
	make server-kill

run-noserver: ## Same as "run", but without the test HTTP server running.
	FONT_SIZE=$(FONT_SIZE) $(EMACS) -Q -L . --load test/init.el
