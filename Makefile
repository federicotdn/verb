EMACS = emacs
LISP_DIR = lisp

test:
	$(EMACS) -Q \
		 --batch \
		 -L . \
		 -l post-test.el \
		 -f ert-run-tests-batch-and-exit

test-font-lock:
	$(EMACS) -l post.el \
		 --eval "(find-file \"docs/example.post\")" \
		 --eval "(post-mode)"

ielm:
	$(EMACS) -l post.el --eval "(ielm)"

# TODO:
# package-lint (https://github.com/purcell/package-lint)
# checkdoc
# byte-compile-file
