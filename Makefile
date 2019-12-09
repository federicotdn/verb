EMACS = emacs
LISP_DIR = lisp

test:
	$(EMACS) -Q \
		--batch \
		-L $(LISP_DIR) \
		-l t/post-test.el \
		-f ert-run-tests-batch-and-exit

# TODO:
# package-lint (https://github.com/purcell/package-lint)
# checkdoc
# byte-compile-file
