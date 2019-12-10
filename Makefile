EMACS = emacs
LISP_DIR = lisp

test:
	$(EMACS) -Q \
		--batch \
		-L . \
		-l post-test.el \
		-f ert-run-tests-batch-and-exit

# TODO:
# package-lint (https://github.com/purcell/package-lint)
# checkdoc
# byte-compile-file
