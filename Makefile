EMACS = emacs
LISP_DIR = lisp

test:
	$(EMACS) -Q \
		--batch \
		-L . \
		-l post-test.el \
		-f ert-run-tests-batch-and-exit

ielm:
	$(EMACS) -l post.el --eval "(ielm)"

# TODO:
# package-lint (https://github.com/purcell/package-lint)
# checkdoc
# byte-compile-file
