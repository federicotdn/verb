EMACS = emacs
LISP_DIR = lisp

test:
	$(EMACS) -Q \
		 --batch \
		 -L . \
		 -l post-test.el \
		 -f ert-run-tests-batch-and-exit

load-example:
	$(EMACS) -l post.el \
		 --eval "(find-file \"docs/example.post\")" \
		 --eval "(post-mode)" \
		 --eval "(delete-other-windows)"

# TODO:
# package-lint (https://github.com/purcell/package-lint)
# checkdoc
# byte-compile-file
