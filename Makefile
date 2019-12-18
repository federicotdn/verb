EMACS = emacs
LISP_DIR = lisp

test:
	$(EMACS) -Q \
		 --batch \
		 -L . \
		 -l post-test.el \
		 -f ert-run-tests-batch-and-exit

load-examples:
	$(EMACS) -l post.el \
		 --eval "(find-file \"docs/swapi.post\")" \
		 --eval "(find-file \"req-res.post\")" \
		 --eval "(post-mode)" \
		 --eval "(delete-other-windows)"

# TODO:
# package-lint (https://github.com/purcell/package-lint)
# checkdoc
# byte-compile-file
