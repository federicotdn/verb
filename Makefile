EMACS = emacs
LISP_DIR = lisp

test:
	$(EMACS) -Q \
		 --batch \
		 -L . \
		 -l verb-test.el \
		 -f ert-run-tests-batch-and-exit

load-examples:
	$(EMACS) -l verb.el \
		 --eval "(toggle-debug-on-error)" \
		 --eval "(find-file \"docs/swapi.verb\")" \
		 --eval "(find-file \"req-res.verb\")" \
		 --eval "(delete-other-windows)"

# TODO:
# package-lint (https://github.com/purcell/package-lint)
# checkdoc
# byte-compile-file
