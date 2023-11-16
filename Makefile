EMACS := emacs

all:
	$(EMACS) --batch --quick \
		--directory . \
		--load typewriter-roll-mode-tests.el \
		--funcall ert-run-tests-batch
