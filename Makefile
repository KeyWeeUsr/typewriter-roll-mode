EMACS := emacs

.PHONY: all
all: byte-compile test

.PHONY: clean
clean:
	@-rm typewriter*.elc 2>/dev/null
	@-rm *.ok 2>/dev/null

%.elc: %.el
	@-rm "$@" 2>/dev/null
	@$(EMACS) --batch --quick \
		--directory . \
		--load compile-setup \
		--eval '(byte-compile-file "$(subst .elc,.el,$@)")' \
		&& test -f "$@"

byte-compile: \
	typewriter-roll-mode.elc \
	typewriter-roll-mode-tests.elc

.PHONY: test
test: byte-compile main-tests

typewriter-roll-mode-tests.ok: \
	typewriter-roll-mode.elc typewriter-roll-mode-tests.elc
	$(EMACS) --batch --quick \
		--directory . \
		--load typewriter-roll-mode-tests.el \
		--funcall ert-run-tests-batch \
	&& touch typewriter-roll-mode-tests.ok
main-tests: typewriter-roll-mode-tests.ok

Makefile.ok: Makefile
	@make -n all
	@docker run \
		--network=none \
		--volume "$(PWD)"/Makefile:/Makefile \
		backplane/checkmake /Makefile
lint-makefile: Makefile.ok

.PHONY: tag
tag:
	$(MAKE) all
	git add -f . && git stash
	@grep ";; Version:" typewriter-roll-mode.el \
		| tee /dev/stderr | grep "$(TAG)"
	@git tag "$(TAG)" --sign
