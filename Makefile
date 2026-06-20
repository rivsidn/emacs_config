EMACS_DIR ?= $(HOME)/.emacs.d
STOW ?= stow
EMACS ?= emacs

.PHONY: install uninstall format

install:
	@$(STOW) --dir="$(CURDIR)" --target="$(EMACS_DIR)" --stow .
	@echo "Linked Emacs config to $(EMACS_DIR)"

uninstall:
	@$(STOW) --dir="$(CURDIR)" --target="$(EMACS_DIR)" --delete .
	@echo "Unlinked Emacs config from $(EMACS_DIR)"

format:
	@$(EMACS) -Q --batch -l scripts/format-elisp.el
