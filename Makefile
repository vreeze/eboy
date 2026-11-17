# Eboy development tasks.  Run `make help` for an overview.
EMACS ?= emacs
SRCS   = eboy-macros.el eboy-cpu.el eboy.el
TEST   = test/eboy-headless.el test/eboy-mbc-test.el
BATCH  = LANG=en_US.UTF-8 $(EMACS) -Q --batch -L . -L test

.PHONY: all help run test test-visual bench bench-redraw roms clean

all: $(SRCS:.el=.elc) $(TEST:.el=.elc) native-stamp  ## Byte- and native-compile

help:  ## Show this help
	@grep -E '^[a-z-]+:.*## ' $(MAKEFILE_LIST) | sed 's/:.*## /\t/'

%.elc: %.el eboy-macros.el
	@$(BATCH) --eval '(setq byte-compile-warnings nil)' -f batch-byte-compile $<

# Native code goes to the eln-cache; Emacs loads it automatically for the .elc.
native-stamp: $(SRCS) $(TEST)
	@$(BATCH) --eval "(if (not (native-comp-available-p)) (message \"No native compilation in this Emacs, skipping\") \
	  (progn (setq byte-compile-warnings nil) (dolist (f '($(patsubst %,\"%\",$^))) (native-compile f))))"
	@touch $@

run: all  ## Start eboy in a fresh Emacs (ROM=path/to/game.gb, or pick one)
	@EBOY_ROM="$(ROM)" LANG=en_US.UTF-8 $(EMACS) -Q --chdir "$(CURDIR)" -L . -l eboy --eval \
	  "(add-hook 'window-setup-hook (lambda () (let ((rom (getenv \"EBOY_ROM\"))) \
	     (if (string-empty-p rom) (call-interactively #'eboy-load-rom) (eboy-load-rom rom)))))"

test: all roms/blargg/cpu_instrs  ## Run Blargg's cpu_instrs tests headless
	@$(BATCH) -l eboy-headless -f eboy-blargg-run-batch
	@$(BATCH) -l eboy-mbc-test -f eboy-mbc-test-run-batch

test-visual: all roms/blargg/cpu_instrs  ## Run the same tests in a GUI Emacs, with display
	@LANG=en_US.UTF-8 $(EMACS) -Q --chdir "$(CURDIR)" -L . -L test -l eboy-headless \
	  --eval "(add-hook 'window-setup-hook #'eboy-blargg-run-visual)"

bench: roms/blargg/cpu_instrs  ## Run the benchmarks (opens a GUI Emacs twice)
	@bench/run.sh

bench-redraw: roms/blargg/cpu_instrs  ## Redraw experiment: ways to show a frame (GUI)
	@bench/redraw.sh

roms: roms/blargg/cpu_instrs  ## Download Blargg's test ROMs (not committed)

roms/blargg/cpu_instrs:
	@test/get-roms.sh $@

clean:  ## Remove compiled files
	rm -f *.elc test/*.elc native-stamp
