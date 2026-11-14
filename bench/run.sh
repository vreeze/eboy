#!/bin/bash
# Run the eboy benchmarks against a compiled copy of the current sources.
# Usage: bench/run.sh [results-file]      (default: bench/results-<commit>.txt)
set -euo pipefail
cd "$(dirname "$0")/.."
export LANG=en_US.UTF-8
EMACS=${EMACS:-emacs}
commit=$(git rev-parse --short HEAD)$(git diff --quiet HEAD -- '*.el' || echo "-dirty")
out=${1:-bench/results-$commit.txt}
out="$(cd "$(dirname "$out")" && pwd)/$(basename "$out")"
: > "$out"
export EBOY_BENCH_OUT="$out"
# Real-ROM workload (skipped if missing; `make roms` downloads it)
export EBOY_BENCH_ROM=${EBOY_BENCH_ROM:-$PWD/roms/blargg/cpu_instrs/10-bit ops.gb}

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cp eboy-macros.el eboy-cpu.el eboy.el test/eboy-headless.el bench/eboy-bench.el "$work/"
srcs='(list "eboy-macros.el" "eboy-cpu.el" "eboy.el" "eboy-headless.el" "eboy-bench.el")'

echo "# eboy $commit, $($EMACS --batch --eval '(princ emacs-version)' 2>/dev/null), $(sysctl -n machdep.cpu.brand_string 2>/dev/null || uname -m)" >> "$out"

# Byte-compiled
(cd "$work" && $EMACS -Q --batch -L . -f batch-byte-compile eboy-macros.el eboy-cpu.el eboy.el eboy-headless.el eboy-bench.el >/dev/null 2>&1)
load_byte="(dolist (f $srcs) (load (expand-file-name (concat (file-name-sans-extension f) \".elc\") \"$work\")))"
$EMACS -Q --batch -L "$work" --eval "$load_byte" --eval '(eboy-bench-batch "byte")' 2>&1 | grep '^|'
$EMACS -Q -L "$work" --eval "$load_byte" --eval '(eboy-bench-gui "byte")'

# Native-compiled
load_native="(progn (setq byte-compile-warnings nil inhibit-message t) (dolist (f $srcs) (load (native-compile (expand-file-name f \"$work\")) nil t)))"
$EMACS -Q --batch -L "$work" --eval "$load_native" --eval '(eboy-bench-batch "native")' 2>&1 | grep '^|'
$EMACS -Q -L "$work" --eval "$load_native" --eval '(eboy-bench-gui "native")'

echo "Results written to $out"
