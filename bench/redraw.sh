#!/bin/bash
# Redraw experiment: compare ways of showing a frame (see bench/eboy-redraw.el).
# Usage: bench/redraw.sh [results-file]   (default: bench/redraw-<commit>.txt)
set -euo pipefail
cd "$(dirname "$0")/.."
export LANG=en_US.UTF-8
EMACS=${EMACS:-emacs}
commit=$(git rev-parse --short HEAD)$(git diff --quiet HEAD -- '*.el' || echo "-dirty")
out=${1:-bench/redraw-$commit.txt}
out="$(cd "$(dirname "$out")" && pwd)/$(basename "$out")"
export EBOY_BENCH_OUT="$out"
export EBOY_BENCH_ROM=${EBOY_BENCH_ROM:-$PWD/roms/blargg/cpu_instrs/10-bit ops.gb}

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
files="eboy-macros.el eboy-cpu.el eboy.el test/eboy-headless.el bench/eboy-bench.el bench/eboy-redraw.el"
cp $files "$work/"
srcs="(list $(for f in $files; do printf '"%s" ' "$(basename $f)"; done))"

{ echo "# eboy $commit, $($EMACS --batch --eval '(princ emacs-version)' 2>/dev/null), $(sysctl -n machdep.cpu.brand_string 2>/dev/null || uname -m)"
  echo "| Variant | Workload | Build ms | Insert ms | Redisplay ms | Total ms/frame | Shown px |"
  echo "|-"; } > "$out"

(cd "$work" && $EMACS -Q --batch -L . -f batch-byte-compile $(for f in $files; do basename $f; done) >/dev/null 2>&1)
load_byte="(dolist (f $srcs) (load (expand-file-name (concat (file-name-sans-extension f) \".elc\") \"$work\") nil t))"
$EMACS -Q -L "$work" --eval "$load_byte" --eval '(eboy-redraw-gui "byte")'

load_native="(progn (setq byte-compile-warnings nil inhibit-message t) (dolist (f $srcs) (load (native-compile (expand-file-name f \"$work\")) nil t)))"
$EMACS -Q -L "$work" --eval "$load_native" --eval '(eboy-redraw-gui "native")'

cat "$out"
