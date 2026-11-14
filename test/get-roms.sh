#!/bin/bash
# Download Blargg's cpu_instrs test ROMs into DIR (default roms/blargg/cpu_instrs).
set -euo pipefail
dir=${1:-roms/blargg/cpu_instrs}
base=https://raw.githubusercontent.com/retrio/gb-test-roms/master/cpu_instrs
mkdir -p "$dir"
for f in "01-special" "02-interrupts" "03-op sp,hl" "04-op r,imm" "05-op rp" \
         "06-ld r,r" "07-jr,jp,call,ret,rst" "08-misc instrs" "09-op r,r" \
         "10-bit ops" "11-op a,(hl)"; do
  enc=$(printf '%s' "$f.gb" | sed 's/ /%20/g; s/,/%2C/g; s/(/%28/g; s/)/%29/g')
  curl -sfL -o "$dir/$f.gb" "$base/individual/$enc"
done
curl -sfL -o "$dir/cpu_instrs.gb" "$base/cpu_instrs.gb"
curl -sfL -o "$dir/readme.txt" "$base/readme.txt"
echo "Downloaded Blargg's cpu_instrs ROMs to $dir"
