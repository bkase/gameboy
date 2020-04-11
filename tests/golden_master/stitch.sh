#!/bin/bash

set -x
set -eou pipefail

COLOR='#e3e3e3'

if [[ $# -ne 3 ]]; then
  echo "Diffs golden and new and then stitches together a composite"
  echo ""
  echo "Usage: $0 <in.golden.png> <in.new.png> <stitched.png>"
  exit 1
fi

GOLDEN="$1"
NEW="$2"
OUT="$3"
DIFF="$(mktemp).png"

if diff -q "$GOLDEN" "$NEW"; then
  exit 0
fi

# Compute diff
echo "Make diff"
compare -compose src "$GOLDEN" "$NEW" "$DIFF" || true

GOLDEN_SCRATCH="$(mktemp).png"
NEW_SCRATCH="$(mktemp).png"
DIFF_SCRATCH="$(mktemp).png"

# Annotate
echo "Annotate"
convert "$GOLDEN" -background "$COLOR" label:'Golden' +swap -gravity Center -append "$GOLDEN_SCRATCH"
convert "$NEW" -background "$COLOR" label:'New' +swap -gravity Center -append "$NEW_SCRATCH"
convert "$DIFF" -background "$COLOR" label:'Diff' +swap -gravity Center -append "$DIFF_SCRATCH"

# Combine
echo "Combine"
TEMP12="$(mktemp).png"

convert "$GOLDEN_SCRATCH" "$NEW_SCRATCH" -size 20x20 xc:"$COLOR" +swap -gravity Center -background "$COLOR" +append "$TEMP12"
convert "$TEMP12" "$DIFF_SCRATCH" -size 20x20 xc:"$COLOR" +swap -gravity Center -background "$COLOR" +append "$OUT"

