#!/usr/bin/env bash
# set -e
OUT_DIR="./bin"
COMPILER="../target/debug/exfo"

mkdir -p "$OUT_DIR"

for file in *.exfo; do
    [ -e "$file" ] || continue

    name="${file%.exfo}"

    echo "Compiling $file -> $OUT_DIR/$name"
    "$COMPILER" "$file" -o "$OUT_DIR/$name"
    echo "------------------"
done

echo "All examples are placed into $BIN folder."
