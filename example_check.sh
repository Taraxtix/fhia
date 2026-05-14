#!/bin/sh

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR" || exit 1

SAVE=0
if [ "$1" = "--save" ]; then
    SAVE=1
fi

EXAMPLES_DIR="$SCRIPT_DIR/examples"
EXPECTED_DIR="$EXAMPLES_DIR/expected_output"

# Temp file to communicate failure across the pipe subshell boundary
FAIL_FLAG=$(mktemp)

find "$EXAMPLES_DIR" -name "*.fhia" -not -path "*/expected_output/*" | sort | while read -r example; do
    rel="${example#"$EXAMPLES_DIR"/}"
    expected_file="$EXPECTED_DIR/$rel"

    rm -f __example__.out

    # Capture both stdout and stderr; -q suppresses cargo's own build messages
    output=$(cargo run -q -- --parser --typer --llvm-ir --output __example__.out "$example" 2>&1)

    if [ -f __example__.out ]; then
        ./__example__.out
        run_code=$?
        output="$output
Return code: $run_code"
    fi

    if [ "$SAVE" -eq 1 ]; then
        mkdir -p "$(dirname "$expected_file")"
        printf '%s\n' "$output" > "$expected_file"
        echo "saved: $rel"
    else
        if [ ! -f "$expected_file" ]; then
            echo "MISSING expected output: $rel"
            echo 1 > "$FAIL_FLAG"
        else
            diff_result=$(printf '%s\n' "$output" | diff -u --color=always "$expected_file" -)
            if [ -n "$diff_result" ]; then
                echo "DIFF: $rel"
                printf '%s\n' "$diff_result"
                echo 1 > "$FAIL_FLAG"
            fi
        fi
    fi
done

rm -f __example__.out

if [ "$(cat "$FAIL_FLAG")" = "1" ]; then
    rm -f "$FAIL_FLAG"
    exit 1
fi
rm -f "$FAIL_FLAG"
exit 0