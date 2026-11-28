#!/usr/bin/env bash
set -euo pipefail

source "$(dirname "${BASH_SOURCE[0]}")/sagittarius.sh"
[[ $# == 0 || ($# == 1 && "$1" == --real) ]] || die "Usage: test-workflows.sh [--real]"
TEMP_ROOT=$(workspace_path .tmp)
mkdir -p "$TEMP_ROOT"
TEST_WORK=$(mktemp -d "$TEMP_ROOT/skill-tests.XXXXXXXX")
trap 'rm -rf "$TEST_WORK"' EXIT
FIXTURE="$TEST_WORK/fixture space"
mkdir -p "$FIXTURE/.github/skills/scripts" "$FIXTURE/.git" "$FIXTURE/bin" \
         "$FIXTURE/build" "$FIXTURE/test/tests" "$FIXTURE/doc" \
         "$FIXTURE/bench/gambit-benchmarks"
cp "$SCRIPTS/sagittarius.sh" "$SCRIPTS/scheme-tools.scm" \
   "$SCRIPTS/compare-benchmarks.awk" "$FIXTURE/.github/skills/scripts/"
chmod +x "$FIXTURE/.github/skills/scripts/sagittarius.sh"
printf 'fixture\n' > "$FIXTURE/CMakeLists.txt"
printf 'CMAKE_HOME_DIRECTORY:INTERNAL=%s\n' "$FIXTURE" > "$FIXTURE/CMakeCache.txt"
: > "$FIXTURE/CTestTestfile.cmake"
: > "$FIXTURE/test/runner.scm"
: > "$FIXTURE/bench/benchmark-runner.scm"
for name in pass banner fail xpass empty crash; do : > "$FIXTURE/test/tests/$name.scm"; done
for name in fib broken overflow; do : > "$FIXTURE/bench/gambit-benchmarks/$name.scm"; done

cat > "$FIXTURE/build/sagittarius" <<'SASH'
#!/usr/bin/env bash
set -eu
printf '%s\n' "$@" >> "$FAKE_CALLS"
case "$TMPDIR" in "$FAKE_ROOT"/.tmp/*) ;; *) exit 90 ;; esac
[[ "$TMPDIR" == "$SAGITTARIUS_CACHE_DIR" ]] || exit 91
case "$PWD" in "$FAKE_ROOT"|"$FAKE_ROOT"/*) ;; *) exit 92 ;; esac
no_main=0
while [[ "${1:-}" == -* ]]; do
    if [[ "$1" == -n || "$1" == --no-main ]]; then no_main=1; fi
    shift
done
script=$1
shift
case "$script" in
    */test/runner.scm)
        case "$1" in
            */pass.scm) printf '%%%%%%%% Starting test fixture\n# of expected passes      1\n' ;;
            */banner.scm) printf '%%%%%%%% Starting test FAIL reporting\n# of expected passes      1\n' ;;
            */fail.scm) printf '%s:12:FAIL fixture\n# of expected passes      1\n' "$1" ;;
            */xpass.scm) printf 'XPASS fixture\n# of expected passes      1\n' ;;
            */empty.scm) printf '%%%%%%%% Starting test fixture\n' ;;
            */crash.scm) printf 'native failure\n'; exit 7 ;;
        esac ;;
    */benchmark-runner.scm)
        case "$1" in
            broken) printf 'no timing\n' ;;
            overflow) printf 'RESULT:overflow:1e999\n' ;;
            *) printf 'RESULT:%s:10.0\n' "$1" ;;
        esac
        [[ "$no_main" == 1 ]] || exit 201 ;;
    */gendoc)
        output=${1#--output=}
        printf '<html>fixture</html>\n' > "$output" ;;
    *) printf 'fixture script\n' ;;
esac
SASH
cat > "$FIXTURE/bin/ctest" <<'CTEST'
#!/usr/bin/env bash
set -eu
printf 'ctest %s\n' "$*" >> "$FAKE_CALLS"
if [[ "$*" == *"-N"* ]]; then
    if [[ "$*" == *"nomatch"* ]]; then printf 'Total Tests: 0\n'; else printf 'Total Tests: 1\n'; fi
else
    printf '100%% tests passed, 0 tests failed out of 1\nTotal Test time (real) = 0.01 sec\n'
fi
CTEST
cat > "$FIXTURE/bin/cmake" <<'CMAKE'
#!/usr/bin/env bash
set -eu
printf 'cmake <%s>\n' "$@" >> "$FAKE_CALLS"
if [[ "${1:-}" != --build ]]; then
    printf 'CMAKE_HOME_DIRECTORY:INTERNAL=%s\n' "$1" > CMakeCache.txt
fi
printf 'fixture build\n'
CMAKE
cat > "$FIXTURE/dist.sh" <<'DIST'
#!/bin/sh
$SASH generated.scm
false
echo 'must not mask failure'
DIST
chmod +x "$FIXTURE/build/sagittarius" "$FIXTURE/bin/ctest" "$FIXTURE/bin/cmake"
export FAKE_ROOT="$FIXTURE" FAKE_CALLS="$TEST_WORK/calls"
export PATH="$FIXTURE/bin:$PATH"
DRIVER="$FIXTURE/.github/skills/scripts/sagittarius.sh"
BASH_DRIVER=("$BASH" "$DRIVER")
RESULT="$TEST_WORK/result"
PASSES=0

expect_status() {
    local expected=$1 name=$2 actual
    shift 2
    if "$@" > "$RESULT" 2>&1; then actual=0; else actual=$?; fi
    if [[ "$actual" != "$expected" ]]; then
        printf 'FAIL %s: expected status %s, got %s\n' "$name" "$expected" "$actual" >&2
        cat "$RESULT" >&2
        exit 1
    fi
    PASSES=$((PASSES + 1))
    printf 'PASS %s\n' "$name"
}

expect_failure() {
    local name=$1
    shift
    if "$@" > "$RESULT" 2>&1; then
        printf 'FAIL %s: expected a nonzero status\n' "$name" >&2
        cat "$RESULT" >&2
        exit 1
    fi
    PASSES=$((PASSES + 1))
    printf 'PASS %s\n' "$name"
}

check_output() {
    local pattern=$1 file=${2:-$RESULT}
    if ! grep -Fq -- "$pattern" "$file"; then
        printf 'FAIL missing output: %s\n' "$pattern" >&2
        cat "$file" >&2
        exit 1
    fi
}

expect_status 0 "detect root configuration with spaces" "${BASH_DRIVER[@]}" status
check_output "CMake directory: $FIXTURE"
expect_status 0 "explicit workspace root" "${BASH_DRIVER[@]}" --build-dir "$FIXTURE" status
expect_status 0 "incremental target build" "${BASH_DRIVER[@]}" build sash
check_output "cmake <$FIXTURE>" "$FAKE_CALLS"
expect_status 0 "unfiltered CTest listing" "${BASH_DRIVER[@]}" list-tests
check_output 'Total Tests: 1'
expect_status 0 "explicit full-suite selection" "${BASH_DRIVER[@]}" test-all
check_output '100% tests passed'
expect_status 0 "complete SRFI-64 summary" "${BASH_DRIVER[@]}" test-file test/tests/pass.scm
expect_status 0 "FAIL in a suite name is not a failure record" "${BASH_DRIVER[@]}" test-file test/tests/banner.scm
expect_status 2 "zero-exit FAIL is not success" "${BASH_DRIVER[@]}" test-file test/tests/fail.scm
expect_status 2 "zero-exit XPASS is not success" "${BASH_DRIVER[@]}" test-file test/tests/xpass.scm
expect_status 2 "missing summary is not success" "${BASH_DRIVER[@]}" test-file test/tests/empty.scm
expect_status 2 "empty file cannot hide in a batch" "${BASH_DRIVER[@]}" test-file test/tests/pass.scm test/tests/empty.scm
expect_status 7 "preserve interpreter exit status" "${BASH_DRIVER[@]}" test-file test/tests/crash.scm
expect_status 0 "repeat selected tests" "${BASH_DRIVER[@]}" --repeat 2 test fixture
check_output 'CTest run 2/2'
expect_status 2 "reject zero CTest matches" "${BASH_DRIVER[@]}" test nomatch
expect_status 2 "reject empty CTest selector" "${BASH_DRIVER[@]}" test ""
expect_status 2 "reject outside path" "${BASH_DRIVER[@]}" run "$ROOT/CMakeLists.txt"
expect_status 2 "reject parent traversal" "${BASH_DRIVER[@]}" run ../CMakeLists.txt
ln -s "$ROOT" "$FIXTURE/escape"
expect_status 2 "reject symlink escape" "${BASH_DRIVER[@]}" --build-dir escape status
expect_status 2 "reject zero timeout" "${BASH_DRIVER[@]}" --timeout 0 status
expect_status 2 "reject integer overflow" "${BASH_DRIVER[@]}" --repeat 18446744073709551616 test fixture

mkdir -p "$FIXTURE/out/tree/build"
cp "$FIXTURE/build/sagittarius" "$FIXTURE/out/tree/build/sagittarius"
printf 'CMAKE_HOME_DIRECTORY:INTERNAL=%s\n' "$FIXTURE" > "$FIXTURE/out/tree/CMakeCache.txt"
expect_status 0 "out-of-tree interpreter selection" "${BASH_DRIVER[@]}" --build-dir out/tree run test/tests/pass.scm
check_output "-D$FIXTURE/out/tree/build" "$FAKE_CALLS"
expect_status 1 "generator failure cannot be masked" env "SASH=$FIXTURE/build/sagittarius" "${BASH_DRIVER[@]}" generate stub
check_output 'fixture script'
if grep -Fq 'must not mask failure' "$RESULT"; then exit 1; fi

expect_status 0 "benchmark records all runs" "${BASH_DRIVER[@]}" benchmark --runs 2 bench/results/current.csv fib
grep -Fxq 'fib,10.0,2' "$FIXTURE/bench/results/current.csv"
expect_status 2 "benchmark output is not overwritten" "${BASH_DRIVER[@]}" benchmark bench/results/current.csv fib
expect_status 2 "reject duplicate benchmark names" "${BASH_DRIVER[@]}" benchmark bench/results/duplicate.csv fib fib
expect_status 2 "missing timing fails before publishing CSV" "${BASH_DRIVER[@]}" benchmark bench/results/broken.csv broken
[[ ! -e "$FIXTURE/bench/results/broken.csv" ]]
expect_status 2 "non-finite timing fails before publishing CSV" "${BASH_DRIVER[@]}" benchmark bench/results/overflow.csv overflow
[[ ! -e "$FIXTURE/bench/results/overflow.csv" ]]
printf 'benchmark,time_ms,run\nfib,100,1\nfib,100,2\n' > "$FIXTURE/before.csv"
printf 'benchmark,time_ms,run\nfib,110,1\nfib,110,2\n' > "$FIXTURE/after.csv"
expect_status 0 "regression threshold is inclusive" "${BASH_DRIVER[@]}" compare before.csv after.csv 10
grep -Fxq 'fib,100.000,110.000,0.909,+10.000,2,2' "$RESULT" ||
    { cat "$RESULT" >&2; die "Incorrect comparison output"; }
printf 'benchmark,time_ms,run\nfib,0.7,1\n' > "$FIXTURE/decimal-before.csv"
printf 'benchmark,time_ms,run\nfib,0.77,1\n' > "$FIXTURE/decimal-after.csv"
expect_status 0 "decimal equality does not create a false regression" "${BASH_DRIVER[@]}" compare decimal-before.csv decimal-after.csv 10
printf 'benchmark,time_ms,run\nfib,110.01,1\n' > "$FIXTURE/after.csv"
expect_status 1 "regression above threshold fails" "${BASH_DRIVER[@]}" compare before.csv after.csv 10
printf 'benchmark,time_ms,run\nfib,nan,1\n' > "$FIXTURE/after.csv"
expect_status 2 "reject nonnumeric timing" "${BASH_DRIVER[@]}" compare before.csv after.csv
printf 'benchmark,time_ms,run\nfib,1e999,1\n' > "$FIXTURE/after.csv"
expect_status 2 "reject non-finite timing" "${BASH_DRIVER[@]}" compare before.csv after.csv
printf 'benchmark,time_ms,run\nfib,100,1\nfib,100,1\n' > "$FIXTURE/after.csv"
expect_status 2 "reject duplicate samples" "${BASH_DRIVER[@]}" compare before.csv after.csv
printf 'benchmark,time_ms,run\nfib,100,2\n' > "$FIXTURE/after.csv"
expect_status 2 "reject missing run IDs" "${BASH_DRIVER[@]}" compare before.csv after.csv
printf 'benchmark,time_ms,run\ntak,100,1\n' > "$FIXTURE/after.csv"
expect_status 2 "reject unmatched benchmark sets" "${BASH_DRIVER[@]}" compare before.csv after.csv
printf 'benchmark,time_ms,run\n' > "$FIXTURE/after.csv"
expect_status 2 "reject empty benchmark data" "${BASH_DRIVER[@]}" compare before.csv after.csv

expect_status 124 "portable timeout stops descendants" "$BASH" -c '
    source "$1"
    WORK=$2
    export WORK
    command() {
        if [[ "${1:-}" == -v && ("${2:-}" == gtimeout || "${2:-}" == timeout) ]]; then
            return 1
        fi
        builtin command "$@"
    }
    run_timed 1 "$BASH" -c '\''sleep 30 & echo $! > "$WORK/descendant.pid"; wait'\''
' _ "$SCRIPTS/sagittarius.sh" "$TEST_WORK"
read -r descendant < "$TEST_WORK/descendant.pid"
state=$(ps -p "$descendant" -o stat= || true)
[[ -z "$state" || "$state" == *Z* ]] || die "Timeout left a running descendant: $descendant"

if [[ "${1:-}" == --real ]]; then
    REAL_DRIVER=("$BASH" "$SCRIPTS/sagittarius.sh")
    cat > "$TEST_WORK/r6rs.scm" <<'R6RS'
(library (fixture reader)
  (export (rename (internal external)))
  (import (only (rnrs) error))
  (error 'fixture "The library body must not be evaluated"))
R6RS
    cat > "$TEST_WORK/r7rs.scm" <<'R7RS'
(define-library (fixture conditional)
  (export value)
  (import (scheme base))
  (cond-expand (else (include-library-declarations "missing.scm")))
  (begin (error "The library body must not be evaluated")))
R7RS
    printf '(import (rnrs))\n' > "$TEST_WORK/program.scm"
    expect_status 0 "reader preserves R6RS modifiers without evaluation" "${REAL_DRIVER[@]}" inspect "$TEST_WORK/r6rs.scm"
    check_output 'export: (rename (internal external))'
    check_output 'import: (only (rnrs) error)'
    expect_status 0 "reader reports unexpanded R7RS declarations" "${REAL_DRIVER[@]}" inspect "$TEST_WORK/r7rs.scm"
    check_output 'form: define-library'
    check_output 'unexpanded declaration: (cond-expand'
    expect_status 0 "compiler diagnostic selects instruction output" "${REAL_DRIVER[@]}" compiler-pass 5 '((lambda (x) (+ x 1)) 5)'
    check_output 'RET'
    expect_failure "reject files without a library form" "${REAL_DRIVER[@]}" inspect "$TEST_WORK/program.scm"
    check_output 'No library or define-library form'
    expect_failure "reject multiple compiler expressions" "${REAL_DRIVER[@]}" compiler-pass 5 '1 2'
    check_output 'Expected exactly one Scheme expression'
fi
printf '%s workflow checks passed\n' "$PASSES"
