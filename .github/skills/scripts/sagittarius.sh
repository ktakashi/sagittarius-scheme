#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd -P)
SCRIPTS="$ROOT/.github/skills/scripts"

die() {
    printf 'ERROR: %s\n' "$*" >&2
    exit 2
}

workspace_path() {
    local candidate relative component resolved
    local -a components
    [[ -n "$1" ]] || die "Empty workspace path"
    case "$1" in
        "$ROOT"|"$ROOT"/*) candidate=$1 ;;
        /*) die "Path outside workspace: $1" ;;
        *) candidate="$ROOT/$1" ;;
    esac
    [[ "$candidate" != *$'\n'* && "$candidate" != *$'\r'* ]] ||
        die "Newlines are not supported in paths"
    relative=${candidate#"$ROOT"}
    IFS=/ read -r -a components <<< "$relative"
    resolved=$ROOT
    # Bash 3.2 treats empty arrays as unset under nounset.
    for component in ${components[@]+"${components[@]}"}; do
        case "$component" in
            ""|.) continue ;;
            ..) die "Parent traversal is not supported: $1" ;;
        esac
        resolved="$resolved/$component"
        [[ ! -L "$resolved" ]] || die "Symlink path is not supported: $resolved"
    done
    printf '%s\n' "$resolved"
}

positive_integer() {
    [[ "$1" =~ ^[1-9][0-9]{0,8}$ ]]
}

run_timed() {
    local seconds=$1
    shift
    if command -v gtimeout >/dev/null 2>&1; then
        gtimeout -k 2 "$seconds" "$@" </dev/null
    elif command -v timeout >/dev/null 2>&1; then
        timeout -k 2 "$seconds" "$@" </dev/null
    else
        (
            local status
            TIMED_MARKER="$WORK/timed-out"
            rm -f "$TIMED_MARKER"
            # Separate job groups let the watchdog stop descendants as well.
            set -m
            (set +m; exec "$@" </dev/null) &
            TIMED_CHILD=$!
            (
                set +m
                sleep "$seconds"
                : > "$TIMED_MARKER"
                kill -TERM -- "-$TIMED_CHILD" 2>/dev/null || true
                sleep 2
                kill -KILL -- "-$TIMED_CHILD" 2>/dev/null || true
            ) &
            TIMED_MONITOR=$!
            cleanup_timed() {
                kill -KILL -- "-$TIMED_CHILD" "-$TIMED_MONITOR" 2>/dev/null || true
                wait "$TIMED_MONITOR" 2>/dev/null || true
                rm -f "$TIMED_MARKER"
            }
            trap cleanup_timed EXIT
            trap 'exit 130' INT
            trap 'exit 143' TERM
            if wait "$TIMED_CHILD"; then status=0; else status=$?; fi
            if [[ -f "$TIMED_MARKER" ]]; then status=124; fi
            exit "$status"
        )
    fi
}

capture() {
    local status
    if run_timed "$DEADLINE" "$@" > "$LOG" 2>&1; then
        return 0
    else
        status=$?
        printf 'ERROR: command exited %s' "$status" >&2
        if [[ "$status" == 124 ]]; then
            printf ' (timeout after %ss)' "$DEADLINE" >&2
        fi
        printf ': ' >&2
        printf '%q ' "$@" >&2
        printf '\n' >&2
        cat "$LOG" >&2
        return "$status"
    fi
}

require_file() {
    [[ -f "$1" ]] || die "File not found: $1"
}

check_config() {
    local cache
    cache=$(workspace_path "$BUILD_DIR/CMakeCache.txt")
    require_file "$cache"
    grep -Fxq "CMAKE_HOME_DIRECTORY:INTERNAL=$ROOT" "$cache" ||
        die "CMake cache belongs to another source tree: $BUILD_DIR"
}

find_binary() {
    local candidate
    BINARY=""
    for candidate in "$BUILD_DIR/build/sagittarius" "$BUILD_DIR/sagittarius"; do
        candidate=$(workspace_path "$candidate")
        if [[ -x "$candidate" && -f "$candidate" ]]; then
            BINARY=$candidate
            DYNAMIC_DIR=${candidate%/*}
            return 0
        fi
    done
    return 1
}

require_binary() {
    find_binary || die "Interpreter missing for $BUILD_DIR; configure and build first"
    SASH_OPTIONS=("$BINARY" "-L$ROOT/lib" "-L$ROOT/sitelib"
                  "-L$ROOT/ext/*" "-L$ROOT/test/lib"
                  "-L$BUILD_DIR/test/data" "-D$DYNAMIC_DIR")
}

ctest_capture() {
    (cd "$BUILD_DIR"; capture ctest "$@")
}

check_tests() {
    local test_file
    test_file=$(workspace_path "$BUILD_DIR/CTestTestfile.cmake")
    require_file "$test_file"
    ctest_capture -N "$@"
    if ! grep -Eq '^Total Tests: [1-9][0-9]*[[:space:]]*$' "$LOG"; then
        cat "$LOG" >&2
        die "No CTest tests selected"
    fi
}

test_file_summary() {
    if ! grep -Eq '^# of (expected passes|expected failures|skipped tests)[[:space:]]+[0-9]+' "$LOG" ||
       grep -Eq '(^|:)(FAIL|XPASS)([[:space:]]|$)|^# of unexpected (failures|successes)[[:space:]]+[1-9]' "$LOG"; then
        cat "$LOG" >&2
        die "Failed or incomplete SRFI-64 test run"
    fi
    grep -E '^%%%%|^# of ' "$LOG"
}

benchmark_result() {
    local result
    if result=$(LC_ALL=C awk -F: -v name="$1" '
        function finite(value) {
            return sprintf("%.17g", value) ~ /^[0-9]+([.][0-9]+)?([eE][+-]?[0-9]+)?$/
        }
        /^RESULT:/ {
            count++
            if (NF != 3 || $2 != name ||
                $3 !~ /^[0-9]+([.][0-9]+)?([eE][+-]?[0-9]+)?$/ ||
                $3 + 0 <= 0 || !finite($3 + 0)) bad = 1
            value = $3
        }
        END {
            if (count != 1 || bad) exit 1
            print value
        }' "$LOG"); then
        printf '%s\n' "$result"
    else
        cat "$LOG" >&2
        die "Expected one positive RESULT:$1:<milliseconds> record"
    fi
}

benchmark() {
    local runs=3 warmup=1 output name file run result names=" "
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --runs|--warmup)
                [[ $# -ge 2 ]] || die "Missing value for $1"
                if [[ "$1" == --runs ]]; then runs=$2; else warmup=$2; fi
                shift 2 ;;
            --*) die "Unknown benchmark option: $1" ;;
            *) break ;;
        esac
    done
    positive_integer "$runs" || die "Benchmark runs must be a positive integer of at most 9 digits"
    [[ "$warmup" =~ ^(0|[1-9][0-9]{0,8})$ ]] ||
        die "Warmup must be a nonnegative integer of at most 9 digits"
    [[ $# -ge 2 ]] || die "benchmark requires OUTPUT and at least one NAME"
    output=$(workspace_path "$1")
    shift
    [[ ! -e "$output" ]] || die "Refusing to overwrite benchmark output: $output"
    for name in "$@"; do
        [[ "$name" =~ ^[a-zA-Z0-9_-]+$ ]] || die "Invalid benchmark name: $name"
        [[ "$names" != *" $name "* ]] || die "Duplicate benchmark name: $name"
        names="$names$name "
        file=$(workspace_path "$ROOT/bench/gambit-benchmarks/$name.scm")
        require_file "$file"
    done
    require_file "$ROOT/bench/benchmark-runner.scm"
    printf 'benchmark,time_ms,run\n' > "$WORK/benchmark.csv"
    for name in "$@"; do
        for ((run=1; run<=warmup; run++)); do
            capture "${SASH_OPTIONS[@]}" -n -d "$ROOT/bench/benchmark-runner.scm" "$name"
            benchmark_result "$name" >/dev/null
        done
        for ((run=1; run<=runs; run++)); do
            capture "${SASH_OPTIONS[@]}" -n -d "$ROOT/bench/benchmark-runner.scm" "$name"
            result=$(benchmark_result "$name")
            printf '%s,%s,%s\n' "$name" "$result" "$run" >> "$WORK/benchmark.csv"
        done
    done
    mkdir -p "${output%/*}"
    (set -C; cat "$WORK/benchmark.csv" > "$output")
    printf 'OK benchmark: %s\n' "${output#"$ROOT/"}"
    LC_ALL=C awk -F, '
        NR > 1 {
            if (!count[$1]) names[++n] = $1
            sum[$1] += $2
            count[$1]++
        }
        END {
            for (i = 1; i <= n; i++)
                printf "%s: %.3f ms (%d runs)\n", names[i], sum[names[i]] / count[names[i]], count[names[i]]
        }' "$output"
}

usage() {
    cat <<'USAGE'
Usage: bash .github/skills/scripts/sagittarius.sh [options] COMMAND [arguments]

Options before COMMAND:
  --build-dir DIR  Configured CMake directory (auto: ., build, out/build)
  --jobs N        Build/CTest parallelism (default: 4)
  --timeout N     Operation timeout in seconds
  --repeat N      Repeat selected tests, stopping at the first failure

Commands:
  status                         Show configuration and interpreter paths
  configure [-DNAME=VALUE ...]    Configure without changing an existing layout
  build [TARGET]                 Incremental build; optional CMake target
  generate PHASE                 stub, precomp, srfi, insn, unicode, html, tz, gen
  list-tests [REGEX]              List registered CTest tests
  test REGEX                     Run matching CTest tests; zero matches is an error
  test-all                       Explicitly run the complete CTest suite
  test-file FILE...               Run SRFI-64 files; require complete summaries
  run FILE [ARGS...]              Run a Scheme script with local library paths
  inspect FILE                   Read library exports/imports without loading its body
  compiler-pass N EXPR            Print compiler pass 1..5 for one Scheme expression
  doc FILE...                    Render selected Markdown to temporary HTML
  benchmark [--runs N] [--warmup N] OUTPUT NAME...
  compare BEFORE AFTER [MAX_REGRESSION_PERCENT]

Defaults: build/generate/doc 300s, CTest 900s, scripts/benchmarks 120s.
Paths must stay in this workspace; symlinks and parent traversal are rejected.
Successful builds are terse; failed commands retain their exit code and output.
USAGE
}

main() {
    local action option build_override="" jobs=4 repeat=1 timeout=""
    local file target phase host iteration before after output
    local -a filters files

    # dist.sh expands SASH as a command; a PATH entry avoids splitting paths with spaces.
    if [[ "${1:-}" == host ]]; then
        shift
        host=${SAGITTARIUS_SKILL_HOST:-}
        [[ -n "$host" ]] || die "host is an internal generation command"
        workspace_path "$PWD" >/dev/null
        file=$(workspace_path "${SAGITTARIUS_CACHE_DIR:-}")
        [[ -d "$file" ]] || die "Generation cache directory is missing"
        if [[ "$host" == */* ]]; then host=$(workspace_path "$host"); fi
        if [[ -n "${SAGITTARIUS_SKILL_DYNAMIC_DIR:-}" ]]; then
            file=$(workspace_path "$SAGITTARIUS_SKILL_DYNAMIC_DIR")
            exec "$host" -d "-D$file" "$@"
        else
            exec "$host" -d "$@"
        fi
    fi

    while [[ $# -gt 0 ]]; do
        case "$1" in
            --build-dir|--jobs|--timeout|--repeat)
                [[ $# -ge 2 ]] || die "Missing value for $1"
                option=$1
                case "$option" in
                    --build-dir) build_override=$2 ;;
                    --jobs) jobs=$2 ;;
                    --timeout) timeout=$2 ;;
                    --repeat) repeat=$2 ;;
                esac
                shift 2 ;;
            --help|-h) usage; return ;;
            --*) die "Unknown option: $1" ;;
            *) break ;;
        esac
    done
    [[ $# -gt 0 ]] || { usage; return 2; }
    action=$1
    shift
    positive_integer "$jobs" || die "Jobs must be a positive integer of at most 9 digits"
    positive_integer "$repeat" || die "Repeat must be a positive integer of at most 9 digits"
    [[ -z "$timeout" ]] || positive_integer "$timeout" ||
        die "Timeout must be a positive integer of at most 9 digits"
    [[ "$repeat" == 1 || "$action" == test || "$action" == test-file || "$action" == test-all ]] ||
        die "--repeat is only supported for test commands"

    [[ -f "$ROOT/CMakeLists.txt" && -e "$ROOT/.git" ]] ||
        die "Helper must be used inside the Sagittarius workspace"
    cd "$ROOT"
    if [[ -n "$build_override" ]]; then
        BUILD_DIR=$(workspace_path "$build_override")
    elif [[ -f "$ROOT/CMakeCache.txt" ]]; then
        BUILD_DIR=$ROOT
    elif [[ -f "$ROOT/build/CMakeCache.txt" ]]; then
        BUILD_DIR=$(workspace_path build)
    else
        BUILD_DIR=$(workspace_path out/build)
    fi
    if [[ -e "$BUILD_DIR/CMakeCache.txt" ]]; then check_config; fi

    umask 077
    file=$(workspace_path .tmp)
    mkdir -p "$file"
    WORK=$(mktemp -d "$file/sagittarius-skill.XXXXXXXX")
    LOG="$WORK/output.log"
    trap 'rm -rf "$WORK"' EXIT
    export TMPDIR="$WORK" TMP="$WORK" TEMP="$WORK" SAGITTARIUS_CACHE_DIR="$WORK"
    case "$action" in
        configure|build|generate|doc) DEADLINE=${timeout:-300} ;;
        test|test-all) DEADLINE=${timeout:-900} ;;
        status|list-tests|compare) DEADLINE=${timeout:-15} ;;
        *) DEADLINE=${timeout:-120} ;;
    esac

    case "$action" in
        status)
            [[ $# == 0 ]] || die "status takes no arguments"
            printf 'Workspace: %s\nCMake directory: %s\n' "$ROOT" "$BUILD_DIR"
            if [[ -f "$BUILD_DIR/CMakeCache.txt" ]]; then
                printf 'Configured: yes\n'
            else
                printf 'Configured: no (run configure first)\n'
            fi
            if find_binary; then
                printf 'Interpreter: %s\nDynamic libraries: %s\n' "$BINARY" "$DYNAMIC_DIR"
            else
                printf 'Interpreter: missing (run build after configure)\n'
            fi ;;
        configure)
            for option in "$@"; do
                [[ "$option" == -D*=* ]] || die "configure accepts only -DNAME=VALUE arguments"
            done
            mkdir -p "$BUILD_DIR"
            (cd "$BUILD_DIR"; capture cmake "$ROOT" "$@")
            check_config
            printf 'OK configure: %s\n' "$BUILD_DIR" ;;
        build)
            [[ $# -le 1 ]] || die "build accepts one optional target"
            check_config
            target=${1:-}
            if [[ -n "$target" ]]; then
                [[ "$target" != -* ]] || die "Invalid build target: $target"
                capture cmake --build "$BUILD_DIR" --parallel "$jobs" --target "$target"
            else
                capture cmake --build "$BUILD_DIR" --parallel "$jobs"
            fi
            printf 'OK build: %s%s\n' "$BUILD_DIR" "${target:+ ($target)}" ;;
        generate)
            [[ $# == 1 ]] || die "generate requires one phase"
            phase=$1
            case "$phase" in
                stub|precomp|srfi|insn|unicode|html|tz|gen) ;;
                *) die "Unsupported generation phase: $phase" ;;
            esac
            host=${SASH:-sagittarius}
            SAGITTARIUS_SKILL_DYNAMIC_DIR=""
            if [[ -z "${SASH:-}" ]] && ! command -v sagittarius >/dev/null 2>&1; then
                require_binary
                host=$BINARY
                SAGITTARIUS_SKILL_DYNAMIC_DIR=$DYNAMIC_DIR
            elif [[ "$host" == */* ]]; then
                host=$(workspace_path "$host")
                [[ -x "$host" ]] || die "SASH is not executable: $host"
                SAGITTARIUS_SKILL_DYNAMIC_DIR=${host%/*}
            else
                command -v "$host" >/dev/null 2>&1 || die "SASH command not found: $host"
            fi
            export SAGITTARIUS_SKILL_HOST="$host" SAGITTARIUS_SKILL_DYNAMIC_DIR
            require_file "$ROOT/dist.sh"
            printf 'Generator host: %s\n' "$host"
            (cd "$ROOT"; capture env "PATH=$SCRIPTS:$PATH" 'SASH=sagittarius.sh host' sh -e ./dist.sh "$phase")
            printf 'OK generate: %s\n' "$phase" ;;
        list-tests)
            [[ $# -le 1 ]] || die "list-tests accepts one optional regex"
            check_config
            require_file "$(workspace_path "$BUILD_DIR/CTestTestfile.cmake")"
            filters=()
            if [[ $# == 1 ]]; then filters=(-R "$1"); fi
            ctest_capture -N ${filters[@]+"${filters[@]}"}
            cat "$LOG" ;;
        test|test-all)
            filters=()
            if [[ "$action" == test ]]; then
                [[ $# == 1 && -n "$1" ]] || die "test requires one nonempty CTest regex"
                filters=(-R "$1")
            else
                [[ $# == 0 ]] || die "test-all takes no arguments"
            fi
            check_config
            require_binary
            check_tests ${filters[@]+"${filters[@]}"}
            for ((iteration=1; iteration<=repeat; iteration++)); do
                ctest_capture --output-on-failure -j "$jobs" ${filters[@]+"${filters[@]}"}
                printf 'CTest run %s/%s\n' "$iteration" "$repeat"
                grep -E 'tests passed|tests failed out|Total Test time' "$LOG"
            done ;;
        test-file)
            [[ $# -gt 0 ]] || die "test-file requires at least one file"
            files=()
            for file in "$@"; do
                file=$(workspace_path "$file")
                require_file "$file"
                files+=("$file")
            done
            require_binary
            for ((iteration=1; iteration<=repeat; iteration++)); do
                for file in "${files[@]}"; do
                    capture "${SASH_OPTIONS[@]}" "$ROOT/test/runner.scm" "$file"
                    printf 'Test file: %s (run %s/%s)\n' "${file#"$ROOT/"}" "$iteration" "$repeat"
                    test_file_summary
                done
            done ;;
        run|inspect|compiler-pass|doc|benchmark)
            require_binary
            case "$action" in
                run|inspect)
                    [[ $# -gt 0 ]] || die "$action requires a file"
                    file=$(workspace_path "$1")
                    require_file "$file"
                    shift
                    if [[ "$action" == inspect ]]; then
                        [[ $# == 0 ]] || die "inspect accepts one file"
                        capture "${SASH_OPTIONS[@]}" -d "$SCRIPTS/scheme-tools.scm" inspect "$file"
                    else
                        capture "${SASH_OPTIONS[@]}" -d "$file" "$@"
                    fi
                    cat "$LOG" ;;
                compiler-pass)
                    [[ $# == 2 && "$1" =~ ^[1-5]$ ]] || die "compiler-pass requires N (1..5) and one expression"
                    capture "${SASH_OPTIONS[@]}" -d "$SCRIPTS/scheme-tools.scm" compiler-pass "$@"
                    cat "$LOG" ;;
                doc)
                    [[ $# -gt 0 ]] || die "doc requires at least one Markdown file"
                    iteration=0
                    for file in "$@"; do
                        file=$(workspace_path "$file")
                        [[ "$file" == "$ROOT/doc/"* ]] || die "Document must be under doc/: $file"
                        require_file "$file"
                        iteration=$((iteration + 1))
                        output="$WORK/document-$iteration.html"
                        (cd "$ROOT/doc"; capture "${SASH_OPTIONS[@]}" -d "$ROOT/doc/gendoc" "--output=$output" "$file")
                        [[ -s "$output" ]] || die "Document generator produced no HTML: $file"
                        printf 'OK doc: %s\n' "${file#"$ROOT/"}"
                    done ;;
                benchmark) benchmark "$@" ;;
            esac ;;
        compare)
            [[ $# == 2 || $# == 3 ]] || die "compare requires BEFORE AFTER and an optional regression percentage"
            before=$(workspace_path "$1")
            after=$(workspace_path "$2")
            require_file "$before"
            require_file "$after"
            capture env LC_ALL=C awk -v "limit=${3:-}" -f "$SCRIPTS/compare-benchmarks.awk" "$before" "$after"
            cat "$LOG" ;;
        *) die "Unknown command: $action (use --help)" ;;
    esac
}

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
    main "$@"
fi
