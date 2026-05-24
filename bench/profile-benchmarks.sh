#!/bin/bash
#
# Sagittarius VM Instruction Profiler
# Runs benchmarks with PROF_INSN enabled and collects instruction counts
#

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
BUILD_DIR="${PROJECT_ROOT}/build"
SAGITTARIUS="${BUILD_DIR}/sagittarius"
RESULTS_DIR="${SCRIPT_DIR}/profile-results"
BENCH_RUNNER="${SCRIPT_DIR}/benchmark-runner.scm"

# Check if build has PROF_INSN enabled
check_prof_insn() {
    if ! grep -q "PROF_INSN" "${BUILD_DIR}/sagittarius/config.h" 2>/dev/null; then
        echo "Error: Build does not have PROF_INSN enabled."
        echo "Rebuild with: cmake -DPROF_INSN=ON . && make"
        exit 1
    fi
}

# Create results directory
mkdir -p "$RESULTS_DIR"

# Parse arguments
BENCHMARKS=""
while [[ $# -gt 0 ]]; do
    case $1 in
        --benchmark)
            BENCHMARKS="$2"
            shift 2
            ;;
        --all)
            BENCHMARKS="fib tak sum boyer browse cpstak ctak dderiv deriv destruc"
            shift
            ;;
        --quick)
            BENCHMARKS="fib tak sum"
            shift
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--benchmark NAME] [--all] [--quick]"
            exit 1
            ;;
    esac
done

# Default to quick benchmarks
if [[ -z "$BENCHMARKS" ]]; then
    BENCHMARKS="fib tak sum"
fi

echo "VM Instruction Profiler"
echo "======================="
echo ""

# Check build
check_prof_insn

TIMESTAMP=$(date +%Y%m%d_%H%M%S)
OUTPUT_FILE="${RESULTS_DIR}/profile_${TIMESTAMP}.txt"

echo "Running benchmarks: $BENCHMARKS"
echo "Output: $OUTPUT_FILE"
echo ""

cd "$PROJECT_ROOT"

for bench in $BENCHMARKS; do
    echo "Profiling: $bench"
    echo "" >> "$OUTPUT_FILE"
    echo "=== $bench ===" >> "$OUTPUT_FILE"
    
    # Run benchmark and capture stderr (where instruction counts go)
    "$SAGITTARIUS" -Llib -Lsitelib -Lext/threads -Lext/time -Dbuild \
        "$BENCH_RUNNER" "$bench" 2>> "$OUTPUT_FILE" > /dev/null
    
    echo "  Done"
done

echo ""
echo "Results saved to: $OUTPUT_FILE"
echo ""

# Parse and summarize results
echo "Summary:"
echo "--------"
grep -E "^INSN:" "$OUTPUT_FILE" | sort -t'(' -k2 -rn | head -20

echo ""
echo "Top 20 instructions shown. See full results in $OUTPUT_FILE"
