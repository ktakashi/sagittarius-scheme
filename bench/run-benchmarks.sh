#!/bin/bash
#
# Sagittarius Scheme Benchmark Runner
# Runs benchmarks multiple times and outputs results in CSV format
#

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
BUILD_DIR="${PROJECT_ROOT}/build"
SAGITTARIUS="${BUILD_DIR}/sagittarius"
RESULTS_DIR="${SCRIPT_DIR}/results"
BASELINE_DIR="${SCRIPT_DIR}/baseline"
BENCH_RUNNER="${SCRIPT_DIR}/benchmark-runner.scm"

# Sagittarius load options - must run from project root
SASH_OPTS="-Llib -Lsitelib -Lext/threads -Lext/time -Dbuild"

# Default iterations
WARMUP_RUNS=1
MEASURE_RUNS=3

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --warmup)
            WARMUP_RUNS="$2"
            shift 2
            ;;
        --runs)
            MEASURE_RUNS="$2"
            shift 2
            ;;
        --output)
            OUTPUT_FILE="$2"
            shift 2
            ;;
        --compare)
            COMPARE_BASELINE="$2"
            shift 2
            ;;
        --baseline)
            SAVE_BASELINE=1
            shift
            ;;
        --quick)
            # Quick mode: fewer benchmarks, fewer runs
            WARMUP_RUNS=0
            MEASURE_RUNS=1
            QUICK_MODE=1
            shift
            ;;
        --help)
            echo "Usage: $0 [options]"
            echo ""
            echo "Options:"
            echo "  --warmup N      Number of warmup runs (default: 1)"
            echo "  --runs N        Number of measurement runs (default: 3)"
            echo "  --output FILE   Output CSV file (default: results/TIMESTAMP.csv)"
            echo "  --compare FILE  Compare against baseline CSV file"
            echo "  --baseline      Save results as new baseline"
            echo "  --quick         Quick mode: 1 run, fewer benchmarks"
            echo "  --help          Show this help"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Generate output filename if not specified
if [ -z "$OUTPUT_FILE" ]; then
    TIMESTAMP=$(date +%Y%m%d_%H%M%S)
    OUTPUT_FILE="${RESULTS_DIR}/benchmark_${TIMESTAMP}.csv"
fi

# Ensure output directory exists
mkdir -p "$(dirname "$OUTPUT_FILE")"
mkdir -p "$BASELINE_DIR"

# Check if sagittarius binary exists
if [ ! -x "$SAGITTARIUS" ]; then
    echo "Error: Sagittarius binary not found at $SAGITTARIUS"
    echo "Please build the project first with: cmake . && make"
    exit 1
fi

# Check if benchmark runner exists
if [ ! -f "$BENCH_RUNNER" ]; then
    echo "Error: Benchmark runner not found at $BENCH_RUNNER"
    exit 1
fi

echo "Sagittarius Benchmark Suite"
echo "==========================="
echo "Binary: $SAGITTARIUS"
echo "Warmup runs: $WARMUP_RUNS"
echo "Measurement runs: $MEASURE_RUNS"
echo "Output: $OUTPUT_FILE"
echo ""

# List of benchmarks to run
if [ -n "$QUICK_MODE" ]; then
    BENCHMARKS=(fib tak sum boyer browse)
else
    BENCHMARKS=(
        boyer browse cpstak ctak dderiv deriv destruc diviter divrec puzzle takl triangl
        fft fib fibc fibfp mbrot pnpoly sum sumfp tak
        ack conform earley graphs mazefun nqueens paraffins peval scheme compiler
    )
fi

# Write CSV header
echo "benchmark,time_ms,run" > "$OUTPUT_FILE"

# Track failures
FAILED_BENCHMARKS=()

run_bench() {
    local bench="$1"
    cd "$PROJECT_ROOT"
    result=$("$SAGITTARIUS" $SASH_OPTS "$BENCH_RUNNER" "$bench" 2>&1 | grep "^RESULT:" | cut -d: -f3)
    echo "$result"
}

# Run each benchmark
for bench in "${BENCHMARKS[@]}"; do
    printf "Running %-15s " "$bench..."
    
    # Warmup runs (discard results)
    for ((i=1; i<=WARMUP_RUNS; i++)); do
        run_bench "$bench" > /dev/null 2>&1 || true
    done
    
    # Measurement runs
    total_time=0
    success_count=0
    
    for ((i=1; i<=MEASURE_RUNS; i++)); do
        result=$(run_bench "$bench")
        if [ -n "$result" ]; then
            total_time=$(echo "$total_time + $result" | bc -l 2>/dev/null || echo "0")
            echo "$bench,$result,$i" >> "$OUTPUT_FILE"
            ((success_count++)) || true
        fi
    done
    
    if [ "$success_count" -eq 0 ]; then
        echo "FAILED"
        FAILED_BENCHMARKS+=("$bench")
    else
        avg_time=$(echo "scale=2; $total_time / $success_count" | bc -l 2>/dev/null || echo "N/A")
        echo "${avg_time}ms (avg of $success_count runs)"
    fi
done

echo ""
echo "Results saved to: $OUTPUT_FILE"

# Save as baseline if requested
if [ -n "$SAVE_BASELINE" ]; then
    BASELINE_FILE="${BASELINE_DIR}/baseline.csv"
    cp "$OUTPUT_FILE" "$BASELINE_FILE"
    echo "Saved as baseline: $BASELINE_FILE"
fi

# Generate summary
echo ""
echo "Summary (averages):"
echo "==================="
printf "%-15s %12s\n" "Benchmark" "Time(ms)"
echo "----------------------------"

# Use awk to calculate averages
awk -F, 'NR>1 {
    sum[$1] += $2
    count[$1]++
}
END {
    for (b in sum) {
        printf "%-15s %12.2f\n", b, sum[b]/count[b]
    }
}' "$OUTPUT_FILE" | sort

# Report failed benchmarks
if [ ${#FAILED_BENCHMARKS[@]} -gt 0 ]; then
    echo ""
    echo "Failed benchmarks: ${FAILED_BENCHMARKS[*]}"
fi

# Comparison with baseline if specified
if [ -n "$COMPARE_BASELINE" ] && [ -f "$COMPARE_BASELINE" ]; then
    echo ""
    echo "Comparison with baseline: $COMPARE_BASELINE"
    echo "================================================"
    printf "%-15s %12s %12s %10s\n" "Benchmark" "Before(ms)" "After(ms)" "Speedup"
    echo "---------------------------------------------------"
    
    # Create temp files for processing
    BASELINE_AVG=$(mktemp)
    CURRENT_AVG=$(mktemp)
    
    # Calculate baseline averages
    awk -F, 'NR>1 {sum[$1]+=$2; count[$1]++} END {for(b in sum) print b","sum[b]/count[b]}' "$COMPARE_BASELINE" > "$BASELINE_AVG"
    
    # Calculate current averages
    awk -F, 'NR>1 {sum[$1]+=$2; count[$1]++} END {for(b in sum) print b","sum[b]/count[b]}' "$OUTPUT_FILE" > "$CURRENT_AVG"
    
    # Join and compare
    while IFS=, read -r bench curr_time; do
        base_time=$(grep "^$bench," "$BASELINE_AVG" | cut -d, -f2)
        if [ -n "$base_time" ] && [ -n "$curr_time" ]; then
            speedup=$(echo "scale=2; $base_time / $curr_time" | bc -l 2>/dev/null || echo "N/A")
            printf "%-15s %12.2f %12.2f %9.2fx\n" "$bench" "$base_time" "$curr_time" "$speedup"
        fi
    done < "$CURRENT_AVG" | sort
    
    rm -f "$BASELINE_AVG" "$CURRENT_AVG"
fi

echo ""
echo "Done!"
