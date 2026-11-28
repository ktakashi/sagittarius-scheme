BEGIN {
    FS = ","
    if (limit != "" && (limit !~ /^[0-9]+([.][0-9]+)?$/ ||
                       !finite(limit + 0)))
        fail("Regression percentage must be a nonnegative number")
}

function fail(message) {
    print "ERROR: " message
    failed = 1
    exit 2
}

function finite(value) {
    # Formatting classifies infinity/NaN even on awks with unusual NaN comparisons.
    return sprintf("%.17g", value) ~ /^-?[0-9]+([.][0-9]+)?([eE][+-]?[0-9]+)?$/
}

{
    sub(/\r$/, "")
}

FNR == 1 {
    file++
    if ($0 != "benchmark,time_ms,run")
        fail("Expected benchmark,time_ms,run header in " FILENAME)
    next
}

{
    if (NF != 3 || $1 !~ /^[a-zA-Z0-9_-]+$/ ||
        $2 !~ /^[0-9]+([.][0-9]+)?([eE][+-]?[0-9]+)?$/ ||
        $2 + 0 <= 0 || !finite($2 + 0) ||
        $3 !~ /^[1-9][0-9]*$/)
        fail("Invalid sample in " FILENAME ":" FNR)
    key = file SUBSEP $1
    sample = key SUBSEP $3
    if (seen[sample]++)
        fail("Duplicate run " $3 " for " $1 " in " FILENAME)
    if (!count[key] && file == 1)
        names[++n] = $1
    sum[key] += $2
    if (!finite(sum[key]))
        fail("Non-finite total for " $1)
    count[key]++
    if ($3 + 0 > max_run[key])
        max_run[key] = $3 + 0
    rows[file]++
}

END {
    if (failed) exit 2
    if (file != 2 || !rows[1] || !rows[2])
        fail("Both CSV files must contain samples")
    for (key in count) {
        split(key, parts, SUBSEP)
        other = (parts[1] == 1 ? 2 : 1) SUBSEP parts[2]
        if (!count[other])
            fail("Benchmark sets differ: " parts[2])
        if (count[key] != max_run[key])
            fail("Missing run number for " parts[2])
    }
    print "benchmark,before_ms,after_ms,speedup,change_percent,before_runs,after_runs"
    for (i = 1; i <= n; i++) {
        name = names[i]
        before_key = 1 SUBSEP name
        after_key = 2 SUBSEP name
        before = sum[before_key] / count[before_key]
        after = sum[after_key] / count[after_key]
        change = ((after - before) / before) * 100
        speedup = before / after
        if (!finite(change) || !finite(speedup) || speedup <= 0)
            fail("Comparison exceeds numeric range: " name)
        printf "%s,%.3f,%.3f,%.3f,%+.3f,%d,%d\n",
               name, before, after, speedup, change,
               count[before_key], count[after_key]
        scale = limit + 0 > 1 ? limit + 0 : 1
        if (change > scale) scale = change
        # Ignore floating roundoff at an exact decimal threshold boundary.
        if (limit != "" && change > limit && change - limit > scale * 1e-12)
            regressions++
    }
    if (regressions) {
        print "ERROR: " regressions " benchmark(s) exceeded " limit "% regression"
        exit 1
    }
}
