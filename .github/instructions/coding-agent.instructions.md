---
applyTo: "**"
---

General Coding Agent Instructions
=================================

This document provides rules and best practices for coding agents working 
in this repository.

Definitions
-----------

**Workspace root**: The directory containing this repository's `.git` folder 
and `CMakeLists.txt`. All relative paths in this document are relative to 
the workspace root.

Workspace Restrictions
----------------------

### Use Only the Current Workspace

All file operations **MUST** be performed within the current workspace.

**NEVER** use these directories for any operation:
- `/tmp`
- `/var/tmp`
- `~/.cache`
- Any directory outside the workspace root

If you need temporary files or directories, create them within the workspace:

```
# Good - within workspace
.tmp/
build/tmp/
test/tmp/

# Bad - outside workspace
/tmp/my-test-file
/var/tmp/output
```

When creating temporary content:

1. Create a `.tmp/` directory in the workspace root if needed:
   ```shell
   mkdir -p .tmp
   # Verify it's not a symlink pointing outside workspace
   if [ -L .tmp ]; then
       echo "ERROR: .tmp is a symlink, refusing to use"
       exit 1
   fi
   ```

2. Ensure `.tmp/` is in `.gitignore` (this entry should be permanent):
   ```shell
   if ! grep -qxF '.tmp/' .gitignore 2>/dev/null; then
       echo '.tmp/' >> .gitignore
   fi
   ```

3. Clean up temporary files after use or on script exit:
   ```shell
   rm -rf .tmp/*
   ```

### Rationale

External directories like `/tmp` may require additional permission checks 
that prevent fully automated workflows. Keeping all operations within the 
workspace ensures consistent behavior across different environments and 
permission models.

### Path Verification

Before operating on any path, verify it is within the workspace:

```shell
# Resolve and verify path is within workspace
verify_workspace_path() {
    local path=$1
    local workspace
    workspace=$(git rev-parse --show-toplevel 2>/dev/null || pwd)
    local resolved
    resolved=$(cd "$path" 2>/dev/null && pwd) || {
        echo "ERROR: Cannot resolve path: $path"
        return 1
    }
    case "$resolved" in
        "$workspace"/*|"$workspace") return 0 ;;
        *) echo "ERROR: Path outside workspace: $path"; return 1 ;;
    esac
}
```

Process Execution
-----------------

### Timeout Monitoring

When executing shell commands, you **MUST** implement timeout monitoring 
to avoid infinite waiting.

**Decision flow**:
1. Check for `gtimeout` first (macOS with GNU coreutils)
2. Fall back to `timeout` (Linux/GNU systems)
3. Fall back to portable shell implementation (bash required)

#### Using the `timeout` Command (Preferred)

Most Linux systems have the `timeout` command available:

```shell
# Set a 60-second timeout
timeout 60 make

# Set a 5-minute timeout for longer operations
timeout 300 ctest --output-on-failure
```

#### macOS Compatibility

On macOS, `timeout` is **not available by default**. Use `gtimeout` from 
GNU coreutils if installed:

```shell
# Prefer gtimeout (from GNU coreutils) on macOS
if command -v gtimeout >/dev/null 2>&1; then
    gtimeout 60 make
elif command -v timeout >/dev/null 2>&1; then
    timeout 60 make
else
    # Use portable implementation (see below)
    echo "Warning: No timeout command available, using portable fallback"
fi
```

To install GNU timeout on macOS:
```shell
brew install coreutils  # Provides gtimeout
```

#### Checking for `timeout` Availability

Universal check pattern:

```shell
# Function to run with timeout on any platform
run_timed() {
    local secs=$1
    shift
    if command -v gtimeout >/dev/null 2>&1; then
        gtimeout "$secs" "$@"
    elif command -v timeout >/dev/null 2>&1; then
        timeout "$secs" "$@"
    else
        # Portable fallback
        "$@" &
        local pid=$!
        (
            sleep "$secs"
            if kill -0 $pid 2>/dev/null; then
                kill -TERM $pid 2>/dev/null
                sleep 2
                kill -KILL $pid 2>/dev/null
            fi
        ) &
        local monitor=$!
        wait $pid
        local code=$?
        kill $monitor 2>/dev/null
        wait $monitor 2>/dev/null
        return $code
    fi
}

# Usage
run_timed 60 make build
```

### Recommended Timeouts

These are baseline values; adjust based on system performance.

| Operation | Recommended Timeout | Notes |
|-----------|-------------------|-------|
| Simple commands (ls, cat, grep) | 5 seconds | If exceeds 5s, likely a hung process |
| Build commands (make, cmake) | 5 minutes (300s) | First build may take longer |
| Full test suite (ctest) | 15 minutes (900s) | Use `-j 4` for parallelism |
| Code generation (dist.sh gen) | 5 minutes (300s) | Requires working sagittarius binary |
| Individual test execution | 2 minutes (120s) | Increase for crypto tests |

### Background Process Handling

When starting background processes (servers, watch mode):
1. Always use appropriate flags to mark them as background processes
2. Record the process ID for later termination:
   ```shell
   ./server &
   pid=$!
   echo $pid > .tmp/server.pid
   ```
3. Set up cleanup handlers when appropriate

### Cleanup After Failures

When a command fails or times out, clean up properly:

```shell
# Pattern for safe execution with cleanup
cleanup() {
    rm -rf .tmp/build-output 2>/dev/null
    # Kill any background processes started by this script
    jobs -p | xargs -r kill 2>/dev/null
}
trap cleanup EXIT

# Your commands here
timeout 300 make || { echo "Build failed"; exit 1; }
```

For timed-out commands:
1. Remove any partial output files created
2. Log the failure with context (command, timeout value, exit code)
3. Ensure no orphaned background processes remain

Command Execution Best Practices
--------------------------------

### Avoid Interactive Commands

Never run commands that require interactive input without providing the 
input through other means:

```shell
# Bad - requires interactive input
./configure

# Good - non-interactive
./configure --prefix=/usr/local </dev/null

# Good - auto-accept prompts
yes | ./install.sh
```

### Capture Output Appropriately

For commands that may produce large output:

```shell
# Limit output to reasonable size
make 2>&1 | head -n 1000

# Or redirect to file for later analysis
make 2>&1 > build.log && tail -n 100 build.log
```

### Error Handling

Always check command exit status and handle errors:

```shell
# Check exit status
if ! make build; then
    echo "Build failed"
    exit 1
fi

# Or use set -e for automatic exit on error
set -e
make build
make test
```

Project-Specific Commands
-------------------------

### Building

Use the `run_timed` function or check for timeout availability:

```shell
# With portable timeout handling
if command -v gtimeout >/dev/null 2>&1; then
    gtimeout 300 cmake . && gtimeout 300 make
elif command -v timeout >/dev/null 2>&1; then
    timeout 300 cmake . && timeout 300 make
else
    echo "Warning: No timeout available"
    cmake . && make
fi
```

### Testing

```shell
# Full test suite with timeout (use gtimeout on macOS)
run_timed 900 ctest --output-on-failure -j 4

# Individual test with timeout
run_timed 120 ./build/sagittarius -Llib -Lsitelib -L'ext/*' -Dbuild \
    test/runner.scm test/tests/some-test.scm
```

To find available tests:
```shell
ls test/tests/*.scm
ctest -N  # List all test names and numbers
```

### Code Generation

```shell
# Run code generators with timeout
run_timed 300 ./dist.sh gen
```

Summary Checklist
-----------------

Before executing any shell command:

- [ ] Verify all file paths are within the workspace
- [ ] Check for `timeout`/`gtimeout` availability (especially on macOS)
- [ ] Add timeout to long-running commands
- [ ] Avoid interactive prompts
- [ ] Handle potential errors appropriately
- [ ] Consider output size and truncation if needed
- [ ] Plan for cleanup on failure or timeout
- [ ] For background processes: record PID and plan termination
