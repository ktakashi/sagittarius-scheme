# Implementation Plan: C11 Modernization

## Overview

Modernize the Sagittarius C codebase to leverage C11 features while
maintaining full backward compatibility with older compilers (C99/C89)
through a unified compatibility layer.

## Detailed Plan

### Phase 1: Update platform.h with Compatibility Macros

**Objective**: Update the public compatibility header to abstract
C11 features with fallbacks for older compilers.

**Steps**:

1. Update `src/sagittarius/platform.h`
   - C standard version detection (`SG_C11`, `SG_C99`, `SG_C89`)
   - Compiler detection macros
   - Feature availability checks

2. Implement `SG_NORETURN` macro
   - C11: `_Noreturn` via `<stdnoreturn.h>`
   - GCC: `__attribute__((noreturn))`
   - MSVC: `__declspec(noreturn)`
   - Fallback: empty

3. Implement `SG_STATIC_ASSERT` macro
   - C11: `_Static_assert(cond, msg)`
   - Fallback: typedef array trick

4. Implement `SG_ALIGNAS` macro
   - C11: `_Alignas(n)` via `<stdalign.h>`
   - GCC: `__attribute__((aligned(n)))`
   - MSVC: `__declspec(align(n))`
   - Fallback: empty

5. Implement `SG_INLINE` macro
   - C99/C11: `inline`
   - GCC: `__inline__`
   - MSVC: `__inline`
   - Fallback: empty

**Files to modify**:
- `src/sagittarius/platform.h` (add compatibility macros)
- `src/sagittarius/private/sagittariusdefs.h` (verify platform.h included)

### Phase 2: Apply `SG_NORETURN`

**Objective**: Replace existing `SG_NO_RETURN` usage with new `SG_NORETURN`
macro and apply to all non-returning functions.

**Steps**:

1. Update `sagittariusdefs.h`
   - Remove old `SG_NO_RETURN` definition
   - Use `SG_NORETURN` from `platform.h`

2. Apply `SG_NORETURN` to functions in `src/sagittarius/private/error.h`:
   - `Sg_Panic`
   - `Sg_Error`
   - Other error functions that never return

3. Apply `SG_NORETURN` to exit functions:
   - `Sg_Exit` (if exists)

4. Verify no warnings on all supported compilers

**Files to modify**:
- `src/sagittarius/private/sagittariusdefs.h`
- `src/sagittarius/private/error.h`
- `src/error.c` (if declarations differ)

### Phase 3: Add Static Assertions

**Objective**: Add compile-time checks for critical assumptions using
`SG_STATIC_ASSERT`.

**Steps**:

1. Add size/alignment checks in `sagittariusdefs.h`:
   ```c
   SG_STATIC_ASSERT(sizeof(SgWord) == sizeof(void*),
                    "SgWord must match pointer size");
   SG_STATIC_ASSERT(sizeof(intptr_t) == sizeof(void*),
                    "intptr_t must match pointer size");
   ```

2. Add tag value assumptions in `sagittariusdefs.h`:
   ```c
   SG_STATIC_ASSERT((SG_INT_MAX & 0x3) == 0,
                    "Fixnum max must preserve tag bits");
   ```

3. Add structure layout checks where critical:
   - `SgPair` structure in `pair.h`
   - `SgClosure` structure in `closure.h`

4. Identify and replace runtime `ASSERT` with `SG_STATIC_ASSERT`
   where condition is compile-time determinable

**Files to modify**:
- `src/sagittarius/private/sagittariusdefs.h`
- `src/sagittarius/private/pair.h`
- Other header files with critical structure definitions

### Phase 4: Alignment Optimization

**Objective**: Apply `SG_ALIGNAS` to performance-critical structures
where alignment improves cache behavior.

**Steps**:

1. Identify alignment-sensitive code:
   - VM stack operations in `vm.c`
   - Call frame structures in `vmcall.c`
   - Bytecode structures

2. Apply `SG_ALIGNAS` conservatively:
   - Only where measurable benefit exists
   - Document alignment requirements

3. Ensure fallback works (no alignment on old compilers)

**Files to modify**:
- `src/sagittarius/private/vm.h`
- `src/vm.c`
- `src/vmcall.c`

### Phase 5: Build System Updates

**Objective**: Update CMake to detect C11 and enable features appropriately.

**Steps**:

1. Add C11 detection to CMakeLists.txt:
   ```cmake
   CHECK_C_SOURCE_COMPILES("
     _Static_assert(1, \"test\");
     int main() { return 0; }
   " HAVE_C11_STATIC_ASSERT)
   ```

2. Generate config.h entries:
   - `HAVE_C11`
   - `HAVE_C11_STATIC_ASSERT`
   - `HAVE_C11_NORETURN`

3. Add C11 flag for MSVC:
   ```cmake
   IF(MSVC AND MSVC_VERSION >= 1928)
     SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /std:c11")
   ENDIF()
   ```

4. **Do not require C11** - keep optional

**Files to modify**:
- `CMakeLists.txt`
- `cmake/config.h.cmake` (or equivalent)

### Phase 6: Testing

**Objective**: Verify compatibility across compiler versions using CI.

**Steps**:

1. Review existing CI configuration:
   - `.github/workflows/posix-build.yml` (Linux, macOS)
   - `.github/workflows/windows-build.yml` (MSVC)
   - `.circleci/config.yml` (Debian, Ubuntu, Windows)

2. Consider adding compiler version matrix to CI:
   - GCC versions (current + older)
   - Clang versions (current + older)
   - MSVC versions if feasible

3. No new Scheme test files needed:
   - Existing test suite covers runtime behavior
   - CI provides compiler compatibility verification

4. Verify all existing tests pass after changes:
   - Run `ctest --output-on-failure` locally
   - Push to CI and verify all jobs pass

**CI Configuration Updates (if needed)**:
- Add GCC version matrix to posix-build.yml
- Add explicit C standard flags to verify fallbacks work

**Files to modify**:
- `.github/workflows/posix-build.yml` (optional: add compiler matrix)
- `.circleci/config.yml` (optional: add compiler versions)

### Phase 7: Documentation

**Objective**: Update all relevant documentation.

**Steps**:

1. Update `.github/instructions/c-coding.instructions.md`:
   - Add "Compatibility Macros" section
   - Document `SG_NORETURN`, `SG_STATIC_ASSERT`, `SG_ALIGNAS`, `SG_INLINE`
   - Add usage examples

2. Add inline documentation in `platform.h`:
   - Explain each macro
   - Document fallback behavior
   - List supported compilers

3. Update `AGENTS.md` if build instructions change

4. Update `README.md` with compiler requirements matrix

**Files to modify**:
- `.github/instructions/c-coding.instructions.md`
- `src/sagittarius/platform.h` (inline docs)
- `AGENTS.md`
- `README.md`

## Estimation

| Phase | Effort | Notes |
|-------|--------|-------|
| Phase 1: Update platform.h | 2-3 hours | Careful macro design |
| Phase 2: SG_NORETURN | 1-2 hours | Simple replacement |
| Phase 3: Static Assertions | 2-3 hours | Identify all critical checks |
| Phase 4: Alignment | 1-2 hours | Conservative application |
| Phase 5: Build System | 1-2 hours | CMake detection |
| Phase 6: CI Testing | 1-2 hours | Verify CI passes |
| Phase 7: Documentation | 1-2 hours | Update all docs |
| **Total** | **9-16 hours** | Depending on CI updates |

## Testing Strategy

> **Note**: This project implements tests at the Scheme level, not C-level
> unit tests. All test files are located in `test/tests/` directory.
> No new Scheme test files are needed for this change.

### CI-Based Testing

- Rely on existing CI pipelines for compiler compatibility
- GitHub Actions: Linux (Ubuntu), macOS
- CircleCI: Debian, Ubuntu, Windows
- All existing tests must pass on all platforms

### Cross-component Tests

- Build and run full test suite via CI
- Verify static assertions don't break older compiler builds
- Test debug and release builds through CI matrix

### Local Verification

1. Run `ctest --output-on-failure` locally before pushing
2. Verify build completes without warnings
3. Check CI results after push

## Code Quality Guidelines

### Maintainability

- Code must be maintainable by human developers
- Follow existing project conventions and patterns
- Compatibility macros must be self-documenting
- Use clear, descriptive naming (`SG_NORETURN` not `SG_NR`)

### Comments

- Include concise comments explaining fallback behavior
- Document compiler-specific quirks in platform.h
- Avoid redundant comments that repeat the code
- Each macro in platform.h must have a brief description

### Example Comment Style

```c
/**
 * SG_NORETURN - Mark function as non-returning.
 *
 * On C11+: Uses standard _Noreturn keyword.
 * On GCC:  Uses __attribute__((noreturn)).
 * On MSVC: Uses __declspec(noreturn).
 * Fallback: Empty (no optimization hint).
 */
#ifdef SG_C11
#  define SG_NORETURN _Noreturn
/* ... */
#endif
```

## Implementation Checklist

- [x] Phase 1: platform.h updated with all macros
- [x] Phase 2: SG_NORETURN applied to all non-returning functions
- [x] Phase 3: Static assertions added for critical assumptions
- [x] Phase 4: Alignment (skipped - conservative approach)
- [x] Phase 5: Build system (no changes needed)
- [x] Phase 6: CI passes on all platforms
- [x] Phase 7: Documentation updated
- [x] Code reviewed for maintainability
- [x] Comments added where necessary

## User Feedback

After implementation is complete:

1. Present the implementation to the user for review
2. Address any feedback or requested changes
3. Once approved, update `.copilot/README.md` to mark this feature as completed
