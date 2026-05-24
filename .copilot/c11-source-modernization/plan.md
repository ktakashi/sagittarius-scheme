# C11 Source File Modernization - Implementation Plan

## Overview

This plan details the systematic modernization of C source files to use
the compatibility macros defined in `platform.h`.

## Phase 1: SG_INLINE Migration

### 1.1 Core Source Files (src/)

Replace `static inline` with `static SG_INLINE`:

| File | Count | Priority |
|------|-------|----------|
| [vm.c](../../src/vm.c) | 7 | High |
| [number.c](../../src/number.c) | 10 | High |
| [bignum.c](../../src/bignum.c) | 7 | High |
| [cache.c](../../src/cache.c) | 5 | Medium |
| [bytevector.c](../../src/bytevector.c) | 3 | Medium |
| [unicode.c](../../src/unicode.c) | 3 | Medium |
| [string.c](../../src/string.c) | 2 | Medium |
| [pair.c](../../src/pair.c) | 2 | Medium |
| [library.c](../../src/library.c) | 2 | Low |
| [mbignum.c](../../src/mbignum.c) | 1 | Low |
| [port.c](../../src/port.c) | 1 | Low |
| [file.c](../../src/file.c) | 1 | Low |
| [main.c](../../src/main.c) | 1 | Low |
| [clos.c](../../src/clos.c) | 1 | Low |
| **Total** | **46** | |

**Note:** vm.c has 2 functions already using `INLINE` macro - skip those.

### 1.2 OS-Dependent Source Files (src/os/)

| File | Count |
|------|-------|
| [win/win_util.c](../../src/os/win/win_util.c) | 2 |

### 1.3 Extension Source Files (ext/)

| File | Count | Notes |
|------|-------|-------|
| [socket/sagittarius-socket.c](../../ext/socket/sagittarius-socket.c) | 1 | |
| [ffi/sagittarius-ffi.c](../../ext/ffi/sagittarius-ffi.c) | 4 | |
| **Total** | **5** | Excludes libffi/ |

### Phase 1 Checklist

- [x] 1.1.1 Update vm.c (7 functions)
- [x] 1.1.2 Update number.c (10 functions)
- [x] 1.1.3 Update bignum.c (8 functions)
- [x] 1.1.4 Update cache.c (5 functions)
- [x] 1.1.5 Update bytevector.c (3 functions)
- [x] 1.1.6 Update unicode.c (3 functions)
- [x] 1.1.7 Update string.c (2 functions)
- [x] 1.1.8 Update pair.c (2 functions)
- [x] 1.1.9 Update library.c (2 functions)
- [x] 1.1.10 Update remaining src/ files (5 functions)
- [x] 1.2.1 Update src/os/win/win_util.c (2 functions)
- [x] 1.3.1 Update ext/socket/sagittarius-socket.c (1 function)
- [x] 1.3.2 Update ext/ffi/sagittarius-ffi.c (4 functions)
- [x] 1.4 Build verification

## Phase 2: SG_STATIC_ASSERT Addition

### 2.1 Type Size Assertions

Add assertions for critical type size assumptions:

| File | Assertion Target |
|------|------------------|
| bignum.c | `sizeof(unsigned long) * CHAR_BIT >= 32` |
| number.c | `sizeof(double) == 8` (IEEE 754) |
| bytevector.c | `sizeof(uint8_t) == 1` |
| cache.c | Word size assumptions for serialization |

### 2.2 Structure Layout Assertions

| File | Assertion Target |
|------|------------------|
| vm.c | Stack frame layout assumptions |
| pair.c | Pair structure alignment |
| closure.c | Closure layout assumptions |

### 2.3 Alignment Assertions

| File | Assertion Target |
|------|------------------|
| gc-related files | Alignment requirements for tagged pointers |

### Phase 2 Checklist

- [x] 2.1.1 Add type size assertions (bignum.c, number.c, bytevector.c)
- [ ] 2.1.2 Add structure layout assertions (deferred - existing defs.h assertions sufficient)
- [ ] 2.1.3 Add alignment assertions (deferred - existing defs.h assertions sufficient)
- [x] 2.2 Build verification

## Phase 3: Documentation & Testing

### 3.1 Documentation

- [x] 3.1.1 Update [c-coding.instructions.md](../../.github/instructions/c-coding.instructions.md) if needed
- [x] 3.1.2 Add inline comments for non-obvious assertions

### 3.2 Testing

- [x] 3.2.1 Run full test suite (236/237 passed, 1 flaky network test)
- [x] 3.2.2 Run stub generator (`./dist.sh stub`)
- [ ] 3.2.3 Verify build on multiple compilers (deferred - requires CI)

## Execution Order

1. **Phase 1** - Low risk mechanical changes
   - Start with high-priority files (vm.c, number.c, bignum.c)
   - Build after each batch of changes
   - Run tests after completing all inline migrations

2. **Phase 2** - Add static assertions incrementally
   - Add assertions in small batches
   - Verify each batch compiles

3. **Phase 3** - Final verification
   - Full test suite
   - Code generation verification

## Risk Assessment

| Risk | Mitigation |
|------|------------|
| Inline expansion differences | Use `SG_INLINE` consistently |
| Static assertion failures | Assertions verify existing assumptions |
| Build failures on old compilers | Compatibility macros handle fallbacks |

## Estimated Time

| Phase | Duration |
|-------|----------|
| Phase 1: SG_INLINE | 3-4 hours |
| Phase 2: Static assertions | 4-5 hours |
| Phase 3: Testing & docs | 2 hours |
| **Total** | **9-11 hours** |
