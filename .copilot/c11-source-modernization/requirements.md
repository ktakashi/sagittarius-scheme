# C11 Source File Modernization

## User Story

As a Sagittarius developer, I want to apply C11 compatibility macros
throughout the C source files so that the codebase consistently uses
the new `SG_INLINE`, `SG_STATIC_ASSERT`, and other macros defined in
`platform.h`.

## Pre-requisites / Dependencies

- Completed: C11 Modernization (compatibility macros in platform.h)
- Platform.h macros available: `SG_INLINE`, `SG_STATIC_ASSERT`, 
  `SG_ALIGNAS`, `SG_NORETURN`

## Scope Analysis

### Files to Modify

#### src/ Directory (~45 files)

Non-generated C source files:

| Category | Files | Count |
|----------|-------|-------|
| Core Runtime | core.c, vm.c, vmcall.c, vminsn.c | 4 |
| Data Types | pair.c, string.c, vector.c, bytevector.c, symbol.c | 5 |
| Numbers | number.c, bignum.c, mbignum.c, bits.c | 4 |
| I/O | port.c, file.c, codec.c, transcoder.c | 4 |
| Memory | cache.c, weak.c, hashtable.c, treemap.c | 4 |
| Compiler | compiler.c, code.c, closure.c | 3 |
| CLOS | clos.c, subr.c | 2 |
| Text | reader.c, writer.c, unicode.c | 3 |
| Regex | regex.c, regex_match.c, charset.c | 3 |
| System | system.c, thread.c, load.c | 3 |
| Other | error.c, exceptions.c, macro.c, etc. | ~10 |
| **Total src/** | | **~45** |

#### src/os/ Directory (~15 files)

OS-dependent C source files:

| Platform | Files | Count |
|----------|-------|-------|
| POSIX (src/os/posix/) | file.c, pam.c, system.c, thread.c, transcoder.c | 5 |
| Windows (src/os/win/) | file.c, pam.c, pwd.c, shared.c, system.c, thread.c, transcoder.c, win_util.c | 8 |
| **Total src/os/** | | **~13** |

#### ext/ Directory (~30 files)

Extension libraries with native C code:

| Extension | Project Files | Exclude (bundled) |
|-----------|---------------|-------------------|
| atomic/ | *.c | - |
| crypto/ | *.c, sagittarius-*.c | libtomcrypt/ |
| ffi/ | sagittarius-ffi.c, ffi_stub.c | libffi/ |
| filewatch/ | *.c | - |
| odbc/ | *.c | - |
| process/ | *.c | - |
| regex/ | *.c | - |
| socket/ | *.c | - |
| termios/ | *.c | - |
| threads/ | *.c | - |
| time/ | *.c | - |
| zlib/ | *.c | - |
| **Total ext/** | | **~30** |

### Files to Skip

- `lib_*.c` - Auto-generated from .stub or precompiled Scheme
- `builtin-*.c` - Auto-generated symbol/keyword tables
- `ext/crypto/libtomcrypt/` - Bundled libtomcrypt library
- `ext/ffi/libffi/` - Bundled libffi library
- `*.stub` generated output files

## Detailed Tasks

### Phase 1: SG_INLINE Migration

- [ ] Replace `static inline` with `static SG_INLINE` in src/
- [ ] Replace `static inline` with `static SG_INLINE` in src/os/
- [ ] Replace `static inline` with `static SG_INLINE` in ext/

### Phase 2: SG_STATIC_ASSERT Addition

- [ ] Add compile-time checks for structure assumptions in src/
- [ ] Add compile-time checks for structure assumptions in src/os/
- [ ] Add compile-time checks for structure assumptions in ext/

### Phase 3: Documentation

- [ ] Update each modified file header if needed
- [ ] Add comments for non-obvious assertions

## Estimation

| Phase | Files | Effort |
|-------|-------|--------|
| Phase 1: SG_INLINE (src/) | ~10 files | 2-3 hours |
| Phase 1: SG_INLINE (src/os/) | ~5 files | 1 hour |
| Phase 1: SG_INLINE (ext/) | ~5 files | 1-2 hours |
| Phase 2: SG_STATIC_ASSERT | ~25 files | 4-5 hours |
| Phase 3: Documentation | - | 1 hour |
| **Total** | | **9-12 hours** |

## Clarifications

- This is a follow-up to the C11 Modernization feature
- Changes are incremental and low-risk
- Each file can be done independently
- Bundled libraries (libtomcrypt, libffi) are explicitly excluded

## Next Steps

After user approval, update `.copilot/README.md` to include this feature.
