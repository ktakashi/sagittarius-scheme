# C11 Modernization

## User Story

As a Sagittarius developer, I want to modernize the C codebase to leverage
C11 features while maintaining backward compatibility with older compilers
through a compatibility layer, so that the code is simpler and potentially
faster on modern compilers while still supporting legacy environments.

## Pre-requisites / Dependencies

### Compiler Requirements

**Primary Target (C11):**

| Compiler | Minimum Version | C11 Support Notes |
|----------|-----------------|-------------------|
| GCC | 4.9+ | Full C11 support |
| Clang | 3.1+ | Full C11 support |
| MSVC | VS 2019 (16.8+) | C11 support via `/std:c11` |

**Fallback Support (C99/C89):**

| Compiler | Fallback Strategy |
|----------|-------------------|
| GCC < 4.9 | C99 mode with compat macros |
| MSVC < 2019 | C89 mode with compat macros |
| Other compilers | Best-effort compatibility |

### Build System

- CMake 3.12+ (already satisfied)
- Detect C11 availability and set appropriate feature flags
- **Keep existing fallback code** for older compilers

### External Dependencies

- Boehm GC: No changes required
- libffi: No changes required
- OpenSSL: No changes required

## Detailed Tasks

### 1. Update Compatibility Macros in platform.h

- [ ] Update `src/sagittarius/platform.h` with:
  - C standard version detection macros
  - Feature availability macros
  - Fallback implementations for C11 features

```c
/* Detect C standard version */
#if __STDC_VERSION__ >= 201112L
#  define SG_C11 1
#elif __STDC_VERSION__ >= 199901L
#  define SG_C99 1
#else
#  define SG_C89 1
#endif

/* _Noreturn compatibility */
#ifdef SG_C11
#  include <stdnoreturn.h>
#  define SG_NORETURN _Noreturn
#elif defined(__GNUC__)
#  define SG_NORETURN __attribute__((noreturn))
#elif defined(_MSC_VER)
#  define SG_NORETURN __declspec(noreturn)
#else
#  define SG_NORETURN /* nothing */
#endif

/* _Static_assert compatibility */
#ifdef SG_C11
#  define SG_STATIC_ASSERT(cond, msg) _Static_assert(cond, msg)
#else
#  define SG_STATIC_ASSERT(cond, msg) \
     typedef char sg_static_assert_##__LINE__[(cond) ? 1 : -1]
#endif

/* _Alignas compatibility */
#ifdef SG_C11
#  include <stdalign.h>
#  define SG_ALIGNAS(n) _Alignas(n)
#elif defined(__GNUC__)
#  define SG_ALIGNAS(n) __attribute__((aligned(n)))
#elif defined(_MSC_VER)
#  define SG_ALIGNAS(n) __declspec(align(n))
#else
#  define SG_ALIGNAS(n) /* nothing */
#endif
```

### 2. Update `_Noreturn` Usage

- [ ] Replace `SG_NO_RETURN` with `SG_NORETURN` from platform.h
- [ ] Keep fallback for GCC `__attribute__((noreturn))`
- [ ] Keep fallback for MSVC `__declspec(noreturn)`
- [ ] Apply to: `Sg_Panic`, `Sg_Error`, `Sg_Exit`

### 3. Introduce Static Assertions (with fallback)

- [ ] Add compile-time checks using `SG_STATIC_ASSERT`:
  - `SgWord` size matches pointer size
  - `SgPair` structure layout
  - Immediate value tag assumptions
- [ ] Use C89-compatible fallback (typedef trick) for old compilers

### 4. Simplify `inline` Handling (preserve fallbacks)

- [ ] Add `SG_INLINE` macro to platform.h:
  ```c
  #if defined(SG_C99) || defined(SG_C11)
  #  define SG_INLINE inline
  #elif defined(__GNUC__)
  #  define SG_INLINE __inline__
  #elif defined(_MSC_VER)
  #  define SG_INLINE __inline
  #else
  #  define SG_INLINE /* nothing */
  #endif
  ```
- [ ] **Keep** existing fallback code in sagittariusdefs.h

### 5. Memory Alignment (with fallback)

- [ ] Add `SG_ALIGNAS` macro with:
  - C11 `_Alignas` when available
  - GCC `__attribute__((aligned(n)))` fallback
  - MSVC `__declspec(align(n))` fallback
- [ ] Apply where beneficial for performance

### 6. Build System Updates

- [ ] Detect C11 support in CMake
- [ ] Add C11 flags only when supported
- [ ] **Do not require C11** - keep fallback path
- [ ] Add `HAVE_C11` config option

### 7. Documentation

- [ ] Document compatibility layer in code
- [ ] Add compiler support matrix to README or docs
- [ ] Document minimum compiler versions for full features
- [ ] Update `.github/instructions/c-coding.instructions.md`:
  - Add section on using platform.h compatibility macros
  - Document `SG_NORETURN`, `SG_STATIC_ASSERT`, `SG_ALIGNAS` usage
  - Update naming conventions to include new macros

## Clarifications

- **Backward compatibility is mandatory**: All features must have fallbacks
- **Thread support**: Continue using pthread/Win32 threads (not C11 threads)
- **Atomics**: Continue using Boehm GC atomics (GC integration required)
- **Testing**: Test on both modern (C11) and older (C99/C89) compilers

## Performance Considerations

- `SG_NORETURN` enables better optimization on modern compilers
- `SG_STATIC_ASSERT` catches errors at compile time (zero runtime cost)
- `SG_ALIGNAS` improves cache performance where applied
- Fallbacks ensure no performance regression on older compilers

## Next Steps

After user approval, update `.copilot/README.md` to include this feature.
