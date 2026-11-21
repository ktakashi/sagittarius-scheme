# Experimental PQC backend

The public-domain sources in `libtomcrypt/`, except the local compatibility
header `tomcrypt_private.h`, are copied without changes from LibTomCrypt
commit `8cbf0b89c46584f694a0c7349140e4c4fe98d1d2`
([PR 720](https://github.com/libtom/libtomcrypt/pull/720)).
`algname_match.c` comes from `src/misc/`, `tomcrypt_pqc.h` from `src/headers/`,
and the remaining files from `src/pqc/` and its algorithm subdirectories.
Their SPDX license is Unlicense. The existing bundled LibTomCrypt is not
upgraded. Its SHA-2, SHA-3 and SHAKE primitives are reused.

This is experimental support, built by default, not a security certification.
The upstream PR is unmerged. Pre-standard Kyber, Dilithium and SPHINCS+
encodings are not accepted as substitutes for the final FIPS algorithms.

## Integration boundary

`sagittarius-pqc.c` is the only Sagittarius adapter that knows backend key
structures and identifiers. Internal `(sagittarius crypto logic pqc)`
bindings exchange bytevectors and Sagittarius parameter names. No backend
key object, ownership rule, error code or ciphertext-validity flag is public.

The native contracts are:

* Sizes: public key, expanded private key, ciphertext/signature, generation
  seed and operation randomness lengths.
* Seed expansion: seed to public and expanded private key.
* Key import validation: raw public/private key to validated public key.
* Encapsulation: public key and 32 random bytes to ciphertext and secret.
* Decapsulation: expanded private key and ciphertext to secret only.
* Signing: expanded private key, message, context and explicit randomness to
  signature; verification returns a boolean. ML-DSA also accepts an internal
  64-byte message representative for FIPS 204 prehash processing.
* Secret protection/clearing: best-effort GC finalization and explicit wiping.

Randomness is obtained in Scheme using Sagittarius random generators, then
passed explicitly to the adapter. Signing is randomized by default.
Deterministic ML-DSA uses zero randomness; deterministic SLH-DSA uses PK.seed,
not zero randomness. Contexts are bytevectors of at most 255 bytes.

Key classes contain normalized bytes, not native key structures. Private-key
bytes and retained seeds must be cleared on destruction. Import/export takes
copies, so callers cannot mutate a live key through an exported bytevector.
Explicit destruction cannot erase copies retained by applications, exported
keys, or all copies made by the runtime/OS.

ASN.1 belongs to Scheme: RFC 9935 for ML-KEM, RFC 9881 for pure ML-DSA, and
RFC 9909 for SLH-DSA. Algorithm parameters must be absent. ML-KEM and ML-DSA
seed, expanded and combined private-key representations must be distinguished;
combined representations must be checked for consistency. Seed encoding is
preferred when a seed is available.

A future native LibTomCrypt backend should replace this adapter and its
source list, not the public key/signature APIs or serialization. It must pass
the same vectors, implicit-rejection, malformed-input and interoperability
tests before replacing this backend.

## Validation scope

Local validation is on macOS. Existing CI covers Linux/macOS, Windows
(MSVC x86/x64), FreeBSD, OpenBSD and NetBSD. All five `crypto/pqc*` tests
are registered unconditionally, even with `SKIP_TEST_VECTOR=ON`, so these
platform jobs exercise the same tests. Their `pqc` label supports focused runs:

```sh
ctest --output-on-failure -L pqc -j 4
```

The selected NIST ACVP fixtures in `../tests/testvectors/pqc/` retain their
group/test identifiers and pinned source references. They cover every key
parameter set, pure and named prehash signatures, deterministic and hedged
signing, all twelve approved ML-DSA prehashes, ML-KEM key validation and
exact implicit-rejection secrets. Key-generation groups retain one case;
signature groups retain the shortest available message per supported hash
and result/reason. Internal-interface-only ACVP groups are not used.
SigGen fixtures store the original signature's SHA-256 and length rather
than duplicating the full signature. SigVer fixtures retain full signatures.
This is a selected conformance corpus, not the complete ACVP dataset.

Independent OpenSSL 4.0.0 vectors and generated public/private keys and
certificates come from the pinned upstream test corpus. Tests check exact
key encodings and verify independent signatures/certificate self-signatures,
not only local round trips.

The standalone `../tests/pqc-native.c` harness exercises all native parameter
sets with bounded malformed-key and signature mutations. On macOS, after an
in-tree build, run AddressSanitizer and UndefinedBehaviorSanitizer as follows
(use `-Wl,--gc-sections` instead of `-Wl,-dead_strip` on Linux):

```sh
mkdir -p .tmp
export TMPDIR="$PWD/.tmp"
cc -std=c11 -O1 -g -DARGTYPE=4 -DLTC_NO_TEST \
  -fsanitize=address,undefined -fno-sanitize-recover=all \
  -fno-omit-frame-pointer -ffunction-sections -fdata-sections -Wl,-dead_strip \
  -Iext/crypto/libtomcrypt/src/headers \
  ext/crypto/tests/pqc-native.c ext/crypto/pqc/libtomcrypt/*.c \
  ext/crypto/libtomcrypt/liblibtomcrypt.a -o .tmp/pqc-sanitizers
ASAN_OPTIONS=detect_leaks=0 UBSAN_OPTIONS=halt_on_error=1 .tmp/pqc-sanitizers
```

The harness instruments the vendored PQC code, not the Sagittarius runtime
or the prebuilt LibTomCrypt hash primitives. Leak checking is disabled.
The Scheme tests additionally cover malformed ASN.1, truncation, parameter
mixing, secret clearing, copying, destruction, and concurrent independent keys.
Bounded mutation tests are not a coverage-guided fuzzing campaign.

Local checks have passed on macOS: all 255 registered CTest tests, selected
NIST/OpenSSL interoperability tests, and the standalone ASan/UBSan harness.
The reference documentation builds, its PQC examples run, and a workspace-staged
installation includes and successfully loads the native and Scheme modules.
Other platform CI jobs have not been executed locally. Whole-runtime
sanitization, sustained fuzzing, timing analysis, and independent/formal
side-channel review remain outstanding. Source inspection and test-vector
success do not prove constant-time behavior or constitute certification.
