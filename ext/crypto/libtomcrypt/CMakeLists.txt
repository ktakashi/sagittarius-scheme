# -*- mode: cmake; coding: utf-8; -*-
# Build file for libtomcrypt
#
# We only use some of the modules, so the source tree is not completed.
# If you want to see orignal libtomcrypt, see http://libtom.org
#
# This file was written by Takashi Kato and not a part of libtomcrypt.
# 

# reset c flags
IF (WATCOM)
  # watcom does not allow to modify.
  ADD_DEFINITIONS(-DLTC_NO_PROTOTYPES)
ENDIF()

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR}/src/headers)

IF (WIN32)
  ADD_DEFINITIONS(-DLTC_NO_FILE)
ENDIF()

# we do not support encryption only AES
ADD_LIBRARY(aes_enc STATIC src/ciphers/aes/aes.c)
SET_PROPERTY(TARGET aes_enc
  PROPERTY COMPILE_DEFINITIONS ENCRYPT_ONLY)

# we don't need base64 and pkcs5
ADD_LIBRARY(misc STATIC
  src/misc/zeromem.c
  src/misc/error_to_string.c
  src/misc/burn_stack.c
  src/misc/crypt/crypt.c
  src/misc/crypt/crypt_argchk.c
  src/misc/crypt/crypt_fsa.c
  #src/misc/crypt/crypt_ltc_mp_descriptor.c
  )

# ciphers
ADD_LIBRARY(ciphers STATIC
  src/misc/crypt/crypt_cipher_descriptor.c
  src/misc/crypt/crypt_cipher_is_valid.c
  src/misc/crypt/crypt_find_cipher.c
  src/misc/crypt/crypt_find_cipher_any.c
  src/misc/crypt/crypt_find_cipher_id.c
  src/misc/crypt/crypt_register_cipher.c
  src/misc/crypt/crypt_unregister_cipher.c

  src/ciphers/des.c
  src/ciphers/anubis.c
  src/ciphers/blowfish.c
  src/ciphers/cast5.c
  src/ciphers/des.c
  src/ciphers/kasumi.c
  src/ciphers/khazad.c
  src/ciphers/kseed.c
  src/ciphers/multi2.c
  src/ciphers/noekeon.c
  src/ciphers/rc2.c
  src/ciphers/rc5.c
  src/ciphers/rc6.c
  src/ciphers/safer
  src/ciphers/skipjack.c
  src/ciphers/xtea.c
  src/ciphers/camellia.c
  # aes
  src/ciphers/aes/aes.c
  # safer+
  src/ciphers/safer/safer.c src/ciphers/safer/saferp.c
  src/ciphers/safer/safer_tab.c
  # twofish
  src/ciphers/twofish/twofish.c


  # stream cipher
  # chacha
  src/stream/chacha/chacha_crypt.c
  src/stream/chacha/chacha_done.c
  src/stream/chacha/chacha_ivctr32.c
  src/stream/chacha/chacha_ivctr64.c
  src/stream/chacha/chacha_keystream.c
  src/stream/chacha/chacha_setup.c
  # rc4
  src/stream/rc4/rc4.c
  # sober
  src/stream/sober128/sober128.c
  src/stream/sober128/sober128tab.c
  )

# hashes
# we don't need helpers
ADD_LIBRARY(hashes STATIC
  src/misc/crypt/crypt_find_hash.c
  src/misc/crypt/crypt_find_hash_any.c
  src/misc/crypt/crypt_find_hash_id.c
  src/misc/crypt/crypt_find_hash_oid.c
  src/misc/crypt/crypt_register_hash.c
  src/misc/crypt/crypt_unregister_hash.c
  src/misc/crypt/crypt_hash_descriptor.c
  src/misc/crypt/crypt_hash_is_valid.c

  src/hashes/md2.c
  src/hashes/md4.c
  src/hashes/md5.c
  src/hashes/rmd128.c
  src/hashes/rmd160.c
  src/hashes/rmd256.c
  src/hashes/rmd320.c
  src/hashes/sha1.c
  src/hashes/tiger.c
  # chc
  src/hashes/chc/chc.c
  # sha2
  src/hashes/sha2/sha256.c
  src/hashes/sha2/sha224.c
  src/hashes/sha2/sha384.c
  # this is included in sha512.c
  #src/hashes/sha2/sha384.c
  src/hashes/sha2/sha512.c
  src/hashes/sha2/sha512_224.c
  src/hashes/sha2/sha512_256.c
  # sha3
  src/hashes/sha3.c
  src/hashes/sha3_test.c
  # whirl
  src/hashes/whirl/whirl.c
  # blake
  src/hashes/blake2b.c
  src/hashes/blake2s.c)

SET_TARGET_PROPERTIES(hashes PROPERTIES COMPILE_DEFINITIONS LTC_NO_TEST)

# modes
ADD_LIBRARY(modes STATIC
  #cbc
  src/modes/cbc/cbc_decrypt.c
  src/modes/cbc/cbc_done.c
  src/modes/cbc/cbc_encrypt.c
  src/modes/cbc/cbc_getiv.c
  src/modes/cbc/cbc_setiv.c
  src/modes/cbc/cbc_start.c
  #cfb
  src/modes/cfb/cfb_decrypt.c
  src/modes/cfb/cfb_done.c
  src/modes/cfb/cfb_encrypt.c
  src/modes/cfb/cfb_getiv.c
  src/modes/cfb/cfb_setiv.c
  src/modes/cfb/cfb_start.c
  #ctr
  src/modes/ctr/ctr_decrypt.c
  src/modes/ctr/ctr_done.c
  src/modes/ctr/ctr_encrypt.c
  src/modes/ctr/ctr_getiv.c
  src/modes/ctr/ctr_setiv.c
  src/modes/ctr/ctr_start.c
  src/modes/ctr/ctr_test.c
  #ecb
  src/modes/ecb/ecb_decrypt.c
  src/modes/ecb/ecb_done.c
  src/modes/ecb/ecb_encrypt.c
  src/modes/ecb/ecb_start.c
  #f8 do we need this?
  src/modes/f8/f8_decrypt.c
  src/modes/f8/f8_done.c
  src/modes/f8/f8_encrypt.c
  src/modes/f8/f8_getiv.c
  src/modes/f8/f8_setiv.c
  src/modes/f8/f8_start.c
  src/modes/f8/f8_test_mode.c
  #ofb
  src/modes/ofb/ofb_decrypt.c
  src/modes/ofb/ofb_done.c
  src/modes/ofb/ofb_encrypt.c
  src/modes/ofb/ofb_getiv.c
  src/modes/ofb/ofb_setiv.c
  src/modes/ofb/ofb_start.c

  #gcm
  src/encauth/gcm/gcm_add_aad.c
  src/encauth/gcm/gcm_add_iv.c
  src/encauth/gcm/gcm_done.c
  src/encauth/gcm/gcm_init.c
  src/encauth/gcm/gcm_gf_mult.c
  src/encauth/gcm/gcm_mult_h.c
  src/encauth/gcm/gcm_process.c
  src/encauth/gcm/gcm_reset.c
)

#prngs
ADD_LIBRARY(prngs STATIC
  src/misc/crypt/crypt_find_prng.c
  src/misc/crypt/crypt_prng_descriptor.c
  src/misc/crypt/crypt_prng_is_valid.c
  src/misc/crypt/crypt_register_prng.c
  src/misc/crypt/crypt_unregister_prng.c

  src/prngs/chacha20.c
  src/prngs/fortuna.c
  src/prngs/rc4.c
  src/prngs/rng_get_bytes.c
  src/prngs/rng_make_prng.c
  src/prngs/sober128.c
  src/prngs/sprng.c
  src/prngs/yarrow.c)

SET_TARGET_PROPERTIES(prngs PROPERTIES COMPILE_DEFINITIONS LTC_NO_TEST)

SET(DUMMY_FILE ${CMAKE_CURRENT_BINARY_DIR}/src/libtomcrypt.c)
ADD_LIBRARY(libtomcrypt STATIC
  ${DUMMY_FILE})
SET_SOURCE_FILES_PROPERTIES(${DUMMY_FILE}
      PROPERTIES
      GENERATED TRUE)
IF (NOT EXISTS ${DUMMY_FILE})
  FILE(WRITE ${DUMMY_FILE} "#include <tomcrypt.h>\nint main() { return 0; }")
ENDIF()

TARGET_LINK_LIBRARIES(libtomcrypt prngs modes ciphers hashes aes_enc misc)

