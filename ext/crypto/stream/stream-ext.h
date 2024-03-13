#ifndef STREAM_EXT_H

#include "tomcrypt_private.h"

#ifdef __cplusplus
extern "C" {
#endif

int xchacha_ivctr(chacha_state *st,
		  const unsigned char *iv,
		  unsigned long ivlen,
		  ulong32 counter);

int xchacha20poly1305_setiv(chacha20poly1305_state *st,
			    const unsigned char *iv,
			    unsigned long ivlen);

#ifdef __cplusplus
}
#endif

#endif /* STREAM_EXT_H */
