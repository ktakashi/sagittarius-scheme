#ifndef STREAM_EXT_H

#include "tomcrypt_private.h"

int xchacha_ivctr(chacha_state *st,
		  const unsigned char *iv,
		  unsigned long ivlen,
		  ulong32 counter);

int xchacha20poly1305_setiv(chacha20poly1305_state *st,
			    const unsigned char *iv,
			    unsigned long ivlen);

#endif /* STREAM_EXT_H */
