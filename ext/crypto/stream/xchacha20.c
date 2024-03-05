/*
  XChaCha20 extension
  Ref: https://datatracker.ietf.org/doc/html/draft-arciszewski-xchacha-03
 */

#include "tomcrypt_private.h"

#define QUARTERROUND(a,b,c,d)			\
  a += b; d = ROL(d ^ a, 16);			\
  c += d; b = ROL(b ^ c, 12);			\
  a += b; d = ROL(d ^ a,  8);			\
  c += d; b = ROL(b ^ c,  7);
  

#define debug_print(in, size)			\
  do {						\
    int __i;					\
    fputs(#in ":", stderr);			\
    for (__i = 0; __i < (size); __i++) {	\
      if ((__i % 4) == 0) fputs(" ", stderr);	\
      if ((__i % 16) == 0) fputs("\n", stderr);	\
      fprintf(stderr, "%02x", (in)[__i]);	\
    }						\
    fputs("\n", stderr);			\
  } while (0)					\

static int hchacha(unsigned char *out,
		   const unsigned char *in,
		   const unsigned char *k,
		   int rounds)
{
  int i;
  ulong32 x0, x1, x2, x3, x4, x5, x6, x7;
  ulong32 x8, x9, x10, x11, x12, x13, x14, x15;
  
  x0 = 0x61707865;
  x1 = 0x3320646e;
  x2 = 0x79622d32;
  x3 = 0x6b206574;

  LOAD32L(x4,  k + 0);
  LOAD32L(x5,  k + 4);
  LOAD32L(x6,  k + 8);
  LOAD32L(x7,  k + 12);
  LOAD32L(x8,  k + 16);
  LOAD32L(x9,  k + 20);
  LOAD32L(x10, k + 24);
  LOAD32L(x11, k + 28);

  LOAD32L(x12,  in + 0);
  LOAD32L(x13,  in + 4);
  LOAD32L(x14,  in + 8);
  LOAD32L(x15,  in + 12);

  for (i = 0; i < rounds; i += 2) {
    QUARTERROUND(x0, x4,  x8, x12);
    QUARTERROUND(x1, x5,  x9, x13);
    QUARTERROUND(x2, x6, x10, x14);
    QUARTERROUND(x3, x7, x11, x15);
    QUARTERROUND(x0, x5, x10, x15);
    QUARTERROUND(x1, x6, x11, x12);
    QUARTERROUND(x2, x7,  x8, x13);
    QUARTERROUND(x3, x4,  x9, x14);
  }

  STORE32L( x0, out +  0);
  STORE32L( x1, out +  4);
  STORE32L( x2, out +  8);
  STORE32L( x3, out + 12);
  STORE32L(x12, out + 16);
  STORE32L(x13, out + 20);
  STORE32L(x14, out + 24);
  STORE32L(x15, out + 28);
  return CRYPT_OK;
}

int xchacha_ivctr(chacha_state *st,
		  const unsigned char *iv,
		  unsigned long ivlen,
		  ulong32 counter)
{
  int i;
  unsigned char k2[32];
  unsigned char k0[32];

  LTC_ARGCHK(st != NULL);
  LTC_ARGCHK(iv != NULL);
  /* 192bit IV */
  LTC_ARGCHK(ivlen == 24);
  LTC_ARGCHK(st->ivlen == 0);	/* we can't restore the original key so nok */

  for (i = 0; i < 8; i++) {
    STORE32L(st->input[4 + i], k0 + i*4);
  }
  hchacha(k2, iv, k0, st->rounds);

  /* set derived key  */
  LOAD32L(st->input[4],  k2 + 0);
  LOAD32L(st->input[5],  k2 + 4);
  LOAD32L(st->input[6],  k2 + 8);
  LOAD32L(st->input[7],  k2 + 12);
  LOAD32L(st->input[8],  k2 + 16);
  LOAD32L(st->input[9],  k2 + 20);
  LOAD32L(st->input[10], k2 + 24);
  LOAD32L(st->input[11], k2 + 28);

  /* set nonce */
  st->input[12] = counter;
  st->input[13] = 0;		/* 4 null bytes */
  LOAD32L(st->input[14], iv + 16);
  LOAD32L(st->input[15], iv + 20);
  
  st->ksleft = 0;
  st->ivlen = 12;
  return CRYPT_OK;
}

int xchacha20poly1305_setiv(chacha20poly1305_state *st,
			    const unsigned char *iv,
			    unsigned long ivlen)
{
   chacha_state tmp_st;
   int i, err;
   unsigned char polykey[32], piv[12];

   LTC_ARGCHK(st != NULL);
   LTC_ARGCHK(iv != NULL);
   LTC_ARGCHK(ivlen == 24);

   
   if ((err = xchacha_ivctr(&st->chacha, iv, ivlen, 1)) != CRYPT_OK) return err;

   /* copy xchacha20 key to temporary state */
   for(i = 0; i < 12; i++) tmp_st.input[i] = st->chacha.input[i];
   tmp_st.rounds = 20;
   /* copy nonce of xchacha20 to temporary state */
   STORE32L(st->chacha.input[13], piv);
   STORE32L(st->chacha.input[14], piv + 4);
   STORE32L(st->chacha.input[15], piv + 8);

   /* set IV */
   if ((err = chacha_ivctr32(&tmp_st, piv, 12, 0)) != CRYPT_OK) return err;
   
   /* (re)generate new poly1305 key */
   if ((err = chacha_keystream(&tmp_st, polykey, 32)) != CRYPT_OK) return err;

   /* (re)initialise poly1305 */
   if ((err = poly1305_init(&st->poly, polykey, 32)) != CRYPT_OK) return err;
   st->ctlen  = 0;
   st->aadlen = 0;
   st->aadflg = 1;

   return CRYPT_OK;
}
