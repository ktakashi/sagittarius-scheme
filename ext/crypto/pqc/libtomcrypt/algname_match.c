/* LibTomCrypt, modular cryptographic library -- Tom St Denis */
/* SPDX-License-Identifier: Unlicense */
#include "tomcrypt_private.h"

/**
  @file algname_match.c
  Shared relaxed name-matching helper for algorithm lookup tables.
*/

/**
   Compare two algorithm-name strings.

   Matching is case-insensitive (ASCII) and ignores ' ', '-' and '_' on both sides
   e.g. "SECP256R1", "secp_256_r1", "secp-256-r1" and "secp 256 r1" all match
   @param left   First NUL-terminated string
   @param right  Second NUL-terminated string
   @return 1 if the strings match under the relaxed rules, 0 otherwise
*/
int ltc_algname_match(const char *left, const char *right)
{
   char lc_r, lc_l;

   if (left == NULL || right == NULL) return 0;

   while ((*left != '\0') && (*right != '\0')) {
      while ((*left  == ' ') || (*left  == '-') || (*left  == '_')) left++;
      while ((*right == ' ') || (*right == '-') || (*right == '_')) right++;
      if (*left == '\0' || *right == '\0') break;
      lc_r = *right;
      lc_l = *left;
      if ((lc_r >= 'A') && (lc_r <= 'Z')) lc_r += 32;
      if ((lc_l >= 'A') && (lc_l <= 'Z')) lc_l += 32;
      if (lc_l != lc_r) return 0;
      left++;
      right++;
   }

   if ((*left == '\0') && (*right == '\0')) return 1;
   return 0;
}
