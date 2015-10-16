/* sagittarius-stty.h                            -*- mode: c; coding: utf-8; -*-
 *
 *   Copyright (c) 2015  Takashi Kato <ktakashi@ymail.com>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef SAGITTARIUS_STTY_H_
#define SAGITTARIUS_STTY_H_

#include <sagittarius.h>

/* 
   For now we don't do that much 
   The console mode specifies how console should work.
   The primary pupose for this is disabling echo back.
   Then We might want to get all control character so
   for now we only support 2 flags.

   NB: this must be done on the greatest common divisor
       between Windows and POSIX. Seems Windows has a
       lot less than POSIX.
   
   On Windows we might use DCB to make termios compatible
   functionalities. However it's a bit (or a lot) pain in
   the ass since `I' don't use baud width control and so.
   Until we get enough demand, we only support what we need.
 */
enum SgConsoleMode {
  /* Process control character on system.
     On Windows ENABLE_PROCESSED_OUTPUT or ENABLE_PROCESSED_INPUT
     On POSIX ICANON
   */
  SG_CONSOLE_CANON  = 1L,
  /* Echo back the input
     On Windows ENABLE_ECHO_INPUT
     On POSIX ECHO
   */
  SG_CONSOLE_ECHO = 1L<<1,
};

/*
  NOTE: don't use own constructed mode.
  Usage: 
    int mode = Sg_GetConsoleMode(port);
    // construct flags here
    Sg_SetConsoleMode(port, mode);
  The above doesn't have to be done on C, can be Scheme as well.
  
 */
void     Sg_SetConsoleMode(SgObject port, int mode);
SgObject Sg_GetConsoleMode(SgObject port);

int    Sg_GetConsoleModeFlag(int mode);
/* TODO 
   Add terminal size retriever.
 */

#endif
