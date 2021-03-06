/* -*- mode:c; coding:utf-8 -*-
 *
 * time.h: srfi-19 time library
 *
 *   Copyright (c) 2010-2015  Takashi Kato <ktakashi@ymail.com>
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
 *
 *  $Id: $
 */
#ifndef SAGITTARIUS_TIME_H_
#define SAGITTARIUS_TIME_H_

#include <sagittarius.h>
#include <time.h>

#define TM_NANO  1000000000
#define TM_SID   86400
#define TM_SIDH  43200
#define TM_TAI_EPOCH_IN_JD (double)(4881175/2)

SG_CDECL_BEGIN

SgObject Sg_MakeTime(SgObject type, int64_t sec, uint64_t nsec);
SgObject Sg_SecondsToTime(int64_t sec);
SgObject Sg_TimeToSeconds(SgTime *time);
SgObject Sg_TimeDifference(SgTime *x, SgTime *y, SgTime *r);
SgObject Sg_AddDuration(SgTime *x, SgTime *y, SgTime *r);
SgObject Sg_SubDuration(SgTime *x, SgTime *y, SgTime *r);

SgObject Sg_LocalTimezoneName();

SG_CDECL_END

#endif /* SAGITTARIUS_TIME_H_ */
