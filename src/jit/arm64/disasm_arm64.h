/* disasm_arm64.h                                  -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2026  Takashi Kato <ktakashi@ymail.com>
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
#ifndef SAGITTARIUS_JIT_DISASM_ARM64_H_
#define SAGITTARIUS_JIT_DISASM_ARM64_H_

#include "../jit.h"

#include <stdint.h>
#include <stddef.h>

/*
 * Disassemble a single ARM64 instruction.
 *
 * Parameters:
 *   insn    - The 32-bit instruction to disassemble
 *   address - The address of the instruction (for PC-relative calculations)
 *   buf     - Output buffer for the disassembly string
 *   bufsize - Size of the output buffer
 *
 * Returns:
 *   Number of characters written (excluding null terminator)
 */
int arm64_disasm_insn(uint32_t insn, uint64_t address, char *buf, size_t bufsize);

/*
 * Disassemble a code buffer.
 *
 * Parameters:
 *   code    - Pointer to the code buffer
 *   size    - Size of the code buffer in bytes
 *   port    - Output port for the disassembly
 */
struct SgPortRec;
void arm64_disasm_buffer(const uint8_t *code, size_t size, struct SgPortRec *port);

#endif /* SAGITTARIUS_JIT_DISASM_ARM64_H_ */
