/* asm_arm64.c                                     -*- mode:c; coding:utf-8; -*-
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

#include "asm_arm64.h"

#include <stdlib.h>
#include <string.h>

/* For logging via VM logPort */
#include "../../sagittarius.h"

/* Log warning message to VM's logPort if log level permits */
static void jit_asm_log_warn(const char *fmt, ...)
{
  SgVM *vm = Sg_VM();
  if (SG_VM_LOG_LEVEL(vm, SG_WARN_LEVEL)) {
    va_list ap;
    va_start(ap, fmt);
    /* Simple format - just print string for now */
    Sg_Printf(vm->logPort, UC(";; JIT ASM: %s\n"), fmt);
    va_end(ap);
  }
}

/* Set error flag with logging */
#define SET_ERROR(a, msg) do { \
  jit_asm_log_warn(msg); \
  (a)->error = 1; \
} while (0)

/*
 * ARM64 Instruction Encoding Reference
 *
 * All ARM64 instructions are 32 bits. Key encoding patterns:
 *
 * Data Processing (register):
 *   sf | opc | 01011 | shift | 0 | Rm | imm6 | Rn | Rd
 *
 * Data Processing (immediate):
 *   sf | opc | 100 | op | ... | Rn | Rd
 *
 * Load/Store (unsigned offset):
 *   size | 111 | V | 01 | opc | imm12 | Rn | Rt
 *
 * Load/Store (pre/post-indexed):
 *   size | 111 | V | 00 | opc | imm9 | idx | Rn | Rt
 *
 * Conditional branch:
 *   0101010 | o1 | imm19 | o0 | cond
 *
 * Unconditional branch:
 *   0 | op | 00101 | imm26
 */

/* Emit a 32-bit instruction */
static void emit32(Arm64Asm *a, uint32_t insn)
{
  if (a->pos + 4 > a->size) {
    /* Try to resize the buffer if we have a JIT buffer */
    if (a->jitBuf != NULL) {
      size_t newSize = a->size * 2;
      /* Update used size before resize so memcpy copies the right amount */
      a->jitBuf->used = a->pos;
      if (Sg_ResizeJitBuffer(a->jitBuf, newSize) == 0) {
	/* Update our local pointers */
	a->buf = a->jitBuf->code;
	a->size = a->jitBuf->size;
      } else {
	SET_ERROR(a, "Failed to resize JIT buffer");
	return;
      }
    } else {
      SET_ERROR(a, "Buffer overflow (fixed buffer, cannot resize)");
      return;
    }
  }
  /* Little-endian byte order */
  a->buf[a->pos++] = (uint8_t)(insn & 0xFF);
  a->buf[a->pos++] = (uint8_t)((insn >> 8) & 0xFF);
  a->buf[a->pos++] = (uint8_t)((insn >> 16) & 0xFF);
  a->buf[a->pos++] = (uint8_t)((insn >> 24) & 0xFF);
}

/* Add a patch entry for forward references */
static void add_patch(Arm64Asm *a, int label, int type)
{
  Arm64Patch *patch = malloc(sizeof(Arm64Patch));
  if (patch == NULL) {
    SET_ERROR(a, "malloc failed in add_patch");
    return;
  }
  patch->offset = a->pos;
  patch->label = label;
  patch->type = type;
  patch->next = a->patches;
  a->patches = patch;
}

/*
 * Assembler Lifecycle
 */

/* Internal helper to initialize an Arm64Asm structure */
static Arm64Asm* arm64_asm_init(Arm64Asm *a, uint8_t *buf, size_t size,
				SgJitCodeBuffer *jitBuf)
{
  a->buf = buf;
  a->pos = 0;
  a->size = size;
  a->jitBuf = jitBuf;
  a->labelCount = 0;
  a->labelCapacity = 32;
  a->labelOffsets = malloc(32 * sizeof(int));
  a->patches = NULL;
  a->error = 0;

  if (a->labelOffsets == NULL) {
    free(a);
    return NULL;
  }

  return a;
}

Arm64Asm* arm64_asm_new(SgJitCodeBuffer *jitBuf)
{
  Arm64Asm *a;
  if (jitBuf == NULL) return NULL;

  a = malloc(sizeof(Arm64Asm));
  if (a == NULL) return NULL;

  return arm64_asm_init(a, jitBuf->code, jitBuf->size, jitBuf);
}

Arm64Asm* arm64_asm_new_fixed(uint8_t *buf, size_t size)
{
  Arm64Asm *a = malloc(sizeof(Arm64Asm));
  if (a == NULL) return NULL;

  return arm64_asm_init(a, buf, size, NULL);
}

void arm64_asm_free(Arm64Asm *a)
{
  if (a == NULL) return;

  /* Free patches */
  Arm64Patch *p = a->patches;
  while (p) {
    Arm64Patch *next = p->next;
    free(p);
    p = next;
  }

  free(a->labelOffsets);
  free(a);
}

size_t arm64_asm_size(Arm64Asm *a)
{
  return a->pos;
}

int arm64_asm_error(Arm64Asm *a)
{
  return a->error;
}

int arm64_asm_finalize(Arm64Asm *a)
{
  if (a->error) {
    jit_asm_log_warn("Error already set before finalize");
    return -1;
  }

  /* Resolve all patches */
  Arm64Patch *p = a->patches;
  while (p) {
    int labelOff = a->labelOffsets[p->label];
    if (labelOff < 0) {
      /* Unresolved label */
      jit_asm_log_warn("Unresolved label at patch offset");
      SET_ERROR(a, "Unresolved label");
      return -1;
    }

    /* Calculate relative offset */
    int32_t relOff = labelOff - (int32_t)p->offset;

    /* Read existing instruction */
    uint32_t insn = a->buf[p->offset]
      | ((uint32_t)a->buf[p->offset + 1] << 8)
      | ((uint32_t)a->buf[p->offset + 2] << 16)
      | ((uint32_t)a->buf[p->offset + 3] << 24);

    switch (p->type) {
    case ARM64_PATCH_B: {
      /* B: imm26 at bits 0-25, offset in words */
      int32_t imm26 = relOff / 4;
      if (imm26 < -(1 << 25) || imm26 >= (1 << 25)) {
	SET_ERROR(a, "error");
	return -1;
      }
      insn = (insn & 0xFC000000) | (imm26 & 0x03FFFFFF);
      break;
    }
    case ARM64_PATCH_BL: {
      /* BL: imm26 at bits 0-25, offset in words (same as B but opcode differs) */
      int32_t imm26 = relOff / 4;
      if (imm26 < -(1 << 25) || imm26 >= (1 << 25)) {
	SET_ERROR(a, "error");
	return -1;
      }
      insn = (insn & 0xFC000000) | (imm26 & 0x03FFFFFF);
      break;
    }
    case ARM64_PATCH_BCOND: {
      /* B.cond: imm19 at bits 5-23, offset in words */
      int32_t imm19 = relOff / 4;
      if (imm19 < -(1 << 18) || imm19 >= (1 << 18)) {
	SET_ERROR(a, "error");
	return -1;
      }
      insn = (insn & 0xFF00001F) | ((imm19 & 0x7FFFF) << 5);
      break;
    }
    default:
      SET_ERROR(a, "error");
      return -1;
    }

    /* Write patched instruction */
    a->buf[p->offset] = (uint8_t)(insn & 0xFF);
    a->buf[p->offset + 1] = (uint8_t)((insn >> 8) & 0xFF);
    a->buf[p->offset + 2] = (uint8_t)((insn >> 16) & 0xFF);
    a->buf[p->offset + 3] = (uint8_t)((insn >> 24) & 0xFF);

    p = p->next;
  }

  return 0;
}


/*
 * Label Management
 */

int arm64_new_label(Arm64Asm *a)
{
  if (a->labelCount >= a->labelCapacity) {
    int newCap = a->labelCapacity * 2;
    int *newOffsets = realloc(a->labelOffsets, newCap * sizeof(int));
    if (newOffsets == NULL) {
      SET_ERROR(a, "error");
      return -1;
    }
    a->labelOffsets = newOffsets;
    a->labelCapacity = newCap;
  }

  int label = a->labelCount++;
  a->labelOffsets[label] = -1;  /* Unbound */
  return label;
}

void arm64_bind_label(Arm64Asm *a, int label)
{
  if (label < 0 || label >= a->labelCount) {
    SET_ERROR(a, "Invalid label number");
    return;
  }
  a->labelOffsets[label] = (int)a->pos;
}


/*
 * Data Movement Instructions
 */

/* MOV Xd, Xm  -- encoded as ORR Xd, XZR, Xm */
void arm64_mov_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg src)
{
  /* ORR Xd, XZR, Xm: 10101010000 Rm 000000 11111 Rd */
  uint32_t insn = 0xAA0003E0
    | (src << 16)
    | dst;
  emit32(a, insn);
}

/* MOVZ Xd, #imm16, LSL #shift (shift = 0, 16, 32, 48) */
void arm64_movz(Arm64Asm *a, Arm64Reg dst, uint16_t imm, int shift)
{
  int hw = shift / 16;
  /* 110100101 hw imm16 Rd */
  uint32_t insn = 0xD2800000
    | (hw << 21)
    | ((uint32_t)imm << 5)
    | dst;
  emit32(a, insn);
}

/* MOVK Xd, #imm16, LSL #shift */
void arm64_movk(Arm64Asm *a, Arm64Reg dst, uint16_t imm, int shift)
{
  int hw = shift / 16;
  /* 111100101 hw imm16 Rd */
  uint32_t insn = 0xF2800000
    | (hw << 21)
    | ((uint32_t)imm << 5)
    | dst;
  emit32(a, insn);
}

/* MOVN Xd, #imm16, LSL #shift */
void arm64_movn(Arm64Asm *a, Arm64Reg dst, uint16_t imm, int shift)
{
  int hw = shift / 16;
  /* 100100101 hw imm16 Rd */
  uint32_t insn = 0x92800000
    | (hw << 21)
    | ((uint32_t)imm << 5)
    | dst;
  emit32(a, insn);
}

/* Load 64-bit immediate using MOVZ/MOVK sequence */
void arm64_mov_r64_imm(Arm64Asm *a, Arm64Reg dst, int64_t imm)
{
  uint64_t uimm = (uint64_t)imm;

  /* Check for simple cases */
  if (uimm == 0) {
    /* MOV Xd, XZR */
    arm64_mov_r64_r64(a, dst, ARM64_XZR);
    return;
  }

  /* Check if we can use MOVN (for negative numbers) */
  uint64_t inverted = ~uimm;
  int useMovn = 0;
  int movzCount = 0, movnCount = 0;

  for (int i = 0; i < 4; i++) {
    if (((uimm >> (i * 16)) & 0xFFFF) != 0) movzCount++;
    if (((inverted >> (i * 16)) & 0xFFFF) != 0) movnCount++;
  }
  useMovn = (movnCount < movzCount);

  /* Generate instruction sequence */
  int first = 1;
  uint64_t val = useMovn ? inverted : uimm;

  for (int i = 0; i < 4; i++) {
    uint16_t chunk = (val >> (i * 16)) & 0xFFFF;
    if (chunk != 0 || (first && i == 3)) {
      if (first) {
	if (useMovn) {
	  arm64_movn(a, dst, chunk, i * 16);
	} else {
	  arm64_movz(a, dst, chunk, i * 16);
	}
	first = 0;
      } else {
	uint16_t kchunk = useMovn ? ~chunk : chunk;
	arm64_movk(a, dst, kchunk, i * 16);
      }
    }
  }

  /* Handle case where all chunks are zero (but we need MOVN for -1) */
  if (first) {
    if (useMovn) {
      arm64_movn(a, dst, 0, 0);
    } else {
      arm64_movz(a, dst, 0, 0);
    }
  }
}

/* Load pointer value */
void arm64_mov_r64_ptr(Arm64Asm *a, Arm64Reg dst, void *ptr)
{
  arm64_mov_r64_imm(a, dst, (int64_t)(intptr_t)ptr);
}


/*
 * Load/Store Instructions
 */

/* LDR Xt, [Xn, #offset] - unsigned offset */
void arm64_ldr_r64_mem(Arm64Asm *a, Arm64Reg dst, Arm64Reg base, int32_t offset)
{
  /* Check alignment and range */
  if ((offset % 8) != 0 || offset < 0 || offset > 32760) {
    /* Use unscaled offset form if out of range */
    /* LDUR Xt, [Xn, #simm9] */
    if (offset >= -256 && offset <= 255) {
      uint32_t insn = 0xF8400000
	| ((offset & 0x1FF) << 12)
	| (base << 5)
	| dst;
      emit32(a, insn);
      return;
    }
    SET_ERROR(a, "error");
    return;
  }

  /* LDR (unsigned offset): 11111001 01 imm12 Rn Rt */
  int32_t imm12 = offset / 8;
  uint32_t insn = 0xF9400000
    | (imm12 << 10)
    | (base << 5)
    | dst;
  emit32(a, insn);
}

/* STR Xt, [Xn, #offset] - unsigned offset */
void arm64_str_r64_mem(Arm64Asm *a, Arm64Reg src, Arm64Reg base, int32_t offset)
{
  if ((offset % 8) != 0 || offset < 0 || offset > 32760) {
    /* Use unscaled offset */
    if (offset >= -256 && offset <= 255) {
      uint32_t insn = 0xF8000000
	| ((offset & 0x1FF) << 12)
	| (base << 5)
	| src;
      emit32(a, insn);
      return;
    }
    SET_ERROR(a, "error");
    return;
  }

  /* STR (unsigned offset): 11111001 00 imm12 Rn Rt */
  int32_t imm12 = offset / 8;
  uint32_t insn = 0xF9000000
    | (imm12 << 10)
    | (base << 5)
    | src;
  emit32(a, insn);
}

/* LDR Xt, [Xn, #offset]! - pre-indexed */
void arm64_ldr_r64_mem_pre(Arm64Asm *a, Arm64Reg dst, Arm64Reg base, int32_t offset)
{
  if (offset < -256 || offset > 255) {
    SET_ERROR(a, "error");
    return;
  }
  /* 11111000 010 imm9 11 Rn Rt */
  uint32_t insn = 0xF8400C00
    | ((offset & 0x1FF) << 12)
    | (base << 5)
    | dst;
  emit32(a, insn);
}

/* STR Xt, [Xn, #offset]! - pre-indexed */
void arm64_str_r64_mem_pre(Arm64Asm *a, Arm64Reg src, Arm64Reg base, int32_t offset)
{
  if (offset < -256 || offset > 255) {
    SET_ERROR(a, "error");
    return;
  }
  /* 11111000 000 imm9 11 Rn Rt */
  uint32_t insn = 0xF8000C00
    | ((offset & 0x1FF) << 12)
    | (base << 5)
    | src;
  emit32(a, insn);
}

/* LDR Xt, [Xn], #offset - post-indexed */
void arm64_ldr_r64_mem_post(Arm64Asm *a, Arm64Reg dst, Arm64Reg base, int32_t offset)
{
  if (offset < -256 || offset > 255) {
    SET_ERROR(a, "error");
    return;
  }
  /* 11111000 010 imm9 01 Rn Rt */
  uint32_t insn = 0xF8400400
    | ((offset & 0x1FF) << 12)
    | (base << 5)
    | dst;
  emit32(a, insn);
}

/* STR Xt, [Xn], #offset - post-indexed */
void arm64_str_r64_mem_post(Arm64Asm *a, Arm64Reg src, Arm64Reg base, int32_t offset)
{
  if (offset < -256 || offset > 255) {
    SET_ERROR(a, "error");
    return;
  }
  /* 11111000 000 imm9 01 Rn Rt */
  uint32_t insn = 0xF8000400
    | ((offset & 0x1FF) << 12)
    | (base << 5)
    | src;
  emit32(a, insn);
}

/* STR Wt, [Xn, #offset] - unsigned offset 32-bit store */
void arm64_str_r32_mem(Arm64Asm *a, Arm64Reg src, Arm64Reg base, int32_t offset)
{
  if ((offset % 4) != 0 || offset < 0 || offset > 16380) {
    /* Use unscaled offset for non-aligned or negative offsets */
    if (offset >= -256 && offset <= 255) {
      /* STUR Wt, [Xn, #simm9]: 10 111000 00 0 imm9 00 Rn Rt */
      uint32_t insn = 0xB8000000
	| ((offset & 0x1FF) << 12)
	| (base << 5)
	| src;
      emit32(a, insn);
      return;
    }
    SET_ERROR(a, "error");
    return;
  }

  /* STR (unsigned offset) Wt: 10 111001 00 imm12 Rn Rt */
  int32_t imm12 = offset / 4;
  uint32_t insn = 0xB9000000
    | (imm12 << 10)
    | (base << 5)
    | src;
  emit32(a, insn);
}

/* LSR Xd, Xn, #shift - logical shift right by immediate */
void arm64_lsr_r64_r64_imm(Arm64Asm *a, Arm64Reg dst, Arm64Reg src, int32_t shift)
{
  if (shift < 0 || shift > 63) {
    SET_ERROR(a, "error");
    return;
  }
  /* UBFM Xd, Xn, #shift, #63 (alias: LSR Xd, Xn, #shift)
   * Encoding: 11 010011 01 immr imms Rn Rd
   * For LSR: immr = shift, imms = 63 */
  uint32_t insn = 0xD340FC00
    | (shift << 16)
    | (src << 5)
    | dst;
  emit32(a, insn);
}

/* ASR Xd, Xn, #shift - arithmetic shift right by immediate */
void arm64_asr_r64_r64_imm(Arm64Asm *a, Arm64Reg dst, Arm64Reg src, int32_t shift)
{
  if (shift < 0 || shift > 63) {
    SET_ERROR(a, "error");
    return;
  }
  /* SBFM Xd, Xn, #shift, #63 (alias: ASR Xd, Xn, #shift)
   * Encoding: 10 010011 01 immr imms Rn Rd
   * For ASR: immr = shift, imms = 63 */
  uint32_t insn = 0x9340FC00
    | (shift << 16)
    | (src << 5)
    | dst;
  emit32(a, insn);
}

/* LSL Xd, Xn, #shift - logical shift left by immediate */
void arm64_lsl_r64_r64_imm(Arm64Asm *a, Arm64Reg dst, Arm64Reg src, int32_t shift)
{
  if (shift < 0 || shift > 63) {
    SET_ERROR(a, "error");
    return;
  }
  /* UBFM Xd, Xn, #(-shift mod 64), #(63-shift) (alias: LSL Xd, Xn, #shift)
   * Encoding: 11 010011 01 immr imms Rn Rd
   * For LSL: immr = -shift mod 64 = 64-shift, imms = 63-shift */
  int32_t immr = (64 - shift) & 0x3F;
  int32_t imms = 63 - shift;
  uint32_t insn = 0xD3400000
    | (immr << 16)
    | (imms << 10)
    | (src << 5)
    | dst;
  emit32(a, insn);
}

/* AND Xd, Xn, #imm - AND with bitmask immediate (limited patterns) */
void arm64_and_r64_r64_imm(Arm64Asm *a, Arm64Reg dst, Arm64Reg src, uint64_t imm)
{
  /* ARM64 logical immediates use a special encoding. For simple
   * patterns like 0x07 (3 bits), we can encode directly.
   * For 0x07: N=1, immr=0, imms=2 (element size 64, 3 ones at LSB)
   *
   * Encoding format: sf=1 opc=00 100100 N immr imms Rn Rd
   * Base = 0x92400000, need to set immr and imms
   * For N=1, 64-bit element: imms encodes (number of ones - 1)
   */
  if (imm == 0xFF) {
    /* AND Xd, Xn, #0xFF: N=1, immr=0, imms=7 (8 ones at LSB) */
    uint32_t insn = 0x92401C00
      | (src << 5)
      | dst;
    emit32(a, insn);
  } else if (imm == 0x07) {
    /* AND Xd, Xn, #0x7: N=1, immr=0, imms=2 (3 ones at LSB) */
    uint32_t insn = 0x92400800
      | (src << 5)
      | dst;
    emit32(a, insn);
  } else if (imm == 0x03) {
    /* AND Xd, Xn, #0x3: N=1, immr=0, imms=1 (2 ones at LSB) */
    uint32_t insn = 0x92400400
      | (src << 5)
      | dst;
    emit32(a, insn);
  } else if (imm == 0x01) {
    /* AND Xd, Xn, #0x1: N=1, immr=0, imms=0 (1 one at LSB) */
    uint32_t insn = 0x92400000
      | (src << 5)
      | dst;
    emit32(a, insn);
  } else {
    /* For other patterns, fall back to loading immediate and using AND */
    SET_ERROR(a, "unsupported AND immediate");
  }
}

/* ORR Xd, Xn, #imm - OR with bitmask immediate (limited patterns) */
void arm64_orr_r64_r64_imm(Arm64Asm *a, Arm64Reg dst, Arm64Reg src, uint64_t imm)
{
  /* For simple patterns we can encode directly.
   * ARM64 ORR immediate encoding: sf opc 100100 N immr imms Rn Rd
   * For 64-bit: sf=1, opc=01, N=1
   * Base encoding: 0xB2400000, need to set N bit (bit 22) */
  if (imm == 0x01) {
    /* ORR Xd, Xn, #0x1: N=1, immr=0, imms=0
     * Element size 64 (N=1), one bit set at position 0 */
    uint32_t insn = 0xB2400000
      | (1 << 22)        /* N=1 for 64-bit element */
      | (src << 5)
      | dst;
    emit32(a, insn);
  } else {
    /* For other patterns, fall back */
    SET_ERROR(a, "unsupported ORR immediate");
  }
}

/* LDP Xt1, Xt2, [Xn, #offset] */
void arm64_ldp(Arm64Asm *a, Arm64Reg r1, Arm64Reg r2, Arm64Reg base, int32_t offset)
{
  if ((offset % 8) != 0 || offset < -512 || offset > 504) {
    SET_ERROR(a, "error");
    return;
  }
  /* 10 101 0 010 1 imm7 Rt2 Rn Rt */
  int32_t imm7 = offset / 8;
  uint32_t insn = 0xA9400000
    | ((imm7 & 0x7F) << 15)
    | (r2 << 10)
    | (base << 5)
    | r1;
  emit32(a, insn);
}

/* STP Xt1, Xt2, [Xn, #offset] */
void arm64_stp(Arm64Asm *a, Arm64Reg r1, Arm64Reg r2, Arm64Reg base, int32_t offset)
{
  if ((offset % 8) != 0 || offset < -512 || offset > 504) {
    SET_ERROR(a, "error");
    return;
  }
  /* 10 101 0 010 0 imm7 Rt2 Rn Rt */
  int32_t imm7 = offset / 8;
  uint32_t insn = 0xA9000000
    | ((imm7 & 0x7F) << 15)
    | (r2 << 10)
    | (base << 5)
    | r1;
  emit32(a, insn);
}

/* LDP with pre-index */
void arm64_ldp_pre(Arm64Asm *a, Arm64Reg r1, Arm64Reg r2, Arm64Reg base, int32_t offset)
{
  if ((offset % 8) != 0 || offset < -512 || offset > 504) {
    SET_ERROR(a, "error");
    return;
  }
  /* 10 101 0 011 1 imm7 Rt2 Rn Rt */
  int32_t imm7 = offset / 8;
  uint32_t insn = 0xA9C00000
    | ((imm7 & 0x7F) << 15)
    | (r2 << 10)
    | (base << 5)
    | r1;
  emit32(a, insn);
}

/* STP with pre-index */
void arm64_stp_pre(Arm64Asm *a, Arm64Reg r1, Arm64Reg r2, Arm64Reg base, int32_t offset)
{
  if ((offset % 8) != 0 || offset < -512 || offset > 504) {
    SET_ERROR(a, "error");
    return;
  }
  /* 10 101 0 011 0 imm7 Rt2 Rn Rt */
  int32_t imm7 = offset / 8;
  uint32_t insn = 0xA9800000
    | ((imm7 & 0x7F) << 15)
    | (r2 << 10)
    | (base << 5)
    | r1;
  emit32(a, insn);
}


/*
 * Arithmetic Instructions
 */

/* ADD Xd, Xn, Xm */
void arm64_add_r64_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, Arm64Reg m)
{
  /* 10001011 000 Rm 000000 Rn Rd */
  uint32_t insn = 0x8B000000
    | (m << 16)
    | (n << 5)
    | dst;
  emit32(a, insn);
}

/* ADD Xd, Xn, #imm12 */
void arm64_add_r64_r64_imm(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, int32_t imm)
{
  if (imm < 0 || imm > 4095) {
    SET_ERROR(a, "error");
    return;
  }
  /* 10010001 00 imm12 Rn Rd */
  uint32_t insn = 0x91000000
    | (imm << 10)
    | (n << 5)
    | dst;
  emit32(a, insn);
}

/* ADDS Xd, Xn, Xm */
void arm64_adds_r64_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, Arm64Reg m)
{
  /* 10101011 000 Rm 000000 Rn Rd */
  uint32_t insn = 0xAB000000
    | (m << 16)
    | (n << 5)
    | dst;
  emit32(a, insn);
}

/* SUB Xd, Xn, Xm */
void arm64_sub_r64_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, Arm64Reg m)
{
  /* 11001011 000 Rm 000000 Rn Rd */
  uint32_t insn = 0xCB000000
    | (m << 16)
    | (n << 5)
    | dst;
  emit32(a, insn);
}

/* SUB Xd, Xn, #imm12 */
void arm64_sub_r64_r64_imm(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, int32_t imm)
{
  if (imm < 0 || imm > 4095) {
    SET_ERROR(a, "error");
    return;
  }
  /* 11010001 00 imm12 Rn Rd */
  uint32_t insn = 0xD1000000
    | (imm << 10)
    | (n << 5)
    | dst;
  emit32(a, insn);
}

/* SUBS Xd, Xn, Xm */
void arm64_subs_r64_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, Arm64Reg m)
{
  /* 11101011 000 Rm 000000 Rn Rd */
  uint32_t insn = 0xEB000000
    | (m << 16)
    | (n << 5)
    | dst;
  emit32(a, insn);
}

/* NEG Xd, Xm -- SUB Xd, XZR, Xm */
void arm64_neg_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg src)
{
  arm64_sub_r64_r64_r64(a, dst, ARM64_XZR, src);
}

/* MUL Xd, Xn, Xm */
void arm64_mul_r64_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, Arm64Reg m)
{
  /* MADD Xd, Xn, Xm, XZR: 10011011 000 Rm 0 11111 Rn Rd */
  uint32_t insn = 0x9B007C00
    | (m << 16)
    | (n << 5)
    | dst;
  emit32(a, insn);
}

/* SMULH Xd, Xn, Xm - Signed Multiply High (upper 64 bits of 128-bit result) */
void arm64_smulh_r64_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, Arm64Reg m)
{
  /* SMULH Xd, Xn, Xm: 10011011 010 Rm 0 11111 Rn Rd */
  uint32_t insn = 0x9B407C00
    | (m << 16)
    | (n << 5)
    | dst;
  emit32(a, insn);
}


/*
 * Logical Instructions
 */

/* AND Xd, Xn, Xm */
void arm64_and_r64_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, Arm64Reg m)
{
  /* 10001010 000 Rm 000000 Rn Rd */
  uint32_t insn = 0x8A000000
    | (m << 16)
    | (n << 5)
    | dst;
  emit32(a, insn);
}

/* ORR Xd, Xn, Xm */
void arm64_orr_r64_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, Arm64Reg m)
{
  /* 10101010 000 Rm 000000 Rn Rd */
  uint32_t insn = 0xAA000000
    | (m << 16)
    | (n << 5)
    | dst;
  emit32(a, insn);
}

/* EOR Xd, Xn, Xm */
void arm64_eor_r64_r64_r64(Arm64Asm *a, Arm64Reg dst, Arm64Reg n, Arm64Reg m)
{
  /* 11001010 000 Rm 000000 Rn Rd */
  uint32_t insn = 0xCA000000
    | (m << 16)
    | (n << 5)
    | dst;
  emit32(a, insn);
}

/*
 * TST with immediate is complex (uses bitmask encoding).
 * For now, we support common cases like testing low bits.
 */
void arm64_tst_r64_imm(Arm64Asm *a, Arm64Reg n, uint64_t imm)
{
  /* For simple power-of-2 - 1 masks (testing low bits), we use a simpler approach:
   * AND with a register loaded with the immediate, then compare */
  /* This is a simplified implementation - full bitmask encoding is complex */

  /* For tag checking (imm = 0x7 typically), we can use:
   * ANDS XZR, Xn, #imm  -- but bitmask encoding is complex
   * Instead: use TST with register */
  if (imm <= 0xFFFF) {
    /* Load immediate and use register form */
    arm64_movz(a, ARM64_X16, (uint16_t)imm, 0);
    arm64_tst_r64_r64(a, n, ARM64_X16);
  } else {
    /* TODO: Implement full bitmask immediate encoding */
    SET_ERROR(a, "error");
  }
}

/* TST Xn, Xm -- ANDS XZR, Xn, Xm */
void arm64_tst_r64_r64(Arm64Asm *a, Arm64Reg n, Arm64Reg m)
{
  /* 11101010 000 Rm 000000 Rn 11111 */
  uint32_t insn = 0xEA00001F
    | (m << 16)
    | (n << 5);
  emit32(a, insn);
}


/*
 * Comparison Instructions
 */

/* CMP Xn, Xm -- SUBS XZR, Xn, Xm */
void arm64_cmp_r64_r64(Arm64Asm *a, Arm64Reg n, Arm64Reg m)
{
  /* 11101011 000 Rm 000000 Rn 11111 */
  uint32_t insn = 0xEB00001F
    | (m << 16)
    | (n << 5);
  emit32(a, insn);
}

/* CMP Xn, #imm12 -- SUBS XZR, Xn, #imm */
void arm64_cmp_r64_imm(Arm64Asm *a, Arm64Reg n, int32_t imm)
{
  if (imm < 0 || imm > 4095) {
    SET_ERROR(a, "error");
    return;
  }
  /* 11110001 00 imm12 Rn 11111 */
  uint32_t insn = 0xF100001F
    | (imm << 10)
    | (n << 5);
  emit32(a, insn);
}


/*
 * Branch Instructions
 */

/* B label */
void arm64_b(Arm64Asm *a, int label)
{
  if (label >= 0 && label < a->labelCount && a->labelOffsets[label] >= 0) {
    /* Label is bound - calculate offset */
    int32_t offset = a->labelOffsets[label] - (int32_t)a->pos;
    int32_t imm26 = offset / 4;
    if (imm26 < -(1 << 25) || imm26 >= (1 << 25)) {
      SET_ERROR(a, "error");
      return;
    }
    /* 000101 imm26 */
    uint32_t insn = 0x14000000 | (imm26 & 0x03FFFFFF);
    emit32(a, insn);
  } else {
    /* Forward reference - emit placeholder and patch later */
    add_patch(a, label, ARM64_PATCH_B);
    emit32(a, 0x14000000);  /* B with 0 offset */
  }
}

/* B.cond label */
void arm64_b_cond(Arm64Asm *a, Arm64Cond cond, int label)
{
  if (label >= 0 && label < a->labelCount && a->labelOffsets[label] >= 0) {
    /* Label is bound */
    int32_t offset = a->labelOffsets[label] - (int32_t)a->pos;
    int32_t imm19 = offset / 4;
    if (imm19 < -(1 << 18) || imm19 >= (1 << 18)) {
      SET_ERROR(a, "error");
      return;
    }
    /* 01010100 imm19 0 cond */
    uint32_t insn = 0x54000000 | ((imm19 & 0x7FFFF) << 5) | cond;
    emit32(a, insn);
  } else {
    /* Forward reference */
    add_patch(a, label, ARM64_PATCH_BCOND);
    emit32(a, 0x54000000 | cond);
  }
}

/* BL label (branch with link to label) */
void arm64_bl_label(Arm64Asm *a, int label)
{
  if (label >= 0 && label < a->labelCount && a->labelOffsets[label] >= 0) {
    /* Label is bound - calculate offset */
    int32_t offset = a->labelOffsets[label] - (int32_t)a->pos;
    int32_t imm26 = offset / 4;
    if (imm26 < -(1 << 25) || imm26 >= (1 << 25)) {
      SET_ERROR(a, "BL label out of range");
      return;
    }
    /* 100101 imm26 */
    uint32_t insn = 0x94000000 | (imm26 & 0x03FFFFFF);
    emit32(a, insn);
  } else {
    /* Forward reference - emit placeholder and patch later */
    /* Note: BL uses same encoding as B but with different opcode */
    add_patch(a, label, ARM64_PATCH_BL);
    emit32(a, 0x94000000);  /* BL with 0 offset */
  }
}

/* BL addr (branch with link) */
void arm64_bl(Arm64Asm *a, void *target)
{
  /*
   * Always use indirect call for C functions.
   * This is necessary because the buffer may be resized and moved to a new
   * address, which would invalidate any PC-relative offsets calculated
   * based on the old buffer address.
   *
   * Using indirect call: load the target address to X16, then BLR X16.
   * X16 is the "intra-procedure-call" scratch register in AAPCS64.
   */
  arm64_mov_r64_ptr(a, ARM64_X16, target);
  arm64_blr(a, ARM64_X16);
}

/* BLR Xn */
void arm64_blr(Arm64Asm *a, Arm64Reg reg)
{
  /* 11010110 00 11111 0000 00 Rn 00000 */
  uint32_t insn = 0xD63F0000 | (reg << 5);
  emit32(a, insn);
}

/* BR Xn */
void arm64_br(Arm64Asm *a, Arm64Reg reg)
{
  /* 11010110 00 01111 0000 00 Rn 00000 */
  uint32_t insn = 0xD61F0000 | (reg << 5);
  emit32(a, insn);
}

/* RET (RET X30) */
void arm64_ret(Arm64Asm *a)
{
  /* 11010110 01 01111 0000 00 11110 00000 */
  emit32(a, 0xD65F03C0);
}


/*
 * Miscellaneous
 */

void arm64_nop(Arm64Asm *a)
{
  emit32(a, 0xD503201F);
}
