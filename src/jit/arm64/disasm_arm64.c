/* disasm_arm64.c                                  -*- mode:c; coding:utf-8; -*-
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
#include "disasm_arm64.h"

#include <stdio.h>
#include <string.h>
#include <sagittarius/private/port.h>
#include <sagittarius/private/writer.h>

/* Register names */
static const char *reg_names_64[] = {
  "x0",  "x1",  "x2",  "x3",  "x4",  "x5",  "x6",  "x7",
  "x8",  "x9",  "x10", "x11", "x12", "x13", "x14", "x15",
  "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23",
  "x24", "x25", "x26", "x27", "x28", "fp",  "lr",  "sp"
};

/* For zero register in certain contexts */
static const char *reg_name_64(int reg, int use_sp) {
  if (reg == 31) {
    return use_sp ? "sp" : "xzr";
  }
  return reg_names_64[reg];
}

/* Condition code names */
static const char *cond_names[] = {
  "eq", "ne", "cs", "cc", "mi", "pl", "vs", "vc",
  "hi", "ls", "ge", "lt", "gt", "le", "al", "nv"
};

/* Extract bit fields */
#define BITS(insn, hi, lo) (((insn) >> (lo)) & ((1u << ((hi) - (lo) + 1)) - 1))
#define BIT(insn, pos) (((insn) >> (pos)) & 1)

/* Sign extend a value */
static int64_t sign_extend(uint64_t val, int bits) {
  uint64_t sign_bit = 1ULL << (bits - 1);
  if (val & sign_bit) {
    return (int64_t)(val | (~0ULL << bits));
  }
  return (int64_t)val;
}

/*
 * Decode and format different instruction classes
 */

/* Data processing - immediate */
static int disasm_dp_imm(uint32_t insn, uint64_t addr, char *buf, size_t bufsize) {
  int op0 = BITS(insn, 25, 23);
  
  /* MOVZ, MOVN, MOVK (Move wide immediate) */
  if (op0 == 5) {  /* 101 */
    int opc = BITS(insn, 30, 29);
    int hw = BITS(insn, 22, 21);
    int imm16 = BITS(insn, 20, 5);
    int rd = BITS(insn, 4, 0);
    int sf = BIT(insn, 31);
    int shift = hw * 16;
    
    const char *mnemonic;
    switch (opc) {
      case 0: mnemonic = "movn"; break;
      case 2: mnemonic = "movz"; break;
      case 3: mnemonic = "movk"; break;
      default:
        return snprintf(buf, bufsize, ".inst 0x%08x", insn);
    }
    
    if (sf) {
      if (shift == 0) {
        return snprintf(buf, bufsize, "%s %s, #0x%x",
                        mnemonic, reg_name_64(rd, 0), imm16);
      } else {
        return snprintf(buf, bufsize, "%s %s, #0x%x, lsl #%d",
                        mnemonic, reg_name_64(rd, 0), imm16, shift);
      }
    } else {
      return snprintf(buf, bufsize, "%s w%d, #0x%x, lsl #%d",
                      mnemonic, rd, imm16, shift);
    }
  }
  
  /* ADD/SUB immediate */
  if (op0 == 2) {  /* 010 */
    int sf = BIT(insn, 31);
    int op = BIT(insn, 30);
    int S = BIT(insn, 29);
    int sh = BIT(insn, 22);
    int imm12 = BITS(insn, 21, 10);
    int rn = BITS(insn, 9, 5);
    int rd = BITS(insn, 4, 0);
    
    const char *mnemonic;
    if (op == 0 && S == 0) mnemonic = "add";
    else if (op == 0 && S == 1) mnemonic = "adds";
    else if (op == 1 && S == 0) mnemonic = "sub";
    else mnemonic = "subs";
    
    /* CMP is SUBS with Rd = XZR */
    if (op == 1 && S == 1 && rd == 31) {
      if (sh) {
        return snprintf(buf, bufsize, "cmp %s, #0x%x, lsl #12",
                        reg_name_64(rn, 1), imm12);
      } else {
        return snprintf(buf, bufsize, "cmp %s, #0x%x",
                        reg_name_64(rn, 1), imm12);
      }
    }
    
    if (sf) {
      if (sh) {
        return snprintf(buf, bufsize, "%s %s, %s, #0x%x, lsl #12",
                        mnemonic, reg_name_64(rd, 1), reg_name_64(rn, 1), imm12);
      } else {
        return snprintf(buf, bufsize, "%s %s, %s, #0x%x",
                        mnemonic, reg_name_64(rd, 1), reg_name_64(rn, 1), imm12);
      }
    } else {
      return snprintf(buf, bufsize, "%s w%d, w%d, #0x%x",
                      mnemonic, rd, rn, imm12);
    }
  }
  
  return 0;
}

/* Branches */
static int disasm_branch(uint32_t insn, uint64_t addr, char *buf, size_t bufsize) {
  int op0 = BITS(insn, 31, 29);
  int op1 = BITS(insn, 25, 22);
  
  /* Unconditional branch immediate */
  if ((insn & 0x7C000000) == 0x14000000) {
    int op = BIT(insn, 31);
    int64_t imm26 = sign_extend(BITS(insn, 25, 0), 26) << 2;
    uint64_t target = addr + imm26;
    
    if (op == 0) {
      return snprintf(buf, bufsize, "b 0x%llx", (unsigned long long)target);
    } else {
      return snprintf(buf, bufsize, "bl 0x%llx", (unsigned long long)target);
    }
  }
  
  /* Conditional branch */
  if ((insn & 0xFF000010) == 0x54000000) {
    int cond = BITS(insn, 3, 0);
    int64_t imm19 = sign_extend(BITS(insn, 23, 5), 19) << 2;
    uint64_t target = addr + imm19;
    
    return snprintf(buf, bufsize, "b.%s 0x%llx",
                    cond_names[cond], (unsigned long long)target);
  }
  
  /* Unconditional branch register */
  if ((insn & 0xFE1FFC00) == 0xD61F0000) {
    int opc = BITS(insn, 24, 21);
    int rn = BITS(insn, 9, 5);
    
    const char *mnemonic;
    switch (opc) {
      case 0: mnemonic = "br"; break;
      case 1: mnemonic = "blr"; break;
      case 2: mnemonic = "ret"; break;
      default:
        return snprintf(buf, bufsize, ".inst 0x%08x", insn);
    }
    
    if (opc == 2 && rn == 30) {
      return snprintf(buf, bufsize, "ret");
    }
    return snprintf(buf, bufsize, "%s %s", mnemonic, reg_name_64(rn, 0));
  }
  
  return 0;
}

/* Load/Store */
static int disasm_ldst(uint32_t insn, uint64_t addr, char *buf, size_t bufsize) {
  int op0 = BITS(insn, 31, 28);
  int op1 = BIT(insn, 26);
  
  /* Load/Store register (unsigned offset) */
  if ((insn & 0x3B000000) == 0x39000000) {
    int size = BITS(insn, 31, 30);
    int V = BIT(insn, 26);
    int opc = BITS(insn, 23, 22);
    int imm12 = BITS(insn, 21, 10);
    int rn = BITS(insn, 9, 5);
    int rt = BITS(insn, 4, 0);
    
    if (V == 0 && size == 3) {  /* 64-bit integer */
      int scale = 3;  /* 8 bytes */
      int offset = imm12 << scale;
      
      const char *mnemonic = (opc == 0) ? "str" : "ldr";
      if (offset == 0) {
        return snprintf(buf, bufsize, "%s %s, [%s]",
                        mnemonic, reg_name_64(rt, 0), reg_name_64(rn, 1));
      } else {
        return snprintf(buf, bufsize, "%s %s, [%s, #%d]",
                        mnemonic, reg_name_64(rt, 0), reg_name_64(rn, 1), offset);
      }
    }
  }
  
  /* Load/Store register (pre/post-indexed) */
  if ((insn & 0x3B200C00) == 0x38000400 ||  /* pre-indexed */
      (insn & 0x3B200C00) == 0x38000000) {  /* post-indexed unscaled */
    int size = BITS(insn, 31, 30);
    int V = BIT(insn, 26);
    int opc = BITS(insn, 23, 22);
    int imm9 = BITS(insn, 20, 12);
    int op2 = BITS(insn, 11, 10);
    int rn = BITS(insn, 9, 5);
    int rt = BITS(insn, 4, 0);
    
    if (V == 0 && size == 3) {  /* 64-bit integer */
      int64_t offset = sign_extend(imm9, 9);
      const char *mnemonic = (opc == 0) ? "str" : "ldr";
      
      if (op2 == 3) {  /* pre-indexed */
        return snprintf(buf, bufsize, "%s %s, [%s, #%lld]!",
                        mnemonic, reg_name_64(rt, 0), reg_name_64(rn, 1),
                        (long long)offset);
      } else if (op2 == 1) {  /* post-indexed */
        return snprintf(buf, bufsize, "%s %s, [%s], #%lld",
                        mnemonic, reg_name_64(rt, 0), reg_name_64(rn, 1),
                        (long long)offset);
      }
    }
  }
  
  /* Load/Store pair (offset/pre/post-indexed) */
  if ((insn & 0x3A000000) == 0x28000000 ||
      (insn & 0x3A000000) == 0x2A000000) {
    int opc = BITS(insn, 31, 30);
    int V = BIT(insn, 26);
    int mode = BITS(insn, 24, 23);  /* 01=post, 10=offset, 11=pre */
    int L = BIT(insn, 22);
    int imm7 = BITS(insn, 21, 15);
    int rt2 = BITS(insn, 14, 10);
    int rn = BITS(insn, 9, 5);
    int rt = BITS(insn, 4, 0);
    
    if (V == 0 && opc == 2) {  /* 64-bit pair */
      int64_t offset = sign_extend(imm7, 7) << 3;  /* 8-byte scale */
      const char *mnemonic = L ? "ldp" : "stp";
      
      if (mode == 2) {  /* signed offset */
        if (offset == 0) {
          return snprintf(buf, bufsize, "%s %s, %s, [%s]",
                          mnemonic, reg_name_64(rt, 0), reg_name_64(rt2, 0),
                          reg_name_64(rn, 1));
        } else {
          return snprintf(buf, bufsize, "%s %s, %s, [%s, #%lld]",
                          mnemonic, reg_name_64(rt, 0), reg_name_64(rt2, 0),
                          reg_name_64(rn, 1), (long long)offset);
        }
      } else if (mode == 3) {  /* pre-indexed */
        return snprintf(buf, bufsize, "%s %s, %s, [%s, #%lld]!",
                        mnemonic, reg_name_64(rt, 0), reg_name_64(rt2, 0),
                        reg_name_64(rn, 1), (long long)offset);
      } else if (mode == 1) {  /* post-indexed */
        return snprintf(buf, bufsize, "%s %s, %s, [%s], #%lld",
                        mnemonic, reg_name_64(rt, 0), reg_name_64(rt2, 0),
                        reg_name_64(rn, 1), (long long)offset);
      }
    }
  }
  
  return 0;
}

/* Data processing - register */
static int disasm_dp_reg(uint32_t insn, uint64_t addr, char *buf, size_t bufsize) {
  int op0 = BIT(insn, 30);
  int op1 = BIT(insn, 28);
  int op2 = BITS(insn, 24, 21);
  int op3 = BITS(insn, 15, 10);
  
  /* ADD/SUB shifted register */
  if ((insn & 0x1F200000) == 0x0B000000) {
    int sf = BIT(insn, 31);
    int op = BIT(insn, 30);
    int S = BIT(insn, 29);
    int shift = BITS(insn, 23, 22);
    int rm = BITS(insn, 20, 16);
    int imm6 = BITS(insn, 15, 10);
    int rn = BITS(insn, 9, 5);
    int rd = BITS(insn, 4, 0);
    
    const char *mnemonic;
    if (op == 0 && S == 0) mnemonic = "add";
    else if (op == 0 && S == 1) mnemonic = "adds";
    else if (op == 1 && S == 0) mnemonic = "sub";
    else mnemonic = "subs";
    
    /* CMP is SUBS with Rd = XZR */
    if (op == 1 && S == 1 && rd == 31) {
      if (sf) {
        if (imm6 == 0) {
          return snprintf(buf, bufsize, "cmp %s, %s",
                          reg_name_64(rn, 0), reg_name_64(rm, 0));
        }
      }
    }
    
    /* NEG is SUB with Rn = XZR */
    if (op == 1 && S == 0 && rn == 31) {
      if (sf) {
        return snprintf(buf, bufsize, "neg %s, %s",
                        reg_name_64(rd, 0), reg_name_64(rm, 0));
      }
    }
    
    /* MOV is ADD with Rn = XZR or ORR with Rn = XZR */
    
    if (sf) {
      if (imm6 == 0 && shift == 0) {
        return snprintf(buf, bufsize, "%s %s, %s, %s",
                        mnemonic, reg_name_64(rd, 0),
                        reg_name_64(rn, 0), reg_name_64(rm, 0));
      }
    }
  }
  
  /* Logical shifted register */
  if ((insn & 0x1F000000) == 0x0A000000) {
    int sf = BIT(insn, 31);
    int opc = BITS(insn, 30, 29);
    int shift = BITS(insn, 23, 22);
    int N = BIT(insn, 21);
    int rm = BITS(insn, 20, 16);
    int imm6 = BITS(insn, 15, 10);
    int rn = BITS(insn, 9, 5);
    int rd = BITS(insn, 4, 0);
    
    const char *mnemonic;
    if (opc == 0 && N == 0) mnemonic = "and";
    else if (opc == 0 && N == 1) mnemonic = "bic";
    else if (opc == 1 && N == 0) mnemonic = "orr";
    else if (opc == 1 && N == 1) mnemonic = "orn";
    else if (opc == 2 && N == 0) mnemonic = "eor";
    else if (opc == 2 && N == 1) mnemonic = "eon";
    else if (opc == 3 && N == 0) mnemonic = "ands";
    else mnemonic = "bics";
    
    /* MOV is ORR with Rn = XZR */
    if (opc == 1 && N == 0 && rn == 31 && imm6 == 0) {
      if (sf) {
        return snprintf(buf, bufsize, "mov %s, %s",
                        reg_name_64(rd, 0), reg_name_64(rm, 0));
      }
    }
    
    /* TST is ANDS with Rd = XZR */
    if (opc == 3 && N == 0 && rd == 31) {
      if (sf) {
        if (imm6 == 0) {
          return snprintf(buf, bufsize, "tst %s, %s",
                          reg_name_64(rn, 0), reg_name_64(rm, 0));
        }
      }
    }
    
    if (sf) {
      if (imm6 == 0 && shift == 0) {
        return snprintf(buf, bufsize, "%s %s, %s, %s",
                        mnemonic, reg_name_64(rd, 0),
                        reg_name_64(rn, 0), reg_name_64(rm, 0));
      }
    }
  }
  
  /* Data processing (3 source) - MUL, etc. */
  if ((insn & 0x1F000000) == 0x1B000000) {
    int sf = BIT(insn, 31);
    int op54 = BITS(insn, 30, 29);
    int op31 = BITS(insn, 23, 21);
    int rm = BITS(insn, 20, 16);
    int o0 = BIT(insn, 15);
    int ra = BITS(insn, 14, 10);
    int rn = BITS(insn, 9, 5);
    int rd = BITS(insn, 4, 0);
    
    /* MUL is MADD with Ra = XZR */
    if (op54 == 0 && op31 == 0 && o0 == 0 && ra == 31) {
      if (sf) {
        return snprintf(buf, bufsize, "mul %s, %s, %s",
                        reg_name_64(rd, 0), reg_name_64(rn, 0),
                        reg_name_64(rm, 0));
      }
    }
  }
  
  return 0;
}

/* NOP and hints */
static int disasm_hints(uint32_t insn, uint64_t addr, char *buf, size_t bufsize) {
  if (insn == 0xD503201F) {
    return snprintf(buf, bufsize, "nop");
  }
  return 0;
}

/*
 * Main disassembly function
 */
int arm64_disasm_insn(uint32_t insn, uint64_t addr, char *buf, size_t bufsize) {
  int n;
  
  /* Try NOP first */
  n = disasm_hints(insn, addr, buf, bufsize);
  if (n > 0) return n;
  
  /* Try branches */
  n = disasm_branch(insn, addr, buf, bufsize);
  if (n > 0) return n;
  
  /* Try load/store */
  n = disasm_ldst(insn, addr, buf, bufsize);
  if (n > 0) return n;
  
  /* Try data processing (register) */
  n = disasm_dp_reg(insn, addr, buf, bufsize);
  if (n > 0) return n;
  
  /* Try data processing (immediate) */
  n = disasm_dp_imm(insn, addr, buf, bufsize);
  if (n > 0) return n;
  
  /* Unknown instruction */
  return snprintf(buf, bufsize, ".inst 0x%08x", insn);
}

/*
 * Disassemble a buffer
 */
void arm64_disasm_buffer(const uint8_t *code, size_t size, SgPort *port) {
  char line[128];
  size_t offset = 0;
  int ret_count = 0;
  
  while (offset + 4 <= size) {
    uint32_t insn = *(const uint32_t *)(code + offset);
    uint64_t addr = (uint64_t)(code + offset);
    
    /* Stop at NUL padding */
    if (insn == 0) {
      break;
    }
    
    arm64_disasm_insn(insn, addr, line, sizeof(line));
    
    Sg_Printf(port, UC("  0x%08lx:  %08x  "),
              (unsigned long)offset, insn);
    Sg_PutzUnsafe(port, line);
    Sg_PutcUnsafe(port, '\n');
    
    /* Check for RET instruction (0xD65F03C0) */
    if (insn == 0xD65F03C0) {
      ret_count++;
      if (ret_count >= 1) {
        break;  /* Stop after first RET */
      }
    }
    
    offset += 4;
  }
}
