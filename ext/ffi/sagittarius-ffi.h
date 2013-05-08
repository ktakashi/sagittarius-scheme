/* -*- C -*- */
/*
 * ffi.h
 *
 *   Copyright (c) 2010  Takashi Kato <ktakashi@ymail.com>
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
#ifndef SAGITTARIUS_FFI_H_
#define SAGITTARIUS_FFI_H_

#ifdef HAVE_FFI_H
# include <ffi.h>		/* libffi */
#endif
#include <sagittarius.h>

typedef struct SgPointerRec
{
  SG_HEADER;
  uintptr_t pointer;
} SgPointer;

SG_CLASS_DECL(Sg_PointerClass);
#define SG_CLASS_POINTER   (&Sg_PointerClass)
#define SG_POINTER(obj)   ((SgPointer *)obj)
#define SG_POINTERP(obj) SG_XTYPEP(obj, SG_CLASS_POINTER)

#define POINTER_REF(T, p__, offset__) (*((T*)(SG_POINTER(p__)->pointer + offset__)))
#define POINTER_SET(T, p__, offset__, value__)			\
  (*((T*)(SG_POINTER(p__)->pointer + offset__)) = (T)value__)

/* ffi parameter signature*/
enum {
  FFI_MAX_ARGC = 32,
  FFI_MAX_REG  = 6,
  FFI_SIGNATURE_FLOAT    = 'f',
  FFI_SIGNATURE_DOUBLE   = 'd',
  FFI_SIGNATURE_INT      = 'i',
  FFI_SIGNATURE_BOOL     = 'b',
  FFI_SIGNATURE_INT64    = 'x',
  FFI_SIGNATURE_POINTER  = 'p',
  FFI_SIGNATURE_CALLBACK = 'c',
  FFI_SIGNATURE_UINT     = 'u',
  FFI_SIGNATURE_UINT64   = 'U',
};

typedef struct SgFuncInfoRec
{
  SG_HEADER;
  ffi_cif    cif;
  ffi_type  *returnType;
  ffi_type **parameterTypes;
  /* maybe it's better to create a struct for closure */
  int        closureCount;
  /*
    ffi_closure **closures;
    void     **closurelocs;
  */
  uintptr_t  code;
  int        argc;
  SgObject   signatures;
  /* for print for now. */
  SgObject   sReturnType;
  SgObject   sParameterTypes;
} SgFuncInfo;

SG_CLASS_DECL(Sg_FuncInfoClass);
#define SG_CLASS_FUNC_INFO   (&Sg_FuncInfoClass)
#define SG_FUNC_INFO(obj)   ((SgFuncInfo *)obj)
#define SG_FUNC_INFO_P(obj) SG_XTYPEP(obj, SG_CLASS_FUNC_INFO)

typedef struct SgCallbackRec
{
  SG_HEADER;
  uintptr_t uid;
  int returnType;
  SgString *signatures;
  SgObject  proc;
  /* for struct method
     TODO: refactor, this is duplicated management.
   */
  ffi_cif       cif;
  ffi_type    **parameterTypes;
  ffi_closure  *closure;
  void         *code;
} SgCallback;

SG_CLASS_DECL(Sg_CallbackClass);
#define SG_CLASS_CALLBACK   (&Sg_CallbackClass)
#define SG_CALLBACK(obj)   ((SgCallback *)obj)
#define SG_CALLBACKP(obj) SG_XTYPEP(obj, SG_CLASS_CALLBACK)


/* c-struct
   c struct object.
   it contains struct name, layout, size and custom ffi_type.
 */
/* layout */
typedef struct SgCStructRec SgCStruct;

typedef struct struct_layout_rec_t
{
  SgObject   name;		/* member name */
  SgCStruct *cstruct;
  int        array;		/* -1 not array, otherwise array size */
  int        tag;		/* type tag */
  size_t     offset;		/* offset of this member */
  ffi_type  *type;		/* native type */
} struct_layout_t;

struct SgCStructRec
{
  SG_HEADER;
  SgObject  name;
  size_t    fieldCount;
  ffi_type  type;
  int       packed;
  struct_layout_t layouts[1];
};

SG_CLASS_DECL(Sg_CStructClass);
#define SG_CLASS_CSTRUCT   (&Sg_CStructClass)
#define SG_CSTRUCT(obj)   ((SgCStruct *)obj)
#define SG_CSTRUCTP(obj) SG_XTYPEP(obj, SG_CLASS_CSTRUCT)

enum {
FFI_RETURN_TYPE_VOID     =  0x0000,
FFI_RETURN_TYPE_BOOL     =  0x0001,
FFI_RETURN_TYPE_SHORT    =  0x0002,
FFI_RETURN_TYPE_INT      =  0x0003,
FFI_RETURN_TYPE_LONG     =  0x0004,
FFI_RETURN_TYPE_INTPTR   =  0x0005,
FFI_RETURN_TYPE_USHORT   =  0x0006,
FFI_RETURN_TYPE_UINT     =  0x0007,
FFI_RETURN_TYPE_ULONG    =  0x0008,
FFI_RETURN_TYPE_UINTPTR  =  0x0009,
FFI_RETURN_TYPE_FLOAT    =  0x000a,
FFI_RETURN_TYPE_DOUBLE   =  0x000b,
FFI_RETURN_TYPE_STRING   =  0x000c,
FFI_RETURN_TYPE_SIZE_T   =  0x000d,
FFI_RETURN_TYPE_INT8_T   =  0x000e,
FFI_RETURN_TYPE_UINT8_T  =  0x000f,
FFI_RETURN_TYPE_INT16_T  =  0x0010,
FFI_RETURN_TYPE_UINT16_T =  0x0011,
FFI_RETURN_TYPE_INT32_T  =  0x0012,
FFI_RETURN_TYPE_UINT32_T =  0x0013,
FFI_RETURN_TYPE_INT64_T  =  0x0014,
FFI_RETURN_TYPE_UINT64_T =  0x0015,
FFI_RETURN_TYPE_POINTER  =  0x0016,
FFI_RETURN_TYPE_STRUCT   =  0x0017,
/* for struct method */
FFI_RETURN_TYPE_CALLBACK =  0x0018
};


SgObject Sg_MakePointer(void *p);
SgObject Sg_CreateCFunction(SgPointer *handle, int rettype,
			    SgObject args, SgObject sret, SgObject sparam);
SgObject Sg_CreateCallback(int rettype, SgString *signatures, SgObject proc);
void     Sg_ReleaseCallback(SgCallback *callback);
SgObject Sg_CreateCStruct(SgObject name, SgObject layouts, int packedp);
SgObject Sg_CStructRef(SgPointer *p, SgCStruct *st, SgSymbol *name);
void     Sg_CStructSet(SgPointer *p, SgCStruct *st, SgSymbol *name,
		       SgObject value);
void     Sg_DescCStruct(SgCStruct *ct, SgObject port);

void     Sg_PointerSet(SgPointer *p, int offset, int type, SgObject v);

/* malloc */
SgObject Sg_CMalloc(size_t size);
void     Sg_CFree(SgPointer *p);

/* finalize */
SgObject Sg_RegisterFFIFinalizer(SgPointer *pointer, SgObject proc);
SgObject Sg_UnregisterFFIFinalizer(SgPointer *pointer);

#endif /* SAGITTARIUS_FFI_H_ */
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
