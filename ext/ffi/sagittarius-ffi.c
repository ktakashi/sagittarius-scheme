/* ffi.c                                           -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2025  Takashi Kato <ktakashi@ymail.com>
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
#include <string.h>
#include <errno.h>
#include <sagittarius.h>
#include <wchar.h>
#define LIBSAGITTARIUS_BODY
#include <sagittarius/extend.h>
#include "sagittarius-ffi.h"

/* fuck C++!!!! */
#define typename typname

static void pointer_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  SgPointer *p = SG_POINTER(self);
  Sg_Printf(port, UC("#<pointer %p>"), p->pointer);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_PointerClass, pointer_printer);

static SgPointer* make_pointer(uintptr_t p)
{
  SgPointer *z = SG_NEW(SgPointer);
  SG_SET_CLASS(z, SG_CLASS_POINTER);
  z->pointer = p;
  return z;
}

/* To reset pointer... This is really dangerous*/
static SgObject pointer_value(SgPointer *p)
{
#if SIZEOF_VOIDP == 4
  return Sg_MakeIntegerU(p->pointer);
#else
  return Sg_MakeIntegerFromU64(p->pointer);
#endif
}
static void pointer_value_set(SgPointer *p, SgObject value)
{
  /* for now only exact integer */
  if (SG_EXACT_INTP(value)) {
    uintptr_t v;
#if SIZEOF_VOIDP == 4
    v = Sg_GetUIntegerClamp(value, SG_CLAMP_NONE, NULL);
#else
    v = Sg_GetIntegerU64Clamp(value, SG_CLAMP_NONE, NULL);
#endif
    p->pointer = v;
  } else {
    Sg_Error(UC("exact integer required but got %S"), value);
  }
}

static SgSlotAccessor pointer_slots[] = {
  SG_CLASS_SLOT_SPEC("value",  0, pointer_value, pointer_value_set),
  { { NULL } }
};

SgObject Sg_MakePointer(void *p)
{
  return make_pointer((uintptr_t)p);
}

static uintptr_t mask_pointer(SgPointer *p, int bits)
{
  uintptr_t v = p->pointer, mask = 1;
  if (bits < 0) return v;	/* should we do <= ? */
  if (bits >= SIZEOF_VOIDP<<3) return v; /* return all */
  /* create mask */
  mask <<= bits;
  mask--;
  return v & mask;
}
SgObject Sg_PointerToInteger(SgPointer *p, int bits)
{
  intptr_t v = (intptr_t)mask_pointer(p, bits);
#if SIZEOF_VOIDP == 4
  return Sg_MakeInteger((long)v);
#else
  return Sg_MakeIntegerFromS64((int64_t)v);
#endif
}
SgObject Sg_PointerToUInteger(SgPointer *p, int bits)
{
  uintptr_t v = mask_pointer(p, bits);
#if SIZEOF_VOIDP == 4
  return Sg_MakeIntegerU(v);
#else
  return Sg_MakeIntegerFromU64(v);
#endif
}

/* function info */
static void funcinfo_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<c-function %A %A%A>"),
	    SG_FUNC_INFO(self)->sReturnType, 
	    SG_FUNC_INFO(self)->name, 
	    SG_FUNC_INFO(self)->sParameterTypes);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_FuncInfoClass, funcinfo_printer);

static ffi_type* lookup_ffi_return_type(int rettype);

static int set_ffi_parameter_types(SgObject signatures, ffi_type **types)
{
  const SgChar *sigs;
  int i;
  sigs = SG_STRING_VALUE(signatures);
  for (i = 0; i < SG_STRING_SIZE(signatures); i++) {
    switch (sigs[i]) {
    case FFI_SIGNATURE_BOOL:
      types[i] = &ffi_type_uint8; break;
    case FFI_SIGNATURE_INT8:
      types[i] = &ffi_type_sint8; break;
    case FFI_SIGNATURE_UINT8:
      types[i] = &ffi_type_uint8; break;
    case FFI_SIGNATURE_INT16:
      types[i] = &ffi_type_sint16; break;
    case FFI_SIGNATURE_UINT16:
      types[i] = &ffi_type_uint16; break;
    case FFI_SIGNATURE_INT32:
      types[i] = &ffi_type_sint32; break;
    case FFI_SIGNATURE_UINT32:
      types[i] = &ffi_type_uint32; break;
    case FFI_SIGNATURE_INT64:
      types[i] = &ffi_type_sint64; break;
    case FFI_SIGNATURE_UINT64:
      types[i] = &ffi_type_uint64; break;
    case FFI_SIGNATURE_FLOAT:
      types[i] = &ffi_type_float; break;
    case FFI_SIGNATURE_DOUBLE:
      types[i] = &ffi_type_double; break;
    case FFI_SIGNATURE_CHAR:
      types[i] = &ffi_type_sint; break;
    case FFI_SIGNATURE_WIDE_CHAR:
    case FFI_SIGNATURE_WCHAR_T:
#if SIZEOF_WCHAR_T == 2
      types[i] = &ffi_type_uint16; break;
#else
      types[i] = &ffi_type_uint32; break;
#endif
    case FFI_SIGNATURE_CALLBACK:
    case FFI_SIGNATURE_POINTER:
    case FFI_SIGNATURE_STR:
    case FFI_SIGNATURE_WCHAR_STR:
      types[i] = &ffi_type_pointer; break;
    case FFI_SIGNATURE_VARGS:
      /* this must be the last so just return */
      return i;
    default:
      Sg_Error(UC("invalid signature %c"), sigs[i]);
      return -1;		/* dummy */
    }
  }
  return i;
}

static SgFuncInfo* make_funcinfo(uintptr_t proc, int retType, 
				 SgObject signatures)
{
  SgFuncInfo *fn = SG_NEW(SgFuncInfo);
  int i;
  SG_SET_CLASS(fn, SG_CLASS_FUNC_INFO);
  /* signatures must be created in Scheme file */
  /* ASSERT(SG_STRINGP(signatures)); */
  fn->signatures = signatures;
  fn->initialized = TRUE;
  fn->code = proc;
  fn->returnType = lookup_ffi_return_type(retType);
  for (i = 0; i < SG_STRING_SIZE(signatures); i++) {
    if (FFI_SIGNATURE_VARGS == SG_STRING_VALUE_AT(signatures, i)) {
      /* if i grep libffi source i could only find ARM need special handling */
#if !defined(HAVE_FFI_PREP_CIF_VAR) && defined(__arm__)
      Sg_Warn(UC("This build of FFI doesn't support variable length argument properly."));      
#endif
      fn->initialized = FALSE;
      break;
    }
  }
  /* usual case we can pre allocate and reuse it. */
  if (fn->initialized) {
    fn->argc = SG_STRING_SIZE(signatures);
    fn->parameterTypes = SG_NEW_ARRAY(ffi_type*, fn->argc);
    
    set_ffi_parameter_types(signatures, fn->parameterTypes);
    /* initialize ffi_cif */
    if (ffi_prep_cif(&fn->cif, FFI_DEFAULT_ABI, fn->argc,
		     fn->returnType, 
		     fn->parameterTypes) != FFI_OK) {
      Sg_Error(UC("FFI initialization failed."));
      return SG_FUNC_INFO(SG_UNDEF);
    }
  }

  return fn;
}

SgObject Sg_CreateCFunction(SgPointer *handle, SgObject name, int rettype, 
			    SgObject sigs, SgObject sret, SgObject sparam)
{
  SgFuncInfo *fn;
  if (!handle->pointer) {
    Sg_Error(UC("invalid c-function address %S"), handle);
    return SG_UNDEF;
  }
  fn = make_funcinfo(handle->pointer, rettype, sigs);
  fn->name = name;
  fn->sReturnType = sret;
  fn->sParameterTypes = sparam;
  return SG_OBJ(fn);
}

/* callback */
static void callback_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  SgString *sig = SG_CALLBACK(self)->signatures;
  Sg_Printf(port, UC("#<c-callback %A>"), 
	    (sig)? sig : SG_CALLBACK(self)->proc);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_CallbackClass, callback_printer);

static void callback_invoker(ffi_cif *cif, void *result, void **args,
			     void *userdata);
/*
  Callbacks are sometimes stored in non managed storage 
  ex) RegisterClassEx
  And in that case the only memory storing is actually allocated by
  libffi then it will cause SEGV because it might be collected by GC.
  To avoid it, we need to store it into static area so that GC won't
  collect it.

  FIXME: we need to somehow manage releasing callbacks otherwise it consumes
  too much memory when it's created for nothing.
 */
static SgHashTable *callbacks = NULL;

static void release_callback(SgCallback *callback)
{
  /* if the proc isn't procedure (then must be a pointer),
     we don't have to release anything. */
  if (SG_PROCEDUREP(callback->proc)) {
    Sg_HashTableDelete(callbacks, callback->code);
    ffi_closure_free(callback->closure);
  }
}

#if 0
static void callback_finalize(SgObject callback, void *data)
{
  release_callback(SG_CALLBACK(callback));
}
#endif

static SgCallback *make_callback(int rettype, SgString *sig, SgObject proc)
{
  SgCallback *c = SG_NEW(SgCallback);
  SG_SET_CLASS(c, SG_CLASS_CALLBACK);
  c->returnType = rettype;
  c->signatures = sig;
  c->proc = proc;  
  return c;
}

SgObject Sg_CreateCallback(int rettype, SgString *signatures, SgObject proc)
{
  SgCallback *c = make_callback(rettype, signatures, proc);
  c->closure = (ffi_closure*)ffi_closure_alloc(sizeof(ffi_closure), &c->code);
  c->parameterTypes = NULL;
  /* store callback to static area to avoid GC. */
  Sg_HashTableSet(callbacks, c->code, c, 0);
  /* if this won't be GCed then no need to register finalizer */
  /* Sg_RegisterFinalizer(SG_OBJ(c), callback_finalize, NULL); */
  return SG_OBJ(c);
}

void Sg_ReleaseCallback(SgCallback *callback)
{
  release_callback(callback);
  /* Sg_UnregisterFinalizer(SG_OBJ(callback)); */
}

/* cstruct */
static void cstruct_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<c-struct %A %d>"), 
	    SG_CSTRUCT(self)->name, SG_CSTRUCT(self)->type.size);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_CStructClass, cstruct_printer);

static ffi_type* lookup_ffi_return_type(int rettype);
static int convert_scheme_to_c_value(SgObject v, int type, void **result);

static SgCStruct* make_cstruct(size_t size)
{
  SgCStruct *st = SG_NEW2(SgCStruct *,
			  sizeof(SgCStruct)+sizeof(struct_layout_t)*(size-1));
  SG_SET_CLASS(st, SG_CLASS_CSTRUCT);
  /* initialize ffi_type */
  st->type.size = st->type.alignment = 0;
  st->type.elements = SG_NEW_ARRAY(ffi_type *, size + 1);
  st->type.elements[size] = NULL;
  return st;
}

static SgObject SYMBOL_STRUCT = SG_UNDEF;
static SgObject SYMBOL_BIT_FIELD = SG_UNDEF;
static SgLibrary *impl_lib = NULL;

/* compute offset
   from: http://en.wikipedia.org/wiki/Data_structure_alignment
 */
static inline size_t compute_offset(size_t offset, size_t align)
{
  return (offset + align - 1) & ~(align - 1);
}

static inline size_t compute_padding(size_t offset, size_t align, int a)
{
  if (a > 0 && offset % a == 0) return 0;
  return ((size_t)-(ssize_t)offset) & (align - 1);
}

static inline void check_bif_field_size(SgObject fields, size_t size)
{
  size_t sum = 0;
  SgObject field;
  SG_FOR_EACH(field, SG_CDR(fields)) {
    sum += SG_INT_VALUE(SG_CAR(SG_CDAR(field)));
  }
  if ((size<<3) < sum) {
    Sg_AssertionViolation(SG_INTERN("make-c-struct"),
			  SG_MAKE_STRING("bit field size overflow"),
			  fields);
  }
}

SgObject Sg_CreateCStruct(SgObject name, SgObject layouts, int alignment)
{
  SgCStruct *st;
  SgObject cp;
  int index = 0;
  size_t size, max_type, offset, padding;
  if (!SG_LISTP(layouts)) {
    Sg_Error(UC("list required but got %S"), layouts);
  }
  if (alignment > 0
      && alignment != 1 && alignment != 2
      && alignment != 4 && alignment != 8
      && alignment != 16) {
    Sg_Error(UC("alignment must be 1, 2, 4, 8 or 16 but got %d"), alignment);
  }

  size = Sg_Length(layouts);
  st = make_cstruct(size);
  st->name = name;
  st->fieldCount = size;
  /* argument layouts must be like this
     ((name type . type-symbol)
      (name -1   . (struct . struct-name))
      ;; array
      (name type . (size . type-symbol)) ...)
      ;; bit-field
      (bit-field type endian (name bit) ...)
   */
  padding = offset = max_type = size = 0;
  SG_FOR_EACH(cp, layouts) {
    SgObject layout = SG_CAR(cp);
    int type;
    ffi_type *ffi;
    size_t array_off = 0, a_size = 0;
    ASSERT(SG_INTP(SG_CADR(layout)));

    st->layouts[index].name = SG_CAR(layout);
    st->layouts[index].array = -1;
    /* Sg_Printf(Sg_StandardErrorPort(), UC("%S\n"), SG_CAR(layout)); */
    if (SG_PAIRP(SG_CDDR(layout))) {
      if (SG_EQ(SYMBOL_STRUCT, SG_CAR(SG_CDDR(layout)))) {
	SgObject st2;
	st2 = SG_CDR(SG_CDDR(layout));
	if (!SG_CSTRUCTP(st2)) {
	  Sg_Error(UC("c-struct required, but got %S"), st2);
	  return SG_UNDEF;
	}
	st->type.elements[index] = &SG_CSTRUCT(st2)->type;
	st->layouts[index].type = &SG_CSTRUCT(st2)->type;
	st->layouts[index].cstruct = SG_CSTRUCT(st2);
	st->layouts[index].tag = FFI_RETURN_TYPE_STRUCT;
	size += SG_CSTRUCT(st2)->type.size;
	padding = compute_padding(size, SG_CSTRUCT(st2)->type.alignment, alignment);
	size += padding;
	/* Sg_Printf(Sg_StandardErrorPort(), */
	/* 	  UC("%S[%d, %d], size=%d, pad=%d, offset=%d\n"), */
	/* 	  st2, SG_CSTRUCT(st2)->type.size, */
	/* 	  SG_CSTRUCT(st2)->type.alignment, size, padding, offset); */

	if (alignment < 0) {
	  offset = compute_offset(offset, SG_CSTRUCT(st2)->type.alignment);
	} else {
	  offset = compute_offset(offset, alignment);
	}

	st->layouts[index].offset = offset;
	if (SG_CSTRUCT(st2)->type.alignment > max_type) 
	  max_type = SG_CSTRUCT(st2)->type.alignment;
	a_size = SG_CSTRUCT(st2)->type.size;
      } else if (SG_EQ(SYMBOL_BIT_FIELD, SG_CAR(layout))) {
	type = SG_INT_VALUE(SG_CADR(layout));
	ffi = lookup_ffi_return_type(type);
	/* special case, we save endiann and (member bit) alist */
	st->layouts[index].name = SG_CDDR(layout);
	check_bif_field_size(SG_CDDR(layout), ffi->size);
	size += ffi->size;
	a_size = ffi->size;
	goto primitive_type;
      } else if (SG_INTP(SG_CAR(SG_CDDR(layout)))) {
	int asize = SG_INT_VALUE(SG_CAR(SG_CDDR(layout)));
	type = SG_INT_VALUE(SG_CADR(layout));
	ffi = lookup_ffi_return_type(type);
	array_off = asize * ffi->size;
	size += array_off;
	st->layouts[index].array = array_off;
	array_off -= ffi->alignment;
	a_size = array_off;
	goto primitive_type;
      } else {
	Sg_Error(UC("invalid struct layout %S"), layouts);
      }
    } else {
      type = SG_INT_VALUE(SG_CADR(layout));
      ffi = lookup_ffi_return_type(type);
      size += ffi->size;
      a_size = ffi->size;
    primitive_type:
      if (ffi->size > max_type) max_type = ffi->size;
      /* compute new offset */
      padding = compute_padding(size, ffi->alignment, alignment);
      size += padding;
      /* padded size - alignment is offset ... */
      /* offset = compute_offset(offset + 1, ffi->alignment); */
      offset = size - ffi->alignment - array_off;
      /* fprintf(stderr, "size=%d, offset=%d\n", size, offset); */
      st->type.elements[index] = ffi;
      st->layouts[index].type = ffi;
      st->layouts[index].cstruct = NULL;
      st->layouts[index].tag = type;
      st->layouts[index].offset = offset;
    }
    /* next rough offset  */
    offset += a_size;
    index++;
  }

  st->alignment = alignment;
  st->type.alignment = (unsigned short)max_type;
  /* fixup size */
  if (alignment > 0) {
    alignment--;
    size = (size + alignment) & ~alignment;
  } else {
    max_type--;
    size = (size + max_type) & ~max_type;
  }

  st->type.size = size;
  return SG_OBJ(st);
}

/* 
   c-struct-ref:
   we have two situations,
   1. simply refered ex: (c-struct-ref p st 'name)
   2. refered inner struct ex: (c-struct p st 'name.inner)
   latter case, we need to parse '.' and refer it.
 */
static SgObject parse_member_name_rec(SgString *v, SgObject ret)
{
  SgObject index = Sg_StringScanChar(v, '.', SG_STRING_SCAN_INDEX);
  if (SG_FALSEP(index)) {
    return Sg_Cons(Sg_Intern(v), ret);
  } else {
    SgString *rest = SG_STRING(Sg_Substring(v, 1+SG_INT_VALUE(index), -1));
    return Sg_Cons(Sg_Intern(Sg_Substring(v, 0, SG_INT_VALUE(index))),
		   parse_member_name_rec(rest, ret));
  }
}

static SgObject parse_member_name(SgSymbol *name)
{
  SgObject ret = parse_member_name_rec(name->name, SG_NIL);
  return ret;
}

typedef struct bit_field_info
{
  uint64_t mask;
  ssize_t  shifts;
} bfi;

static int name_match(SgObject name, SgObject names, ssize_t size, bfi *mask)
{
  if (SG_SYMBOLP(names)) {
    /* usual member */
    return SG_EQ(name, names);
  } else if (SG_PAIRP(names)) {
    /* bit field */
    SgObject endian = SG_CAR(names);
    /* now calculate the offset.
       here we need to consider endianness. if it's big, then we need to
       pack MSB -> LSB, otherwise LSB -> MSB
       See: http://mjfrazer.org/mjfrazer/bitfields/
    */
    int big_endianP = SG_EQ(endian, SG_INTERN("big"));
    int off = 0;
    ssize_t sizebits = size<<3;

    SG_FOR_EACH(names, SG_CDR(names)) {
      SgObject n = SG_CAR(names);
      int bit = SG_INT_VALUE(SG_CADR(n));
      if (SG_EQ(SG_CAR(n), name)) {
	if (mask) {
	  uint64_t m;
	  if (big_endianP) {
	    /* size = 4, off = 8, bit = 2, 
	       then we need #b0000 0000 1100 0000 0000 0000 0000 0000
	    */
	    if (sizebits-off == sizeof(uint64_t)<<3) {
	      m = (uint64_t)-(int64_t)1ULL;
	    } else {
	      m = 1ULL<<(sizebits-off);
	    }
	    mask->mask = m-1;
	    mask->mask ^= (m>>bit)-1;
	    mask->shifts = -(sizebits - (off+bit));
	  } else {
	    /* off = 8, bit = 2, 
	       then we need #b1100 0000
	    */
	    if (bit+off == sizeof(uint64_t)<<3) {
	      m = (uint64_t)-(int64_t)1ULL;
	    } else {
	      m = 1ULL<<(bit+off);
	    }
	    mask->mask = m-1;
	    mask->mask ^= (m>>bit)-1;
	    mask->shifts = -off;
	  }
	}
	return TRUE;
      }
      off += bit;
    }
  }
  return FALSE;
}
static size_t calculate_alignment(SgObject names, SgCStruct *st,
				  int *foundP, int *type,
				  ssize_t *array, size_t *size,
				  bfi *bitMask)
{
  size_t i;
  SgObject name = SG_CAR(names);
  struct_layout_t *layouts = st->layouts;

  /* names are list of property name for struct.
     name        => (name)
     name1.name2 => (name1 name2)
   */
  for (i = 0; i < st->fieldCount; i++) {
    /* property found */
    if (name_match(name, layouts[i].name, layouts[i].type->size, bitMask)) {
      /* it's this one */
      if (SG_NULLP(SG_CDR(names))) {
	*foundP = TRUE;
	*type = layouts[i].tag;
	*array = layouts[i].array;
	*size = layouts[i].type->size;
	return layouts[i].offset;
      /* property was struct */
      } else if (layouts[i].cstruct) {
	size_t align = layouts[i].offset;
	/* search from here */
	align += calculate_alignment(SG_CDR(names),
				     layouts[i].cstruct,
				     foundP, type, array, size, NULL);
	/* second name was a member of the struct */
	if (foundP) {
	  return align;
	}
      }
    }
  }
  /* not found! */
  return 0;
}

/* static SgHashTable *ref_table; */

static SgObject convert_c_to_scheme(int rettype, SgPointer *p, size_t align)
{
  switch (rettype) {
  case FFI_RETURN_TYPE_BOOL    :
    return SG_MAKE_BOOL(POINTER_REF(intptr_t, p, align));
  case FFI_RETURN_TYPE_SHORT   :
    return SG_MAKE_INT(POINTER_REF(short, p, align));
  case FFI_RETURN_TYPE_INT     :
    return Sg_MakeInteger(POINTER_REF(int, p, align));
  case FFI_RETURN_TYPE_LONG    :
    return Sg_MakeInteger(POINTER_REF(long, p, align));
  case FFI_RETURN_TYPE_INTPTR  :
#if SIZEOF_VOIDP == 4
    return Sg_MakeInteger(POINTER_REF(intptr_t, p, align));
#else
    return Sg_MakeIntegerFromS64(POINTER_REF(intptr_t, p, align));
#endif
  case FFI_RETURN_TYPE_INT8_T  :
    return SG_MAKE_INT(POINTER_REF(int8_t, p, align));
  case FFI_RETURN_TYPE_INT16_T :
    return SG_MAKE_INT(POINTER_REF(int16_t, p, align));
  case FFI_RETURN_TYPE_INT32_T :
    return Sg_MakeInteger(POINTER_REF(int32_t, p, align));
  case FFI_RETURN_TYPE_USHORT  :
    return SG_MAKE_INT(POINTER_REF(unsigned short, p, align));
  case FFI_RETURN_TYPE_UINT    :
    return Sg_MakeIntegerU(POINTER_REF(unsigned int, p, align));
  case FFI_RETURN_TYPE_SIZE_T  :
  case FFI_RETURN_TYPE_ULONG   :
    return Sg_MakeIntegerU(POINTER_REF(unsigned long, p, align));
  case FFI_RETURN_TYPE_UINTPTR :
#if SIZEOF_VOIDP == 4
    return Sg_MakeIntegerU(POINTER_REF(uintptr_t, p, align));
#else
    return Sg_MakeIntegerFromU64(POINTER_REF(uintptr_t, p, align));
#endif
  case FFI_RETURN_TYPE_UINT8_T :
    return SG_MAKE_INT(POINTER_REF(uint8_t, p, align));
  case FFI_RETURN_TYPE_UINT16_T:
    return SG_MAKE_INT(POINTER_REF(uint16_t, p, align));
  case FFI_RETURN_TYPE_UINT32_T:
    return Sg_MakeIntegerU(POINTER_REF(uint32_t, p, align));
  case FFI_RETURN_TYPE_FLOAT   :
    return Sg_MakeFlonum((double)POINTER_REF(float, p, align));
  case FFI_RETURN_TYPE_DOUBLE  :
    return Sg_MakeFlonum(POINTER_REF(double, p, align));
  case FFI_RETURN_TYPE_STRING  : {
    char *s = POINTER_REF(char*, p, align);
    if (!s) return Sg_MakePointer(s);
    return Sg_Utf8sToUtf32s(s, (int)strlen(s));
  }
  case FFI_RETURN_TYPE_INT64_T :
    return Sg_MakeIntegerFromS64(POINTER_REF(int64_t, p, align));
  case FFI_RETURN_TYPE_UINT64_T:
    return Sg_MakeIntegerFromU64(POINTER_REF(uint64_t, p, align));
  case FFI_RETURN_TYPE_POINTER :
    return make_pointer(POINTER_REF(uintptr_t, p, align));
  case FFI_RETURN_TYPE_STRUCT  :
    return make_pointer((uintptr_t)&POINTER_REF(uintptr_t, p, align));
  case FFI_RETURN_TYPE_CALLBACK: {
    void *pp = POINTER_REF(void*, p, align);
    SgObject r;
    /* TODO should we return pointer? */
    if (!pp) return SG_FALSE;

    r = Sg_HashTableRef(callbacks, pp, SG_FALSE);
    if (SG_FALSEP(r)) {
      /* wrap with call back
	 NB: no finalizer is needed, it doesn't have libffi thing
       */
      return make_callback(-1, NULL, Sg_MakePointer(pp));
    }
    return r;
  }
  case FFI_RETURN_TYPE_WCHAR_T: 
#if SIZEOF_WCHAR_T == 2
    return SG_MAKE_INT(POINTER_REF(wchar_t, p, align));
#else
    return Sg_MakeIntegerFromU64(POINTER_REF(wchar_t, p, align));
#endif
  case FFI_RETURN_TYPE_WCHAR_STR: {
    wchar_t *s = POINTER_REF(wchar_t*, p, align);
    if (!s) return Sg_MakePointer(s);
    return Sg_WCharTsToString(s, wcslen(s));
  }
  case FFI_RETURN_TYPE_CHAR:
    return SG_MAKE_CHAR(POINTER_REF(char, p, align));
  case FFI_RETURN_TYPE_WIDE_CHAR:
    return SG_MAKE_CHAR(POINTER_REF(wchar_t, p, align));
  default:
    Sg_Error(UC("unknown FFI return type: %d"), rettype);
    return NULL;
  }
}

SgObject Sg_CStructRef(SgPointer *p, SgCStruct *st, SgSymbol *name)
{
  SgObject names = parse_member_name(name);
  int foundP = FALSE, type = 0;
  ssize_t array;
  size_t size;
  bfi bitMask = {0,};
  size_t align = calculate_alignment(names, st, &foundP, &type, &array, &size,
				     &bitMask);

  if (!foundP || type == 0) {
    Sg_Error(UC("c-struct %A does not have a member named %A"), st->name, name);
    return SG_UNDEF;		/* dummy */
  }
  if (array < 0) {
    if (bitMask.mask == 0) {
      return convert_c_to_scheme(type, p, align);
    } else {
      SgObject r = convert_c_to_scheme(type, p, align);
      /* bit field type check must be done by definition so the
	 returning value should always be exact integer (fixnum or bignum)
	 we check it here just in case.
	 Bit field type must have less than 64 bit. it's purely limitation
	 of Sagittarius FFI...
       */
      
      if (!SG_EXACT_INTP(r) || size > 8) {
	Sg_Error(UC("c-struct-ref: %A isn't integer"), name);
      }
      r = Sg_LogAnd(r, Sg_MakeIntegerFromU64(bitMask.mask));
      if (bitMask.shifts) {
	return Sg_Ash(r, (long)bitMask.shifts);
      }
      return r;
    }
  } else {
    /* TODO what should we return for array? so far vector.*/
    SgObject vec;
    int i;
    array /= size;
    vec = Sg_MakeVector((long)array, SG_UNDEF);
    for (i = 0; i < array; i++) {
      /* array won't have bit field*/
      SG_VECTOR_ELEMENT(vec, i) = 
	convert_c_to_scheme(type, p, align + (i * size));
    }
    return vec;
  }
}

void Sg_CStructSet(SgPointer *p, SgCStruct *st, SgSymbol *name, SgObject value)
{
  SgObject names = parse_member_name(name);
  int foundP = FALSE, type = 0;
  ssize_t array;
  size_t size;
  bfi bitMask = {0,};
  size_t align = calculate_alignment(names, st, &foundP, &type, &array, &size,
				     &bitMask);

  if (!foundP || type == 0) {
    Sg_Error(UC("c-struct %A does not have a member named %A"), st->name, name);
    return;		/* dummy */
  }
  if (array < 0) {
    switch (type) {
    case FFI_RETURN_TYPE_STRUCT:
      /* we need to copy it */
      Sg_CMemcpy(p, (long)align, value, 0, size);
      break;
    default:
      if (bitMask.mask) {
	SgObject r = convert_c_to_scheme(type, p, align);
	if (!SG_EXACT_INTP(value)) {
	  Sg_Error(UC("c-struct-set!: bit field value must be an integer. %S"),
		   value);
	}
	if (!SG_EXACT_INTP(r) || size > 8) {
	  Sg_Error(UC("c-struct-set!: %A isn't integer"), name);
	}
	if (bitMask.shifts) {
	  value = Sg_Ash(value, (long)-bitMask.shifts);
	}
	/* clear current field */
	r = Sg_LogAnd(r, Sg_MakeIntegerFromU64(~bitMask.mask));
	value = Sg_LogIor(value, r);
	Sg_PointerSet(p, (int)align, type, value);
      } else {
	Sg_PointerSet(p, (int)align, type, value);
      }
      break;
    }
  } else {
    int i;
    /* TODO what should we return for array? so far vector.*/
    if (!SG_VECTORP(value)) {
      Sg_Error(UC("Array member %A requires a vector but got %S"), value);
      return;
    }
    array /= size;
    for (i = 0; i < array && i < SG_VECTOR_SIZE(value); i++) {
      switch (type) {

      case FFI_RETURN_TYPE_STRUCT:
	Sg_Error(UC("array of struct is not supported. %S"), st);
	break;
      default:
	Sg_PointerSet(p, (int)(align + (i * size)), type, 
		      SG_VECTOR_ELEMENT(value, i));
	break;
      }
    }
  }
}

static inline void put_indent(SgPort *port, int indent)
{
  int i;
  for (i = 0; i < indent; i++) {
    Sg_PutcUnsafe(SG_PORT(port), ' ');
  }
}

static void desc_c_struct_rec(SgCStruct *ct, SgPort *port, int indent)
{
  size_t i;
  put_indent(SG_PORT(port), indent);
  Sg_Printf(SG_PORT(port), UC("%A (%d):\n"), ct->name, ct->type.size);
  for (i = 0; i < ct->fieldCount; i++) {
    put_indent(SG_PORT(port), indent + 2);
    Sg_Printf(SG_PORT(port), UC("%2d %A"), ct->layouts[i].offset,
	      ct->layouts[i].name);
    if (ct->layouts[i].cstruct) {
      Sg_PutcUnsafe(SG_PORT(port), '\n');
      desc_c_struct_rec(ct->layouts[i].cstruct, port, indent + 4);
    } else {
      Sg_Printf(SG_PORT(port), UC("(%d"), ct->layouts[i].type->size);
      if (ct->layouts[i].array > 0) {
	Sg_Printf(SG_PORT(port), UC(" x %d"), 
		  ct->layouts[i].array / ct->layouts[i].type->size);
      }
      Sg_PutcUnsafe(SG_PORT(port), ')');
    }
    Sg_PutcUnsafe(SG_PORT(port), '\n');
  }
}

void Sg_DescCStruct(SgCStruct *ct, SgObject port)
{
  SG_PORT_LOCK_WRITE(SG_PORT(port));
  desc_c_struct_rec(ct, SG_PORT(port), 0);
  SG_PORT_UNLOCK_WRITE(SG_PORT(port));
}

/* from ruby-ffi module */
typedef union
{
  int8_t s8;
  uint8_t u8;
  int16_t s16;
  uint16_t u16;
  int32_t s32;
  uint32_t u32;
  int64_t s64;
  uint64_t u64;
  signed long sl;
  unsigned long ul;
  wchar_t wc;
  void* ptr;
  float f32;
  double f64;
} ffi_storage;

static void set_ffi_callback_parameter_types(SgObject signatures,
					     ffi_type **types)
{
  int i;
  for (i = 0; i < SG_STRING_SIZE(signatures); i++) {
    SgChar c = SG_STRING_VALUE_AT(signatures, i); 
    switch (c) {
      /* bool */
    case FFI_SIGNATURE_BOOL:
      types[i] = &ffi_type_sint; break;
      /* byte */
    case FFI_SIGNATURE_INT8:
      types[i] = &ffi_type_sint8; break;
    case FFI_SIGNATURE_UINT8:
      types[i] = &ffi_type_uint8; break;
      /* word */
    case FFI_SIGNATURE_INT16:
      types[i] = &ffi_type_sint16; break;
    case FFI_SIGNATURE_UINT16:
      types[i] = &ffi_type_uint16; break;
      /* dword */
    case FFI_SIGNATURE_INT32:
      types[i] = &ffi_type_sint32; break;
    case FFI_SIGNATURE_UINT32:
      types[i] = &ffi_type_uint32; break;
      /* qword */
    case FFI_SIGNATURE_INT64:
      types[i] = &ffi_type_sint64; break;
    case FFI_SIGNATURE_UINT64:
      types[i] = &ffi_type_uint64; break;
      /* float */
    case FFI_SIGNATURE_FLOAT:
      types[i] = &ffi_type_float; break;
    case FFI_SIGNATURE_DOUBLE:
      types[i] = &ffi_type_double; break;
    case FFI_SIGNATURE_POINTER:
    case FFI_SIGNATURE_STR:
    case FFI_SIGNATURE_WCHAR_STR:
      types[i] = &ffi_type_pointer; break;
    case FFI_SIGNATURE_CHAR:
      types[i] = &ffi_type_sint; break;
    case FFI_SIGNATURE_WIDE_CHAR:
    case FFI_SIGNATURE_WCHAR_T:
#if SIZEOF_WCHAR_T == 2
      types[i] = &ffi_type_uint16; break;
#else
      types[i] = &ffi_type_uint32; break;
#endif
    default:
      Sg_Error(UC("invalid callback argument signature: %c"),
	       SG_STRING_VALUE_AT(signatures, i));
      break;
    }
  }
}

static int prep_method_handler(SgCallback *callback);

static SgObject address_mark = SG_FALSE;

static SgObject get_error_message(SgChar signature, SgObject obj)
{
  switch (signature) {
  case FFI_SIGNATURE_BOOL:
    return Sg_Sprintf(UC("'bool' required but got %A"), obj);
  case FFI_SIGNATURE_INT8:
  case FFI_SIGNATURE_UINT8:
    return Sg_Sprintf(UC("'int8' required but got %A"), obj);
  case FFI_SIGNATURE_INT16:
  case FFI_SIGNATURE_UINT16:
    return Sg_Sprintf(UC("'int16' required but got %A"), obj);
  case FFI_SIGNATURE_INT32:
  case FFI_SIGNATURE_UINT32:
    return Sg_Sprintf(UC("'int32' required but got %A"), obj);
  case FFI_SIGNATURE_INT64:
  case FFI_SIGNATURE_UINT64:
    return Sg_Sprintf(UC("'int64' required but got %A"), obj);
  case FFI_SIGNATURE_WCHAR_T:
    return Sg_Sprintf(UC("'wchar_t' required but got %A"), obj);
  case FFI_SIGNATURE_FLOAT:
    return Sg_Sprintf(UC("'float' required but got %A"), obj);
  case FFI_SIGNATURE_DOUBLE:
    return Sg_Sprintf(UC("'double' required but got %A"), obj);
  case FFI_SIGNATURE_CALLBACK:
    return Sg_Sprintf(UC("'callback' required but got %A"), obj);
  case FFI_SIGNATURE_POINTER:
    return Sg_Sprintf(UC("'pointer' required but got %A"), obj);
  case FFI_SIGNATURE_STR:
    return Sg_Sprintf(UC("'char*' required but got %A"), obj);
  case FFI_SIGNATURE_WCHAR_STR:
    return Sg_Sprintf(UC("'wchar_t*' required but got %A"), obj);
  case FFI_SIGNATURE_VARGS:
    return Sg_Sprintf(UC("'size of pointer value' required but got %A"), obj);
  default:
    return Sg_Sprintf(UC("'unknown signature type' %c with %A"), 
		      signature, obj);
  }
}

static int push_ffi_type_value(SgFuncInfo *info,
			       SgChar signature,
			       SgObject obj,
			       ffi_storage *storage,
			       SgObject *lastError)
{
  if (SG_INTP(obj)) {
    /* fixnum -> int */
    switch (signature) {
    case FFI_SIGNATURE_INT8:
    case FFI_SIGNATURE_UINT8:
    case FFI_SIGNATURE_INT16:
    case FFI_SIGNATURE_UINT16:
    case FFI_SIGNATURE_INT32:
    case FFI_SIGNATURE_UINT32:
      storage->sl = SG_INT_VALUE(obj);
      return TRUE;
    case FFI_SIGNATURE_INT64:
    case FFI_SIGNATURE_UINT64:
      storage->s64 = SG_INT_VALUE(obj);
      return TRUE;
    case FFI_SIGNATURE_WCHAR_T:
      storage->wc = (wchar_t)SG_INT_VALUE(obj);
      return TRUE;
    case FFI_SIGNATURE_FLOAT:
      storage->f32 = (float)SG_INT_VALUE(obj);
      return TRUE;
    case FFI_SIGNATURE_DOUBLE:
      storage->f64 = (double)SG_INT_VALUE(obj);
      return TRUE;
    default:
      *lastError = get_error_message(signature, obj);
      return FALSE;
    }
  } else if (SG_FLONUMP(obj)) {
    /* flonum -> double */
    switch (signature) {
    case FFI_SIGNATURE_FLOAT: {
#if defined(__cplusplus) && defined(USE_IMMEDIATE_FLONUM)
      storage->f32 = (float)Sg_FlonumValue(obj);
#else
      storage->f32 = SG_FLONUM_VALUE(obj);
#endif
      return TRUE;
    }
    case FFI_SIGNATURE_DOUBLE:
#if defined(__cplusplus) && defined(USE_IMMEDIATE_FLONUM)
      storage->f64 = Sg_FlonumValue(obj);
#else
      storage->f64 = SG_FLONUM_VALUE(obj);
#endif
      return TRUE;
    default:
      *lastError = get_error_message(signature, obj);
      return FALSE;
    }
  } else if (SG_BIGNUMP(obj)) {
    int oor = FALSE;
    switch (signature) {
    case FFI_SIGNATURE_UINT8:
    case FFI_SIGNATURE_UINT16:
    case FFI_SIGNATURE_UINT32:
      storage->ul = Sg_GetUIntegerClamp(obj, SG_CLAMP_NONE, &oor);
      if (oor) {
	*lastError = SG_MAKE_STRING("out of range");
	return FALSE;
      }
      return TRUE;
    case FFI_SIGNATURE_INT8:
    case FFI_SIGNATURE_INT16:
    case FFI_SIGNATURE_INT32:
      storage->sl = Sg_GetIntegerClamp(obj, SG_CLAMP_NONE, &oor);
      if (oor) {
	*lastError = SG_MAKE_STRING("out of range");
	return FALSE;
      }
      return TRUE;
    case FFI_SIGNATURE_INT64:
      storage->s64 = Sg_GetIntegerS64Clamp(obj, SG_CLAMP_NONE, &oor);
      if (oor) {
	*lastError = SG_MAKE_STRING("out of range");
	return FALSE;
      }
      return TRUE;
    case FFI_SIGNATURE_UINT64:
      storage->u64 = Sg_GetIntegerU64Clamp(obj, SG_CLAMP_NONE, &oor);
      if (oor) {
	*lastError = SG_MAKE_STRING("out of range");
	return FALSE;
      }
      return TRUE;
    default:
      *lastError = get_error_message(signature, obj);
      return FALSE;
    }
  } else if (SG_CHARP(obj)) {
    switch (signature) {
    case FFI_SIGNATURE_INT8:
      storage->s8 = (char)SG_CHAR_VALUE(obj);
      return TRUE;
    case FFI_SIGNATURE_UINT8:
      storage->u8 = (unsigned char)SG_CHAR_VALUE(obj);
      return TRUE;
    case FFI_SIGNATURE_CHAR:
      storage->sl = SG_CHAR_VALUE(obj);
      return TRUE;
    case FFI_SIGNATURE_WCHAR_T:
    case FFI_SIGNATURE_WIDE_CHAR:
      storage->wc = (wchar_t)SG_CHAR_VALUE(obj);
      return TRUE;
    default:
      *lastError = get_error_message(signature, obj);
      return FALSE;
    }
  } else if (SG_STRINGP(obj)) {
    /* string -> char* (utf-8 ascii only) */
    switch (signature) {
    case FFI_SIGNATURE_STR:
    case FFI_SIGNATURE_POINTER:
      storage->ptr = (void*)(Sg_Utf32sToUtf8s(SG_STRING(obj)));
      return TRUE;
    case FFI_SIGNATURE_WCHAR_STR:
      storage->ptr = (void*)(Sg_StringToWCharTs(SG_STRING(obj)));
      return TRUE;
    default:
      *lastError = get_error_message(signature, obj);
      return FALSE;
    }
  } else if (SG_BVECTORP(obj)) {
    /* bytevector -> char* */
    switch (signature) {
    case FFI_SIGNATURE_POINTER:
    case FFI_SIGNATURE_WCHAR_STR:
      storage->ptr = (void*)(SG_BVECTOR_ELEMENTS(obj));
      return TRUE;
    default:
      *lastError = get_error_message(signature, obj);
      return FALSE;
    }
  } else if (SG_POINTERP(obj)) {
    switch (signature) {
    case FFI_SIGNATURE_POINTER:
    case FFI_SIGNATURE_WCHAR_STR:
      storage->ptr = (void*)SG_POINTER(obj)->pointer;
      return TRUE;
    case FFI_SIGNATURE_CALLBACK:
      /* accept only NULL pointer */
      if (!SG_POINTER(obj)->pointer) {
	storage->ptr = (void*)NULL;
	return TRUE;
      }
    default:
      *lastError = get_error_message(signature, obj);
      return FALSE;
    }
  } else if (SG_BOOLP(obj)) {
    switch (signature) {
    case FFI_SIGNATURE_BOOL:
      storage->sl = SG_TRUEP(obj) ? 1L : 0L;
      return TRUE;
    default:
      *lastError = get_error_message(signature, obj);
      return FALSE;
    }
  } else if (SG_CALLBACKP(obj)) {
    switch (signature) {
    case FFI_SIGNATURE_CALLBACK:
      /* prepare closure here */
      if (SG_PROCEDUREP(SG_CALLBACK(obj)->proc)) {
	if (!prep_method_handler(SG_CALLBACK(obj))) {
	  *lastError = Sg_Sprintf(UC("failed to prepare the callback."));
	  return FALSE;
	}
	
	storage->ptr = SG_CALLBACK(obj)->code;
	return TRUE;
      } else if (SG_POINTERP(SG_CALLBACK(obj)->proc)) {
	/* the pointer should contain function pointer */
	storage->ptr = (void *)SG_POINTER(SG_CALLBACK(obj)->proc)->pointer;
	return TRUE;
      } else {
	*lastError = SG_MAKE_STRING("invalid callback.");
	return FALSE;
      }
    default:
      *lastError = get_error_message(signature, obj);
      return FALSE;
    }
    /* address stuff */
  } else if (SG_PAIRP(obj) && 
	     SG_EQ(SG_CAR(obj), address_mark) &&
	     !SG_NULLP(SG_CDR(obj)) &&
	     (SG_POINTERP(SG_CADR(obj)) || SG_BVECTORP(SG_CADR(obj)))) {
    int offset = 0;
    if (!SG_NULLP(SG_CDDR(obj))) {
      SgObject off = SG_CAR(SG_CDDR(obj));
      if (!SG_INTP(off) || SG_INT_VALUE(off) < 0) {
	*lastError = Sg_Sprintf(UC("address offset must be a positive fixnum"
				   " but got %S"),
				off);
	return FALSE;
      }
      offset = SG_INT_VALUE(off);
    }
    switch (signature) {
    case FFI_SIGNATURE_POINTER:
      if (SG_POINTERP(SG_CADR(obj))) {
	/* we need to do kinda silly thing here to get offset*/
	if (offset) {
	  if (SG_POINTER(SG_CADR(obj))->pointer) {
	    storage->ptr = 
	      &*(void**)(SG_POINTER(SG_CADR(obj))->pointer + offset);
	  } else {
	    *lastError = 
	      Sg_Sprintf(UC("offset of null pointer is not allowed"));
	    return FALSE;
	  }
	} else {
	  /* you may want to pass null pointer. then we just pass the
	     address of this 'pointer' slot. */
	  storage->ptr = &(SG_POINTER(SG_CADR(obj))->pointer);
	}
      } else {
	/* simple overflow check */
	if (offset) {
	  if (SG_BVECTOR_SIZE(SG_CADR(obj)) <= offset) {
	    *lastError = Sg_Sprintf(UC("specified offset is overflowing, %d"),
				    offset);
	    return FALSE;
	  }
	}
	/* even though allocate memory of a bytevector is continuous but
	   we do calculate the address offset like this (hope no overhead).
	   this is because, we may change the structure of bytevector in 
	   the future. */
	storage->ptr = 
	  &*(void**)((uintptr_t)SG_BVECTOR_ELEMENTS(SG_CADR(obj)) + offset);
      }
      return TRUE;
    default:
      *lastError = get_error_message(signature, obj);
      return FALSE;
    }    
  } else {
    *lastError = Sg_Sprintf(UC("unsupported ffi argument %S"), obj);
    return FALSE;
  }
}

static ffi_type* lookup_ffi_return_type(int rettype)
{
  switch (rettype) {
  case FFI_RETURN_TYPE_VOID    : return &ffi_type_void;
  case FFI_RETURN_TYPE_BOOL    : return &ffi_type_sint;
  case FFI_RETURN_TYPE_SHORT   : return &ffi_type_sshort;
  case FFI_RETURN_TYPE_INT     : return &ffi_type_sint;
  case FFI_RETURN_TYPE_LONG    : return &ffi_type_slong;
  case FFI_RETURN_TYPE_USHORT  : return &ffi_type_ushort;
  case FFI_RETURN_TYPE_UINT    : return &ffi_type_uint;
  case FFI_RETURN_TYPE_ULONG   : return &ffi_type_ulong;
    /* intptr_t and uintptr_t must be the same as pointer */
  case FFI_RETURN_TYPE_INTPTR  : return &ffi_type_pointer;
  case FFI_RETURN_TYPE_UINTPTR : return &ffi_type_pointer;
  case FFI_RETURN_TYPE_FLOAT   : return &ffi_type_float;
  case FFI_RETURN_TYPE_DOUBLE  : return &ffi_type_double;
  case FFI_RETURN_TYPE_STRING  : return &ffi_type_pointer;
  case FFI_RETURN_TYPE_SIZE_T  : return &ffi_type_ulong;
  case FFI_RETURN_TYPE_INT8_T  : return &ffi_type_sint8;
  case FFI_RETURN_TYPE_UINT8_T : return &ffi_type_uint8;
  case FFI_RETURN_TYPE_INT16_T : return &ffi_type_sint16;
  case FFI_RETURN_TYPE_UINT16_T: return &ffi_type_uint16;
  case FFI_RETURN_TYPE_INT32_T : return &ffi_type_sint32;
  case FFI_RETURN_TYPE_UINT32_T: return &ffi_type_uint32;
  case FFI_RETURN_TYPE_INT64_T : return &ffi_type_sint64;
  case FFI_RETURN_TYPE_UINT64_T: return &ffi_type_uint64;
  case FFI_RETURN_TYPE_POINTER : return &ffi_type_pointer;
  case FFI_RETURN_TYPE_CALLBACK: return &ffi_type_pointer;
    /* scheme character, map to sint8 :) */
  case FFI_RETURN_TYPE_CHAR    : return &ffi_type_sint8;
    /* libffi doesn't support this by default */
  case FFI_RETURN_TYPE_WCHAR_T:
  case FFI_RETURN_TYPE_WIDE_CHAR:
#if SIZEOF_WCHAR_T == 2
    return &ffi_type_uint16;
#else
    return &ffi_type_uint32;
#endif
  case FFI_RETURN_TYPE_WCHAR_STR: return &ffi_type_pointer;
  default:
    Sg_Error(UC("failed to lookup. unknown FFI return type: %d"), rettype);
    return NULL;
  }
}

/* TODO cleanup the code. use macro! */
static int convert_scheme_to_c_value(SgObject v, int type, void **result)
{
#define CONVERT(type, conv)					\
  do {								\
    if (SG_EXACT_INTP(v)) {					\
      if (SG_INTP(v)) {						\
	*((type *)result) = (type)SG_INT_VALUE(v);		\
      } else {							\
	*((type *)result) = (type)conv(v, SG_CLAMP_NONE, NULL);	\
      }								\
    } else {							\
      *((type *)result) = (type)0;				\
    }								\
  } while (0)

#define SCONVERT(type) CONVERT(type, Sg_GetIntegerClamp)
#define UCONVERT(type) CONVERT(type, Sg_GetUIntegerClamp)
#define BCONVERT(type) *((intptr_t *)result) = !SG_FALSEP(v)
#define S64CONVERT(type) CONVERT(type, Sg_GetIntegerS64Clamp)
#define U64CONVERT(type) CONVERT(type, Sg_GetIntegerU64Clamp)
#if SIZEOF_VOIDP == 4
# define INT2IP Sg_GetIntegerClamp
#else
# define INT2IP Sg_GetIntegerS64Clamp
#endif

#define FCONVERT(type)					\
  do {							\
    if (!SG_REALP(v)) *((type *)result) = 0.0;		\
    else *((type *)result) = (type)Sg_GetDouble(v);	\
  } while (0)
#define IPCONVERT(type)							\
  do {									\
    if (SG_EXACT_INTP(v)) {						\
      if (SG_INTP(v)) {							\
	*((type *)result) = SG_INT_VALUE(v);				\
      } else {								\
	*((type *)result) = INT2IP(v, SG_CLAMP_NONE, NULL);		\
      }									\
    } else if (SG_POINTERP(v)) {					\
      *((type *)result) = SG_POINTER(v)->pointer;			\
    } else goto ret0;							\
  } while (0)

#define STRCONVERT_REC(type, stob)			\
  do {							\
    if (SG_STRINGP(v)) {				\
      *((type *)result) = (type)stob(SG_STRING(v));	\
    } else if (SG_BVECTORP(v)) {			\
      *((type *)result) = (type)SG_BVECTOR_ELEMENTS(v);	\
    } else if (SG_POINTERP(v)) {			\
      *((type *)result) = (type)SG_POINTER(v)->pointer;	\
    } else goto ret0;					\
  } while (0)
#define CSCONVERT(type) STRCONVERT_REC(type, Sg_Utf32sToUtf8s)
#define WSCONVERT(type) STRCONVERT_REC(type, Sg_StringToWCharTs)
#define CHCONVERT(type)					\
  do {							\
    if (SG_CHARP(v)) {					\
      *((type *)result) = (type)SG_CHAR_VALUE(v);	\
    } else goto ret0;					\
  } while (0)

#define case_type(type, ctype, conv) case type : conv(ctype); break;
#define scase_type(type, ctype) case_type(type, ctype, SCONVERT)
#define ucase_type(type, ctype) case_type(type, ctype, UCONVERT)

  switch (type) {
    case_type(FFI_RETURN_TYPE_BOOL, int, BCONVERT);

    scase_type(FFI_RETURN_TYPE_SHORT  , short);
    scase_type(FFI_RETURN_TYPE_INT    , int);
    scase_type(FFI_RETURN_TYPE_LONG   , long);
    scase_type(FFI_RETURN_TYPE_INT8_T , int8_t);
    scase_type(FFI_RETURN_TYPE_INT16_T, int16_t);
    scase_type(FFI_RETURN_TYPE_INT32_T, int32_t);
    
    ucase_type(FFI_RETURN_TYPE_USHORT  , unsigned short);
    ucase_type(FFI_RETURN_TYPE_UINT    , unsigned int);
    ucase_type(FFI_RETURN_TYPE_ULONG   , unsigned long);
    ucase_type(FFI_RETURN_TYPE_SIZE_T  , size_t);
    ucase_type(FFI_RETURN_TYPE_UINT8_T , uint8_t);
    ucase_type(FFI_RETURN_TYPE_UINT16_T, uint16_t);
    ucase_type(FFI_RETURN_TYPE_UINT32_T, uint32_t);
    /* wchar_t as integer */
    ucase_type(FFI_RETURN_TYPE_WCHAR_T, wchar_t);

    case_type(FFI_RETURN_TYPE_INT64_T,  int64_t,  S64CONVERT);
    case_type(FFI_RETURN_TYPE_UINT64_T, uint64_t, U64CONVERT);

    case_type(FFI_RETURN_TYPE_FLOAT,  float,  FCONVERT);
    case_type(FFI_RETURN_TYPE_DOUBLE, double, FCONVERT);

    case_type(FFI_RETURN_TYPE_INTPTR,  intptr_t,  IPCONVERT);
    case_type(FFI_RETURN_TYPE_UINTPTR, uintptr_t, IPCONVERT);

    case_type(FFI_RETURN_TYPE_STRING,  intptr_t, CSCONVERT);
    case_type(FFI_RETURN_TYPE_POINTER, intptr_t, CSCONVERT);

    case_type(FFI_RETURN_TYPE_WCHAR_STR, intptr_t, WSCONVERT);

    case_type(FFI_RETURN_TYPE_CHAR, char, CHCONVERT);
    case_type(FFI_RETURN_TYPE_WIDE_CHAR, wchar_t, CHCONVERT);
  ret0:
    /* callback will be treated separately */
  case FFI_RETURN_TYPE_CALLBACK:
  case FFI_RETURN_TYPE_VOID    :
    *((intptr_t *)result) = (intptr_t)0;
    return TRUE;
  default:
    Sg_Error(UC("failed to Scheme->C. unknown FFI return type: %d"), type);
    return FALSE;
  }
  return TRUE;

#undef CONVERT
#undef SCONVERT
#undef UCONVERT
#undef BCONVERT
#undef FCONVERT
#undef IPCONVERT
#undef case_type
#undef scase_type
#undef ucase_type
}

static SgObject get_callback_arguments(SgCallback *callback, void **args)
{
  SgObject h = SG_NIL, t = SG_NIL;
  int i;
  
  for (i = 0; i < SG_STRING_SIZE(callback->signatures); i++) {
    SgChar c = SG_STRING_VALUE_AT(callback->signatures, i); 
    switch (c) {
    case FFI_SIGNATURE_BOOL: {	/* bool */
      int8_t arg = *(int8_t *)args[i];
      SG_APPEND1(h, t, arg ? SG_MAKE_INT(1) : SG_MAKE_INT(0));
      break;
    }
    case FFI_SIGNATURE_INT8: case FFI_SIGNATURE_UINT8: { /* byte */
      int8_t arg = *(int8_t *)args[i];
      SG_APPEND1(h, t, SG_MAKE_INT(arg));
      break;
    }
    case FFI_SIGNATURE_INT16: case FFI_SIGNATURE_UINT16: { /* word */
      int16_t arg = *(int16_t *)args[i];
      SG_APPEND1(h, t, SG_MAKE_INT(arg));
      break;
    }
    case FFI_SIGNATURE_INT32: {	/* dword */
      int32_t arg = *(int32_t *)args[i];
      SG_APPEND1(h, t, Sg_MakeInteger(arg));
      break;
    }
    case FFI_SIGNATURE_UINT32: { /* dword */
      int32_t arg = *(int32_t *)args[i];
      SG_APPEND1(h, t, Sg_MakeIntegerU(arg));
      break;
    }
    case FFI_SIGNATURE_INT64: {	 /* qword */
      int64_t arg = *(int64_t *)args[i];
      SG_APPEND1(h, t, Sg_MakeIntegerFromS64(arg));
      break;
    }
    case FFI_SIGNATURE_UINT64: { /* qword */
      int64_t arg = *(int64_t *)args[i];
      SG_APPEND1(h, t, Sg_MakeIntegerFromU64(arg));
      break;
    }
    case FFI_SIGNATURE_FLOAT: {
      float arg = *(float *)args[i];
      SG_APPEND1(h, t, Sg_MakeFlonum((double)arg));
      break;
    }
    case FFI_SIGNATURE_DOUBLE: {
      double arg = *(double *)args[i];
      SG_APPEND1(h, t, Sg_MakeFlonum(arg));
      break;
    }
    case FFI_SIGNATURE_POINTER: {
      void *arg = *(void **)args[i];
      SG_APPEND1(h, t, Sg_MakePointer(arg));
      break;
    }
    case FFI_SIGNATURE_WCHAR_T: {
      wchar_t arg = *(wchar_t *)args[i];
      SG_APPEND1(h, t, Sg_MakeIntegerFromU64(arg));
      break;
    }
    case FFI_SIGNATURE_WCHAR_STR: {
      wchar_t *arg = *(wchar_t **)args[i];
      SG_APPEND1(h, t, Sg_WCharTsToString(arg, wcslen(arg)));
      break;
    }
    case FFI_SIGNATURE_STR: {
      char *arg = *(char **)args[i];
      SG_APPEND1(h, t, Sg_Utf8sToUtf32s(arg, strlen(arg)));
      break;
    }
    case FFI_SIGNATURE_CHAR: {
      int arg = *(int *)args[i];
      SG_APPEND1(h, t, SG_MAKE_CHAR(arg));
      break;
    }
    case FFI_SIGNATURE_WIDE_CHAR: {
      wchar_t arg = *(wchar_t *)args[i];
      SG_APPEND1(h, t, SG_MAKE_CHAR(arg));
      break;
    }
    default:
      Sg_Error(UC("invalid callback argument signature: %c"),
	       SG_STRING_VALUE_AT(callback->signatures, i));
      break;
    }
  }
  return h;
}

static void set_callback_result(SgCallback *callback, SgObject ret,
				ffi_cif *cif, void *result)
{
  switch (callback->returnType) {    
  case FFI_RETURN_TYPE_BOOL:
    /* somehow, on my envionment(X86 windows cygwin) I needed this. why? */
    *((ffi_sarg *) result) = !SG_FALSEP(ret);
    break;
  case FFI_RETURN_TYPE_INT:
  case FFI_RETURN_TYPE_INT8_T:
  case FFI_RETURN_TYPE_INT16_T:
  case FFI_RETURN_TYPE_SHORT:
    if (!SG_NUMBERP(ret)) goto ret0;
    *((ffi_sarg *) result) = SG_INT_VALUE(ret);
    break;
    /* long needs special treatment */
  case FFI_RETURN_TYPE_LONG:
#if SIZEOF_LONG == 4
    /* fall though */
#else
    goto int64_entry;
#endif
  case FFI_RETURN_TYPE_INT32_T:
    if (!SG_NUMBERP(ret)) goto ret0;
    *((ffi_sarg *) result) = Sg_GetIntegerClamp(ret, SG_CLAMP_NONE, NULL);
    break;
  case FFI_RETURN_TYPE_UINT:
  case FFI_RETURN_TYPE_SIZE_T:
  case FFI_RETURN_TYPE_UINT8_T:
  case FFI_RETURN_TYPE_UINT16_T:
  case FFI_RETURN_TYPE_USHORT:
    if (!SG_NUMBERP(ret)) goto ret0;
    *((ffi_arg *) result) = SG_INT_VALUE(ret);
    break;
    /* long needs special treatment */
  case FFI_RETURN_TYPE_ULONG:
#if SIZEOF_LONG == 4
    /* fall though */
#else
    goto uint64_entry;
#endif
  case FFI_RETURN_TYPE_UINT32_T:
    if (!SG_NUMBERP(ret)) goto ret0;
    *((ffi_arg *) result) = Sg_GetUIntegerClamp(ret, SG_CLAMP_NONE, NULL);
    break;
  case FFI_RETURN_TYPE_FLOAT:
    if (!SG_NUMBERP(ret)) goto ret0;
    *((float *) result) = (float)Sg_GetDouble(ret);
    break;
  case FFI_RETURN_TYPE_DOUBLE:
    if (!SG_NUMBERP(ret)) goto ret0;
    *((double *) result) = Sg_GetDouble(ret);
    break;
#if SIZEOF_LONG != 4
  int64_entry:
#endif
  case FFI_RETURN_TYPE_INT64_T:
    if (!SG_NUMBERP(ret)) goto ret0;
    *((int64_t *) result) = Sg_GetIntegerS64Clamp(ret, SG_CLAMP_NONE, NULL);
    break;
#if SIZEOF_LONG != 4
  uint64_entry:
#endif
  case FFI_RETURN_TYPE_UINT64_T:
    if (!SG_NUMBERP(ret)) goto ret0;
    *((uint64_t *) result) = Sg_GetIntegerU64Clamp(ret, SG_CLAMP_NONE, NULL);
    break;
  case FFI_RETURN_TYPE_STRING:
    if (!SG_STRINGP(ret)) goto ret0;
    *((char **) result) = Sg_Utf32sToUtf8s(SG_STRING(ret));
    break;
  case FFI_RETURN_TYPE_WCHAR_T:
#if SIZEOF_WCHAR_T == 2
    if (!SG_INTP(ret)) goto ret0;
    *((wchar_t *) result) = (wchar_t)SG_INT_VALUE(ret);
    break;
#else
    goto uint64_entry;
#endif
  case FFI_RETURN_TYPE_WCHAR_STR:
    if (!SG_STRINGP(ret)) goto ret0;
    *((wchar_t **) result) = Sg_StringToWCharTs(ret);
    break;
  case FFI_RETURN_TYPE_POINTER:
  case FFI_RETURN_TYPE_INTPTR:
  case FFI_RETURN_TYPE_UINTPTR:
    if (SG_EXACT_INTP(ret)) {
      if (SG_BIGNUMP(ret)) {
	*((ffi_arg *) result)
	  = (ffi_arg)Sg_GetIntegerS64Clamp(ret, SG_CLAMP_NONE, NULL);
      } else {
	*((ffi_arg *) result) = SG_INT_VALUE(ret);
      }
      break;
    } else if (SG_POINTERP(ret)) {
      *((ffi_arg *) result) = SG_POINTER(ret)->pointer;
      break;
    }
  case FFI_RETURN_TYPE_CHAR:
    if (!SG_CHARP(ret)) goto ret0;
    *((ffi_arg *)result) = (char)SG_CHAR_VALUE(ret);
    break;
  case FFI_RETURN_TYPE_WIDE_CHAR:
    if (!SG_CHARP(ret)) goto ret0;
    *((wchar_t *)result) = (wchar_t)SG_CHAR_VALUE(ret);
    break;
    /* fall through */
  case FFI_RETURN_TYPE_VOID:
  ret0:
  default:
    *((ffi_arg *) result) = 0;
    break;
  }
}

static void callback_invoker(ffi_cif *cif, void *result, void **args,
			     void *userdata)
{
  SgCallback *callback = SG_CALLBACK(userdata);
  SgObject argv = get_callback_arguments(callback, args), ret;

  ret = Sg_Apply(callback->proc, argv);
  set_callback_result(callback, ret, cif, result);
}

static void** get_fixed_size_ffi_values(SgFuncInfo *func, SgObject args)
{
  SgObject signatures = func->signatures, cp, lastError = SG_FALSE;
  int i, size = Sg_Length(args);
  void **ffi_values;
  /* check if the argument count is correct */
  if (size != func->argc) {
    Sg_Error(UC("argument count is not correct. required %d, but got %d"),
	     func->argc, size);
    return NULL;
  }
    
  ffi_values = SG_NEW_ARRAY(void *, func->argc);
    
  i = 0;
  SG_FOR_EACH(cp, args) {
    ffi_storage *param = SG_NEW(ffi_storage);
    if (!push_ffi_type_value(func,
			     SG_STRING_VALUE_AT(signatures, i),
			     SG_CAR(cp),
			     param,
			     &lastError)) {
      Sg_Error(UC("argument error %A on index %d[%S]: %S"), func, i,
	       SG_CAR(cp), lastError);
      return NULL;
    }
    ffi_values[i++] = param;
  }
  return ffi_values;
}

static void set_ffi_varargs_parameter_types(SgObject oargs, int startIndex,
					    ffi_type **types)
{
  /* for now we only suppots upto pointer size arguments */
  int i;
  SgObject args = oargs;
  /* we know it has at least until start index */
  for (i = 0; i < startIndex; i++) args = SG_CDR(args);
  /* accessing va_list must be pointer boundary, so the FFI type
     of the arguments must be pointer.
   */
  SG_FOR_EACH(args, args) {
    types[i++] = &ffi_type_pointer;
  }
}

static int push_varargs_ffi_type_value(SgFuncInfo *func, SgObject arg,
				       ffi_storage *storage,
				       SgObject *lastError)
{
  if (SG_BOOLP(arg)) {
    storage->sl = SG_TRUEP(arg) ? 1L : 0L;
  } else if (SG_INTP(arg)) {
    storage->sl = SG_INT_VALUE(arg);
  } else if (SG_POINTERP(arg)) {
    storage->ptr = (void*)SG_POINTER(arg)->pointer;
  } else if (SG_BVECTORP(arg)) {
    storage->ptr = (void*)(SG_BVECTOR_ELEMENTS(arg));
  } else if (SG_STRINGP(arg)) {
    storage->ptr = (void*)(Sg_Utf32sToUtf8s(SG_STRING(arg)));
  } else if (SG_CALLBACKP(arg)) {
    if (!prep_method_handler(SG_CALLBACK(arg))) {
      *lastError = Sg_Sprintf(UC("failed to prepare the callback."));
      return FALSE;
    }
    storage->ptr = SG_CALLBACK(arg)->code;
  } else if (SG_FLONUMP(arg)) {
#if defined(__cplusplus) && defined(USE_IMMEDIATE_FLONUM)
      storage->f64 = Sg_FlonumValue(arg);
#else
      storage->f64 = SG_FLONUM_VALUE(arg);
#endif
  } else {
    *lastError = Sg_Sprintf(UC("non supported variable length arguments %S"),
			    arg);
    return FALSE;
  }
  return TRUE;
}

static void** get_varargs_ffi_values(SgFuncInfo *func, SgObject args)
{

  SgObject signatures = func->signatures, cp, lastError = SG_FALSE;
  int i, size = Sg_Length(args), index;
  void **ffi_values;
    
  ffi_values = SG_NEW_ARRAY(void *, size);
    
  func->argc = size;
  func->parameterTypes = SG_NEW_ARRAY(ffi_type*, size);
    
  index = set_ffi_parameter_types(signatures, func->parameterTypes);
  set_ffi_varargs_parameter_types(args, index, func->parameterTypes);
  /* initialize ffi_cif */
#ifdef HAVE_FFI_PREP_CIF_VAR
  if (ffi_prep_cif_var(&func->cif, FFI_DEFAULT_ABI, 
		       index,	/* required argc */
		       func->argc, /* total */
		       func->returnType,
		       func->parameterTypes) != FFI_OK) {
    Sg_Error(UC("VARARGS FFI initialization failed."));
    return NULL;
  }
#else
  /* is this ok? */
  if (ffi_prep_cif(&func->cif, FFI_DEFAULT_ABI, 
		   func->argc,
		   func->returnType,
		   func->parameterTypes) != FFI_OK) {
    Sg_Error(UC("VARARGS FFI initialization failed."));
    return NULL;
  }
#endif
  i = 0;
  index = 0;
  SG_FOR_EACH(cp, args) {
    ffi_storage *param = SG_NEW(ffi_storage);
    if (SG_STRING_VALUE_AT(signatures, index) == FFI_SIGNATURE_VARGS) {
      if (!push_varargs_ffi_type_value(func,
				       SG_CAR(cp),
				       param,
				       &lastError)) {
	Sg_Error(UC("argument error %A on index %d: %S"), func, i, lastError);
	return NULL;
      }
    } else {
      if (!push_ffi_type_value(func,
			       SG_STRING_VALUE_AT(signatures, index),
			       SG_CAR(cp),
			       param,
			       &lastError)) {
	Sg_Error(UC("argument error %A on index %d: %S"), func, i, lastError);
	return NULL;
      }
      index++;
    }
    ffi_values[i++] = param;
  }
  return ffi_values;
}

static SgObject internal_ffi_call(SgObject *args, int argc, void *data)
{
  int retType;
  SgFuncInfo *func;
  void **ffi_values;

#ifdef FFI_NOT_SUPPORTED
  Sg_Error(UC("ffi not supported on this architecture"));
  return SG_UNDEF;
#endif

  if (argc < 2) {
    Sg_WrongNumberOfArgumentsAtLeastViolation(SG_INTERN("%ffi-call"),
					      2, argc, SG_NIL);
  }
  if (!SG_INTP(args[0])) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("%ffi-call"),
				    SG_MAKE_STRING("fixnum"), args[0], SG_NIL);
  }
  if (!SG_FUNC_INFO_P(args[1])) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("%ffi-call"),
				    SG_MAKE_STRING("func-info"),
				    args[0], SG_NIL);
  }
  retType = SG_INT_VALUE(args[0]);
  func = SG_FUNC_INFO(args[1]);

  if (func->initialized) {
    ffi_values = get_fixed_size_ffi_values(func, args[argc-1]);
  } else {
    ffi_values = get_varargs_ffi_values(func, args[argc-1]);
  }
  if (!ffi_values) return SG_UNDEF; /* in case */

  /* sanity check */
  if (!func->code) {
    Sg_Error(UC("invalid c-function %S"), func);
    return SG_UNDEF;
  }
#define UNDEF_CONV(type) SG_UNDEF
#define BOOL_CONV(type)  SG_MAKE_BOOL(ret != FALSE)
#define SINT_CONV(type)  SG_MAKE_INT((type)ret)
#define INT_CONV(type)   Sg_MakeInteger((type)ret)
#define UINT_CONV(type)  Sg_MakeIntegerU((type)ret)
#if SIZEOF_VOIDP == 4
#define INTPTR_CONV(type)  Sg_MakeInteger((type)ret)
#define UINTPTR_CONV(type) Sg_MakeIntegerU((type)ret)
#else
#define INTPTR_CONV(type)  Sg_MakeIntegerFromS64((type)ret)
#define UINTPTR_CONV(type) Sg_MakeIntegerFromU64((type)ret)
#endif
#define FLONUM_CONV(type)  Sg_MakeFlonum((type)ret)
#define S64_CONV(type)     Sg_MakeIntegerFromS64((type)ret)
#define U64_CONV(type)     Sg_MakeIntegerFromU64((type)ret)
#define PTR_CONV(type)     make_pointer((type)ret)
#define CHAR_CONV(type)    SG_MAKE_CHAR((type)ret);
#define WSTR_CONV(type)    Sg_WCharTsToString((type)ret, wcslen((type)ret))

#define FFI_RET_CASE_REC(type, rettype, return_body)			\
  case type: {								\
    rettype ret;							\
    ffi_call(&func->cif, FFI_FN(func->code), &ret, (void **)ffi_values); \
    return_body;							\
  } break

#define FFI_RET_CASE4(type, conv, rettype, ctype)		\
  FFI_RET_CASE_REC(type, rettype, return conv(ctype))

#define FFI_RET_CASE(type, conv, ctype)		\
  FFI_RET_CASE4(type, conv, intptr_t, ctype)

  switch (retType) {
    FFI_RET_CASE(FFI_RETURN_TYPE_VOID,   UNDEF_CONV, void);
    FFI_RET_CASE(FFI_RETURN_TYPE_BOOL,   BOOL_CONV, bool);
    FFI_RET_CASE(FFI_RETURN_TYPE_SHORT,  SINT_CONV, short);
    FFI_RET_CASE(FFI_RETURN_TYPE_INT,    INT_CONV, int);
    FFI_RET_CASE(FFI_RETURN_TYPE_LONG,   INT_CONV, long);
    FFI_RET_CASE(FFI_RETURN_TYPE_INTPTR, INTPTR_CONV, intptr_t);

    FFI_RET_CASE(FFI_RETURN_TYPE_USHORT,  SINT_CONV, unsigned short);
    FFI_RET_CASE(FFI_RETURN_TYPE_UINT,    UINT_CONV, unsigned int);
    FFI_RET_CASE(FFI_RETURN_TYPE_ULONG,   UINT_CONV, unsigned long);
    FFI_RET_CASE(FFI_RETURN_TYPE_WCHAR_T, UINT_CONV, wchar_t);
    FFI_RET_CASE(FFI_RETURN_TYPE_UINTPTR, UINTPTR_CONV, uintptr_t);

    FFI_RET_CASE(FFI_RETURN_TYPE_SIZE_T,  UINTPTR_CONV, size_t);

    FFI_RET_CASE(FFI_RETURN_TYPE_INT8_T,   SINT_CONV, int8_t);
    FFI_RET_CASE(FFI_RETURN_TYPE_UINT8_T,  SINT_CONV, uint8_t);
    FFI_RET_CASE(FFI_RETURN_TYPE_INT16_T,  SINT_CONV, int16_t);
    FFI_RET_CASE(FFI_RETURN_TYPE_UINT16_T, SINT_CONV, uint16_t);
    FFI_RET_CASE(FFI_RETURN_TYPE_INT32_T,  INT_CONV,  int32_t);
    FFI_RET_CASE(FFI_RETURN_TYPE_UINT32_T, UINT_CONV, uint32_t);

    FFI_RET_CASE4(FFI_RETURN_TYPE_FLOAT,  FLONUM_CONV, float, double);
    FFI_RET_CASE4(FFI_RETURN_TYPE_DOUBLE, FLONUM_CONV, double, double);

    FFI_RET_CASE4(FFI_RETURN_TYPE_INT64_T,  S64_CONV, int64_t, int64_t);
    FFI_RET_CASE4(FFI_RETURN_TYPE_UINT64_T, U64_CONV, uint64_t, uint64_t);

    FFI_RET_CASE(FFI_RETURN_TYPE_POINTER,   PTR_CONV, intptr_t);
    FFI_RET_CASE(FFI_RETURN_TYPE_WCHAR_STR, WSTR_CONV, wchar_t*);

    FFI_RET_CASE_REC(FFI_RETURN_TYPE_STRING, intptr_t,
		     if (ret == 0) {
		       return make_pointer((uintptr_t)NULL);
		     } else {
		       return Sg_Utf8sToUtf32s((char *)ret,
					       strlen((char *)ret));
		     });

    FFI_RET_CASE_REC(FFI_RETURN_TYPE_CALLBACK, intptr_t,
		     return Sg_HashTableRef(callbacks, SG_OBJ(ret), SG_FALSE));
    FFI_RET_CASE(FFI_RETURN_TYPE_CHAR, CHAR_CONV, char);
    FFI_RET_CASE(FFI_RETURN_TYPE_WIDE_CHAR, CHAR_CONV, wchar_t);
  default:
    Sg_AssertionViolation(SG_INTERN("c-function"),
			  SG_MAKE_STRING("invalid return type"),
			  SG_LIST1(args[0]));
    return SG_UNDEF;
  }
}

static SG_DEFINE_SUBR(internal_ffi_call_stub, 2, 1, internal_ffi_call,
		      SG_FALSE, NULL);

/* not used */
#if 0
static void attached_method_invoker(ffi_cif *cif, void *result,
				    void **args, void *userdata)
{
  SgCallback *callback = SG_CALLBACK(userdata);
#if 0
  SgObject *argv = *(SgObject **)args[0];
  int argc = *(int *)args[1];
  void *data = *(void **)args[2];
#endif
  SgObject argv = get_callback_arguments(callback, args), ret;
  ret = Sg_Apply(callback->proc, argv);
  set_callback_result(callback, ret, cif, result);
}
#endif

static int prep_method_handler(SgCallback *callback)
{
  if (callback->parameterTypes) return TRUE;
  else {
    ffi_status status;
    SgString *sig = callback->signatures;
    ffi_type **params = SG_NEW_ARRAY(ffi_type *, SG_STRING_SIZE(sig));
    ffi_type *ret = lookup_ffi_return_type(callback->returnType);
    set_ffi_callback_parameter_types(sig, params);
    ffi_prep_cif(&callback->cif, FFI_DEFAULT_ABI, SG_STRING_SIZE(sig),
		 ret, params);
    status = ffi_prep_closure_loc(callback->closure,
				  &callback->cif,
				  callback_invoker, callback,
				  callback->code);
    callback->parameterTypes = params;
    return status == FFI_OK;
  }
}

/* utility */
#define DEFINE_POINTER_SET(ft, t)				\
  static void pointer_set_##ft(SgPointer *p, int offset,	\
			      int type, SgObject v)		\
  {								\
    t result;							\
    convert_scheme_to_c_value(v, type, (void**)&result);	\
    POINTER_SET(t, p, offset, result);				\
  }
DEFINE_POINTER_SET(FFI_RETURN_TYPE_SHORT   , short);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_INT     , int);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_LONG    , long);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_INTPTR  , intptr_t);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_USHORT  , unsigned short);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_UINT    , unsigned int);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_ULONG   , unsigned long);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_UINTPTR , uintptr_t);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_FLOAT   , float);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_DOUBLE  , double);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_SIZE_T  , size_t);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_INT8_T  , int8_t);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_UINT8_T , uint8_t);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_INT16_T , int16_t);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_UINT16_T, uint16_t);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_INT32_T , int32_t);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_UINT32_T, uint32_t);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_INT64_T , int64_t);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_WCHAR_T,  wchar_t);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_UINT64_T, uint64_t);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_STRING  , intptr_t);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_POINTER , void*);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_STRUCT  , void*);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_WCHAR_STR, void*);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_CHAR     , char);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_WIDE_CHAR, wchar_t);

void Sg_PointerSet(SgPointer *p, int offset, int type, SgObject v)
{
  if (!p->pointer) {
    Sg_AssertionViolation(SG_INTERN("pointer-set!"),
			  SG_MAKE_STRING("got null pointer"),
			  SG_LIST2(p, v));
  }
#define case_type(ft) case ft: pointer_set_##ft(p, offset, type, v); break;
  switch (type) {
    case_type(FFI_RETURN_TYPE_SHORT    );
    case_type(FFI_RETURN_TYPE_INT      );
    case_type(FFI_RETURN_TYPE_LONG     );
    case_type(FFI_RETURN_TYPE_INTPTR   );
    case_type(FFI_RETURN_TYPE_USHORT   );
    case_type(FFI_RETURN_TYPE_UINT     );
    case_type(FFI_RETURN_TYPE_ULONG    );
    case_type(FFI_RETURN_TYPE_UINTPTR  );
    case_type(FFI_RETURN_TYPE_FLOAT    );
    case_type(FFI_RETURN_TYPE_DOUBLE   );
    case_type(FFI_RETURN_TYPE_SIZE_T   );
    case_type(FFI_RETURN_TYPE_INT8_T   );
    case_type(FFI_RETURN_TYPE_UINT8_T  );
    case_type(FFI_RETURN_TYPE_INT16_T  );
    case_type(FFI_RETURN_TYPE_UINT16_T );
    case_type(FFI_RETURN_TYPE_INT32_T  );
    case_type(FFI_RETURN_TYPE_UINT32_T );
    case_type(FFI_RETURN_TYPE_INT64_T  );
    case_type(FFI_RETURN_TYPE_UINT64_T );
    case_type(FFI_RETURN_TYPE_WCHAR_T  );
    case_type(FFI_RETURN_TYPE_STRING   );
    case_type(FFI_RETURN_TYPE_POINTER  );
    case_type(FFI_RETURN_TYPE_STRUCT   );
    case_type(FFI_RETURN_TYPE_WCHAR_STR);
    case_type(FFI_RETURN_TYPE_CHAR     );
    case_type(FFI_RETURN_TYPE_WIDE_CHAR);
  case FFI_RETURN_TYPE_CALLBACK: {
    if (!SG_CALLBACKP(v)) Sg_Error(UC("callback required, but got %S "), v);
    if (prep_method_handler(SG_CALLBACK(v))) {
      POINTER_SET(void*, p, offset, SG_CALLBACK(v)->code);
      Sg_HashTableSet(callbacks, SG_CALLBACK(v)->code, v, 0);
    }
    break;
  }
  default:
    Sg_Error(UC("invalid type %d"), type);
  }
}

SgObject Sg_CMalloc(size_t size)
{
  void *real = malloc(size);
  SgPointer *p = make_pointer((uintptr_t)real);
  return SG_OBJ(p);
}

void Sg_CFree(SgPointer *p)
{
  free((void*)p->pointer);
  p->pointer = (uintptr_t)NULL;
}

void Sg_CMemcpy(SgPointer *d, long offset, 
		SgObject   s, long start,
		size_t size)
{
  void *dst = (void *)(d->pointer + offset);
  void *src = NULL;

  if (SG_POINTERP(s)) {
    src = (void *)(SG_POINTER(s)->pointer + start);
  } else if (SG_BVECTORP(s)) {
    src = (void *)(SG_BVECTOR_ELEMENTS(s) + start);
  } else {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("c-memcpy"),
				    SG_MAKE_STRING("pointer or bytevector"),
				    s, SG_NIL);
    return;
  }
  memcpy(dst, src, size);
}

static void invoke_finalizer(SgObject obj, void *data)
{
  Sg_Apply1(SG_OBJ(data), obj);
}

SgObject Sg_RegisterFFIFinalizer(SgPointer *pointer, SgObject proc)
{
  Sg_RegisterFinalizer(SG_OBJ(pointer), invoke_finalizer, proc);
  return SG_OBJ(pointer);
}

SgObject Sg_UnregisterFFIFinalizer(SgPointer *pointer)
{
  Sg_UnregisterFinalizer(SG_OBJ(pointer));
  return SG_OBJ(pointer);
}

extern void Sg__Init_ffi_stub(SgLibrary *lib);

SG_EXTENSION_ENTRY void CDECL Sg_Init_sagittarius__ffi()
{
  SgLibrary *lib;
  SgSymbol *name = SG_INTERN("%ffi-call");

  /* Sg_AddCondFeature(UC("sagittarius.ffi")); */
  SG_PROCEDURE_NAME(&internal_ffi_call_stub) = name;

  SYMBOL_STRUCT = SG_INTERN("struct");
  SYMBOL_BIT_FIELD = SG_INTERN("bit-field");
  address_mark = SG_INTERN("address");

  SG_INIT_EXTENSION(sagittarius__ffi);
  /* init impl library first */
  lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius ffi)"), FALSE));
  Sg__Init_ffi_stub(lib);
  Sg_InsertBinding(lib, name, &internal_ffi_call_stub);
  impl_lib = lib;
  /* callback storage */
  callbacks = SG_HASHTABLE(Sg_MakeHashTableSimple(SG_HASH_EQ, 0));
  /* ref_table = SG_HASHTABLE(Sg_MakeHashTableSimple(SG_HASH_EQ, 0)); */

  Sg_InitStaticClassWithMeta(SG_CLASS_POINTER, UC("<pointer>"), lib, NULL,
			     SG_FALSE, pointer_slots, 0);
  Sg_InitStaticClassWithMeta(SG_CLASS_FUNC_INFO, UC("<function-info>"),
			     lib, NULL, SG_FALSE, NULL, 0);
  Sg_InitStaticClassWithMeta(SG_CLASS_CALLBACK, UC("<callback>"),
			     lib, NULL, SG_FALSE, NULL, 0);
  Sg_InitStaticClassWithMeta(SG_CLASS_CSTRUCT, UC("<c-struct>"),
			     lib, NULL, SG_FALSE, NULL, 0);

#define CONST_VALUE(name, v)					\
  Sg_MakeBinding(lib, SG_INTERN(#name), SG_MAKE_INT(v), TRUE)
#define SIZE_VALUE(name) CONST_VALUE(size-of-name, sizeof(name))
  /* bool is not C type */
  CONST_VALUE(size-of-bool, sizeof(char));
  SIZE_VALUE(char);
  SIZE_VALUE(short);
  CONST_VALUE(size-of-unsigned-short, sizeof(unsigned short));
  SIZE_VALUE(int);
  CONST_VALUE(size-of-unsigned-int, sizeof(unsigned int));
  SIZE_VALUE(long);
  CONST_VALUE(size-of-unsigned-long, sizeof(unsigned long));
  CONST_VALUE(size-of-long-long, sizeof(long long));
  CONST_VALUE(size-of-unsigned-long-long, sizeof(unsigned long long));
  SIZE_VALUE(void*);
  SIZE_VALUE(size_t);
  SIZE_VALUE(float);
  SIZE_VALUE(double);
  SIZE_VALUE(int8_t);
  SIZE_VALUE(uint8_t);
  SIZE_VALUE(int16_t);
  SIZE_VALUE(uint16_t);
  SIZE_VALUE(int32_t);
  SIZE_VALUE(uint32_t);
  SIZE_VALUE(int64_t);
  SIZE_VALUE(uint64_t);
  SIZE_VALUE(intptr_t);
  SIZE_VALUE(uintptr_t);
  SIZE_VALUE(wchar_t);

#define ALIGN_OF2(name, type)						\
  do {									\
    struct x { char y; type z; };					\
    CONST_VALUE(name, offsetof(struct x, z));				\
  } while (0)

#define ALIGN_OF(type) ALIGN_OF2(align-of-type, type)
  
  ALIGN_OF2(align-of-bool, char);
  ALIGN_OF(char);
  ALIGN_OF(short);
  ALIGN_OF2(align-of-unsigned-short, unsigned short);
  ALIGN_OF(int);
  ALIGN_OF2(align-of-unsigned-int, unsigned int);
  ALIGN_OF(long);
  ALIGN_OF2(align-of-unsigned-long, unsigned long);
  ALIGN_OF2(align-of-long-long, long long);
  ALIGN_OF2(align-of-unsigned-long-long, unsigned long long);
  ALIGN_OF(void*);
  ALIGN_OF(size_t);
  ALIGN_OF(float);
  ALIGN_OF(double);
  ALIGN_OF(int8_t);
  ALIGN_OF(int16_t);
  ALIGN_OF(int32_t);
  ALIGN_OF(int64_t);
  ALIGN_OF(uint8_t);
  ALIGN_OF(uint16_t);
  ALIGN_OF(uint32_t);
  ALIGN_OF(uint64_t);
  ALIGN_OF(intptr_t);
  ALIGN_OF(uintptr_t);
  ALIGN_OF(wchar_t);

#define CONST_VALUE1(v) CONST_VALUE(v, v)

  CONST_VALUE1(FFI_RETURN_TYPE_VOID);
  CONST_VALUE1(FFI_RETURN_TYPE_BOOL);
  CONST_VALUE1(FFI_RETURN_TYPE_SHORT);
  CONST_VALUE1(FFI_RETURN_TYPE_INT);
  CONST_VALUE1(FFI_RETURN_TYPE_LONG);
  CONST_VALUE1(FFI_RETURN_TYPE_INTPTR);
  CONST_VALUE1(FFI_RETURN_TYPE_USHORT);
  CONST_VALUE1(FFI_RETURN_TYPE_UINT);
  CONST_VALUE1(FFI_RETURN_TYPE_ULONG);
  CONST_VALUE1(FFI_RETURN_TYPE_UINTPTR);
  CONST_VALUE1(FFI_RETURN_TYPE_FLOAT);
  CONST_VALUE1(FFI_RETURN_TYPE_DOUBLE);
  CONST_VALUE1(FFI_RETURN_TYPE_STRING);
  CONST_VALUE1(FFI_RETURN_TYPE_SIZE_T);
  CONST_VALUE1(FFI_RETURN_TYPE_INT8_T);
  CONST_VALUE1(FFI_RETURN_TYPE_UINT8_T);
  CONST_VALUE1(FFI_RETURN_TYPE_INT16_T);
  CONST_VALUE1(FFI_RETURN_TYPE_UINT16_T);
  CONST_VALUE1(FFI_RETURN_TYPE_INT32_T);
  CONST_VALUE1(FFI_RETURN_TYPE_UINT32_T);
  CONST_VALUE1(FFI_RETURN_TYPE_INT64_T);
  CONST_VALUE1(FFI_RETURN_TYPE_UINT64_T);
  CONST_VALUE1(FFI_RETURN_TYPE_POINTER);
  CONST_VALUE1(FFI_RETURN_TYPE_STRUCT);
  CONST_VALUE1(FFI_RETURN_TYPE_CALLBACK);
  CONST_VALUE1(FFI_RETURN_TYPE_WCHAR_T);
  CONST_VALUE1(FFI_RETURN_TYPE_WCHAR_STR);
  CONST_VALUE1(FFI_RETURN_TYPE_CHAR);
  CONST_VALUE1(FFI_RETURN_TYPE_WIDE_CHAR);

#define CONST_CHAR(name)						\
  Sg_MakeBinding(lib, SG_INTERN(#name), SG_MAKE_CHAR(name), TRUE)

  CONST_CHAR(FFI_SIGNATURE_INT8     );
  CONST_CHAR(FFI_SIGNATURE_UINT8    );
  CONST_CHAR(FFI_SIGNATURE_INT16    );
  CONST_CHAR(FFI_SIGNATURE_UINT16   );
  CONST_CHAR(FFI_SIGNATURE_INT32    );
  CONST_CHAR(FFI_SIGNATURE_UINT32   );
  CONST_CHAR(FFI_SIGNATURE_INT64    );
  CONST_CHAR(FFI_SIGNATURE_UINT64   );
  CONST_CHAR(FFI_SIGNATURE_FLOAT    );
  CONST_CHAR(FFI_SIGNATURE_DOUBLE   );
  CONST_CHAR(FFI_SIGNATURE_BOOL     );
  CONST_CHAR(FFI_SIGNATURE_WCHAR_T  );
  CONST_CHAR(FFI_SIGNATURE_STR      );
  CONST_CHAR(FFI_SIGNATURE_WCHAR_STR);
  CONST_CHAR(FFI_SIGNATURE_POINTER  );
  CONST_CHAR(FFI_SIGNATURE_CALLBACK );
  CONST_CHAR(FFI_SIGNATURE_VARGS    );
  CONST_CHAR(FFI_SIGNATURE_CHAR     );
  CONST_CHAR(FFI_SIGNATURE_WIDE_CHAR);

#undef CONST_VALUE
#undef SIZE_VALUE
#undef ALIGN_OF
}
