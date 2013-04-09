/* -*- C -*- */
/*
 * ffi.c
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
#include <string.h>
#include <errno.h>
#include <sagittarius.h>
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

/* function info */
static void funcinfo_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<c-function %A (*)%A>"),
	    SG_FUNC_INFO(self)->sReturnType, 
	    SG_FUNC_INFO(self)->sParameterTypes);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_FuncInfoClass, funcinfo_printer);

static ffi_type* lookup_ffi_return_type(int rettype);

#if 0
static void funcinfo_finalize(SgObject obj, void *data)
{
  int i;
  for (i = 0; i < SG_FUNC_INFO(obj)->closureCount; i++) {
    ffi_closure_free(SG_FUNC_INFO(obj)->closures[i]);
  }
}
#endif

static int set_ffi_parameter_types(SgObject signatures, ffi_type **types)
{
  const SgChar *sigs;
  int i, callback = 0;
  sigs = SG_STRING_VALUE(signatures);
  for (i = 0; i < SG_STRING_SIZE(signatures); i++) {
    switch (sigs[i]) {
    case FFI_SIGNATURE_BOOL:
      types[i] = &ffi_type_sint; break;
    case FFI_SIGNATURE_INT:
      types[i] = &ffi_type_slong; break;
    case FFI_SIGNATURE_UINT:
      types[i] = &ffi_type_ulong; break;
    case FFI_SIGNATURE_FLOAT:
      types[i] = &ffi_type_float; break;
    case FFI_SIGNATURE_DOUBLE:
      types[i] = &ffi_type_double; break;
    case FFI_SIGNATURE_INT64:
      types[i] = &ffi_type_sint64; break;
    case FFI_SIGNATURE_UINT64:
      types[i] = &ffi_type_uint64; break;
    case FFI_SIGNATURE_CALLBACK:
      callback++;
    case FFI_SIGNATURE_POINTER:
      types[i] = &ffi_type_pointer; break;
    default:
      Sg_Error(UC("invalid signature %c"), sigs[i]);
      return -1;		/* dummy */
    }
  }
  return callback;
}

static SgFuncInfo* make_funcinfo(uintptr_t proc, int retType, 
				 SgObject signatures)
{
  SgFuncInfo *fn = SG_NEW(SgFuncInfo);
  int callback = 0;
  SG_SET_CLASS(fn, SG_CLASS_FUNC_INFO);
  /* signatures must be created in Scheme file */
  ASSERT(SG_STRINGP(signatures));
  fn->argc = SG_STRING_SIZE(signatures);
  fn->parameterTypes = SG_NEW_ARRAY(ffi_type*, fn->argc);
  fn->signatures = signatures;
  fn->code = proc;

  callback = set_ffi_parameter_types(signatures, fn->parameterTypes);
  /* initialize ffi_cif */
  if (ffi_prep_cif(&fn->cif, FFI_DEFAULT_ABI, fn->argc,
		   lookup_ffi_return_type(retType), 
		   fn->parameterTypes) != FFI_OK) {
    Sg_Error(UC("FFI initialization failed."));
    return SG_FUNC_INFO(SG_UNDEF);
  }

  fn->closureCount = callback;
#if 0
  if (callback) {
    fn->closures = SG_NEW_ARRAY(ffi_closure, callback);
    fn->closurelocs = SG_NEW_ARRAY(void, callback);
    for (i = 0; i < callback; i++) {
      fn->closures[i] = (ffi_closure*)ffi_closure_alloc(sizeof(ffi_closure),
							&fn->closurelocs[i]);
    }
    Sg_RegisterFinalizer(SG_OBJ(fn), funcinfo_finalize, NULL);
  }
#endif
  return fn;
}

SgObject Sg_CreateCFunction(SgPointer *handle, int rettype, 
			    SgObject sigs, SgObject sret, SgObject sparam)
{
  SgFuncInfo *fn;
  if (!handle->pointer) {
    Sg_Error(UC("invalid c-function address %S"), handle);
    return SG_UNDEF;
  }
  fn = make_funcinfo(handle->pointer, rettype, sigs);
  fn->sReturnType = sret;
  fn->sParameterTypes = sparam;
  return SG_OBJ(fn);
}

/* callback */
static void callback_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<c-callback %A>"), SG_CALLBACK(self)->signatures);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_CallbackClass, callback_printer);

static uintptr_t global_uid = 0;
static SgHashTable *ctable;

static void callback_invoker(ffi_cif *cif, void *result, void **args,
			     void *userdata);
static void release_callback(SgCallback *callback)
{
  Sg_HashTableDelete(ctable, SG_MAKE_INT(callback->uid));
  ffi_closure_free(callback->closure);
}
static void callback_finalize(SgObject callback, void *data)
{
  release_callback(SG_CALLBACK(callback));
}

SgObject Sg_CreateCallback(int rettype, SgString *signatures, SgObject proc)
{
  SgCallback *c = SG_NEW(SgCallback);
  uintptr_t uid = global_uid++;
  SG_SET_CLASS(c, SG_CLASS_CALLBACK);
  c->returnType = rettype;
  c->signatures = signatures;
  c->proc = proc;
  c->uid = uid;
  c->closure = (ffi_closure*)ffi_closure_alloc(sizeof(ffi_closure), &c->code);
  /* store callback to static area to avoid GC. */
  Sg_HashTableSet(ctable, SG_MAKE_INT(uid), SG_OBJ(c), 0);
  Sg_RegisterFinalizer(SG_OBJ(c), callback_finalize, NULL);
  return SG_OBJ(c);
}

void Sg_ReleaseCallback(SgCallback *callback)
{
  release_callback(callback);
  Sg_UnregisterFinalizer(SG_OBJ(callback));
}

/* cstruct */
static void cstruct_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<c-struct %A %d>"), 
	    SG_CSTRUCT(self)->name, SG_CSTRUCT(self)->size);
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
static SgLibrary *impl_lib = NULL;

SgObject Sg_CreateCStruct(SgObject name, SgObject layouts)
{
  SgCStruct *st;
  SgObject cp;
  SgVM *vm = Sg_VM();
  int index = 0;
  size_t size;
  if (!SG_LISTP(layouts)) {
    Sg_Error(UC("list required but got %S"), layouts);
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
   */
  size = 0;
  SG_FOR_EACH(cp, layouts) {
    SgObject layout = SG_CAR(cp);
    SgObject typename;
    int type;
    ffi_type *ffi;
    ASSERT(SG_INTP(SG_CADR(layout)));

    st->layouts[index].name = SG_CAR(layout);
    st->layouts[index].array = -1;
    if (SG_PAIRP(SG_CDDR(layout))) {
      if (SG_EQ(SYMBOL_STRUCT, SG_CAR(SG_CDDR(layout)))) {
	SgObject gloc;
	SgObject st2;
	typename = SG_CDR(SG_CDDR(layout));
	/* FIXME: this should not be like this */
	gloc = Sg_FindBinding(vm->currentLibrary, typename, SG_FALSE);
	if (SG_FALSEP(gloc)) {
	  Sg_Error(UC("undefined c-struct %S"), typename);
	  return SG_UNDEF;
	}
	st2 = SG_GLOC_GET(SG_GLOC(gloc));
	if (!SG_CSTRUCTP(st2)) {
	  Sg_Error(UC("c-struct required, but got %S"), st2);
	  return SG_UNDEF;
	}
	st->type.elements[index] = &SG_CSTRUCT(st2)->type;
	st->layouts[index].type = &SG_CSTRUCT(st2)->type;
	st->layouts[index].cstruct = SG_CSTRUCT(st2);
	st->layouts[index].tag = FFI_RETURN_TYPE_STRUCT;
	size += SG_CSTRUCT(st2)->size;
      } else if (SG_INTP(SG_CAR(SG_CDDR(layout)))) {
	SgObject size_of, gloc;
	int asize = SG_INT_VALUE(SG_CAR(SG_CDDR(layout)));
	type = SG_INT_VALUE(SG_CADR(layout));
	ffi = lookup_ffi_return_type(type);
	size_of = Sg_Sprintf(UC("size-of-%A"), SG_CDR(SG_CDDR(layout)));
	gloc = Sg_FindBinding(impl_lib, Sg_Intern(size_of), SG_FALSE);
	if (SG_FALSEP(gloc)) {
	  Sg_Error(UC("struct layout contains invalid array type %S"), layouts);
	}
	asize = asize * SG_INT_VALUE(SG_GLOC_GET(SG_GLOC(gloc))) - SG_INT_VALUE(SG_GLOC_GET(SG_GLOC(gloc)));
	size += asize;
	goto primitive_type;
      } else {
	Sg_Error(UC("invalid struct layout %S"), layouts);
      }
    } else {
      type = SG_INT_VALUE(SG_CADR(layout));
      ffi = lookup_ffi_return_type(type);
    primitive_type:
      st->type.elements[index] = ffi;
      st->layouts[index].type = ffi;
      st->layouts[index].cstruct = NULL;
      st->layouts[index].tag = type;
      size += ffi->alignment;
    }
    index++;
  }
  st->size = size;		/* alignment */
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

static size_t calculate_alignment(SgObject names, SgCStruct *st,
				  int *foundP, int *type)
{
  size_t align = 0;
  SgObject name = SG_CAR(names);
  size_t size = st->fieldCount, i;
  struct_layout_t *layouts = st->layouts;

  /* names are list of property name for struct.
     name        => (name)
     name1.name2 => (name1 name2)
   */
  for (i = 0; i < size; i++) {
    /* property found */
    if (SG_EQ(name, layouts[i].name)) {
      /* it's this one */
      if (!layouts[i].cstruct && SG_NULLP(SG_CDR(names))) {
	*foundP = TRUE;
	*type = layouts[i].tag;
	return align;
      /* property was struct */
      } else if (layouts[i].cstruct) {
	/* struct in struct */
	if (SG_NULLP(SG_CDR(names))) {
	  align += layouts[i].cstruct->size;
	} else {
	  /* search from here */
	  int foundP2 = FALSE, type2;
	  align += calculate_alignment(SG_CDR(names),
				       layouts[i].cstruct,
				       &foundP2,
				       &type2);
	  /* second name was a member of the struct */
	  if (foundP2) {
	    *foundP = TRUE;
	    *type = type2;
	    return align;
	  }
	}
      }
    } else {
      align += layouts[i].type->alignment;
      if (layouts[i].array != -1) {
	align += layouts[i].array;
      }
    }
  }
  return align;
}

static SgHashTable *ref_table;

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
    return Sg_Utf8sToUtf32s(s, (int)strlen(s));
  }
  case FFI_RETURN_TYPE_INT64_T :
    return Sg_MakeIntegerFromS64(POINTER_REF(int64_t, p, align));
  case FFI_RETURN_TYPE_UINT64_T:
    return Sg_MakeIntegerFromU64(POINTER_REF(uint64_t, p, align));
  case FFI_RETURN_TYPE_POINTER :
  case FFI_RETURN_TYPE_STRUCT  :
    return make_pointer(POINTER_REF(uintptr_t, p, align));
  case FFI_RETURN_TYPE_CALLBACK:
    return Sg_HashTableRef(ref_table, (POINTER_REF(void*, p, align)), SG_FALSE);
  default:
    Sg_Panic("unknown FFI return type: %d", rettype);
    return NULL;
  }
}

SgObject Sg_CStructRef(SgPointer *p, SgCStruct *st, SgSymbol *name)
{
  SgObject names = parse_member_name(name);
  int foundP = FALSE, type = 0;
  size_t align = calculate_alignment(names, st, &foundP, &type);

  if (!foundP || type == 0) {
    Sg_Error(UC("c-struct %A does not have a member named %A"), st->name, name);
    return SG_UNDEF;		/* dummy */
  }

  return convert_c_to_scheme(type, p, align);
}

void Sg_CStructSet(SgPointer *p, SgCStruct *st, SgSymbol *name, SgObject value)
{
  SgObject names = parse_member_name(name);
  int foundP = FALSE, type = 0;
  size_t align = calculate_alignment(names, st, &foundP, &type);

  if (!foundP || type == 0) {
    Sg_Error(UC("c-struct %A does not have a member named %A"), st->name, name);
    return;		/* dummy */
  }
  Sg_PointerSet(p, (int)align, type, value);
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
  void* ptr;
  float f32;
  double f64;
} ffi_storage;

static int prep_method_handler(SgCallback *callback);

static SgObject address_mark = SG_FALSE;

static SgObject get_error_message(SgChar signature, SgObject obj)
{
  switch (signature) {
  case FFI_SIGNATURE_BOOL:
    return  Sg_Sprintf(UC("'bool' required but got %A"), obj);
  case FFI_SIGNATURE_INT:
  case FFI_SIGNATURE_UINT:
    return  Sg_Sprintf(UC("'int' required but got %A"), obj);
  case FFI_SIGNATURE_INT64:
  case FFI_SIGNATURE_UINT64:
    return  Sg_Sprintf(UC("'int64' required but got %A"), obj);
  case FFI_SIGNATURE_FLOAT:
    return  Sg_Sprintf(UC("'float' required but got %A"), obj);
  case FFI_SIGNATURE_DOUBLE:
    return  Sg_Sprintf(UC("'double' required but got %A"), obj);
  case FFI_SIGNATURE_CALLBACK:
    return  Sg_Sprintf(UC("'callback' required but got %A"), obj);
  case FFI_SIGNATURE_POINTER:
    return  Sg_Sprintf(UC("'pointer' required but got %A"), obj);
  default:
    ASSERT(FALSE);
    return SG_FALSE;
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
    case FFI_SIGNATURE_INT:
    case FFI_SIGNATURE_UINT:
      storage->sl = SG_INT_VALUE(obj);
      return TRUE;
    case FFI_SIGNATURE_INT64:
    case FFI_SIGNATURE_UINT64:
      storage->s64 = SG_INT_VALUE(obj);
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
      storage->f32 = Sg_FlonumValue(obj);
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
    switch (signature) {
    case FFI_SIGNATURE_UINT:
      storage->ul = Sg_GetUIntegerClamp(obj, SG_CLAMP_NONE, NULL);
      return TRUE;
    case FFI_SIGNATURE_INT:
      storage->sl = Sg_GetIntegerClamp(obj, SG_CLAMP_NONE, NULL);
      return TRUE;
    case FFI_SIGNATURE_INT64:
      storage->s64 = Sg_GetIntegerS64Clamp(obj, SG_CLAMP_NONE, NULL);
      return TRUE;
    case FFI_SIGNATURE_UINT64:
      storage->u64 = Sg_GetIntegerU64Clamp(obj, SG_CLAMP_NONE, NULL);
      return TRUE;
    default:
      *lastError = get_error_message(signature, obj);
      return FALSE;
    }
  } else if (SG_STRINGP(obj)) {
    /* string -> char* (utf-8 ascii only) */
    switch (signature) {
    case FFI_SIGNATURE_POINTER:
      storage->ptr = (void*)(Sg_Utf32sToUtf8s(SG_STRING(obj)));
      return TRUE;
    default:
      *lastError = get_error_message(signature, obj);
      return FALSE;
    }
  } else if (SG_BVECTORP(obj)) {
    /* bytevector -> char* */
    switch (signature) {
    case FFI_SIGNATURE_POINTER:
      storage->ptr = (void*)(SG_BVECTOR_ELEMENTS(obj));
      return TRUE;
    default:
      *lastError = get_error_message(signature, obj);
      return FALSE;
    }
  } else if (SG_POINTERP(obj)) {
    switch (signature) {
    case FFI_SIGNATURE_POINTER:
      storage->ptr = (void*)SG_POINTER(obj)->pointer;
      return TRUE;
    default:
      *lastError = get_error_message(signature, obj);
      return FALSE;
    }
  } else if (SG_BOOLP(obj)) {
    switch (signature) {
    case FFI_SIGNATURE_BOOL:
      storage->sl = SG_TRUEP(obj) ? 1 : 0;
      return TRUE;
    default:
      *lastError = get_error_message(signature, obj);
      return FALSE;
    }
  } else if (SG_CALLBACKP(obj)) {
    switch (signature) {
    case FFI_SIGNATURE_CALLBACK:
      /* prepare closure here */
      if (!prep_method_handler(SG_CALLBACK(obj))) {
	*lastError = Sg_Sprintf(UC("failed to prepare the callback."));
	return FALSE;
      }

      storage->ptr = SG_CALLBACK(obj)->code;
      return TRUE;
    default:
      *lastError = get_error_message(signature, obj);
      return FALSE;
    }
    /* address stuff */
  } else if (SG_PAIRP(obj) && 
	     SG_EQ(SG_CAR(obj), address_mark) &&
	     SG_POINTERP(SG_CADR(obj))) {
    switch (signature) {
    case FFI_SIGNATURE_POINTER:
      storage->ptr = &(SG_POINTER(SG_CADR(obj))->pointer);
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
  default:
    Sg_Panic("unknown FFI return type: %d", rettype);
    return NULL;
  }
}

static int convert_scheme_to_c_value(SgObject v, int type, void **result)
{
  switch (type) {
  case FFI_RETURN_TYPE_BOOL    :
    *((intptr_t *)result) = !SG_FALSEP(v);
    return TRUE;
  case FFI_RETURN_TYPE_SHORT   :
  case FFI_RETURN_TYPE_INT     :
  case FFI_RETURN_TYPE_LONG    :
  case FFI_RETURN_TYPE_INT8_T  :
  case FFI_RETURN_TYPE_INT16_T :
  case FFI_RETURN_TYPE_INT32_T :
    if (!SG_EXACT_INTP(v)) goto ret0;
    if (SG_INTP(v)) {
      *((intptr_t *)result) = SG_INT_VALUE(v);
    } else {
      *((intptr_t *)result) = Sg_GetIntegerClamp(v, SG_CLAMP_NONE, NULL);
    }
    break;
  case FFI_RETURN_TYPE_USHORT  :
  case FFI_RETURN_TYPE_UINT    :
  case FFI_RETURN_TYPE_ULONG   :
  case FFI_RETURN_TYPE_SIZE_T  :
  case FFI_RETURN_TYPE_UINT8_T :
  case FFI_RETURN_TYPE_UINT16_T: 
  case FFI_RETURN_TYPE_UINT32_T:
    if (!SG_EXACT_INTP(v)) goto ret0;
    if (SG_INTP(v)) {
      *((uintptr_t *)result) = SG_INT_VALUE(v);
    } else {
      *((uintptr_t *)result) = Sg_GetUIntegerClamp(v, SG_CLAMP_NONE, NULL);
    }
    break;

  case FFI_RETURN_TYPE_INT64_T :
    if (SG_EXACT_INTP(v)) {
      if (SG_INTP(v)) {
	*((int64_t *)result) = SG_INT_VALUE(v);
      } else {
	*((int64_t *)result) = Sg_GetIntegerS64Clamp(v, SG_CLAMP_NONE, NULL);
      }
    } else {
      *((int64_t *)result) = 0;
    }
    break;
  case FFI_RETURN_TYPE_UINT64_T:
    if (SG_EXACT_INTP(v)) {
      if (SG_INTP(v)) {
	*((uint64_t *)result) = SG_INT_VALUE(v);
      } else {
	*((uint64_t *)result) = Sg_GetIntegerU64Clamp(v, SG_CLAMP_NONE, NULL);
      }
    } else {
      *((uint64_t *)result) = 0;
    }
    break;
  case FFI_RETURN_TYPE_FLOAT   :
    if (!SG_REALP(v)) *((float *)result) = 0.0;
    else *((float *)result) = (float)Sg_GetDouble(v);
    break;
  case FFI_RETURN_TYPE_DOUBLE  :
    if (!SG_REALP(v)) *((double *)result) = 0.0;;
    *((double *)result) = (float)Sg_GetDouble(v);
    break;
  case FFI_RETURN_TYPE_INTPTR  :
    if (SG_EXACT_INTP(v)) {
      if (SG_INTP(v)) {
	*((intptr_t *)result) = SG_INT_VALUE(v);
      } else {
	*((intptr_t *)result) = Sg_GetIntegerClamp(v, SG_CLAMP_NONE, NULL);
      }      
    } else if (SG_POINTERP(v)) {
      *((intptr_t *)result) = SG_POINTER(v)->pointer;
    } else goto ret0;
    break;
  case FFI_RETURN_TYPE_UINTPTR :
    if (SG_EXACT_INTP(v)) {
      if (SG_INTP(v)) {
	*((uintptr_t *)result) = SG_INT_VALUE(v);
      } else {
	*((uintptr_t *)result) = Sg_GetUIntegerClamp(v, SG_CLAMP_NONE, NULL);
      }      
    } else if (SG_POINTERP(v)) {
      *((uintptr_t *)result) = SG_POINTER(v)->pointer;
    } else goto ret0;
    break;
  case FFI_RETURN_TYPE_STRING  :
  case FFI_RETURN_TYPE_POINTER :
    if (SG_STRINGP(v)) {
      *((intptr_t *)result) = (intptr_t)Sg_Utf32sToUtf8s(SG_STRING(v));
    } else if (SG_BVECTORP(v)) {
      *((intptr_t *)result) = (intptr_t)SG_BVECTOR_ELEMENTS(v);
    } else if (SG_POINTERP(v)) {
      *((intptr_t *)result) = (intptr_t)SG_POINTER(v)->pointer;
    } else goto ret0;
    break;
  ret0:
    /* callback will be treated separately */
  case FFI_RETURN_TYPE_CALLBACK:
  case FFI_RETURN_TYPE_VOID    :
    *((intptr_t *)result) = 0;
    return TRUE;
  default:
    Sg_Panic("unknown FFI return type: %d", type);
    return FALSE;
  }
  return TRUE;
}

static SgObject get_callback_arguments(SgCallback *callback, void **args)
{
  SgObject h = SG_NIL, t = SG_NIL;
  int i;
  
  for (i = 0; i < SG_STRING_SIZE(callback->signatures); i++) {
    /* name convension :
       small letter signed, capital letter unsigned.
       b byte 	 : 1 byte
       h word 	 : 2 byte
       w dword	 : 4 byte
       q quadword: 8 byte
       specials
       f float
       d double
       p void*
       l bool
     */
    SgChar c = SG_STRING_VALUE_AT(callback->signatures, i); 
    switch (c) {
    case 'l':			/* bool */
      {
	int8_t arg = *(int8_t *)args[i];
	SG_APPEND1(h, t, arg ? SG_MAKE_INT(1) : SG_MAKE_INT(0));
	break;
      }
    case 'b': case 'B':		/* byte */
      {
	int8_t arg = *(int8_t *)args[i];
	SG_APPEND1(h, t, SG_MAKE_INT(arg));
	break;
      }
    case 'h': case 'H':		/* word */
      {
	int16_t arg = *(int16_t *)args[i];
	SG_APPEND1(h, t, SG_MAKE_INT(arg));
	break;
      }
    case 'w':			/* dword */
      {
	int32_t arg = *(int32_t *)args[i];
	SG_APPEND1(h, t, Sg_MakeInteger(arg));
	break;
      }
    case 'W':			/* dword */
      {
	int32_t arg = *(int32_t *)args[i];
	SG_APPEND1(h, t, Sg_MakeIntegerU(arg));
	break;
      }
    case 'q':			/* qword */
      {
	int64_t arg = *(int64_t *)args[i];
	SG_APPEND1(h, t, Sg_MakeIntegerFromS64(arg));
	break;
      }
    case 'Q':			/* qword */
      {
	int64_t arg = *(int64_t *)args[i];
	SG_APPEND1(h, t, Sg_MakeIntegerFromU64(arg));
	break;
      }
    case 'f':
      {
	float arg = *(float *)args[i];
	SG_APPEND1(h, t, Sg_MakeFlonum((double)arg));
	break;
      }
    case 'd':
      {
	double arg = *(double *)args[i];
	SG_APPEND1(h, t, Sg_MakeFlonum(arg));
	break;
      }
    case 'p':
      {
	void *arg = *(void **)args[i];
	SG_APPEND1(h, t, Sg_MakePointer(arg));
	break;
      }
    default:
      FATAL("invalid callback argument signature\n[[exit]\n]");
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
    cif->flags = FFI_TYPE_INT;
    *((ffi_sarg *) result) = !SG_FALSEP(ret);
    break;
  case FFI_RETURN_TYPE_INT:
    cif->flags = FFI_TYPE_INT;
    goto signed_entry;
  case FFI_RETURN_TYPE_INT8_T:
    cif->flags = FFI_TYPE_SINT8;
    goto signed_entry;
  case FFI_RETURN_TYPE_INT16_T:
  case FFI_RETURN_TYPE_SHORT:
    cif->flags = FFI_TYPE_SINT16;
  signed_entry:
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
    cif->flags = FFI_TYPE_INT;
    if (!SG_NUMBERP(ret)) goto ret0;
    *((ffi_sarg *) result) = Sg_GetIntegerClamp(ret, SG_CLAMP_NONE, NULL);
    break;
  case FFI_RETURN_TYPE_UINT:
  case FFI_RETURN_TYPE_SIZE_T:
    cif->flags = FFI_TYPE_INT;
    goto unsigned_entry;
  case FFI_RETURN_TYPE_UINT8_T:
    cif->flags = FFI_TYPE_UINT8;
    goto unsigned_entry;
  case FFI_RETURN_TYPE_UINT16_T:
  case FFI_RETURN_TYPE_USHORT:
    cif->flags = FFI_TYPE_UINT16;
  unsigned_entry:
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
    cif->flags = FFI_TYPE_INT;
    if (!SG_NUMBERP(ret)) goto ret0;
    *((ffi_arg *) result) = Sg_GetUIntegerClamp(ret, SG_CLAMP_NONE, NULL);
    break;
  case FFI_RETURN_TYPE_FLOAT:
    cif->flags = FFI_TYPE_FLOAT;
    if (!SG_NUMBERP(ret)) goto ret0;
    *((float *) result) = (float)Sg_GetDouble(ret);
    break;
  case FFI_RETURN_TYPE_DOUBLE:
    cif->flags = FFI_TYPE_DOUBLE;
    if (!SG_NUMBERP(ret)) goto ret0;
    *((double *) result) = Sg_GetDouble(ret);
    break;
  int64_entry:
  case FFI_RETURN_TYPE_INT64_T:
    cif->flags = FFI_TYPE_SINT64;
    if (!SG_NUMBERP(ret)) goto ret0;
    *((int64_t *) result) = Sg_GetIntegerS64Clamp(ret, SG_CLAMP_NONE, NULL);
    break;
  uint64_entry:
  case FFI_RETURN_TYPE_UINT64_T:
    cif->flags = FFI_TYPE_UINT64;
    if (!SG_NUMBERP(ret)) goto ret0;
    *((uint64_t *) result) = Sg_GetIntegerU64Clamp(ret, SG_CLAMP_NONE, NULL);
    break;
  case FFI_RETURN_TYPE_POINTER:
  case FFI_RETURN_TYPE_INTPTR:
  case FFI_RETURN_TYPE_UINTPTR:
    cif->flags = FFI_TYPE_POINTER;
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
    /* fall through */
  case FFI_RETURN_TYPE_VOID:
    cif->flags = FFI_TYPE_VOID;
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


static SgObject internal_ffi_call(SgObject *args, int argc, void *data)
{
  SgObject lastError = SG_FALSE;
  /* SgObject *funcargs; */
  SgObject funcargs, cp;
  int retType, i, size;
  SgObject signatures;
  SgFuncInfo *func;
  intptr_t ret;
  ffi_storage *params;
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

  signatures = func->signatures;
  /* check if the argument count is correct */
  size = Sg_Length(args[argc-1]);
  if (size != func->argc) {
    Sg_Error(UC("argument count is not correct. required %d, but got %d"),
	     func->argc, size);
    return SG_UNDEF;
  }

  ffi_values = SG_NEW_ARRAY(void*, func->argc);
  params = SG_NEW_ARRAY(ffi_storage, func->argc);

  /* funcargs = Sg_ListToArray(args[argc-1], FALSE); */
  funcargs = args[argc-1];
  i = 0;
  SG_FOR_EACH(cp, funcargs) {
    /* for (i = 0; i < func->argc; i++) { */
    if (!push_ffi_type_value(func,
			     SG_STRING_VALUE_AT(signatures, i),
			     /* funcargs[i],  */
			     SG_CAR(cp),
			     params + i,
			     &lastError)) {
      Sg_Error(UC("argument error %A on index %d: %S"), func, i, lastError);
      return SG_UNDEF;
    }
    ffi_values[i] = (params + i);
    i++;
  }
  /* sanity check */
  if (!func->code) {
    Sg_Error(UC("invalid c-function %S"), func);
    return SG_UNDEF;
  }
  switch (retType) {
  case FFI_RETURN_TYPE_VOID:
    ffi_call(&func->cif, FFI_FN(func->code), &ret, ffi_values);
    return SG_UNDEF;
  case FFI_RETURN_TYPE_BOOL:
    ffi_call(&func->cif, FFI_FN(func->code), &ret, ffi_values);
    return SG_MAKE_BOOL(ret != FALSE);
  case FFI_RETURN_TYPE_SHORT:
    ffi_call(&func->cif, FFI_FN(func->code), &ret, ffi_values);
    return SG_MAKE_INT((short)ret);
  case FFI_RETURN_TYPE_INT:
    ffi_call(&func->cif, FFI_FN(func->code), &ret, ffi_values);
    return Sg_MakeInteger((int)ret);
  case FFI_RETURN_TYPE_LONG:
    ffi_call(&func->cif, FFI_FN(func->code), &ret, ffi_values);
    return Sg_MakeInteger((long)ret);
  case FFI_RETURN_TYPE_INTPTR:
    ffi_call(&func->cif, FFI_FN(func->code), &ret, ffi_values);
#if SIZEOF_VOIDP == 4
    return Sg_MakeInteger((intptr_t)ret);
#else
    return Sg_MakeIntegerFromS64((intptr_t)ret);
#endif
  case FFI_RETURN_TYPE_USHORT:
    ffi_call(&func->cif, FFI_FN(func->code), &ret, ffi_values);
    return SG_MAKE_INT((unsigned short)ret);
  case FFI_RETURN_TYPE_UINT:
    ffi_call(&func->cif, FFI_FN(func->code), &ret, ffi_values);
    return Sg_MakeIntegerU((unsigned int)ret);
  case FFI_RETURN_TYPE_ULONG:
    ffi_call(&func->cif, FFI_FN(func->code), &ret, ffi_values);
    return Sg_MakeIntegerU((unsigned long)ret);
  case FFI_RETURN_TYPE_UINTPTR:
    ffi_call(&func->cif, FFI_FN(func->code), &ret, ffi_values);
#if SIZEOF_VOIDP == 4
    return Sg_MakeIntegerU((uintptr_t)ret);
#else
    return Sg_MakeIntegerFromU64((uintptr_t)ret);
#endif
  case FFI_RETURN_TYPE_FLOAT: {
    float fret;
    ffi_call(&func->cif, FFI_FN(func->code), &fret, ffi_values);
    return Sg_MakeFlonum((double)fret);
  }
  case FFI_RETURN_TYPE_DOUBLE: {
    double fret;
    ffi_call(&func->cif, FFI_FN(func->code), &fret, ffi_values);
    return Sg_MakeFlonum(fret);
  }
  case FFI_RETURN_TYPE_STRING:
    ffi_call(&func->cif, FFI_FN(func->code), &ret, ffi_values);
    if (ret == 0) {
      return make_pointer((uintptr_t)NULL);
    } else {
      return Sg_MakeStringC((char *)ret);
    }
  case FFI_RETURN_TYPE_SIZE_T:
    ffi_call(&func->cif, FFI_FN(func->code), &ret, ffi_values);
    return Sg_MakeIntegerU((unsigned long)ret);
  case FFI_RETURN_TYPE_INT8_T:
    ffi_call(&func->cif, FFI_FN(func->code), &ret, ffi_values);
    return SG_MAKE_INT((int8_t)ret);
  case FFI_RETURN_TYPE_UINT8_T:
    ffi_call(&func->cif, FFI_FN(func->code), &ret, ffi_values);
    return SG_MAKE_INT((uint8_t)ret);
  case FFI_RETURN_TYPE_INT16_T:
    ffi_call(&func->cif, FFI_FN(func->code), &ret, ffi_values);
    return SG_MAKE_INT((int16_t)ret);
  case FFI_RETURN_TYPE_UINT16_T:
    ffi_call(&func->cif, FFI_FN(func->code), &ret, ffi_values);
    return SG_MAKE_INT((uint16_t)ret);
  case FFI_RETURN_TYPE_INT32_T:
    ffi_call(&func->cif, FFI_FN(func->code), &ret, ffi_values);
    return Sg_MakeInteger((int)ret);
  case FFI_RETURN_TYPE_UINT32_T:
    ffi_call(&func->cif, FFI_FN(func->code), &ret, ffi_values);
    return Sg_MakeIntegerU((unsigned int)ret);
  case FFI_RETURN_TYPE_INT64_T: {
    int64_t ret64;
    ffi_call(&func->cif, FFI_FN(func->code), &ret64, ffi_values);
    return Sg_MakeIntegerFromS64(ret64);
  }
  case FFI_RETURN_TYPE_UINT64_T: {
    int64_t ret64;
    ffi_call(&func->cif, FFI_FN(func->code), &ret64, ffi_values);
    return Sg_MakeIntegerFromU64(ret64);
  }
  case FFI_RETURN_TYPE_POINTER:
    ffi_call(&func->cif, FFI_FN(func->code), &ret, ffi_values);
    return make_pointer(ret);
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

static void set_ffi_callback_parameter_types(SgObject signatures,
					     ffi_type **types)
{
  int i;
  for (i = 0; i < SG_STRING_SIZE(signatures); i++) {
    SgChar c = SG_STRING_VALUE_AT(signatures, i); 
    switch (c) {
      /* bool */
    case 'l':
      types[i] = &ffi_type_sint; break;
      /* byte */
    case 'b':
      types[i] = &ffi_type_sint8; break;
    case 'B':
      types[i] = &ffi_type_uint8; break;
      /* word */
    case 'h':
      types[i] = &ffi_type_sint16; break;
    case 'H':
      types[i] = &ffi_type_uint16; break;
      /* dword */
    case 'w':
      types[i] = &ffi_type_sint32; break;
    case 'W':
      types[i] = &ffi_type_uint32; break;
      /* qword */
    case 'q':
      types[i] = &ffi_type_sint64; break;
    case 'Q':
      types[i] = &ffi_type_uint64; break;
      /* float */
    case 'f':
      types[i] = &ffi_type_float; break;
    case 'd':
      types[i] = &ffi_type_double; break;
    case 'p':
      types[i] = &ffi_type_pointer; break;
    default:
      FATAL("invalid callback argument signature\n[[exit]\n]");
      break;
    }
  }
}

static int prep_method_handler(SgCallback *callback)
{
  ffi_status status;
  ffi_type **params = SG_NEW_ARRAY(ffi_type *, SG_STRING_SIZE(callback->signatures));
  ffi_type *ret = lookup_ffi_return_type(callback->returnType);
  set_ffi_callback_parameter_types(callback->signatures, params);
  ffi_prep_cif(&callback->cif, FFI_DEFAULT_ABI,
	       SG_STRING_SIZE(callback->signatures),
	       ret, params);
  status = ffi_prep_closure_loc(callback->closure,
				&callback->cif,
				callback_invoker, callback,
				callback->code);
  callback->parameterTypes = params;
  return status == FFI_OK;
}

/* utility */
#define DEFINE_POINTER_SET(ft, t)				\
  static void pointer_set_##ft(SgPointer *p, int offset,	\
			      int type, SgObject v)		\
  {								\
    t result[256];						\
    convert_scheme_to_c_value(v, type, (void**)result);		\
    POINTER_SET(t, p, offset, *(t *)result);			\
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
DEFINE_POINTER_SET(FFI_RETURN_TYPE_UINT64_T, uint64_t);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_STRING  , intptr_t);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_POINTER , void*);
DEFINE_POINTER_SET(FFI_RETURN_TYPE_STRUCT  , void*);

void Sg_PointerSet(SgPointer *p, int offset, int type, SgObject v)
{
  if (!p->pointer) {
    Sg_AssertionViolation(SG_INTERN("pointer-set!"),
			  SG_MAKE_STRING("got null pointer"),
			  SG_LIST2(p, v));
  }
#define case_type(ft, t) case ft: pointer_set_##ft(p, offset, type, v); break;
  switch (type) {
    case_type(FFI_RETURN_TYPE_SHORT   , short);
    case_type(FFI_RETURN_TYPE_INT     , int);
    case_type(FFI_RETURN_TYPE_LONG    , long);
    case_type(FFI_RETURN_TYPE_INTPTR  , intptr_t);
    case_type(FFI_RETURN_TYPE_USHORT  , unsigned short);
    case_type(FFI_RETURN_TYPE_UINT    , unsigned int);
    case_type(FFI_RETURN_TYPE_ULONG   , unsigned long);
    case_type(FFI_RETURN_TYPE_UINTPTR , uintptr_t);
    case_type(FFI_RETURN_TYPE_FLOAT   , float);
    case_type(FFI_RETURN_TYPE_DOUBLE  , double);
    case_type(FFI_RETURN_TYPE_SIZE_T  , size_t);
    case_type(FFI_RETURN_TYPE_INT8_T  , int8_t);
    case_type(FFI_RETURN_TYPE_UINT8_T , uint8_t);
    case_type(FFI_RETURN_TYPE_INT16_T , int16_t);
    case_type(FFI_RETURN_TYPE_UINT16_T, uint16_t);
    case_type(FFI_RETURN_TYPE_INT32_T , int32_t);
    case_type(FFI_RETURN_TYPE_UINT32_T, uint32_t);
    case_type(FFI_RETURN_TYPE_INT64_T , int64_t);
    case_type(FFI_RETURN_TYPE_UINT64_T, uint64_t);
    case_type(FFI_RETURN_TYPE_STRING  , intptr_t);
    case_type(FFI_RETURN_TYPE_POINTER , void*);
    case_type(FFI_RETURN_TYPE_STRUCT  , void*);
  case FFI_RETURN_TYPE_CALLBACK: {
    if (!SG_CALLBACKP(v)) Sg_Error(UC("callback required, but got %S "), v);
    if (prep_method_handler(SG_CALLBACK(v))) {
      POINTER_SET(void*, p, offset, SG_CALLBACK(v)->code);
      Sg_HashTableSet(ref_table, SG_CALLBACK(v)->code, v, 0);
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

  Sg_AddCondFeature(UC("sagittarius.ffi"));
  SG_PROCEDURE_NAME(&internal_ffi_call_stub) = name;

  SYMBOL_STRUCT = SG_INTERN("struct");
  address_mark = SG_INTERN("address");

  SG_INIT_EXTENSION(sagittarius__ffi);
  /* init impl library first */
  lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius ffi)"), FALSE));
  Sg__Init_ffi_stub(lib);
  Sg_InsertBinding(lib, name, &internal_ffi_call_stub);
  impl_lib = lib;
  /* callback storage */
  ctable = SG_HASHTABLE(Sg_MakeHashTableSimple(SG_HASH_EQ, 0));
  ref_table = SG_HASHTABLE(Sg_MakeHashTableSimple(SG_HASH_EQ, 0));

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

#undef CONST_VALUE
#undef SIZE_VALUE
#undef ALIGN_OF
}
