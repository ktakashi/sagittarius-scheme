/* This file is autmatically generated from "/home/t.kato/projects/sagittarius/ext/zlib/zlib_stub.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
#include "zlib.h"
;
static SgObject _sagittarius_zlib_deflate_init(SgObject *args, int argc, void *data_)
{
  SgObject level_scm;
  int level;
  SgObject windowbits_scm;
  int windowbits;
  SgObject memlevel_scm;
  int memlevel;
  SgObject strategy_scm;
  int strategy;
  DeclareProcedureName("deflate-init");
  checkArgumentLength(4);
  argumentAsFixnum(0, level_scm, level);
  argumentAsFixnum(1, windowbits_scm, windowbits);
  argumentAsFixnum(2, memlevel_scm, memlevel);
  argumentAsFixnum(3, strategy_scm, strategy);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (    Sg_DeflateInit    (    level    ,     windowbits    ,     memlevel    ,     strategy    )    )    ;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_deflate_init_Stub, 4, 0, _sagittarius_zlib_deflate_init, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_deflate_reset(SgObject *args, int argc, void *data_)
{
  SgObject strm_scm;
  SgZStream *strm;
  DeclareProcedureName("deflate-reset");
  checkArgumentLength(1);
  argumentAsZStream(0, strm_scm, strm);
  {
    int SG_RETURN;
    SG_RETURN = (    Sg_DeflateReset    (    strm    )    )    ;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_deflate_reset_Stub, 1, 0, _sagittarius_zlib_deflate_reset, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_deflate_set_dictionary(SgObject *args, int argc, void *data_)
{
  SgObject strm_scm;
  SgZStream *strm;
  SgObject dict_scm;
  SgByteVector *dict;
  DeclareProcedureName("deflate-set-dictionary");
  checkArgumentLength(2);
  argumentAsZStream(0, strm_scm, strm);
  argumentAsByteVector(1, dict_scm, dict);
  {
    int SG_RETURN;
    SG_RETURN = (    Sg_DeflateSetDictionary    (    strm    ,     dict    )    )    ;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_deflate_set_dictionary_Stub, 2, 0, _sagittarius_zlib_deflate_set_dictionary, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_deflate(SgObject *args, int argc, void *data_)
{
  SgObject strm_scm;
  SgZStream *strm;
  SgObject data_scm;
  SgByteVector *data;
  SgObject dest_scm;
  SgByteVector *dest;
  SgObject flush_scm;
  int flush;
  DeclareProcedureName("deflate");
  checkArgumentLength(4);
  argumentAsZStream(0, strm_scm, strm);
  argumentAsByteVector(1, data_scm, data);
  argumentAsByteVector(2, dest_scm, dest);
  argumentAsFixnum(3, flush_scm, flush);
  {
    int SG_RETURN;
    SG_RETURN = (    Sg_Deflate    (    strm    ,     data    ,     dest    ,     flush    )    )    ;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_deflate_Stub, 4, 0, _sagittarius_zlib_deflate, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_deflate_end(SgObject *args, int argc, void *data_)
{
  SgObject strm_scm;
  SgZStream *strm;
  DeclareProcedureName("deflate-end");
  checkArgumentLength(1);
  argumentAsZStream(0, strm_scm, strm);
  {
    int SG_RETURN;
    SG_RETURN = (    Sg_DeflateEnd    (    strm    )    )    ;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_deflate_end_Stub, 1, 0, _sagittarius_zlib_deflate_end, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_inflate_init(SgObject *args, int argc, void *data_)
{
  SgObject windowbits_scm;
  int windowbits;
  DeclareProcedureName("inflate-init");
  checkArgumentLength(1);
  argumentAsFixnum(0, windowbits_scm, windowbits);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (    Sg_InflateInit    (    windowbits    )    )    ;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_inflate_init_Stub, 1, 0, _sagittarius_zlib_inflate_init, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_inflate_reset(SgObject *args, int argc, void *data_)
{
  SgObject strm_scm;
  SgZStream *strm;
  SgObject bits_scm;
  int bits;
  DeclareProcedureName("inflate-reset");
  checkArgumentLengthBetween(1, 2);
  argumentAsZStream(0, strm_scm, strm);
  if (argc >= 2) {
    argumentAsFixnum(1, bits_scm, bits);
  } else {
    bits = -1;
  }

  {
    int SG_RETURN;
    SG_RETURN = (    Sg_InflateReset    (    strm    ,     bits    )    )    ;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_inflate_reset_Stub, 1, 1, _sagittarius_zlib_inflate_reset, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_inflate_set_dictionary(SgObject *args, int argc, void *data_)
{
  SgObject strm_scm;
  SgZStream *strm;
  SgObject dict_scm;
  SgByteVector *dict;
  DeclareProcedureName("inflate-set-dictionary");
  checkArgumentLength(2);
  argumentAsZStream(0, strm_scm, strm);
  argumentAsByteVector(1, dict_scm, dict);
  {
    int SG_RETURN;
    SG_RETURN = (    Sg_InflateSetDictionary    (    strm    ,     dict    )    )    ;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_inflate_set_dictionary_Stub, 2, 0, _sagittarius_zlib_inflate_set_dictionary, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_inflate_sync(SgObject *args, int argc, void *data_)
{
  SgObject strm_scm;
  SgZStream *strm;
  DeclareProcedureName("inflate-sync");
  checkArgumentLength(1);
  argumentAsZStream(0, strm_scm, strm);
  {
    int SG_RETURN;
    SG_RETURN = (    Sg_InflateSync    (    strm    )    )    ;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_inflate_sync_Stub, 1, 0, _sagittarius_zlib_inflate_sync, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_inflate(SgObject *args, int argc, void *data_)
{
  SgObject strm_scm;
  SgZStream *strm;
  SgObject data_scm;
  SgByteVector *data;
  SgObject dest_scm;
  SgByteVector *dest;
  SgObject flush_scm;
  int flush;
  DeclareProcedureName("inflate");
  checkArgumentLength(4);
  argumentAsZStream(0, strm_scm, strm);
  argumentAsByteVector(1, data_scm, data);
  argumentAsByteVector(2, dest_scm, dest);
  argumentAsFixnum(3, flush_scm, flush);
  {
    int SG_RETURN;
    SG_RETURN = (    Sg_Inflate    (    strm    ,     data    ,     dest    ,     flush    )    )    ;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_inflate_Stub, 4, 0, _sagittarius_zlib_inflate, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_inflate_end(SgObject *args, int argc, void *data_)
{
  SgObject strm_scm;
  SgZStream *strm;
  SgObject flush_scm;
  int flush;
  DeclareProcedureName("inflate-end");
  checkArgumentLength(2);
  argumentAsZStream(0, strm_scm, strm);
  argumentAsFixnum(1, flush_scm, flush);
  {
    int SG_RETURN;
    SG_RETURN = (    Sg_InflateEnd    (    strm    ,     flush    )    )    ;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_inflate_end_Stub, 2, 0, _sagittarius_zlib_inflate_end, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_zstream3f(SgObject *args, int argc, void *data_)
{
  SgObject obj;
  DeclareProcedureName("zstream?");
  checkArgumentLength(1);
  argumentRef(0, obj);
  {
    int SG_RETURN;
    SG_RETURN = (    SG_ZSTREAM_P    (    obj    )    )    ;
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_zstream3f_Stub, 1, 0, _sagittarius_zlib_zstream3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_zstream_total_in(SgObject *args, int argc, void *data_)
{
  SgObject strm_scm;
  SgZStream *strm;
  DeclareProcedureName("zstream-total-in");
  checkArgumentLength(1);
  argumentAsZStream(0, strm_scm, strm);
  {
    int SG_RETURN;
    SG_RETURN = (    strm    ->    strm    ->    total_in    )    ;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_zstream_total_in_Stub, 1, 0, _sagittarius_zlib_zstream_total_in, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_zstream_total_out(SgObject *args, int argc, void *data_)
{
  SgObject strm_scm;
  SgZStream *strm;
  DeclareProcedureName("zstream-total-out");
  checkArgumentLength(1);
  argumentAsZStream(0, strm_scm, strm);
  {
    int SG_RETURN;
    SG_RETURN = (    strm    ->    strm    ->    total_out    )    ;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_zstream_total_out_Stub, 1, 0, _sagittarius_zlib_zstream_total_out, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_zstream_avail_in(SgObject *args, int argc, void *data_)
{
  SgObject strm_scm;
  SgZStream *strm;
  DeclareProcedureName("zstream-avail-in");
  checkArgumentLength(1);
  argumentAsZStream(0, strm_scm, strm);
  {
    int SG_RETURN;
    SG_RETURN = (    strm    ->    strm    ->    avail_in    )    ;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_zstream_avail_in_Stub, 1, 0, _sagittarius_zlib_zstream_avail_in, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_zstream_avail_out(SgObject *args, int argc, void *data_)
{
  SgObject strm_scm;
  SgZStream *strm;
  DeclareProcedureName("zstream-avail-out");
  checkArgumentLength(1);
  argumentAsZStream(0, strm_scm, strm);
  {
    int SG_RETURN;
    SG_RETURN = (    strm    ->    strm    ->    avail_out    )    ;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_zstream_avail_out_Stub, 1, 0, _sagittarius_zlib_zstream_avail_out, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_zstream_data_type(SgObject *args, int argc, void *data_)
{
  SgObject strm_scm;
  SgZStream *strm;
  DeclareProcedureName("zstream-data-type");
  checkArgumentLength(1);
  argumentAsZStream(0, strm_scm, strm);
  {
    int SG_RETURN;
    SG_RETURN = (    strm    ->    strm    ->    data_type    )    ;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_zstream_data_type_Stub, 1, 0, _sagittarius_zlib_zstream_data_type, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_zstream_adler32(SgObject *args, int argc, void *data_)
{
  SgObject strm_scm;
  SgZStream *strm;
  DeclareProcedureName("zstream-adler32");
  checkArgumentLength(1);
  argumentAsZStream(0, strm_scm, strm);
  {
    int SG_RETURN;
    SG_RETURN = (    strm    ->    strm    ->    adler    )    ;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_zstream_adler32_Stub, 1, 0, _sagittarius_zlib_zstream_adler32, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_zstream_read_count(SgObject *args, int argc, void *data_)
{
  SgObject strm_scm;
  SgZStream *strm;
  SgObject in_scm;
  SgByteVector *in;
  DeclareProcedureName("zstream-read-count");
  checkArgumentLength(2);
  argumentAsZStream(0, strm_scm, strm);
  argumentAsByteVector(1, in_scm, in);
  {
    int SG_RETURN;
    SG_RETURN = (    (    strm    ->    strm    ->    next_in     -     SG_BVECTOR_ELEMENTS    (    in    )    )    )    ;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_zstream_read_count_Stub, 2, 0, _sagittarius_zlib_zstream_read_count, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_zstream_write_count(SgObject *args, int argc, void *data_)
{
  SgObject strm_scm;
  SgZStream *strm;
  SgObject out_scm;
  SgByteVector *out;
  DeclareProcedureName("zstream-write-count");
  checkArgumentLength(2);
  argumentAsZStream(0, strm_scm, strm);
  argumentAsByteVector(1, out_scm, out);
  {
    int SG_RETURN;
    SG_RETURN = (    (    strm    ->    strm    ->    next_out     -     SG_BVECTOR_ELEMENTS    (    out    )    )    )    ;
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_zstream_write_count_Stub, 2, 0, _sagittarius_zlib_zstream_write_count, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_zlib_error_message(SgObject *args, int argc, void *data_)
{
  SgObject strm_scm;
  SgZStream *strm;
  DeclareProcedureName("zlib-error-message");
  checkArgumentLength(1);
  argumentAsZStream(0, strm_scm, strm);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (    strm    ->    strm    ->    msg    ) {
      SG_RETURN = (      Sg_MakeStringC      (      strm      ->      strm      ->      msg      )      )      ;
    }     else {
      SG_RETURN = (      Sg_MakeString      (      UC("no z-stream error message available")      ,       SG_LITERAL_STRING      )      )      ;
    }    
    ;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_zlib_error_message_Stub, 1, 0, _sagittarius_zlib_zlib_error_message, SG_FALSE, NULL);

;
static SgObject _sagittarius_zlib_zlib_version(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("zlib-version");
  checkArgumentLength(0);
  {
    SgObject SG_RETURN = SG_UNDEF;
    SG_RETURN = (    Sg_ZlibVersion    (    )    )    ;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_zlib_zlib_version_Stub, 0, 0, _sagittarius_zlib_zlib_version, SG_FALSE, NULL);

;
void Sg__Init_sagittarius_zlib()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius zlib)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_sagittarius_zlib_deflate_init_Stub) = Sg_MakeString(UC("deflate-init"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("deflate-init"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_deflate_init_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_inflate_set_dictionary_Stub) = Sg_MakeString(UC("inflate-set-dictionary"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("inflate-set-dictionary"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_inflate_set_dictionary_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_zstream_avail_in_Stub) = Sg_MakeString(UC("zstream-avail-in"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("zstream-avail-in"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_zstream_avail_in_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_zlib_version_Stub) = Sg_MakeString(UC("zlib-version"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("zlib-version"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_zlib_version_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_deflate_reset_Stub) = Sg_MakeString(UC("deflate-reset"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("deflate-reset"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_deflate_reset_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_zstream_avail_out_Stub) = Sg_MakeString(UC("zstream-avail-out"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("zstream-avail-out"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_zstream_avail_out_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_inflate_sync_Stub) = Sg_MakeString(UC("inflate-sync"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("inflate-sync"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_inflate_sync_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_deflate_set_dictionary_Stub) = Sg_MakeString(UC("deflate-set-dictionary"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("deflate-set-dictionary"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_deflate_set_dictionary_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_inflate_Stub) = Sg_MakeString(UC("inflate"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("inflate"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_inflate_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_zstream_data_type_Stub) = Sg_MakeString(UC("zstream-data-type"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("zstream-data-type"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_zstream_data_type_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_deflate_Stub) = Sg_MakeString(UC("deflate"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("deflate"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_deflate_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_inflate_end_Stub) = Sg_MakeString(UC("inflate-end"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("inflate-end"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_inflate_end_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_zstream_adler32_Stub) = Sg_MakeString(UC("zstream-adler32"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("zstream-adler32"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_zstream_adler32_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_deflate_end_Stub) = Sg_MakeString(UC("deflate-end"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("deflate-end"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_deflate_end_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_zstream3f_Stub) = Sg_MakeString(UC("zstream?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("zstream?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_zstream3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_zstream_read_count_Stub) = Sg_MakeString(UC("zstream-read-count"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("zstream-read-count"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_zstream_read_count_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_inflate_init_Stub) = Sg_MakeString(UC("inflate-init"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("inflate-init"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_inflate_init_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_zstream_total_in_Stub) = Sg_MakeString(UC("zstream-total-in"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("zstream-total-in"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_zstream_total_in_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_zstream_write_count_Stub) = Sg_MakeString(UC("zstream-write-count"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("zstream-write-count"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_zstream_write_count_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_inflate_reset_Stub) = Sg_MakeString(UC("inflate-reset"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("inflate-reset"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_inflate_reset_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_zstream_total_out_Stub) = Sg_MakeString(UC("zstream-total-out"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("zstream-total-out"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_zstream_total_out_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_zlib_zlib_error_message_Stub) = Sg_MakeString(UC("zlib-error-message"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("zlib-error-message"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_zlib_zlib_error_message_Stub));
}
