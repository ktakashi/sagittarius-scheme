/* This file is autmatically generated from "/home/t.kato/projects/sagittarius/ext/socket/socket_stub.stub". DO NOT EDIT!!*/
#define LIBSAGITTARIUS_BODY
#include <sagittarius.h>
#include "socket.h"
;
;
static SgObject _sagittarius_socket_impl_make_client_socket(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-client-socket");
  SgObject node;
  SgObject service;
  SgObject ai_family_scm;
  int ai_family;
  SgObject ai_socktype_scm;
  int ai_socktype;
  SgObject ai_flags_scm;
  int ai_flags;
  SgObject ai_protocol_scm;
  int ai_protocol;
  checkArgumentLengthBetween(2, 6);
  argumentRef(0, node);
  argumentRef(1, service);
  if (argc >= 3) {
    argumentAsFixnum(2, ai_family_scm, ai_family);
  } else {
    ai_family = AF_INET;
  }

  if (argc >= 4) {
    argumentAsFixnum(3, ai_socktype_scm, ai_socktype);
  } else {
    ai_socktype = SOCK_STREAM;
  }

  if (argc >= 5) {
    argumentAsFixnum(4, ai_flags_scm, ai_flags);
  } else {
    ai_flags = (AI_V4MAPPED + AI_ADDRCONFIG);
  }

  if (argc >= 6) {
    argumentAsFixnum(5, ai_protocol_scm, ai_protocol);
  } else {
    ai_protocol = 0;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_STRINGP(node) || SG_FALSEP(node)))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("string or #f"), SG_LITERAL_STRING), node, SG_NIL);
      return SG_UNDEF;
    }
;
    if (!((SG_STRINGP(service) || SG_FALSEP(service)))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("string or #f"), SG_LITERAL_STRING), service, SG_NIL);
      return SG_UNDEF;
    }
;
    if (SG_FALSEP(node)) {
      node=NULL;
    }
;
    if (SG_FALSEP(service)) {
      service=NULL;
    }
;
    {
      SgObject sock = Sg_CreateClientSocket(node, service, ai_family, ai_socktype, ai_flags, ai_protocol);
      if (Sg_SocketOpenP(sock)) {
        SG_RETURN = (sock);
      } else {
        Sg_IOError(-1, procedureName, Sg_GetLastErrorMessageWithErrorCode(SG_SOCKET(sock)->lastError), SG_MAKE_BOOL(FALSE), SG_LIST2(node, service));
        SG_RETURN = (SG_UNDEF);
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_socket_impl_make_client_socket_Stub, 2, 4, _sagittarius_socket_impl_make_client_socket, SG_FALSE, NULL);

;
static SgObject _sagittarius_socket_impl_make_server_socket(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("make-server-socket");
  SgObject service;
  SgObject ai_family_scm;
  int ai_family;
  SgObject ai_socktype_scm;
  int ai_socktype;
  SgObject ai_protocol_scm;
  int ai_protocol;
  checkArgumentLengthBetween(1, 4);
  argumentRef(0, service);
  if (argc >= 2) {
    argumentAsFixnum(1, ai_family_scm, ai_family);
  } else {
    ai_family = AF_INET;
  }

  if (argc >= 3) {
    argumentAsFixnum(2, ai_socktype_scm, ai_socktype);
  } else {
    ai_socktype = SOCK_STREAM;
  }

  if (argc >= 4) {
    argumentAsFixnum(3, ai_protocol_scm, ai_protocol);
  } else {
    ai_protocol = 0;
  }

  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!((SG_STRINGP(service) || SG_FALSEP(service)))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("string or #f"), SG_LITERAL_STRING), service, SG_NIL);
      return SG_UNDEF;
    }
;
    if (SG_FALSEP(service)) {
      service=NULL;
    }
;
    {
      SgObject sock = Sg_CreateServerSocket(service, ai_family, ai_socktype, ai_protocol);
      if (Sg_SocketOpenP(sock)) {
        SG_RETURN = (sock);
      } else {
        Sg_IOError(-1, procedureName, Sg_GetLastErrorMessageWithErrorCode(SG_SOCKET(sock)->lastError), SG_MAKE_BOOL(FALSE), service);
        SG_RETURN = (SG_UNDEF);
      }
;
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_socket_impl_make_server_socket_Stub, 1, 3, _sagittarius_socket_impl_make_server_socket, SG_FALSE, NULL);

;
;
static SgObject _sagittarius_socket_impl_socket3f(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("socket?");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    int SG_RETURN;
    SG_RETURN = (SG_SOCKET_P(o));
    return SG_MAKE_BOOL(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_socket_impl_socket3f_Stub, 1, 0, _sagittarius_socket_impl_socket3f, SG_FALSE, NULL);

;
static SgObject _sagittarius_socket_impl_socket_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("socket-port");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_SOCKET_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("socket"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    SG_RETURN = (Sg_MakeSocketPort(SG_SOCKET(o)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_socket_impl_socket_port_Stub, 1, 0, _sagittarius_socket_impl_socket_port, SG_FALSE, NULL);

;
static SgObject _sagittarius_socket_impl_shutdown_output_port(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("shutdown-output-port");
  SgObject o_scm;
  SgPort *o;
  checkArgumentLength(1);
  argumentAsPort(0, o_scm, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_ShutdownPort(o);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_socket_impl_shutdown_output_port_Stub, 1, 0, _sagittarius_socket_impl_shutdown_output_port, SG_FALSE, NULL);

;
static SgObject _sagittarius_socket_impl_socket_accept(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("socket-accept");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_SOCKET_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("socket"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    SG_RETURN = (Sg_SocketAccept(SG_SOCKET(o)));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_socket_impl_socket_accept_Stub, 1, 0, _sagittarius_socket_impl_socket_accept, SG_FALSE, NULL);

;
static SgObject _sagittarius_socket_impl_socket_send(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("socket-send");
  SgObject o;
  SgObject bv_scm;
  SgByteVector *bv;
  SgObject flags_scm;
  int flags;
  checkArgumentLength(3);
  argumentRef(0, o);
  argumentAsByteVector(1, bv_scm, bv);
  argumentAsFixnum(2, flags_scm, flags);
  {
    int SG_RETURN;
    if (!(SG_SOCKET_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("socket"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    SG_RETURN = (Sg_SocketSend(SG_SOCKET(o), SG_BVECTOR_ELEMENTS(bv), SG_BVECTOR_SIZE(bv), flags));
    return SG_MAKE_INT(SG_RETURN);
  }
}
static SG_DEFINE_SUBR(_sagittarius_socket_impl_socket_send_Stub, 3, 0, _sagittarius_socket_impl_socket_send, SG_FALSE, NULL);

;
static SgObject _sagittarius_socket_impl_socket_recv(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("socket-recv");
  SgObject o;
  SgObject len_scm;
  int len;
  SgObject flags_scm;
  int flags;
  checkArgumentLength(3);
  argumentRef(0, o);
  argumentAsFixnum(1, len_scm, len);
  argumentAsFixnum(2, flags_scm, flags);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_SOCKET_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("socket"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    {
      uint8_t* data = SG_NEW_ATOMIC2(uint8_t*, len);
      int result = Sg_SocketReceive(SG_SOCKET(o), data, len, flags);
      SG_RETURN = (Sg_MakeByteVectorFromU8Array(data, result));
    }
;
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_socket_impl_socket_recv_Stub, 3, 0, _sagittarius_socket_impl_socket_recv, SG_FALSE, NULL);

;
static SgObject _sagittarius_socket_impl_socket_shutdown(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("socket-shutdown");
  SgObject o;
  SgObject how_scm;
  int how;
  checkArgumentLength(2);
  argumentRef(0, o);
  argumentAsFixnum(1, how_scm, how);
  {
    SgObject SG_RETURN = SG_UNDEF;
    if (!(SG_SOCKET_P(o))) {
      Sg_WrongTypeOfArgumentViolation(procedureName, Sg_MakeString(UC("socket"), SG_LITERAL_STRING), o, SG_NIL);
      return SG_UNDEF;
    }
;
    Sg_SocketShutdown(SG_SOCKET(o), how);
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_socket_impl_socket_shutdown_Stub, 2, 0, _sagittarius_socket_impl_socket_shutdown, SG_FALSE, NULL);

;
static SgObject _sagittarius_socket_impl_socket_close(SgObject *args, int argc, void *data_)
{
  DeclareProcedureName("socket-close");
  SgObject o;
  checkArgumentLength(1);
  argumentRef(0, o);
  {
    SgObject SG_RETURN = SG_UNDEF;
    Sg_SocketClose(SG_SOCKET(o));
    return SG_RETURN;
  }
}
static SG_DEFINE_SUBR(_sagittarius_socket_impl_socket_close_Stub, 1, 0, _sagittarius_socket_impl_socket_close, SG_FALSE, NULL);

;
void Sg__Init_sagittarius_socket_impl()
{
  SgLibrary *lib = Sg_FindLibrary(Sg_Intern(Sg_MakeString(UC("(sagittarius socket impl)"), SG_LITERAL_STRING)), TRUE);
  SG_PROCEDURE_NAME(&_sagittarius_socket_impl_socket_send_Stub) = Sg_MakeString(UC("socket-send"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("socket-send"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_socket_impl_socket_send_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_socket_impl_make_client_socket_Stub) = Sg_MakeString(UC("make-client-socket"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-client-socket"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_socket_impl_make_client_socket_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_socket_impl_socket_recv_Stub) = Sg_MakeString(UC("socket-recv"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("socket-recv"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_socket_impl_socket_recv_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_socket_impl_make_server_socket_Stub) = Sg_MakeString(UC("make-server-socket"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("make-server-socket"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_socket_impl_make_server_socket_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_socket_impl_socket_shutdown_Stub) = Sg_MakeString(UC("socket-shutdown"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("socket-shutdown"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_socket_impl_socket_shutdown_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_socket_impl_socket3f_Stub) = Sg_MakeString(UC("socket?"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("socket?"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_socket_impl_socket3f_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_socket_impl_socket_close_Stub) = Sg_MakeString(UC("socket-close"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("socket-close"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_socket_impl_socket_close_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_socket_impl_socket_port_Stub) = Sg_MakeString(UC("socket-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("socket-port"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_socket_impl_socket_port_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_socket_impl_shutdown_output_port_Stub) = Sg_MakeString(UC("shutdown-output-port"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("shutdown-output-port"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_socket_impl_shutdown_output_port_Stub));
  SG_PROCEDURE_NAME(&_sagittarius_socket_impl_socket_accept_Stub) = Sg_MakeString(UC("socket-accept"), SG_LITERAL_STRING);
  Sg_InsertBinding(lib, Sg_Intern(Sg_MakeString(UC("socket-accept"), SG_LITERAL_STRING)), SG_OBJ(&_sagittarius_socket_impl_socket_accept_Stub));
}
