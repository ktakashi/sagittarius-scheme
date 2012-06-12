#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "$header$"

extern void Sg__Init_$stub-init$();

SG_EXTENSION_ENTRY void CDECL Sg_Init_$flat-name$()
{
  /* Initialize the package DSO */
  SG_INIT_EXTENSION($package-name$);
  /* Call stub initialiser, the stub library will be automatically created. */
  Sg__Init_$stub-init$();
  /* Do your initialisation here. */
}
