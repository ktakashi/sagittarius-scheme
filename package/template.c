#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "$header$"

extern void Sg__Init_$flat-name$lib(SgLibrary *lib);

SG_EXTENSION_ENTRY void CDECL Sg_Init_$flat-name$()
{
  SgLibrary *lib;
  /* Initialize the package DSO */
  SG_INIT_EXTENSION($package-name$);
  lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("$library-name$"),
				  FALSE));
  /* Call stub initialiser, the stub library will be automatically created. */
  Sg__Init_$flat-name$lib(lib);
  /* Do your initialisation here. */
}
