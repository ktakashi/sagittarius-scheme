/* Internal, backend-independent PQC binding contracts. */
#ifndef SAGITTARIUS_PQC_H_
#define SAGITTARIUS_PQC_H_

#include <sagittarius.h>

SgObject Sg_PQCSizes(SgWord family, SgWord parameter);
SgObject Sg_PQCExpand(SgWord family, SgWord parameter, SgObject seed);
SgObject Sg_PQCImport(SgWord family, SgWord parameter, SgObject key, int privateP);
SgObject Sg_PQCEncapsulate(SgWord parameter, SgObject key, SgObject randomness);
SgObject Sg_PQCDecapsulate(SgWord parameter, SgObject key, SgObject ciphertext);
SgObject Sg_PQCSign(SgWord family, SgWord parameter, SgObject key, SgObject message,
                   SgObject context, SgObject randomness, int representativeP);
SgObject Sg_PQCVerify(SgWord family, SgWord parameter, SgObject key, SgObject message,
                     SgObject context, SgObject signature, int representativeP);
SgObject Sg_PQCProtect(SgObject bytes);
SgObject Sg_PQCClear(SgObject bytes);

#endif
