#include "raum.h"

SEXP print_array(SEXP x) {
  R_xlen_t i, len;
  len = XLENGTH(x);
  switch(TYPEOF(x)) {
  case LGLSXP:{
    int *ix = LOGICAL(x);
    for (i = 0; i < len; ++i) printf("%d ", ix[i]);
  } break;
  case INTSXP:{
    int *ix = INTEGER(x);
    for (i = 0; i < len; ++i) printf("%d ", ix[i]);
  } break;
  case REALSXP:{
    double *ix = REAL(x);
    for (i = 0; i < len; ++i) printf("%f ", ix[i]);
  } break;
  case CPLXSXP:{
    Rcomplex *ix = COMPLEX(x);
    for (i = 0; i < len; ++i) printf("%f+%f ", ix[i].r, ix[i].i);
  } break;
  case STRSXP:{
    SEXP *ix = STRING_PTR(x);
    for (i = 0; i < len; ++i) printf("%s ", CHAR(ix[i]));
  } break;
  default:
    error(_("invalid input"));
  }
  return R_NilValue;
}
