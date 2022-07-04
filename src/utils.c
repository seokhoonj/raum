#include "raum.h"

SEXP r2c(SEXP x) {
  size_t size = XLENGTH(x);
  switch(TYPEOF(x)) {
  case LGLSXP:{
    int* ix = INTEGER(x);
    for (int i = 0; i < size; ++i) {
      printf("%d ", ix[i]);
    }
  } break;
  case INTSXP:{
    int* ix = INTEGER(x);
    for (int i = 0; i < size; ++i) {
      printf("%d ", ix[i]);
    }
  } break;
  case REALSXP:{
    double* ix = REAL(x);
    for (int i = 0; i < size; ++i) {
      printf("%f ", ix[i]);
    }
  } break;
  case STRSXP:{
    SEXP *ix = STRING_PTR(x);
    for (int i = 0; i < size; ++i) {
      printf("%s ", CHAR(ix[i]));
    }
  } break;
  default:
    error(_("invalid input"));
  }
  printf("\n");
  return x;
}
