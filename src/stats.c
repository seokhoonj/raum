#include "raum.h"

// row min
SEXP row_min(SEXP x) {
  R_xlen_t m, n;
  SEXP z;

  m = nrows(x), n = ncols(x);

  switch(TYPEOF(x)){
    case INTSXP:{
      PROTECT(z = allocVector(INTSXP, m));
      int *xi = INTEGER(x), *xe = xi + m * n;
      int *zi = INTEGER(z), *ze = zi + m;
      int *xj, *zj;

      for (zj = zi; zj != ze; ++zj, ++xi)
        *zj = *xi;

      for (;xi != xe;) {
        for (zj = zi, xj = xi, xi += m; xj != xi; ++zj, ++xj) {
          *zj = (*zj < *xj) ? *zj : *xj;
        }
      }
      break;
    }
    case REALSXP:{
      PROTECT(z = allocVector(REALSXP, m));
      double *xi = REAL(x), *xe = xi + m * n;
      double *zi = REAL(z), *ze = zi + m;
      double *xj, *zj;

      for (zj = zi; zj != ze; ++zj, ++xi)
        *zj = *xi;

      for (;xi != xe;) {
        for (zj = zi, xj = xi, xi += m; xj != xi; ++zj, ++xj) {
          *zj = (*zj < *xj) ? *zj : *xj;
        }
      }
      break;
    }
    default:{
      error(_("invalid input"));
    }
  }
  UNPROTECT(1);
  return z;
}

// row max
SEXP row_max(SEXP x) {
  R_xlen_t m, n;
  SEXP z;

  m = nrows(x), n = ncols(x);

  switch(TYPEOF(x)){
    case INTSXP:{
      PROTECT(z = allocVector(INTSXP, m));
      int *xi = INTEGER(x), *xe = xi + m * n;
      int *zi = INTEGER(z), *ze = zi + m;
      int *xj, *zj;

      for (zj = zi; zj != ze; ++zj, ++xi)
        *zj = *xi;

      for (;xi != xe;) {
        for (zj = zi, xj = xi, xi += m; xj != xi; ++zj, ++xj) {
          *zj = (*zj > *xj) ? *zj : *xj;
        }
      }
      break;
    }
    case REALSXP:{
      PROTECT(z = allocVector(REALSXP, m));
      double *xi = REAL(x), *xe = xi + m * n;
      double *zi = REAL(z), *ze = zi + m;
      double *xj, *zj;

      for (zj = zi; zj != ze; ++zj, ++xi)
        *zj = *xi;

      for (;xi != xe;) {
        for (zj = zi, xj = xi, xi += m; xj != xi; ++zj, ++xj) {
          *zj = (*zj > *xj) ? *zj : *xj;
        }
      }
      break;
    }
    default:{
      error(_("invalid input"));
    }
  }
  UNPROTECT(1);
  return z;
}

// row sum
SEXP row_sum(SEXP x) {
  R_xlen_t m, n;
  SEXP z;

  m = nrows(x), n = ncols(x);

  switch(TYPEOF(x)){
    case INTSXP:{
      PROTECT(z = allocVector(INTSXP, m));
      int *xi = INTEGER(x), *xe = xi + m * n;
      int *zi = INTEGER(z), *ze = zi + m;
      int *xj, *zj;

      for (zj = zi; zj != ze; ++zj, ++xi)
        *zj = *xi;

      for (;xi != xe;) {
        for (zj = zi, xj = xi, xi += m; xj != xi; ++zj, ++xj) {
          *zj = (*zj != NA_INTEGER) ? *zj + *xj : NA_INTEGER;
        }
      }
      break;
    }
    case REALSXP:{
      PROTECT(z = allocVector(REALSXP, m));
      double *xi = REAL(x), *xe = xi + m * n;
      double *zi = REAL(z), *ze = zi + m;
      double *xj, *zj;

      for (zj = zi; zj != ze; ++zj, ++xi)
        *zj = *xi;

      for (;xi != xe;) {
        for (zj = zi, xj = xi, xi += m; xj != xi; ++zj, ++xj) {
          *zj = (*zj != NA_REAL) ? *zj + *xj : NA_REAL;
        }
      }
      break;
    }
    default:{
      error(_("invalid input"));
    }
  }
  UNPROTECT(1);
  return z;
}
