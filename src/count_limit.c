#include "raum.h"

SEXP count_limit(SEXP x, SEXP limit, SEXP waiting) {
  // limit: 90, waiting: 90
  // ex) x = c(rep(1, 5), rep(0, 5), rep(1, 8), rep(0, 5), rep(1, 5))
  // ex) count_limit(x, 3, 6)
  // x = c(1,1,1,1,1,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,1,1,1,1,1)
  // c(1,1,1,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0)
  // waiting periods == elimination periods == qualifying periods
  // printf('%d\n', isInteger(x));
  int n = XLENGTH(x);
  SEXP z = PROTECT(allocVector(REALSXP, n));
  double *xs = REAL(x), *xe = xs + XLENGTH(x);
  double *zs = REAL(z);
  double *xi = xs, *zi = zs;
  double lim = asReal(limit), wait = asReal(waiting);

  double sum = 0, len = 0;
  while (xi < xe) {
    if (sum < lim) {
      sum += *xi;
      *zi = *xi;
    } else if (len < wait) {
      *zi = 0;
      len += 1;
    } else {
      sum = 0, len = 0;
      sum += *xi;
      *zi = *xi;
    }
    ++zi, ++xi;
  }
  UNPROTECT(1);
  return z;
}
