#include "raum.h"

SEXP dupVecR(SEXP x, SEXP uniq, SEXP fromLast) {
  const bool buniq = asLogical(uniq);
  if(!IS_BOOL(fromLast)) {
    error("Argument 'fromLast' must be TRUE or FALSE and length 1.");
  }
  const bool pfromLast = asLogical(fromLast);
  if (isFactor(x) && buniq) {
    const int len = LENGTH(PROTECT(getAttrib(x, R_LevelsSymbol)));
    UNPROTECT(1);
    bool *count = (bool*)Calloc(len+1,bool);
    const int *px = INTEGER(x);
    const int xlen = LENGTH(x);
    SEXP ans = PROTECT(allocVector(INTSXP, len));
    copyMostAttrib(x, ans);
    int *pans = INTEGER(ans);
    if (pfromLast) {
      int j = len-1;
      for (int i = xlen-1; i >= 0; --i) {
        if (!count[px[i]]) {
          pans[j--] = px[i];
          if (j == -1)
            break;
          count[px[i]] = true;
        }
      }
      if (j != -1) {
        SEXP ans2 = PROTECT(allocVector(INTSXP, len-j-1));
        copyMostAttrib(x, ans2);
        memcpy(INTEGER(ans2),pans+j+1,(len-j-1)*sizeof(int));
        Free(count);
        UNPROTECT(2);
        return ans2;
      }
    } else {
      int j = 0;
      for (int i = 0; i < xlen; ++i) {
        if (!count[px[i]]) {
          pans[j++] = px[i];
          if (j == len)
            break;
          count[px[i]] = true;
        }
      }
      if (j != len) {
        SETLENGTH(ans, j);
      }
    }
    Free(count);
    UNPROTECT(1);
    return ans;
  }
  if (isLogical(x) && buniq) {
    bool *count = (bool*)Calloc(3,bool);
    const int *px = LOGICAL(x);
    const int xlen = LENGTH(x);
    SEXP ans = PROTECT(allocVector(LGLSXP, 3));
    copyMostAttrib(x, ans);
    int *pans = LOGICAL(ans);
    if (pfromLast) {
      int j = 2;
      for (int i = xlen-1; i >= 0; --i) {
        const int cs = px[i] == NA_LOGICAL ? 2 : px[i];
        if (!count[cs]) {
          pans[j--] = cs == 2 ? NA_LOGICAL : px[i];
          if (j == -1)
            break;
          count[cs] = true;
        }
      }
      if (j != -1) {
        SEXP ans2 = PROTECT(allocVector(LGLSXP, 2-j));
        copyMostAttrib(x, ans2);
        memcpy(LOGICAL(ans2),pans+j+1,(2-j)*sizeof(int));
        Free(count);
        UNPROTECT(2);
        return ans2;
      }
    } else {
      int j = 0;
      for (int i = 0; i < xlen; ++i) {
        const int cs = px[i] == NA_LOGICAL ? 2 : px[i];
        if (!count[cs]) {
          pans[j++] = cs == 2 ? NA_LOGICAL : px[i];
          if (j == 3)
            break;
          count[cs] = true;
        }
      }
      if (j != 3) {
        SETLENGTH(ans, j);
      }
    }
    Free(count);
    UNPROTECT(1);
    return ans;
  }
  const R_xlen_t n = xlength(x);
  const SEXPTYPE tx = UTYPEOF(x);
  int K;
  size_t M;
  if (tx == INTSXP || tx == STRSXP || tx == REALSXP || tx == CPLXSXP ) {
    if(n >= 1073741824) {
      error("Length of 'x' is too large. (Long vector not supported yet)"); // # nocov
    }
    const size_t n2 = 2U * (size_t) n;
    M = 256;
    K = 8;
    while (M < n2) {
      M *= 2;
      K++;
    }
  } else if (tx == LGLSXP) {
    M = 4;
    K = 2;
  } else {
    error("Type %s is not supported.", type2char(tx));
  }
  R_xlen_t count = 0;
  int *h = (int*)Calloc(M, int);
  SEXP ans = buniq ? R_NilValue : PROTECT(allocVector(LGLSXP, n));
  int *pans = buniq ? (int*)Calloc(n, int) : LOGICAL(ans);
  switch (tx) {
  case LGLSXP: {
    const int *px = LOGICAL(x);
    size_t id = 0;
    if (pfromLast) {
      for (int i = n-1; i > -1; --i) {
        id = (px[i] == NA_LOGICAL) ? 2U : (size_t) px[i];
        while (h[id]) {
          if (px[h[id]-1]==px[i]) {
            pans[i]=1;
            goto lbldt;
          }
          id++; id %= M; // # nocov
        }
        h[id] = (int) i + 1;
        pans[i] = 0;
        count++;
        lbldt:;
      }
    } else {
      for (int i = 0; i < n; ++i) {
        id = (px[i] == NA_LOGICAL) ? 2U : (size_t) px[i];
        while (h[id]) {
          if (px[h[id]-1]==px[i]) {
            pans[i]=1;
            goto lbld;
          }
          id++; id %= M; // # nocov
        }
        h[id] = (int) i + 1;
        pans[i] = 0;
        count++;
        lbld:;
      }
    }
    Free(h);
  } break;
  case INTSXP: { // think about factor and levels number
    const int *px = INTEGER(x);
    size_t id = 0;
    if (buniq) {
      if (pfromLast) {
        for (int i = n-1; i > -1; --i) {
          id = (px[i] == NA_INTEGER) ? 0 : HASH(px[i], K);
          while (h[id]) {
            if (px[h[id]-1]==px[i]) {
              goto iblt;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]++;
          count++;
          iblt:;
        }
      } else {
        for (int i = 0; i < n; ++i) {
          id = (px[i] == NA_INTEGER) ? 0 : HASH(px[i], K);
          while (h[id]) {
            if (px[h[id]-1]==px[i]) {
              goto ibl;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]++;
          count++;
          ibl:;
        }
      }
      Free(h);
      SEXP indx = PROTECT(allocVector(tx, count));
      size_t ct = 0;
      int *py = INTEGER(indx);
      for (R_xlen_t i = 0; ct < count; ++i) {
        if (pans[i]) {
          py[ct++] = px[i];
        }
      }
      Free(pans);
      copyMostAttrib(x, indx);
      UNPROTECT(1);
      return indx;
    } else {
      if (pfromLast) {
        for (int i = n-1; i > -1; --i) {
          id = (px[i] == NA_INTEGER) ? 0 : HASH(px[i], K);
          while (h[id]) {
            if (px[h[id]-1]==px[i]) {
              pans[i]=1;
              goto ibldt;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i] = 0;
          count++;
          ibldt:;
        }
      } else {
        for (int i = 0; i < n; ++i) {
          id = (px[i] == NA_INTEGER) ? 0 : HASH(px[i], K);
          while (h[id]) {
            if (px[h[id]-1]==px[i]) {
              pans[i]=1;
              goto ibld;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i] = 0;
          count++;
          ibld:;
        }
      }
      Free(h);
    }
  } break;
  case REALSXP: {
    const double *px = REAL(x);
    size_t id = 0;
    union uno tpv;
    if (buniq) {
      if (pfromLast) {
        for (int i = n-1; i > -1; --i) {
          tpv.d = R_IsNA(px[i]) ? NA_REAL : (R_IsNaN(px[i]) ? R_NaN :px[i]);
          id = HASH(tpv.u[0] + tpv.u[1], K);
          while (h[id]) {
            if (REQUAL(px[h[id]-1], px[i])) {
              goto rblt;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]++;
          count++;
          rblt:;
        }
      } else {
        for (int i = 0; i < n; ++i) {
          tpv.d = R_IsNA(px[i]) ? NA_REAL : (R_IsNaN(px[i]) ? R_NaN :px[i]);
          id = HASH(tpv.u[0] + tpv.u[1], K);
          while (h[id]) {
            if (REQUAL(px[h[id]-1], px[i])) {
              goto rbl;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]++;
          count++;
          rbl:;
        }
      }
      Free(h);
      SEXP indx = PROTECT(allocVector(tx, count));
      size_t ct = 0;
      double *py = REAL(indx);
      for (R_xlen_t i = 0; ct < count; ++i) {
        if (pans[i]) {
          py[ct++] = px[i];
        }
      }
      Free(pans);
      copyMostAttrib(x, indx);
      UNPROTECT(1);
      return indx;
    } else {
      if (pfromLast) {
        for (int i = n-1; i > -1; --i) {
          tpv.d = R_IsNA(px[i]) ? NA_REAL : (R_IsNaN(px[i]) ? R_NaN :px[i]);
          id = HASH(tpv.u[0] + tpv.u[1], K);
          while (h[id]) {
            if (REQUAL(px[h[id]-1], px[i])) {
              pans[i]=1;
              goto rbldt;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]=0;
          count++;
          rbldt:;
        }
      } else {
        for (int i = 0; i < n; ++i) {
          tpv.d = R_IsNA(px[i]) ? NA_REAL : (R_IsNaN(px[i]) ? R_NaN :px[i]);
          id = HASH(tpv.u[0] + tpv.u[1], K);
          while (h[id]) {
            if (REQUAL(px[h[id]-1], px[i])) {
              pans[i]=1;
              goto rbld;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]=0;
          count++;
          rbld:;
        }
      }
      Free(h);
    }
  } break;
  case STRSXP: {
    const SEXP *px = STRING_PTR(x);
    size_t id = 0;
    if (buniq) {
      if (pfromLast) {
        for (int i = n-1; i > -1; --i) {
          id = HASH(((intptr_t) px[i] & 0xffffffff), K);
          while (h[id]) {
            if (px[h[id] - 1]==px[i]) {
              goto sblt;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]++;
          count++;
          sblt:;
        }
      } else {
        for (int i = 0; i < n; ++i) {
          id = HASH(((intptr_t) px[i] & 0xffffffff), K);
          while (h[id]) {
            if (px[h[id] - 1]==px[i]) {
              goto sbl;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]++;
          count++;
          sbl:;
        }
      }
      Free(h);
      SEXP indx = PROTECT(allocVector(tx, count));
      R_xlen_t ct = 0;
      for (int i = 0; ct < count; ++i) {
        if (pans[i]) {
          SET_STRING_ELT(indx, ct++, px[i]);
        }
      }
      Free(pans);
      copyMostAttrib(x, indx);
      UNPROTECT(1);
      return indx;
    } else {
      if (pfromLast) {
        for (int i = n-1; i > -1; --i) {
          id = HASH(((intptr_t) px[i] & 0xffffffff), K);
          while (h[id]) {
            if (px[h[id] - 1]==px[i]) {
              pans[i]=1;
              goto sbldt;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]=0;
          count++;
          sbldt:;
        }
      } else {
        for (int i = 0; i < n; ++i) {
          id = HASH(((intptr_t) px[i] & 0xffffffff), K);
          while (h[id]) {
            if (px[h[id] - 1]==px[i]) {
              pans[i]=1;
              goto sbld;
            }
            id++; id %= M;
          }
          h[id] = (int) i + 1;
          pans[i]=0;
          count++;
          sbld:;
        }
      }
      Free(h);
    }
  } break;
  }
  UNPROTECT(1);
  return ans;
}
