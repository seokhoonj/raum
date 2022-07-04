#include "raum.h"

SEXP uni(SEXP x, SEXP uniq, SEXP fromLast) {
  const bool buniq = asLogical(uniq);
  const bool pfromLast = asLogical(fromLast);
  const R_xlen_t n = XLENGTH(x);
  const SEXPTYPE tx = TYPEOF(x);
  int K;
  size_t M;

  const size_t n2 = 2U * (size_t) n; // 2 times of n
  M = 256; // 2^8 = 256
  K = 8;   // 2^8 = 256
  while (M < n2) {
    // #define HASH(key, K) (3141592653U * (unsigned int)(key) >> (32 - (K))), 2^31 < 3141592653U < 2^32, 3 * 107 * 9786893
    M *= 2; // 256, 512, 1024, 2048, 4096 ... 2^exponential
    K++;    //   8,   9    10,   11,   12 ...   exponential
  }
  // n < M < n2

  R_xlen_t count = 0;
  int* h = (int*)Calloc(M, int); // M is a length of hash table.
  SEXP ans = buniq ? R_NilValue : PROTECT(allocVector(LGLSXP, n));
  int* pans = buniq ? (int*)Calloc(n, int) : LOGICAL(x);

  switch(tx) {
  case INTSXP:{
    const int *px = INTEGER(x);
    size_t id = 0;
    if (buniq) {
      if (pfromLast) {
        for (int i = n-1; i > -1; --i) {
          id = (px[i] == NA_INTEGER) ? 0 : HASH(px[i], K); // NA: id = 0, NOT NA: id = HASH(x[i], K)
          while (h[id]) { // 0: not implemented, !0: repeat
            if (px[h[id]-1] == px[i]) {
              goto iblt;
            }
            id++; id %= M; // n < M < n2, so mostly id == id after id++
          }
          h[id] = (int) i+1;
          pans[i]++;
          count++;
          iblt:;
        }
      } else {
        printf("========================================\n");
        for (int i = 0; i < n; ++i) {
          id = (px[i] == NA_INTEGER) ? 0 : HASH(px[i], K);
          printf("i = [%d], px[%d] = [%d]\n", i, i, px[i]);
          printf("========================================\n");
          printf("id = [%ld], h[%ld] = [%d]\n", id, id, h[id]);
          while (h[id]) {
            printf("id = [%ld], h[%ld] = [%d], h[%ld]-1 = [%d]\n", id, id, h[id], id, h[id]-1);
            if (px[h[id]-1] == px[i]) {
              goto ibl;
            }
            id++; id %= M;
          }
          h[id] = (int) i+1; printf("h[%ld] = [%d]\n", id, i+1);
          pans[i]++; printf("pans[%d] = [%d]\n", i, pans[i]); // ans = {0, 0, 0, ..., 0} -> ans = {0, 1, 0, ..., 1}
          count++;
          ibl:;
          printf("========================================\n");
        }
      }
      printf("count = [%ld]\n", count);
      printf("========================================\n");
      for (int i = 0; i < M; ++i) printf("%d,", h[i]); // h contains index of unique x
      printf("\n========================================\n");
      for (int i = 0; i < n; ++i) printf("%d,", pans[i]);
      printf("\n========================================\n");
      Free(h);
      SEXP indx = PROTECT(allocVector(tx, count));
      size_t ct = 0;
      int* py = INTEGER(indx);
      for (R_xlen_t i = 0; ct < count; ++i) {
        if (pans[i]) { // if pans[i] != 0
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
          h[id] = (int) i+1;
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
          h[id] = (int) i+1;
          pans[i] = 0;
          count++;
          ibld:;
        }
      }
      Free(h);
    }
    break;
    }
  }
  UNPROTECT(1);
  return ans;
}
