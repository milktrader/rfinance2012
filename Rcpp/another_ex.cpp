#include <Rcpp.h>

extern "C" SEXP foo() {

  StringVector res(2);
  res[0] = "foo";
  res[1] = "bar";
  return res;

}
