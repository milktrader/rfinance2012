#include <R.h> 
#include <Rinternals.h>

extern "C" SEXP helloWorld(void) { 
  Rprintf("Hello, World!\n"); 
  return R_NilValue;
}
