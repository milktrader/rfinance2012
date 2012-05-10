// #include <stdio.h>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>

SEXP doubleloop(SEXP distance, SEXP data){
 
  int i, j, n = length(data);
  SEXP dmatrix;

  PROTECT(dmatrix = allocVector(VECSXP, 1 :w));

  for (i = 0; i < n; i++) 
  for (j = 0; j < n; j++) 
  
  // perform Euclidean distance calc here
  // and store value in distance (empty matrix)
  
  UNPROTECT(1);
 
  return dmatrix;
}
