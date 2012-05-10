#include <stdio.h>
#include <R.h>


void loops(int *nrow) {

  int i, j;

  for(i = 1; i < *nrow + 1; i++)
  for(j = 1; j < *nrow + 1; j++)

  Rprintf("Parameter one is %d and Parameter two is %d\n", i, j);


}
