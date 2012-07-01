#include <cstdio>
#define MATLIB_STANDALONE
#include <Rmath.h>
    
int main(void) {
  
  printf("N(0,1) 95th percentile %9.8f\n",
         qnorm(0.95, 0.0, 1.0, 1, 0));


}
