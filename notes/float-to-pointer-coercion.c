#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <time.h>
#include <math.h>

// pi = 3.14159265358979323846...

const uint64_t iterations  =  1000000000;   // 9 zeros

int main(int argc, [[maybe_unused]] char* argv[argc+1]) {
  struct timespec start, end;
  double duration;
  clock_gettime(CLOCK_MONOTONIC, &start);
  
  double tmp = 0.0;
  double sign = 0.0;
  double denom = 0.0;
  for (uint64_t i = 0; i <= iterations; i++) {
    if (i % 2 == 0) {
      sign = 1.0;
    } else {
      sign = -1.0;
    }
    denom = (2.0 * i + 1.0);
    tmp = tmp + sign / denom;
  }
  tmp = 4 * tmp;
  
  clock_gettime(CLOCK_MONOTONIC, &end);
  duration = (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) / 1e9;
  printf("Duration: %f seconds\n", duration);
  
  printf("Result: %.20f\n", tmp);
  
    return EXIT_SUCCESS;
}

// end
