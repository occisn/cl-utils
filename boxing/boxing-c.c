#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <time.h>

// pi = 3.14159265358979323846...

double sub(uint64_t start, uint64_t end) {
  double tmp = 0.0;
  for (uint64_t i = start; i <= end - 2; i += 2) {
    tmp += 1 / (2.0 * i + 1.0);
    tmp -= 1 / (2.0 * i + 3.0);
  }
  return tmp;
}

int main(int argc, [[maybe_unused]] char* argv[argc+1]) {
  struct timespec start, end;
  double duration;
  clock_gettime(CLOCK_MONOTONIC, &start);

  double tmp = 0.0;
  for (uint64_t i = 0; i < 100000000; i++) {
    uint64_t start = i * 100;
    uint64_t end = start + 100;
    tmp = tmp + sub(start, end);
  }
  tmp = tmp * 4.0;
  printf("Result: %.20f\n", tmp);
  
  clock_gettime(CLOCK_MONOTONIC, &end);
  duration = (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) / 1e9;
  printf("Duration: %f seconds\n", duration);
  return EXIT_SUCCESS;
}

// end
