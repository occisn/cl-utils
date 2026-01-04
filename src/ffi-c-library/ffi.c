#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// A simple struct
typedef struct {
  int x;
  int y;
} Point;

// Function 1: Add two integers
int add(int a, int b)
{
  return a + b;
}

// Function 2: Multiply two floats
float multiply_floats(float a, float b)
{
  return a * b;
}

// Function 3: Print a string
void print_string(char *str)
{
  printf("From C: %s\n", str);
}

// Function 4: Modify a Point struct
void move_point(Point *p, int dx, int dy)
{
  p->x += dx;
  p->y += dy;
}

// Function 5: Sum an array of integers
int sum_array(int *arr, int length)
{
  int sum = 0;
  for (int i = 0; i < length; i++) {
    sum += arr[i];
  }
  return sum;
}

// Function 6: Double each element of an array and return a new array
int *double_array(int *arr, int length)
{
  int *result = (int *)malloc(length * sizeof(int));
  for (int i = 0; i < length; i++) {
    result[i] = arr[i] * 2;
  }
  return result;
}

// Function 7: Free the memory allocated by double_array
void free_double_array(int *arr)
{
  free(arr);
}

// end
