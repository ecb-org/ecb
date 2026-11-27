#include <stdio.h>

struct point { int x; int y; };

static int helper(int a) { return a * 2; }

int main(int argc, char **argv) {
  struct point p = {1, 2};
  return helper(p.x + p.y);
}
