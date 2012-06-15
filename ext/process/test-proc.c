#include <stdio.h>

/* just echo the given argument */
int main(int argc, char **argv)
{
  if (argc != 2) {
    puts("error");
    return -1;
  }
  puts(argv[1]);
  return 0;
}

