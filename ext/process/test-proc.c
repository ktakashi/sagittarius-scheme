#include <stdio.h>

/* just echo the given argument */
int main(int argc, char **argv)
{
  if (argc != 2) {
    fputs("error", stderr);
    return -1;
  }
  fputs(argv[1], stdout);
  fflush(stdout);
  return 0;
}

