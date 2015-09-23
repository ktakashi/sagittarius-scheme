#include <stdio.h>
#include <string.h>
#ifdef _MSC_VER
# include <windows>
# define sleep Sleep
#else
# include <unistd.h>
#endif

/* just echo the given argument */
int main(int argc, char **argv)
{
  if (argc != 2) {
    fputs("error", stderr);
    return -1;
  }
  if (strncmp("sleep", argv[1], strlen("sleep")) == 0) {
    /* don't wake up! 
       NB: on POSIX, it's 1000000 sec
           on Windows, it's 1000000 msec (1000 sec) 
       more than enough.
    */
    sleep(1000000);
  } else {
    fputs(argv[1], stdout);
    fflush(stdout);
  }
  return 0;
}

