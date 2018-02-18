#ifdef _MSC_VER
# include <windows.h>
# define sleep Sleep
#else
# include <unistd.h>
#endif

/* just make infinite loop */
int main(int argc, char **argv)
{
  /* emulation of infinite loop but in case of MSYS
     (means eventually ends and won't consume CPU)
     NB: on POSIX, it's 1000000 sec
     on Windows, it's 1000000 msec (1000 sec) 
     more than enough.
  */
  sleep(1000000);
  return 0;
}
