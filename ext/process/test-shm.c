#include <string.h>
#include <stdint.h>

#ifndef _WIN32
# include <unistd.h>
# include <sys/file.h>
# include <sys/mman.h>
# include <sys/wait.h>

int main()
{
  int fd;
  uint8_t *ptr;

  fd = shm_open("/sagittarius-process", O_RDWR, 0666);
  /* to avoid SIGBUS... */
  /* ftruncate(fd, 4096); */
  ptr = (uint8_t *)mmap(NULL, 4096, PROT_READ | PROT_WRITE, 
			MAP_SHARED, fd, 0);
  close(fd);
  strcpy((char *)ptr, "process");
  munmap(ptr, 4096);
  return 0;
}

#else
# include <windows.h>

int main()
{
  HANDLE hMapFile = OpenFileMappingW(FILE_MAP_ALL_ACCESS, FALSE,
				     _T("/sagittarius-process"));
  uint8_t *ptr = (uint8_t *)MapViewOfFile(hMapFile, FILE_MAP_ALL_ACCESS,
					  0, 0, 4096);
  strcpy((char *)ptr, "process");
  UnmapViewOfFile(ptr);
  CloseHandle(hMapFile);
  return 0;
}

#endif

