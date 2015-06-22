#include <string.h>
#include <stdint.h>

#ifndef _WIN32
# include <semaphore.h>
# include <fcntl.h>
# include <unistd.h>
# include <sys/file.h>
# include <sys/mman.h>
# include <sys/wait.h>

void ipc()
{
  int fd;
  char *ptr;

  fd = shm_open("/sagittarius-process", O_RDWR | O_CREAT, 0666);
  /* to avoid SIGBUS... */
  ftruncate(fd, 4096);
  ptr = (char *)mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
  strcpy(ptr, "process");
  munmap(ptr, 4096);
  close(fd);
}

int main()
{
  /* should already there */
  sem_t *sem1 = sem_open("/input", 0);
  ipc();
  sem_post(sem1);
  sem_close(sem1);
  return 0;
}
#else

# include <windows.h>

void ipc()
{
  HANDLE hMapFile = CreateFileMappingW(INVALID_HANDLE_VALUE,
				       NULL,
				       PAGE_READWRITE,
				       0,
				       4096,
				       L"/sagittarius-process");
  char *ptr = (char *)MapViewOfFile(hMapFile, FILE_MAP_ALL_ACCESS, 0,
				    0, 4096);
  SecureZeroMemory(ptr, 4096);
  strcpy(ptr, "process");
  UnmapViewOfFile(ptr);
  CloseHandle(hMapFile);
}

int main()
{
  HANDLE hSem = OpenSemaphoreW(SEMAPHORE_ALL_ACCESS, FALSE, L"/input");
  ipc();
  ReleaseSemaphore(hSem);
  CloseHandle(hSem);
}

#endif
