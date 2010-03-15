#include <unistd.h>
#include <sys/stat.h>
#include <sys/fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>

int main(int argc, char ** argv) {
  int fd = open("OkadWork.cf", O_RDWR);
  if (fd < 0) {
    printf("ERRor opening file\n");
  }

  struct stat st;
  fstat(fd, &st);

  void * buffer = mmap(0, st.st_size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
  printf("pointer: %p\n", ((char *)buffer + 6));
}
