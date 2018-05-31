#define _GNU_SOURCE
#include <fcntl.h>
#include <linux/fs.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

#define WRITE_SIZE 512

int main(int argc, char **argv) {
	if (argc < 2) {
		printf("Usage: %s <device_to_corrupt> <blocks_to_corrupt>", argv[0]);
	}
	int fd = open(argv[1], O_WRONLY | O_SYNC | O_DSYNC);
	if (fd < 0) {
		perror("Could not open device");
		exit(EXIT_FAILURE);
	}

	struct stat fstats;

	if (fstat(fd, &fstats) < 0) {
		fprintf(stderr, "Cannot stat file %s\n", argv[1]);
		perror("");
		return -1;
	}

	uint64_t size = fstats.st_size;
	if (S_ISBLK(fstats.st_mode))
		if (ioctl(fd, BLKGETSIZE64, &size) == -1) {
			perror("Could not get size of block device");
			exit(-1);
		}

	int n = atoi(argv[2]);
	if (n == 0) {
		fprintf(stderr, "Invalid number of bytes to corrupt %s\n", argv[2]);
		return -2;
	}

	srand(time(NULL)); // should only be called once

	unsigned char buf[WRITE_SIZE];

	for (int i = 0; i < n; i++) {
		long r = rand() % (size - WRITE_SIZE);
		printf("Corrupting: %lu\n", r);
		if (lseek(fd, r, SEEK_SET) == (off_t)-1)
			perror("Failed to seek");
		if (write(fd, buf, WRITE_SIZE) != WRITE_SIZE) {
			perror("Failed to write");
			return -1;
		}
	}

	close(fd);
	return 0;
}
