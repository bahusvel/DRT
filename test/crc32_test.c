#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>

const char *path = "ext4fs/fiemap.c";

uint32_t crc32(uint32_t crc, const uint8_t *data, unsigned int length);

int main() {

	int fd;
	if ((fd = open(path, O_RDONLY)) < 0) {
		fprintf(stderr, "Cannot open file %s\n", path);
		perror("");
		return -1;
	}

	struct stat fstats;

	if (fstat(fd, &fstats) < 0) {
		fprintf(stderr, "Cannot stat file %s\n", path);
		perror("");
		return -1;
	}

	uint32_t crc = 0;
	uint8_t buf[4096];

	for (long i = 0; i < fstats.st_size;) {
		int n = read(fd, buf, 4096);
		crc = crc32(crc, buf, n);
		i += n;
	}

	printf("%08x\n", crc);
}
