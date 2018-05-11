#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <linux/fiemap.h>
#include <linux/fs.h>

void syntax(char **argv) { fprintf(stderr, "%s [filename]...\n", argv[0]); }
uint32_t crc32c(uint32_t crc, const uint8_t *data, unsigned int length);

struct fiemap *read_fiemap(int fd) {
	struct fiemap *fiemap;
	int extents_size;

	if ((fiemap = (struct fiemap *)malloc(sizeof(struct fiemap))) == NULL) {
		fprintf(stderr, "Out of memory allocating fiemap\n");
		return NULL;
	}
	memset(fiemap, 0, sizeof(struct fiemap));

	fiemap->fm_start = 0;
	fiemap->fm_length = ~0; /* Lazy */
	fiemap->fm_flags = FIEMAP_FLAG_SYNC;
	fiemap->fm_extent_count = 0;
	fiemap->fm_mapped_extents = 0;

	/* Find out how many extents there are */
	if (ioctl(fd, FS_IOC_FIEMAP, fiemap) < 0) {
		fprintf(stderr, "fiemap ioctl() failed\n");
		return NULL;
	}

	/* Read in the extents */
	extents_size = sizeof(struct fiemap_extent) * (fiemap->fm_mapped_extents);

	/* Resize fiemap to allow us to read in the extents */
	if ((fiemap = (struct fiemap *)realloc(fiemap, sizeof(struct fiemap) +
													   extents_size)) == NULL) {
		fprintf(stderr, "Out of memory allocating fiemap\n");
		return NULL;
	}

	memset(fiemap->fm_extents, 0, extents_size);
	fiemap->fm_extent_count = fiemap->fm_mapped_extents;
	fiemap->fm_mapped_extents = 0;

	if (ioctl(fd, FS_IOC_FIEMAP, fiemap) < 0) {
		fprintf(stderr, "fiemap ioctl() failed\n");
		return NULL;
	}

	return fiemap;
}

void gen_drt(const char *file) {
	int fd;
	if ((fd = open(file, O_RDONLY)) < 0) {
		fprintf(stderr, "Cannot open file %s\n", file);
		perror("");
		return;
	}

	struct stat fstats;

	if (fstat(fd, &fstats) < 0) {
		fprintf(stderr, "Cannot stat file %s\n", file);
		perror("");
		return;
	}

	uint32_t crc = 0;
	uint8_t buf[4096];

	for (long i = 0; i < fstats.st_size; i += 4096) {
		int n = read(fd, &buf, 4096);
		crc = crc32c(crc, buf, 4096);
		i += n;
	}

	struct fiemap *fiemap = read_fiemap(fd);
	if (fiemap == NULL) {
		return;
	}

	for (int i = 0; i < fiemap->fm_mapped_extents; i++) {
		struct fiemap_extent *extent = &fiemap->fm_extents[i];
		printf("%d:\t%-16.16llx %-16.16llx %-16.16llx %-4.4x\n", i,
			   extent->fe_logical, extent->fe_physical, extent->fe_length,
			   extent->fe_flags);
	}
}

void dump_fiemap(struct fiemap *fiemap, char *filename) {
	int i;

	printf("File %s has %d extents:\n", filename, fiemap->fm_mapped_extents);

	printf("#\tLogical          Physical         Length           Flags\n");
	for (i = 0; i < fiemap->fm_mapped_extents; i++) {
		printf("%d:\t%-16.16llx %-16.16llx %-16.16llx %-4.4x\n", i,
			   fiemap->fm_extents[i].fe_logical,
			   fiemap->fm_extents[i].fe_physical,
			   fiemap->fm_extents[i].fe_length, fiemap->fm_extents[i].fe_flags);
	}
	printf("\n");
}

int main(int argc, char **argv) {
	int i;

	if (argc < 2) {
		syntax(argv);
		exit(EXIT_FAILURE);
	}

	for (i = 1; i < argc; i++) {
		int fd;

		if ((fd = open(argv[i], O_RDONLY)) < 0) {
			fprintf(stderr, "Cannot open file %s\n", argv[i]);
		} else {
			struct fiemap *fiemap;

			if ((fiemap = read_fiemap(fd)) != NULL)
				dump_fiemap(fiemap, argv[i]);
			close(fd);
		}
	}
	exit(EXIT_SUCCESS);
}
