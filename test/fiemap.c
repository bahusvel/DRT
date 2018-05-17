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

#include "entities.h"

static drt_data_id DATA_ID = 0;
static drt_blob_id BLOB_ID = 0;
static drt_medium_id MEDIUM_ID = 1;

DRTMedium BLK_MEDIUM = (DRTMedium){.id = 0, .tags = {0, NULL}};

int DRT_LOG_FD = 0;

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

void dump_fiemap(struct fiemap *fiemap, const char *filename) {
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

#define DRT_WRITE_ENTITY(entity, type, fd)                                     \
	{                                                                          \
		size_t _size = size_drt_##type(&entity);                               \
		void *_buff = alloca(_size);                                           \
		enc_drt_##type(&entity, _buff);                                        \
		write(fd, _buff, _size);                                               \
	}

drt_blob_id encode_extent(drt_data_id data, drt_medium_id medium,
						  struct fiemap_extent *extent) {
	DRTBlob blob = {.id = BLOB_ID++,
					.data = data,
					.medium = medium,
					.offset = extent->fe_physical,
					.length = extent->fe_length};

	DRT_WRITE_ENTITY(blob, blob, DRT_LOG_FD);

	return blob.id;
}

void gen_fs_drt(int fd, DRTBlob *fileblob) {
	struct fiemap *fiemap = read_fiemap(fd);
	if (fiemap == NULL) {
		return;
	}

	dump_fiemap(fiemap, "file");

	struct drt_arg *in_blobs = NEW_DRT_ARGS(fiemap->fm_mapped_extents);

	for (int i = 0; i < fiemap->fm_mapped_extents; i++) {
		in_blobs[i].type = BLOB;
		in_blobs[i].blob = encode_extent(fileblob->data, BLK_MEDIUM.id,
										 &fiemap->fm_extents[i]);
	}

	DRTTransform trans = {.type = REVERSIBLE,
						  .reverse = {
							  .arg_count = fiemap->fm_mapped_extents,
							  .args = in_blobs,
							  .func = 1,
							  .out_count = 1,
							  .out_blobs = &fileblob->id,
						  }};

	DRT_WRITE_ENTITY(trans, transform, DRT_LOG_FD);
}

void gen_file_drt(const char *file) {
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

	struct drt_tags filetags = NEW_DRT_TAGS(1);
	SET_DRT_TAG_STRING(0, file, filetags);

	DRTMedium file_medium = {.id = MEDIUM_ID++, .tags = filetags};
	DRT_WRITE_ENTITY(file_medium, medium, DRT_LOG_FD);

	DRTData drtfile = {.id = DATA_ID++, .tags = filetags, .checksum = crc};
	DRT_WRITE_ENTITY(drtfile, data, DRT_LOG_FD);

	DRTBlob fileblob = {.id = BLOB_ID++,
						.data = drtfile.id,
						.medium = file_medium.id,
						.length = fstats.st_size};
	DRT_WRITE_ENTITY(fileblob, blob, DRT_LOG_FD);

	gen_fs_drt(fd, &fileblob);
}

void syntax(char **argv) {
	fprintf(stderr, "%s <drt_log_file> [file] ...\n", argv[0]);
}

int main(int argc, char **argv) {

	int i;

	if (argc < 3) {
		syntax(argv);
		exit(EXIT_FAILURE);
	}

	DRT_LOG_FD = open(argv[1], O_APPEND | O_CREAT | O_WRONLY);

	if (DRT_LOG_FD < 0) {
		perror("Cannot open drt_log_file");
		exit(EXIT_FAILURE);
	}
	DRT_WRITE_ENTITY(BLK_MEDIUM, medium, DRT_LOG_FD);

	for (i = 2; i < argc; i++) {
		gen_file_drt(argv[i]);
	}
	exit(EXIT_SUCCESS);
}
