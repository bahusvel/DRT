#include <fcntl.h>
#include <libgen.h>
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

DRTMedium BLK_MEDIUM = (DRTMedium){.id = 0, .tags = {0, NULL}};

int DRT_LOG_FD = 0;

uint32_t crc32(uint32_t crc, const uint8_t *data, unsigned int length);

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

void gen_fs_drt(int fd, DRTData *data) {
	struct fiemap *fiemap = read_fiemap(fd);
	if (fiemap == NULL) {
		return;
	}

	size_t remaining_length = data->size;

	dump_fiemap(fiemap, "file");

	struct drt_arg *in_blobs = NEW_DRT_ARGS(fiemap->fm_mapped_extents);
	struct drt_blob *declares =
		malloc(sizeof(DRTBlob) * fiemap->fm_mapped_extents);

	for (int i = 0; i < fiemap->fm_mapped_extents; i++) {
		in_blobs[i].type = BLOB;

		struct fiemap_extent *extent = &fiemap->fm_extents[i];

		declares[i] = (DRTBlob){.id = BLOB_ID++,
								.medium = BLK_MEDIUM.id,
								.offset = extent->fe_physical,
								.length = remaining_length < extent->fe_length
											  ? remaining_length
											  : extent->fe_length};

		in_blobs[i].blob = declares[i].id;
		remaining_length -= extent->fe_length;
	}

	struct drt_tran tran = {
		.arg_count = fiemap->fm_mapped_extents,
		.args = in_blobs,
		.func = 1,
		.out_count = 1,
		.out_blobs = &data->iblobid,
	};

	DRTTransform trans = {.type = REVERSIBLE,
						  .dec_len = fiemap->fm_mapped_extents,
						  .declares = declares,
						  .reverse = &tran};

	DRT_WRITE_ENTITY(trans, transform, DRT_LOG_FD);
	free(declares);
}

void gen_file_drt(const char *path) {
	int fd;
	if ((fd = open(path, O_RDONLY)) < 0) {
		fprintf(stderr, "Cannot open file %s\n", path);
		perror("");
		return;
	}

	struct stat fstats;

	if (fstat(fd, &fstats) < 0) {
		fprintf(stderr, "Cannot stat file %s\n", path);
		perror("");
		return;
	}

	uint32_t crc = 0;
	uint8_t buf[4096];

	for (long i = 0; i < fstats.st_size;) {
		int n = read(fd, buf, 4096);
		crc = crc32(crc, buf, n);
		i += n;
	}

	char *pathcp = alloca(strlen(path));
	strcpy(pathcp, path);
	char *bs_name = basename(pathcp);

	char *name_tag = alloca(strlen("name:") + strlen(bs_name) + 1);
	strcpy(name_tag, "name:");
	strcat(name_tag, bs_name);

	char *path_tag = alloca(strlen("path:") + strlen(path) + 1);
	strcpy(path_tag, "path:");
	strcat(path_tag, path);

	struct drt_tags filetags = NEW_DRT_TAGS(2);
	SET_DRT_TAG_STRING(0, name_tag, filetags);
	SET_DRT_TAG_STRING(1, path_tag, filetags);

	DRTData file = {.id = DATA_ID++,
					.iblobid = BLOB_ID++,
					.size = fstats.st_size,
					.tags = filetags,
					.checksum = crc};

	DRT_WRITE_ENTITY(file, data, DRT_LOG_FD);

	gen_fs_drt(fd, &file);
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

	DRT_LOG_FD = open(argv[1], O_APPEND | O_CREAT | O_WRONLY, 0755);

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
