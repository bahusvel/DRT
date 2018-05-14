#ifndef LIBDRT_ENTITIES
#define LIBDRT_ENTITIES

typedef unsigned long drt_id;
typedef drt_id drt_blob_id;
typedef drt_id drt_func_id;
typedef drt_id drt_data_id;
typedef drt_id drt_medium_id;

typedef long drt_off;
typedef int drt_inline_len;
typedef unsigned long drt_checksum;

struct drt_tag {
	drt_inline_len len;
	char *val;
};

struct drt_tags {
	drt_inline_len count;
	struct drt_tag *tags;
};

#define NEW_DRT_TAGS(count)                                                    \
	(struct drt_tags) { count, alloca(count * sizeof(struct drt_tag)) }

#define SET_DRT_TAG_STRING(i, str, _tags)                                      \
	_tags.tags[i] = (struct drt_tag) { strlen(str), (char *)str }

typedef struct drt_medium {
	drt_id id;
	struct drt_tags tags;
} DRTMedium;

int size_drt_medium(DRTMedium *medium);
void enc_drt_medium(DRTMedium *medium, unsigned char *buf);

typedef struct drt_data {
	drt_data_id id;
	struct drt_tags tags;
	drt_checksum checksum;
} DRTData;

int size_drt_data(DRTData *data);
void enc_drt_data(DRTData *data, unsigned char *buf);

typedef struct drt_blob {
	drt_blob_id id;
	drt_data_id data;
	drt_medium_id medium;
	drt_off offset;
	drt_off length;
} DRTBlob;

int size_drt_blob();
void enc_drt_blob(DRTBlob *blob, unsigned char *buf);

typedef struct drt_func {
	drt_func_id id;
	drt_inline_len len;
	char *code;
} DRTFunc;

int size_drt_func(DRTFunc *func);
void enc_drt_func(DRTFunc *func, unsigned char *buf);

struct inline_arg {
	drt_inline_len len;
	char *val;
};

enum drt_arg_type { BLOB, INLINE };

struct drt_arg {
	enum drt_arg_type type;
	union {
		drt_blob_id blob;
		struct inline_arg direct;
	};
};

#define NEW_DRT_ARGS(count)                                                    \
	((struct drt_arg *)alloca(sizeof(struct drt_arg) * count))

struct drt_tran {
	drt_inline_len arg_count;
	struct drt_arg *args;
	drt_func_id func;
	drt_inline_len out_count;
	drt_blob_id *out_blobs;
};

enum drt_trans_type { DISCARD, LOSSY, LOSSLESS };

typedef struct drt_transform {
	enum drt_trans_type type;
	union {
		struct drt_tran forward;
		struct drt_tran reverse;
	};
} DRTTransform;

int size_drt_transform(DRTTransform *trans);
void enc_drt_transform(DRTTransform *trans, unsigned char *buf);

#endif
