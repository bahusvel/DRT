#ifndef LIBDRT_ENTITIES
#define LIBDRT_ENTITIES

typedef unsigned long drt_id;
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

typedef struct drt_medium {
	drt_id id;
	struct drt_tags tags;
} DRTMedium;

typedef struct drt_data {
	drt_id id;
	struct drt_tags tags;
	drt_checksum checksum;
} DRTData;

typedef struct drt_blob {
	struct drt_data *data;
	struct drt_medium *medium;
	drt_off offset;
	drt_off length;
} DRTBlob;

typedef struct drt_func {
	drt_id id;
	char *code;
	drt_inline_len len;
} DRTFunc;

struct inline_arg {
	drt_inline_len len;
	char *val;
};

enum drt_arg_type { BLOB, INLINE };

struct drt_arg {
	enum drt_arg_type type;
	union {
		struct drt_blob *blob;
		struct inline_arg direct;
	};
};

struct drt_tran {
	drt_inline_len arg_count;
	struct drt_arg *args;
	struct drt_func *func;
	drt_inline_len out_count;
	struct drt_blob *out_blobs;
};

enum drt_trans_type { DISCARD, LOSSY, LOSSLESS };

typedef struct drt_transform {
	drt_id id;
	enum drt_trans_type type;
	struct drt_tran forward;
	struct drt_tran *reverse;
} DRTTransform;

#endif
