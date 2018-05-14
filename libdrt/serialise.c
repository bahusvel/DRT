#include "entities.h"

#define NULL (void *)0

static int size_drt_tags(struct drt_tags *tags) {
	int a = sizeof(drt_inline_len) * (tags->count + 1);
	for (int i = 0; i < tags->count; i++) {
		a += tags->tags[i].len;
	}
	return a;
}

static void enc_drt_tags(struct drt_tags *tags, unsigned char *buf) {}

int size_drt_medium(DRTMedium *medium) {
	return sizeof(drt_id) + size_drt_tags(&medium->tags);
}

void enc_drt_medium(DRTMedium *medium, unsigned char *buf) {}

int size_drt_data(DRTData *data) {
	return sizeof(drt_checksum) + sizeof(drt_id) + size_drt_tags(&data->tags);
}

void enc_drt_data(DRTData *data, unsigned char *buf) {}

int size_drt_blob() {
	return sizeof(drt_id) + sizeof(drt_id) + sizeof(drt_off) + sizeof(drt_off);
}

void enc_drt_blob(DRTBlob *blob, unsigned char *buf) {}

int size_drt_func(DRTFunc *func) {
	return sizeof(drt_id) + sizeof(drt_inline_len) + func->len;
}

void enc_drt_func(DRTFunc *func, unsigned char *buf) {}

static int size_drt_tran(struct drt_tran *tran) {
	int a = sizeof(drt_inline_len) + sizeof(drt_inline_len) + sizeof(drt_id) +
			tran->out_count * sizeof(drt_id);
	for (int i = 0; i < tran->arg_count; i++) {
		if (tran->args[i].type == BLOB)
			a += sizeof(drt_id);
		else
			a += sizeof(drt_inline_len) + tran->args[i].direct.len;
	}
	return a;
}

int size_drt_transform(DRTTransform *trans) {
	int a = sizeof(enum drt_trans_type) + size_drt_tran(&trans->forward);
	if (trans->reverse != NULL)
		a += size_drt_tran(trans->reverse);
	return a;
}

void enc_drt_transform(DRTTransform *trans, unsigned char *buf) {}
