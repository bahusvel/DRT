#include "entities.h"
#include <byteswap.h>
#include <stdint.h>
#include <string.h>

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define htobe64(val) bswap_64(val)
#define be64toh(val) bswap_64(val)
#define htobe32(val) bswap_32(val)
#define be32toh(val) bswap_32(val)
#define htobe16(val) bswap_16(val)
#define be16toh(val) bswap_16(val)
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#define htobe64(val) val
#define be64toh(val) val
#define htobe32(val) val
#define be32toh(val) val
#define htobe16(val) val
#define be16toh(val) val
#endif

static int size_drt_tags(struct drt_tags *tags) {
	int a = sizeof(drt_tag_len) * (tags->count + 1);
	for (int i = 0; i < tags->count; i++) {
		a += tags->tags[i].len;
	}
	return a;
}

static int enc_drt_tags(struct drt_tags *tags, unsigned char *buf) {
	unsigned char *orig = buf;
	*(drt_tag_len *)buf = htobe16(tags->count);
	buf += sizeof(drt_tag_len);
	for (int i = 0; i < tags->count; i++) {
		*(drt_tag_len *)buf = htobe16(tags->tags[i].len);
		buf += sizeof(drt_tag_len);
		memcpy(buf, tags->tags[i].val, tags->tags[i].len);
		buf += tags->tags[i].len;
	}
	return buf - orig;
}

int size_drt_medium(DRTMedium *medium) {
	return SIZE_ENTITY_TYPE + sizeof(drt_medium_id) +
		   size_drt_tags(&medium->tags);
}

void enc_drt_medium(DRTMedium *medium, unsigned char *buf) {
	*buf++ = (uint8_t)ENT_MEDIUM;
	*(drt_id *)buf = htobe64(medium->id);
	buf += sizeof(drt_id);
	enc_drt_tags(&medium->tags, buf);
}

int size_drt_data(DRTData *data) {
	return SIZE_ENTITY_TYPE + sizeof(drt_checksum) + sizeof(drt_data_id) +
		   sizeof(drt_blob_id) + sizeof(drt_off) + size_drt_tags(&data->tags);
}

void enc_drt_data(DRTData *data, unsigned char *buf) {
	*buf++ = (uint8_t)ENT_DATA;
	*(drt_id *)buf = htobe64(data->id);
	buf += sizeof(drt_id);
	*(drt_id *)buf = htobe64(data->iblobid);
	buf += sizeof(drt_id);
	*(drt_off *)buf = htobe64(data->size);
	buf += sizeof(drt_off);
	*(drt_checksum *)buf = htobe32(data->checksum);
	buf += sizeof(drt_checksum);
	enc_drt_tags(&data->tags, buf);
}

int size_drt_blob(DRTBlob *blob) {
	return sizeof(drt_blob_id) + sizeof(drt_medium_id) + sizeof(drt_off) +
		   sizeof(drt_off);
}

void enc_drt_blob(DRTBlob *blob, unsigned char *buf) {
	*(drt_id *)buf = htobe64(blob->id);
	buf += sizeof(drt_id);
	*(drt_id *)buf = htobe64(blob->medium);
	buf += sizeof(drt_id);
	*(drt_off *)buf = htobe64(blob->offset);
	buf += sizeof(drt_off);
	*(drt_off *)buf = htobe64(blob->length);
	buf += sizeof(drt_off);
}

int size_drt_func(DRTFunc *func) {
	return SIZE_ENTITY_TYPE + sizeof(drt_id) + sizeof(drt_inline_len) +
		   func->len;
}

void enc_drt_func(DRTFunc *func, unsigned char *buf) {
	*buf++ = (uint8_t)ENT_FUNC;
	*(drt_id *)buf = htobe64(func->id);
	buf += sizeof(drt_id);
	*(drt_inline_len *)buf = htobe32(func->len);
	buf += sizeof(drt_inline_len);
	memcpy(buf, func->code, func->len);
}

static int size_drt_tran(struct drt_tran *tran) {
	int a = sizeof(drt_inline_len) + sizeof(drt_inline_len) +
			sizeof(drt_func_id) + (tran->arg_count * SIZE_ARG_TYPE) +
			(tran->out_count * sizeof(drt_blob_id));
	for (int i = 0; i < tran->arg_count; i++) {
		if (tran->args[i].type == BLOB)
			a += sizeof(drt_id);
		else
			a += sizeof(drt_inline_len) + tran->args[i].direct.len;
	}
	return a;
}

static int enc_drt_tran(struct drt_tran *tran, unsigned char *buf) {
	unsigned char *orig = buf;
	*(drt_inline_len *)buf = htobe32(tran->arg_count);
	buf += sizeof(drt_inline_len);
	for (int i = 0; i < tran->arg_count; i++) {
		*buf++ = (uint8_t)tran->args[i].type;
		if (tran->args[i].type == BLOB) {
			*(drt_id *)buf = htobe64(tran->args[i].blob);
			buf += sizeof(drt_id);
		} else {
			*(drt_inline_len *)buf = htobe32(tran->args[i].direct.len);
			buf += sizeof(drt_inline_len);
			memcpy(buf, tran->args[i].direct.val, tran->args[i].direct.len);
			buf += tran->args[i].direct.len;
		}
	}
	*(drt_id *)buf = htobe64(tran->func);
	buf += sizeof(drt_id);
	*(drt_inline_len *)buf = htobe32(tran->out_count);
	buf += sizeof(drt_inline_len);
	for (int i = 0; i < tran->out_count; i++) {
		*(drt_id *)buf = htobe64(tran->out_blobs[i]);
		buf += sizeof(drt_id);
	}
	return buf - orig;
}

int size_drt_transform(DRTTransform *trans) {
	int a = SIZE_ENTITY_TYPE + SIZE_TRANS_TYPE + sizeof(drt_inline_len);
	if (trans->reverse != NULL)
		a += size_drt_tran(trans->reverse);
	for (int i = 0; i < trans->dec_len; i++)
		a += size_drt_blob(&trans->declares[i]);
	return a;
}

void enc_drt_transform(DRTTransform *trans, unsigned char *buf) {
	*buf++ = (uint8_t)ENT_TRANSFORM;
	*buf++ = (uint8_t)trans->type;
	*(drt_inline_len *)buf = htobe32(trans->dec_len);
	buf += sizeof(drt_inline_len);
	for (int i = 0; i < trans->dec_len; i++) {
		enc_drt_blob(&trans->declares[i], buf);
		buf += size_drt_blob(&trans->declares[i]);
	}
	if (trans->reverse != NULL)
		enc_drt_tran(trans->reverse, buf);
}
