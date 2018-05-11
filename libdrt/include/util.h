#ifndef LIBDRT_UTIL
#define LIBDRT_UTIL

#include <stdint.h>

uint32_t crc32c(uint32_t crc, const uint8_t *data, unsigned int length);

#endif
