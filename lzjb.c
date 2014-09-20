#include <stdlib.h>

#define	MATCH_BITS	6
#define	MATCH_MIN	3
#define	MATCH_MAX	((1 << MATCH_BITS) + (MATCH_MIN - 1))
#define	OFFSET_MASK	((1 << (16 - MATCH_BITS)) - 1)
#define	LEMPEL_SIZE	1024
#define NBBY				8

typedef unsigned char	uchar_t;

int
lzjb_decompress(void *s_start, void *d_start, size_t s_len, size_t d_len)
{
	uchar_t *src = s_start;
	uchar_t *dst = d_start;
	uchar_t *d_end = (uchar_t *)d_start + d_len;
	uchar_t *cpy, copymap = 0;
	int copymask = 1 << (NBBY - 1);

	while (dst < d_end) {
		if ((copymask <<= 1) == (1 << NBBY)) {
			copymask = 1;
			copymap = *src++;
		}
		if (copymap & copymask) {
			int mlen = (src[0] >> (NBBY - MATCH_BITS)) + MATCH_MIN;
			int offset = ((src[0] << NBBY) | src[1]) & OFFSET_MASK;
			src += 2;
			if ((cpy = dst - offset) < (uchar_t *)d_start)
				return (-1);
			while (--mlen >= 0 && dst < d_end)
				*dst++ = *cpy++;
		} else {
			*dst++ = *src++;
		}
	}
	return (0);
}


