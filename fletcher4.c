#include <stdlib.h>
#include <inttypes.h>

void fletcher4(const void *buf, uint64_t size, uint64_t ck[4])
{
	const uint32_t *ip = buf;
	const uint32_t *ipend = ip + (size / sizeof (uint32_t));
	uint64_t a, b, c, d;

	for (a = b = c = d = 0; ip < ipend; ip++) {
		a += ip[0];
		b += a;
		c += b;
		d += c;
	}

	ck[0]=a;
	ck[1]=b;
	ck[2]=c;
	ck[3]=d;
}

