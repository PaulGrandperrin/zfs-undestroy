#include <stdlib.h>
#include <inttypes.h>

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

#ifndef roundup
#define roundup(x, y)		((((x) + ((y) - 1)) / (y)) * (y))
#endif

#define offsetof(s, m)  ((size_t)(&(((s *)0)->m)))

typedef struct raidz_col {
	uint64_t rc_devidx;		/* child device index for I/O */
	uint64_t rc_offset;		/* device offset */
	uint64_t rc_size;		/* I/O size */
	void *rc_data;			/* I/O data */
	void *rc_gdata;			/* used to store the "good" version */
	int rc_error;			/* I/O error for this device */
	uint8_t rc_tried;		/* Did we attempt this I/O column? */
	uint8_t rc_skipped;		/* Did we skip this I/O column? */
} raidz_col_t;

typedef struct raidz_map {
	uint64_t rm_cols;		/* Regular column count */
	uint64_t rm_scols;		/* Count including skipped columns */
	uint64_t rm_bigcols;		/* Number of oversized columns */
	uint64_t rm_asize;		/* Actual total I/O size */
	uint64_t rm_missingdata;	/* Count of missing data devices */
	uint64_t rm_missingparity;	/* Count of missing parity devices */
	uint64_t rm_firstdatacol;	/* First data column/parity count */
	uint64_t rm_nskip;		/* Skipped sectors for padding */
	uint64_t rm_skipstart;	/* Column index of padding start */
	void *rm_datacopy;		/* rm_asize-buffer of copied data */
	uintptr_t rm_reports;		/* # of referencing checksum reports */
	uint8_t	rm_freed;		/* map no longer has referencing ZIO */
	uint8_t	rm_ecksuminjected;	/* checksum error was injected */
	raidz_col_t rm_col[1];		/* Flexible array of I/O columns */
} raidz_map_t;


static raidz_map_t *
raidz_map(uint64_t voffset, uint64_t vsize, uint64_t unit_shift, uint64_t dcols, uint64_t nparity)
{
	raidz_map_t *rm;
	uint64_t b = voffset >> unit_shift;
	uint64_t s = vsize >> unit_shift;
	uint64_t f = b % dcols;
	uint64_t o = (b / dcols) << unit_shift;
	uint64_t q, r, c, bc, col, acols, scols, coff, devidx, asize, tot;

	q = s / (dcols - nparity);
	r = s - q * (dcols - nparity);
	bc = (r == 0 ? 0 : r + nparity);
	tot = s + nparity * (q + (r == 0 ? 0 : 1));

	if (q == 0) {
		acols = bc;
		scols = MIN(dcols, roundup(bc, nparity + 1));
	} else {
		acols = dcols;
		scols = dcols;
	}

	if (!(acols <= scols))
		printf("WTF !(acols, <=, scols)");

	rm = malloc(offsetof(raidz_map_t, rm_col[scols]));

	rm->rm_cols = acols;
	rm->rm_scols = scols;
	rm->rm_bigcols = bc;
	rm->rm_skipstart = bc;
	rm->rm_missingdata = 0;
	rm->rm_missingparity = 0;
	rm->rm_firstdatacol = nparity;
	rm->rm_datacopy = NULL;
	rm->rm_reports = 0;
	rm->rm_freed = 0;
	rm->rm_ecksuminjected = 0;

	asize = 0;

	for (c = 0; c < scols; c++) {
		col = f + c;
		coff = o;
		if (col >= dcols) {
			col -= dcols;
			coff += 1ULL << unit_shift;
		}
		rm->rm_col[c].rc_devidx = col;
		rm->rm_col[c].rc_offset = coff;
		rm->rm_col[c].rc_data = NULL;
		rm->rm_col[c].rc_gdata = NULL;
		rm->rm_col[c].rc_error = 0;
		rm->rm_col[c].rc_tried = 0;
		rm->rm_col[c].rc_skipped = 0;

		if (c >= acols)
			rm->rm_col[c].rc_size = 0;
		else if (c < bc)
			rm->rm_col[c].rc_size = (q + 1) << unit_shift;
		else
			rm->rm_col[c].rc_size = q << unit_shift;

		asize += rm->rm_col[c].rc_size;
	}

	if (!(asize == (tot << unit_shift)))
		printf("WTF !(asize, ==, tot << unit_shift)");

	rm->rm_asize = roundup(asize, (nparity + 1) << unit_shift);
	rm->rm_nskip = roundup(tot, nparity + 1) - tot;
	if (!((rm->rm_asize - asize) ==( rm->rm_nskip << unit_shift)))
		printf("WTF !(rm->rm_asize - asize, ==, rm->rm_nskip << unit_shift)");

	if (!(rm->rm_nskip <= nparity))
		printf("WTF !(rm->rm_nskip, <=, nparity)");


	/*
	 * If all data stored spans all columns, there's a danger that parity
	 * will always be on the same device and, since parity isn't read
	 * during normal operation, that that device's I/O bandwidth won't be
	 * used effectively. We therefore switch the parity every 1MB.
	 *
	 * ... at least that was, ostensibly, the theory. As a practical
	 * matter unless we juggle the parity between all devices evenly, we
	 * won't see any benefit. Further, occasional writes that aren't a
	 * multiple of the LCM of the number of children and the minimum
	 * stripe width are sufficient to avoid pessimal behavior.
	 * Unfortunately, this decision created an implicit on-disk format
	 * requirement that we need to support for all eternity, but only
	 * for single-parity RAID-Z.
	 *
	 * If we intend to skip a sector in the zeroth column for padding
	 * we must make sure to note this swap. We will never intend to
	 * skip the first column since at least one data and one parity
	 * column must appear in each row.
	 */
	if (!(rm->rm_cols >= 2))
		printf("WTF !(rm->rm_cols >= 2)");

	if (!(rm->rm_col[0].rc_size == rm->rm_col[1].rc_size))
		printf("WTF !(rm->rm_col[0].rc_size == rm->rm_col[1].rc_size)");

	if (rm->rm_firstdatacol == 1 && (voffset & (1ULL << 20))) {

		printf("WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW\n");
		devidx = rm->rm_col[0].rc_devidx;
		o = rm->rm_col[0].rc_offset;
		rm->rm_col[0].rc_devidx = rm->rm_col[1].rc_devidx;
		rm->rm_col[0].rc_offset = rm->rm_col[1].rc_offset;
		rm->rm_col[1].rc_devidx = devidx;
		rm->rm_col[1].rc_offset = o;

		if (rm->rm_skipstart == 0)
			rm->rm_skipstart = 1;
	}

	return (rm);
}


int main()
{

	raidz_map_t * rm;
	rm = raidz_map(3382445801472,8192,12, 4, 1);

	printf("col=%llu\n",rm->rm_cols);
	printf("scols=%llu\n",rm->rm_scols);
	printf("bigcols=%llu\n",rm->rm_bigcols);
	printf("skipstart=%llu\n",rm->rm_skipstart);
	printf("firstdatacol=%llu\n",rm->rm_firstdatacol);
	printf("asize=%llu\n",rm->rm_asize);
	printf("nskip=%llu\n",rm->rm_nskip);

	printf("0 devidx=%llu\n",rm->rm_col[0].rc_devidx);
	printf("0 offset=%llu\n",rm->rm_col[0].rc_offset);
	printf("0 csize=%llu\n",rm->rm_col[0].rc_size);

	printf("1 devidx=%llu\n",rm->rm_col[1].rc_devidx);
	printf("1 offset=%llu\n",rm->rm_col[1].rc_offset);
	printf("1 csize=%llu\n",rm->rm_col[1].rc_size);

	printf("2 devidx=%llu\n",rm->rm_col[2].rc_devidx);
	printf("2 offset=%llu\n",rm->rm_col[2].rc_offset);
	printf("2 csize=%llu\n",rm->rm_col[2].rc_size);

	printf("3 devidx=%llu\n",rm->rm_col[3].rc_devidx);
	printf("3 offset=%llu\n",rm->rm_col[3].rc_offset);
	printf("3 csize=%llu\n",rm->rm_col[3].rc_size);

	printf("ok\n");


	uint64_t a= 342016;
	if (a & (1ULL << 20))
		printf("vrai");
	else
		printf("faux");
	


	return 0;
}
