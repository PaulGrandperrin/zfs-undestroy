#include <stdlib.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>

// struct

typedef struct dva {
	uint64_t	dva_word[2];
} dva_t;

/*
 * Each block has a 256-bit checksum -- strong enough for cryptographic hashes.
 */
typedef struct zio_cksum {
	uint64_t	zc_word[4];
} zio_cksum_t;

#define	SPA_BLKPTRSHIFT	7		/* blkptr_t is 128 bytes	*/
#define	SPA_DVAS_PER_BP	3		/* Number of DVAs in a bp	*/

typedef struct blkptr {
	dva_t		blk_dva[SPA_DVAS_PER_BP]; /* Data Virtual Addresses */
	uint64_t	blk_prop;	/* size, compression, type, etc	    */
	uint64_t	blk_pad[2];	/* Extra space for the future	    */
	uint64_t	blk_phys_birth;	/* txg when block was allocated	    */
	uint64_t	blk_birth;	/* transaction group at birth	    */
	uint64_t	blk_fill;	/* fill count			    */
	zio_cksum_t	blk_cksum;	/* 256-bit checksum		    */
} blkptr_t;

typedef struct zil_header {
	uint64_t zh_claim_txg;	/* txg in which log blocks were claimed */
	uint64_t zh_replay_seq;	/* highest replayed sequence number */
	blkptr_t zh_log;	/* log chain */
	uint64_t zh_claim_blk_seq; /* highest claimed block sequence number */
	uint64_t zh_flags;	/* header flags */
	uint64_t zh_claim_lr_seq; /* highest claimed lr sequence number */
	uint64_t zh_pad[3];
} zil_header_t;

#define	DNODE_SHIFT		9
#define	DNODE_CORE_SIZE		64
#define SPA_BLKPTRSHIFT 7
#define	DNODE_SIZE	(1 << DNODE_SHIFT)
#define	DN_MAX_BONUSLEN	(DNODE_SIZE - DNODE_CORE_SIZE - (1 << SPA_BLKPTRSHIFT))

typedef struct dnode_phys {
	uint8_t dn_type;		/* dmu_object_type_t */
	uint8_t dn_indblkshift;		/* ln2(indirect block size) */
	uint8_t dn_nlevels;		/* 1=dn_blkptr->data blocks */
	uint8_t dn_nblkptr;		/* length of dn_blkptr */
	uint8_t dn_bonustype;		/* type of data in bonus buffer */
	uint8_t	dn_checksum;		/* ZIO_CHECKSUM type */
	uint8_t	dn_compress;		/* ZIO_COMPRESS type */
	uint8_t dn_flags;		/* DNODE_FLAG_* */
	uint16_t dn_datablkszsec;	/* data block size in 512b sectors */
	uint16_t dn_bonuslen;		/* length of dn_bonus */
	uint8_t dn_pad2[4];

	/* accounting is protected by dn_dirty_mtx */
	uint64_t dn_maxblkid;		/* largest allocated block ID */
	uint64_t dn_used;		/* bytes (or sectors) of disk space */

	uint64_t dn_pad3[4];

	blkptr_t dn_blkptr[1];
	uint8_t dn_bonus[DN_MAX_BONUSLEN - sizeof (blkptr_t)];
	blkptr_t dn_spill;
} dnode_phys_t;







#define	OBJSET_PHYS_SIZE 2048

typedef struct objset_phys {
	dnode_phys_t os_meta_dnode;
	zil_header_t os_zil_header;
	uint64_t os_type;
	uint64_t os_flags;
	char os_pad[OBJSET_PHYS_SIZE - sizeof (dnode_phys_t)*3 -
	    sizeof (zil_header_t) - sizeof (uint64_t)*2];
	dnode_phys_t os_userused_dnode;
	dnode_phys_t os_groupused_dnode;
} objset_phys_t;








typedef struct dsl_dataset_phys {
	uint64_t ds_dir_obj;		/* DMU_OT_DSL_DIR */
	uint64_t ds_prev_snap_obj;	/* DMU_OT_DSL_DATASET */
	uint64_t ds_prev_snap_txg;
	uint64_t ds_next_snap_obj;	/* DMU_OT_DSL_DATASET */
	uint64_t ds_snapnames_zapobj;	/* DMU_OT_DSL_DS_SNAP_MAP 0 for snaps */
	uint64_t ds_num_children;	/* clone/snap children; ==0 for head */
	uint64_t ds_creation_time;	/* seconds since 1970 */
	uint64_t ds_creation_txg;
	uint64_t ds_deadlist_obj;	/* DMU_OT_DEADLIST */
	/*
	 * ds_referenced_bytes, ds_compressed_bytes, and ds_uncompressed_bytes
	 * include all blocks referenced by this dataset, including those
	 * shared with any other datasets.
	 */
	uint64_t ds_referenced_bytes;
	uint64_t ds_compressed_bytes;
	uint64_t ds_uncompressed_bytes;
	uint64_t ds_unique_bytes;	/* only relevant to snapshots */
	/*
	 * The ds_fsid_guid is a 56-bit ID that can change to avoid
	 * collisions.  The ds_guid is a 64-bit ID that will never
	 * change, so there is a small probability that it will collide.
	 */
	uint64_t ds_fsid_guid;
	uint64_t ds_guid;
	uint64_t ds_flags;		/* DS_FLAG_* */
	blkptr_t ds_bp;
	uint64_t ds_next_clones_obj;	/* DMU_OT_DSL_CLONES */
	uint64_t ds_props_obj;		/* DMU_OT_DSL_PROPS for snaps */
	uint64_t ds_userrefs_obj;	/* DMU_OT_USERREFS */
	uint64_t ds_pad[5]; /* pad out to 320 bytes for good measure */
} dsl_dataset_phys_t;







// raidz_map


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

	rm = malloc(offsetof(raidz_map_t, rm_col[scols])); // FIXME Stop leaking

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










// lzjb

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
	return (((void*)src)-s_start);
}



// ----------------------------------------------------------------------------

int check(char* buf, int nb, uint64_t offset, char* str, uint64_t asize)
{	
	int matches=0;
	int i;
	for(i = 0; i < nb; ++i)
	{
		dnode_phys_t* dnode=(dnode_phys_t*) &(buf[512*i]);
		
		if(
			dnode->dn_type == 16 &&
			dnode->dn_indblkshift == 14 &&
			dnode->dn_nlevels == 1 &&
			dnode->dn_nblkptr == 1 &&
			dnode->dn_bonustype == 16 &&
			dnode->dn_checksum == 0 &&
			dnode->dn_compress == 0 &&
			//dnode->dn_flags == 0 &&
			dnode->dn_datablkszsec == 1 &&
			dnode->dn_bonuslen == 320 &&
			dnode->dn_maxblkid == 0 &&
			//dnode->dn_used == 0 &&
			dnode->dn_pad2[0] == 0 && dnode->dn_pad2[1] == 0 && dnode->dn_pad2[2] == 0 && dnode->dn_pad2[3] == 0 && 
			dnode->dn_pad3[0] == 0 && dnode->dn_pad3[1] == 0 && dnode->dn_pad3[2] == 0 && dnode->dn_pad3[3] == 0
			)
		{
			dsl_dataset_phys_t* dset = (dsl_dataset_phys_t*) dnode->dn_bonus;
			if(
				//dset->ds_prev_snap_obj == 18 &&
				//dset->ds_prev_snap_txg == 1 &&
				dset->ds_next_snap_obj == 0 &&
				dset->ds_num_children == 0 &&
				//dset->ds_creation_time >= 9876543210 &&
				//dset->ds_creation_txg != 1 &&
				//dset->ds_creation_txg != 3320 &&
				//dset->ds_creation_txg != 46668 &&
				//dset->ds_creation_txg != 55107 &&
				//dset->ds_referenced_bytes > 0 &&
				dset->ds_pad[0] == 0 && dset->ds_pad[1] == 0 && dset->ds_pad[2] == 0 && dset->ds_pad[3] == 0 && dset->ds_pad[4] == 0
				)
			{
				// We have a match, Yeah
				char name[1024];
				int fd;
				snprintf(name, 1024, "off=%llu-%s=%u-ctxg=%llu-refb=%llu-ind=%d.dump",offset, str, asize, dset->ds_creation_txg, dset->ds_referenced_bytes, i);
				fd = open(name, O_CREAT|O_WRONLY);
				
				if(fd == -1)
					perror("open dump");

				write(fd, &(buf[512*i]), 512);
				close(fd);
				matches++;


			}
		}
	}
	return matches;
}


int main(int argc, char* argv[])
{


	int fd0, fd1, fd2, fd3;
	int asize;
	int psize;
	objset_phys_t *obj;	
	unsigned long long offset;
	unsigned long nbvalid;


	if(argc < 5)
		fprintf(stderr,"Usage:\n%s: vdev0 vdev1 vdev2 vdev3\n", argv[0]);

	fd0 = open(argv[1],O_RDONLY);
	if(fd0 == -1)
		perror("open fd0");

	fd1 = open(argv[2],O_RDONLY);
	if(fd1 == -1)
		perror("open fd1");

	fd2 = open(argv[3],O_RDONLY);
	if(fd2 == -1)
		perror("open fd2");

	fd3 = open(argv[4],O_RDONLY);
	if(fd3 == -1)
		perror("open fd3");

	offset = 0;

	// Go to the 4th MiB
	lseek(fd0, (1024+offset)*4096, SEEK_SET);
	lseek(fd1, (1024+offset)*4096, SEEK_SET);
	lseek(fd2, (1024+offset)*4096, SEEK_SET);
	lseek(fd3, (1024+offset)*4096, SEEK_SET);

	posix_fadvise(fd0, 0, 0, POSIX_FADV_SEQUENTIAL);
	posix_fadvise(fd1, 0, 0, POSIX_FADV_SEQUENTIAL);
	posix_fadvise(fd2, 0, 0, POSIX_FADV_SEQUENTIAL);
	posix_fadvise(fd3, 0, 0, POSIX_FADV_SEQUENTIAL);

	int start_dontneed = 0;

	for(nbvalid = 0; offset >=0; ++offset)
	{
		int ret=0;
		char block_vdev[4][4096];
		ret += read(fd0, block_vdev[0], 4096);
		ret += read(fd1, block_vdev[1], 4096);
		ret += read(fd2, block_vdev[2], 4096);
		ret += read(fd3, block_vdev[3], 4096);

		if(ret != 4096*4)
			break;


		if(offset % (50*1024) == 0 && start_dontneed) // every 200MiB
		{
			fprintf(stderr, "%llu - %lu\n",offset, nbvalid);
			posix_fadvise(fd0, offset*4096-200*1024*1024, 200*1024*1024, POSIX_FADV_DONTNEED);
			posix_fadvise(fd1, offset*4096-200*1024*1024, 200*1024*1024, POSIX_FADV_DONTNEED);
			posix_fadvise(fd2, offset*4096-200*1024*1024, 200*1024*1024, POSIX_FADV_DONTNEED);
			posix_fadvise(fd3, offset*4096-200*1024*1024, 200*1024*1024, POSIX_FADV_DONTNEED);
			
		}
		start_dontneed = 1;


		// Check directly the blocks for uncompressed valid data
		nbvalid += check(block_vdev[0], 8, offset, "raw",0);
		nbvalid += check(block_vdev[1], 8, offset, "raw",1);
		nbvalid += check(block_vdev[2], 8, offset, "raw",2);
		nbvalid += check(block_vdev[3], 8, offset, "raw",3);


		// Try reading at this offset (in the raidz space) a block of 4K or 8K or 12K. (in theory we should also try 16K but thanks to the lzjb compression it should very rarely happen)

		int lsize = 16384; // 2^14
		int ashift = 12; // 4K sector size
		int ncol = 4; // 4 vdevs
		int nparity = 1; // raidz-1 == 1 parity
		char block_raw[16384];
		char block_decompressed[16384];
		int rsize;


		// Loop
		int s;
		for(s=1; s<=3; ++s)
		{
			psize = 4096*s;

			raidz_map_t * rm;
			rm = raidz_map(offset*4096*4, psize, ashift, ncol, nparity);

			raidz_col_t * rmcols = rm->rm_col;
			int i, l;
			for(i = rm->rm_firstdatacol, l=0; i < rm->rm_cols; ++i, ++l)
			{
				uint64_t devidx = rmcols[i].rc_devidx;
				memcpy(&(block_raw[4096*l]),block_vdev[devidx], 4096);
			}
			free(rm);
			memset(block_decompressed, 0,16384 );
			lzjb_decompress(block_raw, block_decompressed, 0, lsize);
			memset(block_raw, 0,16384 );
			nbvalid += check(block_decompressed, 32, offset, "lzc",psize/4096);
			

		}

		// Second half
		psize = 4096;

		raidz_map_t * rm;
		rm = raidz_map(offset*4*4096+2*4096, psize, ashift, ncol, nparity);

		raidz_col_t * rmcols = rm->rm_col;
		int i;
		for(i = rm->rm_firstdatacol; i < rm->rm_cols; ++i)
		{
			uint64_t devidx = rmcols[i].rc_devidx;
			memcpy(&(block_raw[4096*i]),block_vdev[devidx], 4096);
		}
		free(rm);
		memset(block_decompressed, 0,16384 );
		rsize = lzjb_decompress(block_raw, block_decompressed, 0, lsize);
		memset(block_raw, 0,16384 );
		nbvalid += check(block_decompressed, 32, offset, "lzc",psize/4096);


	}
	
	fprintf(stderr, "END - %lu\n", nbvalid);



	return 0;
}
