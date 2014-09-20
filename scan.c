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

int main(int argc, char* argv[])
{
	int fd;
	char s_buf[4096];
	char d_buf[2048];
	int asize;
	int psize;
	objset_phys_t *obj;	
	unsigned long long offset = 0;
	unsigned long nbvalid=0;


	fd = open(argv[1],O_RDONLY);
	if(fd == -1)
		perror("open");

	//boucle	
	while((asize = read(fd, s_buf, 4096)) == 4096)
	{
		if(offset % (555550*1024) == 0)
			fprintf(stderr, "%llu - %lu\n",offset, nbvalid);

		offset++;
		psize = lzjb_decompress(s_buf, d_buf, asize, 2048);

		obj = (objset_phys_t*) d_buf;
	
		if (obj->os_meta_dnode.dn_type != 0xa)
			continue;

		if (obj->os_meta_dnode.dn_indblkshift < 12)
			continue;

		if (obj->os_meta_dnode.dn_nlevels == 0)
			continue;
		
		if (obj->os_meta_dnode.dn_pad3[0] != 0 || obj->os_meta_dnode.dn_pad3[1] != 0 || obj->os_meta_dnode.dn_pad3[2] != 0 || obj->os_meta_dnode.dn_pad3[3] != 0)
			continue;

		if (obj->os_meta_dnode.dn_maxblkid < 2)
			continue;

		if (obj->os_meta_dnode.dn_blkptr[0].blk_pad[0] != 0 || obj->os_meta_dnode.dn_blkptr[0].blk_pad[1] != 0)
			continue;
		
		if (obj->os_meta_dnode.dn_blkptr[0].blk_birth == 0)
			continue;
		
		// check that the bytes after the compressed payload are zero
		int i;
		for(i=psize+1; i<4096; ++i)
		{
			if(s_buf[i] != 0)
				break;
		}
		if(i!=4096)
			continue;

		nbvalid++;
		char name[255];
		sprintf(name,"%llu",offset-1);
		int fdd = open(name, O_CREAT|O_WRONLY);
		if(fdd == -1)
			perror("open2");

		write(fdd, d_buf, 2048);
		close(fdd);
	}
	
	fprintf(stderr, "END - %d\n",nbvalid);



	return 0;
}
