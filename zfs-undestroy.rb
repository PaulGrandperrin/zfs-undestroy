#!/usr/bin/env ruby
#require 'debugger'

require 'nice-ffi'
require 'term/ansicolor'
require 'awesome_print'

#include Term::ANSIColor
class String
  include Term::ANSIColor
end

# ------------------------- Configuration ---------------------------------

DEBUG = true

$dump = false
# zdb -C -e tank
$pool = {
	:vdevs => [
			   {:path => "/dev/vgbackupdisk1/disk1"},
			   {:path => "/dev/vgbackup/disk2"},
			   {:path => "/dev/vgbackup/disk3"},
			   {:path => "/dev/vgbackup/disk4"},
			   ],
	:dcols => 4, #number of disk
	:nparity => 1, #number of parities
	:ashift => 12 # block size in power of two
}

# ------------------------- Utilities -------------------------------------

class Fixnum
	def to_human
		return "0" if self==0
	    units = %w{B KiB MiB GiB TiB}
	    e = (Math.log(self)/Math.log(1024)).floor
	    s = "%.3f" % (to_f / 1024**e)
	    s.sub(/\.?0*$/, units[e])
  	end
end

class Integer
	def to_human
		return "0" if self==0
	    units = %w{B KiB MiB GiB TiB}
	    e = (Math.log(self)/Math.log(1024)).floor
	    s = "%.3f" % (to_f / 1024**e)
	    s.sub(/\.?0*$/, units[e])
  	end
end

def assert(msg=nil)
    if DEBUG
        raise msg || "Assertion failed!" unless yield
    end
end

def roundup x, y
	return ((((x) + ((y) - 1)) / (y)) * (y))
end


def print_title title
	l=(80-2-2-title.size)/2
	puts "#{"="*l}> #{title.capitalize} <#{"="*l}".bold
end

def ask question
	puts question.capitalize.red
	r=STDIN.gets
	return r
end

def aps o, raw = false
	ap o.summary, options = {indent: 8, raw: raw}
end

def summary o
	if o.methods.include? :summary
		o.summary
	else
		o
	end
end

class Hash
	def summary
		self.map{|k,v| [k, (v.methods.include? :summary) ? v.summary : v]}.to_h
	end
end

class Array
	def summary
		self.map{|o| (o.methods.include? :summary) ? o.summary : o}
	end
end

class FFI::Struct::InlineArray
	def summary
		self.map{|o| (o.methods.include? :summary) ? o.summary : o}
	end
end
# ---------------------------- Natives libraries ---------------------------


module Lz4
	extend FFI::Library
	ffi_lib ['lz4','./liblz4.so.1.0.0']
	attach_function :LZ4_decompress_safe, [:pointer, :pointer, :int, :int], :int

	def self.decompress input, out_size
		inputRaw = FFI::MemoryPointer.new(:char, input.size)
		inputRaw.write_string input
		outputRaw = FFI::MemoryPointer.new(:char, out_size)
		len = LZ4_decompress_safe(inputRaw, outputRaw, input.size, out_size)
		if len < 0
			raise "LZ4 fail"
		end
		output = outputRaw.read_string len
		return output
	end
end

module Lzjb
	extend FFI::Library
	ffi_lib ['lzjb','./liblzjb.so']
	attach_function :lzjb_decompress, [:pointer, :pointer, :int, :int], :int

	def self.decompress input, out_size
		inputRaw = FFI::MemoryPointer.new(:char, input.size)
		inputRaw.write_string input
		outputRaw = FFI::MemoryPointer.new(:char, out_size)
		ret = lzjb_decompress(inputRaw, outputRaw, input.size, out_size)
		if ret != 0
			raise "LZJB fail"
		end
		output = outputRaw.read_string out_size
		return output
	end
end

module Fletcher4
	extend FFI::Library
	ffi_lib ['fletcher4','./libfletcher4.so']
	attach_function :fletcher4, [:pointer, :uint64, :pointer], :void

	def self.check input, size
		inputRaw = FFI::MemoryPointer.new(:char, input.size)
		inputRaw.write_string input
		outputRaw = FFI::MemoryPointer.new(:uint64, 4)
		fletcher4(inputRaw, input.size, outputRaw)
		ret = outputRaw.read_array_of_uint64 4
		return ret
	end
end

# -------------------------- ZFS Structures --------------------------------

TYPESTRINGS = [
				:NONE,
				# general:
				:OBJECT_DIRECTORY,
				:OBJECT_ARRAY,
				:PACKED_NVLIST,
				:PACKED_NVLIST_SIZE,
				:BPOBJ,
				:BPOBJ_HDR,
				# spa:
				:SPACE_MAP_HEADER,
				:SPACE_MAP,
				# zil:
				:INTENT_LOG,
				# dmu:
				:DNODE,
				:OBJSET,
				# dsl:
				:DSL_DIR,
				:DSL_DIR_CHILD_MAP,
				:DSL_DS_SNAP_MAP,
				:DSL_PROPS,
				:DSL_DATASET,
				# zpl:
				:ZNODE,
				:OLDACL,
				:PLAIN_FILE_CONTENTS,
				:DIRECTORY_CONTENTS,
				:MASTER_NODE,
				:UNLINKED_SET,
				# zvol:
				:ZVOL,
				:ZVOL_PROP,
				# other; for testing only!
				:PLAIN_OTHER,
				:UINT64_OTHER,
				:ZAP_OTHER,
				# new object types:
				:ERROR_LOG,
				:SPA_HISTORY,
				:SPA_HISTORY_OFFSETS,
				:POOL_PROPS,
				:DSL_PERMS,
				:ACL,
				:SYSACL,
				:FUID,
				:FUID_SIZE,
				:NEXT_CLONES,
				:SCAN_QUEUE,
				:USERGROUP_USED,
				:USERGROUP_QUOTA,
				:USERREFS,
				:DDT_ZAP,
				:DDT_STATS,
				:SA,
				:SA_MASTER_NODE,
				:SA_ATTR_REGISTRATION,
				:SA_ATTR_LAYOUTS,
				:SCAN_XLATE,
				:DEDUP,
				:DEADLIST,
				:DEADLIST_HDR,
				:DSL_CLONES,
				:BPOBJ_SUBOBJ
			]

class Zio_cksum_t < NiceFFI::Struct
	layout :zc_word,		[:uint64, 4]

	def summary
		zc_word.map{|w| w.to_s(16)}*' '
	end
end

class Dva_t < NiceFFI::Struct
	layout	:dva_word,		[:uint64, 2]
	
	def get_vdev
		return (self[:dva_word][0] & 0xFFFFFFFF00000000)>>32
	end

	def get_grid
		return (self[:dva_word][0] & 0x00000000FF000000)>>24
	end

	def get_asize
		return (self[:dva_word][0] & 0x0000000000FFFFFF) << 9
	end

	def get_offset
		return (self[:dva_word][1] & 0x7FFFFFFFFFFFFFFF) << 9
	end

	def get_gang
		return self[:dva_word][1] & 0x8000000000000000 == 1 ? true : false
	end

	def summary
		{
			vdev: 	get_vdev,
			grid: 	get_grid,
			asize: 	"#{get_asize.to_human} - 0x#{get_asize.to_s(16)}",
			gang: 	get_gang,
			offset: "#{get_offset.to_human} - 0x#{get_offset.to_s(16)}"
		}
	end

end


SPA_DVAS_PER_BP = 3

class Blkptr_t < NiceFFI::Struct

	@@read_cksum = []

	layout	:blk_dva, 		[Dva_t, SPA_DVAS_PER_BP],
			:blk_prop,		:uint64,
			:blk_pad,		[:uint64, 2],
			:blk_phys_birth,:uint64,
			:blk_birth,		:uint64,
			:blk_fill,		:uint64,
			:blk_cksum,		Zio_cksum_t

	def get_endian
		return (self[:blk_prop] & 0x8000000000000000)>>63 == 0 ? :big_endian : :little_endian
	end

	def get_level
		return (self[:blk_prop] & 0x7F00000000000000)>>56
	end

	def get_compression
		return [:inherit, :on, :off, :lzjb, :empty, :g1, :g2, :g3, :g4, :g5, :g6, :g7, :g8, :g9, :zle, :lz4][(self[:blk_prop] & 0x000000FF00000000)>>32]
	end

	def get_lsize
		return ((self[:blk_prop] & 0x000000000000FFFF) + 1) << 9
	end

	def get_psize
		return (((self[:blk_prop] & 0x00000000FFFF0000)>>16) + 1) << 9
	end

	def get_type
		return TYPESTRINGS[((self[:blk_prop] & 0x00FF000000000000)>>48)]
	end

	def get_cksumt
		return [:inherit, :on, :off, :label, :gang_header, :zilog, :fletcher2, :fletcher4, :sha256, :zilog2][((self[:blk_prop] & 0x0000FF0000000000)>>40)]
	end

	def get_fill
		return self[:blk_fill]
	end

	def get_phys_birth
		return self[:blk_phys_birth]
	end

	def get_birth
		return self[:blk_birth]
	end

	def summary
		{
			dvas: blk_dva.summary,
			endian: get_endian,
			level: get_level,
			type: get_type,
			cksumt: get_cksumt,
			compression: get_compression,
			psize: "#{get_psize} - 0x#{get_psize.to_s(16)}",
			lsize: "#{get_lsize} - 0x#{get_lsize.to_s(16)}",
			birth: "#{get_birth}L/#{(get_phys_birth == 0) ? get_birth : get_phys_birth}P",
			fill: get_fill,
			checksum: blk_cksum.summary
		}
	end

	def raidzMap offset, psize, unit_shift
		b = offset >> unit_shift
		s = psize >> unit_shift

		f = b % $pool[:dcols]
		o = (b / $pool[:dcols]) << unit_shift
		q, r, c, bc, col, acols, scols, coff, devidx, asize, tot = nil

		q = s /($pool[:dcols] - $pool[:nparity])
		r = s - q * ($pool[:dcols] - $pool[:nparity])
		bc = ((r == 0) ? 0 : (r + $pool[:nparity]))
		tot = s + $pool[:nparity] * (q + ((r == 0) ? 0 : 1))

		if q == 0
			acols = bc
			scols = [$pool[:dcols], roundup(bc, $pool[:nparity] + 1)].min
		else
			acols = $pool[:dcols]
			scols = $pool[:dcols]
		end

		assert {acols <= scols}

		rm = { 	:cols => acols,
				:scols => scols,
				:bigcols => bc,
				:skipstart => bc,
				:firstdatacol => $pool[:nparity],
				:col => Array.new}

		asize = 0

		acols.times do |c|
			col = f + c
			coff = o
			if col >= $pool[:dcols]
				col -= $pool[:dcols]
				coff += 1 << unit_shift
			end

			rm[:col][c] = { :devidx => col,
							:offset => coff,
							:csize => nil}
			csize = nil
			if c >= acols
				csize = 0
			elsif c < bc
				csize = (q + 1) << unit_shift
			else
				csize = q << unit_shift
			end
			rm[:col][c][:csize] = csize
					
			asize += csize
		end

		assert {asize == (tot << unit_shift)}
		rm[:asize] = roundup(asize, ($pool[:nparity] + 1) << unit_shift)
		rm[:nskip] = roundup(tot, $pool[:nparity] + 1) - tot
		assert {rm[:asize] - asize == rm[:nskip] << unit_shift}
		assert {rm[:nskip] <= $pool[:nparity]}

		# If all data stored spans all columns, there's a danger that parity
		# will always be on the same device and, since parity isn't read
		# during normal operation, that that device's I/O bandwidth won't be
		# used effectively. We therefore switch the parity every 1MB.
		#
		# ... at least that was, ostensibly, the theory. As a practical
		# matter unless we juggle the parity between all devices evenly, we
		# won't see any benefit. Further, occasional writes that aren't a
		# multiple of the LCM of the number of children and the minimum
		# stripe width are sufficient to avoid pessimal behavior.
		# Unfortunately, this decision created an implicit on-disk format
		# requirement that we need to support for all eternity, but only
		# for single-parity RAID-Z.

		assert {rm[:cols] >= 2}
		assert {rm[:col][0][:size] == rm[:col][1][:size]}

		if rm[:firstdatacol] == 1 and ((offset & (1 << 20) != 0))
			devidx = rm[:col][0][:devidx]
			o = rm[:col][0][:offset]
			rm[:col][0][:devidx] = rm[:col][1][:devidx]
			rm[:col][0][:offset] = rm[:col][1][:offset]
			rm[:col][1][:devidx] = devidx
			rm[:col][1][:offset] = o

			if rm[:skipstart] == 0
				rm[:skipstart] = 1
			end
		end
		return rm
	end

	def get_data check = true
		#assert {not @@read_cksum.include?(self[:blk_cksum][:zc_word].to_a)}
		valid = false
		voffset = nil # TODO remove
		raidz_col = nil # TODO remove
		level = self.get_level
		lsize = self.get_lsize
		psize = self.get_psize

		dsize = ((psize >> $pool[:ashift]) + 1).floor << $pool[:ashift] # round to the superior sector

		data = nil
		try = 0
		self[:blk_dva].each do |dva|
			try += 1
			voffset = dva.get_offset
			asize = dva.get_asize
			vdev = dva.get_vdev

			if $pool[:nparity] == 0
				fd = $pool[:vdevs][vdev][:fd]
				fd.sysseek (voffset + 0x400000) # skip L0 et L1
				data = fd.sysread psize
			else
				raidz_meta = raidzMap(voffset, dsize, $pool[:ashift])

				assert {asize == raidz_meta[:asize]}

				raidz_col = raidz_meta[:col]

				# We don't care about the parity column(s) yet
				raidz_col.shift raidz_meta[:firstdatacol]

				# Read each column
				data = String.new
				raidz_col.each do |col|
					devidx = col[:devidx]
					offset = (col[:offset]) + 0x400000 # skip L0 et L1
					csize = [col[:csize],(psize - data.size)].min
					
					fd = $pool[:vdevs][devidx][:fd]
					fd.sysseek offset
					cdata = fd.sysread csize

					data += cdata
				end
			end
			assert {data.size == psize}


			cksum = Fletcher4::check data, data.size

			valid = (cksum == [self[:blk_cksum][:zc_word][0], self[:blk_cksum][:zc_word][1], self[:blk_cksum][:zc_word][2],	self[:blk_cksum][:zc_word][3]])
			puts "cksum : #{valid ? "valid" : "invalid"} at try #{try} with #{raidz_col.size} cols"
			valid = true if $dump and try == 2
			break if valid or check == false
		end
		assert {valid} if check
		@@read_cksum.push(self[:blk_cksum][:zc_word].to_a)

		# decompress data if needed
		if self.get_compression == :on or self.get_compression == :lzjb
			data = Lzjb::decompress data, lsize
			assert {data.size == lsize}
		elsif self.get_compression != :off
			raise "Unimplemented decompressor"
		end

		if $dump
			File.open("dump#{voffset}","w").write data
		end
		return data
	end

end

class Uberblock_t < NiceFFI::Struct
	layout 	:ub_magic, 		:uint64,
			:ub_version, 	:uint64,
			:ub_txg,		:uint64,
			:ub_guid_sum,	:uint64,
			:ub_timestamp,	:uint64,
			:ub_rootbp,		Blkptr_t,
			:ub_software_version, :uint64

	def summary
		{
			magic: 		"0x"+ub_magic.to_s(16),
			version: 	ub_version,
			txg: 		ub_txg,
			guid_sum: 	ub_guid_sum,
			timestamp: 	Time.at(ub_timestamp),
			rootbp: 	ub_rootbp.summary,
			soft_version: ub_software_version
		}
	end
end

DNODE_SHIFT = 9
DNODE_CORE_SIZE = 64
SPA_BLKPTRSHIFT = 7
DNODE_SIZE = (1 << DNODE_SHIFT)
DN_MAX_BONUSLEN = (DNODE_SIZE - DNODE_CORE_SIZE - (1 << SPA_BLKPTRSHIFT))

class Dnode_phys_t < NiceFFI::Struct
	layout	:dn_type,				:uint8,
    		:dn_indblkshift,		:uint8,
    		:dn_nlevels,			:uint8,
    		:dn_nblkptr,			:uint8,
    		:dn_bonustype,			:uint8,
    		:dn_checksum,			:uint8,
    		:dn_compress,			:uint8,
    		:dn_flags,				:uint8,
    		:dn_datablkszsec,		:uint16,
    		:dn_bonuslen,			:uint16,
    		:dn_pad2,				[:uint8, 4],
		
		  	:dn_maxblkid,			:uint64,
		  	:dn_used,				:uint64,

  			:dn_pad3,				[:uint64, 4],

  			:dn_blkptrbonus,		[:char, [3*Blkptr_t.size, DN_MAX_BONUSLEN].max],
    		:dn_spill,				Blkptr_t

    def summary
    	{
    		type:					get_type,
    		indblkshift:			dn_indblkshift,
    		nlevels:				dn_nlevels,
    		nblkptr:				dn_nblkptr,
    		bonustype:				dn_bonustype,
    		checksum:				dn_checksum,
    		compress:				dn_compress,
    		flags:					dn_flags,
    		datablkszsec:			dn_datablkszsec,
    		bonuslen:				dn_bonuslen,
		  	
		  	maxblkid:				dn_maxblkid,
		  	used:					dn_used,
  			
  			#dn_blkptrbonus:			[:char, [3*Blkptr_t.size, DN_MAX_BONUSLEN].max],
    		spill:					dn_spill.summary
    	}
    end

    def get_type
    	TYPESTRINGS[dn_type]
    end

    def get_bonus
    	return self[:dn_blkptrbonus].to_ptr.read_bytes(DN_MAX_BONUSLEN)[Blkptr_t.size, DN_MAX_BONUSLEN - Blkptr_t.size]
    end

    def get_blkptrs
    	data =self[:dn_blkptrbonus].to_ptr.read_bytes(Blkptr_t.size*3)
    	ret=[Blkptr_t.new(data[0*Blkptr_t.size,Blkptr_t.size]),Blkptr_t.new(data[1*Blkptr_t.size,Blkptr_t.size]),Blkptr_t.new(data[2*Blkptr_t.size,Blkptr_t.size])]
    end

    def get_path blkid
    	indblkshift = self[:dn_indblkshift]
    	nlevels = self[:dn_nlevels]

    	blkptr_per_indblk = (1 << indblkshift) / Blkptr_t.size
    	relpath = []
    	q = blkid
    	begin
    		q, r = q / blkptr_per_indblk, q %  blkptr_per_indblk
    		relpath.push r
    	end while r > 2 # While it can't fit in the dnode's 3 blkptr

    	relpath += Array.new(nlevels - relpath.size){0}

    	offset = 0
    	path = relpath.reverse.map do |blkid|
    		blkid += offset
    		offset += blkid * blkptr_per_indblk
    		blkid
    	end

    	return path.reverse
    end

    def get_data first_blkid=0, last_blkid=dn_maxblkid

    	max_blkid = self[:dn_maxblkid]
    	nlevels  = self[:dn_nlevels]
    	
    	#assert {first_blkid >= 0 and last_blkid <= max_blkid}

    	first_path = get_path first_blkid
    	last_path = get_path last_blkid
    	assert {first_path.size == nlevels and last_path.size == nlevels}
    
    	size_path=[first_path,last_path].transpose.map{|i| i.last - i.first}

    	data = Array.new(nlevels+1){String.new}
    	data[nlevels] += self[:dn_blkptrbonus].to_ptr.read_bytes (Blkptr_t.size*3)

    	(nlevels).downto(1).each do |cur_level|
	    	(size_path[cur_level-1]+1).times do |i|
	    		blkptr = Blkptr_t.new data[cur_level][(first_path[cur_level-1]+i)*Blkptr_t.size, Blkptr_t.size]
	    		data[cur_level-1] += blkptr.get_data
	    	end
	    end
		

		assert {data[0].size == self[:dn_datablkszsec]*512*(last_blkid-first_blkid+1)}
		return data[0]
	end

end

class Dsl_dir_phys_t < NiceFFI::Struct
	layout	:dd_creation_time,				:uint64,
			:dd_head_dataset_obj,			:uint64,
			:dd_parent_obj,					:uint64,
			:dd_origin_obj,					:uint64,
			:dd_child_dir_zapobj,			:uint64,
			:dd_used_bytes,					:uint64,
			:dd_compressed_bytes,			:uint64,
			:dd_uncompressed_bytes,			:uint64,
			:dd_quota,						:uint64,
			:dd_reserved,					:uint64,
			:dd_props_zapobj,				:uint64,
			:dd_deleg_zapobj,				:uint64,
			:dd_flags,						:uint64,
			:dd_used_breakdown,				:uint64,
			:dd_clones,						:uint64,
			:dd_pad,						[:uint64, 13]
end

class Dsl_dataset_phys_t < NiceFFI::Struct
	layout	:ds_dir_obj,					:uint64,
			:ds_prev_snap_obj,				:uint64,
			:ds_prev_snap_txg,				:uint64,
			:ds_next_snap_obj,				:uint64,
			:ds_snapnames_zapobj,			:uint64,
			:ds_num_children,				:uint64,
			:ds_creation_time,				:uint64,
			:ds_creation_txg,				:uint64,
			:ds_deadlist_obj,				:uint64,
			:ds_referenced_bytes,			:uint64,
			:ds_compressed_bytes,			:uint64,
			:ds_uncompressed_bytes,			:uint64,
			:ds_unique_bytes,				:uint64,
			:ds_fsid_guid,					:uint64,
			:ds_guid,						:uint64,
			:ds_flags,						:uint64,
			:ds_bp,							Blkptr_t,
			:ds_next_clones_obj,			:uint64,
			:ds_props_obj,					:uint64,
			:ds_userrefs_obj,				:uint64,
			:ds_pad,						[:uint64, 5]
end

class Zil_header_t < NiceFFI::Struct
	layout	:zh_claim_txg,		:uint64,
			:zh_replay_seq,		:uint64,
			:zh_log,			Blkptr_t,
			:zh_claim_blk_seq,	:uint64,
			:zh_flags,			:uint64,
			:zh_claim_lr_seq,	:uint64,
			:zh_pad,			[:uint64, 3]

	def summary
		{
			claim_txg:		zh_claim_txg,
			replay_seq:		zh_replay_seq,
			log:			zh_log.summary,
			claim_blk_seq:	zh_claim_blk_seq,
			flags:			zh_flags,
			claim_lr_seq:	zh_claim_lr_seq
		}
	end
end


OBJSET_PHYS_SIZE = 2048
class Objset_phys_t < NiceFFI::Struct
	layout	:os_meta_dnode,			Dnode_phys_t,
			:os_zil_header,			Zil_header_t,
			:os_type,				:uint64,
			:os_flags,				:uint64,
			:os_pad,				[:char, OBJSET_PHYS_SIZE - Dnode_phys_t.size*3 - Zil_header_t.size - 8*2],
			:os_userused_dnode,		Dnode_phys_t,
			:os_groupused_dnode,	Dnode_phys_t

	def summary
		{
			meta_dnode: 	os_meta_dnode.summary,
			zil_header: 	os_zil_header.summary,
			type: 			os_type,
			flags: 			os_flags,
			userused_dnode: os_userused_dnode.summary,
			groupused_dnode: os_groupused_dnode.summary
		}
	end
end


# ZAP classes

ZBT_LEAF = ((1 << 63) + 0)
ZBT_HEADER = ((1 << 63) + 1)
ZBT_MICRO = ((1 << 63) + 3)

class Uint64 < NiceFFI::Struct
	layout	:value,					:uint64
end

# Fat ZAP classes

ZAP_MAGIC = 0x2F52AB2AB
ZAP_LEAF_MAGIC = 0x2AB1EAF
ZAP_LEAF_CHUNKSIZE = 24
ZAP_LEAF_ARRAY_BYTES = (ZAP_LEAF_CHUNKSIZE - 3)

class Zap_table_phys_t < NiceFFI::Struct
	layout	:zt_blk,				:uint64,
			:zt_numblks,			:uint64,
			:zt_shift,				:uint64,
			:zt_nextblk,			:uint64,
			:zt_blks_copied,		:uint64
end

class Zap_phys_t < NiceFFI::Struct
	layout	:zap_block_type,		:uint64,
			:zap_magic,				:uint64,
			:zap_ptrtbl,			Zap_table_phys_t,
			:zap_freeblk,			:uint64,
			:zap_num_leafs,			:uint64,
			:zap_num_entries,		:uint64,
			:zap_salt,				:uint64,
			:zap_normflags,			:uint64,
			:zap_flags,				:uint64

	def initialize data
		@data = data
		super data[0,self.size]
		assert {self[:zap_magic] == ZAP_MAGIC}
		assert {self[:zap_ptrtbl][:zt_blk] == 0} # Otherwise not implemented yet
		assert {(1<<(self[:zap_ptrtbl][:zt_shift]+5)) == @data.size}
	end

	def get_leaf idx
		ptrtbl_offset = (1 << (self[:zap_ptrtbl][:zt_shift] - 3 - 1))
		puts "offset: #{ptrtbl_offset}"
		leaf_offset = Uint64.new(@data[(idx + ptrtbl_offset)*Uint64.size,Uint64.size])[:value]
		assert {idx == 0}
		leaf = Zap_leaf_phys_t.new(@data[(1<<(self[:zap_ptrtbl][:zt_shift])+4),(1<<(self[:zap_ptrtbl][:zt_shift])+4)])
		return leaf
	end



end

class Zap_leaf_header_t < NiceFFI::Struct
	layout	:lh_block_type,			:uint64,
	 		:lh_pad1,				:uint64,
	 		:lh_prefix,				:uint64,
	 		:lh_magic,				:uint32,
	 		:lh_nfree,				:uint16,
	 		:lh_nentries,			:uint16,
	 		:lh_prefix_len,			:uint16,
			:lh_freelist,			:uint16,
			:lh_flags,				:uint8,
			:lh_pad2,				[:uint8, 11]

end

class Zap_leaf_phys_t < NiceFFI::Struct
	layout	:l_hdr,					Zap_leaf_header_t,
			:l_hash,				[:uint16, 1]

	def initialize data
		@data = data
		super data[0,self.size]
		assert {self[:l_hdr][:lh_block_type] == ZBT_LEAF}
		assert {self[:l_hdr][:lh_magic] == ZAP_LEAF_MAGIC}
	end

	def get_chunk idx
		mp = FFI::MemoryPointer.new :char, Zap_leaf_chunk.size
		mp.put_bytes(0,@data[Zap_leaf_header_t.size+1024+Zap_leaf_chunk.size*idx,Zap_leaf_chunk.size ], 0, Zap_leaf_chunk.size)
		chunk = Zap_leaf_chunk.new mp
		case chunk[:l_entry][:le_type]
		when 253
			return chunk[:l_free]
		when 252
			return chunk[:l_entry]
		when 251
			return chunk[:l_array]
		else
			raise "WTF CHUNK TYPE"
		end
	end

	def get_entry idx
		entry_c = get_chunk idx
		name_c = get_chunk entry_c[:le_name_chunk]
		assert {name_c[:la_next] == 0xFFFF}
		value_c = get_chunk entry_c[:le_value_chunk]
		assert {value_c[:la_next] == 0xFFFF}
		return [name_c[:la_array], value_c[:la_array].map{|c| c}*' ']

	end

#ZAP_LEAF_CHUNK(l, idx) \
#	((zap_leaf_chunk_t *) ((l)->l_phys->l_hash + ZAP_LEAF_HASH_NUMENTRIES(l)))[idx]
#
#ZAP_LEAF_ENTRY(l, idx) (&ZAP_LEAF_CHUNK(l, idx).l_entry)
end

class Zap_leaf_entry_t < NiceFFI::Struct
	layout	:le_type,					:uint8,
			:le_value_intlen,			:uint8,
			:le_next,					:uint16,
			:le_name_chunk,				:uint16,
			:le_name_numints,			:uint16,
			:le_value_chunk,			:uint16,
			:le_value_numints,			:uint16,
			:le_cd,						:uint32,
			:le_hash,					:uint64
end

class Zap_leaf_array_t < NiceFFI::Struct
	layout	:la_type,					:uint8,
			:la_array,					[:uint8, ZAP_LEAF_ARRAY_BYTES],
			:la_next,					:uint16
end

class Zap_leaf_free_t < NiceFFI::Struct
	layout	:lf_type,					:uint8,
			:lf_pad,					[:uint8, ZAP_LEAF_ARRAY_BYTES],
			:lf_next,					:uint16
end

class Zap_leaf_chunk < FFI::Union
	layout	:l_entry,				Zap_leaf_entry_t,
			:l_array,				Zap_leaf_array_t,
			:l_free,				Zap_leaf_free_t
end

# Micro ZAP classes

MZAP_ENT_LEN = 64
MZAP_NAME_LEN = (MZAP_ENT_LEN - 8 - 4 - 2)

class Mzap_ent_phys_t < NiceFFI::Struct
	layout	:mze_value,				:uint64,
			:mze_cd,				:uint32,
			:mze_pad,				:uint16,
			:mze_name,				[:char, MZAP_NAME_LEN]
end

class Mzap_phys_t < NiceFFI::Struct
	layout	:mz_block_type,			:uint64,
			:mz_salt,				:uint64,
			:mz_normflags,			:uint64,
			:mz_pad,				[:uint64, 5],
			:mz_chunk,				[Mzap_ent_phys_t, 1]
end




# ------------------------------- Helpers ----------------------------------

def getLabelUberblocks vdevId, labelId
	vdev = $pool[:vdevs][vdevId]
	uberblocks = []
	128.times do |i|
		vdev[:fd].sysseek (labelId >= 2 ? vdev[:size] - 1024*512 : 0) + (labelId%2)*1024*256 + 1024*128+(1024*i)
		u = Uberblock_t.new (vdev[:fd].sysread Uberblock_t.size)
		uberblocks << u if u[:ub_magic] == 12235020
	end
	return uberblocks
end

# ----------------------------Main code-------------------------------------

# Open vdevs
$pool[:vdevs].each do |vdev|
	vdev[:fd] = File.open vdev[:path], "r"
	vdev[:size] = `blockdev --getsize64 #{vdev[:path]}`.to_i
end

def main txg = nil

	# Find all uberblocks
	uberblocks={}
	($pool[:vdevs].size-1).downto 0 do |vdev|
		0.upto 3 do |i|
			(getLabelUberblocks vdev, i).each do |u|
				uberblocks[u[:ub_txg]] = u 
			end
		end
	end

	# Which one should we use
	if txg.nil?
		print_title "Available uberblocks txg by date"
		uberblocks.values.sort_by{|u| u[:ub_txg]}.each do |u|
			puts "#{u[:ub_txg]} - #{Time.at(u[:ub_timestamp])}"
		end

		begin
			txg = (ask "Selected uberblock's txg?").to_i
		end while not uberblocks.has_key? txg
	end
	uberblock = uberblocks[txg]

	# Get the MOS
	meta_dnode = (Objset_phys_t.new uberblock.ub_rootbp.get_data).os_meta_dnode
	mos_raw = meta_dnode.get_data
	mos = []
	(mos_raw.size / 512).times do |offset|
		mos <<  (Dnode_phys_t.new mos_raw[512*offset, 512])
	end

	# Print the MOS
	mos.each_index do |i|
		dnode = mos[i]
		puts "#{i} - #{dnode.get_type}" if dnode.dn_type > 0 and dnode.dn_type < TYPESTRINGS.size
	end

	return
end

## Get one uberblock block pointer
#vdev = $pool[:vdevs][0][:fd]
#if ARGV.size < 1
#	# Print uberblocks
#	print_title "uberblocks"
#	ap getL0Uberblocks vdev
#	uberblock_offset = Integer(ask "Which Uberblock offset do you want?")
#else
#	uberblock_offset = Integer(ARGV[0])
#end
#vdev.sysseek 1024*128+(1024*uberblock_offset)
#uberblock = Uberblock_t.new (vdev.sysread Uberblock_t.size)
#uberblock_bp = uberblock[:ub_rootbp]
#
#
#puts "reading Objset MOS"
## Get first Objset containing the MOS
##$dump = true
#objset = Objset_phys_t.new(uberblock_bp.get_data)
##$dump = false
#puts "reading MOS"
## Get the MOS
#mos_data = objset[:os_meta_dnode].get_data 0,objset[:os_meta_dnode][:dn_maxblkid]
#
#mos = Array.new
#
#
#(mos_data.size / 512).times do |i|
#	mos.push Dnode_phys_t.new mos_data[512*i, 512]
#end
#id=0
#puts "============> #{mos.size}"
#mos.each do |dnode|
#	if dnode[:dn_type] == 16
#		puts "=> #{id} <="
#		puts dnode.to_s 
#		dset = Dsl_dataset_phys_t.new(dnode.get_bonus)
#		puts dset.to_s
#		puts
#		puts
#	end
#	id+=1
#end
#
#exit 0
##dir_dnode=mos[0x05]
##dir = Dsl_dir_phys_t.new(dir_dnode.get_bonus)
##puts dir.to_s
##exit 0
#
#
#mos.size.times do |i|
#	begin
#		dset_dnode = mos[i]
#		puts "dn_type #{dset_dnode[:dn_type]} i=#{i}"
#		if dset_dnode[:dn_type] == 16 #DMU_OT_DSL_DATASET
#			puts "====================================================================================================="
#			puts "====================================================================================================="
#			puts "====================================================================================================="
#			puts dset_dnode.to_s
#
#			#dset_dnode.get_blkptrs[2].pretty
#
#			
#			dset =Dsl_dataset_phys_t.new(dset_dnode.get_bonus)
#			puts dset.to_s
#			dset_dnode.get_blkptrs[2].pretty
#			
#
#
#			if dset[:ds_creation_txg] == 55107 
#
#
#
#				data = dset_dnode.get_blkptrs[2].get_data
#				obj = Objset_phys_t.new data
#				puts obj.to_s
#				puts obj[:os_meta_dnode].to_s
#				b = obj[:os_meta_dnode].get_blkptrs[0]
#				b.pretty
#				b = Blkptr_t.new b.get_data[0, Blkptr_t.size]
#				b.pretty
#				b = Blkptr_t.new b.get_data[0, Blkptr_t.size]
#				b.pretty
#				b = Blkptr_t.new b.get_data[0, Blkptr_t.size]
#				b.pretty
#				b = Blkptr_t.new b.get_data[0, Blkptr_t.size]
#				b.pretty
#				b = Blkptr_t.new b.get_data[0, Blkptr_t.size]
#				b.pretty
#				b = Blkptr_t.new b.get_data[0, Blkptr_t.size]
#				b.pretty
#				zpl = Dnode_phys_t.new b.get_data[512*1,Dnode_phys_t.size]
#				puts zpl.to_s
#			end
#			puts "-----------------------------------------------------------------------------------------------------"
#			puts "-----------------------------------------------------------------------------------------------------"
#			puts "-----------------------------------------------------------------------------------------------------"
#		end
#	rescue => e
#		puts e
#		puts e.backtrace
#	end
#end
#
## 1  DMU_OT_OBJECT_DIRECTORY
## 5  DMU_OT_BPOBJ,
## 8  DMU_OT_SPACE_MAP,
## 10 DMU_OT_DNODE
## 11 DMU_OT_OBJSET
## 12 DMU_OT_DSL_DIR
## 13 DMU_OT_DSL_DIR_CHILD_MAP
## 14 DMU_OT_DSL_DS_SNAP_MAP
## 15 DMU_OT_DSL_PROPS
## 16 DMU_OT_DSL_DATASET,
## 17 DMU_OT_ZNODE
## 18 
## 19 DMU_OT_PLAIN_FILE_CONTENTS
## 20 DMU_OT_DIRECTORY_CONTENTS
## 21 DMU_OT_MASTER_NODE
#
#exit 0
#f=File.open("aaa","w")
#mos.each do |dnode| 
#	puts dnode.to_s
#	begin
#		if dnode[:dn_nlevels] != 0
#			data= dnode.get_data 0, dnode[:dn_maxblkid]
#			f.write data
#		end
#	rescue
#	end
#end
#
#object_directory = mos[1]
#puts object_directory.to_s
#
#puts "reading ZAP"
#zap_data = object_directory.get_data 0, object_directory[:dn_maxblkid]
#zap_type = Uint64.new zap_data[0,Uint64.size]
#
#case zap_type[:value]
#when ZBT_MICRO # microzap
#	zap = Mzap_phys_t.new zap_data[0,Mzap_phys_t.size]
#when ZBT_HEADER # fatzap
#	zap = Zap_phys_t.new zap_data
#else
#	raise "Unknown zap type"
#end
#
#leaf = (zap.get_leaf 0)
#puts leaf[:l_hdr].to_s
#
#
#entry = leaf.get_entry 0
#puts entry.to_s
#
## OSEF root_dataset = 2
#
#root_dataset = mos[2]
#assert {root_dataset[:dn_type] == 12} #DMU_OT_DSL_DATASET 
#puts root_dataset.to_s
#
#dp_root_dir = Dsl_dir_phys_t.new(root_dataset.get_bonus[0,Dsl_dir_phys_t.size])
#puts dp_root_dir[:dd_child_dir_zapobj]
#
#temp = mos[dp_root_dir[:dd_child_dir_zapobj]]
#puts temp.to_s
#zap_data = temp.get_data 0, temp[:dn_maxblkid]
#zap_type = Uint64.new zap_data[0,Uint64.size]
#
#case zap_type[:value]
#when ZBT_MICRO # microzap
#	zap = Mzap_phys_t.new zap_data[0,Mzap_phys_t.size]
#when ZBT_HEADER # fatzap
#	zap = Zap_phys_t.new zap_data
#else
#	raise "Unknown zap type"
#end
#puts zap_data
#
#
#
#dp_mos_dir = mos[5]
#dp_origin_dir = mos[12]
#

if __FILE__ == $0
	require 'pry'
	binding.pry quiet: true
end