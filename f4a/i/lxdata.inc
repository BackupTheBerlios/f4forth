//  ./lxdata.inc : kernel 0204018 - stat include/asm/stat.h:
	.struct 0
stat:
stat_st_dev:	.word 0	/*;unsigned short st_dev;	*/
stat___pad1:	.word 0	/*;unsigned short __pad1;	*/
stat_st_ino:	.long 0	/*;unsigned long  st_ino;	*/
stat_st_mode:	.word 0	/*;unsigned short st_mode;	*/
stat_st_nlink:	.word 0	/*;unsigned short st_nlink;	*/
stat_st_uid:	.word 0	/*;unsigned short st_uid;	*/
stat_st_gid:	.word 0	/*;unsigned short st_gid;	*/
stat_st_rdev:	.word 0	/*;unsigned short st_rdev;	*/
stat___pad2:	.word 0	/*;unsigned short __pad2;	*/
stat_st_size:	.long 0	/*;unsigned long  st_size;	*/
stat_st_blksize:.long 0	/*;unsigned long  st_blksize;	*/
stat_st_blocks:	.long 0	/*;unsigned long  st_blocks;	*/
stat_st_atime:	.long 0	/*;unsigned long  st_atime;	*/
stat___unused1:	.long 0	/*;unsigned long  __unused1;	*/
stat_st_mtime:	.long 0	/*;unsigned long  st_mtime;	*/
stat___unused2:	.long 0	/*;unsigned long  __unused2;	*/
stat_st_ctime:	.long 0	/*;unsigned long  st_ctime;	*/
stat___unused3:	.long 0	/*;unsigned long  __unused3;	*/
stat___unused4:	.long 0	/*;unsigned long  __unused4;	*/
stat___unused5:	.long 0	/*;unsigned long  __unused5;	*/
stat_end:
.equ stat_size,(stat_end-stat)
