//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
//; =============================================================================
//; fig4th.h - asm constants defns
//; =============================================================================
//;
#ifndef FIG4TH_H
#define FIG4TH_H

// =========================================================================== //
#ifndef F4ASMCFG_H
#include "f4-asm-cfg.h"
#endif
//;
//; -----------------------------------------------------------------------------
//; host system
//; -----------------------------------------------------------------------------
//;

//;	ELF header
#define e_phoff     28	//; .d disp to program header(s)
#define e_phentsize 42	//; .w size of a program header
#define e_phnum	    44	//; .w no. of program headers
#define p_offset     4	//; file ptr to data
#define p_vaddr      8	//; destn addr, don't copy if equal to label <D>
#define p_filesz    16	//; size of data block
//;	elf cons
#define osabi	0	//; 'sysv'=0, 'linux'=3
#define PF_R	4
#define PF_W	2
#define PF_X	1
//;
//;	signals
#define SIGHUP  1
#define SIGQUIT 3
#define SIGILL  4
#define SIGBUS  7
#define SIGFPE  8
#define SIGKILL	9
#define SIGSEGV 11
#define SIGALRM 14
#define SIGCHLD 17
#define SIGWINCH 28
#define SA_SIGINFO 4	// ;asm/signal.h
#define SA_ONSTACK 0x08000000
#define SA_RESETHAND 0x80000000

#define NSIG 64
#define SIGRTMIN 32

#define sigmax 64	//; no. of standard+real-time signals
#define sighsizel 8	//; signal handler block size per longs (?)

// ; use special or, parent's hnd
#define alrmsig  0b00000000000000000100000000000000
#define fpesig   0b00000000000000000000000100000000
#define winchsig 0b00010000000000000000000000000000
// ; segv & buserror
#define segvsig  0b00000000000000000000100010000000
#define ilegsig  0b00000000000000000000000000010000
// ; silently return, noop
#define chldsig  0b00000000000000100000000000000000
// ; ctr/"\"	   10987654321098765432109876543210
#define quitsig  0b00000000000000000000000000001000
#define intsig   0b00000000000000000000000000000100
// ;		   UpIwPVxXuTtTsCcStApUsUkFbAtIqIh
// ;		   NwOiRTfCrTtStOhTeLiSeSiPuBrLuNu
// ;		   Ur nOAsPgOiToNlKrRpRgRlEsRaLiTp
// ;		   S  cFLzU UnPpTdFmMe2v1l  Tp t  
// ;		   E  h R         L
// ;		   D    M         T
// ;	disabled:
#undef  fpesig
#define fpesig 0
// ;    #define winchsig,	0
// ;    #define segvsig,	0
// ;    #define ilegsig,	0
// ;    #define quitsig,	0
#undef  chldsig
#define chldsig	0
// ; use parent's hnd if bit set
//	 so: #if (#lxver == 22)
//	 oder:
#if #lxver (22)
#define lxkontrolle 22
// ;		   10987654321098765432109876543210
#define sgnsys   (0b01010101000100100010000100100000&~(fpesig|winchsig|segvsig|ilegsig|quitsig|chldsig|alrmsig))
// ; exitting, message if rsp. bit unset
// .set sgmask,  0b01000001011000011000101011010110
#define sgmask   (0b01000001011000011000111011010110|fpesig|winchsig|segvsig|ilegsig|quitsig|chldsig|alrmsig)
#else	// ; sigint w. kernel 2.2
#define lxkontrolle 24
// ;		   10987654321098765432109876543210
#define sgnsys   (0b01010101000100100010000100100100&~(fpesig|winchsig|segvsig|ilegsig|quitsig|chldsig|alrmsig))
// ; exitting, message if rsp. bit unset
// .set sgmask,  0b01000001011000011000101011010110
#define sgmask   (0b01000001011000011000111011010010|fpesig|winchsig|segvsig|ilegsig|quitsig|chldsig|alrmsig)
#endif
//;
//;	sigh(label)	siginfo type signal handler defn block.
#define sigh(x)	.long x,SA_SIGINFO|SA_ONSTACK,0,0,0
//;
//; -----------------------------------------------------------------------------
//; error codes, messages
//; -----------------------------------------------------------------------------
//;	M_NIMP	 0	not implemented, unknown word
#define M_NIMP	 0
//;	M_STKMTY 1	data-stack empty
#define M_STKMTY 1
//;	M_DICTOV 2	dictionary full
#define M_DICTOV 2
//;	M_AMODE	 3	incorrect addressing mode
#define M_AMODE	 3
//;	M_NUNIQ	 4	not unique, word already known
#define M_NUNIQ	 4
//;	M_DISCRG 6	not within disc range
#define M_DISCRG 6
//;	M_STKOVF 7	data-stack overflow
#define M_STKOVF 7
//;	M_DISKER 8	disc access error
#define M_DISKER 8
//;	M_COMPO 17	use only while compiling
#define M_COMPO 17
//;	M_EXECO 18	use only while executing
#define M_EXECO 18
//;	M_PAIRS	19	conditionals not paired
#define M_PAIRS	19
//;	M_CSP   20	data-stack out of balance, definition not finished
#define M_CSP   20
//;	M_FENCE 21	address in protected dictionary, below @fence
#define M_FENCE 21
//;	M_LOAD  22	use only while "load"ing
#define M_LOAD  22
//;	M_EDSCR 23	off current editting screen
#define M_EDSCR 23
//;	M_VOCAB 24	declare vocabulary
#define M_VOCAB 24
//;	msgbits		dword, bits corresponding to def'd messages set
#define msgbits (1<<M_NIMP|1<<M_STKMTY|1<<M_DICTOV|1<<M_AMODE|1<<M_NUNIQ|1<<M_DISCRG|1<<M_STKOVF|1<<M_DISKER|1<<M_COMPO|1<<M_EXECO|1<<M_PAIRS|1<<M_CSP|1<<M_FENCE|1<<M_LOAD|1<<M_EDSCR|1<<M_VOCAB)
//;
//; -----------------------------------------------------------------------------
//; files, channels
//; -----------------------------------------------------------------------------

#define SEEK_SET 0 //; from beginning of file. 
#define SEEK_CUR 1 //; from current position.
#define SEEK_END 2 //; from end of file.

//;
//; <chans> - actual figure available by forth "chmax".
//;	number of supported file/device channels, a power of 2, max 64.
//;	dft 32, any greater no. requires adjustment of forth "w4f".
#define chans  32
//;
//; cdtsizeb, cdtsize	6, 1<<cdtsizeb
//;	min 64 bytes cdt size, any(?) power of 2 (ok:6..9)
//;	bytes per channel definition table
#define cdtsizeb 6
#define cdtsize (1<<cdtsizeb)
//;
//; cdt
//;	F4 system channel definition table,
//;	size (.cdte-.cdt) aligned to next greater or equal power of 2 (64)
//; chid	 0	file descriptor
//;	@chid = chinval	0xffffffff pseudo file descriptor 'invalid channel id'
//;		chfmmap	0x80000000 %10... ORed to fd after mmap-ing.
//;		chfmclo 0xbfffffff        ORed to fd after file closed.
//;	which can be checked by testing O flag after { 1 lshift }, whereafter
//;		NO and NC is an open channel
//;		NO and C  is a free cdt entry (closed channel).
//;		O  and NS is an mmap-ed channel
//;			then, another shift (or add reg,reg) returns
//;		       NS if the mmap-ed channel is open,
//;		        S channel closed but still mapped into memory.
// ; 1) open file		chid:=fd
// ; 2) mmap			set tyfl..bmapc, <chid> := <chid> OR <chfmmap>
// ; 3.0) close	not mapped	chid:=<chinval>
// ; 3.1) close mapped		chid:=<chfmclo>
// ; 4.0) unmap	open		clr tyfl..bmapc
// ; 4.1) unmap closed		chid:=<chinval>
#define chinval (-1)
#define chfmmap 0x80000000//;OR  to fd after mmapping
#define chfmclo 0xbfffffff//;OR  to fd after (any) file closed
#define chfmtst 0x3fffffff//;AND to fd by 'close', un-used chan if fd=<chfmtst>
// ; ------------
//;
//; tyfl	( -- ) cdt channel flags
//;  file descriptor type flags, dword at <cdt.tyfl>.
//;  lo 16 bits by F4, hi 16 bits reserved for file-type data from <sys_fstat>.
//;	
//;  <b...> bit number, <m...> bit mask
//;  bcons,mcons	 0 - 0001
//;	set if fd is echoing console
#define bcons 0
#define mcons (1<<bcons)
//;  bicon,micon	 1 - 0002
//;	set if 2ndary console opened
#define bicon 1
#define micon (1<<bicon)
//;  bcflg,mcflg	 2 - 0004
//;	character received flag, set if any char present (buffered)
#define bcflg 2
#define mcflg (1<<bcflg)
//;  bcrgt,mcrgt	 3 - 0008
//;	charcter reget flag, set if non-ascii char received (buffered)
#define bcrgt 3
#define mcrgt (1<<bcrgt)
//;  bedif,medif	 4 - 0010
//;	edit mode, 1st cursor backup done
#define bedif 4
#define medif (1<<bedif)
//;  bptyf,mptyf	 5 - 0020
//;	set if channel is a pty device
#define bptyf 5
#define mptyf (1<<bptyf)
//;  bqryf,mqryf	 6	(not used)
//;	{query}-compliant {accept} mode, terminating if buffer full
#define bqryf 6
#define mqryf (1<<bqryf)
//;  bchrf,mchrf	 7	(not used)
//;	nonblocking i/o if flag set (i.e. not waiting for availabilty), else "ndelay"
#define bchrf 7
#define mchrf (1<<bchrf)
//;  biowf,miowf	 8 - 0100
//;	inserting editting mode, overwriting if bit unset
#define biowf 8
#define miowf (1<<biowf)
//;  bwdwf,mwdwf	 9	(not used)
//;	console channel is a "window" (n.i.)
#define bwdwf 9
#define mwdwf (1<<bwdwf)
//;  bpipe,mpipe	10 - 0400
//;	channel is a pipe
#define bpipe 10
#define mpipe (1<<bpipe)
//;  bterm,mterm	11 - 0800
//;	channel is a terminal/console; re/set by 'vt?'
#define bterm 11
#define mterm (1<<bterm)
//;  bcann,mcann	12 - 1000
//;	channel in canonical input mode
#define bcann 12
#define mcann (1<<bcann)
//;  bdupc,mdupc	13 - 2000
//;	fd duplicated from previous redir chan, by <sys dup>, by 'load'ing words.
#define bdupc 13
#define mdupc (1<<bdupc)
//;  bmapc,mmapc	14 - 4000
//;	file/device mmapped, regardless of fd valid (file open) or not.
//;	valid only if bit 31 of <chid> also set and bit 30 un-set.
#define bmapc 14
#define mmapc (1<<bmapc)
//;  bxtnc,mxtnc	15 - 8000
//;	cdt extension field @chpt valid.
#define bxtnc 15
#define mxtnc (1<<bxtnc)
//;
//;  consflags	mcons|micon|mptyf|mterm - 0823
//;	bitflags either of which mark a terminal device
#define consflags (mcons|micon|mptyf|mterm)
//;
//;  setconsf	mcons|micon - 0003 
//;	bitflags either of which mark a console; re/set by 'open', 'cons?'
#define setconsf  (mcons|micon)
//;
//;  chid-.cdt	file descriptor (linux), dword.
//;
//;  binval, minval	31, sign-bit of flags dword
//;	set if channel id is not a file descriptor, 
//;	all bits set, @chid = 0xfffffff (-1) is an un-used cdt.
#define binval 31
#define minval (1<<binval)
//;  bmmval, ummval	30
//;	<bmmval> unset and <binval> set denotes an mmaped channel.
#define bmmval 30
#define ummval (1<<bmmval)

//;
//;  perm-.cdt
//;  file descriptor permission bits, dword.
//;
//;  d_fperm
//;	new files' permissions, dft 660. on file open taken from user vari (fperm),
#define d_fperm 0660

#define wait4c 0x80000000	//; flag to waiting for file input
//;
//; NVTIME:=0 and NVMIN:=0 for ?terminal
//; non-canonical input once entered will #not# time out! thus NVMIN=0
//; required to receiving EAGAIN if no input immediately available.
//; sys_read after NVTIME=0 & NVMIN=0 returns 0 if no char available.
#define NVTIME 0		//; console chars input timeout (+ve ok, too, but slow...)
#define NVMIN  0		//; min no. of chars to receive, EAGAIN only if set to 0

#define lwmin  80		//; min console/file-i/o line width by chars
#define lhmin  (-1)		//; min console window height, -1 if channel not a console
#define wminsz (((lwmin)<<16)|((lhmin)&((1<<16)-1))) 
//;
//; -----------------------------------------------------------------------------
//; memory set-up
//; -----------------------------------------------------------------------------
//;
#define page_size  (1<<page_shift)
#define page_mask  (-page_size)
#define page_align (page_size-1)
#define pg(x)	   (((x)+page_align)&page_mask)
//;
//; screenfile editor workspace,
//; a 'screen' is 1024 chars(bytes), modifyable defaults:
//;	ssbits 6		for 64 byte headers = one display line
//;	ssize (1<<ssbits)	bytes per screen-file buffer (64), "b/buf"
//;	nbuf (1024/ssize)	buffers per screen (1024/bytes-per-buffer)
//;	blkno  4		cell for block no. & update-flag
//;	blknl  4		cell for block terminating <nul>s
//;			=>	thus, b/buf+8 bytes per block buffer required.
//;	bblk   (ssize+blkno+blknl) bytes per block: blk#, content, <nul>s
//;	nscr   4		no. of screen buffers
//;
//;	cbyln  64		chars/line, for the screen-file editor
#define ssbits 6
#define ssize (1<<ssbits)
#define nbuf (1024/ssize)
#define blkno  4
#define blknl  4
#define bblk   (ssize+blkno+blknl)
#define nscr   4
//;
#define cbyln  64
#define bbybufn ssize
//;
//; user area, min 256 bytes,
//;	actual, +page-aligned size by used space in asm source plus user definable margin
#define uarea (uvarieu-uvariu)
//;
//;
//; memory blocks:  									sizes:
//;	.text:   [ f4 kernel ][ init.data ]             kernel, basic program memory
//;	.bss:    [dcbot)dictionary .. ->(dctop]         dictionary, after .text section (ini_bss)
//;	.ss:     [<- RS -rsmem- (r0| arg+env-strings ]  rstack, program return-stack    (ini_ss)
//;	mmapped: [s0) PS-> -datap- ]                    dstack, at @psbot, size @pssiz  (ini_ps)
//;	  "      [u0) -uarea-  ]                        uvari,  at @u0, size @uvsize    (ini_uvr)
//;	  "      [vabot) -variables- ]                  vmem,   at @vabot, @vasize      (ini_mva)
//;	  "      [first) -bmag- (limit|tib) -tibm- ]    blkbuf, at @sbbot, @sbsize      (ini_sbf)
//;
//;	bmag  (bblk*nbuf*nscr)	'bbuf', screenfile blocks buf, page_size aligned for mmap
//;	tibm  page_size		'tib', terminal input buffer, page_size aligned for mmap
//;	uvrm  uarea		'uvari', user-vari vector (1024.b), extendable by mremap
//;	varm  (page_size)	'vmem', indexed variables memory, extendable w. mmap
//;	rsmem (page_size)	'rstack', return stack, expands top down in .ss section
//;	datap (page_size)	'dstack', data stack, extendable w. mmap
//;	dictm (page_size)	dictionary, extendble w. sys_brk
//;
//; initially allocated 'page' sized defaults, self-extending at run-time, as required:
//;	ini_ps  (pg(datap))	data stack
//;	ini_ss  (pg(rsmem))	return-stack
//;	ini_sbf (pg(bmag))	screenfile block-buffer
//;	ini_tib (pg(tibm))	tib
//;	ini_bss (pg(dictm))	dictionary
//;	ini_var (pg(varm))	indexed variables
//;	ini_uvr (pg(uvrm))	per task user vari
//;
//; limits which if not remaining will trigger the rsp. ranges re-allocation:
//;	ps_xtd	datap		count of bytes by which 'mremap' extends dstack at ovf
//;	ps_min	(ps_xtd/4)	limit of free dstack before newly allocating w. mremap
//;	ds_brk	dictm		count of bytes by which sys_brk extends the dictionary
//;	ds_min	(ds_brk/4)	limit of free dictionary space for next sys_brk call
//;	va_xtd	varm		count of bytes by which 'mremap' extends vmem at ovf
//;	va_min	(va_xtd/4)	limit of free vari-mem before re-mapping
//;
//; other:
//;	ps_rsv	32		8 cells data-stack safety margin 
//;				for the syscalls and to preventing empty stack segfaults
//;	lumpi	256		disp from <here> to <pad> - was 69->80
//;	tibqi	lumpi		tib space for 'query'
//;
#define bmag  (bblk*nbuf*nscr)	//; screenfile blocks buffer, page_size aligned for mmap
#define tibm  page_size		//; tib, page_size aligned for mmap
#define uvrm  uarea		//; user-vari vector (1024.b), extendable by mremap
#define varm  (page_size)	//; indexed variables, 'mmap'ed
#define rsmem (page_size)	//; return stack, expands top down in .ss section
#define datap (page_size)	//; data stack, extendable w. mmap
#define dictm (page_size)	//; dictionary, extendble w. sys_brk
//;
#define ini_ps  (pg(datap))	//; data-stack
#define ini_ss  (pg(rsmem))	//; ret-stack
#define ini_sbf (pg(bmag))	//; screenfile block-buffer
#define ini_tib (pg(tibm))	//; tib
#define ini_bss (pg(dictm))	//; dictionary
#define ini_var (pg(varm))	//; indexed variables
#define ini_uvr (pg(uvrm))	//; per task user vari
//
#define ps_xtd	datap		//; count of bytes to at ovf extend dstack w. mremap
#define ps_min	(ps_xtd/4)	//; limit of free dstack before newly allocating w. mremap
#define ds_brk	dictm		//; new allocation after less than <ds_min> bytes free in dictionary
#define ds_min	(ds_brk/4)	//; limit of free dictionary space before newly allocating w. sys_brk
#define va_xtd	varm		//; count of bytes to at ovf extend vari-mem w. mremap
#define va_min	(va_xtd/4)	//; limit of free vari space before re-mapping
//
#define ps_rsv	32		//; 8 cells data-stack initial reserve against segfault on empty stack
#define lumpi	256		//; disp from <here> to <pad> - was 69->80
#define tibqi	lumpi		//; tib space for 'query'
//;
//; --------------------------------------------------------------------------- //
//; ascii/chars
//;
//; lo ctrls, <del>, <csi>, <nl>, <bl>, <blbl>, leading chars for directories
#define BEL 	7
#define BS	8
#define TAB	9
#define LF	10
#define VT	11
#define FF	12
#define CR	13
#define SO	14
#define SI	15
#define ESC	27
#define DEL	127
#define CSI	('['<<8|ESC)	//;ascii <csi>=155, fault in xterm/console, thus replaced by "<esc>["
#define NL	LF
//#define NL	(CR<<8|LF)	//;neither, 'OCRNL' nor 'OCRNL' helps.. but 'OPOST'!
#define CHR_DIR  47	// "/" directory
#define CHR_LDIR 46	// "." prepended for relative path
#define BL	32	// " "
#define BLBL (BL<<8|BL)	// "  "
//; d_pchr	lsb[32,126],[160,255]msb, iso8859 ranges of printing chars.
#define d_pchr  0xffa07e20
//; bidbt	text which can be entered as 'numeric' character, "[id], used
//;		in {id+} as a flag to early terminating, after vectored {[id]}.
#define bidbt (((']'<<8|'d')<<8|'i')<<8|'[')
//;
//; --------------------------------------------------------------------------- //
//;
//; Registers: (d)estroyable, (i)nput only, (F)orth reserved
//;    mm0..mm7 (d) not used - free for application, user code can communicate
//; TS	eax	(F) top cell of (data-)stack
//; NXL	ebx	(F) location of <next> entry, #def'd 'NX' expands to <jmp *ebx>
//; T	ecx	(i) address of entered code
//; W	edx	(i) parameter field pointer
//; UR	ebp	(F) ptr into user vari(disp+0x80), wrt address of uvari "(up)"
//; IP	esi	(F) forth next instruction pointer
//; PS	edi	(F) forth parameter-stack pointer
//; RS	esp	(F) forth and subroutines return-stack pointer
//; affix 'x' for 16bit reg, 'h'/'l' for hi/lo 8bit regs, rsp. - e.g. TSh = ah
#define TS eax
#define TSx ax
#define TSl al
#define TSh ah
#define NXL ebx
#define T  ecx
#define Tx  cx
#define Tl  cl
#define Th  ch
#define W  edx
#define Wx  dx
#define Wl  dl
#define Wh  dh
#define UR ebp
#define IP esi
#define PS edi
#define RS esp
//;
//; disp's to 4th regs by <pushal> opr -re- below
#define s_TS	s_eax
#define s_NXL	s_ebx
#define s_NX	s_ebx
#define s_T	s_ecx
#define s_W	s_edx
#define s_UR	s_ebp
#define s_IP	s_esi
#define s_PS	s_edi
#define s_RS	s_esp
//;
//; word header bits
//;	smud	bit#7 "smudge" bit, word findable if #set#(!)
#define smud 128
//;	imed	bit#6 "immediate" bit
#define imed 64
//;		bit#5 till unused... (may become flag for 'view field' or 'task' or optimizing words)
#define unus 32
//;	widt	max. len of a 4th-word's name + countbyte
#define widt 32
//; 	maxw	max. "width"-1, mask for <test ...>
#define maxw (widt-1)
//;	wmsk	width mask for "(find)"
#define wmsk (widt-1)
//; 	fmsk	(wmsk|smud), mask for "(find)"
#define fmsk ((widt-1)|smud)
//;
//; vocabulary base-name, flag bits
//;	vnfd, vnam	dword name-field, 2 bytes dummy name of (empty) vocabulary
#define vnam 0x2081	//; both, 2081 and A081 can be used, equally
#define vnfd ((255<<16|vnam)<<8|255)
//; 	bfind, mfind	vocflag bit# & mask, by (find) set if already searched
#define bfind 0
#define mfind (1<<bfind)
//; 	bfloat, mfloat	vocflag, bit #31, provisional for floats fetching [number]
#define bfloat 31
#define mfloat (1<<bfloat)
//;
// ; ---------------------------------------------------------------------------
#define lfa2cfa	4
#define lfa2pfa 8
#define cfa2pfa 4
#define vnfa2vlfa 3
// ; ---------------------------------------------------------------------------
//;
//; (bits)	( -- a )
//;	uvari, system state bit-flags
//;	winchb,winch	0	set after reception of SIGWINCH
#define winchb 0
#define winch (1<<winchb)
//;	edinsb,edins	1	"expect" editting insert/~overwrite mode; in l.s.byte
//;				inserting mode dft if F4 compiled w. "-D__expect_ins".
#define edinsb 1
#define edins (1<<edinsb)
//;	scrixb,scrix	2	set if executed as a script for implicite 'bye' at eof.
#define scrixb 2
#define scrix (1<<scrixb)
//;	cargsb,cargs	3	unset if commandline args evaluated or, none present.
#define cargsb 3
#define cargs (1<<cargsb)
//;	xargsb,xargs	4	set if executeable cmd-line args passed.
#define xargsb 4
#define xargs (1<<xargsb)
//;	lcdepb,lcdep	5	letter case independency if set ("find"), set by dft
#define lcdepb 5
#define lcdep (1<<lcdepb)
//;	incfib,incfi	6	whether input from {chain}ed file
#define incfib 6
#define incfi (1<<incfib)
//;	evalxb,evalx	7	whether input by {evaluate}
#define evalxb 7
#define evalx (1<<evalxb)
//;	edlnfb,edlnf	8	line editting w. default text, variant of expect
#define edlnfb 8
#define edlnf (1<<edlnfb)
//;	sgdfb,sgdf	9	cpl next opr 'signed', flg cleared immediately after.
#define sgdfb 9
#define sgdf (1<<sgdfb)
//; provisional for rational numbers - re <f4f.scr>:
//;	ranumb,ranumf	17	read next numeric input as 'ranum' (extn by f4f.scr)
#define ranumb 17
#define ranumf (1<<ranumb)
//;	floatb,floatf	19	persistent 'ranum' input mode (extn by f4f.scr)
#define floatb 19
#define floatf (1<<floatb)
//;	quadb,mquadf	20	next numeric input is quad
#define quadb 20
#define quadf (1<<quadb)
//;
//;	edplb 24(l.s.b.), edplf 24..31(bits), edpl 3(bytes) disp to bit 0 of dpl
//;			auxilary dpl for -ve exponent w/ rational numbers input
#define edpl	3
#define edplb	24
#define edplf	(0xff<<edplb)
//; quad ranum if mfloat,mranu,mquad all set and, medpl mask = $080
#define mdfloat ranumf|quadf	; single quad ranum
#define mfloatm floatf|quadf	; persistent, double ranum mode
//
#ifndef __expect_ins
#define expect_ins	0
#else
#define expect_ins	edins
#endif
//;
//; =============================================================================
//; asm

//;
//; disps into pushal/popal regs storage block
//;	s_tor, s_eax ... s_edi
#define s_tor	32
#define s_eax	28
#define s_ecx	24
#define s_edx	20
#define s_ebx	16
#define s_esp	12
#define s_ebp	8 
#define s_esi	4 
#define s_edi
//;
//;
// --------------------------------------------------------------------------- //
//; program specific

//;
//; NX		#define-d <jmp *NXL>, w. #define-d NXL = ebx
#define NX jmp *NXL

//; SPUSH  	push one dstack item
//; spush(x) 	push # x dstack items
#define SPUSH(x) .fill x,1,0xab
#define spush stosl
//; SPOP(n) 	drop n dstack items
//; spop   	drop top dstack item
#define SPOP(x) movl -4*(x)(PS),TS;leal -4*(x)(PS),PS
#define spop 	movl -4(PS),TS;leal -4(PS),PS

#define TOS0	   (PS)
#define TOS(x)	   -4*(x)(PS)
#define TOSPULL(x) lea -4*(x)(PS),PS
#define TOSPUSH(x) lea  4*(x)(PS),PS

//; uref(name)	ref to uvari
#define udsp    -uuseru-0x80
#define udef(x) .byte ((x)udsp)
#define udefl(x) .long ((x)udsp)
#define uref(x) (x)udsp(UR)
//; vdisp(name)	disp to mmaped vari
#define vdisp(x) (x)-D
//;
#define bytec(x) cfa2pfa+(x)
#define consv(x) x ## v
//;
#define cdtptr(x) (((x)<<cdtsizeb)+cdtv) //; ref to channel x definition table (x +ve or 0)
#define cdtref(x) ((x)-.cdt)	//; item in channel definition table
//;
//; --------------------------------------------------------------------------- //
//; hi-level forth control flow flags
//;	figure	pushed by		received by
//;
//;	1	begin			again, repeat, while, until
#define f_begin	1
//;	2	=if, if, else		endif
#define f_if	2
//;	3	do			+loop, loop
#define f_do	3
//;	4	while (recives 1)	repeat, until
#define f_while	4
//;	5	enter			entry
#define f_enter	5
//;
// ==============================================================================
//	conditional out-commenting; set from #define-d constants, re above
// ==============================================================================

// asm cpl vari which imply others:

#ifndef _dx_

#if (_bn_|_mx_|_rx_)	//; either one implies _dx_
#define _dx_	12
#endif

#endif
// ------------------------------------------------------------------------------
#ifndef _lx_

#if (_ld_|_rx_)		//; either one implies _lx_
#undef  _lx_
#define _lx_	12
#endif

#endif
// ------------------------------------------------------------------------------
#if _rx_		//; _rx_ implies _fx_ _dx_, _ld_, _lx_, _mx_, _sq_, _sb_
#undef  _fx_
#undef  _ld_
#undef  _mx_
#undef  _sq_
#undef  _sb_
#define _fx_	11
#define _ld_	11
#define _mx_	11
#define _sq_	11
#define _sb_	11
#endif
// ==============================================================================

// asm cpl vari
#if _0a_
#define _0a
#else
#define _0a	#
#endif

#if _af_
#define _af
#else
#define _af	#
#endif

#if _al_
#define _al
#else
#define _al	#
#endif

#if _bn_
#define _bn
#else
#define _bn	#
#endif

#if _ck_
#define _ck
#else
#define _ck	#
#endif

#if _oh_
#define oh_	#
#define _oh
#else
#define oh_
#define _oh	#
#endif

#if _fc_
#define _fc
#else
#define _fc	#
#endif

#if _fx_
#define _fx
#else
#define _fx	#
#endif

#if _hx_
#define _hx
#else
#define _hx	#
#endif

#if _i3_
#define _i3
#else
#define _i3	#
#endif

#if _ie_
#define _ie
#else
#define _ie	#
#endif

#if _k2_
#define _k2
#else
#define _k2	#
#endif

#if _ld_
#define _ld
#else
#define _ld	#
#endif

#if _lx_
#define _lx
#else
#define _lx	#
#endif

#if _mx_
#define _mx
#else
#define _mx	#
#endif

#if _pa_
#define _pa
#else
#define _pa	#
#endif

#if _pd_
#define _pd
#else
#define _pd	#
#endif

#if _sb_
#define _sb
#else
#define _sb	#
#endif

#if _sh_
#define _sh
#else
#define _sh	#
#endif

#if _ti_
#define _ti
#else
#define _ti	#
#endif

#if _to_
#define _to
#else
#define _to	#
#endif

#if _tq_
#define _tq
#else
#define _tq	#
#endif

#if _rv_
#define _rv
#define rv_	#
#else
#define _rv	#
#define rv_
#endif

#if _rx_
#define _rx
#else
#define _rx	#
#endif

#if _sq_
#define _sq
#else
#define _sq	#
#endif

#if _tm_
#define _tm
#else
#define _tm	#
#endif

#if _tu_
#define _tu
#else
#define _tu	#
#endif

#if _nv_
#define _nv
#define nv_	#
#else
#define _nv	#
#define nv_
#endif

#if _dx_
#define _dx
#else
#define _dx	#
#endif

// asm cpl vari which require others:

#undef _gs
#if (_gs_&_dx_)
#define _gs
#else
#define _gs	#
#endif

#if (_rx_|_dx_|_sb_)
#define _rdx
#else
#define _rdx	#
#endif

//;
#endif /* FIG4TH_H */
// =========================================================================== //
// -eof-
