//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
//; =============================================================================
//; fig4th.inc - cdt cons, word linkage & syscall macroes
//; =============================================================================
//;	( text formatted for 132 chars display width )
//; -----------------------------------------------------------------------------
//;
//;
//; F4 execution:
//;
//;	fig4th				interactive mode
//;
//;		no. & size of initially passed commandline args 
//;		is not limited, tib range extended as required:
//;	fig4th here dup . hex .		w. executeable commandline args
//;	fig4th here . -- 1 2 3 4	followed by in-active ones, after '--'
//;
//;	fig4th < file1 [ > file2 ]	redirected i/o
//;
//;	echo "${inputstring)"|fig4th	piped. startup message, 'prompt' and <cr>
//;					after "expect" suppressed, for scripting:
//;	vari=$(echo "123 hex 456 * .\" 0x\" . cr bye"|./fig4th)
//;					alternatively, w. executeable arguments:
//;	vari=$(./f4t 234 567 + . cr bye)
//;
//;	<bash> command line, filtering printable chars into outfile, 76 chars/line:
//;	cat infile | f4a ": x 0 begin ?terminal 0= -exit key key>char \
//;			  if emit 1+ dup 76 mod 0= if drop 0 cr endif \
//;			  else drop endif again ; x bye" > outfile
//;
//;	#! /usr/local/bin/f4a		1st (top) line in an executeable script,
//;					initial executeable arguments may follow:
//;	#! /usr/local/bin/f4a forth-words
//;
//;	piped i/o might require preservation of by a secondary program changed  
//;	terminal state which normally would be restored on F4 termination to the
//;	F4 initial state. this can be disabled, storeing -1 to "(vtr)" variable.
//;		F4prog | less
//;	where F4 would be terminated while 'less' is still active, which probably 
//;	will be corrupting the 'less' display if F4 terminal reset not disabled
//;	(in this particular case, {rs} at program beginning fixed the problem).
//;
//;	re '--' for further specification
//;
//;
//; Documentation
//;
//;	by 'glossary' and the HELP word in F4 which, expects the below mentioned 
//;	comment header until, at least, the stack flow notation, inclusively, 
//;	after a leading empty comment line, by "//;" in source text (fig4th.S).
//;	description or hi-level screen-file source of words which troughout this 
//;	text apppear in braces, "{" ... "}", can be displayed with {help} which,
//;	for instance, can be loaded from <f4x.scr> with { lload help }.
//;
//; words' comments:
//;	name	( in -- out )			(type)	(voc)
//; with data-stack flow by ( data in -- data out ) or, specifically
//;	(D: .. ) wrt data-stack, (R: .. ) return-stack
//;	(C: .. ) data-stack while compiling, (X: .. ) while executing
//; (voc):
//;	the vocabulary in which the rsp. word is defined, if not "forth".
//; (type):
//;	(c)	use while compiling, only
//;	(d)	deferred by disp wrt variables' memory base address
//;	(h)	header-less, cfa present, hi-level defn may be added
//;	(i)	immediate
//;	(k)	kernel defns, may also be def'd for 4th access
//;	(l)	use while loading, only
//;	(p)	lo-level word ('primitive')
//;	(s)	word using a Linux system call
//;	(u)	uvari-deferred by disp wrt address of uvari '(up)'.
//;	(v)	vocabulary
//;	(x)	use while executing/interpreting, only
//;	(...)	other definition types, e.g:
//;			(bcons)	unmodifiable constant by byte in kernel memory
//;			(const)	unmodifiable constant by cell in kernel memory
//;			(mcons)	modifiable constant by disp to memory @(vb)
//;			(pcons)	unmodifiable constant which adds itself to @tos
//;			(ucons)	modifiable constant by disp into user area.
//;
//; primitives, lo-level words:
//;	by jump into executeable cpu code the address of which is stored in the 
//;	rsp. word's cfa, terminated by jump to <nextloc>.
//;
//; forth, hi-level words:
//;	entry into <docolon> etc, feching the address of a list of rsp. cfa-s 
//;	which point to executeable code - F4 is 'indirectly threaded'.
//;
//; characters' representation based on indexed tables of 256 items:
//;	Words will be found letter-case independently if not specified differently 
//;	w. case-dependency by "lc-depend" or, back to "lc-ingnore". iso8859 ranges 
//;	apply by default and, may be set to any two ranges in uvari "(ascii)" for 
//;	printing characters and, the corresponding 256 bits array at "(uclc)" with 
//;	bits set at the rsp. positions of letter case convertible codes. Upper and 
//;	lower letter case character conversion uses another array of 256 bits with 
//;	bits set which correspond to convertible lower case characters. that table 
//;	should be re-placed, accordingly, at { (uclc) 32 + }.
//;
//; -----------------------------------------------------------------------------
//;
//; optimizing compiling
//;
//;	several standard compiling words which are immediately executing check 
//;	the precedingly compiled code for certain words which can be converted 
//;	to an appropriate, combined operation:
//;
//;		0= IF, WHILE, UNTIL	->	-branch
//;		0< IF, WHILE, UNTIL	->	+bra
//;		0< 0= IF, WHILE, UNTIL	->	-bra
//;		0= -EXIT		->	+;s
//;		EXIT ELSE		->	;s	dispensing w. branch to
//;		DROP EXIT ELSE		->	drop;s	  after next regular endif
//;		LITERAL LITERAL		->	dlit
//;		LITERAL +		->	[+]
//;		colondefn ;		->	branch into colondefn,
//;						also resolves end-recursion
//;	each of those will be combined with ;S at a words end or, before EXIT:
//;		DROP, 2DROP, 4DROP, SDROP, 2SDROP, SWAP, 2SWAP, RDROP, 2RDROP, R>, 2R>
//;
//;	the user variable {cmc} counts compiled cfa-s and, if not > 0 inhibits 
//;	code compacting, which can also globally be disabled w. { 0 is cmo } or
//;	re/enabled w. { -1 is cmo }. {cmc} setting \< 0 can be used to temporary 
//;	disabling for the given, -ve number of compiled words.
//;	{cmc} implicitely zeroed by program flow control words and {literal}, 
//;	{dliteral}, {compile}, {[compile]}, {is}, {'}.
//;
//; asm labelling:
//;	C<name>		subroutine, 4th-regs preserved,
//;				coresponding X<name> exists for hi-level access.
//;	L<name>		4th word-header labels by macro <c> ('create').
//;	X<name>		xec entry to forth primitive w. returnn to <nextloc>.
//;	c<name>		subroutine, 4th-regs preserved; kernel routine.
//;	x<name>		xec entry to forth primitive; kernel routine.
//;	<name>		cfa of 4th word-header.
//;	<name>4		labels the name of which would conflict w. a mnemonic &c.
//;	<name>o		vari in '+origin' area.
//;	<name>s		displacements, by AS <.struct 0> abs. reference.
//;	<name>u		user variables, copied from '+origin'.
//;	<name>v		other variables, execution vectors.
//;	<numeric>	AS' re-definable, directionally referrable ('local') labels.
//;
//;
//; =============================================================================
//;
//; pre-set channels
//;	stdin  0	standard input (expect, key, ?terminal, etc)
//;	stdout 1	standard output (type, emit, etc)
//;	stderr 2	standard error output 
//;	workch 3	for user code, e.g. as an output or work file 
//;	auxch  4	for user code, e.g. as an input or update file
//;	prtch  5	2nd-ary output device, printer &c
//;	dr0ch  chans-2	dr0 (r/w, etc)
//;	dr1ch  chans-1	dr1 (ditto, plus save' and chain')	
//;	kbdch  chans	keyboard or terminal i/o, rsp
	.struct 0
stdin:	.byte 0
stdout:	.byte 0
stderr:	.byte 0
workch:	.byte 0
auxch:	.byte 0
prtch:	.byte 0
	.rept chans-3-prtch
	.byte 0
	.endr
dr0ch:	.byte 0
dr1ch:	.byte 0
kbdch:	.byte 0
//;
//; redirected channels' table (per 8-bit bytes) at "chp"
//;	0	 0	standard input (expect, key, ?terminal, etc)
//;	1	 1	standard output (type, emit, etc)
//;	2	 2	standard error output
//;	3	 3	for user code, e.g. as an output or work file
//;	4	 4	for user code
//;	5	 5	for 2nd-ary output device, printer &c
//;	   [6,chmax-3]	not redirectable, for use by application
//;	6	chmax	keyboard channel, main console /dev/tty
//;	7	chmax-2	dr0 (r/w &c)
//;	8	chmax-1	dr1 (r/w &c, plus save' and chain')
	.struct 0
rstdin:	.byte 0
rstdout:.byte 0
rstderr:.byte 0
rworkch:.byte 0
rauxch:	.byte 0
rprtch:	.byte 0
rkbdch:	.byte 0
rdr0ch:	.byte 0	//; at index 7, else adjust "rbchan", accordingly
rdr1ch:	.byte 0
rchans:
//;
//; cdt		( ch -- a )
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
//; chpt	 4	(reserved for ptr to extension field)
//; tyfl	 8	type flags, by f4, and flag whether mmap-ed
//; chnd	12	2nd-ary handler (un-used)
//; mdfl	16	open mode, file acces
//; perm	20	file permissions
//; rnum	24	latest op error code
//; fpos	28,32	current file pointer
//; ibuf	36	single char input buffer
//; fsiz	40,44	file size
//; fctl	48	by iomode test:	i/o flags from fcntl 
//; 			if bmapc@tyfl:	mode|prot
//; cmds	52	if bmapc@tyfl: disp into file in units of PAGE_SIZE
//;			console channel: received vt ctrl sequence (un-used)
//; cmap	56	if bmapc@tyfl: mmapped base address
//; cmsz	60	if bmapc@tyfl: mmapped size
	.struct 0
.cdt:
// ;NOTE: F4 code depends on <chid> at 1st cdt item!
chid:	.long 0
chpt:	.long 0
tyfl:	.long 0
chnd:	.long 0
mdfl:	.long 0
perm:	.long 0
rnum:	.long 0
fpos:	.long 0,0
ibuf:	.long 0
fsiz:	.long 0,0
fctl:	.long 0
cmds:	.long 0
cmap:	.long 0
cmsz:	.long 0
// ; 64: -- dft cdt ends -- 64 bytes for windowing extension: --
.ifgt cdtsizeb-6
// ; origin top,left, width, height, disp to i/o posn from left, top
xmin:	.long 0
ymin:	.long 0
xsiz:	.long 0
ysiz:	.long 0
xpos:	.long 0
ypos:	.long 0
// ; text & 'pen' attr., cursor flags, <nl>-status, 'fill' on/off
attr:	.byte 0
curf:	.byte 0
nlst:	.byte 0
fmod:	.byte 0
// ; colours: 'paper', 'strip', 'ink', 'border'
colr:
pcol:	.byte 0
scol:	.byte 0
icol:	.byte 0
bcol:	.byte 0
// ; graphic parameters by double cell rational 'floats':
// ; drawing angle, disp from top left pixel addr to graphic origin, scale
gang:	.long 0,0
gxrg:	.long 0,0
gyrg:	.long 0,0
gsca:	.long 0,0
.endif
// ; ------------
//;
//; stdioblk v,stdin,stdout,stderr,workch,auxch,prtch,kbdch,dr0ch,dr1ch
//; channels redirection table, referred by disp to <stdiov>:
//;	chpv:	.byte	0,1,2,3,4,5,16,chans-2,chans-1
.macro stdioblk o,dr0=0,dr1=0,si=0,so=0,se=0,sw=0,su=0,pt=0,kb=0
chp\o:	.byte	\si,\so,\se,\sw,\su,\pt,\kb
cdr0\o:	.byte	\dr0,\dr1
.endm
//;
	.struct 0
	stdioblk s
//;
//; sys_ r0=eax,r1=ebx,r2=ecx,r3=edx,r4=esi,r5=edi,r6=ebp
//;	cpl syscall by r0=name w.o. '__NR_' praefix
.macro sys_ r0=eax,r1=ebx,r2=ecx,r3=edx,r4=esi,r5=edi,r6=ebp
.list
.ifnc \r0,eax;.ifnc \r0,%eax
    .ifgt (__NR_\r0-127)
	movl $__NR_\r0,eax
    .else
	pushl $__NR_\r0
	popl eax
.endif;.endif;.endif
	int $0x80
.nolist
.endm
//;
//; macro <chanid rs rd>
//;	sto channel-id from cdt by <rs> to reg <rd>, eflags preserved
.macro chanid rs,rd
	movl (\rs),\rd
.endm
//;
//; macro <tochanid rs rd>
//;	sto channel-id @reg <rs> to cdt by reg <rd>, eflags preserved
.macro tochanid rs,rd
	movl \rs,(\rd)
.endm
//;
//; macro <relcdt rs rd>
//;	sto ctd address of reloc. channel by name <rs> to reg <rd>
//;	modifies eflags
.macro relcdt rs,rd
	movzbl \rs+chpv,\rd
	shll $cdtsizeb,\rd
	addl $cdtv,\rd
.endm
//;
//; -----------------------------------------------------------------------------
//;
//; F4 word header:
//; Non-standard, due to Linux/ELF and because AS was not found capable of setting
//; up a standard fig-type Forth name - and because I'd consider typing in those
//; count-bytes and flags and chars|128, manually, most stupid...
//; The single one remaining problem with AS is the missing assembler variables 
//; which, for the Forth words' and vocabularies' linked lists was circumvented 
//; with the directionally referrable numeric labels, pointing back to the rsp. 
//; equally numbered one, before setting up the current label - re macro 'c' &c.
//;
//; The AS assembler and the praeprocessing 'cpp' part of 'gcc' were chosen because of
//; known reliability and their common availability in any Linux system. And, I'm fed
//; up with those many unfinished, carelessly compiled 'advanced' tools, thus I do not 
//; care for yet another assembler, &c!
//;
//; Thus, F4 provides a different & faster to scan, by one byte extended structure
//; plus, pc-relative pointers for position independent items, simplifying relocation
//; required for saved Linux/ELF binaries:
//;	nfa: [ flags+count.b ][ name ][-ve count.b]
//;		adjusted for lfa at dword(4 bytes) aligned address.
//;	lfa: [ -ve disp to prev. link ]
//;		pc-relative words linkage by disp to preceding lfa; e.g. by {+@}.
//;	cfa: [ wa ]
//;		address of executeable code which determines use of data in pf.
//;		
//;	pfa: [ data... ... ]
//;
//;	cfa		code-field address
//;	ca, wa		code address, word addr: the code entry of the rsp word
//;	lfa		link-field address
//;	nfa		name field address
//;		flags+count byte bits (if set):
//;		#7:	unsmudged	word marked "-find"able
//;		#6:	immediate	word always executing, a compiler word
//;		#5:			(till un-used)
//;		#4..#0	count		name length.
//;	pfa		parameter-field address
//;
//; NOTE that the entire dictionary space will be write protected when executing an F4 
//; program which was generated w. {save'}. re 'vmem' (variables mem) for writable data.
//;
//; vocabularies linkage by {voc-link}, beginning w. {voc-link}, by pc-relative pointers, 
//; 	1st in chain at { voc-link +@ }, next at { voc-link +@ +@ } &c, until zero result.
//;
//; "vocabulary" 2nd-ary word header ref'd by pointer @pfa, i.e.
//; the word's pfa contains an index to the vocabulay definition block in writable memory
//; (currently implemented by pc-rel disp to its true address, not yet feasible w. {save'}).
//;	pfa:	[disp wrt vmem vocabulary defn block ] ptr to vocabulary pseudo-header:
//;		[ -1 `\=> | $81,$20,-1] nfa	vocabulary base nfa
//;		[ voc-top  ] 		lfa	pseudo-lfa, w. voc's 'latest' nfa
//;		[ voc-link ]		cfa	pc-rel vocabulary linkage, 'voc-link'
//;		[ 2nd-voc  ]		pfa	disp wrt vmem to 2nd-ary search voc lfa.
//;		[ voc-flg  ]			several bit-flags, for '-find' &c
//;		[ backlink ]			this voc-block's word-header lfa.
//;	note: standard pseudo name $81,A0 would equally be useable but, for functional 
//;	consistency was replaced w. the word the name of which in F4 is the blank space.
//;
//; new vocabularies, which are not pre-defined in the kernel, compile an additional short
//; header-block into dictionary space as [dummyname][lfa][cfa], to terminating the rsp 
//; word-list independently of the variables memory position, which enables vmem remapping 
//; to a different base address. 
//; initial, 'bottom' nfa of a new vocabulary is the voc-block nfa in dictionary space.
//;
//; I.e. in Forth words,
//;	after { latest-voc definitions forth } :
//;		{ ' latest-voc +@ pfa lfa } = {   context  @ }
//;		{ ' latest-voc +@ pfa cfa } = {   voc-link +@ }
//;	after { 2nd-voc hidden forth hidden definitions forth }:
//;		{ ' forth      +@ pfa lfa } = { ' hidden  +@ pfa @ }
//;
//;
//; macro <link\voc>
//;	hilfsweise in <c>reate zur vocabularbezogenen wortverkettung.
//;	weil in AS durch die vollverbloedeten `entwickler` desselben per behauptung - als
//;	offenbar der zeitgemaeszen programmierungsmethode - zwar die ausstattung des GASP
//;	integriert wurde, sodasz dieser grauenhafte mist angeblich `obsolet` wurde, aber 
//;	nichts von alldem funktioniert, brauchts fuer jedes voc eine eigenes macro fuer die
//;	wortverkettung:
//;		beim ersten antreffen pc-rel ref auf linkfeld des btr. vocabulars eingetragen,
//;		danach ref auf vorgaenger-label rel. zur eigenen link-label position,
//;		link-label 1000[1-9] (letzte ziffer per vocabular) _nach_ lfa des betr. wortes.
.macro linkmtyv;.ifndef LU0;LU0:	;.long lfmtyv-.		;.else;.long 10000b-10000f;.endif;10000:;.endm
.macro linkforth;.ifndef LF0;LF0:	;.long lfforth-.	;.else;.long 10001b-10001f;.endif;10001:;.endm
.macro linkroot;.ifndef LR0;LR0:	;.long lfroot-.		;.else;.long 10002b-10002f;.endif;10002:;.endm
.macro linkhidden;.ifndef LH0;LH0:	;.long lfhidden-.	;.else;.long 10003b-10003f;.endif;10003:;.endm
.macro linklinux;.ifndef LX0;LX0:	;.long lflinux-.	;.else;.long 10004b-10004f;.endif;10004:;.endm
.macro linkeditor;.ifndef LD0;LD0:	;.long lfeditor-.	;.else;.long 10005b-10005f;.endif;10005:;.endm
.macro linkassembler;.ifndef LA0;LA0:	;.long lfassembler-.	;.else;.long 10006b-10006f;.endif;10006:;.endm
.macro linklocal;.ifndef LO0;LO0:	;.long lflocal-.	;.else;.long 10009b-10009f;.endif;10009:;.endm
// spielt nicht:
////#define linkmtyv	.ifndef LU0;LU0:;.long lfmtyv-.		;.else;.long 10000b-10000f;.endif;10000:;.endm
////#define linkforth	.ifndef LF0;LF0:;.long lfforth-.	;.else;.long 10001b-10001f;.endif;10001:
////#define linkroot	.ifndef LR0;LR0:;.long lfroot-.		;.else;.long 10002b-10002f;.endif;10002:
////#define linkhidden	.ifndef LH0;LH0:;.long lfhidden-.	;.else;.long 10003b-10003f;.endif;10003:
////#define linklinux	.ifndef LX0;LX0:;.long lflinux-.	;.else;.long 10004b-10004f;.endif;10004:
////#define linkeditor	.ifndef LD0;LD0:;.long lfeditor-.	;.else;.long 10005b-10005f;.endif;10005:
////#define linkassembler	.ifndef LA0;LA0:;.long lfassembler-.	;.else;.long 10006b-10006f;.endif;10006:
////#define linklocal	.ifndef LO0;LO0:;.long lflocal-.	;.else;.long 10009b-10009f;.endif;10009:
// ; ---------------------------------------------------------------------------
//;
//; create forth word header field:
//; <c label "name" cfa flags(dft=0) arg1(dft="nix") vocabulary(dft="forth") >
//;	non-standard header because neither, CPP nor AS, supplies any useable 
//;	string operations, e.g. can't OR a char code of a string to binary data...
//;		[ nfa..] count+flags byte, name
//;		[ lfa ]	 -ve pc-rel link to previous header
//;		[ cfa ]	 address of rsp word' lo-level entry, this word's wa
//;		[ pfa..] colon defn wa list, data specific to the particular word
.macro cnfa l="label missing",i=0,m="nix",v=forth
	.fill ((4-(((LE\l-LN\l-1)+2+.-Z)&3))&3),1,-1
.set ntop,.
.set ntop\v,.
LN\l:
  	.byte (LE\l-1-.)|(\i&-32)|smud
.endm
//
.macro clfa l,x,i=0,m="nix",v=forth
LE\l:
	.byte -(LE\l-LN\l-1)
.set ltop,.
.set ltop\v,.
LL\l:
	link\v
\l:	.long \x
    .equ prev\v,LL\l
.endm
// ;lab=label t="name" xaddr=xec-entry im=0(header-flags) m=nix(un-used) v=forth(wordlist)
// ;    <label>		cfa			<- list items pointed to by IP
// ;    LN<label>	nfa			for xntop, wordlist linkage
// ;    L<label>	lfa	4-aligned	.eq thislab
.macro c lab,t,xaddr,im=0,m="nix",v=forth
    cnfa \lab,\im,\m,\v
    .ascii "\t" //
    clfa \lab,\xaddr,\im,\m,\v
.endm
//;
//; macro <dbh n>
//;	dummy name char "n", for debugging w. 'see' if _dh_ =/= 0
.macro dbn n
.ifne _dh_
	.byte 0,1
	.ascii "\n"
	.byte -1
	.long -1
.endif
.endm
//;
//;
//;
//; <vocblock vocname,2nd-ary-voc,flags> macro,
//;	asm labels: nf(name),lf(name),vl(name),sd(name),fl(name).
//;	basic vocabulary linkage block - at 4-aligned address in writable memory
//;	    b[-1] vocnfa:b[$81,$20,-1] d[voctop] d[voclink] d[2nd-ary] d[flags] d[back-lfa].
//;	   pseudo header: ^- nfa        ^- lfa    ^- cfa     ^- pfa     ^- wa1   ^- wa2
//;	context stores pseudo-lfa of -^ voc-header, ^- voc-link chain at pseudo-cfa.
//;	back-link such that { ' vocname dup lfa swap @ >vmem pfa 8+ @ = } is true
//;	with                    ^- word-pfa                     ^- voc-pfa
NN = 0
.macro vocblock o=s,sd=0,fl=0,pv=20002b-20002f
vh\o:	.byte -1	//;		alignment byte
nf\o:	.word vnam	//;vnforth nfa:	2nd-ary nfa, dummy name terminates wordlist
	.byte -1
lf\o:	.long ntop\o	//;lfforth lfa:	'wid' to context/current; addr of top nfa within voc, init w. own voc-nfa
vl\o:	.long \pv	//;vlforth cfa:	link to prev. voc wrt this posn, 'voc-link'
20002:
.ifnc "\sd","0"
sd\o:	.long lf\sd-D	//;sdforth pfa:	disp wrt mem-base to voc-lf of 2nd-ary search voc
.else
sd\o:	.long 0
.endif
fl\o:	.long \fl	//;flforth	voc flags
vx\o:	.long \o-lfa2cfa//;		this voc's 4th word-header lfa
.equ prevvoc,vl\o
.set NN\o,NN
NN = NN+1
.ifndef ntop\o
.set ntop\o,LN\o
.endif
.endm
	.struct 0
vhs:	.byte 0
nfs:	.byte 0,0,0
lfs:	.long 0
vls:	.long 0
sds:	.long 0
fls:	.long 0
vxs:	.long 0
.ifne _dh_
vns:
.endif
.equ lf0,0
//;
//; channel definition tables
.macro cdtblock o,s=0
cdt\o:
.nolist
	.rept chans+1
	.rept 6;.long 0;.endr;.rept cdtsize/4-6;.long -1;.endr
	.endr
.list
.endm
//;
//; -----------------------------------------------------------------------------
//;
//; sdatac [string=],[byte=-1]
//;	compiles byte-counted <string>, appends byte if <byte> >/ 0
.macro sdatac s=,b=-1
	.byte L\@-.-1
	.ascii "\s"
.ifge \b
.ifeq \b
L\@:
.endif
	.byte \b
.ifne \b
L\@:
.endif
.else
L\@:
.endif
.endm
//;
// =========================================================================== //
// -eof-
