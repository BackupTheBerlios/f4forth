//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; fileio.S 
// =============================================================================
.psize 0
.att_syntax noprefix
.text
//;
//; ----------------------------------------------------- //
//; ---- i/o ----
//;
.list
//; ---- stdin/stdout
// ( -- f ) ; tf if stdout is a terminal or, F4 not in scripting mode
Xoconsi:pushl $rstdout//<interpret)
	spush
	popl TS
// ( rch -- f ) ; 'ch-cons'
Xchcons:movb chpv(TS),TSl//;edx:=relchan cdt
	call ccdtp
	xorl TS,TS
	testb $scrix,uref(bitsu)
	jnz 1f		//;?;scripting mode
	testl $consflags,cdtref(tyfl)(edx)
	jz 1f		//;?;stdout not a tty
	incl TS
1:	NX
// ( chan -- flg ) ; cons? NZ*C if channel is a console channel
Xconsq:	pushl NXL
Cconsq:	pushl ebx	//<Xopen); test system channel
	call bchid
	jmp cconsn
// ( -- flg ) ; io-cons
Xiocons:pushl NXL
Ciocons:call cincons	//<Xiniq)
	pushl $rstdout
	spush
	popl TS
	call crconsq
	andl TS,TOS(1)
	TOSPULL(1)
	ret
// ( -- f )
cincons:pushl $rstdin	//<Ciocons)
	spush
	popl TS
// ( rch -- f ) ; out: edx=cdt, NC if not a terminal channel
crconsq:pushl ebx	//<cincons,Ciocons)
	call brchid	//; test reloc ch
cconsn:	js 1f		//<Cconsq)
	pushl $1
	pushl edx
	orl $setconsf,cdtref(tyfl)(edx)//;consflags
	movl $TIOCGSID,ecx
	movl PS,edx
	sys_ ioctl	//;test whether console
	cmpl $maxerr,TS
	popl edx
	popl TS
	jc 2f		//;?; flg:=1 on success
	andl $~setconsf,cdtref(tyfl)(edx)//;consflags
1:	xorl TS,TS	//; NC & 0 if not a terminal channel
2:	popl ebx
	ret
// ( a -- ) ; type in reversed video, by 'id.'
crevt:	leal jctype,edx	//<crevwp)
// ( xx -- xx ) ; xec @edx w. reversed video
crevx:	pushl $crevo
	pushl edx
creve:	spush		//<Xsee)
	leal vtrevt,TS
	jmp cvtseq	//;( -- a v
// ( n -- ) ; <cr> if n = true flg
Xdashcr:testl TS,TS
	spop
	jnz Xcr	//;?; <cr>
	NX
// ( -- ) ; rev video off
crevo:	spush		//<Xsee)
	leal vtroft,TS
// ( a -- ) ; send vt sequence, max 2 cells = 8 chars
cvtseq:	movl TS,ecx
	movl (ecx),TS
	cmpb $ESC+1,TSl
	jc cvemit	//;don't count if 1st byte is a ctrl code
	spush
	xchg TS,ecx
	call cbytes	//; ecx:=ecx, edx=l.s. non-zero bytes in ecx
	pushl uref(outu)
	subl edx,uref(outu)//; count trailing bytes
	movl 4(TS),TS	//;fetch next, send 1st
	call cvemit
	testl $consflags,cdtref(tyfl)(edx)
	popl edx
	jnz cvemit	//;?;
	movl edx,uref(outu)//; rst @out if nothing sent
cvemit:	relcdt rstdout,edx//;fetch reloc cdt of stdout to reg ecx
	testl $consflags,cdtref(tyfl)(edx)
	jnz Cemit	//; bytes to stdout (not the deferred word!)
	spop
	ret
// ( n1 -- n2 ) ; open-flag modifier
Xlfi:	orl $O_LARGEFILE,TS
	NX
// ( n1 -- n2 ) ; open-flag modifier
Xapp:	orl $O_APPEND,TS
	NX
// ( n1 -- n2 ) ; open-flag modifier
Xcre:	orl $O_CREAT|O_TRUNC,TS
	NX
// ( n1 -- n2 ) ; open-flag modifier
Xdir:	orl $O_DIRECTORY,TS
3:	NX	//<Xbokb)
// ( -- ) ; g0 charset
Xg0:	pushl NXL
Cg0:	pushl $SI
	jmp 1f
// ( -- )
Xbel:	pushl NXL
Cbel:	pushl $BEL
	jmp 1f
// ( -- )
Xbs:	pushl NXL
Cbs:	pushl $BS
	jmp 1f
// ( -- )
Xtab:	pushl NXL
Ctab:	pushl $TAB
	jmp 1f
// ( -- )
Xcr:	pushl NXL	//<Xiniq,Xtriad)
Ccr:	pushl $NL
	jmp 1f
// ( -- )
Xbokb:	relcdt rstdout,edx
	cmpl $consflags,(edx)//; <chid>
	jz 3b		//;?; not at tty, j NX
	pushl $'<'
	spush
	popl TS
	cmpl $0,uref(stateu)
	jnz 4f		//;?;compiling
	incl TS
4:	testb $edins,uref(bitsu)
	jz 2f		//;?;edit overwriting
	subl $26,TS	//; "= => "#, "> => "$
2:	call cdemit
// ( -- )
Xspace:	pushl NXL
Cspace:	pushl $BL
1:	spush
	popl TS
cdemit:	leal emit+4,W
	jmp Cudfer
// ( -- )
Xsi:	pushl NXL
	pushl $SI
	jmp 1b
// ( -- )
Xso:	pushl NXL
	pushl $SO
	jmp 1b
// ( n c -- ) ; emit n chars c
1:	spush
	call cdemit	//; call vector
cdemitm:decl TOS(1)
	jns 1b
	SPOP(2)
	ret
// ( n -- )
Xspaces:pushl NXL
Cspaces:pushl $BL
	spush
	popl TS
	jmp Cemitm
// ( n c -- ) ; emit n chars c
Xemitm:	pushl NXL
Cemitm:	movzbl TSl,ecx	//; c
	movl TOS(1),edx	//; n
	cmpl TS,ecx
	jne Cemitm	//;?; multi-byte char
	movl edx,ecx
	sarl $2,edx
	js cdemitm	//;?; exit if -ve count passed
	movl edx,TOS(1)	//; n/4
	movzbl al,edx
	andl $3,ecx	//; n&3
	movb al,ah
	movb al,dh
	shll $16,TS
	orl edx,TS
	shll $3,ecx	//;bytes
	jz cdemitm
	subl $32,ecx
	negl ecx
	spush
	shrl cl,TS
	call cdemit
	jmp cdemitm
// ( char -- )
Xemit:	pushl NXL
Cemit:	pushl ebx	//<vtqp)
	pushl ecx
	movl TS,ecx
	movb $rstdout,TSl//;rel stdout
	call brchid
	js 4f
cemitw:	pushl edx	//<expemit,wfemit); cdt
	movl ecx,cdtref(rnum)(edx)// sto char to temp buf
	call cbytes	//;edx := count
	addl edx,uref(outu)
	cmpb $DEL,cl
	jz 6f		//;-1
	cmpb $BL,cl
	jnc 6f		//;printing char
	subl edx,uref(outu)
6:	movl (esp),ecx	//;cdt
	leal cdtref(rnum)(ecx),ecx //;temp buf
	sys_ write	//;emit the buffered char's bytes
	popl edx	//;cdt
	call fposadd
4:	popl ecx	//<wfemit)
	popl ebx
	spop
	ret
//
expecr:	pushl $NL	//<expect)
//-???-//-.
// ;relcdt rstdin,edx//;fetch reloc cdt of stdout to reg ecx
	relcdt rstdout,edx//;fetch reloc cdt of stdout to reg ecx
//-???-//-^
	movl (esp),TS
	movl ebx,(esp)
	chanid edx,ebx
	jmp 1f		//>expemit)
//
expemit:pushl ebx	//<expect)
1:	pushl ecx	//<expecr)
	movl TS,ecx
	spush
	testl $mcons,cdtref(tyfl)(edx)
	jnz cemitw	//;echoing stdin
	jmp 4b
//
wfemit:	pushl ebx
	pushl ecx
	movl TOS(1),ecx	//; char
	jmp cemitw
// ;i: ecx=char(s) o: ecx=ecx, edx := no. of significant (non-zero) l.s. bytes
cbytes:	//<Xbcount,cemitw,cvtseq,Xbstore,Cfread)
	movl ecx,edx	//;byte-cnt or zero
	bsrl ecx,edx	//;find top bit to send
	addl $7,edx	//;round up to 8bit-byte size
	shrl $3,edx	//;bytewise
	jnz 1f		//;?; non-zero cnt
	incl edx	//;at least one byte
1:	ret
// ( a u -- ); uvector
Xtype:	pushl NXL
Ctype:	movzwl TSx,edx	//;count
	cmpl TS,edx	//;limit to 64K
	spop
	jnz 2f
	testl edx,edx
	jng 2f
	testl TS,TS
	jz 2f		//; 'noop' if <zero> address
	pushl ebx
	pushl $rstdout
	movl TS,ecx	//;adr
	popl TS		//;stdout
	addl edx,uref(outu)
	pushl edx	//;count &c
	call brchid	//;i: al=rel-ch o: ebx:=fd edx:=chp
	js 1f		//;no channel
	pushl edx	//;cdt
	movl 4(esp),edx	//;passed, 3rd arg
	sys_ write	//;'type'...
	popl edx	//;cdt
	call fposadd	//;updt file-ptr
1:	leal 4(esp),esp	//;drop 3rd arg
	popl ebx
2:	spop		//;drop ernum
	ret
// ;update ernum & 32bit fileptr i: TS er/count edx:cdt eflags:C else, NC on error
fposadd:movl TS,cdtref(rnum)(edx)//<;cemitw,Ctype,creadf)
	cmpl $maxerr,TS
	jnc 3f
	testl $consflags,cdtref(tyfl)(edx)
	jnz 4f
	pushl ecx
	movl TS,ecx
	sarl $31,ecx
	addl TS,cdtref(fpos)(edx)
	adcl ecx,4+cdtref(fpos)(edx)
	popl ecx
4:	neg edx		//;force C
	neg edx
3:	ret
// ( n -- )
Xquery:	pushl NXL
	movl $0,uref(inu)
	btrl $xargsb,uref(bitsu)
	jc 3b		//;?;xec initial args already in @tib
	stosl
	movl uref(tibu),TS
	spush
	movl $tibqi,TS
	leal expect+4,W
	jmp Cudfer
// ( a n -- ) ; buffer & default asciz text at addr a
Xedlin:	pushl edi
	pushl TS
	movl TOS(1),edi
	xorl TS,TS
	leal -1(TS),ecx
	cmpb $BL,(edi)
	jc 1f
	repne scasb
1:	notl ecx
	popl TS
	popl edi
	movl ecx,edx	//;dft stg length
	jmp 12f
// ( a n -- )
// ;sto max n bytes, until <nl>, as dword<nul>-terminated stg from terminal input to addr a.
Xexpect:xorl edx,edx
12:	movl TOS(1),ecx	//;ptr				{ .. -- a n
	decl TS		//;provide for trailing <nul>
	jng 7f		//;-ve or no count, drop a, n
	pushl ebx	//;save NXL ->
	pushl TS	//;count ->
	pushl edx	//;count of default asciz text at a ('edlin')
	movb $rstdin,al	//;rel stdin
	btrl $scrixb,uref(bitsu)
	jnc 9f		//;?;not a script-file
	movb $rdr1ch,al
9:	call brchid	//;ebx=fd, edx=cdt, ecx=ecx
	popl ecx
	popl TS		//;count <-						| ebx
	js 8f		//;?;not a valid fd or, closed and mmapped
	spush
	xorl TS,TS	//;initial charcount
	spush		//;initial curpos
	spush		//; ( a n -- a n1 n2=0 u3 c=0 ) stg, buflen, charcount, cursor, char
	decl ecx
	js 0f		//;?;zero default stg
	movl ecx,TOS(2)
	movl ecx,TOS(1)
	call vteol	//;re-display current input
	call vtcud	//;re-edit latest input
//  ;regs: ebx=fd, ecx=curs.addr, edx=cdt
//  ;PS( stg-address maxcount charcount cursor-posn char -- )
0://;fetch a byte for expect, tos:=tos, unchanged and, ebx=fd edx=cdt already set, regs preseved
	call expekey	//;get key from stdin		{ .. -- a n1 n2 u3 c 	| ebx
// ;ctrlkey: check & perform ctrl action, ret eflags & char to emit;
	call ctrlkey	//;any control action		{ .. -- a n1 n2 u3 c 	| ebx
	jc 0b		//;printing char
	js 3f		//;done (Z may also be set, thus no <jna>, above)
	jz 0b		//;nothing to emit or store
// ;expemit: copy char @tos to stdout, all regs but TS preserved;
	call expemit	//;don`t store, emit char
	jmp 0b		//;------^			{ .. -- a n1 n2 u3 xx	| ebx
3://;done
	movl TOS(1),ecx
	addl TOS(4),ecx
	TOSPULL(3)	//;				{ .. -- a n1 		| ebx
6:	popl ebx	//;nextloc
	testl $consflags,cdtref(tyfl)(edx)
7:	movl $0,(ecx)	//;terminate buf
	jz 10f		//;?;not a terminal
	movl uref(vbotu),TS
	cmpl $0,vdisp(okev)(TS)
	jz 10f		//;?; prevent <cr> if prompt vector inactive 
	call Ccr
10:	SPOP(2)
	NX
2://;channel not accessible; ecx=buf
	movb $rstdin,rstdin+chpv//;try reloc to rstdin
	jmp 6b
// ; try mmapped 'expect': eax=TS=maxcount,ecx=TOS(1)=addr, edx=dr1-cdt
8:	addl ebx,ebx
	jno 2b		//;?;not mmapped
	testl $mmapc,cdtref(tyfl)(edx)
	jz 2b		//;?;file not in memory
	bts $scrixb,uref(bitsu)//;rst scripting mode
	spush
	pushl $NL
	movzbl rdr1ch+chpv,eax
	spush
	popl TS
// ( a u c -- a u' tf | a ernum )
	movl (esp),ebx
	call Cfread
11:	movl TOS(1),ecx
	addl TS,ecx
	cmpl $0,cdtref(rnum)(edx)
	jnz 6b		//;?; continue
	xorl TS,TS	//;exit 0
9:
.ifne _cy_
    pushal
    pushfl
    movl $0x55555555,eax
    int $3
    popfl
    popal
.endif
	cmpl $-EAGAIN,cdtref(rnum)(edx)
	jz 11b		//;?;last line w/o <nl>
	jmp Xbyer	//; terminate script at <eof>
// ;1:C:fetch next 2:S:ready 3:Z:fetch next, else: char emitted, not stored, ptrs unchanged
xpem:// ;emit+sto printing char, continue or, terminate if buffer filled;
	push TS
	call Ckey2chare	//;-> NC+NS+(Z|NZ)
	jz 1f		//;discard hi range of non-printing chars, rt NC+NS+Z
	movl TOS(1),TS	//;curdisp
	movl TOS(4),ecx	//;buf
	addl TS,ecx	//;charpos
	incl TS	//;next curs disp
	cmpl TS,TOS(3)	//;max
	clc
	js 1f		//;buffer ovf, rt NC+S+NZ, terminate
	testb $edins,uref(bitsu)
	jz 3f
	call xins
3:	xchg (esp),TS
	call expemit	//;display
	movb al,(ecx)	//;char to buf
	movl (esp),TS
	pushl ebx
	movl TOS(2),ebx	//;current line length
	incl ecx	//;advance cursor posn
	cmpl ebx,TS	//;maxchar-nextcurs
	jc 2f		//;cursor within line, after <cmc> rt NC+S
	subl $-1,TOS(2)	//;inc line length, force C
2:	movl TS,TOS(1)	//;disp to curpos
	popl ebx
1:	popl TS	//;drop char; done: NC+S+x, more: C+x+x
	ret
// ------------------------------------------------------------------------------------------
// ;ctrl:  BS:cur left, TAB cur right, ^L:cls DEL:del left, <del>:del at curs, <ins> tog ovw/ins
// ;ascii: 08           09             12     127           1b,5b,33,7e        1b,5b,32,7e
// ;rxvt:  08           09             12     127           1b,5b,33,7e        1b,5b,32,7e
// ------------------------------------------------------------------------------------------
// ;ctrls: arrow left:=BS, right:curs right, up:hist+, dn:hist-, pos1:1st posn, pos2:last posn
// ;ascii: 1b,5b,44        1b,5b,43          1b,5b,41  1b,5b,42  1b,5b[31]7e    1b,5b[34]7e
// ;rxvt:  1b,5b,44        1b,5b,43          1b,5b,41  1b,5b,42  1b,5b[37]7e    1b,5b[38]7e
// ------------------------------------------------------------------------------------------
// ;returned flags precedence:
// ;1:C:fetch next 2:S:ready 3:Z:fetch next, else: char emitted, not stored, ptrs unchanged
// ------------------------------------------------------------------------------------------
// ; ( buf max count cursor char -- buf max count1 cursor1 xx )
ctrlkey:// ; o: ebx=ebx edx=edx, ecx=curpos if ret to non-emitting
	movl TOS(1),ecx	//;cursor
	addl TOS(4),ecx	//;current cursor address
	cmpl $DEL,TS
	jz cdel
	cmpl $BL,TS
	jnc xpem	//;emit & insert printing char
	jmp *cc4t(,TS,4)
// ; stdin not a console:
nvnl:	cmpb $LF,al	//;terminate at any <eol> code
	jz chnl
	cmpb $FF,al
	jz chnl
	testb al,al
	jnz xcpy	//;get next
chnl://; <nl>,<cr> terminate
	notl TS	//;store final <nul>
	testl TS,TS	//;NC+S+NZ, ready
	ret
// ; <del> char at cursor, fill gap w. trailing chars
vtdel:	call vt00
	movl TOS(2),TS	//;len
	decl TS
	cmpl TOS(1),TS	//;cur
	jc 1f		//;cur at eol
	movzbl (ecx),TS
	call expemit	//;advance curs for deletion
	incl ecx
	incl TOS(1)
// ;DEL char left from cursor, fill gap w. char at cursor and trailing chars
cdel:	cmpl $0,TOS(1);
	jng chbs	//;?; curs at sol
	spush
	movl ecx,TS	//;next char = src
	spush
	decl TS		//;new cursor = dst
	spush
	pushl TS	//;save curs addr
	movl TOS(5),TS	//;len		{ .. -- a n1 n2 u3 chr s d cnt	| cdt ebx
	subl TOS(4),TS	//;curs
	addl $2,TS	//; w. trailing <nul>
	call Ccmove
	movl vdel,TS	//;vt: curs back + del char display
	call expemit	//;( char vt -- char )
	popl ecx
	decl TOS(2)	//;adust length
	addl $-1,TOS(1)	//;set C
1:	ret
// --
// ;indexed from 1st ctrls table, cc4t
// ;PS( stg-address maxcount charcount cursor-posn char -- )
// ;1:C:fetch next 2:S:ready 3:Z:fetch next, else: char emitted, not stored, ptrs unchanged
// --
// ; arrow left
vtbs:	movl TOS(4),ecx	//;rst cursor posn
	addl TOS(1),ecx
// ;BS, curs back by 1 char
chbs:	subl $1,TOS(1)	//;dec cursor, get Cy
	jc xbel		//;?; at sol
	xorl TS,TS
	addl $BS,TS	//;force NC*NS*NZ, emit <bs>, don`t store
	ret
vbel:	call vt00
xbel:	xorl TS,TS
	movl TS,TOS(1)	//;cursor:=0
ebel:	xorl TS,TS
	addl $BEL,TS	//;NC*NS*NZ: emit signal, don`t store
	ret
// ;cursor right by 1 char
vtcur:	call vt00
ctab:	movl TOS(1),TS	//;curs
	cmpl TOS(2),TS	//;count
	jns ebel	//;at eol, bell & rt
	movzbl (ecx),TS	//;rst char
	incl ecx	//;advance cursor + emit
	addl $1,TOS(1)	//;NC*NS*NZ
	ret
chff://;form feed: clear screen+pos0+erase line if console, else emit
	xorl TS,TS
	addl clst,TS	//;NC+NS+NZ, emit + get next
	ret
cesc://;cc4t,27: handle <esc> sequence
	call expekey
	cmpl $BL+1,TS
	jnc qvt		//;1st vt char?
	cmpl $ESC,TS
	jz es2c		//;2nd esc, no other 2nd ctrl (yet)
// --
// ;1:C:fetch next ;2:S:ready ;3:Z:fetch next, else: char emitted, not stored, ptrs unchanged
// --
stoky:	orl $mcflg,cdtref(tyfl)(edx)//;flg char received
vt00:	movl TOS(4),ecx	//;rst cursor posn
	addl TOS(1),ecx
ch00:	xorl TS,TS	//;NC+NS+Z, neither emit nor copy, continue
	ret
// ;27: send next as ctrl char from (code-64), else silently discard
es2c:	call expekey	//;C from <sar>, other flags from <inc>
	cmpl $BL,TS
	jc xcpy		//; pass <esc><esc><ctrl> as <ctl>
	subl $64,TS
	movl TS,TOS(1)
	js ch00		//;discard+get next
xcpy:// ;printing char w.o. additional action
	negl TS
	negl TS		//;C+NS+NZ, emit+get next
	ret
// ;i: TS=chars, ecx=cursor addr, edx=cdt
vtchr:	call vt00	//;rst ecx := cursor addr
	call expekey
	cmp $BL,TS
	jc stoky	//;ctrl?
	ret
qvt:	cmpb $'[',al
	jnz vt00	//;?; not a csi sequence
	call vtchr	//;1st chr
	jz vt00		//;ctrl received, abandon vt chars (incl. BL), re-get ctrl
	leal csi11,ecx	//;"ABCDFH ": up,dn,rgt,lft,eol(xterm),sol(xterm)
	call qvtc
	leal csi1x,ecx	//;A<cup> B<cdn> C<cli> D<cre> [1~<ps1> [4~<end>
	jz 2f		//;sing found
vtd1:	leal csi21,ecx	//;"134782 ": 1st of 2-chars seq
	call qvtc
	jnz vt00	//;?; invalid
	pushl TS
	call vtchr	//;2nd char
	popl ecx
vtd2:	jz vt00
	movb ch,ah
	shll $8,TS	//;save disp for ctls w. common dlm
	movb ah,al
	leal csi22,ecx	//;"       ~": 2nd of 2-chars seq
	call qvtc
	jz 1f
	cmpb al,1(ecx)	//;common char
	jnz vt00
	shrl $8,TS
1:	leal csi2x,ecx	//;vt found
2:	movzbl ah,TS
	jmp *(ecx,TS)
//
qvtc:	pushl $csi21-csi11-2
	movb $-4,ah	//;check char in vt-seq
1:	addb $4,ah
	cmpb al,(ecx)
	jz 2f
	incl ecx
	decl (esp)
	jns 1b
2:	leal 4(esp),esp
	ret
vtsoe:	call expemit
vtsol:	call chbs
	cmpl $BEL,TS
	jnz vtsoe
	ret
vteoe:	call expemit	//;copy & emit @tos
vteol:	call vtcur	//;curs right one posn
	cmpl $BEL,TS
	jnz vteoe	//;?; more, else NC+NS+Z, don't emit, get next
	ret
// ; re-edit latest input
vtcud:	call vtsol
	movl TOS(3),ecx
	movl TOS(4),TS
	movl ecx,TOS(2)
	decl ecx
	movl ecx,TOS(1)	// ;max
1:	jz 3f
	pushl TS
	movzbl (TS),TS
	call expemit
	popl TS
	cmpb $BL,(TS)
	incl TS
	decl ecx
	jnc 1b
3:	incl ecx
	subl ecx,TOS(1)
	subl ecx,TOS(2)
	jmp vt00
// ; foggle overwrite/insert mode
vtins:	xor $edins,uref(bitsu)
	xor TS,TS
	ret
// ; shift chars from cursor to the right by one col, from <xpem>
xins:	pushl ecx
	pushl TS
	spush
	movl ecx,TS	//;current curs
	spush
	incl TS
	spush
	movl TOS(5),TS	//;len
	subl TOS(4),TS	//;curs
	incl TS
	call Clcmove
	movl inst,TS
	call expemit
	incl TOS(2)
	incl TOS(1)
	popl TS
	popl ecx
	ret
// ; arrow up, reserved for cmd-line history, previous entry
vtcuu:	jmp vt00
//

// -- <expect> ends --

// ( -- ch ) ; rel kbd channel
Xrkbdch:pushl NXL
crkbdch:spush		//<pqkey)
2:	movzbl rkbdch+chpv,TS//<ckbdkey)
	ret
// ( -- char ) ; from keyboard channel - not used -
1:	call 2b
	call Cw4f
Ckbdkey:call pqkey	//>
	jnz 1b
	ret
Xkbdkey:pushl NXL
	jmp Ckbdkey
// ( -- char ); read kbd-chan, out: TS:=char, eflags := NZ invalid, ebx=ebx, ecx=ecx, edx=edx
pqkey:	pushl edx	//<Ckbdkey)
	call crkbdch
	call Cqinput	//;( ch -- char flg )
	decl TS		//;Z if char received
	popl edx
	spop		//;drop flag
	ret
.ifne _ck_
// ( c -- a ) ; 'ch-key' ret address of character input buffer
Xchkey:	pushl NXL
Cchkey:	pushl ebx
	call bchid
	leal cdtref(ibuf)(edx),TS
	popl ebx
	ret
.endif
// ( -- flg ) ; tf after <esc>, else ff; wait until any other if <bl> received, else ret immediately
Xqesck:	pushl NXL
Cqesck:	pushl ecx	//<Xsee,Xvlist); o: edx:=cdt stdin
	leal qkey+4,W
	call Cudfer
	testl TS,TS
	jng 1f		//; no key, ret 0
	spop
	subl $ESC,TS
	jz 2f		//; <esc>, ret 1
3:	spop
	leal key+4,W
	call Cudfer
	subl $ESC,TS
	jz 2f		//; <esc>, ret 1
	subl $BL-ESC,TS
	jnz 3b		//; not <bl>, else ret 0
	decl TS
2:	incl TS
1:	popl ecx
	ret
// ( -- n )
Xkey:	pushl NXL
Ckey:	call Cqterminal	//<dbnextl..,Cqesck(vec))
	jg 2f		//;?;char present
1:// ;wait until received, fetch any one byte from channel TS
	chanid edx,TS
	call Cwf
	call cqtky	//;'?terminal', same channel
	jng 1b		//;?;retry
2:	movl cdtref(ibuf)(edx),TS
	andl $~(mcflg|mcrgt),cdtref(tyfl)(edx)//;clr availabilty flags
	ret
// ( -- c tf | ff ) ;if received, fetch any one byte from stdin
Xqkey:	pushl NXL
Cqkey:	call Cqterminal	//<Cqesck)
	jng 1f		//;?;no input
cqnky:	spush		//<Cqinput,creadf)
	movl cdtref(ibuf)(edx),ecx
	andl $~(mcflg|mcrgt),cdtref(tyfl)(edx)//;clr availabilty flags
	movl ecx,TOS(1)
1:	ret
// ( ch -- c tf | ff ) ;if received, fetch any one byte from supplied channel
Xqinput:pushl NXL
Cqinput:call jcqi
	jg cqnky	//;?;char received
	ret
1://<;expekey) ;fetch a byte for expect, i.e. tos:=tos and, ebx=fd edx=chp already set, regs preseved;
	chanid edx,TS
	call Cwf
expekey:call expeqt	//<Xexpect,(ctrlkey,(esc2c,vtchr))
	jng 1b		//;?;wait until <key>
3:	movl cdtref(ibuf)(edx),TS
	andl $~(mcflg|mcrgt),cdtref(tyfl)(edx)//;clr availabilty flags
	ret
jcqi:	pushl ebx	//<Cqinput)
	call bchid
	jmp 1f
// ;i: -/- o: TS=flg, no other regs, cdt: tyfl=recived, rnum=error code, ibuf=char
expeqt:	pushl ebx	//<expekey,creadf)
	jmp 1f
// ( -- flg )
Xqterminal:
	pushl NXL
// ;i: -/- o: TS=flg, tos:=tos+4, no other regs, cdt: tyfl=recived, rnum=error code, ibuf=char
Cqterminal:
	spush
cqtky:	pushl ebx	//<Ckey)
	movb $rstdin,TSl//;redir stdin
	call brchid	//;ebx:=fd edx:=cdt
1:	movl cdtref(tyfl)(edx),TS //<jcqi,expeqt)
	shrl $bcflg+1,TS//;(2+1)
	jnc 3f		//;?;no char buffered
	bt $bcrgt-bcflg-1,TS
	jnc 2f		//;?;buffered char valid
3:	pushl ecx
	pushl edx
	leal cdtref(ibuf)(edx),ecx
	shrl $bmapc-bcflg,TS
	pushl $1
	movl edx,eax	//;pass cdt to <mmapread>
	popl edx
	movl edx,(ecx)	//;clr hi bits
	jc 4f		//;?;mmapped access
	sys_ read	//;read a char if present, else leave immediately
6:	popl edx	//;cdt
	popl ecx	//;reg
	call fposadd	//; TS as error code to cdt(rnum), 0 at <eof>
	decl TS
	js 2f		//;no char
	orl $mcflg,cdtref(tyfl)(edx)//;flg char received
2:	sar $31,TS	//;-1 after eagain(-11) or error
	popl ebx
	incl TS		//;1(char available) or 0(none)
	ret
4:	call mmapread
	jmp 6b
//
//; ---- files random access

// ( ch -- flg ) ; tf if channel ch accessible, chid > 0 or $80000000|id or $bfffffff
Xchopn:	pushl ebx
	call bchid
	movl ebx,TS
	popl ebx
	addl TS,TS
	sarl $31,TS
	incl TS
	NX
// ( ch -- er ) ; "ch-rnum", ptr to ernum of system channel
Xchrnum:call ccdtp	//;edx:=chan cdt
	movl cdtref(rnum)(edx),TS
	NX
// ( -- n ) ; i: -/- o: TS=@tos=ch, edx=cdt
Xnewchan:
	pushl NXL
Cnewchan:		//<;copyarg
	pushl ecx
	pushl $chans-1-prtch-1-2//; scan range between prtch and dr0chan
	spush
	popl ecx	//; scan range between prtch and dr0chan
	lea cdtref(chid)+cdtptr(prtch+1),edx
	leal prtch+1(ecx),TS
1:	cmpl $chinval,(edx)
	jz 2f		//;?; unused cdt
	addl $cdtsize,edx
	decl ecx
	jns 1b		//;?; next cdt
	movl ecx,TS	//;ret 0 if no entry available
2:	subl ecx,TS
	popl ecx
	ret
// ( -- ch ) ; channel & cdt of current screen-file relchan
cscrcdt:call Cdrq	//<Cldfind,cinclude); ret @tos=TS=channel, edx=cdt
// ( dr -- ch ); i: TS=drive no., o: TS=TS, edx:=cdt
cscrchn:call Cbchan	//; i: TS=reloc. chan,  o: TS:=screenfilechannel
// ( ch -- ch ); i: TS=channel, o: TS=TS, edx:=cdt
ccdtp:	cmpb $chans+1,TSl//<bchid,Xchcons,Xcdt,Xchid,Xoconsi,Xchrnum)
	jnc chrng	//;?;chan no. out of range
	movzbl al,edx	//;channel
	shll $cdtsizeb,edx
	addl $cdtv,edx	//;cdt
	ret
chrng:	pushl $-ECHRNG	//;'channel number out of range'
	jmp qern
// ; i: al = drive no.; o: ebx:=fd edx:=cdt, ecx=ecx, NC+(S|NS)
bscrch:	addb $cdr0v-chpv,TSl//<Crbyw); dr0 index addr
// ; i: al = channel; o: ebx:=fd edx:=cdt, ecx=ecx, NC+(S|NS)
brchid:	call crelcn	//<winsz,Cpsysp,Crbyw,crconsq,Cemit,Ctype,Xexpect,Cqterminal)
bchid:	call ccdtp	//<Cconsq,jcqit,creadf<Cfread,Cfposre,Cfwrite,cwritef,Cfstat,Cflen,Cclose)
	chanid edx,ebx
	testl ebx,ebx	//;fd, NC provisionally, for error hnd
	ret
// ( dr -- ch ); screenfile 'drive' channel id
Xbchan:	pushl NXL
Cbchan:	addb $cdr0v-chpv,TSl//; dr0 index addr
// ;( relchan -- chan )
crelcn:	cmpb $rchans,TSl
	jnc chrng	//;?;relchan no. out of range
	movzbl TSl,edx	//<brchid,Xrelch); redirected channel`s ID
	movb chpv(edx),TSl
	ret
// ( ch -- cdt ) ;cdt from system channel
Xcdt:	call ccdtp
	movl edx,TS
	NX
// ( rch -- ch )
Xrelch:	call crelcn
	movsbl al,TS
	NX
// ( ch -- fd ) ;fd from system channel
Xchid:	pushl NXL
Cchid:	call ccdtp	//<Cmmapf,Cw4f,vtqp)
	chanid edx,TS
	ret
// ( ch -- ) ; wait until input available
Xw4f:	pushl NXL
Cw4f:	call Cchid	//<vtqp,crkbdch(Ckbdkey))
	call Cwf
	spop
	ret
// ( ch -- ch ) ;all regs preserved, no return value but eflags
Cwf:	pushal		//<Ckey,expekey,Cw4f)
	movzbl al,TS
	cmpl $32,TS
	jnc 2f		//;?;not in range [0,31]
	xorl ebx,ebx	//;0
	movl esp,esi	//;for empty rstack
	btsl TS,ebx	//;in
	pushl ebx	//;r/w/x flags
	movl esp,ecx	//;-> read-fds, also xec, write is NULL
	pushl 4+uref(timou)//;timeval.µs
	pushl uref(timou)//;        sec
	movl esp,edi	//;time (ptr=0 ret immediately; value=0 block forever until input available)
	pushl esi	//;save empty rstack
	xorl edx,edx	//;write-fds
	movl ecx,esi	//;read-fds
	leal 32(edx),ebx
	sys_ _newselect	//;wait until char input pending; b:fdmax,c:*r,d:*w,S:*x,D:*time
	movl (esp),esp
2:	popal
1:	ret
// ( a n ch c -- a n1 | a er )
// ;fetch at most n bytes from channel ch to buffer a, if c not -ve 
// ;terminate once (multi-)byte c found and ret stg w.o. delimiter.
// ;i: eax=TS=chan o: TS=count, ecx=xx, edx=dt; eflags=NC on error
Xfread:	pushl NXL
// ; readf if 2dup c scan swap drop - dup minus ch fposre
Cfread:	movl TS,ecx	//<readml(Xx))
	call cbytes
	pushl edx	//; byte-size of dlm
	pushl TS	//; >r   char/flg
	spop
	pushl TS	//; >rr  chan	( a n c 
	call creadf	//;		( a n' tf | a ernum ff
	jz 1f		//; error
	movl 4(esp),ecx	//;char
	testl ecx,ecx
	js 1f		//; no terminator
	movl TOS(2),TS	//; drop over	( a n a
	spush
	movl TOS(2),TS	//; over	( a n a n
	spush
	movl ecx,TS	//; r		( a n a n char
	call Cscan	//; a u a disp
	jz 2f		//;?;dlm not found
	subl TS,TOS(2)	//; a u1 a disp
	subl 8(esp),TS	//; exclude terminator char bytes (false w. multi-byte terminator!)
	negl TS		//; a u1 a -disp
2:	movl TS,TOS(1)
	movl TS,ecx	//;save disp
	movl (esp),TS	//;		( a n -ds ch
	call Cfposre	//; a u1 -disp ch -- a u1 fpos
	jnc 1f		//;?;error
	addl ecx,cdtref(rnum)(edx)//;adjust received/sent chars count
	negl ecx	//;set C
1:	leal 12(esp),esp//;drop [chan,flg,bytes
	spop
	ret
// ( a u c -- a u' tf | a ernum ff )
cwritef:pushl ebx	//<Cfwrite)
	call bchid
	pushl $__NR_write
	jmp 2f
// ( a u c -- a u' tf | a ernum ff )
creadf:	pushl ebx	//<vtqp,Cfread)
	call bchid	//; <bchid> edx:=chp, ebx:=fd, eflags: S if not a valid channel
	pushl $__NR_read
2:	popl TS		//<cwritef)
	js 3f		//;?;try mmapped
	cmpl $0,TOS(2)
	jz 9f		//;?; 'ßterminal' w. known channel
	pushl edx	//;cdt
8:	movl TOS(1),edx	//;u
	movl TOS(2),ecx	//;a
	sys_ eax	//;read or write
	popl edx
5:	movl TS,TOS(1)	//; ( u u' u' ) copy received count or ernum
	call fposadd
	notl TS
4:	notl TS
0:	sarl $31,TS
	incl TS		//;u=0 if c not a file
	popl ebx	//<cwritef)
	ret
// ; no buffer, read char, don't send
9:	subl $__NR_read,TS
	movl TS,TOS(1)	//; clr "u" item
	jnz 0b
	call expeqt	//; ( ch -- c tf | ff )
	call cqnky
	movl cdtref(rnum)(edx),ecx
	movl TOS(1),edx
	movl ecx,TOS(2)	//;ernum
	TOSPULL(1)
	testl TS,TS
	jz 4b
	movl edx,TOS(1)
	jmp 4b
// ; out of range for writing, try file
7:	movl (eax),ebx
	pushl TS	//;cdt
	xorl $chfmmap,ebx//;fd
	jmp 8b
// ; try mmapped memory
3:	cmpl $0,TOS(2)
	jz 9b
	addl ebx,ebx
	pushl $-EBADF
	movl TOS(1),ebx	//;u
	popl TOS(1)	//;ernum
	jno 4b		//;?;not mmapped
	testl $mmapc,cdtref(tyfl)(edx)
	jz 4b		//;?;not mmaped
	movl TS,ecx
	movl edx,TS	//;pass cdt
	movl ebx,edx	//;len
	movl ecx,ebx	//;i/o code
	movl TOS(2),ecx	//;buf
	cmpl $__NR_write,ebx
	jz 1f
// ;i: eax=cdt, ebx=r/w code, ecx=buf, edx=count
// ;o: eax:=#received|erno, ecx:=buf-end, edx:=cdt
	call mmapread
	jmp 5b
1:	movl cdtref(flen)(eax),ebx
	subl cdtref(fpos)(eax),ebx
	js 7b		//;?;not in mapped range
	subl edx,ebx
	js 7b		//;?;not in mapped range
	call mmapwrite
	jmp 5b
// ; <mmapread>  mmapped file sys_read substitute, useing but #not# updateing the rsp. cdt data.
// ; <mmapwrite> ditto, sys_write.
// ;	i: eax=cdt, ebx=r/w, ecx=buf, edx=count
// ;	o: eax:=count of received bytes, ecx:=buffer end, edx:=cdt, eflags:=xx, other regs preserved
mmapread:
	pushal
	call mmparm
	js 1f
	rep movsb
1:	movl ebx,s_eax(esp)//;ret fetched count or -ve = error
	movl edi,s_ecx(esp)//;ecx:=buffer after received data
	movl eax,s_edx(esp)//;edx:=cdt
	popal
	ret
mmparm:	movl ecx,edi	//;buf
	movl cdtref(fpos)(eax),ebx
	movl edx,ecx	//;count
	movl cdtref(cmap)(eax),esi
	sarl $31,edx
	addl ebx,esi	//;mmem
	addl ebx,ecx	//;fpos.lo
	adcl 4+cdtref(fpos)(eax),edx//;fpos.hi
	pushl ecx
	pushl edx
	subl cdtref(fsiz)(eax),ecx
	sbbl 4+cdtref(fsiz)(eax),edx
	js 1f		//;within file size
	subl ecx,4(esp)
	sbbl edx,(esp)	//;adjust to <eof>
1:	popl edx
	popl ecx
	movb $0,(edi)	//;clr buf
3:	subl ebx,ecx	//;actual size to fetch
	movl ecx,ebx
	jz 2f		//;?;nothing
	pushl $-EINVAL
	popl ebx
	js 2f		//;?;-ve count
	movl ecx,ebx
2:	ret
mmapwrite:
	pushal
	call mmparm	//;chack parm, clip to <eof>
	js 1f		//;?;out of mapped range
	leal -EINVAL(ebx),ebx
	jz 1f		//;?;cannot write beyond <eof>
	movl ecx,ebx
	call cxee
	rep movsb
	call cxee
1:	movl ebx,s_eax(esp)//;ret fetched count or -ve = error
	movl edi,s_ecx(esp)//;ecx:=buffer after received data
	movl eax,s_edx(esp)//;edx:=cdt
	popal
	ret
cxee:	movl esi,edx
	movl edi,esi
	movl edx,edi
	ret
// ( a n ch c -- n'|er )
Xfwrite:pushl ebx
	pushl TS	//; c
	spop
	pushl TS	//; ch 	( a n ch		[ ch c
	call cwritef
	jz 1f		//;?;error ( a n ch - a er f	[ ch c
	movl 4(esp),ecx	//; c
	testl ecx,ecx
	js 1f		//;?;no <eol>
	spush
	movl ecx,TOS(1)	//; char
	movl (esp),TS	//; ch
	call bchid
	call wfemit
1:	leal 8(esp),esp
	movl TOS(1),TS
	TOSPULL(2)
	popl ebx
	NX
// ( disp ch -- fpos )
Xfposre:pushl NXL
Cfposre:lea cfposr,ecx	//<Cfread)
1:	pushl ebx	//<Xfpose)
	call bchid	//;edx:=cdt, ebx:=fd
	spop		//;TS:=disp
	call *ecx	//>cfposr,cfpose,cfposa)
	popl ebx
	ret
// ( disp-sof ch -- fpos )
Xfpose:	lea cfpose,ecx
2:	pushl NXL	//<Xfposa)
	jmp 1b
// ( disp-eof ch -- fpos )
Xfposa:	lea cfposa,ecx
	jmp 2b
// ; in: TS=fpos ebx=fd ecx=xx (esp)=rel fpos edx=cdt
// ;out: TS=fpos|er ebx=fd ecx=disp edx=edx
cfpose:	pushl $SEEK_END	//<Cflen)
	jmp cfposn
// ; in: TS=fpos ebx=fd ecx=xx (esp)=rel fpos edx=cdt
// ;out: TS=fpos|er ebx=fd ecx=disp edx=edx eflags
cfposa:	pushl $SEEK_SET	//<Crbyw,Cflen)
	jmp cfposn
// ; in: TS=disp ebx=fd ecx=xx edx=cdt
// ;out: TS=fpos|er ebx=fd ecx=ecx edx=edx eflags; if w.o. error fposn stored to cdt
cfposr:	pushl $SEEK_CUR	//<Cfposre,Cflen)
cfposn:	movl (esp),ecx	//<cfpose,cfposa)
	movl ebx,(esp)	//;save fd for subsequent calls by <Cflen>
	pushl edx	//<cfposa,cfpose)
	testl $consflags,cdtref(tyfl)(edx)
	jnz 9f		//;?;not applicable to tty &c
	addl ebx,ebx
	jno 1f		//;?;not mmapped
	testl $mmapc,cdtref(tyfl)(edx)
	jnz 3f		//;?;mmaped, ret fptr from cdt
1:	sarl $1,ebx	//;rst fd, leave mmap-flag masked off
	js 9f
	pushl esi
// ; _llseek: ebx:fd, ecx:disp.h, edx:disp.l, esi:(result), edi:rel(0:sof, 1:posn, 2:eof)
	movl edi,esi	//;result buf @PS
	movl TS,edx	//;disp.lo
	sarl $31,TS
	movl ecx,edi	//;'whence'
	movl TS,ecx	//;disp.hi
	movl TS,(esi)
	movl TS,4(esi)
	sys_ _llseek	//;fetch 64-bit fptr
	movl esi,edi
	popl esi
	cmpl $maxerr,TS	//;error if NC
	jc 0f
        orl TS,ecx
0:	movl (esp),edx
	movl (edi),TS
	movl 4(edi),ecx
4:	movl ecx,4+cdtref(fpos)(edx)
	movl TS,cdtref(fpos)(edx)
2:	popl edx
	popl ebx	//;fd
	ret
// ; file ptr may point beyond eof, w.o. error but, may not result to -ve posn
3:	cmpl $SEEK_CUR,ecx	//1
	jz 5f		//;?;+/- disp wrt current file ptr
	cmpl $SEEK_END,ecx	//2
	jz 6f		//;?;+/- disp wrt eof, +ve virtually appends to eof
	cmpl $SEEK_SET,ecx	//0
	jnz 9f		//;?;false 'whence' code
// ;+ve disp wrt file beginning
	xorl ecx,ecx
	testl TS,TS
	js 9f		//;?;disp to before file beginning
	movl ecx,4+cdtref(fpos)(edx)
	movl ecx,cdtref(fpos)(edx)
// ;+/- disp wrt current file ptr
5:	movl TS,ecx
	sarl $31,ecx
	addl cdtref(fpos)(edx),TS
	adcl 4+cdtref(fpos)(edx),ecx
	js 9f
7:	negl edx
	negl edx	//;force C
	jmp 4b
// ;+/- disp wrt eof, +ve virtually appends to eof
6:	movl TS,ecx
	sarl $31,ecx
	addl cdtref(fsiz)(edx),TS
	adcl 4+cdtref(fsiz)(edx),ecx
	jns 7b
// ;invalid op code, ret NC, cdt not modified
9:	pushl $-EINVAL
	xorl ecx,ecx
	popl TS
	jmp 2b

// ( a ch -- a|er )
Xfstat:	pushl NXL
// ; i: dstack; o: @tos=TS=a|ernum, ecx:=xx, edx:=cdt
Cfstat:	pushl ebx	//<copenm)
	call bchid	//;ebx=fd, edx=cdt
	movl ebx,TS
	js 1f		//; ret -1
	movl TOS(1),ecx	//; a
	sys_ fstat	//;receive file/device status
	cmpl $maxerr,TS
	movl TOS(1),TS
	jnc 1f
	movw stat_st_dev(ecx),cx
	movw cx,cdtref(tyfl)+2(edx)
1:	popl ebx
	TOSPULL(1)
	ret
// ( ch -- n )	;ret file size, also w. mmapped file/memory
Xflen:	pushl NXL
Cflen:	pushl ebx	//<Cmmapf,copenm)
	call bchid
	xorl TS,TS
	call cfposr	//;save current fptr
	pushl cdtref(fpos)(edx)
	pushl 4+cdtref(fpos)(edx)
	xorl TS,TS
	call cfpose	//; get eof posn
	popl ecx
	popl TS
	pushl 4+cdtref(fpos)(edx)
	pushl cdtref(fpos)(edx)
	call cfposa	//; rst current
	popl ecx
	popl TS
	movl ecx,cdtref(fsiz)(edx)
	movl TS,4+cdtref(fsiz)(edx)
	popl ebx
	movl ecx,TS
	ret
// ;close fs by channel number in TS
// ;ret edx:=chp for open &c, TS := 0 if channel closed, else fd
Xclose:	pushl NXL
Cclose:	pushl ebx	//<rghb(argcopy),Cldfind,cclosc(Xinclude2),copenm(Xopen(m)))
	pushl ecx
	call bchid	//;ebx:=fd, edx:=chp
	jns 6f		//;?;not mmapped
	addl ebx,ebx
	jno 5f		//;?;invalid fd
	addl ebx,ebx
	js 8f		//;?;not an open channel, may be mmapped
	shrl $2,ebx	//;rst fd
6:	movl ebx,TS
	jng 1f		//;protect stdin
	pushl $chans-workch
	leal cdtsize*workch+cdtref(chid)+cdtv,TS//;wrt table of system channels
	popl ecx
	cmpw bx,(TS)
	jnz 3f
	pushl $chinval
	popl (TS)	//;invalidate "work"
3:	addl $cdtsize,TS
	decl ecx
	js 2f
	cmpl edx,TS
	jz 3b		//;?;self
	cmpw bx,(TS)
	jnz 3b		//;?;different fd
	movl (TS),TS	//
	andl $chfmtst,TS//; mask off mmapped flag
	subl ebx,TS
	jz 4f		//;?;prevent close on fd of duplicate entry, ret fd+1
2:	sys_ close	//;free fd
	movl ebx,TS
4:	notl ebx
	addl ebx,(edx)	//; <chid>
	jnc 7f
	xorl $chfmmap,(edx)//; <chid>
	andl $chfmclo,(edx)//; mark mmapped
7:	addl ebx,TS
1:	notl TS		//;channel closed flag:0, used:+ve, no channel:-ve, stdin:-1
5:	popl ecx
	popl ebx
	ret
// ;unmap if mapped and file already closed
8:	testl $consflags,cdtref(tyfl)(edx)
	jnz 1b		//;?;not applicable to tty &c
	testl $mmapc,cdtref(tyfl)(edx)
	jz 1b		//;?;not mmaped
	movl cdtref(cmsz)(edx),TS
	spush
	movl cdtref(cmap)(edx),TS
	orl $chinval,cdtref(chid)(edx)
	call Cmunmapa
	jmp 5b
// ( a -- ch|er )	<Xusing)
// ;ret TS=channnel or ernum, edx=cdt, eflags NC on error
cusing:	spush		//;name
	movl uref(puopu),TS//;dft r/o
	call Cdrq	//;drive no
	call Cbchan	//;-> chan
copen:	xorl edx,edx	//;perm as by <umask>
	jmp copenm
// ( counted-asciz-name type chan --- fd|er ) ; open
Xopen:	movl uref(fpermu),edx//;permission bits XOR <umask>
	jmp xopenn
// ( counted-asciz-name type perm chan --- fd|er ) ; 'openm' w. permission bits @nos
// ;ret TS=channnel or ernum, edx=cdt, eflags NC on error
Xopenm:	movl TOS(1),edx	
	TOSPULL(1)
xopenn:	pushl NXL	//<Xopenm):c:
copenm:	pushl TS	//<Xusing); channel no.
	pushl ebx
	pushl edx	//;save permission bits
	call Cclose
	movl (esp),TS	//;permission bits
	movl edx,(esp)	//;save cdt
	movl TS,cdtref(perm)(edx)// ;permission bits to cdt
	pushl TS	//;flags
	xorl TS,TS
	movl TOS(1),ecx	//;->open mode
	movl TOS(2),ebx	//;->name
	movl TS,cdtref(fpos)(edx)// ;init fpos
	movl TS,cdtref(tyfl)(edx)// ;clr type flags
	movl TS,4+cdtref(fpos)(edx)
	movl TS,cdtref(rnum)(edx)// ;init rnum
	movl ecx,cdtref(mdfl)(edx)//;open-mode to cdt
	popl edx	//;->flags
	incl ebx	//;ptr to 1st char
	sys_ open	//;eax:=fd
	popl edx	//;cdt
	popl ebx
	popl ecx
	cmpl $maxerr,TS
	jnc 1f		//;?; open error, ret NC
	tochanid TS,edx	//;TS into chid posn of cdt @edx
	movl ecx,TS	//;ret channel no.
	pushl ecx
	spush;spush
	movl PS,TOS(1)
	call Cfstat	//;( a ch -- a|er )
	movl (esp),TS
	call Cconsq	//;whether a terminal channel, TS:=1|0 edx:=cdt
	movl (esp),TS
	call Cflen	//;fetch file size into cdt
	TOSPULL(1)
	popl TS
1:	TOSPULL(2)
	ret
.nolist
// -----------------------------------------------------------------------------
// fileio.S <eof>
