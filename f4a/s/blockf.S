//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; blockf.S 
// =============================================================================
.psize 0
.att_syntax noprefix
.text
.list

//; ---- screenfiles -----

// ( -- n )
Xbbyscr:pushl NXL
Cbbyscr:xorl edx,edx	//<Cldfind,Xload1,Cddgreat,Cplineq,Xqq,Xx)
	spush
	movl $1024,TS
	divl consv(bbybuf)
	ret
// ( f -- )
Xdrs:	decl TS
	spop
	jz Xdrone
// ( -- )
Xdrzero:andb $~64,3+uref(offsetu)
	NX
// ( -- )
Xdrone:	orb $64,3+uref(offsetu)	//<Xdrs)
	NX
// ( -- n )
Xdrq:	pushl NXL
Cdrq:	spush		//<cscrcdt,cusing,Xchpq,cclosc)
	movl uref(offsetu),TS
	addl TS,TS
	shrl $31,TS
	ret
// ( -- a ) ; current blockfile channels' chp pointer
Xchpq:	call Cdrq
	addl $cdr0v-chpv,TS//; dr0 index addr
	NX
// ( -- )
Xupdate:movl prevv,ecx
	orb $128,3(ecx)
	NX
// ( n -- ) ; continue loading after n lines (blocks)
// ; c/l in @ over mod - in +! c/l b/buf */ blk +!
Xpluscont:
	call Cqloading	//; ?loading
	mull consv(cbyl)//; c/l *
	divl consv(bbybuf)//; b/buf /
	addl TS,uref(blku)//; blk +!
	xorl edx,edx
	movl consv(cbyl),ecx//; c/l in @ over mod - in +!
	movl uref(inu),TS
	divl ecx
	testl edx,edx
	jz 1f		//;?;in@ at eol
	subl edx,ecx
	addl ecx,uref(inu)
1:	spop
	NX
// ( a1 -- a2 flg ) ;: +buf b/buf + 2+ 2+ dup limit = if drop first then dup prev @ - ;
Xplusbuf:
	pushl NXL
Cplusbuf:		//<Cblock,Cbuffer)
	addl consv(bbybuf),TS//; b/buf
	addl $blkno+blknl,TS
	cmpl limitv,TS
	jc 1f
	movl firstv,TS
1:	spush
	subl prevv,TS
	ret
// ( n -- a )
// ; offset @ + >r prev @ dup @ r - dup + if begin
// ;	+buf 0= if drop r buffer dup r 1 r/w 4- then dup @ r - dup + 0= until
// ; dup prev ! then r> drop 2+ ;
Xblock:	pushl NXL
Cblock:	addl uref(offsetu),TS//<Cplineq,Csource,Cword); offset @ + >r
	pushl TS
	movl TS,ecx
	movl prevv,TS	//; prev @ dup @ r -
	subl (TS),ecx
	shll $1,ecx	//; dup + if
	jz 3f		//;begin
4:	call Cplusbuf	//; +buf 0= if
	spop		//;
	jnz 5f
	movl (esp),TS	//; drop r
	call Cbuffer	//; buffer dup r 1
	spush;spush
	movl (esp),TS
	pushl $1	//; flg read
	spush
	popl TS
	call Crbyw	//; r/w			( a a b f -- adr )
	subl $4,TS	//; 4-
	jc 3f		//;  no channel	  <-'
5:	movl (TS),ecx	//; endif
	subl (esp),ecx	//; dup @ r -
	shll $1,ecx	//; dup +
	jnz 4b		//;0= until
	movl TS,prevv	//; dup prev !
3:	addl $4,esp	//; r> drop
	addl $4,TS	//; 4+
	ret
// ( n -- a )
Xbuffer:pushl NXL	//;( n -- a )
Cbuffer:movl usev,ecx	//<Cblock,Cflush); fetch,trr
	spush
	pushl ecx
	movl ecx,TS	//;  -- n use@ 		[ use@
1:	call Cplusbuf	//; plusbuf,zerobranch,1b-.
	spop
	jz 1b		//;  -- n buf+ 		[ use@
	movl TS,usev	//; use,store
	movl (esp),TS	//; rx,fetch,zlt,zerobranch,2f-.
	cmpl $0,(TS)	//;  -- n use@ 		[ use@
	jns 2f		//; rx,p4,rx,fetch,lit,0x7fffffff,andx,zero,rbyw
	spush
	addl $blkno,TS	//;  -- n buf+ 		[ use@
	pushl TS
	spush
	movl -blkno(TS),TS
	andl $0x7fffffff,TS// ; off "update"-flag
	spush
	xorl TS,TS
	call Crbyw
	popl ecx	//;block buffer
	jnc 2f		//;r/w error
	andb $127,3-blkno(ecx)//;clr 'update' flg
2:	movl TOS(1),ecx	//;rx,store,rx,prev,store,rgreat,p4,semis
	movl ecx,(TS)
	movl TS,prevv
	TOSPULL(1)
	popl TS
	addl $blkno,TS
	ret
// ( a b f -- )
Xrbyw:	call Crbyw
	jnc 1f
	NX
1:	spush
	movl cdtref(rnum),TS
	jmp Xert
// ( buf blk flg -- ) ;NC on error
Crbyw:	pushl ebx	//<Cblock,Cbuffer,Xrbyw)
	pushl TS	//; r/~w
	movl TOS(1),ecx	//;blk
	movl ecx,TS
	shll $2,TS	//;bit below m.s.b. for dr0/dr1 selection
	sbbl ecx,ecx	//;rchan
	shrl $2,TS
	mull consv(bbybuf)//; buffer fptr
	movl TS,TOS(1)
	movl ecx,TS	//;0-1=>dr0, -1-1=>dr1
	negl TS
// ;brchid) in: al = relchan; out: ebx:=fd edx:=cdt
	call bscrch
	movl ebx,ecx
	addl ecx,ecx
	jc 2f		//;no channel, no error
	movl TOS(1),TS
// ; in: TS=fpos ebx=fd ecx=xx (esp)=rel fpos edx=cdt
// ;out: TS=fpos|er ebx=fd ecx=disp edx=edx
	call cfposa
	jnc 2f		//;file error
	movl (esp),TS	//;flag
	movl TOS(2),ecx	//;addr
.ifeq (__NR_read+1-__NR_write)
	negl eax
	addl $__NR_write,eax
.else
	testl TS,TS
	leal __NR_write(eax),eax
	jz 1f
	addl $__NR_read-__NR_write,eax //;if flag was 1, convert to 3=io.fstrg
1:
.endif
	movl edx,(esp)	//;save cdt
	movl $ssize,edx
	sys_ eax	//;either, read or write
	movl (esp),edx
	movl eax,cdtref(rnum)(edx)//; sto error code
	cmpl $maxerr,TS
	jnc 2f		//;?; error
	addl TS,cdtref(fpos)(edx)
	adcl $0,4+cdtref(fpos)(edx)
2:	popl TS
	popl ebx
	SPOP(3)
	ret		//;C = error

// ; -- 'load', 'l-load' &c --

// ( chan -- )
Xchain:	call Csinput
	movb TSl,chpv+rdr1ch
	movl $lumpi*2,TS
	call C2loc
	movl uref(l0u),ecx
	orl $incfi,uref(bitsu)
	movl ecx,uref(tibu)
	movl $-1,uref(blku)
	NX
// ( -- )(R: xt -- n0..n7 ) ; save @lword to rstack
cslld:	popl ecx	//<Xlsload1)
	pushl lldv
	pushl lldv+4
	pushl lldv+8
	pushl lldv+12
	pushl lldv+16
	pushl lldv+20
	pushl lldv+24
	pushl lldv+28
	jmp *ecx
// ( -- )(R: n0..n7 xt -- ) ; rst @lword from rstack
crlld:	popl ecx	//<Xlload2)
	popl lldv+28
	popl lldv+24
	popl lldv+20
	popl lldv+16
	popl lldv+12
	popl lldv+8
	popl lldv+4
	popl lldv
	jmp *ecx
// ( -- )(R: -- tib scr blk in offset base s0 )
// ; i: edx preseved!, o: eflags, ecx
Xsinput:pushl NXL
Csinput:popl ecx	//<Cldfind,Xinclude,Xload2,Xchain)
	pushl uref(tibu)
	pushl uref(scru)
	pushl uref(blku)
	pushl uref(inu)
	pushl uref(offsetu)
	pushl uref(baseu)
	pushl uref(currentu)
	pushl uref(contextu)
//-'sictx'
	pushl cdtref(chid)+cdtptr(dr0ch)//; dr0/1 channels @tor!
	pushl cdtref(chid)+cdtptr(dr1ch)
	pushl uref(bitsu)
	pushl (chpv)	//; stdin/out/err,work reloc indices
#define sictx 5*4
	pushl ecx
	movl uref(vbotu),ecx
	subl ecx,sictx(esp)//; stored @context & @current rel vmem base
	subl ecx,sictx+4(esp)
	ret
// ( -- )(R: -latest i/o-description- -- )
Xend:	btrl $incfib,uref(bitsu)//<; 'end' after 'chain'
	jc 1f
	NX
1:	spush		//; drop local tib
	movl $-(lumpi*2),TS
	call C2loc
// ( -- )(R: tib scr blk in offset base s0 -- )
// ; i: edx don't change!, o: eflags, ecx
Xrinput:pushl NXL
Crinput:movl uref(vbotu),ecx//<Cldfind,Xlload2,Xload2,cinclude)
	addl ecx,sictx(esp)
	addl ecx,sictx+4(esp)
	popl ecx
	popl (chpv)
	popl uref(bitsu)
	popl cdtref(chid)+cdtptr(dr1ch)//; dr0/1 channels @tor!
	popl cdtref(chid)+cdtptr(dr0ch)
	popl uref(contextu)
	popl uref(currentu)
	popl uref(baseu)
	popl uref(offsetu)
	popl uref(inu)
	popl uref(blku)
	popl uref(scru)
	popl uref(tibu)
	jmp *ecx
// ( a1 a2 -- scr ) ; { file|0 name l-load }, f := zero flag after success
// ; find ": name ", name at a2, in top line per screen in file a1 or current if a1=0 and "load" the rsp. screen
Xldfind:pushl NXL
Cldfind:popl ecx	//<Xlload1,Xlsload1):!:;1st level subroutine :: :only: ::
	movl TOS(1),edx	//;a1
	spush
	movl ecx,TOS(2)	//;save return address
	movl edx,TOS(1)	//; ret a1 a2
	call Csinput	//;save current input parameters
	call cscrcdt	//; ret a1 a2 ch, edx:=cdt
	spush		//; ret a1 a2 ch ch
	movl TOS(3),TS	//; ret a1 a2 ch a1
	call cinclude	//; ret a1 a2 ch a1 -- ret a1 a2 ch chn ) get source ch, edx=cdt, C if successful
	jnc 5f		//;?; open failed
	pushl TS	//;save temp. channel for 'close'
	cmpl $1,TOS(3)	//;a1
	jnc 1f		//;?; =/= 0
	cmpl $0,(edx)	//; <chid>
	js 2f		//;?;no channel
	pushl edx
	call cscrcdt	//;used channel
	movl TS,TOS(4)
	spop
	popl edx
	testl $mdupc,cdtref(tyfl)(edx)
	jnz 1f		//;?; <dup>licated, current file
2:	movl $f4xscrt,TOS(3)//;use dft screen-file
1:	TOSPULL(1)
	andl $~mdupc,cdtref(tyfl)(edx)
	call Cmmapf	//; -- ret a1 a2 size addr )
	movl TOS(1),ecx	//;size
	spush;spush;spush;spush;spush //; -- ret a1 a2 sz mm mm mm mm mm mm
	movl ecx,TOS(3)
	popl TS		//; -- ret a1 a2 sz mm mm sz mm mm chn
	call Cclose
	call Crinput	//;rst input descr.
	movl TOS(1),TS	//; -- ret a1 a2 sz mm mm sz mm mm er=mm
	call Cert	//;xit on -ve mmap error
	pushl edi	//; -- ret a1 a2 sz mm mm sz mm mm
	pushl esi
	movl TOS(6),ecx	//;a2 counted name
	leal lldv+1,edx	//; 'llword' stg vari
	movl edx,TOS(1)	//; -- ret a1 a2 sz mm mm sz a2nm mm
	movl $0x203a20,(edx)//; " : "
	leal 3(edx),edi
	movzbl (ecx),TS	//; -- ret a1 a2 sz mm mm sz a2nm s2nm
	leal 1(ecx),esi
	movl TS,ecx
	addl $4,TS
	movb TSl,-1(edx)//; count-byte for 'lword?'
	pushl $BL
	rep movsb
	popl (edi)	//; -> " : name ",0
	popl esi
	popl edi	//; -- ret a1 a2 sz mm  mm sz a2nm s2nm
	call Cmatch	//; -- ret a1 a2 sz mm  mflg disp
	call Cswap2	//; -- ret a1 a2 mflg disp sz mm
	call Cmunmapa	//; -- ret a1 a2 mflg disp er
	xorl edx,edx
	testl TS,TS
	jnz 4f		//;?; m(un)map error
	movl TOS(2),ecx	//;mflg
	movl TOS(1),TS	//;disp: byte posn in screenfile
	testl ecx,ecx
	jz 4f		//;?; word not found
	movl TOS(4),ecx	//;a1, file
	movl TS,TOS(4)	//; f=disp
	divl consv(cbyl)//; -- ret f a2 mflg disp blks
	movl ecx,TOS(2)	//; a1 file
	call Cbbyscr	//; -- ret f a2 a1 disp blks b/scr
	movl TS,ecx	//; b/scr
	movl TOS(1),TS	//; blk
	divl ecx	//; -- ret f a2 a1 disp blks scr
3:	pushl TOS(6)
	movl TOS(3),ecx	//;file
	movl TS,TOS(6)	//;scr#
	testl TS,TS	//; Z if word not found, i.e. cant'load scr# 0 (anyway)
	movl ecx,TS
	TOSPULL(5)	//; -- scr a1|xx 0 
	ret
5:	xorl TS,TS
	spush
	movl TS,edx
4:	movl edx,TS
	movl edx,TOS(2)	//; -- 0 0 )
	spush
	jmp 3b
// ( ccc, -- 0 a c f | 0 0 )
cloadq:	spush
	xorl TS,TS
	call Cdashfind
	movl uref(dpu),TS
	ret
// ( -- )
Xledit:	spush
	leal fedscrt,TS	//; file
	spush
	leal ldedt,TS	//; name
	jmp Xlsload1	//; l-load
// ccc( -- ) ; if word ccc not found l-load from f4 extensions screenfile
Xxlloadq:call cloadq
	lea f4xscrt,ecx
	jnz 3f
	movl ecx,TOS(1)
	jmp 2f
// ccc( -- ) ; lload word ccc from pre-defined extensions screenfile
Xxlload:spush
	lea f4xscrt,TS
	jmp 1f
// ccc( -- ) ; if word ccc not found l-load from current screenfile
Xlloadq:call cloadq
	jz 2f
3:	popl IP
	jmp Xdrop4
// ccc( -- ) ; l-load word ccc from current screenfile
Xlload1:spush
	xorl TS,TS
1:	call Cblword
2:	call cslld	//;save previous 'lword'
	call Cldfind	//; a1 a2 -- scr a1
	jnz Xinclude1	//;?; 'lword' found
	SPOP(2)
	jmp 3f		//>Xlload2)
// ( a1 a2 -- f ) ; { file|0 name l-load }, f := zero flag after success
Xlsload1:
	call cslld	//;save previous 'lword'
	call Cldfind	//; a1 a2 -- scr a1
	jz 2f
// ( n a -- ) ; use current screenfile if a = 0
Xinclude1:
	call Csinput	//;save input parameters
	call cinclude	//;( a|0 -- ch )
	spop		//; drop channel
	jmp 1f		//>Xload1)
//<;Xlsload1) error, rst old lload-name
2:	TOSPULL(1)	//;false flag for l-load error
	jmp 3f		//>Xlload2)
// ( -- n ) ; trailing part of 'l-load', returns last encounterd scr#
Xlsload2:
	spush
	movl uref(scru),TS
// ( -- ) ; trailing part of 'lload'
Xlload2:call cclosc	//;close current scrrenfile channel
	call Crinput	//;rst i/o parameters
3:	call crlld	//<Xlsload1) on error, rst previous 'lword'
	popl IP		//; 'semis'
	NX
// ( n -- ) ; leading part of "load" -
Xload1:	call Csinput	//;save input parameters
1:	pushl $10	//<Xinclude1)
	pushl $0
	movl TS,uref(scru)
	popl uref(inu)
	popl uref(baseu)//;decimal
	call Cbbyscr
	mull TOS(1)
	movl TS,uref(blku)
	SPOP(2)
	NX
// ( a -- ch )
cinclude://<Xinclude1,Cldfind); 1st level subroutine :only:!
	call cscrcdt	//;current scr-file edx=cdt, TS=ch 
	movl TOS(1),ecx	//;file
	cmpl $chans,ecx
	jc 2f		//;?;channel no.
	cmpw $(32<<8)|4,(ecx)
	jc 2f		//;?;no file-name, duplicate current channel
	spop		//;drop channel
6:	tochanid $-1,edx//;prevent closing previously used, saved channel!
	call cusing	//;( a -- chn )
// ; TS=channnel or ernum, edx=cdt, eflags NC on error
	jc 4f		//;?; open ok
3:	popl edx
	call Crinput	//;rst input descr
	pushl edx
	jmp Cert	//;handle -ve open error, ret NC, drop ch|er
2:// ;open duplicate channel, edx:=cdt ( .. -- a ch 
	movl TS,TOS(1)	//;save ch
	cmpl $2,(edx)	//; <chid>
	jg 5f		//;?; screenfile open
	TOSPULL(1)	//;try dft ( .. -- a' )
	leal f4xscrt,TS
	jmp 6b		//;-> 'using'
5:	pushl ebx	//; ( .. -- 0 ch
	chanid edx,ebx
	sys_ dup	//;duplicate current screen-file channel
	popl ebx
	cmpl $maxerr,TS
	jnc 3b		//;?;'open' error
	tochanid TS,edx
	orl $mdupc,cdtref(tyfl)(edx)//;for l-load from current
	negl TS		//; rst C flag
	spop
4:	ret		//; -- chn | abort ) continue 'load'ing
// ( -- ) ; trailing part of 'include' -
Xinclude2:
	call cclosc	//;:contd: Xload2
// ( -- ) ; trailing part of 'load' -
Xload2:	call Crinput	//<Xinclude) :c:
	popl IP		// ;'semis'
	NX
cclosc:	call Cdrq	//<Xinclude2,Xlload2)
	call Cbchan
	call Cclose
	spop
	ret
// ( -- ) ; '[-->]' { ?loading b/scr blk @ over mod - blk +! 1 scr +! 0 in ! }
Xddgreat:
	call Cqloading
	call Cbbyscr
	xorl edx,edx
	movl TS,ecx
	movl edx,uref(inu)
	movl uref(blku),TS
	divl ecx
	incl uref(scru)
	spop
	subl edx,ecx
	addl ecx,uref(blku)
	NX
// ccc( a -- )
Xusing:	call Cflush
	call cusing	//;open file for current 'drive'
	jmp Xert
// ( -- )
Xflush:	pushl NXL
Cflush:	pushl prevv	//<Xusing)
	spush
	addl $blkno,(esp)
1:	movl $0x7fffffff,TS
	call Cbuffer
	cmpl (esp),TS
	jnz 1b
	popl TS
	spop
	ret
// ( -- )
Xemptybuffers:
	movl firstv,ecx
	spush
	movl ecx,TS
	spush
	movl limitv,TS
	subl ecx,TS
	jmp Xerase
// ( line scr -- a u ) ;(line) >r c/l b/buf */mod r> b/scr * + block + c/l ;
Xplineq:pushl NXL
Cplineq:movl TS,ecx	//<Cdotline)
	movl consv(cbyl),TS//;					[ s
	pushl TS
	pushl ecx	//;				 	[ scr c/l
	spush
	movl consv(bbybuf),TS//; b/buf ( -- l c/l b/b
	call Cmulbymod	//; 		( l c/l b/buf -- chars blks )
	call Cbbyscr	//;b/scr	( -- chars blks b/scr )
	popl ecx	//;scr				 	[ c/l
	imull ecx	//;scr-blocks	( -- chars blks scrblocks )
	addl TOS(1),TS	//;+ blocks from line#
	TOSPULL(1)	//;		( -- chars blk# )
	call Cblock
	addl TS,TOS(1)	//;rmd from n1/blocks -> actual line
	popl TS		//;c/l
	ret
// ( line scr -- )
Xdotline:	//;plineq,dashtrailing,type,semis
	pushl NXL
Cdotline:		//<Cmessge,Xlist,Xindex)
	call Cplineq
	call Cdashtrailing
	jmp jctype
// ( -- ) ; 'l'
Xl:	spush
	movl uref(scru),TS
clist:	leal blistb+4,W
	jmp Cmdfer
// ( n -- )
Xlist:	pushl NXL
	pushl uref(baseu)
	pushl $10
	call Ccr
	movl TS,ecx
	popl uref(baseu)//;decimal
	sarl $31,ecx
	notl ecx
	andl ecx,TS	//; 0max
	movl TS,uref(scru)
	SPUSH(4)	//;( n -- n n n n x
	leal scrt,TS
	call Cprint
	xorl TS,TS
	call Cdotr	//;( n n n 0 -- n n
	xorl ecx,ecx
	movl TOS(1),TS
	movl ecx,TOS(1)	//;( n n -- 0 n
1:	movl TOS(1),ecx	//;s
	incl TOS(1)	//;( s' n
	SPUSH(4)
	pushl $-4
	movl ecx,TOS(3)
	movl ecx,TOS(1)	//;( s' n s n s x
	call Ccr
	popl TS
	call Cdotr	//;( s' n s n
	call Cdotline
	cmpl $16,TOS(1)
	jl 1b
	popl uref(baseu)
	SPOP(2)
	jmp Ccr
.ifne _ti_
// ( n -- ) ; "triad"
Xtriad:	pushl $3
	xorl edx,edx
	popl ecx
	spush
	divl ecx
	leal (TS,TS,2),TS
	movl TS,TOS(1)
	addl $3,TOS(1)
1:	stosl
	call Ccr
	call clist
	incl TS
	cmpl TOS(1),TS
	jl 1b
	pushl $15
	TOSPULL(1)
	popl TS
	call Cmessage
	jmp Xcr
.endif
.ifne _ie_
// ( n1 n2 -- )
Xindex:	movl TS,ecx
	movl TOS(1),TS
	movl ecx,TOS(1)	//;swap
1:	pushl $0
	pushl $-4
	spush;spush;spush;spush//;( to fr fr fr fr xx
	popl TS
	call Cdotr
	popl TOS(1)	//;( to fr 0 fr
	call Cdotline
	call Ccr
	call Cqesck
	spop
	jnz 2f
	cmpl TOS(1),TS
	incl TS
	jc 1b
2:	SPOP(2)
	NX
.endif
// ( in@ blk@ -- ) ; '??'
Xqq:	pushl NXL
	call Cbbyscr	//; b/scr
	call Cbym	//; /mod
	pushl $'c'
	pushl $'B'
	pushl $'S'
	movl TS,uref(scru)
	popl ecx
	call 1f
	popl ecx
	call 1f
	popl ecx
1:	spush
	movl ecx,TS
	call cdemit
	jmp Cdot
.nolist
// -----------------------------------------------------------------------------
// blockf.S <eof>
