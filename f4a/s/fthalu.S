//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; fthalu.S 
// =============================================================================
.psize 0
.att_syntax noprefix
.list
.text

//; ---- division, suitable to the fpe sighnd ----

//; ---- unsigned div/mul
// ( ud1 u2 -- u2 u4 )
Xuby:	pushl NXL	// 'u/'
	call Cumbym
ertd:	spop	//<;Cert)
	ret
.ifne _dx_
// ;:::::: (shorter but not necessarily faster)
// ( ud1 u2 -- u3 ud4 ) ; 'm/mod'
Xumbym:	pushl NXL	//<;Csharp,Csharps,Xuby)
Cumbym:	movl TS,ecx
	xorl TS,TS
	spush;spush;spush//; uq1
	movl ecx,TOS(1)	//; ud2
	call Cuqdbym	//; ( uq1 ud2 -- ud3 ud4 )
	movl TOS(1),ecx
	movl TOS(3),edx	//; pass rmd to caller
	movl ecx,TOS(2)	//; -- u3 ud4 )
	TOSPULL(1)
	ret
.else # _dx_ = 0
//<;Cumbym)
9:	incl TOS(2)	//<;0/0)
6:	xorl TS,TS	//<;div/1
	movl TOS(2),ecx
	movl TS,TOS(2)
	movl TOS(1),TS
8:	movl ecx,TOS(1)	//<;div/0
	ret
7:	orl TOS(1),TS	//;div/0
	orl TOS(2),TS
	jz 9b		//;?;0/0
	pushl $-1
	popl TS
	movl TS,ecx
	movl TS,TOS(2)
	jmp 8b
// ( ud1 u2 -- u3 ud4 ) ; 'm/mod'
Xumbym:	pushl NXL
Cumbym:			//<;Csharp,Csharps,Xuby)
// ; edx returns overfow flag,
// ; ovf 'exception' by signal handler
	movl TS,ecx	// ;ecx:=dvs
	cmpl $1,TS
	jz 6b		//;?;trivial case, div/1
	jc 7b		//;div 0
	spop
1:	leal 10f,edx	//;ret from sigh
	pushl ebx
	pushl edx
	leal fpecx,ebx	//;udiv sighnd <fpecx>
// ; i:	TS:dvd.lo edx:dvd.hi ecx:dvs
// ; o:	TS:qot    edx:rmd    ecx:dvs
	xorl edx,edx	//;unsigned dvd.hi
	divl ecx	//;a:=qot.h d:=r.h=q.lh
2:	movl TS,(PS)	//;qh
	mov TOS(1),TS	//;tos0:=q.h a:dvd.lo
	divl ecx	//;a:=qot.l d:=rmd
	addl $4,esp
10:	popl ebx
3:	movl edx,TOS(1)	//;rmd
	movl (PS),ecx	//;qh
	pushl TS	//;ql
	spush		//;push qot.lo, tos:=qot.h
	mov ecx,TS	//;d:=qot.h tos2:=rmd
	popl ecx	//;"#","#S": edx=rmd, TS|ecx=qot
	ret
.endif # _dx_ = 0
// ( ud1 n2 -- ud3 uvf ) ; 'udm*'
Xudmmul:pushl NXL
Cudmmul:movl TS,ecx
	mull TOS(2)
	movl TS,TOS(2)
	movl TOS(1),TS
	movl edx,TOS(1)
	mull ecx
	addl TS,TOS(1)
	sbbl ecx,ecx	// ovf
	movl edx,TS
	ret
// ( u1 u2 -- ud ) ; 'u*'
Xumul:	movl TS,edx
	cmpl $1,TS
	jz 1f		//;?; * 1
	jc 3f		//;?;   0
	mull TOS(1)
3:	movl TS,TOS(1)	//<Xmmul,Xumul); *0
	movl edx,TS
	NX

//; ---- signed div/mul

// ( n1 n2 -- d ) ; 'm*'
2:	negl TOS(1)	//<Xmmul); *-1
1:	movl TOS(1),TS	//<Xmmul,Xumul); *1
	sarl $31,TS
	NX
Xmmul:	movl TS,edx
	cmpl $1,TS
	jz 1b		//;?; * 1
	jc 3b		//;?;   0
	cmpl $-1,TS
	jz 2b		//;?; *-1
	imull TOS(1)
	movl TS,TOS(1)
	movl edx,TS
	NX
// ( n1 n2 -- n3 ) ; '*'
2:	negl TOS(1)	//<Xmul); *-1
1:	spop		//<Xmul); *1
	NX
Xmul:	cmpl $1,TS
	jz 1b		//;?; * 1
	jc 3f		//;?;   0
	cmpl $-1,TS
	jz 2b		//;?; *-1
	mull TOS(1)
3:	TOSPULL(1)	//<; *0
	NX
// ( dn n -- dn n ) ; edx:=TOS1=dh, ecx:=TS=n, TS:=sgn(d XOR n)|sgn(n), eflags ecx ?= -1
cmdsgn:	movl TOS(1),edx	//<;Cdmmul,Cmdbym,Cmmulby) dvd.hi
	movl TS,ecx	//;dvs
	sarl $16,TS	//;eax := sgn(d XOR n)|sgn(n)
	xorl edx,TS	//;qot sign
	cmpl $-1,ecx
	ret
// ( dn n -- |dn| |n| ) ; i:edx=dh, ecx=n, TS=dl ; o: TS=eax=edx:=|dh|, TOS(2):=|dl|, ecx:=|n|, eflags from dvs ?= 1
cmdabs:	testl ecx,ecx	//<:Cdmmul:,:Cmdbym:)
	jns 2f		//;?;+ve dvs
	negl ecx
2:	testl edx,edx
	jns 3f		//;?;+ve dvd
	negl TS
	adcl $0,edx
	negl edx
	movl TS,TOS(2)	//;save d.lo
3:	movl edx,TS	//;dh
	cmpl $1,ecx
	ret
// ( d n -- t ) ; 'dm*'
Xdmmul:	pushl NXL
Cdmmul:	call cmdsgn	//;edx:=TOS1=dh, ecx:=TS=n, TS:=sgn(d XOR n)|sgn(n), eflags ecx ?= -1
	pushl TS	//; save sgn flags
	jz 2f		//;?;mul -1
	movl TOS(2),TS
	call cmdabs	//;edx:=|dh|, TOS(2):=|dl|, ecx:=|n|, eflags from dvs ?= 1
	jz 2f		//;?;mul 1
	jc 6f		//;?;mul 0
	movl edx,TOS(1)	//;save |md.h|
	movl ecx,TS	//;rst mr
	mull TOS(2)	//;md.lo
	movl TS,TOS(2)	//;p.ll
	pushl edx	//;p.lh
	movl TOS(1),TS
	mull ecx
	popl ecx	//;p.lh
	addl ecx,TS
	adcl $0,edx	//;p.ov
	movl TS,TOS(1)	//;p.hh
	movl edx,TS
//<;apply sign
4:	pop edx		//; sgn flags
	testl edx,edx
	jns 3f		//;?;+ve prod
	negl TOS(2)
	adcl $0,TOS(1)
Cdminus:negl TOS(1)	//<Cqfmbym,cdpm)
	adcl $0,TS
	negl TS		//<Xmul)
3:	ret
//<;mul +/-1)
2:	movl TOS(1),TS
	sarl $31,TS	//; sgn xtd ovf
	jmp 4b
//<;mul 0)
6:	xorl TS,TS
	popl edx	//; drop sgn flags
	movl TS,TOS(1)
	movl TS,TOS(2)
	ret
// ( d -- |d| ) ; dabs
Xdabs:	pushl NXL
Cdabs:	testl TS,TS	//<Cdbym,Cdbym)
	js Cdminus
	ret
.ifne _dx_
// ( t1 n1 -- d2 ) ; 'm*/mod'
Xmmbym:
	call Cmmulby
	spush
	movl TOS(2),edx
	movl ecx,TOS(2)
	movl edx,TOS(1)
	NX
// ( t1 n1 -- d2 ) ; 'dm/' treble by sing
Xdmby:	pushl NXL
	jmp 4f
.endif
// ( d1 n1 +n2 -- d2 ) ; m*/ ecx:=rmd
Xmmulby:pushl NXL
Cmmulby:pushl TS	//; dvs
	spop
	call Cdmmul	//; ( d n -- t )
	spush
	popl TS		//; ( t n -- 
4:	call cmdsgn	//;edx:=TOS1=dh, ecx:=TS=n, TS:=sgn(d XOR n)|sgn(n), eflags ecx ?= -1
	jz 2f		//;?;negate
	cmpl $1,ecx	//;dvs
	jz 6f		//;?;div 1
	pushl TS	//;save sgn flags
	jc 3f		//;?;div0
	xorl edx,edx
	movl TOS(1),TS	//;dvd.o
	divl ecx	//;a=q.o
	movl TOS(2),TS
	divl ecx
	movl TS,TOS(2)	//;q2
	movl TOS(3),TS
	divl ecx
	movl TS,TOS(3)	//;q1
	movl edx,ecx	//;pass rmd to caller
	popl edx
	testl edx,edx
	jns 6f
2://<;div -1)
	negl TOS(3)
	adcl $0,TOS(2)
	negl TOS(2)
6://<;div1)
	SPOP(2)		//;drop dvs & ov
	ret
//<;div0, Cmmulby)
3:	movl TOS(1),edx
//<;div/0, Cmdbym)
1:	call rzreg	//;div0
	movl ebx,(esp)	//;drop sgn flags
	movl edx,TOS(2)	//;rmd
	movsbl al,TS
	popl ebx
	movl TS,TOS(1)	//;q.lo
	movb dh,dl	//;+ve: 7fffffff -ve: 80000000
	movl edx,TS	//;q.hi
	xorl ecx,ecx	//;ovf flag Z
	ret
.ifne _dx_
// ;:::::: (shorter but not necessarily faster)
// ( dn n -- n1 d2 ) ; md/mod, rmd dqot, common div routine
Xmdbym:	pushl NXL
Cmdbym:	spush		//<:Cmby:)
	sarl $31,TS
	call Cdbym	//;( d1 d2 -- d3 d4 ) ; d/mod
	movl TOS(1),ecx
	movl ecx,TOS(2)
	TOSPULL(1)
	ret
.else # _dx_ = 0
// ( dn n -- n1 d2 ) ; md/mod, rmd dqot, common div routine
Xmdbym:	pushl NXL
Cmdbym:	call cmdsgn	//<:Cmby:); dvd.hi
// ;:cmdsgn: edx:=TOS1=dn.h, ecx:=TS=n, TS:=sgn(d XOR n)|sgn(n), eflags ecx ?= -1
	pushl TS	//;sgn(r) = sgn dvd|sgn(q) = sgn(dvd xOR dvs)
	movl TOS(2),TS	//;dl
	call cmdabs	//;edx:=|dh|, TOS(2):=|dl|, ecx:=|n|, eflags from dvs ?= 1
	jz 6f		//;div/1
	jc 1b		//;div/0
	sarl $31,edx	//;sign extend dvd
	divl ecx	//;div hi
	movl TS,TOS(1)	//;q.hi
	mov TOS(2),TS	//;d.lo
	divl ecx	//;div lo
	movl edx,TOS(2)	//;r
	movl TS,ecx
	movl TOS(1),TS
	movl ecx,TOS(1)
7:	popl ecx	//;sgn flags
	testl ecx,ecx
	jns 4f		//;?; +ve rmd
	negl TOS(2)
4:	shll $16,ecx
	jns 5f		//;?; +ve qot
	negl TOS(1)
	adcl $0,TS
	negl TS
5:	movl TOS(2),edx
	incl ecx	//;flag NZ, for m/ &c
	ret
6://<;div/1) TS=edx=d.hi, TOS(2)=d.lo
	xorl edx,edx	//;rmd
	movl TOS(2),ecx	//;d.lo
	movl edx,TOS(2)	//;->rmd
	movl ecx,TOS(1)	//;->d.lo
	jmp 7b		//; apply sign, ret
.endif # _dx_ = 0
// ( n1 n2 -- n3 n4 ) ; '/mod'
Xbym:	pushl NXL
Cbym:	movl TS,ecx	//<;Xby)
	movl TOS(1),TS
	sarl $31,TS
	spush
	movl ecx,TS	//:contd: Cmby
// ( d n -- r q ) 'm/'
Cmby:	call Cmdbym	//:Cbym:<:Cmulbymod:
	movzbl TOS(1),ecx
.ifeq _dx_
	jz 1f		//;ovf, div/0
.endif
	movl TOS(1),TS
1:	TOSPULL(1)
	orl ecx,TS
	ret
// ( d n -- r q ) ; 'm/' double by sing, ret sings
Xmby:	pushl NXL
	jmp Cmby
// ( n1 n2 n3 -- n4 n5 ) ; '*/mod'
Xmulbymod:
	pushl NXL
Cmulbymod:		//<;Cmulby)
	movl TS,ecx
	movl TOS(1),TS
	imull TOS(2)	//;signed
	movl TS,TOS(2)
	movl edx,TOS(1)
	movl ecx,TS
	jmp Cmby
// ( n1 n2 -- rmd ) ; 'mod'
Xmod:	call Cbym
	jmp Xdrop
// ( n1 n2 n3 -- n4 ) "*/"
Xmulby:	call Cmulbymod
	jmp Xsdrop	//>;Xmul)
// ( n1 n2 -- n3 )
Xby:	call Cbym
	jmp Xsdrop	//>;Xmul)

//; ---- other alu ----

// ( n1 -- n2 ) ; 'not' one's complement
Xnot:	notl TS
	NX
// ( n1 -- n2 ) ; { 1 and } true flag if n1 odd num.
Xand1:	andl $1,TS
	NX
// ( n1 n2 -- n3 ); 'and'
Xand:	andl TOS(1),TS
	jmp Xsdrop
// ( n1 n2 -- n3 ) ; 'or'
Xorx:	orl TOS(1),TS
	jmp Xsdrop
// ( n1 n2 -- n3 ) ; 'xor'
Xxorx:	xorl TOS(1),TS
	jmp Xsdrop
// ( n1 n2 -- n3 ) ; '-'
Xdash:	negl TS
Xplus:	addl TOS(1),TS	//; '+'
Xsdrop:	TOSPULL(1)	//<;Xxorc,Xorx,Xand,Xby,Xmulby) 'sdrop' { swap drop }
	NX
// ( n1 n2 -- n3 ) ; swap-
Xswapm:	negl TOS(1)
	jmp Xplus
// ( n -- flg ) ; '0>'
Xzgreat:negl TS
// ( n -- flg ) ; '0<'
Xzless:	shrl $31,TS
	NX
// ( d1 d2 -- d3 ) ; 'd-'
Xdsub:	negl TOS(1)
	adcl $0,TS
	negl TS
// ( d1 d2 -- d3 ) ; 'd+'
Xdplus:	pushl NXL
Cdplus:	movl TOS(1),T
	addl T,TOS(3)
	adcl TOS(2),TS
	TOSPULL(2)
	ret
Xdsgn:	pushl NXL
Cdsgn:	movl TS,ecx
	spop
	orl ecx,TS
	jz 1f
	movl ecx,TS
	sarl $30,TS
	orl $1,TS
1:	ret
.ifne _dx_
Xd1m:	subl $1,TOS(1)
	sbbl $0,TS
	NX
// ( d1 d2 -- flg ) ; d<
Xdless:	movl TOS(3),ecx	//; 1l
	movl TOS(2),edx	//; 1h
	cmpl TOS(1),ecx	//; 1l-2l
	sbbl eax,edx	//; 1h-2h
	TOSPULL(3)
	movl edx,eax
	jmp Xzless
// ( d1 -- d1 f ) ; ?d0=
Xqdz:	movl TOS(1),ecx
	SPUSH(2)
	movl ecx,TOS(1)
// ( d1 -- f ) ; d0=
Xdz:	spush
	xorl TS,TS
	spush
// ( d1 d2 -- f ) ; d=
Xdeq:	movl TOS(1),ecx
	cmpl TOS(2),eax
	movl $0,eax
	jnz 1f
	cmp TOS(3),ecx
	jnz 1f
	incl eax
1:	TOSPULL(3)
	NX
// ( ud1 ud2 -- f ) ; 'du>'
Xdug:	pushl NXL
Cdug:	call Cswap2
	jmp Cdul
// ( ud1 ud2 -- f ) ; 'du<'
Xduless:pushl NXL
Cdul:	movl TOS(3),ecx	//; 1l
	movl TOS(2),edx	//; 1h
	cmpl TOS(1),ecx	//; 1l-2l
	sbbl TS,edx	//; 1h-2h
	TOSPULL(3)
	sbbl TS,TS
	negl TS
	ret
.endif # _dx_
// ( d n -- d' ) ; 'd+-' apply sign of a sing to a double
Xdplusdash:
	testl TS,TS
	spop
	jns 1f		//:contd: Xdminus
// ( d -- d' ) ; 'dminus' negate double
Xdminus:negl TOS(1)
	adcl $0,TS
	negl TS
1:	NX
// ( n -- +n ) ; 'abs' s->d >rr xor r> - ;
Xabs:	movl TS,ecx
	sarl $31,ecx
	xorl ecx,TS
	subl ecx,TS
	NX
// ( n1 -- n2 ) ; '-1^'
Xm1p:	shrl $1,TS
	sbb TS,TS
	leal 1(TS,TS),TS
	NX
// ( n1 n -- n1' ) ; '+-'
Xplusdash:
	testl TS,TS
	spop
	js Xminus
	NX
// ( n -- n flg ) ; '?0=' { dup 0= }
Xzeqq:	spush
// ( n -- flg ) ; '0='
Xzeq:	cmpl $1,TS //; '0='
1:	sbbl TS,TS
Xminus:	negl TS //; 'minus' negate
	NX
// ( n -- flg ) ; '-0='
Xmzeq:	addl TS,TS
	jnc 1b
	jmp Xzeq
// ( n1 n2 -- flg ) ; '='
Xeq:	subl TOS(1),TS
	TOSPULL(1)
	jmp Xzeq
// ( n -- 0|-1 ) ; '0=/='
Xzneq:	cmpl $1,TS	//; Cy if TS=0
	sbbl TS,TS	//; -1 
	incl TS
	NX
// ( n1 n2 -- n1 n3 ) ; 'over-'
Xoverdash:
	subl TOS(1),TS
	NX
// ( n1 n2 -- n3 n1 ) ; 'bounds'
Xbounds:xaddl TS,TOS(1)
	NX
// ( n -- n' ) ; swap bytes
Xbswap:	bswap TS
	NX
// ( n1 n2 -- n )
Xmin:	movl TS,T
	spop
	cmpl TS,T
	jge 2f
1:	movl T,TS
2:	NX
// ( n1 n2 -- n )
Xmax:	movl TS,T
	spop
	cmpl TS,T
	jg 1b
	NX
.ifne _dx_
// ( n -- q )
Xs2q:	spush
	sarl $31,TS	//:contd: Xd2q)
// ( d -- q )
Xd2q:	spush		//<Xs2q)
	sarl $31,TS	//:contd: Xs2d)
.endif
// ( n -- d )
Xs2d:	spush		//<Xd2q)
	sarl $31,TS
	NX
// ( c -- n ) ; 'c->s'
Xc2s:	movsbl TSl,TS
	NX
.ifne _al_
// ( n -- +n ) ; 'abs' s->d >rr xor r> - ;
X1abs:	movl TS,ecx
	sarl $31,ecx
	xorl ecx,TS
	NX
// ( n1 n2 -- n3 ) ; align n1 to equal or greater multiple of n2, n3:=0 if n2 not a power of two
Xali:	movl TOS(1),ecx
	leal -1(TS,ecx),ecx
	call Cpow2q
	jz 1f
	movl edx,TS
	negl TS
	andl ecx,TS
1:	TOSPULL(1)
3:	NX
// ( n -- f ) ; tf if n is a power of two
Xpow2q:	pushl NXL
Cpow2q:	movl TS,edx	//<;Xali)
	subl $1,TS
	jc 1f		//;?; n = 0
	jns 2f		//;?; +ve or 80000000h
	notl edx
	notl TS
2:	andl edx,TS
1:	movl $-1,TS
	jnz 3f		//;?; not a power of 2
	incl TS		//; S flag!
3:	incl TS
	ret
// ( +n -- dn ) ; dn := +n-th power of 2
Xnp2:	pushl NXL
Cnp2:	xorl edx,edx
	movl TS,ecx
	leal 1(edx),TS
	shll cl,TS
	spush
	subl $32,ecx
	jng 1f
	movl edx,TOS(1)
	movl TS,edx
1:	movl edx,TS
	ret
.endif # _al_
// ( u1 n -- u3 ) ; unsigned mul by 2^n
Xshleft:movl TS,ecx
	spop
	shll cl,TS
3:	NX
// ( u1 n -- u3 ) ;unsigned div by 2^n
Xshright:
	movl TS,ecx
	spop
	shrl cl,TS
	NX
.ifne _sb_
// ; (f8) scaling shift count of a +ve double:
// ( d1 -- d2 n ) ; find top set bit and cell, complement less 2, +ve or zero, is max scaling power of 2
Xdslb:	pushl NXL
	movl PS,edx
//:e:			//<;dvsqrt 2nd)
	spush
//:n:			//<;dvsqrt 1st)
	xorl ecx,ecx
	leal 29(ecx),TS	//; 63 bits max, less sign bit, less start bsr:0 vs. shift:1
	cmpl (edx),ecx	//; @@tos
	jnz 3f
	leal -4(edx),edx
	leal 32(eax),eax
3:	bsrl (edx),ecx
	subl ecx,eax
	jns 4f
	xorl TS,TS
4:	ret
//
crlsh:	movb al,cl	//<;X2lsh,X2rsh)
	movb al,ch
	movl TOS(2),edx
	spop
	subb $32,ch
	jna 1f
	movb ch,cl
1:	ret
// ( ud1 -- ud2 ) ; unsigned mul by 2
Xd2mul:	pushl $1
	spush
	popl TS
// ( ud1 n -- ud2 ) ; mul by 2^n
X2lsh:	call crlsh
	js 1f
	movl edx,TS
	xorl edx,edx
1:	shldl cl,edx,TS	//;(shld count,src,dst)
	shll cl,edx
	movl edx,TOS(1)
	NX
// ( ud1 n -- ud2 ) ;signed div by 2^n, floored
Xa2rsh:	call crlsh
	js 1f
	xorl TS,TS
1:	shrdl cl,TS,edx
	sarl cl,TS
	movl edx,TOS(1)
	NX
// ( ud1 n -- ud2 ) ;unsigned div by 2^n, floored
X2rsh:	call crlsh
	js 1f
	xorl TS,TS
1:	shrdl cl,TS,edx
	shrl cl,TS
	movl edx,TOS(1)
	NX
// ( ud1 -- ud2 ) ; 'd2/' signed div by 2, round near zero
Xd2div:	sarl $1,TS
	rcrl $1,TOS(1)
	jnc 1f
	test eax,eax
	jns 1f		//:contd: Xd1p
.endif # _sb_
// ( ud1 -- ud2 ) ; 'd1+'
Xd1p:	addl $1,TOS(1)	//<Xd2div
	adcl $0,TS
1:	NX
// ( n1 -- n2 )
Xp8:	addl $3,TS	//; 'l>pfa'
Xp7:	incl TS		//; 'brchan'
Xp4:	addl $2,TS
Xp2:	incl TS
Xp1:	incl TS
	NX
// ( n1 -- n2 )
Xm8:	subl $4,TS
Xm4:	decl TS
Xm3:	decl TS
Xm2:	decl TS
Xm1:	decl TS
	NX
// ( n1 -- n2 )
_rx Xmul16:addl TS,TS
_rx Xmul8: addl TS,TS
Xmul4:	addl TS,TS
Xmul2:	addl TS,TS
	NX
// ( n1 -- n2 )
Xmul3:	leal (TS,TS,2),TS
	NX
// ( n1 -- n2 )
_rx Xby16:	shrl $1,TS
_rx Xby8:	shrl $1,TS
Xby4:	shrl $1,TS
Xby2:	shrl $1,TS
	NX
// ( n -- +n|0 )
X0max:	testl TS,TS
	jl Xand0
	NX
// ( d1 -- 1|0|-1 )
Xsgn:	testl TS,TS
	jz 1f
	sarl $31,TS
	leal 1(,TS,2),TS
1:	NX
// ( n1 n2 -- flg )
Xless:	cmpl TOS(1),TS
        jg pop1
pop0:	TOSPULL(1)	//<-:Xgreat::Xuless:-.
Xand0:	xorl TS,TS	//<-:X0max:-''
	NX
// ( n1 n2 -- flg )
Xgreat:	cmpl TOS(1),TS
	jge pop0	//;?; -'
pop1:	TOSPULL(1)	//<-:Xless::Xuless:
Xor1:	movl $1,TS	//<-:Xone::Xlwordq:
	NX
// ( n1 n2 -- flg )
Xne:	cmpl TOS(1),TS
	jne pop1	//; -'
	jmp pop0	//; -'
// ( x1 x2 x3 -- flg ) ; 'within?'
Xwithinq:subl TOS(1),TS	//; over-
	movl TS,ecx	//; >r
	spop
	subl TS,TOS(1)	//; -
	movl ecx,TS	//; r> u< ;
// ( n1 n2 -- flg )
Xuless:	cmpl TOS(1),TS
	ja pop1		//; -'
	jmp pop0	//; -'
// ( -- 1 )
Xone:	spush
	jmp Xor1
// ( n a -- 0|-1 )
Xbit:	movl TOS(1),ecx
	TOSPULL(1)
	btl ecx,(TS)
	sbbl TS,TS
	negl TS
	NX
// ( x1 x2 x3 -- x4 );F6: 'within' adjust to bounds if not within
Xwithin:pushl NXL	//;F6: d0=TS:max d1=T1:min d2=T2:num
#define d0 TS
#define d1 TOS(1)
#define d2 TOS(2)
#define d4 edx
Cwithin:movl d1,d4	//;move.w  d1,d4 d4=D
	cmpl d2,d0	//;cmp.w   d2,d0
	jl 1f           //;blt.s   wput    ? max <= num
	movl d2,ecx	//;exg     d2,d0   d0:num d2:max
	movl d0,d2	//;TS:=num d2
	movl ecx,d0	//;T2:=max d0
	movl ecx,d4	//;move.w  d0,d4   d0->D
	cmpl d0,d1	//;cmp.w   d0,d1
	jle 1f		//;ble.s   wput    ? min <= num
	movl d2,d4	//;move.w  d2,d4
	movl d1,d0	//;move.w  d1,d0   min
1:	TOSPULL(2)
	ret
// ( x1 x2 x3 -- x4 );F6: 'wraps' adjust to opposite bounds if not within
Xwraps:	call Cwithin	//>
	movl edx,TS
	NX
.nolist
// -----------------------------------------------------------------------------
// fthalu.S <eof>
