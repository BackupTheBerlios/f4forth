//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; qarith.S - provisional for rational numbers by pairs of fdoubles
// =============================================================================
.psize 0
.att_syntax noprefix
.text
.list
.ifne (_fc_|_sq_|_dx_|_mx_)
//;
//; ---- optional doubles & quad ----
//;
.ifne _fc_
// ( u -- ud ) ;'factorial' (ex L4)
Xfactorial:
	spush
	leal fact,ecx
	movl 4(ecx,eax,8),edx
	cmpl $21,eax
	movl (ecx,eax,8),eax
	jc 1f
	xorl edx,edx
	movl edx,TS
1:	movl edx,TOS(1)
	NX
.endif # _fc_
.ifne _sq_
// ( n1 n2 -- u3 ) gcd
Xgcd:	movl TS,edx	//;r := nom
	mov TOS(1),ecx	//;n
	TOSPULL(1)
	testl TS,TS	//;m
	jns 1f		//; abs swap
	negl edx
1:	testl ecx,ecx	//; abs
	jz 2f		//;?; ret den if nom = 0
	jns 3f
	negl ecx
	jmp 3f		//; enter begin
4:	movl edx,ecx	//;d:=n:=r
	xorl edx,edx
	divl ecx	//;d:=r'
3:	mov ecx,eax	//;m:=n
	testl edx,edx	//;r
	jnz 4b		//; -dup until
2:	NX
// ( d -- q ) ; 'd^2', q := d^2 = lo^2 + 2^32*(lo*hi)^2 + 2^64*hi^2
Xdsq:	testl TS,TS
	jns 1f
	negl TOS(1)
	adcl $0,TS
	negl TS
1:	movl TOS(1),ecx	//;ecx := nnl
	spush
	spush		//; nnl nnh nnh nnh 
	movl ecx,TS
	mull TS		//; nnl nnh nnh pll; ecx=nnl, edx=plh
	movl TS,TOS(3)	//; 
	movl edx,TOS(2)	//; PLL plh nnh pll
	movl ecx,TS	//;TS := nnl
	movl TOS(1),ecx	//;ecx:= nnh
	mull ecx	//; nnl*nnh
	shll $1,TS
	adcl edx,edx	//; 2*nnl*nnh
	addl TS,TOS(2)
	adcl $0,edx
	movl edx,TOS(1)	//; Pll PLH phl xxx
	movl ecx,TS	//;nnh
	sbbl ecx,ecx	//; 2nd carry
	mull TS		//; PLL PLH phl1 phl2; edx=phh2
	negl ecx
	addl TS,TOS(1)
	adcl ecx,edx
	movl edx,TS
	NX
// ( n1 -- n2 ) ; '^2'; n2 := n1*n1 = n1^2
Xsq:	spush
	imull TS
1:	movl TS,TOS(1)
	movl edx,TS
	NX
// ( u1 -- u2 ) ; 'u^2'; u2 := u1^2
Xusq:	spush
	mull TS
	jmp 1b
// ( u1 u2 -- d3 ) ; 'u^'
Xupow:	movl TS,ecx
	decl ecx
	js 1f		//; u1^0, u1^(-ve)
	movl ecx,edx	//;d:=u2-1
	jz 2f		//; u1 ^ 1
	movl TOS(1),TS
	xorl edx,edx
	cmpl $1,TS
	jna 2f		//;?; 1^u2=1 or 0^u2=0; u2>0
	pushl ebx
	movl TS,ebx	//; exp
	xorl TS,TS
3:	mull ebx	//; a:=hl d:= hh
	movl eax,edx	//; p.hl
	movl TOS(1),eax	//;   ll
	movl edx,TOS(1)	//; p.hl
	mull ebx
	addl TOS(1),edx	//; p.h
	movl eax,TOS(1)	//; p.l
	movl edx,eax
	decl ecx
	jg 3b
	popl ebx
2:	movl edx,TS
	NX
1:	xorl TS,TS
	movl TS,TOS(1)
	incl ecx
	jnz 4f		//; n1^(-n2) < 1 := 0
	incl TOS(1)
4:	NX
// ( ud -- ud jf ) ; 'udsqrt'
Xudsqrt:spush
	sarl $31,TS
	spush		//; s->d (for 'j')
	call Cuqsqrt	//; ret ( uq flg --> ud flg )
	TOSPULL(2)
	NX
// ( uq -- ud jf ) ; 'uqsqrt' (ex L4)
Xuqsqrt:pushl NXL
Cuqsqrt:movl TS,ecx
	sarl $31,ecx
	leal 1(,ecx,2),ecx//; "j"
	pushl ebx
	pushl ebp
	pushl ecx	//; save sign for 'j'
	jns 1f
	call Cqminus
1:	spush
	xorl TS,TS
	SPUSH(12)	//; p:=0, a:=0, t:=0, workspace cell @tos
	pushl $63	//; bitcount/2
// ;tos+cells: 16)qx 12)qa 8)qp 4)qt 0)xx | j
// ; shift left x into a 2 places ( 16)qx 12)qa 8)qp 4)qt 0)xx -- qx/4 qa' qp xx
2:	movl TOS(9),eax	//; a.hh
	movl TOS(10),ecx//; a.hl
	shldl $2,ecx,eax
	movl eax,TOS(9)
	movl ecx,eax
	movl TOS(11),ecx
	shldl $2,ecx,eax
	movl eax,TOS(10)
	movl ecx,eax
	movl TOS(12),ecx
	shldl $2,ecx,eax
	movl eax,TOS(11)
	movl ecx,eax
	movl TOS(13),ecx
	shldl $2,ecx,eax
	movl eax,TOS(12)
	movl ecx,eax
	movl TOS(14),ecx
	shldl $2,ecx,eax
	movl eax,TOS(13)
	movl ecx,eax
	movl TOS(15),ecx
	shldl $2,ecx,eax
	movl eax,TOS(14)
	movl ecx,eax
	movl TOS(16),ecx
	shldl $2,ecx,eax
	movl eax,TOS(15)
	shll $2,ecx
	movl ecx,TOS(16)
// ; shift left p one place ;	( 16)qx 12)qa 8)qp 4)qt 0)xx -- qx/4 qa' qp*2 qp*4 xx
	movl TOS(5),eax	//; p.hh
	movl TOS(6),ecx	//; p.hl
	movl eax,edx	//; copy for t
	shldl $1,ecx,eax//; p:=2p
	movl eax,TOS(5)
	shldl $2,ecx,edx//; t:=4p
	movl edx,TOS(1)
	movl edx,ebx	//; t.hh
	movl ecx,eax
	movl TOS(7),ecx
	movl eax,edx
	shldl $1,ecx,eax
	movl eax,TOS(6)
	shldl $2,ecx,edx
	movl edx,TOS(2)
	movl ecx,eax
	movl edx,ebp	//; t.hl
	movl TOS(8),ecx
	movl eax,edx
	shldl $1,ecx,eax
	movl eax,TOS(7)
	shldl $2,ecx,edx
	movl edx,TOS(3)	//; t.lh
	addl ecx,ecx
	movl ecx,TOS(8)
	addl ecx,ecx
	movl ecx,TOS(4)	//; t.ll
// ;tos+cells: 16)qx 12)qa 8)qp 4)qt 0)xx | j
// ; if a > t then a:=a-(t+1); p:=p+1
// ; u> if ...
// ;tos+cells: 16)qx 12)qa 8)qp 4)qt 0)xx | j
	cmpl TOS(9),ebx
	jnz 3f
	cmpl TOS(10),ebp
	jnz 3f
	cmpl TOS(11),edx
	jnz 3f
	cmpl TOS(12),ecx
3:	jnc 4f		//;?;
// ;tos+cells: 15)qx 11)qa 7)qp 3)qt | j
	notl ecx	//; t:=-(t+1)
	notl edx
	notl ebp
	notl ebx
// ; 15)qx 11)qp 7)-(t+1) 3)qa
	addl ecx,TOS(12)//; a:=a+t=a-(4*p+1)
	adcl edx,TOS(11)
	adcl ebp,TOS(10)
	adcl ebx,TOS(9)
	addl $1,TOS(8)	//; p:=p+1
	adcl $0,TOS(7)
	adcl $0,TOS(6)
	adcl $0,TOS(5)
4:	decl (esp)
	jns 2b
// ; tos+cells: 16)qx 12)qa 8)qp 4)qt 0)xx | j
	movl TOS(5),eax	//; copy p
	movl TOS(6),ebp
	movl TOS(7),ecx
	movl TOS(8),edx
	TOSPULL(12)
	lea 4(esp),esp	//; drop counter
	movl eax,TOS(1)	//; result to @tos
	movl ebp,TOS(2)
	popl eax	//; q1 sign cell
	popl ebp
	popl ebx
	movl ecx,TOS(3)
	movl edx,TOS(4)
	ret
.endif # _sq_
.ifne _dx_
// ( d1 d2 -- d3 ) ; 'dgcd'
Xdgcd:	call Cdabs	//; abs
	call Cswap2	//; swap
	call Cdabs	//; abs
	jmp 1f		//; ENTER BEGIN
2:	call Csover2	//;  swap over
	call Cdbym	//;  /mod
	SPOP(2)		//;  drop
1:	movl TS,ecx	//; ENTRY -dup
	orl TOS(1),ecx	//;  0=
	jnz 2b		//; until
	SPOP(2)		//; ;s
	NX
// ( d1 d2 d3 -- dr dq ) ; 'd*/mod '
Xdmbym:	pushl $Xdrop2
	pushl TOS(1)
	pushl TS
	SPOP(2)
	call Cmdmul// ( d1 d2 -- q3 ) ; 'md*'
	SPUSH(2)
	popl TS
	popl TOS(1)
	jmp Cqmbym
// ( q d -- dr qq ) ; qm/mod ; +/+ q+,r+ ; +/- q-,r+ ; -/- q+,r- ; -/+ q-,r-
Xqmbym:	pushl NXL
Cqmbym:	movl TOS(1),ecx
	movl TS,edx
	orl ecx,edx
	jz 1f		//;?; div 0 :ok:
	decl edx
	jnz 2f		//;?; not div 1
	decl ecx
	jz 3f		//>Cuqmbym) ;?; div 1
2:	movl TS,edx	//; dvs
	movl TOS(2),ecx	//;dvd
	xorl  ecx,edx
	pushl ecx	//; sg(rmd)
	pushl edx	//; sg(qot)
	testl ecx,ecx
	jns 6f
	pushl TS
	pushl TOS(1)
	SPOP(2)
	call Cqminus
	spush
	popl TS
	spush
	popl TS
6:	testl TS,TS
	jns 7f
	call Cdminus
7:	call Cuqmbym
	popl ecx
	testl ecx,ecx
	jns 8f
	call Cqminus
8:	popl ecx
	testl ecx,ecx
	jns 9f
	negl TOS(5)
	adcl $0,TOS(4)
	negl TOS(4)
9:	ret
// ; div-0
1:	movl TOS(2),edx	//;sg(dvd)
	movl edx,ecx
	orl TOS(3),edx
	orl $1,ecx	//;flag NZ
	orl TOS(4),edx
	orl TOS(5),edx
	jnz 2f		//;?; non-zero dvd
	xorl ecx,ecx
2:	call 1f		//>Cuqmbym) ;?; fetch -1,0
	testl ecx,ecx
	jz 4f		//;?; 0/0 ret 1,0
	shrl $1,TS
	testl ecx,ecx
4:	jng Cqminus	//;?; -ve, ret $80..01
	ret		//; +ve, ret $7ff..ff
// ( uq ud -- dr qq ) ; uqm/mod
Xuqmbym:pushl NXL
Cuqmbym:movl TOS(1),ecx	//<Cqmbym)
	movl TS,edx
	orl ecx,edx
	jz 1f		//;?; div 0, ret ff..ff,0
	decl edx
	jnz 2f		//;?; not div 1
	decl ecx
	jz 3f		//;?; div 1
2:	movl TOS(2),edx	//; dvd.hh
	movl TOS(3),ecx	//; dvd.hl
	orl edx,ecx	//; m.s. dvd double
	jz 4f		//;?; qot in doubles' range
	testl edx,edx	//; dvd.hh
	jnz 5f		//;?; uncertain
	test eax,eax	//; dvs.h
	jnz 4f		//;?; qot not above double size
5:	call Cdup4	//;4dup  qd1l qd1h ud2 ud1h ud2
	SPUSH(2)	//; qd1l qd1h ud2 ud1h ud2 dxx
	movl $0,edx
	movl TOS(2),eax	//; d2h<-
	movl TOS(3),ecx	//; d2l
	movl edx,TOS(2)
	movl edx,TOS(3)	//; q1h' := qd1h|00
	movl ecx,TOS(1)	//; d2l<-
	call Cuqdbym	//; qd1l qd1h ud2 drh dqh
	movl TOS(1),ecx	//; dqhl
	movl TOS(8),edx	//; qdlh
	movl eax,TOS(6)	//;			; qdhh<-	; save dqh
	movl ecx,TOS(7)	//; qd1l dqh: ud2 drh dqh	; dqhl<-
	movl TOS(4),eax	//; d2h<-
	movl TOS(5),ecx	//; d2l
	movl edx,TOS(4)	//; qdlh<-
	movl TOS(9),edx	//; qdll
	movl ecx,TOS(1)	//; d2l<-
	movl edx,TOS(5)	//; qd1l dqh: qdl: drh ud2:
	call Cuqdbym	//; qdll dqh  drl  dql
	call Cswap4	//; drl  dql  qdll dqh 
	jmp Csdrop2	//; drl  dql  dqh 
3://<Cqmbym); dvs = 1 -> rmd:=0, qot:=q1
	movl ecx,TOS(1)
	jmp Cmrot2
1://; dvs = 0, ret ff..ff,0
	movl eax,TOS(5)
	movl eax,TOS(4)
	decl eax
	movl eax,TOS(3)
	movl eax,TOS(2)
	movl eax,TOS(1)
	ret
4://; quad in range of a double
	call Cuqdbym
	spush
	movl $0,eax
	spush
	ret
    .ifne _hx_
// ( ud1 ud2 -- ud3 ud4 ) ; { ud/mod 2drop }
Xudmod:	pushl $Xdrop2
	jmp Cudbym
// ( ud1 ud2 -- ud3 ud4 ) ; { ud/mod 2sdrop }
Xudby:	pushl $Xsdrop2
	jmp Cudbym
    .endif # _hx_
// ( ud1 ud2 -- ud3 ud4 ) ; { 0. 2swap uqd/mod }
Xudbym:	pushl NXL
Cudbym:	movl TS,ecx	//; h2
	xorl TS,TS
	movl TOS(1),edx	//; l2
	movl TS,TOS(1)	//; h1l := 0, h1h := 0
	spush
	movl edx,TS	//; l2
	spush
	movl ecx,TS	//; h2
	jmp Cuqdbym
// ( ud1 ud2 ud3 -- udr udq ) ; 'ud*/mod '
Xudmbym:pushl TOS(1)
	pushl TS
	SPOP(2)
	call Cumdmul// ( d1 d2 -- q3 ) ; 'umd*'
	SPUSH(2)
	popl TS
	popl TOS(1)	//:cntd: Xuqdbym)
// ( uq1 ud2 -- ud3 ud4 ) ; 'uqd/mod' (ex lib4th)
Xuqdbym:pushl NXL	//<Xudmbym)
Cuqdbym:pushl ebp	//<Cqfmbym,Cdbym,Cuqmbym)
	pushl ebx
	pushl esi
	pushl edi	//;@tor := PS
	movl eax,ebp	//;B: dvs	h
	movl TOS(1),edx	//;d:	l
	movl TOS(2),eax	//;a: dvd	hh
	movl TOS(3),ebx	//;b:	hl
	movl TOS(5),esi	//;S:	ll
	movl TOS(4),edi	//;D:	lh
	cmp eax,ebp	//;sh-dhh
	jc 1f		//;?; hi dvs > dvd
	jz 2f		//;?; lo dvs > dvd ?
6:	movl $64,ecx
	orl eax,eax
4:	rcll $1,esi
	rcll $1,edi
	rcll $1,ebx
	rcll $1,eax
	jnc 3f		//;?; -.
8:	subl edx,ebx	//;hl-l
	sbbl ebp,eax	//;hh-h
	stc
	decl ecx
	jnz 4b		//;?; -'
	jmp 5f		//;?; q:=2q+1, done -.
2:	cmpl ebx,edx	//;sl-dhl
	ja 6b		//;?; -'
1:	movl esi,ebx	//;ll
	movl $-1,esi
	movl edi,eax	//; rh = rmd = dvd.lo
	movl esi,edi	//; qh = qot = -1
	jmp 7f		//; - ret ovf
3:	cmpl ebp,eax	//; hh'-h
	ja 8b		//;?; -'
	jc 9f		//;?; -.
	cmpl edx,ebx
	jnc 8b		//;?; -'
9:	orl eax,eax
	decl ecx
	jnz 4b		//;?; -'
5:	rcll $1,esi	//; ql
	rcll $1,edi	//; qh
7:	movl edi,ecx	//; qh
	popl edi
	movl ebx,TOS(5)	//; rl
	movl eax,TOS(4)	//; rh
	movl esi,TOS(3)	//; ql
	movl ecx,TOS(2)
	TOSPULL(2)
	popl esi
	popl ebx
	popl ebp
	movl ecx,TS
	ret
// ( d1 d2 -- d1r d2q ) ; floored div doubles, ret double rem & qot
Xdfbym:	pushl NXL
Cdfbym:	movl TOS(2),ecx	//; h1
	movl TOS(1),edx	//; l2
	sarl $31,ecx
	pushl TS	//; h2
	movl ecx,TOS(1)
	movl ecx,TS
	spush
	movl edx,TS
	spush
	popl TS
	jmp Cqfmbym
// ( qn1 dn2 -- d1r d2q ) ; floored div quad by double, ret double rem & qot
Xqfmbym:pushl NXL
Cqfmbym:movl TS,ecx	//<Cdfbym)
	movl TS,edx
	xorl TOS(2),ecx
	orl TOS(1),edx
	jz cdivz	//;?; div-0 :ok:
	pushl eax	//; sign of rmd
	pushl ecx	//; sign of qot
	call Cmrot2	//; save dvd for -ve qot floored adjustmt
	call Cqabs
	movl TOS(5),ecx	//; dvs dvd dvs	{ 5 pick 5 pick } = {third2}
	spush
	movl ecx,TS
	spush
	movl TOS(6),TS
	testl TS,TS	//; dabs
	jns 0f
	call Cdminus	//; dminus
0:	call Cuqdbym	//; uqd/mod -> dr dq
	popl ecx	//; sgn(q)
	testl ecx,ecx
	jns 2f		//;?; +ve quot
	notl TOS(1)	//; 2not
	notl eax	//; qot := -qot-1
	call Cmrot2	// ; ( d1 d2 d3 -- d3 d1 d2 )
	popl edx	//; sgn(r)
	testl edx,edx
	js 3f		//;?; +ve rmd
	call Cdminus	//; r := -r
3:	call Cdplus	//; r := -(r-dvs)
	jmp Cswap2
2:	popl edx	//; sgn(r)
	testl edx,edx
	jns 4f		//;?; +ve rmd
	call Cdminusn	//; 2swap dminus 2swap
4:	call 9f		//; 2rot 2drop <- 4 roll drop 4 roll drop
9:	spush
	movl $4,TS
	call Croll	//; 4 roll drop
	spop
	ret
// ;div-0, i: ecx=dvs.hi xor dvd.hi
cdbyz:	spush;spush	//<Cdbym) ; d/mod (all double)
cdivz:	sarl $31,ecx	//<Cqbym) ; (all quad)
	jnz 5f		//;?; -ve ovf
	orl TOS(2),eax
	orl TOS(3),eax
	orl TOS(4),eax
	orl TOS(5),eax
	jnz 5f		//;?; +ve ovf
	incl eax	//; 0/0
	movl ecx,TOS(5)
	movl ecx,TOS(4)
	movl eax,TOS(3)
	xorl eax,eax	//; ret 1,0
	TOSPULL(2)
	ret
5://; 0/(=/=0): +ve q:=r:=7fff.ffff; -ve q:=r:=8000.0001
	notl ecx		//; 0> 2* 1+ $80000000 + 
	leal 0x80000001(,ecx,2),ecx
	TOSPULL(2)
	movl ecx,eax
	movb ah,al	//; 7fffffff or 80000000
	movl eax,TOS(2)
	addl ecx,ecx
	sarl $1,ecx	//; ffffffff or 00000001
	movl ecx,TOS(1)
	movl ecx,TOS(3)
	ret
Cswap4:	pushl eax
	pushl TOS(1)
	pushl TOS(2)
	pushl TOS(3)
	movl TOS(5),eax
	movl TOS(6),ecx
	movl TOS(7),edx
	movl eax,TOS(1)
	movl ecx,TOS(2)
	movl edx,TOS(3)
	movl TOS(4),TS
	popl TOS(7)
	popl TOS(6)
	popl TOS(5)
	popl TOS(4)
	ret
// ( q n -- q' ); q+- ; q' := q*sgn(n)
Cqpm:	testl TS,TS
	spop
	js Cqminus
	ret
// ( q -- q' ) ; qabs
Cqabs:	testl TS,TS	//<Cqfmbym)
	jns 8f
// ( q -- q' ) ; qminus
Cqminus:call Cqnot	//<Cqabs,Cqpm)
// ( q -- q' ) ; q1+
Cq1p:	addl $1,TOS(3)	//<Cqminus)
jo1p:	adcl $0,TOS(2)
	adcl $0,TOS(1)
	adcl $0,eax
8:	ret		//<Cqabs)
// ( d -- d' ) ; 4not
Cqnot:	notl TOS(3)	//<Cqminus)
	notl TOS(2)
1:	notl TOS(1)	//<Xnot2)
	notl eax
	ret
// ( o -- o' ) ;
Xominus:pushl NXL
Cominus:call Cnot8
Co1p:	call Cq1pn
	adcl $0,TOS(3)
	jmp jo1p
// ( d -- ~d ) ; 2not
Xnot2:	pushl NXL
	jmp 1b		//>Cqnot)
Cnot8:	call Cqnot	//:c: Cnot4n
Cnot4n:	notl TOS(7)
	notl TOS(6)
	notl TOS(5)
	notl TOS(4)
	ret
Cqnegaten:		//; 'qminus' negate quad @nos
	notl TOS(7)
	notl TOS(6)
	notl TOS(5)
	notl TOS(4)	//:o: Cq1pn
Cq1pn:	addl $1,TOS(7)	//; 'q1+'
	adcl $0,TOS(6)
	adcl $0,TOS(5)
	adcl $0,TOS(4)
	ret
    .ifne _mx_
// ( o -- o' ) ; 'o1+'
_hx Xo1p: pushl NXL
_hx	jmp Co1p
// ( q1 q2 -- q1' q2 ) ; 'nos-q1+'
Xq1pn:	pushl NXL
	jmp Cq1pn
// ( q -- q' ) ; 'nos-qminus'
Xqnegaten:
	pushl NXL
	jmp Cqnegaten
// ( q1 q2 -- q1' q2 ) ; 'nos-4not'
Xnot4n:	pushl NXL
	jmp Cnot4n
// ( o -- o' ) ; '8not'
Xnot8:	pushl NXL
	jmp Cnot8
// ( q -- q' ) ; 'qabs'
Xqabs:	pushl NXL
	jmp Cqabs
// ( q -- q' ) ; 'q1+'
Xq1p:	pushl NXL
	jmp Cq1p
// ( q -- q' ) ; '4not'
Xqnot:	pushl NXL
	jmp Cqnot
// ( q1 q2 -- q2 q1 ) ; '4swap'
Xswap4:	pushl NXL
	jmp Cswap4
// ( q n -- q' ); q+- ; q' := q*sgn(n)
Xqpm:	pushl NXL
	jmp Cqpm
// ( q -- q' ) ; q2/
Xq2div:	sarl $1,TS
	rcrl $1,TOS(1)
	rcrl $1,TOS(2)
	rcrl $1,TOS(3)
	jc Xq1p
	test eax,eax
	js Xq1p
	NX
// ( q -- q' ) ; q2*
Xq2mul:	shll $1,TOS(3)
	rcll $1,TOS(2)
	rcll $1,TOS(1)
	rcll $1,TS
3:	NX		//<Xqmpow)
// ( n1 +n2 -- q ) ; 'mq^'
Xmqpow:	movl TOS(1),ecx
	xorl edx,edx
	testl ecx,ecx
	jns 2f
	movl TS,edx
	negl TOS(1)	//; |n1|
2:	pushl edx
	movl TS,ecx
	xorl edx,edx
	testl TS,TS
	jns 1f		//;?; +ve n2
	movl edx,TOS(1)	//; force zero result 
1:	call Cumqpow
	popl ecx
	sarl $1,ecx
	jnc 3b		//>Xq2div); even or +ve :c:
// ( q -- q' ) ; 'qminus'
Xqminus:pushl NXL	//<Xqmpow)
	jmp Cqminus
    .endif # _mx_
// ( d1 d2 -- d1' d2 ) ; 'nos-dminus' negate double @nos
_hx Xdminusn:pushl NXL
Cdminusn:
	negl TOS(3)
	adcl $0,TOS(2)
	negl TOS(2)
	ret
// ( d1 d2 -- d2 )
Xdmod:	call Cdbym
	jmp Xdrop2
// ( d1 d2 -- d2 )
Xdby:	Call Cdbym
// ( d1 d2 -- d2 ) ; '2sdrop'
Xsdrop2:pushl NXL
Csdrop2:movl TOS(1),ecx
	TOSPULL(2)
	movl ecx,TOS(1)
	ret
//;
//; ---- optional quad & mixed -- _mx_ ----
//;
// ( d1 d2 -- q3 ) ; 'md*'
Xmdmul:	pushl NXL
Cmdmul:	movl eax,ecx
	xorl TOS(2),ecx
	pushl ecx	//; final negation flag
	js 1f		//;?; d1 or d2 -ve
	testl eax,eax
	jns 2f		//;?; d1 and d2 +ve
1:	call Cdabs
	call Cswap2
	call Cdabs
2:	call Cumdmul	//; ( d1 d2 -- q3
	popl ecx
	testl ecx,ecx
	js Cqminus
	ret
// ( d1 d2 -- d3 ) ; 'd*'
Xdmul:	call Cmdmul
1:	SPOP(2)
	NX
// ( d1 d2 -- d3 ) ; 'ud*'
Xudmul:	call Cumdmul
	jmp 1b
// ( ud1 ud2 -- uq3 ) ; 'umd*'
Xumdmul:pushl NXL
Cumdmul:pushl ebp	//; ( h1 l1 h2 l2 -- ll lh hl hh )
	pushl esi
	pushl ebx
	pushl eax	//;[s] := h2
	movl TOS(2),ecx	//;ecx := h1
	movl TOS(3),esi	//;esi := l1
	movl TOS(1),ebx	//;ebx := l2
	movl ebx,eax	//; a=l2
	mull esi	//; l1*l2
	movl eax,TOS(3)	//;p3 result-ll
	movl esi,eax	//; a=l1
	movl edx,esi	//;esi := l1l2o
	movl eax,edx	//; d=l1
	movl (esp),eax	//; a=h2
	mull edx	//; l1*h2
	addl eax,esi	//; l1h2+l1l2o
	adcl $0,edx	//;edx := l1h2o+cy
	movl ebx,eax	//;eax := l2
	movl edx,ebx	//;ebx := l1l2o+cy
	mull ecx	//; h1*l2
	addl eax,esi	//;esi := l1h2+l1l2o+h1l2
	adcl edx,ebx	//;ebx := l1l2o+c+h1l2o
	sbbl ebp,ebp	//;-1 = Cy
	movl ecx,eax	//;eax := h1
	negl ebp	//;ebp := Cy
	popl edx	//; h2
	mull edx	//; h1h2
	addl ebx,eax	//;p1
	adcl ebp,edx	//;p0
	movl esi,TOS(2)	//;p2
	movl eax,TOS(1)
	movl edx,eax
	popl ebx
	popl esi
	popl ebp
	ret
// ( d1 d2 -- d' ) ; 2and
Xand2:	movl TOS(1),edx
	andl TOS(2),TS
	andl edx,TOS(3)
	TOSPULL(2)
	NX
// ( d1 d2 -- d' ) ; 2xor
Xxor2:	movl TOS(1),edx
	xorl TOS(2),TS
	xorl edx,TOS(3)
	TOSPULL(2)
	NX
// ( d1 d2 -- d' ) ; 2or
Xor2:	movl TOS(1),edx
	orl TOS(2),TS
	orl edx,TOS(3)
	TOSPULL(2)
	NX
.endif # _dx_
.ifne _mx_
// ( q1 q2 -- q' ) ; q-
Xqsub:	call Cqminus
	jmp Xqplus
// ( q n -- q' ) ; qm+
Xqmplus:spush
	sarl $31,TS
	SPUSH(2)
// ( q1 q2 -- q' ) ; q+
Xqplus:	pushl NXL
Cqplus:	movl TOS(3),ecx
	movl TOS(2),edx
	addl ecx,TOS(7)
	adcl edx,TOS(6)
	movl TOS(1),ecx
	adcl ecx,TOS(5)
	adcl eax,TOS(4)
// ( q1 q2 -- q1 ) ; 4drop
Cdrop4:	SPOP(4)
	ret
// ( uq1 ud1 -- uq2 ud2 ) ; 'uqd*' ; :ok:
Xuqdmul:pushl NXL
Cuqdmul:pushl TS
	pushl TOS(1)	//; >rr
	call Cumdmul	//;  ( q1l phl.phh
	call Crot2	//;  ( phl.phh q1l
	spush
	popl eax
	spush
	popl eax	//;  ( phl.phh q1l d1
	call Cumdmul	//;  ( phl.phh pll.plh
	call Cswap4	//;  ( pll.plh phl.phh
	spush 
	xorl TS,TS	//;  ( pll.plh phl.phh 00
	spush
	call Cmrot2
	jmp Cqplus
/// ;// ( o1 o2 -- o ) ; 'o-'
/// ;Xosub:	call Cominus
// ( o1 o2 -- o ) ; 'o+'
Xoplus:	pushl NXL
	jmp Coplus
// ( q1 -- q2 ovf ); q^2
Xuqsq:	call Cdup4
// ( q1 q2 -- q2 qovf )
Xuqmul:	pushl NXL
Cuqmul:	call Cdup8	//; q1l q1h q2l q2h q1l q1h q2l q2h
	call Csdrop2	//; q1l q1h q2l q2h q1l q1h q2h
	call Cuqdmul	//; q1l q1h q2l q2h p1h.p1l.o1h		; quad by double.hi
	spush
	xorl TS,TS
	spush;spush	//; q1l q1h q2l q2h p1l.p1h.o1h.000
	movl $-3,eax	//; 0. -3 2roll
	call Croll2	//; q1l q1h q2l q2h 000 p1l p1h o1h	; mul hi.prd by 2^64
	call Cswap8	//; 000 p1l p1h o1h q1l q1h q2l q2h
	SPOP(2)		//; 000 p1l p1h o1h q1l q1h q2l
	call Cuqdmul	//; 000 p1l p1h o1l p0l.p0h.o1l		; lo.prod
	spush
	xorl TS,TS
	spush		//; 000 p1l p1h o1l p0l p0h ovf 000
Coplus:	movl TOS(7),ecx
	movl TOS(6),edx
	addl ecx,TOS(15)
	adcl edx,TOS(14)
	movl TOS(5),ecx
	movl TOS(4),edx
	adcl ecx,TOS(13)
	adcl edx,TOS(12)
	movl TOS(3),ecx
	movl TOS(2),edx
	adcl ecx,TOS(11)
	adcl edx,TOS(10)
	movl TOS(1),ecx
	adcl ecx,TOS(9)
	adcl eax,TOS(8)
	SPOP(8)
	ret
// ( q1 q2 -- q2 qovf )
Xqmul:	pushl NXL
Cqmul:	pushl TS
	testl TS,TS
	jns 1f
	call Cqminus
1:	movl TOS(4),ecx
	testl ecx,ecx
	jns 2f
	xorl ecx,(esp)
	call Cqnegaten
2:	call Cuqmul
	cmpl $0,(esp)
	leal 4(esp),esp
	js Cominus
	ret
// ( q -- q -1|0|1 ) ; '?q0='
Xqqzeq:	pushl NXL
// ; (e: Z if q = 0, ecx = sign cell | NZ*(ecx<0): q<0, NZ*((ecx>0)+(ecx=0)): q>0
Cqqzeq:	spush
	movl TS,ecx	//; sign
	movl TS,edx
	orl TOS(4),edx
	xorl TS,TS
	orl TOS(3),edx
	orl TOS(2),edx
	jnz 1f
	leal 1(TS),TS	//;preserve flags
1:	ret
// ( q1 q2 -- sgn ) ; 'qcmp' signum(q1-q2)
Xqcmp:	call Cqminus
	call Cqplus
// ( q -- 1|0|-1 ) ; 'qsgn'
// ; c: ecx,edx; eflags: NZ
Xqsgn:	pushl NXL
Cqsgn:	call Cqqzeq	//<Cuqbym)
	sarl $31,ecx
	TOSPULL(4)
	decl eax
	jz 1f		//;?; 0
	leal 1(,ecx,2),TS
1:	ret
Xozeq:	orl TOS(4),TS
	orl TOS(3),TS
	orl TOS(2),TS
	orl TOS(1),TS
	TOSPULL(4)	//:contd: Xqzeq)
// ( q -- flg ) ; 'q0='
Xqzeq:	pushl NXL	//<Xozeq)
Cqzeq:	call Cqqzeq	//<Cuqbym)
	TOSPULL(4)
	ret
// ( q -- q' ) ; q1-
Cqonem:	subl $1,TOS(3)	//<Cuqbym)
	sbbl $0,TOS(2)
	sbbl $0,TOS(1)
	sbbl $0,eax
	ret
// ( q1 q2 -- q1 q2 sgn ) ; '?uqcmp' signum(q1-q2)
Xquqcmp:pushl NXL
Cquqcmp:movl TS,ecx
	subl TOS(4),ecx	//; sign of uq1 uq2 -
	movl TOS(1),edx
	spush
	movl $-1,TS
	jnz 1f
	cmpl TOS(6),edx
	movl TOS(3),edx
	jnz 1f
	cmpl TOS(7),edx
	movl TOS(4),edx
	jnz 1f
	cmpl TOS(8),edx
	jz 2f		//; ret 0
1:	jc 3f		//; ret -1
	incl eax	//; ret 1
2:	incl eax
3:	ret
// ( q -- q n ) ; 'qslb' no. of bit by which q can be shifted left w.o. change of sign
Xqslb:	pushl NXL
Cqslb:	movl PS,edx	//<Cuqbym)
// ;.:e://; <qvsqrt, 2nd)
	spush
// ;.:n://; <qvsqrt, 1st)
	xorl ecx,ecx
	leal 30(ecx),eax//; 63 bits max, less sign bit, less start bsr:0 vs. shift:1
	cmpl (edx),ecx	//; @@tos
	jg 1f		//;?; -ve
	jnz 2f		//;?; +ve, non-zero
	subl $4,edx
	addl $32,eax
	cmpl (edx),ecx
	jnz 2f
	subl $4,edx
	addl $32,eax
	cmpl (edx),ecx
	jnz 2f
	subl $4,edx
	addl $32,eax
	cmpl (edx),ecx
	leal 1(ecx),ecx	//; prevent -1 if no bits set
	jz 3f		//;?; all zero
2:	bsrl (edx),ecx
	subl ecx,eax
3:	ret
// ; -ve, count m.s. set bits
1:	addl $128-30,eax
	subl $12,edx
4:	decl eax
	js 5f		//;?; all bits set
	btl eax,(edx)
	jc 4b		//;?; next bit
5:	negl eax
	addl $126,eax
	ret
.ifne _bn_
// ( .nn. N m -- .mm. N' ) ; shift counted int left by +m bits, right by -m bits; ret N' adjusted cells count
Xnshift:
	pushl NXL
Cnshift:
	movl TS,ecx
	spop
	testl ecx,ecx
	jz 1f
	jns 5f
	negl ecx	//; shift count
	js 1f		//;?; invalid bitcount
3:	subl $32,ecx
	jc 2f
	call nd32
	jmp 3b
2:	addl $32,ecx
	movl TS,edx	//; cells count
	negl TS
	leal 0(PS,TS,4),PS//,[PS+eax*4]//; l.s. cell
	pushl ebx
	pushl edx
	xorl ebx,ebx
4:	movl (PS),TS
	shrdl cl,TS,ebx	//shrd ebx,eax,cl
	movl ebx,(PS)
	movl TS,ebx
	addl $4,PS
	decl edx
	jnz 4b		//;?; more
	shrl cl,TS
	movl TS,(PS)
	lea 4(PS),PS
	popl eax	// ebx,eax
	popl ebx
	incl eax
1:	ret
//-
1:	call nm32
5:	subl $32,ecx
	jnc 1b
2:	spush
	addl $32,ecx
	pushl PS
	spop
	movl TS,edx	//; cells
	pushl ebx
	pushl TS	//; reg & kref & count
	xorl ebx,ebx	//; begin
3:	movl -4(PS),TS	//; from m.s. cell "down"
	shldl cl,TS,ebx	//shld ebx,eax,cl
	movl ebx,(PS)
	subl $4,PS
	movl TS,ebx
	decl edx
	jns 3b		//;?; more
	shldl cl,TS,edx	//shld edx,eax,cl
	popl TS
	popl ebx
	popl PS		//pop PS,ebx,eax
	incl TS
	ret
//-
1:	movl TOS(1),ecx
	xorl TS,TS
	notl ecx
	leal 0(PS,ecx,4),PS
	spush
	incl TS		//; ret { 0 0 }
	ret
nd32:	cmpl $1,TS
	jna 1b		//; leave min. size of cell + count
	pushl ecx
	spush
	call Croll
	spop
	popl ecx
	decl TS
	ret
nm32:	movl TS,edx
	spush
	pushl ecx
	xorl eax,eax
	notl edx
	spush
	movl edx,TS
	call Croll
	incl TS
	popl ecx
	ret
// ( .nn1. N M -- .nn2. M ); signed bignum size conversion
Xn2n:	pushl NXL
Cn2n:	movl TS,edx
	subl TOS(1),TS
	TOSPULL(1)
	jz 3f
	jng 2f		//>Cun2un)
	movl TS,ecx
	movl TOS(1),TS
	sarl $31,TS
	rep stosl
3:	movl edx,TS
	movl edx,(PS)
	ret		//:c: nicht vom Xun2un trennen :c:
// ( nm .. n0 M N -- nn .. n0 N ) ; unsigned bignum size conversion
Xun2un:	pushl NXL	//:c: nicht vom Xn2n trennen :c:
Cun2un:	movl TS,edx
	subl TOS(1),TS
	TOSPULL(1)
	jz 1f
	jng 2f
	movl TS,ecx
	xorl TS,TS
	rep stosl
2:	lea (PS,TS,4),PS//<Cn2n); drop index, add/drop n cells
1:	movl edx,TS
	movl edx,(PS)
	ret
// ( n1..nn N u -- m1..mn N u ) ; 'ns+' add sing to counted int
Xnsp:	pushl NXL
	pushl TS
	spop
	jmp 1f
// ( n1..nn N -- m1..mn N ) ; 'n1-'
Xn1p:	pushl NXL
Cn1p:	pushl $1
	jmp 1f
// ( n1..nn N -- m1..mn N ) ; 'n1-'
Xn1m:	pushl NXL
Cn1m:	pushl $-1
1:	popl edx	//<Cn1p)
	movl TS,ecx	//; n cells
	negl TS
	leal (PS,TS,4),PS
    addl edx,(PS)
	movl ecx,edx
	leal (ecx,TS),TS//; preserve Cy flag!
	jmp 2f
3:	leal 4(PS),PS	//; Cy!
    adcl TS,(PS)
2:	decl ecx
	jnz 3b
	movl (PS),TS
	spush
	mov edx,TS
	ret
// ( n1..nn n -- m1..mn n ) ; negate counted int
Xnminus:call Cn1m	//>:c: Xnnot)
// ( n1..nn n -- m1..mn n ) ; bitwise invert counted int
Xnnot:	pushl NXL	//<:c: Xnminus)
Cnnot:	pushl eax	//; count
	movl eax,ecx	//; n cells
	notl eax
	lea (PS,TS,4),PS
1:	addl $4,PS
	notl (PS)
	decl ecx
	jnz 1b
	movl (PS),eax
	spush
	popl eax
	ret
.endif // _bn_
// :HIER: steckt der wurm drin: bei uq/mod treten mit beiden neuen defn fehler auf
.if 0 // qrshift // hiermit geht garnichts
// ( q n -- q' ) ; 4>>
Xqrshift:	//; o:edx := ovf
	pushl NXL
Cqrshift:		//<Cuqbym)
	spush
	movl $-4,TOS(1)
	call Cnshift
	decl TS
	call Croll
	movl TS,edx	//??
	spop	//; drop ovf cell
	ret
.else // qrshift
// ( q n -- q' ) ; 4>>
Xqrshift://; o:edx := ovf
	pushl NXL
Cqrshift:		//<Cuqbym)
	movl TS,edx
	movl $0,TOS0
	movl TS,ecx
	shrl $5,edx	//; bits/cell
	jz 1f		//;?; no cells
	pushl esi
2:	leal -12(PS),esi
	leal -16(PS),edi
	movsl
	movsl
	movsl
	movsl
	decl edx
	jg 2b
	popl esi
1:	subl $4,PS
	movl TOS(3),edx	//; (3)
	andl $31,ecx
	jng 3f
	movl TOS(2),TS
	shrdl cl,eax,edx//; [d][a][.][.]
	movl edx,TOS(3)
	movl eax,edx	//; (2)
	movl TOS(1),eax
	shrdl cl,eax,edx//; [3][d][a][.]
	movl edx,TOS(2)
	movl eax,edx	//; (1)
	movl TOS0,eax
	shrdl cl,eax,edx//; [3][2][d][a]
	movl edx,TOS(1)
	xorl edx,edx
	shrdl cl,eax,edx//; save ovf
	shrl cl,eax
	movl eax,TOS0
3:	movl TOS0,TS
	ret
.endif // qrshift
.if 0 // qlshift // hiermit spielen die cons, kommt aber sonst auch muell
// ( q n -- q' ) ; '4<<'
Xqlshift:// ; o:edx := ovf
	pushl NXL
Cqlshift:		//<Cuqbym)
	spush
	movl $4,TOS(1)
	call Cnshift
	movl TOS(1),edx	//??
	SPOP(2)
	ret
.else // qlshift
// ( q n -- q' ) ; '4<<'
Xqlshift:		//<Xq2mul):c: ; o:edx := ovf
	pushl NXL
Cqlshift:		//<Cuqbym)
	movl TS,ecx
	leal TOS(1),TS
	subl $32,ecx
	js 1f		//;?; none, check bits
	std
	pushl esi
2:	movl eax,edi	//; m.s.cell
	leal -4(eax),esi
	movsl
	movsl
	movsl
	movl $0,(edi)
	subl $32,ecx
	jns 2b
	popl esi
	cld
1:	movl eax,PS	//; ptr to m.s. cell
	addl $32,ecx
	jng 3f
	movl TOS0,eax	//; hh
	movl TOS(1),edx	//; hl
	shldl cl,edx,eax//; hh<<hl
	movl eax,TOS0	//;hh
	movl TOS(2),eax	//; lh
	shldl cl,eax,edx//; hl<<lh
	movl edx,TOS(1)	//;hl
	movl TOS(3),edx
	shldl cl,edx,eax//; lh<<ll
	movl eax,TOS(2)	//;lh
	shll cl,edx
	movl edx,TOS(3)	//;ll
3:	movl TOS0,eax
	ret
.endif // qlshift
// ( q1 q2 q3 -- q1' q2 q3 ) ; mul 3rd quad on dstack by two
Cqmul2t:shll $1,TOS(11)
	rcll $1,TOS(10)
	rcll $1,TOS(9)
	rcll $1,TOS(8)
	ret
// ( q1 q2 q3 -- q1' q2 q3 ) ; inc 3rd quad on dstack by one
Cq1pt:	addl $1,TOS(11)
	adcl $0,TOS(10)
	adcl $0,TOS(9)
	adcl $0,TOS(8)
	ret
// ( q1 q2 -- q1' q2 ) ; mul quad @nos by 2
Cqmul2n:shll $1,TOS(7)
	rcll $1,TOS(6)
	rcll $1,TOS(5)
	rcll $1,TOS(4)
	ret
//<:Cuqbym:); tuq/mod, special cases
// ;test dvd =/= 0, ret rmd=0, qot=max; 0/0 -> ret q=1,r=0
1:	SPOP(4)
	call Cqsgn	//; sgn dvd
	spush
	sarl $31,eax
	spush;spush;spush//; sg ssg ssg ssg ssg
	jmp 7f		//; -> 0 0 0 0 0 sg ssg ssg ssg
// ;q div 1, ret qot=dvd, rmd=0
2:	TOSPULL(3)
7:	call 6f
	jmp Cswap4
// ;0 div q, ret qot=0, rmd=0
3:	TOSPULL(7)
	call 6f
	spush
6:	movl $0,eax	//; drop sing & push quad 0
	spush;spush;spush
	ret
// ;dvd < dvs, ret qot=0, rmd=dvd; dvd @tos -!-
4:	call Cswap4
	TOSPULL(3)
	jmp 6b
// ;dvd=dvs, ret qot=1, rmd=0
5:	TOSPULL(7)
	call 6b
	spush
	movl $1,eax
	spush
	decl eax
	spush;spush
	ret
// ( q1 q2 -- q3 ) ; uq/
Xuqby:	call Cqbym	//; q/mod
	jmp Xsdrop4	//; 4swap 4drop
// ( q1 q2 -- q3 q4 ) ; uq/mod
Xuqbym:	pushl NXL
Cuqbym:	call Cqqzeq
	spop
	jz 1b		//;?; dvd=0, dvs/0
	call Cdup4	//;dup4
	call Cqonem
	call Cqzeq	//; 4dup q1- q0=
	spop
	jz 2b		//;?; dvd=1, dvs/1
	call Cswap4
	call Cqqzeq	//; 4swap ?q0=
	spop
	jz 3b		//;?; dvs=0, 0/dvd
	call Cquqcmp	//; dvd dvs f
	testl eax,eax
	spop
	js 4b		//;?; dvs>dvd
	jz 5b		//;?; dvs=dvd
	call Cqslb	//; dvd free bits
	push eax
	call Cqlshift	//;
	call Cswap4	//; ( dvd dvs --
	call Cqslb	//; dvs free bits
	movl TS,ecx
	subl (esp),ecx	//; scaling bitcount
	incl ecx
	addl ecx,(esp)	//; bitcount to restoring remainder
	pushl ecx	//; qot bitcount
	call Cqlshift	//; shift 'up' by free bits of dvs
	spush
	xorl TS,TS
	spush;spush;spush
	call Cmrot4	//; ( q z n'
	jmp 8f
// ; ( qot dvd dvs -- qot rmd xxx )
9:	call Cqmul2t	//; 3rd @tos		qot := qot*2
	call Cquqcmp	//; 1 if q1 u< q2, dvd < dvs
	testl eax,eax	//;C:-1,Z:0;A:1
	spop
	jg 0f		//;?; dvs > dvd
	movl TOS(3),ecx	//; q 7)z 3)n
	movl TOS(2),edx
	subl ecx,TOS(7)
	movl TOS(1),ecx
	sbbl edx,TOS(6)
	sbbl ecx,TOS(5)
	sbbl eax,TOS(4)	//; q z' n, z' = z-n	dvd := dvd-dvs
	call Cq1pt	//; q' z' n, inc 3rd @tos	qot := qot+1
0:	call Cqmul2n	//; q' z' n'		dvd' := dvd*2
8:	decl (esp)	//;loop entry
	jns 9b		//;?; next bit
	SPOP(3)
	popl ecx	//; discard counter
	popl eax
	call Cqrshift	//; rmd
	jmp Cswap4	//; ( ... -- r q )
// ( q1 q2 -- q3 ) ; 'qmod'
Xqmod:	call Cqbym	//; q/mod 4drop
// ( q -- ) ; '4drop'
Xdrop4:	pushl NXL
	jmp Cdrop4
// ( q1 q2 -- q3 ) ; 'q/'
Xqby:	call Cqbym	//; q/mod
Xsdrop4:call Cswap4	//; 4swap 4drop
	jmp Xdrop4
// ( q1 q2 -- q3 q4 ) ; 'q/mod', sign: +/+ q+,r+ ; +/- q-,r+ ; -/- q+,r- ; -/+ q-,r-
Xqbym:	pushl NXL
Cqbym:	call Cqqzeq	// ; (e: Z if q = 0, ecx = sign cell | NZ*(ecx<0): q<0, NZ*((ecx>0)+(ecx=0)): q>0
	spop
	jz 4f		//;?; div0 :ok:
	testl ecx,ecx	//; sgn dvd
	jns 1f		//;?; +ve dvd
	pushl ecx	//; sg(dvs)
	call Cqminus	//; abs(dvd)
	popl ecx
1:	movl TOS(4),edx
	xorl edx,ecx
	pushl edx	//; sg(rmd)=sg(dvs)
	pushl ecx	//; sg(qot)=sg(dvd xor dvs)
	testl edx,edx
	jns 2f		//;?; +ve dvd
	call Cqnegaten	//; abs(dvs)
2:	call Cuqbym	//; div; div0:
	popl ecx
	testl ecx,ecx
	jns 3f		//;?; +ve qot
	call Cqminus
3:	popl ecx
	testl ecx,ecx
	js Cqnegaten	//;?; +ve rmd
	ret
// ; 0/0 1,0 (q/mod: all quad) 
// ; +ve 7fffffff.ffffffff r=q
// ; -ve 80000000.00000001 r=q
4:	xorl TOS(4),ecx	//; sgn(dvd xor dvs)
	TOSPULL(2)
	call cdivz	//; 
	movl TS,ecx	//; 7f.. or 80..
	movl TOS(1),edx	//; 1 if 0/0
	movsbl ch,ecx	//; ffff or 00
	spush;spush;spush;spush
	movl ecx,TOS(6)
	movl ecx,TOS(5)
	movl edx,TOS(3)
	movl ecx,TOS(2)
	movl ecx,TOS(1)
	movl TS,TOS(4)
	ret
Xsover4:pushl NXL
Csover4:call Cswap4
	jmp Cover4
// ( q1 q2 -- q3 q4 ) ; qf/mod
Xqfbym:	pushl NXL
Cqfbym:	call Csover4	//; 4swap 4over
	call Cqbym	//; q/mod
	testl eax,eax	//; ( 11:dvs 7:rmd 3:qot
	jns 1f		//;?; +ve qot no adjustmt
	call Cqonem	//; qot := qot-1
	movl TOS(11),ecx
	movl TOS(10),edx
	addl ecx,TOS(7)
	adcl edx,TOS(6)	//; rmd := rmd + dvs
	movl TOS(9),ecx
	movl TOS(8),edx
	adcl ecx,TOS(5)
	adcl edx,TOS(4)
1:	call Crot4	//; drop dvs
	jmp Cdrop4
// ( q1 n -- q2 ovf ) ; uqsm*
Xuqsmmul:
	pushl NXL
Cuqsmmul:
	movl TS,ecx
	movl TOS(4),TS
	mull ecx	//; ll
	movl eax,TOS(4)	//;   p.lll
	movl TOS(3),eax
	movl edx,TOS(3)	//; 3:p.ll.h
	mull ecx
	addl eax,TOS(3)	//; 3:p.llh
	adcl $0,edx	//;cy
	sbbl eax,eax
	negl eax
	pushl eax	//;cy 3->2
	movl TOS(2),eax
	movl edx,TOS(2)
	mull ecx
	addl eax,TOS(2)
	popl eax
	adcl eax,edx
	sbbl eax,eax
	negl eax
	pushl eax
	movl TOS(1),eax
	movl edx,TOS(1)
	mull ecx
	addl eax,TOS(1)
	popl eax
	adcl edx,eax	//; ovf
	ret
// ( q1 n -- rmd q2 ) ; 'uqsm/'
Xuqsmby:pushl NXL
Cuqsmby:testl eax,eax
	movl eax,ecx
	jz 1f
	movl $0,edx
	movl TOS(1),eax	//;dhh
	divl ecx
	pushl eax	//; qhh
	movl TOS(2),eax	//;dhl
	divl ecx
	movl eax,TOS(1)	//; qhl
	movl TOS(3),eax	//;dlh
	divl ecx
	movl eax,TOS(2)	//; qlh
	movl TOS(4),eax	//;dll
	divl ecx
	movl eax,TOS(3)	//; qll
	movl edx,TOS(4)	//;rmd
	popl eax
	ret
1:	decl eax	//; div0, ret ff....ff
	movl eax,TOS(4)
	movl eax,TOS(3)
	movl eax,TOS(2)
	movl eax,TOS(1)
	ret
// ( uq1 u2 -- uq3 ) ; power (alternate algorithm)
// >rr 0> if 1. 0. 4swap qabs 
Xqpowu:	pushl TS	//; rs{ u2
	xorl TS,TS
	SPUSH(3)
	incl TOS(3)	//; quad 01
	call Cswap4	//; 4swap
	cmpl $0,(esp)
	jng 1f		//;?;ret 1 = q^0 or q^-ve
	testl TS,TS
	jns 2f		//;?; +ve q
	call Cqminus
// begin r 1and if 4sover uq* 4drop 4swap endif
2:	btl $0,(esp)	//<---4
	jnc 3f
	call Csover4
	call Cuqmul
	SPOP(4)
	call Cswap4
// 4dup uq* q0= 0= r> 2/ >rr 0= or
3:	call Cdup4
	call Cuqmul
	orl TOS(1),TS
	orl TOS(2),TS
	orl TOS(3),TS
	SPOP(4)
	jnz 1f		//;?;ovf
	shrl $1,(esp)
	jnz 2b		//-->2;?;more bits
// until endif 4drop rdrop
1:	addl $4,esp
	SPOP(4)
	NX
// ( n1 +n2 -- q ) ; 'umq^'
Xumqpow:pushl NXL
Cumqpow:movl TOS(1),ecx
	movl TS,edx
	testl TS,TS	//; mask max exp
	jz 1f		//;?; n^0: ret 1
	js 2f		//;?; n1(-n) ret 0
	testl ecx,ecx
	jz 2f		//;?; 0^n: ret 0
	andl $127,TS	//; limit to max pover of min, 2^127
	pushl ecx	//; n1
	pushl TS	//; exp
	decl TS
	jz 3f		//;?; 1^n2, ret n2
	xorl TS,TS
	SPUSH(3)
	jmp 5f	  // -->
4:	movl 4(esp),TS
	call Cuqsmmul	//;q(i)=n1*q(i-1)
5:	decl (esp)// <--
	jg 4b
	popl ecx
	popl edx
	spop
	ret
3:	popl ecx	//; ret quad n1
	popl edx
	xorl TS,TS
	decl edx
1:	incl edx	//; ret 1
	movl edx,TOS(1)
	SPUSH(2)
	ret
2:	call 1b		//; ret 0
	movl TS,TOS(3)
	ret
.endif # _mx_
.ifne _rx_
Xqgcd:	call Cqabs	//; abs
	call Cswap4	//; swap
	call Cqabs	//; abs
	jmp 1f		//; ENTER BEGIN
2:	call Csover4	//;  swap over
	call Cuqbym
	SPOP(4)		//;  umod
1:	movl eax,ecx	//; ENTRY -dup
	orl TOS(1),ecx	//;  0=
	orl TOS(2),ecx
	orl TOS(3),ecx
	jnz 2b		//; until
	SPOP(4)		//; ;s
	NX
.endif # _rx_
// ( d1 d2 -- d3 d4 ) ; d/mod ; +/+ q+,r+ ; +/- q-,r+ ; -/- q+,r- ; -/+ q-,r-
// ; 2 pick >r >rr 2>r dabs d->q 2r> dabs uqd/mod r> r xor d+- 2swap r> d+- 2swap ;
Xdbym:	pushl NXL
Cdbym:	movl TS,ecx
	movl TS,edx
	xorl TOS(2),ecx
	orl TOS(1),edx
	jz cdbyz	//; ret 1:0/0, -max:-ve/0, +max:+ve/0 ::ok::
	pushl TOS(2)	//<Xdmod,Xdby,Cmdbym,Xdgcd) ; 2 pick >r
	pushl TS	//; >rr
	pushl TOS(1)	//; (2)>r	[ d2l d2h=sg2 d1h=sg1 ] sg{rmd}=sg{d1h}, sg{qot}=sg{d2h xor d1h}
	SPOP(2)
	call Cdabs	//; abs{dvd}
	spush
	xorl TS,TS	//; d->q
	SPUSH(3)
	popl TOS(1)
	movl (esp),TS	//; 2r>		[ d2h=sg2 d1h=sg1 ]
	call Cdabs	//; abs{dvs}
	call Cuqdbym
	popl ecx
	xorl (esp),ecx
	jns 1f		//;?;+ve qot
	call Cdminus
1:	call Cswap2
	popl ecx
	testl ecx,ecx
	jns Cswap2	//;?; +ve rmd
	call Cdminus
	jmp Cswap2

// ( d1 d2 -- d2 d1 d2 ) ; '2sover' { 2swap 2over }
Xsover2:pushl NXL
Csover2:call Cswap2
	jmp Cover2
.endif # _fc_|_sq_|_dx_|_mx_
Xswap2x:popl IP
// ( d1 d2 -- d2 d1 )
Xswap2:	pushl NXL
Cswap2:	pushl TOS(2)	//<Xuntil,Xrepeat,Xentry,Xestg,Xswap2)
	movl TOS(3),W
	movl TOS(1),T
	movl TS,TOS(2)
	movl T,TOS(3)
	movl W,TOS(1)
	popl TS
	ret
.nolist
// -----------------------------------------------------------------------------
// qarith.S <eof>
