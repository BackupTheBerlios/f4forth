//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; dsmem.S 
// =============================================================================
.psize 0
.att_syntax noprefix
.list
.text

//; ---- mem & dstack ----

Xndrops:popl IP	//<Xexit)
// ( .xx. .nn. n -- ) ; drop +n, allocate -n cells to dstack
Xndrop:	notl TS
	leal 0(PS,TS,4),PS
	movl (PS),TS
	call Csprst
	jmp 1f
// ( a -- ) ; '2off'
Xoff2:	andl $0,4(TS)
// ( a -- ) ; 'off'
Xoff:	andl $0,(TS)
1:	spop		//<Xndrop)
	NX
// ( a1 a2 -- ) ; 'c!!' exchange two bytes in memory
Xcss:	movl TOS(1),edx
	movb (TS),ch
	movb (edx),cl
	movb ch,(edx)
	movb cl,(TS)
	jmp Xdrop2
// ( a1 a2 -- ) ; '!!' exchange two cells in memory
Xstosto:movl TOS(1),edx
	movl (TS),ecx
	pushl (edx)
	movl ecx,(edx)
	popl (TS)
	jmp Xdrop2
// ( a c -- ) ; 'toggle' xor byte in memory
Xtoggle:movl TOS(1),T
	xorb TSl,(T)
	jmp Xdrop2
// ( n a -- ) ; '-!'
Xmsto:	negl TOS(1)
// ( n a -- ) ; '+!'
Xpsto:	movl TOS(1),T
	addl T,(TS)
Xdrop2:	SPOP(2)		//<Xstosto,Xtoggle,Xstore,Xwstore,Xcstore,Xands,Xors,Xxors)
	NX
// ( n a -- ) ; '!'
Xstore:	movl TOS(1),T
	movl T,(TS)
	jmp Xdrop2
// ( w a -- ) ; 'w!'
Xwstore:movl TOS(1),ecx
	movw cx,(TS)
	jmp Xdrop2
// ( c a -- ) ; 'c!'
Xcstore:movb TOS(1),cl
	movb cl,(TS)
	jmp Xdrop2
// ( n a -- ) ; 'and!' AND into cell in memory
Xands:	movl TOS(1),ecx
	andl ecx,(TS)
	jmp Xdrop2
// ( n a -- ) ; 'xor!' XOR into cell in memory
Xors:	movl TOS(1),ecx
	orl ecx,(TS)
	jmp Xdrop2
// ( n a -- ) ; 'or!' OR into cell in memory
Xxors:	movl TOS(1),ecx
	xorl ecx,(TS)
	jmp Xdrop2
// ( n a -- a' ) ; '!+' sto cell & increment addr by 4
Xstorep:movl TOS(1),ecx
	TOSPULL(1)
	movl ecx,(TS)
	addl $4,TS
	NX
// ( n a -- a' ) ; 'c!+' sto byte & increment addr by 1
Xcstorep:
	movl TOS(1),ecx
	TOSPULL(1)
	movb cl,(TS)
	incl TS
	NX
// ( a1 -- a2 n ) ; '@+' fetch & inc addr by cell size
Xfetchplus:
	pushl NXL
Cfetchplus:		//<Cidplus)
	leal 4(TS),TS
	spush
	movl -4(TS),TS
	ret
// ( a1 -- a1 n ) ; 'dup@' dup and fetch
Xdupf:	spush
// ( a -- n ) ; '@'
Xfetch: movl (TS),TS
	NX
// ( a -- w ) ; 'w@'
Xwfetch:movzwl (TS),TS
	NX
// ( a -- c ) ; 'c@'
Xcfetch:movzbl (TS),TS
	NX
// ( a1 -- a2 ) ; 'c+@'
Xcpf:	movsbl (TS),ecx
	addl ecx,TS
	NX

//; ---- memory ----

.ifne _mx_
// ( a -- .nn. N ) 'n@'
Xnfetch:addl $4,TS
	spush
	movl -4(TS),TS
	pushl TS
	shll $2,TS
	call Cmtos
	spush
	popl TS
	NX
// ( .nn. N a -- ) 'n!'
Xnstore:movl TOS(1),ecx	//; count
	movl ecx,(TS)
	addl $4,TS
	movl TS,TOS(1)
	leal (,ecx,4),TS//:contd: Xstom
.endif
// ( .nn. a n -- ); 's>m' copy and discard (4+ aligned) n bytes @tos to memory address a
Xstom:	pushl esi	//<Xnstore)
	movl TS,ecx
	leal TOS(1),esi
	addl $3,TS
	movl (esi),PS	//; PS = edi := dst
	andl $-4,eax
	subl TS,esi	//; dst := tos - bytecount, -cell aligned
	leal -4(esi),edx
	rep movsb
	movl edx,PS	//; = edi
	movl (edx),TS
	popl esi
	NX
// ( a b -- .nn. ) ; 'm>s'
Xmtos:	pushl NXL
Cmtos:	pushl esi
	movl TS,ecx
	leal TOS(1),PS
	movl (PS),esi
	movl $0,(PS,ecx)
	rep movsb
	popl esi
	decl PS
	andl $-4,PS
	movl (PS),TS
	ret
// ( p n -- )(R: n1..nn -- ) ; r>m
Xrtom:	movl TS,ecx	//; count
	leal TOS(1),eax
	movl (eax),edi	//; dst
	movl esi,(eax)	//; save reg
	movl esp,esi
	rep movsb
	addl $4,esi
	andl $-4,esi	//; -align
	movl esi,esp	//; drop data from rs
	movl (eax),esi
	leal -4(eax),PS
	movl (PS),TS
	NX
// ( a n -- )(R: -- n1..nn ) ; m>r
Xmtor:	movl TS,ecx	//; count
	lea TOS(1),eax	//; save PS by tos+cell
	movl (eax),edx	//; a
	movl esi,(eax)	//; save IP
	pushl $0
	movl edx,esi	//; src = a
	leal 3(esp),edi
	subl ecx,edi
	andl $-4,edi	//; dst
	movl edi,esp	//; storage space
	rep movsb
	movl (eax),esi	//; rst IP
	leal -4(eax),PS	//; drop a
	movl (PS),TS
	NX
// ( a1 a2 +n -- ); '>move<'
Xmovex:	testl TS,TS
	jng Xdrop3	//;?; nothing to copy
	spush
	shrl $2,TS
	addl $2,TS
	pushl TS	//; locals
	call C2loc
	movl uref(l0u),edx//; l0
	pushl esi
	pushl edi
	movl TOS(2),esi	//; p1, src beginning
// ; align such that w/ swapped dwords src of final move will be aligned
	addl $4,edx	//; safety margin for aligned transfer
	leal 3(TS),ecx	//; count
	shrl $2,ecx	//; copying by dwords
	pushl edx	//; p1 <- @s0 src for back-transfer
	pushl TOS(1)	//; final destn
// ; provide for unaligned end of stg
	addl $3,edx	//; dst end
	andl $-4,edx	//; dst
	leal (edx,eax),edi//;dst end
1:	movl (esi),edx	//;
	subl $4,edi
	bswap edx
	movl edx,(edi)
	addl $4,esi
	decl ecx
	jns 1b		//;?;-';
	popl edi	//; p2 dst
	popl esi	//; copied src
	movl TS,ecx	//; count
	shr $2,ecx
	jz 2f
	rep movsl
2:	andl $3,TS
	jz 3f
	movl TS,ecx
	rep movsb
3:	popl edi	//;rst SP
	popl esi
	TOSPULL(2)
	popl eax
	negl eax
	jmp X2loc
// ( from to +/-n --- )
Xlcmove:pushl NXL
	jmp Clcmove
Xmove:	pushl NXL
Cmove:	movl TOS(1),edx
	cmpl edx,TOS(2)
	jg jcmove
Clcmove:testl TS,TS
	jng 1f
	jmp 3f		//<;cmove: top down
Xcmove:	pushl NXL
Ccmove:	testl TS,TS
	jng 1f
jcmove:	negl TS
	jz 1f
3:	pushl edi
	pushl esi
	movl TS,ecx
	movl TOS(2),esi
	movl TOS(1),edi
	jns cmtop	//;cmove top down
	negl ecx
2:	rep movsb
	popl esi
	popl edi
	cld
1:	SPOP(3)
	ret
cmtop:	leal -1(edi,ecx),edi
	leal -1(esi,ecx),esi
	std
	jmp 2b
// ( n -- )
Xblanks:movb $BL,cl
	jmp 1f
// ( a n -- )
Xerase:	movb $0,cl
1:	spush
	movzbl cl,TS
Xfill:	pushl edi
	movl TOS(1),ecx
	movl TOS(2),edi
	testl ecx,ecx
	jng 1f
	rep stosb
1:	popl edi
Xdrop3:	SPOP(3)		//<Xstore2,Xpsto2)
	NX
// ( d a -- ); '2!'
Xstore2:movl TOS(2),edx
	movl TOS(1),ecx
	movl edx,(TS)
	movl ecx,4(TS)
	jmp Xdrop3
// ( d a -- ); '2+!'
Xpsto2:	movl TOS(2),edx
	movl TOS(1),ecx
	addl edx,(TS)
	adcl ecx,4(TS)
	jmp Xdrop3
// ( a -- d ); '2@'
Xfetch2:movl 4(TS),T
	movl (TS),TS
	spush	
	movl T,TS
	NX

//; ---- stacks ----

//; ---- rstack ----

// ( -- )
Xploopq:movl   $1,T
loopcmp:addl (RS),T	//<Xploopq)
	cmpl 4(RS),T
	jge loopex	//;?; -.
loopon:	movl T,(RS)
Xbranch:addl (IP),IP	//<Xnzbra)
Xnoop:	NX
// ( n -- )
Xpploopq:
	movl TS,T
	spop
	testl T,T
	jg loopcmp	//; +ve
	addl (RS),T	//; prevent 0 +loop
	cmpl 4(RS),T
	jg loopon
loopex:	leal 4(IP),IP	//<loopcmp)
Xr2drop:leal 8(RS),RS
	NX
//////Xrzbra: spush	//:HIER: fuer if-optimierung mit ( r> ... )
//////	popl TS
//////	jmp Xzbra
//////Xrpbra:	spush
//////	popl TS
// ( n -- ); branch if n >/ 0
Xpbra:	shrl $31,TS
// ( n -- ); branch if n = 0; 'if' runtime code
Xzbra:	addl TS,TS	//:Xpbra:
	spop
	jz Xbranch	//;?; -'
1:	leal 4(IP),IP	//<Xnzbra)
        NX
// ( n -- ); branch if n < 0
Xmbra:	shrl $31,TS
// ( n -- ); branch if n =/= 0
Xnzbra:	addl TS,TS	//:Xmbra:
2:	spop		//<Xneqbra)
	jnz Xbranch
	jmp 1b		//>Xzbra)
// ( n1 n2 -- | n1 ) ; ~bra, 'case' runtime
Xneqbra:cmpl TOS(1),TS
	jnz 2b		//>Xnzbra)
	leal 4(IP),IP
	jmp 1f		//>Xpdoq); 2drop
// ( n1 n2 -- )(R: -- n1 n2 )
Xgreatr2:		//<Xgreatr4)
Xpdoq:	pushl TOS(1)	// limit
	pushl TS	// start
1:	SPOP(2)		//<Xneqbra)
	NX
// (R: n -- )
Xrdrop:	leal 4(RS),RS
	NX
// (R: n1 n2 -- )
Xrdrop2:leal 8(RS),RS
	NX
// ( n1 n2 -- n1 n2 )(R: -- n1 n2 )
Xtrr2:	pushl TOS(1)
	pushl TS
	NX
// ( -- n )(R: n -- ) ; 'r>'
Xrgreat:spush
	popl TS
	NX
// ( -- n1 n2 )(R: n1 n2 -- )
Xrgreat2:
	spush
	spush
	popl TS
	popl TOS(1)
	NX
// ( -- n ) ; also 'i', 1st level loop index
Xrx:	spush
	movl (RS),TS
	NX
// ( -- n ) ; next level loop index
Xj:	spush
	movl 8(RS),TS
	NX
// ( n1 -- n2 ) ; n1-th loop limit
Xll:	movl 4(RS,TS,8),TS
	NX
// ( n1 -- n2 ) ; n1-th loop index
Xni:	movl 0(RS,TS,8),TS
	NX
// ( -- n ) ; 'r>>r'  r> swap >r
Xr2r:	movl TS,ecx
	movl (RS),TS
	movl ecx,(RS)
	NX
// ( n -- ) (R: ..n.. -- ) ; drop n items from return-stack
Xnrdrop:leal (RS,TS,4),RS
	spop
	NX
// ( .xx. xn ... n -- .xx. ) ; 'n>r'
Xntor:	movl TS,ecx
	jmp 1f
2:	pushl eax
1:	subl $4,PS
	movl (PS),eax
	decl ecx
	jns 2b
	NX
// ( .xx. n -- .xx. xn ... x1 ) ; 'nr>'
Xnrfrom:movl TS,ecx
	spop
	jmp 1f
2:	addl $4,PS
	popl TS
1:	movl TS,(PS)
	decl ecx
	jns 2b
	NX
.ifne _mx_
// ( n1 n2 n3 n4 -- )(R: -- n1 n2 n3 n4 )
Xtor4:	pushl TOS(3)
	pushl TOS(2)
	pushl TOS(1)
	pushl TS
	SPOP(4)	
	NX
// ( -- n1 n2 n3 n4 )(R: n1 n2 n3 n4 -- )
Xrfrom4:SPUSH(4)
	popl TS
	popl TOS(1)
	popl TOS(2)
	popl TOS(3)
	NX
Xrx4:	spush
	movl 12(RS),TS
	spush
	movl 8(RS),TS	//:contd: Xrx2
.endif
// ( -- n1 n2 )(R: n1 n2 -- n1 n2 )
Xrx2:	spush		//<Xrx4)
	movl 4(RS),TS
	spush
	movl (RS),TS
	NX
// ( -- n )(R: n1 -- n2 ) ; 'r+' r> r> dup 4+ >r swap >r @ 
Xrplus:	spush
	movl (RS),TS
	addl $4,(RS)
	movl (TS),TS
	NX
// ( n -- n )(R: -- n ) ; '>rr'
Xtrr:	pushl TS
	NX
// ( n -- )(R: -- n ) ; '>r'
Xgreatr:pushl TS	//:contd: Xdrop
// ( n -- ) ; 'drop'
Xdrop:	spop		//<Xgreatr,Xmplus,Xuplus)
	NX

//; ---- dstack ----

// ( d1 n -- d2 ) ; 'm+'
Xmplus:	movl TS,ecx
	sarl $31,ecx
	addl TS,TOS(2)
	adcl ecx,TOS(1)
	jmp Xdrop
// ( ud1 u -- ud2 ) ; 'u+' unsigned add sing to double
Xuplus:	addl TS,TOS(2)
	adcl $0,TOS(1)
	jmp Xdrop
// ( n -- n | 0 0 ) ; '0dup' dup if zero or '-0'
Xzdup:	negl TS
	jz 2f
	jns 3f
	negl TS
	jns 1f
	addl TS,TS	//; '-0' -> '0'
	negl TS
3:	negl TS
2:	spush
1:	NX
// ( n -- n n | 0 -- 0 ) ; '-dup' dup if non-zero (nee: and not '-0')
Xmdup:	testl TS,TS
	jz 1f
Xdup:// ( n -- n n ) ; 'dup'
	spush
1:	NX
// ( d -- d d )
X2dup:	movl TOS(1),T
	SPUSH(2)
	movl T,TOS(1)
	NX
// ( n1 n2 n3 -- n3 n1 n2 )
Xmrot:	call Crot
// ( n1 n2 n3 -- n2 n3 n1 )
Xrot:	pushl NXL
Crot:	movl TOS(1),T
	movl TOS(2),W
	movl TS,TOS(1)
	movl T,TOS(2)
	movl W,TS
	ret
// ( .x. -- )
Xspfetch:
	movl PS,W
puhw:	spush
	movl W,TS
	NX
// (R: .x. -- ) ( -- )
Xrpfetch:
	movl RS,W
	jmp puhw
.ifne _dx_
    .ifne _mx_
Xdup4:	pushl NXL
Cdup4:	movl TOS(1),ecx
	movl TOS(2),edx
	pushl TOS(3)
	SPUSH(4)
	movl ecx,TOS(1)
	movl edx,TOS(2)
	popl TOS(3)
	ret
    .else
Xdup4:	pushl NXL
Cdup4:	call Cover2
	jmp Cover2
    .endif
.endif
// ( d1 d2 -- d1 d2 d1 )
Xover2:	pushl NXL
Cover2:	movl TOS(2),T	//<Cuqmbym,Xdgcd,Xdup4)
	movl TOS(3),W
	SPUSH(2)
	movl T,TS
	movl W,TOS(1)
	ret
// ; { swap ;s } opt by ';'
Xswapx:	popl IP
// ( n1 n2 -- n2 n1 )
Xswap:	pushl NXL	//<Xcswap)
Cswap:	movl TOS(1),T	//<C2variable,Xcswap)
	movl TS,TOS(1)
	movl T,TS
	ret
.ifne _dx_
// ; { cswap ;s } opt by ';'
Xcswaps:popl IP
// ( n1 n2 n3 n4 -- n4 n3 n2 n1 ); `cswap'
Xcswap:	call Cswap
	call Cswap2
	jmp Xswap
// doppelt! siehe 'cswap' :HIER:
// ( n1 n2 n3 n4 -- n4 n3 n2 n1 ) ; >swap<
Xgswapl:pushl TOS(3)
	movl TOS(2),ecx
	movl TOS(1),edx
	movl TS,TOS(3)
	movl edx,TOS(2)
	movl ecx,TOS(1)
	popl TS
	NX
// ( n -- n n n )
Xddup:	SPUSH(2)
	NX
// ( n1 n2 n3 n4 n5 -- 1n n2 n3 n4 n5 n1 ) ; 'pick4'
X4pick:	pushl $4
	jmp 1f		//>X2pick)
// ( n1 n2 n3 n4 -- 1n n2 n3 n4 n1 ) ; 'pick3'
X3pick:	pushl $3
	jmp 1f		//>X2pick)
// ( n1 n2 n3 -- 1n n2 n3 n1 ) ; 'pick2'
X2pick:	pushl $2
1:	spush		//<X4pick,X3pick)
	popl TS		//:c: Xpick
.endif
// ( .x. xn .x. +n -- .x. xn .x. xn )( .x. xn .x. xn' -n -- .x. xn' .x. )
Xpick:	negl TS		//<:c: X2pick)
	jg 1f
	movl -4(PS,TS,4),TS
        NX
1:	movl TOS(1),ecx
	notl TS		//; -ve index stores back into PS
	movl ecx,-4(PS,TS,4)
	SPOP(2)
	NX
// ( n1 n2 -- 1n n2 n1 )
Xover:	spush
	movl TOS(2),TS
	NX
// ( n1 n2 -- n1 n1 ) ; dropd
Xdropd:	movl TOS(1),TS
	NX
// ( n1 n2 -- n2 n1 n1 ) ; sdup
Xsdup:	movl TS,ecx
	movl TOS(1),TS
	movl ecx,TOS(1)
	spush
	NX
// ( n1 n2 -- n1 n1 n2 ) ; dup2
Xdup2:	movl TS,ecx
	movl TOS(1),TS
	spush
	movl ecx,TS
	NX
// ( n1 n2 n3 -- n2 n3 ) ; 'drop2' drop 3rd
Xrotd:	movl TOS(1),ecx
	movl ecx,TOS(2)
	TOSPULL(1)
	NX
// ( n1 n2 -- n2 n1 n2 )
Xsover:	movl TOS(1),ecx	//; n1
	TOSPUSH(1)
	movl ecx,TOS(1)
	movl TS,TOS(2)	//; n2
	NX
// ( .xn. .yn. n -- .yn. .xn. ) ; dup 2* 1- dup do 0 do j roll loop leave loop
Xnswap:	pushl NXL
Cnswap:	lea -1(,TS,2),edx//<Cswap8)
	testl TS,TS
	jng 1f
	pushl edx
	movl TS,edx
2:	movl (esp),TS
	call Croll
	spush
	decl edx
	jg 2b
	addl $4,esp
1:
crolov:	spop		//<Croll2,Cpick4,Croll4)
	ret
.ifne _dx_
    .ifne _mx_
// ( o1 o2 -- o2 o1 ) ; 
Xswap8:	pushl NXL
Cswap8:	pushl $8
	spush
	popl TS
	jmp Cnswap
// ( q1 q2 -- q1 q2 q1 ) ; '4over' { 1 4pick }
Xover4:	pushl NXL
Cover4:	pushl $1
	spush
	popl TS
	jmp Cpick4
// ( .x. on .y. n -- .x. .y. on ) ; 8roll
Xpick8:	addl TS,TS	// ;2* s->d 2* 1+ + 
	movl TS,ecx
	sarl $30,TS
	addl ecx,TS
	incl TS
	pushl TS
	call Cpick4
	spush
	popl TS		//->:c: Xpick4
// ( .qn. .. .qx. n -- .. .qx. .qn. ) ; : 4pick 2* s->d 2* 1+ + >rr 2pick r> 2pick ;
Xpick4:	pushl NXL	//<-:c: Xpick8
Cpick4:	leal (,TS,2),ecx//<Cover4)
	sarl $30,TS
	orl $1,TS
	addl ecx,TS
	jo crolov
	pushl TS
	call Cpick2
	spush
	popl TS
	jmp Cpick2
    .endif
Xpick2:	pushl NXL
Cpick2:	testl TS,TS	//<Cpick4)
	js 1f
	notl TS
	movl 4(PS,TS,8),ecx
	movl (PS,TS,8),TS
	spush
	movl ecx,TS
	ret
1:	movl TOS(1),ecx
	movl TOS(2),edx
	movl edx,-8-8(PS,TS,8)
	movl ecx,-4-8(PS,TS,8)
	SPOP(3)
	ret
    .ifne _mx_
// ( .x. on .y. n -- .x. .y. on ) ; 8roll
Xroll8:	movl TS,ecx	// ;2* s->d 2* 1+ + 
	addl TS,TS
	sarl $31,ecx
	leal 1(TS,ecx,2),TS
	pushl TS
	call Croll4
	spush
	popl TS		//->:c: Xroll4
// ( .qn. .. .qx. n -- .. .qx. .qn. ) ; : 4roll 2* s->d 2* 1+ + >rr 2roll r> 2roll ;
Xroll4:	pushl NXL	//<-:c: Xroll8
	jmp Croll4
// ( q1 q2 q3 -- q2 q3 q1 ) ; 4rot
_hx Xrot4:pushl NXL
Crot4:	pushl $2
	jmp 1f
// ( q1 q2 q3 -- q3 q1 q2 ) ; 4-rot
_hx Xmrot4:pushl NXL
Cmrot4:	pushl $-2
1:	spush		//<Crot4)
	popl TS
// ( .x. qn .y. n -- .x. .y. qn ) ; 4roll
Croll4:	leal (,TS,2),ecx//<Xroll4)
	sarl $30,TS
	orl $1,TS
	addl ecx,TS
	jo crolov	//; drop & ret
	pushl TS
	call Croll2
	jmp 1f
    .endif
// ( d1 d2 d3 -- d3 d1 d2 ) ; '2-rot'
Xmrot2:	pushl NXL
Cmrot2:	pushl $-2	//<Cqfmbym)
	jmp 1f
// ( d1 d2 d3 -- d2 d3 d1 ) ; '2rot'
Xrot2:	pushl NXL
Crot2:	pushl $2
1:	spush		//<Cmrot2,Croll4)
	popl TS
	jmp Croll2
// ( nx...nm +N -- nx...nm nN ) ; '2roll'
// ( nx...nm -N -- nN nx...nm )
Xroll2:	pushl NXL
Croll2:	addl TS,TS	//<Crot2,Croll4)
	jo crolov	//;?; bounds ovf: drop & ret
	incl TS
	jns 1f
	subl $2,TS
	jns crolov	//;?; bounds ovf: drop & ret
1:	pushl TS
	call Croll
	spush
	popl TS		//:contd: :Xroll:
.endif
// ; edx preserved for 'nswap' 
Croll:	movl TS,ecx	//<Xroll,Croll2)
	negl TS
	jg 2f		//; roll back
	pushl edi
	pushl esi
	leal -4(PS,TS,4),edi
	movl (edi),TS
	jnl 1f
	leal 4(edi),esi
	rep movsl
1:	popl esi
	popl edi
	TOSPULL(1)
	ret
// ( .x. xn .x. +n -- .x.x. xn )( .x.x. xn -n -- .x. xn .x. )
Xroll:	pushl NXL
	jmp Croll
// --
2:	spop		//<Croll
	pushl edi
	pushl esi
	leal -4(PS),esi
	std
	negl ecx
	rep movsl
	cld
	movl TS,(edi)
	popl esi
	popl edi
	movl (PS),TS
	ret
.ifne _mx_
// ( n1 n2 n3 -- n1 n2 n3 n1 n2 n3 )
_hx Xdup8:	pushl NXL
Cdup8:	pushl $8
	spush
	popl TS
	jmp Cndup
.endif
.ifne _dx_
// ( n1 n2 n3 -- n1 n2 n3 n1 n2 n3 )
Xdup3:	pushl $3
	spush
	popl TS
.endif
// ( .xx. +n -- .xx. .xn. )
Xndup:	pushl NXL
Cndup:	testl TS,TS
	jng 1f
	pushl TS
	addl TS,TS	//;reserve...
	call Cpsprst	//;alloc if required
	movl (esp),TS
	movl (esp),ecx
	negl TS
	movl esi,(esp)
	leal (PS,TS,4),esi
	rep movsl
	popl esi
	spop
1:	ret
.nolist
// -----------------------------------------------------------------------------
// dsmem.S <eof>
