//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; numio.S 
// =============================================================================
.psize 0
.att_syntax noprefix
.text
.list

// ( n -- c ) ; 'h>a' ; convert digit to char code
Xh2a:	pushl NXL
Ch2a:	movzbl TSl,TS
	cmpb $0,uref(baseu)
	jz 1f		//;?; ascii-base
	cmpb $10,TSl
	jc 2f		//;?; decimal range
	addl $'A'-'9'-1,TS
2:	addl $'0',TS
1:	ret
// ( n1 base -- n2 flag )//; char code n1 to digit n2 and tf if in valid range
Xdigit:	pushl NXL
Cdigit:	movb al,dl	//;base
	shr $9,TS
	jnz 4f		//;false base value, force to decimal, return ff
	movzbl TOS(1),TS
	jc 5f		//;base=256 => char
	subb $'0',al
	js 6f		//;no digit if < "0"
	cmpb $10,al
	jc 2f
	cmpb $'a'-'0',al
	jc 1f
	subb $'a'-'A',al
1:	cmpb $'A'-'0',al
	jc 6f
	subb $'A'-'0'-10,al
2:	cmpb dl,al
	jnc 6f
	movl TS,TOS(1)
7:	xorl TS,TS
	incl TS
	ret
5:	cmp $BL,al	//;bl terminates ascii-base
	ja 7b
6:
Cand0:	xorl TS,TS
	ret
4:	pushl $10
	popl uref(baseu)
	jmp 6b
// ( d1 adr1 -- d2 addr2 )
Xpnumberq:
	pushl NXL
Cpnumberq:
	incl TS
	pushl TS
	SPUSH(2)	//;( d1 xx xx xx -- .. | adr
	jmp 1f
2:	incl (esp)	//;( d.l d.h xx n flg -- .. | adr++
	pushl TOS(1)	//;save digit
	movl TOS(4),ecx	//;d.l
	movl TOS(3),edx	//;d.h
	movl ecx,TOS(2)
	movl edx,TOS(1)
	movl uref(baseu),TS
	call Cudmmul	//;( ud1l ud1h u -- ud2l ud2h ovf )
	popl TS		//;digit, drop ovf
	movl TOS(2),ecx	//;l
	movl TOS(1),edx	//;h
	addl TS,ecx
	adcl $0,edx
	movl ecx,TOS(4)	//;accu
	movl edx,TOS(3)
	cmpl $-1,uref(dplu)
	adcl $0,uref(dplu)//;inc @dpl if dp marker exists
1:	movl (esp),ecx
	movzbl (ecx),edx
	movl edx,TOS(1)	//;( dn xx n flg -- .. | adr
	movl uref(baseu),TS
	call Cdigit
	jnz 2b
	SPOP(2)
	popl TS		//;( .. -- d2 adr2
	ret
// ( a1 -- a2 b ) ; a1+1=address of numeric stg -> a2:=adr less praefix, b:=sign|base
Xabase:	pushl NXL
Cabase:	leal 1(TS),ecx
	spush
	movl uref(baseu),TS
	cmpb $'-',(ecx)//; "-"
	jnz 1f
	incl ecx
	orl $0xffff0000,TS//; sign flg
1:	cmpb $'0',(ecx)
	jnz 2f		//;try single char praefix
	incl ecx
	cmpb $'x',(ecx)
	jz 3f
	cmpb $'X',(ecx)
	jnz 4f
3:	movw $16,TSx
5:	incl ecx
6:	decl ecx
	movl ecx,TOS(1)
	ret
7:	movw $-64,TSx	//; ctrl char
	jmp 5b
8:// ;ranum flag
	xorl $ranumf,uref(bitsu)
	movl edx,eax
9:	call 12f	//;find praefix
	jmp 10f
4:	decl ecx	//;not '0x..'
2:	cmpb $'^',(ecx)//; "^"
	jz 7b
	call 12f	//;find praefix
	js 8b		//;ranum (n.i.)
10:	testl edx,edx	//;sign after praefix
	js 11f		//;-ve
	shr $8,TS
	cmpb $'-',TSh
	jnz 11f		//;+ve
	incl ecx
	notl edx	//;flag
	notw dx
11:	movl edx,TS	//;sgn|radix
	jmp 6b
// ; find base praefix
12:	pushl ecx	//;stg
	pushl edi	//;reg
	leal npref,edi	//;praefices
	movl TS,edx	//;save @base & sgn
	movl (ecx),TS	//;leading stg chars
	shll $8,TS
	movb ah,al
	movl $nprefe-npref,ecx
	repnz scasb
	movsbw nbase-npref-1(edi),TSx
	popl edi
	popl ecx
	jnz 13f		//;no praefix
	testb al,al	//;radix
	jz 14f		//;not valid
	leal 1(ecx),ecx	//;count praefix
// :HIER: -2 kommt nur, wenn keine weiteren praefices folgen: :FEHLER:
	js 14f		//;ranum (n.i.), ret lo.w=-2, hi.w 1st 2 chars
	movw TSx,dx
	decb al
	jnz 13f
	xorl TS,TS	//;suppress eval of '-' after praefix for sign
	movw $256,dx	//;char
13:	xorb al,al
14:	ret
// ( a -- dn f ) ; [number], ff if conversion successful
Xbnumberb:pushl NXL
Cbnumberb:		//<Cnumber)
	pushl uref(baseu)//; 	[ bs
	call Cabase
	testw TSx,TSx
	jg 6f		//;base
	js 7f		//;ctrl or ranum
	spop
	call Cbasef
6:	pushl TS	//;sign	[ sg bs
	movzwl TSx,TS
	movl TS,uref(baseu)
	movl TOS(1),edx	//; a
	xorl TS,TS
	movl TS,TOS(1)
	pushl $-1	//; initial @dpl
	spush
	popl ecx
	movl edx,TS	//; @tos := addr
	pushl TS	//;	[ a sg bs
3:	movl ecx,uref(dplu)
	call Cpnumberq	//; d1 a1 -- d2 a2 ), Z-flag from 'digit'
	pushl TS	//; save a2, address of 1st non-convertible code
	call cpmulq	//;ecx:=flag=next char ; not a number if a'=a+1 and eflags=NS*Z
	popl edx	//; a2
	js 3b		//;?;dpl
	subl (esp),edx
	decl edx
	jnz 4f		//;?;stg not a single one char
	movb $1,cl
4:	popl TS		//; a1	[ sg bs
	popl edx	//;sign	[ bs
	movl ecx,TS	//;flag 0 or any =/= 0
	testl edx,edx
	jns 5f		//;?; +ve
	negl TOS(2)
	adcl $0,TOS(1)
	negl TOS(1)
5:	testl TS,TS
	popl uref(baseu)//;	[ -/-
	ret
// ; ctrl char, 'ranum' if TSx=-2
7:	movl TOS(1),ecx
	movswl TSx,TS
	movzbl 1(ecx),ecx
	addl TS,ecx
	xorl TS,TS
	spush
	movl ecx,TOS(2)
	jmp 5b
// ;multiply by cons from unit chars (KMGPE); Z:termination, S:after dpl, else error
cpmulq:	xorl edx,edx	//; multiplier
	pushl TS	//; save addr
	movzbl (TS),TS	//; next char
	pushl edi	//; reg PS
	pushl TS	//; save char
	movl $(cmule-cmult),ecx
	leal cmult,edi	//; postfix table
	repnz scasb
	jnz 6f		//;?;ret NS*Z if neither dpl nor postfix, TS:=char for "error"
	movl edx,(esp)	//; mark 'no error'
	cmp $(cmule-cmuld),ecx
	jnc 6f		//;?;ret NS*Z if <bl> or <nul>
	leal -100(edx),TS//; flag and '%' divisor
	jecxz 6f
	cmp $(cmule-cmuls),ecx
	jnc 1f		//;?;ret S if dpl marker
	movl $1024,TS
	cmpb $'a',-1(edi)
	jc 2f		//;?;digital notation
	subl $24,TS
2:	pushl TS	//; multiplier
	subl $(cmule-cmult)-4,ecx
	sarl $1,ecx
	jge 3f		//;?;
	movl 8(esp),edi	//; PS
4:	movl TOS(2),TS
	mull (esp)
	movl TS,TOS(2)
	movl TOS(1),TS
	movl edx,TOS(1)
	mull (esp)
	addl TS,TOS(1)
	incl ecx
	jnz 4b		//;?;
3:	popl ecx	//; discard multiplier
6:	xorl TS,TS	//; terminate
1:	testl TS,TS
	popl ecx	//; char/flag
	popl edi
	popl TS		//; rst addr
	ret
// ( a -- d | abort-0 )
Xnumber:pushl NXL
Cnumber:		//<Xinter)pret)
.ifne _nv_
leal bnumberb+4,W
	call Cmdfer
	testl TS,TS
.else
	call Cbnumberb
.endif
	jz Cdrop
.ifeq M_NIMP
	xorl TS,TS
.else
	pushl $M_NIMP
	popl TS
.endif
	jmp Cerror
//; ---- num to string conversion

// ( dn -- 0 0 )
Xsharps:pushl NXL
Csharps:call Cbasef	//<cpubseq,Cpdnump)
2:	movl uref(baseu),TS
	call Cumbym	//; edx=rmd, TS|ecx=qot
	movl ecx,TOS(2)
	movl TS,TOS(1)
	cmpl $10,edx
	jc 1f
	addl $7,edx
1:	decl uref(hldu)
	orl TS,ecx
	leal '0'(edx),TS
	movl uref(hldu),edx
	movb TSl,(edx)
	jnz 2b
// ( n -- )
Cdrop:	spop	//<Cidplus,Cnumber,Chold,Xsign,Xroll2)
	ret
// ( dn -- dn' )
Xsharp:	pushl NXL
Csharp:	call Cbasef	//<cpudig,cpuseq,Cpdnump)
	call Cumbym	//;edx=rmd, TS|ecx=qot
	movl ecx,TOS(2)
	movl TS,TOS(1)
qsharpn:cmpl $10,edx	//<Cqsharp)
	jc 1f
	addl $7,edx
1:	leal '0'(edx),TS
.ifne 1
Chold:	movl TS,ecx
	call cbytes
	movl edx,ecx
	movl uref(hldu),edx
	bswap TS
	subl ecx,uref(hldu)
//negl ecx
//addl $4,ecx
//shll $3,ecx
//shrl cl,TS
	movl TS,-4(edx)
	jmp Cdrop
.else
Chold:	decl uref(hldu)
	movl uref(hldu),edx
	movb TSl,(edx)
	jmp Cdrop
.endif
// ( n -- )
Xhold:	pushl NXL
	jmp Chold
// ( n dn -- dn )
Xsign:	pushl NXL
	movl TOS(2),ecx
	movl TOS(1),edx
	movl TS,TOS(1)
	movl edx,TOS(2)
	testl ecx,ecx
	jns Cdrop
	movb $'-',al
	jmp Chold
.ifne _mx_
Xqsharp:pushl NXL
Cqsharp:spush
	movl uref(baseu),TS
	call Cuqsmby
	spush
	movl $4,eax
	call Croll
	movl TS,edx
	jmp qsharpn	//>Csharp)
.endif # _mx_
// ( -- )
Xlesssharp:
	pushl NXL
Clesssharp:		//<cpudig,cpuseq,Cpdnump,Cpnump)
	movl uref(dpu),ecx
	addl $lumpi,ecx
	movl ecx,uref(hldu)//;pad hld !
	ret
// ( 0 0 -- a u )
Xsharpgreat:
	pushl NXL
Csharpgreat:		//<cpudig,Cpdnump,Xatxy)
	movl uref(dpu),TS//;2drop
	movl uref(hldu),ecx//;hld @
	leal lumpi(TS),TS//;pad
	movl ecx,TOS(1)
	subl ecx,TS	//;over -
	ret

// ( ud -- a u ) ; (udnum)
Cpudnump:		//<;Cpudnum)
	pushl $0
	call Clesssharp
	pushl $BL
	spush
	popl TS
	call Chold
	call Csharp	//;min 2 digts for <dump>, <.s>
3:	call Csharps
	popl ecx
	sarl $31,ecx
	jns 4f
	andl $'-',ecx
2:	decl uref(hldu)
	movl uref(hldu),edx
	movb cl,(edx)
4:	jmp Csharpgreat
// ( n -- a u )
Cpnump:	pushl TS	//<;Cdots,Cddotr)
	call Cdabs
	call Clesssharp
	jmp 3b
// ( -- )
Xdotcpu:pushl NXL
Cdotcpu:spush		//<Xabort)
	pushl uref(baseu)
	pushl $36
	movl odotcpu,TS
	popl uref(baseu)
	call Cdot
	popl uref(baseu)
	ret
// ( dn base -- )
Xdbdot:	pushl NXL
	pushl uref(baseu)
	movl TS,uref(baseu)
	jmp 2f
// ( n base -- )
Xbdot:	pushl NXL
Cbdot:	pushl uref(baseu)//<cidnum)
	movl TS,uref(baseu)
	movl TOS(1),TS
	sarl $31,TS
	spush
2:	xorl TS,TS	//<Xdbdot)
	call 3f		//>Cddotr<
	popl uref(baseu)
	ret
// ( n count -- )
Xdotr:	pushl NXL
	jmp Cdotr
// ( n -- )
Cdot:	spush		//<Cdotcpu,Xqq)
	xorl TS,TS
	notl TS
Cdotr:	movl TS,ecx	//<dbnext(xx),Clist,Xindex,Cmessage)
	movl TOS(1),TS
	sarl $31,TS
	spush
	movl ecx,TS
	jmp Cddotr
// ( u -- )
Xudot:	spush
	xorl TS,TS
	jmp Xddot
// ( a -- )
Xquest:	movl (TS),TS
Xdot:	spush
	sarl $31,TS
Xddot:	pushl NXL	//<Xudot)
	spush
	xorl TS,TS
	notl TS
Cddotr:	testl TS,TS	//<Cdotr)
	jns 1f
	notl TS
3:	pushl $Cspace	//<Cbdot>
1:	pushl TS	//;maxlen
	spop
	call Cpnump
	spush
	popl TS
	subl TOS(1),TS
	call Cspaces
	jmp jctype
// ( d -- )
Xddotr:	pushl NXL
	jmp Cddotr
.nolist
// -----------------------------------------------------------------------------
// numio.S <eof>
