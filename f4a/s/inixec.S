//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; inixec.S 
// =============================================================================
.psize 0
.att_syntax noprefix
.list
.text

// ( -- 0. )
Xdzero:	spush
	xorl TS,TS	//; :c:
// ( -- 0 )
Xzero:	spush
	xorl TS,TS
	NX
// ( -- ) ; 1st in initial 'quit' vector
Xiniq:	xorl ecx,ecx
	movl ecx,uref(blku)
	testb $scrix,uref(bitsu)
	jnz 1f		//;?;scripting
	movl ecx,uref(stateu)
	call Ciocons
	pushfl
	movl $kbdch,TS
	call Cconsq
	spop
	popfl
	jnz Xcr
1:	NX
// ( -- )
Xabort:	call Csprst	//;sprst,drop,drzero,decimal
	pushl $10
	andb $~64,3+uref(offsetu)//;force dr0
	popl uref(baseu)
	call Ciocons
	jz 1f
	testb $scrix,uref(bitsu)
	jnz 1f		//;?;scripting mode
	call Ccr	//;cr,dotcpu,lit,qf,count,type
	call Cdotcpu
	leal qf,TS
	call Cprint
1:	SPOP(2)		//; :c:
// ( -- )
Xquits:	movl uref(vbotu),PS//<Ctdb); quit debug, recover from locals
	movl s0o-D(PS),PS
	movl PS,uref(s0u)//; :c:
// ( -- )
Xquit:	movl uref(quitu),W
	jmp exent
//
// -------------------------------------------------
//
dbpnum:	spush
	movl edx,TS	//; num
	spush
	xorl TS,TS	//; .r field
	spush
	movl ecx,TS	//; emit
	call cdemit////Cemit
	jmp Cdotr
dbnxbot:// ;for sigfpe handler
dbnextl1://;'2 tdb' debug diplay: names after bound vari, nowait
	pushal
	pushfl
	cmpl uref(dbbotu),IP
	jc 8f	//;below lower debugging bound
	cmpl $task,IP
	jc 8f
	call Cndu
	popfl
	jmp 9f
dbnextl3://;'4 tdb' debug diplay, (3) within bounds at @(gb), clr display to the right of cursor
	pushal
	pushfl
	cmpl $task,IP
	jc 6f
	cmpl uref(dbbotu),IP
	jc 6f	//;below lower debugging bound
	cmpl uref(dbtopu),IP
	jc 2f	//;within debugging bounds
8:	jmp 6f
dbnextl0://;'1 tdb'
	pushal
	cmpl $task,IP
	jc 7f
9:	call Cndu
	pushl TS
	call cqtky
	popl TS
	popal
	jz nextloc
dbnextl2://;'3 tdb' debug diplay: names + flags + dstack,cr,wait until kbd active, after task-pfa
	pushal
	pushfl
	cmpl $task,IP
	jc 6f
2:	call Cndu	// ; ip id+
	call Ctab
.ifne _tf_
	movl (IP),edx
	call ctbra
	jz 1f
	popfl
	pushfl
	call cpzs	//; flags
	call Ctab
1:
.endif
	movl $0x523a,ecx
	movl uref(r0u),edx
	subl RS,edx
	shrl $2,edx
	subl $2+1+1+7,edx//; 7:pushal, 1:pushfl; 1:hi level entry, 2:hi-level nesting
	call dbpnum
	call Cdots	//; display DS
	cmpl $dbnextl3,NXL
	jnz 5f		//;?; not mode 4
	spush
	lea vtelr,TS	//; clr till e.o.l. (next line)
	call cvtseq
5:	call Ckey	//<dbnextl1)
0:	call Ctdb	//; re-fetch debugging state
	movl NXL,s_NXL+4(esp)
6:	popfl		//<dbnextl3)
7:	popal		//>:c: <nextloc>
// -------------------------------------------------
nextloc:movl (IP),W
	leal 4(IP),IP
exent:	movl (W),T	//; xec cfa @W, pass pfa = W+, xec entry = T
	leal 4(W),W
	jmp *T
dbnextl4:		//; ruhender modus
	pushal
	pushfl
	call Cqkey
	movl TOS(1),TS	//; @TS ist egal, wenn keine taste ankam, sonst der code
	jng 6b
	jmp 0b
dbnxtop:// ;for sigfpe handler
// -------------------------------------------------

// ( cfa|0 -- .x. ); execute if >/ <docold>
Xdexec:	cmpl $ocold,TS
	jc Xdrop
// ( cfa -- .x. )
Xexecute:
	movl TS,W	// ; cfa
	spop
	jmp exent
// ( +n -- n )( -n -- , quit )
Xertd:	spush
// ( +n -- )( -n -- , quit )
Xert:	pushl NXL
Cert:	testl TS,TS
	jns ertd	// ; drop, ret NC
	call Cmessage
	leal quit,W
	jmp exent
// ( -- d )
Xdlit:	spush
	movl (IP),TS
	spush
	movl 4(IP),TS
	leal 8(IP),IP
	NX
// ( -- n )
Xlit:	spush
	movl (IP),TS
	leal 4(IP),IP
	NX
// ( n1 -- n2 ) ; 'lit-+' by optimizing 'literal'
Xlitp:	addl (IP),TS
	leal 4(IP),IP
	NX
// ( n1 -- n2 ) ; 'lit-*' by optimizing 'literal' - standard opr
Xlitm:	imull (IP)
	leal 4(IP),IP
	NX
// ( n1 -- n2 ) ; 'lit-u*' unsigned mul, by optimizing 'literal' - n.i: after 'unsigned' modifier, bit#9 of (bits)
Xliti:	mull (IP)
	leal 4(IP),IP
	NX
// ( n1 -- n2 ) ; 'lit-u/' unsigned div, by optimizing 'literal' - n.i: after 'unsigned' modifier, bit#9 of (bits)
Xlitu:	xorl edx,edx
	divl (IP),TS
	leal 4(IP),IP
	NX
// ( n1 -- n2 ) ; 'lit-/' signed div, by optimizing 'literal' - standard opr
Xlitd:	movl TS,edx
	sarl $31,edx
	idivl (IP),TS
	leal 4(IP),IP
	NX
// ( n1 -- n2 ) ; 'lit-and' by optimizing 'literal'
Xlita:	andl (IP),TS
	leal 4(IP),IP
	NX
// ( n1 -- n2 ) ; 'lit-or' by optimizing 'literal'
Xlito:	orl (IP),TS
	leal 4(IP),IP
	NX
// ( n1 -- n2 ) ; 'lit-xor' by optimizing 'literal'
Xlitx:	xorl (IP),TS
	leal 4(IP),IP
	NX
Xlitb:	subl (IP),TS	# -
	leal 4(IP),IP
	NX
Xlitg:	cmpl (IP),TS	# >
	setg TSl
	jmp 1f
Xlitl:	cmpl (IP),TS	# <
	setl TSl
	jmp 1f
Xlite:	cmpl (IP),TS	# =
	sete TSl
1:	movzx TSl,TS
	leal 4(IP),IP
	NX
// ( -- )
Xsgd:	orl $sgdf,uref(bitsu)
	NX
.nolist
// -----------------------------------------------------------------------------
// iniexec.S <eof>
