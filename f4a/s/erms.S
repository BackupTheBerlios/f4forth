//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; erms.S 
// =============================================================================
.psize 0
.att_syntax noprefix
.list
.text

//
//; ---- f4 version ----

// ( -- ); 'f4ver` sedecimal version no. by packed bytes
Xf4v:	spush
	movl $VERNUM,TS
	NX
//
//; ---- error messages ----

// ( ca -- ) ; by 'forget', 'sforget'
Xfencems:
	pushl $M_FENCE
	popl TS		//;:contd: Xmessage
// ( n -- )
Xmessage:		//<Xfencems)
	pushl NXL
Cmessage:
	movl uref(warningu),ecx
	testl ecx,ecx
	jz 6f		//;?; message by no.
	js 5f		//;?; internal text
	cmpl $0,cdtref(chid)+cdtptr(dr0ch)
	jng 5f		//;?; use internal text
	pushl uref(offsetu)
	pushl $4	//;wrt scr 4 of dr0
	andb $~64,3+uref(offsetu)//;force dr0
	spush
	popl TS
	call Cdotline	//; .line
	popl uref(offsetu)
	ret
5:	pushl TS
	call cfmsg
	popl ecx
	jns Cprint
	movl ecx,TS
6:	SPUSH(2)
	leal mgtmsg,TS
	call Cprint
	xorl TS,TS
	jmp Cdotr
// ; i: eax=message no.; o: ptr to byte-counted text
cfmsg:	movl TS,ecx	//<Cmessage);
	movl uref(bmsgbu),TS
	testl TS,TS
	jnz 2f
	leal msgt0,TS
2:	decl ecx
	js 1f		//;?; msg no. found
	movzbl (TS),edx
	leal 2(edx,TS),eax
	cmpl $-1,(TS)
	jnz 2b		//;?; more in list
1:	cmpb $1,(TS)
	jns 3f		//;?; stg not empty 
4:	leal mgtmsg,TS	//; text fuer undefinierte meldung
3:	ret
// ( -- )
Xqexec:	pushl NXL
Cqexec:	cmpl $0,uref(stateu)
	jz eret
	pushl $M_EXECO
	jmp qern
// ( n -- | abort-19 )
Xqdepth:incl TS
	call Cdepth
	cmpl TOS(1),TS
	jnc Xdrop2
// ( n2 n1 -- )
Xqpairs:pushl NXL
	jmp Cqpairs
// ; ?comp 2 ?pairs
qpairs2:pushl $2
	spush
	popl TS
cqpairs:call Cqcomp
Cqpairs:cmpl TOS(1),TS
	TOSPULL(1)
	jz epop
	pushl $M_PAIRS
	jmp qerp
// ( flg n -- )
Xqerror:pushl NXL
Cqerror:cmpl $0,TOS(1)
	TOSPULL(1)
	jnz Cerror
epop:	spop
eret:	ret
// ( -- )
Xqcomp:	pushl NXL
Cqcomp:	cmpl $0,uref(stateu)
	jnz eret
	pushl $M_COMPO
qern:	spush
qerp:	popl TS
qerj:	jmp Cerror
// ( -- )
Xqcsp:	pushl NXL
Cqcsp:	cmpl uref(cspu),PS
	jz eret
	pushl $M_CSP
	jmp qern
// ( -- )
Xqloading:
	pushl NXL
Cqloading:
	cmpl $0,uref(blku)
	jnz eret
	pushl $M_LOAD
	jmp qern
// ( -- )
Xqstack:pushl NXL
Cqstack:call Csprst
	testl TS,TS
	jz epop
Csemty:	pushl $M_STKMTY	//<;Xloc)
	js qerp
	popl TS
    	addl $M_STKOVF-M_STKMTY,TS
	jmp qerj
//
Xaerr:	pushl $M_AMODE	//; addressing mode
	jmp qern
// ( -- abort-0 )
Xnimp:	pushl $M_NIMP
	spush
	popl TS
// ( n -- in@ blk@ | abort ) ; [error]
Xerror:	pushl NXL	//<Xcplbrac,Xisv2)
Cerror:	movl uref(warningu),ecx//<Cnumber,Ctick,Cvalc,Cqerror,qerj)
	call cerd	//;display last word & error source
	testl ecx,ecx
	jns 8f
	leal pabortq+4,W//; "[abort]" if warning@ < 0
	jmp Cudfer
8:	lea quit,W
	jmp exent
// ;display last word & error source to stderr
cerd:	pushl ecx	//nur!<Cerror)
	pushl rstdout+chpv
	testl ecx,ecx
	jz 1f		//;message text if warning@=0
	pushl TS
	movb $rstderr,rstdout+chpv
	call Cbel
	call Csprst
	movl uref(inu),TS
	spush
	movl uref(blku),TS
	SPUSH(4)
	movl uref(lastu),TS
	call Ciddot
	movl uref(dpu),TS
	call Cprint	//; (filter & 'emit' printing chrs)
	movl $0x203f20,TS//; " ? "
	call cdemit
	popl TS		//;rst msg#
	cmpl $0,(esp)	//;@warning
	jng 2f		//;latest input & message text if >0
1:	call Cmessage	//<;warning@ = 0, message by number
2:	popl rstdout+chpv
	popl ecx	//; rst @warning
	ret
.nolist
// -----------------------------------------------------------------------------
// erms.S <eof>
