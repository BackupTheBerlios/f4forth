//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; fthdebug.S 
// =============================================================================
.psize 0
.att_syntax noprefix
.list
.text
	
//; ---- debugging
.ifne _tf_
// ; debugging <expect>
cpzs://; : f? pushregs flg >rr flg>smb type space r>flg popregs ;
	pushal
	pushfl
	pushfl
	spush
	popl ecx
	movl $0x535a5043,TS//;SZPC
	sarb $1,cl	//;C
	jc 1f
	movb $'-',al
1:	sarb $2,cl	//;P
	jc 1f
	movb $'-',ah
1:	sarb $4,cl	//;Z,S
	bswap TS
	jc 1f
	movb $'-',ah
1:	js 1f
	movb $'-',al
1:	bswap TS
	call cdemit////Cemit
	call Cspace
	popfl
	popal
	ret
.endif
.ifne _tm_
tem://; push flg >rr >r .s r 255 and 512 or 2 b. r f? dup 16 b. ( vt-char) tib @ 80 dump emit cr r>flg pop
	pushal
	pushfl
	pushfl
	call Cdots
	spush
	popl TS
	spush
	popl TS
	andl $255,TS
	pushl $2
	orl $512,TS
	spush
	popl TS
	call Cbdot
	popfl
	pushfl
	call cpzs
	spush
	movl $16,TS
	call Cbdot
	spush
	pushl $80
	movl uref(tibu),TS
	spush
	popl TS
	call Cdump
	call cdemit
	call Ccr
	popfl
	popal
	ret
.endif

.ifne _i3_
Xint3:	int $3
	NX
.endif
// ( -- n ) ; ret no. of current debug mode, revert to dft if invalid
Xtdbq:	spush
	xorl TS,TS
	cmpl $nextloc,NXL
	jz 1f
	incl TS
	cmpl $dbnextl0,NXL
	jz 1f
	incl TS
	cmpl $dbnextl1,NXL
	jz 1f
	incl TS
	cmpl $dbnextl2,NXL
	jz 1f
	incl TS
	cmpl $dbnextl3,NXL
	jz 1f
	incl TS
	cmpl $dbnextl4,NXL
	jz 1f
	negl TS
	leal nextloc,NXL
1:	NX		//<)
// ( n -- ) ;determine debug/dft <next> routine
Xtdb:	pushl NXL	//; set non/debugging display mode
Ctdb:	cmpl $ESC,TS	//<dbnext(xx)); back to dft w. <esc>
	jnz 6f
	pushl $Xquit	//;immediately terminate
6:	pushl edx	//Ctdb)
	pushl ecx
	cmpl $'-',TS	//;ignore next instr.
	jnz 7f
	movl (IP),edx
	cmpl $semis,edx
	jz 8f
	call ctbra
	jnz 8f
	addl $4,IP
	cmpl $dlit,edx
	jnz 8f
	addl $4,IP
8:	notl TS		//<6)
7:	cmpl $'0',TS	//<6)
	jc 5f
	subl $'0',TS
5:	testl TS,TS
	js 3f	//; leave as is if code below "0" or -ve num
	leal nextloc,edx
	jz 2f	//; 0: no debugging
	leal dbnextl0,edx
	decl TS	//; 1: continuous names' display
	jz 2f
	leal dbnextl1,edx
	decl TS	//; 2: names + wait for key
	jz 2f
	leal dbnextl2,edx
	decl TS	//; 3: names + stack + wait for key
	jz 2f
	leal dbnextl3,edx
	decl TS	//; 4: names + wait for key above "task"
	jz 2f
	leal dbnextl4,edx
	decl TS	//; 5: latent, stand-by mode
	jnz 3f
2:	movl edx,NXL
3:	popl ecx
	popl edx
	spop
	ret
// ; print name of next word in current hi-level list
Xndu:	pushl NXL
Cndu:	SPUSH(2)	//<;dbnext(xx)
	movl $vtelr,TS
	call cvtseq
	movl IP,TS
	call Cidplus
	spop
	ret
// ( -- ) ;toggle [ok] and .S xec by (ok)
Xo:	leal dots,ecx
	movl uref(vbotu),edx
	cmpl okev-D(edx),ecx
	jnz 1f
	leal bokb,ecx
1:	movl ecx,okev-D(edx)
	NX
//
.ifne _tm_
// ( -- ) ; dump tib
Xt:	pushl $99
	spush
	movl uref(tibu),TS
	spush
	popl TS		//; :contd:-> Xdump
.endif
// ( a n -- )
Xdump:	pushl NXL	//<Xt):c:
Cdump:	testl TS,TS
	jng 1f		//;?;2drop & ret
	call Ccr
	pushl uref(baseu)
	pushl $16
	spush
	popl uref(baseu)
	movl TOS(2),TS
	call Cpudnum	//; requested address
	addl $15,TS
	pushl $-1
	andl $-16,TS	//; dump 16 bytes/line
	movl TOS(1),ecx
	andl $-16,TOS(1)//; begin 16-aligned
	addl $16,(esp)
6:	spush
	mov TOS(2),TS
	incl TOS(2)
	cmpl TS,ecx
	pushl ecx
	jnz 7f
	movl $'-'<<8|BL,TS//; mark requested addr
	call cdemit
	jmp 8f
7:	call cpudig	//; byte
8:	call Cspace
	popl ecx
	decl (esp)
	jns 6b
	subl $16,TOS(1)
5:	call Ccr
	spush
	addl $16,(esp)
	movl TOS(2),TS
	call Cpudnum	//; address
2:	spush
	movl TOS(2),ecx	//; addr
	incl TOS(2)
	movzbl (ecx),TS
	call Cpudnum	//; byte
	decl (esp)
	jns 2b
	call Cspace
	subl $16,TOS(1)
	addl $16,(esp)
4:	spush
	movl TOS(2),ecx	//; addr
	incl TOS(2)
	movzbl (ecx),TS
	pushl TS
	call Ckey2chare
	popl TS
	jnz 3f
	movl $'_',TS
3:	call cdemit
	decl (esp)
	jns 4b
	subl $16,TS
	jns 5b
	call Ccr
	popl TS
	popl uref(baseu)
1:	SPOP(2)
	ret
//
cpudig:	call Cspace	//<Cdump)
	spush
	call Clesssharp
	call Csharp	//; 1st digit
cputype:call Csharpgreat//<cpubseq)
	jmp jctype
//
cpubseq:pushl TOS(1)	//<Cdots)
	call cdemit 	//; leading char @tos
	xorl TS,TS	//; unsigned sing
	call Clesssharp
	call Csharp
	call Csharps	//; min 2 digits
	call cputype	//; finish num, type
	popl ecx
	testl ecx,ecx
	jnz 2f		//; trailing char @nos
	ret
2:	spush
	movl ecx,TS
	jmp cdemit
// ( ud -- )
Cpudnum:spush		//<Cdump)
	xorl TS,TS
	call Cpudnump	//; (udnum)
jctype:	leal type+4,W	//<cpudig)
	jmp Cudfer
// ( -- )
Xdots:	pushl NXL
Cdots:	pushfl		//<dbnext(xx); "["base":"depth"]"
	pushal
	spush
	pushl uref(hldu)
	pushl uref(dpu)
	movl $page_size*4,TS
	pushl TS
	call Callot
	movl uref(dcsizu),ecx//; 'local'ly use upper end of dictionary for numeric strings conversion
	addl uref(dcbotu),ecx
	subl $1024,ecx
	movl ecx,uref(dpu)
	call Cdepth	//; depth
	spush
	spush
	movl $BL<<8|']',TS
	pushl $':'
	spush
	popl TS
	call Cbasef
	pushl TS
	movl $10,uref(baseu)
	spush
	xorl TS,TS
	spush
	addl $'[',TS
	call cpubseq	//; "["base
	call cpubseq	//; ":"depth"] "
	popl uref(baseu)
	testl TS,TS	//; depth
	jng 1f
	cmpl consv(dotsn),TS
	jc 3f
	movl consv(dotsn),TS
3:	pushl TS
4:	spush
	movl (esp),TS
	notl TS
	movl 0(PS,TS,4),TS//; pick
	spush
	sarl $31,TS
	jz 2f
	cmpl $10,uref(baseu)
	jz 2f
	xorl TS,TS
2:	call Cpnump
	call jctype
	call Cspace
	decl (esp)
	jg 4b
	addl $4,esp
1:	call Ccr
	popl TS
	negl TS
	call Callot
	popl uref(dpu)
	popl uref(hldu)
	spop
	popal
	popfl
	ret
//<Xsee)
8:	movl uref(vbotu),ecx//<Xsee:14)
7:	addl (TS),ecx
	TOSPULL(1)
	movl (ecx),TS
	testl TS,TS
	jz 2f
	addl $cfa2pfa,TS
// ( a -- a1 u )
Xnsee:	spush		//; copy start addr
	cmpl $docold,TS
2:	jna cidnf	//<Xsee:7)>Xiddot);?; '-?-'
	movl uref(dpu),edx
	jmp 9f		//>Xsee)
// ( a -- a u ) ; 'see' by pfa, for <debug>
Xpseep:	testl TS,TS
	TOSPUSH(3)
	jmp 13f		//>Xsee)
// ccc( -- a u )
Xsee:	call Cdashfind
	spush
13:	jz 3f		//<Xpseep)
	movl TOS(3),TS
	movl TS,TOS(1)	//; ( .. -- pfa flg pfa pfa )
	call cidnum	//;display pfa
	call ctopnfa	//; ( .. -- pfa flg pfa )
	cmpl ecx,edx
	jnz 14f
	movl uref(dpu),ecx
14:	andl $-4,ecx	//<);next nfa -ve aligned to dword=cfa bounds
	movl edx,TS
	pushl ecx	//;next nfa
	call Ciddot	//;name
	call Ccr	//; ( .. -- pfa flg )
	movl TOS(1),TS	//; ( .. -- pfa pfa )
	popl edx
	cmpl $domdfer,-cfa2pfa(TS)
	jz 8b		//>nsee);deferred
	movl UR,ecx
	cmpl $doudfer,-cfa2pfa(TS)
	jz 8b		//>nsee);?; uvari deferred
	call ctddsg
	jnz 9f		//;?; nicht dodoes &c
// redirect hi-level disassembly into dodoes part of defing word
// <pfa> cfa @ 4- @ c>pfa nfa id.
	movl TS,TOS(1)
	SPUSH(2)
	pushl edx
	pushl ecx
	call creve
	movl -cfa2pfa(TS),TS
	movl -4(TS),TS
	addl $cfa2pfa,TS
	call Cnfa
	call Ciddot	//; run-time execution word
	movl (TS),TS
	call ctopnfa
	movl edx,TS	//; nfae of defining word
	call Ciddot
	call crevo
	movl TOS(1),TS
	popl ecx
	popl edx
	movl (TS),TS	//; entry into hi-level run-time part 
	movl TS,TOS(1)
9:	pushl edx	//<14,Xnsee:2); next nfa|0
	pushl TS	//; save start addr
	incl (esp)
	leal bseeb+4,W	//; [see] pfa
	call Cmdfer	//;test/xec extension code
	testl TS,TS
	jz 4f		//;?;abbruch
1:	cmpl (esp),TS	//<6)
	jc 12f		//;?;1st step
	cmpl $lit,-8(TS)
	jz 11f
12:	movl -4(TS),edx	//<1)
	call ctis
	jz 4f		//;?;cons/vari &c
11:	cmpl 4(esp),TS	//<1)
	jnc 4f		//;not at next nfa | >0 after #see
	pushl TS	//; curent IP for ";S" check
	call Cidplus
	popl ecx
	cmpl $semis,(ecx)
	jz 4f		//;?; terminate at ';'
0:	cmpl (esp),TS
	jc 4f		//;?; back to before entry point, somehow...
	cmpl $branch,(ecx)
	jz 5f		//;?; terminal branch into some preceding colon defn
6:	call Cqesck	//<5); ?esc (ecx preserved)
	spop
	jz 1b		//;?>1)
4:	popl edx	//<0,5,9,11,12);drop end marker
	popl edx	//;drop start addr
3:	jmp Xoverdash	//<13)
5:	addl 4(ecx),ecx	//<0); a branch
	cmpl (esp),ecx
	jnc 6b		//;?>6)
	spush
	movl ecx,TS
	spush
	call cidnum	//; display cfa
	addl $cfa2pfa,TS
	call Cnfa
	call Ciddot	//;  ..and name of terminal word
	jmp 4b		//>4)
// ; debug mode display, branches check
ctbra:	movl $brat,ecx	//<Cndu,Csee)
9:	cmpl (ecx),edx	//<)
	jz 8f
	addl $4,ecx
	cmpl $0,(ecx)
	jnz 9b		//<)
	testl ecx,ecx
8:	ret
//<Cidplus) ( cfa -- nfa ) ; Z if not a nfa
ccnfa:	movsbl -(1+cfa2pfa)(TS),ecx//; -ve count
	lea -(2+cfa2pfa)(TS,ecx),TS
	negb cl
	jz 1f		//;?;0 is not a name count
	jns 2f		//;?;-ve back-disp is valid
	movb (TS),cl	//;force Z
2:	subb (TS),cl
	andb $~(smud|imed|unus),cl
1:	ret
//<Cidplus)
cttext:	pushl edi
	leal txtt+4,edi
	movl -4(edi),ecx
	repne scasl
	popl edi
	ret
//<;Cidplus)
cidnum:	pushl $16
	pushl $'$'
	spush
	spush
	popl TS
	call cdemit
	popl TS
	jmp Cbdot
//<;Cidplus)
cidtext:pushl $BS	//; ...'(")text" '...
	spush
	spush
	popl TS
	call cdemit
	call Ccount
	movl TOS(1),ecx
	leal 1(TS,ecx),edx
	movl edx,TOS(2)
	call Cptype
	spush
	movl $BL<<8|'"',TS
	jmp cdemit
// ; ret from redirected [id]
14:	spop		//<Cidplus)
	ret
// ( a -- a' ) ; 'id+'
Xidplus:pushl NXL
Cidplus:cmpl $noop-lfa2pfa-6,TS//<;Xsee)
	jc 7f		//;?; below word headers range
	call Cfetchplus
	cmpl $noop-lfa2pfa-6,TS
	jc 7f		//;?; below word headers range
	cmpl $task,TS
	jc 13f		//;?; kernel word
	leal bidb+4,W	//; [id] pfa
	call Cmdfer	//;test/xec extension code
	cmpl $bidbt,TS	//; flag "]id[
	jz 14b		//;?; set
13:	call cttext	//<)
	jnz 11f		//; not a printing stg
12:	pushl $cidtext
11:	movl TS,edx	//<)
	call ctbra	//;check whether cfa is a branch word
	pushfl
	call ccnfa	//;cfa>nfa
	jnz 3f		//;?;not a name
	call Ciddot
	popfl
	jz 1f		//; branch or (d)lit
	ret
//
3:	popl TS		//<Cidplus:11)
4:	jmp 7f
//
1:	call Cfetchplus	//<Cidplus:11)
	cmpl $dlit,edx
	jnz 7f
	call cidnum
	call Cfetchplus
7:	jmp cidnum	//<Cidplus,1)
//
// ccc( -- )
Xv:	call Cafind
	jz 1f
	movl TOS(2),TS
	call Cnfa
	pushl TS
	call Ciddot
	popl TS
2:	call Cnnfa
	cmpw $vnam,(TS)
	jnz 2b		//>)
	addl $vnfa2vlfa+lfa2pfa,TS
	call Cvocnfa
	call Ciddot
	subl $cfa2pfa,TS
1:	pushl $16	//<)
	spush
	popl TS
	jmp Xbdot
// ( nfa -- )
Xiddot:	pushl NXL	//<Xidplus)
Ciddot:	spush		//<Xsee,Cidplus,Xvlist,Ccreate,cerd(Cerror))
	cmpl $noop-lfa2cfa-4-2,TS
	jc cidnf	//;?;not in word headers memory
	incl TOS(1)	//; chars addr
	movzbl (TS),ecx	//; count+flags
	movl TS,edx	//;nfa
	pushl ecx	//;flags
	andb $~(smud|imed|unus),cl
	jz cidnf	//;?;zero count is not a name
	movzbl cl,TS	//;count
	addb 1(edx,ecx),cl
	jnz cidnf	//;?;not a word header
	call Cprintingq
	popl ecx
	jz cidnf	//;?; not all printing chars
	testl $imed,ecx
	jnz crevwp
3:	call Cptype	//<Crevwp,Ciddot)
10:	jmp Cspace	//<crevwp)
11:	popl TS		//;drop flag-byte
cidnf:	pushl $3	//<Xnsee)
	movl $nptt,TOS(1)
	popl TS
	jmp 3b
// ( a u -- )
crevwp:	call crevt	//<Ciddot);'type' w. reversed video
	jmp 10b		//>Ciddot)
cmaxxy:	leal maxxy+4,W
	jmp Cudfer
// ( -- )
Xvlist:	call cmaxxy	// ;( -- width height )
	subl $4,TOS(1)
	jg 1f
	movzwl 2+uref(wsizu),ecx
	movl ecx,TOS(1)
1:	movl uref(contextu),TS
	spush
	addl $lfa2pfa,TS
	call Cvocnfa
	call Ciddot
	movl (TS),TS
	jmp 6f
2:	cmpl $2,TOS(1)
	js 6f
	addl uref(outu),ecx
	cmpl TOS(1),ecx	//;window width
	jc 3f		//;?; u< 
6:	pushl $2
	call Ccr
	popl uref(outu)
3:	movl (TS),ecx
	shll $8,ecx
	cmpl $((vnfd)&-0x100),ecx
	jz 4f		//; voc ends
	spush
	call Ciddot
	movl (TS),ecx
	andl $maxw,ecx
	leal 2(ecx,TS),edx
	addl (edx),edx	//;prev. lfa
	cmpl $0,(edx)
	jz 4f
	movsbl -1(edx),TS
	lea -2(TS,edx),TS//;nfa
	call Cqesck
	spop
	jz 2b		//;cont if no input
4:	SPOP(2)
	NX
// ccc( -- flg ) ; tf if word ccc found
Xfoundq:pushl NXL
Cfoundq:call Cdashfind	//<;Xiffnfound,Xiffound)
	jz 5f
	TOSPULL(2)
5:	ret
.nolist
// -----------------------------------------------------------------------------
// fthdebug.S <eof>
