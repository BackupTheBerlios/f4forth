//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; cpldef.S - compiler(s)
// =============================================================================
.psize 0
.att_syntax noprefix
.text
.list
//; ---- dictionary, compiling 
// T:ecx W:edx

cindiq:	movl uref(currentu),T//<Ximmediate,Csmudge,Xunsmudge)
	movl (T),W
// ; nein, nicht <ein rind für'n autozirkus> sondern: 'in dictionary?'
indiq:	cmpl W,uref(dpu)//<Cosemi)
	jc 1f		//;?; address > 'here'
	cmpl W,uref(fenceu)
	cmc		//; below protected or program addr range
1:	ret		//; C if addr not in dictionary
// ( -- ); cpl jmp to <nextloc>
Xnextcomma:
	movl uref(dpu),T
	movl 1f,W
	addl $2,uref(dpu)
	movw Wx,(T)
1:	NX
// ( c -- ); cpl cell in vari-mem
Xvcomma:pushl NXL
Cvcomma:pushl TS
	pushl $4
	popl TS
	call Cvalc
	popl -4(edx)
	ret
// ( n -- ); cpl byte in vari-mem
Xwvcomma:
	pushl TS
	pushl $2
	popl TS
	call Cvalc
	popl ecx
	movw cx,-2(edx)
	NX
// ( n -- ); cpl byte in vari-mem
Xcvcomma:
	pushl TS
	pushl $1
	popl TS
	call Cvalc
	popl ecx
	movb cl,-1(edx)
	NX
// ( n -- ); cpl l.s. bytes of n to vmem
Xbvcomma:
	movl TS,ecx
	call cbytes
	spush
	movl uref(vpu),TS
	spush
	movl edx,TS
	call Cvalc
	addl uref(vbotu),TS
	jmp Xbstore
// ---- "+to" runtime code ----
Xpstou:	movl (TS),TS	//;mem by disp wrt UP
1:	movl TOS(1),ecx
	addl UR,TS
	addl ecx,(TS)
	SPOP(2)
	NX
Xpstobu:movsbl (TS),TS	//;mem by byte-disp wrt UP
	jmp 1b
Xpstom:	movl TOS(1),ecx	//;mem by pc-relative ptr
	addl (TS),TS
	addl ecx,(TS)
	SPOP(2)
	NX
Xpstov:	call cparmv
	addl ecx,(edx)
	NX
Xpstov2:call cparmv
	addl TS,(edx)
	adcl ecx,4(edx)
	spop
	NX
// ---- "is" runtime code ----
cparmv:	movl uref(vbotu),edx
cparmu:	addl (TS),edx
	movl TOS(1),ecx
	SPOP(2)
	ret
//
Xtwostov:		//;double by disp wrt @(vb)
	call cparmv
	mov TS,(edx)
	movl ecx,4(edx)
	spop
	NX
Xisumr:	movl UR,edx	//;user-deferred
	call cparmu
1:	cmpl $0,ecx
	jz 2f
	subl $cfa2pfa,ecx//; convert src pfa to cfa
2:	movl ecx,(edx)
	NX
Xisvmr:	call cparmv	//;deferred by disp wrt @(vb)
	jmp 1b
Xstorev:call cparmv
	movl ecx,(edx)
	NX
Xstorem:movl TOS(1),ecx	//;mem by pc-relative ptr
	addl (TS),TS
	movl ecx,(TS)
	SPOP(2)
	NX
Xstoreu:movl (TS),TS	//;mem by disp wrt UP
1:	movl TOS(1),ecx
	addl UR,TS
	movl ecx,(TS)
	SPOP(2)
	NX
Xstorbu:movsbl (TS),TS	//;mem by byte-disp wrt UP
	jmp 1b
.ifne _to_
// ccc( n -- ) ; store to, or create a constant
Xtocons:call Cqexec
	call Csinput
	call Cdashfind
	call Crinput
	testl TS,TS
	spop
	jz Xvcons
	SPOP(2)
.endif
// X: ccc( pfa|n|d -- ) ; modify 'defer'ed word, or vari & cons words
Xis:	call Ctick
	cmpl $0,uref(stateu)
	jnz 6f		//;?; cpl
3:	movl -cfa2pfa(TS),edx//<Xislt); data item @TOS(1)
	call ctis	//;find corresponding storeage word
	jnz 2f		//;?;not a suitable item
	jmp exent	//;execute
// ; cpl'd 'is<'
4:	movl uref(dpu),ecx
	movl -4(ecx),TS	//; pfa
	movl 4(TS),edx	//; dft pfa
	movl edx,-4(ecx)//; modify parameter to {is}
	movl $dlit,-8(ecx)
	call Ccomma	//; cpl destn pfa
// ; cpl'd 'is'
6:	pushl NXL
	movl uref(dpu),ecx
	movl -4(ecx),edx//; cpld destn address
	movl -cfa2pfa(edx),edx
	call ctis
	jz ccommad	//;?;cpl
2:	jmp Xaerr	//<Xis,Xpto); abort-3 if not found
// ;type of ref adr, edx:=storeage cfa; NZ if not found
ctis:	leal isrefc-4,ecx//<Xis,Xsee)
ctisn:	pushl edi
	pushl TS
	leal 4(ecx),edi
	movl edx,TS
	movl (ecx),ecx
	pushl ecx
	shrl $2,ecx
	repnz scasl
	jnz 1f
	movl (esp),TS
	movl -4(edi,TS),edx
1:	popl edi	//; drop table size
	popl TS
	popl edi
	ret
// X: ccc( n|d|f -- ) ; '+to' add @tos item to any type vari, cons
Xpto:	call Ctick
	movl uref(dpu),ecx
	cmpl $0,uref(stateu)
	jz 8f
	movl -4(ecx),edx
	spush
	pushl edx
	movl -cfa2pfa(edx),TS
8:	leal bptob+4,W	//; ( n|d|f -- pfa|0 ) ; [+to] extension code
	call Cmdfer
	testl TS,TS
	jz Xdrop	//;?; @tos=0 is flag for xtn code execution
	movl TS,edx
	cmpl $0,uref(stateu)
	jz 7f		//;?; interpreting
	spop
	popl edx
7:	movl -cfa2pfa(edx),edx//;data item @TOS(1)
	leal ptrefc-4,ecx
	call ctisn
	jnz 2b		//;?; not a suitable word
	cmpl $0,uref(stateu)
	jz exent	//;?;execute
	pushl NXL
	jmp ccommad	//;?;cpl
// ccc( -- ) ; 'is<' revert deferred to default procedure
Xislt:	spush
	call Ctick
	cmpl $0,uref(stateu)
	jnz 4b		//;?; cpl
	movl 4(TS),ecx
	movl ecx,TOS(1)
	jmp 3b		//>Xis)
// hhh www( -- ); forget: (i) assign forget handler hhh to word www
Xforgetco:
	call Ctick	//; deferred handler
	call Ctick	//; word
	cmpl $0,uref(stateu)
	jnz 2f
// ( a1 a2 -- ) ; [forget] assign forget handler by pfa a1 to word by pfa a2
Xbforgetb://	; here to (fl)@ cpl prv val to dict, cpl a2-nfa, cpl a1-pfa => [ prevlink ][ word-nfa ][ hnd-pfa ]
	movl TOS(1),edx	//; hnd-pfa
	movl -cfa2pfa(edx),ecx
	cmpl $domdfer,ecx
	jnz Xaerr	//; abort-3, addressing mode, if hnd not a deferred word
	movl uref(pflpu),edx//; (fl) @
	movl uref(vpu),ecx//; here
	spush
	pushl ecx
	testl edx,edx
	jz 1f		//;?; 1st link, chain ends, cpl <0>
	subl ecx,edx
1:	movl edx,TS
	call Cvcomma	//;here (fl) @ dup if over- endif ,
	popl ecx
	movl ecx,uref(pflpu) //; (fl) ! nfa , , [ 0 cmc ! ] ;
	call Cnfa
	call Cvcomma
	jmp Xvcomma
// ; cpl [forget]
2:	pushl NXL
	leal bfgtb,edx
	jmp ccomma0
//
// ---- conditonals ----

// ( -- ) ; last @ pfa cfa , 1 cmc +!
Xrecurse:spush
	addl $1,uref(cmcu)
	movl uref(lastu),TS
	call Cpfap	//; nfa -> pfa+
	jmp Xcomma
// ( -- a 1 )
Xbegin:	call Cqcomp
	spush
	pushl $f_begin//;1
	movl uref(dpu),TS
	spush
	call Cnulcmc
	popl TS
	NX
// ( a 1 -- a 1 a1 4 )
Xwhile:	call Cif
	addl $2,TS
	NX
// ( a 1 [ a1 4 ] -- )
Xuntil:	pushl $-1	//; flag a 'while' or, not
	cmpl $f_begin,TS//; "begin"
	jz 1f
	call Cswap2	//; "while"
	incl (esp)	//; -> 0
1:	pushl $1
	spush
	popl TS
	call cqpairs
	call cozbra	//; convert 0< 0=
	call againn	//; w. cmc := 0
	popl ecx
	testl ecx,ecx
	jz reptn	//;?; resolve 'while'..
	NX
// ( a 1 -- )
Xagain:	pushl NXL
Cagain:	pushl $f_begin	//<Xrepeat)
	spush
	popl TS
	call cqpairs	//; ?comp ?pairs
	movl $branch,edx
againn:	call ccommad
	jmp Cback
// ; one,questpairs,compile,zerobranch,back,semis
// ( -- a 5 )
Xenter:	call Cqcomp
	movl $branch,edx
	spush
	call ccomma0
	movl uref(dpu),TS
	pushl $f_enter
	spush 
	popl TS
	spush
	jmp Xcomma
// ( a 2 -- a' 2 ) ; pack: exit else..; dexit else..
Xelse:	decl uref(cmcu)
	js 2f		//;?;don't opt
	movl uref(dpu),ecx
	cmpl $drops,-4(ecx)
	jz 3f		//;?;don't cpl the 'branch'
	cmpl $semis,-4(ecx)
	jz 3f		//;?;don't cpl the 'branch'
2:	pushl $f_if
	call qpairs2
	movl $branch,edx
	call ccommad	//;compile,branch
	spush
	pushl uref(dpu)
	call clitr0	//;here,zero,comma
	movl TS,edx
	popl TS		//( a -- here )
	spush
	movl (esp),TS	//( -- h 2 )
	spush
	movl edx,TS	//( -- h 2 a 2 )
	spush		//;swapx,two,endif,two,semis
	popl TS		//:contd: Xendif
// ( a 2 -- )
Xendif:	call qpairs2	//; 2 ?pairs ?comp
	call Cnulcmc
	testl TS,TS
	jz 1f		//;?; silently discard after { ..exit else.. }
	movl uref(dpu),ecx
	subl TS,ecx
	movl ecx,(TS)
1:	spop
	NX
3:	movl TOS(1),ecx	//<Xelse)
	movl $0,TOS(1)	//; deactivate regular 'endif'
	spush;spush
	movl ecx,TOS(1)
	jmp Xendif
// ( a 1 a1 4 -- )
Xrepeat:call Cswap2
	call Cagain
reptn:	subl $f_if,TS
	jmp Xendif
// ( a 5 x x -- x x )
Xentry:	call Cswap2
	subl $f_enter-f_if,TS
	jmp Xendif
// ( xx -- xx ) ; modified branches conditions
cozbra:	movl uref(dpu),ecx//<Xuntil,Cif)
	leal ift,edx	//; 0branch (if)
	decl uref(cmcu)
	js 1f		//;?; no opt
// whether true or inverse condition:
	cmpl $zeq,-4(ecx)
	jnz 4f		//;?; nicht (0=); test +bra
	leal -4(ecx),ecx
	leal 4(edx),edx	//; -branch
	decl uref(cmcu)
	js 2f
// 0< or 0>/ modifier
4:	cmpl $zlt,-4(ecx)//; ( 0< )
	jnz 2f		//; if:0branch; 0= if:-branch
	leal 8(edx),edx	//; +bra  oder -bra
	leal -4(ecx),ecx//; 0< if oder 0< 0= if 
//// ;// whether r> [ 0< ][ 0= ] if	//:HIER:
2:////	cmpl $rgt,-4(ecx)//; ( 0< )
////	jnz 3f
////	leal 16(edx),edx
////	leal -4(ecx),ecx//; r> .. if 
3:	movl ecx,uref(dpu)
1:	movl (edx),edx
	ret
// ( n -- )
Xdleave:testl TS,TS
	spop
	jz 1f
Xleave:	movl (RS),T	//:contd: Xdleave)
	movl T,4(RS)
1:	NX		//<Xdleave)
// ( n1 n2 -- )
Xdo:	call Cqcomp
	movl $pdoq,edx
	spush
	call ccommad
	pushl $f_do
	movl uref(dpu),TS
	spush
	popl TS
	NX
// ( n -- )
Xplusloop:
	pushl $pplusloopq
	jmp 1f
// ( -- )
Xloop:	pushl $ploopq
1:	pushl $f_do
	spush
	popl TS
	call cqpairs
	popl edx
	call ccommad
	jmp Xback
// (X: n1 n2 -- | n1 ); '=if'
Xqif:	pushl NXL
	movl $neqbra,edx
	spush
	jmp 1f
// ( -- a 2 ) ; 'if', optimizing 0=, 0<, 0< 0=, also w. 'while'
Xif:	pushl NXL
Cif:	spush		//<Xwhile)
	call cozbra
1:	pushl $f_if
	call ccommad
	movl uref(dpu),TS
	SPUSH(2)
	popl TOS(1)	//; -- flg here )
	jmp clitr0
// ( a -- )
Xback:	pushl NXL	//<Xloop)
Cback:	subl uref(dpu),TS//<Cagain)
	jmp clitr0

// ---- cpl literals & wa ----

//<Cliteral); ret Z and discard if '+' follows, else restore 'in' & 'blk'
ctlp:	pushl TS
	pushl edi
	pushl uref(inu)
	pushl uref(blku)
	movl uref(dpu),ecx
	andl $0,(ecx)
	call Cblword
	leal ctlpt,edi
	movl $(ctlpc-ctlpt)/4-1,ecx
	movl (eax),eax
	repnz scasl
	movl ctlpc-ctlpt-4(edi),eax
	popl edx	//;'blk'
	popl ecx	//;'in'
	jz 1f
	movl ecx,uref(inu)
	movl edx,uref(blku)
1:	movl eax,edx
	popl edi
	popl TS
	ret
// ccc( -- pfa | abort ); ''' dashfind,zeq,zero,questerror,drop,literal,semis
Xtick:	pushl NXL
Ctick:	call Cdashfind	//<Xis)
	call 7f		//; don't cpl combined code
	testl TS,TS
	jz Cerror	//;?;not found
	SPOP(2)
	cmpl $0,uref(stateu)
	jnz 6f		//>Cliteral);?; compiling
	ret
//<Cliteral); 'lit (n) lit (n)' => 'dlit (dn)'
5:	movl $dlit,-8(ecx)
	jmp 2f
// ccc( -- )(X: -- n) ccc(I: -- n )
Xascii:	call Cblword
	movzbl 1(TS),TS	//:contd: Xliteral
// (C: n -- )(X: -- n )(I: n -- n )
Xliteral:		//<Xasacii)
	pushl NXL
Cliteral:		//<Xinter)
	cmpl $0,uref(stateu)
	jz 7f		//;?;ret, 'noop' if interpreting
	cmpl $1,uref(cmou)
	jc 3f		//;?; opt. off
	call ctlp	//; test for '+' &c
	jz 1f		//;?; cpl 'lit+' &c
3:	call 7f		//; don't cpl combined code
6:	movl uref(dpu),ecx//<Ctick)
	leal lit,edx	//; 'lit' dft opr
	cmpl -8(ecx),edx
	jnz 1f		//;?;not a 2nd lit
	cmpl $1,uref(cmou)
	jc if		//;?; opt. off
	decl uref(cmcu)
	jns 5b		//;?;in same structure
1:	btl $sgdfb,uref(bitsu)
	jnc 8f		//;?; 'unsigned' not set
	leal liti,ecx
	cmpl $litm,edx
	jz 9f
	cmpl $litd,edx
	jnz 8f
	leal litu-liti(ecx),ecx
9:	movl ecx,edx
8:	call ccommad	//<6);not convertible
2:	movl $1,uref(cmcu)//<5)
	call Ccomma
7:	btrl $sgdfb,uref(bitsu)//<Ctick,Cliteral)
	ret
.ifne 0
	cmpl $0,uref(stateu)
	jz 7b		//>Ccomma);?;ret if interpreting
6:	movl uref(dpu),ecx//<Ctick)
	leal lit,edx
	cmpl -8(ecx),edx
	jnz 1f		//;?;not a 2nd lit
	subl $1,uref(cmcu)
	jns 5b		//;?;in same structure
1:	call ccommad	//<6);not convertible
2:	movl $1,uref(cmcu)//<5)
	jmp Ccomma
.endif
//
clitr0:	call Cnulcmc	//<Xelse,Cif,Cback)
Ccomma:	pushl TS
3:	pushl $4	//<ccommad)
	popl TS
	call Callot
	popl -4(edx)
4:	ret		//<Cdliteral)
//
ccomma0:call Cnulcmc	//<Xbforgetb,Xenter,Cdliteral,Xcompile,Xdexit,Xsemicode)
ccommad:pushl edx	// ; cpl <edx> content
	spush
	jmp 3b		//>Ccomma)
// ( n -- )
Xcomma:	pushl NXL
	jmp Ccomma
// (C: d -- )(X: -- d )(I: d -- d )
Xdliteral:
	pushl NXL
Cdliteral:		//<Xinter)
	cmpl $0,uref(stateu)
	jz 4b		//>Ccomma); ret
	leal dlit,edx
	call ccomma0
	pushl TS
	movl TOS(1),TS
	call Ccomma
	popl TS
	jmp Ccomma
// ccc( -- )
Xcplbrac:
	call Cdashfind
	jz Xerror	//; 0 error
	movl TOS(1),ecx	//; flag-byte
	SPOP(2)
	subl $cfa2pfa,TS
	cmpl $0,uref(stateu)
	jz ccnx		//>Xccomma)
	cmpb $imed|smud,cl
	jnc 2f
	leal compile,edx
	call ccommad
2:	call Cnulcmc
	jmp Xcomma
// ccc( -- )
Xcompile:
	pushl NXL
	call Cqcomp
	movl (IP),edx
	leal 4(IP),IP
	jmp ccommad

// ---- optimizing <exit> ---

// ( n -- ) ; '-exit'
Xdexit:	cmpl $0,uref(stateu)
	jz Xdsemis	//;?; conditionally terminate loading
	pushl NXL
	pushl $(dexitoc-exitoc)/4
	movl uref(dpu),ecx
	popl edx	//; '-exit ' ; <-;s>
	decl uref(cmcu)
	js 5f
	cmpl $zeq,-4(ecx)
	jnz 5f
	incl edx	//; ' 0= -exit ' : <+;s>
3:	decl uref(cmcu)	//<Cexit)
	cmpl $compile,-4(ecx)
	jz 0f		//;?; do not compact a 'compile'd cfa
	subl $4,uref(dpu)
1:	incl uref(cmcu)
5:	movl exitoc(,edx,4),edx
	jmp ccommad
0:	xorl edx,edx
	jmp 1b
// ; test & cpl branch to last colon defn before ';'
2:	call Cosemi	// ; ecx:=@dp-4, edx:=@ecx, ret Z(=edx) if branch cpl'd
	jnz 0b
	ret		//:nicht::trennen:\
// ( -- ) ; 'exit'	//:nicht::trennen:/
Xexit:	pushl NXL
Cexit:	xorl edx,edx	//<Xsemi)
	cmpl edx,uref(stateu)
	jz 5b		//;?; exit 'load'ing
	movl uref(dpu),ecx
	cmpl edx,uref(cmcu)
	jng 1b		//;?; cmc off or, less than one word after a structure
	subl $4,ecx
	incl edx
	cmpl $drop,(ecx)
	jz 3b		//;?; ' drop ; ' 
	incl edx
	cmpl $drop2,(ecx)
	jz 3b		//;?; ' 2drop ; '
	incl edx
	cmpl $drop4,(ecx)
	jz 3b		//;?; ' 4drop ; '
	incl edx
	cmpl $rdrop,(ecx)
	jz 3b		//;?; ' rdrop ; '
	incl edx
	cmpl $rdrop2,(ecx)
	jz 3b		//;?; ' 2rdrop ;'
	incl edx
	cmpl $sdrop2,(ecx)
	jz 3b		//;?; ' 2sdrop ; '
	incl edx
	cmpl $swap,(ecx)
	jz 3b		//;?; ' swap ; '
	incl edx
	cmpl $sw2,(ecx)
	jz 3b		//;?; ' 2swap ; '
	incl edx
	cmpl $rgreat,(ecx)
	jz 3b		//;?; ' r> ; '
	incl edx
	cmpl $rg2,(ecx)
	jz 3b		//;?; ' 2r> ; '
	incl edx
	cmpl $ndrop,(ecx)
	jz 3b
	incl edx
	cmpl $cswap,(ecx)
	jz 3b
// ``-- insert further tests here --'' //
	incl edx
	cmpl $sdrop,(ecx)
	jnz 2b		//;?; 'sdrop ; ' else cpl <semis> regularly
	cmpl $sdrop,-4(ecx)
	jnz 3b		//;?; single 'sdrop'
	cmpl $1,uref(cmcu)
	jng 3b		//;?; less than 2 words after a structure
	decl uref(cmcu)
	incl edx
	subl $4,uref(dpu)
	jmp 3b		//>; ' sdrop sdrop ; '
// ( n -- ) ; '-;s'
Xzsemis:addl TS,TS
	cmpl $1,TS
	sbbl TS,TS
// ( n -- ) ; ';s'
Xdsemis:addl TS,TS
	spop
	jz 1f		//>Xsemis)
Xsemis:	popl IP		//<Xdrops,X2drops); cpl'd by Xexit
1:	NX		//<Xdsemis)
X4drops:TOSPULL(2)	//;cpl'd by Xexit
// ( d -- )
X2drops:TOSPULL(1)	//;cpl'd by Xexit
// ( n -- )
Xdrops:	movl TOS(1),TS	//;cpl'd by Xexit
// ( n1 n2 -- n2 )
Xsdrops:TOSPULL(1)	//;cpl'd by Xexit
	jmp Xsemis
Xsdrops2:movl TOS(1),ecx
	movl ecx,TOS(3)
// ( n1 n2 n3 -- n3 )
Xs2drops:TOSPULL(2)	//;cpl'd by Xexit
	jmp Xsemis
// (R: d -- )
Xr2drops:popl IP	//;cpl'd by Xexit
// (R: n -- )
Xrdrops:popl IP		//;cpl'd by Xexit
	popl IP
	NX
// ( -- n )(R: n -- ) ; 'r> ;s'
Xrgreatx:spush		//;cpl'd by Xexit
	popl TS
	popl IP
	NX
// ( -- n1 n2 )(R: n1 n2 -- ) ; '2r> ;s'
Xr2greatx:		//;cpl'd by Xexit
	SPUSH(2)
	popl TS
	popl TOS(1)
	popl IP
	NX
// ( n -- )
Xccomma:movl uref(dpu),ecx
	incl uref(dpu)
	movb TSl,(ecx)
ccnx:	spop		//<Xcplbrac)
	NX
// ( n -- ) ; extend dictionary into .bss if less than <ds_min+n> bytes left
Xqallot:pushl NXL
	addl uref(dpu),TS
	jmp 2f
// ( n -- ) ; allot in dictionary, extend into .bss if less than <ds_min> bytes left
Xallot:	pushl NXL
Callot:	addl uref(dpu),TS//<Cdots,Ccomma,Xscomma)
	movl TS,uref(dpu)
2:	movl TS,edx	//Xqallot); pass "here" to caller
	movl uref(dcsizu),ecx
	addl uref(dcbotu),ecx
	addl $ds_min,TS
	cmpl TS,ecx
	jnc 1f
	pushl ebx
	leal page_align+ds_brk(TS),ebx
	andl $page_mask,ebx
	sys_ brk	//;extend .bss section
	popl ebx
	subl uref(dcbotu),TS
	movl TS,uref(dcsizu)
1:	spop	//<Callot)
	ret
// ( -- )
Xfcsp:	movl TS,(PS)
	movl uref(cspu),PS
	movl (PS),TS
	NX
// ( -- )
Xstorecsp:
	movl PS,uref(cspu)
	NX
// ( a -- )			//;def word header w. name by string
Xhead:	pushl NXL
	jmp 4f
// ccc( -- )
Xcreate:pushl NXL
Ccreate:call Cdashfind		//<Cconstant,Xcolon,C2vari)
	jz 2f			//;name not yet known
	cmpl $0,uref(warningu)
	jz 3f			//;no msg if @warning =/= 0
	pushl $4
	movl TOS(2),TS		//;flg<-pfa
	call Cnfa
	call Ciddot
	popl TS			//;countb<-msg
	call Cmessage
	SPUSH(2)
	call Cspace
3:	SPOP(2)
2:	movl uref(dpu),TS	//;pfa|0<-here
4:	movzbl (TS),ecx		//<Xhead);count
	movl uref(widthu),edx
	cmpl ecx,edx		//;dup dup width > if drop width endif
	jnc 1f
	movl edx,ecx
1:// ;tight alignment such, that lfa at 4-aligned address
	leal 3+2(TS,ecx),edx	//; len+count+disp
	andl $-4,edx		//; ->lfa
	pushl edx
	addl $lfa2pfa,edx	//;include lfa+cfa cells
	movl edx,uref(dpu)	//;allot
// ; won't overwrite name @ here + unknown disp, for 'head'
	negl ecx
	movl (esp),edx		//;lfa
	pushl ecx
    	leal -2(ecx,edx),edx	//;nfa
	negl ecx
	spush
	movl edx,TS		//;nfa+1
	spush
	leal 1(ecx),TS		//;count+1
	call Clcmove
	movl uref(currentu),ecx	//;current @
	pushl (ecx)
	movl edx,(ecx)		//;new nfa to voc-top, @current
	movl edx,uref(lastu)	//;'last'
	popl ecx
	popl edx
	xchg (esp),ecx		//;save previous nfa
	movb dl,-1(ecx)
	movl uref(dpu),edx	//;new here, address of pfa
	movl edx,-cfa2pfa(edx)	//;cpl entry into primitive word at <pfa>
	subl $lfa2pfa,edx	//;new lfa
	movl edx,uref(ltopu)
	movl (esp),ecx		//;prev. nfa
	movzbl (ecx),ecx
	andl $maxw,ecx		//;count
	addl (esp),ecx
	addl $2,ecx		//;prev. lf
	subl edx,ecx		//;disp=oldla-newla
	movl ecx,(edx)		//;link, prev lfa = link+@link
	popl ecx
// ( -- ) ; reset cmc counter
Cnulcmc:andl $0,uref(cmcu)	//<Xbegin,Xendif,clitr0,Xcplbrac,ccomma0);... all defining & program flow words)
	ret
// ( -- )(X: -- a ) ; 'does>'
Xdoesgreat:
	pushl $dodoesgreat
1:	call Cnulcmc		//<Xcdoesgreat)
	call Clatestpfa
	movl IP,(TS)
	popl -4(TS)
	popl IP
	spop
	NX
// ( -- )(X: -- n ) ; 'does>@' { ..does> @.. }
Xfdoesgreat:
	pushl $dfdoesgreat
	jmp 1b			//>Xdoesgreat)
// ( -- )(X: -- va ) ; 'does>v' { ..does> >body.. }
Xvdoesgreat:
	pushl $dvdoesgreat
	jmp 1b			//>Xdoesgreat)
.long ds// ( -- a )
dodoesgreat:
	spush
	pushl IP
	leal 4(W),TS
	movl (W),IP
	NX
.long df// ( -- n )
dfdoesgreat:
	spush
	pushl IP
	movl 4(W),TS
	movl (W),IP
	NX
.long dv// ( -- va )
dvdoesgreat:
	spush
	pushl IP
	movl 4(W),TS
	movl (W),IP
	addl uref(vbotu),TS
	NX
// ( -- )
Xpsemicodeq:// ; (;code) ;rgreat,latest,pfa,cfa,store,semis
	call Cnulcmc
	call Clatestpfa
	movl IP,-4(TS)	//; modify cfa
	spop
	popl IP
	NX
// ; F4 inline <psemicodeq>. subroutine, returns to next level caller!
Csemico:call Clatestpfa	//<Xconsp,Cconstant,Xcolon,Cdefer,Cvari,X2cons,C2vari,Xuser)
	popl -4(TS)
	spop
	ret
//
Xsemicode:// ";code" ;questcsp,compile,psemicodeq,brac,smudge,semis
	call Cqcsp
	leal psemicodeq,edx
	pushl $0
	call ccomma0
	popl uref(stateu)//;:contd: Xunsmudge
// ( -- );:note: un-smudged if bit #set# -!-
Xunsmudge:
	call cindiq	//; dictionary?
	jc 1f
	orb $smud,(edx)
1:	NX
// ( -- )
Xsmudge:pushl NXL
Csmudge:call cindiq	//<Xsemi,Cconstant,C2vari)
	jc 1f
	xorb $smud,(edx)
1:	ret
// ( -- )
Ximmediate:
	call cindiq
	jc 1f
	xorb $imed,(edx)
1:	NX
// ( -- nfa )
Xlatest:pushl NXL
Clatest:spush		//<Clatestpfa)
	movl uref(currentu),T
	movl (T),TS
	ret
// ( -- pfa ) ;kernel only
Clatestpfa:		//<Xdoesgreat,Xpsemicodeq,Csemico,Xvocabulary)
	call Clatest
	jmp Cpfa
// ( nfa -- pfa ) ; [nfa)name(-count] [lfa] [cfa] [pfa...]
Xpfa:	pushl NXL
Cpfa:	movzbl (TS),T	//<Xiddot)
	andl $wmsk,T
	lea 1+(1+lfa2pfa)(TS,T),TS//; nfa+1+count+1+lfa+cfa
	ret
// ( a1 -- a2 ) ;procede to next nfa in chain (by 'forget')
Xnnfa:	pushl NXL
Cnnfa:	movzbl (TS),T	//<Xforget,ctnnfa); nfa -- pfa
	andl $wmsk,T
	lea 2(TS,T),TS	//;nfa+1+count+1
	addl (TS),TS	//;from nfa to preceding lfa
	movsbl -1(TS),T
	lea -2(TS,T),TS	//;lfa=TS-T+2
	ret
// ( lfa1 -- lfa2 ) ;in 'forget'; also '+@'
Xplfa:	addl (TS),TS
	NX
// ; [nfa)name(-count] [lfa] [cfa] [pfa|cfa2] [pfa+...] 
// ( nfa|pfa -- pfa[+] ) ; convert either, pfa or nfa to pointer to the actual parameter field, 'pfa' or 'pfa+'
Xpfap:	pushl NXL
Cpfap:	call ctddsg	//<Xrecurse)
	jz 2f		//;?; arg was pfa
	movzbl (TS),W
	andl $wmsk,W
	movl W,T
	negl W
	cmpb 1(TS,T),Wl
	jnz 1f		//;?; not valid for nfa
	lea 1+(1+lfa2pfa)(TS,T),TS//; pfa := nfa+1+count+1+lfa+cfa
	call ctddsg
	jnz 1f		//;?; standard pfa
2:	addl $4,TS
1:	ret
// ( pfa+ -- a ); '>body' v-mem address by pfa
X2body:	pushl NXL	//<dvdoesgreat)
C2body:	call ctddsg
	jnz 1f
	addl $4,TS
1:	movl (TS),TS
	cmpl uref(vpu),TS
	jna 2f
	movl uref(vbotu),TS
	negl TS
2:	addl uref(vbotu),TS
	ret
// ( n1 -- n2 ) ; 'v<nfa', defblk link -> voc-name nfa
Xvocnfa:pushl NXL
Cvocnfa:movl 8(TS),TS//<Xvlist,Xv); voclink at voc-cfa -> word-lfa
	addl $lfa2pfa,TS
	jmp Cnfa
// {[ nfa | lfa | cfa | pfa | pfa+ daten ... ]}
// {[ name link  dodoes   wa [ pfa+ ... ]}	cpl`d by hi-level definig word
// {[ name link   ca  [ pfa ... ]}		standard primitive or colon defn
// 1) a ist pfa und @cfa ist nicht <dodoes> 	-> pfa & NZ
// 2) a ist pfa und @cfa ist <dodoes>		-> pfa & Z
// 3) a ist pfa+4 und @cfa ist nicht <dodoes>	-> fehler, zweifelhafter rueckgabewert & NZ
// 4) a ist pfa+ und @cfa ist <dodoes>		-> pfa & Z
// ( a -- a, eflags T=ecx ) ; ret Z if a is a pfa+ item; either, does>, does>@ or, does>v is the precedingly cpl`d wa.
ctddsg:	movl -cfa2pfa(TS),T
ctddst:	cmpl $dodoesgreat,T//<Xpfap,C2body,ctddsg4,Ccfa)
	jz 1f		//;?; a ist pfa
	cmpl $dfdoesgreat,T
	jz 1f
	cmpl $dvdoesgreat,T
	ret
// ( pfa|pfa+ -- pfa & NZ|Z ) ; ret cfa and NZ or , pfa' & Z if either, does>, does>@ or, does>v is the precedingly cpl'c wa.
ctdds:	call ctddsg
	jz 1f		// pfa ist pfa mit dodoes
	movl -cfa2pfa-cfa2pfa(TS),T
	call ctddst
	jnz 1f		// pfa war nicht pfa+ mit dodoes
	leal -cfa2pfa(TS),TS
1:	ret		//; pfa&Z wenncfa dodoes enthaelt
// ( pfa|pfa+ -- nfa ) ; [nfa)name(-count] [lfa] [cfa] [pfa...]
Xnfa:	pushl NXL
Cnfa:	call ctdds	//<Xbforgetb,Ccreate,Cvocnfa)
	movsbl -(1+lfa2pfa)(TS),T//<;Xsee,Xidplus,Xv,Ciddot,Xforget,Ccreate,Cvocnfa}
	lea -(2+lfa2pfa)(TS,T),TS
	ret
// ( pfa[+] -- cfa )
Xcfa:	pushl NXL
Ccfa:	call ctdds	//; Z if pfa+ given
	subl $cfa2pfa,TS//; pfa -> cfa
	ret
// ( pfa[+] -- lfa )
Xlfa:	pushl NXL
// ( pfa[+] -- lfa )
Clfa:	call ctdds	//; ? dodoes &c @pfa ?
	subl $lfa2pfa,TS//; pfa -> lfa
	ret
// ( -- ) ecx:=@dp-4, edx:=@ecx ; test & cpl branch to last colon defn before ';' ret Z(=edx) if branch cpl'd
Cosemi:	movl uref(dpu),ecx//<Xsemi,Xexit)
	subl $4,ecx
	movl (ecx),edx
	cmpl $1,uref(cmcu)
	js 2f		//;?; 0 or -ve, optimizing off
	call indiq
	jc 2f		//;?; address not in dictionary range
	cmpl $docolon,(edx)
	jnz 2f		//;?;
	cmpl $compile,-4(ecx)
	jz 3f		//;?; prevent modification of cfa to 'compile'
	movl $branch,(ecx)//; branch into last word if colon defn
	subl ecx,edx
	call ccommad	//; edx:=pfa
	movl edx,ecx
3:	subl ecx,edx
2:	ret
// ( -- )
Xsemi:	call Cqcsp
	call Cosemi
	jz 2f		//;?; branch cpl'd
1:	call Cexit
2:	call Csmudge	//;:contd: Xbrac
// ( -- ) ; '['
Xbrac:	movl $0,uref(stateu)
	NX
// ( -- ) ; ']'
Xcarb:	movl $imed|smud,uref(stateu)
	NX
// ( -- )
Xdefinitions:
	movl uref(contextu),ecx
	movl ecx,uref(currentu)
	NX
// ( -- )
Xdotquote:
	pushl NXL
	pushl $'"'	//;'"'
	spush
	leal pdotquoteq,edx//;compile,pdotquoteq,stglit,semis
	popl TS
	cmpl $0,uref(stateu)//;asciiquote,statefetch,zerobranch,3f-.
	jnz 4f		//>Cquote)
3:	call Cword	//;word,here,count,type,semis
	spush
	movl uref(dpu),TS
	jmp Cprint
// ccc( -- a u )
Xsquote:pushl uref(dpu)
	call Cquote
	popl ecx
	cmpl $0,uref(stateu)
	jz Xcount
	leal psquoteq,edx
	movl edx,(ecx)
	NX
// ccc( -- a ) 
Xquote:	pushl NXL
Cquote:	pushl $'"'	//<Xsquote); '"'
	spush
	popl TS
qten:	cmpl $0,uref(stateu)//<Xstring)
	jnz 5f
3:	call Cword	//;word,here,count,type,semis
	SPUSH(2)
	movl uref(dpu),TS
	movzbl (TS),ecx
	spush
	incl ecx
	addl $lumpi,TS
	spush
	movl TS,TOS(3)
	movb $0,(TS,ecx)
	movl ecx,TS
	jmp Ccmove
// ddd ccc( -- a )
Xstring:pushl NXL
	call Cblword
	movzbl 1(TS),TS
	jmp qten	//>Cquote)
5:	leal pquoteq,edx//;compile,pquoteq,stglit,semis
4:	call ccommad	//<Xdotquote)
Cstglit:call Cword	//<Cquote)
	movl uref(dpu),ecx//;here
	movzbl (ecx),edx//;count
	addl $2,edx
	addl edx,uref(dpu)
	movb $0,-1(ecx,edx)
	jmp Cnulcmc
// ccc( -- )
Xstglit:pushl NXL
	jmp Cstglit
// ( a -- )
Xscomma:call Ccount
	pushl uref(dpu)
	SPUSH(2)
	addl $2,TS	//; ( -- a u u u'
	call Callot	//; ( -- a u u
	popl edx
	movl TS,(edx)
	incl edx
	movb $0,(edx,TS)
	movl edx,TOS(1)
	jmp Xmove

// ---- defining words ----

// ( n -- )(X n1 -- n2 ); 'vcons*' alterable multiplying constant
Xvconsm:pushl NXL
	spush
	movl uref(vpu),TS
	call Cconstant
	call Cvcomma
	call Csemico
dovconsm:
	movl (W),T
	addl uref(vbotu),T
	mull (T)
	NX
// ( n -- )(X n1 -- n2 ); 'cons*' non-alterable multiplying constant
Xconsm:	pushl NXL
	call Cconstant
	call Csemico
	mull (W)
	NX
// ( n -- )(X n1 -- n2 ); 'vcons+' alterable incrementing constant
Xvconsp:pushl NXL
	spush
	movl uref(vpu),TS
	call Cconstant
	call Cvcomma
	call Csemico
dovconsp:
	movl (W),T
	addl uref(vbotu),T
	addl (T),TS
	NX
// ( n -- )(X n1 -- n2 ); 'cons+' non-alterable incrementing constant
Xconsp:	pushl NXL
	call Cconstant
	call Csemico
docplus:addl (W),TS	//<+origin); (sonstnix)
	NX
// ccc( -- )(X: -- ); place-holder for by 'does>' redirected cfa
Xlessbuilds:
	pushl NXL
Clessbuilds:		//<Xvocabulary)
	spush
	xorl TS,TS
Cconstant:		//<Xuser,Cvari,Xconstant,Xconsp)
	call Ccreate
	call Csmudge
	call Ccomma
	call Csemico	//; F4 inline <psemicodeq>
doconstant:
	spush
	movl (W),TS
	NX
// ( -- n );alterable kernel constants
domconstant:
	addl (W),W
	jmp doconstant
// ( n -- )(X -- n ); 'cons' non-alterable constant
Xconstant:
	pushl NXL
	jmp Cconstant
// ( -- n ); non-alterable kernel constants by sign ext'd byte value
dobconstant:
	spush
	movsbl (W),TS
	NX
// ( n -- )(X: -- n ); 'constant', alterable cons in vari-men
Xvcons:	pushl NXL
Cvcons:	call Ccreate
	call Csmudge
	pushl TS
	movl uref(vpu),TS
	call Ccomma	//;cpl disp to @(vb)
	movl uref(vbotu),ecx
	addl uref(vpu),ecx
	addl $4,uref(vpu)
	popl (ecx)	//;sto cons value
	call Csemico
dovcons:spush
	movl (W),T
	addl uref(vbotu),T
	movl (T),TS
	NX
// ccc( -- )
Xcolon:	pushl NXL
	call Cqexec	//;questexec,storecsp,current,fetch,context,store
	movl uref(currentu),ecx
	movl PS,uref(cspu)
	movl ecx,uref(contextu)
	call Ccreate	//;create,carb,psemicodeq
	movl $imed|smud,uref(stateu)
	call Csemico
docolon:pushl IP
	movl W,IP
	NX

//::---- subroutine calls to 4th words ----..
// ;return from subroutine calls to 4th words, <Cprim>, <Cmdfer>, <Cudfer>, <Cdcol>
cpret:	.long .+4
	.long .+4
	popl IP
	ret
.ifne _c4_
// ( xx -- xx ) ;W=pfa of colon-definition to execute
Cdcol:	pushl IP
	leal cpret,IP
	jmp docolon
// ( xx -- xx ) ;W=cfa of primitive word to execute
Cprim:	pushl IP
	leal cpret,IP
	jmp *W
.endif			// ----
// ( xx -- xx ) ;W=pfa of memory-deferred word to execute
Xmdfer:	pushl NXL	//<Xwlq)
Cmdfer:	pushl IP
	leal cpret,IP
	jmp domdfer
// ( xx -- xx ) ;W=pfa of user-deferred word to execute
Cudfer:	pushl IP	//;by '[inter]', 'type', 'emit', 'key', '?key')
	leal cpret,IP
	jmp doudfer
//::---- subroutine calls to 4th words ----''

// ccc( xpfa -- )
Xdefer:	pushl NXL
Cdefer:	spush		//;default item by pfa (for cons etc dft by 'is<')
	subl $cfa2pfa,TS//;cfa
	pushl TS
	movl uref(vpu),TS
	addl $4,uref(vpu)
	pushl TS
	call Cconstant
	call Ccomma
	popl ecx
	addl uref(vbotu),ecx
	popl (ecx)
	call Csemico
domdfer:leal 4(W),T	//;2nd-ary pfa
	movl (W),W
	addl uref(vbotu),W
3:	cmpl $0,(W)	//<doudfer)
	jz 2f		//>dovari);?; inactive
	pushl T		//; save pfa
	movl (W),T	//; primary ca
	cmpl uref(dpu),T
	popl T
	jc 1f		//;?; within dictionary - not executeable in external memory
	movl (T),T	//;dft
	subl $cfa2pfa,T	//;pfa->cfa
	movl T,(W)
1:	movl (W),T
	leal 4(T),W	//-!- musz sein -!-
	jmp *(T)
// ccc( a n -- )
Xudefer:pushl NXL
Cudefer:pushl TS	//;pfa
	pushl TOS(1)
	call Cconstant	//;cpl name & uvari disp
	call Ccomma	//;sto dft pfa
	popl edx	//;pfa
	popl ecx	//;udisp
	subl $cfa2pfa,edx
	movl edx,(UR,ecx)//;cpl xec wa
	call Csemico
doudfer:leal 4(W),T
	movl (W),W
	addl UR,W
	jmp 3b
// ccc( n -- )(X: -- a ); vari by disp into vari-mem
Xvari:	pushl NXL
Cvari:	pushl TS
	movl uref(vpu),TS
	call Cconstant
	movl uref(vbotu),ecx
	addl uref(vpu),ecx
	addl $4,uref(vpu)
	popl (ecx)
	call Csemico
dovari:	spush
	movl (W),TS
	addl uref(vbotu),TS
2:	NX		//<domdfer)
// ( d -- )(X -- d )
X2cons:	pushl NXL
	call C2vari
	call Csemico
do2cons:movl (W),W
	addl uref(vbotu),W
	spush
	movl (W),TS
	spush
	movl 4(W),TS
	NX
// ( d -- )(X: -- a )
X2vari:	pushl NXL
C2vari:	call Ccreate	//<X2cons)
	call Csmudge
	spush
	movl uref(vpu),TS
	call Ccomma
	call Cswap
	call Cvcomma	//;init to vmem
	call Cvcomma
	call Csemico	//; F4 inline <psemicodeq>
do2vari:spush
	movl (W),TS
	addl uref(vbotu),TS
	NX
// ( n -- )(X: -- a )
Xuser:	pushl NXL
	call Cconstant
	call Csemico
douser:	movl (W),T
1:	spush
	leal (UR,T),TS
	NX
// ( -- a ) ; kernel uvari by byte sized signed disp
dobuser:movsbl (W),T
	jmp 1b
// ( n -- )(X: -- n )
Xucons:	pushl NXL
	call Cconstant
	call Csemico
doucons:movl (W),T
	spush
	movl (UR,T),TS
	NX
// ;vocabulary, disp to vmem at pfa
// ;vocabulary defn block in vmem:
// ;vhs:	.byte -1	//;		alignment byte
// ;nfs:	.word vnam	nfa:	2nd-ary nfa, dummy name terminates wordlist
// ;	.byte -1
// ;lfs:	.long ntop\o	lfa:	'wid' to context/current; addr of top nfa within voc, init w. own voc-nfa
// ;vls:	.long \pv	cfa:	link to prev. voc wrt this posn, 'voc-link'
// ;.ifnc "\sd","0"
// ;sds:	.long lf\sd-D	pfa:	disp wrt mem-base to voc-lf of 2nd-ary search voc; 0 at root block
// ;fls:	.long \fl		voc flags
// ;vxs:	.long \o-lfa2cfa	this voc's 4th word-header lfa
// ;vocnm
// ;	prevvoc:.equ vl\o
// ;	prev\o: .equ lf\o
// ;	    b[-1] vocnfa:b[$81,$20,-1] d[voctop] d[voclink] d[2nd-ary] d[flags] d[back-lfa].
// ;	   pseudo header: ^- nfa        ^- lfa    ^- cfa     ^- pfa     ^- wa1   ^- wa2
// ;	context stores pseudo-lfa of -^ voc-header, ^- voc-link chain at pseudo-cfa.
// ;	back-link such that { ' vocname dup lfa swap @ >vmem pfa 8+ @ = } is true
// ;	with                    ^- word-pfa            ^-nfa   ^- voc-pfa->back-ptr
// ccc( -- )(X: -- a )
Xwordlist:
	pushl $dowl		//;'wordlist' ca
	jmp 1f
// ccc( -- )
Xvocabulary:
	pushl $dovoc		//;ca
1:	call Clessbuilds	//; <docons> @cfa ; create 0 , ; allot incl. pfa
	call Clatestpfa		//
	movl uref(vpu),ecx
	popl -4(TS)		//; <do{voc,wl}> to cfa
	incl ecx
	pushl TS		//;-> pfa for back-ref
	movl ecx,(TS)		//;disp wrt vmem to voc-block nfa
	spush
	movl $6*4,TS
	call Cqvalc		//; prevent re-allocation while voc. compiling
	movl $vnfd,TS		//;-1,vnam,-1
	call Cvcomma
	spush
	movl uref(currentu),TS	//;@current=voclfa
	pushl TS		//;-> lfa: ntop, initial 2nd-voc, ptr to prev voc dummy nfa
	movl uref(dpu),TS
	incl TS
	call Cvcomma		//;prev. voc is initial voc-top (@(00) after "forth")
	spush
	leal uref(voclu),edx	//;voclink
	movl edx,TS
	addl (TS),TS		//;@+ -> previous voc
	movl uref(vpu),ecx
	addl uref(vbotu),ecx	//;vhere
	subl ecx,TS		//;disp new to prev voclink posn
	subl edx,ecx
	movl ecx,(edx)		//;voc-link !
	call Cvcomma		//;voclink ,
	spush;spush;spush
	popl TS			//;@current lfa
	subl uref(vbotu),TS	//;by disp wrt vmem
	call Cvcomma		//;2nd-ary search voc is parent voc
	xorl TS,TS
	call Cvcomma		//;flags field
	popl TS			//;pfa
	subl $lfa2pfa,TS
	spush
	call Cvcomma		//;back-link by latest lfa
// ;dummy dictionary-header for new word-list:
	movl $vnfd,edx
	call ccommad		//;'nfa'
	movl $-lfa2pfa,edx	//;8, ptr to voc's dictionary-pfa
	call ccommad		//;'lfa'
	movl $-(lfa2cfa+vnfa2vlfa),edx
	call ccommad		//;'cfa'
	call ccommad		//;'pfa' platzhalter
	call ccommad		//;(flags) platzhalter
	call Ccomma		//;header-lfa <= required, re <Xv>. thus, no short dummy header
	NX
dovoc:	movl (W),W
	addl uref(vbotu),W	//;'>body' -> voc-nfa
	leal cfa2pfa-1(W),T	//;ptr to top nfa=voclfa
	movl T,uref(contextu)
	NX
dowl:	spush
	movl (W),TS
	addl uref(vbotu),TS	//;'>body' -> voc-nfa
	addl $cfa2pfa-1,TS	//;ptr to top nfa=voclfa
	NX
// ;assign 2ndary voc by vocname ddd to voc sss, independently of current search order
// ddd sss( -- ) ; { 2ND-VOC destn-voc search-voc }
Xvoc2nd:call 1f			//; voc to modify
	jz 3f			//; "0" not valid
	call 1f			//; voc to enter
	jmp 6f
// ;by 'is' assign 2ndary voc by pfa to voc sss, independently of current search order
// sss( pfa -- ) ; 'dest-voc is vocname'
Xisv2:	call Cswap		//<Xis)
	spush
	movl TOS(2),TS		//; ( a1 a2 a1 --
	call 5f
3:	jz Xerror		//; 'not found'
	movl TS,TOS(2)		//; ( v1 a2 v1  --
	spop		//; ( v1 a2 --
	call 5f			//; ( v1 v2  --
6:	jz 4f
	subl uref(vbotu),TS
4:	movl TOS(1),ecx
	movl TS,sds-lfs(ecx)
	SPOP(2)
	NX
1:	call Cafind		//; search all vocs
	SPOP(2)
	jz 3b
5:	movl TS,ecx		//;pfa
	xorl TS,TS		//; 'not found'
	cmpl $zero+cfa2pfa,ecx
	jz 2f			//;?; '0'
	addl $3,TS		//; 'incorrect addressing mode'
	cmpl $dovoc,-cfa2pfa(ecx)
	jnz Xerror		//;?;not a vocabulary
	movl (ecx),TS		//;disp to 
	addl uref(vbotu),TS	//; ..voc defn block nfa
	addl $vnfa2vlfa,TS	//;lfa, ptr to 'latest'
2:	ret
// ( pfa1 -- pfa2 ) ; ret 2nd-voc pfa by primary voc pfa
Xv2pfa:	movl (TS),TS
	addl uref(vbotu),TS	//; >body -> dummy nfa
	movzbl (TS),T
	andl $wmsk,T
	lea 1+(1+lfa2pfa)(TS,T),TS//; pfa, vmem ptr
	movl (TS),TS
	addl uref(vbotu),TS	//; >body -> voc-nfa
	addl $8,TS		//; l>pfa
	call Cvocnfa		//; v<nfa
	movzbl (TS),T		//<Xiddot)
	andl $wmsk,T
	lea 1+(1+lfa2pfa)(TS,T),TS//; nfa+1+count+1+lfa+cfa
	NX
.nolist
// -----------------------------------------------------------------------------
// cpldef.S <eof>
