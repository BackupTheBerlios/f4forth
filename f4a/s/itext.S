//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; itext.S 
// =============================================================================
.psize 0
.att_syntax noprefix
.text
.list
//; ---- text ----

// ccc( -- ) ; '-wl' ret 0 if if @lword=0, else ret flg from comparison of @lword w. string ccc 
Xwlq:	cmpl $0,lldv
	jg Xdlword
	movl $1,lldv
// ccc( -- f ) ; '-l' flag whether word ccc is the word searched for by 'l-load', f =/= 0 if no 'lword' exists
Xdlword:call Cblword
// ( a -- f ) ; flag whether counted stg at a is the word searched for by 'l-load', f =/= 0 if no 'lword' exists
Xlwordq:cmpl $1,lldv	////cmpl $0,lldv
	jl Xor1		////jz Xor1		//;?;ret tf if gloabally loading
	jnz 2f
	decl lldv
2:	call Ccount
	leal 4(TS),ecx
	movl uref(dpu),TS
	addl $4,TS
	spush
	pushl ecx
	movl ecx,TS
	call Clcmove	//; search-string at 'here': " : (name) "
	spush
	popl ecx
	movl uref(dpu),TS
	movl $0x203a2000,(TS)
	movb $BL,(ecx,TS)
	movb cl,(TS)
	incl TS
	spush
	movl ecx,TS
	spush
	leal lldv,TS
	call Ccount
	call Cseq	//; s=
	jmp Xzeq
// ( n a -- ) ; sto significant bytes of char n to address a
Xbstore:movl TOS(1),ecx
	call cbytes	//>; edx := no. of ls non-zero bytes in ecx, min 1
	pushl esi
	pushl edi
	movl ecx,(PS)
	movl PS,esi
	movl edx,ecx
	movl TS,edi
	jecxz 1f
	rep movsb
1:	popl edi
	popl esi
2:	SPOP(2)		//<Xlwordq)::
	NX
// ( n -- n n1 ) ; n1 no of significant bytes of char n
Xbcount:spush
	movl TS,ecx
	call cbytes
	movl edx,TS
2:	NX		//<Xscount)
// ; (s")
Xpsquoteq:
	spush
	movzbl (IP),T
	movl IP,TS
	lea 2(IP,T),IP	//:contd: Xcount
// ( a -- a' u )
Xcount:	pushl NXL	//<Xpsquoteq):c:
Ccount:	spush		//<cidtext,Xscomma,Xlwordq,Xscount,Cprint)
	testl TS,TS
	jz 1f		//;zero address, ret 0 0 
	incl TOS(1)
	movzbl (TS),TS
1:	ret
// ( a -- a u ) ; try counted stg, if count=0 ret size of asciz stg
Xscount:pushl NXL
Cscount:call Ccount
	jz 1b		#>Ccount) zero address, ret 0 0 
	testl TS,TS
	jnz 1b		#>Ccount) counted string
	spop	# drop 0
Czcount:spush		# dup stg address, push eax into dstack
	testl eax,eax
	jz 5f		# ? zero address
	pushl edx
	pushl ecx
2:	cmpb $0,(eax)	# realign pointer on dword boundaries
	jz 1f
	incl eax
	testl $3,eax
	jnz 2b
3:	movl (eax),edx	# 4 bytes per test
	lea 0xfefefeff(edx),ecx
	notl edx
	andl ecx,edx
	addl $4,eax
	testl $0x80808080,edx
	jz 3b		# ? not a <nul> byte
4:    	incl eax
	shrl $8,edx	# check first lo byte
	jnc 4b		# ? bit 7 un-set, not a <nul> posn
	subl $5,eax	# realign pointer to null char
1:	popl ecx
	popl edx
	subl -4(edi),eax # eax:=length
5:	ret
// ( a -- a u ) ; ret size of asciz stg
Xzcount:pushl NXL
	jmp Czcount
// ( a u char -- a u1 )
Xctrailing:
	pushl NXL
	jmp Cctrailing
// ( a u -- a u1 ); '-trailing'
Xdasht:	pushl NXL
Cdashtrailing:		//<Cdotline)
	pushl $BL
	spush
	popl TS
Cctrailing:		//;all regs but TS preserved
	pushl ecx
	pushl TOS(2)	//;save ptr
	pushl TOS(1)
	call Crskip	//>
	testl TS,TS
	jg 1f		//;?;not just blanks
	movl TS,(esp)
	decl (esp)	//;ret zero count
1:	subl TS,(esp)
	testl TS,TS
	popl TS
	popl TOS(1)
	incl TS
	popl ecx
	ret
// ( a n -- a1 )
Xtraverse:
	movl TS,T
	spop
1:	addl T,TS
	cmpb $1,(TS)
	jnl 1b	//;?;either, byte = 0 or signed < 127, bit#7 set //jnc 1b
	NX
// ( a u n -- a1 u1 ) ; { over min rot over+ -rot - }
Xsstring:
	movl TS,T
	spop
	cmpl TS,T
	jle 1f
	movl TS,T
1:	addl T,TOS(1)
	subl T,TS
	NX
// ( a u -- a1 u1 ) ;1/string
Xs1string:
	pushl NXL
Cs1string:		//<Xestg)
	testl TS,TS
	jng 1f
	incl TOS(1)
	decl TS
1:	ret
// ( a1 u1 a2 -- a3 u3 ) ; e\stg
Xestg:	pushl $0
	popl (TS)
	spush
	call Cswap2	//;init a3	( a2 a3' a1 u1
	pushl uref(baseu)//; save base
	pushl uref(bitsu)//; i/o flags
	pushl $8
	popl uref(baseu)//; dft to octal base
	jmp 1f
2://; <[etype], no escape seq. found)
	SPOP(2)
3://; <false esc seq at stg end)		( a2 a3' a' u'
	popl uref(bitsu)
	popl uref(baseu)
	movl TOS(2),ecx	//; p2'
	addl TS,TOS(2)
	spush
	movl ecx,TOS(1)	//;		( a2 a3' a' a2 u'
	call Cmove	//;		( .. -- a2 a3'
	subl TOS(1),TS	//;		( a3 u3			
	NX
4://; <emit escaped code)
	notl ecx
	movl etypett+((etypete-etypet)*4)(,ecx,4),TS//;replacement char(s)
5://; ;<after \smb)
	cmpl $-1,TS
	jz 6f		//;?; noop dummy
	movl TOS(3),ecx	//;		( a2 a3" a' u' c
	decl ecx
9:	incl ecx
	incl TOS(3)	//; next char in output stg
	movb al,(ecx)
	shr $8,eax
	jnz 9b		//;?; till all chars inserted
6:	spop
	call Cs1string	//; advance over char
	jz 3b		//;?; stg ends
1://; <loop entry)	;		( a2 a3' a u -- ..
	movl TOS(1),ecx	//; a1
	spush;spush;spush
	movl ecx,TOS(2)	//; 2dup
	movl uref(esccu),TS//; get escape char, dft "\"
	call Cscan	//;		( a2 a3' a u a' u'  
	movl TOS(1),edx	//; a'
	movl TOS(3),ecx	//; a
	jz 2b		//;?; no escaped chars, copy till end
	movl TS,TOS(2)	//; u:=u' rmg len
	movl edx,TOS(3)	//; a:=a' ptr
	movl edx,TS
	subl ecx,TS	//; u" = a'-a, till escape char
	movl TOS(4),edx	//; a3
	movl ecx,TOS(1)	//; a" = a
	addl TS,TOS(4)	//; next a3'
	spush
	movl edx,TOS(1)	//; destn a3'
	call Cmove	//;		( a2 a3' a' u' a" a3'u" -- a2 a3" a' u'
	call Cs1string	//; 1/string, discard escape smb
	movl TOS(1),ecx
	jz 3b		//;?; stg ends
	spush
	movzbl (ecx),TS	//; next char
	cmpl uref(esccu),TS//; escape symbol
	jz 5b		//;?; inst esc
// ; test/get escaped special symbol;		( a2 a3' a' u' nl nh f=u"
	movl edi,edx
	leal etypet,edi
	movl $etypete-etypet,ecx
	repnz scasb
	movl edx,edi	//; rst ps
	jz 4b		//;?; inst symbol	( a2 a3' a1 u1 a1 u1
	movl TOS(2),TS
	spush
	movl TOS(2),TS
	call cnbnumberb	//; [number] w. count, copied to 'here' ( a2 a3' a' u' nl nh f=u"
// ; flag u" = u' if no numeric chars converted
	movl TOS(2),ecx	//; nl		( a2 a3' a' u' nl nh f=u"
	movl TOS(5),edx	//; p3
	cmpl TOS(3),TS	//; u"-u'
	jz 8f		//;?; non-numeric, discard num & flg
	incl TOS(5)	//; p3'
	movb cl,(edx)		
	movl TOS(3),ecx	//; u', before "\"
	movl TOS(4),edx	//; a'
	subl TS,ecx	//; ud:=u'-um 
	addl ecx,edx	//; pm:=a'+ud
// ; test whether bl after num which would have been discarded by [number]
	cmpb $BL,-1(edx)
	jnz 7f		//;?; not <bl> after \num
	decl edx		
	incl TS		//; include blank into rmg stg			
7:	movl edx,TOS(4)
8:	TOSPULL(3)	//;		( a2 a3' pm um
	jmp 1b		//; more
cnbnumberb:// ( a u -- dn f ) <Cestg); - ecx=count, edx=addr 1-chars
	pushl TS	//;stg count
	movl TOS(1),ecx	//;adr
	xorl TS,TS
	movl TS,TOS(1)
	spush
	pushl ecx	//;adr
	leal -1(ecx),TS	//;ptr to dummy count-byte
	call Cpnumberq	//;d a2
	popl edx	//;a1
	popl ecx	//;u1
	subl TS,ecx	
	addl edx,ecx
	subl edx,TS	//;num len=flg
	movl ecx,TS	//;rmg count
	ret
// ( a c -- a n1 n2 n3 ) ; enclose
1:	SPUSH(2)	//<Cenclose); a=0, ( .. -- 0 0 0 1 )
	movl edx,TS
	ret
Xenclose:
	pushl NXL
Cenclose:
	movl TS,ecx	//;char
	movl TOS(1),TS
	leal 1(TS),edx
	testl TS,TS
	jz 1b		//;?; a=0
	cmpb $BL,cl
	jz 6f		//;?; bl includes lo ctrls
1:	movb (TS),ch
	incl TS
	testb ch,ch
	jz 5f		//;leading <nul> returns ( -- a 0 1 1 )
	cmpb ch,cl
	jz 1b		//;skip leading
5:	spush
	subl edx,TOS(1)
2:	incl TS
	testb ch,ch
	jz 3f
	movb -1(TS),ch
	testb ch,ch
	jz 3f
	cmpb ch,cl
	jnz 2b
3:	spush
	subl edx,TOS(1)
	testb ch,ch
	jz 4f
	incl TS
4:	subl edx,TS
	ret
6:	movb (TS),ch	//;string
	incl TS
	testb ch,ch
	jz 7f		//;leading <nul> ret ( -- a 0 1 1 )
	cmpb ch,cl
	jnc 6b		//;skip leading bl or below
7:	spush
	subl edx,TOS(1)
9:	incl TS
	testb ch,ch
	jz 8f		//;?; <nul>
	movb -1(TS),ch
	cmpb ch,cl
	jc 9b		//;?; abuf bl
8:	spush
	subl edx,TOS(1)
	testb ch,ch
	jz 0f
	incl TS
0:	subl edx,TS
	ret
// ;skip leading chars = c of length u string at adr a,
// ;ret posn a' of 1st char =/= c and remaining cnt u'.
// ;ret a':= a+u, u':=0 if no other chars in the entire string. 
// ;specific handling of <bl> including low ctrl-s if c = <bl><bl>.
// ( a u c -- a' u' ) ; skip, o: edx preserved, eflags Z if char not found or u=0
Xskip:	pushl NXL	//:ok:
Cskip:	pushl edx
	movl TS,edx
	spop
	testl TS,TS
	jz 1f		//;empty stg
	movl TS,ecx
	pushl edi
	movl TOS(1),edi
	movl edx,TS
	cmpl $BLBL,edx
	jz 3f		//;special
	movzbl TSl,TS
	bt TS,cchart
	jnc 7f		//;?;char not covertible
	movb $lcdep,TSh
	andb uref(bitsu),TSh
7:	xorb TSl,TSh	//;other char if 'lc-ignore' state
6:	jecxz 2f	//;?;all=conv-char
	repz scasb
	jz 2f		//;?;all=char until stg end
	cmpb -1(edi),TSh
	jz 6b		//;?;@mem=conv-char
5:	inc ecx
4:	movl ecx,TS
	leal -1(edi),ecx
	popl edi
	movl ecx,TOS(1)
	testl TS,TS	//;->eflags
1:	popl edx
	ret
3:	scasb
	jc 4b		//;code > bl
	decl ecx
	jnz 3b
2:	inc edi
	jmp 4b		//;ret stg end
// ;scan string(a,u) for 1st occurrence of char = c,
// ;ret posn a' of 1st char = c and remaining cnt u', else addr after stg & u'=0
// ( a u c -- a' u' ) ; scan, o: edx preserved, eflags Z if char not found or u=0
Xscan:	pushl NXL
Cscan:	pushl edx
	movl TS,edx	//;char
	spop
	movl TS,ecx	//;count
	testl TS,TS
	jz 4f		//;?;empty stg
	pushl edi
	movl edx,TS
	movl TOS(1),edi
	cmpl $BLBL,TS
	jz 5f		//;?;'whitespace'
	movzbl TSl,TS
	bt TS,cchart
	jnc 6f		//;?;not a convertible char
	movb $lcdep,TSh
	andb uref(bitsu),TSh
6:	xorb TSl,TSh
8:	jecxz 1f
	movb (edi),dl
	decl ecx
	incl edi
	cmpb dl,TSl
	jz 3f
	cmpb dl,TSh
	jnz 8b
3:	movl ecx,TS	//<;found <bl,bl>
	leal -1(edi),ecx
	popl edi
	movl ecx,TOS(1)
	incl TS		//;->eflags
4:	popl edx
	ret
5:	scasb
	decl ecx
	jnc 3b		//;?;char>/@mem: ctrl found
	jnz 5b		//;?;more
1:	incl edi	//;ret addr after stg
	decl ecx	//; ..and zero count
	jmp 3b
// ;skip trailing chars = c of length u string at adr a,
// ;ret posn a' of 1st char =/= c and remaining cnt u'.
// ;ret a':= a, u':=0 if no other chars in the entire string. 
// ;specific handling of <bl> including low ctrl-s if c = <bl><bl>.
// ( a u char -- a1 u1 ) ;o: edx preserved, eflags Z if char not found or u=0
Xrskip:	pushl NXL	//:ok:
Crskip:	pushl edx	//<Cctrailing)
	movl TS,edx	//;char
	spop
	movl TS,ecx	//;cnt
	testl TS,TS	//;empty stg
	jz 1f
	pushl ebx
	pushl edi
	std		//;top down
	movl TOS(1),edi	//;adr
	movl edx,TS	//;char
	leal -1(edi,ecx),edi//;top adr
	mov ecx,edx	//;cpy count
	cmpw $BLBL,ax	//;not the special mode
	jz 3f		//;skip also lo ctrls
	cmpl $BLBL,edx
	jz 3f		//;special
	movzbl TSl,TS
	bt TS,cchart
	jnc 7f		//;?;char not covertible
	movb $lcdep,TSh
	andb uref(bitsu),TSh
7:	xorb TSl,TSh	//;other char if 'lc-ignore' state
8:	jecxz 4f	//;?;all=conv-char
	repz scasb
	jz 4f		//;?;all=char until stg end
	cmpb 1(edi),TSh
	jz 8b		//;?;@mem=conv-char
	jmp 5f
4:	leal 1(edx),ecx
6:	decl ecx
5:	subl ecx,edx	//;remaining cnt
	leal 1(edi),ecx	//;compensate dec from scasb
	movl edx,TS
	popl edi
	popl ebx
	cld
	movl ecx,TOS(1)	//;1st =/= char
	testl TS,TS	//;->eflags
1:	popl edx
	ret
3:	scasb		//;skip ctrls and <bl>
	jc 6b		//;higher code
	decl ecx
	jnz 3b		//;more
	jmp 4b		//;no other chrs
// ;reversed scan string(a,u) for 1st occurrence of char = c,
// ;ret posn a' of 1st char = c and remaining cnt u'.
// ;ret a':= a+u (ptr to after stg), u':=0 if char c not found. 
// ;specific handling of <bl> including low ctrl-s if c = <bl><bl>.
// ( a u char -- a1 u1 ) ;o: edx preserved, eflags Z if char not found or u=0
Xrscan:	pushl NXL
Crscan:	pushl edx
	movl TS,edx	//;char
	spop
	movl TS,ecx	//;count
	testl TS,TS
	jz 1f		//; zero len
	pushl ebx
	pushl edi
	movl TOS(1),edi	//;ptr
	leal -1(edi,TS),edi //;stg end
	movl edx,TS	//;char
	movl ecx,edx	//;cpy count
	xorl ebx,ebx
	cmpl $BLBL,TS
	jz 2f		//;include lo ctrls
	movzbl TSl,TS
	bt TS,cchart
	jnc 7f		//;?;not a convertible char
	movb $lcdep,TSh
	andb uref(bitsu),TSh
7:	xorb TSl,TSh
8:	jecxz 6f
	movb (edi),bl
	decl ecx
	decl edi
	cmpb bl,TSl
	jz 4f
	cmpb bl,TSh
	jnz 8b
5:	cld
4:	subl ecx,edx	//;remaining cnt
	leal 1(edi),ecx
	movl edx,TS
	popl edi
	popl ebx
	movl ecx,TOS(1)	//;1st =/= char
	testl TS,TS	//;->eflags
1:	popl edx
	ret
2:	std		//;scan top down
3:	scasb		//;skip ctrl-s
	decl ecx
	jnc 5b		//;code not less than <bl>
	jnz 3b		//;more
6:	movl edx,ecx	//;zero count
	addl edx,edi	//;end of stg-1
	jmp 5b
// ( a1 u1 a2 u2 js -- flg ) ; 'sj=', modified 's=', w. single "js" chars replacing 'joker'
Xsjeq:	pushl NXL
Csjeq:	pushl TS	//; js
	spop
	jmp 2f
// ( a1 u1 a2 u2 -- flg ) ; flg <0: stg [a1] < [a2], =0: [a1]=[a2], >0 [a1]>[a2], eflags reflect result
Xseq:	pushl NXL
Cseq:	pushl $-1	//<Cmatch,Clwordq); comparison stg flag/char
2:	pushl $-1	//;mem 'joker'; RS+12: mem, RS+16: stg
	movl TOS(2),ecx
	subl TOS(2),TS
	jnz 1f		//;?; unequal length
	jecxz 0f	//;?; empty stgs, ret Z
	pushl esi
	pushl edi
	pushl ebx
	movl uref(bitsu),edx
	movl TOS(3),esi	//; a1
	movl TOS(1),edi	//; a2
	andl $lcdep,edx	//; flg in l.s. byte! upper/lower case toggle mask
	incl esi
3:	decl esi
4:	movb $0,TSl	//; clr fetched char
	repz cmpsb
	jz 8f		//;?; stg identity
	movzbl -1(edi),ebx//; char a2
	movzbl -1(esi),TS//; char a1
	cmpl 16(esp),ebx//; stg 'joker'
	jz 4b
	cmpl 12(esp),TS	//; mem 'joker'
	jz 3b
	movb bl,dh	//; copy char2
	testb dl,dl	//; flag
	jz 7f		//;?; case dependency set, ret (c1-c2)
	xorb TSl,dh	//; true diff
	xorb dl,dh
	jnz 7f		//;?; other char
	bt ebx,cchart
	jc 4b		//;?; convertible char
7:	subl ebx,TS	//; diff
8:	popl ebx
	popl edi
	popl esi
	jz 0f
1:	shrl $31,TS
	leal -1(,TS,2),TS
	negl TS
0:	leal 8(esp),esp
	TOSPULL(3)
	ret
// ( a1 u1 a2 u2 js -- flg disp ); joker js in stg(a2,u2
Xmatchj:pushl NXL
	pushl TS
	spop
	jmp 7f
// ( a1 u1 a2 u2 -- flg disp ) ; find string{a2,u2} in memory range{a1,u1}
// ; ret tf and count till end of found stg, else ff and mem-count
Xmatch:	pushl NXL
.ifne 01
Cmatch:	pushl $-1	//<Cldfind)
7:	pushl $-1
	xorl ecx,ecx	//;init ret-flg
	cmpl TOS(2),TS
	ja 1f		//;?; mem < stg
	incl ecx
	testl TS,TS
	jng 1f		//;?; empty stg always 'found'
	pushl TOS(1)	//;a2 stg
	push TS		//;u2
	pushl TOS(2)	//;u1 mem
	spop		//;( a1 u1 a2		[ u1 u2 a2 jm js
9:	movl 8(esp),ecx	//;stg a2
	movzbl (ecx),TS	//;1st char
	call Cscan	//;( a1 u1 char2 -- a1' rmd )
	movl TS,ecx	//;flg=rmd
	jz 2f		//;?; 1st char not found
8:	movl 4(esp),edx	//;u2 stg
	xorl ecx,ecx
	testl edx,edx	//; ( a1' rmd		[ u1 u2 a2
	jz 3f		//;single char
	movl TOS(1),ecx	//;a1'
	spush
	cmpl 4(esp),TS	//;u1 mem
	jc 6f		//;?; not found if stg > mem
	spush
	movl ecx,TOS(1)	//;2dup
	movl 4(esp),TS	//;u2
	spush
	movl 8(esp),TS	//;a2
	spush
	movl 4(esp),TS	//; a1' r a1' r a2 u2	[ u1 u2 a2 jm js
    spush
    movl 16(esp),TS	//; stg joker
    call Csjeq
	jz 4f		//;?; found
	incl TOS(2)
	decl TOS(1)
	jg 9b		//;?; more
6:	xorl ecx,ecx
	movl ecx,TOS(2)
4:	spop	//; a1 r 
3:	movl TOS(1),ecx	//; a1' where found stg begins
	subl (esp),TS	//; u1 mem count
	subl 4(esp),TS	//; u2 stg count
	negl TS		//; count from beginning till after found stg
	movl TS,(esp)
2:	movl (esp),TS	//; 0,u1 or =/=0,disp
	leal 20(esp),esp
5:	movl ecx,TOS(1)	//; stg not found, ret 0,u1
	ret
1:	SPOP(2)
	leal 8(esp),esp
	jmp 5b	
.else
Cmatch:	xorl ecx,ecx	//<Cldfind); init ret-flg
	cmpl TOS(2),TS
	ja 1f		//;?; mem < stg
	incl ecx
	testl TS,TS
	jng 1f		//;?; empty stg always 'found'
	pushl TOS(1)	//;a2 stg
	push TS		//;u2
	pushl TOS(2)	//;u1 mem
	spop	//;( a1 u1 a2		[ u1 u2 a2
9:	movl 8(esp),ecx	//;stg a2
	movzbl (ecx),TS	//;1st char
	call Cscan	//;( a1 u1 char2 -- a1' rmd )
	movl TS,ecx	//;flg=rmd
	jz 2f		//;?; 1st char not found
	movl 4(esp),edx	//;u2 stg
	xorl ecx,ecx
	testl edx,edx	//; ( a1' rmd		[ u1 u2 a2
	jz 3f		//;single char
	movl TOS(1),ecx	//;a1'
	spush
	cmpl 4(esp),TS	//;u1 mem
	jc 6f		//;?; not found if stg > mem
	spush
	movl ecx,TOS(1)	//;2dup
	movl 4(esp),TS	//;u2
	spush
	movl 8(esp),TS	//;a2
	spush
	movl 4(esp),TS	//;a1' r a1' r a2 u2	[ u1 u2 a2
	call Cseq
	jz 4f		//;?; found
	incl TOS(2)
	decl TOS(1)
	jg 9b		//;?; more
6:	xorl ecx,ecx
	movl ecx,TOS(2)
4:	spop	//; a1 r 
3:	movl TOS(1),ecx	//; a1' where found stg begins
	subl (esp),TS	//; u1 mem count
	subl 4(esp),TS	//; u2 stg count
	negl TS		//; count from beginning till after found stg
	movl TS,(esp)
2:	movl (esp),TS	//; 0,u1 or =/=0,disp
	leal 12(esp),esp
5:	movl ecx,TOS(1)	//; stg not found, ret 0,u1
	ret
1:	SPOP(2)
	jmp 5b	
.endif
// ( a u -- a u ) ; modify string(a,u) to upper letter case
Xsu:	testl TS,TS
	jng 1f
	movl TOS(1),edx
	pushl TS
	movl TS,ecx
2:	movzbl (edx),TS
	bt TS,lchart
	jnc 3f
	xorb $32,(edx)
3:	incl edx
	decl ecx
	jg 2b
	popl TS
1:	NX
// ( chr -- chr' ) ; convert to upper letter case
Xcu:	movzbl TSl,TS
	bt TS,lchart
	jmp 2f		//>Xcx)
// ( chr -- chr' ) ; convert to lower letter case
Xcl:	movzbl TSl,TS
	bt TS,lchart
	jc 1f		//:c:;?; lower case char
// ( chr -- chr' ) ; convert to other letter case
Xcx:	movzbl TSl,TS	//<Xcl):c:
	bt TS,cchart
2:	jnc 1f		//<Xcu);?; no conversion
	xorl $32,TS
1:	NX
// ( -- )
Xlci:	orl $lcdep,uref(bitsu)
	NX
// ( -- )
Xlcd:	andl $~lcdep,uref(bitsu)
	NX
// ; (")
Xpquoteq:
	spush
	movzbl (IP),T
	movl IP,TS
	lea 2(IP,T),IP	//;count+stg+<nul>
	NX
// ( adr cnt -- flg )
Xprintingq:
	pushl NXL
Cprintingq:
	movl TOS(1),ecx
	pushl ecx
	addl TS,(esp)
	spush
	movl ecx,TS
1:	pushl TS
	movzbl (TS),TS
	call Ckey2chare
	jz 2f
	popl TS
	incl TS
	cmpl (esp),TS
	jc 1b
	pushl TS
	testl TS,TS
	movl TOS(1),TS
2:	TOSPULL(1)	//;flag @tos
	popl ecx	//;drop char
	popl ecx	//;ptr
	ret
.ifne _k2_
// ( char -- char flg ) ;flag whether char @tos is letter case convertible
Xkey2alpha:
	spush
	movzbl al,eax
	bt TS,cchart
	sbbl TS,TS
	negl TS
	NX
.endif
// ( char -- char flg ) ;flag whether char @tos is in printing range
Xkey2char:
	pushl NXL
Ckey2char:	//<Cptype)
	spush
Ckey2chare:	//<xpem(Xexpect),Cprintingq,Cdump)
	pushl edx
	pushl ecx
	movzbl TSl,ecx
	xorl edx,edx	//;prepare ff
	subl TS,ecx
	jnz 2f		//;?;multi-byte char
	orl uref(asciu),ecx
	jz 2f
	incl edx	//;1 if in range
4:	cmpb cl,TSl
	jb 2f		//;?;below lower bound, ret 0
	cmpb ch,TSl
	jbe 1f		//;?;valid in lower range, ret 1/-1
	negb dl		//;-1 for upper range
	shrl $16,ecx	//
	jnz 4b		//;?;check 2nd range
	movb cl,dl	//;-> 0
1:	movb dl,dh	//;-> flg
2:	movsbl dh,TS	//;flg
	popl ecx
	popl edx
	testl TS,TS	//;pass eflags to caller, and NC+NO
	ret
// ( p1 u1 -- p1 u1 ) ; '>lower'
Xlo:	jmp Xnimp
// ;  (.")
Xpdotquoteq:
	incl IP
	spush
	movl IP,TS	//;chars
	spush
	movzbl -1(IP),TS
	addl TS,IP	//;stg+<nul>
	incl IP		//;:contd: -> Xptype
// ( a u -- ) ;sends a string of d2.w bytes from T.l)+ to channel-id 
Xptype:	pushl NXL
Cptype:	decl TS
	movl TOS(1),ecx
	pushl TS
	js 4f
	cmpl $65535,TS	//; limit to 64K
	jnc 4f
	test ecx,ecx
	jz 4f		//; noop by <zero> address
2:	movzbl (ecx),TS
1:	call Ckey2char
	shrl $1,TS
	sbbl TS,TS
	andl TOS(1),TS
	jnz 3f
	addl $'_',TS
3:	pushl ecx
	call cdemit
	popl ecx
	incl ecx
	decl (esp)
	jns 2b
4:	popl TS
	SPOP(2)
	ret
// ( a -- )
Xprint:	pushl NXL
Cprint:	call Ccount
	jmp Cptype
//; ---- interpreter ----

// ccc( -- ) ; printing comment
Xdotpx:	pushl $')'
	spush;spush
	popl TS
	call Cword
	movl uref(dpu),TS
	jmp Xprint
// ( -- a ) ; ptr to the <nul> word, because 'is' is not applicable
Xbxb:	spush
	leal cfa2pfa+x,TS
	addl (TS),TS
	NX
// ; : x blk @ if 1 blk +! 0 in ! blk @ b/scr 1- and 0= if ?exec r> drop endif else r> drop endif ;
// ; : X blk@ -dup if 0< if tib@ 256 dr1-ch #10 fread dr1ch ch-rnum 
// ( -- )
Xx:	movl uref(blku),ecx
	testl ecx,ecx	//;blk @ if
	jz 5f		//;?;command line i/o or piped/redirected stdin
	js readml	//;?;{chain}ed
	incl ecx
	movl ecx,uref(blku)//; 1 blk +! 0 in ! blk @
	pushl ecx
	call Cbbyscr
	leal -1(TS),edx	//;b/scr 1- and 0=
	popl ecx	//;if ?exec r> drop endif
	spop
8:	andl ecx,edx	//<readml)
	jnz 6f		//;?;not blk# 0 or 1st blk/screen
	call Cqexec
5:	popl IP		//;else r> drop endif
6:	movl $0,uref(inu)
	NX		//; ;s
readml:// ; {chain}ed input
.ifne _cy_////
    movl $0x22222222,edx
    int $3
.endif
	spush
	movl uref(tibu),TS
	spush
	movl $lumpi,TS
	spush
	movzbl rdr1ch+chpv,TS//;chan
	movl TS,edx
	shll $cdtsizeb,edx
	addl $cdtv,edx
	movl cdtref(fpos)(edx),ecx
	notl ecx
	movl ecx,uref(blku)//;updt @blk to current line beginning
	pushl $NL
	spush
	popl TS		//; <nl>
	call Cfread
	cmpl $0,cdtref(rnum)(edx)
	jz 9f
	pushl $0
	addl TOS(1),TS
	popl (TS)
	SPOP(2)
	movl edx,ecx
	jmp 8b		//;?;continue w. script
9:	btl $incfib,uref(bitsu)
	jnc Xbye	//;?;xit at script eof
	jmp Xend	//; terminate {chain}ed input
// ( -- a ) ; { bl word here }
Xname:	pushl NXL
Cblword:spush
	movl $BLBL,TS
	call Cword
	spush
	movl uref(dpu),TS
	ret
// ( n -- ) ; conditionally discard input stream until eol, if n = 0
Xiftrue:testl TS,TS
	spop
	jz Xbslb
	NX
// ccc( -- ) ; discard input stream until eol, if word ccc found
Xifnfound:
	call Cfoundq
	spop
	jnz Xbslb
iffnx:	NX		//<Xiffound)
// ( -- ) ; comment until eol plus, 2nd <argcopyargs>
Xbhbb:	pushl uref(bitsu)
.ifne _cy_////
    movl $0x33333333,edx
    int $3
.endif
	andb $~(scrix|xargs),uref(bitsu)//;pretend non-scripting, no execxuteable args
	call nxargs
	decl uref(argcu)//;less '--' dummy arg
	testb $xargs,uref(bitsu)
	popl uref(bitsu)
	jz Xbslb	//;?;no executeable args
	orb $xargs,uref(bitsu)
	jmp Xquery
// ccc( -- ) ; discard input stream until eol, if word ccc found
Xiffound:
	call Cfoundq
	spop
	jnz iffnx	//>Xifnfound) :contd: Xbslb
// ( -- ) ; comment until eol, dft for deferred "\"
Xbslb:	pushl $0	//<Xbhbb)
	cmpl $0,uref(blku)
	jng 1f		//;?; sequential include-file (-ve) or, terminal (0)
	popl edx
	movl consv(cbyl),ecx
	pushl eax
	movl uref(inu),eax
	divl ecx
	subl edx,ecx
	popl eax
	addl ecx,uref(inu)
	NX
// ( -- a n )
Xsource:pushl NXL
Csource:spush
	movl uref(blku),TS
	testl TS,TS
	jng 2f
	call Cblock
	spush
	movl consv(cbyl),TS
	ret
2:	movl uref(tibu),TS
	jmp Czcount
// ccc( -- ) ; comment until ')'
Xpx:	pushl $')'
1:	spush		//<Xbslb)
	popl TS		//;:contd: Xword
// ccc( c -- )
Xword:	pushl NXL
Cword:
.ifne 1
	call Csource
	spop
.else
	spush
	movl uref(blku),ecx//;blkfetch,zerobranch,8f-.
	movl uref(tibu),TS//;8:tib,fetch
	testl ecx,ecx	//;( c a 
	jng 9f		//;?;mmapped	//:://jz 9f//:://
	movl ecx,TS	//;blkfetch
	call Cblock	//;block,branch,9f
.endif
9:	addl uref(inu),TS//;in,fetch,plus
	movl TOS(1),ecx	//;( c a 
	movl TS,TOS(1)	//;swapx,enclose
	movl ecx,TS	//;( a c
	call Cenclose	//;( a c -- a n1 n2 n3 )
// ;enclosed len = n2-n1; enclosed text at a+n1, next at a+n3.
	addl TS,uref(inu)//;n3: in,plusstore
	movl TOS(2),ecx	//;n1
	movl TOS(1),TS	//;n2
	addl ecx,TOS(3)	//;a1+n1:1st char
	subl ecx,TS	//;n2-n1:len
	movl uref(dpu),ecx//;here
	TOSPULL(1)	//;( ... -- a1 n1 n2-n1 )
	movb TSl,(ecx)	//;len here c!
	movb $BL,1(ecx,TS)//;text delimiter @here
	incl ecx	//;here,p1,rgreat,cmove
	movl ecx,TOS(1)	//;( a1 here len -- ..
	jmp Ccmove
// ( -- ) ; leading part of "interpret"
7:	spop
	call Cliteral
Xinter:	call Cqstack
	call Cdashfind
	leal pinterq+4,W
	call Cudfer	//; xec '[inter]', user-deferred vector
	testl TS,TS
	jz 3f		//;?;try numeric
	movl TOS(1),ecx
	SPOP(2)
	subl $cfa2pfa,TS
	cmpl uref(stateu),ecx
	jnc iffnx	//;?; imed/interpret, ret NX
	movl uref(cmou),ecx
	subl ecx,uref(cmcu)
	call Ccomma	//; compile
	jmp Xinter	//>recurse
3:	movl uref(dpu),TS
	call Cnumber
	cmpl $-1,uref(dplu)
	jz 7b		//;?;sing
	call Cdliteral
	jmp Xinter	//>recurse; loop will be left w. <nul> word
// ( nfa -- nfa ) ; xec forget handlers
// ; i/o: TS=nfa of removed word, ecx=new here, edx=new @last
// ; latest link at (fl)@ => [ prevlink vdisp ][ word-nfa ][ hnd-pfa ]
cxfgh:	cmp ecx,TS
	jc 5f		//;?;nfa below new dp
	pushl edx	//<Xforget) ; new @last = next nfa
	pushl ecx	//; new @dp
	movl uref(pflpu),ecx//;ptr to linked blocks: @(fl) -> [link-pc][owner-nfa][handler-pfa]
	movl uref(vbotu),edx
	jmp 3f		//; check wether hnd exists, loop until all done
2:// ; @tos[ @dp ][ @last ]
	addl ecx,edx
	movl 8(edx),ecx	//;handler pfa
	cmpl 4(edx),TS	//;'owner' word
	ja 4f		//;?;in remaining dictionary ( => fin?)
	testl ecx,ecx
	jz 8f		//;?;inactive, try next
	cmpl uref(dpu),ecx
	ja 6f		//;?;hnd beyond 'here'
	spush
	pushl edx	//;save link
	movl 4(edx),TS	//;pass owner word nfa = @tos at entry
	spush
	mov ecx,edx
	movl ecx,TS	//;pass hnd pfa
	call Cmdfer
	popl edx
	SPOP(2)
6:	movl $0,8(edx)	//;clear hnd reference
8:	subl uref(vbotu),edx
	cmpl uref(vpu),edx
	jnc 9f		//;?; hnd beyond currrent vmem bound
	movl edx,uref(vpu)
9:	movl edx,uref(pflpu)//;unlink executed hnd field
	addl uref(vbotu),edx
	movl (edx),ecx
	addl ecx,uref(pflpu)//;preceding lk
	testl ecx,ecx
	jnz 4f
	movl ecx,uref(pflpu)//;rmv initial link
4:	movl (edx),ecx
3:	testl ecx,ecx
	jnz 2b		//;?;more
1:	popl ecx
	popl edx
5:	ret
// ( n -- n1 n2 ) ; 't<nfa'
// ; n1=ecx:=lowest nfa >/ n, n2=edx:=highest nfa < n, n1=n2 if n at top of dictionary
Xtopnfa:call ctopnfa
	movl ecx,TS
	spush
	movl edx,TS
	NX
ctnnfa:	stosl		//<;ctopnfa)
	movl ecx,TS
	call Cnnfa
	movl TS,ecx
	spop
	ret
// ; <ctopnff)
2:	addl (edx),edx
	jnz 3f		//;?;next in voc-link
9:	popl edx	//;edx := 1st nfa below @tos
	popl ecx	//;ecx := 1st nfa above or = @tos
	testl edx,edx
	jz 8f		//;?;
	cmpl $-1,ecx
	jnz 8f		//;?;not at top of all words
	movl edx,ecx
8:	ret
//<;Xtopnfa)
// ( n -- n ) ; 't<nfa', ecx := min(nfa) >/ n, edx := max(nfa) < n
ctopnfa:
//<;Caforget,Xforget)
ctopnff:pushl $-1	//;-> mn
	pushl $0	//;-> mx
	leal uref(voclu),edx
	addl (edx),edx	//; +@
3:	leal lfs-vls(edx),ecx//;top nfa
	cmpl $0,(ecx)
	jz 2b		//;?;voc empty
	movl (ecx),ecx	//;top header
1://;don't store if @ecx = 0 or ecx is a voc-nfa
	cmpl $0,(ecx)
	jz 2b		//;?;
	cmpw $vnam,(ecx)
	jz 2b		//;?;voc-nfa doesn't count
	cmpl ecx,TS
	jc 4f		//;?;nfa > n	-> mn
	jz 7f		//;?;nfa=n	=: mn
// ;(esp) := greatest nfa which is lower than TS=n:
// ;store to (esp) if ecx=nfa = TS or nfa < (esp) and nfa > TS
	cmpl (esp),TS
	jna 6f		//;?;mx >/ n
	cmpl (esp),ecx	//; if mx<n
	jna 2b		//;?;mx >/ nfa (@tor >/ ecx)
	movl ecx,(esp)	//;mx:=nfa
	jmp 1b
4://;4(esp) := lowest nfa which is equal to or greater than TS=n:
//  ;sto to 4(esp) if ecx=nfa = TS or nfa > TS and nfa < 4(esp)
	cmpl 4(esp),TS
	jae 6f		//;?;mn \< n
	cmpl 4(esp),ecx	//; if mn>n:
	jae 6f		//;?;mn \< nfa (4@tor \< ecx)
7:	movl ecx,4(esp)	//<;nfa=n
6:	call ctnnfa	//;ecx := next nfa, edx=edx, eax=eax=TS
	jmp 1b
// ( a -- ) ; forget by string
Xsforget:
	pushl NXL
	call Csfind
	jnz 3f		//>;Xforget)
4:	spop
	ret
// ( a -- ) ; forget by address, silently exit if 'fence'd
Xaforget:
	pushl NXL
	jmp Caforget
// ( -- ) ; virgin dictionary, till 'fence'd but not "task" or below
Xempty:	pushl NXL
Cempty:	spush		//<;segcold)
	movl uref(fenceu),TS
	cmpl $task-10,TS//; ' task nfa
	jc 4b		//>;?Xsforget); invalid figure
	addl $4,TS	//>:contd: Caforget); forget till next newer word
// ( a -- ) ; forget by address, silently exit if 'fence'd
Caforget:		//<Cempty):c:
	call ctopnff
	mov ecx,TS	//; min. nfa >/ a
	spush
	jmp 8f		//>Xforget)
// ccc( -- ) ; forget by name
Xforget:pushl NXL
	call Cdashfind
	jz 4b		//>Xsforget); drop & ret
3:	movl TOS(2),TS	//<Xsforget); pfa
	TOSPULL(1)	//;drop count-byte
	call Cnfa	//; ( pfa pfa -- .. )
	call ctopnff	//; edx: 1st nfa below @tos, ecx: 1st nfa above or = @tos
	cmpl uref(fenceu),ecx
	jna Xfencems	//;?;protected word
	movl ecx,TS	//;nfa which to 'forget'
	movl ecx,TOS(1)	//; ( .. -- nfa nfa )
8:	pushl ebx
	cmpl uref(fenceu),ecx//<;Caforget)
	jna 9f		//;?;protected
	cmpl ecx,edx
	jz 9f		//;?;above top of dictionary, rst 'forth'
	cmpl $task,ecx
	jna 9f		//;?;kernel memory
	cmpl ecx,uref(dpu)
	jc 9f		//;?;cannot 'forget' in external memory
	call cxfgh	//;xec forget handlers
	movl ecx,uref(dpu)//;clr dictionary
	movl edx,uref(lastu)//;next nfa to 'last'
	call Cpfa	//;ändert ecx
	subl $lfa2pfa,TS
	movl TS,uref(ltopu)//;top lfa to uvari
	leal uref(voclu),edx//<; voc-link +@...
2:	addl (edx),edx	//;(next) voc-link
	jz 5f		//;?;voc-list ends
6:	movl (edx),ecx	//;disp to next voc-link
	addl edx,ecx
	jnz xfg4	//;?; test/sto new voc-top
5:	movl TOS(1),TS	//;last word's nfa
	leal uref(voclu),edx
	movl (edx),ecx
	movl edx,ebx
	addl edx,ecx	//; voc-link +@...
7:	addl (edx),edx
	cmpl TS,lfs-vls(edx)
	ja 7b		//;?; C+Z ;lfa>voc
	movl edx,(ebx)	//;top voc
	subl ebx,(ebx)	//;voc-link wrt own address, for '+@'
	cmpl edx,ecx
	jz 8f		//;?; no voc on top of or, equal to word to forget removed
9:	movl forth+cfa2pfa,edx//;+4; ' forth
	addl uref(vbotu),edx//;voc-head nfa
	addl $lfs-nfs,edx//;+3; ptr to top nfa = voc-lfa
	movl edx,uref(contextu)//; force "forth definitions"
	movl edx,uref(currentu)
8:	popl ebx
	SPOP(2)
	ret
xfg4:	movl lfs-vls(ecx),TS//;-4; top nfa
1:	movl TS,lfs-vls(ecx)//;-4; to voc-top
	cmpl TOS(1),TS
	jc 2b		//; n > nfa
	cmpl $0,(TS)
	jz 2b
	cmpw $vnam,(TS)
	jz 2b
	pushl ecx
	call Cnnfa	//; n<nfa
	popl ecx
	jmp 1b
.ifne _af_
// ( a -- )
Xasfind:pushl NXL
	jmp Casfind
// ccc( -- )
Xafind:	pushl NXL
.endif
Cafind:	call Cblword
// ( a -- )
Casfind:call cvflags// ; clr vocs 'searched'-flags
	movl TS,ecx
	leal uref(voclu),edx
3:	addl (edx),edx
	jz 2f		//;?;linkage ends
	pushl edx
	subl $vls-lfs,edx//;-4; voc-top posn
	call csfd1	//>Csfind)
	popl edx
	jnz 2f		//;?;ret
	cmpl $0,(edx)
	jnz 3b		//;?;-^
2:	ret
cvflags://<;Csfind,Casfind) clr vocs 'searched'-flags
	leal uref(voclu),edx
	addl (edx),edx
6:	andl $~(mfind),fls-vls(edx)//;+8; vocblock
	addl (edx),edx
	jnz 6b
4:	ret
// ( a -- 0 | pfa cbyte 1 ) ; 'sfind' through @context, @current and, 'root'
Xsfind:	pushl NXL
	jmp Csfind
Xdashfind:
	pushl NXL
Cdashfind:
	call Cblword
Csfind:	call cvflags	//;clr vocs 'searched'-flags
	movl uref(vbotu),edx//;vmem
	movl TS,ecx	//; string addr
	addl $lflocal-D,edx//;search 'local' 1st
	call csfd1
	jnz 4b		//;?;'local' word
	movl uref(contextu),edx
	call csfd1
	jnz 4b		//;?;in @context chain
2:	movl uref(currentu),edx
	call csfd1
	jnz 4b		//;?;found in @current chain
	movl uref(vbotu),edx//;vmem
	addl $lfroot-D,edx//;lastly, try 'root' voc
csfd1:	pushl ebx	//<Casfind)
	movl uref(vbotu),ebx//;vmem
1:	pushl ecx
	cmpl ecx,ecx	//;force Z
	btsl $bfind,fls-lfs(edx)
	jc 3f		//;?; C*Z voc already done
	movl ecx,TS	//;string
	spush
	movl edx,TS	//;voclink
	pushl edx
	movl (TS),TS	//;top-nfa
	call Cpfindq	//;search single voc
	popl edx
3:	popl ecx
	jnz 5f		//;?;name found, ret NZ
	spush		//; find next un-searched voc, xit if Z, i.e. none left
	xorl TS,TS
	pushl PS
	movl TS,(PS)	//;flag, equal to passed voc if loop/recursion encounterd
7:	movl sds-lfs(edx),edx//; 2nd-voc header ptr
	TOSPUSH(1)
	movl edx,(PS)
	movl (esp),TS
9:	cmpl (TS),edx
	jz 8f		//;?;Z at end of chain or repeated entry
	addl $4,TS
	cmpl PS,TS
	jc 9b		//;?;
	addl ebx,edx	//; wrt vmem & NZ flag state
	btl $bfind,fls-lfs(edx)
	jnc 8f		//;?;NC if not yet searched
	jnz 7b		//;?;more vocs in chain
8:	popl PS
	spop
	ja 1b		//;?;next 2nd-ary if NC*NZ
5:	popl ebx
	ret
// - saved -- fd_num --.
#define fd_esi 20(esp)
#define fd_edi 16(esp)
#define fd_PS  fd_edi
#define fd_ebx 12(esp)
// - working ptrs
#define fd_nfa 8(esp)
#define fd_nam 4(esp)
#define fd_cnt (esp)
// - rstack depth
#define fd_pop 12	// drop for pop regs
#define fd_num 24	// data
// ccc( -- 0 | pfa cbyte 1 ) ; '-find'
// ( name topnfa -- 0 | pfa cbyte 1 ) ; '(find)'
Xpfindq:pushl NXL
// ( name topnfa -- 0 | pfa char 1 ) ; '(find)'
Cpfindq:		//<Csfind)
11:	pushl esi	//; IP <lods>
	pushl edi	//; PS <scas>
	subl $4,(esp)	//; drop for false result
	pushl ebx
	movl TOS(1),edx	//; passed name
	movzbl (edx),ecx//; count
	addl $2,edx	//; addr of 2nd char
	movl TS,esi
	pushl TS	//; -> 1st nfa
	pushl edx	//; -> name
	pushl ecx	//; -> count
	movl uref(bitsu),edx//; -> letter case dependency flag
	andl $lcdep,edx	//; flg in l.s. byte !
	jmp 2f
// ; fetch next nfa
// ;   name[count|chars]  [nfa: count+flags chars][-ve count] [lfa disp] [cfa: xex entry] [pfa ...]
4:	movl fd_nfa,ebx
	movzbl (ebx),TS
	andl $wmsk,TS	//; max name-len
	leal 2(ebx,TS),ecx//; lfa
	movl (ecx),TS	//; -ve disp to prev. word
	test TS,TS
	jz 3f		//;?; end of chain
	leal -1(ecx,TS),ebx//; next lfa, word len back-count
	movsbl (ebx),TS//; -ve disp to name beginning
	leal -1(ebx,TS),esi
	movl esi,fd_nfa	//; next nfa, count+flags-byte posn
2:// ;esi = nfa = (tor)
	movl fd_nam,edi	//; string
	xorl TS,TS	//; TS
	movzwl (esi),ebx//; bl:=count+flags
	cmpl $vnam,ebx	//; dummy name
	jz 3f		//;?; wordlist ends
	andb $fmsk,bl	//; width and smudge bit
	xorb $smud,bl	//; whether un-smudged
	jz 3f		//;?; false header defn!
	cmpb fd_cnt,bl
	jnz 4b		//;?; other len or smudged
	movb bh,dh	//; 1st char
	xorb -1(edi),bh
	jz 9f		//;?; found
	testb dl,dl
	jz 4b		//;?; case dependency set
	xorb dl,bh	//; toggle letter case bit
	jnz 4b		//;?; other char
	movzbl dh,ecx
	bt ecx,cchart
	jnc 4b		//;?; not in range of convertible chars
9:	movb (esi),ah	//; save count+flags result
	movzbl bl,ecx	//; count
	movl uref(widthu),ebx//; max count
	cmpl ebx,ecx
	jle 6f		//; "width" not less than count
	movl ebx,ecx	//; truncate
6:	addl $2,esi
	jmp 7f
8:// ;cmp after 1st char
	cmpsb
	jz 7f		//; chars equal
	testb dl,dl
	jz 4b
	movb -1(esi),al
	movzbl al,ebx
	xorb -1(edi),al	//; cmp w. char, including non-printing chars > 32
	xorb dl,al
	jnz 4b		//;?; other char
	bt ebx,cchart
	jnc 4b		//;?; not in range of convertible chars
7:	decl ecx
	jg 8b
	movb ah,al	//; pass count+flags byte
10://; word found
	movl fd_PS,PS
	movzbl ah,TS
	addl $8,PS	//; adjust for three args, NZ
	lea 1+lfa2pfa(esi),ecx
12:	movl PS,fd_PS
	movl TS,TOS(1)	//; 1st char
	pushl $1	//; flag
	movl ecx,TOS(2)	//; pfa
	popl TS
3:	leal fd_pop(esp),esp//; keep eflags; NZ if found, else Z
	popl ebx
	popl edi
	popl esi
	ret
.nolist
// -----------------------------------------------------------------------------
// itext.S <eof>
