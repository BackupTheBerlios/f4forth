//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; locmem.S 
// =============================================================================
.psize 0
.att_syntax noprefix
.text
.list
//; ---- local mem ----

.ifne _lx_
// ( n -- ) ; l!0
Xls0:	movl uref(s0u),ecx
	movl TS,-4(ecx)	//; @cfa+5
	spop
	NX
// ( -- n ) ; l@0
Xlf0:	movl uref(s0u),ecx
	spush
	movl -4(ecx),TS	//; @cfa+6
	NX
Xlfetch:notl TS
	movl uref(s0u),ecx
	movl (ecx,TS,4),TS
	NX
// ( n1 n2 -- ) ; 'l!'
Xlsto:	notl TS
	movl uref(s0u),ecx
	movl TOS(1),edx
	movl edx,(ecx,TS,4)
	jmp Xdrop2
.endif
.ifne _ld_
// ( dn -- ) ; l2!0
Xl2s0:	movl uref(s0u),ecx
	movl TOS(1),edx
	movl edx,-8(ecx)//; @cfa+
	movl TS,-4(ecx)	//; @cfa+
	SPOP(2)
	NX
// ( -- dn ) ; l2@0
Xl2f0:	movl uref(s0u),ecx
	spush
	movl -8(ecx),TS	//; @cfa+6
	spush
	movl -4(ecx),TS	//; @cfa+10
	NX
// ( n -- dn ) ; 'l2@'
Xl2fetch:notl TS
	movl uref(s0u),ecx
	leal (ecx,TS,8),ecx
	movl (ecx),TS
	spush
	movl 4(ecx),TS
	NX
// ( dn n -- ) ; 'l2!'
Xl2sto:	notl TS
	movl uref(s0u),ecx
	movl TOS(2),edx
	movl edx,(ecx,TS,8)
	movl TOS(1),edx
	movl edx,4(ecx,TS,8)
	jmp Xdrop3
.endif
// ( n -- ) ; alloc +n, de-alloc -n cells; adjust @csp, @l0, @s0
X2loc:	pushl NXL	//<Xmovex)
C2loc:	movl TS,edx	//<Xmovex,Xchain,Xend);cells
	shll $2,edx	//; ->bytes
	addl edx,uref(cspu)//;adjust SP control ptr
	testl TS,TS
	pushl esi
	js 2f		//;?;release
// alloc:
	pushl TS
	pushl edx
	call Cpsprst	//;check, remap dstack if required
	popl edx	//;#bytes
	popl TS		//;#cells
	movl uref(s0u),esi
	movl edi,ecx	//;PS
	pushl esi	//;save for ret 'l0'
	testl TS,TS	
	jz 1f		//;?;no alloc
	subl esi,ecx	//;depth bytes
	addl edx,uref(s0u)//;new s0
	movl edi,esi	//;old tos
	shrl $2,ecx	//;depth in cells
	addl edx,edi	//;new tos
	movl edi,edx	//;saved
	std		//;copy top down
	rep movsl	//;dstack content
	leal 1(TS),ecx	//;#locals+@@s0
	xorl TS,TS
	testl $7,edi
	jz 3f		//;?;8-aligned
	stosl
	decl ecx
	jng 4f
3:	rep stosl
4:	cld
	movl edx,PS	//;new tos
1:	popl uref(l0u)	//;'l0' = @s0 prior to alloc
	popl esi
	spop
	ret
// de-alloc:
2:	movl uref(psbotu),ecx
	addl $ps_rsv,ecx
	addl edx,PS	//;sub dealloc bytes
	pushl ecx	//;save bottom bound
	subl PS,ecx	//;-ve depth bytes
	jz 6f		//;?;nothing
	jl 5f		//;?;de-alloc
	addl ecx,edx
5:	subl edx,PS	//;rst tos addr
	negl edx
	jz 6f		//;?;nothing
	js 8f		//;?;'stack empty'
	movl PS,ecx	//;edi
	movl uref(s0u),edi
	subl edx,uref(s0u)//;drop locals
	subl edx,uref(l0u)
	subl edi,ecx	//;depth
	movl edi,esi
	sarl $2,ecx	//;cells count
	jng 8f
	subl edx,edi	//;depth
	rep movsl
8:	movl uref(s0u),edx
	movl uref(l0u),ecx
	movl (esp),TS	//;@(pb)+reserve
	cmpl TS,edx
	jnc 7f
	movl TS,uref(s0u)
	movl TS,PS	//;recover from erroneuos PS
7:	cmpl TS,ecx
	jnc 6f
	movl TS,uref(l0u)
6:	popl TS		//;drop @(pb)
	popl esi
	spop
	ret

.nolist
// -----------------------------------------------------------------------------
// locmem.S <eof>
