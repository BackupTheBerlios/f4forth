//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; uvario.S 
// =============================================================================
.psize 0
.att_syntax noprefix
.text
.list

//; ---- uvari opr ----

// ( a -- disp )
X24th:	subl $orign+0x74,TS
	NX
// ( disp -- a )
X2v:	addl uref(vbotu),TS
	NX
// ( a -- disp )
Xv2:	subl uref(vbotu),TS
	NX
// ( -- n ) ; 'blk@'
Xblkfetch:
	spush
	movl uref(blku),TS
	NX
// ( -- ) ; '0in!'
Xztoin:	movl $0,uref(inu)
	NX
// ( -- n ) ; 'state@'
Xstatef:spush
	movl uref(stateu),TS
	NX
// ( -- a ) ; 'vhere' address of 1st free byte in variables mem
Xvhere:	spush
	movl uref(vpu),TS
	addl uref(vbotu),TS
	NX
// ( -- a )
Xhere:	spush
	movl uref(dpu),TS
	NX
// ( -- a )
Xpad:	spush
	movl uref(dpu),TS
	leal lumpi(TS),TS
	NX
// ( .x. -- )
Xspstore:
	movl uref(s0u),PS
	NX
// ( a -- ) ; set UR := a, sto previous UR to new 'u0', sto new UR to previous 'u0'
Xups:	addl upu-uvariu,TS
	movl uref(u0u),ecx
	movl TS,uref(u0u)
	movl TS,UR
	movl ecx,uref(u0u)
	spop
	NX
// ( -- a ) ; 'local's bottom, before allocation
Xl0:	spush
	movl uref(s0u),TS
	NX
// ( -- n ) ; fetch/reset to safe base value
Xbasef:	pushl NXL
Cbasef:	spush
	movl uref(baseu),TS
	cmpl $256,TS
	jnc 1f
	cmpl $2,TS
	jnc 2f
1:	movl $10,TS
	movl TS,uref(baseu)
2:	ret
.ifne 0
// ( -- )
Xhex:	movl $16,uref(baseu)
	NX
// ( -- )
Xdecimal:
	movl $10,uref(baseu)
	NX
// ( -- )
Xbases:	movl TS,uref(baseu)
	jmp Xdrop
.else
// ( -- )
Xhex:	pushl $16
	jmp 1f
// ( -- )
Xdecimal:
	pushl $10
	jmp 1f
// ( -- )
Xbases:	pushl TS
	spop
1:	popl uref(baseu)
	NX
.endif
.nolist
// -----------------------------------------------------------------------------
// uvario.S <eof>
