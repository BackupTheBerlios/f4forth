//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; vtsys.S 
// =============================================================================
.psize 0
.att_syntax noprefix
.text
.list

//;
//; ----------------------------------------------------- //
//; host access
//;

//; ---- cpu ----

.ifne _pa_
// ( p -- n ) ; input from cpu port
Xpf:	movl TS,edx
	inl dx,eax
	NX
Xpwf:	movl TS,edx
	inw dx,ax
	movzwl ax,TS
	NX
Xpcf:	movl TS,edx
	inb dx,al
	movzbl al,TS
	NX
// ( n p -- ) ; output to cpu port
Xps:	movl TS,edx
	spop
	outl eax,dx
	spop
	NX
Xpws:	movl TS,edx
	spop
	outw ax,dx
	spop
	NX
Xpcs:	movl TS,edx
	spop
	outb al,dx
	spop
	NX
.endif
// ( -- d ) ; read the cpu cp counter
Xtic:	spush
	rdtsc
	spush
	movl edx,TS
	NX
// ( -- )
Xclrsegv:pushl NXL	//<iniquit)
Cclrsegv:		//<docold)
	pushl $-2
	popl segvv
	ret
//; ---- tty by vt ----

// ( -- x y ) ; ohne header, musz auf { ' !max-xy cfa @ 4- } stehenbleiben!
Xfmaxxy:pushl NXL	//<maxxy:Cudfer(Xvlist))
	spush
	jmp 3f
// ( -- x y )
Xsmaxxy:movl uref(vbotu),edx
	movl $0,winszv-D(edx)//;force ioctl reading
// ( -- x y )
Xmaxxy:	pushl NXL
Cmaxxy:	movl uref(vbotu),edx//; wdw sizes
	spush
	movl winszv-D(edx),ecx
	testw cx,cx
	jg 1f		//;?;already known
3:	call winsz	//<Xfmaxxy);ioctl for window outline by chars
1:	movl ecx,TS
	shrl $16,TS
	jnz 2f
	notl TS		//; x := -1 if not a console
2:	spush
	movzwl cx,TS
	ret
// ( x y -- ) ; convert parm to string <esc>[{y};{x}H and send to stdout
Xatxy:	movl TS,ecx
	sarl $31,TS
	notl TS
	andl ecx,TS	//; 0 max
	movl TOS(1),edx
	movl TS,TOS(1)
	movl edx,TS	//;x @tos
	pushl uref(baseu)
	sarl $31,edx
	pushl $10
	notl edx
	popl uref(baseu)
	andl edx,TS	//; 0 max
	spush
	pushl $';'
	pushl $'H'
	xorl TS,TS
	spush
	call Clesssharp
	popl TS		//;"H"
	call Chold
	call Csharps	//;convert {y}
	popl TS		//;";"
	call Chold
	pushl $ESC
	pushl $'['	//;"["
	call Csharps	//;convert {x}
	popl TS
	call Chold
	popl TS
	call Chold
	SPUSH(2)
	call Csharpgreat
	popl uref(baseu)
	jmp Xtype
// ;ret NZ if rel-stdin is a console channel
cioc:	movzbl rstdin+chpv,edx//<Xxy)
	movl edx,ecx
	shll $cdtsizeb,edx
	leal cdtv(edx),edx
	testl $consflags,cdtref(tyfl)(edx)
	ret
// ;ret NZ if rel-stdout is a console channel
cioco:	movzbl rstdout+chpv,edx//<Xvtq)
	movl edx,ecx
	shll $cdtsizeb,edx
	leal cdtv(edx),edx
	testl $consflags,cdtref(tyfl)(edx)
1:	ret
// ( -- u1 u2 ) ; cursor posn, col u1, row u2, = -1 if posn not available.
Xxy:	pushl $-1
	spush
	popl TS
	call cioco	//;rel-stdout ecx:=channel, edx:=cdt
	spush
	jz 4f		//;?; not a vt, ret -1,-1
	movl vtcpos,TS	//; <esc>[6n
	TOSPUSH(10)	//;local string buffer
	movl ecx,TOS(1)	//;input chan, @tos after emit
	movl $'R',ecx	//;terminating char
	call vtqp	//;query cursor posn
	jz 2f		//;?; not received
	cmpl $6,TOS(1)	//;min expected: <esc>[0;0R - {y,x}
	jc 2f		//;?; false response
	leal TOS(9),ecx
	cmpb $ESC,(ecx)
	jnz 2f		//;?; error, ret -1,-1
	cmpb $'[',1(ecx)//; "["
	jnz 2f		//;?; error, ret -1,-1
	pushl $';'	//;";"
	incl ecx
	popl edx	//;terminator char ";"
	call vtfp	//;convert 1st parm
	jnz 2f		//;?; error, ret -1,-1
	pushl $'R'
	movl TS,TOS(9)	//;'y'=row, overwrites initial '-1'
	popl edx
	call vtfp
	jnz 2f		//;?; error, ret -1,xx
	movl TS,TOS(10)	//;'x'=column, overwrites initial '-1'
2:	SPOP(9)		//;drop workspace
4:	NX
// ; emit @tos, vt cursor posn query, set up and receive response to local buffer, get terminating char in ecx
vtqp:	pushl ecx
	call Cemit	//; (by unvectored syscall)
	popl edx
	pushl $-1	//;flag <esc>
	pushl $11	//;max 10 chars: <esc>[999;999R
	lea TOS(9),ecx	//; buf
	pushl ebx
	pushl ecx	//;buf
	pushl edx	//;term. char
	pushl ecx	//;buf
// ; rs: 0:buf 4:"char 8:buf 12:nxl 16:maxcount 20:flag
	call Cchid	//;@tos := fd
	pushl $1
	movl TS,ebx
	popl edx
	incl TS
3:	decl 16(esp)
	js 4f		//;?;buffer ovf
6:	decl TS
	js 4f		//;?;no channel/input error
	movl (esp),ecx
	movb $0,(ecx)
	movl ebx,TS
	call Cwf	//;wait until input received, ret ebx := fd
	sys_ read
	movzbl (ecx),ecx
	cmpl $ESC,20(esp)
	jz 5f
	cmpl $ESC,ecx
	jnz 6b		//;?;wait for initial <esc> code
	movl ecx,20(esp)//; flag
5:	incl (esp)
	cmpb 4(esp),cl
	jnz 3b		//;?;not the terminating char
4:	popl ecx	//;charpos
	movb $0,(ecx)
	popl ecx	//;char
	popl TOS(2)	//;buf
	popl ebx	//;NXL
	popl TS		//;rmg count
	popl ecx	//;drop <esc>-flag
	subl $12,TS
	negl TS
	movl TS,TOS(1)	//;received count = flg
	ret
// ; convert decimal numeric stg from vt response until terminal or invalid char
// ; i: ecx+1=string, dl=terminal char
// ; o: TS:=number, ecx:=ptr to 1st char=dl, dh:=last char, dl=dl, Z if regularly terminated
vtfp:	pushl edx	//;term. char
	xorl TS,TS	//;accu
	jmp 3f
1:	cmpl $9,edx
	ja 2f		//;?; NC*NZ, else accumulate
	leal (TS,TS,4),TS
	leal (edx,TS,2),TS
3:	incl ecx
	movzbl (ecx),edx
	movb dl,1(esp)
	cmpb (esp),dl
	jz 2f		//;?; Z, orderly termination
	subl $'0',edx
	jnc 1b		//;?; S+NZ, invalid, other than terminator
2:	popl edx
	ret
// ( -- flg ) ; 'vt?' flg > 0 if stdin is a terminal, vt102 or 'enhanced' vt100 (xterm)
Xvtq:	pushl $0
	call cioco	//; NZ if stdout by console-chan
	jz 2f
	btrl $bterm,cdtref(tyfl)(edx)
	spush 
2:	popl TS		//;preserve flags from <cioco>
	jz 1f		//;?;stdin not by console-chan
	TOSPUSH(11)	//;local string buffer
	movl TS,TOS(11)
	movl vtqt,TS
	movl ecx,TOS(1)	//;input chan, @tos after emit
	pushl edx
	movl $'c',ecx	//;terminating char
	call vtqp	//;( n -- ) emit char n, query stdin
	popl edx
	jz 2f		//;?; not received
	leal TOS(9),ecx
	cmpb $ESC,(ecx)
	jnz 2f		//;?; error, ret 0
	movl vtqr1t,TS
	cmpl 1(ecx),TS	//; [?6c
	jz 3f		//;?; error, ret 0
	movl vtqr2t,TS	//; [?1;2c
	cmpl 1(ecx),TS
	jnz 2f
	movl 5(ecx),TS
	cmpb $'c',TSh
	jnz 2f
	subb $'0',TSl	//; ret vt100 variant+1
	movb TSl,TOS(10)
3:	incb TOS(10)
	orl $mterm,cdtref(tyfl)(edx)
2:	SPOP(10)
1:	NX
//; ---- system calls, shell cmd ----

// ( -- ) ; un-recoverably revert from suid-prog to real user
Xrealu:	pushal
	sys_ getuid
	sys_ setuid
	popal
	NX
// ( -- n ) ; ret own process id
Xpidf:	spush
	sys_ getpid
	NX
// ( n -- )
Xalarm:	pushl ebx
	movl eax,ebx
	sys_ alarm
	mov eax,ecx	//; rmg time from previous, unfinished <alarm>
	popl ebx
	spop
	NX
.ifne _sh_
// ( a -- er ) ; xec shell command fron stg a
Xsh:	spush
	xorl TS,TS
	pushal
	xorl ebx,ebx
	sys_ fork	//;establish 2ndary job
	testl TS,TS
	jnz 4f		//;?;continue in primary job
	movl uref(dpu),ecx
	movl $rg1t,(ecx)//;"sh"
	movl $rg2t,4(ecx)//;"-c"
	movl TOS(1),edx
	testl edx,edx
	jnz 3f
	leal rg3t-1,edx
3:	incl edx
	movl edx,8(ecx)
	movl TS,12(ecx)	//;terminate ags list
	movl $sht,ebx	//;execution shell, "/bin/sh"
	movl esp,ebp
	movl envpo,edx	//; envp => set 0 if {sh} returns w. error
	sys_ execve	//;xec shell command, no return
4:	movl TS,s_TS(esp)
	js 5f		//;?;error from <fork>
	movl eax,ebx
	xorl ecx,ecx
	xorl edx,edx
	sys_ waitpid	//;wait until 2ndary job exitted
5:	popal
	TOSPULL(1)
	NX
.endif
// ( a -- n ) ;"(s)"
Xpsysp:	pushl NXL
Cpsysp:	pushl edi	//;save reg to PS
	pushl esi	//;
	pushl NXL
	pushl $0	//;prepare error flag
	leal syscalref-2,edx//;names, ptr to before 1st stg <.byte 0,0 :<>: 0,0>
	movl TS,ebx
	mov -10(edx),ecx//;no. of syscalls
	testl TS,TS
	js 1f
	jz 3f
	leal 4(ebx),ebx	//;stg after leading 3 chars
5:	movl -4(ebx),TS	//;count & leading 3 chars, 1st test
1:	decl ecx
	js 3f		//;?;ret name not found
	movl edx,edi	//;list, ptr to stg
	movzbl (edx),edx//;stg countbyte
	leal 4(edi,edx),edi//;for 16bit no, countbyte, <nul>-terminator
	movl edi,edx	//;save next stg
	scasl
	jnz 4f		//;?;not found or, "display all" mode
	movzbl al,TS	//;stg count
	subl $3,TS
	movl ebx,esi
	js 5b		//;?;..(shouldn't happen, shortest name is 3 chars)..
	jz 2f		//;?;done
	xchg TS,ecx
	repz cmpsb
	mov TS,ecx	//;rst syscalls count
	jnz 5b		//;?;no match
2:	movzwl -2(edx),ecx//; syscall number
	movl ecx,(esp)
3:	popl TS
	popl NXL
	popl esi
	popl edi
	ret
// ; list syscall names if adr a -ve
4:	testl TS,TS
	jns 1b		//;?;not found
	cmpl TS,ebx
	jnz 1b		//;?;not found
	decl (esp)	//;ret -ve no. of syscalls
	pushal
	movb $rstdout,TSl
	call brchid
	movl s_edx(esp),edx
	leal 1(edx),ecx
	movzbl (edx),edx
	pushl $BL
	pushl $1
	sys_ write	//;send the syscall name
	popl edx
	movl esp,ecx
	sys_ write	//;append a blank
	popl ecx
	popal
	jmp 1b
//<;Xsys)
9:	call Cliteral
	movl $ssc,edx
	pushl NXL
	jmp ccommad
// ccc( a -- er ) ; 'sys'
Xsys:	call Cblword
	call Cpsysp
	cmpl $0,uref(stateu)
	jnz 9b		//;:contd: Xssc
// ( n a -- a er )
Xssc:// ;syscall no @tos=TS, ptr to top address (last item) of args list @nos
	pushal
	movl TOS(1),edi	//;last item in arguments list
	movl (edi),ebx
	movl -4(edi),ecx
	movl -8(edi),edx
	movl -12(edi),esi
	movl -20(edi),ebp
	movl -16(edi),edi
	sys_ eax	//;xec syscall @eax by int 128
	movl TS,s_eax(esp)
	popal
	NX

//; ---- ELF hash figure ----

// ;{ unsigned long   h = 0, g;
// ;  while (*name)
// ;  { h = (h << 4) + *name++;
// ;    if (g = h & 0xf0000000)
// ;     h ^= g >> 24;
// ;    h &= ~g;
// ;  }
// ;  return h;
// ;}
// ( a u -- n ) ; hash11
Xhash11:movl TS,ecx	//; char-count+
	spop
	pushl esi
	pushl edi
	movl TS,esi
	xorl TS,TS	//; TS = 'h' := 0
	jmp 1f
2:	shll $4,TS	//;h = (h << 4)...
	movzbl (esi),edx//;	*name
	addl edx,eax	//; +
	movl $0xf0000000,edx
	incl esi	//;	name++
	andl TS,edx	//;g = h & 0xf0000000
	jz 1f		//;?;
	movl edx,edi
	shrl $24,edx	//;g >> 24
	notl edi	//;~g
	xorl edx,TS	//;h ^= g
	andl edi,TS	//;h &= g'
1:	decl ecx
	jns 2b
	popl edi
	popl esi
	NX
.nolist
// -----------------------------------------------------------------------------
// vtsys.S <eof>
