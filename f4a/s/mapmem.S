//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; mapmem.S 
// =============================================================================
.psize 0
.att_syntax noprefix
.text
.list
//; ---- data stack, mmap/mremap

//-?-//; test open mode, ret suitable mmap mode & flags
// ; i: edx=cdt; o: edx:mode, ecx:flags
// ;	r/w -> shared, prot-r/w; w/o: shared, prot-w; r/o: private,prot-r
mmofl:	pushl TS	//;chan
	pushl $MAP_PRIVATE//;->edx mode
	pushl $PROT_READ// ;->ecx prot
	movl cdtref(mdfl)(edx),edx
	popl TS
	andl $O_ACCMODE,edx
	cmpl $O_RDONLY,edx
	jz 1f		//;?; r/o
	addl $MAP_SHARED-MAP_PRIVATE,(esp)//;mode
	cmpl $O_WRONLY,edx
	jz 2f		//;?; w/o
	addl TS,TS	//; w/r
2:	addl $PROT_WRITE-PROT_READ,TS//;prot
1:	movl TS,ecx
	popl edx
	popl TS
	ret
// ( ch n1 n2 -- );mmap w. access n1{r/w/x}, mode n2{fixed,shared,private,noreserve,growsdown,anonymous}
Xmmapm:	pushl NXL
Cmmapm:	pushl ebx	//<;-/-)
	movl TOS(1),edx
	movl TS,ecx
	SPOP(2)
	jmp 2f
// ( ch -- n a ) ; r/w/- map file into memory, ret size n, address a
// ;i: -/- , o: ecx=xx, edx=cdt, cdt: @flen, @tyfl
Xmmapf:	pushl NXL
Cmmapf:	pushl ebx	//<;argcopy,Cldfind) save reg for the syscall arg
	call ccdtp
	call mmofl	//;ecx:=(un)protection bits, edx:=private/shared
2:	pushl edx	//; 1->mmap mode
	pushl ecx	//; 2->prot
	TOSPUSH(11)	//;local work-space
	pushl TS	//; 3->channel
// ; mmap data-block: [10:addr 9:size 8:prot-r/w/x 7:flg 6:fd 5:disp ]
	call Cflen	//;edx:=cdt
	pushl $0	//; 4->leave memory address selection to syscall
	movl TS,TOS(11)	//;ret flen to dstack
	testl TS,TS
	jg 3f		//;?;not an empty file
	movl $page_size,TS//;4K dft mapping
3:	addl $page_align,TS
	andl $page_mask,TS
	movl TS,TOS(9)	//;flen = mmap range
	popl TOS(10)	//; 4<-memaddr = 0
	popl TS		//; 3<-chan
	call Cchid
	popl TOS(8)	//; 2<-prot
	popl TOS(7)	//; 1<-mode
	pushl $0	//; ->disp wrt file beginning to cdt
	pushl $0	//; ->disp wrt file beginning
	leal TOS(10),ebx//;ptr to mmap structure
	movl TS,TOS(6)	//;file descriptor
	popl TOS(5)	//; <-fptr disp
// ; [ ebx:=10)*start, 9)length, 8)prot, 7)flags, 6)fd, 5)offset ]
	sys_ mmap	//;entire file, r/w, shared, from start
	cmpl $maxerr,TS
	jnc 1f		//;?;ret NC on error
	movl TOS(9),ecx	//;mapped size
	movl TOS(7),ebx	//;mode
	movl TS,cdtref(cmap)(edx)
	movl ecx,cdtref(cmsz)(edx)
	shll $16,ebx
	orl $chfmmap,(edx)//;mark <chid> mmapped
	orl $mmapc,cdtref(tyfl)(edx)
	orl TOS(8),ebx	//;prot
	movl ebx,cdtref(fctl)(edx)//;mode|prot to cdt
1:	popl cdtref(cmds)(edx)//; disp wrt s.o.f. := 0
	popl ebx
	TOSPULL(10)
	ret
// ( ch -- ms ma ); channel's mapped mem parameters
Xmap:	pushl NXL
Cmap:	pushl ebx
	call Cchid
	xorl ecx,ecx
	addl TS,TS
	jno 1f
	testl $mmapc,cdtref(tyfl)(edx)
	jz 1f
	movl cdtref(cmap)(edx),ecx//; addr
	movl cdtref(cmsz)(edx),TS//;size
1:	spush
	popl ebx
	movl ecx,TS
	ret
// ( ch -- er ) ;sync mapped w. file content
Xmsync:	pushl NXL
Cmsync:	pushl ebx
	call Cchid	//;fetch cdt, fd
	pushl $-EBADF
	movl TS,ebx
	popl TS
	addl ebx,ebx
	jno 1f		//;?;not mmapped
	testl $mmapc,cdtref(tyfl)(edx)
	jz 1f		//;?;not mmapped
	pushl edx
	pushl $MS_SYNC|MS_INVALIDATE
	movl cdtref(cmap)(edx),ebx//;address
	movl cdtref(cmsz)(edx),ecx//;size
	popl edx
	sys_ msync	//;flush mmaped data back to file
	popl edx
1:	cmpl $maxerr,TS
	popl ebx
	ret
2:	NX		//<;Xunmapf)
// ( ch -- er ); unmap by i/o channel no.
Xunmapf:pushl TS
	call Cmsync
	popl ecx
	jnc 2b		//>;Xmap)
	pushl TS
	movl ecx,TS
	call Cmap
	popl ecx
	shll $2,(edx)
	andl $~mmapc,cdtref(tyfl)(edx)
	sarl $2,(edx)	//;mark <chid> unmapped
	movl ecx,cdtref(cmsz)(edx)//;size
	movl ecx,cdtref(cmap)(edx)//; addr
// ( u a -- er ) ; unmap any mode mmaped mem of size u at addr a
Xmunmapa:
	pushl NXL
Cmunmapa:		//<Cldfind)
	pushl ebx
	movl TOS(1),ecx
	movl TS,ebx
	TOSPULL(1)
	sys_ munmap	//;free mem(u,a)
	popl ebx
	ret
// ( a2 a1 u1 u2 -- adr|er+NC )
// ;mremap: ebx=a1=addr,ecx=u1=old-len,edx=u2=new-len,esi=flags,edi=a2=new-addr
Xmremap:pushl NXL
Cmremap:pushal		//<Cvremap); save all regs
	pushl $MREMAP_MAYMOVE//;flg:=1
	movl TOS(2),ebx	//;a1
	movl TOS(1),ecx	//;u1
	movl TS,edx	//;u2
	movl TOS(3),edi	//;a2 (don`t care value for linux ver < 2.4)
	popl esi	//;flags
	sys_ mremap	//;adjust...
	movl TS,s_TS(esp)//;ret addr|ernum
	popal
	TOSPULL(3)	//;leave new address/ernum
	cmpl $maxerr,TS
	ret
// ( a u -- a|er ); ptr a to base & ptr+4 size uvari, +/- n additional alloc
// ; o:TS = ernum or new base address, ecx=size, ptr-s updated if ret w.o. error
Xvremap:pushl NXL
Cvremap:movl TOS(1),ecx	//<;Cvalc) uvari
	spush;spush
	movl 4(ecx),edx	//;current size
	movl edx,TOS(1)	//;u1 old size
	leal page_align(edx,TS),TS
	andl $page_mask,TS//;new size
	pushl ecx	//;save ptr to uvari
	movl (ecx),edx	//;base address
	movl TS,ecx	//;pass new size to caller
	movl edx,TOS(2)	//;addr
	movl edx,TOS(3)	//; -- a2 a1 u1 u2
	call Cmremap	//; -- addr|er
	popl edx	//;uvari
	jnc 1f		//;?;error
	movl TS,(edx)	//;new address
	movl ecx,4(edx)	//;new size
1:	ret
// ( n -- ) ; extend variables memory if less than <ds_min+n> bytes left, edx:=base+disp after alloc
Xqvalc:	pushl NXL
Cqvalc:	pushl uref(vpu)	//<Xvocabulary)
	call Cvalc
	popl uref(vpu)
	ret
// ( n -- ) ; allot in variables memory, extend if less than <ds_min> bytes left
Xvalc:	pushl NXL
Cvalc:	addl uref(vpu),TS//<Cvcomma,Ccvcomma)
	movl TS,uref(vpu)//;new vp
	movl TS,edx	//;save for dealloc
	addl $va_min,TS	//;margin before re-mapping
	movl uref(vsizu),ecx
	cmpl TS,ecx
	jnc 3f		//;?;within limit
	leal va_xtd(edx),ecx
4:	leal uref(vbotu),TS
	subl uref(vsizu),ecx//;pass difference
	spush
	pushl uref(vbotu)
	movl ecx,TS
	call Cvremap
	popl ecx
	jnc Cerror
	movl uref(vbotu),edx//; adjust for relocated vmem base
	addl edx,uref(voclu)
	addl edx,uref(contextu)
	addl edx,uref(currentu)
	subl ecx,uref(voclu)
	subl ecx,uref(contextu)
	subl ecx,uref(currentu)
	leal vlroot-D(edx),ecx
	negl ecx
	movl ecx,vlroot-D(edx)
1:	movl uref(vpu),edx//;disp after alloc
	spop
	addl uref(vbotu),edx//;pass disp+@(vb) to caller
	ret
3:	addl $va_xtd,TS
	cmpl TS,ecx
	jc 1b		//;?;below de-alloc margin
	movl edx,ecx	//;de-allocate
	jmp 4b
// ( n -- flg ) ; '+sp'
Xpsprst:pushl NXL
Cpsprst:movl PS,ecx	//<C2loc,Cndup);current depth
	sall $2,TS	//;bytes requested
	js 1f		//;?; cannot de-allocate
	subl uref(s0u),ecx
	addl TS,PS
	pushl ecx
	call Csprst
	movl uref(s0u),PS
	popl ecx
	jnz 1f		//; mmap error or stack below bounds
	addl ecx,PS	//; rst depth
1:	ret
// ( -- n )
Xdepth:	pushl NXL
	call Csprst	//; alloc additional space if bounds exceded
	jmp 2f		//>;Cdepth)
//
jdepth:	pushl $Cand0	//<;Csprst)
Cdepth:	spush		//<;Csprst)
2:	movl PS,TS	//<;Xdepth)
	movl uref(vbotu),ecx
	addl $ps_min,ecx
	cmpl uref(l0u),ecx
	jna 3f		//;?;l0 within bounds
	movl ecx,uref(l0u)
3:	subl uref(s0u),TS
	sar $2,TS
	decl TS
	jns 1f
	movl uref(s0u),PS
	spush
1:	ret
// ( -- flg )
// ;for "-sp", by abort, error:
// ;restore stack, self-allocating +ps_xtd bytes if less than ps_min left.
// ;flg = -ve stack empty, +ve overflow and sp! only if out of bounds
Xsprst:	pushl NXL
Csprst:	leal 4(PS),ecx	//<Xdepth,Xpsprst,Xndrop,Xabort,Cqstack,cerd(Cerror);
	subl uref(s0u),ecx
	sar $2,ecx
	decl ecx	//; depth
	js Cdepth	//;?; rst empty stack
	movl uref(vbotu),ecx
	movl psboto-D(ecx),edx
	addl pssizo-D(ecx),edx
	subl $ps_min,edx//;min pqage_size/2 = 2K below top of mem
	cmpl PS,edx	//;limit-tos
	jnc jdepth
	subl $4,PS
// ;mremap: ebx=a1=addr,ecx=u1=old-len,edx=u2=new-len,esi=flags,edi=a2=new-addr
	pushal	//-
	movl psboto-D(ecx),ebx//;a1
	movl pssizo-D(ecx),ecx//;u1
	leal page_align(PS),edx
	andl $page_mask,edx//; page-aligned tos
	subl ebx,edx	//; actually used/required size
	pushl $MREMAP_MAYMOVE//;flg:=1
	cmpl edx,ecx
	jc 4f		//;?;preserve all current dstack data
	movl ecx,edx	//;max
4:	movl ebx,edi	//;a2
	addl $ps_xtd,edx//;u2
	popl esi	//;flg
	movl edx,s_edx(esp)
	sys_ mremap	//;separate to preventing segfault by access to un-allocated mem
	movl eax,s_eax(esp)
	cmpl $maxerr,eax
	popal	//-
	pushl edx	//;new size
	movl uref(vbotu),edx
	movl s0o-D(edx),ecx//;old, initial PS
	leal 4(ecx),ecx	//;clr dstack if mremap failed
	movl ecx,edx	//;leave top item for +ve error code (ovf)
	jnc 2f		//;?;error
	movl (esp),edx	//;new size
	movl TS,uref(psbotu)//; to +origin and uvari
	movl uref(vbotu),ecx
	movl TS,psboto	//;->new PS memory bottom to +origin (for 'cold' restart)
	movl edx,pssizo	//;->new size
	movl TS,psboto-D(ecx)//;->new PS memory bottom to vari-block
	movl edx,pssizo-D(ecx)//;->save new size
	addl $ps_rsv,TS	//;reserve
	movl edx,uref(pssizu)
	movl TS,edx	//;->pass new dstack bot
	leal 4(PS,TS),ecx//;adjust running ptr
	subl uref(s0u),ecx
	xorl TS,TS	//;flg success
2:	movl uref(s0u),PS
	addl edx,uref(l0u)
	subl PS,uref(l0u)//;local memory
	movl edx,uref(s0u)//;->copy to uvari
	movl edx,s0o	//;->copy to +origin
	movl uref(vbotu),PS
	movl edx,s0o-D(PS)//;->copy to to vari-block
	movl ecx,PS	//;new tos
	negl TS		//;flg > 0 for ovf error
	movl TS,(PS)
	popl edx	//;drop new size
1:	ret		//<;tos below bounds, PS already re-set
.nolist
// -----------------------------------------------------------------------------
// mapmem.S <eof>
