//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; inikernel.S 
// =============================================================================
.psize 0
.att_syntax noprefix
.list
	.global _start
	.text
//;
//; <Z> program memory base address
Z:
	.ascii F4MAGIC	//;"%F4QF%" 'magic' at offset 116 for linux <find>
// --------------------------------------------------------------------------- //
//;
//; system init, data stored to +origin area, before uvari initiation.
//;
//; <defsig>
//;	signal handlers set-up
defsig:	pushal
	xorl edi,edi
	movl edi,edx
	leal NSIG/8(edi),esi// ; for rt_sigaction
	leal 31(edi),ebx// ; available signal no's, 1..31
1:	movl $sgnsys,TS	// ; signals which remain unchanged
	btl ebx,TS
	jc 2f		// ;?; > 31 rt-signal, only
	movl $sa_x,ecx	// ; handler for exit sgs
	movl $sgmask,TS	// ; signals which exit
	btl ebx,TS
	jc 3f		// ;?; sig-exit
	addl $(sa_i-sa_x),ecx// ; messageing signals
3:
	sys_ rt_sigaction//;standard set-up
//;	rt signals
2:	addl $SIGRTMIN,ebx
	movl $sa_m,ecx
	sys_ rt_sigaction//;std rt signal hnd
	subl $SIGRTMIN+1,ebx
	jns 1b
//;	dedicated handlers
	addl $1+SIGSEGV,ebx
	movl $sa_segv,ecx
	sys_ rt_sigaction//;sa_segv> recovering segfault
	addl $SIGWINCH-SIGSEGV,ebx
	movl $sa_winch,ecx
	sys_ rt_sigaction//;sa_winch> new window sizes
	addl $SIGFPE-SIGWINCH,ebx
	movl $sa_fpe,ecx
	sys_ rt_sigaction//;sa_fpe> arthmetic exc
	addl $SIGQUIT-SIGFPE,ebx
	movl $sa_qit,ecx
	sys_ rt_sigaction//;sa_quit> sigquit
	addl $SIGALRM-SIGQUIT,ebx
	movl $sa_alrm,ecx
	sys_ rt_sigaction//;sa_quit> sigquit
	popal
	ret
//;	<fpe> catch arithmetic overflow from <divl ecx>
fpe:	pushal
	leal s_tor+4(esp),TS
	movl 8(TS),TS	//;arg(2)
	leal 19*4(TS),edx//;pc at offending opr
	movl (edx),TS
	movl (TS),TS	//;op-code
	movl ebx,ecx
	cmpl $task,NXL
	jnc 3f		// ; <next> at user supplied debug hnd
	cmpl $dbnxtop,NXL
	jnc 1f		// ; use supplied fpe hnd
	cmpl $dbnxbot,NXL
	jc 1f		// ; use supplied fpe hnd
3:	leal fpecx,ecx
	cmpb $0xf1,ah
	jz 1f		//; <div ecx>
	cmpb $0xf9,ah
	jnz 2f		//; not <idiv ecx>
	addl fpecxi-fpecx,ecx
1:	movl ecx,(edx)
	popal
	ret
2:	popal		// ; handle unrecoverable fpe as segv :contd:
//;	<segv> catch segfault
//;		1st -> <warm>, 2nd -> <cold>, next <bye>
segv:	pushl TS
	movl 16(esp),TS	// ; tor+arg(2)
	movl $dowarm,19*4(TS)//; posn(19) -> pc at offending opr
	incl segvv
	js 1f		//;"warm" if 1st segv
	addl $segcold-dowarm,19*4(TS)
	jz 1f		//;"cold" if 2nd segv
	addl $Xbyer-segcold,19*4(TS)//;else exit
	movl 12(esp),TS
	movl TS,(esp)
1:	popl TS
//;	<sigret> signals which silently return
sigret:	ret
//; signals handling code
//;	<sigxit> exitting signals
sigxit:	movl 4(esp),TS
	movl 12(esp),edx
	movl $Xbyer,19*4(edx)
	ret
//;	<sigqit> 'warm' re-start
sigqit:	pushl TS
	movl 16(esp),TS	// ; tor+arg(2)
	movl $dowarm,19*4(TS)
	popl TS
	ret
//;	<rsegwinch> updates main console window sizes for "max-xy
rsegwinch:
	orl $winch,uref(bitsu)//;set SIGWINCH reception flag
defscr:	call winsz
	js 2f		//;?;ioctl error
	testl ecx,ecx
	jg 1f		//;?;size may be valid
2:	movl $wminsz,ecx
1:	movl uref(vbotu),edx
	movl ecx,winszv-D(edx)//;dft for file i/o width from kernel vari, height:=0
	ret
//;	<alrm> vectored {[alarm]} sigalrm execution
alrm:	pushal
	leal nextloc,ebx//;rst NXL, corrupt from interrupt after syscalls which redefine the reg, e.g. at 'Cwf'
	spush
	spush
	leal balb+4,W
	call Cudfer
	popal
	ret
fpecx:// ; unsigned div 0, div ovf
// ; i:	TS:dvd.lo edx:dvd.hi ecx:dvs
// ; o:	TS:qot    edx:rmd    ecx:dvs
// ;   /1  TS:TS    edx: 0
	cmpl $1,ecx
	jz rpureg	//;?; ovf from div 1
	jc rzreg	//;?; div 0
	pushl TS	//; dvd.lo
	movl edx,TS	//; dvd.hi
	xorl edx,edx
	divl ecx
	popl TS		//; dvd.lo
	divl ecx	//; leave rmd
	ret
// ; signed div 0 sigfpe hnd
fpecxi:// ; signed idiv 0, idiv ovf 
// ; i:	TS:dvd.lo edx:dvd.hi ecx:dvs
// ; o:	TS:=q.lo, [PS]:=q.hi ecx:=dvd edx:=rmd
// ; ovf other than x/unity or x/zero: [PS]:qot.hi/ovf
	cmpl $-1,ecx
	jz rmureg	//;?; ovf from div -1
	jl 1f		//;?; other -ve
	cmpl $1,ecx
	jz rpureg	//;?; ovf from div 1
	jl rzreg	//;?; div0
1:	pushl TS	//; dvd.hi
	movl edx,TS	//; dvd.hi
	xorl edx,edx
	idivl ecx
	movl TS,(PS)	//; save q,hi
	popl TS		//; dvd.lo
	idivl ecx	//; leave rmd
	ret
rmureg:// ; ; ovf from div -1
	negl TS
rpureg:// ; ; ovf from div 1
	xorl edx,edx
	ret
rzreg:// ; ; div0
	orl edx,TS
	jz 2f		//;?; 0/0
	sarl $31,edx
	notl edx	//; +ve:-1 -ve:0
	leal 0x80000001(,edx,2),TS//;q: +/- max
	movl TS,edx
	ret
2:// ; ; 0/0
	incl TS
	ret
//;
//; --------------------------------------------------------------------------- 
//; initiate channels
//;
// ; 'vmem': channel data remain in +origin memory range
//;
//; <defscr>
//;	fetch initial screen sizes
// ;=>	re <rsegwinch>
winsz://; <max-xy) c: ecx := x|y
	pushl ebx
	movb $rstdout,TSl
	call brchid	//;ebx:=fd, edx:=cdt
	pushl $0
	pushl $0
	movl esp,edx
    	movl $TIOCGWINSZ,ecx
	sys_ ioctl	//;get window sizes
	popl ecx	//;x|y
	popl edx	//;n.i. pixel sizes
	popl ebx
	testl TS,TS
	ret
// ; copy saved <stattv> to workspace at <tattrv>
cpattr:	pushal		//<vtini(_n))
	pushl $termios_size
	lea tattrv,edi	//;copy to <tattrv>
	lea stattv,esi	//;from saved data at <stattv>
	popl ecx
	rep movsb
	popal
	ret
//; <vtrst>	restore console initial state, can be disabled w. { -1 (vtr) ! }
Xvtrst:	pushl NXL
vtrst:	pushal		//<Xbyer,Xrs)
	movl fdatrv,ebx
	leal stattv,edx	//;initially stored attributes
	testl ebx,ebx
.ifne 1
	js 1f		//;?;disabled
.else
	jns 2f
	leal ttyst,edx
.endif
2:	movl $TCSETSW,ecx
	sys_ ioctl	//;rst kbd channel state
	pushl $F_SETFL
	movl infctv,edx
	popl ecx
	sys_ fcntl	//;rst i/o channel flags
	pushl $K_XLATE	//; rst kbd mode
	movl $KDSKBMODE,ecx
	popl edx
	sys_ ioctl
1:	popal
	ret
//; <vtset>	save initial state of stdin, if console channel. i: -/- o: -/- no regs modified
vtset:	pushal		//<stoio)
	lea cdtptr(stdin),edx
	call vst
	jc 1f
	leal cdtptr(kbdch),edx
	call vst
1:	popal
	ret
vst:	testl $consflags,cdtref(tyfl)(edx)
	jz 1f
	pushl edx
	pushl $F_GETFL
	andl $~consflags,cdtref(tyfl)(edx)
	chanid edx,ebx
	leal infctv,edx
	popl ecx
	sys_ fcntl	//;read i/o channel flags
	movl eax,(edx)
	movl $TCGETS,ecx
	addl $4,edx
	sys_ ioctl	//;fetch stdin vt state for rst on exit
	cmpl $maxerr,eax
	jnc 2f
	movl ebx,-8(edx)//;save fd
	movl (esp),edx
	orl $consflags,cdtref(tyfl)(edx)
	leal stattv,edx
	sys_ ioctl	//;fetch stdin vt state for modification
	cmpl $maxerr,eax
2:	popl edx
1:	ret
//;
//; <iomode>  	( -- )				(k)(s)
//;	fcntl stdin to non-blocking & r/o, stdout to sync & w/o.
//;	NOTE:	channels opened w/ sys_dup (etc.) cannot be set to distinct 
//;		non/blocking mode, will be set to 'sync', as output channel.
iomode:leal cdtptr(stdout),edx	//<stoio); i: esi=initial vmem
	movl $(O_WRONLY|O_SYNC),ecx
	call Ciofcntl
	jmp Cinonbl
// ; determine & store i/o parameters
stoio:	cmpl $0,cdtref(chid)+cdtptr(kbdch)//<;_start)
	jns iomode		// already initiated
	call vtset		// store initial console state
	call kbopen
	call vtini		// :contd: <Cinonbl>
// ; set stdin to r/w non-blocking mode, o: ebx=ebx=fd, ecx:=ernum|flags
Cinonbl:movl $(O_RDWR|O_NONBLOCK),ecx//<;Ciomode);
	lea cdtptr(stdin),edx	//<Cisetbl); :contd: <Ciofcntl>
//;
//; <iofcntl>					(k)(s)
//;	modify i/o-flags of channel by edx=cdt to any of 
//;		ecx=(O_WRONLY|O_RDONLY|O_RDWR and O_NONBLOCK|O_SYNC)
//;	i: ebx=fd, ecx=pattern to set, esi=initial vmem
//;	o: ebx=ebx ecx:=ernum|flags, flags copied to cdtref(fctl) or, 0 on error.
Ciofcntl:			// :contd: <Ciononbl><stoio,Ciomode)
	pushal
	pushl $F_GETFL
	testl edx,edx
	jz 2f
	chanid edx,ebx
	movl $0,cdtref(fctl)(edx)
2:	popl ecx
	sys_ fcntl	//;read i/o channel flags
	movl TS,s_ecx(esp)
	testl TS,TS
	js 1f
	andl $~(O_WRONLY|O_RDONLY|O_RDWR|O_NONBLOCK|O_SYNC),TS//;clr flag-bits
	pushl $F_SETFL
	orl s_ecx(esp),TS//;set by ecx passed flags
	movl TS,edx
	popl ecx
	sys_ fcntl	//;write modified i/o flags
	movl TS,s_ecx(esp)
	testl TS,TS
	js 1f
	movl s_edx(esp),edx
	testl edx,edx
	jz 1f
	movl TS,cdtref(fctl)(edx)
1:	popal
	ret
// ; provisorisch: tty7 aufmachen, wenn 'rkey' mit kbd nicht spielt
.ifne 1
t7open:	pushal		//<Xrqky)
	leal tty7t,ebx
	jmp 2f
// ; open + config kbd channel, i: esi=vmem data template
kbopen:	pushal		//<stoio)
	leal ttyt,ebx
2:	movl cdtref(chid)+cdtptr(kbdch),TS
	xorl edx,edx
	cmpl $0,TS
	jns 1f		// ;?; if already open set kbd mode
	movl $O_RDWR|O_NONBLOCK,ecx
	sys_ open	//;open kbd channel
	movl TS,cdtref(chid)+cdtptr(kbdch)
1:	movl TS,ebx
	jmp vtini_n
.else
kbopen:	pushal		//<stoio)
	xorl edx,edx
	movl cdtref(chid)+cdtptr(kbdch),ebx
	cmpl edx,ebx
	jns vtini_n	// ;?; if already open set kbd mode
	pushl $O_RDWR
	leal ttyt,ebx
	popl ecx
	sys_ open	//;open kbd channel
	movl TS,cdtref(chid)+cdtptr(kbdch)
	movl TS,ebx
	jmp vtini_n
.endif
// ; i: esi=vmem data template
vtini:	pushal		//<stoio)
	movl cdtref(chid)+cdtptr(stdin),ebx
vtini_n:call cpattr	//<kbopen(stoio))
	leal tattrv,edx
	movl $TCSETSW,ecx
	andl $~(ICANON|ECHO),termios_c_lflag(edx)//;leave sigs enabled, disable erase/fill processing, echo
	orl $ONLCR,termios_c_oflag(edx)		// ;enabling NL<-CR/NL
	andl $~(INPCK|ISTRIP|IXON),termios_c_iflag(edx)// ;8bit chars
	movb $NVTIME,termios_c_cc+VTIME(edx)	//; timo * 1/10 s (if ~ICANON)
	movb $NVMIN,termios_c_cc+VMIN(edx)	//; min no. of chars for a single read opr, EAGAIN from sys_read only if NVMIN=0
	sys_ ioctl	//;set new tty mode
	popal
	ret
//;
//; <defchp> initial channels, i: esi=vmem data template
defchp:	xorl TS,TS	//<;_start) i/o console flags
    	leal cdtref(chid)+cdtptr(stdin),edx
	pushl edx
	lea chans(TS),ecx
	decl TS
1:	movl TS,(edx)
	addl $(stdout*cdtsize)-(stdin*cdtsize),edx
	decl ecx
	jns 1b
	incl TS
	popl edx
	leal setconsf(TS),ecx	//; i/o console flags
	movl $d_fperm,ebx	//; re 'fperm'
	movl TS,(edx)
	incl TS
	movl ecx,tyfl-chid(edx)
	movl ebx,perm-chid(edx)
	addl $(stdout*cdtsize)-(stdin*cdtsize),edx
	movl TS,(edx)
	incl TS
	movl ecx,tyfl-chid(edx)
	movl ebx,perm-chid(edx)
	addl $(stderr*cdtsize)-(stdout*cdtsize),edx
	movl TS,(edx)
	movl ecx,tyfl-chid(edx)
	movl ebx,perm-chid(edx)
//;	kbd-id remains -1 as initial flag to vt settings &c
	addl $(kbdch*cdtsize)-(stderr*cdtsize),edx
	movl ecx,tyfl-chid(edx)
	movl ebx,perm-chid(edx)
	ret
//;
//; -------------------------------------------------------------------------------------
//; init memory
//;
//;
//; =====================================================================================
//; memory map		range				F4 uvari/+origin pointers, size
//; -------------------------------------------------------------------------------------
//;	.text:   [ f4 kernel ][ init.data ]		basic program memory	(kernel)
//;	.bss:    [dcbot)dictionary .. ->(dcsiz]		at end of .text section,(ini_bss)
//;	.ss:     [<- RS -rsmem- (r0| arg+env-strings ]  at program return-stack,(ini_ss)
//;	mmapped: [s0) PS-> -datap- ] 			at @psbot, size @pssiz, (ini_ps)
//;	  "      [u0) -uarea-  ] 			at @u0,    size @uvsize,(ini_uvr)
//;	  "      [first) -bmag- (limit] 		at @iobot, @iosiz,	(ini_sbf)
//;	  "      [ -tibm- ] 				at @tbbot, @tbsiz,	(ini_tib)
//; =====================================================================================
//;

// ; mmapa	( size -- size addr|er )	(k)(s)
// ;	anonymous mmap r/w/-, in: requested size - re F4 word "mmapa"
Xmmapa:	pushl NXL
mmapa:	leal page_align(TS),T	//<;inimem)
	xorl TS,TS		//; p
	spush
	andl $page_mask,T
	pushl $(PROT_READ|PROT_WRITE)
	movl T,TS		//; u
	spush
	popl TS
	pushl $(MAP_PRIVATE|MAP_ANONYMOUS)//; 34
	spush
	popl TS
	spush
	xorl TS,TS		//; dummy fd
	spush
	spush
	pushl ebx
	movl TS,TOS(0)
	leal TOS(6),ebx
	sys_ mmap		//;anonymous, r/w, private
	movl T,TOS(6)
	TOSPULL(5)
	popl ebx
	cmpl $maxerr,TS		//;-4095, error if NC
	ret
// ; fetch vmem parameters from the 2nd ELF/program header
// ; i: -/- $orign ELF program origin va
// ; o: ecx=vmem-size. esi=file source addr, edi=destn addr
vblock:	leal orign,esi
	movl e_phoff(esi),edi	//; 1st program header
	addl esi,edi
	movzwl e_phentsize(esi),ecx
	addl ecx,edi		//;2nd ph
	addl p_offset(edi),esi	//;data source
	movl p_filesz(edi),ecx	//;size
	movl p_vaddr(edi),edi	//;destn
	ret
// ; 1st program header o: esi=destn addr
vblockd:movzwl e_phentsize+orign,esi//;size of a program header entry
	addl e_phoff+orign,esi	//; disp to program header table
	movl p_vaddr+orign(esi),esi//;destn
	ret
// ; r/o env & args, i: edx=W= -ve aligned tor; T=ecx= envp
wostk:	pushal			//<;setmem)
	xorl TS,TS
	leal -8(edx),edi
	leal -1(TS),ecx
	repne scasl		//;find top of env-vari
	leal page_align(edi),ecx
	andl $page_mask,ecx
	movl edx,ebx
	subl edx,ecx
	movl esp,edx
	movl ebx,esp
	pushl edx		//;alloc to stack section
	popl esp
	pushl $PROT_READ
	jmp 1f
// ; r/w sysvari, required for saved program code - re f4x.scr
rwmem:	pushal			//<;setmem)
	pushl $PROT_WRITE|PROT_READ
	leal D,ebx
	leal DE+page_align,ecx
	andl $page_mask,ebx
	andl $page_mask,ecx
	subl ebx,ecx
1:	popl edx
	sys_ mprotect		//;sysvari block in .text -> r/w
	popal
	ret
//;
//; <setmem>
//; in (1st or 2nd level subroutine, only!):
//;	edx = argp = esp-4, sto argc & argp to mem @+origin
//; allocates
//;	.ss:  screenfile-buffer, tib, user-area, return-stack (bottom end, self-extending)
//;	.bss: dictionary (extendable by sys_brk)
//;	mmap: PS, UP, tib+first/limit (resizeable by sys_mremap)
//; out
//;	regs:    KR, PS, RS, UR
//;	sysvari:     s0, r0, u0, argc, argp, dcbot, dcsiz, psbot, pstop, tib, tibsz
//;	stored to initial +origin for emergency re-start ("warm"), after segfault &c
//;	constitutes write-protected ranges of args and env vari, r/w enabled F4 sysvari
//;
setmem:	call rwmem	//<;_start) unprotect channel tables & other runtime data which are not acessed via vmem
	call vblockd		//; esi := 1st program header destn address (p_vaddr) :8051004:::
	movl ecx,argco-D(esi)	//;argc) ecx = T
	leal 4(edx,ecx,4),ecx	//;argp) edx = W
	movl edx,argpo-D(esi)
	movl ecx,envpo-D(esi)	//;envp):contd: <inimem>
	subl $page_align,edx
	and  $page_mask,edx	//; adjust rstack to page_size bounds
	popl fldo-D(esi)	//; 1st level <ret>
	popl cspo-D(esi)	//; 2nd level <ret>
	leal -ini_ss(edx),RS
//; - write protect args & env vari, set F4 bottom of rstack
	call wostk
//; - push/pop in .ss: return-stack - ini_ss
	movl RS,PS		//; intermediate PS
	pushl ecx		//; initiate .ss memory
	popl ecx
	movl edx,RS		//; return-stack
	leal quit,ecx
	pushl ecx		//; for safe return (and distinct cpu chache line(?)
	pushl ecx
	pushl ecx
	pushl ecx
	pushl ecx
	pushl ecx
	pushl ecx
	pushl ecx
	movl uvsizo-D(esi),TS	//;ini_uvr, pass inital user area size
	movl RS,r0o-D(esi)	//;r0)
//; - mmap user vari: $ini_uvr, uvsiz, olink, obckl, u0
	call mmapa		//; allocate user vari
	jnc Xbyer
	leal uuseru+0x80(TS),UR//;movl RS,UR
	movl UR,olinko-D(esi)	//; UR up-link
	movl UR,obcklo-D(esi)	//; UR down-link
	movl UR,u0o-D(esi)	//;u0)
//; - mmap: block-file i/o buffer - $ini_sbf, iosiz, iobot
	movl iosizo-D(esi),TS	//;iosiz) tib & blockbuf size
	call mmapa		//; allocate i/o buffers
	jnc Xbyer
	movl TS,ioboto-D(esi)	//;iobot)
//; - mmap: terminal i/o buffer - $ini_tib, tbsiz, tbbot
	movl tbsizo-D(esi),TS	//;iosiz) tib & blockbuf size
	call mmapa		//; allocate i/o buffers
	jnc Xbyer
	movl TS,tbboto-D(esi)	//;tib memory)
	movl TS,tibo-D(esi)	//;current tib)
//; - mmap: variable(s) data - $ini_var, vasiz, vabot
	pushl edi
	call vblock		//;edi := (p_vaddr) esi := (e_phoff+p_offset)
	movl edi,esi		//;vmem F4 src, (ELF-head destn)
	movl ecx,TS		//;  size
	popl edi
	addl vsizo-D(esi),TS
	addl $page_align,TS
	andl $page_mask,TS
	movl TS,vsizo-D(esi)	//;vsiz) vari buf size
	call mmapa		//; allocate
	jnc Xbyer
	movl TS,vboto-D(esi)	//;vabot)	:40004000:::
//; - mmap: parameter stack - $ini_ps, pssiz, psbot, s0
//;   set up last, for minimal relocation probability.
	movl pssizo-D(esi),TS	//;pssiz) PS size = ini_ps
	call mmapa		//; allocate parameter stack
	jnc Xbyer
	movl TS,psboto-D(esi)	//;psbot)	:40006000:::
	leal ps_rsv(TS),PS	//; 8 'cells' reserve, init s0 to empty dstack :40006020:::
	movl PS,s0o-D(esi)	//;s0) sto initial ptrs to '+origin'	8051004+0dc ::
	movl PS,l0o-D(esi)	//;l0)					8051004+1cc ::
//; - sys_brk: dictionary - $ini_bss, dcbot, dcsiz, dp
	pushl ebx
	xor  ebx,ebx
	sys_ brk		//;find .bss bottom address
	addl $page_align,TS
	andl $page_mask,TS
	movl TS,dcboto-D(esi)	//; membot, initial dp true address
	movl TS,dpo-D(esi)	//; dictionary ptr
	movl dcsizo-D(esi),ebx	//;$ini_bss
	addl TS,ebx
	sys_ brk		//;extend .bss for dictionary
	popl ebx
	subl dcboto-D(esi),TS	//;top-bot
	pushl cspo-D(esi)	//; rst 2nd <ret>
	pushl fldo-D(esi)	//; rst 1st <ret>
	movl TS,dcsizo-D(esi)	//;dcsiz) top of dictionary (1st un-accessible address)
	ret
// ; global sysvari, before <copymem>
inivari:call vblockd		//<;docold)
	movl ioboto-D(esi),ecx
	movl ecx,firstv-D(esi)
	movl ecx,prevv-D(esi)
	movl ecx,usev-D(esi)
	leal bblk*nbuf*nscr(ecx),edx//;bblk=(blkno+blknl+@bbybufv), bytes per block: blk#, content, <nul>s
	addl iosizo-D(esi),ecx
	movl edx,limitv-D(esi)
	movl ecx,tibv-D(esi)
	ret
// ; after <inivari>, copy from +origin to initial user vari, sto F4 vari into vari mem
copymem:pushal			//<;segcold)
	call vblockd		//; esi:=destn addr
	leal uvariv-D(esi),esi	//; -32 +origin, <fperm>
	leal uref(uvariu),edi	//; base addr of per job sysvari & user area, immediately after initial voc-blocks
	movl $((uvariev-uvariv)/4),ecx//;4a = 72
	rep movsl		//; copy range [porigino,uvariev], <+origin> wrt <cold> entry & uvari
	call vblock
	shrl $2,ecx
	movl vboto-D(edi),edi	//;mmap-ed destn address
	movl edi,ebx
	rep movsl		//;copy sysvari, vocabulary defn blocks
	leal uref(voclu),edx	//;					(asm:ebp+0b0, @:098
	addl ebx,(edx)		//; + <vbot> - <D>
	subl edx,(edx)		//; 'voc-link' ptr relative to itself
	leal vlroot-D(ebx),ecx
	negl ecx
	movl ecx,vlroot-D(ebx)	//;end of voc-link, for ..+@ := 0	(asm:fffffff8
	movl $0,lldv
	popal
	ret
//;
//; -------------------------------------------------------------------------------
//; <copyargs>
//;	copy command line arguments to tib, until '--'
//;
// ; i:	argp	ptr to 1st argument, count at argp-4
// ; o:	argp	ptr to 1st remainig arg, 1st inactive argument, '--' or none
// ;	 '' -4	argc, remaining args' count
// ;	blku	0
// ;	inu	0 if any args copied
// ;	bitsu	<cargs>, <xargs> & <scrix> flags in '(bits)' uvari
// ;		scrixb,scrix	set if executed as a script for implicite 'bye' at eof.
// ;		cargsb,cargs	unset if commandline args evaluated or, none present.
// ;		xargsb,xargs	set if executeable cmd-line args passed.
// ; c: TS,ecx,edx,sysv
//; while executing by '#!' 1st initial arg of top line is some path and the rsp 
//; script name: { /usr/local/bin/f4a here . pad 11 dump } wherin the 1st arg is 
//; the top line after the path+filename, which is 2nd. command line args follow.
//; the assumed script file will then be investigated whether the '#!' is at 1st
//; column of 1st line, otherwise closed and the name treated as an ordinary arg.
//;	./script cmdline-args -> [ script-args script-filename cmdline-args ]
copyargs:		//<;segcold)
.ifne _cy_
    xorl ecx,ecx
0:
    movl ecx,ecx
    movl ecx,ecx
    loop \(0b)
.endif
	testb $cargs,uref(bitsu)
	jz 1f
nxargs:			//<;Xbhbb)
.ifne _cy_////
    int $3
.endif
	pushl edi	//;16:PS
	pushl esi	//;12:IP
	pushl uref(tbsizu)//;8:dec. tib size
	pushl uref(argpu)//;4:incr. ptr
	pushl uref(argcu)//;0:decr. args count
	movl uref(tbbotu),edi
	subl $6,8(esp)	//;provide for trailing <nul>s
	movl edi,uref(tibu)
	movl $0,uref(blku)
	jmp 4f
3:	movl 4(esp),esi//<;test for more args
	cmpl $0,(esi)
	jz 9f		//;?;none
	addl $4,4(esp)	//;next arg, inc. <argp>
	movl (esi),esi
	call rglen	//;ecx:=arg stg length
	movl (esp),edx	//;pass count of args left
	movl 16(esp),eax//;pass PS
	call rghb	//;?;test for xec-script header
	testb $scrix,uref(bitsu)
	jnz 9f		//;?;scripting mode
8:	subl ecx,8(esp)	//;rmg tib size
	jg 7f		//;?;sufficient
	call rgalc	//;extend the buffer
	jnc Xbyer	//;?;terminate if allocation failed
	subl ecx,edx	//;less current stg
	movl edx,8(esp)	//;new rmg size
7:	cmpl $3,ecx
	jnz 6f		//;?;not the delimiter
	cmpw $'-'<<8|'-',(esi)
	jz 2f		//;?;active args end
6:	rep movsb	//;copy arg to tib
	movb $BL,-1(edi)
	orb $xargs,uref(bitsu)//;flag executeable args present
4:	decl (esp)	//<;loop entry
	jns 3b
9:	incl (esp)
2:	xorl edx,edx
	popl ecx	//;rmg args count
	movl edx,(edi)	//; zero terminate @tib
	movl edx,uref(inu)
	subl uref(argcu),ecx
	addl ecx,uref(argcu)
	popl edx	//; <argp> after last arg
	popl ecx	//;drop rmg tib size
	movl edx,uref(argpu)//;adjust argp uvari
	popl esi
	popl edi
10:	andb $~cargs,bitso//;flag args scanned
1:	ret
rglen:	xorl eax,eax
	movl edi,edx
	lea -1(eax),ecx
	movl esi,edi
	repne scasb
	notl ecx
	movl edx,edi
	ret
// ;test whether an executeable script, if so, 'include' else treat as an arg
// ;scriptfile name in 1st or 2nd arg, i.e. F4 arg(0) or arg(1)
ctscrix:pushl edi	  //;12:PS
	pushl esi	  //;08:IP
	or ecx,ecx	  //; force NC
	pushl uref(argpu) //;04
	pushl uref(argcu) //;00
2:	decl (esp)
	js 1f
	movl 4(esp),esi
	movl (esi),esi
	addl $4,4(esp)
	cmpl $0,esi
	jz 1f		//;?;no more args
	call rglen
	movl 12(esp),TS	//;pass PS
	movl (esp),edx	//;arg no.
	call rghb
	testb $scrix,uref(bitsu)
	jz 2b		//;?;scripting mode
	negl edi	//;force C
1:	popl edi	//;drop argp
	popl esi
	popl edi
	ret
// ;i: ecx=stg len, edx=rmg args, esi=string
// ;o: scrix bit in '(bits') set and @blk:=-1 if F4 script
rghb:	subl uref(argcu),edx
	addl $2,edx
	js 2f		//;?;past 2nd arg
	cmpb $CHR_DIR,(esi)
	jz 1f		//;?; abs path
	cmpw $CHR_DIR<<8|CHR_LDIR,(esi)
	jnz 2f		//;?; not a relative path
1:	pushal
.ifne _cy_
    movl $0x11111111,PS	//;hier sind die script-args bereits gelesen
    int $3
.endif
	leal 8(eax),PS	//;PS
	decl esi
	movzbl bytec(rbyo),TS
	movl esi,TOS(1)
	spush
	movzbl rdr1ch+chpv,TS
	call copen	//;edx:=cdt
	jnc 3f		//;?;not a file
	testl $chfmmap,(edx)
	jnz 5f		//;?; 2nd args scan for scriptfile
	pushl TS
	call Cmmapf	//;edx=edx ( ch -- size addr ), mmap-flags set
	spush
	popl TS
	call Cclose	//;edx:=cdt of closed channel
	spop		//;@tos = mmapped mem
5:	cmpw $'!'<<8|'#',(TS)
	jnz 4f
	orb $scrix,uref(bitsu)	//;flag scripting mode
	popal
2:	ret
4:	call Cmunmapa
3:	popal
	ret
// ;1st level subroutine, extend tib memory by page_size 2 *
// ;in:  ecx=size of next arg, edi=posn in tib
// ;out: edx=rmg size, esi=posn in tib
// ;mremap: ebx=a1=addr,ecx=u1=old-len,edx=u2=new-len,esi=flags,edi=a2=new-addr
rgalc:	pushal
	pushl $MREMAP_MAYMOVE
	leal 6+page_size+page_size+page_align(ecx),edx
	movl uref(tbbotu),edi
	movl uref(tbsizu),ecx
	popl esi
	addl ecx,edx
	andl $page_mask,edx
	movl edi,ebx
	sys_ mremap	//;extend tib for rmg cmd-line args
	movl eax,s_eax(esp)
	cmpl $maxerr,eax
	jnc 1f
	movl s_edi(esp),ecx//;current posn in tib
	movl ecx,edi
	subl uref(tibu),ecx//;rmg size
	movl edx,uref(tbsizu)//;->new size
	movl eax,uref(tibu)//;->new tib
	movl eax,uref(tbbotu)
	subl ecx,edx	//;edx:=new rmg size
	subl $6,edx	//;reserve
	addl eax,ecx	//;ecx:=new posn in tib
	movl edx,s_edx(esp)//;=rmg size
	movl ecx,s_edi(esp)//;=posn in tib
	negl eax	//;rst C
1:	popal
	ret
//;
//; -------------------------------------------------------------------------------
//;
//; <_start>
//; 	initial code entry, drops into cold start at { 0 +origin }
_start:	popl ecx		//; argc
	popl edx
	decl ecx
	movl esp,edx
	call setmem	//;memory set-up
	call defsig	//;signal handlers
	call defchp	//;channel defn tables
	call stoio	//;non-canonical, non/blocking console i/o
	call vblockd
	cmpl $0,argco-D(esi)
	jz 1f
	orb $cargs,bitso-D(esi)
1:
//;
//; -------------------------------------------------------------------------------
//; base of fig-FORTH proper
//;
	.align 4
ocold:	jmp docold
	.align 4
owarm:	jmp dowarm
	.align 4	//; for the cpu pipes
// ( -- )
Xf4b:	leal f4ver,TS	//; display version no. and exit
	call Ccount
	call Ctype
// ( -- )
Xbye:	spush
	xorl TS,TS
	leal bbi+4,W
	jmp Cudfer
// ( n -- ) ; 'bbi' quit executeable with exit-code n
Xbyer:	call vtrst	//;'bbi' quit the program w. exit code @tos
	movl TS,ebx
	sys_ exit	//;leave the program, ret exit-code @ebx
//
Xrs:	pushl NXL	//;reset console state
	spush
	movl $('c'<<8|ESC)<<8|SI,TS
	call Cemit	//; g0 charset, <RIS>
	jmp vtrst
//
docold:	call Cclrsegv	//;1st segfault -> <warm>, 2nd -> <cold>, next <bye>
	call inivari	//;blockfile ptrs sys-vari
segcold:call copymem	//;initate user vari area
	call copyargs	//;sto comandline args to tib
	call defscr	//;fetch initial main window sizes
	movl ecx,winszv	//;  copy to +orig
	leal lfforth-D,ecx//;forth definitions
	addl uref(vbotu),ecx
	movl ecx,uref(currentu)
	movl ecx,uref(contextu)
	call Cempty	//;rst virgin dictionary, clears all after @fence
dowarm:	movl uref(s0u),PS
	movl uref(r0u),RS
	pushl cdtref(rnum)+cdtptr(dr0ch)//;->warning
	lea nextloc,ebx
	leal doabort+4,IP
	andl $0,uref(stateu)
	popl uref(warningu)
Xrpstore:
	movl uref(r0u),RS
	NX
.nolist
// -----------------------------------------------------------------------------
// inikernel.S <eof>
