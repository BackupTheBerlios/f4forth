//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; inidata.S 
// =============================================================================
.psize 0
.att_syntax noprefix
.list
//; -- modifiable data, 'vmem' defaults --
//;	=> defining words' data blocks relocated to mmap-ed vari mem, after startup,
//;	   i.e. vocabularies and by memory ref/disp adressed deferred words.
	.section .data,"aw",@progbits
	.list
//;
D:
lfD:// ; voc-link ref base address
//;
//;	vocabulary linkage block - at 4-aligned address in writable memory:
//;	alignment byte[ -1] vocnfa: bytes[$81,$20,-1] dword[ voctop ] dword[ voclink ]
//;		vh\voc	[-1]
//;		nf\voc	[81,20,ff]	vnam - vnfa, dummy name terminates wordlist
//;		lf\voc  vlfa		ptr to top nfa in the rsp. vox
//;		vl\voc  vpfa		voc-link
//;		sd\voc			2nd-ary vlfa
//;		fl\voc			voc-flags, bit#0: voc already searched
//; by macro <vocblock vocname,2nd-ary vlfa,flags>
//;
//; initial, circular search order, modifiable w. {is}:
//;	local				implicitely 1st, no 2nd-ary
//;		big => forth => hidden => linux => editor => assembler => forth
//;			root		implicitely last, forth for 2nd-ary
//; recursion ends once all chained vocs searched.
//; chain ends at voc the 2nd-ary of which is zero, { 0 is vocname },
//; search order can (mostly) be 'sealed' with all 2nd-ary voc items zeroed.
//; "root" and "local" can't be excluded other than by header block modification.
// ;             vnfa      2nd, flg,link
	vocblock root,     forth,0,0	// ; voc-link base ref at <lfD>
	vocblock forth,    editor	// ; ! editor after forth !
	vocblock editor,   hidden
	vocblock hidden,   linux
	vocblock linux,	   assembler
	vocblock assembler,forth
	vocblock local,    D		// ; 'local' w.o. 2nd-ary context
//;
//; pre-set uvari <origino> .. <originendo> - in sync w. primary uvari.
// ; as-macroes with faulty parameter pssing which, falsely evaluate
// ; those ccp text-'macroes', thus passed by asm-vari:
.set udcsiz,ini_bss
.set upssiz,ini_ps
.set uwsiz,wminsz
.set uuvsiz,ini_uvr
.set uiosiz,ini_sbf	
.set uvasiz,ini_var	
.set utsz,ini_tib
//;
//; dft system vari at '+origin'
//;	by <copymem> to user area from <uvariv> till <uvariev>
//;	accessible w. { -disp +origin }. 
uvariv:
    porigvo
    defuvari o,	ocold,owarm,0,   0,   0,   0,    31,     -1,       ntop,   0,(prevvoc-D),0  ,0    ,0,\
		0,    0,       lfforth,  lfforth,  0,      10,    -1,   0,    0,    0,       0,iniquit,\
		0,      udcsiz, 0,      upssiz, ((expect_ins)|(lcdep)),ltop,  ntop,uwsiz,uuvsiz,0,\
		uiosiz, 0x7fffffff,d_pchr,0,   (originendu-uuseru-0x80),\
		'\\',0,      uvasiz, 0,    -1,     (task+4),udbvec, 0,      0,  7,  -1,\
		bemitb,btypeb,bqkeyb,bkeyb,bexpectb,abort,patxyq,pxyq,utsz,\
		(DE-D),0,     0,     bel,   pbbip,0,     0,     pmaxyq,berrb, msgt0
uvariev:                                                                    
#;   defuvari o, coo=0,wmo=0,u0=0,s0=0,r0=0,tib=0,width=0,warning=0,fence=0,dp=0,vocl=0,blk=0,tin=0,out=0,\
#;	        scr=0,offset=0,context=0,current=0,state=0,base=0,dpl=0,fld=0,csp=0,rsharp=0,hld=0,iquit=0,\
#;		dcbot=0,dcsiz=0,psbot=0,pssiz=0,bits=0,                ltop=0,last=0,wsz=0,uvsiz=0,iobot=0,\
#;		iosiz=0,timo=0,    asci=0,dbg=0,upv=0,\
#;		es=0,vabot=0,vasiz=0,inu=0,dbtop=0,dbbot=0, dbvec=0,dbflg=0,p=0,i=0,cmo=0,\
#;		emit=0,type=0,qkey=0,key=0,expect=0,abt=0,atxy=0,xy=0,tsz=0,\
#;		vptr=0,puop=0,pflp=0,palp=0,bbi=0,pcsp=0,pcrp=0,maxy=0,berrb=0,bmsgb=msgt0

//;	end of vari copied to user area
//;
fdatrv:	.long 0	//;fd by which stored to <stattv>\ keep
infctv:	.long 0	//;file attr. mask from that fd	  ) ..together w. <stattv>
//; stattv:	saved tty attributes		 /
stattv:	.fill termios_size,1,0
//; tattrv:	tty attributes for non-canonical i/o
tattrv:	.fill termios_size,1,0
//;
//; signal handler definition blocks, 'siginfo' type:
sighdv:
//;	sa_x:		signals which exit
sa_x: sigh(sigxit)
//;	sa_i:		rt signals, > 31
sa_i:
//;	sa_m:		signals which send a message
sa_m:
//;	sa_n:		signals which silently return
sa_n: sigh(sigret)
//;	sa_alrm:	sigalarm handler, deferred {(alarm)}
sa_alrm: sigh(alrm)
//;	sa_winch:	copy changed window sizes 
sa_winch: sigh(rsegwinch)
//;	sa_segv:	recover from segfault
sa_segv: sigh(segv)
//;	sa_fpe:		ret min/max
sa_fpe: sigh(fpe)
//;	sa_quit:	"warm" re-start (by key: CTRL'\' or 'print')
sa_qit: sigh(sigqit)
// ;	sa_kil:		sigkill, sigaction standard struct - spielt nicht -
// ;sa_kil:	.long sigkil,0,SA_SIGINFO,0,0
//;
//; global channel definition tables
//; <cdtblock>
//;	cdtv:
    cdtblock v
//; sysdef: system vari, deferred defaults
//;	tabelle beginnt bei { chmax cdt cdt-len + }, mit { cdt 0 cdt - constant cdt-len }
.ifeq _nv_
.set bbnumberb,0
.endif
//; sysdef v,winsz ,[ok],abort,<nul>,c/l,b/buf ,b@ ,b!      ,edit ,[-->],[\],message,#!,[see],
    sysdef v,wminsz,bokb,abort,xx,cbyln,bbybufn,zeq,bbstoreq,ledit,bddgb,bbslb,mesg,bhbb,0\
	    ,0  ,bbnumberb,16,0   ,sem,0    ,0   ,blistb
//;	     [n],[number],.sn,[id],;  ,[+to],[is],[list]
//; channels redirection bytes
//; <stdioblk>
//; 	chpv:	.byte	chans-2,chans-1 ; dr1, dr0
//; 	stdiov:	.byte(s) 0...16
    stdioblk v,dr0ch,dr1ch,stdin,stdout,stderr,workch,auxch,prtch,kbdch
//;
//; vt ctrl xec vectors from <Xexpect>
// ------------------------------------------------------------------------------------------
	.align 4,-1
//<expect);     A    B      C     D
csi1x:	.long vtcuu,vtcud,vtcur,vtbs,vteol,vtsol
//<expect);     1~   3~     4~    7~    8~   2~
csi2x:	.long vtsol,vtdel,vteol,vtsol,vteol,vtins
//<expect); 1st ctrl, keycode < 32
cc4t: 	.long ch00,ch00,ch00,ch00,ch00,ch00,ch00,xcpy	//;7:emit(bell)
	.long chbs,ctab,chnl,chnl,chff,ch00,xcpy,xcpy	//;8:back 9:tab 10:ready 11:<vt>:=<nl> 12:clearscreen 13:<cr>:=<nul> 14,15:emit
	.long ch00,ch00,ch00,ch00,ch00,ch00,ch00,ch00
	.long ch00,ch00,ch00,cesc,ch00,ch00,ch00,ch00
//;	uper<->lower case convertible (iso8859) chars bitflags array, uvari {(uc-lc)}.
//<;Cpfindq,Cseq,Xcx('uc-lc'),Xkey2alpha,C{r,}skip,C{r,}scan
cchart:	.long 0		//;   0, 32	; ctrls
	.long 0		//;  32, 48	; bl, pnct
			//;  48, 58	; num
			//;  58, 65	; pnct, at
	.long 0b0000111111111111111111111111110
			//;  65, 91	;< upper alpha
			//;  91, 97	; braces
	.long 0b0000111111111111111111111111110
			//;  97,123	;< lower alpha
			//; 123,127	; braces
	.long 0,0	//; 127,161	; del, undefined
			//; 161,192	; text descr, form
	.long 0b0111111111111111111111111111111
			//; 192,223	;< upper accented
			//; 223,224	; sz
	.long 0b0111111111111111111111111111111
			//; 224,255	;< lower accented
			//; 255,256	; y"
//;	<lchart> lower case character bitflags array, uvari { (uc-lc) @ 8 4* + }.
lchart:	.long 0		//;   0, 32	; ctrls
	.long 0		//;  32, 48	; bl, pnct
			//;  48, 58	; num
			//;  58, 65	; pnct, at
	.long 0b0000000000000000000000000000000
			//;  65, 91	;< upper alpha
			//;  91, 97	; braces
	.long 0b0000111111111111111111111111110
			//;  97,123	;< lower alpha
			//; 123,127	; braces
	.long 0,0	//; 127,161	; del, undefined
			//; 161,192	; text descr, form
	.long 0b0000000000000000000000000000000
			//; 192,223	;< upper accented
			//; 223,224	; sz
	.long 0b0111111111111111111111111111111
			//; 224,255	;< lower accented
			//; 255,256	; y"
DE://; .data section ends
//;
//; ------------------------------------------------------------------------------------------
//; ---- fixed data (after .text, page-aligned)
	.section .rodata,"a",@progbits
R:
qf:	sdatac "F4"
odotcpu:.long 1324807719//;"lwr86f" in radix 36
f4ver:	sdatac version,10//;f4 version string
ttyt:	.asciz "/dev/tty"//<;kbopen)
// ;:unused:;fd0t:	.asciz "/proc/self/fd/0"
//;	numeric eror message, screen no.
mgtmsg:	sdatac " msg #"
scrt:	sdatac " scr # "
nptn:	.byte 3		//; id. w. header-less word
nptt:	.ascii "-?-"
	.byte -3
//; path to: libdir, f4x.scr, docdir, figed.scr, edit, errno.h
libdirc: sdatac ilibdir
	.byte 0
f4xscrt: sdatac ilibdir/f4x.scr
	.byte 0
docdirc: sdatac idocdir
	.byte 0
fedscrt: sdatac ilibdir/figed.scr
	.byte 0
ldedt:	 sdatac edit
	.byte 0
lernot:	 sdatac lxerno
	.byte 0
//; strings for the "sh" word
sht:	.asciz "/bin/sh"
rg1t:	.asciz "sh"
rg2t:	.asciz "-c"
rg3t:	.asciz "ls -l ."
	.align 4,0
//; <stty sane> emergency setting, by 'rs'
ttyst:	.long 0x00002102//;iflag
	.long 0x00000005//;oflag
	.long 0x000004bf//;cflag
	.long 0x00008A3B//;lflag
	.byte 0		//;lineb
ttermio:.byte 0x03	//;VINTR	
	.byte 0x1C	//;VQUIT	
	.byte 0x7F	//;VERASE	
	.byte 0x15	//;VKILL	
	.byte 0x4	//;VEOF	
	.byte 0x0	//;VTIME	
	.byte 0x1	//;VMIN	
	.byte 0x0	//;VSWTC	
	.byte 0x11	//;VSTART	
	.byte 0x13	//;VSTOP	
	.byte 0x1A	//;VSUSP	
	.byte 0x0	//;VEOL	
	.byte 0x12	//;VREPRINT
	.byte 0x0F	//;VDISCARD
	.byte 0x17	//;VWERASE
	.byte 0x16	//;VLNEXT
	.byte 0x0	//;VEOL2
	.fill NCCS-(.-ttermio),1,0
	.align 4,0
.ifne _fc_
fact:// ;Xfactorial) sing & double factorials
	.long 0,1,0,1,0,2,0,6,0,24,0,120,0,720,0,5040,0,40320
	.long 0,362880,0,3628800,0,39916800,0,479001600
	.long 1,1932053504,20,1278945280,304,2004310016
	.long 4871,2004189184,82814,-288522240,1490668
	.long -898433024,28322707,109641728,566454140,-2102132736
.endif
//; base praefices wrt. base setting
nbase:	.byte 1,2,0,4,0,0,0,8,0,10,0,12,0,0,0,16,8,-2,0				//<Xabase)
npref:	.asciz "\"%.!...&.#.§...$@\\"//"
nprefe:
	.align 4,0
//; 'id+' words w. compiled text
txtt:	.long 3,pquoteq,psquoteq,pdotquoteq
//; branches check for see, id+, debug mode display
brat:	.long branch,pbra,mbra,zerobranch,nzbranch,ploopq,pplusloopq,lit,dlit,0	//<Xsee,Xtdb,ctbra)
//; conditional branches optimizing if, until
ift:	.long zerobranch,      nzbranch,       pbra, mbra	//::;,rbra	,rzbra	,rpbra    , rzbra//<cozbra)
// ;         0branch(if),-branch(0= if),+bra(0< if),-bra(0< 0=),//:: r> if , r>0= if, r> 0< if, r> 0< 0= if
//; opt. semis
exitoc:	.long semis,drops,drops2,drops4,rdrops,r2drops,sdrops2			//<Xexit,Xdexit)
	.long swapx,swap2x,rgreatx,r2greatx,ndrops,cswaps	# <- extend here
////.long swapx,swap2x,rgreatx,r2greatx	# <- extend here
	.long sdrops,s2drops
dexitoc:.long dsemis,zsemis
//; cfa entries, 'is' and '+to' runtime code
	.long isrefx-isrefc
isrefc:	.long dovari,douser,doucons,dobuser,domconstant,dovcons,docplus,do2cons,do2vari,dovconsp,doudfer,domdfer,dovoc	//<Xis)
isrefx:	.long storev,storeu,storeu ,storbu ,storem     ,storev ,store  ,twostov,twostov,storev  ,isumr  ,isvmr  ,isv2 
	.long ptrefx-ptrefc
ptrefc:	.long dovari,douser,doucons,dobuser,domconstant,dovcons,docplus,do2cons,do2vari,dovconsp	//<Xpto)
ptrefx:	.long pstov ,pstou ,pstou  ,pstobu ,pstom      ,pstov  ,psto   ,pstov2 ,pstov2 ,pstov   
// --------------------------------------------------------------------------- //
//; <ctlpt> names and <ctlpc> token list by <ctlp> for optimizing {literal}; file <cpldef.S>
ctlpt:	.byte 1
	.asciz "+ "
	.byte 1
	.asciz "- "
	.byte 1
	.asciz "* "
	.byte 1
	.asciz "/ "
	.byte 1
	.asciz "< "
	.byte 1
	.asciz "= "
	.byte 1
	.asciz "> "
	.byte 3
	.ascii "and"
	.byte 2
	.ascii "or "
	.byte 3
	.ascii "xor"
	.byte 2
	.ascii "s* "
	.byte 2
	.ascii "s/ "
//   ;           +,   -,   *,   /,   <,   =,   >, and,  or, xor,  s*,  u/
ctlpc:	.long litp,litb,litm,litd,litl,lite,litg,lita,lito,litx,liti,litu,lit
// --------------------------------------------------------------------------- //
//;
//; ---- syscall names & numbers from file <scdata.inc>
    .include "scdata.s"
//;
//; ---- vt ctrl, <Xexpect>
//;
//; ctrl:  BS:cur left, TAB cur right, ^L:cls DEL:del left, <del>:del at curs, <ins> tog ovw/ins
//; ascii: 08           09             12     127           1b,5b,33,7e        1b,5b,32,7e
//; rxvt:  08           09             12     127           1b,5b,33,7e        1b,5b,32,7e
// ------------------------------------------------------------------------------------------
//; ctrl:  arrow left:=BS, right:curs right, up:hist+, dn:hist-, pos1:1st posn, pos2:last posn
//; ascii: 1b,5b,44        1b,5b,43          1b,5b,41  1b,5b,42  1b,5b,31,7e    1b,5b,34,7e
//; rxvt:  1b,5b,44        1b,5b,43          1b,5b,41  1b,5b,42  1b,5b,37,7e    1b,5b,38,7e
//; xterm: 1b,5b,44        1b,5b,43          1b,5b,41  1b,5b,42  1b,5b,46       1b,5b,48
// ------------------------------------------------------------------------------------------
csi11:	.ascii "ABCDFH "	//;up,dn,rgt,lft,eol(xterm),sol(xterm)
csi21:	.ascii "134782 "
csi22:	.ascii "       ~"	//; len = 1+ len of modifiers, last is common dlm char
etypet:// ; by Xestg, for {e\stg}
	.ascii "abcefinrt-v "
etypete:
etypett:// ;dword provisional for multi(ple)-byte chars extension -
	.long BEL//;\a	<bel>
	.long  BS//;\b	<bs>
	.long 	0//;\c	not applicable: 'echo' w/o trailing <lf>
	.long ESC//;\e	<esc>
	.long  FF//;\f	<ff>
	.long CSI//;\i	<csi> by "<esc>[" to preventing xterm false CSI handling
	.long  NL//;\n	<new line> (should be the plain 'line feeed')
	.long  CR//;\r	<cr> carriage return
	.long TAB//;\t	<ht>
	.long  -1//;\-	noop, delimiter for numeric chars after \nnn
	.long  VT//;\v	<vt>
	.long  BL
//;	"expect" vt response (filled w. <nul>s towards msb, dword per item!) re <cvtseq>:
inst:	.long 0x00405b1b//; \e[@
clst:	.long 0x4a325b1b//; \e[2J clear screen & keep curs, by ^L
vdel:	.long 0x505b1b08//; curs back + del char display
vtrevt:	.long 0x6d375b1b//; \e[7m rev video
vtroft:	.long 0x6d37325b,0x1b//;\e[27m rev video off
vtcpos:	.long 0x6e365b1b//; \e[6n request cursor posn
vtelr:	.long 0x004b5b1b//; \e[K erase line from curpos till eol
// ;vtsave:	.long 0x0000371b//; \e7, save vt state
// ;vtrest:	.long 0x0000381b//; \e8 restore saved
// ;vtbold:	.long 0x6d315b1b//; \e[1m bold
// ;vtaoff:	.long 0x6d305b1b//; \e[0m attrib off
// ; vt?
vtqt:	.byte 27,91,99,0//;" \e[?"
vtqr1t:	.ascii "[?6c"
vtqr2t:	.ascii "[?1;2c"
// ; Cbnumberb
cmult:	.byte 0,BL
cmuld:	.ascii ".,"
cmuls:	.ascii "kKmMgGtTpP%%"
cmule:
// -- zero length string, <nul>-variable, ref to internal message strings
nullc:	.long 0
// -- standard 4th messages by ptr @<msg>, displayed if @warning < 0
msgt0:	sdatac "unknown word",0
msgt1:	sdatac "dstack empty",0
msgt2:	sdatac "dictionary full",0
msgt3:	sdatac "addressing mode",0
msgt4:	sdatac "not unique",0
   	sdatac ,0
msgt6:	sdatac "disc range",0
msgt7:	sdatac "dstack overflow",0
msgt8:	sdatac "disc access error",0
	sdatac ,0
	sdatac ,0
	sdatac ,0
	sdatac ,0
	sdatac ,0
	sdatac ,0
msgt15:	sdatac "F4 Forth (C) Berlin 2004 h-peter recktenwald",0
	sdatac ,0
msgt17:	sdatac "use only while compiling",0
msgt18:	sdatac "use only while executing",0
msgt19:	sdatac "conditionals not paired",0
msgt20:	sdatac "definition not finished",0
msgt21:	sdatac "in protected dictionary",0
msgt22:	sdatac "use only while 'load'ing",0
msgt23:	sdatac "off editting screen",0
msgt24:	sdatac "declare vocabulary",0
	sdatac ,0
	sdatac ,0
	sdatac ,0
	sdatac ,0
	sdatac ,0
	sdatac ,0
msgt31:	sdatac ,0
	.byte -1,-1
// --
	.align page_size,-1
// :DAS: <.align> :HIER: vermeidet segv-s ab gewisser (ja, welcher denn???) codebereichsgroesze...
RE:
.nolist
// -----------------------------------------------------------------------------
// inidata.S <eof>
