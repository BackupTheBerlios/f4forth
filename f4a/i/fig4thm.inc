//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
//; =============================================================================
//; fig4thm.inc - sysdef & origin macroes, uvari
//; =============================================================================
//;

//;
//; ===============================================================================
//; main memory references (memory map) - range { beginning } {    end        }
//; -------------------------------------------------------------------------------
//;
//;	program memory, not modifiable:
//;	.text:   [ f4 kernel ][ init.data ]     { 0. >4th - } {        (db) @ }
//;
//;	unitialized data section, upper bound modifiable w. <sys_brk>:
//;	.bss:    [dcbot) dictionary ==>dcsiz    {  (db} @+ }  { swap @ over + }
//;
//;	stack section, extending towards lower addresses by <push> cpu op.:
//;	.ss:     [ arg+env-strings ]            { (argp) @ }  { (argp) 4- @ 4* over + }
//;	.ss:     <== RS -rsmem- (r0]            {      rp@ }  {          r0 @ }
//;	return-stack lower bound limited by top bound of top mmap-ed memory block
//;
//;	separatly mmapped, size modifiable w. <sys_mremap>:
//;	         [u0) -uarea-  ]                {   u0  @+ }  { swap @ over + }
//;	         [s0) PS-> -datap- ]            {  (pb) @+ }  { swap @ over + }
//;	         [vabot) -variables- ]          {  (vb) @+ }  { swap @ over + }
//;	         [first) -bmag- (limit]         {  (ib) @+ }  { swap @ over + }
//;	         [tib) -tibm- ]                 {  (tb) @+ }  { swap @ over + }
//;
//;	over all memory range by Linux/ELF limited to 3G bytes.
//;	actual assignation by host system by pages, not before rsp. memory access.
//;
//;
//; kernel vari & deferred
//;    sysdef v,wminsz ,bokb,abort,xx  ,cbyln ,bbybufn ,zeq     ,bbstoreq,ledit ,bddgb    ,bbslb,mesg  ,bhbb,0   ,0    ,bbnumberb ,16     ,0   ,sem   ,0    ,0   ,blistb
.macro sysdef o,winsz=0,ok=0,abt=0,xx=0,cbyl=0,bbybuf=0,bfetch=0,bstore=0,edit=0,ddgreat=0,bsl=0,mesg=0,hb=0,uo=0,see=0,bnumberb=0,dotsn=0,id=0,semi=0,pto=0,is=0,list=0
winsz\o:.long	\winsz
limit\o:.long	0
first\o:.long	0
prev\o:	.long	0
use\o:	.long	0
tib\o:	.long	0
segv\o:	.long	0
oke\o:	.long	\ok
xx\o:	.long	\xx
cbyl\o:	.long	\cbyl
bbybuf\o:.long	\bbybuf
bfetch\o:.long	\bfetch
bstore\o:.long	\bstore
edit\o:	.long	\edit
lld\o:	.long	0,0,0,0,0,0,0,0
ddgreat\o:.long	\ddgreat
bslb\o:	.long	\bsl
mesg\o:	.long	\mesg
hb\o:	.long	\hb
see\o:	.long	\see
bnumberb\o: .long \bnumberb
dotsn\o:.long \dotsn
bidb\o:	.long \id
semiv\o:.long \semi
bptob\o:.long \pto
bisb\o:	.long \is
list\o:	.long \list
.endm
	.struct 0
	sysdef s
//;
//; <porigv affix, args...>
//;	range of '+origin' copied to uarea but, 
//;	not accessible by byte addressable named user vari
.macro porigv o,olin=0,obck=0,minl=0,argc=0,argp=0,envp=0,sigh=0,fper=0
// ;         olin,obck,  minl,argc,argp,envp,sigh  ,fperm  
porigin\o:
fperm\o:.long \fper
olink\o:.long \olin
obckl\o:.long \obck
minl\o:	.long \minl
argc\o:	.long \argc
argp\o:	.long \argp
envp\o:	.long \envp
sigh\o:	.long \sigh
.endm
.macro porigvo
porigino:
fpermo:	.long d_fperm
olinko:	.long 0
obcklo:	.long 0
minlo:	.long wminsz
argco:	.long 0
argpo:	.long 0
envpo:	.long 0
sigho:	.long sighdv
.endm
//;
//; <defuvari affix, args...>
//;	1st arg is name post-fix because, neither of (g)as(p) enable a variable 
//;	labels praefix, thus arg 'o' &c appended to #end# of labels; postfix 'u'
//;	for user vari structure and 'o' for "+origin" set-up.
//;	{ 0 +origin } returns address of 1st uvari dft item, ptr to "cold".
// ;uvari follow after 7 cells, beginning at 1st mmaped block, $40000000+:
// ;	olin,obck,minl,argc,argp,envp,sigh
.macro defuvari o,coo=0,wmo=0,u0=0,s0=0,r0=0,tib=0,width=0,warning=0,fence=0,dp=0,\
	vocl=0,blk=0,tin=0,out=0,scr=0,offset=0,context=0,current=0,state=0,base=0,\
	dpl=0,fld=0,csp=0,rsharp=0,hld=0,iquit=0,dcbot=0,dcsiz=0,psbot=0,pssiz=0,\
	bits=0,ltop=0,last=0,wsz=0,uvsiz=0,iobot=0,iosiz=0,timo=0,asci=0,dbg=0,upv=0,\
	es=0,vabot=0,vasiz=0,inu=0,dbtop=0,dbbot=0,dbvec=0,dbflg=0,p=0,i=0,cmo=0,\
	emit=0,type=0,qkey=0,key=0,expect=0,abt=0,atxy=0,xy=0,tsz=0,vptr=0,puop=0,\
	pflp=0,palp=0,bbi=0,pcsp=0,pcrp=0,maxy=0,err=0
origin\o:
// -------. byte-addressable <--
cold\o:		.long \coo
warm\o:		.long \wmo
		.long 0,0
// -------. uvari available by name - more w. <creauvari> macro
		.long 0		
s0\o:		.long \s0
r0\o:		.long \r0
tib\o:  	.long \tib
width\o:	.long \width
warning\o:	.long \warning
fence\o:	.long \fence
dp\o:		.long \dp
vocl\o:		.long \vocl
blk\o:		.long \blk
in\o:		.long \tin
out\o:		.long \out
scr\o:		.long \scr
offset\o:	.long \offset	
context\o:	.long \context	
current\o:	.long \current	
state\o:	.long \state	
base\o:		.long \base	
dpl\o:		.long \dpl	
fld\o:		.long \fld	
csp\o:		.long \csp	
rsharp\o:	.long \rsharp	
hld\o:		.long \hld
timo\o:		.long \timo
		.long 0
asci\o:		.long \asci
tbbot\o:	.long 0	
tbsiz\o:	.long \tsz
up\o:		.long \upv
escc\o:		.long \es
dcbot\o:	.long \dcbot
dcsiz\o:	.long \dcsiz
psbot\o:	.long \psbot
pssiz\o:	.long \pssiz
ltop\o:		.long \ltop
last\o:		.long \last
u0\o:		.long \u0
uvsiz\o:	.long \uvsiz
iobot\o:	.long \iobot
iosiz\o:	.long \iosiz
wsiz\o:		.long \wsz
vbot\o:		.long \vabot
vsiz\o:		.long \vasiz
vp\o:		.long \vptr
inter\o:	.long \inu
dbbot\o:	.long \dbbot
dbtop\o:	.long \dbtop
dbflg\o:	.long \dbflg
bits\o:		.long \bits
p\o:		.long \p
i\o:		.long \i
cmc\o:		.long 0
emit\o:		.long \emit
type\o:		.long \type
qkey\o:		.long \qkey
key\o:		.long \key
expect\o:	.long \expect
abort\o:	.long \abt
atxy\o:		.long \atxy
xy\o:		.long \xy
// -------^ byte-addressable <--
maxy\o:		.long \maxy
cmo\o:		.long \cmo	
l0\o:		.long \s0
puop\o:		.long \puop
pflp\o:		.long \pflp
palp\o:		.long \palp
bbi\o:		.long \bbi
quit\o:		.long \iquit
pcsp\o:		.long \pcsp
pcrp\o:		.long \pcrp
col\o:		.long 0
row\o:		.long 0
berrb\o:	.long \err
originend\o:
.endm
	.struct 0
uvariu:
    porigv u	//; start at poriginu
uuseru:		//; ref w. disp for UP
    defuvari u	//; originu .. originendu
uvarieu:
//;
// ----------------------------------------------------------------------------- 
// fig4thm.inc <eof>
