//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; forthpw.S 
// =============================================================================
.psize 0
.att_syntax noprefix
.list
.text
//;
//; -------------------------------------------------------------------------------
//; Forth headers, auxilary data fields for header-less primitives
//; -------------------------------------------------------------------------------
//;
//;
//; ---- prim. words ----
//;
.list
//;
//; noop   	( -- )				(p)	root
//;	no operation, first 4th word. @cfa: jmp <next>.
	c noop,"noop",Xnoop,,,root
// ; c probier,"probier",Xprobier
.nolist
//;
//; ---- headerless prim. ----
//;
	dbn "p"
pstov:	.long Xpstov	//;cons,vari,vcons+
	dbn "P"
pstov2:	.long Xpstov2	//;2cons,2vari
	dbn "b"
pstou:	.long Xpstou	//;uvari, ucons
	dbn "B"
pstobu:	.long Xpstobu	//;uvari, ucons
	dbn "m"
pstom:	.long Xpstom	//;mem-cons
	dbn "D"
isumr:	.long Xisumr	//;user-deferred
	dbn "d"
storev:	.long Xstorev	//;mem by disp
	dbn "U"
storeu:	.long Xstoreu	//;mem by disp wrt UP
	dbn "u"
storbu:	.long Xstorbu	//;mem by byte-disp wrt UP
	dbn "M"
storem:	.long Xstorem	//;mem by pc-relative ptr
	dbn "T"
twostov:.long Xtwostov	//;double by disp
	dbn "v"
isvmr:	.long Xisvmr	//;deferred by disp
	dbn "V"
isv2:	.long Xisv2	//;2nd-ary voc
	dbn "a"
oconsq:	.long Xoconsi	//;whether rstdin is a terminal
	dbn "T"
inter:	.long Xinter	//;"interpret" leading part
	dbn "N"
include1:.long Xinclude1//;leading part of 'include'
	dbn "n"
include2:.long Xinclude2//;trailing part of 'include'
	dbn "O"
load1:	.long Xload1	//;leading part of 'load'
	dbn "o"
load2:	.long Xload2	//;trailing part of 'load'
	dbn "S"
lsload1:.long Xlsload1	//;leading part of 'l-load'
	dbn "s"
lsload2:.long Xlsload2	//;trailing part of 'l-load'
	dbn "E"
xledit:	.long Xledit	//;deft for "edit", inially loading the editor
	dbn "Ö"
xlloadq1:.long Xxlloadq	//;leading part of 'xlload?'
	dbn "ö"
lloadq1:.long Xlloadq	//;leading part of 'lload?'
	dbn "L"
lload1:	.long Xlload1	//;leading part of 'lload'
	dbn "l"
lload2:	.long Xlload2	//;trailing part of 'lload'
	dbn "X"
xlload1: .long Xxlload	//;leading part of 'xlload'
//;
//; ---- forth prim prim. ----
//;
.list
//;
//; forth  	( -- )				(v)(i)	root
//;	the main kernel words' vocabulary.
	c forth,"forth",dovoc,imed,,root
	.long nfforth-D
//;
//; root   	( -- )				(v)(i)	forth
//;	the miniml vocabulary which will always implicitely be searched by '-find', 
//;	after the particular word wasn't found in @current or @context chains, rsp.
	c root,"root",dovoc,imed,,forth
	.long nfroot-D
//;
//; assembler  	( -- )				(v)(i)	forth
//;	vocabulary, lo-level code compiling.
	c assembler,"assembler",dovoc,imed,,forth
	.long nfassembler-D
//;
//; hidden  	( -- )				(v)(i)	root
//;	frequently used non-standard words, words for internal use, runtimes.
	c hidden,"hidden",dovoc,imed,,root
	.long nfhidden-D
//;
//; editor  	( -- )				(v)(i)	forth
//;	vocabulary, text processing words.
	c editor,"editor",dovoc,imed,,forth
	.long nfeditor-D
//;
//; linux  	( -- )				(v)(i)	root
//;	vocabulary, host system specific words; linux syscalls, ports access &c.
	c linux,"linux",dovoc,imed,,root
	.long nflinux-D
_rv//;
_rv//; ranum  	( -- )				(v)(i)	forth
_rv//;	vocabulary of words wrt real numbers by ordered pairs of doubles, 'ranum'.
_rv	c ranum,"float",dovoc,imed,,root
_rv	.long nfranum-D
//;
//; local  	( -- )				(v)(i)	forth
//;	local data and code compiling support. 
//;	2nd-ary context should normally left un-set but, because the {local} voc
//;	implicitely always searched 1st, can for instance, be used for temporary 
//;	preferrence of some other vocabulary, out of the regular search order.
//;	F4 kernel provides basic, minimal locals support by memory de/allocation 
//;	and the basic words to referring, fetching and storeing the 'local' data.
//;	nevertheless, direct memory access will always be faster than the indexed 
//;	access to locally (wrt the word which did the rsp. set-up) stored data.
//;	re {l0}, {l!}, {l@}, {l!0}, {l@0}, {l2!}, {l2@}, {l2!0}, {l2@0}.
	c local,"local",dovoc,imed,,forth
	.long nflocal-D
//;
//;
//; ---- debugging ----
//;

//; f4ver  	( -- n )			(cons)
//;	F4 sedecimal version number by packed bytes [REL|VER|EXT|TAG]. re {-V}.
//;	{ f4ver 256 /mod 256 /mod 256 /mod . . . . } displays the version number.
	c f4v,"f4ver",Xf4v
//; -V		( -- n )			(cons)
//; --version	( -- n )			(cons)
//;	display f4 version & cr, and exit. re {f4ver}.
	c f4V,"-V",Xf4b
	c f4m,"--version",Xf4b
//;
//; int3   	( -- )				(p)	linux
//;	insert a (p)tracing breakpoint
//;	F4A: optionally in kernel if asm-cons _i3_  =/= 0.
_i3	c int3,"int3",Xint3,,,linux
//;
//; tdb		( char | num -- )		(p)
//;	hi-level debugging display, set debug mode n by redirection of 
//;	the rsp <next> entry, i.e. user programs need no special code.
//;	parameter may either be the rsp. character code or numeric value.
//;	if active, displays name of next word to execute and:
//;	char  =	 "0"	(revert to) standard next entry, debugging 'off'.
//;		<esc>	ditto, immediately exit the traced word.
//;		 "1"	continuous display of the rsp next words' names.
//;		 "2"	'1' but, (*) only words after lower bound "task" or @(gb).
//;		 "3"	'2' and, cpu-flags and dstack display.
//;		 "4"	'3' and, (*) only words after "task" or addr @(gb).
//;		 "5"	'0' but ready to receiving other {tdb} keys.
//;		other	no change.
//;	(*) range of debugging can be restricted to between the contents of uvari 
//;	    (gb) and (gb) 4+, by dft 0,-1 to debugging any words, unrestrictedly.
	c tdb,"tdb",Xtdb
//;
//; tdb?   	( -- n )			(p)
//;	ret numeric debug mode - re "tdb", rst dft if invalid & ret -ve
//;	F4A: optionally in kernel if asm-cons _tq_  =/= 0.
_tq	c tdbq,"tdb?",Xtdbq
//;
//; o		( -- )				(p)
//;	toggles "(ok)" vector between "[ok]" and {.s} execution
	c o,"o",Xo
//;
//; t		( -- )				(p)
//;	dump 112 bytes of tib
//;	F4A: optionally in kernel if asm-cons _tm_ =/= 0.
_tm	c t,"t",Xt
//;
//; dump   	( a n -- )			(p)
//;	display +16 aligned n bytes of memory content 
//;	at -16 aligned address a.
	c dump,"dump",Xdump
//;
//; .s		( -- )				(p)
//;	display up to 16 top data-stack items - re {.sn}.
//;	asm debugging: all regs & eflags preserved.
	c dots,".s",Xdots
//;
//;
//; ---- utils ----
//;

//;
//; hash11 	( a u -- n )			(p)	linux		vtsys
//;	the ELF hashing procedure, ret hash figure n for u chars of string at a.
	c h11,"hash11",Xhash11,,,linux
//;
//; --		( -- )				(i)(p)	root
//;	synonym {\} and, active command-line arguments delimiter. if leading a 
//;	list of program execution command line arguments, introduces 'in-active' 
//;	args which will be passed to the rsp. F4 program, retrievable by ref. to 
//;	the '(argp)' user vari.
//;	while scripting w. '#!', an inner list of args, at the script's heading 
//;	line, takes precedence over those passed with the command line.
//;	the arguments count, at { (argp) 4- }, begins with 0 and { (argp) @ @ } 
//;	points to the rsp. argument, the executing file name already stripped off.
	c slsl,"--",Xbslb,imed,,root
//;
//; edit   	( -- )				(d)	editor
//;	enter the screen-file editor. deferred.
//;	initially loads the fig-forth line editor from <figed.scr> in 'libdir'.
//;	can be altered w. 'is' or 'to' or, by disp @pfa wrt @(vp).
	c edit,"edit",domdfer,,,editor
	.long editv-D,ledit+cfa2pfa
//; 
//; [id]   	( pfa1 -- pfa2 | pfa2 flag )	(d)	hidden
//;	deferred, user definable extension code to {id+}. xec'd immediately after 
//;	the cfa to test was located in dictionary above kernel space. receives pfa 
//;	@tos, should either leave the address unmodified for further processing or 
//;	push the additional pseudo-numeric flag "[id]", e.g. entered as "]id[, for 
//;	termination, with pfa2 adjusted to next valid wa, for continuation.
//;	initial and dft. definition set to in-active mode. modify w. {is}.
//;	dedicated extensions should be set up for new words which cpl literal data 
//;	on de-compiling of which {id.}, {see} &c might crash, otherwise.
	c bidb,"[id]",domdfer,,,hidden
	.long bidbv-D,0
//;
//; id.		( nfa -- )			(p)
//;	by nfa display a word's name, reversed video display if 'immediate'.
//;	basic de-compiler word; re {id+}, {see}, {#see}.
	c iddot,"id.",Xiddot
//;
//; id+		( a1 -- a2 )			(p)
//;	display the word's name the cfa of which was @ a1, ret a2 of next cell 
//;	or, the rsp. following posn if a1 pointed to a branch or literal.
//;	re {id.}, {see}, {#see}, {[id]}, {[see]}.
	c idplus,"id+",Xidplus
//;
//; [see]	( pfa -- pfa )			(d)	hidden
//;	deferred, user definable extension code to {see}. xec'd immediately before
//;	the decoding procedure, after internal {-find} and test for the rsp. words
//;	existence. receives pfa @tos, should leave the address unmodified or place 
//;	another hi-level pfa @tos if the extended handling not applicable or {see} 
//;	should continue, else replace @tos w. zero for immediate termination.
//;	initial and dft. definition set to '0', in-active. modify w. {is}.
//;	re {id.}, {id+}, {see}, {#see}, {[id]}.
	c bseeb,"[see]",domdfer,,,hidden
	.long seev-D,0
//;
//; (see)	( a -- a n | 0 0 )		(p)	hidden
//;	decompile a word by pfa a; re {see}; supporting -re- {debug} of f4x.scr.
	c pseep,"(see)",Xpseep,,,hidden
//;
//; see		ccc( -- a n | 0 0 )		(p)
//;	Simple decompiler which displays names compiled into word ccc. pfa a and 
//;	count u of decompiled bytes left for further analysis or continuation.
//;	can be terminated w. <esc>, halted w. <bl> and cont'd w. any other key.
//;	execution ends at dictionary bounds or, if either, a terminal branch into 
//;	a colon defn or, ";S" was encountered. If {;S} was due to {exit} &c, the 
//;	decompiling procedure can be continued w. {#see}:
//;		{ see name   + ( ccc, -- a n -- a2 ) #see }.
//;	NOTE: new definitions which compile literal data should provide a suitable 
//;	      extension to the basic decompiler word {id.}; re {[id]}, {[see]}. 
	c see,"see",Xsee
//;
//; #see   	( a -- a n | 0 0 )		(p)
//;	decompile hi-level forth code beginning at addr a; else identical "see".
//;	find where compiling aborted due to a non-existing word (message 0):
//;		{ unsmudge last @ dup id. pfa #see 2drop }
//;	which shows the words - if any - compiled, till termination.
//;	re {id.}, {id+}, {see}, {[id]}, {[see]}.
	c nsee,"#see",Xnsee
//;
//; v		ccc( -- )			(i)(p)	root
//;	if found, display word's name ccc and cfa, else 0 - re "id."
//;	"v" is 'immediate', can be executed while compiling.
	c v,"v",Xv,imed,,root
//;
//; vlist  	( -- )				(p)	root
//;	display the current vocabulary's list of words, 
//;	'immediate' words in reversed video mode.
	c vlist,"vlist",Xvlist,,,root
//;
//;
//; ---- optimizing cpl'd code ----
//;
// -- optimizer word headers required for {see} &c which otherwise would run into next header & segfault --
//;

//;
//;	optimizing 'literal' extendable by any opr the name of which consists
//;	of up to 3 chars, file <cpldef.S>, opt suppressed if uvari {cmo} := 0.
//;	word headers omitted if asm-vari '_oh_' set to zero (vari set by default)

//;
//; lit+  	( x1 -- x2 )			(p)	hidden
//;	{literal} optimising w. immediately following {+}.
_oh	c litp,"lit+",Xlitp,,,hidden
oh_ litp: .long Xlitp
//; lit-  	( n1 -- n2 )			(p)	hidden
//;	{literal} optimising w. immediately following {-}.
_oh	c litb,"lit-",Xlitb,,,hidden
oh_ litb: .long Xlitb
//; lit-*  	( n1 -- n2 )			(p)	hidden
//;	{literal} optimising w. immediately following {*}, unsigned, cpu-opr 'imul'.
_oh	c litm,"lit*",Xlitm,,,hidden
oh_ litm: .long Xlitm
//; lit-/  	( n1 -- n2 )			(p)	hidden
//;	{literal} optimising w. immediately following {/}, unsigned, cpu-opr 'idiv'.
_oh	c litd,"lit/",Xlitd,,,hidden
oh_ litd: .long Xlitd
//; lit-u*    	( n1 -- n2 )			(p)	hidden
//;	n.i: { unsigned literal } opt. w. immediately following {*}, cpu-opr 'mul'.
_oh	c liti,"lit-u*",Xliti,,,hidden
oh_ liti: .long Xliti
//; lit-u/    	( n1 -- n2 )			(p)	hidden
//;	n.i: { unsigned literal } opt. w. immediately following {/}, cpu-opr 'div'.
_oh	c litu,"lit-u/",Xlitu,,,hidden
oh_ litu: .long Xlitu
//; lit-a   	( n1 -- n2 )			(p)	hidden
//;	{literal} optimising w. immediately following {and}.
_oh	c lita,"lit-a",Xlita,,,hidden
oh_ lita: .long Xlita
//; lit-o    	( n1 -- n2 )			(p)	hidden
//;	{literal} optimising w. immediately following {or}.
_oh	c lito,"lit-o",Xlito,,,hidden
oh_ lito: .long Xlito
//; lit-x   	( n1 -- n2 )			(p)	hidden
//;	{literal} optimising w. immediately following {xor}.
_oh	c litx,"lit-x",Xlitx,,,hidden
oh_ litx: .long Xlitx
//; lit<   	( n1 -- n2 )			(p)	hidden
//;	{literal} optimising w. immediately following {<}.
_oh	c litl,"lit<",Xlitl,,,hidden
oh_ litl: .long Xlitl
//; lit=   	( n1 -- n2 )			(p)	hidden
//;	{literal} optimising w. immediately following {=}.
_oh	c lite,"lit=",Xlite,,,hidden
oh_ lite: .long Xlite
//; lit>   	( n1 -- n2 )			(p)	hidden
//;	{literal} optimising w. immediately following {>}.
_oh	c litg,"lit>",Xlitg,,,hidden
oh_ litg: .long Xlitg
//;
//; unsigned	( -- )				(i)(p)	hidden
//;	immediate, bit-flag in uvari (bits); for use with {literal} to compiling
//;	the unsigned variant of the opr which follows immediately after {literal}:
//;	{ .. [ num ] unsigned literal / .. } compiles { .. lit-u/ (num) .. } 
	c sgd,"unsigned",Xsgd,imed,,hidden
//;
//; r2;		( -- d )(R: d -- )		(p)	hidden
//;	pop double from rstack & terminate, opt. { 2r> ;s } runtime.
	cnfa r2greatx,,,hidden
	.ascii ">2;"
	clfa r2greatx,Xr2greatx,,,hidden
//;
//;
//; r1;		( -- n ) (R: n -- )		(p)	hidden
//;	pop n from rstack & terminate opt. { r> ;s } runtime.
	cnfa rgreatx,,,hidden
	.ascii "r;s"
	clfa rgreatx,Xrgreatx,,,hidden
//;
//; w2;		( d1 d2 -- d2 d1 )		(p)	hidden
//;	terminate execution, opt. { 2swap ;s } runtime.
	cnfa swap2x,,,hidden
	.ascii "w2;"
	clfa swap2x,Xswap2x,,,hidden
//;
//; w1;		( n1 n2 -- n2 n1 )		(p)	hidden
//;	terminate execution, opt. { swap ;s } runtime.
	cnfa swapx,,,hidden
	.ascii "w1;"
	clfa swapx,Xswapx,,,hidden
//;
//; wc;		( n1 n2 n3 n4 -- n4 n3 n2 n1 )	(p)	hidden
//;	terminate execution, opt. { cswap ;s } runtime.
	cnfa cswaps,,,hidden
	.ascii "wc;"
	clfa cswaps,Xcswaps,,,hidden
//;
//;
//; 1r;		(R: n -- )			(p)	hidden
//;	drop n from rstack & terminate current word, opt. { rdrop ;s } runtime.
	cnfa rdrops,,,hidden
	.ascii "1r;"
	clfa rdrops,Xrdrops,,,hidden
//;
//;
//; 2r;		(R: d -- )			(p)	hidden
//;	drop d from rstack & terminate current word, opt. { 2rdrop ;s } runtime.
	cnfa r2drops,,,hidden
	.ascii "2r;"
	clfa r2drops,Xr2drops,,,hidden
//;
//; nd;		( n -- )			(p)	hidden
//;	ndrop n cells & terminate execution of current word, opt. { ndrop ;s }.
	cnfa ndrops,,,hidden
	.ascii "nd;"
	clfa ndrops,Xndrops,,,hidden
//;
//; 4d;		( q -- )			(p)	hidden
//;	drop quad & terminate execution, opt. { 4drop ;s } runtime.
	cnfa drops4,,,hidden
	.ascii "4d;"
	clfa drops4,X4drops,,,hidden
//;
//; 2d;		( n1 n2 -- )			(p)	hidden
//;	drop double & terminate execution, opt. { 2drop ;s } runtime.
	cnfa drops2,,,hidden
	.ascii "2d;"
	clfa drops2,X2drops,,,hidden
//;
//; 1d;		( n -- )			(p)	hidden
//;	drop n & terminate execution of current word, opt. { drop ;s } runtime.
	cnfa drops,,,hidden
	.ascii "1d;"
	clfa drops,Xdrops,,,hidden
//;
//; 2s;		( d1 d2 -- d2 )			(p)	hidden
//;	drop d1 & terminate execution, opt. { 2swap 2drop ;s } = { 2sdrop } runtime.
	cnfa sdrops2,,,hidden
	.ascii "s2;"
	clfa sdrops2,Xsdrops2,,,hidden
//;
//; ss;		( n1 n2 n3 -- n3 )		(p)	hidden
//;	drop n1, n2 & terminate execution, opt. { sdrop sdrop ;s } runtime.
	cnfa s2drops,,,hidden
	.ascii "2s;"
	clfa s2drops,Xs2drops,,,hidden
//;
//; 1s;		( n1 n2 -- n2 )			(p)	hidden
//;	drop n1 & terminate execution of current word, opt. { sdrop ;s } runtime.
	cnfa sdrops,,,hidden
	.ascii "1s;"
	clfa sdrops,Xsdrops,,,hidden
//;
//;
//; ---- runtimes, cpl'd code ----
//;

//;
//; dodoes 	( -- )				(cons)	hidden
//; dodoes@	( -- )				(cons)	hidden
//; dodoesv	( -- )				(cons)	hidden
//;	by "does>" et al. compiled run-time code.
	c ds,"dodoes",doconstant,,,hidden
	.long dodoesgreat
	c df,"dodoes@",doconstant,,,hidden
	.long dfdoesgreat
	c dv,"dodoesv",doconstant,,,hidden
	.long dvdoesgreat
//;
//; ;s		( -- )				(p)
//;	Terminate execution of current word, EXIT runtime.
//;	can be used to early terminate compiling from block-file screen.
	cnfa semis
	.ascii ";s"
	clfa semis,Xsemis
//;
//; +;s		( n -- )			(p)	hidden
//;	terminate execution of current word if n = 0. -EXIT runtime.
	cnfa zsemis,,,hidden
	.ascii "+;s"
	clfa zsemis,Xzsemis,,,hidden
//;
//; -;s		( n -- )			(p)	hidden
//;	terminate execution of current word if n =/= 0. -EXIT runtime.
	cnfa dsemis,,,hidden
	.ascii "-;s"
	clfa dsemis,Xdsemis,,,hidden
//;
//; lit		( -- n )			(p)
//;	runtime code of 'literal'.
	c lit,"lit",Xlit
//;
//; dlit   	( -- n )			(p)
//;	runtime code of "dliteral".
	c dlit,"dlit",Xdlit//<;interpret)
//;
//; r+		( -- n )(R: n1 -- n2 )		(c)(p)	hidden
//;	copy next cell from code-body of caller to @tos, advance instruction 
//;	ptr accordingly; substitutes { r> r> dup 4+ >r swap >r @ }.
//;	use: { : w1 r+ .. @ .. ;  : w2 ... w1 [ data , ] ... ; }
//;	F4: conditionally in kernel if asm cons _hx_ =/= 0.
_hx	c rplus,"r+",Xrplus,,,hidden
//;
//; ~bra   	( n1 n2 -- | n1 )		(p)	hidden
//;	leave n1 @tos and branch if n1=/=n2, else drop both, n1 and n2.
	c neqbra,"~bra",Xneqbra,,,hidden
//;
//; -bra   	( n -- )			(p)	hidden
//;	branch if n < 0, by optimizing 'if'.
	c mbra,"-bra",Xmbra,,,hidden
//;
//; +bra   	( n -- )			(p)	hidden
//;	branch if n >/ 0, by optimizing 'if'.
	c pbra,"+bra",Xpbra,,,hidden
//;
//; branch 	( -- )				(p)	hidden
//;	unconditionally change program flow, cpld by "ELSE", "AGAIN"
//;	disp to cpld destn. cfa should be compiled immediately after.
	c branch,"branch",Xbranch,,,hidden
//;
//; -branch	( n -- )			(p)	hidden
//;	change program flow if n =/= (0,-0), cpld by "UNTIL"
//;	disp to cpld destn. cfa should be compiled immediately after.
	c nzbranch,"-branch",Xnzbra,,,hidden
//;
//; 0branch	( n -- )			(p)	hidden
//;	change program flow if n = (0,-0), cpld by 'if' &c.
//;	disp to cpld destn. cfa should be compiled immediately after.
	c zerobranch,"0branch",Xzbra,,,hidden
//;
//; (loop) 	( -- )				(p)	hidden
//;	LOOP runtime, increments the loop index by one and tests 
//;	for loop completion.
	c ploopq,"(loop)",Xploopq,,,hidden
//;
//; (+loop)	( n -- )			(p)	hidden
//;	+LOOP runtime, adds signed n to the loop index and tests 
//;	for loop completion.
	c pplusloopq,"(+loop)",Xpploopq,,,hidden
//;
//; (do)   	( -- )				(p)	hidden
//;	DO runtime, moves the loop control parameters to the return stack.
	c pdoq,"(do)",Xpdoq,,,hidden
//;
//; (;code)	( -- )				(p)	hidden
//;	";CODE" runtime, introducing lo-level runtime part of a defining word.
//;	receives ptr to the newly defined words' pfa in Forth W-register (edx).
	cnfa psemicodeq,,,hidden
	.ascii "(;code)"
	clfa psemicodeq,Xpsemicodeq,,,hidden
//;
//; >input 	( -- )(RS -- .xx. )		(p)	hidden
//;	saves current files/terminal input description to RS. re {input>}.
//;	stores 12 cells to rs: dr1-id, dr0-id, context, current, base, offset, 
//;	  in, blk, scr, tib, (bits), the 1st four channel relocation indices.
	c sinput,">input",Xsinput,,,hidden
//;
//; input> 	( .xx. -- )			(p)	hidden
//;	restores files/terminal input description from RS. re {>input}.
	c rinput,"input>",Xrinput,,,hidden
//;
//; (s")   	( -- a u )(R: ip -- ip' )	(p)	hidden
//;	compiled by s" which returns address and count of the 
//;	immediatley following, compiled counted string data.
	cnfa psquoteq,,,hidden
	.ascii "(s\")"		// ;"
	clfa psquoteq,Xpsquoteq,,,hidden
//;
//; (")		( -- a ) (R: ip -- ip' )	(p)	hidden
//;	compiled by " which returns the address of the 
//;	immediatley following, compiled counted string data.
	cnfa pquoteq,,,hidden
	.ascii "(\")"		// ;"
	clfa pquoteq,Xpquoteq,,,hidden
//;
//; (.")   	( -- ) (R: ip -- ip' )		(p)	hidden
//;	compiled by { ." } which transmits the immediately 
//;	following, compiled counted string data to stdout.
	cnfa pdotquoteq,,,hidden
	.ascii "(.\"\)"		// ;"
	clfa pdotquoteq,Xpdotquoteq,,,hidden
//;
//;
//; ---- deferred initial/default words ----
//;

//;
//; csegv	( -- )					hidden
//;	clear, i.e. pre-set to 2, the segfaults counter; by {<quit>}.
	c clrsegv,"csegv",Xclrsegv,,,hidden
//;
//; -quit-	( -- )					hidden
//;	initially by {<quit>}, before entry into the interpreter loop; noop if 
//;	in scripting mode, else determines console flags, terminating w. {cr} 
//;	if {kbd} channel is a terminal.
	c iniq,"-quit-",Xiniq,,,hidden
//;
//; up!		( a -- )			(p)
//;	set disp'd ptr into user area from base address a, at uvari {fperm};
//;	new UR := a+disp, sto previous UR to new 'u0', new UR to previous 'u0'.
	c ups,"up!",Xups
//;
//; [+to]  	( pfa -- pfa|0 )		(d)(p)
//;	{+to} extension vector, immediately after initial {'}, should either pass
//;	the unmodified pfa or, zero after which the procedure will be terminated.
	c bptob,"[+to]",domdfer,,,hidden
	.long bptobv-D,0
//;
//; [is]  	( pfa -- pfa|0 )		(d)(p)
//;	{is} extension vector, immediately after initial {'}, should either pass
//;	the unmodified pfa or, zero after which the proedure will be terminated.
	c bisb,"[is]",domdfer,,,hidden
	.long bisbv-D,0
//;
//; [inter]	( pfa flg 1 | 0 -- pfa flg 1 | 0 ) (u)	hidden
//;	user-deferred word, executed by {interpret} immediately after {-find}.
//;	xec as well as dft initially set to zero, executing as a "noop" word.
//;	the intercepting routine should leave the data on dstack unmodified if 
//;	not applicable or, leave an immediately executeable pfa, e.g. 'noop', 
//;	the imed bit of flg set, e.g. hex c1, and 1, for early termination of 
//;	rsp. interpreter pass - re <f4f.scr> for an example, by the word {df}.
//;	the vector can be altered w. {is}, { is< [inter] } reverts to zero opr.
	c pinterq,"[inter]",doudfer,,,hidden
	udefl(interu),0
//;
//; [#!]   	ccc( -- )			(p)	hidden
//;	'#!' dft vector. introducing an executeable script. 'abort', otherwise. 
	c bhbb,"[#!]",Xbhbb,,,hidden
//;
//; [abort]	( -- )				(u)	hidden
//;	execution vector, by {error} if @warning < 0.
//;	user-deferred, dft is {abort} which can be fetched from pfa+4.
//;	redirection w. { ' new-word is [abort] }.
	c pabortq,"[abort]",doudfer,,,hidden//<;error)
	udefl(abortu),abort+cfa2pfa
//;
//; [error]  	( line -- in blk )		(p)
//;	Error notification and partial system re-initialization, according to
//;	the content of uvari 'warning':
//;		< 0	execute user-deferred '[abort]',
//;		  0	display the rsp. message number, #line, and {quit}
//;		> 0	display latest interpreted word, and the message 
//;			indexed by the line parameter, wrt. dr0, then {quit}.
//;	current at time of the error @in and @blk values pushed to stack for 
//;	further processing. re {message}, also for the message no. encoding.
	c berrb,"[error]",Xerror
//;
//; [;]		( -- )				(p)
//;	default xec vec for {;}. check stack balance, terminate colon definition.
//;	modifies last cfa entry to "branch"+disp, if a colon defn and @cmc > 0.
//;	other words prior to ';' will be combined w. {;s} to a single wa:
//;		drop, 2drop, 4drop, ndrop, swap, 2swap,
//;		rdrop, 2rdrop, r>, 2r>, sdrop, sdrop sdrop, 2sdrop.
	cnfa sem,,,hidden
	.ascii "[;]"
	clfa sem,Xsemi,,,hidden
//;
//; [bbi]  	( n -- )			(p)(s)	linux
//;	quit the 4th job w. exitcode n: close channels, link memory back to system.
//;	restores console state, leaves charset & screen content unchanged - re 'rs'.
	c pbbip,"[bbi]",Xbyer,,,linux
//;
//; [alarm]	( n -- )			(u)	linux
//;	by uvari {(al)} deferred SIGALRM handling word, dft {bel}, re {alarm}.
	c balb,"[alarm]",doudfer,,,linux
	udefl(palpu),bel+cfa2pfa
//;
//; [-->]  	( -- )				(p)	hidden
//;	dft for {-->}
	c bddgb,"[-->]",Xddgreat,,,hidden
//;
//; [\]		( -- )				(p)	hidden
//;	dft for {\}
	c bbslb,"[\\]",Xbslb,,,hidden
//;
//; [atxy] 	( n1 n2 -- )			(p)	hidden
//;	dft 'at-xy'.
	c patxyq,"[atxy]",Xatxy,,,hidden
//;
//; [xy]   	( - n1 n2 )			(p)	hidden
//;	dft 'xy'.
	c pxyq,"[xy]",Xxy,,,hidden
//;
//; [x]		( -- ) (R: ip -- )		(p)	hidden
//;	dft procedure of the deferred <nul> word - re {x}.
	c xx,"[x]",Xx,,,hidden
//;
//; [msg]  	( n -- )			(p)	hidden
//;	if 'warning' =/= 0, send the text of line n relative to line 0 of scr 4.
//;	dft opr. for deferred -re- 'message'.
	c mesg,"[msg]",Xmessage,,,hidden
//;
//; [b!]   	( n a -- )			(p)	hidden
//;	'b!' dft; sto significant bytes of multi-byte char n to address a.
	c bbstoreq,"[b!]",Xbstore,,,hidden
//;
//; [ok]   	( -- )				(p)	hidden
//;	dft commandline prompt, "="/">" while interpreting/compiling;
//;	changed to "#"/"$" if {expect} or {edlin} in characters inserting mode.
	c bokb,"[ok]",Xbokb,,,hidden
//;
//; [emit]     	( n -- )			(p)(s)	hidden
//;	dft 'emit'; transmit multi-byte char, l.s. byte 1st of cell n to stdout.
	c bemitb,"[emit]",Xemit,,,hidden
//;
//; [?key]   	( -- c 1 | 0 )			(p)(s)	hidden
//;	dft '?key'; if available, ret byte from stdin & tf, else ret ff.
	c bqkeyb,"[?key]",Xqkey,,,hidden
//;
//; [key]		( -- c )		(p)(s)	hidden
//;	dft 'key'; fetch any one byte from stdin, waiting until char received
	c bkeyb,"[key]",Xkey,,,hidden
//;
//; [type]     	( a u -- )			(p)(s)	hidden
//;	dft 'type'; transmit u characters (bytes) from address a to stdout.
//;	drop args & ret if either, a or u is zero. 
//;	u limited to 64K. for longer text re "writef" which is not limited.
	c btypeb,"[type]",Xtype,,,hidden
//;
//; [expect] 	( a u -- )			(p)(s)	hidden
//;	dft {expect}; fetch up to u bytes until <nl> from stdin to address a.
	c bexpectb,"[expect]",Xexpect,,,hidden
//;
//;
//; ---- cpu ----
//;

//;
//; p@		( port -- n )			(p)	linux
//;	read 32-bit value from port; re {p!}, {pw!}, {pc!}, {pw@}, {pc@}
//;	F4A: optionally in kernel if asm-cons _pa_ =/= 0.
_pa	c pf,"p@",Xpf,,,linux
//;
//; pw@		( port -- n )			(p)	linux
//;	read 16-bit value from port; re {p!}, {pw!}, {p@}, {pc@}, {pc!}
//;	F4A: optionally in kernel if asm-cons _pa_ =/= 0.
_pa	c pwf,"pw@",Xpwf,,,linux
//;
//; pc@		( port -- n )			(p)	linux
//;	read 8-bit value from port; re {p!}, {pw!}, {p@}, {pw@}, {pc!}
//;	F4A: optionally in kernel if asm-cons _pa_ =/= 0.
_pa	c pcf,"pc@",Xpcf,,,linux
//;
//; p!		( n port -- )			(p)	linux
//;	store 32bit n data to port; re {pw!}, {pc!}, {p@}, {pw@}, {pc@}
//;	F4A: optionally in kernel if asm-cons _pa_ =/= 0.
_pa	c ps,"p!",Xps,,,linux
//;
//; pw!		( n port -- )			(p)	linux
//;	store l.s. 16-bit of n to port; re {p!}, {pw!}, {p@}, {pw@}, {pc@}
//;	F4A: optionally in kernel if asm-cons _pa_ =/= 0.
_pa	c pws,"pw!",Xpws,,,linux
//;
//; pc!		( n port -- )			(p)	linux
//;	store l.s. 8-bit of n to port; re {p!}, {pw!}, {p@}, {pw@}, {pc@}
//;	F4A: optionally in kernel if asm-cons _pa_ =/= 0.
_pa	c pcs,"pc!",Xps,,,linux
//;
//; tick   	( -- d )			(p)	hidden
//;	count of cpu timing clocks since host system start (RDTSC cpu opr).
//;	NOTE: returns the exact timing figure, independently of any interrupts;
//;	      rdtsc operation might have to be enabled at kernel compilation by 
//;	      enabling 'model specific regs' (? - 'rdtsc' was always functional)
	c tic,"tick",Xtic,,,hidden
//;
//;
//; >swap<   	( n1 n2 n3 n4 -- n4 n3 n2 n1 )	(p)	hidden
//;	F4A: optionally in kernel if asm-cons _dx_ and _gs_ =/= 0.
//; cswap   	( n1 n2 n3 n4 -- n4 n3 n2 n1 )	(p)	hidden
//;	revert sequence of top 4 cells, { swap 2swap swap }, i.e. convert 
//;	the top four cells' order between little endian and big endian.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx _gs	c gsl,">swap<",Xgswapl,,,hidden
_dx	c cswap,"cswap",Xcswap,,,hidden
//;
//; bswap   	( n1 -- n2 )			(p)	hidden
//;	convert byte order of cell n between little and big endian (BSWAP cpu opr).
	c bsw,"bswap",Xbswap,,,hidden
//;
//;
//; ---- system ----
//;

//;
//; bbi     	( n -- )			(u)
//;	quit the 4th job w. exitcode n: close channels, link memory back to system.
//;	default {[bbi]} can be altered w. 'is' or, by cfa stored to uvari {(bbi)}.
	c bbi,"bbi",doudfer
	udefl(bbiu),pbbip+cfa2pfa
//;
//; bye		( -- )				(p)(s)
//;	exit 4th job: restore console state, close channels, link resources back 
//;	to the host system. executes user-deferred {bbi} w. zero exitcode.
//;	re {rs}, {(vtr)}
	c bye,"bye",Xbye
//;
//; cold   	( -- )				(p)	root
//;	restart w. initial dictionary; { forth definitions empty warm }.
//;	a {save'}ed binary re-scans/executes the initial command line arguments.
	c cold,"cold",ocold,,,root
//;
//; warm   	( xx -- xx )			(p)
//;	restart w. clean stacks and decimal base.
//;	"warning" set to 1 if dr0 file accessible, else -1.
//;	executes { rp! sp! [abort] begin quit again }.
	c warm,"warm",owarm
//;
//; alarm   	( n -- )			(s)
//;	alarm signal after (n) seconds by syscall 'alarm', signal handler 
//;	executes deferred {[alarm]} which may be set to user supplied word.
//;	example: : a xy 0. at-xy .t&d at-xy 40 alarm ;	\ {.t&d} from f4x.scr
//;		   ' a is [alarm]	1 alarm		\ enable
//;		   ' 0 is [alarm]			\ disable
	c alarm,"alarm",Xalarm
//;
//; sh		( a1 -- er|pid )		(p)	linux
//;	host system command shell execution of program by name and arguments at a1.
//;	a1 expected as a <nul> terminated, counted string, dft " ls -l ." if a1=0.
//;	F4A: optionally in kernel if asm-cons _sh_ =/= 0.
_sh	c sh,"sh",Xsh,,,linux
//;
//; erfile 	( -- a )			(h)
//; docdir 	( -- a )			(h)
//; libdir 	( -- a )			(const)
//;	<nul> terminated, counted string of F4 auxilary files installation dir.
//;	path+name of dft screenfile follows immediately, { libdir count + 1+ } 
//;	which, is immediately followed by documention installation directory name,
//;	glossary in 'docdir': { libdir count + 1+ count + 1+ } and, 
//;	linux ernum 'erfile': { docdir count + 1+ count + 1+ count + 1+ }.
	c libdir,"libdir",doconstant
	.long libdirc
//;
//; >4th   	( a -- ptr )			(p)	hidden
//;	convert addr to ptr wrt beginning of f4 code (.text) section
	c tforth,">4th",X24th,,,hidden
//;
//; v>       	( a -- disp )			(p)	hidden
//;	convert true address pointing to mmap-ed vari-mem to disp wrt vmem base.
//;	re {(vb)}, {(vp)}, {-vallot}, {vallot}, {v,}, {wv,}, {cv,}, {>v}
	c vmt,"v>",Xv2,,,hidden
//;
//; >v     	( disp -- a )			(p)	hidden
//;	convert disp wrt base address of mmap-ed vari-mem to true address.
//;	use w. {v>} to re-storeing after intermediate vmem allocation access.
//;	re {(vb)}, {(vp)}, {-vallot}, {vallot}, {v,}, {wv,}, {cv,}, {v>}. 
	c tvm,">v",X2v,,,hidden
//;
//; pid@   	( -- n )			(p)	linux
//;	own process id
	c pidf,"pid@",Xpidf,,,linux
//;
//; real-user	( -- )				(p)(s)	root
//;	un-recoverably, revert from effective to real user id, particularly, 
//;	to abandoning root access rights while executing setuid root programs.
	c realu,"real-user",Xrealu,,,root
//;
//; (s)		( a -- n|er )( -1 -- -n )	(p)	linux
//;	fetch syscall-no. n by byte-counted name at a.
//;	displays all available names to stdout w. a = -1, ret -ve no. of sycalls.
	c psysp,"(s)",Xpsysp,,,linux
//;
//; ssc		( a n -- a er )			(p)(s)	linux
//;	executes Linux system call by number n. - re {sys}.
	c ssc,"ssc",Xssc,,,linux
//;
//; sys		ccc( a -- a er )		(i)(p)(s) linux
//;	executes OS specific system call by name ccc, ret value/error code er;
//;	returned figures unsigned greater than hex fffff000 are error codes.
//;	syscall parameters from table at a, a pointing to #last# item in list
//;	thus, address a may directly be taken from "sp@".
//;		ebx,		top, where addr a points to (1st 'C' function arg).
//;		ecx, edx, esi, edi
//;		ebp,		at lo addr	(used w. 2.4.+ linux kernels)
//;	For this simplified syscalls access, at least, 5 items towards lower 
//;	addresses and cell at addr a should be in memory accessible for reading.
//;	F4 access pattern, all reg values but TS (eax) & sp@ optional:
//;		( edi esi edx ecx ebx sp@ eax1 -- edi esi edx ecx ebx sp@ eax2 )
//;	where eax1 was passed by 'sys' and, eax2 is result, replacing the syscall 
//;	number from TS, on entry. e.g, reading a directory entry:
//;		{ 200 pad fd sp@ sys getdents }		\ fd by a channel's 'ch-id'
//;	wait for 10s:
//;		{ sp! 10. sp@ 4- 00. sp@ 4- 3 pick sp@ sys nanosleep . }
//;	send a signal:
//;		33 sp@ sys geteuid swap drop sp@ sys kill
//;	display current working directory:
//;		{ 1K pad sp@ sys getcwd sdrop type drop }
//;	mmap a file, after "open" and channel in constant "ch" (re 'mmapf'):
//;		{ pad 0 over ! 4+ ch flen over !	\ start, size 
//;		{ 4+ 7 over ! 4+ 2 over !		\ r/w/x, 'private'
//;		{ 4+ ch ch-id over ! 4+ 0 over !	\ fd, offset
//;		{ drop pad sp@ sys mmap			\ ernum @tos
//;	Linux system calls do not modify any register other than TS = @tos and, 
//;	won't read any in excess of those required. Thus, no parm on stack, 
//;	whether belonging to the syscall or not, will be modified. F4A dstack 
//;	by default is set up such that reading 'above' @TOS would not result to 
//;	an error if less than 8 cells, thus providing a sufficient safety margin 
//;	for any system call. re manual pages section 2 and, for instance:
//;		"http://www.lxhp.in-berlin.de/lhpsyscal.html", syscalls doc
	c sys,"sys",Xsys,imed,,linux
//;
//; ch-map 	( ch -- n a|0 )		(p)(s)	linux
//;	ret memory range size n and, address a or zero, of mmap-ed channel ch.
//;	any value er unsigned greater than hex fffff000 (-4096) is an error code.
	c map,"ch-map",Xmap,,,linux
//;
//; msync  	( ch -- er )			(p)(s)	linux
//;	immediately update mmap-ed file by ch from memory, flushing the caches.
	c msync,"msync",Xmsync,,,linux
//;
//; vremap 	( a n -- a'|er )		(p)(s)	hidden
//;	mremap an F4 memory range: address a stores the pointer to the rsp memory 
//;	block beginning, it's size expected at address { a 4+ }. applicable to the 
//;	kernel defined ranges of
//;		(tb)	terminal input buffer
//;		(pb)	parameter stack	- adjust s0 by previous { s0 @ (pb) @ - }
//;		 u0	user vari	- re/adjust from/to page (4K) bounds
//;		(ib)	block-file buffer space
//;		(vb)	variable's data block
	c vremap,"vremap",Xvremap,,,hidden
//;
//; mmapa  	( n -- n1 a|er )		(p)(s)	linux
//;	get anonymously mapped memory of size n, 
//;	ret actually allocated, page-aligned size n1, ernum er or assigned addr a;
//;	any value unsigned greater than hex fffff000 (-4096) is an error code.
//;	simplified call which implies r/w/- access rights and 'private' mmap-ing.
	c mmap,"mmapa",Xmmapa,,,linux
//;
//; mremap 	( a2 a1 u1 u2 -- a3|er )	(p)(s)	linux
//;	re-map memory mapped range at a1 w. size u1 to address a2 ( = a1 for pre 
//;	2.4 Linux kernels) w. new size u2, ret error code er or, address a3.
//;	any return value unsigned greater than hex fffff000 is an error code.
//;	simpilfied syscall which implies the 'MREMAP_MAYMOVE' flag and thus will 
//;	always succeed if sufficient memory space available but, may return the
//;	base address a3 different to a1.
//;	memory content will be preserved, within the new size.
	c mremap,"mremap",Xmremap,,,linux
//;
//; munmap  	( n a -- er )			(p)(s)	linux
//;	discard by any mode mapped memory of (partial) size n at base addr a;
//;	n may be any figure within actual size of the rsp. memory block, which by 
//;	the system call internally will be page-aligned to next page-size multiple.
//;	any value er unsigned greater than hex fffff000 (-4096) is an error code.
	c munmap,"munmap",Xmunmapa,,,linux
//;
//; -execute	( cfa -- xx )			(p)	hidden
//;	execute forth word by cfa if not unsigned smaller than cfa of {cold}.
	c dexec,"-execute",Xdexec,,,hidden
//;
//; execute	( cfa -- xx )			(p)
//;	execute forth word by address of it's code-field.
	c execute,"execute",Xexecute
//;
//; base@  	( -- n )			(p)	hidden
//;	base @, plus check for safe value, set to decimal if out of range [2,256].
	c basef,"base@",Xbasef,,,hidden
//;
//; source   	( -- a n )			(p)	hidden
//;	ret address and max-count of current input source.
	c source,"source",Xsource,,,hidden
//;
//; blk@   	( -- n )			(p)	hidden
//;	blk @ 
	c blkfetch,"blk@",Xblkfetch,,,hidden
//;
//; state@ 	( -- n )			(p)	hidden
//;	state @
    c statefetch,"state@",Xstatef,,,hidden
//;
//; 0in!   	( -- n )			(p)	hidden
//;	0 in !
	c zeroinstore,"0in!",Xztoin,,,hidden
//;
//;
//; ---- word header pointers conversion ----
//;

//;
//; v2>pfa  	( a -- a' )			(p)	hidden
//;	convert pfa a of a vocabulary to pfa of the rsp. 2nd-ary search voc.
	c v2pfa,"v2>pfa",Xv2pfa,,,hidden
//;
//; v<nfa  	( n1 -- n2 )			(p)	hidden
//;	From voc-nfa n1 at vocabulary defn block find the corresponding nfa n2 
//;	in the word header. linkage: { ' vocname dup @ >v v<nfa = } is true.
//;	re {v2>pfa} for pfa of the rsp. 2nd-ary search order voc.
	c vocnfa,"v<nfa",Xvocnfa,,,hidden
//;
//; t<nfa  	( n -- n1 n2 )			(p)	hidden
//;	From any value n find n1 := lowest nfa >/ n, n2 := highest nfa < n.
//;	displaying all words in sequence of dictionary, top down:
//;	{ last @ begin dup id. t<nfa swap drop dup 0= until drop }
	c topnfa,"t<nfa",Xtopnfa,,,hidden
//;
//; n<nfa  	( a1 -- a2 )			(p)	hidden
//;	convert nfa a1 to preceding nfa a2; re {t<nfa} for nearest and next nfa.
//;	per vocabulary chain ends at base name hex 81,20. - re '+@'.
	c nnfa,"n<nfa",Xnnfa,,,hidden
//;
//; c>pfa  	( cfa -- pfa )			(p)	hidden
//;	convert cfa to corresponding pfa.
	c cpfa,"c>pfa",Xp4,,,hidden
//;
//; l>pfa  	( lfa -- pfa )			(p)	hidden
//;	convert lfa to corresponding pfa.
	c lpfa,"l>pfa",Xp8,,,hidden
//;
//; >body  	( pfa -- a|0 )			(p)	hidden
//;	convert pfa to data memory address a, 
//;	returns zero if the rsp. cell would point beyond top of used vmem range.
//;	applicable to {constant}, {variable}, {2constant}, {2variable}, {defer}, 
//;	{vocabulary} and can be used w. hi-level definitions if the rsp defining 
//;	words were built w. { .. <builds (vp) @ , .. does> .. }, i.e. disp into 
//;	vmem at pfa which, is the 1st compiled cell in dictionary. substitutes:
//;		{ dup 4- @ [dodoes] = if 4+ endif @ >v }.
	c body2,">body",X2body,,,hidden
//;
//; pfa+	( pfa|nfa -- pfa' )		(p)	hidden
//;	either, convert nfa to pfa or, adjust pfa by <dodoes> displacement. for
//;	conversion which, takes the single cell displacement into account which 
//;	is introduced for the 2nd-ary cfa by hi-level defining words w. {does>}.
	c pfap,"pfa+",Xpfap,,,hidden
//;
//; pfa		( nfa -- pfa )			(p)
//;	convert nfa to pfa; cons disp after cfa, regardless of definition type.
//;	re {pfa+} for self-adjusting conversion wrt words by { <builds..does> }
	c pfa,"pfa",Xpfa
//;
//; lfa    	( pfa -- lfa )			(p)
//;	convert pfa to lfa; re {l>pfa} reverse function.
//;	pfa may either be the standard ptr or, by one cell displaced data-field
//;	of a word built w. { .. <builds .. does> .. }; re {pfa}, {nfa}, {cfa}.
	c lfax,"lfa",Xlfa
//;
//; cfa		( pfa -- cfa )			(p)
//;	convert pfa to cfa; re {c>pfa} reverse function.
//;	pfa may either be the standard ptr or, by one cell displaced data-field
//;	of a word built w. { .. <builds .. does> .. }; re {pfa}, {lfa}, {nfa}.
	c cfa,"cfa",Xcfa
//;
//; nfa    	( pfa -- nfa )			(p)
//;	convert pfa to nfa.; re {n<nfa} for preceding, {v<nfa> vocabulary's nfa.
//;	pfa may either be the standard ptr or, by one cell displaced data-field
//;	of a word built w. { .. <builds .. does> .. }; re {pfa+}, {lfa}, {cfa}.
	c nfa,"nfa",Xnfa
//;
//;
//; ---- defining ----
//;

//;
//; vocabulary	( -- )				(i)(p)
//;	define new empty wordlist, initial 2nd-ary search voc set to defining voc.
//;	set 2nd-ary voc w. {is}:	{ ' 2ndary-vocname is vocabulary-name }
//;	de-activate 2nd-ary voc:	{ ' 0 is vocabulary-name }.
//;	The search order setting is safe wrt. recursion. Initially:
//;		root	=>	forth	  by	' forth is root
//;		forth	=>	editor		' editor is forth
//;		editor	=>	hidden		' hidden is editor
//;		hidden	=>	linux		' linux  to hidden
//;		linux	=>	assembler	' assembler to linux
//;		assembler =>	forth		' forth to assembler
//;		local	=>	0		' 0 to local
//;	{editor} should remain 2nd to {forth} because of ambiguous {editor} words.
//;	vs fig-Forth different structure for Linux, top link to new words can't be 
//;	stored to initial vocabulary headers because of non-writable program memory,
//;	<dovoc> at vocabulary cfa, ptr wrt vmem to voc-nfa in data-block follows:
//;		vmem:
//;	defn-block	-4	fill-byte (-1) to dword aligning voc-bot.
//;	voc-nfa		-3 nfa	(vnam),-1; 4-aligned by 0xff byte at vocnfa-1.
//;	voc bottom	00 lfa	top-nfa, final in the rsp. voc's lfa->nfa chain.
//;	voc-link	04 cfa	pc-rel disp to prev. voc-link or, 0 after {+@}.
//;	2nd-ary context 08 pfa	disp wrt vmem to 2nd-ary search voc lfa or, zero.
//;				2nd-ary voc-top ptr { ' vocname >body pfa >body }
//;	flags		12 wa1	'searched'-flag by find words, other bits reserved.
//;	back-link	16 wa2	address of vocabulary word-header lfa.
//;		dictionary:
//;	disp wrt vmem	   pfa	standard word-header pfa, disp to vmem voc-head.
//;		ff	   wa1	fill byte
//;		81,20,ff	dictionary-dummy nfa
//;			-8 wa2	ptr to word-header pfa, w. disp to vmem voc-head
//;			-7 wa3	dictionary-dummy lfa, ptr to own nfa
//;	circular linkage: last nfa is dummy voc-head, the ca of which points to 
//;	vocabulary's pfa, which points to vmem voc-header by disp wrt vmem base.
//;	voc-link stores pc-rel disp to latest vocabulary's voc-block cfa;
//;	initial voc-top in vmem header set to dictionary-dummy nfa.
//;	context/current store the rsp. voc-block lfa:
//;		{ ' vocname dup lfa swap >body pfa 8+ @ = } returns true flag.
//;	memory:        [ff[(vnam),-1][voctop][voclink][2nd-voc][flags][backlk]]
//;	vhead:		  nfa        lfa     cfa      pfa
//;	in writable data space, nfa pointed to by rsp. vocabulary word-header pfa.
	c vocabulary,"vocabulary",Xvocabulary
//;
//; wordlist	( -- )(X: -- a )		(i)(p)	hidden
//;	define new, empty wordlist, linked into {voc-link} chain. returns it's 
//;	word-list identifier address, at voclink-position of a vocabulary defn-block 
//;	which, might explicitely be stored to {context}, {current} and thus can serve 
//;	as a {vocabulary} or, may be used to implementing an independent, by {-find} 
//;	&c searchable list of forth headers (provisional for 'struct'ures &c).
	c wordl,"wordlist",Xwordlist,,,hidden
//;
//; =:		ccc( x -- )			(i)(x)(p)
//;	synonym {to} for a {constant}, initiates a new constant if ccc not found.
//;	F4A: optionally in kernel if asm-cons _to_ =/= 0.
_to	c toco,"=:",Xtocons,imed	/*"*/
//;
//; +to		ccc( x -- )			(i)(p)
//;	add sing/double item x into memory by name ccc, applicable to {constant}, 
//;	{variable}, {cons+}, {vcons+}, {ucons}, {user}, {2variable}, {2constant}.
//;	{+to} can be extended w. hi-/lo-level words by deferred -re- {[+to]}. 
	c pto,"+to",Xpto,imed
//;
//; to		ccc( x -- )			(i)(p)
//;	synonym {is} - re {is}, {defer}.
//;	{to} can be extended w. hi-/lo-level words by deferred -re- {[is]}. 
	c to,"to",Xis,imed
//;
//; is<		ccc( -- )			(i)(p)
//;	revert to compiled dft procedure of a {defer}red word &c - re {is}.
//;	also applicable to single {constant}s and {variable}s where it stores 
//;	the cell content after the rsp. word's pfa to it's data field.
	c islt,"is<",Xislt,imed
//;
//; is		ccc( x -- )			(i)(p)
//;	re-define by either memory reference deferred word ccc to xec a word by pfa x.
//;	if applied to {variable} or, {2constant}, {2variable} and kernel or standard, 
//;	per byte or cell content/disp, {constant} or {user} definitions, stores the 
//;	unmodified item x to the rsp data-field, thus re-defining their values. x may
//;	be a pfa, sing or, double item, sized according to type of ccc. if applied to
//;	a {vocabulary} stores the 2nd-ary search voc or, returns 'addressing error' 
//;	if either name is not a voc or x =/= pfa of {0}. rst default opr with {is<}.
//;	{is} can be extended w. hi-/lo-level words by deferred -re- {[is]}. 
	c is,"is",Xis,imed
//;
//; defer  	ccc( pfa -- )			(p)
//;	define a named execution vector, initially and by dft executing the word 
//;	the pfa of which was passed @tos. dft action stored @pfa+4. automatically 
//;	reverts to dft if vec. not in range of used dictionary. re-vectoring 
//;	with { ' new is deferred-word }, resetting to dft w. { is< deferred-word }.
//;	A zero vector is in-active, w.o. error, de-activate e.g, w.
//;		{ ' 0 is deferred-word }
	c defer,"defer",Xdefer
//;
//; udefer  	ccc( pfa n -- )			(p)
//;	define a named execution vector in uvari memory, initiated to a word by pfa.
//;	re-/define w. {is}, store dft action w. { ' name cfa ' udeferred 4+ ! }.
//;	NOTE: programmers responsibility to updating @(up), accordingly.
	c udefer,"udefer",Xudefer
//;
//; 2variable	ccc( d -- )(xec: -- adr )	(p)
//;	creating the word ccc with its data field in vmem initialized to d.
//;	ccc returns the storage address to data-stack, for "2@" and "2!" &c.
//;	can be altered w. 'is' or 'to' or, by disp to @(vb) at pfa.
	c twovari,"2variable",X2vari
//;
//; 2constant	(def: d -- )(xec: -- d )	(p)
//;	define a double constant (self fetching variable)
//;	can be altered w. 'is' or, storeing by disp wrt @(vb) to parameter field.
	c cons2,"2constant",X2cons
//;
//; ucons  	(def: ix -- )(xec: -- n )	(p)
//;	uninitiated, self-fetching user variable, re 'user', set and modify with 'is'.
//;	while defining uses ix as the index into user area; xec returns stored sing n.
//;	example, returning the address of the array of environmental strings:
//;		{ (argp) (up) - 4+ ucons envp }
//;	NOTE: programmers responsibility to updating @(up), accordingly.
	c ucons,"ucons",Xucons
//;
//; user   	ccc( n -- )			(p)
//;	defining a user variable:
//;		n USER ccc
//;	The parameter field of ccc contains fixed offset n wrt the reference address 
//;	into user variables' memory, by uvari {(up)}, for this user variable. executing 
//;	a {user}-variable returns the forth address of the rsp. memory location; it's 
//;	content can be altered w. 'is' or 'to'.
//;	NOTE: it is in programmers responsibility to updateing @(up), accordingly.
	c user,"user",Xuser
//;
//; vcons*  	(def: n -- )(xec: n1 -- n2 )	(p)
//;	alterable sing constant which multiplies itself w. @tos. re {is}.
	c vconsm,"vcons*",Xvconsm
//;
//; cons*  	(def: n -- )(xec: n1 -- n2 )	(p)
//;	sing constant in non-alterable memory which multiplies itself w. @tos item. 
	c consm,"cons*",Xconsm
//;
//; vcons+  	(def: n -- )(xec: n1 -- n2 )	(p)
//;	alterable sing constant which adds itself to @tos. re {is}.
	c vconsp,"vcons+",Xvconsp
//;
//; cons+  	(def: n -- )(xec: n1 -- n2 )	(p)
//;	sing constant in non-alterable memory which adds itself to @tos item. 
	c consp,"cons+",Xconsp
//;
//; constant	(def: n -- )(xec: -- n )	(p)
//;	sing constant (self fetching variable) which can be altered w. -re- {is}.
	c constant,"constant",Xvcons
//;
//; variable	ccc( n -- )(xec: -- adr )	(p)
//;	defining a "variable": { n VARIABLE ccc }, may also be altered w. "is".
//;	On execution, ccc returns its memory field address, for {@} and {!} &c.
//;	can be altered w. 'is' or 'to' or, by disp to @(vb) at pfa.
	c vari,"variable",Xvari
//;
//;
//; ---- compiling ----
//;

//;
//; next,  	( -- )				(p)	assembler
//;	cpl jmp to 'next' entry, code <jmp ebx>, hex FFE3.
	c nextcomma,"next,",Xnextcomma,,,assembler
//;
//; |loc   	( n -- )			(p)	hidden
//;	allocates and clears +n or de-allocates -n cells of local data-space,
//;	updates ucons {l0} to @s0 prior to de/allocation; adjusts {s0}, {csp}.
//;	could extend and relocate the mmap-ed locals and parameter stack memory 
//;	block, thus absolute ref in range of min(@s0,l0) till sp@ might become 
//;	invalid. - re {l!0}, {l@0} and {l!n}, {l@n} defining words in <f4x.scr>.
	c loc2,"|loc",X2loc,,,hidden
//;
//; vhere   	( -- n )			(p)	hidden
//;	1st free address in variables' memory.
//;	var-mem won't overflow if in range of by 'mmap' allocatable memory.
//;	re {(vb)}, {(vp)}, {-vallot}, {vallot}, {v,}, {wv,}, {cv,}.
//;	NOTE: reference may be invalid after vmem allocation! re {v>} {>v}.
	c vhere,"vhere",Xvhere,,,hidden
//;
//; v,		( n -- )			(p)	hidden
//;	'allot' one cell in variables' memory, store n into the reserved space. 
//;	might allocate additional vari space from host system.
//;	re {(vb)} {(vp)} {-vallot} {vallot} {vhere} {v,} {wv,} {cv,} {v>} {>v}.
	c vcomma,"v,",Xvcomma,,,hidden
//;
//; wv,		( n -- )			(p)	hidden
//;	'allot' two bytes in variables' memory and store l.s. 16bit of n to the 
//;	reserved place. due to alocation might re-locate vmem - re {vallot}.
	c wvcomma,"wv,",Xwvcomma,,,hidden
//;
//; bv,		( n -- )			(p)	hidden
//;	allocate required space in variables' memory and store l.s. non-zero 
//;	bytes of multi-byte character n to the reserved place. - re {vallot}.
	c bvcomma,"bv,",Xbvcomma,,,hidden
//;
//; cv,		( n -- )			(p)	hidden
//;	allocate one address unit (byte) in variables' memory and store the 
//;	l.s. byte of n to the reserved place.
//;	might allocate additional vmem space from host system - re {vallot}.
	c cvcomma,"cv,",Xcvcomma,,,hidden
//;
//; -vallot	( n -- )			(p)(s)	hidden
//;	extend or de-allocate vari memory by sufficient amount to providing +n 
//;	bytes of free space plus safety margin (1K+) or, to releasing -n bytes.
//;	allocation test might unpredictably change vmem base address @(vb).
//;	re {(vb)} {(vp)} {-vallot} {vhere} {v,} {wv,} {cv,} {v>} {>v}.
	c qvalc,"-vallot",Xqvalc,,,hidden
//;
//; vallot 	( n -- )			(p)(s)	hidden
//;	de-/allocate -/+ +ve page-aligned no. n bytes from/to variables memory, 
//;	extend mmap-ed block if less than a safety margin (1K) would remain or, 
//;	de-allocate if more than an allocation unit (4K) + safety margin left.
//;	NOTE: allocation might unpredictably change the vmem base address @(vb).
//;	re {(vb)} {(vp)} {-vallot} {vhere} {v,} {wv,} {cv,} {v>} {>v}.
	c valc,"vallot",Xvalc,,,hidden
//;
//; ,		( n -- )			(p)
//;	'allot' one cell's space (F4: four 8bit-byte) in dictionary and store n 
//;	into the thus reserved - compile to vari-mem w. 'v,'.
//;	might allocate additional dictionary space from host system - re 'allot'.
	c comma,",",Xcomma
//;
//; c,		( n -- )			(p)
//;	'allot' one address unit (F4: 8bit byte) in dictionary and store the 
//;	l.s.byte of n to the thus reserved place - compile to vari-mem w. 'cv,'.
//;	might allocate additional dictionary space from host system - re 'allot'.
	c ccomma,"c,",Xccomma
//;
//; allot  	( n -- )			(p)
//;	Add the signed number n to the dictionary pointer DP. 
//;	Used (by all compiling words) to allocate or re-origin dictionary space.
//;	dictionary of an F4 saved binary is not enabled for writing access, use 
//;	'vallot', 'v,', 'cv,' instead, for data which schould remain modifiable.
//;	F4: Extends the dictionary space by ds_brk (8K) bytes from host system
//;	memory if less than ds_min(1K) would remain free after execution of ALLOT.
//;	ds_brk, ds_min set by asm-cons, as #define-d in F4 asm source.
//;	Display no. of currently available memory bytes e.g: { (db) 4+ @ pad - . }.
	c allot,"allot",Xallot
//;
//; -allot  	( n -- )			(p)
//;	Extends, but does not allocate, the dictionary space by ds_brk (asm cons, 
//;	8K) bytes from host system memory if less than ds_min(1K) would remain 
//;	free after execution of { n ALLOT }.
	c qallot,"-allot",Xqallot
//;
//; [		( -- )				(i)(p)
//;	Set interpreting mode; re "]", "STATE".
//;	Suspend compilation. The words after [ are executed, not compiled.
//;	This allows calculation or compilation exceptions before resuming
//;	compilation with ], can be used in a colon-definition:
//;		: name ... [  words   ] ... ;
	c brac,"[",Xbrac,imed
//;
//; ]		( -- )				(p)
//;	Enter compiling state; re "[", "STATE".
	c carb,"]",Xcarb
//;
//; ;code  	( -- )				(i)
//;	Continues a colon definition at primitive level:
//;		: ccc ... defw ... ;CODE ... lo-level code ... NEXT,
//;	compiles {(;CODE)}, terminates compiling state, word ccc set {-find}able.
//;	lo-level code should follow, terminated w. a jump to <next>, re 'NEXT,'.
//;	The introducing part defines the set-up procedure of a new cathegory of
//;	words, the code after by ';CODE' compiled runtime word '(;CODE)' defines
//;	the runtime behaviour of words defined w. the just created defining word.
//;	some defining word 'defw', other than 'CREATE', should be present in ccc 
//;	prior to ';CODE'. When ccc later executes:
//;		ccc nnn
//;	the word nnn will be created with its execution procedure given by the 
//;	processor code in the ';code' part of ccc. I.e, when nnn is executed, 
//;	execution procedes immeditely after the cpld ';CODE' runtime word:
//;		: xx dup + variable ;code hex ab c, 8b c, 2 c, next,
//;	a doubling 'CONSTANT'-like definition,
//;		{ 1234 xx yy yy . } displays "2468"
//;	or, aequivalenty,
//;		: xx <builds 2* latest pfa ! ;code hex ab c, 8b c, 2 c, next,
//;	F4 specific, after ';code' compiles: <push-dstack; mov TOS,(W); jmp NEXT>
	cnfa semicode
	.ascii ";code"
	clfa semicode,Xsemicode
//;
//; compile	ccc( -- )			(p)(c)
//;	allocates the rsp. space  and stores (i.e. "compiles") the cfa of the 
//;	following word ccc next into dictionary, for later execution.
	c compile,"compile",Xcompile
//;
//; head   	( a -- )			(p)	hidden
//;	builds a primitive word header, re {create}.
//;	name by supplied string at a, no message if same name multiply defined.
//;	the procedure stores (re-locates) the input name string to dictionary at 
//;	{here}+, adds flag bits and trailing -ve count byte, link- and codefield, 
//;	arranged to the lowest address at which lfa begins at a multiple of 4.
	c head,"head",Xhead,,,hidden
//;
//; create 	ccc( -- )			(p)
//;	builds a forth primitive word header, marked not findable, "smudged":
//;	[flg+count.b name -count.b][ -ve link ][ xecptr(pc) ][ data...
//;	[nfa                       [lfa        [cfa          [pfa...
//;	nfa adjusted such that lfa at lowest possible 4-aligned address.
//;	shortest header is a single char name and, lfa + cfa, i.e. 12 bytes.
//;	Any compiling words might allocate additional dictionary space from host 
//;	system - re {allot} or, {vallot} for writable data memory.
//;	re {does>@}, {does>v}, {<builds}, {does>}
	c create,"create",Xcreate
//;
//; <builds	( -- )				(p)
//;	Introducing a hi-level defining word, in a colon defn:
//;		: defname ... <BUILDS ...  DOES> ... ;
//;	When cccc is executed, <BUILDS defines a named, new word with a 
//;	high-level execution procedure, as set up after DOES>.
//;             ( ..param.. ) defname newname
//;	creates a dictionary entry 'newname' and executes the hi-level words 
//;	of "defname" until DOES>. The newly created word "newname" executes 
//;	the words in the DOES> part of the rsp. defining word, "defname"
//;	which receives the pfa of "defname" on stack.
//;	for reference into variables memory block, words by { <builds...does> } 
//;	should compile the current data displacement figure into the new words' 
//;	code body before using {vallot} or {v,}, {cv,}:
//;		{ ... <builds (vp) @ , ... does> ... @ >v ... ; }
//;	'<builds' compiles a constant the data cell of which stores the entry  
//;	into the defining word's DOES> part, at the new word's pfa.
//;	re {does>@}, {does>v}, {does>}, {create}
	c lessbuilds,"<builds",Xlessbuilds
//;
//; does>  	( -- a )(C: -- )		(p)
//;	defines the run-time part of a high-level defining word. 
//;	While creating a new word with a <BUILDS...DOES> defining word, DOES> 
//;	compiles the <dodoes> code wa into the {latest} words' code-field and 
//;	the pointer to the defining words' hi-level run-time code after DOES> 
//;	into it's pfa. Other words created with the new defining word will then 
//;	set up the - by one cell displaced - parameter field as defined after 
//;	<BUILDS and, execute the words after the defining word's DOES>.
//;		; name <BUILDS ..(defining).. DOES> ..(run-time).. ;
//:		...(parameters)... name newname
//;	constitutes a new word "newname", overwrites it's cfa with <dodoes>, 
//;	compiles the hi-level run-time entry into it's <pfa> and, executes the 
//;	defining words' defining part.
//;	re {does>@}, {does>v}, {<builds}, {create}, {pfa+}, {;}
	c doesgreat,"does>",Xdoesgreat
//;
//; does>@  	( -- a )(C: -- )		(p)
//;	combined { does> @ }, for constants &c
	c fdoesg,"does>@",Xfdoesgreat
//;
//; does>v  	( -- a )(C: -- )		(p)
//;	combined { does> >body }, for cpl'd references into vmem
	c vdoesg,"does>v",Xvdoesgreat
//;
//; :		ccc( -- )			(p)(x)
//;	Beginning a 'colon-definition'
//;		: ccc  ... words  ...  ;
//;	Creates a high-level dictionary entry, defining the executeable list ccc 
//;	of the following sequence of Forth words, until termination e.g, w. {;}
//;	or {;code}, from input stream. ":" on entry copies the current (defining)
//;	vocabulary to {context}, where it will remain unless forcedly modified.
//;	compiling words may allocate additional space to the rsp. memory ranges
//;	from host system - re {allot}, {vallot}, {|loc}.
	c colon,":",Xcolon
//;
//; ;		( -- )				(i)(d)
//;	check stack balance, terminate colon definition; re default vec {[;]}.
	cnfa semi,imed
	.byte 59
	clfa semi,domdfer
		.long semivv-D,sem+cfa2pfa
//;
//; -exit   	( flg -- )			(i)(p)
//;	if flg=tf terminate execution of current colon definition or {load}ing.
//;	{ 0= -exit } compiles the single wa of {+;s}, for 'optimization'.
	c dexit,"-exit",Xdexit,imed
//;
//; exit   	( -- )				(i)(p)
//;	terminates execution of current colon definition or block-file 'screen'.
//;	compiles {;s} or, branch into last colon defn or combined code - re {;}.
//;	for conditional execution or {load}ing termination re {-exit}.
	c exit,"exit",Xexit,imed
//;
//; @csp   	( -- )				(p)
//;	restore the stack pointer from uvari {csp}.
	c fcsp,"@csp",Xfcsp
//;
//; !csp   	( -- )				(p)
//;	store the stack pointer to uvari {csp}; part of the compiler security.
	c storecsp,"!csp",Xstorecsp
//;
//; immediate	( -- )				(p)
//;	toggle {latest} word to in either {state} immediately executing.
	c immediate,"immediate",Ximmediate
//;
//; smudge 	( -- )				(p)
//;	toggle the "smudge bit" of the 'latest' or, currently being defined 
//;	word header, rsp., preventing an uncompleted definition from being 
//;	found by dictionary searches, until compiling completed w.o. error.
//;	re {create} which leaves the just defined header in "smudged" state; 
	c smudge,"smudge",Xsmudge
//;
//; unsmudge	( -- )				(p)
//;	set defined {smudge} state of {latest} word to {-find}able.
	c unsmudge,"unsmudge",Xunsmudge
//;
//; [compile]	ccc( -- )			(i)(p)
//;	compiles the next immediate word from input stream, stores cfa to be 
//;	compiled while executing the just defined word if not immediate.
//;	standard useage in a colon-definition:
//;		{ : name ... [COMPILE] FORTH ... ; }
//;	which at execution time would select FORTH as the context vocabulary.
//;	NOTE: F4 specific impl, standard defn applies to immediate words, only.
	c braccompilecarb ,"[compile]", Xcplbrac,imed
//;
//; [_]		ccc( -- )(I: -- a )		(i)(p)
//;	compiles the next immediate word from input stream, stores cfa to be 
//;	compiled while executing the just defined word if not immediate. i.e.
//;	on later execution, immediate words will be executed, others compiled.
//;		{ ... [_] over [_] - [_] if [_] drop ... }
//;	compiles into the word which is currently being defined:
//;		{ ... compile over compile - if compile drop ... }
//;	functionally aequivalent to ANS-4th 'postpone'; re {[compile]}, {compile}.
	c cplbrac,"[_]",Xcplbrac,imed
//;
//; literal	(xec: n -- n ) (cpl: n -- )	(i)(p)
//;	Compile sing n from data-stack as a literal. Execution of the definition 
//;	containing the literal will push it to the stack. A noop while executing.
//;	Two literals immediately after eachother will be compiled as a 'dliteral'.
//;	compiles {[+]} which adds literal to @tos, if {+} follows, immediately.
//;	optimizing if {cmo} non-zero compiles combined, single rsp. wa if either 
//;	opr immediately follows: +, -, *, /, and, or, xor; re {unsigned} modifier.
	c literal,"literal",Xliteral,imed
//;
//; dliteral	(xec: d -- d ) (cpl: d -- )	(i)(p)
//;	Compile double from data-stack as a literal. Execution of the definition 
//;	containing the literal will push it to the stack. A noop while executing.
	c dliteral,"dliteral",Xdliteral,imed
//;
//;
//; ---- interpreting ----
//;

//;
//; (ok)   	( -- )				(d)	hidden
//;	command line prompt, executed by dft 'quit' action;
//;	deferred, dft "[ok]" can be fetched from pfa+4.
//;	redirectable w. 'is' or, e.g. { ' .s cfa ' (ok) @ (vb) @ + ! }
//;	back to dft w. { ' (ok) dup 4+ @ swap @ (vb) @ + ! }.
	c pokq,"(ok)",domdfer,,,hidden
	.long okev-D,bokb+cfa2pfa
//;
//; .cpu   	( -- )				(p)
//;	display forth system signature, "LWR86F".
	c dotcpu,".cpu",Xdotcpu
//;
//; (x)		( -- a )			(p)	hidden
//;	ptr to the deferred <nul> word vector - re 'x'.
//;	refining: { ( new-pfa -- ) cfa (x) >body ! }
	c bxb,"(x)",Xbxb,,,hidden
//;
//; quit   	( -- )				(v)
//;	program interpreter loop, vectored by uvari '(quit)'. by default, 
//;	initially enters {<quit>} and executes program execution command-line 
//;	arguments before entering the Forth input loop.
	c quit,"quit",Xquit
//;
//; interpret	( -- )
//;	The interpreter which, in the {quit} outer Interpreter loop, depending 
//;	on {state} executes or compiles by blanks delimited text from the input 
//;	stream until no more input available. If the {enclose}d text wasn't found 
//;	as a word in the {context} and {current} vocabularies' chain an attempt 
//;	is taken to converting to a number according to the current BASE setting and 
//;	returned as a sing or, double if a decimal fraction maker "." or "," was 
//;	found while converting. Error message and abort if neither, the word was 
//;	found nor the numeric conversion succeded.
//;	re user-deferred {[inter]} by which the intzerpreter may be intercepted.
	c interpret,"interpret",docolon
1:	.long inter
	.long execute
	.long branch
	.long 1b-.
//;
//; \		ccc( -- )			(i)(d)
//;	read and silently discard input stream until end of line. by dft 
//;	deferred to headerless { 0 word } if blk=0 or, next screen-file line start.
//;	can be altered w. 'is' or 'to' or, by disp @pfa wrt @(vb).
	c bsl,"\\" ,domdfer,imed
	.long bslbv-D,bbslb+cfa2pfa
//; #!		ccc( -- )			(d)
//;	xec F4 shell-script, path to F4 binary follows "#!" after a blank space.
//;	dft '[#!]', can be altered w. 'is' or 'to' or, by disp @pfa wrt @(vb).
//;	reset 'scrix' bit (#2) of '(bits)' to returning to interactive mode.
	c hb,"#!",domdfer
	.long hbv-D,bhbb+cfa2pfa
//;
//; .(		ccc( -- )			(i)(p)
//;	type comment from input stream until next ")" or, end of input line,
//;	single input line/block, only, cannot be nested. re {(}, {\}.
	c dotpx,".(",Xdotpx,imed
//;
//; (		ccc( -- )			(i)(p)
//;	read and silently discard input stream until next ')' or end of line code. 
//;	comment cannot be nested, i.e. next ')' or eol terminates any sequence of
//;	opening parentheses '(' or '.('. <eol> is either, the <cr/lf> code or, end
//;	of file or, any 'c/l' bounds of an input source screen-file. re {.(}, {\}.
	c px,"(",Xpx,imed
//;
//; 2nd-voc	ddd vvv( -- | abort-0/3 )	(i)(p)	root
//;	Assign vocabulary vvv as the 2nd-ary searched voc. to vocabulary ddd. 
//;	De-activate 2nd-ary search order w. {0} instead of name vvv. 
//;	abort 'not implemented' if either name not found or ddd = '0', 
//;	'addressing error' if vvv =/= '0' or, either name is not a voc.
	c voc2nd,"2nd-voc",Xvoc2nd,imed,,root
//;
//; [forget]	( a1 a2 -- )			(p)	root
//;	assign deferred forget handler by pfa a1 to any type word by pfa a2; 
//;	multiple handlers may be assigned to a rsp word; re {forget:}, {forget}.
//;	'deferred' type handler because it supplies a uniform execution handler 
//;	w. the rsp. handling word, type independently, primitive or hi-level &c.
	c bfgtb,"[forget]",Xbforgetb,,,root
//;
//; forget:	hhh www( -- )			(i)(p)
//;	assign deferred forget handler hhh to word www;
//;	multiple handlers may be assigned to a rsp word; re {[forget]}, {forget}.
	c fgtco,"forget:",Xforgetco,imed
//;
//; empty  	( -- )				(p)
//;	{forget} all words after "task" or @fence > nfa of "task", rsp.
	c empty,"empty",Xempty
//;
//; aforget	( a -- )			(p)	hidden
//;	forget word with nfa above or equal to a. re {forget}, {sforget}.
	c aforget,"aforget",Xaforget,,,hidden
//;
//; sforget	( a -- )			(p)	hidden
//;	forget word the name of which is counted string a. re {aforget}, {forget}.
	c sforget,"sforget",Xsforget,,,hidden
//;
//; forget 	ccc( -- )			(p)
//;	delete latest defns until word ccc, inclusive. re {aforget}, {sforget}.
//;	executing {forget} also scans a linked list for handlers which define any 
//;	action specific to a certain word to be executed before removal. link base
//;	in uvari {(fl)} which may be zero or, contain the address of the 1st three
//;	cells handler data block, consisting of [link-PC|0][word-nfa][handler-pfa].
//;	linkage by disp wrt. link position to next link, zero at end of chain.
//;	the handler expected as a standard {deferred} word - others will crash!
//;		( a1 a2 -- xx xx )
//;	while executing, a1=owner-word-pfa and a2=deferred-hnd-pfa, passed to the 
//;	handler word - which may modify the parameters but, preserve stack depth.
//;	references passed w. -re- {forget:} by names or, w. {[forget]} by pfa-s.
	c forget,"forget",Xforget
//;
//; (find) 	( a1 a2 -- pfa countb tf | ff ) (p)
//;	Find word by name at a1 in wordlist chain beginning at topnfa a2.
//;	if found, ret <pfa>, count+flags byte <cnt> and true flag, else drop a1 
//;	and ret false flag. The header structure is not fig-forth:
//;		[nfa:count+flags|chars|-ve count][lfa:-ve disp][cfa][pfa...
	c pfindq,"(find)",Xpfindq,,,hidden
//;
//; found?  	ccc(  -- flg )			(i)(p)	hidden
//;	conditional cpl support: immediately, ret tf if word ccc found.
	c foundq,"found?",Xfoundq,imed,,hidden
//;
//; sfind  	( a -- pfa b tf | -- ff (not found) (p) hidden
//;	find word the name of which is counted string a.
	c sfind,"sfind",Xsfind,,,hidden
//;
//; asfind  	( a -- pfa b tf | -- ff (not found) (p)	hidden
//;	find word by name string at a, searching all vocabularies 
//;	and wordlists in sequence of {voc-link} for a matching entry.
//;	F4A: optionally in kernel if asm-cons _af_ =/= 0.
_af	c asfind,"asfind",Xasfind,,,hidden
//;
//; -find  	ccc( -- pfa b tf | -- ff (not found) (p)
//;	Searches {local}, @context and @current chained vocabularies for matching 
//;	entry and always implicitely, the {root} voc last, until the word ccc was 
//;	found. Returns the dictionary entry's pfa, the flags+count byte and true, 
//;	else if ccc not found only the false flag. text ccc remains at {here}.
//;	based on {(find)}; re {found?}, {sfind}, {asfind}, {-afind}, {'}.
	c dashfind,"-find",Xdashfind
//;
//; -afind  	ccc( -- pfa b tf | -- ff (not found) (p) hidden
//;	find word by name 'ccc', searching all vocabularies and
//;	wordlists in sequence of {voc-link} for a matching entry.
//;	F4A: optionally in kernel if asm-cons _af_ =/= 0.
_af	c afind,"-afind",Xafind,,,hidden
//;
//; '		( -- addr | abort )		(i)(p)
//;	Used in the form { ' nnnn }, leaves the parameter field address 
//;	of dictionary word nnnn. As a compiler directive, executes in a 
//;	colon-definition to compile the pfa as a literal. If the word 
//;	is not found after a search of CONTEXT and CURRENT, aborts with 
//;	an appropriate error message. Pronounced "tick".
	cnfa tick,imed
	.byte 39
	clfa tick,Xtick
//;
//; latest 	( -- a )			(p)
//;	name field address ('nfa') of the latest word in {current} vocabulary,
//;	returns the entry from the rsp. vocabulary defn block { current @ @ }.
//;	re {last} which returns the ptr to latest defn wrt. the entire dictionary;
//;	{last @} is equal to {latest} unless, {current} changed after latest defn,
//;	recursion can be compiled, e.g,
//;		{ : name ... [ latest pfa cfa , ] ... ; }
//;		{ : recurse latest pfa cfa , ; immediate }
	c latest,"latest",Xlatest
//;
//; query  	( -- )				(p)
//;	fetch at most 80 (F4: 254, by asm-cons 'tibqui') characters, until <eof>, 
//;	from stdin to terminal input buffer { tib @ }, store zero to {in}.
	c query,"query",Xquery
//;
//; definitions	( -- )				(p)	root
//;	copy @current search voc. to defining voc, @context.
	c defs,"definitions",Xdefinitions,,,root
//;
//; s,		( a -- )			(i)(p)	hidden
//;	allot and, compile <nul>-terminated counted string a to dictionary.
	c scomma,"s,",Xscomma,imed,,hidden
//;
//; stg,   	( c -- )			(i)(p)	hidden
//;	fetch by char c delimited string from input stream,
//;	allot and compile as counted string to 'here'.
	c stglit,"stg,",Xstglit,imed,,hidden
//;
//; ."		ccc( -- )			(i)(p)
//;	compiles the string ccc, delimited by the trailing double quote, '"', 
//;	which at execution will be sent to stdout; may also be used while 
//;	interpreting.
	cnfa dotquote,imed
	.ascii ".\""//"
	clfa dotquote,Xdotquote
//;
//; "		ccc( -- )(X: -- a )		(i)(p)
//;	compiles the string ccc, delimited by the trailing double quote, '"', 
//;	at execution returns it's address, pointing to the count-byte; may also 
//;	be used while interpreting.
	cnfa quote,imed
	.ascii "\""//`"`
	clfa quote,Xquote
//;
//; s"		ccc( -- )(X: -- a u )		(i)(p)
//;	compiles the string ccc, delimited by the trailing double quote, '"', 
//;	at execution returns address of 1st char and count-byte; may also be 
//;	used while interpreting.
	cnfa squote,imed
	.ascii "s\""//`"`
	clfa squote,Xsquote
//;
//; string 	ddd ccc( -- )(X: -- a )		(i)(p)
//;	compiles string ccc, delimited by 1st char of leading char(s sequence) ddd,
//;	at execution returns it's address, pointing to the count-byte; may also be 
//;	used while interpreting.
	c string,"string",Xstring,imed
//;
//; name   	( c -- a )			(p)	hidden
//;	{ bl word here }
	c name,"name",Xname,,,hidden
//;
//; word   	( c -- )			(p)
//;	Read the next text characters from the current input stream until the 
//;	delimiter c is found, storing the character string beginning at the 
//;	dictionary buffer HERE. WORD leaves the character count in the first 
//;	byte, the characters, and ends with two or more blanks. Leading chars c 
//;	are ignored. If BLK is zero text is taken from the terminal input buffer,
//;	otherwise from the disc block stored in BLK. See BLK, IN.
	c word,"word",Xword
//;
//; here   	( -- n )			(p)
//;	1st free address on top of compiled dictionary.
//;	F4: dictionary won't overflow if in range of allocatable '.bss' section.
	c here,"here",Xhere
//;
//; pad		( -- adr )			(p)
//;	temporary workspace, 80 bytes 'above' HERE.
	c pad,"pad",Xpad
//;
//; ascii  	ccc( -- n )			(i)(p)
//;	fetch (code of) 1st char of next word ccc in input stream to dstack.
	c ascii,"ascii",Xascii,imed
//;
//;
//; ---- branches, loops, conditionals ----
//;

//;
//; back   	( a -- )			(p)
//;	Compile the backward branch displacement from HERE to addr a.
	c back,"back",Xback
//;
//; recurse   	( -- )				(p)(i)
//;	Compile call to currently being defined word. recursion at the end of 
//;	a word, outside conditional constructs, will be modified to a branch.
	c recurse,"recurse",Xrecurse,imed
//;
//; =if		( n1 n2 -- | n1 )(C: a 2 -- )	(i)(c)(p)
//;	introduce program flow redirection:
//;	if n1=n2 2drop and continue immediately after '=if', else 
//;	drop n2 and continue after corresponding 'else' or 'endif', rsp.
	c qif,"=if",Xqif,imed
//;
//; if		( n -- )(C: a 2 -- )		(i)(c)(p)
//;	introduce program flow redirection.
//;	if n =/= 0 continue immediately after {if}, otherwise branch to 
//;	after next corresponding {endif} or {else}, whichever comes 1st.
//;	if 'optimizing' enabled, by -1 in uvari "cmo", with conversion of
//;	{0= if}, {0< if}, {0< 0= if} to the rsp. conditional branch code.
	c if,"if",Xif,imed
//;
//; else   	( -- )(C: a 2 -- h 2 )		(i)(c)(p)
//;	introduce alternate execution path after "if".
	c else,"else",Xelse,imed
//;
//; endif   	( -- )(C: a 2 -- )		(i)(c)(p)
//;	terminate redirected execution path after "if"/"else".
	c endif,"endif",Xendif,imed
//;
//; begin   	( -- )(C: -- a 1 )		(i)(c)(p)
//;	enter loop execution path,
//;	[ ENTER ] BEGIN .. [ ENTRY ] .. [ WHILE ] .. REPEAT | UNTIL
//;	[ ENTER ] BEGIN .. [ ENTRY ] .. AGAIN
	c begin,"begin",Xbegin,imed
//;
//; again  	( -- )(C: a 1 -- )		(i)(c)(p)
//;	unconditionally branch back to corresponding "begin".
	c again,"again",Xagain,imed
//;
//; while  	( n -- )(C: a 1 -- a 1 a1 4 )	(i)(c)(p)
//;	if n =/= 0 continue loop execution else, branch to after 
//;	corresponding "repeat" or "until". {0=} and {0<} optimizing as w. {if}.
	c while,"while",Xwhile,imed
//;
//; repeat 	( -- )(C: a 1 a1 4 -- )		(i)(c)(p)
//;	unconditionally branch back to corresponding "begin".
	c repeat,"repeat",Xrepeat,imed
//;
//; until  	( n -- )(C: a 1 [ a1 4 ] -- )	(i)(c)(p)
//;	if n =/= 0 continue after "until" else, branch back to 
//;	corresponding {begin}. {0=} and {0<} optimizing as w. {if}.
	c until,"until",Xuntil,imed
//;
//; enter  	(C: -- a 5 )			(i)(c)(p)
//;	branch forward into BEGIN.. structure to after corresponding ENTRY.
	c enter,"enter",Xenter,imed
//;
//; entry  	(C: a 5 x x -- x x )		(i)(c)(p)
//;	entry point after ENTER in a BEGIN.. loop structure.
	c entry,"entry",Xentry,imed
//;
//; do		(X: n1 n2 -- )			(i)(c)(p)
//;		(C: -- a 3 )
//;	Used in a colon-definition in form:
//;		DO ... [ I | J | LEAVE | -LEAVE ] ... LOOP
//;		DO ... [ I | J | LEAVE | -LEAVE ] ... ( n ) +LOOP
//;	Introduces a sequence with repetitive execution controlled by the loop 
//;	limit n1 and an index with the initial value n2. DO transfers these to
//;	the return-stack. Upon reaching LOOP the index is incremented by one and 
//;	execution loops back to immediately after DO unless the new index equals 
//;	or exceeds the limit n2, in which case the loop parameters are discarded 
//;	and execution resumed, ahead. Both, n1 and n2 are determined at run-time 
//;	and may be the result of other run-time operations. re {i}, {j}, {ni}.
//;	normal execution if n2 > n1, n1 \< n2 catched to executing a single loop.
	c dox,"do",Xdo,imed
//;
//; loop   	(R: ix lim -- ix' lim | -- )(C: a 3 -- ) (i)(c)
//;	Used in a colon-definition,
//;                    DO ... LOOP
//;	LOOP selectively controls branching back to the corresponding DO based on 
//;	the loop index and limit. The index is incremented by one and compared to 
//;	the limit. The LOOP continues until the index equals or exceeds the limit;
//;	On termination, the parameters are discarded and execution continues
//;	after LOOP.
	c loop,"loop",Xloop,imed
//;
//; +loop  	( n -- )(R: ix lim -- ix+n lim | -- )(C: a 3 -- ) (i)(c)
//;	Used in a colon-definition,
//;		DO ... n1 +LOOP
//;	At run-time, +LOOP controls branching back to the corresponding DO based 
//;	on the loop index ix and the loop limit. The signed increment n1 is added 
//;	to the index and the sum compared to the limit. The loop will be repeated 
//;	until the new index is equal to or greater than the limit (n1>0) or until 
//;	the new index is equal to or less than the limit (n1<0). Upon exiting the 
//;	loop, the parameters are discarded and execution continues ahead.
	c plusloop,"+loop",Xplusloop,imed
//;
//; -leave  	( n -- )			(p)
//;	conditionally, if n =/= 0 leave a DO ... LOOP|+LOOP at next loop end.
	c dleave,"-leave",Xdleave
//;
//; leave  	( -- )				(p)
//;	leave (+)LOOP at next iteration.
	c leave,"leave",Xleave
//;
//;
//; ---- error handling ----
//;

//;
//; ??		( n1 n2 -- )			(p)	root
//; 	display screen- and line no, and char posn of where an error occurred by
//;	input parameters from n1 = @in and n2 = @blk as returned w. {error} &c.
	c qq,"??",Xqq,,,root
//;
//; ert		( n -- )			(p)
//; 	{ dup 0< if message quit endif drop }
	c ert,"ert",Xert
//;
//; n.i.   	( -- )				(p)	hidden
//;	message and abort-0, 'word not found' error.
	c nimp,"n.i.",Xnimp,,,hidden
//;
//; ?exec  	( -- )				(p)
//;	message and abort-18 if not in interpreting state
	c questexec,"?exec",Xqexec//<;colon)
//;
//; ?pairs  	( n1 n2 -- )			(p)
//;	message and abort-19 if n1 =/= n2.
	cnfa questpairs,,list
	.ascii "?pairs"
	clfa questpairs,Xqpairs
//;
//; ?error 	( flg n -- | abort )		(p)
//;	message and abort if true flg. re {message} for the message no. encoding.
	cnfa questerror,,list
	.ascii "?error"
	clfa questerror,Xqerror
//;
//; ?comp  	( -- )				(p)
//;	message and abort-17 if not in compiling state
	c questcomp,"?comp",Xqcomp,,list//<;compile,conditionals)
//;
//; ?csp   	( -- )				(p)
//;	message and abort-20 if stack not balanced after colon defn
	c questcsp,"?csp",Xqcsp//<;semis,;code)
//;
//; ?loading 	( -- )				(p)
//;	message and abort-22 if not loading from screen-file.
	c questloading,"?loading",Xqloading//<;'-->'
//;
//; ?stack  	( -- )				(p)
//;	message and abort-1 if stack empty, abort-7 if stack ovf.
//;	checking stack bounds, re-setting to empty stack if 'underflow' or, 
//;	allocating additional dstack space if less than a system defined limit 
//;	would remain - might modify "s0" contents if re-allocation required!
	c queststack,"?stack",Xqstack//<;interpret)
//;
//; ?depth   	( n -- )			(p)
//;	test whether stack depth at least n cells, 
//;	else abort-19, 'conditionals not paired' error.
	c qdep,"?depth",Xqdepth
//;
//; abort  	( .xx. -- )			(p)
//;	Return control to terminal:
//;	Clears the stacks, reverts to 'dr0', decimal base, displays the system 
//;	identification if stdout is a terminal and if not executing as a script, 
//;	exits debugging, discards locals, continues with the '(quit)' vector:
//;	{ -sp dr0 decimal io-cons if .cpu ." F4" endif drop sp! (quit) @ execute }
	c abort,"abort",Xabort
//;
//; error  	( n -- in blk )			(u)
//;	user-deferred, error message and partial re-initialization; re {[error]}.
	c error,"error",doudfer,,,hidden
	udefl(berrbu),berrb+cfa2pfa
//;
//; message	( n -- )			(d)
//;	if "warning" =/= 0, send the text of line n relative to line 0 of scr 4 
//;	from dr0 to stdout; display n if WARNING = 0 or message file in dr0 not 
//;	accessible. deferred, dft '[msg]', standard messages on dr0, scr 4,5:
//;	 0  NOT IMPLEMENTED (kernel text)
//;	 1  EMPTY STACK
//;	 2  DICTIONARY FULL
//;	 3  HAS INCORRECT ADDRESS MODE
//;	 4  ISN'T UNIQUE
//;	 6  DISC RANGE ?
//;	 7  FULL STACK
//;	 8  DISC ERROR !
//;	15  FORTH INTEREST GROUP                                 MAY 1, 1979
//;	17  COMPILATION ONLY, USE IN DEFINITION
//;	18  EXECUTION ONLY
//;	19  CONDITIONALS NOT PAIRED
//;	20  DEFINITION NOT FINISHED
//;	21  IN PROTECTED DICTIONARY
//;	22  USE ONLY WHEN LOADING
//;	23  OFF CURRENT EDITING SCREEN
//;	24  DECLARE VOCABULARY
//;	deferred, can be altered w. 'is' or 'to' or, by disp @pfa wrt @(vb).
    c message,"message",domdfer
	    .long mesgv-D,mesg+cfa2pfa
//;
//;
//; -- user vari --
//;	can be altered w. 'is' or 'to' or, by disp @pfa wrt address of (up).
//; -------------------------------------------------------------------------------
//;
//; umem copied from vmem+168(initial vocs), base of sysvari mem at -200 +origin
//;		( n -- a ) +origin
//; --	ref wrt (u0):+origin|umem(hex)
//;	 -160:-32	fperm	file permission bits.
//;		=>	this item marks the beginning of the user area.

//;
//; fperm  	( -- a )	-160:-32|000		linux
//;	ptr to uvari w. file permission bits, used by "open", "using" (etc).
	c fperm,"fperm",douser,,,linux
	udefl(fpermu)
//;
//;	 -156:-28|004	olink	link to (next) block of user vari, true address
//;	 -152:-24|008	obckl	back-link to (previous) block of user vari, true address
//;	 -148:-20|00c	minl	min. dft output 'window` sizes
//;	 -144:-16|010	argc	command-line args count
//;	 -140:-12|014	argp	ptr to command-line args array
//;
//; (argp) 	( -- a )	-140:-12|020		linux
//;	address pointing to 1st item of array of passed arguments' addresses,
//;	args count at (argp)-4, env-vari ptr at (argp)+4.
	c rgp,"(argp)",douser,,,linux
	udefl(argpu)
//;	 -136:-08|018	envp	ptr to env vari strings array
//;	 -132:-04|01c	sigh	ptr to signal handlers' definition blocks
//;
// -----------------------------------------------------------------------------
//; --	displacements wrt base address '(up)' for byte addressable range [-128,124]
//;	name	( stack ) 	disp(up):disp(+origin)|disp(sysmem)
//;
//;	 -128:000|020  	docold		address of 'cold' re-start entry
//;	 -124:004|024	dowarm		address of 'warm' re-start entry
//;	 -120:008|028 	 -/-		n.a.
//;	 -116:012|02c 	 -/-		n.a.
//;	 -112:016|030	-reserve-
//;
//; s0		( -- a )	-108:020|034
//;	empty data-stack, re {sp}; items pushed towards higher addresses.
//;	also base reference for 'local' data-space, towards lower addr.
	c s0,"s0",dobuser
	udef(s0u)
//;
//; r0		( -- a )	-104:024|038
//;	empty return-stack, as set w. "rp!"
	c r0,"r0",dobuser
	udef(r0u)
//;
//; tib		( -- a )	-100:028|03c
//;	location of current terminal input buffer.
//;	by dft, in mmap-ed memory re "(tb)", size at next uvari.
//;	NOTE: {tib} should not be re-assigned across any {load}ing commands!
	c tib,"tib",dobuser
	udef(tibu)
//;
//; width  	( -- a )	 -96:032|040
//;	count of word names' chars stored in a word header in 
//;	range [1,31]; significant leading part of a name to "-find".
	c width,"width",dobuser
	udef(widthu)
//;
//; warning	( -- a )	 -92:036|044
//;	errors handling mode (dft -1):
//;	< 0	xec user definable hi-level word from vector '(abort)'
//;	  0	display error by number, no data-stack initialization, 'quit'
//;	> 0	message by line wrt scr 4 of dr0 file, if accessible and, 
//;		'quit', else falling back to 'abort'.
	c warning,"warning",dobuser
	udef(warningu)
//;
//; fence  	( -- a )	 -88:040|048
//;	nfa of top word which belongs to against {forget} protected range of 
//;	the dictionary. protecting the entire dictionary: { last @ fence ! }.
	c fence,"fence",dobuser
	udef(fenceu)
//;
//; dp		( -- a )	 -84:044|04c
//;	dictionary pointer, lowest address in available dictionary space.
	c dp,"dp",dobuser
	udef(dpu)
//;
//; voc-link	( -- a )	 -80:048|050
//;	wrt vmem rel address of linkage entry to latest defined vocabulary
//;	{ voc-link +@ } points to 1st data-block pseudo-lfa in the chain of 
//;	linked vocabularies, subsequent {+@} to the next block, until zero.
//;	NOTE: stored adress may change due to vmem allocation; re {v>}, {>v}.
	c vocdashlink,"voc-link",dobuser
	udef(voclu)
//;
//; blk		( -- n )	 -76:052|054
//;	vari, determines whether input source is terminal, script or, screen-file:
//;		> 0	screen-file (block-file)
//;		= 0	terminal or, by {evaluate}
//;		< 0	1-s complement of file ptr initially in an executeable script
//;			(script being mmap-ed, a startup, blk set to 0, re {(bits)})
	c blk,"blk",dobuser
	udef(blku)
//;
//; in		( -- a )	 -72:056|058
//;	ptr to after latest interpreted text input wrt. @tib.
	c in,"in",dobuser
	udef(inu)
//;
//; out		( -- a )	 -68:060|05c
//;	count of characters sent to stdout, by "type" or "emit"
	c out,"out",dobuser
	udef(outu)
//;
//; scr		( -- a )	 -64:064|060
//;	currently used screen-file "screen" (block no / blocks per screen).
	c scr,"scr",dobuser
	udef(scru)
//;
//; offset 	( -- a )	 -60:068|064
//;	vari which determines whether dr0 or dr1 is in use
	c offset,"offset",dobuser
	udef(offsetu)
//;
//; context	( -- a )	 -56:072|068
//;	ptr to top nfa of vocabulary where "-find" begins searching.
//;	NOTE: stored adress may change due to vmem allocation; re {v>}, {>v}.
	c context,"context",dobuser
	udef(contextu)
//;
//; current	( -- a )	 -52:076|06c
//;	ptr to top nfa of currently being extended vocabulary.
//;	NOTE: stored adress may change due to vmem allocation; re {v>}, {>v}.
	c current,"current",dobuser
	udef(currentu)
//;
//; state  	( -- a )	 -48:080|070
//;	vari which determines compiling or interpreting(0) state.
	c state,"state",dobuser
	udef(stateu)
//;
//; base   	( -- a )	 -44:084|074
//;	vari, current number conversion radix
	c base,"base",dobuser
	udef(baseu)
//;
//; dpl		( -- a )	 -40:088|078
//;	least significant decimal point location of latest converted 'number'.
	c dpl,"dpl",dobuser
	udef(dplu)
//;
//; fld		( -- a )	 -36:092|07c
//;	free for application
	c fld,"fld",dobuser
	udef(fldu)
//;
//; csp		( -- a )	 -32:096|080
//;	vari. compiler security, dstack balance control value.
//;	NOTE: unlikely, but, stored adress may change due to dstack allocation.
	c csp,"csp",dobuser
	udef(cspu)
//;
//; r#		( -- a )	 -28:100|084
//;	screenfile editor cursor position wrt top of scr.
	c rsharp,"r#",dobuser
	udef(rsharpu)
//;
//; hld		( -- a )	 -24:104|088
//;	ptr to latest, m.s. char of numeric string conversion
	c hld,"hld",dobuser
	udef(hldu)
//;
//; (ti)   	( -- a )	 -20:112|090		hidden
//;	double, file input time-out figure, re {w4f}, {key};
//;	count in seconds stored at address of (ti), microseconds at { (ti) 4+ }.
	c btimoq,"(ti)",dobuser,,,hidden
	udef(timou)
//;
//; (asci) 	( -- a )	 -12:116|094		hidden
//;	ranges of printing chars (lsb[32,126],[160,255]msb)
//;	used by {key>char}, which is also executed by {expect}, {printing?}.
	c basciq,"(asci)",dobuser,,,hidden
	udef(asciu)
//;	-08:120		<tbbot>		bottom of mmaped terminal input buffer
//;
//; (tb)   	( -- a )	  -8:120|098		hidden
//;	bottom of dft terminal input buffer memory - size in { (tb) 4+ }.
//;	should not be re-assigned across any {load}ing commands! re {input>} &c.
	c btbbotuq,"(tb)",dobuser,,,hidden
	udef(tbbotu)
//;	-04:124:09c	<tbsiz>		size of mmap-ed terminal input buffer
//;
//; (up)   	( -- a )	  00:128|09c		hidden
//;	disp wrt address of (up) points to 1st free entry in user area;
//;	(up) is uvari base ref, { (up) +@ } points to 1st free uvari.
//;	{ (up) u0 @ = } returns 'true' for the initial, sole uvari defn block.
	c bupq	,"(up)",dobuser,,,hidden
	udef(upu)
//;
//; (es)   	( -- a )	  04:132|0a0		hidden
//;	escape char for {e\stg}, dft '\' (backslash).
	c besccq,"(es)",dobuser,,,hidden
	udef(esccu)
//;
//; (db)   	( -- a )	  08:144|0a4		hidden
//;	bottom of dictionary space (in .bss) - next uvari w. disp to top.
	c bdcsizq,"(db)",dobuser,,,hidden
	udef(dcbotu)
//;	 12:136|0a8    	<dcsizu>	.bss (dictionary) disp from <dcbotu> to top
//;
//; (pb)   	( -- a )	  16:144|0ac		hidden
//;	bottom of 'mmap'-ed parameter stack memory - next uvari w. 'mmap'-ed size.
	c bpsbotuq,"(pb)",dobuser,,,hidden
	udef(psbotu)
//;	 20:148|0b0    	<pssizu>	mtop datastack size (hex 4000)
//;	 24:152|0b4    	<ltopu>		top lfa
//;	 28:156|0b8    	<ntopu>		top nfa - "last"
//;
//; last   	( -- a )	  28:156|0b8
//;	uvari w. currently last nfa in dictionary, preceding uvari stores top lfa.
	c last,"last",dobuser	
	udef(lastu)
//;
//; u0		( -- a )	  32:160|0c0
//;	control item, content equal to current UP or, next in chained tasks.
//;	{ (up) u0 @ = } returns 'true' for the initial, sole uvari defn block.
	c u0,"u0",dobuser
	udef(u0u)
//;	 36:164|0c4    	<uvsizu>	uvtop, mmaped uvari block size (hex 1000)
//;	 40:168|0c8    	<iobotu>	tib & blockbuf base address
//;
//; (ib)   	( -- a )	  40:168|0c8		hidden
//;	bottom of 'mmap'-ed block-buffers memory - next uvari w. 'mmap'-ed size.
	c biobotuq,"(ib)",dobuser,,,hidden
	udef(iobotu)
//;	 44:172|0cc    	<iosizu>	blockbuf size (hex 2000)
//;	 48:176|0d0    	<wsizu>		console window sizes
//;	 52:180|0d4    	<vbu>		bottom of mmapped variables' memory
//;
//; (vb)   	( -- a )	  52:180|0d4		hidden
//;	bottom of 'mmap'-ed vari data memory - re {>v}.
	c bvabotuq,"(vb)",dobuser,,,hidden
	udef(vbotu)
//;	 56:184|0d8    	<vsizu>		size of variables' memory
//;
//; (vp)   	( -- a )	  60:188|0dc		hidden
//;	running ptr by disp wrt @(vb) into mapped vari space.
	c bvpq,"(vp)",dobuser,,,hidden
	udef(vpu)
//;
//; (inter)	( -- a )	  64:192|0e0		hidden
//;	[inter] vector in "interpret" immediately after "-find".
	c pinterp,"(inter)",dobuser,,,hidden
	udef(interu)
//;	 68:196|0e4    	<dbbotu>	bottom debugging address, or 0 
//;
//; (gb)   	( -- a )	  68:196|0e4		hidden
//;	bottom bound of debugging address range or, 0.
//;	optional top bound at next uvari, dft -1 for 'here'.
	c pgbq,"(gb)",dobuser,,,hidden
	udef(dbbotu)
//;	 72:200|0e8    	<dbtopu>	top debugging address, or -1
//;	 76:204|0ec   	<dbflgu>	debugging flags & mode
//;
//; (bits) 	( -- a )	  80:208|0f0		hidden
//;	system state flag bits.
	c bbitsq,"(bits)",dobuser,,,hidden
	udef(bitsu)
//;
//; (p)		( -- a )	  84:212|0f4		hidden
//;	 84:212		<pu>		saved 'paper' colour
	c bpq,"(p)",dobuser,,,hidden
	udef(pu)
//; (i)		( -- a )	  88:216|0f8		hidden
//;	 88:216		<iu>		saved 'ink' colour
	c biq,"(i)",dobuser,,,hidden
	udef(iu)
//;
//; cmc    	( -- a )	  92:220|0fc		hidden
//;	compiled words counter, for backward code modification, 
//;	'optimization' enabled if @cmc > 0 and @cmo = -1. re {;}
//;	incremented by {interpret} and, adjusted by compiling words.
	c cmc4,"cmc",dobuser,,,hidden
	udef(cmcu)
//;
//; (emit) 	( -- a )	  96:224|100		hidden
//;	vari, wa of user-deferred {emit} routine.
	c pemitq	,"(emit)",dobuser,,,hidden
	udef(emitu)
//;
//; (type) 	( -- a )	 100:228|104		hidden
//;	vari, wa of user-deferred {type} routine.
	c ptypeq	,"(type)",dobuser,,,hidden
	udef(typeu)
//;
//; (?key) 	( -- a )	 104:232|108		hidden
//;	vari, wa of user-deferred {?key} routine.
	c pqkeyq	,"(?key)",dobuser,,,hidden
	udef(qkeyu)
//;
//; (key) 	( -- a )	 108:236|10c		hidden
//;	vari, wa of user-deferred {key} routine.
	c pkeyq,"(key)",dobuser,,,hidden
	udef(keyu)
//;
//; (expect) 	( -- a )	 112:240|110		hidden
//;	vari, wa of user-deferred {expect} routine.
	c pexpectq,"(expect)",dobuser,,,hidden
	udef(expectu)
//;
//; (abort) 	( -- a )	 116:244|114		hidden
//;	vari, wa of user-deferred {[abort]} routine, xec'd by {error}.
	c babortq,"(abort)",dobuser,,,hidden
	udef(abortu)
//;
//; (atxy) 	( -- a )	 120:248|118		hidden
//;	vari, wa of user-deferred {at-xy} routine.
	c batxyq,"(atxy)",dobuser,,,hidden
	udef(atxyu)
//;
//; (xy) 	( -- a )	 124:252|11c		hidden
//;	vari, wa of user-deferred {xy} routine.
	c bxyq,"(xy)",dobuser,,,hidden
	udef(xyu)
//;
//;	124:252		---	last byte-addressable uvari cell
// -----------------------------------------------------------------------------
//;
//; (maxy) 	( -- a )	 128:256|120		hidden
//;	vari, wa of user-deferred {max-xy} routine.
	c bmaxyq,"(maxy)",douser,,,hidden
	udefl(maxyu)
//;
//; cmo    	( -- a )	 132:260|124		hidden
//;	dis(0)/enable(-1) the 'cmc' words counter (for condition code optimizing).
//;	the content of this vari will be used as the -ve increment, by cmc counter,
//;	thus, no other values should be stored to 'cmo', else expect fatal errors!
	c cmo,"cmo",douser,,,hidden
	udefl(cmou)
//;
//; l0     	( -- a )	 136:264|128	(ucons)	local
//;	'local' memory base addr, by '|loc' prior to allocation or after de-alloc.
	c lp,"l0",doucons,,,local// ; l0
	udefl(l0u)
//;
//; (uo)   	( -- a )	 140:268|13c		hidden
//;	vari which stores the {using} file access open mode.
	c puop,"(uo)",douser,,,hidden
	udefl(puopu)
//;
//; (fl)   	( -- a )	 144:272|130		hidden
//;	pointer to latest forget handler link-field.
	c pflp,"(fl)",douser,,,hidden
	udefl(pflpu)
//;
//; (al)   	( -- a )	 148:276|134		hidden
//;	vari, wa of user-deferred {[alarm]} routine.
	c palp,"(al)",douser,,,hidden
	udefl(palpu)
//;
//; (bbi)  	( -- a )	 152:280|138		hidden
//;	vari, wa of user-deferred {bbi} routine.
	c bbbiq,"(bbi)",douser,,,hidden
	udefl(bbiu)
//;
//; (quit) 	( -- a )	 156:284:13c		hidden
//;	dft <quit> wa. re {(abort)}.
	c bquitq,"(quit)",douser,,,hidden
	udefl(quitu)
//;
//; (col)  	( -- a )	 160:288:140		editor
//; (row)  	( -- a )	 164:292:144		editor
//;	{ xy (col) 2! } stores 'x' to (col) and 'y' to (row).
//;	provisional for f4p, f4e &c: by counted output chars cursor position.
	c pcolp,"(col)",douser,,,editor
	udefl(colu)
	c prowp,"(row)",douser,,,editor
	udefl(rowu)
//;
//; (err) 	( -- a )	 168:296|148		hidden
//;	{error} xec vec, by dft set to {[error]} cfa.
	c perrq,"(err)",dobuser,,,hidden
	udefl(berrbu)
//;
//;
//;	additional hi-level uvari, by dword disp may extend, up to PAGE_SIZE(4K), 
//;	counting from uarea beginning, at <fperm> position = <docold>-32 = u0-48.
//;	a common method to getting the disp for a user variable which returns one 
//;	of the above items, regardless of byte or dword compiled disp to UP, is
//;	the reference to '(up)' the address of which represents zero displacement:
//;		{ (argp) (up) - 4+ ucons envp }
//;
//;	size of uarea may be extended with 'mremap' by -ve page-aligned address 
//;	@u0 and size=@<uvsizu>, at address { u0 4+ } after which the content of 
//;	'up' should be set to the returned address.
//;	to preventing intermediate access to an uvari the base address of which 
//;	was just changed, copy uvari to a newly assigned 'mmapa' block, link to 
//;	that area, copy and, 'munmap' the previously valid block, afterwards.
//;
//;
//; ---- cons & vari ----
//;

//;
//; .sn  	( -- n )			(mcons)
//;	no. of cells to display with {.s}, alterable constant.
	c dotsn,".sn",domconstant
        .long dotsnv-.,16
//;
//; b/scr  	( -- n )			(p)
//;	buffers per screen - by { 1024 b/buf / }.
	c bbyscr,"b/scr",Xbbyscr
//;
//; lword  	( -- a )
//;	string-variable for {l-load}, holds the word to find for the screen
//;	at wich to begin {include}ing. {l-load} stores the supplied name as
//;	' : name ' to lword. the name may be up to 31 chars of size.
	c lword,"lword",doconstant
        .long lldv
//;
//; first  	( -- a )			(mcons)
//;	a := address of the first (lowest) block buffer. 
//;	range FIRST..LIMIT may be changed w. linux mmremap syscall, 
//;	the rsp. pointers, bottom and size in uvari
//;	by ( n -- ) { ' first +@ ! } alterable memory-constant
	c first,"first",domconstant
        .long firstv-.
//;
//; limit  	( -- a )			(mcons)
//;	address above highest memory available for a disc buffer.
//;	range FIRST..LIMIT may be changed w. linux mmremap syscall, 
//;	the rsp. pointers, bottom and size in uvari
//;	by ( n -- ) { ' limit +@ ! } alterable memory-constant
	c limit,"limit",domconstant
        .long limitv-.
//;
//; use		( -- a )
//;	variable containing the address of the block buffer to use next,
//;	as the least recently written.
	c use,"use",doconstant
        .long usev
//;
//; prev   	( -- a )
//;	vari, containing the address of most recently referenced disc buffer.
	c prev,"prev",doconstant
        .long prevv
//;
//; +origin 	( n -- a )			(pcons)
//;	Leave the memory address relative by n to the origin parameter area, 
//;	for access to the boot-up parameters at the origin area.
//;	{ 0 +origin } returns address of 1st uvari dft item, ptr to "cold".
	c plusorigin,"+origin",docplus
        .long origino
//;
//; +adr   	( -- a )			(cons)	hidden
//;	r/o address of a cell the value of which is it's own address; terminal 
//;	absolute link where { dup @ ... @ = } returns 'true' and won't segfault.
	c padr,"+adr",doconstant,,,hidden
	.long .
//;
//; +nul   	( -- a|n )			(cons)	hidden
//;	r/o address of cell the value of which is zero,
//;	for instance, an empty string or, terminal relative link with {+@}
//;	where { dup +@ ... +@ = } returns 'true' and won't segfault. re {+mem}
	c pnul,"+nul",doconstant,,,hidden
	.long nullc
//;
//; c/l		( -- n )			(mcons)
//;	screen-file editor chars per line (64)
//;	by { ( n -- ) ' c/l +@ ! } alterable memory-constant.
//;	changing c/l most certainly requires other adjustments; not tested.
	c cbyl,"c/l",domconstant
        .long cbylv-.
//;
//; b/buf  	( -- n )			(mcons)
//;	bytes per screen-file buffer (64)
//;	by { ( n -- ) ' b/buf +@ ! } alterable memory-constant
//;	changing b/buf most certainly requires other adjustments; not tested.
	c bbybuf,"b/buf",domconstant
        .long bbybufv-.
//;
//; ermax  	( -- c )			(cons)
//;	signed abs greatest, unsigned lowest linux system error code.
//;	test ret values from {linux} words w. { nn ermax u< 0= if ..error.. }.
	c ermax,"ermax",doconstant
        .long maxerr
//;
//; -0		( -- 0x80000000 )		(cons)
//;	constant value of '-ve zero' to @tos (only sign-bit set).
//;	F4A: optionally in kernel if asm-cons _al_ =/= 0.
_al	 c mz,"-0",doconstant
_al 	.long 0x80000000
//;
//; 0.		( -- 0 0 )			(p)
//;	double constant value to @tos
	c dz,"0.",Xdzero
//;
//; 0		( -- 0 )			(p)
//;	constant value to @tos
	c zero,"0",Xzero
//;
//; 1		( -- 1 )			(p)
//;	constant value to @tos. "1" is {true} numeric value.
	c one,"1",Xone
//;
//; -1		( -- -1 )			(bcons)
//;	constant value to @tos
	c mo,"-1",dobconstant
	.byte -1
//;
//; 2		( -- 2 )			(bcons)
//;	constant value to @tos
	c two,"2",dobconstant
        .byte 2
//;
//; 3		( -- 3 )			(bcons)
//;	constant value to @tos
	c three,"3",dobconstant
        .byte 3
//;
//; 4		( -- 4 )			(bcons)
//;	constant value to @tos
	c four,"4",dobconstant
        .byte 4
//;
//; bl		( -- c )			(bcons)
//;	constant, ascii code of the blank character (32).
	c bl,"bl",dobconstant
        .byte 32
//;
//;
//; ---- screen-file i/o ----
//;

//;
//; +buf   	( add1 --- addr2 flg )		(p)
//;	Advance the disc buffer address addr1 to the address of the next buffer 
//;	addr2. flg = 0 when addr2 is the buffer presently pointed to by uvari 
//;	{prev}; default buffer space covers 4 screens w. b/scr (16) buffers, ea.
	c plusbuf,"+buf",Xplusbuf
//;
//; flush  	( -- )				(p)	editor
//;	sto content of 'update'd screen-file block-buffers back to the rsp. source.
	c flush,"flush",Xflush,,,editor
//;
//; update 	( -- )				(p)
//;	Marks the most recently referenced block (pointed to by PREV) as
//;	altered. The block will subsequently be transferred automatically to
//;	disc should its buffer be required for storage of a different block.
	c update,"update",Xupdate
//;
//; empty-buffers  ( -- )			(p)
//;	Mark all block-buffers as empty. Updated blocks are not written to 
//;	the disc. Also for initialization first use of the disc.
	c edbs,"empty-buffers",Xemptybuffers
//;
//; dr!		( f -- )			(p)
//;	set screen-file channel by flag f, 0 for dr0, 1 for dr1.
//;	re {dr?}, {dr0}, {using}, {bchan}, {close} &c
	c drs,"dr!",Xdrs
//;
//; dr?		( -- n )			(p)	hidden
//;	ret which screen-file channel currently in use, 0 for dr0, 1 for dr1.
//;	re {dr!}, {dr0}, {using}, {bchan}, {close} &c
	c drq,"dr?",Xdrq,,,hidden
//;
//; dr0		( -- )				(p)
//;	Select the 1st disc drive for screen-file i/o.
//;	re {dr?}, {dr1}, {using}, {bchan}, {close} &c
	c drzero,"dr0",Xdrzero
//; dr1		( -- )				(p)
//;	Select the 2nd disc drive for screen-file i/o.
//;	re {dr?}, {dr0}, {using}, {bchan}, {close} &c
	c drone,"dr1",Xdrone
//;
//; r/w		( addr blk f -- )		(p)(s)
//;	The standard disc read/write procedure. addr specifies the source 
//;	or destination block buffer, blk is the sequential number of the
//;	referenced block and, f is a flag for f=0 write and f=1 read.
//;	NOTE: to avoiding a nameing conflict the r/w 'open' mode flag is {w/r}.
	c rbyw,"r/w",Xrbyw
//;
//; buffer 	( n -- a )			(p)
//;	obtain next block-file buffer, assigning it to block n. The buffer is 
//;	copied back to the disc if {update}d. Buffered data remain unmodified; 
//;	address a is the first within the buffer, available for data storage.
	c buffer,"buffer",Xbuffer
//;
//; block  	( n -- a|0 )			(p)
//;	memory address a of the block buffer containing block n. If the block is 
//;	not already present, it is transferred from the rsp. block i/o-device as 
//;	previously assigned to {dr0} or {dr1}, w. {using} &c to the next free or 
//;	least recently written buffer; saving the current content if {update}ed.
	c block,"block",Xblock
//;
//; (line) 	( n1 n2 -- addr count )		(p)
//;	Convert line number n1 and screen n2 to the rsp. disc buffer address, 
	c plineq,"(line)",Xplineq
//;
//; .line  	( line scr -- )			(p)
//;	Print to stdout, a line of text from disc by its line and screen number,
//;	w.o. trailing blanks.
	c dotl,".line",Xdotline
//;
//;
//; ---- cpl from screen-files ----
//;

//;
//; if-nfound	ccc( -- )			(i)(p)	root
//;	silently discard input stream until end of line if word ccc found
	c ifnf,"if-nfound",Xifnfound,imed,,root
//;
//; if-found	ccc( -- )			(i)(p)	root
//;	silently discard input stream until end of line if word ccc not found
	c iffd,"if-found",Xiffound,imed,,root
//;
//; if-true	ccc( n -- )			(i)(p)	root
//;	if n=0 read and silently discard input stream until end of line.
	c iftr,"if-true",Xiftrue,imed,,root
//;
//; +continue	( n -- ( jump over n lines )	(i)(p)(l) root
//;	conditional compiling support, ignore the following n lines while 
//;	{load}ing. may span multiple screens. 
//;	-ve count n backwards, beginning at line w. the '+continue' command.
	c pcont,"+continue",Xpluscont,imed,,root
//;
//; using   	( a -- )			(p)
//;	screen-file access by currently set pseudo-'drive', {dr0} or, {dr1}:
//;	flush & close currently used screen-file. open file by fully qualified
//;	name at address a. open access mode from uvari {(uo)}, dft {r/o}.
	c using,"using",Xusing
//;
//; l		(  -- )				(p)	editor
//;	list screen no. from @scr of currently used screenfile.
	c l,"l",Xl,,,editor
//;
//; [list]   	( n -- )			(p)
//;	dft for {list}; n to uvari "scr", text of screen n to stdout.
	c blistb,"[list]",Xlist
//;
//; list   	( n -- )			(d)
//;	Store n to uvari "scr", send the text of screen n to stdout; 'deferred'.
	c list,"list",domdfer
	.long listv-D,blistb+cfa2pfa
//;
//; index  	( from to -- )			(p)
//;	Output the first line of each screen over the range [from,to].
//;	F4A: optionally in kernel if asm-cons _ie_ =/= 0.
_ie	c index,"index",Xindex
//;
//; triad  	( scr -- )			(p)
//;	Send to stdout three screens including scr, beginning w. screen no. 
//;	which is a multiple of three. Output followed by reference line from 
//;	line 15 of screen 4 in dr0 device.
//;	F4A: optionally in kernel if asm-cons _ti_ =/= 0.
_ti	c triad,"triad",Xtriad
//;
//; -wl		ccc( -- flg )			(i)(p)	root
//;	tf if word ccc equals the text stored to {lword}, false flg otherwise.
//;	aequivalent { lword @ if-true -l }; re {-l}, {lword?}, {lload}, {l-load}.
	c wlq,"-wl",Xwlq,imed,,root
//;
//; -l		ccc( -- flg )			(i)(p)	root
//;	tf if word ccc equals the text stored to {lword}, else false flg.
//;	flg =/= 0 also if no 'lword' was stored, @lword = 0, to enabling 
//;	consecutive screen-file loading of the same file, w. {-->} &c. re {-wl}.
	c dlword,"-l",Xdlword,imed,,root
//;
//; lword? 	( a -- flg )			(i)(p)	hidden
//;	tf if counted string at a equals the text stored to {lword}, else false flg.
//;	flg =/= 0 also if no {lword} was stored, i.e. @lword = 0. re {l-load}.
	c lwordq,"lword?",Xlwordq,imed,,hidden
//;
//; l-find 	( a1|0 a2 -- scr a1|ch )	(p)
//;	find pattern " : name ", w. name by address a2, in file by address a1.
//;	a1=0 for currently used or, default screenfile <f4x.scr> if none open.
//;	ret true flg, screen scr of where word found, file name address a1.
//;	returned parameters ready for e.g, { 0 " help" l-find using list } or, 
//;	ret channel no. instead of a1 if the rsp. screen-file is already open.
//;	no additional text entry needed if the rsp. definition begins at a new
//;	line and, the last char of the preceding line is left blank.
	c ldfind,"l-find",Xldfind
//;
//; l-load   	( a1 a2 -- flg )			root
//;	load from screenfile a1 (current if a1=0) the screen which is marked with 
//;	name a2 and preceding colon+blank, followed by a blank, at any line in the 
//;	rsp screen, and which is 1st to show that pattern in the searched file.
//;	flg < 1 is an error, else flg := last encountered scr#, pushed to dstack 
//;	on return from loading.
//;	previous {lword} and i/o description restored upon return.
//;	a1 and a2 point to standard, counted strings.
//;	if no channel open, a1=0 refers to dft extensions screenfile in {libdir}.
//;	re {load}, {lload}, {lload?}, {xlload}, {l-find}, {lword?}, {-wl}, {-l}
//;	NOTE: the {empty-buffers} command should be issued before re-loading from a
//;	modified or different screenfile if assigned to the currently used 'drive'. 
	c lsload,"l-load",docolon,,,root
	.long lsload1,interpret,lsload2
//;
//; lload?   	ccc( -- )				root
//;	{lload} if word ccc not found in current search order.
	c lloadq,"lload?",docolon,,,root
	.long lloadq1,interpret,lload2
//;
//; lload   	ccc( -- )				root
//;	simplified 'l-load': fetching requested word from input stream and
//;	loading from current or default screen-file.
//;	e.g, 'lload'ing and protecting the 'help' word:
//;		{ lload help latest fence ! }
//;	re {load}, {l-load}, {lload?}, {xlload}, {l-find}, {lword?}, {-wl}, {-l}
	c lload,"lload",docolon,,,root
	.long lload1,interpret,lload2
//;
//; xlload   	ccc( -- )				root
//;	simplified 'l-load': fetching requested word from input stream and
//;	loading from the extensions screen-file <f4x.scr> at install dir.
	c xlload,"xlload",docolon,,,root
	.long xlload1,interpret,lload2
//;
//; xlload?   	ccc( -- )				root
//;	{xlload} if word ccc not found in current search order.
	c xlloadq,"xlload?",docolon,,,root
	.long xlloadq1,interpret,lload2
//;
//; load   	( n -- )				root
//;	Begin interpretation of screen n, n > 0. Loading terminates at the end of 
//;	the screen or at ";s", re {-->}, {using}. No nesting limit - re {>input}:
//;	saves {scr}, {blk}, {in}, {offset}, {base}, {tib}, {s0}, {dr0}, {dr1} 
//;	and {context}, {current} across nested calls, defaults to decimal base.
//;	all 'load'ing words may pass parameters on dstack to consecutive calls.
//;	NOTE: the 'empty-buffers' command should be issued before re-loading from a
//;	modified or different screenfile if assigned to the currently used 'drive'. 
	c load,"load",docolon,,,root
	.long load1,interpret,load2
//;
//; -->		( -- )				(d)(i)	root
//;	Continue interpretation w. next screen of currently selected 'drive'.
//;	increments uvari "scr" by one. deferred to "[-->]"
//;	can be altered w. 'is' or 'to' or, by disp @pfa wrt @(vb).
	c ddg,"-->",domdfer,imed,,root
	    .long ddgreatv-D,bddgb+cfa2pfa
//;
//; x		( -- )				(i)(d)(p) local
//;	the <nul> word, i.e. ascii 0; execution procedure either advancing the 
//;	interpreter to the next screen-file block or, terminating an input line 
//;	and continuing interpretation of terminal input. implicitely called at 
//;	eol or at end of a buffer, as the terminating <nul> byte.
//;	can be altered w. { ( new-pfa -- ) cfa (x) >body ! }.
	cnfa x,imed,,local
	.byte 0
	clfa x,domdfer,,,local
	.long xxv-D,xx+cfa2pfa
//;
//;
//; ---- random access file i/o ----
//;

//;
//; chain  	( ch -- )			(p)	root
//;	interpret sequential text file by channel ch, nestable w.o. limits.
//;	allocates 512 bytes of local tib and stores -1 to {blk} which will be 
//;	decremented by the rsp. latest input line length, saves i/o description
//;	i/o descr. restored at eof, use {end} for early termination. re {open}
	c chain,"chain",Xchain,,,root
//;
//; end		( ch -- )			(p)	root
//;	terminates {chain}ed file input - re {chain}, restores previous i/o 
//;	description (re {input>}), de-allocates local tib and, clears bit#6 in 
//;	uvari (bits); {end} is a {noop} if the rsp. file not included by {chain}
//;	and if no other such levels exist - i.e. if bit#6 @(bits) is not set.
//;	implicitely executed at eof of a {chain}ed file.
	c end,"end",Xend,,,root
//;
//; w4f		( ch -- )			(p)(s)	hidden
//;	wait until input pending to channel ch. syscall (new)select w. fd from ch, 
//;	watching input and exceptions events. channels limited to fd range [0,31].
//;	timout defaults to (almost) endless, can explictely set to no. of seconds 
//;	in double uvari {(ti)} - which should be saved across re-defining calls.
	c w4f,"w4f",Xw4f,,,hidden
//;
//; bchp?	( -- a )			(p)	hidden
//;	ret chp to screen-file channel currently in use.
//;	re {dr!}, {dr0}, {using}, {bchan}, {close}, {dr?}, {chp}.
	c chpq,"bchp?",Xchpq,,,hidden
//;
//; chp		( ch -- a' )			(const)
//;	ret pointer a' to redirection index of channel ch, re {chan}, {bchp?}.
//;	{ 0 chp } returns the redirection table base address.
//;	channels are doubly redirected/indexed: chp+disp contains the byte sized 
//;	index into the rsp. channel definition table - re {cdt} - which contains 
//;	the rsp. file descriptor and several, frequently used associated data.
	c chpp,"chp",docplus
	.long chpv
//;
//; chmax  	( -- n )			(bcons)
//;	no. of available system channels, byte-cons pover of 2, dft chmax = 32.
//;	count begins at zero, channel no. {chmax} reserved for keyboard access.
	c chmax,"chmax",dobconstant
	.byte chans
//;
//; chmid  	( -- n )			(const)	linux
//;	mask to the rsp. channel-id identifying an mmapped, open device channel.
//;	the file descriptor can be restored and verified w. 
//;		{ ch-id chmid xor dup 0< 0= }.
//;	where the true flag notifies an mmap-ed channel; re {chmap}.
	c chmid,"chmid",doconstant,,,linux	// = 0x80000000
	.long chfmmap
//;
//; chmap  	( -- n )			(const)	linux
//;	pseudo-fd identifying an mmapped, closed device channel; re {chmid}.
	c chmap,"chmap",doconstant,,,linux	// = 0xbfffffff
	.long chfmclo
//;
//; stdin  	( -- relch )			(bcons) linux
//;	index to redirected standard input chp, re {chp}, {cdt}, {chan}.
	c rstdi,"stdin",dobconstant,,,linux
	.byte rstdin
//;
//; stdout  	( -- relch )			(bcons) linux
//;	index to redirected standard output chp, re {chp}, {cdt}, {chan}.
	c rstdo,"stdout",dobconstant,,,linux
	.byte rstdout
//;
//; stderr  	( -- relch )			(bcons) linux
//;	index to redirected standard error output chp, re {chp}, {cdt}, {chan}.
	c rstde,"stderr",dobconstant,,,linux
	.byte rstderr
//;
//; work   	( -- relch )			(bcons) linux
//;	index to redirected primary auxilary chp for file-i/o operations, re {chp}
	c rwrk,"work",dobconstant,,,linux
	.byte rworkch
//;
//; aux		( -- relch )			(bcons) linux
//;	index to redirected 2nd-ary auxilary chp for file-i/o operations, re {chp}.
	c raux,"aux",dobconstant,,,linux
	.byte rauxch
//;
//; prt		( -- relch )			(bcons) linux
//;	index to redirected 2nd-ary output chan, use {chan} for the system channel;
//;	simultaneous output if {stdout} and {prt} open and, {prt} not a terminal.
	c rprt,"prt",dobconstant,,,linux
	.byte rprtch
//;
//; dr1c   	( -- relch )			(bcons) linux
//;	index to redirected dr1 channel no; re {chp}, {cdt}, {chan}.
	c dr1r,"dr1c",dobconstant,,,linux
	.byte rdr1ch
//;
//; dr0c   	( -- relch )			(bcons) linux
//;	index to redirected dr0 channel no; re {chp}, {cdt}, {chan}.
	c dr0c,"dr0c",dobconstant,,,linux
	.byte rdr0ch
//;
//; kbd		( -- relch )			(bcons) linux
//;	redirected keyboard input channel; re {chp}, {cdt}, {chan}.
	c rkbd,"kbd",dobconstant,,,linux
	.byte rkbdch
//;
//; chan   	( relch -- ch )			(p)
//;	convert redirectable channel relch to system channel - re {chp}, {cdt}.
//;	no error checking. channels redirection for i/o words:
//;	rel | dft system channel (chmax=32)
//;	  0 |  0	stdin	by expect, key, ?terminal, &c
//;	  1 |  1	stdout	by type, emit, &c
//;	  2 |  2	stderr	output by {message}, {error} &c
//;	  3 |  3	work	auxilary channel used by kernel calls, {open}
//;	  4 |  4	aux	for user code, e.g. as an output or work file
//;	  5 |  5	prt	for 2nd-ary output device, printer &c
//;	system chans [6,chmax-3] not redirected. for applications, re {new-chan}.
//;	  6 | chmax	kbd	main console keyboard channel e.g. by /dev/tty.
//;	  7 | chmax-2	dr0c	1st blockfile 'drive', selected by {dr0}, re {using}
//;	  8 | chmax-1	dr1c	2nd 'drive', selected by {dr1}; re {save"}, {chain"}.
	c relch,"chan",Xrelch
//;
//; brchan   	( 0|1 -- c )			(p)
//;	convert {dr?} block-file channel index to index into relocation table;
//;	re {bchan}, {chan}, {dr0c} {dr1c}, {dr0}, {dr1}, {chp}.
	c brc,"brchan",Xp7
//;
//; bchan   	( 0|1 -- ch )			(p)
//;	convert {dr?} block-file channel index via rel-chan look-up table at {chp} 
//;	to the rsp. system channel number; re {chan}, {dr0c} {dr1c}, {dr0}, {dr1}.
	c bchan,"bchan",Xbchan
//;
//; cdt		( ch -- a )			(p)	hidden
//;	ptr to definition table of system channel ch; re {chp}. cdt items:
//;		chid	 0	file descriptor
//;		chpt	 4	(reserved for ptr to extension field)
//;		tyfl	 8	type flags, by f4, and flag whether mmap-ed, 
//;				display type flags spec. w. { help tyfl }.
//;		chnd	12	2nd-ary handler (un-used)
//;		mdfl	16	open mode, file acces
//;		perm	20	file permissions
//;		rnum	24	latest op error code
//;		fpos	28,32	current file pointer
//;		ibuf	36	single char input buffer
//;		fsiz	40,44	file size
//;		fctl	48	by iomode test:	i/o flags from fcntl 
//;				if bmapc@tyfl:	mode|prot
//;		cmds	52	if bmapc@tyfl: disp into file in units of PAGE_SIZE
//;				console channel: received vt ctrl sequence (un-used)
//;		cmap	56	if bmapc@tyfl: mmapped base address
//;		cmsz	60	if bmapc@tyfl: mmapped size
	c cdt,"cdt",Xcdt,,,hidden
//;
//; ch-id  	( ch -- fd )			(p)
//;	file descriptor of channel ch from it's channel definition table, re {cdt}.
//;	special values: -1 channel not open;
//;		{chmap} ORed to fd if file mmap-ed, {chmid} mmap-ed and closed.
//;	restore mmapped, open fd { ( ch -- ) dup ch-id chmid xor swap cdt ! }.
	c chsid,"ch-id",Xchid
//;
//; new-chan	( -- n | 0 )			(p)
//;	find lowest free cdt entry in chp list. false (0) if none left.
	c newchan,"new-chan",Xnewchan
//;
//; openm   	( a n n1 ch -- ch|er )		(p)(s)	hidden
//;	{open} with n1 permission bits instead of current umask setting - re {fperm}.
	c openm,"openm",Xopenm,,,hidden
//;
//; open   	( a n ch -- ch|er )		(p)(s)
//;	close channel ch, open file or device by <nul>-terminated name at a+1 
//;	w. default permissions & open-mode n -re- {r/o}, {w/o}, {w/r}, {cre}; 
//;		other open access flags re lx source, <include/asm/fcntl.h>,
//;		file permission bits defined in lx, <include/linux/stat.h>.
//;	initiates cdt of channel ch, ret channel no. ch or, -ve error code.
//;	file permission bits by uvari {fperm} XOR umask-setting. - re {openm}.
//;	on success, executes {fstat}, {flen}, {cons?} and stores to cdt:
//;		file descriptor	cdt+00  'ch-id', from linux 'sys_open'
//;		file type	cdt+08  hi:'fstat'st_dev, lo:F4 type bits
//;		open mode	cdt+12  mode flags from 'open'
//;		file ptr	cdt+24  double
//;		file size	cdt+36	double
//;	re {bchan}, {cdt}, {ch-id}, {chan}, {close}, {new-chan}, {mmapf}, {w4f}
	c open,"open",Xopen
//;
//; r/o		( -- n )			(bcons)
//;	file open mode flag, linux cons 'O_RDONLY'.
	c rbyo,"r/o",dobconstant
	.byte O_RDONLY
//;
//; w/o		( -- n )			(bcons)
//;	file open mode flag, linux cons 'O_WRONLY'.
	c wbyo,"w/o",dobconstant
	.byte O_WRONLY
//;
//; w/r		( -- n )			(bcons)
//;	file open mode flag, linux cons 'O_RDWR'.
	c wbyr,"w/r",dobconstant
	.byte O_RDWR
//;
//; dir		( n1 -- n2 )			(p)
//;	OR directory creation flag to open mode, linux cons 'O_DIRECTORY'
	c dir,"dir",Xdir
//;
//; app		( n1 -- n2 )			(p)
//;	OR write-append mode flag to open mode, linux cons 'O_APPEND'
	c app,"app",Xapp
//;
//; cre		( n1 -- n2 )			(p)
//;	OR file creation flag to open mode, linux cons 'O_CREAT|O_TRUNC'
	c cre,"cre",Xcre
//;
//; close  	( ch -- flg )			(p)(s)
//;	close channel ch, ret false flg or, +ve fd if duplicate channel exists.
//;	stdin protected, attempt to closeing stdin returns -1.
//;	invalidates file descriptor entry in cdt, i.e. {ch-id} returns -1.
//;	channel-id set to {chmap} if the rsp. file was mmap-ed, subsequent 
//;	execution of {close} unmaps the rsp. memory and stores -1 to {ch-id}.
	c close,"close",Xclose
//;
//; ch-rnum	( ch -- er )			(p)
//;	fetch system channels' ch latest error code, +ve is count of bytes sent 
//;	or received, 0 at <eof>, range [0xffff0001,0xfffffff] is an error code.
//;	
	c chrnum,"ch-rnum",Xchrnum
//;
//; ch-opn  	( ch -- flg )			(p)
//;	true flag if channel ch accessible, whether by file channel or 'mmap'ed.
	c chopn,"ch-opn",Xchopn
//;
//; ch-key 	( ch -- a )			(p)	hidden
//;	adress of channels' ch single char keyboard input buffer.
//;	F4A: optionally in kernel if asm-cons _ck_ =/= 0.
_ck	c chky,"ch-key",Xchkey,,,hidden
//;
//; ch-cons	( rch -- flg )			(p)(s)	linux
//;	true flag from rsp. cdt data if redir. channel rch is a console/terminal.
//;	F4 system channels and newly opened ones will have the flags already set; 
//;	otherwise, {cons?} can be used to determine console state by ioctl query.
	c chcons,"ch-cons",Xchcons,,,linux
//;
//; io-cons	( -- flg )			(p)(s)	linux
//;	true flag if stdin and stdout console channels, test by TIOCGSID ioctl.
//;	if either, initial stdin or, stdout not a console channel, the start-up
//;	message, 'prompt' and <cr> after {expect} are suppressed, for scripting.
	c iocons,"io-cons",Xiocons,,,linux
//;
//; cons?  	( ch -- flg )			(p)(s)
//;	true flag if system-channel ch is a console channel, by TIOCGSID ioctl.
	c consq,"cons?",Xconsq
//;
//; mmapm	( ch n1 n2 -- n a | x er )	(p)(s)	linux
//;	map file by channel ch w. access flags n1 and mode n2, into memory, 
//;	ret size n and addr a or, undeterminate x and ernum er; re {mmapf},{unmap}
	c mmapm,"mmapm",Xmmapm,,,linux
//;
//; mmapf  	( ch -- n a | x er )		(p)(s)	linux
//;	map a file by channel ch into memory, ret size n and addr a or, ernum er.
//;	simplified 'sys mmap', for the entire file, 'shared' if file is writable 
//;	else, 'private' mode and, mmap un-protection according to file open mode.
//;	channel-id ORed w. {chmid}, signifying that the file is in memory.
//;	channel may be closed, afterwards. for de-allocation re {close}, {unmapf}:
//;	2nd execution of {close} unmaps the rsp. memory and stores -1 to {ch-id}, 
//;	{munmapf} won't close an open channel; {munmap} doesn't modify cdt entry.
	c mmapf,"mmapf",Xmmapf,,,linux
//;
//; unmapf 	( ch -- 0|er )			(p)(s)	linux
//;	unmap a file by channel ch; ret 0 or ernum er. an open file remains open.
	c unmapf,"unmapf",Xunmapf,,,linux
//;
//; fstat   	( a ch -- a | er )		(p)(s)	linux
//;	fetch file status of channel ch to buffer a. buffer at a receives the 
//;	status data - the sizes items of which are not reliable or, just un-set!
//;	sys fstat, 64 bytes (lx 2.2),			fstat64, 96 bytes (2.4):
//;		st_dev		00d	00h	16-bit	00d	00h	16-bit
//;		st_ino		04	04		12	0c
//;		st_mode		08	08	16-bit	16	10
//;		st_nlink	10	0a	16-bit	20	14
//;		st_uid		12	0c	16-bit	24	18
//;		st_gid		14	0e	16-bit	28	1c
//;		st_rdev		16	10	16-bit	32	20
//;		st_size		20	14		44	2c	64-bit
//;		st_blksize	24	18		52	34
//;		st_blocks	28	1c		56	38
//;		st_atime	32	20		64	40
//;		st_mtime	40	28		72	48
//;		st_ctime	48	30		80	50
//;		st_ino:		-/-	-/-		88	58	64-bit
//;	un-named positions reserved.
	c fstat,"fstat",Xfstat,,,linux
//;
//; flen   	( ch -- n )			(p)(s)
//;	sing, file size of channel ch - by multiple calls to sys _llstat.
//;	read 64-bit fptr: { ch cdt 24 + 2@ }, file size: { ch cdt 36 + 2@ }.
	c flen,"flen",Xflen
//;
//; fposre  	( disp ch -- fpos )		(p)(s)
//;	set fileptr of channel ch to current fposn + signed disp.
//;	pointing to beyond eof is not an error, next write opr extends file.
//;	pointing to before beginning of file returns -22, 'invalid argument'.
//;	current fptr by disp := 0; re {fposa}, {fpose}; 64-bit fptr re {flen}.
	c fposre,"fposre",Xfposre
//;
//; fposab  	( disp ch -- fpos )		(p)(s)
//;	set fileptr of channel ch to posn disp. re {fposre}
	c fposa,"fposab",Xfposa
//;
//; fpose  	( disp ch -- fpos )		(p)(s)
//;	set fileptr of channel ch to eof-disp. re {fposre}
	c fpose,"fpose",Xfpose
//;
//; fread   	( a u ch c -- a u1 | a er )	(s)(p)
//;	fetch at most u bytes from channel ch to buffer a, if c not -ve 
//;	terminate as soon as byte c found and ret stg w.o. terminator.
//;	ret address a, received count u1 or, address a and -ve ernum.
//;	if a=0 read a single byte and ret a=0,u1=code or, a=0 and u1=0.
	c fread,"fread",Xfread
//;
//; fwrite   	( a u ch c -- u1|er )		(s)(p)
//;	send u bytes from buffer a to channel ch and append (multi-byte) char c 
//;	if c not -ve. ret count u1 of bytes sent or, -ve ernum. 
//;	if a=0 send char c to channel ch, ret 
//;	writing to mmap-ed file within initial file size stores to memory and 
//;	requires subsequent execution of 'msync' to updating the mapped file.
	c fwrite,"fwrite",Xfwrite
//;
//;
//; ---- terminal ----
//;

//;
//; (vtr)  	( -- a )				hidden
//;	stores the fd by which on program termination the console attributes would 
//;	be restored to the inital state; can be disabled by storeing any -ve value, 
//;	to preventing false reset from interleaved programs execution (piped) &c.
	c bvtrp,"(vtr)",doconstant,,,hidden
	.long fdatrv
//;
//; (uclc)   	( -- a )			(const)	hidden
//;	address of 256 bit-flags (32 bytes) for "uc-lc"; indexed by character 
//;	code, set bits correspond to by { 32 XOR } case convertible chars.
	c puclcq,"(uclc)",doconstant,,,hidden
	.long cchart
//;
//; g0		( -- )				(p)
//;	restore G0 character set (emit <si> code). vt command.
	c g0,"g0",Xg0
//;
//; rs		( -- )				(p)(s)	root
//;	restore initial console state, vt command and ioctl. re {(vtr)}
	c rs,"rs",Xrs,,,root
//;
//; !max-xy 	( -- x y )			(p)(s)
//;	forcedly re-fetch and store current console display sizes by sys_ioctl.
//;	{x,y} = {-1,0} if stdout is not a terminal channel, re {at-xy}, {xy}, {vt?}.
//;	code address of the rsp sys_ioctl w.o. data storeage: { ' [maxxy] cfa @ 4- }.
	c smxy,"!max-xy",Xsmaxxy
//;
//; [maxxy] 	( -- x y )			(p)(s)	hidden
//;	default procedure for deferred {max-xy}:
//;	current console display width x and height y, by caracters; by sys_ioctl.
//;	{x,y} = {-1,0} if stdout is not a terminal channel, re {at-xy}, {xy}, {vt?}.
	c pmaxyq,"[maxxy]",Xmaxxy,,,hidden
//;
//; max-xy 	( -- x y )			(u)
//;	current console display width x and height y, by caracters; by sys_ioctl.
//;	{x,y} = {-1,0} if stdout is not a terminal channel, re {at-xy}, {xy}, {vt?}.
//;	deferred, can be altered w. 'is' or, by disp @pfa wrt address of (up).
	c maxxy,"max-xy",doudfer
	udefl(maxyu),pmaxyq+cfa2pfa
//;
//; at-xy  	( x y -- )			(u)
//;	set cursor posn to coumn:=x, row:=y, counted by chars. vt command.
//;	according to vt100,vt102 top left is {1,1} which is not always correctly
//;	handled by Linux console and X-terminal emulators. re {max-xy}, {xy}.
//;	deferred, can be altered w. 'is' or, by disp @pfa wrt address of (up).
	c atxy,"at-xy",doudfer
	udefl(atxyu),patxyq+cfa2pfa
//;
//; xy		( -- x y )			(u)
//;	current cursor posn, colum=x, row=y, by chars. vt command "esc[(y);(x)H".
//;	either of x,y = -1 if stdout is not a terminal, re {at-xy}, {max-xy}.
//;	deferred, can be altered w. 'is' or, by disp @pfa wrt address of (up).
//;	NOTE:	false values after sending a char to the last column, i.e. posn 
//;		fetched before and after return identical co-ordinates!
//;		(console should solve 'pending newline problems', internally!)
	c xy,"xy",doudfer
	udefl(xyu),pxyq+cfa2pfa
//;
//; vt?		( -- flg )			(p)
//;	flg =/= 0 if stdout connected to a terminal of vt102 or vt100 type.
//;	test for '[?6' vt102, then '[?1;Pc', P=dec.digit, vt100+ of xterm/rxvt.
//;	determines whether stdout is a terminal, returns false, otherwise.
//;	flg=1 if vt102 (lx console), flg=P+1, P>/0, if a vt100 variant.
	c vq,"vt?",Xvtq
//;
//; ?rky   	( -- c 1 | 0 )			(p)
//;	test for raw keyboard input; ret accumulated code and tf or, 0.
	c rqky,"?rky",Xrqky
//;
//; rkey   	( -- c )			(p)
//;	raw keyboard input; ret accumulated code
	c rkey,"rkey",Xrkey
//;
//; ?esc   	( -- flg )			(p)
//;	non-waiting fetch next byte from stdin or, ret w. false flg; 
//;	if input available and fetched byte was <esc> ret true flag, else 
//;	wait until <bl> received and ret false or, ret 1 after <esc>.
	c qesck,"?esc",Xqesck
//;
//; ?input 	( ch -- c tf | ff )		(p)
//;	if received, ret any one byte from supplied channel & tf, else ff.
	c qinput,"?input",Xqinput
//;
//; ?key   	( -- c 1 | 0 )			(u)
//;	if available, ret byte from stdin & tf, else ret ff. dft "[?key]".
//;	can be altered w. 'is' or, by disp @pfa wrt address of (up).
	c qkey,"?key",doudfer
	udefl(qkeyu),bqkeyb+cfa2pfa
//;
//; key		( -- c )			(u)
//;	fetch any one byte from stdin, waiting until char received
//;	can be altered w. 'is' or, by disp @pfa wrt address of (up).
	c key,"key",doudfer
	udefl(keyu),bkeyb+cfa2pfa
//;
//; ?terminal	( -- f )			(p)(s)
//;	ret true flag if input from stdin available
	c qterminal,"?terminal",Xqterminal
//;
//; edlin 	( a u -- )			(p)(s)	forth
//;	fetch up to u bytes until <nl> from stdin to address a.
//;	edit default asciz string at a if byte at a =/= 0.
	c edlin,"edlin",Xedlin
//;
//; expect 	( a u -- )			(u)
//;	fetch up to u bytes until <nl> from stdin to address a,
//;	store count-byte at a and terminate stg w. a <nul> byte. string input 
//;	terminated at <enter>, regardless of trailing chars or, after u chars.
//;	deferred, can be altered w. 'is' or, by disp @pfa wrt address of (up).
//;	dft vector "[expect]", editting if stdin is a console:
//;		BS	^H	cursor left
//;		TAB	^I	cursor right
//;			^L	clear screen, keep cursor
//;		DEL	127	del left
//;		<del>		del char at cursor
//;		<ins>		toggle inserting(dft) and overwriting mode
//;		<left>		cursor left
//;		<right>		cursor right
//;		<up>		(previous in history, n.i.)
//;		<dn>		latest line re-call; (next in history, n.i.)
//;		<pos1>		sol, 1st posn
//;		<pos2>		eol, last posn
	c expect,"expect",doudfer
	udefl(expectu),bexpectb+cfa2pfa
//;
//; print     	( a -- )			(p)
//;	counted string from address a to stdout,
//;	substituting non-printing chars by "_" underline char.
//;	drop arg & ret if a or stg count is zero or > 64K.
	c print,"print",Xprint
//;
//; ptype     	( a u -- )			(p)
//;	transmit u characters (bytes) from address a to stdout, 
//;	substituting non-printing chars by "_" underline char.
//;	drop args & ret if either, a or u is zero.
//;	u limited to 64K. for longer text re "writef" which is not limited.
	c ptype,"ptype",Xptype
//;
//; type     	( a u -- )			(u)
//;	transmit u characters (bytes) from address a to stdout.
//;	drop args & ret if either, a or u is zero. deferred, dft "[type]", 
//;	can be altered w. 'is' or, by disp @pfa wrt address of (up).
	c type,"type",doudfer
	udefl(typeu),btypeb+cfa2pfa
//;
//; emitm 	( n c -- )			(p)((u)) hidden
//;	{emit} n chars c to stdout.
	c emm,"emitm",Xemitm,,,hidden
//;
//; emit     	( n -- )			(u)
//;	transmit multi-byte char, l.s. byte 1st, from cell n to stdout.
//;	can be altered w. 'is' or, by disp @pfa wrt address of (up).
	c emit,"emit",doudfer
	udefl(emitu),bemitb+cfa2pfa
//;
//; so     	( -- )				(p)((u)) hidden
//;	emit ascii <so> to stdout (^N, code 14). vt command activates G0 charset.
	c so,"so",Xso,,,hidden
//;
//; si     	( -- )				(p)((u)) hidden
//;	emit ascii <si> to stdout (^O, code 15). vt command activates G1 charset.
	c si,"si",Xsi,,,hidden
//;
//; bel     	( -- )				(p)((u)) hidden
//;	emit acoustic signal vt code, <bel>.
	c bel,"bel",Xbel,,,hidden
//;
//; bs     	( -- )				(p)((u))
//;	emit back-space code, <bs> - which is NOT a back-blank!
	c bs,"bs",Xbs
//;
//; tab     	( -- )				(p)((u))
//;	transmit vertical tabulator, <vt>, code to stdout.
//;	linux console defaults to eight chars' tab spacing.
	c tab,"tab",Xtab
//;
//; -cr     	( flg -- )			(p)((u))
//;	"cr" if true flg.
	c dashcr,"-cr",Xdashcr
//;
//; cr     	( -- )				(p)((u))
//;	{emit} the new-line code (carriage return & line feed, linux: ascii 10).
	c cr,"cr",Xcr
//;
//; space  	( -- )				(p)((u))
//;	{emit} an ascii blank, {bl}.
	c space,"space",Xspace
//;
//; spaces 	( n -- )			(p)((u))
//;	{emit} n blanks.
	c spaces,"spaces",Xspaces
//;
//;
//; ---- text, strings ----
//;

//;
//; lc-depend	( -- )				(p)	root
//;	set letter case independent text processing - iso8859 ranges apply.
//;	re {lc-ignore}, {>uc}, {>lc}, {uc-lc}, {key>alpha}, {key>char}.
	c lcd,"lc-depend",Xlcd,,,root
//;
//; lc-ignore	( -- )				(p)	root
//; LC-IGNORE	( -- )				(p)	root
//;	set letter case independent text processing - iso8859 ranges apply.
//;	re {lc-depend}, {>uc}, {lc}, {uc-lc}, {key>alpha}, {key>char}.
	c lcil,"lc-ignore",Xlci,,,root
	c lciu,"LC-IGNORE",Xlci,,,root
//;
//; bcount  	( c -- c u )			(p)	hidden
//;	ret count u of significant bytes in multi-byte char c. re {b!}
	c bcount,"bcount",Xbcount,,,hidden
//;
//; b@		( a -- n )		n.i.	(d)	hidden
//;	fetch multi-byte char n from address a.
//;	currently xec {0=}, vector left to suitable, later definition.
//;	can be altered w. {is} or, by disp @pfa wrt @(vb).
	c bfetch,"b@",domdfer,,,hidden
	.long bfetchv-D,zeq+cfa2pfa
//;
//; b!		( n a -- )			(d)	hidden
//;	store multi-byte char n to address a. dft {[b!]} which stores 
//;	the least significant bytes, until <nul>, of multi-byte char n.
//;	can be altered w. {is} or, by disp @pfa wrt @(vb). re {bcount}.
	c bstore,"b!",domdfer,,,hidden
	.long bstorev-D,bbstoreq+cfa2pfa
//;
//; cmove  	( from to n -- )		(p)
//;	copy n bytes from to, in ascending order
	c cmove,"cmove",Xcmove
//;
//; <cmove 	( from to n -- )		(p)
//;	copy n bytes from to, top down, in descending order
	c lesscmove,"<cmove",Xlcmove
//;
//; >move< 	( a1 a2 +n -- )			(p)	forth
//;	copy n>0 bytes from a1 in reversed order to a2.
	c mox,">move<",Xmovex,,,hidden
//;
//; move   	( from to n -- )		(p)
//;	non-overwriting copy n bytes from to.
	c move,"move",Xmove
//;
//; erase  	( addr n -- )			(p)
//;	Clear a region of memory to zero from addr over n addresses.
	c erase,"erase",Xerase
//;
//; blanks  	( addr n -- )			(p)
//;	Fill region of memory w. blanks from addr over n addresses.
	c blanks,"blanks",Xblanks
//;
//; fill   	( a n c -- )			(p)
//;	store count<n> byte of char<c> to memory at address<a>
	c fill,"fill",Xfill
//;
//; key>alpha	( n -- n 1|0 )			(p)
//;	check whether char code(byte n) is in alpabetic chars range wrt iso8859
//;	character sets. { dup dup uc-lc =/= } ret 1 if alphabetic char code.
//;	F4A: optionally in kernel if asm-cons _k2_ =/= 0.
//;	re {>uc}, {uc-lc}, {printing?}, {key>char}, {lc-ignore}, {lc-depend}.
_k2	c ky2a,"key>alpha",Xkey2alpha
//;
//; key>char	( n -- n 1|-1|0 )		(p)
//;	check whether char code n is in printing charcters' range wrt iso8859 
//;	char codes; ranges from uvari "(asci)", dft lsb[32,126],[160,255]msb.
//;	ret code n and 0 if not, else 1 if in lower, -1 upper char code range.
//;	re {>uc}, {uc-lc}, {lc-depend}, {key>alpha}, {printing?}, {lc-depend}.
	c ky2c,"key>char",Xkey2char
//;
//; printing?  	( a u -- a u | a 0 )		(p)
//;	check whether entire string(a,u) of printing charcters. 
//;	ret u = 0 if non-printing chars found.
//;	re {>uc}, {uc-lc}, {key>alpha}, {key>char}, {lc-ignore}, {lc-depend}.
	c qpr,"printing?",Xprintingq
//;
//; >us  	( a u -- a u )			(p)	hidden
//;	modify string(a,u) to upper letter case - in iso8859 ranges.
//;	re {>uc}, {uc-lc}, {key>alpha}, {key>char}, {lc-ignore}, {lc-depend}.
	c xsu,">us",Xsu,,,hidden
//;
//; >uc  	( c -- c' )			(p)	hidden
//;	convert character code c to upper letter case - in iso8859 ranges.
//;	table of the rsp. bit-flags at { (uc-lc) 32 + }.
//;	re {>lc}, {uc-lc}, {key>alpha}, {key>char}, {lc-ignore}, {lc-depend}.
	c xcu,">uc",Xcu,,,hidden
//;
//; >lc    	( c -- c' )			(p)	hidden
//;	convert character code c to lower letter case - in iso8859 ranges.
//;	table of the rsp. bit-flags at { (uc-lc) 32 + }.
//;	re {>uc}, {uc-lc}, {key>alpha}, {key>char}, {lc-ignore}, {lc-depend}.
	c xcl,">lc",Xcl,,,hidden
//;
//; uc-lc  	( c -- c' )			(p)	hidden
//;	convert character code c to other letter case - in iso8859 ranges.
//;	table of the rsp. bit-flags at {(uc-lc)}.
//;	re {>uc}, {>lc}, {key>alpha}, {key>char}, {lc-ignore}, {lc-depend}.
	c xcx,"uc-lc",Xcx,,,hidden
//;
//; matchj  	( a1 u1 a2 u2 jc -- f disp )	(p)
//;	find string{a2,u2} in memory(a1,u1), chars jc in stg(a2,u2) always match.
//;	re {match}, {sj=}, {s=}, {lc-depend}, {lc-ignore}.
	c matchj,"matchj",Xmatchj
//; sj=		( a1 u1 a2 u2 jc -- flg )	(p)	hidden
//;	compare strings as w. {s=}, char jc in stg(a2,u2) would always match.
	c sjeq,"sj=",Xsjeq,,,hidden
//;
//; match  	( a1 u1 a2 u2 -- f disp )	(p)
//;	find string{a2,u2} in memory range(a1,u1).
//;	ret f=/=0 and count from found stg till end or, f := 0 and u1. if found,
//;	f=addr of stg beginning (flag by fig-standard may be any non-zero figure)
//;	letter-case dependency as set, for instance, w. {lc-depend}, {lc-ignore}.
	c match,"match",Xmatch
//;
//; s=		( a1 u1 a2 u2 -- flg )		(p)
//;	compare strings (a1,u1) & (a2,u2), 
//;	ret sign of subtraction by un-equal length else, char code.
//;	letter-case dependency as set, for instance, w. {lc-depend}, {lc-ignore}.
	c seq,"s=",Xseq
//;
//; scan   	( a u c -- a' u' )		(p)
//;	scan string(a,u) for 1st occurrence of char = c,
//;	ret posn a' of 1st char = c and remaining cnt u'.
//;	ret u':=0, a':=a+u (ptr to after stg), if char c not found.
//;	skip leading chars =/= c of length u string at a
//;	letter-case dependency as set, for instance, w. {lc-depend}, {lc-ignore}.
//;	specific handling of {bl} including low ctrl-s if c = <bl><bl>.
	c scan,"scan",Xscan
//;
//; rscan   	( a u c -- a' u' )		(p)
//;	reversed scan string(a,u) for 1st occurrence of char = c,
//;	ret posn a' of 1st char = c and remaining cnt u'.
//;	ret a':= a+u (ptr to after stg), u':=0 if char c not found. 
//;	letter-case dependency as set, for instance, w. {lc-depend}, {lc-ignore}.
//;	specific handling of {bl} including low ctrl-s if c = <bl><bl>.
	c rscan,"rscan",Xrscan
//;
//; skip   	( a u c -- a' u' )		(p)
//;	skip leading chars = c of length u string at adr a
//;	ret posn a' of 1st char =/= c and remaining cnt u'.
//;	ret a':= a+u, u':=0 if no other chars in the entire string. 
//;	letter-case dependency as set, for instance, w. {lc-depend}, {lc-ignore}.
//;	specific handling of {bl} including low ctrl-s if c = <bl><bl>.
	c skip,"skip",Xskip
//;
//; rskip   	( a u c -- a' u' )		(p)
//;	skip trailing chars = c of length u string at adr a,
//;	ret posn a' of 1st char =/= c and remaining cnt u'.
//;	ret a':= a, u':=0 if no other chars in the entire string. 
//;	letter-case dependency as set, for instance, w. {lc-depend}, {lc-ignore}.
//;	specific handling of {bl} including low ctrl-s if c = <bl><bl>.
	c rskip,"rskip",Xrskip
//;
//; -trailing	( a u -- a u1 )			(p)
//;	truncate trailing blanks from string(a,u).
	c dashtrailing,"-trailing",Xdasht
//;
//; traverse	( adr1 inc -- adr2 )		(p)
//;	step by <inc> bytes from adr1 until -ve or <nul> byte found.
//;	NOTE: F4A word header structure is different vs. the fig-forth model,  
//;	"traverse" towards the end of a 4th name returns address of 2nd-ary, 
//;	-ve count byte:
//;		[nfa:count+flags|chars|-ve count][lfa:-ve disp][cfa][pfa...
//;	e.g, nfa from pfa: { ' name lfa 1- -1 traverse }
//;	pse, note that F4 "traverse" towards nfa will fail on 'smudge'ed words.
//;	re "nfa", "pfa"; additional words operating on a nfa 'n<nfa' and '+@'.
	c traverse,"traverse",Xtraverse
//;
//; enclose	( a c -- a n1 n2 n3 )		(p)
//;	From text address a and delimiting character c, is determined the byte 
//;	offset to the 1st character n1 =/= c, the offset n2 to the next char = c,
//;	i.e. after the enclosed text and, the offset to the 1st char which is not 
//;	included. ascii <nul> is an unconditional delimiter.
//;	Enclosed len := n2-n1; enclosed text begins at a+n1; next {enclose} should 
//;	begin at a+n3. {enclose} used by WORD to isolate items from input stream, 
//;	operating letter case dependently. <tab> or other low ctrl-characters will 
//;	be considered a delimiter while {enclose}ing w. the blank space, {bl}.
	c enclose,"enclose",Xenclose
//;
//; /string	( a u n -- a1 u1 )		(p)
//;	adjust string beginning at a by +n and length u by -u,
//;	subtracts n from u @tos and adds n to a @nos, unless @tos < 0.
//;	+ve n truncates string(a,u) from beginning towards end, min. u1=0,
//;	-ve n extends string ptr a and length u beyond its beginning.
	c sstring,"/string",Xsstring
//;
//; 1/string	( a u -- a1 u1 )		(p)
//;	cut the leading char (byte) off stg(a,u); short for { 1 /string }.
	c s1string,"1/string",Xs1string
//;
//; e\stg  	( a1 u1 a2 -- a3 u3 )		(p)
//;	convert stg(a,u1) w/ escaped tokens to stg(a3,u3) at buffer (a2).
//;	converted stg is never longer than source, thus buffer expected 
//;	at least u1 chars of size, and not checked. a2 not in stg memory:
//;	e.g. { " X\aX" count 2dup + e\stg type } at "pad" after the string.
//;	the escape symbol is re-definable in uvari {(es)}, '\' by default.
//;	following (lower case, only) chars receive special treatment:
//;	numeric:
//;		\nnn	nnn = octal character code
//;	special symbols, letter case dependent:
//;		\a	<bel>
//;		\b	<bs> back space (which is NOT a back 'blank'!)
//;		\c	'echo' w/o trailing <lf> - not applicable, a 'noop'.
//;		\e	<esc>
//;		\f	<ff> form feed - by <nl> in console
//;		\i	<csi> by "<esc>[" because falsely impl. in xterm/console.
//;		\n	<new line>, converted to ascii 10, the linux <cr+lf> code.
//;		\r	<cr> crriage return, cursor to 1st column on current line.
//;		\t	<ht> horizontal tabulator as set for the rsp. console.
//;		\v	<vt> vertical tab, undef'd for terminals, line feed in F4
//;			which substitutes code 10 which falsely executes <cr+lf>.
//;		\-	noop, e.g. to delimiting regular numeric chars after \nnn.
//;		\\	double escape symbol, sending the escape symbol as a char.
//;	pse, note the linux specific <cr> and <lf> handling; the rsp. actions can
//;	be modified in the F4 sourcce, file 'inidata.S', label <etypett>.
	c estg,"e\\stg",Xestg
//;
//; scount  	( a1 -- a2 u )			(p)	hidden
//;	ret size u of counted string at a1 and address a2 of 1st char, 
//;	ret size u of asciiz stg at a1+1 if count was 0.
	c scount,"scount",Xscount,,,hidden
//;
//; zcount  	( a1 -- a1 c )			(p)
//;	ret size u of <nul> terminated string at a1.
	c zcount,"zcount",Xzcount
//;
//; count  	( a1 -- a2 u )			(p)
//;	ret size u of counted string at a1 and address a2 of 1st char.
	c count,"count",Xcount
//;
//;
//; ---- numeric conversion ----
//;

//;
//; q#		( q1 -- q2 )			(p)	hidden
//;	extract least significant digit from q1 and store corresponding 
//;	numeric character code wrt @base to numeric string assembly memory @hld, 
//;	adjust @hld, leave q2:=q1/@base. basic routine for {q.}, {uq.}, f4x.scr.
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c qs,"q#",Xqsharp,,,hidden
//;
//; <#		( -- )				(p)
//;	prepare ptrs for numeric string conversion, <# # #S SIGN #>.
	cnfa lesssharp
	.byte 60,35
	clfa lesssharp,Xlesssharp
//;
//; #>		( d -- addr  count )		(p)
//;	Terminates numeric output conversion by dropping d, leaving the 
//;	text address and character count suitable for TYPE.
	cnfa sharpgreat
	.byte 35,62
	clfa sharpgreat,Xsharpgreat
//;
//; sign   	( n d -- d )			(p)
//;	if n < 0 prepend the negative sign char to numeric string being built @hld.
	c sign,"sign",Xsign
//;
//; #		( d1 -- d2 )			(p)
//;	extract least significant digit from d1, prepend the rsp. char to the 
//;	output string at "hld", leave d2 as quotient from division by current 
//;	conversion radix, from BASE. Used between <# and #>. See #S.
	cnfa sharp
	.byte 35
	clfa sharp,Xsharp
//;
//; #s		( d1 -- d2 )			(p)
//;	numeric text string in output buffer at "hld", by the use of #,
//;	until a zero double number n2 results. Used between <# and #>.
	cnfa sharps
	.byte 35,115
	clfa sharps,Xsharps
//;
//; h>a		( n -- c )			(p)
//;	convert lsb of n to numeric char c, n < 198, noop if @base>255.
	c h2a,"h>a",Xh2a
//;
//; digit  	( n base -- n tf | ff )		(p)
//;	leave n and tf if n is in range [0,@base), else replace n w. false flag.
	c digit,"digit",Xdigit
//;
//; hold   	( c -- )			(p)
//;	Used between <# and #> to pre-pend a multi-byte character code, l.s.byte 
//;	1st (opposite wrt 'emit'), to the current numeric output string, at @hld.
//;	e.g. { 123. <# #s ":-> hold #> type } displays the string ":->123".
	c hold,"hold",Xhold
//;
//; d.r		( d n -- )			(p)
//;	print double number d right aligned in a field n blanks of width.
	c ddotr,"d.r",Xddotr
//;
//; db.		( d n -- )			(p)
//;	print double number d at radix n to stdout.
	c dbdot,"db.",Xdbdot
//;
//; d.		( d -- )			(p)
//;	print double number to stdout, radix @base.
//;	re "b.", "db." for numeric output at temporary "base"
	c ddot,"d.",Xddot
//;
//; .r		( n1 n2 -- )			(p)
//;	print number n1 right aligned in a field n2 blanks of width.
	c dotr,".r",Xdotr
//;
//; b.		( n1 n2 -- )			(p)
//;	print number n1 at radix n2 to stdout.
	c bdot,"b.",Xbdot
//;
//; .		( n -- )			(p)
//;	print number n to stdout, radix @base.
//;	re "b.", "db." for numeric output at temporary "base"
	c dot,".",Xdot
//;
//; u.		( n -- )			(p)
//;	print unsigned numeric value n to stdout, radix @base.
	c udot,"u.",Xudot
//;
//; ?		( a -- )			(p)
//;	print unsigned numeric value from cell at a to stdout, radix @base.
	c quest,"?",Xquest
//;
//; a>b 	( a1 -- a2 b )			(p)	hidden
//;	extracts sign|base b from numeric string a1, according to praefix, else 
//;	returns current @base. a2 adjusted according to size of praefix + sign.
//;	Numeric input may further be postfixed by a multiplier, re {[number]}.
//;	(almost) any sequence of praefices will do:
//;		-ve:	"{praefix}-nnn"	and, w. the same effect
//;			"-{praefix}nnn"	(by OTA-std not permitted)
//;		e.g:	"-&2.3"		octal double: -23 octal = -19 decimal.
//;	l.s.w. (16-bit) of b set to temporary radix value, sign flag in msw.
//;	valid prefices & radices:
//;		'%'	2		'§'	12
//;		'!'	4		'$'	16
//;		'@'	8 (OTA)		'0x'	16,  -ve w. leading '-', only
//;		'&'	8		'"'	256  ascii, -ve w. leading '-'
//;		'#'	10		'^'	char-64, convert to ctrl code.
//;		'\'	-2	provisional for f.p. numbers
	c abase,"a>b",Xabase,,,hidden
//;
//; (number)	( d1 adr1 -- d2 adr2 )		(p)
//;	accumulate converted numeric text beginning at adr1+1 w.r.t. @BASE
//;	double d1, ret new figure d2 and address of 1st unconvertible digit.
//;	by {[number]}, {e\stg}. identical to {convert} of 79-forth.
	c pnumberq,"(number)",Xpnumberq
//;
//; [number]  	( a -- d f )			(p)	hidden
//;	Convert counted string at a, count-byte ignoring, to a signed double.
//;	flag f := 0 if conversion successful, else code of 1st in-valid char.
//;	executes {[n]} standard procedure; deferred variant may be revectored.
//;	F4A: implemented as a deferred word if asm-cons _nv_ =/= 0.
nv_	c bnumberb,"[number]",Xbnumberb,,,hidden
_nv	c bnumberb,"[number]",domdfer,,,hidden
_nv	    .long bnumberbv-D,bbnumberb+cfa2pfa
//;
//; [n]		( a -- d f )			(p)	hidden
//;	default word of deferred {[number]}.
//;	Convert counted string at a, count-byte ignoring, to a signed double
//;	number, using the base returned from {a>b}. the position of the last
//;	radix fraction marker (e.g. decimal point) encountered in the string 
//;	will be returned to {dpl}, with no other effect; -1 if none found. 
//;	flag f := 0 if conversion successful, else code of 1st in-valid char.
//;	Multiplier postfix characters:
//;		k 10^3	t 10^12		K 2^10	T 2^40
//;		m 10^6	p 10^15		M 2^20	P 2^50
//;		g 10^9			G 2^30
//;	Note: 't','p' doubles, only. all require @base < ascii(postfix)
//;	F4A: compiled if asm-cons _nv_ =/= 0, else cpl'd as {[number]}.
_nv	c bbnumberb,"[n]",Xbnumberb,,,hidden
//;
//; number  	( a -- d | abort-0 )		(p)
//;	Convert counted string at a, count-byte ignoring, to a signed double 
//;	number, using sign and base returned from {a>b}. the position of the
//;	last radix fraction marker (e.g. decimal point) encountered in the 
//;	string will be returned to {dpl}, with no other effect; -1 if none 
//;	found. Abort w. message #0 if conversion could not be completed. the
//;	numeric string may be postfixed by a multiplier char - re {[number]}.
	c number,"number",Xnumber	//<;Cnumber by interpret)
//;
//; hex		( -- )				(p)
//;	Set the numeric conversion radix, uvari {base}, for sedecimal in-/output.
//;	re {b.}, {db.} for numeric output at temporary {base} setting.
	c hex,"hex",Xhex
//;
//; decimal	( -- )				(p)
//;	Set the numeric conversion radix, uvari {base}, for decimal in-/output.
//;	re {b.}, {db.} for numeric output at temporary {base} setting.
	c decimal,"decimal",Xdecimal

//;
//; ---- rstack, multiple cells ----
//;

//;
//; nrdrop	( n -- )(R: ../n).. -- )	(p)	hidden
//;	drop n cells from return-stack. re {rdrop}, {2rdrop}
	c nrd,"nrdrop",Xnrdrop,,,hidden
//;
//; n>r		( n1..nn n -- )(R: -- nn..n1 )	(p)	hidden
//;	transfer n cells from tos to tor, little endian byte order.
	c n2r,"n>r",Xntor,,,hidden
//;
//; nr>		( n -- n1..nn )(R: nn..n1 -- )	(p)	hidden
//;	transfer n cells from tor to tos, little endian byte order.
	c r2n,"nr>",Xnrfrom,,,hidden
//;
//; 4r		( -- q )(R: q -- q )		(p)	hidden
//;	Push top quad of return-stack to @tos. 'big endian' cells order!
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0; re {4r>}, {4>r}
_mx	c rx4,"4r",Xrx4,,,hidden
//;
//; 4r>		( -- q )(R: q -- )		(p)	hidden
//;	push quad from return-stack to @tos, drop from rstack.
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0; re {4>r}, {4r}
_mx	c rg4,"4r>",Xrfrom4,,,hidden
//;
//; 4>r		( q -- )(R: -- q )		(p)	hidden
//;	push quad to return-stack and drop @tos. 'big endian' cells order!
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0; re {4r>}, {4r}
_mx	c gr4,"4>r",Xtor4,,,hidden
//;
//; 2rdrop  	( d -- )			(p)
//;	remove double from data-stack. { rdrop rdrop }, re {nrdrop}.
	c rdrop2,"2rdrop",Xrdrop2
//;
//; 2>r		( d -- )(R: -- d )		(p)
//;	drop @tos and push to return-stack. re {2r>}; 'big endian' cells order!
	c gr2,"2>r",Xgreatr2
//;
//; 2>rr   	( d -- d )(R: -- d )		(p)
//;	Push duplicate @tos to return-stack. re {2r}, {2>r}, {2r>}
	c tr2,"2>rr",Xtrr2
//;
//; 2r>		( -- d )(R: d -- )		(p)
//;	Pop top double from return-stack to @tos. re {2>r}, {2r}, {2>rr}
	c rg2,"2r>",Xrgreat2
//;
//; 2r		( -- d )(R: d -- d )		(p)
//;	Push top double of return-stack to @tos. re {2>rr}; 'big endian' cells!
	c rx2,"2r",Xrx2

//;
//; ---- multiple cells' arith ----
//;

// -- counted int --
//    optionally cpl`d if asm-cons _mx_ & _bn_ =/= 0.

//;
//; big		( .nn1. N M -- .nn2. M )		hidden
//;	counted integer logic & arithmetic opr
//; n:
//;	n->n   	( .nn1. N M -- .nn2. M )	
//;	un->un 	( .nn1. N M -- .nn2. M )	
//;	nrdrop	( n -- )(R: ../n).. -- )	
//;	n>r	( n1..nn n -- )(R: -- nn..n1 )	
//;	nr>	( n -- n1..nn )(R: nn..n1 -- )	
//;	nminus  ( n1..nn n -- m1..mn n )
//;	nnot   	( n1..nn n -- m1..mn n )	
//;	n1-	( n1..nn N -- m1..mn N )
//;	n1+	( n1..nn N -- m1..mn N )
//; n,s
//;	ns+	( n1..nn N -- m1..mn N )
//;	nshift 	( um .. u0 M +n -- n0' .. nm' ov M' )
//;	nshift 	( um .. u0 M -n -- ov n0' .. nm' M' )
//; n,n
//;	n+	( n1..nn N m1..mm M -- k1..kn K )

//;
//; n->n   	( .nn1. N M -- .nn2. M )	(p)	hidden
//;	truncate/signed extend from +N cells to +M cells size
//;	F4A: optionally in kernel if asm-cons _mx_ & _bn_ =/= 0.
_bn	c n2n,"n->n",Xn2n,,,hidden
//;
//; nminus  	( n1..nn n -- m1..mn n )	(p)	hidden
//;	2-s cpl of group of n cells on tos
//;	F4A: optionally in kernel if asm-cons _mx_ & _bn_ =/= 0.
_bn	c nmi,"nminus",Xnminus,,,hidden
//;
//; nnot   	( n1..nn n -- m1..mn n )	(p)	hidden
//;	1-s cpl of group of n cells on tos
//;	F4A: optionally in kernel if asm-cons _mx_ & _bn_ =/= 0.
_bn	c nnot,"nnot",Xnnot,,,hidden
//;
//; n1-		( n1..nn N -- m1..mn N )	(p)	hidden
//;	decrement multi precision integer of n cells on tos by one
//;	F4A: optionally in kernel if asm-cons _mx_ & _bn_ =/= 0.
_bn	c n1m,"n1-",Xn1m,,,hidden
//;
//; n1+		( n1..nn N -- m1..mn N )	(p)	hidden
//;	increment multi precision integer of n cells on tos by one
//;	F4A: optionally in kernel if asm-cons _mx_ & _bn_ =/= 0.
_bn	c n1p,"n1+",Xn1p,,,hidden
//;
//; ns+		( n1..nn N -- m1..mn N )	(p)	hidden
//;	add sing to multi precision integer of n cells on tos
//;	F4A: optionally in kernel if asm-cons _mx_ & _bn_ =/= 0.
_bn	c nsp,"ns+",Xnsp,,,hidden
//;
//; nshift 	( um .. u0 M +n -- n0' .. nm' ov M' ) (p) hidden
//;		( um .. u0 M -n -- ov n0' .. nm' M' ) (p) hidden
//;	shift left M cells by +n bits or, shift right M ells by -n bits;
//;	ret ov w. overflowing bits, shifted cells, adjusted cells count M'.
//;	M, |n| not limited; F4A: optionally in kernel if asm-cons _mx_ =/= 0.
//;	F4A: optionally in kernel if asm-cons _mx_ & _bn_ =/= 0.
_bn	c nsh,"nshift",Xnshift,,,hidden

// -- exp --

//;
//; factorial  	( u -- ud )			(p)
//;	return double ud as the factorial of u, -1 < u < 21, 0 if out of range.
//;	F4A: optionally in kernel if asm-cons _fc_ =/= 0.
_fc	c fac,"factorial",Xfactorial	//[ms]
//;
//; qsqrt  	( q1 -- q2 j )			(p)	hidden
//;	q2 := +ve sqare root of quad |uq1|; j := -1 if q1 signed < 0.
//;	q2 is nearest integer the sqare of which is lower or equal |q1|*(2j-1).
//;	F4A: optionally in kernel if asm-cons _sqrf_ =/= 0.
_sq	c uqsqrt,"qsqrt",Xuqsqrt,,,hidden//[mq]
//;
//; dsqrt  	( d1 -- d2 j )			(p)
//;	d2 := +ve sqare root of double |d1|; j := -1 if d1 signed < 0.
//;	d2 is nearest integer the sqare of which is lower or equal |ud1|*(2j-1).
//;	note: d2 m.s.cell always zero, left in place for consistency.
//;	F4A: optionally in kernel if asm-cons _sqrf_ =/= 0.
_sq	c udsqrt,"dsqrt",Xudsqrt	//[md]
//;
//; mq^   	( n1 +n2 -- q3 )		(p)	hidden
//;	return quad q3 as signed n1 to the power of +n2.
//;	q3 := 0 if n2 < 0; exponent truncated to 127.
//;	F4A: optionally in kernel if asm-cons _mx_ and _hx_ =/= 0.
_mx _hx	c mqpow,"mq^",Xmqpow,,,hidden	//[mss]
//;
//; umq^   	( u1 +n2 -- uq3 )		(p)	hidden
//;	return unsigned quad uq3 as u1 to the power of u2,
//;	uq3 := 0 if n2 < 0; exponent truncated to 127.
//;	F4A: optionally in kernel if asm-cons _mx_ and _hx_ =/= 0.
_mx _hx	c umqpow,"umq^",Xumqpow,,,hidden//[mss]
//;
//; uq^   	( q1 +n2 -- q3 )		(p)	hidden
//;	return quad q3 as unsigned q1 to the power of +n2.
//;	q3 := 0 if n2 < 0.
//;	F4A: optionally in kernel if asm-cons _mx_ and _hx_ =/= 0.
_mx _hx	c qpowu,"uq^",Xqpowu,,,hidden	//[mqs]
//;
//; u^		( +n1 n2 -- +d3 )		(p)
//;	return double +d3 as +ve n1 to the pover of n2,
//;	d3 := 0 if n2 < 0
//;	F4A: optionally in kernel if asm-cons _sqrf_ =/= 0.
_sq	c upow,"u^",Xupow		//[mss]
//;
//; -1^		( n1 -- n2 )			(p)
//;	n2 := n1-th power of -1.
//;	F4A: optionally in kernel if asm-cons _al_ =/= 0.
_al	c m1p,"-1^",Xm1p		//[mss]
//;
//; 2^		( +n -- dn )			(p)
//;	double dn := n-th power of 2.
//;	F4A: optionally in kernel if asm-cons _al_ =/= 0.
_al	c np2,"2^",Xnp2			//[mss]
//;
//; ^2		( n -- d )			(p)
//;	double square of a sing.
//;	F4A: optionally in kernel if asm-cons _sq_ =/= 0.
_sq	c sq,"^2",Xsq			//[ms]
//;
//; u^2		( u -- ud )			(p)
//;	double unsigned square of a sing.
//;	F4A: optionally in kernel if asm-cons _sq_ =/= 0.
_sq	c usq,"u^2",Xusq		//[mss]
//;
//; d^2		( d -- q )			(p)
//;	quad square of a double.
//;	F4A: optionally in kernel if asm-cons _sq_ =/= 0.
_sq	c dh2,"d^2",Xdsq		//[md]
//;
//; uq^2    	( uq -- uo )			(p)
//;	8-cells unsigned square of a quad.
//;	F4A: optionally in kernel if asm-cons _sq_ =/= 0.
_sq	c dq2,"uq^2",Xuqsq		//[mq]

// -----------------------------------------------------------------------------
//;
//; div    	( .nn. -- .mm. )
//;	F4 integers dividing & sqare root words
//; d:
//;	dsqrt  	( d1 -- d2 j )		
//; q:
//;	qsqrt  	( q1 -- q2 j )		
//; n:
//;	nshift	( um .. u0 M -n -- ov n0' .. nm' M' )
//; s/s:
//;	>>	( u1 u -- u2 )		
//;	2/	( u1 -- u2 )		2/,4/,8/,16/
//;	/	( n1  n2 -- quot )	
//;	/mod  	( n1 n2 -- rem quot )	
//;	m/	( d n1 -- n2 n3 )	
//;	mod	( n1 n2 -- mod )	
//;	u/mod  	( ud1 u2 -- u3r u4q )	
//; d/s:
//;	2>>	( d1 u -- d2 )		
//;	a2>>	( d1 u -- d2 )		
//;	d2/	( d1 -- d2 )
//;	dm/	( t1 n2 -- d )		
//;	m/mod  	( ud1 u2 -- u3 ud4 )	
//;	md/mod	( d1 n2 -- n3 d4 )	
//; d/d:
//;	d/	( d1 d2 -- d3 )		
//;	d/mod  	( d1 d2 -- d3 d4 )	
//;	df/mod	( d1 d2 -- d3r d4q )	
//; 	dgcd   	( d1 q2 -- d3 )		
//;	dmod   	( d1 d2 -- d3 )		
//;	ud/ 	( ud1 ud2 -- ud4 )	
//;	udmod 	( ud1 ud2 -- ud3 )	
//; q/s:
//;	4>>	( q1 n -- q2 )		
//;	q2/	( q1 -- q2 )		
//;	uqsm/  	( q1 n2 -- n3 q4 )	
//; q/d:
//;	qfm/mod	( q1 d2 -- d3r d2q )	
//;	qm/mod	( q1 d2 -- d3 q4 )	
//;	uqd/mod	( uq1 ud2 -- ud3 ud4 )	
//;	uqm/mod	( +q1 ud2 -- ud3 uq4 )	
//; q/q:
//;	q/	( q1 q2 -- qq )		
//;	q/mod  	( q1 q2 -- qr qq )	
//;	qf/mod  ( q1 q2 -- qr qq )	
//;	qgcd   	( q1 q2 -- q3 )		
//;	qmod   	( q1 q2 -- qr )		
//;	uq/ 	( uq1 uq2 -- uqq )	
//;	uq/mod 	( uq1 uq2 -- uqr uqq )	

// --

//;
//; uqsm/  	( q1 n2 -- n3 q4 )		(p)	hidden
//;	unsigned divide quad by sing, leave sing rmd n3 & quad quot.
//;	div by zero ret quot and rmd w. all bits set, i.e. max value.
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c uqsmby,"uqsm/",Xuqsmby,,,hidden	//[dqs]
//;
//; q2/		( q1 -- q2 )			(p)	hidden
//;	signed divide quad by two, 'round near zero' division.
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c q2d,"q2/",Xq2div,,,hidden		//[dqs]
//;
//; 4>>		( q1 n -- q2 )			(p)	hidden
//;	shift quad q1 right by +ve n bits, i.e. unsigned floored div by 2^n.
//;	no overflow result, q2 := 0 if unsigned n > 127
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c rsh4,"4>>",Xqrshift,,,hidden		//[dqs]
//;
//; d2/		( d1 -- d2 )			(p)
//;	signed divide double by two, 'round near zero' division.
//;	F4A: optionally in kernel if asm-cons _sb_ =/= 0.
_sb	c d2d,"d2/",Xd2div			//[dds]
//;
//; a2>>		( d1 u -- d2 )			(p)
//;	arithmetic (sign bit copying) shift right double by u mod 64.
//;	F4A: optionally in kernel if asm-cons _sb_ =/= 0.
_sb	c rs2a,"a2>>",Xa2rsh			//[dds]
//;
//; 2>>		( d1 u -- d2 )			(p)
//;	logic shift right double by u mod 64.
//;	F4A: optionally in kernel if asm-cons _sb_ =/= 0.
_sb	c rsh2,"2>>",X2rsh			//[dds]
//;
//; qgcd   	( q1 q2 -- q3 )			(p)
//;	returns the greatest common denominator q3 of the absolute value of two signed 
//;	quads. If either number is zero, returns the absolute value of the other one.
//;	F4A: optionally in kernel if asm-cons _rx_ =/= 0.
_rx	c qgcd,"qgcd",Xqgcd			//[dqq]
//;
//; dgcd   	( d1 d2 -- d3 )			(p)
//;	returns the greatest common denominator d3 of the absolute value of two signed 
//;	doubles. If either number is zero, returns the absolute value of the other one.
//;	F4A: optionally in kernel if asm-cons _rx_ =/= 0.
_rx	c dgcd,"dgcd",Xdgcd			//[ddd]
//;
//; uq/ 	( uq1 uq2 -- uqq )		(p)	hidden
//;	unsigned div quad uq1 by quad uq2, leave quad quotient. re {uq/mod}
//;	div 0: ret uqr := -1, uqq = -1; fast exit if uq2=dvd = 1
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c uqby,"uq/",Xuqby,,,hidden		//[dqq]
//;
//; uq/mod 	( uq1 uq2 -- uqr uqq )		(p)	hidden
//;	unsigned div quad uq1 by quad uq2, leave quad remainder (@nos) and quotient.
//;	div 0: ret uqr := -1, uqq = -1; fast exit if uq2=dvd = 1
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c uqbym,"uq/mod",Xuqbym,,,hidden	//[dqq]
//;
//; qmod   	( q1 q2 -- qr )			(p)	hidden
//;	signed div quad by quad, leave quad remainder. -re- {q/mod}
_mx	c qmod,"qmod",Xqmod,,,hidden		//[dqq]
//;
//; q/	 	( q1 q2 -- qq )			(p)	hidden
//;	signed div quad by quad, leave quad quotient. -re- {q/mod}
_mx	c qby,"q/",Xqby,,,hidden		//[dqq]
//;
//; q/mod  	( q1 q2 -- qr qq )		(p)	hidden
//;	signed div quad by quad, leave quad remainder (@nos) and quotient.
//;	'round near zero' mode, sgn(qot) = sgn(dvd xOR dvs), sgn(rmd) = sgn(dvd).
_mx	c qbym,"q/mod",Xqbym,,,hidden		//[dqq]
//;
//; qf/mod  	( q1 q2 -- qr qq )		(p)	hidden
//;	floored, signed div quad by quad, leave quad remainder (@nos) and quotient.
//;	signed 'round near zero' division but, if qq < 0 ret qq:=qq-1, qr:=qr+q2.
//;	div 0, 0/0: 1,0; +ve/0: +max,+max; -ve/0: hex 80..00,-max.
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0. (slow, bitwise algorithm)
_mx	c qfbym,"qf/mod",Xqfbym			//[dqq]
//;
//; qfm/mod  	( q1 d2 -- d3r d2q )		(p)	hidden
//;	floored division of quad by double, double remainder & quotient
//;	 Dividend Divisor   Quotient Remainder
//;	 -------- ------- | -------- ---------
//;	     10      7    |    1        3
//;	    -10      7    |   -2        4
//;	     10     -7    |   -2       -4
//;	    -10     -7    |    1       -3
//;	 dvs = 0 overflow values:
//;		dvd = 0        1        0
//;		dvd > 0    quot & rmd at +ve maximum
//;		dvd < 0    quot & rmd at -ve maximum
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c qdfbym,"qfm/mod",Xqfmbym,,,hidden	//[dqd]
//;
//; df/mod  	( d1 d2 -- d3r d4q )		(p)
//;	floored division of doubles, double remainder and double quotient
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0. - re {qfm/mod}
_dx	c dfbym,"df/mod",Xdfbym			//[ddd]
//;
//; qm/mod	( q1 d2 -- d3 q4 )		(p)	hidden
//;	signed divide quad q1 by double d2, ret quad quotient q4, double rmd d3.
//;	result signs: +/+ => q+,r+; +/- => q-,r+; -/- => q+,r-; -/+ => q-,r-.
//;	div-0 returns zero rmd d3, max. quot w. sign of q1, or q4 := 1 if q1=0.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c qmbym,"qm/mod",Xqmbym,,,hidden	//[dqd]
//;
//; uqm/mod	( +q1 ud2 -- ud3 uq4 )		(p)	hidden
//;	unsigned divide quad +q1 by ud2, ret quad quotient uq4, double rmd ud3.
//;	div 0: ret ud3:=0, uq4:=-1; fast exit if dvd = 1
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c uqmbym,"uqm/mod",Xuqmbym,,,hidden	//[dqd]
//;
//; uqd/mod	( uq1 ud2 -- ud3 ud4 )		(p)	hidden
//;	unsigned divide quad by double, ret rmd ud3, quot ud4
//;	div 0: ret ud4 := -1., ud3 := uq1.lo
//;	(bit-wise div. from F-PC, R.L.Smith; modified for F4/L4)
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c uqdbym,"uqd/mod",Xuqdbym,,,hidden	//[dqd]
//;
//; dm/		( t1 n2 -- d )			(p)	hidden
//;	signed div treble t1 by sing n2 to double quotient. re {dm*}.
	c dmby,"dm/",Xdmby,,,hidden		//[dds]
//;
//; udmod 	( ud1 ud2 -- ud3 )		(p)
//;	unsigned divide doubles, leave rmd ud3
//;	F4A: optionally in kernel if asm-cons _dx_ and _hx_ =/= 0.
_dx _hx	c udmod,"udmod",Xudmod			//[ddd]
//;
//; ud/ 	( ud1 ud2 -- ud4 )		(p)
//;	unsigned divide doubles, leave qot ud4
//;	F4A: optionally in kernel if asm-cons _dx_ _hx_ =/= 0.
_dx _hx	c udby,"ud/",Xudby			//[ddd]
//;
//; ud/mod 	( ud1 ud2 -- ud3 ud4 )		(p)
//;	unsigned divide doubles, leave qot ud4 & rmd ud3
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c udbym,"ud/mod",Xudbym			//[ddd]
//;
//; dmod   	( d1 d2 -- d3 )			(p)
//;	divide doubles d1 by d2, leave signed double rmd d3, re {d/mod}
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c dmod,"dmod",Xdmod			//[ddd]
//;
//; d/		( d1 d2 -- d3 )			(p)
//;	divide doubles d1 by d2, leave signed double qot d3, re {d/mod}
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c dby,"d/",Xdby				//[ddd]
//;
//; d/mod  	( d1 d2 -- d3 d4 )		(p)
//;	divide doubles d1 by d2, leave signed doubles rmd d3 and qot d4
//;	div-0: d3=0 and, if 0/0 d4=1, +ve/0: d4=+max, -ve/0: d4=-max.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c dbym,"d/mod",Xdbym			//[ddd]
//;
//; md/mod  	( d1 n2 -- n3 d4 )		(p)
//;	signed double d by sing n, ret double quotient d4 and remainder n3.
//;	fast trivial case div/+-1; ret 1.,0 from 0/0, dmax,max from d1/0.
	c mdbm,"md/mod",Xmdbym			//[dds]
//;
//; m/mod  	( ud1 u2 -- u3 ud4 )		(p)
//;	unsigned double quotient ud4 and remainder u3, from double ud1 by sing u2.
//;	fast trivial case div/1; ret 1.,0 from 0/0, udmax,umax from ud1/0.
	c ubym,"m/mod",Xumbym			//[dds]

//;
//; muldiv	( .n. .m. .o. -- .xx. )
//;	F4 integer scaling opr's
//; s*s/s
//;	*/	( n1 n2 n3 -- n4 )	
//;	*/mod  	( n1 n2 n3 -- n4 n5 )	
//; d*s/s
//;	m*/mod  ( d1 n2 n3 -- n4 d5 )	
//;	m*/	( d1 n2 n3 -- d4 )	
//; d*d/d
//;	d*/mod  ( d1 d2 d3 -- d4 d5 )	
//;	ud*/mod ( ud1 ud2 ud3 -- ud4 ud5 )

// --

//;
//; ud*/mod  	( ud1 ud2 ud3 -- ud4 ud5 )	(p)
//;	unsigned mul doubles ud1 by ud2 to quad, unsigned div by ud3,
//;	ret remainder ud4 and quot ud5.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c udmbym,"ud*/mod",Xudmbym
//;
//; d*/mod  	( d1 d2 d3 -- d4 d5 )		(p)
//;	signed mul doubles d1 by d2 to quad, signed div by d3,
//;	ret remainder d4 and quot d5.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c dmbym,"d*/mod",Xdmbym
//;
//; m*/mod  	( d1 n2 n3 -- n4 d5 )		(p)
//;	signed mul double d1 by sing n2 to treble, div by n3,
//;	ret sing remainder n4 and double quot d5. re {m*/}.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c umbym,"m*/mod",Xmmbym
//;
//; m*/		( d1 n2 n3 -- d4 )		(p)
//;	signed mul double d1 by sing n2 to treble, div by n3, ret double quot.
//;	fast xec for trivial cases mul/div by +/- 1 and 0. re {m*/mod}.
	c mmu,"m*/",Xmmulby

// -----------------------------------------------------------------------------
//;
//; mul		( .nn. -- .mmm. )
//;	F4 integers multiplying & exponentiation words
//; s, s^
//;	2^	( +n -- dn )		
//;	^2	( n -- d )
//;	factorial ( u -- ud )		
//;	u^2	( u -- ud )
//; d, d^
//;	2<<	( d1 u -- d2 )		
//;	dslb   	( d1 -- d1 n )		
//;	d^2	( d -- q )
//; q, q^
//;	4<<	( q1 n -- q2 )		
//;	mq^   	( n1 +n2 -- q3 )	
//;	qslb	( q1 -- q1 n )
//;	umq^   	( u1 +n2 -- uq3 )	
//;	uq^2  	( uq -- uo )
//; n
//;	nshift 	( um .. u0 M +n -- n0' .. nm' ov M' )
//; s*s
//;	2*	( u1 -- u2 )		2*,4*,8*,16*
//;	-1^	( n1 -- n2 )
//;	*	( n1 n2 -- n3 )		
//;	m*	( n1 n2 -- d )		
//;	u*	( u1 u2 -- ud )		
//;	u^	( +n1 n2 -- +d3 )	
//; d*s
//;	d2*	( d1 -- d2 )		
//;	dm*	( d1 n2 -- t3 )		
//;	udm*   	( ud1 u2 -- ud3 ovf )	
//; d*d
//;	d*	( d1 d2 -- d )		
//;	md*	( d1 d2 -- q )		
//;	ud*	( ud1 ud2 -- ud )	
//;	umd*	( ud1 ud2 -- uq )	
//; q*s
//;	q2*	( q1 -- q2 )		
//;	uqsm*  	( q1 n1 -- q2 ovf )	
//; q*d
//;	uqd*   	( uq1 ud1 -- uq2 ud2 )	
//; q*q
//;	q* 	( q1 q2 -- q2 qovf )	
//;	uq*	( q1 q2 -- q2 qovf )	

// --

//;
//; qslb	( q1 -- q1 n )			(p)	hidden
//;	ret no. n of bits by which +/- q1 can be shifted left w/o change of sign.
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c qslb,"qslb",Xqslb,,,hidden
//;
//; dslb   	( d1 -- d1 n )			(p)	hidden
//;	ret no. n of bits by which d1 may be shifted left w/o change of sign.
//;	F4A: optionally in kernel if asm-cons _sb_ =/= 0.
_sb	c dslb,"dslb",Xdslb,,,hidden
//;
//; 4<<		( q1 n -- q2 )			(p)	hidden
//;	shift quad q1 left by +ve n bits, i.e. mul by 2^n.
//;	no overflow result, q2 := 0 if unsigned n > 127
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c lsh4,"4<<",Xqlshift,,,hidden
//;
//; 2<<		( d1 u -- d2 )			(p)
//;	logic shift left double by u mod 64
//;	F4A: optionally in kernel if asm-cons _sb_ =/= 0.
_sb	c lsh2,"2<<",X2lsh
//;
//; q2*		( q1 -- q2 )			(p)	hidden
//;	multiply quad by two
//;	F4A: optionally in kernel if asm-cons _sb_ =/= 0.
_sb	c q2m,"q2*",Xq2mul,,,hidden
//;
//; d2*		( d1 -- d2 )			(p)
//;	multiply double by two
//;	F4A: optionally in kernel if asm-cons _sb_ =/= 0.
_sb	c d2m,"d2*",Xd2mul
//;
//; q* 	 	( q1 q2 -- q2 qovf )		(p)	hidden
//;	signed multiply quad by quad, ret quad prod & quad ovf = 8 cells product.
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c qmul,"q*",Xqmul,,,hidden	//		[mqq]
//;
//; uq*		( q1 q2 -- q2 qovf )		(p)	hidden
//;	unsigned multiply quad by quad, ret quad prod & quad ovf = 8 cells product.
//;	does{ 8 nndup 3drop uqd* 6 nnswap drop 2sdrop uqd* 0. -3 2roll 8 n+ drop }
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c uqmul,"uq*",Xuqmul,,,hidden	//		[mqq]
//;
//; uqd*   	( uq1 ud1 -- uq2 ud2 )		(p)	hidden
//;	unsigned multiply quad by double, ret six cells prod.
//;	does{ 6 nndup 2drop uqsm* 5 nnswap drop sdrop uqsm* 0 -5 roll 6 n+ drop }
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c uqdmul,"uqd*",Xuqdmul,,,hidden//		[mqd]
//;
//; uqsm*  	( q1 n1 -- q2 ovf )		(p)	hidden
//;	unsigned multiply quad by sing, leave quintuple prod, i.e. q2 & overflow
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c uqsmmul,"uqsm*",Xuqsmmul,,,hidden//		[mqs]
//;
//; umd*	( ud1 ud2 -- uq )		(p)
//;	multiply doubles, leave quad
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c umdm,"umd*",Xumdmul		//		[mdd]
//;
//; md*		( d1 d2 -- q )			(p)
//;	signed multiply doubles to quad.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c mdm,"md*",Xmdmul		//		[mdd]
//;
//; udm*   	( ud1 u2 -- ud3 ovf )		(p)
//;	unsigned multiply double by sing to double,
//;	ovf is m.s. cell of treble product. re {dm*}.
	c udmm,"udm*",Xudmmul		//		[mds]
//;
//; ud*		( ud1 ud2 -- ud )		(p)
//;	unsigned multiply doubles. from quad by {umd*}.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c udm,"ud*",Xudmul		//		[mdd]
//;
//; d*		( d1 d2 -- d )			(p)
//;	signed multiply doubles. from quad by {md*}.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c dm,"d*",Xdmul			//		[mdd]
//;
//; dm*		( d1 n2 -- t3 )			(p)	hidden
//;	signed mul double d1 by sing n2 to treble product. re {udm*}.
//;	fast xec for trivial cases mul by +/- 1 and 0.
	c dmm,"dm*",Xdmmul,,,hidden	//		[mds]
//;
//; u*		( u1 u2 -- ud )			(p)
//;	unsigned multiply sings, ret double prod.
	c umu,"u*",Xumul		//		[mss]
//;
//; m*		( n1 n2 -- d )			(p)
//;	signed multiply sings to double.
	c mml,"m*",Xmmul		//		[mss]

// -- negate, 1-s and 2-s cmplement --------------------------------------------
//;
//; sub    	( .n. .m. -- .x. )
//;	F4 integer subtraction & sign conversion
//; s:
//;	not    	( n1 -- n2 )		
//;	1abs    ( n1 -- n2 )		
//;	+-	( n1 n2 -- n3 )		
//;	abs     ( n1 -- n2 )		
//;	minus  	( n1 -- n2 )		
//; d:
//;	2not   	( q -- q' )		
//;	d+-	( d n -- d )		
//;	dabs   	( d1 -- d2 )		
//;	dminus 	( d1 -- d2 )		
//;	nos-dminus ( d1 dx -- d2 dx )	
//;	dsgn   	( dn -- n )		
//; o:
//;	8not   	( o1 -- o2 )		
//;	ominus 	( o1 -- o2 )		
//; q:
//;	4not   	( q -- q' )		
//;	nos-4not   ( q1 qx -- q2 qx )	
//;	q+-	( q n -- q' )		
//;	qabs   	( q -- |q| )		
//;	qminus 	( q -- q' )		
//;	nos-qminus ( q1 qx -- q2 qx )	
//; n:
//;	nminus 	( n1..nn n -- m1..mn n )
//;	nnot   	( n1..nn n -- m1..mn n )
//; s-s:
//;	1-	( n1 -- n2 )	1-,2-,3-,4-,8-
//;	-	( n1 n2 -- n3 )		
//;	over-  	( n1 n2 -- n1 n3 )	
//;	swap-   ( n1 n2 -- n3 )
//; d-d:
//;	d1-	( d -- d' )	
//;	d-	( d1 d2 -- d3 )	
//; q-q:
//;	q-	( q1 q2 -- q )	
//; n-s:
//;	n1-	( n1..nn N -- m1..mn N )

//;
//; q+-		( q n -- q' )			(p)
//;	apply sign of n to q; { 0< if qminus endif }.
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c qpm,"q+-",Xqpm
//;
//; d+-		( d n -- d )			(p)
//;	negate d if n -ve; { 0< if dminus endif }.
	c dplusdash,"d+-",Xdplusdash
//;
//; qabs   	( q -- |q| )			(p)	hidden
//;	convert quad to +ve
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c qa,"qabs",Xqabs,,,hidden	
//;
//; dabs   	( d1 -- d2 )			(p)
//;	negate d1 if -ve. { dup d+- }
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c dabs,"dabs",Xdabs
//;
//; ominus 	( o1 -- o2 )			(p)	hidden
//;	negate octal integers
//;	F4A: 4th header optionally in kernel if asm-cons _hx_ and _dx_ =/= 0.
_dx _hx	c ominus,"ominus",Xominus,,,hidden
//;
//; nos-qminus	( q1 qx -- q2 qx )		(p)	hidden
//;	negate quad q1 @nos. (double ranum support)
//;	F4A: 4th header optionally in kernel if asm-cons _hx_ and _dx_ =/= 0.
_dx _hx	c qnn,"nos-qminus",Xqnegaten,,,hidden
//;
//; nos-dminus	( d1 dx -- d2 dx )		(p)	hidden
//;	negate double d1 @nos. (double ranum support)
//;	F4A: 4th header optionally in kernel if asm-cons _hx_ and _dx_ =/= 0.
_dx _hx	c dnn,"nos-dminus",Xdminusn,,,hidden
//;
//; qminus 	( q -- q' )			(p)	hidden
//;	negate a quad
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c qm,"qminus",Xqminus,,,hidden
//;
//; dminus 	( d1 -- d2 )			(p)
//;	negate double d1.
//;	{ swap minus swap over if 1+ endif minus }
	c dminus,"dminus",Xdminus
//;
//; nos-4not	( q1 qx -- q2 qx )		(p)	hidden
//;	one's cpl of quad q1 @nos. (double ranum support)
//;	F4A: 4th header optionally in kernel if asm-cons _hx_ and _dx_ =/= 0.
_dx _hx	c n4n,"nos-4not",Xnot4n,,,hidden
//;
//; 8not   	( o1 -- o2 )			(p)	hidden
//;	q2 := one-s complement of eight-cells item q1
//;	F4A: 4th header optionally in kernel if asm-cons _hx_ and _dx_ =/= 0.
_dx _hx	c n8,"8not",Xnot8,,,hidden
//;
//; 4not   	( q -- q' )			(p)	hidden
//;	one-s complement of a quad
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c n4,"4not",Xqnot,,,hidden
//;
//; 2not   	( q -- q' )			(p)
//;	one-s complement of a quad
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c n2,"2not",Xnot2

// -----------------------------------------------------------------------------
//;
//; add		( .n. .m. -- .x. )
//;	F4 integer addition & format conversion
//; s:
//;	s->q   	( n -- q )		
//;	s->d   	( n -- d )		
//; d:
//;	d->q   	( d -- q )		
//;	dsgn   	( dn -- n )
//; o:
//; n:
//;	un->un 	( .un1. N M -- .un2. M )
//;	n->n   	( .nn1. N M -- .nn2. M )
//; s+s:
//;	1+	( n1 -- n2 )	1+,2+,3+,4+,8+
//;	+	( n1 n2 -- n3 )		
//; d+s:
//;	u+      ( ud1 u2 -- ud3 )	
//;	m+      ( d1 n2 -- d3 )		
//; d+d:
//;	d1+	( d -- d' )		
//;	d+	( d1 d2 -- d3 )		
//; q+s:
//;	qm+	( q n -- q' )		
//; q+q:
//;	nos-q1+	( q1 qx -- q2 qx )	
//;	q1+	( q -- q' )		
//;	q+	( q1 q2 -- q )		
//; o+o:
//;	o1+	( o1 -- o2 )		
//;	o+	( o1 o2 -- o )		
//; n+s:
//;	n1+	( n1..nn N -- m1..mn N )
//;	ns+	( n1..nn N -- m1..mn N )

// --

//;
//; o+		( o1 o2 -- o )			(p)	hidden
//;	add two octal integers
//;	F4A: 4th header optionally in kernel if asm-cons _hx_ and _mx_ =/= 0.
_mx _hx	c op,"o+",Xoplus,,,hidden
//;
//; o1+		( o1 -- o2 )			(p)	hidden
//;	increment eight-cells item by one
//;	F4A: 4th header optionally in kernel if asm-cons _hx_ and _mx_ =/= 0.
_mx _hx	c o1p,"o1+",Xo1p,,,hidden
//;
//; nos-q1+	( q1 qx -- q2 qx )		(p)	hidden
//;	increment quad @nos by one. (double ranum support)
//;	F4A: 4th header optionally in kernel if asm-cons _hx_ and _mx_ =/= 0.
_mx _hx	c q1pn,"nos-q1+",Xq1pn,,,hidden
//;
//; q1+		( q -- q' )			(p)	hidden
//;	increment quad by one
//;	F4A: 4th header optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c q1p,"q1+",Xq1p,,,hidden
//;
//; d1+		( d -- d' )			(p)	hidden
//;	increment double by one
//;	F4A: optionally in kernel if asm-cons _dx_ and _hx_ =/= 0.
_dx _hx	c d1p,"d1+",Xd1p,,,hidden
//;
//; q+		( q1 q2 -- q )			(p)	hidden
//;	add two quads
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c qp,"q+",Xqplus,,,hidden
//;
//; qm+		( q n -- q' )			(p)	hidden
//;	sign extend and add sing to quad.
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c qmp,"qm+",Xqmplus,,,hidden
//;
//; d+		( d1 d2 -- d3 )			(p)
//;	add doubles. { 3 roll m+ rot + }
	c dplus,"d+",Xdplus
//;
//; u+      	( ud1 u2 -- ud3 )		(p)
//;	zero-extend and add sing u2 to double ud1.
	c uplus,"u+",Xuplus
//;
//; m+      	( d1 n2 -- d3 )			(p)
//;	add sign extended sing n2 to d1.
	c mplus,"m+",Xmplus

// -- subtract --

//;
//; d1-		( d -- d' )			(p)	hidden
//;	decrement double by one
//;	F4A: optionally in kernel if asm-cons _dx_ and _hx_ =/= 0.
_dx 	c d1m,"d1-",Xd1m,,,hidden
//;
//; q-		( q1 q2 -- q )			(p)
//;	subtract quads.
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c qsub,"q-",Xqsub
//;
//; d-		( d1 d2 -- d3 )			(p)
//;	subtract doubles.
	c dsub,"d-",Xdsub

// -- format conversion

//;
//; un->un 	( .un1. N M -- .un2. M )	(p)	hidden
//;	unsigned extend/truncate from +N cells to +M cells size
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c un2un,"un->un",Xun2un,,,hidden
//;
//; dsgn   	( dn -- n )			(p)
//;	ret n := signum(dn).
	c dsgn,"dsgn",Xdsgn
//;
//; s->q   	( n -- q )			(p)	hidden
//;	signed extend sing n to quad q, { s->d dup dup }.
//;	unsigned extension by pushing a zero to stack.
//;	F4A: 4th header optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c s2q,"s->q",Xs2q,,,hidden
//;
//; d->q   	( d -- q )			(p)	hidden
//;	signed extend double d to quad q, { dup 0< if -1. endif }.
//;	unsigned extension by pushing a zero to stack.
//;	F4A: 4th header optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c d2q,"d->q",Xd2q,,,hidden
//;
//; s->d   	( n -- d )			(p)
//;	signed extend n to double d, { dup 0< if -1 endif }.
//;	unsigned extension by pushing a zero to stack.
	c sdashgreatd,"s->d",Xs2d

//;
//; ---- dstack, multiple cells' ----
//;

//;
//; ndup   	( .xx. +n -- .xx. .xn. )	(p)
//;	check stack bounds, duplicate block of +n cells on data-stack.
	c ndup,"ndup",Xndup
//;
//; nswap   	( .xn. .yn. n -- .yn. .xn. )	(p)
//;	swap group of +n cells; n=/= 0.
	c nswap,"nswap",Xnswap
//;
//; 3drop  	( t -- )			(p)
//;	remove treble from dstack.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c drop3,"3drop",Xdrop3
//;
//; 4drop  	( d d -- )			(p)	hidden
//;	remove four sings from data-stack. { 2drop 2drop }.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c drop4,"4drop",Xdrop4,,,hidden
//;
//; ndrop   	( n -- )			(p)
//;	discard +n, allocate -n cells of data-stack; check stack bounds. if
//;	allocating, the additional cells' content on stack is indeterminate.
	c ndrop,"ndrop",Xndrop
//;
//; 3dup   	( t --- t t )			(p)
//;	duplicate @tos treble.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c dup3,"3dup",Xdup3
//;
//; 8dup   	( o --- o o )			(p)	hidden
//;	duplicate @tos quad (provisional for real numbers, 'floats').
//;	F4A: 4th header optionally in kernel if asm-cons _hx_ and _mx_ =/= 0.
_mx _hx	c dup8,"8dup",Xdup8,,,hidden
//;
//; 4dup   	( q --- q q )			(p)	hidden
//;	duplicate @tos quad (provisional for real numbers, 'floats').
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c dup4,"4dup",Xdup4,,,hidden
//;
//; 4-rot  	( q1 q2 q3 -- q3 q1 q2 )	(p)	hidden
//;	roll quad @tos to 3rd.
//;	F4A: 4th header optionally in kernel if asm-cons _hx_ and _mx_ =/= 0.
_mx _hx	c mr4,"4-rot",Xmrot4,,,hidden
//;
//; 4rot   	( q1 q2 q3 -- q2 q3 q1 )	(p)	hidden
//;	roll 3rd quad to tos.
//;	F4A: 4th header optionally in kernel if asm-cons _hx_ and _mx_ =/= 0.
_mx _hx	c rot4,"4rot",Xrot4,,,hidden
//;
//; 2-rot  	( d1 d2 d3 -- d3 d1 d2 )	(p)
//;	reverse roll double @tos to 3rd
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c mr2,"2-rot",Xmrot2
//;
//; 2rot   	( d1 d2 d3 -- d2 d3 d1 )	(p)
//;	roll 3rd double to tos.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c rot2,"2rot",Xrot2
//;
//; 4roll   	( qx...qm +N -- qx...qm qN )	(p)	hidden
//;	 	( qx...qm -N -- qN qx...qm )
//;	+/- roll quad, re {roll}, index range -16383 < N < 16382
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c rol4,"4roll",Xroll4,,,hidden
//;
//; 8roll   	( ox...om +N -- ox...om oN )	(p)	hidden
//;	 	( ox...om -N -- oN qx...om )
//;	+/- roll octuple, re {roll}, index range -16383 < N < 16382
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c rol8,"8roll",Xroll8,,,hidden
//;
//; 2roll   	( dx...dm +N -- dx...dm dN )	(p)
//;	 	( dx...dm -N -- dN dx...dm )
//;	+/- roll double, re {roll}, index range -16383 < N < 16382
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c rol2,"2roll",Xroll2
//;
//; 4pick   	( qm..q1 n -- qm..q1 n{n} )	(p)	hidden
//;	fetch +n-th quad item to tos.
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c xp4,"4pick",Xpick4,,,hidden
//;
//; 8pick   	( om..o1 n -- om..o1 o{n} )	(p)	hidden
//;	fetch +n-th quad item to tos.
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c xp8,"8pick",Xpick8,,,hidden
//;
//; 2pick   	( nm..n1 n -- nm..n1 n{n} )	(p)
//;	fetch +n-th double item to tos, { 0 2pick } is {2dup} or,
//;	store @nos item to -n-th item in data-stack. zero index refers to @nos,
//;	i.e. { 2 pick -2 pick } would leave the dstack un-changed.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c xp2,"2pick",Xpick2
//;
//; 8swap  	( o1 o2 --- o2 o1 )		(p)	hidden
//;	exchange top two octuples.
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c sw8,"8swap",Xswap8,,,hidden
//;
//; 4swap  	( q1 q2 --- q2 q1 )		(p)	hidden
//;	exchange top two quads.
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c sw4,"4swap",Xswap4,,,hidden
//;
//; 2swap  	( d1 d2 --- d2 d1 )		(p)
//;	exchange top two doubles. { 3 roll 3 roll }
	c sw2,"2swap",Xswap2//<;repeat)
//;
//; 4sover  	( q1 q2 --- q2 q1 q2 )		(p)	hidden
//;	{ 4swap 4over }
//;	F4A: header optionally in kernel if asm-cons _mx_ and _hx_ =/= 0.
_mx _hx	c sov4,"4sover",Xsover4,,,hidden
//;
//; 2sover  	( d1 d2 --- d2 d1 d2 )		(p)	hidden
//;	{ 2swap 2over }
//;	F4A: header optionally in kernel if asm-cons _mx_ and _hx_ =/= 0.
_mx _hx	c sov2,"2sover",Xsover2,,,hidden
//;
//; 4over  	( q1 q2 --- q1 q2 q1 )		(p)	hidden
//;	copy quad @nos to tos. { 1 4pick }
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c ov4,"4over",Xover4,,,hidden
//;
//; 2over  	( d1 d2 --- d1 d2 d1 )		(p)
//;	copy double @nos to tos. { 3 pick 3 pick }
	c ov2,"2over",Xover2
//;
//; 2dup   	( d --- d d )			(p)
//;	duplicate @tos double. { over over }
	c du2,"2dup",X2dup
//;
//; 4sdrop   	( q1 q2 -- q2 )			(p)	hidden
//;	discard 2nd item of data-stack.
//;	F4A: optionally in kernel if asm-cons _dx_ and _hx_=/= 0.
_dx _hx	c sdrop4,"4sdrop",Xsdrop4,,,hidden
//;
//; 2sdrop   	( d1 d2 -- d2 )			(p)
//;	discard 2nd item of data-stack.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c sdrop2,"2sdrop",Xsdrop2
//;
//; 2drop  	( d -- )			(p)
//;	remove double from data-stack. { drop drop }
	c drop2,"2drop",Xdrop2

//;
//; ---- multiple cells' logic ----
//;

//;
//; 2and   	( d1 d2 -- d' )			(p)
//;	bitwise AND two doubles.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c a2,"2and",Xand2
//;
//; 2xor   	( d1 d2 -- d' )			(p)
//;	bitwise XOR two doubles.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c xo2,"2xor",Xxor2
//;
//; 2or   	( d1 d2 -- d' )			(p)
//;	bitwise OR two doubles.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c o2,"2or",Xor2
//;
//; o0=   	( o -- flg )			(p)	hidden
//;	test octal cells for zero, tf if o = 0
//;	F4A: optionally header-less in kernel if asm-cons _mx_ =/= 0.
//;	F4A: word header optionally in kernel if asm-cons _hx_ =/= 0.
_mx _hx	c oz,"o0=",Xozeq,,,hidden
//;
//; ?uqcmp 	( uq1 uq2 - uq1 uq2 sgn )	(p)	hidden
//;	unsigned compare quads, ret sgn := signum(uq1-uq2).
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c uqcmp,"?uqcmp",Xquqcmp,,,hidden
//;
//; qcmp 	( q1 q2 - sgn )			(p)	hidden
//;	signed compare quads; replace q1,q2 with sgn := signum(q1-q2).
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c qcmp,"?uqcmp",Xquqcmp,,,hidden
//;
//; qsgn   	( q1 -- n )			(p)	hidden
//;	ret n := signum(q1).
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c qsgn,"qsgn",Xqsgn,,,hidden
//;
//; q0=   	( q -- flg )			(p)	hidden
//;	test quad for zero, tf if quad q = 0
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c qz,"q0=",Xqzeq,,,hidden
//;
//; ?q0=   	( q -- q flg )			(p)	hidden
//;	non-destructively test quad for zero, tf if quad q = 0
//;	F4A: optionally in kernel if asm-cons _mx_ =/= 0.
_mx	c qqz,"?q0=",Xqqzeq,,,hidden
//;
//; ?d0=   	( d1 -- d1 flg )		(p)	hidden
//;	tf if d1 = 0.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c qdz,"?d0=",Xqdz,,,hidden
//;
//; d0=		( d1 -- flg )			(p)
//;	tf if d1 = 0.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c d0z,"d0=",Xdz
//;
//; d=		( d1 d2 -- flg )		(p)
//;	tf if d1 equals d2
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c deq,"d=",Xdeq
//;
//; du>		( d1 d2 -- flg )		(p)
//;	tf if unsigned d1 greater than d2
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c dug,"du>",Xdug
//;
//; du<		( d1 d2 -- flg )		(p)
//;	tf if unsigned d1 smaller than d2
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c dul,"du<",Xduless
//;
//; d<		( d1 d2 -- flg )		(p)
//;	tf if signed d1 smaller than d2
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c dl,"d<",Xdless

//;
//;
//; ---- multiple cells' memory ----
//;

//;
//; l2!0   	( dn -- )			(p)
//;	Store dn to local cell at index 0; provisional for other per index 
//;	storeing words, 20 bytes of position-independent code at @cfa
//;		byte disp to l.s.cell at @cfa+8 : -(index+1)*8
//;		byte disp to m.s.cell at @cfa+11: -(index+1)*8+4
//;	F4A: optionally in kernel if asm-cons _ld_|_rx_ =/= 0.
_ld	c l2s1,"l2!0",Xl2s0
//;
//; l2@0   	( -- dn )			(p)
//;	Fetch local cell at index 0; 13 bytes of position-independent code 
//;	at @cfa (re l@0), pattern for other per index fetching words
//;		byte disp to l.s.cell at @cfa+6 : -(index+1)*8
//;		byte disp to m.s.cell at @cfa+10: -(index+1)*8+4
//;	F4A: optionally in kernel if asm-cons _ld_|_rx_ =/= 0.
_ld	c l2f1,"l2@0",Xl2f0
//;
//; l2!		( dn n -- )			(p)
//;	Store double dn to local memory at cell indexed by n.
//;	F4A: optionally in kernel if asm-cons _ld_|_rx_ =/= 0.
_ld	c l2sto,"l2!",Xl2sto
//;
//; l2@		( n -- dn )			(p)
//;	Fetch double dn from local memory at cell indexed by n.
//;	F4A: optionally in kernel if asm-cons _ld_|_rx_ =/= 0.
_ld	c l2fe,"l2@",Xl2fetch
//;
//; n@		( p -- .xx. N )				hidden
//;	fetch counted group of cells from memory w/ count at l.s.cell
//;	F4A: optionally in kernel if asm-cons _rx_ =/= 0.
_rx	c nf,"n@",Xnfetch,,,hidden
//;
//; n!		( .x. N p -- )				hidden
//;	store counted int w/ count at l.s.cell to memory
//;	F4A: optionally in kernel if asm-cons _rx_ =/= 0.
_rx	c ns,"n!",Xnstore,,,hidden
//;
//; r>m		( p n -- )(R: n1..nn -- )	(p)	hidden		Xrtom
//;	little-endian store n bytes from tor to memory location p,
//;	beginning w. cell aligned n1, discarding excess bytes @tor.
	c r2m,"r>m",Xrtom,,,hidden
//;
//; m>r		( p n -- )(R: -- n1..nn )	(p)	hidden		Xmtor
//;	little-endian fetch n bytes to tor; cell aligned w. <nul>s @tor.
	c m2r,"m>r",Xmtor,,,hidden
//;
//; m>s		( p n -- n1..nn )			hidden		Xmtos
//;	fetch n bytes from ptr p to tos, beginning w. cell aligned n1.
	c m2s,"m>s",Xmtos,,,hidden
//;
//; s>m		( n1..nn p n -- )			hidden		Xstom
//;	store n bytes from tos to memory location p
//;	beginning w. cell aligned n1
	c s2m,"s>m",Xstom,,,hidden
//;
//; 2+!		( dn a -- )			(p)
//;	Add dn to double at address a. re {2!}, {2@}; little endian cells order!
	c psto2,"2+!",Xpsto2
//;
//; 2@		( addr --- d1 )			(p)
//;	fetch doube from memory. { dup @ swap 4 + @ }
//;	NOTE:	F4 consistently keeping 'little endian' byte order!
//;		standard conformant substitute: { dup 4 + @ swap @ }
	c twofetch,"2@",Xfetch2//<;load)
//;
//; 2off   	( a -- )			(p)	hidden
//;	sto double cell sized zero to address a.
	c off2,"2off",Xoff2,,,hidden
//;
//; 2!		( d1 addr --- )			(p)
//;	sto doube to memory. { dup rot 4 + ! ! }
//;	NOTE:	F4 consistently keeping 'little endian' byte order!
//;		standard conformant 2! substitute: { swap over ! 4 + ! }
	c twostore,"2!",Xstore2//<;load)
//;
//;
//; ---- memory sings ----
//;

//;
//; l!0		( n -- )			(p)
//;	Store n to local cell at index 0; provisional for other per index 
//;	storeing words, 14 bytes of code, disp at byte -(index+1)*4 at @cfa+5.
//;	F4A: optionally in kernel if asm-cons _lx_|_rx_ =/= 0.
_lx	c ls1,"l!0",Xls0
//;
//; l@0		( -- n )			(p)
//;	Fetch local cell at index 0; 9 bytes of code, pattern for per index 
//;	fetching words, disp -(index+1)*4 byte at @cfa+6; re {l@n} in f4x.scr.
//;	F4A: optionally in kernel if asm-cons _lx_|_rx_ =/= 0.
_lx	c lf1,"l@0",Xlf0
//;
//; l!		( n1 n2 -- )			(p)
//;	Store n1 to local memory at cell indexed by n2.
//;	F4A: optionally in kernel if asm-cons _lx_|_rx_ =/= 0.
_lx	c lsto,"l!",Xlsto
//;
//; l@		( n1 -- n2 )			(p)
//;	Fetch n2 from local memory at cell indexed by n1. @tos stored to 1st 
//;	local cell, at index 0; ascending indices point towards lower addresses.
//;	F4A: optionally in kernel if asm-cons _lx_|_rx_ =/= 0.
_lx	c lfe,"l@",Xlfetch
//;
//; off		( a -- )			(p)	hidden
//;	sto cell sized zero to address a; asm by { 0 and! }.
	c off,"off",Xoff,,,hidden
//;
//; and!   	( n addr -- )			(p)
//;	AND n to cell at addr.
	c ands,"and!",Xands
//;
//; or!    	( n addr -- )			(p)
//;	OR n to cell at addr.
	c ors,"or!",Xors
//;
//; xor!   	( n addr -- )			(p)
//;	XOR n to cell at addr.
	c xors,"xor!",Xxors
//;
//; c+@		( a1 -- a2 )			(p)	hidden
//;	add signed a1 and content of byte at a1. { dup c@ c->s + } 
	c cpf,"c+@",Xcpf,,,hidden
//;
//; +@		( a1 -- a2 )			(p)	hidden
//;	add a1 and content of cell at a1 { dup @ + }. re {c+@}, {n<nfa}, 
//;	used in F4 kernel for memory references; e.g, if applied to a link field 
//;	address, returns the preceding lfa or, the header-block w. 'voc-link' &c.
	c plfa,"+@",Xplfa,,,hidden
//;
//; -!		( n a -- )			(p)	hidden
//;	Subtract n from the value at address a.
//;	F4A: optionally in kernel if asm-cons _0a_ =/= 0.
_0a	c msto,"-!",Xmsto,,,hidden
//;
//; +!		( n a -- )			(p)
//;	Add n to the value at address a. re {-!},{+@},{and!},{or!},{xor!}.
	c psto,"+!",Xpsto
//;
//; c@		( a -- b )			(p)
//;	fetch the 8 bit contents of memory address a.
	c cf,"c@",Xcfetch
//;
//; c!+		( n addr -- addr' )		(p)
//;	increment addr by 1 and store l.s.byte of n to addr.
	c cstop,"c!+",Xcstorep
//;
//; c!!		( a1 a2 -- )			(p)
//;	exchange bytes at a1 and a2.
	c css,"c!!",Xcss
//;
//; c!		( b a -- )			(p)
//;	Store 8-bit byte b to addr a. re {c!!},{c!+},{w!}.
	c csto,"c!",Xcstore
//;
//; w@		( a -- w )			(p)
//;	fetch the 16-bit contents of memory address a.
	c wf,"w@",Xwfetch
//;
//; w!		( w a -- )			(p)
//;	Store 16-bit item (l.s. half cell) to addr a.
	c wsto,"w!",Xwstore
//;
//; @+		( a1 -- a2 n )			(p)
//;	increment a1 by 4, fetch content of cell at a1 { 4+ dup 4- @ }.
	c fplus,"@+",Xfetchplus
//;
//; c@+    	( a1 -- a2 u )			(p)
//;	increment a1 by 1, fetch byte at a1 { 1+ dup 1- c@ }.
	c cfp,"c@+",Xcount
//;
//; dup@   	( a -- a n )			(p)	hidden
//;	push content of cell at address a.
	c dupf,"dup@",Xdupf,,,hidden
//;
//; @		( addr -- n )			(p)
//;	Leave the content of cell (32bit) at address. re {dup@},{@+},{c@},{w@}
	c fetch,"@",Xfetch
//;
//; !!		( a1 a2 -- )			(p)
//;	exchange cell at a1 w. cell at a2.
	c ssto,"!!",Xstosto
//;
//; !+		( n addr -- addr' )		(p)
//;	increment addr by 4 and store cell n (32bit) to addr.
	c stop,"!+",Xstorep
//;
//; !		( n addr -- )			(p)
//;	Store cell n (32bit) to addr. re {!!},{!+},{c!},{w!}.
	c store,"!",Xstore
//;
//; toggle 	( addr b -- )			(p)
//;	Complement the contents of addr by the 8-bit pattern b.
//;	8-bit XOR operation; reversed operators! - re {xor!}.
	c tog,"toggle",Xtoggle
//;
//;
//; ---- bitwise & logic, sings ----
//;

//;
//; bit		( n a -- b )			(p)
//;	ret b := 1 if bit no. n at address a is set, zero otherwise.
//;	for instance, { 1 pad 400 + ! 400 8 *  pad bit } returns 'true'.
//;	F4A: optionally in kernel if asm-cons _0and_ =/= 0.
_0a	c bit,"bit",Xbit
//;
//; pow2   	( n -- flg )			(p)
//;	tf if signed n is a power of 2.
//;	F4A: optionally in kernel if asm-cons _al_ =/= 0.
_al	c pow2,"pow2",Xpow2q
//;
//; <<     	( u1 u2 -- u3 )			(p)
//;	shift u1 left by u2 bit positions. unsigned mul u1 by 2^u2, u2 in [0,31].
	c shl,"<<",Xshleft
//;
//; >>     	( u1 u2 -- u3 )			(p)
//;	(logically) shift u1 right by u2 bit positions, unsigned div u1 by 2^u2.
	c shr,">>",Xshright
//;
//; 0and   	( n -- 0 )			(p)
//;	replace @tos item by 0 (false flag).
//;	F4A: optionally in kernel if asm-cons _0and_ =/= 0.
_0a	c and0,"0and",Xand0
//;
//; 1and   	( n -- flg )			(p)
//;	ret flg = l.s. bit of n1 (tf if n1 odd).
//;	F4A: optionally in kernel if asm-cons _0and_ =/= 0.
_0a	c and1,"1and",Xand1
//;
//; 1or		( n -- 1 )			(p)
//;	replace @tos item by 1 (true flag).
//;	F4A: optionally in kernel if asm-cons _0and_ =/= 0.
_0a	c or1,"1or",Xor1
//;
//; not    	( n1 -- n2 )			(p)
//;	bitwise inversion. re {0=} for logic flags inversion.
	c not4,"not",Xnot
//;
//; and    	( n1 n2 -- n3 )			(p)
//;	bitwise AND: n3 set to the rsp. bits of n1 which are also set in n2.
	c andx,"and",Xand
//;
//; or     	( n1 n2 -- n3 )			(p)
//;	bitwise OR: n3 set to the rsp. bits of n1 and n2.
	c orx,"or",Xorx
//;
//; xor    	( n1 n2 -- n3 )			(p)
//;	bitwise exclusive OR: invert the rsp. bits of n1 which are set in n2.
	c xor,"xor",Xxorx
//;
//;
//; ---- arith sings & mixed source oprs w. sing result ----
//;

//;
//; gcd		( n1 n2 -- u3 )		(p)
//;	greatest common denominator u3 of the absolute value of two signed sings. 
//;	If either number is zero, returns the absolute value of the other number.
//;	F4A: optionally in kernel if asm-cons _sqrf_ =/= 0.
_sq	c gcd,"gcd",Xgcd
//;
//; align   	( u1 +n2 -- u )			(p)
//;	align u1 to equal or unsigned greater multiple of n2; 
//;	ret u := 0 if n2 not a power of 2.
//;	F4A: optionally in kernel if asm-cons _al_ =/= 0.
_al	c ali,"align",Xali
//;
//; c->s   	( n -- d )			(p)
//;	signed extend byte c to sing n.
	c c2g,"c->s",Xc2s
//;
//; 8+		( n1 -- n2 )			(p)
//;	increment item @tos by eight (disp from lfa to pfa).
	c p8,"8+",Xp8
//;
//; 4+		( n1 -- n2 )			(p)
//;	increment item @tos by four (the cells size).
	c p4,"4+",Xp4
//;
//; 2+		( n1 -- n2 )			(p)
//;	increment item @tos by two.
	c p2,"2+",Xp2
//;
//; 1+		( n1 -- n2 )			(p)
//;	increment item @tos by one.
	c p1,"1+",Xp1
//;
//; 8-		( n1 -- n2 )			(p)
//;	decrement item @tos by eight (disp from pfa to lfa, re "lfa").
	c m8,"8-",Xm8
//;
//; 4-		( n1 -- n2 )			(p)
//;	decrement item @tos by four (the cells size).
	c m4,"4-",Xm4
//;
//; 3-		( n1 -- n2 )			(p)
//;	decrement item @tos by three.
	c m3,"3-",Xm3
//;
//; 2-		( n1 -- n2 )			(p)
//;	decrement item @tos by two.
	c m2,"2-",Xm2
//;
//; 1-		( n1 -- n2 )			(p)
//;	decrement item @tos by one.
	c m1,"1-",Xm1
//;
//; 16*		( u1 -- u2 )			(p)
//;	unsigned multiply item @tos by sixteen (f4f.scr: the ranum size).
//;	F4A: optionally in kernel if asm-cons _rx_ =/= 0.
_rx	c mul16,"16*",Xmul16
//;
//; 8*		( u1 -- u2 )			(p)
//;	unsigned multiply item @tos by eight (the doubles size).
//;	F4A: optionally in kernel if asm-cons _rx_ =/= 0.
_rx	c mul8,"8*",Xmul8
//;
//; 4*		( u1 -- u2 )			(p)
//;	unsigned multiply item @tos by four (the cells size).
	c mul4,"4*",Xmul4
//;
//; 3*		( u1 -- u2 )			(p)
//;	unsigned multiply item @tos by three.
	c mul3,"3*",Xmul3
//;
//; 2*		( u1 -- u2 )			(p)
//;	unsigned multiply item @tos by two.
	c mul2,"2*",Xmul2
//;
//; 16/		( u1 -- u2 )			(p)
//;	unsigned divide item @tos by sixteen (f4f.scr: the ranum size).
//;	F4A: optionally in kernel if asm-cons _rx_ =/= 0.
_rx	c div16,"16/",Xby16
//;
//; 8/		( u1 -- u2 )			(p)
//;	unsigned divide item @tos by eight (the double cells size).
//;	F4A: optionally in kernel if asm-cons _rx_ =/= 0.
_rx	c div8,"8/",Xby8
//;
//; 4/		( u1 -- u2 )			(p)
//;	unsigned divide item @tos by four (the cells size).
	c div4,"4/",Xby4
//;
//; 2/		( u1 -- u2 )			(p)
//;	unsigned divide item @tos by two.
	c div2,"2/",Xby2
//;
//; *		( n1 n2 -- n3 )			(p)
//;	signed sign product n3 of two signed sings. re { help mul }
	c mul,"*",Xmul
//;
//; */mod  	( n1 n2 n3 -- n4 n5 )		(p)
//;	quotient n5 and remainder n4 of { n1 n2 m* n3 m/ }, re "*/", "m*", "m/".
	c mbym,"*/mod",Xmulbymod
//;
//; */		( n1 n2 n3 -- n4 )		(p)
//;	ret ratio n4 of signed { */mod drop } with 63-bit intermediate product.
//;	re { help muldiv }
	c mulby,"*/",Xmulby
//;
//; u/mod  	( ud1 u2 -- u3r u4q )		(p)
//;	unsigned divide double by sing, leave sing rmd and quot.
	c uby,"u/mod",Xuby
//;
//; m/		( d n1 -- n2 n3 )		(p)
//;	leaves remainder n2 and quotient n3 from signed division of double
//;	dividend d and sing divisor n1. Remainder w. sign of dividend.
	c mby,"m/",Xmby
//;
//; /mod   	( n1 n2 -- rem quot )		(p)
//;	remainder and signed quotient of n1/n2. Remainder w. sign of dividend.
	c bym,"/mod",Xbym
//;
//; /		( n1  n2 -- quot )		(p)
//;	signed quotient of n1/n2, re { help / }.
	c by,"/",Xby
//;
//; mod		( n1 n2 -- mod )		(p)
//;	remainder of n1/n2, with the same sign as n1.
	c mod,"mod",Xmod
//;
//; +		( n1 n2 -- n3 )			(p)
//;	add two cells @tos.
//;	{ BEGIN DUP WHILE 2DUP AND 2* >R XOR R> REPEAT DROP }
	c plus,"+",Xplus
//;
//; -		( n1 n2 -- n3 )			(p)
//;	subtract n2 from n1.
	c dash,"-",Xdash
//;
//; bounds  	( n1 n2 -- n3 n1 )		(p)
//;	ret n3 as n2 + n1.
//;	{ over + swap }. for instance, to preparing parameters for DO w. 
//;	n2 as loop count wrt the base figure n1, e.g,
//;		{ bounds do i c@ key>char 0= if ascii _ endif emit loop }
//;	could be used a safe aequivalent to "type" - re "ptype".
	c bounds,"bounds",Xbounds
//;
//; swap-  	( n1 n2 -- n3 )			(p)
//;	subtract @nos n1 from @tos n2.
	c sm,"swap-",Xswapm
//;
//; over-  	( n1 n2 -- n1 n3 )		(p)
//;	subtract n1 from n2.
	c om,"over-",Xoverdash
//;
//; +-		( n1 n2 -- n3 )			(p)
//;	apply sign; negate sing n1 if n2 -ve
	c pm,"+-",Xplusdash
//;
//; minus  	( n1 -- n2 )			(p)
//;	negate sing n1
	c minus,"minus",Xminus
//;
//; 1abs    	( n1 -- n2 )			(p)
//;	in one-s complement notation, negate sing n1 if -ve.
//;	F4A: optionally in kernel if asm-cons _al_ =/= 0.
_al	c abs1,"1abs",X1abs
//;
//; abs    	( n1 -- n2 )			(p)
//;	negate sing n1 if -ve.
	c abs,"abs",Xabs
//;
//; wraps 	( x1 x2 x3 -- x4 )		(p)
//;	x4:=x1 if in range [x2,x3), else opposite bound value; un-/signed.
	c wa,"wraps",Xwraps
//;
//; within 	( x1 x2 x3 -- x4 )		(p)
//;	x4:=x1 if in range [x2,x3), else corresponding bound value; un-/signed.
	c wi,"within",Xwithin
//;
//; within? 	( x1 x2 x3 -- flg )		(p)
//;	true flag if x1 in range [x2,x3); signed or unsigned.
	c wiq,"within?",Xwithinq
//;
//; 0max    	( n1 -- n2 )			(p)	hidden
//;	n2 = n1 if +ve, else zero.
//;	F4A: optionally in kernel if asm-cons _0and_ =/= 0.
_0a	c zmax,"0max",X0max,,,hidden
//;
//; min    	( n1 n2 -- n3 )			(p)
//;	leave the signed smaller of two items.
	c min,"min",Xmin
//;
//; max    	( n1 n2 -- n3 )			(p)
//;	leave the signed greater of two items.
	c max,"max",Xmax
//;
//; u<		( n1 n2 -- flg )		(p)
//;	true flag if n1 unsigned smaller than n2.
	c ult,"u<",Xuless
//;
//; <      	( n1 n2 -- flg )		(p)
//;	true flag if n1 signed smaller than n2.
	c lt,"<",Xless
//;
//; >      	( n1 n2 -- flg )		(p)
//;	true flag if n1 signed greater than n2.
	c gt,">",Xgreat
//;
//; sgn		( n1 -- n2 )			(p)	hidden
//;	ret n2 := signum(n1).
	c sgn,"sgn",Xsgn,,,hidden
//;
//; -0=		( n1 -- flg )			(p)	hidden
//;	true flg(1) if n1 = -0 = 0x80000000, any other values return false(0). 
	c mzeq,"-0=",Xmzeq,,,hidden
//;
//; 0=/=   	( n -- 0|-1 )			(p)	hidden
//;	converts item @tos to fig-4th flag value, true = 1 for any non-zero n.
	c zneq,"0=/=",Xzneq,,,hidden
//;
//; ?0=		( n1 -- n1 flg )		(p)	hidden
//;	non-destructively test whether n is zero. re {0=}.
	c zeqq,"?0=",Xzeqq,,,hidden
//;
//; 0=		( n1 -- flg )			(p)
//;	logic inversion, true(1) if n1=0 or -0, any other values return false(0). 
//;	{ 0= 0= } can be used to generate a 'well formed' flag, 0 or 1, from any n1.
//;	re {not} for bitwise inversion; {-0=}, {-0}.
	c zeq,"0=",Xzeq
//;
//; 0>      	( n1 -- flg )			(p)
//;	true flag if n1 signed greater than zero.
	c zgt,"0>",Xzgreat
//;
//; 0<      	( n1 -- flg )			(p)
//;	true flag if n1 signed smaller than zero, i.e. negative.
	c zlt,"0<",Xzless
//;
//; =/=		( x1 x2 -- flg )		(p)
//;	true flag if top two items @tos are not equal.
	c ne,"=/=",Xne
//;
//; =		( x1 x2 -- flg )		(p)
//;	true flag (1) if top two items @tos are equal.
	c eq,"=",Xeq
//;
//;
//; ---- rstack, sings ----
//;

//;
//; rp!		(R: .xx. -- )			(p)
//;	reset return-stack ptr from uvari "r0" to initial value.
//;	F4: return-stack won't 'overflow' if in range of Linux '.ss' section.
	c rpstore,"rp!",Xrpstore
//;
//; rp@		( -- a )			(p)
//;	ret address of the return-stack ptr.
//;	system dependent, should not be used for displaced rstack access.
	c rpf,"rp@",Xrpfetch
//;
//; r>>r   	( n1 -- n2 )(R: n2 -- n1 )	(p)	hidden
//;	exchange @tos w. @tor, { r> swap >r }.
	c r2r,"r>>r",Xr2r,,,hidden
//;
//;
//; rdrop  	(R: n -- )			(p)
//;	remove sing from return-stack. { r> drop }; re {2rdrop}
	c rdrop,"rdrop",Xrdrop
//;
//; >r		( n -- )(R: -- n )		(p)
//;	Copy @tos to return-stack and drop from data-stack. re {r>}, {r}, {>rr}.
//;	single line sequence of { >r ... r> } can be used while interpreting.
	c gr,">r",Xgreatr
//;
//; >rr		( n -- n )(R: -- n )		(p)
//;	Push duplicate @tos to return-stack. re {r>}, {r}, {>r}.
//;	single line sequence of { >r ... r> } can be used while interpreting.
	c trr,">rr",Xtrr
//;
//; r>		( -- n )(R: n -- )		(p)
//;	Pop top item from return-stack to @tos. re {>r}, {r}, {>rr}.
//;	single line sequence of { >r ... r> } can be used while interpreting.
	c rgreat,"r>",Xrgreat
//;
//; r		( -- n )(R: n -- n )		(p)
//;	Push top item of return-stack to @tos. re {r>}, {>r}, {>rr}.
//;	single line sequence of { >r ... r> } can be used while interpreting.
	c rx,"r",Xrx
//;
//; i		( -- n )(R: n -- n )		(p)
//;	Push the loop index of a DO..LOOP to the stack. synonym {r}
	c ix,"i",Xrx
//;
//; j		( -- n )(R: n x x -- n x x )	(p)
//;	Push the next outer loop index of a nested DO..LOOP to stack.
	c j,"j",Xj
//;
//; ll		( n1 -- n2 )(R: .x. n2 .x. -- .x. n2 .x. )(p)	hidden
//;	Push the n1-th loop limit of a nested groop of DO..LOOPs to stack.
	c ll,"ll",Xll,,,hidden
//;
//; ni		( n1 -- n2 )(R: .x. n2 .x. -- .x. n2 .x. )(p)	hidden
//;	Push the n1-th loop index of a nested groop of DO..LOOPs to stack.
//;	{i} is aequivalent to { 0 ni }, {j} to { 1 ni }. re {ll} loop limit.
	c ni,"ni",Xni,,,hidden
//;
//;
//; ---- dstack, sings ----
//;

//;
//; sp!		( .xx. -- )			(p)
//;	reset data-stack ptr from uvari "s0" to initial value. re "-sp":
//;	F4: by frequently checking for stack bounds w. "-sp", data-stack won't 
//;	'overflow' if in range of Linux specific limits for 'mmap'ed memory.
	c spstore,"sp!",Xspstore
//;
//; sp>		( .xx. -- )			(p)
//;
//;	clear data-stack. synonym "sp!".
	c spgreat,"sp>",Xspstore
//;
//; +sp		( n -- flg )			(p)
//;	check stack bounds, allocate if depth+n above top margin -re- "-sp".
//;	ret -ve flg for unsuitable n (-ve) or stack error, else zero, 
//;	abort if n was too much to allocate.
//;	NOTE: might modify "s0" contents if re-allocation required!
	c psprst,"+sp",Xpsprst
//;
//; -sp		( -- flg )			(p)
//;	-ve flg if stack ptr below bounds, +ve if above,
//;	restores stack ptr if out of bounds, else flg = 0.
//;	NOTE: might modify "s0" contents if re-allocation required!
//;	allocates ps_xtd (16K) bytes more from host memory if less than the 
//;	minimum of ds_min (1K) bytes free; fixed ds_min, ds_xtd by asm vari.
	c sprst,"-sp",Xsprst
//;
//; sp@		( -- a )			(p)
//;	ret address of the stack ptr @tos, before xec of SP@.
//;	System dependent, should not be used for random dstack access.
//;	re "roll", "pick" which also store back to selected item on dstack.
//;	F4 data-stack pushes towards higher addresses, i.e. @nos at { sp@ 4- }
	c spfetch,"sp@",Xspfetch
//;
//; depth  	( -- n )			(p)
//;	ret no. of items in data-stack;
//;	extends dstack if bounds in range for new allocation request.
	c depth,"depth",Xdepth
//;
//; pick4   	( n1 n2 n3 n4 n5 -- n1 n2 n3 n4 n5 n1 ) (p)	hidden
//;	aequivalent to { 4 pick }
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c pick4,"pick4",X4pick,,,hidden
//;
//; pick3   	( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 ) (p)	hidden
//;	aequivalent to { 3 pick }
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c pick3,"pick3",X3pick,,,hidden
//;
//; pick2   	( n1 n2 n3 -- n1 n2 n3 n1 )	(p)	hidden
//;	aequivalent to { 2 pick }
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c pick2,"pick2",X2pick,,,hidden
//;
//; ddup   	( n -- n n n )			(p)	hidden
//;	twice duplicate @tos.
//;	F4A: optionally in kernel if asm-cons _dx_ =/= 0.
_dx	c ddup,"ddup",Xddup,,,hidden
//;
//; 0dup   	( n -- n | 0 0 )		(p)	hidden
//;	duplicate n if zero or '-0'
	c zdup,"0dup",Xzdup,,,hidden
//;
//; dup2   	( n1 n2 -- n1 n1 n2 )		(p)	hidden
//;	duplicate @nos, { over swap }.
	c dup2,"dup2",Xdup2,,,hidden
//;
//; sdup   	( n1 n2 -- n2 n1 n1 )		(p)	hidden
//;	{ swap dup }.
	c sdup,"sdup",Xsdup,,,hidden
//;
//; drop2   	( n1 n2 n3 -- n2 n3 )		(p)	hidden
//;	drop 3rd on dstack { rot drop }.
	c rotd,"drop2",Xrotd,,,hidden
//;
//; sdrop   	( n1 n2 -- n2 )			(p)	hidden
//;	discard 2nd item of data-stack.
	c sdrop,"sdrop",Xsdrop,,,hidden
//;
//; dropd  	( n1 n2 -- n1 n1 )		(p)	hidden
//;	aequivalent to { drop dup }.
	c dropd,"dropd",Xdropd,,,hidden
//;
//; sover   	( n1 n2 -- n2 n1 n2 )		(p)	hidden
//;	aequivalent to { swap over }.
	c sover,"sover",Xsover,,,hidden
//;
//; roll   	( n1 - n2 | -n1 -- n2 )		(p)
//;	remove (+n1)-th item from stack and copy to tos or, 
//;	remove @tos and insert item to (-n1)-th position on stack.
	c roll,"roll",Xroll
//;
//; pick   	( +n1 - n2 | n -n1 -- )		(p)
//;	Push +n1-th item from stack to tos, index 0 refers to @tos,
//;	store n back to -n1 item @tos, index wrt 1st remaining item.
	c pick,"pick",Xpick
//;
//; swap   	( n1 n2 -- n2 n1 )		(p)
//;	aequivalent to { 1 roll }
	c swap,"swap",Xswap
//;
//; rot    	( n1 n2 n3 -- n2 n3 n1 )	(p)
//;	aequivalent to { 2 roll } = { >r swap r> swap }
	c rot,"rot",Xrot
//;
//; -rot    	( n1 n2 n3 -- n3 n1 n2 )	(p)
//;	aequivalent to { rot rot }
	c mrot,"-rot",Xmrot
//;
//; over   	( n1 n2 -- n1 n2 n1 )		(p)
//;	aequivalent to { 1 pick }
	c over,"over",Xover
//;
//; -dup   	( n -- n n | 0 )		(p)
//;	duplicate n if non-zero and =/= '-0'
	c dashdup,"-dup",Xmdup
//;
//; dup		( n -- n n )			(p)
//;	aequivalent to { 0 pick }
	c dup,"dup",Xdup
//;
//; drop   	( n -- )			(p)
//;	discard top item of data-stack
	c drop,"drop",Xdrop
.nolist
// -------------------------------------------------
// forthpw.S eof
