//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; forthhw.S 
// =============================================================================
.psize 0
.att_syntax noprefix
.list
.text
//;
//; -------------------------------------------------------------------------------
//; Forth hi-level words & header-less
//; -------------------------------------------------------------------------------
//;

//;
//;
//; ---- hi-level words ----
//;
//;
//; thru   	( n1 n2 -- )				root
//;	load scr n1 till n2 (inclusive). re {load}, {-->}, {lload}.
//;	F4A: optionally in kernel if asm-cons _tu_ =/= 0 (dft off).
_tu	c thru,"thru",docolon,,,root
_tu	.long p1,dup2,eq,zerobranch,1f-.,drop2,semis
_tu 1:	.long swap,pdoq
_tu 2:	.long ix,load,drq,relch,chrnum,zeq,dleave,ploopq,2b-.,semis
//;
//; <abort>	( .x. -- )				hidden
//;	{cold} and initial hi-level entry; { [abort] begin quit again }
	c doabort,"<abort>",docolon,,,hidden
	.long pabortq
1:	.long quit,branch,1b-.
//;
//; <quit>	( .x. -- .x. ) (R: .y. -- )		hidden
//;	default {quit},, stored to uvari (quit), executes
//;	{ io-cons drop -quit-
//;	  begin rp! stdout cons? if (ok) endif query interpret clrsegv again }
	c iniquit,"<quit>",docolon,,,hidden
	.long iocons,drop,iniq
2:	.long rpstore,oconsq,zerobranch,1f-.,pokq
1:	.long query,interpret,clrsegv,branch,2b-.
//;
//; ---- headerless hi-level ----
//;
	dbn "e"
ledit:	.long docolon,xledit,interpret,lload2
// -------------------------------------------------
.nolist
// -------------------------------------------------
// forthhw.S <eof>
