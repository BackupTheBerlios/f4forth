//; =============================================================================
//; F4, F4A, programs & text Copyright (C) 2003 by hp <f4@lxhp.in-berlin.de>,
//; The german variant of the Q-License 1.0 applies, which is included in the 
//; f4a package, available from <http://lxhp.in-berlin.de/lhpf4.html>.
//; re files ./d/QT-License.txt, ./d/QT-Lizenz.txt in documentation directory.
//; =============================================================================
//; F4 fig4th.S
//; =============================================================================
//;
//; fig-1.0 style Forth kernel for i386-PC-Linux/ELF hosts (Linux kernel 2.2+),
//;
//; optional words' compiling can be controlled with asm-constants in <fig4th.h>
//;
//; based on f.i.g. forth Sinclair-QL implementation by Laurence Reeves - 1991
//;	.hpr/2003: extended & modified for Linux; testing CPP & AS capabilites.
//;
//; =============================================================================
//; :ASM: unless noted, subroutines modify eax=@TOS, ecx, edx, edi=PS registers
//;	  all general purpose regs used, mmx/fp-registers free for applications
//; =============================================================================
//;
.psize 0
.att_syntax noprefix
.include "version.inc"	// release no.
.list
// ==============================================================================
//  :REIHENFOLGE: und art der include-s (bis 'inikernel.s') nicht aendern!
//		  ggf die numerischen referenzen beachten, u.u. existieren 
//		  noch welche, die ueber file-grenzen hinausweisen.
// ==============================================================================
#include "fig4th.inc"	// w. control cons for conditional assembly
#include "fig4thm.inc"	// asm macro defns
.include "scfctl.s"	// ioctl & fcntl cons
.include "scdefs.s"	// cons for syscalls 
.include "lxdata.s"	// lx kernel ref
//; ===============================================================================
//; kernel dataspace
.include "inidata.s"
//; -------------------------------------------------------------------------------
//; kernel program code
.include "inikernel.s"
//; -------------------------------------------------------------------------------
//; primitives' code
//; -------------------------------------------------------------------------------
// ---- include new code anywhere below here and before word headers
// ---- debugging
.include "fthdebug.s"
// ---- memory test & allocation
.include "mapmem.s"
// ---- compiler
.include "cpldef.s"
// ---- num input
.include "numio.s"
// ---- hi-level init, <next> code
.include "inixec.s"
// ---- arithmmetic & logic
.include "fthalu.s"
// ---- optional doubles & quad
.include "qarith.s"
// ---- mem & stacks
.include "dsmem.s"
// ---- uvari opr
.include "uvario.s"
// ---- text, interpreter
.include "itext.s"
// ---- local mem
.include "locmem.s"
// ---- raw keyboard input
.include "rawkey.s"
// ---- tty by vt, syscalls, shell cmd
.include "vtsys.s"
// ---- screenfiles
.include "blockf.s"
// ---- error messages
.include "erms.s"
// ---- i/o, random file access
.include "fileio.s"
// ---- include new code anywhere above here and kernel code
// ---- word-headers, interpreter/compiler
// ---- include new headers after last vocabulary header
.include "forthpw.s"
// ---- hi-level words 
.include "forthhw.s"
// ---- include new headers before {task}
//;
//; task   	( -- )
//;	No-operation hi-level word, empty colon defn; last in kernel dictionary.
//;	<docolon> code address by { ' task cfa @ }.
	c task,"task",docolon
	.long semis
// -- 'task' should remain the last kernel word, for reference --
// -- die 'task'-pfa dient an verschiedenen stellen im code als kern-endmarke, --
// -- desgleichen bei den hi-level erweiterungen, musz drum hier stehenbleiben --
//;
//; =================== kernel ends ===============================================
//;
top:
// -----------------------------------------------------------------------------
// fig4th.S <eof>
