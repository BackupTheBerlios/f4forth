//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; rawkey.S 
// =============================================================================
.psize 0
.att_syntax noprefix
.list
.include "kd.s"
.text
//;
//; ---- raw keyboard input ----
//; ----	re /usr/src-linux/linux/include/linux/kd.h
//;

tty7t:	.asciz "/dev/tty7"
//
crkch:	pushl $rkbdch
	spush
	popl TS
	jmp brchid	//; kbd - muﬂ 'echter' tty-kanal sein, pty spielt nicht!
//
cgkbm:	call crkch	//; kbd
	js 1f
	pushl $-1
	movl $KDGKBMODE,ecx
	movl esp,edx	//; mode = @(esp)
	sys_ ioctl
	popl edx
1:	cmpl $maxerr,TS
	ret
//
crkbr:	pushl ecx	//; kbd mode
	jmp 1f
cskbr:	pushl $K_RAW
1:	call crkch
	movl $KDSKBMODE,ecx
	popl edx	//; mode = edx - note the inconsistency wrt KDGKBMODE!
	sys_ ioctl
	cmpl $maxerr,TS
	ret
//
crky:	pushl edx	//; save current keboard mode
	pushl ecx	//; xec
	call cskbr	//; 'raw'
	popl edx
	call *edx
	movl $KDSKBMODE,ecx
	popl edx	//; mode = edx - note the inconsistency wrt KDGKBMODE!
	sys_ ioctl
	ret
// ; in: ebx fd
// ; out:eax code
crgky:
	ret
//
Xrqky:	leal pqkey,ecx
Crqky:	pushl NXL	//<Xrkey)
	pushl ebx
	pushl ecx
	spush
	call cgkbm
	jc 1f		//;?; edx := current kb mode
	call t7open	//; try /dev/tty7
    movl 4(esp),ebx
    call Cdots
	jnc 2f		//;?; fehler
	call cgkbm
    movl 4(esp),ebx
    call Cdots
	jnc 2f		//;?; fd unbrauchbar
1:	movl (esp),ecx
	call crky
    movl 4(esp),ebx
    call Cdots
2:	popl ecx	//; drop xec
	popl ebx
	ret
//	
Xrkey:	leal Ckbdkey,ecx
	jmp Crqky
//;
//;
//; ---- hi-level words ----
//;

// -------------------------------------------------
.nolist
// -------------------------------------------------
// rawkey.S <eof>
