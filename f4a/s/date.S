//; =============================================================================
//; F4,F4A Copyright (C) 2004 by h-peter recktenwald <f4@lxhp.in-berlin.de>
// =============================================================================
//; date.S 
// =============================================================================
.psize 0
.att_syntax noprefix
.text
.list

//;
//; ---- system time & date ----
//;

// ( sec -- s m h D M Y ); sec seit 1.1.1970 nach datum 
.ifne 1
#define dY TS	//,	-> Year
#define dM TOS(1)//,	-> Month
#define dD TOS(2)//,	-> Day
#define dh TOS(3)//,	-> hour
#define dm TOS(4)//,	-> minute
#define ds TOS(5)//,	-> second
#define d1 TS
#define d1h (esp)
#define d2 ecx
#define d4 edx
#define d6 ebx
Xprobier:
//  *L3D78< l3d8a
//  l3d78		<---,,
    //   swap d0
    //   divu #10,d0
    //   swap d0
    //   add.b #'0',d0
    //   subq.l #1,a1
    //   move.b d0,0(a6,a1.l)
//  *L3D8A< l3da8
//  l3d8a
    //   clr.w d0
    //   dbra d5,l3d78  -->-''
    //   move.w (a7)+,d5
    //   add.w d5,a1
    //   rts 

//  *L3D96< cn_date
//  l3d96
    //   bsr.s l3da8  >	; num16 to stg
//  *L3D98< cn_date
//  l3d98
    //   moveq #$20,d0
//  *L3D9A< l3da2
//  l3d9a
    //   move.b d0,0(a6,a1.l)
    //   addq.l #1,a1
    //   rts 

//  *L3DA2< cn_date
//  l3da2
    //   bsr.s l3da8  >		int16 d6h->puffer, d6=d6h|d6l->d6l|d6h, a2:=a1+d5
    //   moveq #':',d0 $3a
    //   bra.s l3d9a  -

//  *L3DA8< l3d96 l3da2 cn_date
// :in: d6:int16|d6l d5:d5h|len a1:adr(a6) :out: d0:00 d5:xx|d5l d6:d6l|d6h a1:stg.end(a6)
//  l3da8
    //   swap d6
    //   move.l d6,d0
    //   move.w d5,-(a7)
    //   add.w d5,a1
    //   bra.s l3d8a  -

//  *L3DB2< cn_date cn_day
//  3db2
    //   moveq #0,d0  mt_inf
    //   trap #1
    //   move.l $14a(a0),a0  sv_mgtab
    //   add.w 0(a0,d7.w),a0
    //   moveq #3,d7
    //   mulu d7,d4
    //   add.w d4,a0
//  *L3DC4< l3dc4
//  l3dc4
    //   move.b (a0)+,0(a6,a1.l)
    //   addq.l #1,a1
    //   subq.b #1,d7
    //   bne.s l3dc4  ?
	ret    //   rts 
// *L3DD0< m00c0 l84d4 l8822 : in: a1(a6),d1 out: a1(a6), d0
// cn_date
//  movem.l d1-d7/a0,-(a7)
	call Cdnum	//  bsr.s l3e1c  >
    //  moveq #$14,d5
    //  sub.l d5,a1
    //  move.w d5,-2(a6,a1.l)
    //  moveq #4,d5
    //  bsr.s l3d96  >
    //  swap d6
    //  move.w d1,d6
    //  moveq #$3a,d7
    //  bsr.s l3db2  >
    //  bsr.s l3d98  >
    //  moveq #2,d5
    //  bsr.s l3d96  >
    //  bsr.s l3da2  >
    //  move.l d3,d6
    //  bsr.s l3da2  >
    //  bsr.s l3da8  >
    //  lea -$16(a1),a1
    // *L3DFC< cn_day
    // l3dfc
    //  movem.l (a7)+,d1-d7/a0
    //  moveq #0,d0
	ret		//  rts 
*L3E1C< cn_date cn_day
Xdnum:	pushl NXL
Cdnum:	pushl ebx	// date->numeric values
	pushl $7	// wochentage
	pushl $0	//
	pushl $59	// 28. februar
//	;pushl $7		// tage/woche
	pushl $24	// stunden/tag
	pushl $60	// min,sec/stunde,minute// moveq #$3c,d2	; 60
	SPUSH(5)
	popl ecx
	xorl edx,edx	//  bsr.s l3e7c  >	;div d1/60; qot=:d0=d1.l, rmd=:d3.w
	divl ecx	//a:4037F100:a:0111ffc0,d:00000000;d0:=sec, edx=ticks
	movl edx,TOS(5)	// swap d3 // ecx := d3.hi := sec
	xorl edx,edx
	divl ecx	//a:0111ffc0:a:00049110,d:00000000// bsr.s l3e7c  >	;div d1/60;
	movl edx,TOS(4)	//d3.lo := min//
	xorl edx,edx
	popl ecx
	divl ecx	//a:00049110:a:000030b6,d:00000000// divu #$18,d1 // TS := day(s)	; 24
	movl edx,TOS(3)	//d6.lo = hour//
	movl TS,TOS(2)	//TS:days//  move.l d1,d6	; d6:= lo:day|hi:hour
//	;popl ecx	//  moveq #0,d4
//	;xorl edx,edx	//  move.w d1,d4
//	;		//  move.l d4,d1
//	;divl ecx	//  divu #7,d4	; weeks,days
//	;movl TOS(2),TS	//tage 
//	;movl edx,TOS(2)//wochentag, TS:woche
	movl $1461,ecx	// 4 jahre + schalttag
	divl ecx	//a:000030b6:a:00000008,d:0000030e  divu #$5b5,d1	; 1461 = 4*365+1
	movl TS,d6	//  move.w d1,d6
	movl edx,TS	//  clr.w d1
	xorl edx,edx	//  swap d1
	movl $365,ecx	//  divu #$16d,d1	; 365
	divl ecx	//a:0000030e:a:00000002,d:00000034
	popl d2		//  moveq #$3b,d2	; 59
	subl $3,d1	//  subq.w #3,d1
	jl 2f	//  blt.s l3e52  ?
	jz 0f	//  beq.s l3e50  ?
	xorl d1,d1	//  move.l #$16d0000,d1 ; 23920640
	movl $0x16d,d1h
0:		// l3e50
	movl $60,d2	//  moveq #$3c,d2	; 60
2:		// l3e52
	shll $2,d6	//  asl.w #2,d6	; 4/
	addl d1,d6	//  add.w d1,d6	; 1+
	addl $1970,d6	//001f->07d1//  add.w #$7ac,d6	; 1964 -> year
	popl d1		//  clr.w d1
			//  swap d1
	subl d2,d1	//  sub.w d2,d1
	jl 4f		//  blt.s l3e64  ?
	movl $61,d2	//  moveq #$3d,d2	; 61
4:		// l3e64
	addl d2,d1	//  add.w d2,d1
	popl d2		//  moveq #7,d2	; week -> day
	mull d2		//  mulu d2,d1
	addl $3,TS	//  addq.w #3,d1
	movl $214,d6
	divl d6		//3:0,3//  divu #$d6,d1	; 214
	movl d1,d6	//  move.w d1,d4
	movl d4,TS	//  clr.w d1
	movl d6,d4	//  swap d1
	divl d2		//  divu d2,d1
	incl d1		//  addq.w #1,d1
	popl ebx
	ret		//  rts 
.else // // // // // // 
*L3DD0< m00c0 l84d4 l8822
cn_date
 movem.l d1-d7/a0,-(a7)
 bsr.s l3e1c  >
 moveq #$14,d5
 sub.l d5,a1
 move.w d5,-2(a6,a1.l)
 moveq #4,d5
 bsr.s l3d96  >
 swap d6
 move.w d1,d6
 moveq #$3a,d7
 bsr.s l3db2  >
 bsr.s l3d98  >
 moveq #2,d5
 bsr.s l3d96  >
 bsr.s l3da2  >
 move.l d3,d6
 bsr.s l3da2  >
 bsr.s l3da8  >
 lea -$16(a1),a1
*L3DFC< cn_day
l3dfc
 movem.l (a7)+,d1-d7/a0
 moveq #0,d0
 rts 
*L3E1C< cn_date cn_day
l3e1c
 moveq #$3c,d2	; 60
 bsr.s l3e7c  >	;div d1/60; qot=:d0=d1.l, rmd=:d3.w
 swap d3	; d3.hi := sec
 bsr.s l3e7c  >	;div d1/60; d3.lo := min
 divu #$18,d1	; 24
 move.l d1,d6	; d6:= day|hour
 moveq #0,d4
 move.w d1,d4
 move.l d4,d1
 divu #7,d4	; weeks,days
 divu #$5b5,d1	; 1461 = 4*365+1
 move.w d1,d6
 clr.w d1
 swap d1
 divu #$16d,d1	; 365
 moveq #$3b,d2	; 59
 subq.w #3,d1
 blt.s l3e52  ?
 beq.s l3e50  ?
 move.l #$16d0000,d1 ; 23920640
*L3E50< l3e1c
l3e50
 moveq #$3c,d2	; 60
*L3E52< l3e1c
l3e52
 asl.w #2,d6	; 4/
 add.w d1,d6	; 1+
 add.w #$7ac,d6	; 1964 -> year
 clr.w d1
 swap d1
 sub.w d2,d1
 blt.s l3e64  ?
 moveq #$3d,d2	; 61
*L3E64< l3e52
l3e64
 add.w d2,d1
 moveq #7,d2	; week -> day
 mulu d2,d1
 addq.w #3,d1
 divu #$d6,d1	; 214
 move.w d1,d4
 clr.w d1
 swap d1
 divu d2,d1
 addq.w #1,d1
 rts 

*L3E7C< l3e1c	; d1 / d2 => d3.rmd d0=d1.qot
l3e7c
 moveq #0,d0
 swap d1
 move.w d1,d0	: nh
 divu d2,d0	; d0:=hr|hq
 swap d0	;     hq|hr
 move.w d0,d1	; d1:=nl:hr
 swap d1
 divu d2,d1	; d1:=lr:lq
 move.w d1,d0	; lq
 swap d1
 move.w d1,d3	; rmd
 move.l d0,d1	; qot
 rts 
.endif
.nolist
// -----------------------------------------------------------------------------
// date.S <eof>
