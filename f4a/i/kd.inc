//;=========================================================================;
//; Copyright (C) 2000 H-Peter Recktenwald, Berlin <l4@lxhp.in-berlin.de>
//;=========================================================================;
// ;
//;  file  	: kd.inc
// ;  switch	: _LINUX_KD_H
// ;  created	: 25-sep-2003
// ;  version	: 0.02
// ;  assembler	: as
// ;  author	: H-Peter Recktenwald, Berlin <l4@lxhp.in-berlin.de>
// ;  source	: linux/include/linux/kd.h
//;=========================================================================;
//;

#ifndef _LINUX_KD_H
#define _LINUX_KD_H

// ; 0x4B is 'K', to avoid collision with termios and vt
// ; sys_ioctl fd, request,

.set GIO_FONT,0x4B60		// ; gets font in expanded form
.set PIO_FONT,0x4B61		// ; use font in expanded form

.set GIO_FONTX,0x4B6B		// ; get font using struct consolefontdesc
.set PIO_FONTX,0x4B6C		// ; set font using struct consolefontdesc


.set PIO_FONTRESET,0x4B6D		// ; reset to default font

.set GIO_CMAP,0x4B70		// ; gets colour palette on VGA+
.set PIO_CMAP,0x4B71		// ; sets colour palette on VGA+

.set KIOCSOUND,0x4B2F		// ; start sound generation (0 for off)
.set KDMKTONE,0x4B30		// ; generate tone

.set KDGETLED,0x4B31		// ; return current led state
.set KDSETLED,0x4B32		// ; set led state [lights, not flags]
.set 	LED_SCR,1		// ; scroll lock led
.set 	LED_CAP,4		// ; caps lock led
.set 	LED_NUM,2		// ; num lock led

.set KDGKBTYPE,0x4B33		// ; get keyboard type
.set 	KB_84,	1
.set 	KB_101,	2 		// ; this is what we always answer
.set 	KB_OTHER,3

.set KDADDIO,0x4B34		// ; add i/o port as valid
.set KDDELIO,0x4B35		// ; del i/o port as valid
.set KDENABIO,0x4B36		// ; enable i/o to video board
.set KDDISABIO,0x4B37		// ; disable i/o to video board

.set KDSETMODE,0x4B3A		// ; set text/graphics mode
.set		KD_TEXT	,0
.set		KD_GRAPHICS,1
.set		KD_TEXT0,2	// ; obsolete
.set		KD_TEXT1,3	// ; obsolete
.set KDGETMODE,0x4B3B		// ; get current mode

.set KDMAPDISP,0x4B3C		// ; map display into address space
.set KDUNMAPDISP,0x4B3D		// ; unmap display from address space

.set	E_TABSZ,256
.set GIO_SCRNMAP,0x4B40		// ; get screen mapping from kernel
.set PIO_SCRNMAP,0x4B41		// ; put screen mapping table in kernel
.set GIO_UNISCRNMAP,0x4B69	// ; get full Unicode screen mapping
.set PIO_UNISCRNMAP,0x4B6A	// ; set full Unicode screen mapping

.set GIO_UNIMAP,0x4B66		// ; get unicode-to-font mapping from kernel
.set PIO_UNIMAP,0x4B67		// ; put unicode-to-font mapping in kernel
.set PIO_UNIMAPCLR,0x4B68		// ; clear table, possibly advise hash algorithm

.set UNI_DIRECT_BASE,0xF000	// ; start of Direct Font Region
.set UNI_DIRECT_MASK,0x01FF	// ; Direct Font Region bitmask

.set		K_RAW	,0
.set		K_XLATE	,1
.set		K_MEDIUMRAW,2
.set		K_UNICODE,3
.set KDGKBMODE,0x4B44		// ; gets current keyboard mode
.set KDSKBMODE,0x4B45		// ; sets current keyboard mode

.set		K_METABIT,3
.set		K_ESCPREFIX,4
.set KDGKBMETA,0x4B62		// ; gets meta key handling mode
.set KDSKBMETA,0x4B63		// ; sets meta key handling mode

.set		K_SCROLLLOCK,1
.set		K_CAPSLOCK,2
.set		K_NUMLOCK,4
.set KDGKBLED,0x4B64		// ; get led flags (not lights)
.set KDSKBLED,0x4B65		// ; set led flags (not lights)

.set		K_NORMTAB,0
.set		K_SHIFTTAB,1
.set		K_ALTTAB,2
.set		K_ALTSHIFTTAB,3

.set KDGKBENT,0x4B46		// ; gets one entry in translation table
.set KDSKBENT,0x4B47		// ; sets one entry in translation table

.set KDGKBSENT,0x4B48		// ; gets one function key string entry
.set KDSKBSENT,0x4B49		// ; sets one function key string entry

    
.set KDGKBDIACR,0x4B4A		// ; read kernel accent table
.set KDSKBDIACR,0x4B4B		// ; write kernel accent table


.set KDGETKEYCODE,0x4B4C	// ; read kernel keycode table entry
.set KDSETKEYCODE,0x4B4D	// ; write kernel keycode table entry

.set KDSIGACCEPT,0x4B4E		// ; accept kbd generated signals

.set KDGHWCLK,0x4B50		// ; get hardware clock
.set KDSHWCLK,0x4B51  		// ; set hardware clock

.set KDKBDREP,0x4B52  		// ; set keyboard delay/repeat rate;
			        // ; actually used values are returned

.set KDFONTOP,0x4B72		// ; font operations


.set KD_FONT_OP_SET,0		// ; Set font
.set KD_FONT_OP_GET,1		// ; Get font
.set KD_FONT_OP_SET_DEFAULT,2	// ; Set font to default, data points to name / NULL
.set KD_FONT_OP_COPY,3		// ; Copy from another console

.set KD_FONT_FLAG_DONT_RECALC,1	// ; Don't recalculate hw charcell size [compat]
// ;#ifdef __KERNEL__
.set KD_FONT_FLAG_OLD,0x80000000// ; Invoked via old interface [compat]
// ;#endif

// ; note: 0x4B00-0x4B4E all have had a value at some time;
// ;   don't reuse for the time being
// ; note: 0x4B60-0x4B6D, 0x4B70-0x4B72 used above

#define UINT(x)   .rept (x);.long 0;.endr
#define INT(x)    .rept (x);.long 0;.endr
#define ULONG(x)  .rept (x);.long 0;.endr
#define USHORT(x) .rept (x);.word 0;.endr
#define UCHAR(x)  .rept (x);.byte 0;.endr
#define U8(x)     .rept (x);.byte 0;.endr

// ; structs:
    .struct 0
consolefontdesc:
consolefontdesc.charcount:	USHORT(1)	// ;	; characters in font (256 or 512)
consolefontdesc.charheight:	USHORT(1)	// ;	; scan lines per character (1-32)
consolefontdesc.chardata:	ULONG(1)	// ;512	;char *chardata; font data in expanded form

    .struct 0
unipair:
unipair.unicode:	USHORT(1)
unipair.fontpos:	USHORT(1)

    .struct 0
unimapdesc:
unimapdesc.entry_ct:	USHORT(1)
unimapdesc.entries:	ULONG(1)

    .struct 0
unimapinit:
.advised_hashsize:	USHORT(1)	// ; 0 if no opinion
.advised_hashstep:	USHORT(1)	// ; 0 if no opinion
.advised_hashlevel:	USHORT(1)	// ; 0 if no opinion

    .struct 0
kbentry:
kbentry.kb_table:	UCHAR(1)
kbentry.kb_index:	UCHAR(1)
kbentry.kb_value:	USHORT(1)
    
    .struct 0
kbsentry:
kbsentry.kb_func:	UCHAR(1)
kbsentry.kb_string:	UCHAR(512)

    .struct 0
kbdiacr:
kbdiacr.diacr:		UCHAR(1)
kbdiacr.base:		UCHAR(1)
kbdiacr.result:		UCHAR(1)
kbdiakre:

    .struct 0
kbdiacrs:
kbdiacrs.kb_cnt:        UINT(1) // ; number of entries in following array
kbdiacrs.kbdiacr:	U8(256*3)// ; MAX_DIACR from keyboard.h

    .struct 0
kbkeycode:
kbkeycode.scancode:	UINT(1)
kbkeycode.keycode:	UINT(1)

    .struct 0
kbd_repeat:
kbd_repeat.delay:	INT(1)		// ; in msec; <= 0: don't change
kbd_repeat.rate:	INT(1)		// ; in msec; <= 0: don't change

    .struct 0
console_font_op:
console_font_op.op:	UINT(1)		// ; operation code KD_FONT_OP_*
console_font_op.flags:	UINT(1)		// ; KD_FONT_FLAG_*
console_font_op.width:	UINT(1)		// ; font size
console_font_op.height:UINT(1)
console_font_op.charcount:UINT(1)
console_font_op.cdata:	ULONG(1)	// ; font data with height 32

// ; neither typed nor used (by name, in kernel 2.2.14):
    .struct 0
hwclk_time:
hwclk_time.sec:	INT(1) // ;unsigned	 0..59
hwclk_time.min:	INT(1) // ;unsigned	 0..59
hwclk_time.hour:INT(1) // ;unsigned	 0..23
hwclk_time.day:	INT(1) // ;unsigned	 1..31
hwclk_time.mon:	INT(1) // ;unsigned	 0..11
hwclk_time.year:INT(1) // ;unsigned	 70...
hwclk_time.wday:INT(1) // ;		 0..6, 0 is Sunday, -1 means unknown/don't set

#endif // ;_LINUX_KD_H
//
// =========================================================================;
// kd.inc <eof>
