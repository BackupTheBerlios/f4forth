* f.i.g. forth by Laurence Reeves - 1991

        section forth   for the gst-macro-assembler
        pagewid 120
        pagelen 20000   (no form feed)

* asm source by Lau
* 	NOT added
* 	bugs at MAX & WARM fixed
*	preventing endless 0 +LOOP
* otherwise unchanged, .hpr'91

* assembler:
*  strings/numbers/-expressions evaluated and assembled in [] brackets
*  equates as labels by their names
*  n1<<n2 :  shift n1 the no. of n2 places to the left
*  n1!n2  :  logical or
*  n1&n2  :  logical and
*  .len(variable) : evaluates the length of the chars of the vari
*  .left(variable,n1) : the leftmost n1 chars of the vari
*  .right(...)              right...
*
* variables:
*  name setstr [variable] or [expression] or {characters}   string
*  name setnum [variable] or number                         number
*
*    dc.b    ''''       assembles one single quote as string data
*    '`' instead of '[' to substitute the left bracket which this
*                       assembler wouldn't accept as string data
*
*
*  create macro t,xaddr          defines a macro:
*  name         parameters       parameters being evaluated as variables
*  [.lab]                        insert the call label
*  name  maclab                  local macro label

* qdos:
*  trap #0
*  used to enter the supervisor state
* trap #1
*  system management
* trap #2
*  i/o allocation
* trap #3
*  i/o operations
*  all i/o operations expect the channel identifier in a0.l
*  the opcode in d0.l(.b) and the timeout in d3.w (-1 forever, +ve * 20 ms)
* qdos-strings
*  are always preceeded by a count-word unless stated
* trap #4
*  preceeding a call to trap #2 or trap #3 the latter would
*  access a0.l (trap#2) or a1 (trap#3) offset by a6.l
* trap #5 to trap #15
*  user definable

dataspace equ   4000            enough to be getting on with!
        data    dataspace

* Registers: (D)estroyable, (I)nput only, (F)orth reserved
     ; d0-d3 (D)
     ; d4-d6 (D) never touched! user code can communicate
     ;    d7.msw (D) zeroed at boot, cold or warm starts
up      setstr  d7  lsw (f) user pointer, needing offset by a6
     ;          a0      (d) destroyabel, used for channel-id's
t       setstr  a1      (i) absolute address of entered code
w       setstr  a2      (i) parameter field pointer
nx      setstr  a3      (f) jump address to leave assembler
ip      setstr  a4      (f) forth next instruction pointer
rp      setstr  a5      (f) return stack pointer
Z       ;       a6      (f) always the program base address
sp      setstr  a7      (f) forth stack pointer, params in/out

* ';;;' code corrected/changed for safety

*
chans   equ     16              number of channels supported (must be even)
*
ssbits  equ     6               is nice for 64 byte headers
ssize   equ     1<<ssbits
nbuf    equ     1024/ssize
*
bmag    equ     (ssize+4)*nbuf  buffer space
uarea   equ     64              user area
tibrp   equ     256             combined tib & return stack
*
* standard layout for QDOS, and me
*
        move.w  (sp)+,d7
        bne.s   gotcha          ?
        bra.s   defscr
*** qdos job-header
        dc.w    $4AFB,2,'QF' to lengthen, patch 'screen' to elsewhere.
screen  dc.w    $201,$207,448,202,32,14  border colr & width, paper, ink; window
savehead ; at locn 24
        dc.l    0               file length, set to 'here' in case save" ser" used
        dc.b    0,1             access code = 0, filetype = executeable
        dc.l    dataspace,0
* 32.w: dataspace, may be altered, but keep <= 32K-'here'!
* 
stdio   equ     *+2
        dc.b    chans-2,chans-1,0,1,2,3
* The above give a simple redirection of our channel table.
* 38: dr0 (r/w, etc)
* 39: dr1 (ditto, plus save" and chain")
* 40: standard input (expect, key, ?terminal, etc)
* 41: standard output (type, emit, etc)
* 42: reserved for user code, e.g. as an input or update file
* 43: reserved for user code, e.g. as an output or work file
*
* boot up code
*
* starting a qdos job by        EX jobfilename,#channel1,..;commandstring
* bot|A6           |(A4,A6)              ..(a7)..    |(A5,A6) top
*    |codespace    |dataspace   initial sp:parameters|
* optional parameters on stack: count.w|qdos-channels.l | count.w|command stg|
*
defscr  lea     screen-z(a6),a1 default window colours & sizes
        move.w  198,a2          qdos vector
        jsr     (a2)            > channel open and window set up routine
        move.l  a0,-(sp)        put default screen on stack
        moveq   #1,d7           say it's there
gotcha  movem.l (sp),d0-d1/d3-d6/a0-a4
        lsl.w   #2,d7
        add.w   d7,sp           top of channels
        lea     -chans*4(sp),a5 channel table base
        subq.w  #4,d7           did we just have one channel?
        bne.s   notone          ?
        move.l  d0,d1           yes - make ch1=ch0
        addq.w  #4,d7           and pretend it was there all along
notone  addq.w  #4*2,d7         put back and include channel 2
        moveq   #-1,d2          channel 2 is initially closed
        movem.l d0-d6/a0-a4,(a5) set down what seems to be going
        add.l   a5,d7
chclear move.l  d2,-(sp)        set remaining channels closed
        cmp.l   sp,d7
        bne.s   chclear         ?
*
* Channels are set up as:
* 0: primary input: first EX chan or screen
* 1: primary output: second or only EX chan or screen
* 2: uncommitted
* 3-11: may also be passed in by EX
* 12-13: uncommitted
* 14: Used by r/w as dr0.
* 15: Used by r/w as dr1. Also used by chain" and save".
*
        move.l  a5,a0
        sub.l   a6,a5
        move.w  a5,limit+2-z(a6)
        lea     -bmag(a5),a1
        move.w  a1,first+2-z(a6)
        move.w  a1,prev+2-z(a6)
        move.w  a1,use+2-z(a6)
        lea     -uarea(a1),[rp] r0
        move.w  [rp],[up]
        lea     -tibrp([rp]),a1 tib
        move.w  a1,sp           s0
        movem.w [up]/a1/[rp]/sp,origin+16-z(a6)
*
        add.l   a6,sp
btclr           
        clr.l   -(a0)
        cmp.l   sp,a0
        bcc.s   btclr           ?
* drops into cold start jump at origin

*
* macros
*

prevlink setstr Z
prevnum setnum  0
previmm setnum  0
sings   setstr  {'`Xx};'
*
create  macro   t,xaddr
z[prevnum] equ  [previmm]
l       setnum  [.len(t)]
prevnum setnum  [prevnum]+1
        dcb.b   (*+1+[l]-z)&1+1,z[prevnum]![l] for cet, etc
        ifnum   [l]             = 1 goto single
        ifstr   {[.left(t,1)]} <> {`} goto ord;'
        dc.b    $5B,'compile',']'!$80
        goto    dclfa
single  maclab  
        ifnum   [.instr(sings,t)] = 0 goto nsing
        ifstr   {[t]} <> ' goto ntick
        dc.b    ''''!$80
        goto    dclfa
ntick   maclab  
        ifstr   {[t]} <> {`} goto nbrac;'
        dc.b    91!128
        goto    dclfa
nbrac   maclab  
        dc.b    -128
        goto    dclfa
nsing   maclab  
        dc.b    '[t]'!$80
        goto    dclfa
ord     maclab  
        dc.b    '[.left(t,[l]-1)]','[.right(t,1)]'!$80
dclfa   maclab  
        dc.w    [prevlink]-z
[.lab]  dc.w    [xaddr]-z
prevlab setstr  [.lab]
prevlink setstr [.lab]-[l]-3
previmm setnum  128
        endm    
*
immed   macro   
previmm setnum  $140-[previmm]   {320}
        endm    
*
;endit   macro
;z[prevnum] equ  [previmm]
;ntop    equ     [prevlink]
;top     ds.w    0
;        end
;        endm

*
* base of fig-FORTH proper
*
origin  jmp     docold-z(a6)
        jmp     dowarm-z(a6)
        dc.l    $4ef6ec54       lwr68k in radix 36 {1324805204}
* cold/warm move the following to the user area
        dc.w    ntop-z          last name in dictionary
        dc.w    194             backspace character (never used)
        ds.w    4               initial up, s0, r0 and tib - filled in at prog exec
        dc.w    31              initial max width for names
        dc.w    0               0: no disk, 1: dr0 available, -1: (abort) in use
* warm stops here
        dc.w    top-z           initial fence address
        dc.w    top-z           initial top of directory
        dc.w    vl0-z           initial vocabulary link pointer
* cold goes all the way, and also resets forth+6 from origin+$c

lit     create  {lit} *+2
        move.w  ([ip])+,-(sp)
nextloc move.w  ([ip])+,[w]
exent   add.l   a6,[w]
        move.w  ([w])+,a1
        add.l   a6,a1
        jmp     (a1)

dlit    create  {dlit} *+2
        move.l  ([ip])+,-(sp)
        jmp     ([nx])

execute create  {execute} *+2
        move.w  (sp)+,[w]
        bra.s   exent

branch  create  {branch} Xbranch

zerobranch create {0branch} *+2
        tst.w   (sp)+
        beq.s   xbranch         ?
bump    addq.l  #2,[ip]
        jmp     ([nx])

ploopq  create  {(loop)} *+2
        moveq   #1,d0
loopcmp add.w   ([rp])+,d0
        cmp.w   ([rp]),d0
        bge.s   loopex          ?
loopon  move.w  d0,-([rp])
xbranch add.w   ([ip]),[ip]
        jmp     ([nx])

pplusloopq create {(+loop)} *+2
        move.w  (sp)+,d0
;;; changed to avoid endless loops on zero increment:
        bgt.s     loopcmp   ;;;bpl.s     loopcmp
        bmi.s     decloop   ;+
        add.l     ([rp]),d0 ; ... 0 +loop
decloop add.w   ([rp])+,d0
        cmp.w   ([rp]),d0
        bgt.s   loopon          ?
loopex  addq.l  #2,[rp]
        bra.s   bump

pdoq    create  {(do)} *+2
        move.l  (sp)+,-([rp])
        jmp     ([nx])

digit   create  {digit} *+2
        move.w  (sp)+,d1
        moveq   #-'0',d0
        add.w   (sp),d0
        bmi.s   putfalse        ?
        cmp.w   #10,d0
        bcs.s   digbas          ?
        cmp.w   #'a'-'0',d0
        bcs.s   diglow          ?
        sub.w   #'a'-'A',d0
diglow  cmp.w   #'A'-'0',d0
        bcs.s   putfalse        ?
        subq.w  #'A'-'0'-10,d0
digbas  move.w  d0,(sp)
        cmp.w   d1,d0
        bcs.s   xone            ?
putfalse clr.w  (sp)
        jmp     ([nx])

pfindq  create  {(find)} *+2
        moveq   #0,d0
        move.w  (sp)+,[w]
        bra.s   findent
findlink tst.b  ([w])+
        bpl.s   findlink        ?
        move.w  ([w]),[w]
findent move.w  [w],d2
        beq.s   putfalse        ?
        add.l   a6,[w]
        move.w  (sp),a1
        add.l   a6,a1
        move.b  ([w])+,d0
        moveq   #63,d2
        and.b   d0,d2
        sub.b   (a1)+,d2
        bne.s   findlink        ?
findscan moveq  #127,d2
        and.b   ([w]),d2
        move.b  (a1)+,d1
        eor.b   d2,d1
        beq.s   findchk         ?
        cmp.b   #32,d1
        bne.s   findlink        ?
        or.b    d1,d2
        sub.b   #'a',d2
        cmp.b   #26,d2
        bcc.s   findlink        ?
findchk tst.b   ([w])+
        bpl.s   findscan        ?
        addq.w  #4,[w]
        sub.w   a6,[w]
        move.w  [w],(sp)
        move.w  d0,-(sp)
xone    move.w  #1,-(sp)
        jmp     ([nx])

enclose create  {enclose} *+2
        move.w  (sp)+,d0
        move.w  (sp),[w]
        add.l   a6,[w]
        move.w  [w],d1
        addq.w  #1,d1
encldskip cmp.b ([w])+,d0
        beq.s   encldskip       ?
        move.w  [w],-(sp)
        sub.w   d1,(sp)
encword move.b  ([w])+,d2
        beq.s   encend          ?
        cmp.b   d2,d0
        bne.s   encword         ?
encend  move.w  [w],-(sp)
        sub.w   d1,(sp)
        tst.b   d2
        beq.s   encnull         ?
        addq.w  #1,[w]
encnull sub.w   d1,[w]
        move.w  [w],-(sp)
        jmp     ([nx])

* qdos routine:
* fetch max. d2.w bytes or until <lf> from channel-id a0.l to  a1.l)+
*            d3.w waiting for(-1)ever, +ve no. of 20ms periods
* flag -ve error code in d0.l, or 0 if ok, -1 time elapsed
xexpect move.w  (sp)+,d2
        move.w  (sp)+,a1
        moveq   #2<<3!4!2,d0    io.fline wait stdin
        bsr.s   iocomm          >
        bpl.s   expa1ok         ?
        addq.l  #1,a1
        sf      stdio-z(a6)     any error, try resetting to chan 0
expa1ok sf      (a1)
        sf      -(a1)
        jmp     ([nx])

xuless  move.w  (sp)+,d0
        cmp.w   (sp)+,d0
hitrue  bhi.s   xone            ?
xzero   clr.w   -(sp)
        jmp     ([nx])

xgreat  move.l  (sp),d0
        swap    d0
        move.l  d0,(sp)
xless   move.w  (sp)+,d0
        cmp.w   (sp)+,d0
        ble.s   xzero           ?
        bra.s   xone

* qdos routine:
* io.cursen     enable cursor of (window-)channel a0
* io.fbyte      fetch one byte from channel-id a0 to d1.b
xkey    moveq   #14<<3!4!2,d0   io.cursen wait stdin
        bsr.s   iocomm          >
        moveq   #1<<3!4!2,d0    io.fbyte wait stdin
        bsr.s   iocomm          >
        move.w  d1,-(sp)
        sf      (sp)
        jmp     ([nx])

* qdos: io.pend test wether bytes are waiting for beeing fetched
*       flag result d0.l =0 if, d0=-1 time elapsed
xquestterminal
        moveq   #14<<3!0!2,d0   io.cursen nowait stdin
        bsr.s   iocomm          >
        moveq   #0<<3!0!2,d0    io.pend nowait stdin
        bsr.s   iocomm          >
        not.l   d0              c=0, err.nc will give z
        bra.s   hitrue

doemit  move.w  ([w]),-(sp)
xemit   move.w  (sp)+,d1
        moveq   #5<<3!4!3,d0    io.sbyte wait stdout
        moveq   #1,d2
        bra.s   iocount

* qdos:
* io.sstrg send a string of < 32K d2.w bytes from a1.l)+ to channel-id a0
xtype   move.w  (sp)+,d2
        move.w  (sp)+,a1
        moveq   #7<<3!4!3,d0    io.sstrg wait stdout
iocount add.w   d2,26(a6,[up].w)
        move.l  [nx],-(sp)
iocomm  moveq   #3,d3
        and.b   d0,d3
        move.b  stdio-2-z(a6,d3.w),d3
        bsr.s   mkchp           >
        move.l  d3,a0
        lsr.w   #3,d0
        subx.w  d3,d3
trap3   add.l   a6,a1
        move.l  a1,-(sp)
        trap    #3
        move.l  (sp)+,a1
        add.w   d1,a1           Ho hum... ERR.BP screws a1, this is for 'expect'
        tst.l   d0
        rts     

xioblo  move.w  ([ip])+,d0
        movem.w (sp)+,d2/a1
        bsr.s   iocomm          >
        bra.s   rwtst

mkchp   and.w   #15,d3
        lsl.b   #2,d3
        move.w  limit+2-z(a6),a0
        add.l   a6,a0
        lea     0(a0,d3.w),a2
        move.l  (a2),d3
        rts     

* qdos:
* fs.posab  fetch filepointer of channel-id a0 to d1.l
xrbyw   move.w  (sp)+,d2
        moveq   #0,d1
        move.w  (sp)+,d1
        move.w  #66<<3!4!0/2,d0 fs.posab wait dr0
        asl.w   #2,d1           block.b14->x
        addx.w  d0,d0           if bit 14 was set, change to dr1
        lsl.l   #ssbits-2,d1
        bsr.s   iocomm          >
        move.w  (sp)+,a1
        bmi.s   rwerr           ?
        moveq   #7,d0           io.sstrg
        lsr.b   d2,d0           if flag was 1, convert to 3=io.fstrg
        moveq   #ssize,d2
        bsr.s   trap3           >
rwtst   bpl.s   gonext          ?
rwerr   lea     quit+2-z(a6),[ip]
        move.l  d0,-(sp)
        move.w  #1,-(sp)
xreport move.w  (sp)+,d3
        move.l  (sp),d0
        move.l  [nx],(sp)
        bsr.s   mkchp           >
        move.l  d3,a0
        move.w  204,a2
        jmp     (a2)

xert    move.l  (sp)+,d0
        bra.s   rwtst

* qdos:
* close channel identified in a0.l
Xclose  move.w  (sp)+,d3
        move.l  [nx],-(sp)
closeit bsr.s   mkchp           >
        moveq   #-1,d1          (also for open)
        move.l  d1,(a2)
        moveq   #2,d0           io.close
        cmp.w   d3,d0
        bge.s   noclose         ?ignore -ve and prevent close of qdos 0-2
        moveq   #chans-1,d2
clchk   cmp.l   (a0)+,d3
        dbeq    d2,clchk
        beq.s   noclose         ?prevent close of duplicate entry
        move.l  d3,a0
        trap    #2
noclose rts

* qdos:
* open a channel to the device with its name at a0.l)+
* return its identifier in a0.l
xopen   move.w  (sp)+,d3
        bsr.s   closeit         >(kindly sets d1=-1 for us)
        movem.w (sp)+,d3/a0
        add.l   a6,a0
        moveq   #1,d0           io.open
        trap    #2
        move.l  d0,-(sp)
        blt.s   gonext          ?
        move.l  a0,(a2)
gonext  jmp     ([nx])

expect  create  {expect} Xexpect
key     create  {key} Xkey
questterminal create {?terminal} Xquestterminal
emit    create  {emit} Xemit
type    create  {type} Xtype

docold  movem.w origin+12-z(a6),d0-d3/[up]/a0-a5
        move.w  d0,forth+6-z(a6)
        movem.w a3-a5,16(a6,d2.w)
dowarm  movem.w origin+12-z(a6),d0-d1/[up]/a0-a4
        movem.w d0-d1/[up]/a0-a4,0(a6,[up].w)
        lea     abort+2-z(a6),[ip]
        lea     nextloc-z(a6),[nx]
xrpstore move.w 8(a6,[up].w),[rp]
        add.l   a6,[rp]
        jmp     ([nx])

domove  movem.w (sp)+,d1/a0-a1  ( from to n --- )
fillent add.l   a6,a0
        add.l   a6,a1
rollent move.w  ([w]),d2
        move.w  d2,liveinstr-z(a6)
        lsl.b   #2,d2
livedown bpl.s  liveent         ?
        add.w   d1,a0
        add.w   d1,a1
        lsl.w   #2,d2
        bra.s   livedown

liveinstr dc.w  0       <=modified by the {move}-routines
liveent dbra    d1,liveinstr    ?
        jmp     ([nx])

xfill   movem.w (sp)+,d0-d1/a0  ( addr n byte --- )
        bra.s   fillent

* move.b d0,(a0)+ 10C0
xroll   move.l  sp,a1
        move.w  (sp)+,d1
        move.w  d1,d0
        add.w   d0,d0
        move.w  0(a1,d0.w),(a1)
        move.l  sp,a0
        bra.s   rollent

cmove   create  {cmove} domove
        move.b  (a1)+,(a0)+     10d9
lesscmove create {<cmove} domove
        move.b  -(a1),-(a0)     1121
wmove   create  {move} domove
        move.w  (a1)+,(a0)+     30d9
lesswmove create {<move} domove
        move.w  -(a1),-(a0)     3121

umul    create  {u*} *+2
        move.w  (sp)+,d0
        mulu    (sp)+,d0
        bra.s   upushl

uby     create  {u/} *+2
        move.w  (sp)+,d1
        move.l  (sp)+,d0
        divu    d1,d0
uswapsh swap    d0
upushl  move.l  d0,-(sp)
        jmp     ([nx])

mmul    create  {m*} *+2
        move.w  (sp)+,d0
        muls    (sp)+,d0
        bra.s   upushl

mby     create  {m/} *+2
        move.w  (sp)+,d1
        move.l  (sp)+,d0
        divs    d1,d0
        bra.s   uswapsh

andx    create  {and} Xand
docand  move.w  ([w]),-(sp)
xand    move.w  (sp)+,d0
        and.w   d0,(sp)
        jmp     ([nx])

dashtwoand dc.w docand-z,-2
onefand dc.w    docand-z,31

orx     create  {or} *+2
        move.w  (sp)+,d0
        or.w    d0,(sp)
        jmp     ([nx])

xor     create  {xor} *+2
        move.w  (sp)+,d0
        eor.w   d0,(sp)
        jmp     ([nx])

spfetch create  {sp@} Xspfetch
spstore create  {sp!} *+2
        move.w  6(a6,[up].w),sp
        add.l   a6,sp
        jmp     ([nx])

rpstore create  {rp!} Xrpstore
rpfetch create  {rp@} Xrpfetch

semis   create  {;s} *+2
        move.w  ([rp])+,[ip]
        add.l   a6,[ip]
        jmp     ([nx])

leave   create  {leave} *+2
        move.w  ([rp]),2([rp])
        jmp     ([nx])

greatr  create  {>r} Xgreatr
greatrr dc.w    *+2-z
        move.w  (sp),-(sp)
xgreatr move.w  (sp)+,-([rp])
        jmp     ([nx])

rgreat  create  {r>} *+2
        move.w  ([rp])+,-(sp)
        jmp     ([nx])

rx      create  {r} *+2
xrx     move.w  ([rp]),-(sp)
        jmp     ([nx])

ix      create  {i} Xrx
j       create  {j} *+2
        move.w  4([rp]),-(sp)
        jmp     ([nx])

nnot    create    not       *+2       ;;;additionally
        not.w     (sp)
        jmp       ([nx])

zeroeq  create  {0=} Xzeroeq
xeq             
        move.w  (sp)+,d0
        sub.w   d0,(sp)
xzeroeq subq.w  #1,(sp)
logic   subx.w  d0,d0
        move.w  d0,(sp)
xminus  neg.w   (sp)
        jmp     ([nx])

xabs    move.w  (sp),-(sp)
xplusdash tst.w (sp)+
        blt.s   xminus          ?
        jmp     ([nx])

zeroless create {0<} *+2
        asl     (sp)
        bra.s   logic

plus    create  {+} Xplus
docplus move.w  ([w]),-(sp)
xplus   move.w  (sp)+,d0
        add.w   d0,(sp)
        jmp     ([nx])

dplus   create  {d+} *+2
        move.l  (sp)+,d0
        add.l   d0,(sp)
        jmp     ([nx])

minus   create  {minus} Xminus
dminus  create  {dminus} Xdminus
xdabs   move.w  (sp),-(sp)
xdplusdash tst.w (sp)+
        bge.s   nodminus        ?
xdminus neg.l   (sp)
nodminus jmp    ([nx])

* qdos:
* remove the 4th job: close its channels, link its memory back to the system
bye     create  {bye} *+2
        moveq   #0,d3
        moveq   #-1,d1
        moveq   #5,d0
        trap    #1      (ends in nowhere, that is: the scheduler)

***

plusstore create {+!} *+2
        move.w  (sp)+,[w]
        move.w  (sp)+,d0
        add.w   d0,0(a6,[w].w)
        jmp     ([nx])

toggle  create  {toggle} Xtoggle
dotog   move.w  ([w]),-(sp)
xtoggle move.w  (sp)+,d0
        move.w  (sp)+,[w]
        eor.b   d0,0(a6,[w].w)
        jmp     ([nx])

msbtog  dc.w    dotog-z,128
immtog  dc.w    dotog-z,64
smutog  dc.w    dotog-z,32

fetch   create  {@} *+2
        move.w  (sp)+,[w]
        add.l   a6,[w]
*fetchlong
        move.w  ([w]),-(sp)
        jmp     ([nx])

*Xfetchl
* move.l (sp)+,[w]
* bra.s fetchlong

cfetch  create  {c@} *+2
        move.w  (sp)+,[w]
cfetchw add.l   a6,[w]
*cfetchlong
        moveq   #0,d0
        move.b  ([w]),d0
        move.w  d0,-(sp)
        jmp     ([nx])

xcount  move.w  (sp),[w]
        addq.w  #1,(sp)
        bra.s   cfetchw

*Xcfetchl
* move.l (sp)+,[w]
* bra.s cfetchlong

store   create  {!} *+2
        move.w  (sp)+,[w]
        add.l   a6,[w]
*storelong
        move.w  (sp)+,([w])
        jmp     ([nx])

*Xstorel
* move.l (sp)+,[w]
* bra.s storelong

cstore  create  {c!} *+2
        move.w  (sp)+,[w]
        add.l   a6,[w]
*cstorelong
        move.w  (sp)+,d0
        move.b  d0,([w])
        jmp     ([nx])

*Xcstorel
* move.l  (sp)+,[w]
* bra.s cstorelong

oneplus create  {1+} Xoneplus
* : 1+ 1 + ;

onedash create  {1-} docplus
        dc.w    -1
* : 1- 1 - ;

twoplus create  {2+} *+2
        addq.w  #2,(sp)
        jmp     ([nx])
* : 2+ 2 + ;

cfa     create  {cfa} twodash+2
* : cfa 2 - ;

twodash create  {2-} *+2
        subq.w  #2,(sp)
        jmp     ([nx])
* : 2- 2 - ;

blkfetch dc.w   *+2-z
        move.w  22(a6,[up].w),-(sp)
        jmp     ([nx])
* : blk@ blk @ ;

zeroinstore dc.w *+2-z
        clr.w   24(a6,[up].w)
        jmp     ([nx])
* : 0in! 0 in ! ;

statefetch dc.w *+2-z
        move.w  36(a6,[up].w),-(sp)
        jmp     ([nx])
* : state@ state @ ;

dash    create  {-} Xdash
* : - minus + ;

overdash dc.w   *+2-z
* : over- over - ;
        move.w  2(sp),-(sp)
xdash   move.w  (sp)+,d0
        sub.w   d0,(sp)
        jmp     ([nx])

eq      create  {=} Xeq
* : = - 0= ;

uless   create  {u<} Xuless
less    create  {<} Xless
great   create  {>} Xgreat
* : > swap < ;

* : traverse swap begin over + 7f over c@ < until swap drop ;
traverse create {traverse} *+2
        move.w  (sp)+,d1
travloop add.w  d1,(sp)
        move.w  (sp),d0
        tst.b   0(a6,d0.w)
        bpl.s   travloop        ?
        jmp     ([nx])

dashtrailing create {-trailing} *+2
        subq.w  #1,(sp)
        bmi.s   xoneplus        ?
        move.l  (sp),d0
        add.w   (sp),d0
        cmp.b   #' ',0(a6,d0.w)
        beq.s   dashtrailing+2  ?
xoneplus        
        addq.w  #1,(sp)
        jmp     ([nx])
*: -trailing dup 0 do over over + 1- c@
* bl - if leave else 1- then loop ;

fill    create  {fill} Xfill
        move.b  d0,(a0)+        10c0
* : fill swap >r over c! dup 1+ r> 1- cmove ;

basecpu dc.w    *+2-z
        move.w  38(a6,[up].w),-(sp)
        move.w  #36,38(a6,[up].w)
        move.l  origin+8-z(a6),-(sp)
        jmp     ([nx])

cold    create  {cold} origin

warm    create  {warm} origin+4 ;;;origin+2

sdashgreatd create {s->d} *+2
        move.w  (sp)+,[w]
        move.l  [w],-(sp)
        jmp     ([nx])
* : s->d dup 0< minus ;

plusdash create {+-} Xplusdash
* : +- 0< if minus then ;

dplusdash create {d+-} Xdplusdash
* : d+- 0< if dminus then ;

abs     create  {abs} Xabs
* : abs dup +- ;

dabs    create  {dabs} Xdabs
* : dabs dup d+- ;

min     create  {min} *+2
        move.w  (sp)+,d0
        cmp.w   (sp),d0
        bge.s   nomin           ?
mmput   move.w  d0,(sp)
nomin   jmp     ([nx])

* : min over over > if swap then drop ;
max     create  {max} *+2   ; ??? ->
        move.w  (sp)+,d0        ;dc.w    swapx-z
        cmp.w   (sp),d0         ;dc.w    min-z
        bgt.s   mmput           ;dc.w    semis-z
        jmp     ([nx])

*fetchl create  {@l} Xfetchl
*cfetchl create {c@l} Xcfetchl
*storel create  {!l} Xstorel
*cstorel create {c!l} Xcstorel

* qdos channels
open    create  {open} Xopen    ( name type chan --- err.l )
close   create  {close} Xclose  ( chan --- )
report  create  {report} Xreport ( err.l chan --- )
ert     dc.w    xert-z          ( err.l --- )
* : ert dup 0< if 1 report quit then drop drop ;

ioblo   dc.w    xioblo-z        ( addr len --- ) ( opn follows like 'lit' )
fsheads equ     70<<3!4!1+z     fs.heads wait dr1
fssave  equ     73<<3!4!1+z     fs.save wait dr1
temp    dc.w    doconstant-z,stdio-1-z ( --- stdio-1 )
* 0e constant temp

pwquoteq create {(w")} Xpwquoteq ( --- addr )
*: (w") r r @ 1+ 1 or 1+ r> + >r ;

twofetch create {2@} *+2
        move.w  (sp)+,[w]
        add.l   a6,[w]
        move.l  ([w]),-(sp)
        jmp     ([nx])
* : 2@ dup 2+ @ swap @ ;  ( addr --- d1 )

twostore create {2!} *+2
        move.w  (sp)+,[w]
        add.l   a6,[w]
        move.l  (sp)+,([w])
        jmp     ([nx])
* : 2! rot over 2+ ! ! ;  ( d1 addr --- )

xpwquoteq move.w [ip],[w]
        moveq   #1,d0
        and.w   ([ip]),d0
        add.w   ([ip])+,d0
        add.w   d0,[ip]
        bra.s   pushl

* stacks *

xspfetch move.w sp,[w]
pushl   sub.w   a6,[w]  ;;bra.s   dovariable
pushw   move.w  [w],-(sp)
        jmp     ([nx])

xrpfetch move.w [rp],[w]
        bra.s   pushl

over    create  {over} *+2
        move.w  2(sp),-(sp)
        jmp     ([nx])

xtwoover move.l 4(sp),-(sp)
        jmp     ([nx])

drop    create  {drop} Xdrop
xtwodrop addq.l #2,sp
xdrop   addq.l  #2,sp
        jmp     ([nx])

swapx   create  {swap} *+2
        move.l  (sp),d0
        swap    d0
        move.l  d0,(sp)
        jmp     ([nx])

xtwoswap move.l (sp)+,d0
        move.l  (sp),d1
        move.l  d0,(sp)
        move.l  d1,-(sp)
        jmp     ([nx])

dup     create  {dup} *+2
        move.w  (sp),-(sp)
        jmp     ([nx])

xtwodup move.l  (sp),-(sp)
        jmp     ([nx])

xrot    move.l  (sp)+,d0
        move.w  (sp)+,[w]
        move.l  d0,-(sp)
        bra.s   pushw

rot     create  {rot} Xrot
* : rot >r swap r> swap ;

twodrop create  {2drop} Xtwodrop
* : 2drop drop drop ; ( d --- )

twodup  create  {2dup} Xtwodup
* : 2dup over over ; ( d --- d d )

twoswap create  {2swap} Xtwoswap
* : 2swap rot >r rot r> ; ( d1 d2 --- d2 d1 )

twoover create  {2over} Xtwoover
* : 2over 4 pick 4 pick ; ( d1 d2 --- d1 d2 d1 )

* : depth sp@ s0 @ swap - 2 / ;
pick    create  {pick} *+2
        move.w  (sp),d0
        add.w   d0,d0
        move.w  0(sp,d0.w),(sp)
        jmp     ([nx])
*: pick dup + sp@ + @ ;

roll    create  {roll} Xroll
        move.w  -(a1),-(a0)     3121
*: roll >r r pick sp@ dup 2+ r> wmove drop ;

dashdup create  {-dup} *+2
        move.w  (sp),d0
        beq.s   nodup           ?
pushd0  move.w  d0,-(sp)
nodup   jmp     ([nx])
*: -dup dup if dup then ;

depth   create  {depth} *+2
        move.w  6(a6,[up].w),d0
        add.w   a6,d0
        sub.w   sp,d0
        asr.w   #1,d0
        bra.s   pushd0

* defining words *

twoconstant create {2constant} docolon
        dc.w    twovariable-z
        dc.w    psemicodeq-z
do[prevlab]     
        move.l  ([w]),-(sp)
        jmp     ([nx])

colon   create  {:} docolon
        dc.w    questexec-z
        dc.w    storecsp-z
        dc.w    current-z
        dc.w    fetch-z
        dc.w    context-z
        dc.w    store-z
        dc.w    create-z
        dc.w    carb-z
        dc.w    psemicodeq-z
previmm setnum  $140-[previmm]
do[prevlab]     
        sub.w   a6,[ip]
        move.w  [ip],-([rp])
        move.l  [w],[ip]
        jmp     ([nx])

constant create {constant} docolon
        dc.w    create-z
        dc.w    smudge-z
        dc.w    comma-z
        dc.w    psemicodeq-z
do[prevlab]     
        move.w  ([w]),-(sp)
        jmp     ([nx])

variable create {variable} docolon
        dc.w    constant-z
        dc.w    psemicodeq-z
do[prevlab]     
        sub.w   a6,[w]
        move.w  [w],-(sp)
        jmp     ([nx])

user    create  {user} docolon
        dc.w    constant-z
        dc.w    psemicodeq-z
do[prevlab]     
        move.w  ([w]),-(sp)
        add.w   [up],(sp)
        jmp     ([nx])

* compiling *

exit    create  {exit} docolon
        dc.w    compile-z
        dc.w    semis-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

semi    create  {;} docolon
        dc.w    questcsp-z
        dc.w    exit-z
        dc.w    smudge-z
        dc.w    brac-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

* constants & vari *

lumpplus dc.w   docplus-z,69
dashone dc.w    doconstant-z,-1
four    dc.w    doconstant-z,4
zero    create  {0} Xzero
one     create  {1} Xone
* 00 constant 0    01 constant 1
two     create  {2} doconstant
        dc.w    2
three   create  {3} doconstant
        dc.w    3
bl      create  {bl} doconstant
        dc.w    32
cbyl    create  {c/l} doconstant
        dc.w    64
first   create  {first} doconstant
        dc.w    0
limit   create  {limit} doconstant
        dc.w    0
bbybuf  create  {b/buf} doconstant
        dc.w    64
bbyscr  create  {b/scr} doconstant
        dc.w    16
plusorigin create {+origin} docplus
        dc.w    origin-z

* 06 user s0
* 08 user r0
tib     create  {tib} douser
        dc.w    10
width   create  {width} douser
        dc.w    12
warning create  {warning} douser
        dc.w    14
fence   create  {fence} douser
        dc.w    16
dp      create  {dp} douser
        dc.w    18
vocdashlink create {voc-link} douser
        dc.w    20
blk     create  {blk} douser
        dc.w    22
in      create  {in} douser
        dc.w    24
out     create  {out} douser
        dc.w    26
scr     create  {scr} douser
        dc.w    28
offset  create  {offset} douser
        dc.w    30
context create  {context} douser
        dc.w    32
current create  {current} douser
        dc.w    34
state   create  {state} douser
        dc.w    36
base    create  {base} douser
        dc.w    38
dpl     create  {dpl} douser
        dc.w    40
fld     create  {fld} douser
        dc.w    42
csp     create  {csp} douser
        dc.w    44
rsharp  create  {r#} douser
        dc.w    46
hld     create  {hld} douser
        dc.w    48

* hi-level words *

here    create  {here} docolon
        dc.w    dp-z
        dc.w    fetch-z
        dc.w    semis-z

allot   create  {allot} docolon
        dc.w    dp-z
        dc.w    plusstore-z
        dc.w    semis-z

comma   create  {,} docolon
        dc.w    here-z
        dc.w    store-z
        dc.w    two-z
        dc.w    allot-z
        dc.w    semis-z

ccomma  create  {c,} docolon
        dc.w    here-z
        dc.w    cstore-z
        dc.w    one-z
        dc.w    allot-z
        dc.w    semis-z


latest  create  {latest} docolon
        dc.w    current-z
        dc.w    fetch-z
        dc.w    fetch-z
        dc.w    semis-z

lfa     create  {lfa} docplus
        dc.w    -4
* : lfa 4 - ;

fourdash equ    lfa
fiveplus dc.w   docplus-z,5
fivedash dc.w   docplus-z,-5

nfa     create  {nfa} docolon
        dc.w    fivedash-z
        dc.w    dashone-z
        dc.w    traverse-z
        dc.w    semis-z

pfa     create  {pfa} docolon
        dc.w    one-z
        dc.w    traverse-z
        dc.w    fiveplus-z
        dc.w    semis-z

storecsp create {!csp} docolon
        dc.w    spfetch-z
        dc.w    csp-z
        dc.w    store-z
        dc.w    semis-z

questerror create {?error} docolon
        dc.w    swapx-z
        dc.w    zerobranch-z
        dc.w    m1-*
        dc.w    error-z
        dc.w    branch-z
        dc.w    m2-*
m1      dc.w    drop-z
m2      dc.w    semis-z

questcomp create {?comp} docolon
        dc.w    statefetch-z
        dc.w    zeroeq-z
        dc.w    lit-z,17
        dc.w    questerror-z
        dc.w    semis-z

questexec create {?exec} docolon
        dc.w    statefetch-z
        dc.w    lit-z,18
        dc.w    questerror-z
        dc.w    semis-z

questpairs create {?pairs} docolon
        dc.w    dash-z
        dc.w    lit-z,19
        dc.w    questerror-z
        dc.w    semis-z

questcsp create {?csp} docolon
        dc.w    spfetch-z
        dc.w    csp-z
        dc.w    fetch-z
        dc.w    dash-z
        dc.w    lit-z,20
        dc.w    questerror-z
        dc.w    semis-z

questloading create {?loading} docolon
        dc.w    blkfetch-z
        dc.w    zeroeq-z
        dc.w    lit-z,22
        dc.w    questerror-z
        dc.w    semis-z

brac    create  {`} docolon;'
        dc.w    zero-z
        dc.w    state-z
        dc.w    store-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

carb    create  {]} docolon
        dc.w    lit-z,192
        dc.w    state-z
        dc.w    store-z
        dc.w    semis-z

compile create  {compile} docolon
        dc.w    questcomp-z
        dc.w    rgreat-z
        dc.w    dup-z
        dc.w    twoplus-z
        dc.w    greatr-z
        dc.w    fetch-z
        dc.w    comma-z
        dc.w    semis-z

smudge  create  {smudge} docolon
        dc.w    latest-z
        dc.w    smutog-z
        dc.w    semis-z

hex     create  {hex} docolon
        dc.w    lit-z,16
        dc.w    base-z
        dc.w    store-z
        dc.w    semis-z

decimal create  {decimal} docolon
        dc.w    lit-z,10
        dc.w    base-z
        dc.w    store-z
        dc.w    semis-z

psemicodeq create {(;code)} docolon
        dc.w    rgreat-z
        dc.w    latest-z
        dc.w    pfa-z
        dc.w    cfa-z
        dc.w    store-z
        dc.w    semis-z

semicode create {;code} docolon
        dc.w    questcsp-z
        dc.w    compile-z
        dc.w    psemicodeq-z
        dc.w    brac-z
        dc.w    smudge-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

lessbuilds create {<builds} docolon
        dc.w    zero-z
        dc.w    constant-z
        dc.w    semis-z

doesgreat create {does>} docolon
        dc.w    rgreat-z
        dc.w    latest-z
        dc.w    pfa-z
        dc.w    store-z
        dc.w    psemicodeq-z
do[prevlab]     
        sub.w   a6,[ip]
        move.w  [ip],-([rp])
        move.w  ([w])+,[ip]
        add.l   a6,[ip]
        jmp     dovariable-z(a6)

count   create  {count} Xcount
* : count dup 1+ swap c@ ;


space   create  {space} doemit
        dc.w    32
cr      create  {cr} doemit
        dc.w    10
asciiquote dc.w doconstant-Z,0+'"'

pdotquoteq create {(.")} docolon
        dc.w    rgreat-z
        dc.w    count-z
        dc.w    twodup-z
        dc.w    plus-z
        dc.w    oneplus-z
        dc.w    dashtwoand-z
        dc.w    greatr-z
        dc.w    type-z
        dc.w    semis-z

dotquote create {."} docolon
        dc.w    asciiquote-z
        dc.w    statefetch-z
        dc.w    zerobranch-z
        dc.w    m3-*
        dc.w    compile-z
        dc.w    pdotquoteq-z
        dc.w    word-z
        dc.w    here-z
        dc.w    cfetch-z
        dc.w    one-z
        dc.w    orx-z
        dc.w    oneplus-z
        dc.w    allot-z
        dc.w    branch-z
        dc.w    m4-*
m3              
        dc.w    word-z
        dc.w    here-z
        dc.w    count-z
        dc.w    type-z
m4              
        dc.w    semis-z
previmm setnum  $140-[previmm]

query   create  {query} docolon
        dc.w    tib-z
        dc.w    fetch-z
        dc.w    lit-z,81
        dc.w    expect-z
        dc.w    zeroinstore-z
        dc.w    semis-z

x       create  {x} docolon
        dc.w    blkfetch-z
        dc.w    zerobranch-z
        dc.w    m5-*
        dc.w    one-z
        dc.w    blk-z
        dc.w    plusstore-z
        dc.w    zeroinstore-z
        dc.w    blkfetch-z
        dc.w    bbyscr-z
        dc.w    onedash-z
        dc.w    andx-z
        dc.w    zeroeq-z
        dc.w    zerobranch-z
        dc.w    m6-*
        dc.w    questexec-z
        dc.w    rgreat-z
        dc.w    drop-z
m6              
        dc.w    branch-z
        dc.w    m7-*
m5              
        dc.w    rgreat-z
        dc.w    drop-z
m7              
        dc.w    semis-z
previmm setnum  $140-[previmm]

erase   create  {erase} docolon
        dc.w    zero-z
        dc.w    fill-z
        dc.w    semis-z

blanks  create  {blanks} docolon
        dc.w    bl-z
        dc.w    fill-z
        dc.w    semis-z

hold    create  {hold} docolon
        dc.w    dashone-z
        dc.w    hld-z
        dc.w    plusstore-z
        dc.w    hld-z
        dc.w    fetch-z
        dc.w    cstore-z
        dc.w    semis-z

pad     create  {pad} docolon
        dc.w    here-z
        dc.w    lumpplus-z
        dc.w    dashtwoand-z
        dc.w    semis-z

word    create  {word} docolon
        dc.w    blkfetch-z
        dc.w    zerobranch-z
        dc.w    m8-*
        dc.w    blkfetch-z
        dc.w    block-z
        dc.w    branch-z
        dc.w    m9-*
m8      dc.w    tib-z
        dc.w    fetch-z
m9      dc.w    in-z
        dc.w    fetch-z
        dc.w    plus-z
        dc.w    swapx-z
        dc.w    enclose-z
        dc.w    here-z
        dc.w    asciiquote-z
        dc.w    blanks-z
        dc.w    in-z
        dc.w    plusstore-z
        dc.w    overdash-z
        dc.w    greatrr-z
        dc.w    here-z
        dc.w    cstore-z
        dc.w    plus-z
        dc.w    here-z
        dc.w    oneplus-z
        dc.w    rgreat-z
        dc.w    cmove-z
        dc.w    semis-z

pnumberq create {(number)} docolon
m10     dc.w    oneplus-z
        dc.w    greatrr-z
        dc.w    cfetch-z
        dc.w    base-z
        dc.w    fetch-z
        dc.w    digit-z
        dc.w    zerobranch-z
        dc.w    m11-*
        dc.w    swapx-z
        dc.w    base-z
        dc.w    fetch-z
        dc.w    umul-z
        dc.w    drop-z
        dc.w    rot-z
        dc.w    base-z
        dc.w    fetch-z
        dc.w    umul-z
        dc.w    dplus-z
        dc.w    dpl-z
        dc.w    fetch-z
        dc.w    oneplus-z
        dc.w    zerobranch-z
        dc.w    m12-*
        dc.w    one-z
        dc.w    dpl-z
        dc.w    plusstore-z
m12     dc.w    rgreat-z
        dc.w    branch-z
        dc.w    m10-*
m11     dc.w    rgreat-z
        dc.w    semis-z

number  create  {number} docolon
        dc.w    zero-z
        dc.w    zero-z
        dc.w    rot-z
        dc.w    dup-z
        dc.w    oneplus-z
        dc.w    cfetch-z
        dc.w    lit-z,45
        dc.w    eq-z
        dc.w    greatrr-z
        dc.w    plus-z
        dc.w    dashone-z
m13     dc.w    dpl-z
        dc.w    store-z
        dc.w    pnumberq-z
        dc.w    dup-z
        dc.w    cfetch-z
        dc.w    bl-z
        dc.w    dash-z
        dc.w    zerobranch-z
        dc.w    m14-*
        dc.w    dup-z
        dc.w    cfetch-z
        dc.w    lit-z,46
        dc.w    dash-z
        dc.w    zero-z
        dc.w    questerror-z
        dc.w    zero-z
        dc.w    branch-z
        dc.w    m13-*
m14     dc.w    drop-z
        dc.w    rgreat-z
        dc.w    zerobranch-z
        dc.w    m15-*
        dc.w    dminus-z
m15     dc.w    semis-z

dashfind create {-find} docolon
        dc.w    bl-z
        dc.w    word-z
        dc.w    here-z
        dc.w    context-z
        dc.w    fetch-z
        dc.w    fetch-z
        dc.w    pfindq-z
        dc.w    dup-z
        dc.w    zeroeq-z
        dc.w    zerobranch-z
        dc.w    m16-*
        dc.w    drop-z
        dc.w    here-z
        dc.w    latest-z
        dc.w    pfindq-z
m16     dc.w    semis-z

pabortq create  {(abort)} docolon
        dc.w    abort-z
        dc.w    semis-z

error   create  {error} docolon
        dc.w    warning-z
        dc.w    fetch-z
        dc.w    zeroless-z
        dc.w    zerobranch-z
        dc.w    m17-*
        dc.w    pabortq-z
m17     dc.w    here-z
        dc.w    count-z
        dc.w    type-z
        dc.w    pdotquoteq-z
        dc.b    2,'?  '
        dc.w    message-z
        dc.w    spstore-z
        dc.w    blk-z
        dc.w    twofetch-z
        dc.w    quit-z
        dc.w    semis-z

iddot   create  {id.} docolon
        dc.w    pad-z
        dc.w    lit-z,32
        dc.w    lit-z,95
        dc.w    fill-z
        dc.w    dup-z
        dc.w    pfa-z
        dc.w    lfa-z
        dc.w    overdash-z
        dc.w    pad-z
        dc.w    swapx-z
        dc.w    twodup-z
        dc.w    plus-z
        dc.w    onedash-z
        dc.w    greatr-z
        dc.w    cmove-z
        dc.w    rgreat-z
        dc.w    msbtog-z
        dc.w    pad-z
        dc.w    count-z
        dc.w    onefand-z
        dc.w    type-z
        dc.w    space-z
        dc.w    semis-z

create  create  {create} docolon
        dc.w    dashfind-z
        dc.w    zerobranch-z
        dc.w    m18-*
        dc.w    drop-z
        dc.w    nfa-z
        dc.w    iddot-z
        dc.w    four-z
        dc.w    message-z
        dc.w    space-z
m18     dc.w    here-z
        dc.w    dup-z
        dc.w    count-z
        dc.w    width-z
        dc.w    fetch-z
        dc.w    min-z
        dc.w    oneplus-z
        dc.w    allot-z
        dc.w    over-z
        dc.w    msbtog-z
        dc.w    here-z
        dc.w    one-z
        dc.w    andx-z
        dc.w    zerobranch-z
        dc.w    m19-*
        dc.w    one-z
        dc.w    allot-z
        dc.w    twodup-z
        dc.w    here-z
        dc.w    overdash-z
        dc.w    lesscmove-z
        dc.w    swapx-z
m19     dc.w    drop-z
        dc.w    here-z
        dc.w    onedash-z
        dc.w    msbtog-z
        dc.w    latest-z
        dc.w    comma-z
        dc.w    current-z
        dc.w    fetch-z
        dc.w    store-z
        dc.w    here-z
        dc.w    twoplus-z
        dc.w    comma-z
        dc.w    smudge-z
        dc.w    semis-z

braccompilecarb create {`compile]} docolon;'
        dc.w    dashfind-z
        dc.w    zeroeq-z
        dc.w    zero-z
        dc.w    questerror-z
        dc.w    drop-z
        dc.w    cfa-z
        dc.w    comma-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

literal create  {literal} docolon
        dc.w    statefetch-z
        dc.w    zerobranch-z
        dc.w    m20-*
        dc.w    compile-z
        dc.w    lit-z
        dc.w    comma-z
m20             
        dc.w    semis-z
previmm setnum  $140-[previmm]

dliteral create {dliteral} docolon
        dc.w    statefetch-z
        dc.w    zerobranch-z
        dc.w    m21-*
        dc.w    compile-z
        dc.w    dlit-z
        dc.w    comma-z
        dc.w    comma-z
m21             
        dc.w    semis-z
previmm setnum  $140-[previmm]

ascii   create  {ascii} docolon
        dc.w    bl-z
        dc.w    word-z
        dc.w    here-z
        dc.w    oneplus-z
        dc.w    cfetch-z
        dc.w    literal-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

queststack create {?stack} docolon
        dc.w    depth-z
        dc.w    zeroless-z
        dc.w    one-z
        dc.w    questerror-z
        dc.w    spfetch-z
        dc.w    pad-z
        dc.w    lumpplus-z
        dc.w    less-z
        dc.w    lit-z,7
        dc.w    questerror-z
        dc.w    semis-z

interpret create {interpret} docolon
m22     dc.w    dashfind-z
        dc.w    zerobranch-z
        dc.w    m23-*
        dc.w    statefetch-z
        dc.w    less-z
        dc.w    zerobranch-z
        dc.w    m24-*
        dc.w    cfa-z
        dc.w    comma-z
        dc.w    branch-z
        dc.w    m25-*
m24     dc.w    cfa-z
        dc.w    execute-z
m25     dc.w    queststack-z
        dc.w    branch-z
        dc.w    m26-*
m23     dc.w    here-z
        dc.w    number-z
        dc.w    dpl-z
        dc.w    fetch-z
        dc.w    oneplus-z
        dc.w    zerobranch-z
        dc.w    m27-*
        dc.w    dliteral-z
        dc.w    branch-z
        dc.w    m28-*
m27     dc.w    drop-z
        dc.w    literal-z
m28     dc.w    queststack-z
m26     dc.w    branch-z
        dc.w    m22-*
      ;; dc.w    semis-z

immediate create {immediate} docolon
        dc.w    latest-z
        dc.w    immtog-z
        dc.w    semis-z

vocabulary create {vocabulary} docolon
        dc.w    lessbuilds-z
        dc.w    lit-z,$81a0
        dc.w    comma-z
        dc.w    current-z
        dc.w    fetch-z
        dc.w    cfa-z
        dc.w    comma-z
        dc.w    here-z
        dc.w    vocdashlink-z
        dc.w    fetch-z
        dc.w    comma-z
        dc.w    vocdashlink-z
        dc.w    store-z
        dc.w    doesgreat-z
        dc.w    twoplus-z
        dc.w    context-z
        dc.w    store-z
        dc.w    semis-z
dovoc   equ     *-8

forth   create  {forth} dodoesgreat
        dc.w    dovoc-z
        dc.w    $81a0            {-32352}
        dc.w    ntop-z
vl0     dc.w    0
previmm setnum  $140-[previmm]

definitions create {definitions} docolon
        dc.w    context-z
        dc.w    fetch-z
        dc.w    current-z
        dc.w    store-z
        dc.w    semis-z

px      create  {(} docolon
        dc.w    lit-z,41
        dc.w    word-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

quit    create  {quit} docolon
        dc.w    zero-z
        dc.w    blk-z
        dc.w    store-z
        dc.w    brac-z
        dc.w    cr-z
m29             
        dc.w    rpstore-z
        dc.w    statefetch-z
        dc.w    zeroeq-z
        dc.w    lit-z,60
        dc.w    plus-z
        dc.w    space-z
        dc.w    emit-z
        dc.w    query-z
        dc.w    interpret-z
        dc.w    branch-z
        dc.w    m29-*
        dc.w    semis-z

dotcpu  create  {.cpu} docolon
        dc.w    basecpu-z
        dc.w    ddot-z
        dc.w    base-z
        dc.w    store-z
        dc.w    semis-z

abort   create  {abort} docolon
        dc.w    spstore-z
        dc.w    drzero-z
        dc.w    decimal-z
        dc.w    cr-z
        dc.w    dotcpu-z
        dc.w    lit-z,9
        dc.w    count-z
        dc.w    type-z
        dc.w    forth-z
        dc.w    definitions-z
        dc.w    quit-z
        dc.w    semis-z

mul     create  {*} docolon
        dc.w    umul-z
        dc.w    drop-z
        dc.w    semis-z

bymod   create  {/mod} docolon
        dc.w    greatr-z
        dc.w    sdashgreatd-z
        dc.w    rgreat-z
        dc.w    mby-z
        dc.w    semis-z

by      create  {/} docolon
        dc.w    bymod-z
        dc.w    swapx-z
        dc.w    drop-z
        dc.w    semis-z

mod     create  {mod} docolon
        dc.w    bymod-z
        dc.w    drop-z
        dc.w    semis-z

mulbymod create {*/mod} docolon
        dc.w    greatr-z
        dc.w    mmul-z
        dc.w    rgreat-z
        dc.w    mby-z
        dc.w    semis-z

mulby   create  {*/} docolon
        dc.w    mulbymod-z
        dc.w    swapx-z
        dc.w    drop-z
        dc.w    semis-z

mbymod  create  {m/mod} docolon
        dc.w    greatr-z
        dc.w    zero-z
        dc.w    rx-z
        dc.w    uby-z
        dc.w    rgreat-z
        dc.w    swapx-z
        dc.w    greatr-z
        dc.w    uby-z
        dc.w    rgreat-z
        dc.w    semis-z

use     create  {use} dovariable
        dc.w    0

prev    create  {prev} dovariable
        dc.w    0

plusbuf create  {+buf} docolon
        dc.w    bbybuf-z
        dc.w    plus-z
        dc.w    four-z
        dc.w    plus-z
        dc.w    dup-z
        dc.w    limit-z
        dc.w    eq-z
        dc.w    zerobranch-z
        dc.w    m30-*
        dc.w    drop-z
        dc.w    first-z
m30             
        dc.w    dup-z
        dc.w    prev-z
        dc.w    fetch-z
        dc.w    dash-z
        dc.w    semis-z

update  create  {update} docolon
        dc.w    prev-z
        dc.w    fetch-z
        dc.w    fetch-z
        dc.w    lit-z,$8000      {-32768}
        dc.w    orx-z
        dc.w    prev-z
        dc.w    fetch-z
        dc.w    store-z
        dc.w    semis-z

emptydashbuffers create {empty-buffers} docolon
        dc.w    first-z
        dc.w    limit-z
        dc.w    overdash-z
        dc.w    erase-z
        dc.w    semis-z

drzero  create  {dr0} docolon
        dc.w    zero-z
        dc.w    offset-z
        dc.w    store-z
        dc.w    semis-z

drone   create  {dr1} docolon
        dc.w    lit-z,$4000      {16384}
        dc.w    offset-z
        dc.w    store-z
        dc.w    semis-z

rbyw    create  {r/w} Xrbyw

buffer  create  {buffer} docolon
        dc.w    use-z
        dc.w    fetch-z
        dc.w    greatrr-z
m31             
        dc.w    plusbuf-z
        dc.w    zerobranch-z
        dc.w    m31-*
        dc.w    use-z
        dc.w    store-z
        dc.w    rx-z
        dc.w    fetch-z
        dc.w    zeroless-z
        dc.w    zerobranch-z
        dc.w    m32-*
        dc.w    rx-z
        dc.w    twoplus-z
        dc.w    rx-z
        dc.w    fetch-z
        dc.w    lit-z,$7fff
        dc.w    andx-z
        dc.w    zero-z
        dc.w    rbyw-z
m32             
        dc.w    rx-z
        dc.w    store-z
        dc.w    rx-z
        dc.w    prev-z
        dc.w    store-z
        dc.w    rgreat-z
        dc.w    twoplus-z
        dc.w    semis-z

block   create  {block} docolon
        dc.w    offset-z
        dc.w    fetch-z
        dc.w    plus-z
        dc.w    greatr-z
        dc.w    prev-z
        dc.w    fetch-z
        dc.w    dup-z
        dc.w    fetch-z
        dc.w    rx-z
        dc.w    dash-z
        dc.w    dup-z
        dc.w    plus-z
        dc.w    zerobranch-z
        dc.w    m33-*
m34             
        dc.w    plusbuf-z
        dc.w    zeroeq-z
        dc.w    zerobranch-z
        dc.w    m35-*
        dc.w    drop-z
        dc.w    rx-z
        dc.w    buffer-z
        dc.w    dup-z
        dc.w    rx-z
        dc.w    one-z
        dc.w    rbyw-z
        dc.w    twodash-z
m35             
        dc.w    dup-z
        dc.w    fetch-z
        dc.w    rx-z
        dc.w    dash-z
        dc.w    dup-z
        dc.w    plus-z
        dc.w    zeroeq-z
        dc.w    zerobranch-z
        dc.w    m34-*
        dc.w    dup-z
        dc.w    prev-z
        dc.w    store-z
m33             
        dc.w    rgreat-z
        dc.w    drop-z
        dc.w    twoplus-z
        dc.w    semis-z

plineq  create  {(line)} docolon
        dc.w    greatr-z
        dc.w    cbyl-z
        dc.w    bbybuf-z
        dc.w    mulbymod-z
        dc.w    rgreat-z
        dc.w    bbyscr-z
        dc.w    mul-z
        dc.w    plus-z
        dc.w    block-z
        dc.w    plus-z
        dc.w    cbyl-z
        dc.w    semis-z

dotline create  {.line} docolon
        dc.w    plineq-z
        dc.w    dashtrailing-z
        dc.w    type-z
        dc.w    semis-z

message create  {message} docolon
        dc.w    warning-z
        dc.w    fetch-z
        dc.w    zerobranch-z
        dc.w    m36-*
        dc.w    dashdup-z
        dc.w    zerobranch-z
        dc.w    m37-*
        dc.w    four-z
        dc.w    offset-z
        dc.w    fetch-z
        dc.w    bbyscr-z
        dc.w    by-z
        dc.w    dash-z
        dc.w    dotline-z
m37     dc.w    branch-z
        dc.w    m38-*
m36     dc.w    pdotquoteq-z
        dc.b    6,'msg #  '
        dc.w    dot-z
m38     dc.w    semis-z

load    create  {load} docolon
        dc.w    blk-z
        dc.w    twofetch-z
        dc.w    greatr-z
        dc.w    greatr-z
        dc.w    zeroinstore-z
        dc.w    bbyscr-z
        dc.w    mul-z
        dc.w    blk-z
        dc.w    store-z
        dc.w    interpret-z
        dc.w    rgreat-z
        dc.w    rgreat-z
        dc.w    blk-z
        dc.w    twostore-z
        dc.w    semis-z

dashdashgreat create {-->} docolon
        dc.w    questloading-z
        dc.w    zeroinstore-z
        dc.w    bbyscr-z
        dc.w    blkfetch-z
        dc.w    over-z
        dc.w    mod-z
        dc.w    dash-z
        dc.w    blk-z
        dc.w    plusstore-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

tick    create  {'} docolon
        dc.w    dashfind-z
        dc.w    zeroeq-z
        dc.w    zero-z
        dc.w    questerror-z
        dc.w    drop-z
        dc.w    literal-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

forget  create  {forget} docolon
        dc.w    tick-z
        dc.w    nfa-z
        dc.w    dup-z
        dc.w    fence-z
        dc.w    fetch-z
        dc.w    uless-z
        dc.w    lit-z,21
        dc.w    questerror-z
        dc.w    greatr-z
        dc.w    vocdashlink-z
        dc.w    fetch-z
m39             
        dc.w    rx-z
        dc.w    over-z
        dc.w    uless-z
        dc.w    zerobranch-z
        dc.w    m40-*
        dc.w    forth-z
        dc.w    definitions-z
        dc.w    fetch-z
        dc.w    dup-z
        dc.w    vocdashlink-z
        dc.w    store-z
        dc.w    branch-z
        dc.w    m39-*
m40             
m41             
        dc.w    dup-z
        dc.w    fourdash-z
m42             
        dc.w    pfa-z
        dc.w    lfa-z
        dc.w    fetch-z
        dc.w    dup-z
        dc.w    rx-z
        dc.w    uless-z
        dc.w    zerobranch-z
        dc.w    m42-*
        dc.w    over-z
        dc.w    twodash-z
        dc.w    store-z
        dc.w    fetch-z
        dc.w    dashdup-z
        dc.w    zeroeq-z
        dc.w    zerobranch-z
        dc.w    m41-*
        dc.w    rgreat-z
        dc.w    dp-z
        dc.w    store-z
        dc.w    semis-z

back    create  {back} docolon
        dc.w    here-z
        dc.w    dash-z
        dc.w    comma-z
        dc.w    semis-z

begin   create  {begin} docolon
        dc.w    questcomp-z
        dc.w    here-z
        dc.w    one-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

endif   create  {endif} docolon
        dc.w    questcomp-z
        dc.w    two-z
        dc.w    questpairs-z
        dc.w    here-z
        dc.w    overdash-z
        dc.w    swapx-z
        dc.w    store-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

then    create  {then} docolon
        dc.w    endif-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

dox     create  {do} docolon
        dc.w    compile-z
        dc.w    pdoq-z
        dc.w    here-z
        dc.w    three-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

loop    create  {loop} docolon
        dc.w    three-z
        dc.w    questpairs-z
        dc.w    compile-z
        dc.w    ploopq-z
        dc.w    back-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

plusloop create {+loop} docolon
        dc.w    three-z
        dc.w    questpairs-z
        dc.w    compile-z
        dc.w    pplusloopq-z
        dc.w    back-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

until   create  {until} docolon
        dc.w    one-z
        dc.w    questpairs-z
        dc.w    compile-z
        dc.w    zerobranch-z
        dc.w    back-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

endx    create  {end} docolon
        dc.w    until-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

again   create  {again} docolon
        dc.w    one-z
        dc.w    questpairs-z
        dc.w    compile-z
        dc.w    branch-z
        dc.w    back-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

repeat  create  {repeat} docolon
        dc.w    twoswap-z
        dc.w    again-z
        dc.w    twodash-z
        dc.w    then-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

if      create  {if} docolon
        dc.w    compile-z
        dc.w    zerobranch-z
        dc.w    here-z
        dc.w    zero-z
        dc.w    comma-z
        dc.w    two-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

else    create  {else} docolon
        dc.w    two-z
        dc.w    questpairs-z
        dc.w    compile-z
        dc.w    branch-z
        dc.w    here-z
        dc.w    zero-z
        dc.w    comma-z
        dc.w    swapx-z
        dc.w    two-z
        dc.w    then-z
        dc.w    two-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

while   create  {while} docolon
        dc.w    if-z
        dc.w    twoplus-z
        dc.w    semis-z
previmm setnum  $140-[previmm]

spaces  create  {spaces} docolon
        dc.w    zero-z
        dc.w    max-z
        dc.w    dashdup-z
        dc.w    zerobranch-z
        dc.w    m43-*
        dc.w    zero-z
        dc.w    pdoq-z
m44             
        dc.w    space-z
        dc.w    ploopq-z
        dc.w    m44-*
m43             
        dc.w    semis-z

lesssharp create {<#} docolon
        dc.w    pad-z
        dc.w    hld-z
        dc.w    store-z
        dc.w    semis-z

sharpgreat create {#>} docolon
        dc.w    twodrop-z
        dc.w    hld-z
        dc.w    fetch-z
        dc.w    pad-z
        dc.w    overdash-z
        dc.w    semis-z

sign    create  {sign} docolon
        dc.w    rot-z
        dc.w    zeroless-z
        dc.w    zerobranch-z
        dc.w    m45-*
        dc.w    lit-z,45
        dc.w    hold-z
m45             
        dc.w    semis-z

sharp   create  {#} docolon
        dc.w    base-z
        dc.w    fetch-z
        dc.w    mbymod-z
        dc.w    rot-z
        dc.w    lit-z,9
        dc.w    over-z
        dc.w    less-z
        dc.w    zerobranch-z
        dc.w    m46-*
        dc.w    lit-z,7
        dc.w    plus-z
m46             
        dc.w    lit-z,48
        dc.w    plus-z
        dc.w    hold-z
        dc.w    semis-z

sharps  create  {#s} docolon
m47             
        dc.w    sharp-z
        dc.w    twodup-z
        dc.w    orx-z
        dc.w    zeroeq-z
        dc.w    zerobranch-z
        dc.w    m47-*
        dc.w    semis-z

ddotr   create  {d.r} docolon
        dc.w    greatr-z
        dc.w    swapx-z
        dc.w    over-z
        dc.w    dabs-z
        dc.w    lesssharp-z
        dc.w    sharps-z
        dc.w    sign-z
        dc.w    sharpgreat-z
        dc.w    rgreat-z
        dc.w    overdash-z
        dc.w    spaces-z
        dc.w    type-z
        dc.w    semis-z

ddot    create  {d.} docolon
        dc.w    zero-z
        dc.w    ddotr-z
        dc.w    space-z
        dc.w    semis-z

dotr    create  {.r} docolon
        dc.w    greatr-z
        dc.w    sdashgreatd-z
        dc.w    rgreat-z
        dc.w    ddotr-z
        dc.w    semis-z

dot     create  {.} docolon
        dc.w    sdashgreatd-z
        dc.w    ddot-z
        dc.w    semis-z

quest   create  {?} docolon
        dc.w    fetch-z
        dc.w    dot-z
        dc.w    semis-z

listx   create  {list} docolon
        dc.w    decimal-z
        dc.w    cr-z
        dc.w    dup-z
        dc.w    scr-z
        dc.w    store-z
        dc.w    pdotquoteq-z
        dc.b    6,'scr #  '
        dc.w    dot-z
        dc.w    lit-z,16
        dc.w    zero-z
        dc.w    pdoq-z
m48             
        dc.w    cr-z
        dc.w    ix-z
        dc.w    three-z
        dc.w    dotr-z
        dc.w    space-z
        dc.w    ix-z
        dc.w    scr-z
        dc.w    fetch-z
        dc.w    dotline-z
        dc.w    ploopq-z
        dc.w    m48-*
        dc.w    cr-z
        dc.w    semis-z

index   create  {index} docolon
        dc.w    oneplus-z
        dc.w    swapx-z
        dc.w    pdoq-z
m49             
        dc.w    cr-z
        dc.w    ix-z
        dc.w    three-z
        dc.w    dotr-z
        dc.w    space-z
        dc.w    zero-z
        dc.w    ix-z
        dc.w    dotline-z
        dc.w    questterminal-z
        dc.w    zerobranch-z
        dc.w    m50-*
        dc.w    leave-z
m50             
        dc.w    ploopq-z
        dc.w    m49-*
        dc.w    semis-z

triad   create  {triad} docolon
        dc.w    three-z
        dc.w    by-z
        dc.w    three-z
        dc.w    mul-z
        dc.w    three-z
        dc.w    over-z
        dc.w    plus-z
        dc.w    swapx-z
        dc.w    pdoq-z
m51             
        dc.w    cr-z
        dc.w    ix-z
        dc.w    listx-z
        dc.w    ploopq-z
        dc.w    m51-*
        dc.w    cr-z
        dc.w    lit-z,15
        dc.w    message-z
        dc.w    cr-z
        dc.w    semis-z

vlist   create  {vlist} docolon
        dc.w    context-z
        dc.w    fetch-z
        dc.w    fetch-z
        dc.w    one-z
m52             
        dc.w    zerobranch-z
        dc.w    m53-*
        dc.w    cr-z
        dc.w    zero-z
        dc.w    out-z
        dc.w    store-z
m53             
        dc.w    dup-z
        dc.w    iddot-z
        dc.w    pfa-z
        dc.w    lfa-z
        dc.w    fetch-z
        dc.w    dup-z
        dc.w    oneplus-z
        dc.w    cfetch-z
        dc.w    lit-z,160
        dc.w    eq-z
        dc.w    questterminal-z
        dc.w    orx-z
        dc.w    onedash-z
        dc.w    andx-z
        dc.w    dashdup-z
        dc.w    zerobranch-z
        dc.w    m54-*
        dc.w    dup-z
        dc.w    cfetch-z
        dc.w    onefand-z
        dc.w    out-z
        dc.w    fetch-z
        dc.w    plus-z
        dc.w    lit-z,72
        dc.w    great-z
        dc.w    branch-z
        dc.w    m52-*
m54             
        dc.w    cr-z
        dc.w    semis-z

twovariable create {2variable} docolon
        dc.w    variable-z
        dc.w    comma-z
        dc.w    semis-z


wquote  create  {w"} docolon
        dc.w    asciiquote-z
        dc.w    zero-z
        dc.w    statefetch-z
        dc.w    zerobranch-z
        dc.w    m55-*
        dc.w    compile-z
        dc.w    pwquoteq-z
        dc.w    ccomma-z
        dc.w    word-z
        dc.w    here-z
        dc.w    cfetch-z
        dc.w    oneplus-z
        dc.w    one-z
        dc.w    orx-z
        dc.w    allot-z
        dc.w    branch-z
        dc.w    m56-*
m55             
        dc.w    pad-z
        dc.w    cstore-z
        dc.w    word-z
        dc.w    here-z
        dc.w    pad-z
        dc.w    oneplus-z
        dc.w    here-z
        dc.w    cfetch-z
        dc.w    oneplus-z
        dc.w    lesscmove-z
        dc.w    pad-z
m56             
        dc.w    semis-z
previmm setnum  $140-[previmm]

savequote create {save"} docolon        (save-system)
        dc.w    temp-z
        dc.w    cfetch-z
        dc.w    dup-z
        dc.w    wquote-z
        dc.w    three-z
        dc.w    rot-z
        dc.w    open-z
        dc.w    ert-z
        dc.w    lit-z
        dc.w    savehead-z
        dc.w    here-z
        dc.w    over-z
        dc.w    twoplus-z
        dc.w    store-z
        dc.w    zero-z
        dc.w    ioblo-z
        dc.w    fsheads-z
        dc.w    zero-z
        dc.w    here-z
        dc.w    ioblo-z
        dc.w    fssave-z
        dc.w    close-z
        dc.w    semis-z

chainquote create {chain"} docolon  (load from textfiles)
        dc.w    temp-z
        dc.w    count-z
        dc.w    swapx-z
        dc.w    over-z
        dc.w    wquote-z
        dc.w    one-z
        dc.w    rot-z
        dc.w    open-z
        dc.w    ert-z
        dc.w    cstore-z
        dc.w    semis-z

***        endit
z[prevnum] equ  [previmm]
ntop    equ     [prevlink]
top     ds.w    0

        end


