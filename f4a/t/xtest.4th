#! /usr/src/asm/f4a/b/fig4th
( " /usr/src/asm/t/f4t.scr" using 7 load bye ( via screen-file ) 
( cat test.4th|fig4th                        ( redir. input    ) 
( else out-comment above line ) 
stdout cons? if-true ' cr is (ok)
\ ' cr is (ok)
4096 allot -4096 allot 
lload order
lload help
lload words
lload u.r
lload ud.r
forth definitions decimal
2nd-voc forth linux
2nd-voc linux hidden 2 (ti) !
( here (gb) ! 4 tdb )
( 0 cdt 64 dump cr 1 cdt 64 dump cr 2 cdt 64 dump cr ) 
( 0 cons? .  1 cons? . 2 cons? .  io-cons . vt? . cr )
( 0 cdt 64 dump cr 1 cdt 64 dump cr 2 cdt 64 dump cr )
order cr 
words cr
0 cdt constant stdin
2 cdt constant kbd
0 variable kk
: k kk @ 1 kk +! cr . . (ti) @ dup 0 0< 0= 
  if ."  : "(ti) @ . ." s pause or, "endif ." any key to continue "
  stdin @ >r kbd @ stdin ! 0 w4f ?key minus 1+ (ti) +! drop r> stdin ! cr ; 
 0 cdt 64 dump cr 1 cdt 64 dump cr 2 cdt 64 dump cr 
0 k 
: .cr . cr ; 
0 variable toto 
: tt toto @ dup . 1+ toto ! ; 
" tt: " print tt tt tt tt tt cr .s see tt cr .s 2drop forget tt 
1 k
." argc:" .s -144 (up) + ? (argp) 4- ? cr
2 k

 : dumpargc (argp) 4- @ . cr ; 
see dumpargc 2drop cr dumpargc  
              : dumpargv (argp) @ begin @+ -dup while zcount type space repeat cr drop ; 
see dumpargv 2drop cr dumpargv cr 
                                  
 : dumpargs dumpargc dumpargv ; 
see dumpargs 2drop  dumpargs cr 
                               
tib @ 200 dump
3 k
: p3  2 pick 18  .r over -9  .r dup -9  .r ;
: u3  2 pick 18 u.r over -9 u.r dup -9 u.r ;
: sm/rem m/ ;

4 k .( => unsigned double by sing, m/mod: ) cr
 30  0   7  u3 m/mod -14 ud.r -14 u.r 
-30  0   7  u3 m/mod -14 ud.r -14 u.r 
-30  0   7  u3 m/mod -14 ud.r -14 u.r 
 30  0   7  u3 m/mod -14 ud.r -14 u.r 
 30  0   0  u3 m/mod -14 ud.r -14 u.r 
-30  0   0  u3 m/mod -14 ud.r -14 u.r 
  0  0   0  u3 m/mod -14 ud.r -14 u.r 

." => signed double by sing, m/: " cr
 30  0   7  p3  sm/rem -14 .r -14 .r 
-30 -1   7  p3  sm/rem -14 .r -14 .r 
-30 -1  -7  p3  sm/rem -14 .r -14 .r 
 30  0  -7  p3  sm/rem -14 .r -14 .r 
 30  0   0  p3  sm/rem -14 .r -14 .r 
-30 -1   0  p3  sm/rem -14 .r -14 .r 
  0  0   0  p3  sm/rem -14 .r -14 .r 

5 k .( => /mod: ) cr
 30      7  p3  /mod   -14 .r -14 .r 
-30      7  p3  /mod   -14 .r -14 .r 
-30     -7  p3  /mod   -14 .r -14 .r 
 30     -7  p3  /mod   -14 .r -14 .r 
 30      0  p3  /mod   -14 .r -14 .r 
-30      0  p3  /mod   -14 .r -14 .r 
  0      0  p3  /mod   -14 .r -14 .r 
 
6 k
cr 
found? help 0= if-true " make doc install" sh drop 
help bbi
cr order
bel \ 0 bbi
bye
end
