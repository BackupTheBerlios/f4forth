#! /usr/local/bin/f4g ( ------- )
( #! /usr/src/asm/f4a/b/fig4th ( ------- )
( copy to a binaries' directory which is in the PATH variable )
( vim: set tabstop=8 shiftwidth=2 expandtab: )

( prevent console corruption - brutal-masche, spielt immerhin... )
( 'stty sane' w. suppressed cmd chars echo )
1 ch-id -1 1 cdt ! rs 1 cdt !

( sdrop = swap drop )
( sover = swap over )
( : dbg" ascii " word here count type cr ; immediate ( ------- )
( lload f4 ( ------- )

0  variable bytes-read
12 constant FF

0  variable sc 
0  variable lc
0  variable filename
 
(argp) (up) - 4- ucons argc
(argp) (up) -    ucons argp

( ret open chanel no; {bye} if none available or open failed )
: open-file     ( name -- ch )
  new-chan -dup 0= if
    drop cr ." No free channel found!" cr bye
  endif
  swap dup 1- rot
  r/o swap open dup 0< if
    swap cr ." Attempt to open '"
    zcount type ." ' failed - error code " . ." !" cr
    bye
  endif
  swap drop
;

: next-line     ( ch -- a bytes-read | -ve )
  here c/l rot -1 fread dup 0< 0= -exit sdrop ;

: u.l 		( n fieldwidth -- )
  swap 0 <# #s #> sover type - dup 
  0< if drop exit else spaces endif ;

: nextlc        ( -- u )	( ret current & increment lines count )
  lc @ dup 1+ b/scr mod lc ! ;

: fill-scr	( -- )
  begin
    nextlc
  -dup while
    3 .r ." :~~~" cr
  repeat ;

: print-screenfile ( ch -- ch )
  0. sc ! lc !
  begin
    dup next-line dup 0< if					( ch -- ch a u )
      drop 2drop cr ." reading failed - skipping ..." cr ;s
    endif
  -dup while							( ch -- ch a +u)
    lc @ 0 = if
      sc @ 0 swap < sc @ 3 mod 0= and if FF emit endif
      cr cr ." SCR #" sc @ 5 u.l 1 sc +! ."  ("
      filename @ zcount type ." )" cr cr
    endif
    nextlc >rr 3 .r ." |" type ." |" sc @ 1- b/scr * r> + 5 .r cr ( ch -- ch )
  repeat
  drop						( drop input buffer address )
  fill-scr					( missing lines of last scr )
;

: -dl 0= -exit 		( print delimiting line before the next screen file )
  cr ." ----------------------------------------------------------" cr ;
: argv ( ix -- a ) 4* argp + ;

: display-file 
  argv @ dup filename ! open-file   ( open next screen-file and store it's name)
  dup print-screenfile close drop ; ( print screen-file's contents, close file.)

( print the first file unconditionally )
( see if there are remaining files )
: main  ( -- )
  argc -dup 0= if				( handle stdin, piped input &c )
    " <stdin>" 1+ filename ! 0 print-screenfile
  else						( handle argument as a file... )
    0 do i dup -dl display-file loop
  endif
;

main
-1 (vtr)  !			( prevent false console reset at eof/exit )
