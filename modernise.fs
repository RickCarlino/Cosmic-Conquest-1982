( TARGET SPECIFIC WORDS)
needs modernise.fs

\ SEE: https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
\ : DRAW CR ~~ ." TODO: DRAW" BYE ;
: H1 ; ( H1 selects HIRES mode 1, which we don't have)
\ : HCLR CR ~~ ." TODO: HCLR" BYE ; ( HCLR clears the current HIRES-mode screen, which we don't have)
\ : HCOLOUR CR ~~ ." TODO: HCOLOUR" BYE ; ( colour --- ) ( select current colour for HIRES drawing mode)
\ : HLINE CR ~~ ." TODO: HPOSN" BYE ; ( x y --- ) ( draw HIRES mode line to position)
\ : HPOSN CR ~~ ." TODO: HPOSN" BYE ; ( x y --- ) ( moves HIRES mode pixel cursor)
\ : SCALE CR ~~ ." TODO: HPOSN" BYE ; ( n --- ) ( no idea about this one)

: hclr ( --- ) ( clear screen using ANSI codes)
   .\" \e[J" ; ( .\" is hard to Google for but means "print, using C-style escape sequences) 

: hcolour drop ( simply discard values) ;
: hline drop drop ( simply discard values) ;
: hposn drop drop ( simply discard values) ;
: scale drop ;

: home ( --- ) ( set cursor to home position, using ANSI codes)
   .\" \e[H" ;

: vhtab ( y x --- ) ( position cursor on screen, using ANSI codes)
   swap .\" \e[" 
   0 <# #s #> type
   ." ;"
   0 <# #s #> type ." H" ;

: draw ( addr delim --- ) ( draw shape table, presumably first value is address and second is delimiter)
   drop drop ; ( discard values from stack)

( POSSIBLE FIG WORDS NOT SUPPORTED IN ANS FORTH)
\ HELPFUL RESOURCE: https://dwheeler.com/6502/fig-forth-glossary.txt
\ Helpful book: Forth Fundamentals Vol. 2, C. Kevin McCabe, dilithium Press 1983, ISBN 0-88056-092-4
: MINUS negate ( FFv2 page 97) ;
: -DUP ?dup ( FFv2 page 39) ;
\ : ?TERMINAL ." TODO: ?TERMINAL" BYE ;
\ ?TERMINAL detects if there's a "break" keypress, leaving true on the stack if there is or false if there is not

: text ( --- ) ( do nothing) ;