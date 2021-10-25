: TEXT                      ( selects text screen )
   0 -16303 C! ;

: END-MSGE                  ( end of game message)
   TEXT 12 0 VHTAB ." END OF GAME COMMANDER" ;

( graphics shapes and utilities)

0 VARIABLE SPACEFIG 80 ALLOT  ( shape tables)

: C$                        ( loads 8-bit value into table)
   OVER C! 1+ ;

: $                         ( loads 16-bit value into table)
   OVER ! 2 + ;

SPACEFIG                    ( load shape tables)
   12 $ 31 $ 41 $ 47 $ 63 $ 74 $ HEX
( space shape)
   24 C$ 3F C$ 37 C$ 36 C$ 2E C$ 24 C$ 2C C$ 36 C$ 2E C$ 2C C$
   2E C$ 25 C$ 24 C$ 3C C$ 37 C$ 2E C$ 34 C$ 36 C$ 00 C$
( colony shape)
   12 C$ 2D C$ 24 C$ 24 C$ 3F C$ 3F C$ 36 C$ 36 C$ 2D C$ 00 C$
( planet shape )
   2C C$ 36 C$ 3F C$ 24 C$ 05 C$ 00 C$
( star shape)
   3C C$ 36 C$ 2D C$ 24 C$ 1C C$ 1F C$ 16 C$ 16 C$ 0D C$ 0D C$
   1C C$ 2C C$ 1C C$ 0C C$ 04 C$ 00 C$
( players fleet shape)
   3C C$ 1B C$ 36 C$ 26 C$ 08 C$ 2D C$ 2D C$ 25 C$ 36 C$ 36 C$
   00 C$
( computers fleet shape)
   36 C$ 07 C$ 20 C$ 29 C$ 32 C$ 00 C$

DECIMAL DROP FORGET C$  ( we don't need C$ and $ any more)

: SKETCH  ( n ---  )    ( sketch shape n at current position)
   2 * 0 SWAP SPACEFIG + @ SPACEFIG + DRAW ;

( into the main game words)

: SET-UP-GALAXY
   NO-OF-STARS 0 DO 2 RANDOM1 RANDOM2 GALAXY C!
                 LOOP   ( set up stars in galaxy)
   NO-OF-PLANETS 0 DO RANDOM1 RANDOM2 2DUP 4 ROT ROT
                      GALAXY C!  ( set up planets)
                      RANDOM1 4 * 8 + ROT ROT INFO1 C!
                      ( set up class of planet)
                   LOOP
   NO-OF-B-HOLES 0 DO 8 RANDOM1 RANDOM2 GALAXY C!
                      ( set up black holes)
                   LOOP ;

: INITIALISE         ( initialise all variables and arrays)
   CR CR
   BEGIN
      ." WHAT LEVEL OF DIFFICULTY (1-4) " INPUT DUP
      5 < IF  ( correct response) 1
          ELSE ( incorrect response) DROP CR 0
          ENDIF
   UNTIL
   DIFF !      ( store difficulty)
   HOME CR CR
   ." DO YOU WANT" CR ." 1. SHORT" CR ." 2. MEDIUM" CR
   ." 3. LONG" CR ." GAME"
   KEY 127 AND     ( pick up reply)
   CASE
      49 ( 1) OF 350 LEN ! ( 350 moves) ENDOF
      50 ( 2) OF 700 LEN ! ( 700 moves) ENDOF
                1500 LEN ! ( 1500 moves otherwise)
   ENDCASE
   HOME ." INITIALISING"
   CLEAR-GALAXY CLEAR-DISP CLEAR-INFO SET-UP-GALAXY
   1 FLEET-FLAG !          ( make -fleet 1 current fleet)
   250 CREDIT !            ( players credit)
   0 PLANETS !             ( no planets)
   0 C-PLANETS !           ( none for computer either)
   20 1 3 FLEETS ! 20 2 3 FLEETS ! ( fleets start with 20 ships)
   50 1 5 FLEETS ! 50 2 5 FLEETS ! ( fleets have 50 legions each)
   DIFF @ 4 * 0 DO ( position computers fleets)
                   RANDOM1 RANDOM2 2DUP 17 ROT ROT GALAXY C!
                   15 ROT ROT INFO2 C!
              LOOP
   16 22 18 GALAXY C! 16 18 22 GALAXY C! ( position fleets)
   22 1 1 FLEETS C! 18 1 2 FLEETS C!
   18 2 1 FLEETS C! 22 2 2 FLEETS C!
   29 3 DIFF @ * - NEW !  ( how often computer creates fleets)
   15 DIFF @ 4 * * TROOPS ! ( initial no. of computer troops)
   20 DIFF @ * C-LEGIONS ! ( no. of spare computer legions)
   DIFF @ 4 * C-FLEETS !   ( no. of computer fleets)
   SPEED DUP COMPUTER !
   COMP-START !   ( how often computer moves)
   1 BUY-V ! ;

: DRAW-BORDERS   ( draw borders o-f display and headings)
   CLEAR-SCREEN
   7 HCOLOUR     ( colour white)
   17 5 HPOSN
      238 5 HLINE 238 126 HLINE 17 126 HLINE 17 5 HLINE
   57 27 HPOSN
      198 27 HLINE 198 104 HLINE 57 104 HLINE 57 27 HLINE
   HOME
   ." PLAYER"
   2  0  VHTAB ." PLANETS ="
   4  0  VHTAB ." EMPIRE"
   6  0  VHTAB ." FLEETS"
   6  21 VHTAB ." PLANETS"
   20 0  VHTAB ." X="
   20 7  VHTAB ." Y="
   21 0  VHTAB ." NO. OF SHIPS ="
   22 0  VHTAB ." LEGIONS ="
   20 25 VHTAB ." SCORE ="
   22 21 VHTAB ." CREDITS" ;

: FIND-DIRECTION     (  --- X Y )
                     ( find out which square player means)
   23 0 VHTAB ." WHICH DIRECTION?"
   2 SPACES KEY 127 AND
   CASE
      87 ( up)    OF -1  0 ENDOF
      90 ( down)  OF  1  0 ENDOF
      83 ( right) OF  0  1 ENDOF
      65 ( left)  OF  0 -1 ENDOF
                      0  0
   ENDCASE
   23 0 VHTAB 35 SPACES   ( clear message )
   2 F C@ + EDGE-CHECK SWAP
   1 F C@ + EDGE-CHECK SWAP ;

: PRINT-IT  ( c  --- )
            ( shape determined by c is printed on screen at)
            ( position in X,Y)
   DUP X @ 1+ Y @ 1+ SCREEN C@ =
   IF  ( display is already showing this shape so don't bother)
      DROP
   ELSE
      DUP X @ 1+ Y @ 1+ SCREEN C! ( remember what screen has)
      0 HCOLOUR                   ( colour black)
      X @ 20 * 27 + Y @ 1+ 11 * HPOSN
      0 SKETCH                    ( blank out char. there)
      7 HCOLOUR                   ( colour white)
      X @ 20 * 27 + Y @ 1+ 11 * HPOSN
      CASE                        ( draw shape)
         2 ( a star)         OF 3 SKETCH ( draw star)     ENDOF
         4 ( empty planet)   OF 2 SKETCH ( a planet)      ENDOF
         5 ( enemy planet)   OF 2 SKETCH ( a planet)      ENDOF
       132 ( players planet) OF 1 SKETCH ( a colony)      ENDOF
        16 ( players fleet)  OF 4 SKETCH ( players fleet) ENDOF
        17 ( enemy fleet)    OF 5 SKETCH ( enemy fleet)   ENDOF
      ENDCASE
   ENDIF ;

: DRAW-SCAN                       ( draw the screen display)
   1 F C@ 5 - 2 F C@ 5 -
   11 0 DO
      11 0 DO
              OVER EDGE-CHECK OVER EDGE-CHECK
              J Y ! I X ! GALAXY C@
              PRINT-IT 1+
           LOOP
           11 - SWAP 1+ SWAP
        LOOP
   DROP DROP ;

: DRAW-FIGURES    ( draw the totals in the disp1ay)
   2 10 VHTAB PLANETS @ 5 .R
   20 33 VHTAB PLANETS @ C-PLANETS @ - W1 *
               1 3 FLEETS @ 2 3 FLEETS @ + W2 * +
               1 5 FLEETS @ 2 5 FLEETS @ + W2 * +
               TROOPS @ W3 * - 6 .R
   6 8   VHTAB C-FLEETS @ 5 .R
   6 29  VHTAB C-PLANETS @ 5 .R
   20 2  VHTAB 2 F C@ 2 .R
   20 9  VHTAB 1 F C@ 2 .R
   21 15 VHTAB 3 F @ 4 .R
   22 10 VHTAB 5 F @ 6 .R
   22 31 VHTAB CREDIT @ 6 .R ;

: DRAW-DISPLAY
   1 SCALE H1 DRAW-SCAN DRAW-FIGURES ;

: NEW-FLEET  ( fleet destroyed for some reason)
   0 1 F C@ 2 F C@ GALAXY C!   ( remove fleet symbol)
   0 3 F !                     ( no ships left)
   0 5 F ! ;                   ( no legions left)

: MOVE-FLEET (  X Y ---  )
   2DUP
   0 1 F C@ 2 F C@ GALAXY C!   ( remove old symbol)
   16 ROT ROT GALAXY C!       ( position fleet)
   2 F C! 1 F C! ;             ( update fleet array)

: CHECK-POSITION  ( X Y --- )
                  ( check if move to position X Y is possib
                  ( and take apropriate action)
   EDGE-CHECK SWAP EDGE-CHECK SWAP 2DUP GALAXY C@
   CASE
      0 ( space)      OF MOVE-FLEET ENDOF
      8 ( black hole) OF 23 0 VHTAB ." FLEET IN BLACK HOLE"
                         MOVE-FLEET DELAY NEW-FLEET
                         23 0 VHTAB 35 SPACES ENDOF
