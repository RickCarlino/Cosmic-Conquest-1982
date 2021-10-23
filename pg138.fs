\ page 138 of from: https://archive.org/details/byte-magazine-1982-12/page/n139/mode/1up?q=cosmic+conquest
\ modified to conform to original from OCR from: https://archive.org/stream/byte-magazine-1982-12/1982_12_BYTE_07-12_Game_Plan_1982_djvu.txt

      0 XY@ GALAXY C!          ( destroy fleet symbol)
         -1 C-FLEETS +!        ( reduce comps fleets)
      ELSE 
         XY@ INFO2 C@ OVER - TROOPS @ SWAP - TROOPS ! 
         ( reduce spare troops) 
         XY@ INFO2 C!          ( reduce legions in fleet) 
      ENDIF 
      - 0 MAX DUP 0= 
      IF  ( players fleet destroyed) 
         DROP NEW-FLEET 
      ELSE 
         3 F ! 
      ENDIF 
   ENDIF 
   DELAY DELAY DRAW-DISPLAY H1 CLEAR-MSGE ; 

: INFORMATION  ( display the text screen information) 
   TEXT KEY H1 ; 

HEX

: OBEY-COMMAND 
   BUY-V @ -DUP 
   IF 
      1 - BUY-V ! 
   ENDIF 
   C001 C@                ( pick up keyboard character) 
   CASE 
      ( A) 41 OF MOVE-LEFT   ENDOF 
      ( S) 53 OF MOVE-RIGHT  ENDOF 
      ( W) 57 OF MOVE-UP     ENDOF 
      ( Z) 5A OF MOVE-DOWN   ENDOF 
      ( O) 4F OF OTHER-FLEET ENDOF 
      ( I) 49 OF INFORMATION ENDOF 
      ( L) 4C OF LAND        ENDOF 
      ( T) 54 OF TAX         ENDOF 
      ( F) 46 OF FIRE        ENDOF 
   ENDCASE SP! ;

: COMPUTER?    ( is it the computers turn or not) 
   COMPUTER @ 1 - DUP 0= 
   IF 
      COMP-START @ COMPUTER ! DROP 1 
   ELSE 
      COMPUTER ! 0
   ENDIF ; 

: GAME-END? 
   LEN @ 0= ;    ( game end if LEN is zero) 

: RESTART        ( restarts the stopped game) 
   CLEAR-DISP 
   HOME DRAW-BORDERS DRAW-DISPLAY 
   BEGIN 
      ?TERMINAL 
      IF    ( player has pressed a key) 
         OBEY-COMMAND 
         -1 LEN +! 
         COMPUTER-TURN 
      ENDIF 
      COMPUTER? 
      IF 
         COMUTER-TURN 
      ENDIF 
      GAME-END? 
   UNTIL 
   END-MSGE ; 

: CONQUEST  ( the main game word) 
   HOME ." HIT ANY KEY" KEY RAND1 ! CR ( random number seed) 
        ." AND AGAIN  " KEY RAND2 !    ( random number seed) 
   HOME CR CR CR 
   ." WELCOME TO COSMIC CONQUEST" CR CR 
   ." DEVISED AND WRITTEN BY" CR CR 
   ." ALAN SARTORI-ANGUS" 
   INITIALISE 
   RESTART ;

