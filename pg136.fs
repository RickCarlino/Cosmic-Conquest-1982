\ page 136 of from: https://archive.org/details/byte-magazine-1982-12/page/n139/mode/1up?q=cosmic+conquest
\ modified to conform to original from OCR from: https://archive.org/stream/byte-magazine-1982-12/1982_12_BYTE_07-12_Game_Plan_1982_djvu.txt

: TAX     ( collect taxes on players planets) 
   0 VTAX !                           ( set tax to 0) 
   TEXT                               ( select text page) 
   10 0 VHTAB ." TAX COLLECTED =" 
   10 17 VHTAB 0 . 
   SIZE 1+ 1 DO 
     SIZE 1+ 1 DO 
                 I J GALAXY C@ 132 = 
                 IF   ( it's a colony) 
                    I J INFO1 C@ 3 * 5 / ( tax from planet) 
                    VTAX @ + DUP VTAX !  ( update tax) 
                    10 17 VHTAB 5 .R 
                    I J INFO1 C@ 7 AND -DUP 
                    IF ( doesn't revolt) 
                       I J INFO1 DUP C@ 1 - SWAP C! 
                    ELSE ( revolt) 
                       I X ! J Y ! REVOLT? 
                    ENDIF 
                 ENDIF 
               LOOP 
             LOOP 
   CREDIT @ VTAX @ + CREDIT !            ( update credit) 
   H1 CLEAR-MSGE DRAW-DISPLAY ;

: COMPUTER-TURN   ( computers turn to do something) 
   -1 NEW +!                            ( decrement NEW)
   NEW @ 0= 
   IF    ( computer creates new fleet) 
      7 EMIT                            ( ring bell)
      1 C-FLEETS +!                     ( update comp. fleets) 
      29 4 DIFF @ * - NEW !             ( reset NEW) 
      CLASS-TOTALS @ 8 / DUP C-LEGIONS +! 
      DUP TROOPS +! 
      BEGIN 
         RANDOM1 RANDOM2 2DUP GALAXY C@ 0= 
         IF  ( empty space in galaxy) 
            2DUP 17 ROT ROT GALAXY C!   ( place fleet symbol)
            INFO2 C! 1                  ( plus legions)
         ELSE 
            DROP DROP DROP 0
         ENDIF 
      UNTIL 
   ENDIF 
   DIFF @ 0 DO   ( see if computer colonises planet) 
      RANDOM1 RANDOM2 2DUP GALAXY C@ 
      CASE 
         4 OF ( empty planet) 
              2DUP 2DUP 5 ROT ROT GALAXY C! ( place colony) 
              C-LEGIONS @ 2 / DUP C-LEGIONS ! 
              ROT ROT INFO2 C! 
              1 C-PLANETS +! 
              INFO1 C@ 8 / CLASS-TOTALS +! ENDOF 
       132 OF ( players planet) 
              2DUP Y ! X ! INFO2 C@ C-LEGIONS @ 2 / < 
              IF ( captures planet) 
                 C-LEGIONS @ 3 / C-LEGIONS ! 
                 5 XY@ GALAXY C! 
                 XY@ INFO1 C@ 8 / CLASS-TOTALS +! 
                 1 C-PLANETS +! 
                 -1 PLANETS +! 
                 5 0 DO 7 EMIT LOOP ENDIF ENDOF 
           DROP DROP 
      ENDCASE 
   LOOP 
   DRAW-FIGURES ;

: FIRE     ( players fleet attacks computer fleet) 
   0 X ! 
   TEXT 
   2 F C@ 2 + DUP 3 - DO 
      1 F C@ 2 + DUP 3 - DO 
         I EDGE-CHECK J EDGE-CHECK GALAXY C@ 17 = 
         IF  ( there's a fleet in range) 
            I EDGE-CHECK X ! J EDGE-CHECK Y ! 
         ENDIF 
      LOOP 
   LOOP 
   X @ 0= 
   IF 
      10 0 VHTAB ." NO ENEMY FLEET IN RANGE" 
   ELSE 
      3 F @ XY@ INFO2 C@ OVER 4 * 10 / 
      OVER 4 * 10 / DUP 
      10 0 VHTAB ." FLEET HIT BY " 5 .R ." UNITS" 
      ROT ROT - 0 MAX DUP 0= 
      IF ( computers fleet destroyed) 
      DROP TROOPS @ XY@ INFO2 C@ - TROOPS ! 
      ( reduce computers troops)