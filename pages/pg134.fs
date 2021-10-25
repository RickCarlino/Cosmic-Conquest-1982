      DROP DROP
   ENDCASE
   DRAW-DISPLAY ;

: OTHER-FLEET   ( make other fleet curent fleet)
   FLEET-FLAG @ 1 =
   IF  2 FLEET-FLAG !
   ELSE 1 FLEET-FLAG !
   ENDIF
   DRAW-DISPLAY ;

: MOVE-LEFT
   1 F C@ 2 F C@ 1 - CHECK-POSITION ;

: MOVE-RIGHT
   1 F C@ 2 F C@ 1+ CHECK-POSITION ;

: MOVE-DOWN
   1 F C@ 1+ 2 F C@ CHECK-POSITION ;

: MOVE-UP
   1 F C@ 1 - 2 F C@ CHECK-POSITION ;

: ENLIST    ( enlisting 1egions on a planet)
   BUY-V @ 0=
   IF  ( it's ok to buy)
       5 BUY-V !  ( can't buy for 5 more moves)
       ( calculate cost of legions)
       RANDOM1 8 / XY@ INFO1 C@ 7 / + DUP TEMP1 !
       10 0 VHTAB ." COST PER LEGION =" 3 .R
       ( calculate no. of legions available)
       XY@ INFO1 C@ 6 / DUP LEG !
       12 0 VHTAB ." NO OF LEGIONS AVAILABLE = " 3 .R
       ( take the order)
       14 0 VHTAB ." HOW MANY DO YOU REQUIRE?" INPUT
       LEG @ MIN DUP TEMP1 @ * CREDIT @ >
       IF  ( not enough money)
         16 0 VHTAB ." NOT ENOUGH CREDIT"
       ELSE
         5 F @ OVER + 5 F !  ( update legions)
         TEMP1 @ * CREDIT @ SWAP - CREDIT ! ( update credit)
       ENDIF
   ELSE
      10 0 VHTAB ." NO TROOPS AVAILABLE"
   ENDIF ;

: BUY    ( purchasing of ships at planet)
   BUY-V @ 0=
   IF    ( it's ok to buy)
      5 BUY-V !               ( stop continous buying)
      RANDOM1 5 / XY@ INFO1 C@ 10 / + 1+ DUP TEMP1 !
      10 0 VHTAB ." COST PER SHIP = " 2 .R
      12 0 VHTAB ." HOW MANY DO YOU WANT?" INPUT
      CREDIT @ TEMP1 @ / MIN    ( no more than he can afford)
      DUP 3 F @ + 3 F !       ( update ships in fleet)
      TEMP1 @ * CREDIT @ SWAP - CREDIT !  ( update credit)
      16 1 F C@ 2 F C@ GALAXY C!  ( make sure fleet symbol there)
   ELSE
      10 0 VHTAB ." NO SHIPS AVAILABLE"
   ENDIF ;

: GATHER   ( pick up legions from garrison onto fleet)
   10 0 VHTAB ." HOW MANY DO YOU WISH TO TAKE?" INPUT
   XY@ INFO2 C@ MIN TEMP1 !  ( no more than are there)
   5 F @ TEMP1 @ + 5 F !     ( update legions on fleet)
   XY@ INFO2 C@ TEMP1 @ - XY@ INFO2 C! ; ( update on planet)

: LEAVE   ( leave legions from fleet on planet as garrison)
   10 0 VHTAB ." HOW MANY DO YOU WISH TO LEAVE?" INPUT
   5 F @ MIN TEMP1 !         ( no more than you have)
   5 F @ TEMP1 @ - 5 F !     ( update legions on fleet)
   XY@ INFO2 C@ TEMP1 @ + 255 MIN ( no more than 255)
   XY@ INFO2 C! ;            ( update on planet)

: FRIENDLY-PLANET   ( options upon landing at colony)
   BEGIN
      10 0 VHTAB ." CLASS " XY@ INFO1 C@ 8 / 2 .R
      ."  PLANET" 16 SPACES CR  ( give class of planet)
      ." LOCAL GARRISON IS " XY@ INFO2 C@ 3 .R ."  LEGIONS"
                                ( give size of local garrison)
      12 0 VHTAB ." DO YOU WISH TO:" 12 SPACES ( give options)
      CR ." 1.  LEAVE LEGIONS ON PLANET"
      CR ." 2.  GATHER LEGIONS FROM PLANET"
      CR ." 3.  BUY SHIPS"
      CR ." 4.  ENLIST TROOPS"
      CR ." 5.  LEAVE" CR
      KEY 127 AND              ( get reply)
      CLEAR-MSGE
      CASE
         49 ( 1) OF LEAVE 0   ( leave legions) ENDOF
         50 ( 2) OF GATHER 0 ( gather legions) ENDOF
         51 ( 3) OF BUY 0         ( buy ships) ENDOF
         52 ( 4) OF ENLIST 0    ( en1ist troops) ENDOF
                           1   ( the default: Ieave planet)
      ENDCASE DELAY
   UNTIL
   H1 CLEAR-MSGE DRAW-DISPLAY ;

: COLONISE ( attack an uncolonised planet)
   CLEAR-MSGE
   XY@ INFO1 C@ 8 / RANDOM1 1 - 5 / 7 + * 10 / DUP TEMP1 !
   ( calaculate relative strength of planet)
   5 F @ >
   IF   ( planet drives off your forces)
      10 0 VHTAB ." YOUR FORCES RETREAT"
      12 0 VHTAB ." YOUR LOSSES = "
      5 F @ 2 / DUP 3 .R 5 F @ SWAP - 5 F !
      DELAY DELAY
   ELSE ( you capture planet)
      10 0 VHTAB ." PLANET CAPTURED"
      12 0 VHTAB ." YOUR LOSSES = "
      TEMP1 @ 3 .R
      5 F @ TEMP1 @ - 5 F !   ( update legions in fleet)
      1 PLANETS +!            ( increment no. of planets)
      132 XY@ GALAXY C!       ( colony symbol in galaxy)
      DELAY DELAY
      FRIENDLY-PLANET
   ENDIF ;

: EMPTY-PLANET   ( in orbit round uncolonised planet)
   CLEAR-MSGE
   10 0 VHTAB ." UNCOLONISED CLASS " XY@ INFO1 C@ 8 / 2 .R
   ." PLANET"
   12 0 VHTAB ." DO YOU WISH TO ATTACK?" KEY 127 AND 89 =
   IF
      COLONISE
   ENDIF
   H1 CLEAR-MSGE ;

: NOT-PLANET   ( there isn't a planet where he's trying to land)
   10 0 VHTAB ." NO PLANET THERE'
   DELAY H1 CLEAR-MSGE ;

: ATTACK       ( attack a planet controlled by the computer)
   CLEAR-MSGE
   XY@ INFO2 C@ RANDOM1 1 - 5 / 7 + * 10 / DUP TEMP1 !
               ( calaculate enemy garrlsons effective strength)
   5 F @ >
   IF   ( enemy garrison wins)
      10 0 VHTAB ." YOUR FORCES RETREAT"
      12 0 VHTAB ." YOUR LOSSES = "
      XY@ INFO2 C@ 5 F @ * TEMP1 @ / 2 / XY@ INFO2 C@ SWAP
      - XY@ INFO2 C!
      5 F @ 2 / DUP 3 .R 5 F @ SWAP - 5 F !
   ELSE
      0 XY@ INFO2 C!           ( reduce legions on planet to 0)
      10 0 VHTAB ." PLANET CAPTURED"
      12 0 VHTAB ." YOUR LOSSES = "
      TEMP1 @ 3 .R
      5 F @ TEMP1 @ - 5 F !    ( update legions with fleet)
      132 XY@ GALAXY C!        ( put colony in galaxy)
      1 PLANETS +!             ( increment planets)
      -1 C-PLANETS +!          ( decrement computer planets)
      XY@ INFO1 C@ 8 / MINUS CLASS-TOTALS +!
      DELAY                   ( reduce classes of compo plnts)
      FRIENDLY-PLANET
   ENDIF
   DELAY H1 CLEAR-MSGE ;

: ENEMY-PLANET   ( player orbits enemy planet)
   XY@ INFO1 C@ 8 /
   10 0 VHTAB ." CLASS " 2 .R ."  PLANET" CR CR
   ." ENEMY GARRISON OF STRENGTH "
   XY@ INFO2 C@ 3 .R CR CR
   ." DO YOU WISH TO ATTACK?" KEY 127 AND 89 =
   IF
      ATTACK
   ENDIF
   H1 CLEAR-MSGE ;

: LAND   ( land on adjacent planet)
   FIND-DIRECTION
   2DUP Y ! X ! TEXT GALAXY C@
   CASE
      4 ( uncolonised planet) OF EMPTY-PLANET    ENDOF
      5 ( computers planet)   OF ENEMY-PLANET    ENDOF
    132 ( players colony)     OF FRIENDLY-PLANET ENDOF
        NOT-PLANET  ( otherwise it's not a planet)
   ENDCASE ;

: REVOLT? ( planet at X,Y revolts)
   12 0 VHTAB ." PLANET AT " Y @ . X @ . ." REVOLTS" DELAY
   XY@ INFO1 C@ 8 / XY@ INFO2 C@ 2DUP >
   IF   ( revolt succeeds)
      DROP 4 XY@ GALAXY C!            ( place planet symbol)
      8 * 7 + XY@ INFO1 C!            ( set revolt factor 7)
      0 XY@ INFO2 C!                  ( set lpgions to 0)
      -1 PLANETS +!                   ( reduce no.of planets )
      7 EMIT                          ( ring bell)
      14 0 VHTAB ." SUCCEEDS"
   ELSE ( revolt fails)
      SWAP 2 / - XY@ INFO2 C!         ( reduce legions)
      XY@ INFO1 C@ 7 OR XY@ INFO1 C!  ( set revolt factor 7)
      14 0 VHTAB ." FAILS"
   ENDIF
   DELAY
   12 0 VHTAB 30 SPACES
   14 0 VHTAB 12 SPACES ;             ( clear messages)
