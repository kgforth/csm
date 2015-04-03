: S' [CHAR] ' PARSE [COMPILE] SLITERAL ; IMMEDIATE


USER /lim USER /cur  USER /s


CREATE (A) 0xE2 C, 0x92 C, 0xB6 C,
CREATE (B) 0xE2 C, 0x92 C, 0xB7 C,
CREATE (U) 0xE2 C, 0x93 C, 0x8A C,
CREATE (W) 0xE2 C, 0x93 C, 0x8C C,
CREATE (R) 0xE2 C, 0x93 C, 0x87 C,
CREATE (I) 0xE2 C, 0x92 C, 0xBE C,
CREATE (E) 0xE2 C, 0x92 C, 0xBA C,
CREATE (T) 0xE2 C, 0x93 C, 0x89 C,
CREATE (D) 0xE2 C, 0x92 C, 0xB9 C,
CREATE (L) 0xE2 C, 0x93 C, 0x81 C,
CREATE (P) 0xE2 C, 0x93 C, 0x85 C,
CREATE (0) 0xE2 C, 0x93 C, 0xAA C,
CREATE (v) 0xE2 C, 0x93 C, 0xA5 C,
CREATE (u) 0xE2 C, 0x93 C, 0xA4 C,


 0  CONSTANT Black
 1  CONSTANT Red
 2  CONSTANT Yellow
 3  CONSTANT Green
 4  CONSTANT Aqua
 5  CONSTANT Blue
 6  CONSTANT Fuchsia


: "Red     S' <font color="#ff0000"' ;
: "Yellow  S' <font color="#fff000"' ;   ( orig #ffff00 )
: "Green   S' <font color="#00ff00"' ;
: "Aqua    S' <font color="#00ffff"' ;
: "Blue    S' <font color="#0000ff"' ;
: "Fuchsia S' <font color="#ff00ff"' ;


: Red"     S' color="#ff0000">' ;
: Yellow"  S' color="#fff000">' ;   ( orig #ffff00 )
: Green"   S' color="#00ff00">' ;
: Aqua"    S' color="#00ffff">' ;
: Blue"    S' color="#0000ff">' ;
: Fuchsia" S' color="#ff00ff">' ;


: Red-tag?  ( --  f ) 
         S' <font style=' /cur @ OVER COMPARE IF -1 EXIT THEN 
         /cur @ 256  S' color="#ff0000">' SEARCH IF DROP 15 + /cur ! 0  ELSE 2DROP -1 THEN  ; 

: Yellow-tag?  ( --  f ) 
         S' <font style=' /cur @ OVER COMPARE IF -1 EXIT THEN 
         /cur @ 256  S' color="#fff000">' SEARCH IF DROP 15 + /cur ! 0  ELSE 2DROP -1 THEN  ; 

: Green-tag?  ( --  f ) 
         S' <font style=' /cur @ OVER COMPARE IF -1 EXIT THEN 
         /cur @ 256  S' color="#00ff00">' SEARCH IF DROP 15 + /cur ! 0  ELSE 2DROP -1 THEN  ; 

: Aqua-tag?  ( --  f ) 
         S' <font style=' /cur @ OVER COMPARE IF -1 EXIT THEN 
         /cur @ 256  S' color="#00ffff">' SEARCH IF DROP 15 + /cur ! 0  ELSE 2DROP -1 THEN  ; 


: Blue-tag?  ( --  f ) 
         S' <font style=' /cur @ OVER COMPARE IF -1 EXIT THEN 
         /cur @ 256  S' color="#0000ff">' SEARCH IF DROP 15 + /cur ! 0  ELSE 2DROP -1 THEN  ; 

: Fuchsia-tag?  ( --  f ) 
         S' <font style=' /cur @ OVER COMPARE IF -1 EXIT THEN 
         /cur @ 256  S' color="#ff00ff">' SEARCH IF DROP 15 + /cur ! 0  ELSE 2DROP -1 THEN  ; 



: "span    S' <span style=' ;
: "<u      S' <u style=' ;
: "<b      S' <b style=' ;

: tag?  ( a u --  f ) 
         /cur @ OVER COMPARE IF -1 EXIT THEN 
         /cur @ 256  S' ">' SEARCH IF DROP 1+ /cur ! 0  ELSE 2DROP -1 THEN  
; 


VARIABLE %C   0  %C !  ( cur color )
VARIABLE %B   0  %B !  ( cur bold  )
VARIABLE %U   0  %U !  ( cur under )
VARIABLE %S   0  %S !  ( cur subs  )


CREATE v_tmp  BL C,
CREATE d_tmp  BL C,


: s+ /s @ STR+ ;

: b+ S"  " s+  ;

: s. /s @ STR@ TYPE ;

: br+ S" <br>" s+ ;

: ??  ( a u --  f ) SWAP OVER /cur @ OVER COMPARE 0= IF 1- /cur +! 0 THEN ; 


: a-z?  ( -- f ) /cur @ C@ [CHAR] a [CHAR] z 1+ WITHIN 0= ;
: A-Z?  ( -- f ) /cur @ C@ [CHAR] A [CHAR] Z 1+ WITHIN 0= ;


: test(a-z) 

  %C @ Fuchsia = %B @ 1 = AND IF /cur @ C@ v_tmp C!  S"  :NONAME " s+  EXIT THEN
  %C @ Fuchsia = %U @ 1 = AND IF S"  TO vector_" s+ /cur @ 1 s+ b+     EXIT THEN
  %C @ Fuchsia =              IF S"  vector_" s+ /cur @ 1 s+ b+        EXIT THEN

  %C @ Blue = %U @ 1 = %B @ 1 = AND AND IF S"  DUP data_" s+ /cur @ 1 s+ S"  ! " s+  EXIT THEN
  %C @ Blue = %U @ 1 = AND IF S"  data_" s+ /cur @ 1 s+ S"  ! " s+   EXIT THEN
  %C @ Blue =              IF S"  data_" s+ /cur @ 1 s+ S"  @ " s+   EXIT THEN
                         
 /cur @ 1 s+

;


: 1-9?  ( -- f ) /cur @ C@ [CHAR] 1 [CHAR] 9 1+ WITHIN 0= ;

: 1-9-load [CHAR] 1  /cur @ C@ DO S"  data_" s+ I d_tmp C! d_tmp 1 s+ S"  ! " s+ -1 +LOOP  ;

: 1-9-copy [CHAR] 1  /cur @ C@ DO /cur @ C@ I - [CHAR] 0 + d_tmp C! b+ d_tmp 1 s+ S"  PICK data_" s+ I d_tmp C! d_tmp 1 s+ S"  ! " s+ -1 +LOOP  ;


: test(1-9) 


  %C @ Blue = %U @ 1 = %B @ 1 = AND AND IF 1-9-copy  EXIT THEN
  %C @ Blue = %U @ 1 = AND              IF 1-9-load  EXIT THEN
  %C @ Blue =    IF S"  data_" s+ /cur @ 1 s+ S"  @ " s+   EXIT THEN
                         
 /cur @ 1 s+

;


: test(IF) 

  %C @ Green = %U @ 1 = %B @ 1 = AND AND IF S"  0 > NOT IF " s+  EXIT THEN
  %C @ Green = %U @ 1 = AND              IF S"  > NOT IF " s+  EXIT THEN
  %C @ Green = %B @ 1 = AND              IF S"  0 > IF " s+  EXIT THEN
  %C @ Green =                           IF S"  > IF " s+  EXIT THEN

  %C @ Yellow = %U @ 1 = %B @ 1 = AND AND IF S"  0 = NOT IF " s+  EXIT THEN
  %C @ Yellow = %U @ 1 = AND              IF S"  = NOT IF " s+  EXIT THEN
  %C @ Yellow = %B @ 1 = AND              IF S"  0 = IF " s+  EXIT THEN
  %C @ Yellow =                           IF S"  = IF " s+  EXIT THEN

  %C @ Red = %U @ 1 = %B @ 1 = AND AND IF S"  0 < NOT IF " s+  EXIT THEN
  %C @ Red = %U @ 1 = AND              IF S"  < NOT IF " s+  EXIT THEN
  %C @ Red = %B @ 1 = AND              IF S"  0 < IF " s+  EXIT THEN
  %C @ Red =                           IF S"  < IF " s+  EXIT THEN                       

 S"  IF "  s+
;

: test(UNTIL) 

  %C @ Green = %U @ 1 = %B @ 1 = AND AND IF S"  0 > NOT UNTIL " s+  EXIT THEN
  %C @ Green = %U @ 1 = AND              IF S"  > NOT UNTIL " s+  EXIT THEN
  %C @ Green = %B @ 1 = AND              IF S"  0 > UNTIL " s+  EXIT THEN
  %C @ Green =                           IF S"  > UNTIL " s+  EXIT THEN

  %C @ Yellow = %U @ 1 = %B @ 1 = AND AND IF S"  0 = NOT UNTIL " s+  EXIT THEN
  %C @ Yellow = %U @ 1 = AND              IF S"  = NOT UNTIL " s+  EXIT THEN
  %C @ Yellow = %B @ 1 = AND              IF S"  0 = UNTIL " s+  EXIT THEN
  %C @ Yellow =                           IF S"  = UNTIL " s+  EXIT THEN

  %C @ Red = %U @ 1 = %B @ 1 = AND AND IF S"  0 < NOT UNTIL " s+  EXIT THEN
  %C @ Red = %U @ 1 = AND              IF S"  < NOT UNTIL " s+  EXIT THEN
  %C @ Red = %B @ 1 = AND              IF S"  0 < UNTIL " s+  EXIT THEN
  %C @ Red =                           IF S"  < UNTIL " s+  EXIT THEN                       

 S"  UNTIL "  s+
;

: test(WHILE) 

  %C @ Green = %U @ 1 = %B @ 1 = AND AND IF S"  0 > NOT WHILE " s+  EXIT THEN
  %C @ Green = %U @ 1 = AND              IF S"  > NOT WHILE " s+  EXIT THEN
  %C @ Green = %B @ 1 = AND              IF S"  0 > WHILE " s+  EXIT THEN
  %C @ Green =                           IF S"  > WHILE " s+  EXIT THEN

  %C @ Yellow = %U @ 1 = %B @ 1 = AND AND IF S"  0 = NOT WHILE " s+  EXIT THEN
  %C @ Yellow = %U @ 1 = AND              IF S"  = NOT WHILE " s+  EXIT THEN
  %C @ Yellow = %B @ 1 = AND              IF S"  0 = WHILE " s+  EXIT THEN
  %C @ Yellow =                           IF S"  = WHILE " s+  EXIT THEN

  %C @ Red = %U @ 1 = %B @ 1 = AND AND IF S"  0 < NOT WHILE " s+  EXIT THEN
  %C @ Red = %U @ 1 = AND              IF S"  < NOT WHILE " s+  EXIT THEN
  %C @ Red = %B @ 1 = AND              IF S"  0 < WHILE " s+  EXIT THEN
  %C @ Red =                           IF S"  < WHILE " s+  EXIT THEN                       

 S"  WHILE "  s+
;


: to-forth


0 CASE                                       

  "Red     tag?  OF Red     %C ! ENDOF 
  "Yellow  tag?  OF Yellow  %C ! ENDOF 
  "Green   tag?  OF Green   %C ! ENDOF 
  "Aqua    tag?  OF Aqua    %C ! ENDOF 
  "Blue    tag?  OF Blue    %C ! ENDOF 
  "Fuchsia tag?  OF Fuchsia %C ! ENDOF 

  Red-tag?       OF Red     %C ! ENDOF 
  Yellow-tag?    OF Yellow  %C ! ENDOF 
  Green-tag?     OF Green   %C ! ENDOF 
  Aqua-tag?      OF Aqua    %C ! ENDOF 
  Blue-tag?      OF Blue    %C ! ENDOF 
  Fuchsia-tag?   OF Fuchsia %C ! ENDOF 


S" </font>"  ??  OF Black   %C ! ENDOF

  S" <b>"    ??  OF 1 %B !       ENDOF
  S" </b>"   ??  OF 0 %B !       ENDOF
  "<b      tag?  OF 1 %B !       ENDOF 

  S" <u>"    ??  OF 1 %U !       ENDOF 
  S" </u>"   ??  OF 0 %U !       ENDOF 
  "<u      tag?  OF 1 %U !       ENDOF 

  S" <sub>"  ??  OF 1 %S ! S"  ( "   s+ ENDOF 
  S" </sub>" ??  OF 0 %S ! S"  ) "   s+ ENDOF 


  S" &lt;"   ??  OF %C @ Blue = IF S"  < " ELSE  S" <"   THEN  s+  ENDOF
  S" &gt;"   ??  OF %C @ Blue = IF S"  > " ELSE  S" >"   THEN  s+  ENDOF

  S" &nbsp;"  ??  OF S"  "   s+   ENDOF

  S" <div>"   ??  OF LT 2 s+ br+  ENDOF
  S" </div>"  ??  OF S"  "   s+   ENDOF

  S" <br>"    ??  OF S"  <br> " s+ LT 2 s+ ENDOF

  S" <span>"  ??  OF S"  "   s+   ENDOF
  S" </span>" ??  OF S"  "   s+   ENDOF

  "span     tag?  OF S"  "   s+   ENDOF 


  (A) 3      ??  OF S"  AGAIN "   s+ ENDOF     
  (B) 3      ??  OF S"  BEGIN "   s+ ENDOF     
  (U) 3      ??  OF test(UNTIL)      ENDOF
  (W) 3      ??  OF test(WHILE)      ENDOF
  (R) 3      ??  OF S"  REPEAT "  s+ ENDOF
  (I) 3      ??  OF test(IF)         ENDOF
  (E) 3      ??  OF S"  ELSE "    s+ ENDOF
  (T) 3      ??  OF S"  THEN "    s+ ENDOF
  (D) 3      ??  OF S"  DO "      s+ ENDOF
  (L) 3      ??  OF S"  LOOP "    s+ ENDOF
  (P) 3      ??  OF S"  +LOOP "   s+ ENDOF
  (0) 3      ??  OF S"  ?DO "     s+ ENDOF
  (v) 3      ??  OF S"  LEAVE "   s+ ENDOF
  (u) 3      ??  OF S"  UNLOOP "  s+ ENDOF

  a-z?  OF  test(a-z)  ENDOF
  A-Z?  OF  test(a-z)  ENDOF
  1-9?  OF  test(1-9)  ENDOF


   S"  ; "     ??  OF   S"  ; " s+  ENDOF
   S" ;"       ??  OF  %C @ Fuchsia = IF S"  ; TO vector_" s+ v_tmp 1 s+ b+ ELSE %C @ Blue = IF S"  EXIT " ELSE  S" ;" THEN s+ THEN ENDOF
   S" &amp;"   ??  OF  %C @ Blue = IF S"  AND "      ELSE  S" &"     THEN  s+  ENDOF
   S" &"       ??  OF  %C @ Blue = IF S"  AND "      ELSE  S" &"     THEN  s+  ENDOF
   S" ~"       ??  OF  %C @ Blue = IF S"  INVERT "   ELSE  S" ~"     THEN  s+  ENDOF 
   S" !"       ??  OF  %C @ Blue = IF S"  ! "        ELSE  S" !"   THEN  s+  ENDOF
   S" @"       ??  OF  %C @ Blue = IF S"  @ "        ELSE  S" @"   THEN  s+  ENDOF
   S" #"       ??  OF  %C @ Blue = IF S"  >NUMBER "  ELSE  S" #"   THEN  s+  ENDOF
   S" $"       ??  OF  %C @ Blue = IF S"  SFIND "    ELSE  S" $"   THEN  s+  ENDOF
   S" %"       ??  OF  %C @ Blue = IF S"  MOD "      ELSE  S" %"   THEN  s+  ENDOF
   S" ^"       ??  OF  %C @ Blue = IF S"  XOR "      ELSE  S" ^"   THEN  s+  ENDOF
   S" *"       ??  OF  %C @ Blue = IF S"  * "        ELSE  S" *"   THEN  s+  ENDOF
   S" ("       ??  OF  %C @ Blue = IF S"  ( "        ELSE  S" ("   THEN  s+  ENDOF
   S" )"       ??  OF  %C @ Blue = IF S"  ) "        ELSE  S" )"   THEN  s+  ENDOF
   S" -"       ??  OF  %C @ Blue = IF S"  - "        ELSE  S" -"   THEN  s+  ENDOF
   S" ,"       ??  OF  %C @ Blue = IF S"  , "        ELSE  S" ,"   THEN  s+  ENDOF
   S" +"       ??  OF  %C @ Blue = IF S"  + "        ELSE  S" +"   THEN  s+  ENDOF
   S" ="       ??  OF  %C @ Blue = IF S"  = "        ELSE  S" ="   THEN  s+  ENDOF
   S" {"       ??  OF  %C @ Blue = IF S"  { "        ELSE  S" {"   THEN  s+  ENDOF
   S" }"       ??  OF  %C @ Blue = IF S"  } "        ELSE  S" }"   THEN  s+  ENDOF
   S" :"       ??  OF  %C @ Blue = IF S"  DUP "      ELSE  %C @ Red = IF S"  : " ELSE  S" :" THEN THEN  s+  ENDOF
   S" \"       ??  OF  %C @ Blue = IF S"  CR "       ELSE  S" \"   THEN  s+  ENDOF
   S" |"       ??  OF  %C @ Blue = IF S"  OR "       ELSE  S" |"   THEN  s+  ENDOF
   S" /"       ??  OF  %C @ Blue = IF S"  / "        ELSE  S" /"   THEN  s+  ENDOF
   S" <"       ??  OF  %C @ Blue = IF S"  < "        ELSE  S" <"   THEN  s+  ENDOF
   S" >"       ??  OF  %C @ Blue = IF S"  > "        ELSE  S" >"   THEN  s+  ENDOF
   S" ."       ??  OF  %C @ Blue = IF S"  . "        ELSE  S" ."   THEN  s+  ENDOF
   S" ?"       ??  OF  %C @ Blue = IF S"  ? "        ELSE  S" ?"   THEN  s+  ENDOF
   S" _"       ??  OF  %C @ Blue = IF S"  _ "        ELSE  S" _"   THEN  s+  ENDOF


  %C @ Black = IF  /cur @ 1 s+  ELSE b+ /cur @ 1 s+ b+ THEN


 ENDCASE  


;



: (FORTHCODE)   ( a u -- s )


OVER /cur ! + /lim !  "" /s !


  BEGIN  /cur @ /lim @ < WHILE to-forth  /cur 1+!  REPEAT

CR  ." {" s. ." }"

/s @ 


;


' (FORTHCODE) TO FORTHCODE







: to-bbcode


0 CASE                                       

  "Red     tag?  OF S" [color=#FF0000]"  s+ ENDOF 
  "Yellow  tag?  OF S" [color=#FFF000]"  s+ ENDOF 
  "Green   tag?  OF S" [color=#00FF00]"  s+ ENDOF 
  "Aqua    tag?  OF S" [color=#00FFFF]"  s+ ENDOF 
  "Blue    tag?  OF S" [color=#0000FF]"  s+ ENDOF 
  "Fuchsia tag?  OF S" [color=#FF00FF]"  s+ ENDOF 

  Red-tag?       OF S" [color=#FF0000]"  s+ ENDOF 
  Yellow-tag?    OF S" [color=#FFF000]"  s+ ENDOF 
  Green-tag?     OF S" [color=#00FF00]"  s+ ENDOF 
  Aqua-tag?      OF S" [color=#00FFFF]"  s+ ENDOF 
  Blue-tag?      OF S" [color=#0000FF]"  s+ ENDOF 
  Fuchsia-tag?   OF S" [color=#FF00FF]"  s+ ENDOF 



S" </font>"  ??  OF S" [/color]"  s+  ENDOF

  S" <b>"    ??  OF S" [b]"   s+   ENDOF
  S" </b>"   ??  OF S" [/b]"  s+   ENDOF
  "<b      tag?  OF S" [b]"   s+   ENDOF 

  S" <u>"    ??  OF S" [u]"   s+   ENDOF 
  S" </u>"   ??  OF S" [/u]"  s+   ENDOF 
  "<u      tag?  OF S" [u]"   s+   ENDOF 

  S" <sub>"  ??  OF S" [sub]"   s+  ENDOF 
  S" </sub>" ??  OF S" [/sub]"  s+  ENDOF 


  S" &lt;"   ??  OF  S" <"      s+  ENDOF
  S" &gt;"   ??  OF  S" >"      s+  ENDOF

  S" &nbsp;"  ??  OF S"  "   s+   ENDOF

  S" <div>"   ??  OF LT 2 s+ br+  ENDOF
  S" </div>"  ??  OF S" "   s+   ENDOF

  S" <br>"    ??  OF S"  <br> " s+ LT 2 s+ ENDOF

  S" <span>"  ??  OF S" "   s+   ENDOF
  S" </span>" ??  OF S" "   s+   ENDOF

  "span     tag?  OF S" "   s+   ENDOF 


  /cur @ 1 s+ 


 ENDCASE  


;









: (BBCODE)   ( a u -- s )


OVER /cur ! + /lim !  "" /s !


  BEGIN  /cur @ /lim @ < WHILE to-bbcode  /cur 1+!  REPEAT


CR  ." {" s. ." }"

/s @ 


;



' (BBCODE) TO BBCODE


