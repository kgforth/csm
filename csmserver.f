VECT FORTHCODE
VECT BBCODE


REQUIRE {            locals.f
REQUIRE CreateSocket sockets.f
REQUIRE STR@         str5.f
REQUIRE CASE         case.f
REQUIRE (MAKECODE)   makecode.f


4000  CONSTANT /QUERY_BUFF
10000 CONSTANT /FILE_BUFF

CREATE CRLFCRLF 13 C, 10 C, 13 C, 10 C,
VARIABLE $root-dir
VARIABLE port

USER http-socket
USER filename
USER uri
USER posted
USER uri-args
USER protocol
USER data
USER active
USER command?
USER method

: ON  ( addr ) TRUE SWAP ! ;
: OFF ( addr ) 0! ;
: ROOT-DIR $root-dir @ STR@ ;

: ReplaceInStr ( a u c1 c2 --  ) 2SWAP OVER + SWAP ?DO OVER I C@ = IF DUP I C! THEN LOOP 2DROP ;

: REPLACE+ ( addr u -- addr2 u2 ) 2DUP [CHAR] + BL ReplaceInStr ;


: HTTP-DECODE { addr u \ count -- addr2 u2 }
    u 0 DO
          I addr + DUP C@ [CHAR] % =
          IF
            1+ DUP C@ 16 DIGIT DROP
            SWAP 1+ C@ 16 DIGIT DROP
            SWAP 4 LSHIFT OR 3
          ELSE C@ 1
          THEN SWAP addr count + C!
          AT count 1+!
        +LOOP addr count
;

: GET-REST-LINE ( addr -- )
  13 PARSE 10 SKIP
  ROT S!
;

VOCABULARY HEADER-VALUES

: GET-URI
    NextWord HTTP-DECODE uri S!
    NextWord protocol S!
    command? OFF
    ONLY HEADER-VALUES
;

: VALUE:
\ Использовать только в модуле!
   >IN @ USER-CREATE >IN !
   DOES>A @ >R
   EXPORT USER
   R> DOES>A !
   DEFINITIONS
   DOES> @ TlsIndex@ + DUP @ ?DUP IF STRFREE THEN
         GET-REST-LINE
;

MODULE: HTTP

: get  GET-URI  data ON  active OFF ;
: post GET-URI  data ON  active ON  ;
: head GET-URI  data OFF active OFF ;

;MODULE

MODULE: HEADER-VALUES

WARNING @ WARNING 0! SWAP

\ Хидер от rfc2068

VALUE: cache-control:
VALUE: connection:
VALUE: date:
VALUE: pragma:
VALUE: transfer-encoding:
VALUE: upgrade:
VALUE: via:

VALUE: accept:
VALUE: accept-charset:  
VALUE: accept-encoding:  
VALUE: accept-language:  
VALUE: authorization:
VALUE: from:
VALUE: host:
VALUE: if-modified-since:
VALUE: if-match:
VALUE: if-none-match:
VALUE: if-range:
VALUE: if-unmodified-since:
VALUE: max-forwards:
VALUE: proxy-authorization:
VALUE: range:
VALUE: referer:
VALUE: user-agent:

VALUE: allow:             
VALUE: content-base:      
VALUE: content-encoding:  
VALUE: content-language:  
VALUE: content-length:    
VALUE: content-location:  
VALUE: content-md5:       
VALUE: content-range:     
VALUE: content-type:      
VALUE: etag:              
VALUE: expires:           
VALUE: last-modified:     

SWAP WARNING !

;MODULE

: CreateServerSocket ( port -- socket )
  { port \ s }
  CreateSocket THROW -> s
  port s BindSocket THROW
  s ListenSocket THROW
  s
;

: SEND-FILE ( hfile -- )
\ hfile уничтожается!
  { h \ end rem [ /FILE_BUFF ] buff }
  h FILE-SIZE THROW /FILE_BUFF
  UM/MOD SWAP TO rem
  0 ?DO buff /FILE_BUFF h READ-FILE THROW
       buff SWAP http-socket @ WriteSocket THROW
    LOOP
  buff rem h READ-FILE THROW
  buff SWAP http-socket @ WriteSocket THROW
  h CLOSE-FILE THROW
;

: WRITE ( str -- )
  DUP STR@ http-socket @ WriteSocket THROW STRFREE
;

: TO-LOWER ( addr u -- addr u )
  2DUP
  OVER + SWAP
  ?DO
    I C@
    [ CHAR A CHAR a XOR ] LITERAL OR
    I C!
   LOOP
;


: SEND-NOTFOUND  { \ file }
     "" TO file
     ROOT-DIR file STR+ 
     S" /nosuchfile" file STR+
     " HTTP/1.0 404 File not found{CRLF}Content-Type: text/html{CRLF}{CRLF}"
     WRITE
     file STR@ R/O OPEN-FILE-SHARED THROW SEND-FILE
     file STRFREE
;

: CHECK-URI ( -- f)
     uri @ STR@ S" .." SEARCH 0= NIP NIP
;

: FORBIDDEN { \ file }
     "" TO file
     ROOT-DIR file STR+ 
     S" /forbidden" file STR+
     " HTTP/1.0 401 Forbidden{CRLF}Content-Type: text/html{CRLF}{CRLF}"
     WRITE
     file STR@ R/O OPEN-FILE-SHARED THROW SEND-FILE
     file STRFREE
;


: MAKE-FILE  { a u \ fid a1 u1 }

    a u  S" :" SEARCH 0= IF 2DROP EXIT THEN

    1- TO u1 1+ TO a1 0 a1 1- C!

    a a1 a - 1-  W/O CREATE-FILE THROW TO fid 
 
    a1 u1  fid WRITE-FILE THROW

    fid CLOSE-FILE THROW

;


: MAKE-FORTH   ( a u -- ) FORTHCODE   WRITE  ;

: MAKE-BBCODE  ( a u -- ) BBCODE      WRITE  ;



: ANSWER-REQUEST
  { \ file fid a u }
  "" TO file
  ROOT-DIR file STR+
  uri @ STR@ 2DUP file STR+
  + 1- C@ [CHAR] / =
  IF S" index.html" file STR+ THEN
  
 file STR@  2DUP CR ." file: " TYPE CR 

  R/O OPEN-FILE-SHARED
  IF DROP SEND-NOTFOUND
  ELSE " HTTP/1.0 200 OK{CRLF}Connection: close{CRLF}{CRLF}" WRITE
       ['] SEND-FILE CATCH
  THEN

  file STR@ S" ./save.html" COMPARE 0=  active @  AND  

  IF   posted @ STR@  MAKE-FILE  THEN

  file STR@ S" ./forth.html" COMPARE 0=  active @  AND  

  IF   posted @ STR@  MAKE-FORTH  THEN

  file STR@ S" ./bbcode.html" COMPARE 0=  active @  AND  

  IF   posted @ STR@  MAKE-BBCODE  THEN



file STRFREE

;

: REFILL-LOOP

   BEGIN
     PeekChar 13 =
     IF   -1
     ELSE
     NextWord TO-LOWER SFIND
         IF EXECUTE 0
         ELSE 2DROP 13 PARSE 10 SKIP 2DROP \ пропускаем если не нашли
         THEN
     THEN
   UNTIL
  \ Теперь если POST надо взять данные

   active @ 

  IF 
       SOURCE CRLFCRLF 4 SEARCH 2DROP 4 +

    0. content-length: @ STR@  >NUMBER 2DROP DROP   

       posted S!

 CR  ." <>" posted @ STR@ ( REPLACE+ HTTP-DECODE ) TYPE CR     

THEN


;



: PROCESS_REQUEST ( addr u -- )

   2DUP TYPE CR

  \ Установим минимально необходимые переменные
      S" /nosuchfile" uri S! S" HTTP/1.0" protocol S!
      S" 0" content-length: S!
      command? ON
  ONLY HTTP
  ['] REFILL-LOOP EVALUATE-WITH
  ONLY CHECK-URI IF ANSWER-REQUEST ELSE FORBIDDEN THEN
;

: (WS-THREAD) ( socket -- )
  { s \ offs [ /QUERY_BUFF ] mem } 
  s http-socket !
  BEGIN
    mem offs + /QUERY_BUFF offs - s ReadSocket -1002 =
    SWAP AT offs +!
    mem offs CRLFCRLF 4 SEARCH NIP NIP OR
  UNTIL
  mem offs PROCESS_REQUEST



;

:NONAME ( socket -- ) 
  { s }
  s ['] (WS-THREAD) CATCH ?DUP IF CR CR ." ERROR !!! = " . CR  THEN 
  s CloseSocket DROP
;

TASK: WS-THREAD

: (WS-SERVER) ( port -- )
  { \ ss }
  CreateServerSocket -> ss
  BEGIN
    ss AcceptSocket 0=
  WHILE
    WS-THREAD START DROP
  REPEAT DROP
  ss CloseSocket THROW
;

:NONAME ( port -- ) ['] (WS-SERVER) CATCH DROP ; TASK: WS-SERVER

: CSM-SERVER ( port S"dir" -- )
    SocketsStartup THROW
    $root-dir S!
    WS-SERVER START
;




: AAA

STARTLOG

86 DUP ." csm server listen port " . CR

S" ." CSM-SERVER ." TYPE " . ." ''STOP'' TO STOP THE SERVER THREAD" CR

;

' AAA MAINX !

S" csm86.exe" SAVE

BYE



