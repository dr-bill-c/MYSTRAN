CALL CD0

REM ************************
REM Del BAK in Root

DEL *.BAK

REM ************************
CD Interfaces
DEL *.BAK
DEL *.ERR
DEL *.FWD
DEL *.OBJ
CD ..

REM ************************
CD USE_IFs
DEL *.BAK
DEL *.ERR
DEL *.FWD
DEL *.OBJ
CD ..

REM ************************
CD %1

REM ************************
REM Del BAK, ERR, FWD, OBJ in ARPACK

CD ARPACK
DEL *.BAK
DEL *.ERR
DEL *.FWD
DEL *.OBJ
CD ..

REM ************************
REM Del BAK, ERR, FWD, OBJ in BANDIT

CD BANDIT
DEL *.BAK
DEL *.ERR
DEL *.FWD
DEL *.OBJ
CD ..

REM ************************
REM Del BAK, ERR, FWD, OBJ in MAIN

CD MAIN
DEL *.BAK
DEL *.ERR
DEL *.FWD
DEL *.OBJ
CD ..

REM ************************
REM Del BAK, ERR, FWD, OBJ in Utilities

CD UTIL
DEL *.BAK
DEL *.ERR
DEL *.FWD
DEL *.OBJ
CD ..

REM ************************
REM Del BAK, ERR, FWD, OBJ in Modules

CD Modules
CALL DMOD-ALL
CD ..

REM ************************
REM Del BAK, ERR, FWD, OBJ in EMG

CD EMG
CALL DEMG-ALL
CD ..

REM ************************
REM Del BAK, ERR, FWD, OBJ in LINK1

CD LK1
CALL DLK1-ALL
CD ..

REM ************************
REM Del BAK, ERR, FWD, OBJ in LINK2

CD LK2
DEL *.BAK
DEL *.ERR
DEL *.FWD
DEL *.OBJ
CD ..
REM ************************
REM Del BAK, ERR, FWD, OBJ in LINK3

CD LK3
DEL *.BAK
DEL *.ERR
DEL *.FWD
DEL *.OBJ
CD ..
REM ************************
REM Del BAK, ERR, FWD, OBJ in LINK4

CD LK4
DEL *.BAK
DEL *.ERR
DEL *.FWD
DEL *.OBJ
CD ..

REM ************************
REM Del BAK, ERR, FWD, OBJ in LINK5

CD LK5
DEL *.BAK
DEL *.ERR
DEL *.FWD
DEL *.OBJ
CD ..

REM ************************
REM Del BAK, ERR, FWD, OBJ in LINK6

CD LK6
DEL *.BAK
DEL *.ERR
DEL *.FWD
DEL *.OBJ
CD ..

REM ************************
REM Del BAK, ERR, FWD, OBJ in LINK9

CD LK9
CALL DLK9-ALL
CD ..

REM ************************
REM Del BAK, ERR, FWD, OBJ in READLIJ

CD \MYSTRAN\READLIJ
DEL *.BAK
DEL *.ERR
DEL *.FWD
DEL *.OBJ
CD ..

CD \MYSTRAN