rem ECHO ON
CD D:\MYSTRAN\%1

REM ************************
REM Compile Main

CD MAIN
DEL *.OBJ
CALL CMY %1
DEL *.FWD
CD ..

REM ************************
CD ARPACK
CALL CARPACK %1
CD ..

REM ************************
CD BANDIT
CALL CBANDIT %1
CD ..

REM ************************
REM Compile Utilities

CD UTIL
DEL *.OBJ
CALL CUTIL0 %1
CALL CUTIL-ALL %1
CD ..

REM ************************
REM Compile EMG

CD EMG
CALL CEMG-ALL %1
CD ..

REM ************************
REM Compile LINK1

CD LK1
CALL CLK1-ALL %1
CD ..

REM ************************
REM Compile LINK2

CD LK2
DEL *.OBJ
CALL CLK2-ALL %1
CD ..
REM ************************
REM Compile LINK3

CD LK3
DEL *.OBJ
CALL CLK3-ALL %1
CD ..

REM ************************
REM Compile LINK4

CD LK4
DEL *.OBJ
CALL CLK4-ALL %1
CD ..

REM ************************
REM Compile LINK5

CD LK5
DEL *.OBJ
CALL CLK5-ALL %1
CD ..

REM ************************
REM Compile LINK6

CD LK6
DEL *.OBJ
CALL CLK6-ALL %1
CD ..

REM ************************
REM Compile LINK9

CD LK9
CALL CLK9-ALL %1
CD ..
CD ..
