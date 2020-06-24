CALL CD0

DEL Interfaces\*.f90
DEL Interfaces\*.ERR
DEL Interfaces\*.OBJ
DEL Interfaces\*.FWD

CD %1

DEL Compile_errors\*.TXT

REM ************************
REM Compile main modules

CD Modules
CALL CMOD1-ALL %1
CD ..

REM ************************
REM Compile OUTA_HERE, OURDAT, OURTIM, MATMULT_SFF interfaces

CD UTIL
CALL CUTIL0_I %1
CD ..

REM ************************
REM Compile ARPACK_INFO_MSG - needed when ARPACK compiled below
CD ARPACK
CALL CARPACK_I %1
CD ..

REM ************************
REM Compile modules that use OUTA_HERE, OURDAT, OURTIM, MATMULT_SFF
REM LAPACK, ARPACK, BANDIT

CD Modules
CALL CMOD2-ALL %1
CD ..

REM ************************
REM Compile Main Interfaces

CD MAIN
DEL *.OBJ
CALL CMY_I %1
CD ..

REM ************************
CD BANDIT
CALL CBANDIT_I %1
CD ..

REM ************************
REM Compile Utilities Interfaces

CD UTIL
CALL CUTIL-ALL_I %1
CD ..

REM ************************
REM Compile EMG Interfaces

CD EMG
CALL CEMG-ALL_I %1
CD ..

REM ************************
REM Compile LINK1 Interfaces

CD LK1
CALL CLK1-ALL_I %1
CD ..

REM ************************
REM Compile LINK2 Interfaces

CD LK2
CALL CLK2-ALL_I %1
CD ..

REM ************************
REM Compile LINK3 Interfaces

CD LK3
CALL CLK3-ALL_I %1
CD ..

REM ************************
REM Compile LINK4 Interfaces

CD LK4
CALL CLK4-ALL_I %1
CD ..

REM ************************
REM Compile LINK5 Interfaces

CD LK5
CALL CLK5-ALL_I %1
CD ..

REM ************************
REM Compile LINK6 Interfaces

CD LK6
CALL CLK6-ALL_I %1
CD ..

REM ************************
REM Compile LINK9 Interfaces

CD LK9
CALL CLK9-ALL_I %1
CD ..
CD ..