ECHO OFF

rem  Using perl script Error_Format.perl, lists lines of code in the input files, enumerated here, that contain

rem  %1 should be C or D (subdirectory of MYSTRAN to search for files)
rem  %2 must be either: ERROR, WARNING or INFORMATION
rem  %3 is the name of the file that the output is directed to in subdirectory \MYSTRAN\%1\Error_Formats

CD \MYSTRAN
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Error_Formats\Error_Formats.perl %2 \MYSTRAN\%1\MAIN\*.f90 \MYSTRAN\%1\EMG\EMG1\*.f90 \MYSTRAN\%1\EMG\EMG2\*.f90 \MYSTRAN\%1\EMG\EMG3\*.f90 \MYSTRAN\%1\EMG\EMG4\*.f90 \MYSTRAN\%1\EMG\EMG5\*.f90 \MYSTRAN\%1\EMG\EMG6\*.f90 \MYSTRAN\%1\EMG\EMG7\*.f90 \MYSTRAN\%1\EMG\EMG8\*.f90 \MYSTRAN\%1\LK1\L1A\*.f90 \MYSTRAN\%1\LK1\L1A-BD\*.f90 \MYSTRAN\%1\LK1\L1A-CC\*.f90 \MYSTRAN\%1\LK1\L1B\*.f90 \MYSTRAN\%1\LK1\L1C\*.f90 \MYSTRAN\%1\LK1\L1D\*.f90 \MYSTRAN\%1\LK1\L1E\*.f90 \MYSTRAN\%1\LK1\L1U\*.f90 \MYSTRAN\%1\LK1\LINK1\*.f90 \MYSTRAN\%1\LK2\*.f90 \MYSTRAN\%1\LK3\*.f90 \MYSTRAN\%1\LK4\*.f90 \MYSTRAN\%1\LK5\*.f90 \MYSTRAN\%1\LK6\*.f90 \MYSTRAN\%1\LK9\L91\*.f90 \MYSTRAN\%1\LK9\L92\*.f90 \MYSTRAN\%1\LK9\LINK9\*.f90 \MYSTRAN\%1\Modules\*.f90 \MYSTRAN\%1\Modules\ARPACK\*.f \MYSTRAN\%1\Modules\BANDIT\*.f \MYSTRAN\%1\Modules\LAPACK\*.f \MYSTRAN\%1\UTIL\*.f90 > \MYSTRAN\%1\Error_Formats\%3
ECHO ON