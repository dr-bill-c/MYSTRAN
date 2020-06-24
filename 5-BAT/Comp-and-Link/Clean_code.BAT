ECHO OFF
rem Using perl script Clean_code.perl, removes lines of code in input files from D:\MYSTRAN\D that contain !!! or !? and writes the cleaned files to D:\MYSTRAN\C.
rem Messages from the perl script are printed to file \MYSTRAN\D\Clean_code_messages\Clean_code_mesgs.txt 

CD \MYSTRAN
\perl\bin\perl D:\MYSTRAN\Perl_scripts\Clean_code\Clean_code.perl D:\MYSTRAN\D D:\MYSTRAN\C D:\MYSTRAN\D\ARPACK\*.f90 D:\MYSTRAN\D\BANDIT\*.f90 D:\MYSTRAN\D\MAIN\*.f90 D:\MYSTRAN\D\EMG\EMG1\*.f90 D:\MYSTRAN\D\EMG\EMG2\*.f90 D:\MYSTRAN\D\EMG\EMG3\*.f90 D:\MYSTRAN\D\EMG\EMG4\*.f90 D:\MYSTRAN\D\EMG\EMG5\*.f90 D:\MYSTRAN\D\EMG\EMG6\*.f90 D:\MYSTRAN\D\EMG\EMG7\*.f90 D:\MYSTRAN\D\EMG\EMG8\*.f90 D:\MYSTRAN\D\LK1\L1A\*.f90 D:\MYSTRAN\D\LK1\L1A-BD\*.f90 D:\MYSTRAN\D\LK1\L1A-CC\*.f90 D:\MYSTRAN\D\LK1\L1B\*.f90 D:\MYSTRAN\D\LK1\L1C\*.f90 D:\MYSTRAN\D\LK1\L1D\*.f90 D:\MYSTRAN\D\LK1\L1E\*.f90 D:\MYSTRAN\D\LK1\L1U\*.f90 D:\MYSTRAN\D\LK1\LINK1\*.f90 D:\MYSTRAN\D\LK2\*.f90 D:\MYSTRAN\D\LK3\*.f90 D:\MYSTRAN\D\LK4\*.f90 D:\MYSTRAN\D\LK5\*.f90 D:\MYSTRAN\D\LK6\*.f90 D:\MYSTRAN\D\LK9\L91\*.f90 D:\MYSTRAN\D\LK9\L92\*.f90 D:\MYSTRAN\D\LK9\LINK9\*.f90 D:\MYSTRAN\D\UTIL\*.f90 D:\MYSTRAN\D\Modules\*.f90 D:\MYSTRAN\D\Modules\ARPACK\*.f D:\MYSTRAN\D\Modules\BANDIT\*.f D:\MYSTRAN\D\Modules\LAPACK\*.f D:\MYSTRAN\D\Modules\LAPACK\Unresolved_Externals_Problem\*.f  > Clean_code_messages\Clean_code_msgs.txt

CD \MYSTRAN\C\Modules
DEL DRIVE*.*

CALL CD0