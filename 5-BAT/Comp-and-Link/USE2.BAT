ECHO OFF

rem This batch file will generate another batch file, USE_Compile_file.BAT, that is executed to generate another batch file, Compile_USE.BAT
rem which, when executed, will compile the subrname_USE_IFs.f90 created when USE1.BAT was run (see USE1.BAT file for an explanation of how it works 

rem In this batch file do not include Module files (these do not need interfaces)

rem To run this batch file, at the DOS prompt type USE2 %1 where:
rem   (b) %1 is the subdirectory in MYSTRAN (e.g. C or D)

rem The output (i.e. the file which is used to do the compilation) will go into D:\MYSTRAN\BAT\Comp-and-Link\USE_Compile_file.BAT

\perl\bin\perl D:\MYSTRAN\Perl_scripts\USE_Interface_statements\Generate_USE_Compile_file-BAT.perl   "-nfix -in"   D:\MYSTRAN\%1\ARPACK\ARPACK_INFO_MSG.f90 D:\MYSTRAN\%1\BANDIT\BANDIT_FILES.f90 D:\MYSTRAN\%1\UTIL\*.f90 D:\MYSTRAN\%1\MAIN\*.f90 D:\MYSTRAN\%1\EMG\EMG1\*.f90 D:\MYSTRAN\%1\EMG\EMG2\*.f90 D:\MYSTRAN\%1\EMG\EMG3\*.f90 D:\MYSTRAN\%1\EMG\EMG4\*.f90 D:\MYSTRAN\%1\EMG\EMG5\*.f90 D:\MYSTRAN\%1\EMG\EMG6\*.f90 D:\MYSTRAN\%1\EMG\EMG7\*.f90 D:\MYSTRAN\%1\EMG\EMG8\*.f90 D:\MYSTRAN\%1\LK1\L1A\*.f90 D:\MYSTRAN\%1\LK1\L1A-BD\*.f90 D:\MYSTRAN\%1\LK1\L1A-CC\*.f90 D:\MYSTRAN\%1\LK1\L1B\*.f90 D:\MYSTRAN\%1\LK1\L1C\*.f90 D:\MYSTRAN\%1\LK1\L1D\*.f90 D:\MYSTRAN\%1\LK1\L1E\*.f90 D:\MYSTRAN\%1\LK1\L1U\*.f90 D:\MYSTRAN\%1\LK1\LINK1\*.f90 D:\MYSTRAN\%1\LK2\*.f90 D:\MYSTRAN\%1\LK3\*.f90 D:\MYSTRAN\%1\LK4\*.f90 D:\MYSTRAN\%1\LK5\*.f90 D:\MYSTRAN\%1\LK6\*.f90 D:\MYSTRAN\%1\LK9\L91\*.f90 D:\MYSTRAN\%1\LK9\L92\*.f90 D:\MYSTRAN\%1\LK9\LINK9\*.f90 > D:\MYSTRAN\BAT\Comp-and-Link\USE_Compile_file.BAT

ECHO ON