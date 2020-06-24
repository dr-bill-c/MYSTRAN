ECHO OFF

rem  Using perl script Count_lines.perl, counts lines of code in the input files, enumerated here, and directs that output to file %6 in directory %2\MYSTRAN\Compile_files
rem  MYSTRAN is the directory from which this BAT file will be executed (we assume that this BAT file exists in a directory that is in the current path
rem  %1 = Y or N. If Y then lines of code from all FOR files in subdirectories to MYSTRAN\%1 that are not ARPACK*.FOR, LAPACK*.FOR, BANDIT*.FOR or *Interface.FOR will be counted
rem  %2 = Y or N. If Y then lines of code from all FOR files in subdirectories to MYSTRAN\%1 that are *Interface.FOR will be counted
rem  %3 = Y or N. If Y then lines of code from all FOR files in subdirectories to MYSTRAN\%1 that are *USE_IFs.FOR   will be counted
rem  %4 = Y or N. If Y then lines of code from all FOR files in subdirectories to MYSTRAN\%1 that are ARPACK*.FOR    will be counted
rem  %5 = Y or N. If Y then lines of code from all FOR files in subdirectories to MYSTRAN\%1 that are BANDIT*.FOR    will be counted
rem  %6 = Y or N. If Y then lines of code from all FOR files in subdirectories to MYSTRAN\%1 that are LAPACK*.FOR    will be counted
rem  %7 is the subdirectory ,C or D, in MYSTRAN where files whose lines to be counted exist
rem  %8 is the file name to put the results in ( \MYSTRAN\%1\Count_lines\%7)
rem  Even if %1, %2, %3, %4 and %5 are Y, a summary is given that gives the total lines of code in all 5 categories above

CD \MYSTRAN
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Count_lines\Count_lines.perl %1 %2 %3 %4 %5 %6 \MYSTRAN\%7\MAIN\*.FOR \MYSTRAN\%7\EMG\EMG1\*.FOR \MYSTRAN\%7\EMG\EMG2\*.FOR \MYSTRAN\%7\EMG\EMG3\*.FOR \MYSTRAN\%7\EMG\EMG4\*.FOR \MYSTRAN\%7\EMG\EMG5\*.FOR \MYSTRAN\%7\EMG\EMG6\*.FOR \MYSTRAN\%7\EMG\EMG7\*.FOR \MYSTRAN\%7\EMG\EMG8\*.FOR \MYSTRAN\%7\LK1\L1A\*.FOR \MYSTRAN\%7\LK1\L1A-BD\*.FOR \MYSTRAN\%7\LK1\L1A-CC\*.FOR \MYSTRAN\%7\LK1\L1B\*.FOR \MYSTRAN\%7\LK1\L1C\*.FOR \MYSTRAN\%7\LK1\L1D\*.FOR \MYSTRAN\%7\LK1\L1E\*.FOR \MYSTRAN\%7\LK1\L1U\*.FOR \MYSTRAN\%7\LK1\LINK1\*.FOR \MYSTRAN\%7\LK2\*.FOR \MYSTRAN\%7\LK3\*.FOR \MYSTRAN\%7\LK4\*.FOR \MYSTRAN\%7\LK5\*.FOR  \MYSTRAN\%7\LK6\*.FOR \MYSTRAN\%7\LK9\L91\*.FOR \MYSTRAN\%7\LK9\L92\*.FOR \MYSTRAN\%7\LK9\LINK9\*.FOR \MYSTRAN\%7\Modules\*.FOR \MYSTRAN\%7\Modules\ARPACK\*.FOR \MYSTRAN\%7\Modules\LAPACK\*.FOR \MYSTRAN\%7\Modules\BANDIT\*.FOR \MYSTRAN\%7\UTIL\*.FOR \MYSTRAN\Interfaces\*.FOR \MYSTRAN\USE_IFs\*.FOR > \MYSTRAN\%7\Count_lines\%8
ECHO ON