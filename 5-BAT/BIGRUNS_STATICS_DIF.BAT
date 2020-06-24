ECHO OFF

rem This BAT file checks all ANS files to make sure that all lines agree with those from the archive (ARC) files for MYSTRAN test runs
rem To run this BAT file type: BIGRUNS_RUNS_DIF %1 at the prompt where:
rem      (a) %1 is the subdirectory (C or D) in MYSTRAN where the test runs to check reside

rem The archive answer files are in D:\MYSTRAN\%1\Runs\TestRuns\Archive_answers (note they are in subdirectory D of MYSTRAN)

CD \MYSTRAN\BigRuns\LAPACK\STATICS\%1
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Statics\Archive\   D:\MYSTRAN\BigRuns\LAPACK\Statics\%1\  AQ3U2S002
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Statics\Archive\   D:\MYSTRAN\BigRuns\LAPACK\Statics\%1\  AQ3U2S004
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Statics\Archive\   D:\MYSTRAN\BigRuns\LAPACK\Statics\%1\  AQ3U2S008
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Statics\Archive\   D:\MYSTRAN\BigRuns\LAPACK\Statics\%1\  AQ3U2S016
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Statics\Archive\   D:\MYSTRAN\BigRuns\LAPACK\Statics\%1\  AQ3U2S032
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Statics\Archive\   D:\MYSTRAN\BigRuns\LAPACK\Statics\%1\  AQ3U2S064
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Statics\Archive\   D:\MYSTRAN\BigRuns\LAPACK\Statics\%1\  AQ3U2S128
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Statics\Archive\   D:\MYSTRAN\BigRuns\LAPACK\Statics\%1\  BAR1
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Statics\Archive\   D:\MYSTRAN\BigRuns\LAPACK\Statics\%1\  BAR10
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Statics\Archive\   D:\MYSTRAN\BigRuns\LAPACK\Statics\%1\  BAR100
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Statics\Archive\   D:\MYSTRAN\BigRuns\LAPACK\Statics\%1\  BAR1000
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Statics\Archive\   D:\MYSTRAN\BigRuns\LAPACK\Statics\%1\  BAR10000

ECHO ON