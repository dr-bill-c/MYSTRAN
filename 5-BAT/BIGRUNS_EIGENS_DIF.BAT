ECHO OFF

rem This BAT file checks all ANS files to make sure that all lines agree with those from the archive (ARC) files for MYSTRAN test runs
rem To run this BAT file type: BIGRUNS_RUNS_DIF %1 at the prompt where:
rem      (a) %1 is the subdirectory (C or D) in MYSTRAN where the test runs to check reside

rem The archive answer files are in D:\MYSTRAN\%1\Runs\TestRuns\Archive_answers (note they are in subdirectory D of MYSTRAN)

CD \MYSTRAN\BigRuns\LAPACK\EIGENS\%1
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S002-INV
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S002-LANCZOS-MODE-2-DPB
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S002-MGIV

D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S004-INV
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S004-LANCZOS-MODE-2-DPB
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S004-MGIV

D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S008-INV
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S008-LANCZOS-MODE-2-DPB
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S008-MGIV

D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S016-INV
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S016-LANCZOS-MODE-2-DPB
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S016-MGIV

D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S032-INV
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S032-LANCZOS-MODE-2-DPB
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S032-MGIV

D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S064-INV
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S064-LANCZOS-MODE-2-DPB
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S064-MGIV

D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S128-INV
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S128-LANCZOS-MODE-2-DPB
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Check_test_run_answers\Check_test_run_answers.perl   D:\MYSTRAN\BigRuns\LAPACK\Eigens\Archive\    D:\MYSTRAN\BigRuns\LAPACK\Eigens\%1\   AQ3U2S128-MGIV

ECHO ON