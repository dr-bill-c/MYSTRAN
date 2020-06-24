rem: Copies cleaned source in D:\MYSTRAN\C to D:\MYSTRAN-FEA-Program
rem: (deletes old files first and renames source ext to f90)

echo off
rem cd   D:\MYSTRAN
del  D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\*.f90 /s
del  D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\*.txt /s
del  D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\*.bak /s
del  D:\MYSTRAN-FEA-Program\2-Documentation\*.pdf

copy D:\MYSTRAN\C\ARPACK\*.f90                                        D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\ARPACK\*.f90
copy D:\MYSTRAN\C\BANDIT\*.f90                                        D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\BANDIT\*.f90
copy D:\MYSTRAN\C\EMG\EMG1\*.f90                                      D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\EMG\EMG1\*.f90
copy D:\MYSTRAN\C\EMG\EMG2\*.f90                                      D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\EMG\EMG2\*.f90
copy D:\MYSTRAN\C\EMG\EMG3\*.f90                                      D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\EMG\EMG3\*.f90
copy D:\MYSTRAN\C\EMG\EMG4\*.f90                                      D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\EMG\EMG4\*.f90
copy D:\MYSTRAN\C\EMG\EMG5\*.f90                                      D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\EMG\EMG5\*.f90
copy D:\MYSTRAN\C\EMG\EMG6\*.f90                                      D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\EMG\EMG6\*.f90
copy D:\MYSTRAN\C\EMG\EMG7\*.f90                                      D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\EMG\EMG7\*.f90
copy D:\MYSTRAN\C\EMG\EMG8\*.f90                                      D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\EMG\EMG8\*.f90
copy D:\MYSTRAN\C\LK1\L1A\*.f90                                       D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\LK1\L1A\*.f90
copy D:\MYSTRAN\C\LK1\L1A-BD\*.f90                                    D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\LK1\L1A-BD\*.f90
copy D:\MYSTRAN\C\LK1\L1A-CC\*.f90                                    D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\LK1\L1A-CC\*.f90
copy D:\MYSTRAN\C\LK1\L1B\*.f90                                       D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\LK1\L1B\*.f90
copy D:\MYSTRAN\C\LK1\L1C\*.f90                                       D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\LK1\L1C\*.f90
copy D:\MYSTRAN\C\LK1\L1D\*.f90                                       D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\LK1\L1D\*.f90
copy D:\MYSTRAN\C\LK1\L1E\*.f90                                       D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\LK1\L1E\*.f90
copy D:\MYSTRAN\C\LK1\L1U\*.f90                                       D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\LK1\L1U\*.f90
copy D:\MYSTRAN\C\LK1\LINK1\*.f90                                     D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\LK1\LINK1\*.f90
copy D:\MYSTRAN\C\LK2\*.f90                                           D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\LK2\*.f90
copy D:\MYSTRAN\C\LK3\*.f90                                           D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\LK3\*.f90
copy D:\MYSTRAN\C\LK4\*.f90                                           D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\LK4\*.f90
copy D:\MYSTRAN\C\LK5\*.f90                                           D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\LK5\*.f90
copy D:\MYSTRAN\C\LK6\*.f90                                           D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\LK6\*.f90
copy D:\MYSTRAN\C\LK9\L91\*.f90                                       D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\LK9\L91\*.f90
copy D:\MYSTRAN\C\LK9\L92\*.f90                                       D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\LK9\L92\*.f90
copy D:\MYSTRAN\C\LK9\LINK9\*.f90                                     D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\LK9\LINK9\*.f90
copy D:\MYSTRAN\C\MAIN\*.f90                                          D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\MAIN\*.f90
copy D:\MYSTRAN\C\Modules\*.f90                                       D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\Modules\*.f90

del  D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\Modules\DRIVE*.f90

copy D:\MYSTRAN\C\Modules\ARPACK\*.f                                  D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\Modules\ARPACK\*.f
copy D:\MYSTRAN\C\Modules\ARPACK\*.h                                  D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\INCLUDE
copy D:\MYSTRAN\C\Modules\Bandit\*.f                                  D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\Modules\Bandit\*.f
copy D:\MYSTRAN\C\Modules\LAPACK\*.f                                  D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\Modules\LAPACK\*.f
copy D:\MYSTRAN\C\Modules\LAPACK\Unresolved_Externals_Problem\*.f     D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\Modules\LAPACK\Unresolved_Externals_Problem\*.f
copy D:\MYSTRAN\C\UTIL\*.f90                                          D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\UTIL\*.f90
copy D:\MYSTRAN\Interfaces\*.f90                                      D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\Interfaces\*.f90
copy D:\MYSTRAN\USE_IFs\*.f90                                         D:\MYSTRAN-FEA-Program\1-SRC\a-Windows-version\USE_IFs\*.f90

copy D:\MYSTRAN\Linux\Linux-differences\GET_INI_FILNAM.f90            D:\MYSTRAN-FEA-Program\1-SRC\b-Linux-differences\GET_INI_FILNAM.f90
copy D:\MYSTRAN\Linux\Linux-differences\GET_MYSTRAN_DIR.f90           D:\MYSTRAN-FEA-Program\1-SRC\b-Linux-differences\GET_MYSTRAN_DIR.f90
copy D:\MYSTRAN\Linux\Linux-differences\mkl_dtfi.f90                  D:\MYSTRAN-FEA-Program\1-SRC\b-Linux-differences\mkl_dtfi.f90
copy D:\MYSTRAN\Linux\Linux-differences\READ_CL.f90                   D:\MYSTRAN-FEA-Program\1-SRC\b-Linux-differences\READ_CL.f90

copy D:\MYSTRAN\Documentation\MYSTRAN-FEA-Program-File-Structure.pdf  D:\MYSTRAN-FEA-Program\2-Documentation\MYSTRAN-FEA-Program-File-Structure.pdf
copy D:\MYSTRAN\Documentation\MYSTRAN-Users-Manual.pdf                D:\MYSTRAN-FEA-Program\2-Documentation\MYSTRAN-Users-Manual.pdf
copy D:\MYSTRAN\Documentation\MYSTRAN-Install-Manual.pdf              D:\MYSTRAN-FEA-Program\2-Documentation\MYSTRAN-Install-Manual.pdf
copy D:\MYSTRAN\Documentation\MYSTRAN-demo-problem-manual.pdf         D:\MYSTRAN-FEA-Program\2-Documentation\MYSTRAN-demo-problem-manual.pdf
copy D:\MYSTRAN\Documentation\Stress-Tensor-coord-transform.pdf       D:\MYSTRAN-FEA-Program\2-Documentation\Stress-Tensor-coord-transform.pdf

copy D:\MYSTRAN\C\Runs\TestRuns\Eigens\Banded\Archive\*.arc           D:\MYSTRAN-FEA-Program\3-TestRuns\Eigens\Banded\Archive\*.arc
copy D:\MYSTRAN\C\Runs\TestRuns\Eigens\Banded\CurrentRuns\*.ans       D:\MYSTRAN-FEA-Program\3-TestRuns\Eigens\Banded\CurrentRuns\*.ans
copy D:\MYSTRAN\C\Runs\TestRuns\Eigens\Banded\CurrentRuns\*.dat       D:\MYSTRAN-FEA-Program\3-TestRuns\Eigens\Banded\CurrentRuns\*.dat
copy D:\MYSTRAN\C\Runs\TestRuns\Eigens\Banded\CurrentRuns\*.f06       D:\MYSTRAN-FEA-Program\3-TestRuns\Eigens\Banded\CurrentRuns\*.f06

copy D:\MYSTRAN\C\Runs\TestRuns\Statics\Banded\Archive\*.arc          D:\MYSTRAN-FEA-Program\3-TestRuns\Statics\Banded\Archive\*.arc
copy D:\MYSTRAN\C\Runs\TestRuns\Statics\Banded\CurrentRuns\*.ans      D:\MYSTRAN-FEA-Program\3-TestRuns\Statics\Banded\CurrentRuns\*.ans
copy D:\MYSTRAN\C\Runs\TestRuns\Statics\Banded\CurrentRuns\*.dat      D:\MYSTRAN-FEA-Program\3-TestRuns\Statics\Banded\CurrentRuns\*.dat
copy D:\MYSTRAN\C\Runs\TestRuns\Statics\Banded\CurrentRuns\*.f06      D:\MYSTRAN-FEA-Program\3-TestRuns\Statics\Banded\CurrentRuns\*.f06

copy D:\MYSTRAN\Documentation\Docs-other\Lahey-Switches.txt           D:\MYSTRAN-FEA-Program\4-Compile-and-Link-switches\Lahey-Switches.txt

copy D:\MYSTRAN\BAT\Test_Runs.BAT                                     D:\MYSTRAN-FEA-Program\5-BAT\Test_Runs.BAT
copy D:\MYSTRAN\BAT\Test_Runs_Dif.BAT                                 D:\MYSTRAN-FEA-Program\5-BAT\Test_Runs_Dif.BAT
copy D:\MYSTRAN\BAT\Comp-and-Link\*.BAT                               D:\MYSTRAN-FEA-Program\5-BAT\Comp-and-Link\*.BAT
copy D:\MYSTRAN\BAT\Comp-and-Link\*.pdf                               D:\MYSTRAN-FEA-Program\5-BAT\Comp-and-Link\*.pdf

copy D:\MYSTRAN\Perl_scripts\BAT_compile_file\*.perl                  D:\MYSTRAN-FEA-Program\6-Perl_Scripts\Interface_modules\*.perl
copy D:\MYSTRAN\Perl_scripts\Interface_modules\*.perl                 D:\MYSTRAN-FEA-Program\6-Perl_Scripts\Interface_modules\*.perl
copy D:\MYSTRAN\Perl_scripts\USE_Interface_statements\*.perl          D:\MYSTRAN-FEA-Program\6-Perl_Scripts\USE_Interface_statements\*.perl

copy D:\MYSTRAN\Errors-corrected\Errors-corrected.pdf                 D:\MYSTRAN-FEA-Program\Errors-corrected.pdf