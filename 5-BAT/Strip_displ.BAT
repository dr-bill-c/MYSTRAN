ECHO OFF

rem This batch file will remove the lines from an MSC F06 file that have the separator text lines between pages 
rem Type Strip_displ at the DOS prompt
 
D:\perl\bin\perl D:\MYSTRAN\Perl_scripts\Strip_MSC\strip_displ.perl D:\MYSTRAN\D\RUNS\Mega\Orig-from-Richard\MODEL002_MSC_Displs.txt > D:\MYSTRAN\D\RUNS\Mega\Orig-from-Richard\MODEL002_MSC_Displs_clean.txt
