D:
cd \MYSTRAN-FEA-Program
"C:\Program Files\WinZip\WZZIP.exe"   -p -r MYSTRAN-FEA-Program-ver-%1.zip D:\MYSTRAN-FEA-Program\*.* > zip-tasks.txt
rem Listing all files in the zip file
"C:\Program Files\WinZip\WZUNZIP.exe"    -v MYSTRAN-FEA-Program-ver-%1.zip D:\MYSTRAN-FEA-Program\*.* > ziped-files.txt
