# BAT_I_compile_file.perl

# Begin license text.                                                                                    
# _______________________________________________________________________________________________________
                                                                                                         
# Copyright 2019 Dr William R Case, Jr (dbcase29@gmail.com)                                              
                                                                                                         
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and      
# associated documentation files (the "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to   
# the following conditions:                                                                              
                                                                                                         
# The above copyright notice and this permission notice shall be included in all copies or substantial   
# portions of the Software.                                                                              
                                                                                                         
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS                                
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,                            
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE                            
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER                                 
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,                          
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN                              
# THE SOFTWARE.                                                                                          
# _______________________________________________________________________________________________________
                                                                                                        
# End license text.                                                                                      

# Creates the xxxx_I.BAT file used to compile MYSTRAN interface routines (e.g. like CEMG1_I.BAT)

# This can be used as follows:

# Create a BAT file, say BAT_I_compile_file.BAT, that contains the following command(s):

#    \perl\bin\perl \MYSTRAN\perl_scripts\BAT_compile_file\BAT_I_compile_file.perl %1 %2 %3 %4 > %5

# As an example (with actual values for %1, %2, %3, %4 %5):

#    \perl\bin\perl \MYSTRAN\perl_scripts\BAT_compile_file\BAT_I_compile_file.perl EMG1 EMG\EMG1 ELMDAT ELMGM1 ELMGM2 ELMTYPE EMG > \MYSTRAN\EMG\EMG1\CEMG1_I.BAT
#    \perl\bin\perl \MYSTRAN\Perl_scripts\BAT_compile_file\BAT_I_compile_file.perl EMG1 EMG\EMG1"ELMDAT ELMGM1 ELMGM2 ELMTYPE EMG > \MYSTRAN\D\EMG\EMG1\CEMG1_I.BAT

# where:

#   %1 would be a name (e.g. EMG1 in the above example) to print out to the xxxx_I.BAT file in several locations (see $name below)
#   %2 would be the subdirectory (e.g. EMG\EMG1 in the above example) where the files to compile reside. This is used only for printout of errors 
#   %3 is either Y or N. If Y then -nfix is a compiler directive in the LF95 command line put into the (e.g.)
#      CEMG1.BAT, etc, files created  
#   %4 would be the list of interface files  to be compiled in the xxxx_I.BAT
#     (e.g., in the above example: ELMDAT ELMGM1 ELMGM2 ELMTYPE EMG - with an assumed last part of _Interface.f90)
#   %5 would be where the output is directed (e.g., in the above example: \MYSTRAN\EMG\EMG1\CEMG1_I.BAT)

# Assume the following:

#    1) perl.exe is in directory \perl\bin,
#    2) This script (BAT_I_compile_file.perl) is in directory \MYSTRAN\perl_scripts\BAT_compile_file

# Copyright (c) 2001 Bill Case.

# Change log:
# **********************************************************************************************************************************
# Get time and date (see code in Time_and_date.perl)

$seconds_since_1_1_1970 = time();
@local_time_date = localtime ($seconds_since_1_1_1970);

$seconds               = @local_time_date[0];
$minute                = @local_time_date[1];
$hour                  = @local_time_date[2];
$day_of_month          = @local_time_date[3];
$month                 = @local_time_date[4] + 1;
$year                  = @local_time_date[5] + 1900;
$day_of_week           = @local_time_date[6] + 1;
$day_of_year           = @local_time_date[7] + 1;
$daylight_savings_flag = @local_time_date[8];

if ($minute >= 10)
{
   $zero_min = "";
}
else
{
   $zero_min = "0";
}


if ($hour <= 12)
{
   $day_night = " AM";
}
else
{
   $day_night = " PM";
   $hour = $hour - 12;
}

if ($year >= 2010)
{
   $zero_hour = "";
}
else
{
   $zero_hour = "0";
}
$year = $year - 2000;

# **********************************************************************************************************************************
$perl_script_name   = $0;

@words = split (/\\/,$perl_script_name);
$num_wrds = $#words;
$perl_filename = @words[$num_wrds];

$arg_num              = 0;                         # code to find the arg number of the 1st file listed in @ARGV (assumes file extension is last arg before the 1st file)
$num_args             = $#ARGV + 1;
$parent_dir           = @ARGV[0];  $arg_num = $arg_num + 1;
$sub_dir              = @ARGV[1];  $arg_num = $arg_num + 1;
$move                 = @ARGV[2];  $arg_num = $arg_num + 1;
$input_switches       = @ARGV[3];  $arg_num = $arg_num + 1;
$deb_or_prod          = @ARGV[4];  $arg_num = $arg_num + 1;
$ext                  = @ARGV[5];  $arg_num = $arg_num + 1;

$num_args_bef_files = $arg_num; #print ("num_args_bef_files = ", $num_args_bef_files, "\n" );  print("\n");
$num_files          = $num_args - $num_args_bef_files;

printf ("%28s %-25s %-s","rem ( 1) perl_filename      = ", $perl_filename     , ", The name of the perl script that was used to generate this batch file that will generate and compie Interface modules"); print ("\n");
printf ("%28s %-25d %-s","rem ( 2) num_args           = ", $num_args          , ", The number of arguments fed to the perl script in ( 1) from the batch file that called it"); print ("\n");
printf ("%28s %-25d %-s","rem ( 3) num_args_bef_files = ", $num_args_bef_files, ", The 1st file name will be the next record in the batch file that called the perl script in ( 1)"); print ("\n");
printf ("%28s %-25s %-s","rem ( 4) name               = ", $parent_dir        , ", The root name of the set of Interface files for which this batch file will compile"); print ("\n");
printf ("%28s %-25s %-s","rem ( 5) dir                = ", $sub_dir           , ", The directory in \MYSTRAN\C where the source files used to generate the Interface modules exists"); print ("\n");
printf ("%28s %-25s %-s","rem ( 6) move               = ", $move              , ", If 'Y' the Interface module source code will be moved to \MYSTRAN\Interfaces"); print ("\n");
printf ("%28s %-25s %-s","rem ( 7) deb_or_prod        = ", $deb_or_prod       , ", This input should be 'prod' for production source files in \MYSTRAN\C"); print ("\n");
printf ("%28s %-25s %-s","rem ( 8) ext                = ", $ext               , ", This is the file extension (typically 'f90' or 'f') read from the batch file that called the perl script in ( 1"); print ("\n");
printf ("%28s %-25d %-s","rem ( 9) num of files       = ", $num_files         , ", This is the number of files to process. It is equal to ( 2) minus ( 3)"); print ("\n");
printf ("%28s %-25s %-s","rem (10) input_switches     = ", $input_switches    , ", These are the compile switches read from the batch file that called the perl script in ( 1)"); print ("\n");

 if ($deb_or_prod =~ /deb/i)
{
  $built_in_switches = "-f95 -c -info  -chk  -pca  -stchk  -trace  -ap  -g -nco ";
}
elsif ($deb_or_prod =~ /prod/i)
{
  $built_in_switches = "-f95 -c -info -nchk -npca -nstchk -ntrace -nap -ng -w -ncover -o1 -nsav ";
}
else
{
  $built_in_switches = "---------- wrong value for built_in_switches  ----------";
}
print ("\n");
print ("rem built_in_switches = ", $built_in_switches , "\n"); 
print ("rem built_in_switches are ones that are built into the perl script: ", $perl_filename," used to create this batch file","\n"); 
print ("\n");

$directory_out = "\\MYSTRAN\\Interfaces\\";
print ("ECHO OFF","\n");
print ("\n");
print ("rem Fortran compilation of ",$parent_dir," Interface subroutines","\n");
print ("rem This BAT file created on ",$month,"/",$day_of_month,"/",$zero_hour,$year," at ",$hour,":",$zero_min,$minute,$day_night," by the perl script: ",$perl_filename,"\n");
print ("\n");

print ("DEL \\MYSTRAN\\%1\\Compile_errors\\Compilation-error-messages-",$parent_dir,"_I.TXT","\n");

$file_count = 1;
while ($file_count <= $num_files)
{
   $filename = $ARGV[$num_args_bef_files + $file_count - 1];   # subtract 1 since @ARGV's 1st record number is 0
   
   $dir_and_file_out = $directory_out . $filename;

   print ("\n");
#  print ("IF NOT %",$file_count," == 1 GOTO START",$file_count+1,"\n");
   print ("echo ###############################################################################################################################","\n");
   print ("echo ",$file_count,") Compiling file ",$filename,"_Interface.",$ext,"\n");
   print ("echo    -----------------------------------------------","\n");
   print ("DEL ",$dir_and_file_out,"_Interface.*","\n");
   print ("DEL \\MYSTRAN\\%1\\Modules\\MOD\\",$filename,"_Interface.MOD","\n");
   print ("\n");
   print ("echo The batch in the statement below uses a perl script to create an Interface module, ", $filename,"_Interface.",$ext, ", based on the code in the source file ", $filename, ".", $ext,"\n");
   print ("CALL Int_Mod_use_in_CLKij.BAT ",$filename,".",$ext," ",$filename,"_Interface.",$ext,"\n");
   print ("\n");
   print ("echo Now compile the interface module, ", $filename,"_Interface.",$ext,"\n");
   print ("LF95 ",$input_switches," ",$built_in_switches ," -MOD \\MYSTRAN\\%1\\Modules\\MOD ",$filename,"_Interface.",$ext," > ",$filename,"_Interface.ERR","\n");
   print ("\n");
   print ("IF ERRORLEVEL 1 GOTO ABORT",$file_count,"2","\n");
   print ("GOTO QUIT",$file_count,"\n");
   print (":ABORT",$file_count,"2","\n");
   print ("echo Compilation errors in file ",$filename,"_Interface.",$ext,"\n");
   print ("CALL Compilation_error_messages.BAT ",$sub_dir," ",$filename,"_Interface,".",$ext ","\\MYSTRAN\\%1\\Compile_errors\\Compilation-error-messages-",$parent_dir,"_I.TXT","\n");
   print (":QUIT",$file_count,"\n");
   print ("DEL ",$dir_and_file_out,"_Interface.fwd","\n");
#  print (":START",$file_count+1,"\n");
   print ("echo End of ", $filename,"_Interface.",$ext, " batch file compilation lines","\n");
   print ("echo ###############################################################################################################################","\n");

   $file_count++;
   print ("\n");
   print ("\n");
}

if ($move =~ Y)
{
   print ("COPY *_Interface.* \\MYSTRAN\\Interfaces","\n");
   print ("DEL  *_Interface.*","\n");
}

print ("TYPE \\MYSTRAN\\%1\\Compile_errors\\Compilation-error-messages-",$parent_dir,"_I.TXT","\n");
print ("TYPE \\MYSTRAN\\%1\\Compile_errors\\Compilation-error-messages-",$parent_dir,"_I.TXT"," >> \\MYSTRAN\\%1\\Compile_errors\\Compilation-error-messages-All.TXT","\n");


exit 0;
