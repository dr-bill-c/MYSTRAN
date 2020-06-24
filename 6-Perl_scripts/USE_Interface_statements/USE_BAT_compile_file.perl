# USE_BAT_Compile_file.perl:
# -------------------------

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

# This perl script creates a batch file (called Compile_USE.BAT) used to compile all of the USE_Ifs source files. It is run from a
# batch file (USE_Compile_file.BAT) which will have a line executing this perl file for every USE_IFs source file

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

# ************************************************************************************************************************************
print ("ECHO OFF", "\n");
$program_name = $0;
print ("echo ###############################################################################################################################","\n");
print ("echo This BAT file created on ",$month,"/",$day_of_month,"/",$zero_hour,$year," at ",$hour,":",$zero_min,$minute,$day_night," by ",$program_name,"\n");

$name[0] = "parent_dir      ";
$name[1] = "input_switches  ";
$name[2] = "deb_or_prod     ";
$name[3] = "filename_and_ext";
$name[4] = "file_num        ";

$parent_dir       = @ARGV[0];
$input_switches   = @ARGV[1];
$deb_or_prod      = @ARGV[2];
$filename_and_ext = @ARGV[3];
$file_num         = @ARGV[4];

print ("echo Fortran compilation of Module subroutine: ", $filename_and_ext, "\n");
print ("\n");

$num_args = $#ARGV + 1;
print ("echo ARGV has ", $num_args, " arguments, shown below, with the variable name given to them on the left and the actual value on the right:", "\n");
print ("echo ---------------------------------------------------------------------------------------------------------------------", "\n");

if ($deb_or_prod =~ /deb/i)                                # Set the switches for complation
{
  $built_in_switches = "-f95 -c -info  -chk  -pca  -stchk  -trace  -ap  -g -nco ";
}
elsif ($deb_or_prod =~ /prod/i)
{
  $built_in_switches = "-f95 -c -info -nchk -npca -nstchk -ntrace -nap -ng -w -ncover -o1 -nsav ";
}
else
{
  $built_in_switches = "----------wrong value for built_in_switches----------";
  print ("deb_or_prod  = ", $deb_or_prod, ", resulting in:", "\n"); 
  print ("----------wrong value for built_in_switches----------", "\n");
  exit 0;
} 

$arg_num = 0;
while ($arg_num <= $num_args-1)
{
    printf("%s %-14s %s", "echo", $name[$arg_num], " <-  ARGV");
    if ($name[$arg_num] =~ /input_switches/i)
    {
       printf("%s %d %-s %s %s %s %s", "(", $arg_num, ") = ", $ARGV[$arg_num], "    (and built_in_switches = ", $built_in_switches, ")");  print("\n");
    }
    else
    {
       printf("%s %d %-s %s", "(", $arg_num, ") = ", $ARGV[$arg_num]);  print("\n");
    }
    $arg_num++;
}
print ("\n");

($filename, $ext) = split (/\./, $filename_and_ext, 2);
#print ("echo filename           =  ", $filename        , "\n");
#print ("echo ext                =  ", $ext             , "\n");
#print ("\n");

print ("echo *******************************************************************************************************************************","\n");

print ("DEL  \\MYSTRAN\\%1\\Compile_errors\\Compilation-error-messages-",$parent_dir,".TXT","\n");

print ("echo ",$file_num,") Compiling file ",$filename_and_ext,"\n");
print ("echo --------------------------------------------------","\n");
print ("IF NOT EXIST ",$filename_and_ext," GOTO ABORT",$file_num,"1","\n");
print ("DEL  ",$filename,".ERR","\n");
print ("DEL  ",$filename,".OBJ","\n");
print ("LF95 ",$input_switches," ",$built_in_switches," -MOD \\MYSTRAN\\%1\\Modules\\MOD ",$filename_and_ext," > ",$filename,".ERR","\n");
print ("IF ERRORLEVEL 1 GOTO ABORT",$file_num,"2","\n");
print ("GOTO QUIT",$file_num,"\n");
print (":ABORT",$file_num,"1","\n");
print ("echo source file ",$filename_and_ext," not found ","\n");
print ("GOTO QUIT",$file_num,"\n");   print ();
print (":ABORT",$file_num,"2","\n");
print ("echo Compilation errors in file ",$filename_and_ext,"\n");
print ("CALL Compilation_error_messages.BAT ",$parent_dir," ",$filename_and_ext," \\MYSTRAN\\%1\\Compile_errors\\Compilation-error-messages-",$parent_dir,".TXT","\n");
print (":QUIT",$file_num,"\n");
print ("echo *******************************************************************************************************************************","\n");

print ("TYPE \\MYSTRAN\\%1\\Compile_errors\\Compilation-error-messages-",$parent_dir,".TXT","\n");
print ("TYPE \\MYSTRAN\\%1\\Compile_errors\\Compilation-error-messages-",$parent_dir,".TXT"," >> \\MYSTRAN\\%1\\Compile_errors\\Compilation-error-messages-All.TXT","\n");
print ("echo ###############################################################################################################################","\n");
print ("\n");   print ("\n");    print ("\n"); 

exit 0;
