# Interface_modules.perl
#
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

# Reads Fortran source files and prints out (output redirected to file %2, below) source code for interface modules
# for the input Fortran source files in %1 (below).

# This can be used as follows. Assuming the following:

#    1) perl.exe is in directory \perl\bin,
#    2) This script (Interface_Modules.perl) is in directory \MYSTRAN\perl_scripts\Interface_Modules,
#    3) the files to create interface modules for are in %1,
#    4) The output is directed to file %2 in directory \MYSTRAN,
#    5) The command below is executed as a batch file from the DOS command line in directory \MYSTRAN,

# then the following command (executed as a batch file from the DOS command line in directory \MYSTRAN) will create a
# file (%2) containing the interface modules for %1:

#       \perl\bin\perl \MYSTRAN\perl_scripts\Interface_Modules\Interface_Modules.perl %1 > %2

# where %1 could be L1C\*.f90 to create interface modules for all Fortran source files in subdirectory
# L1C of \MYSTRAN or it could be one Fortran source file.

# The above command could be put into a batch file (say Int_Mod_use_in_CLKij.BAT) and executed at the DOS command line, from
# the \MYSTRAN prompt, as:

#                                          Int_Mod_use_in_CLKij %1 %2

# where %1 is the name of the file where the interface module is to be directed (into directory \MYSTRAN).

# Copyright (c) 2001 Bill Case.

# Change log:

$filecount = 1;

while ($filecount <= @ARGV) 
{
   $filename = $ARGV[$filecount-1];
   if ($filename !~ /_Interface.f90$/)
   {
      print ("! ###############################################################################################################################\n");

      unless (open(INFILE,$filename))
      {
         if (-e $filename)
         {
            die ("File $filename exists, but cannot be opened.\n");
         }
         else
         {
            die ("File $filename does not exist.\n");
         }
      }

      $interface_line_to_keep = 'n';

      $line_count = 0;
NL:   $line = <INFILE>;
      $line_count++;
      
      if ($line =~ /Begin MIT license text/)               # Write out MIT license text
      {
         print ($line);
         $line = <INFILE>;
         $line_count++;
         until ($line =~ /End MIT license text/)
         {
            print ($line);
            $line = <INFILE>;
            $line_count++;
         }
         print ($line);
         $line = <INFILE>;
         $line_count++;
      }

      $position = index($line," ");                        # Now get the subroutine name

      if ($position == 0)
      {
         @words = split (/[\t (]+/,$line);
         $procedure = $words[1];
         if ($words[2] =~ /[a-zA-Z][_0-9a-zA-Z]*\n/)
         {
            chop $words[2];
         }
            $subr_name = $words[2];
      }
      elsif ($position > 0)
      {
         @words = split (/[\t (]+/,$line);
         $procedure = $words[0];
         if ($words[1] =~ /[a-zA-Z][_0-9a-zA-Z]*\n/)
         {
            chop $words[1];
         }
            $subr_name = $words[1];
      }

      if ($line =~ /PROGRAM|SUBROUTINE/i)
      {
         $num_blank_lines = 0;
         print ("\n");
         print ("   MODULE ",$subr_name,"_Interface","\n");
         print ("\n");
         print ("   INTERFACE\n");
         print ("\n");
         print ("$line","\n");

         while ($line =~ /\& *$/ )
         {
            $line = <INFILE>;
            $line_count++;
            $cont_line++;
            print ($line);
         }

         $cont_line = 0;
         $keep_blank_line = 'y';
NL2:     $line = <INFILE>;
         $line_count++;
         $cont_line = 0;
         @words = split (/[\t ]+/,$line);

#        if ($line =~ /! *\*+/)                                                # We found ! **** (etc) - so end of declaration section
         if (($line =~ /! *\*+/) || ($line =~ /END SUBROUTINE/i))              # We found ! **** (etc) - so end of declaration section
         {
            print ("      END ",$procedure," ",$subr_name,"\n");
            print ("\n");
            print ("   END INTERFACE\n");
            print ("\n");
            print ("   END MODULE ",$subr_name,"_Interface","\n");
            print ("\n");
            goto NF;
         }
         else 
         {
            if ($line =~ /^\!/)                                                # Skip all comment lines (which begin with !)
            {
               $keep_blank_line = "n";
               goto NL2
            }
            else                                                               # Line is not one beginning with ! (comment)
            {
               if ($line =~ /^*[0-9a-zA-Z]/ )                                  # These are I/F lines
               {
                  if ($line =~ /USE/i)
                  {                                                            # Do not keep USE xxxx_Interface or *_USE_IFs lines
                     if (($line =~ /USE *[\_0-9a-zA-Z]+\_Interface/i) || ($line =~ /\_USE_IFs/i))
                     {
                        $keep_blank_line = "n";
                        goto NL2; 
                     }
                     else                                                      # Keep these USE interface lines
                     {
                        $keep_blank_line = "y";
                        $interface_line_to_keep = 'y';
                        print ($line);
                        while ($line =~ /\& *$/ )
                        {
                           $line = <INFILE>;
                           $line_count++;
                           $cont_line++;
                           print ($line);
                        }
                        $cont_line = 0;
                         goto NL2;
                     } 
                  }
                  elsif ($line =~ /IMPLICIT/i )
                  { 
                     $keep_blank_line = "y";
                     $interface_line_to_keep = 'y';
                     print ($line);
                     goto NL2;
                  }     
                  elsif ($line =~ /INTENT/i )
                  { 
                     $keep_blank_line = "y";
                     $interface_line_to_keep = 'y';
                     print ($line);
                     while ($line =~ /\& *$ | \& *\!/ )
                     {
                        $line = <INFILE>;
                        $line_count++;
                        $cont_line++;
                        print ($line);
                     }
                     $cont_line = 0;
                     goto NL2;
                  }
                  elsif ($line =~ /PARAMETER/i )
                  { 
                     $keep_blank_line = "y";
                     $interface_line_to_keep = 'y';
                     print ($line);
                     while ($line =~ /\& *$ | \& *\!/ )
                     {
                        $line = <INFILE>;
                        $line_count++;
                        $cont_line++;
                        print ($line);
                     }
                     $cont_line = 0;
                     goto NL2;
                  }
                  goto NL2;
               }
               elsif ($line =~ / */)                                           # These are blank lines 
               {
                  if ($keep_blank_line eq "y")                                 # Blank line to keep
                  {
                     $num_blank_lines++;
                     $keep_blank_line = 'n';
                     print ($line);
                     goto NL2;
                  }
                  else                                                         # Blank line to skip
                  {
                     goto NL2;
                  }
               }
            }
         }
      }
      else 
      {
         if ($line eq "")
         {
            print ("Have read ",$line_count," lines from file ",$filename," and haven't found procedure name","\n");
            exit 1;
         }
         else
         {
            goto NL
         }
      }
   }

NF:$filecount++;
}   

exit 0;
