# Check.perl
# -----------------------------

# Copyright (c) 2003 Bill Case.

# Change log:

# **********************************************************************************************************************************
$program_name = $0;
$num_args     = 0;
$num_files    = @ARGV - $num_args;

$file_count = 1;
do
{
   $filename[$file_count] = $ARGV[$file_count+$num_args-1];
   $file_count++;
}  until ($file_count > $num_files);                       # Need $file_count > $num_files since $file_count is incremented after last file read

$found_files_w_no_USE_IFs = "no";
$file_count = 1;
while ($file_count <=$num_files)
{
   unless (open(INFILE,$filename[$file_count]))            # Open one of the files
   {
      if (-e $filename[$file_count])
      {
         die ("File $filename[$file_count] exists, but cannot be opened.\n");
      }
      else
      {
         die ("File $filename[$file_count] does NOT exist.\n");
      }
   }

   $num_subrs_called = 0;                                  # Following code gets all CALL statements in $filename (but ignores
   unless (open(INFILE,$filename[$file_count]))            # repetitive names and ones in $CONTAINd_subr_names, above)
   {
      if (-e $filename[$file_count])
      {
         die ("File $filename[$file_count] exists, but cannot be opened.\n");
      }
      else
      {
         die ("File $filename[$file_count] does not exist.\n");
      }
   }

NL:$line = <INFILE>;
   $found_USE_IFs = "no";
   if ($line ne "")
   {
      chop ($line);
      if ($line =~ /^ *PROGRAM|^ *SUBROUTINE|^ *MODULE/i)
      {
         @words = split (/[\( ]+/,$line);
         $procedure_type = $words[1];
         $subr_name = $words[2];
         $USE_statement = "USE " . $subr_name . "_USE_IFs";

NL2:     $line = <INFILE>;
         if ($line ne "")
         {
            chop ($line);
                                                           # Skip any lines that have the following text                                                           # Initially, take all lines that have CALL (any case)
            if ($line =~ / *$USE_statement/i)
            {
               $found_USE_IFs = "yes";
            }
            else
            {
               goto NL2;
            }
         }
      }
      else 
      {
         goto NL
      }
   }

   if ($found_USE_IFs eq "no")
   {
      $found_files_w_no_USE_IFs = "yes";
      printf("%-2s %-32s %29s %-256s", "No",$USE_statement,"statement found in file",$filename[$file_count]);
      print("\n");
   }

NF:$file_count++;

}
   
if ($found_files_w_no_USE_IFs eq "no")
{
   print("All files checked and none are missing the USE_IFs statement","\n");
}

exit 0;
