rem Creates Fortran interface modules for source files in %1 and directs the output to file %2
rem It is CALL'ed in the batch files that also compile the interface modules
 
\perl\bin\perl \MYSTRAN\perl_scripts\Interface_Modules\Interface_Modules.perl %1 > %2
