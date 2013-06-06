I'm trying to implement Scheme compiler in compliance with R7RS-Small.

And also Normal will include 'Prolog' in future.

To install Normal-Scheme you need a MinGW gcc compiler.

Normal-Scheme can run only on Window7 or later.

To compile just enter "make" 

at command prompt, enter "norm" then REPL will start.

<files>
main.c cell.c list.c compute.c function.c r7rs.c are for C.

ncomp.asm is compiler of normal. ncomp.o is compiled S expression file.

initlib.scm is initial program for R7RS library and procedures.
initlib.o is compiled S expression file.

Norm.exe reads ncomp.o and initlib.o when started.

 

 
