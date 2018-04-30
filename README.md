# scheme-to-x86-assembly-compiler
A chez scheme to x86 assembly compiler, written on ubunto as the final project on Compiler Principles taken in BGU, 
as part of the Bsc degree. 

Using the compiler :
Assume you have a chez scheme file, foo.scm, that you want to compile, that is located in the project directory,
simply open a terminal, navigate to project directory, and type : "make foo",
this will create an executable foo in the same directory, and also a foo.s which is the assembly file.

foo.scm will go through a Reader, Tag-Parser, optimization for tail call application and redundent applications, and finaly, the compile-scheme-file that is located at project.scm.
