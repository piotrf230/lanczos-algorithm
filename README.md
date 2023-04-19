# Lanczos algorithm Fortran implementation
The goal of this project is to practice programming in Fortran using modules and external libraries (Lapack).  
The program calculates a value using Lanczos algorithm and compares it woth Laspack's DSYEV procedure output.  
The program always needs an input file with the name "Matrix.dat" and outputs a file "LanczosOut.dat", alongside with console prints. Sample inputs and outputs can be found in "Samples" directory in repository.  
The main part of th projects are modules (located in "Modules" directory), containing data structures and subroutines. Their functions are following:
- VectorOps - contains subroutines for vector operations.
- Matrix - Contains subroutines for operations on matrices: multiplying by vectors, saving and loading from files.
- Lanczos - Contains subroutine for Lanczos algorithm