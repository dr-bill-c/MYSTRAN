# MYSTRAN

MYSTRAN is a general purpose finite element analysis computer program for structures that can be modeled as linear (i.e. displacements, forces and stresses proportional to applied load).

MYSTRAN is an acronym for “My Structural Analysis”, to indicate it’s usefulness in solving a wide variety of finite element analysis problems.

For anyone familiar with the popular NASTRAN computer program developed by NASA (National Aeronautics and Space Administration) in the 1970’s and popularized in several commercial versions since, the input to MYSTRAN will look quite familiar.

Many structural analyses modeled for execution in NASTRAN will execute in MYSTRAN with little, or no, modification. MYSTRAN, however, is not NASTRAN.

---
It is an independent program written in modern Fortran 95.

MYSTRAN Homepage: <a href ="http://www.mystran.com">http://www.mystran.com</a>

Forums: [http://www.mystran.com/forums]()

Documentation, latest Windows executable, Test Runs, and other files are located on the MYSTRAN forum


## Compiling

Requirements:
- A minimal GNU environment and binutils (make, ar, ld)
  - Linux users should already have one
  - Windows users can get one via MinGW
- gfortran
  - Linux users can get it from their distribution's official repositories
  - Windows users can get it with MinGW via their update manager
- CMake 3.1 or later (we recommend latest)
- A copy of this repository (use git or GitHub's "Download ZIP" feature)

Steps for Linux:
1) Move a shell/cmd.exe instance to the root folder of this repository.
2) Run `cmake .` to generate the `Makefile`.
3) Run `make`. If you have an N-core processor, you can run `make -j N`.
For instance, in a quad-core processor, `make -j 4` is much, _much_ faster than just `make`.
4) Wait. When `make` is done, there should be a "Binaries" folder, and a `mystran` binary inside.

Steps for Windows: FIXME
