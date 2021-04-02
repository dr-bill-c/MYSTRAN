MYSTRAN
=======

MYSTRAN is an acronym for “My Structural Analysis” (https://mystran.com)


---

[Introduction](#introduction) |
[Features](#features) |
[Compiling Instructions](#compiling-instructions) |
[Developmental Goals](#developmental-goals) |
[Ways You Can Help](#ways-you-can-help) |
[Community](#community)

---


# Introduction

MYSTRAN is a general purpose finite element analysis computer program for structures that can be modeled as linear (i.e. displacements, forces and stresses proportional to applied load). MYSTRAN is an acronym for “My Structural Analysis”, to indicate it’s usefulness in solving a wide variety of finite element analysis problems.

For anyone familiar with the popular NASTRAN computer program developed by NASA (National Aeronautics and Space Administration) in the 1970’s and popularized in several commercial versions since, the input to MYSTRAN will look quite familiar. Many structural analyses modeled for execution in NASTRAN will execute in MYSTRAN with little, or no, modification. MYSTRAN, however, is not NASTRAN. It is an independent program written in modern Fortran 95.

# Features

- NASTRAN compatibility
- Modal analysis
- Linear Static Analysis
- Support for True Classical Laminate Plate Theory
- All of our documentation can be found in MYSTRAN forums

# Compiling Instructions

For Windows

    * A binary can be downloaded from this link: https://www.mystran.com/forums/showthread.php?tid=39
    * Compilation Instructions can be downloaded from this link: https://www.mystran.com/forums/showthread.php?tid=2

For Linux

    * Compilation Instructions can be downloaded from this link: https://www.mystran.com/forums/showthread.php?tid=2

For the Install and Use Manual, the User Manual, and Test Runs, see this link:
https://www.mystran.com/forums/showthread.php?tid=39

# Developmental Goals

- Add installation instructions for multiple platforms to the readme.
- The differential stiffness matrix for beam elements is coded in MYSTRAN. Therefore a buckling analysis (SOL 105) can be performed for beam elements. However, the differential stiffness matrix needs to be coded for the shell and solid elements. Also, it would be desirable to add the differential stiffness matrix for the shear and rod elements (lower priority).
- The default QUAD element is internally composed of 4 tri elements. These internal tri elements need a coordinate transformation to allow for orthotropic/anisotropic materials. Currently, the default QUAD element is only valid for isotropic materials (though the alternate QUAD element does support anisotropic materials).
- OP2 support is in work, but if you can help with this, please let us know.
- As a longer term goal, geometric nonlinear support is desirable.

# Ways You Can Help

- Join the MYSTRAN forum and/or Discord Channel below

# Community
- [Join our Forums](https://mystran.com/forums)
- [Join our Discord Channel on the Elmer Discord Server](https://discord.com/invite/fUJr75H)

