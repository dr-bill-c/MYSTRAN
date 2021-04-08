MYSTRAN
=======

MYSTRAN is an acronym for “My Structural Analysis” (https://mystran.com)


---

[Introduction](#introduction) |
[Features](#features) |
[Compiling Instructions](#compiling-instructions) |
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

<details>
<summary> Debian/Ubuntu (using apt)</summary>
<br>
MYSTRAN relies upon the following dependencies

- gcc
- gfortran
- make
- git
- f2c (aka libf2c)
- cmake (version 3.18 or above)

Check if gcc is installed. 
```
dpkg-query -l | grep -P "^ii\s+gcc\s+"
```
If it is not installed, installed it.
```
sudo apt install gcc
```

Check if gfortran is installed
```
dpkg-query -l | grep -P "^ii\s+gfortran\s+"
```
If it is not installed, install it.
```
sudo apt install gfortran
```

Check if make is installed
```
dpkg-query -l | grep -P "^ii\s+make\s+"
```
If it is not installed, install it.
```
sudo apt install make
```

Check if git is installed
```
dpkg-query -l | grep -P "^ii\s+git\s+"
```
If it is not installed, install it.
```
sudo apt install git
```

Check if f2c is installed
```
dpkg-query -l | grep -P "^ii\s+f2c\s+"
```
If it is not installed, install it.
```
sudo apt install f2c
```

Check if cmake is installed
```
dpkg-query -l | grep -P "^ii\s+cmake\s"
```
If it is not installed, or it is not version 3.18 or later, you can purge it,
and aquire the newest version from source.
```
sudo apt purge cmake
git clone "https://gitlab.kitware.com/cmake/cmake.git"
cd cmake
./bootstrap
make
sudo make install
```



</details>

    * Compilation Instructions can be downloaded from this link: https://www.mystran.com/forums/showthread.php?tid=2

For the Install and Use Manual, the User Manual, and Test Runs, see this link:
https://www.mystran.com/forums/showthread.php?tid=39



# Ways You Can Help

- Join the MYSTRAN forum and/or Discord Channel below

# Community
- [Join our Forums](https://mystran.com/forums)
- [Join our Discord Channel on the Elmer Discord Server](https://discord.com/invite/fUJr75H)

