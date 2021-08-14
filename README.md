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

### For Windows

    * A binary can be downloaded from this link: https://www.mystran.com/forums/showthread.php?tid=39
    * Compilation Instructions can be downloaded from this link: https://www.mystran.com/forums/showthread.php?tid=2


### For Linux

<details>
<summary> Debian/Ubuntu (using apt)</summary>
<br>
MYSTRAN relies upon the following dependencies

- gcc
- g++ (may come with gcc)
- gfortran
- make
- git
- f2c (aka libf2c)
- cmake (version 3.18 or above)

Check if gcc is installed. 
```
dpkg --list | grep -P "^ii\s+gcc\s+"
```

Check if g++ is installed
```
dpkg --list | grep g++
```

If it is not installed, installed it.
```
sudo apt install gcc
```

Check if gfortran is installed
```
dpkg --list | grep -P "^ii\s+gfortran\s+"
```

If it is not installed, install it.
```
sudo apt install gfortran
```

Check if make is installed
```
dpkg --list | grep -P "^ii\s+make\s+"
```

If it is not installed, install it.
```
sudo apt install make
```

Check if git is installed
```
dpkg --list | grep -P "^ii\s+git\s+"
```

If it is not installed, install it.
```
sudo apt install git
```

Check if f2c is installed
```
dpkg --list | grep -P "^ii\s+f2c\s+"
```

If it is not installed, install it.
```
sudo apt install f2c
```

Check if cmake is installed

```
dpkg --list | grep -P "^ii\s+cmake\s+"
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

You now have all of the prerequisites to compile Mystran.

Aquire the Mystran source.
```
git clone https://github.com/dr-bill-c/MYSTRAN.git
cd MYSTRAN
```

Generate the build scripts.
```
cmake .
```

Compile with make.
```
make
```

Copy the mystran binary to the appropriate location
```
sudo cp MYSTRAN/Binaries/mystran /usr/local/bin/mystran
```

Delete the git repository 
```
cd ..
sudo rm -r MYSTRAN
```

Congragulations! You have just installed MYSTRAN.

</details>

# Developmental Goals

- Add detailed installation instructions for Windows to the README
- The differential stiffness matrix for beam elements, rod elements, and solid elements is coded in MYSTRAN. Therefore a buckling analysis (SOL 105) can be performed for beam elements, rod elements, and solid elements. However, the differential stiffness matrix needs to be coded for the shell elements.
- The default QUAD element is internally composed of 4 tri elements. These internal tri elements need a coordinate transformation to allow for orthotropic/anisotropic materials. Currently, the default QUAD element is only valid for isotropic materials (though the alternate QUAD element does support anisotropic materials).
- OP2 support is in work, but if you can help with this, please let us know.
- Creating easier ways to aquire mystran would be nice. This would include, but is not limited to, entry into the Arch Linux User Repository (AUR), the Debian Advanced Package Manager (apt), the snapcraft store (snap), the chocolatey package manager for Windows, an appimage, or flatpak.
- Creation of a more comprehensive testing suite would be nice. Current method uses VBA and Microsoft excel. New testing methods should ideally include correlation to other solvers, previous versions of mystran, industry standard demonstration problems, and continuum mechanics/direct solutions.
- As a longer term goal, materialistically nonlinear support is desirable.
- As a longer term goal, geometric nonlinear support is desirable.

# Ways You Can Help

- Join the MYSTRAN forum and/or Discord Channel below
- Contribute your MYSTRAN runs to the list of demonstration problems by posting on the forums, or in our discord channel below

# Community
- [Join our Forums](https://mystran.com/forums)
- [Join our Discord Channel on the Elmer Discord Server](https://discord.com/invite/fUJr75H)

