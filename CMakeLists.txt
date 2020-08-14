# CMakeLists.txt for compiling MYSTRAN with gfortran in a GNU environment
# based on an older CMakeLists.txt by ceanwang@gmail.com
# made to work again by Bruno Borges Paschoalinoto (2020)

# set up basic project info
cmake_minimum_required(VERSION 3.1)
enable_language(Fortran)
project(Mystran)

# basic compiler and output options
set(CMAKE_SOURCE_DIR "Source")
set(PROJECT_BINARY_DIR "Binaries")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib")
set(CMAKE_Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/mod")
set(CMAKE_FLAGS "-c -info -chk -pca -stchk -trace -ap -g -nco -nfix -in -O2")
include_directories("${CMAKE_SOURCE_DIR}/INCLUDE")
# uncomment this to debug
# set(CMAKE_VERBOSE_MAKEFILE true)

# set some extra vars for MSYS builds to make the binary portable
if (WIN32)
  set(CMAKE_EXE_LINKER_FLAGS "-static")
endif()

# collect modules and interfaces into a module called MODULES
# the overuse of the word MODULE is brain-twisting, but bear with me
set(Modules_DIR "${CMAKE_SOURCE_DIR}/Modules")
file(GLOB_RECURSE Modules_ARPACK_FILES "${Modules_DIR}/ARPACK/*.f*")
file(GLOB_RECURSE Modules_LAPACK_FILES "${Modules_DIR}/LAPACK/*.f*")
file(GLOB_RECURSE Modules_BANDIT_FILES "${Modules_DIR}/BANDIT/*.f*")
file(GLOB_RECURSE Modules_FILES "${Modules_DIR}/*.f*")
file(GLOB_RECURSE Interfaces_FILES "${CMAKE_SOURCE_DIR}/Interfaces/*.f*")
file(GLOB_RECURSE USE_IFs_FILES "${CMAKE_SOURCE_DIR}/USE_IFs/*.f*")
# MODULES_ALL_FILES shall contain all the sources for the base modules lib
set(
  MODULES_ALL_FILES ${USE_IFs_FILES} ${Interfaces_FILES} ${Modules_FILES}
  ${Modules_LAPACK_FILES} ${Modules_ARPACK_FILES}
)
add_library(MODULES ${MODULES_ALL_FILES})

# choose OS-specific files, put them in a "specifics" module
set(SPECIFICS_DIR "${CMAKE_SOURCE_DIR}/Specifics")
if (WIN32)
  file(GLOB_RECURSE SPECIFICS_FILES "${SPECIFICS_DIR}/Windows/*.f*")
endif()
if (UNIX)
  file(GLOB_RECURSE SPECIFICS_FILES "${SPECIFICS_DIR}/Linux/*.f*")
endif()
if (NOT WIN32 AND NOT UNIX)
  message(FATAL_ERROR, "Environment must be Win32 or Unix-like.")
endif()
add_library(SPECIFICS ${SPECIFICS_FILES})
target_link_libraries(SPECIFICS MODULES)

# collect other modules
list(APPEND modules_names ARPACK BANDIT LK1 LK2 LK3 LK4 LK5 LK6 LK9 EMG)
foreach (modname IN LISTS modules_names)
  file(GLOB_RECURSE TMP_MOD_FILES "${CMAKE_SOURCE_DIR}/${modname}/*.f*")
  add_library(${modname} ${TMP_MOD_FILES})
  target_link_libraries(${modname} MODULES)
endforeach()

# prepare the main executable, linked against the specifics and the m
# it appears utils used to be a module, but that is no longer the case?
file(GLOB_RECURSE UTIL_FILES "${CMAKE_SOURCE_DIR}/UTIL/*.f*")
file(GLOB_RECURSE MAIN_FILES "${CMAKE_SOURCE_DIR}/MAIN/*.f*")
add_executable(mystran ${MAIN_FILES} ${MODULES_ALL_FILES} ${UTIL_FILES})
target_link_libraries(mystran SPECIFICS ${modules_names})
set_target_properties(
  mystran PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}
)

# issue a couple messages for unix users
if (UNIX)
  include(ProcessorCount)
  ProcessorCount(NPROC)
  message(STATUS "You can now compile MYSTRAN by running \"make\".")
  if (NOT NPROC EQUAL 0)
    message(STATUS "Compile faster by running \"make -j ${NPROC}\".")
  endif()
endif()
if (WIN32)
  message(STATUS "FIXME WIN32 STATUS")
endif()
