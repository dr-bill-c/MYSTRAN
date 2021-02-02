# CMakeLists.txt for compiling MYSTRAN with gfortran in a GNU environment
# based on an older CMakeLists.txt by ceanwang@gmail.com
# made to work again by Bruno Borges Paschoalinoto (2020)

# set up basic project info
cmake_minimum_required(VERSION 3.18)
enable_language(Fortran)
project(Mystran)

# basic compiler and output options
set(CMAKE_SOURCE_DIR "Source")
set(PROJECT_BINARY_DIR "Binaries")
set(CMAKE_FLAGS "-c -g -fbacktrace")
set(CMAKE_BUILD_TYPE Debug)

# set some dirs
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib")
set(CMAKE_Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/mod")
include_directories("${CMAKE_SOURCE_DIR}/INCLUDE")
# uncomment this to debug
# set(CMAKE_VERBOSE_MAKEFILE true)

# SuperLU stuff
set(SUPERLU_VERSION "5.2.2")
set(SUPERLU_DIR "superlu-${SUPERLU_VERSION}")
SET(SUPERLU_FN "superlu_${SUPERLU_VERSION}.tar.gz")
set(SUPERLU_URL "https://www.mystran.com/SuperLU/${SUPERLU_FN}")
include_directories("${SUPERLU_DIR}/SRC")

# download the superlu source if needed
if(NOT EXISTS ${SUPERLU_FN})
  message(
    STATUS
    "Downloading SuperLU ${SUPERLU_VERSION} source from ${SUPERLU_URL}..."
  )
  file(DOWNLOAD ${SUPERLU_URL} ${SUPERLU_FN} TIMEOUT 60)
  message(STATUS "Done downloading.")
endif()

# extract superlu source
file(ARCHIVE_EXTRACT INPUT ${SUPERLU_FN})


# set some extra vars for MSYS builds to make the binary portable
if (WIN32)
  set(CMAKE_EXE_LINKER_FLAGS "-static")
endif()

# build the SuperLU and BLAS static libs!
add_subdirectory(${SUPERLU_DIR})

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

# collect modules
list(APPEND modules_names ARPACK BANDIT LK1 LK2 LK3 LK4 LK5 LK6 LK9 EMG)
foreach (modname IN LISTS modules_names)
  file(GLOB_RECURSE TMP_MOD_FILES "${CMAKE_SOURCE_DIR}/${modname}/*.f*")
  add_library(${modname} ${TMP_MOD_FILES} ${TMP_MOD_FILES_PP})
  target_link_libraries(${modname} MODULES)
endforeach()

# look for the F2C library, which is required.
find_library(f2c "f2c")

# add the DTRSV and DGSSV C modules.
add_library(dgssv "${SUPERLU_DIR}/FORTRAN/c_fortran_dgssv.c")
#add_library(dtrsv "${SUPERLU_DIR}/SRC/dtrsv.c")
target_link_libraries(dgssv f2c blas)
#target_link_libraries(dtrsv f2c blas)


# prepare the main executable, linked against the specifics and the m
# it appears utils used to be a module, but that is no longer the case?
file(GLOB_RECURSE UTIL_FILES "${CMAKE_SOURCE_DIR}/UTIL/*.f*")
file(GLOB_RECURSE MAIN_FILES "${CMAKE_SOURCE_DIR}/MAIN/*.[fF]*")
add_executable(mystran ${MAIN_FILES} ${MODULES_ALL_FILES} ${UTIL_FILES})
target_link_libraries(mystran ${modules_names})
target_link_libraries(mystran dgssv superlu f2c)
set_target_properties(
  mystran PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}
)

# issue a couple messages for unix users
include(ProcessorCount)
ProcessorCount(NCPU)
if (UNIX)
  message(STATUS "You can now compile MYSTRAN by running \"make\".")
  if (NOT NCPU EQUAL 0)
    message(STATUS "Compile faster by running \"make -j ${NCPU}\".")
  endif()
endif()
if (WIN32)
  message(STATUS "You can now compile MYSTRAN by running \"mingw32-make.exe\".")
  if (NOT NPROC EQUAL 0)
    message(STATUS "Compile faster by running \"mingw32-make.exe -j ${NCPU}\".")
  endif()
endif()
