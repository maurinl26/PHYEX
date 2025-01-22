cmake_minimum_required(VERSION 3.22)

project(phyex
    LANGUAGES
    Fortran
    C
)

set(CMAKE_Fortran_FLAGS "-ffree-line-length-512 -fconvert=swap")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fbounds-check -finit-real=snan -ffpe-trap=invalid,zero,overflow")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-real-8 -fdefault-double-8")

# WITHOUT_CXXDEMANGLE
add_compile_definitions(LINUX ON)
add_compile_definitions(LITTLE_ENDIAN ON)
add_compile_definitions(LITTLE ON)
add_compile_definitions(WITHOUT_CXXDEMANGLE ON)

# Custom definitions
add_compile_definitions(USE_OPENMP ON)
add_compile_definitions(USE_COLLCAL OFF)
# add_compile_definitions(HAVE_SERIALBOX ON)
set(HAVE_SERIALBOX ON)

# TODO : debug YDSTACK use
# add_compile_definitions(USE_STACK OFF)

# TODO : debug OPENACC use
# add_compile_definitions(_OPENACC OFF)



add_subdirectory(src)

# Find Serialbox

find_package(Serialbox)
