cmake_minimum_required(VERSION 3.22)

project(phyex
    LANGUAGES
    Fortran
)

set(CMAKE_Fortran_FLAGS "-ffree-line-length-512 -fconvert=swap")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fbounds-check -finit-real=snan -ffpe-trap=invalid,zero,overflow")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-real-8 -fdefault-double-8")

# WITHOUT_CXXDEMANGLE
add_compile_definitions(LINUX)
add_compile_definitions(LITTLE_ENDIAN)
add_compile_definitions(LITTLE)
add_compile_definitions(WITHOUT_CXXDEMANGLE)



add_subdirectory(src)


