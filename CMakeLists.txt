cmake_minimum_required(VERSION 3.22)

project(phyex
    LANGUAGES
    Fortran
)
set(CMAKE_Fortran_FLAGS "-ffree-line-length-512")


add_subdirectory(src)


