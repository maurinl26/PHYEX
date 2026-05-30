cmake_minimum_required(VERSION 3.15)
find_package(ecbuild 3.11 REQUIRED HINTS ${CMAKE_SOURCE_DIR})
project(PHYEX VERSION 1.0 LANGUAGES C Fortran)

include(phyex_macros)

#-----------------------------------------------------------------------
# Options
#-----------------------------------------------------------------------

ecbuild_add_option(FEATURE DOUBLE_PRECISION
                   DESCRIPTION "Compile on double precision"
                   DEFAULT ON)

ecbuild_add_option(FEATURE SINGLE_PRECISION
                   DESCRIPTION "Compile on single precision"
                   DEFAULT OFF)

ecbuild_add_option(FEATURE PHYEX_BUILD_PROGS
                   DESCRIPTION "Build PHYEX program executables"
                   DEFAULT OFF)

ecbuild_add_option(FEATURE PHYEX_BUILD_ORACLES
                   DESCRIPTION "Build standalone golden-reference oracle drivers (tests/oracle)"
                   DEFAULT OFF)

if(HAVE_DOUBLE_PRECISION)
  list(APPEND precision_list dp) 
endif()

if(HAVE_SINGLE_PRECISION)
  list(APPEND precision_list sp)
endif()

#-----------------------------------------------------------------------
# Dependencies
#-----------------------------------------------------------------------

# Fetch fiat from GitHub
# Fetch fiat from GitHub
option(PHYEX_FETCH_FIAT "Fetch fiat from GitHub" ON)

if(PHYEX_FETCH_FIAT)
  include(FetchContent)
  FetchContent_Declare(
    fiat
    GIT_REPOSITORY https://github.com/ecmwf-ifs/fiat
    GIT_TAG        main
  )
  FetchContent_MakeAvailable(fiat)
else()
  find_package(fiat REQUIRED)
endif()

#-----------------------------------------------------------------------
# Source structure
#-----------------------------------------------------------------------

# Source selection.
#
# master's src/common holds the *canonical* PHYEX sources, which use the
# mnh_expand / MZM-DZM array-operator dialect and must be passed through
# pyfortool (tools/prep_code.sh) before they compile. The Linux CI does that
# transform on the fly (see .github/workflows). For environments without a
# working fxtran/pyfortool (e.g. macOS), PHYEX_USE_TRANSFORMED_SOURCES builds
# from phyex_transformed/ — a vendored, ready-to-compile snapshot of that
# transform. src/common stays the single source of truth either way.
option(PHYEX_USE_TRANSFORMED_SOURCES
       "Build from the vendored pyfortool-transformed tree (phyex_transformed/) instead of src/common"
       OFF)

if(PHYEX_USE_TRANSFORMED_SOURCES)
  set(PHYEX_SRC_ROOT ${CMAKE_CURRENT_SOURCE_DIR}/phyex_transformed)
  message(STATUS "PHYEX sources: vendored transformed tree (${PHYEX_SRC_ROOT})")
else()
  set(PHYEX_SRC_ROOT ${CMAKE_CURRENT_SOURCE_DIR}/src/common)
  message(STATUS "PHYEX sources: canonical src/common (requires pyfortool transform)")
endif()

file(GLOB PHYEX_SOURCES
     ${PHYEX_SRC_ROOT}/aux/*F90
     ${PHYEX_SRC_ROOT}/turb/*F90
     ${PHYEX_SRC_ROOT}/micro/*F90
     ${PHYEX_SRC_ROOT}/conv/*F90
)

# Source for the python binding
# Source for the python binding
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/src/common/pyphyex.F90)
  list(APPEND PHYEX_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/src/common/pyphyex.F90)
endif()

# Sources for the programs
# Sources for the programs
if(HAVE_PHYEX_BUILD_PROGS)
  # Entry points
  file(GLOB PHYEXPROGS_ENTRYPOINTS src/offline/progs/main_*.F90)

  # Source files
  file(GLOB PHYEXPROGS_SOURCES
       ${CMAKE_CURRENT_SOURCE_DIR}/src/offline/support/*90
       ${CMAKE_CURRENT_SOURCE_DIR}/src/offline/progs/*90
  ) 
  list(REMOVE_ITEM PHYEXPROGS_SOURCES ${PHYEXPROGS_ENTRYPOINTS})
  list(APPEND PHYEX_SOURCES ${PHYEXPROGS_SOURCES})
endif()

# Headers
set(GLOB PHYEX_HEADERS
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/common/phyex/turb>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/common/phyex/micro>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/common/phyex/conv>
)

# Include directories (.h / .func.h headers live next to the sources)
set(PHYEX_INCLUDEDIRS
    $<BUILD_INTERFACE:${PHYEX_SRC_ROOT}/aux>
    $<BUILD_INTERFACE:${PHYEX_SRC_ROOT}/micro>
    $<BUILD_INTERFACE:${PHYEX_SRC_ROOT}/turb>
    $<BUILD_INTERFACE:${PHYEX_SRC_ROOT}/conv>
)

#---------------
#  Loop on precisions
#---------------

foreach(PRECISION ${precision_list})
  #---------------
  #  Determine suitable compilation option to use for unqualified REALs
  #---------------

  if(${PRECISION} STREQUAL "dp")
    set(PHYEX_COMPILE_OPTIONS_REAL ${ECBUILD_Fortran_COMPILE_OPTIONS_REAL8})
  else()
    set(PHYEX_COMPILE_OPTIONS_REAL ${ECBUILD_Fortran_COMPILE_OPTIONS_REAL4})
  endif()
  
  #---------------
  #  Public library
  #---------------

  ecbuild_add_library(
    TARGET phyex_${PRECISION}
    SOURCES ${PHYEX_SOURCES}
    PUBLIC_LIBS
      fiat
      mpi_serial
      parkind_${PRECISION}
    PUBLIC_INCLUDES ${PHYEX_HEADERS}
    INSTALL_HEADERS ALL
    HEADER_DESTINATION include/${PRECISION}
    PUBLIC_INCLUDES ${PHYEX_INCLUDEDIRS}
  ) 

  # Compilation option to use for unqualified REALs
  target_compile_options(phyex_${PRECISION} PUBLIC
    $<$<COMPILE_LANGUAGE:Fortran>:${PHYEX_COMPILE_OPTIONS_REAL}>)

  # Install Library 
  install(
    TARGETS phyex_${PRECISION}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  )
  
  # Install Modules 
  fortran_modules_install(
    TARGET phyex_${PRECISION}
    MODULE_DIR ${CMAKE_CURRENT_BINARY_DIR}/modules_${PRECISION}
  	INSTALL_DIR ${CMAKE_INSTALL_PREFIX}/modules/${PRECISION}
  )
  
  #---------------
  #  Progs
  #---------------
  
  if(HAVE_PHYEX_BUILD_PROGS)
    # Build all executables
    foreach(EP ${PHYEXPROGS_ENTRYPOINTS})
      get_filename_component(NAME ${EP} NAME_WLE)
      ecbuild_add_executable(
        TARGET ${NAME}_${PRECISION}
        SOURCES ${EP}
        OBJECTS phyex_${PRECISION}
        INCLUDES ${CMAKE_CURRENT_BINARY_DIR}/modules_${PRECISION}
        LIBS
          fiat
          mpi_serial
          parkind_${PRECISION}
        OUTPUT_NAME ${NAME}_${PRECISION}.exe
      )
      if(DEFINED FPP_FLAGS_TESTPROGS)
        target_compile_definitions(${NAME}_${PRECISION} PUBLIC ${FPP_FLAGS_TESTPROGS})
      endif()

      # Compilation option to use for unqualified REALs
      target_compile_options(${NAME}_${PRECISION} PUBLIC
        $<$<COMPILE_LANGUAGE:Fortran>:${PHYEX_COMPILE_OPTIONS_REAL}>)
    endforeach()
  endif()

  #---------------
  #  Golden-reference oracles (independent drivers used to validate the bindings)
  #---------------
  if(HAVE_PHYEX_BUILD_ORACLES)
    file(GLOB ORACLE_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/tests/oracle/oracle_*.F90)
    foreach(OS ${ORACLE_SOURCES})
      get_filename_component(ONAME ${OS} NAME_WLE)
      ecbuild_add_executable(
        TARGET ${ONAME}_${PRECISION}
        SOURCES ${OS}
        OBJECTS phyex_${PRECISION}
        INCLUDES ${CMAKE_CURRENT_BINARY_DIR}/modules_${PRECISION}
        LIBS
          fiat
          mpi_serial
          parkind_${PRECISION}
        OUTPUT_NAME ${ONAME}_${PRECISION}.exe
      )
      target_compile_options(${ONAME}_${PRECISION} PUBLIC
        $<$<COMPILE_LANGUAGE:Fortran>:${PHYEX_COMPILE_OPTIONS_REAL}>)
    endforeach()
  endif()

endforeach()

#-----------------------------------------------------------------------
# Python Bindings (scikit-build-core support)
#-----------------------------------------------------------------------

# Check if building Python package
if(DEFINED SKBUILD)
  message(STATUS "Building Python package with scikit-build-core")

  # Find Python
  find_package(Python REQUIRED COMPONENTS Interpreter Development.Module)
  find_package(Python REQUIRED COMPONENTS NumPy)

  # GPU (OpenACC) bindings: build the CuPy/deviceptr wrapper instead of the
  # host wrapper. Requires an OpenACC-capable Fortran compiler (NVHPC nvfortran).
  # The !$acc directives in phyex_bridge.F90 are inert comments under gfortran,
  # so the same bridge source serves both the CPU and GPU builds.
  option(PHYEX_ENABLE_GPU "Build the OpenACC GPU Cython wrapper (needs nvfortran)" OFF)
  # NVHPC -gpu target arch. ccXX of the test GPU: cc89=RTX 4090, cc86=RTX 3090/A40.
  set(PHYEX_GPU_ARCH "cc89" CACHE STRING "NVHPC -gpu compute capability (e.g. cc89, cc86)")

  if(PHYEX_ENABLE_GPU)
    set(CYTHON_SOURCE "${CMAKE_CURRENT_SOURCE_DIR}/cmake/bridge/_phyex_wrapper_acc.pyx")
    message(STATUS "PHYEX Python bindings: GPU/OpenACC wrapper selected")
  else()
    set(CYTHON_SOURCE "${CMAKE_CURRENT_SOURCE_DIR}/cmake/bridge/_phyex_wrapper.pyx")
    message(STATUS "PHYEX Python bindings: CPU wrapper selected")
  endif()
  set(CYTHON_OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/_phyex_wrapper.c")
  set(BRIDGE_FORTRAN_SOURCE "${CMAKE_CURRENT_SOURCE_DIR}/cmake/bridge/phyex_bridge.F90")

  # Check if Cython is available
  execute_process(
    COMMAND ${Python_EXECUTABLE} -m cython --version
    RESULT_VARIABLE CYTHON_CHECK
    OUTPUT_QUIET
    ERROR_QUIET
  )

  if(CYTHON_CHECK EQUAL 0)
    message(STATUS "Cython found")

    # Generate C code from Cython
    add_custom_command(
      OUTPUT ${CYTHON_OUTPUT}
      COMMAND ${Python_EXECUTABLE} -m cython ${CYTHON_SOURCE} -o ${CYTHON_OUTPUT}
      DEPENDS ${CYTHON_SOURCE}
      COMMENT "Generating C code from Cython"
    )

    # Create Python extension module
    Python_add_library(_phyex_wrapper MODULE ${CYTHON_OUTPUT} ${BRIDGE_FORTRAN_SOURCE})

    # Link against the PHYEX library (single precision for Python bindings).
    # The bridge selects its C-interop working precision (WP) from the
    # SINGLE_PRECISION macro, so it MUST be defined to match phyex_sp — otherwise
    # the bridge passes REAL(8) arrays to the REAL(4) physics routines.
    if(HAVE_SINGLE_PRECISION)
      target_link_libraries(_phyex_wrapper PRIVATE phyex_sp)
      target_compile_definitions(_phyex_wrapper PRIVATE SINGLE_PRECISION)
      target_compile_options(_phyex_wrapper PRIVATE
        $<$<COMPILE_LANGUAGE:Fortran>:${ECBUILD_Fortran_COMPILE_OPTIONS_REAL4}>)
      target_include_directories(_phyex_wrapper PRIVATE
        ${CMAKE_CURRENT_BINARY_DIR}/modules_sp
        ${Python_NumPy_INCLUDE_DIRS}
      )
    else()
      # Fallback to double precision if single is not available
      target_link_libraries(_phyex_wrapper PRIVATE phyex_dp)
      target_compile_options(_phyex_wrapper PRIVATE
        $<$<COMPILE_LANGUAGE:Fortran>:${ECBUILD_Fortran_COMPILE_OPTIONS_REAL8}>)
      target_include_directories(_phyex_wrapper PRIVATE
        ${CMAKE_CURRENT_BINARY_DIR}/modules_dp
        ${Python_NumPy_INCLUDE_DIRS}
      )
    endif()

    # OpenACC offload flags for the bridge + linked physics objects (NVHPC).
    # CACC_GPU lets the Fortran bridge guard host-side acc_map_data if needed.
    if(PHYEX_ENABLE_GPU)
      set(PHYEX_ACC_FLAGS "-acc" "-gpu=${PHYEX_GPU_ARCH}" "-Minfo=accel")
      target_compile_options(_phyex_wrapper PRIVATE
        $<$<COMPILE_LANGUAGE:Fortran>:${PHYEX_ACC_FLAGS}>)
      target_link_options(_phyex_wrapper PRIVATE ${PHYEX_ACC_FLAGS})
      target_compile_definitions(_phyex_wrapper PRIVATE PHYEX_GPU)
    endif()

    # Set module properties
    set_target_properties(_phyex_wrapper PROPERTIES
      PREFIX ""
      OUTPUT_NAME "_phyex_wrapper"
      SUFFIX ".${Python_SOABI}${CMAKE_SHARED_MODULE_SUFFIX}"
    )

    # Install Python extension module
    install(TARGETS _phyex_wrapper
      LIBRARY DESTINATION ${SKBUILD_PLATLIB_DIR}/phyex
      RUNTIME DESTINATION ${SKBUILD_PLATLIB_DIR}/phyex
    )

    message(STATUS "Python bindings will be built and installed to ${SKBUILD_PLATLIB_DIR}/phyex")
  else()
    message(WARNING "Cython not found, skipping Python bindings")
  endif()
endif()

#-----------------------------------------------------------------------
# Final packaging
#-----------------------------------------------------------------------

ecbuild_install_project(NAME PHYEX)
ecbuild_print_summary()
