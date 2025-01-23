! (C) Copyright 1988- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

module serialize_mod
   USE PARKIND1, ONLY: JPIM, JPRB

! #ifdef HAVE_SERIALBOX
   USE m_serialize, ONLY: &
      fs_create_savepoint, &
      fs_add_serializer_metainfo, &
      fs_get_serializer_metainfo, &
      fs_read_field, &
      fs_write_field
   USE utils_ppser, ONLY: &
      ppser_initialize, &
      ppser_finalize, &
      ppser_serializer, &
      ppser_serializer_ref, &
      ppser_set_mode, &
      ppser_savepoint
! #endif

   implicit none

contains

   subroutine query_dimensions(KLON, KLEV, KFLDX, NAME)
      ! Initial query routine to determine data dimensions
      INTEGER(KIND=JPIM), INTENT(OUT) :: KLON             ! Number of grid points
      INTEGER(KIND=JPIM), INTENT(OUT) :: KLEV             ! Number of levels
      ! INTEGER(KIND=JPIM),INTENT(OUT) :: NCLV
      INTEGER(KIND=JPIM), INTENT(OUT) :: KFLDX
      CHARACTER(*), INTENT(IN) :: NAME

! #ifdef HAVE_SERIALBOX
      ! Get dimensions information from stored metadata
      call ppser_initialize(directory='data', prefix='dummy', prefix_ref=NAME)
      call fs_create_savepoint('input', ppser_savepoint)
      call ppser_set_mode(0)

      call fs_get_serializer_metainfo(ppser_serializer_ref, 'KLON', KLON)
      call fs_get_serializer_metainfo(ppser_serializer_ref, 'KLEV', KLEV)
      ! call fs_get_serializer_metainfo(ppser_serializer_ref, 'NCLV', NCLV)
      call fs_get_serializer_metainfo(ppser_serializer_ref, 'KFLDX', KFLDX)
! #endif
   end subroutine query_dimensions

   subroutine serialize_ice_adjust( &
     & KLON, KLEV, KBL, &
     & PEXNREF &
   )

      integer, intent(in) :: KLON, KLEV
      integer, intent(in) :: KBL ! KBL is for KBLOCS

      real(kind=JPRB), dimension(KLON, KLEV, KBL), intent(in) :: PEXNREF

! #ifdef HAVE_SERIALBOX
      print *, "Serializing input data to Serialbox format"

      ! Initialize serializer for storing reference input
      call ppser_initialize(directory='./data', prefix='input')

      call fs_create_savepoint('input', ppser_savepoint)

      ! Mode write
      call ppser_set_mode(0)

      ! Store dimensions and timestep size on the serializer
      call fs_add_serializer_metainfo(ppser_serializer, 'KLON', KLON)
      call fs_add_serializer_metainfo(ppser_serializer, 'KLEV', KLEV)
      call fs_add_serializer_metainfo(ppser_serializer, 'NGPBLKS', KBL)

      ! Store arrays

      call fs_write_field(ppser_serializer, ppser_savepoint, TRIM('PEXNREF'), PEXNREF)

      call ppser_finalize

! #endif HAVE_SERIALBOX
   end subroutine serialize_ice_adjust

end module serialize_mod
