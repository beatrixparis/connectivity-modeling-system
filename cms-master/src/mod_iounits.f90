!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : mod_ioinits.f90                                                   *
!* Last Modified: 2011-07-22                                                *
!* Code contributors: Judith Helgers, Ashwanth Srinivasan, Claire B. Paris  * 
!*                                                                          *
!* Copyright (C) 2011, University of Miami                                  *
!*                                                                          *
!* This program is free software: you can redistribute it and/or modify     *
!* it under the terms of the GNU Lesser General Public License as published *
!* by the Free Software Foundation, either version 3 of the License, or     *
!*(at your option) any later version.                                       *
!*                                                                          *
!* This program is distributed in the hope that it will be useful,          *
!* but WITHOUT ANY WARRANTY; without even the implied warranty of           *
!* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
!* See the Lesser GNU General Public License for more details.              *
!*                                                                          *
!* You should have received a copy of the GNU Lesser General                *
!* Public License along with this program.                                  *
!* If not, see <http://www.gnu.org/licenses/>.                              *
!****************************************************************************


MODULE mod_iounits

 USE mod_kinds 

 IMPLICIT NONE

 logical (kind=log_kind), dimension(99), save :: &
     unit_free = .true.   ! flags to determine whether unit is free for use
                          ! maximum of 99 files can be opened at the same time

 CONTAINS

!**************************************************************
!This routine returns the next available I/O unit number.
SUBROUTINE get_unit(iunit)

 integer (kind=int_kind), intent(out) :: iunit   ! next free I/O unit
 integer (kind=int_kind)              :: n

!search for next available unit
 srch_unit: DO n=12,99
  IF (unit_free(n)) THEN
    iunit = n
    unit_free(n) = .false.
    exit srch_unit
  ENDIF
 END DO srch_unit

END SUBROUTINE get_unit

!**************************************************************
!This routine releases the specified unit and closes the file.
SUBROUTINE release_unit(iunit)

 integer (kind=int_kind), intent(in) :: iunit   ! I/O unit to release


!closes I/O unit and declares it free
 unit_free(iunit) = .true.
 close(iunit)

END SUBROUTINE release_unit
!**************************************************************

END MODULE mod_iounits


