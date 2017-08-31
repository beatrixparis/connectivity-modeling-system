!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : mod_TempMort.f90                                                  *
!* Last Modified: 2017-Aug-31                                               *
!* Written by: Sally Wood, University of Bristol (sally.wood@bristol.ac.uk) * 
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

MODULE mod_TempMort

 USE mod_kinds

 IMPLICIT NONE

 CONTAINS

!**************************************************************

 SUBROUTINE define_halflife(temp, halflife)

 real (kind = real_kind), intent(in)    :: temp
 real (kind = real_kind), intent(out)   :: halflife

 integer (kind=int_kind)                :: i
 integer (kind=int_kind), dimension(16) :: temp_ranges
 real (kind=real_kind), dimension(15)   :: halflifes

!define temperature ranges
 temp_ranges = (/(i, i=8,38,2)/)

!define halflife for each temperature range
 halflifes = (/3283200, 3110400, 2937600, 2764800, 2592000, 2419200, 2246400, 2073600, 1900800, 1728000, 1555200, 1382400, 1209600, 1036800, 864000/)

!determine halflife for inputted temperature
 IF(temp > maxval(temp_ranges) .or. temp < minval(temp_ranges)) THEN
   print*, 'Warning: Temperature is outside defined range for mod_envmort:', minval(temp_ranges), ' to', maxval(temp_ranges) 
 ENDIF
   DO i = 1, 15 !loop over length of temp_ranges - 1
     IF(temp >= temp_ranges(i) .and. temp < temp_ranges(i+1)) THEN
        halflife = halflifes(i)
     ENDIF
   ENDDO

 END SUBROUTINE define_halflife

!**************************************************************

END MODULE mod_TempMort
