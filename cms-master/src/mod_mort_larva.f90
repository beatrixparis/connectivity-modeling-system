!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : mod_mort_larva.f90                                                *
!* Last Modified: 2011-07-22                                                *
!* Code contributors: Judith Helgers, Claire B. Paris                       * 
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

MODULE mod_mort

 USE mod_kinds
 USE constants
 USE mod_random

 IMPLICIT NONE

 CONTAINS


!**************************************************************

SUBROUTINE add_mort(h, halflife, mort)

 real (kind = real_kind), intent(in) :: h, halflife
 logical (kind=log_kind), intent(out) :: mort

 real (kind = real_kind) :: lambda,pd,rn

 lambda = 0.693/halflife
 pd=lambda*h

!Write (*,*) "Evaportative decay: ", lambda, "sec-1" 

 CALL random_real(0.,1.,rn) 
 IF (rn .lt. pd) THEN
   mort = .true.
 ELSE
   mort = .false.
 ENDIF
END SUBROUTINE add_mort

!**************************************************************


END MODULE mod_mort
