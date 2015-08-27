!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : dealloc.f90                                                       *
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

!deallocate memory of all variables
SUBROUTINE dealloc_all

 USE globalvariables
 USE mod_reef
 USE mod_ibio
 USE mod_diffpart

 IF (ibio .or. massSpawning) THEN
  call dealloc_ibio(massSpawning)
 ENDIF  
 IF (polygon) THEN
  call dealloc_reef
 ENDIF
 IF (diffpart) THEN
  call dealloc_diffpart
 ENDIF

 deallocate(nests)
 deallocate(particle)   

END SUBROUTINE dealloc_all

