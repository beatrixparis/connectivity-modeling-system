!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : mod_netcdf.f90                                                    *
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

MODULE mod_netcdf

USE mod_kinds
USE netcdf

IMPLICIT NONE

CONTAINS
	
SUBROUTINE ncheck(istat, fname)
!This routine provides a simple interface to netCDF error message routine.
 integer(kind=int_kind), intent(in) :: istat
 character(*), intent(in)           :: fname
 
 IF (istat /= nf90_noerr) THEN
  print *, 'Error in the netCDF file: ', adjustl(trim(fname))
  print *, trim(nf90_strerror(istat))
  stop
 ENDIF
END SUBROUTINE ncheck

END MODULE mod_netcdf


