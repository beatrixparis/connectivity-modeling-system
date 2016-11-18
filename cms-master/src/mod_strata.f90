!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : mod_ibio.f90                                                      *
!* Last Modified: 2016-04-01                                                *
!* Code contributors: Ana Carolina Vaz, Claire B Paris, Dan Holstein        * 
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

MODULE mod_strata

 USE mod_kinds
 USE constants
 USE mod_iounits

 IMPLICIT NONE

 real (kind = real_kind), allocatable, save  :: depthStrata(:) 
 integer (kind=int_kind), save               :: numStrata

 CONTAINS

!***************************************************************************
!load strata data
SUBROUTINE load_strata (StrataFilename)  
!On mod_ibio.f90
 
 character(len = *), intent(in) :: StrataFilename
 integer (kind=int_kind) :: i,j, iunit
 real (kind = real_kind) :: cum
 logical (kind=log_kind) :: file_exists

!open file
 call get_unit(iunit)
 INQUIRE(FILE=trim(StrataFilename), EXIST=file_exists)
 IF (file_exists) THEN
  OPEN(UNIT=iunit,FILE=trim(StrataFilename),STATUS="old")
 ELSE
  print *, "Error: File ", trim(StrataFilename)," does not exist"
  stop
 ENDIF

!read file
 read(iunit, *) numStrata
! numStrata is the number of layers to be used
! depthStrata(1,:) are the initial depth of the layers
! depthStrata(2,:) are the final depth of the layers

 allocate(depthStrata(numStrata))
 read(iunit,*) (depthStrata(j), j=1, numStrata) 

!close file
call release_unit(iunit)

!print *, "Finished loading vertical matrix" 
 END SUBROUTINE load_strata


!***************************************************************************
!adds initial strata to particle
SUBROUTINE assign_strata_start(startdepth,stratastart)

real (kind = real_kind), intent(in)    :: startdepth
integer (kind = int_kind), intent(out) :: stratastart
integer (kind = int_kind)		    :: y

!assign strata to particle

IF (startdepth .gt. depthStrata(numStrata)) THEN
  print *, "Depth in release file is greater than depths in strata matrix", startdepth
ELSE

!Assign a strata to a particle's depth
! loop over all strata
!checks what strata each particle is, by checking if the particle depth is between the initial and final depth of each strata, given on vertStrata
  strata_loop: DO y=1, numStrata
		
  IF (startdepth .le. depthStrata(y)) THEN
   stratastart= y
   exit strata_loop
 
  ENDIF
 ENDDO strata_loop
ENDIF

END SUBROUTINE assign_strata_start


!***************************************************************************
!checks the strata of the particle
SUBROUTINE check_strata(pdepth,strataFlag)

real (kind = real_kind), intent(in)    :: pdepth
integer (kind = int_kind), intent(out) :: strataFlag
integer (kind = int_kind)		    :: y
 
!assign strata to particle

IF (pdepth .le. depthStrata(numStrata)) THEN
!Assign a strata to a particle's depth
! loop over all strata
!checks what strata each particle is, by checking if the particle depth is between the initial and final depth of each strata, given on vertStrata
  strata_loop: DO y=1, numStrata
  IF (pdepth .le. depthStrata(y)) THEN
   strataFlag = y
   exit strata_loop
  ENDIF
 ENDDO strata_loop
 ELSE
   strataFlag=-1
ENDIF

END SUBROUTINE check_strata

!********************************************************************************
!adds initial strata to polygon
SUBROUTINE poly_strata(pDepth,pStrata)

real (kind = real_kind), intent(in)    :: pDepth
integer (kind = int_kind), intent(out) :: pStrata
integer (kind = int_kind)		    :: y

IF (pDepth .gt. depthStrata(numStrata)) THEN
   print *, "Depth in xyz file is greater than depths in strata matrix", pDepth
  ELSE

!    Assign a strata to a polygon's depth
   strata_loop: DO y=1, numStrata
    IF (pDepth .le. depthStrata(y)) THEN
     pStrata = y
     exit strata_loop
    ENDIF
   ENDDO strata_loop
  ENDIF

END SUBROUTINE poly_strata

!***************************************************************************
END module mod_strata
