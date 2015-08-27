!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : diffpart.f90                                                      *
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

MODULE mod_diffpart

 USE mod_kinds
 USE constants
 USE mod_iounits

 IMPLICIT NONE

!module variables
 real (kind = real_kind),allocatable, save :: &
     catPerc(:),catMinSize(:),catMaxSize(:), &
     catminDens(:), catmaxDens(:), catHalfLife(:), cumPerc(:)     

 integer (kind=int_kind), save :: numCat     

 CONTAINS     

!**************************************************************
!read the file with the different categories
SUBROUTINE load_diffpart_data(diffpartFileName)

 character(len = *), intent(in) :: diffpartFileName
 logical (kind=log_kind)  :: file_exists
 integer (kind=int_kind)  :: i,iunit, ios = 0
     
!open file
 CALL get_unit(iunit)
 INQUIRE(FILE=trim(diffpartFileName), EXIST=file_exists)
 IF (file_exists) then
  OPEN(UNIT=iunit,FILE=trim(diffpartFileName), STATUS='old')
 ELSE
  print *, "Error: File ", trim(diffpartFileName)," does not exist"
  stop
 ENDIF

!read data
 read(iunit,*,iostat=ios) numCat
 IF (ios .ne. 0) then
  print *, "Error in reading diffpart_matrix"
  stop
 ENDIF
 allocate(catPerc(numCat))
 read(iunit,*,iostat=ios) (catPerc(i), i=1, numCat)
 IF (ios .ne. 0) then
  print *, "Error in reading diffpart_matrix"
  stop
 ENDIF
 allocate(catminDens(numCat))
 read(iunit,*,iostat=ios) (catminDens(i), i=1, numCat)
 IF (ios .ne. 0) then
  print *, "Error in reading diffpart_matrix"
  stop
 ENDIF
 allocate(catmaxDens(numCat))
 read(iunit,*,iostat=ios) (catmaxDens(i), i=1, numCat)
 IF (ios .ne. 0) then
  print *, "Error in reading diffpart_matrix"
  stop
 ENDIF
 allocate(catMinSize(numCat))
 read(iunit,*,iostat=ios) (catMinSize(i), i=1, numCat)
 IF (ios .ne. 0) then
  print *, "Error in reading diffpart_matrix"
  stop
 ENDIF
 allocate(catMaxSize(numCat))
 read(iunit,*,iostat=ios) (catMaxSize(i), i=1, numCat)
 IF (ios .ne. 0) then
  print *, "Error in reading diffpart_matrix"
  stop
 ENDIF
 allocate(catHalfLife(numCat))
 read(iunit,*,iostat=ios) (catHalfLIFe(i), i=1, numCat)
 IF (ios .ne. 0) then
  print *, "Error in reading diffpart_matrix"
  stop
 ENDIF

!close file
 CALL release_unit(iunit)

!calculate cumulative percentages
 allocate(cumPerc(numCat))
 cumPerc(1) = catPerc(1)
 DO i=2,numCat
  cumPerc(i) = cumPerc(i-1) + catPerc(i)
 ENDDO 

! print *, "Finished loading diffpart data" 
 
 END SUBROUTINE load_diffpart_data

!**************************************************************
!deallocate memory for diffpart variables
SUBROUTINE dealloc_diffpart
  deallocate(catPerc)
  deallocate(catminDens)
  deallocate(catmaxDens)
  deallocate(catMinSize)
  deallocate(catMaxSize)
  deallocate(catHalfLife)
  deallocate(cumPerc)
END SUBROUTINE dealloc_diffpart

!**************************************************************    

END MODULE mod_diffpart

