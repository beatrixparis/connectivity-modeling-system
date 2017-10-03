!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : mod_reef.f90                                                      *
!* Last Modified: 2016-04-01                                                *
!* Code contributors: Claire B. Paris, Ana Carolina Vaz, Judith Helgers,    * 
!*                    Ashwanth Srinivasan                                   * 
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

MODULE mod_reef

 USE mod_kinds
 USE constants
 USE globalvariables
 USE mod_calendar
 USE mod_iounits
 USE mod_strata


 IMPLICIT NONE

!module variables
 real (kind = real_kind),allocatable, save :: & 
     polyLons(:),polyLats(:), polyId(:), polyDepth(:)  !added polyDepth

 integer (kind=int_kind), allocatable, save :: &
     polyBgn(:),polyEnd(:), polyStrata(:) !Moved polyStrata to be an int. !start en endpoint of each polygon
     
 integer (kind=int_kind), save :: &
     npoly ! number of polygons

 CONTAINS     

!**************************************************************
!read the file with the polygons
 
SUBROUTINE load_reef_data (polyFileName)

 character(len = *), intent(in) :: polyFileName
 logical (kind=log_kind)  :: file_exists
 integer (kind=int_kind)  :: & 
      pdm, &    !size of file
      i,j, &    !loop variables
      sze,iunit
 
!get size of file
 CALL getSize(polyFileName,pdm)

!allocate memory for polygon lat,lons and id and read in values
 allocate(polyLons(pdm))
 allocate(polyLats(pdm))
 allocate(polyId(pdm))

!open file
 CALL get_unit(iunit)
 INQUIRE(FILE=trim(polyFileName), EXIST=file_exists)
 IF (file_exists) THEN
  open(UNIT=iunit,FILE=trim(polyFileName), STATUS='old')
 else
  print *, "Error: File ", trim(polyFileName) ," does not exist"
  stop
 ENDIF

!read file
 DO i=1,pdm
   read(iunit,*) polyLons(i),polyLats(i), polyId(i)
 ENDDO
 
!close file
 CALL release_unit(iunit)
 
!number of polygons in file 
 npoly=int(polyId(pdm))
! print *, npoly

!make sure all longitudes of the polygons are between 0 and 360
 DO i=1,pdm
   DO WHILE (polyLons(i) .lt. 0.) 
    polyLons(i) = polyLons(i) + 360.
   ENDDO
   DO WHILE (polyLons(i) .ge. 360.) 
    polyLons(i) = polyLons(i) - 360.
   ENDDO
 ENDDO

!find the starting and ending locations of the individual polygons.
 allocate(polyBgn(npoly))
 allocate(polyEnd(npoly))
 number_of_poly_loop:  DO i=1,npoly
  sze=0
  loop_over_polygon_file: DO j=1,pdm
   IF (int (polyId(j)) .eq. i)THEN
     sze=sze+1
     polyEnd(i)=j
     polyBgn(i)=j-sze+1
   ENDIF
  ENDDO loop_over_polygon_file
 ENDDO number_of_poly_loop

!print *, "Finished loading reef data" 

END SUBROUTINE load_reef_data

!**************************************************************
!read the file with the polygons - strata=TRUE
	
SUBROUTINE load_reef_data_strata (polyFileName)

 character(len = *), intent(in) :: polyFileName
 logical (kind=log_kind)  :: file_exists
 integer (kind=int_kind)  :: & 
      pdm, &    !size of file
      i,j, &    !loop variables
      sze,iunit, &
      pStrata
! real (kind = real_kind)  :: pd
 
!get size of file
 CALL getSize(polyFileName,pdm)

!allocate memory for polygon lat,lons and id and read in values
 allocate(polyLons(pdm))
 allocate(polyLats(pdm))
 allocate(polyId(pdm))
 allocate(polyDepth(pdm)) ! added 4/16
 
!open file
 CALL get_unit(iunit)
 INQUIRE(FILE=trim(polyFileName), EXIST=file_exists)
 IF (file_exists) THEN
  open(UNIT=iunit,FILE=trim(polyFileName), STATUS='old')
 else
  print *, "Error: File ", trim(polyFileName) ," does not exist"
  stop
 ENDIF

!read file 

 DO i=1,pdm
   read(iunit,*) polyLons(i),polyLats(i), polyId(i), polyDepth(i)
   !print *, "poly read depth", polyDepth(i) !Code is reading the depths fine.
 ENDDO

!close file
 CALL release_unit(iunit)

allocate(polyStrata(pdm))
! Assign polyStrata
 DO i=1,pdm
   CALL poly_strata(polyDepth(i),pStrata)
   polyStrata(i) = pStrata
 ENDDO

!number of polygons in file 
 npoly=int(polyId(pdm))
! print *, npoly

!make sure all longitudes of the polygons are between 0 and 360
 DO i=1,pdm
   DO WHILE (polyLons(i) .lt. 0.) 
    polyLons(i) = polyLons(i) + 360.
   ENDDO
   DO WHILE (polyLons(i) .ge. 360.) 
    polyLons(i) = polyLons(i) - 360.
   ENDDO
 ENDDO

!find the starting and ending locations of the individual polygons.
 allocate(polyBgn(npoly))
 allocate(polyEnd(npoly))
 number_of_poly_loop:  DO i=1,npoly
  sze=0
  loop_over_polygon_file: DO j=1,pdm
   IF (int (polyId(j)) .eq. i)THEN
     sze=sze+1
     polyEnd(i)=j
     polyBgn(i)=j-sze+1
   ENDIF
  ENDDO loop_over_polygon_file
 ENDDO number_of_poly_loop

!print *, "Finished loading reef data" 

END SUBROUTINE load_reef_data_strata

!**************************************************************
!checks whether the release points are within a settlement polygon
SUBROUTINE check_release_polygon(lonStart,latStart,id)

integer (kind=int_kind), intent(in) :: id
real (kind = real_kind), intent(in) :: lonStart, latStart

integer (kind=int_kind) :: releasePoly,rflag,i

!find release polygon
 releasePoly = -1 
 DO i=1,npoly
    rflag=-1 
    CALL pip(lonStart,latStart,polyLons(polyBgn(i):polyEnd(i)), &
             polyLats(polyBgn(i):polyEnd(i)),(polyEnd(i)-polyBgn(i)+1),rflag)
    IF (rflag.eq.1) THEN
     releasePoly = i
    ENDIF
 ENDDO

!Give warning if polygon in release file is not correct. 
 IF (releasePoly /= id) THEN
    print *, "Warning: Release ID", id, " in the release file does not match any settlement polygon, ", &
    	"is this what you intend?"
 ENDIF     

END SUBROUTINE check_release_polygon 

!**************************************************************
!checks if a particle is inside a polygon
SUBROUTINE check_reef_recruitment(lonEnd,latEnd,depthEnd,run_time,julian,r, inPoly)

integer (kind=int_kind), intent(in)  :: julian, r
real (kind = real_kind), intent(in)  :: lonEnd, latEnd, depthEnd
integer (kind=int8_kind), intent(in) :: run_time
integer (kind=int_kind), intent(out) :: inPoly

integer (kind=int_kind)              :: &
        i,cday,retentionPoly,rflag, julian2, &
        yearEnd, monthEnd, dayEnd, &
        yearStart, monthStart, dayStart

!calculate date
 cday= int(run_time/secs_in_day)
 julian2 = julian + cday
 CALL cdate (julian2, yearEnd, monthEnd, dayEnd)
 CALL cdate (julian, yearStart, monthStart, dayStart)

!find retention polygon
 retentionPoly = -1
 DO i=1,npoly
   rflag=-1
   !check point in polygon
   CALL pip(lonEnd,latEnd,polyLons(polyBgn(i):polyEnd(i)), &
            polyLats(polyBgn(i):polyEnd(i)),(polyEnd(i)-polyBgn(i)+1),rflag)
   IF (rflag.eq.1) THEN
    retentionPoly = i    
    exit   
  ENDIF
 ENDDO

!write data to con file if in polygon
 IF (retentionPoly .ge. 0) THEN
    CALL stateout_confile(r, retentionPoly, yearStart, monthStart, dayStart, &
                          yearEnd, monthEnd, dayEnd, run_time, depthEnd)
    inPoly = 1
 else
    inPoly = -1
 ENDIF

END SUBROUTINE check_reef_recruitment
!**************************************************************
!checks if a particle is inside a polygon - strata = TRUE
SUBROUTINE check_reef_recruitment_strata(lonEnd,latEnd,depthEnd,run_time,julian,r, inPoly, strataEnd)

integer (kind=int_kind), intent(in)  :: julian, r, strataEnd
real (kind = real_kind), intent(in)  :: lonEnd, latEnd, depthEnd
integer (kind=int8_kind), intent(in) :: run_time
integer (kind=int_kind), intent(out) :: inPoly

integer (kind=int_kind)              :: &
        i,cday,retentionPoly,rflag, julian2, &
        yearEnd, monthEnd, dayEnd, &
        yearStart, monthStart, dayStart

!calculate date
 cday= int(run_time/secs_in_day)
 julian2 = julian + cday
 CALL cdate (julian2, yearEnd, monthEnd, dayEnd)
 CALL cdate (julian, yearStart, monthStart, dayStart)

!find retention polygon
 retentionPoly = -1
 DO i=1,npoly
   rflag=-1
   ! ONLY check if particle depth is within poly strata
   IF (strataEnd == polyStrata(polyBgn(i))) THEN
   !check point in polygon
    CALL pip(lonEnd,latEnd,polyLons(polyBgn(i):polyEnd(i)), &
            polyLats(polyBgn(i):polyEnd(i)),(polyEnd(i)-polyBgn(i)+1),rflag)
    IF (rflag.eq.1) THEN
     retentionPoly = i    
     exit
   ENDIF   
  ENDIF
 ENDDO

!write data to con file if in polygon
 IF (retentionPoly .ge. 0) THEN
    CALL stateout_confile_strata(r, retentionPoly, yearStart, monthStart, dayStart, &
                          yearEnd, monthEnd, dayEnd, run_time, depthEnd, strataEnd)
    inPoly = 1
 else
    inPoly = -1
 ENDIF

END SUBROUTINE check_reef_recruitment_strata

!**************************************************************
! deallocate memory for reef variables
SUBROUTINE dealloc_reef

 deallocate(polyLons)
 deallocate(polyLats)
 deallocate(polyId)
 deallocate(polyBgn)
 deallocate(polyEnd)
 IF (strata) THEN
    deallocate(polyStrata)
 ENDIF

END SUBROUTINE dealloc_reef

!**************************************************************     

END MODULE mod_reef

