!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : util.f90                                                          *
!* Last Modified: 2011-07-22                                                *
!* Code contributors: Judith Helgers, Ashwanth Srinivasan, Claire B. Paris, * 
!*                    Erik van Sebille                                      *
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

!calculate density with temperature and salinity
!Uses sigma-0 so does not work for all data
SUBROUTINE calc_dens (temp,saln,theta,idm,jdm,kdm)
 USE mod_kinds
 IMPLICIT NONE
 
 integer (kind=int_kind), intent(in)                           :: idm,jdm,kdm
 real (kind = real_kind),dimension(idm,jdm,kdm,1), intent(in)  :: temp,saln
 real (kind = real_kind),dimension(idm,jdm,kdm,1), intent(out) :: theta

 integer (kind=int_kind) :: i,j,k
 real (kind = real_kind) :: thbase,s,t,c1,c2,c3,c4,c5,c6,c7

!coefficients for sigma-0 (based on Brydon & Sun fit)
 parameter (c1=-1.36471E-01, c2= 4.68181E-02, c3= 8.07004E-01, &
            c4=-7.45353E-03, c5=-2.94418E-03, c6= 3.43570E-05, c7= 3.48658E-05)

 thbase=1000.0
 theta=0.0 
 DO k=1,kdm
  DO j=1,jdm
   DO i=1,idm
    IF(temp(i,j,k,1) < 2.00**99 .and. saln(i,j,k,1) < 2.00**99) THEN
     s= saln(i,j,k,1)
     t= temp(i,j,k,1)
     theta(i,j,k,1) = (c1+c3*s+t*(c2+c5*s+t*(c4+c7*s+c6*t)))
    ELSE
     theta(i,j,k,1) = 2.00**100
    ENDIF
   ENDDO
  ENDDO
 ENDDO
 theta=thbase+theta

END SUBROUTINE calc_dens

!**************************************************************
!Point in polygon algorithm
!in_out = 1 --> point lies within polygon
!in_out = -1 --> point lies outside polygon
SUBROUTINE pip (x0, y0, x, y, n, in_out)
 USE mod_kinds
 IMPLICIT NONE

 integer (kind=int_kind), intent(in)  :: n 
 real (kind = real_kind), dimension(n), intent(in):: x(n), y(n)
 real (kind = real_kind), intent(in)  :: x0,y0
 integer (kind=int_kind), intent(out) :: in_out

 integer (kind=int_kind) :: i, j

 in_out = -1
 DO i=1,n
  j = i + 1
  IF (j .gt. n) j = 1
  IF ( ((y(i).gt.y0) .neqv. (y(j).gt.y0)) .and. &
       ( x0 .lt. (x(j) -x(i)) * (y0 -y(i)) / (y(j) -y(i)) + x(i))) THEN 
        in_out = in_out * (-1)
  ENDIF
 ENDDO

END SUBROUTINE pip

!**************************************************************
!This SUBROUTINE gets the number of lines in file "FileName" 
!assuming that the number of lines Does not exceed 1e9.
SUBROUTINE getSize(pFileName,NumRecords)
 USE mod_kinds
 USE mod_iounits
 USE constants
 IMPLICIT NONE

 character(len=*), intent(in) :: pFileName
 integer (kind=int_kind), intent(out):: NumRecords

 integer (kind=int_kind) :: i, iunit, stat
 logical (kind=log_kind) :: file_exists

!open file
 call get_unit(iunit)  
 INQUIRE(FILE=trim(pFileName), EXIST=file_exists)
 IF (file_exists) THEN
   OPEN (UNIT=iunit,file=trim(pFileName), STATUS='old')
 ELSE
   print *, "Error: File ", trim(pFileName)," Does not exist"
   stop
 ENDIF

!read file
 DO i=1,max_records
   read(iunit,*,iostat=stat)
   IF (stat /= 0) exit
 ENDDO

!NumRecords is the number of lines in the file
 NumRecords=(i-1)

!close file
 call release_unit(iunit)
END SUBROUTINE getSize

!**************************************************************
!calculate the distance between two points
SUBROUTINE Distance_Sphere(rlon1,rlat1, rlon2, rlat2, distance)
 USE mod_kinds
 USE constants
 IMPLICIT NONE

 real (kind = real_kind), intent(in)  :: rlon1,rlat1, rlon2,rlat2
 real (kind = real_kind), intent(out) :: distance

 real (kind = real_kind) :: dist,rln1,rlt1,rln2,rlt2

 rln1=deg2rad*rlon1; rlt1=deg2rad*rlat1;
 rln2=deg2rad*rlon2; rlt2=deg2rad*rlat2;
 dist = sin((rlt2-rlt1)/2.) * sin((rlt2-rlt1)/2.) + sin((rln2-rln1)/2.) * sin((rln2-rln1)/2.) * cos(rlt1) * cos(rlt2)
 distance=REarth * 2. * atan2(sqrt(dist), sqrt(1-dist))

END SUBROUTINE Distance_Sphere

!**************************************************************





