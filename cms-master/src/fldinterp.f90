!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : fldinterp.f90                                                     *
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

SUBROUTINE fld3_interp (ilon, ilat, idepth, nlon, nlat, ndepth, fld, &
                        weight,nweight, fillvalue,ngrid,value, fail, landFlag)

 USE globalvariables
 USE mod_kinds
 USE constants

 IMPLICIT NONE

 integer (kind=int_kind), intent(in) :: &
     nlat, &   !number of Latitude points in grid
     nlon, &   !number of Longitude points in grid
     ndepth,&  !number of depth points in grid
     nweight,& !number of weights for interpolation
     ngrid
 real (kind = real_kind),intent(in)  :: &
     fillvalue,ilon,ilat,idepth,  &   
     fld (nlon, nlat, ndepth,nweight ) !gridded fields array
 real (kind = real_kind),intent(out) :: value   !interpolated value
 logical (kind=log_kind), intent(out):: &  
     fail, &  !(false) successful interpolation
     landFlag !(false) point is in water

 logical (kind=log_kind) :: landinterp, bicubic, trilinear,tricubic,valueZero
 real (kind = real_kind) :: fld3d (4,4,4),fld2d(4,4), weight(nweight),fr(4,4), fs (4), &
     r, & !fractional parts of ilon
     s, & !fractional parts of ilat
     t, & !fractional parts of idepth
     point1, point2, point3, point4,a,b,c,d,c01,c11,c10,c00,c0,c1, & 
     distance, fldTotal, distTotal,minDistance, minValue
 integer (kind=int_kind) :: counter,ilon_int, ilat_int, idepth_int, &
     i, i1, i2, j, j1, j2, k, k1, k2, xmin, xmax, ymin, ymax, zmin, zmax, countLand, new_x,x,y,z


!Initialize
 xmin = 1
 xmax = nlon+1
 ymin = 1
 ymax = nlat+1
 zmin = 1
 zmax = ndepth
   
 landFlag = .false.
 countLand = 0

 IF (interpversion.eq.1) THEN
! Handling 2d data
  IF (ndepth==1) THEN

!  ensure position is on the grid     
   IF ((ilon .ge. real (xmin) .and. ilon .le. real (xmax) .and. &
        ilat .ge. real (ymin) .and. ilat .le. real (ymax)) .or. (periodicbc)) THEN

!   set grid node offsets
    ilon_int = int (ilon)
    ilat_int = int (ilat)
    idepth_int = int (idepth)
    r = ilon - real (ilon_int)
    s = ilat - real (ilat_int)
    t = idepth - real (idepth_int)

    DO j = 1,4
     DO i = 1,4
      x = ilon_int + i - 2
      y = ilat_int + j - 2
      z = idepth_int
      IF (x .ge. xmin .and. x .le. xmax .and. &
          y .ge. ymin .and. y .le. ymax) THEN
         fld2d(i,j) = 0
         new_x = mod(x-1,nlon)+1
         IF (nests(ngrid)%fill_value .eq. 0.) THEN
          DO counter = 1, nweight
           IF (fld(new_x,y,z,counter) .eq. 0.) THEN
            fld2d(i,j) = 2.0**100
           ELSE
            fld2d(i,j) = fld2d(i,j) + weight(counter)*fld(new_x,y,z,counter)
           ENDIF
          ENDDO
         ELSE
          IF (nweight .eq. 2 .and. fld(new_x,y,z,1) .ge. fillvalue .and. fld(new_x,y,z,2) .lt. fillvalue) THEN
           fld2d(i,j) = fld(new_x,y,z,2)
          ELSE
           IF (nweight .eq. 2 .and. fld(new_x,y,z,2) .ge. fillvalue .and. fld(new_x,y,z,1) .lt. fillvalue) THEN
            fld2d(i,j) = fld(new_x,y,z,1)
           ELSE 
            DO counter = 1, nweight
             fld2d(i,j) = fld2d(i,j) + weight(counter)*fld(new_x,y,z,counter)
            ENDDO
           ENDIF
          ENDIF
         ENDIF
      ELSE
       fld2d(i,j) = 0
      ENDIF
     ENDDO
    ENDDO

!   check if the particle is near the border of the grid
    IF (ilon_int .gt. xmin .and. ilon_int .lt. (xmax-1) .and. &
        ilat_int .gt. ymin .and. ilat_int .lt. (ymax-1)) THEN
     bicubic = .true.
    ELSE
     bicubic = .false.
    ENDIF

    IF (bicubic) THEN
!    check surrounding 16 points
     i1 = 1; i2 = 4
     j1 = 1; j2 = 4
     DO j = j1, j2
      DO i = i1, i2
!      print *, i, " ", j, ": ", fld2d(i,j)
       IF (fld2d(i,j) .gt. fillvalue) THEN
        bicubic = .false.
       ENDIF
      ENDDO
     ENDDO
    ENDIF

    IF (bicubic) THEN
!    bicubic interpolation
     DO j = 1,4     
      point1  = fld2d(1,j)
      point2  = fld2d(2,j)
      point3  = fld2d(3,j)
      point4  = fld2d(4,j)
      a = -sixth*point1 + half*point2 - half*point3 + sixth*point4
      b = half*point1 - point2 + half*point3
      c = -third*point1 - half*point2 + point3 - sixth*point4
      d = point2
      fs(j) = a*r*r*r + b*r*r + c*r + d
     ENDDO
     point1  = fs(1)
     point2  = fs(2)
     point3  = fs(3)
     point4  = fs(4)
     a = -sixth*point1 + half*point2 - half*point3 + sixth*point4
     b = half*point1 - point2 + half*point3
     c = -third*point1 - half*point2 + point3 - sixth*point4
     d = point2
     value   = a*s*s*s + b*s*s + c*s + d
     fail = .false.
    ELSE
!    bilinear interpolation
     point1 = fld2d(3,2)
     point2 = fld2d(2,3)
     point3 = fld2d(3,3)
     point4 = fld2d(2,2)

     IF (point1  .gt. fillvalue) THEN
      countLand = countLand+1
      point1 = 0
     ENDIF
     IF (point2  .gt. fillvalue) THEN
      countLand = countLand+1
      point2 = 0
     ENDIF
     IF (point3  .gt. fillvalue) THEN
      countLand = countLand+1
      point3 = 0
     ENDIF
     IF (point4  .gt. fillvalue) THEN
      countLand = countLand+1
      point4 = 0
     ENDIF

     value = (1. - s) * ((1. - r) * point4 + r * point1) + s * ((1. - r) * point2 + r * point3)
     IF ((avoidCoast) .or. (idepth_int .gt. 1)) THEN
      IF (countLand .le. 1) THEN
       landFlag = .false.
      ELSE
       landFlag = .true.
      ENDIF
      fail = .false.
     ELSE
      IF (countLand .ge. 1) THEN
       value = 0
       fail = .true.
      ELSE
       fail = .false.
      ENDIF
     ENDIF 
    ENDIF 
 
   ELSE
!   IF particle is not on 2d grid  
    value = 0
    fail = .true.
   ENDIF 

  ELSE 
!  Handling 3d data
     
!  ensure position is on the grid
   IF (ilon .ge. real (xmin) .and. ilon .le. real (xmax) .and. &
       ilat .ge. real (ymin) .and. ilat .le. real (ymax) .and. &
       idepth .ge. real (zmin) .and. idepth .le. real (zmax)) THEN

!   set grid node offsets
    ilon_int = int (ilon)
    ilat_int = int (ilat)
    idepth_int = int (idepth)
    r = ilon - real (ilon_int)
    s = ilat - real (ilat_int)
    t = idepth - real (idepth_int)

    DO k = 1,4
     DO j = 1,4
      DO i = 1,4
       x = ilon_int + i - 2
       y = ilat_int + j - 2
       z = idepth_int + k - 2
       IF (x .ge. xmin .and. x .le. xmax .and. &
           y .ge. ymin .and. y .le. ymax .and. &
           z .ge. zmin .and. z .le. zmax) THEN
        fld3d(i,j,k) = 0
        new_x = mod(x-1,nlon)+1
        IF (nests(ngrid)%fill_value .eq. 0.) THEN
         DO counter = 1, nweight
          IF (fld(new_x,y,z,counter) .eq. 0.) THEN
           fld3d(i,j,k) = 2.0**100
          ELSE
           fld3d(i,j,k) = fld3d(i,j,k) + weight(counter)*fld(new_x,y,z,counter)
          ENDIF
         ENDDO
        ELSE
         IF (nweight .eq. 2 .and. abs(fld(new_x,y,z,1)) .ge. fillvalue .and. &
             abs(fld(new_x,y,z,2)) .lt. fillvalue) THEN
          fld3d(i,j,k) = fld(new_x,y,z,2)
         ELSE
          IF (nweight .eq. 2 .and. abs(fld(new_x,y,z,2)) .ge. fillvalue .and. &
              abs(fld(new_x,y,z,1)) .lt. fillvalue) THEN
           fld3d(i,j,k) = fld(new_x,y,z,1)
          ELSE 
           DO counter = 1, nweight
            fld3d(i,j,k) = fld3d(i,j,k) + weight(counter)*fld(new_x,y,z,counter)
           ENDDO
          ENDIF
         ENDIF
        ENDIF
       ELSE
        fld3d(i,j,k) = 0
       ENDIF
      ENDDO
     ENDDO
    ENDDO

    landinterp = .false. 

!   check if the particle is near the border of the grid
    IF (ilon_int .gt. xmin .and. ilon_int .lt. (xmax-1) .and. &
        ilat_int .gt. ymin .and. ilat_int .lt. (ymax-1) .and. &
        idepth_int .gt. zmin .and. idepth_int .lt. (zmax-1)) THEN
     tricubic = .true.
     trilinear = .true.
    ELSE
     tricubic = .false.
     trilinear = .true.
    ENDIF

    IF (tricubic) THEN
!    check surrounding 64 points
     i1 = 1; i2 = 4
     j1 = 1; j2 = 4
     k1 = 1; k2 = 4
    ELSE
!    check surrounding 8 points
     i1 = 2; i2 = 3
     j1 = 2; j2 = 3
     k1 = 2; k2 = 3
    ENDIF

    DO k = k1, k2
     DO j = j1, j2
      DO i = i1, i2
!       print *, i, " ", j, " ", k, ": ", fld3d(i,j,k)
       IF (abs(fld3d(i,j,k)) .gt. fillvalue) THEN
        tricubic = .false.
        IF ((i .eq. 2 .or. i .eq. 3) .and. (j .eq. 2 .or. j .eq. 3) .and. (k .eq. 2 .or. k .eq. 3)) THEN
!        one of the surrounding 8 points is missing
         trilinear = .false.
!        count the points that are on land  
         countLand = countLand + 1
        ENDIF
       ENDIF
      ENDDO
     ENDDO
    ENDDO

!   if one of the 8 points is on land than USE different interpolation
!   print *, "Points on land: ", countLand
    IF ((avoidCoast) .or. (idepth_int .gt. 1)) THEN
     IF ((countLand .le. 8) .and. (countLand .gt. 0))  THEN
      landinterp = .true.
      landFlag = .true.
     ENDIF
    ENDIF

    IF (landinterp) THEN
!    USE this interpolation if one of the points lies on the land
     fldTotal = 0
     distTotal = 0
     valueZero = .false.
     minDistance = fillvalue
     DO k = 2,3
      DO j =2,3
       DO i =2,3
        distance = ((ilon_int+i-2)-ilon)*((ilon_int+i-2)-ilon) &
                 + ((ilat_int+j-2)-ilat)*((ilat_int+j-2)-ilat) &
                 + ((idepth_int+k-2)-idepth)*((idepth_int+k-2)-idepth)
!       print *, "field: ", fld3d(i,j,k)
        IF (distance .eq. 0.) THEN
         IF (fld3d(i,j,k) .lt. fillvalue) THEN
          value = fld3d(i,j,k)
          landFlag = .false.
         ELSE
          value = 0.
         ENDIF
         fail = .false.
         valueZero = .true.
        ENDIF
        IF (valueZero .eqv. .false.) THEN
         IF (distance .lt. minDistance) THEN
          minDistance = distance
          minValue = fld3d(i,j,k)
         ENDIF
         IF (fld3d(i,j,k) .lt. fillvalue) THEN
          distTotal = distTotal + (1/distance)
          fldTotal = fldTotal + (fld3d(i,j,k)/distance) 
         ENDIF   
        ENDIF
       ENDDO
      ENDDO
     ENDDO
     IF (valueZero .eqv. .false.) THEN
      IF (countLand .eq. 8) THEN
       value = 0.
      ELSE
       value = fldTotal / distTotal
      ENDIF
      fail = .false.
      IF (((minValue .lt. fillvalue) .or. (countLand .le. 2)) .and. (countLand .le. 4)) THEN
       landFlag = .false.
      ENDIF
     ENDIF
    ELSE IF (tricubic) THEN
!    perform tricubic interpolation
     DO j = 1,4 
      DO k = 1,4
       point1  = fld3d(1,j,k)
       point2  = fld3d(2,j,k)  
       point3  = fld3d(3,j,k)
       point4  = fld3d(4,j,k)
       a = -sixth*point1 + half*point2 - half*point3 + sixth*point4
       b = half*point1 - point2 + half*point3
       c = -third*point1 - half*point2 + point3 - sixth*point4
       d = point2
       fr(j,k) = a*r*r*r + b*r*r + c*r + d
      ENDDO
     ENDDO
     DO k = 1,4
      point1  = fr(1,k)
      point2  = fr(2,k)
      point3  = fr(3,k)
      point4  = fr(4,k)
      a = -sixth*point1 + half*point2 - half*point3 + sixth*point4
      b = half*point1 - point2 + half*point3
      c = -third*point1 - half*point2 + point3 - sixth*point4
      d = point2
      fs(k) = a*s*s*s + b*s*s + c*s + d
     ENDDO
     point1  = fs(1)
     point2  = fs(2)
     point3  = fs(3)
     point4  = fs(4)
     a = -sixth*point1 + half*point2 - half*point3 + sixth*point4
     b = half*point1 - point2 + half*point3
     c = -third*point1 - half*point2 + point3 - sixth*point4
     d = point2
     value   = a*t*t*t + b*t*t + c*t + d
     fail = .false.
    ELSE IF (trilinear) THEN
!    perform trilinear interpolation
     IF (idepth .eq. zmax .and. ilon .eq. xmax .and. ilat .eq. ymax) THEN 
      value = fld3d(2,2,2)
     ELSE IF (idepth .eq. zmax .and. ilon .eq. xmax) THEN 
      value = ((1-s)*fld3d(2,2,2)) + (s*fld3d(2,3,2))   
     ELSE IF (idepth .eq. zmax .and. ilat .eq. ymax) THEN 
      value = ((1-r)*fld3d(2,2,2)) + (r*fld3d(3,2,2))
     ELSE IF (ilon .eq. xmax .and. ilat .eq. ymax) THEN 
      value = ((1-t)*fld3d(2,2,2)) + (t*fld3d(2,2,3))     
     ELSE IF (idepth .eq. zmax) THEN
      c0 = ((1-r)*fld3d(2,2,2)) + (r*fld3d(3,2,2))
      c1 = ((1-r)*fld3d(2,3,2)) + (r*fld3d(3,3,2))
      value  = ((1-s)*c0) + (s*c1)
     ELSE IF (ilon .eq. xmax) THEN
      c0  = ((1-s)*fld3d(2,2,2)) + (s*fld3d(2,3,2))
      c1  = ((1-s)*fld3d(2,2,3)) + (s*fld3d(2,3,3)) 
      value = ((1-t)*c0) + (t*c1)   
     ELSE IF (ilat .eq. ymax) THEN
      c0  = ((1-r)*fld3d(2,2,2)) + (r*fld3d(3,2,2))
      c1  = ((1-r)*fld3d(2,2,3)) + (r*fld3d(3,2,3)) 
      value = ((1-t)*c0) + (t*c1)
     ELSE
      c00 = ((1-r)*fld3d(2,2,2)) + (r*fld3d(3,2,2))
      c10 = ((1-r)*fld3d(2,3,2)) + (r*fld3d(3,3,2))
      c0  = ((1-s)*c00) + (s*c10)
      c01 = ((1-r)*fld3d(2,2,3)) + (r*fld3d(3,2,3))
      c11 = ((1-r)*fld3d(2,3,3)) + (r*fld3d(3,3,3))
      c1  = ((1-s)*c01) + (s*c11)
      value = ((1-t)*c0) + (t*c1)
     ENDIF
     fail = .false.
    ELSE 
!    no interpolation possible, set value to 0 and failure flag to true
     value = 0
     fail = .true.
    ENDIF
   ELSE
!   position is off grid, set value to 0 and failure flag to true
    value = 0
    fail = .true.
   ENDIF

  ENDIF  ! 3d fields
 ELSEIF (interpversion.eq.2) THEN ! New trilinear interpolation
  print *, 'Version 2 interpolation is not implemented yet. Stopping now'
  stop
 ELSEIF (interpversion.eq.3) THEN ! Non-orthogonal interpolation
  print *, 'Version 3 interpolation is not implemented yet. Stopping now'
  stop
 ENDIF
END SUBROUTINE

!**************************************************************
!Interpolates 2D fields to an arbitray point.
!Only interpolates in space not in time!
SUBROUTINE fld2d_interp (ilon, ilat, nlon, nlat, fld, fillvalue,value, fail)

 USE globalvariables
 USE mod_kinds
 USE constants

 implicit  none
 integer (kind=int_kind), intent(in) :: &
     nlat, &  !number of latitude points in grid
     nlon    !number of longitude points in grid
 real (kind = real_kind),intent(in)     :: &
     fillvalue,ilon,ilat, fld (nlon, nlat)
 real (kind = real_kind),intent(out)    :: &
     value   !interpolated value

 logical (kind=log_kind), intent(out) :: &   
     fail    !(false) successful interpolation
 integer (kind=int_kind) :: &
     ilon_int, ilat_int, xmin, xmax,n, ymin, ymax
 real (kind = real_kind) :: &
     r,s,point1, point2,point3, point4

!Initialize
 xmin = 1
 xmax = nlon
 ymin = 1
 ymax = nlat

!ensure position is on the grid
 IF (ilon .ge. real (xmin) .and. ilon .le. real (xmax) .and. ilat .ge. real (ymin) .and. ilat .le. real (ymax)) THEN

! set grid node offsets
  ilon_int = int (ilon)
  ilat_int = int (ilat)
  r = ilon - real (ilon_int)
  s = ilat - real (ilat_int)

! for 2d data do bilinear straigthaway
  point1 = fld(ilon_int+1,ilat_int)
  point2 = fld(ilon_int,ilat_int+1)
  point3 = fld(ilon_int+1,ilat_int+1)
  point4 = fld(ilon_int,ilat_int)
  value = (1. - s) * ((1. - r) * point4 + r * point1) + s * ((1. - r) * point2 + r * point3)
  fail = .false.
 ELSE
! position is off grid, set missing and failure flags
  value = 0
  fail = .true.
 ENDIF

END SUBROUTINE


!**************************************************************
