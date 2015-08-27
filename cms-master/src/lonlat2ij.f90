!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : lolat.f90                                                         *
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


!given the longitude,latitude and depth, this subroutine 
!calculates the indices (grdi, grdj, grdk) in the grid 
SUBROUTINE lonlat2ij (grdlat, grdlon, grddepth, ngrid, lonlen, latlen, depthlen, &
                      ckperiodicbc, r, n, grdi, grdj, grdk)

 USE mod_kinds
 USE globalvariables

 IMPLICIT NONE

 real (kind = real_kind), intent(in) :: grdlat, grdlon, grddepth
 logical (kind=log_kind), intent(in) :: ckperiodicbc
 integer (kind = int_kind), intent(in) :: r, n, lonlen, latlen, depthlen

 real (kind = real_kind) :: grdi(QAx), grdj(QAx), grdk(QAx), wrk_lat(5), wrk_lon(5), &
                            pt_depth(depthlen), lon1, lon2, lat1, lat2
 integer (kind=int_kind) :: i, j, k, oldi(QAx), oldj(QAx), oldk(QAx), ax, &
                            n_depth, n_lon, n_lat, ngrid, inpoly
 logical (kind=log_kind) :: novalue

 oldi = particle(r)%oldi(n)
 oldj = particle(r)%oldj(n)
 oldk = particle(r)%oldk(n)

 grdi(:) = -1.
 grdj(:) = -1.
 grdk(:) = -1.
do ax=1,QAx
 n_lon=nests(ngrid)%idm(ax)
 n_lat=nests(ngrid)%jdm(ax)
 n_depth=nests(ngrid)%kdm(ax)
 pt_depth=nests(ngrid)%depth(ax,:)

 novalue = .true.
 IF (nests(ngrid)%orthogrid) THEN
!calculate gridj given latitude value
  IF (oldj(ax) .ge. 3 .and. oldj(ax) .le. n_lat-1) THEN
   DO k = oldj(ax)-1, oldj(ax)+1
    lat1=nests(ngrid)%lat(ax,k-1,1)
    lat2=nests(ngrid)%lat(ax,k,1)
    IF (grdlat .ge. lat1 .and. grdlat .le. lat2) THEN
     grdj(ax) = real(k-1) + (grdlat - lat1) / (lat2 - lat1)
     novalue = .false.
    ENDIF
   ENDDO
  ENDIF
  IF (novalue) THEN
   DO k = 2, n_lat
    lat1=nests(ngrid)%lat(ax,k-1,1)
    lat2=nests(ngrid)%lat(ax,k,1)
    IF (grdlat .ge. lat1 .and. grdlat .le. lat2) THEN
     grdj(ax) = real(k-1) + (grdlat - lat1) / (lat2 - lat1)
    ENDIF
   ENDDO
  ENDIF
!calculate gridi given longitude value
  novalue = .true.
  IF (oldi(ax) .ge. 3 .and. oldi(ax) .le. n_lon-1) THEN
   DO k = oldi(ax)-1, oldi(ax)+1
    lon1=nests(ngrid)%lon(ax,k-1,1)
    lon2=nests(ngrid)%lon(ax,k,1)
    IF (grdlon .ge. lon1 .and. grdlon .le. lon2) then
     novalue = .false.
     grdi(ax) = real(k-1) + (grdlon - lon1) / (lon2 - lon1)
    ENDIF
   ENDDO
  ENDIF
  IF (novalue) THEN
   DO k = 2, n_lon
!special case if grdlon is between for example 359.9 and 0.1 degrees.
    lon1=nests(ngrid)%lon(ax,k-1,1)
    lon2=nests(ngrid)%lon(ax,k,1)
    IF (lon1 .gt. lon2) THEN
     IF (grdlon .ge. (lon1-360.) .and. grdlon .le. lon2) THEN
      novalue = .false.
      grdi(ax) = real(k-1) + (grdlon - (lon1-360.)) / (lon2-(lon1-360.))
     ENDIF
     IF (grdlon-360 .ge. (lon1-360.) .and. grdlon-360 .le. lon2) THEN
      novalue = .false.
      grdi(ax) = real(k-1) + (grdlon-360 - (lon1-360.)) / (lon2-(lon1-360.))
     ENDIF
    ELSE
     IF (grdlon .ge. lon1 .and. grdlon .le. lon2) then
      novalue = .false.
      grdi(ax) = real(k-1) + (grdlon - lon1) / (lon2 - lon1)
     ENDIF
    ENDIF
   ENDDO
  ENDIF
!if check periodic boundary conditions is set to true and grdi has no value yet
  IF ((ckperiodicbc) .and. (novalue)) THEN
   lon1=nests(ngrid)%lon(ax,n_lon,1)
   lon2=nests(ngrid)%lon(ax,1,1)
   IF (grdlon .gt. lon1) THEN
     grdi(ax) = n_lon + (grdlon - lon1) / (lon2 - lon1)
   ELSE
     grdi(ax) = 0. + (grdlon - lon1) / (lon2 - lon1)
   ENDIF
  ENDIF
 ELSE !orthogonal grid
  IF (oldi(ax) .ge. 3 .and. oldi(ax) .le. n_lat-1 .and. oldj(ax) .ge. 3 .and. oldj(ax) .le. n_lat-1) THEN
   DO i = oldi(ax)-2, oldi(ax)+2
    DO j = oldj(ax)-2, oldj(ax)+2
     wrk_lat=(/ nests(ngrid)%lat(ax,i-1,j-1), nests(ngrid)%lat(ax,i-1,j), nests(ngrid)%lat(ax,i,j), &
                nests(ngrid)%lat(ax,i,j-1),nests(ngrid)%lat(ax,i-1,j-1) /)
     wrk_lon=(/ nests(ngrid)%lon(ax,i-1,j-1), nests(ngrid)%lon(ax,i-1,j), nests(ngrid)%lon(ax,i,j), &
                nests(ngrid)%lon(ax,i,j-1),nests(ngrid)%lon(ax,i-1,j-1) /)
     CALL pip(grdlon,grdlat,wrk_lon,wrk_lat,5,inpoly)
     IF (inpoly .ge. 0) THEN
      grdi(ax) = real(i-1)
      grdj(ax) = real(j-1)
      novalue = .false.
     ENDIF
    ENDDO
   ENDDO
  ENDIF
  IF (novalue) THEN
   DO i = 2, n_lon
    DO j = 2, n_lat
     wrk_lat=(/ nests(ngrid)%lat(ax,i-1,j-1), nests(ngrid)%lat(ax,i-1,j), nests(ngrid)%lat(ax,i,j), &
                nests(ngrid)%lat(ax,i,j-1),nests(ngrid)%lat(ax,i-1,j-1) /)
     wrk_lon=(/ nests(ngrid)%lon(ax,i-1,j-1), nests(ngrid)%lon(ax,i-1,j), nests(ngrid)%lon(ax,i,j), &
                nests(ngrid)%lon(ax,i,j-1),nests(ngrid)%lon(ax,i-1,j-1) /)
!CODE FOR PERIODIC BOUNDARY CONDITIONS NEEDS TO GO HERE
     CALL pip(grdlon,grdlat,wrk_lon,wrk_lat,5,inpoly)
     IF (inpoly .ge. 0) THEN
      grdi(ax) = real(i-1)
      grdj(ax) = real(j-1)
      novalue = .false.
      goto 70
     ENDIF
    ENDDO
   ENDDO
70 continue
  ENDIF
 ENDIF !orthogonal grid

!calculate gridk given depth value
 IF (n_depth>1) THEN
  novalue = .true.
  IF (oldk(ax) .ge. 3 .and. oldk(ax) .le. n_depth-1) THEN
   DO k = oldk(ax)-1, oldk(ax)+1
    IF (grddepth .ge. pt_depth(k-1) .and. grddepth .le. pt_depth(k)) THEN
     grdk(ax) = real(k-1) + (grddepth - pt_depth(k-1)) / (pt_depth(k) - pt_depth(k-1))
     novalue = .false.
    ENDIF
   ENDDO
  ENDIF
  IF (novalue) THEN
   DO k = 2, n_depth
    IF (grddepth .ge. pt_depth(k-1) .and. grddepth .le. pt_depth(k)) THEN
     grdk(ax) = real(k-1) + (grddepth - pt_depth(k-1)) / (pt_depth(k) - pt_depth(k-1))
    ENDIF
   ENDDO
  ENDIF
 ELSE
   grdk(ax) = 1
 ENDIF
enddo !for ax=1,QAx
!update oldi, oldj and oldk
 particle(r)%oldi(n) = int(grdi(1))
 particle(r)%oldj(n) = int(grdj(1))
 particle(r)%oldk(n) = int(grdk(1))

END SUBROUTINE lonlat2ij
