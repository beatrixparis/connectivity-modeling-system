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
SUBROUTINE lonlat2ij (grdlat, grdlon, grddepth,n_lat, pt_lat,n_lon, pt_lon, &
     n_depth,pt_depth, ckperiodicbc,grdi, grdj, grdk)

 USE mod_kinds
 IMPLICIT NONE

 integer (kind=int_kind), intent(in) :: n_depth, n_lon, n_lat
 real (kind = real_kind), intent(in) :: grdlat, grdlon, grddepth, &
      pt_depth(n_depth), pt_lat(n_lat), pt_lon(n_lon)
 logical (kind=log_kind), intent(in) :: ckperiodicbc
 real (kind = real_kind), intent(out):: grdi, grdj, grdk

 integer (kind=int_kind) :: k
 logical (kind=log_kind) :: novalue


!calculate gridj given latitude value
 DO k = 2, n_lat
  IF (grdlat .ge. pt_lat(k-1) .and. grdlat .le. pt_lat(k)) THEN
   grdj = real(k-1) + (grdlat - pt_lat(k-1)) / (pt_lat(k) - pt_lat(k-1))
  ENDIF
 ENDDO

!calculate gridi given longitude value
 novalue = .true.
 DO k = 2, n_lon
   !special case if grdlon is between for example 359.9 and 0.1 degrees.
   IF (pt_lon(k-1) .gt. pt_lon(k)) THEN
    IF (grdlon .ge. (pt_lon(k-1)-360.) .and. grdlon .le. pt_lon(k)) THEN
     novalue = .false.
     grdi = real(k-1) + (grdlon - (pt_lon(k-1)-360.)) / (pt_lon(k)-(pt_lon(k-1)-360.))
    ENDIF
    IF (grdlon-360 .ge. (pt_lon(k-1)-360.) .and. grdlon-360 .le. pt_lon(k)) THEN
     novalue = .false.
     grdi = real(k-1) + (grdlon-360 - (pt_lon(k-1)-360.)) / (pt_lon(k)-(pt_lon(k-1)-360.))
    ENDIF
   ELSE
    IF (grdlon .ge. pt_lon(k-1) .and. grdlon .le. pt_lon(k)) then
     novalue = .false.
     grdi = real(k-1) + (grdlon - pt_lon(k-1)) / (pt_lon(k)- pt_lon(k-1))
    ENDIF
   ENDIF
 ENDDO

!if check periodic boundary conditions is set to true and grdi has no value yet
 IF ((ckperiodicbc) .and. (novalue)) THEN
  IF (grdlon .gt. pt_lon(n_lon)) THEN
    grdi = n_lon + (grdlon - pt_lon(n_lon)) / (pt_lon(1) - pt_lon(n_lon))
  ELSE
    grdi = 0. + (grdlon - pt_lon(n_lon)) / (pt_lon(1) - pt_lon(n_lon))
  ENDIF
 ENDIF

!calculate gridk given depth value
 IF (n_depth>1) THEN
  DO k = 2, n_depth
   IF (grddepth .ge. pt_depth(k-1) .and. grddepth .le. pt_depth(k)) THEN
    grdk = real(k-1) + (grddepth - pt_depth(k-1)) / (pt_depth(k) - pt_depth(k-1))
   ENDIF
  ENDDO
 ELSE
   grdk = 1
 ENDIF


END SUBROUTINE lonlat2ij
