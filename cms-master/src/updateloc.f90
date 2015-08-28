!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : updateloc.f90                                                     *
!* Last Modified: 2011-07-22                                                *
!* Code contributors: Judith Helgers, Ashwanth Srinivasan, Claire B. Paris, * 
!*                    Ana Carolina Vaz                                      *
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


!calculate new lon,lat and depth with U-velocity, V-velocity and W-velocity
SUBROUTINE updateloc(lon_old,lat_old,depth_old,u,v,w,flag,tmstp,lon_new,lat_new,depth_new,upperdepth)

 USE mod_kinds
 USE constants
 USE globalvariables
  
 IMPLICIT NONE

 real (kind = real_kind),intent(in)  :: lon_old,lat_old,depth_old, u,v,w,tmstp, upperdepth
 logical (kind=log_kind), intent(in) :: flag(3)
 real (kind = real_kind),intent(out) :: lon_new,lat_new,depth_new 

 real (kind = real_kind) :: dlon,rln1,rlt1,rlt2,dx,dy,dz

!check incoming velocities and return IF not valid
 IF((flag(1)) .and. (flag(2))) THEN
  lon_new=lon_old
  lat_new=lat_old
  depth_new=depth_old    
  return
 ELSE 

! calculate translation along 3 axes
  IF ((agrid .eqv. .false.) .and. (velocity_conversion_factor .ne. 1)) THEN
   dx=tmstp*u*velocity_conversion_factor
   dy=tmstp*v*velocity_conversion_factor
  ELSE
   dx=tmstp*u
   dy=tmstp*v
  ENDIF
  IF (agrid .eqv. .false.) THEN
   IF (wvel_positive_direction .eq. 'upward') THEN
    dz=-1.0 * tmstp*w
   ENDIF
   IF (velocity_conversion_factor .ne. 1) THEN
    dz=dz*velocity_conversion_factor
   ENDIF
  ELSE
   dz=tmstp*w
  ENDIF

! convert into new lon/lat/depth positions and return these values
  rln1=deg2rad*lon_old
  rlt1=deg2rad*lat_old
  rlt2=asin(sin(rlt1+dy*REINV)*cos(dx*REINV))
  dlon=atan2(sin(dx*REINV)*cos(rlt1),(cos(dx*REINV)-sin(rlt1)*sin(rlt2)))
  lon_new=(rln1+dlon)*rad2deg; 
  lat_new=rlt2*rad2deg;
  depth_new=depth_old+dz

! longitude values have to be between 0 and 360
  DO WHILE (lon_new .lt. 0.)
   lon_new = lon_new + 360.
  ENDDO
 
  DO WHILE (lon_new .ge. 360.)  
   lon_new = lon_new - 360.
  ENDDO
    
! make sure that particle can not fly in air
  IF (upperlevelsurface.eqv..true.) THEN
   IF (depth_new .lt. upperdepth) THEN
    depth_new = upperdepth
   ENDIF
  ENDIF
 ENDIF 
END SUBROUTINE updateloc  

