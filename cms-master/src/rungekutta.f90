!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : rungekutta.f90                                                    *
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

!calculates u,v,w, temp, saln and dens at postition of particle
 SUBROUTINE rungakutta(ngrid,lon,lat,depth,run_time,diam_part,dens_part,rk_step,&
      u,v,w,t,s,rho,flag,landFlag,r,n,grid_i,grid_j,grid_k)

 USE globalvariables
 USE mod_calendar
 USE mod_kinds
 USE mod_buoyancy
 USE mod_ibio


 IMPLICIT NONE

 real (kind = real_kind), intent(in)  :: lon,lat,depth,diam_part,dens_part
 integer (kind=int_kind), intent(in)  :: ngrid, rk_step, r, n
 integer (kind=int8_kind), intent(in) :: run_time
 logical (kind=log_kind), intent(inout) :: landFlag
 real (kind = real_kind), intent(out) :: u,v,w,t,s,rho
 logical (kind=log_kind), intent(out) :: flag(6)

 real (kind = real_kind) :: grid_i(QAx),grid_j(QAx),grid_k(QAx), & !position of particle in i,j,k
     w2, & !for buoyancy 
     ssh1,ssh2, & !for tidalMovement
     value
 logical (kind=log_kind) :: fail, landFlag2, landFlag1
 integer (kind=int_kind) :: n_weight, firstFile

!calculate i,j,k position of particle
 CALL lonlat2ij (lat,lon, depth, ngrid, size(nests(ngrid)%lon(1,:,:)), size(nests(ngrid)%lat(1,:,:)),&
   size(nests(ngrid)%depth(1,:)),periodicbc,r,n,grid_i,grid_j,grid_k)

 flag=.False.

 IF (nests(ngrid)%time_units == "months") THEN
  n_weight = 4
 ELSE
  n_weight = 2
 ENDIF

!find out which datafiles to use
 IF (nextFile(rk_step) .eqv. .true.) THEN
! start at fname(2) for this runga kutta step
  firstFile = 2
 ELSE 
! standard is to start at fname(1)
  firstFile = 1
 ENDIF
       

!calculate U-velocity
 CALL fld3_interp(grid_i(UAx),grid_j(UAx),grid_k(UAx),nests(ngrid)%idm(UAx),nests(ngrid)%jdm(UAx),nests(ngrid)%kdm(UAx), &
      nests(ngrid)%uvel(:,:,:,firstFile:firstFile+n_weight-1),nests(ngrid)%w(rk_step,:),n_weight,2.0**19, &
      ngrid,value,fail,landFlag1)  
 IF (fail .or. abs(value)>100.0) THEN
  u=0.0
 ELSE
  u=value 
 ENDIF
 flag(1)=fail

!calculate V-velocity
 CALL fld3_interp(grid_i(VAx),grid_j(VAx),grid_k(VAx),nests(ngrid)%idm(VAx),nests(ngrid)%jdm(VAx),nests(ngrid)%kdm(VAx), &
      nests(ngrid)%vvel(:,:,:,firstFile:firstFile+n_weight-1),nests(ngrid)%w(rk_step,:),n_weight,2.0**19, &
      ngrid,value,fail,landFlag2)
 IF (fail .or. abs(value)>100.0) THEN
  v=0.0
 ELSE
  v=value
 ENDIF
 flag(2)=fail

!check if particle is too close to or on land
  IF (landFlag) THEN
! if subroutine is only called to check if particle is on land then 
! check if particle is on land and then return to subroutine move
   IF ((landFlag2) .or. (landFlag1)) THEN
    landFlag = .true.
   ELSE 
    landFlag = .false.
   ENDIF
   RETURN 
  ELSE 
   IF ((landFlag2) .or. (landFlag1)) THEN
    landFlag = .true.
   ELSE 
    landFlag = .false.
   ENDIF
  ENDIF

!calculate W-velocity     
 IF (ibio) THEN
! if ibio = true then the W-velocity is not used
  flag(3) = .false.
  w = 0.0
 ELSE  
  CALL fld3_interp(grid_i(WAx),grid_j(WAx),grid_k(WAx),nests(ngrid)%idm(WAx),nests(ngrid)%jdm(WAx),nests(ngrid)%kdm(WAx), &
       nests(ngrid)%wvel(:,:,:,firstFile:firstFile+n_weight-1),nests(ngrid)%w(rk_step,:),n_weight,2.0**19, &
       ngrid,value,fail,landFlag2)
  IF (fail .or. abs(value)>100.0) THEN
   w=0.0
  ELSE
   w=value
  ENDIF
  flag(3)=fail
 ENDIF

 IF (withibm) THEN
!calculate Temperature, only necessary if buoyancy or mortality is used 
 IF ((buoyancy) .or. (mort) .or. (outputtemp)) THEN
  CALL fld3_interp(grid_i(QAx),grid_j(QAx),grid_k(QAx),nests(ngrid)%idm(QAx),nests(ngrid)%jdm(QAx),nests(ngrid)%kdm(QAx), &
       nests(ngrid)%temp(:,:,:,firstFile:firstFile+1),nests(ngrid)%w(rk_step,:),n_weight,2.0**19,ngrid,value,fail,landFlag2)
  IF (fail) THEN
   t=0.0
  ELSE
   t=value
  ENDIF
  flag(4)=fail
 ELSE
  flag(4) = .false.
  t = 0.0
 ENDIF

!Salinity will not be used in the rest of the cms so it is not calculated
 IF (outputsaln) THEN
  CALL fld3_interp(grid_i(QAx),grid_j(QAx),grid_k(QAx),nests(ngrid)%idm(QAx),nests(ngrid)%jdm(QAx),nests(ngrid)%kdm(QAx), &
       nests(ngrid)%saln(:,:,:,firstFile:firstFile+1),nests(ngrid)%w(rk_step,:),n_weight,2.0**19,ngrid,value,fail,landFlag2)
  IF (fail) THEN
   s=0.0
  ELSE
   s=value
  ENDIF
  flag(5)=fail
 ELSE
  flag(5) = .false.
  s = 0.0
 ENDIF

!calculate Density, only necessary if using buoyancy      
 IF (buoyancy) THEN
  CALL fld3_interp(grid_i(QAx),grid_j(QAx),grid_k(QAx),nests(ngrid)%idm(QAx),nests(ngrid)%jdm(QAx),nests(ngrid)%kdm(QAx), &
      nests(ngrid)%dens(:,:,:,firstFile:firstFile+n_weight-1),nests(ngrid)%w(rk_step,:),n_weight,2.0**19,ngrid,value,fail,landFlag2)
  IF (fail)  THEN
   rho=0.0
  ELSE
   rho=value     
  ENDIF
  flag(6)=fail
 ELSE
  flag(6) = .false.
  rho = 0.0
 ENDIF


!Adding buyancy component only if there is a t and rho    
 IF (buoyancy) THEN
  IF (rho .gt. 0. .and. t .gt. 0.) THEN
   IF (rho .lt. 1000.) THEN
      rho = rho + 1000.
   ENDIF
   CALL get_buoyancy(rho, t,diam_part, dens_part, w2)
   w=w + w2      
  ENDIF
 ENDIF

!calculate if particles can move due to sea surface height
!if the sea surface height rises then the shrimp can move
!if the sea surface height drops then the shrimp cannot move
 IF (tidalMovement) THEN
  notmove = .false. 
  IF (run_time .gt. (tstStart*secs_in_day)) THEN
   CALL fld2d_interp(grid_i(QAx),grid_j(QAx),nests(ngrid)%idm(QAx),nests(ngrid)%jdm(QAx), &
        nests(ngrid)%ssh(:,:,1),2.0**19,ngrid,ssh1,fail)
   CALL fld2d_interp(grid_i(QAx),grid_j(QAx),nests(ngrid)%idm(QAx),nests(ngrid)%jdm(QAx), &
        nests(ngrid)%ssh(:,:,2),2.0**19,ngrid,ssh2,fail)
   IF (ssh1 .ge. ssh2) THEN
    u = 0.0
    v = 0.0
    w = 0.0 
    notmove = .true.
   ENDIF
  ENDIF
 ENDIF

 ENDIF !withibm
END SUBROUTINE

