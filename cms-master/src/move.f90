!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : move.f90                                                          *
!* Last Modified: 2011-07-22                                                *
!* Code contributors: Judith Helgers, Ashwanth Srinivasan, Claire B. Paris, * 
!*                   Ana Carolina Vaz                                       *
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

!moves particle with runge kutta (rk4) method
SUBROUTINE move(startR, endR,time_t)
 USE globalvariables
 USE mod_ibio
 USE mod_mort
 USE mod_turb
 USE mod_mixedlayerphysics

 IMPLICIT NONE

 integer (kind=int_kind), intent(in) :: startR, endR
 integer (kind=int8_kind), intent(in) :: time_t

 integer (kind=int_kind)  :: ngrid, n, r, layerNew 
 integer (kind=int8_kind) :: run_time
 real (kind = real_kind)  :: &
    u1,u2,u3,u4,uf,un, &
    v1,v2,v3,v4,vf,vn, &
    w1,w2,w3,w4,wf,wn, &
    tn,sn,rn, &
    hh,h6,h1, &
    xtmp1,ytmp1,ztmp1, &
    xtmp2,ytmp2,ztmp2, &
    xtmp3,ytmp3,ztmp3, &
    xnew,ynew,znew, &
    xold,yold,zold, &
    grid_i(QAx),grid_j(QAx),grid_k(QAx), &
    ztemp,zsaln,zdens, &
    upperdepth, &
    diam, dens, &
    uturb, vturb, wturb !for turbulence 
 
 logical (kind=log_kind)  :: flag(10),landFlag,flagMort

 hh=timestep*0.5
 h6=timestep/6.0
 h1 = timestep
 flag= .False.
 
!repeat for all particles
 DO r=startR, endR

! only for MassSpawning, define how to move in vertical direction
  IF (massSpawning) THEN
   IF (eggTimePassed(r)) THEN
    buoyancy = .false.
    ibio = .true.
   ELSE
    buoyancy = .true.
    ibio = .false.
   ENDIF
  ENDIF

  IF (particle(r)%start .lt. time_t) THEN
   DO n=1,particle(r)%num_rel
    IF (particle(r)%move(n) .eqv. .true.) THEN
!    move all particles n on location r that are allowed to move

     run_time = time_t - particle(r)%start
     xold=particle(r)%nlon(n)
     yold=particle(r)%nlat(n)
     zold=particle(r)%ndepth(n)
     IF ((buoyancy) .or. (diffpart)) THEN
      diam=particle(r)%diam(n)
      dens=particle(r)%density(n)
     ELSE
      diam=-1
      dens=-1
     ENDIF
     flag = .False.

!    ================== Runga Kutta Step 1 ==================
!    check in which nest the particle is     
     CALL findnest(xold,yold,zold,ngrid,r,n)
     upperdepth=max(nests(ngrid)%depth(UAx,1), nests(ngrid)%depth(VAx,1),nests(ngrid)%depth(WAx,1))
!    IF particle is outside all nests then stop moving 
     IF (ngrid .eq. -1) THEN
      flag(7) = .True.
      goto 100
     ENDIF  

     IF (mixedlayerphysics) THEN
      CALL randdepthinmixedlayer (ngrid,xold,yold,zold,r,n)
     ENDIF

     landFlag = .false.
!    calculate u,v,w,temp,saln,dens at position (xold,yold,zold)
     CALL rungakutta(ngrid,xold,yold,zold,run_time,diam, &
          dens,1,u1,v1,w1,ztemp,zsaln,zdens,flag,landFlag,&
          r,n,grid_i,grid_j,grid_k)

     IF(flag(1) .and. flag(2)) THEN
      goto 100
     ENDIF
     IF (landFlag) THEN
      flag(1) = .true.
      flag(2) = .true.
      goto 100
     ENDIF
      
!    backward
     IF (backward) THEN
      u1 = -1 * u1
      v1 = -1 * v1
      w1 = -1 * w1
     ENDIF

!    calculate new position      
     CALL updateloc(xold,yold,zold,u1,v1,w1,flag,hh,xtmp1,ytmp1,ztmp1,upperdepth)

!     ================== Runga Kutta Step 2 ==================

!     check in which nest the particle is  
      CALL findnest(xtmp1,ytmp1,ztmp1,ngrid,r,n)
!     IF particle is outside all nests then stop moving 
      IF (ngrid .eq. -1) THEN
        flag(7) = .True.
        goto 100
      ENDIF

      landFlag = .false.
!     calculate u,v,w,temp,saln,dens at position (xtmp1,ytmp1,ztmp1)
      CALL rungakutta(ngrid,xtmp1,ytmp1,ztmp1,run_time,diam, &
           dens,2,u2,v2,w2,ztemp,zsaln,zdens,flag,landFlag, &
           r,n,grid_i,grid_j,grid_k)

      IF (flag(1) .and. flag(2)) THEN
        goto 100
      ENDIF

!     backward
      IF (backward) THEN
       u2 = -1 * u2
       v2 = -1 * v2
       w2 = -1 * w2
      ENDIF

!     calculate new position 
      CALL updateloc(xold,yold,zold,u2,v2,w2,flag,hh,xtmp2,ytmp2,ztmp2,upperdepth) 

!     ================== Runga Kutta Step 3 ==================

      CALL findnest(xtmp2,ytmp2,ztmp2,ngrid,r,n)
      IF (ngrid .eq. -1) THEN
        flag(7) = .True.
        goto 100
      ENDIF

      landFlag = .false.
      CALL rungakutta(ngrid,xtmp2,ytmp2,ztmp2,run_time,diam, &
           dens,3,u3,v3,w3,ztemp,zsaln,zdens,flag,landFlag, &
           r,n,grid_i,grid_j,grid_k)

      IF(flag(1) .and. flag(2)) THEN
        goto 100
      ENDIF

!     backward
      IF (backward) THEN
       u3 = -1 * u3
       v3 = -1 * v3
       w3 = -1 * w3
      ENDIF
 
!     calculate new position 
      CALL updateloc(xold,yold,zold,u3,v3,w3,flag,h1,xtmp3,ytmp3,ztmp3,upperdepth)

!     ================== Runga Kutta Step 4 ==================

      CALL findnest(xtmp3,ytmp3,ztmp3,ngrid,r,n)
      IF (ngrid .eq. -1) THEN
        flag(7) = .True.
        goto 100
      ENDIF 

      landFlag = .false.
      CALL rungakutta(ngrid,xtmp3,ytmp3,ztmp3,run_time,diam, &
           dens,4,u4,v4,w4,ztemp,zsaln,zdens,flag,landFlag, &
           r,n,grid_i,grid_j,grid_k)

      IF(flag(1) .and. flag(2)) THEN
        goto 100
      ENDIF

!     backward
      IF (backward) THEN
       u4 = -1 * u4
       v4 = -1 * v4
       w4 = -1 * w4
      ENDIF 

!     ================== Calculate new position  ==================
!     best estimate for u,v,w during movement
      uf=(u1+u4+2.0*(u3+u2))/6.0
      vf=(v1+v4+2.0*(v3+v2))/6.0
      wf=(w1+w4+2.0*(w3+w2))/6.0

!     Adding turb component  
      IF (turb) THEN
       IF (mod(time_t,turbTimestep) .eq. 0) THEN
        CALL calc_turb(real(timestep),horDiff(ngrid), vertDiff(ngrid),uturb,vturb, wturb)
        uf = uf + uturb
        vf = vf + vturb
        IF (wf .ne. 0.) THEN
         wf = wf + wturb
        ENDIF
!       print *, "u en v after turb", u,v
       ENDIF
      ENDIF

!     calculate new position 
      CALL updateloc(xold,yold,zold,uf,vf,wf,flag,h1,xnew,ynew,znew,upperdepth)

!     check if new position is inside grid
      CALL findnest(xnew,ynew,znew,ngrid,r,n)
      IF (ngrid .eq. -1) THEN
        flag(7) = .True.
        goto 100
      ENDIF

!     Check if new position is on land. If flag avoidcoast is turned on 
!     then this is not allowed so the particle has to move to a position in not on land 
!     if particle is on land then check only movement with u and w
!     if particle is on land then check only movement with v and w
!     if particle is on land then check only movement with u and v
!     if particle is on land then do not move the particle
      if ((avoidCoast) .or. (znew .gt. 0)) THEN
!      check if particle is on land
       landFlag = .true.
       CALL rungakutta(ngrid,xnew,ynew,znew,run_time,diam, & 
            dens,1,un,vn,wn,tn,sn,rn,flag,landFlag, &
            r,n,grid_i,grid_j,grid_k)
         IF (landFlag .eqv. .true.) THEN
!         if particle is on land then move particle only with u and w
          CALL updateloc(xold, yold, zold, uf,0.,wf,flag, h1, xnew, ynew, znew,upperdepth)
!         check if particle is on land
          CALL rungakutta(ngrid,xnew,ynew,znew,run_time,diam, &
               dens,1,un,vn,wn,tn,sn,rn,flag,landFlag, &
               r,n,grid_i,grid_j,grid_k)
          IF (landFlag .eqv. .true.) THEN
!          if particle is on land then move particle only with v and w
           CALL updateloc(xold, yold, zold, 0.,vf,wf,flag, h1, xnew, ynew, znew,upperdepth)
!          check if particle is on land
           CALL rungakutta(ngrid,xnew,ynew,znew,run_time,diam, &
                dens,1,un,vn,wn,tn,sn,rn,flag,landFlag, &
                r,n,grid_i,grid_j,grid_k)
           IF (landFlag .eqv. .true.) THEN
!           if particle is on land then move particle only with u and v
            CALL updateloc(xold, yold, zold, uf,vf,0.,flag, h1, xnew, ynew, znew,upperdepth)
!           check if particle is on land
            CALL rungakutta(ngrid,xnew,ynew,znew,run_time,diam, &
                 dens,1,un,vn,wn,tn,sn,rn,flag,landFlag, &
                 r,n,grid_i,grid_j,grid_k)
            IF (landFlag .eqv. .true.) THEN
!             if particle is on land then do not move the particle
              xnew = xold
              ynew = yold
              znew = zold
            ENDIF
           ENDIF
          ENDIF
         ENDIF
      ENDIF

      IF (ibio) THEN
!      Move particle with vertical matrix
!      Particle can only go up or down one layer a time step. 
       IF (mod(time_t,ibioTimestep) .eq. 0) THEN
        CALL change_layer(run_time,massSpawning,larvaStart,layerNew)
        IF (layerNew .gt. particle(r)%layer(n)) THEN
!        move one layer down
         particle(r)%layer(n) = particle(r)%layer(n) + 1
!        check if particle is below bottom of the ocean
         landFlag = .true.
         CALL rungakutta(ngrid,xnew,ynew,vertLayers(particle(r)%layer(n)), &
             run_time,diam,dens,1,un,vn,wn,tn,sn,rn,flag,landFlag, &
             r,n,grid_i,grid_j,grid_k)
         IF (landFlag .eqv. .true.) THEN
!           if particle is below bottom of the ocean, move one layer up
            particle(r)%layer(n) = particle(r)%layer(n) - 1 
         ENDIF
        ELSE IF (layerNew .lt. particle(r)%layer(n)) THEN
!        move one layer up
         particle(r)%layer(n) = particle(r)%layer(n) - 1
        ENDIF
!       calculate depth of new layer
        particle(r)%ndepth(n) = vertLayers(particle(r)%layer(n))
       ENDIF
      ELSE
!      else give the particle his new depth position	   
       particle(r)%ndepth(n)=znew    
      ENDIF
!     give the particle his new lon and lat position
      particle(r)%nlon(n)=xnew
      particle(r)%nlat(n)=ynew

!     interpolate temperature onto particle location if required
      IF (outputtemp) THEN
       CALL rungakutta(ngrid,xnew,ynew,znew,run_time,diam, &
           dens,1,un,vn,wn,tn,sn,rn,flag,landFlag, &
           r,n,grid_i,grid_j,grid_k)
       particle(r)%temp(n)=tn 
      ENDIF

!     interpolate salinity onto particle location if required
      IF (outputsaln) THEN
       CALL rungakutta(ngrid,xnew,ynew,znew,run_time,diam, &
           dens,1,un,vn,wn,tn,sn,rn,flag,landFlag, &
           r,n,grid_i,grid_j,grid_k)
       particle(r)%saln(n)=sn 
      ENDIF

!     dying particles
      IF (mort) THEN
       flagMort = .false.
       CALL add_mort(h1,particle(r)%halflife(n),flagMort)
       IF (flagMort .eqv. .true.) THEN     
         flag(8) = .True.
         goto 100
       ENDIF
      ENDIF

100   particle(r)%flag(n,:) = flag
  
    ENDIF
   ENDDO
  ENDIF
 ENDDO

END SUBROUTINE move



