!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : output.f90                                                        *
!* Last Modified: 2012-03-20                                                *
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

 SUBROUTINE init_trajfile(locname, startR, endR)

 USE constants
 USE globalvariables
 USE mod_kinds
 USE mod_netcdfoutput
 USE mod_iounits

 IMPLICIT NONE
    
 character(Len=*),intent(in)         :: locname
 integer (kind=int_kind), intent(in) :: startR, endR 
 integer (kind=int_kind) :: r
 integer (kind=int_kind) :: numParticles, numTime,numTime2, numLoc

!calculate number of release locations and time steps of each output file
!Total number of particles released
 numParticles = 0
 DO r = startR, endR
   numParticles = numParticles + particle(r)%num_rel
 ENDDO
!Total number of time steps in output file
 numTime = ceiling(real(timeMax)/real(outputFreq)) + 1
!Total number of timesteps
 numTime2 = int(total_seconds/timeStep) + 1
!Total number release locations
 numLoc  = (endR - startR) + 1

!create trajectory output file
 IF (ascii) THEN   
  trajname = "traj_"//trim(locName)   
  CALL get_unit(iunit_traj)
  open (unit=iunit_traj,file=trim(filescratch)//"traj_"//trim(locName),status="unknown")
 ELSE
  trajname = "traj_"//trim(locName)//".nc"
  CALL output_create (trim(filescratch)//trim(trajname), numTime)    
 ENDIF

!print information to screen
 print *, "=========================================="
 print *, "File name  : ", trim(trajname)
 print '(A,I0,A,I0,A,I0,A)', & 
   " Total number release locations     : ",numLoc," (",startR," to ",endR,")"
 print '(A,I0)', &
   " Total number of particles released : ",numParticles
 print '(A,I0)', &
   " Total number of time steps   : ",numTime2
 print '(A,I0)', &
   " Total number of time steps in output file: ",numTime
    
END SUBROUTINE init_trajfile

!**************************************************************
SUBROUTINE init_confile(locname)
 USE constants
 USE globalvariables
 USE mod_kinds
 USE mod_iounits

 IMPLICIT NONE
  
 character(Len=*),intent(in) :: locname
 conname = "con_"//trim(locName)
 CALL get_unit(iunit_con)   
 open (unit=iunit_con,file=trim(filescratch)//trim(conname),status="unknown")

END SUBROUTINE init_confile

!**************************************************************

SUBROUTINE close_trajfile
 USE globalvariables
 USE mod_netcdfoutput
 USE mod_iounits

 IMPLICIT NONE

!close file
 IF (ascii) THEN 
    CALL release_unit(iunit_traj) 
 ELSE
    CALL output_close(trajname) 
 ENDIF

END SUBROUTINE close_trajfile

!**************************************************************
SUBROUTINE close_confile
 USE globalvariables
 USE mod_iounits

 IMPLICIT NONE

!close file
 CALL release_unit(iunit_con)   

END SUBROUTINE close_confile

!**************************************************************

SUBROUTINE stateout_trajfile_netcdf(n,r,run_time,time_t,exitCode,startR)    
 USE mod_kinds
 USE constants
 USE mod_calendar
 USE globalvariables
 USE mod_netcdfoutput

 IMPLICIT NONE

 integer (kind=int_kind), intent(in)  :: r, exitCode, n, startR
 integer (kind=int8_kind), intent(in) :: time_t, run_time

 real (kind = real_kind) :: cdist,nlon,nlat,ndepth, &
     ilon,ilat,idepth,diam,dens,dist, sec, temp, saln
 double precision        :: relDate
 integer (kind=int_kind) :: particleId, timeId,julian,i, poly, inmld
 character(char_len)     :: locname
 logical (kind=log_kind) :: sync

 nlon=particle(r)%nlon(n)
 nlat=particle(r)%nlat(n)
 ndepth=particle(r)%ndepth(n)
 IF ((tidalMovement) .and. (notmove)) THEN
   ndepth = -999.99
 ENDIF
 ilon=particle(r)%ilon
 ilat=particle(r)%ilat
 idepth=particle(r)%idepth
 IF ((buoyancy) .or. (diffpart)) THEN
  diam=particle(r)%diam(n)
  dens=particle(r)%density(n)
  temp=particle(r)%temp(n)
  saln=particle(r)%saln(n)
 ELSE
  diam=-1
  dens=-1
 ENDIF
 poly = particle(r)%id
 locname = particle(r)%rel_loc_name

!calculate the time Id
 IF (run_time .eq. 0) THEN
    timeId = 1
 ELSE
    timeId = int(run_time/outputFreq)+1
 ENDIF

!calculate distance
 IF (withibm) THEN
  IF (run_time .eq. 0) THEN
      cdist = 0.
  ELSE
    CALL Distance_Sphere(particle(r)%old_lonDist(n), &
     particle(r)%old_latDist(n),nlon,nlat, cdist)
  ENDIF 
  particle(r)%dist(n) = particle(r)%dist(n) + cdist
  dist = particle(r)%dist(n)  
 ELSE
  dist = -1
 ENDIF

!set the first temperature to NaN (because it's not know yet)
 IF (outputtemp .and. run_time .eq. 0) THEN
      temp = 2.0**100
 ENDIF

!set the first salinity to NaN (because it's not know yet)
 IF (outputsaln .and. run_time .eq. 0) THEN
      saln = 2.0**100
 ENDIF

!calculate releasedate
 CALL jd(particle(r)%year,particle(r)%month,particle(r)%day,julian) 
 sec =  ((real(particle(r)%seconds)/8640.)*0.1) 
 relDate = dble(julian) + dble(sec)

!calculate the particle Id
 particleId = 0
 DO i = startR, (r-1)
   particleId = particleId + particle(i)%num_rel
 ENDDO
 particleId = particleId + n

!sync data if 30 timesteps are written to output file
 sync = .false.
 IF (mod(int(real(time_t)/real(outputFreq)),30) .eq. 0 .and. (saveTime .ne. time_t)) THEN
   saveTime = time_t
   sync = .true.
 ENDIF

!save whether in mixedlayer or not
 IF (mixedlayerphysics) THEN
  IF (run_time .eq. 0) THEN
   inmld = 100
  ELSE
   IF (particle(r)%inmld(n) .eqv. .true.) THEN
    inmld = 1
   ELSE
    inmld = 0
   ENDIF
  ENDIF
 ELSE
   inmld = -1
 ENDIF

!write output
 CALL output_write_loc(trajname,particleId,timeId,r,run_time,nlon,nlat,ndepth, &
   dist,exitCode,relDate,poly,dens,diam,temp,saln,sync,polygon,buoyancy, &
   massSpawning,withibm,outputtemp,outputsaln,inmld)

 IF (withibm) THEN
!save current lon and lat
  particle(r)%old_lonDist(n) = particle(r)%nlon(n)
  particle(r)%old_latDist(n) = particle(r)%nlat(n)
 ENDIF

END SUBROUTINE stateout_trajfile_netcdf

!**************************************************************

SUBROUTINE stateout_trajfile_ascii(n, r,run_time,exitCode)
 USE constants
 USE globalvariables
 USE mod_kinds
 USE mod_calendar

 IMPLICIT NONE

 integer (kind=int_kind), intent(in)  :: r,exitCode,n
 integer (kind=int8_kind), intent(in) :: run_time

 real (kind = real_kind) :: cdist,dist,nlon,nlat,ndepth, &
     ilon,ilat,idepth,diam_part,dens_part, sec
 character(char_len)     :: locname
 double precision        :: relDate
 integer (kind=int_kind) :: julian, poly

 nlon=particle(r)%nlon(n)
 nlat=particle(r)%nlat(n)
 ndepth=particle(r)%ndepth(n)
 IF ((tidalmovement) .and. (notmove)) THEN
   ndepth = -999.99
 ENDIF
 ilon=particle(r)%ilon
 ilat=particle(r)%ilat
 idepth=particle(r)%idepth
 IF ((buoyancy) .or. (diffpart)) THEN
  diam_part=particle(r)%diam(n)
  dens_part=particle(r)%density(n)
 ELSE
  diam_part=-1
  dens_part=-1
 ENDIF
 poly = particle(r)%id
 locname = particle(r)%rel_loc_name

 
!calculate distance
 IF (withibm) THEN
  IF (run_time .eq. 0) THEN
      cdist = 0.
  ELSE
    CALL Distance_Sphere(particle(r)%old_lonDist(n), &
      particle(r)%old_latDist(n),nlon,nlat, cdist)
  ENDIF 
  particle(r)%dist(n) = particle(r)%dist(n) + cdist
  dist = particle(r)%dist(n)  
 ELSE
  dist = -1
 ENDIF

!calculate releasedate
 CALL jd(particle(r)%year,particle(r)%month,particle(r)%day,julian) 
 sec =  ((real(particle(r)%seconds)/8640.)*0.1) 
 relDate = dble(julian) + dble(sec)
    
!write output  
 IF (buoyancy .or. massspawning) THEN
   IF (polygon) THEN
    write(iunit_traj,'(i8,1x,i8,1x,i12,1x,F8.3,1x,F8.3,1x,F8.3,1x,F12.3,1x,i2,1x,f12.3,1x,i4,1x,F6.1,1x,F9.7)') &
      r, n,run_time,nlon,nlat,ndepth,dist,exitCode,relDate,poly,dens_part,diam_part
   ELSE
    write(iunit_traj,'(i8,1x,i8,1x,i12,1x,F8.3,1x,F8.3,1x,F8.3,1x,F12.3,1x,i2,1x,f12.3,1x,F6.1,1x,F9.7)') &
      r, n,run_time,nlon,nlat,ndepth,dist,exitCode,relDate,dens_part,diam_part
   ENDIF

 ELSE
   IF (polygon) THEN
    write(iunit_traj,'(i8,1x,i8,1x,i12,1x,F8.3,1x,F8.3,1x,F8.3,1x,F12.3,1x,i2,1x,f12.3,1x,i4)') &
      r, n,run_time,nlon,nlat,ndepth,dist,exitCode,relDate, poly
   ELSE
     IF (withibm) THEN
      write(iunit_traj,'(i8,1x,i8,1x,i12,1x,F8.3,1x,F8.3,1x,F8.3,1x,F12.3,1x,i2,1x,f12.3)') &
        r, n,run_time,nlon,nlat,ndepth,dist,exitCode,relDate
     ELSE
      write(iunit_traj,'(i8,1x,i12,1x,F8.3,1x,F8.3,1x,F8.3,1x,i2)') &
        r,run_time,nlon,nlat,ndepth,exitCode
     ENDIF
   ENDIF
 ENDIF 

!save current lon and lat
 IF (withibm) THEN
  particle(r)%old_lonDist(n) = particle(r)%nlon(n)
  particle(r)%old_latDist(n) = particle(r)%nlat(n)
 ENDIF

END SUBROUTINE stateout_trajfile_ascii

!**************************************************************

SUBROUTINE stateout_confile(r, retentionPoly, yearStart, monthStart, dayStart, &
                            yearEnd, monthEnd, dayEnd, run_time, depthEnd)
 USE constants
 USE globalvariables
 USE mod_kinds


 IMPLICIT NONE

 real (kind = real_kind), intent(in)  :: depthEnd
 integer (kind=int8_kind), intent(in) :: run_time
 integer (kind=int_kind),intent(in)   :: r, retentionPoly, yearEnd, monthEnd, dayEnd, &
                                         yearStart, monthStart, dayStart
  
 
 write(iunit_con,'(I5,1x,I5,1x,I5,1x,I2,1x,I2,1x,I12,1x,f6.1,1x,I5,1x,I2,1x,I2)') & 
     particle(r)%id,retentionPoly, yearEnd, monthEnd, dayEnd, run_time, depthEnd, & 
     yearStart, monthStart, dayStart

END SUBROUTINE stateout_confile

!**************************************************************
!**************************************************************
SUBROUTINE stateout_confile_strata(r, retentionPoly, yearStart, monthStart, dayStart, &
                            yearEnd, monthEnd, dayEnd, run_time, depthEnd, strataEnd)
 USE constants
 USE globalvariables
 USE mod_kinds


 IMPLICIT NONE

 real (kind = real_kind), intent(in)  :: depthEnd
 integer (kind=int8_kind), intent(in) :: run_time
 integer (kind=int_kind),intent(in)   :: r, retentionPoly, yearEnd, monthEnd, dayEnd, &
                                         yearStart, monthStart, dayStart, strataEnd
  

 write(iunit_con,'(I5,1x,I5,1x,I5,1x,I2,1x,I2,1x,I12,1x,f6.1,1x,I5,1x,I2,1x,I2,1x,I2,1x,I2)') & 
     particle(r)%id,retentionPoly, yearEnd, monthEnd, dayEnd, run_time, depthEnd, & 
     yearStart, monthStart, dayStart, particle(r)%strataStart, strataEnd

END SUBROUTINE stateout_confile_strata

!**************************************************************



