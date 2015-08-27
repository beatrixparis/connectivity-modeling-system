!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : getphysicaldata.f90                                               *
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

!read data netcdf files (given by getfilenames.f90)
SUBROUTINE getphysicaldata(time_t, r)

 USE constants
 USE globalvariables
 USE mod_kinds
 USE mod_netcdf
 USE mod_calendar
 USE mod_nciorw

 IMPLICIT NONE

 integer (kind=int_kind), intent(in)  :: r
 integer (kind=int8_kind), intent(in) :: time_t

 integer (kind=int_kind)         :: ncID,i,idm,jdm,kdm,frac_tstep,numberFiles,dmonth, yyyy, dd, mm, &
                                     extradays, jj,fileNumber
 integer (kind=int8_kind)        :: lc, ss, startFile, tstep_save
 real (kind = real_kind)         :: tstep, x,x1, x_save  
 logical (kind=log_kind)         :: file_exists, readdata, firsttime
 logical (kind=log_kind), save   :: wExist, densExist, tempExist, salExist,sshExist
 character(char_len),allocatable :: fname(:)

 DO i=1,nnests
  firsttime = .false.

  !if time_unit is months then interpolate between 4 files
  !if time_unit is days or seconds then interpolate between 2 files
  !for both options is one extra file needed because of the runga kutta 4 method in time is used
  IF (nests(i)%time_units == "months") THEN 
   allocate(fname(5))
   numberFiles=5
  ELSE
   allocate(fname(3))
   numberFiles=3
  ENDIF

  idm=nests(i)%idm
  jdm=nests(i)%jdm
  kdm=nests(i)%kdm
  lc = time_t
!  print *, "lc: ", lc

! generate filenames to load
  IF (nests(i)%time_units == "months") THEN   
   CALL getflnmMonthly(i,r,lc,fname)
  ELSE   
   CALL getflnm(i,r,lc,fname)
  ENDIF
!  print *, "Filenames: ",fname

! check if all the filenames exist 
  nests(i)%dataExist = .true.
  DO fileNumber = 1,numberFiles  
   INQUIRE(FILE=fname(fileNumber), EXIST=file_exists)
   IF (file_exists .eqv. .false.) THEN
    nests(i)%dataExist = .false.
   ENDIF
  ENDDO

! only read data from files if not done before
  IF (nests(i)%fnameold .eq. "") THEN
     firsttime = .true.
  ENDIF
  readdata = .true.
  IF (fname(1) .eq. nests(i)%fnameold) then
    readdata = .false.
  ENDIF
  nests(i)%fnameold = fname(1)
!  print *, "Read data: ", readdata   


  IF (readdata .and. nests(i)%dataExist) then
   wExist = .false.
   densExist = .false.
   tempExist = .false.
   salExist = .false.


!  only download the last filename 
!  except if it is the first time that data is downloaded
   IF (firsttime) then
     startFile = 1
   ELSE
     startFile = numberFiles
     DO fileNumber = 1,numberFiles-1  
        nests(i)%uvel(:,:,:,fileNumber) = nests(i)%uvel(:,:,:,fileNumber+1)
        nests(i)%vvel(:,:,:,fileNumber) = nests(i)%vvel(:,:,:,fileNumber+1)
        nests(i)%wvel(:,:,:,fileNumber) = nests(i)%wvel(:,:,:,fileNumber+1)
        nests(i)%dens(:,:,:,fileNumber) = nests(i)%dens(:,:,:,fileNumber+1)
        nests(i)%temp(:,:,:,fileNumber) = nests(i)%temp(:,:,:,fileNumber+1)
        nests(i)%saln(:,:,:,fileNumber) = nests(i)%saln(:,:,:,fileNumber+1)
        nests(i)%ssh(:,:,fileNumber) = nests(i)%ssh(:,:,fileNumber+1)
     ENDDO
   ENDIF

!  Get data from all the different filenames
   DO fileNumber = startFile,numberFiles        
!   open file
    CALL nc_open(trim(fname(fileNumber)),ncId)       

!   read U-velocity
    CALL nc_read4d(fname(fileNumber), ncId, "zu",nests(i)%uvel(:,:,:,fileNumber),idm,jdm,kdm,1)
!   read V-velocity
    CALL nc_read4d(fname(fileNumber), ncId, "zv",nests(i)%vvel(:,:,:,fileNumber),idm,jdm,kdm,1)
!   read W-velocity
    IF (nc_exists(fname(fileNumber), ncId, "zw")) THEN
     CALL nc_read4d(fname(fileNumber), ncId, "zw",nests(i)%wvel(:,:,:,fileNumber),idm,jdm,kdm,1)
     wExist = .true.
    ENDIF
!   read density
    IF (nc_exists(fname(fileNumber), ncId, "zd")) THEN
     CALL nc_read4d(fname(fileNumber), ncId, "zd",nests(i)%dens(:,:,:,fileNumber),idm,jdm,kdm,1)
     densExist = .true.
    ENDIF
!   read temperature
    IF (nc_exists(fname(fileNumber), ncId, "zt")) THEN
     CALL nc_read4d(fname(fileNumber), ncId, "zt",nests(i)%temp(:,:,:,fileNumber),idm,jdm,kdm,1)
     tempExist = .true.
    ENDIF
!   read salinity
    IF (nc_exists(fname(fileNumber), ncId, "zs")) THEN
     CALL nc_read4d(fname(fileNumber), ncId, "zs",nests(i)%saln(:,:,:,fileNumber),idm,jdm,kdm,1)
     salExist = .true.
    ENDIF
!   read sea surface height
    IF (nc_exists(fname(fileNumber), ncId, "ssh")) THEN
     CALL nc_read3d(fname(fileNumber), ncId, "ssh",nests(i)%ssh(:,:,fileNumber),idm,jdm,1)
     sshExist = .true.
    ENDIF

!   close file
    CALL nc_close(fname(fileNumber), ncId) 

!   call theta to get density
    IF (densExist .eqv. .false.) THEN
     IF (tempExist .and. salExist) THEN
       CALL calc_dens(nests(i)%temp(:,:,:,fileNumber),nests(i)%saln(:,:,:,fileNumber),nests(i)%dens(:,:,:,fileNumber), idm,jdm,kdm)
     ENDIF
    ENDIF
   ENDDO !end loop: do fileNumber = 1,numberFiles  
  ENDIF
        
! checks if ssh exists to calculate tidal movement
  IF (tidalMovement) THEN
   IF (sshExist .eqv. .false.) THEN
    print *, "Error: Missing ssh in the nest files. The ssh is necessary if cktidalmovement is set to true"
    stop
   ENDIF
  ENDIF


 nextFile = .false.

 !calculate the weights for interpolation
 IF (nests(i)%time_units == "months") THEN
 
! calculate weights for rk step 1
  yyyy=particle(r)%year
  mm=particle(r)%month
  dd=particle(r)%day
  ss = lc
  extradays = lc/secs_in_day
  ss = mod(ss,secs_in_day);
  CALL jd(yyyy, mm, dd, jj)
  IF (backward) THEN
   jj = jj - extradays
  ELSE
   jj = jj + extradays
  ENDIF
  CALL cdate(jj, yyyy, mm, dd)
  IF (dd .ge. nests(i)%tstart_dd) THEN
      CALL DaysMonth (yyyy,mm,dmonth)
      lc  = dd - nests(i)%tstart_dd
  ELSE
     CALL DaysMonth (yyyy,mm-1,dmonth)
     lc = dmonth - nests(i)%tstart_dd + dd
  ENDIF
  x = mod((lc + (real(ss)/real(secs_in_day))) /real(dmonth),1.)
  x_save = x
  x1 = 1.-x
  IF (backward) THEN
  nests(i)%w(1,3)=x1*(1.+x *(1.-1.5*x ))
  nests(i)%w(1,2)=x *(1.+x1*(1.-1.5*x1))
  nests(i)%w(1,1)=-.5*x1*x *x
  nests(i)%w(1,4)=-.5*x *x1*x1
  ELSE
  nests(i)%w(1,2)=x1*(1.+x *(1.-1.5*x ))
  nests(i)%w(1,3)=x *(1.+x1*(1.-1.5*x1))
  nests(i)%w(1,4)=-.5*x1*x *x
  nests(i)%w(1,1)=-.5*x *x1*x1
  ENDIF

! calculate weights for rk step 2+3
  lc = time_t + 0.5 * timestep
  yyyy=particle(r)%year
  mm=particle(r)%month
  dd=particle(r)%day
  ss = lc
  extradays = lc/secs_in_day
  ss = mod(ss,secs_in_day);
  CALL jd(yyyy, mm, dd, jj)
  IF (backward) THEN
   jj = jj - extradays
  ELSE
   jj = jj + extradays
  ENDIF
  CALL cdate(jj, yyyy, mm, dd)
  IF (dd .ge. nests(i)%tstart_dd) THEN
      CALL DaysMonth (yyyy,mm,dmonth)
      lc  = dd - nests(i)%tstart_dd
  ELSE
     CALL DaysMonth (yyyy,mm-1,dmonth)
     lc = dmonth - nests(i)%tstart_dd + dd
  ENDIF
  x = mod((lc + (real(ss)/real(secs_in_day))) /real(dmonth),1.)
  x1 = 1.-x
  IF (backward) THEN
  nests(i)%w(2,3)=x1*(1.+x *(1.-1.5*x ))
  nests(i)%w(2,2)=x *(1.+x1*(1.-1.5*x1))
  nests(i)%w(2,1)=-.5*x1*x *x
  nests(i)%w(2,4)=-.5*x *x1*x1
  nests(i)%w(3,:)=nests(i)%w(2,:)
  ELSE
  nests(i)%w(2,2)=x1*(1.+x *(1.-1.5*x ))
  nests(i)%w(2,3)=x *(1.+x1*(1.-1.5*x1))
  nests(i)%w(2,4)=-.5*x1*x *x
  nests(i)%w(2,1)=-.5*x *x1*x1
  nests(i)%w(3,:)=nests(i)%w(2,:)
  ENDIF
  IF (x .lt. x_save) THEN
   nextFile(2) = .true.
   nextFile(3) = .true.
  ENDIF
 

! calculate weights for rk step 4
  lc = time_t + timestep
  yyyy=particle(r)%year
  mm=particle(r)%month
  dd=particle(r)%day
  ss = lc
  extradays = lc/secs_in_day
  ss = mod(ss,secs_in_day);
  CALL jd(yyyy, mm, dd, jj)
  IF (backward) THEN
   jj = jj - extradays
  ELSE
   jj = jj + extradays
  ENDIF
  CALL cdate(jj, yyyy, mm, dd)
  IF (dd .ge. nests(i)%tstart_dd) THEN
      CALL DaysMonth (yyyy,mm,dmonth)
      lc  = dd - nests(i)%tstart_dd
  ELSE
     CALL DaysMonth (yyyy,mm-1,dmonth)
     lc = dmonth - nests(i)%tstart_dd + dd
  ENDIF
  x = mod((lc + (real(ss)/real(secs_in_day))) /real(dmonth),1.)
  x1 = 1.-x
  IF (backward) THEN
  nests(i)%w(4,3)=x1*(1.+x *(1.-1.5*x ))
  nests(i)%w(4,2)=x *(1.+x1*(1.-1.5*x1))
  nests(i)%w(4,1)=-.5*x1*x *x
  nests(i)%w(4,4)=-.5*x *x1*x1
  ELSE
  nests(i)%w(4,2)=x1*(1.+x *(1.-1.5*x ))
  nests(i)%w(4,3)=x *(1.+x1*(1.-1.5*x1))
  nests(i)%w(4,4)=-.5*x1*x *x
  nests(i)%w(4,1)=-.5*x *x1*x1
  ENDIF
  IF (x .lt. x_save) THEN
   nextFile(4) = .true.
  ENDIF

!  print *, "weight runge kutta step 1: ", nests(i)%w(1,:)
!  print *, "weight runge kutta step 2: ", nests(i)%w(2,:)
!  print *, "weight runge kutta step 3: ", nests(i)%w(3,:)
!  print *, "weight runge kutta step 4: ", nests(i)%w(4,:)
 ELSE

! calculate weights for rk step 1
  tstep=real(lc)/real(nests(i)%time_step)
  tstep_save = int(tstep)
  frac_tstep=(tstep - (lc/nests(i)%time_step))*nests(i)%time_step
  nests(i)%w(1,1) = ((real(frac_tstep)-real(nests(i)%time_step)) /(-real(nests(i)%time_step)))
  nests(i)%w(1,2) = ((real(frac_tstep))/(real(nests(i)%time_step)))
! calculate weights for rk step 2 + 3
  lc = time_t + 0.5 * timestep
  tstep=real(lc)/real(nests(i)%time_step)
  frac_tstep=(tstep - (lc/nests(i)%time_step))*nests(i)%time_step
  nests(i)%w(2,1) = ((real(frac_tstep)-real(nests(i)%time_step)) /(-real(nests(i)%time_step)))
  nests(i)%w(2,2) = ((real(frac_tstep))/(real(nests(i)%time_step)))
  nests(i)%w(3,1) = nests(i)%w(2,1)
  nests(i)%w(3,2) = nests(i)%w(2,2)
  IF (int(tstep) .gt. tstep_save) THEN
      nextFile(2) = .true.
      nextFile(3) = .true.
  ENDIF
! calculate weights for rk step 4
  lc = time_t + timestep
  tstep=real(lc)/real(nests(i)%time_step)
  frac_tstep=(tstep - (lc/nests(i)%time_step))*nests(i)%time_step
  nests(i)%w(4,1) = ((real(frac_tstep)-real(nests(i)%time_step)) /(-real(nests(i)%time_step)))
  nests(i)%w(4,2) = ((real(frac_tstep))/(real(nests(i)%time_step)))
  IF (int(tstep) .gt. tstep_save) THEN
      nextFile(4) = .true.
  ENDIF
!  print *, "weight runge kutta step 1: ", nests(i)%w(1,:)
!  print *, "weight runge kutta step 2: ", nests(i)%w(2,:)
!  print *, "weight runge kutta step 3: ", nests(i)%w(3,:)
!  print *, "weight runge kutta step 4: ", nests(i)%w(4,:)
 ENDIF

 deallocate(fname)

ENDDO !end loop: do i=1,nnests

END SUBROUTINE getphysicaldata

