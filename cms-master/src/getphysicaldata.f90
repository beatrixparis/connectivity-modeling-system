!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : getphysicaldata.f90                                               *
!* Last Modified: 2016-07-25                                                *
!* Code contributors: Claire B Paris, Ana Carolina Vaz, Judith Helgers,     * 
!*                    Ashwanth Srinivasan, Erik van Sebille                 *
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
SUBROUTINE getphysicaldata(time_t, basedate)

 USE constants
 USE globalvariables
 USE mod_kinds
 USE mod_netcdf
 USE mod_calendar
 USE mod_nciorw

 IMPLICIT NONE

 integer (kind=int8_kind), intent(in) :: time_t
 integer (kind=int_kind), intent(in)  :: basedate
 integer (kind=int_kind)         :: ncId, i, frac_tstep, numberFiles, dmonth, &
                                    yyyy, dd, mm, &
                                    extradays, jj,fileNumber, ncIdmld
 integer (kind=int8_kind)        :: lc, ss, startFile, tstep_save
 real (kind = real_kind)         :: tstep, x,x1, x_save  
 logical (kind=log_kind)         :: file_exists, readdata, firsttime
 logical (kind=log_kind), save   :: wExist, densExist, tempExist, salExist, &
                                    sshExist, mldExist
 character(char_len),allocatable :: fname(:,:)
 character(char_len)             :: fnamemld

 DO i=1,nnests
  firsttime = .false.
  !if time_unit is months then interpolate between 4 files
  !if time_unit is days or seconds then interpolate between 2 files
  !for both options is one extra file needed because of the runga kutta 
  !4 method in time is used
  IF (nests(i)%time_units == "months") THEN 
    allocate(fname(4,5))
    numberFiles=5
  ELSE
    allocate(fname(4,3))
    numberFiles=3
  ENDIF

  lc = time_t
! print *, "lc: ", lc
! generate filenames to load
  IF (nests(i)%time_units == "months") THEN   
   CALL getflnmMonthly(i,basedate,lc,fname)
  ELSE   
   CALL getflnm(i,basedate,lc,fname)
  ENDIF

! check if all the filenames exist 
  nests(i)%dataExist = .true.
  DO fileNumber = 1,numberFiles  
   INQUIRE(FILE=fname(UAx,fileNumber), EXIST=file_exists)
   IF (file_exists .eqv. .false.) THEN
    nests(i)%dataExist = .false.
   ENDIF
  ENDDO

! only read data from files if not done before
  IF (nests(i)%fnameold .eq. "") THEN
     firsttime = .true.
  ENDIF
  readdata = .true.
  IF (fname(UAx,1) .eq. nests(i)%fnameold) then
    readdata = .false.
  ENDIF
  nests(i)%fnameold = fname(UAx,1)
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
     snapshotvec(:) = 1
     IF (backward) THEN
       CALL nc_open(trim(fname(UAx,1)),ncId)
       CALL nc_info_dim2(fname(UAx,1),ncId,axorderT,totsnapshotsinfile)
       CALL nc_close(fname(UAx,1), ncId)
       snapshotvec(4) = totsnapshotsinfile+1
     ELSE
       snapshotvec(4) = 0
     ENDIF
   ELSE
     startFile = numberFiles
     DO fileNumber = 1,numberFiles-1  
        nests(i)%uvel(:,:,:,fileNumber) = nests(i)%uvel(:,:,:,fileNumber+1)
        nests(i)%vvel(:,:,:,fileNumber) = nests(i)%vvel(:,:,:,fileNumber+1)
        nests(i)%wvel(:,:,:,fileNumber) = nests(i)%wvel(:,:,:,fileNumber+1)
        nests(i)%mld(:,:,fileNumber) = nests(i)%mld(:,:,fileNumber+1)
        IF (buoyancy .or. diffpart .or. massspawning) THEN
          nests(i)%dens(:,:,:,fileNumber) = nests(i)%dens(:,:,:,fileNumber+1)
          nests(i)%temp(:,:,:,fileNumber) = nests(i)%temp(:,:,:,fileNumber+1)
          nests(i)%saln(:,:,:,fileNumber) = nests(i)%saln(:,:,:,fileNumber+1)
          nests(i)%ssh(:,:,fileNumber) = nests(i)%ssh(:,:,fileNumber+1)
        ENDIF
     ENDDO
   ENDIF

!  Get data from all the different filenames
   DO fileNumber = startFile,numberFiles        

    IF (backward) THEN
!    get the total number of snapshots in file for the next iteration (needs to
!    be done before for backwards)
     CALL nc_open(trim(fname(UAx,fileNumber)),ncId)       
     CALL nc_info_dim2(fname(UAx,fileNumber),ncId,axorderT,totsnapshotsinfile)
     CALL nc_close(fname(UAx,fileNumber), ncId) 
     snapshotvec(4) = snapshotvec(4) - 1
     IF (snapshotvec(4) .lt. 1) THEN
       IF (totsnapshotsinfile .gt. 1) THEN
         print *, 'resetting to last snapshot'
       ENDIF
       snapshotvec(4) = totsnapshotsinfile
     ENDIF
    ELSE
     snapshotvec(4) = snapshotvec(4) + 1
     IF (snapshotvec(4) .gt. totsnapshotsinfile) THEN
      IF (totsnapshotsinfile .gt. 1) THEN
       print *, 'resetting to first snapshot'
      ENDIF
      snapshotvec(4) = 1
     ENDIF
!    get the total number of snapshots in file for the next iteration (needs to
!    be done afterwards for forward)
     CALL nc_open(trim(fname(UAx,fileNumber)),ncId)       
     CALL nc_info_dim2(fname(UAx,fileNumber),ncId,axorderT,totsnapshotsinfile)
     CALL nc_close(fname(UAx,fileNumber), ncId) 
    ENDIF

    !print *, "Filenames: ",trim(fname(UAx,fileNumber)), snapshotvec(4)
!   read U-velocity
    CALL nc_open(trim(fname(UAx,fileNumber)),ncId)       
    CALL nc_read4d(fname(UAx,fileNumber), ncId, trim(nests(i)%uname), &
       nests(i)%uvel(:,:,:,fileNumber), nests(i)%idm(UAx), &
       nests(i)%jdm(UAx), nests(i)%kdm(UAx),1,snapshotvec)
    CALL nc_close(fname(UAx,fileNumber), ncId) 

!   read V-velocity
    CALL nc_open(trim(fname(VAx,fileNumber)),ncId)       
    CALL nc_read4d(fname(VAx,fileNumber), ncId, trim(nests(i)%vname), &
    nests(i)%vvel(:,:,:,fileNumber), nests(i)%idm(VAx), &
    nests(i)%jdm(VAx),nests(i)%kdm(VAx),1,snapshotvec)
    CALL nc_close(fname(VAx,fileNumber), ncId) 

!   read W-velocity
    IF (AxUsed(3).eqv..true.) THEN
      CALL nc_open(trim(fname(WAx,fileNumber)),ncId)       
      IF (nc_exists(ncId, trim(nests(i)%wname))) THEN
        CALL nc_read4d(fname(WAx,fileNumber), ncId, trim(nests(i)%wname), &
         nests(i)%wvel(:,:,:,fileNumber), nests(i)%idm(WAx), &
         nests(i)%jdm(WAx),nests(i)%kdm(WAx),1,snapshotvec)
        wExist = .true.
      ENDIF
      CALL nc_close(fname(WAx,fileNumber), ncId) 
    ENDIF

    IF ((withibm .or. outputtemp) .and. AxUsed(4)) then
      CALL nc_open(trim(fname(QAx,fileNumber)),ncId)       
!     read density
      IF (nc_exists(ncId, trim(nests(i)%densname))) THEN
        CALL nc_read4d(fname(QAx,fileNumber), ncId, trim(nests(i)%densname), &
         nests(i)%dens(:,:,:,fileNumber), nests(i)%idm(QAx), &
         nests(i)%jdm(QAx),nests(i)%kdm(QAx),1,snapshotvec)
        densExist = .true.
      ENDIF
!     read temperature
      IF (nc_exists(ncId, trim(nests(i)%tempname))) THEN
        CALL nc_read4d(fname(QAx,fileNumber), ncId, trim(nests(i)%tempname), &
         nests(i)%temp(:,:,:,fileNumber), nests(i)%idm(QAx), &
         nests(i)%jdm(QAx),nests(i)%kdm(QAx),1,snapshotvec)
        tempExist = .true.
      ENDIF
!     read salinity
      IF (nc_exists(ncId, trim(nests(i)%salnname))) THEN
        CALL nc_read4d(fname(QAx,fileNumber), ncId, trim(nests(i)%salnname), &
         nests(i)%saln(:,:,:,fileNumber), nests(i)%idm(QAx), &
         nests(i)%jdm(QAx),nests(i)%kdm(QAx),1,snapshotvec)
        salExist = .true.
      ENDIF
!     read sea surface height
      IF (nc_exists(ncId, "ssh")) THEN
        CALL nc_read3d(fname(QAx,fileNumber), ncId, "ssh", &
          nests(i)%ssh(:,:,fileNumber), nests(i)%idm(QAx), &
          nests(i)%jdm(QAx),1,snapshotvec)
          sshExist = .true.
        ENDIF
        CALL nc_close(fname(QAx,fileNumber), ncId) 
      ENDIF

      IF (mixedlayerphysics) THEN
!       read mixed layer depth
        mldExist = .false.
        fnamemld = adjustl(trim(fname(QAx,filenumber)))
        CALL nc_open(trim(fnamemld),ncIdmld)
        IF (nc_exists(ncIdmld, "mld")) THEN
          CALL nc_read3d(fnamemld, ncIdmld, "mld",nests(i)%mld(:,:,fileNumber), &
           nests(i)%idm(QAx),nests(i)%jdm(QAx),1,snapshotvec)
          mldExist = .true.
        ENDIF
        CALL nc_close(fnamemld, ncIdmld) 
      ENDIF

!   call theta to get density
    IF (densExist .eqv. .false.) THEN
     IF (tempExist .and. salExist) THEN
       CALL calc_dens(nests(i)%temp(:,:,:,fileNumber), &
         nests(i)%saln(:,:,:,fileNumber),nests(i)%dens(:,:,:,fileNumber), &
         nests(i)%idm(QAx),nests(i)%jdm(QAx),nests(i)%kdm(QAx))
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
! checks if mld exists
  IF (mixedlayerphysics) THEN
    IF (mldExist .eqv. .false.) THEN
      print *, "Error: Missing mld in the nest files. The mld is necessary if mixedlayerphysics is set to true"
      stop
    ENDIF
  ENDIF
 nextFile = .false.

!check if temperature can be outputted
  IF ((outputtemp .eqv. .true.) .and. (tempExist .eqv. .false.)) THEN
    print *, 'Error: You set outputtemp to true but there are no temperature files'
    stop
  ENDIF
!check if salinity can be outputted
  IF ((outputsaln .eqv. .true.) .and. (salExist .eqv. .false.)) THEN
    print *, 'Error: You set outputsaln to true but there are no salinity files'
    stop
  ENDIF

 !calculate the weights for interpolation
 IF (nests(i)%time_units == "months") THEN
! calculate weights for rk step 1
  CALL cdate(basedate,yyyy,mm,dd)
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
    nests(i)%w(3,1)=x1*(1.+x *(1.-1.5*x ))
    nests(i)%w(2,1)=x *(1.+x1*(1.-1.5*x1))
    nests(i)%w(1,1)=-.5*x1*x *x
    nests(i)%w(4,1)=-.5*x *x1*x1
  ELSE
    nests(i)%w(2,1)=x1*(1.+x *(1.-1.5*x ))
    nests(i)%w(3,1)=x *(1.+x1*(1.-1.5*x1))
    nests(i)%w(4,1)=-.5*x1*x *x
    nests(i)%w(1,1)=-.5*x *x1*x1
  ENDIF
 
! calculate weights for rk step 2+3
  lc = time_t + 0.5 * timestep
  CALL cdate(basedate,yyyy,mm,dd)
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
    nests(i)%w(3,2)=x1*(1.+x *(1.-1.5*x ))
    nests(i)%w(2,2)=x *(1.+x1*(1.-1.5*x1))
    nests(i)%w(1,2)=-.5*x1*x *x
    nests(i)%w(4,2)=-.5*x *x1*x1
    nests(i)%w(:,3)=nests(i)%w(:,2)
  ELSE
    nests(i)%w(2,2)=x1*(1.+x *(1.-1.5*x ))
    nests(i)%w(3,2)=x *(1.+x1*(1.-1.5*x1))
    nests(i)%w(4,2)=-.5*x1*x *x
    nests(i)%w(1,2)=-.5*x *x1*x1
    nests(i)%w(:,3)=nests(i)%w(:,2)
  ENDIF
  
  IF (x .lt. x_save) THEN
    nextFile(2) = .true.
    nextFile(3) = .true.
  ENDIF
 
! calculate weights for rk step 4
  lc = time_t + timestep
  CALL cdate(basedate,yyyy,mm,dd)
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
    nests(i)%w(3,4)=x1*(1.+x *(1.-1.5*x ))
    nests(i)%w(2,4)=x *(1.+x1*(1.-1.5*x1))
    nests(i)%w(1,4)=-.5*x1*x *x
    nests(i)%w(4,4)=-.5*x *x1*x1
  ELSE
    nests(i)%w(2,4)=x1*(1.+x *(1.-1.5*x ))
    nests(i)%w(3,4)=x *(1.+x1*(1.-1.5*x1))
    nests(i)%w(4,4)=-.5*x1*x *x
    nests(i)%w(1,4)=-.5*x *x1*x1
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
  nests(i)%w(2,1) = ((real(frac_tstep))/(real(nests(i)%time_step)))
! calculate weights for rk step 2 + 3
  lc = time_t + 0.5 * timestep
  tstep=real(lc)/real(nests(i)%time_step)
  frac_tstep=(tstep - (lc/nests(i)%time_step))*nests(i)%time_step
  nests(i)%w(1,2) = ((real(frac_tstep)-real(nests(i)%time_step)) /(-real(nests(i)%time_step)))
  nests(i)%w(2,2) = ((real(frac_tstep))/(real(nests(i)%time_step)))
  nests(i)%w(1,3) = nests(i)%w(1,2)
  nests(i)%w(2,3) = nests(i)%w(2,2)
  IF (int(tstep) .gt. tstep_save) THEN
    nextFile(2) = .true.
    nextFile(3) = .true.
  ENDIF
! calculate weights for rk step 4
  lc = time_t + timestep
  tstep=real(lc)/real(nests(i)%time_step)
  frac_tstep=(tstep - (lc/nests(i)%time_step))*nests(i)%time_step
  nests(i)%w(1,4) = ((real(frac_tstep)-real(nests(i)%time_step)) /(-real(nests(i)%time_step)))
  nests(i)%w(2,4) = ((real(frac_tstep))/(real(nests(i)%time_step)))
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