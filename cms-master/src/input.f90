!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : input.f90                                                         *
!* Last Modified: 2016-11-17                                                *
!* Code contributors: Claire B. Paris, Ana Carolina Vaz, Judith Helgers,    * 
!*                    Ashwanth Srinivasan                                   *
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
!read files from modules if flag is turned on
SUBROUTINE load_mod_input

 USE globalvariables
 USE mod_reef
 USE mod_ibio
 USE mod_diffpart
 USE mod_strata
 USE mod_buoyancy
 
!loads strata file
 IF (strata) THEN
  call load_strata( trim(fileinput)//trim(strataFilename))
 ENDIF

!loads polygon file
 IF (polygon) THEN
  IF (strata) THEN
    call load_reef_data_strata( trim(fileinput)//trim(polyFilename))
  ELSE
  call load_reef_data( trim(fileinput)//trim(polyFilename))
 ENDIF     
 ENDIF      
  
!loads ibio file  
 IF (ibio .or. massSpawning) THEN
  call load_ibio_data( trim(fileinput)//trim(ibioFilename))
 ENDIF
 
!load different size, density and half life time file
 IF (diffpart) THEN
  call load_diffpart_data( trim(fileinput)//trim(diffpartFilename))
 ENDIF

  IF (buoyancy) THEN
   call load_buoyancy( trim(fileinput)//trim(buoyancyFilename))
 ENDIF

END SUBROUTINE load_mod_input


!**************************************************************
!read file runconf.list
SUBROUTINE load_runconf
 USE constants
 USE globalvariables
 USE mod_kinds
 USE mod_iounits

 IMPLICIT NONE

 integer (kind=int_kind) :: i, iunit
 logical (kind=log_kind) :: file_exists

!runconf.list
 namelist /runconf/nnests, timeMax, timeStep, &
    outputFreq, releaseFilename, &
    turb, horDiff, vertDiff, turbTimestep, &
    periodicbc, avoidcoast, backward, ascii, &
    upperlevelsurface, loopfiles, loopfilesstartyear, loopfilesstartmonth, &
    loopfilesstartday, loopfilesendyear, loopfilesendmonth, loopfilesendday, &
    writerestart, restartfromfile, restartwritefreq, &
    mixedlayerphysics, mixedlayerwmax, &
    trajout

 INQUIRE(FILE=trim(fileinput)//'runconf.list',EXIST=file_exists)
 IF (file_exists) THEN
! open file
  call get_unit(iunit)
  open(iunit,file=trim(fileinput)//'runconf.list',form='formatted')
 ELSE
  print *, "Error: File ", trim(fileinput)//'runconf.list'," does not exist"
  stop
 ENDIF
 
!read file   
 read(iunit,nml=runconf)  

!close file
 call release_unit(iunit)

!check parameters
 IF (nnests .le. 0) THEN
  print *, "Error: Parameter nnests in the file runconf.list should have a value larger than 0"
  stop
 ENDIF    

 IF (timeMax .le. 0) THEN
  print *, "Error: Parameter timeMax in the file runconf.list should have a value larger than 0"
  stop
 ENDIF  

 IF (timeStep .le. 0) THEN
  print *, "Error: Parameter timeStep in the file runconf.list should have a value larger than 0"
  stop
 ENDIF  

 IF (outputFreq .le. 0) THEN
  print *, "Error: Parameter outputFreq in the file runconf.list should have a value larger than 0"
  stop
 ENDIF

 IF (writerestart) THEN
 print *, "Writing restart file. Will be updated every:",restartwriteFreq,"seconds"
  IF (restartwritefreq .le. 0) THEN
   restartwriteFreq = outputFreq*10
   print *, "Setting parameter restartwriteFreq to that of 10 * outputFreq at ", restartwriteFreq
  ENDIF
 ENDIF

 IF (releaseFilename .eq. "") THEN
  print *, "Error: Parameter releaseFilename in the file runconf.list should have a value "
  stop
 ENDIF

 IF (turb) THEN
   IF (turbTimestep .eq. 0) THEN
     turbTimestep = timestep
     print *, "WARNING: turbTimestep in the file runconf.list is set to 0 when turb=true, assuming turbTimestep = timestep"
   ENDIF
   IF (turbTimestep .lt. timestep) THEN
    print *, "turbTimestep in the file runconf.list has a value larger or equal to timestep"
    stop
   ENDIF
   IF (mod(turbTimestep, timestep) .ne. 0) THEN
    print *, "turbTimestep in the file runconf.list has to be an integer multiple of timestep"
    stop
   ENDIF
 ENDIF
  
 IF (ibio) THEN
   IF (ibioTimestep .eq. 0) THEN
     ibioTimestep = timestep
   ENDIF
print *, 'ibiotimestep', timestep
   IF (ibioTimestep .lt. timestep) THEN
    print *, "ibioTimestep in the file ibm.list has to be larger or equal to timestep"
    stop
   ENDIF
   IF (mod(ibioTimestep, timestep) .ne. 0) THEN
    print *, "ibioTimestep in the file ibm.list has to be an integer multiple of timestep"
    stop
   ENDIF
  ENDIF
 
!allocate and populate nests
 allocate(nests(nnests))

!read data from nestfiles
 DO i=1,nnests
   call getnestinfo(i)
 ENDDO

!calculate diffusity for nests
 IF (turb) THEN
  DO i=1,nnests
   IF (horDiff(1) .eq. -1 .or. vertDiff(1) .eq. -1) THEN
    print *, "There is no horizontal or vertical diffusivity kh defined"
    stop
   ENDIF
   IF (horDiff(i) .eq. -1 .and. i .gt. 1) THEN
     horDiff(i) = horDiff(i-1)
   ENDIF
   IF (vertDiff(i) .eq. -1 .and. i .gt. 1) THEN
     vertDiff(i) = vertDiff(i-1)
   ENDIF
  ENDDO
 ENDIF
!write (*,*) "Run conf data accquired"
END SUBROUTINE load_runconf

!**************************************************************
!Read the file ibm.list
SUBROUTINE load_ibm

 USE mod_kinds
 USE constants
 USE mod_iounits
 USE globalvariables

 IMPLICIT NONE

 logical (kind=log_kind):: file_exists
 integer (kind=int_kind):: iunit

!ibm.list
 namelist /ibm/ &
   buoyancy, buoyancyFilename, &
   polygon, polyFilename, settlementStart, &
   ibio, ibioFilename,ibioTimeStep, &
   mort, halflife, &
   diffpart, diffpartFilename, &
   massSpawning, larvaStart, &
   tidalMovement, tstStart, &
   strata, strataFilename, &
   outputtemp, outputsaln

 polyFilename = "Polygon file"
 ibioFilename = "Ibio file"
 diffpartFilename = "Diffpart file"
 fractionFilename = "Fraction file"
 strataFilename = "Strata file"
 buoyancyFilename = "Buoyancy file"
 ibioTimeStep = 0
 withibm=.true.

 INQUIRE(FILE=trim(fileinput)//'ibm.list',EXIST=file_exists)
 IF (file_exists) THEN
  call get_unit(iunit)    
! open file
  open(iunit,file=trim(fileinput)//'ibm.list',form='formatted')
! read file
  read(iunit,nml=ibm)
! close file
  call release_unit(iunit)
 ELSE
  print *, "Warning: File ", trim(fileinput)//'ibm.list'," does not exist, CMS will run in passive mode (no IBM)"
  withibm=.false.
  buoyancy=.false.
  ibio=.false.
  mort=.false.
  diffpart=.false.
  massspawning=.false. 
  tidalMovement=.false.
  strata=.false.
 ENDIF

END SUBROUTINE load_ibm

!**************************************************************
!reads the input file with the release info. 
!lon, lat, depth of release point.
SUBROUTINE load_release_info

 USE mod_ibio
 USE mod_kinds
 USE constants
 USE mod_iounits
 USE globalvariables
 USE mod_strata
 USE mod_buoyancy

 IMPLICIT NONE

 character(char_len)     :: rfname, locname 
 real (kind = real_kind) :: lon, lat, depth
 integer (kind=int_kind) :: id,num_rel,year, month, day, seconds, &
     layer, strataStart, & !ibio
     i, iunit, sze !number of release locations
 logical (kind=log_kind) :: file_exists

 rfname = adjustl(trim(fileinput))//trim(releaseFilename)
 call getSize(rfname,sze)
 allocate(particle(sze))

!open file
 call get_unit(iunit) 
 INQUIRE(FILE=trim(rfname),EXIST=file_exists)
 IF (file_exists) THEN
  open(iunit,file=trim(rfname), status="old")
 ELSE
  print *,  "Error: File ", trim(rfname),"does not exist"
  stop
 ENDIF

!read each line of file file    
 DO i=1,sze
  read (iunit,*) id,lon,lat,depth,num_rel, year, month, day,seconds
  DO while (lon .lt. 0.) 
    lon = lon + 360.
  ENDDO
  DO while (lon .ge. 360.) 
    lon = lon - 360.
  ENDDO
  allocate(particle(i)%nlon(num_rel))
  allocate(particle(i)%nlat(num_rel))
  allocate(particle(i)%ndepth(num_rel))
  allocate(particle(i)%move(num_rel))
  allocate (particle(i)%flag(num_rel,10)) 
  allocate(particle(i)%oldi(num_rel))
  allocate(particle(i)%oldj(num_rel))
  allocate(particle(i)%oldk(num_rel))
  particle(i)%id=id
  particle(i)%ilon=lon
  particle(i)%ilat=lat
  particle(i)%idepth=depth
  particle(i)%rel_loc_name=trim(locname)
  particle(i)%nlon=lon
  particle(i)%nlat=lat
  particle(i)%ndepth=depth
  particle(i)%num_rel = num_rel
  if (withibm) then
    allocate(particle(i)%dist(num_rel))
    IF ((buoyancy) .or. (diffpart)) THEN
      allocate(particle(i)%diam(num_rel))
      allocate(particle(i)%density(num_rel))
    ENDIF
    IF ((mort) .or. (diffpart)) THEN  
      allocate(particle(i)%halflife(num_rel))
    ENDIF
    IF (ibio) THEN
      allocate(particle(i)%layer(num_rel))
    ENDIF  
    allocate(particle(i)%temp(num_rel))
    allocate(particle(i)%saln(num_rel))
    allocate(particle(i)%old_lonDist(num_rel))
    allocate(particle(i)%old_latDist(num_rel))
    particle(i)%old_lonDist=lon
    particle(i)%old_latDist=lat
    particle(i)%dist=0.
  endif
  if (mixedlayerphysics) then
    allocate(particle(i)%inmld(num_rel))
  endif
  IF (particle(i)%num_rel .eq. 0) THEN
    print *,'Error: the release file has at the column: number of releases a 0' 
    stop
  ENDIF
    IF (strata) THEN
    CALL assign_strata_start(particle(i)%idepth,stratastart)
    particle(i)%strataStart = stratastart
  ENDIF
  particle(i)%year = year
  particle(i)%month = month
  particle(i)%day = day
  particle(i)%seconds = seconds
  particle(i)%move=.true.
  particle(i)%start = 0.
  particle(i)%flag = .False.
  IF (ibio) THEN
    call set_layer(particle(i)%idepth, layer)
    particle(i)%layer = layer
  ENDIF
 ENDDO

!only for massSpawining   
 IF (massSpawning) THEN
  allocate(eggTimePassed(sze))
  eggTimePassed=.false.
 ENDIF

!close file
 call release_unit(iunit)
!print *, "Finished loading release data" 
!print *, "No of Release Locations = ", sze 

END SUBROUTINE load_release_info
!**************************************************************
