!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : getnestinfo.f90                                                   *
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

SUBROUTINE getnestinfo(nest_n)
   
 USE globalvariables
 USE mod_netcdf
 USE mod_kinds
 USE mod_iounits
 USE mod_nciorw

 IMPLICIT NONE

 integer (kind=int_kind), intent(in) :: nest_n

 character(char_len)     :: nlname, nestname
 integer (kind=int_kind) :: ncId,varId, lonDimId, iunit, numDims, i, j 
 logical (kind=log_kind) :: file_exists


!namelist variables
 character(char_len) :: &
    filename, time_units, zaxis_positive_direction, &
    xaxis, yaxis, zaxis, taxis,&
    lon_name, lat_name, depth_name, time_name,&
    uvel_name, vvel_name, wvel_name, &
    saln_name, temp_name, dens_name, ssh_name, & 
    wvel_positive_direction, angle_file
 integer (kind=int_kind) :: & 
    tstart_yy, tstart_mm, tstart_dd, &
    tend_yy, tend_mm, tend_dd, &
    jdate_yy, jdate_mm, jdate_dd, time_step 
 real (kind = real_kind) :: &
    xstart, xend,ystart, yend, zstart, zend, &
    velocity_conversion_factor, depth_conversion_factor,fill_value

!dataset namelists
 namelist /nest_input/filename, &
   xaxis,yaxis,zaxis,taxis,xstart, & 
   xend,ystart,yend,zstart,zend, zaxis_positive_direction, &
   tstart_yy,tstart_mm,tstart_dd,tend_yy,tend_mm,tend_dd, &
   time_step,time_units,jdate_yy,jdate_mm,jdate_dd, &
   lon_name,lat_name,depth_name,depth_conversion_factor,time_name, &
   uvel_name,vvel_name,wvel_name,wvel_positive_direction, &
   velocity_conversion_factor,dens_name,temp_name,saln_name, &
   ssh_name,angle_file,fill_value 


!read input namelist: nest.nml

 write(nlname,'(A,I0,A)') trim(fileinput)//'nest_',nest_n, '.nml' 

 INQUIRE(FILE=trim(nlname), EXIST=file_exists)
 if (file_exists) then
! open file
  call get_unit(iunit)
  open(iunit, file=nlname, status='old',form='formatted')
 else
  print *, "Error: File ", trim(nlname)," does not exist"
  stop
 endif
!read file
 read(iunit, nml=nest_input)
!close file
 call release_unit(iunit)

!save some variables for later use
 nests(nest_n)%time_step = time_step
 nests(nest_n)%time_units = time_units
 nests(nest_n)%tstart_yy = tstart_yy
 nests(nest_n)%tstart_mm = tstart_mm
 nests(nest_n)%tstart_dd = tstart_dd

 if (int(nests(nest_n)%time_step/(30*secs_in_day)).ge.1) then
   nests(nest_n)%time_units = "months"
 endif 


!read lon, lat and depth from first nestfile
 write(nestname,'(A,I0,A,I4.4,I2.2,I2.2,A)') &
   trim(filenest)//'nest_',nest_n,'_',tstart_yy,tstart_mm,tstart_dd,'000000.nc'

!open nestfile  
 call nc_open(trim(nestname),ncId)  

 call nc_info_dim2(nestname,ncId,1,nests(nest_n)%idm)
 call nc_info_dim2(nestname,ncId,2,nests(nest_n)%jdm)
 call nc_info_dim2(nestname,ncId,3,nests(nest_n)%kdm)
 call nc_info_dim2(nestname,ncId,4,nests(nest_n)%tdm)

!read longitude
 call nc_info_var(nestname,ncId,"Longitude", numDims)
 if (numDims .eq. 1) then
  allocate(nests(nest_n)%lon(nests(nest_n)%idm))
 else
  print *, "Error: Dimension of Longitude has to be 1" 
  stop
 endif 
 call nc_read1d(nestname, ncId,"Longitude",nests(nest_n)%lon,nests(nest_n)%idm)

!read latitude 
 call nc_info_var(nestname,ncId,"Latitude", numDims)
 if (numDims .eq. 1) then
  allocate(nests(nest_n)%lat(nests(nest_n)%jdm))
 else
  print *, "Error: Dimension of Latitude has to be 1" 
  stop
 endif 
 call nc_read1d(nestname, ncId,"Latitude",nests(nest_n)%lat,nests(nest_n)%jdm)

!read depth
 allocate(nests(nest_n)%depth(nests(nest_n)%kdm))
 call nc_read1d(nestname, ncId,"Depth",nests(nest_n)%depth,nests(nest_n)%kdm)

!allocate arrays
 if (nests(nest_n)%time_units == "months") then
! if time_unit is months then interpolation between 4 different files
  allocate(nests(nest_n)%w(4,4))
  allocate(nests(nest_n)%uvel(nests(nest_n)%idm,nests(nest_n)%jdm,nests(nest_n)%kdm,5))
  allocate(nests(nest_n)%vvel(nests(nest_n)%idm,nests(nest_n)%jdm,nests(nest_n)%kdm,5))
  allocate(nests(nest_n)%wvel(nests(nest_n)%idm,nests(nest_n)%jdm,nests(nest_n)%kdm,5))
  allocate(nests(nest_n)%temp(nests(nest_n)%idm,nests(nest_n)%jdm,nests(nest_n)%kdm,5))
  allocate(nests(nest_n)%saln(nests(nest_n)%idm,nests(nest_n)%jdm,nests(nest_n)%kdm,5))
  allocate(nests(nest_n)%dens(nests(nest_n)%idm,nests(nest_n)%jdm,nests(nest_n)%kdm,5))
  allocate(nests(nest_n)%ssh(nests(nest_n)%idm,nests(nest_n)%jdm,5))
 else
! if time_unit is days or seconds then interpolation between 2 different files
  allocate(nests(nest_n)%w(4,2))
  allocate(nests(nest_n)%uvel(nests(nest_n)%idm,nests(nest_n)%jdm,nests(nest_n)%kdm,3))
  allocate(nests(nest_n)%vvel(nests(nest_n)%idm,nests(nest_n)%jdm,nests(nest_n)%kdm,3))
  allocate(nests(nest_n)%wvel(nests(nest_n)%idm,nests(nest_n)%jdm,nests(nest_n)%kdm,3))
  allocate(nests(nest_n)%temp(nests(nest_n)%idm,nests(nest_n)%jdm,nests(nest_n)%kdm,3))
  allocate(nests(nest_n)%saln(nests(nest_n)%idm,nests(nest_n)%jdm,nests(nest_n)%kdm,3))
  allocate(nests(nest_n)%dens(nests(nest_n)%idm,nests(nest_n)%jdm,nests(nest_n)%kdm,3))
  allocate(nests(nest_n)%ssh(nests(nest_n)%idm,nests(nest_n)%jdm,3))
 endif

!read uvel to calculate mask
 call nc_read4d(nestname,ncId,"zu",nests(nest_n)%uvel(:,:,:,1), &
    nests(nest_n)%idm,nests(nest_n)%jdm,nests(nest_n)%kdm,1)
 
!get attribute regridded if exists
 if (nc_exists(nestname,ncId,"regridded")) then
   nests(nest_n)%tilted = .true.
 else
   nests(nest_n)%tilted = .false.
 endif

!close nestfile   
 call nc_close(nestname, ncId)

!Set mask. 
!Mask is necessary for nests with strange grids to see which 
!lon, lat points are inside and outside the grid
 allocate(nests(nest_n)%mask(nests(nest_n)%idm,nests(nest_n)%jdm))
 nests(nest_n)%mask = 1
 do i=1, nests(nest_n)%idm
  do j=1, nests(nest_n)%jdm
   if (nests(nest_n)%uvel(i,j,1,1) .eq. 0) then
     nests(nest_n)%mask(i,j) = 0
   endif
  enddo
 enddo

 nests(nest_n)%uvel=0.0
 nests(nest_n)%vvel=0.0
 nests(nest_n)%wvel=0.0
 nests(nest_n)%dens=0.0
 nests(nest_n)%temp=0.0
 nests(nest_n)%saln=0.0
 nests(nest_n)%ssh=0.0
 nests(nest_n)%dataExist = .false. 
 nests(nest_n)%fnameold = "" 

end subroutine getnestinfo




