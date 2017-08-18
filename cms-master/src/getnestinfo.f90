!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : getnestinfo.f90                                                   *
!* Last Modified: 2016-04-01                                                *
!* Code contributors: Claire B. Paris, Judith Helgers, Ashwanth Srinivasan  * 
!*                    Ana Carolina Vaz, Erik van Sebille                    *
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

 character(char_len)     :: nlname, nestname, nestname1, nestname2, nestname3, nestname4, &
                            lonname, latname, depname
 integer (kind=int_kind) :: ncId,varId, lonDimId, iunit, numDims, i, j, ax, &
                            sizLon, sizLat, sizDep
 logical (kind=log_kind) :: file_exists
 real (kind=real_kind), allocatable   :: tmp2dvar(:,:), lonaux(:), lataux(:), depaux(:)

!namelist variables
 character(char_len) :: &
    filename, time_units, zaxis_positive_direction, &
    xaxis, yaxis, zaxis, taxis,&
    lon_name, lat_name, depth_name, time_name,&
    lon_nameU, lat_nameU, dep_nameU,&
    lon_nameV, lat_nameV, dep_nameV,&
    lon_nameW, lat_nameW, dep_nameW,&
    lon_nameT, lat_nameT, dep_nameT,&
    uvel_name, vvel_name, wvel_name, &
    saln_name, temp_name, dens_name, ssh_name, & 
    angle_file
 integer (kind=int_kind) :: & 
    tstart_yy, tstart_mm, tstart_dd, &
    tend_yy, tend_mm, tend_dd, &
    jdate_yy, jdate_mm, jdate_dd, time_step, &
    axorderX, axorderY, axorderZ
 real (kind = real_kind) :: &
    xstart, xend,ystart, yend, zstart, zend, &
    fill_value
 logical (kind = log_kind) :: &
    orthogrid, testorthogrid
    
!dataset namelists
 namelist /nest_input/filename, &
   xaxis,yaxis,zaxis,taxis,xstart, & 
   xend,ystart,yend,zstart,zend, zaxis_positive_direction, &
   tstart_yy,tstart_mm,tstart_dd,tend_yy,tend_mm,tend_dd, &
   time_step,time_units,jdate_yy,jdate_mm,jdate_dd, &
   lon_name,lat_name,depth_name,lon_nameU,lat_nameU,dep_nameU,&
   lon_nameV,lat_nameV,dep_nameV,lon_nameW,lat_nameW,dep_nameW,&
   lon_nameT,lat_nameT,dep_nameT,depth_conversion_factor,time_name, &
   uvel_name,vvel_name,wvel_name,wvel_positive_direction, &
   velocity_conversion_factor,dens_name,temp_name,saln_name, &
   ssh_name,angle_file,fill_value, agrid, orthogrid, &
   axorderX, axorderY, axorderZ, axorderT

! set some default values for namelist
 orthogrid = .true.
 axorderX = 1
 axorderY = 2
 axorderZ = 3
 axorderT = 4
 depth_conversion_factor = 1.
!read input namelist: nest.nml
 write(nlname,'(A,I0,A)') trim(fileinput)//'nest_',nest_n, '.nml'
 INQUIRE(FILE=trim(nlname), EXIST=file_exists)
 if (file_exists) then
   !open file
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
 nests(nest_n)%tend_yy = tend_yy
 nests(nest_n)%tend_mm = tend_mm
 nests(nest_n)%tend_dd = tend_dd
 nests(nest_n)%fill_value=fill_value
 nests(nest_n)%uname="zu"
 nests(nest_n)%vname="zv"
 nests(nest_n)%wname="zw"
 nests(nest_n)%densname="zd"
 nests(nest_n)%tempname="zt"
 nests(nest_n)%salnname="zs"
 nests(nest_n)%orthogrid = orthogrid
 
 ! check if fill value is the one used by getdata
  if (fill_value .ne. 1.2676506E30) then
   print *, "WARNING: The fill value you have entered in nest_x.nml is not the one used by cms", &
   " (1.2676506E30), is that what you intend?"
  endif
 ! just a user check
 
 if (int(nests(nest_n)%time_step/(30*secs_in_day)).ge.1) then
   nests(nest_n)%time_units = "months"
 endif
!read lon, lat and depth from first nestfile
 AxUsed(:)=.true.
!if agrid, then one single nest file. Otherwise, one nestfile per velocity component
 if (agrid) then
   allocate(nests(nest_n)%idm(1))
   allocate(nests(nest_n)%jdm(1))
   allocate(nests(nest_n)%kdm(1))
   UAx=1
   VAX=1
   WAx=1
   QAx=1
   write(nestname,'(A,I0,A,I4.4,I2.2,I2.2,A)') &
    trim(filenest)//'nest_',nest_n,'_',tstart_yy,tstart_mm,tstart_dd,'000000.nc'
   call nc_open(trim(nestname),ncId)  
   call nc_info_dim2(nestname,ncId,1,nests(nest_n)%idm(1))
   call nc_info_dim2(nestname,ncId,2,nests(nest_n)%jdm(1))
   call nc_info_dim2(nestname,ncId,3,nests(nest_n)%kdm(1))
   call nc_info_dim2(nestname,ncId,4,nests(nest_n)%tdm)
   call nc_close(nestname, ncId)
 else ! not agrid
   allocate(nests(nest_n)%idm(4))
   allocate(nests(nest_n)%jdm(4))
   allocate(nests(nest_n)%kdm(4))
   nests(nest_n)%idm(:)=0
   nests(nest_n)%jdm(:)=0
   nests(nest_n)%kdm(:)=0
   UAx=1
   VAX=2
   WAx=3
   QAx=4
   write(nestname1,'(A,I0,A,I4.4,I2.2,I2.2,A)') &
    trim(filenest)//'nest_',nest_n,'_',tstart_yy,tstart_mm,tstart_dd,'000000u.nc'
   nests(nest_n)%uname = trim(uvel_name)
   write(nestname2,'(A,I0,A,I4.4,I2.2,I2.2,A)') &
    trim(filenest)//'nest_',nest_n,'_',tstart_yy,tstart_mm,tstart_dd,'000000v.nc'
   nests(nest_n)%vname = trim(vvel_name)
   write(nestname3,'(A,I0,A,I4.4,I2.2,I2.2,A)') &
    trim(filenest)//'nest_',nest_n,'_',tstart_yy,tstart_mm,tstart_dd,'000000w.nc'
   INQUIRE(FILE=nestname3, EXIST=file_exists)
   IF (file_exists.eqv..false.) THEN
     WAx=2
     AxUsed(3)=.false.
     print *, 'Not using vertical velocity data'
   ELSE
     nests(nest_n)%wname = trim(wvel_name)
   ENDIF
   write(nestname4,'(A,I0,A,I4.4,I2.2,I2.2,A)') &
    trim(filenest)//'nest_',nest_n,'_',tstart_yy,tstart_mm,tstart_dd,'000000t.nc'
   INQUIRE(FILE=nestname4, EXIST=file_exists)
   IF (file_exists.eqv..false.) THEN
     QAx=WAx
     AxUsed(4)=.false.
     print *, 'Not using temperature data'
   ELSE
     nests(nest_n)%densname = trim(dens_name)
     nests(nest_n)%tempname = trim(temp_name)
     nests(nest_n)%salnname = trim(saln_name)
   ENDIF
   do ax=1,QAx
     if (axUsed(ax) .eqv. .true.) then
       if (ax.eq.1) then
         nestname=nestname1
       elseif (ax.eq.2) then
         nestname=nestname2
       elseif (ax.eq.3) then
         nestname=nestname3
       elseif (ax.eq.4) then
         nestname=nestname4
       endif
       call nc_open(trim(nestname),ncId)  
       call nc_info_dim2(nestname,ncId,axorderX,nests(nest_n)%idm(ax))
       call nc_info_dim2(nestname,ncId,axorderY,nests(nest_n)%jdm(ax))
       call nc_info_dim2(nestname,ncId,axorderZ,nests(nest_n)%kdm(ax))
       call nc_info_dim2(nestname,ncId,axorderT,nests(nest_n)%tdm)
       call nc_close(nestname, ncId)
     endif
   enddo
 endif

 if (nests(nest_n)%orthogrid) then
   allocate(nests(nest_n)%lon(QAx,maxval(nests(nest_n)%idm),1))
   allocate(nests(nest_n)%lat(QAx,maxval(nests(nest_n)%jdm),1))
 else
   allocate(nests(nest_n)%lon(QAx,maxval(nests(nest_n)%idm),maxval(nests(nest_n)%jdm))) 
   allocate(nests(nest_n)%lat(QAx,maxval(nests(nest_n)%idm),maxval(nests(nest_n)%jdm))) 
   allocate(nests(nest_n)%angle(QAx,maxval(nests(nest_n)%idm),maxval(nests(nest_n)%jdm))) 
 endif
 allocate(nests(nest_n)%depth(QAx,maxval(nests(nest_n)%kdm)))
 allocate(tmp2dvar(maxval(nests(nest_n)%idm),maxval(nests(nest_n)%jdm)))

 do ax=1,QAx
   if (agrid.eqv..true.) then
     call nc_open(trim(nestname),ncId)  
     !read longitude
     call nc_read1d(nestname, ncId,"Longitude", nests(nest_n)%lon(ax,1:nests(nest_n)%idm(ax),1), nests(nest_n)%idm(ax))
     call nc_read1d(nestname, ncId,"Latitude", nests(nest_n)%lat(ax,1:nests(nest_n)%jdm(ax),1), nests(nest_n)%jdm(ax))
     call nc_read1d(nestname, ncId,"Depth", nests(nest_n)%depth(ax,1:nests(nest_n)%kdm(ax)), nests(nest_n)%kdm(ax))
     call nc_close(nestname, ncId)
   else
     if (AxUsed(ax) .eqv. .true.) then
       if (ax.eq.1) then
         lonname=lon_nameU
         latname=lat_nameU
         depname=dep_nameU
         nestname=nestname1
       elseif (ax.eq.2) then
         lonname=lon_nameV
         latname=lat_nameV
         depname=dep_nameV
         nestname=nestname2
       elseif (ax.eq.3) then
         lonname=lon_nameW
         latname=lat_nameW
         depname=dep_nameW
         nestname=nestname3
       elseif (ax.eq.4) then
         lonname=lon_nameT
         latname=lat_nameT
         depname=dep_nameT
         nestname=nestname4
       endif
     endif
     call nc_open(trim(nestname),ncId)
     !read longitude
     call nc_info_var(nestname,ncId,trim(lonname), numDims)
     if (numDims .ne. 1) then
       call nc_read2d(nestname, ncId,trim(lonname),tmp2dvar,nests(nest_n)%idm(ax),nests(nest_n)%jdm(ax))
       if (nests(nest_n)%orthogrid) then
         nests(nest_n)%lon(ax,1:nests(nest_n)%idm(ax),1)=tmp2dvar(:,1)
         do i=1,nests(nest_n)%idm(ax)
           do j=2,nests(nest_n)%jdm(ax)
             if (tmp2dvar(i,j) .ne. tmp2dvar(i,1)) then
               print *, "Error: Longitude grid is not orthogonal. Set orthogrid to false in nest.nml"
               stop
             endif
           enddo
         enddo
       else
         nests(nest_n)%lon(ax,1:nests(nest_n)%idm(ax),1:nests(nest_n)%jdm(ax))=tmp2dvar
         testorthogrid=.true.
         do i=1,nests(nest_n)%idm(ax)
           do j=2,nests(nest_n)%jdm(ax)
             if (tmp2dvar(i,j) .ne. tmp2dvar(i,1)) then
               testorthogrid=.false.
             endif
           enddo
         enddo
         if (testorthogrid .and. ax .eq. 1) then
           print *, 'WARNING: You set orthogrid to false, but your grids appear orthogonal.'
           print *, 'This can seriously slown down the code. Try setting orthogrid to true.'
         endif
         print *, 'NON-ORTHOGONAL GRIDS ARE NOT YET IMPLEMENTED. STOPPING NOW.'
         print *, 'set orthogrid to false'
         stop
       endif
     else
       call nc_read1d(nestname, ncId,trim(lonname), nests(nest_n)%lon(ax,1:nests(nest_n)%idm(ax),1), nests(nest_n)%idm(ax))
     endif 
     do i=1,nests(nest_n)%idm(ax)
       do j=1,size(nests(nest_n)%lon,3)
         if (nests(nest_n)%lon(ax,i,j) .gt. 360.) then
           nests(nest_n)%lon(ax,i,j) = nests(nest_n)%lon(ax,i,j) - 360.
         elseif (nests(nest_n)%lon(ax,i,j) .lt. 0.) then
           nests(nest_n)%lon(ax,i,j) = nests(nest_n)%lon(ax,i,j) + 360.
         endif
       enddo
     enddo
     !read latitude 
     call nc_info_var(nestname,ncId,trim(latname), numDims)
     if (numDims .ne. 1) then
       call nc_read2d(nestname, ncId,trim(latname),tmp2dvar,nests(nest_n)%idm(ax),nests(nest_n)%jdm(ax))
       if (nests(nest_n)%orthogrid) then
         nests(nest_n)%lat(ax,1:nests(nest_n)%jdm(ax),1)=tmp2dvar(1,:)
         do j=1,nests(nest_n)%jdm(ax)
           do i=2,nests(nest_n)%idm(ax)
             if (tmp2dvar(i,j) .ne. tmp2dvar(1,j)) then
               print *, "Error: Latitude grid is not orthogonal. Set orthogrid to false in nest.nml"
               stop
             endif
           enddo
         enddo
       else
         nests(nest_n)%lat(ax,1:nests(nest_n)%idm(ax),1:nests(nest_n)%jdm(ax))=tmp2dvar
       endif
     else
       call nc_read1d(nestname, ncId,trim(latname), nests(nest_n)%lat(ax,1:nests(nest_n)%jdm(ax),1), nests(nest_n)%jdm(ax))
     endif
!    read depth
     call nc_read1d(nestname, ncId, trim(depname), nests(nest_n)%depth(ax,1:nests(nest_n)%kdm(ax)), nests(nest_n)%kdm(ax))
     nests(nest_n)%depth = nests(nest_n)%depth * depth_conversion_factor
!    read angle file if not orthogrid
     if (nests(nest_n)%orthogrid .eqv. .false.) then
       call nc_read2d(angle_file, ncId,trim("angle"),tmp2dvar,nests(nest_n)%idm(ax),nests(nest_n)%jdm(ax))
       nests(nest_n)%angle(ax,1:nests(nest_n)%idm(ax),1:nests(nest_n)%jdm(ax))=tmp2dvar
     endif
     call nc_close(nestname, ncId)
   endif
 enddo !ax=1,QAx

!allocate arrays
 if (nests(nest_n)%time_units == "months") then
   !if time_unit is months then interpolation between 4 different files
   allocate(nests(nest_n)%w(4,4))
   allocate(nests(nest_n)%uvel(nests(nest_n)%idm(UAx),nests(nest_n)%jdm(UAx),nests(nest_n)%kdm(UAx),5))
   allocate(nests(nest_n)%vvel(nests(nest_n)%idm(VAx),nests(nest_n)%jdm(VAx),nests(nest_n)%kdm(VAx),5))
   allocate(nests(nest_n)%wvel(nests(nest_n)%idm(WAx),nests(nest_n)%jdm(WAx),nests(nest_n)%kdm(WAx),5))
   allocate(nests(nest_n)%mld(nests(nest_n)%idm(QAx),nests(nest_n)%jdm(QAx),5))
   if (buoyancy .or. diffpart .or. massspawning) then
     allocate(nests(nest_n)%temp(nests(nest_n)%idm(QAx),nests(nest_n)%jdm(QAx),nests(nest_n)%kdm(QAx),5))
     allocate(nests(nest_n)%saln(nests(nest_n)%idm(QAx),nests(nest_n)%jdm(QAx),nests(nest_n)%kdm(QAx),5))
     allocate(nests(nest_n)%dens(nests(nest_n)%idm(QAx),nests(nest_n)%jdm(QAx),nests(nest_n)%kdm(QAx),5))
     allocate(nests(nest_n)%ssh(nests(nest_n)%idm(QAx),nests(nest_n)%jdm(QAx),5))
   endif  
 else
   !if time_unit is days or seconds then interpolation 
   !between 2 different files
   allocate(nests(nest_n)%w(2,4))
   allocate(nests(nest_n)%uvel(nests(nest_n)%idm(UAx), &
    nests(nest_n)%jdm(UAx),nests(nest_n)%kdm(UAx),3))
   allocate(nests(nest_n)%vvel(nests(nest_n)%idm(VAx), &
    nests(nest_n)%jdm(VAx),nests(nest_n)%kdm(VAx),3))
   allocate(nests(nest_n)%wvel(nests(nest_n)%idm(WAx), &
    nests(nest_n)%jdm(WAx),nests(nest_n)%kdm(WAx),3))
   allocate(nests(nest_n)%mld(nests(nest_n)%idm(QAx), &
    nests(nest_n)%jdm(QAx),3))
   if (withibm) then
     allocate(nests(nest_n)%temp(nests(nest_n)%idm(QAx), &
      nests(nest_n)%jdm(QAx),nests(nest_n)%kdm(QAx),3))
     allocate(nests(nest_n)%saln(nests(nest_n)%idm(QAx), &
      nests(nest_n)%jdm(QAx),nests(nest_n)%kdm(QAx),3))
     allocate(nests(nest_n)%dens(nests(nest_n)%idm(QAx), &
      nests(nest_n)%jdm(QAx),nests(nest_n)%kdm(QAx),3))
     allocate(nests(nest_n)%ssh(nests(nest_n)%idm(QAx), &
      nests(nest_n)%jdm(QAx),3))
   endif
 endif

 !read uvel to calculate mask
 if (agrid.eqv..false.) then
   nestname=nestname1
 endif
 call nc_open(trim(nestname),ncId)
 call nc_read4d(nestname, ncId,trim(nests(nest_n)%uname), &
 nests(nest_n)%uvel(:,:,:,1), nests(nest_n)%idm(1), &
 nests(nest_n)%jdm(1), nests(nest_n)%kdm(1), 1)
 call nc_close(nestname, ncId)
 
!get attribute regridded if exists
 if (nc_exists(ncId,"regridded")) then
   nests(nest_n)%tilted = .true.
 else
   nests(nest_n)%tilted = .false.
 endif

!Set mask. 
!Mask is necessary for nests with strange grids to see which 
!lon, lat points are inside and outside the grid
 allocate(nests(nest_n)%mask(nests(nest_n)%idm(1),nests(nest_n)%jdm(1)))
 nests(nest_n)%mask = 1
 do i=1, nests(nest_n)%idm(1)
   do j=1, nests(nest_n)%jdm(1)
     if (nests(nest_n)%uvel(i,j,1,1) .eq. 0) then
       nests(nest_n)%mask(i,j) = 0
     endif
   enddo
 enddo

 nests(nest_n)%uvel=0.0
 nests(nest_n)%vvel=0.0
 nests(nest_n)%wvel=0.0
 nests(nest_n)%mld=0.0
 if (withibm) then
   nests(nest_n)%dens=0.0
   nests(nest_n)%temp=0.0
   nests(nest_n)%saln=0.0
   nests(nest_n)%ssh=0.0
 endif
 nests(nest_n)%dataExist = .false. 
 nests(nest_n)%fnameold = "" 

end subroutine getnestinfo
