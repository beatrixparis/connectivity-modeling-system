!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : mod_getdata.f90                                                   *
!* Last Modified: 2016-11-18                                                *
!* Code contributors: Ana Carolina Vaz, Judith Helgers, Claire B. Paris     * 
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
MODULE mod_getdata

 USE constants
 USE mod_kinds
 USE mod_iounits
 USE mod_nciorw
 USE mod_calendar

 IMPLICIT NONE

 EXTERNAL directory

!module variables
 character(char_len) :: &
    filename, time_units, zaxis_positive_direction, &
    xaxis, yaxis, zaxis, taxis, lon_name, lat_name, depth_name, time_name,&
    lon_nameU, lat_nameU, dep_nameU,lon_nameV, lat_nameV, dep_nameV,&
    lon_nameW, lat_nameW, dep_nameW,lon_nameT, lat_nameT, dep_nameT,&
    uvel_name, vvel_name, wvel_name, saln_name, temp_name, dens_name, ssh_name,&
    wvel_positive_direction, angle_file, filenest, fileinput
 integer (kind=int_kind) :: &
    tstart_yy, tstart_mm, tstart_dd,tend_yy, tend_mm, tend_dd,&
    jdate_yy, jdate_mm, jdate_dd,time_step,&
    gidm, gjdm, gkdm, gtdm, ndimsLon, ndimsLat,&
    idm_out, jdm_out, kdm_out, tdm_out, idm_out_new, jdm_out_new,&
    ipart1, ipart1_new, istart, jstart, iend, jend,&
    istart_new, jstart_new, iend_new, jend_new,&
    kstart, lstart, kend, lend, gidm_new, gjdm_new, nest_n, numFiles, &
    axorderX, axorderY, axorderZ, axorderT
 real (kind = real_kind) :: &
    xstart, xend,ystart, yend, zstart, zend, velocity_conversion_factor, &
    depth_conversion_factor, fill_value, fill_valueNew
 logical (kind=log_kind) :: &
    hasDepth, hasTime, grid2d, was2d, first_time_downl, LatDec, bgrid, &
    agrid, orthogrid
 real (kind = real_kind), allocatable :: &
    tmpLon1d(:),tmpLat1d(:),tmpLon2d(:,:),tmpLat2d(:,:),tmpDepth(:), &
    tmpTime(:),lon1d(:),lat1d(:),lon1d_new(:),lat1d_new(:), lon2d(:,:), &
    lat2d(:,:),depth(:),time(:), uvel(:,:,:,:), vvel(:,:,:,:), &
    wvel(:,:,:,:), temp(:,:,:,:),saln(:,:,:,:), dens(:,:,:,:),&
    uvel_new(:,:,:,:),vvel_new(:,:,:,:),wvel_new(:,:,:,:),&
    temp_new(:,:,:,:),saln_new(:,:,:,:),dens_new(:,:,:,:),&
    ssh(:,:,:),ssh4d(:,:,:,:),ssh_new(:,:,:),tmpLon_new (:), tmpLat_new(:), &
    src_bbox_lon(:,:,:), src_bbox_lat(:,:,:),w(:,:,:), angle(:,:),&
    tmpDepthW(:)
 integer (kind = int_kind), allocatable :: &
    x(:,:,:), y(:,:,:)
 real (kind = dbl_kind), allocatable :: &
    tmpDoubleTime(:)
 character(char_len), allocatable :: &
    downl_nest(:)
   
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

CONTAINS

!**************************************************************
!create directories
SUBROUTINE create_directories(filenumber)
 character(char_len), intent(in)  :: filenumber    
 character(char_len)              :: filedir, fileoutput, filescratch 
 
 !make /expt_
 write(filedir,'(A,A)') 'expt_',trim(filenumber)
 CALL make_dir (adjustl(trim(filedir)),Len(adjustl(trim(filedir))))
 !make /expt_/nests
 write(filenest,'(A,A,A)') 'expt_',trim(filenumber),'/nests/'
 CALL make_dir(adjustl(trim(filenest)),Len(adjustl(trim(filenest))))
 
 !add path to /input_/
 write(fileinput,'(A,A,A)') 'input_',trim(filenumber), '/'

END SUBROUTINE create_directories

!**************************************************************
!read the file nest.nml
SUBROUTINE read_nest_file 

 integer (kind=int_kind) :: iunit
 character(char_len)     :: nlname
 logical (kind=log_kind) :: file_exists

!set variables
 filename = 'unknown'
 xaxis = 'unknown'
 yaxis = 'unknown'
 zaxis = 'unknown'
 taxis = 'unknown'
 lon_name = 'unknown'
 lat_name = 'unknown'
 depth_name = 'unknown'
 time_name = 'unknown'
 uvel_name = 'unknown'
 vvel_name = 'unknown'
 wvel_name = 'unknown'
 saln_name = 'unknown'
 temp_name = 'unknown'
 dens_name = 'unknown'
 ssh_name= 'unknown'
 zstart = 0
 zend = 0
 velocity_conversion_factor =1
 depth_conversion_factor = 1
 wvel_positive_direction = 'unknown' 
 angle_file='unknown'
 time_units='unknown'
 fill_value = 0
 was2d = .false.

 write(nlname,'(A,I0,A)') 'nest_', nest_n, '.nml' 

!open file
 CALL get_unit(iunit)
 INQUIRE(FILE=trim(fileinput)//nlname, EXIST=file_exists)
 IF (file_exists) THEN
   open(iunit, file=trim(fileinput)//nlname, status='old',form='formatted')
 ELSE
   print *, "Error: File ", trim(fileinput)//trim(nlname)," does not exist"
   stop
 ENDIF
!read file
 read(iunit, nml=nest_input)
!close file
 CALL release_unit(iunit)
 IF (filename .eq. '') THEN
   filename = 'unknown'
 ENDIF
 IF (zaxis .eq. '') THEN
   zaxis = 'unknown'
 ENDIF
 IF (wvel_name .eq. '') THEN
   wvel_name = 'unknown'
   wvel_positive_direction = 'unknown'   
 ENDIF
 IF (dens_name .eq. '') THEN
   dens_name = 'unknown'
 ENDIF
 IF (temp_name .eq. '') THEN
   temp_name = 'unknown'
 ENDIF
 IF (saln_name .eq. '') THEN
   saln_name = 'unknown'
 ENDIF
 IF (ssh_name .eq. '') THEN
   ssh_name = 'unknown'
 ENDIF

 IF (filename .ne. 'unknown') THEN
  IF (time_units .ne. "months" .and. time_units .ne. "days" .and. time_units .ne. "seconds" .and. time_units .ne. "hours") THEN
    print *, 'time_units in the nestfile needs to be equal to: months, days, hours or seconds'
    stop
  ENDIF
 ENDIF
 
 IF (xaxis .eq. 'unknown' .or. xaxis .eq. '') THEN
  print *, 'xaxis in the nestfile has to have a value'
  stop
 ENDIF
 IF (yaxis .eq. 'unknown' .or. yaxis .eq. '') THEN
  print *, 'yaxis in the nestfile has to have a value'
  stop
 ENDIF
! IF (taxis .eq. 'unknown' .or. taxis .eq. '') THEN
!  print *, 'taxis in the nestfile has to have a value'
!  stop
! ENDIF
 IF (lon_name .eq. 'unknown' .or. lon_name .eq. '') THEN
  print *, 'lon_name in the nestfile has to have a value'
  stop
 ENDIF
 IF (lat_name .eq. 'unknown' .or. lat_name .eq. '') THEN
  print *, 'lat_name in the nestfile has to have a value'
  stop
 ENDIF
 IF (uvel_name .eq. 'unknown' .or. uvel_name .eq. '') THEN
  print *, 'uvel_name in the nestfile has to have a value'
  stop
 ENDIF
 IF (vvel_name .eq. 'unknown' .or. vvel_name .eq. '') THEN
  print *, 'vvel_name in the nestfile has to have a value'
  stop
 ENDIF
 IF (fill_value .eq. 0) THEN
  print *, 'fill_value in the nestfile has to have a value'
  stop
 ENDIF
 
 print *, 'fill_value and velocity_conversion_factor on read', fill_value, velocity_conversion_factor
END SUBROUTINE read_nest_file

!**************************************************************
!read longitude, latitude, depth and time data from url or local files
SUBROUTINE read_data 

 integer  (kind=int_kind) :: ncId, ncIdW
 character (char_len)     :: datafile, datafileW

!if method = opendap then read from url 
!if method = localfile then read from first nestfile in raw directory
 IF (filename .eq. 'unknown') THEN
  write(datafile,'(A,I0,A,I4.4,I2.2,I2.2,A)') &
  trim(filenest)//'raw/nest_',nest_n,'_',tstart_yy,tstart_mm,tstart_dd,'000000u.nc'
  IF (nc_file_exists(datafile) .eqv. .false.) THEN
    write(datafile,'(A,I0,A,I4.4,I2.2,I2.2,A)') &
    trim(filenest)//'raw/nest_',nest_n,'_',tstart_yy,tstart_mm,tstart_dd,'000000.nc'
  ENDIF 
 ELSE
    datafile = filename
 ENDIF

!Open file for read access
 print *, 'Opening file for reading: ', trim(datafile) 
 CALL nc_open(datafile,ncId) 

!Get coordinate dimensions
!xaxis ..
 CALL nc_info_dim(datafile, ncId, xaxis, gidm)
!yaxis ..
 CALL nc_info_dim(datafile, ncId, yaxis, gjdm)
!zaxis ..
 IF (zaxis .ne. 'unknown') THEN 
  CALL nc_info_dim(datafile, ncId, zaxis, gkdm)
  hasDepth = .true.
 ELSE
  gkdm = 1
  hasDepth = .false.
 ENDIF
!taxis ..
 IF (taxis .ne. 'unknown') THEN 
  CALL nc_info_dim(datafile, ncId, taxis, gtdm)
  hasTime = .true.
  !here checks what is the gtdm
 ELSE
  gtdm = 1
  hasTime= .false.
 ENDIF

!check if lon is 1D or 2D
 CALL nc_info_var(datafile, ncId, lon_name, ndimsLon)
!check if lat is 1D or 2D
 CALL nc_info_var(datafile, ncId, lat_name, ndimsLat)
 IF (ndimsLon .eq. 1 .and. ndimsLat .eq. 1) THEN
  grid2d = .false.
 ELSE
  IF (ndimsLon .eq. 2 .and. ndimsLat .eq. 2) THEN
   grid2d = .true.
  ELSE
   print *, 'Longitude and Latitude have a dimension that is not supported' 
   stop
  ENDIF
 ENDIF

!allocate temporary coordinate arrays  
 IF(grid2d) THEN
   allocate (tmpLon2d(gidm,gjdm))
   allocate (tmpLat2d(gidm,gjdm))
 ELSE
   allocate (tmpLon1d(gidm))
   allocate (tmpLat1d(gjdm))
 ENDIF 
 allocate (tmpDepth(gkdm))
 allocate (tmpDepthW(gkdm))
 allocate (tmpTime(gtdm))
 allocate (tmpDoubleTime(gtdm))

!get longitude values
 IF(grid2d) THEN
  CALL nc_read2d(datafile, ncId, lon_name, tmpLon2d, gidm, gjdm)
 ELSE
  CALL nc_read1d(datafile, ncId, lon_name, tmpLon1d, gidm)
  IF (tmpLon1d(1) .eq. (tmpLon1d(gidm)-360)) THEN
   gidm = gidm-1
  ENDIF
 ENDIF
 print *, 'Succesfully read data: Longitude'

!get latitude values
 latDec = .false.
 IF(grid2d) THEN
  CALL nc_read2d(datafile, ncId, lat_name, tmpLat2d, gidm, gjdm)
 ELSE
  CALL nc_read1d(datafile, ncId, lat_name, tmpLat1d, gjdm)
  IF (tmpLat1d(1) .gt. tmpLat1d(2)) THEN
    LatDec = .true.
  ENDIF
 ENDIF
 print *, 'Succesfully read data: Latitude'

!get depth values
 IF (hasDepth) THEN
  CALL nc_read1d(datafile, ncId, depth_name, tmpDepth, gkdm)
! handle inverted z axis conventions
  IF (zaxis_positive_direction .eq. 'up') THEN 
   tmpDepth=-1.*tmpDepth
  ELSE IF (zaxis_positive_direction .ne. 'down') THEN
   print *, 'zaxis_positive_direction in the nestfile has to have the value up or down'
   stop
  ENDIF
  tmpDepth = tmpDepth * depth_conversion_factor
  print *, 'Succesfully read data: Depth'
 ELSE
  tmpDepth = 0
 ENDIF

!get time values - convert double time to real*4 time
 IF (hasTime .and. filename .ne. 'unknown') THEN
   CALL nc_read1d_dbl(datafile, ncId, time_name, tmpDoubleTime, gtdm)
   tmpTime=real(tmpDoubleTime)
   print *, 'Succesfully read data: Time'
  ELSE
   tmpTime=1;
 ENDIF

!Check for B-grid where W values are on different points from u and v values
 bgrid = .false.
 IF (filename .eq. 'unknown' .and. wvel_name .ne. 'unknown' ) THEN
  write(datafileW,'(A,I0,A,I4.4,I2.2,I2.2,A)') &
       trim(filenest)//'raw/nest_',nest_n,'_',tstart_yy,tstart_mm,tstart_dd,'000000w.nc'
  CALL nc_open(datafileW,ncIdW) 
! get depthdata from Wfile
  CALL nc_read1d(datafileW, ncIdW, depth_name, tmpDepthW, gkdm)
! handle inverted z axis conventions
  IF (zaxis_positive_direction .eq. 'up') THEN 
   tmpDepthW=-1.*tmpDepthW
  ENDIF
  tmpDepthW = tmpDepthW * depth_conversion_factor
  CALL nc_close(datafileW,ncIdW)
  IF (tmpDepthW(1) .ne. tmpDepth(1)) THEN
   bgrid=.true.
  ENDIF
  IF (bgrid .eqv. .true.) THEN
   print *, 'Assuming B-grid for vertical velocity: regridding W'
  ELSE
   print *, 'Vertical velocity is not on a B-Grid'
  ENDIF
 ENDIF

!Close file after data read 
 CALL nc_close(datafile,ncId)
 
END SUBROUTINE read_data

!**************************************************************
!SUBROUTINE that creates an extra metadatafile to save which files are already downloaded
SUBROUTINE create_filedownload(nestname)

 character(len=*),intent(in)   :: nestname
 integer (kind=int_kind) :: iunit
 character(char_len)     :: downlname

 write(downlname,'(A,I0,A)') 'download_',nest_n,'.txt' 
 CALL get_unit(iunit)

 IF ((first_time_downl) .and. (numFiles .le. 0)) THEN
! if metafile not exists then create a new file

  open(iunit, file=trim(filenest)//downlname)
! write data to metafile
  write(iunit,'(A)') filename
  write(iunit,*) xstart
  write(iunit,*) xend
  write(iunit,*) ystart
  write(iunit,*) yend
  write(iunit,*) zstart
  write(iunit,*) zend
  write(iunit,'(A)') zaxis_positive_direction
  write(iunit,*) time_step
  write(iunit,'(A)') time_units
  write(iunit,'(A)') lon_name
  write(iunit,'(A)') lat_name
  write(iunit,'(A)') depth_name
  write(iunit,*) depth_conversion_factor
  write(iunit,'(A)') time_name
  write(iunit,'(A)') uvel_name
  write(iunit,'(A)') vvel_name
  write(iunit,'(A)') wvel_name
  write(iunit,'(A)') wvel_positive_direction
  write(iunit,*) velocity_conversion_factor
  write(iunit,'(A)') dens_name
  write(iunit,'(A)') temp_name
  write(iunit,'(A)') saln_name
  write(iunit,'(A)') ssh_name
  write(iunit,*) fill_value
  write(iunit,'(A)') nestname
  first_time_downl = .false.
 ELSE
! if metafile already exists then append data
  open(iunit, file=trim(filenest)//downlname,access = 'APPEND')
  write(iunit,'(A)') nestname
 ENDIF
!close file
 CALL release_unit(iunit)

END SUBROUTINE create_filedownload

!**************************************************************
!reads which datafiles are already downloaded
!down_nest stores the names of the datafiles that are downloaded
!numFiles is the number of files that is already downloaded
!if the nestfile is the same then these datafiles do not have to be downloaded again
!if the nestfile is different then all datafiles have to be downloaded again
SUBROUTINE read_filedownload
  
 integer (kind=int_kind) :: iunit, numLines, i,downl_timeStep
 real (kind = real_kind) :: downl_xstart, downl_xend,downl_ystart, downl_yend,downl_zstart, downl_zend,&
                            downl_velFactor,downl_depthFactor,downl_fillvalue
 logical (kind=log_kind) :: file_exists
 character(char_len)     :: downlname,downl_filename, downl_timeUnits,downl_zDirection,downl_lon,downl_lat, & 
                            downl_depth,downl_time,downl_uvel, downl_vvel, downl_wvel,downl_wDirection, &  
                            downl_dens, downl_temp,downl_saln, downl_ssh

 first_time_downl = .true.

 write(downlname,'(A,I0,A)') 'download_',nest_n,'.txt' 
 INQUIRE(FILE=trim(filenest)//downlname, EXIST=file_exists)
 IF (file_exists) THEN     
! get number of lines in file
  CALL getsize(trim(filenest)//downlname,numLines)
! get number of downloaded files
  numFiles = numLines - numLinesBeforeFiles
  IF (numFiles .ge. 0) THEN
   allocate(downl_nest(numFiles))

!  open file
   CALL get_unit(iunit)
   open(iunit, file=trim(filenest)//downlname)

!  read file
   read(iunit,'(A)') downl_filename
   read(iunit,*) downl_xstart
   read(iunit,*) downl_xend
   read(iunit,*) downl_ystart
   read(iunit,*) downl_yend
   read(iunit,*) downl_zstart
   read(iunit,*) downl_zend
   read(iunit,'(A)') downl_zDirection   
   read(iunit,*) downl_timeStep
   read(iunit,'(A)') downl_timeUnits
   read(iunit,'(A)') downl_lon
   read(iunit,'(A)') downl_lat
   read(iunit,'(A)') downl_depth
   read(iunit,*) downl_depthFactor
   read(iunit,'(A)') downl_time
   read(iunit,'(A)') downl_uvel
   read(iunit,'(A)') downl_vvel
   read(iunit,'(A)') downl_wvel
   read(iunit,'(A)') downl_wDirection
   read(iunit,*) downl_velFactor
   read(iunit,'(A)') downl_dens
   read(iunit,'(A)') downl_temp
   read(iunit,'(A)') downl_saln
   read(iunit,'(A)') downl_ssh   
   read(iunit,*) downl_fillvalue

!  check if nestfile did not change
   IF ((downl_filename .eq. filename) .and.&
     (abs(downl_xstart - xstart) .lt. 0.01).and.&
     (abs(downl_xend - xend).lt. 0.01).and.&
     (abs(downl_ystart - ystart).lt. 0.01).and. &
     (abs(downl_yend - yend).lt. 0.01).and.&
     (abs(downl_zstart - zstart).lt. 0.01).and.&
     (abs(downl_zend - zend).lt. 0.01) .and.  &  
     (downl_zDirection .eq. zaxis_positive_direction) .and.&
     (downl_timeStep .eq. time_step) .and.&
     (downl_timeUnits .eq. time_units) .and.&
     (downl_lon .eq. lon_name) .and.&
     (downl_lat .eq. lat_name) .and. &
     (downl_depth .eq. depth_name) .and.&
     (downl_time .eq. time_name) .and.&
     (downl_uvel .eq. uvel_name) .and.&
     (downl_vvel .eq. vvel_name) .and.&
     (downl_wvel .eq. wvel_name) .and.&
     (downl_dens .eq. dens_name) .and.&
     (downl_temp .eq. temp_name) .and.&
     (downl_saln .eq. saln_name) .and.&
     (downl_ssh .eq. ssh_name) .and.&
     (abs(downl_fillvalue - fill_value).lt. 0.01) ) THEN

!     if the nestfile did not change get the names of the downloaded files
      DO i = 1, numFiles
        read(iunit,'(A)') downl_nest(i)
      ENDDO
   ELSE
     numFiles = 0
   ENDIF
!  close file
   CALL release_unit(iunit)
  ELSE
    allocate(downl_nest(1))
  ENDIF
 ELSE !if not file_exists
  numFiles = 0
  allocate(downl_nest(1))
 ENDIF

END SUBROUTINE read_filedownload

!**************************************************************
!handle longitude values to 0<x<360
SUBROUTINE change_longitudes

 integer (kind=int_kind)  :: i,j  

 DO WHILE (xstart .lt. 0) 
   xstart = xstart + 360
 ENDDO
 DO WHILE (xstart .ge. 360)
   xstart = xstart - 360
 ENDDO
 DO WHILE (xend .lt. 0) 
   xend = xend + 360
 ENDDO
 DO WHILE (xend .ge. 360)
   xend = xend - 360
 ENDDO

 IF (grid2d) THEN
  DO i=1,gidm
   DO j=1,gjdm
    DO WHILE (tmpLon2d(i,j) .ge. 360)
      tmpLon2d(i,j) = tmpLon2d(i,j) - 360
    ENDDO
    DO WHILE (tmpLon2d(i,j) .lt. 0)
      tmpLon2d(i,j) = tmpLon2d(i,j) + 360
    ENDDO
   ENDDO
  ENDDO
 ELSE
  DO i=1,gidm
   DO WHILE (tmpLon1d(i) .ge. 360)
     tmpLon1d(i) = tmpLon1d(i) - 360
   ENDDO
   DO WHILE (tmpLon1d(i) .lt. 0)
     tmpLon1d(i) = tmpLon1d(i) + 360
   ENDDO
  ENDDO
 ENDIF

END SUBROUTINE change_longitudes

!**************************************************************
!define new grid if lon and lat are 2d
SUBROUTINE define_new_grid

 logical (kind=log_kind) :: possible1d
 integer (kind=int_kind) :: i,j , ii, jj,small_i, small_j 
 real (kind=real_kind)   :: small_surface=bignum,minLat, minLon, maxLat, maxLon,surface

!check if 2D lon and lat can be changed to 1D lon and lat
 possible1d = .true.
 DO i=1,gidm
  DO j=1,gjdm
   IF (tmpLon2d(i,j) .gt. xstart .and. tmpLon2d(i,j) .lt. xend .and. &
       tmpLat2d(i,j) .gt. ystart .and. tmpLat2d(i,j) .lt. yend) THEN
    IF (tmpLon2d(i,1) .ne. tmpLon2d(i,j)) THEN
     possible1d = .false.
    ENDIF
    IF (tmpLat2d(1,j) .ne. tmpLat2d(i,j)) THEN
     possible1d = .false.
    ENDIF
   ENDIF
  ENDDO
 ENDDO
!if 2D lon and lat can be changed to 1D lon and lat then do it
 IF (possible1d) THEN   
  print *, "transform longitude and latitude from 2d -> 1d"
  was2d = .true.
  allocate (tmpLon1d(gidm))
  allocate (tmpLat1d(gjdm))
  grid2d = .false.
  DO i=1,gidm
   tmpLon1d(i) = tmpLon2d(i,1)
  ENDDO
  DO j=1,gjdm
   tmpLat1d(j) = tmpLat2d(1,j)
  ENDDO
 ENDIF

!if grid is still 2d then define new grid
 IF (grid2d) THEN

! get the bounding box of each grid cell in the original grid.
  allocate (src_bbox_lon(gidm-1,gjdm-1,4))
  allocate (src_bbox_lat(gidm-1,gjdm-1,4))
  allocate (angle(gidm,gjdm))
  DO j=1,gjdm-1 
   DO i=1,gidm-1
     src_bbox_lon(i,j,1)=  tmpLon2d(i,j)
     src_bbox_lat(i,j,1)=  tmpLat2d(i,j)
     src_bbox_lon(i,j,2)=  tmpLon2d(i+1,j)
     src_bbox_lat(i,j,2)=  tmpLat2d(i+1,j)
     src_bbox_lon(i,j,3)=  tmpLon2d(i+1,j+1)
     src_bbox_lat(i,j,3)=  tmpLat2d(i+1,j+1)
     src_bbox_lon(i,j,4)=  tmpLon2d(i,j+1)
     src_bbox_lat(i,j,4)=  tmpLat2d(i,j+1)
   ENDDO
  ENDDO
     
! find the biggest bounding box around all gridcells in the original grid
  minLat = minval(tmpLat2d)
  maxLat = maxval(tmpLat2d)
  minLon = minval(tmpLon2d)
  maxLon = maxval(tmpLon2d)

! find the smallest grid cell in the original grid 
  DO j=1,gjdm-1  
   DO i=1,gidm-1
    surface = abs(minval(src_bbox_lon(i,j,:)) - maxval(src_bbox_lon(i,j,:))) &
           *  abs(minval(src_bbox_lat(i,j,:)) - maxval(src_bbox_lat(i,j,:)))   
    IF (surface .lt. small_surface) THEN
     small_i = i
     small_j = j
     small_surface = surface
    ENDIF
   ENDDO
  ENDDO

! calculate number of point in new grid
  gidm_new = int(abs(minLon-maxLon)/abs(minval(src_bbox_lon(small_i,small_j,:)) - maxval(src_bbox_lon(small_i,small_j,:))))
  gjdm_new = int(abs(minLat-maxLat)/abs(minval(src_bbox_lat(small_i,small_j,:)) - maxval(src_bbox_lat(small_i,small_j,:))))

! allocate longitude and latitude of new grid
  allocate (tmpLon_new (gidm_new))
  allocate (tmpLat_new (gjdm_new))

! fill the longitude and latitude arrays with the values of the new grid
  DO ii=1,gidm_new
   tmpLon_new(ii)=minLon+ii*abs(minval(src_bbox_lon(small_i,small_j,:)) - maxval(src_bbox_lon(small_i,small_j,:)))    
  ENDDO
  DO jj=1,gjdm_new
   tmpLat_new(jj)=minLat+jj*abs(minval(src_bbox_lat(small_i,small_j,:)) - maxval(src_bbox_lat(small_i,small_j,:)))
  ENDDO
  print *, "Data needs to be regridded" 
  print *, "Size old grid: ", gidm, " by ", gjdm
  print *, "Size new grid: ", gidm_new, " by ", gjdm_new
 ENDIF

END SUBROUTINE define_new_grid

!**************************************************************
!regrid W velocity if the original data was on a B-grid
SUBROUTINE regridW

 integer (kind=int_kind)   :: i,j,k
 real (kind = real_kind), allocatable :: wvel_bg(:,:,:,:), wvel_2d(:,:,:,:), &
     temp_bg(:,:,:,:), temp_2d(:,:,:,:), saln_bg(:,:,:,:), saln_2d(:,:,:,:), &
     dens_bg(:,:,:,:), dens_2d(:,:,:,:)

 allocate(wvel_2d(idm_out,jdm_out,kdm_out,1),wvel_bg(idm_out,jdm_out,kdm_out,1))
 IF (temp_name .ne. 'unknown' ) THEN
  allocate(temp_2d(idm_out,jdm_out,kdm_out,1),temp_bg(idm_out,jdm_out,kdm_out,1))
 ENDIF
 IF (saln_name .ne. 'unknown' ) THEN
  allocate(saln_2d(idm_out,jdm_out,kdm_out,1),saln_bg(idm_out,jdm_out,kdm_out,1))
 ENDIF
 IF (dens_name .ne. 'unknown' ) THEN
  allocate(dens_2d(idm_out,jdm_out,kdm_out,1),dens_bg(idm_out,jdm_out,kdm_out,1))
 ENDIF

 wvel_2d=wvel
 IF (temp_name .ne. 'unknown' ) THEN
  temp_2d=temp
 ENDIF
 IF (saln_name .ne. 'unknown' ) THEN
  saln_2d=saln
 ENDIF
 IF (dens_name .ne. 'unknown' ) THEN
  dens_2d=dens
 ENDIF
 DO k=1,kdm_out
  DO j=2,jdm_out
   DO i=2,idm_out
    wvel_2d(i,j,k,1)=1./4.*(wvel(i,j,k,1)+wvel(i-1,j,k,1)+wvel(i,j-1,k,1)+wvel(i-1,j-1,k,1))
    IF (temp_name .ne. 'unknown' ) THEN
     temp_2d(i,j,k,1)=1./4.*(temp(i,j,k,1)+temp(i-1,j,k,1)+temp(i,j-1,k,1)+temp(i-1,j-1,k,1))
    ENDIF
    IF (saln_name .ne. 'unknown' ) THEN
     saln_2d(i,j,k,1)=1./4.*(saln(i,j,k,1)+saln(i-1,j,k,1)+saln(i,j-1,k,1)+saln(i-1,j-1,k,1))
    ENDIF
   ENDDO
    IF (dens_name .ne. 'unknown' ) THEN
     dens_2d(i,j,k,1)=1./4.*(dens(i,j,k,1)+dens(i-1,j,k,1)+dens(i,j-1,k,1)+dens(i-1,j-1,k,1))
    ENDIF
  ENDDO
 ENDDO
 DO j=1,jdm_out
  DO i=1,idm_out
   wvel_bg(i,j,1,1)=wvel_2d(i,j,1,1)/tmpDepthW(1)*tmpDepth(1)
   IF (temp_name .ne. 'unknown' ) THEN
    temp_bg(i,j,1,1)=temp_2d(i,j,1,1)/tmpDepthW(1)*tmpDepth(1)
   ENDIF
   IF (saln_name .ne. 'unknown' ) THEN
    saln_bg(i,j,1,1)=saln_2d(i,j,1,1)/tmpDepthW(1)*tmpDepth(1)
   ENDIF
   IF (dens_name .ne. 'unknown' ) THEN
    dens_bg(i,j,1,1)=dens_2d(i,j,1,1)/tmpDepthW(1)*tmpDepth(1)
   ENDIF
   DO k=2,kdm_out
    wvel_bg(i,j,k,1)=wvel_2d(i,j,k-1,1)+(wvel_2d(i,j,k,1)-wvel_2d(i,j,k-1,1))/&
                     (tmpDepthW(k)-tmpDepthW(k-1))*(tmpDepth(k)-tmpDepthW(k-1))
    IF (temp_name .ne. 'unknown' ) THEN
     temp_bg(i,j,k,1)=temp_2d(i,j,k-1,1)+(temp_2d(i,j,k,1)-temp_2d(i,j,k-1,1))/&
                     (tmpDepthW(k)-tmpDepthW(k-1))*(tmpDepth(k)-tmpDepthW(k-1))
    ENDIF
    IF (saln_name .ne. 'unknown' ) THEN
     saln_bg(i,j,k,1)=saln_2d(i,j,k-1,1)+(saln_2d(i,j,k,1)-saln_2d(i,j,k-1,1))/&
                     (tmpDepthW(k)-tmpDepthW(k-1))*(tmpDepth(k)-tmpDepthW(k-1))
    ENDIF
    IF (dens_name .ne. 'unknown' ) THEN
     dens_bg(i,j,k,1)=dens_2d(i,j,k-1,1)+(dens_2d(i,j,k,1)-dens_2d(i,j,k-1,1))/&
                     (tmpDepthW(k)-tmpDepthW(k-1))*(tmpDepth(k)-tmpDepthW(k-1))
    ENDIF
   ENDDO
  ENDDO
 ENDDO

 wvel=wvel_bg
 deallocate(wvel_2d,wvel_bg)
 IF (temp_name .ne. 'unknown' ) THEN
  temp=temp_bg
  deallocate(temp_2d,temp_bg)
 ENDIF
 IF (saln_name .ne. 'unknown' ) THEN
  saln=saln_bg
  deallocate(saln_2d,saln_bg)
 ENDIF
 IF (dens_name .ne. 'unknown' ) THEN
  dens=dens_bg
  deallocate(dens_2d,dens_bg)
 ENDIF
END SUBROUTINE regridW

!**************************************************************
!calculate weights that are necessary for regridding
SUBROUTINE calculate_weights

 integer (kind=int_kind)  :: &
     i,j ,ii,jj,k,l,polySides, crossedLines,iter 
 real (kind=real_kind)    :: &
     plat, plon, &! lat/lon coords of destination point
     iguess, jguess, &  ! current guess for bilinear coordinate
     deli, delj,& ! corrections to i,j
     dth1, dth2, dth3,& ! some latitude  differences
     dph1, dph2, dph3,& ! some longitude differences
     dthp, dphp,& ! difference between point and sw corner
     mat1, mat2, mat3, mat4,& ! matrix elements
     determinant! matrix determinant  
 real (kind=real_kind) , dimension(4) :: &
     src_lats,src_lons 


 allocate (w(idm_out_new,jdm_out_new,4))
 allocate (x(idm_out_new,jdm_out_new,4))
 allocate (y(idm_out_new,jdm_out_new,4))
 polySides = 4
 w=0

 ! for all dest points
 print *, "start calculating weight points... "
 DO jj=1,jdm_out_new
  DO ii=1,idm_out_new
   plon=lon1d(ii)
   plat=lat1d(jj)
   !for all src points
   DO j=1,gjdm-1
    DO i=1,gidm-1
!    calculate in which gridcell plon and plat are
     crossedLines = 0
     l = polySides
     DO k=1, polySides
      IF ((src_bbox_lat(i,j,k).gt.pLat).neqv. (src_bbox_lat(i,j,l).gt.pLat)) THEN
       IF ( pLon .lt. (src_bbox_lon(i,j,l)-src_bbox_lon(i,j,k)) * (pLat-src_bbox_lat(i,j,k))/ &
          (src_bbox_lat(i,j,l)-src_bbox_lat(i,j,k))+ src_bbox_lon(i,j,k))THEN
            crossedLines = crossedLines + 1
       ENDIF
      ENDIF
      l=k
     ENDDO !end loop: k=1, polySides

!    if plon and plat are in gridcell, calculate the 4 weights
     IF (mod(crossedLines,2) .eq. 1) THEN 
      src_lats(1)=src_bbox_lat(i,j,1)
      src_lats(2)=src_bbox_lat(i,j,2)
      src_lats(3)=src_bbox_lat(i,j,3)
      src_lats(4)=src_bbox_lat(i,j,4)

      src_lons(1)=src_bbox_lon(i,j,1)
      src_lons(2)=src_bbox_lon(i,j,2)
      src_lons(3)=src_bbox_lon(i,j,3)
      src_lons(4)=src_bbox_lon(i,j,4)

      x(ii,jj,1)= i
      x(ii,jj,2)= i+1
      x(ii,jj,3)= i+1
      x(ii,jj,4)= i

      y(ii,jj,1)= j
      y(ii,jj,2)= j
      y(ii,jj,3)= j+1
      y(ii,jj,4)= j+1

      dth1 = src_lats(2) - src_lats(1)
      dth2 = src_lats(4) - src_lats(1)
      dth3 = src_lats(3) - src_lats(2) - dth2

      dph1 = src_lons(2) - src_lons(1)
      dph2 = src_lons(4) - src_lons(1)
      dph3 = src_lons(3) - src_lons(2)

      IF (dph1 >  pi) dph1 = dph1 - pi2
      IF (dph2 >  pi) dph2 = dph2 - pi2
      IF (dph3 >  pi) dph3 = dph3 - pi2
      IF (dph1 < -pi) dph1 = dph1 + pi2
      IF (dph2 < -pi) dph2 = dph2 + pi2
      IF (dph3 < -pi) dph3 = dph3 + pi2
      dph3 = dph3 - dph2

      iguess = 0.5
      jguess = 0.5
 
      DO iter=1,max_iter
       dthp = plat - src_lats(1) - dth1*iguess - dth2*jguess - dth3*iguess*jguess
       dphp = plon - src_lons(1)
       IF (dphp >  3.*pih) dphp = dphp - pi2
       IF (dphp < -3.*pih) dphp = dphp + pi2

       dphp = dphp - dph1*iguess - dph2*jguess - dph3*iguess*jguess
       mat1 = dth1 + dth3*jguess
       mat2 = dth2 + dth3*iguess
       mat3 = dph1 + dph3*jguess
       mat4 = dph2 + dph3*iguess
       determinant = mat1*mat4 - mat2*mat3

       delj = (mat1*dphp - dthp*mat3)/determinant
       deli = (dthp*mat4 - mat2*dphp)/determinant

       IF (abs(deli) < converge .and. abs(delj) < converge) exit 

       iguess = iguess + deli
       jguess = jguess + delj
      ENDDO

      IF (iter <= max_iter) THEN

       w(ii,jj,1) = (1.-iguess)*(1.-jguess)
       w(ii,jj,2) = iguess*(1.-jguess)
       w(ii,jj,3) = iguess*jguess
       w(ii,jj,4) = (1.-iguess)*jguess
       exit
      ENDIF
     ENDIF
    ENDDO !gidm-1
   ENDDO !gjdm-1
  ENDDO !gidm_new
 ENDDO !gjdm_new
 print *, "Finished "

END SUBROUTINE calculate_weights

!**************************************************************
!regrid the data with the calculated weights
SUBROUTINE regrid

 integer (kind=int_kind)  :: ii,jj,kk,l,fillval_bool, outside_bool,ncId
 real (kind=real_kind)    :: uvel_temp, vvel_temp, wvel_temp,dens_temp, temp_temp, saln_temp,ssh_temp
 character (char_len)     :: anglefile

!angles of original grid. The velocities have to be recalculated with help of the angles.
 angle=0  
 IF (angle_file .ne. 'unknown') THEN
! open file with angles
  anglefile = trim(fileinput)//trim(angle_file)//".nc"
  CALL nc_open(anglefile,ncId) 
! get angles
  CALL nc_read2d(anglefile,ncId,"angle",angle,gidm,gjdm)
! close file with angles
  CALL nc_close(anglefile, ncId)
 ENDIF

 DO ii = 1, idm_out_new
  DO jj = 1, jdm_out_new
   DO kk = 1, kdm_out
    fillval_bool = 0
    outside_bool = 0
    uvel_temp = 0 
    vvel_temp = 0
    wvel_temp = 0
    dens_temp = 0
    temp_temp = 0 
    saln_temp = 0
    ssh_temp = 0

    DO l=1, 4
     IF (w(ii,jj,l) .eq. 0) THEN
       outside_bool = outside_bool+1
     ELSE
      IF ((uvel(x(ii,jj,l),y(ii,jj,l),kk,1) .ne. fill_valueNew) .and. (vvel(x(ii,jj,l),y(ii,jj,l),kk,1) .ne. fill_valueNew)) THEN
!      regrid uvel
       uvel_temp = uvel_temp + ((uvel(x(ii,jj,l),y(ii,jj,l),kk,1) * cos(angle(x(ii,jj,l),y(ii,jj,l)))) - &
                               ( vvel(x(ii,jj,l),y(ii,jj,l),kk,1) * sin(angle(x(ii,jj,l),y(ii,jj,l))))) * w(ii,jj,l)
!      regrid vvel
       vvel_temp = vvel_temp + (( uvel(x(ii,jj,l),y(ii,jj,l),kk,1) * sin(angle(x(ii,jj,l),y(ii,jj,l)))) + &
                                ( vvel(x(ii,jj,l),y(ii,jj,l),kk,1) * cos(angle(x(ii,jj,l),y(ii,jj,l))))) * w(ii,jj,l)
!      regrid wvel
       wvel_temp = wvel_temp + wvel(x(ii,jj,l),y(ii,jj,l),kk,1) * w(ii,jj,l)
!      regrid density
       IF (dens_name .ne. 'unknown' ) THEN
        dens_temp = dens_temp + dens(x(ii,jj,l),y(ii,jj,l),kk,1) * w(ii,jj,l)
       ENDIF
!      regrid temp
       IF (temp_name .ne. 'unknown' ) THEN
        temp_temp = temp_temp + temp(x(ii,jj,l),y(ii,jj,l),kk,1) * w(ii,jj,l)
       ENDIF
!      regrid saln
       IF (saln_name .ne. 'unknown' ) THEN
        saln_temp = saln_temp + saln(x(ii,jj,l),y(ii,jj,l),kk,1) * w(ii,jj,l)
       ENDIF
!      regrid ssh
       IF (ssh_name .ne. 'unknown' ) THEN
        IF (kk .eq. 1) THEN
         ssh_temp = ssh_temp + ssh(x(ii,jj,l),y(ii,jj,l),1) * w(ii,jj,l)
        ENDIF
       ENDIF
      ELSE
       fillval_bool = fillval_bool + 1
      ENDIF
     ENDIF
    ENDDO !end do: DO l=1, 4

    IF (outside_bool .eq. 4) THEN
 !   points outside grid
     uvel_new(ii,jj,kk,1) = 0
     vvel_new(ii,jj,kk,1) = 0
     wvel_new(ii,jj,kk,1) = 0
     IF (dens_name .ne. 'unknown' ) THEN
      dens_new(ii,jj,kk,1) = 0
     ENDIF
     IF (temp_name .ne. 'unknown' ) THEN
      temp_new(ii,jj,kk,1) = 0
     ENDIF
     IF (saln_name .ne. 'unknown' ) THEN
      saln_new(ii,jj,kk,1) = 0
     ENDIF
     IF (ssh_name .ne. 'unknown' ) THEN
      IF (kk .eq. 1) THEN
       ssh_new(ii,jj,1) = 0
      ENDIF
     ENDIF
    ELSE
     IF (fillval_bool .eq. 4) THEN
!     points on land
      uvel_new(ii,jj,kk,1) = fill_valueNew
      vvel_new(ii,jj,kk,1) = fill_valueNew
      wvel_new(ii,jj,kk,1) = fill_valueNew
      IF (dens_name .ne. 'unknown' ) THEN
       dens_new(ii,jj,kk,1) = fill_valueNew
      ENDIF
      IF (temp_name .ne. 'unknown' ) THEN
       temp_new(ii,jj,kk,1) = fill_valueNew
      ENDIF
      IF (saln_name .ne. 'unknown' ) THEN
       saln_new(ii,jj,kk,1) = fill_valueNew
      ENDIF
      IF (ssh_name .ne. 'unknown' ) THEN
       IF (kk .eq. 1) THEN  
        ssh_new(ii,jj,1) = fill_valueNew
       ENDIF
      ENDIF
     ELSE
!     points in water
      uvel_new(ii,jj,kk,1) = uvel_temp
      vvel_new(ii,jj,kk,1) = vvel_temp
      wvel_new(ii,jj,kk,1) = wvel_temp
      IF (dens_name .ne. 'unknown' ) THEN
       dens_new(ii,jj,kk,1) = dens_temp
      ENDIF
      IF (temp_name .ne. 'unknown' ) THEN
       temp_new(ii,jj,kk,1) = temp_temp
      ENDIF
      IF (saln_name .ne. 'unknown' ) THEN
       saln_new(ii,jj,kk,1) = saln_temp
      ENDIF
      IF (ssh_name .ne. 'unknown' ) THEN
       IF (kk .eq. 1) THEN 
        ssh_new(ii,jj,1) = ssh_temp
       ENDIF
      ENDIF
     ENDIF
    ENDIF

   ENDDO !end loop: do kk = 1, kdm_out
  ENDDO !end loop: do jj = 1, jdm_out_new
 ENDDO !end loop: do ii = 1, idm_out_new

END SUBROUTINE regrid


!**************************************************************
!find indices for Depth or Time or 1D lon and 1D lat
SUBROUTINE locateIndex(array, num, point,pointName, pointIndex)
 
 integer (kind=int_kind), intent(in) :: num
 real (kind = real_kind), intent(in)  :: array(num)
 real (kind = real_kind), intent(in) :: point 
 character(len=*), intent(in)  :: pointName
 integer (kind=int_kind), intent(out):: pointIndex    
 integer (kind=int_kind) :: k
 logical (kind=log_kind) :: foundIndex    


!find index
 foundIndex = .false.
 IF ((pointName .eq. 'Lat') .and. (LatDec)) THEN
  DO k=2,num 
   IF (foundIndex .eqv. .false.) THEN
    IF ((point .gt. array(k)) .and. (point .le. array(k-1))) THEN
     foundIndex = .true.
     pointIndex = k-1
    ENDIF
   ENDIF
  ENDDO 
 ELSE
  DO k=2,num 
   IF (foundIndex .eqv. .false.) THEN
    !  print *,"point ",point,"array k", array(k), "array K-1 ", array(k-1)
      IF ((point .lt. array(k)) .and. (point .ge. array(k-1))) THEN
       foundIndex = .true.
       pointIndex = k-1
      ENDIF  
   ENDIF
  ENDDO
 ENDIF 

 IF (foundIndex .eqv. .false.) THEN
  IF (abs(point-array(num)) .lt. 0.01) THEN
    foundIndex = .true.
    pointIndex = num
  ENDIF
 ENDIF

 IF (foundIndex .eqv. .false.) THEN
  IF (pointName .eq. 'Lon') THEN
   print *,  'The data for longitude is not available.' 
   print *,  'Choose a different value in the nest file.'
   print *,  'Available: ', array(1),' to ', array(num)
   print *,  'Your request: ', point
  ENDIF
  IF (pointName .eq. 'Lat') THEN
   print *,  'The data for latitude is not available.' 
   print *,  'Choose a different value in the nest file.'
   print *,  'Available: ', array(1),' to ', array(num)
   print *,  'Your request: ', point
  ENDIF
  IF (pointName .eq. 'Time') THEN
   print *,  'The data for time is not available.' 
   print *,  'Choose a different value in the nest file'
   print *,  'Available: ', array(1),' to ', array(num)
   print *,  'Your request: ', point
  ENDIF
  IF (pointName .eq. 'Depth') THEN
   print *,  'The data for depth is not available.' 
   print *,  'Choose a different value in the nest file'
   print *,  'Available: ', array(1),' to ', array(num)
   print *,  'Your request: ', point
  ENDIF
  stop
 ENDIF
END SUBROUTINE locateIndex

!**************************************************************
!subset the needed domain from the available datasets
SUBROUTINE subset_domain 

 real (kind = real_kind) :: stime, etime, difftime
 integer (kind=int_kind) :: timeloop,stime_int, etime_int  

!find indices for lat en lon
 IF (grid2d) THEN
  !if grid has to be regridded, take the whole region of the original grid
  !caculate the indices for the new grid
  istart = 1
  iend = gidm
  jstart = 1
  jend = gjdm
  CALL locateIndex(tmpLon_new,gidm_new,xstart,'Lon', istart_new)
  CALL locateIndex(tmpLon_new,gidm_new,xend,'Lon', iend_new)
  CALL locateIndex(tmpLat_new,gjdm_new,ystart,'Lat',jstart_new)  
  CALL locateIndex(tmpLat_new,gjdm_new,yend,'Lat', jend_new)
 ELSE
  CALL locateIndex(tmpLon1d,gidm,xstart,'Lon', istart)  
  CALL locateIndex(tmpLon1d,gidm,xend,'Lon', iend)
  CALL locateIndex(tmpLat1d,gjdm,ystart,'Lat', jstart)  
  CALL locateIndex(tmpLat1d,gjdm,yend,'Lat', jend)
 ENDIF

!find indices for depth
 CALL locateIndex(tmpDepth,gkdm,zstart,'Depth',kstart) 
 CALL locateIndex(tmpDepth,gkdm,zend,'Depth',kend) 

!find indices for time
 IF (filename .ne. 'unknown') THEN
   IF (time_units .eq. 'months') THEN
     CALL nmonths(tstart_mm,tstart_yy,jdate_mm,jdate_yy, stime_int)
     CALL nmonths(tend_mm,tend_yy,jdate_mm,jdate_yy, etime_int)
   ELSE 
     CALL ndays(tstart_mm,tstart_dd,tstart_yy,jdate_mm,jdate_dd,jdate_yy,stime_int)
     CALL ndays(tend_mm,tend_dd,tend_yy,jdate_mm,jdate_dd,jdate_yy,etime_int)
   ENDIF
   stime = real(stime_int)
   etime = real(etime_int)

   IF (time_units .eq. 'seconds') THEN
     stime=stime*real(secs_in_day)
     etime=etime*real(secs_in_day)
   ENDIF
   IF (time_units .eq. 'months') THEN
     IF (tstart_dd .eq. 15) stime = stime + 0.5
   ENDIF
   IF (time_units .eq. 'hours') THEN
     stime=stime*real(24)
     etime=etime*real(24)
     !print *, "stime: ", stime," etime: ",etime
   ENDIF
  
   CALL locateIndex(tmpTime,gtdm,stime,'Time',lstart)
   CALL locateIndex(tmpTime,gtdm,etime,'Time',lend)
 ENDIF

!calculate size of the subset
!if istart > iend wrap around domain - split in two halves
 IF (istart .gt. iend) THEN
   IF (grid2d) THEN
     print *, "This part of cms is not implemented yet"
     stop
   ELSE
     idm_out=iend + (gidm-istart)+1
     ipart1=(gidm-istart)+1
   ENDIF
 ELSE
   IF (grid2d) THEN
     idm_out_new = iend_new-istart_new+1
     idm_out=iend-istart+1
   ELSE
     idm_out=iend-istart+1
   ENDIF
 ENDIF
 IF (grid2d) THEN
   jdm_out_new=jend_new-jstart_new+1
   jdm_out=jend-jstart+1
 ELSE
   jdm_out=jend-jstart+1
 ENDIF
 kdm_out=kend-kstart+1
 IF (filename .ne. 'unknown') THEN
   tdm_out=lend-lstart+1
 ENDIF

 print *, 'Starting X Axis index: ', istart
 print *, 'Ending X Axis index  : ', iend
 print *, 'Starting Y Axis index: ', jstart
 print *, 'Ending Y Axis index  : ', jend
 print *, 'Starting Z Axis index: ', kstart
 print *, 'Ending Z Axis index  : ', kend
 IF (filename .ne. 'unknown') THEN
   print *, 'Starting T Axis index: ', lstart
   print *, 'Ending T Axis index  : ', lend
 ENDIF

!allocate coordinate arrays
 IF (grid2d) THEN
  allocate(lon1d(idm_out_new))
  allocate(lat1d(jdm_out_new))
 ELSE
  allocate(lon1d(idm_out))
  allocate(lat1d(jdm_out))
 ENDIF     
 allocate(depth(kdm_out))
 allocate(time(tdm_out))

!fill lon array
 IF (istart .gt. iend) THEN
  IF (grid2d) THEN
   print *, "This part of cms is not implemented yet"
   stop
  ELSE
   lon1d(1:ipart1)=tmpLon1d(istart:gidm) 
   lon1d(ipart1+1:idm_out)=tmpLon1d(1:iend)
  ENDIF
 ELSE  
  IF (grid2d) THEN
   lon1d=tmpLon_new(istart_new:iend_new)
  ELSE
   lon1d=tmpLon1d(istart:iend)
  ENDIF
 ENDIF

!fill lat array
 IF (grid2d) THEN
   lat1d=tmpLat_new(jstart_new:jend_new)
 ELSE
   lat1d=tmpLat1d(jstart:jend)
 ENDIF

!fill depth array
 depth=tmpDepth(kstart:kend)

!fill time array
 IF (filename .ne. 'unknown') THEN
  time=tmpTime(lstart:lend)
  DO timeloop = 2, tdm_out-1
    difftime = (time(timeloop+1) - time(timeloop))
    IF (time_units .eq. "days") THEN
     difftime = difftime * 86400
    ENDIF
    IF (time_units .eq. "months") THEN
     difftime = difftime * 2635200
    ENDIF
    IF (time_units .eq. "hours") THEN
     difftime = difftime * 3600
     !print *, "difftime: ", difftime
    ENDIF
    IF ((difftime .gt. (1.2*time_step)) .or. (difftime .lt. 0) .or. (difftime .lt. 0.8*(time_step))) THEN
      print *, "Error: CMS can only handle regular time steps between data. " &
      , "The field with the name ",trim(time_name), &
      " has a irregular time step between the data: ", &
      time(timeloop), " and ", time(timeloop+1)
    ENDIF
  ENDDO
 ENDIF
END SUBROUTINE subset_domain 

!**************************************************************
!called from SUBROUTINE make_nestfiles_opendap or
!from SUBROUTINE make_nestfiles_localfile
!create the files with the oceonographic data
SUBROUTINE create_nestfile(nestname, wvel_change, num) 
 character (len = *), intent(in)       :: nestname
 logical (kind=log_kind), intent(in)   :: wvel_change
 integer (kind=int_kind), intent(in)   :: num

 integer (kind=int_kind) :: size_idm, size_jdm, ncId, i, j, k   
 character (char_len)    :: units(4), year_start, month_start, day_start,total_start
 real (kind = real_kind), allocatable :: uvel_save(:,:,:,:), vvel_save(:,:,:,:), &
     wvel_save(:,:,:,:), dens_save(:,:,:,:),temp_save(:,:,:,:), &
     saln_save(:,:,:,:), ssh_save(:,:,:), lat_save(:)

! USE velocity_conversionfactor 
! change sign if wvelocity direction is upward
! make fillvalues 2^100 
  DO i=1,idm_out
    DO j=1,jdm_out
      DO k=1,kdm_out
!       change uvel
        !print *, 'uvel(i,j,k,1):', uvel(i,j,k,1), abs(uvel(i,j,k,1) - fill_value)
        !print *, 'fill_value', fill_value 
        !print *, 'velocity_conversion_factor', velocity_conversion_factor
        IF (abs(uvel(i,j,k,1) - fill_value) .lt. 0.01) THEN
          uvel(i,j,k,1) = 2.**100
          !print *, 'got into the if equal fill_value'        
        ELSE
          IF (isnan(uvel(i,j,k,1))) THEN
            uvel(i,j,k,1) = 2.**100
            !print *, 'got into the if equal nan'
          ELSE
            uvel(i,j,k,1) = uvel(i,j,k,1) * velocity_conversion_factor
            !print *, 'got into the if conversion'
          ENDIF
        ENDIF
        
        IF (abs(vvel(i,j,k,1) - fill_value) .lt. 0.01) THEN
          vvel(i,j,k,1) = 2.**100
        ELSE
          IF (isnan(uvel(i,j,k,1))) THEN
            vvel(i,j,k,1) = 2.**100
          ELSE
            vvel(i,j,k,1) = vvel(i,j,k,1) * velocity_conversion_factor
          ENDIF
        ENDIF

!      change wvel if there is a wvel
       IF (wvel_name .ne. 'unknown') THEN
         IF (abs(wvel(i,j,k,1) - fill_value) .lt. 0.01) THEN
           wvel(i,j,k,1) = 2.**100
         ELSE
           IF (isnan(wvel(i,j,k,1))) THEN
             wvel(i,j,k,1) = 2.**100
           ELSE
             wvel(i,j,k,1) = wvel(i,j,k,1) * velocity_conversion_factor
             IF (wvel_change) THEN
               wvel(i,j,k,1) = -1. * wvel(i,j,k,1)
             ENDIF
           ENDIF
         ENDIF  
       ENDIF

!      change dens
       IF (dens_name .ne. 'unknown' ) THEN
         IF (abs(dens(i,j,k,1) - fill_value) .lt. 0.01) THEN
           dens(i,j,k,1) = 2.**100
         ENDIF
       ENDIF

!      change temp
       IF (temp_name .ne. 'unknown' ) THEN
         IF (abs(temp(i,j,k,1) - fill_value) .lt. 0.01) THEN
           temp(i,j,k,1) = 2.**100
         ENDIF
       ENDIF

!      change saln
       IF (saln_name .ne. 'unknown' ) THEN
         IF (abs(saln(i,j,k,1) - fill_value) .lt. 0.01) THEN
           saln(i,j,k,1) = 2.**100
         ENDIF
       ENDIF

!      change ssh
       IF (ssh_name .ne. 'unknown' ) THEN
         IF (abs(ssh(i,j,1) - fill_value) .lt. 0.01) THEN
           ssh(i,j,1) = 2.**100
         ENDIF
       ENDIF
    ENDDO
   ENDDO
  ENDDO

  fill_valueNew = 2.**100

  IF (grid2d) THEN
    CALL regrid
  ENDIF

! flip latitudes if latitudes are decreasing.
  IF (LatDec) THEN
   allocate(lat_save(jdm_out))
   allocate(uvel_save(idm_out,jdm_out,kdm_out,1))
   allocate(vvel_save(idm_out,jdm_out,kdm_out,1))
   allocate(wvel_save(idm_out,jdm_out,kdm_out,1))
   allocate(dens_save(idm_out,jdm_out,kdm_out,1))
   allocate(temp_save(idm_out,jdm_out,kdm_out,1))
   allocate(saln_save(idm_out,jdm_out,kdm_out,1))
   allocate(ssh_save(idm_out,jdm_out,1))
   uvel_save = uvel
   vvel_save = vvel
   wvel_save =wvel
   dens_save = dens
   temp_save = temp
   saln_save = saln
   ssh_save = ssh
   lat_save = lat1d
   DO j=1,jdm_out
    uvel(:,j,:,1) = uvel_save(:,jdm_out-j+1,:,1)
    vvel(:,j,:,1) = vvel_save(:,jdm_out-j+1,:,1)
    wvel(:,j,:,1) = wvel_save(:,jdm_out-j+1,:,1)
    dens(:,j,:,1) = dens_save(:,jdm_out-j+1,:,1)
    temp(:,j,:,1) = temp_save(:,jdm_out-j+1,:,1)
    saln(:,j,:,1) = saln_save(:,jdm_out-j+1,:,1)
    ssh(:,j,1) = ssh_save(:,jdm_out-j+1,1)
    lat1d(j) = lat_save(jdm_out-j+1)
   ENDDO
   deallocate(uvel_save)
   deallocate(vvel_save)
   deallocate(wvel_save)
   deallocate(dens_save)
   deallocate(temp_save)
   deallocate(saln_save)
   deallocate(ssh_save)
   deallocate(lat_save)
  ENDIF

!define the units of the dimension variables
 IF (filename .ne. 'unknown') THEN
   write(year_start,*) jdate_yy
   year_start = adjustl(year_start)
   write(month_start,*) jdate_mm
   month_start = adjustl(month_start)
   write(day_start,*) jdate_dd
   day_start = adjustl(day_start)
   total_start=trim(time_units)//" since "//trim(year_start)//"-"//trim(month_start)//"-"//trim(day_start)
 ELSE
   total_start=""
 ENDIF
 units(1) = "degrees_east"
 units(2) = "degrees_north"
 units(3) = "meter"
 units(4) = total_start

!get size of x and y dimension
 IF (grid2d) THEN
   size_idm = idm_out_new
   size_jdm = jdm_out_new
 ELSE
   size_idm = idm_out
   size_jdm = jdm_out
 ENDIF

!create netcdf file
 IF (filename .ne. 'unknown') THEN
   CALL nc_create(nestname, size_idm, size_jdm, kdm_out, filename, &
   lon1d, lat1d, depth, units, ncId, time(num+1))
 ELSE
   CALL nc_create(nestname,size_idm,size_jdm,kdm_out,filename, &
   lon1d, lat1d, depth, units, ncId)
 ENDIF

!define extra global attribute in the netcdf file
 IF (grid2d) THEN
  CALL nc_att(nestname,ncId,"regridded","true")
 ENDIF

!write variables to netcdf file
 IF (grid2d) THEN
  CALL nc_write4d(nestname,ncId,"zu",uvel_new,size_idm, size_jdm, kdm_out,1,"eastward_sea_water_velocity","m/s" )
  CALL nc_write4d(nestname,ncId,"zv",vvel_new,size_idm, size_jdm, kdm_out,1, "northward_sea_water_velocity","m/s" )
  CALL nc_write4d(nestname,ncId,"zw",wvel_new,size_idm, size_jdm, kdm_out,1, "downward_sea_water_velocity","m/s" )
  IF (dens_name .ne. 'unknown' ) THEN
   CALL nc_write4d(nestname,ncId,"zd",dens_new,size_idm, size_jdm, kdm_out,1, "Density","kg/m3")
  ENDIF
  IF (temp_name .ne. 'unknown' ) THEN
   CALL nc_write4d(nestname,ncId,"zt",temp_new,size_idm, size_jdm, kdm_out,1, "Temperature","degC")
  ENDIF
  IF (saln_name .ne. 'unknown' ) THEN
   CALL nc_write4d(nestname,ncId,"zs",saln_new,size_idm, size_jdm, kdm_out,1, "Salinity","psu")
  ENDIF
  IF (ssh_name .ne. 'unknown' ) THEN
   CALL nc_write3d(nestname,ncId,"ssh",ssh_new,size_idm, size_jdm, 1,"Sea surface height","meter")
  ENDIF
 ELSE
  CALL nc_write4d(nestname,ncId,"zu",uvel,size_idm, size_jdm, kdm_out,1, "eastward_sea_water_velocity","m/s" )
  CALL nc_write4d(nestname,ncId,"zv",vvel,size_idm, size_jdm, kdm_out,1, "northward_sea_water_velocity","m/s" )
  CALL nc_write4d(nestname,ncId,"zw",wvel,size_idm, size_jdm, kdm_out, 1,"downward_sea_water_velocity","m/s" )
  IF (dens_name .ne. 'unknown' ) THEN
   CALL nc_write4d(nestname,ncId,"zd",dens,size_idm, size_jdm, kdm_out, 1,"Density","kg/m3")
  ENDIF
  IF (temp_name .ne. 'unknown' ) THEN
   CALL nc_write4d(nestname,ncId,"zt",temp,size_idm, size_jdm, kdm_out, 1,"Temperature","degC")
  ENDIF
  IF (saln_name .ne. 'unknown' ) THEN
   CALL nc_write4d(nestname,ncId,"zs",saln,size_idm, size_jdm, kdm_out, 1,"Salinity","psu")
  ENDIF
  IF (ssh_name .ne. 'unknown' ) THEN
    CALL nc_write3d(nestname,ncId,"ssh",ssh,size_idm, size_jdm, 1,"Sea surface height","meter")
  ENDIF
 ENDIF

!close netcdf file
 CALL nc_close(nestname, ncId) 

END SUBROUTINE create_nestfile

!**************************************************************
!read the oceonographic data with opendap
SUBROUTINE make_nestfiles_opendap

 integer (kind=int_kind) :: year, month, days, hours, minutes, seconds, &
                            mm, hh, dd, jj, i, lc, num, ncId
 logical (kind=log_kind) :: wvel_change, downl
 character (char_len)    :: nestname


 allocate(uvel(idm_out,jdm_out,kdm_out,1))
 allocate(vvel(idm_out,jdm_out,kdm_out,1))
 allocate(wvel(idm_out,jdm_out,kdm_out,1))
 allocate(saln(idm_out,jdm_out,kdm_out,1))
 allocate(temp(idm_out,jdm_out,kdm_out,1))
 allocate(dens(idm_out,jdm_out,kdm_out,1))
 allocate(ssh(idm_out,jdm_out,1))
 allocate(uvel_new(idm_out_new,jdm_out_new,kdm_out,1))
 allocate(vvel_new(idm_out_new,jdm_out_new,kdm_out,1))
 allocate(wvel_new(idm_out_new,jdm_out_new,kdm_out,1))
 allocate(saln_new(idm_out_new,jdm_out_new,kdm_out,1))
 allocate(temp_new(idm_out_new,jdm_out_new,kdm_out,1))
 allocate(dens_new(idm_out_new,jdm_out_new,kdm_out,1))
 allocate(ssh_new(idm_out_new,jdm_out_new,1))

!make a netcdf file for each timestep
 year = tstart_yy
 month = tstart_mm
 days = tstart_dd
 hours = 0
 minutes = 0
 seconds = 0
 DO lc=lstart,lend
  num = lc - lstart
  write(nestname,'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
    trim(filenest)//'nest_',nest_n,'_',year, month, days,hours,minutes,seconds,'.nc'

! check if file is already downloaded
  downl = .false.
  IF (numFiles .gt. 0) THEN
   DO i=1, numFiles
    IF (nestname .eq. downl_nest(i)) THEN
     downl = .true.
    ENDIF
   ENDDO
  ENDIF
! if not already downloaded then download
  IF (downl .eqv. .false.) THEN
   print *, 'Reading data file ',num+1,' of ',lend-lstart+1
!  get data from file
!  read u
   IF (uvel_name .ne. 'unknown' ) THEN
     CALL readdata(filename,ncId,uvel_name,uvel,lstart+num)
     print *, 'Succesfully read data: U-velocity'
   ELSE
     print *, 'U-velocity is not available in file'
   ENDIF
!  read v
   IF (vvel_name .ne. 'unknown' ) THEN
    CALL readdata(filename,ncId,vvel_name,vvel,lstart+num)
    print *, 'Succesfully read data: V-velocity'
   ELSE
     print *, 'V-velocity is not available in file'
   ENDIF
!  read w
   IF (wvel_name .ne. 'unknown' ) THEN
     CALL readdata(filename,ncId,wvel_name,wvel,lstart+num)
!    check if wvelocity is positive upward or positive downward
!    IF the wvelocity is positive upward then the sign of the w velocities have to changed
     IF (wvel_positive_direction .eq. 'upward') THEN
       wvel_change = .true.
     ELSE IF (wvel_positive_direction .eq. 'downward') THEN
       wvel_change = .false.
     ELSE
       print *, 'wvel_positive_direction in the nestfile has to have the value upward or downward'
       stop
     ENDIF
     print *, 'Succesfully read data: W-velocity'
   ELSE
     wvel = 0
   ENDIF
!  read density
   IF (dens_name .ne. 'unknown' ) THEN
     CALL readdata(filename,ncId,dens_name,dens,lstart+num)
     print *, 'Succesfully read data: Density'
   ENDIF
!  read temperature
   IF (temp_name .ne. 'unknown' ) THEN
     CALL readdata(filename,ncId,temp_name,temp,lstart+num)
     print *, 'Succesfully read data: Temperature'
   ENDIF
!  read salinity
   IF (saln_name .ne. 'unknown' ) THEN
     CALL readdata(filename,ncId,saln_name,saln,lstart+num)
     print *, 'Succesfully read data: Salinity'
   ENDIF
!  read ssh
   IF (ssh_name .ne. 'unknown' ) THEN
     CALL readdata(filename,ncId,ssh_name,ssh4d,lstart+num)
     print *, 'Succesfully read data: Sea surface height'
     ssh(:,:,:)=ssh4d(:,:,:,1)
   ENDIF
   CALL create_nestfile(nestname,wvel_change, num)    
   CALL create_filedownload(nestname)  
  ELSE
    print *, 'Data file ',num+1,' of ',lend-lstart+1,' already exists'
  ENDIF !end if: if (downl .eqv. .false.) THEN

! calculate next date of nestfile
  IF ((time_units .eq. "months") .or. (int(time_step/(30*secs_in_day)) .ge. 1)) THEN
    month = month + int(time_step/(30*secs_in_day))
    IF (month .eq. 13) THEN
      month = 1
      year = year + 1
    ENDIF
  ELSE  
    seconds = seconds + time_step
    mm = int(seconds/60)
    seconds = mod(seconds,60)
    minutes = minutes + mm
    hh = int(minutes/60)
    minutes = mod(minutes,60)

    hours = hours + hh
    dd = int(hours/24)
    hours = mod(hours,24)
    CALL jd(year, month, days,jj)
    jj = jj + dd
    CALL cdate(jj, year, month, days)
  ENDIF 
   
 ENDDO !lc=lstart,lend

END SUBROUTINE make_nestfiles_opendap

!**************************************************************
!read the oceonographic data from local files
SUBROUTINE make_nestfiles_local

 integer (kind=int_kind) :: year, month, days, hours, minutes, seconds, &
                            mm, hh, dd, jj,i, ncId    
 logical (kind=log_kind) :: wvel_change, downl
 character (char_len)    :: nestname, file_uvel, file_vvel, file_wvel, &
                            file_dens, file_temp, file_saln, file_ssh    

 allocate(uvel(idm_out,jdm_out,kdm_out,1))
 allocate(vvel(idm_out,jdm_out,kdm_out,1))
 allocate(wvel(idm_out,jdm_out,kdm_out,1))
 allocate(saln(idm_out,jdm_out,kdm_out,1))
 allocate(temp(idm_out,jdm_out,kdm_out,1))
 allocate(dens(idm_out,jdm_out,kdm_out,1))
 allocate(ssh(idm_out,jdm_out,1))
 allocate(uvel_new(idm_out_new,jdm_out_new,kdm_out,1))
 allocate(vvel_new(idm_out_new,jdm_out_new,kdm_out,1))
 allocate(wvel_new(idm_out_new,jdm_out_new,kdm_out,1))
 allocate(saln_new(idm_out_new,jdm_out_new,kdm_out,1))
 allocate(temp_new(idm_out_new,jdm_out_new,kdm_out,1))
 allocate(dens_new(idm_out_new,jdm_out_new,kdm_out,1))
 allocate(ssh_new(idm_out_new,jdm_out_new,1))

!make a netcdf file for each timestep
 year = tstart_yy
 month = tstart_mm
 days = tstart_dd
 hours = 0
 minutes = 0
 seconds = 0
 DO WHILE (year .ne. tend_yy  .or. month .ne. tend_mm .or. days .ne. tend_dd)

  write(nestname,'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
    trim(filenest)//'nest_',nest_n,'_',year, month, days,hours,minutes,seconds,'.nc'

! check if file is already downloaded
  downl = .false.
  IF (numFiles .gt. 0) THEN
   DO i=1, numFiles
    IF (nestname .eq. downl_nest(i)) THEN
     downl = .true.
    ENDIF
   ENDDO
  ENDIF

! IF not already downloaded then download
  IF (downl .eqv. .false.) THEN

   write(*,'(A,I0,A,I0,A,I0,A,I2.2,A,I2.2,A,I2.2)') & 
     'Reading data files: ',year,"-",month,"-",days," ",hours,":",minutes,":",seconds

   IF (uvel_name .ne. 'unknown' ) THEN
!   Open the file with uvel for reading 
    write(file_uvel,'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
          trim(filenest)//'raw/nest_',nest_n,'_',year, month, days, hours,minutes,seconds,'u.nc'
    IF (nc_file_exists(file_uvel) .eqv. .false.) THEN
     write(file_uvel,'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') & 
           trim(filenest)//'raw/nest_',nest_n,'_',year, month, days,hours,minutes,seconds,'.nc'
    ENDIF 

    CALL readdata(file_uvel,ncId,uvel_name,uvel,1)
    print *, 'Succesfully read file with: U-velocity'
   ELSE
    print *, 'U-velocity is not available in file'
   ENDIF

   IF (vvel_name .ne. 'unknown' ) THEN
!   Open the file with vvel for reading 
    write(file_vvel,'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
          trim(filenest)//'raw/nest_',nest_n,'_',year, month, days, hours,minutes,seconds,'v.nc'
    IF (nc_file_exists(file_vvel) .eqv. .false.) THEN
     write(file_vvel,'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
           trim(filenest)//'raw/nest_',nest_n,'_',year, month, days, hours,minutes,seconds,'.nc'
    ENDIF 
    CALL readdata(file_vvel,ncId,vvel_name,vvel,1)
    print *, 'Succesfully read file with: V-velocity'
   ELSE
     print *, 'V-velocity is not available in file'
   ENDIF

   IF (wvel_name .ne. 'unknown' ) THEN
!   Open the file with wvel for reading 
    write(file_wvel,'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') & 
          trim(filenest)//'raw/nest_',nest_n,'_',year, month, days, hours,minutes,seconds,'w.nc'
    IF (nc_file_exists(file_wvel) .eqv. .false.) THEN
     write(file_wvel,'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
           trim(filenest)//'raw/nest_',nest_n,'_',year, month, days,hours,minutes,seconds,'.nc'
    ENDIF 
    CALL readdata(file_wvel,ncId,wvel_name,wvel,1)
!   check if wvelocity is positive upward or positive downward
!   if the wvelocity is positive upward then the sign of the w velocities have to changed
    IF (wvel_positive_direction .eq. 'upward') THEN
     wvel_change = .true.
    ELSE IF (wvel_positive_direction .eq. 'downward') THEN
     wvel_change = .false.
    ELSE
     print *, 'wvel_positive_direction in the nestfile has to have the value upward or downward'
     stop
    ENDIF
    print *, 'Succesfully read file with: W-velocity'
!   Regrid wvel if on B-grid
    IF (bgrid) THEN
     CALL regridW
    ENDIF
   ELSE
    wvel = 0
   ENDIF

   IF (dens_name .ne. 'unknown' ) THEN
!   Open the file with dens for reading 
    write(file_dens,'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
          trim(filenest)//'raw/nest_',nest_n,'_',year, month, days,hours,minutes,seconds,'d.nc'
    IF (nc_file_exists(file_dens) .eqv. .false.) THEN
      write(file_dens,'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') & 
            trim(filenest)//'raw/nest_',nest_n,'_',year, month, days, hours,minutes,seconds,'.nc'
    ENDIF 
    CALL readdata(file_dens,ncId,dens_name,dens,1)
    print *, 'Succesfully read file with: Density'
   ENDIF

   IF (temp_name .ne. 'unknown' ) THEN
!   Open the file with temp for reading 
    write(file_temp,'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
          trim(filenest)//'raw/nest_',nest_n,'_',year, month, days,hours,minutes,seconds,'t.nc'
    IF (nc_file_exists(file_temp) .eqv. .false.) THEN
     write(file_temp,'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
           trim(filenest)//'raw/nest_',nest_n,'_',year, month, days,hours,minutes,seconds,'.nc'
    ENDIF 
    CALL readdata(file_temp,ncId,temp_name,temp,1)
    print *, 'Succesfully read file with: Temperature'
   ENDIF

   IF (saln_name .ne. 'unknown' ) THEN
!   Open the file with saln for reading 
    write(file_saln,'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
          trim(filenest)//'raw/nest_',nest_n,'_',year, month, days,hours,minutes,seconds,'s.nc'
    IF (nc_file_exists(file_saln) .eqv. .false.) THEN
     write(file_saln,'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
           trim(filenest)//'raw/nest_',nest_n,'_',year, month, days,hours,minutes,seconds,'.nc'
    ENDIF 
    CALL readdata(file_saln,ncId,saln_name,saln,1)
    print *, 'Succesfully read file with: Salinity'
   ENDIF

!  read ssh
   IF (ssh_name .ne. 'unknown' ) THEN
!   Open the file with ssh for reading 
    write(file_ssh,'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
          trim(filenest)//'raw/nest_',nest_n,'_',year, month, days,hours,minutes,seconds,'ssh.nc'

    IF (nc_file_exists(file_ssh) .eqv. .false.) THEN
      write(file_ssh,'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
            trim(filenest)//'raw/nest_',nest_n,'_',year, month, days,hours,minutes,seconds,'.nc'
    ENDIF 
    CALL readdata(file_ssh,ncId,ssh_name,ssh4d,1)
    ssh(:,:,:)=ssh4d(:,:,:,1)
    print *, 'Succesfully read file with: Sea surface height'
   ENDIF

   CALL create_nestfile (nestname, wvel_change, 0)
   CALL create_filedownload(nestname)
  ELSE
    write(*,'(A,I0,A,I0,A,I0,A,I2.2,A,I2.2,A,I2.2,A)')  &
          'Data file: ',year,"-",month,"-",days," ",hours,":",minutes,":",seconds," already exists"
  ENDIF

! calculate next date of nestfile
  IF ((time_units .eq. "months") .or. (int(time_step/(30*secs_in_day)) .ge. 1)) THEN
    month = month + int(time_step/(30*secs_in_day))
    IF (month .eq. 13) THEN
      month = 1
      year = year + 1
    ENDIF
  ELSE  
    seconds = seconds + time_step
    mm = int(seconds/60)
    seconds = mod(seconds,60)
    minutes = minutes + mm
    hh = int(minutes/60)
    minutes = mod(minutes,60)
    hours = hours + hh
    dd = int(hours/24)
    hours = mod(hours,24)
    CALL jd(year, month, days, jj)
    jj = jj + dd
    CALL cdate(jj, year, month, days)
  ENDIF
 
 ENDDO !end loop: do WHILE (year .ne. tend_yy  .or. month .ne. tend_mm .or. days .ne. tend_dd)

END SUBROUTINE make_nestfiles_local

!************************************************************
SUBROUTINE readdata(flename,ncId,fieldname,field,tstart)
 integer (kind=int_kind) :: ncId, tstart
 character (char_len)    :: flename, fieldname
 real (kind = real_kind) :: field(idm_out,jdm_out,kdm_out,1),field3d(idm_out,jdm_out,1), field2d(idm_out,jdm_out)
 CALL nc_open(flename,ncId)
 IF (hasDepth .and. fieldname.ne.ssh_name) THEN
  IF (hasTime) THEN
   IF (istart > iend) THEN
    CALL nc_read4d(flename, ncId, fieldname,field(1:ipart1,:,:,1),ipart1,jdm_out,kdm_out,1,(/ istart,jstart,kstart,tstart /))
    CALL nc_read4d(flename, ncId, fieldname,field(ipart1+1:idm_out,:,:,1),iend,jdm_out,kdm_out,1,(/ 1,jstart,kstart,tstart /))
   ELSE
    CALL nc_read4d(flename, ncId, fieldname,field(:,:,:,1),idm_out,jdm_out,kdm_out,1,(/ istart,jstart,kstart,tstart /))
   ENDIF 
  ELSE ! not hasTime
   IF (istart > iend) THEN
    CALL nc_read3d(flename, ncId, fieldname,field3d(1:ipart1,:,:),ipart1,jdm_out,kdm_out,(/ istart,jstart,kstart /))
    CALL nc_read3d(flename, ncId, fieldname,field3d(ipart1+1:idm_out,:,:),iend,jdm_out,kdm_out,(/ 1,jstart,kstart /))
   ELSE
    CALL nc_read3d(flename, ncId, fieldname,field3d(:,:,:),idm_out,jdm_out,kdm_out,(/ istart,jstart,kstart /))
   ENDIF 
   field(:,:,:,1) = field3d (:,:,:)
  ENDIF
 ELSE ! not hasDepth
  IF (hasTime) THEN
   IF (istart > iend) THEN
    CALL nc_read3d(flename, ncId, fieldname,field3d(1:ipart1,:,1),ipart1,jdm_out,1,(/ istart,jstart,tstart /))
    CALL nc_read3d(flename, ncId, fieldname,field3d(ipart1+1:idm_out,:,1),iend,jdm_out,1,(/ 1,jstart,tstart /))
   ELSE
    CALL nc_read3d(flename, ncId, fieldname,field3d(:,:,1),idm_out,jdm_out,1,(/ istart,jstart,tstart /))
   ENDIF 
   field(:,:,1,1) = field3d (:,:,1)
  ELSE ! not hasTime
   IF (istart > iend) THEN
    CALL nc_read2d(flename, ncId, fieldname,field2d(1:ipart1,:),ipart1,jdm_out,(/ istart,jstart /))
    CALL nc_read2d(flename, ncId, fieldname,field2d(ipart1+1:idm_out,:),iend,jdm_out,(/ 1,jstart /))
   ELSE
    CALL nc_read2d(flename, ncId, fieldname,field2d(:,:),idm_out,jdm_out,(/ istart,jstart /))
   ENDIF 
   field(:,:,1,1) = field2d (:,:)
  ENDIF
 ENDIF
 CALL nc_close(flename, ncId) 

END SUBROUTINE readdata

!**************************************************************
!deallocate memory
SUBROUTINE deallocate_memory
 deallocate(uvel)
 deallocate(vvel)
 deallocate(wvel)
 deallocate(saln)
 deallocate(ssh)
 deallocate(temp)
 deallocate(dens)
 deallocate(uvel_new)
 deallocate(vvel_new) 
 deallocate(wvel_new)
 deallocate(saln_new)
 deallocate(temp_new)
 deallocate(dens_new)
 deallocate(ssh_new)
 deallocate(tmpDepth)
 deallocate(tmpTime)
 deallocate(tmpDoubleTime)
 deallocate(lon1d)
 deallocate(lat1d)
 deallocate(depth)
 deallocate(time)
 deallocate(downl_nest)
 IF (was2d) THEN
  deallocate(tmpLon2d)
  deallocate(tmpLat2d)
 ENDIF
 IF (grid2d) THEN
  deallocate(tmpLon2d)
  deallocate(tmpLat2d)
  deallocate(src_bbox_lon)
  deallocate(src_bbox_lat)
  deallocate(angle)
  deallocate(tmpLon_new)
  deallocate(tmpLat_new)
  deallocate(w)
  deallocate(x)
  deallocate(y)
 ELSE
  deallocate(tmpLon1d) 
  deallocate(tmpLat1d)
 ENDIF
 
END SUBROUTINE  

!**************************************************************
!main SUBROUTINE. Calls all the other subroutines
SUBROUTINE get_data(pnest_n, filenumber )

 integer (kind=int_kind), intent(in) :: pnest_n
 character(char_len), intent(in)     :: filenumber    
     
 nest_n = pnest_n

 CALL create_directories(filenumber)
 CALL read_nest_file
 CALL read_data
 CALL change_longitudes
 CALL read_filedownload  
 IF (grid2d) THEN
  CALL define_new_grid
 ENDIF
 CALL subset_domain
 IF (grid2d) THEN
  CALL calculate_weights 
 ENDIF
 IF (filename .eq. 'unknown') THEN
  CALL make_nestfiles_local
 ELSE
  CALL make_nestfiles_opendap
 ENDIF
 CALL deallocate_memory


END SUBROUTINE get_data

!**************************************************************

END MODULE mod_getdata
