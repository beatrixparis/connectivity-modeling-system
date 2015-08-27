!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : mod_nciorw.f90                                                    *
!* Last Modified: 2011-07-22                                                *
!* Code contributors: Judith Helgers, Ashwanth Srinivasan, Claire B. Paris  * 
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

MODULE mod_nciorw

 USE mod_kinds
 USE mod_netcdf
 USE constants

 IMPLICIT NONE

 CONTAINS

!**************************************************************
!open netcdf file for reading
SUBROUTINE nc_open(ncfname,ncId)
 character (len=*), intent(in)   :: ncfname
 integer  (kind=int_kind), intent(out) :: ncId

 CALL ncheck(nf90_open(trim(ncfname),nf90_NoWrite,ncId),ncfname)
END SUBROUTINE nc_open


!**************************************************************
!create netcdf file
!- with four dimensions: Longitude, Latitude, Depth and Time
!- size of the four dimensions is: xmax, ymax, zmax and 1
!- creates also four dimension variables: Longitude, Latitude, Depth and Time(if given)
!- fills the variables with the values: lon, lat, depth and time
SUBROUTINE nc_create(ncfname,xmax,ymax,zmax,source,lon,lat,depth, units,ncId,time)   
 character (len=*), intent(in)   :: ncfname,source, units(4)
 integer  (kind=int_kind), intent(in)  :: xmax, ymax, zmax
 real (kind=real_kind), intent(in)     :: lon(xmax), lat(ymax),depth(zmax)
 real (kind=real_kind), intent(in),optional  :: time
 integer  (kind=int_kind), intent(out) :: ncId

 integer  (kind=int_kind)  ::  xDim, yDim, zDim, tDim,xVar, yvar, zVar, tVar
 real (kind=real_kind) :: fill_value

!create the file
 CALL ncheck(nf90_create(trim(ncfname),or(nf90_clobber,nf90_64bit_offset),ncId),ncfname)


!create dimensions
 CALL ncheck(nf90_def_dim(ncId, 'Longitude',xmax,xDim),ncfname)
 CALL ncheck(nf90_def_dim(ncId, 'Latitude',ymax,yDim),ncfname)
 CALL ncheck(nf90_def_dim(ncId, 'Depth',zmax,zDim),ncfname)
 CALL ncheck(nf90_def_dim(ncId, 'Time',nf90_unlimited,tDim),ncfname)
    
!create global attribute
 if (present(time)) then
  CALL ncheck(nf90_put_att(ncId,nf90_global,'source',source),ncfname)
 ENDIF

 
!define variables
 CALL ncheck(nf90_def_var(ncId,"Longitude",nf90_float,(/ xDim /),xVar),ncfname)
 CALL ncheck(nf90_def_var(ncId,"Latitude",nf90_float,(/ yDim /),yVar),ncfname)
 CALL ncheck(nf90_def_var(ncId,"Depth",nf90_float,(/ zDim /),zVar),ncfname)
 IF (present(time)) then
   CALL ncheck(nf90_def_var(ncId,"Time",nf90_float,(/ tDim /),tVar),ncfname)
 ENDIF

!add attributes
 fill_value = 2.**100
 CALL ncheck(nf90_put_att(ncId,xVar,'units',units(1)),ncfname)
 CALL nc_range1d(ncfname,ncId,xVar,lon,xmax,fill_value)
 CALL ncheck(nf90_put_att(ncId,yVar,'units',units(2)),ncfname)
 CALL nc_range1d(ncfname,ncId,yVar,lat,ymax,fill_value)
 CALL ncheck(nf90_put_att(ncId,zVar,'units',units(3)),ncfname)
 CALL nc_range1d(ncfname,ncId,zVar,depth,zmax,fill_value)
 CALL ncheck(nf90_put_att(ncId,zVar,'positive','down'),ncfname)
 IF (present(time)) then
   CALL ncheck(nf90_put_att(ncId,tVar,'units',units(4)),ncfname)
   CALL ncheck(nf90_put_att(ncId,tVar,'calender',"standard"),ncfname)
 ENDIF

!end define mode
 CALL ncheck(nf90_enddef(ncid),ncfname)

!put variables
 CALL ncheck(nf90_put_var(ncId, xVar, lon),ncfname) 
 CALL ncheck(nf90_put_var(ncId, yVar, lat),ncfname) 
 CALL ncheck(nf90_put_var(ncId, zVar, depth),ncfname)
 IF (present(time)) then 
   CALL ncheck(nf90_put_var(ncId, tVar, time),ncfname) 
 ENDIF

END SUBROUTINE nc_create

!**************************************************************

!close netcdf file
SUBROUTINE nc_close(ncfname, ncId) 
 character (len=*), intent(in)        :: ncfname
 integer  (kind=int_kind), intent(in) :: ncid

 CALL ncheck(nf90_close(ncId),ncfname)

END SUBROUTINE nc_close


!**************************************************************
!Write a variable 4d to netcdf file:
!with the name: varname
!with the values: values
!with the size: nx, ny, nz, nt
!with the attributes: lname and units
SUBROUTINE nc_write4d(ncfname,ncId,varname,values,nx,ny,nz,nt,lname,units, start)   

 integer  (kind=int_kind), intent(in) :: ncId,nx,ny,nz,nt
 character (len=*), intent(in)        :: ncfname, varname, lname, units   
 real  (kind=real_kind),dimension(nx,ny,nz,nt),intent(in) :: values
 integer  (kind=int_kind), intent(in),optional            :: start(4)

 real (kind=real_kind)     :: fill_value
 integer  (kind=int_kind)  :: varId,xDim, yDim, zDim, tDim


!get dimensions
 CALL ncheck(nf90_inq_dimid(ncId, "Longitude", xDim),ncfname)
 CALL ncheck(nf90_inq_dimid(ncId, "Latitude", yDim),ncfname)
 CALL ncheck(nf90_inq_dimid(ncId, "Depth", zDim),ncfname)
 CALL ncheck(nf90_inq_dimid(ncId, "Time", tDim),ncfname)

!start define mode
 CALL ncheck(nf90_redef(ncid), ncfname)

!define variable
 CALL ncheck(nf90_def_var(ncId,varName,nf90_float, &
   (/xDim, yDim, zDim, tDim /),varId),ncfname)

!add attributes
 fill_value = 2.**100
 CALL ncheck(nf90_put_att(ncId,varId,"long_name",lname),ncfname)
 CALL ncheck(nf90_put_att(ncId,varId,"units",units),ncfname)
 CALL ncheck(nf90_put_att(ncId,varId,"_FillValue",fill_value),ncfname)
 CALL nc_range4d(ncfname,ncId,varId,values,nx,ny,nz,nt,fill_value)
!end define mode
 CALL ncheck(nf90_enddef(ncid),ncfname)

!put variable
 IF (present(start)) then
  CALL ncheck(nf90_put_var(ncId, varId, values,start,(/ nx,ny,nz,nt /)),ncfname) 
 ELSE
  CALL ncheck(nf90_put_var(ncId, varId, values),ncfname)     
 ENDIF

END SUBROUTINE nc_write4d

!**************************************************************
!Write variable 3d to netcdf file:
!with the name: varname
!with the values: values
!with the size: nx, ny, nt
!with the attributes: lname and units
SUBROUTINE nc_write3d(ncfname,ncId,varname,values,nx,ny,nt,lname,units, start)   

 integer (kind=int_kind), intent(in) :: ncId,nx,ny,nt
 character (len=*), intent(in)       :: ncfname, varname, lname, units   
 real (kind=real_kind),dimension(nx,ny,nt),intent(in) :: values
 integer  (kind=int_kind), intent(in),optional        :: start(3)

 real (kind=real_kind)     :: fill_value
 integer  (kind=int_kind)  :: varId,xDim, yDim, zDim, tDim

!get dimensions
 CALL ncheck(nf90_inq_dimid(ncId, "Longitude", xDim),ncfname)
 CALL ncheck(nf90_inq_dimid(ncId, "Latitude", yDim),ncfname)
 CALL ncheck(nf90_inq_dimid(ncId, "Depth", zDim),ncfname)
 CALL ncheck(nf90_inq_dimid(ncId, "Time", tDim),ncfname)

!start define mode
 CALL ncheck(nf90_redef(ncid), ncfname)

!define variable
 CALL ncheck(nf90_def_var(ncId,varName,nf90_float,(/xDim, yDim, tDim /),varId),ncfname)

!add attributes
 fill_value = 2.**100
 CALL ncheck(nf90_put_att(ncId,varId,"long_name",lname),ncfname)
 CALL ncheck(nf90_put_att(ncId,varId,"units",units),ncfname)
 CALL ncheck(nf90_put_att(ncId,varId,"_FillValue",fill_value),ncfname)
 CALL nc_range3d(ncfname,ncId,varId,values,nx,ny,nt,fill_value)

!end define mode
 CALL ncheck(nf90_enddef(ncid),ncfname)

!put variable
 IF (present(start)) then
  CALL ncheck(nf90_put_var(ncId, varId, values,start,(/ nx,ny,nt /)),ncfname)     
 ELSE
  CALL ncheck(nf90_put_var(ncId, varId, values),ncfname)     
 ENDIF

END SUBROUTINE nc_write3d
!**************************************************************

!read variable 4d from netcdf file (real)
SUBROUTINE nc_read4d(ncfname, ncId, varname, values,nx,ny,nz,nt,start)

 integer  (kind=int_kind), intent(in) :: ncId,nx,ny,nz,nt
 character (len=*), intent(in)        :: ncfname, varname
 integer  (kind=int_kind), intent(in),optional :: start(4)
 real  (kind=real_kind),dimension(nx,ny,nz,nt),intent(out) :: values
 real (kind=real_kind)     :: scalefactor
 integer  (kind=int_kind)  :: varId, sfpresent

 CALL ncheck(nf90_inq_varid(ncId, varName, varId),ncfname)

 IF (present(start)) then
  CALL ncheck(nf90_get_var(ncId, varId, values,start,(/ nx,ny,nz,nt /)),ncfname) 
 ELSE
  CALL ncheck(nf90_get_var(ncId, varId, values),ncfname)
 ENDIF    

 CALL nc_scalefactor(ncfname, ncId, varId, sfpresent, scalefactor)
 IF (sfpresent.eq.1) THEN
  values = values * scalefactor
 ENDIF
END SUBROUTINE nc_read4d

!**************************************************************

!read variable 3d from netcdf file (real)
SUBROUTINE nc_read3d(ncfname, ncId, varname, values,nx,ny,nt,start)
 integer  (kind=int_kind), intent(in) :: ncId,nx,ny,nt
 character (len=*), intent(in) :: ncfname, varname
 integer  (kind=int_kind), intent(in),optional :: start(3)
 real (kind=real_kind),dimension(nx,ny,nt),intent(out) :: values
 real (kind=real_kind)     :: scalefactor
 integer  (kind=int_kind)  :: varId, sfpresent

 CALL ncheck(nf90_inq_varid(ncId, varName, varId),ncfname)

 IF (present(start)) then
  CALL ncheck(nf90_get_var(ncId, varId, values,start,(/ nx,ny,nt /)),ncfname) 
 ELSE
  CALL ncheck(nf90_get_var(ncId, varId, values),ncfname) 
 ENDIF    

 CALL nc_scalefactor(ncfname, ncId, varId, sfpresent, scalefactor)
 IF (sfpresent.eq.1) THEN
  values = values * scalefactor
 ENDIF
END SUBROUTINE nc_read3d

!**************************************************************

!read variable 2d from netcdf file (real)
SUBROUTINE nc_read2d(ncfname, ncId, varname, values,nx,ny,start)
 integer  (kind=int_kind), intent(in) :: ncId,nx,ny
 character (len=*), intent(in)        :: ncfname, varname
 integer  (kind=int_kind), intent(in),optional :: start(2)
 real  (kind=real_kind),dimension(nx,ny),intent(out) :: values

 real (kind=real_kind)     :: scalefactor
 integer  (kind=int_kind)  :: varId, sfpresent

 CALL ncheck(nf90_inq_varid(ncId, varName, varId),ncfname)

 IF (present(start)) then
  CALL ncheck(nf90_get_var(ncId, varId, values,start,(/ nx,ny /)),ncfname) 
 ELSE
  CALL ncheck(nf90_get_var(ncId, varId, values),ncfname) 
 ENDIF  

 CALL nc_scalefactor(ncfname, ncId, varId, sfpresent, scalefactor)
 IF (sfpresent.eq.1) THEN
  values = values * scalefactor
 ENDIF
END SUBROUTINE nc_read2d


!**************************************************************

!read variable 1d from netcdf file (real)
SUBROUTINE nc_read1d(ncfname, ncId, varname, values,nx,start)
 integer  (kind=int_kind), intent(in) :: ncId,nx
 character (len=*), intent(in)        :: ncfname, varname
 integer  (kind=int_kind), intent(in),optional   :: start(1)
 real  (kind=real_kind),dimension(nx),intent(out):: values

 real (kind=real_kind)     :: scalefactor
 integer  (kind=int_kind)  :: varId, sfpresent

 CALL ncheck(nf90_inq_varid(ncId, varName, varId),ncfname)

 IF (present(start)) then
  CALL ncheck(nf90_get_var(ncId, varId, values,start,(/ nx /)),ncfname) 
 ELSE
  CALL ncheck(nf90_get_var(ncId, varId, values),ncfname) 
 ENDIF  

 CALL nc_scalefactor(ncfname, ncId, varId, sfpresent, scalefactor)
 IF (sfpresent.eq.1) THEN
  values = values * scalefactor
 ENDIF
END SUBROUTINE nc_read1d

!**************************************************************

!read variable 1d from netcdf file (double)
SUBROUTINE nc_read1d_dbl(ncfname, ncId, varname, values,nx,start)

 integer  (kind=int_kind), intent(in) :: ncId,nx
 character (len=*), intent(in)        :: ncfname, varname
 integer  (kind=int_kind), intent(in),optional :: start(1)
 real  (kind=dbl_kind),dimension(nx),intent(out) :: values
 integer  (kind=int_kind) :: varId

 CALL ncheck(nf90_inq_varid(ncId, varName, varId),ncfname)

 IF (present(start)) then
  CALL ncheck(nf90_get_var(ncId, varId, values,start,(/ nx /)),ncfname) 
 ELSE
  CALL ncheck(nf90_get_var(ncId, varId, values),ncfname) 
 ENDIF  

END SUBROUTINE nc_read1d_dbl

!**************************************************************

!read variable 1d from netcdf file (integer)
SUBROUTINE nc_read1d_int(ncfname, ncId, varname, values,nx,start)
 integer  (kind=int_kind), intent(in) :: ncId,nx
 character (len=*), intent(in) :: ncfname, varname
 integer  (kind=int_kind), intent(in),optional :: start(1)
 integer  (kind=int_kind),dimension(nx),intent(out) :: values

 integer  (kind=int_kind)  :: varId

 CALL ncheck(nf90_inq_varid(ncId, varName, varId),ncfname)

 IF (present(start)) then
  CALL ncheck(nf90_get_var(ncId, varId, values,start,(/ nx /)),ncfname) 
 ELSE
  CALL ncheck(nf90_get_var(ncId, varId, values),ncfname) 
 ENDIF  

END SUBROUTINE nc_read1d_int

!**************************************************************

!fix a possible scale factor as an attribute
SUBROUTINE nc_scalefactor(ncfname, ncId, varId, sfpresent, scalefactor)
 character (len=*), intent(in)         :: ncfname
 integer  (kind=int_kind), intent(in)  :: ncId, varId
 integer  (kind=int_kind), intent(out) :: sfpresent
 real  (kind=real_kind),   intent(out) :: scalefactor
 integer  (kind=int_kind)              :: ScaleLength, status

 status = nf90_inquire_attribute(ncid, varId, 'scale_factor', len = ScaleLength)
 IF (status .eq. nf90_noerr) THEN 
  sfpresent = 1
  CALL ncheck(nf90_get_att(ncId, varId, 'scale_factor',scalefactor), ncfname)
 ELSE
  sfpresent = 0
 ENDIF

END SUBROUTINE nc_scalefactor

!**************************************************************

!get length of dimension with axis name
SUBROUTINE nc_info_dim(ncfname, ncId, axis, length)
 integer  (kind=int_kind), intent(in)  :: ncId
 character (len=*), intent(in)         :: ncfname, axis
 integer  (kind=int_kind), intent(out) :: length

 integer  (kind=int_kind) :: axisId 

 CALL ncheck(nf90_inq_dimid(ncId, axis, axisId),ncfname)
 CALL ncheck(nf90_inquire_dimension(ncId,axisId, len=length),ncfname)

END SUBROUTINE nc_info_dim

!**************************************************************
!get length of dimension with axis number
SUBROUTINE nc_info_dim2(ncfname, ncId, axisNum, length)
 integer  (kind=int_kind), intent(in)  :: ncId, axisNum 
 character (len=*), intent(in)         :: ncfname     
 integer  (kind=int_kind), intent(out) :: length
     
 CALL ncheck(nf90_inquire_dimension(ncId,axisNum, len=length),ncfname)

END SUBROUTINE nc_info_dim2


!**************************************************************
!get length of variable
SUBROUTINE nc_info_var(ncfname, ncId, varName, length)
 integer  (kind=int_kind), intent(in)  :: ncId
 character (len=*), intent(in)         :: ncfname, varName
 integer  (kind=int_kind), intent(out) :: length

 integer  (kind=int_kind) :: varId 

 CALL ncheck(nf90_inq_varid(ncId, varName, varId),ncfname)
 CALL ncheck(nf90_inquire_variable(ncId,varId, ndims=length),ncfname)

END SUBROUTINE nc_info_var

!**************************************************************
!check IF name exists as variable or attribute
logical FUNCTION nc_exists(ncId, ncName)
 integer  (kind=int_kind), intent(in) :: ncId
 character (len=*), intent(in)        :: ncName   
 integer  (kind=int_kind) :: varID,status1, status2
 character (char_len)     :: values

 status1 = nf90_get_att(ncId,nf90_global,ncName,values)
 status2 =nf90_inq_varId(ncID,ncName,varID) 

 IF (status1 == nf90_noerr .or. status2 == nf90_noerr) then   
   nc_exists = .true.
 ELSE
   nc_exists = .false.
 ENDIF

END FUNCTION nc_exists

!**************************************************************
!check if a file exists
 logical FUNCTION nc_file_exists(ncfname)
 character (len=*), intent(in):: ncfname  

 integer  (kind=int_kind) :: ncId,status 
     
 status = nf90_open(trim(ncfname),nf90_NoWrite,ncId)

 IF (status == nf90_noerr) then   
   CALL ncheck(nf90_close(ncId),trim(ncfname))
   nc_file_exists = .true.
 ELSE
   nc_file_exists = .false.
 ENDIF

END FUNCTION nc_file_exists

!**************************************************************
!add range of 1d array
SUBROUTINE nc_range1d(ncfname,ncId,varId,h,ii,fill_value)

 integer  (kind=int_kind), intent(in) :: ii, ncId, varId
 real (kind = real_kind), intent(in)  :: h(ii),fill_value
 character (len=*), intent(in) :: ncfname

 real (kind = real_kind)  :: hmin,hmax
 integer (kind=int_kind)  :: i
 real (kind = real_kind)  :: hhmin,hhmax
 
 hhmin =  abs(fill_value)
 hhmax = -abs(fill_value)
 DO i= 1,ii
  IF (h(i).ne.fill_value) then
   hhmin = min(hhmin,h(i))
   hhmax = max(hhmax,h(i))
  ENDIF
 ENDDO
 hmin = hhmin
 hmax = hhmax

 CALL ncheck(nf90_put_att(ncId,varId, "valid_range",(/ hmin, hmax /))  ,ncfname)

END SUBROUTINE nc_range1d

!**************************************************************
!add range of 3d array
SUBROUTINE nc_range3d(ncfname,ncId,varId,h,ii,jj,kk,fill_value)

 integer  (kind=int_kind), intent(in) :: ii,jj,kk, ncId, varId
 real (kind = real_kind), intent(in)  :: h(ii,jj,kk),fill_value
 character (len=*), intent(in) :: ncfname

 real (kind = real_kind)  :: hmin,hmax
 integer  (kind=int_kind) :: i,j,k
 real (kind = real_kind)  :: hhmin,hhmax
 
 hhmin =  abs(fill_value)
 hhmax = -abs(fill_value)
 DO k= 1,kk
  DO j= 1,jj
   DO i= 1,ii
     IF (h(i,j,k).ne.fill_value) then
       hhmin = min(hhmin,h(i,j,k))
       hhmax = max(hhmax,h(i,j,k))
     ENDIF
   ENDDO
  ENDDO
 ENDDO
 hmin = hhmin
 hmax = hhmax

 CALL ncheck(nf90_put_att(ncId,varId, "valid_range",(/ hmin, hmax /))  ,ncfname) 
END SUBROUTINE nc_range3d

!**************************************************************

!add range of 4d array
SUBROUTINE nc_range4d(ncfname,ncId,varId,h,ii,jj,kk,tt,fill_value)

 integer  (kind=int_kind), intent(in) :: ii,jj,kk,tt, ncId, varId
 real (kind = real_kind), intent(in)  :: h(ii,jj,kk,tt),fill_value
 character (len=*), intent(in)        :: ncfname
 real (kind = real_kind) :: hmin,hmax
 
 integer  (kind=int_kind) :: i,j,k,t
 real (kind = real_kind)  :: hhmin,hhmax

 hhmin =  abs(fill_value)
 hhmax = -abs(fill_value)
 DO t= 1,tt
  DO k= 1,kk
   DO j= 1,jj
    DO i= 1,ii
     IF (h(i,j,k,t).ne.fill_value) then
      hhmin = min(hhmin,h(i,j,k,t))
      hhmax = max(hhmax,h(i,j,k,t))
     ENDIF
    ENDDO
   ENDDO
  ENDDO
 ENDDO
 hmin = hhmin
 hmax = hhmax

 CALL ncheck(nf90_put_att(ncId,varId, "valid_range", (/ hmin, hmax /))  ,ncfname) 
END SUBROUTINE nc_range4d

!**************************************************************

!add an attribute (global)
SUBROUTINE nc_att(ncfname, ncId,attName, attValue )
 integer  (kind=int_kind), intent(in)  :: ncId
 character (len=*), intent(in)         :: ncfname, attName, attValue  

!start define mode
 CALL ncheck(nf90_redef(ncid), ncfname)

 CALL ncheck(nf90_put_att(ncId,nf90_global,attName, attValue),ncfname)

!end define mode
 CALL ncheck(nf90_enddef(ncid),ncfname)

END SUBROUTINE nc_att

!**************************************************************

END MODULE mod_nciorw


