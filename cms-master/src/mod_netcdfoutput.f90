!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : mod_netcdfoutput.f90                                              *
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

MODULE mod_netcdfoutput

 USE mod_kinds
 USE mod_nciorw
 USE constants


 IMPLICIT NONE

 integer (kind=int_kind), save :: &
     ncId, locVar,timeVar,lonVar, latVar, depthVar, tempVar, salnVar,  mldVar, &
     distanceVar, statusVar, releaseVar, polyVar, densVar, sizeVar



 CONTAINS

!**************************************************************

!create netcdf file for outputfile

SUBROUTINE output_create(ncfname, numTime)   

 USE globalvariables

 character (len=*), intent(in)         :: ncfname
 integer  (kind=int_kind), intent(in)  :: numTime

 integer  (kind=int_kind)              :: partDim, timeDim


!create the file
 CALL ncheck(nf90_create(trim(ncfname),nf90_clobber,ncId),ncfname)

!create dimensions
 CALL ncheck(nf90_def_dim(ncId, 'particle',nf90_unlimited,partDim),ncfname)
 CALL ncheck(nf90_def_dim(ncId, 'time',numTime,timeDim),ncfname) 

!add variables to netcdf file

!add time
 CALL ncheck(nf90_def_var(ncId,"time",nf90_int,(/ timeDim /),timeVar),trim(ncfname))
 CALL ncheck(nf90_put_att(ncId,timeVar,"units",'seconds'),ncfname)
 CALL ncheck(nf90_put_att(ncId,timeVar,"long_name",'Time after release'),ncfname)
 CALL ncheck(nf90_put_att(ncId,timeVar,"_FillValue",-1),ncfname)

!add release location
 CALL ncheck(nf90_def_var(ncId,"location",nf90_int,(/ partDim /),locVar),trim(ncfname))
 CALL ncheck(nf90_put_att(ncId,locVar,"long_name",'Line number in the release file') ,ncfname)
 CALL ncheck(nf90_put_att(ncId,locVar,"_FillValue",-1),ncfname)

!add longitude
 CALL ncheck(nf90_def_var(ncId,"lon",nf90_float,(/ timeDim,partDim /),lonVar),trim(ncfname))
 CALL ncheck(nf90_put_att(ncId,lonVar,"units",'degrees_east'),ncfname)
 CALL ncheck(nf90_put_att(ncId,lonVar,"long_name",'Longitude'),ncfname)
 CALL ncheck(nf90_put_att(ncId,lonVar,"_FillValue",2.**100),ncfname)

!add latitude
 CALL ncheck(nf90_def_var(ncId,"lat",nf90_float,(/ timeDim,partDim/),latVar),trim(ncfname))
 CALL ncheck(nf90_put_att(ncId,latVar,"units",'degrees_north'),ncfname)
 CALL ncheck(nf90_put_att(ncId,latVar,"long_name",'Latitude'),ncfname)
 CALL ncheck(nf90_put_att(ncId,latVar,"_FillValue",2.**100),ncfname)

!add depth
 CALL ncheck(nf90_def_var(ncId,"depth",nf90_float,(/ timeDim,partDim /),depthVar),trim(ncfname))
 CALL ncheck(nf90_put_att(ncId,depthVar,"units",'meter'),ncfname)
 CALL ncheck(nf90_put_att(ncId,depthVar,"long_name",'Depth'),ncfname)
 CALL ncheck(nf90_put_att(ncId, depthVar,"_FillValue",2.**100),ncfname)

 IF (withibm) THEN
!add distance
  CALL ncheck(nf90_def_var(ncId,"distance",nf90_float,(/ timeDim,partDim /),distanceVar),trim(ncfname))
  CALL ncheck(nf90_put_att(ncId,distanceVar,"units",'kilometer'),ncfname)
  CALL ncheck(nf90_put_att(ncId,distanceVar,"long_name",'Cumulative distance'),ncfname)
  CALL ncheck(nf90_put_att(ncId,distanceVar,"_FillValue",2.**100),ncfname)
 ENDIF

 IF (outputtemp) THEN
!add interpolated temperature
  CALL ncheck(nf90_def_var(ncId,"temperature",nf90_float,(/ timeDim,partDim /),tempVar),trim(ncfname))
  CALL ncheck(nf90_put_att(ncId,tempVar,"units",'degC'),ncfname)
  CALL ncheck(nf90_put_att(ncId,tempVar,"long_name",'interpolated along-track temperature'),ncfname)
  CALL ncheck(nf90_put_att(ncId,tempVar,"_FillValue",2.**100),ncfname)
 ENDIF

 IF (outputsaln) THEN
!add interpolated salinity
  CALL ncheck(nf90_def_var(ncId,"salinity",nf90_float,(/ timeDim,partDim /),salnVar),trim(ncfname))
  CALL ncheck(nf90_put_att(ncId,salnVar,"units",'psu'),ncfname)
  CALL ncheck(nf90_put_att(ncId,salnVar,"long_name",'interpolated along-track salinity'),ncfname)
  CALL ncheck(nf90_put_att(ncId,salnVar,"_FillValue",2.**100),ncfname)
 ENDIF

 IF (mixedlayerphysics) THEN
!add whether in mixed layer
  CALL ncheck(nf90_def_var(ncId,"inmld",nf90_int,(/ timeDim,partDim /),mldVar),trim(ncfname))
  CALL ncheck(nf90_put_att(ncId,mldVar,"long_name",'In mixed layer'),ncfname)
  CALL ncheck(nf90_put_att(ncId,mldVar,"_FillValue",100),ncfname)
 ENDIF

!add exitcode
 CALL ncheck(nf90_def_var(ncId,"exitcode",nf90_int,(/ partDim /),statusVar),trim(ncfname))
 CALL ncheck(nf90_put_att(ncId,statusVar,"long_name",'Status with which the particle exits'),ncfname)

!add release date
 CALL ncheck(nf90_def_var(ncId,"releasedate",nf90_double,(/ partDim /),releaseVar),trim(ncfname))
 CALL ncheck(nf90_put_att(ncId,releaseVar,"units",'Julian date'),ncfname)
 CALL ncheck(nf90_put_att(ncId,releaseVar,"long_name",'Date the particle is released'),ncfname)

!add release polygon if flag polygon is turned on
 IF (polygon) THEN
  CALL ncheck(nf90_def_var(ncId,"releasepolygon",nf90_int,(/ partDim /),polyVar),trim(ncfname))
  CALL ncheck(nf90_put_att(ncId,polyVar,"long_name",'Number of release polygon'),ncfname)
 ENDIF

!only output density and diameter of particle if buoyancy or massspawning is turned on
 IF (buoyancy .or. massSpawning .or. diffpart) THEN
  CALL ncheck(nf90_def_var(ncId,"density",nf90_float,(/ partDim /),densVar),trim(ncfname))
  CALL ncheck(nf90_put_att(ncId,densVar,"units",'kg/m3'),ncfname)
  CALL ncheck(nf90_put_att(ncId,densVar,"long_name",'Density of particle'),ncfname)

  CALL ncheck(nf90_def_var(ncId,"diameter",nf90_float,(/ partDim /),sizeVar),trim(ncfname))
  CALL ncheck(nf90_put_att(ncId,sizeVar,"units",'meter'),ncfname)
  CALL ncheck(nf90_put_att(ncId,sizeVar,"long_name",'Diameter of particle') ,ncfname)
 ENDIF

!add global attributes
 CALL ncheck(nf90_put_att(ncId,nf90_global,"nnests", nnests),ncfname)
 CALL ncheck(nf90_put_att(ncId,nf90_global,"timeMax", timeMax),ncfname)
 CALL ncheck(nf90_put_att(ncId,nf90_global,"timeStep", timeStep),ncfname)
 CALL ncheck(nf90_put_att(ncId,nf90_global,"releaseFilename", releaseFilename),ncfname)
 IF (turb) THEN
  CALL ncheck(nf90_put_att(ncId,nf90_global,"turb", ".true."),ncfname)
  CALL ncheck(nf90_put_att(ncId,nf90_global,"horDiff", horDiff(1:nnests)),ncfname)
  CALL ncheck(nf90_put_att(ncId,nf90_global,"vertDiff", vertDiff(1:nnests)),ncfname)
  CALL ncheck(nf90_put_att(ncId,nf90_global,"turbTimestep", turbTimestep),ncfname)
 ENDIF
 IF (periodicbc) THEN
  CALL ncheck(nf90_put_att(ncId,nf90_global,"periodicbc", ".true."),ncfname)
 ENDIF
 IF (avoidcoast) THEN
  CALL ncheck(nf90_put_att(ncId,nf90_global,"avoidcoast", ".true."),ncfname)
 ENDIF
 IF (backward) THEN
  CALL ncheck(nf90_put_att(ncId,nf90_global,"backward", ".true."),ncfname)
 ENDIF
 IF (buoyancy) THEN
  CALL ncheck(nf90_put_att(ncId,nf90_global,"buoyancy", ".true."),ncfname)
  IF (diffpart) THEN
   CALL ncheck(nf90_put_att(ncId,nf90_global,"diffpart", ".true."),ncfname)
   CALL ncheck(nf90_put_att(ncId,nf90_global,"diffpartFilename", diffpartFilename),ncfname)
  ELSE
   CALL ncheck(nf90_put_att(ncId,nf90_global,"dens_particle", dens_particle),ncfname)
   CALL ncheck(nf90_put_att(ncId,nf90_global,"diam_particle", diam_particle),ncfname)  
  ENDIF 
 ENDIF
 IF (polygon) THEN
  CALL ncheck(nf90_put_att(ncId,nf90_global,"polygon", ".true."),ncfname)
  CALL ncheck(nf90_put_att(ncId,nf90_global,"polyFilename", polyFilename),ncfname)
  CALL ncheck(nf90_put_att(ncId,nf90_global,"settlementStart", settlementStart),ncfname)
 ENDIF
 IF (ibio) THEN
  CALL ncheck(nf90_put_att(ncId,nf90_global,"ibio", ".true."),ncfname)
  CALL ncheck(nf90_put_att(ncId,nf90_global,"ibioFilename", ibioFilename),ncfname)
  CALL ncheck(nf90_put_att(ncId,nf90_global,"ibioTimestep", ibioTimestep),ncfname)
 ENDIF
 IF (mort) THEN
  CALL ncheck(nf90_put_att(ncId,nf90_global,"mort", ".true."),ncfname)
  IF (diffpart) THEN
   CALL ncheck(nf90_put_att(ncId,nf90_global,"diffpart", ".true."),ncfname)
   CALL ncheck(nf90_put_att(ncId,nf90_global,"diffpartFilename", diffpartFilename),ncfname)
  ELSE
   CALL ncheck(nf90_put_att(ncId,nf90_global,"halflife", halflife),ncfname)
  ENDIF 
 ENDIF
 IF (massSpawning) THEN
  CALL ncheck(nf90_put_att(ncId,nf90_global,"massSpawning", ".true."),ncfname)
  CALL ncheck(nf90_put_att(ncId,nf90_global,"larvaStart", larvaStart),ncfname)
 ENDIF
 IF (tidalMovement) THEN
  CALL ncheck(nf90_put_att(ncId,nf90_global,"tidalMovement", ".true."),ncfname)
  CALL ncheck(nf90_put_att(ncId,nf90_global,"tstStart", tstStart),ncfname)
 ENDIF
!end define mode

 CALL ncheck(nf90_enddef(ncid),trim(ncfname))

END SUBROUTINE output_create

!**************************************************************

!write location of particle to outputfile
SUBROUTINE output_write_loc(ncfname, particleId, timeId,locnr, time, &
     lon, lat, depth,distance, stat,date,poly, dens, diam, temp, saln, sync, &
     polygon,buoyancy, massSpawning,withibm,outputtemp,outputsaln,inmld)

 character (len=*), intent(in)         :: ncfname
 integer  (kind=int_kind), intent(in)  :: particleId, timeId
 integer  (kind=int_kind), intent(in)  :: locnr, stat, poly, inmld
 real (kind = real_kind), intent(in)   :: lon, lat, depth, distance,dens, diam, temp, saln
 double precision, intent(in)          :: date
 integer (kind=int8_kind), intent(in)  :: time
 logical (kind =log_kind), intent(in)  :: sync,polygon,buoyancy, massSpawning, withibm, outputtemp, outputsaln

!put variable
 CALL ncheck(nf90_put_var(ncId, timeVar,time,start =(/ timeId /)),trim(ncfname))
 IF (timeId .eq. 1) THEN
  CALL ncheck(nf90_put_var(ncId, locVar,locnr,start =(/ particleId /)),trim(ncfname)) 
 ENDIF
 CALL ncheck(nf90_put_var(ncId, lonVar,lon,(/timeId,particleId /)),trim(ncfname))     
 CALL ncheck(nf90_put_var(ncId, latVar,lat,(/timeId,particleId /)),trim(ncfname))     
 CALL ncheck(nf90_put_var(ncId, depthVar,depth,(/timeId,particleId /)),trim(ncfname))
 IF (withibm) THEN
  CALL ncheck(nf90_put_var(ncId, distanceVar,distance,(/timeId,particleId /)),trim(ncfname))
 ENDIF
 IF (outputtemp) THEN
  CALL ncheck(nf90_put_var(ncId, tempVar,temp,(/timeId,particleId /)),trim(ncfname))
 ENDIF
 IF (outputsaln) THEN
  CALL ncheck(nf90_put_var(ncId, salnVar,saln,(/timeId,particleId /)),trim(ncfname))
 ENDIF
 IF (inmld .ge. 0) THEN
  CALL ncheck(nf90_put_var(ncId, mldVar,inmld,(/timeId,particleId /)),trim(ncfname))
 ENDIF
 IF (timeId .eq. 1) THEN
  CALL ncheck(nf90_put_var(ncId, statusVar,0,(/particleId /)),trim(ncfname))   
 ENDIF
 IF (stat .lt. 0) THEN     
  CALL ncheck(nf90_put_var(ncId, statusVar,stat,(/particleId /)),trim(ncfname))   
 ENDIF
 IF (timeId .eq. 1) THEN
  CALL ncheck(nf90_put_var(ncId, releaseVar,date,(/ particleId /)),trim(ncfname))
 ENDIF

 IF (polygon) THEN
  IF (timeId .eq. 1) THEN
   CALL ncheck(nf90_put_var(ncId, polyVar,poly,(/particleId /)),trim(ncfname))   
  ENDIF
 ENDIF

!only output size and density of particle if buoyancy or massspawning is turned on
 IF (buoyancy .or. massSpawning) THEN
  IF (timeId .eq. 1) THEN
   CALL ncheck(nf90_put_var(ncId,densVar,dens,start =(/ particleId /)),trim(ncfname)) 
   CALL ncheck(nf90_put_var(ncId,sizeVar,diam,start = (/particleId /)),trim(ncfname))   
  ENDIF
 ENDIF

 IF (sync) THEN
  CALL ncheck(nf90_sync(ncid),trim(ncfname)) 
 ENDIF

END SUBROUTINE output_write_loc

!**************************************************************

SUBROUTINE output_close(ncfname)
 character (len=*), intent(in)         :: ncfname

 CALL nc_close(ncfname, ncId) 

END SUBROUTINE output_close

!**************************************************************

END MODULE mod_netcdfoutput

