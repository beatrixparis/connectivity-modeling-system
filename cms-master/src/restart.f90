!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : output.f90                                                        *
!* Last Modified: 2012-05-04                                               *
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

SUBROUTINE writerestartfile(locname, startR, endR, time_t)

 USE mod_kinds
 USE constants
 USE mod_calendar
 USE globalvariables
 USE mod_iounits

 IMPLICIT NONE

 character(Len=*),intent(in)         :: locname
 integer (kind=int8_kind), intent(in) :: time_t
 integer (kind=int_kind), intent(in) :: startR, endR
 integer (kind=int_kind) :: r, n
 character(char_len)     :: rfname

 restartname = "restart_"//trim(locname)//".bin"
 print *,'Writing restart file'
 CALL get_unit(iunit_restart)
 open (unit=iunit_restart,file=trim(filescratch)//trim(restartname),status="unknown",form="unformatted")

 write(iunit_restart) time_t, startR, endR
 
 DO r=startR,endR
  write(iunit_restart) particle(r)%id, particle(r)%num_rel, particle(r)%seconds, particle(r)%day, & 
       particle(r)%month, particle(r)%year, particle(r)%start, particle(r)%ilon, particle(r)%ilat, &
       particle(r)%idepth, particle(r)%rel_loc_name
  DO n=1, particle(r)%num_rel
   IF (withibm) THEN
    write(iunit_restart) particle(r)%dist(n), particle(r)%nlon(n), particle(r)%nlat(n), particle(r)%ndepth(n), &
          particle(r)%old_lonDist(n), particle(r)%old_latDist(n), particle(r)%diam(n), particle(r)%density(n), &
          particle(r)%halflife(n), particle(r)%layer(n), particle(r)%move(n), particle(r)%flag(n,1), &
          particle(r)%flag(n,2), particle(r)%flag(n,3), particle(r)%flag(n,4), particle(r)%flag(n,5), &
          particle(r)%flag(n,6), particle(r)%flag(n,7), particle(r)%flag(n,8), particle(r)%flag(n,9), &
          particle(r)%flag(n,10)
   ELSE
    write(iunit_restart) particle(r)%nlon(n), particle(r)%nlat(n), particle(r)%ndepth(n), &
          particle(r)%move(n), particle(r)%flag(n,1), particle(r)%flag(n,2), particle(r)%flag(n,3), &
          particle(r)%flag(n,4), particle(r)%flag(n,5), particle(r)%flag(n,6), particle(r)%flag(n,7), & 
          particle(r)%flag(n,8), particle(r)%flag(n,9), particle(r)%flag(n,10)
   ENDIF
  ENDDO
 ENDDO
 close(iunit_restart)
 CALL release_unit(iunit_restart)
 END SUBROUTINE writerestartfile

!**************************************************************

 SUBROUTINE readrestartfile(locname,startR,endR,time_t)

 USE mod_kinds
 USE constants
 USE mod_calendar
 USE globalvariables
 USE mod_iounits

 IMPLICIT NONE

 character(Len=*),intent(in)         :: locname
 integer (kind=int8_kind), intent(out) :: time_t
 integer (kind=int_kind), intent(out) :: startR, endR
 integer (kind=int_kind) :: r, n
 character(char_len)     :: rfname

 restartname = "restart_"//trim(locname)//".bin"
 print *,'Restarting from file' 
 CALL get_unit(iunit_restart)
 open (unit=iunit_restart,file=trim(filescratch)//trim(restartname),status="unknown",form="unformatted")

 read(iunit_restart) time_t, startR, endR

 DO r=startR, endR
  read(iunit_restart) particle(r)%id, particle(r)%num_rel, particle(r)%seconds, particle(r)%day, & 
       particle(r)%month, particle(r)%year, particle(r)%start, particle(r)%ilon, particle(r)%ilat, &
       particle(r)%idepth, particle(r)%rel_loc_name
  DO n=1, particle(r)%num_rel
   IF (withibm) THEN
    read(iunit_restart) particle(r)%dist(n), particle(r)%nlon(n), particle(r)%nlat(n), particle(r)%ndepth(n), &
          particle(r)%old_lonDist(n), particle(r)%old_latDist(n), particle(r)%diam(n), particle(r)%density(n), &
          particle(r)%halflife(n), particle(r)%layer(n), particle(r)%move(n), particle(r)%flag(n,1), &
          particle(r)%flag(n,2), particle(r)%flag(n,3), particle(r)%flag(n,4), particle(r)%flag(n,5), &
          particle(r)%flag(n,6), particle(r)%flag(n,7), particle(r)%flag(n,8), particle(r)%flag(n,9), &
          particle(r)%flag(n,10)
   ELSE
    read(iunit_restart) particle(r)%nlon(n), particle(r)%nlat(n), particle(r)%ndepth(n), &
          particle(r)%move(n), particle(r)%flag(n,1), particle(r)%flag(n,2), particle(r)%flag(n,3), &
          particle(r)%flag(n,4), particle(r)%flag(n,5), particle(r)%flag(n,6), particle(r)%flag(n,7), &
          particle(r)%flag(n,8), particle(r)%flag(n,9), particle(r)%flag(n,10)
   ENDIF
  ENDDO
 ENDDO
 print *, 'Succesfully read ',endR-startR+1,' particles for restart'

 close(iunit_restart)
 CALL release_unit(iunit_restart)
 END SUBROUTINE readrestartfile

!**************************************************************


