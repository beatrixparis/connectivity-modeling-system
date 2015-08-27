!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : getfilename.f90                                                   *
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



!returns file names that contain physical data 

!returns two filenames
SUBROUTINE getflnm(nest,r,lc,fname)

 USE globalvariables
 USE constants
 USE mod_kinds
 USE mod_calendar

 IMPLICIT NONE

 integer (kind=int_kind),intent(in)  :: r, nest
 integer (kind=int8_kind),intent(in) :: lc
 character(char_len),    intent(out) :: fname(3)
 integer (kind=int8_kind)    :: lc1, lc2, lc3, ss
 integer (kind=int_kind)     :: yyyy, mm, dd, hh, mi,jj, extradays


!if time_units are days or seconds
!It must be possible to divide lc by time_step
 lc1 = lc - mod(lc,nests(nest)%time_step)
 IF (backward) then
  lc1 = -1 * lc1
 ENDIF
!calculate the correct filename
 yyyy=particle(r)%year
 mm=particle(r)%month
 dd=particle(r)%day
 ss=lc1
 IF (backward) then
  DO WHILE (ss .lt. 0)
   ss = ss + 86400
   CALL jd(yyyy, mm, dd, jj)
   jj = jj - 1
   CALL cdate(jj, yyyy, mm, dd)
  ENDdo
 ENDIF
 mi = ss/60
 ss = mod(ss,60)
 hh = mi/60
 mi = mod(mi,60)
 extradays = hh/24
 hh = mod(hh,24)
 CALL jd(yyyy, mm, dd, jj)
 jj = jj + extradays
 CALL cdate(jj, yyyy, mm, dd)
!create filename 1
 write(fname(1),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'.nc'
 fname(1) = adjustl(trim(filenest))//trim(fname(1))

!calculates second filename 
 lc2=lc1+nests(nest)%time_step
!backward
 IF (backward) then
  lc2 = lc1-nests(nest)%time_step
 ENDIF
 yyyy=particle(r)%year
 mm=particle(r)%month
 dd=particle(r)%day
 ss=lc2
 IF (backward) then
  do WHILE (ss .lt. 0)
   ss = ss + 86400
   CALL jd(yyyy, mm, dd, jj)
   jj = jj - 1
   CALL cdate(jj, yyyy, mm, dd)
  ENDdo
 ENDIF
 mi = ss/60
 ss = mod(ss,60)
 hh = mi/60
 mi = mod(mi,60)
 extradays = hh/24
 hh = mod(hh,24)
 CALL jd(yyyy, mm, dd, jj)
 jj = jj + extradays
 CALL cdate(jj, yyyy, mm, dd)

!create filename 2
 write(fname(2),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'.nc'
 fname(2) = adjustl(trim(filenest))//trim(fname(2))


!calculates third filename 
 lc3=lc2+nests(nest)%time_step
!backward
 IF (backward) then
  lc3 = lc2-nests(nest)%time_step
 ENDIF
 yyyy=particle(r)%year
 mm=particle(r)%month
 dd=particle(r)%day
 ss=lc3
 IF (backward) then
  do WHILE (ss .lt. 0)
   ss = ss + 86400
   CALL jd(yyyy, mm, dd, jj)
   jj = jj - 1
   CALL cdate(jj, yyyy, mm, dd)
  ENDdo
 ENDIF
 mi = ss/60
 ss = mod(ss,60)
 hh = mi/60
 mi = mod(mi,60)
 extradays = hh/24
 hh = mod(hh,24)
 CALL jd(yyyy, mm, dd, jj)
 jj = jj + extradays
 CALL cdate(jj, yyyy, mm, dd)

!create filename 2
 write(fname(3),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'.nc'
 fname(3) = adjustl(trim(filenest))//trim(fname(3))

END SUBROUTINE getflnm

!**************************************************************
!returns four filenames if time_units are months
SUBROUTINE getflnmMonthly(nest,r,lc,fname)

 USE globalvariables
 USE constants
 USE mod_kinds
 USE mod_calendar

 IMPLICIT NONE

 integer (kind=int_kind),intent(in)  :: r, nest
 integer (kind=int8_kind),intent(in) :: lc 
 character(char_len),    intent(out) :: fname(5)
 integer (kind=int_kind) :: dd, hh, mi,ss,jj,extradays, &
     mm,mm2, mm3, mm4,mm5,yyyy,yyyy2,yyyy3,yyyy4,yyyy5


!calculate fname1
 yyyy=particle(r)%year
 mm=particle(r)%month
 dd=particle(r)%day
 IF (backward) then
  extradays = lc/secs_in_day
  CALL jd(yyyy, mm, dd, jj)
  jj = jj - extradays
  CALL cdate(jj, yyyy, mm, dd)
 else
  extradays = lc/secs_in_day
  CALL jd(yyyy, mm, dd, jj)
  jj = jj + extradays
  CALL cdate(jj, yyyy, mm, dd)
 ENDIF

 IF (backward) then
  IF (dd .ge. nests(nest)%tstart_dd) then
   mm = mm + 1
   IF (mm == 13) then
    mm = 1
    yyyy = yyyy + 1
   ENDIF
  ENDIF
 else
  IF (dd .lt. nests(nest)%tstart_dd) then
   mm = mm - 1
   IF (mm == 0) then
    mm =12
    yyyy = yyyy -1
   ENDIF
  ENDIF
 ENDIF
 dd=nests(nest)%tstart_dd     
 hh = 0
 mi = 0
 ss = 0

 write(fname(2),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'.nc'
 fname(2) = adjustl(trim(filenest))//trim(fname(2))

!calculate fname2 
 IF (backward) then
  mm2 = mm - int(nests(nest)%time_step/(30*secs_in_day))
 else
  mm2 = mm + int(nests(nest)%time_step/(30*secs_in_day))
 ENDIF
   
 yyyy2 = yyyy
 IF (mm2 == 13) then
  mm2 = 1
  yyyy2 = yyyy2 + 1
 ENDIF
 IF (mm2 == 0) then
  mm2 = 12
  yyyy2 = yyyy2 - 1
 ENDIF

 write(fname(3),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                'nest_',nest,'_',yyyy2,mm2,dd,hh,mi,ss,'.nc'
 fname(3) = adjustl(trim(filenest))//trim(fname(3))

!calculate fname3
 IF (backward) then
  mm3 = mm - 2*(int(nests(nest)%time_step/(30*secs_in_day)))
 else
  mm3 = mm + 2*(int(nests(nest)%time_step/(30*secs_in_day)))
 ENDIF
   
 yyyy3 = yyyy
 IF (mm3 .gt. 12) then
  mm3 = mm3 - 12
  yyyy3 = yyyy3 + 1
 ENDIF
 IF (mm3 .le. 0) then
  mm3 = mm3 + 12
  yyyy3 = yyyy3 - 1
 ENDIF

 write(fname(4),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                'nest_',nest,'_',yyyy3,mm3,dd,hh,mi,ss,'.nc'
 fname(4) = adjustl(trim(filenest))//trim(fname(4))

!calculate fname0
 IF (backward) then
  mm4 = mm + int(nests(nest)%time_step/(30*secs_in_day))
 else
  mm4 = mm - int(nests(nest)%time_step/(30*secs_in_day))
 ENDIF
   
 yyyy4 = yyyy
 IF (mm4 == 0) then
  mm4 = 12
  yyyy4 = yyyy4  - 1
 ENDIF
 IF (mm4 == 13) then
  mm4 = 1
  yyyy4 = yyyy4  + 1
 ENDIF

 write(fname(1),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
     &          'nest_',nest,'_',yyyy4,mm4,dd,hh,mi,ss,'.nc'
 fname(1) = adjustl(trim(filenest))//trim(fname(1))

!calculate fname4
 IF (backward) then
  mm5 = mm - 3*(int(nests(nest)%time_step/(30*secs_in_day)))
 else
  mm5 = mm + 3*(int(nests(nest)%time_step/(30*secs_in_day)))
 ENDIF
   
 yyyy5 = yyyy
 IF (mm5 .gt. 12) then
  mm5 = mm5 - 12
  yyyy5 = yyyy5 + 1
 ENDIF
 IF (mm3 .le. 0) then
  mm5 = mm5 + 12
  yyyy5 = yyyy5 - 1
 ENDIF

 write(fname(5),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                'nest_',nest,'_',yyyy5,mm5,dd,hh,mi,ss,'.nc'
 fname(5) = adjustl(trim(filenest))//trim(fname(5))
   
END SUBROUTINE getflnmMonthly



