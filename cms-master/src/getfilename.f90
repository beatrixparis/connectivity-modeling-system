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
SUBROUTINE getflnm(nest,basedate,lc,fname)

 USE globalvariables
 USE constants
 USE mod_kinds
 USE mod_calendar

 IMPLICIT NONE

 integer (kind=int_kind),intent(in)  :: nest,basedate
 integer (kind=int8_kind),intent(in) :: lc
 character(char_len),    intent(out) :: fname(4,3)
 integer (kind=int8_kind)    :: lc1, lc2, lc3, ss
 integer (kind=int_kind)     :: yyyy, mm, dd, hh, mi,jj, extradays, jjfirstfile, jjlastfile, ax

 IF (loopfiles .eqv. .true.) THEN
  CALL jd(loopfilesstartyear,loopfilesstartmonth,loopfilesstartday,jjfirstfile)
  jjfirstfile=jjfirstfile-nests(nest)%time_step/(1.*secs_in_day)
  CALL jd(loopfilesendyear,loopfilesendmonth,loopfilesendday,jjlastfile)
 ENDIF

!if time_units are days or seconds
!It must be possible to divide lc by time_step
 lc1 = lc - mod(lc,nests(nest)%time_step)
 IF (backward) then
  lc1 = -1 * lc1
 ENDIF
!calculate the correct filename
 CALL cdate(basedate,yyyy,mm,dd)
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
 IF (loopfiles .eqv. .true.) THEN
  IF (jj .gt. jjlastfile) THEN
   DO WHILE (jj.gt.jjlastfile)
    jj=jj-jjlastfile+jjfirstfile
   ENDDO
  ELSEIF (jj .le. jjfirstfile) THEN
   DO WHILE (jj.le.jjfirstfile)
    jj=jj+jjlastfile-jjfirstfile
   ENDDO
  ENDIF
 ENDIF
 CALL cdate(jj, yyyy, mm, dd)
!create filename 1
 if (agrid) then
  write(fname(UAx,1),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'.nc'
 else
  write(fname(1,1),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'u.nc'
  write(fname(2,1),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'v.nc'
  write(fname(3,1),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'w.nc'
  write(fname(4,1),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'t.nc'
 endif
 do ax=1,QAx
  fname(ax,1) = adjustl(trim(filenest))//trim(fname(ax,1))
 enddo

!calculates second filename 
 lc2=lc1+nests(nest)%time_step
!backward
 IF (backward) then
  lc2 = lc1-nests(nest)%time_step
 ENDIF
 CALL cdate(basedate,yyyy,mm,dd)
 ss=lc2
 IF (backward) then
  do WHILE (ss .lt. 0)
   ss = ss + 86400
   CALL jd(yyyy, mm, dd, jj)
   jj = jj - 1
   CALL cdate(jj, yyyy, mm, dd)
  ENDDO
 ENDIF
 mi = ss/60
 ss = mod(ss,60)
 hh = mi/60
 mi = mod(mi,60)
 extradays = hh/24
 hh = mod(hh,24)
 CALL jd(yyyy, mm, dd, jj)
 jj = jj + extradays
 IF (loopfiles .eqv. .true.) THEN
  IF (jj .gt. jjlastfile) THEN
   DO WHILE (jj.gt.jjlastfile)
    jj=jj-jjlastfile+jjfirstfile
   ENDDO
  ELSEIF (jj .le. jjfirstfile) THEN
   DO WHILE (jj.le.jjfirstfile)
    jj=jj+jjlastfile-jjfirstfile
   ENDDO
  ENDIF
 ENDIF
 CALL cdate(jj, yyyy, mm, dd)

 if (agrid) then
  write(fname(UAx,2),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'.nc'
 else
  write(fname(1,2),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'u.nc'
  write(fname(2,2),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'v.nc'
  write(fname(3,2),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'w.nc'
  write(fname(4,2),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'t.nc'
 endif
 do ax=1,QAx
  fname(ax,2) = adjustl(trim(filenest))//trim(fname(ax,2))
 enddo


!calculates third filename 
 lc3=lc2+nests(nest)%time_step
!backward
 IF (backward) THEN
  lc3 = lc2-nests(nest)%time_step
 ENDIF
 CALL cdate(basedate,yyyy,mm,dd)
 ss=lc3
 IF (backward) THEN
  DO WHILE (ss .lt. 0)
   ss = ss + 86400
   CALL jd(yyyy, mm, dd, jj)
   jj = jj - 1
   CALL cdate(jj, yyyy, mm, dd)
  ENDDO
 ENDIF
 mi = ss/60
 ss = mod(ss,60)
 hh = mi/60
 mi = mod(mi,60)
 extradays = hh/24
 hh = mod(hh,24)
 CALL jd(yyyy, mm, dd, jj)
 jj = jj + extradays
 IF (loopfiles .eqv. .true.) THEN
  IF (jj .gt. jjlastfile) THEN
   DO WHILE (jj.gt.jjlastfile)
    jj=jj-jjlastfile+jjfirstfile
   ENDDO
  ELSEIF (jj .le. jjfirstfile) THEN
   DO WHILE (jj.le.jjfirstfile)
    jj=jj+jjlastfile-jjfirstfile
   ENDDO
  ENDIF
 ENDIF
 CALL cdate(jj, yyyy, mm, dd)

!create filename 2
 if (agrid) then
  write(fname(UAx,3),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'.nc'
 else
  write(fname(1,3),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'u.nc'
  write(fname(2,3),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'v.nc'
  write(fname(3,3),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'w.nc'
  write(fname(4,3),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'t.nc'
 endif
 do ax=1,QAx
  fname(ax,3) = adjustl(trim(filenest))//trim(fname(ax,3))
 enddo

END SUBROUTINE getflnm

!**************************************************************
!returns four filenames if time_units are months
SUBROUTINE getflnmMonthly(nest,basedate,lc,fname)

 USE globalvariables
 USE constants
 USE mod_kinds
 USE mod_calendar

 IMPLICIT NONE

 integer (kind=int_kind),intent(in)  :: nest, basedate
 integer (kind=int8_kind),intent(in) :: lc 
 character(char_len),    intent(out) :: fname(4,5)
 integer (kind=int_kind) :: dd, hh, mi,ss,jj,extradays, &
     mm,yyyy,yymm,yymm2,yymm3,yymm4,yymm5,yymmstart,yymmend, ax

 IF (loopfiles .eqv. .true.) THEN
  yymmstart = loopfilesstartyear *12 + (loopfilesstartmonth - 1)
  yymmend   = loopfilesendyear   *12 + (loopfilesendmonth   - 1)
 ENDIF

!calculate fname1
 CALL cdate(basedate,yyyy,mm,dd)
 extradays = lc/secs_in_day
 CALL jd(yyyy, mm, dd, jj)
 IF (backward) then
  jj = jj - extradays
 else
  jj = jj + extradays
 ENDIF
 CALL cdate(jj, yyyy, mm, dd)
 yymm=yyyy*12+(mm-1)

 IF (backward) THEN
  IF (dd .gt. nests(nest)%tend_dd) THEN
   yymm = yymm + 1
  ENDIF
 ELSE 
  IF (dd .lt. nests(nest)%tstart_dd) THEN
   yymm = yymm - 1
  ENDIF
 ENDIF
 IF (loopfiles .eqv. .true.) THEN
  call loopmonthly(yymm,yymmstart,yymmend)
 ENDIF
 yyyy = yymm/12
 mm = mod(yymm,12)+1
 dd = nests(nest)%tstart_dd
 hh = 0
 mi = 0
 ss = 0

 if (agrid) then
  write(fname(UAx,2),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'.nc'
 else
  write(fname(1,2),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'u.nc'
  write(fname(2,2),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'v.nc'
  write(fname(3,2),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'w.nc'
  write(fname(4,2),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'t.nc'
 endif
 do ax=1,QAx
  fname(ax,2) = adjustl(trim(filenest))//trim(fname(ax,2))
 enddo

!calculate fname2 
 IF (backward) then
  yymm2 = yymm - 1
 else
  yymm2 = yymm + 1
 ENDIF
 IF (loopfiles .eqv. .true.) THEN
  call loopmonthly(yymm2,yymmstart,yymmend)
 ENDIF
 yyyy = yymm2/12
 mm = mod(yymm2,12)+1
 if (agrid) then
  write(fname(UAx,3),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'.nc'
 else
  write(fname(1,3),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'u.nc'
  write(fname(2,3),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'v.nc'
  write(fname(3,3),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'w.nc'
  write(fname(4,3),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'t.nc'
 endif
 do ax=1,QAx
  fname(ax,3) = adjustl(trim(filenest))//trim(fname(ax,3))
 enddo

!calculate fname3
 IF (backward) then
  yymm3 = yymm - 2
 else
  yymm3 = yymm + 2
 ENDIF
 IF (loopfiles .eqv. .true.) THEN
  call loopmonthly(yymm3,yymmstart,yymmend)
 ENDIF
 yyyy = yymm3/12
 mm = mod(yymm3,12)+1
 if (agrid) then
  write(fname(UAx,4),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'.nc'
 else
  write(fname(1,4),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'u.nc'
  write(fname(2,4),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'v.nc'
  write(fname(3,4),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'w.nc'
  write(fname(4,4),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'t.nc'
 endif
 do ax=1,QAx
  fname(ax,4) = adjustl(trim(filenest))//trim(fname(ax,4))
 enddo

!calculate fname0
 IF (backward) then
  yymm4 = yymm + 1
 else
  yymm4 = yymm - 1
 ENDIF
 IF (loopfiles .eqv. .true.) THEN
  call loopmonthly(yymm4,yymmstart,yymmend)
 ENDIF
 yyyy = yymm4/12
 mm = mod(yymm4,12)+1
 if (agrid) then
  write(fname(UAx,1),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'.nc'
 else
  write(fname(1,1),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'u.nc'
  write(fname(2,1),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'v.nc'
  write(fname(3,1),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'w.nc'
  write(fname(4,1),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'t.nc'
 endif
 do ax=1,QAx
  fname(ax,1) = adjustl(trim(filenest))//trim(fname(ax,1))
 enddo

!calculate fname4
 IF (backward) then
  yymm5 = yymm - 3
 else
  yymm5 = yymm + 3
 ENDIF
 IF (loopfiles .eqv. .true.) THEN
  call loopmonthly(yymm5,yymmstart,yymmend)
 ENDIF
 yyyy = yymm5/12
 mm = mod(yymm5,12)+1
 if (agrid) then
  write(fname(UAx,5),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'.nc'
 else
  write(fname(1,5),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'u.nc'
  write(fname(2,5),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'v.nc'
  write(fname(3,5),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'w.nc'
  write(fname(4,5),'(A,I0,A,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2,A)') &
                 'nest_',nest,'_',yyyy,mm,dd,hh,mi,ss,'t.nc'
 endif
 do ax=1,QAx
  fname(ax,5) = adjustl(trim(filenest))//trim(fname(ax,5))
 enddo
END SUBROUTINE getflnmMonthly

SUBROUTINE loopmonthly(yymm,yymmstart,yymmend)
 USE mod_kinds

 IMPLICIT NONE
 integer (kind=int_kind) :: yymm,yymmstart,yymmend

 IF (yymm .gt. yymmend) THEN
  DO WHILE (yymm .gt. yymmend)
    yymm = yymm - 1 - yymmend + yymmstart
  END DO
 ELSEIF (yymm .lt. yymmstart) THEN
  DO WHILE (yymm .lt. yymmstart)
    yymm = yymm + 1 + yymmend - yymmstart
  END DO
 ENDIF

END SUBROUTINE loopmonthly
