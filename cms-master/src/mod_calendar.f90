!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : calendar.f90                                                      *
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

MODULE mod_calendar

USE mod_kinds

IMPLICIT NONE

CONTAINS

!**************************************************************

!calculates how many days are in the given month.
SUBROUTINE DaysMonth(yyyy, mm, days)
 integer (KIND=int_KIND), intent(IN)  :: yyyy
 integer (KIND=int_KIND), intent(IN)  :: mm
 integer (KIND=int_KIND), intent(OUT) :: days

 IF (mm==0) days = 31
 IF (mm==1) days = 31
 IF (mm==2) then
    IF(mod(yyyy,100).ne.0.and.mod(yyyy,4).eq.0)then
      days = 29
    ELSEIF(mod(yyyy,400).eq.0) THEN
      days = 29
    ELSE
      days= 28
    ENDIF
 ENDIF
 IF (mm==3) days = 31
 IF (mm==4) days = 30
 IF (mm==5) days = 31
 IF (mm==6) days = 30
 IF (mm==7) days = 31
 IF (mm==8) days = 31
 IF (mm==9) days = 30
 IF (mm==10) days = 31
 IF (mm==11) days = 30
 IF (mm==12) days = 31

END SUBROUTINE DaysMonth

!**************************************************************

!converts calendar date to julian date number.
SUBROUTINE jd(yyyy, mm, dd, julian)

 integer (KIND=int_KIND), intent(IN)  :: yyyy
 integer (KIND=int_KIND), intent(IN)  :: mm
 integer (KIND=int_KIND), intent(IN)  :: dd
 integer (KIND=int_KIND), intent(OUT) :: julian
 integer (KIND=int_KIND):: A,B,C

!The algorithm is valid for all Gregorian calendar dates after 4800 BC
 A = (14-mm)/12
 B = yyyy + 4800 - A
 C = mm + (12*A) - 3
 julian = dd + ((153*C+2)/5) + 365*B + (B/4) - (B/100) + (B/400) - 32045
      
END SUBROUTINE

!**************************************************************

!converts julian date number to calendar date
SUBROUTINE cdate(jd, yyyy, mm, dd)
 integer (KIND=int_KIND), intent(IN)   :: jd
 integer (KIND=int_KIND), intent(OUT)  :: yyyy
 integer (KIND=int_KIND), intent(OUT)  :: mm
 integer (KIND=int_KIND), intent(OUT)  :: dd
 integer (KIND=int_KIND)               :: j,g,dg,c,dc,b,db,a,da

 j = jd + 32044
 g = int(j/146097); dg = mod(j, 146097)
 c = int((int(dg/36524) + 1) * 3 / 4); dc = dg - c * 36524
 b = int(dc/1461); db = mod(dc, 1461)
 a = int((int(db/365) + 1) * 3 / 4); da = db - a * 365
 yyyy = g * 400 + c * 100 + b * 4 + a
 mm = int((da * 5 + 308)/ 153) - 2
 dd = int(da - (mm + 4) * 153 / 5) + 122
 yyyy = int(yyyy - 4800 + (mm + 2) / 12)
 mm = mod((mm + 2),12) + 1
 dd = dd + 1

END SUBROUTINE cdate

!**************************************************************

!calculates number of days between two dates.
!days is positive if date1 is more recent than date2
SUBROUTINE ndays(mm1, dd1, yyyy1, mm2, dd2, yyyy2, days)
 
 integer (KIND=int_KIND), intent(IN)  :: mm1
 integer (KIND=int_KIND), intent(IN)  :: dd1
 integer (KIND=int_KIND), intent(IN)  :: yyyy1
 integer (KIND=int_KIND), intent(IN)  :: mm2
 integer (KIND=int_KIND), intent(IN)  :: dd2
 integer (KIND=int_KIND), intent(IN)  :: yyyy2
 integer (KIND=int_KIND), intent(OUT) :: days
 integer (KIND=int_KIND):: jd1, jd2

 CALL jd(yyyy1, mm1, dd1, jd1)
 CALL jd(yyyy2, mm2, dd2, jd2)
 days = jd1 - jd2

END SUBROUTINE ndays

!**************************************************************

!calculates number of months between two dates.
!nmonths is positive if date1 is more recent than date2
SUBROUTINE nmonths(mm1,yyyy1,mm2,yyyy2, months)
 integer (KIND=int_KIND), intent(IN)  :: mm1
 integer (KIND=int_KIND), intent(IN)  :: yyyy1
 integer (KIND=int_KIND), intent(IN)  :: mm2
 integer (KIND=int_KIND), intent(IN)  :: yyyy2
 integer (KIND=int_KIND), intent(OUT) :: months

 months = (yyyy1 - yyyy2)*12 + (mm1 - mm2)  

END SUBROUTINE nmonths

END MODULE mod_calendar

