!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : getdata.f90                                                       *
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

PROGRAM getData
 USE mod_kinds
 USE mod_getdata
       
 IMPLICIT NONE
 
 character(char_len)     :: filenumber,pnest_n_char
 integer (kind=int_kind) :: pnest_n
      
      
!check which experiment to run
 IF (command_argument_count() .lt. 2) THEN
   print *, "You have to enter the experiment number/name you want to run and the number of the nest you want to get the data for"
   stop
 ENDIF
 CALL getarg(1,filenumber)
 CALL getarg(2,pnest_n_char)

! from char to int
! Value 80 is equal to char_len, if char_len changes change the value 80 too.
  read (pnest_n_char,'(I80)') pnest_n
             
  CALL get_data(pnest_n, filenumber)
       
END PROGRAM getData
