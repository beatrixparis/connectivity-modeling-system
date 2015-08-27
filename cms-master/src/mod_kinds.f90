!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : mod_kinds.f90                                                     *
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

MODULE mod_kinds

IMPLICIT NONE
save

INTEGER, PARAMETER :: char_len  = 80, &
                     int_kind  = kind(1), &
                     int8_kind  = selected_int_kind(18), &
                     log_kind  = kind(.true.), &
                     real_kind = selected_real_kind(6), &
                     dbl_kind  = selected_real_kind(13) 

END MODULE mod_kinds


