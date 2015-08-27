!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : def_constants.f90                                                 *
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

MODULE constants

 USE mod_kinds

 IMPLICIT NONE

 save

 real (kind = dbl_kind), parameter :: &
     half   = 1./2., &
     quart  = 1./4., &
     sixth  = 1./6., &
     third =  1./3., &
     bignum = 1.e+20_dbl_kind, &
     tiny   = 1.e-14_dbl_kind, &
     pi     = 4.0 * atan(1.0), &
     pi2    = 2.*pi, &
     pih    = half*pi, &
     converge = 1.e-5, &
     TOLERANCE=1.e-14, & 
     deg2rad=PI/180.0, &
     rad2deg=180.0/PI, &
     REarth=6371.22, &
     REINV=1./6371220.

 integer (kind = int_kind), parameter :: &   
     secs_in_day  = 86400, &
     max_iter = 30, &
     max_nests = 50, &
     max_records=1e9, &
     numLinesBeforeFiles = 25, &
     bignum_int = 2147483647, &
     namesInOutput = 25 
 
END MODULE constants

