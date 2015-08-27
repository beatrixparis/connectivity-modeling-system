!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : def_particle.f90                                                  *
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

MODULE def_particle
 USE mod_kinds         

 IMPLICIT NONE

!define particle type

 TYPE tparticle

  integer (kind=int_kind)              :: id, num_rel, seconds, day, month, year
  integer (kind=int8_kind)             :: start
  real (kind = real_kind)              :: ilon, ilat, idepth
  character(char_len)                  :: rel_loc_name
  real (kind = real_kind), allocatable :: dist(:), nlon(:), nlat(:), ndepth(:), &
                                          old_lonDist(:), old_latDist(:), &
                                          diam(:), first_diam(:), density(:), halflife(:) 
  logical (kind=log_kind), allocatable :: move(:), flag(:,:)
  integer (kind=int_kind), allocatable :: layer(:)

 END TYPE tparticle

END MODULE def_particle

