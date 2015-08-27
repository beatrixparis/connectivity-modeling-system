!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : def_nests.f90                                                     *
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


MODULE def_nests
 USE mod_kinds         

 IMPLICIT NONE

 TYPE grid

  integer (kind=int_kind)             :: tdm,time_step,tstart_yy, tstart_mm, tstart_dd, &
                                         tend_yy, tend_mm, tend_dd
  integer (kind=int_kind),allocatable :: mask(:,:), idm(:), jdm(:), kdm(:)
  logical (kind=log_kind)             :: dataExist, tilted, orthogrid
  real (kind = real_kind)             :: fill_value
  real (kind = real_kind),allocatable :: w(:,:), lon(:,:,:), lat(:,:,:), depth(:,:), uvel(:,:,:,:), vvel(:,:,:,:), &
                                         wvel(:,:,:,:), temp(:,:,:,:), saln(:,:,:,:), dens(:,:,:,:), ssh(:,:,:), &
                                         mld(:,:,:), angle(:,:,:)
  character(char_len)                 :: fnameold, time_units, uname, vname, wname, &
                                         densname, salnname, tempname

 END TYPE grid

END MODULE def_nests
