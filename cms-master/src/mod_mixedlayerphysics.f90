!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : mod_mixedlayerphysics.f90                                         *
!* Last Modified: 2013-06-04                                                *
!* Code contributors: Judith Helgers, Erik van Sebille, Claire B. Paris     *
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
!*                                                                          *
!* This code is part of the publication                                     *
!* Van Sebille, Spence, Mazloff, England, Rintoul and Saenko (2013), Abyssal* 
!*  connections of Antarctic Bottom Water in a Southern Ocean State         *
!*  Estimate, Geophysical Research Letters, 40, doi:10.1002/grl.50483       *
!****************************************************************************

MODULE mod_mixedlayerphysics

USE globalvariables
USE mod_kinds
USE mod_random
USE constants

IMPLICIT NONE

CONTAINS

!Calculates random vertical displacement when particle is in mixed layer.
SUBROUTINE randdepthinmixedlayer(ngrid,lon,lat,depth,r,n)
  real (kind = real_kind), intent(in)  :: lon, lat
  integer (kind=int_kind), intent(in)  :: ngrid

  real (kind = real_kind) :: grid_i(QAx), grid_j(QAx), grid_k(QAx), &
                             depth, mld, maxdepthchange, mindepth, maxdepth
  integer (kind = int_kind) :: r, n
  logical (kind=log_kind) :: fail

  CALL lonlat2ij (lat,lon, depth,ngrid, size(nests(ngrid)%lon(1,:,:)), size(nests(ngrid)%lat(1,:,:)),&
   size(nests(ngrid)%depth(1,:)), periodicbc,r,n,grid_i,grid_j,grid_k)
  mld = min(nests(ngrid)%mld(floor(grid_i(WAx)),floor(grid_j(WAx)),1),&
            nests(ngrid)%mld(floor(grid_i(WAx)),floor(grid_j(WAx)+1),1),&
            nests(ngrid)%mld(floor(grid_i(WAx)+1),floor(grid_j(WAx)),1),&
            nests(ngrid)%mld(floor(grid_i(WAx)+1),floor(grid_j(WAx)+1),1))

  IF (depth.lt.mld) THEN

   particle(r)%inmld(n)=.true.

   maxdepthchange = mixedlayerwmax * timestep

   mindepth = depth - maxdepthchange
   IF (mindepth.lt.nests(ngrid)%depth(UAx,1)) THEN
    mindepth = nests(ngrid)%depth(UAx,1)
   ENDIF

   maxdepth = depth + maxdepthchange
   IF (maxdepth.gt.mld) THEN
    maxdepth = mld
   ENDIF

   CALL random_real(mindepth, maxdepth, depth)

  ELSE
   particle(r)%inmld(n)=.false.
  ENDIF
END SUBROUTINE randdepthinmixedlayer

END MODULE mod_mixedlayerphysics
