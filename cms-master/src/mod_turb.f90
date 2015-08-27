!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : mod_turb.f90                                                      *
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

MODULE mod_turb

USE mod_kinds
USE mod_random
USE constants

IMPLICIT NONE

CONTAINS

!Calculates random component to reproduce turbulent diffusion.
SUBROUTINE calc_turb(timestep,horDiff,vertDiff,uturb,vturb, wturb)
  
 real (KIND = real_KIND), intent(IN) :: &
     timestep
 real (KIND = real_KIND), intent(IN) :: &  
     horDiff, & !horizontal diffusivity 
     vertDiff   !vertical diffusivity 
 real (KIND = real_KIND), intent(OUT) :: &
     uturb, vturb, wturb

 real(KIND = real_KIND) :: &
     rn,htvelscl, vtvelscl
   
 htvelscl=sqrt((2.*horDiff)/timestep)  
 vtvelscl=sqrt((2.*vertDiff)/timestep)  

!Random component to add to U-velocity
 call random_gaussian (0.,1.,rn) 
 uturb = htvelscl*rn
!Random component to add to V-velocity
 call random_gaussian (0.,1.,rn)
 vturb = htvelscl*rn
!Random component to add to W-velocity
 call random_gaussian (0.,1.,rn)
 wturb = vtvelscl*rn

END SUBROUTINE calc_turb

END MODULE mod_turb
