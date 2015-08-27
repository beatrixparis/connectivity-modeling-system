!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : def_globalvariables.f90                                           *
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

MODULE globalvariables

 USE mod_kinds  
 USE def_particle
 USE def_nests
 USE constants

 IMPLICIT NONE
 
!the nests
 type (Grid), allocatable      :: nests(:)
 integer (kind=int_kind)       :: axorderT, totsnapshotsinfile
 real (kind = real_kind)       :: velocity_conversion_factor, depth_conversion_factor

!the particles
 type (tparticle), allocatable :: particle(:)

!directories
 character (char_len) :: filedir, filenest, fileoutput,fileinput, filescratch

!outputfiles
 integer (kind=int_kind)       :: iunit_traj, iunit_con, iunit_restart
 character(char_len)           :: trajname, conname, restartname
 integer (kind=int8_kind)      :: saveTime

!runconf.list + ibm.list
 integer (kind=int_kind)       :: nnests, timeStep, outputFreq, ibioTimestep, turbTimestep, &
                                  settlementStart, larvaStart,tstStart, orientStart, &
                                  loopfilesstartyear, loopfilesstartmonth, loopfilesstartday, &
                                  loopfilesendyear, loopfilesendmonth, loopfilesendday, restartwritefreq
 integer (kind=int8_kind)      :: timeMax, total_seconds, UAx, VAx, WAx, QAx 
 character(char_len)           :: releaseFilename,polyFilename,tideFilename, &
                                  ibioFilename,diffpartFilename, fractionFilename, wvel_positive_direction, &
                                  strataFilename, buoyancyFilename
 logical (kind=log_kind)       :: polygon, buoyancy, ibio,turb, mort, avoidcoast, diffpart, periodicbc, &
                                  massSpawning, tidalMovement, backward, ascii, upperlevelsurface=.true., &
                                  loopfiles, restartfromfile=.false., mixedlayerphysics=.false., &
                                  agrid=.true., withibm, AxUsed(4), notmove, strata ! needed for cktidalmovement
 real (kind = real_kind)       :: dens_particle, diam_particle, horDiff(max_nests)=-1.,vertDiff(max_nests)=-1., &
                                  horDiffOrient, halflife, maxDistance,orientAbility,swimmingSpeedHatch, &
                                  swimmingSpeedSettle, mixedlayerwmax
 logical (kind=log_kind)       :: nextFile(4)

!inputfiles
 integer (kind=int_kind)       :: snapshotvec(4)
 logical (kind=log_kind)       :: outputtemp=.false., outputsaln=.false.

!other
 integer (kind=int_kind)       :: interpversion=1 !version of interpolation 
                                  !(1=tricubic; 2=new trilinear; 3=nonorthogonal)

END MODULE globalvariables

