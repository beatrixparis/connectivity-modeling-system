!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : cms.f90                                                           *
!* Last Modified: 2011-07-22                                                *
!* Code contributors: Judith Helgers, Ashwanth Srinivasan, Claire B. Paris, * 
!*                    Ana Carolina Vaz                                      *
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

PROGRAM CMS

 USE MPI !remove if not using mpi
 USE globalvariables
 USE mod_random
 
 IMPLICIT NONE

 EXTERNAL directory
 
 character(char_len)     :: filenumber
 integer (kind=int_kind) :: ierr, my_id, npes, number1, number2
 logical (kind=log_kind) :: file_exists

 my_id = 0 !used if not using mpi
 npes = 1 !used if not using mpi

!initialise MPI
 CALL MPI_INIT(ierr) !remove if not using mpi
!what processor am I (what is my rank)?
 CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_id, ierr) !remove if not using mpi
!how many processors are there?
 CALL MPI_COMM_SIZE(MPI_COMM_WORLD, npes, ierr) !remove if not using mpi

!check which experiment to run
 IF (command_argument_count() .eq. 0) THEN
    print *, "You have to enter the experiment number/name you want to run"
    stop
 ENDIF
 CALL getarg(1,filenumber)

!initialise random seed for random number generator
 CALL SYSTEM_CLOCK(COUNT=number1)
 CALL SYSTEM_CLOCK(COUNT=number2)
 number1 = abs(mod((number1*(my_id+1)),31328))
 number2 = abs(mod((number2*(my_id+1)),30081))
 CALL random_initialize (number1,number2)
 
!check whether input runconf file exists 
 write(fileinput,'(A,A,A)') 'input_',trim(filenumber), '/'
 INQUIRE(FILE=trim(fileinput)//'runconf.list',EXIST=file_exists)
 IF (file_exists .eqv. .false.) THEN
  print *, "Error: File ", trim(fileinput)//'runconf.list'," does not exist"
  stop
 ENDIF

!create directories
 write(filedir,'(A,A)') 'expt_',trim(filenumber)
 CALL make_dir (adjustl(trim(filedir)),Len(adjustl(trim(filedir))))
 write(filenest,'(A,A,A)') 'expt_',trim(filenumber),'/nests/'
 CALL make_dir(adjustl(trim(filenest)),Len(adjustl(trim(filenest))))
 write(fileoutput,'(A,A,A)') 'expt_',trim(filenumber),'/output/'
 CALL make_dir(adjustl(trim(fileoutput)),Len(adjustl(trim(fileoutput))))
 write(filescratch,'(A,A,A)') 'expt_',trim(filenumber),'/SCRATCH/'
 CALL make_dir(adjustl(trim(filescratch)),Len(adjustl(trim(filescratch))))

!load input files
 CALL load_ibm
 CALL load_runconf
 CALL load_mod_input
 CALL load_release_info

!move the particles
 CALL loop(my_id, npes)

!finish up
 CALL dealloc_all 

!quit MPI
 CALL MPI_FINALIZE(ierr) !remove if not using mpi
     
END PROGRAM CMS

