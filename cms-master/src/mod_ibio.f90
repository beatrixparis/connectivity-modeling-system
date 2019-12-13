!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : mod_ibio.f90                                                      *
!* Last Modified: 2016-04-01                                                *
!* Code contributors: Claire B. Paris, Ana Carolina Vaz, Judith Helgers,    * 
!*                    Ashwanth Srinivasan                                   * 
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

MODULE mod_ibio

 USE mod_kinds
 USE constants
 USE mod_iounits

 IMPLICIT NONE

 real (kind = real_kind), allocatable, save  :: tprob(:,:), tprobnosum(:,:), vertLayers(:) 
 integer (kind=int_kind), save               :: vertX, vertY
 integer (kind=int_kind), allocatable, save  :: vertDays(:)
 logical (kind=log_kind), allocatable, save  :: eggTimePassed(:)!for mass spawning

 CONTAINS

!************************************************************** 
!load the data from the ibio file
SUBROUTINE load_ibio_data(ibioFileName)  
 character(len = *), intent(in) :: ibioFileName
 integer (kind=int_kind) :: i,j, iunit
 real (kind = real_kind) :: cum
 logical (kind=log_kind) :: file_exists

 !open file
 call get_unit(iunit)
 INQUIRE(FILE=trim(ibioFileName), EXIST=file_exists)
 IF (file_exists) THEN
   OPEN(UNIT=iunit,FILE=trim(ibioFileName),STATUS="old")
 ELSE
   print *, "Error: File ", trim(ibioFileName)," does not exist"
   stop
 ENDIF

 !read file
 read(iunit, *) vertX
 read(iunit, *) vertY
 allocate(vertLayers(vertY))
 read(iunit,*) (vertLayers(j), j=1, vertY)
 allocate(vertDays(vertX))
 read(iunit,*) (vertDays(i), i=1, vertX)
 allocate(tprob(vertX,vertY))  
 read(iunit,*) ((tprob(i,j), i = 1,vertX), j = 1, vertY)   
 allocate(tprobnosum(vertX,vertY))  

 !close file
 call release_unit(iunit)

 !calculate cumulative number of days
 DO i=2, vertX
   vertDays(i) = vertDays(i) + vertDays(i-1)
 ENDDO

 !calculate cumulative probabilities
 DO i=1,vertX
   cum =0 
   DO j=1,vertY
     tprobnosum(i,j) = tprob(i,j)/100
     cum = cum + (tprob(i,j)/100)
     tprob(i,j)= cum
   ENDDO
   IF (cum .gt. 1) THEN
     print '(A,I3,A)', "Error: probabilities in column ", j, " of vertical matrix exceed the 100%"
     stop
   ENDIF 
 ENDDO
 !print *, "Finished loading vertical matrix" 
END SUBROUTINE load_ibio_data

!**************************************************************
!calcultates a layer number, given a release depth in meters
SUBROUTINE set_layer(depth, layer)

 real (kind = real_kind), intent(IN)  :: depth
 integer (kind=int_kind), intent(OUT) :: layer
 integer (kind=int_kind)              :: y

 IF (depth .gt. vertLayers(vertY)) THEN
   print *, "Depth in release file is greater than depths in vert_matrix file"
   stop
 ELSE 
! loop over all layers 	 
  layer_loop: DO y=1, vertY
   IF (depth .le. vertLayers(y)) THEN
    layer = y
    exit layer_loop
   ENDIF
  ENDDO layer_loop
 ENDIF

END SUBROUTINE set_layer

!**************************************************************
!calculate new layer for particle
SUBROUTINE change_layer(run_time, massSpawning, larvaStart, layer, layerOld)
  
 use mod_random

 integer (kind=int8_kind),intent(in)  :: run_time
 logical (kind=log_kind), intent(in)  :: massSpawning
 integer (kind=int_kind),intent(in)  :: larvaStart, layerOld
 integer (kind=int_kind),intent(out)  :: layer

 integer (kind=int_kind)  :: x, y, numlayer, layerE, layerB, yaux
 integer (kind=int8_kind) :: time 
 real (kind = real_kind)  :: rn, cum, sumAux
 real (kind = real_kind), allocatable  :: probAux(:)
 
 !calculate which column to use in vert matrix
 x = 1
 IF (massSpawning) THEN
   time = run_time - (larvaStart*secs_in_day)
 ELSE
   time = run_time 
 ENDIF

 find_x: DO while (time .ge. (vertDays(x)))
   x = x + 1
   IF (x .gt. vertX) THEN
     !print *, "Warning: There are not enough columns in vertical matrix"
     x = vertX
     exit find_x 
   ENDIF
 ENDDO find_x
     
 !get layer with help of random number
 layer = layerOld
 
 call random_real(0.,1.,rn)
 layerB = layerOld - 1
 layerE = layerOld + 1
 IF (layerOld .eq. 1) THEN
   layerB = 1
 ENDIF
 IF (layerOld .eq. vertY) THEN
   layerE = vertY
 ENDIF
 numlayer = layerE - layerB + 1
 allocate(probAux(numlayer))
 sumAux = 0
 cum = 0
 DO y = layerB, layerE
   sumAux = sumAux + tprobnosum(x,y)
 ENDDO
 yaux = 1
 DO y = layerB, layerE
   cum = cum + tprobnosum(x,y)/sumAux
   probAux(yaux) = cum
   yaux = yaux + 1
 ENDDO
 ! if cumsum not 0, there are probabilities a layer up or down
 IF (cum .gt. 0) THEN
   yaux = 1
   layer_loop_short: DO y = layerB, layerE
     IF (rn .le. probAux(yaux)) THEN
       layer = y
       !print *, 'rn, probAux(y), layer', rn, probAux(yaux), layer
       exit layer_loop_short
     ENDIF
     yaux = yaux + 1
   ENDDO layer_loop_short
 ELSE
 ! if was 0, run over all the matrix
   layer_loop: DO y = 1, vertY
     IF (rn .le. tprob(x,y)) THEN
       layer = y
       !print *, 'rn, tprob(x,y), layer', rn, tprob(x,y), layer
       exit layer_loop
     ENDIF
   ENDDO layer_loop
 ENDIF
 deallocate(probAux)
END SUBROUTINE change_layer

!**************************************************************
! deallocate memory for ibio variables
SUBROUTINE dealloc_ibio (massSpawning)
 logical (kind=log_kind), intent(IN)  :: massSpawning
 
 deallocate(vertLayers)
 deallocate(vertDays)
 deallocate(tprob)
 IF (massSpawning) THEN
  deallocate(eggTimePassed)
 ENDIF     
 
END SUBROUTINE dealloc_ibio

!**************************************************************

END module mod_ibio

