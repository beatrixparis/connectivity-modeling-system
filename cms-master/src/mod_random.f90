!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : mod_random.f90                                                    *
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

MODULE mod_random

!This random number generator originally appeared in "Toward a Universal
!Random Number Generator" by George Marsaglia and Arif Zaman.
!Florida State University Report: FSU-SCRI-87-50 (1987)
!It was later modIFied by F. James and published in "A Review of Pseudo-
!random Number Generators"
	
USE mod_kinds
	
IMPLICIT NONE


real (kind = dbl_kind) , save :: uniform(97),c,cd,cm
integer (kind=int_kind), save :: i97,j97
logical (kind=log_kind), save :: test = .false.

CONTAINS

!**************************************************************
!This is the initialization routine for the random number generator.
!NOTE: The seed variables can have values between:    0 <= seed1 <= 31328
!                                                     0 <= seed2 <= 30081
SUBROUTINE random_initialize (pseed1, pseed2)
   integer (kind=int_kind), intent(IN) :: pseed1, pseed2
   real (kind = real_kind)             :: s,t
   integer (kind=int_kind)             :: ii,i,j,k,l,jj,m
   integer (kind=int_kind)             :: seed1, seed2

   seed1 = pseed1
   seed2 = pseed2

   IF (seed1 < 0 .or. seed1 > 31328 .or. seed2 < 0 .or. seed2 > 30081) THEN
      seed1 = 1802
      seed2 = 9373
   ENDIF

   i = mod((seed1 / 177),177) + 2
   j = mod(seed1, 177) + 2
   k = mod((seed2 / 169),178) + 1
   l = mod(seed2, 169)

   DO ii=1,97 
      s = 0.0
      t = 0.5
      DO jj=1, 24
         m = mod(mod(i * j,179) * k,179)
         i = j
         j = k
         k = m
         l = mod(53 * l + 1,169)
         IF (mod(l*m,64) .ge. 32) THEN
            s = s + t
         ENDIF
         t = t * 0.5
      ENDDO
      uniform(ii) = s
   ENDDO

   c    = 362436.0 / 16777216.0
   cd   = 7654321.0 / 16777216.0
   cm   = 16777213.0 / 16777216.0
   i97  = 97
   j97  = 33
   test = .true.
END SUBROUTINE

!**************************************************************

SUBROUTINE random_uniform (uni) 
   real (kind = dbl_kind), intent(OUT) :: uni
   
   uni = uniform(i97) - uniform(j97)
   IF (uni <= 0.0) THEN
      uni=uni+1
   ENDIF
   uniform(i97) = uni
   i97 = i97 - 1
   IF (i97 == 0) THEN
      i97 = 97
   ENDIF
   j97 = j97 - 1
   IF (j97 == 0) THEN
      j97 = 97
   ENDIF
   c = c - cd
   IF (c < 0.0) THEN
      c = c + cm
   ENDIF
   uni = uni - c
   IF (uni < 0.0) THEN
      uni=uni+1
   ENDIF

END SUBROUTINE

!**************************************************************
!ALGORITHM 712, COLLECTED ALGORITHMS FROM ACM.
!THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
!VOL. 18, NO. 4, DECEMBER, 1992, PP. 434-435.

!Returns random real number with a given mean and standard devaiation.
SUBROUTINE random_gaussian(mean, stddev, gaus) 

   real (kind = real_kind), intent(IN)  :: mean, stddev
   real (kind = real_kind), intent(OUT) :: gaus
   real (kind = dbl_kind)               :: q,u,v,x,y


   call random_uniform(u)
   call random_uniform(v)
   IF (u <= 0.0 .or. v <= 0.0) THEN
      u = 1.0
      v = 1.0
   ENDIF
   v = 1.7156 * (v - 0.5)
   x = u - 0.449871
   y = abs(v) + 0.386595
   q = x * x + y * (0.19600 * y - 0.25472 * x)

   DO while (((q > 0.27846) .or. (v * v > -4.0 * log(u) * u * u))  &
            .and. (q > 0.27597))
      call random_uniform(u)
      call random_uniform(v)
      IF (u <= 0.0 .or. v <= 0.0) THEN
         u = 1.0
         v = 1.0
      ENDIF
      v = 1.7156 * (v - 0.5)

      x = u - 0.449871
      y = abs(v) + 0.386595
      q = x * x + y * (0.19600 * y - 0.25472 * x)
   ENDDO   

    gaus = (mean + stddev * v / u)
END SUBROUTINE

!**************************************************************
!Returns random integer number
SUBROUTINE random_int (minrnd,maxrnd,rnd)
   integer (kind=int_kind), intent(IN)  :: minrnd, maxrnd
   integer (kind=int_kind), intent(OUT) :: rnd   
   real (kind = dbl_kind)               :: uni
   
   call random_uniform (uni)
   rnd = (uni * (maxrnd - minrnd + 1)) + minrnd
   
END SUBROUTINE

!**************************************************************
!Returns random real number
SUBROUTINE random_real (minrnd,maxrnd,rnd)
   real (kind = real_kind), intent(IN)  :: minrnd, maxrnd
   real (kind = real_kind), intent(OUT) :: rnd
   real (kind = dbl_kind)               :: uni
   
   call random_uniform (uni)
   rnd = (uni * (maxrnd - minrnd )) + minrnd
   
END SUBROUTINE

!**************************************************************

SUBROUTINE test_random
   integer (kind=int_kind) ::  i
   real (kind = dbl_kind)  :: r,rmin,rmax

   rmin=1e32
   rmax=-1e32
   call random_initialize(1802,9373)
   DO i=1,20000
      call random_uniform (r) 
      IF (r < rmin) rmin = r
      IF (r > rmax) rmax = r
   ENDDO
   print *, "Numbers range from", rmin," to ", rmax
   
!If the random number generator is working properly, 
!the next six ranDOm numbers should be:
!6533892.0  14220222.0  7275067.0 6172232.0  8354498.0   10633180.0

   DO i=1,6
      call random_uniform (r) 
      print *, r*4096.0 * 4096.0
   ENDDO
   
END SUBROUTINE

!**************************************************************

SUBROUTINE test_gaussian
   integer (kind=int_kind) :: i
   real (kind = real_kind) :: r, sum, sum2

      
!   call random_initialize(1802,9373)
   DO i=1, 100000
      call random_gaussian (1.0,0.5, r)
      sum = sum +  r
      sum2 = sum2 + (r*r)
   ENDDO

   print *, "Mean: ", sum/100000.
   print *, "Standard deviation: ", sqrt((sum2 - sum*sum/100000.)/100000.)
   
END SUBROUTINE

!**************************************************************

SUBROUTINE test_uniform_real
   integer (kind=int_kind) :: i, binnr, bins(1001)
   real (kind = real_kind) :: r, sum, sum2

   DO i=1, 1001      
     bins(i) = 0
   ENDDO
      
!   call random_initialize(1802,9373)
   DO i=1, 100000
      call random_real(0.,1.,r)
      sum = sum + r
      sum2 = sum2 + (r*r)
      binnr = (r*1000)+1
      bins(binnr) = bins(binnr) + 1
   ENDDO
   
   print *, "Mean: ", sum/100000.

   DO i=1,1001
!      print *, (i-1)/1000., bins(i)
   ENDDO

END SUBROUTINE

!**************************************************************


SUBROUTINE test_uniform_int
   integer (kind=int_kind) :: i, binnr, bins(101)
   integer (kind=int_kind) :: r, sum, sum2

   DO i=1, 101      
     bins(i) = 0
   ENDDO
      
!   call random_initialise_new(1802,9373)
   DO i=1, 100000
      call random_int(0,100,r)
      sum = sum + r
      sum2 = sum2 + (r*r)
      binnr = r + 1
      bins(binnr) = bins(binnr) + 1
   ENDDO
   
   print *, "Mean: ", sum/100000.

   DO i=1,101
      print *, i-1, bins(i)
   ENDDO

END SUBROUTINE

!**************************************************************
	
END MODULE mod_random


