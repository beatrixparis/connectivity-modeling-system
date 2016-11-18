
!****************************************************************************
!* System: Connectivity Modeling System (CMS)                               *
!* File : findnest.f90                                                      *
!* Last Modified: 2016-09-14                                                *
!* Code contributors: Claire B. Paris, Ana Carolina Vaz, Judith Helgers,    * 
!*                    Ashwanth Srinivasan                                   *                                !*                                                                          *
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

!calculates in which nest the particle is
!if a particle is in 0 nests and ckperiodicbc=false then ngrid = -1
!if a particle is in 1 nest then ngrid = the number of that nest
!if a particle is in more than 1 nest then ngrid = the nest with the smallest area
!if a particle is in 0 nests and ckperiodicbc=true then ngrid = the nest with the largest area
subroutine findnest(lon,lat,depth,ngrid,r,n)

 use globalvariables
 use mod_kinds

 implicit none

 real (kind = real_kind),intent(in)  :: lon,lat,depth
 integer (kind=int_kind),intent(out) :: ngrid

 integer (kind=int_kind)   :: & 
    l,l1,l2,i,r,n, &
    nvertices, &!number of points that define a nest
    largestNest,& !number of the nest with the largest area
    gridi, gridj, ax
 real (kind = real_kind)   :: &
    wrk_lon(5),wrk_lon1(5),wrk_lon2(5),wrk_lat(5), &
    sizeNest, sizeNestOld, & !size of the nest with smallest area 
    sizeLargest, & !size of the nest with largest area
    xstart, xend, ystart, yend,zstart,zend, &
    grid_i(4), grid_j(4), grid_k(4) 
 logical (kind=log_kind)   :: twoParts, checkSurrounding


 sizeNestOld = 550
 sizeLargest = 0
 ngrid = -1
 nvertices = 5

 do i=1,nnests
   xstart = nests(i)%lon(1,1,1)
   ystart = nests(i)%lat(1,1,1)
   zstart = nests(i)%depth(1,1)
   zend = nests(i)%depth(1,nests(i)%kdm(1)) 
   if (nests(i)%orthogrid) then
     xend = nests(i)%lon(1,nests(i)%idm(1),1)
     yend = nests(i)%lat(1,nests(i)%jdm(1),1)
   else
     xend = nests(i)%lon(1,nests(i)%idm(1),nests(i)%jdm(1))
     yend = nests(i)%lat(1,nests(i)%idm(1),nests(i)%jdm(1))
   endif
   do ax=2,QAx
     if (axUsed(ax) .eqv. .true. ) then
       xstart = max(xstart,nests(i)%lon(ax,1,1))
       ystart = max(ystart,nests(i)%lat(ax,1,1))
       zstart = max(zstart,nests(i)%depth(ax,1))
       zend = min(zend,nests(i)%depth(ax,nests(i)%kdm(1))) 
       if (nests(i)%orthogrid) then
         xend = min(xend,nests(i)%lon(ax,nests(i)%idm(ax),1))
         yend = min(yend,nests(i)%lat(ax,nests(i)%jdm(ax),1))
       else
         xend = min(xend,nests(i)%lon(ax,nests(i)%idm(ax),nests(i)%jdm(ax)))
         yend = min(yend,nests(i)%lat(ax,nests(i)%idm(ax),nests(i)%jdm(ax)))
       endif
     endif
   enddo

   if (xstart > xend) then
      wrk_lon1(1)=xstart
      wrk_lon1(2)=360
      wrk_lon1(3)=360
      wrk_lon1(4)=xstart
      wrk_lon1(5)=wrk_lon1(1)
      wrk_lon2(1)=0
      wrk_lon2(2)=xend
      wrk_lon2(3)=xend
      wrk_lon2(4)=0
      wrk_lon2(5)=wrk_lon2(1)
      twoParts = .true.
   else
     wrk_lon(1)=xstart
     wrk_lon(2)=xend
     wrk_lon(3)=xend
     wrk_lon(4)=xstart
     wrk_lon(5)=wrk_lon(1)
     twoParts = .false.
   endif
   wrk_lat(1)=ystart
   wrk_lat(2)=ystart
   wrk_lat(3)=yend
   wrk_lat(4)=yend
   wrk_lat(5)=wrk_lat(1)

!  calculate i,j,k position of particle
   call lonlat2ij (lat,lon, depth, i, size(nests(i)%lon(1,:,:)), size(nests(i)%lat(1,:,:)),&
   size(nests(i)%depth(1,:)),periodicbc,r,n,grid_i,grid_j,grid_k)
   
   gridi = int(grid_i(1))
   gridj = int(grid_j(1))
   checkSurrounding = .false.
   if ((gridi .gt. 2) .and. (gridj .gt. 2) .and. (gridi .lt. (nests(i)%idm(1)-2)) .and. (gridj .lt. (nests(i)%jdm(1)-2))) then
     checkSurrounding = .true.
   endif
   
   if (twoParts) then
!    grid exists of 2 parts
!    get size of nest
     sizeNest = 360 - xstart + xend + abs(ystart - yend)
     if (sizeNest .gt. sizeLargest) then
       sizeLargest = sizeNest
       largestNest = i
     endif
!    check if lon and lat are inside the nest
     call pip (lon,lat, wrk_lon1, wrk_lat, nvertices, l1)
     call pip (lon,lat, wrk_lon2, wrk_lat, nvertices, l2)
     if (nests(i)%tilted .eqv. .false.) then 
!      if nest is not tilted
       if (((l1 >= 0) .OR. (l2>=0)) .and. (sizeNest < sizeNestOld)) then
         if (depth >= zstart .and. depth<=zend) then 
           if (nests(i)%dataExist) then
             sizeNestOld = sizeNest
             ngrid=i
           endif
         endif
       endif
     else
!      if nest is is tilted
       if (((l1 >= 0) .OR. (l2>=0)) .and. (sizeNest < sizeNestOld) .and. (checkSurrounding .eqv. .true.)) then
         if  ((nests(i)%mask(gridi-1,gridj-1) .eq. 1).and. &
          (nests(i)%mask(gridi-1,gridj+2) .eq. 1).and. &
          (nests(i)%mask(gridi+2,gridj-1) .eq. 1).and. &
          (nests(i)%mask(gridi+2,gridj+2) .eq. 1)) then
            if ( depth >= zstart .and. depth<=zend) then 
              if (nests(i)%dataExist) then
                sizeNestOld = sizeNest
                ngrid=i
              endif
            endif
          endif
        endif    
      endif
    else  
!     grid exists of 1 part
!     get size of nest
      sizeNest = abs(xstart - xend) + abs(ystart - yend)
      if (sizeNest .gt. sizeLargest) then
        sizeLargest = sizeNest
        largestNest = i
      endif
!     check if lon and lat are inside the nest
      call pip (lon, lat, wrk_lon, wrk_lat, nvertices, l)
      if (nests(i)%tilted .eqv. .false.) then 
!     if nest is not tilted
        if ((l >= 0) .and. (sizeNest < sizeNestOld)) then
          if (depth >= zstart .and. depth<=zend) then 
            if (nests(i)%dataExist) then
              sizeNestOld = sizeNest
              ngrid=i
            endif
          endif
        endif
      else
!       if nest is is tilted
        if ( (l >= 0) .and. (sizeNest < sizeNestOld) .and. (checkSurrounding .eqv. .true.) ) then
          if ( (nests(i)%mask(gridi-1,gridj-1) .eq. 1).and. &
            (nests(i)%mask(gridi-1,gridj+2) .eq. 1).and. &
            (nests(i)%mask(gridi+2,gridj-1) .eq. 1).and. &
            (nests(i)%mask(gridi+2,gridj+2) .eq. 1)) then
            if ( depth >= zstart .and. depth<=zend) then 
              if (nests(i)%dataExist) then
                sizeNestOld = sizeNest
                ngrid=i
              endif
            endif
          endif
        endif  
      endif
    endif ! end if:twoParts 
 
 enddo !end loop: i=1, nnests
     
 if ((ngrid .eq. -1) .and. (periodicbc) .and. (lat .ge. ystart) .and. (lat .le. yend)) then
   zstart = nests(largestNest)%depth(1,1)
   zend = nests(largestNest)%depth(1,nests(largestNest)%kdm(1))  
   if (depth >= zstart .and. depth<=zend) then 
     ngrid = largestNest
   endif
 endif
      
end subroutine findnest