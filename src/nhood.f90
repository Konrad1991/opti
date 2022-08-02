! Particle Swarm Optimization (PSO)
! Copyright (C) 2021 Konrad Krämer

! This file is part of pso


!pso is free software; you can redistribute it and/or
!modify it under the terms of the GNU General Public License
!as published by the Free Software Foundation; either version 2
!of the License, or (at your option) any later version.

!This program is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!GNU General Public License for more details.

!You should have received a copy of the GNU General Public License along with pso
! If not see: https://www.gnu.org/licenses/old-licenses/gpl-2.0.html#SEC4


module nhood

use stdlib_error

implicit none

public

! neighberhood for one particle
type :: nb
    logical :: allocated
    integer :: size
    integer, allocatable, dimension(:) :: neighbours
end type nb

contains

function init(size) result(nbhood)
    use random_int, only: rand_int, ri

    implicit none

    integer, intent(in) :: size ! size of swarm
    type(nb), allocatable, dimension(:) :: nbhood
    integer :: K, Klb, Kub, i, j

    call check(size >= 3, msg = 'The swarm should consists of at least 3 particles')

    Klb = 0 ! no neighbours
    Kub = 4
    allocate(nbhood(size))
    
    do i = 1, size 
        K = rand_int(Klb, Kub)
        if(K == 0) then
            allocate(nbhood(i)%neighbours(1))
            nbhood(i)%size = 1 
            nbhood(i)%neighbours(1) = i ! i is itself
            nbhood(i)%allocated = .TRUE.
        else
            allocate(nbhood(i)%neighbours(K))
            nbhood(i)%size = K
            nbhood(i)%neighbours(1) = i ! i is itself    
            nbhood(i)%allocated = .TRUE.
            do j = 2, K
                nbhood(i)%neighbours(j) = ri(1, size, i) !draw neighbours
            end do
        end if    
    end do
end function init

subroutine print_nbhood(size, nbhood)
    implicit none

    integer:: i, size
    type(nb), dimension(size) :: nbhood
    
    do i = 1, size
        print*, nbhood(i)%neighbours
    end do

end subroutine


subroutine recalc(size, nbhood)
    use random_int, only: rand_int, ri

    implicit none
    integer :: i, j, size
    integer :: K, Klb, Kub

    type(nb), dimension(size) :: nbhood

    Klb = 0 ! no neighbours
    Kub = 4

    do i = 1, size 
        K = rand_int(Klb, Kub)
        if(K == 0) then

            if(nbhood(i)%allocated .eqv..TRUE.) then
                deallocate(nbhood(i)%neighbours)
            end if 
            
            allocate(nbhood(i)%neighbours(1))
            nbhood(i)%size = 1 
            nbhood(i)%neighbours(1) = i ! i is itself
        else
            if(nbhood(i)%allocated .eqv..TRUE.) then
                deallocate(nbhood(i)%neighbours)
            end if 
            
            allocate(nbhood(i)%neighbours(K))
            nbhood(i)%size = K
            nbhood(i)%neighbours(1) = i ! i is itself    
            do j = 2, K
                nbhood(i)%neighbours(j) = ri(1, size, i) !draw neighbours
            end do
        end if    
    end do


end subroutine

subroutine delete_nbhood(size, nbhood)
    implicit none

    integer:: i, size
    type(nb), allocatable, dimension(:) :: nbhood
    
    do i = 1, size
        deallocate(nbhood(i)%neighbours)
    end do

    deallocate(nbhood)

end subroutine


end module nhood