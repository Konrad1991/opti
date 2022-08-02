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


module random_int

use stdlib_error

contains

integer function rand_int(lower, upper) result(res)
    implicit none
    real(8) :: rand_real(1)
    integer, intent(in) :: lower
    integer, intent(in) :: upper
    logical :: ch

    ! conduct some basic checks
    call check(upper > lower, msg = 'upper boundary is smaller or equal lower boundary')

    call random_number(rand_real) ! number between 0 and 1 --> here memory is leaked; Need another library!!!

    if(rand_real(1) >= 0.5) then
        ch = .TRUE.
    else
        ch = .FALSE.
    end if

    rand_real(1) = lower + (upper - lower)*rand_real(1)

    if(ch .eqv. .FALSE.) then
        res = floor(rand_real(1))
    else
        res = ceiling(rand_real(1))
    end if 
    
end function rand_int

integer function ri(lower, upper, curr) result(res)
    implicit none
    integer, intent(in) :: curr
    integer :: random_val
    integer, intent(in) :: lower
    integer, intent(in) :: upper
    
    res = curr

    do while(res == curr)
        res = rand_int(lower, upper)
    end do 

end function

end module random_int