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

module testfct

    contains 
    function test_fct(inp, problem_size) result(out)
        !! This function defines a multidimensional rosenbrock function
        !! For problem_size = 3 Minimum at (1,1,1) 
        !! For 4 <= problem_size <= 7 local minima near (-1, 1, ...,1) 
        
        implicit none
        integer, intent(in) :: problem_size
        real(8), intent(in), dimension(problem_size) :: inp
        real(8) :: out
        integer :: i

        out = 0.0
        do i = 1, (problem_size -1)
            out = out + 100.0*(inp(i+1) - inp(i)**2)**2 + (1 - inp(i) )**2
        end do
    end function
    
end module