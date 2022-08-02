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



module swarm

!SwarmStruct contains information about the swarm such as:
! velocities, the particles, personal bests, global bests parameter and errors
type swarm_struct
    real(8), allocatable, dimension(:,:) :: S
    real(8), allocatable, dimension(:,:) :: velocities
    real(8), allocatable, dimension(:) :: best_errors
    real(8), allocatable, dimension(:) :: current_errors
    integer :: best_particle
    real(8), allocatable, dimension(:) :: parameter_of_best_particle
    real(8), allocatable, dimension(:,:) :: personal_best_parameters
    real(8) :: global_best_error
    integer :: n_swarm !! number of rows of swarm & velocities
    integer :: n_params !! number of cols of swarm & velocities
    procedure(func), pointer, nopass :: userfct 
end type swarm_struct

! Definition of the signature of the user fct --> Is it possible to define sth like a void*?
abstract interface
    function func (inp, problem_size) result(out)
      implicit none
      integer, intent(in) :: problem_size
      real(8), intent(in), dimension(problem_size) :: inp
      real(8) :: out
    end function func
end interface

! functions
contains

! print fct
subroutine print_swarm(inp)
    implicit none
    integer :: i
    type(swarm_struct) :: inp

    do i = 1, inp%n_swarm
      print*, inp%S(i, :)
    end do

end subroutine

! Calculates error of initial particles and store the results
subroutine calculate_errors_init(inp)
    implicit none
    type(swarm_struct) :: inp
    integer :: i
    do i = 1, inp%n_swarm
        inp%best_errors(i) = inp%userfct(inp%S(i, :), inp%n_params)
        inp%current_errors(i) = inp%best_errors(i)
    end do

end subroutine

! Initialisation of particles and call of subroutine calculate_errors_init
function initialise(n_swarm, n_params, lb, ub, fct) result(inp_struct)
    type(swarm_struct) :: inp_struct
    integer :: n_swarm, n_params
    real(8), dimension(n_params) :: lb, ub
    real(8), allocatable, dimension(:) :: temp
    integer :: i
    integer, dimension(1) :: tp

    interface
        function fct (inp, problem_size) result(out)
            implicit none
            integer, intent(in) :: problem_size
            real(8), intent(in), dimension(problem_size) :: inp
            real(8) :: out
        end function fct
    end interface

    ! allocation
    inp_struct%n_swarm = n_swarm
    inp_struct%n_params = n_params
    allocate(inp_struct%S(n_swarm, n_params))
    allocate(inp_struct%velocities(n_swarm, n_params))
    allocate(inp_struct%personal_best_parameters(n_swarm, n_params))
    allocate(inp_struct%best_errors(n_swarm))
    allocate(inp_struct%current_errors(n_swarm))
    allocate(inp_struct%parameter_of_best_particle(n_params))

    allocate(temp(n_params))
    inp_struct%best_errors = 0.0
    inp_struct%current_errors = 0.0
    inp_struct%parameter_of_best_particle = 0.0
    inp_struct%velocities = 0.0

    inp_struct%userfct => fct

    !Initialisation
    do i = 1, n_swarm
        call random_number(temp)
        inp_struct%S(i, :) = lb + (ub - lb)*temp
        inp_struct%personal_best_parameters(i, :) = inp_struct%S(i, :)
    end do

    call calculate_errors_init(inp_struct)

    ! store of global best particle
    tp  = minloc(inp_struct%best_errors)
    inp_struct%best_particle = tp(1)
    inp_struct%global_best_error = inp_struct%best_errors(inp_struct%best_particle)

    deallocate(temp)

end function

subroutine delete_swarm(inp)
    implicit none
    type(swarm_struct) :: inp
    deallocate(inp%S)
    deallocate(inp%velocities)
    deallocate(inp%personal_best_parameters)
    deallocate(inp%best_errors)
    deallocate(inp%current_errors)
    deallocate(inp%parameter_of_best_particle)

end subroutine delete_swarm

end module swarm