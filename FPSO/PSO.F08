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


! Module Init initialisation and first error calculation 
! definition the type SwarmStruct 
! definition the objective function signature 
module Init

    !SwarmStruct contains information about the swarm such as:
    ! velocities, the particles, personal bests, global bests parameter and errors
    type SwarmStruct
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
end type SwarmStruct

! Definition of the signature of the user fct --> Is it possible to define sth like a void*?
abstract interface
    function func (inp, problem_size) result(out)
      implicit none
      integer, intent(in) :: problem_size
      real(8), intent(in), dimension(problem_size) :: inp
      real(8) :: out
    end function func
end interface

contains

! Calculates error of initial particles and store the results
subroutine calculate_errors_init(inp)
    implicit none
    type(SwarmStruct) :: inp
    integer :: i
    do i = 1, inp%n_swarm
        inp%best_errors(i) = inp%userfct(inp%S(i, :), inp%n_params)
        inp%current_errors(i) = inp%best_errors(i)
    end do

end subroutine

! Initialisation of particles and call of subroutine calculate_errors_init
subroutine init_fct(inp_struct, n_swarm, n_params, lb, ub, fct)
    type(SwarmStruct) :: inp_struct
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

end subroutine

end module Init

! module neighberhood calculates the neighberhood for the entire swarm
module neighberhood
    implicit none

    ! neighbours defines one neighberhood for a specific particle
    type neighbours
        integer, allocatable, dimension(:) :: neigh
    end type neighbours
    
    ! defines the entire neighberhood of the swarm. 
    ! Each element of N contains the neighberhood of the specific particle
    type neighbor
        integer :: number_particles
        type(neighbours), allocatable, dimension(:) :: N
        integer, allocatable, dimension(:) :: K
    end type neighbor
contains

! resizes the neighberhood
subroutine resize(NE) 
    implicit none
    type(neighbor), intent(inout) :: NE
    allocate(NE%N(NE%number_particles))
    allocate(NE%K(NE%number_particles))
end subroutine

! calculate random integers
subroutine generate_random_int(upper, num, res, position)
    implicit none
    real(8) :: rand_real
    integer, intent(in) :: num
    integer, intent(inout), dimension(num) :: res
    integer, intent(in) :: upper
    integer :: i
    integer :: position

    res(1) = position

    do i = 2, num
        call random_number(rand_real)
        res(i) = floor(upper*rand_real)
    end do
end subroutine

! calculation of the neighberhood
subroutine calc_neighbours(NE, number_particles)
    implicit none
    type(neighbor), intent(inout) :: NE
    integer, intent(in) :: number_particles
    integer :: K_Lower
    integer :: K_upper
    real :: temp1
    real :: temp2
    integer :: i, j
    type(neighbours) :: temp3 !! 0 neighbours
    type(neighbours) :: temp4 !! 1 neighbour
    type(neighbours) :: temp5 !! 2 neighbours
    type(neighbours) :: temp6 !! 3 neighbours
    integer, allocatable, dimension(:) :: temp_neigh

    NE%number_particles = number_particles
    call resize(NE)
    K_Lower = 0
    K_upper = 3
    do i = 1, number_particles
        call random_number(temp1)
        NE%K(i) = floor(4*temp1)

        allocate(temp3%neigh(1))
        allocate(temp4%neigh(2))
        allocate(temp5%neigh(3))
        allocate(temp6%neigh(4))

        temp3%neigh(1) = i

        if (NE%K(i) == 0) then
            NE%N(i) = temp3
        else if(NE%K(i) == 1) then 
            allocate(temp_neigh(2))
            call generate_random_int(number_particles, 2, temp_neigh, i)
            temp4%neigh = temp_neigh
            NE%N(i) = temp4
            deallocate(temp_neigh)
        else if(NE%K(i) == 2) then
            allocate(temp_neigh(3))
            call generate_random_int(number_particles, 3, temp_neigh, i)
            temp5%neigh = temp_neigh
            NE%N(i) = temp5
            deallocate(temp_neigh)
        else if(NE%K(i) == 3) then
            allocate(temp_neigh(4))
            call generate_random_int(number_particles, 4, temp_neigh, i)
            temp6%neigh = temp_neigh
            NE%N(i) = temp6
            deallocate(temp_neigh)
        end if

        deallocate(temp3%neigh)
        deallocate(temp4%neigh)
        deallocate(temp5%neigh)
        deallocate(temp6%neigh)

    end do 
end subroutine

end module neighberhood

! Contains parameter which are important during optimization
module parameters
    implicit none
    real(8) :: cog
    real(8) :: soc
    real(8) :: initial_cog = 2.5
    real(8) :: initial_soc = 0.5
    real(8) :: final_cog = 0.5
    real(8) :: final_soc = 2.5
    real(8) :: par_w = 0.5
    real(8) :: par_w_max = 0.9
    real(8) :: par_w_min = 0.4
end module 

! Main module of the particle swarm optimization (PSO)
module psomod 

use init
use parameters
use neighberhood
use parameters

contains

! subroutine which evaluates particle
subroutine calculate_errors(inp)
    implicit none
    type(SwarmStruct) :: inp
    integer :: i
    do i = 1, inp%n_swarm
        inp%current_errors(i) = inp%userfct(inp%S(i, :), inp%n_params)
    end do
end subroutine

! checks if particle are above or below boundary. 
! If boundaries are violated the particle is set on the boundary
subroutine check_boundaries(inp, n_params, lb, ub, index)
    implicit none
    type(swarmStruct), intent(inout):: inp
    integer :: n_params
    real(8), dimension(n_params) :: lb, ub
    integer :: i, index

    do i = 1, n_params
        if (inp%S(index, i) > ub(i)) then
            inp%S(index, i) = ub(i)
        else if (inp%S(index, i) < lb(i)) then
            inp%S(index, i) = lb(i)
        end if
    end do
end subroutine

! The actual optimizer (PSO)
subroutine optimizer(n_swarm, n_generations, n_params,  lb, ub, desired_error, fct, result)
    implicit none
    type(SwarmStruct) :: struct
    type(neighbor) :: hood
    integer :: n_swarm
    integer :: n_params
    real(8), dimension(n_params) :: lb, ub
    real(8), allocatable, dimension(:) :: temp
    type(neighbours) :: hood_numbers
    integer :: i, j, k, l
    integer :: n_generations
    integer :: best_neighberhood_particle
    real(8), intent(in) :: desired_error
    logical :: checker
    logical :: convergence
    real(8), allocatable, dimension(:) :: errors_hood
    real(8), dimension(n_params) :: local_best_parameters
    real(8), dimension(n_params) :: rand1
    real(8), dimension(n_params) :: rand2
    real(8), dimension(n_params) :: cog_vector
    real(8), dimension(n_params) :: soc_vector
    integer, dimension(1) :: min_position
    real(8) :: min_position_error
    integer :: convergence_counter
    integer :: position_local_best

    real(8), intent(inout), dimension(n_params) :: result
    
    interface
        function fct (inp, problem_size) result(out)
            implicit none
            integer, intent(in) :: problem_size
            real(8), intent(in), dimension(problem_size) :: inp
            real(8) :: out
        end function fct
    end interface

    ! Initialisation
    call init_fct(struct, n_swarm, n_params, lb, ub, fct)
    struct%best_particle = minloc(struct%best_errors, 1)
    struct%parameter_of_best_particle = struct%S(struct%best_particle, :)

    checker = .TRUE.
    convergence = .FALSE.
    i = 0
    allocate(temp(n_params))

    ! Start generation loop
    do while ( (checker .eqv. .TRUE.) .and. (i < n_generations))

        ! calculation of neighberhood if needed
        if (i == 0) then
            call calc_neighbours(hood, n_swarm)
        else if (convergence .eqv. .TRUE.) then
            deallocate(hood%N)
            deallocate(hood%K)
            call calc_neighbours(hood, n_swarm)
            convergence = .FALSE.
        end if

        ! update par_w, cog and soc
        par_w = par_w_max - i*(par_w_max - par_w_min)/n_generations

        cog = initial_cog - (initial_cog - final_cog) * (i + 1) / n_generations
        soc = initial_soc - (initial_soc - final_soc) * (i + 1) / n_generations

        ! Start population loop
        do j = 1, n_swarm 

            ! get the best particle of neigherhood j
            ! =================================================
            allocate(hood_numbers%neigh(hood%K(j)))
            allocate(errors_hood(hood%K(j)))
            hood_numbers%neigh = hood%N(j)%neigh

            do k = 1, hood%K(j)
                errors_hood(k) = struct%best_errors(hood_numbers%neigh(k))
            end do

            min_position = minloc(errors_hood)

            position_local_best = hood_numbers%neigh(min_position(1))

            local_best_parameters = struct%S(position_local_best, :)
            ! =================================================

            ! Update velocities and particle
            ! =================================================
            call random_number(rand1)
            call random_number(rand2)

            struct%velocities(j, :) = par_w*struct%velocities(j,:)
            rand1 = rand1*cog
            rand2 = rand2*soc

            do l = 1, n_params
                cog_vector(l) = struct%personal_best_parameters(j,l) - struct%S(j, l)
                soc_vector(l) = local_best_parameters(l) - struct%S(j, l)
            end do
            
            do l = 1, n_params
                cog_vector(l) = rand1(l)*cog_vector(l)
                soc_vector(l) = rand2(l)*soc_vector(l)
            end do

            struct%velocities(j, :) = struct%velocities(j, :) + cog_vector
            struct%velocities(j, :) = struct%velocities(j, :) + soc_vector

            struct%S(j, :) = struct%S(j, :) + struct%velocities(j, :)
            ! =================================================

            ! evaluate particle
            ! =================================================
            call check_boundaries(struct, n_params, lb, ub, j)
            ! =================================================

            ! check boundaries
            ! =================================================
            call calculate_errors(struct)
            ! =================================================

            ! check if personal best is found 
            ! =================================================
            if (struct%current_errors(j) < struct%best_errors(j)) then
                struct%best_errors(j) = struct%current_errors(j)
                struct%personal_best_parameters(j, :) = struct%S(j,:)
            end if

            min_position = minloc(struct%best_errors)
            min_position_error = struct%best_errors(min_position(1))
            ! =================================================

            deallocate(errors_hood)
            deallocate(hood_numbers%neigh)
        end do
        
        ! check if global best is found
        ! =================================================
        if(min_position_error < struct%global_best_error) then
            struct%global_best_error = min_position_error
            struct%best_particle = min_position(1)
            struct%parameter_of_best_particle = struct%S(min_position(1), :)

            if(struct%global_best_error <= desired_error) then
                exit
            end if
        else
            convergence_counter = convergence_counter + 1
            if (convergence_counter > 50) then
                convergence =  .TRUE.
                convergence_counter = 0
            end if
        end if
        ! =================================================

        ! print results 
        ! =================================================
        if (modulo(i, 50) == 0) then
            print*, struct%global_best_error
            print *, i
        end if 
        ! =================================================
        
        i = i + 1
    end do 

    deallocate(temp)

    result = struct%parameter_of_best_particle

end subroutine

end module psomod





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


program PSO
    use testfct
    use psomod

    implicit none

    real(8), allocatable, dimension(:) :: lbound
    real(8), allocatable, dimension(:) :: ubound
    real(8), allocatable, dimension(:) :: result

    real(8) :: desired_error = 0.000001

    allocate(lbound(3))
    allocate(ubound(3))
    allocate(result(3))
    lbound = 0.001
    ubound = 200.0
    call optimizer(60, 100000, 3, lbound, ubound, desired_error,  test_fct, result)

    print*, result

end program PSO