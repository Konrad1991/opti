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


! Main module of the particle swarm optimization (PSO)
module psomod

    use swarm
    use parameters
    use nhood

contains

    ! subroutine which evaluates particle
    subroutine calculate_errors(inp)
        implicit none
        type(swarm_struct) :: inp
        integer :: i
        do i = 1, inp%n_swarm
            inp%current_errors(i) = inp%userfct(inp%S(i, :), inp%n_params)
        end do
    end subroutine

    ! checks if particle are above or below boundary.
    ! If boundaries are violated the particle is set on the boundary
    subroutine check_boundaries(inp, n_params, lb, ub, index)
        implicit none
        type(swarm_struct), intent(inout):: inp
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
        type(swarm_struct) :: struct

        type(nb), allocatable, dimension(:) :: nbhood
        integer :: min_position
        integer :: best_pos(1)

        real(8) :: min_position_error

        integer :: n_swarm
        integer :: n_params

        real(8), dimension(n_params) :: lb, ub
        real(8), allocatable, dimension(:) :: temp

        integer :: i, j, k, l
        integer :: n_generations
        integer :: best_neighberhood_particle
        real(8), intent(in) :: desired_error
        logical :: checker
        logical :: convergence

        real(8), dimension(n_params) :: local_best_parameters

        real(8), dimension(n_params) :: rand1
        real(8), dimension(n_params) :: rand2
        real(8), dimension(n_params) :: cog_vector
        real(8), dimension(n_params) :: soc_vector

        integer :: convergence_counter

        real(8), intent(inout), dimension(n_params) :: result

        interface
            function fct (inp, problem_size) result(out)
                implicit none
                integer, intent(in) :: problem_size
                real(8), intent(in), dimension(problem_size) :: inp
                real(8) :: out
            end function fct
        end interface

        ! Initialisation swarm
        struct = initialise(n_swarm, n_params, lb, ub, fct)
        struct%best_particle = minloc(struct%best_errors, 1)
        struct%parameter_of_best_particle = struct%S(struct%best_particle, :)

        ! init remaining vars
        checker = .TRUE.
        convergence = .FALSE.
        i = 0
        allocate(temp(n_params))
        convergence_counter = 0

        ! Start generation loop
        do while ( (checker .eqv. .TRUE.) .and. (i < n_generations))

            ! calculation of neighberhood if needed
            if (i == 0) then
                ! Initialize nbhood
                nbhood = init(n_swarm)
            else if (convergence .eqv. .TRUE.) then
                call recalc(n_swarm, nbhood)
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
                if(nbhood(j)%size == 1) then
                    min_position = j
                else if(nbhood(j)%size == 2) then
                    if(struct%best_errors(nbhood(j)%neighbours(1)) < struct%best_errors(nbhood(j)%neighbours(2)) ) then
                        min_position = nbhood(j)%neighbours(1)
                    else
                        min_position = nbhood(j)%neighbours(2)
                    end if
                else if(nbhood(j)%size > 2) then
                    do k = 1, (nbhood(j)%size -1)
                        if(struct%best_errors(nbhood(j)%neighbours(k)) < struct%best_errors(nbhood(j)%neighbours(k + 1)) ) then
                            min_position = nbhood(j)%neighbours(k)
                        end if
                    end do
                end if

                local_best_parameters = struct%S(min_position, :)
                ! =================================================

                ! Update velocities and particle
                ! =================================================
                call random_number(rand1)
                call random_number(rand2)

                struct%velocities(j, :) = par_w*struct%velocities(j,:) + &
                        rand1*cog*(struct%personal_best_parameters(j, :) - struct%S(j, :) ) &
                        + rand2*soc*(local_best_parameters - struct%S(j, :))
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

                best_pos = minloc(struct%best_errors)
                min_position_error = struct%best_errors(best_pos(1))
                ! =================================================

            end do

            ! check if global best is found
            ! =================================================
            if(min_position_error < struct%global_best_error) then
                struct%global_best_error = min_position_error
                struct%best_particle = min_position
                struct%parameter_of_best_particle = struct%S(best_pos(1), :)

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

        call delete_nbhood(n_swarm ,nbhood)
        call delete_swarm(struct)

    end subroutine

end module psomod