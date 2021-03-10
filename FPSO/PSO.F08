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

module psomod 

use init
use parameters
use neighberhood
use parameters

contains 
subroutine calculate_errors(inp)
    implicit none
    type(SwarmStruct) :: inp
    integer :: i
    do i = 1, inp%n_swarm
        inp%current_errors(i) = inp%userfct(inp%S(i, :), inp%n_params)
    end do
end subroutine

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

    call init_fct(struct, n_swarm, n_params, lb, ub, fct)
    struct%best_particle = minloc(struct%best_errors, 1)
    struct%parameter_of_best_particle = struct%S(struct%best_particle, :)

    checker = .TRUE.
    convergence = .FALSE.
    i = 0
    allocate(temp(n_params))

    do while ( (checker .eqv. .TRUE.) .and. (i < n_generations))

        if (i == 0) then
            call calc_neighbours(hood, n_swarm)
        else if (convergence .eqv. .TRUE.) then
            deallocate(hood%N)
            deallocate(hood%K)
            call calc_neighbours(hood, n_swarm)
            convergence = .FALSE.
        end if

        par_w = par_w_max - i*(par_w_max - par_w_min)/n_generations

        cog = initial_cog - (initial_cog - final_cog) * (i + 1) / n_generations
        soc = initial_soc - (initial_soc - final_soc) * (i + 1) / n_generations

        do j = 1, n_swarm 
            allocate(hood_numbers%neigh(hood%K(j)))
            allocate(errors_hood(hood%K(j)))
            hood_numbers%neigh = hood%N(j)%neigh

            do k = 1, hood%K(j)
                errors_hood(k) = struct%best_errors(hood_numbers%neigh(k))
            end do

            min_position = minloc(errors_hood)

            position_local_best = hood_numbers%neigh(min_position(1))

            local_best_parameters = struct%S(position_local_best, :)

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

            call check_boundaries(struct, n_params, lb, ub, j)

            call calculate_errors(struct)

            if (struct%current_errors(j) < struct%best_errors(j)) then
                struct%best_errors(j) = struct%current_errors(j)
                struct%personal_best_parameters(j, :) = struct%S(j,:)
            end if

            min_position = minloc(struct%best_errors)
            min_position_error = struct%best_errors(min_position(1))

            deallocate(errors_hood)
            deallocate(hood_numbers%neigh)
        end do
        
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

        if (modulo(i, 50) == 0) then
            print*, struct%global_best_error
            print *, i
        end if 
        
        i = i + 1
    end do 

    deallocate(temp)

    result = struct%parameter_of_best_particle

end subroutine

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