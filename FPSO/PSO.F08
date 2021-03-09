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
    do i = 1, (problem_size -1)
        out = out + 100.0*(inp(i+1) - inp(i)**2)**2 + (1 - inp(i)**2)**2
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

subroutine optimizer(n_swarm, n_generations, n_params,  lb, ub, desired_error, fct)
    implicit none
    type(SwarmStruct) :: struct
    type(neighbor) :: hood
    integer :: n_swarm
    integer :: n_params
    real(8), dimension(n_params) :: lb, ub
    real(8), allocatable, dimension(:) :: temp
    integer, allocatable, dimension(:) :: hood_numbers
    integer :: i, j, k
    integer :: n_generations
    integer :: best_neighberhood_particle
    real(8), intent(in) :: desired_error
    logical :: checker
    logical :: convergence

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
        end if

        par_w = par_w_max - i*(par_w_max - par_w_min)/n_generations

        cog = initial_cog - (initial_cog - final_cog) * (i + 1) / n_generations
        soc = initial_soc - (initial_soc - final_soc) * (i + 1) / n_generations

        do j = 1, n_swarm 
            allocate(hood_numbers(hood%K(j)))

            hood_numbers = hood%N%neigh(j)


            deallocate(hood_numbers)
        end do 







        i = i + 1
    end do 

    deallocate(temp)

end subroutine

end module

program PSO
    use testfct
    use psomod

    implicit none

    real(8), allocatable, dimension(:) :: lbound
    real(8), allocatable, dimension(:) :: ubound
    real(8) :: desired_error = 0.00001

    allocate(lbound(3))
    allocate(ubound(3))
    lbound = 0.9
    ubound = 1.1
    call optimizer(4000, 10, 3, lbound, ubound, desired_error,  test_fct)


end program PSO