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
    real(8) :: initial_cog = 2.5
    real(8) :: initial_soc = 0.5
    real(8) :: final_cog = 0.5
    real(8) :: final_soc = 2.5
    real(8) :: par_w = 0.5
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

subroutine optimizer(n_swarm, n_params,  lb, ub, desired_error, fct)
    implicit none
    type(SwarmStruct) :: struct
    type(neighbor) :: hood
    integer :: n_swarm
    integer :: n_params
    real(8), dimension(n_params) :: lb, ub
    real(8), allocatable, dimension(:) :: temp
    integer :: i
    real(8) :: desired_error

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


end subroutine

end module

program PSO
    use testfct
    use psomod

    implicit none

    real(8), allocatable, dimension(:) :: lbound
    real(8), allocatable, dimension(:) :: ubound

    allocate(lbound(3))
    allocate(ubound(3))
    lbound = 0.9
    ubound = 1.1
    call optimizer(4000, 3, lbound, ubound, 0.0,  test_fct)


end program PSO