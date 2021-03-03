module Init
    type SwarmStruct
    real(8), allocatable, dimension(:,:) :: S
    real(8), allocatable, dimension(:,:) :: velocities
    real(8), allocatable, dimension(:) :: best_errors
    real(8) :: best_particle
    real(8), allocatable, dimension(:) :: parameter_of_best_particle
    integer :: n_swarm !! number of rows of swarm & velocities
    integer :: n_params !! number of cols of swarm & velocities 
    procedure(func), pointer, nopass :: userfct 
end type SwarmStruct

abstract interface
    function func (inp, problem_size) result(out)
      implicit none
      integer, intent(in) :: problem_size
      real(8), intent(in), dimension(problem_size) :: inp
      real(8) :: out
    end function func
end interface

real(8) :: cog = 2.5
real(8) :: soc = 0.5

contains

subroutine calculate_errors(inp)
    implicit none
    type(SwarmStruct) :: inp
    integer :: i
    do i = 1, inp%n_swarm
        inp%best_errors(i) = inp%userfct(inp%S(i, :), inp%n_params)
    end do

end subroutine

subroutine init_fct(inp_struct, n_swarm, n_params, lb, ub, fct)
    type(SwarmStruct) :: inp_struct
    integer :: n_swarm, n_params
    real(8), dimension(n_params) :: lb, ub
    real(8), allocatable, dimension(:) :: temp
    integer :: i

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
    allocate(inp_struct%best_errors(n_swarm))
    allocate(inp_struct%parameter_of_best_particle(n_params))

    allocate(temp(n_params))
    inp_struct%best_errors = 0.0
    inp_struct%parameter_of_best_particle = 0.0
    inp_struct%velocities = 0.0

    inp_struct%userfct => fct

    do i = 1, n_swarm
        call random_number(temp)
        inp_struct%S(i, :) = lb + (ub - lb)*temp
    end do

    call calculate_errors(inp_struct)
    
end subroutine

end module