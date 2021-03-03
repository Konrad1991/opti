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


program PSO
    use init
    use ps
    use testfct

    implicit none
    real(8), allocatable, dimension(:) :: lbound
    real(8), allocatable, dimension(:) :: ubound
    type(SwarmStruct) :: SOne
    real(8) res
    integer :: i

    allocate(lbound(5))
    allocate(ubound(5))
    lbound = 0.0
    ubound = 10
    call init_fct(SOne, 40, 3, lbound, ubound, test_fct)
    i = minloc(SOne%best_errors, 1)

    call calculate_errors(SOne)
    print *, i
    print *, SOne%best_errors(i)
    print *, SOne%S(i, :)
    
    call test()

end program PSO