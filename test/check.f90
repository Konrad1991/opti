program basic_checks
    
    use testfct
    use psomod
    implicit none
    real(8), allocatable, dimension(:) :: lbound
    real(8), allocatable, dimension(:) :: ubound
    real(8), allocatable, dimension(:) :: result
    real(8) :: desired_error = 0.000001
    integer :: topology
    allocate(lbound(3))
    allocate(ubound(3))
    allocate(result(3))
    lbound = -10
    ubound = 10
    topology = 1
    !call optimizer(60, 10000, 3, lbound, ubound, &
    !               desired_error,  test_fct, result, topology)
    !print*, result

end program basic_checks


