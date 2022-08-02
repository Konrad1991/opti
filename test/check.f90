program check
    
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
    lbound = -10
    ubound = 10
    call optimizer(60, 100000, 3, lbound, ubound, desired_error,  test_fct, result)
    print*, result

end program check
