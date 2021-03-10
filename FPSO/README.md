# Particle swarm optimization (PSO)

The repository pso contains a particle swarm optimizer written in fortran. The Code is in ./FPSO/PSO.F08. If you want to use it you can include the file in your project. The main function which conducts the parameter search is called 'optimizer' and needs 8 arguments.

1. number of particles (particle)
2. number of generations (n_gen)
3. how many parameters have to be optimized (n_params)
4. An array containing the lower boundaries for the optimization (dimension(n_params))
5. An array containing the upper boundaries for the optimization (dimension(n_params))
6. The desired minimal error 
7. The loss function
8. An array where the optimized parameter are stored (dimension(n_params))


# An easy example:


```fortran
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
```



