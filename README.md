
# Particle swarm optimization (PSO)

The repository pso contains a particle swarm optimizer written in fortran. It uses an random adaptive neighberhood for better exploration of parameter spaces. Useful when many parameters have to be optimized.

Furthermore, is the project structured as a fpm project. Thus, the project can be used by adding the following two lines to the toml file:

[dependencies] \
pso = { git="https://github.com/Konrad1991/pso"}



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

  integer:: n_swarm, n_generations, n_params
  real(8) :: lb(3), ub(3)
  real(8) :: desired_error
  real(8) :: result(3)


  n_swarm = 40
  n_generations = 1000
  n_params = 3
  lb = -10
  ub = 10
  desired_error = 0.00001

  call optimizer(n_swarm, n_generations, n_params,  lb, ub, desired_error, test_fct, result)

  print*, result

end program PSO
```




