
# Overview

The repository opti contains a particle swarm optimizer (PSO) written in fortran. In the future more optimizer are planed.
For the PSO the user can chose between two topologies. Either the traditional star topology or an random adaptive neighberhood which is better for exploration of parameter spaces. This is useful when many parameters have to be optimized.

# Installation

The project structured as a fpm (fortran package manager) project. Thus, the project can be used by adding the following two lines to the toml file:

[dependencies] \
opti = { git="https://github.com/Konrad1991/opti"}

# Documentation

The project contains only one subroutine called optimizer. It accepts the following parameter:

1. **integer ::** number of particles (particle)
2. **integer ::** number of generations (n_gen)
3. **integer ::** how many parameters have to be optimized (n_params)
4. **real(8), dimension(n_params) ::** An array containing the lower boundaries for the optimization (dimension(n_params))
5. **real(8), dimension(n_params) ::** An array containing the upper boundaries for the optimization (dimension(n_params))
6. **real(8) ::** The desired minimal error 
7. The loss function is of type:

```fortran 
interface
    function fct (inp, problem_size) result(out)
        implicit none
        integer, intent(in) :: problem_size
        real(8), intent(in), dimension(problem_size) :: inp
        real(8) :: out
    end function fct
end interface
```
        
8. **real(8), dimension(n_params) ::** An array where the optimized parameter are stored (dimension(n_params))
9. **integer ::** An optional parameter defining the topology. If chosen 1 the star topology is used. If chosen 2 the random adaptive topology is used. The default value is 1 (topo) 
10. **integer ::** An optional parameter defining the number of neighbours if the topology is the random adaptive topology. Otherwise the parameter is ignored. The default value is set to 4. 


## An easy example:


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
  n_generations = 10000
  n_params = 3
  lb = -10
  ub = 10
  desired_error = 0.00001

  call optimizer(n_swarm, n_generations, n_params,  lb, ub, desired_error, test_fct, result)

  print*, result

end program PSO
```




