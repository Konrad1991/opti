program main

  use nhood, only: nb, init, print_nbhood, recalc, delete_nbhood
  use swarm
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

end program main
