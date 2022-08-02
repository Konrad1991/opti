! Particle Swarm Optimization (PSO)
! Copyright (C) 2021 Konrad Krämer

! This file is part of pso


!pso is free software; you can redistribute it and/or
!modify it under the terms of the GNU General Public License
!as published by the Free Software Foundation; either version 2
!of the License, or (at your option) any later version.

!This program is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!GNU General Public License for more details.

!You should have received a copy of the GNU General Public License along with pso
! If not see: https://www.gnu.org/licenses/old-licenses/gpl-2.0.html#SEC4

! Contains parameter which are important during optimization
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
