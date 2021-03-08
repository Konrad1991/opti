module neighberhood
    implicit none

    type neighbours
        integer, allocatable, dimension(:) :: neigh
    end type neighbours
    
    type neighbor
        integer :: number_particles
        type(neighbours), allocatable, dimension(:) :: N
        integer, allocatable, dimension(:) :: K
    end type neighbor
contains

subroutine resize(NE) 
    implicit none
    type(neighbor) :: NE
    allocate(NE%N(NE%number_particles))
    allocate(NE%K(NE%number_particles))
end subroutine

subroutine generate_random_int(upper, num, res, position)
    implicit none
    real(8) :: rand_real
    integer, intent(in) :: num
    integer, intent(inout), dimension(num) :: res
    integer, intent(in) :: upper
    integer :: i
    integer :: position

    res(1) = position

    do i = 2, num
        call random_number(rand_real)
        res(i) = floor(upper*rand_real)
    end do
end subroutine


subroutine calc_neighbours(NE, number_particles)
    implicit none
    type(neighbor), intent(inout) :: NE
    integer, intent(in) :: number_particles
    integer :: K_Lower
    integer :: K_upper
    real :: temp1
    real :: temp2
    integer :: i, j
    type(neighbours) :: temp3 !! 0 neighbours
    type(neighbours) :: temp4 !! 1 neighbour
    type(neighbours) :: temp5 !! 2 neighbours
    type(neighbours) :: temp6 !! 3 neighbours
    integer, allocatable, dimension(:) :: temp_neigh

    NE%number_particles = number_particles
    call resize(NE)
    K_Lower = 0
    K_upper = 3
    do i = 1, number_particles
        call random_number(temp1)
        NE%K(i) = floor(4*temp1)

        allocate(temp3%neigh(1))
        allocate(temp4%neigh(2))
        allocate(temp5%neigh(3))
        allocate(temp6%neigh(4))

        temp3%neigh(1) = i

        if (NE%K(i) == 0) then
            NE%N(i) = temp3
        else if(NE%K(i) == 1) then 
            allocate(temp_neigh(2))
            call generate_random_int(number_particles, 2, temp_neigh, i)
            temp4%neigh = temp_neigh
            NE%N(i) = temp4
            deallocate(temp_neigh)
        else if(NE%K(i) == 2) then
            allocate(temp_neigh(3))
            call generate_random_int(number_particles, 3, temp_neigh, i)
            temp5%neigh = temp_neigh
            NE%N(i) = temp5
            deallocate(temp_neigh)
        else if(NE%K(i) == 3) then
            allocate(temp_neigh(4))
            call generate_random_int(number_particles, 4, temp_neigh, i)
            temp6%neigh = temp_neigh
            NE%N(i) = temp6
            deallocate(temp_neigh)
        end if

        deallocate(temp3%neigh)
        deallocate(temp4%neigh)
        deallocate(temp5%neigh)
        deallocate(temp6%neigh)

    end do 
end subroutine

end module neighberhood
