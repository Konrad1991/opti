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


subroutine calc_neighbours(NE, number_particles)
    implicit none
    type(neighbor), intent(inout) :: NE
    integer, intent(in) :: number_particles
    integer :: K_Lower
    integer :: K_upper
    real :: temp1
    real :: temp2
    integer :: i, j
    type(neighbours) :: temp3 

    NE%number_particles = number_particles
    call resize(NE)
    K_Lower = 0
    K_upper = 3
    do i = 1, number_particles
        call random_number(temp1)
        NE%K(i) = floor(4*temp1)

        if(NE%K(i) == 0) then 
            allocate(temp3%neigh(1))
            temp3%neigh(1) = i 
            NE%N(1) = temp3
            deallocate(temp3%neigh)
        else if (NE%K(i) == 1) then 
            allocate(temp3%neigh(2))
            temp3%neigh(1) = i 
            call random_number(temp2)
            temp3%neigh(1) = i
            NE%N(1) = temp3
            temp3%neigh(2) = floor(number_particles*temp2)
            NE%N(2) = temp3
            deallocate(temp3%neigh)
        else if (NE%K(i) > 1) then
            allocate(temp3%neigh(NE%K(i)))
            temp3%neigh(1) = i
            NE%N(1) = temp3
            do j = 2, NE%K(i)
                call random_number(temp2)
                temp3%neigh(j) = floor(number_particles*temp2)
                NE%N(j) = temp3
            end do 
            deallocate(temp3%neigh)
        end if 

       
    end do 


end subroutine



    
end module neighberhood