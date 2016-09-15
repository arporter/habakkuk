module basic_loop_mod
  implicit none

contains

  subroutine basic_loop_routine(aprod)
    ! Simple routine that contains a loop
    implicit none
    real(wp), dimension(:), intent(inout) :: aprod
    ! Locals
    integer :: i

    do i = 1, size(aprod, 1)
       aprod(i) = 2.0*i
    end do

  end subroutine basic_loop_routine

end module basic_loop_mod
