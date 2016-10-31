module nocontrol_loop_mod
  implicit none

contains

  subroutine basic_loop_routine(aprod)
    ! Simple routine that contains a loop with no control attributes and
    ! thus no obvious loop variable
    implicit none
    real(wp), dimension(:), intent(inout) :: aprod
    ! Locals
    integer :: i = 1

    do 
       aprod(i) = 2.0*i
       if(i > 10)exit
    end do

  end subroutine basic_loop_routine

end module nocontrol_loop_mod
