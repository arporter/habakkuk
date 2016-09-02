module first_line_array_assign_mod
  implicit none

contains

  subroutine array_assign(i, j, aprod, var1)
    ! Simple routine that has an array element assignment as
    ! its first executable statement.
    implicit none
    real(wp), dimension(:,:), intent(inout) :: aprod
    real(wp), intent(in) :: var1
    integer, intent(in) :: i, j

    aprod(i,j) = var1

  end subroutine array_assign
    
end module first_line_array_assign_mod
