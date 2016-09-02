module repeated_array_assign_mod
  implicit none

contains

  subroutine test_repeated_assign3(aprod, var1, var2, var3)
    ! Simple routine that contains multiple updates to the
    ! same variable
    implicit none
    real(wp), dimension(:,:), intent(inout) :: aprod
    real(wp), intent(in) :: var1, var2, var3
    integer :: i, j

    i = 1
    j = 1

    aprod(i,j) = var1 * var2 * var3
    aprod(i,j) = aprod(i,j) * var1

  end subroutine test_repeated_assign3
    
end module repeated_array_assign_mod
