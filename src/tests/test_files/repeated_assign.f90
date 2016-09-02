module repeated_assign_mod
  implicit none

contains

  subroutine test_repeated_assign1(aprod, var1, var2, var3)
    ! Simple routine that contains multiple updates to the
    ! same scalar variable
    implicit none
    real(wp), intent(inout) :: aprod
    real(wp), intent(in) :: var1, var2, var3

    aprod = var1 * var2 * var3 * aprod
    aprod = aprod * var1
    aprod = aprod * aprod

  end subroutine test_repeated_assign1

  subroutine test_repeated_assign2(aprod, var1, var2, var3)
    ! Simple routine that contains multiple updates to the
    ! same scalar variable
    implicit none
    real(wp), intent(inout) :: aprod
    real(wp), intent(in) :: var1, var2, var3
    ! Locals
    real(wp) :: tmp1

    tmp1 = 2.0*aprod
    aprod = var1 * var2 * var3 * aprod
    aprod = aprod * var1 * tmp1
    aprod = aprod * aprod

  end subroutine test_repeated_assign2
    
end module repeated_assign_mod
