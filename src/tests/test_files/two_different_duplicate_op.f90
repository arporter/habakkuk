module two_different_duplicate_op_mod
  implicit none

  private

contains

  !=================================================================

  subroutine test_duplicate_prod_div(aprod, bprod, cprod, dprod, &
                                     var1, var2, var3)
    ! Simple routine containing four assignments; three contain the 
    ! same product and two the same division operation
    implicit none
    real(wp), intent(out) :: aprod, bprod, cprod, dprod
    real(wp), intent(in)  :: var1, var2, var3

    aprod = var1 * var2 * var3

    bprod = var1 * var2 / var3
    
    cprod = var1 * var2 + var3

    dprod = var1 + var2 / var3
    
  end subroutine test_duplicate_prod_div
  
end module two_different_duplicate_op_mod
