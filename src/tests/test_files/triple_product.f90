module triple_product_mod
  implicit none

  private

contains

  !=================================================================

  subroutine test_triple_product(aprod, var1, var2, var3)
    ! Simple routine to check what the parser makes of a single
    ! line containing two multiplication operations
    implicit none
    real(wp), intent(out) :: aprod
    real(wp), intent(in) :: var1, var2, var3

    aprod = var1 * var2 * var3
    
  end subroutine test_triple_product
  
end module triple_product_mod
