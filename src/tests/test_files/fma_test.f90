module fma_test_mod
  implicit none

  private

contains

  !=================================================================

  subroutine test_fma(aprod, bprod, var1, var2, var3)
    ! Simple routine to check whether we can correctly identify
    ! the opportunity to perform a Fused Multiply Add.
    implicit none
    real(wp), intent(out) :: aprod, bprod
    real(wp), intent(in) :: var1, var2, var3

    aprod = var1 + var2 * var3
    bprod = var1*var2 + var3
    cprod = aprod + bprod
  end subroutine test_fma
  
end module fma_test_mod
