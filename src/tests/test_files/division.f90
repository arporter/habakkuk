module division_mod
  implicit none

  private

contains

  !=================================================================

  subroutine do_division(aprod, var1, var2, var3)
    ! Simple routine to check what the parser makes of a single
    ! line containing a division operation
    implicit none
    real(wp), intent(out) :: aprod
    real(wp), intent(in) :: var1, var2, var3

    aprod = var1 / var2
    
  end subroutine do_division
  
end module division_mod
