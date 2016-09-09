module empty_routine_mod
  implicit none

contains

  subroutine empty_routine(aprod)
    ! Simple routine that contains no assignment statements
    ! so we shouldn't generate a DAG
    implicit none
    real(wp), intent(inout) :: aprod

    call a_routine(aprod)

  end subroutine empty_routine

end module empty_routine_mod
