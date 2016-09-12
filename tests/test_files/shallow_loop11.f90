module shallow_module
  implicit none

contains

  subroutine loop11(M, N, uold,vold,pold,unew,vnew,pnew, u, v, p)
    implicit none
    integer, intent(in) :: M, N
    real, dimension(M, N) :: uold,vold,pold,unew,vnew,pnew,u,v,p
  
    integer :: i, j
    real :: alpha = 0.058
  
    DO J=1,N
       DO I=1,M
          UOLD(I,J) = U(I,J)+ALPHA*(UNEW(I,J)-2.0d0*U(I,J)+UOLD(I,J))
          VOLD(I,J) = V(I,J)+ALPHA*(VNEW(I,J)-2.0d0*V(I,J)+VOLD(I,J))
          POLD(I,J) = P(I,J)+ALPHA*(PNEW(I,J)-2.0d0*P(I,J)+POLD(I,J))
          U(I,J) = UNEW(I,J)
          V(I,J) = VNEW(I,J)
          P(I,J) = PNEW(I,J)
       END DO
    END DO

  end subroutine loop11

end module shallow_module
