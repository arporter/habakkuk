!> Example Fortran code where array look-ups are indirect
subroutine gather(nlayers, &
                  r_u, rho, rho_ref, theta, theta_ref, &
                  ndf_w2, undf_w2, map_w2,   &
                  ndf_w3, undf_w3, map_w3,   &
                  ndf_w0, undf_w0, map_w0,   &
                  nqp_h, nqp_v, wqp_h, wqp_v)
                           
  use calc_exner_pointwise_mod, only: calc_exner_pointwise
  
  !Arguments
  integer, intent(in) :: nlayers,nqp_h, nqp_v
  integer, intent(in) :: ndf_w0, ndf_w2, ndf_w3
  integer, intent(in) :: undf_w0, undf_w2, undf_w3
  integer, dimension(ndf_w0), intent(in) :: map_w0
  integer, dimension(ndf_w2), intent(in) :: map_w2
  integer, dimension(ndf_w3), intent(in) :: map_w3   

  real(kind=r_def), dimension(undf_w2), intent(inout) :: r_u
  real(kind=r_def), dimension(undf_w3), intent(in)    :: rho, rho_ref
  real(kind=r_def), dimension(undf_w0), intent(in)    :: theta, theta_ref

  real(kind=r_def), dimension(nqp_h), intent(in)      ::  wqp_h
  real(kind=r_def), dimension(nqp_v), intent(in)      ::  wqp_v

  !Internal variables
  integer               :: df, k 
  integer               :: qp1, qp2
  
  real(kind=r_def), dimension(ndf_w3)          :: rho_e, rho_ref_e
  real(kind=r_def), dimension(ndf_w2)          :: ru_e

  real(kind=r_def) :: grad_theta_at_quad(3), v(3), grad_theta_ref_at_quad(3)
  real(kind=r_def) :: exner_at_quad, rho_at_quad, theta_at_quad, &
                      grad_term, dv, rho_ref_at_quad, exner_ref_at_quad, &
                      theta_ref_at_quad

  do k = 0, nlayers-1
     do df = 1, ndf_w3
        rho_e(df)     = rho    ( map_w3(df) + k )
        rho_ref_e(df) = rho_ref( map_w3(df) + k )
     end do
  end do

end subroutine gather
