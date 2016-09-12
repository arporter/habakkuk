!> @brief Kernel which computes the perturbation pressure gradient for lhs of the momentum equation
module pert_pressure_gradient_kernel_mod

use argument_mod,      only : arg_type, func_type,                 &
                              GH_FIELD, GH_READ, GH_INC,           &
                              W0, W2, W3, GH_BASIS, GH_DIFF_BASIS, &
                              CELLS
use constants_mod,     only : r_def, vlen
use kernel_mod,        only : kernel_type
use planet_config_mod, only : Cp, kappa, Rd, p_zero
use timing_mod
implicit none

!-------------------------------------------------------------------------------
! Contained functions/subroutines
!-------------------------------------------------------------------------------
public pert_pressure_gradient_code
contains

subroutine pert_pressure_gradient_code(nlayers,&
                                  r_u, rho, rho_ref, theta, theta_ref,&
                                  ndf_w2, undf_w2, map_w2, w2_basis,w2_diff_basis, &
                                  ndf_w3, undf_w3, map_w3, w3_basis,&
                                  ndf_w0, undf_w0, map_w0, w0_basis,w0_diff_basis, &
                                  nqp_h, nqp_v, wqp_h, wqp_v&
                                  )

  use calc_exner_pointwise_mod, only: calc_exner_pointwise

  !Arguments
  integer, intent(in) :: nlayers,nqp_h, nqp_v
  integer, intent(in) :: ndf_w0, ndf_w2, ndf_w3
  integer, intent(in) :: undf_w0, undf_w2, undf_w3
  integer, dimension(ndf_w0), intent(in) :: map_w0
  integer, dimension(ndf_w2), intent(in) :: map_w2
  integer, dimension(ndf_w3), intent(in) :: map_w3

  real(kind=r_def), dimension(1,ndf_w3,nqp_h,nqp_v), intent(in) :: w3_basis
  real(kind=r_def), dimension(3,ndf_w2,nqp_h,nqp_v), intent(in) :: w2_basis
  real(kind=r_def), dimension(1,ndf_w0,nqp_h,nqp_v), intent(in) :: w0_basis
  real(kind=r_def), dimension(1,ndf_w2,nqp_h,nqp_v), intent(in) :: w2_diff_basis
  real(kind=r_def), dimension(3,ndf_w0,nqp_h,nqp_v), intent(in) :: w0_diff_basis

  real(kind=r_def), dimension(undf_w2), intent(inout) :: r_u
  real(kind=r_def), dimension(undf_w3), intent(in)    :: rho, rho_ref
  real(kind=r_def), dimension(undf_w0), intent(in)    :: theta, theta_ref

  real(kind=r_def), dimension(nqp_h), intent(in)      ::  wqp_h
  real(kind=r_def), dimension(nqp_v), intent(in)      ::  wqp_v

  !Internal variables
  integer               :: df, k ,i, map
  integer               :: qp1, qp2

  real(kind=r_def), dimension(vlen,ndf_w3)          :: rho_e, rho_ref_e
  real(kind=r_def), dimension(vlen,ndf_w2)          :: ru_e
  real(kind=r_def), dimension(vlen,ndf_w0)          :: theta_e, theta_ref_e

  real(kind=r_def) :: grad_theta_at_quad(vlen,3), grad_theta_ref_at_quad(vlen,3)
  real(kind=r_def) :: exner_at_quad(vlen), rho_at_quad(vlen), theta_at_quad(vlen), &
                      grad_term(vlen), rho_ref_at_quad(vlen), exner_ref_at_quad(vlen), &
                      theta_ref_at_quad(vlen),temp1, temp2
  integer                           :: itimer0

  temp1 = Rd/p_zero
  temp2 = kappa/(1.0_r_def - kappa)

  !CALL timer_start('pert-pressure v1', itimer0 )
  do k = 0, nlayers-1,vlen
    do df = 1, ndf_w3
      map = map_w3(df)
      !$OMP SIMD
      do i = 1,vlen
      rho_e(i,df)     = rho    ( map + k + i - 1 )
      rho_ref_e(i,df) = rho_ref( map + k + i - 1 )
      end do
      !$OMP END SIMD
    end do
    do df = 1, ndf_w0
      map = map_w0(df)
      !$OMP SIMD
      do i = 1,vlen
      theta_e(i,df)     = theta    ( map + k + i - 1)
      theta_ref_e(i,df) = theta_ref( map + k + i - 1)
      end do
      !$OMP END SIMD
    end do
    do df = 1, ndf_w2
      ru_e(:,df) = 0.0_r_def
    end do
  ! compute the RHS integrated over one cell  
    do qp2 = 1, nqp_v
      do qp1 = 1, nqp_h
        rho_at_quad(:) = 0.0_r_def
        rho_ref_at_quad(:) = 0.0_r_def
        do df = 1, ndf_w3
          !$OMP SIMD
          do i = 1,vlen
          rho_at_quad(i)      = rho_at_quad(i)     + rho_e(i,df)*w3_basis(1,df,qp1,qp2)
          rho_ref_at_quad(i)  = rho_ref_at_quad(i) + rho_ref_e(i,df)*w3_basis(1,df,qp1,qp2)
          end do
          !$OMP END SIMD
        end do
 
        theta_at_quad(:) = 0.0_r_def
        grad_theta_at_quad(:,:) = 0.0_r_def
        theta_ref_at_quad(:) = 0.0_r_def
        grad_theta_ref_at_quad(:,:) = 0.0_r_def

        do df = 1, ndf_w0
          !$OMP SIMD
          do i = 1,vlen
            theta_at_quad(i)   = theta_at_quad(i)                                     &
                          + theta_e(i,df)*w0_basis(1,df,qp1,qp2)
            theta_ref_at_quad(i)   = theta_ref_at_quad(i)                             &
                          + theta_ref_e(i,df)*w0_basis(1,df,qp1,qp2)
            grad_theta_at_quad(i,:) = grad_theta_at_quad(i,:) &
                                + theta_e(i,df)*w0_diff_basis(:,df,qp1,qp2)
            grad_theta_ref_at_quad(i,:) = grad_theta_ref_at_quad(i,:) &
                                + theta_ref_e(i,df)*w0_diff_basis(:,df,qp1,qp2)
          end do
          !$OMP END SIMD
        end do

        !$OMP SIMD
        do i = 1,vlen
          exner_ref_at_quad(i)  = ( temp1 * rho_ref_at_quad(i) * theta_ref_at_quad(i) ) ** temp2
          exner_at_quad(i) = temp2 * exner_ref_at_quad(i) * ( theta_at_quad(i)/theta_ref_at_quad(i) &
                      + rho_at_quad(i)/rho_ref_at_quad(i) )
        end do
        !$OMP END SIMD

        do df = 1, ndf_w2
          !v  = w2_basis(:,df,qp1,qp2)
          !dv = w2_diff_basis(1,df,qp1,qp2)
          !$OMP SIMD
          do i = 1,vlen
          ! theta_prime * grad(exner_ref) term
          grad_term(i) = Cp*exner_ref_at_quad(i) * (theta_at_quad(i) * w2_diff_basis(1,df,qp1,qp2) &
                    + grad_theta_at_quad(i,1) * w2_basis(1,df,qp1,qp2)     &
                    + grad_theta_at_quad(i,2) * w2_basis(2,df,qp1,qp2)     &
                    + grad_theta_at_quad(i,3) * w2_basis(3,df,qp1,qp2) )
          ! theta_ref * grad(exner_prime) term
          grad_term(i) = grad_term(i) + Cp*exner_at_quad(i) * ( theta_ref_at_quad(i) & 
                    * w2_diff_basis(1,df,qp1,qp2)      &
                    + grad_theta_ref_at_quad(i,1) * w2_basis(1,df,qp1,qp2) & 
                    + grad_theta_ref_at_quad(i,2) * w2_basis(2,df,qp1,qp2) &
                    + grad_theta_ref_at_quad(i,3) * w2_basis(3,df,qp1,qp2) )

          ru_e(i,df) = ru_e(i,df) +  wqp_h(qp1)*wqp_v(qp2)*grad_term(i)
          end do
          !$OMP END SIMD
        end do
      end do
    end do
    do df = 1, ndf_w2
      map = map_w2(df)
      !$OMP SIMD
      do i = 1,vlen
      r_u( map + k + i - 1 ) =  r_u( map + k + i - 1) + ru_e(i,df)
      end do
      !$OMP END SIMD
    end do
  end do
  !call timer_stop(itimer0)

end subroutine pert_pressure_gradient_code

end module pert_pressure_gradient_kernel_mod
