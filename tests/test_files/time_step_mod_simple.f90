module time_step_mod
  implicit none

  private

contains

  !=================================================================

  subroutine invoke_momentum_u_arrays(nx, ny, M, N, &
                                      ua, un, vn, &
                                      hu, hv, ht, &
                                      ssha_u, sshn_t,  &
                                      sshn_u, sshn_v,  &
                                      tmask,  &
                                      dx_u,   &
                                      dx_v,   &
                                      dx_t,   &
                                      dy_u,   &
                                      dy_t,   &
                                      area_u, &
                                      gphiu)
    use kind_params_mod
    use dl_timer
    use model_mod,       only: rdt, cbfr, visc
    use physical_params_mod, only: g, omega, d2r
    implicit none
    integer, intent(in) :: nx, ny, M, N
    real(wp), intent(out) :: ua(nx,ny)
    real(wp), dimension(nx,ny), intent(in) :: un, vn, hu, hv, ht, ssha_u, sshn_t, &
                                              sshn_u, sshn_v, dx_u, dx_v, dx_t, &
                                              dy_u, dy_t, area_u, gphiu
    integer, intent(in) :: tmask(nx,ny)
    ! Locals
    integer :: jj, ji
    real(wp) :: u_e, u_w, v_n, v_s, v_sc, v_nc, depe, depw, deps, depn
    real(wp) :: uu_e, uu_w, uu_s, uu_n, dudy_s, dudy_n, dudx_e, dudx_w
    real(wp) :: vis, adv, cor, hpg

    do jj = 2, N, 1
      do ji = 2, M-1, 1

         ! -viscosity

         !kernel  u vis 
         dudx_e = (un(ji+1,jj) - un(ji,  jj)) / dx_t(ji+1,jj) * &
              (ht(ji+1,jj) + sshn_t(ji+1,jj))

         dudx_w = (un(ji,  jj) - un(ji-1,jj)) / dx_t(ji,  jj) * &
              (ht(ji,  jj) + sshn_t(ji,  jj))

         dudy_s = (un(ji,jj) - un(ji,jj-1)) / (dy_u(ji,jj) + dy_u(ji,jj-1)) * &
            & (hu(ji,jj) + sshn_u(ji,jj) + hu(ji,jj-1) + sshn_u(ji,jj-1))

         dudy_n = (un(ji,jj+1) - un(ji,jj)) / (dy_u(ji,jj) + dy_u(ji,jj+1)) * &
            & (hu(ji,jj) + sshn_u(ji,jj) + hu(ji,jj+1) + sshn_u(ji,jj+1))

         vis = (dudx_e - dudx_w ) * dy_u(ji,jj)  + &
              & (dudy_n - dudy_s ) * dx_u(ji,jj) * 0.5_wp  
         !vis = visc * vis   !visc will be an array visc(1:jpijglou) 
         !for variable viscosity, such as turbulent viscosity
         !End  kernel u vis 

      end do
    end do
 
  end subroutine invoke_momentum_u_arrays

  subroutine test_triple_product(aprod, var1, var2, var3)
    ! Simple routine to check what the parser makes of a single
    ! line containing two multiplication operations
    implicit none
    real(wp), intent(out) :: aprod
    real(wp), intent(in) :: var1, var2, var3

    aprod = var1 * var2 * var3
    
  end subroutine test_triple_product
  
end module time_step_mod
