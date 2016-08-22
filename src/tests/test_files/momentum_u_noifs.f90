module time_step_mod
  implicit none

contains

  !=============================================================

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
    integer :: jj, ji, idxt
    real(wp) :: u_e, u_w, v_n, v_s, v_sc, v_nc, depe, depw, deps, depn
    real(wp) :: uu_e, uu_w, uu_s, uu_n, dudy_s, dudy_n, dudx_e, dudx_w
    real(wp) :: vis, adv, cor, hpg
    call timer_start('Momentum-u',idxt)
    !call likwid_markerStartRegion('Momentum-u')

    do jj = 2, N, 1
      do ji = 2, M-1, 1

         u_e  = 0.5 * (un(ji,jj) + un(ji+1,jj)) * dy_t(ji+1,jj)   !add length scale.
         depe = ht(ji+1,jj) + sshn_t(ji+1,jj)

         u_w  = 0.5 * (un(ji,jj) + un(ji-1,jj)) * dy_t(ji,jj)     !add length scale
         depw = ht(ji,jj) + sshn_t(ji,jj)

         v_sc = 0.5_wp * (vn(ji,jj-1) + vn(ji+1,jj-1))
         v_s  = 0.5_wp * v_sc * (dx_v(ji,jj-1) + dx_v(ji+1,jj-1))
         deps = 0.5_wp * (hv(ji,jj-1) + sshn_v(ji,jj-1) + hv(ji+1,jj-1) + &
              sshn_v(ji+1,jj-1))

         v_nc = 0.5_wp * (vn(ji,jj) + vn(ji+1,jj))
         v_n  = 0.5_wp * v_nc * (dx_v(ji,jj) + dx_v(ji+1,jj))
         depn = 0.5_wp * (hv(ji,jj) + sshn_v(ji,jj) + hv(ji+1,jj) + &
                     sshn_v(ji+1,jj))

         ! -advection (currently first order upwind)
         uu_w = (0.5_wp - SIGN(0.5_wp, u_w)) * un(ji,jj)              + & 
              & (0.5_wp + SIGN(0.5_wp, u_w)) * un(ji-1,jj) 
         uu_e = (0.5_wp + SIGN(0.5_wp, u_e)) * un(ji,jj)              + & 
              & (0.5_wp - SIGN(0.5_wp, u_e)) * un(ji+1,jj) 

            uu_s = (0.5_wp - SIGN(0.5_wp, v_s)) * un(ji,jj)              + & 
                 & (0.5_wp + SIGN(0.5_wp, v_s)) * un(ji,jj-1) 

            uu_n = (0.5_wp + SIGN(0.5_wp, v_n)) * un(ji,jj)              + & 
                 & (0.5_wp - SIGN(0.5_wp, v_n)) * un(ji,jj+1)

         adv = uu_w * u_w * depw - uu_e * u_e * depe + &
              uu_s * v_s * deps - uu_n * v_n * depn
         !end kernel u adv 

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
         vis = visc * vis   !visc will be an array visc(1:jpijglou) 
         !for variable viscosity, such as turbulent viscosity
         !End  kernel u vis 

         ! -Coriolis' force (can be implemented implicitly)
         !kernel cor 
         cor = 0.5_wp * (2._wp * omega * SIN(gphiu(ji,jj) * d2r) * (v_sc + v_nc)) * &
         & area_u(ji,jj) * (hu(ji,jj) + sshn_u(ji,jj))
         !end kernel cor 

         ! -pressure gradient
         !start kernel hpg 
         hpg = -g * (hu(ji,jj) + sshn_u(ji,jj)) * dy_u(ji,jj) * &
              (sshn_t(ji+1,jj) - sshn_t(ji,jj))
         !end kernel hpg 
         ! -linear bottom friction (implemented implicitly.
         !kernel ua calculation 
         ua(ji,jj) = (un(ji,jj) * (hu(ji,jj) + sshn_u(ji,jj)) + rdt * &
              (adv + vis + cor + hpg) / area_u(ji,jj)) / &
              (hu(ji,jj) + ssha_u(ji,jj)) / (1.0_wp + cbfr * rdt) 

      end do
    end do
 
    !call likwid_markerStopRegion('Momentum-u')
    call timer_stop(idxt)

  end subroutine invoke_momentum_u_arrays

end module time_step_mod
