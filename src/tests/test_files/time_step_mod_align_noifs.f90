module time_step_mod
  implicit none

  private

  public invoke_time_step

contains

  subroutine invoke_time_step(istp, ssha, ssha_u, ssha_v, &
                              sshn_t, sshn_u, sshn_v, &
                              hu, hv, ht, ua, va, un, vn)
    use global_parameters_mod, only: ALIGNMENT
    use kind_params_mod
    use dl_timer
    use field_mod
    use grid_mod
    use model_mod,       only: rdt, cbfr, visc
    use physical_params_mod, only: g, omega, d2r
!    use momentum_mod,    only: momentum_v_code
!    use momentum_mod,    only: momentum_u_code
!    use continuity_mod,  only: continuity_code
!    use time_update_mod, only: next_sshu_code, next_sshv_code
    use boundary_conditions_mod
    !use likwid
    implicit none
    real(wp),        intent(in)    :: istp
    type(r2d_field), intent(inout) :: un, vn, sshn_t, sshn_u, sshn_v
    type(r2d_field), intent(inout) :: ua, va, ssha, ssha_u, ssha_v
    type(r2d_field), intent(in)    :: hu, hv, ht
    ! Locals
    integer :: ji, jj, jiu, jiv
    integer :: M, N, idxt
    ! Locals for momentum
    REAL(wp) :: u_e, u_w, v_n, v_s
    real(wp) :: v_nc, v_sc
    real(wp) :: depe, depw, deps, depn
    real(wp) :: hpg, adv, cor, vis
    real(wp) :: dudx_e, dudx_w, dudy_s, dudy_n
    real(wp) :: uu_e, uu_n, uu_s, uu_w
    real(wp) :: u_ec, u_wc, vv_e, vv_n, vv_s, vv_w
    real(wp) :: dvdx_e, dvdx_w, dvdy_n, dvdy_s
    real(wp) :: rtmp1
    ! end locals for momentum
    ! Locals for BCs
    real(wp) :: amp_tide, omega_tide, rtime

    M  = ssha%grid%simulation_domain%xstop
    if(mod(M, ALIGNMENT) .ne. 0)then
       M = (M/ALIGNMENT + 1)*ALIGNMENT
    end if
!DIR$ ASSUME (MOD(M,ALIGNMENT) .EQ. 0)
    N  = ssha%grid%simulation_domain%ystop

    ! In the general case we have to reason about whether or not the
    ! domain has PBCs and what sort of offset convention the kernels
    ! use. However, this is a middle layer specific to NEMOLite2D and
    ! therefore we know that we have no periodic BCs and are using a
    ! NE stagger
    !txstart = 2 ! grid%simulation_domain%xstart
    !tystart = 2 ! grid%simulation_domain%ystart

    !uxstart = 2 ! grid%simulation_domain%xstart
    !uxstop  = M - 1
    !uystart = 2 ! grid%simulation_domain%ystart
    !uystop  = N

    !vxstart = 2 ! grid%simulation_domain%xstart
    !vxstop  = M
    !vystart = 2 ! grid%simulation_domain%ystart
    !vystop  = N - 1

    !uwhole_xstart = 1 ! uxstart - NBOUNDARY
    !uwhole_xstop  = M ! uxstop  + NBOUNDARY
    !uwhole_ystart = 1 ! uystart - NBOUNDARY
    !uwhole_ystop  = N+1 ! uystop  + NBOUNDARY

    !vwhole_xstart = 1 ! vxstart - NBOUNDARY
    !vwhole_xstop  = M+1 ! vxstop  + NBOUNDARY
    !vwhole_ystart = 1 ! vystart - NBOUNDARY
    !vwhole_ystop  = N ! vystop  + NBOUNDARY

    call invoke_continuity_arrays(sshn_t%grid%nx, sshn_t%grid%ny, M, N, rdt, &
                                  ssha%data, &
                                  sshn_t%data, sshn_u%data, sshn_v%data, &
                                  hu%data, hv%data, &
                                  un%data, vn%data, sshn_t%grid%area_t)


!!$    call invoke_momentum_u_arrays(sshn_t%grid%nx, sshn_t%grid%ny, M, N, &
!!$                                  ua%data, un%data, vn%data, &
!!$                                  hu%data, hv%data, ht%data, &
!!$                                  ssha_u%data, sshn_t%data,  &
!!$                                  sshn_u%data, sshn_v%data,  &
!!$                                  un%grid%tmask,  &
!!$                                  un%grid%dx_u,   &
!!$                                  un%grid%dx_v,   &
!!$                                  un%grid%dx_t,   &
!!$                                  un%grid%dy_u,   &
!!$                                  un%grid%dy_t,   &
!!$                                  un%grid%area_u, &
!!$                                  un%grid%gphiu)

    call timer_start('Momentum-u',idxt)
    !call likwid_markerStartRegion('Momentum-u')

!    do jj = ua%internal%ystart, ua%internal%ystop, 1
!      do ji = ua%internal%xstart, ua%internal%xstop, 1
!dir$ safe_address
    do jj = 2, N, 1
! SIMD
!dir$ vector always
      do ji = 2, M-1, 1

    u_e  = 0.5 * (un%data(ji,jj) + un%data(ji+1,jj)) * un%grid%dy_t(ji+1,jj)   !add length scale.
    depe = ht%data(ji+1,jj) + sshn_t%data(ji+1,jj)

    u_w  = 0.5 * (un%data(ji,jj) + un%data(ji-1,jj)) * un%grid%dy_t(ji,jj)     !add length scale
    depw = ht%data(ji,jj) + sshn_t%data(ji,jj)

    v_sc = 0.5_wp * (vn%data(ji,jj-1) + vn%data(ji+1,jj-1))
    v_s  = 0.5_wp * v_sc * (un%grid%dx_v(ji,jj-1) + un%grid%dx_v(ji+1,jj-1))
    deps = 0.5_wp * (hv%data(ji,jj-1) + sshn_v%data(ji,jj-1) + hv%data(ji+1,jj-1) + &
                     sshn_v%data(ji+1,jj-1))

    v_nc = 0.5_wp * (vn%data(ji,jj) + vn%data(ji+1,jj))
    v_n  = 0.5_wp * v_nc * (un%grid%dx_v(ji,jj) + un%grid%dx_v(ji+1,jj))
    depn = 0.5_wp * (hv%data(ji,jj) + sshn_v%data(ji,jj) + hv%data(ji+1,jj) + &
                     sshn_v%data(ji+1,jj))

    ! -advection (currently first order upwind)
    uu_w = (0.5_wp - SIGN(0.5_wp, u_w)) * un%data(ji,jj)              + & 
         & (0.5_wp + SIGN(0.5_wp, u_w)) * un%data(ji-1,jj) 
    uu_e = (0.5_wp + SIGN(0.5_wp, u_e)) * un%data(ji,jj)              + & 
         & (0.5_wp - SIGN(0.5_wp, u_e)) * un%data(ji+1,jj) 

       uu_s = (0.5_wp - SIGN(0.5_wp, v_s)) * un%data(ji,jj)              + & 
            & (0.5_wp + SIGN(0.5_wp, v_s)) * un%data(ji,jj-1) 

       uu_n = (0.5_wp + SIGN(0.5_wp, v_n)) * un%data(ji,jj)              + & 
            & (0.5_wp - SIGN(0.5_wp, v_n)) * un%data(ji,jj+1)

    adv = uu_w * u_w * depw - uu_e * u_e * depe + &
          uu_s * v_s * deps - uu_n * v_n * depn
    !end kernel u adv 

    ! -viscosity

    !kernel  u vis 
    dudx_e = (un%data(ji+1,jj) - un%data(ji,  jj)) / un%grid%dx_t(ji+1,jj) * &
             (ht%data(ji+1,jj) + sshn_t%data(ji+1,jj))
    dudx_w = (un%data(ji,  jj) - un%data(ji-1,jj)) / un%grid%dx_t(ji,  jj) * &
             (ht%data(ji,  jj) + sshn_t%data(ji,  jj))

       dudy_s = (un%data(ji,jj) - un%data(ji,jj-1)) / (un%grid%dy_u(ji,jj) + un%grid%dy_u(ji,jj-1)) * &
            & (hu%data(ji,jj) + sshn_u%data(ji,jj) + hu%data(ji,jj-1) + sshn_u%data(ji,jj-1))

       dudy_n = (un%data(ji,jj+1) - un%data(ji,jj)) / (un%grid%dy_u(ji,jj) + un%grid%dy_u(ji,jj+1)) * &
            & (hu%data(ji,jj) + sshn_u%data(ji,jj) + hu%data(ji,jj+1) + sshn_u%data(ji,jj+1))

    vis = (dudx_e - dudx_w ) * un%grid%dy_u(ji,jj)  + &
         & (dudy_n - dudy_s ) * un%grid%dx_u(ji,jj) * 0.5_wp  
    vis = visc * vis   !visc will be an array visc(1:jpijglou) 
    !for variable viscosity, such as turbulent viscosity
    !End  kernel u vis 

    ! -Coriolis' force (can be implemented implicitly)
    !kernel cor 
    cor = 0.5_wp * (2._wp * omega * SIN(un%grid%gphiu(ji,jj) * d2r) * (v_sc + v_nc)) * &
         & un%grid%area_u(ji,jj) * (hu%data(ji,jj) + sshn_u%data(ji,jj))
    !end kernel cor 

    ! -pressure gradient
    !start kernel hpg 
    hpg = -g * (hu%data(ji,jj) + sshn_u%data(ji,jj)) * un%grid%dy_u(ji,jj) * &
           (sshn_t%data(ji+1,jj) - sshn_t%data(ji,jj))
    !end kernel hpg 
    ! -linear bottom friction (implemented implicitly.
    !kernel ua calculation 
    ua%data(ji,jj) = (un%data(ji,jj) * (hu%data(ji,jj) + sshn_u%data(ji,jj)) + rdt * &
                 (adv + vis + cor + hpg) / un%grid%area_u(ji,jj)) / &
                (hu%data(ji,jj) + ssha_u%data(ji,jj)) / (1.0_wp + cbfr * rdt) 

      end do
    end do
 
    !call likwid_markerStopRegion('Momentum-u')
    call timer_stop(idxt)

    call timer_start('Momentum-v',idxt)

!dir$ safe_address
    do jj = 2, N-1, 1
! SIMD
!dir$ vector always
      do ji = 2, M, 1

!!$        call momentum_v_code(ji, jj, &
!!$                             va%data, un%data, vn%data, &
!!$                             hu%data, hv%data, ht%data, &
!!$                             ssha_v%data, sshn_t%data,  &
!!$                             sshn_u%data, sshn_v%data,  &
!!$                             vn%grid%tmask,    &
!!$                             vn%grid%dx_v,     &
!!$                             vn%grid%dx_t,     &
!!$                             vn%grid%dy_u,     &
!!$                             vn%grid%dy_v,     &
!!$                             vn%grid%dy_t,     &
!!$                             vn%grid%area_v,   &
!!$                             vn%grid%gphiv)

    IF(vn%grid%tmask(ji,jj) + vn%grid%tmask(ji+1,jj) <= 0)  cycle !jump over non-computatinal domain
    IF(vn%grid%tmask(ji,jj) <= 0 .OR. vn%grid%tmask(ji,jj+1) <= 0) cycle !jump over v boundary cells

    ! kernel v adv 
    v_n  = 0.5 * (vn%data(ji,jj) + vn%data(ji,jj+1)) * vn%grid%dx_t(ji,jj+1)  !add length scale.
    depn = ht%data(ji,jj+1) + sshn_t%data(ji,jj+1)

    v_s  = 0.5 * (vn%data(ji,jj) + vn%data(ji,jj-1)) * vn%grid%dx_t(ji,jj)    !add length scale
    deps = ht%data(ji,jj) + sshn_t%data(ji,jj)

    u_wc = 0.5_wp * (un%data(ji-1,jj) + un%data(ji-1,jj+1))
    u_w  = 0.5_wp * u_wc * (vn%grid%dy_u(ji-1,jj) + vn%grid%dy_u(ji-1,jj+1))
    depw = 0.50_wp * (hu%data(ji-1,jj) + sshn_u%data(ji-1,jj) + &
                      hu%data(ji-1,jj+1) + sshn_u%data(ji-1,jj+1))

    u_ec = 0.5_wp * (un%data(ji,jj) + un%data(ji,jj+1))
    u_e  = 0.5_wp * u_ec * (vn%grid%dy_u(ji,jj) + vn%grid%dy_u(ji,jj+1))
    depe = 0.50_wp * (hu%data(ji,jj) + sshn_u%data(ji,jj) + &
                      hu%data(ji,jj+1) + sshn_u%data(ji,jj+1))

    ! -advection (currently first order upwind)
    vv_s = (0.5_wp - SIGN(0.5_wp, v_s)) * vn%data(ji,jj)     + & 
         & (0.5_wp + SIGN(0.5_wp, v_s)) * vn%data(ji,jj-1) 
    vv_n = (0.5_wp + SIGN(0.5_wp, v_n)) * vn%data(ji,jj)     + & 
         & (0.5_wp - SIGN(0.5_wp, v_n)) * vn%data(ji,jj+1) 

    IF(vn%grid%tmask(ji-1,jj) <= 0 .OR. vn%grid%tmask(ji-1,jj+1) <= 0) THEN   
       vv_w = (0.5_wp - SIGN(0.5_wp, u_w)) * vn%data(ji,jj)  
    ELSE
       vv_w = (0.5_wp - SIGN(0.5_wp, u_w)) * vn%data(ji,jj)    + & 
            & (0.5_wp + SIGN(0.5_wp, u_w)) * vn%data(ji-1,jj) 
    END If

    IF(vn%grid%tmask(ji+1,jj) <= 0 .OR. vn%grid%tmask(ji+1,jj+1) <= 0) THEN
       vv_e = (0.5_wp + SIGN(0.5_wp, u_e)) * vn%data(ji,jj)
    ELSE
       vv_e = (0.5_wp + SIGN(0.5_wp, u_e)) * vn%data(ji,jj)  + & 
              (0.5_wp - SIGN(0.5_wp, u_e)) * vn%data(ji+1,jj)
    END IF

    adv = vv_w * u_w * depw - vv_e * u_e * depe + &
          vv_s * v_s * deps - vv_n * v_n * depn

    !end kernel v adv 

    ! -viscosity

    
    !kernel v dis 
    dvdy_n = (vn%data(ji,jj+1) - vn%data(ji,  jj)) / vn%grid%dy_t(ji,jj+1) * &
                          (ht%data(ji,jj+1) + sshn_t%data(ji,jj+1))
    dvdy_s = (vn%data(ji,  jj) - vn%data(ji,jj-1)) / vn%grid%dy_t(ji,  jj) * &
                          (ht%data(ji,  jj) + sshn_t%data(ji,  jj))

    IF(vn%grid%tmask(ji-1,jj) <= 0 .OR. vn%grid%tmask(ji-1,jj+1) <= 0) THEN
       dvdx_w = 0.0_wp !slip boundary
    ELSE
       dvdx_w = (vn%data(ji,jj) - vn%data(ji-1,jj)) / &
                (vn%grid%dx_v(ji,jj) + vn%grid%dx_v(ji-1,jj)) * &
                (hv%data(ji,jj) + sshn_v%data(ji,jj) + hv%data(ji-1,jj) + sshn_v%data(ji-1,jj))
    END IF

    IF(vn%grid%tmask(ji+1,jj) <= 0 .OR. vn%grid%tmask(ji+1,jj+1) <= 0) THEN
       dvdx_e = 0.0_wp ! slip boundary
    ELSE
       dvdx_e = (vn%data(ji+1,jj) - vn%data(ji,jj)) / (vn%grid%dx_v(ji,jj) + vn%grid%dx_v(ji+1,jj)) * &
                  (hv%data(ji,jj) + sshn_v%data(ji,jj) + hv%data(ji+1,jj) + sshn_v%data(ji+1,jj))
    END If

    vis = (dvdy_n - dvdy_s ) * vn%grid%dx_v(ji,jj)  + &
          (dvdx_e - dvdx_w ) * vn%grid%dy_v(ji,jj) * 0.5_wp  

    vis = visc * vis   !visc will be a array visc(1:jpijglou) 
    !for variable viscosity, such as turbulent viscosity
    !end kernel v dis 

    ! -Coriolis' force (can be implemented implicitly)
    !kernel v cor 
    cor = -0.5_wp*(2._wp * omega * SIN(vn%grid%gphiv(ji,jj) * d2r) * (u_ec + u_wc)) * &
               vn%grid%area_v(ji,jj) * (hv%data(ji,jj) + sshn_v%data(ji,jj))
    !end kernel v cor 

    ! -pressure gradient
    !kernel v hpg 
    hpg = -g * (hv%data(ji,jj) + sshn_v%data(ji,jj)) * vn%grid%dx_v(ji,jj) * &
           (sshn_t%data(ji,jj+1) - sshn_t%data(ji,jj))
    !kernel v hpg 

    ! -linear bottom friction (implemented implicitly.
    !kernel ua calculation 
    va%data(ji,jj) = (vn%data(ji,jj) * (hv%data(ji,jj) + sshn_v%data(ji,jj)) + &
                 rdt * (adv + vis + cor + hpg) / vn%grid%area_v(ji,jj) ) / &
                 ((hv%data(ji,jj) + ssha_v%data(ji,jj))) / (1.0_wp + cbfr * rdt) 

      end do
    end do

    call timer_stop(idxt)

    ! Apply open and solid boundary conditions

    call timer_start('BCs', idxt)

!    DO jj = ssha%internal%ystart, ssha%internal%ystop 
!       DO ji = ssha%internal%xstart, ssha%internal%xstop 
    DO jj = 2, N
! SIMD
       DO ji = 2, M
!          call bc_ssh_code(ji, jj, &
!                           istp, ssha%data, sshn_t%grid%tmask)

          amp_tide   = 0.2_wp
          omega_tide = 2.0_wp * 3.14159_wp / (12.42_wp * 3600._wp)
          rtime = istp * rdt

          if(sshn_t%grid%tmask(ji,jj) <= 0) cycle

          IF     (sshn_t%grid%tmask(ji,jj-1) < 0) THEN
             ssha%data(ji,jj) = amp_tide * sin(omega_tide * rtime)
          ELSE IF(sshn_t%grid%tmask(ji,jj+1) < 0) THEN
             ssha%data(ji,jj) = amp_tide * sin(omega_tide * rtime)
          ELSE IF(sshn_t%grid%tmask(ji+1,jj) < 0) THEN
             ssha%data(ji,jj) = amp_tide * sin(omega_tide * rtime)
          ELSE IF(sshn_t%grid%tmask(ji-1,jj) < 0) THEN
             ssha%data(ji,jj) = amp_tide * sin(omega_tide * rtime)
          END IF

       END DO
    END DO


!    do jj = uwhole_ystart, uwhole_ystop, 1
!       do ji = uwhole_xstart, uwhole_xstop, 1
!dir$ safe_address
    do jj = 1, N+1, 1
       do ji = 1, M, 1
!          call bc_solid_u_code(ji, jj, &
!                               ua%data, va%grid%tmask)

          if(sshn_t%grid%tmask(ji,jj) * sshn_t%grid%tmask(ji+1,jj) == 0)then
             ua%data(ji,jj) = 0._wp
          end if

       end do
    end do

!    DO jj = va%whole%ystart, va%whole%ystop, 1 
!       DO ji = va%whole%xstart, va%whole%xstop, 1
!    do jj = vwhole_ystart, vwhole_ystop, 1
!       do ji = vwhole_xstart, vwhole_xstop, 1
!dir$ safe_address
    do jj = 1, N, 1
       do ji = 1, M+1, 1
!          call bc_solid_v_code(ji,jj, &
!                               va%data, ua%grid%tmask)
    if(sshn_t%grid%tmask(ji,jj) * sshn_t%grid%tmask(ji,jj+1) == 0)then
       va%data(ji,jj) = 0._wp
    end if

      end do
    end do

!    DO jj = uwhole_ystart, uwhole_ystop, 1
!       DO ji = uwhole_xstart, uwhole_xstop, 1
!dir$ safe_address
    DO jj = 1, N+1, 1
       DO ji = 1, M, 1
!          call bc_flather_u_code(ji,jj, &
!                                 ua%data, hu%data, sshn_u%data, &
!                                 sshn_u%grid%tmask)
          ! Check whether this point lies within the domain
          if(sshn_t%grid%tmask(ji,jj) + sshn_t%grid%tmask(ji+1,jj) <= -1) cycle

          if(sshn_t%grid%tmask(ji,jj) < 0) then
             jiu = ji + 1
             ua%data(ji,jj) = ua%data(jiu,jj) + sqrt(g/hu%data(ji,jj))* &
                  (sshn_u%data(ji,jj) - sshn_u%data(jiu,jj))
          else if(sshn_t%grid%tmask(ji+1,jj )< 0) then
             jiu = ji - 1 
             ua%data(ji,jj) = ua%data(jiu,jj) + sqrt(g/hu%data(ji,jj)) * &
                  (sshn_u%data(ji,jj) - sshn_u%data(jiu,jj))
          end if
       END DO
    END DO

!    DO jj = va%whole%ystart, va%whole%ystop, 1 
!       DO ji = va%whole%xstart, va%whole%xstop, 1
!     DO jj = vwhole_ystart, vwhole_ystop, 1
!       DO ji = vwhole_xstart, vwhole_xstop, 1
!dir$ safe_address
     DO jj = 1, N, 1
       DO ji = 1, M+1, 1
!          call bc_flather_v_code(ji,jj, &
!                                 va%data, hv%data, sshn_v%data, &
!                                 sshn_v%grid%tmask)
          IF(sshn_t%grid%tmask(ji,jj) + sshn_t%grid%tmask(ji,jj+1) <= -1) cycle
    
          IF(sshn_t%grid%tmask(ji,jj) < 0) THEN
             jiv = jj + 1
             va%data(ji,jj) = va%data(ji,jiv) + SQRT(g/hv%data(ji,jj)) * &
                  (sshn_v%data(ji,jj) - sshn_v%data(ji,jiv))
          ELSE IF(sshn_t%grid%tmask(ji,jj+1) < 0) THEN
             jiv = jj - 1 
             va%data(ji,jj) = va%data(ji,jiv) + SQRT(g/hv%data(ji,jj)) * &
                  (sshn_v%data(ji,jj) - sshn_v%data(ji,jiv))
          END IF

       END DO
    END DO

    call timer_stop(idxt)

    ! Time update of fields

    call timer_start('Next', idxt)

!    call copy_field(ua, un)
!    call copy_field(va, vn)
!    call copy_field(ssha, sshn_t)
    un%data = ua%data
    vn%data = va%data
    sshn_t%data = ssha%data

!dir$ safe_address
    do jj = 2, N, 1
!dir$ vector always
      do ji = 2, M-1, 1

!         call next_sshu_code(ji, jj, sshn_u%data, sshn_t%data, &
!                            sshn_t%grid%tmask,                 &
!                            sshn_t%grid%area_t, sshn_t%grid%area_u)

         if(sshn_t%grid%tmask(ji,jj) + &
            sshn_t%grid%tmask(ji+1,jj) <= 0) cycle !jump over non-computational domain

         IF(sshn_t%grid%tmask(ji,jj) * sshn_t%grid%tmask(ji+1,jj) > 0) THEN
            rtmp1 = sshn_t%grid%area_t(ji,jj) * sshn_t%data(ji,jj) + &
                 sshn_t%grid%area_t(ji+1,jj) * sshn_t%data(ji+1,jj)
            sshn_u%data(ji,jj) = 0.5_wp * rtmp1 / sshn_t%grid%area_u(ji,jj) 
         ELSE IF(sshn_t%grid%tmask(ji,jj) <= 0) THEN
            sshn_u%data(ji,jj) = sshn_t%data(ji+1,jj)
         ELSE IF(sshn_t%grid%tmask(ji+1,jj) <= 0) THEN
            sshn_u%data(ji,jj) = sshn_t%data(ji,jj)
         END IF

      end do
    end do

!dir$ safe_address
    do jj = 2, N-1, 1
!dir$ vector always
      do ji = 2, M, 1

!        call next_sshv_code(ji, jj,                   &
!                            sshn_v%data, sshn_t%data, &
!                            sshn_t%grid%tmask,        &
!                            sshn_t%grid%area_t, sshn_t%grid%area_v)
 
         if(sshn_t%grid%tmask(ji,jj) + &
            sshn_t%grid%tmask(ji,jj+1) <= 0)  cycle !jump over non-computational domain
         if(sshn_t%grid%tmask(ji,jj) * sshn_t%grid%tmask(ji,jj+1) > 0) then
            rtmp1 = sshn_t%grid%area_t(ji,jj)*sshn_t%data(ji,jj) + &
                 sshn_t%grid%area_t(ji,jj+1) * sshn_t%data(ji,jj+1)
            sshn_v%data(ji,jj) = 0.5_wp * rtmp1 / sshn_t%grid%area_v(ji,jj) 
         else if(sshn_t%grid%tmask(ji,jj) <= 0) then
            sshn_v%data(ji,jj) = sshn_t%data(ji,jj+1)
         else if(sshn_t%grid%tmask(ji,jj+1) <= 0) then
            sshn_v%data(ji,jj) = sshn_t%data(ji,jj)
         end if
      end do
    end do

    call timer_stop(idxt)

  end subroutine invoke_time_step

  !=================================================================

  subroutine invoke_continuity_arrays(nx, ny, M, N, rdt, ssha, &
                                      sshn_t, sshn_u, sshn_v, &
                                      hu, hv, un, vn, area_t)
    use global_parameters_mod, only: ALIGNMENT
    use kind_params_mod
    use dl_timer
    implicit none
    integer, intent(in) :: nx, ny, M, N
    real(wp), intent(in) :: rdt
    real(wp), intent(out) :: ssha(nx,ny)
    real(wp), intent(in)  :: sshn_u(nx,ny), sshn_v(nx,ny), sshn_t(nx,ny)
    real(wp), intent(in)  :: un(nx,ny), vn(nx,ny)
    real(wp), intent(in)  :: hu(nx,ny), hv(nx,ny), area_t(nx,ny)
    ! Locals
    integer :: jj, ji, idxt
    real(wp) :: rtmp1, rtmp2, rtmp3, rtmp4
    integer :: nrepeat, ic
!DIR$ ASSUME (MOD(NX,ALIGNMENT) .EQ. 0)
!DIR$ ASSUME (MOD(M,ALIGNMENT) .EQ. 0)
!DIR$ ASSUME_ALIGNED ssha:64, sshn_u:64, sshn_v:64, sshn_t:64
!DIR$ ASSUME_ALIGNED un:64, vn:64, hu:64, hv:64, area_t:64

    nrepeat = 1

    call timer_start('Continuity',idxt,nrepeat)
    !call likwid_markerStartRegion('Continuity')
!DIR$ VECTOR ALIGNED
    do jj = 2, N, 1

      do ji = ALIGNMENT+1, M, 1

         rtmp1 = (sshn_u(ji  ,jj ) + hu(ji  ,jj  ))*un(ji  ,jj)
         rtmp2 = (sshn_u(ji-1,jj ) + hu(ji-1,jj))*un(ji-1,jj)
         rtmp3 = (sshn_v(ji ,jj )  + hv(ji  ,jj  ))*vn(ji ,jj)
         rtmp4 = (sshn_v(ji ,jj-1) + hv(ji ,jj-1))*vn(ji,jj-1)

         ssha(ji,jj) = sshn_t(ji,jj) + (rtmp2 - rtmp1 + rtmp4 - rtmp3) * &
                       rdt / area_t(ji,jj)
      end do
      
    end do
    !call likwid_markerStopRegion('Continuity')
    call timer_stop(idxt)

  end subroutine invoke_continuity_arrays

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
