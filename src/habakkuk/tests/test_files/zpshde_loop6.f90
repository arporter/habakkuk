subroutine zps_hde()

  DO jj = 1, jpjm1
     DO ji = 1, jpim1
        iku = miku(ji,jj) ; ikup1 = miku(ji,jj) + 1
        ikv = mikv(ji,jj) ; ikvp1 = mikv(ji,jj) + 1
        ze3wu  = (gdepw_0(ji+1,jj,iku+1) - gdept_0(ji+1,jj,iku)) - (gdepw_0(ji,jj,iku+1) - gdept_0(ji,jj,iku))
        ze3wv  = (gdepw_0(ji,jj+1,ikv+1) - gdept_0(ji,jj+1,ikv)) - (gdepw_0(ji,jj,ikv+1) - gdept_0(ji,jj,ikv))

        pgzui  (ji,jj) = (gdep3w_0(ji+1,jj,iku) + ze3wu) - gdep3w_0(ji,jj,iku)
        pgrui  (ji,jj) = umask(ji,jj,iku)   * ( zri(ji,jj) - prd(ji,jj,iku) )          ! i: 1
        pmrui  (ji,jj) = umask(ji,jj,iku)   * ( zri(ji,jj) + prd(ji,jj,iku) )          ! i: 1 
        pge3rui(ji,jj) = umask(ji,jj,iku+1)                                                                  &
             * ( (e3w_0(ji+1,jj,iku+1) - ze3wu) * (zri(ji,jj    ) + prd(ji+1,jj,iku+1) + 2._wp)   &
             - e3w_0(ji  ,jj,iku+1)          * (prd(ji,jj,iku) + prd(ji  ,jj,iku+1) + 2._wp)   ) ! i: 1
        pgzvi  (ji,jj) = (gdep3w_0(ji,jj+1,ikv) + ze3wv) - gdep3w_0(ji,jj,ikv) 
        pgrvi  (ji,jj) = vmask(ji,jj,ikv)   * ( zrj(ji,jj  ) - prd(ji,jj,ikv) )        ! j: 1
        pmrvi  (ji,jj) = vmask(ji,jj,ikv)   * ( zrj(ji,jj  ) + prd(ji,jj,ikv) )        ! j: 1
        pge3rvi(ji,jj) = vmask(ji,jj,ikv+1)                                                                  & 
             * ( (e3w_0(ji,jj+1,ikv+1) - ze3wv) * ( zrj(ji,jj    ) + prd(ji,jj+1,ikv+1) + 2._wp)  &
             - e3w_0(ji,jj  ,ikv+1)          * ( prd(ji,jj,ikv) + prd(ji,jj  ,ikv+1) + 2._wp)  ) ! j: 1
        ! + 2 due to the formulation in density and not in anomalie in hpg sco
     END DO
  END DO

end subroutine zps_hde
