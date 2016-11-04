   REAL(KIND=wp) FUNCTION fspott( pft, pfs, pfp )

      !! * Arguments
      REAL(KIND=wp) :: pft   ! in situ temperature in degrees celcius
      REAL(KIND=wp) :: pfs   ! salinity in psu
      REAL(KIND=wp) :: pfp   ! pressure in bars

      fspott = pfp*(8.9e-7 + pft*(-3.1e-8 + pft*2.1e-10)-(pfs - 35)*4.1e-9)

   END FUNCTION fspott
