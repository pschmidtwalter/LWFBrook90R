! This module contains the constant parameters for the LWFBrook90R model
! All the parameters are ordered in the alphabetical ordered
! V.Trotsiuk [volodymyr.trostiuk@wsl.ch]

module mod_decl_const
    
    implicit none

    real(kind=8), parameter :: CPRHO = 1240.d0     !CPRHO - volumetric heat capacity of air, J m-3 K-1)
    real(kind=8), parameter :: CVLQ = .00418d0     ! CVLQ - volumetric heat capacity of water, MJ m-2 mm-1 K-1
    real(kind=8), parameter :: CVICE = .00192d0    ! CVICE - volumetric heat capacity of ice, MJ m-2 mm-1 K-1
    integer, dimension(12), parameter :: DAYMO = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/) ! day of the 
    real(kind=8), parameter :: DT = 1.d0           ! DT  time step for DFILE interval,  must be 1 d
    real(kind=8), parameter :: DTIMIN = 1.e-9       ! minimum time step for iteration interval, d
    real(kind=8), parameter :: ETOM = .4085d0      ! ETOM  (mm water)/(MJ/m2) using Lv 2448 MJ/Mg and density of water 1 Mg/m3
    real(kind=8), parameter :: GAMMA = .067d0      ! GAMMA - psychrometer constant, kPa/K
    real(kind=8), parameter :: K = .4d0            ! K  vonKarman constant
    real(kind=8), parameter :: LF = .335d0         ! LF heat of fusion of water, MJ m-2 mm-1
    real(kind=8), parameter :: LS = 2.824d0        ! LS  latent heat of sublimation of snow, MJ m-2 mm-1
    real(kind=8), parameter :: PI = 3.1416d0       ! PI  pi
    real(kind=8), parameter :: RHOWG = .00981d0    ! RHOWG  density of water times gravity acceleration, MPa/m or kPa/mm
    real(kind=8), parameter :: SIGMA = 5.67E-08     ! SIGMA  Stefan-Boltzmann constant, W m-2 K-4)
    real(kind=8), parameter :: SC = 1367.d0        ! SC  solar constant, value from Lean (1991), W/m2
    real(kind=8), parameter :: ThCrit = 1.e-4       ! * minimal fraction of water content above residual water content to allow water supply for evapotranspiration
    real(kind=8), parameter :: WTOMJ = .0864d0     ! WTOMJ  (MJ m-2 d-1)/(watt/m2) = 86400 s/d * .000001 MJ/J
    real(kind=8), parameter :: zeroCurRange = 1.d0 ! near zero degree Celsius caused by the latent heat of fusion and thawing, see for example Boike et al. 1998
    real(kind=8), parameter :: zeroCurTemp = 0.d0  ! zeroCurTemp temperature for the so-called zero curtain

end module mod_decl_const