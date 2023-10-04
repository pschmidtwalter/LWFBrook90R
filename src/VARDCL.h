!     ***************************************************************************
!VARLST.INC - alphabetic list of variables for B90
!     will be used for declarations in Fortran
!
!  * indicates a parameter input in PFILE.DAT
! ** indicates a variable in DFILE.DAT or PRFILE.DAT
!*** indicates a constant in CONSTANT.INC
!     **** indicates a variable initialized or parameter set in DFILE.DAT
!  & indicates a variable initialized in PFILE.DAT
!  @ indicates a calculated parameter (constant for run)
!    all others are variables
!     xxxxPflow for precipitation interval, mm
!     xxxxDflow for day, mm
!     xxxxMflow for month, mm
!     xxxxYflow for year, mm

integer, parameter :: ML = 1000, MPar= 10, MMat=20, Reset=1
real(kind=c_double) :: B0              !,B1,B2,B3,B4,B5     ! buffer
!real(kind=c_double) :: A0,A1           !,A2,A3,A4,A5     ! buffer
real(kind=c_double) :: AA(2)           ! average available energy over daytime or nighttime, W/m2
real(kind=c_double) :: ADEF            ! available water deficit in root zone, mm, output only
real(kind=c_double) :: ALB             ! * albedo with no snow
real(kind=c_double) :: ALBEDO          ! albedo
real(kind=c_double) :: ALBSN           ! * albedo with snow on the ground
real(kind=c_double) :: ALPHA(ML)       ! modified Cowan alpha, MPa
real(kind=c_double) :: age             ! * age of vegetation
real(kind=c_double) :: ASPECT          ! * aspect, degrees through east from north
real(kind=c_double) :: ASUBS(2)        ! average avail. energy at ground over day or night, W/m2
real(kind=c_double) :: ATR(2)          ! actual transpiration rate for daytime or night, mm/d
real(kind=c_double) :: ATRANI(ML)      ! actual transp.rate from layer for daytime or night,mm/d
real(kind=c_double) :: ATRI(2,ML)      ! actual transp.rate from layer for daytime and night,mm/d
real(kind=c_double) :: AWAT            ! available soil water in root zone, mm, output only
real(kind=c_double) :: AWATFK          ! available soil water in root zone at field cap., mm, output only
! real(kind=c_double) :: AWATM           ! average monthly AWAT
real(kind=c_double) :: AWAT40          ! available soil water til 40 cm, mm, output only
real(kind=c_double) :: BALERD          ! BALERM,,BALERY error in water balance. mm
real(kind=c_double) :: BYFLI(ML)       ! bypass flow rate from layer, mm/d
real(kind=c_double) :: BYFLPI(ML), BYFLDI(ML) !, BYFLMI(ML), BYFLYI(ML)
real(kind=c_double) :: BYFLP, BYFLD    !, BYFLM, BYFLY bypass flow
real(kind=c_double) :: BYFRAC(ML)      ! fraction of layer infiltration to bypass flow
integer :: BYPAR                ! * 1 to allow BYFL, or 0 to prevent BYFL
real(kind=c_double) :: C1              !* intercept of relation of solar rad. to sunshine duration
real(kind=c_double) :: C2              ! * slope of relation of solar rad. to sunshine duration
real(kind=c_double) :: C3              ! * longwave correction factor for overcast sky
real(kind=c_double) :: CC              ! cold content of snowpack (positive), MJ m-2
real(kind=c_double) :: CCFAC           ! * cold content factor, MJ m-2 d-1 K-1
real(kind=c_double) :: CINTRL          ! * maximum interception storage of rain per unit LAI, mm
real(kind=c_double) :: CINTRS          ! * maximum interception storage of rain per unit SAI, mm
real(kind=c_double) :: CINTSL          ! * maximum interception storage of snow per unit LAI, mm
real(kind=c_double) :: CINTSS          ! * maximum interception storage of snow per unit SAI, mm
real(kind=c_double) :: CR              ! * light extinction coefficient for projected LAI + SAI
real(kind=c_double) :: CS              ! * ratio of projected SAI to canopy height, m-1
real(kind=c_double) :: CVPD            !* vapor pressure deficit at which conductance is halved,kPa
real(kind=c_double) :: CZR             ! * ratio of roughness to HEIGHT for rough closed canopies
real(kind=c_double) :: CZS             ! * ratio of roughness to HEIGHT for smooth closed canopies
real(kind=c_double) :: DAYLEN          ! daylength in fraction of day, d-1
integer, dimension(12) :: DAYMO ! * days in month
integer :: DD                   ! ** day of the month
real(kind=c_double) :: DELTA           ! dES/dT at some temperature T, kPa/K
real(kind=c_double) :: dep(ML)         ! soil depth [m]
real(kind=c_double) :: depmax          ! depth [m] of soil surface
real(kind=c_double) :: DENSEF          ! * density factor for MAXLAI,CS,RTLEN,RPLANT, not <.001
real(kind=c_double) :: DISP            ! zero-plane displacement, m
real(kind=c_double) :: DISPC           ! zero-plane displacement for closed canopy of HEIGHT, m
integer :: DOM                  ! day of month
integer :: DOY                  ! **** first day of the year in DFILE
real(kind=c_double) :: DPSIDW(ML)      !rate of change of total potential with watercontent,kPa/mm
real(kind=c_double) :: DPSIMX          ! * maximum potential difference considered equal, kPa
real(kind=c_double) :: DRAIN           ! * multiplier of VFLUX(n) for drainage to groundwater
real(kind=c_double) :: DSFLI(ML)       ! downslope flow rate from layer, mm/d
real(kind=c_double) :: DSFLP, DSFLD    !, DSFLM, DSFLY downslope flow
real(kind=c_double) :: DSFLPI(ML), DSFLDI(ML) !, DSFLMI(ML), DSFLYI(ML)
real(kind=c_double) :: DSLOPE          ! * slope for DSFL, degrees
real(kind=c_double) :: DSWMAX          ! * maximum change allowed in SWATI, percent of SWATMX(i)
real(kind=c_double) :: DTI             ! time step for iteration interval, d
real(kind=c_double) :: DTIMAX          ! * maximum iteration time step, d
real(kind=c_double) :: DTINEW          ! second estimate of DTI
real(kind=c_double) :: DTP             ! @ time step for precipitation interval, may be <= 1 d
real(kind=c_double) :: DTRI            ! time remaining in precipitation interval, d
real(kind=c_double) :: DUMM(ML)        ! dummy array for subroutine calls
real(kind=c_double) :: DUMMY           ! dummy variable for subroutine calls
real(kind=c_double), dimension(12) :: DURATN ! * average duration of daily precip by month, hr
real(kind=c_double) :: EA              ! ** vapor pressure for the day, kPa
real(kind=c_double) :: ES              ! saturated vapor pressure, kPa
real(kind=c_double) :: ESLOPE          ! * slope for evapotranspiration and snowmelt, degrees
real(kind=c_double) :: Eta             ! volumetric air content
!real(kind=c_double) :: ETDiff          ! Evapotranspirationdifferenz
real(kind=c_double) :: EVAPP, EVAPD    ! , EVAPM, EVAPY, EVAPVP evapotranspiration
real(kind=c_double) :: FETCH           ! * weather station fetch, m"
real(kind=c_double) :: FLOWP, FLOWD    ! , , FLOWM, FLOWY FLOWVPtotal flow
real(kind=c_double) :: FRINTL          ! * intercepted fraction of rain per unit LAI
real(kind=c_double) :: FRINTS          ! * intercepted fraction of rain per unit SAI
real(kind=c_double) :: frelden(ML)     ! * relative values of final root density per unit volume
real(kind=c_double) :: FSINTL          ! * intercepted fraction of snow per unit LAI
real(kind=c_double) :: FSINTS          ! * intercepted fraction of snow per unit SAI
real(kind=c_double) :: FXYLEM          ! * fraction of plant resistance in xylem
real(kind=c_double) :: PGER(2)         ! ground evaporation rate for daytime or night, mm/d
real(kind=c_double) :: GER(2)          ! ground evaporation rate for daytime or night, mm/d
real(kind=c_double) :: GEVP            ! average ground evaporation for day, mm/d
real(kind=c_double) :: GIR(2)          !ground evap. rate with intercep. for daytime or night,mm/d
real(kind=c_double) :: GIVP            !average ground evaporation for day with interception, mm/d
real(kind=c_double) :: GLMAX           ! * maximum leaf conductance, m/s
real(kind=c_double) :: GLMIN           ! * minimum leaf conductance, m/s
real(kind=c_double) :: GRDMLT          ! * rate of groundmelt of snowpack, mm/d
real(kind=c_double) :: GSC             ! * discharge from GWAT, fraction per day, d-1
real(kind=c_double) :: GSP             ! * fraction of discharge to seepage
real(kind=c_double) :: GWAT            ! **** groundwater storage below soil layers, mm
real(kind=c_double) :: GWFL            ! streamflow rate from groundwater, mm/d
real(kind=c_double) :: GWFLP, GWFLD    ! , GWFLM, GWFLYgroundwater flow
double precision   HA(ML), HB(ML), HC(ML), HD(ML) ! Buffers for heat flow
integer :: HEAT                 !  switch to include (1) or to exclude (0) soil heat flow
real(kind=c_double) :: HeatCapNew(ML), HeatCapOld(ML) ! Volumetric heat capacity of layer [J m-3 K-1]
real(kind=c_double) :: HEIGHT          ! canopy height, m
real(kind=c_double) :: HR              ! * height above which CZR applies, m
real(kind=c_double) :: HS              ! * height below which CZS applies, m
integer :: i1, i2,ifoundfirst
integer :: I, II                ! index variable for layer number
real(kind=c_double) :: I0HDAY          ! potential insolation on horizontal, MJ m-2 d-1
integer :: IDAY                 ! day number in run
integer :: ILAYER               ! * number of layers over which infiltration is distributed
integer :: imodel               ! * parameterization of hydraulic functions
real(kind=c_double) :: IMPERV          ! * impervious fraction of area for SRFL
real(kind=c_double) :: INFEXP          ! * infiltration exponent, 0-all to top to 1-uniform
real(kind=c_double) :: inFil           ! infiltration [mm] for heat flow
real(kind=c_double) :: INFRAC(ML)      ! @ fraction of infiltration to each layer
real(kind=c_double) :: INFLI(ML)       ! infiltration rate into layer, mm/d
real(kind=c_double) :: INFLP, INFLD    ! , INFLM, INFLY infiltration into soil matrix, mm
real(kind=c_double) :: INFLPI(ML), INFLDI(ML) !, INFLMI(ML), INFLYI(ML)
real(kind=c_double) :: inirdep         ! initial root depth, e.g. at planting [m]
real(kind=c_double) :: inirlen         ! initial total root length, e.g. at planting [m m-2]
real(kind=c_double) :: INTR            ! & intercepted rain, mm
real(kind=c_double) :: INTS            ! & intercepted snow, mm
real(kind=c_double) :: IRVP            ! evaporation rate of intercepted rain, mm/d
real(kind=c_double) :: IRVPD           !, IRVPM, IRVPY, IRVPVP ! evaporation of intercepted rain, mm
real(kind=c_double) :: ISVP            ! evaporation rate of intercepted snow, mm/d
real(kind=c_double) :: ISVPD           !, ISVPM, ISVPY, ISVPVP ! evaporation of intercepted snow, mm
integer :: J, JJ                ! index variable for day-night separation
real(kind=c_double) :: KK(ML)          ! hydraulic conductivity, mm/d
real(kind=c_double) :: KSNVP           ! * multiplier to fix snow evaporation problem
real(kind=c_double) :: L1              ! @ latitude of equivalent slope, radians
real(kind=c_double) :: L2              ! @ time shift of equivalent slope, radians
real(kind=c_double) :: LAI             ! leaf area index, m2/m2
real(kind=c_double) :: LAIMLT          ! * parameter for snowmelt dependence on LAI,dimensionless
real(kind=c_double) :: LAT             ! **** latitude, degrees
real(kind=c_double) :: LENGTH          ! * slope length for DSFL, m
real(kind=c_double) :: LPC             ! * minimum LAI defining a closed canopy
real(kind=c_double) :: LNGNET(2)       ! Net Longwave radiation for day and night, W m-2
real(kind=c_double) :: LWIDTH          ! * leaf width, m
integer :: mat(ML)              ! * material index for layer
real(kind=c_double) :: MAXLQF          ! * maximum liquid water fraction of SNOW, dimensionless
real(kind=c_double) :: MELFAC          ! * degree day melt factor for open, MJ m-2 d-1 K-1
real(kind=c_double) :: MESFL           ! ** measured streamflow for day, mm
real(kind=c_double) :: MESFLD          !, MESFLM, MESFLY, MESFLVP ! measured streamflow, mm
real(kind=c_double) :: MESFLP          ! ** measured streamflow rate for precip interval, mm/d
integer :: MM                   ! ** month
integer :: MONTH                ! month number
real(kind=c_double) :: MUE(ML)
real(kind=c_double) :: MXKPL           ! * maximum plant conductivity, (mm/d)/MPa
real(kind=c_double) :: MXRTLN          ! * maximum root length per unit land area, m/m2
integer :: N                    ! index variable for precipitation interval
real(kind=c_double) :: NN              ! * wind/diffusivity extinction coefficient
integer :: NDAYS                ! * number of days in run
integer :: NITS                 ! number of iterations in precipitation interval
integer :: NITSD                ! total number of iterations for day
integer :: NITSM                ! total number of iterations for month
integer :: NITSY                ! total number of iterations for year
integer :: NLAYER               ! * number of soil layers to be used in model, <= ML
integer :: nmat                 ! * number of materials for profile
integer :: NOOUTF               ! * 1 if no outflow allowed from roots, otherwise 0
integer :: NPINT                ! **** number of precipitation intervals per day
real(kind=c_double) :: NTFLI(ML)       ! net flow rate into layer, mm/d
real(kind=c_double) :: NTFLPI(ML), NTFLDI(ML) !, NTFLMI(ML), NTFLYI(ML)
! integer :: OP(10,5)             ! * output file selections
real(kind=c_double) :: ParMat(MPar,MMat) ! hydraulic parameters for material
real(kind=c_double), allocatable, dimension(:,:) :: Par    ! hydraulic parameters for layer
real(kind=c_double) :: PINT            ! average potential interception for day, mm/d
real(kind=c_double) :: PINTD           !, PINTM, PINTY, PINTVP ! potential interception, mm
real(kind=c_double) :: PIR(2)          ! potential interception rate for daytime or night, mm/d
real(kind=c_double) :: PREC            ! precipitation rate, mm/d
real(kind=c_double) :: PRECD           !, PRECM, PRECY ! precipitation, mm
real(kind=c_double) :: PREINT          ! ** precipitation for precipitation interval, mm
real(kind=c_double) :: PSICR           ! * minimum plant leaf water potential, MPa
real(kind=c_double) :: PsiCrit(ML)     ! * minimum soil matric potential to allow water supply for evapotranspiration, Pa
real(kind=c_double) :: PSIG(ML)        ! @ gravity potential, kPa
real(kind=c_double) :: PSIM(ML)        ! & matric soil water potential for layer, kPa
real(kind=c_double) :: PSITI(ML)       ! total potential, kPa
real(kind=c_double) :: PSLVP           ! potential evaporation rate from soil, mm/d
real(kind=c_double) :: PSLVPD          !, PSLVPM, PSLVPY, PSLVPVP ! potential soil evaporation, mm
real(kind=c_double) :: PSNVP           ! potential snow evaporation, mm/d
real(kind=c_double) :: PTR(2)          ! potential transpiration rate for daytime or night, mm/d
real(kind=c_double) :: PTRAN           ! average potential transpiration rate for day, mm/d
real(kind=c_double) :: PTRAND          !, PTRANM, PTRANY, PTRANVP ! potential transpiration, mm
real(kind=c_double) :: QFFC            ! * quick flow fraction (SRFL or BYFL) at field capacity
real(kind=c_double) :: QFPAR           ! * quick flow parameter (SRFL or BYFL)
integer :: QLAYER               ! * number of soil layers for SRFL, 0 to prevent SRFL
real(kind=c_double) :: R5              ! * solar radiation at which conductance is halved, W/m2
real(kind=c_double) :: RAA             ! Shuttleworth-Wallace atmosphere aerodynamic resistance,s/m
real(kind=c_double) :: RAC             ! Shuttleworth-Wallace canopy aerodynamic resistance, s/m
real(kind=c_double) :: RAS             ! Shuttleworth-Wallace ground aerodynamic resistance, s/m
real(kind=c_double) :: RELAWAT         ! relative AWAT : AWAT/AWATFK
real(kind=c_double) :: RELDEN(ML)      ! * relative values of root density per unit volume
real(kind=c_double) :: RFAL            ! rainfall rate, mm/d
real(kind=c_double) :: RFALD           !, RFALM, RFALY ! rainfall, mm
real(kind=c_double) :: rgroper         !  * root grow period [a] until final root density is reached
real(kind=c_double) :: rgrorate        ! * vertical root grow rate [m a-1]
real(kind=c_double) :: RHOTP           ! * ratio of total leaf area to projected area
real(kind=c_double) :: RINT            ! rainfall catch rate, mm/d
real(kind=c_double) :: RINTD           !, RINTM, RINTY ! rain interception, mm
real(kind=c_double) :: RootZoneThickness
real(kind=c_double) :: RM              ! * maximum solar radiation, at which FR = 1, W/m2
real(kind=c_double) :: RNET            ! rain reaching soil surface, mm/d
real(kind=c_double) :: RNETD           !, RNETM, RNETY ! rainfall to soil surface, mm
real(kind=c_double) :: RPLANT          ! plant resistivity to water flow, MPa d/mm
real(kind=c_double) :: RROOTI(ML)      ! root resistance for layer, MPa d/mm
real(kind=c_double) :: RTHR            ! rain throughfall rate, mm/d
real(kind=c_double) :: RTHRD           !, RTHRM, RTHRY ! rain throughfall, mm
real(kind=c_double) :: RTLEN           ! root length per unit land area, m/m2
real(kind=c_double) :: RTRAD           ! * average root radius, mm
real(kind=c_double) :: RSC             ! Shuttleworth-Wallace canopy surface resistance, s/m
real(kind=c_double) :: RSNO            ! rain added to snowpack, mm/d
real(kind=c_double) :: RSNOD           !, RSNOM, RSNOY ! rain on snow, mm
real(kind=c_double) :: RSS             ! Shuttleworth-Wallace soil surface resistance, s/m
real(kind=c_double) :: RSSA            ! * soil evaporation resistance at field capacity, s/m
real(kind=c_double) :: RSSB            !* exponent in relation of soil evap res to water potential
real(kind=c_double) :: RSTEMP          ! * base temperature for snow-rain transition, C
real(kind=c_double) :: RXYLEM          ! xylem resistance, MPa d/mm
real(kind=c_double) :: SAFRAC          ! source area fraction
real(kind=c_double) :: SAI             ! stem area index, m2/m2
real(kind=c_double) :: SAIMLT          ! * parameter for snowmelt dependence on SAI, dimensionless
real(kind=c_double) :: SEEP            ! deep seepage loss from groundwater, mm/d
real(kind=c_double) :: SEEPP, SEEPD    !, SEEPM, SEEPY ! seepage loss, mm
real(kind=c_double) :: SFAL            ! snowfall rate, mm/d
real(kind=c_double) :: SFALD           !, SFALM, SFALY ! snowfall, mm
real(kind=c_double) :: SHEAT           ! @ average soil heat flux for the day, W/m2, fixed at 0
real(kind=c_double) :: Sink(ML)        ! sink (term) for Richards equation, 1/d
real(kind=c_double) :: SINT            ! snowfall catch rate, mm/d
real(kind=c_double) :: SINTD           !, SINTM, SINTY ! snow interception, mm
real(kind=c_double) :: SLFDAY          ! ratio of potential insolation on slope to horizontal,
real(kind=c_double) :: SLFL            !, SLFLOld    ! input rate to soil surface, mm/d
real(kind=c_double) :: SLFLP, SLFLD    !, SLFLM, SLFLY ! input to soil surface, mm
real(kind=c_double) :: SLRAD           ! average solar radiation on slope over daytime, W/m2
real(kind=c_double) :: SLVP            ! , SLVPOldevaporation rate from soil, mm/d
real(kind=c_double) :: SLVPD           !, SLVPM, SLVPY, SLVPVP ! soil evaporation, mm
real(kind=c_double) :: SMLT            ! melt drainage rate from snowpack, mm/d
real(kind=c_double) :: SMLTD           !, SMLTM, SMLTY ! snowmelt, mm
real(kind=c_double) :: SNODEN          ! * snow density, mm/mm
real(kind=c_double) :: SNOEN           ! energy flux density to snow surface, MJ m-2 mm-1 d-1
real(kind=c_double) :: SNOFRC          ! fraction of precipitation for the day as snow, unitless
real(kind=c_double) :: SNOW            ! **** water equivalent of snow on the ground, mm
real(kind=c_double) :: SNOWLQ          ! liquid water content of snow on the ground, mm
real(kind=c_double) :: SNVP            ! evaporation rate from snowpack, mm/d
real(kind=c_double) :: SNVPD           !, SNVPM, SNVPY, SNVPVP ! evaporation from snowpack, mm
real(kind=c_double) :: SOLRAD          !** solar radiation for the day, horizontal surface, MJ/m2
real(kind=c_double) :: SRFL            ! source area flow rate, mm/d
real(kind=c_double) :: SRFLP, SRFLD    !, SRFLM, SRFLY ! source area flow, mm
real(kind=c_double) :: STHR            ! snow throughfall rate, mm/d
real(kind=c_double) :: STHRD           !, STHRM, STHRY ! snow throughfall, mm
real(kind=c_double) :: STONEF(ML)      ! * stone volume fraction, unitless, for layers
real(kind=c_double) :: StonefMat(MMat) ! * stone volume fraction, unitless ,  for materials
real(kind=c_double) :: STORD, STORM, STORY ! total water storage in system, mm
real(kind=c_double) :: STRES  ! , STRESVP TRAN / PTRAN for time period
real(kind=c_double) :: SWAT            ! total soil water in all layers, mm
real(kind=c_double) :: SWATI(ML)       ! water volume in layer, mm
real(kind=c_double) :: SWATMX(ML)      ! maximum water storage for layer, mm
real(kind=c_double) :: SWATQF          ! @water storage at field capacity for layers 1 to
real(kind=c_double) :: SWATQX          ! @ maximum water storage for layers 1 to QLAYER%, mm
real(kind=c_double) :: T1              ! * lowest temp. at which stomates not temp. limited, degC
real(kind=c_double) :: T2              ! * highest temp. at which stomates not temp. limited,degC
real(kind=c_double) :: TA              ! mean temperature for the day at reference height, degC
real(kind=c_double) :: TADTM           ! average daytime temperature at reference height, degC
real(kind=c_double) :: TAJ             ! TADTM or TANTM depending on J%
real(kind=c_double) :: TANTM           ! average nighttime temperature at reference height, degC
real(kind=c_double) :: tBot            ! Temperature of the upper boundary or the incoming fluid [deg. C]
integer :: TEMP                 ! temporary integer variable
real(kind=c_double) :: TemperatureOld (ML)! Old temperature [deg. C]
real(kind=c_double) :: TemperatureNew (ML)! New temperature [deg. C]
real(kind=c_double) :: TMean (ML)      ! Daily mean temperature [deg. C]
real(kind=c_double) :: ThermCond (ML)  ! Thermal conductivity of layer [J d-1 m-1 K-1]
real(kind=c_double) :: TH              ! * temperature above which stomates are closed, degC
real(kind=c_double) :: THETA(ML)       ! water content, mm water / mm soil matrix
real(kind=c_double) :: ThNew(ML)       ! ThOld(ML), water content, mm water / mm soil including stones for heat flow
real(kind=c_double) :: THICK(ML)       ! * layer thicknesses, mm
real(kind=c_double) :: THS             ! water content at saturation, iModel=1
real(kind=c_double) :: THR             ! residual water content, iModel=1
real(kind=c_double) :: tini(ML)        ! * initial time for root growth in layer, a
real(kind=c_double) :: TL              ! * temperature below which stomates are closed, degC
real(kind=c_double) :: TMAX            ! ** maximum temperature for the day, degC
real(kind=c_double) :: TMIN            ! ** minimum temperature for the day, degC
real(kind=c_double) :: TRANI(ML)       ! average transpiration rate from layer, mm/d
real(kind=c_double) :: TRANP, TRAND    !, TRANM, TRANY, TRANVP ! transpiration, mm
real(kind=c_double) :: TRANPI(ML), TRANDI(ML) !, TRANMI(ML), TRANYI(ML),TRANVPI(ML)
real(kind=c_double) :: TSNOW           ! snowpack temperature (isothermal assumed), degC
real(kind=c_double) :: TInt
real(kind=c_double) :: TSno
real(kind=c_double) :: TPrec
real(kind=c_double) :: TEvap
real(kind=c_double) :: TPar(10,MMat)   ! All material dependent heat flow parameters, see PFILE.INC
real(kind=c_double) :: TVrFl
real(kind=c_double) :: TSFLD
real(kind=c_double) ::  tTop           ! Temperature of the upper boundary or the incoming fluid [deg. C]
integer :: TopInfT              ! switch for constant (0, uses value of tTop) or time dependent (1) temperature at upper boundary
real(kind=c_double) :: UA              ! average wind speed for the day at reference height, m/s
real(kind=c_double) :: UADTM           ! average wind speed for daytime at reference height, m/s
real(kind=c_double) :: UAJ             ! UADTN or UANTM depending on J%
real(kind=c_double) :: UANTM           !average wind speed for nighttime at reference height, m/s
real(kind=c_double) :: UW              ! ** average wind speed for day at weather station, m/s
real(kind=c_double) :: vNew(ML)        ! Darcy flow for heat flow
real(kind=c_double) :: VPD             ! vapor pressure deficit at reference height, kPa
real(kind=c_double) :: VRFLI(ML)       ! vertical drainage rate from layer, mm/d
real(kind=c_double) :: VRFLPI(ML), VRFLDI(ML) !, VRFLMI(ML), VRFLYI(ML)
real(kind=c_double) :: VV(ML)          ! temporary VRFLI
real(kind=c_double) :: WETC(ML)        ! @ wetness at PSICR, dimensionless
real(kind=c_double) :: WETFR           ! fraction of precipitation interval that canopy is wet
real(kind=c_double) :: WETNES(ML)      ! wetness, fraction of saturation
real(kind=c_double) :: WNDRAT          ! * ratio of nighttime to daytime wind speed
integer :: YEAR                 ! **** two-digit first year of DFILE
integer :: YY                   ! ** two-digit year
real(kind=c_double) :: ZL(ML)          !
real(kind=c_double) :: Z0              ! roughness parameter, m
real(kind=c_double) :: Z0C             ! roughness parameter for closed canopy of HEIGHT, m
real(kind=c_double) :: Z0G             ! * ground surface roughness, m
real(kind=c_double) :: Z0GS            ! ground or snow surface roughness, m
real(kind=c_double) :: Z0S             ! * snow surface roughness, m
real(kind=c_double) :: Z0W             ! * weather station roughness parameter, m
real(kind=c_double) :: ZA              ! reference height for TA, EA, UA, above ground, m
real(kind=c_double) :: ZMINH           ! * ZA minus HEIGHT, reference height above canopy top, m
real(kind=c_double) :: ZW              ! * weather station measurement height for wind, m"
