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
real(kind=8) :: B0              !,B1,B2,B3,B4,B5     ! buffer
!real(kind=8) :: A0,A1           !,A2,A3,A4,A5     ! buffer
real(kind=8) :: AA              ! average available energy over daytime or nighttime, W/m2
real(kind=8) :: ADEF            ! available water deficit in root zone, mm, output only
real(kind=8) :: ALB             ! * albedo with no snow
real(kind=8) :: ALBEDO          ! albedo
real(kind=8) :: ALBSN           ! * albedo with snow on the ground
real(kind=8) :: ALPHA(ML)       ! modified Cowan alpha, MPa
real(kind=8) :: age             ! * age of vegetation
real(kind=8) :: ASPECT          ! * aspect, degrees through east from north
real(kind=8) :: ASUBS           ! average avail. energy at ground over day or night, W/m2
real(kind=8) :: ATR(2)          ! actual transpiration rate for daytime or night, mm/d
real(kind=8) :: ATRANI(ML)      ! actual transp.rate from layer for daytime or night,mm/d
real(kind=8) :: ATRI(2,ML)      ! actual transp.rate from layer for daytime and night,mm/d
real(kind=8) :: AWAT            ! available soil water in root zone, mm, output only
real(kind=8) :: AWATFK          ! available soil water in root zone at field cap., mm, output only
! real(kind=8) :: AWATM           ! average monthly AWAT
real(kind=8) :: AWAT40          ! available soil water til 40 cm, mm, output only
real(kind=8) :: BALERD          ! BALERM,,BALERY error in water balance. mm
real(kind=8) :: BYFLI(ML)       ! bypass flow rate from layer, mm/d
real(kind=8) :: BYFLPI(ML), BYFLDI(ML) !, BYFLMI(ML), BYFLYI(ML)
real(kind=8) :: BYFLP, BYFLD    !, BYFLM, BYFLY bypass flow
real(kind=8) :: BYFRAC(ML)      ! fraction of layer infiltration to bypass flow
integer :: BYPAR                ! * 1 to allow BYFL, or 0 to prevent BYFL
real(kind=8) :: C1              !* intercept of relation of solar rad. to sunshine duration
real(kind=8) :: C2              ! * slope of relation of solar rad. to sunshine duration
real(kind=8) :: C3              ! * longwave correction factor for overcast sky
real(kind=8) :: CC              ! cold content of snowpack (positive), MJ m-2
real(kind=8) :: CCFAC           ! * cold content factor, MJ m-2 d-1 K-1
real(kind=8) :: CINTRL          ! * maximum interception storage of rain per unit LAI, mm
real(kind=8) :: CINTRS          ! * maximum interception storage of rain per unit SAI, mm
real(kind=8) :: CINTSL          ! * maximum interception storage of snow per unit LAI, mm
real(kind=8) :: CINTSS          ! * maximum interception storage of snow per unit SAI, mm
real(kind=8) :: CR              ! * light extinction coefficient for projected LAI + SAI
real(kind=8) :: CS              ! * ratio of projected SAI to canopy height, m-1
real(kind=8) :: CVPD            !* vapor pressure deficit at which conductance is halved,kPa
real(kind=8) :: CZR             ! * ratio of roughness to HEIGHT for rough closed canopies
real(kind=8) :: CZS             ! * ratio of roughness to HEIGHT for smooth closed canopies
real(kind=8) :: DAYLEN          ! daylength in fraction of day, d-1
integer, dimension(12) :: DAYMO ! * days in month
integer :: DD                   ! ** day of the month
real(kind=8) :: DELTA           ! dES/dT at some temperature T, kPa/K
real(kind=8) :: dep(ML)         ! soil depth [m]
real(kind=8) :: depmax          ! depth [m] of soil surface
real(kind=8) :: DENSEF          ! * density factor for MAXLAI,CS,RTLEN,RPLANT, not <.001
real(kind=8) :: DISP            ! zero-plane displacement, m
real(kind=8) :: DISPC           ! zero-plane displacement for closed canopy of HEIGHT, m
integer :: DOM                  ! day of month
integer :: DOY                  ! **** first day of the year in DFILE
real(kind=8) :: DPSIDW(ML)      !rate of change of total potential with watercontent,kPa/mm
real(kind=8) :: DPSIMX          ! * maximum potential difference considered equal, kPa
real(kind=8) :: DRAIN           ! * multiplier of VFLUX(n) for drainage to groundwater
real(kind=8) :: DSFLI(ML)       ! downslope flow rate from layer, mm/d
real(kind=8) :: DSFLP, DSFLD    !, DSFLM, DSFLY downslope flow
real(kind=8) :: DSFLPI(ML), DSFLDI(ML) !, DSFLMI(ML), DSFLYI(ML)
real(kind=8) :: DSLOPE          ! * slope for DSFL, degrees
real(kind=8) :: DSWMAX          ! * maximum change allowed in SWATI, percent of SWATMX(i)
real(kind=8) :: DTI             ! time step for iteration interval, d
real(kind=8) :: DTIMAX          ! * maximum iteration time step, d
real(kind=8) :: DTINEW          ! second estimate of DTI
real(kind=8) :: DTP             ! @ time step for precipitation interval, may be <= 1 d
real(kind=8) :: DTRI            ! time remaining in precipitation interval, d
real(kind=8) :: DUMM(ML)        ! dummy array for subroutine calls
real(kind=8) :: DUMMY           ! dummy variable for subroutine calls
real(kind=8), dimension(12) :: DURATN ! * average duration of daily precip by month, hr
real(kind=8) :: EA              ! ** vapor pressure for the day, kPa
logical(kind=1) :: error        ! if there is error in the subroutine
real(kind=8) :: ES              ! saturated vapor pressure, kPa
real(kind=8) :: ESLOPE          ! * slope for evapotranspiration and snowmelt, degrees
real(kind=8) :: Eta             ! volumetric air content
!real(kind=8) :: ETDiff          ! Evapotranspirationdifferenz
real(kind=8) :: EVAPP, EVAPD    ! , EVAPM, EVAPY, EVAPVP evapotranspiration
real(kind=8) :: FETCH           ! * weather station fetch, m"
real(kind=8) :: FLOWP, FLOWD    ! , , FLOWM, FLOWY FLOWVPtotal flow
real(kind=8) :: FRINTL          ! * intercepted fraction of rain per unit LAI
real(kind=8) :: FRINTS          ! * intercepted fraction of rain per unit SAI
real(kind=8) :: frelden(ML)     ! * relative values of final root density per unit volume
real(kind=8) :: FSINTL          ! * intercepted fraction of snow per unit LAI
real(kind=8) :: FSINTS          ! * intercepted fraction of snow per unit SAI
real(kind=8) :: FXYLEM          ! * fraction of plant resistance in xylem
real(kind=8) :: PGER(2)         ! ground evaporation rate for daytime or night, mm/d
real(kind=8) :: GER(2)          ! ground evaporation rate for daytime or night, mm/d
real(kind=8) :: GEVP            ! average ground evaporation for day, mm/d
real(kind=8) :: GIR(2)          !ground evap. rate with intercep. for daytime or night,mm/d
real(kind=8) :: GIVP            !average ground evaporation for day with interception, mm/d
real(kind=8) :: GLMAX           ! * maximum leaf conductance, m/s
real(kind=8) :: GLMIN           ! * minimum leaf conductance, m/s
real(kind=8) :: GRDMLT          ! * rate of groundmelt of snowpack, mm/d
real(kind=8) :: GSC             ! * discharge from GWAT, fraction per day, d-1
real(kind=8) :: GSP             ! * fraction of discharge to seepage
real(kind=8) :: GWAT            ! **** groundwater storage below soil layers, mm
real(kind=8) :: GWFL            ! streamflow rate from groundwater, mm/d
real(kind=8) :: GWFLP, GWFLD    ! , GWFLM, GWFLYgroundwater flow
double precision   HA(ML), HB(ML), HC(ML), HD(ML) ! Buffers for heat flow
integer :: HEAT                 !  switch to include (1) or to exclude (0) soil heat flow
real(kind=8) :: HeatCapNew(ML), HeatCapOld(ML) ! Volumetric heat capacity of layer [J m-3 K-1]
real(kind=8) :: HEIGHT          ! canopy height, m
real(kind=8) :: HR              ! * height above which CZR applies, m
real(kind=8) :: HS              ! * height below which CZS applies, m
integer :: i1, i2,ifoundfirst
integer :: I, II                ! index variable for layer number
real(kind=8) :: I0HDAY          ! potential insolation on horizontal, MJ m-2 d-1
integer :: IDAY                 ! day number in run
integer :: ILAYER               ! * number of layers over which infiltration is distributed
integer :: imodel               ! * parameterization of hydraulic functions
real(kind=8) :: IMPERV          ! * impervious fraction of area for SRFL
real(kind=8) :: INFEXP          ! * infiltration exponent, 0-all to top to 1-uniform
real(kind=8) :: inFil           ! infiltration [mm] for heat flow
real(kind=8) :: INFRAC(ML)      ! @ fraction of infiltration to each layer
real(kind=8) :: INFLI(ML)       ! infiltration rate into layer, mm/d
real(kind=8) :: INFLP, INFLD    ! , INFLM, INFLY infiltration into soil matrix, mm
real(kind=8) :: INFLPI(ML), INFLDI(ML) !, INFLMI(ML), INFLYI(ML)
real(kind=8) :: inirdep         ! initial root depth, e.g. at planting [m]
real(kind=8) :: inirlen         ! initial total root length, e.g. at planting [m m-2]
real(kind=8) :: INTR            ! & intercepted rain, mm
real(kind=8) :: INTS            ! & intercepted snow, mm
real(kind=8) :: IRVP            ! evaporation rate of intercepted rain, mm/d
real(kind=8) :: IRVPD           !, IRVPM, IRVPY, IRVPVP ! evaporation of intercepted rain, mm
real(kind=8) :: ISVP            ! evaporation rate of intercepted snow, mm/d
real(kind=8) :: ISVPD           !, ISVPM, ISVPY, ISVPVP ! evaporation of intercepted snow, mm
integer :: J, JJ                ! index variable for day-night separation
real(kind=8) :: KK(ML)          ! hydraulic conductivity, mm/d
real(kind=8) :: KSNVP           ! * multiplier to fix snow evaporation problem
real(kind=8) :: L1              ! @ latitude of equivalent slope, radians
real(kind=8) :: L2              ! @ time shift of equivalent slope, radians
real(kind=8) :: LAI             ! leaf area index, m2/m2
real(kind=8) :: LAIMLT          ! * parameter for snowmelt dependence on LAI,dimensionless
real(kind=8) :: LAT             ! **** latitude, degrees
real(kind=8) :: LENGTH          ! * slope length for DSFL, m
real(kind=8) :: LPC             ! * minimum LAI defining a closed canopy
real(kind=8) :: LWIDTH          ! * leaf width, m
integer :: mat(ML)              ! * material index for layer
real(kind=8) :: MAXLQF          ! * maximum liquid water fraction of SNOW, dimensionless
real(kind=8) :: MELFAC          ! * degree day melt factor for open, MJ m-2 d-1 K-1
real(kind=8) :: MESFL           ! ** measured streamflow for day, mm
real(kind=8) :: MESFLD          !, MESFLM, MESFLY, MESFLVP ! measured streamflow, mm
real(kind=8) :: MESFLP          ! ** measured streamflow rate for precip interval, mm/d
integer :: MM                   ! ** month
integer :: MONTH                ! month number
real(kind=8) :: MUE(ML)
real(kind=8) :: MXKPL           ! * maximum plant conductivity, (mm/d)/MPa
real(kind=8) :: MXRTLN          ! * maximum root length per unit land area, m/m2
integer :: N                    ! index variable for precipitation interval
real(kind=8) :: NN              ! * wind/diffusivity extinction coefficient
integer :: NDAYS                ! * number of days in run
integer :: NITS                 ! number of iterations in precipitation interval
integer :: NITSD                ! total number of iterations for day
integer :: NITSM                ! total number of iterations for month
integer :: NITSY                ! total number of iterations for year
integer :: NLAYER               ! * number of soil layers to be used in model, <= ML
integer :: nmat                 ! * number of materials for profile
integer :: NOOUTF               ! * 1 if no outflow allowed from roots, otherwise 0
integer :: NPINT                ! **** number of precipitation intervals per day
real(kind=8) :: NTFLI(ML)       ! net flow rate into layer, mm/d
real(kind=8) :: NTFLPI(ML), NTFLDI(ML) !, NTFLMI(ML), NTFLYI(ML)
! integer :: OP(10,5)             ! * output file selections
real(kind=8) :: ParMat(MPar,MMat) ! hydraulic parameters for material
real(kind=8) :: Par(MPar,ML)    ! hydraulic parameters for layer
real(kind=8) :: PINT            ! average potential interception for day, mm/d
real(kind=8) :: PINTD           !, PINTM, PINTY, PINTVP ! potential interception, mm
real(kind=8) :: PIR(2)          ! potential interception rate for daytime or night, mm/d
real(kind=8) :: PREC            ! precipitation rate, mm/d
real(kind=8) :: PRECD           !, PRECM, PRECY ! precipitation, mm
real(kind=8) :: PREINT          ! ** precipitation for precipitation interval, mm
real(kind=8) :: PSICR           ! * minimum plant leaf water potential, MPa
real(kind=8) :: PsiCrit(ML)     ! * minimum soil matric potential to allow water supply for evapotranspiration, Pa
real(kind=8) :: PSIG(ML)        ! @ gravity potential, kPa
real(kind=8) :: PSIM(ML)        ! & matric soil water potential for layer, kPa
real(kind=8) :: PSITI(ML)       ! total potential, kPa
real(kind=8) :: PSLVP           ! potential evaporation rate from soil, mm/d
real(kind=8) :: PSLVPD          !, PSLVPM, PSLVPY, PSLVPVP ! potential soil evaporation, mm
real(kind=8) :: PSNVP           ! potential snow evaporation, mm/d
real(kind=8) :: PTR(2)          ! potential transpiration rate for daytime or night, mm/d
real(kind=8) :: PTRAN           ! average potential transpiration rate for day, mm/d
real(kind=8) :: PTRAND          !, PTRANM, PTRANY, PTRANVP ! potential transpiration, mm
real(kind=8) :: QFFC            ! * quick flow fraction (SRFL or BYFL) at field capacity
real(kind=8) :: QFPAR           ! * quick flow parameter (SRFL or BYFL)
integer :: QLAYER               ! * number of soil layers for SRFL, 0 to prevent SRFL
real(kind=8) :: R5              ! * solar radiation at which conductance is halved, W/m2
real(kind=8) :: RAA             ! Shuttleworth-Wallace atmosphere aerodynamic resistance,s/m
real(kind=8) :: RAC             ! Shuttleworth-Wallace canopy aerodynamic resistance, s/m
real(kind=8) :: RAS             ! Shuttleworth-Wallace ground aerodynamic resistance, s/m
real(kind=8) :: RELAWAT         ! relative AWAT : AWAT/AWATFK
real(kind=8) :: RELDEN(ML)      ! * relative values of root density per unit volume
real(kind=8) :: RFAL            ! rainfall rate, mm/d
real(kind=8) :: RFALD           !, RFALM, RFALY ! rainfall, mm
real(kind=8) :: rgroper         !  * root grow period [a] until final root density is reached
real(kind=8) :: rgrorate        ! * vertical root grow rate [m a-1]
real(kind=8) :: RHOTP           ! * ratio of total leaf area to projected area
real(kind=8) :: RINT            ! rainfall catch rate, mm/d
real(kind=8) :: RINTD           !, RINTM, RINTY ! rain interception, mm
real(kind=8) :: RootZoneThickness
real(kind=8) :: RM              ! * maximum solar radiation, at which FR = 1, W/m2
real(kind=8) :: RNET            ! rain reaching soil surface, mm/d
real(kind=8) :: RNETD           !, RNETM, RNETY ! rainfall to soil surface, mm
real(kind=8) :: RPLANT          ! plant resistivity to water flow, MPa d/mm
real(kind=8) :: RROOTI(ML)      ! root resistance for layer, MPa d/mm
real(kind=8) :: RTHR            ! rain throughfall rate, mm/d
real(kind=8) :: RTHRD           !, RTHRM, RTHRY ! rain throughfall, mm
real(kind=8) :: RTLEN           ! root length per unit land area, m/m2
real(kind=8) :: RTRAD           ! * average root radius, mm
real(kind=8) :: RSC             ! Shuttleworth-Wallace canopy surface resistance, s/m
real(kind=8) :: RSNO            ! rain added to snowpack, mm/d
real(kind=8) :: RSNOD           !, RSNOM, RSNOY ! rain on snow, mm
real(kind=8) :: RSS             ! Shuttleworth-Wallace soil surface resistance, s/m
real(kind=8) :: RSSA            ! * soil evaporation resistance at field capacity, s/m
real(kind=8) :: RSSB            !* exponent in relation of soil evap res to water potential
real(kind=8) :: RSTEMP          ! * base temperature for snow-rain transition, C
real(kind=8) :: RXYLEM          ! xylem resistance, MPa d/mm
real(kind=8) :: SAFRAC          ! source area fraction
real(kind=8) :: SAI             ! stem area index, m2/m2
real(kind=8) :: SAIMLT          ! * parameter for snowmelt dependence on SAI, dimensionless
real(kind=8) :: SEEP            ! deep seepage loss from groundwater, mm/d
real(kind=8) :: SEEPP, SEEPD    !, SEEPM, SEEPY ! seepage loss, mm
real(kind=8) :: SFAL            ! snowfall rate, mm/d
real(kind=8) :: SFALD           !, SFALM, SFALY ! snowfall, mm
real(kind=8) :: SHEAT           ! @ average soil heat flux for the day, W/m2, fixed at 0
real(kind=8) :: Sink(ML)        ! sink (term) for Richards equation, 1/d
real(kind=8) :: SINT            ! snowfall catch rate, mm/d
real(kind=8) :: SINTD           !, SINTM, SINTY ! snow interception, mm
real(kind=8) :: SLFDAY          ! ratio of potential insolation on slope to horizontal,
real(kind=8) :: SLFL            !, SLFLOld    ! input rate to soil surface, mm/d
real(kind=8) :: SLFLP, SLFLD    !, SLFLM, SLFLY ! input to soil surface, mm
real(kind=8) :: SLRAD           ! average solar radiation on slope over daytime, W/m2
real(kind=8) :: SLVP            ! , SLVPOldevaporation rate from soil, mm/d
real(kind=8) :: SLVPD           !, SLVPM, SLVPY, SLVPVP ! soil evaporation, mm
real(kind=8) :: SMLT            ! melt drainage rate from snowpack, mm/d
real(kind=8) :: SMLTD           !, SMLTM, SMLTY ! snowmelt, mm
real(kind=8) :: SNODEN          ! * snow density, mm/mm
real(kind=8) :: SNOEN           ! energy flux density to snow surface, MJ m-2 mm-1 d-1
real(kind=8) :: SNOFRC          ! fraction of precipitation for the day as snow, unitless
real(kind=8) :: SNOW            ! **** water equivalent of snow on the ground, mm
real(kind=8) :: SNOWLQ          ! liquid water content of snow on the ground, mm
real(kind=8) :: SNVP            ! evaporation rate from snowpack, mm/d
real(kind=8) :: SNVPD           !, SNVPM, SNVPY, SNVPVP ! evaporation from snowpack, mm
real(kind=8) :: SOLRAD          !** solar radiation for the day, horizontal surface, MJ/m2
real(kind=8) :: SRFL            ! source area flow rate, mm/d
real(kind=8) :: SRFLP, SRFLD    !, SRFLM, SRFLY ! source area flow, mm
real(kind=8) :: STHR            ! snow throughfall rate, mm/d
real(kind=8) :: STHRD           !, STHRM, STHRY ! snow throughfall, mm
real(kind=8) :: STONEF(ML)      ! * stone volume fraction, unitless, for layers
real(kind=8) :: StonefMat(MMat) ! * stone volume fraction, unitless ,  for materials
real(kind=8) :: STORD, STORM, STORY ! total water storage in system, mm
real(kind=8) :: STRES  ! , STRESVP TRAN / PTRAN for time period
real(kind=8) :: SWAT            ! total soil water in all layers, mm
real(kind=8) :: SWATI(ML)       ! water volume in layer, mm
real(kind=8) :: SWATMX(ML)      ! maximum water storage for layer, mm
real(kind=8) :: SWATQF          ! @water storage at field capacity for layers 1 to
real(kind=8) :: SWATQX          ! @ maximum water storage for layers 1 to QLAYER%, mm
real(kind=8) :: T1              ! * lowest temp. at which stomates not temp. limited, degC
real(kind=8) :: T2              ! * highest temp. at which stomates not temp. limited,degC
real(kind=8) :: TA              ! mean temperature for the day at reference height, degC
real(kind=8) :: TADTM           ! average daytime temperature at reference height, degC
real(kind=8) :: TAJ             ! TADTM or TANTM depending on J%
real(kind=8) :: TANTM           ! average nighttime temperature at reference height, degC
real(kind=8) :: tBot            ! Temperature of the upper boundary or the incoming fluid [deg. C]
integer :: TEMP                 ! temporary integer variable
real(kind=8) :: TemperatureOld (ML)! Old temperature [deg. C]
real(kind=8) :: TemperatureNew (ML)! New temperature [deg. C]
real(kind=8) :: TMean (ML)      ! Daily mean temperature [deg. C]
real(kind=8) :: ThermCond (ML)  ! Thermal conductivity of layer [J d-1 m-1 K-1]
real(kind=8) :: TH              ! * temperature above which stomates are closed, degC
real(kind=8) :: THETA(ML)       ! water content, mm water / mm soil matrix
real(kind=8) :: ThNew(ML)       ! ThOld(ML), water content, mm water / mm soil including stones for heat flow
real(kind=8) :: THICK(ML)       ! * layer thicknesses, mm
real(kind=8) :: THS             ! water content at saturation, iModel=1
real(kind=8) :: THR             ! residual water content, iModel=1
real(kind=8) :: tini(ML)        ! * initial time for root growth in layer, a
real(kind=8) :: TL              ! * temperature below which stomates are closed, degC
real(kind=8) :: TMAX            ! ** maximum temperature for the day, degC
real(kind=8) :: TMIN            ! ** minimum temperature for the day, degC
real(kind=8) :: TRANI(ML)       ! average transpiration rate from layer, mm/d
real(kind=8) :: TRANP, TRAND    !, TRANM, TRANY, TRANVP ! transpiration, mm
real(kind=8) :: TRANPI(ML), TRANDI(ML) !, TRANMI(ML), TRANYI(ML),TRANVPI(ML)
real(kind=8) :: TSNOW           ! snowpack temperature (isothermal assumed), degC
real(kind=8) :: TInt
real(kind=8) :: TSno
real(kind=8) :: TPrec
real(kind=8) :: TEvap
real(kind=8) :: TPar(10,MMat)   ! All material dependent heat flow parameters, see PFILE.INC
real(kind=8) :: TVrFl
real(kind=8) :: TSFLD
real(kind=8) ::  tTop           ! Temperature of the upper boundary or the incoming fluid [deg. C]
integer :: TopInfT              ! switch for constant (0, uses value of tTop) or time dependent (1) temperature at upper boundary
real(kind=8) :: UA              ! average wind speed for the day at reference height, m/s
real(kind=8) :: UADTM           ! average wind speed for daytime at reference height, m/s
real(kind=8) :: UAJ             ! UADTN or UANTM depending on J%
real(kind=8) :: UANTM           !average wind speed for nighttime at reference height, m/s
real(kind=8) :: UW              ! ** average wind speed for day at weather station, m/s
real(kind=8) :: vNew(ML)        ! Darcy flow for heat flow
real(kind=8) :: VPD             ! vapor pressure deficit at reference height, kPa
real(kind=8) :: VRFLI(ML)       ! vertical drainage rate from layer, mm/d
real(kind=8) :: VRFLPI(ML), VRFLDI(ML) !, VRFLMI(ML), VRFLYI(ML)
real(kind=8) :: VV(ML)          ! temporary VRFLI
real(kind=8) :: WETC(ML)        ! @ wetness at PSICR, dimensionless
real(kind=8) :: WETFR           ! fraction of precipitation interval that canopy is wet
real(kind=8) :: WETNES(ML)      ! wetness, fraction of saturation
real(kind=8) :: WNDRAT          ! * ratio of nighttime to daytime wind speed
integer :: YEAR                 ! **** two-digit first year of DFILE
integer :: YY                   ! ** two-digit year
real(kind=8) :: ZL(ML)          !
real(kind=8) :: Z0              ! roughness parameter, m
real(kind=8) :: Z0C             ! roughness parameter for closed canopy of HEIGHT, m
real(kind=8) :: Z0G             ! * ground surface roughness, m
real(kind=8) :: Z0GS            ! ground or snow surface roughness, m
real(kind=8) :: Z0S             ! * snow surface roughness, m
real(kind=8) :: Z0W             ! * weather station roughness parameter, m
real(kind=8) :: ZA              ! reference height for TA, EA, UA, above ground, m
real(kind=8) :: ZMINH           ! * ZA minus HEIGHT, reference height above canopy top, m
real(kind=8) :: ZW              ! * weather station measurement height for wind, m"
