! Tony Federer's original [Brook90 Fortran 77 code](http://www.ecoshift.net/brook/b90doc.html)
! (Brook90_v3.1F, License: CC0) was enhanced by Klaus Hammel and Martin Kennel at Bavarian State
! Institute of Forestry (LWF) around the year 2000. Since then, LWF-BROOK90 is distributed by
! [LWF](https://www.lwf.bayern.de/boden-klima/wasserhaushalt/index.php) upon request as a !
! pre-compiled Fortran command line program together with in MS Access User Interface.
! In 2019, Volodymyr Trotsiuk converted the Fortran 77 code to Fortran 95 and implemented
! the connection to R. Paul Schmidt-Walter's *brook90r* package for LWF-Brook90 input data
! generation, model execution and result processing was adapted and extended to control this
! interface function.

! 2020-05 adjustments made by V. Trotsik [volodymyr.trostiuk@wsl.ch]
!   - constants are declared in separate module md_decl_const.f95
!   - renamed 'brook90.f95' to 'md_brook90.f95'
!   - renamed main subroutine from 'fbrook90' to 's_brook90_f'
!   - all outputs are provided in two tables: output_day and output_layer
!   - all writings to files are removed
!   - removed ZMONTH, ZYEAR, MSUM, YSUM, MACCUM, YACCUM
!   - removing unussed variables from DECLARATION

! 2022-06 adjustements made by P Schmidt-Walter [paul.schmidt-walter@dwd.de]
!   - basic output now at the precipitation interval - level.


! Reshape inout-arrays to hold values on precipitation interval level
! Output values at the prec int level to




module fbrook_mod

    use, intrinsic :: iso_c_binding, only: c_double, c_int, c_bool
    use mod_decl_const

    implicit none
    private
    public :: s_brook90_f

contains

subroutine s_brook90_f( siteparam, climveg, param, pdur, soil_materials, soil_nodes, precdat, &
                     pr, timer, error, output_day, output_layer) bind(C, name="s_brook90_f_")

    implicit none

    !*************************************************************************************
    ! DECLARATION

    ! Input of parameters
    real(kind=c_double), dimension(84), intent(in) :: param
    real(kind=c_double), dimension(1,12), intent(in) :: pdur

    real(kind=c_double), dimension(6), intent(in) :: siteparam
    real(kind=c_double), dimension( INT(param(66)),8), intent(in) :: soil_materials
    real(kind=c_double), dimension( INT(param(65)),6), intent(in) :: soil_nodes
    real(kind=c_double), dimension( INT(param(1)),15), intent(in) :: climveg
    real(kind=c_double), dimension( INT(param(1)*siteparam(6)),6), intent(in) :: precdat

    ! Printing
    integer(kind=c_int), intent(in) :: pr

    ! Check execution time
    integer(kind=c_int), intent(in) :: timer

    ! Error-code
    integer(kind=c_int), intent(inout) :: error

    ! Output matrix
    real(kind=c_double), dimension( INT(param(1) * siteparam(6)), 47), intent(inout) :: output_day
    real(kind=c_double), dimension( INT(param(1) * siteparam(6)), 15, INT(param(65))), intent(inout) :: output_layer

    ! Variables
    include 'VARDCL.h'

    allocate( Par(MPar, ML) )
    DATA DAYMO / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
    error = 0._c_int

    ! Not sure why temperature is not initialized
    TPar(:,:) = -1
    tBot = -1
    RSNO = -1
    TSNOW = 0

    ! Parameters
    include 'PFILE.h'

    ! Site level information
    YEAR = INT( siteparam( 1 ) )
    DOY = INT( siteparam( 2 ) )
    LAT = siteparam( 3 )
    SNOW = siteparam( 4 )
    GWAT = siteparam( 5 )
    NPINT = INT( siteparam( 6 ) )

    LAT = LAT / 57.296 ! convert to lat: 180/pi

    ! read PRFILE.DAT if requested
    
    if (NPINT .GT. 1) then
        DTP = DT / real( NPINT, kind=8)
!         OPEN (UNIT = 10, FILE = 'in/PRFILE.DAT')
    else
        DTP = DT
    end if

!   days in month for February - DAYMO(2)
    include 'LEAP.h'
!   initial MONTH and DOM
    include 'GETDOM.h'

    ! equivalent slope for solar radiation
    CALL EQUIVSLP(LAT, ESLOPE, ASPECT, L1, L2)

    ! soil water parameters and initial variables
    CALL SOILPAR (NLAYER, iModel, Par, THICK, STONEF, PSIM, PSICR, &
        PSIG, SWATMX, WETC, WETNES, SWATI, MPar, ML, pr, error)
    if ( error .ne. 0 ) go to 999

    ! Check for timelimits set by R-user
    if (timer .EQ. 1) call rchkusr()

    ! more initial soil water variables
    CALL SOILVAR(NLAYER, iModel, Par, PSIG, PSIM, WETNES, SWATI, &
        PSITI, THETA, KK, SWAT, MPar, ML)

    DO 43 i = 1, NLAYER
        HeatCapOld(i) = TPar(7,mat(i)) * TPar(1,mat(i)) + TPar(8,mat(i))
     !               TPar(2,mat(i))+TPar(9,mat(i))*SWATI(I)/THICK(I)
43  CONTINUE

    ! critical potentials
    CALL MinPsi(ThCrit, PsiCrit, Par, iModel, NLAYER, MPar, ML)

    ! infiltration parameters
    CALL INFPAR(INFEXP, ILAYER, NLAYER, THICK, INFRAC)

    ! source area parameters
    CALL SRFPAR(QLAYER, Par, THICK, STONEF, SWATMX, SWATQX, SWATQF, MPar, ML)

    ! initial total water in system
    STORD = INTR + INTS + SNOW + SWAT + GWAT
    STORM = STORD
    STORY = STORD
    ! any initial snow has zero liquid water and cold content
    CC = 0.0d0
    SNOWLQ = 0.0d0

    ! program initializations
    IDAY = 1
    NITSY = 0
    NITSM = 0

    TInt=0
    TSno=0
    TPrec=0
    TEvap=0
    TVrFl=0
    TSFLD=0

    NTFLI(1)=0

!   ***************  B E G I N    D A Y   L O O P  ***************************************
500 if (IDAY .LE. NDAYS) then

        NITSD = 0
        IF (IDAY .EQ. 1 .OR. (MONTH .EQ. 1 .AND. DOM .EQ. 1)) THEN
!           new year
            INCLUDE 'LEAP.h'
        END IF

        ! Check for timelimits set by R-user
        if (timer .EQ. 1) call rchkusr()

!       * * I N P U T   W E A T H E R   L I N E   F R O M   D F I L E . D A T * *
!       next line can be modified for different input formats
        YY = INT( climveg( IDAY, 1) )
        MM = INT( climveg( IDAY, 2) )
        DD = INT( climveg( IDAY, 3) )
        SOLRAD = climveg( IDAY, 4)
        TMAX = climveg( IDAY, 5)
        TMIN = climveg( IDAY, 6)
        EA = climveg( IDAY, 7)
        UW = climveg( IDAY, 8)
        PREINT = climveg( IDAY, 9)
        MESFL = climveg( IDAY, 10)
        DENSEF = climveg( IDAY, 11)
        HEIGHT = climveg( IDAY, 12)
        LAI = climveg( IDAY, 13)
        SAI = climveg( IDAY, 14)
        age = climveg( IDAY, 15)

        DENSEF=MAX(0.050d0,DENSEF)

        IF (YEAR .NE. YY .OR. MONTH .NE. MM .OR. DOM .NE. DD) THEN
            error = 3
            if (pr .EQ. 1) then
              call intpr("STOP - inconsistent dates in climate - expected (year, month, day):", -1, &
              (/ YEAR, MONTH, DOM/),3)
              call intpr("but got", -1, (/ YY, MM, DD/),3)
              !print*, 'STOP - DFILE error, expected', YEAR, MONTH, DOM, ' but got:', YY, MM, DD
              !STOP
            end if
        goto 999
        END IF

!        * *  S E T  R O O T  D I S T R I B U T I O N * * * * *  * * * *  * * * *
        CALL RootGrowth (frelden, RELDEN, tini, age, rgroper, inirdep, inirlen, NLAYER)

        ! solar parameters depending on DOY
        CALL SUNDS(LAT, ESLOPE, DOY, L1, L2, DAYLEN, I0HDAY, SLFDAY)

        ! canopy parameters depending on DOY
        IF (SNOW .GT. 0.0d0) THEN
            Z0GS = Z0S
        ELSE
            Z0GS = Z0G
        END IF

        CALL CANOPY(SNOW, SNODEN,MXRTLN, MXKPL, DENSEF, HEIGHT, LAI, SAI, RTLEN, RPLANT)

        ! canopy roughness parameters
        CALL ROUGH(HEIGHT, ZMINH, LAI, SAI, CZS, CZR, HS, HR, LPC, CS, Z0G, Z0C, DISPC, Z0, DISP, ZA)

        ! plant resistance components
        CALL PLNTRES(NLAYER, THICK, STONEF, RTLEN, RELDEN, RTRAD, RPLANT, FXYLEM, RXYLEM, RROOTI, ALPHA)

        ! calculated weather data
        SHEAT = 0.0d0
        CALL WEATHER(TMAX, TMIN, DAYLEN, I0HDAY, EA, UW, ZA, DISP, Z0, &
            WNDRAT, FETCH, Z0W, ZW, SOLRAD, TA, TADTM, TANTM, UA, UADTM, UANTM)

        ! fraction of precipitation as SFAL
        CALL SNOFRAC(TMAX, TMIN, RSTEMP, SNOFRC)

        IF (SNOW .GT. 0.0d0) THEN
!        snowpack temperature at beginning of day
            TSNOW = -CC / (CVICE * SNOW)
!        potential snow evaporation
            CALL SNOVAP(TSNOW, TA, EA, UA, ZA, HEIGHT, Z0, DISP, Z0C, DISPC, Z0GS, &
                LWIDTH, RHOTP, NN, LAI, SAI, KSNVP, PSNVP)
            ALBEDO = ALBSN
            RSS = 0.0d0
        ELSE
            TSNOW = 0.0d0
            PSNVP = 0.0d0
            ALBEDO = ALB
!        soil evaporation resistance
!         RSS = FRSS(RSSA, RSSB, Par(4,1), PSIM(1))
!             RSS = FRSS(RSSA, RSSB, Par(1,1), PSIM(1), PsiCrit(1), iModel)
            RSS = FRSS(RSSA, RSSB, Par(1,1), PSIM(1), PsiCrit(1))
        END IF

        ! snow surface energy balance (even if SNOW = 0 in case of snow during day)
        CALL SNOENRGY(TSNOW, TA, DAYLEN, CCFAC, MELFAC, SLFDAY, LAI, SAI, LAIMLT, SAIMLT, SNOEN)

!       * * * * *  B E G I N   D A Y - N I G H T   E T   L O O P  * * * * * * * * * * * *
        DO 300 JJ = 1, 2
            J = JJ
!           1 for daytime, 2 for nighttime
!           net radiation
            IF (J .EQ. 1) THEN
                SLRAD = SLFDAY * SOLRAD / (WTOMJ * DAYLEN)
                TAJ = TADTM
                UAJ = UADTM
            ELSE
                SLRAD = 0.0d0
                TAJ = TANTM
                UAJ = UANTM
            END IF

            CALL AVAILEN(SLRAD, ALBEDO, C1, C2, C3, TAJ, EA, &
                SOLRAD / I0HDAY, SHEAT, CR, LAI, SAI, AA(J), ASUBS(J),LNGNET(J))
            !AADTNT(J) = AA
!           vapor pressure deficit
            CALL ESAT(TAJ, ES, DELTA)
!                       ^^  ^^^^^
            VPD = ES - EA
!        Shuttleworth-Wallace resistances
            CALL SWGRA(UAJ, ZA, HEIGHT, Z0, DISP, Z0C, DISPC, Z0GS, &
                LWIDTH, RHOTP, NN, LAI, SAI, RAA, RAC, RAS)
!                                            ^^^  ^^^  ^^^
            IF (J .EQ. 1) THEN
                CALL SRSC(SLRAD, TA, VPD, LAI, SAI, GLMIN, GLMAX, R5, CVPD, &
                    RM, CR, TL, T1, T2, TH, RSC)
!                                           ^^^
            ELSE
                RSC = 1.0d0 / (GLMIN * LAI)
            END IF
!           Shuttleworth-Wallace potential transpiration and ground evaporation
!           rates
            CALL SWPE(AA(J), ASUBS(J), VPD, RAA, RAC, RAS, RSC, RSS, DELTA, PTR(J), GER(J))

!        Shuttleworth-Wallace potential interception and ground evap. rates
!        (RSC=0)
            CALL SWPE(AA(J), ASUBS(J), VPD, RAA, RAC, RAS, 0.0d0, RSS, DELTA, PIR(J), GIR(J))

!        Shuttleworth-Wallace potential interception and ground evap. rates
!        (RSS=0)
            CALL SWPE(AA(J), ASUBS(J), VPD, RAA, RAC, RAS, RSC, 0.0d0, DELTA, B0, PGER(J))

!        actual transpiration and ground evaporation rates
            IF (PTR(J) .GT. .0010d0) THEN
                CALL TBYLAYER(J, PTR(J), DISPC, ALPHA, KK, RROOTI, RXYLEM, &
                    PSITI, NLAYER, PSICR, NOOUTF, ATR(J), ATRANI)

                DO 11 I = 1, NLAYER
                    ATRI(J, I) = ATRANI(I)
11              CONTINUE
                IF (ATR(J) .LT. PTR(J)) THEN
!              soil water limitation, new GER
                    CALL SWGE(AA(J), ASUBS(J), VPD, RAA, RAS, RSS, DELTA, ATR(J), GER(J))
                END IF
            ELSE
!           no transpiration, condensation ignored, new GER
                PTR(J) = 0.0d0
                ATR(J) = 0.0d0
                DO 21 I = 1, NLAYER
                    ATRI(J, I) = 0.0d0
21              CONTINUE
                CALL SWGE(AA(J), ASUBS(J), VPD, RAA, RAS, RSS, DELTA, 0.0d0, GER(J))
            END IF
300     CONTINUE
!     * * * * * * * * *  E N D   D A Y - N I G H T   L O O P  * * * * * * * * * * * * * *

        ! average rates over day
        PTRAN = (PTR(1) * DAYLEN + PTR(2) * (1.0d0 - DAYLEN)) / DT
        GEVP = (GER(1) * DAYLEN + GER(2) * (1.0d0 - DAYLEN)) / DT
        PINT = (PIR(1) * DAYLEN + PIR(2) * (1.0d0 - DAYLEN)) / DT
        GIVP = (GIR(1) * DAYLEN + GIR(2) * (1.0d0 - DAYLEN)) / DT
        PSLVP = (PGER(1) * DAYLEN + PGER(2) * (1.0d0 - DAYLEN)) / DT

!      write(10,*) ' ep=',PSLVP,' tp=',PTRAN
!      write(10,*) ' ep=',PSLVP,' epss=',GEVP,' epi=',GIVP
        DO 32 I = 1, NLAYER
            TRANI(I) = (ATRI(1, I) * DAYLEN + ATRI(2, I) * (1.0d0 - DAYLEN)) / DT
32      CONTINUE
!     TRAN from ATR(J) is not needed
!     zero daily integrators
        INCLUDE 'ZDAY.h'

!        * * * * * * * * B E G I N   P R E C I P   I N T E R V A L * * * * * * * * * * * *

        DO 200 N = 1, NPINT

            MESFLP = 0.0d0

            IF (NPINT .GT. 1) THEN
! vt this part is commented as I don't have PFILE.DAT file for now
! !               more than one precip interval in day, read line from PRFILE.DAT
!                 READ (10, *) YY, MM, DD, II, PREINT, MESFLP
                J = N + NPINT * (IDAY - 1)
                YY = INT( precdat( J, 1 ) )
                MM = INT( precdat( J, 2 ) )
                DD = INT( precdat( J, 3 ) )
                II = INT( precdat( J, 4 ) )
                PREINT = precdat( J, 5 )
                MESFLP = precdat( J, 6 )
! !             check PRFILE order
                INCLUDE 'PREINCHK.h'
                IF (MESFLP .LE. .000010d0) MESFLP = MESFL / DTP
            ELSE
!               one precip interval per day
!               PREINT comes from DFILE.DAT, NPINT = 1, and DTP = DT = 1 d
                MESFLP = MESFL / DTP
            END IF
            !        convert precipitation interval mm to rate in mm/d
            PREC = PREINT / DTP
            SFAL = SNOFRC * PREC
            RFAL = PREC - SFAL
            IF (NPINT .GT. 1) THEN
!               more than one precip interval in day
!               snow interception
                CALL INTER(SFAL, PINT, LAI, SAI, FSINTL, FSINTS, CINTSL, &
                    CINTSS, DTP, INTS, SINT, ISVP)
!               rain interception,  note potential interception rate is PID/DT-ISVP
                CALL INTER(RFAL, PINT - ISVP, LAI, SAI, FRINTL, FRINTS, &
                    CINTRL, CINTRS, DTP, INTR, RINT, IRVP)
            ELSE
!               one precip interval in day, use storm DURATN and INTER24
!               snow interception
                CALL INTER24(SFAL, PINT, LAI, SAI, FSINTL, FSINTS, CINTSL, &
                    CINTSS, DURATN(MONTH), INTS, SINT, ISVP)
!               rain interception,  note potential interception rate is PID/DT-ISVP
                CALL INTER24(RFAL, PINT - ISVP, LAI, SAI, FRINTL, FRINTS, &
                    CINTRL, CINTRS, DURATN(MONTH), INTR, RINT, IRVP)
            END IF
            !        throughfall
            RTHR = RFAL - RINT
            STHR = SFAL - SINT

!        reduce transpiration for fraction of precip interval that canopy is wet
            WETFR = MIN(1.0d0, (IRVP + ISVP) / PINT)
            PTRAN = (1.0d0 - WETFR) * PTRAN
            DO 41 I = 1, NLAYER
                TRANI(I) = (1.0d0 - WETFR) * TRANI(I)
41          CONTINUE

            IF (SNOW .LE. 0.0d0 .AND. STHR .LE. 0.0d0) THEN
!           no snow, soil evaporation weighted for WETFR
                SLVP = WETFR * GIVP + (1.0d0 - WETFR) * GEVP
                RNET = RTHR
                RSNO = 0.0d0
                SNVP = 0.0d0
                SMLT = 0.0d0
            ELSE
                IF (SNOW .LE. 0.0d0 .AND. STHR .GT. 0.0d0) THEN
!              new snow only, zero CC and SNOWLQ assumed
                    CC = 0.0d0
                    SNOWLQ = 0.0d0
                END IF
!           snow accumulation and melt
                CALL SNOWPACK(RTHR, STHR, PSNVP, SNOEN, CC, SNOW, SNOWLQ, &
                    DTP, TA, MAXLQF, GRDMLT, RSNO, SNVP, SMLT)
                RNET = RTHR - RSNO
                SLVP = 0.0d0
            END IF

            !        initialize for iterations
!        initial time remaining in iteration time step = precip time step
            DTRI = DTP
!        initialize iteration counter
            NITS = 0
!        zero precip interval integrators
            INCLUDE 'ZPINT.h'

            !        *  *  *  *  *  *  B E G I N   I T E R A T I O N   *  *  *  *  *  *  *

            ! Check for timelimits set by R-user
            if (timer .EQ. 1) call rchkusr()

101         CONTINUE
!         write(10,*)'begin iteration'
            NITS = NITS + 1
!         save old variables for heat flow
            IF (HEAT .EQ. 1) then
                DO 44 i = 1, NLAYER
                    TemperatureOld(i)=TemperatureNew(i)
                    HeatCapOld(i)=HeatCapNew(i)
44              CONTINUE
            END IF

            !           source area flow rate
!         write(10,*)' source area flow rate'
            IF (QLAYER .GT. 0) THEN
                CALL SRFLFR(QLAYER, SWATI, SWATQX, QFPAR, SWATQF, QFFC, SAFRAC)
            ELSE
                SAFRAC = 0.0d0
            END IF
            SRFL = MIN(1.0d0, (IMPERV + SAFRAC)) * (RNET + SMLT)
            !           water supply rate to soil surface
!         write(10,*)'water supply rate to soil surface'
            SLFL = RNET + SMLT - SRFL

            !           bypass fraction of infiltration to each layer
!         write(10,*)'bypass fraction of infiltration to each layer'
            CALL BYFLFR(BYPAR, NLAYER, WETNES, Par, QFFC, QFPAR, BYFRAC, MPar, ML)
!            write(10,*)'begin layer loop'
!           begin layer loop

            DO 51 I = NLAYER, 1, -1
!              downslope flow rates
                CALL DSLOP(DSLOPE, LENGTH, THICK(I), STONEF(I), PSIM(I), RHOWG, KK(I), DSFLI(I))
!              vertical flow rates
                IF (I .LT. NLAYER) THEN
                    IF (ABS(PSITI(I) - PSITI(I + 1)) .LT. DPSIMX) THEN
                        VRFLI(I) = 0.0d0
                    ELSE
                        CALL VERT(KK(I), KK(I + 1),Par(6,i),Par(6,i+1), THICK(I), THICK(I + 1), &
                             PSITI(I), PSITI(I + 1), STONEF(I), STONEF(I + 1), RHOWG, VRFLI(I))
                    END IF
                ELSE
!                 bottom layer
                    IF (DRAIN .GT. .000010d0) THEN
!                    gravity drainage only
                        VRFLI(NLAYER) = DRAIN * KK(NLAYER) * (1.0d0 - STONEF(NLAYER))
                    ELSE
                        VRFLI(NLAYER) = 0.0d0
                    END IF
                END IF
!               if(MM.eq.1.and.dd.eq.15.and.imodel.eq.0) then
!                write(10,*) 'vrfli ',i,VRFLI(i),kk(I),psim(i),DTI
!               end if
51          CONTINUE
!           end of layer loop


!           first approximation for iteration time step,time remaining or DTIMAX
            DTI = MIN(DTRI, DTIMAX)
!           net inflow to each layer including E and T withdrawal adjusted
!           for interception
            CALL INFLOW(NLAYER, DTI, INFRAC, BYFRAC, SLFL, VRFLI, DSFLI, &
                TRANI, SLVP, SWATMX, SWATI, VV, INFLI, BYFLI, NTFLI)

!           second approximation to iteration time step
            DO 61 I = 1, NLAYER
                DPSIDW(I) = FDPSIDW(WETNES(I),Par(1,i),iModel)
61          CONTINUE

            CALL ITER(NLAYER, DTI, DPSIDW, NTFLI, SWATMX, PSITI, DSWMAX, DPSIMX, DTINEW, &
                Thick, Wetnes, Par, iModel, TRANI,SLVP, MPar, ML, DTIMIN, pr)

            IF (DTINEW .LT. DTI) THEN
!              recalculate flow rates with new DTI
                DTI = DTINEW
                CALL INFLOW(NLAYER, DTI, INFRAC, BYFRAC, SLFL, VRFLI, DSFLI, TRANI, SLVP, &
                    SWATMX, SWATI, VV, INFLI, BYFLI, NTFLI)
            END IF

!           VV is the new VRFLI
            DO 71 I = 1, NLAYER
                VRFLI(I) = VV(I)
71          CONTINUE

!           groundwater flow and seepage loss
            CALL GWATER(GWAT, GSC, GSP, DT, VRFLI(NLAYER), GWFL, SEEP)
!           end of rate calculations

!           integrate below ground storages over iteration interval
            DO 81 I = 1, NLAYER
                SWATI(I) = SWATI(I) + NTFLI(I) * DTI
81          CONTINUE
            GWAT = GWAT + (VRFLI(NLAYER) - GWFL - SEEP) * DTI

!           new soil water variables and test for errors
            DO 91 I = 1, NLAYER
                INCLUDE 'SWCHEK.h'
                if(iModel.eq.0) then
                    WETNES(I) = SWATI(I) / SWATMX(I)
                end if
                if(iModel.eq.1) then
                    THS=Par(1,i)
                    THR=Par(10,i)
                    WETNES(I) = (THS * SWATI(I) / SWATMX(I) -THR) / (THS - THR)
                end if
                PSIM(I) = FPSIM(WETNES(I),Par(1,i),iModel)
91          CONTINUE

!            CALL SOILVAR(NLAYER, PSIG, PSIM, WETNES, THSAT, KF, BEXP,
!     *         WETF, SWATI, PSITI, THETA, KK, SWAT)
            CALL SOILVAR (NLAYER, iModel, Par, PSIG, PSIM, WETNES, &
                SWATI, PSITI, THETA, KK, SWAT, MPar, ML)

            !           heat flow
            IF (HEAT.EQ. 1) THEN
                IF (TopInfT.EQ. 1) THEN
                    IF (SNOW .GT. 0) THEN
                        tTop = TSNOW
!               if(NITS.eq.1) write(10,*)'TSnow: ',TSNOW
                    ELSE
                        tTop = TA
!               if(NITS.eq.1) write(10,*)'TA: ',TA
                    END IF
!        write(20,*)dd,'.',mm,'.',yy,'TSN:',TSNOW,'TA:',TA,'tTop:',tTop
                END IF
                inFil = SLFL * INFRAC(1) - SLVP
                do 92 i = 1, NLAYER
                    Sink(i) = ATRANI(I) *  THICK(I)
                    ThNew(i) = SWATI(I)/THICK(I)
                    vnew(i) = 0
92              continue
                call Temper(NLAYER, nmat, THICK, ZL, MUE, DTI, mat, &
                TemperatureOld,&
                TemperatureNew, HA, HB, HC, HD, TPar, &
                zeroCurRange, zeroCurTemp,&
                VRFLI,inFil,ThNew,HeatCapNew,HeatCapOld,&
                ThermCond,&
                tTop,tBot)
                do 95 i = 1, NLAYER
                    TMean(i) = TMean(i) + DTI/DTP * TemperatureNew(i)
95                continue
            END IF

!           flows accumulated over precip interval
            INCLUDE 'PACCUM.h'
           
!           time remaining in precipitation time-step
            DTRI = DTRI - DTI

            IF (DTRI .GT. 0) then
!***********************************************************
!      new!! update soil limited boundary flows and source/sink terms during iteration loop
                If(Reset.eq.1) then
                    IF (SNOW .GT. 0.0d0) THEN
                        RSS = 0.0d0
                    ELSE
!                       RSS = FRSS(RSSA, RSSB, Par(4,1), PSIM(1))
!                         RSS = FRSS (RSSA, RSSB, Par(1,1), PSIM(1),PsiCrit(1), iModel)
                        RSS = FRSS (RSSA, RSSB, Par(1,1), PSIM(1),PsiCrit(1))
                    END IF
!     * * * * *  B E G I N   D A Y - N I G H T   E T   L O O P  * * * * * * * *
                    !        write(10,*)'begin day-night et loop'
                    DO 301 JJ = 1, 2
                        J = JJ
!         1 for daytime, 2 for nighttime
!         net radiation
                        IF (J .EQ. 1) THEN
                            SLRAD = SLFDAY * SOLRAD / (WTOMJ * DAYLEN)
                            TAJ = TADTM
                            UAJ = UADTM
                        ELSE
                            SLRAD = 0.0d0
                            TAJ = TANTM
                            UAJ = UANTM
                        END IF
                        CALL AVAILEN(SLRAD, ALBEDO, C1, C2, C3, TAJ, EA, &
                            SOLRAD / I0HDAY, SHEAT, CR, LAI, SAI, AA(J), ASUBS(J),LNGNET(J))

!         vapor pressure deficit
                        CALL ESAT(TAJ, ES, DELTA)
!                       ^^  ^^^^^
                        VPD = ES - EA
!         Shuttleworth-Wallace resistances
                        CALL SWGRA(UAJ, ZA, HEIGHT, Z0, DISP, Z0C, DISPC, Z0GS, LWIDTH, &
                            RHOTP, NN, LAI, SAI, RAA, RAC, RAS)

                        IF (J .EQ. 1) THEN
                            CALL SRSC(SLRAD, TA, VPD, LAI, SAI, GLMIN, GLMAX, R5, CVPD,&
                                RM, CR, TL, T1, T2, TH, RSC)
                        ELSE
                            RSC = 1.0d0 / (GLMIN * LAI)
                        END IF
!         Shuttleworth-Wallace potential transpiration and ground evaporation
!         rates
                        CALL SWPE(AA(J), ASUBS(J), VPD, RAA, RAC, RAS, RSC, RSS, DELTA, PTR(J), GER(J))
!         Shuttleworth-Wallace potential interception and ground evap. rates
!         (RSC=0)
                        CALL SWPE(AA(J), ASUBS(J), VPD, RAA, RAC, RAS, 0.0d0, RSS, DELTA, PIR(J), GIR(J))
!         Shuttleworth-Wallace potential interception and ground evap. rates
!          (RSS=0)
                        CALL SWPE(AA(J), ASUBS(J), VPD, RAA, RAC, RAS, RSC, 0.0d0, DELTA, B0, PGER(J))
!         actual transpiration and ground evaporation rates
                        IF (PTR(J) .GT. .0010d0) THEN
                            CALL TBYLAYER(J, PTR(J), DISPC, ALPHA, KK, RROOTI, RXYLEM,&
                                PSITI, NLAYER, PSICR, NOOUTF, ATR(J), ATRANI)
                            DO 311 I = 1, NLAYER
                                ATRI(J, I) = ATRANI(I)
311                         CONTINUE
                            IF (ATR(J) .LT. PTR(J)) THEN
!            soil water limitation, new GER
                                CALL SWGE(AA(J), ASUBS(J), VPD, RAA, RAS, RSS, DELTA, ATR(J), GER(J))
                            END IF
                        ELSE
!          no transpiration, condensation ignored, new GER
                            PTR(J) = 0.0d0
                            ATR(J) = 0.0d0
                            DO 321 I = 1, NLAYER
                                ATRI(J, I) = 0.0d0
321                         CONTINUE
                            CALL SWGE(AA(J), ASUBS(J), VPD, RAA, RAS, RSS, DELTA, 0.0d0, GER(J))
                        END IF
301                 CONTINUE
!      * * * * * * * * *  E N D   D A Y - N I G H T   L O O P  * * * * * * * * * *
            !        average rates over day
                    PTRAN = (PTR(1) * DAYLEN + PTR(2) * (1.0d0 - DAYLEN)) / DT
                    GEVP = (GER(1) * DAYLEN + GER(2) * (1.0d0 - DAYLEN)) / DT
                    PINT = (PIR(1) * DAYLEN + PIR(2) * (1.0d0 - DAYLEN)) / DT
                    GIVP = (GIR(1) * DAYLEN + GIR(2) * (1.0d0 - DAYLEN)) / DT
                    PSLVP = (PGER(1) * DAYLEN + PGER(2) * (1.0d0 - DAYLEN)) / DT

                    DO 320 I = 1, NLAYER
                        TRANI(I) = (ATRI(1, I) * DAYLEN + ATRI(2, I) * (1.0d0 - DAYLEN)) / DT
320                 CONTINUE
!        TRAN from ATR(J) is not needed

                    IF (NPINT .GT. 1) THEN
!          more than one precip interval in day
!          snow interception
                        CALL INTER(SFAL, PINT, LAI, SAI, FSINTL, FSINTS, CINTSL, &
                            CINTSS, DTP, INTS, SINT, ISVP)
!        rain interception,  note potential interception rate is PID/DT-ISVP
                        CALL INTER(RFAL, PINT - ISVP, LAI, SAI, FRINTL, FRINTS, &
                        CINTRL, CINTRS, DTP, INTR, RINT, IRVP)
                    ELSE
!           one precip interval in day, use storm DURATN and INTER24
!           snow interception
                        CALL INTER24(SFAL, PINT, LAI, SAI, FSINTL, FSINTS, CINTSL, &
                            CINTSS, DURATN(MONTH), INTS, SINT, ISVP)
!           rain interception,  note potential interception rate is PID/DT-ISVP
                        CALL INTER24(RFAL, PINT - ISVP, LAI, SAI, FRINTL, FRINTS, &
                            CINTRL, CINTRS, DURATN(MONTH), INTR, RINT, IRVP)
                    END IF

!        throughfall
                    RTHR = RFAL - RINT
                    STHR = SFAL - SINT
!        reduce transpiration for fraction of precip interval that canopy is wet
                    WETFR = MIN(1.0d0, (IRVP + ISVP) / PINT)
                    PTRAN = (1.0d0 - WETFR) * PTRAN
                    DO 341 I = 1, NLAYER
                        if(PsiM(i).gt.PsiCrit(i)) then
                            TRANI(I) = (1.0d0 - WETFR) * TRANI(I)
                        else
                            TRANI(I)=0.0d0
                        end if
341                 CONTINUE


                    SLVP=0
                    IF (SNOW .LE. 0.0d0 .AND. STHR .LE. 0.0d0) THEN
!           no snow, soil evaporation weighted for WETFR
                        if(PsiM(1).gt.PsiCrit(1)) then
                            SLVP = WETFR * GIVP + (1.0d0 - WETFR) * GEVP
                        end if
                    end if
                end if
!*********************************************************************
                GO TO 101
            end if


!        *  *  *  *   E N D   I T E R A T I O N    L O O P  *  *  *  *  *  *  *  *  *  *  *
!           integrate interception storages over precip interval
            INTS = INTS + (SINT - ISVP) * DTP
            INTR = INTR + (RINT - IRVP) * DTP

!           flows for precip interval summed from components
           INCLUDE 'PSUM.h'

!           precipitation interval output
           INCLUDE 'output_pint.h'
           INCLUDE 'output_layer_pint.h'
           
!           flows accumulated over day 
           INCLUDE 'DACCUM.h'

!           accumulate iterations
            NITSD = NITSD + NITS
            NITSM = NITSM + NITS
            NITSY = NITSY + NITS
200     CONTINUE

! * * * * *  E N D   P R E C I P   I N T E R V A L   L O O P  * * * * * * * *

!     flows for day summed from components
        INCLUDE 'DSUM.h'


    ! daily outputs: STRES & BALERD
    ! stress factor - ratio of actual to potential transpiration
    ! separate, based on daily values.
        if (PTRAND .GT. 0.001d0) THEN
            STRES = TRAND / PTRAND 
        else
            STRES = 1.0d0
        end if
        if (STRES .GT. 1.d0) STRES = 1.d0
        output_day(NPINT*IDAY, 37) = STRES !STRES     
        
!   calc for daily water balance error 
        BALERD = STORD - (INTR + INTS + SNOW + SWAT + GWAT) + PRECD - EVAPD - FLOWD - SEEPD
        STORD = INTR + INTS + SNOW + SWAT + GWAT
        
        output_day(NPINT*IDAY, 42) = BALERD !STRES 

        IF (DOM .EQ. DAYMO(MONTH)) THEN
!        set up for next month
            MONTH = MONTH + 1
            DOM = 0
            NITSM = 0
        END IF

        IF (MONTH .EQ. 13) THEN
!        set up for next year
            MONTH = 1
            DOM = 0
            DOY = 0
            YEAR = YEAR + 1
!        planning ahead!!
!         IF (YEAR .EQ. 100) YEAR = 0
            NITSY = 0
            NITSM = 0
        END IF

        ! set up for next day
        DOM = DOM + 1
        DOY = DOY + 1
        IDAY = IDAY + 1

!        if( pr .eq. 1) then
!            if( MOD(IDAY,20) .eq. 2 ) then
!        call intpr("date         store  inter  snow    rain  et   drain  lat   ep    tp   mbal error", &
!                     -1, 0, 0)
!            end if

!            call intpr(makeString(13, (/real(yy,8),real(mm,8),real(dd,8), SWAT,IRVPD+ISVPD,SNOW, &
!                    PRECD,EVAPD,Flowd,DSFLD,PSLVP,PTRAN,BALERD/), &
!                40, '(F5.0,2F3.0,F6.0,2F7.2,F7.1,5F6.1,F12.3)'),&
!                -1, 0, 0)
!       end if

        TInt=TInt+IRVPD+ISVPD
        TSno=TSno+SNOW
        TPrec=TPrec+PrecD
        TEvap=TEvap+EvapD
        TVrFl=TVrFl+Flowd
        TSFLD=TSFLD+DSFLD

        go to 500

    end if

!    if( pr .eq. 1) then
!        call intpr(" ", -1, 0, 0)
!        call intpr("TOTAL:   inter     snow    rain    et      drain     lat", -1, 0, 0)
!        call intpr(makeString(6, (/TINT,TSNOW,TPREC,TEVAP,TVRFL,TSFLD/), 21,'(5X,2F9.2,F9.1,3F8.1)'), -1, 0, 0)
!    end if

!     ***************   E N D    D A Y   L O O P    **************************
999 if (pr .EQ. 1) then
        if ( error.ne.0 ) then
            call intpr("Finished with errors", -1,(/ 0/),0)
        else
            call intpr("THAT IS THE END", -1,(/ 0/),0)
        end if
    end if

    deallocate( PAR )
end subroutine s_brook90_f


!function makeString( nn, num, nf, fmt) result(res)
!    integer, intent(in) :: nn
!    real(kind=8), dimension(nn), intent(in) :: num
!    integer, intent(in) :: nf
!    character(len=nf), intent(in) :: fmt
!    character(len=255) :: res

!    write(res, fmt ) num
!end function makeString


!*************************************************************************************
! SUBROUTINES (in alphabetical order)

subroutine ACCUM (A1, A2, A3, A4, A5, B1, B2, B3, B4, B5)
!     accumulator; adds Aj to Bj
    IMPLICIT NONE
    real(kind=8) ::  A1, A2, A3, A4, A5, B1, B2, B3, B4, B5

    B1 = B1 + A1
    B2 = B2 + A2
    B3 = B3 + A3
    B4 = B4 + A4
    B5 = B5 + A5

end subroutine ACCUM

subroutine ACCUMI (N, A1, A2, A3, A4, A5, B1, B2, B3, B4, B5)
!     accumulator for array components; adds Aj(i) to Bj(i) for each i up to n
    IMPLICIT NONE
    integer :: N, I
    real(kind=8) :: A1(*), A2(*), A3(*), A4(*), A5(*)
    real(kind=8) :: B1(*), B2(*), B3(*), B4(*), B5(*)

    DO 10 I = 1, N
        B1(I) = B1(I) + A1(I)
        B2(I) = B2(I) + A2(I)
        B3(I) = B3(I) + A3(I)
        B4(I) = B4(I) + A4(I)
        B5(I) = B5(I) + A5(I)
10  CONTINUE
end subroutine ACCUMI

subroutine AVAILEN (SLRAD, ALBEDO, C1, C2, C3, TA, EA, RATIO, SHEAT, CR, LAI, SAI, AAR, ASUBSR, LNGNETR)
!     available energy at canopy and ground
!     longwave equations and parameters from Brutsaert (1982)
!     net radiation extinction from Shuttleworth and Wallace(1985)
    IMPLICIT NONE
!     input
    real(kind=8) :: SLRAD   ! solar radiation on slope, W/m2
    real(kind=8) :: ALBEDO  ! albedo
    real(kind=8) :: C1      ! intercept of relation of solar radiation to sunshine
                         ! duration
    real(kind=8) :: C2      ! slope of relation of solar radiation to sunshine
                         ! duration
    real(kind=8) :: C3      ! longwave correction factor for overcast sky
    real(kind=8) :: TA      ! air temperature, degC
    real(kind=8) :: RATIO   ! ratio of solar radiation on horizontal to potential
                         ! insolation for day
    real(kind=8) :: EA      ! vapor pressure, kPa
    real(kind=8) :: SHEAT   ! average soil heat flux for the day, W/m2, usually 0
    real(kind=8) :: CR      ! light extinction coefficient for projected LAI + SAI"
    real(kind=8) :: LAI     ! leaf area index, m2/m2
    real(kind=8) :: SAI     ! stem area index, m2/m2
!     output
    real(kind=8) :: AAR      ! available energy rate, W/m2
    real(kind=8) :: ASUBSR   ! availble energy rate at ground, W/m2
    real(kind=8) :: LNGNETR  ! net longwave radiation, W/m2
!     local
    real(kind=8) :: SOLNET  ! net solar radiation, W/m2
    real(kind=8) :: EFFEM   ! effective emissivity from clear sky
    real(kind=8) :: NOVERN  ! sunshine duration fraction of daylength
    real(kind=8) :: CLDCOR  ! cloud cover correction to net longwave under clear
                         ! sky

    real(kind=8) :: RN      ! net radiation, W/m2


    SOLNET = (1.0d0 - ALBEDO) * SLRAD
!     Brutsaert equation for effective clear sky emissivity
    EFFEM = 1.240d0 * (EA * 10.0d0 / (TA + 273.150d0)) ** (1.0d0 / 7.0d0)
    NOVERN = (RATIO - C1) / C2
    IF (NOVERN .GT. 1.0d0) NOVERN = 1.0d0
    IF (NOVERN .LT. 0.0d0) NOVERN = 0.0d0
    CLDCOR = C3 + (1.0d0 - C3) * NOVERN
!     emissivity of the surface taken as 1.0 to also account for reflected
    LNGNETR = (EFFEM - 1.0d0) * CLDCOR * SIGMA * (TA + 273.150d0) ** 4.0d0
    RN = SOLNET + LNGNETR
    AAR = RN - SHEAT
    ASUBSR = RN * EXP(-CR * (LAI + SAI)) - SHEAT

end subroutine AVAILEN

subroutine BYFLFR (BYPAR, NLAYER, WETNES, Par, QFFC, QFPAR, BYFRAC, MPar, ML)
!     bypass flow fraction of infiltration to layer
    IMPLICIT NONE
!     input
    integer :: BYPAR     ! 1 to allow BYFL, or 0 to prevent BYFL
    integer :: NLAYER    ! number of soil layers to be used in model, <= ML
    integer :: MPAR,ML   ! maximum number of parameters and layers
    real(kind=8) :: WETNES(*) ! wetness, fraction of saturation
!    real(kind=8) :: WETF(*)   ! wetness at field capacity, dimensionless
    real(kind=8) :: QFFC      ! BYFL fraction at field capacity
    real(kind=8) :: QFPAR     ! quick flow parameter
    real(kind=8) :: Par(MPar,ML)  ! parameter array
!     output
    real(kind=8) :: BYFRAC(*) ! fraction of layer infiltration to bypass flow
!     local
    integer :: I         ! counter

    DO 10 I = 1, NLAYER
        IF (BYPAR .EQ. 1) THEN
            BYFRAC(I) = QFFC ** (1.0d0 - (1.0d0 / QFPAR) * (WETNES(I) - Par(5,i)) / (1.0d0 - Par(5,i)))
            IF (BYFRAC(I) .GT. 1.0d0) BYFRAC(I) = 1.0d0
!           generate bypass flow to avoid saturation
            if(wetnes(i) .gt. 0.990d0) BYFRAC(I) = 1.0d0
        ELSE
            BYFRAC(I) = 0.0d0
        END IF
10  CONTINUE

end subroutine BYFLFR

subroutine CANOPY (SNOW, SNODEN, MXRTLN, MXKPL, DENSEF, HEIGHT, LAI, SAI, RTLEN,  RPLANT)
! subroutine CANOPY (SNOW, SNODEN, MXRTLN, MXKPL, CS, DENSEF, HEIGHT, LAI, SAI, RTLEN,  RPLANT)
!     canopy parameters
    IMPLICIT NONE
!     input
    real(kind=8) :: SNOW      ! water equivalent of snow on the ground, mm
    real(kind=8) :: SNODEN    ! snow density, mm/mm
    real(kind=8) :: MXRTLN    ! maximum root length per unit land area, m/m2
    real(kind=8) :: MXKPL     ! maximum plant conductivity, (mm/d)/MPa
!     real(kind=8) :: CS        ! ratio of projected SAI to canopy height, m-1, not needed in this version
    real(kind=8) :: DENSEF    ! density factor
!     output
    real(kind=8) :: HEIGHT    ! canopy height above any snow, m, minimum of 0.01 m
    real(kind=8) :: LAI       ! leaf area index, m2/m2, minimum of 0.00001
    real(kind=8) :: SAI       ! stem area index, m2/m2
    real(kind=8) :: RTLEN     ! root length per unit land area, m/m2
    real(kind=8) :: RPLANT    ! plant resistivity to water flow, MPa d/mm
!     local
    real(kind=8) :: SNODEP    ! snow depth
    real(kind=8) :: HNOSNO    ! height of canopy without snow
    real(kind=8) :: HSNO      ! height of canopy above snow
    real(kind=8) :: RATIO     ! fraction of canopy above snow
    real(kind=8) :: KPL       ! plant conductivity, mm d-1 MPa-1
!     intrinsic
!        REAL, MAX
!     external functions needed
!    real(kind=8) :: INTERP

    SNODEP = .0010d0 * SNOW / SNODEN
    HNOSNO = MAX(.010d0, HEIGHT)
    HSNO = MAX(0.0d0, HNOSNO - SNODEP)
    RATIO = HSNO / HNOSNO
    HEIGHT = MAX(.010d0, HSNO)

    LAI = RATIO * DENSEF * LAI
!      SAI = DENSEF * CS * HEIGHT           Original Brook90 expression replaced by direct input via climate.in
    SAI = DENSEF * SAI
    IF (LAI .LT. .000010d0) LAI = .000010d0

    RTLEN = DENSEF * MXRTLN
    KPL = DENSEF * MXKPL
    IF (KPL .LT. 1E-08) KPL = 1E-08
    RPLANT = 1.0d0 / KPL

end subroutine CANOPY

subroutine DSLOP (DSLOPE, LENGTH, THICK, STONEF, PSIM, RHOWG, KK, DSFLI)
!     downslope flow rate from layer
    IMPLICIT NONE
!     input
    real(kind=8) :: DSLOPE  ! slope for soil water flow, radians
                         ! no DSFLI if DSLOPE = 0
    real(kind=8) :: LENGTH  ! slope length (for DSFLI), m
    real(kind=8) :: THICK   ! layer thicknesses, mm
    real(kind=8) :: STONEF  ! stone volume fraction, unitless
    real(kind=8) :: PSIM    ! matric soil water potential, kPa
    real(kind=8) :: RHOWG   ! density of water times acceleration of gravity,
                         ! kPa/mm
    real(kind=8) :: KK      ! hydraulic conductivity, mm/d
!     output
    real(kind=8) :: DSFLI   ! downslope flow rate from layer, mm/d
!     local
    real(kind=8) :: LL      ! LENGTH in mm
    real(kind=8) :: GRAD    ! downslope potential gradient, kPa/mm
    real(kind=8) :: ARATIO  ! outflow area / map area
!     intrinsic
!        SIN, COS

    LL = 1000.0d0 * LENGTH

    GRAD = RHOWG * SIN(DSLOPE) + (2.0d0 * PSIM / LL) * COS(DSLOPE)
!      GRAD = RHOWG * SIN(DSLOPE)
    ARATIO = THICK * (1.0d0 - STONEF) * COS(DSLOPE) / LL
    DSFLI = KK * ARATIO * GRAD / RHOWG
!     no water uptake into dry soil because no free water at outflow face
    IF (DSFLI .LT. 0.0d0) DSFLI = 0.0d0

end subroutine DSLOP

subroutine EQUIVSLP (LAT, SLOPE, ASPECT, L1, L2)
!     latitude and time shift of "equivalent slope", the point on globe where a
!        horizontal surface is parallel to the slope
!     needed only once for each non-horizontal slope
!     Swift's L1 and L2, Lee (3.31, 3.32)
    IMPLICIT NONE
!     inputs
    real(kind=8) ::  LAT     ! latitude, radians (S neg)
    real(kind=8) :: SLOPE   ! slope, radians
    real(kind=8) :: ASPECT  ! aspect, radians from N thru E
!     outputs
    real(kind=8) :: L1      ! latitude of equivalent slope, radians
    real(kind=8) :: L2      ! time shift of equivalent slope, radians
!     local
    real(kind=8) :: D1
    real(kind=8) :: PI
!     intrinsic
!        SIN, COS, ATAN, ASIN
!
    PI = 3.141590d0

    L1 = ASIN(COS(SLOPE) * SIN(LAT) + SIN(SLOPE) * COS(LAT) * COS(ASPECT))
    D1 = COS(SLOPE) * COS(LAT) - SIN(SLOPE) * SIN(LAT) * COS(ASPECT)

    IF (D1 .EQ. 0.0d0) D1 = 1E-10

    L2 = ATAN(SIN(SLOPE) * SIN(ASPECT) / D1)

    IF (D1 .LT. 0.0d0) L2 = L2 + PI
end subroutine EQUIVSLP

subroutine ESAT (TA, ES, DELTA)
!     calculates saturated vp and DELTA from temp
!     from Murray J Applied Meteorol 6:203
    IMPLICIT NONE
!     input
    real(kind=8) :: TA      ! air temperature, degC
!     output
    real(kind=8) :: ES      ! saturated vapor pressure at TA, kPa
    real(kind=8) :: DELTA   ! dES/dTA at TA, kPa/K
!     intrinsic
!        EXP

    ES = .610780d0 * EXP(17.269390d0 * TA / (TA + 237.30d0))
    DELTA = 4098.0d0 * ES / (TA + 237.30d0) ** 2.0d0
    IF (TA .LT. 0.0d0) THEN
        ES = .610780d0 * EXP(21.874560d0 * TA / (TA + 265.50d0))
        DELTA = 5808.0d0 * ES / (TA + 265.50d0) ** 2.0d0
    END IF

end subroutine ESAT

subroutine GWATER (GWAT, GSC, GSP, DT, VRFLN, GWFL, SEEP)
!     calculates groundwater flow and seepage loss
    IMPLICIT NONE
!     input
    real(kind=8) :: GWAT    ! groundwater storage below soil layers, mm
    real(kind=8) :: GSC     ! discharge from GWAT, fraction per day, d-1
    real(kind=8) :: GSP     ! fraction of discharge to seepage
    real(kind=8) :: DT      ! time step for interval, d
    real(kind=8) :: VRFLN   ! vertical drainage rate from lowest layer, mm/d
!     output
    real(kind=8) :: GWFL    ! streamflow from groundwater discharge, mm/d
    real(kind=8) :: SEEP    ! deep seepage loss from groundwater, mm/d

    IF (GSC .LT. 1E-08) THEN
!        no groundwater
        SEEP = GSP * VRFLN
        GWFL = VRFLN - SEEP
    ELSE
        SEEP = GWAT * GSC * GSP
        GWFL = GWAT * GSC * (1.0d0 - GSP)
!        prevent negative GWAT
        IF (GWAT / DT - (GWFL + SEEP) .LT. 0.0d0) THEN
            SEEP = GSP * GWAT / DT
            GWFL = (1.0d0 - GSP) * GWAT / DT
        END IF
    END IF

end subroutine GWATER

subroutine INFLOW (NLAYER, DTI, INFRAC, BYFRAC, SLFL, VRFLI, DSFLI, TRANI, SLVP, SWATMX, &
    SWATI, VV, INFLI, BYFLI, NTFLI)
!     inflow and byflow for each layer, and net inflow including E and T
!        withdrawal
    IMPLICIT NONE
!     input
    integer :: NLAYER    ! number of soil layers being used, max of 20
    real(kind=8) :: DTI       ! time step for iteration interval, d
    real(kind=8) :: INFRAC(*) ! fraction of infiltration to each layer
    real(kind=8) :: BYFRAC(*) ! fraction of layer infiltration to bypass flow
    real(kind=8) :: SLFL      ! input rate to soil surface, mm/d
    real(kind=8) :: DSFLI(*)  ! downslope flow rate from layer, mm/d
    real(kind=8) :: TRANI(*)  ! transpiration rate from layer, mm/d
    real(kind=8) :: SLVP      ! evaporation rate from soil, mm/d
    real(kind=8) :: SWATMX(*) ! maximum water storage for layer, mm
    real(kind=8) :: SWATI(*)  ! water volume in layer, mm
    real(kind=8) :: VRFLI(*)  ! vertical drainage rate from layer, mm/d
!     output
    real(kind=8) :: VV(*)     ! modified VRFLI, mm/d
    real(kind=8) :: BYFLI(*)  ! bypass flow rate from layer, mm/d
    real(kind=8) :: INFLI(*)  ! infiltration rate into layer, mm/d
    real(kind=8) :: NTFLI(*)  ! net flow rate into layer, mm/d
!     local
    integer :: I         ! index variable for layer number
    real(kind=8) :: INFIL     ! water reaching layer, SLFL * INFRAC(I), mm/d
    real(kind=8) :: MAXIN     ! maximum allowed rate of input of water to layer,
                           ! mm/d

    DO 100 I = NLAYER, 1, -1
!        need to do from bottom up
        INFIL = SLFL * INFRAC(I)
        BYFLI(I) = BYFRAC(I) * INFIL
        INFLI(I) = INFIL - BYFLI(I)
        IF (I .EQ. NLAYER) VV(I) = VRFLI(I)
        IF (I .GT. 1) THEN
            MAXIN = (SWATMX(I) - SWATI(I)) / DTI + VV(I) + DSFLI(I) + TRANI(I)
            IF (VRFLI(I - 1) + INFLI(I) .GT. MAXIN) THEN
!              adjust to prevent oversaturation
                IF (BYFRAC(I) .GT. 0.0d0) THEN
                    IF (VRFLI(I - 1) .LT. MAXIN) THEN
!                    reduce INFLI, increase BYFLI
                        BYFLI(I) = BYFLI(I) + INFLI(I) - (MAXIN - VRFLI(I - 1))
                        INFLI(I) = MAXIN - VRFLI(I - 1)
                        VV(I - 1) = VRFLI(I - 1)
                    ELSE
!                    shift all INFLI to BYFLI and reduce VRFLI(I - 1)
                        BYFLI(I) = BYFLI(I) + INFLI(I)
                        INFLI(I) = 0.0d0
                        VV(I - 1) = MAXIN
                    END IF
                ELSE
!                 no bypass flow allowed, reduce VRFLI(I-1), INFLI(I) unchanged
                    VV(I - 1) = MAXIN - INFLI(I)
                END IF
            ELSE
!              BYFLI and INFLI unchanged
                VV(I - 1) = VRFLI(I - 1)
            END IF
            NTFLI(I) = VV(I - 1) + INFLI(I) - VV(I) - DSFLI(I) - TRANI(I)
        ELSE
!           I = 1
            MAXIN = (SWATMX(1) - SWATI(1)) / DTI + VV(1) + DSFLI(1) + TRANI(1) + SLVP
            IF (INFLI(1) .GT. MAXIN) THEN
!              increase BYFLI(1) to prevent oversaturation
                BYFLI(1) = BYFLI(1) + INFLI(1) - MAXIN
                INFLI(1) = MAXIN
!                 may be negative
            END IF
            NTFLI(1) = INFLI(1) - VV(1) - DSFLI(1) - TRANI(1) - SLVP
        END IF
100 CONTINUE

end subroutine INFLOW

subroutine INFPAR (INFEXP, ILAYER, NLAYER, THICK, INFRAC)
    IMPLICIT NONE
!     input
    real(kind=8) :: INFEXP    ! infiltration exponent, 0 all to top, 1 uniform
                           ! with depth
                           !     >1.0=more at bottom than at top
    integer :: ILAYER    ! number of layers over which infiltration is
                           ! distributed
    integer :: NLAYER    ! number of soil layers being used
    real(kind=8) :: THICK(*)  ! layer thicknesses, mm
!     output
    real(kind=8) :: INFRAC(*) ! fraction of infiltration to each layer
!     local
    real(kind=8) :: THICKT    ! total thickness of ILAYERs, mm
    real(kind=8) :: THICKA(0:NLAYER) ! accumulated thickness downward, mm
    integer :: I         ! do counter

    IF (INFEXP .LE. 0.) THEN
        INFRAC(1) = 1.0d0
        DO 10 I = 2, NLAYER
            INFRAC(I) = 0.0d0
10      CONTINUE
    ELSE
        THICKT = 0.0d0
        DO 20 I = 1, ILAYER
            THICKT = THICKT + THICK(I)
20      CONTINUE
        THICKA(0) = 0.0d0
        DO 30 I = 1, NLAYER
            IF (I .LE. ILAYER) THEN
                THICKA(I) = THICKA(I - 1) + THICK(I)
                INFRAC(I) = (THICKA(I) / THICKT) ** INFEXP - &
                    (THICKA(I - 1) / THICKT) ** INFEXP
            ELSE
                INFRAC(I) = 0.0d0
            END IF
30    CONTINUE
    END IF
end subroutine INFPAR

subroutine INTER (RFAL, PINT, LAI, SAI, FRINTL, FRINTS, CINTRL, CINTRS, DTP, INTR, RINT, IRVP)
!     rain interception, used when NPINT > 1
!     same routine is used for snow interception, with different calling
!        variables
    IMPLICIT NONE
!     input
    real(kind=8) :: RFAL     ! rainfall rate, mm/d
    real(kind=8) :: PINT     ! potential interception rate, mm/d
    real(kind=8) :: LAI      ! projected leaf area index, m2/m2
    real(kind=8) :: SAI      ! projected stem area index, m2/m2
    real(kind=8) :: FRINTL   ! intercepted fraction of RFAL per unit LAI
    real(kind=8) :: FRINTS   ! intercepted fraction of RFAL per unit SAI
    real(kind=8) :: CINTRL   ! maximum interception storage of rain per unit LAI, mm
    real(kind=8) :: CINTRS   ! maximum interception storage of rain per unit SAI, mm
    real(kind=8) :: DTP      ! precipitation interval time step, d
    real(kind=8) :: INTR     ! intercepted rain, mm
!     output
    real(kind=8) :: RINT     ! rain catch rate, mm/d
    real(kind=8) :: IRVP     ! evaporation rate of intercepted rain, mm/d
!     local
    real(kind=8) :: INTRMX   ! maximum canopy storage for rain, mm
    real(kind=8) :: CATCH    ! maximum RINT, mm/d
    real(kind=8) :: NEWINT   ! first approximation to new canopy storage (INTR)

    CATCH = (FRINTL * LAI + FRINTS * SAI)
    If (Catch .GT. 1.0d0) Catch = 1.0d0
    CATCH = CATCH * RFAL
    INTRMX = CINTRL * LAI + CINTRS * SAI
    NEWINT = INTR + (CATCH - PINT) * DTP
    IF (NEWINT .GT. 0.0d0) THEN
!        canopy is wet throughout DTP
        IRVP = PINT
        IF (NEWINT .GT. INTRMX) THEN
!           canopy capacity is reached
            RINT = PINT + (INTRMX - INTR) / DTP
!           RINT can be negative if INTR exists and LAI or SAI is
!              decreasing over time
        ELSE
!           canopy capacity is not reached
            RINT = CATCH
        END IF
    ELSE
!        canopy dries during interval or stays dry
        RINT = CATCH
        IRVP = (INTR / DTP) + CATCH
!       IRVP is < PINT
    END IF

end subroutine INTER

subroutine INTER24 (RFAL, PINT, LAI, SAI, FRINTL, FRINTS, CINTRL, CINTRS, DURATN, INTR, RINT, IRVP)
!     rain interception with duration in hours, used when NPINT = 1
!     same routine is used for snow interception, with different calling
!        variables
    IMPLICIT NONE
!     input
    real(kind=8) :: RFAL     ! 24-hour average rainfall rate, mm/d
    real(kind=8) :: PINT     ! potential interception rate, mm/d
    real(kind=8) :: LAI      ! projected leaf area index, m2/m2
    real(kind=8) :: SAI      ! projected stem area index, m2/m2
    real(kind=8) :: FRINTL   ! intercepted fraction of RFAL per unit LAI
    real(kind=8) :: FRINTS   ! intercepted fraction of RFAL per unit SAI
    real(kind=8) :: CINTRL   ! maximum interception storage of rain per unit LAI, mm
    real(kind=8) :: CINTRS   ! maximum interception storage of rain per unit SAI, mm
    real(kind=8) :: DURATN   ! average storm duration, hr
    real(kind=8) :: INTR     ! intercepted rain storage, mm,
!     output
    real(kind=8) :: RINT     ! rain catch rate, mm/d
    real(kind=8) :: IRVP     ! evaporation rate of intercepted rain, mm/d
!     local
    real(kind=8) :: INTRMX   ! maximum canopy storage for rain, mm
    real(kind=8) :: INTRNU   ! canopy storage at end of hour, mm
    real(kind=8) :: NEWINT   ! first approximation to INTRNU, mm
    real(kind=8) :: RINTHR   ! rain catch rate for hour, mm/hr
    real(kind=8) :: CATCH    ! maximum RINTHR, mm/hr
    real(kind=8) :: IRVPHR   ! evaporation rate for hour, mm/hr
    real(kind=8) :: SMINT    ! daily accumulated actual catch, mm
    real(kind=8) :: SMVP     ! daily accumulated actual evaporation, mm
    integer :: IHD     ! half DURATN in truncated integer hours
    integer :: I       ! hour, 0 to 23
    real(kind=8) :: DTH      ! time step, = 1 hr
!     intrinsic
!       REAL, INT
    IHD = INT((DURATN + .10d0) / 2.0d0)
    INTRMX = CINTRL * LAI + CINTRS * SAI
    INTRNU = INTR
    SMINT = 0.0d0
    SMVP = 0.0d0
    DTH = 1.0d0
    DO 100 I = 0, 23
        IF (I .LT. (12 - IHD) .OR. I .GE. (12 + IHD)) THEN
!           before or after rain
            CATCH = 0.0d0
        ELSE
!           during rain, mm/hr is rate in mm/d divided by hr of rain/d
            CATCH = (FRINTL * LAI + FRINTS * SAI)
            If (Catch .GT. 1.0d0) Catch = 1.0d0
            CATCH = Catch * RFAL / REAL(2 * IHD, kind=8)
        ENDIF
        NEWINT = INTRNU + (CATCH - PINT / 24.0d0) * DTH
        IF (NEWINT .GT. .00010d0) THEN
!           canopy is wet throughout hour, evap rate is PINT
            IRVPHR = PINT / 24.0d0
            IF (NEWINT .GT. INTRMX) THEN
!              canopy capacity is reached
                RINTHR = IRVPHR + (INTRMX - INTRNU) / DTH
!               INTRMX - INTRNU can be negative if LAI or SAI is decreasing
!                 over time
            ELSE
!              canopy capacity is not reached
               RINTHR = CATCH
            END IF
        ELSE
!           canopy dries during hour or stays dry
            RINTHR = CATCH
            IRVPHR = INTRNU / DTH + CATCH
!           IRVPHR for hour is < PI/24
        END IF
        INTRNU = INTRNU + (RINTHR - IRVPHR) * DTH
        SMVP = SMVP + IRVPHR * DTH
        SMINT = SMINT + RINTHR * DTH
100 CONTINUE
    IRVP = SMVP
!                 / 1 d
    RINT = SMINT
!                 / 1 d

end subroutine INTER24

subroutine ITER (NLAYER, DTI, DPSIDW, NTFLI, SWATMX, PSITI, DSWMAX, DPSIMX, DTINEW, &
     Thick, Wetnes, Par, iModel, TRANI,SLVP, MPar, ML, DTIMIN, pr)
! subroutine ITER (NLAYER, DTI, DPSIDW, NTFLI, SWATMX, PSITI, DSWMAX, DPSIMX, DTINEW, SWATI, &
!      Thick, Wetnes, Par, iModel, TRANI,SLVP, MPar, ML, DTIMIN)
    IMPLICIT NONE
!     input
    integer :: NLAYER    ! number of soil layers to be used in model
    integer :: MPAR,ML   ! maximum number of parameters and layers
    real(kind=8) :: DTI       ! time step for iteration interval, d
    real(kind=8) :: DTIMIN    ! minimum time step for iteration interval, d
    real(kind=8) :: DPSIDW(*) ! rate of change of total potential with water
                           ! content, kPa/mm
    real(kind=8) :: NTFLI(*)  ! net flow rate into layer, mm/d
    real(kind=8) :: Thick(*)  ! thickness of layer, mm
    real(kind=8) :: Wetnes(*) ! water saturation of layer
    real(kind=8) :: TRANI(*)  ! actual transpiration (also output after check)
    real(kind=8) :: SLVP      ! actual evaporation (also output after check)
    real(kind=8) :: Par(MPar,ML)  ! parameter array
    integer :: iModel    ! type of parameterization of hydraulic functions
    real(kind=8) :: SWATMX(*) ! maximum water storage for layer, mm
!     real(kind=8) :: SWATI(*)  ! actual water storage for layer, mm
    real(kind=8) :: PSITI(*)  ! total potential, kPa
    real(kind=8) :: DSWMAX    ! maximum change allowed in SWATI, percent of
                           ! SWATMX(i)
    real(kind=8) :: DPSIMX    ! maximum potential difference considered
                           ! "equal", kPa
    integer :: pr  ! print output to console?

!     output
    real(kind=8) :: DTINEW    ! second estimate of DTI
!     local
    real(kind=8) :: A(NLAYER)
    real(kind=8) :: T(NLAYER)
    real(kind=8) :: TT        ! new potential difference between layers
    real(kind=8) :: PP        ! original potential difference between layers
    real(kind=8) :: Thr,Th,K,psi  ! residual and actual water content
!     real(kind=8) :: Eps       ! minimal time step, d
    integer :: I,J       ! do counter
!     function
!     real(kind=8) :: FTheta,FK,FPSIM,FRSS

!     first approximation to new total potential
    DO 10 I = 1, NLAYER
        if(iModel.eq.0) then
            A(I) = NTFLI(I) * DPSIDW(I) / SWATMX(I)
        end if
        if(iModel.eq.1) then
            A(I) = NTFLI(I)/Thick(i) * DPSIDW(I) / (Par(1,i) - Par(10,i))
        end if
        T(I) = PSITI(I) + A(I) * DTI
10  CONTINUE
!     test to see if DTI should be reduced
    DTINEW = DTI
    DO 20 I = 1, NLAYER
!        prevent too large a change in water content
        DTINEW = MIN(DTINEW, .010d0 * DSWMAX * SWATMX(I) / MAX(.0000010d0, ABS(NTFLI(I))))
!        prevent a change in water content larger than total available water
        if((iModel.eq.1).and.(NTFLI(I).lt.0)) then   !
            Thr=Par(10,i)
            Th=FTheta(Wetnes(i),Par(1,i),iModel)
            DTINEW = MIN(DTINEW,(Thr-Th)*Thick(i)/NTFLI(I)/1.30d0)
            if(DTINEW.lt.DTIMIN) then
                Thr=Par(10,i)
                Th=FTheta(Wetnes(i),Par(1,i),iModel)
                psi=FPSIM(Wetnes(i),Par(1,i),iModel)
                K= FK(Wetnes(i),Par(1,i),iModel)
                DO 21 J = 1, NLAYER
                    Th=FTheta(Wetnes(j),Par(1,j),iModel)
                    Thr=Par(10,j)
                    psi=FPSIM(Wetnes(j),Par(1,j),iModel)
                    K= FK(Wetnes(j),Par(1,j),iModel)

                    if (pr .EQ. 1) then
                        call realpr('xxx i=, th=, thr=, netflow=, thick=, K=, Psi=', -1, &
                            (/real(j,8),th,thr,NTFLI(j),Thick(j),K,PSI/), 7)
                    end if
21              continue
                DTINEW=DTIMIN
                TRANI(i)=0
                if(i.eq.1) SLVP=0
            end if
        end if
        if((iModel.eq.0).and.(NTFLI(I).lt.0)) then   !
            Th=FTheta(Wetnes(i),Par(1,i),iModel)
            DTINEW = MIN(DTINEW,-Th*Thick(i)/NTFLI(I)/1.30d0)
            if(DTINEW.lt.DTIMIN) then
                DO 22 J = 1, NLAYER
                    Th=FTheta(Wetnes(j),Par(1,j),iModel)
                    Thr=Par(10,j)
                    psi=FPSIM(Wetnes(j),Par(1,j),iModel)
                    K= FK(Wetnes(j),Par(1,j),iModel)

                    if (pr .EQ. 1) then
                        call realpr('xxx i=, th=, netflow=, thick=, K=, Psi=', -1, &
                            (/real(j,8),th,NTFLI(j),Thick(j),K,PSI/),7)
                    end if
22              continue
                DTINEW=DTIMIN
                TRANI(i)=0
                if(i.eq.1) SLVP=0
            end if
        end if
!        prevent oscillation of potential gradient
        if(iModel.eq.0) then
            IF (I .LT. NLAYER) THEN
!           total potential difference at beginning of iteration
                PP = PSITI(I) - PSITI(I + 1)
!           first approx to total potential difference at end of iteration
                TT = T(I) - T(I + 1)
                IF (ABS(TT) .GT. DPSIMX .AND. ABS(PP) .GT. DPSIMX) THEN
                    IF (TT .LT. 0.0d0 .AND. PP .GT. 0.0d0 .OR. TT .GT. 0.0d0 .AND. PP .LT. 0.0d0) THEN
                        DTINEW = MIN(DTINEW, -PP / (A(I) - A(I + 1)))
                        DTINEW = Max(DTINEW, DTIMIN)
                    END IF
                END IF
            END IF
        end if
20  CONTINUE

end subroutine ITER

subroutine MinPsi (ThCrit,PsiCRit,Par,iModel,NLAYER,MPar,ML)
!     minimum soil matric potential to allow water supply for evapotranspiration , Hammel, 2001
    IMPLICIT NONE
!     input
    real(kind=8) :: ThCrit    !  minimal fraction of water content above residual water content to allow water supply for evapotranspiration
    real(kind=8) :: PsiCRit(*)! actual relative values of root length per unit volume
    integer :: I         ! iteration index
    integer :: iModel    ! hydraulic parameterization
    integer :: MPAR,ML   ! maximum number of parameters and layers
    integer :: NLAYER    ! number of soil layers (max 1000)
    real(kind=8) :: Par(MPar,ML ) ! hydraulic parameters
!     local
    real(kind=8) :: WetCrit   ! minimal wetness corresponding to ThCrit
!     functions
!     real(kind=8) :: FPSIM

    if(iModel.eq.0) then
        DO 10 I = 1, NLAYER
            WetCrit=ThCrit/Par(1,i)
            PsiCrit(i)=FPSIM(WetCrit,Par(1,i),iModel)
10      Continue
    end if
    if(iModel.eq.1) then
        DO 11 I = 1, NLAYER
            WetCrit=ThCrit/(Par(1,i)-Par(10,i))
            PsiCrit(i)=FPSIM(WetCrit,Par(1,i),iModel)
11      Continue
    end if
end subroutine MinPsi

subroutine PLNTRES (NLAYER, THICK, STONEF, RTLEN, RELDEN, RTRAD, RPLANT, FXYLEM, RXYLEM, RROOTI, ALPHA)
!     allocates total plant resistance to xylem and root layers
    IMPLICIT NONE
!     input
    integer :: NLAYER ! number of soil layers (max 50)
    real(kind=8) :: THICK(*) ! layer thicknesses, mm
    real(kind=8) :: STONEF(*)! stone volume fraction, unitless
    real(kind=8) :: RTLEN    ! root length per unit land area, m/m2,
                        ! RTLEN * DENSEF
    real(kind=8) :: RELDEN(*)! relative values of root length per unit volume
    real(kind=8) :: RTRAD    ! average root radius, mm
    real(kind=8) :: RPLANT   ! plant resistance to water flow, MPa d/mm,
                        ! 1/(KPLANT*DENSEF)
    real(kind=8) :: FXYLEM   ! fraction of plant resistance in xylem
!     output
    real(kind=8) :: RXYLEM   ! xylem resistance, MPa d/mm, 1E20 if no roots
    real(kind=8) :: RROOTI(*)! root resistance for layer, MPa d/mm, 1E20 if no roots
    real(kind=8) :: ALPHA(*) ! modified Cowan alpha, MPa
!     local
    integer :: I       ! layer counter
    real(kind=8) :: D(NLAYER)   ! stonefree layer thickness
    real(kind=8) :: SUM      ! total relative length, mm
    real(kind=8) :: RTFRAC   ! fraction of total root length in layer
    real(kind=8) :: RTDENI   ! root density for layer, mm/mm3
    real(kind=8) :: DELT     ! root cross-sectional area * LI, dimensionless
!     intrinsic
!       LOG

!     xylem resistance
    RXYLEM = FXYLEM * RPLANT

    SUM = 0.0d0
    DO 10 I = 1, NLAYER
        D(I) = THICK(I) * (1.0d0 - STONEF(I))
        SUM = SUM + RELDEN(I) * D(I)
10  CONTINUE
    DO 20 I = 1, NLAYER
        IF (RELDEN(I) .LT. .000010d0 .OR. RTLEN .LT. .10d0) THEN
!           no roots in layer
            RROOTI(I) = 1E+20
            ALPHA(I) = 1E+20
        ELSE
            RTFRAC = RELDEN(I) * D(I) / SUM
!           root resistance for layer
            RROOTI(I) = (RPLANT - RXYLEM) / RTFRAC
!           rhizosphere resistance for layer
            RTDENI = RTFRAC * .0010d0 * RTLEN / D(I)
!                            .001 is (mm/mm2)/(m/m2) conversion
            DELT = PI * RTRAD ** 2.0d0 * RTDENI
            ALPHA(I) = (1.0d0 / (8.0d0 * PI * RTDENI)) * (DELT - 3.0d0 - 2.0d0 * (LOG(DELT)) / (1.0d0 - DELT))
            ALPHA(I) = ALPHA(I) * .0010d0 * RHOWG / D(I)
!                            .001 is MPa/kPa conversion
        END IF
20  CONTINUE

end subroutine PLNTRES

subroutine RootGrowth (frelden, RELDEN, tini, age, rgroper, inirdep, inirlen, NLAYER)
!     root growth according to LWF root growth model, Hammel and Kennel, 2000
    IMPLICIT NONE
!     input
    real(kind=8) :: frelden(*)! final relative values of root length per unit volume
    real(kind=8) :: RELDEN(*) ! actual relative values of root length per unit volume
    real(kind=8) :: tini(*)   ! initial time for root growth in layer
    real(kind=8) :: age       ! age of vegetation
    real(kind=8) :: rgroper   ! period of root growth in layer, a
    real(kind=8) :: inirdep   ! intial root depth, m
    real(kind=8) :: inirlen   ! intial total root length, m m-2
    real(kind=8) :: rl0       ! constant intial root length density, m m-3
    integer :: NLAYER    ! number of soil layers (max 1000)
    integer ::I         ! number of soil layers (max 1000)

!      write(10,*)'root growth'
    if(rgroper .GT. 0.0d0) then
        DO 10 I = 1, NLAYER
            if(age .LT. tini(I)) then
                RELDEN(I)=0.0d0
            end if
            if((age.GE.tini(I)).AND.(age.LE.rgroper+tini(I))) then
                rl0=inirlen/inirdep
                RELDEN(I)=rl0*(frelden(I)/rl0)**((age-tini(I))/rgroper)
            end if
            if(age.GT.rgroper+tini(I)) then
                RELDEN(I)=frelden(I)
            end if
10      Continue
    else
        DO 11 I = 1, NLAYER
            RELDEN(I)=frelden(I)
11      Continue
    end if

end subroutine RootGrowth

subroutine ROUGH (HEIGHT, ZMINH, LAI, SAI, CZS, CZR, HS, HR, LPC, CS, Z0G, Z0C, DISPC, Z0, DISP, ZA)
!     closed canopy parameters
    IMPLICIT NONE
!     input
    real(kind=8) :: HEIGHT    ! canopy height, m, minimum of 0.01 m
    real(kind=8) :: ZMINH     ! ZA minus HEIGHT,reference height above canopy top,m
    real(kind=8) :: LAI       ! leaf area index, m2/m2, minimum of 0.00001
    real(kind=8) :: SAI       ! stem area index, m2/m2
    real(kind=8) :: CZS       ! ratio of roughness to height for smooth closed
                          ! canopies
    real(kind=8) :: CZR       ! ratio of roughness to height for rough closed
                          ! canopies
    real(kind=8) :: HS        ! height below which CZS applies, m
    real(kind=8) :: HR        ! height above which CZR applies, m
    real(kind=8) :: LPC       ! minimum LAI defining a closed canopy
    real(kind=8) :: CS        ! ratio of projected SAI to canopy height, m-1
!     input and output, ZOGS in B90
    real(kind=8) :: Z0G       ! roughness parameter of soil surface, m
!     output
    real(kind=8) :: Z0C       ! roughness length for closed canopy, m
    real(kind=8) :: DISPC     ! zero-plane displacement for closed canopy, m
    real(kind=8) :: Z0        ! roughness parameter, m
    real(kind=8) :: DISP      ! zero-plane displacement, m
    real(kind=8) :: ZA        ! reference height for TA, EA, UA, above ground, m
!     local
    real(kind=8) :: RATIO     ! (LAI + SAI) / (LAI + SAI for closed canopy)
    real(kind=8) :: XX
!     intrinsic
!        LOG, EXP, MIN

    IF (HEIGHT .GE. HR) THEN
        Z0C = CZR * HEIGHT
    ELSEIF (HEIGHT .LE. HS) THEN
        Z0C = CZS * HEIGHT
    ELSE
        Z0C = CZS * HS + (CZR * HR - CZS * HS) * (HEIGHT - HS) / (HR - HS)
    END IF
    DISPC = HEIGHT - Z0C / .30d0
    IF (Z0G .GT. Z0C) Z0G = Z0C
    RATIO = (LAI + SAI) / (LPC + CS * HEIGHT)
    IF (RATIO .GE. 1.0d0) THEN
!        closed canopy
        Z0 = Z0C
        DISP = DISPC
    ELSE
!        sparse canopy modified from Shuttleworth and Gurney (1990)
        XX = RATIO * (-1.0d0 + EXP(.9090d0 - 3.030d0 * Z0C / HEIGHT)) ** 4.0d0
        DISP = 1.10d0 * HEIGHT * LOG(1.0d0 + XX ** .250d0)
        Z0 = MIN(.30d0 * (HEIGHT - DISP), Z0G + .30d0 * HEIGHT * XX ** .50d0)
    END IF
    ZA = HEIGHT + ZMINH

end subroutine ROUGH

subroutine SNOENRGY (TSNOW, TA, DAYLEN, CCFAC, MELFAC, SLFDAY, LAI, SAI, LAIMLT, SAIMLT, SNOEN)
!     snow surface energy balance
    IMPLICIT NONE
!     input
    real(kind=8) :: TSNOW   ! snowpack temperature (isothermal assumed), degC
    real(kind=8) :: TA      ! "mean" temperature for the day, C
    real(kind=8) :: DAYLEN  ! daylength in fraction of day
    real(kind=8) :: CCFAC   ! cold content factor, MJ m-2 d-1 K-1
    real(kind=8) :: MELFAC  ! degree day melt factor for open, MJ m-2 d-1 K-1
    real(kind=8) :: SLFDAY  ! ratio of potential insolation on slope to on
                         ! horizontal for day
    real(kind=8) :: LAI     ! leaf area index, m2/m2
    real(kind=8) :: SAI     ! stem area index, m2/m2
    real(kind=8) :: LAIMLT  ! parameter for snowmelt dependence on LAI,
                         ! dimensionless
    real(kind=8) :: SAIMLT  ! parameter for snowmelt dependence on SAI,
                         ! dimensionless
!     output
    real(kind=8) :: SNOEN   ! energy flux density to snow surface, MJ m-2 d-1
!     intrinsic
!        EXP

    IF (TA .LE. 0) THEN
!        snow warms or cools proportional to snow-air temperature difference
        SNOEN = CCFAC * 2.0d0 * DAYLEN * (TA - TSNOW)
    ELSE
!        energy input proportional to TA, modified by cover, slope-aspect,
!           and daylength
        SNOEN = MELFAC * 2.0d0 * DAYLEN * TA * EXP(-SAIMLT * SAI) * EXP(-LAIMLT * LAI) * SLFDAY
    END IF

end subroutine SNOENRGY

subroutine SNOFRAC (TMAX, TMIN, RSTEMP, SNOFRC)
!     separates RFAL from SFAL
    IMPLICIT NONE
!     input
    real(kind=8) :: TMAX    ! maximum temperature for the day, C
    real(kind=8) :: TMIN    ! minimum temperature for the day, C
    real(kind=8) :: RSTEMP  ! base temperature for snow-rain transition, C
!     output
    real(kind=8) :: SNOFRC  ! fraction of precipitation for the day as SFAL,
                         ! unitless

    IF (TMIN .GE. RSTEMP) THEN
        SNOFRC = 0.0d0
    ELSEIF (TMAX .LT. RSTEMP) THEN
        SNOFRC = 1.0d0
    ELSE
        SNOFRC = 1 - (TMAX - RSTEMP) / (TMAX - TMIN)
    END IF

end subroutine SNOFRAC

subroutine SNOVAP (TSNOW, TA, EA, UA, ZA, HEIGHT, Z0, DISP, Z0C, DISPC, Z0GS, LWIDTH, &
    RHOTP, NN, LAI, SAI, KSNVP, PSNVP)
!     snow evaporation and condensation
    IMPLICIT NONE
!     input
    real(kind=8) :: DISP    ! zero-plane displacement, m
    real(kind=8) :: DISPC   ! zero-plane displacement for closed canopy of HEIGHT,m
    real(kind=8) :: EA      ! vapor pressure for the day, kPa
    real(kind=8) :: HEIGHT  ! canopy height, m
    real(kind=8) :: KSNVP   ! multiplier to fix snow evaporation problem
    real(kind=8) :: LAI     ! leaf area index, m2/m2
    real(kind=8) :: LWIDTH  ! leaf width, m
    real(kind=8) :: NN      ! wind/diffusivity extinction coefficient
    real(kind=8) :: RHOTP   ! ratio of total leaf area to projected area
    real(kind=8) :: SAI     ! stem area index, m2/m2
    real(kind=8) :: TA      ! mean  temperature for the day at reference height,C
    real(kind=8) :: TSNOW   ! snowpack temperature (isothermal assumed), C
    real(kind=8) :: UA      ! average wind speed for the day at reference height,
                         ! m/s
    real(kind=8) :: Z0      ! roughness parameter, m
    real(kind=8) :: Z0C     ! roughness parameter for closed canopy of HEIGHT, m
    real(kind=8) :: Z0GS    ! snow surface roughness, m
    real(kind=8) :: ZA      ! reference height for TA, EA, UA, above ground, m
!     output
    real(kind=8) :: PSNVP   ! potential snow evaporation, mm/d
!     local
    real(kind=8) :: DUMMY
    real(kind=8) :: ESNOW   ! vapor pressure at snow surface, kPa
    real(kind=8) :: RAA     ! Shuttleworth-Wallace atmosphere aerodynamic
                         ! resistance, s/m
    real(kind=8) :: RAC     ! Shuttleworth-Wallace canopy aerodynamic
                         ! resistance, s/m
    real(kind=8) :: RAS     ! Shuttleworth-Wallace ground aerodynamic resistance,
                         ! s/m
!     intrinsic
!        MIN

!        ignores effect of interception on PSNVP or of PSNVP on PTRAN
    IF (TSNOW .GT. -.10d0) THEN
        ESNOW = .610d0
    ELSE
!           snow surface vapor pressure saturated at lower of TA and TSNOW
        CALL ESAT(MIN(TA, TSNOW), ESNOW, DUMMY)
    END IF
    CALL SWGRA(UA, ZA, HEIGHT, Z0, DISP, Z0C, DISPC, Z0GS, LWIDTH, RHOTP, NN, LAI, SAI, RAA, RAC, RAS)

!                        ^^^       ^^^
    PSNVP = (WTOMJ / LS) * (CPRHO / GAMMA) * (ESNOW - EA) / (RAA + RAS)
!     fix for PSNVP overestimate
    PSNVP = KSNVP * PSNVP

end subroutine SNOVAP

subroutine SNOWPACK (RTHR, STHR, PSNVP, SNOEN, CC, SNOW, SNOWLQ, DTP, TA, MAXLQF, GRDMLT, RSNO, SNVP, SMLT)
!        adds net snowfall or rainfall to snowpack, subtracts groundmelt,
!           evaporation, and melt
    IMPLICIT NONE
!     input
    real(kind=8) ::  RTHR    ! rain throughfall rate, mm/d
    real(kind=8) ::  STHR    ! snow throughfall rate, mm/d
    real(kind=8) ::  PSNVP   ! potential evaporation rate from snowpack, mm/d
    real(kind=8) ::  SNOEN   ! energy flux density to snow surface, MJ m-2 d-1
    real(kind=8) ::  DTP     ! time step for precipitation interval, may be <= 1 d
    real(kind=8) ::  TA      ! "mean" temperature for the day, C
    real(kind=8) ::  MAXLQF  ! maximum liquid water fraction of SNOW, dimensionless
    real(kind=8) ::  GRDMLT  ! rate of groundmelt of snowpack, mm/d
!     input and output
    real(kind=8) ::  CC      ! cold content of snowpack (positive), MJ/m2
    real(kind=8) ::  SNOW    ! water equivalent of snow on the ground, mm
    real(kind=8) ::  SNOWLQ  ! liquid water content of snow on the ground, mm
!     output
    real(kind=8) ::  RSNO    ! rain added to snowpack, mm/d
    real(kind=8) ::  SNVP    ! evaporation rate from snowpack, mm/d
    real(kind=8) ::  SMLT    ! melt drainage rate from snowpack, mm/d
!     local
    real(kind=8) ::  FRAC    ! groundmelt and evaporation fraction of SNOW,
                         ! dimensionless
    real(kind=8) ::  EQEN    ! meltwater equivalent of energy input, including
                         ! warm rain, mm
    real(kind=8) ::  NMLT    ! -EQEN when EQEN is negative, "negative melt", mm
    real(kind=8) ::  ALQ     ! MAXLQF*SNOW - SNOWLQ, available space for liquid
                         ! water, mm
    real(kind=8) ::  RIN     ! RTHR*DTP, rain input to snow, mm
!     intrinsic
!        MIN, MAX
!     snow throughfall and its cold content, SNOWLQ unchanged
    SNOW = SNOW + STHR * DTP
    CC = CC + CVICE * MAX(-TA, 0.0d0) * STHR * DTP
    IF (CC .GT. 0.0d0 .AND. SNOWLQ .GT. 0.0d0) THEN
        IF (CC .GT. SNOWLQ * LF) THEN
!           refreeze all liquid
            CC = CC - SNOWLQ * LF
            SNOWLQ = 0.0d0
        ELSE
!           refreeze part
            SNOWLQ = SNOWLQ - CC / LF
            CC = 0.0d0
        END IF
    END IF
!     groundmelt and evaporation loss as fraction of SNOW
    FRAC = (GRDMLT + PSNVP) * DTP / SNOW
!     FRAC can be negative if condensation exceeds groundmelt
    IF (FRAC .LT. 1.0d0) THEN
        SMLT = GRDMLT
        SNVP = PSNVP
!        reduce CC,SNOWLQ,and SNOW proportionally for groundmelt and evaporation
!        increase them proportionally if condensation exceeds groundmelt
        CC = CC * (1.0d0 - FRAC)
        SNOWLQ = SNOWLQ * (1.0d0 - FRAC)
        SNOW = SNOW * (1.0d0 - FRAC)
    ELSE
!        all snow disappears from groundmelt and/or evaporation
        SMLT = GRDMLT / FRAC
        SNVP = PSNVP / FRAC
        RSNO = 0.0d0
        CC = 0.0d0
        SNOWLQ = 0.0d0
        SNOW = 0.0d0
    END IF
!     snowpack cooling or warming
    IF (SNOW .GT. 0.0d0) THEN
!        equivalent ice melted by energy input including warm rain, mm
        EQEN = DTP * (SNOEN + RTHR * MAX(TA, 0.0d0) * CVLQ) / LF
        IF (EQEN .LE. 0.0d0) THEN
!           snowpack cooling
            NMLT = -EQEN
            IF (NMLT .LT. SNOWLQ) THEN
!              only part of SNOWLQ refreezes
                CC = 0.0d0
!               should be 0 already because SNOWLQ is positive
                SNOWLQ = SNOWLQ - NMLT
            ELSE
!              all SNOWLQ (if any) refreezes, remaining NMLT increases CC
               NMLT = NMLT - SNOWLQ
               SNOWLQ = 0.0d0
               CC = CC + NMLT * LF
!             do not allow TSNOW to cool below TA
               CC = MIN(CC, -TA * SNOW * CVICE)
            END IF
        ELSE
!           snowpack warming  (can't have both CC and SNOWLQ)
            IF (EQEN * LF .LT. CC .OR. TA .LT. 0.0d0) THEN
!              reduce but don't eliminate CC
                IF (TA .LT. 0.0d0) THEN
!                 do not allow TSNOW to warm above TA when TA < 0
                    CC = MAX(CC - EQEN * LF, -TA * SNOW * CVICE)
                ELSE
                    CC = CC - EQEN * LF
                END IF
                SNOWLQ = 0.0d0
            ELSE
!              CC eliminated
                EQEN = EQEN - CC / LF
                CC = 0.0d0
                IF (EQEN .LE. MAXLQF * SNOW - SNOWLQ) THEN
!                 remaining energy increases liquid water
                  SNOWLQ = SNOWLQ + EQEN
!                 SMLT and SNOW unchanged
                ELSE
!                 liquid water capacity reached, snow melt produced
                    EQEN = EQEN - (MAXLQF * SNOW - SNOWLQ)
                    IF (SNOW * (1.0d0 - MAXLQF) .GT. EQEN) THEN
!                    melt is ice plus the liquid included in it
                        SMLT = SMLT + (EQEN / DTP) / (1.0d0 - MAXLQF)
                        SNOW = SNOW - EQEN / (1.0d0 - MAXLQF)
                        SNOWLQ = MAXLQF * SNOW
                    ELSE
!                    all snow melts
                        SMLT = SMLT + SNOW / DTP
                        SNOW = 0.0d0
                        SNOWLQ = 0.0d0
                    END IF
                END IF
            END IF
        END IF

!        add rain to snowpack,
        IF (RTHR .EQ. 0.0d0 .OR. SNOW .EQ. 0.0d0) THEN
            RSNO = 0.0d0
        ELSE
!           rain on snow
            RIN = RTHR * DTP
            IF (CC .GT. 0.0d0) THEN
!              use CC to refreeze rain
                IF (CC .GT. RIN * LF) THEN
!                 refreezes all rain
                    CC = CC - RIN * LF
                    RSNO = RTHR
                    SNOW = SNOW + RIN
                ELSE
!                 CC refreezes part of rain
                    SNOW = SNOW + CC / LF
                    RSNO = (CC / LF) / DTP
                    CC = 0.0d0
!                 remaining rain
                    RIN = RIN - RSNO * DTP
!                 increase liquid water, SNOWLQ initially zero
                    IF (RIN .LT. MAXLQF * SNOW / (1.0d0 - MAXLQF)) THEN
!                    remaining RIN all to SNOWLQ
                        SNOWLQ = RIN
                        RSNO = RSNO + RIN / DTP
                        SNOW = SNOW + RIN
                    ELSE
                        SNOWLQ = MAXLQF * SNOW / (1.0d0 - MAXLQF)
                        RSNO = RSNO + SNOWLQ / DTP
                        SNOW = SNOW + SNOWLQ
                    END IF
                END IF
            ELSE
!              CC = 0.
                IF (SNOWLQ .GE. MAXLQF * SNOW) THEN
!                 snow already holding maximum liquid
                    RSNO = 0.0d0
                ELSE
                    ALQ = MAXLQF * SNOW - SNOWLQ
                    IF (RIN .LT. ALQ) THEN
!                    all RIN to SNOW
                        RSNO = RTHR
                        SNOWLQ = SNOWLQ + RIN
                        SNOW = SNOW + RIN
                    ELSE
!                    maximum liquid reached
                        RSNO = (ALQ / (1.0d0 - MAXLQF)) / DTP
                        SNOW = SNOW + RSNO * DTP
                        SNOWLQ = MAXLQF * SNOW
                    END IF
                END IF
            END IF
        END IF
    END IF

end subroutine SNOWPACK

subroutine SOILPAR (NLAYER, iModel, Par, THICK, STONEF, PSIM, PSICR, &
    PSIG, SWATMX, WETC, WETNES, SWATI, MPar, ML, pr, error)
!       calculated soil water parameters and initial variables
    IMPLICIT NONE
!       input
    integer :: NLAYER    ! number of soil layers
    integer :: MPAR, ML   ! maximum number of parameters and layers
    real(kind=8) :: THICK(*)  ! layer thicknesses, mm"
    real(kind=8) :: THSAT     ! theta at saturation, matrix porosity, iModel=0
    real(kind=8) :: THS       ! theta at saturation, matrix porosity, iModel=1
    real(kind=8) :: THR       ! residual theta, iModel=1
    real(kind=8) :: STONEF(*) ! stone volume fraction, unitless"
    real(kind=8) :: THETAF    ! volumetric water content at field capacity"
    real(kind=8) :: PSIF      ! matric potential at field capacity, kPa
    real(kind=8) :: BEXP      ! exponent for psi-theta relation
    real(kind=8) :: WETINF    ! wetness at dry end of near-saturation range
    real(kind=8) :: PSIM(*)   ! matric soil water potential for layer, kPa
    real(kind=8) :: KF        ! hydraulic conductivity at field capacity, mm/d
    real(kind=8) :: PSICR     ! minimum plant leaf water potential, MPa
    integer :: iModel    ! parameterization of hydraulic functions
    real(kind=8) :: Par(MPar,ML)  ! parameter array
    integer :: pr     ! print messages to console?
    integer :: timer     ! check timelimits set by R-user?
!       output
    real(kind=8) :: PSIG(*)   ! gravity potential, kPa
    real(kind=8) :: SWATMX(*) ! maximum water storage for layer, mm
    real(kind=8) :: WETF      ! wetness at field capacity, dimensionless
    real(kind=8) :: WETC(*)   ! wetness at PSICR, dimensionless
    real(kind=8) :: CHM       ! Clapp and Hornberger m, kPa
    real(kind=8) :: CHN       ! Clapp and Hornberger n
    real(kind=8) :: WETNES(*) ! wetness, fraction of saturation
    real(kind=8) :: SWATI(*)  ! water volume in layer, mm
    real(kind=8) :: KSAT      ! saturated hydraulic conductivity, mm/d
    integer :: error          ! error code (default 0)
!       function
!         real(kind=8) :: FWETK, FPSIM, FWETNES, FTHETA
!       local
    integer :: I         ! soil layer
!       intrinsic
    real(kind=8) :: PSIINF(NLAYER)
!          potential at dry end of near saturation range, kPa

    DO 100 I = 1, NLAYER
!           gravity potential is negative down from surface
        IF (I .EQ. 1) THEN
            PSIG(1) = -RHOWG * THICK(1) / 2.0d0
        ELSE
            PSIG(I) = PSIG(I - 1) - RHOWG * ((THICK(I - 1) + THICK(I)) / 2.0d0)
        END IF

        if (imodel .eq. 0) then
            THSAT = Par(1,i)
            THETAF=Par(2,i)
            SWATMX(I) = THICK(I) * THSAT * (1.0d0 - STONEF(I))
            WETF = THETAF / THSAT
            Par(5,i) = WETF
            WETINF=Par(10,i)
            BEXP=Par(9,i)
            PSIF=Par(4,i)
            PSIINF(I) = PSIF * (WETINF / WETF) ** (-BEXP)
            CHM = (-PSIINF(I) / (1.0d0 - WETINF) ** 2.0d0) - BEXP * &
                (-PSIINF(I)) / (WETINF * (1.0d0 - WETINF))
            Par(7,i) = CHM
            CHN = 2.0d0 * WETINF - 1.0d0 - (-PSIINF(I) * BEXP / (CHM * WETINF))
            Par(8,i) = CHN
            IF (PSIM(I) .GT. 0.0d0) THEN
                !print*, 'matrix psi must be negative or zero'
                error = 1
                if ( pr .EQ. 1 ) call intpr("STOP: positive matrix potential occured in layer no.:", -1, (/ I/),1)
                return
            ELSEIF (PSIM(I) .EQ. 0.0d0) THEN
                WETNES(I) = 1.0d0
            ELSE
                WETNES(I) = WETF * (PSIM(I) / PSIF) ** (-1.0d0 / BEXP)
                IF (WETNES(I) .GT. WETINF) THEN
                    WETNES(I) = (1.0d0 + CHN) / 2.0d0 + 0.50d0 * SQRT(CHN ** 2.0d0 &
                             - 2.0d0 * CHN + 1.0d0 + 4.0d0 * PSIM(I) / CHM)
                END IF
            END IF
            SWATI(I) = WETNES(I) * SWATMX(I)
            KF=Par(3,i)
            KSAT = KF * (1.0d0 / WETF) ** (2.0d0 * BEXP + 3)
            Par(6,i) = KSAT
            WETC(I) = WETF * (1000 * PSICR / PSIF) ** (-1.0d0 / BEXP)
        end if

        if (imodel .eq. 1) then
            Par(5,i)=FWETK(Par(3,i),Par(1,i),iModel, pr, timer)
            if ( Par(5,i) == -99999.d0 ) then
                call intpr('Warning: FWETK failed to determine wetness at KF',-1,(/ I/),0)
                error = 2 !STOP: FWETK failed to determine wetness at KF
                return
            end if
            Par(4,i)=FPSIM(Par(5,i),Par(1,i),iModel)
            Par(2,i)=FTheta(Par(5,i),Par(1,i),iModel)
            THS=Par(1,i)
            THR=Par(10,i)
            SWATMX(I) = THICK(I) * THS * (1.0d0 - STONEF(I))

            IF (PSIM(I) .GT. 0.) THEN
                !print*, 'matrix psi must be negative or zero'
                error = 1
                if ( pr .EQ. 1 ) call intpr("STOP: positive matrix potential occured in layer no.:", -1,(/ I/), 1 )
                return
            ELSE
                WETNES(I) =FWETNES(PSIM(i),Par(1,i),iModel)
            END IF
            SWATI(I) =FTHETA(WETNES(I),Par(1,i),iModel) * SWATMX(I)/THS
            WETC(I) = FWETNES(1000 * PSICR,Par(1,i),iModel)
        end if
    100 CONTINUE
end subroutine SOILPAR

subroutine SOILVAR (NLAYER, iModel, Par, PSIG, PSIM, WETNES, SWATI, &
    PSITI, THETA, KK, SWAT, MPar, ML)
!     soil water variables
    IMPLICIT NONE
!     input
    integer :: NLAYER    ! number of soil layers
    integer :: MPAR,ML   ! maximum number of parameters and layers
    real(kind=8) :: PSIG(*)   ! gravity potential, kPa
    real(kind=8) :: PSIM(*)   ! matric soil water potential for layer, kPa
    real(kind=8) :: WETNES(*) ! wetness, fraction of saturation.
!     real(kind=8) :: THSAT     ! theta at saturation, matrix porosity
    real(kind=8) :: KF        ! hydraulic conductivity at field capacity, mm/d
    real(kind=8) :: BEXP      ! exponent for psi-theta relation
    real(kind=8) :: WETF      ! wetness at field capacity, dimensionless
    real(kind=8) :: SWATI(*)  ! water volume in layer, mm
    integer :: iModel    ! parameterization of hydraulic functions
    real(kind=8) :: Par(MPar,ML)  ! parameter array
!     output
    real(kind=8) :: PSITI(*)  ! total potential, kPa
    real(kind=8) :: THETA(*)  ! water content, mm water / mm soil matrix
    real(kind=8) :: SWAT      ! total soil water in all layers, mm
    real(kind=8) :: KK(*)     ! hydraulic conductivity, mm/d
!     function
!         real(kind=8) :: FK, FTheta
!     local
    integer :: I         !soil layer

    SWAT = 0.0d0
    DO 10 I = 1, NLAYER
        KF=Par(3,i)
        WETF=Par(5,i)
        BEXP=Par(9,i)
        PSITI(I) = PSIM(I) + PSIG(I)
        THETA(I) = FTheta (Wetnes(i),Par(1,i),iModel)
        if (iModel.eq.0) then
            IF (WETNES(I) .GT. .00010d0) THEN
                KK(I)=KF * (WETNES(I) / WETF) ** (2.0d0 * BEXP+ 3.0d0)
            ELSE
!           extremely dry
                KK(I) = 1E-10
            END IF
        end if
        if (iModel.eq.1) then
            KK(I)=FK(Wetnes(i),Par(1,i),iModel)
        end if
        SWAT = SWAT + SWATI(I)
    10 CONTINUE
end subroutine SOILVAR

subroutine SRFLFR (QLAYER, SWATI, SWATQX, QFPAR, SWATQF, QFFC, SAFRAC)
    IMPLICIT NONE
!     input
    integer :: QLAYER    ! number of soil layers for SRFL
    real(kind=8) :: SWATI(*)  ! water volume by layer, mm
    real(kind=8) :: SWATQX    ! maximum water storage for layers 1 through QLAYER
    real(kind=8) :: QFPAR     ! quickflow parameter, 0 for bucket
    real(kind=8) :: SWATQF    ! water storage at field capacity for layers 1
                           ! through QLAYER, mm
    real(kind=8) :: QFFC      ! SRFL fraction at field capacity
!     output
    real(kind=8) :: SAFRAC    ! source area fraction
!     local
    real(kind=8) :: SUM       ! soil water in layers 1 through QLAYER
    integer :: I

    SUM = 0.0d0
    DO 10 I = 1, QLAYER
        SUM = SUM + SWATI(I)
!         write(10,*)SWATI(I)
10  CONTINUE
    SAFRAC = QFFC ** (1.0d0 - (1.0d0 / QFPAR) * (SUM - SWATQF) / (SWATQX - SWATQF))
    IF (SAFRAC .GT. 1.0d0) SAFRAC = 1.0d0

end subroutine SRFLFR

subroutine SRFPAR (QLAYER, Par, THICK, STONEF, SWATMX, SWATQX, SWATQF, MPar, ML)
!     source area parameters
    IMPLICIT NONE
!     input
    integer :: QLAYER    ! number of soil layers for SRFL
    integer :: MPAR,ML   ! maximum number of parameters and layers
!    real(kind=8) :: THETAF(*) ! volumetric water content of layer at field capacity
    real(kind=8) :: THICK(*)  ! layer thickness, mm
    real(kind=8) :: STONEF(*) ! stone volume fraction of layer
    real(kind=8) :: SWATMX(*) ! maximum water storage for layer, mm
    real(kind=8) :: Par(MPar,ML)  ! parameter array
!     output
    real(kind=8) :: SWATQX    ! maximum water storage for layers 1 through
                           ! QLAYER, mm
    real(kind=8) :: SWATQF    ! water storage at field capacity for layers 1
                           ! through QLAYER, mm
!     local
    integer :: I
!
    SWATQX = 0.0d0
    SWATQF = 0.0d0
    DO 10 I = 1, QLAYER
        SWATQX = SWATQX + SWATMX(I)
        SWATQF = SWATQF + Par(2,i) * THICK(I) * (1.0d0 - STONEF(I))
10  CONTINUE

end subroutine SRFPAR

subroutine SRSC (RAD, TA, VPD, LAI, SAI, GLMIN, GLMAX, R5, CVPD, RM, CR, TL, T1, T2, TH, RSC)
!     canopy surface resistance, RSC, after Shuttleworth and Gurney (1990) and
!        Stewart (1988)
    IMPLICIT NONE
!     input
    real(kind=8) :: RAD     ! solar radiation on canopy, W/m2
    real(kind=8) :: TA      ! mean  temperature for the day at reference height,
                         ! degC
    real(kind=8) :: VPD     ! vapor pressure deficit, kPa
    real(kind=8) :: LAI     ! projected leaf area index
    real(kind=8) :: SAI     ! projected stem area index
    real(kind=8) :: GLMIN   ! minimum leaf conductance, closed stomates, all
                         ! sides, s/m
    real(kind=8) :: GLMAX   ! maximum leaf conductance, open stomates, all
                         ! sides, s/m
    real(kind=8) :: R5      ! solar radiation at which conductance is halved, W/m2
    real(kind=8) :: CVPD    ! vpd at which leaf conductance is halved, kPa
    real(kind=8) :: RM      ! maximum solar radiation, at which FR = 1, W/m2
    real(kind=8) :: CR      ! light extinction coefficient for LAI, projected area
    real(kind=8) :: TL      ! temperature below which stomates are closed, degC
    real(kind=8) :: T1      ! lowest temp. at which stomates not temp. limited,
                         ! degC
    real(kind=8) :: T2      ! highest temp. at which stomates not temp. limited,
                         ! degC
    real(kind=8) :: TH      ! temperature above which stomates are closed, degC
!     output
    real(kind=8) :: RSC     ! canopy surface resistance, s/m
!     local
    real(kind=8) :: FS      ! correction for stem area
    real(kind=8) :: R0      ! a light response parameter
    real(kind=8) :: FRINT   ! integral of fR dL over Lp
    real(kind=8) :: FD      ! dependence of leaf conductance on vpd, 0 to 1
    real(kind=8) :: FT      ! dependence of leaf conductance on temperature,0 to 1
    real(kind=8) :: GSC     ! canopy conductance, m/s
!     intrinsic
!        LOG, EXP

!     solar radiation limitation integrated down through canopy
!     Stewart (1988) and Saugier and Katerji (1991)
    FS = (2.0d0 * LAI + SAI) / (2.0d0 * LAI)
    IF (RAD .LE. 1E-10) THEN
        FRINT = 0.0d0
    ELSE
        R0 = RM * R5 / (RM - 2.0d0 * R5)
        FRINT = ((RM + R0) / (RM * CR * FS)) * LOG((R0 + CR * RAD) / &
            (R0 + CR * RAD * EXP(-CR * FS * LAI)))
    END IF
!     vapor deficit limitation
!     Lohammar et al. (1980) and Stannard (1993)
    FD = 1.0d0 / (1.0d0 + VPD / CVPD)
!     temperature limitation
    IF (TA .LE. TL) THEN
        FT = 0.0d0
    ELSEIF (TA .GT. TL .AND. TA .LT. T1) THEN
        FT = 1.0d0 - ((T1 - TA) / (T1 - TL)) ** 2.0d0
    ELSEIF (TA .GE. T1 .AND. TA .LE. T2) THEN
        FT = 1.0d0
    ELSEIF (TA .GT. T2 .AND. TA .LT. TH) THEN
        FT = 1.0d0 - ((TA - T2) / (TH - T2)) ** 2.0d0
    ELSE
        FT = 0.0d0
    END IF
    GSC = FD * FT * FRINT * (GLMAX - GLMIN) + LAI * GLMIN
    RSC = 1.0d0 / GSC

end subroutine SRSC

subroutine SUMI (N, A1, A2, A3, A4, A5, A6, B1, B2, B3, B4,B5, B6)
!     array summer; sums Aj(i) for i = 1,n with result in Bj
    IMPLICIT NONE
    integer ::  N, I
    real(kind=8) :: A1(*), A2(*), A3(*), A4(*), A5(*), A6(*)
    real(kind=8) :: B1, B2, B3, B4, B5, B6

    CALL ZERO(B1, B2, B3, B4, B5, B6)
    DO 20 I = 1, N
        B1 = B1 + A1(I)
        B2 = B2 + A2(I)
        B3 = B3 + A3(I)
        B4 = B4 + A4(I)
        B5 = B5 + A5(I)
        B6 = B6 + A6(I)
20  CONTINUE
end subroutine SUMI

subroutine SUNDS (LAT, SLOPE, DOY, L1, L2, DAYLEN, I0HDAY, SLFDAY)
!     daylength, potential daily solar radiation on horizontal,
!        and ratio of potential on slope (map area) to horizontal
!     from Swift (1976)
    IMPLICIT NONE
!     input
    real(kind=8) :: LAT     ! latitude, radians
    real(kind=8) :: SLOPE   ! slope, radians
    integer :: DOY     ! day of the year
    real(kind=8) :: L1      ! latitude of equivalent slope, radians, from EQUIVSLP
    real(kind=8) :: L2      ! time shift of equivalent slope,radians,from EQUIVSLP
!     outputs
    real(kind=8) :: DAYLEN  ! daylength (sun above horizontal) in fraction of
                         ! day, d
    real(kind=8) :: I0HDAY  ! potential insolation on horizontal surface, MJ/m2
    real(kind=8) :: SLFDAY  ! ratio of potential insolation on slope to
                         ! horizontal, map area
!     local
    real(kind=8) :: I0SDAY  ! potential insolation on slope, map area basis, MJ/m2
    real(kind=8) :: SCD     ! solar constant for day, W/m2
    real(kind=8) :: DEC     ! declination of the sun, radians
    integer :: TWORIS  ! 1 if two sunrises on slope
    real(kind=8) :: T       ! temporary variable
    real(kind=8) :: T0      ! hour angle of sunrise on horizontal, radians
    real(kind=8) :: T1      ! hour angle of sunset on horizontal
    real(kind=8) :: T2      ! hour angle of sunrise on slope
    real(kind=8) :: T3      ! hour angle of sunset on slope
    real(kind=8) :: T6      ! hour angle of sunrise on equivalent slope
    real(kind=8) :: T7      ! hour angle of sunset on equivalent slope
    real(kind=8) :: T8      ! hour angle of second sunrise on slope
    real(kind=8) :: T9      ! hour angle of second sunset on slope
!     external functions needed
!     real(kind=8) :: HAFDAY
!     real(kind=8) :: FUNC3
!     intrinsic
!        COS, SIN, MIN, MAX, ASIN

    SCD = SC / (1.0d0 - .01670d0 * COS(.01720d0 * (DOY - 3.0d0))) ** 2.0d0
    DEC = ASIN(.397850d0 * SIN(4.8689610d0 + .0172030d0 * DOY + .0334460d0 * &
        SIN(6.2241110d0 + .0172020d0 * DOY)))
    T = HAFDAY(LAT, DEC)
    DAYLEN = MAX(.00010d0, MIN(.99990d0, T / PI))
!        to avoid zero divides for 0 and 1
    T1 = T
    T0 = -T
    T = HAFDAY(L1, DEC)
    T7 = T - L2
    T6 = -T - L2
    T3 = MIN(T1, T7)
    T2 = MAX(T0, T6)
    IF (T3 .LT. T2) THEN
        T2 = 0.0d0
        T3 = 0.0d0
    END IF
        T6 = T6 + 2.0d0 * PI
    IF (T6 .LT. T1) THEN
        T8 = T6
        T9 = T1
        TWORIS = 1
    ELSE
        T7 = T7 - 2.0d0 * PI
        IF (T7 .GT. T0) THEN
            T8 = T0
            T9 = T7
            TWORIS = 1
        END IF
        TWORIS = 0
    END IF
    IF (TWORIS .EQ. 1) THEN
!        two sunrises
        I0SDAY = WTOMJ * SCD * (FUNC3(DEC, L2, L1, T3, T2) + FUNC3(DEC, L2, L1, T9, T8)) / COS(SLOPE)
!        "daylength" on the slope = ((T3 - T2) + (T9 - T8)) / (2. * PI)
    ELSE
!        one sunrise
        I0SDAY = WTOMJ * SCD * FUNC3(DEC, L2, L1, T3, T2) / COS(SLOPE)
!        COS(SLOPE) adjusts from slope area to map area
!        "daylength" on the slope = (T3 - T2) / (2. * PI)
    END IF
    I0HDAY = WTOMJ * SCD * FUNC3(DEC, 0.0d0, LAT, T1, T0)
    IF (I0HDAY .LE. 0.0d0) THEN
        SLFDAY = 0.0d0
    ELSE
        SLFDAY = I0SDAY / I0HDAY
    END IF

end subroutine SUNDS

subroutine SWPE (AA, ASUBS, VPD, RAA, RAC, RAS, RSC, RSS, DELTA, PRATE, ERATE)
!     Shuttleworth and Wallace (1985) transpiration and ground evaporation
    IMPLICIT NONE
!     input
    real(kind=8) :: AA      ! net radiation at canopy top minus ground flux, W/m2
    real(kind=8) :: ASUBS   ! net radiation minus ground flux at ground, W/m2
    real(kind=8) :: VPD     ! vapor pressure deficit, kPa
    real(kind=8) :: RAA     ! boundary layer resistance, s/m
    real(kind=8) :: RAC     ! leaf-air resistance, s/m
    real(kind=8) :: RAS     ! ground-air resistance, s/m
    real(kind=8) :: RSC     ! canopy surface resistance, s/m
    real(kind=8) :: RSS     ! ground evaporation resistance, s/m
    real(kind=8) :: DELTA   ! dEsat/dTair, kPa/K
!     output
    real(kind=8) :: PRATE   ! potential transpiration rate, mm/d
    real(kind=8) :: ERATE   ! ground evaporation rate, mm/d
!     local
    real(kind=8) :: RS, RC, RA, CS, CC, PMS, PMC, D0
                         ! as in Shuttleworth and Wallace (1985)
    real(kind=8) :: LE      ! total latent heat flux density, W/m2
!     external function needed
!     real(kind=8) :: PM

    RS = (DELTA + GAMMA) * RAS + GAMMA * RSS
    RC = (DELTA + GAMMA) * RAC + GAMMA * RSC
    RA = (DELTA + GAMMA) * RAA
    CS = 1.0d0 / (1.0d0 + RS * RA / (RC * (RS + RA)))
    CC = 1.0d0 / (1.0d0 + RC * RA / (RS * (RC + RA)))
    PMS = PM(AA, VPD - DELTA * RAS * (AA - ASUBS) / CPRHO, DELTA, RAA + RAS, RSS)
    PMC = PM(AA, VPD - DELTA * RAC * ASUBS / CPRHO, DELTA, RAA + RAC, RSC)
    LE = (CC * PMC + CS * PMS)
    D0 = VPD + RAA * (DELTA * AA - (DELTA + GAMMA) * LE) / CPRHO
    PRATE = ETOM * WTOMJ * PM(AA - ASUBS, D0, DELTA, RAC, RSC)
    ERATE = ETOM * WTOMJ * PM(ASUBS, D0, DELTA, RAS, RSS)

end subroutine SWPE

subroutine SWGE (AA, ASUBS, VPD, RAA, RAS, RSS, DELTA, ARATE, ERATE)
!     Shuttleworth and Wallace (1985) ground evaporation when transpiration
!        known
    IMPLICIT NONE
!     input
    real(kind=8) :: AA      ! net radiation at canopy top minus ground flux, W/m2
    real(kind=8) :: ASUBS   ! net radiation minus ground flux at ground, W/m2
    real(kind=8) :: VPD     ! vapor pressure deficit, kPa
    real(kind=8) :: RAA     ! boundary layer resistance, s/m
    real(kind=8) :: RAS     ! ground-air resitance, s/m
    real(kind=8) :: RSS     ! ground evaporation resistance, s/m
    real(kind=8) :: DELTA   ! dEsat/dTair, kPa/K
    real(kind=8) :: ARATE   ! actual transpiration rate, mm/d
!     output
    real(kind=8) :: ERATE   ! ground evaporation rate, mm/d
!     local
    real(kind=8) :: RS, RA  ! as in Shuttleworth and Wallace (1985)
    real(kind=8) :: LE      ! total latent heat flux density, W/m2
    real(kind=8) :: LEC     ! actual transpiration latent heat flux density, W/m2

    LEC = ARATE / (ETOM * WTOMJ)
    RS = (DELTA + GAMMA) * RAS + GAMMA * RSS
    RA = (DELTA + GAMMA) * RAA
    LE = (RS / (RS + RA)) * LEC + (CPRHO * VPD + DELTA * RAS * ASUBS + DELTA * RAA * AA) / (RS + RA)
    ERATE = ETOM * WTOMJ * (LE - LEC)

end subroutine SWGE

subroutine SWGRA (UA, ZA, HEIGHT, Z0, DISP, Z0C, DISPC, Z0G, LWIDTH, &
    RHOTP, NN, LAI, SAI, RAA, RAC, RAS)
!     atmospheric resistances RAA, RAC, and RAS
!     from Shuttleworth and Gurney (1990)
    IMPLICIT NONE
!     input
    real(kind=8) :: UA      ! wind speed at reference height, m/s
    real(kind=8) :: ZA      ! reference height, m
    real(kind=8) :: HEIGHT  ! canopy height, m
    real(kind=8) :: Z0      ! roughness parameter, m
    real(kind=8) :: DISP    ! zero-plane displacement, m
    real(kind=8) :: Z0C     ! roughness length for closed canopy, m
    real(kind=8) :: DISPC   ! zero-plane displacement for closed canopy, m
    real(kind=8) :: Z0G     ! roughness parameter of soil surface, m
    real(kind=8) :: LWIDTH  ! characteristic leaf width, m
    real(kind=8) :: RHOTP   ! ratio of total leaf area to projected leaf area
    real(kind=8) :: NN      ! wind/diffusivity extinction coefficient
    real(kind=8) :: LAI     ! projected leaf area index
    real(kind=8) :: SAI     ! projected stem area index
!     output
    real(kind=8) :: RAA     ! boundary layer resistance, s/m
    real(kind=8) :: RAC     ! leaf-air resistance, s/m
    real(kind=8) :: RAS     ! ground-air resitance, s/m
!     local
    real(kind=8) :: USTAR, KH, UH, RB
!     intrinsic
!        LOG, EXP

    USTAR = K * UA / (LOG((ZA - DISP) / Z0))
    KH = K * USTAR * (HEIGHT - DISP)
    RAS = (HEIGHT * EXP(NN) / (NN * KH)) * (EXP(-NN * Z0G / HEIGHT) - EXP(-NN * (Z0C + DISPC) / HEIGHT))
    IF (RAS .LT. 1.0d0) RAS = 1.0d0
    RAA = LOG((ZA - DISP) / (HEIGHT - DISP)) / (K * USTAR) + (HEIGHT / (NN * KH)) * &
        (-1.0d0 + EXP(NN * (HEIGHT - DISPC - Z0C) / HEIGHT))
    UH = (USTAR / K) * LOG((HEIGHT - DISP) / Z0)
!     the Shuttleworth-Gurney RB equation is strictly for one side of
!        flat leaves
!     when RHOTP > 2, LWIDTH is small (needles) so RAC is small
!     their equation should have NN in numerator, see Choudhury and
!        Monteith(1988)
    RB = (100.0d0 * NN) * (LWIDTH / UH) ** .50d0 / (1.0d0 - EXP(-NN / 2.0d0))
    RAC = RB / (RHOTP * LAI + PI * SAI)
!     note LAI is prevented from being less than 1E-5

end subroutine SWGRA

subroutine TBYLAYER (J, PTR, DISPC, ALPHA, KK, RROOTI, RXYLEM, &
    PSITI, NLAYER, PSICR, NOOUTF, ATR, ATRANI)
!     actual transpiration rate by layers
!     watch MPa - kPa conversions carefully
    IMPLICIT NONE
!     input
    integer :: J        ! 1 for daytime, 2 for nighttime
    real(kind=8) :: PTR       ! average potential transpiration rate over time
                         ! period, mm/d
    real(kind=8) :: DISPC     ! zero-plane displacement for closed canopy, m
    real(kind=8) :: ALPHA(*)  ! modified Cowan alpha, MPa
    real(kind=8) :: KK(*)     ! hydraulic conductivity, mm/d
    real(kind=8) :: RROOTI(*) ! root resistance for layer, MPa d/mm
    real(kind=8) :: RXYLEM    ! xylem resistance, MPa d/mm
    real(kind=8) :: PSITI(*)  ! total soil water potential, kPa
    integer :: NLAYER   ! number of soil layers (max 20)
    real(kind=8) :: PSICR     ! critical potential for plant, MPa
    integer :: NOOUTF   ! 1 if no outflow allowed from roots, otherwise 0
!     output
    real(kind=8) :: ATR       ! actual transpiration rate over time period, mm/d
    real(kind=8) :: ATRANI(*) ! actual transpiration rate from layer over time
                         ! period, mm/d
!     local
    integer :: I      ! layer counter
    integer :: II     ! loop index
    real(kind=8) :: RI(0:NLAYER)  ! root plus rhizosphere resistance, MPa d/mm
    real(kind=8) :: RT      ! combined root resistance from unflagged layers,
                         ! MPa d/mm
    real(kind=8) :: SUM     ! sum of layer conductances, (mm/d)/MPa
    real(kind=8) :: TRMIN   ! largest negative transpiration loss, mm/d
    real(kind=8) :: PSIT    ! weighted average total soil water potential for
                         ! unflagged layers, kPa
    real(kind=8) :: R       ! (2/pi)(SUPPLY/PTR)
    real(kind=8) :: SUPPLY  ! soil water supply rate, mm/d
    integer :: IDEL   ! subscript of flagged layer
    integer :: FLAG(0:NLAYER) ! 1 if layer has no transpiration uptake,otherwise 0
    integer :: NEGFLAG ! 1 if second iteration is needed
!     intrinsic function
!         SIN, ACOS, MIN

!     flag layers with no roots, indicated by RROOTI = 1E20
!     if outflow from roots is prevented, flag layers with PSITI <= PSICR
    DO 10 I = 1, NLAYER
        IF (RROOTI(I) .GT. 1E+15) FLAG(I) = 1
        IF (NOOUTF .EQ. 1 .AND. PSITI(I) / 1000.0d0 .LE. PSICR) THEN
            FLAG(I) = 1
        ELSE
            FLAG(I) = 0
        END IF
10  CONTINUE

!     top of loop for recalculation of transpiration if more layers get flagged
    DO 300 II = 1, 100
        NEGFLAG = 0
        SUM = 0.0d0
        DO 20 I = 1, NLAYER
            IF (FLAG(I) .EQ. 0) THEN
                RI(I) = RROOTI(I) + ALPHA(I) / KK(I)
                SUM = SUM + 1.0d0 / RI(I)
            ELSE
!              flagged
                ATRANI(I) = 0.0d0
            END IF
20      CONTINUE
        IF (SUM .LT. 1E-20) THEN
!           all layers flagged, no transpiration
            ATR = 0.0d0
            PSIT = -1E+10
            RETURN
         ELSE
            RT = 1.0d0 / SUM
         END IF
!        weighted mean soil water potential
         PSIT = 0.0d0
         DO 30 I = 1, NLAYER
            IF (FLAG(I) .EQ. 0) THEN
                PSIT = PSIT + RT * PSITI(I) / RI(I)
            END IF
30      CONTINUE
!        soil water supply rate, assumed constant over day
        SUPPLY = (PSIT / 1000.0d0 - PSICR - RHOWG * DISPC) / (RT + RXYLEM)
!        transpiration rate limited by either PTR or SUPPLY
        IF (J .EQ. 1) THEN
!           daytime, PTR is average of a half sine over daytime
            R = (2.0d0 / PI) * (SUPPLY / PTR)
            IF (R .LE. 0.0d0) THEN
                ATR = 0.0d0
            ELSEIF (R .LT. 1.0d0) THEN
                ATR = PTR * (1.0d0 + R * ACOS(R) - SIN(ACOS(R)))
            ELSE
                ATR = PTR
            END IF
        ELSE
!           nighttime, PTR assumed constant over nighttime
            IF (SUPPLY .LE. 0.0d0 .OR. PTR .LE. 0.0d0) THEN
                ATR = 0.0d0
            ELSE
                ATR = MIN(SUPPLY, PTR)
            END IF
        END IF
!        distribute total transpiration rate to layers
        DO 40 I = 1, NLAYER
            IF (FLAG(I) .EQ. 1) THEN
                ATRANI(I) = 0.0d0
            ELSE
               ATRANI(I) = ((PSITI(I) - PSIT) / 1000.0d0 + RT * ATR) /RI(I)
!              check for any negative transpiration losses
               IF (ATRANI(I) .LT. -.0000010d0) NEGFLAG = 1
            END IF
40      CONTINUE
        IF (NOOUTF .EQ. 1 .AND. NEGFLAG .EQ. 1) THEN
!           find layer with most negative transpiration and omit it
            IDEL = 0
            TRMIN = 0.0d0
            DO 50 I = 1, NLAYER
                IF (ATRANI(I) .LT. TRMIN) THEN
                    TRMIN = ATRANI(I)
                    IDEL = I
                END IF
50          CONTINUE
                FLAG(IDEL) = 1
!           repeat main loop with flagged layers excluded
        ELSE
!          done
            RETURN
        END IF
300 CONTINUE

end subroutine TBYLAYER

subroutine Temper(N,NMat,THICK,ZL,MUE,STEP,MatNum,TempO,TempN, &
                      A,B,C,D,TPar, zeroCurRange,zeroCurTemp, &
                      vNew,inFil,ThNew,CapNew,CapOld,Cond,&
                      tTop,tBot)
! subroutine Temper(N,NMat,THICK,ZL,MUE,STEP,MatNum,TempO,TempN, &
!                       A,B,C,D,TPar,LF,zeroCurRange,zeroCurTemp, &
!                       vNew,inFil,ThNew,CapNew,CapOld,Cond,Sink,&
!                       tTop,tBot)


    integer :: i, M, N, NMat, MatNum(N)
    double precision      A(N), B(N), C(N), D(N)
    real(kind=8) :: THICK(N), ZL(N), MUE(N), &
               TempO(N),TempN(N),TPar(10,NMat),vNew(N), &
               ThNew(N),CapNew(N),CapOld(N),Cond(N) !Sink(N)

    real(kind=8) :: tTop, tBot, STEP, CKM, QKM, HLU, HLO, FHLP, inFil
    real(kind=8) :: zeroCurTemp, zeroCurRange,  zeroCurU,  zeroCurL ! LF
    integer :: IFEHL
!
!     ------------------- Berechnung der Transportparameter --------------------
!
    zeroCurU= zeroCurTemp+0.50d0*zeroCurRange
    zeroCurL= zeroCurTemp-0.50d0*zeroCurRange
    do 12 i=1,N
        M=MatNum(i)
        Cond(i)=TPar(4,M)+TPar(5,M)*ThNew(i)+TPar(6,M)*sqrt(ThNew(i))
     !          TPar(9,M)*TPar(3,M)*abs(vNew(i))
        CapNew(i)=TPar(7,M)*TPar(1,M)+TPar(8,M)*TPar(2,M)
     !         +TPar(9,M)*ThNew(i)
        if((TempO(i).GT.zeroCurL) .AND. (TempO(i).LT.zeroCurU)) then
            CapNew(i) =  CapNew(i)
     !                ThNew(i) * (LF/zeroCurRange - TPar(9,M))
!         write(10,*)'zeroCurtain at ',i
        end if
12  continue

!
!     -------------------- Aufbau der Koeffizientenmatrix ----------------------
!
    DO 150 I=1,N
        CKM   = 0.50d0*(CapNew(I)+CapOld(I))
        FHLP  = STEP/(THICK(I)*CKM)
        A(I)  = TempO(I)
        IF(I.EQ.1) THEN
            QKM   = inFil*TPar(9,MatNum(i))
            HLU   = MUE(I)*Cond(I+1) + (1.0d0-MUE(I))*Cond(I)
            HLO   = Cond(I)
            B(I)  = FHLP*(-0.50d0*QKM-(HLO/THICK(I)))
            D(I)  = 1.0d0 + FHLP*(HLU/ZL(I)+QKM*(1.0d0-MUE(I)-0.50d0) + HLO/THICK(I))
        END IF
        IF((I.GT.1).AND.(I.LT.N)) THEN
            QKM   = vNew(i)*TPar(9,MatNum(i))
            HLU   = MUE(I)*Cond(I+1) + (1.0d0-MUE(I))*Cond(I)
            HLO   = MUE(I-1)*Cond(I) + (1.0d0-MUE(I-1))*Cond(I-1)
            B(I)  = FHLP*(-QKM*(1.0d0-MUE(I-1))-(HLO/ZL(I-1)))
            D(I)  = 1.0d0 + FHLP*(HLU/ZL(I)+QKM*(1.0d0-MUE(I)-MUE(I-1)) + HLO/ZL(I-1))
        END IF
        IF(I.EQ.N) THEN
            QKM   = vNew(i)*TPar(9,MatNum(i))
            HLU   = Cond(I)
            HLO   = MUE(I-1)*Cond(I) + (1.0d0-MUE(I-1))*Cond(I-1)
            B(I)  = FHLP*(-QKM*(1.0d0-MUE(I-1))-(HLO/ZL(I-1)))
            D(I)  = 1.0d0 + FHLP*(HLU/ZL(I)+QKM*(1.-MUE(I)-MUE(I-1)) + HLO/ZL(I-1))
        END IF
        C(I)  = FHLP*(-HLU/ZL(I)+QKM*MUE(I))
150  CONTINUE
!
!     -------------------------- Randbedingungen -------------------------------
!
!     - oberer Rand -
    A(1)     = A(1) - 2.0d0*B(1)*tTop
    D(1)     = D(1) - B(1)
    B(1)     = 9999.990d0
!     - unterer Rand -
    A(N)     = A(N) - C(N)*tBot
    C(N)     = 9999.990d0
!
!     ------------- Loesung des linearen Gleichungssystems ---------------------
!
    CALL TRIDIG(N,A,B,C,D,TempN,IFEHL)
!
!
    if(IFEHL .eq. 1)  call intpr("tridig failed", -1,(/ 0/),0) !print*, 'tridig failed'

!   DO 160 I=1,N
!       write(10,*) i, TempN(i)
!160   CONTINUe
    RETURN

end subroutine Temper

subroutine TRIDIG(N,A,B,C,D,X,IFEHL)
!*******************************************************************************
!   Loesung der tridiagonalen Gleichungssysteme mit dem Thomas-Algorithmus     *
!*******************************************************************************

    integer :: I, J, K, N, IFEHL
    double precision A(N),B(N),C(N),CH(N),D(N),DH(N)
    real(kind=8) :: X(N),ALPHA

    IFEHL=0

    IF(N.LE.1) THEN
        IF(D(1).EQ.0.0) THEN
            IFEHL = 1
            RETURN
        END IF
        X(1)=A(1)/D(1)
        IF(N.LE.0) IFEHL = -1
        RETURN
    ELSE
        ALPHA=D(1)
    END IF
    IF (ALPHA.EQ.0.00d0) THEN
        I=1
        IFEHL=I
        RETURN
    END IF

    CH(1)=C(1)/ALPHA
    CH(N)=0.00d0
    DH(1)=A(1)/ALPHA
    DO 50 I=2,N
        ALPHA=D(I)-B(I)*CH(I-1)
        IF(ALPHA.EQ.0.00d0) THEN
            IFEHL = I
            RETURN
        END IF
        CH(I)=C(I)/ALPHA
        DH(I)=(A(I)-B(I)*DH(I-1))/ALPHA
50  CONTINUE

    X(N)=DH(N)
    DO 100 K=2,N
        J=N+1-K
        X(J)=DH(J)-CH(J)*X(J+1)
100 CONTINUE

    RETURN

end subroutine TRIDIG

subroutine VERT (KK, KK1, KSAT, KSAT1, THICK, THICK1, PSIT, PSIT1, STONE, STONE1, RHOWG, VRFLI)
!     vertical flow rate
!        flow rate = gradient * cond    / rhog
!        mm/day   = kPa/mm   * mm/day  / kPa/mm
    IMPLICIT NONE
!     input
    real(kind=8) :: KK      ! hydraulic conductivity for upper layer, mm/d
    real(kind=8) :: KK1     ! hydraulic conductivity for lower layer, mm/d
    real(kind=8) :: KSAT    ! saturated hydraulic conductivity of upper layer,mm/d
    real(kind=8) :: KSAT1   ! saturated hydraulic conductivity of lower layer,mm/d
    real(kind=8) :: THICK   ! thickness of upper layer, mm
    real(kind=8) :: THICK1  ! thickness of lower layer, mm
    real(kind=8) :: PSIT    ! total potential of upper layer, kPa
    real(kind=8) :: PSIT1   ! total potential of lower layer, kPa
    real(kind=8) :: STONE   ! stone volume fraction of upper layer, unitless
    real(kind=8) :: STONE1  ! stone volume fraction of lower layer, unitless
    real(kind=8) :: RHOWG   ! density of water times gravity acceleration, kPa/mm
!     output
    real(kind=8) :: VRFLI   ! vertical drainage rate from layer i, mm/d
!     local
    real(kind=8) :: GRAD    ! potential gradient, positive downward, kPa/mm
    real(kind=8) :: KKMEAN  ! geometric mean conductivity
!     intrinsic
!        LOG, EXP

    KKMEAN = EXP((THICK1 * LOG(KK) + THICK * LOG(KK1)) / (THICK + THICK1))
!     limit KKMEAN to lesser saturated conductivity
    IF (KKMEAN .GT. KSAT) KKMEAN = KSAT
    IF (KKMEAN .GT. KSAT1) KKMEAN = KSAT1
    GRAD = (PSIT - PSIT1) / ((THICK + THICK1) / 2.0d0)
    VRFLI = (GRAD * KKMEAN / RHOWG) * (1.0d0 - (STONE + STONE1) / 2.0d0)

end subroutine VERT

subroutine WEATHER (TMAX, TMIN, DAYLEN, I0HDAY, EA, UW, ZA, DISP, Z0, WNDRAT, FETCH, &
    Z0W, ZW, SOLRAD, TA, TADTM, TANTM, UA, UADTM, UANTM)
    IMPLICIT NONE
!     input
    real(kind=8) :: TMAX    ! maximum temperature for the day, C
    real(kind=8) :: TMIN    ! minimum temperature for the day, C
    real(kind=8) :: DAYLEN  ! daylength in fraction of day
    real(kind=8) :: I0HDAY  ! potential insolation on horizontal, MJ m-2 d-1
    real(kind=8) :: EA      ! vapor pressure for the day, kPa
    real(kind=8) :: UW      ! average wind speed for day at weather station, m/s
    real(kind=8) :: ZA      ! reference height for TA, EA, UA, above ground, m
    real(kind=8) :: DISP    ! zero-plane displacement, m
    real(kind=8) :: Z0      ! roughness parameter, m
    real(kind=8) :: WNDRAT  ! ratio of nighttime to daytime wind speed
    real(kind=8) :: FETCH   ! weather station fetch, m
    real(kind=8) :: Z0W     ! weather station roughness parameter, m
    real(kind=8) :: ZW      ! weather station measurement height for wind, m
!     input and output
    real(kind=8) :: SOLRAD  ! solar radiation for the day, horizontal surface,
                         ! MJ/m2
!     output
    real(kind=8) :: TA      ! mean temperature for the day, C
    real(kind=8) :: TADTM   ! average daytime temperature, C
    real(kind=8) :: TANTM   ! average nighttime temperature, C
    real(kind=8) :: UADTM   ! average wind speed for daytime at ZA, m/s
    real(kind=8) :: UANTM   ! average wind speed for nighttime at ZA, m/s
!     local
    real(kind=8) :: UA      ! average wind speed for the day at reference height,
                         ! m/s
    real(kind=8) :: PI
    real(kind=8) :: DUMMY
!     intrinsic
!        SIN
!     external function needed
!     real(kind=8) :: WNDADJ

    PI = 3.14160d0
!     estimate SOLRAD if missing
    IF (SOLRAD .LT. .0010d0) SOLRAD = .550d0 * I0HDAY
!     average temperature for day
    TA = (TMAX + TMIN) / 2.0d0
!     daytime and nighttime average air temperature
    TADTM = TA + ((TMAX - TMIN) / (2.0d0 * PI * DAYLEN)) * SIN(PI * DAYLEN)
    TANTM = TA - ((TMAX - TMIN) / (2.0d0 * PI * (1.0d0 - DAYLEN))) * SIN(PI * DAYLEN)
!     if no vapor pressure data, use saturated vapor pressure at minimum temp.
    IF (EA .EQ. 0.0d0) CALL ESAT(TMIN, EA, DUMMY)
!     if no wind data, use 3 m/s
    IF (UW .EQ. 0.0d0) UW = 3.0d0
!     if wind .LT. 0.2 m/s, set to 0.2 to prevent zero divide
    IF (UW .LT. .20d0) UW = .20d0
!     adjust wind speed from weather station to ZA
    UA = UW * WNDADJ(ZA, DISP, Z0, FETCH, ZW, Z0W)
!     daytime and nighttime average wind speed
    UADTM = UA / (DAYLEN + (1.0d0 - DAYLEN) * WNDRAT)
    UANTM = WNDRAT * UADTM

end subroutine WEATHER

subroutine ZERO (V1, V2, V3, V4, V5, V6)
!     zeroes variables
    IMPLICIT NONE
    real(kind=8) :: V1, V2, V3, V4, V5, V6

    V1 = 0.0d0
    V2 = 0.0d0
    V3 = 0.0d0
    V4 = 0.0d0
    V5 = 0.0d0
    V6 = 0.0d0

end subroutine ZERO

subroutine ZEROA (N, A1, A2, A3, A4)
!     zeroes arrays
    IMPLICIT NONE
    integer :: N, I
    real(kind=8) :: A1(*), A2(*), A3(*), A4(*)

    DO 30 I = 1, N
        A1(I) = 0.0d0
        A2(I) = 0.0d0
        A3(I) = 0.0d0
        A4(I) = 0.0d0
30  CONTINUE

end subroutine ZEROA


    !*************************************************************************************
    ! FUNCTIONS (in alphabetical order)

function FDPSIDW (WETNES,Par,iModel)
!     d PSI / d WETNES, used in 2nd approximation to iteration timestep
    IMPLICIT NONE
!     input
    real(kind=8) :: WETNES   ! wetness, fraction of saturation
    integer :: iModel    ! parameterization of hydraulic functions
    real(kind=8) :: Par(*)  ! parameter array
!                         Clapp and Hornberger (iModel=0)
    real(kind=8) :: PSIF     ! matrix potential at field capacity, kPa
    real(kind=8) :: BEXP     ! exponent for psi-theta relation
    real(kind=8) :: WETINF   ! wetness at dry end of near-saturation range
    real(kind=8) :: WETF     ! saturation fraction at field capacity
    real(kind=8) :: CHM      ! Clapp and Hornberger m, kPa
    real(kind=8) :: CHN      ! Clapp and Hornberger n
!                         Mualem van Genuchten (iModel=1)
!     real(kind=8) :: THS      ! water content at saturation
!     real(kind=8) :: THR      ! residual water content
    real(kind=8) :: ALFA     ! parameter alpha, 1/m
    real(kind=8) :: MVGN     ! parameter n
!     real(kind=8) :: KS       ! hydraulic conductivity at saturation, mm/d
!     real(kind=8) :: A        ! tortuosity parameter (default = 0.5)
!     output
    real(kind=8) :: FDPSIDW  ! d PSI / d WETNES, kPa
!
    real(kind=8) :: MVGM     ! parameter m
    real(kind=8) :: TINY     !
    Parameter (TINY = 1.e-6)

    FDPSIDW = 0.0d0

    if(iModel.eq.0) then
        PSIF=Par(4)
        BEXP=Par(9)
        WETF=Par(5)
        WETINF=Par(10)
        CHM=Par(7)
        CHN=Par(8)
        IF (WETNES .LT. WETINF) THEN
            FDPSIDW = (-BEXP * PSIF / WETF) * (WETNES / WETF) ** (-BEXP - 1.0d0)
        ELSEIF (WETNES .LT. 1.0d0) THEN
!        in near-saturated range
            FDPSIDW = CHM * (2.0d0 * WETNES - CHN - 1.0d0)
        ELSE
!        saturated
            FDPSIDW = 0.0d0
        END IF
    end if
    if(iModel.eq.1) then
        ALFA=Par(7)
        MVGN=Par(8)
        MVGM=1.0d0-1.0d0/MVGN;
        IF (WETNES .le. TINY) THEN
            FDPSIDW = (-1.0d0/ALFA)*(1.0d0/MVGN)*(TINY**(-1.0d0/MVGM)-1)** &
                (1.0d0/MVGN-1)*(-1.0d0/MVGM)*TINY**(-1.0d0/MVGM-1)
        end if
        IF ((WETNES .gt. TINY).and.(WETNES .lt. 1.0d0)) THEN
            FDPSIDW = (-1.0d0/ALFA)*(1.0d0/MVGN)*(WETNES**(-1.0d0/MVGM)-1)** &
                (1.0d0/MVGN-1)*(-1.0d0/MVGM)*WETNES**(-1.0d0/MVGM-1)
        end if
        IF (WETNES .ge. 1.0d0) THEN
            FDPSIDW = 0.0d0
        END IF
        FDPSIDW = FDPSIDW * 9.810d0  ! conversion from m to kPa
    end if

end function FDPSIDW

function FK (Wetnes, Par, iModel)
!     hydraulic conductivity from wetness
    IMPLICIT NONE
!     input
    real(kind=8), intent(in) :: WETNES  ! wetness, fraction of saturation
    integer, intent(in) :: iModel   ! parameterization of hydraulic functions
    real(kind=8), intent(in) :: Par(*)  ! parameter array
!                         Clapp and Hornberger (iModel=0)
!     real(kind=8) :: PSIF     ! matrix potential at field capacity, kPa
    real(kind=8) :: BEXP     ! exponent for psi-theta relation
!     real(kind=8) :: WETINF   ! wetness at dry end of near-saturation range
    real(kind=8) :: WETF     ! saturation fraction at field capacity
!     real(kind=8) :: CHM      ! Clapp and Hornberger m, kPa
!     real(kind=8) :: CHN      ! Clapp and Hornberger n
    real(kind=8) :: KF       ! hydraulic conductivity at field capacity, mm/d
!                        Mualem van Genuchten (iModel=1)
!     real(kind=8) :: THS      ! water content at saturation
!     real(kind=8) :: THR      ! residual water content
    real(kind=8) :: ALFA     ! parameter alpha, 1/m
    real(kind=8) :: MVGN     ! parameter n
    real(kind=8) :: KS       ! hydraulic conductivity at saturation, mm/d
    real(kind=8) :: A        ! tortuosity parameter (default = 0.5)
!     output
    real(kind=8) :: FK       !  hydraulic conductivity, mm/d

    real(kind=8) :: TINY,AWET!
    parameter (TINY = 1.e-6)

    FK = 0.0d0

    if(iModel .eq. 0) then
        KF=Par(3)
        WETF=Par(5)
        BEXP=Par(9)
        IF (WETNES .GT. .00010d0) THEN
            FK=KF * (WETNES / WETF) ** (2.0d0 * BEXP+ 3.0d0)
        ELSE
!      extremely dry
            FK = 1E-10
        end if
    end if
    if(iModel.eq.1) then
        KS=Par(6)
        ALFA=Par(7)
        MVGN=Par(8)
        A=Par(9)
        AWET=MAX(WETNES,TINY)
        AWET=MIN(WETNES,1.0d0)
        FK =KS*AWET**A*(1.0d0-(1.0d0-AWET**(MVGN/(MVGN-1.0d0)))** (1.0d0-1.0d0/MVGN))**2
!        write(10,*) FK,AWET,log(FK)
    end if
end function FK

function FPSIM (WETNES, Par, iModel)
!     matric potential from wetness
    IMPLICIT NONE
!     input
    real(kind=8) :: WETNES   ! wetness, fraction of saturation
    integer :: iModel   ! parameterization of hydraulic functions
    real(kind=8) :: Par(*)  ! parameter array
!                         Clapp and Hornberger (iModel=0)
    real(kind=8) :: PSIF     ! matrix potential at field capacity, kPa
    real(kind=8) :: BEXP     ! exponent for psi-theta relation
    real(kind=8) :: WETINF   ! wetness at dry end of near-saturation range
    real(kind=8) :: WETF     ! saturation fraction at field capacity
    real(kind=8) :: CHM      ! Clapp and Hornberger m, kPa
    real(kind=8) :: CHN      ! Clapp and Hornberger n
!                         Mualem van Genuchten (iModel=1)
!     real(kind=8) :: THS      ! water content at saturation
!     real(kind=8) :: THR      ! residual water content
    real(kind=8) :: ALFA     ! parameter alpha, 1/m
    real(kind=8) :: MVGN     ! parameter n
    real(kind=8) :: MVGM     ! parameter m
!     real(kind=8) :: KS       ! hydraulic conductivity at saturation, mm/d
!     real(kind=8) :: A        ! tortuosity parameter (default = 0.5)
!     output
    real(kind=8) :: FPSIM    ! matric potential, kPa
!     integer :: i
    real(kind=8) :: TINY,AWET!
    parameter (TINY = 1.e-6)

    FPSIM = 0.0d0

    if(iModel.eq.0) then
        PSIF=Par(4)
        BEXP=Par(9)
        WETF=Par(5)
        WETINF=Par(10)
        CHM=Par(7)
        CHN=Par(8)
        IF (WETNES .LE. 0.0d0) THEN
!       arbitrary very negative value
            FPSIM = -1E+10
        ELSEIF (WETNES .LT. WETINF) THEN
            FPSIM = PSIF * (WETNES / WETF) ** (-BEXP)
        ELSEIF (WETNES .LT. 1.0d0) THEN
!        in near-saturated range
            FPSIM = CHM * (WETNES - CHN) * (WETNES - 1.0d0)
        ELSE
!        saturated
            FPSIM = 0.0d0
        END IF
    end if
    if(iModel.eq.1) then
        ALFA=Par(7)
        MVGN=Par(8)
        MVGM=1.0d0-1.0d0/MVGN;
        AWET=MAX(WETNES,TINY)
        AWET=MIN(WETNES,1.0d0)
        FPSIM = (-1.0d0/ALFA)*(AWET**(-1.0d0/MVGM)-1)**(1.0d0/MVGN)
        FPSIM = FPSIM * 9.810d0  ! conversion from m to kPa
    end if
end function FPSIM

function FRSS (RSSA, RSSB, Par, PSIM, PsiCrit) !, iModel
!     soil surface resistance to evaporation
    IMPLICIT NONE
!     input
    real(kind=8) :: RSSA    ! soil evaporation resistance at field capacity, s/m
    real(kind=8) :: RSSB    ! exponent in relation of soil evap res to water
                         ! potential
    real(kind=8) :: PSIM    ! matric potential, kPa
!     Integer iModel  ! parameterization of hydraulic functions
    real(kind=8) :: Par(*)  ! hydraulic parameter
!     output
    real(kind=8) :: FRSS    ! Shuttleworth-Wallace soil surface resistance, s/m
!     function
!     real(kind=8) :: FWetnes, FDPSIDW, FK
!     local
!     real(kind=8) :: Wetnes
!     real(kind=8) :: Diff    ! actual diffusivity
!     real(kind=8) :: DiffF   ! diffusivity at field capacity
    real(kind=8) :: PSIF    ! water potential at field capacity, kPa
    real(kind=8) :: PsiCrit ! critical matric potential, kPa

    FRSS = 0.0d0

    IF (RSSB .LE. .00010d0) THEN
        FRSS = 1E+20
    ELSE
!        PSIF=-6.0
        PSIF=Par(4)
!        Write(*,'(''PSIF = '',F7.1)') PSIF
        IF (PSIM .LT. PSIF) THEN
            FRSS = RSSA * (PSIM / PSIF) ** RSSB  ! original version
        ELSE
            FRSS = RSSA
        END IF
!        Wetnes=FWETNES(PSIM,Par,iModel)
!        Diff=FK(Wetnes,Par,iModel)*FDPSIDW(Wetnes,Par,iModel)
!        DiffF=FK(Par(5),Par,iModel)*FDPSIDW(Par(5),Par,iModel)
!        IF(Diff.eq.0) then
!         FRSS=1.e-20
!         return
!        end if
!        FRSS = RSSA * (DiffF / Diff) ** RSSB
        IF(PSIM.lt.PsiCrit) FRSS=1.e+20
    END IF

end function FRSS

function FTheta (Wetnes,Par,iModel)
!     water content from wetness
    IMPLICIT NONE
!     input
    real(kind=8) :: Wetnes   ! wetness
    integer :: iModel   ! parameterization of hydraulic functions
    real(kind=8) :: Par(*)  ! parameter array
!                         Clapp and Hornberger (iModel=0)
!     real(kind=8) :: THSAT    ! water content at saturation
!                         Mualem van Genuchten (iModel=1)
    real(kind=8) :: THS      ! water content at saturation
    real(kind=8) :: THR      ! residual water content
!     output
    real(kind=8) :: FTheta   ! water content

    FTheta = 0.0d0

    if(iModel.eq.0) then
        FTheta=Wetnes*Par(1)
    end if
    if(iModel.eq.1) then
        THS=Par(1)
        THR=Par(10)
        FTheta=Wetnes*(THS-THR)+THR
    end if
end function FTheta

function FUNC3 (DEC, L2, L1, T3, T2)
!     daily integration for slope after Swift (1976), d
    IMPLICIT NONE
!     input
    real(kind=8) :: DEC     ! declination of the sun, radians
    real(kind=8) :: L2      ! time shift of equivalent slope, radians
    real(kind=8) :: L1      ! latitude of equivalent slope, radians
    real(kind=8) :: T3      ! hour angle of sunset on slope
    real(kind=8) :: T2      ! hour angle of sunrise on slope
!     output
    real(kind=8) :: FUNC3
!     intrinsic
!        SIN, COS

    FUNC3 = 0.0d0

    FUNC3 = (1.0d0 / (2.0d0 * 3.141590d0)) * (SIN(DEC) * SIN(L1) * (T3 - T2) + &
        COS(DEC) * COS(L1) * (SIN(T3 + L2) - SIN(T2 + L2)))

end function FUNC3

function FWETK (K, Par, iModel, pr, timer)
!     wetness from conductivity solved iteratively by Newton method
    IMPLICIT NONE
!     input
    integer iModel   ! parameterization of hydraulic functions
    integer :: pr ! print messages flag
    integer :: timer ! Check-Flag for timelimits set by user
    real(kind=8) :: Par(*)  ! parameter array
!                         Mualem van Genuchten (iModel=1)
!     real(kind=8) :: THS      ! water content at saturation
!     real(kind=8) :: THR      ! residual water content
!     real(kind=8) :: ALFA     ! parameter alpha, 1/m
!     real(kind=8) :: MVGN     ! parameter n
!     real(kind=8) :: KS       ! hydraulic conductivity at saturation, mm/d
    real(kind=8) :: K        ! hydraulic conductivity, mm/d
!     real(kind=8) :: A        ! tortuosity parameter (default = 0.5)
!     output
    real(kind=8) :: FWETK    ! wetness at KF if FWETK -99999.d0 then there is error
!      function
!         real(kind=8) :: FK
!      local
    real(kind=8) :: WetStart
    real(kind=8) :: WetOld
    real(kind=8) :: WetNew
    real(kind=8) :: KOld
    real(kind=8) :: KNew
    integer :: It, ItMax !, Itk, ItKonv ! number of iterations, maximum of that
    real(kind=8) :: Eps      ! precision of solution
    real(kind=8) :: DeltaS   ! relative difference for numerical differentiation
    real(kind=8) :: dKdS1, dKdS2, Konver
    real(kind=8) :: b0,b1,b2,b3

!    parameter (ItKonv = 1000, ItMax=50, DeltaS=0.0010d0, Eps=1.e-6)
     parameter (ItMax=50, DeltaS=0.0010d0, Eps=1.e-6)

    FWETK = 0.0d0

    if(iModel .eq. 0) then
        if (pr .EQ. 1) call intpr("Function FWETK can not be called for iModel=0, stopping program", -1,(/ 0/),0)
        !print*, 'Function FWETK can not be called for iModel=0, stopping program'
        FWETK = -99999.d0
        return
    end if

    if(iModel .eq. 1) then

!        Itk=0 ! VT 2019.11.29 To limit number of iteration for convergence

5       WetStart=0.50d0
        WetOld=WetStart

10      continue

        KOld=FK(WetOld,Par,iModel)-K
        b0=FK(WetOld+DeltaS,Par,iModel)-K
        b1=FK(WetOld+2*DeltaS,Par,iModel)-K
        b2=FK(WetOld-DeltaS,Par,iModel)-K
        b3=FK(WetOld-2*DeltaS,Par,iModel)-K
        dKdS1=(b0-b2)/(2*DeltaS)
        dKdS2=(b1-2*KOld+b3)/(4*DeltaS**2)
        Konver=abs(KOld*dKdS2/dKdS1**2)
!        Write (*,'('' Konv: '',E10.5,'' WetOld: '',F10.7)')Konver,WetOld

        if( (Konver .gt. 1.0d0) .and. (WetOld .lt. 0.950d0) ) then
            WetOld=WetOld+0.050d0
            goto 10
        end if

        if(Konver .gt. 1.0d0) then
            if (pr .EQ. 1) call intpr("FWETK: no convergence found, trying next iteration!", -1,(/ 0/),0)
            if (timer .EQ. 1) call rchkusr()
!            if(Itk .eq. ItKonv) then ! VT 2019.11.29 To limit number of iteration for convergence. This was in !original program
!                if ( pr  .eq. 1 ) call intpr("FWETK: maximum number of iterations exceeded, stopping program!", -1, 0, 0)
!                FWETK = -99999.d0
!                return
!            end if
!            Itk=Itk+1
            K=K/2.0d0
            goto 5
!               stop
        end if

        It=0
20      continue
        b0=FK(WetOld+DeltaS,Par,iModel)-K
        b1=FK(WetOld-DeltaS,Par,iModel)-K
        dKdS1=(b0-b1)/(2*DeltaS)
        if(dKdS1 .ne. 0.0d0) then
            WetNew=WetOld-KOld/dKdS1
        else
            if ( pr .EQ. 1 ) call intpr("FWETK: slope is zero, stopping programm!", -1,(/ 0/), 0)
            FWETK = -99999.d0
            return
        end if
        KNew=FK(WetNew,Par,iModel)-K
        WetOld=WetNew
        KOld=KNew
        It=It+1
!            Write (*,'('' Konv: '',E10.5,'' WetOld: '',F10.7)')Konver,WetOld
        if(It .eq. ItMax) then
            if ( pr .EQ. 1 ) call intpr("FWETK: maximum number of iterations exceeded, adjusting K!", -1,(/ 0/),0)
            if (timer .EQ. 1) call rchkusr()
            K=K/2.0d0
            Goto 5
!              stop
        end if
        if(abs(KNew) .ge. Eps) goto 20
        FWETK=WetOld
    end if
end function FWETK

function FWETNES (PSIM,Par,iModel)
!     wetness from matric potential
    IMPLICIT NONE
!     input
    real(kind=8) :: PSIM    ! matric potential, kPa
    real(kind=8) :: Par(*)  ! parameter array
    integer :: iModel    ! parameterization of hydraulic functions
!                        Clapp and Hornberger (iModel=0)
    real(kind=8) :: PSIF     ! matrix potential at field capacity, kPa
!     real(kind=8) :: PSIINF     ! matrix potential at field capacity, kPa
    real(kind=8) :: BEXP     ! exponent for psi-theta relation
    real(kind=8) :: WETINF   ! wetness at dry end of near-saturation range
    real(kind=8) :: WETF     ! saturation fraction at field capacity
    real(kind=8) :: CHM      ! Clapp and Hornberger m, kPa
    real(kind=8) :: CHN      ! Clapp and Hornberger n
!     real(kind=8) :: THETAF   ! volumetric water content at field capacity"
!                         Mualem van Genuchten (iModel=1)
!     real(kind=8) :: THS      ! water content at saturation
!     real(kind=8) :: THR      ! residual water content
    real(kind=8) :: ALFA     ! parameter alpha, 1/m
    real(kind=8) :: MVGN     ! parameter n
    real(kind=8) :: MVGM     ! parameter m
!     real(kind=8) :: KS       ! hydraulic conductivity at saturation, mm/d
! vt    real(kind=8) :: A        ! tortuosity parameter (default = 0.5)

!     output
    real(kind=8) :: FWETNES  ! wetness, fraction of saturation

    real(kind=8) :: bpsi
!     integer :: i

    FWETNES = 0.0d0

    if(iModel.eq.0) then
        WETF = Par(5)
        WETINF=Par(10)
        BEXP=Par(9)
        PSIF=Par(4)
        CHM = Par(7)
        CHN = Par(8)
!       write(10,*)'Function FWETNES',(Par(i),i=1,10)
!       IF (PSIM .GT. 0.) THEN
!        PRINT*, 'matrix psi must be negative or zero'
!        STOP
!       END IF
        IF (PSIM .GE. 0.) THEN
            FWETNES = 1.0d0
        ELSE
            FWETNES = WETF * (PSIM / PSIF) ** (-1.0d0 / BEXP)
            IF (FWETNES .GT. WETINF) THEN
                FWETNES = 0.50d0*((1.0d0 + CHN) + SQRT((CHN-1.0d0)**2.0d0+ 4.0d0*PSIM/CHM))
            END IF
        END IF
    end if
    if(iModel.eq.1) then
!       write(10,*)'Function FWETNES',(Par(i),i=1,10)
        ALFA=Par(7)
        MVGN=Par(8)
        MVGM=1.0d0-1.0d0/MVGN;
        bpsi = PSIM/9.810d0   ! conversion from kPa to m

        IF (bpsi .le. 0.0d0) THEN
            FWETNES =(1.0d0+(-ALFA*bpsi)**MVGN)**(-MVGM)
        ELSE
            FWETNES = 1.0d0
        END IF
!       write(10,*)'FWETNES= ',FWETNES,' psi[m]= ',bpsi
    end if
end function FWETNES

function HAFDAY (LAT, DEC)
!     half day length in radians
    IMPLICIT NONE
!     inputs
    real(kind=8) :: LAT     ! latitude, radians (S neg)
    real(kind=8) :: DEC     ! declination of the sun, radians
!     output
    real(kind=8) :: HAFDAY  ! half daylength, radians
!     local
    real(kind=8) :: ARG
    real(kind=8) :: PI
!     intrinsic
!        ABS, TAN, ACOS
    HAFDAY = 0.0d0

    PI = 3.14159
    IF (ABS(LAT) .GE. (PI / 2.0d0) .AND. LAT .LT. 0.0d0) then
        LAT = -1.0d0 * (PI / 2.0d0 - .010d0)
    end if

    IF (ABS(LAT) .GE. (PI / 2.0d0) .AND. LAT .GT. 0.0d0) then
        LAT = PI / 2.0d0 - .010d0
    end if

    ARG = -TAN(DEC) * TAN(LAT)

    IF (ARG .GE. 1.0d0) THEN
!        sun stays below horizon
        HAFDAY = 0.0d0
    ELSEIF (ARG .LE. -1.0d0) THEN
!        sun stays above horizon
        HAFDAY = PI
    ELSE
        HAFDAY = ACOS(ARG)
    END IF

end function HAFDAY

function INTERP (NPAIRS, FUNCT, XVALUE)
!     interpolates between points in data functions
    IMPLICIT NONE
!     input
    integer :: NPAIRS  ! number of pairs of values to be used
    real(kind=8) ::  FUNCT(*)! array of pairs of values: x1, y1, x2, y2, ...
    real(kind=8) ::  XVALUE  ! x value
!     output
    real(kind=8) ::  INTERP  ! y value
!     local
    integer :: I, J ! DO indexes
    real(kind=8) ::  XX(1:10) ! Cseries of x values of FUNCT
    real(kind=8) ::  YY(1:10) ! Cseries of y values of FUNCT

    INTERP = 0.0d0
!     put FUNCT into XX and YY
    I = 0
    DO 10 j = 1, 2 * NPAIRS - 1, 2
        I = I + 1
        XX(I) = FUNCT(j)
        YY(I) = FUNCT(j + 1)
10  CONTINUE
!     interpolate using XX and YY
    DO 20 j = 1, NPAIRS
        IF (XVALUE .EQ. XX(j)) THEN
            INTERP = YY(j)
            RETURN
        ELSEIF (XVALUE .LT. XX(j)) THEN
            INTERP = YY(j - 1) + (XVALUE - XX(j - 1)) * (YY(j) - YY(j - 1)) / (XX(j) - XX(j - 1))
            RETURN
        ELSE
        END IF
20  CONTINUE

end function INTERP

function PM (AA, VPD, DELTA, RA, RC)
!     Penman-Monteith transpiration rate equation
    IMPLICIT NONE
!     input
    real(kind=8) :: AA      ! net energy input, Rn - S, W/m2
    real(kind=8) :: VPD     ! vapor pressure deficit, kPa
    real(kind=8) :: DELTA   ! dEsat/dTair, kPa/K
    real(kind=8) :: RA      ! boundary layer resistance, s/m
    real(kind=8) :: RC      ! canopy resistance, s/m
!     output
    real(kind=8) :: PM      ! Penman-Monteith latent heat flux density, W/m2

    PM = 0.0d0

    PM = (RA * DELTA * AA + CPRHO * VPD) / ((DELTA + GAMMA) * RA + GAMMA * RC)

end function PM

function WNDADJ (ZA, DISP, Z0, FETCH, ZW, Z0W)
!     ratio of wind speed at reference height (above canopy) to
!        wind speed at weather station
    IMPLICIT NONE
!    input
    real(kind=8) :: ZA      ! reference height, m
    real(kind=8) :: DISP    ! height of zero-plane, m
    real(kind=8) :: Z0      ! roughness parameter, m
    real(kind=8) :: FETCH   ! weather station fetch, m
    real(kind=8) :: ZW      ! weather station measurement height for wind,
                         ! above any zero plane, m
    real(kind=8) :: Z0W     ! weather station roughness parameter, m
!     output
    real(kind=8) :: WNDADJ  ! ratio
!     local
    real(kind=8) :: HIBL    ! height of internal boundary layer, m
!     intrinsic
!        LOG

    WNDADJ = 0.0d0

    IF (Z0W .LT. .0000010d0) THEN
        WNDADJ = 1.0d0
    ELSE
!        Brutsaert (1982) equation 7-39
        HIBL = .3340d0 * FETCH ** .8750d0 * Z0W ** .1250d0
!        Brutsaert equations 7-41 and 4-3
        WNDADJ = LOG(HIBL / Z0W) * LOG((ZA - DISP) / Z0) / (LOG(HIBL / Z0) * LOG(ZW / Z0W))
    END IF

end function WNDADJ

end module fbrook_mod
