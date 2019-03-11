    !*************************************************************************************
!     PFILE.INC reads parameters from the input


    ! output specifications -------
    NDAYS = INT( param( 1 ) )
    HEAT = INT( param( 2 ) )
    OP(:,:) = output(:,:)
    
    ! Meteorologic parameters -------
    ESLOPE = param( 3 )
    ASPECT = param( 4 )
    
    ! Convert to radians
    ESLOPE = ESLOPE / 57.296
    ASPECT = ASPECT / 57.296
    
    ALB = param( 5 )
    ALBSN = param( 6 )
    C1 = param( 7 )
    C2 = param( 8 )
    C3 = param( 9 )
    WNDRAT = param( 10 )
    FETCH = param( 11 )
    Z0W = param( 12 )
    ZW = param( 13 )
    
    ! Canopy parameters -------
    LWIDTH = param( 14 )
    Z0G = param( 15 )
    Z0S = param( 16 )
    LPC = param( 17 )
    CS = param( 18 )
    CZS = param( 19 )
    CZR = param( 20 )
    HS = param( 21 )
    HR = param( 22 )
    ZMINH = param( 23 )
    RHOTP = param( 24 )
    NN = param( 25 )
    
    ! Interception parameters -------
    RSTEMP = param( 26 )
    DURATN = paramYear( 1, : )
    INTR = param( 27 )
    INTS = param( 28 )
    FRINTL = param( 29 )
    FSINTL = param( 30 )
    FRINTS = param( 31 )
    FSINTS = param( 32 )
    CINTRL = param( 33 )
    CINTRS = param( 34 )
    CINTSL = param( 35 )
    CINTSS = param( 36 )
    
    ! Snow parameters -------
    MELFAC = param( 37 )
    CCFAC = param( 38 )
    LAIMLT = param( 39 )
    SAIMLT = param( 40 )
    GRDMLT = param( 41 )
    MAXLQF = param( 42 )
    KSNVP = param( 43 )
    SNODEN = param( 44 )
    
    ! leaf parameters affecting PE -------
    GLMAX = param( 45 )
    CR = param( 46 )
    GLMIN = param( 47 )
    RM = param( 48 )
    R5 = param( 49 )
    CVPD = param( 50 )
    TL = param( 51 )
    T1 = param( 52 )
    T2 = param( 53 )
    TH = param( 54 )
    
    ! plant parameters affecting soil-water supply -------
    MXKPL = param( 55 )
    MXRTLN = param( 56 )
    inirlen = param( 57 )
    inirdep = param( 58 )
    rgrorate = param( 59 )
    rgroper = param( 60 )
    FXYLEM = param( 61 )
    PSICR = param( 62 )
    RTRAD = param( 63 )
    NOOUTF = INT( param( 64 ) )
    
    if( FXYLEM .GE. 0.990d0 ) FXYLEM = 0.990d0
    if( inirlen .LT. 0.010d0 ) inirlen=0.010d0
    if( inirdep .LT. 0.010d0 ) inirdep=0.010d0
    
    ! soil parameters -------
    NLAYER = INT( param( 65 ) )
    nmat = INT( param( 66 ) )
    ILAYER = INT( param( 67 ) )
    QLAYER = INT( param( 68 ) )
    IMODEL = INT( param( 69 ) )
    
    if ( (NLAYER .GT. ML) .OR. (ILAYER .GT. NLAYER) .OR. (QLAYER .GT. NLAYER)) then
        PRINT*, 'Failure of QLAYER and ILAYER .LE. NLAYER .LE. ML'
        STOP
    end if
    
    DO 201 I=1, nmat
        if (imodel .eq. 0) then
            ParMat(1,I) = materials(I,2)
            ParMat(2,I) = materials(I,2)
            ParMat(4,I) = materials(I,3)
            ParMat(9,I) = materials(I,4)
            ParMat(3,I) = materials(I,5)
            ParMat(10,I) = materials(I,7)
            StonefMat(I) = materials(I,8)
            ParMat(5,I) = 0.0d0
            ParMat(6,I) = 0.0d0
            ParMat(7,I) = 0.0d0
            ParMat(8,I) = 0.0d0
        end if
        
        if(imodel .eq. 1) then
            ParMat(1,I) = materials(I,2)
            ParMat(10,I) = materials(I,3)
            ParMat(7,I) = materials(I,4)
            ParMat(8,I) = materials(I,5)
            ParMat(6,I) = materials(I,6)
            ParMat(9,I) = materials(I,7)
            StonefMat(I) = materials(I,8)
            ParMat(2,I) = 0.0d0
            ParMat(4,I) = 0.0d0
            ParMat(5,I) = 0.0d0
            !..it's a sin......
            ! Hard default [mm d-1] for saturated hydraulic conductivity at field capacity
            ParMat(3,I) = 2
        end if
201 CONTINUE
    
    
    DO 202 I=1, NLAYER
        if ( HEAT .EQ. 1 ) then
            dep(I) = soil(I,2) 
            THICK(I) = soil(I,3) 
            mat(I) = INT( soil(I,4) )
            PSIM(I) = soil(I,5) 
            frelden(I) = soil(I,6) 
            ! TemperatureNew(i) = soil(I,7) we don't have it in the input file!!!
            
            if(I .LT. NLAYER) then
                MUE(I) = THICK(I) / ( THICK(I) + THICK(I+1) )
                ZL(I) = 0.50d0 * ( THICK(I) + THICK(I+1) )
            else
                MUE(I) = 0.50d0
                ZL(I) = THICK(I)
            end if
            
            TMean(I) = 0.0d0
        else
            dep(I) = soil(I,2) 
            THICK(I) = soil(I,3) 
            mat(I) = INT( soil(I,4) )
            PSIM(I) = soil(I,5) 
            frelden(I) = soil(I,6) 
        end if
202 CONTINUE
    
    depmax = dep(1) - THICK(1) / 1000.0d0
    !..from material-specific to layer-specific parameter values
    DO 203 I=1, NLAYER
        DO 204 J=1, MPAR
            PAR(J,I) = ParMat(J,mat(I))
204     CONTINUE
        STONEF(I) = StonefMat( mat(I) )
203 CONTINUE
    
    RSSA = param( 70 )
    RSSB = param( 71 )
    
    ! find thickness of maximum root zone
    ifoundfirst = 0
    i1=0
    i2=0
    DO 205 I=1, NLAYER
        if(ifoundfirst .EQ. 0) then
            if( frelden(I) .GE. 1.e-6) then
                ifoundfirst = 1
                i1 = I
            end if
        end if
        if( frelden(I) .GE. 1.e-6) i2 = I-1
205 CONTINUE
    
    if( (ifoundfirst .eq. 1) .and. (i2 .lt. i1) ) i2 = NLAYER
    !write(*,*)' i1= ',i1,' i2= ',i2
    
    DO 206 I=1,NLAYER
        tini(I) = 1.e+20
        if( (I .GE. i1) .AND. (I .LE. i2) ) frelden(I) = MAX( frelden(I), 1.01e-6)
        if( (frelden(I) .GE. 1.e-6) .AND. (depmax-dep(I)) .LE. inirdep) then
            tini(I) = 0.0d0
        end if
        if( (frelden(I) .GE. 1.e-6) .AND. (depmax-dep(I)) .GT. inirdep) then
            if(rgrorate .GT. 0) then
                tini(I) = (depmax-dep(I)-inirdep)/rgrorate
            end if
        end if
        !write(*,*)'dep= ',dep(I),' tini= ',tini(I)
206 CONTINUE

    ! flow parameters -------
    INFEXP = param( 72 )
    BYPAR = INT( param( 73 ) )
    QFPAR = param( 74 )
    QFFC = param( 75 )
    IMPERV = param( 76 )
    DSLOPE = param( 77 )
    LENGTH = param( 78 )
    DRAIN = param( 79 )
    GSC = param( 80 )
    GSP = param( 81 )
    
    ! integration parameters -------
    DTIMAX = param( 82 )
    DSWMAX = param( 83 )
    DPSIMX = param( 84 )
    
    ! heat flow -------
    ! we assign some so compilation does not complain
    TopInfT = 1
!       if (HEAT .EQ. 1) then
!        READ (12,*) Comment
!        READ (12,*) tTop, Comment
!        tTop=tTop
!        READ (12,*) tBot, Comment
!        tBot=tBot
!        READ (12,*) TopInfT, Comment
!        READ (12,*) BotInfT, Comment
!        READ (12,*) kTopT, Comment
!        READ (12,*) kBotT, Comment
!        DO 207 I = 1, 7
!         READ (12,*) Comment
! 207    CONTINUE
!        DO 208 I = 1, nmat
!         READ (12,*) ilay, SV(I), OV(I), HB1(I), HB2(I), HB3(I)
!         TPar(1,I) = SV(I)
!         TPar(2,I) = OV(I)
!         TPar(3,I) = THDis
! C        thermal conductivities -- transfer from [J m-1 s-1 K-1] to  [J mm-1 d-1 K-1]
!         TPar(4,I) = HB1(I) * 86.400
!         TPar(5,I) = HB2(I) * 86.400
!         TPar(6,I) = HB3(I) * 86.400
! C         volumetric heat capacities for solid, water and organic -- transfer from [MJ m-2 mm-1 K-1] to [J mm-3 K-1]
!         TPar(7,I) = CVSOL
!         TPar(8,I) = CVORG
!         TPar(9,I) = CVLQ
! 208    CONTINUE
!        READ (12,*) C
!       end if