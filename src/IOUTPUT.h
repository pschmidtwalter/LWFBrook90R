!     ******   IOUTPUT.INC   ******
!     iteration output, flows as rates in mm/d

IF (OP(6,5) .EQ. 1) THEN
    IF (IDAY .EQ. 1 .AND. N .EQ. 1 .AND. NITS .EQ. 1) THEN
        OPEN (UNIT = 16, FILE = 'out/BELO.ITR', STATUS='REPLACE')
        WRITE (16,'(''YR,MO,DA,DOY,NP,NITS,DTI,NL,INFLI,BYFLI,TRANI,SLVP,VRFLI,DSFLI,NTFLI'')')
    END IF
    WRITE (16,'(I4,A1,2(I3,A1),2(I4,A1),I5,A1,F7.4,A1,I3,A1,7(F7.2,A1))') YEAR,',',MONTH,',',&
        DOM,',',DOY,',',N,',',NITS,',',DTI,',',1,',',INFLI(1),',',BYFLI(1),',',TRANI(1),',',&
        SLVP,',',VRFLI(1),',',DSFLI(1),',',NTFLI(1)
    DO 12 I = 2, NLAYER
        WRITE(16,'(I4,A1,2(I3,A1),2(I4,A1),I5,A1,F7.4,A1,I3,A1,7(F7.2,A1))') YEAR,',',MONTH,',',&
            DOM,',',DOY,',',N,',',NITS,',',DTI,',',I,',',INFLI(I),',',BYFLI(I),',',&
            TRANI(I),',',0.0d0,',',VRFLI(I),',',DSFLI(I),',',NTFLI(I)
12  CONTINUE
    WRITE (16,*)
END IF

IF (OP(8,5) .EQ. 1) THEN
    IF (IDAY .EQ. 1 .AND. N .EQ. 1 .AND. NITS .EQ. 1) THEN
        OPEN (UNIT = 17, FILE = 'out/PSIT.ITR', STATUS='REPLACE')
        WRITE(17,'(''YR   MO DA DOY  NP NITS    DTI   PSIT1   PSIT2   PSIT3   PSIT4   PSIT5   PSIT6   PSIT7...'')')
        WRITE (17,'(''                      PSIG   '',50F8.1)') (PSIG(I), I = 1,NLAYER)
    END IF
    WRITE (17,'(I4,2I3,2I4,I5,F7.4,50F8.1)') YEAR, MONTH, DOM,DOY, N, NITS, DTI, (PSITI(I), I = 1,NLAYER)
END IF
