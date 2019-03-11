!     ******   PACCUM.INC   ******
!           accumulate flows over precip interval (below ground only)
!           zeroed by ZPINT.INC
            DO 52 I = 1, NLAYER
               VRFLPI(I) = VRFLPI(I) + VRFLI(I) * DTI
               INFLPI(I) = INFLPI(I) + INFLI(I) * DTI
               BYFLPI(I) = BYFLPI(I) + BYFLI(I) * DTI
               DSFLPI(I) = DSFLPI(I) + DSFLI(I) * DTI
               NTFLPI(I) = NTFLPI(I) + NTFLI(I) * DTI
               TRANPI(I) = TRANPI(I) + TRANI(I) * DTI
!              note TRANI() are constant over precipitation interval
   52       CONTINUE
            PSLVPD = PSLVPD + PSLVP * DTI
            SLVPD = SLVPD + SLVP * DTI
            SRFLP = SRFLP + SRFL * DTI
            SLFLP = SLFLP + SLFL * DTI
            GWFLP = GWFLP + GWFL * DTI
            SEEPP = SEEPP + SEEP * DTI
