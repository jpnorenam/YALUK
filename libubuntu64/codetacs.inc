      IF ( ISTEP .EQ. 1 )  THEN
      UD1(   5738) = UD1(   5738) - TENFLZ
      UD1(   5739) = UD1(   5739) - TENFLZ
      UD1(   5743) = UD1(   5743) - TENFLZ
      UD1(   5744) = UD1(   5744) - TENFLZ
      UD1(   5748) = UD1(   5748) - TENFLZ
      UD1(   5749) = UD1(   5749) - TENFLZ
      UD1(   5753) = UD1(   5753) - TENFLZ
      UD1(   5754) = UD1(   5754) - TENFLZ
      UD1(   5758) = UD1(   5758) - TENFLZ
      UD1(   5759) = UD1(   5759) - TENFLZ
      UD1(   5763) = UD1(   5763) - TENFLZ
      UD1(   5764) = UD1(   5764) - TENFLZ
      UD1(   5768) = UD1(   5768) - TENFLZ
      UD1(   5769) = UD1(   5769) - TENFLZ
      UD1(   5773) = UD1(   5773) - TENFLZ
      UD1(   5774) = UD1(   5774) - TENFLZ
      UD1(   5778) = UD1(   5778) - TENFLZ
      UD1(   5779) = UD1(   5779) - TENFLZ
      UD1(   5783) = UD1(   5783) - TENFLZ
      UD1(   5784) = UD1(   5784) - TENFLZ
      UD1(   5788) = UD1(   5788) - TENFLZ
      UD1(   5789) = UD1(   5789) - TENFLZ
      UD1(   5793) = UD1(   5793) - TENFLZ
      UD1(   5794) = UD1(   5794) - TENFLZ
      UD1(   5798) = UD1(   5798) - TENFLZ
      UD1(   5799) = UD1(   5799) - TENFLZ
         ENDIF
      CALL MOVER0 ( XTCS(  44671),      13 )
C   Following is for user source   1.   Type =11.    Name = "VREFD "
      VREFD  = UD1(   5735)
C   Following is for user source   2.   Type =90.    Name = "TRSA  "
      TRSA   = E(     34)
C   Following is for user source   3.   Type =90.    Name = "TRSB  "
      TRSB   = E(     33)
C   Following is for user source   4.   Type =90.    Name = "TRSC  "
      TRSC   = E(     32)
C   Following is for user source   5.   Type =91.    Name = "RMAB  "
      L = IABS ( KPOS(      1) )
      D1 = DABS ( UD1(   5759) - UD1(   5758) - ONEHAF )
      IF ( D1 .LT. FLZERO )  THEN
      IF ( L .EQ. 11 ) RMAB   = TCLOSE(      1)
            ELSE
      IF ( L .LE.  3 ) RMAB   = TCLOSE(      1)
            ENDIF
C   Following is for user source   6.   Type =91.    Name = "RMBC  "
      L = IABS ( KPOS(      2) )
      D1 = DABS ( UD1(   5764) - UD1(   5763) - ONEHAF )
      IF ( D1 .LT. FLZERO )  THEN
      IF ( L .EQ. 11 ) RMBC   = TCLOSE(      2)
            ELSE
      IF ( L .LE.  3 ) RMBC   = TCLOSE(      2)
            ENDIF
C   Following is for user source   7.   Type =91.    Name = "RMCA  "
      L = IABS ( KPOS(      3) )
      D1 = DABS ( UD1(   5769) - UD1(   5768) - ONEHAF )
      IF ( D1 .LT. FLZERO )  THEN
      IF ( L .EQ. 11 ) RMCA   = TCLOSE(      3)
            ELSE
      IF ( L .LE.  3 ) RMCA   = TCLOSE(      3)
            ENDIF
C   Following is for user source   8.   Type =91.    Name = "TRXA  "
      L = IABS ( KPOS(      4) )
      D1 = DABS ( UD1(   5774) - UD1(   5773) - ONEHAF )
      IF ( D1 .LT. FLZERO )  THEN
      IF ( L .EQ. 11 ) TRXA   = TCLOSE(      4)
            ELSE
      IF ( L .LE.  3 ) TRXA   = TCLOSE(      4)
            ENDIF
C   Following is for user source   9.   Type =91.    Name = "TRXB  "
      L = IABS ( KPOS(      5) )
      D1 = DABS ( UD1(   5779) - UD1(   5778) - ONEHAF )
      IF ( D1 .LT. FLZERO )  THEN
      IF ( L .EQ. 11 ) TRXB   = TCLOSE(      5)
            ELSE
      IF ( L .LE.  3 ) TRXB   = TCLOSE(      5)
            ENDIF
C   Following is for user source  10.   Type =91.    Name = "TRXC  "
      L = IABS ( KPOS(      6) )
      D1 = DABS ( UD1(   5784) - UD1(   5783) - ONEHAF )
      IF ( D1 .LT. FLZERO )  THEN
      IF ( L .EQ. 11 ) TRXC   = TCLOSE(      6)
            ELSE
      IF ( L .LE.  3 ) TRXC   = TCLOSE(      6)
            ENDIF
C   Following is for user source  11.   Type =23.    Name = "FRLA  "
 1000 IF ( T .GE. UD1(   5788) )  THEN
      D17 = UD1(   5788) + UD1(   5786)
      IF ( T .LT. D17 ) THEN
      FRLA   = UD1(   5785)
            ELSE
      UD1(   5788) = UD1(   5788) + UD1(   5787)
      GO TO 1000
            ENDIF
         ENDIF
C   Following is for user source  12.   Type =23.    Name = "FRLB  "
 1001 IF ( T .GE. UD1(   5793) )  THEN
      D17 = UD1(   5793) + UD1(   5791)
      IF ( T .LT. D17 ) THEN
      FRLB   = UD1(   5790)
            ELSE
      UD1(   5793) = UD1(   5793) + UD1(   5792)
      GO TO 1001
            ENDIF
         ENDIF
C   Following is for user source  13.   Type =23.    Name = "FRLC  "
 1002 IF ( T .GE. UD1(   5798) )  THEN
      D17 = UD1(   5798) + UD1(   5796)
      IF ( T .LT. D17 ) THEN
      FRLC   = UD1(   5795)
            ELSE
      UD1(   5798) = UD1(   5798) + UD1(   5797)
      GO TO 1002
            ENDIF
         ENDIF
C   Following is supplemental variable  I =   1
      QTCR    = 33.3*10**6
C   Following is supplemental variable  I =   2
      QREF    = 0.30
C   Following is supplemental variable  I =   3
      TRAB    =  TRSA - TRSB
C   Following is supplemental variable  I =   4
      TRBC    =  TRSB - TRSC
C   Following is supplemental variable  I =   5
      TRCA    =  TRSC - TRSA
C   Following is supplemental variable  I =   6
      TABX    =  TRAB/34500.D0
C   Following is supplemental variable  I =   7
      TBCX    =  TRBC/34500.D0
C   Following is supplemental variable  I =   8
      TCAX    =  TRCA/34500.D0
C    ------  Begin Type-66 RMS averaging               I =   9.    Name = "TXNAB "
      B = XTCS(  45317)
      N8 = IVARB( 194092) + 1
      IF ( N8 .GT.     360 )  N8 = 1
      IVARB( 194092) = N8
      NDX1 =   9002 + N8
      STORE1(KSTORE+      9) = STORE1(KSTORE+      9) - PARSUP(NDX1)
      PARSUP(NDX1) = B * B
      STORE1(KSTORE+      9) = STORE1(KSTORE+      9) + PARSUP(NDX1)
      IF ( ISTEP .GT. 1 )  THEN
      A = STORE1(KSTORE+      9)
         ELSE
      A = 0.0
      DO 1003  JK=1,   360
 1003 A = A + PARSUP(   9002+JK)
      STORE1(KSTORE+      9) = A
         ENDIF
      A = DSQRT ( A * PARSUP(   9002) )
      XTCS(  45320) = A  ! End supplemen device "TXNAB "
C    ------  Begin Type-66 RMS averaging               I =  10.    Name = "TXNBC "
      B = XTCS(  45318)
      N8 = IVARB( 194097) + 1
      IF ( N8 .GT.     360 )  N8 = 1
      IVARB( 194097) = N8
      NDX1 =   9363 + N8
      STORE1(KSTORE+     10) = STORE1(KSTORE+     10) - PARSUP(NDX1)
      PARSUP(NDX1) = B * B
      STORE1(KSTORE+     10) = STORE1(KSTORE+     10) + PARSUP(NDX1)
      IF ( ISTEP .GT. 1 )  THEN
      A = STORE1(KSTORE+     10)
         ELSE
      A = 0.0
      DO 1004  JK=1,   360
 1004 A = A + PARSUP(   9363+JK)
      STORE1(KSTORE+     10) = A
         ENDIF
      A = DSQRT ( A * PARSUP(   9363) )
      XTCS(  45321) = A  ! End supplemen device "TXNBC "
C    ------  Begin Type-66 RMS averaging               I =  11.    Name = "TXNCA "
      B = XTCS(  45319)
      N8 = IVARB( 194102) + 1
      IF ( N8 .GT.     360 )  N8 = 1
      IVARB( 194102) = N8
      NDX1 =   9724 + N8
      STORE1(KSTORE+     11) = STORE1(KSTORE+     11) - PARSUP(NDX1)
      PARSUP(NDX1) = B * B
      STORE1(KSTORE+     11) = STORE1(KSTORE+     11) + PARSUP(NDX1)
      IF ( ISTEP .GT. 1 )  THEN
      A = STORE1(KSTORE+     11)
         ELSE
      A = 0.0
      DO 1005  JK=1,   360
 1005 A = A + PARSUP(   9724+JK)
      STORE1(KSTORE+     11) = A
         ENDIF
      A = DSQRT ( A * PARSUP(   9724) )
      XTCS(  45322) = A  ! End supplemen device "TXNCA "
C    ------  Begin Type-52 level-triggered switch      I =  12.    Name = "FAB1  "
      B = XTCS(  44668)
      B = B * PARSUP(  10085)
      A = 0.0
      D9 = PARSUP(  10086)
      D = XTCS(  44675)
      IF ( D .GE. D9 ) A = B
      XTCS(  45323) = A  ! End supplemen device "FAB1  "
C    ------  Begin Type-52 level-triggered switch      I =  13.    Name = "FAB2  "
      B = XTCS(  44668)
      B = B * PARSUP(  10088)
      A = 0.0
      D9 = PARSUP(  10089)
      D = XTCS(  44675)
      IF ( D .LT. D9 ) A = B
      XTCS(  45324) = A  ! End supplemen device "FAB2  "
C   Following is supplemental variable  I =  16
      DELIN   = .004167
C    ------  Begin Type-52 level-triggered switch      I =  17.    Name = "FBC1  "
      B = XTCS(  44668)
      B = B * PARSUP(  10098)
      A = 0.0
      D9 = PARSUP(  10099)
      D = XTCS(  44676)
      IF ( D .GE. D9 ) A = B
      XTCS(  45328) = A  ! End supplemen device "FBC1  "
C    ------  Begin Type-52 level-triggered switch      I =  18.    Name = "FBC2  "
      B = XTCS(  44668)
      B = B * PARSUP(  10101)
      A = 0.0
      D9 = PARSUP(  10102)
      D = XTCS(  44676)
      IF ( D .LT. D9 ) A = B
      XTCS(  45329) = A  ! End supplemen device "FBC2  "
C    ------  Begin Type-52 level-triggered switch      I =  21.    Name = "FCA1  "
      B = XTCS(  44668)
      B = B * PARSUP(  10110)
      A = 0.0
      D9 = PARSUP(  10111)
      D = XTCS(  44677)
      IF ( D .GE. D9 ) A = B
      XTCS(  45332) = A  ! End supplemen device "FCA1  "
C    ------  Begin Type-52 level-triggered switch      I =  22.    Name = "FCA2  "
      B = XTCS(  44668)
      B = B * PARSUP(  10113)
      A = 0.0
      D9 = PARSUP(  10114)
      D = XTCS(  44677)
      IF ( D .LT. D9 ) A = B
      XTCS(  45333) = A  ! End supplemen device "FCA2  "
C    ------  Begin Type-53 transport delay             I =  25.    Name = "TRIA  "
      B = XTCS(  44678)
      N6 = PARSUP(  10124)
      N7 = IVARB( 194170)
      D7 = PARSUP(  10123)
      D7 = D7 * DLTINV
 1006 D8 = D7 + TENFLZ
      D9 = D7 - TENFLZ
      IF ( D8 .LT. 0.0 )  THEN
      IF ( T .GE. TIMTAC )  THEN
      LIMTAC = LIMTAC - 1
      IF ( LIMTAC .GE. 0 )    THEN
      NDX6 = ILNTAB( 212556)
      INDPRT = KENFNT(KOVM10+4) + 1
      BUS5 = TEXVEC(NDX6)
      FLSTAT(12) = T
      CALL TFLUSH
               ENDIF
            ENDIF
      D7 = 0.0
      GO TO 1006
         ENDIF
      IF ( D9 .LE. 0.0 )  THEN
      A = B
      GO TO 1007
         ENDIF
      M1 = 0
      J = D9
      IF ( D9 .GT. J ) J = J + 1
      IF ( J .LE. D8 ) M1 = 1
      IF ( J .GT. N6 ) THEN
      IF ( IUTY( 176277) .NE. 0 .AND. T .GE. TIMTAC )     THEN
      LIMTAC = LIMTAC - 1
      IF ( LIMTAC .GE. 0 )    THEN
      INDPRT = KENFNT(KOVM10+4) + 2
      D4 = DELTAT * N6
      NDX6 = ILNTAB( 212556)
      BUS5 = TEXVEC(NDX6)
      FLSTAT(13) = D4
      FLSTAT(14) = T
      CALL TFLUSH
               ENDIF
            ENDIF
      J = N6
      M1 = 1
         ENDIF
      N4 = N7 - J
      IF ( N4 .LT.    1132) N4 = N4 +    91
      A = PARSUP(   8994+N4)
      IF ( M1 .EQ. 1 ) GO TO  1007
      N3 = N4 + 1
      IF ( N3 .GE.    1223 ) N3 = N3 -    91
      D5 = B
      IF ( J .GT. 1 ) D5 = PARSUP(   8994+N3)
      A = A - ( J - D7 ) * ( A - D5 )
 1007 XTCS(  45336) = A  ! End supplemen device
C    ------  Begin Type-53 transport delay             I =  26.    Name = "TRVA  "
      B = XTCS(  44672)
      N6 = PARSUP(  10219)
      N7 = IVARB( 194175)
      D7 = PARSUP(  10218)
      D7 = D7 * DLTINV
 1008 D8 = D7 + TENFLZ
      D9 = D7 - TENFLZ
      IF ( D8 .LT. 0.0 )  THEN
      IF ( T .GE. TIMTAC )  THEN
      LIMTAC = LIMTAC - 1
      IF ( LIMTAC .GE. 0 )    THEN
      NDX6 = ILNTAB( 212557)
      INDPRT = KENFNT(KOVM10+4) + 1
      BUS5 = TEXVEC(NDX6)
      FLSTAT(12) = T
      CALL TFLUSH
               ENDIF
            ENDIF
      D7 = 0.0
      GO TO 1008
         ENDIF
      IF ( D9 .LE. 0.0 )  THEN
      A = B
      GO TO 1009
         ENDIF
      M1 = 0
      J = D9
      IF ( D9 .GT. J ) J = J + 1
      IF ( J .LE. D8 ) M1 = 1
      IF ( J .GT. N6 ) THEN
      IF ( IUTY( 176277) .NE. 0 .AND. T .GE. TIMTAC )     THEN
      LIMTAC = LIMTAC - 1
      IF ( LIMTAC .GE. 0 )    THEN
      INDPRT = KENFNT(KOVM10+4) + 2
      D4 = DELTAT * N6
      NDX6 = ILNTAB( 212557)
      BUS5 = TEXVEC(NDX6)
      FLSTAT(13) = D4
      FLSTAT(14) = T
      CALL TFLUSH
               ENDIF
            ENDIF
      J = N6
      M1 = 1
         ENDIF
      N4 = N7 - J
      IF ( N4 .LT.    1227) N4 = N4 +    91
      A = PARSUP(   8994+N4)
      IF ( M1 .EQ. 1 ) GO TO  1009
      N3 = N4 + 1
      IF ( N3 .GE.    1318 ) N3 = N3 -    91
      D5 = B
      IF ( J .GT. 1 ) D5 = PARSUP(   8994+N3)
      A = A - ( J - D7 ) * ( A - D5 )
 1009 XTCS(  45337) = A  ! End supplemen device
C   Following is supplemental variable  I =  27
      QINA    =( -TRSA * TRIA * 0.5  +  TRXA * TRVA * 0.5 ) / QTCR
C    ------  Begin Type-53 transport delay             I =  28.    Name = "TRIB  "
      B = XTCS(  44679)
      N6 = PARSUP(  10316)
      N7 = IVARB( 194210)
      D7 = PARSUP(  10315)
      D7 = D7 * DLTINV
 1010 D8 = D7 + TENFLZ
      D9 = D7 - TENFLZ
      IF ( D8 .LT. 0.0 )  THEN
      IF ( T .GE. TIMTAC )  THEN
      LIMTAC = LIMTAC - 1
      IF ( LIMTAC .GE. 0 )    THEN
      NDX6 = ILNTAB( 212559)
      INDPRT = KENFNT(KOVM10+4) + 1
      BUS5 = TEXVEC(NDX6)
      FLSTAT(12) = T
      CALL TFLUSH
               ENDIF
            ENDIF
      D7 = 0.0
      GO TO 1010
         ENDIF
      IF ( D9 .LE. 0.0 )  THEN
      A = B
      GO TO 1011
         ENDIF
      M1 = 0
      J = D9
      IF ( D9 .GT. J ) J = J + 1
      IF ( J .LE. D8 ) M1 = 1
      IF ( J .GT. N6 ) THEN
      IF ( IUTY( 176277) .NE. 0 .AND. T .GE. TIMTAC )     THEN
      LIMTAC = LIMTAC - 1
      IF ( LIMTAC .GE. 0 )    THEN
      INDPRT = KENFNT(KOVM10+4) + 2
      D4 = DELTAT * N6
      NDX6 = ILNTAB( 212559)
      BUS5 = TEXVEC(NDX6)
      FLSTAT(13) = D4
      FLSTAT(14) = T
      CALL TFLUSH
               ENDIF
            ENDIF
      J = N6
      M1 = 1
         ENDIF
      N4 = N7 - J
      IF ( N4 .LT.    1324) N4 = N4 +    91
      A = PARSUP(   8994+N4)
      IF ( M1 .EQ. 1 ) GO TO  1011
      N3 = N4 + 1
      IF ( N3 .GE.    1415 ) N3 = N3 -    91
      D5 = B
      IF ( J .GT. 1 ) D5 = PARSUP(   8994+N3)
      A = A - ( J - D7 ) * ( A - D5 )
 1011 XTCS(  45339) = A  ! End supplemen device
C    ------  Begin Type-53 transport delay             I =  29.    Name = "TRVB  "
      B = XTCS(  44673)
      N6 = PARSUP(  10411)
      N7 = IVARB( 194215)
      D7 = PARSUP(  10410)
      D7 = D7 * DLTINV
 1012 D8 = D7 + TENFLZ
      D9 = D7 - TENFLZ
      IF ( D8 .LT. 0.0 )  THEN
      IF ( T .GE. TIMTAC )  THEN
      LIMTAC = LIMTAC - 1
      IF ( LIMTAC .GE. 0 )    THEN
      NDX6 = ILNTAB( 212560)
      INDPRT = KENFNT(KOVM10+4) + 1
      BUS5 = TEXVEC(NDX6)
      FLSTAT(12) = T
      CALL TFLUSH
               ENDIF
            ENDIF
      D7 = 0.0
      GO TO 1012
         ENDIF
      IF ( D9 .LE. 0.0 )  THEN
      A = B
      GO TO 1013
         ENDIF
      M1 = 0
      J = D9
      IF ( D9 .GT. J ) J = J + 1
      IF ( J .LE. D8 ) M1 = 1
      IF ( J .GT. N6 ) THEN
      IF ( IUTY( 176277) .NE. 0 .AND. T .GE. TIMTAC )     THEN
      LIMTAC = LIMTAC - 1
      IF ( LIMTAC .GE. 0 )    THEN
      INDPRT = KENFNT(KOVM10+4) + 2
      D4 = DELTAT * N6
      NDX6 = ILNTAB( 212560)
      BUS5 = TEXVEC(NDX6)
      FLSTAT(13) = D4
      FLSTAT(14) = T
      CALL TFLUSH
               ENDIF
            ENDIF
      J = N6
      M1 = 1
         ENDIF
      N4 = N7 - J
      IF ( N4 .LT.    1419) N4 = N4 +    91
      A = PARSUP(   8994+N4)
      IF ( M1 .EQ. 1 ) GO TO  1013
      N3 = N4 + 1
      IF ( N3 .GE.    1510 ) N3 = N3 -    91
      D5 = B
      IF ( J .GT. 1 ) D5 = PARSUP(   8994+N3)
      A = A - ( J - D7 ) * ( A - D5 )
 1013 XTCS(  45340) = A  ! End supplemen device
C   Following is supplemental variable  I =  30
      QINB    =( -TRSB * TRIB * 0.5  +  TRXB * TRVB * 0.5 ) / QTCR
C    ------  Begin Type-53 transport delay             I =  31.    Name = "TRIC  "
      B = XTCS(  44680)
      N6 = PARSUP(  10508)
      N7 = IVARB( 194250)
      D7 = PARSUP(  10507)
      D7 = D7 * DLTINV
 1014 D8 = D7 + TENFLZ
      D9 = D7 - TENFLZ
      IF ( D8 .LT. 0.0 )  THEN
      IF ( T .GE. TIMTAC )  THEN
      LIMTAC = LIMTAC - 1
      IF ( LIMTAC .GE. 0 )    THEN
      NDX6 = ILNTAB( 212562)
      INDPRT = KENFNT(KOVM10+4) + 1
      BUS5 = TEXVEC(NDX6)
      FLSTAT(12) = T
      CALL TFLUSH
               ENDIF
            ENDIF
      D7 = 0.0
      GO TO 1014
         ENDIF
      IF ( D9 .LE. 0.0 )  THEN
      A = B
      GO TO 1015
         ENDIF
      M1 = 0
      J = D9
      IF ( D9 .GT. J ) J = J + 1
      IF ( J .LE. D8 ) M1 = 1
      IF ( J .GT. N6 ) THEN
      IF ( IUTY( 176277) .NE. 0 .AND. T .GE. TIMTAC )     THEN
      LIMTAC = LIMTAC - 1
      IF ( LIMTAC .GE. 0 )    THEN
      INDPRT = KENFNT(KOVM10+4) + 2
      D4 = DELTAT * N6
      NDX6 = ILNTAB( 212562)
      BUS5 = TEXVEC(NDX6)
      FLSTAT(13) = D4
      FLSTAT(14) = T
      CALL TFLUSH
               ENDIF
            ENDIF
      J = N6
      M1 = 1
         ENDIF
      N4 = N7 - J
      IF ( N4 .LT.    1516) N4 = N4 +    91
      A = PARSUP(   8994+N4)
      IF ( M1 .EQ. 1 ) GO TO  1015
      N3 = N4 + 1
      IF ( N3 .GE.    1607 ) N3 = N3 -    91
      D5 = B
      IF ( J .GT. 1 ) D5 = PARSUP(   8994+N3)
      A = A - ( J - D7 ) * ( A - D5 )
 1015 XTCS(  45342) = A  ! End supplemen device
C    ------  Begin Type-53 transport delay             I =  32.    Name = "TRVC  "
      B = XTCS(  44674)
      N6 = PARSUP(  10603)
      N7 = IVARB( 194255)
      D7 = PARSUP(  10602)
      D7 = D7 * DLTINV
 1016 D8 = D7 + TENFLZ
      D9 = D7 - TENFLZ
      IF ( D8 .LT. 0.0 )  THEN
      IF ( T .GE. TIMTAC )  THEN
      LIMTAC = LIMTAC - 1
      IF ( LIMTAC .GE. 0 )    THEN
      NDX6 = ILNTAB( 212563)
      INDPRT = KENFNT(KOVM10+4) + 1
      BUS5 = TEXVEC(NDX6)
      FLSTAT(12) = T
      CALL TFLUSH
               ENDIF
            ENDIF
      D7 = 0.0
      GO TO 1016
         ENDIF
      IF ( D9 .LE. 0.0 )  THEN
      A = B
      GO TO 1017
         ENDIF
      M1 = 0
      J = D9
      IF ( D9 .GT. J ) J = J + 1
      IF ( J .LE. D8 ) M1 = 1
      IF ( J .GT. N6 ) THEN
      IF ( IUTY( 176277) .NE. 0 .AND. T .GE. TIMTAC )     THEN
      LIMTAC = LIMTAC - 1
      IF ( LIMTAC .GE. 0 )    THEN
      INDPRT = KENFNT(KOVM10+4) + 2
      D4 = DELTAT * N6
      NDX6 = ILNTAB( 212563)
      BUS5 = TEXVEC(NDX6)
      FLSTAT(13) = D4
      FLSTAT(14) = T
      CALL TFLUSH
               ENDIF
            ENDIF
      J = N6
      M1 = 1
         ENDIF
      N4 = N7 - J
      IF ( N4 .LT.    1611) N4 = N4 +    91
      A = PARSUP(   8994+N4)
      IF ( M1 .EQ. 1 ) GO TO  1017
      N3 = N4 + 1
      IF ( N3 .GE.    1702 ) N3 = N3 -    91
      D5 = B
      IF ( J .GT. 1 ) D5 = PARSUP(   8994+N3)
      A = A - ( J - D7 ) * ( A - D5 )
 1017 XTCS(  45343) = A  ! End supplemen device
C   Following is supplemental variable  I =  33
      QINC    =( -TRSC * TRIC * 0.5  +  TRXC * TRVC * 0.5 ) / QTCR
      DO 6631  I=4,     60, 4
 6631 RSBLK(I+     30) = 0.0
      RSBLK(     86) = RSBLK(     86) + PARSUP(  10702)
      RSBLK(     38) = RSBLK(     38) + PARSUP(  10761)
      RSBLK(     46) = RSBLK(     46) + PARSUP(  10820)
      RSBLK(     54) = RSBLK(     54) + PARSUP(  10879)
C    ----  Begin downward on RHS.  NUK =  15
      RSBLK(     34) = RSBLK(     34) * ATCS(  61757)
      RSBLK(     38) = RSBLK(     38) * ATCS(  61762)
      RSBLK(     42) = RSBLK(     42) * ATCS(  61765)
      RSBLK(     46) = RSBLK(     46) * ATCS(  61770)
      RSBLK(     50) = RSBLK(     50) * ATCS(  61773)
      RSBLK(     54) = RSBLK(     54) * ATCS(  61778)
      RSBLK(     58) = RSBLK(     58) * ATCS(  61781)
      RSBLK(     62) = RSBLK(     62) * ATCS(  61783)
      RSBLK(     66) = RSBLK(     66) * ATCS(  61786)
      RSBLK(     70) = RSBLK(     70) * ATCS(  61788)
      RSBLK(     74) = RSBLK(     74) * ATCS(  61791)
      RSBLK(     78) = RSBLK(     78) * ATCS(  61793)
      RSBLK(     82) = RSBLK(     82) * ATCS(  61796)
      RSBLK(     86) = RSBLK(     86) * ATCS(  61799)
      RSBLK(     90) = RSBLK(     90) * ATCS(  61802)
C     ------------   Begin backward on RHS.
      XTCS(  44659) = RSBLK(     90)
      XTCS(  44659) = XTCS(  44659) - ATCS(  61803) * XTCS(  45320)
      XTCS(  44659) = XTCS(  44659) - ATCS(  61804) * XTCS(  45321)
      XTCS(  44659) = XTCS(  44659) - ATCS(  61805) * XTCS(  45322)
      IF ( XTCS(  44659) .LT. RSBLK(     88) )
     1 XTCS(  44659) = RSBLK(     88)
      IF ( XTCS(  44659) .GT. RSBLK(     89) )
     1 XTCS(  44659) = RSBLK(     89)
      XTCS(  44658) = RSBLK(     86)
      XTCS(  44658) = XTCS(  44658) - ATCS(  61800) * XTCS(  44659)
      XTCS(  44658) = XTCS(  44658) - ATCS(  61801) * XTCS(  44671)
      IF ( XTCS(  44658) .LT. RSBLK(     84) )
     1 XTCS(  44658) = RSBLK(     84)
      IF ( XTCS(  44658) .GT. RSBLK(     85) )
     1 XTCS(  44658) = RSBLK(     85)
      XTCS(  44657) = RSBLK(     82)
      XTCS(  44657) = XTCS(  44657) - ATCS(  61797) * XTCS(  44658)
      XTCS(  44657) = XTCS(  44657) - ATCS(  61798) * XTCS(  45313)
      IF ( XTCS(  44657) .LT. RSBLK(     80) )
     1 XTCS(  44657) = RSBLK(     80)
      IF ( XTCS(  44657) .GT. RSBLK(     81) )
     1 XTCS(  44657) = RSBLK(     81)
      XTCS(  44656) = RSBLK(     78)
      XTCS(  44656) = XTCS(  44656) - ATCS(  61794) * XTCS(  44657)
      XTCS(  44656) = XTCS(  44656) - ATCS(  61795) * XTCS(  45338)
      IF ( XTCS(  44656) .LT. RSBLK(     76) )
     1 XTCS(  44656) = RSBLK(     76)
      IF ( XTCS(  44656) .GT. RSBLK(     77) )
     1 XTCS(  44656) = RSBLK(     77)
      XTCS(  44655) = RSBLK(     74)
      XTCS(  44655) = XTCS(  44655) - ATCS(  61792) * XTCS(  44656)
      IF ( XTCS(  44655) .LT. RSBLK(     72) )
     1 XTCS(  44655) = RSBLK(     72)
      IF ( XTCS(  44655) .GT. RSBLK(     73) )
     1 XTCS(  44655) = RSBLK(     73)
      XTCS(  44654) = RSBLK(     70)
      XTCS(  44654) = XTCS(  44654) - ATCS(  61789) * XTCS(  44657)
      XTCS(  44654) = XTCS(  44654) - ATCS(  61790) * XTCS(  45341)
      IF ( XTCS(  44654) .LT. RSBLK(     68) )
     1 XTCS(  44654) = RSBLK(     68)
      IF ( XTCS(  44654) .GT. RSBLK(     69) )
     1 XTCS(  44654) = RSBLK(     69)
      XTCS(  44653) = RSBLK(     66)
      XTCS(  44653) = XTCS(  44653) - ATCS(  61787) * XTCS(  44654)
      IF ( XTCS(  44653) .LT. RSBLK(     64) )
     1 XTCS(  44653) = RSBLK(     64)
      IF ( XTCS(  44653) .GT. RSBLK(     65) )
     1 XTCS(  44653) = RSBLK(     65)
      XTCS(  44652) = RSBLK(     62)
      XTCS(  44652) = XTCS(  44652) - ATCS(  61784) * XTCS(  44657)
      XTCS(  44652) = XTCS(  44652) - ATCS(  61785) * XTCS(  45344)
      IF ( XTCS(  44652) .LT. RSBLK(     60) )
     1 XTCS(  44652) = RSBLK(     60)
      IF ( XTCS(  44652) .GT. RSBLK(     61) )
     1 XTCS(  44652) = RSBLK(     61)
      XTCS(  44651) = RSBLK(     58)
      XTCS(  44651) = XTCS(  44651) - ATCS(  61782) * XTCS(  44652)
      IF ( XTCS(  44651) .LT. RSBLK(     56) )
     1 XTCS(  44651) = RSBLK(     56)
      IF ( XTCS(  44651) .GT. RSBLK(     57) )
     1 XTCS(  44651) = RSBLK(     57)
      XTCS(  44650) = RSBLK(     54)
      XTCS(  44650) = XTCS(  44650) - ATCS(  61779) * XTCS(  45327)
      XTCS(  44650) = XTCS(  44650) - ATCS(  61780) * XTCS(  45356)
      IF ( XTCS(  44650) .LT. RSBLK(     52) )
     1 XTCS(  44650) = RSBLK(     52)
      IF ( XTCS(  44650) .GT. RSBLK(     53) )
     1 XTCS(  44650) = RSBLK(     53)
C    ------  Begin Type-54 binary pulse delay          I =  23.    Name = "FICA1 "
      B = XTCS(  45332)
      D9 = PARSUP(  10117) + XTCS(  44650)
      D = PARSUP(  10116)
      D7 = PARSUP(  10118)
      A = 0.0
      IF ( D7 .NE. -9999. ) THEN
      NDX1 = ILNTAB( 212554)
      IF ( B .GT. FLEVEL .AND. T .LT. TIMTAC )  THEN
      LIMTAC = LIMTAC - 1
      IF ( LIMTAC .GE. 0 )  THEN
      INDPRT = KENFNT(KOVM10+4) + 3
      BUS5 = TEXVEC(NDX1)
      FLSTAT(13) = D9
      FLSTAT(14) = T
      CALL TFLUSH
               ENDIF
            ENDIF
         ELSE
      IF ( B .GT. FLEVEL  .AND. D .EQ. -9999. ) PARSUP(  10116) = T
      IF ( B .LE. FLEVEL  .AND. D .NE. -9999. ) PARSUP(  10118) = T
         ENDIF
      D16 = D9 - TENFLZ
      IF ( T .GE. D + D16 .AND. D .NE. -9999. )  THEN
      IF ( T .LT. D7 + D16 .OR. D7 .EQ. -9999. )    THEN
      A = 1.0
            ELSE
      PARSUP(  10116) = -9999.
      PARSUP(  10118) = -9999.
            ENDIF
         ENDIF
      XTCS(  45334) = A  ! End supplemen device "FICA1 "
C    ------  Begin Type-54 binary pulse delay          I =  24.    Name = "FICA2 "
      B = XTCS(  45333)
      D9 = PARSUP(  10120) + XTCS(  44650)
      D = PARSUP(  10119)
      D7 = PARSUP(  10121)
      A = 0.0
      IF ( D7 .NE. -9999. ) THEN
      NDX1 = ILNTAB( 212555)
      IF ( B .GT. FLEVEL .AND. T .LT. TIMTAC )  THEN
      LIMTAC = LIMTAC - 1
      IF ( LIMTAC .GE. 0 )  THEN
      INDPRT = KENFNT(KOVM10+4) + 3
      BUS5 = TEXVEC(NDX1)
      FLSTAT(13) = D9
      FLSTAT(14) = T
      CALL TFLUSH
               ENDIF
            ENDIF
         ELSE
      IF ( B .GT. FLEVEL  .AND. D .EQ. -9999. ) PARSUP(  10119) = T
      IF ( B .LE. FLEVEL  .AND. D .NE. -9999. ) PARSUP(  10121) = T
         ENDIF
      D16 = D9 - TENFLZ
      IF ( T .GE. D + D16 .AND. D .NE. -9999. )  THEN
      IF ( T .LT. D7 + D16 .OR. D7 .EQ. -9999. )    THEN
      A = 1.0
            ELSE
      PARSUP(  10119) = -9999.
      PARSUP(  10121) = -9999.
            ENDIF
         ENDIF
      XTCS(  45335) = A  ! End supplemen device "FICA2 "
C   Following is supplemental variable  I =  42
      DLC1    =  1.D0 - DELCA/.004167
C    ------  Begin Type-56 nonlinearity (X-Y points)   I =  43.    Name = "REOCA "
      B = XTCS(  45353)
      IF ( B .EQ. STORE1(KSTORE+  43) ) THEN
      IF ( ISTEP .GT. 1 ) GO TO 1018
          ENDIF
      STORE1(KSTORE+  43) = B
      IF ( B .GE. PARSUP(  10849) )  THEN
      A = PARSUP(  10850)
      GO TO 1019
         ENDIF
      IF ( B .LE. PARSUP(  10831) )  THEN
      A = PARSUP(  10832)
      GO TO 1019
         ENDIF
      K = KEEP1(KKESUP+     43)
      IF ( K .EQ. 0 ) K =10831
 1020 IF ( B .GT. PARSUP(K) )  THEN
      K = K + 2
      GO TO 1020
         ENDIF
 1021 IF ( B .LT. PARSUP(K-2) )  THEN
      K = K - 2
      GO TO 1021
         ENDIF
      D9 = PARSUP(K+1) - PARSUP(K-1)
      D9 = D9 / ( PARSUP(K) - PARSUP(K-2) )
      A = PARSUP(K-1) + D9 * ( B - PARSUP(K-2) )
      KEEP1(KKESUP+     43) = K
 1019 XTCS(  45354) = A  ! End supplemen device
 1018 CONTINUE 
      XTCS(  44649) = RSBLK(     50)
      XTCS(  44649) = XTCS(  44649) - ATCS(  61774) * XTCS(  44651)
      XTCS(  44649) = XTCS(  44649) - ATCS(  61775) * XTCS(  44653)
      XTCS(  44649) = XTCS(  44649) - ATCS(  61776) * XTCS(  44655)
      XTCS(  44649) = XTCS(  44649) - ATCS(  61777) * XTCS(  45354)
      IF ( XTCS(  44649) .LT. RSBLK(     48) )
     1 XTCS(  44649) = RSBLK(     48)
      IF ( XTCS(  44649) .GT. RSBLK(     49) )
     1 XTCS(  44649) = RSBLK(     49)
C    ------  Begin Type-56 nonlinearity (X-Y points)   I =  44.    Name = "DELYCC"
      B = XTCS(  44649)
      IF ( B .EQ. STORE1(KSTORE+  44) ) THEN
      IF ( ISTEP .GT. 1 ) GO TO 1022
          ENDIF
      STORE1(KSTORE+  44) = B
      IF ( B .GE. PARSUP(  10872) )  THEN
      A = PARSUP(  10873)
      GO TO 1023
         ENDIF
      IF ( B .LE. PARSUP(  10854) )  THEN
      A = PARSUP(  10855)
      GO TO 1023
         ENDIF
      K = KEEP1(KKESUP+     44)
      IF ( K .EQ. 0 ) K =10854
 1024 IF ( B .GT. PARSUP(K) )  THEN
      K = K + 2
      GO TO 1024
         ENDIF
 1025 IF ( B .LT. PARSUP(K-2) )  THEN
      K = K - 2
      GO TO 1025
         ENDIF
      D9 = PARSUP(K+1) - PARSUP(K-1)
      D9 = D9 / ( PARSUP(K) - PARSUP(K-2) )
      A = PARSUP(K-1) + D9 * ( B - PARSUP(K-2) )
      KEEP1(KKESUP+     44) = K
 1023 XTCS(  45355) = A  ! End supplemen device
 1022 CONTINUE 
C   Following is supplemental variable  I =  45
      DELYC   =DELYCC * 0.004167
      XTCS(  44648) = RSBLK(     46)
      XTCS(  44648) = XTCS(  44648) - ATCS(  61771) * XTCS(  45327)
      XTCS(  44648) = XTCS(  44648) - ATCS(  61772) * XTCS(  45352)
      IF ( XTCS(  44648) .LT. RSBLK(     44) )
     1 XTCS(  44648) = RSBLK(     44)
      IF ( XTCS(  44648) .GT. RSBLK(     45) )
     1 XTCS(  44648) = RSBLK(     45)
C    ------  Begin Type-54 binary pulse delay          I =  19.    Name = "FIBC1 "
      B = XTCS(  45328)
      D9 = PARSUP(  10105) + XTCS(  44648)
      D = PARSUP(  10104)
      D7 = PARSUP(  10106)
      A = 0.0
      IF ( D7 .NE. -9999. ) THEN
      NDX1 = ILNTAB( 212550)
      IF ( B .GT. FLEVEL .AND. T .LT. TIMTAC )  THEN
      LIMTAC = LIMTAC - 1
      IF ( LIMTAC .GE. 0 )  THEN
      INDPRT = KENFNT(KOVM10+4) + 3
      BUS5 = TEXVEC(NDX1)
      FLSTAT(13) = D9
      FLSTAT(14) = T
      CALL TFLUSH
               ENDIF
            ENDIF
         ELSE
      IF ( B .GT. FLEVEL  .AND. D .EQ. -9999. ) PARSUP(  10104) = T
      IF ( B .LE. FLEVEL  .AND. D .NE. -9999. ) PARSUP(  10106) = T
         ENDIF
      D16 = D9 - TENFLZ
      IF ( T .GE. D + D16 .AND. D .NE. -9999. )  THEN
      IF ( T .LT. D7 + D16 .OR. D7 .EQ. -9999. )    THEN
      A = 1.0
            ELSE
      PARSUP(  10104) = -9999.
      PARSUP(  10106) = -9999.
            ENDIF
         ENDIF
      XTCS(  45330) = A  ! End supplemen device "FIBC1 "
C    ------  Begin Type-54 binary pulse delay          I =  20.    Name = "FIBC2 "
      B = XTCS(  45329)
      D9 = PARSUP(  10108) + XTCS(  44648)
      D = PARSUP(  10107)
      D7 = PARSUP(  10109)
      A = 0.0
      IF ( D7 .NE. -9999. ) THEN
      NDX1 = ILNTAB( 212551)
      IF ( B .GT. FLEVEL .AND. T .LT. TIMTAC )  THEN
      LIMTAC = LIMTAC - 1
      IF ( LIMTAC .GE. 0 )  THEN
      INDPRT = KENFNT(KOVM10+4) + 3
      BUS5 = TEXVEC(NDX1)
      FLSTAT(13) = D9
      FLSTAT(14) = T
      CALL TFLUSH
               ENDIF
            ENDIF
         ELSE
      IF ( B .GT. FLEVEL  .AND. D .EQ. -9999. ) PARSUP(  10107) = T
      IF ( B .LE. FLEVEL  .AND. D .NE. -9999. ) PARSUP(  10109) = T
         ENDIF
      D16 = D9 - TENFLZ
      IF ( T .GE. D + D16 .AND. D .NE. -9999. )  THEN
      IF ( T .LT. D7 + D16 .OR. D7 .EQ. -9999. )    THEN
      A = 1.0
            ELSE
      PARSUP(  10107) = -9999.
      PARSUP(  10109) = -9999.
            ENDIF
         ENDIF
      XTCS(  45331) = A  ! End supplemen device "FIBC2 "
C   Following is supplemental variable  I =  38
      DLB1    =  1.D0 - DELBC/.004167
C    ------  Begin Type-56 nonlinearity (X-Y points)   I =  39.    Name = "REOBC "
      B = XTCS(  45349)
      IF ( B .EQ. STORE1(KSTORE+  39) ) THEN
      IF ( ISTEP .GT. 1 ) GO TO 1026
          ENDIF
      STORE1(KSTORE+  39) = B
      IF ( B .GE. PARSUP(  10790) )  THEN
      A = PARSUP(  10791)
      GO TO 1027
         ENDIF
      IF ( B .LE. PARSUP(  10772) )  THEN
      A = PARSUP(  10773)
      GO TO 1027
         ENDIF
      K = KEEP1(KKESUP+     39)
      IF ( K .EQ. 0 ) K =10772
 1028 IF ( B .GT. PARSUP(K) )  THEN
      K = K + 2
      GO TO 1028
         ENDIF
 1029 IF ( B .LT. PARSUP(K-2) )  THEN
      K = K - 2
      GO TO 1029
         ENDIF
      D9 = PARSUP(K+1) - PARSUP(K-1)
      D9 = D9 / ( PARSUP(K) - PARSUP(K-2) )
      A = PARSUP(K-1) + D9 * ( B - PARSUP(K-2) )
      KEEP1(KKESUP+     39) = K
 1027 XTCS(  45350) = A  ! End supplemen device
 1026 CONTINUE 
      XTCS(  44647) = RSBLK(     42)
      XTCS(  44647) = XTCS(  44647) - ATCS(  61766) * XTCS(  44651)
      XTCS(  44647) = XTCS(  44647) - ATCS(  61767) * XTCS(  44653)
      XTCS(  44647) = XTCS(  44647) - ATCS(  61768) * XTCS(  44655)
      XTCS(  44647) = XTCS(  44647) - ATCS(  61769) * XTCS(  45350)
      IF ( XTCS(  44647) .LT. RSBLK(     40) )
     1 XTCS(  44647) = RSBLK(     40)
      IF ( XTCS(  44647) .GT. RSBLK(     41) )
     1 XTCS(  44647) = RSBLK(     41)
C    ------  Begin Type-56 nonlinearity (X-Y points)   I =  40.    Name = "DELYBB"
      B = XTCS(  44647)
      IF ( B .EQ. STORE1(KSTORE+  40) ) THEN
      IF ( ISTEP .GT. 1 ) GO TO 1030
          ENDIF
      STORE1(KSTORE+  40) = B
      IF ( B .GE. PARSUP(  10813) )  THEN
      A = PARSUP(  10814)
      GO TO 1031
         ENDIF
      IF ( B .LE. PARSUP(  10795) )  THEN
      A = PARSUP(  10796)
      GO TO 1031
         ENDIF
      K = KEEP1(KKESUP+     40)
      IF ( K .EQ. 0 ) K =10795
 1032 IF ( B .GT. PARSUP(K) )  THEN
      K = K + 2
      GO TO 1032
         ENDIF
 1033 IF ( B .LT. PARSUP(K-2) )  THEN
      K = K - 2
      GO TO 1033
         ENDIF
      D9 = PARSUP(K+1) - PARSUP(K-1)
      D9 = D9 / ( PARSUP(K) - PARSUP(K-2) )
      A = PARSUP(K-1) + D9 * ( B - PARSUP(K-2) )
      KEEP1(KKESUP+     40) = K
 1031 XTCS(  45351) = A  ! End supplemen device
 1030 CONTINUE 
C   Following is supplemental variable  I =  41
      DELYB   =DELYBB * 0.004167
      XTCS(  44646) = RSBLK(     38)
      XTCS(  44646) = XTCS(  44646) - ATCS(  61763) * XTCS(  45327)
      XTCS(  44646) = XTCS(  44646) - ATCS(  61764) * XTCS(  45348)
      IF ( XTCS(  44646) .LT. RSBLK(     36) )
     1 XTCS(  44646) = RSBLK(     36)
      IF ( XTCS(  44646) .GT. RSBLK(     37) )
     1 XTCS(  44646) = RSBLK(     37)
C    ------  Begin Type-54 binary pulse delay          I =  14.    Name = "FIAB1 "
      B = XTCS(  45323)
      D9 = PARSUP(  10092) + XTCS(  44646)
      D = PARSUP(  10091)
      D7 = PARSUP(  10093)
      A = 0.0
      IF ( D7 .NE. -9999. ) THEN
      NDX1 = ILNTAB( 212545)
      IF ( B .GT. FLEVEL .AND. T .LT. TIMTAC )  THEN
      LIMTAC = LIMTAC - 1
      IF ( LIMTAC .GE. 0 )  THEN
      INDPRT = KENFNT(KOVM10+4) + 3
      BUS5 = TEXVEC(NDX1)
      FLSTAT(13) = D9
      FLSTAT(14) = T
      CALL TFLUSH
               ENDIF
            ENDIF
         ELSE
      IF ( B .GT. FLEVEL  .AND. D .EQ. -9999. ) PARSUP(  10091) = T
      IF ( B .LE. FLEVEL  .AND. D .NE. -9999. ) PARSUP(  10093) = T
         ENDIF
      D16 = D9 - TENFLZ
      IF ( T .GE. D + D16 .AND. D .NE. -9999. )  THEN
      IF ( T .LT. D7 + D16 .OR. D7 .EQ. -9999. )    THEN
      A = 1.0
            ELSE
      PARSUP(  10091) = -9999.
      PARSUP(  10093) = -9999.
            ENDIF
         ENDIF
      XTCS(  45325) = A  ! End supplemen device "FIAB1 "
C    ------  Begin Type-54 binary pulse delay          I =  15.    Name = "FIAB2 "
      B = XTCS(  45324)
      D9 = PARSUP(  10095) + XTCS(  44646)
      D = PARSUP(  10094)
      D7 = PARSUP(  10096)
      A = 0.0
      IF ( D7 .NE. -9999. ) THEN
      NDX1 = ILNTAB( 212546)
      IF ( B .GT. FLEVEL .AND. T .LT. TIMTAC )  THEN
      LIMTAC = LIMTAC - 1
      IF ( LIMTAC .GE. 0 )  THEN
      INDPRT = KENFNT(KOVM10+4) + 3
      BUS5 = TEXVEC(NDX1)
      FLSTAT(13) = D9
      FLSTAT(14) = T
      CALL TFLUSH
               ENDIF
            ENDIF
         ELSE
      IF ( B .GT. FLEVEL  .AND. D .EQ. -9999. ) PARSUP(  10094) = T
      IF ( B .LE. FLEVEL  .AND. D .NE. -9999. ) PARSUP(  10096) = T
         ENDIF
      D16 = D9 - TENFLZ
      IF ( T .GE. D + D16 .AND. D .NE. -9999. )  THEN
      IF ( T .LT. D7 + D16 .OR. D7 .EQ. -9999. )    THEN
      A = 1.0
            ELSE
      PARSUP(  10094) = -9999.
      PARSUP(  10096) = -9999.
            ENDIF
         ENDIF
      XTCS(  45326) = A  ! End supplemen device "FIAB2 "
C   Following is supplemental variable  I =  34
      DLA1    =  1 - DELAB/.004167
C    ------  Begin Type-56 nonlinearity (X-Y points)   I =  35.    Name = "REOAB "
      B = XTCS(  45345)
      IF ( B .EQ. STORE1(KSTORE+  35) ) THEN
      IF ( ISTEP .GT. 1 ) GO TO 1034
          ENDIF
      STORE1(KSTORE+  35) = B
      IF ( B .GE. PARSUP(  10731) )  THEN
      A = PARSUP(  10732)
      GO TO 1035
         ENDIF
      IF ( B .LE. PARSUP(  10713) )  THEN
      A = PARSUP(  10714)
      GO TO 1035
         ENDIF
      K = KEEP1(KKESUP+     35)
      IF ( K .EQ. 0 ) K =10713
 1036 IF ( B .GT. PARSUP(K) )  THEN
      K = K + 2
      GO TO 1036
         ENDIF
 1037 IF ( B .LT. PARSUP(K-2) )  THEN
      K = K - 2
      GO TO 1037
         ENDIF
      D9 = PARSUP(K+1) - PARSUP(K-1)
      D9 = D9 / ( PARSUP(K) - PARSUP(K-2) )
      A = PARSUP(K-1) + D9 * ( B - PARSUP(K-2) )
      KEEP1(KKESUP+     35) = K
 1035 XTCS(  45346) = A  ! End supplemen device
 1034 CONTINUE 
      XTCS(  44645) = RSBLK(     34)
      XTCS(  44645) = XTCS(  44645) - ATCS(  61758) * XTCS(  44651)
      XTCS(  44645) = XTCS(  44645) - ATCS(  61759) * XTCS(  44653)
      XTCS(  44645) = XTCS(  44645) - ATCS(  61760) * XTCS(  44655)
      XTCS(  44645) = XTCS(  44645) - ATCS(  61761) * XTCS(  45346)
      IF ( XTCS(  44645) .LT. RSBLK(     32) )
     1 XTCS(  44645) = RSBLK(     32)
      IF ( XTCS(  44645) .GT. RSBLK(     33) )
     1 XTCS(  44645) = RSBLK(     33)
C    ------  Begin Type-56 nonlinearity (X-Y points)   I =  36.    Name = "DELYAA"
      B = XTCS(  44645)
      IF ( B .EQ. STORE1(KSTORE+  36) ) THEN
      IF ( ISTEP .GT. 1 ) GO TO 1038
          ENDIF
      STORE1(KSTORE+  36) = B
      IF ( B .GE. PARSUP(  10754) )  THEN
      A = PARSUP(  10755)
      GO TO 1039
         ENDIF
      IF ( B .LE. PARSUP(  10736) )  THEN
      A = PARSUP(  10737)
      GO TO 1039
         ENDIF
      K = KEEP1(KKESUP+     36)
      IF ( K .EQ. 0 ) K =10736
 1040 IF ( B .GT. PARSUP(K) )  THEN
      K = K + 2
      GO TO 1040
         ENDIF
 1041 IF ( B .LT. PARSUP(K-2) )  THEN
      K = K - 2
      GO TO 1041
         ENDIF
      D9 = PARSUP(K+1) - PARSUP(K-1)
      D9 = D9 / ( PARSUP(K) - PARSUP(K-2) )
      A = PARSUP(K-1) + D9 * ( B - PARSUP(K-2) )
      KEEP1(KKESUP+     36) = K
 1039 XTCS(  45347) = A  ! End supplemen device
 1038 CONTINUE 
C   Following is supplemental variable  I =  37
      DELYA   =DELYAA * 0.004167
C   ----------  Done backsubstitution on RHS.
      N7 = IVARB( 194170)
      PARSUP(   8994+N7) = XTCS(  44678)
      N7 = N7 + 1
      IF ( N7 .EQ.    1223 ) N7 =  1132
      IVARB( 194170) = N7
      N7 = IVARB( 194175)
      PARSUP(   8994+N7) = XTCS(  44672)
      N7 = N7 + 1
      IF ( N7 .EQ.    1318 ) N7 =  1227
      IVARB( 194175) = N7
      N7 = IVARB( 194210)
      PARSUP(   8994+N7) = XTCS(  44679)
      N7 = N7 + 1
      IF ( N7 .EQ.    1415 ) N7 =  1324
      IVARB( 194210) = N7
      N7 = IVARB( 194215)
      PARSUP(   8994+N7) = XTCS(  44673)
      N7 = N7 + 1
      IF ( N7 .EQ.    1510 ) N7 =  1419
      IVARB( 194215) = N7
      N7 = IVARB( 194250)
      PARSUP(   8994+N7) = XTCS(  44680)
      N7 = N7 + 1
      IF ( N7 .EQ.    1607 ) N7 =  1516
      IVARB( 194250) = N7
      N7 = IVARB( 194255)
      PARSUP(   8994+N7) = XTCS(  44674)
      N7 = N7 + 1
      IF ( N7 .EQ.    1702 ) N7 =  1611
      IVARB( 194255) = N7
C   ----------  Done Type-53 history update.
      PRX = XTCS(  44658)
      PRU = 0.0
      PRU = PRU + XTCS(  44659)
      PRU = PRU - XTCS(  44671)
      IF ( PRX .NE. RSBLK(     84) .AND. PRX .NE. RSBLK(     85) )  THEN
      PARSUP(  10702) = PARSUP(  10704) * PRU - PARSUP(  10705) * PRX
          ELSE
C        Code for dynamic limiter (later)
      CALL MISSING
          ENDIF
      PRX = XTCS(  44646)
      PRU = 0.0
      PRU = PRU + XTCS(  45327)
      PRU = PRU - XTCS(  49626)
      IF ( PRX .NE. RSBLK(     36) .AND. PRX .NE. RSBLK(     37) )  THEN
      PARSUP(  10761) = PARSUP(  10763) * PRU - PARSUP(  10764) * PRX
          ELSE
C        Code for dynamic limiter (later)
      CALL MISSING
          ENDIF
      PRX = XTCS(  44648)
      PRU = 0.0
      PRU = PRU + XTCS(  45327)
      PRU = PRU - XTCS(  49630)
      IF ( PRX .NE. RSBLK(     44) .AND. PRX .NE. RSBLK(     45) )  THEN
      PARSUP(  10820) = PARSUP(  10822) * PRU - PARSUP(  10823) * PRX
          ELSE
C        Code for dynamic limiter (later)
      CALL MISSING
          ENDIF
      PRX = XTCS(  44650)
      PRU = 0.0
      PRU = PRU + XTCS(  45327)
      PRU = PRU - XTCS(  49634)
      IF ( PRX .NE. RSBLK(     52) .AND. PRX .NE. RSBLK(     53) )  THEN
      PARSUP(  10879) = PARSUP(  10881) * PRU - PARSUP(  10882) * PRX
          ELSE
C        Code for dynamic limiter (later)
      CALL MISSING
          ENDIF
      CALL MOVER ( XTCS(  45312), XTCS(  49590),     45 )
