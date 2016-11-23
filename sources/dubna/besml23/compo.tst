      PROGRAM COMP01
      EXTERNAL  INDEX , INDEX1
      CALL INDEX  (INDEX,INDEX1)
      CALL EXIT
      END
      SUBROUTINE INDEX  (ANF,FIN)
      EXTERNAL SUBGEN
      EXTERNAL  DUM01
      DIMENSION  A(1000),B(100,10),C(10,50,2),XX(2)
      DOUBLE PRECISION D1(500),D2(5,100),D3(5,2,5),XD
      DOUBLE PRECISION DABS
      COMPLEX C1(500),C2(10,50),C3(5,5,20),XC
      EQUIVALENCE (A,B,C,D1,D2,D3,C1,C2,C3)
      EQUIVALENCE (XX(1),XD)
      I=0
      KDUMP = 0
C
C   **  CHECK  1  .  CHECKS  INDEXING  FOR  SINGLE  PRECISION  NUMBERS
C
      PRINT 1000
      I=I+1
      C(3,4,2)=1298.5
      B(4,4)=10.E2
      J1=5
      B(10,1)=A(60*J1+4)
      C(J1,4*J1,1)=6.66
      B(20*J1-1,2*J1)=A(39*J1)
      J2=4
      A(555)=B(J1-1,4)*C(3,J2,2*J1-8)
      X=C(J1-2,4,J1-3)-A(533)+B(2*J1-6,J1-1)-A(10)+C(5,20,1)-A(200*J1-1)
     1+B(55,6)-12985.00E2
      EVA1 = ABS(X)
      IF(ABS(X).GT..1) GO TO 2
      PRINT 1001,I
      GO TO 1
    2 PRINT 1002,I,EVA1,EVA1
      KDUMP = KDUMP + 1
    1 I=I+1
C   **   CHECK  2  .  CHECKS  INDEXING  FOR  DOUBLE  PRECISION  NUMBERS
C
      D2(3,15)=7777777777777.7777777777D-11
      J1=6
      D2(J1-1,99)=D1(73)*10.0E1
      D3(5,J1-5,2*J1-8)=D1(80*J1+15)
      D3(J1-5,2,2*J1-10)=D1(10*J1+13)*D1(495)
      XD=D1(3*J1-2)-D2(3,15)*D2(5,99)+D2(J1-1,11*J1+33)-D1(35)
      EVA2 = ABS(XX(1))
      IF(ABS(XX(1)).GT..1) GO TO 4
      PRINT 1001,I
      GO TO 3
    4 PRINT 1002,I,EVA2,EVA2
      KDUMP = KDUMP + 1
    3 I=I+1
C
C   **   CHECK  3  .  CHECKS  INDEXING  FOR  COMPLEX  NUMBERS
C
      C2(4,3)=(1.,200.0E-02)
      J1=3
      C2(2*J1,45)=C1(8*J1)
      C3(J1,J1+1,2*J1-4)=(+0.0,-.1E01)
      C3(5,J1-1,5*J1)=C2(6,45)*C2(4,3)
      XC=C2(4,3)-C2(6,45)+C3(3,4,2)-C1(43)+C1(360)-C1(24)**2
      EVA3 = ABS(REAL(XC))
      IF (ABS(REAL(XC)).GT..1) GO TO 5
      PRINT 1001,I
      GO TO 6
    5 PRINT 1002,I,EVA3,EVA3
      KDUMP = KDUMP + 1
      EVA4 = ABS(AIMAG(XC))
    6 IF (ABS(AIMAG(XC)).GT..1) GO TO 7
      PRINT 1001,I
      GO TO 8
    7 PRINT 1002,I,EVA4,EVA4
      KDUMP = KDUMP + 1
    8 I=I+1
C
C   **   CHECK  4  .  CHECKS  INDEXING  IN  CALL  STATEMENTS
C   **             (FLOATING  POINT  PARAMETERS . )
C
      A(432)=7654.32
      J1=4
      C(J1,3*J1,1) =10.E00
      CALL  SUBGEN(A(100*J1+32),A(30*J1-6),B(50,J1+1))
      X=765432.E-3-2.*A(450)  +A(108*J1)/C(4,12,1)
      EVA5 = ABS(X)
      IF (ABS(X).GT..1) GO TO 9
      PRINT 1001,I
      GO TO 10
    9 PRINT 1002,I,EVA5,EVA5
      KDUMP = KDUMP + 1
   10 I=I+1
C
C   **   CHECK  5  .  CHECKS  INDEXING  IN  CALL  STATEMENTS
C   **          ( DOUBLE  PRECISION  PARAMETERS  )
C
      D1(422)=666.6666666666666666D+00
      J1=7
      D2(5,11*J1+3)  = .3333333333333333333D03
      CALL  DPGEN (D1(60*J1+2),D1(26),D1(400))
      XD=D3(1,2,J1-4)-333.3333333333333333D00
      EVA6 = ABS(XX(1))
      IF(DABS(XX).GT.1.D-18) GO TO 11
      PRINT 1001,I
      GO TO 12
   11 PRINT 1004,I,EVA6,EVA6
      KDUMP = KDUMP + 1
   12 I=I+1
C
C   **   CHECK  6  .  CHECKS  INDEXING  IN  CALL  STATEMENTS
C   **           (  COMPLEX  PARAMETERS  )
C
      C1(200)=(-1.0E1,2.)
      J1=6
      C2(J1+4,J1)  =(+10.,-.002E3)
      CALL  CMPGEN  (C1(100*J1-400),C1(10*J1),C3(3,2,1))
      XC=C1(J1+2)
      EVA7 = ABS(REAL(XC))
      EVA8 = ABS(AIMAG(XC))
      IF (ABS(REAL(XC)).GT..1) GO TO 13
      PRINT 1001,I
      GO TO 14
   13 PRINT 1002,I,EVA7,EVA7
      KDUMP = KDUMP + 1
   14 IF (ABS(AIMAG(XC)).GT..1) GO TO 15
      PRINT 1001,I
      GO TO 16
   15 PRINT 1002,I,EVA8,EVA8
      KDUMP = KDUMP + 1
   16 CONTINUE
C
C   ** SUMMARY AND DUMP
C
      INKR = I-KDUMP
      IF  (KDUMP .EQ. 0) GO TO 500
      IF (INKR .EQ.0) GO TO 501
      PRINT 510,I,INKR,KDUMP
      PRINT 513
      CALL PDUMP (ANF,FIN,3,FIN,SUBGEN,0,SUBGEN,DUM01,3)
      GO TO 502
  500 PRINT 511,I
      GO TO 502
  501 PRINT 512,I
      PRINT 513
      CALL PDUMP (ANF,FIN,3,FIN,SUBGEN,0,SUBGEN,DUM01,3)
  502 CONTINUE
  510 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' CHECKS DONE..................',I2,'   (INCLUDING DUM
     *MY CHECKS)'/
     3            ' CHECKS OKAY..................',I2/
     4            ' CHECKS WRONG.................',I2)
  511 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' ALL ',I2,' CHECKS ARE OKAY')
  512 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' ALL ',I2,' CHECKS ARE WRONG')
  513 FORMAT ('1',34(/),30X,10('*'),'   DUMP IN CASE OF FAILING TEST(S)
     *  ',10('*') )
C  **
C  **
C  **  OUTPUT FORMATS TO PRINT RESULTS OF CHECKS
C  **
C  **
  999 FORMAT ('    I= ',I2,'  DUMMY CHECK')
C  **
 1000 FORMAT (   50X,30H ***************************    /
     *           50X,30H *I-----------------------I*    /
     *        2( 50X, 3H *I  ,23X,              2HI*    /  ),
     *           50X,30H *I COMPILER TEST PROGRAM I*    /
     *        2( 50X, 3H *I  ,23X,              2HI*    /  ),
     *           50X,30H *I-----------------------I*    /
     *           50X,30H ***************************    ///
     $               18H THIS PART CHECKS
     $  'INDEXING '                   ////'    OKAY            *****  ER
     *ROR IN  ******************'/ ' CHECK NO.          *****  CHECK NO.
     *     CALCULATED VALUES             EXPECTED VALUES'/87('-')///)
 1001 FORMAT ('    I= ',I2)
 1002 FORMAT(/20X,'*****     I= ',I2,10X,F10.4,16X,' LE   0.1 '/
     *        41X,O20,' (OCTAL)'/)
 1004 FORMAT(/20X,'*****     I= ',I2,10X,E10.3,16X,' LE 1.D-18'/
     *        41X,O20,' (OCTAL)'/)
      ENTRY INDEX1
      RETURN
      END
      SUBROUTINE  SUBGEN  (A,C,B)
      B=A/C
      RETURN
      END
      SUBROUTINE  DPGEN (D1,D3,D2)
      DOUBLE  PRECISION  D1,D2,D3
      D3=D1-D2
      RETURN
      END
      SUBROUTINE  CMPGEN (C1,C2,C3)
      COMPLEX  C1,C2,C3
      C3=C1+C2
      RETURN
      END
      SUBROUTINE  DUM01
      RETURN
      END
*EXECUTE
      PROGRAM COMPO 2
      CALL ARITHS
      CALL EXIT
      STOP
      END
      SUBROUTINE ARITHS
      EXTERNAL  DUM02
C          **  CHECKS  ADDITION ,SUBTRACTION ,MULTIPLICATION  AND  **
C          **  EXPONENTIATION OF  INTEGER  AND  FLOATING  POINT   **
C          **  NUMBERS .                                           **
      DIMENSION L99999(10),IRRAY2(7,5),IRRAY3(6,6,10),ARRAY(5),
     1ARRAY2(17,11),ARRAY3(5,5,5)
      I=0
      KDUMP = 0
      PRINT 1000
C
C         **  CHECK  1 .  ADDITION AND  SUBTRACTION  OF  SIMPLE   **
C         **  INTEGER  VARIABLES  AND  CONSTANTS .                **
C
      I=I+1
      I0=10+95+5
      I1=I0+90
      I2=I1+I0
      I3=314159265-1314159265
      I4=I3-100
      I5=-I4+I3-110
      M=I5+10
      IF (M.NE.0) GO TO 1
      PRINT 1001,I
      GO TO 2
    1 PRINT 1002,I,M,M
      KDUMP = KDUMP + 1
    2 I=I+1
C
C         **  CHECK  2 .  ADDITION  AND  SUBTRACTION OF  SIMPLE   **
C         **  FLOATING  POINT  VARIABLES  AND  CONSTANTS .        **
C
      X0=1111111.+0.99-1111.99
      X1=90500.25+X0
      X2=X0+X1
      X3= 654.232 - .232
      X4= -15400.E-2+X3
      X5=-X2+X0+500.25
      Y=X5+12.E+05
      IF (Y.GT..1E-4) GO TO 3
      PRINT 1001,I
      GO TO 4
    3 PRINT 1003,I,Y,Y
      KDUMP = KDUMP + 1
    4 I=I+1
C
C   ** CHECK 3
C
      L99999(2)=31416+70000-1416
      IRRAY2(7,4) =555 +L99999(2)
      IRRAY2(1,1) = IRRAY2(7,4)+L99999(2)
      IRRAY3(6,6,6)= 555+L99999(2)-IRRAY2(1,1)
      IRRAY3(1,2,3)=-L99999(2)+IRRAY3(6,6,6)+IRRAY2(1,1)-1555
      IRRAY2(4,5)=-1000-IRRAY3(1,2,3)
      IRRAY3(3,2,1)=IRRAY2(4,5)-IRRAY3(1,2,3)+IRRAY3(6,6,6)+IRRAY2(1,1)
     1-L99999(2)
      M=1555-IRRAY3(3,2,1)
      IF (M.NE.0) GO TO 5
      PRINT 1001,I
      GO TO 6
    5 PRINT 1002,I,M,M
      KDUMP = KDUMP + 1
    6 I=I+1
C
C         **  CHECK  4 .  ADDITION  AND  SUBTRACTION  OF  SUBSCRIPTED  *
C         **  FLOATING  POINT  VARIABLES .(CONSTANT  SUBSCRIPTS)      **
C
      ARRAY(1)=355.55E+02 -5555.0
      ARRAY2(17,11)=-ARRAY(1)-3.E04
      ARRAY3(1,1,1)=
     1ARRAY(1)+ARRAY2(17,11)
      ARRAY2(10,1)=27985432.1 +145679.E-1
      ARRAY3(5,4,3)= ARRAY2(10,1)-ARRAY(1)
      Y=ARRAY2(17,11)+ARRAY3(5,4,3)-2.797E+07+ARRAY(1)-ARRAY3(1,1,1)
      IF (Y.GT..1E-4) GO TO 7
      PRINT 1001,I
      GO TO 8
    7 PRINT 1003,I,Y,Y
      KDUMP = KDUMP + 1
    8 I=I+1
C
C         **  CHECK  5 .  MULTIPLICATION AND DIVISION OF SIMPLE     **
C         **  INTEGER  VARIABLES  AND  CONSTANTS.                   **
C
      J0=2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2*2
     1*2*2
      J1=J0-1
      J2=J0+J1
      J3=J2/2
      J4=J0/J3
      J5=(J4*J0)/J2
      J6=7*10*J4
      J7=13/6
      J8=J6*J7*J4
      J9=-(J2*J4)/(-J3)
      M=J2/J3-J4+J5+J6*J7+J8-J9*140                           -1
      IF (M.NE.0) GO TO 9
      PRINT 1001,I
      GO TO 10
    9 PRINT 1002,I,M,M
      KDUMP = KDUMP + 1
   10 I=I+1
C
C         ** CHECK  6 .  MULTIPLICATION  AND  DIVISION  OF  SIMPLE  **
C         ** FLOATING  POINT  VARIABLES  AND  CONSTANTS .           **
C
      X0 = 2.5* 0.4E01
      X1=X0*11.1
      X2=X0*X1
      X3=X2/4.
      X4=X0+X3/X2
      X5=X0*X0/3.
      X6=X1/X4*X0
      Y=X6*X4/X2-1.0
      IF (Y.GT..1E-4) GO TO 11
      PRINT 1001,I
      GO TO 12
   11 PRINT 1003,I,Y,Y
      KDUMP = KDUMP + 1
   12 I=I+1
C         ** CHECK  7.  ADDITION  AND  SUBTRACTION  OF  SUBSCRIPTED    *
C         ** INTEGER  VARIABLES .(ANY  POSSIBLE  SUBSCRIPTS)           *
C
C
      K=1
      L99999(K+1)=31416+70000-1416
      IRRAY2(6*K+1,5*K-1)=555+L99999(2)
      IRRAY2(K,1)=IRRAY2(7,4)+L99999(2*K)
      K1=7
      IRRAY3(K1-1,2*K1-8,6*K)=IRRAY2(K1,4*K)-IRRAY2(K,K)
      IRRAY3(K,2*K,K1-4)=-L99999(K1-5)+IRRAY3(6,6,6)+IRRAY2(1,1)-1555
      IRRAY2(4,5)=-1000-IRRAY3(K,K+1,K+2)
      IRRAY3(K1-4,2*K,K)=IRRAY2(K1-3,5*K)-IRRAY3(1,2,3)-IRRAY3(6,6,6)+
     1IRRAY2(1,K)-L99999(2*K)
      M=-IRRAY3(3,2,1)+201555
      IF (M.NE.0) GO TO 13
      PRINT 1001,I
      GO TO 14
   13 PRINT 1002,I,M,M
      KDUMP = KDUMP + 1
   14 I=I+1
C
C         ** CHECK  8 .  ADDITION  AND  SUBTRACTION  OF  SUBSCRIPTED   *
C         ** FLOATING  POINT  VARIABLES .(ANY POSSIBLE SUBSCRIPTS)     *
C
      K=1
      K1=10
      ARRAY(K)=355.55E2-5555.
      ARRAY2(K1+7,2*K1-9)=-3.0E+4-ARRAY(K)
      ARRAY3(K,1,1)=ARRAY(1)+ARRAY2(2*K1-3,11)
      ARRAY2(K1,K)=27985432.1+145679.E-01
      ARRAY3(5*K,K1-6,2*K+1)=ARRAY2(10,1)-ARRAY(1)
      Y=-2797.E4+ARRAY2(17,11)+ARRAY3(K1-5,4*K,3)  +60000 .
      IF (Y.GT..1E-4) GO TO 15
      PRINT 1001,I
      GO TO 16
   15 PRINT 1003,I,Y,Y
      KDUMP = KDUMP + 1
   16 I=I+1
C
C         ** CHECK  9 . MULTIPLICATION AND DIVISION OF SUBSCRIPTED     *
C         ** INTEGER  VARIABLES .(CONSTANT  SUBSCRIPTS)                *
C
      L99999(6)=450*2
      IRRAY2(5,5)=L99999(6)*2+200
      IRRAY3(2,3,10)=(L99999(6)*IRRAY2(5,5))/110000
      IRRAY2(1,2)=(IRRAY2(5,5)/IRRAY3(2,3,10))*L99999(6)
      IRRAY3(1,1,1)=(((100*IRRAY2(1,2))/IRRAY2(5,5))*4)/IRRAY3(2,3,10)
      M=IRRAY2(5,5)/L99999(6)-(IRRAY3(2,3,10)*IRRAY3(1,1,1))/2+11246
      IF (M.NE.0) GO TO 17
      PRINT 1001,I
      GO TO 18
   17 PRINT 1002,I,M,M
      KDUMP = KDUMP + 1
   18 I=I+1
C
C         ** CHECK 10 . MULTIPLICATION  AND  DIVISION  OF  SUBSCRIPTED *
C         ** FLOATING  POINT VARIABLES. (CONSTANT  SUBSCRIPTS)         *
C
      ARRAY(4)= 950.0/500.E-2
      ARRAY2(5,5)= ARRAY(4)* 70.5
      ARRAY3(2,4,3)= ARRAY2(5,5)*1.E+04/5.
      ARRAY2 (17,10)=ARRAY3(2,4,3)*10./ARRAY2(5,5)
      ARRAY(1)=ARRAY(4)*ARRAY2(17,10)/38.E+02
      ARRAY3(5,5,5)=ARRAY(1)*ARRAY2(17,10)/5.0E+02*.0004
      Y=ARRAY3(5,5,5)/16. -1.E0
      IF (Y.GT..1E-4) GO TO 19
      PRINT 1001,I
      GO TO 20
   19 PRINT 1003,I,Y,Y
      KDUMP = KDUMP + 1
   20 I=I+1
C
C         ** CHECK  11. MULTIPLICATION  AND  DIVISION  OF SUBSCRIPTED  *
C         ** INTEGER  VARIABLES .( ANY SORT  OF  SUBSCRIPTS.)          *
      K=1
      K1=8
      L99999(K)=10
      IRRAY2(K1-1,4*K)=L99999(K1-7)*9
      IRRAY2(4*K+2,2)=(L99999(6*K-5)-IRRAY2(K+6,K1-4))/8+12
      IRRAY3(K1-2,3*K,K1)=IRRAY2(6*K,2)*L99999(2*K-1)+IRRAY2(7,K1-4)/85
      M=(IRRAY2(7,K1-4)*IRRAY2(7*K-1,K1-6))/IRRAY3(K1-2,3,K1)+IRRAY2
     1(6,2*K)/L99999(K)-8
      IF (M.NE.0) GO TO 21
      PRINT 1001,I
      GO TO 22
   21 PRINT 1002,I,M,M
      KDUMP = KDUMP + 1
   22 I=I+1
C
C   **CHECK 12. MULTIPLICATION  AND  DIVISION  OF  SUBSCRIPTED         *
C   **  FLOATING  POINT  VARIABLES ( ANY  SORT  OF  SUBSCRIPTS).       *
C
      K=2
      K1=3
      K2=4
      ARRAY(K2-1)=298.5E+1
      ARRAY2(3*K2,K1+5)=ARRAY(K1)*3.+.145E03
      ARRAY2(K-1,1)=(ARRAY2(5*K1-3,2*K2)/ARRAY(K1))*10.
      ARRAY3(K,K1,K2)=(ARRAY2(4*K-7,K1-2)*1.E+02+605142.379E-02)/ARRAY2
     1(12,2*K2)
      X=ARRAY2(12,2*K2)/ARRAY3(K2-2,K+1,2*K1-2)-9100.00
      X=-X
      IF (X.GT.0.1E-04) GO TO 23
      PRINT 1001,I
      GO TO 24
   23 PRINT 1003,I,X,X
      KDUMP = KDUMP + 1
   24 I=I+1
C
C   **  CHECK  13 .EXPONENTIATION.                                    **
C   **       BASE = SIMPLE  INTEGER  CONSTANT  OR  VARIABLE           **
C   **       EXPONENT = SIMPLE  INTEGER  CONSTANT  OR  VARIABLE       **
C
      J1=2**4
      J2=J1**1
      J3=2**J1
      J4=2
      J5=(J4**J2)**2
      J6=1
      J7=J4**J1**J6**J4
      M=J7/J5-J1/J2+J3/(2**16) -1
      IF (M.NE.0) GO TO 25
      PRINT 1001,I
      GO TO 26
   25 PRINT 1002,I,M,M
      KDUMP = KDUMP + 1
   26 I=I+1
C   **  CHECK  14 . EXPONENTIATION
C   **       BASE = SIMPLE  FLOATING  POINT  CONSTANT  OR  VARIABLE   **
C   **       EXPONENT = SIMPLE  INTEGER  CONSTANT  OR  VARIABLE       **
C
      X1=0.5**2
      X2=X1**3
      K=3
      X3=90.E+00**K
      X4=9.E+1
      X5=X4**K
      X6=X4**2**2
      X=90.*X5-X6+(X4/3.)**3-27.E+03
      IF (X.GT..00001) GO TO 27
      PRINT 1001,I
      GO TO 28
   27 PRINT 1003,I,X,X
      KDUMP = KDUMP + 1
   28 I=I+1
C
C   **  CHECK 15 EXPONENTIATION
C   **        BASE = SIMPLE  FLOATING  POINT  CONSTANT  OR  VARIABLE  **
C   **        EXPONENT =SIMPLE FLOATING POINT CONSTANT OR VARIABLE    **
C
      X1=0.5**2.0E+00
      X2=X1**3.
      Y=3.
      X3=90.E+00**Y
      X4=9.E01
      X5=X4**Y
      X6=X4**2.0**.2E+01
      X=90.*X5-X6+(X4/Y)**3.0-27000.+X2-X1**Y
      X=-X
      IF (X.GT.1.E-5) GO TO 29
      PRINT 1001,I
      GO TO 30
   29 PRINT 1003,I,X,X
      KDUMP = KDUMP + 1
   30 I=I+1
C
C   **  CHECK 16. EXPONENTIATION                                       *
C   **       BASE=SUBSCRIPTED  INTEGER  VARIABLE                       *
C   **       EXPONENT = INTEGER  CONSTANT  OR  VARIABLE  OR  EXPRESSION*
C
      L99999(2)=50
      K=4
      IRRAY2 (7,K-3)= L99999(K-2)**2
      IRRAY2(2*K-7,K)= IRRAY2(K+3,1)**(K-2)
      IRRAY3(K+2,4,K-2)=-(L99999(2*K-6)**(3*K-10))*IRRAY2(2*K-1,1)
      M=(L99999(2*K-6)*L99999(2))/IRRAY2(K+3,1)-1+IRRAY2(1,K)+IRRAY
     1  3(2*K-2,K,2)
      IF (M.NE.0) GO TO 31
      PRINT 1001,I
      GO TO 32
   31 PRINT 1002,I,M,M
      KDUMP = KDUMP + 1
   32 I=I+1
C
C   **  CHECK  17. EXPONENTIATION
C   **      BASE = ANY  INTEGER  VARIABLE                             **
C   **      EXPONENT  = ANY  INTEGER  VARIABLE                        **
C
      K=4
      K1=3
      L99999(2)=50
      IRRAY2(7,K-3)=L99999(K1-1)**(K-K1+1)
      IRRAY2(K1-1,2*K-6)=  2**(L99999(K1-1)-46)
      IRRAY3(6,2,K1) = (K1-1)**IRRAY2(K-2,2)
      L99999(K+1)=2
      IRRAY3(K+2,K1-1,K)=  L99999(5)**IRRAY2(2*K-6,K1-1)
      M=(L99999(K+1)*IRRAY3(6,K-2,K))/(2*IRRAY3(6,2,K1))+IRRAY2(7,K1-2)
     1-2500 -IRRAY2(K-2,K1-1)/16
      IF (M.NE.0) GO TO 33
      PRINT 1001,I
      GO TO 34
   33 PRINT 1002,I,M,M
      KDUMP = KDUMP + 1
   34 I=I+1
C
C   **  CHECK   18 .  EXPONENTIATION
C   **       BASE = SUBSCRIPTED  FLOATING  POINT  VARIABLE             *
C   **        EXPONENT  =INTEGER  CONSTANT  OR  VARIABLE  OR  EXPRESSION
C
      K=3
      ARRAY(5)= 10.0
      ARRAY(4)= ARRAY(K+2)**2
      ARRAY2(5*K+1,2*K)= ARRAY(2*K-2)**(K-4)
      ARRAY3(5,K+1,K)=ARRAY(2*K-1)**K
      Y= ARRAY3(K+2,4,K)/(ARRAY(K+2)*ARRAY(2*K-2)) - ARRAY2(4*K+4,6)
     1*ARRAY(4)
      IF (Y.GT.0.00001) GO TO 35
      PRINT 1001,I
      GO TO 36
   35 PRINT 1003,I,Y,Y
      KDUMP = KDUMP + 1
   36 I=I+1
C
C   **  CHECK  19 .  EXPONENTIATION                                   **
C   **      BASE  =  ANY  FLOATING  POINT  VARIABLE                   **
C   **      EXPONENT  =  ANY  INTEGER  VARIABLE                       **
C
      K=2
      L99999(K+1) = 3
      ARRAY(5)= 10.E+00 ** L99999(3)
      ARRAY(K)=  ARRAY(K+3)**(-L99999(2*K-1))
      ARRAY3(K+2,K,K-1)=  1./ARRAY(2)**1
      X=  ARRAY(2*K+1)* ARRAY(2)*ARRAY3(K+2,2,1)/ARRAY(5) -1.
      IF (X.GT.10.0E-6) GO TO 37
      PRINT 1001,I
      GO TO 38
   37 PRINT 1003,I,X,X
      KDUMP = KDUMP + 1
   38 I=I+1
C
C   **  CHECK  20 .  EXPONENTIATION                                   **
C   **       BASE = SUBSCRIPTED  FLOATING  POINT  VARIABLE             *
C   **       EXPONENT  =  FLOATING  POINT  EXPRESSIONS  OR  CONSTANTS **
C
      A=5.
      ARRAY(4)= 2.**A
      K=4
      ARRAY(K-2) =  ARRAY(K)**(1.0E+00/A)
      ARRAY2(4*K,K+1) = A** ARRAY(2)
      ARRAY2(2*K+2,K-1) =  ARRAY2(5*K-4,5)** ARRAY (2*K-6)
      Y =   ARRAY2( 10,2*K-5) **( 1./ ARRAY2(3*K+4,K+1)) - ARRAY(K-2)
     1+0.7063
      IF (Y.GT..1E-4) GO TO 39
      PRINT 1001,I
      GO TO 40
   39 PRINT 1003,I,Y,Y
      KDUMP = KDUMP + 1
   40 CONTINUE
C
C   ** SUMMARY AND DUMP
C
      INKR = I-KDUMP
      IF  (KDUMP .EQ. 0) GO TO 500
      IF (INKR .EQ.0) GO TO 501
      PRINT 510,I,INKR,KDUMP
      PRINT 513
      GO TO 502
  500 PRINT 511,I
      GO TO 502
  501 PRINT 512,I
      PRINT 513
  502 CONTINUE
      IIIII=1
  510 FORMAT (////' **********  SUMMARY **********'/
     1            '            -------'//
     2            ' CHECKS DONE..................',I2,' (INCLUDING DUM
     *MY CHECKS)'/
     3            ' CHECKS OKAY.................',I2/
     4            ' CHECKS WRONG.................',I2)
  511 FORMAT (////'********** SUMMARY **********'/
     1            '                  -----------'//
     2            ' ALL ',I2,' CHECKS ARE OKAY')
  512 FORMAT (////' ********** SUMMARY **********'/
     1            '            ----------'//
     2            ' ALL ',I2,' CHECKS ARE WRONG')
  513 FORMAT ('1',34(/),30X,10('*'),'  DUMP IN CASE OF FAILING TEST(S)
     *  ',10('*') )
C  **
C  **
C  **  OUTPUT FORMATS TO PRINT RESULTS OF CHECKS
C  **
C  **
  999 FORMAT ('    I= ',I2,' DUMMY CHECK')
C  **
 1000 FORMAT (   50X,30H ***************************    /
     *           50X,30H *I-----------------------I*    /
     *        2( 50X, 3H *I  ,23X,              2HI*    /  ),
     *           50X,30H *I COMPILER TEST PROGRAM I*    /
     *        2( 50X, 3H *I  ,23X,              2HI*    /  ),
     *           50X,30H *I-----------------------I*    /
     *           50X,30H ***************************    ///
     $               18H THIS PART CHECKS
     $'SINGLE PRECISION ARITHMETIC'   ////'   OKAY        ***** ER*****
     *ROR IN ******************'/ ' CHECK NO.          ***** CHECK NO.
     *     CALCULATED VALUES        EXPECTED VALUES'/87('-')///)
 1001 FORMAT ('    I= ',I2)
 1002 FORMAT(/20X,'*****     I=',I2,10X,E10.3,16X,'   0.0   '/
     *         41X,O20,' (OCTAL)'/)
 1003 FORMAT(/20X,'*****     I= ',I2,10X,E10.3,16X,' LE 0.1E-4'/
     *          41X,O20,' (OCTAL)'/)
      ENTRY  ARITH1
      RETURN
      END
      SUBROUTINE  DUM02
      RETURN
      END
*EXECUTE
      PROGRAM COMP04
      EXTERNAL ARITHC,ARITH3
      CALL ARITHC (ARITHC,ARITH3)
      CALL  EXIT
      END
      SUBROUTINE ARITHC (ANF,FIN)
      EXTERNAL  DUM04
      DIMENSION  KK(5)
      COMPLEX A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,X
      COMPLEX  X1,X2,X3
     1 ,C1(10),C2(5,4),C3(3,3,3)
C
C   **  CHECKS  ADDITION  ,  SUBTRACTION  ,  MULTIPLICATION  ,  DIVISION
C   **     AND  EXPONENTIATION  OF  COMPLEX  AND  REAL  NUMBERS  .
C
C   **  ODD CHECK NUMBERS TEST REAL PARTS
C   ** EVEN CHECK NUMBERS TEST IMAG PARTS
C
C
      I=0
      KDUMP = 0
      PRINT 1000
C
C   **   CHECK 1/2 .  ADDITION  AND  SUBTRACTION  OF  COMPLEX  AND  REAL
C   **        CONSTANTS  AND  VARIABLES  .
C
      I=I+1
      A1=(548.0E-02 , 12.)
      A2=(0.52,0.8 E1)  +  A1
      A3 = -(0.0,14000000000.0E-09)  +A2
      A4=A2+A3
      A5=-A4
      A6=A5+A3
      A7=A6-1.4E1
      A8=-  .52
      A9=A1-A8
      A10=  -A9-A5
      X=A2-(6.,2.E+1)+A3+(-6.,-.6E1) +A5 +(1.2E01,26.0E00)
     1 -A6-(+600.E-2,+20.)+A7-(-20.0,-2.E1)+A9+(-6.0,-12.)
     2  -A10-(-6.,-14.)
      EVA1 = ABS(REAL(X))
      EVA2 = ABS(AIMAG(X))
      IF (ABS(REAL(X))  .GT.  .1) GO TO 1
      PRINT 1001,I
      GO TO  2
    1 PRINT 1003,I,EVA1
      KDUMP = KDUMP + 1
    2 I=I+1
      IF (ABS(AIMAG(X))  .GT.  .1) GO TO 3
      PRINT 1002,I
      GO TO  4
    3 PRINT 1004,I,EVA2
      KDUMP = KDUMP + 1
C
C   **   CHECK  3/4   ADDITION  AND  SUBTRACTION  OF  SUBSCRIPTED
C   **     COMPLEX  NUMBERS  .  (CONSTANT  SUBSCRIPTS  )
C
    4 I = I+1
      C1(6)=298.E-2
      C2(4,4)=C1(6)+(+.02, -1.E1)
      C3(1,1,1)=C1(6)-C2(4,4)
      C2(3,2)=(-1.E6,+1.)-C2(4,4) +C3(1,1,1)
      C3(1,2,3)=
     1  C2(4,4)-C2(3,2)+C3(1,1,1)
      X=(-298.E-2, 0.0)+C1(6)-C2(4,4)-(-3.E00,100.0E-01)
     1 +C3(1,1,1)+(2.E-2,-10.)+C3(1,2,3)-(+1000006.,-21.0)
      EVA1 = ABS(REAL(X))
      EVA2 = ABS(AIMAG(X))
      IF (ABS(REAL(X))  .GT.  .1) GO TO 5
      PRINT 1001,I
      GO TO  6
    5 PRINT 1003,I,EVA1
      KDUMP = KDUMP + 1
    6 I=I+1
      IF (ABS(AIMAG(X))  .GT.  .1) GO TO 7
      PRINT 1002,I
      GO TO  8
    7 PRINT 1004,I,EVA2
      KDUMP = KDUMP + 1
C
C   **   CHECK  5/6   ADDITION  AND  SUBTRACTION  OF  SUBSCRIPTED
C   **      COMPLEX  NUMBERS  (ANY  POSSIBLE  SUBSCRIPTS . )
C
    8 I = I+1
      J1=6
      J2=1
      C1(J1)=2.98
      C2(J1-2,4*J2)=C1(6)-(-.02,10.)
      C3(1,J2,J1-5)=C1(5*J2+1)-C2(4,4)
      C2(J1-3,2*J2)=-C2(4,J1-2)+C3(1,6*J1-35,J2)
     1  -(1000000.0,-.1E01)
      C3(J2,2*J2,J1-3)=C2(4*J2,4)-C2(J1-3,2*J2)
     1  +C3(J2,1,1)
      X=(-298.E-2, 0.0)+C1(6)-C2(4,4)-(-3.E00,100.0E-01)
      EVA1 = ABS(REAL(X))
      EVA2 = ABS(AIMAG(X))
     1 +C3(1,1,1)+(2.E-2,-10.)+C3(1,2,3)-(+1000006.,-21.0)
      IF (ABS(REAL(X))  .GT.  .1) GO TO 9
      PRINT 1001,I
      GO TO 10
    9 PRINT 1003,I,EVA1
      KDUMP = KDUMP + 1
   10 I=I+1
      IF (ABS(AIMAG(X))  .GT.  .1) GO TO 11
      PRINT 1002,I
      GO TO 12
   11 PRINT 1004,I,EVA2
      KDUMP = KDUMP + 1
C
C   **   CHECK  7/8 MULTIPLICATION  AND  DIVISION  OF  COMPLEX
C   **            CONSTANTS  AND  VARIABLES   .
C
   12 I = I+1
      A1=(+2.0,-.4E+01)
      A2=(2.0,-.4E+1)*(5.,4.)
      A3=A1*(500.0E-2,4.0)
      A4=(+26.,-12.0)/(1.,2.0)
      A5=A2/(10.0E-01,.2E1)
      A6=(2.,-4.)/A2
      A7=A1/A2
      A8=A1*A2
      A9=(A6*A2)/A1
      A10=((1.,+.0)/((A6*A2)/A1))*A4
      X=A2+A3-2.*(26.,-1.2E1)+A4+A5-2.*(+.4,-12.8)+
     1 A6+A7+A8+A9-(5.,-128.0)+(-.12195122,0.09756)*2.E00
      EVA1 = ABS(REAL(X))
      EVA2 = ABS(AIMAG(X))
      IF (ABS(REAL(X))  .GT.  .1) GO TO 13
      PRINT 1001,I
      GO TO 14
   13 PRINT 1003,I,EVA1
      KDUMP = KDUMP + 1
   14 I=I+1
      IF (ABS(AIMAG(X))  .GT.  .1) GO TO 15
      PRINT 1002,I
      GO TO 16
   15 PRINT 1004,I,EVA2
      KDUMP = KDUMP + 1
C
C   **   CHECK  9/10  MULTIPLICATION  AND  DIVISION  OF  COMPLEX
C   **   SUSCRIPTED  VARIABLES   (CONSTANT  SUSCRIPTS  )
C
   16 I = I+1
      C1(1)=(+0.,-2.)*4.
      C1(5)=(-1.,+3.0)*(-2.,+2.)
      C2(3,2)=C1(1)*(1.,-1.)
      C2(1,4)=C2(3,2)*C1(5)
      C2(2,2)=(3.,-4.0E00)/(-.2E1,+2.0)
      C3(1,2,3)=C1(5)/(1.,1.)
      C3(2,2,2)=C2(3,2)/C3(1,2,3)
      C3(3,1,3)=((C1(5)*C2(3,2))/(C3(1,2,3)*C3(2,2,2)   ))/C1(5)
      X=C1(5)+(4.,8.)+C2(3,2)+(+.8E1,80.E-01)+C2(1,4)+ (32.0,-96.)
     1 +C2(2,2)-(-1.75,25.0E-2)+C3(1,2,3)+(+6.,+200000000.E-8)
     2  +C3(2,2,2)+C3(3,1,3)-(2.6,0.8)
      EVA1 = ABS(REAL(X))
      EVA2 = ABS(AIMAG(X))
      IF (ABS(REAL(X))  .GT.  .1) GO TO 17
      PRINT 1001,I
      GO TO 18
   17 PRINT 1003,I,EVA1
      KDUMP = KDUMP + 1
   18 I=I+1
      IF (ABS(AIMAG(X))  .GT.  .1) GO TO 19
      PRINT 1002,I
      GO TO 20
   19 PRINT 1004,I,EVA2
      KDUMP = KDUMP + 1
C
C   **  CHECK  11/12 MULTIPLICATION  AND  DIVISION  OF  COMPLEX        *
C  **      SUBSCRIPTED  VARIABLES  (  ANY  SORT  OF  SUBSCRIPTS  . )  **
   20 I = I+1
      J1=1
      J2=2
      C1(J1)=(0.0,-2.)*4.
      C1(J2+3)=(-2.,2.0)*(-1.,+3.0)
      C2(3*J1,J2)=C1(J2-1)*(1.,-100.E-2)
      C2(1,2*J2)=C2(3*J2-3,J2)*C1(2*J2+1)
      C2(J2,2)=(3.0,-4.0E00)/(-2.,.2E1)
      C3(J1,J2,J2+1)=C1(5)/(1.0,+1.)
      C3(2,J2,J1+1)=C2(J1+2,J2)/C3(J1,J2,3)
      C3(2*J2-1,J2-1,J2+1)=((C1(J2+3)*C2(3*J1,J2))/ (C3(2*J2-3,2,J1+2)
     1 *C3(2,J2,J2)))/C1(2*J2+1)
      X=C1(5)+(4.,8.)+C2(3,2)+(+.8E1,80.E-01)+C2(1,4)+ (32.0,-96.)
     1 +C2(2,2)-(-1.75,25.0E-2)+C3(1,2,3)+(+6.,+200000000.E-8)
     2  +C3(2,2,2)+C3(3,1,3)-(2.6,0.8)
      EVA1 = ABS(REAL(X))
      EVA2 = ABS(AIMAG(X))
      IF (ABS(REAL(X))  .GT.  .1) GO TO 21
      PRINT 1001,I
      GO TO 22
   21 PRINT 1003,I,EVA1
      KDUMP = KDUMP + 1
   22 I=I+1
      IF (ABS(AIMAG(X))  .GT.  .1) GO TO 23
      PRINT 1002,I
      GO TO 24
   23 PRINT 1004,I,EVA2
      KDUMP = KDUMP + 1
C
C   **   CHECK  13/14 EXPONENTIATION  .                                *
C   **         BASE  .  COMPLEX  CONSTANT  OR  VARIABLE
C   **         EXPONENT  .  INTEGER  CONSTANT  OR  VARIABLE            *
C
   24 I = I+1
      J1=-4
      A1=(1.732,-1.E0)**2
      A2=A1**3
      A3=A1**J1
      X=(+2.0,-3464.E-3)-A1-A2+(-64. ,.0)+A3+(+.195E-2,.0033)
      EVA1 = ABS(REAL(X))
      EVA2 = ABS(AIMAG(X))
      IF (ABS(REAL(X))  .GT.  .1) GO TO 25
      PRINT 1001,I
      GO TO 26
   25 PRINT 1003,I,EVA1
      KDUMP = KDUMP + 1
   26 I=I+1
      IF (ABS(AIMAG(X))  .GT.  .1) GO TO 27
      PRINT 1002,I
      GO TO 28
   27 PRINT 1004,I,EVA2
      KDUMP = KDUMP + 1
C
C   **   CHECK  15/16 EXPONENTIATION  .                                *
C  **         BASE  .  COMPLEX  CONSTANT  OR  VARIABLE                 *
C   **         EXPONENT  .  ANY  INTEGER  EXPRESSION                   *
C
   28 I = I+1
      J1=-4
      J2=5
      A1=(+1.73205,-1.0)**((-3*J1)/6)
      A2=A1**(J1*J2+23)
      A3=A1**(-4)
      KK(5)=6
      A4=A1**(KK(J2)+2*J1-2)
      X=
     $( 2.,-3.464)+(-64.,0.)-(A1+A2)+A3+A4-2.*(+0.00195,+0.0033)
      EVA1 = ABS(REAL(X))
      EVA2 = ABS(AIMAG(X))
      IF (ABS(REAL(X))  .GT.  .1) GO TO 29
      PRINT 1001,I
      GO TO 30
   29 PRINT 1003,I,EVA1
      KDUMP = KDUMP + 1
   30 I=I+1
      IF (ABS(AIMAG(X))  .GT.  .1) GO TO 31
      PRINT 1002,I
      GO TO 32
   31 PRINT 1004,I,EVA2
      KDUMP = KDUMP + 1
C
C   **   CHECK  17/18 EXPONENTIATION                                   *
C   **         BASE  .  SUBSCRIPTED  VARIABLE  (CONSTANT  SUBSCRIPTS ) *
C   **         EXPONENT  .  INTEGER  VARIABLE  OR  EXPRESSION          *
C
   32 I = I+1
      J1=1
      J2=2
      J3=-3
      KK(2)=4
      C1(5)=(+1.73205,-1.)**(-J3-1)
      C2(3,2)=(-C1(5))**(-J3)
      C2(1,3)=C1(5)**((J3*J2)/(-(J2*2+J1+1)))
      C3(1,2,3)=C2(1,3)**(-KK(2*J3+8))
      C3(2,2,1)=C2(    1,3)**(-(J2**J2))
      X=(2.,-3.464)-C1(5)+(64.,0.E-1)-C2(3,2)+C2(1,3)-C1(5)
     $  +C3(1,2,3)+C3(2,2,1)-2.*(195.E-5,3.3E-03)
      EVA1 = ABS(REAL(X))
      EVA2 = ABS(AIMAG(X))
      IF (ABS(REAL(X))  .GT.  .1) GO TO 33
      PRINT 1001,I
      GO TO 34
   33 PRINT 1003,I,EVA1
      KDUMP = KDUMP + 1
   34 I=I+1
      IF (ABS(AIMAG(X))  .GT.  .1) GO TO 35
      PRINT 1002,I
      GO TO 36
   35 PRINT 1004,I,EVA2
      KDUMP = KDUMP + 1
C
C   **   CHECK  19/20  EXPONENTIATION ..                               *
C   **         BASE  .  SUBSCRIPTED  VARIABL(ANY SORT OF SUBSCRIPTS )  *
C   **         EXPONENT  .  INTEGER  VARIABLE  OR  EXPRESSION          *
C
   36 I = I+1
      J1=1
      J2=2
      J3=-3
      KK(J2)=-J3+1
      C1(2*J2+1)=(1.73205,-10.E-1)**J2
      C2(J2+1,J2)=(-C1(5))**(J2+J1)
      C2(J1,3)=C1(2*J3+11)**(J1*1)
      C3(J1,J2,2*J2-1)=C2(J1,J1+2)**(-1+J3)
      C3(2,J2,J1)=C2(J1,5*J2-7)**((J3*KK(3*J3+11))/3)
      X=(2.,-3.464)-C1(5)+(64.,0.E-1)-C2(3,2)+C2(1,3)-C1(5)
     $  +C3(1,2,3)+C3(2,2,1)-2.*(195.E-5,3.3E-03)
      EVA1 = ABS(REAL(X))
      EVA2 = ABS(AIMAG(X))
      IF (ABS(REAL(X))  .GT.  .1) GO TO 37
      PRINT 1001,I
      GO TO 38
   37 PRINT 1003,I,EVA1
      KDUMP=KDUMP+1
   38 I=I+1
      IF (ABS(AIMAG(X))  .GT.  .1) GO TO 39
      PRINT 1002,I
      GO TO 40
   39 PRINT 1004,I,EVA2
      KDUMP=KDUMP+1
   40 CONTINUE
C
C   ** SUMMARY AND DUMP
C
      INKR = I-KDUMP
      IF  (KDUMP .EQ. 0) GO TO 500
      IF (INKR .EQ.0) GO TO 501
      PRINT 510,I,INKR,KDUMP
      PRINT 513
      CALL PDUMP (ANF,FIN,3,FIN,DUM04,0)
      GO TO 502
  500 PRINT 511,I
      GO TO 502
  501 PRINT 512,I
      PRINT 513
      CALL PDUMP (ANF,FIN,3,FIN,DUM04,0)
  502 CONTINUE
  510 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' CHECKS DONE..................',I2,'   (INCLUDING DUM
     *MY CHECKS)'/
     3            ' CHECKS OKAY..................',I2/
     4            ' CHECKS WRONG.................',I2)
  511 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' ALL ',I2,' CHECKS ARE OKAY')
  512 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' ALL ',I2,' CHECKS ARE WRONG')
  513 FORMAT ('1',34(/),30X,10('*'),'   DUMP IN CASE OF FAILING TEST(S)
     *  ',10('*') )
C  **
C  **
C  **  OUTPUT FORMATS TO PRINT RESULTS OF CHECKS
C  **
C  **
  999 FORMAT ('    I= ',I2,'  DUMMY CHECK')
C  **
 1000 FORMAT (   50X,30H ***************************    /
     *           50X,30H *I-----------------------I*    /
     *        2( 50X, 3H *I  ,23X,              2HI*    /  ),
     *           50X,30H *I COMPILER TEST PROGRAM I*    /
     *        2( 50X, 3H *I  ,23X,              2HI*    /  ),
     *           50X,30H *I-----------------------I*    /
     *           50X,30H ***************************    ///
     $               18H THIS PART CHECKS
     $'COMPLEX ARITHMETIC'            ////'    OKAY            *****  ER
     *ROR IN  ******************'/ ' CHECK NO.          *****  CHECK NO.
     *     CALCULATED VALUES             EXPECTED VALUES'/87('-')///)
 1001 FORMAT ('     I= ',I2,' REAL PART'  )
 1002 FORMAT ('     I= ',I2,' IMAG PART'  )
 1003 FORMAT (/11X,'REAL PART ****     I= ',I2,10X,E10.3,16X,
     *' LE .1    '/41X,O20,' (OCTAL)'/)
 1004 FORMAT (/11X,'IMAG PART ****     I= ',I2,10X,E10.3,16X,
     *' LE .1    '/41X,O20,' (OCTAL)'/)
      ENTRY  ARITH3
      RETURN
      END
      SUBROUTINE  DUM04
      RETURN
      END
*EXECUTE
      PROGRAM COMP05
      EXTERNAL  LOGIF,LOGIF1
      CALL LOGIF  (LOGIF,LOGIF1)
      CALL  EXIT
      END
      SUBROUTINE LOGIF  (ANF,FIN)
      EXTERNAL  DUM05
      LOGICAL L,M,Z
      L2=2
      L3=3
      L= .TRUE.
      M= .FALSE.
      Z= .TRUE.
      I=0
      KDUMP = 0
C
      PRINT 1000
      I = I+1
C
C
C   **  CHECK  1
      IF (2.EQ.2) GO TO 1
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 02
    1 PRINT 1001,I
    2 I = I+1
C   **  CHECK  2
      IF (L2.EQ.L2) GO TO 3
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 04
    3 PRINT 1001,I
    4 I = I+1
C   **  CHECK  3
      IF (L2.EQ.2) GO TO 5
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 06
    5 PRINT 1001,I
    6 I = I+1
C   **  CHECK  4
      IF (2.EQ.L2) GO TO 7
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 08
    7 PRINT 1001,I
    8 I = I+1
C   **  CHECK  5
      IF ((2).EQ.2) GO TO 9
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 10
    9 PRINT 1001,I
   10 I = I+1
C   **  CHECK  6
      IF (2.EQ.(2)) GO TO 11
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 12
   11 PRINT 1001,I
   12 I = I+1
C   **  CHECK  7
      IF ((L2).EQ.L2) GO TO 13
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 14
   13 PRINT 1001,I
   14 I = I+1
C   **  CHECK  8
      IF (L2.EQ.(L2)) GO TO 15
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 16
   15 PRINT 1001,I
   16 I = I+1
C   **  CHECK  9
      IF ((L2).EQ.2) GO TO 17
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 18
   17 PRINT 1001,I
   18 I = I+1
C   **  CHECK 10
      IF (L2.EQ.(2)) GO TO 19
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 20
   19 PRINT 1001,I
   20 I = I+1
C   **  CHECK 11
      IF (2.EQ.(L2)) GO TO 21
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 22
   21 PRINT 1001,I
   22 I = I+1
C   **  CHECK 12
      IF ((2).EQ.L2) GO TO 23
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 24
   23 PRINT 1001,I
   24 I = I+1
C   **  CHECK 13
      IF (((3*2+1)/2).EQ.((4-1)*(3+2)/5)) GO TO 25
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 26
   25 PRINT 1001,I
   26 I = I+1
C   **  CHECK 14
      IF ((((L3*L2)+1)/2).EQ.(((4-1)*(3+L2))/(L3+L2))) GO TO 27
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 28
   27 PRINT 1001,I
   28 I = I+1
C   **  CHECK 15
      IF (L2.NE.L3) GO TO 29
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 30
   29 PRINT 1001,I
   30 I = I+1
C   **  CHECK 16
      IF (L3.GT.L2) GO TO 31
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 32
   31 PRINT 1001,I
   32 I = I+1
C   **  CHECK 17
      IF (L3.GE.L2) GO TO 33
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 34
   33 PRINT 1001,I
   34 I = I+1
C   **  CHECK 18
      IF (L2.LT.L3) GO TO 35
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 36
   35 PRINT 1001,I
   36 I = I+1
C   **  CHECK 19
      IF (L2.LE.L2) GO TO 37
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 38
   37 PRINT 1001,I
   38 I = I+1
C   **  CHECK 20
      IF (L) GO TO 39
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 40
   39 PRINT 1001,I
   40 I = I+1
C   **  CHECK 21
      IF(M) GO TO 41
      PRINT 1001,I
      GO TO 42
   41 PRINT 1002,I
      KDUMP=KDUMP+1
   42 I = I+1
C   **  CHECK 22
      IF (L.AND.Z) GO TO 43
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 44
   43 PRINT 1001,I
   44 I = I+1
C   **  CHECK 23
      IF (L.AND..TRUE.) GO TO 45
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 46
   45 PRINT 1001,I
   46 I = I+1
C   **  CHECK 24
      IF (L.AND.(L3.GT.L2)) GO TO 47
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 48
   47 PRINT 1001,I
   48 I = I+1
C   **  CHECK 25
      IF (.TRUE..AND.Z) GO TO 49
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 50
   49 PRINT 1001,I
   50 I = I+1
C   **  CHECK 26
      IF (.TRUE..AND..TRUE.) GO TO 51
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 52
   51 PRINT 1001,I
   52 I = I+1
C   **  CHECK 27
      IF (.TRUE..AND.(3.GE.2)) GO TO 53
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 54
   53 PRINT 1001,I
   54 I = I+1
C   **  CHECK 28
      IF ((2.LT.3).AND.Z) GO TO 55
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 56
   55 PRINT 1001,I
   56 I = I+1
C   **  CHECK 29
      IF ((2.LE.L3).AND.(.TRUE.)) GO TO 57
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 58
   57 PRINT 1001,I
   58 I = I+1
C   **  CHECK 30
      IF ((2.EQ.2).AND.(L2.LE.L3)) GO TO 59
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 60
   59 PRINT 1001,I
   60 I = I+1
C   **  CHECK 31
      IF (.NOT.M) GO TO 61
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 62
   61 PRINT 1001,I
   62 I = I+1
C   **  CHECK 32
      IF (.NOT..FALSE.) GO TO 63
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 64
   63 PRINT 1001,I
   64 I = I+1
C   **  CHECK 33
      IF (.NOT.(.FALSE.)) GO TO 65
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 66
   65 PRINT 1001,I
   66 I = I+1
C   **  CHECK 34
      IF (.NOT.(L2.GT.3)) GO TO 67
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 68
   67 PRINT 1001,I
   68 I = I+1
C   **  CHECK 35
      IF (.NOT.(M.OR.M)) GO TO 69
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 70
   69 PRINT 1001,I
   70 I = I+1
C   **  CHECK 36
      IF (.NOT.(M.OR.(.FALSE.))) GO TO 71
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 72
   71 PRINT 1001,I
   72 I = I+1
C   **  CHECK 37
      IF (.NOT.(M.OR.(2.GT.L3))) GO TO 73
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 74
   73 PRINT 1001,I
   74 I = I+1
C   **  CHECK 38
      IF (.NOT.(.FALSE..OR.M)) GO TO 75
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 76
   75 PRINT 1001,I
   76 I = I+1
C   **  CHECK 39
      IF (.NOT.((.FALSE.).OR.(.FALSE.))) GO TO 77
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 78
   77 PRINT 1001,I
   78 I = I+1
C   **  CHECK 40
      IF (.NOT.(.FALSE..OR.(3.LT.2))) GO TO 79
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 80
   79 PRINT 1001,I
   80 I = I+1
C   **  CHECK 41
      IF (L.AND..NOT.M) GO TO 81
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 82
   81 PRINT 1001,I
   82 I = I+1
C   **  CHECK 42
      IF (.FALSE..OR..NOT..FALSE.) GO TO 83
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 84
   83 PRINT 1001,I
   84 I = I+1
C   **  CHECK 43
      IF (L.AND.(.NOT.(M))) GO TO 85
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 86
   85 PRINT 1001,I
   86 I = I+1
C   **  CHECK 44
      IF (M.OR.(.NOT.M)) GO TO 87
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 88
   87 PRINT 1001,I
   88 I = I+1
C   **  CHECK 45
      IF (M.OR..NOT.M)  GO TO 89
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 90
   89 PRINT 1001,I
   90 I = I+1
C   **  CHECK 46
      IF (.NOT.(.NOT.(.NOT.(.NOT.(.NOT.(.NOT.(.NOT.(.NOT.(.NOT.(.NOT.(.N
     1OT.(.NOT.(.NOT.M))))))))))))) GO TO 91
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 92
   91 PRINT 1001,I
   92 I = I+1
C   **  CHECK 47
      IF (L2.GT.3.OR.((L2.GT.3).OR.(3.GT.L2).AND..NOT.(.FALSE..OR.L2.
     1GT.3))) GO TO 93
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 94
   93 PRINT 1001,I
   94 I = I+1
C   **  CHECK 48
      IF (.NOT..FALSE..AND..NOT..FALSE..AND..NOT..FALSE.) GO TO 95
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 96
   95 PRINT 1001,I
   96 I = I+1
C   **  CHECK 49
      IF (.NOT..FALSE..AND..NOT..FALSE..OR..NOT..FALSE.) GO TO 97
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 98
   97 PRINT 1001,I
   98 I = I+1
C   **  CHECK 50
      IF (.NOT.((L2.GT.2).AND.((L2 .LT.3).OR.(L2.GT.3))).OR.((M.OR.(L3.G
     1   T.2)).OR..NOT.(L.OR.3.GT.2)).AND.((.NOT.(M.OR.Z)).AND. ((3.GT.2
     2   ).AND..NOT.(2.GT.3)))) GO TO 99
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO  100
   99 PRINT 1001,I
  100 CONTINUE
C
C   ** SUMMARY AND DUMP
C
      INKR = I-KDUMP
      IF  (KDUMP .EQ. 0) GO TO 500
      IF (INKR .EQ.0) GO TO 501
      PRINT 510,I,INKR,KDUMP
      PRINT 513
      GO TO 502
  500 PRINT 511,I
      GO TO 502
  501 PRINT 512,I
      PRINT 513
  502 CONTINUE
  510 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' CHECKS DONE..................',I2,'   (INCLUDING DUM
     *MY CHECKS)'/
     3            ' CHECKS OKAY..................',I2/
     4            ' CHECKS WRONG.................',I2)
  511 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' ALL ',I2,' CHECKS ARE OKAY')
  512 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' ALL ',I2,' CHECKS ARE WRONG')
  513 FORMAT ('1',34(/),30X,10('*'),'   DUMP IN CASE OF FAILING TEST(S)
     *  ',10('*') )
C  **
C  **
C  **  OUTPUT FORMATS TO PRINT RESULTS OF CHECKS
C  **
C  **
  999 FORMAT ('    I= ',I2,'  DUMMY CHECK')
C  **
 1000 FORMAT (   50X,30H ***************************    /
     *           50X,30H *I-----------------------I*    /
     *        2( 50X, 3H *I  ,23X,              2HI*    /  ),
     *           50X,30H *I COMPILER TEST PROGRAM I*    /
     *        2( 50X, 3H *I  ,23X,              2HI*    /  ),
     *           50X,30H *I-----------------------I*    /
     *           50X,30H ***************************    ///
     $               18H THIS PART CHECKS
     $'LOGICAL IF STATEMENTS'         ////'    OKAY            *****  ER
     *ROR IN  ******************'/ ' CHECK NO.          *****  CHECK NO.
     *     CALCULATED VALUES             EXPECTED VALUES'/87('-')///)
 1001 FORMAT ('    I= ',I2)
 1002 FORMAT(/20X,'*****     I= ',I2/ )
      ENTRY LOGIF1
      RETURN
      END
      SUBROUTINE  DUM05
      RETURN
      END
*EXECUTE
      PROGRAM COMP06
      EXTERNAL  ARIF,ARIF1
      CALL ARIF  (ARIF,ARIF1)
      CALL  EXIT
      END
      SUBROUTINE ARIF   (ANF,FIN)
      EXTERNAL  DUM06
      DOUBLE PRECISION D,DIM
      DIMENSION DIM(2)
      DIMENSION INT(1)
      DIMENSION FLOAT(1)
      DIMENSION X(2)
      EQUIVALENCE (X(2),D)
      I=0
      KDUMP = 0
      PRINT 1000
C1
      I=I+1
      IF(34359738367) 1,1,2
    1 PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 50
    2 PRINT 1001,I
   50 I = I+1
C2
      IF(0) 3,4,3
    3 PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 51
    4 PRINT 1001,I
   51 I = I+1
C3
      IF(-0) 5,6,5
    5 PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 52
    6 PRINT 1001,I
   52 I = I+1
C4
      IF(0.0) 7,8,7
    7 PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 53
    8 PRINT 1001,I
   53 I = I+1
C5
      IF(5.) 10,10,11
   10 PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 54
   11 PRINT 1001,I
   54 I = I+1
C6
      IF(.001E-1) 12,12,13
   12 PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 55
   13 PRINT 1001,I
   55 I = I+1
C7
      IF(.00000000000000001D-29) 14,14,15
   14 PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 56
   15 PRINT 1001,I
   56 I = I+1
C8
      IF(-0.0D0) 16,17,16
   16 PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 57
   17 PRINT 1001,I
   57 I = I+1
C9
      IF(.1D1) 18,18,19
   18 PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 58
   19 PRINT 1001,I
   58 I = I+1
C10
      DIM=-0000000000001.D0
      IF(DIM.LT.0.D0) GO TO 20
      PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 59
   20 PRINT 1001,I
   59 I = I+1
C11
      IF(3.D-38) 21,21,22
   21 PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 60
   22 PRINT 1001,I
   60 I = I+1
C12
      X(1)=0.
      X(2)=1.
      IF (D) 24,23,24
   23 PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 61
   24 PRINT 1001,I
   61 I = I+1
C13
      DIM(1)=3.D-38
      IF(DIM(1)) 25,26,26
   25 PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 62
   26 PRINT 1001,I
   62 I = I+1
C14
      INT(1)=0
      IF(INT(1)) 27,28,27
   27 PRINT 1002,I
      KDUMP=KDUMP+1
      GO TO 63
   28 PRINT 1001,I
   63 I = I+1
C15
      FLOAT(1)=-0.023E-15
      IF(FLOAT(1)) 29,30,30
   29 PRINT 1001,I
      GO TO 64
   30 PRINT 1002,I
      KDUMP=KDUMP+1
   64 CONTINUE
C
C   ** SUMMARY AND DUMP
C
      INKR = I-KDUMP
      IF  (KDUMP .EQ. 0) GO TO 500
      IF (INKR .EQ.0) GO TO 501
      PRINT 510,I,INKR,KDUMP
      PRINT 513
      CALL PDUMP (ANF,FIN,3,FIN,DUM06,0)
      GO TO 502
  500 PRINT 511,I
      GO TO 502
  501 PRINT 512,I
      PRINT 513
      CALL PDUMP (ANF,FIN,3,FIN,DUM06,0)
  502 CONTINUE
  510 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' CHECKS DONE..................',I2,'   (INCLUDING DUM
     *MY CHECKS)'/
     3            ' CHECKS OKAY..................',I2/
     4            ' CHECKS WRONG.................',I2)
  511 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' ALL ',I2,' CHECKS ARE OKAY')
  512 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' ALL ',I2,' CHECKS ARE WRONG')
  513 FORMAT ('1',34(/),30X,10('*'),'   DUMP IN CASE OF FAILING TEST(S)
     *  ',10('*') )
C  **
C  **
C  **  OUTPUT FORMATS TO PRINT RESULTS OF CHECKS
C  **
C  **
  999 FORMAT ('    I= ',I2,'  DUMMY CHECK')
C  **
 1000 FORMAT (   50X,30H ***************************    /
     *           50X,30H *I-----------------------I*    /
     *        2( 50X, 3H *I  ,23X,              2HI*    /  ),
     *           50X,30H *I COMPILER TEST PROGRAM I*    /
     *        2( 50X, 3H *I  ,23X,              2HI*    /  ),
     *           50X,30H *I-----------------------I*    /
     *           50X,30H ***************************    ///
     $               18H THIS PART CHECKS
     $'ARIRHMETICAL IF STATEMENTS'    ////'    OKAY            *****  ER
     *ROR IN  ******************'/ ' CHECK NO.          *****  CHECK NO.
     *     CALCULATED VALUES             EXPECTED VALUES'/87('-')///)
 1001 FORMAT ('    I= ',I2)
 1002 FORMAT(/20X,'*****     I= ',I2/)
      ENTRY ARIF1
      RETURN
      END
      SUBROUTINE  DUM06
      RETURN
      END
*EXECUTE
      PROGRAM COMP09
      EXTERNAL  FUNCT,FUNCT1
      CALL FUNCT  (FUNCT,FUNCT1)
      CALL  EXIT
      END
      SUBROUTINE FUNCT  (ANF,FIN)
      EXTERNAL R
      EXTERNAL L
      EXTERNAL  DUM09
      INTEGER X2(2)
      COMPLEX C1,C2,C3
      DOUBLE PRECISION DO
      DOUBLE PRECISION  F4,X1,X3
      COMPLEX CO
      LOGICAL L,F6
      LOGICAL FRI
      DOUBLE PRECISION D1,D2,D3
      DIMENSION BA(1)
      DIMENSION FR(2)
      DIMENSION X11(2),X33(2)
      EQUIVALENCE (C3,FR(1)),(X11,X1),(X33,X3)
      DATA (X1=3.333333D0),(XET=3.3333),(IET=6H3.3333)
      I=0
      KDUMP = 0
      E=10.E-10
      B=1.
      C=2.
      D1=0.0000000001D10
      D2=0.00002D5
      C1=(1.,4.)
      C2=(2.,1.)
      PRINT 1000
C  1-7 TEST REPLACEMENT
      I=I+1
C1 R=R
      X=R(1.,2.)
      IF (.NOT.  L(X,3.) ) GO TO 1
      PRINT 1001,I
      GO TO  2
    1 PRINT 1002,I,X,X
      KDUMP = KDUMP + 1
    2 I=I+1
C2 R=I
      X=IN(1,2)
      IF (.NOT.  L(X,3.) ) GO TO 3
      PRINT 1001,I
      GO TO  4
    3 PRINT 1002,I,X,X
      KDUMP = KDUMP + 1
    4 I=I+1
C3 R=DP
      X=DO(D1,D2)
      IF (.NOT.  L(X,3.) ) GO TO 5
      PRINT 1001,I
      GO TO  6
    5 PRINT 1002,I,X,X
      KDUMP = KDUMP + 1
    6 I=I+1
C4 I=R
      N=R(1.,2.)
      IF ( N .NE. 3 ) GO TO 7
      PRINT 1001,I
      GO TO  8
    7 PRINT 1005,I,N,N
      KDUMP = KDUMP + 1
    8 I=I+1
C5 I=I
      N=IN(1,2)
      IF ( N .NE. 3 ) GO TO 9
      PRINT 1001,I
      GO TO 10
    9 PRINT 1005,I,N,N
      KDUMP = KDUMP + 1
   10 I=I+1
C6 I=DP
      N=DO(D1,D2)
      IF ( N .NE. 3 ) GO TO 11
      PRINT 1001,I
      GO TO 12
   11 PRINT 1005,I,N,N
      KDUMP = KDUMP + 1
   12 I=I+1
C7 C=C
      C3=CO(C1,C2)
      IF (  ( .NOT. L(FR(1),3.) )  .OR.  ( .NOT. L(FR(2),5.))) GO TO 13
      PRINT 1001,I
      GO TO 14
   13 PRINT 1008,I,FR(1),FR(1),FR(2),FR(2)
      KDUMP = KDUMP + 1
   14 I=I+1
C FUNCTION DEFINITION OTHER THAN LEFTSIDE
C8
      X=F3(R,1.,2.)
      IF ( .NOT. L(X,3.)) GO TO 15
      PRINT 1001,I
      GO TO 16
   15 PRINT 1002,I,X,X
      KDUMP = KDUMP + 1
   16 I=I+1
C FUNCTION IS DEFINED BY ENCODE-STATEMENT
C9
      IX=IENC(XET)
      IF( .NOT.L(IX,IET) ) GO TO 17
      PRINT 1001 , I
      GO TO 18
   17 PRINT 1015 , I,IX,IET
      KDUMP = KDUMP + 1
   18 I=I+1
C10 FORMAL ARRAY
      BA(1)=1.
      BA(1)=F5(BA)
      IF ( .NOT. (L(BA(1),2.) )  )  GO TO 19
      PRINT 1001,I
      GO TO 20
   19 PRINT 1011,I,BA(1),BA(1)
      KDUMP = KDUMP + 1
   20 I=I+1
C11
      FRI=F6(.TRUE.)
      IF (FRI) GO TO 21
      PRINT 1001,I
      GO TO 22
   21 PRINT 1012,I,FRI,FRI
      KDUMP = KDUMP + 1
   22 I=I+1
C12   C   FUNCTION DEFINITION BY  ACTUAL PARAM.
      IJK=NOB1(1)
      IF (IJK .NE. 3) GO TO 23
      PRINT 1001,I
      GO TO 24
   23 PRINT 1013,I,IJK,IJK
      KDUMP = KDUMP + 1
   24 I=I+1
C13FUNCTION DEFINITION AS ACTUAL PARAM. TO  OTHER FUNCTION
      IF (NOB2(2) .NE. 4) GO TO 25
      PRINT 1001,I
      GO TO 26
   25 NOB = NOB2(2)
      PRINT 1014,I,NOB,NOB
      KDUMP = KDUMP + 1
   26 CONTINUE
C
C   ** SUMMARY AND DUMP
C
      INKR = I-KDUMP
      IF  (KDUMP .EQ. 0) GO TO 500
      IF (INKR .EQ.0) GO TO 501
      PRINT 510,I,INKR,KDUMP
      PRINT 513
      CALL PDUMP (ANF,FIN,3, FIN,L     ,0,L     ,DUM09,3)
      GO TO 502
  500 PRINT 511,I
      GO TO 502
  501 PRINT 512,I
      PRINT 513
      CALL PDUMP (ANF,FIN,3, FIN,L     ,0,L     ,DUM09,3)
  502 CONTINUE
  510 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' CHECKS DONE..................',I2,'   (INCLUDING DUM
     *MY CHECKS)'/
     3            ' CHECKS OKAY..................',I2/
     4            ' CHECKS WRONG.................',I2)
  511 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' ALL ',I2,' CHECKS ARE OKAY')
  512 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' ALL ',I2,' CHECKS ARE WRONG')
  513 FORMAT ('1',34(/),30X,10('*'),'   DUMP IN CASE OF FAILING TEST(S)
     *  ',10('*') )
C  **
C  **
C  **  OUTPUT FORMATS TO PRINT RESULTS OF CHECKS
C  **
C  **
  999 FORMAT ('    I= ',I2,'  DUMMY CHECK')
C  **
 1000 FORMAT (   50X,30H ***************************    /
     *           50X,30H *I-----------------------I*    /
     *        2( 50X, 3H *I  ,23X,              2HI*    /  ),
     *           50X,30H *I COMPILER TEST PROGRAM I*    /
     *        2( 50X, 3H *I  ,23X,              2HI*    /  ),
     *           50X,30H *I-----------------------I*    /
     *           50X,30H ***************************    ///
     $               18H THIS PART CHECKS
     *'FUNCTIONS'                     ////'    OKAY            *****  ER
     *ROR IN  ******************'/ ' CHECK NO.          *****  CHECK NO.
     *     CALCULATED VALUES             EXPECTED VALUES'/87('-')///)
 1001 FORMAT ('    I= ',I2)
 1002 FORMAT(/20X,'*****     I= ',I2,6X,'X = ',F10.4,16X,'        3.'/
     *        41X,O20,' (OCTAL)'/)
 1005 FORMAT(/20X,'*****     I= ',I2,6X,'N = ',I10  ,16X,'         3'/
     *        41X,O20,' (OCTAL)'/)
 1008 FORMAT(/20X,'*****     I= ',I2,6X,'FR(1)= ',F10.4,13X,'        3.'
     1/       41X,O20,' (OCTAL)'/
     2        41X,                      'FR(2)= ',F10.4,13X,'        5.'
     *        41X,O20,' (OCTAL)'/)
 1010 FORMAT(/20X,'*****     I= ',I2,6X,'X33=',E13.6,13X,E13.6/
     $        41X,O20,' (OCTAL)'/
     1        41X,                      'X33=',E13.6,13X,E13.6 /
     *        41X,O20,' (OCTAL)'/)
 1011 FORMAT(/20X,'*****     I= ',I2,6X,'BA(1)= ',F10.4,13X,'        2.'
     *        41X,O20,' (OCTAL)'/)
 1012 FORMAT(/20X,'*****     I= ',I2,6X,'FRI= ',L10,14X,'     FALSE'/
     *        41X,O20,' (OCTAL)'/)
 1013 FORMAT(/20X,'*****     I= ',I2,6X,'IJK= ',I10,14X,'         3'/
     *        41X,O20,' (OCTAL)'/)
 1014 FORMAT(/20X,'*****     I= ',I2,6X,'NOB2(2)= ',I10,12X,'       4'/
     *        41X,O20,' (OCTAL)'/)
 1015 FORMAT(/20X'*****    I ='I3,11X,A6,24X,A6/)
      ENTRY FUNCT1
      RETURN
      END
      FUNCTION IENC(XT)
      ENCODE (6,100,IENC) XT
  100 FORMAT(F6.4)
      RETURN
      END
      LOGICAL FUNCTION L(A,B)
      L=(ABS(A-B) .LE. 1.E-9)
      RETURN
      END
      REAL FUNCTION R(A,B)
      R=A+B
      RETURN
      END
      INTEGER FUNCTION IN(M,N)
      IN=M+N
      RETURN
      END
      DOUBLE PRECISION FUNCTION DO(A,B)
      DOUBLE PRECISION A,B
      DO = A+B
      RETURN
      END
      COMPLEX FUNCTION CO (A,B)
      COMPLEX A,B
      CO = A+B
      RETURN
      END
      FUNCTION F3(X,Y,Z)
      F3=X(Y,Z)
      RETURN
      END
      DOUBLE PRECISION FUNCTION  F4(Y)
      DECODE (10,1,Y) F4
      RETURN
    1 FORMAT (D10.6)
      END
      FUNCTION F5(A)
      DIMENSION A(1)
      F5=A(1)+1.
      RETURN
      END
      FUNCTION NOB1(I)
      CALL NOC1(NOB1,I)
      RETURN
      END
      SUBROUTINE NOC1(I,J)
      I=3*J
      RETURN
      END
      FUNCTION NOB2(J)
      K=JOE(NOB2,J)
      RETURN
      END
      FUNCTION JOE(L,M)
      L=2*M
      JOE=5
      RETURN
      END
      SUBROUTINE  DUM09
      RETURN
      END
      LOGICAL FUNCTION F6(X)
      LOGICAL X
      F6=.NOT. X
      RETURN
      END
*EXECUTE
      PROGRAM COMPO12
      EXTERNAL ORDER,ORDER1
      CALL ORDER (ORDER,ORDER1)
      CALL EXIT
      END
      SUBROUTINE ORDER (ANF,FIN)
      EXTERNAL DUM12
      DIMENSION  H(13),C(15,3)
C
C   **   CHECKS  THE  HIERARCHY  OF  ARITHMETIC  OPERATION
C
      I=0
      KDUMP=0
      PRINT 1000
C
C
C  **   CHECK  1 . ADDITION , SUBTRACTION / DIVISION , MULTIPLICATION
C  **     WITHOUT  PARENTHESES .
C
      I=I+1
      J0=4+7/2
      J1=13/5+10
      J2=4*2-5
      J3=-6+3/2
      J4=J3+3*3
      M=J1-J0/J2+J3*2+5/J4-1
      IF (M .NE. 0) GO TO 1
      PRINT 1001,I
      GO TO 2
    1 PRINT 1002,I,M,M
      KDUMP=KDUMP+1
    2 I=I+1
C
C  **   CHECK  2  .  ADDITION , SUBTRACTION / EXPONENTIATION
C  **        WITHOUT  PARENTHESES  .
C
      Y1=3.**2.+1.E00
      Y2=5.1-3.0**3
      Y3=2.5E+01**2 +Y2-3.1
      X=Y1**2+Y3+Y2-678.1
      IF (X .LT. 0)  X=-X
      IF (X .GT. .1) GO TO 3
      PRINT 1001,I
      GO TO 4
    3 PRINT 1003,I,X,X
      KDUMP=KDUMP+1
    4 I=I+1
C
C  **  CHECK  3  .  DIVISION , MULTIPLICATION / EXPONENTIATION
C  **           WITHOUT  PARENTHESES .
      J0=3**2/4
      J1=2**3*5
      J2=3*9**2
      J3=3/J0**2
      M=J3+J2+J1-283
      IF (M .NE. 0) GO TO 5
      PRINT 1001,I
      GO TO 6
    5 PRINT 1002,I,M,M
      KDUMP=KDUMP+1
    6 I=I+1
C
C  **   CHECK  4  .  ADDITION , SUBTRACTION , DIVISION ,MULTIPLICATION
C  **   AND  EXPONENTIATION  WITHOUT  PARENTHESES .
C
      Y0=4.0**.2E01/8.E0+2.5*3.*.2-3.5
      Y1= 2.7E+1/.003E3**2+9.97E2-3.E4/1.E1**2-70.E1
      X=Y0+Y1
      IF (X .LT. 0)  X=-X
      IF (X .GT. .1) GO TO 7
      PRINT 1001,I
      GO TO 8
    7 PRINT 1003,I,X,X
      KDUMP=KDUMP+1
    8 I=I+1
C
C  **  CHECK 5 .  EXPRESSIONS  WITH  ONE  LEVEL  OF  PARENTHESES .
C
      J0=4**2/(5+4)+1
      J1 =(3**2)**3
      J2=3**(J0**3)
      J3=9/(5-3)**3-5*(3**J0)
      M=(J0-2)+(J1-729)-(J3+44)-6561+J2
      IF (M .NE. 0) GO TO 9
      PRINT 1001,I
      GO TO 10
    9 PRINT 1002,I,M,M
      KDUMP=KDUMP+1
   10 I=I+1
C
C   **  CHECK  6  .  EXPRESSIONS  WITH  EMBEDDED  PARENTHESES .
C
      J1=6*(12+((16/3)-2))
      J3=((6-(4/3))/2)**((25/20)*2)
      M=J3-90+J1-4
      IF (M .NE. 0) GO TO 11
      PRINT 1001,I
      GO TO 12
   11 PRINT 1002,I,M,M
      KDUMP=KDUMP+1
   12 I=I+1
C
C   **  CHECK  7 . EXPRESSIONS  CONTAINING  A  FUNCTION .
C
      H(13)=3.45
      C(14,2)=4.E00
      Y1=H(13)+C(14,2)*(COS(3.14159265/.3E1))**2
      Y2=C(14,2)
     1    *(SIN(3.14159265/6.0)+1.)-1.E3/(C(14,2)*(60.-
     1(Y1+5.55)))
      X=Y1-4.45+Y2-1.
      IF (X .LT. 0)  X=-X
      IF (X .GT. .1) GO TO 13
      PRINT 1001,I
      GO TO 14
   13 PRINT 1003,I,X,X
      KDUMP=KDUMP+1
   14 CONTINUE
C
C   ** SUMMARY AND DUMP
C
      INKR = I-KDUMP
      IF  (KDUMP .EQ. 0) GO TO 500
      IF (INKR .EQ.0) GO TO 501
      PRINT 510,I,INKR,KDUMP
      PRINT 513
      GO TO 502
  500 PRINT 511,I
      GO TO 502
  501 PRINT 512,I
      PRINT 513
  502 CONTINUE
      IIIII=1
  510 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' CHECKS DONE..................',I2,'   (INCLUDING DUM
     *MY CHECKS)'/
     3            ' CHECKS OKAY..................',I2/
     4            ' CHECKS WRONG.................',I2)
  511 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' ALL ',I2,' CHECKS ARE OKAY')
  512 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' ALL ',I2,' CHECKS ARE WRONG')
  513 FORMAT ('1',34(/),30X,10('*'),'   DUMP IN CASE OF FAILING TEST(S)
     *  ',10('*') )
C  **
C  **
C  **  OUTPUT FORMATS TO PRINT RESULTS OF CHECKS
C  **
C  **
  999 FORMAT ('    I= ',I2,'  DUMMY CHECK')
C  **
 1000 FORMAT (   50X,30H ***************************    /
     *           50X,30H *I-----------------------I*    /
     *        2( 50X, 3H *I  ,23X,              2HI*    /  ),
     *           50X,30H *I COMPILER TEST PROGRAM I*    /
     *        2( 50X, 3H *I  ,23X,              2HI*    /  ),
     *           50X,30H *I-----------------------I*    /
     *           50X,30H ***************************    ///
     $               18H THIS PART CHECKS
     *'ORDER OF ARITHM EXPRESSIONS'   ////'    OKAY            *****  ER
     *ROR IN  ******************'/ ' CHECK NO.          *****  CHECK NO.
     *     CALCULATED VALUES             EXPECTED VALUES'/87('-')///)
 1001 FORMAT ('    I= ',I2)
 1002 FORMAT(/20X,'*****     I=',I2,6X,'M=  ',I10  ,16X,'         0'/
     *        41X,O20,' (OCTAL)'/)
 1003 FORMAT(/20X,'*****     I=',I2,6X,'X=  ',F10.3,16X,'      .1  '/
     *        41X,O20,' (OCTAL)'/)
      ENTRY ORDER1
      RETURN
      END
      SUBROUTINE DUM12
      RETURN
      END
*EXECUTE
      PROGRAM COMP13
      EXTERNAL STFCZF,STFCZ1
      CALL STFCZF (STFCZF,STFCZ1)
      CALL EXIT
      END
      SUBROUTINE STFCZF (ANF,INF)
      EXTERNAL PROD
      EXTERNAL DUM13
      DIMENSION F1(2),F2(2)
      COMPLEX COM2,C1,C2
      COMPLEX CV
      DOUBLE PRECISION D,CDOUB,D1,D2
      LOGICAL ANDGV,L1,L2,L3,L4,REL,LPR3,LPR6
      EQUIVALENCE (CV,F1(1)),(D2,F2(1))
      DATA (D1=.33333333333333333333333333333D0)
      DATA (L3=.TRUE.),(L4=.FALSE.)
C  ARITHMETIC  REAL
      SBIN(Z,B)=Z**2.+2.*Z*B+B**2.
C  INTEGER
      IQUAD (M,N,I)=M**I+M*N*I+N**I
C  COMPLEX
      COM2(C1)=C1*C1
C  DOUBLE
      CDOUB(D)=D*0.3D0
C LOGICAL
      ANDGV(L1,L2)=L1.OR.L2
C REFERRING LIBRARY FUNCTION
      GREAT(X,Y,Z)=AMAX1(X,Y,Z)
C REFERRING FUNTIONS
      DSQUA(X)=PROD(X,X)
C REFERRING STATEMENTS FUNCTION
      NEXT(I)=I+1
      LAST(I)=NEXT(I)-2
C REFERRING VARIABLES LOCAL TO THE ROUTINE
       SOME(X) = X/2.0+C
      DUMM(XI,YI)=XI+YI
C LOGICAL REALATIONAL
      REL(M1,M2,M3)= M1.GT.M2 .OR. M2.GT.M3
C
C
      I=0
      KDUMP=0
      PRINT 1000
      EPS=10E-6
      N1=2
      N2=3
      N3=4
      I=I+1
C1 ARITHMETIC  REAL
       PR1  = ABS( SBIN(3.,2.) )
      IF (ABS(SBIN(3.,2.)-25.).GT.EPS) GO TO 1
      PRINT 1001,I
      GO TO  2
    1 PRINT 1002,I,PR1,PR1
      KDUMP=KDUMP+1
    2 I=I+1
C2 INTEGER
      IPR2 = IQUAD(N2,N3,N1)
      IF (IQUAD(N2,N3,N1) .NE. 49) GO TO 3
      PRINT 1001,I
      GO TO  4
    3 PRINT 1003,I,IPR2,IPR2
      KDUMP=KDUMP+1
    4 I=I+1
C3 COMPLEX
      CV=COM2((2.,3.))
      IF (ABS(F1(1)+5.).GT. EPS .OR. ABS(F1(2)-12.).GT.EPS) GO TO 5
      PRINT 1001,I
      GO TO  6
    5 PRINT 1004,I,F1(1),F1(1),F1(2),F1(2)
      KDUMP=KDUMP+1
    6 I=I+1
C4 DOUBLE
      D2=CDOUB(D1)
      IF (ABS(F2(1)-.099999999D0).GT.EPS .OR.
     $ABS(F2(2)-.999999999D-49).GT.EPS) GO TO 7
      PRINT 1001,I
      GO TO  8
    7 PRINT 1005,I,F2(1),F2(1),F2(2),F2(2)
      KDUMP=KDUMP+1
    8 I=I+1
C5 LOGICAL
      LPR3 = ANDGV(L3,L4)
      IF (.NOT. ANDGV(L3,L4)) GO TO 9
      PRINT 1001,I
      GO TO 10
    9 PRINT 1006,I,LPR3,LPR3
      KDUMP=KDUMP+1
   10 I=I+1
C6 REFERENCING LIBRARY FUNCTIONS
      PR4  = ABS( GREAT(3.,28.,-5.) )
      IF (ABS(GREAT(3.,28.,-5.)-28.).GT.EPS)  GO TO 11
      PRINT 1001,I
      GO TO 12
   11 PRINT 1007,I,PR4,PR4
      KDUMP=KDUMP+1
   12 I=I+1
C7 REFERRING FUNCTIONS
      Z=DSQUA(3.)
      IF(ABS(Z-9. ).GT.EPS) GO TO 13
      PRINT 1001,I
      GO TO 14
   13 PRINT 1008,I,Z,Z
      KDUMP=KDUMP+1
   14 I=I+1
C8 REFERRING STATEMENT FUNCTIONS
      IPR5 = LAST(29)
      IF(LAST(29) .NE. 28) GO TO 15
      PRINT 1001,I
      GO TO 16
   15 PRINT 1009,I,IPR5,IPR5
      KDUMP=KDUMP+1
   16 I=I+1
C9 REFERTING LOCAL VARIABLES
      X=6.
      C=3.
      Z=SOME(14.)
      IF (ABS(Z-10.) .GT.EPS) GO TO 17
      PRINT 1001,I
      GO TO 18
   17 PRINT 1010,I,Z,Z
      KDUMP=KDUMP+1
   18 I=I+1
C10 CHECKING THE SAME NAME OF FORMAL AND LOCAL PARAMETERS
      IF (ABS(X-6.).GT.EPS) GO TO 19
      PRINT 1001,I
      GO TO 20
   19 PRINT 1011,I,X,X
      KDUMP=KDUMP+1
   20 I=I+1
C11  LOGICAL RELATIONAL
      LPR6 = REL(2,4,3)
      IF (.NOT.(REL(2,4,3))) GO TO 21
      PRINT 1001,I
      GO TO 22
   21 PRINT 1012,I,LPR6,LPR6
      KDUMP=KDUMP+1
   22 I=I+1
C      ......
C12 LOCAL PARAMETERS WITH SAME NAME THAN PROGRAM VARIABLES
      XI=2.
      YI=3.
      ZI=DUMM(5.,3.)
      IF (ABS (XI+YI-5.) .GT. EPS) GO TO 23
      PRINT 1001,I
      GO TO 24
   23 PRINT 1013,I,XI,XI,YI,YI,ZI,ZI
      KDUMP=KDUMP+1
   24 CONTINUE
C
C   ** SUMMARY AND DUMP
C
      INKR = I-KDUMP
      IF  (KDUMP .EQ. 0) GO TO 500
      IF (INKR .EQ.0) GO TO 501
      PRINT 510,I,INKR,KDUMP
      PRINT 513
      CALL PDUMP (ANF,FIN,3, FIN,PROD  ,0,PROD  ,DUM13,3)
      GO TO 502
  500 PRINT 511,I
      GO TO 502
  501 PRINT 512,I
      PRINT 513
      CALL PDUMP (ANF,FIN,3, FIN,PROD  ,0,PROD  ,DUM13,3)
  502 CONTINUE
  510 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' CHECKS DONE..................',I2,'   (INCLUDING DUM
     *MY CHECKS)'/
     3            ' CHECKS OKAY..................',I2/
     4            ' CHECKS WRONG.................',I2)
  511 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' ALL ',I2,' CHECKS ARE OKAY')
  512 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' ALL ',I2,' CHECKS ARE WRONG')
  513 FORMAT ('1',34(/),30X,10('*'),'   DUMP IN CASE OF FAILING TEST(S)
     *  ',10('*') )
C  **
C  **
C  **  OUTPUT FORMATS TO PRINT RESULTS OF CHECKS
C  **
C  **
  999 FORMAT ('    I= ',I2,'  DUMMY CHECK')
C  **
 1000 FORMAT (   50X,30H ***************************    /
     *           50X,30H *I-----------------------I*    /
     *        2( 50X, 3H *I  ,23X,              2HI*    /  ),
     *           50X,30H *I COMPILER TEST PROGRAM I*    /
     *        2( 50X, 3H *I  ,23X,              2HI*    /  ),
     *           50X,30H *I-----------------------I*    /
     *           50X,30H ***************************    ///
     $               18H THIS PART CHECKS
     *'STATEMENTFUNCTIONS'            ////'    OKAY            *****  ER
     *ROR IN  ******************'/ ' CHECK NO.          *****  CHECK NO.
     *     CALCULATED VALUES             EXPECTED VALUES'/87('-')///)
 1001 FORMAT ('    I= ',I2)
 1002 FORMAT(/20X,'*****     I=',I2,6X,'PR1=',F10.3,16X,'    25.   '/
     *        41X,O20,' (OCTAL)'/)
 1003 FORMAT(/20X,'*****     I=',I2,6X,'IPR2=',I10,15X,'        49'/
     *        41X,O20,' (OCTAL)'/)
 1004 FORMAT(/20X,'*****     I=',I2,6X,'REAL(CV)=',F10.3,11X,'     -5.'/
     $        41X,O20,' (OCTAL)'/
     1        41X,                     'IMAG(CV)=',F10.3,11X,'     12.'/
     *        41X,O20,' (OCTAL)'/)
 1005 FORMAT(/20X,'*****     I=',I2,6X,'UPPER(D2)=',E16.9,4X,'.099999999
     $'/      41X,O20,' (OCTAL)'/
     1        41X,                     'LOWER(D2)=',E16.9,4X,'.999999999
     *        41X,O20,' (OCTAL)'/)
 1006 FORMAT(/20X,'*****     I=',I2,6X,'LPR3=',L10 ,15X,'   TRUE   '/
     *        41X,O20,' (OCTAL)'/)
 1007 FORMAT(/20X,'*****     I=',I2,6X,'PR4=',F10.3,16X,'    28.   '/
     *        41X,O20,' (OCTAL)'/)
 1008 FORMAT(/20X,'*****     I=',I2,6X,'Z  =',F10.3,16X,'     9.   '/
     *        41X,O20,' (OCTAL)'/)
 1009 FORMAT(/20X,'*****     I=',I2,6X,'IPR5=',I10 ,15X,'        28'/
     *        41X,O20,' (OCTAL)'/)
 1010 FORMAT(/20X,'*****     I=',I2,6X,'Z  =',F10.3,16X,'    10.   '/
     *        41X,O20,' (OCTAL)'/)
 1011 FORMAT(/20X,'*****     I=',I2,6X,'X  =',F10.3,16X,'     6.   '/
     *        41X,O20,' (OCTAL)'/)
 1012 FORMAT(/20X,'*****     I=',I2,6X,'LPR6=',L10 ,15X,'   TRUE   '/
     *        41X,O20,' (OCTAL)'/)
 1013 FORMAT(/20X,'*****     I=',I2,6X,'XI =',F10.3,16X,'     2.   '/
     $        41X,O20,' (OCTAL)'/
     1        41X,                     'YI =',F10.3,16X,'     3.   '/
     $        41X,O20,' (OCTAL)'/
     1        41X,                     'ZI =',F10.3,16X,'     5.   '/
     *        41X,O20,' (OCTAL)'/)
      ENTRY STFCZ1
      RETURN
      END
      FUNCTION PROD(A,B)
      PROD=A*B
      RETURN
      END
      SUBROUTINE DUM13
      RETURN
      END
*EXECUTE
       PROGRAM COMP15
      EXTERNAL VARDIM,VARDI1
      CALL VARDIM (VARDIM,VARDI1)
      CALL EXIT
      END
          SUBROUTINE VARDIM (ANF,FIN)
          COMMON /TAPE/D1(500),INT(64),D2(300),FL(64),D3(600),DP(64),D4(
     1    600),CP(64)
          COMPLEX CP,C
          DOUBLE PRECISION DP
          DIMENSION CP1(128),DP1(128)
          EQUIVALENCE (CP(1),CP1(1)),(DP1(1),DP(1))
C
C     PRESET ARRAYS WITH CHECKVALUES
C
          DO 1 I=1,64
          INT(I)=I
    1     FL(I)=I
          S=0
          DO 2 I=1,128,2
          S=S+1.
          CP1(I)=S
          CP1(I+1)=S+64.
          DP1(I)=S
    2     DP1(I+1)=S+64.
C
C      CHECK WITH SINGLE WORD VARIABLES   (CHECK 1 - 28)
C
          CALL SIWOZV(INT,INT,INT,FL,FL,FL,64,16,4,4,4,4,64,4,16,8,4,2,
     *                ANF,FIN )
C
C     CHECK WITH DOUBLE PRECISION
C
          K=28
          RETURN
      END
          SUBROUTINE SIWOZV (I1,F2,I3,F1,I2,F3,M1,M2,M3,M4,M5,M6,N1,N2,N
     1    3,N4,N5,N6,  ANF,FIN )
      EXTERNAL M1ZV
          EXTERNAL DUM15
          DIMENSION I1(M1),F3(N4,N5,N6),I4(28),F4(28)
          INTEGER F2(M2,M3),I3(M4,M5,M6),R(64)
          REAL F1(N1),I2(N2,N3),W(64)
          EQUIVALENCE(R(1),W(1)),(I4(1),F4(1))
      ARITH 1(K,P)=P-FLOAT(K)
C
      PRINT 1000
C     CHECK 1 - 11           RIGHT SIDE,FIXED AND VARIABLE SUBSCRIPTS
C     CHECK 1 - 11           I4(I) INDICATES CHECK CASE
C                                                            ALONE
          I4(1)=I1(1)
          I4(2)=I3(2,2,3)-36
          I4(3)=F2(7,2)-20
          I4(4)=I1(M6)
          I4(5)=F2(2*M2-16,N4-4)-59
          K=3
          L=2
          J1=5
          I4(6)=I3(3*J1-11,2*L,K-1)-26
C                                                            EXPRESSION
          I4(7)=(I1(7)*2-(I1(3)-3))-I1(4)-3
          I4(8)=F2(16,1)-I3(2,2,2)+14
          I4(9)=I3(M1-60,2*K-5,J1-4)*2+(F2(5,L-1)-I1(2))*L-J1
C                                                            OUTPUT-LIST
          REWIND 16
          WRITE (16)I3
          WRITE(16)((F2(K1,K2),K1=1,N3),K2=1,N5)
          REWIND 16
          READ (16)R
          I4(10)=10
          DO 1 I=1,64
          IF(R(I).NE.I) I4(10)=0
    1     CONTINUE
          I4(11)=11
          READ (16)R
          DO 2 I=1,64
          IF(R(I).NE.I) I4(11)=0
    2     CONTINUE
          REWIND 16
C
C     CHECK 12 - 19          LEFT SIDE, FIXED AND VARIABLE SUBSCRIPTS
C                            F4(I) INDICATES CHECK CASE
C                                                            REPLACEMENT
          F1(12)=12.
          F4(12)=F1(12)
          I2(1,4)=13.
          F4(13)=F1(13)
          F3(6,2,1)=14.
          F4(14)=F1(14)
          F1(M2-1)=15.
          F4(15)=F1(15)
          I2(N6-1,3*N3-44)=16.
          F4(16)=F1(16)
          F3(1,2*M3-7,2*L-3)=17.
          F4(17)=F1(17)
C                                                            INPUT LIST
          READ (16)I3
          F4(18)=18.
          DO 3 I=1,64
          IF(I1(I).NE.I) F4(18)=0
    3     CONTINUE
          READ (16)((F2(K1,K2),K1=1,N3),K2=1,N5)
          F4(19)=19.
          DO 4 I=1,64
          IF(I1(I).NE.I) F4(19)=0
    4     CONTINUE
C
C         CHECK 20 - 25      ACTUAL PARAMETER, DIFFERENT FORMS
C
          CALL M1ZV (F4(20),F1(20),I2(1,6),F3(6,3,1),I2(M3-1,3*N6),F3(N4
     1    ,M5-1,2*N5-7),(F3(N3-8,M6,2*N5-6)-I2(2,4))/2.0)
C
C     CHECL 26               ARITHMETIC STATEMENT FUNCTION.
C
          L=2
          F4(26)=ARITH1(IFIX(I2(2,7)),52.)
C
C     CHECK 27 - 28            I/O LIST,DIFFERENT FROM ABOVE
C
          REWIND 16
          WRITE (16)((I2(K1,K2),K1=N6,N5),K2=M4,N4)
          REWIND 16
          READ (16)(W(I),I=1,15)
          WSUM=0
          DO 5 I=1,15
    5     WSUM=WSUM+W(I)
          F4(27)=27.
          IF(WSUM.NE.345.) F4(27)=0
          REWIND 16
         READ(16)(((F3(K1,K2,K3),K1=4,8),K2=2,4),K3=N6,2)
          WSUM=0.
          DO 6 K1=4,8
          DO 6 K2=2,4
          DO 6 K3=N6,2
    6     WSUM=WSUM+F3(K1,K2,K3)
          F4(28)=28.
          IF(WSUM.NE.345.) F4(28)=0.
C  *****
C  *****
C  ***** FINAL CHECKING WITH PRINTING  *****
C  *****
      KDUMP=0
      DO 8  I=1,28
      S = I
      IF( .NOT.  (I4(I) .EQ. I   .OR.   F4(I) .EQ. S)  )     GO TO 10
      PRINT 1001,I
      GO TO 8
   10 PRINT 1002,I
    7 KDUMP=KDUMP+1
    8  C O N T I N U E
C
C   ** SUMMARY AND DUMP
C
      INKR = I-KDUMP
      IF  (KDUMP .EQ. 0) GO TO 500
      IF (INKR .EQ.0) GO TO 501
      PRINT 510,I,INKR,KDUMP
      PRINT 513
      CALL PDUMP (ANF,FIN,3, FIN,M1ZV  ,0,M1ZV  ,DUM15,3)
      GO TO 502
  500 PRINT 511,I
      GO TO 502
  501 PRINT 512,I
      PRINT 513
      CALL PDUMP (ANF,FIN,3, FIN,M1ZV  ,0,M1ZV  ,DUM15,3)
  502 CONTINUE
  510 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' CHECKS DONE..................',I2,'   (INCLUDING DUM
     *MY CHECKS)'/
     3            ' CHECKS OKAY..................',I2/
     4            ' CHECKS WRONG.................',I2)
  511 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' ALL ',I2,' CHECKS ARE OKAY')
  512 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' ALL ',I2,' CHECKS ARE WRONG')
  513 FORMAT ('1',34(/),30X,10('*'),'   DUMP IN CASE OF FAILING TEST(S)
     *  ',10('*') )
C  **
C  **
C  **  OUTPUT FORMATS TO PRINT RESULTS OF CHECKS
C  **
C  **
  999 FORMAT ('    I= ',I2,'  DUMMY CHECK')
C  **
 1000 FORMAT (   50X,30H ***************************    /
     *           50X,30H *I-----------------------I*    /
     *        2( 50X, 3H *I  ,23X,              2HI*    /  ),
     *           50X,30H *I COMPILER TEST PROGRAM I*    /
     *        2( 50X, 3H *I  ,23X,              2HI*    /  ),
     *           50X,30H *I-----------------------I*    /
     *           50X,30H ***************************    ///
     $               18H THIS PART CHECKS
     *'VARIABLE DIMENSIONS'           ////'    OKAY            *****  ER
     *ROR IN  ******************'/ ' CHECK NO.          *****  CHECK NO.
     *     CALCULATED VALUES             EXPECTED VALUES'/87('-')///)
 1001 FORMAT ('    I= ',I2)
 1002 FORMAT(/20X,' *****      I=',I2,' *****'/)
      ENTRY VARDI1
          RETURN
      END
          SUBROUTINE M1ZV(A,B,C,D,E,F,G)
          DIMENSION A(6)
          A(1)=B
          A(2)=C
          A(3)=D
          A(4)=E
          A(5)=F
          A(6)=G
          RETURN
          END
      SUBROUTINE DUM15
      RETURN
      END
*EXECUTE
      PROGRAM COMPO8
      EXTERNAL ENCDEC,ENCDE1
      CALL ENCDEC  (ENCDEC,ENCDE1)
      CALL  EXIT
       I=1
       END
      SUBROUTINE ENCDEC  (ANF,FIN)
      EXTERNAL FEN,DUM08
      DIMENSION ZABBB(3)
      DIMENSION NP1(2),NQ1(2),ZAA(2),NBB(2),NBE(2),NLB(2),NLH(2),NLW(4),
     ,NNA(2),FGB(6),NUB(2)
      DIMENSION NG(2),NN(2),NO(2),NR(2),NAA(2),LAST(2)
      DIMENSION NAD(10),NAG(6)
      DIMENSION FAC(5)
      DIMENSION EL1(12),EL2(4),NAB(3)
      DIMENSION NS(4)
      DIMENSION NAC(15)
      DIMENSION NAE(5),NAF(5),AG(2),AD( 2)
      DIMENSION DDB(9)
      DIMENSION NLD(3)
      DIMENSION DPRI(2)
      EQUIVALENCE (NAF(4),AG(1))
      EQUIVALENCE(NAD(4),AD(1)  )
      EQUIVALENCE ( ZABBB(1),ZAB)
      EQUIVALENCE (FAC(2),NAH)
      EQUIVALENCE (ZAB,LSTA)
      EQUIVALENCE(ELF,NS(4))
      EQUIVALENCE (NNE,FGA)
      DOUBLE PRECISION DDA,DDC
      EQUIVALENCE (DDC,DPRI(1) )
      DOUBLE PRECISION DA,DEPS
      LOGICAL LA
      LOGICAL NLS,NLT
      DATA (NA=10H31415.9E-4)
      DATA  (NB=10H    3.1415)
      DATA  (NC=10H000.3D-41      )
      DATA  (ND=10H       325)
      DATA (NE=10H       100)
      DATA (NF=10H    AAAB  )
      DATA NG/20240501 20420040B,10020040 10020040B/
      DATA  (NH=10H      T    )
      DATA  (NI=10H  314.1592)
      DATA  (NL=10H1234567890)
      DATA  (NM=10HA        A)
      DATA NN/20220040 10020040B,10020040 20220040B/
      DATA  (NP=10HB       BB)
      DATA NO/20420040 10020040B,10020102 20420040B/
      DATA (NQ=10HBBBB','BBB)
      DATA  (NT=10HBBBBBB      )
      DATA NR/20441102 20441102B,10020040 10020040B/
      DATA ((NS(I),I=1,3)=312,1,1)
      DATA ( ELF=3.14)
      DATA    EL1 /70H+1.632E-04-0032.7216    +328+5   .629E-1      +136
     * .07628431      E-06/
      DATA    EL2 /20H 3.67294+5 3.67294+5/
      DATA (EL3=10H 8936+004   )
      DATA (EL4=10H          )
      DATA (EL5=10H144.15E-03)
      DATA NAA/10HAABBBB    /
      DATA (NAB=14HAAAAAAAAAABBBB)
      DATA (NAC=2H11,2H12,2H13,2H14,2H15)
      DATA (NAE=1H1,1H2,1H3,1H4,1H5)
      DATA((NAF(IS),IS=1,3)=4H1000,4H2000,4H3000)
      DATA (AG(1)=2H0.),(AG(2)=4H3.14)
      DATA NAG/18H    123.8 00312382,18H         1.234    /
      DATA (FAD=10H  3.1E-128)
      DATA (ZAD=-6.732E+1)
      DATA LAST/10H          /
      DATA (ZAZ=1.3)
      DATA (ZAA=0.006E+3)
      INTEGER ST00
      DATA ST00/3H*00/
      DATA (ZAF=36.12)
      DATA (NBC=-32.6948921)
      DATA  (ISTARS=10H*2.6948921)
      DATA (DDA=.33333333333333333333333333333D-205)
      DATA (NBD=3456)
      DATA (NBEF = 4H  *6)
      DATA (NBG=334B)
      DATA NBH/777444B/
      DATA (NLA   =5HNONNA)
      DATA (NLC=10H     NONNA)
      DATA (NLD =  10H     MAMMA,4H MIA   )
      DATA (NLF=10H     M MIA)
      DATA (NLS=.FALSE.),(NLT=.TRUE.)
      DATA (NLU=5H    F),(NLV=2H T)
      DATA (NNB=5H BBBB)
      DATA (NNC=10H  ABC  ABC)
      DATA (FGA= 3.1415926536)
      DATA (NND=10H3.14      )
      I=0
      K = 0
      EPS=1.E-6
      DEPS = 1.D-20
C TEST OF FORMAT INPUT SPECIFICATIONS BY DECODE
      I=I+1
      PRINT 1000
C1 EWD
      DECODE (10,2,NA) FA
      IF (ABS(FA - 3.14159) .GT. EPS) GO TO 101
      PRINT 1001,I
      GO TO 102
  101 PRINT 1002,I,FA,FA
      K = K+1
  102 I = I+1
C2 FWD
      DECODE (10,2,NB) FA
      IF (ABS(FA - 3.1415) .GT. EPS) GO TO 103
      PRINT 1001,I
      GO TO 104
  103 PRINT 1003,I,FA,FA
      K = K+1
  104 I = I+1
C3 DWD
      DECODE (10,3,NC) DA
      IF (DABS(DA - .3 D-41) .GT. DEPS) GO TO 105
      PRINT 1001,I
      GO TO 106
  105 PRINT 1004,I,DA,DA
      K = K+1
  106 I = I+1
C4 IW
      DECODE(10,4,ND) MA
      IF (MA .NE. 325) GO TO 107
      PRINT 1001,I
      GO TO 108
  107 PRINT 1005,I,MA,MA
      K = K+1
  108 I = I+1
C5 OW
      DECODE (10,5,NE) MA
      IF(MA.NE.100B)GO TO 109
      PRINT 1001,I
      GO TO 110
  109 PRINT 1006,I,MA,MA
      K = K+1
  110 I = I+1
C6 AW
      DECODE (10,6,NF) MA
      IF (MA .NE. NG) GO TO 111
      PRINT 1001,I
      GO TO 112
  111 PRINT 1007,I   ,MA,MA
      K = K+1
  112 I = I+1
C7 LW
      DECODE (10,7,NH)LA
      IF (.NOT. LA) GO TO 113
      PRINT 1001,I
      GO TO 114
  113 PRINT 1008,I,LA,LA
      K = K+1
  114 I = I+1
C8 NP SCALE FACTOR
      DECODE (10,8,NI) FA
      IF (ABS(FA - 3.141592) .GT. EPS) GO TO 115
      PRINT 1001,I
      GO TO 116
  115 PRINT 1009,I,FA,FA
      K = K+1
  116 I = I+1
C9 NP SCALE FACTOR (-)
      DECODE (10,9,NI) FA
      IF (ABS(FA - 31415.92) .GT. EPS) GO TO 117
      PRINT 1001,I
      GO TO 118
  117 PRINT 1010,I,FA,FA
      K = K+1
  118 I = I+1
C10 WX
      DECODE (10,10,NL)MA
      IF (MA .NE. 4567890) GO TO 119
      PRINT 1001,I
      GO TO 120
  119 PRINT 1011,I,MA,MA
      K = K+1
  120 I = I+1
C11 WH
      DECODE (10,11,NM)
      ENCODE (10,11,NM1)
      IF (NM1 .NE.NM) GO TO 121
      PRINT 1001,I
      GO TO 122
  121 PRINT 1012,I,NM1,NM1
      K = K+1
  122 I = I+1
C HAS TO BE TESTED WITH ENCODE OR PRINT
C12 APOSTROPHE
      DECODE (10,12,NP)
      ENCODE (10,12,NP1)
      IF (NP1 .NE. NP) GO TO 123
      PRINT 1001,I
      GO TO 124
  123 PRINT 1013,I,NP1(1),NP1(1)
      K = K+1
  124 I = I+1
C SAME AS ABOVE
C13 APOSTROPHE 2
      DECODE (10,13,NQ)
      ENCODE (10,13,NQ1)
      IF (NQ1 .NE. NT) GO TO 125
      PRINT 1001,I
      GO TO 126
  125 PRINT 1014,I,NQ1(1),NQ1(1)
      K = K+1
  126 I = I+1
C14 DUMMY
      PRINT 999,I
      I=I+1
C15 SUBFIELDS COMBINATIONS FOR EWD
      DECODE (70,15,EL1) FA,FB,FC,FD,FE,FF,FG
      IF (ABS(FA-1.632E-4) .GT. EPS  .OR. ABS(FB+32.7216).GT.EPS
     *.OR. ABS(FC-3.E7) .GT.EPS .OR. ABS(FD-.0629).GT.EPS
     *.OR. ABS(FE-1.36E2).GT.EPS .OR. ABS(FF-8.E-2) .GT.EPS
     * .OR. ABS(FG) .GT.EPS ) GO TO 129
      PRINT 1001,I
      GO TO 130
  129 PRINT 1016,I,FA,FA,FB,FB,FC,FC,FD,FD,FE,FE
     * , FF,FF,FG,FG
      K = K+1
  130 I = I+1
C16 OVERRIDING D BY DECIMAL POINT
      DECODE (20,16,EL2) FA,FB
      IF (ABS(FA-FB) .GT. EPS) GO TO 131
      PRINT 1001,I
      GO TO 132
  131 PRINT 1017,I,FA,FA,FB,FB
      K = K+1
  132 I = I+1
C17 NO FRACTION SUBFIELD
      DECODE (10,17,EL3) FA
      IF (ABS(FA-.008936) .GT. EPS) GO TO 133
      PRINT 1001,I
      GO TO 134
  133 PRINT 1018,I,FA,FA
      K = K+1
  134 I = I+1
C18 ALL BLANKS IN INPUT FIELS
      DECODE (10,18,EL4) FA
      IF (ABS(FA) .GT. EPS) GO TO 135
      PRINT 1001,I
      GO TO 136
  135 PRINT 1019,I,FA,FA
      K = K+1
  136 I = I+1
C19 READING E UNDER F FORMAY
      DECODE (10,19,EL5) FA
      IF (ABS(FA-.14415) .GT. EPS) GO TO 137
      PRINT 1001,I
      GO TO 138
  137 PRINT 1020,I,FA,FA
      K = K+1
  138 I = I+1
C20 AW, W.GT.10
      DECODE (14,20,NAB) MA
      IF (MA .NE. NAA) GO TO 139
      PRINT 1001,I
      GO TO 140
  139 PRINT 1021,I,MA,MA
      K = K+1
  140 I = I+1
C21 LW WITH ALL BLANKS
      DECODE (10,21,EL4) LA
      IF (LA) GO TO 141
      PRINT 1001,I
      GO TO 142
  141 PRINT 1022,I,LA,LA
      K = K+1
  142 I = I+1
C PECULIAR TESTS ON DECODE
C22 TYPES OF SUBSCRIPT
      IS=2
      DECODE(2,22,NAC)NAD(1),NAD(IS),NAD(IS+1),NAD(2*IS),NAD(2*IS+1)
      IF( (NAD(1)+NAD(2)+NAD(3)+NAD(4)+NAD(5)).NE.65 ) GO TO 143
      PRINT 1001,I
      GO TO 144
  143 PRINT 1023, I,(NAD(IS),NAD(IS),IS=1,5)
      K = K+1
  144 I = I+1
      DECODE (   1,23,NAE)(NAD(IS),IS=1,5)
C23 IMPLIED DO LOOP
      IF( (NAD(1)+NAD(2)+NAD(3)+NAD(4)+NAD(5)).NE.15 ) GO TO 145
      PRINT 1001,I
      GO TO 146
  145 PRINT 1024, I,(NAD(IS),NAD(IS),IS=1,5)
      K = K+1
  146 I = I+1
C24 REPETITION AND DECODE
      DECODE(50,24,NAF)(NAD(IS),IS=1,5)
      IF ((NAD(1)+NAD(2)+NAD(3)) .NE. 6000   .OR.
     $   ABS(AD(1)+AD(2) - 3.14) .GT. EPS ) GO TO 147
      PRINT 1001,I
      GO TO 148
  147 PRINT 1025, I,(NAD(IS),NAD(IS),IS=1,5)
      K = K+1
  148 I = I+1
C25 RECORD ENDS IN THE MIDDLE OF A WORD
      DECODE (13,25,NAG) (FAC(IS),IS=1,3)
      IF (ABS(FAC(1)-123.8) .GT.EPS.OR.NAH .NE. 3
     $ .OR. ABS(FAC(3)-1.23) .GT. EPS) GO TO 149
      PRINT 1001,I
      GO TO 150
  149 PRINT 1026 ,I,(FAC(IS), FAC(IS),IS=1,3)
      K = K+1
  150 I = I+1
C26 DECODING ON THE SAME WORD
      DECODE (8,26,FAD) FAD
      IF (ABS(FAD-0.31) .GT. EPS) GO TO 151
      PRINT 1001,I
      GO TO 152
  151 PRINT 1027, I,FAD,FAD
      K = K+1
  152 I = I+1
C27 USING FUNCTION NAME AS VARIABLE IDENTIFIER IN ENCODE
      ARGUM = 100.
      FENC = FEN (ARGUM)
      DECODE (6,18,FENC) FDEC
      IF (FDEC .NE. 100.) GO TO 205
      PRINT 1001,I
      GO TO 206
  205 PRINT  1028,I,FDEC,FDEC
      K = K + 1
  206 I=I+1
C28 DUMMY
      PRINT 999,I
      I=I+1
C29 DUMMY
      PRINT 999,I
      I=I+1
C ENCODE STATEMENT TESTS
C ENCODE IS TESTED BY DECODE
C30 EW.D
      ENCODE (13,30,ZAB) ZAD
      DECODE (13,31,ZAB) ZAC
      IF ( ABS(ZAC+6732E-2) .GT. EPS ) GO TO 153
      PRINT 1001,I
      GO TO 154
  153 PRINT 1031,I,ZAC,ZAC
      K = K+1
  154 I = I+1
C31 EW
      ENCODE (6,32,ZAB) ZAA
      DECODE (10,33,ZAB) ZAC
      IF ( ABS(ZAC-6.E00) .GT. EPS ) GO TO 155
      PRINT 1001,I
      GO TO 156
  155 PRINT 1032,I,ZAC,ZAC
      K = K+1
  156 I = I+1
C32 ASTERISK FEATURE
      ENCODE (10,34,ZAB)ZAA
      IF  (LSTA .NE. ST00) GO TO 157
      PRINT 1001,I
      GO TO 158
  157 PRINT 1033,I,LSTA,LSTA
      K = K+1
  158 I = I+1
C33 RIGHT JUSTIFYING
      ENCODE (10,35,ZAB) ZAZ
      DECODE (10,36,ZAB) NBA,ZAC
      IF (NBA .NE. 0 .OR. ABS(ZAC-1.3) .GT. EPS ) GO TO 159
      PRINT 1001,I
      GO TO 160
  159 PRINT 1034,I,NBA,ZAC
      K = K+1
  160 I = I+1
C FW.D
C34 NORMAL
      ENCODE (10,37,ZAB)ZAZ
      DECODE (8,38,ZAB) ZAC
      IF ( ABS(ZAC-3.) .GT. EPS ) GO TO 161
      PRINT 1001,I
      GO TO 162
  161 PRINT 1035,I,ZAC,ZAC
      K = K+1
  162 I = I+1
C35 FW.D  JUSTIFYING
      ENCODE (10,39,ZAB) ZAD
      DECODE (10,40,ZAB) ZAC
      IF ( ABS(ZAC-7.3) .GT. EPS ) GO TO 163
      PRINT 1001,I
      GO TO 164
  163 PRINT 1036,I,ZAC,ZAC
      K = K+1
  164 I = I+1
C36 FW
        ENCODE (4,41,ZAB) ZAF
      DECODE (4,42,ZAB) ZAC
      IF (ABS(ZAC-36.) .GT. EPS) GO TO 165
       PRINT 1001,I
      GO TO 166
  165 PRINT 1037,I,ZAC,ZAC
        K = K+1
  166 I = I+1
C37 STAR FEATURE
      ENCODE (10,43,NBB) NBC
      IF ( NBB .NE. ISTARS) GO TO 167
      PRINT 1001,I
      GO TO 168
  167 PRINT 1038,I,NBB,NBB
      K = K+1
C DW.D
C38 NORMAL CASE
  168 I = I+1
      ENCODE (50,44,DDB) DDA
        DECODE (50,45,DDB) DDC
      IF ( DABS(DDC-DDA) .GT. DEPS ) GO TO 169
      PRINT 1001,I
      GO TO 170
  169  PRINT 1039,I,DDC,DPRI(1),DPRI(2)
      K = K+1
  170 I = I+1
C IW
C39
      ENCODE (9,46,NBE)NBD
      DECODE (10,47,NBE) NBF
      IF (NBF .NE. 3456) GO TO 171
      PRINT 1001,I
      GO TO 172
  171 PRINT 1040,I,NBF,NBF
      K = K+1
  172 I = I+1
C40 DISCARDING CHAR.
      ENCODE (10,48,NBE) NBD
      DECODE (10,49,NBE) NBF
      IF (NBF .NE. NBEF) GO TO 173
      PRINT 1001,I
      GO TO 174
  173 PRINT 1041,I,NBF,NBF
      K = K+1
  174 I = I+1
C  OW
C41
      ENCODE (6,50,NBE) NBG
      DECODE (6,51,NBE) NBF
      IF (NBF .NE.  334B)GO TO 175
      PRINT 1001,I
      GO TO 176
  175 PRINT 1042,I,NBF,NBF
      K = K + 1
  176 I = I+1
C42 DISCARDING
      ENCODE (6,52,NBE)NBG
      DECODE (6,51,NBE)NBF
      IF(NBF .NE. 34B)GO TO 177
      PRINT 1001,I
      GO TO 178
  177 PRINT 1043,I,NBF,NBF
      K = K+1
  178 I = I+1
C43 NEGATIVE OCTAL
      NBI=-NBG
C IT SHOULD LOOK LIKE NBH
      ENCODE (6,53,NBE)NBI
      DECODE (6,53,NBE)NBF
      IF ( NBF .NE. NBH ) GO TO 179
      PRINT 1001,I
      GO TO 180
  179 PRINT 1044,I,NBF,NBF
      K = K+1
  180 I = I+1
C44 AW
      ENCODE (10,60,NLB)NLA
      IF ( NLB .NE. NLC ) GO TO 181
      PRINT 1001,I
      GO TO 182
  181 PRINT 1045,I,NLB,NLB
      K = K+1
  182 I = I+1
C45 RECORD .GT. 10
      ENCODE (14,61,NLH)NLD(1),NLD(2)
      IF ( NLH .NE. NLF ) GO TO 183
      PRINT 1001,I
      GO TO 184
  183 PRINT 1046,I,NLH,NLH
      K = K+1
  184 I = I+1
C LOGICAL
C46 NORMAL
      ENCODE (10,62,NLW) NLS,NLT
      IF(NLW(1) .NE. NLU .OR. NLW(3) .NE. NLV)GO TO 185
      PRINT 1001,I
      GO TO 186
  185 PRINT 1047,I,NLW(1),NLW(1),NLW(3),NLW(3)
      K = K+1
  186 I = I+1
C47  NH
      ENCODE (8,63,NNA)
      IF (NNA .NE. NNB) GO TO 187
      PRINT 1001,I
      GO TO 188
  187 PRINT 1048,I, NNA,NNA
      K = K+1
  188 I = I+1
C48 APOSTROPHE
      ENCODE (10,64,NNA)
      IF (NNA .NE. NNC) GO TO 189
      PRINT 1001,I
      GO TO 190
  189 PRINT 1049,I, NNA,NNA
      K = K+1
  190 I = I+1
C49 NP SCALE FACTOR
      ENCODE (13,70,FGB)FGA,FGA
      DECODE (13,71,FGB)FGC,FGD
      IF (ABS(FGC-31.415927  ).GT.EPS     .OR.
     $   ABS(FGD - .314159) .GT. EPS ) GO TO 191
      PRINT 1001,I
      GO TO 192
  191 PRINT 1050,I, FGC,FGC,FGD,FGD
      K = K+1
  192 I = I+1
C50 NP SCALE FACTOR EW.D
      ENCODE (13,72,FGB)FGA,FGA
      DECODE (13,73,FGB)FGC,FGD
      IF (ABS (FGC-31415.93E-04) .GT. EPS      .OR.
     $   ABS(FGD - 0.31E+01) .GT. EPS ) GO TO 193
      PRINT 1001,I
      GO TO 194
  193 PRINT 1051,I, FGC,FGC,FGD,FGD
      K = K+1
  194 I = I+1
C51 ENCODING IN THE SAME WORD
      ENCODE(5,74,FGA)FGA
      IF (NNE .NE. NND) GO TO 195
      PRINT 1001,I
      GO TO 196
  195 PRINT 1052,I, NNE,NNE
      K = K+1
  196 I = I+1
C52 TESTING CASE 11
      ENCODE (10,11,NUB)
      IF (NUB .NE. NN) GO TO 197
      PRINT 1001,I
      GO TO 198
  197 PRINT 1053,I, NUB,NUB
      K = K+1
  198 I = I+1
C53  TESTING CASE 12
      ENCODE (10,12,NUB)
      IF (NUB .NE. NO) GO TO 199
      PRINT 1001,I
      GO TO 200
  199 PRINT 1054,I,NUB(1),NUB(1)
      K = K+1
  200 I = I+1
C54 TESTING CASE 13
      ENCODE (10,13,NUB)
      IF (NUB .NE. NR) GO TO 201
      PRINT 1001,I
      GO TO 202
  201 PRINT 1055,I,NUB(1),NUB(1)
      K = K+1
  202 CONTINUE
C   ** SUMMARY  AND DUMP
C
  499 INKR = I-K
      IF (K .EQ. 0) GO TO 500
      IF (INKR .EQ.0) GO TO 501
      PRINT 510,I,INKR,K
      PRINT 513
      CALL PDUMP (ANF,FIN,3,FIN,FEN,0,FEN,DUM08,3)
      GO TO 502
  500 PRINT 511,I
      GO TO 502
  501 PRINT 512,I
      PRINT 513
      CALL PDUMP (ANF,FIN,3,FIN,FEN,0,FEN,DUM08,3)
  502 CONTINUE
    1 FORMAT(E10.1)
    2 FORMAT(F10.4)
    3 FORMAT(D10.1)
    4 FORMAT(I10)
    5 FORMAT(O10)
    6 FORMAT(A10)
    7 FORMAT(L10)
    8 FORMAT(2PF10.4)
    9 FORMAT(-2PF10.4)
   10 FORMAT(3XI7)
   11 FORMAT(10H          )
   12 FORMAT('          ')
   13 FORMAT('          ')
   14 FORMAT(I10,//F10.3)
   15 FORMAT (E10.3,E10.4,E10.0,E10.3,E10.0,E10.8,E10.0)
   16 FORMAT(E10.2,E10.5)
   17 FORMAT(E9.10 )
   18 FORMAT(F6.2)
   19 FORMAT(F10.2)
   20 FORMAT(A14)
   21 FORMAT(L10)
   22 FORMAT (I2)
   23 FORMAT (I1)
   24 FORMAT(3(I4,2X),2(F4.2,2X))
   25 FORMAT (4X,F5.1,2X,I2/9XF4.2)
   26 FORMAT(2X,E6.)
   30 FORMAT (E13.3)
   31 FORMAT (3X,E10.3)
   32 FORMAT (E6.)
   33 FORMAT (E6.,4X)
   34 FORMAT (E3.1,7X)
   35 FORMAT (E10.1)
   36 FORMAT (I3,E7.)
   37 FORMAT (2X,F6.2,2X)
   38 FORMAT (6X,F1.0,1X)
   39 FORMAT (F10.1)
   40 FORMAT (7X,F3.1)
   41 FORMAT (F4)
   42 FORMAT (2X,E2.0)
   43 FORMAT (F10.7)
   44 FORMAT (D50.29)
   45 FORMAT (15XD35.29)
   46 FORMAT (I9)
   47 FORMAT (I9,1X)
   48 FORMAT (8XI2)
C
   49 FORMAT (6X,A4)
C
   50 FORMAT (1XO5)
   51 FORMAT (O6)
   52 FORMAT (4XO2)
   53 FORMAT (O6)
   60 FORMAT (5XA5)
   61 FORMAT (A6,A4)
   62 FORMAT(         L5  / L2  )
   63 FORMAT (5H BBBB)
   64 FORMAT (2X,'ABC',2X,'ABC')
   70 FORMAT (1PF13.6/-1PF13.6)
   71 FORMAT (2XF11.6/6XF7.6)
   72 FORMAT(4PE13.2/-1PE13.2)
   73 FORMAT (1XE12.2/5XE8.2)
   74 FORMAT (F4.2)
  510 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' CHECKS DONE..................',I2,'   (INCLUDING DUM
     *MY CHECKS)'/
     3            ' CHECKS OKAY..................',I2/
     4            ' CHECKS WRONG.................',I2)
  511 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' ALL ',I2,' CHECKS ARE OKAY')
  512 FORMAT (////' **********  SUMMARY  **********'/
     1            '             -------'//
     2            ' ALL ',I2,' CHECKS ARE WRONG')
  513 FORMAT ('1',34(/),30X,10('*'),'   DUMP IN CASE OF FAILING TEST(S)
     *  ',10('*') )
C  **
C  **
C  **  OUTPUT FORMATS TO PRINT RESULTS OF CHECKS
C  **
C  **
  999 FORMAT ('    I= ',I2,'  DUMMY CHECK')
C  **
 1000 FORMAT (   50X,30H ***************************    /
     *           50X,30H *I-----------------------I*    /
     *        2( 50X, 3H *I  ,23X,              2HI*    /  ),
     *           50X,30H *I COMPILER TEST PROGRAM I*    /
     *        2( 50X, 3H *I  ,23X,              2HI*    /  ),
     *           50X,30H *I-----------------------I*    /
     *           50X,30H ***************************    ///
     $               18H THIS PART CHECKS
     $ 'ENCODE- AND DECODE-STATEMENTS'////'    OKAY            *****  ER
     *ROR IN  ******************'/ ' CHECK NO.          *****  CHECK NO.
     *     CALCULATED VALUES             EXPECTED VALUES'/87('-')///)
 1001 FORMAT ('    I= ',I2)
 1002 FORMAT(/20X,'*****     I= ',I2,6X,'FA =',F10.4,16X,'    3.1415'/
     *        41X,O20,' (OCTAL)'/)
 1003 FORMAT(/20X,'*****     I= ',I2,6X,'FA =',F10.4,16X,'    3.1415'/
     *        41X,O20,' (OCTAL)'/)
 1004 FORMAT(/20X,'*****     I= ',I2,6X,'DA =',D10.1,16X,'   0.3D-41'/
     *        41X,O20,' (OCTAL)'/)
 1005 FORMAT(/20X,'*****     I= ',I2,6X,'MA =',I10  ,16X,'       325'/
     *        41X,O20,' (OCTAL)'/)
 1006 FORMAT(/20X,'*****     I= ',I2,6X,'MA =',I10  ,16X,' 64 = 100B'/
     *        41X,O20,' (OCTAL)'/)
 1007 FORMAT(/20X,'*****     I= ',I2,6X,'MA =',A10  ,16X,'AAAB      '/
     *        41X,O20,' (OCTAL)'/)
 1008 FORMAT(/20X,'*****     I= ',I2,6X,'LA =',L10  ,16X,'     T    '/
     *        41X,O20,' (OCTAL)'/)
 1009 FORMAT(/20X,'*****     I= ',I2,6X,'FA =',F10.6,16X,'  3.141592'/
     *        41X,O20,' (OCTAL)'/)
 1010 FORMAT(/20X,'*****     I= ',I2,6X,'FA =',F10.2,16X,'  31415.92'/
     *        41X,O20,' (OCTAL)'/)
 1011 FORMAT(/20X,'*****     I= ',I2,6X,'MA =',I10  ,16X,'   4567890'/
     *        41X,O20,' (OCTAL)'/)
 1012 FORMAT(/20X,'*****     I= ',I2,6X,'NM1=',A10  ,16X,'A        A'/
     *        41X,O20,' (OCTAL)'/)
 1013 FORMAT(/20X,'*****     I= ',I2,6X,'NP1=',A10  ,16X,'B       BB'/
     *        41X,O20,' (OCTAL)'/)
 1014 FORMAT(/20X,'*****     I= ',I2,6X,'NQ1=',A10  ,16X,'BBBB      '/
     *        41X,O20,' (OCTAL)'/)
 1016 FORMAT(/20X'*****     I='I3
     1  6X'FA='E10.3,18X'1.632-04'/41X,O20' (OCTAL)'/
     2 41X'FB='F10.4,18X'-32.7216'/41X,O20' (OCTAL)'/
     3 41X'FC='E10.0,18X'    3+07'/41X,O20' (OCTAL)'/
     4 41X'FD='F10.4,18X'  0.0629'/41X,O20' (OCTAL)'/
     5 41X'FE='E10.2,18X' 1.36+02'/41X,O20' (OCTAL)'/
     6 41X'FF='E10.0,18X'    8-02'/41X,O20' (OCTAL)'/
     7 41X'FG='E10.0,18X'    1-06'/41X,O20' (OCTAL)'/ )
 1017 FORMAT(/20X,'*****     I= ',I2,6X,'FA =',E12.5,14X,' 3.67294+5'/
     $        41X,O20,' (OCTAL)'/
     1        41X,                      'FB =',E12.5,14X,' 3.67294+5'/
     *        41X,O20,' (OCTAL)'/)
 1018 FORMAT(/20X,'*****     I= ',I2,6X,'FA =',F10.6,16X,'   .008936'/
     *        41X,O20,' (OCTAL)'/)
 1019 FORMAT(/20X,'*****     I= ',I2,6X,'FA =',E10.3,16X,'  0.000E+0'/
     *        41X,O20,' (OCTAL)'/)
 1020 FORMAT(/20X,'*****     I= ',I2,6X,'FA =',F10.5,16X,'   -.14415'/
     *        41X,O20,' (OCTAL)'/)
 1021 FORMAT(/20X,'*****     I= ',I2,6X,'MA =',A10  ,16X,'AAAAAABBBB'/
     *        41X,O20,' (OCTAL)'/)
 1022 FORMAT(/20X,'*****     I= ',I2,6X,'LA =',L10  ,16X,'        F'/
     *        41X,O20,' (OCTAL)'/)
 1023 FORMAT(/20X,'*****     I= ',I2,6X,'NAD(1)= ',I10,13X,'        11'/
     $        41X,O20,' (OCTAL)'/
     1        41X,                      'NAD(2)= ',I10,13X,'        12'/
     $        41X,O20,' (OCTAL)'/
     2        41X,                      'NAD(3)= ',I10,13X,'        13'/
     $        41X,O20,' (OCTAL)'/
     3        41X,                      'NAD(4)= ',I10,13X,'        14'/
     $        41X,O20,' (OCTAL)'/
     4        41X,                      'NAD(5)= ',I10,13X,'        15'/
     *        41X,O20,' (OCTAL)'/)
 1024 FORMAT(/20X,'*****     I= ',I2,6X,'NAD(1)= ',I10,13X,'         1'/
     $        41X,O20,' (OCTAL)'/
     1        41X,                      'NAD(2)= ',I10,13X,'         2'/
     $        41X,O20,' (OCTAL)'/
     2        41X,                      'NAD(3)= ',I10,13X,'         3'/
     $        41X,O20,' (OCTAL)'/
     3        41X,                      'NAD(4)= ',I10,13X,'         4'/
     $        41X,O20,' (OCTAL)'/
     4        41X,                      'NAD(5)= ',I10,13X,'         5'/
     *        41X,O20,' (OCTAL)'/)
 1025 FORMAT(/20X,'*****     I= ',I2,6X,'NAD(1)= ',I10,13X,'      1000'/
     $        41X,O20,' (OCTAL)'/
     1        41X,                      'NAD(2)= ',I10,13X,'      2000'/
     $        41X,O20,' (OCTAL)'/
     2        41X,                      'NAD(3)= ',I10,13X,'      3000'/
     $        41X,O20,' (OCTAL)'/
     3        41X,                       'AD(1)= ',F10.2,13X,'      0.00
     *'/      41X,O20,' (OCTAL)'/
     4        41X,                       'AD(2)= ',F10.2,13X,'      3.14
     *        41X,O20,' (OCTAL)'/)
 1026 FORMAT(/20X,'*****     I= ',I2,6X,'FAC(1)= ',F10.1,13X,'      123.
     $8'/     41X,O20,' (OCTAL)'/
     1        41X                      ,'FAC(2)= ',I10,13X,'        03'/
     $        41X,O20,' (OCTAL)'/
     2        41X                      ,'FAC(3)= ',F10.2,13X,'      1.23
     *        41X,O20,' (OCTAL)'/)
 1027 FORMAT(/20X,'*****     I= ',I2,6X,'FAD=',F10.2,16X,'       .31'/
     *        41X,O20,' (OCTAL)'/)
 1028 FORMAT(/20X,'*****     I= ',I2,6X,'FENC=',E10.3,15X,'      100.'/
     *        41X,O20,' (OCTAL)'/)
 1031 FORMAT(/20X,'*****     I= ',I2,6X,'ZAC=',E10.3,16X,'  -6732E-2'/
     *        41X,O20,' (OCTAL)'/)
 1032 FORMAT(/20X,'*****     I= ',I2,6X,'ZAC=',E10. ,16X,'     6.E00'/
     *        41X,O20,' (OCTAL)'/)
 1033 FORMAT(/20X,'*****     I= ',I2,6X,'LSTA=',A10 ,15X,'***       '/
     *        41X,O20,' (OCTAL)'/)
 1034 FORMAT(/20X,'*****     I= ',I2,6X,'NBA=',I10  ,16X,'       000'/
     $        41X,O20,' (OCTAL)'/
     1        41X                      ,'ZAC=',E10.0,16X,'    13.E-1'/
     *        41X,O20,' (OCTAL)'/)
 1035 FORMAT(/20X,'*****     I= ',I2,6X,'ZAC=',E10.0,16X,'        3.'/
     *        41X,O20,' (OCTAL)'/)
 1036 FORMAT(/20X,'*****     I= ',I2,6X,'ZAC=',E10.1,16X,'       7.3'/
     *        41X,O20,' (OCTAL)'/)
 1037 FORMAT(/20X,'*****     I= ',I2,6X,'ZAC=',E10.2,16X,'     36.  '/
     *        41X,O20,' (OCTAL)'/)
 1038 FORMAT(/20X,'*****     I= ',I2,6X,'NBB=',A10  ,16X,'**********'/
     *        41X,O20,' (OCTAL)'/)
 1039 FORMAT(/20X,'*****     I= ',I2,6X,'DDC=',D35.29,6X,'.3333333333333
     +3333333333333333D-205'/41X,2O20,' (OCTAL)'/)
 1040 FORMAT(/20X,'*****     I= ',I2,6X,'NBF=',I10  ,16X,'      3456'/
     *        41X,O20,' (OCTAL)'/)
 1041 FORMAT(/20X,'*****     I= ',I2,6X,'NBF=',A10  ,16X,'  *6      '/
     *        41X,O20,' (OCTAL)'/)
 1042 FORMAT(/20X,'*****     I= ',I2,6X,'NBF=',I10  ,16X,'       220'/
     *        41X,O20,' (OCTAL)'/)
 1043 FORMAT(/20X,'*****     I= ',I2,6X,'NBF=',I10  ,16X,'        28'/
     *        41X,O20,' (OCTAL)'/)
 1044 FORMAT(/20X,'*****     I= ',I2,6X,'NBF=',I10  ,16X,'      -220'/
     *        41X,O20,' (OCTAL)'/)
 1045 FORMAT(/20X,'*****     I= ',I2,6X,'NLB=',A10  ,16X,'     NONNA'/
     *        41X,O20,' (OCTAL)'/)
 1046 FORMAT(/20X,'*****     I= ',I2,6X,'NLH=',A10  ,16X,'     M MIA'/
     *        41X,O20,' (OCTAL)'/)
 1047 FORMAT(/20X,'*****     I= ',I2,6X,'NLW(1)=',L10,13X,'    F     '/
     $        41X,O20,' (OCTAL)'/
     1              41X,                'NLW(2)=',L10,13X,' T        '/
     *        41X,O20,' (OCTAL)'/)
 1048 FORMAT(/20X,'*****     I= ',I2,6X,'NNA=',A10  ,16X,' BBBB     '/
     *        41X,O20,' (OCTAL)'/)
 1049 FORMAT(/20X,'*****     I= ',I2,6X,'NNA=',A10  ,16X,'  ABC  ABC'/
     *        41X,O20,' (OCTAL)'/)
 1050 FORMAT(20X,'*****     I= ',I2,6X,'FGC=',F13.6,13X,'+31.415926'/
     $        41X,O20,' (OCTAL)'/
     1        41X,                      'FGD=',F13.6,13X,'   .314159'/
     *        41X,O20,' (OCTAL)'/)
 1051 FORMAT(/20X,'*****     I= ',I2,6X,'FGC=',F13.2,13X,'31415.935-04'/
     $        41X,O20,' (OCTAL)'/
     1           41X,                   'FGD=',F13.2,13X,'  0.31E+01'/
     *        41X,O20,' (OCTAL)'/)
 1052 FORMAT(/20X,'*****     I= ',I2,6X,'NNE=',F10.2,16X,'      3.14'/
     *        41X,O20,' (OCTAL)'/)
 1053 FORMAT(/20X,'*****     I= ',I2,6X,'NUB=',A10  ,16X,'A        A'/
     *        41X,O20,' (OCTAL)'/)
 1054 FORMAT(/20X,'*****     I= ',I2,6X,'NUB=',A10  ,16X,'B       BB'/
     *        41X,O20,' (OCTAL)'/)
 1055 FORMAT(/20X,'*****     I= ',I2,6X,'NUB=',A10  ,16X,'BBBB      '/
     *        41X,O20,' (OCTAL)'/)
      ENTRY ENCDE1
      RETURN
      END
      FUNCTION FEN (X)
      ENCODE(6,18,FEN)X
       I=1
      RETURN
   18 FORMAT(F6.2)
       END
      SUBROUTINE DUM08
       I=1
      RETURN
      END
*EXECUTE
