*
      PROGRAM F002 A
      DIMENSION X(6),Z(3),N(16)
      DATA X/-5.,1.,1.,4.,0.,2./
      S=10.
      N(1)=5HRVSET
      CALL RVSET(3,S,Z(1),Z(2))
      PRINT 1,N(1),Z
      N(2)=5HRVRAN
      CALL RVRAN(3,0.,1.,Z(1),Z(2))
      PRINT 1,N(2),Z
      N(3)=5HRVCPY
      CALL RVCPY(3,X(1),X(3),Z(1),Z(2))
      PRINT 1,N(3),Z
      N(4)=5HRVXCH
      CALL RVXCH(3,X(1),X(3),X(2),X(4))
      PRINT 2,N(4),X
      N(5)=5HRVADD
      CALL RVADD(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(5),Z
      N(6)=5HRVSUB
      CALL RVSUB(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(6),Z
      N(7)=5HRVMUL
      CALL RVMUL(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(7),Z
      N(8)=6HRVMULA
      CALL RVMULA(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(8),Z
      N(9)=6HRVMUNA
      CALL RVMUNA(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(9),Z
      N(10)=5HRVDIV
      CALL RVDIV(3,X(1),X(3),X(2),X(4),Z(1),Z(2),IFAIL)
      PRINT 4,N(10),Z,IFAIL
      N(11)=5HRVSCL
      CALL RVSCL(3,S,X(1),X(2),Z(1),Z(2))
      PRINT 1,N(11),Z
      N(12)=5HRVSCA
      CALL RVSCA(3,S,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(12),Z
      N(13)=5HRVSCS
      CALL RVSCS(3,S,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(13),Z
      N(14)=5HRVSUM
      F=RVSUM(3,X(1),X(3))
      PRINT 3,N(14),F
      N(15)=5HRVMPY
      F=RVMPY(3,X(1),X(3),X(2),X(4))
      PRINT 3,N(15),F
      N(16)=5HRVMPA
      F=RVMPA(3,X(1),X(3),X(2),X(4),S)
      PRINT 3,N(16),F
 1    FORMAT(3X,A6,10X,2HZ=,3E20.11)
 2    FORMAT(3X,A6,5X,2HX=,6E17.9)
 3    FORMAT(3X,A6,10X,2HF=,E20.11)
 4    FORMAT(3X,A6,10X,2HZ=,3E20.11,5X,6HIFAIL=,I2)
      CALL EXIT
      END
*EXECUTE
      PROGRAM F002 B
      DIMENSION X(6),Z(3),N(16)
      DOUBLE PRECISION X,Z,S,F,DVMPY,DVMPA,DVSUM
      DATA X/-5.D0,1.D0,1.D0,4.D0,0.D0,2.D0/
      S=10.D0
      N(1)=5HDVSET
      CALL DVSET(3,S,Z(1),Z(2))
      PRINT 1,N(1),Z
      N(2)=5HDVRAN
      CALL DVRAN(3,0.D0,1.D0,Z(1),Z(2))
      PRINT 1,N(2),Z
      N(3)=5HDVCPY
      CALL DVCPY(3,X(1),X(3),Z(1),Z(2))
      PRINT 1,N(3),Z
      N(4)=5HDVXCH
      CALL DVXCH(3,X(1),X(3),X(2),X(4))
      PRINT 2,N(4),X
      N(5)=5HDVADD
      CALL DVADD(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(5),Z
      N(6)=5HDVSUB
      CALL DVSUB(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(6),Z
      N(7)=5HDVMUL
      CALL DVMUL(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(7),Z
      N(8)=6HDVMULA
      CALL DVMULA(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(8),Z
      N(9)=6HDVMUNA
      CALL DVMUNA(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(9),Z
      N(10)=5HDVDIV
      CALL DVDIV(3,X(1),X(3),X(2),X(4),Z(1),Z(2),IFAIL)
      PRINT 4,N(10),Z,IFAIL
      N(11)=5HDVSCL
      CALL DVSCL(3,S,X(1),X(2),Z(1),Z(2))
      PRINT 1,N(11),Z
      N(12)=5HDVSCA
      CALL DVSCA(3,S,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(12),Z
      N(13)=5HDVSCS
      CALL DVSCS(3,S,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(13),Z
      N(14)=5HDVSUM
      F=DVSUM(3,X(1),X(3))
      PRINT 3,N(14),F
      N(15)=5HDVMPY
      F=DVMPY(3,X(1),X(3),X(2),X(4))
      PRINT 3,N(15),F
      N(16)=5HDVMPA
      F=DVMPA(3,X(1),X(3),X(2),X(4),S)
      PRINT 3,N(16),F
 1    FORMAT(3X,A6,10X,2HZ=,3D30.17)
 2    FORMAT(3X,A6,5X,2HX=,6D17.9)
 3    FORMAT(3X,A6,10X,2HF=,D30.17)
 4    FORMAT(3X,A6,10X,2HZ=,3D25.17,5X,6HIFAIL=,I2)
      CALL EXIT
      END
*EXECUTE
      PROGRAM F002 C
      DIMENSION X(6),Z(3),N(16)
         COMPLEX   X,Z,S,A,B,F,CVMPY,CVMPA,CVSUM
      DATA X/-5.,1.,1.,2.,1.,1.,4.,4.,0.,1.,2.,3./,  S/10.,0./
      DATA A/0.,0./, B/1.,0./
      N(1)=5HCVSET
      CALL CVSET(3,S,Z(1),Z(2))
      PRINT 1,N(1),Z
      N(2)=5HCVRAN
      CALL CVRAN(3,A,B,Z(1),Z(2))
      PRINT 1,N(2),Z
      N(3)=5HCVCPY
      CALL CVCPY(3,X(1),X(3),Z(1),Z(2))
      PRINT 1,N(3),Z
      N(4)=5HCVXCH
      CALL CVXCH(3,X(1),X(3),X(2),X(4))
      PRINT 2,N(4),X
      N(5)=5HCVADD
      CALL CVADD(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(5),Z
      N(6)=5HCVSUB
      CALL CVSUB(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(6),Z
      N(7)=5HCVMUL
      CALL CVMUL(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(7),Z
      N(8)=6HCVMULA
      CALL CVMULA(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(8),Z
      N(9)=6HCVMUNA
      CALL CVMUNA(3,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(9),Z
      N(10)=5HCVDIV
      CALL CVDIV(3,X(1),X(3),X(2),X(4),Z(1),Z(2),IFAIL)
      PRINT 4,N(10),Z,IFAIL
      N(11)=5HCVSCL
      CALL CVSCL(3,S,X(1),X(2),Z(1),Z(2))
      PRINT 1,N(11),Z
      N(12)=5HCVSCA
      CALL CVSCA(3,S,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(12),Z
      N(13)=5HCVSCS
      CALL CVSCS(3,S,X(1),X(3),X(2),X(4),Z(1),Z(2))
      PRINT 1,N(13),Z
      N(14)=5HCVSUM
      F=CVSUM(3,X(1),X(3))
      PRINT 3,N(14),F
      N(15)=5HCVMPY
      F=CVMPY(3,X(1),X(3),X(2),X(4))
      PRINT 3,N(15),F
      N(16)=5HCVMPA
      F=CVMPA(3,X(1),X(3),X(2),X(4),S)
      PRINT 3,N(16),F
 1    FORMAT(3X,A6,10X,2HZ=,6E14.7)
 2    FORMAT(1X,A6,1X,2HX=,12E9.2)
 3    FORMAT(3X,A6,10X,2HF=,2E14.7)
 4    FORMAT(3X,A6,10X,2HZ=,6E14.7,3X,6HIFAIL=,I2)
      STOP
      END
*EXECUTE
*
*
      PROGRAM F003 A
      DIMENSION X(2,3),Z(2,3),Y(2,3),D(2),U(3,3),V(3),N(19)
      DATA X/1.5,0.5,0.5,2.,0.2,0.4/,Y/1.5,1.5,0.2,0.7,0.5,0.1/
      DATA A,B/0.,1./,D/2.,3./,U/1.,0.,0.,2.,4.,0.,3.,5.,0./,S/10./
      DATA V/1.,2.,3./
      N(1)=5HRMSET
      CALL RMSET(2,3,S,Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(1),((Z(I,J),J=1,3),I=1,2)
      N(2)=5HRMRAN
      CALL RMRAN(2,3,A,B,Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(2),((Z(I,J),J=1,3),I=1,2)
      N(3)=5HRMCPY
      CALL RMCPY(2,3,X(1,1),X(1,2),X(2,1),Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(3),((Z(I,J),J=1,3),I=1,2)
      N(4)=5HRMUTL
      CALL RMUTL(3,U(1,1),U(1,2),U(2,1))
      PRINT 1,N(4),((U(K,L),L=1,3),K=1,3)
      N(5)=5HRMADD
      CALL RMADD(2,3,X(1,1),X(1,2),X(2,1),Y(1,1),Y(1,2),Y(2,1),Z(1,1),
     *           Z(1,2),Z(2,1))
      PRINT 1,N(5),((Z(I,J),J=1,3),I=1,2)
      N(6)=5HRMSUB
      CALL RMSUB(2,3,X(1,1),X(1,2),X(2,1),Y(1,1),Y(1,2),Y(2,1),Z(1,1),
     *           Z(1,2),Z(2,1))
      PRINT 1,N(6),((Z(I,J),J=1,3),I=1,2)
      N(7)=5HRRSCL
      CALL RRSCL(2,3,D(1),D(2),X(1,1),X(1,2),X(2,1),Z(1,1),Z(1,2),
     *           Z(2,1))
      PRINT 1,N(7),((Z(I,J),J=1,3),I=1,2)
      N(8)=5HRMSCL
      CALL RMSCL(2,3,S,X(1,1),X(1,2),X(2,1),Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(8),((Z(I,J),J=1,3),I=1,2)
      N(9)=5HRMMPY
      CALL RMMPY(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(9),(Z(I),I=1,2)
      N(10)=5HRMMPA
      CALL RMMPA(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(10),(Z(I),I=1,2)
      N(11)=5HRMMPS
      CALL RMMPS(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(11),(Z(I),I=1,2)
      N(12)=5HRMMNA
      CALL RMMNA(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(12),(Z(I),I=1,2)
      N(13)=5HRMMNS
      CALL RMMNS(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(13),(Z(I),I=1,2)
      N(14)=5HRUMPY
      CALL RUMPY(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(14),(Z(J),J=1,3)
      N(15)=5HRUMPA
      CALL RUMPA(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(15),(Z(J),J=1,3)
      N(16)=5HRUMPS
      CALL RUMPS(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(16),(Z(J),J=1,3)
      N(17)=5HRUMNA
      CALL RUMNA(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(17),(Z(J),J=1,3)
      N(18)=5HRUMNS
      CALL RUMNS(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(18),(Z(J),J=1,3)
      N(19)=5HRMBIL
      F=RMBIL(3,V(1),V(2),U(1,1),U(1,2),U(2,1),Y(1),Y(2))
      PRINT 2,N(19),F
 1    FORMAT(15X,A6/(3X,3E20.10)/)
 2    FORMAT(15X,A6/ 3X, E20.10)
 3    FORMAT(15X,A6/(3X,2E20.10))
 4    FORMAT(15X,A6/(3X,3E20.10))
      CALL EXIT
      END
*EXECUTE
      PROGRAM F003 B
      DIMENSION        X(2,3),Z(2,3),Y(2,3),D(2),U(3,3),V(3),N(19)
      DOUBLE PRECISION X,Z,Y,D,U,V,A,B,S,F,DMBIL
      DATA X/1.5D0,0.5D0,0.5D0,2.D0,0.2D0,0.4D0/,A,B,S/0.D0,1.D0,10.D0/
      DATA Y/1.5D0,1.5D0,0.2D0,0.7D0,0.5D0,0.1D0/,D/2.D0,3.D0/
      DATA U/1.D0,0.D0,0.D0,2.D0,4.D0,0.D0,3.D0,5.D0,0.D0/
      DATA V/1.D0,2.D0,3.D0/
      N(1)=5HDMSET
      CALL DMSET(2,3,S,Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(1),((Z(I,J),J=1,3),I=1,2)
      N(2)=5HDMRAN
      CALL DMRAN(2,3,A,B,Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(2),((Z(I,J),J=1,3),I=1,2)
      N(3)=5HDMCPY
      CALL DMCPY(2,3,X(1,1),X(1,2),X(2,1),Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(3),((Z(I,J),J=1,3),I=1,2)
      N(4)=5HDMUTL
      CALL DMUTL(3,U(1,1),U(1,2),U(2,1))
      PRINT 1,N(4),((U(K,L),L=1,3),K=1,3)
      N(5)=5HDMADD
      CALL DMADD(2,3,X(1,1),X(1,2),X(2,1),Y(1,1),Y(1,2),Y(2,1),Z(1,1),
     *           Z(1,2),Z(2,1))
      PRINT 1,N(5),((Z(I,J),J=1,3),I=1,2)
      N(6)=5HDMSUB
      CALL DMSUB(2,3,X(1,1),X(1,2),X(2,1),Y(1,1),Y(1,2),Y(2,1),Z(1,1),
     *           Z(1,2),Z(2,1))
      PRINT 1,N(6),((Z(I,J),J=1,3),I=1,2)
      N(7)=5HDRSCL
      CALL DRSCL(2,3,D(1),D(2),X(1,1),X(1,2),X(2,1),Z(1,1),Z(1,2),
     *           Z(2,1))
      PRINT 1,N(7),((Z(I,J),J=1,3),I=1,2)
      N(8)=5HDMSCL
      CALL DMSCL(2,3,S,X(1,1),X(1,2),X(2,1),Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(8),((Z(I,J),J=1,3),I=1,2)
      N(9)=5HDMMPY
      CALL DMMPY(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(9),(Z(I),I=1,2)
      N(10)=5HDMMPA
      CALL DMMPA(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(10),(Z(I),I=1,2)
      N(11)=5HDMMPS
      CALL DMMPS(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(11),(Z(I),I=1,2)
      N(12)=5HDMMNA
      CALL DMMNA(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(12),(Z(I),I=1,2)
      N(13)=5HDMMNS
      CALL DMMNS(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(13),(Z(I),I=1,2)
      N(14)=5HDUMPY
      CALL DUMPY(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(14),(Z(J),J=1,3)
      N(15)=5HDUMPA
      CALL DUMPA(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(15),(Z(J),J=1,3)
      N(16)=5HDUMPS
      CALL DUMPS(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(16),(Z(J),J=1,3)
      N(17)=5HDUMNA
      CALL DUMNA(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(17),(Z(J),J=1,3)
      N(18)=5HDUMNS
      CALL DUMNS(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(18),(Z(J),J=1,3)
      N(19)=5HDMBIL
      F=DMBIL(3,V(1),V(2),U(1,1),U(1,2),U(2,1),Y(1),Y(2))
      PRINT 2,N(19),F
 1    FORMAT(15X,A6/(3X,3D25.16)/)
 2    FORMAT(15X,A6/ 3X, D25.16)
 3    FORMAT(15X,A6/(3X,2D25.16))
 4    FORMAT(15X,A6/(3X,3D25.16))
      CALL EXIT
      END
*EXECUTE
      PROGRAM F003 C
      DIMENSION        X(2,3),Z(2,3),Y(2,3),D(2),U(3,3),V(3),N(19)
      COMPLEX          X,Z,Y,D,U,V,A,B,S,F,CMBIL
      DATA X/1.5,1.,0.5,2.,0.5,2.,2.,0.,0.2,0.5,0.4,0./,D/2.,1.,3.,0.5/
      DATA Y/1.5,1.,1.5,0.5,0.2,01.,0.7,1.,0.5,0.,0.1,0.5/
      DATA U/1.,1.,0.,0.,0.,0.,2.,0.1,4.,0.5,0.,0.,3.,0.,5.,0.5,0.,0./
      DATA V/1.,1.,2.,0.5,3.,1./,A,B,S/0.,0.,1.,0.,10.,0./
      N(1)=5HCMSET
      CALL CMSET(2,3,S,Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(1),((Z(I,J),J=1,3),I=1,2)
      N(2)=5HCMRAN
      CALL CMRAN(2,3,A,B,Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(2),((Z(I,J),J=1,3),I=1,2)
      N(3)=5HCMCPY
      CALL CMCPY(2,3,X(1,1),X(1,2),X(2,1),Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(3),((Z(I,J),J=1,3),I=1,2)
      N(4)=5HCMUTL
      CALL CMUTL(3,U(1,1),U(1,2),U(2,1))
      PRINT 1,N(4),((U(K,L),L=1,3),K=1,3)
      N(5)=5HCMADD
      CALL CMADD(2,3,X(1,1),X(1,2),X(2,1),Y(1,1),Y(1,2),Y(2,1),Z(1,1),
     *           Z(1,2),Z(2,1))
      PRINT 1,N(5),((Z(I,J),J=1,3),I=1,2)
      N(6)=5HCMSUB
      CALL CMSUB(2,3,X(1,1),X(1,2),X(2,1),Y(1,1),Y(1,2),Y(2,1),Z(1,1),
     *           Z(1,2),Z(2,1))
      PRINT 1,N(6),((Z(I,J),J=1,3),I=1,2)
      N(7)=5HCRSCL
      CALL CRSCL(2,3,D(1),D(2),X(1,1),X(1,2),X(2,1),Z(1,1),Z(1,2),
     *           Z(2,1))
      PRINT 1,N(7),((Z(I,J),J=1,3),I=1,2)
      N(8)=5HCMSCL
      CALL CMSCL(2,3,S,X(1,1),X(1,2),X(2,1),Z(1,1),Z(1,2),Z(2,1))
      PRINT 1,N(8),((Z(I,J),J=1,3),I=1,2)
      N(9)=5HCMMPY
      CALL CMMPY(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(9),(Z(I),I=1,2)
      N(10)=5HCMMPA
      CALL CMMPA(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(10),(Z(I),I=1,2)
      N(11)=5HCMMPS
      CALL CMMPS(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(11),(Z(I),I=1,2)
      N(12)=5HCMMNA
      CALL CMMNA(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(12),(Z(I),I=1,2)
      N(13)=5HCMMNS
      CALL CMMNS(2,3,X(1,1),X(1,2),X(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 3,N(13),(Z(I),I=1,2)
      N(14)=5HCUMPY
      CALL CUMPY(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(14),(Z(J),J=1,3)
      N(15)=5HCUMPA
      CALL CUMPA(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(15),(Z(J),J=1,3)
      N(16)=5HCUMPS
      CALL CUMPS(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(16),(Z(J),J=1,3)
      N(17)=5HCUMNA
      CALL CUMNA(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(17),(Z(J),J=1,3)
      N(18)=5HCUMNS
      CALL CUMNS(3,U(1,1),U(1,2),U(2,1),Y(1),Y(2),Z(1),Z(2))
      PRINT 4,N(18),(Z(J),J=1,3)
      N(19)=5HCMBIL
      F=CMBIL(3,V(1),V(2),U(1,1),U(1,2),U(2,1),Y(1),Y(2))
      PRINT 2,N(19),F
 1    FORMAT(15X,A6/(3X,6E15.7)/)
 2    FORMAT(15X,A6/ 3X,2E20.10)
 3    FORMAT(15X,A6/(3X,4E15.7))
 4    FORMAT(15X,A6/(3X,6E15.7))
      STOP
      END
*EXECUTE
*
*
      PROGRAM F004 A
      DIMENSION A(2,3),B(3,4),C(4,4),T(4)
      DATA A/1.5,0.5,0.5,2.,0.2,0.4/,
     *     B/0.5,0.1,2.,0.1,1.,3.,0.4,0.3,1.,0.2,0.1,1.5/
C 1)  *****************************************************
      CALL RMMLT(2,3,4,A(1,1),A(1,2),A(2,1),B(1,1),B(1,2),B(2,1),
     *                 C(1,1),C(1,2),C(2,1),T)
      PRINT 1,((A(I,J),J=1,3),I=1,2),((B(I,K),K=1,4),I=1,3),
     *        ((C(I,K),K=1,4),I=1,2)
C 2)  *****************************************************
      CALL RMMLT(2,3,2,A(1,1),A(1,2),A(2,1),A(1,1),A(2,1),A(1,2),
     *                 C(1,1),C(1,2),C(2,1),T)
      PRINT 2,((C(I,K),K=1,4),I=1,2)
C 3)  ******************************************************
      CALL RMMLT(4,3,4,B(1,1),B(2,1),B(1,2),B(1,1),B(1,2),B(2,1),
     *     C(1,1),C(1,2),C(2,1),T)
      PRINT 3,((C(I,K),K=1,4),I=1,4)
C 4)  ******************************************************
      CALL RMMLT(2,3,4,A(1,1),A(1,2),A(2,1),B(1,1),B(1,2),B(2,1),
     *     B(1,1),B(1,2),B(2,1),T)
      PRINT 4,((B(I,K),K=1,4),I=1,2)
C 5)  *****************************************************
      CALL RMMLA(2,3,4,A(1,1),A(1,2),A(2,1),B(1,1),B(1,2),B(2,1),
     *                 C(1,1),C(1,2),C(2,1))
      PRINT 5,((C(I,K),K=1,4),I=1,2)
C 6)  *****************************************************
      CALL RMMLS(2,3,4,A(1,1),A(1,2),A(2,1),B(1,1),B(1,2),B(2,1),
     *                 C(1,1),C(1,2),C(2,1))
      PRINT 6,((C(I,K),K=1,4),I=1,2)
C 7)  *****************************************************
      CALL RMNMA(2,3,4,A(1,1),A(1,2),A(2,1),B(1,1),B(1,2),B(2,1),
     *                 C(1,1),C(1,2),C(2,1))
      PRINT 7,((C(I,K),K=1,4),I=1,2)
C 8)  *****************************************************
      CALL RMNMS(2,3,4,A(1,1),A(1,2),A(2,1),B(1,1),B(1,2),B(2,1),
     *                 C(1,1),C(1,2),C(2,1))
      PRINT 8,((C(I,K),K=1,4),I=1,2)
 1    FORMAT(1H1,1X,2H1),20X,1HA/2(3X,3E20.11/)/20X,1HB/3(3X,4E20.11/)/
     *       20X,1HC/2(3X,4E20.11/))
 2    FORMAT(1X,2H2),20X,1HC/2(3X,4E20.11/))
 3    FORMAT(1X,2H3),20X,1HC/2(3X,4E20.11/))
 4    FORMAT(1X,2H4),20X,1HB/2(3X,4E20.11/))
  5   FORMAT(1X,2H5),20X,1HC/2(3X,4E20.11/))
  6   FORMAT(1X,2H6),20X,1HC/2(3X,4E20.11/))
  7   FORMAT(1X,2H7),20X,1HC/2(3X,4E20.11/))
  8   FORMAT(1X,2H8),20X,1HC/2(3X,4E20.11/))
  9   FORMAT(1X,2H9),20X,1HC/2(3X,4E20.11/))
      CALL EXIT
      END
*EXECUTE
      PROGRAM F004 B
      DOUBLE PRECISION A(2,3),B(3,4),C(4,4),T(4)
      DATA A/1.5D0,.5D0,.5D0,2.D0,.2D0,.4D0/,
     * B/.5D0 ,.1D0,2.D0,.1D0,1.D0,3.D0,.4D0,.3D0,1.D0,.2D0,.1D0,1.5D0/
C 1)  *****************************************************
      CALL DMMLT(2,3,4,A(1,1),A(1,2),A(2,1),B(1,1),B(1,2),B(2,1),
     *                 C(1,1),C(1,2),C(2,1),T)
      PRINT1,((A(I,J),J=1,3),I=1,2),((B(I,K),K=1,4),I=1,3),
     *       ((C(I,K),K=1,4),I=1,2)
C 2)  *****************************************************
      CALL DMMLT(2,3,2,A(1,1),A(1,2),A(2,1),A(1,1),A(2,1),A(1,2),
     *                 C(1,1),C(1,2),C(2,1),T)
      PRINT 2,((C(I,K),K=1,4),I=1,2)
C 3)  ******************************************************
      CALL DMMLT(4,3,4,B(1,1),B(2,1),B(1,2),B(1,1),B(1,2),B(2,1),
     *     C(1,1),C(1,2),C(2,1),T)
      PRINT 3,((C(I,K),K=1,4),I=1,4)
C 4)  ******************************************************
      CALL DMMLT(2,3,4,A(1,1),A(1,2),A(2,1),B(1,1),B(1,2),B(2,1),
     *     B(1,1),B(1,2),B(2,1),T)
      PRINT 4,((B(I,K),K=1,4),I=1,2)
C 5)  *****************************************************
      CALL DMMLA(2,3,4,A(1,1),A(1,2),A(2,1),B(1,1),B(1,2),B(2,1),
     *                 C(1,1),C(1,2),C(2,1))
      PRINT 5,((C(I,K),K=1,4),I=1,2)
C 6)  *****************************************************
      CALL DMMLS(2,3,4,A(1,1),A(1,2),A(2,1),B(1,1),B(1,2),B(2,1),
     *                 C(1,1),C(1,2),C(2,1))
      PRINT 6,((C(I,K),K=1,4),I=1,2)
C 7)  *****************************************************
      CALL DMNMA(2,3,4,A(1,1),A(1,2),A(2,1),B(1,1),B(1,2),B(2,1),
     *                 C(1,1),C(1,2),C(2,1))
      PRINT 7,((C(I,K),K=1,4),I=1,2)
C 8)  *****************************************************
      CALL DMNMS(2,3,4,A(1,1),A(1,2),A(2,1),B(1,1),B(1,2),B(2,1),
     *                 C(1,1),C(1,2),C(2,1))
      PRINT 8,((C(I,K),K=1,4),I=1,2)
 1    FORMAT(1H1,1X,2H1),20X,1HA/2(3X,3D30.15/)/20X,1HB/3(3X,4D25.15/)/
     *       20X,1HC/2(3X,4D25.15/))
 2    FORMAT(1X,2H2),20X,1HC/2(3X,4D25.15/))
 3    FORMAT(1X,2H3),20X,1HC/4(3X,4D25.15/))
 4    FORMAT(1X,2H4),20X,1HA/2(3X,4D25.15/))
  5   FORMAT(1X,2H5),20X,1HC/2(3X,4D25.15/))
  6   FORMAT(1X,2H6),20X,1HC/2(3X,4D25.15/))
  7   FORMAT(1X,2H7),20X,1HC/2(3X,4D25.15/))
  8   FORMAT(1X,2H8),20X,1HC/2(3X,4D25.15/))
      CALL EXIT
      END
*EXECUTE
      PROGRAM F004 C
      COMPLEX A(2,3),B(3,4),C(4,4),T(4)
      DATA A/1.5,.1,.5,.2,.5,.1,2.,.01,.2,.02,.4,.01/,
     *   B/.5,.1,.1,.2,2.,.01,.1,.01,1.,.2,3.,.01,.4,.1,.3,.2,1.,.3,
     *     .2,.005,.1,.05,1.5,0.01/
C 1)  *****************************************************
      CALL CMMLT(2,3,4,A(1,1),A(1,2),A(2,1),B(1,1),B(1,2),B(2,1),
     *                 C(1,1),C(1,2),C(2,1),T)
      PRINT1,((A(I,J),J=1,3),I=1,2),((B(I,K),K=1,4),I=1,3),
     *       ((C(I,K),K=1,4),I=1,2)
C 2)  *****************************************************
      CALL CMMLT(2,3,2,A(1,1),A(1,2),A(2,1),A(1,1),A(2,1),A(1,2),
     *                 C(1,1),C(1,2),C(2,1),T)
      PRINT 2,((C(I,K),K=1,4),I=1,2)
C 3)  ******************************************************
      CALL CMMLT(4,3,4,B(1,1),B(2,1),B(1,2),B(1,1),B(1,2),B(2,1),
     *     C(1,1),C(1,2),C(2,1),T)
      PRINT 3,((C(I,K),K=1,4),I=1,4)
C 4)  ******************************************************
      CALL CMMLT(2,3,4,A(1,1),A(1,2),A(2,1),B(1,1),B(1,2),B(2,1),
     *     B(1,1),B(1,2),B(2,1),T)
      PRINT 4,((B(I,K),K=1,4),I=1,2)
C 5)  *****************************************************
      CALL CMMLA(2,3,4,A(1,1),A(1,2),A(2,1),B(1,1),B(1,2),B(2,1),
     *                 C(1,1),C(1,2),C(2,1))
      PRINT 5,((C(I,K),K=1,4),I=1,2)
C 6)  *****************************************************
      CALL CMMLS(2,3,4,A(1,1),A(1,2),A(2,1),B(1,1),B(1,2),B(2,1),
     *                 C(1,1),C(1,2),C(2,1))
      PRINT 6,((C(I,K),K=1,4),I=1,2)
C 7)  *****************************************************
      CALL CMNMA(2,3,4,A(1,1),A(1,2),A(2,1),B(1,1),B(1,2),B(2,1),
     *                 C(1,1),C(1,2),C(2,1))
      PRINT 7,((C(I,K),K=1,4),I=1,2)
C 8)  *****************************************************
      CALL CMNMS(2,3,4,A(1,1),A(1,2),A(2,1),B(1,1),B(1,2),B(2,1),
     *                 C(1,1),C(1,2),C(2,1))
      PRINT 8,((C(I,K),K=1,4),I=1,2)
C 9)  *****************************************************
      CALL CMMLTC(2,3,4,A(1,1),A(1,2),A(2,1),B(1,1),B(1,2),B(2,1),
     *                 C(1,1),C(1,2),C(2,1),T)
      PRINT 9,((C(I,K),K=1,4),I=1,2)
 1    FORMAT(1H1,1X,2H1),20X,1HA/2(3X,6E18.10/)/20X,1HB/3(3X,8E13.6/)/
     *       20X,1HC/2(3X,8E13.6/))
 2    FORMAT(1X,2H2),20X,1HC/2(3X,8E13.6/))
 3    FORMAT(1X,2H3),20X,1HC/4(3X,8E13.6/)))
 4    FORMAT(1X,2H4),20X,1HB/2(3X,8E13.6/))
 5    FORMAT(1X,2H5),20X,1HC/2(3X,8E13.6/))
 6    FORMAT(1X,2H6),20X,1HC/2(3X,8E13.6/))
 7    FORMAT(1X,2H7),20X,1HC/2(3X,8E13.6/))
 8    FORMAT(1X,2H8),20X,1HC/2(3X,8E13.6/))
 9    FORMAT(1X,2H9),20X,1HC/2(3X,8E13.6/))
      STOP
      END
*EXECUTE
*
*
      PROGRAM F010 A
      DIMENSION A1(3,3),A2(3,3),A3(3,3),B1(3),B2(3),B3(3),R(3)
      DATA A1/2.,1.,3.,1.,-2.,2.,3.,1.,2./,B1/9.,-2.,7./
      DO 1 I=1,3
      DO 1 J=1,3
      A2(I,J)=A1(I,J)
 1    A3(I,J)=A1(I,J)
      DO 2 K=1,3
 2    B3(K)=B1(K)
      CALL REQN(3,A1,3,R,IFAUL,1,B1)
      PRINT 3,B1,IFAUL
      CALL RINV(3,A2,3,R,IFAUL)
      PRINT 4,((A2(I,J),J=1,3),I=1,3)
      CALL REQINV(3,A3,3,R,IFAUL,1,B3)
      PRINT 5,B3,IFAUL
      PRINT 6,((A3(I,J),J=1,3),I=1,3)
 3    FORMAT(1H1,1X,'REQN'/1X,2HB=,3E20.11,5X,6HIFAUL=,I3/)
 4    FORMAT(/1X,'RINV',1X,6HA(3,3)/3(10X,3E20.10/))
 5    FORMAT(/1X,'REQINV'/1X,2HB=,3E20.11,5X,6HIFAUL=,I3/)
 6    FORMAT(/1X,'REQINV',1X,6HA(3,3)/3(10X,3E20.10/))
      CALL EXIT
      END
*CALL PTIME
*EXECUTE
      PROGRAM F010 B
      DOUBLE PRECISION A1(3,3),A2(3,3),A3(3,3),B1(3),B2(3),B3(3)
      DIMENSION R(3)
      DATA A1/2.D0,1.D0,3.D0,1.D0,-2.D0,2.D0,3.D0,1.D0,2.D0/,
     *     B1/9.D0,-2.D0,7.D0/
      DO 1 I=1,3
      DO 1 J=1,3
      A2(I,J)=A1(I,J)
 1    A3(I,J)=A1(I,J)
      DO 2 K=1,3
 2    B3(K)=B1(K)
      CALL DEQN(3,A1,3,R,IFAUL,1,B1)
      PRINT 3,B1,IFAUL
      CALL DINV(3,A2,3,R,IFAUL)
      PRINT 4,((A2(I,J),J=1,3),I=1,3)
      CALL DEQINV(3,A3,3,R,IFAUL,1,B3)
      PRINT 5,B3,IFAUL
      PRINT 6,((A3(I,J),J=1,3),I=1,3)
 3    FORMAT(1H1,'DEQN'/1X,2HB=,3D30.15,5X,6HIFAUL=,I3/)
 4    FORMAT(/1X,'DINV',1X,6HA(3,3)/3(10X,3D26.16/))
 5    FORMAT(/1X,'DEQINV'/1X,2HB=,3D30.15,5X,6HIFAUL=,I3/)
 6    FORMAT(/1X,'DEQINV',1X,6HA(3,3)/3(10X,3D30.15/))
      CALL EXIT
      END
*CALL PTIME
*EXECUTE
      PROGRAM F010 C
      COMPLEX   A1(3,3),A2(3,3),A3(3,3),B1(3),B2(3),B3(3)
      DIMENSION R(3)
      DATA A1/2.,0.,1.,0.,3.,0.,1.,0.,-2.,0.,2.,0.,3.,0.,1.,0.,2.,0./,
     *     B1/9.,0.,-2.,0.,7.,0./
      DO 1 I=1,3
      DO 1 J=1,3
      A2(I,J)=A1(I,J)
 1    A3(I,J)=A1(I,J)
      DO 2 K=1,3
 2    B3(K)=B1(K)
      CALL CEQN(3,A1,3,R,IFAUL,1,B1)
      PRINT 3,B1,IFAUL
      CALL CINV(3,A2,3,R,IFAUL)
      PRINT 4,((A2(I,J),J=1,3),I=1,3)
      CALL CEQINV(3,A3,3,R,IFAUL,1,B3)
      PRINT 5,B3,IFAUL
      PRINT 6,((A3(I,J),J=1,3),I=1,3)
 3    FORMAT(1H1,'CEQN',1X,2HB=,6E17.10,2X,6HIFAUL=,I3/)
 4    FORMAT(/1X,'CINV',1X,6HA(3,3)/3(10X,6E17.10/))
 5    FORMAT(/1X, 'CEQINV',1X,2HB=,6E17.10,2X,6HIFAUL=,I3/)
 6    FORMAT(/1X,'CEQINV',1X,6HA(3,3)/3(10X,6E17.10/))
      STOP
      END
*CALL PTIME
*EXECUTE
*
*
      PROGRAM F011 A
      DIMENSION A(3,3),B(3),R(3),Z(3,2)
      DATA A/2.,1.,3.,1.,-2.,2.,3.,1.,2./,B/9.,-2.,7./,Z/9.,-2.,7.,
     *       9.12345,-2.1258,7.123/
      CALL RFACT(3,A,3,R,IF,DET,JF)
      PRINT 1,IF,JF,DET
      CALL RFEQN(3,A,3,R,1,B)
      PRINT 2,B
      CALL RFEQN(3,A,3,R,2,Z)
      PRINT 3,((Z(I,J),J=1,2),I=1,3)
      CALL RFINV(3,A,3,R)
      PRINT 4,((A(I,J),J=1,3),I=1,3)
 1    FORMAT(1H1,1X,'RFACT'/5X,'IFAIL=',I1,5X,'JFAIL=',I1,
     *       5X,'DET=',E17.11/)
 2    FORMAT(/1X,'RFEQN'/5X,'B=',3E20.11/)
 3    FORMAT(/1X,'RFEQN',5X,'Z'/(5X,2E20.11)/)
 4    FORMAT(/1X,'RFINV',5X,'A'/(5X,3E20.11)/)
      CALL EXIT
      END
*CALL PTIME
*EXECUTE
      PROGRAM F011 B
      DOUBLE PRECISION A(3,3),B(3),Z(3,2),DET
      DATA A/2.D0,1.D0,3.D0,1.D0,-2.D0,2.D0,3.D0,1.D0,2.D0/,
     * B/9.D0,-2.D0,7.D0/,Z/9.D0,-2.D0,7.D0,9.12345D0,-2.1258D0,7.123D0/
      CALL DFACT(3,A,3,R,IF,DET,JF)
      PRINT 1,IF,JF,DET
      CALL DFEQN(3,A,3,R,1,B)
      PRINT 2,B
      CALL DFEQN(3,A,3,R,2,Z)
      PRINT 3,((Z(I,J),J=1,2),I=1,3)
      CALL DFINV(3,A,3,R)
      PRINT 4,((A(I,J),J=1,3),I=1,3)
 1    FORMAT(1H1,1X,'DFACT'/5X,'IFAIL=',I1,5X,'JFAIL=',I1,
     *       5X,'DET=',D26.16/)
 2    FORMAT(/1X,'DFEQN',5X,'B=',3D26.16)
 3    FORMAT(/1X,'DFEQN',5X,'Z'/(5X,2D26.16)/)
 4    FORMAT(/1X,'DFINV',5X,'A'/(5X,3D26.16)/)
      CALL EXIT
      END
*CALL PTIME
*EXECUTE
      PROGRAM F011 C
      DIMENSION R(3)
      COMPLEX A(3,3),B(3),DET,Z(3,2)
      DATA A/2.,0.,1.,0.,3.,0.,1.,0.,-2.,0.,2.,0.,3.,0.,1.,0.,2.,0./,
     *     B/9.,0.,-2.,0.,7.,0./,
     *     Z/9.,0.,-2.,0.,7.,0.,9.12345,0.,-2.1258,0.,7.123,0./
      CALL CFACT(3,A,3,R,IF,DET,JF)
      PRINT 1,IF,JF,DET
      CALL CFEQN(3,A,3,R,1,B)
      PRINT 2,B
      CALL CFEQN(3,A,3,R,2,Z)
      PRINT 3,((Z(I,J),J=1,2),I=1,3)
      CALL CFINV(3,A,3,R)
      PRINT 4,((A(I,J),J=1,3),I=1,3)
 1    FORMAT(1H1,1X,'CFACT'/5X,'IFAIL=',I1,5X,'JFAIL=',I1,
     *       5X,'DET=',2E17.11/)
 2    FORMAT(/1X,'CFEQN'/5X,'B=',6E17.10/)
 3    FORMAT(/1X,'CFEQN',5X,'Z'/(5X,4E20.11)/)
 4    FORMAT(/1X,'CFINV',5X,'A'/(5X,6E17.10)/)
      STOP
      END
*CALL PTIME
*EXECUTE
*
*
      PROGRAM F012 A
      DIMENSION A1(4,4),A2(4,4),A3(4,4),B1(4,2),B2(4,2),B3(4,2)
      DATA A1/1.,.42,.54,.66,.42,1.,.32,.44,.54,.32,1.,.22,.66,.44,.22,
     *        1./,B1/4.,3.,2.,1.,3.,3.,2.,1./
      DO 10 I=1,4
      DO 10 J=1,4
      A2(I,J)=A1(I,J)
 10   A3(I,J)=A1(I,J)
      DO 20 I=1,4
      DO 20 J=1,2
      B2(I,J)=B1(I,J)
 20   B3(I,J)=B1(I,J)
      CALL RSINV(4,A1,4,IFAIL)
      PRINT 1,IFAIL,((A1(I,J),J=1,4),I=1,4)
      CALL RSEQN(4,A2,4,IFAIL,2,B1)
      PRINT 2,IFAIL,((B1(I,J),J=1,2),I=1,4)
      CALL RSFACT(4,A3,4,IFAIL,DET,JFAIL)
      PRINT 3,IFAIL,JFAIL,DET
      CALL RSFEQN(4,A3,4,1,B2)
      PRINT 4,(B2(I,1),I=1,4)
      CALL RSFEQN(4,A3,4,2,B3)
      PRINT 5,((B3(I,J),J=1,2),I=1,4)
      CALL RSFINV(4,A3,4)
      PRINT 6,((A3(I,J),J=1,4),I=1,4)
 1    FORMAT(1H1,1X,'RSINV',5X,'IFAIL=',I2,15X,'A'/(5X,4E20.11/))
 2    FORMAT(/1X,'RSEQN',5X,'IFAIL=',I2,10X,'A'/(5X,2E20.11/))
 3    FORMAT(/1X,'RSFACT'/5X,'IFAIL=',I2,5X,'JFAIL=',I2,5X,'DET=',
     *       E17.11)
 4    FORMAT(/1X,'RSFEQN'/1X,'B=',4E20.11)
 5    FORMAT(/1X,'RSFEQN',10X,'B '/(5X,2E20.11/))
 6    FORMAT(/1X,'RSFINV',15X,'A'/(5X,4E20.11/))
      CALL EXIT
      END
*CALL PTIME
*EXECUTE
      PROGRAM F012 B
      DOUBLE PRECISION A1(4,4),A2(4,4),A3(4,4),B1(4,2),B2(4,2),B3(4,2)
      DOUBLE PRECISION DET
      DATA A1/1.D0,.42D0,.54D0,.66D0,.42D0,1.D0,.32D0,.44D0,.54D0,.32D0,
     *        1.D0,.22D0,.66D0,.44D0,.22D0,1.D0/,
     *     B1/4.D0,3.D0,2.D0,1.D0,3.D0,3.D0,2.D0,1.D0/
      DO 10 I=1,4
      DO 10 J=1,4
      A2(I,J)=A1(I,J)
 10   A3(I,J)=A1(I,J)
      DO 20 I=1,4
      DO 20 J=1,2
      B2(I,J)=B1(I,J)
 20   B3(I,J)=B1(I,J)
      CALL DSINV(4,A1,4,IFAIL)
      PRINT 1,IFAIL,((A1(I,J),J=1,4),I=1,4)
      CALL DSEQN(4,A2,4,IFAIL,2,B1)
      PRINT 2,IFAIL,((B1(I,J),J=1,2),I=1,4)
      CALL DSFACT(4,A3,4,IFAIL,DET,JFAIL)
      PRINT 3,IFAIL,JFAIL,DET
      CALL DSFEQN(4,A3,4,1,B2)
      PRINT 4,(B2(I,1),I=1,4)
      CALL DSFEQN(4,A3,4,2,B3)
      PRINT 5,((B3(I,J),J=1,2),I=1,4)
      CALL DSFINV(4,A3,4)
      PRINT 6,((A3(I,J),J=1,4),I=1,4)
 1    FORMAT(1H1,1X,'DSINV',5X,'IFAIL=',I2,15X,'A'/(5X,4D26.15/))
 2    FORMAT(/1X,'DSEQN',5X,'IFAIL=',I2,10X,'A'/(5X,2D26.15/))
 3    FORMAT(/1X,'DSFACT'/5X,'IFAIL=',I2,5X,'JFAIL=',I2,5X,'DET=',
     *       D23.15)
 4    FORMAT(/1X,'DSFEQN'/1X,'B=',4D23.15)
 5    FORMAT(/1X,'RSFEQN',10X,'B '/(5X,2D26.15/))
 6    FORMAT(/1X,'DSFINV',15X,'A'/(5X,4D26.15/))
      CALL EXIT
      END
*CALL PTIME
*EXECUTE
*
*
      PROGRAM F100
      DIMENSION A(8,8),B(8,8),C(8,8),D(8,8),F(8,8),G(8,8),
     1          I1(8,2),I2(8,8)
      DOUBLE PRECISION D,F,G,S2
      DO 3 I=1,8
      DO 3 J=1,8,2
   3  CALL RANNOR(A(I,J),A(I,J+1))
      DO 1 I=1,8
      DO 1 J=1,8
      D(I,J)=A(I,J)
      G(I,J)=0.0 D0
      B(I,J)=A(I,J)
      C(I,J)=0.0
   1  F(I,J)=D(I,J)
      CALL MATIN1 (A,8,8,8,0,I1,N1,S1)
      CALL MATIN2 (D,8,8,8,0,I2,N3,S2)
      DO 2 I=1,8
      DO 2 J=1,8
      DO 2 K=1,8
      C(I,J)=C(I,J)+A(I,K)*B(K,J)
   2  G(I,J)=G(I,J)+D(I,K)*F(K,J)
      PRINT 100,((B(I,J),J=1,8),I=1,8),((A(I,J),J=1,8),(D(I,J),J=1,8),
     1I=1,8),((C(I,J),J=1,8),(G(I,J),J=1,8),I=1,8),S1,S2
C     TRY A LINEARLY DEPENDANT SYSTEM
      DO 4 I=1,8
      A(I,3)=0.
   4  D(I,3)=0.
      CALL MATIN1 (A,8,8,8,0,I1,N1,S1)
      CALL MATIN2 (D,8,8,8,0,I2,N3,S2)
      PRINT 101,N1,N3
C      SOLVE CASE N=1
      CALL MATIN1 (B,8,1,8,2,I1,N1,S1)
      CALL MATIN2 (F,8,1,8,2,I2,N3,S2)
  100 FORMAT(1H1, 40X,40HTWO PROGRAMS, INVERTING THE SAME MATRIX./1H0,
     1 13HGIVEN MATRIX. ,/1H0, 8(8E15.8,/1H ),  /1H0, 16HINVERTED MATRIX
     2.//8(7HMATIN1./2(4E20.11/)//7HMATIN2./4(2D43.23/)//)///
     3 14HCOMPUTED UNIT.///8(7HMATIN1./2(4E20.11/)//7HMATIN2./
     4 4(2D43.23/)//)///18HTHE DETERMINANT IS ,E20.11,D43.23///)
  101 FORMAT(5X,9H0NERROR= ,2I5//)
      PRINT 102 ,B(1,1),B(1,2),B(1,3),F(1,1),F(1,2),F(1,3)
  102 FORMAT ( ,21H0FIRST 3 OF MATRIX B ,3E20.11//5X,21H FIRST 3 OF MATR
     1IX F ,5X,2D43.23/26X,D43.23)
      END
*EXECUTE
*
*
      PROGRAM F101
      DIMENSION A(8,8),B(8,8),C(8,8),D(8,8),F(8,8),G(8,8),
     1          I1(8,2),I2(8,8)
      DOUBLE PRECISION D,F,G,S2
      DO 3 I=1,8
      DO 3 J=1,8,2
   3  CALL RANNOR(A(I,J),A(I,J+1))
      DO 1 I=1,8
      DO 1 J=1,8
      D(I,J)=A(I,J)
      G(I,J)=0.0 D0
      B(I,J)=A(I,J)
      C(I,J)=0.0
   1  F(I,J)=D(I,J)
      CALL MATIN1 (A,8,8,8,0,I1,N1,S1)
      CALL MATIN2 (D,8,8,8,0,I2,N3,S2)
      DO 2 I=1,8
      DO 2 J=1,8
      DO 2 K=1,8
      C(I,J)=C(I,J)+A(I,K)*B(K,J)
   2  G(I,J)=G(I,J)+D(I,K)*F(K,J)
      PRINT 100,((B(I,J),J=1,8),I=1,8),((A(I,J),J=1,8),(D(I,J),J=1,8),
     1I=1,8),((C(I,J),J=1,8),(G(I,J),J=1,8),I=1,8),S1,S2
C     TRY A LINEARLY DEPENDANT SYSTEM
      DO 4 I=1,8
      A(I,3)=0.
   4  D(I,3)=0.
      CALL MATIN1 (A,8,8,8,0,I1,N1,S1)
      CALL MATIN2 (D,8,8,8,0,I2,N3,S2)
      PRINT 101,N1,N3
C      SOLVE CASE N=1
      CALL MATIN1 (B,8,1,8,2,I1,N1,S1)
      CALL MATIN2 (F,8,1,8,2,I2,N3,S2)
  100 FORMAT(1H1, 40X,40HTWO PROGRAMS, INVERTING THE SAME MATRIX./1H0,
     1 13HGIVEN MATRIX. ,/1H0, 8(8E15.8,/1H ),  /1H0, 16HINVERTED MATRIX
     2.//8(7HMATIN1./2(4E20.11/)//7HMATIN2./4(2D43.23/)//)///
     3 14HCOMPUTED UNIT.///8(7HMATIN1./2(4E20.11/)//7HMATIN2./
     4 4(2D43.23/)//)///18HTHE DETERMINANT IS ,E20.11,D43.23///)
  101 FORMAT(5X,9H0NERROR= ,2I5//)
      PRINT 102 ,B(1,1),B(1,2),B(1,3),F(1,1),F(1,2),F(1,3)
  102 FORMAT ( ,21H0FIRST 3 OF MATRIX B ,3E20.11//5X,21H FIRST 3 OF MATR
     1IX F ,5X,2D43.23/26X,D43.23)
      END
*EXECUTE
*
*
      PROGRAM F102
      DIMENSION A(10,10), B(10,10), X(10,10), A1(10,10), SCR1(10,10)
      DIMENSION SCR2(10), SCR3(10), SCR4(10), SCR5(10), SCR6(10)
      LOGICAL SING
      INTEGER EX
C
C** READ IN ORIGINAL MATRIX A
C
 1    READ 1000, N
      IF(N.EQ.0) STOP
      DO  10  I=1,N
 10   READ 1010, (A(I,J),J=1,N)
      PRINT 2050
C
C** SAVE ORIGINAL MATRIX A IN A1
C
      DO  20  I=1,N
      DO  20  J=1,N
 20   A1(I,J) = A(I,J)
C
C** PRINT ORIGINAL MATRIX A
C
      PRINT 1020
      DO  30  I=1,N
 30   PRINT 1030, (A(I,J),J=1,N)
C
C** CALCULATION OF THE DETERMINANT OF A
C
      CALL LINVER (A,B,X,N,-1,DET,EX,CNR,SING,10,SCR1,SCR2,SCR3,SCR4,
     1 SCR5,SCR6)
C
C** TEST FOR SINGULARITY
C
      IF  (SING)  GO TO 40
      PRINT 1050
      GO TO 50
 40   PRINT 1060
C
C** PRINT DETERMINANT
C
 50   DET = DET * 10.**EX
      PRINT 1040, DET
      DO  60  I=1,N
      DO  60  J=1,N
 60   A(I,J) = A1(I,J)
C
C** INVERT A
C
      CALL LINVER (A,B,X,N,0,DET,EX,CNR,SING,10,SCR1,SCR2,SCR3,SCR4,
     1 SCR5,SCR6)
C
C** TEST FOR SINGULARITY
C
      IF  (SING)  GO TO 70
      PRINT 1050
      GO TO 75
 70   PRINT 1060
      GO TO 85
C
C** PRINT THE INVERSE OF A
C
 75   PRINT 1070
      DO  80  I=1,N
 80   PRINT 1030, (X(I,J),J=1,N)
C
C** PRINT ESTIMATE OF SPECTRAL NORM AND DETERMINANT
C
      PRINT 1080, CNR
 85   DET = DET * 10.**EX
      PRINT 1040, DET
      IF  (SING)  GO TO 1
C
C** VERIFY THE SOLUTION BY MULTIPLYING A WITH A-INVERSE
C
      DO  100  I=1,N
      DO  100  J=1,N
      SUM = 0.
      DO  90  K=1,N
 90   SUM = SUM + A1(I,K) * X(K,J)
 100  A(I,J) = SUM
C
C** PRINT PRODUCT (SHOULD BE UNITY MATRIX)
C
      PRINT 1090
      DO  110  I=1,N
 110  PRINT 1030, (A(I,J),J=1,N)
      DO  120  I=1,N
      DO  120  J=1,N
 120  A(I,J) = A1(I,J)
C
C** GENERATE N RIGHT HAND SIDES
C
      DO  130  I=1,N
      DO  130  J=1,N
 130  B(J,I) = I
C
C** PRINT RIGHT HAND SIDES
C
      PRINT 2000, N
      DO  140  I=1,N
 140  PRINT 1030, (B(I,J),J=1,N)
      PRINT 2030
C
C** SOLVE LINEAR SYSTEM WITH N RIGHT HAND SIDES
C
      CALL LINVER (A,B,X,N,N,DET,EX,CNR,SING,10,SCR1,SCR2,SCR3,SCR4,
     1 SCR5,SCR6)
C
C** TEST FOR SINGULARITY
C
      IF  (SING)  GO TO 150
      PRINT 1050
      GO TO 155
 150  PRINT 1060
      GO TO 170
C
C** PRINT SOLUTION
C
 155  PRINT 2010
      DO  160  I=1,N
 160  PRINT 1030, (X(I,J),J=1,N)
C
C** PRINT ESTIMATE OF SPECTRAL NORM AND DETERMINANT
C
      PRINT 1080, CNR
 170  DET = DET * 10.**EX
      PRINT 1040, DET
C
C** VERIFY SOLUTION BY MULTIPLYING A WITH SOLUTION X
C** TO OBTAIN RIGHT HAND SIDES AGAIN
C
      DO  190  I=1,N
      DO  190  J=1,N
      SUM = 0.
      DO  180  K=1,N
 180  SUM = SUM + A1(I,K) * X(K,J)
 190  A(I,J) = SUM
C
C** PRINT VERIFICATION
C
      PRINT 2020
      DO  200  I=1,N
 200  PRINT 1030, (A(I,J),J=1,N)
      GO TO 1
 1000 FORMAT (I2)
 1010 FORMAT (10F5.0)
 1020 FORMAT(/// 18HOORIGINAL MATRIX A///)
 1030 FORMAT (1H0 5E20.11)
 1040 FORMAT (/// 17H0DETERMINANT OF A,E25.16///70(1H-)///)
 1050 FORMAT (///  18H0A IS NOT SINGULAR)
 1060 FORMAT (///  53H0A IS FOUND TO BE SINGULAR DURING FURTHER CALCULAT
     1ION)
 1070 FORMAT (///  13H0INVERSE OF A///)
 1080 FORMAT (///  48H0ESTIMATE OF SPECTRAL NORM, WHICH IS THE PRODUCT//
     1/    43H OF THE NORM OF A AND THE NORM OF A-INVERSE,E25.16)
 1090 FORMAT (   ///  27H0PRODUCT OF A AND A-INVERSE///)
 2000 FORMAT(70(1H-)///I2, 17H RIGHT HAND SIDES///)
 2010 FORMAT    (///  70H0SOLUTION OF THE LINEAR SYSTEM WITH THE ABOVE P
     1RINTED RIGHT HAND SIDES///)
 2020 FORMAT (///  34H0VERIFIED SOLUTION - RESULT OF A*X///)
 2030 FORMAT(70(1H-)///)
 2050 FORMAT(1H1///50X,9HTEST F102///)
      END
      SUBROUTINE FINISH
      STOP
      RETURN
      END
*EXECUTE
 5
  129 -231  312 -175  276
 -348  624 -842  473 -745
  194 -347  468 -263  414
  271 -485  655 -368  580
 -206  370 -499  280 -442
 4
    1    2    3    4
    2    1    4    3
    3    4    1    2
    4    3    2    1
 3
    2   -6    2
   -6    1   -4
    2   -4   -3
*
*
      PROGRAM      F103
         DIMENSION A(5,7),B(10,10),C(10,10),D(10,10),E(10,10)
        DATA(((A(I,J),I=1,4),J=1,4)=
     1   1.00 , 0.42 , 0.54 , 0.66,
     2   0.42 , 1.00 , 0.32 , 0.44 ,
     3   0.54 , 0.32 , 1.00 , 0.22 ,
     4   0.66 , 0.44 , 0.22 , 1.00 )
         DATA (((A(I,K),K=5,6),I=1,5) =
     *    0.76 ,  0.08 ,  1.12 ,  0.68 ,  3.14  ,
     *    1.52 ,  0.16 ,  2.24 ,  1.36 ,  6.28  )
      DATA (((B(I,K),I=1,4),K=1,4)=
     1   4.0,3.0,2.0,1.0,
     2   3.0,3.0,2.0,1.0,
     3   2.0,2.0,2.0,1.0,
     4   1.0,1.0,1.0,1.0)
         PRINT 9
         PRINT 10,((A(I,K),K=1,7),I=1,5)
         PRINT 12
         PRINT  11,((B(I,K),K=1,10),I=1,10)
         KA=5
         KB=10
         KC=10
         M=4
         N=4
         CALL MATRIX(20,4,4,4,A,5,B,10,C,10)
         CALL MATRIX(10,4,6,0,A,KA,D,0,0,0)
         PRINT 19
      PRINT 10, ((A(I,K),K=1,4),I=1,4)
         PRINT 21,((A(I,K),I=1,4),K=5,6)
         PRINT 22,D(1)
         CALL MATRIX(10,4,4,0,C,KC,D,0,0,0)
         CALL MATRIX(10,4,4,0,B,KB,E,0,0,0)
         CALL MATRIX(20,4,4,4,B,KB,A,KA,A,KA)
         CALL MATRIX(22,4,4,0,A,KA,C,KC,D,KC)
         PRINT 23
         PRINT  11,((D(I,K),K=1,10),I=1,10)
 9       FORMAT(1H1,10X,8HMATRIX A/)
 10      FORMAT(5X,7F10.3)
 11      FORMAT(5X,10F10.3)
 12      FORMAT(/10X,8HMATRIX B/)
 13      FORMAT(/10X,13H****  ADD A,B/)
 14      FORMAT(/10X,17H*** (A,B)-(A,)B=0/)
 15      FORMAT(/10X,21HSYMMETRIC PRODUCT AMA/)
 16      FORMAT(/10X,28HSUBTRACT A AND PACK-UNPACK A)
 17      FORMAT(/10X,18HMOVE A TO D,B TO E/)
 18      FORMAT(/10X,15HAB-A, AB-B, A-B/)
 19      FORMAT(/10X,24HINVERS A,SOLUTION SYSTEM/)
 20      FORMAT(10X,4F20.5)
  21    FORMAT((/5X,2HX=,4F20.5/))
 22      FORMAT(/5X,7HDETERM=,F10.5)
 23      FORMAT(/5X,24HINV(A*B)-INV(B)*INV(A)=0)
         END
*EXECUTE
*
*
      PROGRAM F104
      DIMENSION A(6,10)
      DATA (((A(I,J),I=1,4),J=1,4)=5(1.),3(2.),1.,2.,3.,3.,1.,2.,3.,4.)
      PRINT 10,((A(I,J),J=1,4),I=1,4)
      CALL SYMINV(A,6,10,4,IR)
      PRINT 11,((A(I,J),J=1,4),I=1,4)
      STOP
 10   FORMAT(//3X,6HMATRIX//1(4F8.2))
 11   FORMAT(//3X,14HINVERSE MATRIX//1(4F8.2))
      END
*EXECUTE
*
*
      PROGRAM F105
         THETA=0.7
         PHI=0.26
         THAXE=1.570796
         PHAXE=0.0
         ROTANG=3.141593
      PRINT 10
       CALL POLROT(THETA,PHI,THPRIM,PHPRIM,THAXE,PHAXE,ROTANG)
      PRINT 1,THETA,PHI,THPRIM,PHPRIM,THAXE,PHAXE,ROTANG
      STOP
   1  FORMAT(14X,5HTHETA,15X,3HPHI,17X,6HTHPRIM,14X,6HPHPRIM//5X,4E20.11
     1///14X,5HTHAXE,15X,5HPHAXE,15X,6HROTANG//5X,3E20.11)
  10  FORMAT(///50X,9HTEST F105//)
       END
*EXECUTE
*
*
      PROGRAM F106 A
      DIMENSION A(10)
      DATA((A(I),I=1,10)=4(1.),3(2.),2(3.),4.)
      PRINT 10,A $ CALL SPXINV(A,4,R) $ PRINT 11,A $ STOP
 10   FORMAT(//3X,6HMATRIX//10F10.2)
 11   FORMAT(///3X,14HMATRIX INVERSE//10F10.2)
      END
*EXECUTE
*
*
      PROGRAM F106 B
      DIMENSION A(210)
      DIMENSION B(20,20),C(20,20),D(20,20)
      DATA A/20*1.,19*2.,18*3.,17*4.,16*5.,15*6.,14*7.,13*8.,
     (12*9.,11*10.,10*11.,9*12.,8*13.,7*14.,6*15.,5*16.,4*17.,
     U3*18.,2*19.,20./
      N=20
      KOKI=20
      DO 111 IIR=1,KOKI
      K=1
      DO 1013 J=1,N
      DO 1013 I=J,N
      B(I,J)=A(K)
      B(J,I)=A(K)
      K=K+1
1013  CONTINUE
      CALL SPXINV(A,20,R)
      K=1
      DO 1113 J=1,N
      DO 1113I=J,N
      D(I,J)=A(K)
      D(J,I)=A(K)
      K=K+1
1113  CONTINUE
      DO 1302 I=1,N
      DO 1302 K=1,N
      S=0.
      DO 1303 J=1,N
      S=S+D(I,J)*B(J,K)
1303  C(I,K)=S
1302  CONTINUE
 111   CONTINUE
      PRINT 4303,C
4303  FORMAT (///3X,11HMATRIX MULT//1(10E10.2))
 10   FORMAT(//3X,6HMATRIX//10F10.2)
 11   FORMAT(///3X,14HMATRIX INVERSE//10F10.2)
      END
*EXECUTE
*
*
      PROGRAM F107
      DIMENSION A(4,4)
      DATA (((A(I,J),I=1,4),J=1,4)=5(1.),3(2.),1.,2.,3.,3.,1.,2.,3.,4.)
      PRINT 10,((A(I,J),J=1,4),I=1,4)
      CALL SMXINV(A,4,R)
      PRINT 11,((A(I,J),J=1,4),I=1,4)
 10   FORMAT(//3X,6HMATRIX//1(4F8.2))
 11   FORMAT(//3X,14HINVERSE MATRIX//1(4F8.2))
      END
*EXECUTE
*
*
      PROGRAM F108
      DIMENSION A(3,3,3) ,B(3,3),C(3,3)
      COMPLEX A,B,C
      READ1,A  $  PRINT 12  $  PRINT 2, (((A(I,J,K),J=1,3),I=1,3),K=1,1)
      PRINT 3  $  PRINT 2,(((A(I,J,K),J=1,3),I=1,3),K=2,2)
      PRINT 5  $  PRINT 2,(((A(I,J,K),J=1,3),I=1,3),K=3,3)
      M=3  $  N=3
      CALL MUXMAC(A,C,M,N,B)
      PRINT 4  $  PRINT 2,((C(I,J),J=1,3),I=1,3)   $  STOP
 1    FORMAT(20F4.1)
 2    FORMAT(3(10X,2F9.2,5X))
 3    FORMAT(///3X,'MATRIX 2'//)
   4   FORMAT(///3X' RESULT'//)
  5   FORMAT(///3X,'MATRIX 3'//)
 12   FORMAT(1H1,3X,'MATRIX 1'//)
      END
*EXECUTE
 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.010.011.012.013.014.015.016.017.018.
 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 2.0 1.0 1.0
 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
*
*
      PROGRAM F109
      DIMENSION A(3,3),B(3,1)
      DATA M,N/3,1/
      DO 5  I=1,M
  5   READ 100,(A(I,J),J=1,M)
 100  FORMAT(13(F10.6))
      DO 10  I=1,M
  10  READ 100,(B(I,J),J=1,N)
      PRINT 200
 200  FORMAT(1H1,20X,8HMATRIX A//)
      DO 15  I=1,M
  15  PRINT 300,(A(I,J),J=1,M)
 300  FORMAT(1H0,9(4X,F10.6))
      PRINT 400
 400  FORMAT(//1H0,20X,8HMATRIX B//)
      DO 20  I=1,M
  20  PRINT 500,(B(I,J),J=1,N)
 500  FORMAT(1H0,10X,9(F10.6,4X))
      I=M
      J=N
      CALL MXEQU(A,B,I,J)
      PRINT 600
 600  FORMAT(//1H0,20X,8HMATRIX X//)
      DO 25  I=1,M
  25  PRINT 500,(B(I,J),J=1,N)
      END
*EXECUTE
10.0       1.0       2.0
1.0         5.0      2.0
2.0       2.0         7.0
13.0
8.0
11.0
*
*
      PROGRAM F110
C        TEST  FOR  F110,F111,F112,F113,F114,F115
       DIMENSION AR(4),AI(4),BR(4),BI(4),CR(4),CI(4)
        AR(1)=3.
       AR(2)=6.
       AR(3)=2.
       AR(4)=4.
       AI(1)=2.
       AI(2)=5.
       AI(3)=1.
       AI(4)=2.
       BR(1)=1.
       BR(2)=3.
        BR(3)=4.
       BR(4)=2.
        BI(1)=1.
       BI(2)=2.
      BI(3)=4.
       BI(4)=3.
      PRINT 1004
 1004 FORMAT(////,15X,26H=TEST LI SANG HO= 5,5,1972,////)
       PRINT 5
  5   FORMAT(5X,5HMXADD,//)
      CALL MXADD(AR,BR,CR,2,2)
      PRINT 4,CR(1),CR(3)
      PRINT 4,CR(2),CR(4)
       PRINT 6
  6   FORMAT(5X,5HMXSUB,//)
      CALL MXSUB(AR,BR,CR,2,2)
      PRINT 4,CR(1),CR(3)
      PRINT 4,CR(2),CR(4)
      B=2.0
       PRINT 7
  7   FORMAT(5X,5HMXMTR,//)
      CALL MXMTR(AR,B ,CR,2,2)
      PRINT 4,CR(1),CR(3)
      PRINT 4,CR(2),CR(4)
      PRINT 8
  8   FORMAT(5X,5HMXNTR,//)
      CALL MXNTR(AR,B ,CR,2,2)
      PRINT 4,CR(1),CR(3)
      PRINT 4,CR(2),CR(4)
      PRINT 14
  14  FORMAT(5X,5HMXTRP,//)
       CALL MXTRP(AR,CR,2,2)
      PRINT 4,CR(1),CR(3)
      PRINT 4,CR(2),CR(4)
       PRINT 9
  9   FORMAT(5X,5HMXUTY,//)
      CALL MXUTY(CI,2)
      PRINT 4,CI(1),CI(3)
      PRINT 4,CI(2),CI(4)
      PRINT 16
      PRINT 15,AR(1),AR(3),BR(1),BR(3),CI(1),CI(3)
      PRINT 15,AR(2),AR(4),BR(2),BR(4),CI(2),CI(4)
       PRINT 10
  10   FORMAT(5X,6HMXDMAL,//)
      CALL MXDMAL(AR,BR,CI,2,2)
      PRINT 4,CI(1),CI(3)
      PRINT 4,CI(2),CI(4)
      PRINT 16
      PRINT 15,AR(1),AR(3),BR(1),BR(3),CI(1),CI(3)
      PRINT 15,AR(2),AR(4),BR(2),BR(4),CI(2),CI(4)
       PRINT 11
  11   FORMAT(5X,6HMXDMAR,//)
      CALL MXDMAR(AR,BR,CI,2,2)
      PRINT 4,CI(1),CI(3)
      PRINT 4,CI(2),CI(4)
      PRINT 16
       PRINT 12
  12    FORMAT(5X,5HMXDML,//)
      CALL MXDML (AR,BR,CI,2,2)
      PRINT 15,AR(1),AR(3),BR(1),BR(3),CI(1),CI(3)
      PRINT 15,AR(2),AR(4),BR(2),BR(4),CI(2),CI(4)
      PRINT 16
       PRINT 13
  13     FORMAT(5X,5HMXDMR,//)
      CALL MXDMR (AR,BR,CI,2,2)
      PRINT 15,AR(1),AR(3),BR(1),BR(3),CI(1),CI(3)
      PRINT 15,AR(2),AR(4),BR(2),BR(4),CI(2),CI(4)
 16   FORMAT(15H***************,/,12X,5HAR(I),22X,5HBR(I),22X,5HCI(I),
     */)
  15  FORMAT(5X,2F10.3,5X,2F10.3,5X,2F10.0,//)
   4   FORMAT(5X,2F10.3,//)
      END
*EXECUTE
*
*
      PROGRAM F111
      DIMENSION A(4,4),B(4,4),C(4,4)
      DO 10 I=1,16
      A(I)=I
  10  B(I)=I+10
      CALL MXADD(A,B,C,4,4)
      PRINT11
      PRINT 12,(A(I),I=1,16)
      PRINT 14,(B(I),I=1,16)
      PRINT  15,(C(I),I=1,16)
      CALL MXSUB(B,A,C,4,4)
      PRINT 16
      PRINT 17,(C(I),I=1,16)
      CALL MXTRA(A,B,C,4,4)
      PRINT 18
      PRINT 17,(C(I),I=1,16)
      CALL MXMTR(A,10.0,C,4,4)
      PRINT 19
      PRINT 17,(C(I),I=1,16)
      CALL MXNTR(A,B,C,4,4)
      PRINT 20
      PRINT 17,(C(I),I=1,16)
   11 FORMAT(1H1,3X,5HMXADD,//)
   12 FORMAT(3X,1HA,//1(4F12.2))
   14 FORMAT(3X//3X,1HB,//1(F12.2))
   15 FORMAT(//3X,1HC,//1(F12.2))
   16 FORMAT(///5HMXSUB//)
   17 FORMAT(4F12.2)
   18 FORMAT(///5HMXTRA//)
   19 FORMAT(///5HMXMTR//)
   20 FORMAT(///5HMXNTR//)
      END
*EXECUTE
*
*                                                                      T
      PROGRAM F112
      DIMENSION WS(6),WT(6),WS1(3),WS2(6)
      DIMENSION R(6),S(6),A1(4),A(9),B(9),A2(9)
C
C     àCXOÑHAü TPEYÉOãúHAü MATPàñA
C
      DATA WT/1.,0.5,0.5,1.,1.,2./
       DATA WS/2.,1.,1.,1.,1.,2./
       DATA WS1/2.,1.,1./
      DATA A/1.,3.,5.,2.,4.,6.,3.,5.,7./
       DATA A1/1.,3.,5.,2./
       DATA WS2/2.,1.,1.,1.,1.,2./
      DATA A2/1.,3.,5.,2.,4.,6.,3.,5.,7./
       M=3
       PRINT 31
 31     FORMAT(//5X,11HTEST TRSMUL,//)
      CALL TRSMUL(WT,S,3)
      CALL PR(S,M)
      PRINT 32
 32     FORMAT(//5X,11HTEST TRSMLU,//)
       CALL TRSMLU (WT,R,3)
       CALL PR(R,M)
C---------------------------------------
       PRINT 61
 61     FORMAT(//5X,10HTEST TRINV,//)
       CALL TRINV(WT,R,M)
       CALL PR(R,M)
C
C      àCXOÑHAü CàMMETPàóHAü MATPàñA
C
       PRINT 98
 98     FORMAT(//5X,11HTEST TRCHUL,//)
       CALL TRCHUL(S,WS,M)
       CALL PR(WS,M)
C
       PRINT 99
       CALL TRCHLU(S,WS,M)
       CALL PR(WS,M)
C
 99     FORMAT(//5X,11HTEST TRCHLU,//)
C------------------------------------------
       PRINT 7
 7       FORMAT(//5X,11HTEST TRSINV,//)
       CALL TRSINV(WS1,R,2)
       CALL PR(R,2)
C------------------------------------------
C     àCXOÑHAü PACèAKOBAHHAü MATPàñA
C
       CALL TRLA(WT,A,B,M,M)
       PRINT 10,B
 10     FORMAT(//5X,9HTEST TRLA,//(5X,3E11.3)/)
C
       CALL TRLTA(WT,A,B,M,M)
       PRINT 11,B
 11     FORMAT(//5X,10HTEST TRLTA,//(5X,3E11.3)/)
C
        CALL TRAL(A,WT,B,M,M)
       PRINT 12,B
 12     FORMAT(//5X,9HTEST TRAL,//(5X,3E11.3)/)
C
       CALL TRALT(A,WT,B,M,M)
       PRINT 13,B
 13     FORMAT(//5X,10HTEST TRALT,//(5X,3E11.3)/)
C-----------------------------------
       M=2
       CALL TRSA(WS1,A1,B,M,M)
       PRINT 14,(B(I),I=1,4)
 14     FORMAT(//5X,9HTEST TRSA,//(5X,2E11.3)/)
C
       CALL TRAS(A1,WS1,B,M,M)
       PRINT 15,(B(I),I=1,4)
 15     FORMAT(//5X,9HTEST TRAS,//(5X,2E11.3)/)
C
       CALL TRSAT(WS1,A1,B,M,M)
       PRINT 16,(B(I),I=1,4)
 16     FORMAT(//5X,10HTEST TRSAT,//(5X,2E11.3)/)
C
       CALL TRATS(A1,WS1,B,M,M)
       PRINT 17,(B(I),I=1,4)
 17     FORMAT(//5X,10HTEST TRATS,//(5X,2E11.3)/)
C
       PRINT 93
 93     FORMAT(//5X,11HTEST TRASAT,//)
       CALL TRASAT(A1,WS1,R,M,M)
       CALL PR(R,M)
C
       PRINT 94
 94     FORMAT(//5X,11HTEST TRATSA,//)
       CALL TRATSA(A1,WS1,R,M,M)
       CALL PR(R,M)
C
       PRINT 95
 95     FORMAT(//5X,10HTEST TRQSQ,//)
       CALL TRQSQ(WS1,WS1,R,M)
       CALL PR(R,M)
C----------------------------------------
       M=3
       PRINT 91
 91     FORMAT(//5X,10HTEST TRAAT,//)
       CALL TRAAT(A2,WS2,M,M)
       CALL PR(WS2,M)
C
       PRINT 92
 92     FORMAT(//5X,10HTEST TRATA,//)
       CALL TRATA(A2,WS2,M,M)
       CALL PR(WS2,M)
C---------------------------------------
       PRINT 96
 96     FORMAT(//5X,10HTEST TRPCK,//)
       CALL TRPCK(A2,S,M)
       CALL PR(S,M)
C
       PRINT 97
 97     FORMAT(//5X,11HTEST TRUPCK,//)
       CALL TRUPCK(S,A2,M)
       PRINT3,A2
 3     FORMAT(5X,3E11.3//)
C
       STOP
       END
       SUBROUTINE PR(A,M)
       DIMENSION A(10)
       I1=1
       I2=1
       DO 1 K=1,M
       PRINT 2,(A(I),I=I1,I2)
       I1=2*I1
       I2=I2+K+1
 1     CONTINUE
 2      FORMAT(5X,3E11.3//)
       RETURN
       END
*EXECUTE
*                                                        TESTF112
*
      PROGRAM F112
C        TEST  FOR  F112,F113,F114
      DIMENSION AR(4),BR(4),CR(4),CI(4)
        AR(1)=3.
       AR(2)=6.
       AR(3)=2.
       AR(4)=4.
       BR(1)=1.
       BR(2)=3.
        BR(3)=4.
       BR(4)=2.
      PRINT 1004
 1004 FORMAT(////,15X,26H=TEST LI SANG HO= 5,5,1972,////)
      PRINT 14
  14  FORMAT(5X,5HMXTRP,//)
       CALL MXTRP(AR,CR,2,2)
      PRINT 4,CR(1),CR(3)
      PRINT 4,CR(2),CR(4)
       PRINT 9
  9   FORMAT(5X,5HMXUTY,//)
      CALL MXUTY(CI,2)
      PRINT 4,CI(1),CI(3)
      PRINT 4,CI(2),CI(4)
      PRINT 16
      PRINT 15,AR(1),AR(3),BR(1),BR(3),CI(1),CI(3)
      PRINT 15,AR(2),AR(4),BR(2),BR(4),CI(2),CI(4)
       PRINT 10
  10   FORMAT(5X,6HMXDMAL,//)
      CALL MXDMAL(AR,BR,CI,2,2)
      PRINT 4,CI(1),CI(3)
      PRINT 4,CI(2),CI(4)
      PRINT 16
      PRINT 15,AR(1),AR(3),BR(1),BR(3),CI(1),CI(3)
      PRINT 15,AR(2),AR(4),BR(2),BR(4),CI(2),CI(4)
       PRINT 11
  11   FORMAT(5X,6HMXDMAR,//)
      CALL MXDMAR(AR,BR,CI,2,2)
      PRINT 4,CI(1),CI(3)
      PRINT 4,CI(2),CI(4)
      PRINT 16
       PRINT 12
  12    FORMAT(5X,5HMXDML,//)
      CALL MXDML (AR,BR,CI,2,2)
      PRINT 15,AR(1),AR(3),BR(1),BR(3),CI(1),CI(3)
      PRINT 15,AR(2),AR(4),BR(2),BR(4),CI(2),CI(4)
      PRINT 16
       PRINT 13
  13     FORMAT(5X,5HMXDMR,//)
      CALL MXDMR (AR,BR,CI,2,2)
      PRINT 15,AR(1),AR(3),BR(1),BR(3),CI(1),CI(3)
      PRINT 15,AR(2),AR(4),BR(2),BR(4),CI(2),CI(4)
 16   FORMAT(15H***************,/,12X,5HAR(I),22X,5HBR(I),22X,5HCI(I),
     */)
  15  FORMAT(5X,2F10.3,5X,2F10.3,5X,2F10.0,//)
   4   FORMAT(5X,2F10.3,//)
      END
*EXECUTE
*
*
      PROGRAM F115
      DIMENSION A(3,3),D(3),B(3,3)
      DATA(A=0.5,0.1,0.4,0.1,1.0,0.3,2.,3.,1.)
      D(1)=1.
      D(2)=1.5
      D(3)=0.5
      CALL MXDMA(A,2,D,B,3)
      PRINT 1,A,D,B
  1   FORMAT(///20X,8HMATRIX A//20X,3(3F10.3)///20X,8HVECTOR D//20X,
     13F10.3///20X,8HMATRIX B//20X,3(3F10.3))
      END
*EXECUTE
*
*
       PROGRAM F116
        DIMENSION A(4),B(4)
         DATA A/1.5,0.8,2.0,2.5/
        DATA B/1.0,2.0,3.0,3.5/
      PRINT 10
         P1=DOT(A,B)
        P2=DOT4(A,B)
        P3=DOTI(A,B)
         P4=DOTNOR(A,B)
         PRINT 2,P1,P2,P3,P4
      STOP
   2  FORMAT(15X,8HDOT(A,B),11X,9HDOT4(A,B),11X,9HDOTI(A,B),10X,
     1     11HDOTNOR(A,B)//5X,4E20.11)
  10  FORMAT(///50X,9HTEST F116//40X,23HA=1.5 , 0.8 , 2.0 , 2.5//40X,
     1      23HB=1.0 , 2.0 , 3.0 , 3.5///)
        END
*EXECUTE
*
*
      PROGRAM F117
      COMMON/SLATE/Z1,Z2,XQX(38)
      DIMENSION A(3),B(3),C(3)
      DATA A/1.,3.,4./
      DATA B/5.,6.,3./
      PRINT 10
      CALL CROSS(A,B,C)
      PRINT 1,A,B,C
      PRINT 20,Z1,Z2
   1  FORMAT(10X,2HA=,3F7.2//10X,2HB=, 3F7.2//10X,2HC=,3F7.2//)
  10  FORMAT(///50X,9HTEST F117///)
  20  FORMAT(10X,3HZ1=,F7.2,10X,3HZ2=,F7.2)
      END
*EXECUTE
*
*
      PROGRAM F118
      DIMENSION A(3),B(3)
      A(1)=1.
      A(2)=1.5
      A(3)=0.5
      PRINT 10
      CALL ROT(A,0.5,B)
      PRINT 1,A,B
   1  FORMAT(20X,2HA=,3F10.3,10X,6HTH=0.5//20X,2HB=,3F10.3)
  10  FORMAT(///50X,9HTEST F118///)
      END
*EXECUTE
*
*
      PROGRAM F119
      DIMENSION A(3),B(3)
      A(1)=4.
      A(2)=5.
      A(3)=6.
      B(1)=1.
      B(2)=2.
      B(3)=3.
      PRINT 10,A,B
      D=DIST(A,B)
      P=VMODUL(A)
      PRINT 1,D,P
   1  FORMAT(///20X,10HDIST(A,B)=,F10.3//20X,10HVMODUL(A)=,F10.3)
  10  FORMAT(///50X,9HTEST F119///20X,2HA=,3F10.3,10X,2HB=,3F10.3)
      END
*EXECUTE
*
*
      PROGRAM F120
      DIMENSION A(3),B(3),GA(9),GB(9)
      A(1)=0.345
      A(2)=1.0
      CALL DIRCOS(A,B)
      PRINT 10,A(1),A(2),B(1),B(2),B(3)
      X=COS(A(1))*COS(A(2))
      Y=COS(A(1))*SIN(A(2))
      Z=SIN(A(1))
      Z=SIN(A(1))
      PRINT 11,X,Y,Z
      CALL ERDIRC(GA,GB)
      PRINT 12,GA
      PRINT 14,GB
 10   FORMAT(1H1,3X'ANGLES'2F15.7///3X'DIRECTION  COSINES'
     */10X,3E20.8)
 11   FORMAT(3X//3X'FOR COMPARISON'/10X,3E20.8)
 12   FORMAT(///3X'ERROR MATRIX GA'//1(3E20.8))
 14   FORMAT(///3X'ERROR MATRIX GB'//1(3E20.8))
      END
*EXECUTE
*
*
      PROGRAM   F121
      DIMENSION A(3),X(3),W(3),G(3,3),H(3,3),EX(3),B(3)
      DIMENSION Q(3),JA(3),IA(9),T(9),NN(32)
      DATA NN/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,
     *23,24,25,26,27,28,29,30,31,32/
      DATA A/-5.,1.,0./
      DATA B/1.,4.,2./
      DATA G/1.,2.,3.,4.,2.,1.,3.,2.,5./
      DATA H/1.,3.,2.,4.,6.,1.,7.,2.,5./
      DATA Q/5.2,1.5,3.4/
      DATA JA/1,2,3/
      ALPHA=6.
      BETA=8.
      N=3
      K=3
      PRINT 10,A,B,G,H,Q,JA
      CALL VADD (A,B,X,3)
      PRINT 5,X,NN(1)
      CALL VSUB (A,B,X,3)
      PRINT 5,X,NN(2)
      CALL VMUL (A,B,X,N)
      PRINT 5,X,NN(3)
      CALL VBIAS (A,ALPHA,X,N)
      PRINT 5,X,NN(4)
      CALL VSCALE (A,ALPHA,X,N)
      PRINT 5,X,NN(5)
      CALL VLINE (A,ALPHA,B,BETA,X,N)
      PRINT 5,X,NN(6)
      CALL VUNIT (A,X,N)
      PRINT 5,X,NN(7)
      CALL VMATR (A,G,W,N,K)
      PRINT 5,W,NN(8)
      CALL VMATL (H,A,W,K,N)
      PRINT 5,W,NN(9)
      CALL VFILL (X,N,ALPHA)
      PRINT 5,X,NN(10)
      CALL VZERO(X,N)
      PRINT 5,X,NN(11)
      CALL VBLANK (X,N)
      PRINT 20,X,NN(12)
      CALL VEXCUM (A,EX,N)
      PRINT 5,EX,NN(13)
      Y=VDOT(A,B,N)
      PRINT 7,Y,NN(14)
      Y = VDOTN (A,B,N)
      PRINT 7,Y,NN(15)
      Y = VMOD (A,N)
      PRINT 7,Y,NN(16)
      Y = VASUM (A,N)
      PRINT 7,Y,NN(17)
      Y = VSUM (A,N)
      PRINT 7,Y,NN(18)
      Y = VMAXA (A,N)
      PRINT 7,Y,NN(19)
      Y = VMAX (A,N)
      PRINT 7,Y,NN(20)
      Y = VMINA (A,N)
      PRINT 7,Y,NN(21)
      Y = VMIN (A,N)
      PRINT 7,Y,NN(22)
      I = LVMAXA (A,N)
      PRINT 6,I,NN(23)
      I = LVMAX (A,N)
      PRINT 6,I,NN(24)
      I = LVMINA (A,N)
      PRINT 6,I,NN(25)
      I = LVMIN (A,N)
      PRINT 6,I,NN(26)
      Y=VDOTN2(A,B,N)
      PRINT 7,Y,NN(27)
      CALL VFLOAT(JA,T,N)
      PRINT 8,T,NN(28)
      CALL VFIX(Q,IA,N)
      PRINT 9,IA,NN(29)
      CALL VCOPYN(A,X,N)
      PRINT 5,X,NN(30)
      Y=VDIST2(A,B,N)
      PRINT 7,Y,NN(31)
      Y=VDIST(A,B,N)
      PRINT 7,Y,NN(32)
  5   FORMAT(3X,3E20.11,10X,3HNN ,I2)
  6   FORMAT(3X,I3,67X,3HNN ,I2)
  7   FORMAT(3X,E20.11,50X,3HNN ,I2)
  8   FORMAT(2(/5X,3E20.11)/5X,3E20.11, 8X,3HNN ,I2)
  9   FORMAT(2(/10X,3I5),/10X,3I5,48X,3HNN ,I2)
  10  FORMAT(///50X,9HTEST F121///20X,2HA=,3F10.3/20X,2HB=,3F10.3/
     120X,2HG=,9F10.3/20X,2HH=,9F10.3/20X,2HQ=,3F10.3/20X,3HJA=,3I4,
     210X,3HN=3,10X,3HK=3,20X,8HALPHA=6.,10X,7HBETA=8.///)
  20  FORMAT(3X,3A6,52X,3HNN ,I2)
      END
*CALL PTIME
*EXECUTE
*
*
      PROGRAM F122
      DIMENSION B(4,5)
      DIMENSION A(4,5),NOIS(10),NOJS(10)
      DATA(((A(I,J),I=1,4),J=1,5)=1.,2.,7.,4.,2.,3.,8.,5.,3.,4.,9.,6.,
     M4.,5.,10.,7.,5.,6.,11.,8.)
      DATA(((B(I,J),I=1,4),J=1,5)=1.,2.,7.,4.,2.,3.,8.,5.,3.,4.,9.,6.,
     M4.,5.,10.,7.,5.,6.,11.,8.)
      NI=4
      NJ=5
      NIS=1
      NJS=3
      NOIS(1)=3
      NOJS(1)=2
      NOJS(2)=4
      NOJS(3)=5
      PRINT 10
      PRINT 20
       PRINT 2,((A(I,J),J=1,5),I=1,4)
      CALL MATRED(A,NI,NJ,NIS,NJS,NOIS,NOJS)
      PRINT 13,NIS,NJS
      PRINT 20
       PRINT 2,((A(I,J),J=1,5),I=1,4)
      CALL UCO(A,B,4,5)
      NIS=1
      NJS=0
      CALL MATRED(A,NI,NJ,NIS,NJS,NOIS,NOJS)
      PRINT 13,NIS,NJS
      PRINT 20
       PRINT 2,((A(I,J),J=1,5),I=1,4)
      CALL UCO(A,B,4,5)
      NIS=0
      NJS=3
      CALL MATRED(A,NI,NJ,NIS,NJS,NOIS,NOJS)
      PRINT 13,NIS,NJS
      PRINT 20
       PRINT 2,((A(I,J),J=1,5),I=1,4)
  2   FORMAT (10X,5F10.3)
  10  FORMAT (///50X,9HTEST F122//)
  13  FORMAT (///20X,4HNIS=,I2,10X,4HNJS=,I2/)
  20  FORMAT (30X,6HA(I,J)//)
      STOP
      END
      SUBROUTINE UCO(A,B,I,J)
      DIMENSION A(I,J),B(I,J)
      DO 30 M=1,I
      DO 30 N=1,J
      A(M,N)=B(M,N)
   30 CONTINUE
      RETURN
      END
*EXECUTE
*
*
      PROGRAM F123
       DIMENSION AR(4),AI(4),BR(4),BI(4),CR(4),CI(4)
        AR(1)=3.
       AR(2)=6.
       AR(3)=2.
       AR(4)=4.
       AI(1)=2.
       AI(2)=5.
       AI(3)=1.
       AI(4)=2.
       BR(1)=1.
       BR(2)=3.
        BR(3)=4.
       BR(4)=2.
        BI(1)=1.
       BI(2)=2.
      BI(3)=4.
       BI(4)=3.
      PRINT 1004
 1004 FORMAT(////,15X,26H=TEST LI SANG HO= 5,5,1972,////)
       CALL RRMPY(AR,BR,CR,2,2,2)
      PRINT 1001
 1001 FORMAT(5X,26HCALL RRMPY(AR,BR,CR,2,2,2))
       PRINT 6,(CR(IS),IS=1,4)
   6   FORMAT(5X,2F15.5,//)
  7   CALL CCMPY(AR,AI,BR,BI,CR,CI,2,2,2)
      PRINT 1002
 1002 FORMAT(3X,35HCALL CCMPY(AR,AI,BR,BI,CR,CI,2,2,2))
      PRINT 4,CR(1),CI(1),CR(3),CI(3)
      PRINT 4,CR(2),CI(2),CR(4),CI(4)
   8   CALL CCMPY1(AR,AI,BR,BI,CR,CI,2,2,2)
      PRINT 1003
 1003 FORMAT(3X,36HCALL CCMPY1(AR,AI,BR,BI,CR,CI,2,2,2))
      PRINT 4,CR(1),CI(1),CR(3),CI(3)
      PRINT 4,CR(2),CI(2),CR(4),CI(4)
   9   CALL CCMPY2(AR,AI,BR,BI,CR,CI,2,2,2)
      PRINT 1006
 1006 FORMAT(3X,36HCALL CCMPY2(AR,AI,BR,BI,CR,CI,2,2,2))
      PRINT 4,CR(1),CI(1),CR(3),CI(3)
      PRINT 4,CR(2),CI(2),CR(4),CI(4)
  10   CALL CCMPY3(AR,AI,BR,BI,CR,CI,2,2,2)
      PRINT 1005
 1005 FORMAT(3X,36HCALL CCMPY3(ARTAI,BR,BI,CR,CI,2,2,2))
      PRINT 4,CR(1),CI(1),CR(3),CI(3)
      PRINT 4,CR(2),CI(2),CR(4),CI(4)
   4   FORMAT(5X,2F10.3,3X,2F10.3,//)
       END
*EXECUTE
*
*
      PROGRAM F124
      COMPLEX A,B
      DIMENSION A(2,2),AR(2,2),AI(2,2),B(2,2)
       A(1,1)=(-2.9,1.6E-4)
       A(1,2)=(3.E4,120.)
       A(2,1)=(-1.4,-3.E-3)
       A(2,2)=(1.5,-5.E-2)
       CALL CXSPLT(A,AR,AI,2,2,2)
      PRINT 4
      PRINT 301,AR(1,1)
      PRINT 301,AR(2,1)
      PRINT 301,AR(1,2)
      PRINT 301,AR(2,2)
      PRINT 301,AI(1,1)
      PRINT 301,AI(2,1)
      PRINT 301,AI(1,2)
      PRINT 301,AI(2,2)
 301  FORMAT(5X,5HTEST=,F15.5)
    4 FORMAT(////,20X,26H=TEST LI SANG HO= 4,4,1972,///)
       CALL CXJOIN(AR,AI,B,2,2,2)
       DO 6 I=1,2
       PRINT 7,(A(I,J),J=1,2)
   7   FORMAT(5X,4F15.5,//)
   6   CONTINUE
       END
*EXECUTE
*
*
       PROGRAM F133
       COMPLEX CDOT,Z(5),W,A(3,1),B(3,2)
      DIMENSION D(6),E(12),C(10)
      EQUIVALENCE (A,D),(B,E),(Z,C)
      DATA D/0.,1.,-1.,1.,2.,1./,E/0.,0.,1.,1.,2.,2.,0.,0.,1.,1.,0.,0./,
     *C/2.,3.,-1.,2.,1.,3.,2.,5.,3.,-1./
      PRINT 10
      DO 2 I=1,5
       W=CDOT(Z(I),A,B,3,1,2)
  2   PRINT  1,Z(I),W
  1   FORMAT(10X,F4.1,1H,,F4.1,10X,E20.11,1H,,E20.11/)
  10  FORMAT(40X,9HTEST F133///14X,1HZ,30X,4HCDOT//)
      STOP
       END
*EXECUTE
*
*
      PROGRAM F136
      COMPLEX CDOTC,A(3),B(3),Z(5),V,W
      PRINT 10
      A(1)=(1.,1.)
      A(2)=(2.,2.)
      A(3)=(3.,3.)
      B(1)=(4.,4.)
      B(2)=(5.,5.)
      B(3)=(6.,6.)
      Z(1)=(0.,0.)
      Z(2)=(2.,3.)
      Z(3)=(-1.,2.)
      Z(4)=(3.,-1.)
      Z(5)=(1.,3.)
      DO 2 I=1,5
      V=Z(I)
      W=CDOTC(V,A,B,3,1,1)
  2   PRINT 1,Z(I),V,W
  1   FORMAT(10X,4F7.1,10X,2E20.11/)
  10  FORMAT(//50X,9HTEST F136//17X,1HZ,14X,1HV,35X,5HCDOTC//)
      STOP
      END
*EXECUTE
*
*
      PROGRAM F140
      DIMENSION X1(3),X2(3),X3(3),X(3),G1(9),G2(9),G3(9),G(9),G12(9),
     MG13(9),G23(9),XN(9),GN(9)
      DATA((G1(I),I=1,9)=1.,5.,6.,5.,2.,7.,6.,7.,3.)
      DATA((G2(J),J=1,9)=3.,8.,9.,8.,2.,6.,9.,6.,1.)
      DATA((G3(K),K=1,9)=5.,1.,3.,1.,6.,2.,3.,2.,7.)
      DATA((X3(I3),I3=1,3)=7.,8.,9.)
       DATA((X2(I2),I2=1,3)=4.,5.,6.)
       DATA((X1(I1),I1=1,3)=1.,2.,3.)
      DATA((G12(I12),I12=1,9)=0.1,0.4,0.5,0.4,0.2,0.7,0.5,0.7,0.3)
      CALL ERCROS(X1,X2,X,G1,G2,G12,G)
      PRINT 201
 201  FORMAT(5X,6HERCROS,//)
       PRINT 101,(X1(I1),I1=1,3)
      PRINT 102,(X2(J),J=1,3 )
       PRINT 104,(X(J),J=1,3)
      PRINT 105,(G1(I2),I2=1,9)
      PRINT 106,(G2(K),K=1,9)
      PRINT 108,(G(J),J=1,9)
      CALL CRONOR(X1,X2,X,G1,G2,G12,G)
      PRINT 202
 202  FORMAT(5X,6HCRONOR,//)
       PRINT 101,(X1(I1),I1=1,3)
      PRINT 102,(X2(J),J=1,3 )
       PRINT 104,(X(J),J=1,3)
      PRINT 105,(G1(I2),I2=1,9)
      PRINT 106,(G2(K),K=1,9)
      PRINT 108,(G(J),J=1,9)
  101 FORMAT(15X,5HX1(I),//,3F10.3,//)
  102 FORMAT(15X,5HX2(I),//,3F10.3,//)
  103 FORMAT(15X,5HX3(I),//,3F10.3,//)
  104 FORMAT(15X,5HX (I),//,3F10.3,//)
  105 FORMAT(15X,5HG1(I),//,3F10.3,//)
  106 FORMAT(15X,5HG2(I),//,3F10.3,//)
  107 FORMAT(15X,5HG3(I),//,3F10.3,//)
  108 FORMAT(15X,5HG (I),//,3F10.3,//)
  109 FORMAT(15X,6HG12(I),//,3F10.3)
  110 FORMAT(15X,6HG13(I),//,3F10.3)
  111 FORMAT(15X,6HG23(I),//,3F10.3)
  112 FORMAT(15X,5HXN(I),//,3F10.3,//)
  113 FORMAT(15X,5HGN(I),//,3F10.3,//)
  114 FORMAT(10X,2HGU,//,5X,F15.5,//)
  115 FORMAT(10X,2H U,//,5X,F15.5,//)
       END
*EXECUTE
*
*
      PROGRAM F141
      DIMENSION X1(3),X2(3),X3(3),X(3),G1(9),G2(9),G3(9),G(9),G12(9),
     MG13(9),G23(9),XN(9),GN(9)
      DATA((G1(I),I=1,9)=1.,5.,6.,5.,2.,7.,6.,7.,3.)
      DATA((G2(J),J=1,9)=3.,8.,9.,8.,2.,6.,9.,6.,1.)
      DATA((G3(K),K=1,9)=5.,1.,3.,1.,6.,2.,3.,2.,7.)
      DATA((X3(I3),I3=1,3)=7.,8.,9.)
       DATA((X2(I2),I2=1,3)=4.,5.,6.)
       DATA((X1(I1),I1=1,3)=1.,2.,3.)
      DATA((G12(I12),I12=1,9)=0.1,0.4,0.5,0.4,0.2,0.7,0.5,0.7,0.3)
      DATA((G13(I13),I13=1,9)=0.2,0.5,0.6,0.5,0.3,0.8,0.6,0.8,0.4)
      DATA((G23(I23),I23=1,9)=0.3,0.1,0.2,0.1,0.4,0.6,0.2,0.6,0.5)
       CALL TRIVEC(X1,X2,X3,X,G1,G2,G3,G12,G13,G23,G)
       PRINT 203
  203  FORMAT(/////,5X,39HTRIVEC(X1,X2,X3,G1,G2,G3,G12,G13,G23,G),///)
      PRINT 101,(X1(I),I=1,3)
      PRINT 102,(X2(I),I=1,3)
      PRINT 103,(X3(I),I=1,3)
      PRINT 104,(X (I),I=1,3)
      PRINT 105,(G1(I),I=1,9)
      PRINT 106,(G2(I),I=1,9)
      PRINT 107,(G3(I),I=1,9)
       PRINT 109,(G12(I),I=1,9)
       PRINT 110,(G13(I),I=1,9)
       PRINT 111,(G23(I),I=1,9)
      PRINT 108,(G (I),I=1,9)
  101 FORMAT(15X,5HX1(I),//,3F10.3,//)
  102 FORMAT(15X,5HX2(I),//,3F10.3,//)
  103 FORMAT(15X,5HX3(I),//,3F10.3,//)
  104 FORMAT(15X,5HX (I),//,3F10.3,//)
  105 FORMAT(15X,5HG1(I),//,3F10.3,//)
  106 FORMAT(15X,5HG2(I),//,3F10.3,//)
  107 FORMAT(15X,5HG3(I),//,3F10.3,//)
  108 FORMAT(15X,5HG (I),//,3F10.3,//)
  109 FORMAT(15X,6HG12(I),//,3F10.3)
  110 FORMAT(15X,6HG13(I),//,3F10.3)
  111 FORMAT(15X,6HG23(I),//,3F10.3)
  112 FORMAT(15X,5HXN(I),//,3F10.3,//)
  113 FORMAT(15X,5HGN(I),//,3F10.3,//)
  114 FORMAT(10X,2HGU,//,5X,F15.5,//)
  115 FORMAT(10X,2H U,//,5X,F15.5,//)
       END
*EXECUTE
*
*
      PROGRAM F142
      DIMENSION X1(3),X2(3),X3(3),X(3),G1(9),G2(9),G3(9),G(9),G12(9),
     MG13(9),G23(9),XN(9),GN(9)
      DATA((G1(I),I=1,9)=1.,5.,6.,5.,2.,7.,6.,7.,3.)
      DATA((G2(J),J=1,9)=3.,8.,9.,8.,2.,6.,9.,6.,1.)
      DATA((G3(K),K=1,9)=5.,1.,3.,1.,6.,2.,3.,2.,7.)
      DATA((X3(I3),I3=1,3)=7.,8.,9.)
       DATA((X2(I2),I2=1,3)=4.,5.,6.)
       DATA((X1(I1),I1=1,3)=1.,2.,3.)
      DATA((G12(I12),I12=1,9)=0.1,0.4,0.5,0.4,0.2,0.7,0.5,0.7,0.3)
      U=ERDOT(X1,X2,G1,G2,G12,GU)
       PRINT 204
  204  FORMAT(/////,5X,27HU=ERDOT(X1,X2,G1,G2,G12,GU),///)
       PRINT 115,U
      PRINT 101,(X1(I),I=1,3)
      PRINT 102,(X2(I),I=1,3)
      PRINT 105,(G1(I),I=1,9)
      PRINT 106,(G2(I),I=1,9)
       PRINT 109,(G12(I),I=1,9)
       PRINT 114,GU
      U=ERDOTN(X1,X2,G1,G2,G12,GU)
       PRINT 205
  205  FORMAT(/////,5X,28HU=ERDOTN(X1,X2,G1,G2,G12,GU),///)
      PRINT 101,(X1(I),I=1,3)
      PRINT 102,(X2(I),I=1,3)
      PRINT 105,(G1(I),I=1,9)
      PRINT 106,(G2(I),I=1,9)
       PRINT 109,(G12(I),I=1,9)
       PRINT 114,GU
  101 FORMAT(15X,5HX1(I),//,3F10.3,//)
  102 FORMAT(15X,5HX2(I),//,3F10.3,//)
  103 FORMAT(15X,5HX3(I),//,3F10.3,//)
  104 FORMAT(15X,5HX (I),//,3F10.3,//)
  105 FORMAT(15X,5HG1(I),//,3F10.3,//)
  106 FORMAT(15X,5HG2(I),//,3F10.3,//)
  107 FORMAT(15X,5HG3(I),//,3F10.3,//)
  108 FORMAT(15X,5HG (I),//,3F10.3,//)
  109 FORMAT(15X,6HG12(I),//,3F10.3)
  110 FORMAT(15X,6HG13(I),//,3F10.3)
  111 FORMAT(15X,6HG23(I),//,3F10.3)
  112 FORMAT(15X,5HXN(I),//,3F10.3,//)
  113 FORMAT(15X,5HGN(I),//,3F10.3,//)
  114 FORMAT(10X,2HGU,//,5X,F15.5,//)
  115 FORMAT(10X,2H U,//,5X,F15.5,//)
       END
*EXECUTE
*
*
      PROGRAM F143
      DIMENSION X1(3),X2(3),X3(3),X(3),G1(9),G2(9),G3(9),G(9),G12(9),
     MG13(9),G23(9),XN(9),GN(9),GU1(3)
       DATA((X1(I1),I1=1,3)=1.,2.,3.)
      DATA((G1(I),I=1,9)=1.,5.,6.,5.,2.,7.,6.,7.,3.)
      DATA((GU1(IU),IU=1,3)=2.,4.,5.)
       GU=2.0
      U=5.0
      CALL ERSCAL(U,X1,X,GU,G1,GU1,G)
       PRINT 115,U
       PRINT 101,(X1(I1),I1=1,3)
       PRINT 104,(X(J),J=1,3)
       PRINT 114,GU
      PRINT 105,(G1(I2),I2=1,9)
       PRINT 116,(GU1(K),K=1,3)
      PRINT 108,(G(J),J=1,9)
  101 FORMAT(15X,5HX1(I),//,3F10.3,//)
  104 FORMAT(15X,5HX (I),//,3F10.3,//)
  105 FORMAT(15X,5HG1(I),//,3F10.3,//)
  108 FORMAT(15X,5HG (I),//,3F10.3,//)
  114 FORMAT(10X,2HGU,//,5X,F15.5,//)
  115 FORMAT(10X,2H U,//,5X,F15.5,//)
 116  FORMAT(10X,6HGU1(I),//,5X,3F15.5,//)
       END
*EXECUTE
*
*
      PROGRAM F144
      DIMENSION X1(3),X2(3),X3(3),X(3),G1(9),G2(9),G3(9),G(9),G12(9),
     MG13(9),G23(9),XN(9),GN(9)
      DATA((G1(I),I=1,9)=1.,5.,6.,5.,2.,7.,6.,7.,3.)
      DATA((G2(J),J=1,9)=3.,8.,9.,8.,2.,6.,9.,6.,1.)
      DATA((G3(K),K=1,9)=5.,1.,3.,1.,6.,2.,3.,2.,7.)
      DATA((X3(I3),I3=1,3)=7.,8.,9.)
       DATA((X2(I2),I2=1,3)=4.,5.,6.)
       DATA((X1(I1),I1=1,3)=1.,2.,3.)
      DATA((G12(I12),I12=1,9)=0.1,0.4,0.5,0.4,0.2,0.7,0.5,0.7,0.3)
      DATA((G13(I13),I13=1,9)=0.2,0.5,0.6,0.5,0.3,0.8,0.6,0.8,0.4)
      DATA((G23(I23),I23=1,9)=0.3,0.1,0.2,0.1,0.4,0.6,0.2,0.6,0.5)
       U=TRISCA(X1,X2,X3,G1,G2,G3,G12,G13,G23,GU)
       PRINT 207
  207 FORMAT(/////,5X,41HU=TRISCA(X1,X2,X3,G1,G2,G3,G12,G13,G23,GU),///)
       PRINT 115,U
      PRINT 101,(X1(I),I=1,3)
      PRINT 102,(X2(I),I=1,3)
      PRINT 103,(X3(I),I=1,3)
      PRINT 105,(G1(I),I=1,9)
      PRINT 106,(G2(I),I=1,9)
      PRINT 107,(G3(I),I=1,9)
       PRINT 109,(G12(I),I=1,9)
       PRINT 110,(G13(I),I=1,9)
       PRINT 111,(G23(I),I=1,9)
       PRINT 114,GU
  101 FORMAT(15X,5HX1(I),//,3F10.3,//)
  102 FORMAT(15X,5HX2(I),//,3F10.3,//)
  103 FORMAT(15X,5HX3(I),//,3F10.3,//)
  104 FORMAT(15X,5HX (I),//,3F10.3,//)
  105 FORMAT(15X,5HG1(I),//,3F10.3,//)
  106 FORMAT(15X,5HG2(I),//,3F10.3,//)
  107 FORMAT(15X,5HG3(I),//,3F10.3,//)
  108 FORMAT(15X,5HG (I),//,3F10.3,//)
  109 FORMAT(15X,6HG12(I),//,3F10.3)
  110 FORMAT(15X,6HG13(I),//,3F10.3)
  111 FORMAT(15X,6HG23(I),//,3F10.3)
  112 FORMAT(15X,5HXN(I),//,3F10.3,//)
  113 FORMAT(15X,5HGN(I),//,3F10.3,//)
  114 FORMAT(10X,2HGU,//,5X,F15.5,//)
  115 FORMAT(10X,2H U,//,5X,F15.5,//)
       END
*EXECUTE
*
*
      PROGRAM F145
      DIMENSION X1(3),X2(3),X3(3),X(3),G1(9),G2(9),G3(9),G(9),G12(9),
     MG13(9),G23(9),XN(9),GN(9)
      DATA((G1(I),I=1,9)=1.,5.,6.,5.,2.,7.,6.,7.,3.)
      DATA((G2(J),J=1,9)=3.,8.,9.,8.,2.,6.,9.,6.,1.)
      DATA((G3(K),K=1,9)=5.,1.,3.,1.,6.,2.,3.,2.,7.)
      DATA((X3(I3),I3=1,3)=7.,8.,9.)
       DATA((X2(I2),I2=1,3)=4.,5.,6.)
       DATA((X1(I1),I1=1,3)=1.,2.,3.)
       U=ERMOD(X1,G1,GU)
      PRINT 208
  208 FORMAT(/////,5X,15HU=ERMOD(X,G,GU),///)
       PRINT 115,U
      PRINT 104,(X1(I),I=1,3)
      PRINT 108,(G1(I),I=1,9)
       PRINT 114,GU
       U=ERMIV(X1,G1,GU)
       PRINT 209
  209 FORMAT(/////,5X,15HU=ERMIV(X,G,GU),///)
       PRINT 115,U
      PRINT 104,(X1(I),I=1,3)
      PRINT 108,(G1(I),I=1,9)
       PRINT 114,GU
  101 FORMAT(15X,5HX1(I),//,3F10.3,//)
  102 FORMAT(15X,5HX2(I),//,3F10.3,//)
  103 FORMAT(15X,5HX3(I),//,3F10.3,//)
  104 FORMAT(15X,5HX (I),//,3F10.3,//)
  105 FORMAT(15X,5HG1(I),//,3F10.3,//)
  106 FORMAT(15X,5HG2(I),//,3F10.3,//)
  107 FORMAT(15X,5HG3(I),//,3F10.3,//)
  108 FORMAT(15X,5HG (I),//,3F10.3,//)
  109 FORMAT(15X,6HG12(I),//,3F10.3)
  110 FORMAT(15X,6HG13(I),//,3F10.3)
  111 FORMAT(15X,6HG23(I),//,3F10.3)
  112 FORMAT(15X,5HXN(I),//,3F10.3,//)
  113 FORMAT(15X,5HGN(I),//,3F10.3,//)
  114 FORMAT(10X,2HGU,//,5X,F15.5,//)
  115 FORMAT(10X,2H U,//,5X,F15.5,//)
       END
*EXECUTE
*
*
      PROGRAM F146
      DIMENSION X1(3),XN(3),G1(9),GN(9)
       DATA((X1(I1),I1=1,3)=1.,2.,3.)
      DATA((G1(I),I=1,9)=1.,5.,6.,5.,2.,7.,6.,7.,3.)
       CALL ERNORM(X1,XN,G1,GN)
       PRINT 101,(X1(I1),I1=1,3)
       PRINT 112,(XN(I3),I3=1,3)
      PRINT 105,(G1(I2),I2=1,9)
       PRINT 113,(GN(J),J=1,9)
  101 FORMAT(15X,5HX1(I),//,3F10.3,//)
  105 FORMAT(15X,5HG1(I),//,3F10.3,//)
  112 FORMAT(15X,5HXN(I),//,3F10.3,//)
  113 FORMAT(15X,5HGN(I),//,3F10.3,//)
      END
*EXECUTE
*
*
      PROGRAM F147
      DIMENSION A1(2),A2(2),B1(3),B2(3),GA1(9),GA2(9),GA12(9),GB1(9),
     F  GB2(9),GB12(9)
      A1(1)=0.5
      A1(2)=0.333333
      A2(1)=0.33333
      A2(2)=0.5
      GA1(5)=1.
      GA1(6)=2.
      GA1(8)=3.
      GA1(9)=4.
      GA2(5)=2.
      GA2(6)=3.
      GA2(8)=4.
      GA2(9)=5.
      GA12(5)=3.
      GA12(6)=4.
      GA12(8)=5.
      GA12(9)=6.
      PRINT 10
      CALL EDICO (A1,A2,B1,B2,GA1,GA2,GA12,GB1,GB2,GB12)
      PRINT103,(GA1(IA),GA2(IA),GA12(IA),GB1(IA),GB2(IA),GB12(IA),IA=1,9
     *)
  10  FORMAT(/50X,9HTEST F147//10X,3HGA1,15X,3HGA2,15X,4HGA12,15X,3HGB1,
     * 15X,3HGB2,15X,4HGB12//)
  103 FORMAT(2X,6E19.11/)
      END
*EXECUTE
*
*
      PROGRAM F150
      DIMENSION A(4,4),B(4,4),C(16,16),D(16,16)
      PRINT 102
  10  READ 100,IA,KA,JB,LB
      IF(IA.EQ. 0) STOP
      PRINT 101,IA,KA,JB,LB
      DO 1 I = 1,IA
      DO 1 K = 1,KA
  1   A(I,K)=100*(10*I+K)
      DO 2 J = 1,JB
      DO 2 L = 1,LB
  2   B(J,L)=10*J+L
      IAJB=IA*JB
      KALB=KA*LB
      DO 3 I = 1,IA
   3  PRINT 103, (A(I,K),K=1,KA)
      PRINT 103
      DO 4 J = 1,JB
  4   PRINT 103, (B(J,L),L=1,LB)
      CALL MXDISM(A,B,D,4,4,16,IA,KA,JB,LB)
      PRINT 103
      DO 5 I = 1,IAJB
  5   PRINT 103, (D(I,K),K=1,KALB)
      CALL MXDIPR(A,B,C,4,4,16,IA,KA,JB,LB)
      PRINT 103
      DO 6 I = 1,IAJB
  6   PRINT 103, (C(I,K),K=1,KALB)
      GO TO 10
  100 FORMAT(4I5)
  101 FORMAT(///7H  IA = ,I3,7H  KA = ,I3,7H  JB = ,I3,7H  LB = ,I3/)
  102 FORMAT(1H1)
  103 FORMAT(1X16F8.0)
      END
      SUBROUTINE MXDISM(A,B,C,IAD,JBD,IJD,IA,KA,JB,LB)
      DIMENSION A(IAD,1),B(JBD,1),C(IJD,1)
      IAJB=IA*JB
      KALB=KA*LB
      IF(IAJB*KALB .EQ. 0) RETURN
      JB1=JB-1
      LB1=LB-1
      DO 1 I = 1,IAJB
      IIA=(I+JB1)/JB
      IIB=MOD(I-1,JB)+1
      DO 1 K = 1,KALB
      KKA=(K+LB1)/LB
      KKB=MOD(K-1,LB)+1
  1   C(I,K)=A(IIA,KKA)+B(IIB,KKB)
      RETURN
      END
*EXECUTE
   2   2   2   2
   3   3   2   2
   3   2   2   3
   2   1   2   2
   4   3   4   2
   4   4   4   4
   4   1   2   1
   3   4   1   4
   1   2   3   4
   4   3   2   1
   1   1   1   1
   0
*
*
          PROGRAM F200
         DIMENSION H(4,4),ROOTR(4),ROOTI(4)
      H ( 1,1 ) = 1.00
      H ( 1,2 ) = 0.42
      H ( 1,3 ) = 0.54
      H ( 1,4 ) = 0.66
      H ( 2,1 ) = 0.42
      H ( 2,2 ) = 1.00
      H ( 2,3 ) = 0.32
      H ( 2,4 ) = 0.44
      H ( 3,1 ) = 0.54
      H ( 3,2 ) = 0.32
      H ( 3,3 ) = 1.00
      H ( 3,4 ) = 0.22
      H ( 4,1 ) = 0.66
      H ( 4,2 ) = 0.44
      H ( 4,3 ) = 0.22
      H ( 4,4 ) = 1.00
         M=4
         IDIM1=4
         IDIM2=4
          CALL QREIG(H,IDIM1,M,IDIM2,ROOTR,ROOTI)
         PRINT 10,(ROOTR(I),I=1,4),(ROOTI(K),K=1,4)
  10      FORMAT(10X,5HEIGEN/(15X,4F20.5))
      END
*EXECUTE
*
*
      PROGRAM      F201
      DIMENSION H(4,4),RR(4),RI(4),GAMMA(4)
      H ( 1,1 ) = 1.00
      H ( 1,2 ) = 0.42
      H ( 1,3 ) = 0.54
      H ( 1,4 ) = 0.66
      H ( 2,1 ) = 0.42
      H ( 2,2 ) = 1.00
      H ( 2,3 ) = 0.32
      H ( 2,4 ) = 0.44
      H ( 3,1 ) = 0.54
      H ( 3,2 ) = 0.32
      H ( 3,3 ) = 1.00
      H ( 3,4 ) = 0.22
      H ( 4,1 ) = 0.66
      H ( 4,2 ) = 0.44
      H ( 4,3 ) = 0.22
      H ( 4,4 ) = 1.00
         KA=4
         STARTR=1.0
         STARTI=0.0
         PRIN=7.0
      CALL EIG5(H,4,4,RR,RI,STARTR,STARTI,PRIN,GAMMA,KA)
         PRINT  10,(RR(I),I=1,4) ,(RI(K),K=1,4)
  10     FORMAT(10X,5HEIGEN/(15X,4F20.5))
         END
*EXECUTE
*
*
      PROGRAM F202
      DIMENSION A(6,6),SCR(9,3),FLAMB(6)
      DATA A/32.,16.,8.,4.,2.,1.,16.,0.,4.,2.,1.,0.,8.,4.,0.,1.,0.,0.,
     *18*0./
      PRINT 10,((A(I,J),J=1,3),I=1,6)
      LOWER=1
      CALL LRCH(6,3,A,0.1E-9,LOWER,FLAMB,6,9,SCR)
      PRINT 1,FLAMB
      LOWER=-1
      CALL LRCH(6,3,A,0.1E-9,LOWER,FLAMB,6,9,SCR)
  1   FORMAT(2X,6E18.10/)
  10  FORMAT(//50X,9HTEST F202//30X,1HA//6(20X,3F10.1/)//)
      STOP
      END
*EXECUTE
*
*
      PROGRAM F 203
C     TEST PROGRAM  F 203
      COMPLEX A(8,8),EW(8),EV(8,8)
      DIMENSION ME(8),AD(2,8,8)
      PRINT 13
      READ 10,N
      DO 1 I=1,N
      READ 11,(A(I,J),J=1,N)
      PRINT12,(A(I,J),J=1,N)
    1 CONTINUE
      NCE=20
      NORM1=1
      DWS=0.01
      IWRIT=1
      CALL EIMA(A,EW,EV,N,NCE,NORM1,DWS,ME,IWRIT,8,8,AD)
      PRINT 14
      DO 2 I=1,N
    2 PRINT 15,EW(I)
      PRINT 16
      DO 3 I=1,N
    3 PRINT 19,(EV(I,J),J=1,N)
      PRINT 17
      PRINT 18,(ME(I),I=1,N)
      STOP
   10 FORMAT(I5)
   11 FORMAT(8F10.2)
   12 FORMAT(1X,4('(',F5.2,' ,',F5.2,' )',5X))
   13 FORMAT( 1H1,'GIVEN MATRIX'///)
   14 FORMAT(1H1,' EIGENVALUES'//)
   15 FORMAT(1X,'(',E15.3,' ,',E15.3,' )')
   16 FORMAT(///' EIGENVECTORS'//)
   17 FORMAT(///' ORDER OF EIGENVALUES IN EW'//)
   18 FORMAT(1X,20I5)
   19  FORMAT(1X,1H(,E10.2,2H ,,E10.2,2H ),5X)
C  19 FORMAT(1X,'('(',E10.2,' ,',E10.2,' )',5X)
      END
*EXECUTE
    4
      2.00     -1.00      3.00      2.00      1.00      1.00      5.00
      0.40      1.50     -4.70      5.45      0.18     -4.00      0.10
      6.00      5.00      1.00     -1.00     -2.00     -2.00     -1.00
      1.25     -0.66      3.42      1.11     -2.50      4.85      2.33
*
*
      PROGRAM F220
      DIMENSION AR(10,10),AI(10,10),WR(10),WI(10),ZR(10,10),ZI(10,10),
     X WORK(100)
      NM=10
      RLB=-1.
      RUB=2.
      CALL MCR(NM,N,AR,AI)
      CALL MCP(NM,N,AR,AI)
C     ALL EIGENVALUES AND CORRESPONDING EIGENVECTORS OF A COMPLEX
C     GENERAL MATRIX
      CALL EISCG1(NM,N,AR,AI,WR,WI,ZR,ZI,IERR,WORK)
      NR=1
      PRINT 100,NR,IERR
      IF(IERR.NE.0) GO TO 2
      CALL VALCP(N,WR,WI)
      CALL VECCP(NM,N,N,ZR,ZI)
    2 CALL MCR(NM,N,AR,AI)
      CALL MCP(NM,N,AR,AI)
C     ALL EIGENVALUES OF A COMPLEX GENERAL MATRIX
      CALL EISCG2(NM,N,AR,AI,WR,WI,IERR)
      NR=2
      PRINT 100,NR,IERR
      IF(IERR.NE.0) GO TO 4
      CALL VALCP(N,WR,WI)
    4 CALL MCR(NM,N,AR,AI)
      CALL MCP(NM,N,AR,AI)
C     ALL EIGENVALUES AND CORRESPONDING EIGENVECTORS OF A COMPLEX
C     HERMITIAN MATRIX
      CALL EISCH1(NM,N,AR,AI,WR,ZR,ZI,IERR,WORK)
      NR=1
      PRINT 110,NR,IERR
      IF(IERR.NE.0) GO TO 6
      CALL VALRP(N,WR)
      CALL VECCP(NM,N,N,ZR,ZI)
    6 CALL MCR(NM,N,AR,AI)
      CALL MCP(NM,N,AR,AI)
C     ALL EIGENVALUES OF A COMPLEX HERMITIAN MATRIX
      CALL EISCH2(NM,N,AR,AI,WR,IERR,WORK)
      NR=2
      PRINT 110,NR,IERR
      IF(IERR.NE.0) GO TO 8
      CALL VALRP(N,WR)
    8 CALL MCR(NM,N,AR,AI)
      CALL MCP(NM,N,AR,AI)
C     SOME EIGENVALUES AND CORRESPONDING EIGENVECTORS OF A COMPLEX
C     HERMITIAN MATRIX
      CALL EISCH3(NM,N,AR,AI,RLB,RUB,N,M,WR,ZR,ZI,IERR,WORK)
      NR=3
      PRINT 110,NR,IERR
      IF(IERR.NE.0) GO TO 10
      IF(M.EQ.0) GO TO 9
      CALL VALRP(M,WR)
      CALL VECCP(NM,N,M,ZR,ZI)
    9 PRINT 500,M
   10 CALL MCR(NM,N,AR,AI)
      CALL MCP(NM,N,AR,AI)
C     SOME EIGENVALUES OF A COMPLEX HERMITIAN MATRIX
      CALL EISCH4(NM,N,AR,AI,RLB,RUB,N,M,WR,IERR,WORK)
      NR=4
      PRINT 110,NR,IERR
      IF(IERR.NE.0) GO TO 12
      IF(M.EQ.0) GO TO 11
      CALL VALRP(M,WR)
   11 PRINT 500,M
   12 CALL MRR(NM,N,AR)
      CALL MRP(NM,N,AR)
C     ALL EIGENVALUES AND CORRESPONDING EIGENVECTORS OF A REAL GENERAL
C     MATRIX
      CALL EISRG1(NM,N,AR,WR,WI,ZR,IERR,WORK)
      NR=1
      PRINT 120,NR,IERR
      IF(IERR.NE.0) GO TO 14
      CALL VALCP(N,WR,WI)
      CALL VECRP(NM,N,N,ZR)
   14 CALL MRR(NM,N,AR)
      CALL MRP(NM,N,AR)
C     ALL EIGENVALUES OF A REAL GENERAL MATRIX
      CALL EISRG2(NM,N,AR,WR,WI,IERR)
      NR=2
      PRINT 120,NR,IERR
      IF(IERR.NE.0) GO TO 16
      CALL VALCP(N,WR,WI)
   16 CALL MRR(NM,N,AR)
      CALL MRP(NM,N,AR)
C     ALL EIGENVALUES AND CORRESPONDING EIGENVECTORS OF A REAL
C     SYMMETRIC MATRIX
      CALL EISRS1(NM,N,AR,WR,ZR,IERR,WORK)
      NR=1
      PRINT 130,NR,IERR
      IF(IERR.NE.0) GO TO 18
      CALL VALRP(N,WR)
      CALL VECRP(NM,N,N,ZR)
   18 CALL MRR(NM,N,AR)
      CALL MRP(NM,N,AR)
C     ALL EIGENVALUES OF A REAL SYMMETRIC MATRIX
      CALL EISRS2(NM,N,AR,WR,IERR,WORK)
      NR=2
      PRINT 130,NR,IERR
      IF(IERR.NE.0) GO TO 20
      CALL VALRP(N,WR)
   20 CALL MRR(NM,N,AR)
      CALL MRP(NM,N,AR)
C     SOME EIGENVALUES AND CORRESPONDING EIGENVECTORS OF A REAL
C     SYMMETRIC MATRIX
      CALL EISRS3(NM,N,AR,RLB,RUB,N,M,WR,ZR,IERR,WORK)
      NR=3
      PRINT 130,NR,IERR
      IF(IERR.NE.0) GO TO 22
      IF(M.EQ.0) GO TO 21
      CALL VALRP(M,WR)
      CALL VECRP(NM,N,M,ZR)
   21 PRINT 500,M
   22 CALL MRR(NM,N,AR)
      CALL MRP(NM,N,AR)
C     SOME EIGENVALUES OF A REAL SYMMETRIC MATRIX
      CALL EISRS4(NM,N,AR,RLB,RUB,N,M,WR,IERR,WORK)
      NR=4
      PRINT 130,NR,IERR
      IF(IERR.NE.0) GO TO 24
      IF(M.EQ.0) GO TO 23
      CALL VALRP(M,WR)
   23 PRINT 500,M
   24 CALL MTR(NM,N,AR)
      CALL MTP(NM,N,AR)
C     ALL EIGENVALUES AND CORRESPONDING EIGENVECTORS OF A REAL
C     SYMMETRIC TRIDIAGONAL MATRIX
      CALL EISST1(NM,N,AR,WR,ZR,IERR,WORK)
      NR=1
      PRINT 140,NR,IERR
      IF(IERR.NE.0) GO TO 26
      CALL VALRP(N,WR)
      CALL VECRP(NM,N,N,ZR)
   26 CALL MTR(NM,N,AR)
      CALL MTP(NM,N,AR)
C     ALL EIGENVALUES OF A REAL SYMMETRIC TRIDIAGONAL MATRIX
      CALL EISST2(NM,N,AR,WR,IERR,WORK)
      NR=2
      PRINT 140,NR,IERR
      IF(IERR.NE.0) GO TO 28
      CALL VALRP(N,WR)
   28 CALL MTR(NM,N,AR)
      CALL MTP(NM,N,AR)
C     SOME EIGENVALUES AND CORRESPONDING EIGENVECTORS OF A REAL
C     SYMMETRIC TRIDIAGONAL MATRIX
      CALL EISST3(NM,N,AR,RLB,RUB,N,M,WR,ZR,IERR,WORK)
      NR=3
      PRINT 140,NR,IERR
      IF(IERR.NE.0) GO TO 30
      IF(M.EQ.0) GO TO 29
      CALL VALRP(M,WR)
      CALL VECRP(NM,N,M,ZR)
   29 PRINT 500,M
   30 CALL MTR(NM,N,AR)
      CALL MTP(NM,N,AR)
C     SOME EIGENVALUES OF A REAL SYMMETRIC TRIDIAGONAL MATRIX
      CALL EISST4(NM,N,AR,RLB,RUB,N,M,WR,IERR,WORK)
      NR=4
      PRINT 140,NR,IERR
      IF(IERR.NE.0) GO TO 32
      IF(M.EQ.0) GO TO 31
      CALL VALRP(M,WR)
   31 PRINT 500,M
   32 CONTINUE
  100 FORMAT(/,10X,8H***EISCG,I1,18H CALLED***   IERR=,I2)
  110 FORMAT(/,10X,8H***EISCH,I1,18H CALLED***   IERR=,I2)
 120  FORMAT(/,10X,8H***EISRG,I1,18H CALLED***   IERR=,I2)
  130 FORMAT(/,10X,8H***EISRS,I1,18H CALLED***   IERR=,I2)
  140 FORMAT(/,10X,8H***EISST,I1,18H CALLED***   IERR=,I2)
  500 FORMAT(/,10X,   2HM=,I3)
      END
      SUBROUTINE VALCP(N,WR,WI)
      DIMENSION WR(N),WI(N)
      PRINT 500
      PRINT 600,(WR(I),WI(I),I=1,N)
      RETURN
  500 FORMAT(//,15X,  19HCOMPLEX EIGENVALUES,/)
 600  FORMAT(5X,2E25.11,5X,2E25.11)
      END
      SUBROUTINE VALRP(N,WR)
      DIMENSION WR(N)
      PRINT 500
      PRINT 600,(WR(I),I=1,N)
      RETURN
  500 FORMAT(//,15X,  16HREAL EIGENVALUES,/)
 600  FORMAT(5X,5E21.11)
      END
      SUBROUTINE VECCP(NM,N,M,ZR,ZI)
      DIMENSION ZR(NM,NM),ZI(NM,NM)
      PRINT 500
      DO 20 I=1,M
      PRINT 700
   20 PRINT 600,(ZR(J,I),ZI(J,I),J=1,N)
      RETURN
  500 FORMAT(//,15X,  20HCOMPLEX EIGENVECTORS)
 600  FORMAT(5X,2E21.11,5X,2E21.11)
  700 FORMAT(/)
      END
      SUBROUTINE VECRP(NM,N,M,ZR)
      DIMENSION ZR(NM,NM)
      PRINT 500
      DO 20 I=1,M
      PRINT 700
   20 PRINT 600,(ZR(J,I),J=1,N)
      RETURN
  500 FORMAT(//,15X,  17HREAL EIGENVECTORS)
  600 FORMAT(5X,5E21.11)
  700 FORMAT(/)
      END
      SUBROUTINE MRP(NM,N,AR)
      DIMENSION AR(NM,NM)
      PRINT 300
      DO 15 I=1,N
   15 PRINT 400,(AR(I,J),J=1,N)
      RETURN
  300 FORMAT(////,15X,  11HREAL MATRIX,/)
  400 FORMAT(5X,12F10.3)
      END
      SUBROUTINE MTP(NM,N,AR)
      DIMENSION AR(NM,2)
      PRINT 300
      PRINT 400,(AR(I,1),I=2,N)
      PRINT 400,(AR(I,2),I=1,N)
      RETURN
  300 FORMAT(////,15X,  54HSYMMETRIC TRIDIAGONAL MATRIX, SUBDIAGONAL AND
     1 DIAGONAL,/)
  400 FORMAT(5X,12F10.3)
      END
      SUBROUTINE MCP(NM,N,AR,AI)
      DIMENSION AR(NM,NM),AI(NM,NM)
      PRINT 300
      DO 15 I=1,N
   15 PRINT 400,(AR(I,J),AI(I,J),J=1,N)
      RETURN
  300 FORMAT(////,15X,  14HCOMPLEX MATRIX,/)
  400 FORMAT(5X,5(2F10.3,2X))
      END
      SUBROUTINE MCR(NM,N,AR,AI)
      DIMENSION AR(NM,NM),AI(NM,NM)
      READ 100,N
      DO 5 I=1,N
    5 READ 200,(AR(I,J),AI(I,J),J=1,N)
      RETURN
  100 FORMAT(I2)
  200 FORMAT(10F8.0)
      END
      SUBROUTINE MRR(NM,N,AR)
      DIMENSION AR(NM,NM)
      READ 100,N
      DO 5 I=1,N
    5 READ 200,(AR(I,J),J=1,N)
      RETURN
  100 FORMAT(I2)
  200 FORMAT(10F8.0)
      END
      SUBROUTINE MTR(NM,N,AR)
      DIMENSION AR(NM,2)
      READ 100,N
      READ 200,(AR(I,1),I=2,N)
      READ 200,(AR(I,2),I=1,N)
      RETURN
  100 FORMAT(I2)
  200 FORMAT(10F8.0)
      END
*EXECUTE
 4
5.      9.      5.      5.      -6.     -6.     -7.     -7.
3.      3.      6.      10.     -5.     -5.     -6.     -6.
2.      2.      3.      3.      -1.     3.      -5.     -5.
1.      1.      2.      2.      -3.     -3.     0.      4.
 4
5.      9.      5.      5.      -6.     -6.     -7.     -7.
3.      3.      6.      10.     -5.     -5.     -6.     -6.
2.      2.      3.      3.      -1.     3.      -5.     -5.
1.      1.      2.      2.      -3.     -3.     0.      4.
 4
3.      0.      1.      0.      0.      0.      0.      2.
1.      0.      3.      0.      0.      -2.     0.      0.
0.      0.      0.      2.      1.      0.      1.      0.
0.      -2.     0.      0.      1.      0.      1.      0.
 4
3.      0.      1.      0.      0.      0.      0.      2.
1.      0.      3.      0.      0.      -2.     0.      0.
0.      0.      0.      2.      1.      0.      1.      0.
0.      -2.     0.      0.      1.      0.      1.      0.
 4
3.      0.      1.      0.      0.      0.      0.      2.
1.      0.      3.      0.      0.      -2.     0.      0.
0.      0.      0.      2.      1.      0.      1.      0.
0.      -2.     0.      0.      1.      0.      1.      0.
 4
3.      0.      1.      0.      0.      0.      0.      2.
1.      0.      3.      0.      0.      -2.     0.      0.
0.      0.      0.      2.      1.      0.      1.      0.
0.      -2.     0.      0.      1.      0.      1.      0.
 3
8.      -1.     -5.
-4.     4.      -2.
18.     -5.     -7.
 3
8.      -1.     -5.
-4.     4.      -2.
18.     -5.     -7.
 4
5.      4.      1.      1.
4.      5.      1.      1.
1.      1.      4.      2.
1.      1.      2.      4.
 4
5.      4.      1.      1.
4.      5.      1.      1.
1.      1.      4.      2.
1.      1.      2.      4.
 4
5.      4.      1.      1.
4.      5.      1.      1.
1.      1.      4.      2.
1.      1.      2.      4.
 4
5.      4.      1.      1.
4.      5.      1.      1.
1.      1.      4.      2.
1.      1.      2.      4.
 5
4.      6.      6.      4.
-4.     -10.    -12.    -10.    -4.
 5
4.      6.      6.      4.
-4.     -10.    -12.    -10.    -4.
 5
4.      6.      6.      4.
-4.     -10.    -12.    -10.    -4.
 5
4.      6.      6.      4.
-4.     -10.    -12.    -10.    -4.
*
*
      PROGRAM F221
      DIMENSION AR(10,10),AI(10,10),WR(10),WI(10),ZR(10,10),ZI(10,10),
     *   WORK(100),BR(10,10),BI(10,10)
      DATA (((AR(I,J),I=1,4),J=1,4)=
     *   5.,3.,2.,1.,5.,6.,3.,2.,-6.,-5.,-1.,-3.,-7.,-6.,-5.,0.)
      DATA (((AI(I,J),I=1,4),J=1,4)=
     *    9.,3.,2.,1.,5.,10.,3.,2.,-6.,-5.,3.,-3.,-7.,-6.,-5.,4.)
      DO 20 I=1,4
      DO 20 J=1,4
      BR(I,J)=AR(I,J)
      BI(I,J)=AI(I,J)
  20  CONTINUE
      CALL EISCG1(10, 4,AR,AI,WR,WI,ZR,ZI,IERR,WORK)
      PRINT 3,IERR
      PRINT 2,(WR(I),WI(I),I=1,4)
      DO 10 J=1,4
      PRINT 1,(ZR(I,J),I=1,4)
 10   PRINT 1,(ZI(I,J),I=1,4)
      CALL EISCG2(10,4,BR,BI,WR,WI,IERR)
      PRINT 2,(WR(I),WI(I),I=1,4)
  1   FORMAT(5X,4E20.11/)
  2   FORMAT(5X,2E20.11/)
  3   FORMAT(10X,5HIERR=,I3)
      STOP
      END
*EXECUTE
*
*
      PROGRAM F222
      DIMENSION AR(10,10),AI(10,10),WR(10),ZR(10,10),ZI(10,10),BR(10,10)
     * ,BI(10,10),CR(10,10),CI(10,10),DR(10,10),DI(10,10),WORK(100)
      DATA(((AR(I,J),I=1,4),J=1,4)=
     * 3.,1.,0.,0.,1.,3.,0.,0.,0.,0.,1.,1.,0.,0.,1.,1.)
      DATA(((AI(I,J),I=1,4),J=1,4)=
     * 0.,0.,0.,-2.,0.,0.,2.,0.,0.,-2.,0.,0.,2.,0.,0.,0.)
      DO 20 I=1,4
      DO 20 J=1,4
      BR(I,J)=AR(I,J)
      CR(I,J)=AR(I,J)
      DR(I,J)=AR(I,J)
      BI(I,J)=AI(I,J)
      CI(I,J)=AI(I,J)
  20  DI(I,J)=AI(I,J)
      CALL EISCH1(10,4,AR,AI,WR,ZR,ZI,IERR,WORK)
      PRINT 3,IERR
      PRINT 2,(WR(I),I=1,4)
       DO 10 J=1,4
      PRINT 1,(ZR(I,J),I=1,4)
 10   PRINT 1,(ZI(I,J),I=1,4)
      CALL EISCH2(10,4,BR,BI,WR,IERR,WORK)
      PRINT 2,(WR(I),I=1,4)
      CALL EISCH3(10,4,CR,CI,-1.,2.,4,M,WR,ZR,ZI,IERR,WORK)
      PRINT 5,M
      PRINT 2,(WR(I),I=1,M)
      DO 30 J=1,M
      PRINT 1,(ZR(I,J),I=1,4)
  30  PRINT 1,(ZI(I,J),I=1,4)
      CALL EISCH4(10,4,DR,DI,-1.,2.,4,M,WR,IERR,WORK)
      PRINT 5,M
      PRINT 2,(WR(I),I=1,M)
  1   FORMAT(10X,4E20.11)
  2   FORMAT(5X,4E20.11/)
  3   FORMAT(//15X,5HIERR=,I3//)
  5   FORMAT(/10X,2HM=,I2)
      STOP
      END
*EXECUTE
*
*
      PROGRAM F223
      DIMENSION AR(10,10),WR(10),WI(10),ZR(10,10),BR(10,10),WORK(100)
      DIMENSION B(10,10),C(10,10),D(10,10)
      DATA(((AR(I,J),I=1,3),J=1,3)=
     * 8.,-4.,18.,-1.,4.,-5.,-5.,-2.,-7.)
      DATA(((B(I,J),I=1,3),J=1,3)=
     * 4.,2.,0.,1.,4.,1.,1.,1.,4.)
      DATA(((C(I,J),I=1,3),J=1,3)=
     * 33.,-24.,-8.,16.,-10.,-4.,72.,-57.,-17.)
      K=3
      DO 20 I=1,3
      DO 20 J=1,3
 20   D(I,J)=AR(I,J)
      PRINT 5
      CALL EISRG1(10,3,AR,WR,WI,ZR,IERR,WORK)
  44  PRINT 3,IERR
      PRINT 2,(WR(I),WI(I),I=1,3)
      PRINT 4
      DO 10 J=1,3
  10  PRINT 1,(ZR(I,J),I=1,3)
      IF(K.LE.2) GO TO 55
      CALL EISRG1(10,3,B,WR,WI,ZR,IERR,WORK)
      K=K-1
      GO TO 44
  55  CONTINUE
      IF(K.EQ.1) GO TO 66
      CALL EISRG1(10,3,C,WR,WI,ZR,IERR,WORK)
      K=K-1
      GO TO 44
  66  CONTINUE
      CALL EISRG2(10,3,D,WR,WI,IERR)
      PRINT 3,IERR
      PRINT 2,(WR(I),WI(I),I=1,3)
  1   FORMAT(5X,4E20.11/)
  2   FORMAT(5X,2E20.11//)
   3   FORMAT(///10X,5HIERR=,I3/15X,2HWR,20X,2HWI/)
  4   FORMAT (/40X,2HZR/)
  5   FORMAT(50X,9HTEST F223)
      STOP
      END
*EXECUTE
*
*
      PROGRAM F224
      DIMENSION AR(10,10),WR(10),ZR(10,10),BR(10,10),CR(10,10),DR(10,10)
     *   ,WORK(100)
      DATA(((AR(I,J),I=1,4),J=1,4)=
     *  5.,4.,1.,1.,4.,5.,1.,1.,1.,1.,4.,2.,1.,1.,2.,4.)
      DO 20 I=1,4
      DO 20 J=1,4
      BR(I,J)=AR(I,J)
      CR(I,J)=AR(I,J)
  20  DR(I,J)=AR(I,J)
      CALL EISRS1(10,4,AR,WR,ZR,IERR,WORK)
      PRINT 3,IERR
      PRINT 2,(WR(I),I=1,4)
      DO 10 J=1,4
  10  PRINT 1,(ZR(I,J),I=1,4)
      CALL EISRS2(10,4,BR,WR,IERR,WORK)
      PRINT 2,(WR(I),I=1,4)
      CALL EISRS3(10,4,CR,-1.,2.,4,M,WR,ZR,IERR,WORK)
      PRINT 3,IERR
      PRINT 5,M
      PRINT 2,(WR(I),I=1,M)
      DO 15 J=1,M
  15  PRINT 1,(ZR(I,J),I=1,4)
      CALL EISRS4(10,4,DR,-1.,2.,4,M,WR,IERR,WORK)
      PRINT 2,(WR(I),I=1,M)
  1   FORMAT(10X,4E20.11)
  2   FORMAT(5X,4E20.11/)
  3   FORMAT(//15X,5HIERR=,I2//)
  5   FORMAT(/10X,2HM=,I2/)
      STOP
      END
*EXECUTE
*
*
      PROGRAM F225
      DIMENSION AR(10,10),WR(10),ZR(10,10),BR(10,10),CR(10,10),DR(10,10)
     *  ,WORK(100)
      AR(2,1)=4.
      AR(3,1)=6.
      AR(4,1)=6.
      AR(5,1)=4.
      AR(1,2)=-4.
      AR(2,2)=-10.
      AR(3,2)=-12.
      AR(4,2)=-10.
      AR(5,2)=-4.
      DO 20 J=1,2
      DO 20 I=1,5
      BR(I,J)=AR(I,J)
      CR(I,J)=AR(I,J)
  20  DR(I,J)=AR(I,J)
      PRINT 8
      PRINT 7,(AR(I,1),I=2,5)
      PRINT 7,(AR(I,2),I=1,5)
      CALL EISST1(10,5,AR,WR,ZR,IERR,WORK)
      PRINT 3,IERR
      PRINT 2,(WR(I),I=1,5)
      DO 10 J=1,5
  10  PRINT 1,(ZR(I,J),I=1,5)
      CALL EISST2(10,5,BR,WR,IERR,WORK)
      PRINT 2,(WR(I),I=1,5)
      CALL EISST3(10,5,CR,-1.,2.,5,M,WR,ZR,IERR,WORK)
      PRINT 3,IERR
      PRINT 5,M
      PRINT 2,(WR(I),I=1,M)
      DO 15 J=1,M
  15  PRINT 1,(ZR(I,J),I=1,5)
      CALL EISST4(10,5,DR,-1.,2.,5,M,WR,IERR,WORK)
      PRINT 5,M
      PRINT 2,(WR(I),I=1,M)
  1   FORMAT(4X,5E20.11)
  2   FORMAT(5X,5E20.11/)
  3   FORMAT(//15X,5HIERR=,I2//)
  5   FORMAT(/10X,2HM=,I2/)
  7   FORMAT(30X,5F10.3)
  8   FORMAT(30X,6HMATRIX//)
      STOP
      END
*EXECUTE
*
*
      PROGRAM F230
      DIMENSION A(3,3),B(4,4),C(10),EIG(3),EIGV(4)
      DATA A/8.,-4.,18.,-1.,4.,-5.,-5.,-2.,-7./,EIG/2.,-1.,-1./
      DATA B/5.,4.,1.,1.,4.,5.,1.,1.,1.,1.,4.,2.,1.,1.,2.,4./
      DATA C/5.,4.,5.,1.,1.,4.,1.,1.,2.,4./,EIGV/-1.,-1.,2.,2./
      CALL DEFL(A,3,3,1.,EIG)
      CALL DEFLS(B,4,4,5.,EIGV)
      CALL DEFLSS(C,4,5.,EIGV)
      PRINT 1,A
      PRINT 2,B
      PRINT 3,C
  1   FORMAT(/30X,13HTEST FOR DEFL/3(2X,3E20.11/))
  2   FORMAT(/30X,14HTEST FOR DEFLS/4(2X,4E20.11/))
  3   FORMAT(/30X,15HTEST FOR DEFLSS/2(2X,5E20.11/))
      STOP
      END
*EXECUTE
*
*
      PROGRAM F240
      DIMENSION H ( 4,4 ), U ( 4,4 ), X ( 4 )
      PRINT 8
      M=1
      ACCURA = 0.65
  10  A=A
      H ( 1,1 ) = 1.00
      H ( 1,2 ) = 0.42
      H ( 1,3 ) = 0.54
      H ( 1,4 ) = 0.66
      H ( 2,1 ) = 0.42
      H ( 2,2 ) = 1.00
      H ( 2,3 ) = 0.32
      H ( 2,4 ) = 0.44
      H ( 3,1 ) = 0.54
      H ( 3,2 ) = 0.32
      H ( 3,3 ) = 1.00
      H ( 3,4 ) = 0.22
      H ( 4,1 ) = 0.66
      H ( 4,2 ) = 0.44
      H ( 4,3 ) = 0.22
      H ( 4,4 ) = 1.00
      CALL KIM ( H, 4, 0, U, NR, 4, X, 0, ACCURA )
      PRINT 100, H
      PRINT 100, U
      PRINT 101, X, NR, M
      ACCURA = 0.00161637
      IF ( M - 2 ) 20,20,30
  20  M=3
      GO TO 10
   30 IF ( M - 4 ) 40,40,50
  40  M=5
      ACCURA = 0.0000001
      GO TO 10
   50 X (1) = 2.32274880
      X ( 2 ) = 0.79670669
      X ( 3 ) = 0.63828380
      X ( 4 ) = 0.24226071
      PRINT 102, X
      STOP
  8      FORMAT( 30X,'EIGENVALUES AND EIGENVECTORS OF A REAL SYMMETRIC M
     1ATRIX',20X,'TEST F220'/30X,55(1H*),20X,9(1H*)//)
  100 FORMAT ( //   (4 (  F19.9 )))
  101 FORMAT ( 6X, 4F19.9, 6X3HNR=I5, 6X2HM=I5 )
  102 FORMAT ( 6X4F19.9, 12HEXACT EVALUE )
      END
*EXECUTE
*
*
      PROGRAM  F301 A
      DIMENSION A(4,4),B(4,4),N(3)
      DATA B/2.,2.,4.,1.,5.,-7.,0.,0.,9.,12.,3.,6.,4.,8.,-5.,4/,N/4,3,2/
      PRINT 10,((B(I,J),J=1,4),I=1,4)
      DO 1 I=1,3
      DO 5 L=1,4
      DO 5 K=1,4
  5   A(L,K)=B(L,K)
      Y=   DET(A,4,N(I))
  1   PRINT 2,N(I),Y
  2   FORMAT(10X,I3,10X,E20.11)
  10  FORMAT(//30X,9HTEST F301//30X,1HA//4(10X,4F10.1/)//11X,1HN,20X,
     * 1HY/)
      CALL EXIT
      END
*EXECUTE
      PROGRAM F301 B
      DOUBLE PRECISION A,B,DY,DDETER
      DIMENSION A(4,6),B(4,6),N(3)
      DATA B/2.D0,2.D0,4.D0,1.D0,5.D0,-7.D0,0.D0,0.D0,9.D0,12.D0,3.D0,
     *6.D0,4.D0,8.D0,-5.D0,4.D0,8*0.D0/,N/4,3,2/
      PRINT 10,B
      DO 1 I=1,3
      DO 5 L=1,4
      DO 5 K=1,6
  5   A(L,K)=B(L,K)
      DY=DDETER(A,4,N(I))
  1   PRINT 2,N(I),DY
  2   FORMAT(10X,I3,10X,D20.11)
   10    FORMAT(//50X,9HTEST F320//30X,1HA//6(10X,4D10.1/)//11X,1HN,20X,
     * 1HY/)
      STOP
      END
*EXECUTE
*
 
*
      PROGRAM F301
      DIMENSION A(4,4),B(4,4),N(3),ALFA(5)
      DATA B/2.,2.,4.,1.,5.,-7.,0.,0.,9.,12.,3.,6.,4.,8.,-5.,4./,
     * N/4,3,2/,ALFA/0.,0.5,1.,2.,3./
      PRINT 10,B
      DO 1 I=1,3
      DO 1 J=1,5
      DO 5 L=1,4
      DO 5 K=1,4
  5   A(L,K)=B(L,K)
      Y=DET(A,4,N(I),ALFA(J))
  1   PRINT 2,N(I),ALFA(J),Y
  2   FORMAT(10X,I3,10X,F4.1,10X,E20.11)
  10  FORMAT(//30X,9HTEST F301//30X,1HA//4(10X,4F10.1/)//11X,1HN,12X,
     * 4HALFA,20X,1HY/)
      STOP
       END
*EXECUTE
*
*
      PROGRAM F303
      DIMENSION A(2,6),INDEX(4)
      COMPLEX A
      DATAA/1.,2.,3.,2.,-1.,-1.,1.,-1.,-1.,0.,5.,0.,-0.5,0.,2.5,0.,8*0./
      IDIM1=2
      N=2
      M=2
      PRINT 10,A
      CALL CINV1(A,IDIM1,N,M,INDEX,NERROR)
      PRINT 5,A
      PRINT 2,NERROR
  2   FORMAT(10X,7HNERROR=I2)
  5   FORMAT(15X,5HA(-1)/2(10X,4E20.11/)//15X,9HZ=A(-1)*B/2(10X,4E20.11/
     * )//15X,8F8.1)///)
  10  FORMAT(//50X,9HTEST F303//30X,1HA/2(10X,4F10.1/)//30X,1HB/2(10X,
     * 4F10.1/)//15X,8F8.1)///)
      STOP
      END
*EXECUTE
*
*
      PROGRAM  F311
      DIMENSION A(4,6),B(4,6),N(3)
      DATA B/2.,2.,4.,1.,5.,-7.,0.,0.,9.,12.,3.,6.,4.,8.,-5.,4.,8*0./,
     * N/4,3,2/
      PRINT 10,B
      DO 1 I=1,3
      DO 5 L=1,4
      DO 5 K=1,6
  5   A(L,K)=B(L,K)
      Y=DETERM(A,4,N(I))
  1   PRINT 2,N(I),Y
  2   FORMAT(10X,I3,10X,E20.11)
  10  FORMAT(//30X,9HTEST F311//30X,1HA//6(10X,4F10.1/)//11X,1HN,20X,
     * 1HY/)
      STOP
      END
*EXECUTE
*
*
      PROGRAM F320
      DOUBLE PRECISION A,B,DY,DDETER
      DIMENSION A(4,6),B(4,6),N(3)
      DATA B/2.D0,2.D0,4.D0,1.D0,5.D0,-7.D0,0.D0,0.D0,9.D0,12.D0,3.D0,
     *6.D0,4.D0,8.D0,-5.D0,4.D0,8*0.D0/,N/4,3,2/
      PRINT 10,B
      DO 1 I=1,3
      DO 5 L=1,4
      DO 5 K=1,6
  5   A(L,K)=B(L,K)
      DY=DDETER(A,4,N(I))
  1   PRINT 2,N(I),DY
  2   FORMAT(10X,I3,10X,D20.11)
   10    FORMAT(//50X,9HTEST F320//30X,1HA//6(10X,4D10.1/)//11X,1HN,20X,
     * 1HY/)
      STOP
      END
*EXECUTE
*
*
      PROGRAM F401
      DIMENSION C(3,4),D(3,4),I1(3),I2(3),E(3,4),F(3,4)
      DOUBLE PRECISION D,F,DET2
      DATA C/2.,1.,3.,1.,-2.,2.,3.,1.,2.,9.,-2.,7./
      PRINT 10,((C(I,J),J=1,4),I=1,3)
      DO 1 I=1,3
      DO 1 J=1,4
      D(I,J)=C(I,J)
      E(I,J)=C(I,J)
  1   F(I,J)=C(I,J)
      CALL LINEQ1(C,3,3,4,1,I1,NEROR1,DET1)
      CALL LINEQ2(D,3,3,4,1,I2,NEROR2,DET2)
      PRINT 2,DET1,NEROR1
      PRINT 4,(C(I,1),I=1,3)
      PRINT 3,DET2,NEROR2
      PRINT 5,(D(I,1),I=1,3)
      DO 6 I=1,3
      DO 6 J=1,4
      D(I,J)=E(I,J)
  6   C(I,J)=E(I,J)
      CALL LINEQ1(E,3,2,4,2,I1,NEROR1,DET1)
      CALL LINEQ2(F,3,2,4,2,I2,NEROR2,DET2)
      PRINT 2,DET1,NEROR1
      PRINT 8,((E(I,J),J=1,2),I=1,2)
      PRINT 3,DET2,NEROR2
      PRINT 9,((F(I,J),J=1,2),I=1,2)
      DO 7 I=1,3
      C(I,3)=0.
  7   D(I,3)=0.
      CALL LINEQ1(C,3,3,4,1,I1,NEROR1,DET1)
      CALL LINEQ2(D,3,3,4,1,I2,NEROR2,DET2)
      PRINT 11,NEROR1,NEROR2
      STOP
  2   FORMAT(/1X,9HLINEQ1...,10X,5HDET1=,E20.11,5X,7HNEROR1=,I3/)
  3   FORMAT(/1X,9HLINEQ2...,10X,5HDET2=,D43.23,5X,7HNEROR2=,I3/)
 4    FORMAT (15X,1HX,3(/5X,E20.11))
  5   FORMAT (25X,1HX,3(/5X,D40.23))
  8   FORMAT(15X,1HX,25X,1HX,2(/5X,E20.11,5X,E20.11))
  9   FORMAT(25X,1HX,40X,1HX,2(/5X,2D40.23))
  10  FORMAT(//50X,9HTEST F401///30X,3HC,D//3(10X,4F10.1/))
  11  FORMAT(5X,7HNEROR1=,I3,5X,7HNEROR2=,I3)
      END
*EXECUTE
*
*
      PROGRAM F402
      DIMENSION C(3,4),D(3,4),I1(3),I2(3),E(3,4),F(3,4)
      DOUBLE PRECISION D,F,DET2
      DATA C/2.,1.,3.,1.,-2.,2.,3.,1.,2.,9.,-2.,7./
      PRINT 10,((C(I,J),J=1,4),I=1,3)
      DO 1 I=1,3
      DO 1 J=1,4
      D(I,J)=C(I,J)
      E(I,J)=C(I,J)
  1   F(I,J)=C(I,J)
      CALL LINEQ1(C,3,3,4,1,I1,NEROR1,DET1)
      CALL LINEQ2(D,3,3,4,1,I2,NEROR2,DET2)
      PRINT 2,DET1,NEROR1
      PRINT 4,(C(I,1),I=1,3)
      PRINT 3,DET2,NEROR2
      PRINT 5,(D(I,1),I=1,3)
      DO 6 I=1,3
      DO 6 J=1,4
      D(I,J)=E(I,J)
  6   C(I,J)=E(I,J)
      CALL LINEQ1(E,3,2,4,2,I1,NEROR1,DET1)
      CALL LINEQ2(F,3,2,4,2,I2,NEROR2,DET2)
      PRINT 2,DET1,NEROR1
      PRINT 8,((E(I,J),J=1,2),I=1,2)
      PRINT 3,DET2,NEROR2
      PRINT 9,((F(I,J),J=1,2),I=1,2)
      DO 7 I=1,3
      C(I,3)=0.
  7   D(I,3)=0.
      CALL LINEQ1(C,3,3,4,1,I1,NEROR1,DET1)
      CALL LINEQ2(D,3,3,4,1,I2,NEROR2,DET2)
      PRINT 11,NEROR1,NEROR2
      STOP
  2   FORMAT(/1X,9HLINEQ1...,10X,5HDET1=,E20.11,5X,7HNEROR1=,I3/)
  3   FORMAT(/1X,9HLINEQ2...,10X,5HDET2=,D43.23,5X,7HNEROR2=,I3/)
 4    FORMAT (15X,1HX,3(/5X,E20.11))
  5   FORMAT (25X,1HX,3(/5X,D40.23))
  8   FORMAT(15X,1HX,25X,1HX,2(/5X,E20.11,5X,E20.11))
  9   FORMAT(25X,1HX,40X,1HX,2(/5X,2D40.23))
  10  FORMAT(//50X,9HTEST F402///30X,3HC,D//3(10X,4F10.1/))
  11  FORMAT(5X,7HNEROR1=,I3,5X,7HNEROR2=,I3)
      END
*EXECUTE
*
*
      PROGRAM F403
      DIMENSION B(5,3),R(5)
      DATA B/1.,2.,1.,1.,1.,2.,1.,3.,5.,2.,0.,1.,1.,1.,0./,
     *     R/1.,2.,1.,3.,2./
      N=5
      MM=3
      K=1
      IDIM=5
      PRINT 10,((B(I,J),J=1,3),I=1,5)
       CALL BLEQ(B,N,MM,K,IDIM,R,PIV)
      PRINT 1,R,PIV
      STOP
  10  FORMAT(//50X,9HTEST F403//30X,1HA//5(10X,3F10.1/))
  1   FORMAT(5X,2HR=,5E20.11/5X,4HPIV=,E20.11)
      END
*EXECUTE
*
*
      PROGRAM F404
C        THIS TESTING PROGRAM RUNS THREE SYSTEMS OF EQUATIONS WITH
C        NT = 1 AND 0. SYSTEM 1 IS WELL CONDITIONED, SYSTEM 2 ILL CONDI-
C        TIONED AND SYSTEM 3 IS SINGULAR. THE SIX OUTPUT NUMBERS SHOULD
C    BE ABOUT 11, ABOUT 1,0,1,1,0.  THIS TEST INCLUDES ALSO A TEST
C        OF TRISOL, WHICH IS CALLED BY TRIDIA.
      REAL SYST(10,3,3),A1(10),B1(10),W(20)
      DATA SYST/10*.1,10*1.,10*.1,4*1.,6*.1,1.,2*2.,1.0000000001,9*1.,0.
     è,6*.1,30*0./
      DO 10 N=1,2
      NT = 2-N
      DO 10 I=1,3
      CALL TRIDIA(SYST(1,1,I),SYST(1,2,I),SYST(1,3,I),10,A1,B1,W,NT,ND)
   10 PRINT 20,ND
   20 FORMAT(10X,I10)
      END
*EXECUTE
*
*
      PROGRAM F405 A
C*     TESTPROGRAM FOR LINSYS
C*     THE PROGRAM READS A MATRIX, CREATES 3 RANDOM X-VECTORS AND
C*     CORRESPONDING RIGHT-HAND SIDE VECTORS B=A*X. LINSYS IS THEN
C*     CALLED.
      DIMENSION A(10,10),B(10),X(10),WORK(140)
      LOGICAL LF,LP
      LP=.TRUE.
      N=10
      DO 777 NMAT=1,10
      LF=.TRUE.
      PRINT 700
  700 FORMAT(//,5X,  12HINPUT MATRIX,/)
      DO 5 I=1,N
      DO 17 J=1,N
 17   A(I,J)=RNDM(-1.)+1.E9
    5 PRINT 300,(A(I,J),J=1,N)
 300  FORMAT(5X,5E20.11)
      DO 25 K=1,3
      DO 10 I=1,N
  10  X(I)=RNDM(-1.)
      DO 20 I=1,N
      T=0.
      DO 15 J=1,N
   15 T=T+A(I,J)*X(J)
   20 B(I)=T
      PRINT 500
  500 FORMAT(///,5X,  68HORIGINAL SOLUTION VECTOR, USED TO COMPUTE THE R
     1IGHT-HAND SIDE VECTOR)
      PRINT 300,(X(J),J=1,N)
      CALL LINSYS(A,10,N,B,X,IS,LF,LP,WORK)
      IF(IS.EQ.1) GO TO 777
      PRINT 600
  600 FORMAT(/,5X,  34HSOLUTION VECTOR COMPUTED BY LINSYS)
      PRINT 300,(X(I),I=1,N)
   25 LF=.FALSE.
  777 CONTINUE
      END
*EXECUTE
*
*
      PROGRAM F405 B
      DIMENSION A(2,2),B(2),X(2),C(2),WORK(20)
      LOGICAL LP,LF
      DATA A/2.,1.,1.,-2./,B/3.,1./,C/9.,-2/
      LF=.TRUE.
      LP=.TRUE.
      PRINT 1
      CALL LINSYS(A,2,2,B,X,IS,LF,LP,WORK)
      PRINT 2,X,IS
      LF=.FALSE.
      CALL LINSYS(A,2,2,C,X,IS,LF,LP,WORK)
      PRINT 2,X,IS
  1   FORMAT(50X,9HTEST F405///)
  2   FORMAT(10X,2HX=,2F20.10,10X,3HIS=,I2///)
      STOP
      END
*EXECUTE
*
*
      PROGRAM F406
      DIMENSION ABAND(3,3),F(3,1)
      DIMENSION A(10,3) ,B(10,5),A1(10,3) ,C(10,5)
      DOUBLE PRECISION D(10,3) ,E(10,3)
      DATA ABAND/1.,2.,1.,2.,2.,1.,0.,1.,0./,F/3.,6.,3./
      DATA A/2.,10*-1.,9*2.,0.,8*-1.,0./,
     * A1/2.005,-0.997,-0.994,-0.991,-0.988,-0.985,-0.982,-0.979,-0.976,
     1    -0.973,-0.997,2.01,2.015,2.02,2.025,2.03,2.035,2.04,2.045,2.05
     2  ,0.,-0.994,-0.991,-0.988,-0.985,-0.982,-0.979,-0.976,-0.973,0./,
     * D/2.005D0,-0.997D0,-0.994D0,-0.991D0,-0.988D0,-0.985D0,-0.982D0,
     1   -0.979D0,-0.976D0,-0.973D0,-0.997D0,2.01D0,2.015D0,2.02D0,
     2   2.025D0,2.03D0,2.035D0,2.04D0,2.045D0,2.05D0,0.D0,-0.994D0,
     3   -0.991D0,-0.988D0,-0.985D0,-0.982D0,-0.979D0,-0.976D0,-0.973D0,
     4   0.D0/,
     *     B/1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,
     1       1.,-2.,3.,-4.,5.,-6.,7.,-8.,9.,-10.,
     2       10.,9.,8.,7.,6.,5.,4.,3.,2.,1.,
     3       10.,-9.,8.,-7.,6.,-5.,4.,-3.,2.,-1.,
     4      1.111,2.222,3.333,4.444,5.555,6.666,7.777,8.888,9.999,11.11/
C                  T E C T   F403
      CALL RBEQN(3,1,ABAND,3,IFAIL,1,F)
      PRINT 1,IFAIL
      PRINT 6,F
C                  T E C T   F420
      DO 10 I=1,10
      DO 10 J=1,5
  10  C(I,J)=B(I,J)
      DO 30 I=1,10
      DO 30 J=1,3
  30  E(I,J)= B(I,J)
      PRINT 3,((A(I,J),J=1,3),I=1,10)
      PRINT 4
      PRINT 2,((B(I,J),J=1,5),I=1,10)
      CALL RBEQN(10,1,A,10,IFAIL,5,B)
      PRINT 1,IFAIL
      PRINT 5
      PRINT 2,((B(I,J),J=1,5),I=1,10)
      PRINT 3,((A1(I,J),J=1,3),I=1,10)
      PRINT 4
      PRINT 2,((C(I,J),J=1,5),I=1,10)
      CALL RBEQN(10,1,A1,10,IFAIL,5,C)
      PRINT 1,IFAIL
      PRINT 5
      PRINT 2,((C(I,J),J=1,5),I=1,10)
      PRINT 13,((D(I,J),J=1,3),I=1,10)
      PRINT 4
      PRINT 12,((E(I,J),J=1,3),I=1,10)
      CALL DBEQN(10,1,D,10,IFAIL,3,E)
      PRINT 1,IFAIL
      PRINT 5
      PRINT 12,((E(I,J),J=1,3),I=1,10)
      STOP
  1   FORMAT(//10X,6HIFAIL=,I2)
  2   FORMAT(10X,5E20.10)
  3   FORMAT(1H1,50X,11HBAND MATRIX//(25X,3E20.10))
  4   FORMAT(//50X,16HRIGHT-HAND SIDES//)
  5   FORMAT(//50X,9HSOLUTIONS//)
  6    FORMAT(10X,3E20.10)
 12   FORMAT(10X,3D30.20)
  13  FORMAT(1H1,50X,11HBAND MATRIX//(25X,3D30.20))
      END
*CALL PTIME
*EXECUTE
*
*
       PROGRAM F413
      COMPLEX A,B,C,D,E,F,G,H,DET
      DIMENSION A(2,2),B(2,2),C(2,2),D(2,2),E(2,2),F(2,2),G(2,2),H(2,2),
     1V(2)
      DATA(A=1.,2.,3.,2.,-1.,-1.,1.,-1.),(E=-1.,0.,5.,0.,-0.5,0.,2.5,0.)
      DATA(B=1.,2.,3.,2.,-1.,-1.,1.,-1.),(F=-1.,0.,5.,0.,-0.5,0.,2.5,0.)
      DATA(C=1.,2.,3.,2.,-1.,-1.,1.,-1.),(G=-1.,0.,5.,0.,-0.5,0.,2.5,0.)
      DATA(D=1.,2.,3.,2.,-1.,-1.,1.,-1.),(H=-1.,0.,5.,0.,-0.5,0.,2.5,0.)
      PRINT 5,A,E
      CALL CMLIN(A,E,2,2,2,DET,V,0)
      PRINT 1,A,E,DET
      PRINT 6,B,F
      CALL CMLIN(B,F,2,2,2,DET,V,1)
      PRINT 2,B,F,DET
      PRINT 7,C,G
      CALL CMLIN(C,G,2,2,2,DET,V,2)
      PRINT 3,C,G,DET
      PRINT 8,D,H
      CALL CMLIN(D,H,2,2,2,DET,V,3)
      PRINT 4,D,H,DET
  1   FORMAT(30X,6HMODE=0//25X,2HA=,8F7.2/25X,2HE=,8F7.2/25X,4HDET=,
     12F7.2//)
  2   FORMAT(30X,6HMODE=1//25X,2HB=,8F7.2/25X,2HF=,8F7.2/25X,4HDET=,
     12F7.2//)
  3   FORMAT(30X,6HMODE=2//25X,2HC=,8F7.2/25X,2HG=,8F7.2/25X,4HDET=,
     12F7.2//)
  4   FORMAT(30X,6HMODE=3//25X,2HD=,8F7.2/25X,2HH=,8F7.2/25X,4HDET=,
     12F7.2//)
  5   FORMAT(30X,2HA=,8F7.2/30X,2HE=,8F7.2//)
  6   FORMAT (30X,2HB=,8F7.2/30X,2HF=,8F7.2//)
  7   FORMAT(30X,2HC=,8F7.2/30X,2HG=,8F7.2//)
  8   FORMAT(30X,2HD=,8F7.2/30X,2HH=,8F7.2//)
      END
*EXECUTE
*
*
       PROGRAM F420
      DIMENSION A(10),B(10),A1(10),B1(10),C(10,10),D(10),E(10)
      DATA((B(I),I=1,10)=10(2.)),((A(I),I=1,10)=9(-1.),0.),
     1((C(1,I),I=1,10)=2(1.),-1.,2(10.),-10.,1.1,1.11,2(1.111))
      N=10
      N1=N-1
      M=10
      A1(1)=A(1)
      B1(1)=B(1)
      DO 1 I=2,N
      I1=I-1
      A1(I)=A(I)
      B1(I)=B(I)
      C(I,1)=C(I1,1)+1
      C(I,2)=C(I,1)
      C(I,3)=C(I,1)
      C(I,4)=C(I1,4)-1
      C(I,5)=C(I,4)
       C(I,6)=C(I,4)
      C(I,7)=C(I1,7)+1.1
      C(I,8)=C(I1,8)+1.11
      C(I,9)=C(I1,9)+1.111
    1 C(I,10)=C(I,9)
      DO 2 I=2,N,2
      C(I,2)=-C(I,2)
      C(I,5)=-C(I,5)
    2 C(I,10)=-C(I,10)
      DO 3 I=1,N1,2
      C(I,3)=-C(I,3)
    3 C(I,6)=-C(I,6)
      PRINT 33
      PRINT 35,((C(I,J),J=1,M),I=1,N)
      TAS=0.
      TAK=0.
      DO 49 IJK=1,10
      SAT=0.
      SAK=0.
      DO 48 I=1,N
      SAT=SAT+TAS
      SAK=SAK+TAK
      A1(I)=A1(I)+SAT
      B1(I)=B1(I)+SAK
      A(I)=A1(I)
   48 B(I)=B1(I)
      PRINT 30
      PRINT 22,(A(I),I=1,N1)
      PRINT 32
      PRINT 22,(B(I),I=1,N)
      CALL PATVIL(N,M,1.,DET,A,B,C,D)
      PRINT 21
      PRINT 22,(B(I),I=1,N)
      PRINT 23
      PRINT 22,(A(I),I=1,N)
      PRINT 24
      PRINT 25,DET
      PRINT 26
      DO 52 I=1,N
      DO 50 J=1,N
      IF(J.GE.I) GO TO 51
      E(J)=A(I)*B(J)
      GO TO 50
   51 E(J)=B(I)*A(J)
   50 CONTINUE
   52 PRINT 35,(E(J),J=1,N)
      PRINT 27
      PRINT 35,((C(I,J),J=1,M),I=1,N)
      PRINT 36
      DO 56 I=1,N
      DO 54 J=1,N
      IF(J.GE.I) GO TO 53
      E(J)=A(I)*B(J)
      GO TO 54
   53 E(J)=B(I)*A(J)
   54 CONTINUE
      D(1)=E(1)*B1(1)+E(2)*A1(1)
      DO 55 J=2,N1
   55 D(J)=E(J-1)*A1(J-1)+E(J)*B1(J)+E(J+1)*A1(J)
      D(N)=E(N1)*A1(N1)+E(N)*B1(N)
   56 PRINT 35,(D(J),J=1,N)
      DO 45 I=1,M
      D(1)=B1(1)*C(1,I)+A1(1)*C(2,I)
      DO 46 J=2,N1
   46 D(J)=A1(J-1)*C(J-1,I)+B1(J)*C(J,I)+A1(J)*C(J+1,I)
      D(N)=A1(N1)*C(N1,I)+B1(N)*C(N,I)
      DO 47 J=1,N
   47 C(J,I)=D(J)
   45 CONTINUE
      PRINT 28
      PRINT 35,((C(I,J),J=1,M),I=1,N)
      TAS=0.003
   49 TAK=0.005
   21 FORMAT(//46X,26HTHE COMPONENTS OF VECTOR V/)
   22 FORMAT(10(X,E11.4)//)
   23 FORMAT(46X,26HTHE COMPONENTS OF VECTOR W/)
   24 FORMAT(40X,34HDETERMINANT OF TRI DIAGONAL MATRIX/)
   25 FORMAT(50X,E15.8//)
   26 FORMAT(50X,16HMATRIX INVERSION/)
   27 FORMAT(//20X,79HTHE SOLUTIONS OF A LINEAR ALGEBRAIC SYSTEM WITH TR
     1I DIAGONAL COEFFICIENT MATRIX/)
   28 FORMAT(//55X,8H(AB)*X=C/)
   30 FORMAT(49X,23HVECTOR OF COEFFICIENT A/)
   31 FORMAT(2X,10(7X,F4.1)//)
   32 FORMAT(48X,24HVECTOR OF COEFFICIENTS B/)
   33 FORMAT(43X,34HMATRIX OF COEFFICIENTS RIGHT PARTS/)
   35 FORMAT(10(X,E11.4))
   36 FORMAT(//53X,13HE=(AB)*(AB)-1/)
      END
*EXECUTE
*
*
      PROGRAM F421
       COMMON/OUTL/ITL,KPL,KPFL
       DIMENSION Z(30,30),ZL(10,10),XX(10),YR(10)
       KPL=1
       M=10
       N=10
       R=0.000000000001
       FR=0.0000001
       DO 10 K=1,2
       DO 2 I=1,M
       DO 1 J=1,N
   1   ZL(I,J)=1.
       IF(I-N) 3,3,2
   3   CONTINUE
       ZL(I,I)=ZL(I,I)+R*I+(K-1)*FR*(-1)**I
   2   YR(I)=N+R*I+(K-1)*FR*(-1)**(I-1)
       KPFL=1
       CALL SILLC(M,N,Z,ZL,XX,YR)
  10   CONTINUE
       END
       SUBROUTINE RELADI(N,NP,INDEX,A,Y,DF)
       DIMENSION A(N),DF(N)
       RETURN
       END
*EXECUTE
*
*
      PROGRAM F422
      DIMENSION S(99,4),B(6),Y(101),C(100,3)
      DATA (S=99(1.),99(0.),99(1.),99(0.)),
     *(B=2(1.,0.,0.)),(H=0.01),
     *(N=101),(L=100),(M=99)
      E=SQRT(2.)*100.$B(6)=-E*H*COS(E)
      PRINT 5,E
      CALL TSYS(S,B,Y,C,M,N,L)
      PRINT 3,(C(I,1),I=1,100)
      PRINT 4
      PRINT 3,Y
      PRINT 4
      DO 2 K=90,100,10
      E=K$Z=H*E$B(6)=-Z*COS(E)
      Z=2.-Z*Z$B(5)=Z*0.5
      DO 1 I=1,99
    1 S(I,2)=Z
      PRINT 5,E
      CALL TSYS(S,B,Y,C,M,N,L)
      PRINT 3,(C(I,1),I=1,100)
      PRINT 4
      PRINT 3,Y
      PRINT 4
    2 CONTINUE
      B(3)=B(1)-B(2)$B(6)=B(4)-B(5)
      DO 6 I=1,99
    6 S(I,4)=S(I,2)-(S(I,1)+S(I,3))
      CALL TSYS(S,B,Y,C,M,N,L)
      PRINT 3,Y
      PRINT 4
    3 FORMAT(10F12.7)
    4 FORMAT(//)
    5 FORMAT(2HK=,F10.3//)
      END
*EXECUTE
*
*
      PROGRAM F423
      DIMENSION A1(5000),A(5000),C(600),X(600),IIJ(600)
      NN=0
      DO 70 JU=1,2
      NN=NN+100
      JOB=1
      REWIND 1
      NNN0=5000
      NN1=NNN0/NN
      IF(NN1.GT.NN)NN1=NN
      KT0=NN1
      LLLL=0
      NNNN=0
      KKKK=0
      DO 2 K=1,NN
2     C(K)=0.
      LLL0=0
      DO 1 K=1,NN
      LLLL=LLLL+1
      KKKK=KKKK+1
      DO 3 L=1,NN
      NNNN=NNNN+1
      A(NNNN)=RNDM(-1.)-0.5
      C(L)=C(L)+A(NNNN)*KKKK
3     CONTINUE
      IF(LLLL.NE.NN1)GO TO 1
      LLLL=0
      NNNN=0
      WRITE(1) (A(LK),LK=1,NNN0)
      LLL0=LLL0+1
1     CONTINUE
      IF(LLLL.NE.0)WRITE(1) (A(LK),LK=1,NNN0)
      IF(LLLL.NE.0)LLL0=LLL0+1
      CALL AGAUSS(NNN0,NN,NN1,A,A1,IIJ,C,X,JOB)
      JOB=2
      PRINT 1000,(X(K),K=1,NN)
      DO 4 K=1,NN
4     C(K)=0.
      REWIND 1
      KKKK=0
      DO 5 K=1,LLL0
      READ(1) (A(LK),LK=1,NNN0)
      NNNN=0
      IF(KKKK+KT0.GT.NN)KT0=NN-KKKK
      DO 6 L=1,KT0
      KKKK=KKKK+1
      DO 7 M=1,NN
      NNNN=NNNN+1
      C(M)=C(M)+A(NNNN)*KKKK
7     CONTINUE
6     CONTINUE
5     CONTINUE
      JOB=2
      CALL AGAUSS(NNN0,NN,NN1,A,A1,IIJ,C,X,JOB)
      PRINT 1000,(X(K),K=1,NN)
70    CONTINUE
1000  FORMAT(1X,4E20.10)
      STOP
      END
*CALL FT*REGIM:F1=BUF
*CALL FT*REGIM:F2=BUF
*EXECUTE
*
*
      PROGRAM F424
C         *FILE:SCRATCH,F01,W,100
C         *FILE:SCRATCH,F02,W,100
      DIMENSION A(100),A1(100),III(20),NN(20),C(20)
      EXTERNAL FUNCT
      NDIM=20
      N1=NDIM-1
      C(1)=2.*1+1.*2
      DO 1 K=2,N1
  1   C(K)=(K-1)*1.+K*2.+(K+1)*1.
      C(NDIM)=N1*1.+NDIM*2.
      LB=100
      IND=1
      CALL SQROOT(A,A1,III,NN,NDIM,C,LB,IND,FUNCT)
      PRINT 100,C
 100  FORMAT(1X,5E20.10)
      STOP
      END
      FUNCTION FUNCT(L,M)
      IF(L.EQ.M) GO TO 1
      IF(IABS(L-M).EQ.1) GO TO 2
      FUNCT=0.
      RETURN
  1   FUNCT=2.
      RETURN
  2   FUNCT=1.
      RETURN
      END
*CALL PTIME
*EXECUTE
*
*
      PROGRAM F500
      DIMENSION A(8,5),IW(8,5)
      DIMENSION V(5,20),W(20,10)
      M=8
      N=5
      MAXV=20
      NV=5
      EPS=1.E-10
      IOUT=1
      MA=8
      A(1,1)=0.799217  $ A(1,2)=0.864099  $ A(1,3)=0.160325
      A(1,4)=0.632229  $ A(1,5)=0.052457
      A(2,1)=0.466098  $ A(2,2)=0.375965  $ A(2,3)=0.596879
      A(2,4)=0.271234  $ A(2,5)=0.072917
      A(3,1)=0.791670  $ A(3,2)=0.887416  $ A(3,3)=0.650871
      A(3,4)=0.039460  $ A(3,5)=0.048344
      A(4,1)=0.367939  $ A(4,2)=0.400156  $ A(4,3)=0.476228
      A(4,4)=0.457051  $ A(4,5)=0.462931
      A(5,1)=0.845622  $ A(5,2)=0.513245  $ A(5,3)=0.942230
      A(5,4)=0.099140  $ A(5,5)=0.553517
      A(6,1)=0.341419  $ A(6,2)=0.891279  $ A(6,3)=0.227223
      A(6,4)=0.340023  $ A(6,5)=0.059538
      A(7,1)=0.800058  $ A(7,2)=0.926708  $ A(7,3)=0.638934
      A(7,4)=0.819688  $ A(7,5)=0.514403
      A(8,1)=0.464460  $ A(8,2)=0.330900  $ A(8,3)=0.811808
      A(8,4)=0.546348  $ A(8,5)=0.262904
      CALL LIHOIN(A,MA,M,N,MAXV,V,NV,NVEC,EPS,IOUT,W,IW)
      END
*EXECUTE
*
*
      PROGRAM F600
C     THIS IS A TEST PROGRAM FOR THE SINGULAR VALUE DECOMPOSITION
C     ROUTINE SVD, F600.
C     44 M*N-MATRICES (M=3,10, N=2,M) ARE CREATED FROM THEIR SINGULAR
C     VALUE DECOMPOSITION, SVD IS CALLED AND THE RESULTS ARE COMPARED
C     WITH THE ORIGINAL DATA.
C     THE RELATIVE ERRORS IN THE SINGULAR VALUES AND THE ABSOLUTE ERRORS
C     IN THE ELEMENTS OF THE UNITARY MATRICES ARE COMPUTED.
C     A TEST FOR UT*B (SEE COMMENTS IN SVD) IS ALSO MADE WHEN M>N. THE
C     COLUMNS IN B ARE THEN ORTHOGONAL TO THE COLUMNS OF U, WHICH SHOULD
C     GIVE A ZERO MATRIX. THE ABSOLUTE ERROR IN UT*B IS COMPUTED.
C     THE LARGEST ERROR FOUND IS PRINTED. IF THIS VALUE IS LESS THAN
C     10**-12 THE TEXT 'TEST PASSED' IS PRINTED.
C     THE TEST PROGRAM WAS WRITTEN 23.4.1975 BY EVA EDBERG, CERN.
C
      DIMENSION A(10,10),V(10,10),S(10),U1(10,10),V1(10,10),S1(10)
      DIMENSION B(10,10)
      INTEGER P
      ERRMAX=0.
      DO 50 M=3,10
      DO 50 N=2,M
      CALL UNITAR(U1,M,10)
      CALL UNITAR(V1,N,10)
      DO 5 I=1,N
      S1(I)=FLOAT(N-I)+RNDM(-1.)
      DO 5 J=1,N
    5 V(J,I)=V1(J,I)*S1(I)
      DO 10 I=1,M
      DO 10 J=1,N
      A(I,J)=0.
      DO 10 K=1,N
   10 A(I,J)=A(I,J)+U1(I,K)*V(J,K)
      IF(N.EQ.M) GO TO 25
C    -----
C     IF M>N, A IS COPIED TO A MATRIX B, WHERE ALSO THE LAST M-N COLUMNS
C     ARE SET EQUAL TO THE LAST COLUMNS OF U1.
      P=M-N
      NP1=N+1
      DO 20 I=1,M
      DO 15 J=1,N
   15 B(I,J)=A(I,J)
      DO 20 J=NP1,M
   20 B(I,J)=U1(I,J)
      CALL SVD(B,S,V,10,10,M,N,P,.TRUE.,.TRUE.)
      CALL CSV(S,S1,N,ERRMX)
      IF(ERRMX.GT.ERRMAX) ERRMAX=ERRMX
      CALL CZR(B(1,NP1),10,N,P,ERRMX)
      IF(ERRMX.GT.ERRMAX) ERRMAX=ERRMX
C     ADDITIONAL OUTPUT
C     PRINT 700,ERRMX
C    -----
   25 CONTINUE
      CALL SVD(A,S,V,10,10,M,N,0,.TRUE.,.TRUE.)
      CALL CSV(S,S1,N,ERRMX)
C     ADDITIONAL PRINTING
C     PRINT 100,ERRMX
      IF(ERRMX.GT.ERRMAX) ERRMAX=ERRMX
      CALL CMTRX(A,U1,10,M,N,ERRMX)
C     ADDITIONAL PRINTING
C     PRINT 200,ERRMX
      IF(ERRMX.GT.ERRMAX) ERRMAX=ERRMX
      CALL CMTRX(V,V1,10,N,N,ERRMX)
C     ADDITIONAL PRINTING
C     PRINT 300,ERRMX
      IF(ERRMX.GT.ERRMAX)  ERRMAX=ERRMX
   50 CONTINUE
      PRINT 400,ERRMAX
      IF(ERRMAX.GE.1.E-10) PRINT 500
C 100 FORMAT(///,5X,*MAX. REL. ERROR IN SINGULAR VALUE*,E15.5)
C 200 FORMAT(5X,*MAX. ERROR IN THE MATRIX U       *,E15.5)
C 300 FORMAT(5X,*MAX. ERROR IN THE MATRIX V       *,E15.5)
  400 FORMAT(///,5X,  10HMAX. ERROR,E15.5)
  500 FORMAT(////,10X,  15HTEST NOT PASSED)
C 700 FORMAT(5X,*MAX. ERROR IN THE ZERO MATRIX    *,E15.5)
      END
      SUBROUTINE CSV(S,V,N,ERM)
      DIMENSION S(N),V(N)
      ERM=0.
      DO 10 I=1,N
      ERR=ABS((S(I)-V(I))/V(I))
      IF(ERR.GT.ERM) ERM=ERR
   10 CONTINUE
      RETURN
      END
      SUBROUTINE CMTRX(A,B,MDIM,M,N,ERM)
      DIMENSION A(MDIM,N),B(MDIM,N)
      ERM=0.
C     COMPARE EVERY COLUMN SEPARATELY, SINCE THE CORRESPONDING COLUMNS
C     IN A AND B CAN HAVE DIFFERENT SIGNS.
      DO 20 J=1,N
      DO 5 I=1,M
      IF(ABS(A(I,J)).LT.1.E-5.OR.ABS(B(I,J)).LT.1.E-5) GO TO 5
      SGN=SIGN(1.,A(I,J)*B(I,J))
      GO TO 6
    5 CONTINUE
      PRINT 100
      STOP
    6 IF(SGN.LT.0.) GO TO 8
      DO 7 I=1,M
      ERR=ABS(A(I,J)-B(I,J))
      IF( ERR.GT.ERM) ERM=ERR
    7 CONTINUE
      RETURN
    8 DO 9 I=1,M
      ERR=ABS(A(I,J)+B(I,J))
      IF( ERR.GT.ERM) ERM=ERR
    9 CONTINUE
   20 CONTINUE
      RETURN
  100 FORMAT(10X,  42HCOLUMN WITH TOO SMALL ELEMENTS ENCOUNTERED)
      END
      SUBROUTINE UNITAR(U,N,NDIM)
      DIMENSION U(NDIM,1)
      DO 1 I=1,N
      DO 1 J=1,N
 1    U(I,J)=-1.+2.*(RNDM(-1.))
      DO 2 J=1,N
      UJ=0.
      DO 3 I=1,N
 3    UJ=UJ+U(I,J)*U(I,J)
      IF(J.EQ.N) GOTO 7
      KSTA=J+1
      DO 4 K=KSTA,N
      UJK=0.
      DO 5 I=1,N
 5    UJK=UJK+U(I,J)*U(I,K)
      CJK=UJK/UJ
      DO 6 I=1,N
 6    U(I,K)=U(I,K)-CJK*U(I,J)
 4    CONTINUE
 7    UJ=1./SQRT(UJ)
      DO 8 I=1,N
 8    U(I,J)=UJ*U(I,J)
 2    CONTINUE
      RETURN
      END
      SUBROUTINE CZR(A,MDIM,M,N,ERM)
C     THIS SUBROUTINE GIVES THE LARGEST (IN ABSOLUTE VALUE) ELEMENT IN A
C     MATRIX.
      DIMENSION A(MDIM,N)
      ERM=0.
      DO 10 I=1,M
      DO 10 J=1,N
      IF(ABS(A(I,J)).GT.ERM) ERM=ABS(A(I,J))
   10 CONTINUE
      RETURN
      END
*EXECUTE
*
*
      PROGRAM G100
      DIMENSION P(1000),N2(1000),U(1000)
      DATA L/0/
      DATA((U(N),N=1,30)=0.455,0.713,1.005,1.06,1.14,1.13,1.24,2.03,
     A4.86,7.0,9.0,12.3,16.2,14.3,18.4,16.3,14.4,13.7,12.7,12.4,11.6,
     B10.6,10.2,12.0,14.6,17.3,20.7,23.6,28.3,33.5)
      DO 1 K=1,10
      T=0.
      DO 1 M=1,50
      L=L+1
      N2(L)=L
      IF(N2(L).LE.30)GO TO 4
       U(L)=FLOAT(N2(L))* RND(T)
   4  P(L)=PROB(U(L),N2(L))
    1 CONTINUE
      PRINT 1973
 1973 FORMAT(//,3(6X,2H N,10X,4HCHI2,10X,4HPROB),/)
      PRINT 2,(N2(J) , U (J),  P(J),J=1,500)
   2  FORMAT(3(2X,I6,2F14.7))
      END
      FUNCTION RND(T)
       DIMENSION U(2)
      IF(T)1,2,1
   2  U(1)=3.14159265
      U(2)=0.542101887
    1 T=U(1)+U(2)
      U(1)=U(2)
        IF(T-4.)3,4,4
  4   T=T-4.
  3   U(2)=T
      RND=T/4.
       RETURN
       END
*EXECUTE
*
*
      PROGRAM G101
C         TEST CHISIN G101
      DIMENSION P(6),S(6,33)
      DIMENSION P2(6)
      DIMENSION NB(33),NA(6)
      DATA P/0.95, 0.8, 0.5, 0.2, 0.01, 0.005/
      DO 29 I=1,30
   29 NB(I)=I
      NB(31)=40
      NB(32)=60
      NB(33)=120
C     COMPUTE TABLE VALUES TO TEST CHISIN
      DO 20 I=1,6
      DO 20 NDEX= 1, 33
      N = NB(NDEX)
   20 S(I,NDEX) = CHISIN(P(I),N)
      PRINT 21
   21 FORMAT(1H1,10X,' TEST VALUES FOR CHISIN'//)
   42 CONTINUE
      PRINT 22,(P(I),I=1,6)
      PRINT 94
      DO 23 NDEX= 1, 33
      N = NB(NDEX)
   23 PRINT 24, N,(S(I,NDEX),I=1,6)
      PRINT 95
      PRINT 22,(P(I),I=1,6)
      PRINT 94
      DO 27 NDEX= 1, 33
      N = NB(NDEX)
      DO 26 I= 1, 6
      P2(I) = PROB(S(I,NDEX),N)
   26 CONTINUE
      PRINT 24,N,P2
   27 CONTINUE
   22 FORMAT (7X,6F18.9)
   24 FORMAT (2X,I5,6F18.9)
   94 FORMAT (1H )
   95 FORMAT (40H1  TEST IF INVERSE OF INVERSE IS 1          //)
      END
*EXECUTE
*
*
      PROGRAM G102
C         MAKE TABLE OF FUNCTION VALUES
      Z = 0.
      PRINT 510
      DO 100 I= 1,170
      Z = Z + 0.025
      P = PROBKL(Z)
      PRINT 500, Z,P
  100 CONTINUE
C         TEST CONTINUITY AROUND 1.
      Z = 0.99995
      DO 300 I= 1, 20
      P1 = PROBKL(Z)
      Z = Z + 0.000005
      P2 = PROBKL(Z)
      PDIFF = P2-P1
      PRINT 530, Z,P2, PDIFF
  300 CONTINUE
  500 FORMAT (10X,F10.5,E17.9)
  510 FORMAT (1H1)
  530 FORMAT (20X,F12.8,E17.9,E14.6)
      END
*EXECUTE
*
*
      PROGRAM G104
C    PROGRAM TO TEST THE ROUTINES
C          STUDIN
C          STUDIS
      DIMENSION P(6),S(6,33), P2(6),NB(33)
      DATA P/0.95, 0.8, 0.5, 0.2, 0.01, 0.005/
C    COMPUTE TABLE VALUES TO TEST STUDIN
      N1=1
      DO 1 I=1,6
      X=0.5*(1.+P(I))
      S(I,31)=STUDIN(X,40)
      S(I,32)=STUDIN(X,60)
      S(I,33)=STUDIN(X,120)
      DO 1 N=1,30
    1 S(I,N)=STUDIN(X,N)
C      PRINT TABLE VALUES
      PRINT 11
   11 FORMAT(1H1,10X,' TEST VALUES FOR STUDIN'//)
    6 PRINT 2,(P(I),I=1,6)
      DO 3 N=1,30
    3 PRINT 4,N,(S(I,N),I=1,6)
      N=40
      PRINT 4,N,(S(I,31),I=1,6)
      N=60
      PRINT 4,N,(S(I,32),I=1,6)
      N=120
      PRINT 4,N,(S(I,33),I=1,6)
      IF(N1. EQ. 2) STOP
C     COMPUTE INVERSE TABLE VALUES TO TEST STUDIS
  10  PRINT 12
   12 FORMAT(1H1,10X,' TEST VALUES FOR STUDIS'//)
      DO 5 I=1,6
      S(I,31)=2.*STUDIS(S(I,31),40) -1.
      S(I,32)=2.*STUDIS(S(I,32),60) -1.
      S(I,33)=2.*STUDIS(S(I,33),120)-1.
      DO 5 N=1,30
    5 S(I,N)=2.*STUDIS(S(I,N),N)-1.
      N1=N1+1
C    PRINT INVERSE TABLE VALUES
      GO TO 6
    2 FORMAT (7X,6F18.9/)
    4 FORMAT (2X,I5,6F18.9)
      END
*EXECUTE
*
*
      PROGRAM G106
      DIMENSION P(1000),N2(1000),U(1000),P1(1000),P2(1000)
      DATA L/0/
      DATA((U(N),N=1,30)=0.455,0.713,1.005,1.06,1.14,1.13,1.24,2.03,
     A4.86,7.0,9.0,12.3,16.2,14.3,18.4,16.3,14.4,13.7,12.7,12.4,11.6,
     B10.6,10.2,12.0,14.6,17.3,20.7,23.6,28.3,33.5)
      DO 1 K=1,10
      T=0.
      DO 1 M=1,50
      L=L+1
      N2(L)=L
      IF(N2(L).LE.30)GO TO 4
       U(L)=FLOAT(N2(L))* RND(T)
   4  P(L)=PROB(U(L),N2(L))
      P1(L)=1.0-GAMDIS(U(L)/2.,FLOAT(N2(L))/2.)
      P2(L)=(P1(L)-P(L))/P(L)
    1 CONTINUE
      PRINT 10,(P2(J),J=1,500)
  10  FORMAT((2X,5E20.10)/)
      END
      FUNCTION RND(T)
       DIMENSION U(2)
      IF(T)1,2,1
   2  U(1)=3.14159265
      U(2)=0.542101887
    1 T=U(1)+U(2)
      U(1)=U(2)
        IF(T-4.)3,4,4
  4   T=T-4.
  3   U(2)=T
      RND=T/4.
       RETURN
       END
*EXECUTE
*
*
      PROGRAM G110
C     TEST RUN FOR THE LANDAU DISTRIBUTION
      DIMENSION X(10),Y(10)
      READ 3,(X(I),I=1,10)
      READ 3,(Y(I),I=1,10)
    3 FORMAT(8F10.0)
      PRINT 99
   99 FORMAT(1H1,23X,39HTEST VALUES FOR THE LANDAU DISTRIBUTION//)
      PRINT 100
  100 FORMAT(10X,8HABSCISSA,5X,7HINVERSE,6X,7HDENSITY,8X,7HDENSITY,4X,
     114H  DISTRIBUTION/21X,11HCONDITIONAL,4X,8HFUNCTION,7X,8HFUNCTION,
     25X,10H  FUNCTION/20X,13HDISTR. FUNCT.,3X,8H(DISLAN),4X,14H(BORSCH-
     3SUPAN)/)
      DO 1 I = 1,10
      F=DISLAN(X(I),0)
      G=DISLAN(X(I),1)
      U=DINLAN(G/0.99)
      PRINT 101,X(I),U,F,Y(I),G
  101 FORMAT(F17.3,F12.3,F14.5,2F15.5)
    1 CONTINUE
      END
*CALL PTIME
*EXECUTE
-3.5      -2.5      -1.5      0.0       5.0       10.0      20.0      30
50.0      80.0
0.00001   0.00964   0.10055   0.17886   0.03917   0.01198   0.00300   0.
0.00045   0.00017
*
*
      PROGRAM G111
C     TEST RUN FOR THE VAVILOV DISTRIBUTION
      DIMENSION X(10),Y(10),Z(3,10)
      M=0
      RKA=0.01
      BE2=0.
      S=0.99
  106 READ 3,(X(I),I=1,10)
      READ 3,(Y(I),I=1,10)
    3 FORMAT(8F10.0)
      PRINT 99
   99 FORMAT(1H1,23X,40HTEST VALUES FOR THE VAVILOV DISTRIBUTION//)
      PRINT 104,RKA,BE2
  104 FORMAT(29X,7HKAPPA =,F5.2,4X,9HBETA**2 =,F5.1//)
      CALL COEDIS(RKA,BE2,0,-2)
      DO 1 I = 1,10
    1 Z(1,I)=DISVAV(X(I),0)
      CALL COEDIN(RKA,BE2,-2)
      PRINT 100
  100 FORMAT(10X,8HABSCISSA,5X,7HINVERSE,2(6X,7HDENSITY,3X),14H  DISTRIB
     1UTION/21X,11HCONDITIONAL,2(4X,8HFUNCTION,4X),10H  FUNCTION/20X,13H
     2DISTR. FUNCT.,3X,8H(DISVAV),4X,16H(SELTZER-BERGER)/)
      DO 2 I = 1,10
      Z(2,I)=DISVAV(X(I),1)
    2 Z(3,I)=DINVAV(Z(2,I)/S)
      DO 4 I = 1,10
  4   PRINT 101,X(I),Z(3,I),Z(1,I),Y(I),Z(2,I)
  101 FORMAT(F17.3,F12.3,F14.5,2F15.5)
      IF(M .EQ. 1) GO TO 105
      M=1
      RKA=4.0
      BE2=0.5
      S=0.995
      GO TO 106
  105 CONTINUE
      END
*EXECUTE
-3.0      -2.0      -1.0      0.0       5.0       10.0      15.0      20
30.0      34.0
0.00068   0.04440   0.15300   0.18100   0.03960   0.01210   0.00546   0.
0.00131   0.00101
-3.5      -3.25     -3.0      -2.75     -2.5      -2.25     -2.0      -1
-1.5      -1.25
0.00945   0.06880   0.26400   0.60000   0.88100   0.89500   0.66300   0.
0.16400   0.05850
*
*
      PROGRAM G200
C     THE  SUBROUTINE   S T U D N T   TEST
      DIMENSION S(8),NSTEP(6),NBEG(6),NFIN(6),T(8)
      DATA NBEG/1,30,50,70,100,300/,
     $NFIN/29,48,65,90,250,500/,
     $S/.6826894921,.9,.95,.9544997361,.98,.99,.9973002039,.999/,
     $NSTEP/1,2,5,10,50,100/
      PRINT 30
      CALL STUDNT (-0.2,5,0.01,MIST,6HB25..1,COEF)
      GO TO (1,3,3,3,3),MIST
  1   PRINT 20,COEF
  3   PRINT 10,MIST
      CALL STUDNT (0.5,5,-0.000000001,MIST,6HB25..2,COEF)
      GO TO (4,7,7,7,7),MIST
  4   PRINT 20,COEF
  7   PRINT 10,MIST
      CALL STUDNT (0.5,5,37.,MIST,6HB25..3,COEF)
      GO TO (8,9,9,9,9),MIST
  8   PRINT 20,COEF
  9   PRINT10,MIST
      CALL STUDNT (0.5,-600,0.001,MIST,6HB25..4,COEFF)
      GO TO (11,13,13,13,13),MIST
  11  PRINT 20,COEF
  13  PRINT10,MIST
      CALL STUDNT (0.9,2,0.005,MIST,6HB25..5,COEF)
      GO TO (14,15,15,15,15),MIST
  14  PRINT 20,COEF
  15  PRINT10,MIST
      CALL STUDNT (0.9,2,0.00000001,MIST,6HB25..6,COEF)
      GO TO(16,17,17,17,17),MIST
  16  PRINT 20,COEF
  17  PRINT10,MIST
      M=31
      DO 2 I=1,6
      J1=NBEG(I)
      J2=NFIN(I)
      J3=NSTEP(I)
      DO 12 J=J1,J2,J3
      DO 22 K=1,8
  22  CALL STUDNT (S(K),J+1,0.0001,MIST,6HB25..7,T(K))
      IF(M.EQ.31)GOTO5
      GO TO 6
  5   M=1
      PRINT 40,S
  6   M=M+1
      PRINT 60,J,T
  12  CONTINUE
  2   CONTINUE
      STOP
  10  FORMAT(1H+,81X,5HMIST=,I2//)
  20  FORMAT (20X, 22HSTUDENT COEFFICIENT = ,E17.10)
  30  FORMAT(//50X,9HTEST G200//)
  40  FORMAT(1H1//////45X 21HSTUDENT COEFFICIENTS /////
     $55X 13HPROBABILITIES // 4H N-1 4X 8F14.10 //)
  60  FORMAT (I4 ,4X 8(F14.3))
      END
*EXECUTE
*
*
      PROGRAM G900
      DOUBLE PRECISION D(10),DRANF,SEED,A(10)
      DIMENSION B(6),R(10)
      DUMMY=1.
      DO 1 I=1,10
      R(I)=RANF(DUMMY)
      CALL RANGET(SEED)
  1   A(I)=SEED
      PRINT 2,R,A
  2   FORMAT(//50X,'TEST OF G900'//5X,'R=',10E10.4/5X,'A=',10D10.4)
      CALL RANSET(A(5))
      DO 4 I=1,6
  4   B(I)=RANF(DUMMY)
      PRINT 5,B
  5   FORMAT(5X,'B=',6E10.4)
      STOP
      END
*EXECUTE
*
*
      PROGRAM G901
      REAL X(10),Y(10),Z(10),X1(3),Y1(3),Z1(3),R1(3)
      RADIUS=2.0
      DO 1 I=1,10
      CALL RAN2VS(RADIUS,X(I),Y(I))
  1   CONTINUE
      PRINT 2,X,Y
      RADIUS=0.5
      DO 3 I=1,10
      CALL RAN3VS(RADIUS,X(I),Y(I),Z(I))
  3   CONTINUE
      PRINT 4,X,Y,Z
      RADIUS=1.5
      CALL VRAN2S(RADIUS,3,X1,Y1,R1)
      PRINT 5,X1,Y1
      RADIUS=3.0
      CALL VRAN3S(RADIUS,2,X1,Y1,Z1,R1)
      PRINT 6,(X1(I),I=1,2), (Y1(K),K=1,2), (Z1(J),J=1,2)
 2    FORMAT(1H1//10X,'TEST OF G901(RAN2VS)',/5X,'X=',10(2X,F8.5),
     *      /5X,'Y=',10(2X,F8.5)//)
  4   FORMAT(//10X,'TEST OF G901(RAN3VS)',/5X,'X=',10(2X,F8.5),
     *      /5X,'Y=',10(2X,F8.5),/5X,'Z=',10(2X,F8.5)//)
  5   FORMAT(//10X,'TEST OF G901(VRAN2S)',/5X,'X=',3F12.5/
     *       5X,'Y=',3F12.5//)
  6   FORMAT(//10X,'TEST OF G901(VRAN3S)',/5X,'X=',2F12.5/
     *       5X,'Y=',2F12.5/5X,'Z=',2F12.5//)
      STOP
      END
*EXECUTE
*
*
      PROGRAM H100
      DOUBLE PRECISION A(20,30), AA(20,30), B(20), BB(20), C(30), CC(30)
      DOUBLE PRECISION Z0, ZZ0, Q(100)
      INTEGER          IROW(20), ICOL(30), LCV(80), MN(4), FALL
      DIMENSION        X(50), Y(30), TITLE(8)
      MD = 20
      READ 100, NUMBER
      DO 50  NUM = 1, NUMBER
      READ 101, TITLE
      READ 102, MN
      M = MN(2)
      N = MN(4)
      DO 5  K = 1, N
    5 READ 103, (A(I,K),  I = 1,M), C(K)
      READ 103, (B(I),  I = 1, M),  Z0
      DO 7  K = 1, N
      DO 6  I = 1, M
    6 AA(I,K) = A(I,K)
    7 CC(K) = C(K)
      DO 8  I = 1, M
    8 BB(I) = B(I)
      ZZ0 = Z0
      CALL SIMPLE (AA,BB,CC,ZZ0,MN,MD,IROW,ICOL,LCV,Q,MODE,2)
      PRINT 110, TITLE
      CALL SIMPLE (A,B,C,Z0,MN,MD,IROW,ICOL,LCV,Q,FALL,0)
      IF(FALL .EQ. 0)  PRINT 111
      IF(FALL .EQ. 1)  PRINT 112
      IF(FALL .EQ. 2)  PRINT 113
      IF(FALL .EQ. 3)  PRINT 114
      IF(FALL .EQ. 1  .OR.  FALL .EQ. 2)  GOTO 50
      IND = M+N
      DO 10  I = 1, IND
   10 X(I) = SNGL(Q(I))
      Z = SNGL(Z0)
      DO 30  K = 1, N
      IND = M + K
   30 Y(K) = X(IND)
      IF(M-N)  35,35,40
   35 PRINT 120, (IND, X(IND), IND, Y(IND), IND = 1, M)
      IF(M .EQ. N)  GOTO 45
      M1 = M + 1
      PRINT 121, (IND, Y(IND),  IND = M1, N)
      GOTO 45
   40 PRINT 120, (IND, X(IND), IND, Y(IND),  IND = 1, N)
      N1 = N + 1
      PRINT 122, (IND, X(IND),  IND = N1, M)
   45 PRINT 123,  Z
   50 CONTINUE
  100 FORMAT(I5)
  101 FORMAT(10A6)
  102 FORMAT(4I5)
  103 FORMAT(8D10.0)
  110 FORMAT( // 10A6/1H ,70 (1H-)// )
  111 FORMAT(  36H THE PROBLEM HAS ONE FINITE SOLUTION///)
  112 FORMAT(  52H THE PROBLEM HAS NO FINITE SOLUTION (MAX = INFINITE))
  113 FORMAT(  52H NO FEASIBLE INITIAL SILUTION EXISTS FOR THE PROBLEM)
  114 FORMAT(  38H MORE THAN ONE MAXIMUM SOLUTION EXISTS///)
  120 FORMAT(   2H X,I2,   3H = ,E22.12,22X,   1HY,I2,   3H = ,E22.12)
  121 FORMAT(51X,   1HY,I2,   3H = ,E22.12)
  122 FORMAT(   2H X,I2,   3H = ,E22.12)
  123 FORMAT(  44H0MAXIMUM VALUE OF THE OBJECTIVE FUNCTION  = ,E22.12)
      END
*CALL PTIME
*EXECUTE
    9
EXAMPLE 1,  DEGENERACIES
    4    4    3    3
   -0.04    -60.     0.25       9.        0.
   -0.02     -90.     0.5      3.         0.
    1.      0.        0.        0.        1.
   -0.02     150.     -0.75        6.      0.
EXAMPLE 2,   NO RESTRICTIONS FOR THE VARIABLES X(I)
    0    2    6    6
    -2.      5.       20.
    -2.      15.       0.
     2.       5.       -40.
    2.      1.        -16.
    6.      -1.       0.
     1.      -1.      10.
     -1.      -1.      0.
EXAMPLE 3,   EQUATION CONSTRAINT
    0    2    6    7
    -2.      5.       20.
    -2.      15.       0.
     2.       5.     -40.
    2.      1.        -16.
    6.      -1.       0.
     1.      -1.      10.
    -10.     3.       0.
     -1.      -1.      0.
EXAMPLE 4,  MORE THAN ONE SOLUTION
    2    2    3    3
    10.      20.      1100.
    1.      4.       160.
     1.       1.      100.
    -1.      -2.       0.
EXAMPLE 5,    NO FEASIBLE INITIAL SOLUTION EXISTS
    0    2    7    7
    -2.      5.       20.
    -2.      15.       0.
     2.       5.       -40.
    2.      1.        -16.
    6.      -1.       0.
     1.      -1.      10.
   -1.       0.       -1.
     -1.      -1.      0.
EXAMPLE 6,   MAXIMUM = INFINITE
    0    2    6    6
    -2.      5.       20.
    -2.      15.       0.
     2.       5.       -40.
    2.      1.        -16
    6.      -1.       0.
     1.      -1.      10.
     1.      1.      0.
EZAMPLE 7,  MAXIMUM IN DEGENERATED COIN
    0    2    6    6
    -2.      5.       20.
    -2.      15.       0.
     2.       5.       -40.
    2.      1.        -16.
    6.      -1.       0.
     1.      -1.      10.
    0.     -1.      0.
EXAMPLE 8,   ALL THE  C(K) ARE NEGATIVE
    2    2    3    3
    -1.     -4.      -8.
   -2.      -3.      -12.
    -2.     -1.       -6.
    1.      1.       0.
EXAMPLE 9,  ONLY EQUALITIES AS CONSTRAINTS
    2    4    0    4
    -4.      -9.       3.        -7.       2.
    -5.     2.        -7.        -1.        12.
    -10.       -5.       2.       -1.      -35.
     -2.        3.        -2.     3.        -11.
    -4.      -10.       4.        5.       -14.
*
*
      PROGRAM H300
      DIMENSION A(20,20),B(400),C(20,20),K(20),II(20)
      EQUIVALENCE (A,B)
      READ 10,(B(I),I=1,400)
      PRINT 12
      DO 5 I=1,20
    5 II(I) = I
      PRINT 14
      PRINT 18,(II(I),I=1,20)
      DO 1 I=1,20
    1 PRINT 11,I,(A(I,J),J=1,20)
      DO 2 J=1,2
      DO 3 I1=1,20
      DO 3 I2=1,20
    3 C(I1,I2) = A(I1,I2)
      CALL ASSIGN(C,20,20,20,K,TOTAL,J)
      PRINT 13,J
      PRINT 15,(K(I),I=1,20)
      PRINT 16,TOTAL
      PRINT 17
      DO 4 I=1,20
    4 PRINT 11,I,(C(I,L),L=1,20)
    2 CONTINUE
      STOP
   10 FORMAT(8F10.4)
   11 FORMAT(I3,2X,20F6.3)
   12 FORMAT(1H1)
   13 FORMAT(////'MODE = ',I1,/'--------'//)
   14 FORMAT('GIVEN 20*20 SQUARE-MATRIX'//)
   15 FORMAT('THE ASSIGNED INTEGER VALUES ARE'//10X,20I4,//)
   16 FORMAT(/'WHICH MINIMIZE'//,15X,F10.4)
   17 FORMAT(//'THE MATRIX AFTER THE CALL SUBROUTINE ASSIGN'//)
  18  FORMAT(3X,20I6,/)
      END
*EXECUTE
.0        .298      .0        .271      .311      -0.187    .143      .0
.0        .0        -0.136    .0        .0        -0.136    .0        -0
-0.039    .346      -0.258    -0.161    .355      -0.27     -0.162    .1
-0.271    .554      .232      -0.275    -0.078    .355      -0.11     .0
.0        .0        .0        .0        .0        .0        .0        .3
.0        .0        .0        .0        .0        .0        .0        .0
-0.082    .0        .0        .0        .0        .0        .0        .0
.0        .415      -0.166    .0        .0        .0        .0        .0
-0.154    .0        .218      -0.226    .0        .453      .031      -0
-0.166    .0        .0        .0        -0.329    .0        -0.058    -0
.0        -0.121    .363      .029      .244      .362      .0        .0
.0        .104      .0        -0.368    .149      .519      .221      .1
-0.302    .068      .148      -0.278    .154      .0        -0.218    .0
.0        .0        -0.031    -0.309    -0.247    .227      .405      -0
.124      -0.034    -0.202    .0        .0        -0.358    .0        .0
.0        .0        .0        .083      .0        .0        -0.453    .1
.089      -0.09     .203      -0.344    .0        .049      .0        .0
.0        .317      -0.241    -0.008    -0.504    .114      .114      -0
-0.124    .181      .303      -0.225    .176       .0       -0.248    -0
.0        .218      -0.05     -0.351    -0.23     .103      -0.23     -0
.07       -0.056    -0.242    -0.052    -0.23     -0.231    .0        .0
-0.422    .116      .0        .454      .17       .0        .342      .1
.209      -0.104    .171      .169      -0.178    .233      -0.079    .3
.279      -0.416    .0        .021      .523      .0        .093      .3
-0.327    -0.101    -0.088    -0.156    .078      -0.111    -0.121    .1
.434      -0.215    .018      .375      -0.33     .154      .0        -0
-0.176    .0        .248      -0.103    -0.309    -0.103    .05       -0
-0.201    .051      -0.303    -0.204    -0.144    -0.287    .019      -0
-0.233    -0.047    .534      .0        .189      -0.409    .0        -0
-0.059    -0.308    .119      .377      .051      .233      .474      -0
.291      -0.071    -0.058    .111      .098      -0.135    .164      .4
.0        .0        -0.084    .0        .0        -0.462    .0        .0
-0.239    .0        -0.2 92    .106     .0        -0.193    .13       .1
.08       -0.234    .218      .024      -0.185    .191      -0.331    -0
.594      .278      .0        .0        .0        .0        .0        .0
.0        -0.571    .0        .0        .0        .0        .0        .0
.0        .0        .0        .0        .162      .169      -0.393    .0
.0        .0        .0        .0        .0        -0.308    -0.106    -0
-0.21     .689      .321      -0.232    .465      .259      .0        .0
.0        .244      .305      .0        .21       -0.836    .0        .0
-0.658    .0        .0        .0        .0        .0        -0.288    .3
-0.011    -0.098    .325      .126      .205      -0.759    .077      .1
-0.614    .0        .0        .0        .122      -0.212    -0.172    .7
.0        -0.323    -0.181    -0.376    .019      .0        .0        .0
.52       .0        -0.26     .371      .101      -0.122    -0.075    -0
.0        -0.745    .261      .022      .364      .0        .0        .0
-0.15     .0        -0.599     -0.287   .166      -0.197    .233      .2
-0.207    .667      .166      .032      -0.303     .134     .0        .0
.0        -0.269    .604      -0.133    .88       .0        -0.373    .0
-0.257    .0        -0.864    .0        .0        -0.16     .086      .1
*
*
      PROGRAM H600
C   USER  MUST  REQUIRE  TAPE  5
C        CARDS  OF  DATAS  196
      CALL NETB
      STOP
      END
*EXECUTE
13 MAY
1/2 DAYS
01 MAY   2
01 MAY   2
02 MAY
02 MAY
03 MAY
03 MAY
04 MAY
04 MAY
05 MAY
05 MAY
06 MAY
06 MAY
07 MAY   1
07 MAY   1
08 MAY   2
08 MAY   2
09 MAY
09 MAY
10 MAY
10 MAY
11 MAY
11 MAY
12 MAY
12 MAY
13 MAY
13 MAY
14 MAY   1
14 MAY   1
15 MAY   2
15 MAY   2
16 MAY
16 MAY
17 MAY
17 MAY
18 MAY
18 MAY
19 MAY   2
19 MAY   2
20 MAY
20 MAY
21 MAY   1
21 MAY   1
22 MAY   2
22 MAY   2
23 MAY
23 MAY
24 MAY
24 MAY
25 MAY
25 MAY
26 MAY
26 MAY
27 MAY
27 MAY
28 MAY   1
28 MAY   1
29 MAY   2
29 MAY   2
30 MAY   2
30 MAY   2
31 MAY
31 MAY
01 JUN
01 JUN
02 JUN
02 JUN
03 JUN
03 JUN
04 JUN   1
04 JUN   1
05 JUN   2
05 JUN   2
06 JUN
06 JUN
07 JUN
07 JUN
08 JUN
08 JUN
09 JUN
09 JUN
10 JUN
10 JUN
11 JUN   1
11 JUN   1
12 JUN   2
12 JUN   2
13 JUN
13 JUN
14 JUN
14 JUN
15 JUN
15 JUN
12211
F. K. HUEBNER, , LIBRARY GROUP, LWTA ROOM 322
EJECTION SS62 PROJECT
         25    9                    SLOW EJECTION 62 PROJECT
  119   163  4   4 1                MODIFY COIL PROTECTIONS
  127   137  2  17 1                CONNECT SEPTUM MAGNET
  137   140 10  17 1                RF TEST SEPTUM MAGNET
  139   163  4  15 1                INSTALL TV AND SUPPORTS
  140   163  2   7 2                FINAL VACUUM TEST SECTOR 6
  145   147  4  13 4                REMOUNT 012.2 - 012.3 - ME2 - ME3
  147   163  6   7 2                INSTALL VACUUM - EJECTED BEAM
  160   163  2   2 1                MOUNT EJECTED BEAM 62
         25    9                    MOVE MAGNET 102
  350   363  4   9 2                MAGNETIC MEASUREMENTS
  359   363  6   7 2                VAC. TESTS SECTOR
         15    9                    NEW TANK 26 PROJECT
  529   531  2                      MAKE DOWNSTREAM BOX
  531   549  6   7 1                VAC. TEST DOWNSTREAM BOX
  543   547  3   7 2                VAC. TEST IN RING
  547   549  6  19 2                HT RING TEST
  549   551  6                      CLEAN + MOUNT DOWNSTREAM BOX + CONNE
  551   553  2   2 2                INSTALL FINAL ASSEMPLY
  553   555  6   7 1                VACUUM TEST FINAL ASSEMPLY
99999100000                         LAST DATA CARD
               9                    SEPARATION CARD
9999925 MAY
               9                    SEPARATION CARD
  16325 MAY
  36325 MAY
  55525 MAY
               9                    SEPARATION CARD
12211
F. K. HUEBNER, , LIBRARY GROUP, LWTA ROOM 322
EJECTION SS62 PROJECT
         25    9                    SLOW EJECTION 62 PROJECT
  119   163  4   4 1                MODIFY COIL PROTECTIONS
  127   137  2  17 126 5            CONNECT SEPTUM MAGNET
  137   140 10  17 1                RF TEST SEPTUM MAGNET
  139   163  4  15 145 160 270 599 1INSTALL TV AND SUPPORTS
  145   147  4  13 4                REMOUNT 012.2 - 012.3 - ME2 - ME3
  140   163  2   7 2                FINAL VACUUM TEST SECTOR 6
  160   163  2   2 1                MOUNT EJECTED BEAM 62
  147   163  6   7 2                INSTALL VACUUM - EJECTED BEAM
         24    9                    MOVE MAGNET 102
  350   363  4   9 2                MAGNETIC MEASUREMENTS
  350   363  4   9 2                MAGNETIC MEASUREMENTS
  359   363  6   7 2                VAC. TESTS SECTOR
         15    9                    NEW TANK 26 PROJECT
  531   531  6   7 1                VAC. TEST DOWNSTREAM BOX
  531   549  6   7 1                VAC. TEST DOWNSTREAM BOX
  543   547  3   7 2                VAC. TEST IN RING
  547   549  6  19 2                HT RING TEST
  549   551  6                      CLEAN + MOUNT DOWNSTREAM BOX + CONNE
  551   553  2   2 2                INSTALL FINAL ASSEMPLY
  553   555  6   7 1                VACUUM TEST FINAL ASSEMPLY
  700   531  2                      MAKE DOWNSTREAM BOX
99999100000                         LAST DATA CARD
               9                    SEPARATION CARD
9999925 MAY
               9                    SEPARATION CARD
  16325 MAY
  36325 MAY
  55525 MAY
               9                    SEPARATION CARD
21112
F. K. HUEBNER, , LIBRARY GROUP, LWTA ROOM 322
EJECTION SS62 PROJECT
         25    9                    SLOW EJECTION 62 PROJECT
  119   163  4   4 1                MODIFY COIL PROTECTIONS
  127   137  21 17 1                CONNECT SEPTUM MAGNET
  137   140 101 17 1                RF TEST SEPTUM MAGNET
  139   163  4  15 1                INSTALL TV AND SUPPORTS
  140   163  21  7 2                FINAL VACUUM TEST SECTOR 6
  145   147  4  13 4                REMOUNT 012.2 - 012.3 - ME2 - ME3
  147   163  6   7 2                INSTALL VACUUM - EJECTED BEAM
  160   163  2   2 1                MOUNT EJECTED BEAM 62
         25    9                    MOVE MAGNET 102
  350   363  4   9 2                MAGNETIC MEASUREMENTS
  359   363  6   7 2                VAC. TESTS SECTOR
         15    9                    NEW TANK 26 PROJECT
  529   531  2                      MAKE DOWNSTREAM BOX
  531   549  6   7 1                VAC. TEST DOWNSTREAM BOX
  543   547  3   7 2                VAC. TEST IN RING
  547   549  6  19 2                HT RING TEST
  549   551  6                      CLEAN + MOUNT DOWNSTREAM BOX + CONNE
  551   553  2   2 2                INSTALL FINAL ASSEMPLY
  553   555  6   7 1                VACUUM TEST FINAL ASSEMPLY
99999100000                         LAST DATA CARD
  547   549 102                     DATA ALTERATION CARD
  553   557 10                             BOGUS DATA ALTERATION CARD
               9                    SEPARATION CARD
  70016 MAY
  553XX MAY
9999925 MAY
               9                    SEPARATION CARD
  70016 MAY
  553XX MAY
  16325 MAY
  36325 MAY
  55525 MAY
               9                    SEPARATION CARD
3
*
*
      PROGRAM H601
C   USER  MUST  REQUIRE  TAPE  5
C        CARDS  OF  DATAS   129
      CALL PRAM
      STOP
      END
*EXECUTE
01 MAY   2
01 MAY   2
02 MAY
02 MAY
03 MAY
03 MAY
04 MAY
04 MAY
05 MAY
05 MAY
06 MAY
06 MAY
07 MAY   1
07 MAY   1
08 MAY   2
08 MAY   2
09 MAY
09 MAY
10 MAY
10 MAY
11 MAY
11 MAY
12 MAY
12 MAY
13 MAY
13 MAY
14 MAY   1
14 MAY   1
15 MAY   2
15 MAY   2
16 MAY
16 MAY
17 MAY
17 MAY
18 MAY
18 MAY
19 MAY   2
19 MAY   2
20 MAY
20 MAY
21 MAY   1
21 MAY   1
22 MAY   2
22 MAY   2
23 MAY
23 MAY
24 MAY
24 MAY
25 MAY
25 MAY
26 MAY
26 MAY
27 MAY
27 MAY
28 MAY   1
28 MAY   1
29 MAY   2
29 MAY   2
30 MAY   2
30 MAY   2
31 MAY
31 MAY
01 JUN
01 JUN
02 JUN
02 JUN
03 JUN
03 JUN
04 JUN   1
04 JUN   1
05 JUN   2
05 JUN   2
06 JUN
06 JUN
07 JUN
07 JUN
08 JUN
08 JUN
09 JUN
09 JUN
10 JUN
10 JUN
11 JUN   1
11 JUN   1
12 JUN   2
12 JUN   2
13 JUN
13 JUN
14 JUN
14 JUN
15 JUN
15 JUN
2  10   2
F. K. HUEBNER, , LIBRARY GROUP, LWTA ROOM 322
EJECTION SS62 PROJECT
 2                   2 RESERVE FORCE  CAT. 2
 7                   5 RESERVE FORCE  CAT. 7
 9                   3 RESERVE FORCE  CAT. 9
19                   5 RESERVE FORCE  CAT. 19
 
 4   10 MAY      8   1 RESOURCE AVAIL.  CAT. 4
19   10 MAY      4   1 RESOURCE AVAIL.  CAT. 19
 7   11 MAY      9   1 RESOURCE AVAIL.  CAT. 7
 2   13 MAY     10   1 RESOURCE AVAIL.  CAT. 2
15   14 MAY     11   1 RESOURCE AVAIL.  CAT. 15
13   15 MAY     10   1 RESOURCE AVAIL.  CAT. 13
 9   17 MAY      7   1 RESOURCE AVAIL.  CAT. 9
17   18 MAY      8   1 RESOURCE AVAIL.  CAT. 17
 
  119  163  400 4 1 0 0 0 0 0 0 0 0MODIFY COIL PROTECTIONS             2
  127  137  20017 1 0 0 0 0 0 0 0 0CONNECT SEPTUM MAGNET               2
  137  140 100017 1 0 0 0 0 0 0 0 0RF TEST SEPTUM MAGNET               3
  139  163  40015 1 0 0 0 0 0 0 0 0INSTALL TV AND SUPPORTS             2
  140  163  200 7 2 0 0 0 0 0 0 0 0FINAL VACUUM TEST SECTOR 6          4
  145  147  40013 4 0 0 0 0 0 0 0 0REMOUNT 012.2 - 012.3 - ME2 - ME3   2
  147  163  600 7 2 0 0 0 0 0 0 0 0INSTALL VACUUM - EJECTED BEAM       3
  160  163  200 2 1 0 0 0 0 0 0 0 0MOUNT EJECTED BEAM 62               2
  350  363  400 9 2 0 0 0 0 0 0 0 0MAGNETIC MEASUREMENTS               2
  359  363  600 7 2 0 0 0 0 0 0 0 0VAC. TESTS SECTOR                   2
  529  531  200 0 0 0 0 0 0 0 0 0 0MAKE DOWNSTREAM BOX                 1
  531  549  600 7 1 0 0 0 0 0 0 0 0VAC. TEST DOWNSTREAM BOX            1
  543  547  300 7 2 0 0 0 0 0 0 0 0VAC. TEST IN RING                   1
  547  549  60019 2 0 0 0 0 0 0 0 0HT RING TEST                        1
  549  551  600 0 0 0 0 0 0 0 0 0 0CLEAN + MOUNT DOWNSTREAM BOX + CONN 2
  551  553  200 2 2 0 0 0 0 0 0 0 0INSTALL FINAL ASSEMPLY              3
  553  555  600 7 1 0 0 0 0 0 0 0 0VACUUM TEST FINAL ASSEMPLY          3
9999910000  000 0 0 0 0 0 0 0 0 0 0LAST DATA CARD                      4
   -1
1
*
*
      PROGRAM H602
C        CARDS  OF  DATAS  114
      CALL BART
      STOP
      END
*EXECUTE
01 MAY   21
01 MAY   2
02 MAY
02 MAY
03 MAY
03 MAY
04 MAY
04 MAY
05 MAY
05 MAY
06 MAY
06 MAY
07 MAY   1
07 MAY   1
08 MAY   2
08 MAY   2
09 MAY
09 MAY
10 MAY
10 MAY
11 MAY
11 MAY
12 MAY
12 MAY
13 MAY
13 MAY
14 MAY   1
14 MAY   1
15 MAY   2
15 MAY   2
16 MAY
16 MAY
17 MAY
17 MAY
18 MAY
18 MAY
19 MAY   2
19 MAY   2
20 MAY
20 MAY
21 MAY   1
21 MAY   1
22 MAY   2
22 MAY   2
23 MAY
23 MAY
24 MAY
24 MAY
25 MAY
25 MAY
26 MAY
26 MAY
27 MAY
27 MAY
28 MAY   1
28 MAY   1
29 MAY   2
29 MAY   2
30 MAY   2
30 MAY   2
31 MAY
31 MAY
01 JUN
01 JUN
02 JUN
02 JUN
03 JUN
03 JUN
04 JUN   1
04 JUN   1
05 JUN   2
05 JUN   2
06 JUN
06 JUN
07 JUN
07 JUN
08 JUN
08 JUN
09 JUN
09 JUN
10 JUN
10 JUN
11 JUN   1
11 JUN   1
12 JUN   2
12 JUN   2
13 JUN
13 JUN
14 JUN
14 JUN
15 JUN
15 JUN
13 MAY
F. K. HUEBNER,  LIBRARY GROUP            L W T A , ROOM 322
EJECTION SS62 PROJECT
  119  163   25   32   49   50    4  MODIFY COIL PROTECTIONS
  127  137   25   26   31   32    2  CONNECT SEPTUM MAGNET
  137  140   31   46   47   48   10  RF TEST SEPTUM MAGNET
  139  163   25   32   49   50    4  INSTALL TV AND SUPPORTS
  140  163   47   48   49   50    2  FINAL VACUUM TEST SECTOR 6
  145  147   25   32   33   40    4  REMOUNT 012.2 - 012.3 - ME2 - ME3
  147  163   33   40   49   50    6  INSTALL VACUUM - EJECTED BEAM
  160  163   25   26   49   50    2  MOUNT EJECTED BEAM 62
  350  363   25   32   49   50    4  MAGNETIC MEASUREMENTS
  359  363   25   34   49   50    6  VAC. TESTS SECTOR
  529  531   15   17   18   20    2  MAKE DOWNSTREAM BOX
  531  549   18   23   25   26    6  VAC. TEST DOWNSTREAM BOX
  543  547   15   18   19   20    3  VAC. TEST IN RING
  547  549   19   24   25   26    6  HT RING TEST
  549  551   25   34   35   36    6  CLEAN + MOUNT DOWNSTREAM BOX + CONN
  551  553   35   36   39   40    2  INSTALL FINAL ASSEMPLY
  553  555   39   48   49   50    6  VACUUM TEST FINAL ASSEMPLY
9999910000   49   49   49   49    0  LAST DATA CARD
 
*
*
      PROGRAM I302
      COMMON/TEST/A(10),B,C(20),D(10)
      DIMENSION KEY(4),LOCVAR(4),LENVAR(4)
      DATA NKEY/4/
      DATA KEY(1)/4HKEYA/
      DATA KEY(2)/4HKEYB/
      DATA KEY(3)/4HKEYC/
      DATA KEY(4)/4HKEYD/
      DATA LENVAR/10,1,20,10/
      DATA C/20*0/
      DATA B/0/
      DATA A/10*0/
      DATA D/10*0/
      LOCVAR(1)=LOCF(A(1))
      LOCVAR(2)=LOCF(B)
      LOCVAR(3)=LOCF(C(1))
      LOCVAR(4)=LOCF(D(1))
      CALL FFREAD(NKEY,KEY,LOCVAR,LENVAR)
      PRINT 100,A,B
 100  FORMAT(//2X,4HA = ,10I4/6X,4HB = A6)
      PRINT 110,C,D
 110  FORMAT(/2X,4HC = ,2(10E10.1/)/6X,4HD = ,10L3)
      END
*EXECUTE
LIST
KEYS
KEYA 10*3
C    LOGICAL CNCTANT
KEYD TRUE FALS 5=2*FALS 3=2*TRUE
KEYB $TEST$
KEYC 0.1E1 0.2E1 KOLIA,0.3E1
     0.4E1 20=2.2E2
KEYA 5=0
END
STOP
*
*         M255     àáMEHàãà HA I310       07/1985
      PROGRAM I310
      DIMENSION K(18),R(18)
      EQUIVALENCE(K(1),R(1))
      CALL UREAD(K,1,18)
      PRINT2,(R(I),I=1,3),(R(I),I=7,9)
       PRINT1,(K(I),I=4,6),(K(I),I=13,15)
      PRINT3,(K(I),I=10,12),(K(I),I=16,18)
    1 FORMAT(3X,8I6)
    2 FORMAT(1H1,3X,8E11.3)
    3 FORMAT(3X,10A6)
      STOP
      END
*CALL PTIME
*EXECUTE
1.,200.,3E3,-1,-2,-3, -.4E5, 5  .E3, . 6 E0 8,ABCDEFGH,IJKLMNOP,
), 4 ,  5  ,  6   1, A 1 2 B, 3 4 C D ,  END  END,
*
*
          PROGRAM I312 A
C         COCTABàã  MàããEP B.B.   1986 É.
          REAL MB,MS,CS
          INTEGER II(10)
          DATA II/1,45,-2,.4,98888888888, .2,0,-75,12,-12/
      DATA MB,MS/7757777777777776B,7777777777777777B/
     *,CS/0010000000000000B/
          COMMON/B/B(200)
          COMMON/AA/A(21)
          CALL PRINTL(II,II(10),16,6,2,'II-II(10)=' )
          CALL PRINTL(II,II( 6),6,15,2,'II-II( 6)=' )
          B=1.1E18$DO2 I=2,40
    2 B(I)=B(I-1)*(-.1213)
          B(9)=.2
          B(7)=2.E-3
          B(8)=1.E-7
          B(5)=.1$B(6)=1.E-8
          PRINT88,(B(I),I=1,40)
   88 FORMAT(10E12.5)
          CALL PRINTF(B,B(25),21,6)
          CALL PRINTF(B,B(40),12,5)
          CALLPRINTF(B,B(5),16,6)
          CALL PRINTF(B,B(35),15,8)
          CALL PRINTL(B,B(20),18,7,1,6H+TEST  )
       CALL PRINTF(B(2),B(1),12,4)
          CALL PRINTF(B(2),B(0),15,4)
          CALLPRINTL(B,B(40),16,6,4,24H+TECT èEóATàTECT èEóATà  )
          CALL PRINTL(B,B(40),18,7,2,12H+TECT èEóATà )
          B(11)=0.
          CALL PRINTO( B, B,7,17)
          CALL PRINTF(B,B(37),20,4)
      CALL PRNT(11,B,B(3),B(5),B(7),B(9),B(2),B(4),B,B(10),B
     *,B(6),11,4,24H+TECT èEóATà,TECTèEóATà )
          CALL PRNT(16,B(2),B(15),B(25),B(26),4,4,
     *24H+B(2),B(15),B(25),B(26)=  )
      CALL PRNT(21,B,B(3),B(5),B(7),B(9),B(2),B(4),B,B(10),B
     *,B,B,B,B,B,B,B,B,B,19,2,10H+B(1-10)=  )
          CALL PRINTF(B,B(16),16,6)
      CALL PRNT(21,B,B(3),B(5),B(7),B(9),B(2),B(4),B,B(10),B
     *,B,B,B,B,B,B,B,B,B,B,B(8),21,0,0)
          CALL PRINTL(B,B(26),16,9,11
     *,'+TEKCT                       PPPP                           RR')
          PRINT 45,MB,MS,CS
   45 FORMAT(3E20.13)
          CALL PRINTE(MB,MB,2,15)
          CALL PRINTF(MB,MB, 2,20)
          CALL PRINTE(CS,CS,2,20)
          CALL PRINTF(CS,CS,2,15)
          CS=1./MB
          PRINT 45,CS
          CALL PRINTO(CS,CS,7,17)
          CALL PRINTE(CS,CS,2,13)
          CALL PRINTF(CS,CS,2,15)
          CALL PR NT(15,B,B(5),B(6),B(7),4,10
     *,'+TEKCT                       PPPP                           RR')
          MB=1./CS
          CALL PRINTE(MB,MB,2,13)
          CALL PRINTF(MB,MB,2,15
     *)
          PRINT 45,MB
          CALL PRINTO(MB,MB,7,17)
      STOP
      END
*CALL PTIME
*EXECUTE
*
*   M257  àáMEHàãà HA I312   07/1985
      PROGRAM I312 B
C   èPàMEP B OèàCAHàà
      DIMENSION A(6)
      COMMON /AA/C(21)
      DATA A/0.2787,-98765.,-2.8641E-5,0.27E-19,2E18,-1.9E-18/
      PRINT 1,A
  1   FORMAT(6E20.7)
      CALL PRINTF(A(1),A(6),3,4)
      CALL PRINTL(A(1),A(6),3,4,1,6H TECT )
      CALL PRNT(6,A(2),A(4),A(6),3,2,10H+TECT PRNT)
      STOP
      END
*CALL PTIME
*EXECUTE
*
*
      PROGRAM I901
C
C     TEST SUBROUTINE UNPAK
C
      DIMENSION STORE(15),DICT(3),NTYPE(30),VALUE(30)
      DATA(DICT=1HX,4HMASS,6HENERGY)
      DATA(STORE=87HPS SIMULATION/EMPIRICAL DATA/ X = 3.296, 4.0 (.02) 4
     *.1, ENERGY = 4.2+10, MASS = .13-16 )
      PRINT 10
      CALL UNPAK(STORE,15,DICT,3,NTYPE,VALUE,NI)
      DO 3 I=1,NI
      GO TO (1,1,1,1,1,8,2,1) , NTYPE(I)
  1   PRINT 4,I,NTYPE(I),VALUE(I)
      GO TO 3
    2 PRINT 5,I,NTYPE(I),VALUE(I)
      GO TO 3
    8 PRINT 6,I,NTYPE(I),VALUE(I)
    4 FORMAT(40X,2I10,9X,A6)
    5 FORMAT(40X,2I10,5X,E10.3)
    6 FORMAT(40X,2I10,5X,E10)
   10 FORMAT(//50X,9HTEST I901///49X,1HI,7X,5HNTYPE,5X,5HVALUE//)
    3 CONTINUE
      STOP
      END
*EXECUTE
*
*
      PROGRAM J300
      DIMENSION A(5,5)
      DATA(A=3.0,4.0,2.0,1.5,9.2,7.0,6.5,1.0,1.5,3.0,2.8,9.0,5.0,6.0,
     110.0,8.0,7.5,1.0,3.2,4.3,9.0,3.4,1.1,0.95,2.0)
         IDIM1=5
      IDIM2=5
         IROW=5
         ICOL=5
         NCOL=3
         CALL MXPRNT(A,IDIM1,IDIM2,IROW,ICOL,NCOL,5HF10.1)
      END
*EXECUTE
*
*
      PROGRAM J500
      DIMENSION TITLE(36)
      DIMENSION LA(50)
      DIMENSION LB(50)
      REAL LA,LB
      DATA(TITLE=19HASYMMETRY PARAMETER)
      DATA(LA=3.5,5.4,2.2,3.4,3.2,3.0,2.2,4.0,2.2,3.2,2.0,2.0,2.0,
     12.0,1.7,1.3,1.9,0.9,0.5,1.8,1.6,1.3,0.5,8.0,9.1,4.3,2.6,2.9,
     22.7,2.6,2.9,1.9,2.3,4.5,2.4,2.4,2.5,2.5,3.6,1.8,2.0,1.9,2.0,
     37.0,1.5,1.5,1.5,2.0,16.0,3.2)
      DATA(LB=1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     11.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     21.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     31.0,1.0,1.0,1.0,1.0,1.0,1.0)
      HLIMIT=15.0
      SLIMIT=1.0
      RESOL=1.0
      IDIME=50
      CALL WHISTO(LA,HLIMIT,SLIMIT,RESOL,IDIME,LB,TITLE)
      END
*EXECUTE
*
*
      PROGRAM J501
C
C---  TEST PLOTTING SYSTEM
C
      DIMENSION X(100),Y(100),P(111),KKK(14)
      DIMENSION PP(90)
      DIMENSION A(6)
      DATA(PP   =120HOPTION 8 IS CHOSEN HERE   TO  DEMON-STRATEHOW TOWRI
     *TE A MES- SAGE  DOWN  THE   PAGE AT THE LEFT OF THE PLOT USING THE
     * P )
      DATA(PP(26)=6HARRAY.)
      DATA(A=1.,.5,.25,.125,.0625,.03125)
      DATA(BLANK=6H      )
      EQUIVALENCE (PP,P(17))
      DO 4 I=12,16
   4  P(I)=BLANK
      NFIG = 0
      CALL HOLITH(P(2),1H8)
      P(3) = 5.
      P(4) = 5.
      P(5) = 6.
      NPTS = 26
    2 CONTINUE
      READ 40,KODE
      NFIG = NFIG+1
      DO 1 I = 1,NPTS
      Y(I) = .06*(I-1)
    1 X(I) = COS(Y(I))
      CALL SORTXY(X,Y,NPTS)
      P(1) = FLOAT(NPTS)
      PRINT 10,NFIG,KODE
      CALL PLOTXY(X,Y,KODE,P)
      PRINT 20
      READ 30,ISW
      IF(ISW)  2,3,2
   3  STOP
   10 FORMAT(9H1FIGURE  ,I2,60X,17HPLOTXY --- KODE = ,I3)
   20 FORMAT( 42X    ,23HDATA USED FOR THIS PLOT,5X,
     *26HX = COSY AND Y = 0(.06)1.5)
   30 FORMAT(I1)
   40 FORMAT(I5)
   50 FORMAT(9H1FIGURE  ,I2,60X,17HPLOTMY --- KODE = ,I3)
      END
*EXECUTE
    4
1
    8
1
   72
0
*
*
      PROGRAM J502A
      DIMENSION STEP(3),X(31),Y(30)
      DATA(STEP=.01,.1,5.)
      EXTERNAL ALOG10
      A=5.
      XN=-1.5
      J=1
      X(1)=.01
      DO 1 I=1,30
      Y(I)=A*X(I)**XN
      X(I+1)=X(I)+STEP(J)
      IF(I.EQ.9)J=2
      IF(I.EQ.19)J=3
    1 CONTINUE
      DO 2 I=1,30
      X(I)=ALOG10(X(I))
    2 Y(I)=ALOG10(Y(I))
      CALL SCALES(X,30,7.,XMIN,DX,1)
      CALL SCALES(Y,30,5.,YMIN,DY,1)
      DO 3 I=1,30
      X(I)=X(I)+2.
    3 Y(I)=Y(I)+2.
      CALL PLTIN(9.)
      CALL LINEAR(X,Y,30,1)
      CALL AXILOG(2.,2.,12HY=A*X**(-XN),12,5.,90.,YMIN,DY,ALOG10)
      CALL AXILOG(2.,2.,1HX,1,7.,0.,XMIN,DX,ALOG10)
      CALL ENDPZD
      STOP
      END
*EXECUTE
BERCEANU    LNF,LVTA      1200
*
*
      PROGRAM J502B
      DIMENSION X(51),Y(51)
      DLTX=0.01
      DO 10 I=1,3
      DLTX=DLTX+0.01
      X(1)=DLTX
      DO 20 J=1,50
      Y(J)=(X(J)+6.)*(5.-X(J)**I)*(X(J)+3.)/(X(J)+2.)
   20 X(J+1)=X(J)+DLTX
      CALL SCALES(X,50,7.0,XMIN,DX,1)
      CALL SCALES(Y,50,10.,YMIN,DY,1)
      CALL PLTIN(8.)
      ENCODE(6,30,TEXT) I
      CALL SYMBL4(.5,.5,.28,TEXT,0.,5)
      CALL LINEAR(X,Y,50,1)
      CALL AXIS(0.,0.,6HHORIZ.,-6,7.0,0.,XMIN,DX)
   10 CALL AXIS(0.,0.,8HVERTICAL,8,10.,90.,YMIN,DY)
      CALL ENDPZD
      STOP
  30  FORMAT(3HI =,I2)
      END
*EXECUTE
SOMEBODY    FKH-322        600
*
*
      PROGRAM J502C
      DIMENSION SYMBOL(64)
      DATA((SYMBOL(I),I= 1,32)=
     1    00B, 1HA, 1HB, 1HC, 1HD, 1HE, 1HF, 1HG,
     1    1HH, 1HI, 1HJ, 1HK, 1HL, 1HM, 1HN, 1HO,
     1    1HP, 1HQ, 1HR, 1HS, 1HT, 1HU, 1HV, 1HW,
     1    1HX, 1HY, 1HZ, 33B, 34B, 35B, 36B, 37B)
      DATA((SYMBOL(I),I=33,64)=
     1    1H , 41B, 42B, 43B, 1H$, 45B, 46B, 1H',
     1    1H(, 1H), 1H*, 1H+, 1H,, 1H-, 1H., 1H/,
     1    1H0, 1H1, 1H2, 1H3, 1H4, 1H5, 1H6, 1H7,
     1    1H8, 1H9, 72B, 73B, 74B, 1H=, 1H , 1H )
      DATA(ZERO=0B)
      DATA(OBIT=7760000000000000B)
      DIMENSION TEXT(6)
      DATA(TEXT=32H PLOT. CHAR. CODES FOR BESM-6   )
      DIMENSION TEXT1(6)
      DATA((TEXT1(I),I=1,4)=24H (PROGRAM J 502)        )
      CALL PLTIN(10.)
      DO 4 I=1,8
      L=I-1
      Y=1.0+L*0.82
      ENCODE(6,10,ZAHL) L
    4 CALL SYMBL4(0.,Y,0.21,ZAHL,90.,2)
      CALL PLOT(0.2,Y+0.48,3)
      CALL PLOT(0.2,0.,2)
      DO 1 I=1,8
      L=I-1
      X=FLOAT(I)
      ENCODE(6,11,ZAHL) L
    1 CALL SYMBL4(X,0.,0.21,ZAHL,90.,2)
      CALL PLOT(X+0.3,0.6,3)
      CALL PLOT(-0.3,0.6,2)
      DO 2 I=1,64
      X=FLOAT(1+(I-1)/8)
      Y=1.0+0.82*MOD(I-1,8)
      IF(SAME(AND(SYMBOL(I),OBIT),ZERO))   6,5,6
    5 SYMBOL(I)=SHIFTB(SYMBOL(I),-40)
      XX=X-0.25
      YY=Y+0.25
      CALL SYMBL4(XX,YY,0.28,1H*,90.,1)
    6 CALL SYMBL4(X,Y,0.56,SYMBOL(I),90.,1)
    2 CONTINUE
      CALL PLOT(X+0.3,Y+0.48,3)
      CALL PLOT(X+0.3,0.,2)
      CALL SYMBL4(9.,0.,0.28,TEXT,90.,30)
      CALL DATEZB(TEXT1(4))
      CALL SYMBL4(9.5,0.,0.28,TEXT1,90., 30)
      CALL ENDPZD
      STOP
   10 FORMAT(1H0,I1)
   11 FORMAT(I1,1H0)
      END
*EXECUTE
SOMEBODY    LWTA           600
*
*
      PROGRAM J503
      DIMENSION LA(57)
      DATA LA/5,10,3,4,8,9,11,15,10,4,20,22,21,19,18,19,8,6,7,15,14,12,
     1 11,10,8,7,4,3,13,25,28,20,21,12,22,19,18,17,27,25,19,14,11,9,10,
     2 12,14,17,20,21,17,3,4,5,2,0,0/
      CALL STAP(LA,1,57,0)
      CALL STAP(LA,1,57,1)
      STOP
      END
*EXECUTE
*
*
        PROGRAM J504
      CALL HIST(6,0.,0.,0.,0,0)
         DO 10 I=1,500
      Y=RNDM(-1)
      CALL HIST(1,1.,Y,0.,1,0)
      X=RNDM(-1)**2
      CALL HIST(1,1.,X,0.,2,0)
      X=ASIN(RNDM(-1))*57.3
      CALL HIST(1,1.,X,0.,5,0)
      CALL HIST(2,1.,Y,X,4,5)
   10 CONTINUE
      CALL HIST(3,0.,0.,0.,0,0)
      END
*EXECUTE
  0123456789ABCDEFGHIJKLMNOPQRSTUVWXY0.-XX
 ----Y(HI00)-X(HI00)-PLOT(60X60),A=10-19,B=20-29,Z.GE.260----
1100 3    THIS   IS   HISTOGRAM   OF   RANDOM   NUMBER   DISTRIBUTION
          FROM   0   TO   1.0                  1.0        .0       .01
       0.0       .01
1 60 5    THIS   IS   TWO-DIMENSION   HISTOGRAM     WITH   PROJECTS
          ON   AXIS                            4.0       0.0       .02
       0.0        2.        0.        0.        0.        0.        0.
     0
        .0        .0        .0        .0        .0        .0        .0
        .0        .0        .0        .0        .0        .0        .0
*
*
      PROGRAM J506
      DIMENSION R(500),S(500),T(500),U(500),V(500),W(500),TITLE(36)
      DATA (TITLE=20H ***TESTIDEOGRAM*** )
      DATA(S=100(1.0),100(2.0),100(0.5),100(10.0),100(4.0))
      DATA(T=500(0.0)),(R=500(1.0)),(U=200(1.0),100(2.0),200(0.1))
      DO 2 I=1,500
      CALL NORMCO(V(I),W(I),10,10,5,5,0)
  2   CONTINUE
      CALL WIDEO(V,30,-30,1,500,R,TITLE,T)
      CALL WIDEO(V,30,-30,3,500,R,TITLE,T)
      CALL WIDEO(V,30,-30,1,500,R,TITLE,U)
      CALL WIDEO(V,30,-30,1,500,S,TITLE,T)
      CALL WIDEO(V,30,-30,1,500,S,TITLE,U)
      STOP
      END
*EXECUTE
*
*
      PROGRAM J508
      DIMENSION Q(50), TITLE(20)
      DATA (TITLE=9HHISTOGRAM,5H TEST)
      DATA (Q=0.1,1.0,5.2,2.3,10.5,15.7,3.5,2.2,1.0,30.5,
     1-1.5,6.7,2.0,8.5,9.1,-2.0,-0.5,0.0,2.2,3.5,
     22.5,4.7,4.1,2.0,6.5,4.5,3.7,4.2,7.3,11.5,
     315.0,12.5,13.7,14.8,15.0,19.0,22.0,-2.5,-0.2,-0.25,
     425.0,28.5,35.0,27.0,32.0,10.5,-1.0,-0.9,30.0,17.7)
      NQ=50
      QMIN=-2.5
      RESOL=2.0
      NBIN=25
      CALL PLOTH(Q,NQ,QMIN,RESOL,NBIN,TITLE)
      QMIN=0.0
      RESOL=5.0
      NBIN=10
      CALL PLOTH(Q,NQ,QMIN,RESOL,NBIN,TITLE)
      QMIN=1.0
      RESOL=5.0
      NBIN=10
      CALL PLOTH(Q,NQ,QMIN,RESOL,NBIN,TITLE)
      QMIN=-0.5
      RESOL=10.0
      NBIN=5
      CALL PLOTH(Q,NQ,QMIN,RESOL,NBIN,TITLE)
      QMIN=5.5
      RESOL=2.0
      NBIN=25
      CALL PLOTH(Q,NQ,QMIN,RESOL,NBIN,TITLE)
      QMIN=-35.0
      RESOL=2.0
      NBIN=25
      CALL PLOTH(Q,NQ,QMIN,RESOL,NBIN,TITLE)
      END
*EXECUTE
*
*
      PROGRAM J509 A
      DIMENSION ARRAY(10,10)
      DO 10 I=1,10
      READ 11,(ARRAY(I,J),J=1,10)
 11   FORMAT(10F8.3)
 10   PRINT 12,(ARRAY(I,J),J=1,10)
 12   FORMAT(1H ,10F8.3)
      CALL CONPRT(ARRAY,10,10,10,10,10,0.,10.)
      CALL EXIT
      END
*EXECUTE
      0.      0.      0.      0.      0.      0.      0.     0.      0.
      0.      3.      3.      3.      3.      3.      3.     3.      3.
      0.      3.      5.      5.      5.      5.      5.     5.      3.
      0.      3.      5.      8.      8.      8.      8.     5.      3.
      0.      3.      5.      8.     10.     10.      8.     5.      3.
      0.      3.      5.      8.     10.     10.      8.     5.      3.
      0.      3.      5.      8.      8.      8.      8.     5.      3.
      0.      3.      5.      5.      5.      5.      5.     5.      3.
      0.      3.      3.      3.      3.      3.      3.     3.      3.
      0.      0.      0.      0.      0.      0.      0.     0.      0.
*
*
      PROGRAM J509 B
      DIMENSION F(115,20)
      PI=4.*ATAN(1.)
      DO 5 K=1,2
      IF(K.EQ.2) GO TO 10
      FMIN=-0.22
      FMAX=0.32
      NTC=10
      GO TO 15
 10   FMIN=0.
      FMAX=0.
      NTC=8
 15   M=113
      N=17
      PRINT 1008
 1008 FORMAT (1H1)
      DX=1.
      DY=1.
      DO 1 I=1,M
      X=FLOAT(I-1)*DX
      DO 1 J=1,N
      Y=FLOAT(J-1)*DY
 1    F(I,J)=-0.3+(0.5+0.3*SIN(PI*X/56.))*(0.5+0.3*SIN(PI*Y/8.))
      DO 2 I=1,M
 2    PRINT 1009,(F(I,J),J=1,N)
 1009 FORMAT(/(10F9.3))
      CALL CONPRT(F,115,20,M,N,NTC,FMIN,FMAX)
 5    CONTINUE
      STOP
      END
*EXECUTE
*
*
      PROGRAM J509
      DIMENSION F(30,30)
      IDIM1=30
      IDIM2=30
      FMIN=0.0
      FMAX=0.0
      DX=1.0
      DY=1.0
      X=0.0
      R=10.0
      DO 5 I=1,21
      Y=0.0
      DO 4 J=1,21
      RADXY=SQRT((10.-X)**2+(10.-Y)**2)
      IF(R-RADXY) 2,2,1
  2   F(I,J)=0.0
      GO TO 4
    1 F(I,J)=SQRT(R**2-RADXY**2)
  4   Y=Y+DY
  5   X=X+DX
      PRINT 13
      DO 6 I=1,21
      PRINT 10,(F(I,J),J=1,21)
  6   CONTINUE
  7   READ 11,M,N,NC
      CALL CONPRT(F,IDIM1,IDIM2,M,N,NC,FMIN,FMAX)
      PRINT 12,M,N,NC
      READ 11,ISW,IDUM,IDUM
      IF (ISW)  7,8,7
  8   STOP
  10  FORMAT(21F6.2)
  11  FORMAT(3I5)
  12  FORMAT(ÑM = Ñ,I3/ÑN = Ñ,I3/ÑNC= Ñ,I3)
  13  FORMAT(1H1)
      END
*EXECUTE
   21   21   10
    1
   21   11   10
    0
*
*
      PROGRAM J511A
      EXTERNAL F
      A=0.0
      B=10.0
      C=0.5
      CALL MAP(F,A,B,C)
      STOP
      END
      FUNCTION F(X)
      F=EXP(X)
      RETURN
      END
*EXECUTE
*
*
      PROGRAM J511B
      EXTERNAL F
      A=0.0
      B=10.0
      C=0.5
      CALL MAP(F,A,B,C)
      STOP
      END
      FUNCTION F(X)
      F=(X-1.0)/(0.2*X*X-X+0.5)
      RETURN
      END
*EXECUTE
*
*
      PROGRAM J520
      DIMENSION X(20),Y(20),Z(20),U(20),W(20)
      DO 1 J=1,20
      X(J)=.1*J*J
      Y(J)=SQRT (X(J)) +1.
      U(J)=SQRT (X(J)) +2.
      Z(J)=SQRT (X(J)) +3.
      W(J)=SQRT (X(J)) +4.-.1*X(J)
   01 CONTINUE
      PRINT 10,X,Y,U,Z,W
      X(3)=0 .3
      CALL GRAPH1(20,X,Y)
      X(3) =0.5
      CALL GRAPH4(20,X,Y,Z,U,W)
      PRINT 5
      X(3)=0.9
      CALL GRAPH1(20,X,Y)
      CALL GRAPH2(20,X,Y,Z)
      CALL GRAPH3(20,X,Y,Z,U)
      CALL GRAPH4 (20,X,Y,Z,U,W)
  5   FORMAT(1H1)
  10  FORMAT(1H1//50X,9HTEST J520///1X,2HX=,20F5.1//1X,2HY=,20F5.1//1X,
     *       2HU=,20F5.1//1X,2HZ=,20F5.1//1X,2HW=,20F5.1//)
        STOP
      END
*EXECUTE
*
*
      PROGRAM J530
      DIMENSION AL(6),AH(6),NA(6)
      DATA AL/1.83,-4.12,-2.88,1.22,3.6,1.21/,
     1     AH/1.978,-12.105,1.87,1.17,2.37,1.21/,
     2     NA/ 28 , 24 , 1 , 0 , -1 , 8 /
      PRINT 1
      DO 2 I=1,6
      CALL BINSIZ(AL(I),AH(I),NA(I),BL,BH,NB,BWID)
  2    PRINT  3 ,AL(I),AH(I),NA(I),BL,BH,NB,BWID
  1   FORMAT(//50X,9HTEST J530//6X,2HAL,8X,2HAH,10X,2HNA,10X,2HBL,
     1       10X,2HBH,9X,2HNB,10X,4HBWID)
   3  FORMAT (2F10.4,I10,5X,2F10.4,I10,F18.9/)
      STOP
      END
*EXECUTE
*
*
      PROGRAM J531 A
C-----ùTA èPOÉPAMMA COÑEPÜàT èPàMEP OÅPAôEHàü K COPLOT Ñãü èOCTPOEHàü
C-----ÉPAîàKOB èOCãE HAKOèãEHàü B èAMüTà TAÅãàñ áHAóEHàâ îYHKñàâ
      DIMENSION ALPHA(100),BETA(100),GAMMA(100),DELTA(100)
      DIMENSION YMIN(4),YMAX(4)
      DIMENSION TITLES(8)
      DATA(YMIN  = 11., 0.09E-9, 0.2E7, -0.605E-3),
     *    (YMAX  =135., 1.27E-9, 2.1E7,  0.7E-3),
     *    (TITLES=12H*****ALPHA
     *           ,12H+++++BETA
     *           ,12H-----GAMMA
     *           ,12H<<<<<DELTA  ),
     *    (SCALE =7400 0000 0000 0000 B)
C-----èOãYóEHàE TAÅãàñ áHAóEHàâ îYHKñàâ: ALPHA,BETA,GAMMA,DELTA èPà
C-----àáMEHEHàà APÉYMEHTA T OT 0.1 ÑO 1.94 C òAÉOM 0.02
      TFIRST=0.1
      STEP=0.02
      T=TFIRST
      K=1
1     AR=3.1415826525*T
      ALPHA(K)=    80.*SIN(AR)+70.
      BETA(K) = 0.4E-9*SIN(AR)+0.7E-9
      GAMMA(K)= 0.24E7*SIN(AR)+1.4E7
      DELTA(K)=0.12E-3*SIN(AR)*SIN(10.*AR)
      T=T+STEP
      K=K+1
      IF(T-1.94)1,1,2
C-----èOCTPOEHàE ÉPAîàKOB îYHKñàâ: ALPHA,BETA,GAMMA,DELTA
2     CALL COPLOT(ALPHA,BETA,GAMMA,DELTA,4,TITLES,YMIN,YMAX,SCALE,TFIRST
     *,STEP,K-1)
      END
*EXECUTE
*
*
      PROGRAM J531 B
C-----ùTA èPOÉPAMMA COÑEPÜàT èPàMEP àCèOãúáOBAHàü COPLOT Ñãü èOCTPOEHàü
C-----ÉPAîàKOB ÅEá èPEÑBAPàTEãúHOÉO HAKOèãEHàü B èAMüTà TAÅãàñ áHAóEHàâ
C-----îYHKñàâ
      DIMENSION YMIN(4),YMAX(4)
      DIMENSION TITLES(8)
      DATA(YMIN  =0.1, 0.1E-9, 0.2E7, -0.6E-3),
     *    (YMAX  =1.3, 1.3E-9, 2.0E7,  0.6E-3),
     *    (TITLES=12H*****ALPHA
     *           ,12H+++++BETA
     *           ,12H-----GAMMA
     *           ,12H<<<<<DELTA  ),
     *    (SCALE =7400 0000 0000 0000 B)
      T=0.1
      STEP=0.02
1     AR=3.14158226525*T
      ALPHA=    0.8*SIN(AR)+0.7
      BETA =0.4 E-9*SIN(AR)+0.7E-9
      GAMMA=0.24E7 *SIN(AR)+1.4E7
      DELTA=0.12E-3*SIN(AR)*SIN(AR*10.)
C-----áÑECú èPà OÑHOM OÅPAôEHàà K COPLOT ÉPAîàK èPOÑBàÉAETCü HA OÑHO
C-----ÑEãEHàE OCà X
      CALL COPLOT(ALPHA,BETA,GAMMA,DELTA,4,TITLES,YMIN,YMAX,SCALE,T,
     *STEP,1)
      SCALE=0.
C-----SCALE èPàCBAàBAETCü HYãEBOE áHAóEHàE,óTOÅõ áAèPETàTú èOBTOPHYû
C-----BõÑAóY OCEâ OPÑàHAT
      T=T+STEP
      IF(T-1.96)1,1,2
2     CONTINUE
      END
*EXECUTE
*
*
        PROGRAM J540
         DIMENSION T(300)
        H=6.28/50.
        DO 12 I1=1,51
        I=I1*2-1
        T(I)=H*I1-H
        T(I+1)=SIN(T(I))
        T(102+I)=T(I)
 12     T(103+I)=5*T(I+1)
        PRINT 1,T
 1     FORMAT(2X,10F10.4)
        CALL XYPLOT(T,300,0.,6.28,-5.,5.,50,6H******)
       END
*EXECUTE
*
*
      PROGRAM J550
      DIMENSION N(50,40)
      DO 1 I=1,50
      DO 1 J=1,40
      N(I,J)=I+J-2
  1   CONTINUE
      CALL AREAPT(50,40,N,8)
      END
*EXECUTE
*
*
      PROGRAM J551
      DIMENSION A(50,2)
      DX=2.*3.14159265359/50.
      DO 1 I=1,50
      X=DX*(I-1)
      A(I,1)=SIN(X)
    1 A(I,2)=COS(X)
      CALL FIGURE(2,50,A,0,5HI-XSC)
      CALL FIGABS(2,50,A,0,5H  XSC)
      CALL FIGREL(2,50,A,0,5H  OSC)
      STOP
      END
*EXECUTE
*
*
      PROGRAM K100
C        *FILE:SCRATCH,F1,W,40
C        *FILE:SCRATCH,F2,W,40
C        *FILE:SCRATCH,F3,W,40
C        *FILE:SCRATCH,F8,W,40
           COMMON/IFLAG/IFL(4)
           DIMENSION MA(40),MAMA(2400)
        COMMON/NTEST/NTEST,NT1
           DO 77 I77=1,2
        PRINT 27$CALL PTIME
  27    FORMAT(10X,'***** HAóAãO CEPàà TECTOB *****')
        NT1=I77
           NTEST=1
           DO 200 I=1,4
 200        IFL(I)=0
         IFL(4)=1
      N8=3
      N2048=3
C... èEPBõE 4 TECTA èPOBEPüûT ÅãOK èOÑKAóKà OÅM.ãàCTOB
           REWIND 16
           DO 100 I=1,2400
 100       MAMA(I)=5H*****
           MA(2)=1$WRITE(16)MAMA
       DO 1 I=1,N8$LUN=I$CALL TESTRD
  1   CALL FSWOP(LUN)
       DO 2 J=1,N2048$DO 3 I=1,N8$LUN=I$CALL TESTRD$N=I*J
  3        CALL BWRITE(LUN,N,1)
       DO 4 I=1,N8$LUN=I$CALL TESTRD
  4        CALL BACK(LUN,1)
       DO 5 I=1,N8$LUN=I$CALL TESTRD$M=0$CALL BREAD(LUN,M,1,LEN)
           IF(LEN.NE.1.OR.M.NE.J*LUN)GOTO 9999
  5        CONTINUE
  2        CONTINUE
            CALL ETEST
       DO 6 I=1,N8$LUN=I$N=300*I$CALL TESTRD
  6         CALL BACK(LUN,N)
       DO 7 I=1,N8$LUN=I$CALL TESTRD$CALL EWR(LUN,LEN)
        IF(LEN.NE.N2048)GOTO 9999
  7     CONTINUE
          CALL ETEST
           MA(2)=0
            IFL(4)=0
        DO 8 I=1,N8$LUN=I$CALL TESTRD$CALL INPOS(LUN,LUN-1)
        CALL BREAD(LUN,MA,2,LEN)
           PRINT 81,LUN,MA(1),LEN
  81      FORMAT(3I6)
  8     CONTINUE
           CALL ETEST
       DO 9 I=1,N8$LUN=I$CALL TESTRD
  9     CALL SWOP(LUN)
        CALL ETEST
        CALL SOWWT(5)
        CALL LOADGO(4HTRWS)
        CALL SOWWT(5)
        CALL LOADGO(4HTIBE)
C... TECT MEãKàX ÑEüHàâ
        IFL(4)=3$CALL SOWWT(4)
        IFL(1)=0
 601       CONTINUE
        IFL(4)=1
      CALL FSWOP(16)
        DO 600 I=1,48
        CALL BWRITE(16,MAMA,2400)
        IF(IFL(1).EQ.0)GOTO 600
        IFL(1)=0$CALL SOWWT(1)$GOTO 601
 600    CONTINUE
        PRINT 600,IFL(2),IFL(3)
 600    FORMAT(2I10)
        CALL ETEST
           CALL SWOP(16)
        CALL SOWWT(5)
  77       CALL SOWWT(0)
        DO 199 I=1,2400
 199    MAMA(I)=1H+
           CALL SOWWT(6)
           CALL SOWWT(66)
        IFL(4)=4
        CALL SOWWT(4)
        CALL SOWWT(1)
        CALL SOWWT(2)
        LUN=31
      CALL FSWOP(LUN)
        CALL BWRITE(LUN,MAMA,2400)
      DO 300 I=1,20
        CALL BWRITE(LUN,J,1)
        CALL INPOS (LUN,0)
             IFL(4)=1
        CALL BWRITE(LUN,J,1)
        CALL BACK  (LUN,  2)
        CALL BREAD (LUN,MA,40,LEN)
        DO 301 II=2,40$IF(MA(II).NE.0B)GOTO 9999
 301    CONTINUE
        MA(11)=12345
        IF(LEN.NE.2400.OR.MA(1).NE.J)GOTO 9999
        CALL EWR(LUN,LEN)
        IF(LEN.NE.J+1)X=1./0.
 300    CONTINUE
        CALL INPOS (LUN,1)
        DO 302 I=1,2$IFL(4)=I-1
 302    CALL BWRITE(LUN,MAMA,10)
        CALL BACK  (LUN,     1 )
        CALL BREAD (LUN,MA,100,LEN)
        IF(LEN.NE.1.OR.MA(1).NE.1H+)GOTO 9999
        CALL ETEST
            STOP
 9999       CALL ERTEST
       END
            SUBROUTINE TRWS
             COMMON/IFLAG/IFL(4)/NTEST/NT,NT1
          DIMENSION AR(2000)
          DIMENSION A(7000)
          DO 10 I=1,2000
 10       AR(I)=1.*I
           IF(NT1.EQ.2)CALL SOWWT(0)
          DO 1 L=1,2$LUN=L
         IFL(1)=0
         IFL(4)=1
      CALL FSWOP(LUN)
          DO 2 I=1,512
  2       CALL BWRITE(LUN,AR(I),1)
         CALL SWOP  (LUN)
         CALL INPOS (LUN,0)
         DO 3 I=1,512$A(I)=0.$A(1)=0.$J=I
         CALL BREAD (LUN,A,J,LEN)
        IF(LEN.NE.1.OR.A(1).NE.1.*I)PRINT 1,LUN,LEN,I,A(1)
  3      CONTINUE
  1     FORMAT('  ***',3I5,F10.1)
          CALL BWRITE (LUN,AR,1020)
          CALL BWRITE (LUN,AR,2000)
          CALL BACK   (LUN,2)
          II=1
  8       CALL BREAD  (LUN,A,2000,LEN)
          DO 4 I=1,1020
        IF(A(I).NE.1.*I)GOTO 55
  4      CONTINUE
        GOTO 5
  55    PRINT 1,LUN,LEN,I,A(I)
  5        CALL BACK(LUN,3)
             IF(II.EQ.2)GOTO 1
           IFL(4)=1
          CALL BWRITE(LUN,3.62,1)
           IFL(4)=1
          CALL BWRITE(LUN,AR,2000)
          CALL SWOP  (LUN)
          CALL INPOS (LUN,511)
          CALL BREAD (LUN,A,5000,LEN)
            II=2$GOTO 8
  1         CONTINUE
            CALL ETEST
             RETURN
            ENTRY TIBE
            IF(NT1.EQ.2)CALL SOWWT(0)
             CALL INPOS(1,10000)
             IFL(1)=0
               CALL INPOS(1,20)
              CALL EWR(1,LEN)
               RETURN
            END
        SUBROUTINE TESTRD
           COMMON/NTEST/NTEST
         COMMON/IFLAG/IFL(4)
            DIMENSION MM(2400)
            DATA(NTEST=1),(NBEG=0),(NB1=0)
        IF(IFL(1).EQ.0)GOTO 1$PRINT 1$STOP
  1      FORMAT(2X,'----  Aâ-üâ-üâ, TOB. CAèOÜHàKOB  ')
  1      CONTINUE
       IF(NBEG.EQ.0)CALL PAHOM1$NBEG=NBEG+1$READ 101,NSTAR
       IF(NSTAR.NE.3H***)GOTO13$NB1=NB1+1$REWIND 16
          IF(NB1.NE.3)GOTO2$NB1=0$CALL PAHOM2
  2       READ(16),MM$DO 3 I=1,2400$IF(MM(I).NE.5H*****)GOTO13
  3      CONTINUE
          RETURN
           ENTRY ETEST
        CALL PTIME
           PRINT 2,NTEST$NTEST=NTEST+1$CALL SOWWT(3)$RETURN
           ENTRY ERTEST
           PRINT 3,NTEST$STOP
  13       PRINT 4,NTEST$STOP
 101    FORMAT(A3)
  2         FORMAT(2X,'TECT ',I2,'    èPOòEã')
  3         FORMAT(2X,'TECT ',I2,' HE èPOòEã')
  4         FORMAT(2X,'TECT ',I2,' - èOPóA OÅM.ãàCTA')
          END
*ASSEM
  PAHOM1:,NAME,
  10,VTM,
  ,UJ,PAHOM
  PAHOM2:,ENTRY,
  10,VTM,1
  RFMRTFLG:,LC,1
 KCOUNT*:,LC,4
  PAHOM:13,MTJ,11
            ,XTA,RFMRTFLG
           ,ATX,KCOUNT*+2
  ,ITA,10
  ,CALL,SAVE*
  11,UJ,
  ,END,
*EXECUTE
***
***
***
***
***
***
***
*
*
      PROGRAM K101
      DIMENSION FIRST(7300),SECOND(1000),A(10),B(10),C(10)
      CALL FCNEW(16)
      DO 1 I=1,100
    1 FIRST(I)=1.111
      PRINT 10
   10 FORMAT(5X,6HFWRITE/)
      CALL FWRITE(16,'FILE 1',FIRST,100)
      DO 2 J=1,100
    2 FIRST(J)=2.222
      CALL FWRITE(16,'FILE 2',FIRST,100)
      DO 3 K=1,100
    3 FIRST(K)=3.333
      CALL FWRITE(16,'FILE 3',FIRST,100)
      DO 4 N=1,100
    4 FIRST(N)=4.444
      CALL FWRITE(16,'FILE 4',FIRST,100)
      CALL FREADI(16,'FILE 4',A,91,100)
      PRINT 6,A
    6 FORMAT('A=',10F10.3)
      DO 5 M=1,1000
    5 SECOND(M)=5.555
      CALL FWRITE(16,'FILE 5',SECOND,1000)
      CALL FCATAL(16)
      CALL FREADI(16,'FILE 5',B,991,1000)
      PRINT 7,B
    7 FORMAT('B=',10F10.3)
      END
*EXECUTE
*
*
*FORTRAN
      PROGRAM K450
      DIMENSION IA(256)
      COMMON/ICHECK/IC(6)
      CALL DNS556
      IC(1)=1
      CALL BUF608
      DO 1 I=1,10
      IA=I
      CALL WRTBUF(IA,256)
1     CONTINUE
      CALL MKFILE
      DO 2 I=1,10
      IA=10+I
      CALL WRTBUF(IA,256)
2     CONTINUE
      CALL MKFILE
       CALL SCHMFB
      CALL SCHMFB
      CALL SCHMKF
      DO 3 I=1,10
      CALL RDBUF(IA,N)
      PRINT 5,N,IA(1)
3     CONTINUE
5     FORMAT (2I10)
       CALL REWIND
      DO 4 I=1,10
      CALL RDBUF(IA,N)
      PRINT 5,N,IA(1)
4     CONTINUE
      END
*EXECUTE
*
*
      PROGRAM K451A
C1       YèPABãüûôàE KAPTõ
C2       *ASSIGN FTAPE 01 W(1)
C3       *CHECK(1) 000 NO CHECK
C4       *MAIN BUFMT9
      COMMON/BUFMT9/IB(1024)
      COMMON /ECTAPE/IW(20)
      DIMENSION MA(10),MER(10),MC(10)
      DATA(READ=1),(WRITE=2),(FORWD=3),(REVERS=4)
     *(FTOMF=5),(RTOMF=6),(REWIND=7),(RWDALL=8),(WRTMF=9)
      NT=1
      IPLOTN=1
      CALL DNSMT9(IPLOTN)
      NN=0
      CALL MT9EC(NT,REWIND)
      PRINT 6,IW(4)
    6 FORMAT(10X,'èPOBEPKA Mî 'O2)
    8 NN=NN+1
      CALL MT9EC(NT,REWIND)
      CALL MT9EC(NT,WRTMF)
      NZON=8
      K=1
      DO 1 I=1,NZON
      DO 2 J=1,256
      IB(J)=K
    2 K=K+1
      CALL MT9EC(NT,WRITE)
    1 CONTINUE
      CALL MT9EC(NT,WRTMF)
      CALL MT9EC(NT,REVERS)
      CALL MT9EC(NT,RTOMF)
      CALL MT9EC(NT,FORWD)
      K=1
      DO 3 NZ=1,NZON
      DO 4 J=1,256
    4 IB(J)=1
      CALL MT9EC(NT,READ)
      NSL=IW(10)
      N=0
      DO 5 J=1,256
      IF(IB(J).EQ.K) GO TO 5
      IF(N.LT.6) N=N+1
      MA(N)=J $ MER(N)=IB(J) $ MC(N)=K
    5 K=K+1
      IF(N.EQ.0) GO TO 3
      PRINT 1,NN,NZ,NSL
    1 FORMAT(//'ñàKã',I5,'.áOHA',I4,'.èPOóàTAãà',I4,
     *'  CãOB.',20X,'OòàÅKà:')
      PRINT 2,(MA(J),J=1,N)
    2 FORMAT(' AÑPECA:',10(8X,O3,7X))
      PRINT 3,(MC(J),J=1,N)
    3 FORMAT(' èàCAãà:',10O18)
      PRINT 4,(MER(J),J=1,N)
    4 FORMAT(' óàTAEM:',10O18)
    3 CONTINUE
      CALL MT9EC(NT,FTOMF)
      NP=IFPULT(1)
      IF(NP.EQ.0) GO TO 8
      CALL MT9EC(NT,WRTMF)
      CALL MT9EC(NT,REWIND)
      PRINT 5,NN
    5 FORMAT(' KOHEñ ñàKãA',I6)
      END
*MAIN BUFMT9
*EXECUTE
*
*
      PROGRAM K451B
C1       YèPABãüûôàE KAPTõ
C2       *ASSIGN FTAPE 01 W(1)
C3       *CHECK(1) 000 NO CHECK
C4       *MAIN BUFMT9
      DIMENSION A(500),B(500),C(512),D(512)
      COMMON/ICHECK/ICH(6)
      COMMON/BUFMT9/BUFMT9(1024)
      INTEGER A,B,C,D
      DATA(READ=1),(WRITE=2),(FORWD=3),(REVERS=4)
      DATA(FTOMF=5),(RTOMF=6),(REWIND=7),(REWALL=8)
      DATA(WRTMF=9)
      DO1I=1,500
      A(I)=1+I
    1 CONTINUE
      NT=1
       IPLOTN=1
      CALL DNSMT9(IPLOTN)
      CALL MT9NT(NT)
      CALL MT9EC(NT,REWIND)
       DO 12 J=1,256
      C(J)=A(J)
      C(J+256)=A(J)
   12  CONTINUE
      N=0
      DO2 I=1,4
      CALL WR9MT(A,500)
      PRINT 30,I
  30  FORMAT('èàòEM WR9MT',I4)
    2 CONTINUE
      DO 100 I=1,4
  100 CALL BSP9MT
      PRINT 100
 100  FORMAT(' 4 BACKSPASE')
    3 CONTINUE
      DO4 I=1,4
      CALL RD9MT(B,500)
      N=N+1
      PRINT 50,I
  50  FORMAT('óàTAEM RD9MT',I4)
      A(10)=20
      L=0
      DO5 J=1,500
      IF(A(J).EQ.B(J)) GOTO 5
      L=L+1
  80  PRINT81
  81  FORMAT('OSHIBKA')
      PRINT8,N,J,A(J),B(J)
   8  FORMAT(4I10)
      IF(L.EQ.10)GOTO4
    5 CONTINUE
    4 CONTINUE
      CALL MT9EC(NT,WRTMF)
      PRINT 22
  22  FORMAT('MK FILE')
       DO 11 I=1,2
      CALL MT9WR(C,256)
      PRINT 60
  60  FORMAT('WRTBUF')
   11  CONTINUE
      CALL MT9EC(NT,REVERS)
      CALL MT9EC(NT,REVERS)
      CALL MT9EC(NT,RTOMF)
      CALL MT9EC(NT,FTOMF)
      PRINT 23
  23  FORMAT(' REV,REV,RTOMF,FTOMF')
      N=N+1
      CALL MT9RD(D,NSL)
      N=N+1
      CALL MT9RD(B,NSL)
      N=N+1
      DO13I=1,256
      D(256+I)=B(I)
   13  CONTINUE
      C(10)=20
      C(500)=500
      L=0
      DO 16 K=1,512
      IF(C(K).EQ.D(K)) GOTO16
      L=L+1
  70  PRINT71
  71  FORMAT('OSHIBKA')
      PRINT21,N,K,C(K),D(K)
  21  FORMAT(4I10)
      IF(L.EQ.10)GOTO33
   16  CONTINUE
  33  CONTINUE
       PRINT 20
  20  FORMAT('FINISH')
      CALL MT9EC(NT,REWIND)
      PRINT 40
  40  FORMAT('RIWIND')
       END
*MAIN BUFMT9
*EXECUTE
*
*
      PROGRAM K452
      CALL REWRMT
      STOP
      END
*EXECUTE
RGM 7
DNS 7 800
 RCRD 30
END
*
*
*FORTRAN
      PROGRAM M101
      DIMENSION A(200),IA(200),INDEX(5),IN(5),IND(5),ID(5)
      DATA IN/10,50,100,150,200/,IND/10,50,100,150,200/
     1  ,  ID/10,50,100,150,200/
      DO 5 I=1,200
      IA(I)=I
  5   A(I)=RNDM(-1.)
      PRINT 2,(A(J),J=1,20)
      CALL SORTZV(A,INDEX,5,1,0,0)
      PRINT 1,INDEX
      PRINT 2,(A(K),K=10,200,10)
      CALL SORTZV(A,IN,5,1,2,3)
      PRINT 1,IN
      CALL SORTZV(IA,INDEX,5,1,0,0)
      PRINT 1,INDEX
      CALL SORTZV(IA,IND,5,1,2,2)
      PRINT 1,IND
      IA(10)=4HJOHN
      IA(50)=3HTOM
      IA(100)=3HJIM
      IA(150)=4HANNA
      IA(200)=4H1TOM
      CALL SORTZV(IA,INDEX,5,0,1,0)
      PRINT 1,INDEX
      CALL SORTZV(IA,ID,5,0,0,1)
      PRINT 1,ID
  1   FORMAT(2X,6HINDEX=,5I5)
  2   FORMAT(2(1X,10F10.5/))
      STOP
      END
*EXECUTE
*
*
      PROGRAM M103
      DIMENSION A(1000),IA(1000)
      DO 1 I=1,1000
      A(I)=RNDM(-1.)
   1  IA(I)=RNDM( 1.)
      PRINT 2,(A(K),K=100,1000,100)
      CALL FLPSOR(A,1000)
      PRINT 2,(A(K),K=100,1000,100)
      PRINT 3,(IA(K),K=100,1000,100)
      CALL INTSOR(IA,1000)
      PRINT 3,(IA(K),K=100,1000,100)
   2  FORMAT(1X,2HA=,10F10.5)
   3  FORMAT(2(1X,3HIA=,5I15/))
      STOP
      END
*EXECUTE
*
*
          PROGRAM M106
      DIMENSION MX(9),WCP(2,3)
      COMMON/SLATE/I,I1,I2,I3,IC,IR,ICMX,N1,N2,N3,N4,N5,NKI,NKI1,NKI2,
     1   NCSORT,NRM1,INK,JNK,NHELP,NKIP1,XQX(19)
          DATA(MX=3.0,4.0,1.0,1.5,8.0,2.0,4.0,5.0,2.3)
      PRINT 10,MX
      CALL SORTX(MX,3,3,1,WCP)
      PRINT 1,MX
      CALL SORTX(MX,3,3,-2,WCP)
      PRINT 1,MX
  1   FORMAT(20X,27HOUTPUT OF THE RESULTS    MX//3(30X,3F10.1/)//)
  10  FORMAT(//50X,9HTEST M106///45X,2HMX//3(30X,3F10.1/)//)
      STOP
         END
*EXECUTE
*
*
      PROGRAM M108
      DIMENSION X(3,3),MX(3,3)
      DATA MX/3,4,1,1,8,2,4,5,2/,
     *     X/3.2,4.,1.7,1.5,8.1,2.,4.6,5.,2.3/
      PRINT 10,MX
      PRINT 11,X
      CALL SORTMQ(MX,3,3,1)
      PRINT 1,MX
      CALL SORTMQ( X,3,3,-2)
      PRINT 11,X
 1    FORMAT(45X,6HRESULT //45X,2HMX//3(30X,3I3/)//)
 10   FORMAT(//50X,9HTEST M108///45X,2HMX//3(30X,3I5/)//)
 11   FORMAT(45X,1HX//3(30X,3F10.3/)//)
      STOP
      END
*CALL PTIME
*EXECUTE
*
*
      PROGRAM M203
      COMPLEX A,CFLOAT,B
      PRINT 10
      I=3
      A=(2.01304201,3.120031)*CFLOAT(I)
      B=(2.01304201,3.120031)*CMPLX(FLOAT(I),0.0)
      PRINT 1,A,B
  1   FORMAT(20X,2HA=,1H(,E18.11,1H,,E18.11,1H)/
     *       20X,2HB=,1H(,E18.11,1H,,E18.11,1H))
  10  FORMAT(//50X,9HTEST M203///)
      STOP
       END
*EXECUTE
*
*
      PROGRAM M204
      COMPLEX Z
      INTEGER CFIX
      PRINT 10
      Z=(-2.2,5.3)
      DO 2 I=1,10
      Z=Z+(0.5,-0.5)
      M=IFIX(REAL(Z))
      K=CFIX(Z)
  2   PRINT 1,Z,K,M
  1   FORMAT(20X,1H(,F6.2,1H,,F6.2,1H),10X,I3,10X,I3)
  10  FORMAT(//50X,9HTEST M204///27X,1HZ,20X,1HK,12X,1HM/)
      STOP
      END
*EXECUTE
*
*
      PROGRAM M205
      COMPLEX CSIGN,B,C,E
      B=(2.1,-3.2)
      C=(-2.2,3.5)
      PRINT 10
      E=CSIGN(B,C)
      X=SIGN(REAL(B),REAL(C))
      Y=SIGN(AIMAG(B),AIMAG(C))
      PRINT 1,B,C,E,X,Y
  1   FORMAT(10X,3HB=(,F4.1,1H,,F4.1,1H),10X,3HC=(,F4.1,1H,,F4.1,1H),15X
     *  ,3HE=(,F4.1,1H,,F4.1,1H)//60X,2HX=,F4.1,10X,2HY=,F4.1)
  10  FORMAT(//50X,9HTEST M205///)
      STOP
       END
*EXECUTE
*
*
         PROGRAM M210
         DIMENSION A(10),ADB(10)
         DATA A/3.0,10.0,3.5,6.,100.,4.6,5.7,64.,0.5,1./
      PRINT 10,A
      M=1
      CALL DEZBEL(A,ADB,10,M)
      PRINT 1,M,ADB
      M=2
      CALL DEZBEL(A,ADB,10,M)
      PRINT 1,M,ADB
  1   FORMAT(5X,2HM=,I2//5X,4HADB=/2(3X,5F17.11/)///)
  10  FORMAT(//50X,9HTEST M210///10X,2HA=,10F7.2////)
      STOP
      END
*EXECUTE
*
*
      PROGRAM M215
      DIMENSION A(8),B(100),AFACT(8)
      DATA A/1234567800.,1234567.8,1234.5678,1.2345678,0.0012345678,
     *    1234.5678E-9,1234.5678E-12,0.0/
      DO 1 I=1,20
      B(I)=RNDM(-1.)
      B(I+20)=10*RNDM(-1.)
      B(I+40)=-B(I+20)
      B(I+60)=10**2+RNDM(-1.)
      B(I+80)=0.1*RNDM(-1.)
 1    CONTINUE
      PRINT 3,B
      BMAX=AMAXMU(B,1,100,0)
      FACT=PSCALE(N,9,BMAX,4)
      FF=FACT*BMAX
      PRINT 2,N,FACT,FF
      DO 4 I=1,100
 4    B(I)=FACT*B(I)
      PRINT 3,B
      DO 7 I=1,8
 7    AFACT(I)=PSCALE(N,9,A(I),4)
      PRINT 6,AFACT
 2    FORMAT(1X,'N=',I2,5X,'FACT=',E10.1,5X,'FF=',F10.3)
 3    FORMAT(1X,10F10.3/)
 6    FORMAT(1X,'*******'/1X,'AFACT=',8E10.1)
      STOP
      END
*CALL PTIME
*EXECUTE
*
*
      PROGRAM M216
      DOUBLE PRECISION X
      DIMENSION H(30)
      X=4.D0*DATAN(1.D0)
      CALL FDCONV (X,H,24,20)
      PRINT 100,H
  100 FORMAT(1X,'PI=' ,5(4A1,1X))
      END
*EXECUTE
*
*
      PROGRAM M224
      DIMENSION X(10) ,FMT(2),W(12)
      DATA FMT/12H(I4,10F12.0)/
      DO 1 I=1,2
      X(I)=RNDM(-1.)
      X(I+2 )=50*RNDM(-1.)
      X(I+4 )=-X(I+2 )*10**5
      X(I+6 )=500+RNDM(-1.)
 1    X(I+8 )=0.5*RNDM(-1.)
      PRINT 3,X
 3    FORMAT(1H1//1X,10F12.2//)
      DO 2 I=5,10
      CALL UBLOW(FMT,W,12)
      CALL SETFMT(W(7),W(11),I,X,10)
      CALL UBUNCH(W,FMT,12)
      K=I-4
      PRINT FMT,K,X
 2    CONTINUE
      STOP
      END
*CALL PTIME
*EXECUTE
*
*
      PROGRAM M250T
      DIMENSION A(13),B(13),C(13),D(13)
      DIMENSION E(13)
      K=13
      CALL RABM4(A,K)
      PRINT 20
      PRINT 10,A
      PRINT 100
      CALL RABM4(B,K)
      PRINT 30
      PRINT 10,B
      PRINT 100
      CALL RABM4(C,K)
      PRINT 40
      PRINT 10,C
      PRINT 100
      CALL RABM4(D,K)
      PRINT 50
      PRINT 10,D
      PRINT 100
   10 FORMAT(7E10.2/6E10.2)
   20 FORMAT(8HMACCàB A)
   30 FORMAT(8HMACCàB B)
   40 FORMAT(8HMACCàB C)
   50 FORMAT(8HMACCàB D)
  100 FORMAT(1H0)
      STOP
      END
*CALL PTIME
*EXECUTE
*
*
      PROGRAM M251
      COMMON/MINIFL/IFLAG
      DIMENSION MCAR(14,1000),MERR(1000)
      CALL MINCOD(MCAR,MERR,1000)
      IFLAG=1
      CALL MINCOD(MCAR,MERR,1000)
      STOP
      END
*EXECUTE
*
*
      PROGRAM M254
      DATA (A=1775400000000000B), (B=6002377777777777B) ,
     1  (C=2004522000000000B),
     2    (D=5773255777777777B), (E=0000000000000125B),
     3  (F=7777777777777652B)
      DATA (TXT1=6162636465666770B), (TXT2=2365222363646301B)
COMMENT  A=0.125 , B=-0.125 , C=10.5625 , D=-10.5625 , E=85 , F=-85
       TXT1=54.10
      A1=RBESM(A)  $ B1=RBESM(B) $ C1=RBESM(C)
       D1=RBESM(D)
        I=IBESM(E)  $  K=IBESM(F)
       PRINT 10, A,B,C,D,E,F,A1,A1,B1,B1,C1,C1,D1,D1,I,I,K,K
      TXTA=CRBESM(TXT1)  $  TXTB=CRBESM(TXT2)
      PRINT 20,TXT1,TXTA,TXTA
       PRINT 20, TXT2,TXTB,TXTB
 10     FORMAT(10X,'NUMBER CDC IN OCTAL' /15X,3O20,/15X,3O20,/10X,
     *  'IN BESM-6' ,2(/15X,2(O20,F15.6))/ 2(O20,I5))
 20      FORMAT(//3X,O20,10X,A6,O20)
        END
*CALL PTIME
*EXECUTE
*
*
      PROGRAM M401
      DIMENSION N1(5),N2(5)
      DATA(A=7777777777777777B),(N1=1,12,0,46,35),(N2=47,20,46,47,47)
      DO 1 I=1,5
      PRINT 2,N1(I),N2(I)
      Y=BITSZA(A,N1(I),N2(I))
      PRINT 3,Y,A
  1   CONTINUE
  2   FORMAT(//10X,2I3)
  3   FORMAT(10X,O16/10X,O16)
      END
*EXECUTE
*
*
      PROGRAM M402
      DIMENSION R(100)
      CALL HOLN (R,25HTHIS IS R TEST FOR BESM-6,5)
      PRINT 10,(R(I),I=1,5)
 10   FORMAT(3X,5A6)
      END
*EXECUTE
*
*
      PROGRAM M403
      INTEGER P,A
      DIMENSION N(4)
      DATA P/5424643344116622B/,A/6HABCDEF/,N/-4,-2,2,4/
      PRINT 10,P,A
      DO 2 I=1,4
      P=5424643344116622B
      M=5424643344116622B
      A=6HABCDEF
      CALL SHFTZF(P,N(I))
      CALL SHFTZ1(M,N(I))
      CALL SHFTZF(A,N(I))
  2   PRINT 1,N(I),P,M,A
  1   FORMAT(10X,2HN=,I3,5X,O16,5X,O16,7X,A6/)
  10  FORMAT(//50X,9HTEST M403///20X,2HP=,O16,10X,2HA=,A6///20X,
     *       11HSHFTZF(P,N),10X,11HSHFTZ1(P,N),10X,11HSHFTZF(A,N)//)
      STOP
      END
*EXECUTE
*
*
      PROGRAM  M404
      INTEGER FROM, TO
      READ 5,FROM
      DO 50 I=1,5
      READ 5,TO
  5   FORMAT(O16)
      READ 10,N1,N2
  10  FORMAT (I3,I3)
      PRINT 30,FROM
      PRINT 30,TO
  30  FORMAT(2X,O16)
      PRINT 40,N1,N2
  40  FORMAT(I3,I3)
      CALL IMBDZA(FROM,N1,N2,TO)
      PRINT 20,FROM
      PRINT 20,TO
  20  FORMAT(2X,O16)
  50  CONTINUE
      END
*EXECUTE
7777771234444444
7777777777777777
  1 47
7777777777777777
  0 46
7777777777777777
  3 11
7777777777777777
 35 47
7777777777777777
 46 47
*
*
      PROGRAM M405
      DATA(I=0007123445216647B)
      K=1
      DO 1 L=1,48
      J=INVERT(I,K)
      PRINT 10,I,J
      K=K+1
  1   CONTINUE
      K=0
      J=INVERT(I,K)
      PRINT 10,I,J
  10  FORMAT(2X,O16,5X,O16)
      END
*EXECUTE
*
*
      PROGRAM M407
      MEM=0
      CALL STOREB (MEM,3,8,11)
      K=JTHB(MEM,3,11)
      PRINT 10, K,K
      MEM=0
      CALL STORE6(MEM,2,15)
      K=JTH(MEM,2)
      PRINT 10, K,K
      MEM=0
      CALL STORE1(MEM,4,1)
      K=JTH1(MEM,4)
      PRINT 10, K,K
      MEM=0
      CALL STOREN(MEM,3,4,10)
      K=JTHN(MEM,3,10)
      PRINT 10,K,K
      K=JTH10(MEM,3)
      PRINT 10, K,K
 10   FORMAT(O20,3X,I10)
      END
*EXECUTE
*
*
      PROGRAM M408
      DIMENSION INT(10)
      N=5
      INT(1)=10 $ INT(2)=9 $ INT(3)=8 $ INT(4)=7
      INT(5)=6 $ INT(6)=5
      IWORD=IPACK(INT,N)
      PRINT 10, IWORD,IWORD
      DO 5 J=1,5
      K=JTH(IWORD,J)
      PRINT 11, K,K
 5    CONTINUE
 10   FORMAT(1H1,3X,9HPACK WORD,5X,O20,I10)
 11   FORMAT(/3X,14HUNPACK NUMBERS,5X,O20,3X,I10)
      END
*EXECUTE
*
*
      PROGRAM M409
      DIMENSION AM(3),A1(13),AC(3)
      DATA(AM=6HABCDEF,6H/+901X,1HY)
      INTEGER AM,A1,AC
      PRINT 8,AM
      CALL UBLOW(AM,A1,13)
      PRINT 10,A1
  10  FORMAT(2X,A1)
  8   FORMAT(3A6)
      CALL UBUNCH(A1,AC,13)
      PRINT 10,A1
      PRINT 8,AC
      END
*EXECUTE
*
*
      PROGRAM M410
      DIMENSION CH(10),BH(10),AB(10),AH(10),BC(10)
      COMMON /SLATE/ ND,NE,RESERV(38)
      DATA(CH=1HA,1H7,1H ,1HB,1H2,1H ,1HC,1H ,1HD,1H5)
      DATA(AB=1H1,1HA,1H2,1HB,1H3,1HC,1H4,1HD,1H5,1HE)
      DATA(AH=1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9,1H0)
      DATA(BC=1H ,1H ,1H ,1H ,1H ,1H ,1H ,1H ,1H ,1H )
      PRINT 1
   1  FORMAT(1H1)
      PRINT 2,CH,AB,AH
   2  FORMAT(5X,3HCH=,10A1,/5X,3HAB=,10A1,/5X,3HAH=,10A1,//)
      JL=2 $ JR=8
C  TEST ULEFT
      DO 20 I=1,10
   20 BH(I)=CH(I)
      CALL ULEFT(BH,JL,JR)
      PRINT 33
   33 FORMAT(2X,10HTEST ULEFT,/)
      PRINT3,ND,NE,BH
      DO 200 I=1,10
  200 BH(I)=AB(I)
      CALL ULEFT(BH,JL,JR)
      PRINT3,ND,NE,BH
   3  FORMAT (5X,I2,5X,I2,/5X,4HCH1=,10A1,//)
C  TEST URIGHT
      DO 30 I=1,10
   30 BH(I)=CH(I)
      CALL URIGHT(BH,JL,JR)
      PRINT 44
   44 FORMAT (2X,11HTEST URIGHT,/)
      PRINT 4,ND,NE,BH
      DO 300 I=1,10
  300 BH(I)=AB(I)
      CALL URIGHT(BH,JL,JR)
      PRINT 4,ND,NE,BH
    4 FORMAT (5X,I2,5X,I2,/5X,4HCH2=,10A1,//)
C  TEST USET
      INT=12
      DO 40 I=1,10
   40 BH(I)=CH(I)
      CALL USET(INT,BH,JL,JR)
      PRINT 55
   55 FORMAT (2X,9HTEST USET,/)
      PRINT5,INT,ND,BH(NE),BH
      DO 400 I=1,10
  400 BH(I)=AB(I)
      CALL USET(INT,BH,JL,JR)
      PRINT5,INT,ND,BH(NE),BH
      INT=123456789
      DO 50 I=1,10
   50 BH(I)=CH(I)
      CALL USET(INT,BH,JL,JR)
      PRINT5,INT,ND,BH(NE),BH
    5 FORMAT(5X,I10,5X,I2,5X,A1,/5X,4HCH3=,10A1,//)
C  TEST J=IUNEXT
      JL=3
      J=IUNEXT(CH,JL)
      PRINT 66
   66 FORMAT (2X,13HTEST J=IUNEXT,/)
      PRINT6,J,CH
      JL=2
      J=IUNEXT(AB,JL)
      PRINT6,J,AB
   6  FORMAT(5X,I3,/5X,4HCH4=,10A1,//)
C  TEST INTEXT=IULOOK
      N=3
      INTEXT=IULOOK(N,CH,JL,JR)
      PRINT 77
   77 FORMAT(2X,18HTEST INTEXT=IULOOK,/)
      PRINT7,N,INTEXT,CH
      INTEXT=IULOOK(N,AB,JL,JR)
      PRINT7,N,INTEXT,AB
      N=6
      INTEXT=IULOOK(N,CH,JL,JR)
      PRINT7,N,INTEXT,CH
      INTEXT=IULOOK(N,AB,JL,JR)
      PRINT7,N,INTEXT,AB
    7 FORMAT(5X,I7,A6,/5X,4HCH5=,10A1,//)
C  TEST I=IUFORW
      I=IUFORW(CH,JL,JR)
      PRINT 88
   88 FORMAT(2X,13HTEST I=IUFORW,/)
      PRINT8,I,ND,NE,CH(NE),CH
      I=IUFORW(AB,JL,JR)
      PRINT8,I,ND,NE,AB(NE),AB
      I=IUFORW(AH,JL,JR)
      PRINT8,I,ND,NE,AH(NE),AH
      I=IUFORW(BC,JL,JR)
      PRINT8,I,ND,NE,BC(NE),BC
    8 FORMAT(5X,I8,I2,I2,2X,A1,/5X,4HCH6=,10A1,//)
C  TEST I=IUBACK
      I=IUBACK(CH,JL,JR)
      PRINT 99
   99 FORMAT(2X,13HTEST I=IUBACK,/)
      PRINT9,I,ND,NE,CH(NE),CH
      I=IUBACK(AB,JL,JR)
      PRINT9,I,ND,NE,AB(NE),AB
      I=IUBACK(AH,JL,JR)
      PRINT9,I,ND,NE,AH(NE),AH
      I=IUBACK(BC,JL,JR)
      PRINT9,I,ND,NE,BC(NE),BC
    9 FORMAT(5X,I8,I2,I2,2X,A1,/5X,4HCH7=,10A1,//)
      END
*EXECUTE
*
*
       PROGRAM M412
       DIMENSION IA(68),A(68),I(68),H(68)
       DATA(IA=1HA,1HB,1HC,1HD,1HE,1HF,1HG,1HH,1HI,1HJ,1HK,1HL,1HM,
     û1HN,1HO,1HP,1HQ,1HR,1HS,1HT,1HU,1HV,1HW,1HX,1HY,1HZ,1H0,1H1,
     21H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9,1H/,1H+,1H-,1H ,1H.,1H),
     31H$,1H*,1H,,1H(,1H=,1HÑ,1H:,1HÅ,1HÉ,1HÑ,1HÜ,1Há,1Hà,1Hâ,1Hã,1Hè,
     41Hî,1Hñ,1Hó,1H+,1Hô,1Hõ,1Hú,1Hù,1Hû,1Hü)
       DATA(A=101,102,103,104,105,106,107,110,111,112,113,114,115,116,
     *117,120,121,122,123,124,125,126,127,130,131,132,060,061,062,063,
     *064,065,066,067,070,071,057,053,055,040,056,051,044,052,054,050,
     *075,047,072,142,147,144,166,172,151,152,154,160,146,143,176,173,
     *175,171,170,174,140,161)
         DO 10 J=1,68
       K=IA(J)
       I(J)=IFROMA(K)
       N=A(J)
        H(J)=AFROMI(N)
 10     CONTINUE
       PRINT 2,I
 2     FORMAT(1X,10I8)
       PRINT 3,H
 3     FORMAT(1X,10A8)
       END
*EXECUTE
*
*
      PROGRAM M415
      DIMENSION A(5)
      CALL UHOLLR(A,16,28HTHIS IS A TEST FOR HOLLERITH)
      PRINT 1,A
  1   FORMAT((10X,A6)//)
      STOP
      END
*EXECUTE
*
*
      PROGRAM M416
      DIMENSION K(3)
      DATA(K=0,1,1),(B=1000000000000007B)
      N=3
C
      CALL UBNCH1(K,A,N)
      PRINT 1,K,A
  1   FORMAT(30X,11HTEST UBNCH1//10X,2HK=3I3,10X,2HA=O16///)
      CALL UBLOW1(B,K,N)
      PRINT 2,K,B
  2   FORMAT(30X,11HTEST UBLOW1//10X,2HK=3I3,10X,2HB=O16)
      STOP
      END
*EXECUTE
*
*
      PROGRAM M419
      INTEGER Z,ALEFT
      DATA(WORD=2345674321151166B)
      M2=6
      Y=ALEFT(WORD,M2)
      PRINT 10,WORD,Y
      N1=-4
      X=ALEFT(WORD,N1)
      PRINT 10,WORD,X
      Z=LEFT(WORD,M2)
      PRINT 10,WORD,Z
  10  FORMAT(2X,O16,2X,O16)
      END
*EXECUTE
*
*
      PROGRAM M421
      DIMENSION IX(48)
      DATA A,X/6444444444444435B,7777777777777741B/
      PRINT 3
      DO 1 I=1,48
  1   IX(I)=JBIT(A,I)
      PRINT 2,IX
      IX1=JBYT(A,2,4)
      PRINT 4,IX1
      CALL SBYT(A,X,2,4)
      PRINT 5,X
  2   FORMAT(50X,4HJBIT/5X,3HIX=,48I2)
  3   FORMAT(50X,15HTEST  FOR  M421//)
  4   FORMAT(50X,4HJBYT/45X,4HIX1=,O16)
  5   FORMAT(50X,4HSBYT/45X,2HX=,O16)
      STOP
      END
*EXECUTE
*
*
      PROGRAM M422
C        TEST  FOR  M421,M422
      DIMENSION M(200),MPACK(3)
      DATA MPACK/3,16,7/
      PRINT 1000
      N=5
      ID=   5431234567B
      JA=0
      DO 2 L = 1,4
      JA = JA + 1
      CALL UPKBYT (ID,JA,M(1),N,MPACK)
      PRINT 19,JA
      PRINT 20,ID,(M(J),J=1,N)
      IX=0
      CALL PKBYT (M(1),IX,JA,N,MPACK)
      PRINT 21,IX
    2 CONTINUE
   19 FORMAT (20X,19HTEST FOR UPKBYT JA=,I3)
 20   FORMAT(6(2X,O20))
 21   FORMAT(20X,15HRUN SUB.  PKBYT 2X,O20)
 1000 FORMAT (1H1)
      END
*EXECUTE
*
*
      PROGRAM M423
      DIMENSION MPACK(3),IPBYT(7),LOST(7)
      DATA MPACK/3,16,7/
      PRINT 1000
      ID=   5431234567B
      JTH=0
      DO 4 K = 1,7
      JTH=JTH+1
      INC=0
      DO 3 I=1,7,2
      INC = INC + 1
      ID1 =ID
      LOST(I)=INCBYT(INC,ID1,JTH,MPACK)
      IPBYT(I) = ID1
    3 CONTINUE
      PRINT 29,JTH
      PRINT 30,(IPBYT(I),I=1,7)
      PRINT 30,(LOST(I),I=1,7)
    4 CONTINUE
   29 FORMAT (20X,20HTEST FOR INCBYT JTH=,I3)
  30  FORMAT(2X,4O20/)
 1000 FORMAT (1H1)
      END
*EXECUTE
*
*
      PROGRAM M450
      DIMENSION A2(4)
C     SENAS VARIANTAS  SPALIS IR ANKSCIAU.
      DIMENSION  B2(4),C2(4)
      DATA(B1=6HCAPITA),(B=6HMOSKAU)
      DATA(A1=6HACIPAT),(A=6HUAKSOM)
      DATA(A2=24H...KINGNOTYMB IS  WORRNS)
      DATA(B2=24HRNSYMB IS NOT WORKING...)
      DATA(A4=2001B),(A6=6HA15437),
      DATA(A8=6HTAIKAS),(B8=6HKASTAI)
      DATA(A9=6H   421),(B9=6H768765)
      DATA(I22=0B)
      DATA(I21=2001B),(A21=6H001025)
C     B=6H  MOSKAU   B1=6H  CAPITA
C     A=6H  UAKSOM   A1=6H  ACIPAT
      PRINT 5
    5 FORMAT(///,40X,23H1-ST  TEST  SUBP  RSYMB,//)
      DO 1 I=1,6
      J=7-I
      CALL   RSYMB(A,I,C,J)
      K1=(I/2)*2
      IF(I.EQ.K1) K=I-1
      IF(I.NE.K1) K=I+1
      CALL   RSYMB(A1,I,C1,K)
    1 CONTINUE
      IF(B.EQ.C.AND.B1.EQ.C1)  GO TO 10
      PRINT 6,B,B1
    6 FORMAT(2X,12HTRUE TEXT IS,2A6)
      PRINT 7,C,C1
    7 FORMAT(2X,12HREZULT. TEXT,2A6)
   10 CONTINUE
C   INITIAL TEXT   A2(4)=24H...KINGNOTYMB IS  WORRNS
C   REZULT  TEXT   B2(4)=   RNSYMB IS NUT WORKING...
      PRINT 15
   15 FORMAT(///,40X,23H2-ND  TEST  SUBP RNSYMB,//)
      DO 11 I=1,3
      J=I+3
      CALL  RNSYMB(A2,I,C2(4),J,1)
      CALL   RNSYMB(A2(4),J,C2,I,1)
   11 CONTINUE
      CALL   RNSYMB(A2,4,C2(3),6,4)
      CALL   RNSYMB(A2(2),2,C2(2),5,3)
      CALL   RNSYMB(A2(2),5,C2,4,7)
      CALL   RNSYMB(A2(3),6,C2(3),2,4)
      DO  12  I=1,4
      IF(B2(I).EQ.C2(I)) GO TO 12
      GO TO 13
   12 CONTINUE
      GO TO 20
   13 CONTINUE
      PRINT 16,B2
   16 FORMAT(2X,12HTRUE TEXT IS,4A6)
      PRINT 17,C2
   17 FORMAT(2X,12HREZULT. TEXT,4A6)
   20 CONTINUE
C     TEST  FOR  WRINT
      PRINT 25
   25 FORMAT(///,40X,23H3-RD  TEST  SUBP  WRINT,//)
      N11=0
      I4=1
      JAD1=1025
      LAD1=527348
      K=12
      CALL  WRINT(JAD1,C4,J,K,NFL1)
      IF(C4.NE.A4)  N11=N11+1
      CALL  WRINT(LAD1,C5,J,K,NFL2)
      IF(NFL2.EQ.0) N11=N11+1
      J=7
      CALL  WRINT(I4,C6,J,K,NFL3)
      IF(N11.EQ.0)  GO TO 30
      NF1=0
      NF2=-1
      NF3=-2
      PRINT 26,A4,NF1,NF2,NF3
   26 FORMAT(12HTRUE REZULT=,O16,3I12)
      PRINT 27,C4,NFL1,NFL2,NFL3
   27 FORMAT(12HREZULT WAS  ,O16,3I12)
   30 CONTINUE
C     TEST  FOR  RINT
      PRINT  35
   35 FORMAT(///,40X,25H4-TH TEST  FOR  SUBP RINT//)
      I6=15437
      I=2
      N=5
      N1=0
      CALL  RINT(A6,I,N,KADR,NFL1)
      IF(NFL1.NE.0) N1=N1+1
      IF(KADR.NE.I6) N1=N1+1
      I=1
      N=6
      CALL  RINT(A6,I,N,KRDR,NFL2)
      IF(NFL2.EQ.0)  N1=N1+1
      IF(N1.EQ.0) GO TO 40
      NF1=0
      NF2=1
      PRINT 36,I6,NF1,NF2
   36 FORMAT(11HTRUE REZULT,4X,4I12)
      PRINT 37,KADR,NFL1,NFL2
   37 FORMAT(11HREZULT WAS ,4X,4I12)
   40 CONTINUE
      PRINT  55
   55 FORMAT(///,40X,23H5-FTN  TEST  SUBP WRNGR,//)
      NFL7=0
      NF2=-3
      NF3=-1
      N=3
      N1=0
      I=1
      K=4
      LGR=8
      I9=0
      CALL  WRNGR(A8,I,C8,K,N,LGR,NFL1)
      IF(NFL1.NE.0) N1=N1+1
      CALL  WRNGR(A8,K,C8,I,N,LGR,NFL6)
      IF(NFL6.NE.0)   N1=N1+1
      IF(C8.NE.B8)  N1=N1+1
      LGR=7
      CALL  WRNGR(A8,I,C8,K,N,LGR,NFL2)
      IF(NFL2.NE.NF2)  N1=N1+1
      LGR=4
      N=-2
      CALL  WRNGR(A8,I,C8,K,N,LGR,NFL3)
      IF(NFL3.NE.NF3)  N1=N1+1
      N=6
      LGR=-5
      CALL  WRNGR(A8,I,C8,K,N,LGR,NFL4)
      IF(NFL4.NE.NF2) N1=N1+1
      IF(N1.EQ.0) GO TO 60
      PRINT  56,B8,NFL7,NFL7,NF2,NF3,NF2
   56 FORMAT(2X,12HTRUE  REZULT,2X,A6,5I12)
      PRINT  57,C8,NFL1,NFL6,NFL2,NFL3,NFL4
   57 FORMAT(2X,12HREZULT WAS  ,2X,A6,5I12)
   60 CONTINUE
      PRINT 65
   65 FORMAT(///,40X,23H6-TH  TEST  SUBP  RASPI ,//)
      I8=1249
      N=0
      J7=20
      K=20
      CALL  RASPI(I8,J7,K,I6)
      IF(I6.NE.I8)  N=N+1
      IF(N.EQ.0) GO TO  70
      PRINT 66,I8
   66 FORMAT(2X,12HTRUE RESULT ,5I12)
      PRINT 67,I6
   67 FORMAT(2X,12HREZULT WAS   ,5I12)
   70 CONTINUE
C     TEST FOR  SABP  RASPS
      PRINT 75
   75 FORMAT(///,40X,23H7-TH  TEST  SUBP RASPS //)
      I6=421
      LS=6
      N=0
      I1=1
      I7=123768765
      J=40
      K=40
      CALL  RASPS(I6,J,K,C9,I1,LS)
      CALL  RASPS(I7,J,K,D9,I1,LS)
      IF(A9.EQ.C9.AND.B9.EQ.D9)  GO TO 80
      PRINT 76,A9,B9
   76 FORMAT(2X,12HTRUE REZULT ,2A6)
      PRINT 77,C9,D9
   77 FORMAT(2X,12HREZULT WAS  ,2A6)
   80 CONTINUE
      PRINT 85
   85 FORMAT(///,40X,25H8-TH TEST FOR SUBP COPYBI,//)
      J=48
      K=48
      I=48
      I6=1923
      CALL  COPYBI(I6,J,K,I9,I)
      I=24
      K=24
      CALL  COPYBI(I6,J,K,I7,J)
      CALL  COPYBI(I6,I,K,I7,I)
      IF(I6.EQ.I9.AND.I6.EQ.I7)  GO TO 90
      PRINT 86,I6,I6
   86 FORMAT(2X,12HTRUG REZULT ,2I12)
      PRINT 87,I9,I7
   87 FORMAT(2X,12HREZULT  WAS ,2I12)
   90 CONTINUE
C     TEST FOR  SUBP  RCIF
      PRINT 95
   95 FORMAT(///,40X,25H9-TH TEST FOR SUBP  RCIF,//)
      J=12
      K=12
      N1=0
      I=1
      I7=I22
      NF3=-2
      NF4=1
      N=6
      CALL RCIF(A21,1,N,I7,J,K,NFL1,NFL2)
      NF1=0
      NF4=1
      NF2=0
      IF(I7.NE.I21) N1=N1+1
      IF(NFL1.NE.0)  N1=N1+1
      IF(NFL2.NE.0) N1=N1+1
      I=0
      CALL RCIF(A21,I,N,I7,J,K,NFL3,NFL4)
      IF(NFL3.NE.NF3) N1=N1+1
      I=1
      CALL RCIF(A6,I,N,I7,J,K,NFL5,NFL6)
      IF(NFL5.NE.NF4) N1=N1+1
      IF(N1.EQ.0)  GO TO 100
      PRINT  96,A21,I21,NF1,NF1,NF3,NF4
   96 FORMAT(2X,12HTRUE REZULT ,2X,A6,5I12)
      PRINT 97,A21,I7,NFL1,NFL2,NFL3,NFL5
   97 FORMAT(2X,12HREZULT  WAS ,2X,A6,5I12)
  100 CONTINUE
      END
*EXECUTE
*
*
      PROGRAM M501
      DIMENSION IA1(5)
      DATA IA1 /1,2,3,4,5/
C--                TEST FOR IUSAME
      PRINT 30
      J1=1
      J1=1
      J2=5
      MIN=5
      JSAME=1
      KK=1
    3 NSAME=IUSAME (IA1,J1,J2,MIN,JSAME)
      PRINT 40,(IA1(K),K=1,5),NSAME,JSAME
      IF (KK) 8,7,6
    6 IA1(1)=IA1(2)
      IA1(3)=IA1(2)
      KK=KK-1
      GO TO 3
    7 IA1(4)=IA1(2)
      IA1(5)=IA1(2)
      KK=KK-1
      GO TO 3
    8 CONTINUE
  30  FORMAT(1H1//30X'TEST FOR IUSAME'//)
   40 FORMAT (20X,10HVECTOR A=,5I5/20X,6HNSAME=,I2,3X,6HJSAME=,I2)
      END
*EXECUTE
*
*
      PROGRAM M502
      DIMENSION ICODE(2),IOPT(20)
      DATA ICODE /4HWDM.,5HSVNP. /
C--                TEST FOR UOPT
      PRINT 1000
      PRINT 100
      DO 2 I=1,2
      CALL UOPT (ICODE(I),13HSVWMDCHEFVONP,IOPT,13)
      PRINT 20,(IOPT(J),J=1,13)
    2 CONTINUE
   20 FORMAT (20X,13I3)
  100 FORMAT(20X,13HTEST FOR UOPT)
 1000 FORMAT(1H1)
      END
*EXECUTE
*
*
      PROGRAM M506
      DATA A,K/25.,12/
      I=IUMODE(A)
      J=IUMODE(K)
      PRINT 1,A,I,K,J
 1    FORMAT(//50X,9HTECT M506//10X,2HA=,F10.2,5X,10HIUMODE(A)=,I3/
     *10X,2HK=,I3,12X,10HIUMODE(K)=,I3)
      STOP
      END
*EXECUTE
*
*
      PROGRAM N100
      INTEGER XLOC
        Y=1.0
      I=XLOC(Y)
      PRINT 10,I,I
 10   FORMAT(10X,O20,I10)
      END
*EXECUTE
*
*
      PROGRAM N200
      DIMENSION N1(1),N2(2),N3(3),N4(4),N5(5),N6(6),N7(7),N24(24),N31
     1       (31)
      EQUIVALENCE(N31,N1,N2,N3,N4,N5,N6,N7,N24)
        DO 1 K=1,31
   1  N31(K)=6HXA-XA
      WRITE(16)N1
      WRITE(16)N2
      WRITE(16)N3
      WRITE(16)N4
      WRITE(16)N5
      WRITE(16)N6
      WRITE(16)N7
      WRITE(16)N24
      WRITE(16)N31
      REWIND 16
      CALL DUMPZD(16,7,0)
      REWIND 16
      CALL DUMPZD(16,7,1)
      REWIND 16
      CALL DUMPZD(16,0,0)
      REWIND 16
      CALL DUMPZD(16,0,1)
      END
*EXECUTE
*
*
      PROGRAM Q800
C   USER  MUST  REQUIRE  LIBRARY 1  AND  LIBRARY 2
C   USER  MUST  REQUIRE  TAPE 1  AND  TAPE 2
CARDS  OF  ANY  FORTRAN  PROGRAMS,SUBROUTINES OR  FUNCTIONS
C   FOR  EXAMPLE  KIM(F220)
      CALL FLODIA
      STOP
      END
*EXECUTE
      SUBROUTINE TRED2(N,D,E,Z,FAIL)
      DIMENSION D(N),E(N),Z(N,N)
      COMMON/PREC/TOL,EPS,AA,BB
      LOGICAL FAIL
      I=N
    1 L=I-2
      F=Z(I,I-1)
      G=0.
      DO2K=1,L
    2 G=G+Z(I,K)*Z(I,K)
      H=G+F*F
      IF(G-TOL)3,3,4
    3 E(I)=F
      H=0.
      GOTO 9
    4 L=L+1
      P=SQRT(H)
      IF(F.GE.0.)P=-P
      G=P
      E(I)=P
      H=H-F*G
      Z(I,I-1)=F-G
      F=0.
      DO7J=1,L
      Z(J,I)=Z(I,J)/H
      G=0.
      DO5K=1,J
    5 G=G+Z(J,K)*Z(I,K)
      M=J+1
      IF(M.GT.L)GOTO 61
      DO 6 K=M,L
    6 G=G+Z(K,J)*Z(I,K)
   61 E(J)=G/H
    7 F=F+G*Z(J,I)
      HH=F/(H+H)
      DO8 J=1,L
      F=Z(I,J)
      G=E(J)-HH*F
      E(J)=G
      DO8 K=1,J
    8 Z(J,K)=Z(J,K)-F*E(K)-G*Z(I,K)
    9 D(I)=H
      I=I-1
      IF(I-3)10,1,1
   10 E(2)=Z(2,1)
      D(2)=0.
      D(1)=0.
      E(1)=0.
      DO17I=1,N
      L=I-1
      IF(D(I))11,14,11
   11 DO13J=1,L
      G=0.
      DO12K=1,L
   12 G=G+Z(I,K)*Z(K,J)
      DO13K=1,L
   13 Z(K,J)=Z(K,J)-G*Z(K,I)
   13 Z(K,J)=Z(K,J)-G*Z(K,I)
   14 D(I)=Z(I,I)
      Z(I,I)=1.
      IF(L)17,17,15
   15 DO16J=1,L
      Z(I,J)=0.
   16 Z(J,I)=0.
   17 CONTINUE
      DO21I=2,N
   21 E(I-1)=E(I)
      ENTRY TGL2
      E(N)=0.
      B=0.
      F=0.
      DO36L=1,N
      J=0
      H=EPS*(ABS(D(L))+ABS(E(L)))
      IF(B.LT.H)B=H
      DO22M=L,N
      IF(ABS(E(M))-B)23,23,22
   22 CONTINUE
   23 IF(M-L)24,36,24
   24 IF(J-30)25,37,37
   25 J=J+1
      P=(D(L+1)-D(L))/(2.*E(L))
      IF(ABS(P)-1.E+6)261,261,260
  260 Q=2.*P
      GOTO 28
  261 R=SQRT(P*P+1.)
      IF(P)26,27,27
   26 Q=P-R
      GOTO28
   27 Q=P+R
   28 H=D(L)-E(L)/Q
      DO29I=L,N
   29 D(I)=D(I)-H
      F=F+H
      P=D(M)
      C=1.
      S=0.
      I=M-1
   30 Q=E(I)
      G=C*Q
      H=C*P
      IF(ABS(P)-ABS(Q))32,31,31
   31 Q=Q/P
      R=SQRT(Q*Q+1.)
      E(I+1)=S*P*R
      C=1./R
      S=Q*C
      GOTO33
   32 C=P/Q
      R=SQRT(C*C+1.)
      E(I+1)=S*Q*R
      S=1./R
      C=C*S
   33 P=C*D(I)-S*G
      D(I+1)=H+S*(C*G+S*D(I))
      DO34K=1,N
      H=Z(K,I+1)
      Z(K,I+1)=S*Z(K,I)+C*H
   34 Z(K,I)=C*Z(K,I)-S*H
      I=I-1
      IF(I-L)35,30,30
   35 E(L)=S*P
      D(L)=C*P
      IF(ABS(E(L))-B)36,36,24
   36 D(L)=D(L)+F
      IF(.NOT.FAIL)GOTO46
      M=N-1
      DO 45 I=1,M
      K=I
      P=D(I)
      L=I+1
      DO42J=L,N
      IF(D(J)-P)41,42,42
   41 K=J
      P=D(J)
   42 CONTINUE
      IF(K-I)43,45,43
   43 D(K)=D(I)
      D(I)=P
      DO44J=1,N
      P=Z(J,I)
      Z(J,I)=Z(J,K)
   44 Z(J,K)=P
   45 CONTINUE
   46 FAIL=.FALSE.
      RETURN
   37 FAIL=.TRUE.
      RETURN
      END
      EXIT
 
*
*
      PROGRAM Q820
C   USER  MUST  REQUIRE  LIBRARY 1  AND  LIBRARY 2
C   FOR  EXAMPLE  TEST  OF  R.ZALYALOVS
      CALL FLOWER
      STOP
      END
*EXECUTE
CHECK FLOWER
      PROGRAM TEST
CH    TEST FOR THE FLOWER
C     BEGIN
      COMMON A,B,
     1       C,
     2       D(100)
      COMMON /F/L(5)/G/K(5,5)
      DOUBLE PRECISION C,
     1                 D
      REAL K
      LOGICAL H
      DIMENSION A(100,100),B(1,1)
      DATA ((L(I),I=1,5)=1,2,3(55)),
C  INSTRUKTION
C        GO  TO
      GO TO 1
      A(3)=5*A(4) $ GO TO 50
    1 ASSIGN 10 TO IS
      GO TO IS , (5,10,15,20)
    5 NIS=1
      GO TO (10,15,20),NIS
C                IF
   10 IF(L(1)-L(3)) 30,40,50
C       IF     LOGICAL
   20 IF(H) GO TO 50
C         LOOP   DO
   40 DO 41 I=1,100
      DO 41 IK=1,99,2
      DO 42 J=1,10
      DO 43 JK=1,15,5
      DO 44 IJ=10,100,10
      DO 44 IJK=1,33,2
      A(I)=A(IK)*A(J) $ A(IJ)=A(IJK) $ IF(H) A(IK)=A(IK)*0.5
      K(3,4)=5.45 $ IF(A(IJ)) 44,43,44
   44 CONTINUE
      CALL LINK
   43  CONTINUE
      X=0.15
      Y=A(J)/X
      Z=SIN(X)*Y/A(I)
      IF(A(I)-A(I-1)) 41,41,42
   42 CONTINUE
      CALL  BULK
   41 CALL ABC
   15 CALL AIS
   30 PAUSE
      CALL EXITE5
 1000 FORMAT(1H,4(10X,I6,7X,I3,1X))
C        INPUT   AND    OUTPUT
   50 PRINT 1000,A
      PUNCH 1000,A
      WRITE (51,1000) A
      WRITE (1) A(I),(D(IK),IK=1,100)
      READ (50,1000) A
      READ (1) ((A(I,J),I=1,100),J=1,100)
      BACKSPACE 1
      END FILE 1
      REWIND 1
      ENCODE (6,100,L(1)) L(2)
  100 FORMAT (A6)
      DECODE (6,100,L(2)) L(3)
      STOP
      END
*END SEGMENT
*
*
      PROGRAM Q900 A
C  USER  MUST  REQUIRE  TAPE 3  AND  TAPE 4
      CALL TIDY
      STOP
      END
*EXECUTE
*NOCARD
*REFE
*STAT=10.
*IDIN=100.
      PROGRAM RELKIN
CRELKIN    RELATIVISTIC KINEMATICS PROGRAM    POIRIER
C     THIS DECK IS FOR THE CERN 7090 WHICH HAS READ TAPE NO. 1 AND WRITE
C     TAPE NO. 2.  OUTPUT IS IN THE SAME UNITS AS THAT OF THE MASSES
C     READ IN, WITH C = 1.  ADDITIONS TO THE DECK HAVE BEEN..A) DECAY IN
C     FLIGHT.  THIS IS DONE BY READING IN 0 FOR THE MASS OF PARTICLE
C     NO. 2  B) THE FORMAT HAS BEEN CHANGED SO THAT THE TABLES OF PARTIC
C     3 AND 4 ARE NO LONGER ON SEPERATE PAGES.  THE OUTPUT FORMAT HAS
C     BEEN CHANGED TO ACCOMPLISH THIS BY PRINTING OUT ONLY 6 SIGNIFICANT
C     FIGURES, WHICH IS ABOUT THE ACCURACY ONE MIGHT EXPECT.  NUMBERS FO
C     P,T, AND SOLID ANGLE TRANSFORMATION WHICH ARE LARGER THAN 1.E6 ARE
C     PRINTED OUT AS 999999.
C
C
      DIMENSION C(26)
C     FREQUENCY  1700(0,3,1), 7(0,3,1), 6(0,1,5), 1300(3,4,3), 1301(1,10
C    1,1), 1015(1,0,1), 16(1,0,1), 19(1,10,1), 20(0,4,5), 1302(1,0,5),
C    2 92(0,1,10), 131(0,4,5), 1303(0,1,10), 200(0,1,6), 700(35,0,1),
C    3701(1,0,1), 702(10,0,1), 704(0,1,100), 249(1,0,2), 1310(1,0,5),
C    41304(1,0,10), 261(1,0,100),                800(35,0,1), 801(1,0,1)
C    5, 802(10,0,1), 804(0,1,100), 360(1,0,2), 1306(1,0,10), 371(1,0,10)
C    6, 440(5,1,1) , 1308(35,0,1), 401(0,4,5), 420(5,1,1), 1299(0,3,1)
C     DIMENSION C(26)
      CALL HOLITH(Q000HL,4H(1X,  )
      C(1) = Q000HL
      DO 1031 I = 3,25,2
      CALL HOLITH(Q001HL,3H2X,   )
 1031 C(I) = Q001HL
      CALL HOLITH(Q002HL,3H3X,   )
      C(15) = Q002HL
      DO 1032 I = 2,26,2
      CALL HOLITH(Q003HL,5HF7.3, )
 1032 C(I) = Q003HL
      CALL HOLITH(Q004HL,5HF7.5, )
      C(10) = Q004HL
      CALL HOLITH(Q005HL,5HF8.5, )
      C(14) = Q005HL
      C(22) = Q004HL
      CALL HOLITH(Q006HL,5HF8.5) )
      C(26) = Q006HL
  504 FORMAT( 4H M1=F13.6,5H, P1=F12.5,5H, T1=F12.5,5H, V1=F8.5,8H,  P1C
     1M=F12.5,7H, T1CM=F12.5,7H, V1CM=F8.5)
  505 FORMAT (4H M2=F13.6,5H, P2=F12.5,5H, T2=F12.5,5H, V2=F8.5,8H,  P2C
     1M=F12.5,7H, T2CM=F12.5,7H, V2CM=F8.5)
  506 FORMAT( 4H M3=F13.6, 47H, FOR LAB QUANTITIES OF PRTCL 3 SEE TABLE
     1BELOW, 8H,  P3CM=, F12.5, 7H, T3CM=, F12.5, 7H, V3CM=, F8.5)
  507 FORMAT( 4H M4=F13.6, 47H, FOR LAB QUANTITIES OF PRTCL 4 SEE TABLE
     1BELOW, 8H,  P4CM=, F12.5, 7H, T4CM=, F12.5, 7H, V4CM=, F8.5)
  511 FORMAT ( 39H CENTER OF MASS RELATIVE TO LAB GAMMA =, F12.5, 5H,  V
     1=, F8.5, 8H,  ETA =, F12.5, 1H,,3X, 17HTOTAL ENERGY CM = ,F13.6//)
  513 FORMAT(/1X, 7HTHET3CM, 2X, 7H THET3 , 2X, 7H  MOM3 , 2X, 7H   T3
     1 , 2X, 7H   V3  , 2X, 7HCM/LABT, 2X, 8HCOSTHET3, 3X,
     2                           7H THET4 , 2X, 7H  MOM4 , 2X, 7H   T4
     3 , 2X, 7H   V4  , 2X, 7HCM/LABT, 2X, 8HCOSTHET4)
  600 FORMAT (1H1)
C
C     WRITE HEADINGS  (DIRECTIONS FOR THE USE OF THE PROGRAM)
      CALL AHEAD
      B = .572957795E+02
      ROE = 3.E-8
      PRINT 600
C     FORMAT 600 IS 1H1 (SKIP PAGE)
C
    9 FORMAT (8F10.5)
 2    READ 9,XM1,XM2,XM3,XM4,P1,PMAX1,P2
      READ 9,T1,TMAX1,DELTA,THETMN,THETMX,DTHET
C
C     SUPPLY, IF NOT READ IN, THETMN, THETMX, DTHET, AND DELTA
 1299 IF (DTHET) 2, 3, 1700
    3 DTHET=5.0
 1700 IF (THETMX) 2, 1701, 7
 1701 THETMX = 180.0
    7 IF(DELTA)2,5,6
    5 DELTA=5.0
C
C     THRESHOLD CALCULATIONS
    6 IF (XM2) 14,13,14
   13 Q=XM1-XM3-XM4
 1300 IF (Q) 1042, 1040, 1040
 1040 PRINT 1041,Q
 1041 FORMAT(91H I.  DECAY OF PARTICLE 1 WILL TAKE PLACE AT ZERO INCIDEN
     1T KINETIC ENERGY.  THE Q VALUE IS  , F10.5//)
      TTH1 = 0.0
      ERRTH1 = ROE
      GO TO 20
 1042 PRINT 1043 ,XM1,XM3,XM4,Q
 1043 FORMAT ( 27H I.  DECAY OF PARTICLE 1 (  , F10.5, 10H) INTO 3 (  ,
     1 F10.5,  10H) AND 4 (  , F10.5,  38H) CAN NOT TAKE PLACE.  THE Q V
     2ALUE IS   , F10.5 ///)
      GO TO 2
 14   PRINT 1000,XM1,XM2,XM3,XM4
 1000 FORMAT (  49H I.  THRESHOLD PARAMETERS FOR THE REACTION    M1(,
     1 F10.5, 7H) + M2(, F10.5,   14H)  YIELDS  M3(, F10.5,  7H) + M4(,
     2 F10.5, 1H)//)
      E2=SQRT(P2**2+XM2**2)
      T2=E2-XM2
      V2=P2/E2
 1301 IF (P2) 1004,1003,1004
 1003 TERM1 = XM1 + XM2
      GO TO 1005
 1004 TERM1=SQRT(XM1**2+XM2**2+ 2.0*XM1*E2)
 1005 TERM2=XM3+XM4
      Q=TERM1-TERM2
      ERRQ =  ROE  * (TERM1 + TERM2)
 1015 IF (Q+ERRQ) 17,17,16
   16 IF(Q-ERRQ) 18,18,15
   18 Q=0.0
 15   PRINT 510,P2,Q
  510 FORMAT(39H THIS REACTION WILL TAKE PLACE, FOR P2= , F11.5, 46H, AT
     1 ZERO INCIDENT KINETIC ENERGY.  THE Q IS = , F12.5)
      TTH1=0.0
      ERRTH1=ERRQ
      PTH1=0.0
      ETH1=XM1
      GO TO 20
 17   PRINT 602,P2,Q
  602 FORMAT(48H THIS REACTION WILL NOT TAKE PLACE, EVEN FOR P2= , F11.5
     1, 46H, AT ZERO INCIDENT KINETIC ENERGY.  THE Q IS=   , F10.5)
   19 IF (P2 ) 22,21,22
   21 ETH1=0.5*((XM3+XM4)**2-XM1**2-XM2**2)/XM2
      TTH1=ETH1-XM1
      PTH1=SQRT(ETH1**2-XM1**2)
      T2=0.0
      E2=XM2
      V2=0.0
      GO TO 23
   22 E2=SQRT(P2**2+XM2**2)
      V2=P2/E2
      T2=E2-XM2
      DELM=0.5*((XM3+XM4)**2-XM1**2-XM2**2)
      ROOT=SQRT(DELM**2 - XM1**2 * XM2**2)
      PTH1 = (DELM * P2 + E2 * ROOT) / XM2**2
      ETH1=SQRT(PTH1**2+XM1**2)
      TTH1=ETH1-XM1
   23 ERRTH1 = ROE*(XM3+XM4)**2/XM2
C
C     WE NOW KNOW E2,T2,P2,V2,PTH1,TTH1,ETH1,ERRTH1
      VTH1=PTH1/ETH1
      VTHCM=(PTH1+P2)/(ETH1+E2)
      ETTHCM=XM3+XM4
      GTHCM = (ETH1 + E2) / ETTHCM
      ETATHC=VTHCM*GTHCM
      PTHCM2 = GTHCM * (-V2 + VTHCM) * E2
      PTHCM1 = PTHCM2
      TTHCM1=SQRT(PTHCM1**2+XM1**2)-XM1
      VTHCM1=PTHCM1/(TTHCM1+XM1)
      TTHCM2=SQRT(PTHCM2**2+XM2**2)-XM2
      VTHCM2=PTHCM2/(TTHCM2+XM2)
      PRINT 511,GTHCM,VTHCM,ETATHC,ETTHCM
      PRINT 504,XM1,PTH1,TTH1,VTH1,PTHCM1,TTHCM1,VTHCM1
      PRINT 505,XM2,P2,T2,V2,PTHCM2,TTHCM2,VTHCM2
C
C     IF TMAX1 IS ZERO, ASSUME THAT P1 WAS READ IN
   20 IF(TMAX1)2,60,30
   30 E1=T1+XM1
      P1=SQRT(E1**2-XM1**2)
      V1 = P1/E1
 1302 IF(T1-TTH1+ERRTH1) 91,92,92
 91   PRINT 503,T1
  503 FORMAT (/19H THIS VALUE OF T1 (,F10.5,21H) IS BELOW THRESHOLD.//)
      GO TO 401
   60 E1=SQRT(P1**2+XM1**2)
      T1=E1-XM1
   90 V1=P1/E1
 1310 IF (T1 - TTH1 + ERRTH1) 1120, 92, 92
 1120 PRINT 1121,P1
 1121 FORMAT (/19H THIS VALUE OF P1 (,F10.5,21H) IS BELOW THRESHOLD.//)
      GO TO 401
C
C     WE NOW KNOW E1,T1,P1,V1, FOR EITHER P1 OR T1 READ IN
   92 IF(XM2) 94,93,94
   93 ETCM=XM1
      GCM=E1/XM1
      VCM=V1
      VCMA = ABS(VCM)
      ETACM=GCM*VCM
      PCM1=0.0
      TCM1=0.0
      VCM1=0.0
      ERV = ROE
      GO TO 131
   94 ETCM = SQRT(XM1**2 + XM2**2 + 2.0*(E1*E2 - P1*P2))
  100 GCM=(E1+E2)/ETCM
  110 VCM = (P1 + P2) / (E1 + E2)
      VCMA = ABS(VCM)
  120 ETACM=GCM*VCM
  130 PCM2 = GCM * (-V2 + VCM) * E2
      PCM1 = PCM2
      TCM1=SQRT(PCM1**2+XM1**2)-XM1
      VCM1=PCM1/(TCM1+XM1)
C
      TCM2=SQRT(PCM2**2+XM2**2)-XM2
      VCM2=PCM2/(TCM2+XM2)
C     WE NOW KNOW ETCM,GCM,VCM,ETACM,PCM1,TCM1,VCM1,PCM2,TCM2,VCM2
C
C     WRITE OUT HEADINGS FOR PROBLEM + LAB AND CM QUANTITIES FOR PARTICL
C     1 AND 2 PLUS LAB TO CM TRANSFORMATION PARAMETERS
      ERV = ROE
  131 IF (TMAX1) 2, 609, 610
 609  PRINT 619,P1,P2
  619 FORMAT (///35H II.  PROBLEM SOLVED BELOW FOR  P1= , F12.5, 5X,
     1 6H   P2=, F12.5//)
      GO TO 1051
 610  PRINT 620,T1,P2
  620 FORMAT (///35H II.  PROBLEM SOLVED BELOW FOR  T1= , F12.5, 5X,
     1 6H   P2=, F12.5//)
 1051 PRINT 511,GCM,VCM,ETACM,ETCM
 1030 PRINT 504,XM1,P1,T1,V1,PCM1,TCM1,VCM1
 1303 IF (XM2) 1052, 1052, 1050
 1050 PRINT 505,XM2,P2,T2,V2,PCM2,TCM2,VCM2
 1052 CONTINUE
C
C     NOW BEGIN CALCULATIONS ON PARTICLE 3
      THECM3=THETMN
      TCM3 = ((ETCM - XM3)**2 - XM4**2) * 0.5 / ETCM
  170 ECM3 = TCM3 + XM3
  180 PCM3 = SQRT(TCM3 * (TCM3 + 2.0* XM3 ) )
  190 VCM3=PCM3/ECM3
      PRINT 506,XM3,PCM3,TCM3,VCM3
      PCM4 = PCM3
      ECM4 = SQRT(PCM4**2 + XM4**2)
      TCM4 = ECM4 - XM4
      VCM4=PCM4/ECM4
      PRINT 507,XM4,PCM4,TCM4,VCM4
      ISKIP = 0
      PRINT 513
  200 IF (MOD(ISKIP,6)) 1070, 1070, 1071
 1070 PRINT 1072
 1072 FORMAT (1H  )
 1071 ISKIP = ISKIP + 1
      PX3=GCM*(PCM3*COS(THECM3/B)+VCM*ECM3)
  210 PY3=PCM3*SIN(THECM3/B)
  220 P3=SQRT(PX3**2+PY3**2)
  230 E3=SQRT(P3**2+XM3**2)
      T3=E3-XM3
  240 V3=P3/E3
  700 IF (THECM3 - 180.0* (1.0- ROE)) 249,701,701
  701 IF (VCM3 - (VCMA+ ERV)) 702, 703, 703
  702 IF (VCM3 - (VCMA- ERV)) 705, 704, 704
  703 THET3 = 180.0
      GO TO 258
  704 IF (T1) 1081, 1080, 1081
 1080 THET3 = THECM3
      GO TO 258
 1081 THET3 = 90.0
      GO TO 258
  705 THET3 = 0.0
      GO TO 258
  249 IF (PX3) 1082, 704, 1082
 1082 THET3 = ATANG(PY3/PX3) * B
  258 COS3=COS(THET3/B)
      SLD3N = ABS(PCM3**2*GCM*(PCM3 + VCM*ECM3*COS(THECM3/B)))
      ERSLDN = ROE * PCM3**3 * GCM * 2.0
 1304 IF ( SLD3N - ERSLDN) 261, 261, 262
  261 IF (SLD3N - 2.0*ROE) 1111, 1111, 1112
 1111 SLDGL3 = 0.0
      GO TO 270
 1112 SLDGL3 = 999999.0
      GO TO 270
  262 SLDGL3 = P3**3 /  SLD3N
  270 C(6) = TEST(P3)
      C(8) = TEST(T3)
      C(12) = TEST(SLDGL3)
C
C     NOW BEGIN CALCULATIONS ON PARTICLE 4
  300 THECM4 = 180.0 - THECM3
  350 PX4=GCM*(PCM4*COS(THECM4/B)+VCM*ECM4)
      PY4=PCM4*SIN(THECM4/B)
      P4=SQRT(PX4**2+PY4**2)
      E4=SQRT(P4**2+XM4**2)
      T4=E4-XM4
      V4=P4/E4
  800 IF (THECM4 - 180.0* (1.0- 100.0*ROE)) 360,801,801
  801 IF (VCM4 - (VCMA+ ERV)) 802, 803, 803
  802 IF (VCM4 - (VCMA- ERV)) 805, 804, 804
  803 THET4 = 180.0
      GO TO 370
  804 IF(T1) 1091, 1090, 1091
 1090 THET4 = THECM4
      GO TO 370
 1091 THET4 = 90.0
      GO TO 370
  805 THET4 = 0.0
      GO TO 370
  360 IF (PX4) 1092, 804, 1092
 1092 THET4 = ATANG(PY4/PX4) * B
  370 COS4=COS(THET4/B)
      SLD4N = ABS(PCM4**2*GCM*(PCM4 + VCM*ECM4*COS(THECM4/B)))
      ERRSL4 = ROE*PCM4**3*GCM*2.0
 1306 IF( SLD4N-ERRSL4) 371,371,372
  371 IF (P4 - 2.0*ROE ) 1101, 1101, 1102
 1101 SLDGL4 = 0.0
      GO TO 373
 1102 SLDGL4 = 999999.0
      GO TO 373
  372 SLDGL4 = P4**3 /  SLD4N
  373 C(18) = TEST(P4)
      C(20) = TEST(T4)
      C(24) = TEST(SLDGL4)
      PRINT C,THECM3,THET3,P3,T3,V3,SLDGL3,COS3,THET4,P4, T
     14, V4, SLDGL4, COS4
      THECM3 = THECM3 + DTHET
 1308 IF (THECM3 - THETMX - ROE) 200, 200, 400
 400  PRINT 600
  401 IF(TMAX1)2,410,430
  410 P1=P1+DELTA
  420 IF(P1-PMAX1-ROE)60,60,2
  430 T1=T1+DELTA
  440 IF(T1-TMAX1-ROE)30,30,2
       END
*STOP
*STOP
*
*
      PROGRAM Q900 B
C  USER  MUST  REQUIRE  TAPE 3  AND  TAPE 4
      CALL TIDY
      STOP
      END
*EXECUTE
*NOCARD
*NOEXEM
*NOLABE
*NOCOMM
      PROGRAM SI
      DIMEN
     1SION E(10)
C     EXECUTE
      N=1.+2.
      STOP
      END
*STOP
*STOP
*
*
      PROGRAM T110 A
      COMMON/N/NIS(20),CON(20)/B/NZ,JSITR,MKB,T
     */OR/M1,M2,M3,M4,M5,M6,M7,M8,M9,M10/COMP/JCOMP(26)/MK/GMK(11,26)
      DATA(MKB=-1),(T=300.),(JSITR=2),(NIS=3,2,4,8,9,10,24,7),
     1(CON=0.001436,0.007764,0.01055,0.01053,0.00278,0.00124,0.00248,
     2 0.00729),(NZ=8),(JCOMP=5,6,15),(M2=1),(M3=1),(M5=1),(M6=1),
     3(M7=1),(M8=1),(M9=1)
C
      CALL  A P A M A K
C
      END
*EXECUTE
*
*
      PROGRAM T110 B
C        *FILE:SCRATCH,F08,W,10
C        *FILE:SCRATCH,F04,W,10
C        *CALL FICMEMORY
         CALL T R A X Y
      END
      FUNCTION SQRTF (X )
         SQRTF=SQRT(X)
      RETURN
      END
*CALL FICMEMOR
*EXECUTE
     0
*************    T R A X Y   - TEST REGULAR        *********************
     0     1     4     2     6     6     1     1     1     1     1     0
     4     0     4     0     0     0     0     2     3     4     5     0
     0     3     6    10     0     0
                            0.0001      0.00001                1.
   0.          0.          0.          0.
 WA I 345
 3.58   -004 0.          3.63358-001 2.35   -001 0.
 1.93   -002 0.          3.13930+000 3.12   +000 1.28   -001
 WA A 345
 0.          0.          1.786  -001 1.25   -001 0.
 0.          0.          6.72   -001 6.72   -001 5.36   -002
 BR I 345
 4.41   -004 3.97   -004 2.95841-001 2.17   -001 0.
 7.17   -002 1.16   -002 1.8817 +000 1.81   +000 7.84   -002
 BR A 345
 0.          0.          1.325  -001 9.92   -002 0.
 0.          0.          4.38   -001 4.38   -001 3.33   -002
1021.       3
2020.       2021.          5.       3
2020.       2021.          5.       3
001006001006001003001003
          -1          -33
   1.          0.       3
1021.       3
105-.3333333103-.8819170105+.3333333103+.88191703
   -.9428093   -.8819170   -.3333333   +.3333333   +.8819170   -.4714045
   -.3333333   +.3333333   -.9428093   -.8819170   -.3333333   +.3333333
   +.8819170   -.4714045   -.3333333   +.33333333
    .0      104+.0833333    .0      102+.0833333   +.0      104+.0833333
    .0      102+.08333333
*
*
      PROGRAM T110 C
C        *FILE:SCRATCH,F08,W,10
C        *FILE:SCRATCH,F03,W,10
C        *CALL FICMEMORY
      CALL T R A C Y
      END
      FUNCTION SQRTF (X )
         SQRTF=SQRT(X)
      RETURN
      END
*CALL FICMEMOR
*EXECUTE
     0
M************    T R A C Y   -   T 2  1  REGULAR       ****************
     0     1     4     2     6     6     1     1     0     0     1     0
     4     0     4     0     0     0     0     2     3     4     5     0
     0     3     6   100     0     0
   0.          0.          0.001       0.0001      0.          1.
   0.          0.          0.          0.
 WA I 345
 3.58   -004 0.          3.63358-001 2.35   -001 0.
 1.93   -002 0.          3.13930+000 3.12   +000 1.28   -001
 WA A 345
 0.          0.          1.786  -001 1.25   -001 0.
 0.          0.          6.72   -001 6.72   -001 5.36   -002
 BR I 345
 4.41   -004 3.97   -004 2.95841-001 2.17   -001 0.
 7.17   -002 1.16   -002 1.8817 +000 1.81   +000 7.84   -002
 BR A 345
 0.          0.          1.325  -001 9.92   -002 0.
 0.          0.          4.38   -001 4.38   -001 3.33   -002
1021.       3
2020.       2021.          5.       3
2020.       2023.          5.       3
001006001006001003001003
          -1          -33
   1.          0.       3
1021.       3
105-.3333333103-.8819170105+.3333333103+.88191703
   -.9428093   -.8819170   -.3333333   +.3333333   +.8819170   -.4714045
   -.3333333   +.3333333   -.9428093   -.8819170   -.3333333   +.3333333
   +.8819170   -.4714045   -.3333333   +.33333333
    .0      104+.0833333    .0      102+.0833333   +.0      104+.0833333
    .0      102+.08333333
*
*
      PROGRAM U100
      DIMENSION K(2),M(41)
      CALL CLEBS(1,1,1,1,1,1,K,M)
      CALL CLEBS(6,3,4,1,2,3,K,M)
      CALL CLEBS(20,14,8,3,3,6,K,M)
      PRINT 1
      PRINT 2,K
      PRINT 3
      PRINT 4,M
      STOP
    1 FORMAT(50H0 FELD K ZAEHLER MIT VORZEICHEN UND NENNER VON C*C )
    3 FORMAT(23H0 FELD M-M(1) BIS M(41))
    2  FORMAT(10X,2F20.5)
    4 FORMAT(8(F10.5,2X))
      END
*EXECUTE
*
*
      PROGRAM U101
      DIMENSION A(4),B(4),X(4)
      DATA A/1.,2.,3.,0.0/,B/4.,5.,6.,0.0/,SM1,SM2/0.9382,0.139/
C     MACCA èPOTOHA  SM1=0.9382
C     MACCA èàOHA  SM2=0.139
      A(4)=SQRT(A(1)**2+A(2)**2+A(3)**2+SM1**2)
      B(4)=SQRT(B(1)**2+B(2)**2+B(3)**2+SM2**2)
      PRINT 1,A(4),B(4)
      CALL LOREN4(A,B,X)
      PRINT 2,X
  1   FORMAT(50X,9HTECT U101//10X,5HA(4)=, E20.11,5X,5HB(4)=,E20.11)
  2   FORMAT(//10X,2HX=,4E20.11)
      CALL EXIT
      END
*EXECUTE
*
*
      PROGRAM U102
      DIMENSION A(4),B(4),X(4)
      DATA A/1.,2.,3.,0.0/,B/4.,5.,6.,0.0/,SM1,SM2/0.9382,0.139/
C     MACCA èPOTOHA  SM1=0.9382
C     MACCA èàOHA  SM2=0.139
      A(4)=SQRT(A(1)**2+A(2)**2+A(3)**2+SM1**2)
      B(4)=SQRT(B(1)**2+B(2)**2+B(3)**2+SM2**2)
      PRINT 1,A(4),B(4)
      CALL LORENF(SM1,A,B,X)
      PRINT 2,X
      CALL LORENB(SM1,A,X,B)
      PRINT 3,B
  1   FORMAT(50X,9HTECT U102//10X,5HA(4)=, E20.11,5X,5HB(4)=,E20.11)
  2   FORMAT(//10X,2HX=,4E20.11)
  3   FORMAT(//10X,2HB=,4E20.11)
      STOP
      END
*EXECUTE
*
*
      PROGRAM U101
      C=CLEBSH(200.,200.,0.,0.,200.,0.)
      PRINT 4,C
      C=CLEBSH(0.,0.,0.,1.,0.,1.)
      PRINT 4,C
      C=CLEBSH(0.,0.,1.,1.,0.,2.)
      PRINT 4,C
      C=CLEBSH(1.,0.,1.,0.,2.,1.)
      PRINT 4,C
      C=CLEBSH(1.,0.,1.,0.,0.,1.)
      PRINT 4,C
      C=CLEBSH(1.,1.,1.,1.,1.,1.)
      PRINT 4,C
    4 FORMAT(10X,7HCLEBSH=,F14.9)
      DO 5 I=1,16
    2 READ 1,BETA,GAMMA,A,B,C
    1 FORMAT(5F4.1)
      PRINT 1,BETA,GAMMA,A,B,C
      D=GAMMA-BETA
      E=CLEBSH(A,B,D,BETA,C,GAMMA)
      PRINT 3,A,B,D,BETA,C,GAMMA,E
    3 FORMAT(10X,7HCLEBSH(,5(F5.1,1H,),F5.1,2H)=,F14.9)
    5 CONTINUE
      END
*EXECUTE
-.5 0.  .5  .5  1.
.5  0.  .5  .5  1.
.0  .0  1.  1.  1.
.0  .5  .5  1.  1.5
1.  1.5 .5  1.  1.5
1.5 1.5 1.  1.5 1.5
1.  2.  1.  1.  2.
-.5 0.  1.5 1.5 2.
.5  0.  1.5 1.5 2.
2.  1.  1.  2.  2.
1.5 3.  1.5 1.5 3.
2.5 1.5 1.  2.5 2.5
-.5 .5  2.  2.5 2.5
-1.50.  1.5 2.5 3.
2.  3.  3.  3.  3.
1.5 3.5 3.  3.5 3.5
*
*
      PROGRAM U121
         DIMENSION A(3),B(3),C(3)
         A(1)=0.0298/0.105655
         A(2)=0.0
         A(3)=0.0
         B(1)=-7.23329*0.99040
         B(2)=0.0
         B(3)=0.0
         CALL LORENC(A,B,C)
         PRINT 2,C
  2      FORMAT(1X,1F10.5)
         END
*EXECUTE
*
*
      PROGRAM U500
      DIMENSION DKEEP(20)
      N=1000
      DBETA = 360./(N-1)
      BETA = -DBETA
      DO 6 J= 1, 3
      AJ = FLOAT(J)/2.
      AM = AJ
      AN = 0.
      DO 5 I= 1, N
      BETA = -DBETA
    5 DSM = DSMALL(AJ,AM,AN,BETA)
    6  CONTINUE
      N = 20
      K = 0
      DBETA = 360./(N-1)
   10 K = K+1
      PRINT 30
      IF(K.GE.9) GO TO 70
      BETA = -DBETA
      GO TO (11,12,13,14,15,16,17,18),K
   11 AJ = 0.5
      AM = 0.5
      AN  = 0.5
      GO TO 20
   12 AJ = 0.5
      AM = 0.5
      AN  = -0.5
      GO TO 20
   13 AJ = 1.
      AM = 0.
      AN  = 0.
      GO TO 20
   14 AJ = 1.5
      AM = 1.5
      AN  = -0.5
      GO TO 20
   15 AJ = 1.5
      AM = 0.5
      AN  = -0.5
      GO TO 20
   16 AJ = 2.
      AM = 2.
      AN  = 0.
      GO TO 20
   17 AJ = 2.
      AM = 1.
      AN  = 0.
      GO TO 20
   18 AJ = 2.
      AM = 0.
      AN  = 0.
   20 PRINT 30
   30 FORMAT (1H0)
      PRINT 40
   40 FORMAT (13X,1HJ,7X,1HM,6X,2H N,4X,4HBETA,5X,5HDSMAL,7X,5HDTEST,
     * 6X,5HDIFF.)
      DO 50 I=1,N
      BETA = BETA+DBETA
      DSML = DSMALL(AJ,AM,AN,BETA)
      DTST = FTEST(BETA,K)
      DIFF = DTST-DSML
   50 PRINT 60,   AJ,AM,AN,BETA,DSML,DTST,DIFF
   60 FORMAT( 9X,4(F5.1,3X),2(F8.5,3X),E12.5)
      GO TO 10
   70 DSM1 = DSMALL(1.0,2.0,0.0,45.)
C        TEST FOR HIGH VALUES IF SPIN INDICES
      PRINT 149
  149 FORMAT (50H1 TEST DSMALL FOR LARGE VALUES OF INDICES            /)
      BET45 = 1.
      DO 200 I= 1, 12
      AI= I
      IPLUS1 = I + 1
      DO 190 J= 1, IPLUS1
      JM = J-1
      AJ = J-1
      CALL VZERO(DKEEP,J)
      DO 180 K= 1, IPLUS1
      AK = K-1
      DKEEP(K) = DSMALL(AI,AJ,AK,BET45)
  180 CONTINUE
      PRINT 210, I,JM,(DKEEP(KK),KK=1,J)
  190 CONTINUE
      PRINT 30
  200 CONTINUE
  210 FORMAT (2X,2I4,10F12.8/5X,10F12.8)
      END
      FUNCTION FTEST(BETA,K)
      BETB  = BETA*3.1415926535898/180.
      GO TO (11,12,13,14,15,16,17,18),K
   11 FTEST = COS(BETB/2.)
      RETURN
   12 FTEST = -SIN(BETB/2.)
      RETURN
   13 FTEST = COS(BETB)
      RETURN
   14 FTEST = SQRT(3.)*(1.-COS(BETB))*COS(BETB/2.)/2.
      RETURN
   15 FTEST = -(3.*COS(BETB)+1.)*SIN(BETB/2.)/2.
      RETURN
   16 FTEST = SQRT(1.5)*(SIN(BETB))**2/2.
      RETURN
   17 FTEST = -SQRT(1.5)*SIN(BETB)*COS(BETB)
      RETURN
   18 FTEST = 1.5*(COS(BETB))**2-0.5
      RETURN
      END
*EXECUTE
*
*
      PROGRAM V100
      DIMENSION A(30)
      DO 1 K=1,30,2
      CALL RANNOR(X,Y)
      A(K)=X
  1   A(K+1)=Y
      PRINT 2,A
  2   FORMAT(3(10F12.7/))
      END
*EXECUTE
*
*
      PROGRAM V102
      COMMON X(500),Y(500),R,S
      DO 10 I=1,500
      CALL NORMCO(X(I),Y(I),0,0,1,1,0)
   10 CONTINUE
      CALL TESTPR
      DO 20 I=1,500
      CALL NORMCO(X(I),Y(I),10,10,5,5,0)
   20 CONTINUE
      CALL TESTPR
      DO 30 I=1,500
      CALL NORMCO(X(I),Y(I),10,10,5,5,0.5)
   30 CONTINUE
      CALL TESTPR
      DO 40 I=1,500
      CALL NORMCO(X(I),Y(I),10,10,5,5,1)
   40 CONTINUE
      CALL TESTPR
      DO 50 I=1,500
      CALL NORMCO(X(I),Y(I),5,10,3,5,0)
  50  CONTINUE
      CALL TESTPR
      STOP
      END
      SUBROUTINE TESTPR
      COMMON X(500),Y(500),R,S
      DO 110 I=1,500
 110  R=R+X(I)
      R=R/500
      DO 120 I=1,500
 120  S=S+Y(I)
      S=S/500
      PRINT 10
      PRINT 11,(X(I),I=1,500)
      PRINT 12
      PRINT 11,(Y(I),I=1,500)
      PRINT 13
      PRINT 14,R,S
      RETURN
   10 FORMAT (1H1,44H FIRST VALUES OF THE PAIR OF RANDOM NUMBERS )
   12 FORMAT (1H1,45H SECOND VALUES OF THE PAIR OF RANDOM NUMBERS )
   13 FORMAT (1H0,28H MEAN OF THE RANDOM NUMBERS )
   11 FORMAT (1H0,10(F10.5,2X))
   14 FORMAT (1H0,2(F10.5,2X))
      END
*EXECUTE
*
*
      PROGRAM V104
         DIMENSION A(100),IB(100)
      DO 111   IX=1,3
         DO 2 I=1,100
 2       A(I) = RNDM(-1.0)
         DO 1 I=1,100
  1   IB(I)=RNDM(1.0)
         PRINT 10
         PRINT 11,(A(I),I=1,100)
         PRINT 12
         PRINT 13,(IB(I),I=1,100)
         PRINT 14
      PRINT 15,(IB(I),I=1,100)
 111   CONTINUE
  10     FORMAT(1H1,10X,19HRANDOM NUMBER FLOAT/)
 11      FORMAT(5X,5F20.12)
  12     FORMAT(/10X,14HNUMBER INTEGER/)
 13   FORMAT(5X,5I18)
  14     FORMAT(/10X,10HOCT NUMBER/)
  15  FORMAT(5X,5O20)
      END
*EXECUTE
*
*
      PROGRAM V106
      DIMENSION POUT(50,50), NOUT(50)
      DATA NC1,NC2,NC3/ 2H  , 2H.., 2H++/
C         PRINT 500 RANDOM NUMBERS
      CALL PRT500
C                            . . READ MEAN VALUE AND INITIALIZE ........
      XMEAN = 4.
      MEAN = XMEAN
      QDEV = SQRT(XMEAN)
      N1 = XMEAN - QDEV
      N2 = XMEAN + QDEV
C
      DO 500 IX= 2, 5
      IXX = IX - 1
      DO 490 IY= 1, IXX
      LEN = IX
      CALL VZERO(POUT,2500)
C                            . . GENERATE RANDOM DISTRIBUTION ..........
      DO 200 IL= 1, MEAN
      DO 180 JL= 1, 2500
      A = RN32(JL)
      B = RN32(JL+1)
      IF (RN32(JL+2) .GT. 0.9)  B=RN32(JL+3)
      I = A*50. + 1.0
      J = B*50. + 1.0
  180 POUT(I,J) = POUT(I,J) + 1.0
  200 CONTINUE
C                                   ..   FIND CHISQUARE
      CHI2 = 0.000
      DO 300 J= 1, 50
      DO 280 I= 1, 50
      CHI2 = CHI2 + (POUT(I,J)-XMEAN)**2 / XMEAN
  280 CONTINUE
  300 CONTINUE
      DEV = (CHI2-2500.)/SQRT(5000.)
      PRINT 1003, IX,IY,XMEAN,CHI2, DEV
C                            . . PRINT VISUAL PATTERN DENSITY ..........
      DO 400 J= 1, 50
      DO 380 I= 1, 50
      N = POUT(I,J)
      NOUT(I) = NC1
      IF (N .GE. N1)  NOUT(I) = NC2
      IF (N .GT. N2)  NOUT(I) = NC3
  380 CONTINUE
  400 PRINT 1004, NOUT
  490 CONTINUE
  500 CONTINUE
  600 CONTINUE
      CALL RN32OT(IOUT)
      PRINT 1025, IOUT
      CALL PRT500
      IX = 12335
      CALL RN32IN(IX)
      PRINT 1025, IX
C         PRINT 500 RANDOM NUMBERS
      CALL PRT500
      IX =        2147483647
      CALL RN32IN(IX)
      PRINT 1025,IX
      CALL PRT500
C
 1000 FORMAT (4F10.0)
 1003 FORMAT (1H1,2I10,2X,F10.2,5X,5HCHI2=,F10.1,5X,4HDEV=,F10.5/)
 1004 FORMAT (20X ,50A2)
 1005 FORMAT (1H1///2I10,20X,F10.5///)
 1025 FORMAT (15H0RN32OT VALUE         I12)
      STOP
      END
      SUBROUTINE PRT 500
C         PRINT 500 RANDOM NUMBERS
      DIMENSION VEC(20)
COMMENT                                                              1 =
      DO 5 I= 1, 500,10
      DO 4 J= 1, 10
    4 VEC(J) =  RN32 (J)
      PRINT 1006, I,(VEC(L),L=1,10)
 1006 FORMAT (I5,10F12.9)
    5 CONTINUE
      RETURN
      END
*EXECUTE
*
*
      PROGRAM V106 A
C  TECT Ñãü èPàMEPA B OèàCAHàà
      DIMENSION A(10)
C        ÉEHEPAñàü 10 CãYóAâHõX óàCEã
      DO 1 I=1,10
   1  A(I)=RN32(I)
      PRINT 2,A
C  ÉEHEPAñàü CãYóAâHõX óàCEã C HAóAãúHõM  CèEñàAãúHõM  óàCãOM
      CALL RN32IN(65539)
      DO 3 I=1,5
   3  A(I)=RN32(I)
      PRINT 4,(A(K),K=1,5)
C   áAãOMHàTú    èEñàAãHOE   óàCãO èOCãE 5 OÅPAôEHàâ
      CALL RN32OT(IX)
      PRINT 5,IX
C  èPOÑOãÜàTú  ÉEHEPAñàû 10 CãYóAâHõX óàCEã
      DO 6 I=1,5
   6  A(I)=RN32(I)
      PRINT 4,(A(K),K=1,5)
C   BOáOÅHOBàTú BõóàCãEHàE, HAóàHAü C 5-ÉO  CãYóAâHOÉO  óàCãA
      CALL RN32IN(IX)
      DO 7 I=1,5
   7  A(I)=RN32(I)
      PRINT 4,(A(K),K=1,5)
   2  FORMAT(1X,10F12.9)
   4  FORMAT(10X,5F12.9)
  5   FORMAT(5X,3HIX=,I12)
      STOP
      END
*EXECUTE
*
*
      PROGRAM V110
      DIMENSION AMU(5)
      DATA AMU/1.,50.,0.845,-3.5,100./
      PRINT 3
      DO 1 I=1,5
      CALL POISSN(AMU(I),N,IERROR)
  1   PRINT 2 ,AMU(I),N,IERROR
  2   FORMAT(30X,F10.3,2I10)
  3   FORMAT(50X,9HTEST V110///35X,3HAMU,10X,1HN,10X,6HIERROR)
      STOP
      END
*EXECUTE
*
*
      PROGRAM V111
      DIMENSION P(5)
      DATA P/0.5,0.8,1.5,1.,0.9/
      PRINT 3
      NTOT=10
      DO 1 I=1,5
      CALL BINOMI(NTOT,P(I),N,IERROR)
  1   PRINT 2 ,NTOT,P(I),N,IERROR
  2   FORMAT(20X,I10,F10.2,2I10)
  3   FORMAT(50X,9HTEST V111///25X,4HNTOT, 9X,1HP,10X,1HN, 6X,6HIERROR)
      STOP
      END
*EXECUTE
*
*
      PROGRAM V112
      DIMENSION N(10),P(10)
      DATA P/9*0.001,0.991/
      PRINT 3
      CALL CTIME(CT1,RT1)
      DO 100 I=1,100
      CALL MUNOMI(10,50,P,N,IERROR)
  100 CONTINUE
      CALL CTIME(CT2,RT2)
      RT=RT2-RT1
      PRINT 2,P,N,IERROR
      PRINT 1,RT
  1   FORMAT(50X,3HRT=,F7.2,3HSEC)
  2   FORMAT(5X,2HP=,10F10.3//5X,2HN=,10I4//5X,7HIERROR=,I2)
  3   FORMAT(50X,9HTEST V112///)
      STOP
      END
*EXECUTE
*
*
      PROGRAM V116
      IY=0
      DO 10 I=1,10
         IF(URAND(IY).LT.0.5) GO TO 5
         PRINT 1
         GO TO 10
  5      PRINT 2
  10  CONTINUE
   1  FORMAT(6H HEADS)
   2  FORMAT(6H TAILS)
      STOP
      END
*EXECUTE
*
*
      PROGRAM V130
      COMMON /HSTBLK/ JHIST(20), JHISTY(20),JHISTZ(20)
      DATA XLONG/1.0/
C         FIRST TIMING.  THE CLASSICAL METHOD FOR 20000 VECTORS
      CALL CTIME(CT1,T1)
      DO 100 KK= 1, 20000
      CALL RNDDIR(X,Y,Z,XLONG)
  100 CONTINUE
C               TIMING.  THE REJECTION METHOD FOR 20000 VECTORS
      CALL CTIME(CT2,T2)
      DO 120 KK= 1, 20000
      CALL RAN3D(X,Y,Z,XLONG)
  120 CONTINUE
      CALL CTIME(CT3,T3)
      TIME1 = T2-T1
      TIME2 = T3 - T2
      PRINT 1001,TIME1,TIME2
      IF (TIME1 .LT. TIME2)  PRINT 1005
C         TEST THAT THE RESULTING DISTRIBUTIONS ARE UNIFORM
      DO 400 IDO= 1, 3
      CALL VZERO(JHIST,60)
      DO 200 KK= 1, 20000
      CALL RAN3D(X,Y,Z,XLONG)
      IBIN = X*10. + 11.
      JBIN = Y*10. + 11.
      KBIN = Z*10. + 11.
      JHIST (IBIN)= JHIST(IBIN) + 1
      JHISTY(JBIN) = JHISTY(JBIN) + 1
      JHISTZ(KBIN) = JHISTZ(KBIN) + 1
  200 CONTINUE
C         CALCULATE CHISQUARE FROM UNIFORM DISTR. IN X,Y,Z
      DEV = SQRT(1000.)
      CHI2 = 0.
      DO 300 IBIN= 1, 60
      CHI2 = CHI2 +(FLOAT(JHIST(IBIN)-1000)/DEV)**2
  300 CONTINUE
      PRINT 1006
      PRINT 1000, JHIST
      PRINT 1000, JHISTY
      PRINT 1000, JHISTZ
      P = PROB(CHI2,57)
      PRINT 1002, CHI2,P
      IF (P .LT. 0.01)  PRINT 1003
      IF (P .GT. 0.99)  PRINT 1004
  400 CONTINUE
 1000 FORMAT (20I5)
 1001 FORMAT (  38H1  TIMING FOR 20000 RANDOM 3-D VECTORS/  19H   CLASSI
     1CAL METHODF10.4,  25H         REJECTION METHODF10.4,  9H  SECONDS/
     2///)
 1002 FORMAT (  11H0CHISQUARE=F10.4,  15H         PROB.=F10.6/)
 1003 FORMAT (  36H0   TEST FAILED.  CHISQUARE TOO BIG /2X,30(1H*)///)
 1004 FORMAT (  38H0   TEST FAILED.  CHISQUARE TOO SMALL /2X,30(1H*)///)
 1005 FORMAT (  41H0  WARNING.   CLASSICAL METHOD IS FASTER.//)
 1006 FORMAT(/  83H0THE FOLLOWING NUMBERS SHOULD BE AROUND 1000.     THE
     1 CHISQUARE SHOULD BE ABOUT 57./)
      END
      SUBROUTINE RNDDIR(X,Y,Z,XLONG)
      DATA TWOPI/6.283185308/
      Q=-1.
      Z = 2.0*RNDM(Q)-1.0
      PT = SQRT(1.0-Z**2)
      PHI = TWOPI*RNDM(Q)
      X = PT * COS(PHI)
      Y = PT * SIN(PHI)
      RETURN
      END
*EXECUTE
*
*
      PROGRAM V150
      DIMENSION Y(1000)
      N = 1000
      DO 100 I= 1, N
      Y(I) = FLOAT(I)
      IF (2*(I/2) .EQ. I)  Y(I) = 0.
      IF (3*(I/3) .EQ. I)  Y(I) = 0.
  100 CONTINUE
      XLO = -10.
      XWID = 2.
      NTHROW = 100
      N = 300
      CALL HRTSUB(Y,N,XLO,XWID,NTHROW)
      N = 200
      CALL HRTSUB(Y,N,XLO,XWID,NTHROW)
      XLO = 0.
      N = 100
      CALL HRTSUB(Y,N,XLO,XWID,NTHROW)
      N = 50
      CALL HRTSUB(Y,N,XLO,XWID,NTHROW)
      N = 10
      CALL HRTSUB(Y,N,XLO,XWID,NTHROW)
      N = 1
      CALL HRTSUB(Y,N,XLO,XWID,NTHROW)
      END
      SUBROUTINE HRTSUB(Y,N,XLO,XWID,NTHROW)
C         SUBROUTINE TO TEST HISRAN (V150)
      DIMENSION Y(N),YSAVE(100)
C         FIRST CALL TO GET CIM. PROB. DISTR. SET UP.  PRINT NEW Y
      PRINT 1,(Y(I),I=1,N)
      CALL HISPRE(Y,N,XLO,XWID,XRAN)
      PRINT 2
      PRINT 1,(Y(I),I=1,N)
  1   FORMAT(20X,1HY/10(5X,10F10.5/))
  2   FORMAT(/50X,6HHISPRE/)
      CALL CTIME(CT1,RT1)
C         MAIN LOOP TO GENERATE NTHROW NUMBERS
      DO 200 I= 1, NTHROW
      CALL HISRAN(Y,N,XLO,XWID,XRAN)
      YSAVE(I) = XRAN
  200 CONTINUE
C         END OF GENERATION.   PRINT TIMING
      CALL CTIME(CT2,RT2)
      RT=RT2-RT1
      PRINT 1004,NTHROW,N,RT,YSAVE
 1004 FORMAT (///  22H0++   HISRAN GENERATEDI8,  13H NUMBERS OVERI6,   8
     1H BINS IN    F10.2,   9H SECONDS.//50X,4HXRAN//10(5X,10F10.5/))
      END
*EXECUTE
*
*
      PROGRAM V151
      DIMENSION FSPACE(100),X(10),Y(10)
      EXTERNAL FUNC
      COMMON/FUNINT/TFTOT
      XLOW=-2.
      XHIGH=2.
      CALL FUNPRE(FUNC,FSPACE,XLOW,XHIGH)
      DO 1 I=1,10
      CALL FUNRAN(FSPACE,XRAN)
      X(I)=XRAN
      Y(I)=TFTOT
 1    CONTINUE
      PRINT 2,X
      PRINT 3,Y
 2    FORMAT(/1X,'XRAN=',10F11.3)
 3    FORMAT(/1X,'TFTOT=',10F11.3)
      STOP
      END
      FUNCTION FUNC(X)
      FUNC=(1./SQRT(2.*3.14))*EXP(-(X**2)/2.)
      RETURN
      END
*CALL PTIME
*EXECUTE
*
*
      PROGRAM V200
      N=10.0
      DO 2 M=1,10
      I=KBINOM(N,M)
      PRINT 3,N,M,I
  3   FORMAT(2F10.2)
  2   CONTINUE
      END
*EXECUTE
*
*
        PROGRAM V201
         DIMENSION IA(7)
         IA(1)=0
       IR=3
      IN=6
 1     CALL COMBI(IA,IN,IR)
      IF(IA(1)) 2,3,2
 2    PRINT 10,(IA(K),K=1,IR)
 10   FORMAT(1HB3I5)
      GO TO 1
 3    CONTINUE
      END
*EXECUTE
*
*
      PROGRAM V202
      DIMENSION IA(3)
      N=3
      IA(1)=0
 1    CALL PERMU(IA,N)
      IF(IA(1))2,3,2
 2    PRINT 10(IA(K),K=1,N)
 10   FORMAT(1HB3I5)
      GO TO 1
 3    CONTINUE
      END
*EXECUTE
*
*
      PROGRAM V300
      DIMENSION X(7)
      INTEGER X
      X(1)=1
      X(2)=1
      X(3)=1
      X(4)=1
      X(5)=1
      X(6)=1
      X(7)=1
      PRINT 4,X
      CALL UBLANK(X,3,5)
      PRINT 1,X
      CALL UZERO(X,3,6)
      PRINT 2,X
      CALL UFILL(X,2,5,2)
      PRINT 3,X
  1   FORMAT(20X,14HRESULT  UBLANK///10X,2I5,3A5,2I5///)
  2   FORMAT(20X,13HRESULT  UZERO///10X,7I5///)
  3   FORMAT(20X,13HRESULT  UFILL///10X,7I5///)
  4   FORMAT(1H1,20X,8HARRAY  X///10X,7I5///)
      END
*EXECUTE
*
*
      PROGRAM V301
      DIMENSION A(15),B(15),C(15)
      EQUIVALENCE (A(5),B(1))
      PRINT 100
      DO 10 I=1,15
   10 A(I)=I
      CALL UCOPYN(A,C,10)
      PRINT 11
      PRINT 12,(A(I),C(I),I=1,10)
      CALL UCOPIV(A,C,10)
      PRINT 14
      PRINT 12,(A(I),C(I),I=1,10)
      CALL UCOPY2(A,B,10)
      PRINT 15
      PRINT 12,(A(I),B(I),I=1,10)
      CALL UCOPY(B,A,10) $ PRINT 16
      PRINT 12,(A(I),B(I),I=1,10)
  100 FORMAT(1H1)
  11  FORMAT(///10X,'UCOPYN'//5X'A(1-10)'10X'B(1-10)'/)
   12 FORMAT(3X,F12.1,F12.1)
   14 FORMAT(///10X,'UCOPIV'//5X'A(1-10)'10X'C(1-10)'/)
   15 FORMAT(///10X,'UCOPY2'//5X'A(1-10)'10X'B(1-10)'/)
   16 FORMAT(///10X,'UCOPY'//5X'A(1-10)'10X'B(1-10)'/)
      END
*EXECUTE
*
*
      PROGRAM V302
      INTEGER A,B
      DIMENSION A(20),B(40)
      DATA(A=1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
      PRINT 10
   10 FORMAT (X,'DATA')
      PRINT 20,A
   20 FORMAT (10X,2HA=,20I3)
      CALL UCOCOP(A,B,5,4,4,6)
      PRINT 30
   30 FORMAT (X,'UCOCOP')
      PRINT 40,B
   40 FORMAT (10X,2HB=,40I3)
      DO 1 I=1,40
    1 B(I)=0
      CALL UCOCOP (A,B,2,5,10,5)
      PRINT 40,B
      DO 2 I=1,40
    2 B(I)=0
      CALL UCOCOP(A,B,10,1,2,1)
      PRINT 40,B
      DO 3 I=1,40
    3 B(I)=0
      CALL UCOCOP(A,B,4,3,5,3)
      PRINT 40,B
      DO 4 I=1,40
    4 B(I)=0
      PRINT 50
   50 FORMAT (X,'UDICOP')
      CALL UDICOP(A,B,4,4,5,7)
      PRINT 40,B
      DO 5 I=1,40
    5 B(I)=0
      CALL UDICOP(A,B,5,4,4,7)
      PRINT 40,B
      DO 6 I=1,40
    6 B(I)=0
      CALL UDICOP(A,B,10,2,2,4)
      PRINT 40,B
      DO 7 I=1,40
    7 B(I)=0
      CALL UDICOP(A,B,2,1,10,3)
      PRINT 40,B
      DO 8 I=1,40
    8 B(I)=0
      CALL UDICOP(A,B,5,2,4,2)
      PRINT 40,B
      END
*EXECUTE
*
*
      PROGRAM V303
      DIMENSION A(10),B(10)
      DATA ((A(I),I=1,10)=1.,2.,3.,4.,5.,6.,7.,8.,9.,10.)
      DATA ((B(J),J=1,10)=21.,22.,23.,24.,25.,26.,27.,28.,29.,30.)
      PRINT 1, A,B
  1   FORMAT (10X,2HA=,10(F5.1,3X)/10X,2HB=,10(F5.1,3X))
      CALL USWOP (A,B,10)
      PRINT 1, A,B,
      END
*EXECUTE
*
*
      PROGRAM V304A
      DIMENSION M(10)
      DATA (M=1,3,7,5,8,4,10,9,2,6)
      K=IUCOMP(5,M,10)
      PRINT 10,K
  10  FORMAT(10X,2HK=,I2)
      J1=IUHUNT (10,M,10,2)
      J2=IUHUNT(9,M,10,2)
      PRINT 20,J1,J2
   20 FORMAT(10X,3HJ1=,I2,5X,3HJ2=,I2)
      END
*EXECUTE
*
*
      PROGRAM V304
      DIMENSION IVEC (20)
      DO 10 I=1,15
      IVEC(I)=I+3
      PRINT 11, I, IVEC(I)
   10 CONTINUE
      J=IUCOMP(6,IVEC,10)
      PRINT 12,J
      J=IUCOMP(121.5,IVEC,5)
      PRINT 12,J
      J=IUFIND(6,IVEC,2,10)
      PRINT 14,J
      J=IUFIND(121.5,IVEC,5,7)
      PRINT 14,J
   12 FORMAT(///3X,'IUCOMP'10X,I5)
   14 FORMAT(///3X,'IUFIND' 10X,I5)
   11 FORMAT(3X,      3X,5HIVEC(,I2,2H)=,I3)
      END
*EXECUTE
*
*
      PROGRAM V305
      DIMENSION IA1(5)
      DATA IA1/2,2,2,2,2/
C--                TEST FOR IULAST
      PRINT 50
      N=5
      ITEXT=2
      K1=1
    9 IPOS=IULAST(ITEXT,IA1,N)
      PRINT 60,(IA1(J),J=1,5),IPOS
      IF (K1.EQ.0) GO TO 10
      IA1(2)=3
      IA1(3)=3
      K1=K1-1
      GO TO 9
   10 CONTINUE
  50  FORMAT(//30X'TEST FOR IULAST'//)
   60 FORMAT (20X,5I5/ 20X,7HIULAST=,I5)
      END
*EXECUTE
*
*
      PROGRAM V306
      DIMENSION A(15),B(15)
      EQUIVALENCE (A(5),B(1))
      DO 10 I=1,15
   10 A(I)=I
      PRINT 11
      CALL MOVE(A,B,-10)
      PRINT 12,(A(I),B(I),I=1,10)
      CALL MOVE(B,A,10)
      PRINT 11
      PRINT 12,(A(I),B(I),I=1,10)
   11 FORMAT(1H1,10X,2H A,15X,2H B/)
   12 FORMAT(3X,F12.1,F12.1)
      END
*EXECUTE
*
      PROGRAM W100
C        TEST  FOR  ZERO
      DIMENSION A(3),B(2),C(3)
      C(1)=1.0
      C(2)=0.5
      C(3)=1.5
      CALL ZERO (A(1),B(2))
      PRINT 2,A,B,C
  2   FORMAT (4F7.2)
      END
*
      PROGRAM W100
C        CARDS  OF  DATAS  110
      CALL TRAMP1
      STOP
      END
*EXECUTE
 31.654  41
  1.     27.015  14.753   0.
  3.      2.092 698.1671  1.
  1.      2.      2.      0.
  3.      1.091 698.1671  1.
  1.      1.6     1.6     0.
  2.      1.092 615.2359  2.
  1.      2.      2.      0.
  2.      2.093 615.2359  2.
  1.      3.55    3.55    0.
  6.      3.28    2.2918  1.1459
  1.     21.2    21.2     0.
  1.      1.      1.      0.
  1.      1.      1.      0.
  1.      1.8     1.8     0.
  2.      2.093 631.4999  0.
  1.     12.5    12.5     0.
  6.      6.2     4.5837  1.1459
  1.      6.4     6.4     0.
  2.      2.091 844.8165  1.
  1.      2.5     2.5     0.
  3.      2.092 770.2401  2.
  1.     14.9    14.9     0.
  3.      2.092 769.9656  1.
  1.      2.5     2.5     0.
  2.      2.091 897.2723  2.
  1.      3.7     3.7     0.
  6.      4.14    3.094   1.547
  1.     14.4    14.4     0.
  1.      1.      1.      0.
  1.      2.      2.      0.
  2.      2.05  855.      0.
  1.      8.      8.      0.
  6.      4.14    3.094   1.547
  1.      4.8     4.8     0.
  6.      3.28    2.2918  1.1459
  1.      3.6     3.6     0.
  2.      2.05  1144.     1.
  1.      2.4     2.4     0.
  3.      2.05  1022.979  2.
  1.     29.     29.      0.
  1.      1.      1.      0.
  1 13  1 12  1 13
  2
  2
 13 21 13 21 14 21
  4
  4
 22 31 22 29 22 31
  1
  3
 30 41 30 41 32 41
  2
  1
502
  0
  1  0.      0.126
  2  0.      0.51
  3  0.1     0.126   0.
  4  0.075   0.51    0.
 10 31.337
  0
  1  0.      0.126
  2  0.      0.51
 50 25.0
 15
  1 13  1 12  1 13
  2
  2
 13 21 13 21 14 21
  4
  4
 22 31 22 29 22 31
  1
  3
 30 41 30 41 32 41
  2
  1
502
  0
  1  0.      1.26
888
   .31654     1    2
  0
  1   0.      1.26
  2  0.      5.1
  0
  0
  1   0.      1.26
  2  0.      5.1
  0
  0
  1   0.      1.26
  2  0.      5.1
  0
888
   .31654     1    2
  0
999
  1  3.0    -1.26    1.26    3.0
  0
  0
999
  1  3.0    -1.26    1.26    3.0
  0
  0
999
  1  3.0    -1.26    1.26    3.0
  0
 20
*
*
      PROGRAM W101
C        CARDS  OF  DATAS  18
      CALL IPSOFA
      STOP
      END
*EXECUTE
    5   14   12
    1.2       2.5       10.0      0.0       10.0      0.0
    0.0
  1 0.0    0.0     0.5     -0.5    0.25    -0.25
  1 0.4    0.4     100.0    0.0    0.5     -0.5
  1 0.6    0.6     100.0   0.0     0.5     -0.5
  1 0.5    0.5     4.5     -4.5    2.0     -2.0
  1 0.9585 0.9585  8.5     -8.5    100.0   0.0
  2 0.483  0.54219 8.5     -8.5    100.0   0.0
  1 0.4285 0.4285  100.0   0.0     6.35    -6.35
  5 1.66   -12.62  100.0   0.0     6.35    -6.35   -261.8
  1 0.4285 0.4285  100.0   0.0     8.5     -8.5
  3 0.483  0.47108 100.0   0.0     8.5     -8.5
  1 2.1585 2.1585  100.0   0.0     0.8     -0.8
  1 0.3    0.3     100.0   0.0     0.8     -0.8
  1 0.3    0.3     100.0   0.0     0.8     -0.8
  1 0.3    0.3     5.0     -5.0    2.0     -2.0
 -1
*
*
      PROGRAM W104
C   USER  MUST  REQUIRE  LIBRARY 1  AND  LIBRARY 2
C        CARDS  OF  DATAS  122
      CALL TRAJ
      STOP
      END
*EXECUTE
PPS   P     SST   SSE   S     Y     A     DY    DYA   DP    DZ    DZA
VARY  PFT   ALO   LENS  *     NFT   SSM   SM    LABEL OUTR  OUTB  EX
TRAJ  HMAT  VMAT  SIZE  T     E     NEWPG NT1   NT2   NT3   NT4   NT5
NT6   UPDATEEND   B
00.29979300.0142696170.079     0.56     0.124
0.0500000000000000
 2.1976    2.2056    0.2484    2.5032    1.1032    0.0716
00001
010000024.900000.02000000   18  101
(12F6.4)
0.000        40   22
0.01290.01500.01660.01810.01970.02060.02170.02250.02300.02360.02410.0241
0.02360.02330.02300.02250.02170.02070.01920.01760.01550.0135
0.040        36   30
0.01450.01840.02230.02610.02980.03340.03650.03910.04140.04350.04500.0465
0.04790.04920.05000.05000.04950.04920.04870.04750.04530.04350.04060.0375
0.03390.03000.02590.02170.01760.0137
0.080        33   35
0.01270.01760.02360.02980.03650.04320.05000.05590.06110.06630.07070.0743
0.07740.07970.08230.08440.08650.08750.08750.08700.08650.08540.08340.0805
0.07720.07280.06810.06240.05620.04890.04190.03470.02800.02150.0161
0.120        32   38
0.01660.02380.03180.04120.05050.06080.07070.08050.08930.09730.10430.1108
0.11600.12060.12430.12840.13120.13380.13490.13490.13440.13360.13200.1289
0.12500.12040.11420.10720.09890.08980.07950.06890.05800.04760.03750.0287
0.02100.0148
0.160        31   40
0.01890.02720.03750.04920.06190.07480.08850.10170.11500.12630.13770.1473
0.15660.16410.17090.17630.18170.18590.18920.19110.19160.19120.19000.1877
0.18430.17890.17240.16360.15380.14190.12950.11520.10050.08570.07150.0575
0.04450.03310.02380.0163
0.200        30   42
0.01810.02740.03860.05200.06680.08310.09920.11650.13310.15020.16570.1807
0.19440.20740.21880.22940.23820.24620.25220.25730.26120.26330.26360.2628
0.25940.25500.24780.23820.22580.21100.19370.17550.15530.13570.11550.0968
0.07840.06160.04690.03420.02380.0163
0.240        29   44
0.01550.02410.03550.04890.06520.08280.10230.12140.14240.16310.18490.2053
0.22600.24620.26640.28560.30440.32080.33580.34820.35830.36580.37300.3800
0.37700.37300.36660.35550.33940.31720.29150.26220.23250.20170.17370.1465
0.12300.09970.07870.06010.04450.03160.02170.0145
0.280        29   44
0.01940.02930.04220.05750.07610.09580.11780.14060.16520.19000.21720.2444
0.27340.30370.33710.37230.40910.44450.47790.50930.53620.55790.59000.6050
0.61500.61000.60010.58150.54390.48850.42590.36380.30760.25790.21590.1789
0.14780.11880.09400.07170.05330.03830.02690.0181
0.320        28   46
0.01400.02230.03290.04740.06420.08410.10610.13020.15530.18330.21280.2449
0.27910.31740.36090.41170.46910.53390.60140.67130.73970.80650.86990.9500
1.03001.11001.11601.11151.09000.98100.80160.62990.49630.39510.31740.2573
0.20890.17010.13590.10690.08180.06110.04430.03130.02150.0145
0.360        28   47
0.01550.02430.03570.05100.06860.08980.11310.13900.16650.19730.22960.2661
0.30580.35080.40350.46600.53900.62030.70710.79690.88600.97401.06201.1500
1.22501.30001.37001.44001.43691.28341.02210.77670.59390.46110.36350.2905
0.23250.18720.14890.11650.08930.06710.04870.03470.02410.01660.0111
0.400        27   48
0.01060.01680.02590.03780.05310.07170.09370.11810.14500.17420.20660.2418
0.28090.32340.37230.42820.49540.57240.65790.74820.84120.93231.02371.1133
1.20001.28001.36001.44001.52031.52391.36441.09050.83550.63900.49840.3915
0.31170.24910.20010.15820.12430.09500.07150.05200.03750.02640.01860.0127
0.440        27   48
0.01140.01760.02670.03880.05460.07380.09630.12120.14910.17940.21310.2501
0.29150.33610.38580.44300.50980.58800.67470.76560.85920.95301.04491.1368
1.22251.30751.39251.47751.55751.56221.40061.12360.86290.66490.51960.4083
0.32490.25940.20790.16490.12950.09920.07430.05460.03960.02850.02020.0142
0.480        27   49
0.01220.01860.02770.03990.05590.07510.09790.12350.15170.18280.21750.2553
0.29750.34300.39350.45100.51830.59650.68400.77620.87150.96571.05941.1524
1.24001.32601.41301.50001.58141.58581.42321.14280.87920.67830.53070.4189
0.33290.26640.21390.16960.13280.10200.07690.05670.04140.02980.02150.0155
0.0110
0.520        27   49
0.01270.01920.02850.04090.05720.07660.09920.12500.15380.18540.22030.2584
0.30110.34670.39740.45540.52320.60320.69050.78320.87920.97401.06851.1625
1.24001.32601.43801.51601.59841.60311.43661.15420.88850.68710.53720.4241
0.33810.27080.21670.17200.13540.10410.07900.05830.04300.03110.02250.0163
0.0115
0.5600       26   50
0.00830.01320.01990.02930.04190.05800.07770.10050.12630.15530.18690.2221
0.25990.30190.34710.39850.45750.52710.60710.69490.78860.88410.97991.0750
1.16951.25901.34801.43701.52601.61041.61451.44831.16120.89320.68890.5398
0.42610.34070.27340.21900.17400.13720.10590.08050.05980.04400.03210.0233
0.01680.0120
0.6000000000026   51
0.00880.01350.02050.03000.04250.05880.07840.10120.12710.15610.18770.2221
0.25990.30190.34720.39920.45900.52890.60890.69830.79220.88800.98381.0801
1.17511.26501.35501.44301.52701.61791.62171.45191.16660.89680.69000.5403
0.42590.34150.27440.22030.17550.13830.10720.08160.06080.04480.03260.0238
0.01710.01200.0088
0.810000     22   57
0.00300.00490.00760.01130.01640.01960.02350.03320.04610.06240.08240.1058
0.13180.16100.19310.22850.26680.30900.35600.40920.47150.54360.62470.7158
0.81090.90911.00501.10101.19711.29101.38501.47841.56991.64801.65431.4945
1.20720.93180.71850.56190.44540.35650.28650.23070.18430.14620.11390.0874
0.06610.04950.03690.02750.02050.01540.01150.00870.0065
2.60000070   22   57
0.00300.00490.00760.01130.01640.01960.02350.03320.04610.06240.08240.1058
0.13180.16100.19310.22850.26680.30900.35600.40920.47150.54360.62470.7158
0.81090.90911.00501.10101.19711.29101.38501.47841.56991.64801.65431.4945
1.20720.93180.71850.56190.44540.35650.28650.23070.18430.14620.11390.0874
0.06610.04950.03690.02750.02050.01540.01150.00870.0065
*)   HYPOTHETICAL RUN TO ILLUSTRATE THE PROGRAM
NFT=1
PPS=19.2
NEWPG=1
SST= 60
S  = -0.7114
LABEL=TEST 1
SSE= 61,62,    A= 0.04,0.02
P= -7.0
Y=  0.0
OUTB/TRAJ(EX)SIZE(5) /
DY = 1.0 ,DYA = 0.5,DP = 2.0 , DZ = 1.0 , DZA = 0.1
VARY/(SSE, A)/
LABEL=TEST 2
SSE=61.0
P= -4.0(-1.0)-6.0, -8.0
A=0.04
Y=0.0
OUTB/ TRAJ(-10.0,EX)HMAT(1.0,T,E)VMAT(2.0,3.0)SIZE(4.0) /
VARY/ P /
*
*
      PROGRAM W105
C        CARDS  OF  DATAS  11
      CALL SCOPT2
      STOP
      END
*EXECUTE
1 17.0    4
 117.1   17.2   17.5   17.6   18.0   18.1   18.2   17.9   17.8   17.5
 217.8   17.9   18.3   18.4   18.8   18.9   19.3   19.4   19.3   18.9
 318.9   19.0   19.5   19.6   19.6   19.6   19.5   19.0   18.9   18.8
 418.8   18.7   18.75  18.7   18.65  18.6   18.5   18.4   18.2   18.1
224.0400.010.0 1.0   200   0
 400.0     50.0      3
0.087265  0.017453  10
 2 1
 4 1
 5 1
*
*
      PROGRAM W126
C        CARDS  OF  DATAS  21
      CALL ELENS
      STOP
      END
*EXECUTE
3
     2.0            2.0                          1 1234
                                               100
                                                 8
     0.0            0.0           30.0         101
    25.0           55.0           30.0
    30.0           55.0           60.0
    40.0           30.0           60.0
    80.0           30.0           60.0
    95.0           55.0           60.0
   100.0           55.0           80.0
   100.0            0.0           80.0         101
     0.0            0.5            0.0
    1
 1234    4        20.0
     0.25           4.0
    0.01          1.0
     28.0           -0.2
    20.0           -0.1
    10.0          0.2
    1.0           0.5
*
*
      PROGRAM W127
C        CARDS  OF  DATAS  9
      CALL RAYTRA
      STOP
      END
*EXECUTE
    0.01           1.0            4 1234
    20.0                          1    1
    28.0           -0.2
    20.0           -0.1
    10.0           0.2
    1.0            0.5
    100.0          2.0            1    1
                   30.0
                   0.5
*
*
      PROGRAM W300
C   USER  MUST  REQUIRE  LIBRARY 1  AND  LIBRARY 2
C        CARDS  OF  DATAS  4
      CALL RELKIN
      STOP
      END
*EXECUTE
139.59    +938.213  135.0     +939.507  0.        0.        0.        0.
0.        150.      10.       0.        0.        0.        0.        0.
139.59    938.213   135.0     939.507   0.        200.      -50.      0.
0.        0.        20.       45.       135.      1.        0.        0.
*
*
      PROGRAM W303
C        CARDS  OF  DATAS  10
      CALL MISMAS
      STOP
      END
*EXECUTE
.1396  .9383  1.25   .938   20.0   19.6   1.0    .1     .14    .94    75
.1396  .9383  1.25   .938   20.0   19.6   1.0    .1     .14    .94    75
.1396  .9383  1.25   .938   20.0   19.6   1.0    .1     .14    .94    75
.1396  .9383  1.25   .938   20.0   19.6   1.0    .1     .14    .94    75
.1396  .9383  1.25   .938   20.0   19.6   1.0    .1     .14    .94    75
.1396  .9383  1.25   .938   20.0   19.6   1.0    .1     .14    .94    75
.1396  .9383  1.25   .938   20.0   19.6   1.0    .1     .14    .94    75
.1396  .9383  1.25   .938   20.0   19.6   1.0    .1     .14    .94    75
.1396  .9383  1.25   .938   20.0   19.6   1.0    .1     .14    .94    75
.1396  .9383  1.25   .938   20.0   19.6   1.0    .1     .14    .94    75
*
*
      PROGRAM W304
C        CARDS  OF  DATAS  6
      CALL RELKMC
      STOP
      END
*EXECUTE
341GEV
0.13963   0.93821   1.87549   0.93821
0.1       4.0       0.5       8.0       1.0       15.0
1.        20.       2.        70.       5.        180.
2.        180.
PI MINUS    PROTON      DEUTERON    ANTIPROTON
*
*
      PROGRAM W307
C        CARDS  OF  DATAS  7
      CALL STU
      STOP
      END
*EXECUTE
0.5       0.75      0.25      1.0                                   M1 M
180.      A3LAB 20.       EQUID                            P1LAB TYPE FN
0.0       1.0
                                                                    CONT
180.      A3CM  20.       EQUID                            P1LAB TYPE FN
0.0       1.0
STOP                                                                  DA
*
*
      PROGRAM W500
C        CARDS  OF  DATAS  6
      CALL SLY
      STOP
      END
*EXECUTE
  3  3 50
     2230.        0.     1115.     1115.        1.
      549.      549.      549.
  4  4 50
          5000.     140.      938.      100.
495.      140.      1115.     135.
*
*
      PROGRAM W501
C   USER  MUST  REQUIRE  LIBRARY 1  AND  LIBRARY 2
C        CARDS  OF  DATAS  20
      CALL ATHOS
      STOP
      END
*EXECUTE
1.610     0.938     0.938     0.496     0.496     0.138     222249222132
0.1       0.2       0.3       0.4       0.5
0.0       0.02      0.04      0.06      0.08      0.10      0.12
0.14      0.16      0.18      0.20      0.22      0.24      0.26
0.28      0.30      0.32      0.34      0.36      0.38      0.40
0.42      0.44      0.46      0.48      0.50      0.52      0.54
0.56      0.58      0.60      0.62      0.64      0.66      0.68
0.70      0.72      0.74      0.76      0.78      0.80      0.82
0.84      0.86      0.88      0.90      0.92      0.94      0.96
22223     1.05      0.10
1.80      0.938     0.938     0.496     0.496     0.138     222249222134
0.15      0.25      0.35      0.45      0.55
0.0       0.02      0.04      0.06      0.08      0.10      0.12
0.14      0.16      0.18      0.20      0.22      0.24      0.26
0.28      0.30      0.32      0.34      0.36      0.38      0.40
0.42      0.44      0.46      0.48      0.50      0.52      0.54
0.56      0.58      0.60      0.62      0.64      0.66      0.68
0.70      0.72      0.74      0.76      0.78      0.80      0.82
0.84      0.86      0.88      0.90      0.92      0.94      0.96
22223     1.05      0.10
*
*
      PROGRAM W505
C   USER  MUST  REQUIRE  LIBRARY 1  AND  LIBRARY 2
C        CARDS  OF  DATAS    17
      CALL FOWL
      STOP
      END
      SUBROUTINE USER
C-----------------  EXAMPLE  2  ----------------------------------------
      COMMON/UTIL/PLACE(150),WEIGHT(150),NP,TECM,AMASS(18),PCM(5,18),WT
      COMMON/FDUMP/QQ1,QQ2,TDM,DUMMY(47)
      QQ1 = FMASS(1,2,0,0,0,0)
      QQ2 = FMASS(3,2,0,0,0,0)
      I1 = 0
      I2 = 0
      IF(QQ1 .GT. 1.200 .AND. QQ1 .LT. 1.300) I1 = 1
      IF(QQ2 .GT. 1.200 .AND. QQ2 .LT. 1.300) I2 = 1
      IF(I1 + I2 .NE. 1) GO TO 200
      IF(I1 .EQ. 1) GO TO 50
      PLACE(6) = FMASS(1,4,0,0,0,0)
      TDM = DELSQ(3,2,0,0,0,0)
      GO TO 100
   50 PLACE(6) = FMASS(3,4,0,0,0,0)
      TDM = DELSQ(1,2,0,0,0,0)
  100 CONTINUE
      IBANDE = TDM/0.1 + 1.0
      IF(IBANDE .GT. 5) IBANDE = 5
      PLACE(IBANDE) = PLACE(6)
      RETURN
  200 WT = 0.0
      RETURN
      END
*EXECUTE
     10000      5000         5
 0.139     0.9382    2.7500
         4  0.1396   0.9382    0.1396    0.1396
         1   0.200    1.300    0.020
 MASS PIPI WITH ISOBAR   DELSQ BETWEEN 0.0 AND 0.1
         2  0.200    1.500     0.020
 MASS PIPI WITH ISOBAR   DELSQ BETWEEN 0.1 AND 0.2
         3
 MASS PIPI WITH ISOBAR   DELSQ BETWEEN 0.2 AND 0.3
         4
 MASS PIPI WITH ISOBAR   DELSQ BETWEEN 0.3 AND 0.4
         5
 MASS PIPI WITH ISOBAR   DELSQ ABOVE 0.4
         6
 MASS PIPI WITH ISOBAR       ALL DELSQ
SCAT     1         6         6
         0
*
*
      PROGRAM W520
           COMPLEX DE
      COMMON /OST/ DE(2,2,5) , ET(2,2,5)
      DIMENSION S(6) , TMRY(2,2,5)
      DO  1  I=1,2
      DO 1 J=1,2
      READ 15 , ( TMRY(I,J,K) , K=1,5 )
    1 CONTINUE
   15 FORMAT  ( 5F16.2 )
      DO  2  I=1,2
      DO  2  J=1,2
      PRINT 16 , ( TMRY(I,J,K) , K=1,5 )
    2 CONTINUE
   16 FORMAT  ( 1H0,10X,5F16.2 )
      DO  20  I=1,2
      DO  20  J=1,2
      DO  20  K=1,5
      DE(I,J,K) = TMRY(I,J,K)*(0.,2.)/57.296
      ET(I,J,K) = 1.0
   20 CONTINUE
      ET(1,1,2) = 0.75
      ET(1,1,3) = 0.98
      P = 2.417
      PRINT  25
      DO  10  J=1,19
      TETA = 10.*(J-1)
      T = COS (TETA/57.296 )
      CALL  PSCS( P, T, S )
      PRINT 30 , TETA , T , ( S(K) , K=1,5 )
   10 CONTINUE
   25 FORMAT(1H0,10X,4HTETA,5X,8HCOS TETA,2X,8HNEGATIVE,4X,8HPOSITIVE,
     ,3X,7HNEUTRAL ,2X,9HNEG TOTAL,4X,9HPOS TOTAL)
   30 FORMAT ( 1H0,10X,F5.1 , 5X , F7.4 , 5( 2X , F9.3 ) )
      END
*EXECUTE
             0.0            34.3             7.2             1.5
            16.4            -5.1             1.2             0.1
             0.0           -11.6             0.2             0.0
           -24.4           144.8            -1.3             0.8
*
*
      PROGRAM W521
      DATA PIM/0.1396/ , RAD/57.296/
      PRINT 5
      DO  10  J=1,21
      W = 1.1 + 0.02*(J-1)
      S = W*W
      Q2 = 0.25*( S - 1.1623 )*( S - 0.6379 )/S
      A = SQRT(Q2)/PIM
      B = W/PIM
      CALL ROSEN(A,B,C)
      P33 = C*RAD
      PRINT 15 ,   W , P33
   10 CONTINUE
    5 FORMAT (1H1,40X,16HTOTAL CMS ENERGY,14X,15HP33 PHASE SHIFT)
  15  FORMAT(1H0,40X,F10.3,20X,F10.1)
      END
*EXECUTE
*
*
      PROGRAM W601
      DIMENSION W(1000)
      DIMENSION F1(1000),TITLE1(20),TITLE2(20)
      DATA(TITLE1=18HHISTOGRAM FOR IRON)
      DATA(TITLE2=20HHISTOGRAM FOR COPPER)
      DO 30 I=1,1000
      W(I)=1.0
  30   CONTINUE
C FOR IRON ELECTRON
      Z=26.0     $  AV=55.847
      RHA=7.9   $  TM=0.04
      EN=184.0   $  PMASS=0.511006
      ZE=1.0
   1  DO 10 I=1,1000
      CALL MLR(Z,AV,RHA,TM,EN,PMASS,ZE,FI)
      F1(I)=FI
  10  CONTINUE
      PRINT 20,(F1(I),I=1,1000)
  20  FORMAT (5X,10(E12.4))
      IF(Z.EQ.29.0) GO TO 11
       HLIMIT=0.1   $  SLIMIT=3.0E-05
       RESOL=0.001
       CALL WHISTO(F1,HLIMIT,SLIMIT,RESOL,1000,W,TITLE1)
      GO TO 111
  11  CONTINUE
       HLIMIT=0.05  $  SLIMIT=0.00008
      RESOL=0.0005
       CALL WHISTO(F1,HLIMIT,SLIMIT,RESOL,1000,W,TITLE2)
      GO TO 2
  111 CONTINUE
C  FOR COPPER ELECTRON
      Z=29.0   $  AV=63.549
      RHA=8.96   $  TM=0.005
      EN=184.0  $  PMASS=0.511006
      ZE=1.0
      IF(Z.EQ.29.0) GO TO 1
  2   STOP
      END
*EXECUTE
*
*
      PROGRAM W700
C     **************
C
C     ***  CALCULATION OF NORMALIZED ISOSPIN COEFFICIENTS  ***
C
      DIMENSION M(7),ST3(20,20),WEIGHT(20)
      READ 10,TJ1,TM1,TJ2,TM2,DAT1,DAT2
 1000 READ 20,M
      NSP=0
      DO 1 I=1,7
    1 NSP=NSP+M(I)
      IF(NSP.LT.2) GO TO 99
      PRINT 100,DAT1,DAT2,TJ1,TM1,TJ2,TM2,M
      CALL NISCO (TJ1,TM1,TJ2,TM2,M,IZP,ST3,WEIGHT)
      PRINT 200,IZP
      DO 2 I=1,IZP
    2 PRINT 300,I,WEIGHT(I),(ST3(I,K),K=1,NSP)
      PRINT 400
      GO TO 1000
   99 PRINT 500
      STOP
   10 FORMAT (4F10.0/2A6)
   20 FORMAT (7I5)
  100 FORMAT (1H1 ///10X,54HRESULTS OF NORMALIZED ISOSPIN COEFFICIENTS C
     1ALCULATION/10X,54(1H*)//10X,2A6/////10X,24HBEAM   PARTICLE HAS  T
     2=,F4.1,12H   AND  T3 =,F5.1//10X,24HTARGET PARTICLE HAS  T =,F4.1,
     312H   AND  T3 =,F5.1///10X,6HM = ( ,7I2,3H  )/////)
  200 FORMAT (10X,12HNISCO OUTPUT///10X,36HNUMBER OF CHARGE CONFIGURATIO
     1NS IS  ,I3////10X,7HCONFIG.,4X,5HNISCO,5X,30HVALUES OF  T3  FOR SE
     2CONDARIES)
  300 FORMAT(/I14,F13.5,2X,15F6.1)
  400 FORMAT (///40X,40(1H-))
  500 FORMAT (/////10X,17HCALNIC NORMAL END/////)
      END
*EXECUTE
1.        -1.       0.5       0.5
 22.10.1970
    1    0    0    0    2    0    0
    1    0    0    0    3    0    0
    1    0    0    0    4    0    0
    0    0    0    0    0    0    0
*
*
      PROGRAM X202
      DIMENSION XX(4),Y(4)
      DATA Y/.13501,74.,43.,23./
      PRINT 5
      DO 2 I=1,21
      X=VOMAS(I)
      IX=IOFMAS(X)
 2    PRINT 1,IX,X
      DO 4 I=1,4
 4    XX(I)=VOMAS(Y(I))
      PRINT 3,Y,XX
 1    FORMAT(5X,I2, 5X,E15.6)
 3    FORMAT(/5X,'Y =',4F10.3/5X,'XX=',4F10.3)
 5    FORMAT(/5X,'IX',12X,'X'//)
      STOP
      END
*CALL PTIME
*EXECUTE
*
*
      PROGRAM X203
      DATA A/3HNEX/
      DO 3 I=1,50
      PNAME=PAROFI(I)
      KOD=IOFPAR(PNAME)
      PRINT 2,KOD,PNAME
 3    CONTINUE
      PNAME=A
      KOD=IOFPAR(PNAME)
      PRINT 2,KOD,PNAME
 2    FORMAT(5X,I2,10X,A6)
      STOP
      END
*CALL PTIME
*EXECUTE
*
*
      PROGRAM X401
      DIMENSION PINV(10),EM(5),DRR(10)
      DATA PINV/.1,.2,.3,.4,.5,.6,.7,.8,.9,1./,
     *     EM/.938213,.13958,.000511,.4938,1.189/
      PRINT 5
      DO 2 J=1,5
      DO 1 I=1,10
      DRR(I)=STRAG(PINV(I),EM(J))
  1   CONTINUE
      PRINT 10,EM(J),DRR
  2   CONTINUE
  5   FORMAT(50X,9HTEST X401//)
  10  FORMAT(10X,3HEM=,F10.6//2(10X,5E20.11/))
      STOP
      END
*EXECUTE
*
*
      PROGRAM X601
      DIMENSION A1(3),A2(3),B(3)
      DIMENSION GA1(3,3),GA2(3,3),GA12(3,3),GB(3,3)
      GA1(1)=0.01*0.01/10.324**4
      GA1(2)=0.01*0.001/10.324**2
      GA1(3)=0.01*0.001/10.324**2
      GA1(4)=0.01*0.001/10.324**2
      GA1(5)=0.001*0.001
      GA1(6)=0.001*0.001
      GA1(7)=0.01*0.001/10.324**2
      GA1(8)=0.001*0.001
      GA1(9)=0.001*0.001
      GA2(1)=0.01*0.01/1.1792**4
      GA2(2)=0.01*0.001/1.1792**2
      GA2(3)=0.01*0.001/1.1792**2
      GA2(4)=0.01*0.001/1.1792**2
      GA2(5)=0.001*0.001
      GA2(6)=0.001*0.001
      GA2(7)=0.01*0.001/1.1792**2
      GA2(8)=0.001*0.001
      GA2(9)=0.001*0.001
      GA12(1)=0.0
      GA12(2)=0.0
      GA12(3)=0.0
      GA12(4)=0.0
      GA12(5)=0.0
      GA12(6)=0.0
      GA12(7)=0.0
      GA12(8)=0.0
      GA12(9)=0.0
      A1(1)=1/10.324
      A1(2)=0.455
      A1(3)=0.0
      A2(1)=1/1.1792
      A2(2)=3.99
      A2(3)=180
      CALL PRIPAR(A1,A2,B)
      PRINT 2,B
  2   FORMAT(3E20.11)
      CALL ERPRIP(GA1,GA2,GA12,GB)
      PRINT 3,GB
  3   FORMAT(3E20.11)
      END
*EXECUTE
*
*
      PROGRAM Y201
      DATA XLOW,DX,NX/1.,12.,10/
      X=0.
      DO 1 I=1,10
      N=IUCHAN(X,XLOW,DX,NX)
      PRINT 2,X,N
 1    X=X+15.
 2    FORMAT(1X,'X=',F17.10,5X,'N=',I4)
      STOP
      END
*CALL PTIME
*EXECUTE
*
*
      PROGRAM Y202
      DIMENSION PAR(3)
      LOGICAL SPILL
      DATA PAR/10,12.,1./
      X=0.
      DO 1 I=1,12
      N=IUBIN(X,PAR,SPILL)
      PRINT 2,X,N,SPILL
 1    X=X+15.
 2    FORMAT(1X,'X=',F17.10,5X,'N=',I4,5X,'SPILL=',L3)
      STOP
      END
*EXECUTE
*
*
      PROGRAM Y203
      DIMENSION PAR(3)
      LOGICAL SPILL
      DATA PAR/10,.08333,1./
      X=0.
      DO 1 I=1,12
      N=IUHIST(X,PAR,SPILL)
      PRINT 2,X,N,SPILL
 1    X=X+15.
 2    FORMAT(1X,'X=',F17.10,5X,'N=',I4,5X,'SPILL=',L3)
      STOP
      END
*CALL PTIME
*EXECUTE
*
*
      PROGRAM Z005
C     TEST OF DATEZB AND IDATZA
      DIMENSION A(2)
      CALL DATEZB(A)
      PRINT 1,A
      CALL IDATZA(I)
      PRINT 2,I
      STOP
  1   FORMAT(////6H DATA=,A6,A3)
  2   FORMAT(//7H IDATA=,I8)
      END
*EXECUTE
*
*
      PROGRAM Z054
      DIMENSION M(100),MM(20)
      PRINT 11
      DO 1 I=1,100
  1   M(I)=I
      CALL SUPLIS
      DO 2 I=1,2
      DO 3 J=1,100
      DO 4 K=1,20
  4   MM(K)=M(J)
  3   PRINT 10,MM
  10  FORMAT(20I6)
      CALL RESLIS
  2   PRINT 11
  11  FORMAT(1H1)
      STOP
      END
*EXECUTE
*
*
      PROGRAM SINCOS
      DIMENSION X(100),Y(100),Z(100)
      X(1) = -1.9
      Y(1) = SIN(-1.9)
      Z(1) = COS(-1.9)*1.5
      DO 5 I=2,90
      X(I) = X(I-1) + .1
      Y(I) = SIN(X(I))
   5  Z(I) = COS(X(I))*1.5
      CALL PAGE(15.,20.,11HPOÉOB - ãHî,11,0)
       CALL LIMITS(-2.,7.,-1.5,1.5)
      CALL REGION(3.,3.,10.,15.,6HREGION,6,1)
      CALL AXES(4HOCúX,4,1.,5,4HOCúY,4,.3,4,0)
      CALL LINEMO(X,Y,85,2,10)
      CALL BROKEN(.5,.2,.3,.2)
      CALL BRLINE(X,Z,85)
      CALL ENDPG(4H0001)
      END
*EXECUTE
*
*
      PROGRAM ROSES
       DIMENSION X(6),Y(6),N(6),A(6),B(6)
      DATA(X=42.,48.,92.,92.,126.,140.)
      DATA(Y=40.,68.,56.,108.,82.,34.)
      DATA(N=16,8,24,16,8,8)
      DATA(A=11.,10.,18.,11.,10.,10.)
      DATA(B=2.,5.,3.,2.,5.,5.)
      CALL MMS
      CALL PAGE(180.,140.,5HROÉOB,5,1)
      DO 1 J=1,6
      M = N(J)
      DO 3 K=1,M
      L=INT(A(J))+1
      DO 2 I=1,L
      CALL BEGLEV
      CALL PSCALE(X(J),Y(J),1.-FLOAT(I-1)/A(J))
      CALL ELIPS(X(J),Y(J),A(J),B(J),0.,-180.,180.)
   2  CALL ENDLEV
   3  CALL ROTATE(X(J),Y(J),360./FLOAT(M))
   1  CONTINUE
      CALL ENDPG(5HPOÉOB)
      END
*EXECUTE
*
