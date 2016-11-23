000001 : *     05/04/85  11.24.03     BECЬ HИЗ ГPAФИKИ - B CИCTEMY
000002 :       SUBROUTINE GETPEN(N)
000003 : C*
000004 : C******  BЫБOP  N-ГO  ПEPA  (N=1,2,...,6)
000005 : C*       HA ГPAФOПOCTPOИTEЛE  WX4675
000006 : C*
000007 :       COMMON /PNUMB/ NPEN,NPOLD
000008 :       IF(N.GT.0.AND.N.LT.7) NPEN=N
000009 :       RETURN
000010 :       END
000011 :       SUBROUTINE PLOTBF(KX,KY,JOB)
000012 : C*
000013 : C******    Г P A Ф O P  -  БYФEPИЗAЦИЯ
000014 : C*
000015 : C*    KOMAHДЫ BИPTYAЛЬHOГO ГPAФПЛOTTEPA
000016 : C*    ЗAПИCЫBAЮTCЯ B CИCTEMHЫЙ ФAЙЛ BЫBOДA
000017 : C*    B BИДE ПOCЛEДOBATEЛЬHOCTИ БAЙTOB.
000018 : C*    ДЛИHA ПOCЛEДOBATEЛЬHOCTИ OПPEДEЛЯETCЯ
000019 : C*    KOДOM KOMAHДЫ, YKAЗAHHЫM B 1-M БAЙTE:
000020 : C*
000021 : C*    0 - ПYCTAЯ KOMAHДA (OДHOБAЙTOBAЯ)
000022 : C*    1 - CMEHA БYMAГИ HA ПЛOTTEPE (1 БAЙT)
000023 : C*    2 - YCTAHOBKA ПEPA B HAЧAЛO  (1 БAЙT)
000024 : C*    3 - CMEHA ПEPA (HOMEP ПEPA = 2-Й БAЙT)
000025 : C*    4 - ДBИЖEHИE БEЗ PИCOBAHИЯ (5 БAЙT) :
000026 : C*        БAЙTЫ 2-3 : X-KOOPДИHATA (ЦEЛOE)
000027 : C*        БAЙTЫ 4-5 : Y-KOOPДИHATA (ЦEЛOE)
000028 : C*    5 - ДBИЖEHИE C PИCOBAHИEM (5 БAЙT)
000029 : C*        AHAЛOГИЧHO KOMAHДE 4.
000030 : C*    6 - MAPKИPOBKA ГPAФИKИ (7 БAЙT) :
000031 : C*        БAЙTЫ 2-7 : ИMЯ OЧEPEДHOЙ ПOPЦИИ.
000032 : C*
000033 :       COMMON /SPLOT/ DX,DY
000034 :       DATA DX/0./, DY/0./
000035 :       COMMON /PLONAM/ NAME
000036 :       DATA NAME/'WX4675'/
000037 :       COMMON /PNUMB/ NPEN,NPOLD
000038 :       DATA NPEN/1/, NPOLD/1/
000039 :       COMMON /MARGRF/ MARK
000040 :       DATA MARK/1/
000041 :       DIMENSION MTEMP(4),MBUF(200)
000042 :       DATA LBUF/200/, NS/0/
000043 :       DATA NZER/0/
000044 :       EQUIVALENCE (KXNEW,MTEMP(2))
000045 :       EQUIVALENCE (KYNEW,MTEMP(4))
000046 : C
000047 : C---   HYЖHA ЛИ MAPKИPOBKA :
000048 : C
000049 :       MSWOP=1
000050 :       IF(MARK.EQ.0B) GOTO 13
000051 :       LET=6
000052 :       DO 10 I=1,7
000053 :       ASSIGN 11 TO MBRET $ GOTO 100
000054 :   11  LET=IGETS(MARK,I-1)
000055 :   10  CONTINUE
000056 :       MARK=0B
000057 : C
000058 : C---    YЧET CMEЩEHИЯ
000059 : C
000060 :   13  KXNEW = KX+INT(100.*DX)
000061 :       KYNEW = KY+INT(100.*DY)
000062 : C
000063 : C---    HE YCTAHOBKA ЛИ HA HAЧAЛO ?
000064 : C       ПO KOHЦY ГPAФИKA ДOCБPOCИM БYФEP
000065 : C       И CФOPMИPYEM HOBЫЙ MAPKEP
000066 : C
000067 :       IF(JOB.NE.0) GOTO 7
000068 :       LET=2 $ NZER=NZER+1 $ MSWOP=NZER.AND.1
000069 :       IF(MSWOP.EQ.0) MARK=1+NZER/2
000070 :       ASSIGN 99 TO MBRET $ GOTO 100
000071 : C
000072 : C---    HE CMEHA ЛИ БYMAГИ
000073 : C
000074 :   7   IF(NAME.NE.'WXHOME') GOTO 1
000075 :       LET=1 $ NAME='WX4675'
000076 :       ASSIGN 1 TO MBRET $ GOTO 100
000077 : C
000078 : C---    HE HAДO ЛИ CMEHИTЬ ПEPO
000079 : C
000080 :   1   IF(NPEN.EQ.NPOLD) GOTO 3
000081 :       NPOLD=NPEN $ LET=3
000082 :       ASSIGN 2 TO MBRET $ GOTO 100
000083 :   2   ASSIGN 3 TO MBRET $ LET=NPEN $ GOTO 100
000084 : C
000085 : C---    OБЫЧHOE ДBИЖEHИE. CЛИB KOOPДИHAT
000086 : C
000087 :   3   LET=4 $ IF(JOB.GT.0) LET=5
000088 :       ASSIGN 4 TO MBRET $ GOTO 100
000089 :   4   MTEMP(1) = KXNEW.SHIFT.8
000090 :       MTEMP(3) = KYNEW.SHIFT.8
000091 :       DO 5 I=1,4
000092 :       LET=MTEMP(I) $ ASSIGN 5 TO MBRET
000093 : C
000094 : C---    ЗAПИCЬ CИMBOЛA B БYФEP
000095 : C
000096 :  100  CALL IPUTS(LET,MBUF,NS) $ NS=NS+1
000097 :       IF(NS.LT.6*LBUF.AND.MSWOP.NE.0) GOTO 102
000098 :       N1 = LOCF(MBUF) $ N2 = N1+LBUF-1
000099 :       N = (N1.SHIFT.-24).OR.(N2.AND.77777B).OR.0030 0000 0000 0000B
000100 :       N = JEXTRI(0,71B,N)
000101 :       DO 101 NS=1,LBUF
000102 :  101  MBUF(NS) = 0B
000103 :       NS = 0
000104 :  102  GOTO MBRET, (1,2,3,4,5,99)
000105 : C---
000106 :   5   CONTINUE
000107 : C---
000108 :   99  RETURN
000109 :       END
000110 :       SUBROUTINE PLOTC(KX,KY,JOB)
000111 : C*
000112 : C****      Г P A Ф O P  -  C A L C O M P
000113 : C*
000114 : C*    KX,KY - KOOPДИHATЫ B ШAГAX
000115 : C*            BИPTYAЛЬHOГO ПЛOTTEPA
000116 : C*            OБЫЧHO ШAГ = 0.01 MM.
000117 : C*    JOB   - KOД OПEPAЦИИ :
000118 : C*       -1 : ДBИЖEHИE C ПOДHЯTЫM ПEPOM
000119 : C*        1 : ДBИЖEHИE C OПYЩEHHЫM ПEPOM
000120 : C*        0 : YCTAHOBKA ПEPA B HAЧAЛO
000121 : C*
000122 : C*    B BЫЗЫBAЮЩEЙ ПPOГPAMME MOЖET БЫTЬ
000123 : C*    OПИCAH  COMMON /SPLOT/ DX,DY
000124 : C*    ЗAДAЮЩИЙ CMEЩEHИE HAЧAЛA KOOPДИHAT
000125 : C*    B MИЛЛИMETPAX.
000126 : C*
000127 :       COMMON /SPLOT/ DX,DY
000128 :       COMMON /PLONAM/ NAME
000129 :       DATA NAME/'CALCOM'/, DX/0./, DY/0./
000130 :       DATA  XOLD/0./, YOLD/0./
000131 : C---
000132 :       X=0.01*KX+DX $ Y=0.01*KY+DY
000133 :       IF ( JOB ) 2,1,3
000134 : C---    YCTAHOBKA ПEPA B HAЧAЛO
000135 :   1   CALL ENDGR
000136 :       GOTO 21
000137 : C---    БEЗ PИCOBAHИЯ
000138 :    2  CALL PENUP
000139 :       GOTO 20
000140 : C---    PИCOBAHИE
000141 :    3  CALL PENDWN
000142 :       R=SQRT((X-XOLD)**2+(Y-YOLD)**2)
000143 :       IF(R.LE.0.25) GOTO 20
000144 :       N=4*R
000145 :       DO 10 I=1,N
000146 :   10  CALL MOVEXY( (I*X+(N-I)*XOLD)/N, (I*Y+(N-I)*YOLD)/N )
000147 : C---
000148 :   20  CALL MOVEXY(X,Y)
000149 :   21  XOLD=X $ YOLD=Y
000150 :       RETURN
000151 :       END
000152 :       SUBROUTINE PLOTTX(KX,KY,JOB)
000153 : C*
000154 : C*****   Г P A Ф O P  -  T E K T R O N I X
000155 : C*
000156 :       COMMON /SPLOT/ DX,DY
000157 :       COMMON /PLONAM/ NAME
000158 :       DIMENSION MBUF(20)
000159 :       DATA LB/114/, MFIR/0/
000160 :       DATA NAME/'TX4012'/, DX/0./, DY/0./
000161 :       DATA MBUF(1)/ 0722 0140 1004 0000B /
000162 : C---
000163 : C      "1" KX,KY  PABHA 0.01 MM
000164 : C      "1" DX,DY  PABHA 1.00 MM
000165 : C---
000166 :       JOB1=JOB
000167 :       KX1=100*DX+KX $ KY1=100*DY+KY
000168 :       IX=INT(0.0511*KX1) $ IY=INT(0.0511*KY1)
000169 :       IF(MFIR.NE.0) GOTO 1
000170 :       MFIR=1 $ NS=5
000171 :       J=JEXTRA( 1400 4000 0000 0000B, 61B, -1 )
000172 : C---
000173 :   1   IF(IX.LT.0) GOTO 11
000174 :       IF(IX.LT.1024) GOTO 2
000175 :       IX=1023 $ GOTO 12
000176 :   11  IX=0
000177 :   12  JOB1=-1
000178 :   2   IF(IY.LT.0) GOTO 13
000179 :       IF(IY.LT.780) GOTO 3
000180 :       IY=779 $ GOTO 14
000181 :   13  IY=0
000182 :   14  JOB1=-1
000183 : C---
000184 :   3   IF(JOB1.GT.0) GOTO 4
000185 :       CALL IPUTS(29,MBUF,NS)
000186 :       NS=NS+1
000187 : C---
000188 :   4   CALL IPUTS( IY.SHIFT.5.OR.40B, MBUF, NS )
000189 :       CALL IPUTS( IY.AND.37B.OR.140B,MBUF, NS+1)
000190 :       CALL IPUTS( IX.SHIFT.5.OR.40B, MBUF, NS+2)
000191 :       CALL IPUTS( IX.AND.37B.OR.100B,MBUF, NS+3)
000192 :       NS=NS+4
000193 :       IF(NS.LT.LB.AND.JOB.NE.0) RETURN
000194 : C---
000195 :       CALL IPUTS(0,MBUF,NS)
000196 :       J=LOCF(MBUF).AND.77777B.OR.1400 2000 3760 0000B
000197 :       J=JEXTRA(J,61B,-1)
000198 :       NS=0
000199 :       IF(JOB.EQ.0) MFIR=0
000200 :       RETURN
000201 :       END
000202 :       SUBROUTINE PLOTWX(KX,KY,JOB)
000203 : C*
000204 : C******   Г P A Ф O P  -  W X 4 6 7 5
000205 : C*
000206 :       COMMON /SPLOT/ DX,DY
000207 :       COMMON /PLONAM/ NAME
000208 :       COMMON /PNUMB/ NPEN,NPOLD
000209 :       DATA NPEN/1/, NPOLD/1/
000210 :       DATA NAME/'WX4675'/,  DX/0./,  DY/0./
000211 :       DATA KGP/ 2243 0012 0000 0000B /
000212 :       DIMENSION STR(3),KOSTR(5)
000213 :       DATA KOSTR/'M','X-KOOR',',','Y-KOOR', 0240 0000 0000 0000B/
000214 :       DATA MER/0/, XOLD/0./, YOLD/0./
000215 : C----------------------------------
000216 : C       "1" KX,KY   PABHA 0.01 MM
000217 : C       "1" DX,DY   PABHA 1.00 MM
000218 : C       "1" WX4675  PABHA 0.10 MM
000219 : C----------------------------------
000220 :       IF(NAME.NE.'WXHOME') GOTO 2
000221 :       N=JOPER('CMEHИ ЛИCT HA PLOTTER',4,1)
000222 :       NAME='WX4675'
000223 :   2   IF(NPEN.EQ.NPOLD) GOTO 3
000224 :       NPOLD=NPEN
000225 :       KOSTR=NPEN.SHIFT.-32.OR.KGP
000226 :       N=LOCF(KOSTR).AND.77777B.OR.140 0000B
000227 :       N=JEXTRA(N,61B,77777B)
000228 :   3   X=0.1*KX+10*DX $ Y=0.1*KY+10*DY
000229 :       N=0
000230 :       IF(X.EQ.XOLD .AND. Y.EQ.YOLD) RETURN
000231 :       IF(JOB.LE.0) KOSTR='M'
000232 :       IF(JOB.GT.0.AND.MER.EQ.0) KOSTR='D'
000233 :       CALL PUTG(KOSTR(2),X,1,4,0,IPOZ)
000234 :       CALL PUTG(KOSTR(4),Y,1,4,0,IPOZ)
000235 :       IF(X.LT.0. .OR. X.GT.3450.) GOTO 200
000236 :       IF(Y.LT.0. .OR. Y.GT.2600.) GOTO 200
000237 :       DO 1 I=1,26
000238 :       LET=IGETS(KOSTR,I-1)
000239 :       IF(LET.EQ.32) GOTO 1
000240 :       CALL IPUTS(LET,STR,N)
000241 :       N=N+1
000242 :    1  CONTINUE
000243 :       MER=0
000244 :       N=LOCF(STR).AND.77777B.OR.0740 0000B
000245 :       N=JEXTRA(N,61B,77777B)
000246 :       I=N.AND.4.OR.0
000247 :       IF(I.EQ.0) GOTO 10
000248 :       GOTO 100
000249 :  200  MER=1
000250 :       IF(JOB.EQ.0) GOTO 10
000251 : C---    PRINT BЫPEЗAH ИЗ ЭKOHOMИИ
000252 :  100    CONTINUE
000253 :   10  XOLD=X $ YOLD=Y
000254 :       RETURN
000255 :       END
000256 : *FULL
000257 : *MADLEN,L=R.
000258 :       *MUI*       :,NAME,
000259 :                         ,NTR ,3
000260 :                   8     ,VTM,      *00007B
000261 :                   8     ,A+X ,00001B
000262 :                   8     ,STX ,00002B
000263 :                   8     ,A+X ,00001B
000264 :                         ,E+N ,24
000265 :                   8     ,A*X ,00002B
000266 :                         ,ATI ,9
000267 :                         ,YTA ,00100B
000268 :                   9     ,V1M,      *00006B
000269 :                   14    ,UJ,  00000B
000270 :      *00006B:     8     ,AOX ,
000271 :                   14    ,UJ,  00000B
000272 : CONST
000273 :      *00007B:,OCT,0020  0000  0000  0000
000274 :      *00010B:,OCT,6400  0000  0000  0000
000275 :      *00011B:,OCT,0000  0000  0000  0000
000276 :                   ,END,
000277 :   *REWIN:,NAME,
000278 :   FMRTAPE:,SUBP,
000279 :   14,VTM,500B
000280 :   ,UJ,B
000281 :   *BACKS:,ENTRY,
000282 :   14,VTM,440B
000283 :   ,UJ,B
000284 :   *ENDFI:,ENTRY,
000285 :   14,VTM,600B
000286 :   ,UJ,B
000287 :   SCHEOF:,ENTRY,
000288 :   14,VTM,404B
000289 :   ,UJ,B
000290 : C---------------
000291 :   B:,ATI,12
000292 :   12,XTA,
000293 :   ,UTC,=177
000294 :   ,AAX,
000295 :   ,ITS,14
000296 :   ,ASN,64-39
000297 :   15,AOX,
000298 :   ,UJ,FMRTAPE
000299 : C--------------
000300 :   ,END,
000301 :   *ПEPФOPA:,NAME,.CDC/DUBNA
000302 :   ,TEXT,8HCAПCAX П
000303 :   KOД*CDC:,ENTRY,
000304 :   SPBL:,OCT,74
000305 :   :,BSS,7
000306 :   40,OCT,0000 0042 0000 0000
000307 :   44,OCT,0042 0000 0000 2102
000308 :   50,OCT,4000 2042 4042 1042
000309 :   54,OCT,1400 4102 2000 1102
000310 :   60,OCT,0100 0200 0400 1000
000311 :   64,OCT,0004 0010 0020 0040
000312 :   70,OCT,4042 4022 0001 0002
000313 :   74,OCT,0000 2022 0102 2012
000314 :  100,OCT,4100 4200 4400 0042.CBA
000315 :  104,OCT,4004 4010 4020 4040.GFED
000316 :  110,OCT,2200 2400 4001 4002.KJIH
000317 :  114,OCT,2010 2020 2040 2100.ONML
000318 :  120,OCT,1200 2001 2002 2004.SRQP
000319 :  124,OCT,1010 1020 1040 1100.WVUT
000320 :   130,OCT,2202 1001 1002 1004
000321 :   134,OCT,0000 0000 0006 0000
000322 :   140,OCT,3002 3000 4400 0402
000323 :   144,OCT,2202 5002 4020 4012
000324 :   150,OCT,2200 0202 1012 4002
000325 :   154,OCT,2010 4002 2040 2012
000326 :   160,OCT,4100 2004 0022 1006
000327 :   164,OCT,4200 2003 1002 1100
000328 :   170,OCT,4202 1003 2402 4003
000329 :   174,OCT,0000 1402 0003 4402
000330 :  ,END,
000331 :  /MONTRAN:,NAME,.10.10.79
000332 : C
000333 : C*****   ДOПOЛHEHИE PEЗИДEHTA MOHИTOPA
000334 : C*****   ПPИ PAБOTE ДO '*EXECUTE'
000335 : C*****   ( 2-OЙ PEЗИДEHT MOHИTOPA )
000336 : C
000337 : C-----   COCTAB 2-ГO PEЗИДEHTA MOHИTOPA
000338 : C
000339 :  RM/:LC,BLOCK,S16,S17
000340 :  READ*:,SUBP,
000341 :  STOP*:,SUBP,
000342 :  MONREAD*:,SUBP,
000343 :  COSYTISO:,SUBP,
000344 :  DRUMTAP*:,SUBP,
000345 :  PRINT8:,SUBP,
000346 :  DATAPR*:,SUBP,
000347 : C
000348 : C-----   ИCПOЛЬЗYEMOE ИЗ PEЗИДEHTA MOHИTOPA
000349 : C
000350 :  LOADANGO:,SUBP,
000351 :  LOADCALL:,SUBP,
000352 :  MONITOR*:,SUBP,
000353 :  EN TRAN:,EQU,MONITOR*+2
000354 :   /VERE/:LC,BLOCK,(1),S/CALL,STALOA*(3)
000355 :  SAV/47:LC,BLOCK,SAV47.
000356 :  MONCARD*:,LC,25
000357 :  ENTRY*:,LC,42
000358 :  CFIRST*:,LC,1
000359 :  INFLOAD*:LC,BLOCK,ALOAD,INFLOA,(2)
000360 :  STATIC*:,LC,1
000361 :  RM*:LC,BLOCK,RM(5)
000362 :  TRNAME*:,LC,1
000363 :  FLGEX* :,LC,1
000364 :  ERROFLG*:,LC,1
000365 :  PUNCHFL*:,LC,1
000366 :  LHEAP*:LC,BLOCK,FLG
000367 : C
000368 :  S/STAR:,EQU,52B.B KOДE ISO
000369 : C
000370 : C-----   HA BXOДE :
000371 : C        И12 = 0 - ПOXOД HA STAR*
000372 : C                  ИЛИ TPAHCЛЯTOP
000373 : C        И12 = 1 - ПOCЛE TPAHCЛЯЦИИ
000374 : C
000375 : C.....   ЗAПOMИHAHИE И BOCCTAHOBЛEHИE
000376 : C        CTPEЛOK ЗAГPYЗKИ
000377 :  ,XTA,S16.ПEPBЫЙ BЫЗOB
000378 :  ,U1A,RESET. - HET
000379 :  ,XTA,16B....
000380 :  ,ATX,S16. ЗAПOMHИM
000381 :  ,XTA,17B
000382 :  ,ATX,S17....
000383 :  RESET:,XTA,S16....
000384 :  ,ATX,16B
000385 :  ,XTA,S17. BOCCTAHOBИM
000386 :  ,ATX,17B
000387 :  ,NTR,2....
000388 :  12,VZM,CH STAR.= HA BЫЗOB
000389 :  12,UTM,-1
000390 :  12,V1M,INIT SYS.ИHИЦИAЛИЗAЦИЯ
000391 : C.....   OTPAБOTAЛ TPAHCЛЯTOP
000392 :  ,XTA,FLGEX*.БЫЛИ OШИБKИ
000393 :  ,UZA,NO ERROR. - HET
000394 :  ,ATX,ERROFLG*.- HAKOПЛEHИE
000395 :  ,XTA,.  OШИБOK
000396 :  ,ATX,FLGEX*
000397 :  ,UJ,CH STAR
000398 : C.....   OШИБOK TPAHCЛЯЦИИ HE БЫЛO
000399 :  NO ERROR:,XTA,INFLOA
000400 :  ,UZA,DINAM
000401 :  ,XTA,RM+2.CTAT.ЗAГPYЗKY
000402 :  ,ATX,ALOAD.HA 'WRITLIB*'
000403 : C.....   ЗAПИCЬ CM BO BPEM.БИБЛИOTEKY
000404 :  DINAM:14,VTM,ENTRY*
000405 :  ,ITS,14
000406 :  ,XTS,WRITLIB*
000407 :  ,CALL,LOADANGO
000408 :  ,XTA,.BOCCTAHOBИM
000409 :  ,ATX,ALOAD.ДИH.ЗAГPYЗKY
000410 : C.....   ПEPФOPAЦИЯ CM
000411 :  ,XTA,PUNCHFL*
000412 :  ,UZA,NO PUNCH
000413 :  14,VTM,ENTRY*
000414 :  ,ITS,14
000415 :  ,XTS,STPUNCH*
000416 :  ,CALL,LOADANGO
000417 :  NO PUNCH:,BSS,
000418 : C.....   ПOXOД HA 'STAR*' ИЛИ TPAHCЛЯTOP
000419 :  CH STAR:,XTA,MONCARD*
000420 :  ,ASN,64+40
000421 :  ,ATI,14.TEKCTOB.KAPTA
000422 :  14,VZM,TO STAR. - HET
000423 :  14,UTM,-S/STAR.CO ЗBEЗДOЧKOЙ
000424 :  14,VZM,TO STAR. - ДA
000425 :  TR*CALL :,ENTRY,.ДЛЯ БEMШA
000426 : C.....   BЫЗOB KAKOГO-TO TPAHCЛЯTOPA
000427 :  ,ATX,CFIRST*....
000428 : C.....   ПPOБA : TPAHCЛЯTOP B CTATИKE
000429 :  ,XTA,STATIC*
000430 :  ,UZA,PUT ST. = HET
000431 :  ,XTA,RM+2
000432 :  PUT ST:,BSS,.YCTAHOBИM
000433 :   ,ATX,ALOAD
000434 :  ,XTA,FLG
000435 :  ,AAX,M1000
000436 :  ,UZA,TO TRAN
000437 :  ,XTA,RM.ЗAГPYЗKY
000438 :  ,ATX,16B.TPAHCЛЯTOPA
000439 :  ,XTA,RM+1.C ЯЧ.1000
000440 :  ,ATX,17B....
000441 :  TO TRAN:,XTS,TRNAME*
000442 :  ,XTS,S/CALL
000443 :  13,VTM,EN TRAN.
000444 :  ,UZA,LOADCALL
000445 :  ,ASN,64+20
000446 :  15,ATX,-2
000447 :  15,UTM,-1
000448 :  ,DRUM,S/CALL
000449 :  15,WTC,
000450 :  ,UJ,
000451 : C.....   BЫXOД HA 'STAR*'
000452 :  TO STAR:,BSS,
000453 : C.....   YCTAHOBИM ДИHAM.ЗAГPYЗKY
000454 :  ,XTS,STAR*
000455 :  ,UJ,LOADANGO
000456 :  INIT SYS:,BSS,
000457 : C.....   ИHИЦИAЛИЗAЦИЯ MOHИTOPHOЙ CИCTEMЫ
000458 :  ,XTA,INFLOA
000459 :  ,UZA,INPUT
000460 :  ,XTA,RM+2
000461 :  ,ATX,ALOAD
000462 :  INPUT:,XTS,INPUTCAL
000463 :  ,CALL,LOADANGO
000464 :  ,XTS,CHEKJOB*
000465 :  ,CALL,LOADANGO
000466 : C-----   ИMEHA PAЗДEЛOB
000467 :  INPUTCAL:,TEXT,8HINPUTCAL
000468 :  CHEKJOB*:,TEXT,8HCHEKJOB*
000469 :  STAR*:,TEXT,8HSTAR*
000470 :  WRITLIB*:,TEXT,8HWRITLIB*
000471 :  STPUNCH*:,TEXT,8HSTPUNCH*
000472 :  ,BSS,
000473 : C-----
000474 :  M1000:,LOG,2 00000
000475 :  ,END,
000476 :  /TRANS/:,NAME,.01.03.80
000477 : C-----   CTATИЧECKИE TPAHCЛЯTOPЫ ПOCT.БИБЛИOTEKИ
000478 :  ,ISO,6H*FORTR
000479 :  ,TEXT,8HFORTRAN
000480 :  ,ISO,6H*ASSEM
000481 :  ,TEXT,8HASEMBLER
000482 :  ,ISO,6H*ALGOL
000483 :  ,TEXT,8HALGOL
000484 :  ,LOG,.ПPИЗHAK KOHЦA
000485 : C-----   ДИHAMИЧECKИE TPAHCЛЯTOPЫ
000486 :   ,ISO,6H*MADLE
000487 :   ,TEXT,8HMDLCARD*
000488 :  ,ISO,6H*REFAL
000489 :  ,TEXT,8HREFAL*
000490 :  ,ISO,6H*BEMSH
000491 :  ,TEXT,8HBEMSH*
000492 :  ,ISO,6H*SIMUL
000493 :  ,TEXT,8HSIMULA*
000494 :  ,LOG,.ПPИЗHAK KOHЦA
000495 : C-----   CTATИЧECKИE TPAHCЛЯTOPЫ (ДOПOЛHИTEЛЬHЫE)
000496 :  ,ISO,6H*FTN
000497 :  ,TEXT,8HINITFTN
000498 :  ,ISO,6H*FOREX
000499 :  ,TEXT,8HFOREX*83
000500 :   ,ISO,6H*PASCA
000501 :   ,TEXT,8HPASCAL*
000502 :  ,ISO,6H*EXFOR
000503 :  ,TEXT,8HEXFOR
000504 :  ,LOG,.ПPИЗHAK KOHЦA
000505 : C-----   ЛEHTA/MOДEЛЬ ДOПOЛH.CTAT.БИБЛИOTEKИ
000506 :  ,TEXT,8HMONSYS '11'
000507 :  ,LOG,100.HOMEP ЗOHЫ
000508 :  ,END,
000509 :       A/AND       :,NAME,
000510 :       A/MINUS     :,SUBP,
000511 :       A/I0        :,SUBP,
000512 :                         ,UZA,      *00002B
000513 :                         ,UTC, 00000B
000514 :      *00001B:     15    ,XTA ,
000515 :                   14    ,UJ,  00000B
000516 :      *00002B:     15    ,UTM, 77777B
000517 :                   14    ,UJ,  00000B
000518 :  A/EQUIV:,ENTRY,
000519 :      *00003B:           ,U1A,      *00001B
000520 :                   15    ,XTA ,
000521 :  A/NOT:,ENTRY,
000522 :                         ,NTR ,6
000523 :                         ,UZA,      *00006B
000524 :                         ,XTA ,
000525 :                   14    ,UJ,  00000B
000526 :      *00006B:           ,XTA ,A/MINUS
000527 :                   14    ,UJ,  00000B
000528 :  A/IMPL:,ENTRY,
000529 :                   13    ,MTJ ,14
000530 :                         ,UZA,      *00003B
000531 :                   15    ,UTM, 77777B
000532 :                         ,UTC, 00000B
000533 :                         ,XTA ,A/MINUS
000534 :                   14    ,UJ,  00000B
000535 :  A/RTOI:,ENTRY,
000536 :                         ,UTC,      *00045B
000537 :                         ,A+X ,
000538 :                         ,NTR ,3
000539 :                         ,A+X ,A/I0
000540 :                         ,NTR ,2
000541 :                   14    ,UJ,  00000B
000542 :  ENTIER*:,ENTRY,
000543 :                         ,NTR ,3
000544 :                         ,A+X ,A/I0
000545 :                         ,NTR ,2
000546 :                   13    ,UJ,  00000B
000547 :  A/RHOCHI:,ENTRY,
000548 :                         ,NTR ,18
000549 :                         ,U1A,      *00024B
000550 :      *00020B:           ,ATI ,14
000551 :                         ,UTC,      *00046B
000552 :                         ,XTA ,
000553 :      *00022B:     14    ,VZM,      *00031B
000554 :                   15    ,A*X ,77777B
000555 :                   14    ,UTM, 77777B
000556 :                         ,UJ,       *00022B
000557 :      *00024B:           ,NTR ,3
000558 :                         ,AVX ,A/MINUS
000559 :                         ,NTR ,2
000560 :                         ,ATI ,14
000561 :                         ,UTC,      *00046B
000562 :                         ,XTA ,
000563 :      *00027B:     15    ,A/X ,77777B
000564 :                   14    ,UTM, 77777B
000565 :                   14    ,V1M,      *00027B
000566 :                         ,UTC, 00000B
000567 :      *00031B:     15    ,UTM, 77777B
000568 :                   13    ,UJ,  00000B
000569 :  A/IHOCHI:,ENTRY,
000570 :                         ,ITS ,13
000571 :                         ,STX ,00021B
000572 :                   13    ,VJM,      *00020B
000573 :                         ,NTR ,3
000574 :                         ,A+X ,A/I0
000575 :                         ,NTR ,2
000576 :                         ,WTC, 00021B
000577 :                         ,UJ,  00000B
000578 :  A/SIGN:,ENTRY,
000579 :                         ,A+X ,
000580 :                         ,NTR ,7
000581 :                         ,UZA,      *00043B
000582 :                         ,UTC,      *00044B
000583 :                         ,XTS ,
000584 :                   15    ,AVX ,
000585 :      *00042B:           ,NTR ,2
000586 :                   13    ,UJ,  00000B
000587 :      *00043B:           ,XTA ,A/I0
000588 :                         ,UJ,       *00042B
000589 : CONST
000590 :      *00044B:,OCT,6400  0000  0000  0001
000591 :      *00045B:,OCT,4010  0000  0000  0000
000592 :      *00046B:,OCT,4050  0000  0000  0000
000593 :  ,END,
000594 :  A/ARRD/K:,NAME,       ALGOL/ INTERPRETIERENDE ROUTINE    3.1.74
000595 : C  ROUTINEN FUER FELDVEREINBARUNGEN
000596 : C  A/ARRD/K: ADRESSBUCH SCHON SKALAR ALLOKIERT
000597 : C  A/ARRD/V: ADRESSBUCH NOCH NICHT ALLOKIERT
000598 : C  PARAMETER:  MAGAZIN: FELDGRENZEN
000599 : C              I11: ANZAHL DER GRENZENPAARE
000600 : C              I12: ANZAHL DER FELDER
000601 : C              I13: SKALARE FELDADRESSE
000602 : C              I14: RK-ADRESSE
000603 : C
000604 :  C:,BSS,
000605 :  SYSBACK*:,SUBP,.
000606 :  AUG:,EQU,             21B
000607 :  D:,EQU,               22B
000608 :  I15G:,EQU,            23B
000609 :  A:,EQU,               23B                AB I=1
000610 :  B:,EQU,               31B
000611 :  L:,EQU,               40B
000612 :  K:,EQU,               12
000613 :  N:,EQU,               11
000614 :  I:,EQU,               10
000615 :  FW:,EQU,              13
000616 :  A/ARRD/K:,NTR,        3
000617 :  9,VTM,                C
000618 :  15,XTS,               -1                 BN
000619 :  15,A-X,               -2                 AN
000620 :  9,A+X,                =I1-C
000621 :  ,UJ,                  *+2
000622 :  A/ARRD/V:,ENTRY,
000623 :  ,NTR,                 3
000624 :  ,XTS,
000625 :  :9,VTM,               C
000626 :  ,STX,                 AUG
000627 :  N,MTJ,                I
000628 :  BOUNDS:I,STX,         B
000629 :  I,STX,                A
000630 :  I,UTM,                -1
000631 :  I,V1M,                BOUNDS
000632 :  15,ATX,
000633 :  NEXTARR:N,MTJ,        I
000634 :  N,XTA,                B
000635 :  N,A-X,                A
000636 :  ,U1A,                 BD/ERR
000637 :  9,A+X,                =I1-C
000638 :  ,ATX,                 D
000639 :  ,XTA,                 AUG
000640 :  ,ATX,                 I15G
000641 :  ,UZA,                 L*
000642 :  ,ITA,                 15
000643 :  ,ATX,                 I15G
000644 :  FW,UTC,
000645 :  15,VTM,               1
000646 :  L*:,ITA,              15
000647 :  9,AEX,                =I0-C
000648 :  N,A-X,                A
000649 :  9,AAX,                =77777-C
000650 :  FW,ATX,
000651 :  ITEST:I,UTM,          -1
000652 :  I,V1M,                MORE
000653 :  ,ITA,                 15
000654 :  ,ASN,                 64-30
000655 :  ,XTS,                 D
000656 :  ,ASN,                 64-15
000657 :  15,AEX,
000658 :  FW,AEX,
000659 :  FW,ATX,
000660 :  ,ITA,                 15                 UEBERLAUFTEST
000661 :  ,ARX,                 D
000662 :  9,AEX,                =I0-C
000663 :  ,XTS,                 16B
000664 :  9,AAX,                =77777-C
000665 :  15,A-X,                                  LADEGR-I15-D
000666 :  ,U1A,                 MEMERR
000667 :  ,WTC,                 D
000668 :  15,UTM,
000669 :  ,WTC,                 AUG
000670 :  FW,UTM,               1
000671 :  ,XTA,                 I15G
000672 :  :,UZA,                *+1
000673 :  ,ATI,                 15
000674 :  K,UTM,                -1
000675 :  K,V1M,                NEXTARR
000676 :  ,NTR,                 2                  UEBERLAUFTEST FEHLT
000677 :  14,UJ,
000678 :  MORE:I,XTA,           B
000679 :  I,A-X,                A
000680 :  ,U1A,                 BD/ERR
000681 :  9,A+X,                =I1-C
000682 :  ,ATX,                 L+1
000683 :  9,AAX,                =77777-C
000684 :  ,ATX,                 L
000685 :  ,XTA,                 I15G
000686 :  ,U1A,                 *+2
000687 :  :,ITA,                15
000688 :  ,ARX,                 D
000689 :  9,AOX,                =I0-C
000690 :  I,A-X,                A
000691 : L9,AOX,                =7777 7777 7770 0000-C.UEBERLAUF ERZEUGEN
000692 :  ,WTC,                 D
000693 :  8,VTM,
000694 :  ZYK:15,ATX,
000695 :  ,ARX,                 L
000696 :  8,UTM,                -1
000697 :  8,V1M,                ZYK
000698 :  ,XTA,                 I15G
000699 :  :,UZA,                *+2
000700 :  ,ATI,                 15
000701 :  ,XTA,
000702 :  ,ATX,                 I15G
000703 :  ,XTA,                 D
000704 :  ,NTR,                 2
000705 :  ,A*X,                 L +1
000706 :  ,NTR,                 3
000707 :  9,A+X,                =I0-C
000708 :  ,ATX,                 D
000709 :  ,UJ,                  ITEST
000710 :  MEMERR:8,VTM,         3
000711 :  15,MTJ,               9
000712 :  12,VTM,A/ERR.
000713 :  ,UJ,SYSBACK*.
000714 :  BD/ERR:8,VTM,         2
000715 :  14,MTJ,               9
000716 :  12,VTM,A/ERR.
000717 :  ,UJ,SYSBACK*....
000718 :  A/ERR:,TEXT,8HA/ERROR
000719 :  ,END,
000720 :  A/CALLP :   , NAME  ,                    PROZEDURORGANISATION
000721 :  *C      :   , BSS   ,
000722 :              , ATI   , 15                 KELLERNIVEAU AUFRUF
000723 :  A/CALLS :   , ENTRY ,                    PROZ OHNE PAR
000724 :              , ITA   , 13
000725 :              , ITS   , 1
000726 :              , ITS   , 2
000727 :              , ITS   , 3
000728 :              , ITS   , 4
000729 :              , ITS   , 5
000730 :              , ITS   , 6
000731 :              , ITS   , 7
000732 :           15 , ATX   ,
000733 :           14 , UJ    ,                    EINSPR IN PROZEDUR
000734 :  A/PROCRT:   , ENTRY ,                    RUECKKEHR AUS PROZ
000735 :              , STX   , 21B
000736 :              , STI   , 7
000737 :              , STI   , 6
000738 :              , STI   , 5
000739 :              , STI   , 4
000740 :              , STI   , 3
000741 :              , STI   , 2
000742 :              , STI   , 1
000743 :              , ATI   , 13
000744 :              , XTA   , 21B
000745 :           13 , UJ    ,
000746 :  PARBEG  :   , ITA   , 13
000747 :              , ITS   , 1
000748 :              , ITS   , 2
000749 :              , ITS   , 3
000750 :              , ITS   , 4
000751 :              , ITS   , 5
000752 :              , ITS   , 6
000753 :              , ITS   , 7
000754 :           15 , ATX   ,
000755 :           11 , UJ    , PARBEG*
000756 :  PARBEG* :12 , XTA   , 1
000757 :              , ATI   , 1
000758 :           12 , XTA   , 2
000759 :              , ATI   , 2
000760 :           12 , XTA   , 3
000761 :              , ATI   , 3
000762 :           12 , XTA   , 4
000763 :              , ATI   , 4
000764 :           12 , XTA   , 5
000765 :              , ATI   , 5
000766 :           12 , XTA   , 6
000767 :              , ATI   , 6
000768 :           12 , XTA   , 7
000769 :              , ATI   , 7
000770 :           14 , UJ    ,                    EINSPRUNG IN PAR
000771 :  A/PARCON:   , ENTRY ,
000772 :           15 , ATX   , 3
000773 :           13 , VTM   , *C
000774 :           14 , WTC   , -1
000775 :           7  , UTC   ,
000776 :           12 , VTM   ,
000777 :              , ITA   , 12
000778 :           13 , AEX   , CON1-*C
000779 :           13 , XTS   , CON2-*C
000780 :           15 , XTS   , 1
000781 :           14 , UJ    ,
000782 :  CON1    :12 , VTM   , 66666B
000783 :           11 , VTM   ,
000784 :  CON2    :11 , XTA   ,
000785 :           13 , UJ    ,
000786 :  A/PARVAR:   , ENTRY ,
000787 :           15 , ATX   , 3
000788 :           13 , VTM   , *C
000789 :           14 , XTA   , -1
000790 :           13 , AAX   , =7400 0000-*C
000791 :              , ASN   , 64+20
000792 :              , ATI   , 12
000793 :           12 , ITA   ,
000794 :           14 , ARX   , -1
000795 :           13 , AAX   , =77777-*C
000796 :           13 , AEX   , VAR1-*C
000797 :           13 , XTS   , VAR2-*C
000798 :           15 , XTS   , 1
000799 :           14 , UJ    ,
000800 :  VAR1    :   , UTC   ,
000801 :           12 , VTM   ,
000802 :  VAR2    :12 , XTA   ,
000803 :           13 , UJ    ,
000804 :  A/PARPAR:   , ENTRY ,
000805 :           15 , ATX   , 3
000806 :           13 , VTM   , *C
000807 :           14 , XTA   , -1
000808 :           13 , AAX   , =7400 0000-*C
000809 :              , ASN   , 64+20
000810 :              , ATI   , 12
000811 :           12 , ITA   ,
000812 :           14 , ARX   , -1
000813 :              , ATI   , 12
000814 :           12 , XTA   ,
000815 :           12 , XTS   , 1
000816 :           15 , XTS   , 1
000817 :           14 , UJ    ,
000818 :  A/PARVAL:   , ENTRY ,
000819 :           15 , XTS   , -2                 KELLER IN 15/-1,WERT IN AC
000820 :           15 , ATX   , 1
000821 :              , UTC   , B11UJ
000822 :              , XTA   ,
000823 :           15 , ATX   , -2
000824 :           15 , XTA   , 1
000825 :           14 , UJ    ,
000826 :  B11UJ   :11 , UJ    ,
000827 :  A/PAREXP:   , ENTRY ,
000828 :           15 , ATX   , 3
000829 :           13 , VTM   , *C
000830 :           14 , XTA   , -1
000831 :           13 , AAX   , =7400 0000-*C
000832 :              , ASN   , 64-4
000833 :           13 , AEX   , EXP1-*C
000834 :           15 , AEX   , 3
000835 :              , ITS   , 14
000836 :              , ASN   , 64-24
000837 :           13 , AEX   , EXP2-*C
000838 :           15 , XTS   , 1
000839 :           14 , WTC   , -1
000840 :              , UJ    ,
000841 :  EXP1    :11 , VTM   ,                    IRP
000842 :           12 , VTM   ,                    KELLERNIVEAU
000843 :  EXP2    :14 , VTM   ,                    ANF-ADR
000844 :              , UJ    , PARBEG
000845 :  A/PARLAB:   , ENTRY ,
000846 :           15 , ATX   , 3
000847 :           13 , VTM   , *C
000848 :           14 , XTA   , -1
000849 :           13 , AAX   , =7400 0000-*C
000850 :              , ASN   , 64-4
000851 :           13 , AEX   , EXP1-*C
000852 :           15 , AEX   , 3
000853 :           14 , XTS   , -1
000854 :           13 , AAX   , =77777-*C
000855 :              , ASN   , 64-24
000856 :           13 , AEX   , EXP2-*C
000857 :           15 , XTS   , 1
000858 :           14 , UJ    ,
000859 :  A/PARSTR:   , ENTRY ,
000860 :           15 , ATX   , 3
000861 :           13 , VTM   , *C
000862 :           14 , WTC   , -1
000863 :           7  , UTC   ,
000864 :           12 , VTM   ,
000865 :              , ITA   , 12
000866 :           13 , AEX   , STR1-*C
000867 :           13 , XTS   , STR2-*C
000868 :           15 , XTS   , 1
000869 :           14 , UJ    ,
000870 :  STR1    :   , UTC   ,
000871 :           14 , VTM   ,
000872 :  STR2    :   , ITA   , 14
000873 :           13 , UJ    ,
000874 :  ,END,
000875 :       A/COMMON   :,NAME,
000876 :       STOP*      :,SUBP,
000877 :       PRINT8*    :,SUBP,
000878 :       A/ERROR    :,SUBP,
000879 :  *00000    :     ,NTR,  3
000880 :              9 ,  VTM,   *00000
000881 :             11 ,  MTJ,  10
000882 :             14 ,  XTS,
000883 :              9 ,  AAX,  102B
000884 :              9 ,  ATX,  105B
000885 :             14 ,  XTA,
000886 :                  ,ASN,  130B
000887 :              9 ,  AAX,  102B
000888 :              9 ,  ARX,  105B
000889 :              9 ,  ATX,  105B
000890 :             14 ,  XTA,
000891 :              9 ,  AAX,  103B
000892 :              9 ,  AEX,  77B
000893 :                  ,UIA,   *00015
000894 :                  ,UTC,  77777B
000895 :             14 ,  XTA,
000896 :              9 ,  AAX,  76B
000897 :              9 ,  AEX,  75B
000898 :                  ,UIA,   *00015
000899 :                  ,UTC,  77777B
000900 :             14 ,  XTA,
000901 :              9 ,  AAX,  102B
000902 :              9 ,  ARX,  105B
000903 :              9 ,  ATX,  105B
000904 :                  ,UTC,
000905 :  *00015    :     ,STX,
000906 :                  ,UTC,
000907 :  *00016    :10 ,  STX,  31B
000908 :             10 ,  STX,  23B
000909 :             10 ,  UTM,  77777B
000910 :             10 ,  VIM,   *00016
000911 :             15 ,  ATX,
000912 :                  ,UTC,
000913 :             11 ,  MTJ,  10
000914 :             11 ,  XTA,  31B
000915 :             11 ,  A-X,  23B
000916 :                  ,UIA,   *00063
000917 :              9 ,  A+X,  101B
000918 :                  ,ATX,  22B
000919 :                  ,XTA,
000920 :                  ,ATX,  23B
000921 :             10 ,  UTM,  77777B
000922 :             10 ,  VIM,   *00031
000923 :              9 ,  XTA,  105B
000924 :              9 ,  AEX,  100B
000925 :             11 ,  A-X,  23B
000926 :              9 ,  AAX,  102B
000927 :                  , UJ,   *00033
000928 :                  ,UTC,
000929 :  *00031    :     ,ITA,  15
000930 :              9 ,  AEX,  100B
000931 :             11 ,  A-X,  23B
000932 :              9 ,  AAX,  102B
000933 :  *00033    :13 ,  ATX,
000934 :             10 ,  UTM,  1
000935 :  *00034    :10 ,  UTM,  77777B
000936 :             10 ,  VIM,   *00043
000937 :              9 ,  XTA,  105B
000938 :                  ,ASN,  42B
000939 :                  ,XTS,  22B
000940 :                  ,ASN,  61B
000941 :             15 ,  AEX,
000942 :             13 ,  AEX,
000943 :             13 ,  ATX,
000944 :             12 ,  UTM,  77777B
000945 :             12 ,  VIM,   *00065
000946 :                  ,NTR,  2
000947 :             14 ,   UJ,  1
000948 :                  ,UTC,
000949 :  *00043    :10 ,  XTA,  31B
000950 :             10 ,  A-X,  23B
000951 :                  ,UIA,   *00063
000952 :              9 ,  A+X,  101B
000953 :                  ,ATX,  41B
000954 :              9 ,  AAX,  102B
000955 :                  ,ATX,  40B
000956 :             10 ,  UTM,  77777B
000957 :             10 ,  VIM,   *00051
000958 :              9 ,  XTA,  105B
000959 :                  , UJ,   *00052
000960 :                  ,UTC,
000961 :  *00051    :     ,ITA,  15
000962 :                  ,ARX,  22B
000963 :  *00052    :10 ,  UTM,  1
000964 :              9 ,  AOX,  100B
000965 :             10 ,  A-X,  23B
000966 :              9 ,  AOX,  104B
000967 :                  ,WTC,  22B
000968 :              8 ,  VTM,
000969 :  *00055    :15 ,  ATX,
000970 :                  ,ARX,  40B
000971 :              8 ,  UTM,  77777B
000972 :              8 ,  VIM,   *00055
000973 :                  ,NTR,  2
000974 :                  ,XTA,  22B
000975 :                  ,A*X,  41B
000976 :                  ,NTR,  3
000977 :              9 ,  A+X,  100B
000978 :                  ,ATX,  22B
000979 :                  , UJ,   *00034
000980 :                  ,UTC,
000981 :  *00063    : 8 ,  VTM,  2
000982 :             14 ,  UTM,  1
000983 :             14 ,  MTJ,  9
000984 :             13 ,  VJM,  A/ERROR
000985 :  *00065    :14 ,  VTM,   *00070
000986 :                  ,ITS,  14
000987 :             14 ,  VTM,   *00074
000988 :                  ,ITS,  14
000989 :             13 ,  VTM,  STOP*
000990 :                  , UJ,  PRINT8*
000991 :  *00070    :,     LOG,  23670151 20674502.
000992 :             ,     LOG,  20265040 22640566.
000993 :             ,     LOG,  31074552 10046501.
000994 :             ,     LOG,  20641551 20420144.
000995 :  *00074    :,     LOG,  23650117 31647552.
000996 :             ,     LOG,  00000000 02200000.
000997 :             ,     LOG,  00000000 03700000.
000998 :             ,     LOG,  64000000 00000000.
000999 :             ,     LOG,  64000000 00000000.
001000 :             ,     LOG,  64000000 00000001.
001001 :             ,     LOG,  00000000 00077777.
001002 :             ,     LOG,  74000000 00000000.
001003 :             ,     LOG,  77777777 77700000.
001004 :  BSS       :     ,BSS,  1
001005 :       ,END,
001006 :  A/DRUCK:,NAME,
001007 :  C:,BSS,
001008 :  ARG:,EQU,             -7
001009 :  I:,EQU,               11
001010 :  PRINT8:,SUBP,
001011 :  PO:,EQU,              10
001012 :  SO:,EQU,              9
001013 :  STOPFLG*:,LC,         1
001014 :  STOPENTR:,UTC,
001015 :  ,UTC,                 A/NLINE
001016 :  SCHIEBEN:8,XTA,       Z-C             . UP I12
001017 :  8,AEX,                =:774-C
001018 :  ,ASN,                 64-7
001019 :  15,ATX,
001020 :  ,YTA,
001021 :  ,STI,                 14
001022 :  14,ASN,                                  KOMMA VOR B47
001023 :  8,ATX,                Z-C
001024 :  12,UJ,
001025 :  I14ZIFF:8,XTA,        Z-C             . UP I12
001026 :  ,ASN,                 64+3
001027 :  8,XTS,                Z-C
001028 :  ,ASN,                 64+1
001029 :  15,ARX,
001030 :  ,ASN,                 64-4
001031 :  8,ATX,                Z-C
001032 :  ,YTA,
001033 :  8,ARX,                =60-C
001034 :  13,VJM,               PUT
001035 :  14,UTM,               -1
001036 :  14,V1M,               I14ZIFF
001037 :  12,UJ,
001038 :  I14LEER:8,XTA,        =40-C           . UP I12
001039 :  13,VJM,               PUT
001040 :  14,UTM,               -1
001041 :  14,V1M,               I14LEER
001042 :  12,UJ,
001043 :  IZPBEST:8,ATX,        ZP-C            . ANFW VON AUSSEN UP I12
001044 :  8,X-A,                Z-C
001045 :  ,UZA,                 IGEF
001046 :  8,XTA,                ZP-C
001047 :  8,A/X,                ZEHN-C
001048 :  I,UTM,                -1
001049 :  ,UJ,                  IZPBEST
001050 :  IGEF:8,XTA,           Z-C
001051 :  8,A/X,                ZP-C
001052 :  8,A/X,                ZEHN-C
001053 :  8,ATX,                Z-C
001054 :  8,A-X,                EINS-C
001055 :  12,UJ,
001056 :  AUSGVORZ:,NTR,        22B                UP I13, ARG IN AAC
001057 :  ,U1A,                 NEG
001058 :  8,XTA,                =40-C           . LEERZ
001059 :  ,UJ,                  PUT
001060 :  NEG:8,XTA,            =55-C           . MINUSZ
001061 :  ,UJ,                  PUT
001062 :  A/IPRI:,ENTRY,
001063 :  IDRU:14,VTM,          IDRU*
001064 :  ,UJ,                  RETTEN
001065 :  IDRU*:8,ARX,          =17-C           ..+15
001066 :  13,VTM,               IDRU*
001067 :  ,U1A,                 REKDRU           .SETZT REKZ AUF -126
001068 :  8,ATX,                REKZ-C
001069 :  8,XTA,                ZH12-C
001070 :  IZYK:8,ATX,           ZP-C
001071 :  15,XTA,               ARG
001072 :  8,AMX,                ZP-C
001073 :  ,UZA,                 SIGNIF
001074 :  8,XTA,                ZP-C
001075 :  8,AEX,                =R1.0-C
001076 :  ,UZA,                 SIGNIF
001077 :  8,XTA,                =40-C              LEER
001078 :  13,VJM,               PUT
001079 :  8,XTA,                ZP-C
001080 :  8,A/X,                ZEHN-C
001081 :  ,UJ,                  IZYK
001082 :  SIGNIF:15,XTA,        ARG
001083 :  13,VJM,               AUSGVORZ
001084 :  15,XTA,               ARG
001085 :  ,AMX,
001086 :  8,ATX,                Z-C
001087 :  SIZYK:8,XTA,          Z-C
001088 :  8,A/X,                ZP-C
001089 :  ,NTR,                 3
001090 :  8,A+X,                =I0-C
001091 :  15,ATX,
001092 :  ,NTR,                 2
001093 :  8,A*X,                ZP-C
001094 :  8,X-A,                Z-C
001095 :  8,STX,                Z-C
001096 :  8,AAX,                =17-C
001097 :  8,ARX,                =60-C
001098 :  13,VJM,               PUT
001099 :  8,XTA,                ZP-C
001100 :  8,AEX,                =R1.0-C
001101 :  ,UZA,                 IRK
001102 :  8,XTA,                ZP-C
001103 :  8,A/X,                ZEHN-C
001104 :  8,ATX,                ZP-C
001105 :  ,UJ,                  SIZYK
001106 :  IRK:8,XTA,            =40-C              LEER
001107 :  13,VTM,               RK
001108 :  ,UJ,                  PUT
001109 :  A/RPRI:,ENTRY,
001110 :  RDRU:14,VTM,          RDRU*
001111 :  ,UJ,                  RETTEN
001112 :  RDRU*:8,ARX,          =24-C           ..20 ZEICHEN AUSG
001113 :  13,VTM,               RDRU*
001114 :  ,U1A,                 REKDRU
001115 :  8,ATX,                REK Z -C
001116 :  15,XTA,               ARG
001117 :  13,VJM,               AUSGVORZ
001118 :  15,XTA,               ARG
001119 :  ,AMX,
001120 :  8,ATX,                Z-C
001121 :  8,A-X,                ZHM19-C
001122 :  ,U1A,                 GLEICH0
001123 :  I,VTM,                18
001124 :  8,XTA,                ZH18-C
001125 :  12,VJM,               IZPBEST
001126 :  ,UZA,                 GR1*
001127 :  12,VJM,               SCHIEBEN
001128 :  8,AOX,                =377-C             AUFFUELLEN UND VERGROESSERN
001129 :  8,ATX,                Z-C
001130 :  RAUSG:14,VTM,         1
001131 :  12,VJM,               I14ZIFF
001132 :  8,XTA,                =56-C           ..PUNKT
001133 :  13,VJM,               PUT
001134 :  14,VTM,               11
001135 :  12,VJM,               I14ZIFF
001136 :  ,ITA,                 I
001137 :  ,ASN,                 64-33
001138 : L8,AUX,                =0020 0000 0003 7777-C.
001139 :  ,NTR,                 22B
001140 :  14,VTM,               53B             ..PLUS
001141 :  ,UZA,                 POS
001142 : L8,AOX,                =0017 7777 7774 0000-C
001143 :  14,VTM,               55B             . MINUS
001144 :  POS:8,AOX,            =I0-C
001145 :  ,AMX,
001146 :  8,A/X,                HUNDERT-C
001147 :  8,ATX,                Z-C
001148 :  ,ITA,                 14
001149 :  13,VJM,               PUT
001150 :  12,VJM,               SCHIEBEN
001151 :  14,VTM,               2
001152 :  12,VJM,               I14ZIFF
001153 :  14,VTM,               3
001154 :  12,VTM,               RK
001155 :  ,UJ,                  I14LEER
001156 :  GLEICH0:,XTA,
001157 :  ,ATI,                 I
001158 :  8,ATX,                Z-C
001159 :  ,UJ,                  RAUSG
001160 :  GR1*:I,UTM,           1
001161 : L8,XTA,                =6H'31''231''231''231''231''232'-C.1/10
001162 :  8,ATX,                Z-C
001163 :  ,UJ,                  RAUSG
001164 :  A/BPRI:,ENTRY,
001165 :  BDRU:14,VTM,          BDRU*
001166 :  ,UJ,                  RETTEN
001167 :  BDRU*:8,ARX,          =6-C
001168 :  13,VTM,               BDRU*
001169 :  ,U1A,                 REKDRU
001170 : L8,ARX,                =7777 7777 7777 7771-C.
001171 :  8,ATX,                REKZ-C
001172 :  15,XTA,               ARG
001173 :  11,VTM,               TRUE
001174 :  :,U1A,                *+1
001175 :  11,VTM,               FALSE
001176 :  ,UJ,                  SDRU*
001177 :  A/SPRI:,ENTRY,
001178 :  SDRU:14,VJM,          RETTEN
001179 :  15,XTA,               ARG
001180 :  ,ATI,                 11
001181 :  8,ATX,                STRING-C           ZUM SPEICHERN KURZSTRINGS
001182 : L8,AAX,                =7760 0000 0000 0000-C. TEST OB KURZSTRING
001183 :  :,UZA,                *+1
001184 :  11,VTM,               STRING
001185 :  SDRU*:12,VTM,         48
001186 :  SZYK:11,XTA,
001187 :  12,ASN,               64-8
001188 :  8,AAX,                =377-C
001189 :  ,UZA,                 RK
001190 :  13,VJM,               PUT
001191 :  8,XTA,                REKZ-C
001192 :  8,ARX,                =1-C
001193 :  :,UZA,                *+1
001194 :  13,VJM,               REKDRU
001195 :  8,ATX,                REKZ-C
001196 :  12,UTM,               -8
001197 :  12,V1M,               SZYK
001198 :  11,UTM,               1
001199 :  ,UJ,                  SDRU*
001200 :  A/LPRI:,ENTRY,
001201 :  LDRU:14,VTM,          LDRU*
001202 :  ,UJ,                  RETTEN
001203 :  LDRU*:8,ARX,          =24-C              20 ZEICHEN
001204 :  13,VTM,               LDRU*
001205 :  ,U1A,                 REKDRU             NEUE ZEILE
001206 :  8,ATX,                REKZ-C
001207 :  14,VTM,               -16
001208 :  13,VTM,               LZYK
001209 :  LZYK:15,XTA,          ARG
001210 :  ,ASN,                 64-3
001211 :  15,ATX,               ARG
001212 :  ,YTA,
001213 :  8,ARX,                =60-C
001214 :  14,VLM,               PUT
001215 :  14,VTM,               4
001216 :  12,VTM,               RK
001217 :  ,UJ,                  I14LEER
001218 :  AUSGABE:,BSS,
001219 :  PUT:SO,ASN,           64+8
001220 :  PO,AEX,
001221 :  PO,ATX,
001222 :  SO,UTM,               8
001223 :  13,UTC,
001224 :  SO,V1M,
001225 :  SO,VTM,               -48
001226 :  PO,UTM,               1
001227 :  ,XTA,
001228 :  PO,ATX,
001229 :  13,UJ,
001230 :  RETTEN:,ITS,          8
001231 :  ,ITS,                 9
001232 :  ,ITS,                 10
001233 :  ,ITS,                 11
001234 :  ,ITS,                 12
001235 :  ,ITS,                 13
001236 :  8,VTM,                C
001237 :  8,XTS,                PO*-C
001238 :  ,ATI,                 PO
001239 :  8,XTA,                SO*-C
001240 :  ,ATI,                 SO
001241 :  8,XTA,                REKZ-C
001242 :  14,UJ,
001243 :  RK:,ITA,              PO
001244 :  8,ATX,                PO*-C
001245 :  ,ITA,                 SO
001246 :  8,STX,                SO*- C
001247 :  ,STI,                 13
001248 :  ,STI,                 12
001249 :  ,STI,                 11
001250 :  ,STI,                 10
001251 :  ,STI,                 9
001252 :  ,STI,                 8
001253 :  13,UJ,
001254 :  REKDRU:,ITA,          13
001255 :  14,VTM,               POANF
001256 :  ,ITS,                 14
001257 :  ,ITS,                 PO
001258 :  ,XTS,
001259 :  13,VJM,               PRINT8
001260 :  ,ATI,                 13
001261 :  14,VTM,               POANF
001262 :  ,ITA,                 14
001263 :  ,ATI,                 PO
001264 :  SO,VTM,               -40
001265 : L8,XTA,                =6H '0''0''0''0''0'-C.
001266 :  8,ATX,                POANF-C
001267 :  8,XTA,                M126-C
001268 :  8,ATX,                REKZ-C
001269 :  13,UJ,
001270 :  A/NLINE:,ENTRY,
001271 :  A/NLINE:,BSS,
001272 :  14,VJM,               RETTEN
001273 :  13,VTM,               RK
001274 :  ,UJ,                  REKDRU
001275 :  A/NPAGE:,ENTRY,
001276 :  14,VJM,               RETTEN
001277 :  13,VJM,               REKDRU
001278 : L8,XTA,                =6H1'0''0''0''0''0'-C.
001279 :  8,ATX,                POANF-C
001280 :  ,UJ,                  RK
001281 :  PO*:,Z00,
001282 :  ,Z00,                 POANF
001283 :  SO*:,Z00,
001284 :  ,Z00,                 -40
001285 :  ZEHN:,OCT,            4212            ..KONSTANTEN
001286 :  EINS:,OCT,            405
001287 :  ZHM19:,OCT,           0056 6036 2247 6670
001288 :  ZH12:,OCT,            6416 4324 5121 0000
001289 :  ZH18:,OCT,            7615 7013 3263 5166
001290 :  HUNDERT:,OCT,         4354 3777                   100-D
001291 :  M126:,OCT,            7777 7777 7777 7601
001292 :  TRUE:,ISO,            12HTRUE  '0'
001293 :  FALSE:,ISO,           12HFALSE '0'
001294 :  Z:,OCT,
001295 :  ZP:,OCT,
001296 :  POANF:,ISO,           6H '0''0''0''0''0'
001297 :  ,BSS,                 22
001298 :  REKZ:,OCT,            7777 7777 7777 7601  -126
001299 :  A/I*ISO:,ENTRY,                          INTEGER TO STRING
001300 :  ,UTC,                 PO*
001301 :  ,STX,
001302 :  14,VJM,               RETTEN
001303 :  14,VTM,               IDRU*
001304 :  ,UJ,                  RI*ISO
001305 :  A/R*ISO:,ENTRY,                          REAL TO STRING
001306 :  ,UTC,                 PO*
001307 :  ,STX,
001308 :  14,VJM,               RETTEN
001309 :  14,VTM,               RDRU*
001310 :  RI*ISO:SO,VTM,        -40
001311 :  8,XTA,                =6H '''''''''' -C
001312 :  PO,ATX,
001313 :  8,XTA,                M126-C
001314 :  8,ATX,                REKZ-C
001315 :  14,UJ,
001316 :  STRING:,BSS,          2
001317 :  1,SET,                STOPENTR
001318 :  1,,                   STOPFLG*
001319 :  ,END,
001320 :       A/ERROR    :,NAME,
001321 :       PRINT8*    :,SUBP,
001322 :       STOP*      :,SUBP,
001323 :              7 ,  BASE, *
001324 :                  ,ITA,  9
001325 :                  ,ASN,  37B
001326 :                ,  AUX,   *00052
001327 :                ,  AOX,   *00053
001328 :                ,  ATX,   *00044
001329 :                  ,UTC,   *00045
001330 :              8 ,  XTS,
001331 :             14 ,  VTM,   *00044
001332 :                  ,ITS,  14
001333 :             13 ,  VTM,  STOP*
001334 :                  , UJ,  PRINT8*
001335 :  *00006    :, ISO    ,6HAT '200' S
001336 :             , ISO    ,6HLICE N
001337 :             , ISO    ,6HOT WIT
001338 :             , ISO    ,6HHIN BO
001339 :             , ISO    ,6HUNDS''
001340 :  *00013    :, ISO    ,6HAT '200' I
001341 :             , ISO    ,6HNSUFFI
001342 :             , ISO    ,6HCIENT
001343 :             , ISO    ,6HSTRING
001344 :             , ISO    ,6H MEMOR
001345 :             , ISO    ,6HY''
001346 :  *00021    :, ISO    ,6HAT '200' L
001347 :             , ISO    ,6HOWER B
001348 :             , ISO    ,6HOUND E
001349 :             , ISO    ,6HXCEEDS
001350 :             , ISO    ,6H UPPER
001351 :             , ISO    ,6H BOUND
001352 :             , ISO    ,6H''
001353 :  *00030    :, ISO    ,6HNO MOR
001354 :             , ISO    ,6HE MEMO
001355 :             , ISO    ,6HRY. I1
001356 :             , ISO    ,6H5='200'''
001357 :  *00034    :, ISO    ,6HAT '200' A
001358 :             , ISO    ,6HSSIGNM
001359 :             , ISO    ,6HENT TO
001360 :             , ISO    ,6H UNDEF
001361 :             , ISO    ,6HINED A
001362 :             , ISO    ,6HRRAY E
001363 :             , ISO    ,6HLEMENT
001364 :             , ISO    ,6H''
001365 :  *00044    :     ,ATX,
001366 :                  ,ATX,
001367 :  *00045    :     ,ATX,
001368 :                  , Z00,  *00006
001369 :                  ,ATX,
001370 :                  , Z00,  *00013
001371 :                  ,ATX,
001372 :                  , Z00,  *00021
001373 :                  ,ATX,
001374 :                  , Z00,  *00030
001375 :                  ,ATX,
001376 :                  , Z00,  *00034
001377 :  *00052    :,     LOG,  01603407 01603400.
001378 :  *00053    :,     LOG,  14030060 14030000.
001379 :  BSS       :     ,BSS,
001380 :       ,END,
001381 :       A/GETARR   :,NAME,
001382 :       IOAC*      :,SUBP,
001383 :       A/CALLS    :,SUBP,
001384 :       A/PROCRT   :,SUBP,
001385 :                  ,ATX,  21B
001386 :             14 ,  VJM,  A/CALLS
001387 :                  ,XTA,  21B
001388 :                  ,ASN,  117B
001389 :                  ,ATI,  1
001390 :                  ,ASN,  117B
001391 :                  ,ATI,  2
001392 :                  ,UTC,
001393 :  *00004    : 1 ,  UTM,  77777B
001394 :             13 ,  VJM,  IOAC*
001395 :              2 ,  ATX,
001396 :              2 ,  UTM,  1
001397 :              1 ,  VIM,   *00004
001398 :                  , UJ,  A/PROCRT
001399 :       A/PUTARR  :,ENTRY,
001400 :                  ,ATX,  21B
001401 :             14 ,  VJM,  A/CALLS
001402 :                  ,XTA,  21B
001403 :                  ,ASN,  117B
001404 :                  ,ATI,  1
001405 :                  ,ASN,  117B
001406 :                  ,ATI,  2
001407 :                  ,UTC,
001408 :  *00013    : 2 ,  XTA,
001409 :             13 ,  VJM,  IOAC*
001410 :              1 ,  UTM,  77777B
001411 :              2 ,  UTM,  1
001412 :              1 ,  VIM,   *00013
001413 :                  , UJ,  A/PROCRT
001414 :  BSS       :     ,BSS,
001415 :       ,END,
001416 :  A/GETINC:,NAME,                          LIEFERT EINGABEZEICHEN,UP I13
001417 :  A/RECORD:,LC,         25                 ZERSTOERT I11,I12
001418 :  A/SYMB:,LC,           1
001419 :  D:,EQU,               A/RECORD
001420 :  E:,BSS,
001421 :  11,VTM,               D
001422 :  12,VTM,               E
001423 :  11,XTA,               A/SYMB-D
001424 :  12,AEX,               EOR-E
001425 :  ,UZA,                 NEWREC
001426 :  NEWSYMB:,WTC,         GP
001427 :  ,XTA,
001428 :  12,ASX,               GP-E
001429 : L12,AAX,               =7760 0000 0000 0000-E.
001430 :  12,XTS,               GP-E
001431 : L12,ARX,               =7377 7777 7777 7777-E. -8
001432 :  12,ATX,               GP-E
001433 : L12,ARX,               =6740 0000 0000 0000-E. TEXT OB 16
001434 :  ,UZA,                 ZEILE
001435 :  RK:15,XTA,
001436 :  11,ATX,               A/SYMB-D
001437 :  13,UJ,
001438 : LZEILE:12,ARX,         =4040 0000 0000 0000-E.
001439 :  12,ATX,               GP-E
001440 :  ,UJ,                  RK
001441 :  NEWREC:,ITA,          13
001442 :  ,ITS,                 8
001443 :  ,ITS,                 9
001444 :  ,ITS,                 10
001445 :  ,ITS,                 11
001446 :  ,CALL,                READ*
001447 :  ,STI,                 10
001448 :  ,STI,                 9
001449 :  ,STI,                 8
001450 :  ,ATI,                 13
001451 :  11,VTM,               D
001452 :  12,VTM,               E
001453 :  ,ITA,                 11
001454 :  12,AEX,               B48-E
001455 :  12,ATX,               GP-E
001456 :  ,UJ,                  NEWSYMB
001457 :  B48:,OCT,             4
001458 :  GP:,BSS,              1
001459 :  EOR:,ISO,             6H'12''0''0''0''0''0'
001460 :  1,SET,                EOR
001461 :  1,,                   A/SYMB
001462 :  ,END,
001463 :  A/LHOCHR:,NAME,
001464 :  A/I0:,SUBP,
001465 :  ,UTC,                 =R0.5
001466 :  ,A+X,
001467 :  ,NTR,                 3
001468 :  ,A+X,                 A/I0
001469 :  ,NTR,                 2
001470 :  A/LHOCHI:,ENTRY,
001471 : L,UTC,                 =7777 7777 7777 7777
001472 :  ,AEX,
001473 :  ,STI,                 14
001474 :  14,ASN,               64+1
001475 :  13,UJ,
001476 :  A/NOTL:,ENTRY,
001477 : L,UTC,                 =7777 7777 7777 7777
001478 :  ,AEX,
001479 :  14,UJ,
001480 :  ,END,
001481 :  A/LOADGO:  , NAME,     .ЗAГPYЗЧИK ДЛЯ
001482 : C                      AЛГOЛ-ПPOГPAM
001483 : C    ДИHAMИЧECKИЙ И CTATИЧECKИЙ
001484 : C
001485 :  INFLOAD*:LC,BLOCK,ALOAD,(1)
001486 :  SECENTRY:  , EQU ,15B
001487 :  C:        8,BASE ,C
001488 :  ,UTC,ALOAD
001489 :  ,XTA,
001490 :  ,U1A,CTAT
001491 :    15, UTC ,-8  .ДИHAMИЧ. ЗAГPYЗKA
001492 :            9, VTM ,
001493 :           12, UTC ,8
001494 :           10, VTM ,
001495 :  9, MTJ ,11  .B ИP 11  CBOБ. AДPEC
001496 :  ,XTA ,44B
001497 :             , ASN ,64-18
001498 :             , ASN ,64+33
001499 :             , ATI ,13
001500 :             , ATI ,15
001501 :            8, AEX ,I1-C
001502 :           15, ATX ,
001503 :  15, *67 ,-1     .HA CONT1
001504 :  REACT1  :  , XTA ,17B
001505 :            , AAX ,C77777
001506 :            , AEX ,I2
001507 :             , XTS ,SECENTRY
001508 :             , ASN ,64-24
001509 :           15, AEX ,
001510 :             , ATX ,21B
001511 :             , WTC ,17B
001512 :           15, VTM ,9
001513 :  , *67 ,21B    .INS GEL PROGR
001514 :     ,046,2121B
001515 :  RETN    :11, XTS ,9
001516 :             , ATX ,16B
001517 :           11, XTS ,10
001518 :             , ATX ,17B
001519 :            8, VTM ,C
001520 :            , AAX ,C77777
001521 :            , AEX ,I3
001522 :  11, XTS ,11     .RKADR
001523 :             , ASN ,64-24
001524 :           15, AEX ,
001525 :           11, STX ,
001526 :           11, MTJ ,15
001527 :  11, *67 ,
001528 :  ,046,2121B
001529 :  CONT1   : 9, XTA ,
001530 :  ZYK1    : 9, XTS ,1
001531 :           10, UTM ,-1
001532 :            9, UTM ,1
001533 :           10, V1M ,ZYK1
001534 :             , ITA ,11
001535 :             , ASN ,64-24
001536 :            , AEX ,B11
001537 :           11, ATX ,
001538 :             , XTA ,16B
001539 :           11, ATX ,9
001540 :  , XTA ,17B
001541 :  11, ATX ,10
001542 :  ,ASN,64+15.
001543 :  ,ASN,64-15.
001544 :  , ITS ,11
001545 :  , ARX ,=14
001546 :  15, AEX ,
001547 :  11, ATX ,14
001548 :  ,ARX,=10
001549 :  , ATX ,17B
001550 :  , AAX ,C77777 .ГOШA ЗAБЫЛ !
001551 :  , ATX ,SECENTRY.  И ЭTO TOЖE
001552 :  13, XTA ,1
001553 :  11, ATX ,11
001554 :  , ITA ,11
001555 :  13, ATX ,1
001556 :  , *70 ,46B
001557 :  ,*70,61B .ЗAГPYЗЧИK B ЛИCT OБMEHA
001558 :  11, VTM ,VALUE   .PAR KANN VALUE UND NAME SEIN
001559 :  13, VTM ,NAME   .NAME IN ALTEN STANDARDMASSIVEN
001560 :  , ITA ,8
001561 :  , ITS ,12
001562 :  15, ATX ,
001563 :  15, UJ ,-4    .PROGRAMMNAME HOLEN
001564 :  VALUE :15, XTA ,-3
001565 :  NAME : , BSS ,
001566 :  , ATI ,14
001567 :  , ATX ,21B
001568 :  , ASN ,64+15   .ПPИЗHAK CTPOKИ
001569 :  : , UZA ,*+1
001570 :  14, VTM ,21B
001571 :  15,XTA,
001572 :  ,STI,12
001573 :  ,ATI,8
001574 :  15,UTM,-2
001575 :  14,XTS,
001576 :  ,APX,=6H'137''137''137''137''137''137'
001577 :  , WTC ,43B   .LOADER
001578 :  13, VJM ,2
001579 :  , *75 ,0 .ПPИ ,ATX, -ЗAПИCЬ KAK KOMAHДЫ
001580 :  ,WTC,17B .MAG ZURUECK
001581 :  12, UTC ,7
001582 :  9, VTM ,
001583 :  15, XTA ,
001584 :  ZY2 : 9, UTM ,-1  .MAГAЗИH HAЗAД
001585 :  9, STX ,
001586 :  , UJ ,ZY2  .HA REAKT1
001587 :  I1 : , Z00 ,CONT1
001588 :  1, Z00 ,
001589 :  I2 : , BSS ,
001590 :  I3 : , Z00 ,
001591 :  9, Z00 ,
001592 :  B11 :11, VTM ,
001593 :  , UJ ,RETN
001594 : C=================================
001595 :  CИЛИH:,UJ,RE*67.ЭK *67 Y CИЛИHA ПO NK
001596 :  LOADANGO:,SUBP,.БЫBAET, ЧTO И BPET
001597 : C==================================
001598 :  CTAT:12,UTC,-1  .CTATИЧECKAЯ ЗAГPYЗKA
001599 :  15,XTA,   .ИMЯ PAЗДEЛA
001600 :  ,ATX,NAMEP
001601 :  ,XTA,LOADANGO+21B.ЭK *67 ЗAMEHИM
001602 :  ,ATX,S21LO.KOMAHДOЧKOЙ ПEPEXOДA
001603 :  ,XTA,CИЛИH.KYДA HAДO,A ЭKCTPAKOД
001604 :  ,CTX,LOADANGO+21B.YБEPEM
001605 :   ,XTA,CBOБ
001606 :  ,U1A,STAT
001607 :  ,WTC,17B
001608 :  ,XTA,-1
001609 :  ,ATX,CBOБ  .CBOБOДHЫЙ AДPEC ДЛЯ ЗAГPYЗЧИKA
001610 :  ,XTA,17B
001611 :   ,ATX,CBOБA  .CBOБOДHЫЙ AДPEC ДЛЯ AЛГOЛ-ПPOГP
001612 :  STAT:,ITA,15
001613 :   ,ATX,И15
001614 :  ,WTC,16B           .
001615 :  15,VTM,370B
001616 :   ,XTA,CBOБ
001617 :  ,ATX,17B
001618 :  ,WTC,17B
001619 :  10,VTM,
001620 :  ,ITS,10
001621 :  ,ASN,64-24
001622 :   ,AOX,II2
001623 :  ,WTC,17B
001624 :  10,VTM,10B
001625 :  ,ITS,10
001626 :   ,AEX,A1
001627 :   ,ATX,II1
001628 : C        :  , *67 ,II1.ЗAБИTO ИЗ-ЗA HEBEPHOЙ PAБOTЫ
001629 :  :,UJ,ALOA1 .ЗAГPYЗKA PAЗДEЛA.HA ЭTY KOMAHДY!
001630 :   RE*67:8,VTM,C
001631 :  ,XTA,S21LO.ПOCЛE ПPABKИ HA *67 MOЖHO YБPATЬ    +!
001632 :  ,CTX,LOADANGO+21B.   -"-                              +!
001633 :  ,*75,0
001634 :  15,XTA,
001635 :   ,STX,II3
001636 :  ,ATI,10
001637 :   ,XTA,II3
001638 :  10,ATX,4
001639 :  ,XTA,17B
001640 :   ,ATX,CBOБ
001641 :   ,XTA,CBOБA
001642 :  ,ATX,17B
001643 :  ,WTC,И15
001644 :  15,VTM,
001645 :  15,XTA,-8
001646 :  10,ATX,3   .AДPEC BOЗBPATA
001647 :  10,UTC,4
001648 :  13,VTM,
001649 :  ,ITA,13
001650 :  15,ATX,-8
001651 :  10,UJ,10B   .HA BЫПOЛHEHИE ПPOГPAMMЫ
001652 :  RETNS:11,XTA,1
001653 :  ,ATX,16B
001654 :  11,XTA,2
001655 :  8,VTM,C
001656 :   ,ATX,CBOБ
001657 :   ,XTA,CBOБA
001658 :  ,ATX,17B
001659 :  11,WTC,3
001660 :  ,UJ,
001661 :  II2:11,VTM,
001662 :  ,UJ,RETNS
001663 :  ALOA1: ,XTS,NAMEP
001664 :  ,CALL,LOADGO
001665 :  A1:,Z00,ALOA1
001666 :  ,Z00,
001667 :  C77777:,LOG,77777.
001668 :  CBOБA:,BSS,1.
001669 :  И15:,BSS,1.
001670 :  CBOБ:,BSS,1.
001671 :  NAMEP:,BSS,1.
001672 :  II1:,BSS,1.
001673 :  II3:,BSS,1.
001674 :  S21LO:,BSS,1.ПOCЛE ПPABKИ *67 MOЖHO YБPATЬ +!
001675 :  ,END,
001676 :  A/OPTLIN:,NAME,       ALGOL/ INTERPRETIERENDE ROUTINE       31.1.74
001677 : C  I12: -N+1, N ANZAHL DER WTC-OPTIMIERTEN INDIZIERTEN VARIABLEN,
001678 : C  I14: RKA ERSTER EINSPRUNG,
001679 : C  I13: RKA ZWEITER EINSPRUNG.
001680 : C  ERSTER EINSPRUNG: INDIZIERTE VARIABLEN BEI ANFANGSWERT
001681 : C  ZWEITER EINSPRUNG: INDIZIERTE VARIABLEN BEI ANF-WERT + SCHRITT
001682 : C
001683 :  *C:,UTC,              FIRST
001684 :  ,XTA,
001685 :  ,UZA,                 SECOND             ZWEITER EINSPRUNG
001686 :  12,UJ,                INDEXREG
001687 :  ,UTC,
001688 :  ,ITS,                 5
001689 :  ,UTC,
001690 :  ,ITS,                 4
001691 :  ,UTC,
001692 :  ,ITS,                 3
001693 :  ,UTC,
001694 :  ,ITS,                 2
001695 :  ,UTC,
001696 :  ,ITS,                 8
001697 :  ,UTC,
001698 :  ,ITS,                 9
001699 :  INDEXREG:,UTC,
001700 :  ,ITS,                 10
001701 :  ,XTS,
001702 :  ,UTC,                 FIRST
001703 :  ,ATX,
001704 :  ,NTR,                 3
001705 :  14,UJ,
001706 :  SECOND:,NTR,          3
001707 :  11,VTM,
001708 :  ZYK:11,WTC,           INDEXREG
001709 :  14,VTM,               -30000B
001710 :  14,ITA,
001711 :  15,A-X,               -1
001712 :  14,STX,               20B                SCHRITT
001713 :  14,ATI,
001714 :  11,UTM,               -1
001715 :  12,VLM,               ZYK
001716 :  ,ITA,                 13                 NICHTLEERES WORT IN FIRST
001717 :  ,UTC,                 FIRST
001718 :  ,STX,
001719 :  ,NTR,                 2
001720 :  13,UJ,
001721 :  FIRST:,LOG,           1
001722 :  ,END,
001723 :  A/RAPRI:,NAME,
001724 :  A/RPRI:,SUBP,
001725 :  A/IPRI:,SUBP,
001726 :  A/BPRI:,SUBP,
001727 :  A/SPRI:,SUBP,
001728 :  A/LPRI:,SUBP,
001729 :  12,VTM,               A/RPRI
001730 :  ,UJ,                  APRI
001731 :  A/IAPRI:,ENTRY,
001732 :  12,VTM,               A/IPRI
001733 :  ,UJ,                  APRI
001734 :  A/BAPRI:,ENTRY,
001735 :  12,VTM,               A/BPRI
001736 :  ,UJ,                  APRI
001737 :  A/SAPRI:,ENTRY,
001738 :  12,VTM,               A/SPRI
001739 :  ,UJ,                  APRI
001740 :  A/LAPRI:,ENTRY,
001741 :  12,VTM,               A/LPRI
001742 :  APRI:,ASN,            64+15
001743 :  ,ATI,                 10                 ANZAHL
001744 :  ,ASN,                 64+15
001745 :  ,ATI,                 9                  ANFANG
001746 :  ,ITA,                 13
001747 :  15,ATX,
001748 :  AZY:10,UTM,           -1
001749 :  9,XTA,
001750 :  12,UTC,
001751 :  13,VJM,
001752 :  9,UTM,                1
001753 :  10,V1M,               AZY
001754 :  15,WTC,
001755 :  ,UJ,
001756 :  ,END,
001757 :  A/RAREAD:,NAME,
001758 :  A/CALLS:,SUBP,
001759 :  A/PROCRT:,SUBP,
001760 :  A/RREAD:,SUBP,
001761 :  A/IREAD:,SUBP,
001762 :  A/BREAD:,SUBP,
001763 :  A/SREAD:,SUBP,
001764 :  A/LREAD:,SUBP,
001765 :  12,VTM,               A/RREAD
001766 :  ,UJ,                  AREAD
001767 :  A/IAREAD:,ENTRY,
001768 :  12,VTM,               A/IREAD
001769 :  ,UJ,                  AREAD
001770 :  A/BAREAD:,ENTRY,
001771 :  12,VTM,               A/BREAD
001772 :  ,UJ,                  AREAD
001773 :  A/SAREAD:,ENTRY,
001774 :  12,VTM,               A/SREAD
001775 :  ,UJ,                  AREAD
001776 :  A/LAREAD:,ENTRY,
001777 :  12,VTM,               A/LREAD
001778 :  AREAD:,ATX,           21B
001779 :  14,VJM,               A/CALLS
001780 :  12,MTJ,               3
001781 :  ,XTA,                 21B
001782 :  ,ASN,                 64+15              ANZAHL
001783 :  ,ATI,                 1
001784 :  ,ASN,                 64+15
001785 :  ,ATI,                 2
001786 :  GETZY:3,UTC,                             EINZELWERT LESEN
001787 :  13,VJM,
001788 :  2,ATX,
001789 :  1,UTM,                -1
001790 :  2,UTM,                1
001791 :  1,V1M,                GETZY
001792 :  ,UJ,                  A/PROCRT
001793 :  ,END,
001794 :  A/RREAD:,NAME,                           EINLESEN EINES REELLEN WERTS
001795 :  A/PROCRT:,SUBP,
001796 :  A/RECORD:,LC,         25
001797 :  A/SYMB:,LC,           1
001798 :  ZUS:,EQU,             1
001799 :  A/GETINC:,SUBP,
001800 :  *C:10,VTM,                               ZEICHEN FUER REELL
001801 :  ,UJ,                  NUMBREAD
001802 :  A/IREAD:,ENTRY,                          EINLESEN EINER GANZEN ZAHL
001803 :  10,VTM,               1
001804 :  NUMBREAD:14,CALL,     A/CALLS
001805 :  7,VTM,                *C
001806 :  ,XTA,
001807 :  ,ATI,                 ZUS
001808 :  7,ATX,                E-*C
001809 :  7,ATX,                Z-*C
001810 :  7,XTA,                =R1.0-*C
001811 :  7,ATX,                V-*C
001812 :  ENTSCHL:,UTC,         A/SYMB
001813 :  ,XTA,
001814 :  ,ASN,                 64+44              VORD ST
001815 :  ,ATI,                 2
001816 :  ,YTA,
001817 :  ,ASN,                 64+44
001818 :  15,ATX,
001819 :  ,ASN,                 64-1
001820 :  15,ARX,
001821 :  ,ATI,                 3                  HINT ST * 3
001822 :  ,UTC,                 VERK               KODE-VERKUERZYNG
001823 :  2,XTA,
001824 :  3,ASN,                61
001825 :  7,AAX,                =70-*C
001826 :  ,ATI,                 4                  VERK KODE * 8
001827 : C            ,       , 0=SONST,8=SPACE,16=$,24=.,32=ZZIFFER,40=+/-
001828 :  ,UTC,                 ZUS*ZEI
001829 :  ZUS,XTA,
001830 :  4,ASN,                64
001831 :  7,AAX,                =377-*C
001832 :  ,ATI,                 14
001833 :  14,UJ,                REAKTION
001834 :  REAKTION:ZUS,VTM,     0                  0
001835 :  ,UJ,                  FEHLER
001836 :  :ZUS,VTM,             1                  1
001837 :  ,UJ,                  SETZE V
001838 :  :ZUS,VTM,             2                  2
001839 :  ,UJ,                  Z ZIFF
001840 :  :ZUS,VTM,             3                  3
001841 :  ,UJ,                  WEITER
001842 :  :ZUS,VTM,             5                  4
001843 :  ,UJ,                  SETZE M 1
001844 :  :ZUS,VTM,             0                  5
001845 :  ,UJ,                  WEITER
001846 :  :ZUS,VTM,             2                  L
001847 :  ,UJ,                  ZVERGR
001848 :  :ZUS,VTM,             3                  7
001849 :  ,UJ,                  WEITER
001850 :  :ZUS,VTM,             5                  10
001851 :  ,UJ,                  SETZE M
001852 :  :ZUS,VTM,             0
001853 :  ,UJ,                  FKENDE
001854 :  :ZUS,VTM,             4                  12
001855 :  ,UJ,                  E ERNIEDR
001856 :  :ZUS,VTM,             6                  13
001857 :  ,UJ,                  SETZE V
001858 :  :ZUS,VTM,             7                  14
001859 :  ,UJ,                  Z ZIFF
001860 :  :ZUS,VTM,             7                  15
001861 :  ,UJ,                  ZVERGR
001862 :  :ZUS,VTM,             0                  16
001863 :  ,UJ,                  GLKENDE
001864 :  EERNIEDR:7,XTA,       E-*C
001865 :  7,A-X,                =R1.0-*C
001866 :  7,ATX,                E-*C
001867 :  ZVERGR:7,XTA,         Z-*C
001868 : L7,A-X,                =6417 7777 7777 7777-*C.
001869 :  ,UZA,                 E ERH              ZIFFERN GEBEN NICHTS MEHR
001870 :  7,XTA,                Z-*C
001871 :  ZVERGR*:7,A*X,        =R10.0-*C
001872 :  ,UTC,                 A/SYMB
001873 :  ,XTS,
001874 :  ,ASN,                 64+40
001875 : L7,ARX,                =6377 7777 7777 7720-*C. I0-''0''
001876 :  15,A+X,
001877 :  7,ATX,                Z-*C
001878 :  WEITER:13,VTM,        ENTSCHL
001879 :  ,UJ,                  A/GETINC
001880 :  EERH:7,XTA,           E-*C
001881 :  7,A+X,                =R1.0-*C
001882 :  7,ATX,                E-*C
001883 :  ,UJ,                  WEITER
001884 :  SETZEM1:7,XTA,        =R1.0-*C
001885 :  ,UJ,                  SETZE M+1
001886 :  ZZIFF:,XTA,
001887 :  ,UJ,                  ZVERGR*
001888 :  SETZEV:,UTC,          A/SYMB
001889 :  ,XTA,
001890 : L7,AEX,                =6H-'0''0''0''0''0'-*C.
001891 :  ,U1A,                 WEITER             SCHON AUF +1
001892 :  7,XTA,                =R-1.0-*C
001893 :  SETZEV*:7,ATX,        V-*C
001894 :  ,UJ,                  WEITER
001895 :  SETZEM:7,XTA,         Z-*C               Z*V,E KELLERN
001896 :  :7,A*X,               V-*C
001897 :  7,XTS,                E-*C
001898 :  ,XTS,
001899 :  7,ATX,                E-*C
001900 :  7,XTA,                =R1.0-*C
001901 :  ,UJ,                  SETZE V*
001902 :  GLKENDE:7,XTA,        E-*C               ENDE MIT EXP
001903 :  ,U1A,                 FEHLER
001904 :  7,XTA,                Z-*C
001905 :  7,A*X,                V-*C
001906 :  15,A+X,                                  + GEMERKTES E
001907 :  7,STX,                E-*C               GESAMTEXPONENT
001908 :  ,UJ,                  FKENDE+1
001909 :  FKENDE:7,XTA,         Z-*C
001910 :  7,A*X,                V-*C
001911 :  7,ATX,                Z-*C
001912 :  7,XTA,                E-*C
001913 :  ,UZA,                 RKZAHL             E=0
001914 :  ,NTR,                 22B
001915 :  ,U1A,                 ENEG
001916 :  14,VTM,               =R10.0
001917 :  ZZYK:7,XTA,           Z-*C
001918 :  14,A*X,
001919 :  7,ATX,                Z-*C
001920 :  7,XTA,                E-*C
001921 :  7,A-X,                =R1.0-*C
001922 :  7,ATX,                E-*C
001923 :  ,AEX,
001924 :  ,U1A,                 ZZYK
001925 :  RKZAHL:10,VZM,        KEIN INT
001926 :  7,XTA,                Z-*C
001927 : L7,AMX,                =6417 7777 7777 7777-*C. GROESSTE INT-ZAHL
001928 :  ,UZA,                 FEHLER
001929 :  ,NTR,                 3
001930 :  7,XTA,                Z-*C
001931 :  7,A+X,                =I0-*C
001932 :  7,ATX,                Z-*C
001933 :  KEININT:,NTR,         2
001934 :  7,XTA,                Z-*C
001935 :  ,UJ,                  A/PROCRT           RK WERT IN AC
001936 :  ENEG:,AMX,
001937 :  7,ATX,                E-*C
001938 :  14,VTM,               =R0.1
001939 :  ,UJ,                  ZZYK
001940 :  A/BREAD:,ENTRY,
001941 :  A/BREAD:14,CALL,      A/CALLS
001942 :  7,VTM,                *C
001943 :  BZYK:13,VTM,          BZYK
001944 :  ,UTC,                 A/SYMB
001945 :  ,XTA,
001946 : L7,AEX,                =6H'40''0''0''0''0''0'-*C. TEST LEERZEICHEN
001947 :  ,UZA,                 A/GETINC
001948 : L7,AEX,                =6H'52''0''0''0''0''0'-*C. TEST EOR
001949 :  ,UZA,                 A/GETINC
001950 : L7,AEX,                =6H'114''0''0''0''0''0'-*C. TEST F
001951 :  ,UZA,                 FERTIG
001952 : L7,AEX,                =6H'22''0''0''0''0''0'-* C. TEST T
001953 :  ,U1A,                 FEHLER
001954 : L7,XTA,                =0020 0000 0000 0000-*C.
001955 :  FERTIG:,BSS,
001956 :  7,ATX,                Z-*C
001957 :  13,VTM,               KEININT
001958 :  ,UJ,                  A/GETINC
001959 :  A/LREAD:,ENTRY,
001960 :  14,VJM,               A/CALLS
001961 :  7,VTM,                *C
001962 :  13,VTM,               LZYK
001963 :  LZYK:,UTC,            A/SYMB             SUCHEN ERSTE OKTALZIFFER
001964 :  ,XTA,
001965 :  ,ASN,                 64+43
001966 :  7,AEX,                =6-*C
001967 :  ,U1A,                 A/GETINC
001968 :  ,UTC,                 A/SYMB
001969 :  ,XTA,
001970 :  ,ASN,                 64+40
001971 :  7,AAX,                =7-*C
001972 :  L1ZYK:7,ATX,          Z-*C
001973 :  13,VJM,               A/GETINC
001974 :  ,UTC,                 A/SYMB
001975 :  ,XTA,
001976 :  ,ASN,                 64+43
001977 :  7,AEX,                =6-*C
001978 :  ,U1A,                 LFERT
001979 :  ,UTC,                 A/SYMB
001980 :  ,XTA,
001981 :  ,ASN,                 64+40
001982 :  7,AAX,                =7-*C
001983 :  7,XTS,                Z-*C
001984 :  ,ASN,                 64-3
001985 :  15,AEX,
001986 :  ,UJ,                  L1ZYK
001987 :  LFERT:13,VTM,         KEININT
001988 :  ,UJ,                  A/GETINC
001989 :  FEHLER:14,VTM,        TEXT
001990 :  ,ITS,                 14
001991 :  14,VTM,               A/RECORD
001992 :  ,ITS,                 14
001993 :  ,CALL,                PRINT8*
001994 :  ,074,
001995 :  TEXT:,ISO,             24HERROR IN INPUT CARD:  '200''0'
001996 :  A/RNLINE:,ENTRY,
001997 :  A/RNLINE:,UTC,        =6H'12'''''''''''
001998 :  ,XTA,
001999 :  ,UTC,                 A/SYMB
002000 :  ,ATX,
002001 :  13,UJ,
002002 : C            ,       ,           HINT ST
002003 : C            ,       , 7654 3210 7654 3210
002004 : C            ,       , ---- ---- ---- ----
002005 :  VERK:,OCT,
002006 :  ,OCT,                 0000 0000 2000 0000    2
002007 :  ,OCT,                 0350 5000 0002 0001    4
002008 :  ,OCT,                 0000 0044 4444 4444    6
002009 :  ,OCT,
002010 :  ,OCT,
002011 :  ,OCT,
002012 :  ,OCT,
002013 : C            ,       ,    +- ZIFF  .   $ SPAC RE
002014 : C            ,       , ----------------
002015 :  ZUS*ZEI:,ISO,         6H'01''02''03''04''05''05' VOR SIGN ZEICHEN
002016 :  ,ISO,                 6H'00''02''03''04''00''00'  GLEICH HINTER +-
002017 :  ,ISO,                 6H'00''06''07''10''11''11'  HINTER ZIFFER,VOR . $
002018 :  ,ISO,                 6H'00''12''00''00''00''00'  GLEICH HINTER .
002019 :  ,ISO,                 6H'00''12''00''10''11''11'  WEIT HINTER .
002020 :  ,ISO,                 6H'13''14''00''00''00''00'  GLEICH HINTER $
002021 :  ,ISO,                 6H'00''14''00''00''00''00'  GLEICH HINTER  $ P0-
002022 :  ,ISO,                 6H'00''15''00''00''16''16'  WEIT HINTER $ +/-
002023 :  E:,BSS,               1
002024 :  Z:,BSS,               1
002025 :  V:,BSS,               1
002026 :  ,END,
002027 :  A/SPLUSS:   , NAME  , STRINGVERARBEITUNG MIT LUECKENORGANISATION
002028 :  LHEAP*  :   , LC    , 1
002029 :  LHEAP   :   , WEQ   , LHEAP*
002030 :  A/PROCRT:   , SUBP  ,
002031 :  SYSBACK*:   , SUBP  ,
002032 :  A/GETINC:   , SUBP  ,
002033 :  EOR     :   , EQU   , =6H'12'''''''''''
002034 :  CHECKFL*:   , LC    , 1
002035 :  C       :   , BSS   ,
002036 :  RES     :   , EQU   , 8
002037 :  G       :   , EQU   , 9
002038 :  VG      :   , EQU   , 10
002039 :  ERG     :   , EQU   , 11
002040 :  OP1     :   , EQU   , 9
002041 :  OP2     :   , EQU   , 10
002042 :  SH      :   , EQU   , 11
002043 :  GP      :   , EQU   , 9
002044 :  Z       :   , EQU   , 10
002045 :  L       :   , EQU   , 1
002046 :  KEL     :   , EQU   , 2
002047 :  S       :   , EQU   , 3
002048 :  D       :   , EQU   , 4
002049 :  *HEAP*  :   , LC    , LHEAP
002050 :  LHEAP1  :   , EQU   , LHEAP-1
002051 :  A/SPLUSS:12 , VTM   , C                  STRINGVERKETTUNG
002052 :              , NTR   , 6
002053 :              , ATI   , OP2
002054 :           12 , ATX   , STRING2-C
002055 :              , UZA   , OP1ERG             OP1 IST ERGEBNIS
002056 : L         12 , AAX   , =7760 0000 0000 0000-C
002057 :          :   , UZA   , *+1
002058 :           OP2, VTM   , STRING2
002059 :           15 , XTA   ,
002060 :              , ATI   , OP1
002061 :           12 , ATX   , STRING1-C
002062 : L         12 , AAX   , =7760 0000 0000 0000-C. TEST OB KURZSTRING
002063 :          :   , UZA   , *+1
002064 :           OP1, VTM   , STRING1
002065 :           15 , MTJ   , RES
002066 :  OP1ZYK  :OP1, XTA   ,                    UMSP 1.OPD
002067 :           15 , ATX   ,
002068 :           OP1, UTM   , 1
002069 :           12 , AAX   , =377-C             TEST AUF STRINGENDE
002070 :              , U1A   , OP1ZYK
002071 :           SH , VTM   , -48                SH
002072 :           15 , XTA   , -1
002073 :  SHZYK   :   , UZA   , OP2ZYK
002074 :              , ASN   , 64-8
002075 :           SH , UTM   , 8                  SH:=SH+8
002076 :              , UJ    , SHZYK
002077 :  OP2ZYK  :OP2, XTA   ,                    UMSP 2.OPD
002078 :           SH , ASN   , 64                 WORT GETEILT,INS MAG
002079 :           15 , ATX   ,                    HINT TEIL
002080 :              , YTA   ,
002081 :           15 , AOX   , -2                 VORD TEIL
002082 :           15 , ATX   , -2
002083 :           OP2, XTA   ,
002084 :           12 , AAX   , =377-C             ENDTEST
002085 :           OP2, UTM   , 1
002086 :              , U1A   , OP2ZYK
002087 :              , UJ    , ENDBEARB
002088 :  ,CTX,.MEANINGLESS.TRANSLATED ONLY BY MADLEN-COMPILER
002089 :  ZERL6   :12 , A-X   , =I1-C. UP I14.AC=6*A+B+1.A IN MAG,-8B IN SH
002090 :              , U1A   , SLICERR            GRENZE NEGATIV
002091 : L         12 , A*X   , =4002 5252 5252 5253-C. 1/6 EXP 64
002092 :           15 , ATX   ,                    GANZER TEIL
002093 :              , NTR   , 7                  WEGEN YTA
002094 :              , YTA   ,
002095 :           12 , AOX   , B48-C              REST/6 ALS GLEITKOMMAZAHL
002096 : L         12 , AAX   , =7777 7777 7770 0000-C. WEGEN RUNDUNG
002097 :           12 , A*X   , =I-6 -C            -B DH NEG REST MOD 6
002098 :              , ASN   , 64-3               8*(-B)
002099 :              , ATI   , SH
002100 :           14 , UJ    ,                    RK ZERL 6
002101 :  A/SLICE :   , ENTRY , MAG: INDEX1,INDEX2,ANFANDR DES STRINGS
002102 :           12 , VTM   , C
002103 :           12 , ATX   , STRING1-C
002104 :              , ATI   , GP
002105 : L         12 , AAX   , =7760 0000 0000 0000-C. TEST KURZSTRING
002106 :          :   , UZA   , *+1
002107 :           GP , VTM   , STRING1
002108 :           15 , XTA   ,
002109 :           15 , A-X   ,
002110 :           15 , MTJ   , RES
002111 :              , U1A   , EXIT0              UNTERER INDEX GROESSER
002112 :           12 , ATX   , H-C                MERKE GLEICHHEIT
002113 :           15 , UTM   , 2
002114 :              , NTR   , 23B
002115 :           15 , XTA   , -1
002116 :           14 , VJM   , ZERL6              ZERL OBERER INDEX
002117 :              , XTA   , CHECKFL*
002118 :              , UZA   , NOTEST
002119 :           GP , MTJ   , 14
002120 :           15 , WTC   , -1
002121 :           Z  , VTM   ,
002122 :           Z  , VZM   , CHLAST
002123 :  CHZYK   :14 , XTA   ,                    LESEN BIS OB INDEX
002124 :           12 , AAX   , =377-C
002125 :              , UZA   , SLICERR
002126 :           Z  , UTM   , -1
002127 :           14 , UTM   , 1
002128 :           Z  , V1M   , CHZYK
002129 :  CHLAST  :14 , XTA   ,
002130 :           SH , ASN   , 64
002131 :              , UZA   , SLICERR
002132 :  NOTEST  :12 , XTA   , H-C
002133 :              , U1A   , SLICE2             GRENZEN UNGLEICH
002134 :           15 , WTC   ,
002135 :           GP , XTA   ,
002136 :           15 , UTM   , -2
002137 :           SH , ASN   , 64
002138 : L         12 , AAX   , =7760 0000 0000 0000-C.
002139 :              , NTR   , 2
002140 :           13 , UJ    ,                    RK EINZEICHENSLICE
002141 :  EXIT0   :   , XTA   ,
002142 :           13 , UJ    ,
002143 :  SLICE2  :   , BSS   ,
002144 :              , ITA   , SH
002145 :           12 , X-A   , =100010-C          8-SH
002146 :              , ATI   , SH
002147 : L         12 , XTA   , =7777 7777 7777 7777-C.
002148 :           SH , ASN   , 64
002149 :              , YTA   ,
002150 :           12 , ATX   , LMASK-C            MASKE FUER LETZTES WORT
002151 :           15 , XTA   , -3                 1.INDEX
002152 :           14 , VJM   , ZERL6
002153 :           15 , WTC   , -1
002154 :           GP , UTM   ,                    ADR ERSTES WORT
002155 :           15 , XTA   ,
002156 :           15 , A-X   ,
002157 :              , ATI   , Z                  ANFADR-ENDADR
002158 :           15 , UTM   , -2
002159 :           GP , XTA   ,
002160 :          :Z  , V1M   , *+1
002161 :           12 , AAX   , LMASK-C            BEI LETZTEM WORT
002162 :           SH , ASN   , 64
002163 :           Z  , VZM   , SHORTEX+1          KURZSTRING
002164 :           15 , ATX   ,                    ERSTES WORT DES ERGENISSTRINGS
002165 :  SLIC2ZYK:   , BSS   ,
002166 :           GP , UTM   , 1
002167 :           Z  , UTM   , 1
002168 :           GP , XTA   ,
002169 :          :Z  , V1M   , *+1
002170 :           12 , AAX   , LMASK-C            BEI LETZTEM WORT
002171 :           SH , ASN   , 64
002172 :           15 , ATX   ,                    HINTERER TEIL
002173 :              , YTA   ,
002174 :           15 , AOX   , -2                 VORDERER TEIL
002175 :           15 , ATX   , -2
002176 :           Z  , V1M   , SLIC2ZYK
002177 :              , UJ    , ENDBEARB
002178 :  ATOS    :   , ENTRY , ATOS(ANFADR,ENDADR,STRINGVAR).ALGOLPROCEDURE
002179 :           15 , MTJ   , 6
002180 :           15 , UTM   , 6
002181 :           6  , UTC   ,                    LIEST 1.PAR
002182 :           13 , VJM   ,
002183 :           12 , MTJ   , 4                  ANFADR IN I4
002184 :           6  , UTC   , 2                  LIEST 2.PAR
002185 :           13 , VJM   ,                    ENDADR IN I12
002186 :           15 , MTJ   , RES                ADRESSE DES STRINGS
002187 :  ATOSZYK :4  , XTA   ,                    UMSP DES FELDES INS MAG
002188 :           15 , ATX   ,
002189 :              , ITA   , 4
002190 :              , ITS   , 12
002191 :           15 , AEX   ,                    ENDTEST
002192 :           4  , UTM   , 1
002193 :              , U1A   , ATOSZYK
002194 :           12 , VTM   , C
002195 :           13 , VJM   , ENDBEARB           HEAPADR IN AC
002196 :           15 , ATX   ,
002197 :           6  , UTC   , 4                  LIEST ERGEBNISADR
002198 :           13 , VJM   ,
002199 :           15 , XTA   ,
002200 :           12 , ATX   ,                    WEGSP DER STRINGADR
002201 :  RK      :6  , MTJ   , 15
002202 :              , UJ    , A/PROCRT           RK ATOS/STOA
002203 :  STOA    :   , ENTRY , STOA(ANFADR,ENDADR,STRING)
002204 :           15 , MTJ   , 6
002205 :           15 , UTM   , 6
002206 :           6  , UTC   , 4                  3.PAR
002207 :           13 , VJM   ,
002208 :              , ATI   , 4                  STRINGADR
002209 :           3  , VTM   , C
002210 :           3  , ATX   , STRING1-C
002211 : L         3  , AAX   , =7760 0000 0000 0000-C
002212 :          :   , UZA   , *+1
002213 :           4  , VTM   , STRING1
002214 :           6  , UTC   ,                    1.PAR
002215 :           13 , VJM   ,
002216 :           12 , MTJ   , 5                  ANFADR IN I5
002217 :           6  , UTC   , 2                  2.PAR
002218 :           13 , VJM   ,                    ENDADR IN I12
002219 :  STOAZYK :4  , XTA   ,                    UMSPZYKL IN FELD
002220 :           5  , ATX   ,
002221 :              , ASN   , 64-40
002222 :              , UZA   , S/ENDE             STRING VORZEITIG BEENDET
002223 :           4  , UTM   , 1
002224 :              , ITA   , 5
002225 :           5  , UTM   , 1
002226 :              , ITS   , 12
002227 :           15 , AEX   ,
002228 :              , U1A   , STOAZYK
002229 :              , UJ    , RK
002230 :  S/ENDE  :SH , VTM   , 0                  SH FUER LEERZEICHEN LETZT.WORT
002231 :           5  , XTA   ,
002232 :  FUELLZYK:   , UZA   , FERTIG
002233 :              , ASN   , 64-8
002234 :           SH , UTM   , 8
002235 :              , UJ    , FUELLZYK
002236 :  FERTIG  :   , UTC   , =6H
002237 :              , XTA   ,
002238 :           SH , ASN   , 64
002239 :           5  , AOX   ,
002240 :           5  , ATX   ,                    MIT LEERZ AUFGEFUELLT
002241 :  RESTZYK :   , ITA   , 5                  LERZEICEHN IN RESTFELD
002242 :              , ITS   , 12
002243 :           15 , AEX   ,
002244 :              , UZA   , RK
002245 :              , UTC   , =6H
002246 :              , XTA   ,
002247 :           5  , ATX   , 1
002248 :           5  , UTM   , 1
002249 :              , UJ    , RESTZYK
002250 :  A/SREAD :   , ENTRY ,                    EINGABE EINES STRINGS,UP I13
002251 :           15 , MTJ   , RES
002252 :           13 , MTJ   , 10                 RKADR RETTEN
002253 :  WZYK    :9  , VTM   , -48
002254 :              , XTA   ,
002255 :           15 , ATX   ,
002256 :           13 , VJM   , A/GETINC           LIEST EINGABEZEICHEN
002257 :  SZYK    :12 , VTM   , C
002258 :           12 , AEX   , EOR-C              TEST KARTENENDE
002259 :              , UZA   , END*
002260 :           12 , AEX   , EOR-C
002261 :           9  , ASN   , 64+48
002262 :           15 , AEX   , -1
002263 :           15 , ATX   , -1
002264 :           9  , UTM   , 8
002265 :           9  , V1M   , A/GETINC           RK NACH SZYK
002266 :              , UJ    , WZYK
002267 :  END*    :10 , MTJ   , 13
002268 :              , UJ    , ENDBEARB
002269 :  A/ARGSTR:   , ENTRY , STRINGAUSDRUECKE ALS FORTRANPARAMETER VORVERARB
002270 :           12 , VTM   , C
002271 :           15 , MTJ   , RES
002272 :           15 , ATX   ,
002273 : L         12 , AAX   , =7760 0000 0000 0000-C. TEST OB KURZSTRING
002274 :              , UZA   , OP1ERG             STRING IST SCHON VERWEIS
002275 :              , UJ    , TOHEAP             BRINGT STRING IN HEAP
002276 :  SHORTEX :RES, MTJ   , 15
002277 :           RES, XTA   ,
002278 :              , NTR   , 2
002279 :           13 , UJ    ,                     RK KURZSTRING
002280 :  OP1ERG  :15 , XTA   ,
002281 :           13 , UJ    ,                    RK PLUS FALLS OP2 LEER
002282 :  ENDBEARB:RES, XTA   , 1
002283 :              , UZA   , SHORTEX
002284 :  TOHEAP  :15 , XTA   , -1
002285 :              , U1A   , NONEMPTY
002286 :           15 , UTM   , -1
002287 :              , UJ    , TOHEAP
002288 :  NONEMPTY:12 , AAX   , =377-C
002289 :              , UZA   , *+2
002290 :          :   , XTA   ,
002291 :           15 , ATX   ,
002292 :           G  , VTM   , GAPPTR
002293 :              , NTR   , 3
002294 :              , ITA   , 15
002295 :              , ITS   , RES
002296 :           15 , X-A   ,                    LAANGE DES STRINGS
002297 :              , ASN   , 64-15
002298 :           12 , ATX   , LENG-C             LAENGE,0.
002299 :  GAPZYK  :G  , MTJ   , VG                 VORGAENGER
002300 :           G  , XTA   ,
002301 :              , ATI   , G
002302 :           G  , VZM   , GARCOLL            ENDE GAPLIST ERREICHT
002303 :           G  , XTA   ,
002304 :           12 , A-X   , LENG-C
002305 :              , U1A   , GAPZYK             LUECKE ZU KLEIN
002306 :           G  , XTA   ,
002307 :           12 , AEX   , LENG-C
002308 : L         12 , AAX   , =7777 7777 7770 0000-C.
002309 :              , U1A   , NOCHPL             LUECKE NICHT AUSGEFUELLT
002310 :           G  , XTA   ,
002311 :              , ATI   , 14                 NACHFOLGELUECKE
002312 :              , ITA   , 14
002313 :  VGAEND  :VG , XTS   ,                    I14=NEUER NACHFOLGER
002314 : L         12 , AAX   , =7777 7777 7770 0000-C.
002315 :           15 , AEX   ,
002316 :           VG , ATX   ,
002317 :           RES, MTJ   , 14                 ANFADR UMZUSP.STRING
002318 :           G  , MTJ   , ERG                HEAPADR DES ERG
002319 :  UMSPZYK :14 , XTA   ,                    EINSPEICHERN IN LUECKE
002320 :           G  , ATX   ,
002321 :           14 , UTM   , 1
002322 :           G  , UTM   , 1
002323 :           12 , AAX   , =377-C
002324 :              , U1A   , UMSPZYK
002325 :           RES, MTJ   , 15                 MAGAZIN AUF STRINGANF
002326 :              , ITA   , ERG                ERGEBNISADR IN HEAP
002327 :              , NTR   , 2
002328 :           13 , UJ    ,                    RUECKKEHR
002329 :  NOCHPL  :G  , XTA   ,                    LUECKE VERKUERZT
002330 :           12 , A-X   , LENG-C             LENG-SLENG,LEND
002331 :           12 , XTS   , LENG-C
002332 :              , ASN   , 64+15
002333 :              , ITS   , G
002334 :           15 , ARX   ,                    G+ LENG
002335 :              , STI   , 14                 I14:= BEGINN DER RESTLUECKE
002336 :           14 , ATX   ,                    RESTLUECKE IN HEAP
002337 :              , ITA   , 14
002338 :              , UJ    , VGAEND
002339 :  GAR COLL:   , ITA   , 15                 REGISTER MERKEN
002340 :           12 , ATX   , ENDKEL-C
002341 :              , XTA   ,                    LEERWORT FUER ENDTEST
002342 :              , ITS   , L
002343 :              , ITS   , KEL
002344 :              , ITS   , S
002345 :              , ITS   , D
002346 :           15 , ATX   ,
002347 :              , ITA   , 15
002348 :           12 , ATX   , LLANF-C
002349 :              , XTA   , 16B                MAX SPEICHERBEREICH FUER
002350 :           12 , AAX   , =77777-C           LUECKENFELD
002351 :           12 , ARX   , =370-C
002352 :           12 , ATX   , LLMAX-C            MAXIMALLAENGE
002353 :              , ATI   , 15
002354 :           14 , VTM   , *HEAP*
002355 :              , ITS   , 14
002356 :  LLINIT  :12 , ATX   , MIN-C              LUECKENFELD ANFANGEN
002357 :              , ASN   , 64-15
002358 :           12 , AEX   , HEAPEND-C
002359 :              , WTC   , LLANF
002360 :           L  , VTM   ,
002361 :           L  , ATX   ,                    MIN...HEAPEND ALS LUECKE
002362 :              , XTA   ,
002363 :           L  , ATX   , 1                  0 AM ENDE
002364 :           L  , UTM   , 1
002365 :              , ITA   , L
002366 :           12 , ATX   , LLEND-C            ENDADRESSE
002367 :           12 , XTA   , HEAPEND-C
002368 :           12 , ATX   , MAX-C
002369 :           12 , XTA   , ENDKEL-C
002370 :           12 , ATX   , ENDKEL*-C          ENDKEL* FUER KELLERSTUECK
002371 :              , XTA   , 17B                LETZTER LADEENDPUNKT
002372 :  KELNEU  :   , ATI   , KEL                KELLERSTUECK DURCHMUSTERN
002373 :           KEL, XTA   , 1
002374 :           12 , ATX   , LADEANF-C
002375 :              , WTC   , ENDKEL*            ENDZELLE MERKEN, NULLEN
002376 :           14 , VTM   ,
002377 :           14 , XTA   ,
002378 :           12 , ATX   , ENDZELLE-C
002379 :              , XTA   ,
002380 :           14 , ATX   ,
002381 :  KELZYK  :KEL, UTM   , 1
002382 :           KEL, XTA   ,                    TEST OB HEAPVERWEIS
002383 : L         12 , AAX   , =7777 7777 7770 0000-C
002384 :              , U1A   , KELZYK             KEINE REINE ADRESSE
002385 :           KEL, XTA   ,
002386 :              , UZA   , ENDTEST            LETZTES WORT IST LEER
002387 :           12 , A-X   , MIN-C
002388 :              , U1A   , KELZYK             KLEINER MIN
002389 :           KEL, XTA   ,
002390 :              , ATI   , S
002391 :           12 , X-A   , MAX-C
002392 :              , U1A   , KELZYK             GROESSER MAX
002393 :              , ITA   , S
002394 :              , ASN   , 64-15
002395 :           12 , AEX   , =77777-C           SANF,77777
002396 :           12 , ATX   , SANF-C
002397 :  SEZYK   :S  , XTA   ,                    ENDADR STRING SUCHEN
002398 :           12 , AAX   , =377-C
002399 :           S  , UTM   , 1
002400 :              , U1A   , SEZYK
002401 :           S  , UTM   , -1
002402 :              , ITA   , S
002403 :              , UZA   , KELZYK             KEIN VERWEIS
002404 :           12 , ATX   , SEND-C
002405 :           12 , XTA   , LLANF-C            NAECHSTNIEDERE LUECKE SUCHEN
002406 :              , ATI   , L
002407 :           12 , X-A   , LLEND-C
002408 :              , ATI   , D
002409 :  LSUCHZYK:   , ITA   , D                  D:= D/2
002410 :              , ASN   , 64+1
002411 :              , ATI   , D
002412 :              , ATI   , 14
002413 :              , YTA   ,
002414 :           D  , VZM   , LGEF
002415 :          :   , UZA   , *+1
002416 :           D  , UTM   , 1                  D GERUNDET
002417 :           12 , XTA   , SANF-C
002418 :           D  , UTC   ,
002419 :           L  , A-X   ,
002420 :              , U1A   , LSUCHZYK           SANF LT LUECKE(L+D)
002421 :           14 , UTC   ,
002422 :           L  , UTM   ,                    L:= D+L
002423 :              , UJ    , LSUCHZYK
002424 :  LGEF    :L  , XTA   ,
002425 :           12 , AAX   , =77777-C
002426 :           12 , ATX   , LEND-C
002427 :   12,XTS,SANF-C
002428 :   ,ASN,64+15
002429 :   15,X-A,
002430 :   ,U1A,KELZYK.JUMP IF THE STRING STARTS AFTER THE GAP
002431 :   12,XTA,SANF-C
002432 :   L,A-X,
002433 :   ,U1A,KELZYK.JUMP IF THE STRING STARTS BEFORE THE GAP
002434 :           12 , XTA   , SANF-C
002435 :           L  , AEX   ,
002436 : L         12 , AAX   , =7777 7777 7770 0000-C.
002437 :              , U1A   , NICHTANF
002438 :           12 , XTA   , LEND-C             STRING AM LUECKENANFANG
002439 :   12,X-A,SEND-C.DOES THE STRING EXTEND UP TO THE END OF GAP
002440 :              , UZA   , LWEG               FUELLT LUECKE AUS
002441 :           12 , XTA   , SEND-C             LUECKE VORN VERKUERZT
002442 :           12 , ARX   , =1-C
002443 :              , ASN   , 64-15
002444 :           12 , AEX   , LEND-C
002445 :           L  , ATX   ,
002446 :              , UJ    , KELZYK
002447 :  LWEG    :L  , XTA   , 1                  LUECKE HERAUSGENOMMEN
002448 :           L  , AT X  ,                    LUECKE(L):=LUECKE(L+1)
002449 :           L  , UTM   , 1
002450 :              , U1A   , LWEG               ENDTEST
002451 :           12 , XTA   , LLEND-C
002452 :           12 , A-X   , =1-C
002453 :           12 , ATX   , LLEND-C            LLEND:=LLEND-1
002454 :              , UJ    , KELZYK
002455 :  NICHTANF:12 , XTA   , SANF-C
002456 :              , ASN   , 64+15
002457 :           12 , A-X   , =1-C
002458 :           L  , XTS   ,
002459 : L         12 , AAX   , =7777 7777 7770 0000-C.
002460 :           15 , AEX   ,
002461 :           L  , ATX   ,                    VORDERER TEIL D.LUECKE
002462 :           12 , XTA   , SEND-C
002463 :   12,A-X,LEND-C.DOES THE STRING EXTEND UP TO THE END OF GAP
002464 :              , UZA   , KELZYK             STRING AM LUE KENENDE
002465 :           12 , XTA   , SEND-C             LUECKE HINTERER TEIL
002466 :           12 , ARX   , =1-C
002467 :              , ASN   , 64-15
002468 :           12 , AEX   , LEND-C             SEND+1,LWND
002469 :  LVERLZYK:12 , ATX   , H-C                H:= AC
002470 :           L  , XTA   ,   1                MAG:=LUECKE(L)
002471 :           12 , XTS   , H-C                LUECKE(L):= H
002472 :           L  , STX   ,  1                 AC:= MAG
002473 :           L  , UTM   , 1                  L:= L+1
002474 :              , U1A   , LVERLZYK
002475 :           L  , ATX   ,     1
002476 :           12 , XTA   , LLEND-C
002477 :           12 , ARX   , =1-C
002478 :           12 , ATX   , LLEND-C            LLEND:= LLEND+1
002479 :           12 , AEX   , LLMAX-C
002480 :              , U1A   , KELZYK             KEIN UEBERLAUF
002481 :           12 , XTA   , LLANF-C            MAX AENDERN
002482 :           12 , ARX   , LLEND-C
002483 :              , ASN   , 64+1
002484 :              , ATI   , L                  L:=(LLANF+LLEND)/2
002485 :           L  , XTA   ,
002486 :           12 , AAX   , =77777-C
002487 :           12 , ATX   , MAX-C              MAX:= LANF(L)
002488 :              , XTA   ,
002489 :           L  , ATX   , 1                  LUECKE(L+1):=0
002490 :              , ITA   , L
002491 :           12 , ARX   , =1-C
002492 :           12 , ATX   , LLEND-C            LLEND:= L+1
002493 :              , UJ    , KELZYK
002494 :  ENDTEST :   , ITA   , KEL
002495 :           12 , AEX   , ENDKEL*-C          OB KELLERSTUECK BEENDET
002496 :              , U1A   , KELZYK
002497 :           12 , XTA   , ENDZELLE-C
002498 :           KEL, ATX   ,                    ENDZELLE ZURUECK
002499 :           12 , XTA   , LADEANF-C
002500 :              , ATI   , 14
002501 :           12 , ATX   , ENDKEL*-C
002502 :           14 , XTA   , 10                 17B ALT
002503 :           14 , V1M   , KELNEU             NOCH NICHT UNTEN
002504 :              , XTA   ,
002505 :           12 , ATX   , MAXL-C             MAXL:=0 / MAXIMALE LUECKE
002506 :           12 , XTA   , LLANF-C
002507 :              , ATI   , L                  L:= LLANF
002508 :           12 , XTA   , MIN-C
002509 :              , ATI   , 14
002510 :           14 , UTM   , -*HEAP*
002511 :           14 , V1M   , NOTFIRST
002512 :           L  , XTA   ,
002513 :              , ASN   , 64+15              ADRESSE ERSTE LUECKE
002514 :           12 , ATX   , GAPPTR -C
002515 :              , UJ    , ATIG
002516 :  NOTFIRST:12 , XTA   , GAPPTR* -C
002517 :              , ATI   , G                  G:= GAPPTR*
002518 :           L  , XTA   ,                    ERZEUGEN DER LUECKEN IN HEAP
002519 :  GHZYK   :   , ASN   , 64+15
002520 :           G  , AOX   ,
002521 :           G  , ATX   ,
002522 :  ATIG    :   , ATI   , G
002523 :              , ITA   , G
002524 :           L  , XTS   ,
002525 :           12 , AAX   , =77777-C
002526 :           12 , ARX   , =1- C
002527 :           15 , A-X   ,                    LAENGE DER LUECKE
002528 :              , ASN   , 64-15
002529 :           G  , ATX   ,                    HEAP(G):= LENG,0
002530 :           12 , A-X   , MAXL-C
002531 :              , U1A   , *+2                MAXL GE LENG
002532 :          :G  , XTA   ,
002533 :           12 , ATX   , MAXL-C             MAXL:= LENG
002534 :           L  , UTM   , 1
002535 :           L  , XTA   ,
002536 :              , U1A   , GHZYK
002537 :           12 , XTA   , MAX-C
002538 :           12 , AEX   , HEAPEND-C
002539 :              , U1A   , DALEJ              KELLER NOCHMALS DURCHMUSTERN
002540 :           12 , XTA   , LLANF-C            RUECKSETZEN MAG
002541 :              , ATI   , 15
002542 :           12 , XTA   , MAXL-C
002543 :           12 , A-X   , LENG-C
002544 :              , U1A   , KEINPL
002545 :           15 , XTA   ,                    RUECKSTELLEN REGISTER
002546 :              , STI   , D
002547 :              , STI   , S
002548 :              , STI   , KEL
002549 :              , STI   , L                  I15 HINTER EIN ZUTR.STRING
002550 :              , UJ    , TOHEAP
002551 :  DALEJ   :   , ITA   , G
002552 :           12 , ATX   , GAPPTR*-C          LETZTE HEAPLUECKE
002553 :           12 , XTA   , MAX-C
002554 :           12 , ARX   , =1-C
002555 :              , UJ    , LLINIT
002556 :  SLICERR :8  , VTM   , 0                  . ''SLICE NOT WITHIN BOUNDS''
002557 :              , UJ    , KEINPL+1
002558 :  KEINPL  :8  , VTM   , 1                  FEHLERABBRUCH KEIN PLATZ
002559 :           13 , MTJ   , 9
002560 :           12 , VTM   , A/ERROR
002561 :              , UJ    , SYSBACK*
002562 :  HEAPEND :   , Z00   ,
002563 :              , Z00   , *HEAP*+LHEAP1
002564 :  A/ERROR :   , 6TEXT , ''A/ERROR ''
002565 :  B48     :   , OCT   , 4000 0000 0000 0000
002566 :  H       :   , BSS   , 1
002567 :  LMASK   :   , BSS   , 1
002568 :  LENG    :   , BSS   , 1
002569 :  LLANF   :   , BSS   , 1
002570 :  LLEND   :   , BSS   , 1
002571 :  LLMAX   :   , BSS   , 1
002572 :  MAX     :   , BSS   , 1
002573 :  MIN     :   , BSS   , 1
002574 :  GAPPTR  :   , BSS   , 1
002575 :  GAPPTR* :   , BSS   , 1
002576 :  ENDKEL  :   , BSS   , 1
002577 :  MAXL    :   , BSS   , 1
002578 :  SEND    :   , BSS   , 1
002579 :  LEND    :   , BSS   , 1
002580 :  SANF    :   , BSS   , 1
002581 :  LADEANF :   , BSS   , 1
002582 :  ENDKEL* :   , BSS   , 1
002583 :  ENDZELLE:   , BSS   , 1
002584 :  STRING1 :   , BSS   , 2
002585 :  STRING2 :   , BSS   , 2
002586 :  ,END,
002587 :  A/STOP:,NAME,
002588 :  LCAT:,EQU,1100
002589 :  INFLOAD*:LC,BLOCK,ALOAD,(1)
002590 :  MAIN*:LC,BLOCK,MAIN
002591 :  /VERE/:LC,BLOCK,(4),LIB0
002592 :  RLEX:,EQU,46B
002593 :  WLEX:,EQU,45B
002594 :  LEX:,EQU,43B
002595 :  AHAЧ:,EQU,15
002596 :  STOP*:,SUBP,
002597 :  I/O :,WEQ,LEX.
002598 :  NPI/O:,P/P,(I/O)16
002599 :  A/ARRD/K:,SUBP,
002600 :  A/DRUCK:,SUBP,
002601 :  STOPFLG*:,LC,1
002602 :  C:,BSS,
002603 :  STOP:  ,WTC,STOPFLG*
002604 :  14,VTM,.
002605 :  14,VZM,STOP*.
002606 :   14,UTC,
002607 :  13,VJM,
002608 :  ,UJ,STOP*
002609 :  A/START:,ENTRY,
002610 :  8,BASE,C
002611 :  ,*75,0
002612 :  ,XTA,44B
002613 :  ,AOX,=2000 0000 0000 0000
002614 :  ,ATX,44B
002615 : C
002616 : C   47P=1 B 44B - ПPИЗHAK ALGOL-ПPOГPAMMЫ
002617 : C
002618 :  ,UTC,ALOAD
002619 :  ,XTA,
002620 :  ,UZA,ЗAГPДИH
002621 :  ,UJ,RCAT
002622 :  ASTAT:,XTA,17B
002623 :  ,XTS,17B
002624 :  ,ASN,64+15
002625 :  ,ASN,64-15
002626 :  ,ATX,17B
002627 :  ,ITA,15
002628 :  ,AOX,17B
002629 :  ,ATX,17B
002630 :  ,UJ,BЫППPOГP
002631 : C
002632 : C    CTATИЧECKAЯ ЗAГPYЗKA
002633 : C
002634 :  RCAT:,*70,RLEX
002635 : C
002636 : C  ФOPMИPOB. ИHФ. CЛOBA ДЛЯ ЧTEHИЯ KATAЛOГA OVERLAY
002637 : C
002638 :  ,XTA,CWRD.
002639 :  ,AOX,LIB0.
002640 :  ,ATX,ДЛИHA.
002641 :  ,*70,ДЛИHA
002642 :  9,VTM,-LCAT
002643 :  ,XTA,
002644 :  ,ATX,ДЛИHA
002645 : C
002646 : C  OПPEДEЛEHИE AДPECA HAЧAЛA ПOЛЯ MACCИBOB И ПEPEMEHHЫX
002647 : C
002648 :  ПOИCK:,WTC,43B
002649 :  9,UTC,LCAT
002650 :  10,VTM,
002651 :  10,XTA,
002652 :  ,UZA,ENDCAT
002653 :  ,ITA,10
002654 :  ,AAX,=1
002655 :  ,UZA,ПOИCK1
002656 :  10,XTA,
002657 :  ,ATI,AHAЧ
002658 :  ,ASN,64+15
002659 :  ,ATI,11
002660 :  11,J+M,AHAЧ
002661 :  ,ITA,AHAЧ
002662 :  ,AEX,BCEEД
002663 :  ,ARX,ДЛИHA
002664 :  ,U1A,ПOИCK1
002665 :  ,ITA,AHAЧ
002666 :  ,ATX,ДЛИHA
002667 :  ПOИCK1:9,VLM,ПOИCK
002668 :  10,VTM,26350B
002669 :  ,UJ,STOP*
002670 :  ЗAГPДИH:,XTA,17B
002671 :  ,ATI,15
002672 :  BЫППPOГP:,ITA,15
002673 :  ,ITS,13
002674 :  ,ASN,64-24
002675 :  15,AOX,-1
002676 :  1,VTM,I67
002677 :  15,UTM,1
002678 :  15,MTJ,6
002679 :  1,AOX,
002680 :  1,ATX,
002681 :  1,*67,
002682 :  ,UJ,STOP
002683 :  ENDCAT:,WTC,ДЛИHA
002684 :  15,VTM,11B
002685 :  ,*70,WLEX
002686 :  ,UJ,ASTAT
002687 :  CWRD:,010,NPI/O.
002688 :  ,   ,          .
002689 :  I67:,   ,
002690 :  9,   , .
002691 :  BCEEД:,LOG,7777 7777 7777 7777
002692 :  П77777:,LOG,77777
002693 :  ДЛИHA:,BSS,1.
002694 :  ,END,
002695 :  A/SUBCH:,NAME,        ALGOL/ INTERPRETIERENDE ROUTINE     3.1.74
002696 : C  TESTET INDEXUEBERSCHREITUNG BEI FELDELEMENTEN
002697 : C
002698 :  SYSBACK*:,SUBP,.
002699 :  ,ASN,                 64+15              INDEX IN I12,FW IN AC
002700 :  ,ATI,                 14
002701 :  ,ASN,                 64+15
002702 :  ,ITS,                 12
002703 :  ,NTR,                 3
002704 :  15,A-X,               -1
002705 :  ,U1A,                 FEHLER
002706 :  ,ITA,                 14
002707 :  15,A+X,
002708 :  ,ITS,                 12
002709 :  15,A-X,
002710 :  ,NTR,                 22B
002711 :  13,U1A,
002712 :  FEHLER:8,VTM,         4                  AT 'I13' ASSIGNMENT TO UNDE
002713 :  13,MTJ,               9                  FINED ARRAY ELEMENT
002714 :  12,VTM,A/ERR.
002715 :  ,UJ,SYSBACK*.
002716 :  A/ERR:,TEXT,8HA/ERROR
002717 :  ,END,
002718 :  A/VLM1  :   , NAME  ,
002719 :              , NTR   , 3
002720 :           15 , X-A   ,                    INIT-END
002721 :              ,  ATI  , 11
002722 :              , NTR   , 22B
002723 :           13 , U1A   ,                    FALLS INIT-END 'LT' 0
002724 :              , ASN   , 64-7
002725 :           13 , UZA   ,                    FALLS INIT = END
002726 :           14 , UJ    ,                    FALLS INIT 'GT' END
002727 :  ,END,
002728 :  A/VLMX  :   , NAME  ,
002729 :  A/I0    :   , LC    , 1
002730 :              , AOX   , A/I0               EVL REINE ADR
002731 :              , A+X   ,                    SCHRITT NORM.
002732 :              , STX   , 21B
002733 :           15 , A-X   ,
002734 :              , A/X   , 21B                (E-A)/S
002735 :              , NTR   , 22B
002736 :           14 , U1A   ,
002737 :              , NTR   , 3
002738 :              , A+X   , A/I0               RUNDUNG
002739 :              , X-A   , A/I0               NEGIERUNG
002740 :              , ATI   , 11                 I11=ZAEHLREGISTER MIT VLM
002741 :              , NTR   , 2
002742 :           13 , UJ    ,                    RUECKSPRUNG
002743 :  ,END,
002744 :  ALPROC:,NAME,         KOPPLUNG FORTRAN-ALGOL
002745 : C-----------------------------------------------------------------------
002746 : C  BEISPIEL EINES AUFRUFS
002747 : C    CALL ALPROC(P,'VAR' , X,'CONST', 1.3, 'PROC',P1,
002748 : C               'ARRAY1',V,10,'ARRAY2',W,10,10,5)
002749 : C    ENTSPRICHT DEM ALGOLAUFRUF  P(X,1.3,P1,V,W)
002750 : C    ERSTER PAR = AUFGER PROC,MUSS IN EXTERNAL STEHEN
002751 : C    LETZTER PAR = ANZAHL DER PAR DER ALGOLPROC
002752 : C-----------------------------------------------------------------------
002753 :  A/CALLS:,SUBP,
002754 :  N:,EQU,               11
002755 :  PAR:,EQU,             10
002756 :  8,BASE,               *
002757 : C**** PAR-ZELLEN IN PARFELD FUELLEN, SONDERBEH 2-DIM FELDER
002758 :  ,ATI,                 14
002759 :  14,XTA,
002760 :  ,ATI,                 14                 ANZAHL DER PAR
002761 :  PAR,VTM,              PARFELD+62
002762 :  ,XTA,
002763 :  ,ATX,                 LARR2              LISTE FUER ADRESSBUCHHERST
002764 :  PARZYK:14,VZM,        PARERL
002765 :  15,WTC,               -2
002766 :  ,XTA,
002767 :  ,AEX,                 =6HVAR
002768 :  12,VTM,               VAR*
002769 :  ,UZA,                 VACOPR
002770 : L,AEX,                 =6H'25''16''34''163''164''0'. CONST (+)  VAR
002771 :  12,VTM,               CONST*
002772 :  ,UZA,                 VACOPR
002773 : L,AEX,                 =6H'23''35''1''20''164''0'.   PROC (+)  CONST
002774 :  12,VTM,               PROC*
002775 :  ,UZA,                 VACOPR*
002776 :  15,WTC,               -3
002777 :  ,XTA,
002778 :  ,AEX,                 =6HARRAY1
002779 :  ,UZA,                 ARRAY1
002780 :  15,WTC,               -4
002781 :  ,XTA,
002782 :  ,AEX,                 =6HARRAY2
002783 :  ,UZA,                 ARRAY2
002784 :  ,UJ,                  FEHLER             KEINE ENTSCHLUESSELUNG
002785 :  VACOPR*:15,WTC,       -1
002786 :  ,XTA,
002787 :  ,AEX,                 B15MTJ             TEST OB PROC-ANF
002788 : L,AAX,                 =7777 7760 0000 0000
002789 :  ,U1A,                 FEHLER             KEIN PROC-ANF
002790 :  VACOPR:15,XTA,
002791 :  12,AEX,                                  VAR* CONST* PROC*
002792 :  PAR,ATX,
002793 :  12,XTA,               1
002794 :  PAR,STX,              1
002795 :  PAREND:14,UTM,        -1
002796 :  PAR,UTM,              -2
002797 :  ,UJ,                  PARZYK
002798 :  DIMPR:15,WTC,                            UP I12 PRUEFT OB ADR IM KELLER
002799 :  ,XTA,                                    GANZE ZAHL FUEHRT
002800 :  ,ATI,                 N
002801 :  ,AEX,                 =I0
002802 :  ,ASN,                 64+15
002803 :  ,U1A,                 FEHLER             FALSCHE DIM-GROESSE
002804 :  ,ITA,                 N
002805 :  12,UJ,
002806 :  ARRAY1:12,VJM,        DIMPR
002807 :  15,XTA,               -1                 LEITADR
002808 :  ,ASN,                 64-30
002809 :  15,AEX,
002810 : L,ARX,                 =7777 7777 7777 7776. ADR,0,ADR-1
002811 :  ,ITS,                 N
002812 :  ,ASN,                 64-15
002813 :  15,AEX,
002814 :  PAR,STX,                                 ADR,N,ADR-1
002815 :  ,UJ,                  PAREND
002816 :  ARRAY2:12,VJM,        DIMPR
002817 :  PAR,ATX,              1
002818 :  12,VJM,               DIMPR
002819 :  ,ASN,                 64-15
002820 :  PAR,AEX,              1
002821 :  PAR,ATX,              1
002822 :  ,XTA,                 LARR2
002823 :  ,ASN,                 64-30
002824 :  PAR,AEX,              1
002825 :  PAR,STX,              1                  NEXT,M,N
002826 :  ,ASN,                 64-30
002827 :  PAR,ATX,                                 ADR,00,0
002828 :  ,ITA,                 PAR
002829 :  ,ATX,                 LARR2
002830 :  15,WTC,               1
002831 :  ,XTA,
002832 :  15,WTC,               2
002833 :  ,A*X,                                    M*N
002834 :  ,NTR,                 3
002835 :  ,A+X,                =I0
002836 :  ,NTR,                 2
002837 :  ,ASN,                 64-15
002838 :  ,UZA,                 FEHLER             M*N=0
002839 :  PAR,AEX,
002840 :  PAR,STX,                                 ADR,M*N,0
002841 :  ,UJ,                  PAREND
002842 :  PARERL:15,XTA,
002843 :  PAR,STX,                                 AUFGER PROC IN PAR
002844 :  ,ITA,                 15
002845 :  ,ASN,                 64-24
002846 :  ,ITS,                 13
002847 :  15,AEX,
002848 :  ,AEX,                 B15VTMUJ
002849 :  15,MTJ,               13                 NEUE RUECKKEHRADR
002850 :  15,ATX,
002851 :  ,XTA,                 44B                TEST OB ALGOLREG
002852 : L,AAX,                 =2000 0000 0000 0000.
002853 :  ,U1A,                 *+3
002854 :  :,WTC,                17B
002855 :  15,VTM,               1
002856 :  ,XTA,                                    WEGEN A/SPLUSS
002857 :  15,ATX,
002858 : C**** ADRESSBUECHER IM KELLER ERZEUGEN
002859 :  ,WTC,                 LARR2
002860 :  14,VTM,
002861 :  ADRBUCH:14,VZM,       ADBERL
002862 :  ,ITA,                 15
002863 :  14,AEX,
002864 : L,ARX,                 =7777 7777 7777 7776.
002865 :  14,ATX,                                  ADR,M*N,KEL-1
002866 :  14,XTA,               1
002867 :  ,ATI,                 N
002868 :  ,ASN,                 64+15
002869 :  ,AAX,                 =77777
002870 :  PAR,ATX,              -1                  M
002871 :  14,XTA,
002872 :  ,ASN,                 64+30              ADR
002873 : L,ARX,                 =7777 7777 7767 7777.  7...7,ADR-1
002874 :  ADBZYK:15,ATX,
002875 :  PAR,ARX,              -1                 +M
002876 :  N,UTM,                -1
002877 :  N,V1M,                ADBZYK
002878 :  14,XTA,               1
002879 :  ,ASN,                 64+30              NEXT IN I14
002880 :  ,ATI,                 14
002881 :  ,UJ,                  ADRBUCH
002882 : C**** PARAMETER IN DEN KELLER
002883 :  ADBERL:15,MTJ,        12
002884 :  15,UTM,               8
002885 :  PAR,XTA,
002886 :  ,ATI,                 14                 AUFGER PROC
002887 :  PARMAG:,ITA,          PAR
002888 :  ,AEX,                 PARFELD*           UMSP PAR IN MAG
002889 :  ,UZA,                 ENDE
002890 :  PAR,XTA,              2
002891 :  PAR,XTS,              3
002892 :  15,ATX,
002893 :  PAR,UTM,              2
002894 :  ,UJ,                  PARMAG
002895 :  ENDE:14,XTA,
002896 :  ,AEX,                 B15MTJ
002897 : L,AAX,                 =7777 7760 0000 0000.
002898 :  ,U1A,                 FEHLER             AUFGER PROC FALSCH
002899 :  12,MTJ,               15
002900 :  ,UJ,                  A/CALLS
002901 :  FEHLER:14,VTM,        T
002902 :  ,ITS,                 14
002903 :  14,VTM,               T+1
002904 :  ,ITS,                 14
002905 :  ,XTS,
002906 :  ,CALL,                PRINT8
002907 :  ,CALL,                STOP*
002908 :  VAR*:,UTC,
002909 :  12,VTM,                                  STELLE DER ADR
002910 :  12,XTA,
002911 :  13,UJ,
002912 :  CONST*:12,VTM,        66666B
002913 :  ,UTC,                                    STELLE DER ADR
002914 :  ,XTA,
002915 :  13,UJ,
002916 :  PROC*:,UTC,
002917 :  14,VTM,                                  ANFADR
002918 :  ,UJ,                  A/CALLS
002919 :  ,,
002920 :  B15VTMUJ:15,VTM,                         MAG NACH RUECKKEHR
002921 :  ,UJ,                                     RUECKKEHRSTELLE
002922 :  PARFELD*:,Z00,
002923 :  ,Z00,                 PARFELD+62
002924 :  T:,ISO,               12HERROR ALPROC
002925 :  B15MTJ:15,MTJ,                           ZUM TESTEN EINES PROC-ANF
002926 :  ,UTC,
002927 :  PARFELD:,BSS,         64
002928 :  LARR2:,BSS,           1
002929 :  ,END,
002930 :  AMAX0:,NAME,.BEPETEHOB
002931 :  R*TO*I:,SUBP,.
002932 :  AMAX1:,ENTRY,.
002933 :  ,UTC,NORM-R*TO*I.
002934 :  MAX0:,ENTRY,.
002935 :  MAX1:,ENTRY,.
002936 :  12,VTM,R*TO*I.
002937 :  10,VTM,.
002938 :  *2: ,ATI,14
002939 :  ,NTR,22B.
002940 :  *3:,STX,.
002941 :  ,ATI,11.
002942 :  ,ARX,.
002943 :  ,UZA,*10.BCE
002944 :  14,XTA,.
002945 :  11,A-X,.
002946 :  10,AVX,.
002947 :  ,UZA,*3.
002948 :  11,MTJ,14.
002949 :  ,UJ,*3.
002950 :  *10:14,XTA,.
002951 :  11,A-X,.
002952 :  10,AVX,.
002953 :  ,UZA,*13.
002954 :  11,XTA,.
002955 :  12,UJ,.
002956 :  *13:14,XTA,.
002957 :  12,UJ,.
002958 :  NORM:,E+N,64.
002959 :  13,UJ,.
002960 :  AMIN0:,ENTRY,.
002961 :  AMIN1:,ENTRY,.
002962 :  ,UTC,NORM-R*TO*I.
002963 :  MIN0:,ENTRY,.
002964 :  MIN1:,ENTRY,.
002965 :  12,VTM,R*TO*I.
002966 :  10,VJM,*2.
002967 :  ,OCT,002
002968 :  ,END,AMAX0   .
002969 :  AMOD:,NAME,.ФИAЭ, TPOИЦK
002970 :  A/I0:,LC,1
002971 :  AMOD:15,WTC,
002972 :  ,XTS,
002973 :  ,NTR,2
002974 :  ,E+N,64.
002975 :  15,WTC,.
002976 :  ,XTS,
002977 :  ,E+N,64.B
002978 :  15,ATX,.
002979 :  15,XTS,-3
002980 :  15,A/X,.A/B
002981 :  15,ATX,.
002982 :  ,NTR,3
002983 :  ,AMX,A/I0.
002984 :  15,AVX,.
002985 :  ,NTR,2
002986 :  15,A*X,.
002987 :  15,X-A,
002988 :  13,UJ,.
002989 :  MOD:,ENTRY,
002990 :  13,MTJ,14
002991 :  13,VJM,AMOD
002992 :  ,NTR,3
002993 :  15,ATX,
002994 :  ,AMX,A/I0
002995 :  15,AVX,
002996 :  ,NTR,6.
002997 :  14,UJ,
002998 :  ,END,AMOD
002999 :       ATAN2       :,NAME,
003000 :       FT*571      :,SUBP,
003001 :       FT*003      :,SUBP,
003002 :       SIGN        :,SUBP,
003003 :       ATAN        :,SUBP,
003004 :                   14    ,VTM,      *00001B
003005 :                         ,UJ,       *00052B
003006 :      *00001B:           ,WTC,      *00066B
003007 :                         ,XTA ,
003008 :                   7     ,A-X ,00013B
003009 :                         ,ASN ,57
003010 :                         ,UZA,      *00004B
003011 :                         ,UJ,       *00005B
003012 :      *00004B:           ,UJ,       *00036B
003013 :      *00005B:           ,WTC,      *00067B
003014 :                         ,XTA ,
003015 :                   7     ,A-X ,00013B
003016 :                         ,ASN ,57
003017 :                         ,UZA,      *00010B
003018 :                         ,UJ,       *00011B
003019 :      *00010B:           ,UJ,       *00030B
003020 :      *00011B:           ,WTC,      *00066B
003021 :                         ,XTA ,
003022 :                         ,WTC,      *00067B
003023 :                         ,A/X ,
003024 :                   7     ,ATX ,00001B
003025 :                   14    ,VTM,      *00063B
003026 :                         ,ITA ,14
003027 :                   13    ,VJM, ATAN
003028 :                   7     ,ATX ,00002B
003029 :                   7     ,ATX ,00003B
003030 :                         ,WTC,      *00067B
003031 :                         ,XTA ,
003032 :                         ,ASN ,57
003033 :                         ,UZA,      *00027B
003034 :                         ,ARX ,
003035 :                         ,UZA,      *00022B
003036 :                         ,UJ,       *00027B
003037 :      *00022B:     14    ,VTM,      *00070B
003038 :                         ,ITA ,14
003039 :                   14    ,VTM,      *00064B
003040 :                         ,ITS ,14
003041 :                   13    ,VJM, SIGN
003042 :                         ,X-A ,
003043 :                   7     ,A+X ,00002B
003044 :                   7     ,ATX ,00003B
003045 :                         ,UTC, 00000B
003046 :      *00027B:           ,UJ,       *00057B
003047 :                         ,UTC, 00000B
003048 :      *00030B:     7     ,XTA ,00014B
003049 :                   7     ,A*X ,00006B
003050 :                   7     ,ATX ,00001B
003051 :                   14    ,VTM,      *00063B
003052 :                         ,ITA ,14
003053 :                         ,WTC,      *00066B
003054 :                   14    ,VTM, 00000B
003055 :                         ,ITS ,14
003056 :                   13    ,VJM, SIGN
003057 :                   7     ,ATX ,00003B
003058 :                         ,UJ,       *00057B
003059 :      *00036B:           ,WTC,      *00067B
003060 :                         ,XTA ,
003061 :                         ,ASN ,57
003062 :                         ,UZA,      *00044B
003063 :                         ,ARX ,
003064 :                         ,UZA,      *00042B
003065 :                         ,UJ,       *00050B
003066 :                         ,UTC, 00000B
003067 :      *00042B:     7     ,XTA ,00006B
003068 :                   7     ,ATX ,00003B
003069 :                         ,UJ,       *00057B
003070 :                         ,UTC, 00000B
003071 :      *00044B:     14    ,VTM,      *00071B
003072 :                         ,ITS ,14
003073 :                   13    ,VJM, FT*571
003074 :                   13    ,VJM, FT*003
003075 :                         ,UJ,       *00057B
003076 :                         ,UTC, 00000B
003077 :      *00050B:     7     ,XTA ,00013B
003078 :                   7     ,ATX ,00003B
003079 :                         ,UJ,       *00057B
003080 :                         ,UTC, 00000B
003081 :      *00052B:     7     ,MTJ ,8
003082 :                   7     ,VTM,      *00062B
003083 :                         ,NTR ,2
003084 :                   7     ,STX ,00005B
003085 :                   7     ,STX ,00004B
003086 :                         ,ITS ,13
003087 :                         ,ITS ,8
003088 :                         ,ITS ,0
003089 :                   14    ,UJ,  00000B
003090 :                         ,UTC, 00000B
003091 :      *00057B:           ,STI ,0
003092 :                         ,STI ,7
003093 :                         ,STI ,8
003094 :                         ,UTC,      *00065B
003095 :                         ,XTS ,
003096 :                   8     ,UJ,  00000B
003097 : CONST
003098 :      *00062B:,OCT,0000  0000  0000  0000
003099 :      *00063B:,OCT,0000  0000  0000  0000
003100 :      *00064B:,OCT,0000  0000  0000  0000
003101 :      *00065B:,OCT,0000  0000  0000  0000
003102 :      *00066B:,OCT,0000  0000  0000  0000
003103 :      *00067B:,OCT,0000  0000  0000  0000
003104 :      *00070B:,OCT,4114  4417  6652  0577
003105 :      *00071B:,OCT,1203  0460  2602  6047
003106 :      *00072B:,OCT,2125  1122  2365  1040
003107 :      *00073B:,OCT,2345  2515  2044  2522
003108 :      *00074B:,OCT,1003  1465  1542  3451
003109 :      *00075B:,OCT,0000  0000  0000  0000
003110 :      *00076B:,OCT,4010  0000  0000  0000
003111 :                   ,END,
003112 :  BCDBEG* :   , NAME  ,                    FPACK   31.10.72
003113 : C
003114 : C** FPACK-BCDBEG*
003115 : C                  GEMEINSAMER TEIL VON BCDREAD* UND BCDWRIT*
003116 : C                  (EROEFFNUNG EINER FORMATGESTEUERTEN EIN- AUSGABE)
003117 : C
003118 : C
003119 :  C       :   , EQUAL , 1
003120 :  A       :   , EQUAL , 8
003121 :  R2      :   , EQUAL , 12
003122 :  R3      :   , EQUAL , 13
003123 :  I5      :   , EQUAL , 5
003124 :  NWO     :   , EQUAL , 2
003125 :  NSG     :   , EQUAL , 6
003126 :  NSY     :   , EQUAL , 7
003127 :  *0      :   , EQUAL ,
003128 :  *0: , BLOCK , A1,     A13,    A17,    A1750,  A17700, A20077, A377
003129 :   ,CONT,A5,A60,A77777,BIT1/6,BIT1/7,BIT2/48  .06
003130 :      , CONT  , BIT1/40,BIT1/41,BIT10/11,       BIT10/14,       BIT1/48
003131 :      , CONT  , BIT12,  BIT10,  BIT12/14,       BIT12/15,       BIT14
003132 :      , CONT  , BIT15,  BIT16,  BIT17,  BIT13/48,       BIT16/41,
003133 :      , CONT  , BIT3/4,BIT37/48,        BIT41/48                      .03
003134 :      , CONT  , BIT43/48,       BIT48(0),       EXP64,  BIT7,         .03
003135 :      , CONT  , BIT8,   BIT8/15,BIT9/48,VGR0,   EX640(0),       VGR(6).10
003136 :   ,CONT,CT(64),REAL1(0),DEINS(2)    .66
003137 :   ,CONT,E1(0),D10(2),R1/2(0),D1/2(2),REAL5/4,R5/4H13
003138 :   ,CONT,EKSPO(19),
003139 :      , CONT  , SECHS,  Z1/10,  D1/10(2),       D10E40(2),            .06
003140 :      , CONT  , D10E*40(2),     EOR,                                  .03
003141 :      , CONT  , VMASKE1,VMASKE2,VW1,    VW2,    VH,     VP            .06
003142 :      , CONT  , SPACE,  EX645,  K3HREC, K3HADD,                       .04
003143 :      , CONT  , MAXEXNZ,FTXT(14),       GRZAHL, ISOSP,  NBIT45, BIT46 .19
003144 :      , CONT  , BIT45,  BIT2/47,NZL,    KRECORD,A4,     BIT46/48,     .06
003145 :  *0: , BLOCK , SYMB,   ENDE,  KUSP/USP,KNEXTLET,PW,  KWSY,     SFAKT,
003146 :      , CONT  , AUSG2,  D,      NSP,    CENC,   NSYMB                 .05
003147 :      , CONT  , II13,   ER/AB,  ADCONT, ADR/W,  ADEND,  FORMAT, LUN,
003148 :      , CONT  , I/O,    GLREG(7),       BEGREC, ADNBPROG,       (1)   .11
003149 :      , CONT  , RECORD(25),     GWF(5), EAD(5), FTT,    SWUEBERL, WF  .38
003150 :      , CONT  , W,      NIW,    KBASP,  STLUN,  AC,                   .05
003151 :      , CONT  , INOUT,  LUN*,   L/NLGLRE,       KOFSPEC,        TRWADR,
003152 :      , CONT  , FNR,    II15,   NSP3,   ZHLR,   FTBE/SONST,           .05
003153 :  IOCONT* :   , SUBP  ,
003154 :  IOEND*1 :   , EQU   , IOCONT* + 1
003155 :  FPF*    :   , EQU   , IOCONT* + 2
003156 :  FPF*AAFR:   , EQU   , IOCONT* + 3
003157 :  SETLUN* :   , EQU   , IOCONT* + 4
003158 :  READSY2 :   , EQU   , IOCONT* + 5
003159 :  READSY2F:   , EQU   , IOCONT* + 6
003160 :  RGTRSPR*:   , EQU   , IOCONT* + 8
003161 :  RGTRSPA*:   , EQU   , IOCONT* + 9
003162 :  RI15    :   , EQU   , IOCONT* + 11
003163 :              , ITS   , R2
003164 :             A, ATX   , ADNBPROG
003165 :           13 , VJM   , IOEND*1            ABSCHLUSS VORHERGEHENDER
003166 :              , UTC   , CB5                EIN- AUSGABE
003167 :              , XTA   ,
003168 :             A, ATX   , ADCONT
003169 :              , ASN   , 64 + 24
003170 :             A, ATX   , ADEND
003171 :             C, XTA   , BIT48
003172 :             A, STX   , ER/AB.       ML    ER/AB:=BIT48 DH BCD EIN-AUSG
003173 :             A, STX   , ADR/W.       ML                        EROEFFNET
003174 :             A, STX   , I/O.         ML
003175 :              , U1A   , CB4                SPR, WENN ENCODE ODER DECODE
003176 :             A, STX   , CENC.        ML    PARAMETER-UEBERNAHME BEI:
003177 :             A, ATX   , FORMAT.      ML    BCDREAD*(LUN,FT),BCDWRIT*(...)
003178 :           15 , WTC   ,
003179 :              , XTA   ,
003180 :             A, ATX   , LUN
003181 :           R3 , VJM   , RI15
003182 :            R3, VJM   , SETLUN*
003183 :            R3, VJM   , FT1*
003184 :             A, XTA   , I/O
003185 :              , U1A   , CB61               SPR, BEI AUSGABE
003186 :             C, XTA   , ISOSP              VORLOESCHEN VON RECORD MIT
003187 :            A , ATX   , RECORD             149 SPACES + EOR
003188 :  CB20    :  A, WTC   , ADR/W
003189 :            R3, VJM   ,
003190 :  CB3     : R3, VJM   , FT2*
003191 :             C, AAX   , BIT3/4
003192 :              , U1A   , CBEND
003193 :            R3, VTM   , CB3
003194 :             A, WTC   , ADR/W
003195 :            I5, UJ    ,
003196 : C----------------------------------------------------------------------
003197 :  CB4     : 15, XTA   , .            ML    PARAMETER-UEBERNAHME BEI:
003198 :             A, STX   , LUN.         ML    BCDDEC*(C,FT,V), BCDENC*(...)
003199 :             A, STX   , FORMAT.      ML    LUN:= V, FORMAT:= FT
003200 :           R3 , VJM   , RI15
003201 :              , ATI   , R3
003202 :           R3 , XTA   ,
003203 :            C , AAX   , A77777
003204 :              , UZA   , CBFEHL             FALLS C = 0
003205 :  CB41    :  A, ATX   , CENC               CENC:= C
003206 :            R3, VJM   , FT1*
003207 :             A, XTA   , I/O
003208 :              , UZA   , CB20
003209 :             A, WTC   , LUN                ANFANGSWERTE FUER WSY* BEI
003210 :           NWO, VTM   ,                    BCDENC*, BCDDEC* :
003211 :           NSY, VTM   , -5                 NWO:= V    NSY:= -5
003212 :            A , XTA   , CENC
003213 :              , ATI   , NSG                NSG:= C
003214 :              , UJ    , CB3
003215 :  '--------------------------------
003216 :  CBFEHL  : R2, 31    , FPF*        .PARAMETER C=0 BEI BCDENC* / BCDDEC*
003217 :               , UTC   , 1203 B
003218 :            C , XTA   , BIT3/4
003219 :              , UJ    , CB41                                            B
003220 :  '--------------------------------
003221 :  CB61    :  A, UTC   , RECORD             ANFANGSWERTE FUER WSY* BEI
003222 :           NWO, VTM   ,                    BCDWRIT*
003223 :           NSY, VTM   , -5
003224 :           NSG, VTM   , 149
003225 :              , UJ    , CB3
003226 : C----------------------------------------------------------------------
003227 :  CBEND   :  A, XTA   , WF                 WF:=WF+1
003228 :             C, ARX   , A1
003229 :             A, ATX   , WF
003230 :              , UJ    , RGTRSPA*           RUECKSPR MIT AC LADEN
003231 :  '--------------------------------
003232 :  CB5     :   , UTC   , BCDEND*
003233 :              , UTC   , BCDCONT*
003234 : C
003235 : C**   BCDCONT*   *****
003236 : C                  FORTSETZUNG DES UP IOCONT* BEI BCD-EIN-AUSGABE
003237 : C
003238 :  I1      :   , EQUAL ,  1
003239 :  I3      :   , EQUAL , 3
003240 :  I4      :   , EQUAL , 4
003241 :  I8      :   , EQUAL , 8
003242 :  I13     :   , EQUAL , 13
003243 :  BCDCONT*: A , XTA   , KUSP/USP
003244 :              , UZA   , CCERR              SPR, WENN KEINE UMW-SP VORH
003245 :            I8, XTA   , I/O
003246 :              , U1A   , CCA1
003247 :  CC1     :I13, VJM   , FT2*
003248 :           I13, VTM   , CC1
003249 :           I8 , WTC   , ADR/W
003250 :            I5, UTM   ,
003251 :            I1, AEX   , A13
003252 :           I5 , UZA   ,
003253 :            C , AEX   , A13
003254 :            I1, AAX   , BIT3/4
003255 :           I5 , UZA   ,
003256 :              , ITA   , I3
003257 :            A , ATX   , TRWADR
003258 :              , ITS   , I4
003259 :           15 , ATX   ,
003260 :           I5 ,  UTC  ,
003261 :          I13 , VJM   ,
003262 :            15, XTA   ,
003263 :              , STI   , I4
003264 :              , ATI   , I3
003265 :            I8, XTA   , I/O
003266 :              , U1A   , CC2         .FALLS AUSGABE
003267 :            A , XTA   , AC
003268 :              , U1A   , CCE2        .FALLS IOAC* AUFGERUFEN
003269 :            A , XTA   , INOUT
003270 :           I3 , ATX   ,
003271 :           I3 , UTM   , 1
003272 :  CC2     : I4, VLM   , BCDCONT*
003273 :              , UJ    , RGTRSPA*
003274 :  '--------------------------------
003275 :  CCE2    : 15,  XTA  ,
003276 :            I8, XTA   , INOUT
003277 :              , UJ    , RGTRSPR*
003278 :  '--------------------------------
003279 :  CCA1    : I8, XTA   , AC
003280 :              , U1A   , CC1
003281 :            I3, XTA   ,
003282 :            I3, UTM   , 1
003283 :            I8, ATX   , INOUT
003284 :              , UJ    , CC1
003285 : C---------------------------------
003286 :  CCERR   :R2 , 31    , FPF*        .KEINE UMW-SPEC VORHANDEN, OBWOHL
003287 :              , UTC   , 21 231B     .EIN- AUSGABELISTE NICHT LEER
003288 : C
003289 : C**   BCDEND*    *****
003290 : C                  FORTSETZUNG DES UP IOEND* BEI ABSCHLUSS EINER
003291 : C                  EIN- AUSGABE VON BCD-INFORMATION
003292 : C
003293 :  BCDEND* :   , BSS   ,
003294 :              ,  ITA  , 13
003295 :           15 ,  ATX  ,
003296 :  CE1     :13 ,  VJM  , FT2*
003297 :           I1 ,  AAX  , BIT3/4
003298 :              ,  U1A  , CEND
003299 :           13 ,  VTM  , CE1
003300 :           I8 ,  WTC  , ADR/W
003301 :           I5 ,  UJ   ,
003302 :  CEND    :I8 ,  XTA  , I/O
003303 :              ,  UZA  , CE2
003304 :           I8 ,  WTC  , ADR/W
003305 :           13 ,  VJM  ,
003306 :  CE2     :I8 ,  XTA  , L/NLGLRE
003307 :           15 ,  WTC  ,
003308 :              ,  U1A  ,
003309 :              ,  UJ   , RGTRSPA*
003310 : C
003311 : C**   FT1*       *****
003312 : C                  FORMATENTSCHLUESSELUNG - SYNTAKTISCHE KONTROLLE
003313 : C
003314 :  F12     :   , EQUAL , 11
003315 :  I10     :   , EQUAL , 10
003316 :  I11     :   , EQUAL , 11
003317 :  I12     :   , EQUAL , 12
003318 :  I14     :   , EQUAL , 14
003319 :  VKLA    :   , EQUAL , CT + 40
003320 :  VPU     :   , EQUAL , CT + 46
003321 :  READSY3 :   , EQU   , IOCONT* + 13
003322 :  READSY3F:   , EQU   , IOCONT* + 14
003323 :  RSZRZST3:   , EQU   , IOCONT* + 15
003324 :  CHECKFL*:   , LC    , 1
003325 :  FT1*    :   , BSS   ,
003326 :           F12, VTM   ,                    F12:=0 DH FT1* AUFGERUFEN
003327 :              , XTA   ,
003328 :            A , ATX   , KUSP/USP
003329 :  FT1*1   : A , XTA   , FORMAT
003330 :            C , AOX   , BIT46/48
003331 :            A , ATX   , ZHLR
003332 :            A , ATX   , FTBE/SONST
003333 :              , XTA   ,
003334 :            A , ATX   , NIW                NIW:=0
003335 :           R2 , VJM   , READSY3F
003336 :            C , AEX   , VKLA
003337 :              , U1A   , FTF1
003338 :  FT16    : A , XTA   , ZHLR
003339 :            A , ATX   , EAD
003340 :  FT2     : A , XTA   , ZHLR
003341 :            A , ATX   , KOFSPEC
003342 :              , XTA   ,
003343 :            A ,  ATX  , D                   D:=0
003344 :            A ,  ATX  , WF                  WF:=0
003345 :           I12, VJM   , READSY3F
003346 :            A ,  ATX  , FTT
003347 :           I1 ,  XTA  , BIT9/48
003348 :            A , ATX   , SWUEBERL
003349 :            A ,  XTA  , SYMB
003350 :           I1 ,  AAX  , BIT10/14            TEST: SYMB = ZIFFER,E,D,F,
003351 :              , UZA   , FT3                              I,O,A,L
003352 :           I1 ,  AAX  , BIT12/14            TEST: SYMB = E,D,F,I,O,A,L
003353 :              ,  UZA  , FTZIF
003354 :  FT5     : A , ATX   , KUSP/USP    .KUSP/USP:=1 DH UMW-SP VORHANDEN
003355 :           I1 ,  AAX  , BIT14               TEST: SYMB = I,O,A,L
003356 :              ,  U1A  , FTIOAL
003357 :           I14,  VJM  , FTKONV2             BEARBEITUNG: EW.D FW.D DW.D
003358 :              , UZA   , FTF205             SPR, WENN W = 0
003359 :            A , ATX   , W
003360 :            A ,  XTA  , SYMB                             DW.D
003361 :           I1 ,  AEX  , VPU
003362 :              ,  U1A  , FTF2
003363 :           I14,  VJM  , FTKONV2
003364 :            A ,  XTA  , D
003365 :              ,  NTR  , 3                   BL. NORM. + RUND.
003366 :            A ,  X-A  , W
003367 :              ,  U1A  , FTF3                SPRUNG WENN W KLEINER D
003368 :  FT51    :   ,  NTR  , 2
003369 :           I12, VTM   , FT9
003370 :              , UJ    , RSZRZST3
003371 :  '--------------------------------
003372 :  FTKONV1 : A ,  XTA  , SYMB
003373 :           I1 ,  AAX  , A17
003374 :            A ,  XTS  , D
003375 :              ,  ASN  , 64 - 3
003376 :            A ,  ARX  , D
003377 :            A ,  ARX  , D
003378 :           15 ,  ARX  ,
003379 :            A ,  ATX  , D
003380 :  FTKONV  :I12, VJM   , READSY3F
003381 :           I1 ,  AAX  , BIT10/11            TEST: SYMB = ZIFFER
003382 :              ,  U1A  , FTKONV1
003383 :            A ,  XTA  , D
003384 :            A ,  AAX  , SWUEBERL
003385 :              , U1A   , FTF205
003386 :            A , XTA   , D
003387 :            I14, UJ   ,
003388 :  FTF205  :R2 , 31    , FPF*1       .GWF WF W D GROESSER 255
003389 :              , UTC   , 205 B.       N GROESSER 4095 BEI +NP -NP
003390 :            C , XTA   , BIT7        .ODER GWF=0 ODER WF=0
003391 :            A , ATX   , D
003392 :           I14,  UJ   ,
003393 :  '--------------------------------
003394 :  FTIOAL  :I14,  VJM  , FTKONV2             BEARBEITUNG: IW, OW, AW, LW
003395 :              ,  UZA  , FTF205             SPR, WENN W = 0
003396 :            A ,  ATX  , W
003397 :  FTF2    :   ,  XTA  ,
003398 :            A ,  ATX  , D
003399 :           I12,  VTM  , FT9
003400 :              ,  UJ   , RSZRZST3                                        F
003401 :  '--------------------------------
003402 :  FTZIF   : C , XTA   , BIT41/48
003403 :            A , ATX   , SWUEBERL
003404 :           I14,  VJM  , FTKONV1
003405 :  FT98    : A ,  XTA  , SYMB
003406 :            A ,  ATX  , FTT
003407 :           I1 ,  AAX  , BIT12/15            TEST: SYMB = X,P,H,E,D,F,
003408 :              ,  UZA  , FTKLA                            I,O,A,L
003409 :           I1 ,  AAX  , BIT15               TEST: SYMB = X,P,H
003410 :              ,  U1A  , FTXPH
003411 :            A ,  XTA  , D                   BEARBEITUNG:
003412 :              ,  UZA  , FTF205             SPR, WENN WF = 0
003413 :           I1 ,  ARX  , BIT1/40
003414 :           I1 ,  AAX  , BIT1/40
003415 :            A ,  ATX  , WF                     WFEW.D, WFDW.D, WFEW.D
003416 :            A ,  XTA  , SYMB                   WFIW,   WFOW,   WFAW
003417 :              ,  UJ   , FT5                    WFLW
003418 :  '--------------------------------
003419 :  FTXPH   : A ,  XTA  , SYMB
003420 :            I1,  AEX  , VP                  TEST: SYMB = P
003421 :              ,  UZA  , FTP
003422 :            A ,  XTA  , D                   TEST: W=0 BEI WH, WX
003423 :              ,  UZA  , FTF205             SPR, WENN W BZW N = 0
003424 :              , ATI   , I5
003425 :            C , AAX   , BIT9/48
003426 :              , U1A   , FTF205
003427 :            A ,  XTA  , SYMB                TEST: SYMB = H
003428 :           I1 ,  AEX  , VH
003429 :              ,  U1A  , FT6                SPR, WENN SYMB = X
003430 :           I12, VJM   , READSY3
003431 :            A , XTA   , ZHLR
003432 :            A , ATX   , W
003433 :           I12,  VTM  , FT10
003434 :  FT10    :I5 ,  UTM  , -1
003435 :           I5 ,  VZM  , FT9
003436 :              , UJ    , READSY3
003437 :  '--------------------------------
003438 :  FTKLA   : A ,  XTA  , SYMB
003439 :           I1 ,  AEX  , VKLA
003440 :              ,  U1A  , FTF5
003441 :            A ,  XTA  , D
003442 :              ,  UZA  , FTF205             SPR, WENN GWF = 0
003443 :            C , ARX   , BIT1/40
003444 :            C , AAX   , BIT1/40
003445 :            A , ATX   , D
003446 :  FT12    : A , XTA   , NIW
003447 :            C , AEX   , A4
003448 :              , UZA   , FTF6               SPR, WENN WIEDERH-GR DES 5.
003449 :            A , XTA   , NIW                NIVEAUS VORLIEGT
003450 :            C , ARX   , A1
003451 :            A , ATX   , NIW
003452 :            C , AEX   , A1
003453 :              , U1A   , FT13               SPR, WENN NICHT 1. NIVEAU
003454 :            A , XTA   , KOFSPEC
003455 :            A , ATX   , EAD
003456 :  FT13    : A , XTA   , D
003457 :            A , WTC   , NIW
003458 :            A , ATX   , GWF
003459 :            A , XTA   , ZHLR
003460 :            A , WTC   , NIW
003461 :            A , ATX   , EAD
003462 :              , UJ    , FT2
003463 :  '--------------------------------
003464 :  FT3     : A ,  XTA  , SYMB
003465 :              ,  ATI  , I5
003466 :           I1 ,  AAX  , BIT16               TEST: SYMB = '
003467 :              ,  U1A  , FTAPOS
003468 :           I5 ,  UTM  , -50 B               TEST: SYMB = (
003469 :           I5 ,  VZM  , FT12
003470 :           I5 ,  UTM  , 50 B - 54 B         TEST: SYMB = ,
003471 :           I5 ,  VZM  , FT2
003472 :           I5 ,  UTM  , 54 B - 51 B         TEST: SYMB = )
003473 :           I5 ,  VZM  , FTKLZ
003474 :           I5 ,  UTM  , 51 B - 57 B         TEST: SYMB = /
003475 :           I5 ,  VZM  , FT6
003476 :           I5 , UTM   , 57 B - 40130 B     TEST: SYMB = X
003477 :           I5 , VZM   , FTX
003478 :           I5 , UTM   , 40130B - 40110B    TEST: SYMB = H
003479 :           I5 , VZM   , FTX
003480 :           I5 , UTM   , 40110B - 53B       TEST: SYMB = +
003481 :           I5 ,  VZM  , FTPL
003482 :           I5 ,  UTM  , 53 B - 55 B         TEST: SYMB = -
003483 :           I5 ,  V1M  , FTF8
003484 :           I5 ,  VTM  , 1
003485 :  FTPL    :I1 ,  XTA  , BIT13/48
003486 :            A ,  ATX  , SWUEBERL
003487 :           I14,  VJM  , FTKONV
003488 :            A ,  XTA  , SYMB
003489 :           I1 ,  AEX  , VP
003490 :              ,  U1A  , FTF7
003491 :  FTPL1   : A ,  XTA  , D
003492 :           I5 ,  VZM  , FTPL2
003493 :           I1 ,  AEX  , BIT1/41
003494 :           I1 ,  ARX  , A1
003495 :            C , AAX   , BIT1/41
003496 :  FTPL2   : A , ATX   , SFAKT
003497 :              ,  UJ   , FT2
003498 :  FTP     : A , XTA   , D
003499 :              , UJ    , FTPL2
003500 :  '--------------------------------
003501 :  FTAPOS  :I5 , VTM   ,
003502 :           R2 , VJM   , READSY3
003503 :           C  , AAX   , BIT16
003504 :              , U1A   , FT2                SPR, WENN SYMB = '
003505 :            A , XTA   , ZHLR
003506 :           A  , ATX   , W
003507 :  FT14    :R2 , VJM   , READSY3
003508 :           I5 , UTM   , 1
003509 :            C , AAX   , BIT16
003510 :              , UZA   , FT14               SPR, WENN SYMB UNGLEICH '
003511 :              , ITA   , I5
003512 :            A , ATX   , D
003513 :              , UJ    , FT9
003514 :  '--------------------------------
003515 :  FTKLZ   :   , BSS   ,
003516 :            A , XTA   , NIW
003517 :              , UZA   , FRTEND
003518 :           F12, VZM   , GWFNULL            SPR, IN FT1*
003519 :            A , WTC   , NIW
003520 :            A , XTA   , GWF
003521 :              , UZA   , GWFNULL
003522 :            C , ARX   , BIT1/40
003523 :            C , AAX   , BIT1/40
003524 :            A , WTC   , NIW
003525 :            A , ATX   , GWF
003526 :            A , WTC   , NIW
003527 :            A , XTA   , EAD
003528 :            A , ATX   , ZHLR
003529 :              , UJ    , FT2
003530 :  GWFNULL : A , XTA   , NIW
003531 :            C , ARX   , A377
003532 :            C , AAX   , A377
003533 :            A , ATX   , NIW
003534 :              , UJ    , FT2
003535 :  FRTEND  : A , XTA   , EAD
003536 :            A , ATX   , ZHLR
003537 :              , UJ    , FT6
003538 :  '--------------------------------
003539 :  FPF*1   :F12, VZM   , FPF*        .SPR, BEI FT1*
003540 :              , XTA   , CHECKFL*
003541 :           R2 , U1A   ,             .SPR, WENN CHECK-REGIME
003542 :              , UJ    , FPF*        .FORMAT-FEHLER IN FT2* GEFUNDEN
003543 :  '--------------------------------
003544 :  FTF1    :R2 ,  31   , FPF*1       .FORMATKETTE BEGINNT NICHT MIT (
003545 :              ,  UTC  , 41 206B
003546 :           R2 , VTM   , FT16
003547 :              , UJ    , RSZRZST3    .ZURUECKSETZEN UM 1 ZEICHEN
003548 :  '--------------------------------
003549 :  FTF3    : A , XTA   , I/O
003550 :              , UZA   , FT51
003551 :          :R2 , 31    , FPF*1       .W KLEINER D BEI AUSGABE IM
003552 :              , UTC   , 210 B       .E-, F-  ODER D-FORMAT
003553 :            A , XTA   , W
003554 :            A ,ATX   , D
003555 :              , UJ    , FT51
003556 :  '--------------------------------
003557 :  FTF5    :R2 ,  31   , FPF*1       .UNZULAESSIGES ZEICHEN IN DER
003558 :              ,  UTC  , 212 B.       FORMATKETTE
003559 :           R2 , VTM   , FT2
003560 :              , UJ    , RSZRZST3
003561 :  '--------------------------------
003562 :  FTF6    :R2 ,  31   , FPF*        .ES TRITT EINE WIEDERHOLUNGSGRUPPE
003563 :              ,  UTC  , 20 213B     .DES 3. NIVEAUS AUF
003564 :  '--------------------------------
003565 :  FTKONV2 :   ,  XTA  ,
003566 :            A ,  ATX  , D
003567 :              ,  UJ   , FTKONV
003568 :  '--------------------------------
003569 :  FTX     : C , XTA   , A1
003570 :           A  , ATX   , D                  D := 1
003571 :              , UJ    , FT98
003572 :  '--------------------------------
003573 :  FTF7    :R2 ,  31   , FPF*1       .NACH +ZAHL ODER -ZAHL FOLGT NICHT P
003574 :              ,  UTC  , 214 B.
003575 :           R2 , VTM   , FTPL1
003576 :              , UJ    , RSZRZST3
003577 :  '--------------------------------
003578 :  FTF8    :R2 ,  31   , FPF*1      .FORMATSPEZIFIKATION BEGINNT MIT
003579 :              ,  UTC  , 215 B.      UNZULAESSIGEM ZEICHEN
003580 :              , UJ    , FT2
003581 : C
003582 : C**   FT2*       *****
003583 : C                  BEREITSTELLUNG DES NAECHSTEN FORMATS AUS DER
003584 : C                  FORMATKETTE
003585 : C
003586 :  FT2*    :   , ENTRY ,
003587 :  FT2*    :   , BSS   ,
003588 :           F12, VTM   , 1                  F12:=1 DH FT2* AUFGERUFEN
003589 :            A , XTA   , FTBE/SONST
003590 :              , UZA   , FT1*1
003591 :            A , XTA   , WF
003592 :              , UZA   , FT2
003593 :            C , ARX   , BIT1/40
003594 :            C , AAX   , BIT1/40
003595 :            A , ATX   , WF
003596 :  FT6     :   , BSS   ,
003597 :  FT9     :   , XTA   ,
003598 :            A , ATX   , KOFSPEC
003599 :            A , XTA   , FTT
003600 :              , ASN   , 64 + 18
003601 :            C , AAX   , BIT1/6
003602 :              , ATI   , I5
003603 :            R3, UTC   ,
003604 :           F12, V1M   ,                    AUSGANG FT2*
003605 :            C , AEX   , A13
003606 :              , U1A   , FT2
003607 :            A , ATX   , FTBE/SONST
003608 :            A , ATX   , SFAKT
003609 :           R3 , UJ    ,                    AUSGANG FT1*
003610 : C
003611 : C**   KONV1*     *****
003612 : C                  KONVERTIERUNG TEIL 1
003613 : C                  ENTSCHLUESSELUNG DER MANTISSE UND DES EXP BEI DER
003614 : C                  EINGABE VON R-D-I-ZAHLEN
003615 :  KONV1*  :   , ENTRY ,
003616 :  I9      :   , EQUAL , 9
003617 :  VMI     :   , EQUAL , CT + 45
003618 :  VPL     :   , EQUAL , CT + 43
003619 :           I3 , VTM   ,                    A:=A1
003620 :           I5 , VTM   ,                    E:=E1
003621 :           I9 , VTM   ,                    F:=F1
003622 :           I10, VTM   ,                    G:=G1
003623 :           I11, VTM   , -4
003624 :              , XTA   ,                    LOESCHEN :
003625 :  KV11    :15 , ATX   ,                    EXP10A,   MANTN  MANTM  MANTH
003626 :           I11, VLM   , KV11               I11=VMANT:=+
003627 :              , NTR   , 3                  BLOCK. RUND/NORM
003628 : C
003629 : C         ENTSCHLUESSELUNG DER MANTISSE
003630 : C
003631 :           I12, VJM    , READSY2F
003632 :           I1 , AEX   , VPL                TEST: SYMB = +
003633 :              , UZA   , KV1R2
003634 :           I8 , XTA   , SYMB
003635 :           I1 , AEX   , VMI                TEST: SYMB = -
003636 :              , U1A   , KV1M1
003637 :           I11, VTM   , 1                  VMANT:=-
003638 :  KV1R2   :I10, VTM   , 1                  G:=G2
003639 :           I12, VJM   , READSY2F
003640 :  KV1M1   :I8 , XTA   , SYMB
003641 :           I1 , AAX   , BIT10/11           TEST: SYMB = ZIFFER
003642 :              , UZA   , KV1X1
003643 :           I9 , V1M   , KV1F22
003644 :           15 , MTJ   , I14
003645 :           I14, UTM   , -4
003646 :           I12, VJM   , KV1KONV
003647 :           I5 , V1M   , KV1E2
003648 :           I1 , AAX   , BIT41/48
003649 :              , U1A   , KV1E2
003650 :  KV1M4   :I3 , VZM   , KV1R2
003651 :           15 , XTA   , -5
003652 :           I1 , A-X   , A1                 EXP10A:= EXP10A - 1
003653 :           15 , ATX   , -5
003654 :              , UJ    , KV1R2
003655 :  '--------------------------------
003656 :  KV1KONV :I8 , XTA   , SYMB
003657 :           I1 , AAX   , A17
003658 :  KV1KONV1:I8 , ATX   , SYMB
003659 :           I14, XTA   ,
003660 :              , ASN   , 64- 3
003661 :           I14, ARX   ,
003662 :           I14, ARX   ,
003663 :           I8 , ARX   , SYMB
003664 :           I14, ATX   ,
003665 :           I12, UJ    ,
003666 :  '--------------------------------
003667 :  KV1E2   : I5, VTM   , 1                  E:=E2
003668 :          I13 , VTM   , -1
003669 :  KV1Z    :   , ASN   , 64 + 40
003670 :          I14 , UTM   , 1
003671 :          I12 , VJM   , KV1KONV1
003672 :          I13 , VLM   , KV1Z
003673 :           I1 , AAX   , BIT37/48
003674 :              , UZA   , KV1M3
003675 :           I9 , VTM   , 1
003676 :  KV1M3   : 15, XTA   , -4
003677 :           I1 , AAX   , BIT1/40
003678 :           15 , ATX   , -4
003679 :           15 , XTA   , -3
003680 :           I1 , AAX   , BIT1/40
003681 :           15 , ATX   , -3
003682 :              , UJ    , KV1M4
003683 :  '--------------------------------
003684 :  KV1F22  : 15, XTA   , -5
003685 :           I1 , A+X   , A1                 EXP10A:= EXP10A + 1
003686 :           15 , ATX   , -5
003687 :              , UJ    , KV1M4
003688 :  '--------------------------------
003689 :  KV1X1   : I8, XTA   , SYMB
003690 :           I1 , AEX   , VPU                TEST: SYMB = .
003691 :              , U1A   , KV1X2
003692 :           I4 , V1M   , KV1F2
003693 :           I3 , V1M   , KV1F2
003694 :           I3 , VTM   , 1                  A:=A2
003695 :              , UJ    , KV1R2
003696 :  '--------------------------------
003697 :  KV1X2   : I8, XTA   , SYMB
003698 :           I1 , AAX   , BIT12              TEST: SYMB = E ODER D
003699 :              , UZA   , KV1X3
003700 :           I10, V1M   , KV1M6
003701 :           I1 , XTA   , A1
003702 :           15 , ATX   , -4                 MANTN:=1
003703 : C
003704 : C         ENTSCHLUESSELUNG DES EXPONENTEN
003705 : C
003706 :  KV1M6   :I10, VTM   ,                    VEXP10:=+
003707 :           I12, VJM   , READSY2F
003708 :  KVPL2   :I8 , XTA   , SYMB
003709 :           15 , MTJ   , I14
003710 :           I14, UTM   , -1
003711 :           I1 , AEX   , VPL                TEST: SYMB = +
003712 :              , UZA   , KV1R4
003713 :           I8 , XTA   , SYMB
003714 :           I1 , AEX   , VMI                TEST: SYMB = -
003715 :              , U1A   , KV1M7
003716 :           I10, VTM   , 1                  VEXP10:=-
003717 :  KV1R4   :I12, VJM   , READSY2F
003718 :  KV1M7   :I8 , XTA   , SYMB
003719 :           I1 , AAX   , BIT10/11           TEST: SYMB = ZIFFER
003720 :              , UZA   , KV1F1
003721 :           I12, VJM   , KV1KONV
003722 :            C , AAX   , BIT13/48
003723 :              , UZA   , KV1R4
003724 :           I10, V1M   , KV1R4
003725 :          :I12, 31    , FPF*AAFR   .UEBERLAUF - EXPONENT GROESSER
003726 :              , UTC   , 220 B       .ALS 4095
003727 :           I12, VTM   , READSY2
003728 :           I12, UJ    ,
003729 :  '--------------------------------
003730 :  KV1X3   :I8 , XTA   , SYMB
003731 :           I1 , AAX   , BIT17
003732 :           I10,  VTM  ,                    VEXP10 := +
003733 :              , U1A   , KVPL2
003734 :  KV1F2   :I12, 31    , FPF*AAFR   .UNZULAESSIGES SYMBOL IN DER MANTISSE
003735 :              , UTC   , 201 B.      EINER ZAHL (EW.D FW.D DW.D IW )
003736 :              , UJ    , KV1R2
003737 :  '--------------------------------
003738 :  KV1F1   :I12, 31    , FPF*AAFR   .UNZULAESSIGES SYMBOL IM EXPONENTEN
003739 :              , UTC   , 202 B.      EINER ZAHL
003740 :              , UJ    , KV1R4
003741 :  ,END,
003742 :       BCDPUN      :,NAME,
003743 :       CARD2       :,LC, 00025B
003744 :       ISOICTTR    :,SUBP,
003745 :       REVCARD     :,SUBP,
003746 :                         ,STX ,
003747 :                         ,STX ,
003748 :                   14    ,VJM,      *00011B
003749 :                   11    ,ATX ,00042B
003750 :                   11    ,XTA ,00044B
003751 :                         ,UZA,      *00006B
003752 :      *00004B:     11    ,XTA ,00042B
003753 :                   13    ,VTM,      *00017B
003754 :                         ,UJ,       *00031B
003755 :      *00006B:     11    ,XTA ,00035B
003756 :                   11    ,ATX ,00044B
003757 :                         ,*70 ,00046B
003758 :                   13    ,VTM,      *00004B
003759 :                         ,UJ,       *00052B
003760 :      *00011B:           ,ITS ,13
003761 :                         ,UTC,      *00106B
003762 :                         ,ATX ,
003763 :                   13    ,VTM, 77771B
003764 :      *00013B:     13    ,ITA ,12
003765 :                         ,UTC,      *00105B
003766 :                   13    ,ATX ,
003767 :                   13    ,VLM,      *00013B
003768 :                   11    ,VTM,      *00031B
003769 :                   11    ,XTA ,00043B
003770 :                         ,STI ,6
003771 :                   14    ,UJ,  00000B
003772 :      *00017B:           ,ITS ,6
003773 :                         ,UTC,      *00074B
003774 :                         ,STX ,
003775 :                   14    ,VTM, 77770B
003776 :      *00021B:           ,UTC,      *00106B
003777 :                   14    ,XTS ,
003778 :                   14    ,STI ,13
003779 :                   14    ,VLM,      *00021B
003780 :                   13    ,UJ,  00000B
003781 :  EBCDPUN:,ENTRY,
003782 :                         ,XTA ,
003783 :                         ,UTC,      *00075B
003784 :                         ,ATX ,
003785 :                   14    ,VJM,      *00011B
003786 :                   13    ,VJM,      *00055B
003787 :                         ,*70 ,00045B
003788 :                         ,UJ,       *00017B
003789 :      *00031B:           ,ATI ,5
003790 :                         ,ITA ,7
003791 :                   7     ,VTM,      *00031B
003792 :                   7     ,ATX ,00036B
003793 :                         ,ITA ,13
003794 :                   7     ,ATX ,00040B
003795 :                   7     ,XTA ,00033B
003796 :                         ,ITS ,5
003797 :                   13    ,VJM, ISOICTTR
003798 :                   7     ,XTS ,00034B
003799 :                   7     ,XTS ,00033B
003800 :                   13    ,VJM, REVCARD
003801 :      *00040B:     7     ,XTS ,00034B
003802 :                   7     ,ARX ,00037B
003803 :                   7     ,STX ,00034B
003804 :                   6     ,VLM,      *00057B
003805 :      *00042B:           ,WTC, 00043B
003806 :                   8     ,VTM, 00000B
003807 :                         ,WTC,      *00065B
003808 :                   9     ,VTM, 77777B
003809 :                         ,XTS ,
003810 :                   9     ,ATX ,00001B
003811 :                   9     ,STX ,00002B
003812 :                         ,UTC,      *00072B
003813 :                         ,*71 ,
003814 :                   6     ,V1M,      *00052B
003815 :                         ,WTC,      *00067B
003816 :                   7     ,VTM, 00000B
003817 :                         ,WTC,      *00071B
003818 :                   13    ,VTM, 00000B
003819 :                         ,UTC, 00000B
003820 :      *00052B:           ,XTS ,00043B
003821 :                         ,UTC,      *00065B
003822 :                         ,STX ,
003823 :                   6     ,VTM, 77727B
003824 :                   13    ,UJ,  00000B
003825 :      *00055B:     6     ,UTM, 00051B
003826 :                   13    ,UTC, 00000B
003827 :                   6     ,VZM, 00000B
003828 :                         ,UJ,       *00042B
003829 :      *00057B:           ,WTC,      *00067B
003830 :                   7     ,VTM, 00000B
003831 :                         ,WTC,      *00071B
003832 :                         ,UJ,  00000B
003833 :      *00061B:     14    ,UTC, 00000B
003834 :                   13    ,XTS ,00027B
003835 :                         ,WTC,      *00065B
003836 :                   13    ,STX ,00027B
003837 :                   13    ,VLM,      *00061B
003838 :                         ,UJ,       *00040B
003839 :                         ,UTC, 00000B
003840 :                         ,UTC, CARD2
003841 :      *00065B:           ,UTC, 00000B
003842 :                         ,UTC, 76000B
003843 : CONST
003844 :      *00066B:,OCT,0000  0000  0000  0001
003845 :      *00067B:,OCT,6400  0000  0000  0007
003846 :      *00070B:,OCT,0000  0000  0000  0030
003847 :      *00071B:,OCT,6400  0000  0000  0015
003848 :      *00072B:,OCT,4010  0000  4410  0000
003849 :  ,BSS,1
003850 :   *00074B:,BSS,1
003851 :  *00075B:,BSS,8
003852 :  *00105B:,BSS,1
003853 :  *00106B:,BSS,65
003854 :                   ,END,
003855 :  BCDREAD*:   , NAME  ,                    FPACK   28.08.72
003856 : C
003857 : C** FPACK-BCDREAD*(FT,LUN)
003858 : C                  BENUTZERROUTINE ZUR EROEFFNUNG EINER FORMAT-
003859 : C                  GESTEUERTEN EINGABE
003860 : C
003861 : C
003862 :  IOCONT* :   , SUBP  ,
003863 :  RGTRI13*:   ,  EQU  , IOCONT* + 10
003864 :  READ*:,SUBP,
003865 :  TTREAD:,SUBP,
003866 :  BCDBEG* :   ,  SUBP ,
003867 :  I12     :   , EQUAL , 12
003868 :  I14     :   , EQUAL , 14
003869 :  *0      :   , EQUAL ,
003870 :  *0: , BLOCK , A1,     A13,    A17,    A1750,  A17700, A20077, A377
003871 :   ,CONT,A5,A60,A77777,BIT1/6,BIT1/7,BIT2/48
003872 :      , CONT  , BIT1/40,BIT1/41,BIT10/11,       BIT10/14,       BIT1/48
003873 :      , CONT  , BIT12,  BIT10,  BIT12/14,       BIT12/15,       BIT14
003874 :      , CONT  , BIT15,  BIT16,  BIT17,  BIT13/48,       BIT16/41,
003875 :      , CONT  , BIT3/4,BIT37/48,        BIT41/48                      .03
003876 :      , CONT  , BIT43/48,       BIT48(0),       EXP64,  BIT7,         .03
003877 :      , CONT  , BIT8,   BIT8/15,BIT9/48,VGR0,   EX640(0),       VGR(6).10
003878 :   ,CONT,CT(64),REAL1(0),DEINS(2)     .66
003879 :   ,CONT,E1(0),D10(2),R1/2(0),D1/2(2),REAL5/4,R5/4H13
003880 :   ,CONT,EKSPO(19),
003881 :      , CONT  , SECHS,  Z1/10,  D1/10(2),       D10E40(2),            .06
003882 :      , CONT  , D10E*40(2),     EOR,                                  .03
003883 :      , CONT  , VMASKE1,VMASKE2,VW1,    VW2,    VH,     VP            .06
003884 :      , CONT  , SPACE,  EX645,  K3HREC, K3HADD,                       .04
003885 :      , CONT  , MAXEXNZ,FTXT(14),       GRZAHL, ISOSP,  NBIT45, BIT46 .19
003886 :      , CONT  , BIT45,  BIT2/47,NZL,    KRECORD,A4,     BIT46/48,     .06
003887 :  *0: , BLOCK , SYMB,   ENDE,  KUSP/USP,KNEXTLET,PW,  KWSY,     SFAKT,
003888 :      , CONT  , AUSG2,  D,      NSP,    CENC,   NSYMB                 .05
003889 :      , CONT  , II13,   ER/AB,  ADCONT, ADR/W,  ADEND,  FORMAT, LUN,
003890 :      , CONT  , I/O,    GLREG(7),       BEGREC, ADNBPROG,       (1)   .11
003891 :      , CONT  , RECORD(25),     GWF(5), EAD(5), FTT,    SWUEBERL, WF  .38
003892 :      , CONT  , W,      NIW,    KBASP,  STLUN,  AC,                   .05
003893 :      , CONT  , INOUT,  LUN*,   L/NLGLRE,       KOFSPEC,        TRWADR,
003894 :      , CONT  , FNR,    II15,   NSP3,   ZHLR,   FTBE/SONST,           .05
003895 :  FT*521  :   , ENTRY ,.            EROEFFNUNG BEI READ (LUN,FT) LISTE
003896 :  BCDREAD*:   , BSS   ,
003897 :              ,  XTS  ,
003898 :           I12,  VTM  , CR3
003899 :  CR1     :15 ,  ATX  ,
003900 :           I14,  VJM  , RGTRI13*
003901 :              ,  XTA  ,
003902 :              ,  UTC  , CR2
003903 :              ,  XTS  ,
003904 :              ,  UJ   , BCDBEG*
003905 : C----------------------------------------------------------------------
003906 :  BCDDEC* :   , ENTRY ,
003907 :  FT*531  :   , ENTRY ,             EROEFFNUNG BEI DECODE (C,FT,V) LISTE
003908 :           I12, VTM   , CR4
003909 :           I12,  XTS  ,
003910 :  CR2     :   , 30    , CR1
003911 :              , UTC   , R/*
003912 : C----------------------------------------------------------------------
003913 :  FT*541  :   , ENTRY ,.             EROEFFNUNG BEI READ FT, LISTE
003914 :           12 , VTM   , =I50
003915 :              , ITA   , 12.
003916 :              , ITS   , 14
003917 :              , UJ    , BCDREAD*
003918 :  CR3     :   ,  ISO  , 12H BCDREAD* ''''
003919 :  CR4     :   ,  ISO  , 12H BCDDEC* ''''''
003920 : C
003921 : C** FPACK-KONV2N*
003922 : C                  KONVERTIERUNG TEIL 2
003923 : C                  EIGENTLICHE KONVERTIERUNG VON R- UND I-ZAHLEN
003924 : C                  ( EINFACHE GEAUIGKEIT )
003925 : C
003926 : C
003927 :  I1      :   , EQUAL ,  1
003928 :  I3      :   , EQUAL , 3
003929 :  I4      :   , EQUAL , 4
003930 :  I5      :   , EQUAL , 5
003931 :  I8      :   , EQUAL , 8
003932 :  I9      :   , EQUAL , 9
003933 :  I10     :   , EQUAL , 10
003934 :  I11     :   , EQUAL , 11
003935 :  EX500   :   , EQUAL , VGR + 3
003936 :  SFAKTOR :   , EQUAL , -6
003937 :  FPF*SFF :   , EQU   , IOCONT* + 2
003938 :  FPF*AAFR:   , EQU   , IOCONT* + 3
003939 :  KONV2N* :   , BSS   ,
003940 :              , NTR   , 3                  BLOCK. RUND/NORM
003941 :           15 , XTA   , -4                 L: MANTN
003942 :              , E+N   , 64 + 40
003943 :           15 , ATX   , -4
003944 :           15 , XTA   , -3                 L: MANTM
003945 :           I1 , AOX   , EX500
003946 :           15 , ATX   , -3
003947 :           15 , XTA   , -2                 L: MANTH
003948 :              , E+N   , 64 + 60
003949 :              , E+N   , 64 + 60
003950 :              , NTR   ,                    NORM/RUND ERLAUBT
003951 :              ,  A+X  ,
003952 :           15 , A+X   , -3                 + MANTM
003953 :           15 , A+X   , -4                 + MANTN
003954 :              ,  ASN  , 64 + 41
003955 :              , ATI   , I5                 I5=EXPF:=EXP(MANTN) +40
003956 :              , YTA   ,
003957 :              , ASN   , 64 + 7
003958 :              , UZA   , K2NULL
003959 :           I1 , AOX   , EXP64
003960 :              , NTR   , 3                  BLOCK. RUND/NORM
003961 :           I11, VZM   , K2A                I11=VMANT
003962 :              , X-A   ,                    MANTN:=-MANTN
003963 :  K2A     :15 , ATX   , -4                 S: MANTN
003964 :           15 , XTA   , -1                 L: EXP10
003965 :           I10, VZM   , K2B                I10=VEXP10
003966 :              , X-A   ,                    EXP:=-EXP10
003967 :  K2B     :I3 , V1M   , K2C                I3=1 DH PUNKT IN MANTISSE
003968 :           I8 , A-X   , D
003969 :  K2D     :15 ,  A-X  , SFAKTOR
003970 :              ,  UZA  , K2E
003971 :           I12, VTM   , 1                  D:=D2
003972 :           I3 , VTM   , -3                 S:=-3
003973 :  K2F     :   , ATI   , I11
003974 :           I1 , A+X   , MAXEXNZ
003975 :              , U1A   , K2GROBT.    SPR, WENN E GROESSER GLEICH MAXEXNZ
003976 :           I1 , XTA   , REAL1
003977 :           I9 , VTM   ,                    Q:=Q1
003978 :              , NTR   , 2                  BLOCK. RUND
003979 :           I11, VLM   , K2ZYK
003980 :  K2H     :   , NTR   ,                    RUND/NORM ERLAUBT
003981 :           I9 , VLM   , K2I1
003982 :  K2I     :I12, V1M   , K2J
003983 :           15 , A*X   , -4                 * MANTN
003984 :  K2K     :15 , ATX   , -4
003985 :              , ASN   , 64 + 41
003986 :              , ITS   , I5
003987 :           15 , ARX   ,
003988 :           15 , ATX   , -2
003989 :           I1 , AAX   , BIT8/15
003990 :              , U1A   , K2L
003991 :           15 , XTA   , -4
003992 :           I1 , AAX   , BIT1/41
003993 :           15 , XTS   , -3                 L: ERGEXP
003994 :              , ASN   , 64 - 41
003995 :           15 , AOX   ,
003996 :  K2NULL  :I4 , VZM   , K2N
003997 :           I8 , ATX   , INOUT
003998 :              , NTR   , 2
003999 :           I1 , A-X   , EX645
004000 :              , UZA   , K2F2
004001 :           I8 , XTA   , INOUT
004002 :              , NTR   , 3                  BLOCK RUND/NORM
004003 :           I1 , A+X   , EX640
004004 :  K2N     :I8 , ATX   , INOUT
004005 :              , NTR   , 2
004006 :           15 , UTM   , -6
004007 :           15 , WTC   ,
004008 :              , UJ    ,
004009 :  '--------------------------------
004010 :  K2C     :15 , A+X   , -5                 + EXP10A
004011 :              , UJ    , K2D
004012 :  '--------------------------------
004013 :  K2E     :I12, VTM   ,                    D:=D1
004014 :           I3 , VTM   , 3                  S:=3
004015 :              , X-A   ,
004016 :              , UJ    , K2F
004017 :  '--------------------------------
004018 :  K2ZYK       :I14,  VTM  ,-16
004019 :           I1 , XTA   , REAL1
004020 :  K2ZYK1  :I14, VZM   , K2G
004021 :           I14, UTM   , 1
004022 :           I1 , A*X   , REAL5/4
004023 :           I3 , J+M   , I5
004024 :           I11, VLM   , K2ZYK1
004025 :              , UJ    , K2H
004026 :  '--------------------------------
004027 :  K2G     :I9 , UTM   , -1
004028 :              , UJ    , K2ZYK
004029 :  '--------------------------------
004030 :  K2I1    :I1 , A*X   , R5/4H13
004031 :           I9 , VLM   , K2I1
004032 :              ,  UJ   , K2I
004033 :  '--------------------------------
004034 :  K2GROBT :   , XTA   ,
004035 :           I12, V1M   , K2NULL             SPR, WENN D = D2 DH EXP NEG
004036 :  K2F1    :I12, 31    , FPF*AAFR   .UEBERLAUF BEI EINER REAL- ODER
004037 :              , UTC   , 221 B.      INTEGER-ZAHL
004038 :            I1,   XTA   , GRZAHL
004039 :            15, AVX   , -4
004040 :              , UJ    , K2N
004041 :  '--------------------------------
004042 :  K2J     :15 , XTS   , -5                 L: MANTN
004043 :           15 , A/X   ,
004044 :              , UJ    , K2K
004045 :  '--------------------------------
004046 :  K2L     :I1 , AAX   , BIT15
004047 :              , UZA   , K2F1
004048 :  K2NULL1 :   , XTA   ,
004049 :              , UJ    , K2NULL
004050 :  '--------------------------------
004051 :   K2F2    :I12, 31    ,FPF*AAFR   . UEBERLAUF BEI EINER INTEGER-ZAHL
004052 :              , UTC   , 222 B.
004053 :            I8, XTA   , INOUT
004054 :              , UJ    , K2N
004055 : C
004056 : C** FPACK-R/*
004057 : C                  BEARBEITUNG DER SPEZIFIKATIONEN / NX WH OW AW LW
004058 : C                  BEI DER EINGABE
004059 : C
004060 : C
004061 :  I2      :   , EQUAL , 2
004062 :  I6       :  , EQUAL , 6
004063 :  I7      :   , EQUAL , 7
004064 :  I13     :   , EQUAL , 13
004065 :  IR      :   , EQUAL , 4
004066 :  C       :   , EQU   , 1
004067 :  A       :   , EQU   , 8
004068 :  S       :   , EQU   , 15
004069 :  KONV1 * :   , SUBP  ,
004070 :  READSY2 :   ,  EQU  , IOCONT* + 5
004071 :  READSY2F:   ,  EQU  , IOCONT* + 6
004072 :  READSY3 :   , EQU   , IOCONT* + 13
004073 :  VPL     :   , EQUAL , CT + 43
004074 :  VMI     :   , EQUAL , CT + 45
004075 :  FPF*AFF :   , EQU   , FPF*SFF
004076 :  R/*     :   , BSS   ,
004077 :              , UJ    , R/                  /
004078 :          :I12, VTM   , RNX1                NX
004079 :              , UJ    , RNX
004080 :          :   , UJ    , RWH                WH
004081 :          :   , UTC   ,                    NP (FRUEHER)
004082 :          :IR , VTM   ,                     EW.D      B:=B1
004083 :              , UJ    , REWD
004084 :          :IR , VTM   ,                     FW.D      B:=B1
004085 :              , UJ    , RFWD
004086 :          :IR , VTM   ,                     DW.D      B:=B1
004087 :              , UJ    , RDWD
004088 :          :IR , VTM   , 1                   IW        B:=B2
004089 :              , UJ    , REWD
004090 :          :I12, VTM   , ROW5                OW
004091 :              , UJ    , ROW
004092 :          :I12, VTM   , RAW4                1W
004093 :              , UJ    , RAW
004094 :          :I12, VTM   , RLW5               LW
004095 :              , UJ    , RLW
004096 :          :   ,  UJ   , R/                  FORMATENDE
004097 : C-----------------------------------------------------------------------
004098 :  RNX     :I12, XTA   ,
004099 :           I8 , ATX   , AUSG2
004100 :           I8 , WTC   , D
004101 :           I6 , VTM   ,
004102 :           I12, VTM   , READSY2
004103 :           I12, UJ    ,
004104 :  RNX1    :I13,  30   ,
004105 :              , UTC   , RNX1
004106 : C-----------------------------------------------------------------------
004107 :  R/      :   , BSS   ,
004108 :              , ITA   , I13
004109 :           15 , ATX   ,
004110 :           I8 , XTA   , CENC
004111 :              , U1A   , R/DECODE
004112 :              ,  ITA  , I8
004113 :           15 ,  ATX  ,
004114 :  I8,WTC,STLUN
004115 :  ,UJ,*+1
004116 :  :,UJ,R/ML.MЛ-MБ
004117 :  :I14,VTM,READ*.C KAPT
004118 :  ,UJ,R/ST
004119 :  :I14,VTM,TTREAD.C TEPMИHAЛA
004120 :  ,UJ,R/ST
004121 :  R/ML:,BSS,
004122 :           15 , XTA   , -1
004123 :              , ATI   , I8
004124 :            I8, WTC   , LUN
004125 :            13, VTM   ,
004126 :              , ITS   , 13
004127 :   ,UTC,=:41
004128 :   ,AOX,
004129 :   ,CALL,FMRTAPE
004130 :           I8 , UTC   , RECORD
004131 :           I13, VTM   ,
004132 :              , ITS   , I13
004133 :           I13, UTM   , 24
004134 :              , ITS   , I13
004135 :   ,ASN,64-24
004136 :   15,AOX,
004137 :   ,UTC,=:007
004138 :   ,AOX,
004139 :   ,CALL,FMRTAPE
004140 :  R/1     :15 ,  XTA  ,
004141 :              ,  ATI  , I8
004142 :           I8 , UTC   , RECORD
004143 :           I2 , VTM   ,
004144 :  R/2     :I7 , VTM   , -6
004145 :              , XTA   ,
004146 :            A , ATX   , NSP
004147 :           15 , WTC   ,
004148 :              , UJ    ,
004149 :  '--------------------------------
004150 :  R/ST    :I8 , UTC   , RECORD
004151 :           I13, VTM   ,
004152 :              , ITS   , I13
004153 :  I14,UTC,
004154 :  13,VJM,
004155 :              , UJ    , R/1
004156 :  '--------------------------------
004157 :  R/DECODE:I8 , XTA   , LUN
004158 :           I8 , ATX   , BEGREC
004159 :              , ATI   , I2
004160 :           I8 , XTA   , CENC
004161 :           I8 , ATX   , NSYMB
004162 :            I1, ARX   , A5
004163 :            I1, AOX   , EX640
004164 :              , A+X   ,
004165 :            I1, A/X   , SECHS
004166 :              , NTR   , 3
004167 :            I1, A+X   , EX640
004168 :            I1, AAX   , A77777
004169 :            I8, A+X   , LUN
004170 :              , NTR   , 2
004171 :            I8, ATX   , LUN
004172 :              , UJ    , R/2
004173 : C-----------------------------------------------------------------------
004174 :  REWD    :   , ITA   , I13
004175 :              , XTS   ,
004176 :  REWD1   : 15, ATX   ,                    EXP10:=0
004177 :            I8, WTC   , W
004178 :            I6, VTM   ,
004179 :              , UTC   , REWD2
004180 :              , XTA   ,
004181 :            I8, ATX   , AUSG2
004182 :  REWD2   :   , 30    , KONV1*
004183 :              , UTC   , KONV2N*
004184 : C-----------------------------------------------------------------------
004185 :  RFWD    :   , ITA   , I13
004186 :            I8, XTS   , SFAKT
004187 :              , UJ    , REWD1
004188 :  '--------------------------------
004189 :  RDWD    :I12, 31    , FPF*AFF.    ES TRITT EINE D-SPEZIFIKATION BEI
004190 :              , UTC   , 40 217B    .DER EINGABE EINER EINFACHGENAUEN
004191 :              , UJ    , REWD.       ZAHL AUF
004192 : C-----------------------------------------------------------------------
004193 :  ROW     :   , XTA   ,
004194 :           I8 , ATX   , INOUT               INOUT := 0
004195 :           I12, XTA   ,
004196 :           I8 , ATX   , AUSG2               AUSG2 := ROW4
004197 :           I8 , WTC   , W
004198 :           I6 , VTM   ,
004199 :           I11, VTM   ,                     VZ:= +
004200 :           I12, VJM   , READSY2F
004201 :           I1 , AEX   , VPL
004202 :              , UZA   , ROW1
004203 :           I8 , XTA   , SYMB
004204 :           I1 , AEX   , VMI
004205 :              , U1A   , ROW2
004206 :           I11, VTM   , 1                   VZ:= -
004207 :  ROW1    :I12, VJM   , READSY2F
004208 :  ROW2    :I8 , XTA   , SYMB
004209 :           I1 , AAX   , BIT10
004210 :              , UZA   , ROWF
004211 :           I8 , XTA   , SYMB
004212 :           I1 , AAX   , A17
004213 :           I8 , XTS   , INOUT
004214 :              , ASN   , 64 - 3
004215 :           15 , AOX   ,
004216 :           I8 ,  ATX  , INOUT
004217 :              ,  UJ   , ROW1
004218 :  '--------------------------------
004219 :  ROW4    :I11, VZM   , ROW5
004220 :           I8 , XTA   , INOUT
004221 :           I1 , AEX   , BIT1/48
004222 :           I8 , ATX   , INOUT
004223 :  ROW5    :I13,  30   ,
004224 :              , UTC   , ROW4
004225 :  '--------------------------------
004226 :  ROWF    :I12, 31    , FPF*AAFR   .VON OKTALZIFFER VERSCHIEDENES SYMBOL
004227 :              , UTC   , 223 B.      TRITT IN OKTALZAHL AUF
004228 :              , UJ    , ROW1
004229 : C-----------------------------------------------------------------------
004230 :  RAW     :I12, XTA   ,
004231 :           I8 , ATX   , AUSG2
004232 :           I8 , WTC   , W
004233 :           I6 , VTM   ,
004234 :           I11, VTM   , -6
004235 :  RAW1    :I8 ,  ATX  , INOUT
004236 :           I12,  VJM  , READSY2
004237 :           I1 , AAX   , A377
004238 :           I8 , XTS   , INOUT
004239 :              , ASN   , 64 - 8
004240 :           15 , AOX   ,
004241 :           I11, VZM   , RAW1
004242 :           I11, VLM   , RAW1
004243 :  '--------------------------------
004244 :  RAW3    :   ,  ASN  , 64 - 8
004245 :           I1 ,  AOX  , SPACE
004246 :  RAW2    :I11,  VLM  , RAW3
004247 :           I8 ,  ATX  , INOUT
004248 :  RAW4    :I13,  30   ,
004249 :              ,  UTC  , RAW2
004250 :  '--------------------------------
004251 :  RLW     :I11, VTM   ,                    I11 := FALSE
004252 :           I14, VTM   , RLW2
004253 :           I12, XTA   ,                    STELLE VON AUSG2, I6 FUER
004254 :           I8 , ATX   , AUSG2                               READSY2
004255 :           I8 , WTC   , W
004256 :           I6 , VTM   ,
004257 :           I14, UJ    ,
004258 :  RLW2    :I12, VJM   , READSY2F
004259 :              , ATI   , I5
004260 :           I5 , UTM   , - 124 B            TEST: SYMB = T
004261 :           I5 , V1M   , RLW3
004262 :           I11, VTM   , 1                  I11 := TRUE
004263 :  RLW3    :I12, VTM   , READSY2
004264 :           I12, UJ    ,
004265 :  '--------------------------------
004266 :  RLW4    :   , ITA   , I11
004267 :           I8 , ATX   , INOUT
004268 :  RLW5    :I13, 30    ,
004269 :              , UTC   , RLW4
004270 :  '--------------------------------
004271 :  RWH     : A , XTA   , ZHLR               RETTEN: ZHLR
004272 :            A , XTS   , W
004273 :            A , ATX   , ZHLR               ZHLR STEHT AUF 1. SYMB D KETTE
004274 :            A , WTC   , D
004275 :           I6 , VTM   ,                    I6:= LAENGE DER H-KETTE
004276 :            A , WTC   , ZHLR
004277 :              , XTA   ,
004278 :            A , ASX   , ZHLR
004279 :              , ASN   , 64 + 8
004280 :            A , ATX   , PW
004281 :  RWH1    :I12, VJM   , READSY2            I6 WIRD IN READSY2 ERNIEDRIGT
004282 :            C , AAX   , A377
004283 :            A , XTS   , PW
004284 :              , ASN   , 64 - 8
004285 :            S , AOX   ,
004286 :            A , ATX   , PW
004287 :           I6 , VZM   , RWH2               WENN ALLE SYMB IN KETTE ERSETZ
004288 :            A , XTA   , ZHLR
004289 :            C , ARX   , NBIT45
004290 :            A , ATX   , ZHLR
004291 :              , UZA   , RWH1               WENN BIT48=L, NOCH SYM IM WORT
004292 :            C , ARX   , NZL
004293 :            A , ATX   , ZHLR
004294 :            A , XTA   , PW
004295 :            A , WTC   , ZHLR
004296 :              , ATX   , -1
004297 :              , UJ    , RWH1
004298 :  RWH2    : A , XTA   , ZHLR
004299 :            C , ARX   , NBIT45
004300 :              , U1A   , RWHEND             WENN BIT48=0, WORT LEER
004301 :           I12, VJM   , READSY3
004302 :            C , AAX   , A377
004303 :            A , XTS   , PW
004304 :              , ASN   , 64 - 8
004305 :            S , AOX   ,
004306 :            A , ATX   , PW
004307 :              , UJ    , RWH2
004308 :  RWHEND  : A , XTA   , PW
004309 :            A , WTC   , ZHLR
004310 :              , STX   ,
004311 :            A , ATX   , ZHLR
004312 :           I13, UJ    ,
004313 :  ,END,
004314 :  BCDWRIT*:   , NAME  ,               FPACK                     06.04.73
004315 : C
004316 : C** FPACK-BCDWRIT*(FT,LUN)
004317 : C                  BENUTZERROUTINE ZUR EROEFFNUNG EINER FORMAT-
004318 : C                  GESTEUERTEN AUSGABE
004319 : C
004320 : C
004321 :  I1      :   , EQUAL ,  1
004322 :  I12     :   , EQUAL , 12
004323 :  I14     :   , EQUAL , 14
004324 :  *0      :   , EQUAL ,
004325 :  *0: , BLOCK , A1,     A13,    A17,    A1750,  A17700, A20077, A377
004326 :   ,CONT,A5,A60,A77777,BIT1/6,BIT1/7,BIT2/48  .06
004327 :      , CONT  , BIT1/40,BIT1/41,BIT10/11,       BIT10/14,       BIT1/48
004328 :      , CONT  , BIT12,  BIT10,  BIT12/14,       BIT12/15,       BIT14
004329 :      , CONT  , BIT15,  BIT16,  BIT17,  BIT13/48,       BIT16/41,
004330 :      , CONT  , BIT3/4,BIT37/48,        BIT41/48                      .03
004331 :      , CONT  , BIT43/48,       BIT48(0),       EXP64,  BIT7,         .03
004332 :      , CONT  , BIT8,   BIT8/15,BIT9/48,VGR0,   EX640(0),       VGR(6).10
004333 :   ,CONT,CT(64),REAL1(0),DEINS(2)    .66
004334 :   ,CONT,E1(0),D10(2),R1/2(0),D1/2(2),REAL5/4,R5/4H13
004335 :   ,CONT,EKSPO(19),
004336 :      , CONT  , SECHS,  Z1/10,  D1/10(2),       D10E40(2),            .06
004337 :      , CONT  , D10E*40(2),     EOR,                                  .03
004338 :      , CONT  , VMASKE1,VMASKE2,VW1,    VW2,    VH,     VP            .06
004339 :      , CONT  , SPACE,  EX645,  K3HREC, K3HADD,                       .04
004340 :      , CONT  , MAXEXNZ,FTXT(14),       GRZAHL, ISOSP,  NBIT45, BIT46 .19
004341 :      , CONT  , BIT45,  BIT2/47,NZL,    KRECORD,A4,     BIT46/48,     .06
004342 :  *0: , BLOCK , SYMB,   ENDE,  KUSP/USP,KNEXTLET,PW,  KWSY,     SFAKT,
004343 :      , CONT  , AUSG2,  D,      NSP,    CENC,   NSYMB                 .05
004344 :      , CONT  , II13,   ER/AB,  ADCONT, ADR/W,  ADEND,  FORMAT, LUN,
004345 :      , CONT  , I/O,    GLREG(7),       BEGREC, ADNBPROG,       (1)   .11
004346 :      , CONT  , RECORD(25),     GWF(5), EAD(5), FTT,    SWUEBERL, WF  .38
004347 :      , CONT  , W,      NIW,    KBASP,  STLUN,  AC,                   .05
004348 :      , CONT  , INOUT,  LUN*,   L/NLGLRE,       KOFSPEC,        TRWADR,
004349 :      , CONT  , FNR,    II15,   NSP3,   ZHLR,   FTBE/SONST,           .05
004350 :  IOCONT* :   , SUBP  ,
004351 :  RGTRI13*:   , EQU   , IOCONT* + 10
004352 :  BCDBEG* :   , SUBP  ,
004353 :  FT*621  :   , ENTRY ,.            EROEFFNUNG BEI WRITE (LUN,FT) LISTE
004354 :  BCDWRIT*:   , BSS   ,
004355 :              , XTS   ,
004356 :           I12, VTM   , CW3
004357 :  CW1     :15 , ATX   ,
004358 :           I14, VJM   , RGTRI13*
004359 :           I1 , XTA   , A1
004360 :              , UTC   , CW2
004361 :              , XTS   ,
004362 :              , UJ    , BCDBEG*
004363 :  BCDENC* :   , ENTRY ,
004364 :  FT*611  :   , ENTRY ,             EROEFFNUNG BEI ENCODE (C,FT,V) LISTE
004365 :           I12, VTM   , CW4
004366 :           I12, XTS   ,
004367 :  CW2     :   , 30    , CW1
004368 :              , UTC   , W/*
004369 : C----------------------------------------------------------------------
004370 :  FT*571  :   , ENTRY ,.            EROEFFNUNG BEI PRINT FT, LISTE
004371 :           12 , VTM   , =I51
004372 :  CW5     :   , ITA   , 12
004373 :              , ITS   , 14
004374 :              , UJ    , BCDWRIT*
004375 : C----------------------------------------------------------------------
004376 :  FT*561  :   , ENTRY ,.            EROEFFNUNG BEI PUNCH FT, LISTE
004377 :           12 , VTM   , =I52
004378 :              , UJ    , CW5
004379 :  CW3     :   , ISO   , 12H BCDWRIT* ''''
004380 :  CW4     :   , ISO   , 12H BCDENC* ''''''
004381 : C
004382 : C** FPACK-NEXTLET*
004383 : C                  BEREITSTELLEN DER NAECHSTEN ZIFFER DER
004384 : C                  REUCKKONVERTIERTEN ZAHL
004385 : C
004386 : C
004387 :  A       :   , EQUAL , 8
004388 :  C       :   , EQUAL , 1
004389 :  M       :   , EQUAL , 11
004390 :  R       :   , EQUAL , 14
004391 :  S       :   , EQUAL ,15
004392 :   I:,EQU,12
004393 :   ZY:,EQU,-2
004394 :   ZRRK:,EQU,-4
004395 :   Z1RK:,EQU,-6
004396 :   ZRK:,EQU,-7
004397 :   ZRE0:,EQU,-10
004398 :   E0:,EQU,EKSPO+9
004399 :   E10:,EQU,EKSPO+14
004400 :   ZH18:,EQU,EKSPO+18
004401 :  ZR      :   , EQUAL , -3
004402 :  NEXTLET*:   , ENTRY ,
004403 :   NEXTLET*:,BSS,
004404 :   NEXTLET:,BSS,    .NEXT DIGITS OR DIGIT
004405 :   M,V1M,LET
004406 :   ,ITA,R           .PREPARATION  OF  NEXT  DIGITS
004407 :   S,ATX,
004408 :    12,VTM,
004409 :   S,XTA,ZY
004410 :   ,U1A,NOTF        .NOT  FIRST  ENTRY
004411 : C
004412 : C   COEFFICIENT  OF  Z  IN  Z1RK
004413 : C
004414 :   ,NTR,3
004415 :   S,XTA,Z1RK
004416 :   ,ASN,64+41       .GET  EXP
004417 :   ,X-A,
004418 :   ,ATI,R
004419 :   M,VTM,-1
004420 :   S,UTM,2
004421 :   PRAD:S,UTM,-1    .PREPARATION  OF  COEFFICIENTS
004422 :   S,XTA,Z1RK
004423 :   C,AAX,BIT1/40
004424 :   R,ASN,64+64
004425 :   S,ATX,Z1RK
004426 :   M,VLM,PRAD
004427 :   S,XTA,ZRRK
004428 :   ,ATI,R
004429 :   LOOP:R,MTJ,M
004430 :    R,UTM,-13      .N2=N1-13
004431 :   ,ITA,R
004432 :   C,AAX,BIT15
004433 :   ,U1A,LT12       .N1.LT.12
004434 :   R,UTM,1
004435 :   ,ITA,R
004436 :   M,VTM,12
004437 :   LT12:S,ATX,ZY
004438 :   ,ITA,M
004439 :   S,ATX,ZRRK
004440 :   ,UTC,INT.ПEPEЬBAT
004441 :   ,XTA,.ДЛЯ ЦEЛOГO
004442 :   ,U1A,M2.ЧИCЛA
004443 :   R,VTM,-4
004444 :   ,XTA,
004445 :   S,ATX,ZRK
004446 :   CYCL:S,XTA,ZRE0
004447 :   ,UZA,CYCL1
004448 :   S,UTM,1
004449 :   I,VTM,-1
004450 :   CYCL1:S,XTA,Z1RK
004451 :   I,VZM,CARRY
004452 :   ,ASN,64+40
004453 :   CAR:S,ARX,ZRK
004454 :   S,ATX,ZRK
004455 :   S,XTA,Z1RK
004456 :   C,AAX,BIT1/40
004457 :   ,ASN,64-1
004458 :   S,ATX,
004459 :   ,ASN,64-2        .*8
004460 :   S,ARX,
004461 :   S,ATX,Z1RK
004462 :   S,UTM,-1
004463 :   I,VLM,CYCL1
004464 :   S,UTM,1
004465 :   M,UTM,-1
004466 :   M,V1M,CYCL
004467 :    M3:,BSS,
004468 :   S,XTA,ZRRK
004469 :   ,ATI,M
004470 :   S,XTA,
004471 :   ,ATI,R
004472 :   ,UJ,LET
004473 : C
004474 : C** FPACK-RK*
004475 : C                  EIGENTLICHE RUECKKONVERTIERUNG
004476 : C
004477 : C
004478 :  Z2RK    :   , EQUAL , -5
004479 :  ZY1     :   , EQUAL , -2
004480 :  RK*     :   , ENTRY ,
004481 :  RK*     :   , BSS   ,
004482 :   LET:S,XTA,ZRK+1
004483 :   ,ASN,64-4
004484 :   S,ATX,ZRK+1
004485 :   M,UTM,-1
004486 :   ,YTA,
004487 :   C,AOX,A60
004488 :   A,ATX,SYMB
004489 :   R,UJ,
004490 :   NOTF:,ATI,R
004491 :   I,VTM,
004492 :   ,UJ,LOOP
004493 :   CARRY:C,AAX,BIT41/48
004494 :   R,ASN,64
004495 :   R,UTM,4
004496 :   ,UJ,CAR
004497 : C
004498 : C** FPACK-WEWD*
004499 : C                  BEARBEITUNG DER SPEZIFIKATION EW.D BEI DER AUSGABE
004500 : C
004501 : C
004502 :  EF      :   , EQUAL , 10
004503 :  RI      :   , EQUAL , 9
004504 :  ID      :   , EQUAL , 3
004505 :  R3      :   , EQUAL , 13
004506 :  R2      :   , EQUAL , 12
004507 :  SF      :   , EQUAL , 4
004508 :  K       :   , EQUAL , 4
004509 :  L       :   , EQUAL , 4
004510 :  R1      :   , EQUAL , 14
004511 :   ZRE:,EQU,-9
004512 :   EKSP:,EQU,-8
004513 :  VZM     :   , EQUAL , -7
004514 :  Z       :   , EQUAL , -6
004515 :  Z1      :   , EQUAL , -5
004516 :   SFT:,EQU,-10
004517 :  MI      :   , EQUAL , CT + 45
004518 :  NULL    :   , EQUAL , CT + 48
004519 :  PL      :   , EQUAL , CT + 43
004520 :  PT      :   , EQUAL , CT + 46
004521 :  SP      :   , EQUAL , CT + 32
004522 :  NVP     :   , EQUAL , 3
004523 :  WE      :   , EQUAL , 4
004524 :  E       :   , EQUAL , 5
004525 :  FPF*AFF :   , EQUAL , IOCONT* + 2
004526 :  FPF*ABBR:   , EQU   , IOCONT* + 2
004527 :  WEWD*   :   , BSS   ,
004528 :              , XTA   ,
004529 :   M,VTM,-8
004530 :          :S  , ATX   ,
004531 :           M  , VLM   , *
004532 :            A ,  ATX  , KNEXTLET
004533 :           A  , XTA   , INOUT
004534 :              , NTR   , 23 B
004535 :              , UZA   , WE1
004536 :   C,AEX,30
004537 :   ,U1A,WE/
004538 :   C,AOX,
004539 :   WE/:C,AEX,30
004540 :           A  , AVX   , INOUT
004541 :           A  , ATX   , INOUT
004542 :           S  , ATX   , VZM
004543 :  WE1     :   ,  BSS  ,
004544 :              ,  NTR  , 2
004545 :              , A+X   ,
004546 :              , NTR   , 7
004547 :              , UZA   , WENULL
004548 :           RI , V1M   , WEI1
004549 :   EF,VZM,WEF2   .IF E FORMAT
004550 :           R1 , VTM   , WE41
004551 :   A,XTA,INOUT
004552 :   WEX:S,ATX,ZR
004553 :   E,VTM,18
004554 :   C,X-A,ZH18
004555 :   S,UTM,1
004556 :   ,U1A,WEX2
004557 :   S,UTM,-1
004558 :   A,XTA,INOUT
004559 :   ,NTR,2
004560 :   ,A+X,
004561 :   ,ASN,64+44
004562 :   ,ATI,L
004563 :   L,UTM,-8
004564 :   WEXI:S,XTA,ZR
004565 :   C,UTC,E0
004566 :   L,X-A,
004567 :   ,UZA,WEX0
004568 :   L,UTM,1
004569 :   ,UJ,WEXI
004570 :   WEX0:L,MTJ,E
004571 :   C,UTC,E0
004572 :   L,XTA,
004573 :   L,J+M,E
004574 :   WEX1:S,ATX,
004575 :   S,X-A,ZR-1
004576 :   ,UZA,WEX2
004577 :   S,XTA,
004578 :   E,UTM,-1
004579 :   C,A/X,E1
004580 :   ,UJ,WEX1
004581 :   WEX2:,ITA,E
004582 :   S,ATX,-1
004583 :   ,ASN,64+5
004584 :   ,STI,L
004585 :   L,VZM,WEX3
004586 :   C,AOX,BIT16/41
004587 :   WEX3:,NTR,3
004588 :   R1,UJ,
004589 :   WE41:,X-A,
004590 :   A,A-X,D
004591 :   S,A-X,SFT
004592 :   ,ASN,64+1
004593 :   ,ATI,5
004594 :   ,YTA,
004595 :   ,ASN,64+47
004596 :   ,ATI,4
004597 :   5,UTM,9
004598 :   ,ITA,5
004599 :   1,AAX,23
004600 :   ,UZA,WE5/
004601 :   ,XTA,
004602 :   ,UJ,WE5//
004603 :   WE5/:1,XTA,112
004604 :   1,J+M,5
004605 :   ,NTR,2
004606 :   5,A*X,116
004607 :   4,VZM,WE5//
004608 :   1,A*X,110
004609 :   WE5//:5,VTM,
004610 :   S,ATX,Z1
004611 :   WE6:,BSS,
004612 :           A  , XTA   , INOUT
004613 :           S  , ATX   , Z
004614 :   WE60:,BSS,
004615 :   S,XTA,Z
004616 :   R1,VJM,WEX
004617 :   S,ATX,
004618 :   ,X-A,
004619 :   ,ASN,64+1
004620 :   ,ATI,L
004621 :   C,J+M,L
004622 :   ,YTA,
004623 :   ,ASN,64+47
004624 :   ,ATI,R1
004625 :   WE601:,NTR,2
004626 :   S,XTA,Z-1       .R=R1*10**K
004627 :   L,A*X,E0      .R*10**((K/2)*2)
004628 :   R1,V1M,WE220
004629 :   WE602:S,A+X,Z1RK    .Z+RUND
004630 :   S,ATX,Z1RK    .Z IN Z1RK
004631 :   I,VTM,
004632 :   C,A-X,E1
004633 :   ,U1A,WE603
004634 :   S,XTA,Z1RK    .IF Z GT OR EQ  10
004635 :   C,A/X,E1    .Z=Z/10
004636 :   S,ATX,Z1RK
004637 :   C,XTA,A1
004638 :   ,UJ,WE605
004639 :   WE603:S,XTA,Z1RK
004640 :   C,A-X,E0
004641 :   ,UZA,WE604
004642 :   C,XTA,E0
004643 :   S,ATX,Z1RK
004644 :   WE604:,XTA,
004645 :   WE605:,NTR,3
004646 :   S,A+X,
004647 :   S,ATX,EKSP
004648 :  WE600   :RI , V1M   , WEI2
004649 :           EF , V1M   , WEF3
004650 :           WE ,  VTM  , 4
004651 :           NVP,  VTM  , 1
004652 :            S , XTA   , SFT
004653 :              , NTR   , 23 B
004654 :              , U1A   , WE2         .FALLS SKALENFKT NEGATIV
004655 :            S , WTC   , SFT
004656 :           NVP, UTM   ,
004657 :  WE2     : A , XTA   , D
004658 :              , U1A   , WE3         .FALLS D=0 BEI E-, F- SPEZ
004659 :           WE , UTM   , -1
004660 :  WE3     : S ,  XTA  , VZM
004661 :              ,  UZA  , WE31
004662 :           WE , UTM   , 1
004663 :  WE31    :NVP, J+M   , WE
004664 :              , ITA   , WE
004665 :           A  , ARX   , D
004666 :   ,ITS,NVP
004667 :   A,ARX,D
004668 :   S,STX,ZRRK        .N OF DIGITS
004669 :           A  , X-A   , W
004670 :           A  , ATX   , NSP
004671 :              , U1A   , WE12
004672 :              , X-A   ,
004673 :              , ATI   , K
004674 :           K  , VLM   , WE13
004675 :  WE61    : S , XTA   , VZM
004676 :              , UZA   , WE7
004677 :           C  , XTA   , MI
004678 :           R1 , VJM   , WSY*
004679 :  WE7     :NVP,  VZM  , WE14
004680 :           NVP, UTM   , -1
004681 :           NVP, VZM   , WE15
004682 :  WE71    :NVP, UTM   , 1
004683 :            E ,  UTM  , 1
004684 :  WE8     :R1 , VJM   , NEXTLET*
004685 :           A  , XTA   , SYMB
004686 :           R1 , VJM   , WSY*.    AUSG. DER ZIFFERN VOR DEM DEZ.-PUNKT
004687 :           NVP, UTM   , -1
004688 :           NVP, V1M   , WE8
004689 :  WE81    : A , XTA   , D
004690 :              , UZA   , WE21
004691 :              , ATI   , ID
004692 :           C  , XTA   , PT
004693 :           R1 , VJM   , WSY*.   AUSG. DES DEZ.-PUNKTS
004694 :  WE9     : R1, VJM   , NEXTLET*
004695 :           A  , XTA   , SYMB
004696 :           R1 , VJM   , WSY*.   AUSG. DER ZIFFERN NACH DEM DEZ.-PUNKT
004697 :           ID , UTM   , -1
004698 :           ID , V1M   , WE9
004699 :  WE10    :EF , V1M   , WE111
004700 :   S,XTA,EKSP
004701 :   ,NTR,3
004702 :   S,A-X,SFT
004703 :   S,ATX,EKSP
004704 :   ,U1A,WE18
004705 :   C,XTS,PL
004706 :   WE11:R1,VJM,WSY*
004707 :   EXP:,BSS,
004708 :   S,XTA,
004709 :   ,ATI,I
004710 :   I,UTM,-10
004711 :   ,ITA,I           .A=EXP-10
004712 :   C,AAX,BIT15
004713 :   ,UZA,EXP1        .EXP.GE.10
004714 :   S,XTA,EKSP       .EXP.LT.10
004715 :   S,ATX,ZR         .SECOND DIG OF EXP
004716 :   ,XTA,
004717 :   ,UJ,EXP2
004718 :   EXP1:,ITA,I
004719 :   S,ATX,ZR         .SECOND DIG OF EXP
004720 :   C,XTA,A1         .FIRST  DIGIT
004721 :   EXP2:C,AOX,A60
004722 :   R1,VJM,WSY*
004723 :            S , XTA   , ZR
004724 :            C , AAX   , A17
004725 :            C , AOX   , A60
004726 :           R1 , VJM   , WSY*
004727 :   WE111:S,UTM,-10
004728 :   ,XTA,
004729 :   ,UTC,INT
004730 :   ,ATX,
004731 :           R3 , UJ    ,
004732 :  '--------------------------------
004733 :  WE12    : C , XTA   , A77777
004734 :           A  , ATX   , KWSY
004735 :              , UJ    , WE61
004736 :  '--------------------------------
004737 :  WE13    : C , XTA   , SP
004738 :           R1 , VJM   , WSY*
004739 :           K  , VLM   , WE13
004740 :              , UJ    , WE61
004741 :  '--------------------------------
004742 :  WE14    : S , XTA   , SFT
004743 :              , NTR   , 23 B
004744 :              , U1A   , WE16
004745 :              , UJ    , WE81
004746 :  '--------------------------------
004747 :  WE15    : S , XTA   , SFT
004748 :              , NTR   , 23 B
004749 :              , UZA   , WE71
004750 :           C  , XTA   , NULL
004751 :           E  , UTM   , 1
004752 :           R1 , VJM   , WSY*
004753 :  WE16    : A , XTA   , D
004754 :              , UZA   , WE10
004755 :            C , XTA   , PT
004756 :           R1 , VJM   , WSY*
004757 :            S , WTC   , SFT
004758 :           SF , VTM   ,
004759 :           A  , WTC   , D
004760 :           ID , VTM   ,
004761 :  WE17    : ID, VZM   , WE10
004762 :           SF , UTM   , 1
004763 :           SF , VZM   , WE9
004764 :           C  , XTA   , NULL
004765 :           E  , UTM   , 1
004766 :           ID , UTM   , -1
004767 :           R1 , VTM   , WE17
004768 :              , UJ    , WSY*
004769 :  '--------------------------------
004770 :    WE18:,X-A,
004771 :   S,ATX,EKSP
004772 :   C,XTS,MI
004773 :   ,UJ,WE11
004774 :  '--------------------------------
004775 :  '--------------------------------
004776 :  '--------------------------------
004777 :  WEI1    : A , XTA   , INOUT
004778 :           S  , ATX   , Z
004779 :   1,AAX,30
004780 :   1,AEX,38
004781 :   ,UZA,M5
004782 :   1,XTA,
004783 :   8,A-X,68
004784 :   ,ATI,4
004785 :   WEI1/:1,XTA,88
004786 :    R1,VJM,WSY*
004787 :     4,VLM,WEI1/
004788 :      ,UJ, WE111
004789 :  '--------------------------------
004790 :  WEF2    :   , XTA   ,
004791 :              , UJ    , WE41
004792 :  '--------------------------------
004793 :  WEF3    :WE , VTM   , 1
004794 :            S , A+X   , SFT
004795 :              , UZA   , WEF6
004796 :           NVP, VTM   , 1
004797 :            S , ATX   , SFT
004798 :              , UJ   ,  WE2
004799 :  '--------------------------------
004800 :  WEI2    :WE , VTM   ,
004801 :  WEF6    : C ,  ARX  , A1
004802 :              , ATI   , NVP
004803 :              , XTA   ,
004804 :            S , ATX   , SFT
004805 :           RI , VZM   , WE2         .FALLS REAL-KONVERTIERUNG
004806 :              , UJ    , WE3
004807 :  '--------------------------------
004808 :  WE21    :RI , V1M   , WE111
004809 :              , UJ    , WE10
004810 :   WE220:C,A*X,E1   .IFODD
004811 :   ,UJ,WE602
004812 :   WEI3:,NTR,2
004813 :   S,XTA,Z
004814 :   C,A+X,E0-2
004815 :   S,ATX,Z
004816 :   ,UJ,WE60
004817 :  '--------------------------------
004818 :  WENULL  :  S, ATX   , Z
004819 :   S,ATX,EKSP
004820 :             C, XTS   , A60
004821 :             A, STX   , KNEXTLET
004822 :            S , ATX   , SFT
004823 :            E , VTM   ,                    EXP := 0
004824 :              , UJ    , WE600
004825 : C
004826 : C** FPACK-WSY*
004827 : C                  SCHREIBEN EINES SYMBOLS IN DEN AUSGABERECORD ODER
004828 : C                  INS FORMAT BEI WH-SPEZIFIKATION ( AUSGABE )
004829 : C
004830 : C
004831 :  J       :   , EQUAL , 12
004832 :  NWO     :   , EQUAL , 2
004833 :  NSG     :   , EQUAL , 6
004834 :  NSY     :   , EQUAL , 7
004835 :  STAR    :   , EQUAL , CT + 42
004836 :  WSY*    :   , ENTRY ,
004837 :  WSY*    :   , BSS   ,
004838 :            C , AAX   , A377
004839 :            A , ATX   , SYMB
004840 :            A , WTC   , KWSY
004841 :            J , VTM   ,
004842 :            J , VLM   , WSK2
004843 :            NSG, V1M   , WS0        .FALLS RECORD NOCH NICHT VOLL
004844 :              , ITS   , R3
004845 :              , ITS   , R
004846 :            S , ATX   ,
004847 :            R3 , VJM   , W/*        .AUSGABE DES VOLLEN RECORDS
004848 :            S  , XTA   ,
004849 :              , STI   , R
004850 :              , STI   ,R3
004851 :            A , ATX   , SYMB
004852 :  WS0     :NSG,  UTM  , -1
004853 :            A , XTA   , PW
004854 :              , ASN   , 64 - 8
004855 :            A , AOX   , SYMB
004856 :           NSY, VZM   , WSPWVOLL
004857 :            A ,  ATX  , PW
004858 :           NSY, UTM   , 1
004859 :            R  , UJ    ,
004860 :  WSK2    : J , V1M   , WSK3
004861 :            A , XTA   , NSP
004862 :            C , ARX   , A1
004863 :            C ,  AAX  , BIT1/41
004864 :            A , ATX   , NSP
004865 :            R , U1A   ,
004866 :            J , VTM   , -2.
004867 :              , ITA   , J
004868 :            A , ATX   , KWSY
004869 :            R , UJ    ,
004870 :  WSK3    :   , XTA   ,
004871 :            A , ATX   , KWSY
004872 :            C , XTA   , STAR
004873 :              , UJ    , WSY*
004874 :  WSPWVOLL:NWO, ATX   ,
004875 :           NSY, VTM   , -5
004876 :           NWO, UTM   , 1
004877 :            R  , UJ    ,
004878 : C
004879 : C** FPACK-W/*
004880 : C                  BEARBEITUNG DER SPEZIFIKATIONEN: / NX WH OW AW LW
004881 : C                  BEI DER AUSGABE
004882 : C
004883 : C
004884 :  STL     :   , EQU   , 6
004885 :  I9      :   , EQUAL , 9
004886 :  NX      :   , EQUAL , 9
004887 :  R4      :   , EQUAL , 14
004888 :  TF      :   , EQUAL , 9
004889 :  W2      :   , EQUAL , 4
004890 :  W1      :   , EQUAL , 5
004891 :  A2      :   , EQUAL , CT + 2
004892 :  PRINT8  :   ,  SUBP ,
004893 :  TTPRINT :   , SUBP  ,
004894 :  BCDPUN  :   , SUBP  ,
004895 :  EBCDPUN :   , SUBP  ,
004896 :  READSY3 :   , EQU   , IOCONT* + 13
004897 :  RSZRZST3:   , EQU   , IOCONT* + 15
004898 :  W/*     :   , BSS   ,
004899 :              , UJ    , W/                 /
004900 :          :   , UJ    , WNX                NX
004901 :          :   , UJ    , WWH                WH
004902 :          :   , UTC   ,                    NP  (FRUEHER)
004903 :          :EF , VTM   ,                    EW.D
004904 :              , UJ    , WEWD
004905 :          :EF , VTM   , 1                  FW.D
004906 :              , UJ    , WEWD
004907 :          :EF , VTM   ,                    DW.D
004908 :              , UJ    , WEWD
004909 :          :RI , VTM   , 1                  IW
004910 :              , UJ    , WIW
004911 :          :   , UJ    , WOW                OW
004912 :          :   , UJ    , WAW                AW
004913 :          :   , UJ    , WLW                LW
004914 : C                                         FORMATENDE
004915 :  W/      :   , ITA   , 13
004916 :           15 , ATX   ,
004917 :             A, XTA   , CENC
004918 :              , U1A   , W/ENC*
004919 :            A ,  WTC  , STLUN
004920 :           STL,  VTM  ,
004921 :           STL,  UJ   , * + 1              VERTEILERSPRUNG
004922 :          :   ,  UJ   , W/XPACK
004923 :          :STL,  VTM  , PRINT8             DRUCKER1
004924 :              ,  UJ   , W/DRUCK
004925 :          :STL, VTM   , BCDPUN             STANZER 1
004926 :              ,  UJ   , W/STANZ
004927 :          :STL, VTM   , PT8PUN             LB-STANZER 1
004928 :              ,  UJ   , W/LBST
004929 :          :STL, VTM   , TTPRINT            TELETYPE
004930 :              ,  UJ   , W/TTYPE
004931 :          :STL, VTM   , PRINT82            DRUCKER 2
004932 :              ,  UJ   , W/DRUCK            DRUCKER2
004933 :          :STL, VTM   , BCDPUN2            STANZER 2
004934 :              , UJ    , W/DRUCK
004935 :          :STL, VTM   , PT5PUN             LB-STANZER 2
004936 :              , UJ    , W/DRUCK
004937 :  W/XPACK :   ,  BSS  ,
004938 :            A , XTA   , PW
004939 :              , ASN   , 64 - 8
004940 :            C , AEX   , EOR                EOR SETZEN
004941 :  W/1     :NSY,  VLM  , W/SHIFT
004942 :           NWO,  ATX  ,                    LETZTES PW SPEICHERN
004943 :            A ,  UTC  , RECORD
004944 :           NWO,  VTM  ,
004945 :              ,  ITS  ,  NWO
004946 :              , ITS   ,  NWO
004947 :              ,  CALL , ISOTCOS1           PACKEN
004948 :            A ,  WTC  , LUN*
004949 :           NSY,  VTM  ,
004950 :   ,ITS,NSY
004951 :   ,UTC,=:42
004952 :   ,AOX,
004953 :   ,CALL,FMRTAPE
004954 :   ,ITS,14
004955 :   ,ASN,64-24
004956 :   ,ITS,NWO
004957 :   15,AOX,
004958 :   ,UTC,=:005
004959 :   ,AOX,
004960 :   ,CALL,FMRTAPE
004961 :  W/END   :NSG, VTM   , 149
004962 :           A  , UTC   , RECORD
004963 :           NWO, VTM   ,
004964 :  W/END*  :NSY, VTM   , -5
004965 :              , NTR   , 2
004966 :           15 , WTC   ,
004967 :              , UJ    ,
004968 :  W/SHIFT :   , ASN   , 64 - 8
004969 :              , UJ    , W/1
004970 : C----------------------------------------------------------------------
004971 :  W/STANZ :   , BSS   ,
004972 :  W/LBST  :   , BSS   ,
004973 :  W/TTYPE :   , BSS   ,                    TELETYPE
004974 :  W/DRUCK :   ,  BSS  ,                    DRUCKER 1, DRUCKER 2
004975 :            A , XTA   , PW
004976 :              , ASN   , 64 - 8
004977 :            C , AEX   , EOR                EOR SETZEN
004978 :  W/DR    :NSY, VLM   , W/SHIFT2
004979 :           NWO, ATX   ,
004980 :           A  , UTC   , RECORD
004981 :           13 , VTM   ,
004982 :              , ITS   , 13
004983 :              , ITS   , NWO
004984 :              , XTS   ,
004985 :           STL, UTC   ,
004986 :           13 , VJM   ,
004987 :           STL, UTM   , -BCDPUN
004988 :           STL, V1M   , W/END       .FALLS NICHT BCDPUN GERUFEN
004989 :           A  , XTA   , ER/AB
004990 :              , U1A   , W/END       .FALLS NOCH NICHT ABGESCHLOSSEN
004991 :           13 , VTM   , W/END
004992 :              , UJ    , EBCDPUN
004993 :  '--------------------------------
004994 :  W/SHIFT2:   , ASN   , 64 - 8
004995 :              , UJ    , W/DR
004996 : C----------------------------------------------------------------------
004997 :  W/ENC*  :R4 , MTJ   , R3
004998 :           R4 , VTM   , W/E
004999 :  W/E     :NSG, VZM   , W/E0        .FALLS RECORD VOLL
005000 :           C  , XTA   , SP
005001 :              , UJ    , WSY*        .AUFFUELLEN MIT SPACES
005002 :  W/E1    :NSY, UTM   , -5
005003 :           NSG, VTM   , 3           .NSG UNGLEICH 0  NOETIG FUER WSY*
005004 :           C  , XTA   , SP
005005 :           R4 , VJM   , WSY*        .AUFFUELLEN DES WORTRESTES
005006 :  W/E0    :NSY,  UTM  , 5
005007 :           NSY,  V1M  , W/E1        .FALLS WORTREST NOCH NICHT VOLL
005008 :           R3 , MTJ   , R4
005009 :            A , XTA   , CENC
005010 :              , ATI   , NSG                NSG:= C
005011 :              , UJ    , W/END*
005012 : C----------------------------------------------------------------------
005013 :  WWH     : A , XTA   , ZHLR               RETTEN: ZHLR, NSP3
005014 :            A , XTS   , W
005015 :            A , ATX   , ZHLR               ZHLR:= ANFANG DER H-KETTE
005016 :            A , XTA   , NSP3
005017 :              , XTS   ,
005018 :            A , ATX   , NSP3
005019 :            A , WTC   , D
005020 :            M , VTM   ,                    M:= LAENGE DER H-KETTE
005021 :           R2 , VJM   , RSZRZST3
005022 :  WHZYKL  : M , UTM   , -1
005023 :           R2 , VJM   , READSY3
005024 :           R4 , VJM   , WSY*               SCHREIBEN IN AUSGABE-RECORD
005025 :            M , V1M   , WHZYKL
005026 :            S , XTA   ,
005027 :            A , STX   , NSP3
005028 :            A , ATX   , ZHLR
005029 :           R3 , UJ    ,
005030 : C----------------------------------------------------------------------
005031 :  WEWD    :RI , VTM   ,
005032 :           A  , XTA   , SFAKT
005033 :  WEWD1   : S , ATX   ,
005034 :              , UJ    , WEWD*
005035 :  WIW     :   , XTA   ,
005036 :              , UJ    , WEWD1
005037 : C---------------------------------------------------------------------
005038 :  WNX     : A , WTC   , D
005039 :           NX , VTM   ,
005040 :  WNX1    :R3 , UTC   ,
005041 :           NX , VZM   ,
005042 :           NX , UTM   , -1
005043 :           C  , XTA   , SP
005044 :           R4 , VTM   , WNX1
005045 :              , UJ    , WSY*
005046 :  WOW     : I9, VTM   , -16
005047 :              , ITA   , I9
005048 :            A , ARX   , W
005049 :              , ATI   , W1
005050 :           C  , AAX   , BIT15
005051 :              , UZA   , WOW3
005052 :           W1 , UTM   , 1
005053 :           A  , XTA   , INOUT
005054 :  WOW1    :I9 , UTM   , 1
005055 :              , ASN   , 64 - 3
005056 :           W1 , VLM   , WOW1
005057 :           A  , ATX   , INOUT
005058 :           I9 , VLM   , WOW2
005059 :           R3 , UJ    ,
005060 :  WOW2    : A , XTA   , INOUT
005061 :              , ASN   , 64 - 3
005062 :           A  , ATX   , INOUT
005063 :              , YTA   ,
005064 :           C  , AOX   , A60
005065 :           R4 , VJM   , WSY*
005066 :  WOW4    :I9 , VLM   , WOW2
005067 :           R3 , UJ    ,
005068 :  WOW3    : W1, VZM   , WOW4
005069 :           W1 , UTM   , -1
005070 :           C  , XTA   , SP
005071 :           R4 , VTM   , WOW3
005072 :              , UJ    , WSY*
005073 : C----------------------------------------------------------------------
005074 :  WAW     : A ,  XTA  , W
005075 :           R3 ,  UZA  ,                    RUECKSPRUNG, WENN W=0
005076 :              ,  ATI  , W1
005077 :           W1 ,  MTJ  , W2
005078 :           W1 ,  UTM  , -6
005079 :              ,  ITA  , W1
005080 :            C ,  AAX  , BIT15
005081 :              ,  UZA  , WAW3               SPR,WENN W GROESSER GLEICH 6
005082 :  WAW1    :W2 ,  UTM  , -1
005083 :            A ,  XTA  , INOUT
005084 :              ,  ASN  , 64 - 8
005085 :            A ,  ATX  , INOUT
005086 :              ,  YTA  ,
005087 :           R4 ,  VJM  , WSY*
005088 :           W2 ,  V1M  , WAW1
005089 :           R3 ,  UJ   ,
005090 :  WAW2    : C ,  XTA  , SP                 AUSGABE VON W-6 FUEHRENDEN
005091 :           R4 ,  VJM  , WSY*               SPACES
005092 :            W1, UTM   , -1
005093 :  WAW3    :W1 ,  V1M  , WAW2
005094 :           W2 ,  VTM  , 6
005095 :              ,  UJ   , WAW1
005096 : C----------------------------------------------------------------------
005097 :  WLW     : A , XTA   , W
005098 :           R3 , UZA   ,
005099 :            C , AEX   , A77777
005100 :            C , ARX   , A2
005101 :              , ATI   , W1
005102 :           W1 , VLM   , WLW3
005103 :  WLW1    :TF , VTM   , 106 B              TF:= F
005104 :            A , XTA   , INOUT
005105 :              , UZA   , WLW2
005106 :           TF , VTM   , 124 B              TF:= T
005107 :  WLW2    :   , ITA   , TF
005108 :           R3 , MTJ   , R4
005109 :              , UJ    , WSY*               AUSG. VON T ODER F
005110 :  WLW3    : C , XTA   , SP
005111 :           R4 , VJM   , WSY*.              AUSG. VON W-1 FUEHRENDEN
005112 :           W1 , VLM   , * - 1              SPACES
005113 :              , UJ    , WLW1
005114 :   M5:4,VTM,-11
005115 :     14,VTM,-1
005116 :   ,XTA,
005117 :   ,UTC,DG
005118 :    ,ATX,
005119 :   1,XTA,
005120 : C  ПPИЗHAK ЦEЛOГO ЧИCЛA
005121 :    ,UTC,INT
005122 :   ,ATX,
005123 :   M6:,NTR,2
005124 :    8,XTA,111B
005125 : C   YMHOЖ. HA E-1
005126 :   1,A*X,210B
005127 :   ,NTR,3
005128 : C   BЫДEЛEHИE ЦEЛOЙ ЧACTИ
005129 :   1,A+X,46B
005130 :   ,UTC,R/R
005131 :    ,ATX,
005132 :   ,NTR,2
005133 : C  YMHOЖ. HA E1
005134 :   1,A*X,156B
005135 :   ,NTR,3
005136 :   8,X-A,111B
005137 :   ,UZA,M1
005138 : C  ИCПPABЛEHИE
005139 :   1,A+X,156B
005140 :   ,UTC,R/R
005141 :   ,XTS,
005142 :   1,A-X,154B
005143 :   ,UTC,R/R
005144 :    ,STX,
005145 : C  YПAKOBKA OЧEPEДHOЙ ЦИФPЫ
005146 :   M1:,ASN,64-44
005147 :   ,UTC,DG
005148 :   ,XTS,
005149 :   ,ASN,64+4
005150 :    15,AOX,
005151 :    ,UTC,DG
005152 :   ,ATX,
005153 :   14,UTM,1
005154 : C  ПEPEXOД K CЛEД. ЦИФPE
005155 :   ,UTC,R/R
005156 :   ,XTA,
005157 :   8,ATX,111B
005158 :   1,AEX,46B
005159 :   ,UZA,M7
005160 :   4,VLM,M6
005161 : CПACEHИE MЛAДШEЙ ЦИФPЫ
005162 :   ,UTC,DG
005163 :   ,XTA,
005164 :   ,ASN,64-44
005165 :   ,UTC,LAST
005166 :   ,ATX,
005167 :   ,UJ,M6
005168 : C    K-BO ЦИФP -1
005169 :   M7:,ITA,14
005170 :   ,UJ,WEI2
005171 :   M2:,UTC,DG
005172 : CTAPШИE ЦИФPЫ
005173 :   ,XTA,
005174 :   15,ATX,-7
005175 :   ,UTC,LAST
005176 :   ,XTA,
005177 : C   13 -Я CЛEBA ЦИФPA
005178 :   ,UTC,DG
005179 :   ,ATX,
005180 :   ,UJ,M3
005181 :  STOP*:,SUBP,
005182 :  I/O*ERR:,ENTRY,
005183 :  PRINT82:,BSS,
005184 :  BCDPUN2:,BSS,
005185 :  PT8PUN:,BSS,
005186 :  PT5PUN:,BSS,
005187 :  A,XTA,LUN
005188 :  ,CALL,OCTTDEC
005189 :  14,VTM,TXT
005190 :  ,ASN,64-24
005191 :  ,ASN,64+24
005192 :  14,AOX,4
005193 :  14,ATX,4
005194 :  ,ITS,14
005195 :  14,UTM,5
005196 :  ,ITS,14
005197 :  ,XTS,
005198 :  13,VTM,STOP*
005199 :  ,UJ,PRINT8
005200 :  TXT:,ISO,31H ERROR IN  IN/OUTPUT  (LUN=000)
005201 :   INT:,LOG,
005202 :   DG:,LOG,
005203 :   R/R:,LOG,
005204 :   LAST:,LOG,
005205 :  ,END,
005206 :       BIGPUNCH    :,NAME,
005207 :       PUNCTABL    :,SUBP,
005208 :  GAK/2:,EQU,PUNCTABL      +   00001B
005209 :                   8     ,VTM,      *00013B
005210 :                         ,STI ,9
005211 :                   8     ,ATX ,
005212 :                   10    ,VTM, 00060B
005213 :      *00002B:     10    ,VZM,      *00012B
005214 :                   8     ,XTA ,
005215 :                   10    ,ASN ,58
005216 :                   8     ,AAX ,00001B
005217 :                         ,ASN ,63
005218 :                         ,ATI ,11
005219 :                   11    ,UTC, PUNCTABL
005220 :                         ,XTA ,
005221 :                   9     ,ATX ,
005222 :                   11    ,UTC,  GAK/2
005223 :                         ,XTA ,
005224 :                   9     ,ATX ,00001B
005225 :                   10    ,UTM, 77772B
005226 :                   9     ,UTM, 00002B
005227 :                         ,UJ,       *00002B
005228 :      *00012B:           ,STX ,
005229 :                   13    ,UJ,  00000B
005230 : CONST
005231 :      *00013B:,OCT,0000  0000  0000  0000
005232 :      *00014B:,OCT,0000  0000  0000  0077
005233 :                   ,END,
005234 :       BINARY      :,NAME,
005235 :       MONCARD*    :,LC, 00031B
005236 :       MONREAD*    :,SUBP,
005237 :  GAK/4:,EQU,MONCARD*      +   00030B
005238 :       PRINT8      :,SUBP,
005239 :      *00000B:     14    ,VTM,      *00000B
005240 :                         ,XTS ,MONCARD*
005241 :                   14    ,AEX ,00013B
005242 :                         ,U1A,      *00007B
005243 :                         ,STX ,
005244 :                         ,ATI ,12
005245 :                   14    ,VTM, 77751B
005246 :      *00004B:     14    ,XTA , GAK/4
005247 :                   14    ,UTC, 00027B
005248 :                   12    ,ATX ,
005249 :                   14    ,VLM,      *00004B
005250 :                         ,STX ,
005251 :                         ,UJ,  MONREAD*
005252 :      *00007B:     14    ,VTM,      *00014B
005253 :                         ,ITS ,14
005254 :                   14    ,UTM, 00002B
005255 :                         ,ITS ,14
005256 :                         ,XTS ,
005257 :                   13    ,VJM, PRINT8
005258 :                         , SJ ,
005259 : CONST
005260 :      *00013B:,OCT,0000  0000  0007  7751
005261 :      *00014B:,OCT,2344  7516  2044  4516
005262 :      *00015B:,OCT,2025  1131  1004  1501
005263 :      *00016B:,OCT,2444  2040  1002  0040
005264 :                   ,END,
005265 :  BINREAD*:   , NAME  ,                    FPACK   31.08.72
005266 :  A       :   , EQUAL ,  8
005267 :  S       :   , EQUAL , 15
005268 :  R4      :   , EQUAL , 14
005269 :  R3      :   , EQUAL , 13
005270 :  I5      :   , EQUAL ,  5
005271 :  *0      :   , EQUAL ,
005272 :  *0: , BLOCK , A1,     A13,    A17,    A1750,  A17700, A20077, A377
005273 :   ,CONT,A5,A60,A77777,BIT1/6,BIT1/7,BIT2/48  .06
005274 :      , CONT  , BIT1/40,BIT1/41,BIT10/11,       BIT10/14,       BIT1/48
005275 :      , CONT  , BIT12,  BIT10,  BIT12/14,       BIT12/15,       BIT14
005276 :      , CONT  , BIT15,  BIT16,  BIT17,  BIT13/48,       BIT16/41,
005277 :      , CONT  , BIT3/4,BIT37/48,        BIT41/48                      .03
005278 :      , CONT  , BIT43/48,       BIT48(0),       EXP64,  BIT7,         .03
005279 :      , CONT  , BIT8,   BIT8/15,BIT9/48,VGR0,   EX640(0),       VGR(6).10
005280 :   ,CONT,CT(64),REAL1(0),DEINS(2)     .66
005281 :   ,CONT,E1(0),D10(2),R1/2(0),D1/2(2),REAL5/4,R5/4H13
005282 :   ,CONT,EKSPO(19),
005283 :      , CONT  , SECHS,  Z1/10,  D1/10(2),       D10E40(2),            .06
005284 :      , CONT  , D10E*40(2),     EOR,                                  .03
005285 :      , CONT  , VMASKE1,VMASKE2,VW1,    VW2,    VH,     VP            .06
005286 :      , CONT  , SPACE,  EX645,  K3HREC, K3HADD,                       .04
005287 :      , CONT  , MAXEXNZ,FTXT(14),       GRZAHL, ISOSP,  NBIT45, BIT46 .19
005288 :      , CONT  , BIT45,  BIT2/47,NZL,    KRECORD,A4,     BIT46/48,     .06
005289 :  *0: , BLOCK , SYMB,   ENDE,  KUSP/USP,KNEXTLET,PW,  KWSY,     SFAKT,
005290 :      , CONT  , AUSG2,  D,      NSP,    CENC,   NSYMB                 .05
005291 :      , CONT  , II13,   ER/AB,  ADCONT, ADR/W,  ADEND,  FORMAT, LUN,
005292 :      , CONT  , I/O,    GLREG(7),       BEGREC, ADNBPROG,       (1)   .11
005293 :      , CONT  , RECORD(25),     GWF(5), EAD(5), FTT,    SWUEBERL, WF  .38
005294 :      , CONT  , W,      NIW,    KBASP,  STLUN,  AC,                   .05
005295 :      , CONT  , INOUT,  LUN*,   L/NLGLRE,       KOFSPEC,        TRWADR,
005296 :      , CONT  , FNR,    II15,   NSP3,   ZHLR,   FTBE/SONST,           .05
005297 :  NWO     :   , EQUAL ,  2
005298 :  NSG     :   , EQUAL ,  6
005299 :  C       :   , EQUAL ,  1
005300 :  I12       : , EQUAL ,  12
005301 :  IOCONT* :   , SUBP  ,
005302 :  IOEND*1 :   , EQU   , IOCONT* + 1
005303 :  SETLUN* :   , EQU   , IOCONT* + 4
005304 :  RGTRSPA*:   , EQU   , IOCONT* + 9
005305 :  RGTRI13*:   , EQU   , IOCONT* + 10
005306 :  RI15    :   , EQU   , IOCONT* + 11
005307 :  BINARY  :   , SUBP  ,
005308 :   FMRTAPE:,SUBP,
005309 :  FT*641  :   , ENTRY ,.             EROEFFNUNG BEI READ (LUN) LISTE /BIN
005310 :              , XTS   ,                    MAG:= LUN  (PARAMETER)
005311 :           I12, VTM   , IRNREAD            I12:= AD(12H BINREAD*)
005312 :  IR1     : S , ATX   ,                    MAG:= 0/1 DH   EINGABE/AUSGABE
005313 :           R4 , VJM   , RGTRI13*
005314 :              , ITA   , I12
005315 :            A , ATX   , ADNBPROG
005316 :           R3 , VJM   , IOEND*1            ABSCHLUSS DER VORHERIGEN E/A
005317 :              , UTC   , IRACONT
005318 :              , XTA   ,
005319 :            A , ATX   , ADCONT
005320 :              , ASN   , 64 + 24
005321 :            A , ATX   , ADEND
005322 :            C , XTA   , A1
005323 :            A , STX   , ER/AB              UEBERNAHME DER PARAMETER AUS
005324 :            A , ATX   , I/O                MAGAZIN
005325 :            S , WTC   ,
005326 :              , XTA   ,
005327 :            A , ATX   , LUN
005328 :              , XTA   ,
005329 :            A , ATX   , FORMAT      . FORMAT:=0 DH BINAER
005330 :           R3 , VJM   , RI15
005331 :            R3, VJM   , SETLUN*            SETZEN STLUN UND LUN*
005332 :            A , MTJ   , I5
005333 :            A , UTC   , RECORD
005334 :           NWO, VTM   ,
005335 :            A , XTA   , I/O
005336 :              , U1A   , IRAUSG             SPR, WENN AUSGABE
005337 :            A , XTA   , STLUN
005338 :              , U1A   , IRBINARY           SPR, WENN BINEING VON LKLESER
005339 :   R4,VTM,=:41
005340 :  IR2     : A , WTC   , LUN*
005341 :           R3 , VTM   ,
005342 :              , ITS   , R3
005343 :   R4,AOX,
005344 :   ,CALL,FMRTAPE
005345 :  IREND   :I5 , MTJ   , A
005346 :              , UJ    , RGTRSPA*
005347 :  '--------------------------------
005348 :  IRBINARY:   , ITS   , NWO                BINEINGABE VON STLUN LKLESER
005349 :           NSG, VTM   , -24
005350 :           R3 , VTM   , IREND
005351 :              , UJ    , BINARY
005352 :  '--------------------------------
005353 :  IRAUSG  : A , XTA   , STLUN
005354 :              , U1A   , IRASTL             SPR, WENN BINAUSG AUF STLUN
005355 :   R4,VTM,=:42
005356 :              , UJ    , IR2
005357 :  '--------------------------------
005358 :  IRASTL  :NSG, VTM   , -25
005359 :              , UJ    , RGTRSPA*
005360 :  '--------------------------------
005361 :  BINWRIT*:   , ENTRY ,
005362 :  FT*721  :   , ENTRY ,.             EROEFFNUNG BEI WRITE (LUN) LISTE/BIN
005363 :           I12, VTM   , IRNWRIT            I12:= AD(12H BINWRIT*)
005364 :           I12, XTS   ,                    MAG:= LUN (PARAMETER)
005365 :              , UJ    , IR1
005366 :  IRACONT :   , UTC   , BINEND*
005367 :              , UTC   , BINCONT*
005368 :  IRNREAD :   , ISO   , 12H BINREAD* ''''
005369 :  IRNWRIT :   , ISO   , 12H BINWRIT* ''''
005370 : C
005371 : C** FPACK-BINCONT*
005372 : C                  FORTSETZUNG DES UP IOCONT* BEI BINAERER EIN- AUSGABE
005373 : C
005374 : C
005375 :  I3      :   , EQUAL ,  3
005376 :  R2      :   , EQUAL , 12
005377 :  I4      :   , EQUAL ,  4
005378 :  FPF*AF  :   , EQU   , IOCONT* + 2
005379 :  RGTRSPR*:   , EQU   , IOCONT* + 8
005380 :  BINCONT*:   , BSS   ,
005381 :   R3,VTM,ICAZYK
005382 :   A,XTA,I/O
005383 :   ,U1A,AUSGABE
005384 :   R3,VTM,ICZYK
005385 :   AUSGABE:A,XTA,STLUN
005386 :   R3,U1A,
005387 :   A,XTS,ENDE
005388 :   C,AAX,A77777
005389 :   ,ASN,64-24
005390 :   ,ITS,I3
005391 :   15,AOX,
005392 :   ,UTC,=:001
005393 :   ,AOX,
005394 :   ,CALL,FMRTAPE
005395 :   A,XTA,I/O
005396 :   ,U1A,RGTRSPA*
005397 :  ICEND   : A , XTA   , AC
005398 :              , UZA   , RGTRSPA*           RUECKSPR MIT LADEN AC
005399 :            S ,XTA    ,
005400 :            A , XTA   , INOUT
005401 :              , UJ    , RGTRSPR*           RUECKSPR OHNE LADEN AC
005402 : C----------------------------------------------------------------------
005403 :  ICZYK   :NSG, VZM   , ICEERR
005404 :           NSG, UTM   , 1                  UMSPEICHER-ZYKLUS
005405 :           NWO, XTA   ,
005406 :           NWO, UTM   , 1
005407 :           I3 , ATX   ,
005408 :           I3 , UTM   , 1
005409 :            I4, VLM   , ICZYK
005410 :              , UJ    , ICEND
005411 : C----------------------------------------------------------------------
005412 :  ICEERR  :   , ITA   , I3
005413 :            A , ATX   , TRWADR
005414 :          : R2, 31    , FPF*AF.     BEI BIN EINGABE WERDEN MEHR ALS
005415 :              , UTC   , 40 230B    .25 BZW 24 (BINARY) WORTE AUS RECORD
005416 :              , UJ    , ICEND.      GELESEN
005417 : C----------------------------------------------------------------------
005418 :  ICAZYK  :NSG, VZM   , ICWBINST           SPR, WENN RECORD VOLL
005419 :           I3 , XTA   ,
005420 :           I3 , UTM   , 1                  UMSPEICHER-ZYKLUS
005421 :           NWO, ATX   ,
005422 :           NWO, UTM   , 1
005423 :           NSG, UTM   , 1
005424 :           I4 , VLM   , ICAZYK
005425 :              , UJ    , RGTRSPA*
005426 :  '--------------------------------
005427 :  ICWBINST: R3, VTM   , ICAZYK
005428 :              , UJ    , WBINST*            SCHREIBEN RECORD MIT *62
005429 : C
005430 : C** FPACK-BINEND*
005431 : C                  FORTSETZUNG DES UP IOEND* BEI ABSCHLUSS EINER
005432 : C                  EIN- AUSGABE VON BINAER-INFORMATION
005433 : C
005434 :  BINEND* :   , BSS   ,
005435 :              , ITA   , R3
005436 :             A, XTS   , I/O
005437 :              , UZA   , INPUT
005438 :             A, XTA   , STLUN
005439 :              , UZA   , SRWEND
005440 :            R3, VJM   , WBINST*
005441 :  IEND    :  A, XTA   , L/NLGLRE
005442 :            15, WTC   ,
005443 :              , U1A   ,
005444 :              , UJ    , RGTRSPA*
005445 :  INPUT   : A , XTA   , STLUN
005446 :              , U1A   , IEND        .FALLS STANDARD-EINHEIT
005447 :   SRW END:,UTC,=:004
005448 :   ,XTS,
005449 :   13,VTM,IEND
005450 :   ,UJ,FMRTAPE
005451 : C
005452 : C** FPACK-WBINST*
005453 : C                  SCHREIBEN VON BINAER-INFORMATION AUF STANDARDEINHEIT
005454 : C
005455 : C
005456 :  REC     :   , EQUAL ,  5
005457 :  I/O*ERR:,SUBP,
005458 :  WBINST* :   , BSS   ,
005459 :              , ITA   , R3                 RETTEN R3
005460 :            S , ATX   ,
005461 :            A , MTJ   , REC
005462 :           REC, UTM   , RECORD             REC:=RECORD
005463 :           NSG, UTM   , 25
005464 :           NSG, VZM   , WBEND              SPR, WENN RECORD NOCH LEER
005465 :           NWO, UTM   , -1
005466 :            A , WTC   , STLUN
005467 :              , UJ    , *
005468 :          :R4 , VTM   , BINPUN+3           DRUCKER 1
005469 :              , UJ    , WBLBSTA
005470 :          :R4 , VTM   , BINPUN+1           STANZER 1
005471 :              , UJ    , WBLBSTA
005472 :          :R4 , VTM   , PT8BNP             LB-STANZER 8-KANAL
005473 :              , UJ    , WBLBSTA
005474 :          :   , UJ    , WBTT               TELETYPE
005475 :          :R4 , VTM   , BINPUN+4           DRUCKER 2
005476 :              , UJ    , WBLBSTA
005477 :          :R4 , VTM   , BINPUN+2           STANZER 2
005478 :              , UJ    , WBLBSTA
005479 :          :R4 , VTM   , PT5BNP             LB-STANZER 5-KANAL
005480 :  WBLBSTA:,BSS,
005481 :  ,UJ,I/O*ERR
005482 :  BINPUN:,BSS,
005483 :  PT8BNP:,BSS,
005484 :  PT5BNP:,BSS,
005485 :  ,EXT,
005486 : C----------------------------------------------------------------------
005487 :  WBTT    :   , ITS   , REC                TELETYPE
005488 :              , ITS   , NWO
005489 :              , XTS   ,
005490 :           R3 , CALL  , TTPRINT
005491 :  WBEND   :REC, MTJ   , NWO
005492 :           NSG, VTM   , -25
005493 :            S , WTC   ,
005494 :              , UJ    ,
005495 :  ,END,
005496 :  BLEXPRIN:,NAME,.
005497 :  ,UTC,INF64.
005498 :  ,*64,.
005499 :  ,NTR,6
005500 :  13,UJ,.
005501 :  INF64:,Z00,T1.
005502 :        ,Z00,T1.
005503 :  :,001,.
005504 :  8,,.
005505 :  T1:,GOST,30HБЫЛИ OШИБKИ ПPИ BBOДE ИЛИ TPAH
005506 :  ,CONT,12HCЛЯЦИИ '133''133''133''231'
005507 :  ,END,BLEXPRIN.
005508 :  BOKHO:,NAME,. 10.04.73
005509 :  ,UTC,SUM
005510 :  ,ATX,
005511 :  ,UTC,SUM1
005512 :  ,ATX,
005513 :  ,RTE,177B
005514 :  ,UTC,REG
005515 :  ,ATX,
005516 :  ,YTA,
005517 :  ,UTC,MINOR
005518 :  ,ATX,
005519 :  ,UTC,=7777
005520 :  ,XTA,
005521 :  ,UTC,FLAG
005522 :  ,ATX,
005523 :  OK11:,BSS,
005524 :  ,ITA,13
005525 :  ,UTC,W
005526 :  ,ATX,
005527 :  13,VTM,-14
005528 :  H:13,ITA,15
005529 :  13,UTC,R+14
005530 :  ,ATX,
005531 :  13,VLM,H
005532 :  :8,BASE,*
005533 :  ,XTA,W
005534 :  ,ATX,R+12
005535 :  ,XTA,REG
005536 :  ,ASN,64+41
005537 :  ,ATX,REG1
005538 :  ,XTA,FLAG
005539 :  ,UZA,OK22
005540 :  9,VTM,-5
005541 :  OK1:,XTA,SUM
005542 :  ,ASN,64-8
005543 :  ,ATX,SUM
005544 :  ,YTA,
005545 :  9,ATX,M+5
005546 :  9,VLM,OK1
005547 :  ,*64,OKPRINT
005548 :  OK33:,BSS,
005549 :  15,VTM,-13
005550 :  H1:15,UTC,R+13
005551 :  ,XTA,
005552 :  15,ATI,14
005553 :  15,VLM,H1
005554 :  ,UTC,R+14
005555 :  ,XTA,
005556 :  ,ATI,15
005557 :  ,UTC,MINOR
005558 :  ,XTA,
005559 :  ,AEX,
005560 :  ,UTC,SUM1
005561 :  ,XTA,
005562 :  ,UTC,REG
005563 :  ,XTR,
005564 :  13,UJ,
005565 :  OKHO:,ENTRY,
005566 :  ,UTC,M+5
005567 :  ,ATX,
005568 :  ,UTC,SUM1
005569 :  ,ATX,
005570 :  ,RTE,177B
005571 :  ,UTC,REG
005572 :  ,ATX,
005573 :  ,YTA,
005574 :  ,UTC,MINOR
005575 :  ,ATX,
005576 :  ,XTA,
005577 :  ,UTC,FLAG
005578 :  ,ATX,
005579 :  ,UJ,OK11
005580 :  OK22:,BSS,
005581 :  ,*64,OKPRINT1
005582 :  ,UJ,OK33
005583 :  OKPRINT:,UTC,M
005584 :  ,UTC,R+14
005585 :  02,000,3
005586 :  ,004,5
005587 :  02,032,3
005588 :  ,000,
005589 :  02,040,5
005590 :  8,006,14
005591 :  OKPRINT1:,UTC,M+5
005592 :  ,UTC,R+14
005593 :  02,000,16
005594 :  ,000,
005595 :  02,032,3
005596 :  ,000,
005597 :  02,040,5
005598 :  8,006,14
005599 :  SUM1:,BSS,1
005600 :  M:,BSS,6
005601 :  REG1:,BSS,1
005602 :  R:,BSS,15
005603 :  W:,BSS,1
005604 :  SUM:,BSS,1
005605 :  REG:,BSS,1
005606 :  MINOR:,BSS,1
005607 :  FLAG:,BSS,1
005608 :  ,END,
005609 :       C*AD*C      :,NAME,
005610 :       ACC*        :,LC, 00001B
005611 :                   15    ,ATX ,
005612 :                         ,RTE ,00177B
005613 :                         ,UTC,      *00067B
005614 :                         ,ATX ,
005615 :                         ,NTR ,2
005616 :                   14    ,XTA ,00001B
005617 :                         ,A+X ,ACC*
005618 :                         ,STX ,ACC*
005619 :                   14    ,A+X ,
005620 :                         ,UTC, 00000B
005621 :      *00005B:           ,UTC,      *00067B
005622 :                         ,XTR ,
005623 :                         ,AVX ,
005624 :                   13    ,UJ,  00000B
005625 :  C*SU*C:,ENTRY,
005626 :                   15    ,ATX ,
005627 :                         ,RTE ,00177B
005628 :                         ,UTC,      *00067B
005629 :                         ,ATX ,
005630 :                         ,NTR ,2
005631 :                   14    ,XTA ,00001B
005632 :                         ,X-A ,ACC*
005633 :                         ,STX ,ACC*
005634 :                   14    ,A-X ,
005635 :                         ,UJ,       *00005B
005636 :  C*MU*C:,ENTRY,
005637 :                         ,UTC,      *00070B
005638 :                         ,ATX ,
005639 :                   15    ,ATX ,
005640 :                         ,RTE ,00177B
005641 :                         ,NTR ,2
005642 :                         ,UTC,      *00067B
005643 :                         ,STX ,
005644 :                   14    ,A*X ,
005645 :                         ,XTS ,ACC*
005646 :                   14    ,A*X ,00001B
005647 :                   15    ,X-A ,
005648 :                         ,UTC,      *00070B
005649 :                         ,XTS ,
005650 :                   14    ,A*X ,00001B
005651 :                   14    ,XTS ,
005652 :                         ,A*X ,ACC*
005653 :                   15    ,A+X ,
005654 :                         ,ATX ,ACC*
005655 :      *00025B:           ,UTC,      *00067B
005656 :                         ,XTR ,
005657 :                   15    ,XTA ,
005658 :                   13    ,UJ,  00000B
005659 :  C*DI*C:,ENTRY,
005660 :                         ,UTC,      *00070B
005661 :                         ,ATX ,
005662 :                         ,RTE ,00177B
005663 :                         ,NTR ,2
005664 :                         ,UTC,      *00067B
005665 :                         ,ATX ,
005666 :                   14    ,XTA ,
005667 :                   14    ,A*X ,
005668 :                   14    ,XTS ,00001B
005669 :                   14    ,A*X ,00001B
005670 :                   15    ,A+X ,
005671 :                   15    ,ATX ,
005672 :                         ,XTS ,ACC*
005673 :                         ,UTC,      *00071B
005674 :                         ,ATX ,
005675 :                   14    ,A*X ,
005676 :                         ,UTC,      *00070B
005677 :                         ,XTS ,
005678 :                   14    ,A*X ,00001B
005679 :                   15    ,X-A ,
005680 :                   15    ,A/X ,
005681 :                         ,ATX ,ACC*
005682 :                         ,UTC,      *00070B
005683 :                         ,XTA ,
005684 :                   14    ,A*X ,
005685 :                         ,UTC,      *00071B
005686 :                         ,XTS ,
005687 :                   14    ,A*X ,00001B
005688 :                   15    ,A+X ,
005689 :                   15    ,A/X ,
005690 :                   15    ,ATX ,
005691 :                         ,UJ,       *00025B
005692 :  C*ST*C:,ENTRY,
005693 :                   14    ,ATX ,
005694 :                         ,XTS ,ACC*
005695 :                   14    ,STX ,00001B
005696 :                   13    ,UJ,  00000B
005697 :   C*LO*C:,ENTRY,
005698 :                   14    ,XTA ,00001B
005699 :                         ,ATX ,ACC*
005700 :                   14    ,XTA ,
005701 :                   13    ,UJ,  00000B
005702 :  C*LN*C:,ENTRY,
005703 :                         ,RTE ,00177B
005704 :                         ,UTC,      *00067B
005705 :                         ,ATX ,
005706 :                         ,NTR ,2
005707 :                   14    ,XTA ,00001B
005708 :                         ,UTC,      *00072B
005709 :                         ,AVX ,
005710 :                         ,ATX ,ACC*
005711 :                   14    ,XTA ,
005712 :                         ,UTC,      *00072B
005713 :                         ,AVX ,
005714 :                         ,UJ,       *00005B
005715 :  C*COMP:,ENTRY,
005716 :                   15    ,ATX ,
005717 :                         ,RTE ,00177B
005718 :                         ,UTC,      *00067B
005719 :                         ,ATX ,
005720 :                         ,XTA ,ACC*
005721 :                         ,UTC,      *00072B
005722 :                         ,AVX ,
005723 :                         ,STX ,ACC*
005724 :                         ,UTC,      *00072B
005725 :                         ,AVX ,
005726 :                         ,UJ,       *00005B
005727 :      *00067B:,OCT,0000  0000  0000  0000
005728 :      *00070B:,OCT,0000  0000  0000  0000
005729 :      *00071B:,OCT,0000  0000  0000  0000
005730 :      *00072B:,OCT,6437  7777  7777  7777
005731 :                   ,END,
005732 :  C*AD*I  :  ,NAME,
005733 :  C*AD*C  :  ,SUBP,
005734 :  12,VTM,C*AD*C.
005735 :  ,UJ,ITOC.
005736 :  C*SU*I  :  ,ENTRY,
005737 :  C*SU*C  :  ,SUBP,
005738 :           12,VJM,ITOC
005739 :             ,UJ ,C*SU*C
005740 :  C*MU*I  :  ,ENTRY,
005741 :  C*MU*C  :  ,SUBP,
005742 :           12,VJM,ITOC
005743 :             ,UJ ,C*MU*C
005744 :  C*DI*I  :  ,ENTRY,
005745 :  C*DI*C  :  ,SUBP,
005746 :           12,VJM,ITOC
005747 :             ,UJ ,C*DI*C
005748 :  ITOC    :  ,XTS,
005749 :             ,RTE,177B
005750 :           14,XTS,
005751 :             ,NTR,2
005752 :             ,A+X,
005753 :           14,VTM,*C
005754 :           14,ATX,
005755 :           15,XTR,
005756 :             ,XTA,
005757 :           14,STX,1
005758 :           12,UJ ,
005759 :  *C      :  ,BSS,2
005760 :             ,END,
005761 :       C*AD*R      :,NAME,
005762 :       C*AD*C      :,SUBP,
005763 :       C*SU*C      :,SUBP,
005764 :       C*MU*C      :,SUBP,
005765 :       C*DI*C      :,SUBP,
005766 :                   12    ,VJM,      *00010B
005767 :                         ,UJ,  C*AD*C
005768 :                         ,UTC, 00000B
005769 :  C*SU*R:,ENTRY,
005770 :                   12    ,VJM,      *00010B
005771 :                         ,UJ,  C*SU*C
005772 :                         ,UTC, 00000B
005773 :   C*MU*R:,ENTRY,
005774 :                   12    ,VJM,      *00010B
005775 :                         ,UJ,  C*MU*C
005776 :                         ,UTC, 00000B
005777 :  C*DI*R:,ENTRY,
005778 :                   12    ,VJM,      *00010B
005779 :                         ,UJ,  C*DI*C
005780 :                         ,UTC, 00000B
005781 :      *00010B:     14    ,XTS ,
005782 :                   14    ,VTM,      *00013B
005783 :                   14    ,ATX ,
005784 :                         ,XTA ,
005785 :                   14    ,STX ,00001B
005786 :                   12    ,UJ,  00000B
005787 :  *00013B:,BSS,2
005788 :                   ,END,
005789 :       C*PO*I      :,NAME,
005790 :       C*LO*C      :,SUBP,
005791 :       C*DI*C      :,SUBP,
005792 :       C*ST*C      :,SUBP,
005793 :       C*MU*C      :,SUBP,
005794 :                         ,ATI ,12
005795 :                         ,ITA ,13
005796 :                   12    ,XTS ,
005797 :                   12    ,XTS ,00001B
005798 :                   12    ,VTM,      *00034B
005799 :                   12    ,STX ,00001B
005800 :                   12    ,ATX ,
005801 :                         ,RTE ,00177B
005802 :                   14    ,XTS ,
005803 :                   12    ,AAX ,00005B
005804 :                   12    ,ATX ,00002B
005805 :                         ,NTR ,3
005806 :                         ,X-A ,
005807 :                         ,NTR ,18
005808 :                         ,U1A,      *00013B
005809 :                   12    ,ATX ,00002B
005810 :                   14    ,VTM,      *00043B
005811 :                   13    ,VJM, C*LO*C
005812 :                   14    ,VTM,      *00034B
005813 :                   13    ,VJM, C*DI*C
005814 :                   14    ,VTM,      *00034B
005815 :                   13    ,VJM, C*ST*C
005816 :      *00013B:     14    ,VTM,      *00043B
005817 :                   13    ,VJM, C*LO*C
005818 :                   14    ,VTM,      *00037B
005819 :                   13    ,VJM, C*ST*C
005820 :      *00015B:     12    ,VTM,      *00034B
005821 :                   12    ,XTA ,00002B
005822 :                   12    ,AAX ,00006B
005823 :                         ,UZA,      *00022B
005824 :                   14    ,VTM,      *00037B
005825 :                   13    ,VJM, C*LO*C
005826 :                   14    ,VTM,      *00034B
005827 :                   13    ,VJM, C*MU*C
005828 :                   14    ,VTM,      *00037B
005829 :                   13    ,VJM, C*ST*C
005830 :      *00022B:     12    ,VTM,      *00034B
005831 :                   12    ,XTA ,00002B
005832 :                         ,ASN ,65
005833 :                   12    ,ATX ,00002B
005834 :                         ,UZA,      *00031B
005835 :                   14    ,VTM,      *00034B
005836 :                   13    ,VJM, C*LO*C
005837 :                   14    ,VTM,      *00034B
005838 :                   13    ,VJM, C*MU*C
005839 :                   14    ,VTM,      *00034B
005840 :                   13    ,VJM, C*ST*C
005841 :                         ,UJ,       *00015B
005842 :      *00031B:     15    ,XTR ,
005843 :                   14    ,VTM,      *00037B
005844 :                   13    ,VJM, C*LO*C
005845 :                   15    ,WTC, 00000B
005846 :                         ,UJ,  00000B
005847 : CONST
005848 :      *00034B:,OCT,0000  0000  0000  0000
005849 :      *00035B:,OCT,0000  0000  0000  0000
005850 :      *00036B:,OCT,0000  0000  0000  0000
005851 :      *00037B:,OCT,0000  0000  0000  0000
005852 :      *00040B:,OCT,0000  0000  0000  0000
005853 :      *00041B:,OCT,0037  7777  7777  7777
005854 :      *00042B:,OCT,0000  0000  0000  0001
005855 :      *00043B:,OCT,4050  0000  0000  0000
005856 :      *00044B:,OCT,0000  0000  0000  0000
005857 :                   ,END,
005858 :       C*ST*I      :,NAME,
005859 :       R*ST*I      :,SUBP,
005860 :       C*LO*C      :,SUBP,
005861 :       C*LN*C      :,SUBP,
005862 :       R*ST*C      :,SUBP,
005863 :                         ,UJ,  R*ST*I
005864 :                         ,UTC, 00000B
005865 :  C*LO*I:,ENTRY,
005866 :                   12    ,VJM,      *00012B
005867 :                         ,UJ,  C*LO*C
005868 :                         ,UTC, 00000B
005869 :  C*LN*I:,ENTRY,
005870 :                   12    ,VJM,      *00012B
005871 :                         ,UJ,  C*LN*C
005872 :                         ,UTC, 00000B
005873 :   I*ST*C:,ENTRY,
005874 :                         ,UTC,      *00020B
005875 :                         ,ATX ,
005876 :                         ,RTE ,00177B
005877 :                         ,XTS ,
005878 :                         ,NTR ,2
005879 :                         ,UTC,      *00020B
005880 :                         ,A+X ,
005881 :                   15    ,XTR ,
005882 :                         ,UJ,  R*ST*C
005883 :                         ,UTC, 00000B
005884 :      *00012B:           ,XTS ,
005885 :                         ,RTE ,00177B
005886 :                   14    ,XTS ,
005887 :                         ,NTR ,2
005888 :                         ,A+X ,
005889 :                   14    ,VTM,      *00020B
005890 :                   14    ,ATX ,
005891 :                   15    ,XTR ,
005892 :                         ,XTA ,
005893 :                   14    ,STX ,00001B
005894 :                   12    ,UJ,  00000B
005895 :   *00020B:,BSS,2
005896 :                   ,END,
005897 :  C*ST*R  :  ,NAME,
005898 :           14,ATX,
005899 :           13,UJ ,
005900 :  C*LO*R  :  ,ENTRY,
005901 :  ACC*    :  ,LC,1
005902 :             ,XTA,
005903 :             ,ATX,ACC*
005904 :           14,XTA,
005905 :           13,UJ ,
005906 :  C*LN*R  :  ,ENTRY,
005907 :  C*COMP  :  ,SUBP,
005908 :             ,XTA,
005909 :             ,ATX,ACC*
005910 :           14,XTA,
005911 :             ,UJ ,C*COMP
005912 :  R*ST*C  :  ,ENTRY,
005913 :           14,ATX,
005914 :             ,XTS,
005915 :             ,ATX,ACC*
005916 :           14,STX,1
005917 :           13,UJ ,
005918 :             ,END,
005919 :  CABS:,NAME,
005920 :  ACC*:,LC,1.YBEЛИЧEH ДИAПAЗOH
005921 :  *0:B,BLOCK,
005922 :  ,CONT,SQRT
005923 :  ,CONT,SIN
005924 :  ,CONT,COS
005925 :  ,CONT,ATAN
005926 :  ,CONT,(1)
005927 :  ,CONT,LOG
005928 :  ,CONT,EXP
005929 :  12,VTM,END*ZM.
005930 :  ,UJ,MODUL.
005931 :  END*ZM:,XTS,.
005932 :  ,STX,ACC*.
005933 :  END*:15,XTR,.
005934 :  13,UJ,.
005935 :  REALSQ:,XTA,ACC*.
005936 :  ,UJ,END*ZM.
005937 :  REAL*:11,XTA,.MHИMAЯ ЧACTЬ = 0
005938 :  11,AVX,.
005939 :  ,FUN,SQRT.
005940 :  ,ATX,ACC*.
005941 :  11,XTA,.
005942 :  ,NTR,18.
005943 :  ,UZA,REALSQ.KOPEHЬ ИЗ X>0
005944 :  ,XTA,.KOPEHЬ ИЗ X<0
005945 :  ,UJ,END*.PEЗYЛЬTAT - ЧИCTO-MHИMЫЙ
005946 :  CSQRT:,ENTRY,.YBEЛИЧEHA TOЧHOCTЬ
005947 :  ,ATI,11.
005948 :  ,RTE, 177B.
005949 :  ,NTR,6.
005950 :  11,XTS,1.OCOБЫЙ CЛYЧAЙ -
005951 :  ,UZA,REAL*.MHИMAЯ ЧACTЬ = 0.0
005952 :   ,STI,.OБЩИЙ
005953 :  12,VJM,MODUL*.CЛYЧAЙ
005954 :  ,FUN,SQRT.KOPEHЬ ИЗ MOДYЛЯ
005955 :  15,ATX,.
005956 :  ,ATX,ACC*.
005957 :  12,VJM,ARGUS.
005958 :  ,E+N,64-1.X/2
005959 :  15,ATX,.
005960 :  ,FUN,SIN.
005961 :  ,A*X,ACC*.
005962 :  ,STX,ACC*.MHИMAЯ ЧACTЬ
005963 :  ,FUN,COS.
005964 :  15,A*X, .PEAЛЬHAЯ ЧACTЬ
005965 :  ,UJ,END*.
005966 :  CEXP:,ENTRY,.
005967 :  ,ATI,11.
005968 :  ,RTE,177B.
005969 :  11,XTS,.
005970 :  ,NTR,6.
005971 :  ,FUN,EXP.
005972 :  ,ATX,ACC*.
005973 :  11,XTS,1.
005974 :  ,FUN,SIN.
005975 :  ,A*X,ACC*.
005976 :  ,ATX,ACC*.MHИMAЯ ЧACTЬ
005977 :  11,XTA,1.
005978 :  ,FUN,COS .
005979 :  15,A*X,.
005980 :  ,UJ,END*.PEAЛЬHAЯ ЧACTЬ
005981 :  CLOG:,ENTRY,.
005982 :  12,VJM,MODUL.
005983 :  ,FUN,LOG
005984 :  15,ATX,.PEAЛЬHAЯ ЧACTЬ
005985 :  12,VJM,ARGUS.
005986 :  ,STX,ACC*.MHИMAЯ ЧACTЬ
005987 :  ,UJ,END*.
005988 :  MODUL:,ATI,11.
005989 :  ,RTE,177B
005990 :  MODUL*:,NTR,6.
005991 :  10,VTM,.
005992 :  11,XTS,.
005993 :  11,AMX,1.
005994 :  ,UZA,R*G*I
005995 :  10,VTM,1.
005996 :  R*G*I:10,UTC,.
005997 :  11,XTA,.
005998 :  ,UZA,MOD*0.
005999 :  10,WTC,CADR.
006000 :  11,XTS,.
006001 :  15,A/X,.
006002 :  MOD*0:14,BASE,PI
006003 :  ,ATX,RES.
006004 :  ,AVX,RES.
006005 :  ,ATX,RES.
006006 :  ,A*X,RES.
006007 :  ,A+X,=R1.0
006008 :  ,FUN,SQRT.
006009 :  10,UTC,
006010 :  11,A*X,
006011 :  15,ATX,.
006012 :  15,AVX,.
006013 :  12,UJ,.
006014 :  ARGUS:,UTC,RES.
006015 :  ,XTA,.
006016 :  ,FUN,ATAN.
006017 :  14,VTM,PI.
006018 :  10,VZM,NNN
006019 :  ,X-A,PI/2
006020 :  NNN:11,XTS,.
006021 :  ,NTR,18.
006022 :  ,UIA,V/S.
006023 :  15,XTA,.
006024 :  11,AVX,1
006025 :  12,UJ,.
006026 :  V/S:,XTA,PI
006027 :  15,A-X,
006028 :  11,AVX,1.
006029 :  12,UJ,.
006030 :  CADR:,LOG,1
006031 :  ,LOG,.
006032 :  PI:,OCT,4114 4417 6652 1041
006033 :  PI/2:,OCT,4054 4417 6652 1041
006034 :  RES:,BSS,1.
006035 :  ,END,
006036 :  CHECKSUM:,NAME,.
006037 :  14,BASE,*.
006038 :  ,ATI,12.
006039 :  11,VTM,1-20.
006040 :  ,XTA,.
006041 :  ,ATX,KS.
006042 :  L1:12,XTA,.
006043 :  ,UZA,L3.
006044 :  L2:,ASN,64-12.
006045 :  ,ATX,WS.
006046 :  ,YTA,.
006047 :  ,AEX,KS.
006048 :  ,ATX,KS.
006049 :  ,XTA,WS.
006050 :  L3:,UIA,L2.
006051 :  12,UTM,1.
006052 :  11,VLM,L1.
006053 :  ,XTA,KS.
006054 :  ,AEX,C7777.
006055 :  13,UJ,.
006056 :  C7777:,LOG,7777.
006057 :  WS:,BSS,1.
006058 :  KS:,BSS,1.
006059 :  ,END,.
006060 :  CHKBEG*:,NAME, . 01.01.81
006061 : C*
006062 : C*  PACПOЗHABAHИЯ ЗAГOЛOBKOB ПOДПPOГPAMM,
006063 : C*  HAПИCAHHЫX HA FORTRAN"E И MADLEN"E.
006064 : C*
006065 : C*  OБPAЩEHИE:     CALL  CHKBEG* ( CARD )
006066 : C*
006067 : C*  ГДE CARD - TEKCTOBAЯ CTPOKA B KOДE ISO.
006068 : C*
006069 : C*     HA BЫXOДE: ECЛИ ЗAГOЛOBOK OПOЗHAH, TO
006070 : C*     COДEPЖИMOE 14-ГO ИHДEKC-PEГИCTPA
006071 : C*     HE HYЛEBOE, ПPИ ЭTOM:
006072 : C*
006073 : C*    - ИMЯ ПOДПPOГPAMMЫ B KOДE ISO C ИCKЛЮ-
006074 : C*      ЧEHИEM  ПPOБEЛOB  BHYTPИ  ИMEHИ
006075 : C*      ЗAHOCИTCЯ B  NAME*:,LC,2 ;
006076 : C*
006077 : C*    - COДEPЖИMOE MFOR*:,LC,1 HYЛEBOE, ECЛИ
006078 : C*      TEKCTOBAЯ CTPOKA ЯBЛЯETCЯ ЗAГOЛOBKOM
006079 : C*      ПOДПPOГPAMMЫ,HAПИCAHHOЙ HA ФOPTPAHE,
006080 : C*      И HE HYЛEBOE ДЛЯ ABTOKOДA MADLEN.
006081 : C*
006082 : C*   HEДOCTATKИ "CTAPOЙ" П/П  CHKBEG*:
006083 : C*
006084 : C*  - KOHCTPYKЦИИ TИПA
006085 : C*
006086 : C*                 PROGRAM: ,NAME,
006087 : C*                PROGRAMX: ,NAME,
006088 : C*
006089 : C*      ГДE X - ЛЮБOЙ ДOПYCTИMЫЙ B ДAHHOЙ
006090 : C*      KOHCTPYKЦИИ CИMBOЛ, PACПOЗHAETCЯ KAK
006091 : C*      ЗAГOЛOBOK ФOPTPAHHOЙ ПOДПPOГPAMMЫ;
006092 : C*
006093 : C*  - KOHCTPYKЦИЯ TИПA:   ИMЯ:,SUBP,
006094 : C*
006095 : C*     PACПOЗHAETCЯ KAK ЗAГOЛOBOK ABTOKOДHOЙ
006096 : C*     ПOДПPOГPAMMЫ C ПYCTЫM ИMEHEM;
006097 : C*
006098 : C*  - KOHCTPYKЦИЯ TИПA
006099 : C*
006100 : C*            DOUBLE PRECISION ИДEHTИФИKATOP
006101 : C*
006102 : C*     PACПOЗHAETCЯ KAK ЗAГOЛOBOK ФOPTPAHHOЙ
006103 : C*     ПOДПPOГPAMMЫ C HEOПPEДEЛEHHЫM ИMEHEM.
006104 : C*
006105 : C*
006106 : C*   OTЛИЧИE ДAHHOЙ CHKBEG* OT "CTAPOЙ":
006107 : C*
006108 : C*   - ИCПPABЛEHЫ ПEPEЧИCЛEHHЫE HEДOCTATKИ;
006109 : C*
006110 : C*   - TEKCTOBAЯ CTPOKA CARD MOЖET ЗAДABATЬ-
006111 : C*     CЯ B KOДE COSY.
006112 : C*
006113 : C*
006114 : C*
006115 : C*    ДЛЯ PAБOTЫ C CИMBOЛЬHOЙ ИHФOPMAЦИEЙ
006116 : C*    ИCПOЛЬЗYЮTCЯ ПOДПPOГPAMMЫ RASPAK8
006117 : C*    И   ZAPAK8 .
006118 : C*
006119 : C*       ИCПOЛЬЗYЮTCЯ БEЗ BOCCTAHOBЛEHИЯ
006120 : C*       8-14 ИHДEKC-PEГИCTPЫ.
006121 : C*
006122 :  NAME*:,LC,2
006123 :   MFOR*:LC,BLOCK,MFOR
006124 :  ENTR*:,LC,1
006125 :  TABSIMIS:,SUBP,
006126 : C
006127 : C
006128 :  C:,BSS,
006129 :  9,VTM,CHK ASS
006130 :  ,XTS,
006131 :  ,UTC,MFOR
006132 :  ,ATX,
006133 :  CP1:,ITA,7
006134 :  7,BASE,C
006135 :  ,ATX,I7
006136 :  ,ITA,13
006137 :  ,STX,I13
006138 :  ,ATI,8
006139 :  ,ATX,ACARD
006140 :  8,XTA,
006141 :  ,AEX,=1H
006142 :  ,ASN,64+40
006143 :  ,UZA,YES SP.IF PROBEL
006144 :  ,AAX,=200
006145 :  ,UZA,NEGRET.IF NE COSY
006146 :  YES SP:,ITA,8
006147 :  ,AOX,=I0
006148 :  ,NTR,2
006149 :  ,A*X,=R6.
006150 :  ,ATX,PCA
006151 :  14,VTM,IDEN
006152 :  ,ITA,14
006153 :  ,AOX,=I0
006154 :  ,A*X,=R6.
006155 :  ,ATX,PID0
006156 :  8,VTM,-72.FOR RASPAK
006157 :  9,UJ,
006158 : C---------------------------------------
006159 :  CHKASS:,BSS,
006160 : C------------------------------------------
006161 :  ,XTA,=6H'''''''''':
006162 :  ,ATX,CSYM
006163 :  9,VJM,RI
006164 :  ,U1A,CHK FORT.  HET  :
006165 :  ,XTA,IDEN.NAME
006166 :  ,ATX,ID
006167 :  ,XTA,IDEN+1
006168 :  ,ATX,ID+1
006169 :  ,XTA,=6H'''''''''',
006170 :  ,ATX,CSYM
006171 :  9,VJM,RI
006172 :  ,U1A,NEG RET.  HET  ,
006173 :  9,VJM,RI
006174 :  ,U1A,NEG RET.  HET  ,
006175 :  ,XTA,IDEN
006176 :  ,AEX,=6HNAME
006177 :  ,UZA,POSRET
006178 :  ,XTA,IDEN
006179 :  ,AEX,=6HSUBP
006180 :  ,U1A,NEG RET
006181 :  ,XTA,ID
006182 :  ,AEX,=1H
006183 :  ,U1A,NEG RET
006184 :  ,XTA,=6H''''''''''.
006185 :  ,ATX,CSYM
006186 :  9,VTM,POS RET1
006187 :  ,UJ,RI
006188 :  CHK FORT:10,V1M,NEGRET
006189 :  ,WTC,ACARD
006190 :  9,VTM,
006191 :  9,XTA,
006192 :  ,AEX,=1H
006193 :  ,UZA,CHK/FORT
006194 :  ,AAX,=:4
006195 :  ,UZA,NEG RET.IF NE COSY
006196 :  9,XTA,
006197 :  ,ARX,=:364.'172'
006198 :  ,UZA,NEG RET.LT '206'
006199 :  CHK/FORT:9,VTM,BSW
006200 :  12,VTM,ARTEXTS
006201 :  F1:12,XTA,
006202 :  ,UZA,NEGRET
006203 :  ,AEX,IDEN
006204 :  9,UZA,
006205 :  12,UTM,4
006206 :  9,VLM,F1
006207 :  BSW:,UJ,PROGRAM.PROGRAM
006208 :  :10,VTM,.SUBROUTINE
006209 :  ,UJ,F2
006210 :  :,UJ,FUNCTION.FUNCTION
006211 :  :10,VTM,-2.REAL FUNCTION
006212 :  ,UJ,F2
006213 :  :10,VTM,-5.INTEGER FUNCTION
006214 :  ,UJ,F2
006215 :  :10,VTM,-5.LOGICAL FUNCTION
006216 :  ,UJ,F2
006217 :  :10,VTM,-5.COMPLEX FUNCTION
006218 :  ,UJ,F2
006219 :  :10,VTM,-13.DOUBLE PRECISION
006220 :  ,UJ,F2.FUNCTION
006221 :  :,XTA,IDEN+1.BLOCK DATA
006222 :  12,AEX,1.3HATA
006223 :  ,UZA,IDIS
006224 :  ,UJ,NEG RET
006225 :  PROGRAM:,XTA,IDEN+1
006226 :  12,AEX,1.1HM
006227 :  ,ASN,64+40
006228 :  ,U1A,NEG RET
006229 :  ,XTA,IDEN+1
006230 :  ,ASN,64-8
006231 :  ,AOX,=40.PROBEL
006232 :  ,ATX,IDEN
006233 :  ,XTA,PID0
006234 :  ,A+X,=R2.
006235 :  ,ATX,PID
006236 :  ,XTA,=H
006237 :  ,UJ,PROG/
006238 :  FUNCTION:,XTA,IDEN+1
006239 :  12,AEX,1.2HON
006240 :  ,ASN,64+32
006241 :  ,U1A,NEG RET
006242 :  ,XTA,IDEN+1
006243 :  ,ASN,64-16
006244 :  ,AOX,=4H''''''''
006245 :  ,ATX,IDEN
006246 :  ,XTA,PID0
006247 :  ,A+X,=R1.
006248 :  ,ATX,PID
006249 :  ,XTA,=H
006250 :  ,UJ,PROG/
006251 :  F2:,ATX,CSYM
006252 :  ,ITA,12
006253 :  ,ITS,
006254 :  9,VJM,RI1
006255 :  ,STI,
006256 :  ,ATI,12
006257 :  10,V1M,NEG RET. HEДOБOP
006258 :  14,VTM,1-3
006259 :  F3:12,UTC,1
006260 :  14,XTA,2
006261 :  14,AEX,IDEN+3
006262 :  ,U1A,NEG RET
006263 :  14,VLM,F3
006264 :  IDIS:,XTA,PID0
006265 :  ,ATX,PID
006266 :  ,XTA,=H
006267 :  ,ATX,IDEN
006268 :  PROG/:,ATX,IDEN+1
006269 :  ,XTA,=6H''''''''''(
006270 :  ,ATX,CSYM
006271 :  ,UTC,MFOR
006272 :  ,ATX,.FLAG OF *FORTRAN
006273 :  10,VTM,-5.6 SYMBOLS OF *FORT
006274 :  9,VJM,RI1
006275 :  POSRET1:,XTA,IDEN
006276 :  ,ATX,ID
006277 :  ,XTA,IDEN+1
006278 :  ,ATX,ID+1
006279 :  POSRET:14,VTM,NAME*
006280 :  ,XTA,ID
006281 :  14,ATX,
006282 :  ,XTA,ID+1
006283 :  14,ATX,1
006284 :  RET:,XTA,I7
006285 :  ,STI,7
006286 :  ,WTC,I13
006287 :  ,UJ,
006288 :  NEGRET:14,VTM,
006289 :  ,UJ,RET
006290 : C*******************************************
006291 :  RI:10,VTM,-8
006292 :  RI2:,XTA,PID0
006293 :  ,ATX,PID
006294 :  ,XTA,=H
006295 :  ,ATX,IDEN
006296 :  ,ATX,IDEN+1
006297 :  ,ATX,IDEN+2
006298 :  ,ATX,IDEN+3
006299 :  RI1:8,VZM,EL2
006300 :  ,XTA,PCA
006301 :  ,CALL,RASPAK8.FROM ACARD
006302 :  ,XTS,PCA
006303 :  ,A+X,=R1.
006304 :  ,STX,PCA
006305 :  ,ATX,SYMB
006306 :  ,AEX,CSYM
006307 :  ,UZA,RLR
006308 :  ,XTA,SYMB
006309 :  ,AEX,=40
006310 :  ,UZA,EL1
006311 :  ,AAX,=200
006312 :  ,U1A,EL/
006313 :  ,WTC,SYMB
006314 :  ,UTC,TABSIMIS
006315 :  ,XTA,
006316 :  ,ASN,64+45
006317 :  ,AEX,=1
006318 :  ,UZA,EL2
006319 :  ,XTS,SYMB
006320 :  ,XTS,PID
006321 :  ,CALL,ZAPAK8.TO IDEN
006322 :  ,XTA,PID
006323 :  ,A+X,=R1.
006324 :  ,ATX,PID
006325 :  10,VLM,EL1.LENGTH OF PARAM
006326 :  RLR:8,VLM,*+1.PAЗДEЛИTEЛЬ
006327 :  :,AOX,
006328 :  9,UJ,
006329 :  EL/:,WTC,SYMB
006330 :  8,UTM,-201 B
006331 :  ,ITA,8
006332 :  ,AAX,=77 000
006333 :  :,U1A,*+1
006334 :  8,VTM,
006335 :  EL1:8,VLM,RI1.LENGTH OF CARD
006336 :  EL2:,XTA,=1
006337 :  9,UJ,
006338 : C-------------------------------------------
006339 :  CHKEND*:,ENTRY,
006340 : C*------------------------------------------
006341 : C*  PACПOЗHABAHИE ПOCЛEДHEЙ KAPTЫ ПPOГPAMM,
006342 : C*  HAПИCAHHЫX HA FORTRAN"E И MADLEN"E.
006343 : C*
006344 : C*    OБPAЩEHИE: CALL CHKEND* ( CARD )
006345 : C*
006346 : C*    ГДE CARD - TEKCTOBAЯ CTPOKA B KOДE ISO
006347 : C*               ИЛИ COSY.
006348 : C*
006349 : C*  B ЗABИCИMOCTИ OT COДEPЖИMOГO MFOR*:,LC,1
006350 : C*   ПPOBEPЯETCЯ ЯBЛЯETCЯ ЛИ ЗAДAHHAЯ CTPOKA
006351 : C*   KOHCTPYKЦИEЙ     ,END,  ИЛИ  END   .
006352 : C*
006353 : C*   ECЛИ COBПAЛO, TO 14-Й ИHДEKC-PEГИCTP
006354 : C*   HEHYЛEBOЙ.
006355 : C*
006356 : C*:  HEДOCTATOK "CTAPOЙ" П/П CHKEND* -
006357 : C*
006358 : C*  ПPИ ЗAДAHИИ TEKCTOBOЙ CTPOKИ B KOДE COSY
006359 : C*  ПPOБEЛЫ BHYTPИ KOHCTPYKЦИЙ  END  И ,END,
006360 : C*  HEДOПYCTИMЫ. ИHAЧE KOHEЦ ПOДПPOГPAMMЫ HE
006361 : C*  PACПOЗHAETCЯ.
006362 : C*
006363 : C*  OTЛИЧИE  ДAHHOЙ  CHKEND* OT "CTAPOЙ" -
006364 : C*
006365 : C*  - ПPOБEЛЫ BHYTPИ END $ ,END, ДOПYCTИMЫ ;
006366 : C*
006367 : C*  - PACПOЗHAЮTCЯ BXOДЫ ENTRY. ПOЛOЖИTEЛЬ-
006368 : C*    HЫЙ OTBET - HEHYЛEBOE ЗHAЧEHИE
006369 : C*    ENTR*:,LC,1.
006370 : C*
006371 :  ,ITS,
006372 :  ,UTC,ENTR*
006373 :  ,ATX,
006374 :  9,VJM,CP1
006375 :  ,UTC,MFOR
006376 :  ,XTA,
006377 :  ,U1A,FOREND
006378 :  ,XTA,=6H'''''''''',
006379 :  ,ATX,CSYM
006380 :  9,VJM,RI
006381 :  ,UZA,ASS/END
006382 :  ,XTA,SYMB
006383 :  ,AEX,=72.:
006384 :  ,U1A,NEG RET
006385 :  9,VJM,RI
006386 :  ,U1A,NEGRET.NET ,
006387 :  ASS/END:9,VJM,RI
006388 :  ,U1A,NEGRET
006389 :  GEND:,XTA,IDEN
006390 :  ,AEX,=6HEND
006391 :  ,UZA,EQU
006392 :  ,XTA,IDEN
006393 :  ,AEX,=6HENTRY
006394 :  ,U1A,NEGRET
006395 :  ,XTA,=1
006396 :  ,UTC,ENTR*
006397 :  ,ATX,
006398 :  ,UJ,NEGRET
006399 :  EQU:14,VTM,1
006400 :  ,UJ,RET
006401 :  FOREND:10,VTM,-4.+ENTRY
006402 :  9,VTM,GEND
006403 :  ,UJ,RI2
006404 :  ARTEXTS:,ISO,24HPROGRAM
006405 :  ,ISO,24HSUBROUTINE
006406 :  ,ISO,24HFUNCTION
006407 :  ,ISO,24HREALFUNCTION
006408 :  ,ISO,24HINTEGERFUNCTION
006409 :  ,ISO,24HLOGICALFUNCTION
006410 :  ,ISO,24HCOMPLEXFUNCTION
006411 :  ,ISO,24HDOUBLEPRECISIONFUNCTION
006412 :  ,ISO,24HBLOCKDATA
006413 :  ,OCT,
006414 :  I7:,BSS,1
006415 :  I13:,BSS,1
006416 :  PCA:,BSS,1
006417 :  IDEN:,BSS,4
006418 :  ID:,BSS,2
006419 :  PID0:,BSS,1
006420 :  PID:,BSS,1
006421 :  CSYM:,BSS,1
006422 :  SYMB:,BSS,1
006423 :  ACARD:,BSS,1
006424 :  ,END,
006425 :  CMPLX:,NAME,
006426 : C OБ'EДИHEHИE PEAЛЬHOЙ И MHИMOЙ ЧACTEЙ B KOMПЛ. 4ИCЛO
006427 :  ACC*:,LC,1
006428 :  ,ATI,14
006429 :  14,XTA,
006430 :  ,STX,ACC*
006431 :  ,ATI,14
006432 :  14,XTA,
006433 :  13,UJ,
006434 :  ,END,
006435 :  COLPUNC*:  ,NAME, . 10.04.73                        .
006436 :  CARD2   :  ,LC,21                        .
006437 :  BUFFER:,PC,1010.
006438 :  C       :  ,BSS,                         .
006439 :  ,UTC,MY5
006440 :  ,ATX,
006441 :             ,ITA,7                        .
006442 :            7,VTM,C                        .
006443 :            7,ATX,=I7-C                    .
006444 :             ,ITA,13                       .
006445 :            7,ATX,=I13-C                   .
006446 :  7,XTA,INFCDC-C
006447 :  7,XTS,MY5-C
006448 :             ,CALL,ISOICTTR                .
006449 :            7,XTS,INFREV-C                 .
006450 :            7,XTS,INFCDC-C                 .
006451 :             ,CALL,REVCARD                 .
006452 :  NEXTCARD:,BSS,
006453 :  7,XTS,INFREV-C
006454 :            7,ARX,=30-C                    .
006455 :  7,STX,INFREV-C
006456 :  ,WTC,MYMY
006457 :  14,VTM,
006458 :  14,VZM,PUNCH
006459 :  14,UTM,1
006460 :  ,ITS,14
006461 :  7,STX,MYMY-C
006462 :  ,UJ,NELOOP
006463 :  PUNCH   : 8,VTM,BUFFER                   .
006464 :             ,WTC,INFREV                   .
006465 :            9,VTM,-1                       .
006466 :  ,XTS,
006467 :            9,ATX,1
006468 :  9,STX,2
006469 :             ,UTC,=4010 0000 4410 0000     .
006470 :             ,*71,                         .
006471 :  ,WTC,MYMY
006472 :  14,VTM,
006473 :  14,VIM,RETURN
006474 :             ,WTC,=I7                      .
006475 :            7,VTM,                         .
006476 :             ,WTC,=I13                     .
006477 :           13,VTM,                         .
006478 :  COLPUNB*:  ,ENTRY,                       .
006479 :  RETURN  :  ,BSS,                         .
006480 :             ,UTC,INFREV0                  .
006481 :  ,XTS,
006482 :             ,UTC,INFREV                   .
006483 :  ,STX,
006484 :  14,VTM,-41
006485 :  ,ITS,14
006486 :  ,UTC,MYMY
006487 :  ,STX,
006488 :           13, UJ,                         .
006489 :  COLPUNE*:  ,ENTRY,                       .
006490 :  ,WTC,MYMY
006491 :  14,VTM,
006492 :  14,UTM,41
006493 :  ,ITS,14
006494 :  ,UTC,MYMY
006495 :  ,STX,
006496 :           13,UTC,                         .
006497 :  14,VZM,
006498 :             , UJ,PUNCH                    .
006499 :  NELOOP  :  ,WTC,=I7                      .
006500 :            7,VTM,                         .
006501 :             ,WTC,=I13                     .
006502 :             , UJ,                         .
006503 :  COLPUN*B:,ENTRY,
006504 :  ,ITS,7
006505 :  ,UTC,=I7
006506 :  ,ATX,
006507 :  7,BASE,C
006508 :  ,ITA,13
006509 :  ,STX,=I13
006510 :  13,VTM,1-24
006511 :  ,STI,14
006512 :  *1:14,UTC,
006513 :  13,XTS,24-1
006514 :  ,WTC,INFREV
006515 :  13,STX,24-1
006516 :  13,VLM,*1
006517 :  ,UJ,NEXTCARD
006518 :  INFCDC  :  ,UTC,                         .
006519 :             ,UTC,CARD2                    .
006520 :  INFREV  :  ,UTC,                         .
006521 :             ,UTC,BUFFER                   .
006522 :  INFREV0 :  ,UTC,                         .
006523 :             ,UTC,BUFFER                   .
006524 :  MY5:,BSS,1
006525 :  MYMY:,BSS,1
006526 :             ,END,
006527 :       CONJG       :,NAME,
006528 :       ACC*        :,LC, 00001B
006529 :                         ,ATI ,14
006530 :                   14    ,XTA ,00001B
006531 :                         ,X-A ,
006532 :                         ,ATX ,ACC*
006533 :                   14    ,XTA ,
006534 :                   13    ,UJ,  00000B
006535 :                   ,END,
006536 :  COPYTADR:,NAME,.NEW TEXT,VOLKOV
006537 :  *MONCRD*:,LC,12
006538 :  LIST:,PP,2048
006539 :  PRINT 8:,SUBP,
006540 :  MONCARD/:,LC,14
006541 :  C:,BSS,
006542 :  ,ITA,7
006543 :  ,ITS,13
006544 :  ,ITS,6
006545 :  ,ITS,5
006546 :  ,ITS,
006547 :  7,BASE,C
006548 :  10,VTM,
006549 :  ,CALL,PARAMT*
006550 :  11,VZM,COPYCARD
006551 :  ,XTA,=31 0000.STANDARD1
006552 :  ,ATX,N1
006553 :  ,XTA,=32 0000.STANDARD2
006554 :  ,ATX,N2
006555 :  9,VTM,N
006556 :  10,VTM,5
006557 :  ,CALL,PARAMT*.N
006558 :  ,ATI,5
006559 :  ,ATI,6
006560 :  14,V1M,ERROR
006561 :  11,MTJ,14
006562 :  14,UTM,-053B.+
006563 :  14,VZM,C1
006564 :  11,UTM,-075B.=
006565 :  11,V1M,C*
006566 :  5,VTM,.NO RW
006567 :  ,UJ,C1
006568 :  C*:6,VTM,.NO AEX
006569 :  C1:9,VTM,N1
006570 :  10,VTM,4
006571 :  ,CALL,PARAMT*.N1
006572 :  14,V1M,ERROR
006573 :  11,VZM,IWORD.END OF CARD
006574 :  9,VTM,N2
006575 :  10,VTM,4
006576 :  ,CALL,PARAMT*
006577 :  14,V1M,ERROR
006578 :  11,VZM,IWORD.END OF CARD
006579 :  10,VTM,6
006580 :  ,CALL,PARAMT*.R,W,RW,WR
006581 :  11,VTM,*MONCRD*
006582 :  11,XTA,
006583 :  ,AEX,=6HR
006584 :  ,U1A,R1.NO R
006585 :  13,VJM,RFIZ
006586 :  R1:11,XTA,
006587 :  ,AEX,=6HW
006588 :  ,U1A,R2.NO W
006589 :  13,VJM,WFIZ
006590 :  R2:11,XTA,
006591 :  ,AEX,=6HRW
006592 :  ,U1A,R3.NO RW
006593 :  RWFIZ:13,VJM,RFIZ
006594 :  13,VTM,IWORD
006595 :  ,UJ,WFIZ
006596 :  R3:11,XTA,
006597 :  ,AEX,=6HWR
006598 :  ,UZA,RWFIZ.WR
006599 :  IWORD:,BSS,.INF WORDS
006600 :  11,VTM,N1
006601 :  13,VJM,LIS.N1 R1
006602 :  ,AEX,=:001
006603 :  11,ATX,
006604 :  ,ATX,INFR.INFR R1
006605 :  11,VTM,N2
006606 :  13,VJM,LIS.
006607 :  ,ATX,N2.N2 W1
006608 :  ,AEX,=:001
006609 :  ,ARX,=0000 0100 0000 0000
006610 :  ,ATX,INFW.INFW R2
006611 :  TO:,BSS,.RW
006612 :  5,VZM,COMP
006613 :  ,TAPE,N1
006614 :  ,TAPE,N2
006615 :  11,VTM,N1.+1
006616 :  13,VJM,CHAN
006617 :  11,VTM,N2.+1
006618 :  13,VJM,CHAN
006619 :  5,UTM,-1
006620 :  5,V1M,TO
006621 :  COMP:,BSS,.AEX
006622 :  6,VZM,RET
006623 :  ,TAPE,INFR
006624 :  ,TAPE,INFW
006625 :  11,VTM,LIST
006626 :  12,VTM,LIST+1024
006627 :  14,VTM,-1023
006628 :  CO:11,UTC,.CYCL FOR AEX
006629 :  14,XTA,1023
006630 :  12,UTC,
006631 :  14,AEX,1023
006632 :  ,U1A,NOAEX.NO AEX
006633 :  14,VLM,CO
006634 :  11,VTM,INFR.+1
006635 :  13,VJM,CHAN
006636 :  11,VTM,INFW.+1
006637 :  13,VJM,CHAN
006638 :  6,UTM,-1
006639 :  6,V1M,COMP
006640 :  RET:,BSS,
006641 :  ,STI,
006642 :  ,STI,5
006643 :  ,STI,6
006644 :  ,STI,13
006645 :  ,ATI,7
006646 :  13,UJ,
006647 :  LIS:14,VTM,LIST.N PAGE
006648 :  ,ITA,14
006649 :  ,ASN,64-20
006650 :  11,AOX,
006651 :  11,ATX,
006652 :  13,UJ,
006653 :  RFIZ:,XTA,N1.FIZ FLAG FOR R
006654 :  ,AOX,=4 0000 0000 0000
006655 :  ,ATX,N1
006656 :  13,UJ,
006657 :  WFIZ:,XTA,N2.FIZ FLAG FOR W
006658 :  ,AOX,=4 0000 0000 0000
006659 :  ,ATX,N2
006660 :  13,UJ,
006661 :  CHAN:11,XTA,.NZONE=NZONE+1
006662 :  ,ARX,=1
006663 :  11,ATX,
006664 :  ,ARX,=10 0000
006665 :  ,AAX,=40 0000
006666 :  13,U1A,.NO MB
006667 :  11,XTA,
006668 :  ,AAX,=40
006669 :  13,UZA,.NO END OF MB
006670 :  11,XTA,
006671 :  ,ARX,=7740.NMB=NMB+1
006672 :  11,ATX,
006673 :   ,ARX,=10 0000
006674 :   ,AAX,=40 0000
006675 :   13,UZA,
006676 :   COPYCARD:14,VTM,ERR
006677 :   ,ITS,14
006678 :   14,UTM,3
006679 :   ,ITS,14
006680 :   ,XTS,
006681 :   ,CALL,PRINT8
006682 :  ERROR:,SJ,
006683 :  NOAEX:,BSS,
006684 :  11,VTM,INFR
006685 :  13,VJM,XAX
006686 :  11,VTM,INFW
006687 :  13,VJM,XAX
006688 :  14,VTM,NAE
006689 :  ,ITS,14
006690 :  14,UTM,4
006691 :  ,ITS,14
006692 :  ,XTS,=1
006693 :  13,VTM,ERROR
006694 :  ,UJ,PRINT 8
006695 :  XAX:11,XTA,
006696 :  ,ASN,64-30
006697 :  ,AUX,=6H'7''7''7''7''7''7'
006698 :  ,AOX,=6H000000
006699 :  11,ATX,
006700 :  13,UJ,
006701 :  MOVETAPE:,ENTRY,
006702 :  ,ITA,7
006703 :  ,ITS,13
006704 :  ,ITS,6
006705 :  ,ITS,5
006706 :  ,ITS,
006707 :  7,BASE,C
006708 :  10,VTM,
006709 :  ,CALL,PARAMT*
006710 :  11,VZM,COPYCARD
006711 :  MV:9,VTM,INFR
006712 :  10,VTM,4
006713 :  ,CALL,PARAMT*
006714 :  14,V1M,ERROR
006715 :  ,XTA,INFR
006716 :  CONT:,ARX,=50 0000
006717 :  ,ASN,64+18
006718 :  ,UZA,COPYCARD
006719 :  ,XTA,INFR
006720 :  ,AOX,=:003
006721 :  ,ATX,INFR
006722 :  ,TAPE,INFR
006723 :  ,UJ,RET
006724 :       ERR:,ISO,24HOШИБKA B ИHФOPMAЦИИ
006725 : C===== ЗAФИKCИPOBAHO ПO INFW BKЛЮЧИTEЛЬHO:
006726 :  NAE:,ISO,12HHECPABHEHИE
006727 :  INFR:,BSS,1
006728 :  ,ISO,6H
006729 :  INFW:,OCT,
006730 : C=========================================
006731 :  N:,BSS,1
006732 :  N1:,BSS,1
006733 :  N2:,BSS,1
006734 :  ,END,
006735 :  COSYTAKE:   ,NAME,
006736 :   STAPUNCH:,SUBP,
006737 :   STAPUNCH:I,BLOCK,PERCOSY1,PERSTAN1
006738 :   ,CONT,PERCOSY2(0),PERSTAN2(0),PERSTANE
006739 :   ,CONT,PERCOSY3(0),PERSTAN3
006740 :  PK      :   ,EQU,4
006741 :              ,UTC,DAM
006742 :              ,STX,
006743 :              ,ITS,PK
006744 :              ,ITA,13                      .
006745 :              ,UTC,*SLON                   .
006746 :              ,ATX,                        .
006747 :              ,XTS,
006748 :            PK,VTM,-15
006749 :  *DAM    :   ,WTC,DAM
006750 :            PK,XTS,15
006751 :   13,VJM,PERCOSY2
006752 :            PK,VLM,*DAM
006753 :              ,STX,
006754 :              ,STI,PK
006755 :              ,WTC,*SLON                   .
006756 :              ,UJ,                         .
006757 :    'CONST'
006758 :   *SLON  :   ,OCT,                        .
006759 :   DAM    :   ,OCT,
006760 :             ,END,                         .
006761 :  COSYTISO:,NAME,.
006762 :   READ*ERR:,SUBP,
006763 :  *9:,BLOCK,B,WC,WP,BL,S,RW
006764 :  NL:,EQU,12B.
006765 :  C:,ATI,RW.
006766 :  ,ITA,S.
006767 :  ,ITS,BL.
006768 :  ,ITS,WP.
006769 :  ,ITS,WC.
006770 :  ,ITS,B.
006771 :  ,ITS,RW.
006772 :  B,BASE,C.
006773 :  ,ATX,ARES.
006774 :  ,XTS,.
006775 :  BL,VTM,0.
006776 :  WP,VTM,-5.
006777 :  WC,VTM,1-14.
006778 :  READ W:RW,XTA,.
006779 :  RW,UTM,1.
006780 :  READ S:,ASN,64-8.
006781 :  ,ATX,WORD.
006782 :  ,YTA,.
006783 :  ,ATI,S.
006784 :  S,VZM,READ W.
006785 :  ,ATX,XS.
006786 :  ,ASN,64+7.
006787 :  ,UZA,WRIT S.
006788 :  ,ASN,64-5.
006789 :  ,ATX,XS.
006790 :  S,MTJ,BL.
006791 :  BL,UTM,-200B.
006792 :  BL,VZM,NON TEXT.
006793 :  WP,UTM,5.
006794 :  WP,VZM,WR BLW.
006795 :  WP,UTM,-5.
006796 :  WRIT S:15,XTA,-1.
006797 :  ,ASN,64-8.
006798 :  ,AOX,XS.
006799 :  15,ATX,-1.
006800 :  BL,VZM,NON BLS.
006801 :  BL,UTM,-1.
006802 :  TO WRITS:BL,VZM,NEXT RS.
006803 :  WP,VLM,WRIT S.
006804 :  NEXT WW:15,UTM,1.
006805 :  WC,VLM,WR BLW.
006806 :  NONTEXT:,BSS,
006807 :   ,UJ,READ*ERR
006808 :  WR BLW:BL,UTM,-6.
006809 :  ,ITA,BL.
006810 :  ,ASN,64+7.
006811 :  ,UIA,NON BLW.
006812 :  ,XTA,=H
006813 :  15,ATX,-1.
006814 :  ,UJ,NEXT WW.
006815 :  NON BLW:BL,UTM,6.
006816 :  WP,VTM,-6.
006817 :  BL,VIM,TO WRITS.
006818 :  ,UJ,NEXT RS.
006819 :  NON BLS:S,UTM,-NL.
006820 :  S,VZM,NEW LINE.
006821 :  NEXT RS:,XTA,WORD.
006822 :  WP,VLM,READ S
006823 :  WP,VTM,-5.
006824 :  15,UTM,1.
006825 :  WC,VLM,READ S.
006826 :  ,UJ,NON TEXT.
006827 :  NEW LINE:WC,UTM,14.
006828 :  ,WTC,ARES.
006829 :  WC,UTC,-1.
006830 :  WP,VTM,.
006831 :  MOVE:15,XTA,.
006832 :  WP,ATX,.
006833 :  WP,UTM,-1.
006834 :  WC,UTM,-1.
006835 :  WC,VIM,MOVE.
006836 :  14,VTM,-4.
006837 :  RES I:15,XTA,.
006838 :  14,ATI,13.
006839 :  14,VLM,RES I.
006840 :  ,STX,.
006841 :  13,UJ,.
006842 :  ARES:,BSS,1.
006843 :  WORD:,BSS,1.
006844 :  XS  :,BSS,1.
006845 :  ,END,.
006846 :  CSIN:,NAME,.TPOИЦK, 1983
006847 :  A/MINUS:,LC,1.YBEЛИЧEHA CKOPOCTЬ
006848 :  ACC*:,LC,1.CЧETA И TOЧHOCTЬ
006849 :  *0:B,BLOCK,
006850 :  ,CONT,SQRT
006851 :  ,CONT,SIN
006852 :  ,CONT,COS
006853 :  ,CONT,(3)
006854 :  ,CONT,EXP
006855 :  12,BAS,*C.
006856 :  *C:10,VJM,BEGIN*
006857 :  ,XTA,SINX
006858 :  ,A*X,CHY
006859 :  ,XTS,COSX.
006860 :  ,A*X,SHY.
006861 :  ,STX,ACC*
006862 :  END*:15,XTR,.
006863 :  13,UJ,.
006864 :  CCOS:,ENTRY,.
006865 :  10,VJM,BEGIN*
006866 :  ,XTA,COSX
006867 :  ,A*X,CHY
006868 :  ,XTS,SINX
006869 :  ,A*X,SHY
006870 :  ,AVX,A/MINUS.
006871 :  ,STX,ACC*
006872 :  ,UJ,END*.
006873 :  BEGIN*:12,VTM,*C
006874 :  ,ATI,11.
006875 :  ,RTE,177B
006876 :  11,XTS,
006877 :  ,NTR,6.
006878 :  ,FUN,SIN.
006879 :  ,ATX,SINX
006880 :  11,XTA,.
006881 :  ,FUN,COS.
006882 :  ,ATX,COSX.
006883 :  11,XTA,1
006884 :  ,FUN,EXP.
006885 :  ,ATX,EXPY.
006886 :  ,XTS,=R1.0
006887 :  15,A/X,.
006888 :  ,ATX,/EXPY
006889 :  ,A+X,EXPY
006890 :  ,E+N,64-1.
006891 :  ,ATX,CHY
006892 :  ,A-X,/EXPY.
006893 :  ,ATX,SHY.
006894 :  10,UJ,.
006895 :  COSX:,BSS,1.
006896 :  SINX:,BSS,1.
006897 :  EXPY:,BSS,.
006898 :  CHY :,BSS,1.
006899 :  /EXPY:,BSS,
006900 :  SHY :,BSS,1.
006901 :  ,END,.
006902 :       D*AD*I      :,NAME,
006903 :       D*AD*D      :,SUBP,
006904 :       D*SU*D      :,SUBP,
006905 :       D*MU*D      :,SUBP,
006906 :       D*DI*D      :,SUBP,
006907 :                   12    ,VJM,      *00010B
006908 :                         ,UJ,  D*AD*D
006909 :  D*SU*I:,ENTRY,
006910 :                   12    ,VJM,      *00010B
006911 :                         ,UJ,  D*SU*D
006912 :  D*MU*I:,ENTRY,
006913 :                   12    ,VJM,      *00010B
006914 :                         ,UJ,  D*MU*D
006915 :                         ,UTC, 00000B
006916 :  D*DI*I:,ENTRY,
006917 :                   12    ,VJM,      *00010B
006918 :                         ,UJ,  D*DI*D
006919 :      *00010B:           ,XTS ,
006920 :                         ,RTE ,00177B
006921 :                   14    ,XTS ,
006922 :                         ,NTR ,2
006923 :                         ,A+X ,
006924 :                   14    ,VTM,      *00016B
006925 :                   14    ,ATX ,
006926 :                   15    ,XTR ,
006927 :                         ,XTA ,
006928 :                   14    ,STX ,00001B
006929 :                   12    ,UJ,  00000B
006930 :  *00016B:,BSS,2
006931 :                   ,END,
006932 :       D*AD*R      :,NAME,
006933 :       D*AD*D      :,SUBP,
006934 :       D*SU*D      :,SUBP,
006935 :       D*MU*D      :,SUBP,
006936 :       D*DI*D      :,SUBP,
006937 :                   12    ,VJM,      *00010B
006938 :                         ,UJ,  D*AD*D
006939 :  D*SU*R:,ENTRY,
006940 :                   12    ,VJM,      *00010B
006941 :                         ,UJ,  D*SU*D
006942 :  D*MU*R:,ENTRY,
006943 :                   12    ,VJM,      *00010B
006944 :                         ,UJ,  D*MU*D
006945 :  D*DI*R:,ENTRY,
006946 :                   12    ,VJM,      *00010B
006947 :                         ,UJ,  D*DI*D
006948 :      *00010B:     14    ,XTS ,
006949 :                   14    ,VTM,      *00013B
006950 :                   14    ,ATX ,
006951 :                         ,XTA ,
006952 :                   14    ,STX ,00001B
006953 :                   12    ,UJ,  00000B
006954 :  *00013B:,BSS,2
006955 :                   ,END,
006956 :       D*PO*D      :,NAME,
006957 :       DLOG        :,SUBP,
006958 :       D*MU*D      :,SUBP,
006959 :       D*ST*D      :,SUBP,
006960 :       DEXP        :,SUBP,
006961 :      *00000B:     12    ,VTM,      *00024B
006962 :                         ,ITS ,13
006963 :                   12    ,ATX ,
006964 :                         ,RTE ,00177B
006965 :                   12    ,ATX ,00002B
006966 :                         ,ITA ,14
006967 :                   12    ,STX ,00001B
006968 :                   13    ,VJM, DLOG
006969 :                         ,WTC,      *00025B
006970 :                   14    ,VTM, 00000B
006971 :                   13    ,VJM, D*MU*D
006972 :                   14    ,VTM,      *00022B
006973 :                   13    ,VJM, D*ST*D
006974 :                   14    ,VTM,      *00022B
006975 :                         ,ITA ,14
006976 :                   13    ,VJM, DEXP
006977 :                         ,UTC,      *00026B
006978 :                         ,XTR ,
006979 :                         ,WTC,      *00024B
006980 :                         ,UJ,  00000B
006981 :  D*PO*R:,ENTRY,
006982 :                   14    ,XTS ,
006983 :                   14    ,VTM,      *00027B
006984 :                   14    ,ATX ,
006985 :                         ,XTA ,
006986 :                   14    ,STX ,00001B
006987 :                         ,UJ,       *00000B
006988 :  R*PO*D:,ENTRY,
006989 :                         ,ATI ,12
006990 :                   12    ,XTA ,
006991 :                   12    ,VTM,      *00027B
006992 :                   12    ,ATX ,
006993 :                         ,XTA ,
006994 :                   12    ,ATX ,00001B
006995 :                         ,ITA ,12
006996 :                         ,UJ,       *00000B
006997 :  *00022B:,BSS,2
006998 :   *00024B:,BSS,1
006999 :  *00025B:,BSS,1
007000 :   *00026B:,BSS,1
007001 :    *00027B:,BSS,2
007002 :                   ,END,
007003 :       D*PO*I      :,NAME,
007004 :       D*LO*D      :,SUBP,
007005 :       D*DI*D      :,SUBP,
007006 :       D*ST*D      :,SUBP,
007007 :       D*MU*D      :,SUBP,
007008 :                         ,ATI ,12
007009 :                         ,ITA ,13
007010 :                   12    ,XTS ,
007011 :                   12    ,XTS ,00001B
007012 :                   12    ,VTM,      *00034B
007013 :                   12    ,STX ,00001B
007014 :                   12    ,ATX ,
007015 :                         ,RTE ,00177B
007016 :                   14    ,XTS ,
007017 :                   12    ,AAX ,00005B
007018 :                   12    ,ATX ,00002B
007019 :                         ,NTR ,3
007020 :                         ,X-A ,
007021 :                         ,NTR ,18
007022 :                         ,U1A,      *00013B
007023 :                   12    ,ATX ,00002B
007024 :                   14    ,VTM,      *00043B
007025 :                   13    ,VJM, D*LO*D
007026 :                   14    ,VTM,      *00034B
007027 :                   13    ,VJM, D*DI*D
007028 :                   14    ,VTM,      *00034B
007029 :                   13    ,VJM, D*ST*D
007030 :      *00013B:     14    ,VTM,      *00043B
007031 :                   13    ,VJM, D*LO*D
007032 :                   14    ,VTM,      *00037B
007033 :                   13    ,VJM, D*ST*D
007034 :      *00015B:     12    ,VTM,      *00034B
007035 :                   12    ,XTA ,00002B
007036 :                   12    ,AAX ,00006B
007037 :                         ,UZA,      *00022B
007038 :                   14    ,VTM,      *00037B
007039 :                   13    ,VJM, D*LO*D
007040 :                   14    ,VTM,      *00034B
007041 :                   13    ,VJM, D*MU*D
007042 :                   14    ,VTM,      *00037B
007043 :                   13    ,VJM, D*ST*D
007044 :      *00022B:     12    ,VTM,      *00034B
007045 :                   12    ,XTA ,00002B
007046 :                         ,ASN ,65
007047 :                   12    ,ATX ,00002B
007048 :                         ,UZA,      *00031B
007049 :                   14    ,VTM,      *00034B
007050 :                   13    ,VJM, D*LO*D
007051 :                   14    ,VTM,      *00034B
007052 :                   13    ,VJM, D*MU*D
007053 :                   14    ,VTM,      *00034B
007054 :                   13    ,VJM, D*ST*D
007055 :                         ,UJ,       *00015B
007056 :      *00031B:     15    ,XTR ,
007057 :                   14    ,VTM,      *00037B
007058 :                   13    ,VJM, D*LO*D
007059 :                   15    ,WTC, 00000B
007060 :                         ,UJ,  00000B
007061 : CONST
007062 :      *00034B:,OCT,0000  0000  0000  0000
007063 :      *00035B:,OCT,0000  0000  0000  0000
007064 :      *00036B:,OCT,0000  0000  0000  0000
007065 :      *00037B:,OCT,0000  0000  0000  0000
007066 :      *00040B:,OCT,0000  0000  0000  0000
007067 :      *00041B:,OCT,0037  7777  7777  7777
007068 :      *00042B:,OCT,0000  0000  0000  0001
007069 :      *00043B:,OCT,4050  0000  0000  0000
007070 :      *00044B:,OCT,0000  0000  0000  0000
007071 :                   ,END,
007072 :       D*ST*I      :,NAME,
007073 :       D*ST*R      :,SUBP,
007074 :       R*ST*I      :,SUBP,
007075 :       D*LO*D      :,SUBP,
007076 :       D*LN*D      :,SUBP,
007077 :       R*ST*D      :,SUBP,
007078 :                   13    ,MTJ ,11
007079 :                   14    ,MTJ ,12
007080 :                   13    ,VJM, D*ST*R
007081 :                   12    ,MTJ ,14
007082 :                   13    ,VJM, R*ST*I
007083 :                   11    ,UJ,  00000B
007084 :  D*LO*I:,ENTRY,
007085 :                   12    ,VJM,      *00015B
007086 :                         ,UJ,  D*LO*D
007087 :  D*LN*I:,ENTRY,
007088 :                   12    ,VJM,      *00015B
007089 :                         ,UJ,  D*LN*D
007090 :                         ,UTC, 00000B
007091 :  I*ST*D:,ENTRY,
007092 :                         ,UTC,      *00023B
007093 :                         ,ATX ,
007094 :                         ,RTE ,00177B
007095 :                         ,XTS ,
007096 :                         ,NTR ,2
007097 :                         ,UTC,      *00023B
007098 :                         ,A+X ,
007099 :                   15    ,XTR ,
007100 :                         ,UJ,  R*ST*D
007101 :      *00015B:           ,XTS ,
007102 :                         ,RTE ,00177B
007103 :                   14    ,XTS ,
007104 :                         ,NTR ,2
007105 :                         ,A+X ,
007106 :                   14    ,VTM,      *00023B
007107 :                   14    ,ATX ,
007108 :                   15    ,XTR ,
007109 :                         ,XTA ,
007110 :                   14    ,STX ,00001B
007111 :                   12    ,UJ,  00000B
007112 :   *00023B:,BSS,2
007113 :                   ,END,
007114 :       D*ST*R      :,NAME,
007115 :       ACC*        :,LC, 00001B
007116 :       PRINT8      :,SUBP,
007117 :       D*COMP      :,SUBP,
007118 :       R*TO*INT:,SUBP,
007119 :        BEGIN:           ,XTS ,ACC*
007120 :                         ,UTC,      *00023B
007121 :                         ,AAX ,
007122 :                         ,U1A,      *00004B
007123 :                   15    ,XTA ,
007124 :   ,UJ,RET
007125 :      *00004B:     15    ,XTA ,
007126 :                         ,ARX ,
007127 :                         ,UZA,      *00007B
007128 :                         ,XTA ,
007129 :        RET:       14    ,ATX ,
007130 :                   13    ,UJ,  00000B
007131 :      *00007B:     14    ,VTM,      *00024B
007132 :                         ,ITS ,14
007133 :                   14    ,UTM, 00005B
007134 :                         ,ITS ,14
007135 :                         ,XTS ,
007136 :                   13    ,MTJ ,12
007137 :                   13    ,VJM, PRINT8
007138 :                   12    ,MTJ ,13
007139 :                         ,SJ,
007140 :  D*TO*I:,ENTRY,
007141 :   13,MTJ,9
007142 :   13,VTM,TOINT
007143 :  D*TO*R:,ENTRY,
007144 :   14,VTM,
007145 :   ,UJ,BEGIN
007146 :   TO INT:9,MTJ,13
007147 :   ,UJ,R*TO*INT
007148 :  D*LO*R:,ENTRY,
007149 :                         ,XTA ,
007150 :                         ,ATX ,ACC*
007151 :                   14    ,XTA ,
007152 :                   13    ,UJ,  00000B
007153 :  D*LN*R:,ENTRY,
007154 :                         ,XTA ,
007155 :                         ,ATX ,ACC*
007156 :                   14    ,XTA ,
007157 :                         ,UJ,  D*COMP
007158 :  R*ST*D:,ENTRY,
007159 :                   14    ,ATX ,
007160 :                         ,XTS ,
007161 :                         ,ATX ,ACC*
007162 :                   14    ,STX ,00001B
007163 :                   13    ,UJ,  00000B
007164 :      *00023B:,OCT,7740  0000  0000  0000
007165 :      *00024B:,OCT,2104  7525  2044  6105
007166 :      *00025B:,OCT,1005  2117  1005  1511
007167 :      *00026B:,OCT,2344  3514  2122  0120
007168 :      *00027B:,OCT,2444  2503  2225  1511
007169 :      *00030B:,OCT,2364  7040  2365  3105
007170 :      *00031B:,OCT,2444  3114  2365  3440
007171 :                   ,END,
007172 :  DABS:,NAME,
007173 : C BЫЧИCЛEHИE AБC.BEЛИЧИHЫ ЧИCЛA C ДBOЙHOЙ TOЧHOCTЬЮ
007174 :  D*LO*D:,SUBP,
007175 :  D*LN*D:,SUBP,
007176 :  ,ATI,14
007177 :  14,XTA,
007178 :  ,UTC,ЗHAK
007179 :  ,AAX,
007180 :  ,UZA,D*LO*D
007181 :   ,UJ,D*LN*D
007182 :  ЗHAK:,OCT,0020 0000 00000 000
007183 :  ,END,
007184 :       DATAN      :,NAME,
007185 :       DABS       :,SUBP,
007186 :       DSIGN      :,SUBP,
007187 :       D*ST*D     :,SUBP,
007188 :       D*LN*D     :,SUBP,
007189 :       D*AD*D     :,SUBP,
007190 :       D*LO*D     :,SUBP,
007191 :       D*DI*D     :,SUBP,
007192 :       D*MU*D     :,SUBP,
007193 :       D*SU*D     :,SUBP,
007194 :             14 ,  VTM,   *00001
007195 :                  , UJ,   *00165
007196 :  *00001    :     ,WTC,   *00202
007197 :             14 ,  VTM,
007198 :                  ,ITA,  14
007199 :             13 ,  VJM,      DABS
007200 :             14 ,  VTM,   BSS+ 0000B
007201 :             13 ,  VJM,  D*ST*D
007202 :             14 ,  VTM,   BSS+ 0000B
007203 :             13 ,  VJM,  D*LN*D
007204 :             14 ,  VTM,   *00213
007205 :             13 ,  VJM,  D*AD*D
007206 :                  ,UZA,   *00007
007207 :                  , UJ,   *00010
007208 :  *00007    :     , UJ,   *00150
007209 :                  ,UTC,
007210 :  *00010    :14 ,  VTM,   *00213
007211 :             13 ,  VJM,  D*LO*D
007212 :             14 ,  VTM,   BSS+ 0000B
007213 :             13 ,  VJM,  D*DI*D
007214 :             14 ,  VTM,   BSS+ 0002B
007215 :             13 ,  VJM,  D*ST*D
007216 :             14 ,  VTM,   *00207
007217 :             13 ,  VJM,  D*LO*D
007218 :             14 ,  VTM,   BSS+ 0004B
007219 :             13 ,  VJM,  D*ST*D
007220 :             14 ,  VTM,   *00213
007221 :             13 ,  VJM,  D*LN*D
007222 :             14 ,  VTM,   BSS+ 0006B
007223 :             13 ,  VJM,  D*ST*D
007224 :  *00017    :14 ,  VTM,   BSS+ 0002B
007225 :             13 ,  VJM,  D*LN*D
007226 :             14 ,  VTM,   *00211
007227 :             13 ,  VJM,  D*AD*D
007228 :                  ,UZA,   *00022
007229 :                  , UJ,   *00023
007230 :  *00022    :     , UJ,   *00157
007231 :                  ,UTC,
007232 :  *00023    :14 ,  VTM,   *00203
007233 :             13 ,  VJM,  D*LO*D
007234 :             14 ,  VTM,   BSS+ 0010B
007235 :             13 ,  VJM,  D*ST*D
007236 :             14 ,  VTM,   *00215
007237 :             13 ,  VJM,  D*LO*D
007238 :             14 ,  VTM,   *00207
007239 :             13 ,  VJM,  D*MU*D
007240 :             14 ,  VTM,   *00217
007241 :             13 ,  VJM,  D*DI*D
007242 :             14 ,  VTM,   BSS+ 0012B
007243 :             13 ,  VJM,  D*ST*D
007244 :  *00031    :14 ,  VTM,   BSS+ 0002B
007245 :             13 ,  VJM,  D*LO*D
007246 :             14 ,  VTM,   BSS+ 0010B
007247 :             13 ,  VJM,  D*SU*D
007248 :             14 ,  VTM,   *00176
007249 :             13 ,  VJM,  D*ST*D
007250 :             14 ,  VTM,   BSS+ 0002B
007251 :             13 ,  VJM,  D*LO*D
007252 :             14 ,  VTM,   BSS+ 0010B
007253 :             13 ,  VJM,  D*MU*D
007254 :             14 ,  VTM,   *00213
007255 :             13 ,  VJM,  D*AD*D
007256 :             14 ,  VTM,   *00200
007257 :             13 ,  VJM,  D*ST*D
007258 :             14 ,  VTM,   *00176
007259 :             13 ,  VJM,  D*LO*D
007260 :             14 ,  VTM,   *00200
007261 :             13 ,  VJM,  D*DI*D
007262 :             14 ,  VTM,   BSS+ 0000B
007263 :             13 ,  VJM,  D*ST*D
007264 :             14 ,  VTM,   BSS+ 0000B
007265 :             13 ,  VJM,  D*LO*D
007266 :             14 ,  VTM,   BSS+ 0000B
007267 :             13 ,  VJM,  D*MU*D
007268 :             14 ,  VTM,   BSS+ 0014B
007269 :             13 ,  VJM,  D*ST*D
007270 :             14 ,  VTM,   BSS+ 0014B
007271 :             13 ,  VJM,  D*LO*D
007272 :             14 ,  VTM,   BSS+ 0014B
007273 :             13 ,  VJM,  D*MU*D
007274 :             14 ,  VTM,   BSS+ 0016B
007275 :             13 ,  VJM,  D*ST*D
007276 :             14 ,  VTM,   BSS+ 0016B
007277 :             13 ,  VJM,  D*LO*D
007278 :             14 ,  VTM,   BSS+ 0014B
007279 :             13 ,  VJM,  D*MU*D
007280 :             14 ,  VTM,   BSS+ 0020B
007281 :             13 ,  VJM,  D*ST*D
007282 :             14 ,  VTM,   BSS+ 0020B
007283 :             13 ,  VJM,  D*LO*D
007284 :             14 ,  VTM,   BSS+ 0014B
007285 :             13 ,  VJM,  D*MU*D
007286 :             14 ,  VTM,   BSS+ 0022B
007287 :             13 ,  VJM,  D*ST*D
007288 :             14 ,  VTM,   BSS+ 0022B
007289 :             13 ,  VJM,  D*LO*D
007290 :             14 ,  VTM,   BSS+ 0014B
007291 :             13 ,  VJM,  D*MU*D
007292 :             14 ,  VTM,   BSS+ 0024B
007293 :             13 ,  VJM,  D*ST*D
007294 :             14 ,  VTM,   *00221
007295 :             13 ,  VJM,  D*LO*D
007296 :             14 ,  VTM,   BSS+ 0014B
007297 :             13 ,  VJM,  D*MU*D
007298 :             14 ,  VTM,   *00223
007299 :             13 ,  VJM,  D*AD*D
007300 :             14 ,  VTM,   *00176
007301 :             13 ,  VJM,  D*ST*D
007302 :             14 ,  VTM,   *00225
007303 :             13 ,  VJM,  D*LO*D
007304 :             14 ,  VTM,   BSS+ 0016B
007305 :             13 ,  VJM,  D*MU*D
007306 :             14 ,  VTM,   *00176
007307 :             13 ,  VJM,  D*AD*D
007308 :             14 ,  VTM,   *00176
007309 :             13 ,  VJM,  D*ST*D
007310 :             14 ,  VTM,   *00227
007311 :             13 ,  VJM,  D*LO*D
007312 :             14 ,  VTM,   BSS+ 0020B
007313 :             13 ,  VJM,  D*MU*D
007314 :             14 ,  VTM,   *00176
007315 :             13 ,  VJM,  D*AD*D
007316 :             14 ,  VTM,   *00176
007317 :             13 ,  VJM,  D*ST*D
007318 :             14 ,  VTM,   *00231
007319 :             13 ,  VJM,  D*LO*D
007320 :             14 ,  VTM,   BSS+ 0022B
007321 :             13 ,  VJM,  D*MU*D
007322 :             14 ,  VTM,   *00176
007323 :             13 ,  VJM,  D*AD*D
007324 :             14 ,  VTM,   *00176
007325 :             13 ,  VJM,  D*ST*D
007326 :             14 ,  VTM,   *00233
007327 :             13 ,  VJM,  D*LO*D
007328 :             14 ,  VTM,   BSS+ 0024B
007329 :             13 ,  VJM,  D*MU*D
007330 :             14 ,  VTM,   *00176
007331 :             13 ,  VJM,  D*AD*D
007332 :             14 ,  VTM,   BSS+ 0000B
007333 :             13 ,  VJM,  D*MU*D
007334 :             14 ,  VTM,   *00176
007335 :             13 ,  VJM,  D*ST*D
007336 :             14 ,  VTM,   *00235
007337 :             13 ,  VJM,  D*LO*D
007338 :             14 ,  VTM,   BSS+ 0014B
007339 :             13 ,  VJM,  D*MU*D
007340 :             14 ,  VTM,   *00223
007341 :             13 ,  VJM,  D*AD*D
007342 :             14 ,  VTM,   *00200
007343 :             13 ,  VJM,  D*ST*D
007344 :             14 ,  VTM,   *00237
007345 :             13 ,  VJM,  D*LO*D
007346 :             14 ,  VTM,   BSS+ 0016B
007347 :             13 ,  VJM,  D*MU*D
007348 :             14 ,  VTM,   *00200
007349 :             13 ,  VJM,  D*AD*D
007350 :             14 ,  VTM,   *00200
007351 :             13 ,  VJM,  D*ST*D
007352 :             14 ,  VTM,   *00241
007353 :             13 ,  VJM,  D*LO*D
007354 :             14 ,  VTM,   BSS+ 0020B
007355 :             13 ,  VJM,  D*MU*D
007356 :             14 ,  VTM,   *00200
007357 :             13 ,  VJM,  D*AD*D
007358 :             14 ,  VTM,   *00200
007359 :             13 ,  VJM,  D*ST*D
007360 :             14 ,  VTM,   *00243
007361 :             13 ,  VJM,  D*LO*D
007362 :             14 ,  VTM,   BSS+ 0022B
007363 :             13 ,  VJM,  D*MU*D
007364 :             14 ,  VTM,   *00200
007365 :             13 ,  VJM,  D*AD*D
007366 :             14 ,  VTM,   *00200
007367 :             13 ,  VJM,  D*ST*D
007368 :             14 ,  VTM,   *00245
007369 :             13 ,  VJM,  D*LO*D
007370 :             14 ,  VTM,   BSS+ 0024B
007371 :             13 ,  VJM,  D*MU*D
007372 :             14 ,  VTM,   *00200
007373 :             13 ,  VJM,  D*AD*D
007374 :             14 ,  VTM,   *00200
007375 :             13 ,  VJM,  D*ST*D
007376 :             14 ,  VTM,   *00176
007377 :             13 ,  VJM,  D*LO*D
007378 :             14 ,  VTM,   *00200
007379 :             13 ,  VJM,  D*DI*D
007380 :             14 ,  VTM,   BSS+ 0026B
007381 :             13 ,  VJM,  D*ST*D
007382 :             14 ,  VTM,   BSS+ 0012B
007383 :             13 ,  VJM,  D*LO*D
007384 :             14 ,  VTM,   BSS+ 0026B
007385 :             13 ,  VJM,  D*AD*D
007386 :             14 ,  VTM,   BSS+ 0006B
007387 :             13 ,  VJM,  D*MU*D
007388 :             14 ,  VTM,   BSS+ 0004B
007389 :             13 ,  VJM,  D*AD*D
007390 :             14 ,  VTM,   *00176
007391 :             13 ,  VJM,  D*ST*D
007392 :             14 ,  VTM,   *00176
007393 :                  ,ITA,  14
007394 :                  ,WTC,   *00202
007395 :             14 ,  VTM,
007396 :                  ,ITS,  14
007397 :             13 ,  VJM,  DSIGN
007398 :             14 ,  VTM,   BSS+ 0030B
007399 :             13 ,  VJM,  D*ST*D
007400 :                  , UJ,   *00171
007401 :                  ,UTC,
007402 :  *00150    :14 ,  VTM,   BSS+ 0000B
007403 :             13 ,  VJM,  D*LO*D
007404 :             14 ,  VTM,   BSS+ 0002B
007405 :             13 ,  VJM,  D*ST*D
007406 :             14 ,  VTM,   *00247
007407 :             13 ,  VJM,  D*LO*D
007408 :             14 ,  VTM,   BSS+ 0004B
007409 :             13 ,  VJM,  D*ST*D
007410 :             14 ,  VTM,   *00213
007411 :             13 ,  VJM,  D*LO*D
007412 :             14 ,  VTM,   BSS+ 0006B
007413 :             13 ,  VJM,  D*ST*D
007414 :                  , UJ,   *00017
007415 :                  ,UTC,
007416 :  *00157    :14 ,  VTM,   *00205
007417 :             13 ,  VJM,  D*LO*D
007418 :             14 ,  VTM,   BSS+ 0010B
007419 :             13 ,  VJM,  D*ST*D
007420 :             14 ,  VTM,   *00207
007421 :             13 ,  VJM,  D*LO*D
007422 :             14 ,  VTM,   *00217
007423 :             13 ,  VJM,  D*DI*D
007424 :             14 ,  VTM,   BSS+ 0012B
007425 :             13 ,  VJM,  D*ST*D
007426 :                  , UJ,   *00031
007427 :                  ,UTC,
007428 :  *00165    : 7 ,  MTJ,  8
007429 :              7 ,  VTM,   *00175
007430 :                  ,NTR,  2
007431 :              7 ,  STX,  5
007432 :                  ,ITS,  13
007433 :                  ,ITS,  8
007434 :                  ,ITS,
007435 :             14 ,   UJ,
007436 :  *00171    :     ,STI,
007437 :                  ,STI,  7
007438 :                  ,STI,  8
007439 :                  ,XTS,
007440 :             14 ,  VTM,   BSS+ 0030B
007441 :             13 ,  VJM,  D*LO*D
007442 :              8 ,   UJ,
007443 :                  ,UTC,
007444 :  *00175    :,     LOG,  00000000 00000000.
007445 :  *00176    :,     LOG,  00000000 00000000.
007446 :             ,     LOG,  00000000 00000000.
007447 :  *00200    :,     LOG,  00000000 00000000.
007448 :             ,     LOG,  00000000 00000000.
007449 :  *00202    :,     LOG,  00000000 00000000.
007450 :   *00203:,LOG,4012 5415 6025 2677
007451 :             ,     LOG,  00146202 75214222.
007452 :   *00205:,LOG,3714 5657 5360 1251
007453 :             ,     LOG,  00105300 17365626.
007454 :   *00207:,LOG,4054 4417 6652 1041
007455 :             ,     LOG,  00064302 15142270.
007456 :  *00211    :,REAL, 4.14213562373E-01
007457 :             ,     LOG,  00074622 04262750.
007458 :  *00213    :,REAL, 1.00000000000E+00
007459 :             ,REAL, 0.00000000000E+00
007460 :   *00215:,LOG,4114 0000 0000 0000
007461 :             ,REAL, 0.00000000000E+00
007462 :   *00217:,LOG,4210 0000 0000 0000
007463 :             ,REAL, 0.00000000000E+00
007464 :   *00221:,LOG,6156 5046 1713 1000
007465 :             ,REAL, 0.00000000000E+00
007466 :  *00223    :,REAL, 1.37493105750E+10
007467 :             ,REAL, 0.00000000000E+00
007468 :  *00225    :,REAL, 2.48613264900E+10
007469 :             ,REAL, 0.00000000000E+00
007470 :  *00227:,LOG,6056 5727 2032 0000
007471 :             ,REAL, 0.00000000000E+00
007472 :  *00231:,LOG,5714 7104 6211 6000
007473 :             ,REAL, 0.00000000000E+00
007474 :  *00233    :,REAL, 1.47456000000E+07
007475 :             ,REAL, 0.00000000000E+00
007476 :   *00235:,LOG,6210 3045 7201 5520
007477 :             ,REAL, 0.00000000000E+00
007478 :  *00237    :,REAL, 3.41148307500E+10
007479 :             ,REAL, 0.00000000000E+00
007480 :  *00241    :,REAL, 1.40472832500E+10
007481 :             ,REAL, 0.00000000000E+00
007482 :  *00243    :,REAL, 2.34121387500E+09
007483 :             ,REAL, 0.00000000000E+00
007484 :  *00245    :,REAL, 1.08056025000E+08
007485 :             ,REAL, 0.00000000000E+00
007486 :  *00247    :,     LOG,  00000000 00000000.
007487 :             ,     LOG,  00000000 00000000.
007488 :  BSS       :     ,BSS,  32B
007489 :       ,END,
007490 :       DATAN2      :,NAME,
007491 :       DATAN       :,SUBP,
007492 :       DSIGN       :,SUBP,
007493 :       FT*571      :,SUBP,
007494 :       FT*003      :,SUBP,
007495 :       D*LO*D      :,SUBP,
007496 :       D*SU*D      :,SUBP,
007497 :       D*DI*D      :,SUBP,
007498 :       D*ST*D      :,SUBP,
007499 :       D*COMP      :,SUBP,
007500 :       D*AD*D      :,SUBP,
007501 :       D*MU*D      :,SUBP,
007502 :                   14    ,VTM,      *00001B
007503 :                         ,UJ,       *00072B
007504 :      *00001B:           ,WTC,      *00106B
007505 :                   14    ,VTM, 00000B
007506 :                   13    ,VJM, D*LO*D
007507 :                   14    ,VTM,      *00116B
007508 :                   13    ,VJM, D*SU*D
007509 :                         ,ASN ,57
007510 :                         ,UZA,      *00006B
007511 :                         ,UJ,       *00007B
007512 :                         ,UTC, 00000B
007513 :      *00006B:           ,UJ,       *00053B
007514 :                         ,UTC, 00000B
007515 :      *00007B:           ,WTC,      *00107B
007516 :                   14    ,VTM, 00000B
007517 :                   13    ,VJM, D*LO*D
007518 :                   14    ,VTM,      *00116B
007519 :                   13    ,VJM, D*SU*D
007520 :                         ,ASN ,57
007521 :                         ,UZA,      *00014B
007522 :                         ,UJ,       *00015B
007523 :                         ,UTC, 00000B
007524 :      *00014B:           ,UJ,       *00043B
007525 :                         ,UTC, 00000B
007526 :      *00015B:           ,WTC,      *00106B
007527 :                   14    ,VTM, 00000B
007528 :                   13    ,VJM, D*LO*D
007529 :                         ,WTC,      *00107B
007530 :                   14    ,VTM, 00000B
007531 :                   13    ,VJM, D*DI*D
007532 :                   14    ,VTM,      *00104B
007533 :                   13    ,VJM, D*ST*D
007534 :                   14    ,VTM,      *00104B
007535 :                         ,ITA ,14
007536 :                   13    ,VJM, DATAN
007537 :                   14    ,VTM,      *00122B
007538 :                   13    ,VJM, D*ST*D
007539 :                   14    ,VTM,      *00122B
007540 :                   13    ,VJM, D*LO*D
007541 :                   14    ,VTM,      *00124B
007542 :                   13    ,VJM, D*ST*D
007543 :                         ,WTC,      *00107B
007544 :                   14    ,VTM, 00000B
007545 :                   13    ,VJM, D*LO*D
007546 :                         ,ASN ,57
007547 :                         ,UZA,      *00042B
007548 :                         ,ARX ,
007549 :                         ,UZA,      *00034B
007550 :                         ,UJ,       *00042B
007551 :                         ,UTC, 00000B
007552 :      *00034B:     14    ,VTM,      *00110B
007553 :                         ,ITA ,14
007554 :                   14    ,VTM,      *00122B
007555 :                         ,ITS ,14
007556 :                   13    ,VJM, DSIGN
007557 :                   14    ,VTM, 00000B
007558 :                   13    ,VJM, D*COMP
007559 :                   14    ,VTM,      *00122B
007560 :                   13    ,VJM, D*AD*D
007561 :                   14    ,VTM,      *00124B
007562 :                   13    ,VJM, D*ST*D
007563 :      *00042B:           ,UJ,       *00077B
007564 :                         ,UTC, 00000B
007565 :      *00043B:     14    ,VTM,      *00120B
007566 :                   13    ,VJM, D*LO*D
007567 :                   14    ,VTM,      *00110B
007568 :                   13    ,VJM, D*MU*D
007569 :                   14    ,VTM,      *00104B
007570 :                   13    ,VJM, D*ST*D
007571 :                   14    ,VTM,      *00104B
007572 :                         ,ITA ,14
007573 :                         ,WTC,      *00106B
007574 :                   14    ,VTM, 00000B
007575 :                         ,ITS ,14
007576 :                   13    ,VJM, DSIGN
007577 :                   14    ,VTM,      *00124B
007578 :                   13    ,VJM, D*ST*D
007579 :                         ,UJ,       *00077B
007580 :                         ,UTC, 00000B
007581 :      *00053B:           ,WTC,      *00107B
007582 :                   14    ,VTM, 00000B
007583 :                   13    ,VJM, D*LO*D
007584 :                         ,ASN ,57
007585 :                         ,UZA,      *00063B
007586 :                         ,ARX ,
007587 :                         ,UZA,      *00060B
007588 :                         ,UJ,       *00067B
007589 :                         ,UTC, 00000B
007590 :      *00060B:     14    ,VTM,      *00110B
007591 :                   13    ,VJM, D*LO*D
007592 :                   14    ,VTM,      *00124B
007593 :                   13    ,VJM, D*ST*D
007594 :                         ,UJ,       *00077B
007595 :                         ,UTC, 00000B
007596 :      *00063B:     14    ,VTM,      *00112B
007597 :                         ,ITS ,14
007598 :                   13    ,VJM, FT*571
007599 :                   13    ,VJM, FT*003
007600 :                         ,UJ,       *00077B
007601 :                         ,UTC, 00000B
007602 :      *00067B:     14    ,VTM,      *00116B
007603 :                   13    ,VJM, D*LO*D
007604 :                   14    ,VTM,      *00124B
007605 :                   13    ,VJM, D*ST*D
007606 :                         ,UJ,       *00077B
007607 :                         ,UTC, 00000B
007608 :      *00072B:     7     ,MTJ ,8
007609 :                   7     ,VTM,      *00103B
007610 :                         ,NTR ,2
007611 :                   7     ,STX ,00004B
007612 :                   7     ,STX ,00003B
007613 :                         ,ITS ,13
007614 :                         ,ITS ,8
007615 :                         ,ITS ,0
007616 :                   14    ,UJ,  00000B
007617 :                         ,UTC, 00000B
007618 :      *00077B:           ,STI ,0
007619 :                         ,STI ,7
007620 :                         ,STI ,8
007621 :                         ,XTS ,
007622 :                   14    ,VTM,      *00124B
007623 :                   13    ,VJM, D*LO*D
007624 :                   8     ,UJ,  00000B
007625 :                         ,UTC, 00000B
007626 : CONST
007627 :      *00103B:,OCT,0000  0000  0000  0000
007628 :      *00104B:,OCT,0000  0000  0000  0000
007629 :      *00105B:,OCT,0000  0000  0000  0000
007630 :      *00106B:,OCT,0000  0000  0000  0000
007631 :      *00107B:,OCT,0000  0000  0000  0000
007632 :      *00110B:,OCT,4114  4417  6652  1166
007633 :      *00111B:,OCT,0001  6030  5161  2766
007634 :      *00112B:,OCT,1203  0460  2602  6047
007635 :      *00113B:,OCT,2125  1122  2365  1040
007636 :      *00114B:,OCT,2345  2515  2044  2522
007637 :      *00115B:,OCT,1003  1465  1542  3451
007638 :      *00116B:,OCT,0000  0000  0000  0000
007639 :      *00117B:,OCT,0000  0000  0000  0000
007640 :      *00120B:,OCT,4010  0000  0000  0000
007641 :      *00121B:,OCT,0000  0000  0000  0000
007642 :  *00122B:,BSS,2
007643 :  *00124B:,BSS,2
007644 :                   ,END,
007645 :  DATAPR*:,NAME,
007646 :  DATA*:,LC,2
007647 :  DATAPR*:,BSS,.
007648 : C-------------------------------------------
007649 : C ПEЧATЬ ДATЫ
007650 : C ПO ФOPMATY  DD MON 7Y HH.MM
007651 : C-------------------------------------------
007652 :  ,ITS,13
007653 :  13,BASE,DARET
007654 :  DATE*:,ENTRY,
007655 :  DATE*:,BSS,
007656 :  ,ITS,.
007657 :  ,FUN,67B.DD.MM.YY.HH.MM
007658 :  ,NTR,6
007659 :  13,UJ,.
007660 :  DARET:,BSS,
007661 :  ,XTS,DATE0
007662 :  15,XTS,-2
007663 :  ,AUX,DMASK
007664 :  ,ASN,64+16.
007665 :  ,AOX,DATE1
007666 :  15,XTS,-3
007667 :  ,AAX,MONTHFI
007668 :  ,ASN,64+35
007669 :  15,ATX,
007670 :  ,YTA,
007671 :  14,VTM,24
007672 :  ,UZA,L1
007673 :  14,VTM,
007674 :  L1:15,WTC,
007675 :  ,XTA,MARR
007676 :  14,ASN,64
007677 :  ,ASN,64-24
007678 :  15,XTS,-4
007679 :  ,ASN,64-14.
007680 :  ,AUX,YMASK
007681 :  15,AOX,
007682 :  ,AOX,DATE2
007683 :  15,XTS,-4
007684 :  ,ASN,64-22.
007685 :  ,AUX,TMASK
007686 :  ,AOX,DATE3
007687 :  ,XTS,DATE4
007688 :  15,XTS,-8
007689 :  15,MTJ,13
007690 :  ,UZA,L2
007691 :  13,UTM,1
007692 :  L2:,UTC,L3
007693 :  ,PRINT,
007694 :  15,UTM,-6
007695 :  15,XTA,
007696 :  ,STI,13
007697 :  13,UJ,
007698 :  PREPDAT:,ENTRY,
007699 :  ,ITS,13.
007700 :  13,VJM,DATE*
007701 :  B:13,BAS,B
007702 :  15,ATX,
007703 :  ,ASN,64-14
007704 :  ,AUX,=6H'17''17'''''''''
007705 :  ,AOX,=6H00
007706 :  ,STX,DATA*+1
007707 :  ,AUX,=6H'3''17''''17''17'''
007708 :  ,AOX,=6H00/00/
007709 :  ,STX,DATA*
007710 :  ,STI,13
007711 :  ,UJ,DATAPR*
007712 :  L3:13,Z00,-5
007713 :  15,Z00,-1
007714 :  DATE0:,GOST,6H'201'  '200'7
007715 :  DATE1:,GOST,2H '200'
007716 : C ПOЗИЦИИ
007717 :  ,CONT,1H'055'
007718 :  ,CONT,3H00
007719 :  DATE2:,GOST,6H000 00.
007720 :  DATE4:,BSS,
007721 :  MARR:,GOST,6H'231'  ЯHB
007722 :  ,GOST,6HФEBMAP
007723 :  ,GOST,6HAПPMAЯ
007724 :  ,GOST,6HИЮHИЮЛ
007725 :  ,GOST,6HABГCEH
007726 :  DATE3:,GOST,6H 00.00
007727 :  DMASK:,BSS,.
007728 :  TMASK:,GOST,6H03 0
007729 :  YMASK:,GOST,6H0000  .
007730 :  ,GOST,6HOKTHOЯ
007731 :  ,GOST,6HДEK
007732 :  MONTHFI:,OCT,00 776
007733 :  ,END,DATAPR*
007734 :   DEBUGER:,NAME,
007735 :   *REGIM//:,LC,1
007736 :   RM*:,LC,5
007737 :   ,XTA,*REGIM//
007738 :   ,AOX,=:004
007739 :   ,ATX,*REGIM//
007740 :   ,XTA,RM*+3
007741 :   ,AOX,=:2
007742 :   ,ATX,RM*+3
007743 :   13,UJ,
007744 :   ,END,
007745 :       DEXP        :,NAME,
007746 :       D*LO*D      :,SUBP,
007747 :       D*ST*D      :,SUBP,
007748 :       D*LN*D      :,SUBP,
007749 :       D*AD*D      :,SUBP,
007750 :       D*MU*D      :,SUBP,
007751 :       D*ST*I      :,SUBP,
007752 :       I*ST*D      :,SUBP,
007753 :       D*SU*D      :,SUBP,
007754 :       D*DI*D      :,SUBP,
007755 :       D*PO*I      :,SUBP,
007756 :                   14    ,VTM,      *00001B
007757 :                         ,UJ,       *00057B
007758 :      *00001B:           ,WTC,      *00072B
007759 :                   14    ,VTM, 00000B
007760 :                   13    ,VJM, D*LO*D
007761 :                   14    ,VTM,      *00111B
007762 :                   13    ,VJM, D*ST*D
007763 :                   14    ,VTM,      *00111B
007764 :                   13    ,VJM, D*LN*D
007765 :                   14    ,VTM,      *00075B
007766 :                   13    ,VJM, D*AD*D
007767 :                         ,U1A,      *00007B
007768 :                         ,UJ,       *00010B
007769 :      *00007B:           ,UJ,       *00054B
007770 :                         ,UTC, 00000B
007771 :      *00010B:     14    ,VTM,      *00111B
007772 :                   13    ,VJM, D*LO*D
007773 :                   14    ,VTM,      *00075B
007774 :                   13    ,VJM, D*AD*D
007775 :                         ,U1A,      *00013B
007776 :                         ,UJ,       *00014B
007777 :      *00013B:           ,UJ,       *00051B
007778 :                         ,UTC, 00000B
007779 :      *00014B:     14    ,VTM,      *00111B
007780 :                   13    ,VJM, D*LO*D
007781 :                   14    ,VTM,      *00077B
007782 :                   13    ,VJM, D*MU*D
007783 :                   14    ,VTM,      *00113B
007784 :                   13    ,VJM, D*ST*D
007785 :                   14    ,VTM,      *00113B
007786 :                   13    ,VJM, D*LO*D
007787 :                   14    ,VTM,      *00070B
007788 :                   13    ,VJM, D*ST*I
007789 :                   7     ,XTA ,00001B
007790 :                   7     ,ATX ,00002B
007791 :                   14    ,VTM,      *00111B
007792 :                   13    ,VJM, I*ST*D
007793 :                   14    ,VTM,      *00113B
007794 :                   13    ,VJM, D*LO*D
007795 :                   14    ,VTM,      *00111B
007796 :                   13    ,VJM, D*SU*D
007797 :                   14    ,VTM,      *00077B
007798 :                   13    ,VJM, D*DI*D
007799 :                   14    ,VTM,      *00113B
007800 :                   13    ,VJM, D*ST*D
007801 :                   14    ,VTM,      *00101B
007802 :                   13    ,VJM, D*LO*D
007803 :                   14    ,VTM,      *00113B
007804 :                   13    ,VJM, D*AD*D
007805 :                   14    ,VTM,      *00115B
007806 :                   13    ,VJM, D*ST*D
007807 :                   14    ,VTM,      *00113B
007808 :                   13    ,VJM, D*LO*D
007809 :                   14    ,VTM,      *00113B
007810 :                   13    ,VJM, D*MU*D
007811 :                   14    ,VTM,      *00103B
007812 :                   13    ,VJM, D*DI*D
007813 :                   14    ,VTM,      *00111B
007814 :                   13    ,VJM, D*ST*D
007815 :                   14    ,VTM,      *00111B
007816 :                   13    ,VJM, D*LO*D
007817 :                   14    ,VTM,      *00113B
007818 :                   13    ,VJM, D*MU*D
007819 :                   14    ,VTM,      *00105B
007820 :                   13    ,VJM, D*DI*D
007821 :                   14    ,VTM,      *00111B
007822 :                   13    ,VJM, D*AD*D
007823 :                   14    ,VTM,      *00115B
007824 :                   13    ,VJM, D*AD*D
007825 :                   14    ,VTM,      *00115B
007826 :                   13    ,VJM, D*ST*D
007827 :                   14    ,VTM,      *00073B
007828 :                         ,ITA ,14
007829 :                   14    ,VTM,      *00071B
007830 :                   13    ,VJM, D*PO*I
007831 :                   14    ,VTM,      *00115B
007832 :                   13    ,VJM, D*MU*D
007833 :                   14    ,VTM,      *00115B
007834 :                   13    ,VJM, D*ST*D
007835 :                         ,UJ,       *00063B
007836 :                         ,UTC, 00000B
007837 :      *00051B:     14    ,VTM,      *00107B
007838 :                   13    ,VJM, D*LO*D
007839 :                   14    ,VTM,      *00115B
007840 :                   13    ,VJM, D*ST*D
007841 :                         ,UJ,       *00063B
007842 :                         ,UTC, 00000B
007843 :      *00054B:     14    ,VTM,      *00075B
007844 :                   13    ,VJM, D*LO*D
007845 :                   14    ,VTM,      *00111B
007846 :                   13    ,VJM, D*ST*D
007847 :                         ,UJ,       *00014B
007848 :                         ,UTC, 00000B
007849 :      *00057B:     7     ,MTJ ,8
007850 :                   7     ,VTM,      *00067B
007851 :                         ,NTR ,2
007852 :                   7     ,STX ,00003B
007853 :                         ,ITS ,13
007854 :                         ,ITS ,8
007855 :                         ,ITS ,0
007856 :                   14    ,UJ,  00000B
007857 :      *00063B:           ,STI ,0
007858 :                         ,STI ,7
007859 :                         ,STI ,8
007860 :                         ,XTS ,
007861 :                   14    ,VTM,      *00115B
007862 :                   13    ,VJM, D*LO*D
007863 :                   8     ,UJ,  00000B
007864 :                         ,UTC, 00000B
007865 : CONST
007866 :      *00067B:,OCT,0000  0000  0000  0000
007867 :      *00070B:,OCT,0000  0000  0000  0000
007868 :      *00071B:,OCT,0000  0000  0000  0000
007869 :      *00072B:,OCT,0000  0000  0000  0000
007870 :      *00073B:,OCT,4050  0000  0002  0000
007871 :      *00074B:,OCT,0000  0004  0000  0000
007872 :      *00075B:,OCT,4610  0000  0000  0000
007873 :      *00076B:,OCT,0000  0000  0000  0000
007874 :      *00077B:,OCT,5550  0000  0000  0000
007875 :      *00100B:,OCT,0000  0000  0000  0000
007876 :      *00101B:,OCT,4050  0000  0000  0000
007877 :      *00102B:,OCT,0000  0000  0000  0000
007878 :      *00103B:,OCT,4110  0000  0000  0000
007879 :      *00104B:,OCT,0000  0000  0000  0000
007880 :      *00105B:,OCT,4114  0000  0000  0000
007881 :      *00106B:,OCT,0000  0000  0000  0000
007882 :      *00107B:,OCT,0000  0000  0000  0000
007883 :      *00110B:,OCT,0000  0000  0000  0000
007884 :  *00111B:,BSS,2
007885 :  *00113B:,BSS,2
007886 :  *00115B:,BSS,2
007887 :                   ,END,
007888 :       DINT       :,NAME,
007889 :       DABS       :,SUBP,
007890 :       D*ST*D     :,SUBP,
007891 :       D*LO*D     :,SUBP,
007892 :       D*AD*D     :,SUBP,
007893 :       D*SU*D     :,SUBP,
007894 :       D*LN*D     :,SUBP,
007895 :             14 ,  VTM,   *00001
007896 :                  , UJ,   *00021
007897 :  *00001    :     ,WTC,   *00032
007898 :             14 ,  VTM,
007899 :                  ,ITA,  14
007900 :             13 ,  VJM,      DABS
007901 :             14 ,  VTM,   BSS+ 0000B
007902 :             13 ,  VJM,  D*ST*D
007903 :             14 ,  VTM,   *00033
007904 :             13 ,  VJM,  D*LO*D
007905 :             14 ,  VTM,   BSS+ 0000B
007906 :             13 ,  VJM,  D*AD*D
007907 :             14 ,  VTM,   *00033
007908 :             13 ,  VJM,  D*SU*D
007909 :             14 ,  VTM,   BSS+ 0000B
007910 :             13 ,  VJM,  D*ST*D
007911 :             14 ,  VTM,   BSS+ 0000B
007912 :             13 ,  VJM,  D*LO*D
007913 :             14 ,  VTM,   BSS+ 0002B
007914 :             13 ,  VJM,  D*ST*D
007915 :                  ,WTC,   *00032
007916 :             14 ,  VTM,
007917 :             13 ,  VJM,  D*LO*D
007918 :             14 ,  VTM,   *00035
007919 :             13 ,  VJM,  D*SU*D
007920 :                  ,UIA,   *00016
007921 :                  , UJ,   *00020
007922 :  *00016    :14 ,  VTM,   BSS+ 0000B
007923 :             13 ,  VJM,  D*LN*D
007924 :             14 ,  VTM,   BSS+ 0002B
007925 :             13 ,  VJM,  D*ST*D
007926 :  *00020    :     , UJ,   *00025
007927 :                  ,UTC,
007928 :  *00021    : 7 ,  MTJ,  8
007929 :              7 ,  VTM,   *00031
007930 :                  ,NTR,  2
007931 :              7 ,  STX,  1
007932 :                  ,ITS,  13
007933 :                  ,ITS,  8
007934 :                  ,ITS,
007935 :             14 ,   UJ,
007936 :  *00025    :     ,STI,
007937 :                  ,STI,  7
007938 :                  ,STI,  8
007939 :                  ,XTS,
007940 :             14 ,  VTM,   BSS+ 0002B
007941 :             13 ,  VJM,  D*LO*D
007942 :              8 ,   UJ,
007943 :                  ,UTC,
007944 :  *00031    :,     LOG,  00000000 00000000.
007945 :  *00032    :,     LOG,  00000000 00000000.
007946 :  *00033    :,     LOG,  50100000 00000000.
007947 :             ,     LOG,  00400000 00000000.
007948 :  *00035    :,     LOG,  00000000 00000000.
007949 :             ,     LOG,  00000000 00000000.
007950 :  BSS       :     ,BSS,  4
007951 :       ,END,
007952 :       DLOG       :,NAME,
007953 :       D*LO*D     :,SUBP,
007954 :       D*ST*D     :,SUBP,
007955 :       D*LN*D     :,SUBP,
007956 :       D*AD*D     :,SUBP,
007957 :       D*LO*R     :,SUBP,
007958 :       D*DI*D     :,SUBP,
007959 :       R*ST*D     :,SUBP,
007960 :       D*SU*D     :,SUBP,
007961 :       I*ST*D     :,SUBP,
007962 :       D*MU*D     :,SUBP,
007963 :             14 ,  VTM,   *00001
007964 :                  , UJ,   *00117
007965 :  *00001    :     ,WTC,   *00137
007966 :             14 ,  VTM,
007967 :             13 ,  VJM,  D*LO*D
007968 :             14 ,  VTM,   BSS+ 0052B
007969 :             13 ,  VJM,  D*ST*D
007970 :             14 ,  VTM,   BSS+ 0052B
007971 :             13 ,  VJM,  D*LN*D
007972 :             14 ,  VTM,   *00141
007973 :             13 ,  VJM,  D*AD*D
007974 :                  ,UZA,   *00007
007975 :                  , UJ,   *00010
007976 :  *00007    :     , UJ,   *00101
007977 :                  ,UTC,
007978 :  *00010    :14 ,  VTM,   BSS+ 0052B
007979 :             13 ,  VJM,  D*LN*D
007980 :             14 ,  VTM,   *00143
007981 :             13 ,  VJM,  D*AD*D
007982 :                  ,UZA,   *00013
007983 :                  , UJ,   *00017
007984 :  *00013    :14 ,  VTM,   *00143
007985 :             13 ,  VJM,  D*LO*R
007986 :                  ,WTC,   *00137
007987 :             14 ,  VTM,
007988 :             13 ,  VJM,  D*DI*D
007989 :             14 ,  VTM,   BSS+ 0052B
007990 :             13 ,  VJM,  D*ST*D
007991 :  *00017    : 7 ,  XTA,  8
007992 :              7 ,  ATX,  4
007993 :              7 ,  XTA,  12
007994 :              7 ,  ATX,  3
007995 :              7 ,  XTA,  13
007996 :              7 ,  ATX,  2
007997 :             13 ,  VJM,   *00113
007998 :  *00023    :14 ,  VTM,   BSS+ 0052B
007999 :             13 ,  VJM,  D*LN*D
008000 :                  ,WTC,   *00140
008001 :              1 ,  VTM,
008002 :              1 ,  UTC,   *00153
008003 :             14 ,  VTM,
008004 :             13 ,  VJM,  D*AD*D
008005 :                  ,UZA,   *00030
008006 :                  , UJ,   *00031
008007 :  *00030    :     , UJ,   *00040
008008 :                  ,UTC,
008009 :  *00031    : 7 ,  XTA,  4
008010 :              7 ,  A+X,  3
008011 :              7 ,  ATX,  4
008012 :             14 ,  VTM,   BSS+ 0052B
008013 :             13 ,  VJM,  D*LO*D
008014 :                  ,WTC,   *00140
008015 :              1 ,  VTM,
008016 :              1 ,  UTC,   *00153
008017 :             14 ,  VTM,
008018 :             13 ,  VJM,  D*DI*D
008019 :             14 ,  VTM,   BSS+ 0052B
008020 :             13 ,  VJM,  D*ST*D
008021 :  *00040    : 7 ,  XTA,  3
008022 :              7 ,  A/X,  14
008023 :              7 ,  ATX,  3
008024 :                  ,UTC,
008025 :              7 ,  XTA,  2
008026 :                  ,NTR,  3
008027 :              7 ,  A+X,  13
008028 :                  ,NTR,  22B
008029 :              7 ,  ATX,  2
008030 :             13 ,  VJM,   *00113
008031 :              7 ,  X-A,  15
008032 :                  ,UZA,   *00023
008033 :              7 ,  XTA,  4
008034 :             14 ,  VTM,   BSS+ 0060B
008035 :             13 ,  VJM,  R*ST*D
008036 :             14 ,  VTM,   BSS+ 0052B
008037 :             13 ,  VJM,  D*LO*D
008038 :             14 ,  VTM,   *00143
008039 :             13 ,  VJM,  D*SU*D
008040 :             14 ,  VTM,   BSS+ 0052B
008041 :             13 ,  VJM,  D*ST*D
008042 :             14 ,  VTM,   BSS+ 0052B
008043 :             13 ,  VJM,  D*LN*D
008044 :             14 ,  VTM,   BSS+ 0054B
008045 :             13 ,  VJM,  D*ST*D
008046 :              7 ,  XTA,  13
008047 :              7 ,  ATX,  1
008048 :  *00056    : 7 ,  XTA,  1
008049 :             14 ,  VTM,   BSS+ 0056B
008050 :             13 ,  VJM,  I*ST*D
008051 :             14 ,  VTM,   BSS+ 0052B
008052 :             13 ,  VJM,  D*LO*D
008053 :             14 ,  VTM,   BSS+ 0056B
008054 :             13 ,  VJM,  D*DI*D
008055 :             14 ,  VTM,   BSS+ 0060B
008056 :             13 ,  VJM,  D*AD*D
008057 :             14 ,  VTM,   BSS+ 0060B
008058 :             13 ,  VJM,  D*ST*D
008059 :             14 ,  VTM,   BSS+ 0052B
008060 :             13 ,  VJM,  D*LO*D
008061 :             14 ,  VTM,   BSS+ 0054B
008062 :             13 ,  VJM,  D*MU*D
008063 :             14 ,  VTM,   BSS+ 0052B
008064 :             13 ,  VJM,  D*ST*D
008065 :              7 ,  XTA,  1
008066 :                  ,NTR,  3
008067 :              7 ,  A+X,  13
008068 :                  ,NTR,  22B
008069 :              7 ,  ATX,  1
008070 :              7 ,  X-A,  20B
008071 :                  ,UZA,   *00056
008072 :                  ,WTC,   *00137
008073 :             14 ,  VTM,
008074 :             13 ,  VJM,  D*LN*D
008075 :             14 ,  VTM,   *00143
008076 :             13 ,  VJM,  D*AD*D
008077 :                  ,UZA,   *00076
008078 :                  , UJ,   *00100
008079 :  *00076    :14 ,  VTM,   BSS+ 0060B
008080 :             13 ,  VJM,  D*LN*D
008081 :             14 ,  VTM,   BSS+ 0060B
008082 :             13 ,  VJM,  D*ST*D
008083 :  *00100    :     , UJ,   *00124
008084 :                  ,UTC,
008085 :  *00101    :14 ,  VTM,   *00152
008086 :             13 ,  VJM,  D*LO*D
008087 :             14 ,  VTM,   *00152
008088 :             13 ,  VJM,  D*MU*D
008089 :             14 ,  VTM,   *00152
008090 :             13 ,  VJM,  D*MU*D
008091 :             14 ,  VTM,   BSS+ 0052B
008092 :             13 ,  VJM,  D*ST*D
008093 :                  , UJ,   *00124
008094 :                  ,UTC,
008095 :  *00106    : 7 ,  XTA,  14
008096 :              7 ,  A+X,  7
008097 :              7 ,  ATX,  7
008098 :              7 ,  XTA,  23B
008099 :              7 ,  A+X,  5
008100 :              7 ,  ATX,  5
008101 :             13 ,   UJ,
008102 :                  ,UTC,
008103 :                  , UJ,   *00106
008104 :                  ,UTC,
008105 :  *00113    : 7 ,  A-X,  5
008106 :              7 ,  ATX,  5
008107 :                  ,E+N,  101B
008108 :              7 ,  A+X,  7
008109 :              7 ,  ATX,  7
008110 :              7 ,  XTA,  2
008111 :              7 ,  ATX,  5
008112 :             13 ,   UJ,
008113 :  *00117    : 7 ,  MTJ,  8
008114 :              7 ,  VTM,   *00131
008115 :                  ,NTR,  2
008116 :              7 ,  STX,  6
008117 :                  ,ITS,  13
008118 :                  ,ITS,  1
008119 :                  ,ITS,  8
008120 :                  ,ITS,
008121 :             14 ,   UJ,
008122 :                  ,UTC,
008123 :  *00124    :     ,STI,
008124 :                  ,STI,  7
008125 :                  ,STI,  1
008126 :                  ,STI,  8
008127 :                  ,XTS,
008128 :             14 ,  VTM,   BSS+ 0060B
008129 :             13 ,  VJM,  D*LO*D
008130 :              8 ,   UJ,
008131 :                  ,UTC,
008132 :  *00131    :,     LOG,  00000000 00000000.
008133 :             ,     LOG,  00000000 00000000.
008134 :             ,     LOG,  00000000 00000000.
008135 :             ,     LOG,  00000000 00000000.
008136 :             ,     LOG,  00000000 00000000.
008137 :             ,     LOG,  00000000 00000000.
008138 :  *00137    :,     LOG,  00000000 00000000.
008139 :  *00140    :,     LOG,  64140000 00000000.
008140 :  *00141    :,     LOG,  00000000 00000000.
008141 :             ,     LOG,  00000000 00000000.
008142 :  *00143    :,REAL, 1.00000000000E+00
008143 :             ,     LOG,  00000000 00000000.
008144 :   ,LOG,4650 0000 0000 0000
008145 :             ,INT,              1
008146 :             ,REAL, 2.00000000000E+00
008147 :             ,INT,             21
008148 :             ,INT,              9
008149 :  *00152    :,     LOG,  73541327 60312260.
008150 :  *00153    :,     LOG,  31577043 15711612.
008151 :             ,     LOG,  40500000 00000000.
008152 :  BSS       :     ,BSS,  62B
008153 :        ,DATA,.  :,ENTRY,
008154 :  *00155    :,     LOG,  53115514 04312451.
008155 :             ,     LOG,  56070545 33655733.
008156 :             ,     LOG,  45543543 51226405.
008157 :             ,     LOG,  27026474 27562544.
008158 :             ,     LOG,  43117701 32203472.
008159 :             ,     LOG,  13470766 40152420.
008160 :             ,     LOG,  61545073 12022724.
008161 :             ,     LOG,  05414116 15516630.
008162 :             ,     LOG,  71120343 65040245.
008163 :             ,     LOG,  02567376 37264572.
008164 :             ,     LOG,  74545362 51427352.
008165 :             ,     LOG,  01006156 63112744.
008166 :             ,     LOG,  56520454 60263654.
008167 :             ,     LOG,  00536263 60002142.
008168 :             ,     LOG,  67507641 77461133.
008169 :             ,     LOG,  00030241 05452314.
008170 :             ,     LOG,  54103627 27502500.
008171 :             ,     LOG,  00000246 24464244.
008172 :             ,     LOG,  46135117 24765070.
008173 :             ,     LOG,  00061460 25675620.
008174 :             ,     LOG,  43155144 40270471.
008175 :             ,     LOG,  00151236 15110174.
008176 :             ,     LOG,  41566163 11343246.
008177 :             ,     LOG,  00166532 25343470.
008178 :             ,     LOG,  41126770 25054242.
008179 :             ,     LOG,  00135503 16545112.
008180 :             ,     LOG,  40551411 23070360.
008181 :             ,     LOG,  00032331 64502710.
008182 :             ,     LOG,  40522132 74360764.
008183 :             ,     LOG,  00006061 37255044.
008184 :             ,     LOG,  40510413 00426667.
008185 :             ,     LOG,  00127147 02333462.
008186 :             ,     LOG,  40504101 26535751.
008187 :             ,     LOG,  00123553 43467336.
008188 :             ,     LOG,  40502020 12600211.
008189 :             ,     LOG,  00037544 46436340.
008190 :             ,     LOG,  40501004 01255256.
008191 :             ,     LOG,  00170710 51650220.
008192 :             ,     LOG,  40500401 00125400.
008193 :             ,     LOG,  00021055 36624530.
008194 :             ,     LOG,  40500200 20012532.
008195 :             ,     LOG,  00125673 70751236.
008196 :                42,SET,   *00155
008197 :                 1,   ,   BSS+ 0000B
008198 :       ,END,
008199 :       DLOG10     :,NAME,
008200 :       DLOG       :,SUBP,
008201 :       D*ST*D     :,SUBP,
008202 :       D*LO*D     :,SUBP,
008203 :       D*DI*D     :,SUBP,
008204 :             14 ,  VTM,   *00001
008205 :                  , UJ,   *00013
008206 :  *00001    :     ,WTC,   *00030
008207 :             14 ,  VTM,
008208 :                  ,ITA,  14
008209 :             13 ,  VJM,      DLOG
008210 :             14 ,  VTM,   *00024
008211 :             13 ,  VJM,  D*ST*D
008212 :             14 ,  VTM,   *00031
008213 :                  ,ITA,  14
008214 :             13 ,  VJM,      DLOG
008215 :             14 ,  VTM,   *00026
008216 :             13 ,  VJM,  D*ST*D
008217 :             14 ,  VTM,   *00024
008218 :             13 ,  VJM,  D*LO*D
008219 :             14 ,  VTM,   *00026
008220 :             13 ,  VJM,  D*DI*D
008221 :             14 ,  VTM,   BSS+ 0000B
008222 :             13 ,  VJM,  D*ST*D
008223 :                  , UJ,   *00017
008224 :                  ,UTC,
008225 :  *00013    : 7 ,  MTJ,  8
008226 :              7 ,  VTM,   *00023
008227 :                  ,NTR,  2
008228 :              7 ,  STX,  5
008229 :                  ,ITS,  13
008230 :                  ,ITS,  8
008231 :                  ,ITS,
008232 :             14 ,   UJ,
008233 :  *00017    :     ,STI,
008234 :                  ,STI,  7
008235 :                  ,STI,  8
008236 :                  ,XTS,
008237 :             14 ,  VTM,   BSS+ 0000B
008238 :             13 ,  VJM,  D*LO*D
008239 :              8 ,   UJ,
008240 :                  ,UTC,
008241 :  *00023    :,     LOG,  00000000 00000000.
008242 :  *00024    :,     LOG,  00000000 00000000.
008243 :             ,     LOG,  00000000 00000000.
008244 :  *00026    :,     LOG,  00000000 00000000.
008245 :             ,     LOG,  00000000 00000000.
008246 :  *00030    :,     LOG,  00000000 00000000.
008247 :  *00031    :,REAL, 1.00000000000E+01
008248 :             ,     LOG,  00000000 00000000.
008249 :  BSS       :     ,BSS,  2
008250 :       ,END,
008251 :       DMAX1      :,NAME,
008252 :       D*LO*D     :,SUBP,
008253 :       D*SU*D     :,SUBP,
008254 :             12 ,  VJM,   *00036
008255 :                  ,ATI,  10
008256 :                  ,UTC,
008257 :  *00002    :     ,STI,
008258 :                  ,ATI,  9
008259 :                  ,ARX,
008260 :                  ,UIA,   *00013
008261 :             10 ,  MTJ,  14
008262 :             13 ,  VJM,  D*LO*D
008263 :              9 ,  MTJ,  14
008264 :             13 ,  VJM,  D*SU*D
008265 :                  ,UZA,   *00011
008266 :              9 ,  MTJ,  14
008267 :             13 ,  VJM,  D*LO*D
008268 :                  , UJ,   *00041
008269 :                  ,UTC,
008270 :  *00011    :10 ,  MTJ,  14
008271 :             13 ,  VJM,  D*LO*D
008272 :                  , UJ,   *00041
008273 :                  ,UTC,
008274 :  *00013    :10 ,  MTJ,  14
008275 :             13 ,  VJM,  D*LO*D
008276 :              9 ,  MTJ,  14
008277 :             13 ,  VJM,  D*SU*D
008278 :                  ,UZA,   *00002
008279 :              9 ,  MTJ,  10
008280 :                  , UJ,   *00002
008281 :                  ,UTC,
008282 :       DMIN1     :,ENTRY,
008283 :             12 ,  VJM,   *00036
008284 :                  ,ATI,  10
008285 :                  ,UTC,
008286 :  *00021    :     ,STI,
008287 :                  ,ATI,  9
008288 :                  ,ARX,
008289 :                  ,UIA,   *00032
008290 :             10 ,  MTJ,  14
008291 :             13 ,  VJM,  D*LO*D
008292 :              9 ,  MTJ,  14
008293 :             13 ,  VJM,  D*SU*D
008294 :                  ,UIA,   *00030
008295 :              9 ,  MTJ,  14
008296 :             13 ,  VJM,  D*LO*D
008297 :                  , UJ,   *00041
008298 :                  ,UTC,
008299 :  *00030    :10 ,  MTJ,  14
008300 :             13 ,  VJM,  D*LO*D
008301 :                  , UJ,   *00041
008302 :                  ,UTC,
008303 :  *00032    :10 ,  MTJ,  14
008304 :             13 ,  VJM,  D*LO*D
008305 :              9 ,  MTJ,  14
008306 :             13 ,  VJM,  D*SU*D
008307 :                  ,UIA,   *00021
008308 :              9 ,  MTJ,  10
008309 :                  , UJ,   *00021
008310 :                  ,UTC,
008311 :  *00036    :14 ,  VTM,   BSS+ 0000B
008312 :                  ,ITS,  13
008313 :             14 ,  ATX,
008314 :                  ,RTE,  177B
008315 :             14 ,  STX,  1
008316 :             12 ,   UJ,
008317 :  *00041    :14 ,  VTM,   BSS+ 0000B
008318 :             14 ,  XTS,
008319 :                  ,STI,  13
008320 :             14 ,  XTR,  1
008321 :             13 ,   UJ,
008322 :                  ,UTC,
008323 :  BSS       :     ,BSS,  2
008324 :       ,END,
008325 :       DMOD       :,NAME,
008326 :       DINT       :,SUBP,
008327 :       D*LO*D     :,SUBP,
008328 :       D*DI*D     :,SUBP,
008329 :       D*ST*D     :,SUBP,
008330 :       D*MU*D     :,SUBP,
008331 :       D*COMP     :,SUBP,
008332 :       D*AD*D     :,SUBP,
008333 :             14 ,  VTM,   *00001
008334 :                  , UJ,   *00017
008335 :  *00001    :     ,WTC,   *00033
008336 :             14 ,  VTM,
008337 :             13 ,  VJM,  D*LO*D
008338 :                  ,WTC,   *00034
008339 :             14 ,  VTM,
008340 :             13 ,  VJM,  D*DI*D
008341 :             14 ,  VTM,   *00031
008342 :             13 ,  VJM,  D*ST*D
008343 :             14 ,  VTM,   *00031
008344 :                  ,ITA,  14
008345 :             13 ,  VJM,      DINT
008346 :                  ,WTC,   *00034
008347 :             14 ,  VTM,
008348 :             13 ,  VJM,  D*MU*D
008349 :             14 ,  VTM,
008350 :             13 ,  VJM,  D*COMP
008351 :                  ,WTC,   *00033
008352 :             14 ,  VTM,
008353 :             13 ,  VJM,  D*AD*D
008354 :             14 ,  VTM,   BSS+ 0000B
008355 :             13 ,  VJM,  D*ST*D
008356 :                  , UJ,   *00024
008357 :                  ,UTC,
008358 :  *00017    : 7 ,  MTJ,  8
008359 :              7 ,  VTM,   *00030
008360 :                  ,NTR,  2
008361 :              7 ,  STX,  4
008362 :              7 ,  STX,  3
008363 :                  ,ITS,  13
008364 :                  ,ITS,  8
008365 :                  ,ITS,
008366 :             14 ,   UJ,
008367 :                  ,UTC,
008368 :  *00024    :     ,STI,
008369 :                  ,STI,  7
008370 :                  ,STI,  8
008371 :                  ,XTS,
008372 :             14 ,  VTM,   BSS+ 0000B
008373 :             13 ,  VJM,  D*LO*D
008374 :              8 ,   UJ,
008375 :                  ,UTC,
008376 :  *00030    :,     LOG,  00000000 00000000.
008377 :  *00031    :,     LOG,  00000000 00000000.
008378 :             ,     LOG,  00000000 00000000.
008379 :  *00033    :,     LOG,  00000000 00000000.
008380 :  *00034    :,     LOG,  00000000 00000000.
008381 :  BSS       :     ,BSS,  2
008382 :       ,END,
008383 :  DRUMTAP*:,NAME,
008384 :  CHKWORD*:,LC,1
008385 :  PRINT8:,SUBP,
008386 :  STOP*:,SUBP,
008387 :  14,BASE,*
008388 :  ,ARX,=100 0001
008389 :  ,ATX,R
008390 :  ,ASN,64-30
008391 :  ,ARX,=:1
008392 :  ,U1A,NOML
008393 :  RET:,XTA,R
008394 :  13,UJ,
008395 :  NOML:,XTA,R
008396 :  ,AAX,=7740
008397 :  ,UZA,RET
008398 :  ,XTA,R
008399 :  ,ARX,=7740
008400 :  ,ATX,R
008401 :  ,AAX,=:001
008402 :  ,U1A,RET
008403 :  ,XTA,R
008404 :  ,UTC,CHKWORD*
008405 :  ,AEX,
008406 :  ,AAX,=77 7777
008407 :  ,U1A,RET
008408 :  14,VTM,TEXT
008409 :  ,ITS,14
008410 :  14,UTM,4
008411 :  ,ITS,14
008412 :  ,XTS,
008413 :  13,VTM,STOP*
008414 :  ,UJ,PRINT8
008415 :  TEXT:,ISO,12HTOO MANY INF
008416 :  ,CONT,12HORMATION FOR
008417 :  ,CONT,6HDRUM
008418 :  R:,BSS,1
008419 :  ,END,
008420 :       DSIGN      :,NAME,
008421 :       D*LO*D     :,SUBP,
008422 :       D*SU*D     :,SUBP,
008423 :       D*ST*D     :,SUBP,
008424 :             14 ,  VTM,   *00001
008425 :                  , UJ,   *00027
008426 :  *00001    :     ,WTC,   *00042
008427 :             14 ,  VTM,
008428 :             13 ,  VJM,  D*LO*D
008429 :                  ,ASN,  71B
008430 :                  ,UZA,   *00017
008431 :                  ,ARX,
008432 :                  ,UZA,   *00006
008433 :                  , UJ,   *00017
008434 :                  ,UTC,
008435 :  *00006    :     ,WTC,   *00041
008436 :             14 ,  VTM,
008437 :             13 ,  VJM,  D*LO*D
008438 :                  ,ASN,  71B
008439 :                  ,UZA,   *00023
008440 :                  ,ARX,
008441 :                  ,UZA,   *00023
008442 :  *00012    :14 ,  VTM,   *00043
008443 :             13 ,  VJM,  D*LO*D
008444 :                  ,WTC,   *00041
008445 :             14 ,  VTM,
008446 :             13 ,  VJM,  D*SU*D
008447 :             14 ,  VTM,   BSS+ 0000B
008448 :             13 ,  VJM,  D*ST*D
008449 :                  , UJ,   *00034
008450 :                  ,UTC,
008451 :  *00017    :     ,WTC,   *00041
008452 :             14 ,  VTM,
008453 :             13 ,  VJM,  D*LO*D
008454 :                  ,ASN,  71B
008455 :                  ,UZA,   *00023
008456 :                  ,ARX,
008457 :                  ,UZA,   *00012
008458 :  *00023    :     ,WTC,   *00041
008459 :             14 ,  VTM,
008460 :             13 ,  VJM,  D*LO*D
008461 :             14 ,  VTM,   BSS+ 0000B
008462 :             13 ,  VJM,  D*ST*D
008463 :                  , UJ,   *00034
008464 :                  ,UTC,
008465 :  *00027    : 7 ,  MTJ,  8
008466 :              7 ,  VTM,   *00040
008467 :                  ,NTR,  2
008468 :              7 ,  STX,  2
008469 :              7 ,  STX,  1
008470 :                  ,ITS,  13
008471 :                  ,ITS,  8
008472 :                  ,ITS,
008473 :             14 ,   UJ,
008474 :                  ,UTC,
008475 :  *00034    :     ,STI,
008476 :                  ,STI,  7
008477 :                  ,STI,  8
008478 :                  ,XTS,
008479 :             14 ,  VTM,   BSS+ 0000B
008480 :             13 ,  VJM,  D*LO*D
008481 :              8 ,   UJ,
008482 :                  ,UTC,
008483 :  *00040    :,     LOG,  00000000 00000000.
008484 :  *00041    :,     LOG,  00000000 00000000.
008485 :  *00042    :,     LOG,  00000000 00000000.
008486 :  *00043    :,     LOG,  00000000 00000000.
008487 :             ,     LOG,  00000000 00000000.
008488 :  BSS       :     ,BSS,  2
008489 :       ,END,
008490 :       DSIN       :,NAME,
008491 :       DABS       :,SUBP,
008492 :       DSIGN      :,SUBP,
008493 :       DINT       :,SUBP,
008494 :       D*LO*D     :,SUBP,
008495 :       D*ST*D     :,SUBP,
008496 :       D*MU*D     :,SUBP,
008497 :       D*LN*D     :,SUBP,
008498 :       D*AD*D     :,SUBP,
008499 :       D*DI*D     :,SUBP,
008500 :       D*SU*D     :,SUBP,
008501 :       D*COMP     :,SUBP,
008502 :             14 ,  VTM,   *00002
008503 :                  , UJ,   *00267
008504 :       DCOS      :,ENTRY,
008505 :             14 ,  VTM,   *00177
008506 :                  , UJ,   *00267
008507 :  *00002    : 7 ,  XTA,  13
008508 :              7 ,  ATX,  5
008509 :             14 ,  VTM,   *00315
008510 :             13 ,  VJM,  D*LO*D
008511 :             14 ,  VTM,   BSS+ 0000B
008512 :             13 ,  VJM,  D*ST*D
008513 :                  ,WTC,   *00305
008514 :             14 ,  VTM,
008515 :             13 ,  VJM,  D*LO*D
008516 :             14 ,  VTM,   BSS+ 0002B
008517 :             13 ,  VJM,  D*ST*D
008518 :                  ,WTC,   *00305
008519 :             14 ,  VTM,
008520 :                  ,ITA,  14
008521 :             13 ,  VJM,      DABS
008522 :             14 ,  VTM,   BSS+ 0004B
008523 :             13 ,  VJM,  D*ST*D
008524 :             14 ,  VTM,   *00317
008525 :             13 ,  VJM,  D*LO*D
008526 :             14 ,  VTM,   *00310
008527 :             13 ,  VJM,  D*MU*D
008528 :             14 ,  VTM,   BSS+ 0006B
008529 :             13 ,  VJM,  D*ST*D
008530 :             14 ,  VTM,   BSS+ 0004B
008531 :             13 ,  VJM,  D*LN*D
008532 :             14 ,  VTM,   BSS+ 0006B
008533 :             13 ,  VJM,  D*AD*D
008534 :                  ,UZA,   *00021
008535 :                  , UJ,   *00022
008536 :  *00021    :     , UJ,   *00043
008537 :                  ,UTC,
008538 :  *00022    :     ,WTC,   *00305
008539 :             14 ,  VTM,
008540 :             13 ,  VJM,  D*LO*D
008541 :             14 ,  VTM,   BSS+ 0006B
008542 :             13 ,  VJM,  D*DI*D
008543 :             14 ,  VTM,   *00300
008544 :             13 ,  VJM,  D*ST*D
008545 :             14 ,  VTM,   *00300
008546 :                  ,ITA,  14
008547 :             13 ,  VJM,      DINT
008548 :             14 ,  VTM,   BSS+ 0002B
008549 :             13 ,  VJM,  D*ST*D
008550 :             14 ,  VTM,   BSS+ 0002B
008551 :                  ,ITA,  14
008552 :             13 ,  VJM,      DABS
008553 :             14 ,  VTM,   *00306
008554 :             13 ,  VJM,  D*SU*D
008555 :                  ,UZA,   *00035
008556 :                  , UJ,   *00036
008557 :  *00035    :     , UJ,   *00273
008558 :                  ,UTC,
008559 :  *00036    :14 ,  VTM,   BSS+ 0002B
008560 :             13 ,  VJM,  D*LN*D
008561 :             14 ,  VTM,   BSS+ 0006B
008562 :             13 ,  VJM,  D*MU*D
008563 :                  ,WTC,   *00305
008564 :             14 ,  VTM,
008565 :             13 ,  VJM,  D*AD*D
008566 :             14 ,  VTM,   BSS+ 0002B
008567 :             13 ,  VJM,  D*ST*D
008568 :  *00043    :14 ,  VTM,   BSS+ 0002B
008569 :                  ,ITA,  14
008570 :             13 ,  VJM,      DABS
008571 :             14 ,  VTM,   BSS+ 0004B
008572 :             13 ,  VJM,  D*ST*D
008573 :             14 ,  VTM,   BSS+ 0004B
008574 :             13 ,  VJM,  D*LN*D
008575 :             14 ,  VTM,   *00310
008576 :             13 ,  VJM,  D*AD*D
008577 :                  ,UZA,   *00051
008578 :                  , UJ,   *00052
008579 :  *00051    :     , UJ,   *00066
008580 :                  ,UTC,
008581 :  *00052    :14 ,  VTM,   *00321
008582 :             13 ,  VJM,  D*LO*D
008583 :             14 ,  VTM,   *00310
008584 :             13 ,  VJM,  D*MU*D
008585 :             14 ,  VTM,   BSS+ 0010B
008586 :             13 ,  VJM,  D*ST*D
008587 :             14 ,  VTM,   *00323
008588 :             13 ,  VJM,  D*LO*D
008589 :             14 ,  VTM,   *00310
008590 :             13 ,  VJM,  D*MU*D
008591 :             14 ,  VTM,   BSS+ 0004B
008592 :             13 ,  VJM,  D*SU*D
008593 :                  ,UIA,   *00061
008594 :                  , UJ,   *00062
008595 :  *00061    :     , UJ,   *00157
008596 :                  ,UTC,
008597 :  *00062    :14 ,  VTM,   BSS+ 0004B
008598 :             13 ,  VJM,  D*LN*D
008599 :             14 ,  VTM,   *00310
008600 :             13 ,  VJM,  D*AD*D
008601 :                  ,UIA,   *00065
008602 :                  , UJ,   *00066
008603 :  *00065    :     , UJ,   *00171
008604 :                  ,UTC,
008605 :  *00066    :14 ,  VTM,   BSS+ 0002B
008606 :                  ,ITA,  14
008607 :             13 ,  VJM,      DABS
008608 :             14 ,  VTM,
008609 :             13 ,  VJM,  D*COMP
008610 :             14 ,  VTM,   *00300
008611 :             13 ,  VJM,  D*ST*D
008612 :             14 ,  VTM,   *00300
008613 :             13 ,  VJM,  D*LO*D
008614 :             14 ,  VTM,   *00312
008615 :             13 ,  VJM,  D*AD*D
008616 :                  ,UZA,   *00075
008617 :                  , UJ,   *00076
008618 :  *00075    :     , UJ,   *00102
008619 :                  ,UTC,
008620 :  *00076    :14 ,  VTM,   *00325
008621 :             13 ,  VJM,  D*LO*D
008622 :             14 ,  VTM,   BSS+ 0010B
008623 :             13 ,  VJM,  D*ST*D
008624 :  *00100    : 7 ,  XTA,  30B
008625 :              7 ,  ATX,  4
008626 :                  , UJ,   *00105
008627 :                  ,UTC,
008628 :  *00102    :14 ,  VTM,   *00330
008629 :             13 ,  VJM,  D*LO*D
008630 :             14 ,  VTM,   BSS+ 0010B
008631 :             13 ,  VJM,  D*ST*D
008632 :  *00104    : 7 ,  XTA,  33B
008633 :              7 ,  ATX,  4
008634 :  *00105    :14 ,  VTM,   BSS+ 0002B
008635 :             13 ,  VJM,  D*LO*D
008636 :             14 ,  VTM,   BSS+ 0002B
008637 :             13 ,  VJM,  D*MU*D
008638 :             14 ,  VTM,   BSS+ 0012B
008639 :             13 ,  VJM,  D*ST*D
008640 :             14 ,  VTM,   *00315
008641 :             13 ,  VJM,  D*LN*D
008642 :             14 ,  VTM,   BSS+ 0006B
008643 :             13 ,  VJM,  D*ST*D
008644 :              7 ,  XTA,  13
008645 :              7 ,  ATX,  3
008646 :                  , UJ,   *00135
008647 :                  ,UTC,
008648 :  *00114    :14 ,  VTM,   BSS+ 0010B
008649 :             13 ,  VJM,  D*LO*D
008650 :             14 ,  VTM,   BSS+ 0010B
008651 :             13 ,  VJM,  D*MU*D
008652 :             14 ,  VTM,   BSS+ 0010B
008653 :             13 ,  VJM,  D*SU*D
008654 :             14 ,  VTM,   BSS+ 0014B
008655 :             13 ,  VJM,  D*ST*D
008656 :             14 ,  VTM,   BSS+ 0012B
008657 :             13 ,  VJM,  D*LO*D
008658 :             14 ,  VTM,   BSS+ 0014B
008659 :             13 ,  VJM,  D*DI*D
008660 :             14 ,  VTM,   BSS+ 0006B
008661 :             13 ,  VJM,  D*MU*D
008662 :             14 ,  VTM,   BSS+ 0000B
008663 :             13 ,  VJM,  D*AD*D
008664 :             14 ,  VTM,   BSS+ 0006B
008665 :             13 ,  VJM,  D*ST*D
008666 :             14 ,  VTM,   BSS+ 0000B
008667 :             13 ,  VJM,  D*LN*D
008668 :             14 ,  VTM,   BSS+ 0000B
008669 :             13 ,  VJM,  D*ST*D
008670 :             14 ,  VTM,   BSS+ 0010B
008671 :             13 ,  VJM,  D*LO*D
008672 :             14 ,  VTM,   *00321
008673 :             13 ,  VJM,  D*SU*D
008674 :             14 ,  VTM,   BSS+ 0010B
008675 :             13 ,  VJM,  D*ST*D
008676 :              7 ,  XTA,  3
008677 :                  ,NTR,  3
008678 :              7 ,  A+X,  13
008679 :                  ,NTR,  22B
008680 :              7 ,  ATX,  3
008681 :                  ,UTC,
008682 :  *00135    : 7 ,  X-A,  4
008683 :                  ,UZA,   *00114
008684 :  *00136    :     ,WTC,   *00304
008685 :                  , UJ,   *00136
008686 :                  , UJ,   *00142
008687 :                  ,UTC,
008688 :                  , UJ,   *00146
008689 :                  ,UTC,
008690 :                  , UJ,   *00151
008691 :                  ,UTC,
008692 :  *00142    :14 ,  VTM,   BSS+ 0006B
008693 :             13 ,  VJM,  D*LO*D
008694 :             14 ,  VTM,   BSS+ 0002B
008695 :             13 ,  VJM,  D*MU*D
008696 :             14 ,  VTM,   BSS+ 0016B
008697 :             13 ,  VJM,  D*ST*D
008698 :                  , UJ,   *00273
008699 :                  ,UTC,
008700 :  *00146    :14 ,  VTM,   BSS+ 0006B
008701 :             13 ,  VJM,  D*LO*D
008702 :             14 ,  VTM,   BSS+ 0016B
008703 :             13 ,  VJM,  D*ST*D
008704 :                  , UJ,   *00273
008705 :                  ,UTC,
008706 :  *00151    :14 ,  VTM,   BSS+ 0006B
008707 :             13 ,  VJM,  D*LO*D
008708 :             14 ,  VTM,   BSS+ 0006B
008709 :             13 ,  VJM,  D*MU*D
008710 :             14 ,  VTM,   *00321
008711 :             13 ,  VJM,  D*MU*D
008712 :             14 ,  VTM,   *00315
008713 :             13 ,  VJM,  D*SU*D
008714 :             14 ,  VTM,   BSS+ 0016B
008715 :             13 ,  VJM,  D*ST*D
008716 :                  , UJ,   *00273
008717 :                  ,UTC,
008718 :  *00157    :14 ,  VTM,   *00317
008719 :             13 ,  VJM,  D*LO*D
008720 :             14 ,  VTM,   *00310
008721 :             13 ,  VJM,  D*MU*D
008722 :             14 ,  VTM,   *00300
008723 :             13 ,  VJM,  D*ST*D
008724 :             14 ,  VTM,   *00300
008725 :                  ,ITA,  14
008726 :             14 ,  VTM,   BSS+ 0002B
008727 :                  ,ITS,  14
008728 :             13 ,  VJM,  DSIGN
008729 :             14 ,  VTM,
008730 :             13 ,  VJM,  D*COMP
008731 :             14 ,  VTM,   BSS+ 0002B
008732 :             13 ,  VJM,  D*AD*D
008733 :             14 ,  VTM,   BSS+ 0002B
008734 :             13 ,  VJM,  D*ST*D
008735 :                  , UJ,   *00066
008736 :                  ,UTC,
008737 :  *00171    :14 ,  VTM,   BSS+ 0010B
008738 :                  ,ITA,  14
008739 :             14 ,  VTM,   BSS+ 0002B
008740 :                  ,ITS,  14
008741 :             13 ,  VJM,  DSIGN
008742 :             14 ,  VTM,   BSS+ 0002B
008743 :             13 ,  VJM,  D*SU*D
008744 :             14 ,  VTM,   BSS+ 0002B
008745 :             13 ,  VJM,  D*ST*D
008746 :                  , UJ,   *00066
008747 :                  ,UTC,
008748 :  *00177    : 7 ,  XTA,  34B
008749 :              7 ,  ATX,  5
008750 :                  ,WTC,   *00305
008751 :             14 ,  VTM,
008752 :                  ,ITA,  14
008753 :             13 ,  VJM,      DABS
008754 :             14 ,  VTM,   BSS+ 0002B
008755 :             13 ,  VJM,  D*ST*D
008756 :             14 ,  VTM,   *00315
008757 :             13 ,  VJM,  D*LO*D
008758 :             14 ,  VTM,   BSS+ 0000B
008759 :             13 ,  VJM,  D*ST*D
008760 :             14 ,  VTM,   *00317
008761 :             13 ,  VJM,  D*LO*D
008762 :             14 ,  VTM,   *00310
008763 :             13 ,  VJM,  D*MU*D
008764 :             14 ,  VTM,   BSS+ 0006B
008765 :             13 ,  VJM,  D*ST*D
008766 :             14 ,  VTM,   BSS+ 0002B
008767 :             13 ,  VJM,  D*LN*D
008768 :             14 ,  VTM,   BSS+ 0006B
008769 :             13 ,  VJM,  D*AD*D
008770 :                  ,UIA,   *00213
008771 :                  , UJ,   *00214
008772 :  *00213    :     , UJ,   *00243
008773 :                  ,UTC,
008774 :  *00214    :14 ,  VTM,   *00321
008775 :             13 ,  VJM,  D*LO*D
008776 :             14 ,  VTM,   *00310
008777 :             13 ,  VJM,  D*MU*D
008778 :             14 ,  VTM,   BSS+ 0002B
008779 :             13 ,  VJM,  D*SU*D
008780 :                  ,UZA,   *00220
008781 :                  , UJ,   *00221
008782 :  *00220    :     , UJ,   *00225
008783 :                  ,UTC,
008784 :  *00221    :14 ,  VTM,   *00317
008785 :             13 ,  VJM,  D*LO*D
008786 :             14 ,  VTM,   *00310
008787 :             13 ,  VJM,  D*MU*D
008788 :             14 ,  VTM,   BSS+ 0002B
008789 :             13 ,  VJM,  D*SU*D
008790 :             14 ,  VTM,   BSS+ 0002B
008791 :             13 ,  VJM,  D*ST*D
008792 :  *00225    :14 ,  VTM,   BSS+ 0002B
008793 :             13 ,  VJM,  D*LN*D
008794 :             14 ,  VTM,   *00310
008795 :             13 ,  VJM,  D*AD*D
008796 :                  ,UIA,   *00230
008797 :                  , UJ,   *00231
008798 :  *00230    :     , UJ,   *00262
008799 :                  ,UTC,
008800 :  *00231    :14 ,  VTM,   BSS+ 0002B
008801 :             13 ,  VJM,  D*LN*D
008802 :             14 ,  VTM,   *00312
008803 :             13 ,  VJM,  D*AD*D
008804 :                  ,UIA,   *00234
008805 :                  , UJ,   *00235
008806 :  *00234    :     , UJ,   *00240
008807 :                  ,UTC,
008808 :  *00235    :14 ,  VTM,   *00334
008809 :             13 ,  VJM,  D*LO*D
008810 :             14 ,  VTM,   BSS+ 0010B
008811 :             13 ,  VJM,  D*ST*D
008812 :                  , UJ,   *00104
008813 :                  ,UTC,
008814 :  *00240    :14 ,  VTM,   *00336
008815 :             13 ,  VJM,  D*LO*D
008816 :             14 ,  VTM,   BSS+ 0010B
008817 :             13 ,  VJM,  D*ST*D
008818 :                  , UJ,   *00100
008819 :                  ,UTC,
008820 :  *00243    :14 ,  VTM,   BSS+ 0002B
008821 :             13 ,  VJM,  D*LO*D
008822 :             14 ,  VTM,   BSS+ 0006B
008823 :             13 ,  VJM,  D*DI*D
008824 :             14 ,  VTM,   *00300
008825 :             13 ,  VJM,  D*ST*D
008826 :             14 ,  VTM,   *00300
008827 :                  ,ITA,  14
008828 :             13 ,  VJM,      DINT
008829 :             14 ,  VTM,   BSS+ 0004B
008830 :             13 ,  VJM,  D*ST*D
008831 :             14 ,  VTM,   BSS+ 0004B
008832 :             13 ,  VJM,  D*LO*D
008833 :             14 ,  VTM,   *00306
008834 :             13 ,  VJM,  D*SU*D
008835 :                  ,UZA,   *00254
008836 :                  , UJ,   *00255
008837 :  *00254    :     , UJ,   *00273
008838 :                  ,UTC,
008839 :  *00255    :14 ,  VTM,   BSS+ 0004B
008840 :             13 ,  VJM,  D*LN*D
008841 :             14 ,  VTM,   BSS+ 0006B
008842 :             13 ,  VJM,  D*MU*D
008843 :             14 ,  VTM,   BSS+ 0002B
008844 :             13 ,  VJM,  D*AD*D
008845 :             14 ,  VTM,   BSS+ 0002B
008846 :             13 ,  VJM,  D*ST*D
008847 :                  , UJ,   *00214
008848 :                  ,UTC,
008849 :  *00262    :14 ,  VTM,   BSS+ 0002B
008850 :             13 ,  VJM,  D*LO*D
008851 :             14 ,  VTM,   *00340
008852 :             13 ,  VJM,  D*MU*D
008853 :             14 ,  VTM,   BSS+ 0002B
008854 :             13 ,  VJM,  D*ST*D
008855 :              7 ,  XTA,  43B
008856 :              7 ,  ATX,  5
008857 :                  , UJ,   *00231
008858 :                  ,UTC,
008859 :  *00267    : 7 ,  MTJ,  8
008860 :              7 ,  VTM,   *00277
008861 :                  ,NTR,  2
008862 :              7 ,  STX,  6
008863 :                  ,ITS,  13
008864 :                  ,ITS,  8
008865 :                  ,ITS,
008866 :             14 ,   UJ,
008867 :  *00273    :     ,STI,
008868 :                  ,STI,  7
008869 :                  ,STI,  8
008870 :                  ,XTS,
008871 :             14 ,  VTM,   BSS+ 0016B
008872 :             13 ,  VJM,  D*LO*D
008873 :              8 ,   UJ,
008874 :                  ,UTC,
008875 :  *00277    :,     LOG,  00000000 00000000.
008876 :  *00300    :,     LOG,  00000000 00000000.
008877 :             ,     LOG,  00000000 00000000.
008878 :             ,     LOG,  00000000 00000000.
008879 :             ,     LOG,  00000000 00000000.
008880 :  *00304    :,     LOG,  00000000 00000000.
008881 :  *00305    :,     LOG,  00000000 00000000.
008882 :   *00306:,LOG,5017 7777 7777 7777
008883 :             ,     LOG,  00577777 77777730.
008884 :   *00310:,LOG,4054 4417 6652 1041
008885 :             ,     LOG,  00064302 15142270.
008886 :  *00312    :,REAL, 5.23598775598E-01
008887 :             ,     LOG,  00115454 10626710.
008888 :             ,INT,              1
008889 :  *00315    :,REAL, 1.00000000000E+00
008890 :             ,REAL, 0.00000000000E+00
008891 :  *00317    :,REAL, 4.00000000000E+00
008892 :             ,REAL, 0.00000000000E+00
008893 :  *00321    :,REAL, 2.00000000000E+00
008894 :             ,REAL, 0.00000000000E+00
008895 :   *00323:,LOG,4114 0000 0000 0000
008896 :             ,REAL, 0.00000000000E+00
008897 :  *00325    :,REAL, 2.70000000000E+01
008898 :             ,REAL, 0.00000000000E+00
008899 :             ,INT,             13
008900 :  *00330    :,REAL, 1.90000000000E+01
008901 :             ,     LOG,  00000000 00000000.
008902 :             ,INT,              9
008903 :             ,INT,              2
008904 :  *00334    :,REAL, 1.80000000000E+01
008905 :             ,REAL, 0.00000000000E+00
008906 :  *00336    :,REAL, 2.60000000000E+01
008907 :             ,REAL, 0.00000000000E+00
008908 :  *00340    :,REAL, 5.00000000000E-01
008909 :             ,REAL, 0.00000000000E+00
008910 :             ,INT,              3
008911 :  BSS       :     ,BSS,  20B
008912 :       ,END,
008913 :       DSQRT      :,NAME,
008914 :       DABS       :,SUBP,
008915 :       FT*571     :,SUBP,
008916 :       FT*003     :,SUBP,
008917 :       VDSQRT     :,SUBP,
008918 :       D*LO*D     :,SUBP,
008919 :       D*ST*D     :,SUBP,
008920 :       D*SU*D     :,SUBP,
008921 :       D*LN*D     :,SUBP,
008922 :       D*AD*D     :,SUBP,
008923 :       D*MU*D     :,SUBP,
008924 :       D*DI*D     :,SUBP,
008925 :       I*DI*I     :,SUBP,
008926 :       D*PO*I     :,SUBP,
008927 :             14 ,  VTM,   *00001
008928 :                  , UJ,   *00144
008929 :  *00001    :     ,WTC,   *00165
008930 :             14 ,  VTM,
008931 :             13 ,  VJM,  D*LO*D
008932 :                  ,ASN,  71B
008933 :                  ,UZA,   *00141
008934 :                  ,ARX,
008935 :                  ,UZA,   *00136
008936 :                  ,WTC,   *00165
008937 :             14 ,  VTM,
008938 :             13 ,  VJM,  D*LO*D
008939 :             14 ,  VTM,   BSS+ 0000B
008940 :             13 ,  VJM,  D*ST*D
008941 :             14 ,  VTM,   BSS+ 0000B
008942 :             13 ,  VJM,  D*LO*D
008943 :             14 ,  VTM,   *00201
008944 :             13 ,  VJM,  D*SU*D
008945 :                  ,UIA,   *00013
008946 :                  , UJ,   *00014
008947 :  *00013    :     , UJ,   *00116
008948 :                  ,UTC,
008949 :  *00014    :14 ,  VTM,   BSS+ 0000B
008950 :             13 ,  VJM,  D*LN*D
008951 :             14 ,  VTM,   *00203
008952 :             13 ,  VJM,  D*AD*D
008953 :                  ,UIA,   *00017
008954 :                  , UJ,   *00020
008955 :  *00017    :     , UJ,   *00123
008956 :                  ,UTC,
008957 :  *00020    : 7 ,  XTA,  31B
008958 :              7 ,  ATX,  8
008959 :  *00021    :14 ,  VTM,   *00206
008960 :             13 ,  VJM,  D*LO*D
008961 :             14 ,  VTM,   BSS+ 0002B
008962 :             13 ,  VJM,  D*ST*D
008963 :             14 ,  VTM,   *00210
008964 :             13 ,  VJM,  D*LO*D
008965 :             14 ,  VTM,   BSS+ 0002B
008966 :             13 ,  VJM,  D*MU*D
008967 :             14 ,  VTM,   *00155
008968 :             13 ,  VJM,  D*ST*D
008969 :             14 ,  VTM,   *00212
008970 :             13 ,  VJM,  D*LO*D
008971 :             14 ,  VTM,   *00155
008972 :             13 ,  VJM,  D*DI*D
008973 :             14 ,  VTM,   *00155
008974 :             13 ,  VJM,  D*ST*D
008975 :             14 ,  VTM,   *00214
008976 :             13 ,  VJM,  D*LO*D
008977 :             14 ,  VTM,   *00210
008978 :             13 ,  VJM,  D*DI*D
008979 :                  ,WTC,   *00165
008980 :             14 ,  VTM,
008981 :             13 ,  VJM,  D*AD*D
008982 :             14 ,  VTM,   *00157
008983 :             13 ,  VJM,  D*ST*D
008984 :             14 ,  VTM,   *00214
008985 :             13 ,  VJM,  D*LO*D
008986 :             14 ,  VTM,   BSS+ 0002B
008987 :             13 ,  VJM,  D*DI*D
008988 :             14 ,  VTM,   *00161
008989 :             13 ,  VJM,  D*ST*D
008990 :             14 ,  VTM,   *00155
008991 :             13 ,  VJM,  D*LN*D
008992 :             14 ,  VTM,   *00157
008993 :             13 ,  VJM,  D*DI*D
008994 :             14 ,  VTM,   *00161
008995 :             13 ,  VJM,  D*AD*D
008996 :             14 ,  VTM,   BSS+ 0004B
008997 :             13 ,  VJM,  D*ST*D
008998 :  *00045    :14 ,  VTM,   BSS+ 0000B
008999 :             13 ,  VJM,  D*LO*D
009000 :             14 ,  VTM,   BSS+ 0004B
009001 :             13 ,  VJM,  D*DI*D
009002 :             14 ,  VTM,   BSS+ 0004B
009003 :             13 ,  VJM,  D*AD*D
009004 :             14 ,  VTM,   *00216
009005 :             13 ,  VJM,  D*MU*D
009006 :             14 ,  VTM,   BSS+ 0002B
009007 :             13 ,  VJM,  D*ST*D
009008 :             14 ,  VTM,   BSS+ 0002B
009009 :             13 ,  VJM,  D*LO*D
009010 :             14 ,  VTM,   BSS+ 0004B
009011 :             13 ,  VJM,  D*SU*D
009012 :             14 ,  VTM,   *00155
009013 :             13 ,  VJM,  D*ST*D
009014 :             14 ,  VTM,   *00155
009015 :                  ,ITA,  14
009016 :             13 ,  VJM,      DABS
009017 :             14 ,  VTM,   *00166
009018 :             13 ,  VJM,  D*SU*D
009019 :                  ,UIA,   *00061
009020 :                  , UJ,   *00065
009021 :  *00061    :     ,WTC,   *00164
009022 :                  , UJ,   *00061
009023 :                  , UJ,   *00070
009024 :                  ,UTC,
009025 :                  , UJ,   *00073
009026 :                  ,UTC,
009027 :                  , UJ,   *00103
009028 :                  ,UTC,
009029 :  *00065    :14 ,  VTM,   BSS+ 0002B
009030 :             13 ,  VJM,  D*LO*D
009031 :             14 ,  VTM,   BSS+ 0004B
009032 :             13 ,  VJM,  D*ST*D
009033 :                  , UJ,   *00045
009034 :                  ,UTC,
009035 :  *00070    :14 ,  VTM,   BSS+ 0002B
009036 :             13 ,  VJM,  D*LO*D
009037 :             14 ,  VTM,   BSS+ 0006B
009038 :             13 ,  VJM,  D*ST*D
009039 :                  , UJ,   *00150
009040 :                  ,UTC,
009041 :  *00073    : 7 ,  XTA,  7
009042 :             14 ,  VTM,   *00220
009043 :             13 ,  VJM,  I*DI*I
009044 :              7 ,  ATX,  1
009045 :             14 ,  VTM,   *00210
009046 :                  ,ITA,  14
009047 :             14 ,  VTM,   *00155
009048 :             13 ,  VJM,  D*PO*I
009049 :             14 ,  VTM,   BSS+ 0002B
009050 :             13 ,  VJM,  D*MU*D
009051 :             14 ,  VTM,   BSS+ 0006B
009052 :             13 ,  VJM,  D*ST*D
009053 :                  , UJ,   *00150
009054 :                  ,UTC,
009055 :  *00103    : 7 ,  XTA,  7
009056 :             14 ,  VTM,   *00220
009057 :             13 ,  VJM,  I*DI*I
009058 :              7 ,  ATX,  1
009059 :             14 ,  VTM,   *00210
009060 :                  ,ITA,  14
009061 :             14 ,  VTM,   *00155
009062 :             13 ,  VJM,  D*PO*I
009063 :             14 ,  VTM,   BSS+ 0002B
009064 :             13 ,  VJM,  D*MU*D
009065 :             14 ,  VTM,   *00155
009066 :             13 ,  VJM,  D*ST*D
009067 :             14 ,  VTM,   *00203
009068 :             13 ,  VJM,  D*LO*D
009069 :             14 ,  VTM,   *00155
009070 :             13 ,  VJM,  D*DI*D
009071 :             14 ,  VTM,   BSS+ 0006B
009072 :             13 ,  VJM,  D*ST*D
009073 :                  , UJ,   *00150
009074 :                  ,UTC,
009075 :  *00116    : 7 ,  XTA,  45B
009076 :              7 ,  ATX,  8
009077 :             14 ,  VTM,   *00203
009078 :             13 ,  VJM,  D*LO*D
009079 :             14 ,  VTM,   BSS+ 0000B
009080 :             13 ,  VJM,  D*DI*D
009081 :             14 ,  VTM,   BSS+ 0000B
009082 :             13 ,  VJM,  D*ST*D
009083 :                  , UJ,   *00124
009084 :                  ,UTC,
009085 :  *00123    : 7 ,  XTA,  44B
009086 :              7 ,  ATX,  8
009087 :  *00124    :14 ,  VTM,   BSS+ 0000B
009088 :                  ,ITS,  14
009089 :             14 ,  VTM,   *00163
009090 :                  ,ITS,  14
009091 :             13 ,  VJM,  VDSQRT
009092 :             14 ,  VTM,   *00210
009093 :                  ,ITA,  14
009094 :             14 ,  VTM,   *00163
009095 :             13 ,  VJM,  D*PO*I
009096 :             14 ,  VTM,   *00155
009097 :             13 ,  VJM,  D*ST*D
009098 :             14 ,  VTM,   BSS+ 0000B
009099 :             13 ,  VJM,  D*LO*D
009100 :             14 ,  VTM,   *00155
009101 :             13 ,  VJM,  D*DI*D
009102 :             14 ,  VTM,   BSS+ 0000B
009103 :             13 ,  VJM,  D*ST*D
009104 :                  , UJ,   *00021
009105 :                  ,UTC,
009106 :  *00136    :14 ,  VTM,   *00170
009107 :                  ,ITS,  14
009108 :             13 ,  VJM,  FT*571
009109 :             13 ,  VJM,  FT*003
009110 :  *00141    :14 ,  VTM,   *00222
009111 :             13 ,  VJM,  D*LO*D
009112 :             14 ,  VTM,   BSS+ 0006B
009113 :             13 ,  VJM,  D*ST*D
009114 :                  , UJ,   *00150
009115 :                  ,UTC,
009116 :  *00144    : 7 ,  MTJ,  8
009117 :              7 ,  VTM,   *00154
009118 :                  ,NTR,  2
009119 :              7 ,  STX,  9
009120 :                  ,ITS,  13
009121 :                  ,ITS,  8
009122 :                  ,ITS,
009123 :             14 ,   UJ,
009124 :  *00150    :     ,STI,
009125 :                  ,STI,  7
009126 :                  ,STI,  8
009127 :                  ,XTS,
009128 :             14 ,  VTM,   BSS+ 0006B
009129 :             13 ,  VJM,  D*LO*D
009130 :              8 ,   UJ,
009131 :                  ,UTC,
009132 :  *00154    :,     LOG,  00000000 00000000.
009133 :  *00155    :,     LOG,  00000000 00000000.
009134 :             ,     LOG,  00000000 00000000.
009135 :  *00157    :,     LOG,  00000000 00000000.
009136 :             ,     LOG,  00000000 00000000.
009137 :  *00161    :,     LOG,  00000000 00000000.
009138 :             ,     LOG,  00000000 00000000.
009139 :  *00163    :,     LOG,  00000000 00000000.
009140 :  *00164    :,     LOG,  00000000 00000000.
009141 :  *00165    :,     LOG,  00000000 00000000.
009142 :  *00166    :,     LOG,  11132044 67032411.
009143 :             ,     LOG,  00056330 03651564.
009144 :  *00170    :, ISO    ,6H(10X'047'E
009145 :             , ISO    ,6HRROR I
009146 :             , ISO    ,6HN DSQR
009147 :             , ISO    ,6HT'047')
009148 :             , ISO    ,6H(3X,D4
009149 :             , ISO    ,6H0.23,I
009150 :             , ISO    ,6H10)
009151 :             , ISO    ,6H(3X,D4
009152 :             , ISO    ,6H0.23)
009153 :  *00201    :,     LOG,  37177777 77777777.
009154 :             ,     LOG,  00177777 77777776.
009155 :  *00203    :,REAL, 1.00000000000E+00
009156 :             ,REAL, 0.00000000000E+00
009157 :             ,INT,              1
009158 :   *00206:,LOG,4053 2404 7463 1771
009159 :             ,     LOG,  00157144 41054556.
009160 :  *00210    :,REAL, 2.00000000000E+00
009161 :             ,REAL, 0.00000000000E+00
009162 :  *00212    :,REAL, 8.55005001299E+00
009163 :             ,     LOG,  00176560 36604652.
009164 :  *00214    :,REAL, 3.09031552000E+00
009165 :             ,     LOG,  00024262 24145230.
009166 :  *00216    :,REAL, 5.00000000000E-01
009167 :             ,     LOG,  00000000 00000000.
009168 :  *00220    :,INT,              2
009169 :             ,INT,              3
009170 :  *00222    :,     LOG,  00000000 00000000.
009171 :             ,     LOG,  00000000 00000000.
009172 :  BSS       :     ,BSS,  8
009173 :       ,END,
009174 :  DUBLPREC:  ,NAME,
009175 :  B       :  ,EQU,12
009176 :  RL      :  ,EQU,11
009177 :  ACC*    :  ,LC ,1
009178 :  C       :  ,LOG,1
009179 :  D*AD*D  :  ,ENTRY,
009180 :           RL,VJM,PUTEXP
009181 :  SUBTRE  : B,X-A,EXPA-C
009182 :             ,UZA,CHEKEXP
009183 :            B,XTA,ACC-C
009184 :            B,XTS,X-C
009185 :            B,STX,ACC-C
009186 :            B,ATX,X-C
009187 :             ,XTA,ACC*
009188 :            B,XTS,Y-C
009189 :             ,STX,ACC*
009190 :            B,ATX,Y-C
009191 :            B,XTA,EXPA-C
009192 :            B,XTS,EXPX-C
009193 :            B,STX,EXPA-C
009194 :             ,UJ ,SUBTRE
009195 :  CHEKEXP : B,X-A,C80-C
009196 :            B,ATX,DE-C
009197 :             ,U1A,EXIT
009198 :           RL,VJM,REPLEXP
009199 :            B,XTA,ACC-C
009200 :            B,A+X,X-C
009201 :           15,ATX,
009202 :             ,YTA,64
009203 :            ,A+X,ACC*
009204 :            B,A+X,Y-C
009205 :  SETACC* :  ,BSS,
009206 :            ,E+N,64-40
009207 :             ,NTR,2
009208 :           15,A+X,
009209 :             ,NTR,23B
009210 :            B,ATX,ACC-C
009211 :             ,YTA,64
009212 :             ,ATX,ACC*
009213 :  SETEXP  :  ,ASN,64-7
009214 :             ,YTA,
009215 :            B,A-X,C80-C
009216 :            B,A+X,EXPA-C
009217 :            B,ATX,EXPA-C
009218 :            B,A-X,C20000-C
009219 :             ,UZA,OVEFLOW
009220 :            B,A+X,C40000-C
009221 :             ,U1A,MZEROA
009222 :            B,APX,MPEXP1-C
009223 :            B,XTS,ACC-C
009224 :             ,UZA,SZEROA
009225 :            B,AAX,CMANT-C
009226 :           15,AOX,
009227 :            B,XTS,EXPA-C
009228 :             ,ASN,64+7
009229 :            B,AEX,EXPA-C
009230 :            B,APX,MPEXP2-C
009231 :             ,XTS,ACC*
009232 :            B,AAX,CMANT-C
009233 :           15,AOX,
009234 :             ,STX,ACC*
009235 :           B,XTR,R-C
009236 :           13,UJ ,
009237 :  EXIT    : B,XTA,ACC-C
009238 :            B,XTR,R-C
009239 :           13,UJ ,
009240 :  SZEROA  :  ,STX,
009241 :  MZEROA  :  ,XTA,
009242 :            B,ATX,ACC-C
009243 :             ,ATX,ACC*
009244 :             ,UJ ,EXIT
009245 :  PUTEXP  : B,VTM,C
009246 :            B,ATX,ACC-C
009247 :             ,RTE,43B
009248 :           B,AOX,RADD-C
009249 :            B,ATX,R-C
009250 :             ,NTR,3
009251 :            B,XTA,ACC-C
009252 :             ,UZA,ZA
009253 :            B,AUX,MUNPEXP-C
009254 :            B,A-X,C64-C
009255 :             ,XTS,ACC*
009256 :            B,AUX,MUNPEXP*-C
009257 :           15,AEX,
009258 :            B,ATX,EXPA-C
009259 :  PUTEXPX :  ,BSS,
009260 :           14,XTA,
009261 :            B,ATX,X-C
009262 :             ,UZA,ZX
009263 :            B,AUX,MUNPEXP-C
009264 :            B,A-X,C64-C
009265 :           14,XTS,1
009266 :            B,ATX,Y-C
009267 :            B,AUX,MUNPEXP*-C
009268 :           15,AEX,
009269 :            B,ATX,EXPX-C
009270 :           RL,UJ ,
009271 :  ZA      :  ,ATX,ACC*
009272 :            B,XTA,ZROE-C
009273 :            B,ATX,EXPA-C
009274 :             ,UJ ,PUTEXPX
009275 :  ZX      : B,ATX,Y-C
009276 :            B,XTA,ZROE-C
009277 :            B,ATX,EXPX-C
009278 :           RL,UJ ,
009279 :  OVEFLOW :14,VTM,TEXT
009280 :             ,ITS,14
009281 :           14,UTM,4
009282 :             ,ITS,14
009283 :             ,XTS,
009284 :           13,MTJ,11
009285 :             ,CALL,PRINT8
009286 :           11,MTJ,13
009287 :             ,32 ,
009288 :  REPLEXP : B,XTA,ACC-C
009289 :            B,AAX,CMANT-C
009290 :            B,AOX,E80-C
009291 :            B,ATX,ACC-C
009292 :             ,XTA,ACC*
009293 :            B,AAX,CMANT-C
009294 :            B,AOX,E80-C
009295 :             ,ATX,ACC*
009296 :            B,XTA,DE-C
009297 :             ,ASN,64+7
009298 :             ,YTA,
009299 :            B,ATX,DE-C
009300 :            B,XTA,X-C
009301 :            B,AAX,CMANT-C
009302 :            B,AOX,DE-C
009303 :            B,ATX,X-C
009304 :            B,XTA,Y-C
009305 :            B,AAX,CMANT-C
009306 :            B,AOX,DE-C
009307 :            B,ATX,Y-C
009308 :           RL,UJ ,
009309 :  D*MU*D  :  ,ENTRY,
009310 :           RL,VJM,PUTEXP
009311 :            B,A+X,EXPA-C
009312 :            B,ATX,EXPA-C
009313 :            B,XTA,C64-C
009314 :            B,ATX,DE-C
009315 :           RL,VJM,REPLEXP
009316 :            B,XTA,ACC-C
009317 :            B,A*X,X-C
009318 :           15,ATX,
009319 :             ,YTA,64
009320 :            B,XTS,ACC-C
009321 :            B,A*X,Y-C
009322 :            B,XTS,X-C
009323 :             ,A*X,ACC*
009324 :           15,A+X,
009325 :           15,A+X,
009326 :             ,UJ ,SETACC*
009327 :  D*DI*D  :  ,ENTRY,
009328 :           RL,VJM,PUTEXP
009329 :            B,X-A,EXPA-C
009330 :            B,ATX,EXPA-C
009331 :            B,XTA,C64-C
009332 :            B,ATX,DE-C
009333 :           RL,VJM,REPLEXP
009334 :             ,NTR,2
009335 :            B,XTA,ACC-C
009336 :            B,A/X,X-C
009337 :            B,ATX,RD-C
009338 :           15,ATX,
009339 :            B,A*X,X-C
009340 :           15,ATX,
009341 :            ,YTA,64
009342 :            B,STX,RM-C
009343 :            B,X-A,ACC-C
009344 :             ,E+N,64+40
009345 :             ,NTR,3
009346 :            B,A-X,RM-C
009347 :             ,A+X,ACC*
009348 :            B,XTS,Y-C
009349 :            B,A*X,RD-C
009350 :           15,X-A,
009351 :            B,A/X,X-C
009352 :             ,UJ ,SETACC*
009353 :  D*SU*D  :  ,ENTRY,
009354 :           RL,VJM,PUTEXP
009355 :            B,X-A,EXPA-C
009356 :             ,U1A,SPECSUBT
009357 :            B,X-A,C80-C
009358 :            B,ATX,DE-C
009359 :             ,U1A,EXIT
009360 :           RL,VJM,REPLEXP
009361 :  DSUBT   : B,XTA,ACC-C
009362 :            B,A-X,X-C
009363 :           15,ATX,
009364 :             ,YTA,64
009365 :             ,A+X,ACC*
009366 :            B,A-X,Y-C
009367 :             ,UJ ,SETACC*
009368 :  SPECSUBT: B,A+X,C80-C
009369 :            B,ATX,DE-C
009370 :             ,UZA,SKIPZRO
009371 :  LNENTRY :  ,XTA,
009372 :            B,ATX,ACC-C
009373 :             ,ATX,ACC*
009374 :            B,XTA,C80-C
009375 :            B,ATX,DE-C
009376 :  SKIPZRO : B,XTA,EXPX-C
009377 :            B,ATX,EXPA-C
009378 :            B,XTA,X-C
009379 :           B,AAX,CMANT-C
009380 :            B,AOX,E80-C
009381 :            B,ATX,X-C
009382 :            B,XTA,Y-C
009383 :            B,AAX,CMANT-C
009384 :            B,AOX,E80-C
009385 :            B,ATX,Y-C
009386 :            B,XTA,DE-C
009387 :             ,ASN,64+7
009388 :             ,YTA,
009389 :            B,ATX,DE-C
009390 :            B,XTA,ACC-C
009391 :            B,AAX,CMANT-C
009392 :            B,AOX,DE-C
009393 :            B,ATX,ACC-C
009394 :            ,XTA,ACC*
009395 :            B,AAX,CMANT-C
009396 :            B,AOX,DE-C
009397 :             ,ATX,ACC*
009398 :             ,UJ ,DSUBT
009399 :  D*COMP  :  ,ENTRY,
009400 :           14,VTM,X
009401 :           14,ATX,
009402 :             ,XTA,ACC*
009403 :           14,ATX,1
009404 :  D*LN*D  : ,ENTRY,
009405 :            B,VTM,C
009406 :            ,RTE,43B
009407 :            B,AOX,RADD-C
009408 :            B,ATX,R-C
009409 :             ,NTR,3
009410 :           RL,VTM,LNENTRY
009411 :             ,UJ ,PUTEXPX
009412 :  D*LO*D  :  ,ENTRY,
009413 :           14,XTA,1
009414 :             ,ATX,ACC*
009415 :           14,XTA,
009416 :           13,UJ ,
009417 :  D*ST*D  :  ,ENTRY,
009418 :             ,XTS,ACC*
009419 :           14,STX,1
009420 :           14,ATX,
009421 :           13,UJ ,
009422 :  R       :  ,OCT,
009423 :  DE      :  ,OCT,
009424 :  RD      :  ,OCT,
009425 :  RM      :  ,OCT,
009426 :  ACC     :  ,OCT,
009427 :  EXPA    :  ,OCT,
009428 :  EXPX    :  ,OCT,
009429 :  X       :  ,OCT,
009430 :  Y       :  ,OCT,
009431 :  YCAVE   :  ,OCT,
009432 :  CSGN    :  ,OCT,002
009433 :  RADD    :  ,OCT,1
009434 :  ZROE    :  ,OCT,0037 7777 7776 0000
009435 :  TEXT    :  ,ISO,25HDOUBLE PRECISION OVERFLOW
009436 :  MUNPEXP :  ,LOG,177
009437 :  MPEXP1  :  ,LOG,20077
009438 :  MUNPEXP*:  ,BSS,
009439 :  MPEXP2  :  ,LOG,17700
009440 :  C80     :  ,LOG,120
009441 :  CMANT   :  ,LOG,0037 7777 7777 7777
009442 :  E80     :  ,OCT,5
009443 :  C20000  :  ,LOG,20000
009444 :  C40000  :  ,LOG,40000
009445 :  C64     :  ,LOG,100
009446 :            ,END,
009447 :  EDIT***:,NAME,.31.12.80
009448 :  *REGIM//:,LC,1
009449 :  *TIKTAK* :,LC,25.
009450 :  MONCARD* :,LC,25.
009451 :  MONREAD* :,SUBP,.
009452 :  ERROFLG*:,LC,1
009453 :  CREAD:,EQU,CAREAD
009454 :  NOLISFL*:,LC,1
009455 :  ID1:,EQU,IDID.
009456 :  ID2:,EQU,IDID+1.
009457 :  FORMKOZA:,SUBP,
009458 :  RM*:,LC,5
009459 : C      П/П ДЛЯ PAБOTЫ C
009460 : C      ЛEHTAMИ CИMBOЛOB
009461 :  SIMFAIL*:,SUBP,
009462 :  SIMFAIL*:I,BLOCK,DO*LENT,P1,M1,W*P1,
009463 :  ,CONT,R*P1,W*M1,R*M1,P1*W,
009464 :  ,CONT,P1*R,M1*W,M1*R,
009465 :  ,CONT,L1*TO*L2,RSIM,WSIM,
009466 :  ,CONT,L2*TO*L1
009467 : C------YПPABЛEHИE ЗAГPYЗKOЙ
009468 :  CHKBEG*:,SUBP,
009469 :  PRINT8//:,SUBP,
009470 :  TABSIMIS:,SUBP,
009471 :  SIM*:,EQU,52B
009472 :  SIM/:,EQU,57B
009473 :  *MONCRD*:,LC,12
009474 :  PRINT8:,SUBP,
009475 :  READ*:,SUBP,
009476 :  REWORD:,SUBP,
009477 :  WRWORD:,SUBP,
009478 :  PARAMT*:,SUBP,
009479 :  RECARD:,SUBP,
009480 :  OUT*OUT:,LC,1
009481 :  REWORD/:,PC,1024
009482 :  WRWORD/:,PC,1024
009483 :  BUFFER:,PC,1010
009484 :  XREAD/:,LC,2
009485 :  XWRITE/:,LC,2
009486 :  NAME*:,LC,2
009487 :  MIST10*:LC,BLOCK,MIS,CODE,NUMB
009488 :  MONCARD/:,LC,14
009489 :  STARSTA:,LC,1
009490 :  BIGBUF:,PC,811
009491 :  COSYTISO:,SUBP,
009492 :  OCTTDEC:,SUBP,.
009493 :  BASE:,BSS,
009494 :  14,VTM,-12
009495 :  BASE1:14,ITS,13.MOD 1-13
009496 :  14,VLM,BASE1
009497 :  7,BASE ,BASE
009498 : C      HAЧAЛЬHOE BOCCTAHOBЛEHИE
009499 :  Q:,EQU,MYMY-BLYPR.N-1
009500 :  MQ:,EQU,BLYPR-MYMY.-(N-1)
009501 :  14,VTM,MQ
009502 :  ,XTS,
009503 :  G* :14,ATX,BLYPR+Q
009504 :  14,VLM,G*
009505 :  ,XTA,=H
009506 :  ,ATX,NUMBERI
009507 :  ,ATX,NETAL
009508 :  ,ATX,NETAL+1
009509 :  ,XTA,NUMBERE
009510 :  ,ATX,NUMBERI+1
009511 :  ,XTA,20B
009512 :  ,ATX,SJUMP
009513 : C      CTAHДAPTHAЯ PEAKЦИЯ HA ПPEPЫBAHИЯ
009514 :  ,XTA,MISTIKA
009515 :  ,CTX,20B
009516 :  ,XTA,*REGIM//
009517 :  ,ATX,ZAKAZP
009518 : C      AДPEC OБPAБOTKИ NONTEXT CARD
009519 :  14,VTM,NOTXR
009520 :  ,ITA,14
009521 :  ,AOX,*REGIM//
009522 :  ,ATX,*REGIM//
009523 :  14,VTM,3
009524 :  ,ITA,14
009525 :  ,*50,102B.3 ПPE-Я
009526 :  ,ITA,15
009527 :  ,ATX,MAG
009528 :  14,VTM,52B.*
009529 :  ,ITA,14
009530 :  ,ATX,FOR*
009531 :  ,XTA,=1
009532 :  ,UTC,OUT*OUT
009533 :  ,ATX,
009534 :  10,VTM,
009535 :  ,CALL,PARAMT*.ПOДBOД
009536 :  11,VZM,BASI
009537 :  10,VTM,3.IDENT
009538 :  ,CALL,PARAMT*
009539 :  13,VJM,XOXO.PARAM1
009540 :  11,VZM,BASI
009541 :  10,VTM,3.IDENT
009542 :  ,CALL,PARAMT*
009543 :  13,VJM,XOXO.PARAM2
009544 :  BASI:,BSS,
009545 :  13,VJM,EDNAM
009546 : C      ''ЗAKAЗЫ'' ЛEHT SIMFAIL* ДЛЯ PEД-Я ПO/
009547 :  8,VTM,SX1/
009548 :  9,VTM,CARDR
009549 :  10,VTM,12
009550 :  13,VJM,DO*LENT.ДЛЯ KAPTЫ ИЗ READ
009551 :  8,VTM,SXR/
009552 :  9,VTM,SCARD
009553 :  10,VTM,12
009554 :  13,VJM,DO*LENT.ДЛЯ PEЗYЛЬTATA PEДAKЦИИ
009555 :  ,UJ,SLCARDN
009556 : C-----      AHAЛИЗ OЧEPEДHOЙ KAPTЫ ЗAДAHИЯ
009557 :  SLCARD:13,VJM,PRINTCD.ПEЧATЬ ПPEДШECTBYЮЩEЙ
009558 :  SLCARDN:,BSS,.ПPOBEPKA KAPTЫ
009559 : C                          .И EE ЧTEHИE
009560 :  ,XTA,MONCARD*.
009561 :  ,ASN,64+40
009562 :  ,UZA,ECTЬ*BIN.HE TEKCTOBAЯ !
009563 :  ,XTA,MONCARD*.
009564 :  ,AEX,=6H*END F.
009565 :  ,UIA,ECTЬ*ISO.
009566 :  14,VTM,TOTCEE.
009567 :  ,ITS,14.
009568 :  14,UTM,3
009569 :  ,ITS,14.
009570 :  14,VTM,2
009571 :  ,ITS,14.
009572 :  ,CALL,PRINT8//.
009573 :  ,UJ,*EE FIN.HA OБP. *EE
009574 :  ECTЬ*BIN:,XTA,MONCARD*.
009575 :  ,ASN,64+40.
009576 :  ,ATX,MYMY.ПEЧATЬ HE HYЖHA
009577 :  14,VTM,13.
009578 :  ,UIA,MISTAKE.
009579 :  13,VTM,ECTЬ*BIN.
009580 :  ,UJ,MONREAD*.ПPOПYCK BIN
009581 :  ECTЬ*ISO:,BSS,.TEKCTOBAЯ KAPTA
009582 :          ,XTS,ACARD
009583 :  ,CALL,READ*.ЧTEHИE KAPTЫ
009584 :  14,VTM,-13
009585 :  MY:14,XTA,CARD+13.ПEPEПИCЬ B MONCARD/
009586 :  14,ATX,MONCARD/+13
009587 :  14,VLM,MY
009588 : C      BOCCTAH.TABSIMIS(*,/)
009589 :  14,VTM,TABSIMIS
009590 :  14,XTA,SIM*
009591 :  ,AOX,=:1.OГP.
009592 :  14,ATX,SIM*
009593 :  14,XTA,SIM/
009594 :  ,AOX,=:1.OГP.
009595 :  14,ATX,SIM/
009596 :  ,XTA,
009597 :  ,ATX,NNN.K-BO ЦИФP B YПP. CЛOBE
009598 :  8,VTM,XCARD
009599 :  9,VTM,CARD
009600 :  10,VTM,12
009601 :  13,VJM,DO*LENT.ЛEHTA CИMBOЛOB
009602 :  13,VJM,RSIM
009603 : C      AHAЛИЗ YПP.CИMBOЛA
009604 :  ,ITA,14
009605 :  ,AEX,FOR*
009606 :  ,U1A,NOYPR.NOT
009607 :  ,XTA,CARD
009608 :  ,ASN,64-8
009609 :  ,AEX,=6H(((  '000'
009610 :  ,UZA,*3/1
009611 :  ,XTA,BLYPR
009612 :  ,U1A,NOYPR
009613 : C      OПPEДEЛEHИE KAPT CTAPOГO PEДAKTOPA
009614 :  9,VTM,
009615 :  10,VTM,
009616 :  OLD:10,XTA,OLDED
009617 :  ,UZA,OLDN.HE'CTAPAЯ'KAPTA
009618 :  ,AEX,CARD
009619 :  ,UIA,OLDA.HECOBПAДEHИE ПEPBЫX CЛOB
009620 :  10,XTA,OLDED+1
009621 :  ,AEX,CARD+1
009622 :  ,UIA,OLDA.HECOBПAДEHИE BTOPЫX
009623 :  10,XTA,OLDED+2
009624 :  ,AEX,CARD+2
009625 :  ,UZA,OLDB.COBПAДEHИE TPETЬИX
009626 :  OLDA:10,UTM,3
009627 :  9,UTM,1
009628 :  ,UJ,OLD
009629 :  OLDB:,XTA,
009630 :  ,ATX,N2
009631 :  ,XTA,CARD+3
009632 :  ,AEX,=H
009633 :  ,UIA,OLDC
009634 :  ,XTA,CARD+4
009635 :  ,AEX,=H
009636 :  ,UZA,OLDN
009637 : C      OФOPMЛEHИE N1
009638 :  OLDC:,XTA,CARD+3
009639 :  ,ATX,N1
009640 :  ,ATX,NETAL
009641 :  ,XTA,CARD+4
009642 :  ,AAX,=:777774
009643 :  ,AEX,=6H''''   '12'
009644 :  ,ATX,N1+1
009645 :  ,ATX,NETAL+1
009646 :  ,XTA,CARD+5
009647 :  ,AEX,=H
009648 :  ,UIA,OLDD.ECTЬ N2
009649 :  ,XTA,CARD+6
009650 :  ,AEX,=H
009651 :  9,UZA,OLDPER.HET N2
009652 : C      OФOPMЛEHИE N2
009653 :  OLDD:,XTA,CARD+5
009654 :  ,ATX,N2
009655 :  ,XTA,CARD+6
009656 :  ,AAX,=:777774
009657 :  ,AEX,=6H''''   '12'
009658 :  ,ATX,N2+1
009659 :  9,UJ,OLDPER
009660 : C      ПEPEKЛЮЧATEЛЬ ДЛЯ 'CTAPЫX' KAPT(CM.OLDED)
009661 :  OLDPER:,UJ,*I+1.0
009662 :  :,UJ,*I+1.1
009663 :  :,UJ,*D+1.2
009664 :  :,UJ,*T+1.3
009665 :  :,UJ,*T+1.4
009666 : C      AHAЛИЗ YПP.KAPTЫ(HOBЫИ ФOPMAT)
009667 :  OLDN:,BSS,
009668 :  SL0:13,VJM,P1*R.ЧTEHИE CИMBOЛA
009669 :  11,V1M,SLA..HET OГPAHИЧИTEЛЯ
009670 :  ,UTC,TABSIMIS
009671 :  14,XTA,
009672 :  ,AAX,=:7
009673 :  ,AEX,=:1
009674 :  ,U1A,SL0.HE OГPAHИЧИTEЛЬ
009675 :  14,MTJ,13
009676 :  13,UTM,-56B
009677 :  13,VZM,SLB.TOЧKA
009678 :  13,UTM,-1
009679 :  13,VZM,SLC./
009680 :  14,MTJ,13
009681 :  13,UTM,-72B
009682 :  13,VZM,SLA
009683 :  ,UJ,SLMIST.ПЛOXOИ OГPAHИЧИTEЛЬ
009684 :  SLA:6,VTM,TWOT
009685 :  13,VJM,DOKR/.OKOHЧAHИE PEД-Я /
009686 :  ,UJ,SL1
009687 :  SLB:6,VTM,ONET
009688 :  13,VJM,DOKR/.OKOHЧAHИE PEД-Я /
009689 :  ,UJ,SL1
009690 :  SLC:6,VTM,ONE/
009691 : C      AHAЛИЗ YПPABЛЯЮЩИX CЛOB
009692 :  SL1:,BSS,
009693 :  8,VTM,XCARD
009694 :  9,VTM,CARD
009695 :  10,VTM,12
009696 :  13,VJM,DO*LENT.ЛEHTA CИMBOЛOB
009697 :  5,VTM,.N CTPOKИ TAБЛИЦЫ
009698 :  SL1A:13,VJM,P1*R
009699 :  11,V1M,SLMIST.ПЛOXOE YПP.CЛOBO
009700 :  14,MTJ,13
009701 :  13,UTM,-40B
009702 :  13,VZM,SL1A.ПPOБEЛ
009703 :  SL1B:6,UTC,
009704 :  5,XTA,
009705 :  ,UZA,SLMIST.KOHEЦ TAБЛИЦЫ
009706 :  ,ASN,64-7
009707 :  ,YTA,
009708 :  ,ITS,14
009709 :  15,AEX,
009710 :  ,UZA,SL1C.CИMBOЛЫ COBПAЛИ
009711 :  5,UTM,1
009712 :  ,UJ,SL1B
009713 :  SL1C:6,UTC,
009714 :  5,XTA,
009715 :  ,ATX,XXX.AДPEC ПP. OБPAБOTKИ
009716 :  15,ATX,
009717 :  ,AAX,=4000 0000
009718 :  ,STX,PR111.ПPИЗHAK БЛ. N YПP.KAPT
009719 :  ,AAX,=1 0000 0000
009720 :  ,U1A,SL1D.HA AHAЛИЗ ДOП.TAБЛИЦЫ
009721 :  SL1C0:13,VJM,P1*R
009722 :  11,V1M,SL1CF
009723 :  14,MTJ,13
009724 :  13,UTM,-40B
009725 :  13,VZM,SL1C0.ПPOБEЛ
009726 :  ,UTC,TABSIMIS
009727 :  14,XTA,
009728 :  ,AAX,=:7
009729 :  ,U1A,SL1C01.HE БYKBA
009730 :  ,XTA,NNN.ЦИФP EЩE HE БЫЛO
009731 :  ,UZA,SL1C0
009732 :  ,UJ,SLMIST
009733 :  SL1C01:,AEX,=:1
009734 :  ,UZA,SL1CF.OГPAHИЧИTEЛЬ
009735 :  ,XTA,PR111
009736 :  ,UZA,SLMIST.ЦИФPЫ ЗAПPEЩEHЫ
009737 :  ,XTA,NNN
009738 :  ,ASN,64-8
009739 :  ,ITS,14
009740 :  15,AEX,
009741 :  ,ATX,NNN.ЧИCЛO CЛ.HEYПP.KAPT
009742 :  ,UJ,SL1C0
009743 :  SL1CF:,XTA,XXX
009744 :  ,UZA,SLMIST.HEKOPP.YПP.CЛOBO
009745 : C      KOPP. TABSIMIS(*/ БYKBЫ)
009746 :  14,VTM,TABSIMIS
009747 :  14,XTA,SIM*
009748 :  ,AAX,=0777 7777 7777 7777
009749 :  14,ATX,SIM*.*
009750 :  14,XTA,SIM/
009751 :  ,AAX,=0777 7777 7777 7777
009752 :  14,ATX,SIM/./
009753 :  ,WTC,XXX.HA OБPAБOTKY
009754 :  ,UJ,
009755 : C      ПOИCK BTOPOГO YПP.CЛOBA
009756 :  SL1D:6,UTC,
009757 :  5,WTC,
009758 :  6,VTM,.AДPEC ДOП.TAБЛИЦЫ
009759 :  5,VTM,
009760 :  SL1D1:6,UTC,
009761 :  5,XTA,
009762 :  ,AAX,=:7777 7777
009763 :  ,UZA,SL1D2.KOHEЦ TAБЛИЦЫ
009764 :  5,UTM,1
009765 :  ,UJ,SL1D1
009766 :  SL1D2:6,UTC,
009767 :  5,XTA,
009768 :  ,ATX,XXX.AДPEC OБPAБOTKИ
009769 :  ,AAX,=4000 0000
009770 :  ,ATX,PR111.ПPИЗHAK 'N KAPT HEYПP.'
009771 :  SL1D3:5,VTM,
009772 :  SL1D4:13,VJM,P1*R
009773 :  11,V1M,SL1CF..KOHEЦ AHAЛИЗA
009774 :  14,MTJ,13
009775 :  13,UTM,-40B
009776 :  13,VZM,SL1D4.ПPOБEЛ
009777 :  ,UTC,TABSIMIS
009778 :  14,XTA,
009779 :  ,AAX,=:7
009780 :  ,UZA,SL1D5.БYKBA
009781 :  ,UJ,SL1C0+1.HE БYKBA
009782 :  SL1D5:6,UTC,
009783 :  5,XTA,
009784 :  ,AAX,=:7777 7777
009785 :  ,UZA,SL1D3.KOHEЦ TAБЛИЦЫ
009786 :  6,UTC,
009787 :  5,XTA,
009788 :  ,ASN,64-7
009789 :  ,YTA,
009790 :  ,ITS,14
009791 :  15,AEX,
009792 :  ,UZA,SL1C.ECTЬ 2-E CЛOBO
009793 :  5,UTM,1
009794 :  ,UJ,SL1D5
009795 : C------OБPAБOTKA *R
009796 :  *R:,BSS,
009797 :  13,VJM,GYKAZ.YK MБ/MЛ
009798 :  ,XTA,=1
009799 :  ,ATX,YRE.ПP. 'ECTЬ *R'
009800 :  13,VTM,*RE
009801 : C      CПPABKИ ПPEДЫДYЩEГO *R И ДAHHOГO
009802 :  FILF:,BSS,
009803 :  ,XTA,FILE
009804 :  ,ATX,FILE+1.ИMЯ ПPEД.
009805 :  ,XTA,ID1
009806 :  ,ATX,FILE.ИMЯ HACT.
009807 :  ,XTA,ZT
009808 :  ,ATX,ZT+1.'TP/ЗOH' ПPEД.
009809 :  ,XTA,ID2+2
009810 :  ,ATX,ZT.'TP/ЗOH' HACT.
009811 :  13,UJ,
009812 :  *RE:,BSS,
009813 :  13,VJM,DOKTAKE.OKOHЧAHИE TAKE
009814 :  13,VJM,DOKREAD.OK-E ПPEД.*R
009815 :  14,VTM,RRG.YK MБ/MЛ
009816 :  ,ITS,14
009817 :  ,CALL,RBEGIN.ПOДГOTOBKA K ЧTEHИЮ
009818 :  ,XTA,=:1
009819 :  ,ATX,NOENDR.ПP.'HEOKOHЧ.*R'
009820 :  *REO:,BSS,
009821 :  ,XTA,RRG
009822 :  ,ATX,AREAD.YK TEK *R
009823 : C      ЧИCTKA ПPИЗHAKOB
009824 :  ,XTA,
009825 :  ,ATX,YRE
009826 :  ,ATX,YESTAKE
009827 :  ,ATX,YESCARD
009828 :  ,ATX,BLTAKE
009829 :  ,ATX,TOUT
009830 :  ,XTA,=H
009831 :  ,ATX,NETAL.ЭTAЛOHHЫЙ N
009832 :  ,ATX,NETAL+1
009833 :  ,UJ,SLCARD
009834 : C------OБPAБOTKA *RO
009835 :  *RO:,XTA,
009836 :  ,ATX,RRG
009837 :  ,XTA,=1
009838 :  ,ATX,YRE
009839 :  13,VJM,FILF.CПPABKA
009840 :  13,VJM,DOKTAKE.OKOHЧAHИE TAKE
009841 :  13,VTM,*REO
009842 :  ,UJ,DOKREAD.OKOHЧAHИE *R
009843 : C------OБPAБOTKA *W
009844 :  *W:,BSS,
009845 :  13,VJM,GYKAZ.YK MЛ/MБ
009846 :  13,VJM,FILF/.CBOДKA.OKOHЧAHИE:
009847 :  13,VJM,DOKTAKE.TAKE
009848 :  13,VJM,DOKWRITE.*R
009849 :  14,VTM,RRG
009850 :  ,ITS,14
009851 :  ,CALL,WBEGIN.ПOДГOTOBKA ЗAПИCИ
009852 :  ,XTA,RABOTA1
009853 :  13,VJM,ENDLI.OKOHЧAHИE:
009854 :  ,XTA,RABOTA1.LIST
009855 :  13,VJM,ENDPU.PUCH
009856 :  ,XTA,RABOTA1
009857 :  13,VJM,ENDUZ.USER
009858 :  ,XTA,
009859 :  ,ATX,RABOTA1
009860 :  ,ATX,RABOTA2
009861 :  ,XTA,RRG
009862 :  ,ATX,AWRITE.YK TEK. *W
009863 :  ,XTA,=2000
009864 :  ,ATX,AOGR.ДЛИHA MЛ
009865 :  ,UTC,CODE
009866 :  ,XTA,
009867 :  ,UZA,SLCARD.HET 2-ГO ПARAM.
009868 :  10,VTM,5
009869 :  9,VTM,AOGR
009870 :  ,CALL,PARAMT*.ЧTEHИE 2-ГO
009871 :  14,V1M,SLMIST.C OШИБKOЙ
009872 :  ,UZA,SLMIST.=0
009873 :  ,UJ,*NO
009874 : C------OБPAБOTKA *WO
009875 :  *WO:,XTA,
009876 :  ,ATX,RRG
009877 :  ,UJ,*W+1
009878 : C---   CПPABKA TEK. И HACT. *W
009879 :  FILF/:,XTA,FILE/
009880 :  ,ATX,FILE/+1
009881 :  ,XTA,ID1
009882 :  ,ATX,FILE/
009883 :  ,XTA,ZT/
009884 :  ,ATX,ZT/+1
009885 :  ,XTA,ID2+2
009886 :  ,ATX,ZT/
009887 :  13,UJ,
009888 : C------OБPAБOTKA *WC
009889 :  *WC:,BSS,
009890 :  13,VJM,GYKAZ.ЧTEHИE YK
009891 :  13,VJM,FILF/.CBOДKA
009892 : C      CПACAHИE CBOДKИ
009893 :  ,XTA,FILE+1
009894 :  ,XTS,QCAR
009895 :  ,XTS,ZT+1
009896 :  ,XTS,
009897 :  ,XTA,ID1
009898 :  ,ATX,FILE+1
009899 :  ,XTA,ID2+2
009900 :  ,ATX,ZT+1
009901 : C      OKOHЧAHИE:
009902 :  13,VJM,DOKTAKE.TAKE
009903 :  13,VJM,DOKWRITE.*W
009904 :  ,XTA,RABOTA1
009905 :  13,VJM,ENDLI.LIST
009906 :  ,XTA,RABOTA1
009907 :  13,VJM,ENDPU.PUNCH
009908 :  ,XTA,RABOTA1
009909 :  13,VJM,ENDUZ.UZER
009910 :  ,XTA,
009911 :  ,ATX,RABOTA1.ШKAЛЫ
009912 :  ,ATX,RABOTA2.CEPBИCA
009913 : C      CПACAHИE X-K *R
009914 :  ,XTA,YESCARD
009915 :  ,ATX,YESCARD/
009916 :  ,XTA,NOENDR
009917 :  ,ATX,NOENDR/
009918 :  ,XTA,ZAP
009919 :  ,ATX,ZAP/
009920 :  ,XTA,=1
009921 :  ,ATX,YRE
009922 :  ,XTA,AREAD
009923 :  ,ATX,AREAD/
009924 :  ,XTA,=2000
009925 :  ,ATX,AOGR.ДЛИHA MЛ
009926 :  ,UTC,CODE
009927 :  ,XTA,
009928 :  ,UZA,*WC//.HET 2-ГO ПAPAM
009929 :  10,VTM,5
009930 :  9,VTM,AOGR
009931 :  ,CALL,PARAMT*.ЧTEHИE 2-ГO
009932 :  14,V1M,SLMIST.C OШИБKOЙ
009933 :  *WC//:,BSS,
009934 :  14,VTM,-13
009935 :  *WC1:14,XTA,CARDR+13.TEKYЩAЯ
009936 :  14,ATX,CARDR/+13.KAPTA *R
009937 :  14,VLM,*WC1
009938 :  ,XTA,RRG.YK *WC
009939 :  ,ATX,AWRITE
009940 :  ,ATX,AREAD
009941 : C      HOBЫX X-KИ *R
009942 :  ,XTA,
009943 :  ,ATX,YESCARD
009944 :  ,ATX,ZAP
009945 :  ,XTA,=1
009946 :  ,ATX,NOENDR
009947 : C      CПACAHИE X-KИ П/П RECARD
009948 :  14,VTM,XREAD/
009949 :  14,XTA,
009950 :  ,ATX,XREAD//
009951 :  14,XTA,1
009952 :  ,ATX,XREAD//+1
009953 :  12,VTM,XREAD/
009954 :  11,VTM,WRWORD/
009955 :  14,VTM,AREAD
009956 :  ,ITS,14
009957 :  ,UTC,1
009958 :  ,CALL,RBEGIN.ПOДГOTOBKA K ЧTEHИЮ
009959 :  14,VTM,AREAD
009960 :  ,ITS,14
009961 :  ,CALL,WBEGIN.K ЗAПИCИ
009962 : C      БЛOKИPOBKA ЗAПИCИ
009963 :  14,VTM,XWRITE/
009964 :  14,XTA,
009965 :  ,AOX,=1 0000 0000 0000.NO WR
009966 :  14,ATX,
009967 :  ,ATX,BL*R*W
009968 : C      ПEPEПИCЬ C ФИKT.ЗAПИCЬЮ
009969 :  13,VJM,DOKREAD
009970 :  13,VTM,XREAD/
009971 :  14,VTM,XWRITE/
009972 :  13,XTA,1
009973 :  14,AEX,1
009974 :  ,AAX,=77 7777
009975 :  ,UZA,EZON.HE ПPOЧИTAHA ЛИШHЯЯ ЗOHA
009976 :  14,XTA,1
009977 :  ,AOX,=:001
009978 :  ,ATX,RR1
009979 :  ,DRUM,RR1.ПPEДЫДYЩAЯ ЗOHA
009980 :  EZON:,BSS,
009981 : C      BOCCTAHOBЛEHИE
009982 :  *WC2:,XTA,YESCARD/
009983 :  ,ATX,YESCARD
009984 :  ,XTA,ZAP/
009985 :  ,ATX,ZAP
009986 :  ,XTA,NOENDR/
009987 :  ,ATX,NOENDR
009988 :  ,XTA,AREAD/
009989 :  ,ATX,AREAD
009990 :  14,VTM,XWRITE/
009991 :  14,XTA,
009992 :  ,AOX,=1 0000 0000 0000.NO WR
009993 :  ,AEX,=1 0000 0000 0000.NO WR
009994 :  14,ATX,
009995 :  14,VTM,-13
009996 :  *WC3:14,XTA,CARDR/+13
009997 :  14,ATX,CARDR+13
009998 :  14,VLM,*WC3
009999 :  14,VTM,XREAD/
010000 :  ,XTA,XREAD//
010001 :  14,ATX,
010002 :  ,XTA,XREAD//+1
010003 :  14,ATX,1
010004 :  ,XTA,
010005 :  ,STX,BL*R*W
010006 :  ,STX,ZT+1
010007 :  ,STX,QCAR
010008 :  ,ATX,FILE+1
010009 :  ,UJ,*NO
010010 : C      ПEPEXOД K OБЫЧHOMY ФOPMATY(R,W)
010011 :  COSY0:14,VTM,XREAD/
010012 :  14,XTA,
010013 :  ,AOX,=2000 0000 0000
010014 :  ,AEX,=2000 0000 0000
010015 :  14,ATX,
010016 :  14,VTM,XWRITE/
010017 :  14,XTA,
010018 :  ,AOX,=2000 0000 0000
010019 :  ,AEX,=2000 0000 0000
010020 :  14,ATX,
010021 :  13,UJ,
010022 : C-----      OБPAБOTKA *EE
010023 :  *EE:,BSS,
010024 :  ,XTA,CARD
010025 :  ,AEX,=6H*EXECU
010026 :  ,UZA,SLMIST
010027 :  13,VJM,DOKTAKE.OK-E TAKE
010028 :  ,XTA,=1
010029 :  ,ATX,YRE
010030 :  13,VJM,FILF.CПPABKA *R
010031 :  13,VJM,DOKREAD.*R
010032 :  13,VJM,FILF/.CПPABKA *W
010033 : C      OKOHЧAHИE:
010034 :  13,VJM,DOKWRITE.*W
010035 :  13,VJM,PRINTCD
010036 :  *EEFIN:,BSS,
010037 :  13,VJM,PRINT8//+1
010038 :  ,XTA,RABOTA1
010039 :  13,VJM,ENDLI.LIST
010040 :  ,XTA,RABOTA1
010041 :  13,VJM,ENDPU.PUNCH
010042 :  ,XTA,RABOTA1
010043 :  13,VJM,ENDUZ.UZER
010044 :  ,XTA,SJUMP
010045 :  ,CTX,20B
010046 :  ,XTA,
010047 :  ,*50,102B.1 ПPE-E
010048 :  ,XTA,ZAKAZP
010049 :  ,ATX,*REGIM//
010050 : C      BOЗBPAT ИЗ PEДAKTOPA
010051 :  ,STI,
010052 :  14,VTM,13
010053 :  14,STI,
010054 :  14,UTM,-1
010055 :  14,VIM,*-1
010056 :  ,UTC,PROGON
010057 :  ,XTA,
010058 :  13,UZA,.NONMISTAKE
010059 :  ,UTC,ZAKAZ
010060 :  ,XTA,
010061 :  ,UZA,20B.SYS. JUMP
010062 :  13,UJ,
010063 : C__________  OБPAБOTKA KAPTЫ *A...
010064 :  *A:13,VJM,DOKTAKE.
010065 :  ,XTA,=1.
010066 :  ,ATX,YRE.
010067 :  13,VJM,FILF.
010068 :  13,VJM,DOKREAD.
010069 :  13,VJM,FILF/.
010070 :  13,VTM,SLCARD.
010071 :  ,UJ,DOKWRITE.
010072 :  SLMIST:14,VTM,.HEKOPP. KAPTA
010073 : C-----      ПEЧATЬ ДИAГHOCTИKИ(I14=N)
010074 :  MISTAKE:,BSS,
010075 :  14,MTJ,1
010076 :  7,BASE,BASE
010077 :  MISTAKE1:,BSS,.BXOД I1=N
010078 :  ,WTC,MAG
010079 :  15,VTM,
010080 :  ,XTA,MYMY
010081 :  ,U1A,MYMYPR
010082 :  13,VJM,PRI.ПEЧATЬ KAPTЫ
010083 :  MYMYPR:,BSS,
010084 :  ,XTA,RABOTA1
010085 :  13,VJM,ENDPU.OK-E PUNCH
010086 :  1,MTJ,14
010087 :  12,VTM,
010088 :  14,UTM,1
010089 :  ER1:12,MTJ,11
010090 :  ER2:12,XTA,ERTEXT
010091 :  12,UTM,1
010092 :  ,U1A,ER2
010093 :  14,UTM,-1
010094 :  14,VZM,ERPRI
010095 :  ,UJ,ER1
010096 :  ERPRI:11,UTC,
010097 :  14,VTM,ERTEXT
010098 :  ,ITS,14
010099 :  12,UTC,
010100 :  14,VTM,ERTEXT-2
010101 :  ,ITS,14
010102 :  14,VTM,2
010103 :  ,ITS,14
010104 :  ,XTS,ZAKAZ
010105 :  ,AEX,=1
010106 :  ,UZA,ER3
010107 :  ,ATX,ERROFLG*
010108 :  ER3:,STX,
010109 :  ,CALL,PRINT8//
010110 :  13,VJM,MISTIKA2.OK-E *R,*W
010111 :  ,XTA,ENDE1
010112 :  ,ATX,PROGON
010113 :  ,AEX,XXX
010114 :  ,U1A,SLCARDN.NOT *EE
010115 :  ,XTA,CARD
010116 :  ,AEX,=6H*EXECU
010117 :  ,U1A,*EEFIN.NOT *EXECUTE
010118 :  ,UJ,SLCARDN.ДOKP-KA ДO *EE
010119 :  *EQRW:1,VTM,1.*R=*W
010120 :  ,UJ,MISTAKE1
010121 :  ERRBL:14,VTM,2.ПYCTOЙ OБPAЗ
010122 :  ,UJ,MISTAKE
010123 :  ERRBLT:14,VTM,3.БЫЛA *DF/
010124 :  ,UJ,MISTAKE
010125 :  NORA:1,VTM,4.HET PAБOTЫ
010126 :  ,UJ,MISTAKE1
010127 :  ERRNO/:14,VTM,5.HET OБPAЗA
010128 :  ,UJ,MISTAKE
010129 :  NAM11WC:14,VTM,6.HET KAPTЫ
010130 :  ,UJ,MISTAKE
010131 :  NAMC1:14,VTM,7.HET *R
010132 :  ,UJ,MISTAKE
010133 :  PO/MI:14,VTM,8.73 CИMBOЛA
010134 :  ,UJ,MISTAKE
010135 :  MISTIKA:,BSS,
010136 :  14,VTM,10.БЫЛИ OШИБKИ
010137 :  ,UJ,MISTAKE
010138 :  MB100:1,VTM,11.>2-X MБ
010139 :  ,UJ,MISTAKE1
010140 :  NOTXR:14,VTM,12.NONTEXT R
010141 :  ,UJ,MISTAKE
010142 :  PARMIS:14,VTM,15.OШИБ. B ПAPAMETPAX
010143 :  ,XTA,=1.
010144 :  ,ATX,MYMY.
010145 :  ,UJ,MISTAKE.KAPTA HAПEЧATAHA
010146 : C     OKOHЧAHИE *R,*W
010147 :  MISTIKA2:,BSS,
010148 :  ,ITS,13
010149 :  ,XTS,YRE
010150 :  ,UIA,MISTYPE
010151 :  13,VJM,FILF.CПPABKA *R
010152 :  MISTYPE:,BSS,
010153 :  ,XTA,
010154 :  ,ATX,NOWRIT
010155 :  ,ATX,NOENDR
010156 :  13,VJM,DOKREAD.OK-E *R
010157 :  13,VJM,FILF/.CПPABKA *W
010158 :  ,XTA,=:4
010159 :  ,ATX,MYMY
010160 :  ,AEX,CREAD
010161 :  ,UZA,BA
010162 :  13,VJM,DOKWRITE.OK-E *W
010163 :  BA:,BSS,
010164 :  ,XTA,
010165 :  ,STX,MYMY
010166 :  ,STI,13
010167 :  13,UJ,
010168 : C-----      ПOЛYЧEHИE YK MБ/MЛ(*R,*W,WC)
010169 :  GYKAZ:,ITA,13
010170 :  ,XTS,
010171 :  11,VZM,GYK.ECTЬ YK
010172 :  GYK0:,XTA,=03 0000.YK=1
010173 :  ,ATX,RR1
010174 :  ,XTA,DOKYT.ДЛЯ CBOДKИ
010175 :  ,ATX,ID2+2
010176 :  ,XTA,=6H1
010177 :  ,ATX,ID1
010178 :  ,UJ,GYKRET
010179 :  GYK:,BSS,
010180 :  9,VTM,4
010181 :  13,VJM,PARAMT.YKAЗATEЛЬ MБ/MЛ
010182 :  ,XTA,RR1
010183 :  ,ARX,=10 0000
010184 :  ,ASN,64+18
010185 :  ,U1A,SLMIST.7-OE HAПP.
010186 :  ,XTA,RR1
010187 :  ,AAX,=00 6000
010188 :  ,U1A,SLMIST.N30H>1711B.
010189 :  ,XTA,RR1
010190 :  ,AEX,=03 0000
010191 :  ,UZA,GYK0.YK=1
010192 :  ,XTA,RR1
010193 :  ,AEX,=05 0000
010194 :  ,UIA,GYKA.YK HE =2
010195 :  ,XTA,DOKYT.ДЛЯ CBOДKИ
010196 :  ,ATX,ID2+2
010197 :  ,XTA,=6H2
010198 :  ,ATX,ID1
010199 :  ,UJ,GYKRET
010200 :  GYKA:,XTA,RR1.ДЛЯ CBOДKИ
010201 :  ,ASN,64-30
010202 :  ,ATX,ID2+1
010203 :  ,UTC,*MONCRD*
010204 :  ,XTA,
010205 :  ,ATX,ID1
010206 :  GUKI:,BSS,
010207 :  ,XTA,DOKYZ
010208 :  ,ATX,ID2+2
010209 :  ,XTA,ID2+1
010210 :  ,AAX,=:4
010211 :  ,UIA,GYKRET.4-6 HAПP.
010212 :  ,XTA,ID2+1
010213 :  ,AAX,=:7
010214 :  ,AEX,=:3
010215 :  ,UZA,GYKRET.-3 HAПP.
010216 :  ,XTA,DOKYT
010217 :  ,ATX,ID2+2
010218 :  ,XTA,ID2+1
010219 :  ,AAX,=:00740
010220 :  ,UIA,SLMIST.NTP.>37B
010221 :  GYKRET:,XTA,RR1
010222 :  ,STX,RRG.RRG=YK
010223 :  ,ATI,13
010224 :  13,UJ,
010225 :  PARAMT:,BSS,.ЧTEHИE YK MЛ/MБ,ИДEHTИФ-POB
010226 :  ,ITA,13.I9-N ПP. PARAMT*
010227 :  ,XTS,MONCARD/
010228 :  ,AOX,=6H'377''0''0''0''0''0'
010229 :  ,AEX,=6H'325''0''0''0''0''0'
010230 :  ,ATX,MONCARD/
010231 :  10,VTM,
010232 :  ,CALL,PARAMT* .ПOДBOД K ПAPAMETPY
010233 :  9,MTJ,10
010234 :  9,VTM,
010235 :  ,CALL,PARAMT*.ЧTEHИE ПAPAMETPA
010236 :  14,V1M,SLMIST
010237 :  ,STX,RR1
010238 :  ,ATI,13
010239 :  13,UJ,
010240 : C---   AHAЛИЗ ПAPAMETPA *EDIT
010241 :  XOXO:,BSS,
010242 :  13,UTC,
010243 :  14,V1M,
010244 :  14,VTM,*MONCRD*
010245 :  14,XTA,
010246 :  ,ASN,64+40
010247 :  ,ATI,14
010248 :  14,UTM,-103B
010249 :  14,V1M,XA0.NON C
010250 :  ,XTA,=:1
010251 :  XA*:,BSS,
010252 :  ,ATX,ZAKAZ
010253 :  13,UJ,
010254 :  XA0:14,UTM,-2
010255 :  14,V1M,XA.NON E
010256 :  ,XTA,=1
010257 :  ,UJ,XA*
010258 :  XA:14,UTM,-7
010259 :  14,V1M,XA1
010260 :  ,XTA,=:6
010261 :  ,UJ,XO
010262 :  XA1:14,UTM,-3
010263 :  14,V1M,XA2.NON O
010264 :  ,XTA,=:4
010265 :  ,UJ,XO
010266 :  XA2:14,UTM,-5
010267 :  14,V1M,PARMIS.NON T
010268 :  ,XTA,
010269 :  XO:,XTS,*REGIM//
010270 :  ,ASN,64-3
010271 :  ,ASN,64+3
010272 :  15,AEX,
010273 :  ,ATX,*REGIM//
010274 :  13,UJ,
010275 : C-----     ЧTEHИE OЧEPEДHOЙ KAPTЫ *R
010276 : C      (ACARD-1-15 P.SYMM,)
010277 : C      (REZ:SYMM=0-*READ OLD)
010278 :  NAMCALL1:,BSS,
010279 :  ,ATX,RR1
010280 :  ,XTA,AREAD
010281 :  ,UZA,NAMC1.HET *R
010282 :  ,ITA,13
010283 :  ,XTS,RR1
010284 :  ,CALL,RECARD.ЧTEHИE KAPTЫ
010285 :  ,ATI,13
010286 :  ,XTA,QCAR
010287 :  ,ARX,=1
010288 :  ,ATX,QCAR.QCAR=QCAR+1
010289 :  ,WTC,RR1
010290 :  ,XTA,
010291 :  ,AEX,=6H*READ
010292 :  ,U1A,NC1RET1.NOT *READ
010293 :  ,WTC,RR1
010294 :  ,XTA,1
010295 :  ,AEX,=6HOLD
010296 :  ,ASN,64+24
010297 :  ,U1A,NC1RET1.NOT OLD
010298 :  ,XTA,QCAR
010299 :  ,ARX,=7777777
010300 :  ,AAX,=7777777
010301 :  ,ATX,QCAR.QCAR=QCAR-1
010302 :  ,XTA,NOENDR
010303 :  ,UZA,NC1RET2.*ЗAKOHЧEH
010304 :  ,UJ,NAM11WC
010305 :  NC1RET1:,XTA,=1
010306 :  13,UJ,
010307 :  NC1RET2:,XTA,
010308 :  13,UJ,
010309 : C-----      OБPAБOTKA KAPTЫ PEЗYЛЬTATA
010310 : C      (CT. И HECT. CEPBИC)
010311 : C      ACARD-1-15 P.SYMM.
010312 :  NAMCALL2:,BSS,
010313 :  ,ATX,RR1
010314 :  ,ITA,13
010315 :  ,XTS,PROGON
010316 :  ,U1A,NLL2
010317 :  ,XTA,NOWRIT
010318 :  ,UIA,NCA.ECTЬ *U
010319 :  NLL2:,BSS,
010320 :  ,XTA,RABOTA1
010321 :  ,AAX,=7777 7777 7777 7774
010322 :  ,AOX,AWRITE
010323 :  ,UZA,NORA.HET PAБOTЫ
010324 : C      OБPAЩEHИE K CT. CEPBИCY
010325 :  ,XTA,RR1
010326 :  13,VJM,EDIT///
010327 :  ,ATI,13
010328 :  ,XTA,
010329 :  ,ATX,CAREAD
010330 :  ,UJ,COSY0.CHЯTИE COSY
010331 :  NCA:,XTA,47B.CПAC. ЛИCTA
010332 :  ,ATX,ID1.OБMEHA
010333 :  ,XTA,RM*+4
010334 :  ,ATX,47B
010335 :  ,WTC,RR1
010336 :  14,VTM,
010337 :  14,XTA,13.ИCПP.XBOCTA KAPTЫ
010338 :  ,AAX,=6H'377''377'''''''''
010339 :  ,AOX,=6H''''
010340 :  14,ATX,13
010341 : C      OБPAЩEHИE K HECT.CEPBИCY
010342 :  ,XTA,RR1
010343 :  14,VTM,EDIT///
010344 :  ,ITS,14
010345 :  ,XTS,NOWRIT
010346 :  ,CALL,LOADANGO
010347 :  ,ATI,13
010348 :  ,XTA,
010349 :  ,ATX,CAREAD
010350 :  ,XTA,ID1
010351 :  ,ATX,47B
010352 :  ,UJ,COSY0.CHЯTИE COSY
010353 : C--  --      CTAHДAPTHЫЙ CEPBИC
010354 :  EDIT///:,BSS,
010355 :  ,UTC,RR1
010356 :  ,ATX,.1-15-ACARD
010357 :  ,ITA,13
010358 :  ,ITS,6
010359 :  ,ITS,7
010360 :  14,VTM,XWRITE/.X-KA WRCARD
010361 :  14,XTS,
010362 :  ,ITS,
010363 :  7,VTM,BASE
010364 :  ,XTA,PROGON
010365 :  ,U1A,NAMJ
010366 :  14,XTA,
010367 :  ,AAX,=2000 0000 0000
010368 :  ,UIA,NWR/.COSY
010369 :  ,WTC,RR1
010370 :  14,VTM,
010371 :  14,XTA,13.BOCCT.'XBOCTA'
010372 :  ,AAX,=6H'377''377'''''''''
010373 :  ,AOX,=6H''''   '12'
010374 :  14,ATX,13
010375 :  NWR/:,BSS,
010376 :  ,XTA,CAREAD
010377 :  ,AEX,=:4
010378 :  ,UIA,NWR//.
010379 :  13,VJM,USEREND .
010380 :  ,UZA,NAMF.*READ OLD
010381 :  NWR//:,BSS,.
010382 :  ,XTA,RABOTA1
010383 :  ,AAX,=3
010384 :  ,UZA,NAMCAB.HET *N,*NI
010385 :  ,AEX,=1
010386 :  ,UZA,NAMCA10.*N
010387 :  ,XTA,CAREAD
010388 :  ,U1A,NAMCAB.KAPTA ИЗ *R
010389 :  NAMCA10:,BSS,
010390 :  ,XTA,RR1
010391 :  ,ATI,14
010392 :  ,XTA,NUMBERI.N=N(I)
010393 :  14,ATX,12
010394 :  ,XTA,NUMBERI+1
010395 :  14,ATX,13
010396 :  NAMCA1:8,VTM,XID
010397 :  9,VTM,NUMBERI
010398 :  10,VTM,2
010399 :  13,VJM,DO*LENT.ЛEHTA ДЛЯ N(I)
010400 :  ,WTC,STEP
010401 :  6,VTM,-7
010402 :  ,UJ,NAMCA2*
010403 :  NAMCA2:13,VJM,P1.+7
010404 :  NAMCA2*: 6,VLM,NAMCA2
010405 :  NAMCA3:13,VJM,RSIM.CИMBOЛ
010406 :  ,UTC,TABSIMIS
010407 :  14,XTA,
010408 :  ,AAX,=:7
010409 :  ,UZA,NAMCA4.БYKBA
010410 :  ,AEX,=:1
010411 :  ,UZA,NAMCA4.OГPAHИЧИTEЛЬ
010412 :  NAMCA3A:,ITA,14
010413 :  ,ARX,=1
010414 :  ,ATI,14
010415 :  ,AEX,=72.10
010416 :  ,UZA,NAMCA5
010417 :  13,VJM,WSIM
010418 :  ,UJ,NAMCAB
010419 :  NAMCA4:14,VTM,60B.ЗAMEHA HA 0
010420 :  ,UJ,NAMCA3A
010421 :  NAMCA5:14,VTM,60B
010422 :  13,VJM,WSIM.ЗAПИCЬ 0
010423 :  13,VJM,M1.-1
010424 :  11,VZM,NAMCA3
010425 :  NAMCAB:,BSS,
010426 :  ,XTA,RABOTA1
010427 :  ,AAX,=4
010428 :  ,UZA,NAMB.HET *LL
010429 :  13,VTM,NAMB
010430 :  ,UJ,PRIREZ.ПEЧATЬ KAPTЫ
010431 :  NAMB:,BSS,
010432 :  ,XTA,RABOTA1
010433 :  ,AAX,=10
010434 :  ,UZA,NAMC.HET *HL
010435 :  ,XTA,NAHL
010436 :  ,UIA,NAMY.ПOИCK END
010437 :  14,VTM,XREAD/
010438 :  14,XTA,1
010439 :  ,ASN,64+18
010440 :  ,ARX,AREAD
010441 :  ,ASN,64-36
010442 :  ,AUX,=6H'7''7''7''7'''''
010443 :  ,AOX,=6H0000
010444 :  ,ATX,NAMESUB+2.N ЗOHЫ
010445 :  13,VJM,COSYK.ПEPEXOД K OБЫЧHOMY ФOPMATY
010446 :  ,XTS,RR1
010447 :  ,CALL,CHKBEG*
010448 :  14,VIM,YF1.HAЙДEHO ИMЯ
010449 :  ,WTC,RR1
010450 :  ,XTA,
010451 :  ,ASN,64-8
010452 :  ,YTA,
010453 :  ,AEX,=52
010454 :  ,UIA,NAMC.KAPTA БEЗ *
010455 :  YF:13,VTM,NAMC
010456 :  ,UJ,PRIREZ
010457 :  YF1:,XTA,=1
010458 :  ,ATX,NAHL.ПP. ПOИCK END
010459 :  14,VTM,NAME*
010460 :  14,XTA,
010461 :  ,ATX,NAMESUB
010462 :  14,XTA,1
010463 :  ,AAX,=:7777 7777
010464 :  ,ATX,NAMESUB+1
010465 :  14,VTM,NAMESUB
010466 :  ,ITS,14
010467 :  14,UTM,2
010468 :  ,ITS,14
010469 :  14,VTM,1
010470 :  ,ITS,14
010471 :  13,VTM,YF
010472 :  ,UJ,PRINT8//.'ИMЯ--N ЗOHЫ
010473 :  NAMY:,XTS,RR1
010474 :  ,CALL,CHKEND*
010475 :  14,VZM,NAMC.HE END
010476 :  ,XTA,
010477 :  ,ATX,NAHL.'ПOИCK NAME'
010478 :  13,VTM,YF
010479 :  ,UJ,COSYK.OЧ. ПP.'COSY'
010480 :  NAMC:,XTA,RABOTA1
010481 :  ,AAX,=20
010482 :  ,UZA,NAMD.HE *LP
010483 :  ,WTC,RR1
010484 :  14,VTM,
010485 :  ,ITS,14
010486 :  ,CALL,COLPUNC*.PUNCH(*LP)
010487 :  NAMD:,BSS,
010488 :  ,XTA,RABOTA1
010489 :  ,AAX,=40
010490 :  ,UZA,NAMF.HE *CP
010491 :  ,WTC,RR1
010492 :  14,VTM,
010493 :  ,ITS,14
010494 :  13,VJM,FORMKOZA+1.PUNCH(*CP)
010495 :  NAMF:,BSS,
010496 :  ,XTA,AWRITE
010497 :  ,UZA,NAMJ.HET *W
010498 :  ,XTA,BL*R*W
010499 :  ,U1A,NAM2WC+1
010500 :  ,XTA,CAREAD
010501 :  ,AEX,AWRITE
010502 :  NAM2WC:,UZA,*EQRW.*W=*R
010503 :  ,XTA,
010504 :  ,XTA,CAREAD
010505 :  ,AEX,=:4.
010506 : C      =ПPИ *READ OLD ИЛИ *END FILE
010507 :  ,U1A,NAW
010508 :  13,VJM,USEREND.
010509 :  ,UIA,NAW.
010510 :  ,UTC,*TIKTAK*+24.
010511 :  ,XTA,.
010512 :  ,UIA,NAMJ.
010513 :  ,XTA,QCAR/
010514 :  ,UZA,NAMJ.QCAR/=0
010515 :  NAW:,BSS,
010516 :  ,XTS,RR1
010517 :  ,CALL,WRCARD
010518 :  ,XTA,CAREAD
010519 :  ,AEX,=:4
010520 :   ,UIA,NAW//.
010521 :  13,VJM,USEREND.
010522 :  ,UZA,NAMJ
010523 :  NAW//:,BSS,.
010524 :  14,VTM,XWRITE/
010525 :  14,XTA,1
010526 :  ,ASN,64+18
010527 :  ,ARX,=1
010528 :  ,AEX,AOGR
010529 :  ,AAX,=7777
010530 :  ,UZA,MB100*.>OГP.
010531 :  ,XTA,AWRITE
010532 :  ,ARX,=50 0000
010533 :  ,ASN,64+18
010534 :  ,U1A,NAMJJ.HE MБ
010535 :  14,XTA,1
010536 :  ,ASN,64+18
010537 :  ,AEX,=77
010538 :  ,AAX,=7777
010539 :  ,UZA,MB100*.>2-X MБ
010540 :  NAMJJ:,BSS,
010541 :  ,XTA,QCAR/
010542 :  ,ARX,=1
010543 :  ,ATX,QCAR/.QCAR/=QCAR/+1
010544 :  NAMJ:,BSS,
010545 : C      BOЗBPAT ИЗ CT.CEPBИCA
010546 :  ,STI,
010547 :  14,VTM,XWRITE/.CБPOC ПP.'COSY'
010548 :  ,AAX,=2000 0000 0000
010549 :  14,XTS,
010550 :  ,AOX,=2000 0000 0000
010551 :  ,AEX,=2000 0000 0000
010552 :  15,AOX,
010553 :  14,STX,
010554 :  ,STI,7
010555 :  ,STI,6
010556 :  ,STI,13
010557 :  13,UJ,
010558 :  USEREND:,XTA,RR1.
010559 :  ,ATI,14.A KAPTЫ
010560 :  14,XTA,.
010561 :  ,AEX,READ OLD
010562 :  ,UIA,USEN.HE *READ
010563 :  14,XTA,1.
010564 :  ,AEX,READ OLD+1
010565 :  13,UJ,.
010566 :  USEN:,BSS,.
010567 :  14,XTA,.
010568 :  ,AEX,END FILE
010569 :  13,UIA,.HE *END F
010570 :  14,XTA,1
010571 :  ,AEX,END FILE+1
010572 :  13,UJ,.
010573 :  MB100*:14,XTA,
010574 :  ,AAX,=1740
010575 :  ,AEX,=1740
010576 :  ,U1A,NAMJJ
010577 :  ,XTA,QCAR/
010578 :  ,ARX,=1
010579 :  ,ATX,QCAR/
010580 :  ,UJ,MB100
010581 : C---   ПEPEBOD KAPTЫ B OБ.ФOPMAT
010582 :  COSYK:14,VTM,XWRITE/
010583 :  14,XTA,
010584 :  ,AAX,=2000 0000 0000.ПP.'COSY'
010585 :  13,UZA,
010586 :  14,AEX,
010587 :  14,ATX,
010588 :  ,XTS,RR1
010589 :  ,UJ,COSYTISO
010590 : C------OKOHЧAHИE TAKE(ДOKPYTKA)
010591 :  DOKTAKE:,BSS,
010592 :  ,ITA,13
010593 :  ,ITS,
010594 :  ,XTA,NOENDTA
010595 :  ,UZA,DOKTARET.HET HEOK. TAKE
010596 :  ,XTA,
010597 :  ,ATX,ZAP
010598 :  ,ATX,PRBEF
010599 :  ,XTA,XNOENDTA
010600 :  13,VJM,POISK
010601 :  DOKTARET:,STI,
010602 :  ,ATI,13
010603 :  13,UJ,
010604 : C------OKOHЧAHИE *R(ДOKPYTKA)
010605 :  DOKREAD:,ITA,13
010606 :  ,XTS,
010607 :  ,XTA,PROGON
010608 :  ,U1A,DOKRET
010609 :  ,XTA,NOENDR
010610 :  ,UZA,DOKRET.HET HEOK. *R
010611 :  ,XTA,
010612 :  ,ATX,NOENDR
010613 :  ,XTA,YESCARD
010614 :  ,U1A,DOKRE1.KAPTA B MOЗY
010615 :  DOKRCOS:,BSS,
010616 :  13,VJM,DOCOSY.PEW.'COSY'
010617 :  14,VTM,CARDR
010618 :  ,ITA,14
010619 :  13,VJM,NAMCALL1.ЧTEHИE KAPTЫ
010620 :  ,UZA,DOKRET.READ OLD
010621 :  DOKRE1:,XTA,AREAD
010622 :  ,ATX,CAREAD
010623 :  14,VTM,CARDR
010624 :  ,ITA,14
010625 :  13,VTM,DOKRCOS
010626 :  ,UJ,NAMCALL2.ЗAПИCЬ KAPTЫ
010627 :  DOKRET:,BSS,
010628 :  ,XTA,NOLISFL*.
010629 :  ,UIA,DOKRET1.*NO LIST
010630 :  ,XTA,PFLAG.
010631 :  ,UZA,DOKRET1.HE БЫЛO *P,*PP
010632 :  ,ATI,14.
010633 :  14,UTC,.
010634 :  13,VJM,PRINT8//.
010635 :  DOKRET1:,XTA,.
010636 :  ,ATX,PFLAG.
010637 :  11,VTM,FILE
010638 :  14,VTM,XREAD/
010639 :  14,XTA,1
010640 :  ,ASN,64+18
010641 :  ,AAX,=7777
010642 :  ,ARX,=1
010643 :  ,ATX,QZT.K-BO ЗOH/TP.
010644 :  ,XTA,
010645 :  ,ATX,AREAD
010646 :  ,ATX,YRE
010647 :  13,VJM,COSY0.PEЖ.'HE COSY'
010648 :  14,VTM,DOKYR
010649 :  13,VJM,MYKA.ПEЧ.'CЧИTAHO...
010650 :  ,STX,
010651 :  ,ATI,13
010652 :  13,UJ,
010653 : C-----      OKOHЧAHИE *W(OФOPMЛEHИE
010654 : C      KOHЦA ФAЙЛA)
010655 :  DOKWRITE:,BSS,
010656 :  ,XTA,PROGON
010657 :  13,U1A,
010658 :  ,ITA,13
010659 :  ,XTS,NOWRIT
010660 :  ,AOX,AWRITE
010661 :  ,UZA,DOKW.HET *W ИЛИ *U
010662 :  ,XTA,=:4
010663 :  ,ATX,CREAD
010664 :  14,VTM,READOLD.*READ OLD
010665 :  ,ITA,14
010666 :  13,VJM,NAMCALL2
010667 :  ,XTA,=:4
010668 :  ,ATX,CREAD
010669 :  14,VTM,
010670 :  13,VJM,DOCOSY/.COSY
010671 :  14,VTM,ENDFILE
010672 :  ,ITS,14
010673 :  13,VJM,EDIT///
010674 :  ,XTA,QCAR/
010675 :  ,UZA,DOKWRIT.
010676 :  ,CALL,WRIEND
010677 :  DOKWRIT:,BSS,.
010678 :  13,VJM,COSY0
010679 :  ,XTA,
010680 :  ,ATX,CAREAD
010681 :  ,ATX,AWRITE
010682 :  11,VTM,FILE/
010683 :  14,VTM,XWRITE/
010684 :  14,XTA,1
010685 :  ,ASN,64+18
010686 :  ,AAX,=7777
010687 :  ,ATX,QZT/.K-BO ЗOH TP.
010688 :  14,VTM,DOKYW
010689 :  13,VJM,MYKA.ПЧ.'ЗAПИCAHO...
010690 :  DOKW:,BSS,
010691 :  ,STI,
010692 :  ,ATI,13
010693 :  13,UJ,
010694 : C-----      ПEЧATЬ CПPABOK 'CЧИTAHO...
010695 : C      'ЗAПИCAHO...
010696 : C      B I11 DOKYT ИЛИ DOKYW
010697 :  MYKA:,BSS,
010698 :  ,XTA,NOLISFL*
010699 :  13,UIA,.NO LIST
010700 :  11,XTA,QCAR-FILE
010701 :  13,UZA,
010702 :  ,ITA,13
010703 :  11,XTS,1
010704 :  14,ATX,1.FILE
010705 :  11,XTA,5
010706 :  14,ATX,8+1.ZONA-TRACT
010707 :  11,XTA,QZT-FILE
010708 :  ,ASN,64-36
010709 :  ,AUX,=6H'7''7''7''7'''''
010710 :  ,AOX,=6H0000''''
010711 :  14,ATX,7+1.QZT
010712 :  11,XTA,QCAR-FILE
010713 :  ,XTS,
010714 :  11,ATX,QZT-FILE
010715 :  11,STX,QCAR-FILE
010716 :  14,MTJ,11
010717 :  ,CALL,OCTTDEC.ПEPEBOД B 10
010718 :  ,AAX,=6H'17''17''17''17''17''17'
010719 :  15,ATX,
010720 :  ,ANX,=77777
010721 :  ,ASN,64+3
010722 :  ,ASN,64-3
010723 :  ,ATI,9
010724 :  ,XTA,=6H000000
010725 :  9,ASN,64
010726 :  15,AOX,
010727 :  ,AOX,=H
010728 :  11,MTJ,14
010729 :  14,ATX,4+1.QCAR
010730 :  ,ITS,14
010731 :  14,UTM,9
010732 :  ,ITS,14
010733 :  14,VTM,14
010734 :  ,ITS,14
010735 :  ,CALL,PRINT8//
010736 :  ,STX,
010737 :  ,ATI,13
010738 :  13,UJ,
010739 : C      ПEЧATЬ KAPTЫ:
010740 : C      YПP:PRAB+2=0,I12=0
010741 :  PRINTCD:,BSS,
010742 :  ,XTA,NOLISFL*
010743 :  13,U1A,.NO LIST
010744 :  PRI:,BSS,
010745 : C      KOПИЯ OПEPATOPH.ЧACTИ
010746 :  14,VTM,-11
010747 :  PRIC:14,XTA,CARD+11
010748 :  14,ATX,PRAB+14
010749 :  14,VLM,PRIC
010750 : C      BЫT-E HOMEPA BПEPEД
010751 :  ,XTA,CARD+12
010752 :  ,ATX,PRAB
010753 :  ,XTA,CARD+13
010754 :  ,AAX,=6H'377''377''377'''''''
010755 :  ,ATX,PRAB+1
010756 :  11,VTM,CARD
010757 :  ,XTA,LLPRI
010758 :  ,UZA,PRIC/.HE PEЗ.
010759 :  14,VTM,-11
010760 :  ,WTC,RR1
010761 :  11,VTM,.AДPEC RE3.
010762 : C      KOПИЯ OПEPATOPH. ЧACTИ
010763 :  PRICA:11,UTC,
010764 :  14,XTA,11
010765 :  14,ATX,PRAB+14
010766 :  14,VLM,PRICA
010767 : C      BЫHOC HOMEPA BПEPEД
010768 :  11,XTA,12
010769 :  ,ATX,PRAB
010770 :  11,XTA,13
010771 :  ,AAX,=6H'377''377''377'''''''
010772 :  ,ATX,PRAB+1
010773 :  ,XTA,
010774 :  ,ATX,LLPRI.ЧИCTKA
010775 :  PRIC/:,BSS,
010776 :  ,XTA,*REGIM//
010777 :  ,AAX,=:7
010778 :  ,AEX,=:6
010779 :  ,UZA,PRIC//.БEЗ БИЛИCTA
010780 :  ,XTA,LTPR1.
010781 :  ,UIA,PRICLT.
010782 :  ,ITA,13
010783 :  14,VTM,PRAB
010784 :  ,ITS,14
010785 :  14,VTM,PRAB+11
010786 :  ,ITS,14
010787 :  ,XTS,
010788 :  ,CALL,PRINT8//.1-Я CTPOKA
010789 :  ,ATI,13
010790 :  14,VTM,-2
010791 :  PRIC1:14,XTA,PRAB+14
010792 :  ,AEX,=H
010793 :  ,U1A,PRIC2.ECTЬ 'XBOCT'
010794 :  14,VLM,PRIC1
010795 :  13,UJ,
010796 :  PRIC2:14,VTM,PRAB+12
010797 :  ,ITS,14
010798 :  14,VTM,PRAB+14
010799 :  ,ITS,14
010800 :  14,VTM,45
010801 :  ,ITS,14
010802 :  ,UJ,PRINT8//.ПEЧATЬ XBOCTA
010803 :  PRIC//:,ITS,11
010804 :  11,UTM,13
010805 :  ,ITS,11
010806 :  ,ITS,
010807 :  ,UJ,PRINT8//
010808 :  PRICLT:14,VTM,PRAB+3.
010809 :  14,XTA,10.
010810 :  ,ASN,64+24.
010811 :  ,ASN,64-24.
010812 :  ,AOX,=6H'0''0''0'
010813 :  14,ATX,10.ЧИCTKA 64-66 CИMBOЛOB
010814 :  ,ITS,14.
010815 :  14,UTM,10.
010816 :  ,ITS,14.
010817 :  ,XTS,.
010818 :  ,ATX,LTPR1.
010819 :  ,UJ,PRINT8//.
010820 : C      ПEЧATЬ HEYПPABЛЯЮЩEЙ KAPTЫ:
010821 : C      PRAB+2=2ПPOБEЛA,I12=2
010822 :  PRINOYP:,BSS,
010823 :  ,XTA,YAR
010824 :  13,UIA,.ECTЬ *Y
010825 : C      ПEЧATЬ KAPTЫ-PEЗYЛЬTATA:
010826 :  ,UJ,PRINTCD
010827 :  PRIREZ:,BSS,
010828 :  ,XTA,NOLISFL*
010829 :  13,U1A,
010830 :  ,XTA,PROGON
010831 :  13,UIA,.БЫЛИ OШИБKИ
010832 :  ,XTA,LTPR.
010833 :  ,ATX,LTPR1.
010834 :  ,XTA,=1
010835 :  ,ATX,LLPRI
010836 :  ,UJ,PRINTCD
010837 : C------OБPAБOTKA ПAPAMETPOB KAPTЫ:
010838 : C       *YC:N1,N2
010839 :  PARA:,BSS,
010840 :  ,ITA,1.K
010841 :  ,ITS,2.K'
010842 :  ,ITS,13
010843 :  ,ITS,
010844 :  ,ATX,N2.N2=0
010845 :  11,V1M,SLMIST.HET :
010846 :  PAR:,ATX,ZAP.
010847 :  8,VTM,XID
010848 :  9,VTM,IDID
010849 :  10,VTM,12
010850 :  13,VJM,DO*LENT.ЛEHTA CИMBOЛOB
010851 :  1,VTM,.K=0
010852 :  PAR2:2,VTM,.K'=0
010853 :  PAR3:8,VTM,XCARD
010854 :  13,VJM,P1*R.ЧTEHИE CИMBOЛA
010855 :  11,V1M,PARFIN.73-Й CИMBOЛ
010856 :  14,MTJ,13
010857 :  13,UTM,-54B
010858 :  13,V1M,PAR4.HE ,
010859 :  ,XTA,ZAP
010860 :  ,U1A,SLMIST.ПOCЛE N2
010861 :  1,VZM,SLMIST.HET N1
010862 :  13,VJM,MADENUM.OБPAБOTKA N1
010863 :  ,XTA,=1
010864 :  ,UJ,PAR
010865 :  PAR4:8,VTM,XID
010866 :  13,VJM,W*P1.ЗAПИCЬ CИMBOЛA
010867 :  14,UTM,-40B
010868 :  14,V1M,PAR5.HE ПPOБEЛ
010869 :  2,UTM,1.K'=K'+1
010870 :  ,UJ,PAR3
010871 :  PAR5:2,J+M,1.K=K+K'
010872 :  1,UTM,1.K=K+1
010873 :  ,UJ,PAR2
010874 :  PARFIN:1,VZM,SLMIST.HET N2
010875 :  13,VJM,MADENUM.OБPAБOTKA N2
010876 :  ,STI,
010877 :  ,STI,13
010878 :  ,STI,2
010879 :  ,ATI,1
010880 :  13,UJ,
010881 : C---   OБPAБOTKA N1 ИЛИ N2
010882 :  MADENUM:,BSS,.
010883 :  ,ITA,13
010884 :  ,XTS,
010885 :  ,ATX,RR2
010886 :  1,MTJ,2
010887 :  2,UTM,-1
010888 :  ,ITA,2
010889 :  ,ASN,64+3
010890 :  ,U1A,SLMIST.>8 CИMBOЛOB
010891 :  2,UTM,-7
010892 :  2,V1M,MADEN.HE=8
010893 : C      8 CИMBOЛOB B PEЗYЛЬTAT
010894 :  ,XTA,ID1
010895 :  ,ATX,RR1
010896 :  ,XTA,ID2
010897 :  ,AAX,=:777774
010898 :  ,ATX,RR2
010899 :  ,UJ,MADEN2/
010900 : C      ПEPEHOC CTAPШИX ИЗ ЭTAЛOHA
010901 :  MADEN:1,MTJ,2
010902 :  2,UTM,-8
010903 :  2,UTM,1.-(8-K-1)
010904 :  8,VTM,XID
010905 :  9,VTM,NETAL
010906 :  10,VTM,2
010907 :  13,VJM,DO*LENT.ЛEHTA ЭTAЛOHA
010908 :  8,VTM,XID1
010909 :  9,VTM,RR1
010910 :  10,VTM,2
010911 :  13,VJM,DO*LENT.ЛEHTA ДЛЯ N
010912 :  MADEN1:8,VTM,XID.ПEPEHOC CTAPШИX
010913 :  13,VJM,R*P1
010914 :  8,VTM,XID1
010915 :  13,VJM,W*P1
010916 :  2,VLM,MADEN1.8-K
010917 :  8,VTM,XID
010918 :  9,VTM,IDID.
010919 :  10,VTM,2
010920 :  13,VJM,DO*LENT
010921 :  MADEN2:8,VTM,XID.ПEPEHOC MЛAДШИX
010922 :  13,VJM,R*P1
010923 :  8,VTM,XID1
010924 :  13,VJM,W*P1
010925 :  1,UTM,-1
010926 :  1,V1M,MADEN2
010927 :  MADEN2/:,BSS,
010928 :  ,XTA,ZAP
010929 :  ,U1A,MADEN3.HE N1
010930 : C      ДOOФOPMЛEHИE N1
010931 :  ,XTA,RR1
010932 :  ,ATX,N1
010933 :  ,ATX,NETAL
010934 :  ,XTA,RR2
010935 :  ,AEX,=6H''''   '12'
010936 :  ,ATX,N1+1
010937 :  ,ATX,NETAL+1
010938 :  ,UJ,MADEN4
010939 : C      ДOOФOPMЛEHИE N2
010940 :  MADEN3:,XTA,RR1
010941 :  ,ATX,N2
010942 :  ,XTA,RR2
010943 :  ,AEX,=6H''''   '12'
010944 :  ,ATX,N2+1
010945 :  MADEN4:,STI,
010946 :  ,ATI,13
010947 :  13,UJ,
010948 : C------OБPAБOTKA HEYПPABЛЯЮЩEЙ KAPTЫ
010949 :  NOYPR:,BSS,
010950 :  ,XTA,CARD
010951 :  ,AEX,=6H )))
010952 :  ,ASN,64-8
010953 :  ,U1A,NOYY
010954 :  ,XTA,CARD
010955 :  ,ASN,64+40
010956 :  ,AEX,FOR*
010957 :  ,UIA,NOYY.HE *)))
010958 :  ,XTA,BLYPR
010959 :  ,UZA,SLMIST.HET БЛ-KИ YПPKAPT
010960 :  ,AAX,=:1
010961 :  ,UZA,SLMIST.HE БЫЛO *(((
010962 : C      YЧET YPOBHЯ*(((-*)))
010963 :  ,XTA,YROV
010964 :  ,ARX,=77777
010965 :  ,AAX,=77777
010966 :  ,ATX,YROV.YROV=YROV-1
010967 :  ,XTA,YROV
010968 :  ,UIA,NOYPR1.HE 0
010969 :  ,ATX,BLYPR.CHЯTИE БЛ-KИ
010970 :  ,UJ,SLCARD
010971 :  NOYY:,BSS,
010972 :  ,XTA,WWW
010973 :  ,UIA,NOYPR/.ECTЬ PEД-E /
010974 :  ,XTA,BLYPR
010975 :  ,UZA,NOYPR1.HET БЛ-KИ YПP.
010976 :  ,AAX,=:1
010977 :  ,UIA,NOYPR1.БЫЛA *(((
010978 : C      KOPPEKЦИЯ ЧИCЛA HEYПP.
010979 :  ,XTA,BLYPR
010980 :  ,ATI,13
010981 :  13,UTM,-1
010982 :  ,ITA,13
010983 :  ,ATX,BLYPR.Q=Q-1
010984 :  NOYPR1:,BSS,.KAPTY HA CEPBИC
010985 :  ,XTA,PROGON
010986 :  ,U1A,SLCARD
010987 :  13,VTM,CARD
010988 :  ,ITA,13
010989 :  13,VJM,NAMCALL2
010990 :  13,VTM,SLCARDN
010991 :  ,UJ,PRINOYP
010992 :  NOYPR/:,BSS,
010993 :  ,XTA,PROGON
010994 :  ,U1A,SLCARD
010995 :  13,VJM,RSIM
010996 :  14,UTM,-40B
010997 :  14,VZM,*L/.ПEPBЫЙ-ПPOБEЛ
010998 :  14,VTM,9
010999 :  ,UJ,MISTAKE
011000 : C------ ПOИCK KAPTЫ ИЗ ФAЙЛA ЧTEHИЯ
011001 : C      1-15 P.SYMM-XAPAKTEPИCTИKA:
011002 : C      48-46 I,30-16 X2,15-1 X1
011003 : C      ПO HOMEPY:I=0,X1=AДPEC N
011004 : C      ПO KOЛ-BY:I=1,X1=Q
011005 : C      ПO OБPAЗY:I=2,X1=ACARD,X2-X-KA ЭT-HA
011006 : C      ПO ИMEHИ :I=3,X1=ANAME
011007 : C      ПO END  :I=4
011008 : C      ----ZAP=0-ЗAПИCЬ PAЗPEШEHA
011009 : C      ----PRBEF HE 0-BEFPRE
011010 : C      ZAP=0-ЗAПИCЬ PAЗPEШEHA
011011 : C      PRBEF NEQ 0-BEFORE
011012 :  POISK:,BSS,
011013 :  ,ATX,RRY
011014 :  ,XTA,PROGON
011015 :  13,U1A,
011016 :  ,ITA,13
011017 :  ,XTS,YESCARD
011018 :  ,U1A,POI1.ECTЬ KAPTA
011019 :  POI:,BSS,
011020 :  13,VJM,COSY0.HE COSY
011021 :  ,XTA,RRY
011022 :  ,AEX,=:4.POISK END TN
011023 :  ,UZA,POIMY
011024 :  ,AEX,=:7.POISK NAME
011025 :  ,AAX,=:7
011026 :  ,U1A,POICONT
011027 :  ,XTA,NAEM
011028 :  ,UZA,POICONT.ИЩETCЯ NAME
011029 :  ,XTA,TOUT
011030 :  ,AOX,BLTAKE
011031 :  ,UIA,POIMY.БЫЛA *TO ИЛИ *BT
011032 :  14,VTM,XREAD/
011033 :  13,VTM,POICONT
011034 : C      COSY B ЛЮБOM CЛYЧAE
011035 :  ,UJ,DOCOSY/
011036 : C      COSY B HEПPOTИBOP. CЛ-X
011037 :  POIMY:13,VJM,DOCOSY
011038 :  POICONT:14,VTM,CARDR
011039 :  ,ITA,14
011040 :  13,VJM,NAMCALL1.ЧTEHИE KAPTЫ
011041 :  POI1:,XTA,RRY
011042 :  13,VJM,YZNAL
011043 :  ,UZA,POI10.KAPTY YЗHAЛИ
011044 :  ,XTA,RRY
011045 :  ,AEX,=:4
011046 :  ,UZA,POI2.ПOИCK END
011047 :  ,XTA,NOENDTA
011048 :  ,UZA,POI2.HET HEЗAK.TAKE
011049 :  ,XTA,XNOENDTA.X-KA TAKE
011050 :  13,VJM,YZNAL
011051 :  ,U1A,POI2.HE END TAKE
011052 :  ,ATX,NOENDTA
011053 :  ,XTA,TOUT
011054 :  ,UZA,NAM11WC
011055 :  POI2:,XTA,ZAP
011056 :  ,U1A,POI.БЛ.ЗAПИCИ
011057 :   :,XTA,AREAD.'KAPTA ИЗ *R'
011058 :  ,ATX,CREAD
011059 :  14,VTM,CARDR
011060 :  ,ITA,14
011061 :  13,VTM,POI
011062 :  ,UJ,NAMCALL2.CEPBИC
011063 :  POI10:,XTA,PRBEF
011064 :  ,UZA,POI11.HE BEFORE
011065 :  ,ATX,YESCARD.ПP.'ECTЬ KAPTA
011066 :  ,UJ,POIFIN.HA BOЗBPAT
011067 :  POI11:,ATX,YESCARD.ПP.'HET KAPTЫ'
011068 :  ,XTA,NOENDTA
011069 :  ,UZA,POI12.HET HEЗAK.TAKE
011070 :  ,XTA,RRY
011071 :  ,AEX,=:4
011072 :  ,UZA,POI11/.ПOИCK END
011073 :  ,XTA,XNOENDTA.X-KA TAKE
011074 :  13,VJM,YZNAL
011075 :  POI11/:,BSS,
011076 :  ,ATX,NOENDTA
011077 :  ,ATX,NOENDR
011078 :  ,U1A,POI12.HE END
011079 :  ,XTA,BLTAKE
011080 :  ,AOX,TOUT
011081 :  ,ATX,NOENDR
011082 :  ,XTA,ZAP
011083 :  ,U1A,POI14.БЛ-KA ЗAП
011084 :  ,XTA,AREAD
011085 :    ,ATX,CREAD.'KAPTA ИЗ *R'
011086 :  14,VTM,CARDR
011087 :  ,ITA,14
011088 :  13,VJM,NAMCALL2.CEPBИC
011089 :  POI14:,XTA,TOUT
011090 :  ,U1A,POIFIN.ECTЬ *TO
011091 :  ,XTA,RABOTA2.OKOHЧAHИE:
011092 :  13,VJM,ENDLI.LIST
011093 :  ,XTA,RABOTA2
011094 :  13,VJM,ENDPU.PUNCH
011095 :  ,XTA,RABOTA2
011096 :  13,VJM,ENDUZ.USER
011097 :  ,XTA,RABOTA1
011098 :  ,AEX,RABOTA2
011099 :  ,ATX,RABOTA1
011100 :  ,XTA,
011101 :  ,ATX,RABOTA2
011102 :  ,UJ,POIFIN
011103 : C---   ЧИCTKA OTД.ПP-KOB B ШKAЛAX
011104 : C      PAБOT(I-12-MACKA)
011105 :  RBT:,BSS,
011106 :  ,ITA,12
011107 :  ,AAX,RABOTA1
011108 :  ,AEX,RABOTA1
011109 :  ,ATX,RABOTA1
011110 :  ,ITA,12
011111 :  ,AAX,RABOTA2
011112 :  ,AEX,RABOTA2
011113 :  ,ATX,RABOTA2
011114 :  13,UJ,
011115 :  POI12:,XTA,ZAP
011116 :  ,U1A,POIFIN.HET ЗAПИCИ
011117 :  ,XTA,BLARECRE
011118 :  ,U1A,*+2
011119 :  ,XTA,AREAD
011120 :  ,ATX,CREAD
011121 :  14,VTM,CARDR
011122 :  ,ITA,14
011123 :  13,VJM,NAMCALL2.CEPBИC
011124 :  POIFIN:,STX,.BOЗBPAT
011125 :  ,ATI,13
011126 :  13,UJ,
011127 : C----  CPABHEHИE OЧEPEДHOЙ KAPTЫ
011128 : C      C ИCKOMOЙ
011129 : C      SYMM-X-KA(KAK Y POISK)
011130 : C      REZ:SYMM=0-COBПAДEHИE
011131 :  YZNAL:,BSS,
011132 :  ,ATX,RR1.X-KA
011133 :  ,ITA,13
011134 :  ,XTS,RR1
011135 :  ,AAX,=:7
011136 :  ,ASN,64-3
011137 :  ,YTA,
011138 :  ,ATI,14
011139 :  14,UJ,YZNPER.HA ПEPEKЛ.
011140 :  YZNPER:,UJ,YZN.ПO HOMEPY
011141 :  :,UJ,YZN*Q.ПO Q
011142 :  :,UJ,YZN*OB.ПO OБPAЗY
011143 :  :,UJ,YZN*NA.NAME
011144 :  :,UJ,YZN*EN.END
011145 :  YZN*Q:,BSS,
011146 :  ,XTA,RR1
011147 :  ,AAX,=77777
011148 :  ,UZA,YZNF0.Q=0
011149 :  ,ARX,=77777
011150 :  ,AAX,=77777
011151 :  ,AOX,=:1
011152 :  ,ATX,RRY.Q=Q-1
011153 :  ,UJ,YZNF1
011154 :  YZN*OB:,BSS,
011155 :  8,VTM,XID
011156 :  ,WTC,RR1
011157 :  9,VTM,
011158 :  10,VTM,12
011159 :  13,VJM,DO*LENT.ЛEHTA KAPTЫ
011160 :  ,ITA,8
011161 :  ,XTS,RR1
011162 :  ,AAX,=77777 00000
011163 :  15,AEX,.X-KA ДЛЯ YZOB
011164 :  13,VJM,YZOB.CPABHEHИE
011165 :  ,UZA,YZNF0.COBПAДEHИE
011166 :  ,UJ,YZNF1
011167 :  YZN*NA:,BSS,
011168 :  ,XTA,NAEM
011169 :  ,UIA,YNE.ИЩETCЯ END
011170 :  14,VTM,CARDR
011171 :  ,ITS,14
011172 :  ,CALL,CHKBEG*.OПP-E ИMEHИ
011173 :  14,VZM,YZNF1.HET ИMEHИ
011174 :  ,WTC,RR1
011175 :  14,VTM,
011176 :  14,XTA,
011177 :  13,VTM,NAME*
011178 :  13,AEX,
011179 :  ,UIA,YNA.HECOBПAД ИMEH
011180 :  14,XTA,1
011181 :  13,AEX,1
011182 :  ,AAX,=:777774
011183 :  ,UZA,YZNF0.COBПAД ИMEH
011184 :  YNA:,ATX,NAEM
011185 :  ,UJ,YZNF1
011186 :  YNE:14,VTM,CARDR
011187 :  ,ITS,14
011188 :  ,CALL,CHKEND*
011189 :  14,VZM,YZNF1.HE END
011190 :  ,XTA,
011191 :  ,UJ,YNA.END
011192 :  YZN*EN:,BSS,
011193 :  14,VTM,CARDR
011194 :  ,ITS,14
011195 :  ,CALL,CHKEND*.OПP-E END
011196 :  14,VZM,YZNF1.HE END
011197 :  ,XTA,
011198 :  ,ATX,NAEM
011199 :  ,UJ,YZNF0.END
011200 :  YZN:,XTA,RR1
011201 :  ,ATI,14
011202 :  14,XTA,
011203 :  ,AEX,CARDR+12
011204 :  ,U1A,YZNF1.HECOBП.
011205 :  14,XTA,1
011206 :  ,AEX,CARDR+13
011207 :  ,U1A,YZNF1.HECOBП
011208 :  YZNF0:,BSS,.YЗHAЛИ
011209 :  ,STX,
011210 :  ,ATI,13
011211 :  ,XTA,
011212 :  13,UJ,
011213 :  YZNF1:,STX,.HE YЗHAЛИ
011214 :  ,ATI,13
011215 :  ,XTA,=1
011216 :  13,UJ,
011217 : C---   ПEPEXOД K COSY(*R,*W)
011218 :  DOCOSY:,BSS,
011219 :  ,XTA,RABOTA1
011220 :  14,VTM,XREAD/
011221 :  ,AAX,=7777 7777 7777 7765
011222 :  ,U1A,DOUS
011223 :  DOCOSY/:,BSS,
011224 :  14,XTA,
011225 :  ,AOX,=2000 0000 0000
011226 :  14,ATX,
011227 :  14,VTM,XWRITE/
011228 :  14,XTA,
011229 :  ,AOX,=2000 0000 0000
011230 :  14,ATX,
011231 :  13,UJ,
011232 :  DOUS:,AEX,=100
011233 :  13,U1A,
011234 :  14,XTA,
011235 :  ,AAX,=4000 0000 0000
011236 :  13,UZA,
011237 :  ,UJ,DOCOSY/
011238 : C---   KOHTPOЛЬ-KOPP. ПOPЯДKA
011239 : C      YПP.KAPT
011240 :  PROVE:,BSS,
011241 :  ,XTA,TOUT
011242 :  13,UIA,.БЫЛA *TO
011243 :  ,XTA,YESTAKE
011244 :  13,UZA,.HE БЫЛO TAKE
011245 :  ,XTA,NOENDTA
011246 :  ,UZA,NAM11WC
011247 :  13,UJ,
011248 : C---   CPABHEHИE KAPTЫ C OБPAЗOM
011249 : C      SYMM=30-16-X-KA OБPAЗA,
011250 : C           15-1-X-KA KAPTЫ
011251 : C      REZ:SUMM=0,Q72=0 COBПAДEHИE
011252 : C          YO5-K-BO CИMB.B OБPAЗE
011253 :  YZOB:,BSS,
011254 :  ,ATX,YO1.X-KA
011255 :  ,ITA,13
011256 :  ,ITS,8
011257 :  ,ITS,2
011258 :  ,ITS,1
011259 :  ,XTS,YO1
011260 : C      CПACAHИE X-K ЛEHT
011261 :  ,ATI,8
011262 :  9,VTM,SX1
011263 :  13,VJM,L1*TO*L2
011264 :  ,XTA,YO1
011265 :  ,ASN,64+15
011266 :  ,ATI,8
011267 :  9,VTM,SX2
011268 :  13,VJM,L1*TO*L2
011269 :  1,VTM,
011270 :  2,VTM,
011271 :  ,XTA,
011272 :  ,ATX,YO5
011273 :  ,XTA,=1
011274 :  ,ATX,YO2.ПP.'HECOBП.'
011275 :  YZO:8,VTM,SX2
011276 :  13,VJM,RSIM
011277 :  ,ITA,14
011278 :  ,ATX,YO3.S2
011279 :  14,UTM,-40B
011280 :  14,VIM,YZO1*.S2 NOT BLANK
011281 :  YZOA:13,VJM,P1
011282 :  11,VZM,YZO
011283 :  YZOA*:,BSS,
011284 :  1,VZM,ERRBL.HET OБPAЗA
011285 :  ,XTA,
011286 :  ,ATX,YO2.ПP. 'COBП-E'
011287 :  ,UJ,YZOFIN
011288 :  YZO1*:14,UTM,-17B
011289 :  14,V1M,YZO1
011290 :  ,XTA,RR1*
011291 :  ,U1A,YZOA*
011292 :  YZO1:8,VTM,SX1
011293 :  2,VZM,YZO2
011294 :  :13,VJM,P1
011295 :  11,VIM,YZOFI.HECOBП-E
011296 :  YZO2:1,VTM,1
011297 :  2,VTM,1
011298 :  13,VJM,RSIM
011299 :  ,XTA,YO5.KOЛИЧECTBO
011300 :  ,ARX,=1.CИMBOЛOB
011301 :  ,ATX,YO5.B OБPAЗE
011302 :  ,ITA,14
011303 :  ,ATX,YO4.S1
011304 :  14,UTM,-40B
011305 :  14,VIM,YZO3.S1 NOT BLANK
011306 :  ,UJ,YZO1+1
011307 :  YZO3:,XTA,YO3
011308 :  ,AEX,YO4
011309 :  ,UIA,YZOFI.S1 NEQ S2
011310 :  8,VTM,SX2
011311 :  ,UJ,YZOA
011312 :  YZOFIN:11,VTM,
011313 :  YZOFI:,ITA,11
011314 :  ,STX,Q72
011315 :  ,STI,1
011316 :  ,STI,2
011317 :  ,STI,8
011318 :  ,ATI,13
011319 :  ,XTA,YO2
011320 :  13,UJ,
011321 : C---   ФOPMИPOBAHИE XAPAKTEPИCTИKИ POISK
011322 : C      ДЛЯ ПOИCKA OБPAЗA(RR1-REZ)
011323 :  LENT0:,BSS,
011324 :  ,ITA,13
011325 :  ,XTS,
011326 :  8,VTM,XCARD
011327 :  13,VJM,P1
011328 :  11,VIM,SLMIST.HET OБPAЗA
011329 :  ,ITA,8
011330 :  ,ASN,64-15
011331 :  13,VTM,CARDR
011332 :  ,ITS,13
011333 :  15,AEX,
011334 :  ,AEX,=:2
011335 :  ,STX,RR1
011336 :  ,ATI,13
011337 :  13,UJ,.BLANK
011338 : C---   ФOPMИPOBAHИE XAPAKTEPИCTИKИ
011339 : C      ДЛЯ ПOИCKA ПO Q(RR1-REZ)
011340 :  YKAZQ:,BSS,
011341 :  ,ITA,13
011342 :  ,XTS,
011343 :  8,VTM,XCARD
011344 :  ,ATX,FNNN.Q=DI=0
011345 :  YQ:13,VJM,P1*R
011346 :  11,VIM,YQFIN
011347 :  14,MTJ,13
011348 :  13,UTM,-40B
011349 :  13,VZM,YQ
011350 :  ,UTC,TABSIMIS
011351 :  14,XTA,
011352 :  ,AAX,=:7
011353 :  ,UZA,SLMIST.БYKBA
011354 :  ,AEX,=:1
011355 :  ,UZA,SLMIST.OГPAHИЧ-ЛЬ
011356 :  ,XTA,FNNN
011357 :  ,ASN,64-8
011358 :  ,ITS,14
011359 :  15,AOX,
011360 :  ,ATX,FNNN
011361 :  ,UJ,YQ
011362 :  YQFIN:,XTA,FNNN
011363 :  ,UZA,SLMIST.Q=0
011364 :  ,ASN,64+16
011365 :  ,UIA,SLMIST.Q>99
011366 :  ,XTA,FNNN
011367 :  ,AAX,=17.I
011368 :  ,XTS,FNNN
011369 :  ,ASN,64+8
011370 :  ,AAX,=17.D
011371 :  ,ASN,64-1.D*2
011372 :  15,ATX,
011373 :  ,ASN,64-2.D*8
011374 :  15,ARX,.D*10
011375 :  15,ARX,.Q=D*10+I
011376 :  ,ARX,=77777
011377 :  ,AAX,=77777.Q=Q-1
011378 :  ,AEX,=:1.ПP.'ПOИCK Q'
011379 :  ,STX,RR1
011380 :  ,ATI,13
011381 :  13,UJ,
011382 : C------ПOИCK OБPAЗA HA KAPTE
011383 : C      SYMM-XAPAKTEPИCTИKA:
011384 : C      48-46 I,15-1 A
011385 : C      ПO KOЛ-BY:I=0,A=Q
011386 : C      ПO OБPAЗY:I=2,A=AДP.X-KИ
011387 : C      REZ:ECTЬ OБPAЗ:BOЗBPAT
011388 :  POISK/:,BSS,
011389 :  ,ATX,RRY
011390 :  ,XTA,PROGON
011391 :  13,U1A,
011392 :  ,ITA,13
011393 :  ,XTS,
011394 :  ,XTA,WBLAN
011395 :  ,UIA,ERRBLT.БЫЛA *DF
011396 :  ,XTA,RRY
011397 :  ,AAX,=:7
011398 :  ,UIA,PO/2.NOT Q
011399 :  PO/:,XTA,RRY
011400 :  ,UZA,PO/FIN.Q=0
011401 :  ,XTA,Q72
011402 :  ,UIA,ERRNO/.HET OБPAЗA
011403 :  8,VTM,SX1/
011404 :  13,VJM,R*P1.ЧTEHИE CИMBOЛA
011405 :  ,ITA,11
011406 :  ,ATX,Q72
011407 :  ,XTA,ZAP
011408 :  ,U1A,PV1.HET ЗAПИCИ
011409 :  PV:,BSS,
011410 :  8,VTM,SXR/.REZ>72
011411 :  ,XTA,W72
011412 :  ,UIA,PO/MI
011413 :  13,VJM,W*P1
011414 :  ,ITA,11
011415 :  ,ATX,W72
011416 :  PO/0:,XTA,RRY
011417 :  ,ARX,=77777
011418 :  ,AAX,=77777
011419 :  ,ATX,RRY.Q=Q-1
011420 :  ,UJ,PO/
011421 :  PV1:,XTA,ZAP
011422 :  ,AEX,=:1
011423 :  ,U1A,PO/0.*D
011424 :  14,VTM,40B.BLANK
011425 :  ,UJ,PV.*V
011426 :  PO/FIN:,STX,.BOЗBPAT
011427 :  ,ATI,13
011428 :  13,UJ,
011429 :  PO/2:8,VTM,SX1/
011430 :  13,VJM,RSIM.ЧTEHИE CИMBOЛA
011431 :  ,XTA,Q72
011432 :  ,U1A,ERRNO/.HET OБPAЗA
011433 :  14,UTM,-40B
011434 :  14,VZM,PO/2BL.BLANK
011435 :  ,XTA,RRY
011436 :  ,ASN,64-15
011437 :  ,ITS,8
011438 :  15,AEX,
011439 :  13,VJM,YZOB.X-KA
011440 :  ,UZA,PO/EQ.CP-E OБPAЗOB
011441 :  PO/2BL:,BSS,.COBПAЛИ
011442 :  ,XTA,Q72
011443 :  ,UIA,ERRNO/.HET OБPAЗA
011444 :  8,VTM,SX1/
011445 :  13,VJM,R*P1.ЧTEHИE CИMBOЛA
011446 :  ,ITA,11
011447 :  ,ATX,Q72
011448 :  ,XTA,ZAP
011449 :  ,U1A,PV*1.HET ЗAПИCИ
011450 :  PV*:,BSS,
011451 :  8,VTM,SXR/
011452 :  ,XTA,W72
011453 :  ,UIA,PO/MI.>72
011454 :  13,VJM,W*P1.ЗAПИCЬ CИMBOЛA
011455 :  ,ITA,11
011456 :  ,ATX,W72
011457 :  ,UJ,PO/2
011458 :  PV*1:,XTA,ZAP
011459 :  ,AEX,=:1
011460 :  ,U1A,PO/2.*D
011461 :  14,VTM,40B.BLANK
011462 :  ,UJ,PV*
011463 :  PO/EQ:,BSS,
011464 :  ,XTA,PRBEF
011465 :  ,UIA,PO/FIN.BEFORE
011466 :  ,XTA,YO5
011467 :  ,ATX,RRY.Q=YO5
011468 :  ,ATX,PRBEF
011469 :  ,UJ,PO/
011470 : C---   OПPEДEЛEHИE ДЛИHЫ OБPAЗA
011471 : C      HA YПPABЛЯЮЩEЙ KAPTE,
011472 : C      ПOДBOД K KAPTE *R,
011473 : C      KOHTPOЛЬ ПOPЯДKA OПEPAЦИЙ
011474 : C      LONG=Q
011475 : C      RRY-X-KA ДЛЯ POISK/
011476 :  LONG/:,BSS,
011477 :  ,ITA,13
011478 :  ,XTS,WWW
011479 :  ,UIA,LO/A.ПOДBOД HE HYЖEH
011480 :  ,ATX,Q72.Q72=0
011481 :  ,XTA,=:1
011482 :  ,ATX,PRBEF.Q=0
011483 :  13,VJM,POISK.ПOДBOД
011484 :  LO/A:,XTA,=1
011485 :  ,ATX,WWW
011486 :  13,VJM,PROVE.KOHTPOЛЬ
011487 :  8,VTM,XCARD
011488 :  13,VJM,P1
011489 :  9,VTM,X/CARD
011490 :  13,VJM,L1*TO*L2.KOПИЯ X-KИ
011491 :  8,VTM,X/CARD
011492 :  ,XTA,
011493 :  ,ATX,RR1*
011494 :  ,ATX,BLYPR
011495 :  ,ATX,LONG.Q=0
011496 :  ,ATX,LOBL.Q'=0
011497 :  LO/:13,VJM,R*P1
011498 :  14,UTM,-40B
011499 :  14,VIM,LO/1.NOT BLANK
011500 :  ,XTA,LOBL
011501 :  ,ARX,=1
011502 :  ,ATX,LOBL.Q'=Q'+1
011503 :  ,UJ,LO/2
011504 :  LO/1:14,UTM,-17B
011505 :  14,V1M,LO/1A.NON /
011506 :  ,XTA,DVDV
011507 :  ,AEX,=:1
011508 :  ,UZA,LO/B
011509 :  LO/1A:,XTA,LONG
011510 :  ,ARX,=1
011511 :  ,ARX,LOBL
011512 :  ,ATX,LONG.Q=Q+1+Q'
011513 :  ,XTA,
011514 :  ,ATX,LOBL.Q'=0
011515 :  LO/2:11,VZM,LO/.<73
011516 :  11,VTM,
011517 :  8,VTM,XCARD
011518 :  ,ITA,8
011519 :  ,AEX,=:1
011520 :  ,STX,RRY.X-KA POISK/
011521 :  ,ATI,13
011522 :  13,UJ,
011523 :  LO/B:11,V1M,LO/2
011524 :  13,VJM,M1
011525 :  8,VTM,X/CARD
011526 :  9,VTM,XID
011527 :  13,VJM,L1*TO*L2
011528 :  13,VJM,P1
011529 :  LO/C:11,V1M,ERRBL
011530 :  13,VJM,R*P1
011531 :  14,UTM,-40B
011532 :  14,VZM,LO/C.BLANK
011533 :  ,XTA,=1
011534 :  ,ATX,DVDV
011535 :  ,ATX,RR1*
011536 :  11,VTM,1
011537 :  ,UJ,LO/2
011538 : C---   ЗAПИCЬ(INSERT) OБPAЗA
011539 :  VSTAV/:,BSS,
011540 :  ,XTA,PROGON
011541 :  13,U1A,
011542 :  ,ITA,13
011543 :  ,XTS,
011544 :  VST/:,XTA,LONG.ДЛИHA OБP.
011545 :  ,UZA,VST/FIN.Q=0
011546 :  ,ARX,=77777
011547 :  ,AAX,=77777
011548 :  ,ATX,LONG.Q=Q-1
011549 :  8,VTM,XCARD
011550 :  13,VJM,R*P1
011551 :  ,XTA,W72
011552 :  ,UIA,PO/MI.>73
011553 :  8,VTM,SXR/
011554 :  13,VJM,W*P1.ЗAПИCЬ CИMBOЛA
011555 :  ,ITA,11
011556 :  ,ATX,W72
011557 :  ,UJ,VST/
011558 :  VST/FIN:,STX,.BOЗBPAT
011559 :  ,ATI,13
011560 :  13,UJ,
011561 : C---   ДOПOЛHEHИE PEЗYЛЬTATA
011562 : C      PEД-Я / ПPOБEЛAMИ
011563 :  DOKR/B:,ITA,13
011564 :  ,XTS,
011565 :  DOBL/:,XTA,W72.>72
011566 :  ,UIA,DOKR/F
011567 :  14,VTM,40B
011568 :  8,VTM,SXR/
011569 :  13,VJM,W*P1
011570 :  ,ITA,11
011571 :  ,ATX,W72
011572 :  ,UJ,DOBL/
011573 : C------OKOHЧAHИE PEДAKTИPOBAHИЯ /
011574 :  DOKR/:,BSS,
011575 :  ,XTA,WWW
011576 :  ,ATX,WWW/.KOПИЯ WWW
011577 :  13,UZA,.PEД-E ЗAKOHЧEHO
011578 :  ,XTA,WBLAN
011579 :  ,UIA,DOKR/B.БЫЛA *DF
011580 :  ,ITA,13
011581 :  ,XTS,
011582 :  13,VTM,DOBL/.ON BLANKING
011583 :  ,UJ,POISKF/.(*IF)
011584 :  DOKR/F:13,VTM,-11.PEЗYЛЬTAT
011585 :  :13,XTA,11+SCARD.PEДAKTИPOBAHИЯ
011586 :  13,ATX,CARDR+11./ B ИCXOДHYЮ
011587 :  13,VLM,DOKR/F+1.KAPTY
011588 : C      ЛEHTЫ ДЛЯ PEД-Я /
011589 :  8,VTM,SX1/
011590 :  9,VTM,CARDR
011591 :  10,VTM,12
011592 :  13,VJM,DO*LENT
011593 :  8,VTM,SXR/
011594 :  9,VTM,SCARD
011595 :  10,VTM,12
011596 :  13,VJM,DO*LENT
011597 : C      ЧИCTKA
011598 :  ,XTA,=1
011599 :  ,ATX,BLARECRE
011600 :  ,XTA,
011601 :  ,ATX,WWW
011602 :  ,ATX,WBLAN
011603 :  ,ATX,Q72
011604 :  ,ATX,W72
011605 :  ,ATX,ZAP
011606 :  ,ATX,PRBEF
011607 :  ,AEX,=:1
011608 :  13,VJM,POISK.ЗAXOД ЗA KAPTY
011609 :  ,XTA,PROGON.
011610 :  ,UIA,DOKR/E.
011611 :  ,XTA,YAR
011612 :  ,UIA,DOKR/E.БЫЛA *Y
011613 :  13,VJM,PRIREZ.ПЧ REZ
011614 :  DOKR/E:,BSS,
011615 :  ,XTA,
011616 :  ,STX,BLARECRE
011617 :  ,ATI,13
011618 :  13,UJ,
011619 : C---   ПOДBOД ЗA ПOCЛEДHИЙ
011620 : C      NONBLANK SYMBOL
011621 :  POISKF/:,BSS,
011622 :  ,XTA,PROGON
011623 :  13,U1A,
011624 :  ,ITA,13
011625 :  ,XTS,Q72
011626 :  ,UIA,POF/F.>72
011627 :  8,VTM,SX1/
011628 :  9,VTM,X/CARD
011629 :  13,VJM,L1*TO*L2.KOПИЯ X-KИ
011630 :  8,VTM,X/CARD
011631 : C      ПOДBOД K 73-MY
011632 :  POF/:13,VJM,P1
011633 :  11,VZM,POF/
011634 : C      ЗAMEHA ПPOБEЛOB XBOCTA
011635 : C      HYЛЯMИ
011636 :  POF/1:13,VJM,M1*R
011637 :  11,VIM,POF/2.<1
011638 :  14,UTM,-40B
011639 :  14,VIM,POF/2.NONBLANK
011640 :  14,VTM,
011641 :  13,VTM,POF/1
011642 :  ,UJ,WSIM.ЗAПИCЬ 0
011643 :  POF/2:8,VTM,SX1/
011644 :  ,XTA,Q72
011645 :  ,UIA,POF/F
011646 :  13,VJM,RSIM
011647 :  14,VZM,POF/BL.SIM=0
011648 :  8,VTM,SXR/
011649 :  ,XTA,W72
011650 :  ,UIA,PO/MI.>72
011651 :  13,VJM,W*P1.ЗAПИCЬ
011652 :  ,ITA,11
011653 :  ,ATX,W72
011654 :  8,VTM,SX1/
011655 :  13,VJM,P1.+1
011656 :  ,ITA,11
011657 :  ,ATX,Q72
011658 :  ,UJ,POF/2
011659 : C      ЗAMEHA HYЛEЙ ПPOБEЛAMИ
011660 :  POF/BL:9,VTM,X/CARD
011661 :  13,VJM,L1*TO*L2.KOПИЯ X-KИ
011662 :  8,VTM,X/CARD
011663 :  14,VTM,40B.BLANK
011664 :  POF/B:,BSS,
011665 :  11,V1M,POF/F
011666 :  13,VTM,POF/B.ПPOБEЛAMИ
011667 :  ,UJ,W*P1
011668 :  POF/F:,STX,.BOЗBPAT
011669 :  ,ATI,13
011670 :  13,UJ,
011671 : C------OБPAБOTKA NNN-KOЛИЧECTBA
011672 : C      HEYПPABЛЯЮЩИX KAPT,
011673 : C      CЛEДYЮЩИX ЗA ДAHHOЙ
011674 :  TONNN:,BSS,
011675 :  ,XTA,NNN.NNN
011676 :  13,UZA,
011677 :  ,ASN,64+16
011678 :  ,U1A,SLM.>99
011679 :  ,XTA,NNN.NNN=NK
011680 :  ,AAX,=17.K
011681 :  ,XTS,NNN
011682 :  ,ASN,64+8
011683 :  ,AAX,=17.N
011684 :  ,ASN,64-1.N*2
011685 :  15,ATX,
011686 :  ,ASN,64-2.N*8
011687 :  15,ARX,.N*10
011688 :  15,ARX,.N*10+K
011689 :  SLM1: ,ATX,BLYPR
011690 :  ,ATX,NNN
011691 :  ,XTA,BLYPR
011692 :  ,UZA,SLMIST.NNN=0
011693 :  13,UJ,
011694 :  SLM:,XTA,
011695 :  ,UJ,SLM1
011696 : C---   ФOPMИPOBAHИE ПPИЗHAKOB CEPBИCA
011697 : C      ПO MACKE B I12-CHЯTИE
011698 : C      ПO MACKE B I14-ПOCTAHOBKA
011699 :  RBT0:,BSS,
011700 :  ,ITA,12
011701 :  ,AAX,RABOTA1
011702 :  ,AEX,RABOTA1
011703 :  ,ITS,14
011704 :  15,AOX,
011705 :  ,ATX,RABOTA1
011706 :  ,XTA,NOENDTA
011707 :  13,UZA,.HET HEOK. TAKE
011708 :  ,ITA,12
011709 :  ,AAX,RABOTA2
011710 :  ,AEX,RABOTA2
011711 :  ,ITS,14
011712 :  15,AOX,
011713 :  ,ATX,RABOTA2
011714 :  13,UJ,
011715 : C------OБPAБOTKA *N
011716 :  *N:,BSS,
011717 :  14,VTM,1.ПP.*N
011718 :  NUM*:12,VTM,3
011719 :  13,VJM,RBT0
011720 :  11,VIM,SLMIST.HET YKAЗATEЛЯ
011721 :  8,VTM,XID
011722 :  9,VTM,NUMBERI
011723 :  10,VTM,2
011724 :  6,VTM,-7
011725 :  13,VJM,DO*LENT.ЛEHTA CИMBOЛOB
011726 : C      OФOPMЛEHИE HOMEPA B NUMBERI
011727 :  NUM:8,VTM,XCARD
011728 :  13,VJM,P1*R
011729 :  8,VTM,XID
011730 :  13,VJM,W*P1
011731 :  6,VLM,NUM.8 CИMBOЛOB
011732 :  14,UTM,-40B
011733 :  14,V1M,SLCARD
011734 :  13,VJM,M1.-1
011735 :  14,VTM,60B
011736 :  13,VTM,SLCARD
011737 :  ,UJ,WSIM
011738 : C------OБPAБOTKA *NI
011739 :  *NI:,BSS,
011740 :  14,VTM,2
011741 :  ,UJ,NUM*
011742 : C      OБPAБOTKA *NS
011743 :  *NS:,BSS,
011744 :  8,VTM,XCARD
011745 :  1,VTM,.S=0
011746 :  *NS1:11,V1M,*NS3
011747 :  13,VJM,P1*R
011748 :  14,UTM,-40B
011749 :  14,VZM,*NS1.BLANK
011750 :  14,UTM,-21B
011751 :  14,V1M,SLMIST.NOT 1
011752 :  *NS2:11,V1M,*NS3
011753 :  13,VJM,P1*R
011754 :  14,UTM,-40B
011755 :  14,VZM,*NS2.BLANK
011756 :  14,UTM,-20B
011757 :  14,V1M,SLMIST.NOT 0
011758 :  1,UTM,1.S=S+1
011759 :  ,UJ,*NS2
011760 :  *NS3:,ITA,1
011761 :  ,ATX,STEP
011762 :  ,ASN,64+3
011763 :  ,U1A,SLMIST.S>7
011764 :  ,UJ,SLCARD
011765 : C------OБPAБOTKA *LL
011766 :  *LL:,BSS,
011767 :  ,XTA,RABOTA1
011768 :  13,VJM,ENDLI.OK-E LIST
011769 :  14,VTM,4
011770 :  *LL0:12,VTM,14B
011771 :  *LL1:13,VTM,SLCARD
011772 :  ,UJ,RBT0.ПP.PAБOTЫ
011773 : C------OБPAБOTKA *HL
011774 :  *HL:,BSS,
011775 :  ,XTA,RABOTA1
011776 :  13,VJM,ENDLI
011777 :  14,VTM,10B
011778 :  ,UJ,*LL0
011779 : C------OБPAБOTKA *LO
011780 :  *LO:,BSS,
011781 :  ,XTA,RABOTA1
011782 :  13,VJM,ENDLI
011783 :  12,VTM,14B
011784 :  *LO1:13,VTM,SLCARD
011785 :  ,UJ,RBT
011786 : C------OБPAБOTKA *LP
011787 :  *LP:,BSS,
011788 :  ,XTA,RABOTA1
011789 :  13,VJM,ENDPU.OK-E PUNCH
011790 :  ,CALL,COLPUNB*
011791 :  14,VTM,20B
011792 :  *LP0:12,VTM,60B
011793 :  ,UJ,*LL1
011794 : C________ OБPAБOTKA *LT
011795 :  *LT:,XTA,RABOTA1.
011796 :  13,VJM,ENDLI.OKOHЧ. ПPEДЫДYЩ LIST
011797 :  14,VTM,4.
011798 :  ,XTA,=1.
011799 :  ,ATX,LTPR.ПPИЗHAK *LT
011800 :  ,UJ,*LL0
011801 : C------OБPAБOTKA *CP
011802 :  *CP:,BSS,
011803 :  9,VTM,3
011804 :  13,VJM,PARAMT .ИДEHT. B TEXT(RR1)
011805 :  14,V1M,SLMIST
011806 :  ,XTA,RABOTA1
011807 :  13,VJM,ENDPU
011808 :  ,XTS,RR1
011809 :  ,CALL,FORMKOZA.PUNCH NAME
011810 :  14,VTM,40B
011811 :  ,UJ,*LP0
011812 : C------OБPAБOTKA *PO
011813 :  *PO:,BSS,
011814 :  ,XTA,RABOTA1
011815 :  13,VJM,ENDPU
011816 :  12,VTM,60B
011817 :  ,UJ,*LO1
011818 : C------OБPAБOTKA *U
011819 :     *U:,BSS,
011820 :  9,VTM,3
011821 :  13,VJM,PARAMT.ИДEHT. B TEXT(RR1)
011822 :  14,V1M,SLMIST
011823 :  14,VTM,-13.
011824 :  *U1:14,UTC,CARD.
011825 :  ,XTA,13.
011826 :  14,UTC,*TIKTAK*
011827 :  ,ATX,10+13.*U:<ИMЯ>.......
011828 :  14,VLM,*U1.  *U: ....
011829 :  ,XTA,RR1
011830 :  ,ATX,NOWRIT
011831 :  14,VTM,100B
011832 :  12,VTM,100B
011833 :  ,UJ,*LL1
011834 : C------OБPAБOTKA *UO
011835 :  *UO:,BSS,
011836 :  ,XTA,RABOTA1.
011837 :  ,AAX,=100
011838 :  ,UZA,SLCARD.HE *USER
011839 :  ,XTA,=I1
011840 :  ,UTC,*TIKTAK*
011841 :  ,ATX,24.
011842 :  ,XTA,=:4
011843 :  ,ATX,CREAD.
011844 :  14,VTM,READ OLD
011845 :  ,ITA,14.
011846 :  13,VJM,NAMCALL2.
011847 :  ,XTA,RABOTA1
011848 :  13,VJM,ENDUZ
011849 :  12,VTM,100B
011850 :  ,UJ,*LO1
011851 : C---   OKOHЧAHИE PAБOTЫ ПO *U
011852 :  ENDUZ:,BSS,
011853 :  ,AAX,=100
011854 :  13,UZA,
011855 :  ,XTA,
011856 :  ,ATX,NOWRIT
011857 :  ,UTC,*TIKTAK*
011858 :  ,ATX,24.
011859 :  ,WTC,17B.OБECПEЧEHИE
011860 :  ,ATX,.ПEPEЗAГPYЗKИ
011861 :  14,VTM,XREAD/
011862 :  14,XTA,
011863 :  ,AOX,=4000 0000 0000
011864 :  ,AEX,=4000 0000 0000
011865 :  14,ATX,.NONCOSY
011866 :  ,UJ,COSY0
011867 : C---   OKOHЧAHИE PAБOTЫ *LP,*CP
011868 :  ENDPU:,BSS,
011869 :  ,ATX,ID2
011870 :  ,ITA,13
011871 :  ,XTS,ID2
011872 :  ,AAX,=20
011873 :  ,UZA,EPU.HE *LP
011874 :  ,CALL,COLPUNE*
011875 :  EPU:,XTA,ID2
011876 :  ,AAX,=40
011877 :  ,UZA,EPU1.HE *CP
011878 :  ,XTS,
011879 :  13,VJM,FORMKOZA+1
011880 :  EPU1:,STX,
011881 :  ,ATI,13
011882 :  13,UJ,
011883 : C---   OKOHЧAHИE PAБOTЫ *LL,*HL
011884 :  ENDLI:,BSS,
011885 :  ,AAX,=14
011886 :  13,UZA,
011887 :  ,XTA,
011888 :  ,ATX,NAHL
011889 :  ,ATX,LTPR.
011890 :  13,UJ,
011891 : C------OБPAБOTKA *B
011892 :  *B:,BSS,
011893 :  13,VJM,PARA.N1,N2
011894 :  13,VJM,PROVE.KOHTPOЛЬ
011895 :  13,VJM,TONNN.N-HEYПP
011896 :  ,XTA,N2
011897 :  ,U1A,SLMIST.ECTЬ N2
011898 : C      OБPAЩEHИE K POISK
011899 :  ,ATX,ZAP
011900 :  ,XTA,=1
011901 :  ,ATX,PRBEF
011902 :  14,VTM,N1
011903 :  ,ITA,14
011904 :  13,VJM,POISK
011905 :  ,UJ,SLCARD
011906 : C------OБPAБOTKA *I
011907 :  *I:,BSS,
011908 :  13,VJM,PARA
011909 :  13,VJM,PROVE
011910 :  13,VJM,TONNN
011911 :  ,XTA,N2
011912 :  ,U1A,SLMIST.ECTЬ N2
011913 :  ,ATX,ZAP
011914 :  ,ATX,PRBEF
011915 :  14,VTM,N1
011916 :  ,ITA,14
011917 :  13,VTM,SLCARD
011918 :  ,UJ,POISK
011919 : C------OБPAБOTKA *D
011920 :  *D:,BSS,
011921 :  13,VJM,PARA
011922 :  13,VJM,PROVE
011923 :  13,VJM,TONNN
011924 :  ,XTA,
011925 :  ,ATX,ZAP
011926 :  ,XTA,=1
011927 : C      ПOДBOД K N1
011928 :  ,ATX,PRBEF
011929 : C       ИCKЛЮЧEHИE N1 ИЛИ N1-N2
011930 :  14,VTM,N1
011931 :  ,ITA,14
011932 :  13,VJM,POISK
011933 :  14,VTM,N1
011934 :  ,XTA,N2
011935 :  ,UZA,*DEL
011936 :  14,VTM,N2
011937 : C      N2 B ЭTAЛOH
011938 :  ,XTA,N2
011939 :  ,ATX,NETAL
011940 :  ,XTA,N2+1
011941 :  ,ATX,NETAL+1
011942 :  *DEL:,XTA,
011943 :  ,ATX,PRBEF
011944 :  *DEL1: ,XTA,=1
011945 :  ,ATX,ZAP
011946 :  13,VJM,PROVE
011947 :  ,ITA,14
011948 :  13,VJM,POISK
011949 :  ,UJ,SLCARD
011950 : C-----OБPAБOTKA *DA
011951 :  *DA:,BSS,.
011952 :  13,VJM,PARA
011953 :  13,VJM,TONNN
011954 :  ,XTA,N2
011955 :  ,U1A,SLMIST.ECTЬ N2
011956 :  14,VTM,N1
011957 :  ,UJ,*DEL
011958 : C------OБPAБOTKA *DB
011959 :  *DB:,BSS,.
011960 :  13,VJM,PARA
011961 :  13,VJM,TONNN
011962 :  ,XTA,N2
011963 :  ,U1A,SLMIST.ECTЬ N2
011964 :  14,VTM,N1
011965 :  ,XTA,=1
011966 :  ,ATX,PRBEF
011967 :  ,UJ,*DEL1
011968 : C------OБPAБOTKA *L
011969 :  *L:,BSS,
011970 :  13,VJM,TONNN
011971 :  11,VZM,*LIN.ECTЬ :
011972 :  ,UJ,SLCARD
011973 : C      OФOPMЛEHИE KAPTЫ
011974 :  *LIN:14,VTM,-11
011975 :  ,XTA,=H
011976 :  :14,ATX,CARD/+11
011977 :  14,VLM,*LIN+1.ПPOБEЛЫ
011978 :  8,VTM,XID
011979 :  9,VTM,CARD/
011980 :  10,VTM,12
011981 :  13,VJM,DO*LENT.ЛEHTA
011982 : C      ПEPEHOC TEKCTA C *L:TEXT
011983 :  *LIN1:8,VTM,XCARD
011984 :  13,VJM,P1*R
011985 :  11,V1M,*LIN2
011986 :  8,VTM,XID
011987 :  13,VJM,W*P1
011988 :  ,UJ,*LIN1
011989 :  *LIN2:,BSS,
011990 :  ,XTA,PROGON.
011991 :  ,UIA,SLCARD.
011992 :  14,VTM,CARD/
011993 :  ,ITA,14
011994 :  13,VTM,SLCARD
011995 :  ,UJ,NAMCALL2.CEPBИC
011996 : C------OБPAБOTKA *T:N1,N2
011997 :  *T:,BSS,
011998 :  13,VJM,PARA
011999 :  13,VJM,DOKTAKE
012000 :  ,XTA,N2
012001 :  ,UZA,SLMIST.HET N2
012002 :  ,XTA,=1
012003 :  ,ATX,YESTAKE
012004 :  ,ATX,NOENDR
012005 :  ,ATX,ZAP
012006 :  ,ATX,PRBEF
012007 :  ,XTA,BLTAKE
012008 :  ,AOX,TOUT
012009 :  ,UZA,*T1.HET *BT,*TO
012010 :  ,XTA,
012011 :  ,ATX,ZAP
012012 :  *T1:,BSS,
012013 :  14,VTM,N1
012014 :  ,ITA,14
012015 :  13,VJM,POISK.K N1
012016 : C      XAPAKTEPИCTИKA TAKE
012017 :  14,VTM,NTAK
012018 :  ,ITA,14
012019 :  ,ATX,XNOENDTA
012020 :  ,ATX,NOENDTA
012021 : C      ПEPEHOC N2 B NTAK
012022 :  ,XTA,N2
012023 :  ,ATX,NTAK
012024 :  ,XTA,N2+1
012025 :  ,ATX,NTAK+1
012026 :  ,UJ,SLCARD
012027 : C------OБPAБOTKA *TN:NAME
012028 :  *TN:,BSS,
012029 :  13,VJM,DOKTAKE
012030 :  9,VTM,3
012031 :  13,VJM,PARAMT.ИДEHTИФИKATOP B ISO
012032 :  14,V1M,SLMIST.ПAPAMETP C OШИБKOЙ
012033 :  14,VTM,*MONCRD*
012034 :  14,XTA,
012035 :  ,ATX,TN
012036 :  14,XTA,1
012037 :  ,ATX,TN+1
012038 :  ,XTA,=1
012039 :  ,ATX,YESTAKE
012040 :  ,ATX,NOENDR
012041 :  ,ATX,ZAP
012042 :  ,ATX,PRBEF
012043 :  ,XTA,BLTAKE
012044 :  ,AOX,TOUT
012045 :  ,UZA,*TN3.HET *BT,*TO
012046 :  ,XTA,
012047 :  ,ATX,ZAP
012048 : C      ПOДBOД K KAPTE NAME(BEFORE)
012049 :  *TN3:14,VTM,TN
012050 :  ,ITA,14
012051 :  ,AEX,=:3
012052 :  13,VJM,POISK
012053 :  ,XTA,=:4
012054 :  ,ATX,XNOENDTA.X-KA END
012055 :  ,ATX,NOENDTA
012056 :  ,UJ,SLCARD
012057 : C------OБPAБOTKA *TO
012058 :  *TO:,BSS,
012059 :  13,VJM,DOKTAKE
012060 :  ,XTA,=1
012061 :  ,ATX,TOUT
012062 :  ,UJ,SLCARD
012063 : C------OБPAБOTKA *NO
012064 :  *NO:,BSS,
012065 :  ,XTA,=H
012066 :  ,ATX,NUMBERI
012067 :  ,XTA,NUMBERE
012068 :  ,ATX,NUMBERI+1
012069 :  12,VTM,3
012070 :  13,VJM,RBT
012071 :  ,UJ,SLCARD
012072 : C------OБPAБOTKA *BT
012073 :  *BT:,BSS,
012074 :  13,VJM,DOKTAKE
012075 :  ,XTA,=1
012076 :  ,ATX,BLTAKE
012077 :  ,UJ,SLCARD
012078 : C------OБPAБOTKA *I.OБPAЗ
012079 :  *I*:,BSS,
012080 :  13,VJM,LENT0.X-KA
012081 :  13,VJM,   PROVE
012082 :  13,VJM,TONNN
012083 :  ,XTA,
012084 :  *I*1:,ATX,ZAP
012085 :  *I*2:,ATX,PRBEF
012086 :  ,XTA,RR1
012087 :  *I*4:13,VTM,SLCARD
012088 :  ,UJ,POISK
012089 : C------OБPAБOTKA *B.
012090 :  *B*:,BSS,
012091 :  13,VJM,LENT0
012092 :  13,VJM,PROVE
012093 :  13,VJM,TONNN
012094 :  ,XTA,
012095 :  ,ATX,ZAP
012096 :  ,XTA,=1
012097 :  ,UJ,*I*2
012098 : C      OБPAБOTKA *IF.
012099 :  *IF*:,BSS,
012100 :  13,VTM,*RO
012101 :  ,UJ,PROVE
012102 : C------OБPAБOTKA *D.
012103 :  *D*:,BSS,
012104 :  13,VJM,LENT0
012105 :  13,VJM,PROVE
012106 :  13,VJM,TONNN
012107 :  ,XTA,
012108 :  ,ATX,ZAP
012109 :  ,XTA,=1
012110 :  ,ATX,PRBEF
012111 :  ,XTA,RR1
012112 :  13,VJM,POISK.ПOДBOД
012113 :  ,XTA,=1
012114 :  ,ATX,ZAP
012115 :  13,VJM,PROVE
012116 : C      ИCKЛЮЧEHИE KAPTЫ
012117 :  ,XTA,
012118 :  ,ATX,PRBEF
012119 :  ,ATX,YESCARD
012120 :  ,UJ,SLCARD
012121 : C------OБPAБOTKA *DB.
012122 :  *DB*:,BSS,
012123 :  13,VJM,LENT0
012124 :  13,VJM,PROVE
012125 :  13,VJM,TONNN
012126 :  ,XTA,=1
012127 :  ,UJ,*I*1
012128 : C------OБPAБOTKA *DA.
012129 :  *DA*:,BSS,
012130 :  13,VJM,LENT0
012131 :  13,VJM,PROVE
012132 :  13,VJM,TONNN
012133 :  ,XTA,=1
012134 :  ,ATX,ZAP
012135 :  ,XTA,
012136 :  ,UJ,*I*2
012137 : C------OЮPAБOTKA *IQ.
012138 :  *IQ*:,BSS,
012139 :  13,VJM,YKAZQ.X-KA
012140 :  13,VJM,PROVE
012141 :  13,VJM,TONNN
012142 :  ,XTA,
012143 :  ,ATX,ZAP
012144 :  ,XTA,
012145 :  ,UJ,*I*2
012146 : C------OБPAБOTKA *DQ.
012147 :  *DQ*:,BSS,
012148 :  13,VJM,YKAZQ
012149 :  13,VJM,PROVE
012150 :  13,VJM,TONNN
012151 :  ,XTA,=1
012152 :  ,ATX,ZAP
012153 :  ,UJ,*DQ*-1
012154 : C      OБPAБOTKA *DF.
012155 :  *DF*:,BSS,
012156 :  ,XTA,NOENDTA
012157 :  ,UIA,SLMIST.
012158 :  ,ATX,NOENDR
012159 :  13,VTM,*REO
012160 :  ,UJ,PROVE
012161 : C------OБPAБOTKA *C:,*C.,*C/
012162 :  *C:,BSS,
012163 :  *C*:,BSS,
012164 :  *C/:,BSS,
012165 :  8,VTM,XID
012166 :  ,XTA,=H
012167 :  ,ATX,IDID.HAЧAЛO
012168 :  ,ATX,IDID+1.COMMENT
012169 :  9,VTM,IDID+2
012170 :  10,VTM,10
012171 :  13,VJM,DO*LENT.ЛEHTA
012172 : C      ПEPEHOC KOMMEHTAPИЯ
012173 :  *C1:8,VTM,XCARD
012174 :  13,VJM,P1
012175 :  11,VIM,*C2
012176 :  13,VJM,RSIM
012177 :  8,VTM,XID
012178 :  13,VJM,W*P1
012179 :  11,VIM,*C3
012180 :  ,UJ,*C1
012181 : C      ДOПOЛHEHИE ПPOБEЛAMИ
012182 :  *C2:14,VTM,40B
012183 :  8,VTM,XID
012184 :  13,VJM,W*P1
012185 :  11,VZM,*C2
012186 : C      ПEЧATЬ 3-X CTPOK:
012187 : C      CARCOM,COMMENT,CARCOM
012188 :  *C3:,BSS,
012189 :  8,VTM,CARCOM
012190 :  13,VJM,PERCARD
012191 :  13,VJM,PRINTCD.CARCOM
012192 :  8,VTM,IDID
012193 :  13,VJM,PERCARD
012194 :  13,VJM,PRINTCD.COMMENT
012195 :  8,VTM,CARCOM
012196 :  13,VJM,PERCARD
012197 :  13,VTM,SLCARDN
012198 :  ,UJ,PRINTCD.CARCOM
012199 : C---   ПEPEHOC CTPOKИ B CARD
012200 : C      I8=AДPEC CTPOKИ
012201 :  PERCARD:,BSS,
012202 :  9,VTM,-13
012203 :  PCA:9,UTC,
012204 :  8,XTA,13
012205 :  9,ATX,CARD+13
012206 :  9,VLM,PCA
012207 :  13,UJ,
012208 : C------OБPAБOTKA *D/
012209 :  *D/:,BSS,
012210 :  ,XTA,=1
012211 :  ,ATX,DVDV
012212 :  13,VJM,TONNN.NNN
012213 :  13,VJM,LONG/.ДЛИHA,X-KA
012214 :  ,XTA,
012215 :  ,ATX,ZAP
012216 :  ,XTA,=1
012217 :  ,ATX,PRBEF
012218 :  ,XTA,NNN
012219 :  ,UIA,*DK/.NNN NEQ 0
012220 :  ,XTA,RRY
012221 :  15,ATX,
012222 :  13,VJM,POISK/.ПOДBOД
012223 :  ,XTA,DVDV
012224 :  ,ATX,ZAP
012225 :  ,XTA,
012226 :  ,STX,PRBEF
012227 :  *D/A:13,VJM,POISK/.DELETE
012228 :  ,XTA,RR1*
012229 :  ,UZA,SLCARD
012230 :  ,XTA,
012231 :  ,ATX,RR1*
012232 :  8,VTM,XID
012233 :  9,VTM,XCARD
012234 :  13,VTM,*L/+1
012235 :  ,UJ,L1*TO*L2
012236 :  *DK/:,XTA,DVDV
012237 :  ,ATX,ZAP
012238 :  ,ATX,PRBEF
012239 :  ,XTA,NNN
012240 :  13,VJM,POISK/.DEL Q
012241 :  ,UJ,*L/0.INSERT
012242 : C------OБPAБOTKA *V/
012243 :  *V/:,XTA,=:1
012244 :  ,ATX,DVDV
012245 :  ,UJ,*D/+1
012246 : C------OБPAБOTKA *DB/
012247 :  *DB/:,BSS,
012248 :  ,XTA,=1
012249 :  ,ATX,DVDV
012250 :  13,VJM,LONG/
012251 :  ,XTA,DVDV
012252 :  ,ATX,ZAP
012253 :  *DB/0:,ATX,PRBEF
012254 :  ,XTA,RRY
012255 :  ,UJ,*D/A
012256 : C------OБPAБOTKA *VB/
012257 :  *VB/:,XTA,=:1
012258 :  ,ATX,DVDV
012259 :  ,UJ,*DB/+1
012260 : C------OБPAБOTKA *DA/
012261 :  *DA/:,BSS,
012262 :  ,XTA,=1
012263 :  ,ATX,DVDV
012264 :  13,VJM,LONG/
012265 :  ,XTA,DVDV
012266 :  ,ATX,ZAP
012267 :  ,XTA,
012268 :  ,UJ,*DB/0
012269 : C------OБPAБOTKA *VA/
012270 :  *VA/:,XTA,=:1
012271 :  ,ATX,DVDV
012272 :  ,UJ,*DA/+1
012273 : C------OБPAБOTKA *DF/
012274 :  *DF/:,BSS,
012275 :  13,VJM,LONG/
012276 :  ,XTA,=1
012277 :  ,ATX,Q72
012278 :  ,ATX,WBLAN
012279 :  13,VJM,VSTAV/
012280 :   ,UJ,SLCARD
012281 : C------OБPAБOTKA *I/
012282 :  *I/:,BSS,
012283 :  13,VJM,TONNN
012284 :  13,VJM,LONG/
012285 :  ,XTA,
012286 :  ,ATX,ZAP
012287 :  ,XTA,NNN
012288 :  ,UIA,*DK/+1
012289 :  *I/1A:13,VTM,SLCARD
012290 :  ,ATX,PRBEF
012291 :  ,XTA,RRY
012292 :  ,UJ,POISK/
012293 : C------OБPAБOTKA *B/
012294 :  *B/:,BSS,
012295 :  13,VJM,LONG/
012296 :  ,XTA,
012297 :  ,ATX,ZAP
012298 :  ,XTA,=1
012299 :  ,UJ,*I/1A
012300 : C------OБPAБOTKA *L/
012301 :  *L/:,BSS,
012302 :  13,VJM,TONNN
012303 :  13,VJM,LONG/
012304 :  ,XTA,NNN
012305 :  ,UZA,*L/0
012306 :  ,ATX,LONG
012307 :  *L/0:13,VTM,SLCARD
012308 :  ,UJ,VSTAV/
012309 : C------OБPAБOTKA *IF/
012310 :  *IF/:,BSS,
012311 :  13,VJM,LONG/
012312 :  13,VTM,*L/0
012313 :  ,UJ,POISKF/
012314 : C------OБPAБOTKA *M/
012315 :  *M/:,BSS,
012316 :  13,VJM,LONG/
012317 :  13,VJM,POISKF/
012318 :  13,VJM,VSTAV/
012319 :  13,VJM,PRINTCD
012320 :  13,VTM,SLCARDN
012321 :  ,UJ,DOKR/
012322 : C------OБPAБOTKA *SS
012323 :  *SS:,BSS,
012324 :  8,VTM,XCARD
012325 :  *SS0:11,V1M,*SS2
012326 :  13,VJM,P1*R
012327 :  14,UTM,-40B
012328 :  14,VZM,*SS0
012329 :  *SS1:14,UTM,40B
012330 :  ,ITA,14
012331 :  ,ATX,FOR*
012332 :  ,UJ,SLCARD
012333 :  *SS2:14,VTM,12B.*-40B
012334 :  ,UJ,*SS1
012335 : C------OБPAБOTKA *Z
012336 :  *Z:,BSS,
012337 :  ,XTA,WWW/
012338 :  ,UIA,*Z1.HET KAPTЫ
012339 : C      ПPOДBИЖEHИE ЗA OДHY KAPTY
012340 :  ,XTA,=:1
012341 :  ,ATX,PRBEF
012342 :  13,VJM,POISK
012343 :  ,XTA,
012344 :  ,ATX,ZAP
012345 :  ,ATX,PRBEF
012346 : C      ПOДBOД K CЛEДYЮЩEИ
012347 :  ,XTA,=:1
012348 :  13,VJM,POISK
012349 :  13,VTM,SLCARD
012350 :  ,UJ,PROVE
012351 :  *Z1:,XTA,
012352 :  ,ATX,WWW/
012353 :  ,UJ,SLCARD
012354 : C------OБPAБOTKA *Y
012355 :  *Y:,BSS,
012356 :  ,XTA,
012357 :  ,ATX,BLYPR
012358 :  ,XTA,=777777
012359 :  ,ATX,YAR
012360 :  ,UJ,SLCARD
012361 : C------OБPAБOTKA *PAGE
012362 :  *YO:,BSS,
012363 :  ,XTA,.*PAGE
012364 :  ,ATX,YAR
012365 :  ,UJ,SLCARD
012366 :  *P:,BSS,
012367 :  13,VJM,PRINTCD
012368 :  13,VTM,SLCARDN
012369 :  ,XTA,NOLISFL*
012370 :  13,U1A,.NO LIST
012371 :  ,XTA,=2.
012372 :  ,ATX,PFLAG.
012373 :  ,UJ,PRINT8//+2
012374 : C------OБPAБOTKA *PP
012375 :  *PP:,BSS,
012376 :  13,VJM,PRINTCD
012377 :  13,VTM,SLCARDN
012378 :  ,XTA,NOLISFL*
012379 :  13,U1A,
012380 :  ,XTA,=3.
012381 :  ,ATX,PFLAG.
012382 :  ,UJ,PRINT8//+3
012383 : C------OБPAБOTKA *(((
012384 : C      (YЧET YPOBHEЙ BЛ-TИ)
012385 :  *3/1:,BSS,
012386 :  ,XTA,BLYPR
012387 :  ,UZA,*3A.HET БЛ-KИ
012388 :  ,AAX,=:1
012389 :  ,UZA,SLMIST.HE БЫЛO *(((
012390 :  *3A:,BSS,
012391 :  ,XTA,YROV
012392 :  ,UIA,*3D.NOT 0
012393 :  13,VJM,DOKR/.OK-E /
012394 :  ,XTA,=:1
012395 :  ,ATX,BLYPR
012396 :  ,XTA,=1
012397 :  ,ATX,YROV
012398 :  ,UJ,SLCARD
012399 :  *3D:,XTA,YROV
012400 :  ,ARX,=1
012401 :  ,ATX,YROV.YROV+1
012402 :  ,UJ,NOYPR
012403 : C------ПEЧATЬ ДATЫ И ЗAГOЛOBKA
012404 :  EDNAM:,XTA,NOLISFL*
012405 :  13,U1A,
012406 :  ,ITA,13
012407 :  ,XTS,
012408 :  ,CALL,DATAPR*.DATA
012409 :  14,VTM,TEXTA
012410 :  ,ITS,14
012411 :  14,VTM,TEXTB-1
012412 :  ,ITS,14
012413 :  ,XTS,=1
012414 :  ,CALL,PRINT8//.HAЧAЛO
012415 :  ,STX,
012416 :  ,ATI,13
012417 :  14,VTM,TEXTB
012418 :  ,ITS,14
012419 :  14,VTM,TEXTB+1
012420 :  ,ITS,14
012421 :  ,XTS,=4
012422 :  ,UJ,PRINT8//.KOHEЦ
012423 : C------TAБЛИЦЫ ДЛЯ AHAЛИЗA
012424 : C      YПPABЛЯЮЩИX CЛOB:
012425 : C      ЛEBAЯ KOMAHДA:48-42 J,15-1-S
012426 : C       J-KOД HAЧAЛЬHOГO CИMBOЛA
012427 : C       S=1-ECTЬ CCЫЛKA HA ДOП.TAБЛИЦY
012428 : C      ПPABAЯ KOMAHДA:48-N,15-1-A
012429 : C       N=1-PAЗPEШEHO YKAЗAHИE ЧИCЛA
012430 : C       A(S=0)-AДPEC ПP.OБPAБOTKИ
012431 : C       A(S=1)-AДPEC ДOП.TAБЛИЦЫ
012432 : C          KOHEЦ:HYЛEBOE CЛOBO
012433 : CX     ДOПOЛHИTEЛЬHЫE TAБЛИЦЫ:
012434 : C      ИMEЮT ПOДOБHYЮ CTPYKTYPY:S=0
012435 : C      KOHEЦ:ЛEBAЯ KOMAHДA=0
012436 : C*
012437 : C**    TAБЛИЦA PEДAKTИPOBAHИЯ :
012438 : C*
012439 :  TWOT:,BSS,
012440 :  10,Z10,1.R
012441 :  ,UJ,READ
012442 :  10,Z34,1.W
012443 :  ,UJ,WRITE11
012444 :  9,Z30,1.N
012445 :  ,UJ,NUMBER1
012446 :  8,Z10,1.B
012447 :  ,UJ,BBB1
012448 :  9,Z04,.I
012449 :  8,UJ,*I
012450 :  8,Z20,1.D
012451 :  ,UJ,*DELET
012452 :  9,Z20,1.L
012453 :  ,UJ,LINE
012454 :  10,Z20,1.T
012455 :  ,UJ,TAKE1
012456 :  11,Z10,.Z
012457 :  ,UJ,*Z
012458 :  8,Z24,1.E
012459 :  ,UJ,ENDE1
012460 :  10,Z00,1.P
012461 :  ,UJ,PUNCH
012462 :  8,Z14,1.C
012463 :  ,UJ,COMME
012464 :  11,Z04,1.Y
012465 :  ,UJ,YARD
012466 :  9,Z00,1.H
012467 :  ,UJ,HEAD
012468 :  10,Z24,1.U
012469 :  ,UJ,UZER
012470 :  10,Z14,.S
012471 :  ,UJ,*SS
012472 :  8,Z04,.A
012473 :  ,UJ,*A.
012474 :  ,OCT,
012475 : C---   ДOП.TAБЛИЦA ДЛЯ *D
012476 :  *DELET:8,Z04,.A
012477 :  8,UJ,*DA
012478 :  8,Z10,.B
012479 :  8,UJ,*DB
012480 :  ,Z00,
012481 :  8,UJ,*D
012482 : C---   ДOП.TAБЛИЦA ДЛЯ *E
012483 :  ENDE1:8,Z24,.E
012484 :  ,UJ,*EE
012485 :  ,OCT,
012486 : C---   ДOП.TAБЛИЦA ДЛЯ *W
012487 :  WRITE11:8,Z14,.C
012488 :  ,UJ,*WC
012489 :  9,Z34,.O
012490 :  ,UJ,*WO
012491 :  ,Z00,
012492 :  ,UJ,*W
012493 : C---   ДOП.TAБЛИЦA ДЛЯ *N
012494 :  NUMBER1:9,Z04,.I
012495 :  ,UJ,*NI
012496 :  9,Z34,.O
012497 :  ,UJ,*NO
012498 :  10,Z14,.S
012499 :  ,UJ,*NS
012500 :  ,Z00,
012501 :  ,UJ,*N
012502 : C---   ДOП.TAБЛИЦA ДЛЯ *B
012503 :  BBB1:,BSS,
012504 :  10,Z20,.T
012505 :  ,UJ,*BT
012506 :  ,Z00,
012507 :  8,UJ,*B
012508 : C---   ДOП.TAБЛИЦA ДЛЯ *T
012509 :  TAKE1:9,Z34,.O
012510 :  ,UJ,*TO
012511 :  9,Z30,.N
012512 :  ,UJ,*TN
012513 :  ,Z00,
012514 :  ,UJ,*T
012515 : C---   ДOП.TAБЛИЦA ДЛЯ *R
012516 :  READ:9,Z34,.O
012517 :  ,UJ,*RO
012518 :  ,Z00,
012519 :  ,UJ,*R
012520 : C---   ДOП.TAБЛИЦA ДЛЯ *L
012521 :  LINE:9,Z34,.O
012522 :  ,UJ,*LO
012523 :  9,Z20,.L
012524 :  ,UJ,*LL
012525 :  10,Z00,.P
012526 :  ,UJ,*LP
012527 :  10,Z20,.T
012528 :  ,UJ,*LT.
012529 :  ,Z00,
012530 :  8,UJ,*L
012531 : C---   ДOП.TAБЛИЦA ДЛЯ *P
012532 :  PUNCH:9,Z34,.O
012533 :  ,UJ,*PO
012534 :  10,Z00,.P
012535 :  ,UJ,*PP
012536 :  ,Z00,
012537 :  ,UJ,*P
012538 : C---   ДOП.TAБЛИЦA ДЛЯ *C
012539 :  COMME:10,Z00,.P
012540 :  ,UJ,*CP
012541 :  ,Z00,
012542 :  ,UJ,*C
012543 : C---   ДOП.TAБЛИЦA ДЛЯ *Y
012544 :  YARD:9,Z34,.O
012545 :  ,UJ,*YO
012546 :  ,Z00,
012547 :  ,UJ,*Y
012548 : C---   ДOП.TAБЛИЦA ДЛЯ *H
012549 :  HEAD:9,Z20,.L
012550 :  ,UJ,*HL
012551 :  ,Z00,
012552 :  ,UJ,SLMIST
012553 : C---   ДOП.TAБЛИЦA ДЛЯ *U
012554 :  UZER:9,Z34,.O
012555 :  ,UJ,*UO
012556 :  ,Z00,
012557 :  ,UJ,*U
012558 : C------TAБЛИЦA PEДAKTИPOBAHИЯ .
012559 :  ONET:8,Z20,1.D
012560 :  ,UJ,*D*1
012561 :  9,Z04,1.I
012562 :  ,UJ,*I*9
012563 :  8,Z10,.B
012564 :  8,UJ,*B*
012565 :  9,Z20,.L
012566 :  8,UJ,*L
012567 :  8,Z14,.C
012568 :  ,UJ,*C*
012569 :  ,OCT,
012570 : C------TAБЛИЦA PEДAKTИPOBAHИЯ /
012571 :  ONE/:8,Z20,1.D
012572 :  8,UJ,*D/1
012573 :  9,Z04,1.I
012574 :  8,UJ,*I/1
012575 :  10,Z30,1.V
012576 :  ,UJ,*V/1
012577 :  9,Z20,.L
012578 :  8,UJ,*L/
012579 :  8,Z10,.B
012580 :  ,UJ,*B/
012581 :  9,Z24,.M
012582 :  ,UJ,*M/
012583 :  8,Z14,.C
012584 :  ,UJ,*C/
012585 :  ,OCT,
012586 : C------ДOП.TAБЛИЦЫ PEД-Я .
012587 : C---   ДOП.TAБЛИЦA ДЛЯ *D
012588 :  *D*1:8,Z04,.A
012589 :  8,UJ,*DA*
012590 :  8,Z10,.B
012591 :  8,UJ,*DB*
012592 :  10,Z04,.Q
012593 :  8,UJ,*DQ*
012594 :  8,Z30,.F
012595 :  8,UJ,*DF*
012596 :  ,Z00,
012597 :  8,UJ,*D*
012598 : C---   ДOП.TAБЛИЦA ДЛЯ *I
012599 :  *I*9:10,Z04,.Q
012600 :  8,UJ,*IQ*
012601 :  8,Z30,.F
012602 :  8,UJ,*IF*
012603 :  ,Z00,
012604 :  8,UJ,*I*
012605 : C------ДOП.TAБЛИЦЫ PEД-Я /
012606 : C---   ДOП.TAБЛИЦA ДЛЯ *D
012607 :  *D/1:8,Z30,.F
012608 :  ,UJ,*DF/
012609 :  8,Z04,.A
012610 :  ,UJ,*DA/
012611 :  8,Z10,.B
012612 :  ,UJ,*DB/
012613 :  ,Z00,
012614 :  8,UJ,*D/
012615 : C---   ДOП.TAБЛИЦA ДЛЯ*V
012616 :  *V/1:8,Z30,.F
012617 :  ,UJ,*DF/
012618 :  8,Z04,.A
012619 :  ,UJ,*VA/
012620 :  8,Z10,.B
012621 :  ,UJ,*VB/
012622 :  ,Z00,
012623 :  ,UJ,*V/
012624 : C---   ДOП.TAБЛИЦA ДЛЯ *I
012625 :  * I/1:8,Z30,.F
012626 :  ,UJ,*IF/
012627 :  ,Z00,
012628 :  8,UJ,*I/
012629 : C      AДPEC OЧEPEДHOЙ KAPTЫ ЗAДAHИЯ
012630 :  ACARD:,,
012631 :  ,Z00,CARD
012632 : C      TAБЛИЦA YПP.KAPT CTAPOГO PEДAKTOPA
012633 :  CARCOM:,ISO,72H
012634 :  ,CONT,12H   *
012635 :  OLDED:0,ISO,18H*INSERT
012636 :  1,ISO,18H*INSERT BELOW
012637 :  2,ISO,18H*DELETE
012638 :  3,ISO,18H*TAKE
012639 :  4,ISO,18H*LAST TAKE
012640 :  ,OCT,.KOHEЦ
012641 :  TEXTA:,ISO,16HPEДAKTOP TEKCTOB
012642 :  TEXTB:,ISO,12H(01.06.82)
012643 :  ,BSS,
012644 : C      PEЗYЛЬTAT PEДAKTИPOBAHИЯ /
012645 :  CARD/:,BSS,12
012646 :  ,ISO,11H
012647 :  ,CONT,1H'12'
012648 :  ,BSS,
012649 : C      CПPABKA ФAЙЛA ЧTEHИЯ
012650 :  DOKYR:0,ISO,6H ФAЙЛ
012651 :  1,OCT,
012652 :  ,ISO,6H''''''
012653 :  2,ISO,12H:CЧИTAHO  ''''
012654 :  4,OCT,
012655 :  5,ISO,6H KAPT
012656 :  6,ISO,6H(''''''''''
012657 :  7,OCT,
012658 :  8,ISO,6H ЗOH)
012659 :  DOKYZ:,ISO,6H ЗOH)
012660 :  DOKYT:,ISO,6H TP.)
012661 :  ERTEXT:0,ISO,20HHEKOPPEKTHAЯ YПPABЛЯ
012662 :  ,CONT,14HЮЩAЯ KAPTA
012663 :  ,OCT,
012664 :   1,ISO,20HФAЙЛ ЗAПИCИ И ФAЙЛ
012665 :  ,CONT,16HЧTEHИЯ COBПAДAЮT
012666 :  ,OCT,
012667 :  2,ISO,12HПYCTOЙ OБPAЗ
012668 :  ,OCT,
012669 :  3,ISO,18HБЫЛA *DELETE FINAL/
012670 :  ,OCT,
012671 :   4,ISO,22HHEПOHЯTHO ЧTO ДEЛATЬ
012672 :  ,CONT,12H(*W,*LL,...)
012673 :  ,OCT,
012674 :  5,ISO,24HHET ИCKOMOГO OБPAЗA
012675 :  ,OCT,
012676 :   6,ISO,20HB ФAЙЛE ЧTEHИЯ HET
012677 :  ,CONT,14HИCKOMOЙ KAPTЫ
012678 :  ,OCT,
012679 :   7,ISO,22HHE YKAЗAH ФAЙЛ ЧTEHИЯ
012680 :  ,OCT,
012681 :  8,ISO,19HБOЛЬШE 72 CИMBOЛOB
012682 :  ,CONT,23HИЛИ HET ИCKOMOГO OБPAЗA
012683 :  ,OCT,
012684 :  9,ISO,24HПEPBЫЙ CИMBOЛ HE ПPOБEЛ
012685 :  ,OCT,
012686 :  10,ISO,24HБЫЛИ OБHAPYЖEHЫ OШИБKИ
012687 :  ,OCT,
012688 :  ,ISO,18HMAЛO MЛ(MБ) ИЛИ
012689 :  11,ISO,24HФAЙЛ ЗAПИCИ БOЛЬШE 2MБ
012690 :  ,OCT,
012691 :  12,ISO,30HПPOЧИTAHA HETEKCTOBAЯ KAPTA
012692 :  ,OCT,
012693 :  13,ISO,48HPEДAKTOP HE ПOHИMAET CT. MACCИBOB И BINARY'41'
012694 :  ,OCT,.
012695 :  TOTCEE:,ISO,24HHET KOHЦA ЗAДAHИЯ (*EE)'41'
012696 :  ,OCT,.
012697 :  15,ISO,42HHEПPABИЛЬHO ЗAДAHЫ ПAPAMETPЫ PEДAKTOPA
012698 :  ,OCT,
012699 :  DOKYW:0,ISO,6H ФAЙЛ
012700 : C      CПPABKA ФAЙЛA ЗAПИCИ
012701 :  1,OCT,
012702 :  ,ISO,6H''''''
012703 :  2,ISO,12H:ЗAПИCAHO ''''
012704 :  4,OCT,
012705 :  5,ISO,6H KAPT
012706 :  6,ISO,6H(''''''''''
012707 :  7,OCT,
012708 :  8,ISO,6H ЗOH)
012709 :  READ OLD:,ISO,83H*READ OLD
012710 :  ,CONT,1H'12'
012711 :  END FILE:,ISO,12H*END FILE '311''012'
012712 :  NUMBERE:,ISO,6H     '012'
012713 :  IDID:,BSS,12
012714 :  ,ISO,12H   **
012715 :  ,BSS,
012716 : C      NEQ=0-БЛOKИPOBKA YПPABЛЯЮЩИX
012717 :  BLYPR:,BSS,1
012718 : C      OЧEPEДHAЯ KAPTA ЗAДAHИЯ
012719 :  CARD:,BSS,14
012720 : C      X-KA KAPTЫ ЗAДAHИЯ
012721 :  XCARD:,BSS,4
012722 :  FNNN:,BSS,1.(YKAZQ)
012723 : C      K-BO CЛEД.HEYПP.KAPT
012724 :  NNN:,BSS,1
012725 : C      AДP.OБPAБOTKИ YПP.KAPTЫ
012726 :  XXX:,BSS,1
012727 : C      OЧEPEДHAЯ KAPTA ИЗ ФAЙЛA ЧTEHИЯ
012728 :  CARDR:,BSS,14
012729 : C      NEQ 0-KAPTA (ИЗ R) B ПAMЯTИ(BEFORE)
012730 :  YESCARD:,BSS,1
012731 : C      NEQ 0-ECTЬ HEЗAK.READ
012732 :  NOENDR:,BSS,1
012733 : C      AДPEC ФAЙЛA ЗAПИCИ
012734 :  AWRITE:,BSS,1
012735 : C      AДPEC ФAЙЛA ЧTEHИЯ
012736 :  AREAD:,BSS,1
012737 : C      AДPEC U-ПOДПPOГPAMMЫ
012738 :  NOWRIT:,BSS,1
012739 :  ZAKAZP:,BSS,1.KOПИЯ *REGIM//
012740 : C     HAЧI COCTOЯHИE MAГAЗИHA
012741 :  MAG:,BSS,1
012742 : C     ЗAKAЗ BOЗBPATA ПPИ OШИБKAX
012743 :  ZAKAZ:,BSS,1
012744 : C     CPCTOЯHИE 20B
012745 :  SJUMP:,BSS,1
012746 : C       ШAГ HYMEPAЦИИ(S-1)
012747 :  STEP:,BSS,1
012748 : C      KOД YПPABЛЯЮЩEГO CИMBOЛA
012749 :  FOR*:,BSS,1
012750 : C      NEQ 0-ПPOГOH ДO *EE
012751 :  PROGON:,BSS,1
012752 : C      =1 *D,=:1 *V
012753 :  DVDV:,BSS,1
012754 : C       ПP. ЗAMEHЫ ПPИ *V/
012755 :  RR1*:,BSS,1
012756 : C      OЧEPEДHOЙ HOMEP(PEД-Я :)
012757 :  NUMBERI:,BSS,2
012758 : C      ЭTAЛOHHЫЙ HOMEP
012759 :  NETAL:,BSS,2
012760 : C      NEQ 0-БЛ-KA *R=*W
012761 :  BL*R*W:,BSS,1
012762 : C      NEQ 0-БЛOK-KA AREAD=CREAD
012763 :  BLARECRE:,BSS,1
012764 :  RR1:,BSS,1
012765 :  RR2:,BSS,1
012766 :  RRG:,BSS,1
012767 : C      ПEPBЫЙ HOMEP HA YПP.KAPTE
012768 :  N1:,BSS,2
012769 : C      BTOPOЙ HOMEP HA YПP.KAPTE
012770 :  N2:,BSS,2
012771 : C      NEQ 0-ЗAПИCЬ ЗAПPEЩEHA(DELETE)
012772 :  ZAP:,BSS,1
012773 :  XID1:,BSS,4
012774 : C      OГPAHИЧEHИE ФAЙЛA ЗAПИCИ
012775 :  AOGR:,BSS,1
012776 : C      ШKAЛA CEPBИCA
012777 : C      1P.=1-*N,2-*NI,3-*LL,
012778 : C      4-*HL,5-*LP,6-*CP,7-*U
012779 :  RABOTA1:,BSS,1
012780 : C      KOПИЯ ШKAЛЫ RABOTA1
012781 :  RABOTA2:,BSS,2
012782 : C      NEQ 0-БЫЛA *BT
012783 :  BLTAKE:,BSS,1
012784 : C      NEQ 0-БЫЛA *TO
012785 :  TOUT:,BSS,1
012786 : C      NEQ 0-БЫЛA *T,*TN
012787 :  YESTAKE:,BSS,1
012788 : C      NEQ 0-HEЗAKOHЧEH TAKE
012789 :  NOENDTA:,BSS,1
012790 : C      ДЛЯ OPГAHИЗAЦИИ ПEЧATИ
012791 :  PRAB:,BSS,15
012792 :  SX1:,BSS,4
012793 :  SX2:,BSS,4
012794 : C      YO1-YO5-PAБ.BEЛИЧИHЫ YZOB
012795 :  YO1:,BSS,1
012796 :  YO2:,BSS,1
012797 :  YO3:,BSS,1
012798 :  YO4:,BSS,1
012799 :  YO5:,BSS,1
012800 :  LTPR:,BSS,1.
012801 :  LTPR1:,BSS,1.KOПИЯ LTPR
012802 :  PFLAG:,BSS,1.=2/3 - БЫЛA *P/*PP
012803 : C      NEQ 0-БЫЛA *Y
012804 :  YAR:,BSS,1
012805 : C      X-KA ИCXOДHOЙ KAPTЫ(PEД-E /)
012806 :  SX1/:,BSS,4
012807 : C      NEQ 0-KOHEЦ ИCXOДHOЙ KAPTЫ
012808 :  Q72:,BSS,1
012809 : C      X-KA PEЗYЛЬTATA PEД-Я /
012810 :  SXR/:,BSS,4
012811 : C      PEЗYЛЬTAT PEДAKTИPOBAHИЯ /
012812 :  SCARD:,BSS,14
012813 : C      NEQ 0-KOHEЦ PEЗYЛЬTATA PEД-Я /
012814 :  W72:,BSS,1
012815 : C      ДЛИHA ФPAГMEHTA(LONG/)
012816 :  LONG:,BSS,1
012817 :  X/CARD:,BSS,4
012818 :  LOBL:,BSS,1.(LONG/)
012819 : C      HAЧATO PEДAKTИPOBAHИE /
012820 :  WWW:,BSS,1
012821 : C      KOПИЯ WWW
012822 :  WWW/:,BSS,1
012823 : C      YPOBEHЬ CKOБOK *(((-*)))
012824 :  YROV:,BSS,1
012825 : C      БЫЛA *DF/
012826 :  WBLAN:,BSS,1
012827 :  YESCARD/:,BSS,1.(*WC)
012828 :  NOENDR/:,BSS,1.(*WC)
012829 :  ZAP/:,BSS,1.(*WC)
012830 :  CARDR/:,BSS,14.(*WC)
012831 :  AREAD/:,BSS,1.(*WC)
012832 : C      NEQ 0-ПP.'HE KAPTA PEЗYЛЬTATA'
012833 :  LLPRI:,BSS,1
012834 : C      =0-ИЩETCЯ NAME,NEQ-END
012835 :  NAEM:,BSS,1
012836 :  NAHL:,BSS,1
012837 : C      ИMЯ C KAPTЫ *TN
012838 :  TN:,BSS,2
012839 : C      ДЛЯ ПEЧATИ CПPABKИ READ
012840 :  FILE:,BSS,2
012841 :  QCAR:,BSS,1
012842 :  QZT:,BSS,1
012843 :  ZT:,BSS,2
012844 : C      ДЛЯ ПEЧATИ CПPABKИ WRITE
012845 :  FILE/:,BSS,2
012846 :  QCAR/:,BSS,1
012847 :  QZT/:,BSS,1
012848 :  ZT/:,BSS,2
012849 : C      ДЛЯ ПЧ.'ИMЯ-NЗOHЫ' (*HL)
012850 :  NAMESUB:,BSS,3
012851 : C      HYЖHA CПPABKA READ(AB.CИTYAЦИЯ)
012852 :  YRE:,BSS,1
012853 :  RRY:,BSS,1
012854 : C      X-KA HEЗAKOHЧEHHOГO TAKE
012855 : C      15-1-NTAK:*T:N1,N2
012856 : C      48=1:*TN
012857 :  XNOENDTA:,BSS,1
012858 :  PRBEF:,BSS,1
012859 :  NTAK:,BSS,2
012860 :  XID:,BSS,4
012861 : C      ПP.'KAPTA ИЗ READ'
012862 : C      48=1-KAPTA *READ OLD(*END FILE)
012863 :  CAREAD:,BSS,1
012864 :  PR111:,BSS,1
012865 :  XREAD//:,BSS,2
012866 : C      =0 ABOCT HE B MISTIKA2
012867 :  MYMY:,BSS,1
012868 :  ,END,
012869 :  EMULATOR:,NAME,
012870 : C    ЗAKAЗ HA ИHTEPПPETAЦИЮ ЭKCTPAKOДOB
012871 :  OUT*OUT:,LC,1
012872 :  *C:,ITS,13
012873 :  ,ITS,7
012874 :  ,ITS,6
012875 :  7,BASE,*C
012876 :  ,XTS,=:4
012877 :  ,UTC,OUT*OUT
012878 :  ,ATX,
012879 :  ,XTA,
012880 :  ,ATX,SCAL
012881 :  ,MTJ,9
012882 :  ,MTJ,10
012883 :  ,CALL,PARAMT*
012884 :  PERR:,BSS,
012885 :  ,XTA,=62
012886 :  ,ATX,PN
012887 :  ,ATX,PK
012888 :  11,VZM,PUT
012889 :  *LOOP:9,VTM,PN
012890 :  10,VTM,5
012891 :  ,NTR,6
012892 :  ,CALL,PARAMT*
012893 :  14,VIM,ERR
012894 :  ,ATX,PK
012895 :  ,NTR,3
012896 :  ,A-X,=100
012897 :  ,UZA,ERR
012898 :  ,A+X,=30
012899 :  ,UIA,ERR
012900 :  11,UTM,-55B
012901 :  11,VIM,OWN
012902 :  9,VTM,PK
012903 :  10,VTM,5
012904 :  ,CALL,PARAMT*
012905 :  14,VIM,ERR
012906 :  ,NTR,3
012907 :  ,A-X,=100
012908 :  ,UZA,ERR
012909 :  ,A+X,=30
012910 :  ,UIA,ERR
012911 :  PUT:11,UTM,-55B
012912 :  11,VZM,ERR
012913 :  OWN:,NTR,3
012914 :  ,XTA,PK
012915 :  ,A-X,PN
012916 :  ,UIA,ERR
012917 :  ,ATI,6
012918 :  ,XTA,=:4
012919 :  ,WTC,PN
012920 :  ,ASN,64-50B
012921 :  *Z:15,ATX,
012922 :  ,AOX,SCAL
012923 :  ,STX,SCAL
012924 :  ,ASN,64+1
012925 :  6,VZM,BCE
012926 :  6,UTM,-1
012927 :  ,UJ,*Z
012928 :  BCE:11,UTM,55B
012929 :  11,VZM,END
012930 :  11,UTM,-54B
012931 :  11,VZM,*LOOP
012932 :  END:,XTA,SCAL
012933 :  ,*57,5
012934 :  ,STI,
012935 :  ,STI,6
012936 :  ,STI,7
012937 :  ,STI,13
012938 :  ,NTR,6
012939 :  13,UJ,
012940 :  ERR:,MTJ,11
012941 :  ,*64,INF
012942 :  ,UJ,PERR
012943 :  INF:,,ETX
012944 :  ,,ETX
012945 :  :,001,
012946 :  8,,
012947 :  ETX:,GOST,24HOШИБKA B ИHФOPMAЦИИ '133''231'
012948 :  PN:,BSS,1
012949 :  PK:,BSS,1
012950 :  SCAL:,BSS,1
012951 :  ,END,EMULATOR
012952 :  EXECUT:,NAME,.CAXAPOB Б.Б.
012953 :  MONITOR*:,SUBP,.OTKAЗ OT
012954 :   /VERE/:LC,BLOCK,SAVL,(4)
012955 :  RM*:,LC,.HEHYЖHЫX
012956 :  ERROFLG*:,LC,.TPAKTOB MБ.
012957 :  LOADLIS*:,LC,.
012958 :  MAIN*   :,LC,.
012959 :  KCOUNT* :,LC,.
012960 :  LHEAP*:,LC,
012961 :  STOP*:,SUBP,
012962 :   LOADANGO:,SUBP,
012963 :   EXPOINT:,EQU,17B
012964 :   GOAFLOAD:,EQU,LOADANGO+EXPOINT
012965 : C********************
012966 : C   KAK ПOKAЗAЛ OПЫT,
012967 : C   ДOБABЛEHИE HOBЫX
012968 : C   OБ'EKTOB K PEЗИДEHTY
012969 : C   MOHИTOPA  Ч P E B A T O .
012970 : C*****************************
012971 :  BLEXPRIN:,SUBP,.ДИAГHOCTИKA
012972 :  *C:,BSS,
012973 :  13,VTM,STOP*
012974 :  ,XTA,ERROFLG*
012975 :  ,UIA,BLEXPRIN.БЫЛИ OШИБKИ
012976 :  DISP: 1,IFSEQ,D,DD**
012977 :   ,*76,.OTKAЗ Э076
012978 :  1,BASE,*C
012979 :  ,XTA,SAVL
012980 :  ,UZA,NO RES
012981 :  ,ATX,73B
012982 :  ,XTA,
012983 :  ,ATX,SAVL
012984 :  NO RES:,BSS,
012985 :  ,XTA,KCOUNT*+1
012986 :  ,ASN,64+12.
012987 :  ,AAX,=77.YCTPOЙCTBO.
012988 :  ,ATI,14
012989 :   ,XTA,=:6
012990 :  14,ASN,64
012991 :  ,AOX,LHEAP*
012992 :  ,AAX,CONDRU
012993 :  ,AEX,CONDRU
012994 :  ,UZA,EX1
012995 :  ,ATX,CONDRU
012996 :  ,AAX,=:7777 7577
012997 :  ,ASN,64+6
012998 :  ,AOX,=:41
012999 :  ,ATX,SYM
013000 :  ,XTA,GOAFLOAD
013001 :  ,ATX,EOT
013002 :  13,VTM,BOT-EOT
013003 :  ,XTA,44B
013004 :  ,ASN,64+15
013005 :  ,ATI,12
013006 :  ,ITA,12
013007 :  ,ASN,64-24
013008 :  ,ARX,K1
013009 :  ,CTX,GOAFLOAD
013010 :  PUT:13,XTA,EOT
013011 :  13,UTC,EOT-BOT
013012 :  12,CTX,200B
013013 :  13,VLM,PUT
013014 :  EX1:,XTA,RM*
013015 :  ,ATX,16B
013016 :  ,XTA,RM*+1
013017 :  ,ATX,17B
013018 :  ,WTC,RM*+2.MAГAЗИH ПPOДBИHYT
013019 :  15,VTM,2.CПEЦИAЛЬHO (ДЛЯ FTN)
013020 :  ,NTR,2
013021 :  ,XTA,RM*+3
013022 :  ,ATX,44B
013023 :  ,XTA,RM*+4
013024 :  ,ATX,47B
013025 :  ,XTA,LOADLIS*
013026 :  ,UIA,BLP.ЗAГPYЗKY
013027 :  ,XTA,44B.HE ПEЧATATЬ
013028 :  ,ASN,64-1
013029 :  ,ASN,64+1.ПEЧATATЬ.
013030 :  ,ATX,44B
013031 :  BLP:,BSS,
013032 :  ,XTS,MAIN*
013033 :  ,ITS,
013034 :  ,UJ,MONITOR*+3.ПOШЛИ
013035 : C               HA ЗAГPYЗKY
013036 :  FULFILME:,ENTRY,
013037 :  1,VTM,*C
013038 :  ,XTA,
013039 :  ,ATX,CONDRU
013040 :  ,UJ,*C
013041 :  K1:12,VTM,200B
013042 :  12,UJ,
013043 :   BOT:12,REL,*
013044 : C
013045 : C...   OTKAЗ OT MБ, ЗAДAHHЫX
013046 : C      БИTOBOЙ ШKAЛOЙ   S Y M
013047 : C
013048 :   FIX20:,XTA,CONDRU
013049 :   ,AAX,MБ21
013050 :   ,UZA,TEMPO
013051 :   ,XTA,43B
013052 :   ,ASN,64-20
013053 :   ,AOX,IR
013054 :   ,ATX,47B
013055 :   ,ATX,60B
013056 :   TEMPO:,NTR,6
013057 :  DISP:,IFSEQ,  D, ND70
013058 :   ,XTA,C37
013059 :   ,ATX,TPAK
013060 :   ,XTA,B43
013061 :   REFMD:,ATX,BIT
013062 :   ,AAX,SYM
013063 :   ,UZA,NEXT
013064 :   ,*52,REFTR
013065 :   NEXT:,XTA,TPAK
013066 :   ,ARX,T1
013067 :   ,ATX,TPAK
013068 :   ,XTA,BIT
013069 :   ,ASN,64+19
013070 :   ,UZA,RET
013071 :   ,ASN,64-18
013072 :   ,UJ,REFMD
013073 :   B43:,OCT,004
013074 :   C37:,LOG,37
013075 :   T1 :,LOG,40 0000 0040
013076 :   BIT:,LOG,
013077 :   REFTR20:,LOG,30 0000 0000 0001
013078 :   ,LOG,1024 0000 1037
013079 :   REFTR:,LOG,30 0000 0000 0001
013080 :   TPAK:,LOG,
013081 :  DISP:  ,ELSE,
013082 :   ,*72,SYM
013083 :  DISP:  ,ENDIF,
013084 :   RET:,XTA,EOT
013085 :   ,CTX,GOAFLOAD
013086 :   ,MTJ,12
013087 :   ,UJ,GOAFLOAD
013088 : C
013089 : C
013090 : C
013091 : C    ECЛИ ФИPMA HE ЖEЛAET, ЧTOБЫ OT
013092 : C    KAKИX-ЛИБO MБ ПPOИЗBOДИЛCЯ OTKAЗ,
013093 : C    TO CЛEДYET HA MБ 21037 ИCПPABИTЬ
013094 : C    ЯЧEЙKY 516B (LHEAP*) :
013095 : C    БИT 48 - БЛOKИP.OTKAЗA OT MБ 00, ...
013096 : C    БИT 25 -  --"--  --"--    MБ 27.
013097 : C    1-15 P. LHEAP* TPOГATЬ HEЛЬЗЯ !
013098 : C----------------------------------------
013099 : C
013100 :   CONDRU:,OCT,7777 7777
013101 :   SYM:,LOG,
013102 :   MБ20:,OCT,0000 02
013103 :   MБ21:,OCT,0000 01
013104 :   IR:,LOG,14 0000 0021 0007
013105 :   EOT:,LOG,
013106 :  ,END,EXECUT
013107 :       F/START    :,NAME,
013108 :       F/FSTART  :,ENTRY,
013109 :             11 ,  VTM,   *00001
013110 :             12 ,  VIM,   *00021
013111 :  *00001    : 7 ,  ATX,
013112 :                  , UJ,   *00013
013113 :       F/SNOSV   :,ENTRY,
013114 :             11 ,  VTM,   *00003
013115 :             12 ,  VIM,   *00021
013116 :  *00003    : 7 ,  ATX,
013117 :             15 ,  XTA,
013118 :       F/NOSVR   :,ENTRY,
013119 :  *00004    :     ,ITS,  13
013120 :                  ,ITS,  8
013121 :             15 ,  ATX,
013122 :                  ,NTR,  2
013123 :             10 ,   UJ,
013124 :                  ,UTC,
013125 :       F/FNOSV   :,ENTRY,
013126 :             11 ,  VTM,   *00010
013127 :             12 ,  VIM,   *00021
013128 :  *00010    : 7 ,  ATX,
013129 :                  , UJ,   *00004
013130 :       F/SSTART  :,ENTRY,
013131 :             11 ,  VTM,   *00012
013132 :             12 ,  VIM,   *00021
013133 :  *00012    : 7 ,  ATX,
013134 :             15 ,  XTA,
013135 :       F/SAVER   :,ENTRY,
013136 :  *00013    :     ,ITS,  13
013137 :                  ,ITS,  8
013138 :                  ,ITS,  1
013139 :                  ,ITS,  2
013140 :                  ,ITS,  3
013141 :                  ,ITS,  4
013142 :                  ,ITS,  5
013143 :                  ,ITS,  6
013144 :             15 ,  ATX,
013145 :                  ,NTR,  2
013146 :             10 ,   UJ,
013147 :                  ,UTC,
013148 :  *00021    :12 ,  UTM,  1
013149 :                  ,UTC,
013150 :  *00022    : 7 ,  ATX,
013151 :             15 ,  XTA,
013152 :              7 ,  UTM,  77777B
013153 :             12 ,  VLM,   *00022
013154 :             11 ,   UJ,
013155 :                  ,UTC,
013156 :       F/FRETNR  :,ENTRY,
013157 :             15 ,  ATX,  77775B
013158 :                  , UJ,   *00033
013159 :       F/FRET    :,ENTRY,
013160 :             15 ,  ATX,  77767B
013161 :                  ,UTC,
013162 :       F/SRET    :,ENTRY,
013163 :             15 ,  XTA,
013164 :                  ,STI,  6
013165 :                  ,STI,  5
013166 :                  ,STI,  4
013167 :                  ,STI,  3
013168 :                  ,STI,  2
013169 :                  ,ATI,  1
013170 :                  ,UTC,
013171 :       F/SRETNR  :,ENTRY,
013172 :  *00033    :15 ,  XTA,
013173 :                  ,STI,  7
013174 :                  ,STI,  13
013175 :                  ,NTR,  6
013176 :             13 ,   UJ,
013177 :                  ,UTC,
013178 :  BSS       :     ,BSS,
013179 :       ,END,
013180 :  FICMEMOR:,NAME,.  TPOИЦK 1983
013181 :  RM*:,LC,.MOЖHO MEHЯTЬ HAЧAЛO MAГAЗИHA
013182 :   MONCARD/:,LC,1
013183 :  INFLOAD*:LC,BLOCK,(1)
013184 :  ,CONT,INFLOA
013185 :  ,CONT,INFMD
013186 :  ,CONT,INFDM
013187 :  STATIC*:,LC,1.
013188 :  RM/:,LC,2.
013189 :  *C: ,UTC,73400B-55400B.
013190 :  FULLMEMO:,ENTRY,
013191 :  ,UTC,55400B-75400B.
013192 :  ALLMEMOR:,ENTRY,
013193 :  12,VTM,75400B.
013194 :  1,BASE,*C
013195 :  ,ITA,12.
013196 :  ,ATX,CELOAD.
013197 :  ,ITA,14.
013198 :  ,ATX,R14.
013199 :  ,ITA,13.
013200 :  ,ATX,R13.
013201 :  10,VTM,.
013202 :   ,XTA,MONCARD/
013203 :   ,AEX,=6H*SOS
013204 :   ,UZA,NO/PARAM. - ГOHЧAPOB ЭTOГO HE ЛЮБИT !
013205 :  ,CALL,PARAMT*.
013206 :  11,VZM,NO/PARAM.HAЧAЛO MAГAЗИHA HE ЗAДAHO
013207 :  10,VTM,5.BOCЬM. ЧИCЛO
013208 :  9,VTM,CELOAD.
013209 :  ,CALL,PARAMT*.
013210 :  NO/PARAM:,WTC,CELOAD
013211 :  12,VTM,-1.
013212 :  ,ITA,12.
013213 :  ,ATX,CELOAD.KOHEЦ P OБЛACTИ
013214 :  ,ARX,=1.
013215 :  15,ATX,.
013216 :  ,ARX,=1.
013217 :  ,ATX,CMAГ.HAЧAЛO MAГAЗИHA
013218 :  ,ARX,DEBIN.
013219 :  ,STX,DINF.ДЛЯ ЭK *67
013220 :  ,ASN,64-15.
013221 :  ,ATX,DMAГ.ДHO MAГAЗИHA
013222 :  ,XTA,CMAГ.
013223 :  ,ARX,=20.MAГAЗИH HE MEHEE 20  CЛOB.
013224 :  15,ATX,.
013225 :  ,APX,=76000.HOMEP ЛИCTA MAГAЗИHA
013226 :  ,AUX,=:0000 37
013227 :  ,AOX,=:1140 00 00 77
013228 :  ,STX,LINF.
013229 :  ,APX,=77400.
013230 :  ,AUX,=0000 3703 0000 0000
013231 :  ,ATX,PAGE.
013232 :  13,VTM,INFMD.
013233 :  12,VJM,CHANGE.
013234 :  13,VTM,INFDM.
013235 :  12,VJM,CHANGE.
013236 :  13,VTM,INFLOA.
013237 :  13,XTA,.
013238 :  ,UZA,STAT.
013239 :  12,VJM,CHANGE.
013240 :  13,VTM,STATIC*.
013241 :  13,XTA,.
013242 :  ,UZA,STAT.
013243 :  12,VJM,CHANGE.
013244 :  STAT:,WTC,R14.
013245 :  14,VTM,.
013246 :  14,XTA,1.
013247 :  ,AAX,=:7777 7777 7770
013248 :  ,AOX,CELOAD.
013249 :  14,ATX,1.
013250 :  14,XTA,5.
013251 :  ,AAX,=:7777 7777 7770
013252 :  ,AOX,CELOAD.
013253 :  14,ATX,5.
013254 :  ,XTA,16B.
013255 :  ,AAX,=:7777 7777 7770
013256 :  ,AOX,CELOAD.
013257 :  ,ATX,16B.
013258 :  ,XTA,RM/
013259 :  ,AAX,=:7777 7777 7770
013260 :  ,AOX,CELOAD.
013261 :  ,ATX,RM/.
013262 :  ,XTA,RM*.
013263 :  ,AAX,=:7777 7777 7770
013264 :  ,AOX,CELOAD.
013265 :  ,ATX,RM*.
013266 :  ,XTA,CMAГ.
013267 :  ,ATI,15.
013268 :  ,ATX,RM*+2.
013269 :  ,XTA,44B.
013270 :  ,AAX,=777777 00000 77777
013271 :  ,AOX,DMAГ.
013272 :  ,ATX,44B.
013273 :  ,XTA,RM*+3.
013274 :  ,AAX,=777777 00000 77777
013275 :  ,AOX,DMAГ.
013276 :  ,ATX,RM*+3.
013277 :  DISP:1,IFSEQ,D,DD**
013278 :  ,*72,LINF.ЗAKAЗ ЛИCTOB MOЗY
013279 :  ,XTA,17B.
013280 :  ,ATX,R14.
013281 :  ,XTA,M67.
013282 :  ,CTX,17B
013283 :  ,UJ,17B
013284 :  M67:,*67,DINF.
013285 :  EXIT:,XTA,R14.
013286 :  ,ATX,17B.
013287 :  ,WTC,R13.
013288 :  ,UJ,.BOЗBPAT.
013289 :  CHANGE:13,XTA,.
013290 :  ,AAX,=7777 4074 7777 7777
013291 :  ,AOX,PAGE.
013292 :  13,ATX,.
013293 :  12,UJ,.
013294 :  DEBIN:,Z00,EXIT.
013295 :  8,Z37,77777B.OC ПO AЧ
013296 :  LINF:,BSS,1.
013297 :  DINF:,BSS,1.
013298 :  PAGE:,BSS,1.
013299 :  CELOAD:,BSS,1.
013300 :  CMAГ:,BSS,1.
013301 :  DMAГ:,BSS,1.
013302 :  R13:,BSS,1.
013303 :  R14:,BSS,1.
013304 :  ,END,FICMEMOR.
013305 :  FIX DRUM:,NAME,.CAXAPOB ББ
013306 : C    БЛOKИPOBKA OTKAЗA OT MБ.
013307 :  OUT*OUT:,LC,1
013308 :  LHEAP* :,LC,1
013309 :  PARAMT*:,SUBP,
013310 :   MONCARD/:,LC,
013311 :   *C:14,VTM,FIX
013312 :   ,UJ,BEG
013313 :   BEG:,ITS,13
013314 :     ,ITS,7
013315 :     ,ITS,6
013316 :   ,ITS,5
013317 :  7,BASE,*C
013318 :     ,XTS, =:4
013319 :  ,UTC,OUT*OUT
013320 :  ,ATX,
013321 :   14,UJ,
013322 :   FIX:5,VTM,
013323 :   6,VTM,EDR
013324 :   BEGP:,MTJ,10
013325 :   13,VJM,PARAMT*
013326 :   11,VZM,ERR
013327 :  *LOOP:9,VTM,PN
013328 :  10,VTM,5
013329 :  ,NTR,6
013330 :  13,VJM,PARAMT*
013331 :  14,VIM,ERR
013332 :  ,ATX,PK
013333 :  ,NTR,3
013334 :  ,A-X,=30
013335 :  ,UZA,ERR
013336 :  11,UTM,-55B
013337 :  11,VIM,OWN
013338 :   9,VTM,PK
013339 :  10,VTM, 5
013340 :  ,NTR,6
013341 :  13,VJM,PARAMT*
013342 :  14,VIM,ERR
013343 :  ,NTR,3
013344 :  ,A-X,=30
013345 :  ,UZA,ERR
013346 :  PUT:11,UTM,-55B
013347 :  11,VZM,ERR
013348 :  OWN:,NTR,3
013349 :  ,XTA,PK
013350 :  ,A-X,PN
013351 :  ,UIA,ERR
013352 :   ,ATI,14
013353 :  ,XTA,=:4
013354 :  ,WTC,PN
013355 :   5,ASN,64
013356 :  *Z:15,ATX,
013357 :   ,AOX,SCAL
013358 :   ,STX,SCAL
013359 :  ,ASN,64+1
013360 :   14,VZM,BCE
013361 :   14,UTM,-1
013362 :  ,UJ,*Z
013363 :  BCE:11,UTM,55B
013364 :  6,UTC,.
013365 :  11,VZM,.
013366 :  11,UTM,-54B
013367 :  11,VZM,*LOOP
013368 :   6,UJ,
013369 :   EDR:,XTA,SCAL
013370 :   ,AAX,=:7777 7777
013371 :   ,AOX,LHEAP*
013372 :   ,ATX,LHEAP*
013373 :   END:,XTA,
013374 :   ,STX,SCAL
013375 :   ,STI,5
013376 :  ,STI,6
013377 :  ,STI,7
013378 :  ,STI,13
013379 :  ,NTR,6
013380 :  13,UJ,
013381 : C
013382 : C---------
013383 : C
013384 :    TAKETAPE:,ENTRY,
013385 : C
013386 :   14,VJM,BEG
013387 :   ,XTA,MONCARD/+1
013388 :   ,AEX,=6HTAPE
013389 :   ,ASN,64+16
013390 :   ,UZA,TAPE
013391 :   ,MTJ,5
013392 :   6,VJM,BEGP
013393 :   ,XTA,SCAL
013394 :   ,ASN,64+6
013395 :   ,AOX,=:41
013396 :   ,ATX,SCAL
013397 :  DISP: 1,IFSEQ,D,DD**
013398 :   ,*72,SCAL.OTKAЗ MБ
013399 :   ,UJ,END
013400 :  TAPE:,BSS,.ЛEHTA
013401 :  5,VTM,-30B.
013402 :   ,XTA,=70
013403 :   ,ATX,=30
013404 :   6,VJM,BEGP
013405 :  14,VTM,30B.OЙ, OЙ ...
013406 :   ,ITA,14
013407 :   ,ATX,=30
013408 :   ,XTA,SCAL
013409 :  DISP: 1,IFSEQ,D,DD**
013410 :   ,*57,4000B.OTKAЗ MЛ
013411 :   ,UJ,END
013412 : C    ИЗMEHEHИE ДЛИHЫ CTPOЧHOГO HAKOПИTEЛЯ.
013413 :  *HEAP*PL:,ENTRY,
013414 :   14,VJM,BEG
013415 :   ,XTA,=400
013416 :  ,ATX,LH
013417 :   9,VTM,
013418 :  9,MTJ,10
013419 :  13,VJM,PARAMT*
013420 :  11,VZM,STAND
013421 :  9,VTM,LL
013422 :  10,VTM,5
013423 :  13,VJM,PARAMT*
013424 :  14,VIM,RET ERR
013425 :  ,XTA,LL
013426 :  ,AAX,=7 7777
013427 :  ,ATX,LH
013428 :  STAND:,XTA,LHEAP*
013429 :  ,ASN,64+15
013430 :  ,ASN,64-15
013431 :  ,AOX,LH
013432 :  ,ATX,LHEAP*
013433 :  ,UJ,END
013434 :   ERR:,BSS,
013435 :  RET ERR:,*64,INF
013436 :  ,UJ,END
013437 :  INF:,,ETX
013438 :  ,,ETX
013439 :  :,001,
013440 :  8,,
013441 :   SCAL:,LOG,
013442 :  ETX:,GOST,22HOШИБKA B ИHФOPMAЦИИ '133''231'
013443 :  LL:,BSS,1
013444 :  LH:,BSS,1
013445 :  PN:,BSS,1
013446 :  PK:,BSS,1
013447 :  ,END,FIX DRUM
013448 :  FLOAT:,NAME,.TPOИЦK
013449 :  R*TO*INT:,SUBP,.
013450 :  R*TO*I:,SUBP,.
013451 :  A/I0:,LC,1.
013452 :  ,ATI,14.
013453 :  14,XTA,.
013454 :  ,NTR,18.
013455 :  ,E+N,64
013456 :  13,UJ,.
013457 :  ABS:,ENTRY,.
013458 :  ABS:,BSS,.
013459 :  ,ATI,14.
013460 :  14,XTA,.
013461 :  ,NTR,18.
013462 :  14,AVX,.
013463 :  13,UJ,.
013464 :  IABS:,ENTRY,.
013465 :  13,MTJ,12
013466 :  13,VJM,ABS.
013467 :  12,MTJ,13.
013468 :  ,UJ,R*TO*INT.
013469 :  IFIX:,ENTRY,.
013470 :  ,ATI,14
013471 :  14,XTA,.
013472 :  ,NTR,18.
013473 :  ,UJ,R*TO*I.
013474 :  AINT:,ENTRY,.
013475 :  13,MTJ,12.
013476 :  13,VJM,INT.
013477 :  ,E+N,64.
013478 :  12,UJ,.
013479 :  INT:,ENTRY,.
013480 :  INT:,BSS,.
013481 :  ,ATI,14.
013482 :  14,XTA,
013483 :  ,NTR,18.
013484 :  ,UJ,R*TO*INT.
013485 :  SQRT:,ENTRY,
013486 :  12,VTM,.
013487 :  ,UJ,*50.
013488 :  SIN:,ENTRY,.
013489 :  12,VTM,1.
013490 :  ,UJ,*50.
013491 :  COS:,ENTRY,.
013492 :  12,VTM,2.
013493 :  ,UJ,*50.
013494 :  ATAN:,ENTRY,.
013495 :  12,VTM,3.
013496 :  ,UJ,*50.
013497 :  ALOG:,ENTRY,.
013498 :  ALOG:,BSS,.
013499 :  12,VTM,5.
013500 :  ,UJ,*50
013501 :  EXP:,ENTRY,.
013502 :  12,VTM,6.
013503 :  ,UJ,*50.
013504 :  *50:,ATI,14.
013505 :  14,XTA,.
013506 :  ,NTR,18.
013507 :  12,*50,.
013508 :  13,UJ,.
013509 :  ALOG10:,ENTRY,.
013510 :  13,MTJ,11.
013511 :  13,VJM,ALOG.
013512 :  ,UTC,=4111 1535 4335 6652
013513 :  ,A/X,.
013514 :  11,UJ,.
013515 :  ,END,FLOAT  .
000001 :  FM*007:,NAME,
000002 :  A/I0:,LC,
000003 :  IOCONT*:,SUBP,
000004 :  ,ATI,14.KOЛ-BO YMHOЖ.
000005 :  ,ASN,64+15
000006 :  ,ATI,12.ПPИЗHAK DOUBLE
000007 :  ,ASN,64+15
000008 :  ,AOX,A/I0
000009 :  ,NTR,2
000010 :  MULT:14,VZM,EMULT
000011 :  14,UTM,-1
000012 :  15,WTC,
000013 :  ,A*X,
000014 :  EMULT:14,V1M,MULT
000015 :  ,NTR,3
000016 :  ,A+X,A/I0
000017 :  ,AEX,A/I0
000018 :  12,VZM,TO/IOCON
000019 :  ,ASN,64-1
000020 :  TO/IOCON:15,ARX,-1
000021 :  ,ATI,14
000022 :  14,UTM,-1
000023 :  ,ITA,14
000024 :  ,UJ,IOCONT*
000025 :  ,END,
000026 :   FMRTAPE:,NAME,
000027 : C
000028 : C*******************************************
000029 : C*                                         *
000030 : C*          ------------------             *
000031 : C*          I                I             *
000032 : C*          I  ,XTS, A C C   I             *
000033 : C*          I  ,CALL,FMRTAPE I             *
000034 : C*          I                I             *
000035 : C*          ------------------             *
000036 : C*                                         *
000037 : C*  ПOДПPOГPAMMA HИЖHEГO YPOBHЯ ДЛЯ        *
000038 : C*  ФOPTPAHHЫX OПEPATOPOB  READ/WRITE.     *
000039 : C*                                         *
000040 : C*  '1' B 48 PAЗPЯДE  A C C  - ПPИЗHAK     *
000041 : C*  BЫXOДA HA HAЧAЛO HOBOГO PEKOPДA        *
000042 : C*  ПPИ ЭTOM OCTAЛЬHЫE PAЗPЯДЫ ACC TAKOBЫ: *
000043 : C*       1 - 6  ЛOГ. HOMEP YCTPOЙCTBA      *
000044 : C*        47    END FILE                   *
000045 : C*        46    REWIND                     *
000046 : C*        45    BACKSPACE                  *
000047 : C*        44    WRITE /YCTAHOBOЧHЫЙ BXOД/  *
000048 : C*        43    READ  /YCTAHOBOЧHЫЙ BXOД/  *
000049 : C*        42    SCHEOF                     *
000050 : C*                                         *
000051 : C*  '0' B 48 P.  A C C  - ПPИЗHAK PAБOTЫ   *
000052 : C*  C MACCИBOM ИЗ TEKYЩEГO PEKOPДA.        *
000053 : C*  ПPИ ЭTOM OCTAЛЬHЫE PAЗPЯДЫ ACC TAKOBЫ: *
000054 : C*      1 - 15  AДPEC HAЧAЛA MACCИBA       *
000055 : C*     25 - 39  AДPEC KOHЦA  MACCИBA       *
000056 : C*       40     OБMEH ИHФOPMAЦИEЙ          *
000057 : C*       41     C ПPOBEPKOЙ HA KOД  012    *
000058 : C*       42     C ЗAKPЫTИEM PEKOPДA        *
000059 : C*                                         *
000060 : C*  '0' BO BCEX PAЗPЯДAX  A C C  - BXOД    *
000061 : C*      ИЗ  STOP*  /ДOCБPOC БYФEPOB/.      *
000062 : C*                                         *
000063 : C*******************************************
000064 : C
000065 : C
000066 :   *ICHECK*:,LC,6.CBЯЗЬ C USEROM
000067 :   *0:B,BLOCK,CONTROL.PEAKЦИЯ HA OШИБKИ
000068 :   ,CONT,ZONE.TEKYЩAЯ ЗOHA
000069 :   ,CONT,FLER.HOMEP OШИБKИ
000070 :   ,CONT,BAD READ.ЧTEHИE ПO 'KO'
000071 : C
000072 : C
000073 :   RFMRTFLG:LC,BLOCK,PEX BOSS.BЛAДEЛEЦ OБM.ЛИCTA
000074 :   TAPELIST:LC,BLOCK,BUFFO.Ф-ЛИCTЫ YCTPOЙCTB
000075 :   ,CONT,SC/UNIT.CTATYCЫ YCTPOЙCTB
000076 :   ,CONT,SC/EC.CTATYCЫ YCTPOЙCTB.
000077 :   ,CONT,DRUM ZON.TEK.ЗOHA FTAPE 16
000078 : C
000079 : C   BUFFO: 16 ПOЛEЙ ИЗ 3 БИTOB /YCTP.16-1/
000080 : C         B KAЖДOM ПOЛE M.Б. HOMEP ФOPMAЛЬHOГO
000081 : C         ЛИCTA ДЛЯ COOTB.YCTP-BA. /N=1,..,7/
000082 : C
000083 : C   SC/UNIT: 3 ПOЛЯ ИЗ 16 БИT /YCTP.1-16/
000084 : C         ПOЛE 1: YCTPOЙCTBO B PAБOTE.
000085 : C         ПOЛE 2: ПPИЗHAK OШИБKИ
000086 : C         ПOЛE 3: ECTЬ OПPOC KAЧECTBA OБMEHA
000087 : C
000088 : C   SC/EC: 3 ПOЛЯ ИЗ 16 БИT /YCTP.1-16/
000089 : C         ПOЛE 1: ПPИЗHAK MЛ EC
000090 : C         ПOЛE 2: PEЗEPB
000091 : C         ПOЛE 3: PEЗEPB.
000092 : C
000093 :   P EXCH:,WEQ,43B.OБMEHHЫЙ ЛИCT
000094 :   NP:,P/P,(P EXCH)16
000095 :   STOP*:,SUBP,
000096 :   A/I0:,LC,1
000097 : C
000098 : C
000099 : C****** ИHДEKC-PEГИCTPЫ ******
000100 : C
000101 :   *1:B,BLOCK,B.БAЗOBЫЙ
000102 :   ,CONT,C.*ICHECK*
000103 :   ,CONT,Z.TEK.ЗOHA
000104 :   ,CONT,LR(0)
000105 :   ,CONT,REC.TEK.PEKOPД /0,1,../
000106 :   ,CONT,AR .AДP.ПO ФИЗ.PEK./-23,..,0/
000107 :   ,CONT,FR .ФЛAГ ЧTEHИЯ ЗOHЫ+1
000108 :   ,CONT,FW .ФЛAГ ЗAПИCИ TEK.ЗOHЫ
000109 :   ,CONT,A.AДPEC ПO БYФEPY /10,36,..,998/
000110 :   ,CONT,T.HOMEP YCTPOЙCTBA
000111 :   ,CONT,E.HOMEP OШИБKИ
000112 :   ,CONT,ER(0).N OШ.ДЛЯ П/П ДИAГHOCTИKИ
000113 :   ,CONT,AM.TEK.AДP.ПO MACCИBY
000114 :   ,CONT,RR
000115 :   ,CONT,R
000116 : C
000117 : C*** PEГИCTPЫ  Z-T  XPAHЯTCЯ B HAЧAЛE ЛИCTA ***
000118 : C
000119 : C=====================================
000120 :   START:,UTC,ACC
000121 :   ,ATX,
000122 :   ,ITA,14
000123 :   14,VTM,-R
000124 :   SAVR:14,ITS,R+1
000125 :   14,VLM,SAVR
000126 :   ,RTE,177B
000127 :   B,BASE,START.***
000128 :   ,XTS,ACC
000129 :   C,VTM,*ICHECK*
000130 :   ,UZA,SWOP ALL
000131 :   ,ARX,
000132 :   R,VTM,CONTINUE
000133 :   ,U1A,SET REG
000134 :   ,ATI,T
000135 :   ,ASN,64-1
000136 :   ,ANX,AMSW
000137 :   ,ATI,R
000138 :   ,XTA,
000139 :   ,ATX,LEN
000140 : C***
000141 :   GIVE PAGE:,BSS,
000142 :   ,ITA,T
000143 :   ,ATX,NEW BOSS
000144 :   T,MTJ,14
000145 :   14,J+M,14
000146 :   T ,J+M,14.T*3
000147 :   ,XTA,=:4
000148 :   T,ASN,63
000149 :   ,ATX,B/UNIT
000150 :   ,ASN,64-15
000151 :   T,MTJ,RR
000152 :   RR,UTM,40B
000153 :   ,UZA,TAPE
000154 :   RR,VTM,16B
000155 :   ,XTA,=7 0000
000156 :   ,ARX,DRUM ZON
000157 :   ,ATX,NEW BOSS
000158 :   TAPE:,ITA,RR
000159 :   ,ASN,64-12
000160 :   ,AOX,NP31/35
000161 :   ,ATX,IR70
000162 :   ,XTA,BUFFO
000163 :   14,ASN,64-3
000164 :   ,AAX,=7
000165 :   ,ATX,NFOLIS
000166 :   ,U1A,CHANGE
000167 : C***
000168 :   ,XTA,PEX BOSS
000169 :   ,AEX,NEW BOSS
000170 :   ,UZA,OWN PAGE
000171 : C***
000172 :   ,XTA,PEX BOSS
000173 :   ,ARX,IW SAV
000174 :   ,ATX,WS
000175 :   ,*70,WS
000176 : C***
000177 :   ,XTA,NEW BOSS
000178 :   ,ATX,PEX BOSS
000179 :   ,ARX,IR SAV
000180 :   ,ATX,WS
000181 :   ,UJ,FREEPAGE
000182 : C***
000183 :   CHANGE:,ASN,64-36
000184 :   ,AOX,IW72
000185 :   ,ATX,NFOLIS
000186 :   ,*72,NFOLIS
000187 : C***
000188 :   FREEPAGE:,XTA,SC/UNIT
000189 :   ,AAX,B/UNIT
000190 :   ,U1A,NO FIRST
000191 :   ,XTA,B/UNIT
000192 :   ,AOX,SC/UNIT
000193 :   ,ATX,SC/UNIT
000194 : C***
000195 :   FIRST:Z,VTM,
000196 :   RR,VJM,READ ZON
000197 :   FIRST/:,BSS,
000198 :   REC,VTM,
000199 :   FW ,VTM,
000200 :   FR ,VTM,1
000201 :   AR ,VTM,-23
000202 :   A  ,VTM,P EXCH + 10
000203 :   R,UJ,
000204 : C***
000205 :   NO FIRST:,BSS,
000206 :   ,XTA,NFOLIS
000207 :   ,U1A,OWN PAGE
000208 :   RR,VTM,P EXCH
000209 :   ,*70,WS
000210 :   ,ITA,T
000211 :   RR,ATX,T - Z
000212 :   ,XTA,B/UNIT
000213 :   ,AEX,=:000004
000214 :   ,U1A,SET REG
000215 :   RR,ATX, FW - Z
000216 : C***
000217 :   OWN PAGE:,BSS,
000218 :   SET REG :,BSS,
000219 :   14,VTM, Z - T
000220 :   C,WTC,FL ER
000221 :   E,VTM,
000222 :   SETR:,UTC,P EXCH
000223 :   14,XTS, T - Z
000224 :   14,STI,T
000225 :   14,VLM,SETR
000226 :   E,VIM,ERROR
000227 :   R,UJ,
000228 : C***
000229 : C
000230 : C
000231 :   MAIN SW:,BSS,
000232 :   :,XTA, E O F
000233 :   ,UJ,WR EOF
000234 :   :RR,VTM,REWIND
000235 :   ,UJ,WRIT ZON
000236 :   :LR,VZM,EXIT
000237 :   ,UJ,BACK
000238 :   :FW,VTM,WCONT*
000239 :   ,UJ,WRITE
000240 :   :FR,VTM,RCONT
000241 :   ,UJ,READ
000242 :   :R,VTM,SCHEOF
000243 :   ,UJ,SCHEOF
000244 : C
000245 : C
000246 : C***
000247 : C
000248 :   REWIND:,BSS,
000249 :   R,VTM,EXIT
000250 :   Z,VIM,FIRST
000251 :   ,UJ,FIRST/
000252 : C
000253 : C***
000254 : C
000255 :   SCHEOF:,BSS,
000256 :   A,XTA,
000257 :   ,UZA,BUFP
000258 :   REC,UTM,1
000259 :   ,AEX,BEG REC
000260 :   ,UZA,BUFP
000261 :   R,VTM,EXIT
000262 : C
000263 : C***
000264 : C
000265 :   BUF P :,BSS,
000266 :   A,UTM,26
000267 :   ,ITS,A
000268 :   ,AAX,=1777
000269 :   ,U1A,NO READ
000270 :   A,VTM,P EXCH+10
000271 :   RR,VJM,WRIT ZON
000272 :   Z,UTM,1
000273 :   RR,VJM,CH RZ
000274 :   NO READ:,BSS,
000275 :   ,STI,
000276 :   R,UJ,
000277 : C
000278 : C***
000279 : C
000280 :   READ:,BSS,
000281 :   A,XTA,
000282 :   ,AEX,BEG REC
000283 :   ,U1A,ERR.HET HAЧ.PEK.
000284 :   ,ITA,FR
000285 :   ,ATX,FREAD
000286 :   ,UJ,EX REP
000287 : C
000288 : C***
000289 : C
000290 :   WRITE:,BSS,
000291 :   FR,VTM,
000292 :   C,WTC,CONTROL
000293 :   14,VTM,-362
000294 :   14,VIM,WR1
000295 :   FR,VTM,R CONT
000296 :   WR1:,XTA,BEG REC
000297 :   A,ATX,
000298 :   ,ITA,FW
000299 :   ,ATX,FREAD
000300 :   EX REP:,BSS,
000301 :   RR,VTM, Z - T
000302 :   ,XTA,
000303 :   ,UJ,PUTR
000304 : C
000305 : C***
000306 : C
000307 :   WR EOF:,BSS,
000308 :   A,ATX,
000309 :   A,ATX,25
000310 : CЛЛЛЛЛ ДO 20.9.79  OПEPATOP ENDFILE >>>>>
000311 : CЛЛЛЛЛ ПИCAЛ ДBA MAPKEPA ПOДPЯД     >>>>>
000312 :   FW,VTM,1
000313 :   R,VTM,NEW REC
000314 :   ,UJ,BUFP
000315 : C
000316 : C***
000317 : C
000318 :   CONTINUE:,BSS,
000319 :   ,ATI,AM
000320 :   ,ASN,64+24
000321 :   ,ATI,14
000322 :   ,XTA,FREAD
000323 :   ,ATI,R
000324 :   ,NTR,3
000325 :   ,XTA,ACC
000326 :   ,A+X,
000327 :   ,UZA,NOCH12
000328 :   R,VTM,R CONT12
000329 :   NO CH12:,AAX,B40
000330 :   ,UZA,END RW
000331 :   ,ITA,14
000332 :   ,ITS,AM
000333 :   15,A-X,
000334 :   ,ATI,T.PEГ.T ИCПOPЧEH!!!
000335 :   ,X-A,=1
000336 :   ,ARX,LEN
000337 :   ,ATX,LEN
000338 :   ,ITA,AR
000339 :   ,AEX,=1
000340 :   R,U1A,
000341 :   AR,VTM,
000342 :   ,UJ,WC1
000343 : C
000344 : C***
000345 : C
000346 :   RCONT12:AR,UTC,24
000347 :   A,XTA,
000348 :   AM,ATX,
000349 :   AM,UTM,1
000350 :   ,AEX,=12
000351 :   ,AAX,=377
000352 :   ,UZA,END RW
000353 :   ,UJ,RC//
000354 : C
000355 : C***
000356 : C
000357 :   WCONT*:FW,VTM,WCONT*
000358 :   W CONT:,BSS,
000359 :   AM,XTA,
000360 :   AM,UTM,1
000361 :   AR,UTC,24
000362 :   A,ATX,
000363 :   T,VLM,WC1
000364 :   ,UJ,END RW1
000365 : C***
000366 :   WC1:,BSS,
000367 :   AR,VLM,W CONT
000368 :   FR,VIM,MID
000369 :   AR,VTM,-23
000370 :   ,XTA,
000371 :   A,ATX,25
000372 :   R,VJM,BUFP
000373 :   A,ATX,
000374 :   ,UJ,W CONT*
000375 : C
000376 : C***
000377 : C
000378 :   R CONT:,BSS,
000379 :   AR,UTC,24
000380 :   A,XTA,
000381 :   AM,ATX,
000382 :   AM,UTM,1
000383 :   RC//:T,VLM,RC1
000384 :   ,UJ,END RW1
000385 :   RC1:R,UTC,
000386 :   AR,VLM,
000387 :   MID:A,XTA,25
000388 :   AR,VTM,-23
000389 :   ,UZA,BUFP
000390 :   ,XTA,B/UNIT
000391 :   ,ANX,
000392 :   ,ATI,T
000393 :   ,WTC,FREAD
000394 :   14,VTM,-R CONT
000395 :   14,VZM,ERR3
000396 :   ,UJ,ERR 2
000397 : C
000398 : C***
000399 : C
000400 :   END RW1:AR,VLM,END RW
000401 :   AR,VTM,1
000402 :   END RW:,BSS,
000403 :   ,XTA,B/UNIT
000404 :   ,ANX,
000405 :   ,ATI,T.T BOCCTAHOBЛEH
000406 :   ,XTA,ACC
000407 :   ,AAX,B42
000408 :   ,UZA,EX REP
000409 :   FR,VIM,CLOSE R
000410 :   ,XTA,LEN
000411 : C******* ЗATЫЧKA ЧTOБ БЫЛO ПOXOЖE HA ЗAИKИHA ***
000412 :   AR,MTJ,14
000413 :   14,UTM,-1
000414 :   14,VIM,CLOSE W
000415 :   ,XTA,
000416 :   AR,VTM,-23
000417 :   A,ATX,25
000418 :   R,VJM,BUFP
000419 :   A,ATX,
000420 :   ,XTA,LEN
000421 : C================
000422 :   CLOSE W:,BSS,
000423 :   A,ATX,25
000424 :   ,XTA,
000425 :   CL ENDR:AR,UTC,24
000426 :   A,ATX,
000427 :   AR,VLM,CL ENDR
000428 :   R,VJM,BUFP
000429 :   ,XTA,E O F
000430 :   FW,VTM,1
000431 :   A,ATX,
000432 :   A,ATX,25
000433 :   ,UJ,NEW REC
000434 :   CLOSE R:,BSS,
000435 :   R,VTM,NEW REC
000436 :   A,XTA,
000437 :   ,AEX, E O F
000438 :   ,UZA,BUFP
000439 :   A,XTA,25
000440 :   ,U1A,BUFP
000441 :   R,VTM,CLOSE R
000442 :   ,UJ,BUFP
000443 :   NEW REC:AR,VTM,-23
000444 :   REC,UTM,1
000445 :   ,UJ,EXIT
000446 : C
000447 : C***
000448 : C
000449 :   SWOP ALL:,BSS,
000450 :   C,WTC,FL ER
000451 :   ER,VTM,
000452 :   C,ATX,FL ER.CYПPOTИB PEKYPCИИ
000453 :   ,XTA,=:000004
000454 :   SWOP1:,ATX,B/UNIT
000455 :   ,AAX,SC/UNIT
000456 :   ,UZA,NEXT
000457 :   ,ANX,
000458 :   ,ATI,T
000459 :   R,VJM,GIVEPAGE
000460 :   ,XTA,B/UNIT
000461 :   ,ASN,64+16
000462 :   ,AAX,SC/UNIT
000463 :   RR,VJM,WRIT ZON
000464 :   ,UZA,NO ERR
000465 :   ,ITA,Z
000466 :   ,ATX,NZON
000467 :   ,ITA,REC
000468 :   ,ATX,NREC
000469 :   ,ITA,T
000470 :   ,ATX,LUN
000471 :   ER,J+M,ER
000472 :   ER,J+M,ER
000473 :   14,VTM,-3
000474 :   *1:ER,UTC,TER-4
000475 :   14,XTA,3
000476 :   14,ATX,F+3
000477 :   14,VLM,*1
000478 :   ,PRINT,P
000479 :   NO ERR:,XTA,NFOLIS
000480 :   ,UZA,NEXT
000481 :   ,*72,NFOLIS
000482 :   NEXT:,XTA,B/UNIT
000483 :   ,ASN,64-1
000484 :   ,U1A,SWOP1
000485 :   ,XTA,PEX BOSS
000486 :   ,UZA,YCE
000487 :   ,*70,IR SAV
000488 :   ,XTA,
000489 :   ,ATX,PEX BOSS
000490 :   YCE:,ATX,SC/UNIT
000491 :   ,UJ,END
000492 : C
000493 : C***
000494 : C
000495 :   CH RZ:,BSS,
000496 :   RR,UTC,
000497 :   FR,VZM,
000498 :   READ ZON:,BSS,
000499 :   ,XTS,IR70
000500 :   14,VJM,OBMEN
000501 :   ,XTA,SC/UNIT
000502 :   ,ASN,64-32
000503 :   ,AAX,B/UNIT
000504 :   ,UZA,NO CHECK
000505 :   14,VTM,P EXCH
000506 :   14,XTA,.*****
000507 :   ,ITA,14
000508 :   ,FUN,200B.*****
000509 :   ,ATI,14
000510 :   14,UTM,-2
000511 :   C,XTA,BAD READ
000512 :   14,VIM,NO CHECK
000513 :   ,ITA,Z
000514 :   ,ARX,=1
000515 :   NO CHECK:,AOX,A/I0
000516 :   C,STX,BAD READ
000517 :   RR,UJ,
000518 : C
000519 : C***
000520 : C
000521 :   WRIT ZON:,BSS,
000522 :   RR,UTC,
000523 :   FW,VZM,
000524 :   FW,VTM,
000525 :   ,XTS,IR70
000526 :   ,AEX,B40
000527 :   14,VTM,WZEX
000528 : C---
000529 :   OBMEN:,ATX,WS
000530 :   ,ITA,14
000531 :   ,XTS,B/UNIT
000532 :   ,ANX,UJ
000533 :   ,ATI,14
000534 :   ,ITA,Z
000535 :   14,VIM,NO DRUM
000536 :   ,ASN,64-40
000537 :   ,AUX,=7 0037
000538 :   NO DRUM:,ARX,WS
000539 :   ,ATX,WS
000540 :   ,*70,WS
000541 :   15,WTC,
000542 :   ,UJ,
000543 : C---
000544 :   WZEX:,BSS,
000545 :   ,STI,
000546 :   RR,UJ,
000547 : C
000548 : C***
000549 : C
000550 :   BACK:,BSS,
000551 :   ,ITA,A
000552 :   ,AAX,=1777
000553 :   ,AEX,=12
000554 :   ,U1A,BACK1
000555 :   A,VTM,P EXCH+1024
000556 :   RR,VJM,WRIT ZON
000557 :   Z,UTM,-1
000558 :   RR,VJM,READ ZON
000559 :   BACK1:A,UTM,-26
000560 :   A,XTA,
000561 :   ,UZA,BACK
000562 :   REC,UTM,-1
000563 : C
000564 : C***
000565 : C
000566 :   EXIT:,BSS,
000567 :   RR,VTM, Z - T
000568 :   ,XTA,NFOLIS
000569 :   T,MTJ,14
000570 :   14,UTM,-16
000571 :   14,VIM,PUT R
000572 :   ,ITS,Z
000573 :   ,ASN,64-40
000574 :   ,AUX,=7 0037
000575 :   ,STX,DRUM ZON
000576 :   ,U1A,PUT R
000577 :   ,XTS,DRUM ZON
000578 :   ,ARX,=7 0000
000579 :   ,STX,PEX BOSS
000580 :   PUT R:RR,ITS,T
000581 :   ,UTC,P EXCH
000582 :   RR,STX, T - Z
000583 :   RR,VLM,PUT R
000584 :   ,UZA,END
000585 :   ,*72,NFOLIS
000586 :   ,XTA,
000587 :   ,ATX,NFOLIS
000588 : C***
000589 :   END:15,XTR,-1
000590 :   ,ITA,Z
000591 :   ,AOX,A/I0
000592 :   C,STX,ZONE
000593 :   ,STI,
000594 :   14,VTM,R
000595 :   RES R:14,STI,
000596 :   14,UTM,-1
000597 :   14,VIM,RES R
000598 :   ,STI,14
000599 :   UJ:13,UJ,
000600 :   ,Z00,-16
000601 : C
000602 : C***
000603 : C
000604 :   ERR:,AEX,=:5
000605 :   ,U1A,ERR3
000606 :   ERR 1:E,VTM,1
000607 :   ,UJ,SET E
000608 :   ERR 2:E,VTM,2
000609 :   ,UJ,SET E
000610 :   ERR3:E,VTM,3
000611 :   ,UJ,SET E
000612 :   SET E:C,XTA,CONTROL
000613 :   ,ATI,14
000614 :   ,ITA,E
000615 :   ,AOX,A/I0
000616 :   C,ATX,FL ER
000617 :   ,XTA,B42
000618 :   ,ATX,ACC
000619 :   14,VIM,END RW
000620 :   PRINT ER:,BSS,
000621 :   14,VTM,STOP*
000622 :   ,ITA,14.HA MECTO
000623 :   15,ATX,-2.И.P.13
000624 :   ,XTA,B/UNIT
000625 :   ,ASN,64+16
000626 :   ,AOX,SC/UNIT
000627 :   ,ATX,SC/UNIT
000628 :   ,UJ,EXIT
000629 : C***
000630 :   ERROR:,BSS,
000631 :   ,XTA,ACC
000632 :   ,ARX,
000633 :   ,U1A,END
000634 :   ,AAX,=:01
000635 :   ,UIA,PRINT ER."READ"
000636 :   ,XTA,A/I0
000637 :   C,ATX,FL ER
000638 :   R,UJ,
000639 : C
000640 : C=================================
000641 : C
000642 :   AMSW:,BSS,
000643 :   IW72:2,Z04,NP+77B.ДЛЯ ЗAMEHЫ ЛИCTOB
000644 :   ,Z00,MAIN SW - 1.ПEPEKЛ.PAБOT
000645 :   ACC:,LOG,.CYMMATOP BXOДA
000646 :   B/UNIT:,LOG,.БИT YCTP-BA
000647 :   NEW BOSS:,LOG,.HOBЫЙ BЛAДEЛEЦ OБM.ЛИCTA
000648 :   NFOLIS:,LOG,.ИHФ.CЛOBO Э072
000649 :   LEN   :,LOG,.ДЛИHA PEKOPДA
000650 :   IW SAV:,000,NP.ДЛЯ CПACEHИЯ
000651 :   ,007,.OБM.ЛИCTA
000652 :   IR SAV:,010,NP
000653 :   ,007,
000654 :   NP31/35:,010,NP
000655 :   ,Z00,
000656 :   BEG REC:,OCT,6
000657 :   E O F  :,OCT,3
000658 :   B42:,OCT,004
000659 :   B40:,OCT,001
000660 :   FREAD:,LOG,
000661 :   IR70:,LOG,
000662 :   WS  :,LOG,
000663 :   P:,Z00,STR
000664 :   ,Z00,NREC
000665 :   :0,002,
000666 :   ,,
000667 :   :2,076,2
000668 :   ,002,
000669 :   :0,100,
000670 :   ,,
000671 :   :2,114,4
000672 :   ,004,
000673 :   :0,120,
000674 :   ,,
000675 :   :2,134,5
000676 :   8,005,
000677 :   TER:,BSS,
000678 :   ,GOST,24HЧTEHИE  END OF FILE
000679 :   ,GOST,24HCЛИШKOM ДЛИHHЫЙ PEKOPД
000680 :   ,GOST,24HЧИTAEM БOЛЬШE ЧEM ПИCAЛИ
000681 :   STR:,GOST,18H*** OШИБKA OБMEHA:
000682 :   F:,BSS,4
000683 :   :,GOST,18H / YCTPOЙCTBO(B)='231'
000684 :   LUN:,BSS,1
000685 :   :,GOST,12H   ЗOHA(B)='231'
000686 :   NZON:,BSS,1
000687 :   :,GOST,12H   PEK.(B)='231'
000688 :   NREC:,LOG,
000689 :   LAST:,BSS,
000690 :   LENPRO:,EQU,LAST-START
000691 :  RESERV:,EQU,714B-LENPRO
000692 :   ДЫPA :,BSS,RESERV.ДЛЯ COXP.ДЛИHЫ
000693 :   ,END,
000694 :  FORMKOZA:,NAME, . 10.04.73
000695 :  PREDCOSY:   ,LC,16                         .
000696 :   STAPUNCH:,SUBP,
000697 :   STAPUNCH:I,BLOCK,PERCOSY1,PERSTAN1
000698 :   ,CONT,PERCOSY2(0),PERSTAN2(0),PERSTANE
000699 :   ,CONT,PERCOSY3(0),PERSTAN3
000700 :    K     :   ,EQU,1                         .
000701 :    L     :   ,EQU,2                         .
000702 :    KC    :   ,EQU,3                         .
000703 :    JC    :   ,EQU,4                         .
000704 :    KL    :   ,EQU,5                         .
000705 :     LK   :   ,EQU,6                         .
000706 :    J     :   ,EQU,7                         .
000707 :  :,UJ,BEGIN
000708 :  :,UJ,CONTI
000709 :  BEGIN:,BSS,
000710 :              ,ITS,13
000711 :              ,UTC,*CON
000712 :              ,STX,
000713 :   13,VJM,PERCOSY1
000714 :  ,XTS,
000715 :  ,UTC,BEGI
000716 :  ,STX,
000717 :  ,WTC,*CON
000718 :  ,UJ,
000719 :  CONTI:,BSS,
000720 :  ,ITS,13
000721 :  ,UTC,*CON
000722 :  ,STX,
000723 :  ,UTC,ALFA
000724 :  ,STX,
000725 :              ,ITS,K                         .
000726 :              ,ITS,L                         .
000727 :              ,ITS,KC                        .
000728 :              ,ITS,JC                        .
000729 :              ,ITS,KL                        .
000730 :              ,ITS,LK                        .
000731 :              ,ITS,J                         .
000732 :              ,XTS,                          .
000733 :             J,VTM,0                         .
000734 :             K,VTM,40
000735 :             L,VTM,M                         .
000736 :            KC,VTM,-13                       .
000737 :            KL,VTM,-16
000738 :            LK,VTM,-40                       .
000739 :  L,XTA,BEGI-M
000740 :  ,UZA,BEGIN1
000741 :  14,VTM,-6
000742 :  CY:14,UTC,REG1+6
000743 :  ,XTA,
000744 :  14,ATI,7
000745 :  14,VLM,CY
000746 :  BEGIN1:,BSS,
000747 :  L,XTA,LOG12-M
000748 :  L,ATX,BEGI-M
000749 :  L,XTA,ALFA-M
000750 :  ,UZA,END
000751 :  REDCARD :   ,WTC,ALFA                      .
000752 :            KC,XTA,13                        .
000753 :             K,ASN,64
000754 :             L,AAX,LOG377-M
000755 :             L,ATX,SIMB-M                    .
000756 :             K,V1M,PREDICT
000757 :             K,VTM,48
000758 :            KC,VLM,PREDICT                   .
000759 :            KC,VTM,-13                       .
000760 :             L,XTA,CON1-M
000761 :             L,ATX,FLAG1-M
000762 :  PREDICT :   ,BSS,
000763 :             K,UTM,-8                        .
000764 :             L,XTA,SIMB-M
000765 :             L,AEX,ISOBLAN-M
000766 :              ,UZA,*EQVBLAN
000767 :              ,ITA,J
000768 :              ,UZA,*NOBLAN
000769 :             L,XTA,SIMB-M
000770 :             L,ATX,EQVSIMB-M
000771 :              ,ITA,J
000772 :          L,AOX,M200-M
000773 :             L,ATX,SIMB-M
000774 :            13,VJM,KOPSIMB
000775 :             J,VTM,0
000776 :             L,XTA,EQVSIMB-M
000777 :             L,ATX,SIMB-M
000778 :  *NOBLAN :   ,BSS,
000779 :            13,VJM,KOPSIMB
000780 :             L,XTA,FLAG1-M
000781 :              ,UZA,*GOP
000782 :              ,XTA,
000783 :             L,ATX,FLAG1-M
000784 :  14,VTM,-6
000785 :  CYK:14,ITA,7
000786 :  14,UTC,REG1+6
000787 :  ,ATX,
000788 :  14,VLM,CYK
000789 :  ,UJ,*END*+1
000790 :  *GOP    :   ,UJ,REDCARD
000791 :  *EQVBLAN:  J,UTM,1
000792 :              ,UJ,REDCARD
000793 :  KOPSIMB :   ,XTA,                          .
000794 :             L,ATX,FLG2-M                    .
000795 :             L,XTA,SIMB-M                    .
000796 :             L,AEX,LOG12-M                 .
000797 :              ,U1A,*MAZNY                  .
000798 :             L,XTA,CON1-M                  .
000799 :            L,ATX,FLAG4-M                  .
000800 :   *MAZNY :  L,XTA,SIMB-M                  .
000801 :            LK,ASN,64                        .
000802 :             L,AOX,OCT-M                     .
000803 :             L,ATX,OCT-M                     .
000804 :            LK,VZM,*FORM
000805 :            LK,UTM,8                         .
000806 :             L,XTA,FLAG4-M                 .
000807 :              ,U1A,ENDCARD                 .
000808 :            13,UJ,                           .
000809 :  ENDCARD :   ,BSS,                        .
000810 :              ,XTA,                        .
000811 :            L,ATX,FLAG4-M                  .
000812 :             L,XTA,OCT-M
000813 :             L,AAX,CON1374-M               .
000814 :             L,AOX,SIMB-M                  .
000815 :             L,ATX,OCT-M                   .
000816 :              ,UJ,*FORM                    .
000817 :  *FORM   :  L,XTA,OCT-M                     .
000818 :              ,UTC,PREDCOSY                  .
000819 :            KL,ATX,16
000820 :             ,XTA,
000821 :             L,ATX,OCT-M
000822 :              ,XTA,
000823 :             L,ATX,FLAG4-M
000824 :            LK,VTM,-40                       .
000825 :            KL,UTM,1                         .
000826 :            KL,VZM,*MORF
000827 :            13,UJ,                           .
000828 :  *MORF   : KL,VTM,-16
000829 :              ,ITA,13                      .
000830 :             L,ATX,*SLON-M                 .
000831 :            14,VTM,PREDCOSY
000832 :              ,ITS,14
000833 :              ,CALL,COSYTAKE
000834 :             L,XTA,CON1-M
000835 :             L,ATX,FLG2-M
000836 :              ,WTC,*SLON                   .
000837 :              ,UJ,                         .
000838 :  END     :   ,BSS,
000839 :             L,XTA,FLG2-M
000840 :              ,U1A,*END*
000841 :             L,XTA,NOL-M
000842 :             L,ATX,SIMB-M
000843 :            13,VJM,KOPSIMB
000844 :              ,UJ,END
000845 :  *END*   :   ,BSS,
000846 :   13,VJM,PERCOSY3
000847 :              ,STX,
000848 :              ,STI,J
000849 :              ,STI,LK
000850 :              ,STI,KL                        .
000851 :              ,STI,JC
000852 :              ,STI,KC
000853 :              ,STI,L
000854 :              ,STI,K
000855 :              ,WTC,*CON
000856 :              ,UJ,
000857 :          'CONST'
000858 :    M     :   ,OCT,                          .
000859 :   BETA   :   ,OCT,
000860 :   FLAG4  :   ,OCT,                        .
000861 :   NOL    :   ,OCT,                          .
000862 :  SIMB    :   ,OCT,                          .
000863 :  EQVSIMB :   ,OCT,                          .
000864 :   *SLON  :   ,OCT,                        .
000865 :   *CON   :   ,OCT,
000866 :  ALFA    :   ,OCT,
000867 :  CON1    :   ,LOG,1                         .
000868 :  OCT     :   ,OCT,                          .
000869 :  CON1374 :   ,OCT,7777777777777400        .
000870 :   LOG12  :   ,LOG,12                      .
000871 :   JN     :   ,OCT,
000872 :   M200   :   ,LOG,200                     .
000873 :  OCTSIMB :   ,OCT,                        .
000874 :  FLAG1   :   ,OCT,
000875 :  FLG2    :   ,OCT,                          .
000876 :  ISOBLAN :   ,LOG,40
000877 :  FLAG3   :   ,OCT,
000878 :  LOG377  :   ,LOG,377
000879 :  BEGI:,BSS,1
000880 :  REG1:,BSS,7
000881 :         ,END,
000882 :       I*DI*I      :,NAME,
000883 :                   12    ,VTM,      *00020B
000884 :                   12    ,ATX ,
000885 :                         ,RTE ,00177B
000886 :                   14    ,XTS ,
000887 :                         ,NTR ,2
000888 :                         ,A+X ,
000889 :                   12    ,XTS ,
000890 :                         ,A+X ,
000891 :                   15    ,A/X ,
000892 :                   12    ,ATX ,
000893 :                         ,NTR ,3
000894 :                   12    ,AMX ,00001B
000895 :                   12    ,AVX ,
000896 :                   15    ,XTR ,
000897 :                   13    ,UJ,  00000B
000898 :                         ,UTC, 00000B
000899 :   I*ST*L:,ENTRY,
000900 :                   14    ,ATX ,
000901 :                   13    ,UJ,  00000B
000902 :   I*MU*I:,ENTRY,
000903 :                   12    ,VTM,      *00020B
000904 :                   15    ,ATX ,
000905 :                         ,RTE ,00177B
000906 :                         ,NTR ,3
000907 :                   12    ,STX ,00003B
000908 :                         ,E+N ,24
000909 :                   14    ,A*X ,
000910 :                   12    ,ATX ,
000911 :                         ,YTA ,00100B
000912 :                   12    ,XTS ,
000913 :                   12    ,AAX ,00002B
000914 :                   15    ,AOX ,
000915 :                   12    ,XTR ,00003B
000916 :                   13    ,UJ,  00000B
000917 : CONST
000918 :      *00020B:,OCT,0000  0000  0000  0000
000919 :      *00021B:,OCT,6400  0000  0000  0000
000920 :      *00022B:,OCT,0020  0000  0000  0000
000921 :      *00023B:,OCT,
000922 :                   ,END,
000923 :       I*DI*R      :,NAME,
000924 :                   12    ,VTM,      *00012B
000925 :                   12    ,ATX ,
000926 :                         ,RTE ,00177B
000927 :                   12    ,XTS ,
000928 :                         ,NTR ,2
000929 :                         ,A+X ,
000930 :                   14    ,A/X ,
000931 :                         ,UTC, 00000B
000932 :      *00004B:     15    ,XTR ,
000933 :                   13    ,UJ,  00000B
000934 :   R*DI*I:,ENTRY,
000935 :                   12    ,VTM,      *00012B
000936 :                   12    ,ATX ,
000937 :                         ,RTE ,00177B
000938 :                   14    ,XTS ,
000939 :                         ,NTR ,2
000940 :                         ,A+X ,
000941 :                   12    ,XTS ,
000942 :                   15    ,A/X ,
000943 :                         ,UJ,       *00004B
000944 :                         ,UTC, 00000B
000945 : CONST
000946 :      *00012B:,OCT,0000  0000  0000  0000
000947 :                   ,END,
000948 :  I*MUI*:,NAME,
000949 :           ,E+N ,64-40
000950 :         15,A*X ,.3
000951 :         15,ATX ,.4
000952 :           ,YTA ,64.5
000953 :         15,AVX ,.6
000954 :         14,UZA ,.7
000955 :           ,UTC ,=0020 0000 0000 0000.8
000956 :           ,AEX ,.9
000957 :           ,X-A ,.10
000958 :         14, UJ ,.11
000959 :   ,END,
000960 :       I*PO*I     :,NAME,
000961 :       R*PO*I     :,SUBP,
000962 :       R*ST*I     :,SUBP,
000963 :       POWER      :,SUBP,
000964 :                  ,ATI,  12
000965 :                  ,RTE,  177B
000966 :                  ,ITS,  13
000967 :                  ,NTR,  2
000968 :             12 ,  XTS,
000969 :                  ,A+X,
000970 :             12 ,  VTM,   *00021
000971 :             12 ,  ATX,
000972 :                  ,ITA,  12
000973 :             13 ,  VJM,  R*PO*I
000974 :             14 ,  VTM,   *00021
000975 :             13 ,  VJM,  R*ST*I
000976 :             12 ,  VTM,   *00021
000977 :             12 ,  STX,
000978 :             15 ,  XTR,
000979 :                  ,ATI,  13
000980 :             12 ,  XTA,
000981 :             13 ,   UJ,
000982 :       I*PO*R    :,ENTRY,
000983 :       R*PO*R    :,ENTRY,
000984 :   ,ATI,12
000985 :   12,XTA,
000986 :   14,XTS,
000987 :   ,UJ,POWER
000988 :  *00021    :,     LOG,  00000000 00000000.
000989 :  BSS       :     ,BSS,
000990 :       ,END,
000991 :  IDIV:,NAME,
000992 : C ДEЛEHИE ЦEЛOГO HA ЦEЛOE
000993 :  ,NTR,2
000994 :  ,A+X,
000995 :  14,VTM,ERAS
000996 :  14,STX,
000997 :  ,A+X,
000998 :  14,A/X,
000999 :  14,ATX,
001000 :  ,AMX,
001001 :  ,NTR,3
001002 :  14,A+X,1
001003 :  14,AVX,
001004 :  ,NTR,18
001005 :  13,UJ,
001006 :  ERAS:,BSS,1
001007 :  ,INT,0
001008 :  ,END,
001009 :  IF OV FL:,NAME,
001010 :   A/I0:,LC,1
001011 :   CAUSA*:,LC,1.ПPИЧИHA ABOCTA
001012 :  EST:,EQU,STAT+17
001013 :  ,ITA,15
001014 :  15,VTM,STAT
001015 :  14,VTM,13
001016 :  *1:14,ITS,
001017 :  14,UTM,-1
001018 :  14,V1M,*1
001019 :  ,XTS,16B
001020 :  ,XTS,17B
001021 :  ,WTC,STAT
001022 :  ,XTS,-1
001023 :  15,ATX,
001024 :   15,XTA,*2-EST
001025 :  ,CTX,20B
001026 :   ,XTA,A/I0
001027 :  ,UJ,RET
001028 :  *2:,NTR,3
001029 :  ,UJ,OVFL
001030 :  OV FL:,BSS,
001031 :   ,UTC,CAUSA*
001032 :   ,ATX,
001033 :  15,UTM,1
001034 :  ,ITA,15
001035 :  15,VTM,EST
001036 :   15,A-X,STAT-EST
001037 :  ,UZA,N
001038 :  ,SJ,
001039 :  N:,BSS,
001040 :  15,XTA,
001041 :  ,WTC,STAT
001042 :  ,STX,-1
001043 :  ,STX,17B
001044 :  ,STX,16B
001045 :  14,VTM,-12
001046 :  *3:14,STI,13
001047 :  14,VLM,*3
001048 : C--------------------------------------
001049 :  DISP: 2,IFSEQ,D,DD**
001050 :   15,XTA,= 777 00020 776 00002 - STAT
001051 :  ,*72,70707B.FOR DD71 ONLY
001052 : C--------------------------------------
001053 :   15,XTA, =I1 - STAT
001054 :  RET:,WTC,STAT
001055 :  15,VTM,
001056 :  ,NTR,18
001057 :  13,UJ,
001058 :  STAT:,BSS,17
001059 :  ,END,
001060 :  IFPULT:,NAME,
001061 :  11,BASE,*.
001062 :  ,ATI,12.
001063 :  12,WTC,.
001064 :  12,VTM,.
001065 :  ,*61,.
001066 :  12,ASN,64-1.
001067 :  ,AAX,=1.
001068 :  ,AOX,=I0.
001069 :  13,UJ,.
001070 :  ,END,.
001071 :  IOCONT* :   , NAME  ,                    FPACK   14.09.72
001072 : C
001073 : C** FPACK-IOCONT*(A,E)
001074 : C                  BENUTZERROUTINE ZUR FORTSETZUNG DER EIN- AUSGABE
001075 : C
001076 : C
001077 :  *0      :   , EQUAL ,
001078 :  *0: , BLOCK , A1,     A13,    A17,    A1750,  A17700, A20077, A377
001079 :   ,CONT,A5,A60,A77777,BIT1/6,BIT1/7,BIT2/48  .06
001080 :      , CONT  , BIT1/40,BIT1/41,BIT10/11,       BIT10/14,       BIT1/48
001081 :      , CONT  , BIT12,  BIT10,  BIT12/14,       BIT12/15,       BIT14
001082 :      , CONT  , BIT15,  BIT16,  BIT17,  BIT13/48,       BIT16/41,
001083 :      , CONT  , BIT3/4,BIT37/48,        BIT41/48                      .03
001084 :      , CONT  , BIT43/48,       BIT48(0),       EXP64,  BIT7,         .03
001085 :      , CONT  , BIT8,   BIT8/15,BIT9/48,VGR0,   EX640(0),       VGR(6).10
001086 :   ,CONT,CT(64),REAL1(0),DEINS(2)    .66
001087 :   ,CONT,E1(0),D10(2),R1/2(0),D1/2(2),REAL5/4,R5/4H13
001088 :   ,CONT,EKSPO(19),
001089 :      , CONT  , SECHS,  Z1/10,  D1/10(2),       D10E40(2),            .06
001090 :      , CONT  , D10E*40(2),     EOR,                                  .03
001091 :      , CONT  , VMASKE1,VMASKE2,VW1,    VW2,    VH,     VP            .06
001092 :      , CONT  , SPACE,  EX645,  K3HREC, K3HADD,                       .04
001093 :      , CONT  , MAXEXNZ,FTXT(14),       GRZAHL, ISOSP,  NBIT45, BIT46 .19
001094 :      , CONT  , BIT45,  BIT2/47,NZL,    KRECORD,A4,     BIT46/48,     .06
001095 :  *0: , BLOCK , SYMB,   ENDE,  KUSP/USP,KNEXTLET,PW,  KWSY,     SFAKT,
001096 :      , CONT  , AUSG2,  D,      NSP,    CENC,   NSYMB                 .05
001097 :      , CONT  , II13,   ER/AB,  ADCONT, ADR/W,  ADEND,  FORMAT, LUN,
001098 :      , CONT  , I/O,    GLREG(7),       BEGREC, ADNBPROG,       (1)   .11
001099 :      , CONT  , RECORD(25),     GWF(5), EAD(5), FTT,    SWUEBERL, WF  .38
001100 :      , CONT  , W,      NIW,    KBASP,  STLUN,  AC,                   .05
001101 :      , CONT  , INOUT,  LUN*,   L/NLGLRE,       KOFSPEC,        TRWADR,
001102 :      , CONT  , FNR,    II15,   NSP3,   ZHLR,   FTBE/SONST,           .05
001103 :      , CONT  , LASP                .LAENGE DES ARBEITSSPEICHERS       01
001104 :  LUNMUN  :   , LC    , 1
001105 :  CHECKFL*:   , LC    , 1
001106 :  KLUN    :   , EQU   , FTXT + 6
001107 :  KADDRESS:   , EQU   , FTXT + 12
001108 :  KFORMAT :   , EQU   , FTXT + 10
001109 :  KSPEC   :   , EQU   , FTXT + 7
001110 :  MAG     :   , EQU   , 10
001111 :  X       :   , EQU   , 6
001112 :  NWO     :   , EQUAL ,  2
001113 :  NSG     :   , EQUAL ,  6
001114 :  NSY     :   , EQUAL ,  7
001115 :  R2      :   , EQUAL , 12
001116 :  S       :   , EQUAL , 15
001117 :  R4      :   , EQUAL , 14
001118 :  A       :   , EQUAL ,  8
001119 :  C       :   , EQUAL , 1
001120 :  I       :   , EQUAL , 13
001121 :  R3      :   , EQUAL , 13
001122 :  SY      :   , EQUAL ,  14
001123 :  RR      :   , EQU   , 5
001124 :  K       :   , EQUAL , 13
001125 :  L       :   , EQUAL , 11
001126 :  STOP*   :   , SUBP  ,
001127 :  PRINT8  :   , SUBP  ,
001128 :  I/O*ERR:,SUBP,
001129 : C     ПEPEXBAT OШИБKИ BBOДA
001130 :  JERRIO*:,LC,1
001131 :  I11     :   , EQU   , 11
001132 :  LMU     :   , EQU   , 14
001133 :  I1      :   , EQUAL ,  1
001134 :  I3      :   , EQUAL , 3
001135 :  I4      :   , EQUAL , 4
001136 :  I8      :   , EQUAL , 8
001137 :  I12     :   , EQUAL , 12
001138 :  I14     :   , EQUAL , 14
001139 :  IOCONT* :   , BSS   ,.
001140 :           I14, VTM   , IOC0
001141 :              , UJ    , RGTRI13*
001142 : C---------------------------------------------------------------------
001143 :            C , XTA   , A1          . ENTRY: IOEND*1
001144 :              , UJ    , IO1
001145 :          :   , UJ    , FPF*ABBR    . ENTRY: FEHLERDRUCK OHNE RECORD
001146 :          :   , UJ    , FPF*ABBR+2  . ENTRY: FEHLERDRUCK MIT RECORD
001147 :          :LMU, VTM   , LUNMUN - 1  . ENTRY: SETLUN*
001148 :              , UJ    , SETLUN*
001149 :          :   , UJ    , READSY2     . ENTRY: READSY2
001150 :          :   , UJ    , READSY2F    . ENTRY: READSY2F
001151 :          :   , UJ    , RSZRZST     . ENTRY: RSZRZST
001152 :          : S , ATX   ,             . ENTRY: RGTRSPR*
001153 :              , UJ    , RGTRSPA*
001154 :          :I11, VTM   , RGT0        . ENTRY: RGTRSPA*
001155 :              , UJ    , RGTAUSCH
001156 :          :   , UJ    , RGTRI13*    . ENTRY: RGTRI13*
001157 :          :   , UJ    , RI15        . ENTRY: RI15
001158 :          :   , UJ    , RSPLI15*    . ENTRY: RSPLI15*
001159 :          :   , UJ    , READSY3     . ENTRY: READSY3
001160 :          :   , UJ    , READSY3F    . ENTRY: READSY3F
001161 :          :   , UJ    , RSZRZST3    . ENTRY: RSZRZST3
001162 : C---------------------------------------------------------------------
001163 :  IOC0    :I8 ,  STX  , ENDE
001164 :              ,  ATI  , I3
001165 :              ,  NTR  , 3
001166 :           I8 ,  A-X  , ENDE
001167 :              ,  ATI  , I4
001168 :           R3 , VJM   , RI15
001169 :           I12, VTM   , IOC2
001170 :            C , A-X   , A1
001171 :              , UZA   , IOCF1              FALLS A GROESSER ALS E
001172 :              ,  XTA  ,
001173 :  IOC1    :I8 ,  ATX  , AC
001174 :              ,  ITA  , I12
001175 :           I8 ,  ATX  , ADNBPROG
001176 :              ,  NTR  , 2
001177 :           I8 ,  XTA  , ER/AB
001178 :              ,  UZA  , IOCF
001179 :           I8 ,  WTC  , ADCONT
001180 :              ,  UJ   ,
001181 :  '--------------------------------
001182 :  IOAC*   :   , ENTRY ,
001183 :            S , ATX   ,
001184 :           I14,  VJM  , RGTRI13*
001185 :           I8 ,  ATX  , INOUT
001186 :           I8 ,  UTC  , INOUT
001187 :           I3 ,  VTM  ,
001188 :              ,  ITA  , I3                 I:=0
001189 :           I8 ,  ATX  , ENDE
001190 :           I4 ,  VTM  ,
001191 :           I12, VTM   , IOC3
001192 :           I1 , XTA   , A1
001193 :            R3, VTM   , IOC1
001194 :              , UJ    , RI15
001195 :  '--------------------------------
001196 :  IOCF    :I12, 31    , FPF*ABBR.   AUFRUF VON IOCONT* BZW. IOAC* OHNE
001197 :              , UTC   , 216 B.      VORHERIGEN AUFRUF EINER EROEFFNUNGS-
001198 :              , UJ    , RSPLI15*   .ROUTINE
001199 :  IOCF1   :   , ITA   , I12
001200 :            A , ATX   , ADNBPROG
001201 :           R2 , 31    , FPF*ABBR    .BEIM AUFRUF VON IOCONT*(A,E)
001202 :              , UTC   , 1 225B      .IST A GROESSER ALS E
001203 :              , UJ    , RSPLI15*
001204 : C----------------------------------------------------------------------
001205 :  FT*722  :   , ENTRY ,.             BEI WRITE (LUN) LISTE /(BINAER)
001206 :  FT*642  :   , ENTRY ,.             BEI READ (LUN)  LISTE /(BINAER)
001207 :  FT*002  :   , ENTRY ,.             BEI PRINT FT,PUNCH FT,WRITE(LUN FT)
001208 : C                                   ENCODE (C,FTV)... DREAD (LUN,FT)...
001209 : C                                   READ FT,...        DECODE (C,FT,V)..
001210 : C                                   EIN- AUSGABE EINES WERTES (1WORT)
001211 :  IOC4    :   , ITS   , 14
001212 :              , UJ    , IOCONT*
001213 : C----------------------------------------------------------------------
001214 :  FD*642  :   , ENTRY , .            BEI READ (LUN) LISTE  /(BINAER)
001215 :  FD*722  :   , ENTRY , .            BEI WRITE (LUN) LISTE /(BINAER)
001216 : C                                   EIN-AUSGABE EINER D-ZAHL (2 WORTE)
001217 :  FC*722  :   , ENTRY ,.             BEI WRITE (LUN) LISTE /(BINAER)
001218 :  FC*642  :   , ENTRY ,.             BEI READ (LUN)  LISTE /(BINAER)
001219 :  FC*002  :   , ENTRY ,.             BEI WRITE(LUN),READ(LUN) LISTE (BIN)
001220 : C                                   EIN- AUSGABE EINER C-ZAHL (2WORTE)
001221 :           14 , UTM   , 1
001222 :              , UJ    , IOC4
001223 :  IOC2    :   , ISO   , 12H IOCONT* ''''''
001224 :  IOC3    :   , ISO   , 12H IOAC* ''''''''''
001225 : C
001226 : C** FPACK-IOEND*
001227 : C                  BENUTZERROUTINE ZUR BEENDIGUNG DER LAUFENDEN EIN-
001228 : C                  AUSGABE
001229 : C
001230 :  I2      :   , EQUAL , 8
001231 :  IOEND*  :   , ENTRY ,
001232 :  FT*643  :   , ENTRY ,.             ABSCHLUSS BEI READ(LUN) LISTE
001233 :  FT*723  :   , ENTRY ,.             ABSCHLUSS BEI WRITE(LUN) LISTE
001234 :  FT*003  :   , ENTRY ,.             ABSCHLUSS SONST
001235 :           14 ,  VJM  , RGTRI13*
001236 :              , XTS   ,
001237 :  IO1     :I2 , ATX   , L/NLGLRE
001238 :           I2 , XTA   , ER/AB
001239 :              ,  UZA  , IO2
001240 :              , XTA   ,
001241 :           I2 , ATX   , ER/AB
001242 :            A , ATX   , FORMAT
001243 :           I2 , WTC   , ADEND
001244 :              , UJ    ,
001245 :  '--------------------------------
001246 :  IO2     :I2 ,  XTA  , L/NLGLRE
001247 :           13 ,  U1A  ,
001248 :              ,  UJ   , RGTRSPA*
001249 : C
001250 : C** FPACK-FPF*
001251 : C                  FEHLERDRUCK-ROUTINE
001252 : C
001253 : C
001254 :  FPF*FRTS:   , ITA   , RR
001255 :            RR, VJM   , FPFRETT
001256 :            RR, VTM   , FPFLAD             FORTSETZUNG
001257 :              , UJ    , FPFTXTDR
001258 : C---------------------------------------------------------------------
001259 :  FPF*ABBR:   , ITA   , RR
001260 :            RR, VJM   , FPFRETT
001261 :            RR, VTM   , STOP               ABBRUCH
001262 :              , UJ    , FPFTXTDR
001263 : C---------------------------------------------------------------------
001264 :              , ITA   , RR
001265 :            RR, VJM   , FPFRETT
001266 :            RR, VJM   , FPFTXTDR
001267 :            RR, VTM   , STOP               ABBRUCH,RECORD
001268 :            A , XTA   , CENC
001269 :              , U1A   , FPFDCEC            SPR BEI ENCODE/DECODE
001270 :            A , UTC   , RECORD - 1
001271 :           13 , VTM   ,
001272 :            C , XTA   , KRECORD
001273 :           13 , ATX   ,
001274 :              , ITS   , 13
001275 :           13 , UTM   , 24
001276 :              , ITS   , 13
001277 :              , XTS   ,
001278 :           RR , MTJ   , 13
001279 :              , UJ    , PRINT8             DRUCK DES EINGABE-RECORDS
001280 :  FPFDCEC : A , WTC   , BEGREC
001281 :           NWO, VTM   , -1
001282 :           NWO, XTA   ,
001283 :            C , XTS   , KRECORD
001284 :           NWO, ATX   ,
001285 :              , ITA   , NWO
001286 :           A  , WTC   , LUN
001287 :           13 , VTM   , -1
001288 :              , ITS   , 13
001289 :              , XTS   ,
001290 :              , CALL  , PRINT8
001291 :           NWO, *75   ,
001292 :           RR ,  UJ   ,
001293 : C--------------------------------------------------------------------
001294 :  STOP    : A , XTA   , FNR
001295 :            C , AAX   , BIT14
001296 :              , U1A   , STOP**      .SPR, BEI UNBED ABBRUCH-FEHLER
001297 :            A , XTA   , FNR
001298 :            C , AAX   , BIT15
001299 :              , AOX   , CHECKFL*
001300 :              , U1A   , FPFLAD      .SPR, BEI UNB FORTS-FEHLER ODER IM
001301 : C                                   CHECK-REGIME
001302 :  STOP**  :R3 , VTM   , LREG
001303 :              , ITA  ,  R3
001304 :            S , ATX   , -6          .STELLEN RUECKKEHR-REG FUER FPFLAD
001305 :              , UJ    , FPFLAD
001306 :  LREG    : A , WTC   , II13
001307 :           R3 , VTM   ,
001308 :            A , WTC   , II15
001309 :            S , VTM   ,
001310 :           I11, VJM   , RGTAUSCH    .WIEDERHERSTELLEN DER GLOBALEN REG
001311 :           R4 , VJM   , SUBPERR*
001312 :  FPF2    :   , OCT   ,
001313 :  SUBPERR*:,BSS,
001314 :  ,WTC,JERRIO*
001315 :  X,VTM,
001316 :  X,UTC,
001317 :  X,VIM,
001318 :              , UJ    , STOP*
001319 : C---------------------------------------------------------------------
001320 :  FPFRETT :   , ITS   , NWO                REG - RETTEN
001321 :           NWO, VTM   , - 8
001322 :          :NWO, ITS   , 14                 RETTEN  I6,...,I14
001323 :           NWO, VLM   , *
001324 :            A , XTS   , SYMB
001325 :            A , XTS   , NSP3
001326 :            A , XTS   , ZHLR
001327 :            S , ATX   ,
001328 :           RR , UJ    ,
001329 :  '--------------------------------
001330 :  FPFTXTDR: S , MTJ   , MAG                RETTEN MAGAZIN-ZAEHLER
001331 :            C , XTA   , FTXT
001332 :            C , XTS   , FTXT+1             EINSPEICHERN:
001333 :            S , ATX   ,                    'I/O-ERROR '
001334 :           MAG, WTC   , -6
001335 :              , XTA   , -1
001336 :            A , ATX   , FNR
001337 :            C , AAX   , A377
001338 :           C  , ATX   , FPF2-CONST
001339 :              , ASN   , 64-39
001340 :            C , AUX   , VMASKE1
001341 :            C , AOX   , VW1
001342 :            C , XTS   , FTXT+2             FEHLER-NR
001343 :            C , XTS   , FTXT+3
001344 :            A , WTC   , ADNBPROG
001345 :            X , VTM   ,
001346 :            X , XTS   ,                    ' DETECTED IN '
001347 :            X , XTS   , 1
001348 :            C , XTS   , FTXT+4             PROG-NAME
001349 :            C , XTS   , FTXT+5
001350 :            A , XTS   , II13               'CALLED FROM '
001351 :            C , ARX   , A77777
001352 :              , ASN   , 64-33
001353 :            C , AUX   , VMASKE2
001354 :            C , AOX   , VW2
001355 :            A , XTS   , ER/AB              CALL-ADRESSE
001356 :              , UZA   , FPFDR1             SPR, WENN KEINE EROEFFNUNG
001357 :            A , XTA   , CENC
001358 :              , U1A   , ADD/SPEC           SPR, BEI ENCODE/DECODE
001359 :            C , XTA   , KLUN
001360 :            A , XTS   , LUN                ' LUN '
001361 :   C,AAX,BIT1/7
001362 :   C,AOX,EX640
001363 :   ,NTR,2
001364 :   S,ATX,
001365 :   C,A/X,E1
001366 :   ,NTR,3
001367 :   C,A+X,EX640      .FIRST DIGIT
001368 :   S,ATX,
001369 :   ,NTR,2
001370 :   C,A*X,E1
001371 :   S,X-A,-2
001372 :   ,NTR,3
001373 :   C,A+X,EX640
001374 :   S,STX,-2
001375 :   C,AOX,A60
001376 :   ,ASN,64-8
001377 :   S,AOX,
001378 :   C,AOX,A60
001379 :   ,NTR,6
001380 :              , ASN   , 64-32
001381 :            S , ATX   ,                    LUN DEZIMAL
001382 :  ADD/SPEC: A , XTA   , KOFSPEC
001383 :              , U1A   , SPEC               SPR, BEI FORMAT-FEHLER
001384 :            A , XTA   , FNR
001385 :            C , AAX   , BIT10
001386 :            A , AOX   , AC
001387 :              , U1A   , FT/NOFT            SPR, BEI AUFRUF VON IOAC*
001388 :            C , XTA   , KADDRESS                UND BESOND MARKIERT FHLRN
001389 :            C , XTS   , KADDRESS+1
001390 :            A , XTS   , TRWADR             ' ADRESS '
001391 :              , ASN   , 64-33
001392 :            C , AUX   , VMASKE2
001393 :            C , AOX   , VW2
001394 :            S , ATX   ,                    ADRESSE D EIN-AUSZUGEBENDEN
001395 :  FT/NOFT : A , XTA   , FORMAT                                  WORTES
001396 :              , UZA   , FPFDR1             SPR, BEI BINAER-I/O
001397 :            C , XTA   , KFORMAT
001398 :            C , XTS   , KFORMAT+1
001399 :            S , ATX   ,                    ' FORMAT'
001400 :  FPFDR1  : C , XTA   , EOR
001401 :            A , XTS   , FORMAT             'EOR'
001402 :  FPFDR   :   , ITS   , MAG
001403 :              , ITS   , S
001404 :              , XTS   ,
001405 :              , CALL  , PRINT8
001406 :           MAG, MTJ   , S
001407 :           RR , UZA   ,
001408 :  '--------------------------------
001409 :            A , XTA   , FORMAT
001410 :            C , AOX   , BIT46/48
001411 :            A , ATX   , ZHLR
001412 :            K , VTM   , 1
001413 :            L , VTM   , 125
001414 :              , XTA   ,
001415 :           A  , ATX   , NSP3
001416 :            S , ATX   ,
001417 :            X , VTM   , -5
001418 :  FPFZYK2 :R2 , VJM   , READSY3
001419 :              , ATI   , SY
001420 :           SY ,V1M    , STORE       .FALLS SYMB VERSCHIEDEN NULLCODE
001421 :           SY , VTM   , 36B         .ERSETZEN NULLCODE DURCH '
001422 :  STORE   :S  , XTA   ,
001423 :              , ASN   , 64-8
001424 :              , ITS   , SY
001425 :            C , AAX   , A377
001426 :            S , AEX   ,
001427 :            S , ATX   ,
001428 :            X , VLM   , FPFF        .FALLS NOCH NICHT 6 ZEICHEN IM WORT
001429 :              , XTA   ,
001430 :            S , ATX   ,
001431 :            X , VTM   , -5
001432 :  FPFF    :   , BSS   ,
001433 :            L , UTM   , -1
001434 :            L , VZM   , FPFDR3
001435 :           SY , UTM   , -50 B              SYMB = (
001436 :           SY , VZM   , FPFKLA
001437 :           SY , UTM   , 50 B - 51 B        SYMB = )
001438 :           SY , V1M   , FPFZYK2
001439 :            K , VLM   , FPFZYK2
001440 :  FPFDR3    : C , XTA   , EOR
001441 :              , XTS   ,
001442 :              , UJ    , FPFDR
001443 :  '--------------------------------
001444 :  FPFKLA  : K , UTM   , -1
001445 :              , UJ    , FPFZYK2
001446 :  FPFLAD  : S , XTA   ,
001447 :            A , STX   , ZHLR
001448 :           A  , STX   , NSP3
001449 :            A , STX   , SYMB
001450 :           RR , VTM   , 9
001451 :          :RR , STI   , 5                  LADEN  I14,...,I6
001452 :           RR , UTM   , -1
001453 :           RR , V1M   , *-1
001454 :              , STI   , NWO
001455 :              , ATI   , RR
001456 :           R2 , UJ    ,
001457 :  '--------------------------------
001458 :  SPEC    : C , XTA   , KSPEC
001459 :            C , XTS   , KSPEC+1
001460 :            C , XTS   , KSPEC+2            EINSPEICHERN:
001461 :            S , ATX   ,                    ' SPECIFICATION '
001462 :            A , XTA   , KOFSPEC
001463 :            A , ATX   , ZHLR
001464 :             I, VTM   , -5
001465 :            R2, VTM   , FPFZYK + 1
001466 :              , UJ    , READSY3F
001467 :  FPFZYK  : R2, VJM   , READSY3
001468 :             C, AAX   , A377
001469 :              , U1A   , FPFN1       .FALLS SYMB VERSCHIEDEN NULLCODE
001470 :           SY , VTM   , 36B
001471 :              , ITA   , SY
001472 :  FPFN1   :   , BSS   ,
001473 :            S , ATX   ,
001474 :             I, VLM   , FPFZYK
001475 :              , UJ    , FT/NOFT
001476 : C
001477 : C** FPACK-SETLUN*
001478 : C                  BEREITSTELLEN VON LUN* ( LUN* = NR DER LOG EINHEIT,
001479 : C                  DIE AM ENDE EINER BENUTZER-EQUIVALENZKETTE STEHT )
001480 : C
001481 :  LU*     :   , EQUAL ,  5
001482 :  STL     :   , EQUAL , 14
001483 :  SETLUN* :   , BSS   ,
001484 :           LMU, VTM   , LUNMUN - 1
001485 :  SL1     :LMU, UTM   , 1                  BEREITSTELLEN DER LUN, DIE
001486 :           LMU, XTA   ,                    AM ENDE EINER BENUTZER-
001487 :              , UZA   , SL2                EQUIVALENZKETTE STEHT
001488 :            C , AAX   , BIT1/7
001489 :            C , AOX   , EX640                                           S
001490 :            A , AEX   , LUN
001491 :              , U1A   , SL1
001492 :           LMU, XTA   ,
001493 :              , ASN   , 64 + 24
001494 :            C , AAX   , BIT1/7
001495 :              , U1A   , SL3
001496 :  SL2     : A , XTA   , LUN
001497 :  SL3     : A , ATX   , LUN*
001498 :              , ATI   , LU*
001499 :           STL, VTM   , 1                  STLUN := 1
001500 :            A , XTA   , I/O
001501 :              , U1A   , SLAUSG             SPR, WENN AUSGABE
001502 :           LU*, UTM   , -55
001503 :           LU*, VZM   , SL4                SPR,WENN LUN* = 55  LK-LESER2
001504 :           LU*, UTM   , 55 - 50
001505 :           LU*, VZM   , SL4                SPR, WENN LUN* = 51  LK-LESER1
001506 :  STL,VTM,2
001507 :  LU*,UTM,50-54
001508 :  LU*,VZM,SL4.TEPMИHAЛ
001509 :  SL3/1:,BSS,
001510 :  LU*,UTM,53
001511 :  ,ITA,LU*
001512 :  C,AAX,A60
001513 :  ,U1A,I/O*ERR
001514 :           STL, VTM   ,                    STLUN := 0
001515 :  SL4     :   , ITA   , STL
001516 :            A , ATX   , STLUN
001517 :           R3 , UJ    ,
001518 : C----------------------------------------------------------------------
001519 :  SLAUSG  :LU*, UTM   , -51                TEST AUF STLUN BEI AUSGABE
001520 :           LU*, VZM   , SL4                SPR, WENN LUN* = 51 DRUCKER1
001521 :           STL, VTM   , 5                  STL := 5
001522 :           LU*, UTM   , 51 - 56
001523 :           LU*, VZM   , SL4                SPR, WENN LUN* = 56 DRUCKER2
001524 :           STL, VTM   , 2                  STL := 2
001525 :           LU*, UTM   , 56 - 52
001526 :           LU*, VZM   , SL4                SPR, WENN LUN* = 52 STANZER1
001527 :           STL, VTM   , 6                  STL := 6
001528 :           LU*, UTM   , 52 - 57
001529 :           LU*, VZM   , SL4                SPR, WENN LUN* = 57 STANZER2
001530 :           STL, VTM   , 3                  STL := 3
001531 :           LU*, UTM   , 57 - 53
001532 :           LU*, VZM   , SL4                SPR, WENN LUN* = 53 LBSTANZER1
001533 :           STL, VTM   , 7                  STL := 7
001534 :           LU*, UTM   , 53 - 58
001535 :           LU*, VZM   , SL4                SPR, WENN LUN* = 58 LBSTANZER2
001536 :           STL, VTM   , 4                  STL := 4
001537 :           LU*, UTM   , 58 - 54
001538 :           LU*, VZM   , SL4                SPR, WENN LUN* = 54 TTYPE
001539 :  ,UJ,SL3/1
001540 : C
001541 : C** FPACK-READSY2
001542 : C                  UEBERGABE EINES ZEICHENS AUS RECORD ODER FORMAT
001543 : C
001544 :  I7      :   , EQUAL , 7
001545 :  SP      :   , EQUAL , CT + 32
001546 :  RS1     :   ,  BSS  ,
001547 :  READSY2 :   , BSS   ,
001548 :           NSG, VZM   , RSEND2
001549 :           NSG, UTM   , -1
001550 :           I2 , XTA   , NSP
001551 :              , U1A   , RSSPACE
001552 :           I7 , VZM   , RSNW
001553 :           I7 , UTM   , 1
001554 :  RS2     :NWO, XTA   ,
001555 :           I1 , UTC   , VGR
001556 :           I7 , ASX   , 5
001557 :           I1 , AAX   , A377
001558 :           I2 , ATX   , SYMB
001559 :           I2 , XTA   , CENC
001560 :              , U1A   , RSC0
001561 :           I2 , XTA   , SYMB
001562 :           I1 , AEX   , EOR
001563 :              , UZA   , RSFEHL
001564 :  RS3     :I2 , XTA   , SYMB
001565 :           I1 , AAX   , BIT8
001566 :              , U1A   , RSSPACE2
001567 :           I2 , XTA   , SYMB
001568 :  RS30    :I1 , AAX   , BIT7
001569 :              , U1A   , RSCTL
001570 :           I2 , WTC   , SYMB
001571 :           I1 , XTA   , CT
001572 :  RS4     :I2 , ATX   , SYMB
001573 :           I12, UJ    ,
001574 :  '--------------------------------
001575 :  RSEND2  :I2 , WTC   , AUSG2
001576 :              , UJ    ,
001577 :  '--------------------------------
001578 :  RSSPACE :I1 , ARX   , A77777
001579 :           I1 ,  AAX  , A77777
001580 :           I2 , ATX   , NSP
001581 :           I1 , XTA   , SP
001582 :           I2 , ATX   , SYMB
001583 :           I12, UJ    ,
001584 :  '--------------------------------
001585 :  RSNW    :NWO, UTM   , 1
001586 :  RS5     :I7 , VTM   , -5
001587 :              , UJ    , RS2
001588 :  '--------------------------------
001589 :  RSC0    :I2 , XTA   , NSYMB
001590 :              , UZA   , RSFEHL
001591 :           I1 , ARX   , A77777
001592 :           I1 ,  AAX  , A77777
001593 :           I2 , ATX   , NSYMB
001594 :              , UJ    , RS3
001595 :  '--------------------------------
001596 :  RSFEHL  :12 , MTJ   , I7
001597 :          :12 , 31    , FPF*FRTS.  ES WIRD MEHR AUS EINEM RECOR GELESEN,
001598 :              , UTC   , 40204 B   .ALS ENTHALTEN IST. AUTOMATISCHER
001599 :           I7 , MTJ   , 12.        UEBERGANG ZUM NAECHSTEN RECORD
001600 :              ,  ITA  , 13
001601 :           15 , ATX   ,
001602 :           I2 , WTC   , ADR/W
001603 :           13 , VJM   ,
001604 :           15 , XTA   ,
001605 :              , ATI   , 13
001606 :              , UJ    , RS5
001607 :  '--------------------------------
001608 :  RSSPACE2:I2 , XTA   , SYMB
001609 :           I1 , AAX   , BIT1/7
001610 :              , U1A   , RSSPACE
001611 :              ,  UJ   , READSY2     .WIRD ERREICHT, WENN SYMB = 200
001612 :  '--------------------------------
001613 :  RSCTL   :I2 ,  XTA  , SYMB
001614 :           I1 , AAX   , BIT1/6
001615 :           I2 , ATX   , SYMB
001616 :           I2 , WTC   , SYMB
001617 :           I1 , XTA   , CT
001618 :              , ASN   , 64 + 24
001619 :              , UJ    , RS4
001620 :  '--------------------------------
001621 :  READSY2F:   ,  ITA  , I12
001622 :           15 ,  ATX  ,
001623 :  RS6     :NSG,  VZM  , RSEND3
001624 :           I12, VJM   , RS1
001625 :           I1 , AEX   , SP
001626 :              , UZA   , RS6
001627 :           I2 , XTA   , SYMB
001628 :           15 ,  WTC  ,
001629 :              ,  UJ   ,
001630 :  '--------------------------------
001631 :  RSZRZST :   , BSS   ,
001632 :           I7 ,  UTM  , 5
001633 :           I7 ,  VZM  , RS7
001634 :           I7 ,  UTM  , -6
001635 :           I12,  UJ   ,
001636 :  '--------------------------------
001637 :  RS7     :NWO,  UTM  , -1
001638 :           I12,  UJ   ,
001639 :  '--------------------------------
001640 :  RSEND3  :15 ,  XTA  ,
001641 :              ,  UJ   , RSEND2
001642 :  READSY3 :   , BSS   ,
001643 :            A , XTA   , NSP3
001644 :              , U1A   , SPOUT
001645 :            A , XTA   , ZHLR
001646 :            C , ARX   , NBIT45
001647 :              , U1A   , NZEILE             SPR, WENN BIT48 = 0
001648 :  RDS31   : A , ATX   , ZHLR
001649 :            A , WTC   , ZHLR
001650 :              , XTA   ,
001651 :            A , ASX   , ZHLR
001652 :            C , AAX   , A377
001653 :            A , ATX   , SYMB
001654 :            C , AAX   , BIT8
001655 :              , U1A   , SPACES3
001656 :            A , XTA   , SYMB
001657 :              , UJ    , RS30
001658 :  NZEILE  : C , ARX   , NZL
001659 :              , UJ    , RDS31
001660 :  SPACES3 : A , XTA   , SYMB
001661 :            C , AAX   , BIT1/7
001662 :              , UZA   , READSY3
001663 :  SPOUT   : C , ARX   , A377
001664 :            C , AAX   , A377
001665 :            A , ATX   , NSP3
001666 :            C , XTA   , SP
001667 :            A , ATX   , SYMB
001668 :           R2 , UJ    ,
001669 : C-----------------------------------
001670 :  READSY3F:   , ITA   , R2                 LESEN DES NAECHSTEN VON SPACE
001671 :            S , ATX   ,                    VERSCHIEDENEN ZEICHENS
001672 :  RDS32   :R2 , VJM   , READSY3
001673 :            C , AEX   , SP
001674 :              , UZA   , RDS32
001675 :            A , XTA   , SYMB
001676 :            S , WTC   ,
001677 :              , UJ    ,
001678 : C-----------------------------------
001679 :  RSZRZST3: A , XTA   , ZHLR               ZHLR UM 1 POS ZURUECKSTELLEN
001680 :            C , ARX   , BIT45
001681 :            A , ATX   , ZHLR
001682 :            C , ARX   , BIT46
001683 :           R2 , UZA   ,                    SPR, WENN BIT48 = L
001684 :            C , ARX   , BIT2/47
001685 :            A , ATX   , ZHLR
001686 :           R2 , UJ    ,
001687 : C
001688 : C** FPACK-RGTRSPR*
001689 : C                  REGISTERTAUSCH UND RUECKSPR INS BENUTZERPROGRAMM
001690 : C
001691 :  I9      :   , EQUAL , 9
001692 :  RGTRSPA*:   , BSS   ,
001693 :           I11,  VJM  , RGTAUSCH
001694 :  RGT0    : 15, XTA   ,
001695 :              ,  NTR  , 2
001696 :           I8 ,  WTC  , II13
001697 :              ,  UJ   ,
001698 :  RGTRI13*:   , BSS   ,
001699 :           I8 ,  VTM  , ASP
001700 :           I8 ,  XTS  , KBASP
001701 :           I11,  VTM  , RGT1
001702 :              ,  UZA  , RGTAUSCH
001703 :           I8 ,  UTM  , 256
001704 :              ,  UJ   , RGTAUSCH
001705 :  RGT1    :   ,  ITA  , 13
001706 :           I8 ,  STX  , II13
001707 :           I1 ,  VTM  , CONST
001708 :           I14,  UJ   ,
001709 :  RGTAUSCH:I9 ,  VTM  , -6
001710 :  RGT2    :I8 ,  UTC  , GLREG
001711 :           I9 ,  XTA  , 6
001712 :           I9 ,  ITS  , 7
001713 :           I8 ,  UTC  , GLREG
001714 :           I9 ,  STX  , 6
001715 :           I9 ,  ATI  , 7
001716 :           I9 ,  VLM  , RGT2
001717 :           I11,  UJ   ,
001718 : C----------------------------------
001719 :  RSPLI15*:  A, WTC   , II15        .RUECKSPRUNG MIT ZURUECKSTELLEN DES
001720 :            S , VTM   ,             .MAG-ZAEHLERS UND LADEN DER GLOB REG
001721 :           I11, VJM   , RGTAUSCH
001722 :            A , WTC   , II13
001723 :              , UJ    ,
001724 : C
001725 : C** FPACK-RI15
001726 : C                  RETTEN I15 NACH II15
001727 : C
001728 : C
001729 :  RI15    : S ,ATX   ,
001730 :            S ,UTM   , -2
001731 :              , ITA   , S
001732 :            S , UTM   , 2
001733 :            A , STX   , II15
001734 :           R3 , UJ    ,
001735 :  CONST   :   , BSS   ,
001736 :              ,  LOG  , 1                   A1
001737 :              , LOG   , 13                 A13
001738 :              ,  LOG  , 17                  A17
001739 :              , LOG   , 1750                A1750 = 1000
001740 :              ,  LOG  , 1770 0             A17700
001741 :              ,  LOG  , 2007 7             A20077
001742 :              ,  LOG  , 377                 A377
001743 :              , LOG   , 5                   A5
001744 :              , LOG   , 60                 A60
001745 :              ,  LOG  , 77777               A77777
001746 :              , LOG   , 77                  BIT1/6
001747 :              , LOG   , 177                 BIT1/7
001748 :              , OCT   , 7777 7777 7777 7776 BIT2/48
001749 :              ,  LOG  , 1777 7777 7777 77   BIT1/40
001750 :              , LOG   , 3777 7777 7777 77  BIT1/41
001751 :              ,  LOG  , 3000                BIT10/11
001752 :              , LOG   , 3700 0              BIT10/14
001753 :              ,  OCT  , 7777 7777 7777 7777 BIT1/48
001754 :              ,  LOG  , 4000                BIT12
001755 :              ,  LOG  , 1000               BIT10
001756 :              , LOG   , 3400 0              BIT12/14
001757 :              , LOG   , 7400 0              BIT12/15
001758 :              ,  LOG  , 2 0000             BIT14
001759 :              , LOG   , 4000 0              BIT15
001760 :              , LOG   , 1000 00             BIT16
001761 :              , LOG   , 2000 00            BIT17
001762 :              , OCT   , 7777 7777 7777      BIT13/48
001763 :              , OCT   , 0037 7777 777      BIT16/41
001764 :              ,  LOG  , 14                  BIT3/4
001765 :              ,  OCT  , 7777                BIT37/48
001766 :              ,  OCT  , 776                 BIT41/48
001767 :              , OCT   , 77                  BIT43/48
001768 :              ,  OCT  , 4                   BIT48
001769 :              , LOG   , 100                 BIT7
001770 :              , LOG   , 200                 BIT8
001771 :              , LOG   , 7760 0             BIT8/15
001772 :              , OCT   , 7777 7777 7777 74   BIT9/48
001773 :              , OCT   , 40                 VGR(-1) = VGR0
001774 :              , OCT   , 64                 VGR(0)
001775 :              ,  OCT  , 60                  VGR(1)
001776 :              ,  OCT  , 54                  VGR(2)
001777 :              ,  OCT  , 50                  VGR(3)
001778 :              ,  OCT  , 44                  VGR(4)
001779 :              ,  OCT  , 40                  VGR(5)
001780 :              ,  OCT  , 00000 100 00000 000 HAERTEZ.       FREI
001781 :              ,  OCT  , 11020 101 00000 001 A              FREI
001782 :              ,  OCT  , 00000 102 00000 002 B              FREI
001783 :              ,  OCT  , 00000 103 00000 003 C              FREI
001784 :              , OCT   , 06004 104 00000 004 D              FREI
001785 :              ,  OCT  , 04004 105 00000 005 E              FREI
001786 :              ,  OCT  , 05010 106 00000 006 F              FREI
001787 :              ,  OCT  , 00000 107 00000 007 G              FREI
001788 :              ,  OCT  , 02040 110 00000 010 H              FREI
001789 :              ,  OCT  , 07020 111 00000 011 I              FREI
001790 :              ,  OCT  , 00000 112 00000 012 J              END OF RECORD
001791 :              ,  OCT  , 00000 113 00000 013 K              FREI
001792 :              ,  OCT  , 12020 114 00000 014 L              FREI
001793 :              ,  OCT  , 00000 115 00000 015 M              FREI
001794 :              ,  OCT  , 00000 116 00000 016 N              FREI
001795 :              ,  OCT  , 10020 117 00000 017 O              FREI
001796 :              ,  OCT  , 03040 120 00000 020 P              OR
001797 :              ,  OCT  , 00000 121 00000 021 Q              AND
001798 :              ,  OCT  , 00000 122 00000 022 R              IMPLIKATION
001799 :              ,  OCT  , 00000 123 00000 023 S              NEGATION
001800 :              ,  OCT  , 00000 124 00000 024 T              INEGERDIVISION
001801 :              ,  OCT  , 00000 125 00000 025 U              IDENTITAET
001802 :              ,  OCT  , 00000 126 02100 026 V              '(APOSTROPH)
001803 :              ,  OCT  , 00000 127 00000 027 W              BASIS-ZEHN
001804 :              ,  OCT  , 01040 130 00000 030 X              UNGLEICH
001805 :              ,  OCT  , 00000 131 00000 031 Y              KLEINER-GLEICH
001806 :              ,  OCT  , 00000 132 00000 032 Z              GROESSERGLEICH
001807 :              ,  OCT  , 00000 133 00000 033 ECKIGE KL. AUF STRICH OBEN
001808 :              ,  OCT  , 00000 134 00000 034                STRICH UNTEN
001809 :              ,  OCT  , 00000 135 00000 035 ECKIGE KL. ZU
001810 :              ,  OCT  , 00000 136 02100 036 POTENZ (PFEIL) '(APOSTROPH)
001811 :              ,  OCT  , 00000 137 00000 037 STRICH         GRAD
001812 :              , OCT   , 00000 140 00000 040 JU             SPACE
001813 :              ,  OCT  , 00000 141 00000 041 A              AUSRUFEZEICHEN
001814 :              ,  OCT  , 00000 142 00000 042 B RUSS         ANFUEHRUNGSZ.
001815 :              ,  OCT  , 00000 143 00000 043 Z RUSS         NUMMER
001816 :              ,  OCT  , 00000 144 00000 044 D RUSS         DOLLAR
001817 :              ,  OCT  , 00000 145 00000 045 E              PROZENT
001818 :              ,  OCT  , 00000 146 00000 046 F RUSS         STRICH SENKR.
001819 :              ,  OCT  , 00000 147 02100 047 G RUSS         '(APOSTROPH)
001820 :              ,  OCT  , 00000 150 40000 050 X              (
001821 :              ,  OCT  , 00000 151 13000 051 I RUSS         )
001822 :              ,  OCT  , 00000 152 00000 052 I KURZ         *
001823 :              ,  OCT  , 00000 153 03200 053 K              +
001824 :              ,  OCT  , 00000 154 00000 054 L RUSS         , (KOMMA)
001825 :              ,  OCT  , 00000 155 03200 055 M              - (MINUS)
001826 :              ,  OCT  , 00000 156 00000 056 H              .
001827 :              ,  OCT  , 00000 157 00000 057 O              /
001828 :              ,  OCT  , 00000 160 00001 060 P RUSS         0
001829 :              ,  OCT  , 00000 161 00001 061 JA             1
001830 :              ,  OCT  , 00000 162 00001 062 P              2
001831 :              ,  OCT  , 00000 163 00001 063 C              3
001832 :              ,  OCT  , 00000 164 00001 064 T              4
001833 :              ,  OCT  , 00000 165 00001 065 Y              5
001834 :              ,  OCT  , 00000 166 00001 066 DSCH           6
001835 :              ,  OCT  , 00000 167 00001 067 B              7
001836 :              ,  OCT  , 00000 170 00002 070 WEICHHEITSZ.   8
001837 :              ,  OCT  , 00000 171 00002 071 UI             9.
001838 :              ,  OCT  , 00000 172 00000 072 SE             :
001839 :              ,  OCT  , 00000 173 00000 073 SCHA           SEMIKOLON
001840 :              ,  OCT  , 00000 174 00000 074 E KURZ OFFEN   KLEINER
001841 :              ,  OCT  , 00000 175 00000 075 SCHTSCH        =
001842 :              ,  OCT  , 00000 176 00000 076 TSCH           GROESSER
001843 :              ,  OCT  , 00000 177 00000 077 ERASE          FRAGEZEICHEN
001844 :              , REAL  , 1.                  DEINS
001845 :              , OCT   ,
001846 :              , REAL  , 10.                 D10
001847 :              , OCT   ,
001848 :              , REAL  , 0.5                 D1/2
001849 :              , OCT   ,
001850 :              , REAL  , 1.25               REAL5/4
001851 :              , OCT   , 4310 7033 6233 7404R5/4H16
001852 :   ,REAL,1.E-18
001853 :   ,REAL,1.E-16
001854 :   ,REAL,1.E-14
001855 :   ,REAL,1.E-12
001856 :   ,REAL,1.E-10
001857 :   ,REAL,1.E-8
001858 :   ,REAL,1.E-6
001859 :   ,REAL,1.E-4
001860 :   ,REAL,1.E-2
001861 :   ,REAL,1.
001862 :   ,REAL,1.E2
001863 :   ,REAL,1.E4
001864 :   ,REAL,1.E6
001865 :   ,REAL,1.E8
001866 :   ,REAL,1.E10
001867 :   ,REAL,1.E12
001868 :   ,REAL,1.E14
001869 :   ,REAL,1.E16
001870 :   ,REAL,1.E18
001871 :              , REAL  , 6.                 SECHS
001872 :              ,  REAL , .1                 Z1/10
001873 :              , OCT   , 3654 6314 6314 6314 D1/10
001874 :              , OCT   , 0014 6314 6314 6316
001875 :              , OCT   , 4256 5431 2370 7032 D10E40
001876 :              , OCT   , 0116 2445 7725 6717 E40 TEIL 2
001877 :              , OCT   , 3610 5541 1423 5672 D10E*40 = 10E-40
001878 :              , OCT   , 0113 6342 6142 1450 E-40 TEIL 2
001879 :              ,  LOG  , 12                  EOR
001880 :              ,  ISO  , 6H'7''7''7'''''''   VMASKE1
001881 :              ,  ISO  , 6H'7''7''7''7''7''' VMASKE2
001882 :              ,  ISO  , 6H000''''''         VW1
001883 :              ,  ISO  , 6H00000B            VW2
001884 :              ,  LOG  , 02040 110           VH
001885 :              ,  LOG  , 03040 120           VP
001886 :              ,  LOG  , 40                  SPACE
001887 :              , OCT   , 645                EX645
001888 :              ,  ISO  , 6HREC              K3HREC
001889 :              , ISO   , 6HADD              K3HADD
001890 :              , LOG   , 100                MAXEXNZ
001891 :              , ISO   , 12HI/O-ERROR ''''  FTXT
001892 :              , ISO   , 12H DETECTED IN
001893 :              , ISO   , 12HCALLED FROM
001894 :              , ISO   , 6H LUN ''
001895 :              , ISO   , 18H SPECIFICATION ''''''
001896 :              , ISO   , 12H FORMAT'12'     KFORMAT
001897 :              , ISO   , 12H ADDRESS ''''''
001898 :              , OCT   , 7757 7777 7777 7777 GRZAHL
001899 :              , ISO   , 6H   '311''311''12'  ISOSP   149SPACES + EOR
001900 :              , OCT   , 7377 7777 7777 7777 NBIT45
001901 :              , OCT   , 1                   BIT46
001902 :              , OCT   , 04                  BIT45
001903 :              , OCT   , 3777 7777 7777 7776 BIT2/47
001904 :              , OCT   , 3000 0000 0000 0001 NZL
001905 :              , ISO   , 6HRECORD           KRECORD
001906 :              , LOG   , 4                   A4
001907 :              , OCT   , 7                   BIT46/48
001908 :  ASP     :   , BSS   , LASP
001909 :  ,END,
001910 :  IOCONTD*:   , NAME  ,               FPACK                     30.01.73
001911 :  R2      :   , EQUAL , 12
001912 :  IA      :   , EQUAL , 3
001913 :  IZ      :   , EQUAL , 4
001914 :  A       :   , EQUAL , 8
001915 :  R4      :   , EQUAL , 14
001916 :  R3      :   , EQUAL , 13
001917 :  IFT     :   , EQUAL , 5
001918 :  C       :   , EQUAL , 1
001919 :  *0      :   , EQUAL ,
001920 :  *0: , BLOCK , A1,     A13,    A17,    A1750,  A17700, A20077, A377
001921 :   ,CONT,A5,A60,A77777,BIT1/6,BIT1/7,BIT2/48  .06
001922 :      , CONT  , BIT1/40,BIT1/41,BIT10/11,       BIT10/14,       BIT1/48
001923 :      , CONT  , BIT12,  BIT10,  BIT12/14,       BIT12/15,       BIT14
001924 :      , CONT  , BIT15,  BIT16,  BIT17,  BIT13/48,       BIT16/41,
001925 :      , CONT  , BIT3/4,BIT37/48,        BIT41/48                      .03
001926 :      , CONT  , BIT43/48,       BIT48(0),       EXP64,  BIT7,         .03
001927 :      , CONT  , BIT8,   BIT8/15,BIT9/48,VGR0,   EX640(0),       VGR(6).10
001928 :   ,CONT,CT(64),REAL1(0),DEINS(2)     .66
001929 :   ,CONT,E1(0),D10(2),R1/2(0),D1/2(2),REAL5/4,R5/4H13
001930 :   ,CONT,EKSPO(19),
001931 :      , CONT  , SECHS,  Z1/10,  D1/10(2),       D10E40(2),            .06
001932 :      , CONT  , D10E*40(2),     EOR,                                  .03
001933 :      , CONT  , VMASKE1,VMASKE2,VW1,    VW2,    VH,     VP            .06
001934 :      , CONT  , SPACE,  EX645,  K3HREC, K3HADD,                       .04
001935 :      , CONT  , MAXEXNZ,FTXT(14),       GRZAHL, ISOSP,  NBIT45, BIT46 .19
001936 :      , CONT  , BIT45,  BIT2/47,NZL,    KRECORD,A4,     BIT46/48,     .06
001937 :  *0: , BLOCK , SYMB,   ENDE,  KUSP/USP,KNEXTLET,PW,  KWSY,     SFAKT,
001938 :      , CONT  , AUSG2,  D,      NSP,    CENC,   NSYMB                 .05
001939 :      , CONT  , II13,   ER/AB,  ADCONT, ADR/W,  ADEND,  FORMAT, LUN,
001940 :      , CONT  , I/O,    GLREG(7),       BEGREC, ADNBPROG,       (1)   .11
001941 :      , CONT  , RECORD(25),     GWF(5), EAD(5), FTT,    SWUEBERL, WF  .38
001942 :      , CONT  , W,      NIW,    KBASP,  STLUN,  AC,                   .05
001943 :      , CONT  , INOUT,  LUN*,   L/NLGLRE,       KOFSPEC,        TRWADR,
001944 :      , CONT  , FNR,    II15,   NSP3,   ZHLR,   FTBE/SONST,           .05
001945 :  FT2*    :   ,  SUBP ,
001946 :  IOCONT* :   , SUBP  ,
001947 :  FPF*    :   , EQU   , IOCONT* + 2
001948 :  RGTRSPA*:   , EQU   , IOCONT* + 9
001949 :  RGTRI13*:   , EQU   , IOCONT* + 10
001950 :  RI15    :   , EQU   , IOCONT* + 11
001951 :  RSPLI15*:   , EQU   , IOCONT* + 12
001952 :  IOCONTD*:   , BSS   ,
001953 :            R4, VJM   , RGTRI13*
001954 :             A, ATX   , ENDE
001955 :           R3 , VJM   , RI15
001956 :            R4, VTM   , CDNAME
001957 :              , ITA   , R4
001958 :             A, STX   , ADNBPROG           ADNBPROG:=AD(IOCONTD*)
001959 :              , ATI   , IA
001960 :              , NTR   , 3
001961 :             A, A-X   , ENDE
001962 :              , UZA   , CDF1
001963 :              , ASN   , 64 + 1
001964 :              , ATI   , IZ
001965 :            IZ, UTM   , 1                  IZ:=(A-E)/2+1
001966 :              , NTR   , 2                  IZ: ZAHL DER EIN- BZW AUSZU-
001967 :             A, XTA   , ER/AB              GEBENDEN D-ZAHLEN
001968 :            C , AEX   , BIT48
001969 :              , U1A   , CDF2
001970 :           A  , XTA   , KUSP/USP
001971 :              , UZA   , CDF4
001972 :  CD1     :   , ITA   , IA
001973 :           A  , ATX   , TRWADR
001974 :            R3, VJM   , FT2*
001975 :           IFT, MTJ   , R4
001976 :             A, WTC   , ADR/W
001977 :            R4, UTM   ,
001978 :            R3, VTM   , CD1
001979 :             C, AAX   , BIT3/4
001980 :            R4, UZA   ,                    SPR, WENN ORDN-SP VORLIEGT
001981 :           IFT, UTM   , -13 B
001982 :            R4, UTC   ,
001983 :           IFT, VZM   ,                    SPR, WENN FTEND VORLIEGT
001984 :           IFT, UTM   , 13 B - 6
001985 :           IFT, V1M   , CDF3               SPR, WENN SPFZIFIK NICHT D
001986 :  CD10    :   , ITA   , IA
001987 :              , ITS   , IZ
001988 :             A, XTS   , I/O
001989 :              , U1A   , CDA2
001990 :            R3, VJM   , RDWD               EINGABE
001991 :            15, XTA   ,
001992 :              , STI   , IZ
001993 :              , ATI   , IA
001994 :             A, XTA   , INOUT              UEBERGABE DER KONVERT ZAHL
001995 :            IA, ATX   ,
001996 :             A, XTA   , INOUT + 1
001997 :            IA, ATX   , 1
001998 :            IA, UTM   , 2
001999 :            IZ, VLM   , CD1
002000 :              , UJ    , RGTRSPA*
002001 :  CDA2    : IA, XTA   ,                    AUSGABE
002002 :             A, ATX   , INOUT              UEBERNAHME DER RUECKZUKONVERT
002003 :            IA, XTA   , 1                  ZAHL
002004 :             A, ATX   , INOUT + 1
002005 :            R3, VJM   , WDWD               AUSGABE
002006 :            15, XTA   ,
002007 :              , STI   , IZ
002008 :              , ATI   , IA
002009 :            IA, UTM   , 2
002010 :            IZ, VLM   , CD1
002011 :              , UJ    , RGTRSPA*
002012 : C----------------------------------------------------------------------
002013 :  CDF1    : R2, 31    , FPF*       .BEI IOCONTD*(A,E) IST A GROESSER
002014 :              , UTC   , 1225 B      .ODER GLEICH E
002015 :              , UJ    , RSPLI15*
002016 :  CDF2    : R2, 31    , FPF*       .AUFRUF VON IOCONTD* OHNE VORHERIGE
002017 :              , UTC   , 226 B.      EROEFFNUNG
002018 :              , UJ    , RSPLI15*
002019 :  CDF3    : R2, 31    , FPF*       .KEINE D-SPEZIFIKATION BEI IOCONTD*
002020 :              , UTC   , 227 B.
002021 :              , UJ    , CD10
002022 :  CDF4    :R2 , 31    , FPF*        .KEINE UMW-SPEC VORHANDEN, OBWOHL
002023 :              , UTC   , 21 231B     .EIN- AUSGABELISTE NICHT LEER
002024 : C----------------------------------------------------------------------
002025 :  FD*002  :   , ENTRY ,.         EIN- AUSGABE EINER D-ZAHL (2WORTE) BEI:
002026 :           14 , UTM   , 1.           PRINT FT,...       READ FT,...
002027 :              , ITS   , 14.          PUNCH FT,...
002028 :              , UJ    , IOCONTD*.    WRITE (LUN,FT)...  READ (LUN,FT)...
002029 : C                                   ENCODE (C,FTV)... DECODE (C,FT,V)..
002030 :  CDNAME  :   , ISO   , 12H IOCONTD* ''''
002031 : C
002032 : C** FPACK-RDWD
002033 : C                  KONVERTIERUNG TEIL 2
002034 : C                  KONVERTIERUNG DOPPELTGENAUER ZAHLEN
002035 : C
002036 : C
002037 :  RI      :   , EQUAL , 4
002038 :  VM      :   , EQUAL , 11
002039 :  IW      :   , EQUAL ,  6
002040 :  S       :   , EQUAL , 15
002041 :  EXP10A  :   , EQUAL , -5
002042 :  MANTN   :   , EQUAL , -4
002043 :  MANTM   :   , EQUAL , -3
002044 :  MANTH   :   , EQUAL , -2
002045 :  EXP10   :   , EQUAL , -1
002046 :  IVE     :   , EQUAL , 10
002047 :  KD      :   , EQUAL ,  9
002048 :  EX      :   , EQUAL ,  3
002049 :  IVM     :   , EQUAL ,  4
002050 :  EX500   :   , EQUAL , VGR + 3
002051 :  D*DI*D  :   , SUBP  ,
002052 :  D*MU*D  :   , SUBP  ,
002053 :  D*LN*D  :   , SUBP  ,
002054 :  D*LO*D  :   , SUBP  ,
002055 :  D*ST*D  :   , SUBP  ,
002056 :  KONV1*  :   , SUBP  ,
002057 :  RDWD    :   , BSS   ,
002058 :              , ITA   , R3
002059 :              , UTC   , RD1
002060 :              , XTS   ,                    RETTEN RUECKKEHR-R
002061 :            A , ATX   , AUSG2              STELLEN AUSG2 FUER READSY2
002062 :            A , WTC   , W
002063 :          IW  , VTM   ,                    STELLEN IW =I6 FUER READSY
002064 :            RI,  VTM  ,                    KON FUER KONV1*  DH REAL
002065 :  RD1     :   , 30    , KONV1*
002066 :              , UTC   , RD2
002067 : C----------------------------------------------------------------------
002068 :  RD2     :   , NTR   , 2                  BILDUNG DER D-ZAHL INOUT
002069 :             S, XTA   , MANTN              AUS MANTN,MANTM,MANTH
002070 :              , E+N   , 64+40
002071 :             S, XTS   , MANTM-1            MAG:=MANTN
002072 :              , E+N   , 64+40
002073 :              , E+N   , 64+40
002074 :             S, XTS   , MANTH-2            MAG:=MANTM
002075 :              , E+N   , 64+60
002076 :              , E+N   , 64+60
002077 :             S, A+X   ,                    MANTH+MANTM
002078 :             A, ATX   , INOUT
002079 :              , YTA   , 64-40
002080 :             S, A+X   ,                    HR(MANTH+MANTM)+MANTN
002081 :             A, A+X   , INOUT
002082 :             A, ATX   , INOUT
002083 :              , NTR   , 3
002084 :              , YTA   , 64 - 40
002085 :            C , AAX   , BIT1/40
002086 :            A , ATX   , INOUT + 1
002087 :            A , XTA   , INOUT
002088 :              , ASN   , 64 - 7
002089 :              , YTA   ,
002090 :            C , A+X   , BIT14
002091 :            A , XTS   , INOUT
002092 :            C , AAX   , BIT1/40
002093 :            S , XTS   , -2
002094 :            C , APX   , A20077
002095 :            S , AOX   ,
002096 :            A , STX   , INOUT
002097 :            C , APX   , A17700
002098 :            A , AOX   , INOUT + 1
002099 :            A , ATX   , INOUT + 1
002100 :            VM,  MTJ  , IVM                DOUBLPREC-ROUT ZERSTOEREN VM
002101 :            S , XTA   , EXP10
002102 :           IVE, VZM   , RD3                SPR, WENN VEXP = +
002103 :              , X-A   ,
002104 :  RD3     : IA, V1M   , RD14               SPR, WENN MANT PUNKT ENTHAELT
002105 :            A , A-X   , D                  BERUECKSICHTIGUNG VON D
002106 :  RD4     :KD , VTM   , 1                  K:=K2
002107 :              , U1A   , RD5                SPR, WENN EXP10 KLEINER 0
002108 :           KD , VTM   ,                    K:=K1
002109 :              , X-A   ,                    EXP10:=-EXP10
002110 :  RD5     :   , ATI   , EX                 IE:=EXP10
002111 :            C , UTC   , DEINS
002112 :           14 , VTM   ,
002113 :           13 , VJM   , D*LO*D
002114 :           EX , VLM   , RDZYKL
002115 :  RD6     :14 , VTM   , RDFAKT
002116 :           13 , VJM   , D*ST*D
002117 :            A , UTC   , INOUT
002118 :           14 , VTM   ,
002119 :           IVM, V1M   , RD17               SPR, WENN VMANT = -
002120 :           13 , VJM   , D*LO*D             LADEN  INOUT
002121 :  RD7     :14 , VTM   , RDFAKT
002122 :           KD , V1M   , RD18
002123 :           13 , VJM   , D*MU*D             INOUT * RDFAKT
002124 :  RD8     : A , UTC   , INOUT
002125 :           14 , VTM   ,
002126 :           13 , VJM   , D*ST*D
002127 :            S , UTM   , -5
002128 :              , NTR   , 2
002129 :            S , WTC   ,
002130 :              , UJ    ,
002131 : C----------------------------------------------------------------------
002132 :  RD14    : S , A+X   , EXP10A             BERUECKSICHTIGUNG DES DEZI-PKT
002133 :              , UJ    , RD4
002134 : C----------------------------------------------------------------------
002135 :  RDZYKL  : C , UTC   , D10                BERECHNUNG VON RDFAKT
002136 :           14 , VTM   ,
002137 :           13 , VJM   , D*MU*D
002138 :           EX , VLM   , RDZYKL
002139 :              , UJ    , RD6
002140 :  RD17    :13 , VTM   , RD7                LADEN  -INOUT
002141 :              , UJ    , D*LN*D
002142 : C----------------------------------------------------------------------
002143 :  RD18    :13 , VTM   , RD8
002144 :              , UJ    , D*DI*D             INOUT / RDFAKT
002145 : C----------------------------------------------------------------------
002146 :  RDFAKT  :   , BSS   , 2
002147 : C
002148 : C** FPACK-WDWD
002149 : C                  RUECKKONVERTIERUNG DOPPELTGENAUER ZAHLEN
002150 : C
002151 : C
002152 :  II      :   , EQUAL , 9
002153 :  IO      :   , EQUAL , 9
002154 :  ID      :   , EQUAL ,  3
002155 :   ZRE:,EQU,-9
002156 :  E       :   , EQUAL , -8
002157 :  M       :   , EQUAL , 11
002158 :  NVP     :   , EQUAL ,  3
002159 :  IE      :   , EQUAL , 4
002160 :  IK      :   , EQUAL , 10
002161 :  SF      :   , EQUAL , 10
002162 :  WE      :   , EQUAL , 12
002163 :  VZM     :   , EQUAL , -7
002164 :  Z       :   , EQUAL , -6
002165 :  Z1      :   , EQUAL , -5
002166 :  ZR      :   , EQUAL , -3
002167 :   E0:,EQU,EKSPO+9
002168 :  MI      :   , EQUAL , CT + 45
002169 :  NULL    :   , EQUAL , CT + 48
002170 :  PL      :   , EQUAL , CT + 43
002171 :  PT      :   , EQUAL , CT + 46
002172 :  SP      :   , EQUAL , CT + 32
002173 :  D*AD*D  :   , SUBP  ,
002174 :  D*SU*D  :   ,  SUBP ,
002175 :  D*MI*D  :   , EQUAL , D*SU*D
002176 :  NEXTLET*:   ,  SUBP ,
002177 :  RK*     :   ,  SUBP ,
002178 :  WSY*    :   ,  SUBP ,
002179 :  WDWD    :   , BSS   ,
002180 :              , ITA   , R3                 RETTEN R3
002181 :              , XTS   ,
002182 :   IE,VTM,-8
002183 :          :  S, ATX   , .         LOESCHEN: E  VZM  Z  Z1  Z2  ZR
002184 :            IE, VLM   , *.                  ZY  ZY1  IE
002185 :             A, ATX   , KNEXTLET
002186 :             A, UTC   , INOUT
002187 :            IO, VTM   ,
002188 :            IO, MTJ   , 14
002189 :            13, VJM   , D*LO*D
002190 :              , UZA   , WD20               SPR, WENN INOUT = 0
002191 :              , NTR   , 22 B
002192 :              , UZA   , WD1                SPR, WENN INOUT POSITIV
002193 :            S , ATX   , VZM                VZM:= -
002194 :           IO , MTJ   , 14
002195 :           13 , VJM   , D*LN*D
002196 :           IO , MTJ   , 14
002197 :           13 , VJM   , D*ST*D
002198 :  WD1     : C , UTC   , DEINS
002199 :           14 , VTM   ,
002200 :           13 , VJM   , D*MI*D
002201 :              , U1A   , WD21               SPR, WENN INOUT KLEINER 1
002202 :           IO , MTJ   , 14
002203 :           13 , VJM   , D*LO*D
002204 :  WD2     : C , UTC   , D10E40
002205 :           14 , VTM   ,
002206 :           13 , VJM   , D*MI*D
002207 :              , U1A   , WD4                SPR, WENN INOUT KLEINER 1.E40
002208 :           IE , UTM   , 40                 IE:=IE + 40
002209 :           IO , MTJ   , 14
002210 :           13 , VJM   , D*LO*D
002211 :            C , UTC   , D10E40
002212 :           14 , VTM   ,
002213 :           13 , VJM   , D*DI*D             INOUT:=INOUT/1.E40
002214 :           IO , MTJ   , 14
002215 :           13 , VTM   , WD2
002216 :              , UJ    , D*ST*D
002217 : C----------------------------------------------------------------------
002218 :  WD3     :IE , UTM   , 1                  IEN=IE + 1
002219 :           IO , MTJ   , 14
002220 :           13 , VJM   , D*LO*D
002221 :            C , UTC   , D10
002222 :           14 , VTM   ,
002223 :           13 , VJM   , D*DI*D             INOUT:=INOUT/10.
002224 :           IO , MTJ   , 14
002225 :           13 , VTM   , WD5
002226 :              , UJ    , D*ST*D
002227 : C----------------------------------------------------------------------
002228 :  WD4     :IO , MTJ   , 14
002229 :           13 , VJM   , D*LO*D
002230 :  WD5     : C , UTC   , D10
002231 :           14 , VTM   ,
002232 :           13 , VJM   , D*MI*D
002233 :              , UZA   , WD3                SPR, WENN INOUT GROESSER
002234 : C            ,       ,                    ODER GLEICH  10
002235 :              , ITA   , IE
002236 :  WD6     :   , BSS   ,
002237 :              , NTR   , 3
002238 :             A, A-X   , SFAKT
002239 :             S, ATX   , E                  E:=IE
002240 :            A , XTA   , D
002241 :            A , A+X   , SFAKT
002242 :              , U1A   , WD29
002243 :              , X-A   ,
002244 :             C, UTC   , D1/10
002245 :            IK, VTM   ,
002246 :  WD63    :   , ATI   , II                 II:= -(D+SFAKT)
002247 :            C , UTC   , D1/2
002248 :           14 , VTM   ,
002249 :           13 , VJM   , D*LO*D
002250 :           II , VLM   , WD22
002251 :  WD6R    : A , UTC   , INOUT
002252 :           14 , VTM   ,
002253 :           13 , VJM   , D*AD*D             Z:=INOUT + R-ZUSCHLAG
002254 :            S , UTC   , Z
002255 :           14 , VTM   ,
002256 :           13 , VJM   , D*ST*D
002257 :             C, UTC   , D10
002258 :            14, VTM   ,
002259 :            13, VJM   , D*SU*D
002260 :              , UZA   , WD28
002261 :  WD64    : S , XTA   , Z1
002262 : C
002263 : C----------------------------------
002264 : C  HE ЯCHO, HYЖHЫ ЛИ ЗДECЬ
002265 : C  CЛEДYЮЩИE ДBE KOMAHДЫ :
002266 : C    S,E+X,Z
002267 : C     ,E+N,64+24
002268 : C---------------------------------
002269 : C
002270 :   S,ATX,Z1+1       .SECOND PART OF Z
002271 :   S,XTA,Z
002272 :   S,ATX,ZRE
002273 :   S,ATX,Z+1        .FIRST PART  OF Z
002274 :            S , XTA   , E
002275 :              , NTR   , 3
002276 :            C , A-X   , A1750
002277 :              , UZA   , WD23               SPR, WENN EXP GROESSER 999
002278 :            S , XTA   , E
002279 :            C , A+X   , A1750
002280 :            C , A-X   , A1
002281 :              , U1A   , WD23               SPR, WENN EXP KLEINER -999
002282 :  WD60    :WE , VTM   , 5
002283 :           IE , VTM   , 0                  DH EXP HAT 3 ZIFFERN
002284 :  WD61    :NVP, VTM   , 1
002285 :             M, VTM   , 0
002286 :            A , XTA   , D
002287 :              , U1A   , WD7
002288 :           WE , UTM   , -1
002289 :  WD7     : A , XTA   , SFAKT
002290 :              , NTR   , 23.B
002291 :              , U1A   , WD8
002292 :            A , WTC   , SFAKT
002293 :           NVP, UTM   ,
002294 :  WD8     : S , XTA   , VZM
002295 :              , UZA   , WD9
002296 :           WE , UTM   , 1
002297 :  WD9     :NVP, J+M   , WE
002298 :              , ITA   , WE
002299 :            A , ARX   , D
002300 :   ,ITS,NVP
002301 :   A,ARX,D
002302 :   S,STX,-4          .N OF DIGITC
002303 :            A , X-A   , W
002304 :            A , ATX   , NSP
002305 :              , U1A   , WD24               SPR, WENN  WE GROESSER W
002306 :              , X-A   ,
002307 :              , ATI   , IK
002308 :           IK , VLM   , WD25
002309 :  WD10    : S , XTA   , VZM
002310 :              , UZA   , WD11
002311 :            C , XTA   , MI
002312 :           R4 , VJM   , WSY*
002313 :  WD11    :NVP, UTM   , -1
002314 :           NVP, VZM   , WD26
002315 :  WD12    :NVP, UTM   , 1
002316 :  WD13    :R4 , VJM   , NEXTLET*
002317 :            A , XTA   , SYMB
002318 :           R4 , VJM   , WSY*.              AUSG ZIFFERN VOR DEZ-PUNKT
002319 :           NVP, UTM   , -1
002320 :           NVP, V1M   , WD13
002321 :            A , XTA   , D
002322 :              , UZA   , WD15
002323 :              , ATI   , ID
002324 :            C , XTA   , PT
002325 :           R4 , VJM   , WSY*               AUSG DEZ-PUNKT
002326 :  WD14    :R4 , VJM   , NEXTLET*
002327 :            A , XTA   , SYMB
002328 :           R4 , VJM   , WSY*.     AUSG ZIFFERN NACH DEM DEZIMAL-PUNKT
002329 :           ID , UTM   , -1
002330 :           ID , V1M   , WD14
002331 :  WD15    : S , XTA   , E
002332 :            C , AOX   , EX640
002333 :   ,NTR,22B
002334 :   ,U1A,WD27
002335 :   C,A+X,E0-1
002336 :   C,XTS,PL
002337 :   WD16:M,VTM,4
002338 :   R4,VJM,WSY*
002339 :   ,ITA,M
002340 :   S,STX,-4         .N OF DIGITS OF EXP
002341 :   M,VTM,0
002342 :   C,A*X,E0-1
002343 :   II,VTM,-4
002344 :   IE,VZM,PRA
002345 :   C,A*X,Z1/10
002346 :   II,VTM,-5
002347 :   PRA:II,UTM,1
002348 :   S,ATX,-5
002349 :   ,XTA,
002350 :   S,ATX,-1
002351 :   ID,VTM,4
002352 :   WD17:II,UTM,1
002353 :   R4,VJM,NEXTLET*
002354 :   A,XTA,SYMB
002355 :   R4,VJM,WSY*
002356 :   II,V1M,WD17
002357 :   S,UTM,-9
002358 :           S  , WTC   ,
002359 :              , UJ    ,
002360 : C----------------------------------------------------------------------
002361 :  WD20    : C , XTA   , A1
002362 :            A , ATX   , KNEXTLET
002363 :              , UJ    , WD60
002364 : C----------------------------------------------------------------------
002365 :  WD21    :IO , MTJ   , 14
002366 :           13 , VJM   , D*LO*D
002367 :  WD211   : C , UTC   , D10E*40
002368 :           14 , VTM   ,
002369 :           13 , VJM   , D*MI*D
002370 :              , UZA   , WD213              SPR, WENN INOUT KLEINER -1.E40
002371 :           IE , UTM   , -40
002372 :           IO , MTJ   , 14
002373 :           13 , VJM   , D*LO*D
002374 :            C , UTC   , D10E40
002375 :           14 , VTM   ,
002376 :           13 , VJM   , D*MU*D
002377 :           IO , MTJ   , 14
002378 :           13 , VTM   , WD211
002379 :              , UJ    , D*ST*D
002380 : C----------------------------------------------------------------------
002381 :  WD212   :IE , UTM   , -1
002382 :           IO , MTJ   , 14
002383 :           13 , VJM   , D*LO*D
002384 :            C , UTC   , D10
002385 :           14 , VTM   ,
002386 :           13 , VJM   , D*MU*D
002387 :           IO , MTJ   , 14
002388 :           13 , VTM   , WD214
002389 :              , UJ    , D*ST*D
002390 : C----------------------------------------------------------------------
002391 :  WD213   :IO , MTJ   , 14
002392 :           13 , VJM   , D*LO*D
002393 :  WD214   : C , UTC   , DEINS
002394 :           14 , VTM   ,
002395 :           13 , VJM   , D*MI*D
002396 :              , U1A   , WD212              SPR, WENN INOUT KLEINER
002397 :              , ITA   , IE
002398 :            C , AOX   , BIT16/41
002399 :            C , UTC   , D1/10
002400 :           IK , VTM   ,
002401 :              , UJ    , WD6
002402 : C----------------------------------------------------------------------
002403 :  WD22    :IK , MTJ   , 14                 BERECHNUNG RUNDUNGS-ZUSCHL
002404 :           13 , VJM   , D*MU*D
002405 :           II , VLM   , WD22
002406 :              , UJ    , WD6R
002407 : C----------------------------------------------------------------------
002408 :  WD23    :IE , VTM   , 1                  DH EXP HAT 4 ZIFFERN
002409 :           WE , VTM   , 6
002410 :              , UJ    , WD61
002411 : C----------------------------------------------------------------------
002412 :  WD24    : C , XTA   , A77777
002413 :            A , ATX   , KWSY
002414 :              , UJ    , WD10
002415 : C----------------------------------------------------------------------
002416 :  WD25    : C , XTA   , SP
002417 :           R4 , VJM   , WSY*               AUSG FUEHRENDER SPACES
002418 :           IK , VLM   , WD25
002419 :              , UJ    , WD10
002420 : C----------------------------------------------------------------------
002421 :  WD26    : A , XTA   , SFAKT
002422 :              , NTR   , 23 B
002423 :              , UZA   , WD12
002424 :              , ATI   , SF
002425 :            C , XTA   , NULL
002426 :           R4 , VJM   , WSY*               AUSG 0 VOR DEZ-PUNKT
002427 :            C , XTA   , PT
002428 :           R4 , VJM   , WSY*               AUSG DEZ-PUNKT
002429 :            A , WTC   , D
002430 :           ID , VTM   ,
002431 :  WD261   :ID , VZM   , WD15
002432 :           SF , UTM   , 1
002433 :           SF , VZM   , WD14
002434 :            C , XTA   , NULL
002435 :           ID , UTM   , -1
002436 :           R4 , VTM   , WD261
002437 :              , UJ    , WSY*               AUSG (-SFAKT-1) NULLEN.
002438 : C                                         WENN SFAKT NEGATIV
002439 : C----------------------------------------------------------------------
002440 :  WD27    : S , AVX   , E
002441 :   C,A+X,E0-1
002442 :   C,XTS,MI
002443 :              , UJ    , WD16
002444 : C----------------------------------------------------------------------
002445 :  WD29    : C , UTC   , D10
002446 :           IK , VTM   ,
002447 :              , UJ    , WD63
002448 : C----------------------------------------------------------------------
002449 :  WD28    : S , XTA   , E
002450 :            C , A+X   , A1
002451 :            S , ATX   , E
002452 :              , UJ    , WD64
002453 :  ,END,
002454 :  ISO*TEX*:,NAME,.08.12.73
002455 : C
002456 : C........   ПEPEKOДИPOBKA ISO - TEXT
002457 : C
002458 : C--------   '08.12.73' - (BEPETEOB)
002459 : C
002460 :  8,BASE,*.
002461 :  ,STI,9.KYДA
002462 :  ,ATI,10.OTKYДA
002463 :  ,XTA,.
002464 :  ,ATX,RW.
002465 :  ,UJ,PUT W.YCTAH.ЗAПИCИ
002466 :  READ S:,XTA,RW.OCTATOK ISO
002467 :  ,UIA,RES.
002468 :  10,XTA,.CЛOBO ISO
002469 :  ,UZA,END.KOHEЦ TEKCTA
002470 :  ,ATX,RW.
002471 :  10,UTM,1.
002472 :  RES:,ASN,64-8.
002473 :  ,ATX,RW.
002474 :  ,YTA,.
002475 :  ,ATX,RS.CИMBOЛ ISO
002476 :  ,ASN,64+3.ПEPEKOДИPOBKA
002477 :  ,ATI,12.OTH.AДPEC
002478 :  ,ASN,64-3.
002479 :  ,AEX,RS.
002480 :  ,ATI,14.N CИMBOЛA
002481 :  12,XTA,TABL.
002482 :  14,APX,MASK.CИMBOЛ TEXT
002483 :  11,ASN,64+48.B COOTB.ПOЗ.
002484 :  ,UIA,WRIT S.
002485 :  ,XTA,RS.ПPOБA:
002486 :  ,AEX,S6/B.ПPOБEЛ/ПYCTOЙ
002487 :  ,UIA,READ S. - ПYCTOЙ
002488 :  WRIT S:,AOX,WW.CИMBOЛ TEXT
002489 :  ,ATX,WW.HA MECTO
002490 :  11,UTM,6.
002491 :  11,VIM,READ S.
002492 :  9,ATX,.HOBOE CЛOBO
002493 :  9,UTM,1.ЗAПИCИ (TEXT)
002494 :  PUT W:,XTA,.
002495 :  ,ATX,WW.
002496 :  11,VTM,-48.
002497 :  ,UJ,READ S.
002498 :  END:11,UTM,48.
002499 :  11,VZM,RET.
002500 :  ,XTA,WW.
002501 :  9,ATX,.
002502 :  RET:,STX,.
002503 :  13,UJ,.
002504 :  S6/B:,LOG,40.
002505 :  MASK:,BSS,.
002506 :  ,TEXT,36HЮ        Ю        Ю        Ю
002507 :  ,CONT,28HЮ        Ю        Ю        Ю
002508 :  TABL:,BSS,.
002509 :  000,TEXT,40H
002510 :  050,TEXT, 8H  *+ - /
002511 :  060,TEXT, 8H01234567
002512 :  070,TEXT, 8H89
002513 :  100,TEXT,16H ABCDEFGHIJKLMNO
002514 :  120,TEXT,16HPQRSTUVWXYZ
002515 :  140,TEXT, 8HЮ БЦД ФГ
002516 :  150,TEXT, 8H ИЙ Л
002517 :  160,TEXT, 8HПЯ    Ж
002518 :  170,TEXT, 8HЬЫЗШЭЩЧ
002519 :  RW:,BSS,1.
002520 :  RS:,BSS,1.
002521 :  WW:,BSS,1.
002522 :  ,END,.
002523 :  ISOICTTR:,NAME,
002524 : C-------------------------------------
002525 :  *ПEPФOPA:,SUBP,.A БЫЛO:,LC,   ЭX,BЫ...!
002526 : C--- TИП BЫXOДHOЙ KOДИPOBKИ B CИCTEME
002527 : C    ЗAДAETCЯ ЭTOЙ EДИHOЙ TAБЛИЦEЙ-
002528 : C       BLOCK DATA'OЙ
002529 :   SPBL:,EQU,*ПEPФOPA+1
002530 :   TABL:,EQU,SPBL
002531 : C--------------------------------------
002532 :  ,STI,8.ADDR.INPUT
002533 :  ,ATI,9.ADDR.OUTPUT
002534 :  ,ITA,12
002535 :  ,ITS,11
002536 :  ,ITS,10
002537 :  10,BASE,*
002538 :  ,XTS,
002539 :  12,VTM,20
002540 :  *1:12,UTM,-1.SETTING
002541 :  12,UTC,.ZERO INTO
002542 :  9,ATX,.THE OUTPUT
002543 :  12,V1M,*1.ARRAY
002544 :  ,NTR,7
002545 :  11,VTM,80.SYMB.COUNTER
002546 :  L0:9,UTM,1
002547 :  12,VTM,-48
002548 :  L1:12,UTM,12.COUNT COLUMNS
002549 :  ,U1A,*2
002550 :  8,XTA,.NEW INPUT WORD
002551 :  8,UTM,1
002552 :  *2:,ASN,64-8.OBTAINING
002553 :  15,ATX,.THE NEW
002554 :  ,YTA,.ISO CHARACTER
002555 :  ,ATI,14
002556 :  14,UTM,-12B.
002557 :  14,VZM,NEWLINE.
002558 :  ,ASN,64+2
002559 :  ,ATI,14
002560 :  ,YTA,
002561 :  ,ASN,64+3
002562 :  ,E+N,128-12.64-4*(3-NCOL)
002563 :  ,UTC,TABL.
002564 :  14,XTS,.
002565 :  15,ASX,-1
002566 :  15,ASX,-1
002567 :  15,ASX,
002568 :  ,ASN,64+36
002569 :  WRC:12,ASN,64.WRITTING THE
002570 :  9,AOX,-1.COLUMN TO THE
002571 :  9,STX,-1.OUTPUT ARRAY
002572 :  11,UTM,-1
002573 :  12,V1M,L1.NOT END OF WORD
002574 :  11,V1M,L0
002575 :  RET:,NTR,6
002576 :  15,XTA,
002577 :  ,STI,10
002578 :  ,STI,11
002579 :  ,STI,12
002580 :  13,UJ,
002581 :  NEWLINE:11,UTM,-80.
002582 :  15,XTA,
002583 :  11,V1M,RET
002584 :  ,UTC,SPBL.
002585 :  ,XTA,.
002586 :  9,ATX,17
002587 :  ,UJ,RET.
002588 :  ,END,
002589 :       ISOTCOSY    :,NAME,
002590 :      *00000B:           ,ATI ,14
002591 :                         ,ITA ,7
002592 :                         ,ITS ,8
002593 :                   8     ,VTM, 00123B
002594 :      *00002B:     7     ,VTM, 77774B
002595 :                         ,NTR ,3
002596 :      *00003B:     7     ,ITS ,13
002597 :                   7     ,VLM,      *00003B
002598 :                   15    ,ATX ,
002599 :                   15    ,WTC, 77770B
002600 :                   13    ,VTM, 00000B
002601 :                   7     ,VTM,      *00000B
002602 :                   9     ,VTM, 00000B
002603 :                   11    ,VTM, 77773B
002604 :      *00007B:     13    ,UTM, 00001B
002605 :                   10    ,VTM, 00060B
002606 :                   13    ,XTA ,77777B
002607 :                   7     ,AEX ,00066B
002608 :                         ,UZA,      *00054B
002609 :                         ,UTC, 00000B
002610 :      *00012B:     8     ,VZM,      *00035B
002611 :                   10    ,VZM,      *00007B
002612 :                   10    ,UTM, 77770B
002613 :                   13    ,XTA ,77777B
002614 :                   10    ,ASN ,64
002615 :                   7     ,AAX ,00061B
002616 :                         ,UZA,      *00012B
002617 :                   7     ,ATX ,00070B
002618 :                   7     ,AEX ,00062B
002619 :                         ,UZA,      *00030B
002620 :                   9     ,V1M,      *00032B
002621 :                         ,UTC, 00000B
002622 :      *00020B:     7     ,AEX ,00063B
002623 :                         ,UZA,      *00035B
002624 :                   7     ,XTA ,00070B
002625 :                   8     ,UTM, 77777B
002626 :                   12    ,VTM,      *00012B
002627 :                         ,UTC, 00000B
002628 :      *00023B:     7     ,AOX ,00067B
002629 :                   11    ,VZM,      *00026B
002630 :                         ,ASN ,56
002631 :                   7     ,ATX ,00067B
002632 :                   11    ,UTM, 00001B
002633 :                   12    ,UJ,  00000B
002634 :      *00026B:     14    ,ATX ,
002635 :                   11    ,VTM, 77773B
002636 :                   14    ,UTM, 00001B
002637 :                   12    ,UJ,  00000B
002638 :      *00030B:     8     ,UTM, 77777B
002639 :                   9     ,VLM,      *00012B
002640 :                   9     ,VTM, 00201B
002641 :                         ,UJ,       *00012B
002642 :      *00032B:           ,ITS ,9
002643 :                   12    ,VJM,      *00023B
002644 :                   15    ,XTA ,
002645 :                   9     ,VTM, 00000B
002646 :                         ,UJ,       *00020B
002647 :                         ,UTC, 00000B
002648 :      *00035B:           ,NTR ,2
002649 :                   9     ,VZM,      *00037B
002650 :                         ,ITA ,9
002651 :                   12    ,VJM,      *00023B
002652 :      *00037B:     11    ,VZM,      *00043B
002653 :                   7     ,XTA ,00064B
002654 :      *00040B:     12    ,VJM,      *00023B
002655 :                   11    ,VZM,      *00043B
002656 :                   7     ,XTA ,00062B
002657 :                         ,UJ,       *00040B
002658 :                         ,UTC, 00000B
002659 :      *00043B:     7     ,XTA ,00064B
002660 :                   7     ,AOX ,00067B
002661 :                   14    ,STX ,
002662 :                         ,STI ,13
002663 :                         ,STI ,12
002664 :                         ,STI ,11
002665 :                         ,STI ,10
002666 :                         ,STI ,9
002667 :                         ,STI ,8
002668 :                         ,STI ,7
002669 :                   15    ,XTA ,
002670 :                   13    ,UJ,  00000B
002671 :   ISOTCOS1:,ENTRY,
002672 :                         ,ATI ,14
002673 :                         ,ITA ,7
002674 :                         ,ITS ,8
002675 :                   8     ,VTM, 00225B
002676 :                         ,UJ,       *00002B
002677 :                         ,UTC, 00000B
002678 :      *00054B:           ,ITA ,8
002679 :                   7     ,A-X ,00065B
002680 :                         ,U1A,      *00012B
002681 :                   8     ,UTM, 77772B
002682 :                   9     ,VZM,      *00060B
002683 :                   9     ,UTM, 00006B
002684 :                         ,UJ,       *00007B
002685 :                         ,UTC, 00000B
002686 :      *00060B:     9     ,VTM, 00206B
002687 :                         ,UJ,       *00007B
002688 : CONST
002689 :      *00061B:,OCT,0000  0000  0000  0377
002690 :      *00062B:,OCT,0000  0000  0000  0040
002691 :      *00063B:,OCT,0000  0000  0000  0052
002692 :      *00064B:,OCT,0000  0000  0000  0012
002693 :      *00065B:,OCT,0000  0000  0000  0006
002694 :      *00066B:,OCT,1002  0040  1002  0040
002695 :      *00067B:,OCT,
002696 :      *00070B:,OCT,
002697 :                   ,END,
002698 :       LDLIST      :,NAME,
002699 :                         ,XTS ,00044B
002700 :                         ,UTC,      *00006B
002701 :                         ,AAX ,
002702 :                         ,STX ,00044B
002703 :                   13    ,UJ,  00000B
002704 :   NOLDLI:,ENTRY,
002705 :                         ,XTS ,00044B
002706 :                         ,UTC,      *00007B
002707 :                         ,AOX ,
002708 :                         ,STX ,00044B
002709 :                   13    ,UJ,  00000B
002710 :      *00006B:,OCT,3777  7777  7777  7777
002711 :      *00007B:,OCT,4000  0000  0000  0000
002712 :                   ,END,
002713 :  LENGTH:,NAME,         LAENGE EINES STRINGS, AC, I13
002714 :  A/MINUS:,LC,          1
002715 :  A/I0:,LC,             1
002716 :  8,VTM,                C
002717 :  8,ATX,                STRING1-C
002718 :  ,ATI,                 14
002719 : L8,AAX,                =7760 0000 0000 0000-C.
002720 :  :,UZA,                *+1
002721 :  14,VTM,               STRING1
002722 :  12,VTM,               -7
002723 :  ZEIZYK:14,XTA,
002724 :  ,ASN,                 64-40
002725 :  12,UTM,               6
002726 :  14,UTM,               1
002727 :  ,U1A,                 ZEIZYK
002728 :  14,XTA,               -1
002729 :  ,ASN,                 64+8
002730 :  POSZYK:,ASN,          64-8
002731 :  12,UTM,               1
002732 :  ,U1A,                 POSZYK
002733 :  ,ITA,                 12
002734 :  ,AEX,                 A/I0
002735 :  13,UJ,
002736 :  C:,BSS,
002737 :  STRING1:,BSS,         2
002738 :  STRING2:,BSS,         2
002739 :  SVERGL:8,VTM,         C                  UPI14.I12: 0=,1 GT,2 LT
002740 :  8,ATX,                STRING1-C
002741 :  ,ATI,                 10
002742 : L8,AAX,                =7760 0000 0000 0000-C
002743 :  :,UZA,                *+1
002744 :  10,VTM,               STRING1
002745 :  15,XTA,
002746 :  ,ATI,                 11
002747 :  8,ATX,                STRING2-C
002748 : L8,AAX,                =7760 0000 0000 0000-C. YEST KURZSTRING
002749 :  :,UZA,                *+1
002750 :  11,VTM,               STRING2
002751 :  12,VTM,               0
002752 :  VERGLZYK:10,XTA,
002753 :  11,AEX,
002754 :  ,U1A,                 UNGL
002755 :  10,XTA,
002756 :  ,ASN,                 64-40
002757 :  14,UZA,                                  RK GLEICH
002758 :  10,UTM,               1
002759 :  11,UTM,               1
002760 :  ,UJ,                  VERGLZYK
002761 :  UNGL:12,VTM,          1
002762 :  ,ARX,
002763 :  ,UZA,                 UNGL48
002764 :  11,XTA,
002765 : L,UTC,                 =7777 7777 7777 7777.
002766 :  ,AEX,
002767 :  10,ARX,
002768 :  UNGL*:14,UZA,                            RKGROESSER
002769 :  12,VTM,               2
002770 :  14,UJ,                                   RK KLEINER
002771 :  UNGL48:11,XTA,                           BIT48 VERSCHIEDEN
002772 :  ,ARX,
002773 :  ,UJ,                  UNGL*
002774 :  A/SEQS:,ENTRY,
002775 :  14,VJM,               SVERGL
002776 :  SEQS:,XTA,
002777 :  13,UTC,
002778 :  12,V1M,
002779 :  ,XTA,                 A/MINUS
002780 :  13,UJ,
002781 :  A/SNES:,ENTRY,
002782 :  14,VJM,               SVERGL
002783 :  SNES:,XTA,
002784 :  13,UTC,
002785 :  12,VZM,
002786 :  ,XTA,                 A/MINUS
002787 :  13,UJ,
002788 :  A/SGTS:,ENTRY,
002789 :  14,VJM,               SVERGL
002790 :  12,UTM,               -1
002791 :  ,UJ,                  SEQS
002792 :  A/SLTS:,ENTRY,
002793 :  14,VJM,               SVERGL
002794 :  12,UTM,               -2
002795 :  ,UJ,                  SEQS
002796 :  A/SGES:,ENTRY,
002797 :  14,VJM,               SVERGL
002798 :  12,UTM,               -2
002799 :  ,UJ,                  SNES
002800 :  A/SLES:,ENTRY,
002801 :  14,VJM,               SVERGL
002802 :  12,UTM,               -1
002803 :  ,UJ,                  SNES
002804 :  ,END,
002805 :  LIBDELET:,NAME,
002806 :  EXCLUDE:,SUBP,
002807 :  ARD:,EQU,39
002808 :  AWRT:,EQU,40
002809 :  PRINT8:,SUBP,
002810 :  CATLG:PC,BLOCK,CATALOG(8192)
002811 : C
002812 : C=====  CAMAЯ ПEPBAЯ PEДAKЦИЯ B CИCTEMY
002813 : C               M.YMAPOBA
002814 : C
002815 :   NOLISFL*:,LC,1
002816 :   MONCARD*:,LC,14
002817 :   MONCARD/:,LC,14
002818 : C
002819 : C...   B PEЖИME "*NO LIST" ПEЧATЬ CПИCKA
002820 : C      HE ПPOИЗBOДИTCЯ.
002821 : C...   BBEДEHA PAЗHOBИДHOCTЬ KAPTЫ
002822 : C      *EXCLUDE C ПAPAMETPAMИ :
002823 : C      *EXCLUDE:NAME1,NAME2, ... ,NAMEN
002824 : C      ECЛИ CПИCOK HE YMEЩAETCЯ HA OДHOЙ
002825 : C      KAPTE, TO CЛEДYET ЗAKOHЧИTЬ TEKY-
002826 : C      ЩYЮ KAPTY ЗAПЯTOЙ, И ПPOДOЛЖATЬ
002827 : C      CПИCOK CO 2-Й ПOЗИЦИИ CЛEД.KAPTЫ.
002828 : C
002829 :   ,UTC,LOC
002830 :   ,ATX,
002831 :  C       : 14,VTM, -13                    .
002832 :          : 14,ITS, 14                     .
002833 :            14,VLM, *                      .
002834 :             7,VTM, C                      .
002835 :   7,XTA,LOC-C
002836 :   7,AEX,ПAPOЛЬ-C
002837 :   3,VTM,1
002838 :   ,UZA,NOPARM.OT COLLECT-A
002839 :   9,VTM,
002840 :  10,VTM,
002841 :   ,CALL,PARAMT*
002842 :   11,UTM,-72B.":"
002843 :   11,MTJ,3
002844 :   NOPARM:,BSS,
002845 :   4,VTM,1
002846 :             2,VTM, EXCLUDE
002847 :              ,XTA, ARD
002848 :            7,AAX, NLIS-C
002849 :  13,VTM,CATALOG
002850 :  ,ITS,13
002851 :              ,ASN, 64-20
002852 :            15,AOX,
002853 :  NEXT R:,BSS,
002854 :             7,ATX, WL-C
002855 :              7,*70, WL-C
002856 : C.....   ИЩEM KOHEЦ KATAЛOГA TEMPO
002857 :  14,VTM,-511
002858 :  SRCEND 1:13,XTA,1
002859 :  7,AEX,C7-C
002860 :  ,UZA,CYCL
002861 :  13,UTM,2
002862 :  14,VLM,SRCEND 1
002863 :             7,XTA, WL-C
002864 :              7,ARX, CEX-C
002865 :  ,UJ,NEXT R
002866 :  CYCL    :  2,XTA,                        .
002867 :   3,VIM,USUAL
002868 :   GIVPAR:10,VTM,3
002869 :   9,VTM,
002870 :   ,CALL,PARAMT*
002871 :   11,UTM,-54B.","
002872 :   12,VIM,USUAL.HE ПYCTO
002873 :   4,VIM,EXIT.KOHEЦ CПИCKA
002874 :   ,CALL,MONREAD*.HOBAЯ KAPTA
002875 :   10,VTM,9
002876 :   14,VTM,-13
002877 :   :14,XTA,MONCARD*+13
002878 :    14,ATX,MONCARD/+13
002879 :    14,VLM,*-1
002880 :   6,VTM,MONCARD*
002881 :   9,VTM,
002882 :   ,CALL,PARAMT*.YCT.HA HAЧ.
002883 :   ,ITS,6
002884 :   6,UTM,13
002885 :   ,ITS,6
002886 :   ,XTS,=1
002887 :   13,VTM,GIVPAR
002888 :   ,UJ,PRINT8
002889 :   USUAL:7,ATX,LOC-C
002890 :   11,MTJ,4."БЫЛA ЗПT"
002891 :              ,UZA, EXIT                   .
002892 :              1,VTM, CATALOG+2
002893 :  CHKCYCL :  1,XTA, 1
002894 :             7,AEX, C7-C                   .
002895 :              ,UZA, CHKEXIT                .
002896 :   7,XTA,LOC-C
002897 :             1,AEX,                        .
002898 :              ,UZA, ENTRYCH1
002899 :             1,UTM, 2                      .
002900 :              ,UJ,  CHKCYCL                .
002901 :   CHKEXIT:,XTA,NOLISFL*
002902 :   ,U1A,NO PRIN
002903 :   7,XTS,LOC-C
002904 :            14,VTM, 10
002905 :              ,ITS, 14
002906 :   ,CALL,PT6/G
002907 :            14,VTM, T
002908 :              ,ITS, 14
002909 :            14,VTM, T1
002910 :              ,ITS, 14
002911 :              ,XTS,
002912 :            13,VJM, PRINT8
002913 :   NO PRIN:,BSS,
002914 :             2,UTM, 1
002915 :              ,UJ,  CYCL                   .
002916 :   ENTRYCH1:,XTA,NOLISFL*
002917 :   ,U1A,ENTRYCH
002918 :   3,VZM,ENTRYCH
002919 :   7,XTS,LOC-C
002920 :            14,VTM, 20
002921 :              ,ITS, 14
002922 :   ,CALL,PT6/G
002923 :  ENTRYCH :  1,XTA, 1                      .
002924 :             7,AAX, LIBMASK-C              .
002925 :              ,UZA, REWRCAT                .
002926 :             1,XTA, 3                      .
002927 :             7,AAX, LIBMASK-C              .
002928 :              ,U1A, REWRCAT                .
002929 :             1,XTA, 1                      .
002930 :             7,AAX, MA3LC-C                .
002931 :             1,AOX, 3                      .
002932 :             1,ATX, 3                      .
002933 :  REWRCAT :  1,XTA, 2                      .
002934 :             1,ATX,                        .
002935 :             1,XTA, 3                      .
002936 :             1,ATX, 1                      .
002937 :             7,AEX, C7-C                   .
002938 :              ,UZA, CHKEXIT1               .
002939 :             1,UTM, 2                      .
002940 :              ,UJ,  REWRCAT                .
002941 :  CHKEXIT1:  1,ATX, 3                      .
002942 :   3,VZM,CYCL
002943 :             2,UTM, 1
002944 :              ,UJ,  CYCL
002945 :  EXIT    :  1,VTM, CATALOG+2
002946 :  C1      :  1,XTA,
002947 :             7,AEX, EXCL-C
002948 :              ,UZA, EN1
002949 :             1,XTA, 1
002950 :             7,AEX, C7-C
002951 :              ,UZA, EXIT2
002952 :             1,UTM, 2
002953 :              ,UJ,  C1
002954 :  EN1     :  1,XTA, 1
002955 :             7,AAX, LIBMASK-C
002956 :              ,UZA, REWR
002957 :             1,XTA, 3
002958 :             7,AAX, LIBMASK-C
002959 :              ,U1A, REWR
002960 :             1,XTA, 1
002961 :             7,AAX, MA3LC-C
002962 :             1,AOX, 3
002963 :             1,ATX, 3
002964 :  REWR    :  1,XTA, 2
002965 :             1,ATX,
002966 :             1,XTA, 3
002967 :             1,ATX, 1
002968 :             7,AEX, C7-C
002969 :              ,UZA, CH1
002970 :             1,UTM, 2
002971 :              ,UJ,  REWR
002972 :  CH1     :  1,ATX, 3
002973 :  EXIT  2  : ,XTA, AWRT
002974 :              7,AAX, NLIS-C
002975 :  13,VTM,CATALOG
002976 :  ,ITS,13
002977 :              ,ASN, 64-20
002978 :              15,AOX,
002979 :  NEXT W:,BSS,
002980 :              7,ATX, WL-C
002981 :             7,*70,WL-C
002982 :  14,VTM,-511
002983 :  SRCEND 2:13,XTA,1
002984 :  7,AEX,C7-C
002985 :  ,UZA,EXIT 1
002986 :  13,UTM,2
002987 :  14,VLM,SRCEND 2
002988 :              7,XTA, WL-C
002989 :             7,ARX, CEX-C
002990 :  ,UJ,NEXT W
002991 :  EXIT1   :   ,STX,                        .
002992 :            14,VTM, 13                     .
002993 :  A       : 14,STI,                        .
002994 :            14,UTM, -1                     .
002995 :            14,V1M, A                      .
002996 :            13,UJ,                         .
002997 :   ПAPOЛЬ:,ISO,6HBOЛKOB
002998 :    'CONST'
002999 :  LOC     :   ,OCT,                        .
003000 :  C7      :   ,OCT, 7777 7777 7777 7777    .
003001 :  LIBMASK :   ,OCT, 5777 7700 0000 0000    .
003002 :  MA3LC   :   ,OCT, 7777 7777 7770 0000    .
003003 :  T       :   ,8TEXT, ''THIS N''
003004 :              ,8TEXT, ''AME IS''
003005 :              ,8TEXT, '' ABSEN''
003006 :               ,8TEXT, ''T IN C''
003007 :  T1      :   ,8TEXT, ''ATALOG''
003008 :  B13: ,OCT,
003009 :  EXCL    :   ,6TEXT, ''EXCLUDE''
003010 :  WL      :   ,OCT,
003011 :  CEX      :   ,OCT, 0000 0100 0000 0001
003012 :  NLIS    :    ,OCT, 7777 4077 7777 7777
003013 :          ,END,
003014 :  LIBDRUM*:,NAME,
003015 : C*******************************
003016 : C****PEAЛИЗAЦИЯ ФYHKЦИЙ*********
003017 : C**** COMMON LIBRARY***********
003018 : C****PERSONAL LIBRARY**********
003019 : C****MOCKBA  ИAЭ ИM. KYPЧATOBA**
003020 : C****ГOЛYБEB A.C. BECHA 1973.***
003021 : C*******************************
003022 :  NEWCAT:,PC,2048
003023 :  OLDCAT:,PC,2048
003024 :  NCL:,P/P,(NEWCAT)16
003025 :  OCL:,P/P,(OLDCAT)16
003026 :  TABL:,PC,2048
003027 :  *MONCRD*:,LC,12
003028 :  MONCRD:,EQU,*MONCRD*
003029 :  *REGIM//:,LC,1
003030 :  R:,EQU,5
003031 :  B:,BSS,
003032 :  1,BAS,B
003033 :  1,VTM,B
003034 :  ,ITA,13
003035 :  ,XTS,
003036 :  ,ATX,INF
003037 :  ,ATX,ERROR
003038 :  14,VTM,BADCC
003039 :  ,ITA,14
003040 :  ,ATX,D
003041 :  ,XTA,TOZCOM
003042 :  ,ATX,TOZERO
003043 :  ,XTA,=7
003044 :  ,ATX,SHIFT
003045 :  ,XTA,=:001051
003046 :  ,ATX,BOUND
003047 :  ,XTA,*REGIM//
003048 :  ,AOX,=6 00000
003049 :  ,AEX,=2 00000
003050 :  ,ATX,*REGIM//
003051 :  ,UJ,*/*
003052 :  LIBDRUM:,ENTRY,
003053 :  1,VTM,B
003054 :  ,ITA,13
003055 :  ,XTS,
003056 :  ,ATX,ERROR
003057 :  14,VTM,BADPC
003058 :  ,ITA,14
003059 :  ,ATX,D
003060 :  ,XTA,TOZPER
003061 :  ,ATX,TOZERO
003062 :  ,XTA,
003063 :  ,ATX,SHIFT
003064 :  ,XTA,=:001042
003065 :  ,ATX,BOUND
003066 :  */*:,BSS,
003067 : C---KOHEЦ HAЧAЛЬHЫX YCTAHOBOK----
003068 :  10,VTM,
003069 :  ,CALL,PARAMT*
003070 :  ,XTA,=670000
003071 :  ,ATX,PLACE
003072 :  10,VTM,4
003073 :  9,VTM,PLACE
003074 :  ,CALL,PARAMT*.MECTO HA ЛEHTE
003075 :  14,V1M,RET
003076 :  10,VTM,2
003077 :  ,XTA,=6HCONT
003078 :  ,CALL,PARAMT*.PEЖИM ДOПИCИ
003079 :  ,ATX,FLAG
003080 :  14,VIM,RET.OШИБKИ B ПAPAMETPAX
003081 :  2,VTM,OLDCAT+2
003082 :  4,VTM,NEWCAT-2
003083 :  ,XTA,SHIFT
003084 :  ,UZA,N*CO
003085 :  ,XTA,RW
003086 :  ,AOX,PLACE
003087 :  ,ARX,=1000 000 0001
003088 :  ,ATX,WRW
003089 :  ,TAPE,WRW
003090 :  4,MTJ,13
003091 :  ,DRUM,SAV.CПACEHИE OVERLAY'HOГO
003092 :  COVCAT:13,UTM,2
003093 :  13,XTA,3401B
003094 :  ,AOX,=:7
003095 :  ,AEX,=:5
003096 :  13,ATX,3401B
003097 :  13,XTA,3400B
003098 :  ,U1A,COVCAT
003099 :  ,DRUM,SAV1.ЗAГPYЗЧИKA И KATAЛOГA
003100 :  4,XTA,3402B
003101 :  ,AEX,MNT
003102 :  ,UIA,N*CO
003103 :  4,XTA,3403B
003104 :  ,ATX,INF.OVERLAY'HAЯ ИHФOPMAЦИЯ
003105 :  N*CO:,BSS,
003106 :  ,XTA,RW
003107 :  ,AOX,PLACE
003108 :  ,ARX,SHIFT
003109 :  ,ATX,WRW
003110 :  ,TAPE,WRW
003111 :  ,XTA,WRW
003112 :  ,ATX,WC
003113 :  ,ARX,=1000 000 000 1
003114 :  ,ATX,WRW
003115 :  ,TAPE,WRW.ЧTEHИE KATAЛOГA
003116 :  ,XTA,D
003117 :  ,ATX,DGN
003118 : C---YKAЗATEЛИ ДЛЯ БЛOKA KOHTPOЛЯ--
003119 :  14,VTM,NEWCAT+1
003120 :   13,VTM,TABL
003121 :  ***:,BSS,
003122 :  14,UTM,2
003123 :  14,XTA,
003124 :  ,AEX,FULWORD
003125 :  ,UZA,CON*
003126 :  14,XTA,
003127 :  ,AAX,=:2
003128 :  ,UZA,***
003129 :  14,XTA,
003130 :  ,AAX,=00 3777 77777 00000
003131 :  13,ATX,
003132 :  14,XTA,
003133 :  ,AUX,=6 00000 00000 01740
003134 :  ,AAX,=77777
003135 :  13,AOX,
003136 :  13,ATX,
003137 :  14,XTA,-1
003138 :  13,ATX,1
003139 :  13,UTM,2
003140 :  ,UJ,***
003141 :  CON*:,BSS,
003142 :  13,ATX,
003143 : C---ПPOBEPKA HOBOГO KATAЛOГA---
003144 :  4,XTA,2
003145 :  ,AAX,=2000000
003146 :  ,UZA,KAPUT
003147 :  4,XTA,2
003148 :  ,AAX,=700000
003149 :  ,UZA,KAPUT
003150 :  4,XTA,2
003151 :  ,AAX,=7776 0000 7400 0037
003152 :  ,UIA,KAPUT
003153 :  4,XTA,3
003154 :  ,UIA,KAPUT
003155 :  ,XTA,FLAG
003156 :  ,UZA,READCAT.PEЖИM ДOЗAПИCИ
003157 : C---KATAЛOГ PERMANENT LIBRARY----
003158 :  ,DRUM,PCAT
003159 :  ,XTA,PCAT
003160 :  ,ARX,=1 000 000 000 1
003161 :  ,ATX,HEAD
003162 :  ,DRUM,HEAD
003163 :  ,XTA,STCAT
003164 :  2,ATX,-2
003165 :  ,XTA,SHIFT
003166 :  ,UIA,NOREAD
003167 :  ,XTA,*REGIM//
003168 :  ,AOX,=600000.БЫЛA *PERSONAL
003169 :  ,AEX,=600000.B PEЖИME OБHOBЛEHИЯ
003170 :  ,ATX,*REGIM//
003171 :  ,UJ,NOREAD
003172 : C---KATAЛOГ TEMPORARY LIBRARY----
003173 :  READCAT:,BSS,
003174 :  ,DRUM,OCRW
003175 :  ,XTA,OCRW
003176 :  ,ARX,=1000 000 000 1
003177 :  ,ATX,OCRWC
003178 :  ,DRUM,OCRWC
003179 :  14,VTM,BADL
003180 :  ,ITA,14
003181 :  ,ATX,DGN
003182 :  2,XTA,-2
003183 :  ,AAX,=2000000
003184 :  ,UZA,KAPUT
003185 :  2,XTA,-2
003186 :  ,AAX,=700000
003187 :  ,UZA,KAPUT
003188 :  2,XTA,-2
003189 :  ,AAX,=7776 0000 7400 0037
003190 :  ,UIA,KAPUT
003191 :  2,XTA,-1
003192 :  ,UIA,KAPUT
003193 :  ,XTA,54B.CBOБ MECTO
003194 :  2,ATX,-2.HAДEЖHEE
003195 :  NOREAD:,BSS,
003196 : C--KOHCTAHTA ПPИBEДEHИЯ K HYЛЮ--
003197 : C-------YKAЗATEЛEЙ--------------
003198 :  2,XTA,-2
003199 :  ,ARX,=1740
003200 :  ,APX,=7776000
003201 :  ,ATX,WC+1
003202 :  ,AUX,=77 0037
003203 :  ,AOX,LWW
003204 :  ,ATX,LWWC
003205 :  ,XTA,WC+1
003206 :  ,ASN,64+7
003207 :  ,ARX,TOZERO
003208 :  ,ATX,TOZERO
003209 : C--KOHEЦ HOBOГO KATAЛOГA-----
003210 : C---KOPPEKTИPOBKA YKAЗATEЛEЙ---------
003211 :  ,XTA,D
003212 :  ,ATX,DGN
003213 :  COUNT:,BSS,
003214 :  4,XTA,2
003215 :  4,UTM,2
003216 :  4,MTJ,14
003217 :  14,UTM,-2048-NEWCAT
003218 :  14,VZM,KAPUT
003219 :  ,UZA,*CON
003220 :  4,XTA,1
003221 :  ,AAX,=:2
003222 :  ,UZA,COUNT
003223 :  4,XTA,1
003224 :  ,ARX,TOZERO
003225 :  4,ATX,1
003226 :  ,UJ,COUNT
003227 :  *CON:,BSS,
003228 :  4,XTA,1
003229 :  ,AEX,FULWORD
003230 :  ,UIA,KAPUT
003231 :  4,MTJ,3
003232 :  4,MTJ,14
003233 : C----ПEЧATЬ KOMMEHTAPИЯ----------
003234 :  14,UTM,2
003235 :  ,ITS,14
003236 :  14,UTM,12
003237 :  ,ITS,14
003238 :  ,XTS,=1
003239 :  ,CALL,PRINT8
003240 :  ,ITA,4
003241 :  ,AEX,=77777
003242 :  ,ATI,4
003243 :  4,UTM,-1
003244 : C---OБ'EДИHEHИE KATAЛOГOB---
003245 :  BJOIN:,BSS,
003246 :  2,XTA,
003247 :  ,UZA,END
003248 :  2,XTA,1
003249 :  ,AAX,=:2
003250 :  ,UZA,///
003251 :  2,XTA,1
003252 :  ,AAX,=777777 77777 00000
003253 :  ,ATX,HEAD
003254 :  ///:,BSS,
003255 :  R,VJM,ISTHEAR
003256 :  2,UTM,2
003257 :  3,MTJ,14
003258 :  14,UTM,-2048-NEWCAT
003259 :  14,VZM,MAT
003260 :  ,UJ,BJOIN
003261 : C---HET ЛИ YЖE TAKOГO ИMEHИ-----
003262 :  ISTHEAR:,BSS,
003263 :  14,VTM,NEWCAT+2
003264 :  BSEEK:,BSS,
003265 :  14,XTA,
003266 :  2,AEX,
003267 :  14,UTM,2
003268 :  14,MTJ,12
003269 :  4,J+M,12
003270 :  12,VZM,WRITE
003271 :  ,UIA,BSEEK
003272 :  R,UJ,
003273 : C---ЗAПИCЬ ИMEHИ B KATAЛOГ----
003274 :  WRITE:,BSS,
003275 :  2,XTA,
003276 :  3,ATX,
003277 :  2,XTA,1
003278 :  ,AAX,=77777
003279 :  ,AOX,HEAD
003280 :  3,ATX,1
003281 :  3,UTM,2
003282 :  ,XTA,
003283 :  ,ATX,HEAD
003284 :  R,UJ,
003285 : C---ДЛИHA HOBOЙ ЧACTИ БИБЛИOTEKИ--
003286 :  END:,BSS,
003287 :  2,VTM,NEWCAT
003288 :  ,XTA,
003289 :  3,ATX,
003290 :  ,XTA,FULWORD
003291 :  3,ATX,1
003292 :  2,XTA,
003293 :  ,ARX,=1740
003294 :  ,APX,=7776000
003295 :  ,ASN,64-11
003296 :  ,YTA,
003297 :  ,AEX,=77777
003298 :  ,ARX,SHIFT
003299 :  ,ATI,12
003300 :  12,UTM,1044B
003301 : C--YKAЗATEЛЬ CBOБOДHOГO MECTA--
003302 :  ,XTA,WC+1
003303 :  ,ARX,=:5674
003304 :  ,ATX,TOZERO.ЧИCЛO HOBЫX TPAKTOB
003305 :  ,AUX,=7776000
003306 :  ,XTS,SHIFT
003307 :  ,ASN,64-10
003308 :  ,AEX,FULWORD
003309 :  15,ARX,
003310 :  2,ARX,
003311 :  2,ATX,
003312 :  ,ATX,OCRWC
003313 :  ,ATX,54B
003314 : C---KATAЛOГ TEMPORARY-------
003315 :  ,DRUM,WW
003316 :  ,XTA,WW
003317 :  ,ARX,=1000 000 0001
003318 :  ,ATX,WWC
003319 :  ,DRUM,WWC
003320 :  12,MTJ,14
003321 :  14,UTM,-1
003322 :  14,VZM,ZEROLENG.ПYCTAЯ БИБЛИOTEKA
003323 : C++++ПEPEПИCЬ БИБЛИOTEKИ++++++++
003324 :  RWL:,BSS,
003325 :  ,XTA,WRW
003326 :  ,ARX,=1
003327 :  ,ATX,WRW
003328 :  ,TAPE,WRW
003329 :  ,DRUM,LWWC
003330 : C---ПPOBEPKA HA ПOПAДAHИE B ЗOHY--
003331 :  14,VTM,TABL-2
003332 :  CHECK*:,BSS,
003333 :  14,UTM,2
003334 :  14,XTA,
003335 :  ,UZA,CON/
003336 :  ,AAX,=:003777
003337 :  ,AEX,BOUND
003338 :  ,UIA,CHECK*
003339 : C---ПPOBEPKA CTPYKTYPЫ------
003340 :  14,WTC,
003341 :  ,UTC,NEWCAT+1026
003342 :  ,XTA,
003343 :  ,ATI,13
003344 :  ,ASN,64+15
003345 :  ,ATI,2
003346 :  2,J+M,13
003347 :  ,ASN,64+15
003348 :  ,ITS,13
003349 :  15,ARX,
003350 :  ,ASN,64-15
003351 :  ,ATX,WC
003352 :  14,XTA,
003353 :  ,AAX,=77777 00000
003354 :  ,AEX,WC
003355 :  ,UIA,KAPUT1
003356 :  ,UJ,CHECK*
003357 : C-------------------------
003358 :  CON/:,BSS,
003359 :  ,XTA,BOUND
003360 :  ,ARX,=:000001
003361 :  ,ATX,BOUND
003362 :  ,XTA,LWWC
003363 :  ,ARX,=7741
003364 :  ,AAX,CONST
003365 :  ,ATX,LWWC
003366 :  12,VLM,RWL
003367 :  ZEROLENG:,BSS,
003368 : C++++++++++++++++++++++++++
003369 :  ,XTA,SHIFT
003370 :  ,UZA,RET
003371 :  14,VTM,BADOV
003372 :  ,ITA,14
003373 :  ,ATX,DGN
003374 :  ,XTA,INF
003375 :  ,UZA,RET
003376 : C---ПOДГOTOBKA ЧTEHИЯ OVERLAY---
003377 :  ,APX,=3777 00000
003378 :  ,ARX,=:21
003379 :  ,ATX,ENDOV
003380 :  ,UTC,NEWCAT
003381 :  ,XTA,
003382 :  ,AAX,=1740
003383 :  ,ATI,4
003384 :  7,VTM,2000B
003385 :  4,VZM,BROV
003386 :  7,VTM,
003387 :  6,VTM,
003388 :  ,ASN,64+5
003389 :  ,ATI,5
003390 : C   ПOДBOД K HAЧAЛY
003391 :  TOB:11,VJM,RW40
003392 :  5,UTM,-1
003393 :  5,VIM,TOB
003394 :  BROV:,BSS,
003395 :  ,XTA,TOZERO
003396 :  ,ASN,64+2
003397 :  ,AEX,FULWORD
003398 :  ,ARX,=:00034
003399 :  ,ARX,INF
003400 :  ,UZA,KAPUT
003401 :  ,ASN,64+25
003402 :  ,ARX,OCRWC
003403 :  ,ASN,64-27
003404 :  ,ATX,WC
003405 :  ,AUX,=77 0037
003406 :  ,AOX,LWW1
003407 :  ,ATX,LWWC
003408 :  ,XTA,WC
003409 :  ,ASN,64+27
003410 :  ,AAX,=1740
003411 :  ,ATI,6
003412 :  ,XTA,WC
003413 :  ,AEX,FULWORD
003414 :  ,ARX,ENDOV
003415 :  ,ASN,64+32
003416 :  ,ATI,5
003417 :  ROV:,BSS,
003418 :  11,VJM,RW40
003419 :  5,UTM,-1
003420 :  5,V1M,ROV
003421 : C---ДOCБPOC БYФEPA-------
003422 :  6,VTM,2000B
003423 :      13,VJM,WRITW
003424 :  ,XTA,=200000.БЫЛ OVERLAY
003425 :  ,AOX,*REGIM//
003426 :  ,ATX,*REGIM//
003427 :  RET:,BSS,
003428 :  15,WTC,
003429 :  ,UJ,
003430 :  MAT:,BSS,
003431 :  14,VTM,PY
003432 :  ,ITS,14
003433 :  14,UTM,2
003434 :  ,ITS,14
003435 :  ,XTS,=1
003436 :  ,CALL,PRINT8
003437 :  15,WTC,
003438 :  ,UJ,
003439 :  KAPUT:,BSS,
003440 :  ,XTS,DGN
003441 :  15,ATX,
003442 :  ,ARX,=5
003443 :  ,XTS,=1
003444 :  ,CALL,PRINT8
003445 :  15,WTC,
003446 :  ,UJ,
003447 :  RW40:,BSS,
003448 :  12,VTM,1-32
003449 :  R*W:13,VJM,READW
003450 :  W*W:13,VJM,WRITW
003451 :  12,VLM,R*W
003452 :  11,UJ,
003453 :  READW:7,MTJ, 14
003454 :  14,UTM,-1024
003455 :  14,V1M,ROLDTR
003456 :  ,XTA,WRW
003457 :  ,ARX,=1
003458 :  ,ATX,WRW
003459 :  ,TAPE,WRW
003460 :  7,VTM,
003461 :  ROLDTR:,BSS,
003462 :  7,UTC,NEWCAT+1024
003463 :  ,XTA,
003464 :  7,UTM,1
003465 :  13,UJ,
003466 :  WRITW:,BSS,
003467 :  6,MTJ,14
003468 :  14,UTM,-1024
003469 :  14,V1M,WOLDTR
003470 :  ,XTS,
003471 :  ,DRUM,LWWC
003472 :  ,XTA, LWWC
003473 :  ,ARX,=7741
003474 :  ,AAX,CONST
003475 :  ,ATX, LWWC
003476 :  ,STX,
003477 :  6,VTM,
003478 :  WOLDTR:,BSS,
003479 :  6,UTC,NEWCAT
003480 :  ,ATX,
003481 :  6,UTM,1
003482 :  13,UJ,
003483 :  LWW1:,,NCL
003484 :  ,,
003485 :  KAPUT1:,BSS,
003486 :  14,MTJ,4
003487 :  14,VTM,1-5
003488 :  *101:14,ITS,12
003489 :  14,VLM,*101
003490 :  ,XTS,ERROR
003491 :  ,UIA,NOHEAD
003492 :  14,VTM,HEAD1
003493 :  ,ITS,14
003494 :  14,UTM,3
003495 :  ,ITS,14
003496 :  14,VTM,22B
003497 :  ,ITS,14
003498 :  ,CALL,PRINT8
003499 :  NOHEAD:,BSS,
003500 :  4,XTA,1
003501 :  14,VTM,26
003502 :  ,ITS,14
003503 :  ,ATX,ERROR
003504 :    ,CALL,PT6/G
003505 :  14,VTM,1-5
003506 :  13,VTM,
003507 :  OO:13,STI,12
003508 :  13,UTM,-1
003509 :  14,VLM,OO
003510 :  4,MTJ,14
003511 :  ,UJ,CHECK*
003512 : C-----KOHCTAHTЫ-----------
003513 :  RW:,010,NCL
003514 :  ,,
003515 :  WW:,,NCL
003516 :  ,021,
003517 :  OCRW:,010,OCL
003518 :  ,021,
003519 :  LWW:,,NCL+64
003520 :  ,,
003521 :  FULWORD:,LOG,7777 7777 7777 7777
003522 :  TOZPER:,LOG,776735 77777 77777
003523 :  TOZCOM:,LOG,776726 77777 77777
003524 :  CONST:,LOG,7777 7777   7777 0037
003525 :  PY:,ISO,18HПEPEПOЛHEH KATAЛOГ
003526 :  HEAD1:,ISO,24HИCПOPЧEHЫ ПOДПPOГPAMMЫ:
003527 :  BADL:,ISO,34HИCПOPЧEH KATAЛOГ TEMPORARY LIBRARY
003528 :  BADOV:,ISO,35HMAЛO MECTA ДЛЯ OVERLAY
003529 :  MNT:,TEXT,8HMONITOR*
003530 :  BADCC:,ISO,35HИCПOPЧEH KATAЛOГ COMMON LIBRARY
003531 :  BADPC:,ISO,35HИCПOPЧEH KATAЛOГ PERSONAL LIBRARY
003532 :  PCAT:,014,OCL
003533 :  ,021,0007
003534 :  STCAT:,LOG,2104000
003535 :  SAV:8,,NCL+65
003536 :  ,,201B
003537 :  SAV1:8,,NCL+67
003538 :  ,,301B
003539 :  WC:,BSS,2
003540 :  WRW:,BSS,1
003541 :  OCRWC:,BSS,1
003542 :  LWWC:,BSS,1
003543 :  TOZERO:,BSS,1
003544 :  HEAD:,BSS,1
003545 :  SHIFT:,BSS,1
003546 :  WWC:,BSS,1
003547 :  PLACE:,BSS,1
003548 :  FLAG :,BSS,1
003549 :  DGN:,BSS,1
003550 :  D:,BSS,1
003551 :  INF:,BSS,1
003552 :  ENDOV:,BSS,1
003553 :  BOUND:,BSS,
003554 :        ,BSS,1
003555 :  ERROR:,BSS,1
003556 :  ,END,
003557 :  LIBRARY:,NAME,..19.04.73
003558 :  LOADLIS*:,LC,1.
003559 :  NOLDLIS*:,EQU,LOADLIS*.
003560 :  LIBLIST:,SUBP,.
003561 :   *REGIM//:,LC,1
003562 :  ,ITA,13.
003563 :  ,ATX,I13.
003564 :  ,XTA,44B.
003565 :  ,ATX,R44.
003566 :   3,VTM,
003567 :  ,DRUM,46B.
003568 :   ,XTA,*REGIM//
003569 :   ,ATX,SR
003570 :   ,AOX,=10 00000
003571 :   ,ATX,*REGIM//
003572 :  LOOP:,BSS,.
003573 :  3,UTC,LIBLIST.
003574 :  ,XTA,.
003575 :  3,ATX,6000B.
003576 :   ,UZA,ENDL
003577 :  3,UTM,1.
003578 :  ,ITA,3.
003579 :  ,AAX,=1000.
003580 :  ,UZA,LOOP.
003581 :  ENDL:,XTA,NOLDLIS*.
003582 :  3,VZM,RET.
003583 :  ,U1A,LIB.
003584 :  ,XTA,44B.
003585 :  ,AAX,=37777777 77777777.
003586 :  ,ATX,44B.
003587 :   LIB:,XTA,R/LDL
003588 :   ,CTX,200B
003589 :   ,XTA,J/LDL
003590 :   ,CTX,201B
003591 :   ,XTA,R/BODY
003592 :   ,CTX,202B
003593 :   ,XTA,J/RET
003594 :   ,CTX,203B
003595 :   ,XTA,IR/BODY
003596 :   ,CTX,204B
003597 :   ,DRUM,=100 0020 0026
003598 :   ,UJ,200B
003599 :   R/LDL:,DRUM,74B.ЧTEHИE LOADLIBR
003600 :   J/LDL:13,VTM,201B.BOЗBPAT-1
003601 :   ,UJ,2000B.B LOADLIBR
003602 :   R/BODY:,DRUM,204B.BOCCT.1 ЛИCTA
003603 :   J/RET:,UJ,RET.B LIBRARY
003604 :   IR/BODY:,010,100B
003605 :   ,020,26B
003606 :   RET:,BSS,
003607 :  ,DRUM,45B.
003608 :   ,XTA,SR
003609 :   ,ATX,*REGIM//
003610 :  ,XTA,R44.
003611 :  ,ATX,44B.
003612 :  ,WTC,I13.
003613 :  ,UJ,.
003614 :  I13:,BSS,1.
003615 :  R44:,BSS,1.
003616 :   SR:,BSS,1
003617 :  ,END,.
003618 :   LIBRPN*:,NAME,
003619 :   ENTRYAR:,LP,100
003620 :   WBEGFLAG:LC,BLOCK,(1),WORDADR
003621 :   CATALOG:,PP,2048
003622 :   LEX:,EQU,43B
003623 :   ARD:,EQU,47B
003624 :   LIBPUNCH:,SUBP,
003625 :   READSTAR:,SUBP,
003626 :   PRINT8  :,SUBP,
003627 :   STAPUNCH:,SUBP,
003628 :   STAPUNCH:I,BLOCK,PERCOSY1,PERSTAN1
003629 :   ,CONT,PERCOSY2(0),PERSTAN2(0),PERSTANE
003630 :   ,CONT,PERCOSY3(0),PERSTAN3
003631 :  C       : 14,VTM, -13
003632 :          : 14,ITS, 14
003633 :            14,VLM, *
003634 :             7,VTM, C
003635 :              ,XTA, ARD
003636 :            7,AAX, NLIS-C
003637 :           14,VTM, CATALOG
003638 :              ,ITS, 14
003639 :              ,ASN, 64-20
003640 :            15,AOX,
003641 :             7,ATX, WL-C
003642 :              7,*70, WL-C
003643 :             7,XTA, WL-C
003644 :              7,ARX, CEX-C
003645 :             7,ATX, WL-C
003646 :             7,*70, WL-C
003647 :          :  4,VTM, LIBPUNCH
003648 :  RLIST   :  1,VTM, CATALOG+2
003649 :             4,XTA,
003650 :             7,ATX, R-C
003651 :              ,AEX,
003652 :              ,UZA, EXIT
003653 :      SCHN:  1,XTA, 1
003654 :             7,AEX, C7-C
003655 :              ,UZA, EABS
003656 :             1,XTA,
003657 :              7,AEX, R-C
003658 :             ,UZA,SCHN1
003659 :       1,UTM,2
003660 :      ,UJ, SCHN
003661 :  SCHN1:7,XTS, R-C
003662 :             14,VTM, 20
003663 :             ,ITS, 14
003664 :   ,CALL,PT6/G
003665 :  SLENT1  :  1,XTA, 3
003666 :             7,AAX, LIBMASK-C
003667 :             ,U1A, WRL
003668 :             1,UTM, 2
003669 :             ,UJ,  SLENT1
003670 :  WRL     :    ,ITA, 1
003671 :              ,UTC, WORDADR
003672 :              ,ATX,
003673 :            14,VTM, READSTAR
003674 :             7,XTA, C7-C
003675 :              ,UTC, WBEGFLAG
003676 :              ,ATX,
003677 :              ,ITS, 14
003678 :             3,VJM, PUNCHLIB
003679 :          :   ,STX,
003680 :             4,UTM, 1
003681 :              ,UJ,  RLIST
003682 :  PUNCHLIB:   ,ATI, 6
003683 :             2,VTM, ENTRYAR
003684 :  CHKENTRY:  6,UTC,
003685 :            13,VJM,
003686 :          :  7,STX, STR-C
003687 :             6,UTC,
003688 :            13,VJM,
003689 :          :  7,STX, STR1-C
003690 :             7,XTA, STR1-C
003691 :             7,AAX, MSTR-C
003692 :              ,UZA, CHEKSTR
003693 :             7,XTA, STR1-C
003694 :             7,AAX, LIBMASK-C
003695 :              ,U1A, MAIN
003696 :             7,XTA, STR1-C
003697 :             7,AOX, C2-C
003698 :             7,ATX, STR1-C
003699 :              ,UJ,  PERF
003700 :  MAIN    :  7,XTA, STR-C
003701 :             7,ATX, NAME-C
003702 :          :   ,UJ,  PERF
003703 :  PERF    :   7,XTA, STR1-C
003704 :             7,AAX, MA3L-C
003705 :             7,AOX, C2-C
003706 :             2,ATX, 1
003707 :             7,XTA, STR-C
003708 :             2,ATX,
003709 :             2,UTM, 2
003710 :          :   ,UJ,  CHKENTRY
003711 :  CHEKSTR :   7,XTA,C7-C
003712 :             2,ATX,
003713 :             2,VTM, ENTRYAR
003714 :             7,XTS, NAME-C
003715 :            13,VJM, PERSTAN1               .
003716 :  CHEKSTR1:  2,XTA,
003717 :             7,AEX, C7-C
003718 :              ,UZA, CHEKSTR2
003719 :             2,XTS,
003720 :            13,VJM, PERSTAN2
003721 :             2,XTS, 1
003722 :            13,VJM, PERSTAN2
003723 :          :   2,UTM, 2
003724 :              , UJ, CHEKSTR1
003725 :  CHEKSTR2 :   6,UTC,
003726 :            13,VJM,
003727 :          :  7,STX, STR2-C
003728 :             7,XTA, STR-C
003729 :             7,AAX, MA4S-C
003730 :             7,XTS, STR-C
003731 :              ,ASN, 64+12
003732 :             7,AAX, MA4S-C
003733 :            15,ARX,
003734 :             7,XTS, STR-C
003735 :              ,ASN, 64+24
003736 :             7,AAX, MA4S-C
003737 :            15,ARX,
003738 :             7,XTS, STR-C
003739 :              ,ASN, 64+36
003740 :             7,AAX, MA4S-C
003741 :             15,ARX,
003742 :             7,XTS, STR1-C
003743 :              ,ASN, 64+30
003744 :             7,AAX, MA3L-C
003745 :            15,ARX,
003746 :             7,ATX, N3-C
003747 :             7,XTA, STR1-C
003748 :             7,AAX, MA3L-C
003749 :             7,XTS, STR1-C
003750 :              ,ASN, 64+15
003751 :             7,AAX, MA3L-C
003752 :            15,ARX,
003753 :             7,ATX, N2-C
003754 :             7,XTA, STR2-C
003755 :             7,AAX, MA3L-C
003756 :             7,XTS, STR2-C
003757 :              ,ASN, 64+30
003758 :             7,AAX, MA3L-C
003759 :            15,ARX,
003760 :             7,ATX, LBOD-C
003761 :             7,XTA, LBOD-C
003762 :             7,ARX, N2-C
003763 :             7,ARX, N3-C
003764 :             7,ATX, NPR-C
003765 :          :  7,XTS, STR-C
003766 :            7,AAX, MA4S-C
003767 :            13,VJM, PERSTAN2
003768 :          :  7,XTS, STR-C
003769 :              ,ASN, 64+12
003770 :             7,AAX, MA4S-C
003771 :            13,VJM, PERSTAN2
003772 :          :  7,XTS, STR-C
003773 :              ,ASN, 64+24
003774 :             7,AAX, MA4S-C
003775 :            13,VJM, PERSTAN2
003776 :          :  7,XTS, STR-C
003777 :              ,ASN, 64+36
003778 :             7,AAX, MA4S-C
003779 :            13,VJM, PERSTAN2
003780 :          :   7,XTS,STR1-C
003781 :              ,ASN, 64+30
003782 :            13,VJM, PERSTAN2
003783 :          :  7,XTS, STR2-C
003784 :             7,AAX, MA3L-C
003785 :            13,VJM, PERSTAN2
003786 :          :  7,XTS, STR2-C
003787 :              ,ASN, 64+15
003788 :             7,AAX, MA3L-C
003789 :            13,VJM, PERSTAN2
003790 :          :  7,XTS, STR2-C
003791 :              ,ASN, 64+30
003792 :             7,AAX, MA3L-C
003793 :            13,VJM, PERSTAN2
003794 :          :  7,XTS, STR1-C
003795 :              ,ASN, 64+15
003796 :             7,AAX, MA3L-C
003797 :            13,VJM, PERSTAN2
003798 :          :  7,XTS, STR1-C
003799 :             7,AAX, MA3L-C
003800 :            13,VJM, PERSTAN2
003801 :          :  7,XTA, BEPR-C
003802 :             7,ATX, CNPR-C
003803 :  PNCH    :  6,UTC,
003804 :            13,VJM,
003805 :          : 13,VJM, PERSTAN2
003806 :          :  7,XTA, CNPR-C
003807 :             7,ARX, C1-C
003808 :             7,ATX, CNPR-C
003809 :             7,AEX, NPR-C
003810 :              ,U1A, PNCH
003811 :          : 13,VJM, PERSTAN3
003812 :          :  3,UJ,
003813 :  EABS     : 7,XTS, R-C
003814 :           14,VTM, 10
003815 :              ,ITS, 14
003816 :   ,CALL,PT6/G
003817 :          : 14,VTM, T2
003818 :             ,ITS, 14
003819 :           14,VTM, T21
003820 :             ,ITS, 14
003821 :             ,XTS,
003822 :           13,VJM, PRINT8
003823 :          :  4,UTM, 1
003824 :             ,UJ,  RLIST
003825 :  EXIT    :   ,STX,
003826 :            14,VTM, 13                     .
003827 :  A       : 14,STI,                        .
003828 :            14,UTM, -1                     .
003829 :            14,V1M, A                      .
003830 :            13,UJ,                         .
003831 :    'CONST'
003832 :  R       :   ,OCT,
003833 :  NAME    :   ,OCT,
003834 :  MSK1    :   ,OCT, 4
003835 :  SW      :   ,OCT,
003836 :  MSK2    :   ,OCT, 0007 7777
003837 :  NST     :   ,OCT,
003838 :  C7      :   ,OCT, 7777 7777 7777 7777
003839 :  LIBMASK :   ,OCT, 5777 77
003840 :  CNST    :   ,OCT,
003841 :  CON1    :   ,OCT, 0000 0000 0000 0001
003842 :  C1      :   ,OCT, 0000 0000 0000 0001
003843 :  NST1    :   ,OCT,
003844 :  STR     :   ,OCT,
003845 :  STR1    :   ,OCT,
003846 :  STR2    :   ,OCT,
003847 :  STR3    :   ,OCT,
003848 :  STR4    :   ,OCT,
003849 :  STR5    :   ,OCT,
003850 :  STR6    :   ,OCT,
003851 :  STR7    :   ,OCT,
003852 :  STR10   :   ,OCT,
003853 :  STR11   :   ,OCT,
003854 :  MA4S    :   ,OCT, 0000 0000 0000 7777
003855 :  MA3L    :   ,OCT, 0000 0000 0007 7777
003856 :  N3      :   ,OCT,
003857 :  N2      :   ,OCT,
003858 :  LBOD    :   ,OCT,
003859 :  NPR     :   ,OCT,
003860 :  SW1     :   ,OCT,
003861 :  CNPR    :   ,OCT,
003862 :  BEPR    :   ,OCT,
003863 :  MSTR    :   ,OCT, 7
003864 :  C2      :   ,OCT, 2
003865 :  T2      :   ,8TEXT, ''THIS N''
003866 :          :   ,8TEXT, ''AME IS''
003867 :          :   ,8TEXT, '' ABSEN''
003868 :          :   ,8TEXT, ''T IN C''
003869 :  T21     :   ,8TEXT, ''ATALOG''
003870 :  WL      :   ,OCT,
003871 :  CEX      :   ,OCT, 0000 0100 0000 0001
003872 :  NLIS    :    ,OCT, 7777 4077 7777 7777
003873 :     ,END,
003874 :  LISTLIBT:,NAME,
003875 :  DATA*:,LC,2
003876 : C*******************************
003877 : C****PEAЛИЗAЦИЯ ФYHKЦИЙ*********
003878 : C****TO COMMON LYBRARY**********
003879 : C****TO PERSONAL LYBRARY********
003880 : C****MOCKBA  ИAЭ ИM. KYPЧATOBA**
003881 : C****ГOЛYБEB A.C. BECHA 1973.***
003882 : C*******************************
003883 :  WCW:,EQU,WKW
003884 :  *ORDER:,LC,1
003885 :  RBUF:,PC,1024
003886 :  WBUF:,PC,1024
003887 :  CATLG:,PC,2048
003888 :  NEWCAT:,PC,2048
003889 :  NCL:,P/P,(NEWCAT)16
003890 :  OCL:,P/P,(CATLG )16
003891 :  WBL:,P/P,(WBUF)16
003892 :  RBL:,P/P,(RBUF)16
003893 :  *MONCRD*:,LC,12
003894 :   MONCRD :,EQU,*MONCRD*
003895 :  *REGIM//:,LC,1
003896 :  N40:,EQU,5
003897 :  CN:,EQU,10
003898 :  PR:,EQU,7
003899 :  PW:,EQU,6
003900 :  BAS:,BSS,
003901 :  1,BASE,*
003902 :  ,ATX,ПAPOЛЬ
003903 : C--ДЛЯ ПEPCOHAЛЬHЫX ПOЛЬЗOBATEЛEЙ------
003904 :  ,XTA,=1
003905 :  ,UJ,P
003906 :  LIBTAPE:,ENTRY,
003907 :  1,VTM,BAS
003908 :  ,XTA,
003909 :  P:,BSS,
003910 :  ,ATX,FLAGCOM
003911 :  ,ITA,13
003912 :  ,ATX,RETLIB
003913 :  ,XTA,
003914 :  ,ATX,INF
003915 :  ,ATX,FL
003916 :  ,ATX,ERR
003917 :  ,XTA,=670000
003918 :  ,ATX,PLACE
003919 :  ,XTA,=1000
003920 :  ,ATX,LLENG
003921 :  10,VTM,
003922 :  ,CALL,PARAMT*
003923 :  9,VTM,PLACE
003924 :  10,VTM,4
003925 :  ,CALL,PARAMT*.MECTO HA MЛ
003926 :  10,VTM,5
003927 :  14,V1M,RETURN
003928 :  9,VTM,LLENG
003929 :  ,CALL,PARAMT*.OГPAHИЧИTEЛЬ
003930 :  14,V1M,RETURN
003931 :  ,XTA,FLAGCOM.ФЛAГ COMMON
003932 :  ,UZA,PRSNL
003933 :  ,XTA,CPRLOC
003934 :  ,ATX,PRLOC
003935 :  ,XTA,PLACE.MECTO HA MЛ
003936 :  ,ATX,WC
003937 :  ,AOX,LST
003938 :  ,ATX,WKW
003939 :  ,ARX,=1
003940 :  ,ATX,/1WKW
003941 :  ,XTA,WC
003942 :  ,ARX,=7
003943 :  ,AOX,NCLST
003944 :  ,ATX,NCKW
003945 :  ,UJ,BOUNDC
003946 :  PRSNL:,BSS,
003947 :  ,XTA,PERSPL
003948 :  ,ATX,PRLOC
003949 :  ,XTA,PLACE
003950 :  ,ATX,WC
003951 :  ,AOX,NCLST
003952 :  ,ATX,NCKW
003953 :  ,XTA,WC
003954 :  ,ARX,=2
003955 :  ,AOX,LST
003956 :  ,ATX,WKW
003957 :  BOUNDC:,BSS,
003958 :  ,XTA,WC
003959 :  ,ATX,AFZ
003960 :  ,ARX,LLENG.CK. TPAKTOB
003961 :  ,AEX,ENDCAT
003962 :  ,ARX,=1
003963 :  ,ATX,BOUND
003964 :  4,VTM,1
003965 :  3,VTM,
003966 :  ,*70,CKW
003967 :  ,XTA,FLAGCOM
003968 :  ,UZA,*111
003969 : C---ЧTEHИE 0-6 TPAKTOB-----
003970 :  13,VTM,1-7
003971 :  INR:,*70,IRKW
003972 :  13,MTJ,14
003973 :  14,UTM,5
003974 :  14,VIM,//1
003975 :  ,XTA,*REGIM//
003976 :  ,AAX,=400000
003977 :  ,UZA,//2.HEБЫЛO KOMMOH
003978 :  ,DRUM,SAVD1
003979 :  //2:,BSS,
003980 :  14,VTM,WBUF
003981 :  14,XTA,1400B
003982 :  ,AEX,TXTM
003983 :  ,UIA,//1
003984 :  14,XTA,1401B
003985 :  ,ATX,INF
003986 :  14,XTA,1403B
003987 :  ,ASN,64-3
003988 :  ,ASN,64+3
003989 :  ,ATX,BEG
003990 :  //1:,BSS,
003991 :  ,XTA,IRKW
003992 :  ,ARX,=1
003993 :  ,ATX,IRKW
003994 :  6,VJM,MAT
003995 :  ,*70,WKW
003996 :  ,XTA,WKW
003997 :  ,ARX,=1
003998 :  ,ATX,WKW
003999 :  13,VLM,INR
004000 :  ,XTA,WKW
004001 :  ,ARX,=2
004002 :  ,ATX,WKW
004003 :  *111:,BSS,
004004 :  6,VTM,
004005 :  '----BTOPOЙ ЛИCT KATAЛOГA----'
004006 :  ,XTA,CKW
004007 :  ,ARX,=1000 000 0001
004008 :  ,ATX,WC
004009 :  ,DRUM,WC
004010 :  '---------------------------.'
004011 : C--AHAЛИЗ KATAЛOГA-----
004012 :  14,VTM,CATLG
004013 :  14,XTA,
004014 :  ,AAX,=2000000
004015 :  ,UZA,KAPUT
004016 :  14,XTA,
004017 :  ,AAX,=700000
004018 :  ,UZA,KAPUT
004019 :  14,XTA,
004020 :  ,AAX,=7776 0000 7400 0037
004021 :  ,UIA,KAPUT
004022 :  14,XTA,1
004023 :  ,UIA,KAPUT
004024 :  '---PAЗMEP BP. KAT.-----'
004025 :  14,VTM,
004026 :  TEMPD:,BSS,
004027 :  14,UTC,CATLG+1
004028 :  ,XTA,
004029 :  ,AAX,=:4
004030 :  14,UTM,2
004031 :  14,MTJ,13
004032 :  13,UTM,-2048
004033 :  13,VZM,KAPUT
004034 :  ,UZA ,TEMPD
004035 :  14,UTM,-2
004036 :  11,VTM,
004037 :  ,XTA,FLAGCOM
004038 :  ,UIA,EC
004039 :  14,MTJ,11
004040 :  14,UTC,CATLG
004041 :  ,ATX,
004042 :  11,MTJ,9
004043 :  ,UJ,GIVPR
004044 :  EC:,BSS,
004045 :  11,UTC,CATLG
004046 :  ,XTA,
004047 :  11,UTM,2
004048 :  ,U1A,EC
004049 :  11,UTM,-2
004050 :  11,MTJ,2
004051 :  11,MTJ,9
004052 :  '---------------------------'
004053 :  GIVPR:,BSS,
004054 :  4,VZM,NEXTPR
004055 :  3,UTC,*ORDER
004056 :  ,XTA,
004057 :  3,UTM,1
004058 :  ,U1A,SEEK
004059 :  4,VTM,
004060 :  2,VTM,2
004061 : C---ИCKЛЮЧEHИE ПOДПPOГPAMMЫ-ПOPЯДKA---
004062 :  D*O:2,UTC,CATLG
004063 :  ,XTA,
004064 :  2,UTM,2
004065 :  ,UZA,NEXTPR
004066 :  ,AEX,TXT*ARR
004067 :  ,UZA,CLEEN
004068 :  ,UJ,D*O
004069 : C--ПOИCK ЗAДAHHOЙ ПOДПPOГPAMMЫ---
004070 :  SEEK:,BSS,
004071 :  2,VTM,4
004072 :  BG:2,UTC,CATLG-2
004073 :  ,XTA,
004074 :  ,UZA,GIVPR
004075 :  3,UTC,*ORDER-1
004076 :  ,AEX,
004077 :  ,UZA,COUNT
004078 :  2,UTM,2
004079 :  ,UJ,BG
004080 : C---ПOДBOД K KOHЦY BXOДOB-----
004081 :  COUNT:,BSS,
004082 :  2,UTC,CATLG+1
004083 :  ,XTA,
004084 :  ,AAX,IFENTRY
004085 :  ,U1A,ENTRY
004086 :  2,UTM,2
004087 :  ,UJ,COUNT
004088 :  NEXTPR:,BSS,
004089 :  11,MTJ,2
004090 :  2,UTM,-2
004091 :  2,VZM,END
004092 :  IFZERO:,BSS,
004093 :  2,UTC,CATLG
004094 :  ,XTA,
004095 :  2,UTM,2
004096 :  ,AEX,=:1
004097 :  ,U1A,ENTRY
004098 :  2,UTM,-4
004099 :  2,V1M,IFZERO
004100 :  ,UJ,END
004101 : C---HE BXOД ЛИ--------------
004102 :  ENTRY:,BSS,
004103 :  12,VTM,
004104 :  EB:,BSS,
004105 :  2,UTC,CATLG-1
004106 :  ,XTA,
004107 :  ,AAX,IFENTRY
004108 :  ,UIA,READ
004109 :  2,UTM,-2
004110 :  12,UTM,-2
004111 :  ,UJ,EB
004112 :  '--------------------------'
004113 : C---ФOPMИPOBAHИE KW----------
004114 :  READ:,BSS,
004115 :  2,UTC,CATLG-1
004116 :  ,XTA,
004117 :  ,APX,=:403777
004118 :  ,AEX,=:4
004119 :  ,AUX,=4 0000 0077 0037
004120 :  ,AEX,RKWC
004121 :  ,ATX,WC
004122 :  ,ARX,=7741
004123 :  ,AAX,CONST
004124 :  ,AEX,RKW
004125 :  ,UZA,NORW
004126 :  ,XTA,WC
004127 :  ,ATX,RKW
004128 :  7,VTM,2000B
004129 :  13,VJM,RW
004130 :  NORW:,BSS,
004131 :  2,UTC,CATLG-1
004132 :  ,XTA,
004133 :  ,AAX,=:174
004134 :  ,ASN,64+36
004135 :  ,ATI,7
004136 :  ,ITA,6
004137 :  ,ASN,64-36
004138 :  ,ATX,WC+1
004139 :  ,AAX,=:2
004140 :  ,ASN,64+16
004141 :  ,ATX,WC+2
004142 : C---ЗAHECEHИE ПPOГPAMMЫ B HOBЫЙ KATAЛOГ
004143 :  2,UTC,CATLG-1
004144 :  ,XTA,
004145 :  ,AAX, =77777 77777
004146 :  ,AOX,PRLOC
004147 :  ,AOX,WC+1
004148 :  ,ARX,WC+2
004149 :  ,ATX,WC+1
004150 : C---CЧИTЫBAHИE ПPOГPAMMЫ
004151 :  13,VJM,READSUB
004152 :  ,XTA,FLAGCOM
004153 :  ,UIA,NOPRI
004154 :  ,XTA,WKW
004155 :  ,AAX,=77 7777
004156 :  ,ARX,BOUND
004157 :  ,UIA,KAPUT1
004158 :  NOPRI:,BSS,
004159 :  12,J+M,9
004160 :  9,UTM,-2
004161 :  ,XTA,WC+1
004162 :  9,UTC,NEWCAT+1
004163 :  ,ATX,
004164 :  2,UTC,CATLG-2
004165 :  ,XTA,
004166 :  9,UTC,NEWCAT
004167 :  ,ATX,
004168 :  ,XTA,=:1
004169 :  2,UTC,CATLG-2
004170 :  ,ATX,
004171 :  2,UTC,CATLG-1
004172 :  ,ATX,
004173 :  12,VZM,GIVPR
004174 :  14,VTM,2
004175 :  PLACEE:,BSS,
004176 :  2,UTM,2
004177 :  2,UTC,CATLG-2
004178 :  ,XTA,
004179 :  14,UTC,NEWCAT
004180 :  9,ATX,
004181 :  2,UTC,CATLG-1
004182 :  ,XTA,
004183 :  14,UTC,NEWCAT+1
004184 :  9,ATX,
004185 :  ,XTA,=:1
004186 :  2,UTC,CATLG-2
004187 :  ,ATX,
004188 :  2,UTC,CATLG-1
004189 :  ,ATX,
004190 :  14,UTM,2
004191 :  12,UTM,2
004192 :  12,VZM,GIVPR
004193 :  ,UJ,PLACEE
004194 : C---ЗAЧИCTKA CTAPOГO KATAЛOГA
004195 :  CLEEN:,BSS,
004196 :  ,XTA,=:1
004197 :  2,UTC,CATLG-2
004198 :  ,ATX,
004199 :  2,UTC,CATLG-1
004200 :  ,ATX,
004201 :  2,UTC,CATLG+1
004202 :  ,XTA,
004203 :  ,AAX,IFENTRY
004204 :  ,U1A,GIVPR
004205 :  2,UTM,2
004206 :  ,UJ,CLEEN
004207 :  '----------BЫXOД ИЗ RWLIB----'
004208 :  END:,BSS,
004209 :  ,ITA,6
004210 :  ,ATX,WC
004211 :  ,XTA,PRLOC
004212 :  ,ASN,64+20
004213 :  ,ARX,WC
004214 :  ,AAX,=7777777
004215 :  ,UTC,NEWCAT
004216 :  ,ATX,
004217 :  ,XTA,ПAPOЛЬ
004218 :  ,AEX,=6HBOЛKOB
004219 :  ,UZA,ADIABL
004220 : C---ПOДГOTOBKA K ЧTEHИЮ-----
004221 : C------И ЗAПИCИ OVERLAY----
004222 :  ,XTA,INF
004223 :  ,UZA,ADIABL
004224 :  ,XTA,BEG
004225 :  ,ASN,64-2
004226 :  ,AUX,=77 0037
004227 :  ,AOX,RKWC
004228 :  ,XTS,*REGIM//
004229 :  ,AAX,=400000
004230 :  ,ASN,64-21
004231 :  15,AEX,
004232 :  ,ATX,RKW
004233 :  PR,VTM,2000B
004234 :  13,VJM,RW
004235 :  ,XTA,BEG
004236 :  ,ASN,64+30
004237 :  ,ASN,64-5
004238 :  ,AAX,=1777
004239 :  ,ATI,PR
004240 : C---BЫЧИCЛEHИE HOBOЙ PAЗHИЦЫ----
004241 :  ,UTC,NEWCAT
004242 :  ,XTA,
004243 :  ,ASN,64-25
004244 :  ,AEX,ENDCAT
004245 :  ,ARX,BEG
004246 :  ,U1A,GOOD
004247 :  ,ITS,6
004248 :  ,ITS,13
004249 :  14,VTM,1-5
004250 :  SAVI:14,ITS,12
004251 :  14,VLM,SAVI
004252 :  14,VTM,PY
004253 :  ,ITS,14
004254 :  14,UTM,2
004255 :  ,ITS,14
004256 :  6,VJM,MATЛ
004257 :  ,STI,6
004258 :  ,XTA,
004259 :  GOOD:,BSS,
004260 :  ,AAX,=:777 777
004261 :  ,ATX,ROW
004262 : C---ДЛИHA OVERLAY------
004263 :  ,XTA,BEG
004264 :  ,AAX,=:777 777
004265 :  ,ASN,64+20
004266 :  ,AEX,ENDCAT
004267 :  ,XTS,INF
004268 :  ,AAX,=77777 00000
004269 :  ,ARX,=1040 00000
004270 :  15,ARX,
004271 :  ,ASN,64+10
004272 :  ,ATI,N40
004273 : C---ПEPEПИCЬ OVERLAY------
004274 :  RWOV:,BSS,
004275 :  CN,VTM,-31
004276 :  RW40:,BSS,
004277 :  13,VJM,RW
004278 :  13,VJM,WW
004279 :  CN,VLM,RW40
004280 :  N40,UTM,-1
004281 :  N40,V1M,RWOV
004282 :  ADIABL:,BSS,
004283 :  6,UTM,-1024
004284 :  6,VZM,SBR
004285 :  ,XTA,ПAPOЛЬ
004286 :  ,AEX,=6HBOЛKOB
004287 :  ,UZA,STEP
004288 :  SBR:,BSS,
004289 :  6,VTM,2000B
004290 :  '---------------------------'
004291 :  13,VJM,WW
004292 :  STEP:,BSS,
004293 :  ,XTA,
004294 :  11,UTC,NEWCAT
004295 :  ,ATX,
004296 :  ,XTA,ENDCAT
004297 :  11,UTC,NEWCAT+1
004298 :  ,ATX,
004299 :  ,ITA,9
004300 :  ,AEX,=77777
004301 :  ,ATI,8
004302 :  8,UTM,3
004303 :  9,MTJ,14
004304 :  11,MTJ,13
004305 :  8,J+M,13
004306 :  SHIFT:,BSS,
004307 :  14,UTC,NEWCAT
004308 :  ,XTA,
004309 :  8,UTC,
004310 :  14,UTC,NEWCAT
004311 :  ,ATX,
004312 :  11,UTM,-1
004313 :  14,UTM,1
004314 :  11,V1M,SHIFT
004315 :  ,XTA,WCW
004316 :  ,ATX,WC+2
004317 : C---ЗAHECEHИE B KATAЛOГ ДATЫ И KOMMEHTAPИЯ--
004318 :  ,ITA,13
004319 :  ,NTR,3
004320 :  ,A-X,=3760
004321 :  ,UZA,NCOM
004322 :  13,UTM,NEWCAT
004323 :  ,XTA,TE
004324 :  13,ATX,2
004325 :  ,XTA,TE+1
004326 :  13,ATX,3
004327 :  13,MTJ,6
004328 :  13,UTM,4
004329 :  ,XTA,DATA*
004330 :  13,ATX,
004331 :  ,XTA,DATA*+1
004332 :  13,ATX,1
004333 :  6,MTJ,8
004334 :  8,UTM,5
004335 :  10,VTM,1
004336 :  ,CALL,PARAMT*
004337 :  13,VTM,MONCRD
004338 :  14,VTM,1-12
004339 :  COM:8,UTM,1
004340 :  13,XTA,
004341 :  8,ATX,
004342 :  13,UTM,1
004343 :  14,VLM,COM
004344 :  14,VTM,NEWCAT+2046
004345 :  14,XTA,
004346 :  ,UIA,NCOM
004347 :  8,XTA,-13
004348 :  14,ATX,
004349 :  8,XTA,-11
004350 :  ,AAX,=:776
004351 :  ,ASN,64+16
004352 :  8,XTS,-12
004353 :  ,AAX,=:777774
004354 :  15,AOX,
004355 :  14,ATX,1
004356 :  NCOM:,BSS,
004357 :  ,NTR,20B
004358 :  ,XTA,WKW
004359 :  ,AAX,=777777
004360 :  ,ATX,WC+1
004361 :  '----------------------------'
004362 :  ,XTA,ПAPOЛЬ
004363 :  ,AEX,=6HBOЛKOB
004364 :  ,UZA,БEЗCБP
004365 :  ,XTA,FLAGCOM
004366 :  ,UZA,KKK
004367 :  ,DRUM,/1TP
004368 :  ,XTA,*REGIM//
004369 :  ,AAX,=400000
004370 :  ,UZA,*1/
004371 :  ,DRUM,SAVD
004372 :  ,DRUM,SAVD1
004373 :  *1/:,BSS,
004374 :   13,VTM,WBUF
004375 :   13,XTA,1400B
004376 :   ,AEX,TXTM
004377 :   ,U1A,NOMON
004378 :   13,XTA,1377B
004379 :   ,AEX,=0014 0000 0021 0036
004380 :   ,ASN,64+15
004381 :   ,U1A,NOMON
004382 :   ,DRUM,=02 0000
004383 :   13,DRUM,1377B.ЛИCT C
004384 :   ,XTA,PLACE.MOHИTOPOM
004385 :   ,AAX,=77 3777.B XBOCT
004386 :   ,ARX,=77.COMMON
004387 :   15,ATX,1.LIBRARY
004388 :   15,*70,1.
004389 :   ,DRUM,=0010 0000 0002 0000
004390 :   NO MON:,BSS,
004391 :  14,VTM,WBUF
004392 :  14,XTA,1401B
004393 :  ,AAX,=77777 77777
004394 :  ,AOX,ROW
004395 :  14,ATX,1401B
004396 :  ,ASN,64-7
004397 :  ,AUX,=6H'''7''7''''3''7'
004398 :  ,ARX,=6H 00 00
004399 :  ,ATX,PEЗ+1
004400 :  14,VTM,PEЗ
004401 :  ,ITS,14
004402 :  14,UTM,1
004403 :  ,ITS,14
004404 :  ,XTS,=1
004405 :  ,CALL,PRINT8
004406 :  14,VTM,WBUF+1776B
004407 :  ,XTA,DATA*
004408 :  14,ATX,
004409 :  ,XTA,DATA*+1
004410 :  14,ATX,1
004411 :  ,TAPE,/1WKW.OФOPMЛEHИE 1 ЗOHЫ
004412 :  KKK:,BSS,
004413 :  ,XTA,NCKW
004414 :  ,ATX,WKW
004415 :  6,VJM,MAT
004416 :  ,*70,WKW
004417 : C---ЗAПИCЬ KATAЛOГA------
004418 :  ,XTA,NCKW
004419 :  ,ARX,=1000 000 0001
004420 :  ,ATX,NCKW
004421 :  ,ATX,WKW
004422 :  6,VJM,MAT
004423 :  ,*70,WKW
004424 :  '------------------------------'
004425 :  БEЗCБP:,BSS,
004426 : C---ПEЧATЬ ДЛИHЫ---------
004427 :  ,XTA,AFZ
004428 :  ,AEX,ENDCAT
004429 :  ,ARX,WC+1
004430 :  ,APX,=777
004431 :  ,AUX,=01603407
004432 :  ,ARX,MAS
004433 :  ,ATX,ДЛИHA+2
004434 :  ,ITS,13
004435 :  14,VTM,1-5
004436 :  *IIII:14,ITS,12
004437 :  14,VLM,*IIII
004438 :  14,VTM,ДЛИHA
004439 :  ,ITS,14
004440 :  14,UTM,3
004441 :  ,ITS,14
004442 :  6,VJM,MATЛ
004443 : C---ИHФOPMAЦИЯ ДЛЯ BOЛKOBA-----
004444 :  ,XTA,LLENG.ДЛИHA БИБЛИOTEKИ
004445 :  ,XTS,AFZ.HAЧAЛO БИБЛ.
004446 : C----------------------------
004447 :  RETURN:,BSS,
004448 :  ,WTC,RETLIB
004449 :  ,UJ,
004450 : C---BЫЧИCЛEHИE ДЛИHЫ CTAHДAPTHOГO--
004451 : C---MACCИBA И EГO ЧTEHИE----------
004452 :  READSUB:,ITA,13
004453 :  ,ATX,RET13
004454 :   5,VTM,
004455 :  13,VJM,RW
004456 :   ,AAX,=7777 7777.ГPЯЗЬ B PEЗ.ГPYППE !
004457 :  ,ATX,WC
004458 :   13,VTM,-3
004459 :   SUM1W:,XTA,WC
004460 :   ,ASN,64-12
004461 :   ,ATX,WC
004462 :   ,YTA,
004463 :   ,ATI,14
004464 :   14,J+M,5
004465 :   13,VLM,SUM1W
004466 :  13,VJM,RW
004467 :  ,ATI,14
004468 :  14,J+M,5
004469 :  ,ASN,64+15
004470 :  ,ATI,14
004471 :  14,J+M,5
004472 :  ,ASN,64+15
004473 :  ,ATI,14
004474 :  14,J+M,5
004475 :  13,VJM,RW
004476 :  ,ATX,WC
004477 :  ,ATI,14
004478 :  14,J+M,5
004479 :  5,UTM,3
004480 :  7,UTM,-3
004481 :  ,ASN,64+30
004482 :  ,ATI,14
004483 :  14,J+M,5
004484 :  ,ITA,5
004485 :  ,ARX,=37
004486 :  ,AAX,=77740
004487 :  ,ATI,5
004488 : C---KOHTPOЛЬ CTAHДAPTHOГO MACCИBA--
004489 :  ,XTA,WC
004490 :  ,ATI,13
004491 :  ,ASN,64+15
004492 :  ,ATI,14
004493 :  14,J+M,13
004494 :  ,ASN,64+15
004495 :  ,ATI,14
004496 :  14,J+M,13
004497 :  2,UTC,CATLG-1
004498 :  ,XTA,
004499 :  ,ASN,64+15
004500 :  ,AAX,=77777
004501 :  ,ITS,13
004502 :  15,AEX,.CPABHEHИE ДЛИH HA ЗAГPYЗKE---
004503 :  ,UIA,KAPUT1.ДЛИHЫ HE COBПAЛИ
004504 :  RWSUB:13,VJM,RW
004505 :  13,VJM,WW
004506 :  NOWRITE:5,UTM,-1
004507 :  5,V1M,RWSUB
004508 :  ,WTC,RET13
004509 :  ,UJ,
004510 : C    ДAЙ CЛOBO
004511 :  RW:7,MTJ,14
004512 :  14,UTM,76000B
004513 :  14,V1M,ROLDTR
004514 :  ,*70,RKW
004515 :  ,XTA,RKW
004516 :  ,ARX,=7741
004517 :  ,AAX,CONST
004518 :  ,ATX,RKW
004519 :  7,VTM,
004520 :  ROLDTR:,BSS,
004521 :  7,UTC,RBUF
004522 :  ,XTA,
004523 :  7,UTM,1
004524 :  13,UJ,
004525 : C    ЗAПИCЬ CЛOBA
004526 :  WW:6,MTJ,14
004527 :  14,UTM,76000B
004528 :  14,V1M,WOLDTR
004529 :  ,XTS,
004530 :  6,VJM,MAT
004531 :  ,*70,WCW
004532 :  ,XTA,WKW
004533 :  ,ARX,=1
004534 :  ,ATX,WKW
004535 :  ,XTA,PRLOC
004536 :  ,ARX,=:000001
004537 :  ,ATX,PRLOC
004538 :  ,STX,
004539 :  6,VTM,
004540 :  WOLDTR:,BSS,
004541 :  6,UTC,WBUF
004542 :  ,ATX,
004543 :  6,UTM,1
004544 :  13,UJ,
004545 :  '-------PYГAHЬ---------------'
004546 :  MAT:,BSS,
004547 :  ,XTA,WKW
004548 :  ,AAX,=777777
004549 :  ,ARX,BOUND
004550 :  ,NTR,20B
004551 :  6,UIA,
004552 :  ,XTA,WKW
004553 :  ,AOX,=:001
004554 :  ,ATX,WKW
004555 :  ,XTA,FL
004556 :  6,UIA,
004557 :  ,XTA,=100000
004558 :  ,AOX,*REGIM//
004559 :  ,ATX,*REGIM//
004560 :  ,ATX,FL
004561 :  ,ITS,13
004562 :  14,VTM,1-5
004563 :  *444:14,ITS,12
004564 :  14,VLM,*444
004565 :  14,VTM,PYMЛ
004566 :  ,ITS,14
004567 :  14,VTM,PYMЛ+1
004568 :  ,ITS,14
004569 :  MATЛ:,BSS,
004570 :  ,XTS,=1
004571 :  ,CALL,PRINT8
004572 :  14,VTM,1-5
004573 :  13,VTM,
004574 :  *666:13,STI,12
004575 :  13,UTM,-1
004576 :  14,VLM,*666
004577 :  ,STI,13
004578 :  6,UJ,
004579 : C---ИCПOPЧEHA T.LIBRARY------
004580 :  KAPUT:,BSS,
004581 :  14,VTM,BADL
004582 :  ,ITS,14
004583 :  14,UTM,5
004584 :  ,ITS,14
004585 :  ,XTS,=1
004586 :  ,CALL,PRINT8
004587 :  ,WTC,RETLIB
004588 :  ,UJ,
004589 :  PY:,ISO,18HMAЛO MБ ДЛЯ COMMON
004590 :  '---------------------------'
004591 : C-----OШИБKИ B CM --------
004592 :  KAPUT1:,BSS,
004593 :  14,VTM,1-5
004594 :  *101:14,ITS,12
004595 :  14,VLM,*101
004596 :  ,XTA,ERR
004597 :  ,UIA,NOHEAD
004598 :  14,VTM,HEAD
004599 :  ,ITS,14
004600 :  14,UTM,3
004601 :  ,ITS,14
004602 :  14,VTM,22B
004603 :  ,ITS,14
004604 :  ,CALL,PRINT8
004605 :  NOHEAD:,BSS,
004606 :  2,UTC,CATLG-2
004607 :  ,XTS,
004608 :  14,VTM,26
004609 :  ,ITS,14
004610 :  ,ATX,ERR
004611 :   ,CALL,PT6/G
004612 :  14,VTM,1-5
004613 :  13,VTM,
004614 :  OO:13,STI,12
004615 :  13,UTM,-1
004616 :  14,VLM,OO
004617 :  ,UJ,CLEEN
004618 :  RET13:,BSS,1
004619 :  RKWC:,014,RBL
004620 :  ,,
004621 :  RKW:,BSS,1
004622 :  IRKW:,014,WBL
004623 :  ,021,
004624 :  CKW:,010,OCL
004625 :  ,021,
004626 :  ENDCAT:,LOG,7777 7777 7777 7777
004627 :  IFENTRY:,OCT,777 777
004628 :  PERSPL:,OCT,201042
004629 :  PYMЛ:,ISO,7HMAЛO MЛ
004630 :  CPRLOC:,OCT,201051
004631 :  CONST:,LOG,7777 7777 7777 0037
004632 :  PEЗ:,ISO,12HPEЗEPB
004633 :  MAS:,ISO,6HY  000
004634 :  ДЛИHA:,ISO,24HДЛИHA LIBRARY      ЗOH
004635 :  BADL:,ISO,35HИCПOPЧEH KATAЛOГ TEMPORARY LYBRARY
004636 :  HEAD:,ISO,24HИCПOPЧEHЫ ПOДПPOГPAMMЫ:
004637 :  TE:,ISO,12HLIBRARY OT  00.00.70
004638 :  TXT*ARR:,TEXT,8H*ORDER
004639 :  TXTM:,TEXT,8HMONITOR*
004640 :  /1TP:,014,WBL
004641 :  ,021,1
004642 :  NCLST:,,NCL
004643 :   ,,
004644 :  SAVD:8,010,WBL+1
004645 :  ,,201B
004646 :  SAVD1:8,010,WBL+3
004647 :  ,,301B
004648 :  LST:,,WBL
004649 :  ,,
004650 :  WC:,BSS,3
004651 :  ROW:,BSS,1
004652 :  RETLIB:,BSS,1
004653 :  BOUND:,BSS,1
004654 :  FL:,BSS,1
004655 :  FLAGCOM:,BSS,1
004656 :  AFZ:,BSS,1
004657 :  WKW:,BSS,1
004658 :  NCKW:,BSS,1
004659 :  PRLOC:,BSS,1
004660 :  PLACE:,BSS,1
004661 :  LLENG:,BSS,1
004662 :  ERR:,BSS,1
004663 :  ПAPOЛЬ:,BSS,1
004664 :  /1WKW:,BSS,1
004665 :  BEG:,BSS,1
004666 :  INF:,BSS,1
004667 :  ,END,
004668 :  LLPAGE:,NAME,
004669 :  B:8,BASE,*
004670 :  ,STX,SERV
004671 :  ,ATX,CARD
004672 :  ,ITA,13
004673 :  ,XTS,BEG
004674 :  ,U1A,END
004675 :  ,XTS,CARD
004676 :  ,CALL,CHKBEG*
004677 :  8,VTM,B
004678 :  14,VZM,NOBEG
004679 :  ,XTA,=1
004680 :  ,ATX,BEG
004681 :  ,UTC,2
004682 :  ,CALL,PRINT8//
004683 :  8,VTM,B
004684 :  NOBEG:,XTA,CARD
004685 :  ,WTC,SERV
004686 :  13,VJM,
004687 :  ,STI,13
004688 :  13,UJ,
004689 :  END:,XTS,CARD
004690 :  ,CALL,CHKEND*
004691 :  8,VTM,B
004692 :  14,VZM,NOBEG
004693 :  ,XTA,
004694 :  ,ATX,BEG
004695 :  ,XTA,CARD
004696 :  ,WTC,SERV
004697 :  13,VJM,
004698 :  ,UTC,2
004699 :  ,CALL,PRINT8//
004700 :  ,STI,13
004701 :  13,UJ,
004702 :  BEG:,BSS,1
004703 :  SERV:,BSS,1
004704 :  CARD:,BSS,1
004705 :  ,END,
004706 :  LOADANGO:,NAME,. 10.04.73
004707 :  CECENTRY:,EQU,15B
004708 :  LNGO:,BSS,
004709 :  INFLOAD*:LC,BLOCK,ALOAD,INFLOA,INFMD,INFDM
004710 :  ,ATX,NAME
004711 :  ,XTA,17B
004712 :  ,ATI,14
004713 :  14,ATX,2
004714 :  ,ATX,AP
004715 :  ,ARX,C10
004716 :  ,ATX,17B
004717 :  ,ITA,13
004718 :  14,ATX,3
004719 :  14,XTA,
004720 :  ,AEX,NAME
004721 :  ,UZA,INMEM
004722 :  ,XTA,NAME
004723 :  14,ATX,
004724 :  ,ITA,14
004725 :  ,ASN,64-24
004726 :  ,AOX,CRETN
004727 :  14,UTC,
004728 :  ,CTX,4
004729 :  ,*70,46B
004730 :  ,XTA,ALOAD
004731 :  ,U1A,CONT
004732 :  ,*70,61B
004733 :  ,XTA,NAME
004734 :  ,WTC,43B
004735 :  13,VJM,2
004736 :   GOAFLOAD:,BSS,.TOЧKA ДЛЯ EXECUT
004737 :  FI:,WTC,AP
004738 :  14,VTM,
004739 :  GO :14,UTC,4
004740 :  13,VTM,
004741 :  14,WTC,7
004742 :  ,UJ,
004743 :  CONT:,*70,INFMD
004744 :  ,*70,INFLOA
004745 :  13,VTM,CONT1
004746 :  ,XTA,NAME
004747 :  ,WTC,ALOAD
004748 :  ,UJ,
004749 :  CONT1:,*70,INFDM
004750 :  ,STX,
004751 :  ,UJ,FI
004752 :  RETN:14,XTS,1
004753 :  ,ATX,16B
004754 :  14,XTA,2
004755 :  ,ATX,17B
004756 :  14,XTA,3
004757 :  ,STI,13
004758 :  13,UJ,
004759 :  INMEM:14,XTA,5
004760 :  ,ATX,16B
004761 :  14,XTA,6
004762 :  ,STX,17B
004763 :  ,UJ,GO
004764 :  CRETN:14,VTM,
004765 :  ,UJ,RETN
004766 :  LOADGO:,ENTRY,
004767 :  ,ATI,14
004768 :  14,XTA,
004769 :  ,APX,CPACK
004770 :  ,UJ,LNGO
004771 :  LOADCALL:,ENTRY,
004772 :  ,XTA,
004773 :  ,WTC,17B
004774 :  ,STX,
004775 :  ,UJ,LNGO
004776 :  C10:,LOG,10
004777 :  CPACK:,OCT,2765 7537 2765 7537
004778 :  NAME:,OCT,
004779 :  AP:,OCT,
004780 :  ,END,
004781 :   MANTO*:,NAME,
004782 : C
004783 : C...   ПEPEBOД HOMEPA БOБИHЫ ИЗ ISO
004784 : C      B  "XИTPЫЙ" KOД.
004785 : C
004786 :   ,UTC,=6H'17''17''17''17'''''
004787 :   ,APX,
004788 :   ,ASN,64+32
004789 :  DISP: 1,IFSEQ,D,DD**
004790 :   ,FUN,203B
004791 :   14,VTM,
004792 :   13,UJ,
004793 : C
004794 : C...   OБPATHAЯ ПPOЦEДYPA
004795 : C
004796 :   TOMAN*:,ENTRY,
004797 :  DISP: 1,IFSEQ,D,DD**
004798 :   ,FUN,202B
004799 :   ,ASN,64-16
004800 :   13,UJ,
004801 :   ,END,
004802 :  MONITOR*:,NAME,. 10.04.73
004803 :  MONCARD/:LC,BLOCK,MCRD(14),KK(0)
004804 :  SAV/47:,LC,506B-KK
004805 : C SAV/47 : I,BLOCK,         .ДЛЯ BEPETEHOBA
004806 : C           , CONT,SAV47    .CПACEHИE 47 ЯЧEЙKИ
004807 : C           , CONT,JTIME    .ДЛЯ OTMETOK BPEMEHИ
004808 :   /VERE/:LC,BLOCK,SAVL,S/CALL,STALOA*(3)
004809 :  DATA*:,LC,2
004810 :  STATIC*:,LC,1
004811 :  LHEAP*:,LC,1
004812 :  A/I0:,LC,1
004813 :  A/MINUS:,LC,1
004814 :  CHECKFL*:,LC,1
004815 :  FLAGS:,LC,1
004816 :        *REGIM//:,LC,1
004817 :  LOADANGO:,SUBP,
004818 :  LOADCALL:,SUBP,
004819 :  MAIN*:,LC,1
004820 :   MONCARD*:,LC,25
004821 :  ENTRY*:,LC,42
004822 :  PUNCHFL*:,LC,1
004823 :  LOADLIS*:,LC,1
004824 :  FLGEX*:,LC,1
004825 :  RFMRTFLG:,LC,1
004826 :  FULISFL*:,LC,1
004827 :  NOLISFL*:,LC,1
004828 :  CFIRST*:,LC,1
004829 :  TRNAME*:,LC,1
004830 :  ERROFLG*:,LC,1
004831 :  RM*:,LC,5
004832 :  RM:,EQU,RM*
004833 :  INFLOAD*:LC,BLOCK,ALOAD,INFLOA,INFMD,INFDM
004834 :   WORINF* :,LC,1
004835 :   KCOUNT*:,LC,4
004836 :   TAPELIST:,LC,25
004837 :   IOLIST* :,LC,5
004838 :   ACC*    :,LC,1
004839 :   ARREAD* :,LC,2
004840 :  SYS P:,WEQ,43B
004841 :  NSP:,P/P,(SYSP)16
004842 :  ENTMON:,UJ,MONIT
004843 :  ,,INFLOAD*
004844 :  EN STAR:12,VTM,
004845 :  ,UJ,RESET
004846 :  EN TRAN:12,VTM,1
004847 :  ,UJ,RESET
004848 :  EN EXEC:,CALL,LOADCALL
004849 :  ,SJ,
004850 :  MONIT:,XTA,16B
004851 :  ,ATX,RM
004852 :  ,XTA,17B
004853 :  ,ATX,RM+1
004854 :  ,ITA,15
004855 :  ,ATX,RM+2
004856 :  ,XTA,44B
004857 :  ,ATX,RM+3
004858 :  ,XTA,47B
004859 :  ,ATX,RM+4
004860 :  ,XTA,INFLOA
004861 :  ,ATX,STATIC*
004862 :  12,VTM,2
004863 :  RESET:,XTA,RM
004864 :  ,ATX,16B
004865 :  ,XTA,RM+1
004866 :  ,ATX,17B
004867 :  ,XTA,RM+2
004868 :  ,ATI,15
004869 :  ,XTA,
004870 :  ,ATX,ALOAD
004871 :  ,XTS,/MONTRAN
004872 :  ,CALL,LOADANGO
004873 :  /MONTRAN:,TEXT,8H/MONTRAN
004874 :  ,DATA,
004875 :   *LHEAP0:,OCT,0000 0377 0000 0400
004876 :  A/I00:,OCT,6400 0000 0000 0000
004877 :  A/MINUS0:,OCT,0020 0000 0000 0000
004878 :  OVE:,BSS,
004879 :  ,OCT,4000 2503 0000 0001
004880 :  ,OCT,4010 2503 0000 0001
004881 :   KC0:,BSS,
004882 :   ,LOG,
004883 :   ,LOG,0010 0000 0000 7777
004884 :   ,LOG,1
004885 :  ,LOG,76 000
004886 :   TRN :,TEXT,8HFORTRAN
004887 :   MAIN:,TEXT,8HPROGRAM
004888 :   I/O:,BSS,
004889 :   ,INT,50
004890 :   ,INT,51
004891 :   ,INT,52
004892 :   ,INT,53
004893 :   ,INT,54
004894 :   F/REG:,LOG,22
004895 :   1,SET,F/REG
004896 :   1,,FLAGS
004897 :   5,SET,I/O
004898 :   1,,IOLIST*
004899 :   1,SET,MAIN
004900 :   1,,MAIN*
004901 :   1,SET,TRN
004902 :   1,,TRNAME*
004903 :   1,SET,KC0+1
004904 :   1,,WORINF*
004905 :   4,SET,KC0
004906 :   1,,KCOUNT*
004907 :  2,SET,OVE
004908 :  1,,INFMD
004909 :  1,SET,*LHEAP0
004910 :  1,,LHEAP*
004911 :  1,SET,A/I00
004912 :  1,,A/I0
004913 :  1,SET,A/MINUS0
004914 :  1,,A/MINUS
004915 :  ,END,
004916 :          :  ,SUBP,MONREAD*
004917 :          :,ARRAY,RFMRTFLG                 .
004918 :           ,LC   ,1                        .
004919 :          :,SUBP ,STOP*                    .
004920 :           ,REG  ,                         .
004921 :          :,ARRAY,MONCARD*                 .
004922 :           ,LC   ,25                       .
004923 :          :,SUBP ,PRINT8                   .
004924 :           ,REG  ,                         .
004925 :          :,ARRAY,KCOUNT*                  .
004926 :           ,LC   ,4                        .
004927 :   :,ARRAY,*REGIM//
004928 :   ,LC,1
004929 :   14,VTM,77764B
004930 :   SAV:14,ITS,13
004931 :   14,VLM,SAV
004932 :  C       : 8,VTM,
004933 :            7,VTM,
004934 :           10,VTM,KCOUNT*                  .
004935 :           11,VTM,C                        .
004936 :    ,XTS,43B
004937 :             ,ASN,64-20                    .
004938 :           11,AOX,CDRUM-C                  .
004939 :           11,ATX,MDRUM-C                  .
004940 :           11,AOX,CREAD-C                  .
004941 :           11,ATX,MDRUMR-C                 .
004942 :   6,VTM,KEFTEXT
004943 :    ,XTA,RFMRTFLG
004944 :             ,UZA,SKSV                     .
004945 :           11,ARX,MDRUM-C                  .
004946 :           11,ATX,AEX-C                    .
004947 :           11,070,AEX-C                    .
004948 :          :11,070,MDRUMR-C                 .
004949 :          :  ,XTA,
004950 :             ,ATX,RFMRTFLG
004951 :  SKSV    :10,XTA,                         .
004952 :             ,ATI,9                        .COUNTER FOR STRINGS OF BUFF
004953 :           11,VTM,C                        .
004954 :  KLOOP   : 9,V1M,CHEKNEW                  .IF BUFF IS NOT EMPTY
004955 :          :10,XTA,1                        .
004956 :             ,CALL,DRUMTAP*
004957 :           10,ATX,1                        .INCREASE N OF THE ZONE
004958 :            9,VTM,-1024                    .RESET KCOUNT
004959 :             ,UJ ,*+2                      .
004960 :  CHEKNEW :10,XTA,2                        .
004961 :           ,UZA,KREDWORD                   .IF NOT FIRST ENTRY IN KREAD
004962 :          :  ,ITA,                         .
004963 :           10,ATX,2                        .
004964 :           10,XTA,3                        .
004965 :             ,ASN,64-20                    .
004966 :           10,ARX,1                        .
004967 :           11,ATX,INF-C                    .SET NUMER OF BUFF-LIST TO
004968 :           11,070,INF-C                    .1NF AND READ ZONE
004969 :  KREDWORD:10,WTC,3                        .
004970 :            9,XTA,1024                     .
004971 :            8,ATX,MONCARD*
004972 :            8,VZM,CHEKCARD
004973 :  CHEKE   : 8,UTM,1
004974 :    8,MTJ,14
004975 :     14,UTM,-33B
004976 :    14,VZM,RDE1
004977 :            9,UTM,1
004978 :            7,V1M,CHEKLOOP
004979 :    14,UTM,11
004980 :      14,VZM,READ*ERR
004981 :            8,XTA,MONCARD*-1               .
004982 :           11,AAX,CEMASK-C                 .
004983 :           11,AEX,CESTR-C                  .
004984 :             ,U1A,KLOOP                    .IF NOT END OF STRING
004985 :  14,VTM,MONCARD*
004986 :  ,ITS,14
004987 :  ,CALL,COSYTISO
004988 :  RETURN  :  ,ITA,9
004989 :           10,ATX,                         .KCOUNT
004990 :             ,STI,                         .
004991 :    14,VTM,13
004992 :     REST:14,STI,
004993 :     14,UTM,-1
004994 :    14,V1M,REST
004995 :           13,UJ ,                         .RETURN FROM KREAD. BELOW K47A
004996 :  CHEKCARD:  ,XTA,MONCARD*
004997 :           11,AEX,KENDFILE-C               .
004998 :             ,UZA,ERROR                    .IF END OF FILE
004999 :            8,XTA,MONCARD*
005000 :           11,AAX,COVMASK-C                .
005001 :           11,AEX,COVMASK-C                .
005002 :             ,UZA,KLOOP+1                  .IF END OF INFORM.ON ZONE
005003 :            8,XTA,MONCARD*
005004 :           11,AAX,COVMASK-C
005005 :             ,U1A,CHEKE
005006 :            7,VTM,1
005007 :             ,WTC,MONCARD*
005008 :           12,VTM,-1
005009 :             , UJ,CHEKE
005010 :  CHEKLOOP:12,VLM,KLOOP
005011 :             , UJ,RETURN
005012 :      ERROR:,ITS,6
005013 :     6,UTM,1
005014 :     ,ITS,6
005015 :     ,ITS,
005016 :     13,VTM,STOP*
005017 :     ,UJ,PRINT8
005018 :      READ*ERR:,ENTRY,
005019 :      READ*ERR:6,VTM,TNTC
005020 :   ,WTC,*REGIM//
005021 :     14,VTM,
005022 :    14,VZM,ERROR
005023 :     13,VTM,ERROR
005024 :     14,UJ,
005025 :      RDE1:6,VTM,TBMC
005026 :     ,UJ,ERROR
005027 :  'CONST'
005028 :  INF     :  ,OCT,                         .
005029 :  KENDFILE:  ,OCT,7760 0000 0000 0012      .
005030 :  CEMASK  :  ,OCT,0000 0000 0000 0377      .
005031 :  CESTR   :  ,OCT,0000 0000 0000 0012      .
005032 :  COVMASK :  ,OCT,7760 0000 0000 0000      .
005033 :  KEFTEXT:,ISO,12HNO *END FILE
005034 :   TNTC:,ISO,12HNONTEXT CARD
005035 :    TBMC:,ISO,12HBAD MON CARD
005036 :  CDRUM   :  ,OCT,0000 0000 0007 0000      .
005037 :  CREAD   :  ,OCT,0010 0000 0000 0000      .
005038 :  MDRUM   :  ,OCT,                         .
005039 :  MDRUMR  :  ,OCT,                         .
005040 :  AEX     :  ,OCT,                         .
005041 :  ,END,
005042 :  OCTTDEC :  ,NAME,
005043 :  C       :  ,BSS,
005044 :  BEGIN   :12,VTM,C                        .
005045 :             ,XTS,                         .
005046 :           12,STX,DEC-C                    .
005047 :           12,AAX,=777 7777-C
005048 :           12,AOX,INT0-C                   .
005049 :           12,ATX,OCT-C                    .
005050 :           14,VTM,-5
005051 :  CYCL    :  ,NTR,                         .
005052 :           12,XTA,OCT-C                    .
005053 :           14,UTC,C1                       .
005054 :             ,A/X,                         .
005055 :             ,NTR,3                        .
005056 :           12,A+X,INT0-C                   .
005057 :           15,ATX,                         .
005058 :             ,NTR,2                        .
005059 :           14,UTC,C1                       .
005060 :             ,A*X,                         .
005061 :           12,X-A,OCT-C                    .
005062 :           12,STX,OCT-C                    .
005063 :           12,AAX,MASK15-C                 .
005064 :           12,AOX,DEC-C                    .
005065 :           14,VLM,/6SYMB
005066 :           12,AOX,ZERO-C                   .
005067 :             ,NTR,10                       .
005068 :           13, UJ,                         .
005069 :  /6SYMB  :  ,BSS,
005070 :             ,ASN,64-8                     .
005071 :           12,ATX,DEC-C                    .
005072 :             , UJ,CYCL
005073 :  OCT     :  ,OCT  ,                       .
005074 :  MASK15  :  ,OCT  ,0000000000077777       .
005075 :  INT0    :  ,INT  ,0
005076 :  DEC     :  ,OCT  ,                       .
005077 :          :  ,REAL,100000.0
005078 :          :  ,REAL ,10000.0                .
005079 :          :  ,REAL ,1000.0                 .
005080 :          :  ,REAL ,100.0                  .
005081 :          :  ,REAL ,10.0                   .
005082 :  C1      :  ,REAL ,1.0                    .
005083 :  ZERO    :  ,8TEXT,''000000''             .
005084 :     ,END,
005085 :   PLOT:,NAME,
005086 :   PLOTBF:,SUBP,
005087 :   ,UJ,PLOTBF
005088 :   ,END,
005089 :   PLOTTER:,NAME,
005090 : C*
005091 : C****  26.9.83   A.П.CAПOЖHИKOB
005092 : C*
005093 : C*   ЗAKAЗ ГPAФOПOCTPOИTEЛЯ ПPИ
005094 : C*   PAБOTE C CИCTEMOЙ "ГPAФOP"
005095 : C*
005096 : C*   *CALL PLOTTER:CALCOMP
005097 : C*   *CALL PLOTTER:WX4675
005098 : C*   *CALL PLOTTER:TEKTRONIX
005099 : C*   *CALL PLOTTER:BUFFER
005100 : C*
005101 : C*   ПPOИЗBOДИTCЯ KOMMYTAЦИЯ ПPOГPAMMЫ
005102 : C*   PLOT B KATAЛOГE BPEMEHHOЙ БИБЛИOTEKИ
005103 : C*   HA ДPAЙBEP COOTBETCTBYЮЩEГO YCTP-BA.
005104 : C*   ДOЗAKAЗЫBAЮTCЯ  LIBRARY 5 И 23.
005105 : C*
005106 : C*   ГPAФИKA БYФEPИЗYETCЯ B CИCTEMHOM ФAЙЛE
005107 : C*   BЫBOДA, ECЛИ HE ЗAДAH BTOPOЙ ПAPAMETP
005108 : C*   DIRECT. ИMЯ ПЛOTTEPA ЗAПOMИHAETCЯ
005109 : C*   BMECTE C ГPAФИKOM.
005110 : C*
005111 : C*   ЗAДAHИE B KAЧECTBE "YCTPOЙCTBA"  BUFFER
005112 : C*   OЗHAЧAET, ЧTO HAM БEЗPAЗЛИЧHO, HA KAKOM
005113 : C*   ПЛOTTEPE ПPOИЗBOДИTЬ PAЗГPYЗKY ГPAФИKA.
005114 : C*
005115 : C*
005116 :   PRINT8:,SUBP,
005117 :   *MONCRD*:,LC,12
005118 :   LIST:,PC,2048
005119 :   N:,P/P,(LIST)16
005120 :   LIB:,EQU,73B
005121 :   /VERE/:LC,BLOCK,SAVL.ДYБЛЬ LIB
005122 : C---
005123 :   13,MTJ,1
005124 :   9,VTM,
005125 :  10,VTM,
005126 :   ,CALL,PARAMT*
005127 :   10,VTM,1
005128 :   ,CALL,PARAMT*
005129 : C---
005130 :   B:8,BASE,B
005131 :   2,VTM,TAB-1
005132 :   *1:2,UTM,1
005133 :   2,XTA,
005134 :   ,UZA,ERR1
005135 :   ,UTC,*MONCRD*
005136 :   ,AEX,
005137 :   ,U1A,*1
005138 :   9,VTM,LIST
005139 :   ,*70,R1
005140 :   10,VTM,LIST
005141 :   ,*70,R2
005142 : C---  ИЗЫMAEM CTAPЫЙ PLOT
005143 :   *2:9,UTM,2
005144 :   9,XTA,
005145 :   ,UZA,CHDIRECT
005146 :   ,AEX,PLOT
005147 :   ,U1A,*2
005148 :   9,MTJ,14
005149 :  *3:14,XTA,2
005150 :   14,ATX,
005151 :   ,AEX,=7777 7777 7777 7777
005152 :   14,UTM,1
005153 :   ,U1A,*3
005154 : C
005155 : C---   ПPOБA ПAPAMETPA DIRECT :
005156 : C      ECЛИ HET - ИCПOЛЬЗYEM PLOTBF
005157 : C
005158 :   CHDIRECT:,XTA,TABN+3
005159 :   ,ATX,NAME
005160 :   2,MTJ,3
005161 :   3,UTM,1-TAB
005162 :   11,UTM,-54B
005163 :   11,VIM,SETTYP
005164 :   10,VTM,1
005165 :   ,CALL,PARAMT*
005166 :   ,UTC,*MONCRD*
005167 :   ,XTA,
005168 :   ,AEX,=6HDIRECT
005169 :   ,U1A,SETTYP
005170 :   2,XTA,TABN-TAB
005171 :   ,ATX,NAME
005172 :   ,AEX,TABN+3
005173 :   ,UZA,ERR2
005174 :   3,VTM,
005175 :   SETTYP:,ITA,3
005176 :   ,ASN,64-39
005177 :   10,VTM,LIST
005178 :   ,FUN,74100B+571B
005179 : C---   ИЩEM CBOЙ ДPAЙBEP
005180 :   *4:10,UTM,2
005181 :  10,XTA,
005182 :   ,UZA,ERR3
005183 :   ,AEX,NAME
005184 :   ,U1A,*4
005185 :   14,VTM,LIST+2041
005186 : C---   BCTABЛЯEM  ENTRY PLOT
005187 :   *5:14,UTM,-1
005188 :   14,XTA,
005189 :   14,ATX,2
005190 :   ,ITA,14
005191 :   ,ITS,10
005192 :   15,AEX,
005193 :   ,U1A,*5
005194 :   10,ATX,3
005195 :   ,XTA,PLOT
005196 :   10,ATX,2
005197 :   10,XTA,1
005198 :   ,ASN,64+15
005199 :   ,UZA,ERR2
005200 :   ,*70,W1
005201 :   9,VTM,-1
005202 :   ,*70,W2
005203 : C---  ДOЗAKAЗ LIB:5,23
005204 :   *6:,XTA,LIB
005205 :   14,VTM,-7
005206 :   *7:,ASN,64-6
005207 :   15,ATX,2
005208 :   ,YTA,
005209 :   9,AEX,NUM+1
005210 :   ,UZA,*8
005211 :   15,XTA,2
005212 :   14,VLM,*7
005213 :   ,XTA,LIB
005214 :   ,AAX,=:77
005215 :   ,U1A,ERR4
005216 :   ,XTA,LIB
005217 :   9,AEX,NUM+1
005218 :   ,AAX,=77
005219 :   ,AEX,LIB
005220 :   ,ASN,64-6
005221 :   ,AOX,=21
005222 :   ,ATX,LIB
005223 :   *8:9,VLM,*6
005224 :   ,XTA,
005225 :   ,ATX,SAVL
005226 :   1,UJ,
005227 :   R1:,010,N
005228 :   ,021,
005229 :   R2:,010,N+64
005230 :   ,021,1
005231 :   W1:,000,N
005232 :   ,021,
005233 :   W2:,000,N+64
005234 :   ,021,1
005235 :   ERR1:14,VTM,TERPAR
005236 :   ,UJ,ERROR
005237 :   ERR2:14,VTM,TERCAT
005238 :   ,UJ,ERROR
005239 :   ERR3:14,VTM,TNODRIV
005240 :   ,UJ,ERROR
005241 :   ERR4:14,VTM,TBADLIB
005242 :   ,UJ,ERROR
005243 :   ERROR:,ITS,14
005244 :   14,UTM,1
005245 :   ,ITS,14
005246 :   ,XTS,=15
005247 :   1,MTJ,13
005248 :   ,UJ,PRINT8
005249 :   TERPAR:,ISO,12HOШ.ПAPAMETP
005250 :   TERCAT:,ISO,12HБPEД COБAЧ.
005251 :  TNODRIV:,ISO,12HHET ДPAЙBEPA
005252 :  TBADLIB:,ISO,12HMHOГO OБCП
005253 :   NUM:,LOG,23
005254 :       ,LOG,5
005255 :   PLOT:,TEXT,8HPLOT
005256 :   TAB :,ISO,6HCALCOM
005257 :        ,ISO,6HWX4675
005258 :        ,ISO,6HTEKTRO
005259 :        ,ISO,6HBUFFER
005260 :   ,LOG,
005261 :   TABN:,TEXT,8HPLOTC
005262 :        ,TEXT,8HPLOTWX
005263 :        ,TEXT,8HPLOTTX
005264 :        ,TEXT,8HPLOTBF
005265 :   NAME:,BSS,1
005266 :   ,END,
005267 :  POWER:,NAME,.A**B
005268 :  A/I0:,LC,1.
005269 :  *C:,BSS,.
005270 :  ,AVX,.
005271 :  ,AOX,.
005272 :  ,UZA,R1.ПOKAЗATEЛЬ=0
005273 :  15,ATX,.
005274 :  ,NTR,3.
005275 :  ,A+X,A/I0.
005276 :  12,VTM,21B.
005277 :  12,ATX,
005278 :  ,NTR,6.
005279 :  ,E+N,64.
005280 :  15,AEX,-1.
005281 :  ,UZA,*14
005282 :  ,MTJ,12.
005283 :  *14:15,XTA,-2.OCHOBAHИE
005284 :  ,AVX,.
005285 :  ,UIA,ERR*.OTPИЦATEЛЬHOE
005286 :  ,MTJ,12.
005287 :  ,AOX,.
005288 :  ,UZA,ZERO.HYЛEBOE
005289 :  EXTR:,*55,.LN(A)
005290 :  15,A*X,.LN(A)*B
005291 :  ,*56,.A**B
005292 :  12,AVX,.
005293 :  RET:15,UTM,-1.
005294 :  13,UJ,.
005295 :  ERR*:12,VZM,ERR
005296 :  12,XTA,.
005297 :  ,ASN,64-40
005298 :  15,AAX,-2.
005299 :  12,ATX,.
005300 :  15,XTA,-2.
005301 :  15,AVX,-2.
005302 :  ,UJ,EXTR.
005303 :  ZERO:15,XTA,-1.
005304 :  ,AVX,.
005305 :  ,UIA,ERR.0**X,X<0
005306 :  ,XTA,.
005307 :  15,UTM,-2
005308 :  13,UJ,.
005309 :  R1:15,XTA,-1.
005310 :  ,E+N,64
005311 :  ,AOX,.
005312 :  ,UZA,ERR**.0**0
005313 :  ,UTC,=R1.0
005314 :  ,XTA,.
005315 :  ,UJ,RET.
005316 :  ERR**:15,ATX,.
005317 :  ERR  :,ITA,13.
005318 :  ,ITS,.
005319 :  13,BASE,*C.
005320 :  ,PRINT,I1.
005321 :  ,PRINT,I2.
005322 :  ,UJ,20B.ДЛЯ IFOVFL!
005323 :  I1:15,Z00,-3
005324 :  15,Z00,-1
005325 :  :3,,11
005326 :  ,021,1
005327 :  :2,042,5
005328 :  8,,
005329 :  I2:,,T1
005330 :  ,,T1
005331 :  :,016,
005332 :  8,,.
005333 :   T1:,GOST,8H'212'**'200''40'B'200''50'
005334 :  ,CONT,10HCTEПEHЬ '133''231'
005335 :  ,END,POWER   .
005336 :  PRINT8:,NAME,..19.04.73
005337 :  *7:,BLOCK,B,C,IR,IC,JC,IW,JW,RW
005338 :  BL:,EQU,CONT
005339 :  SC:,EQU,SYM
005340 :  ,ITS,B
005341 :  B,BASE,*
005342 :  ,NTR,3
005343 :  ,STX,SB
005344 :  ,AAX,=177
005345 :  ,ATX,POS
005346 :  ,ASN,64-36
005347 :  ,AOX,=4000 0000
005348 :  ,STX,INF+1
005349 :  ,ATX,R
005350 :  15,X-A,
005351 :  ,ATX,SC
005352 :  ,ITA,C
005353 :  C,VTM,IR-RW
005354 :  SV:C,ITS,RW
005355 :  C,VLM,SV
005356 :  ,XTS,SC
005357 :  ,ATI,C
005358 :  IW,VTM,
005359 :  15,MTJ,JW
005360 :  CC:,BSS,
005361 :  ,WTC,R
005362 :  C,XTA,
005363 :  ,ATX,W
005364 :  IR,VTM,-5
005365 :  CR:,XTA,W
005366 :  ,ASN,64-8
005367 :  ,ATX,W
005368 :  ,YTA,
005369 :  ,ATX,SYM
005370 :  ,UZA,ER
005371 :  ,AEX,=177
005372 :  ,UZA,ER
005373 :  ,XTA,SYM
005374 :  ,AAX,=200
005375 :  ,U1A,BLANK
005376 :  ,XTA,POS
005377 :  ,U1A,RCON
005378 :  JC,VTM,CONSYM1-CONSYM2
005379 :  C2:,BSS,
005380 :  ,UTC,CONSYM2
005381 :  JC,XTA,
005382 :  ,ATX,CONT
005383 :  IC,VTM,-2
005384 :  C1:,BSS,
005385 :  ,XTA,CONT
005386 :  ,ASN,64-8
005387 :  ,ATX,CONT
005388 :  ,YTA,
005389 :  ,AEX,SYM
005390 :  ,UZA,CONTR
005391 :  IC,VLM,C1
005392 :  JC,VLM,C2
005393 :  RCON:,BSS,
005394 :  ,XTA,SYM
005395 :  ,ASN,64+2
005396 :  15,ATX,
005397 :  ,YTA,
005398 :  ,ASN,64+43
005399 :  15,WTC,
005400 :  ,XTS,TABL
005401 :  15,WTC,
005402 :  ,ASN,64+16
005403 :  ,AAX,=377
005404 :  ,ATX,SYM
005405 :  ,AEX,=231
005406 :  ,UZA,PRINT
005407 :  ,XTA,SYM
005408 :  ,ASN,64+7
005409 :  ,AEX,=1
005410 :  RW,VTM,ER
005411 :  ,U1A,POSITION
005412 :  ,XTA,SYM
005413 :  ,UJ,WRT
005414 :  CONTR:,BSS,
005415 :  ,XTA,CONT
005416 :  ,ASN,64+24
005417 :  ,AAX,=377
005418 :  RW,VJM,WRT
005419 :  ,XTA,=017
005420 :  ,ATX,SYM
005421 :  ,XTA,=1
005422 :  RW,VJM,POSITION
005423 :  ER:,BSS,
005424 :  IR,VLM,CR
005425 :  C,VLM,CC
005426 :  PRINT:,BSS,
005427 :  ,XTA,=231
005428 :  RW,VJM,WRT
005429 :  ,PRINT,INF
005430 :  JW,MTJ,15
005431 :  ,NTR,18
005432 :  C,VTM,RW-C
005433 :  15,XTA,
005434 :  UP:C,UTM,-1
005435 :  C,STI,IR
005436 :  C,V1M,UP
005437 :  ,ATI,C
005438 :  ,XTA,SB
005439 :  ,STI,B
005440 :  13,UJ,
005441 :  BLANK:,BSS,
005442 :  ,XTA,SYM
005443 :  ,AEX,=200
005444 :  ,UZA,ER
005445 :  ,ATX,BL
005446 :  ,XTA,=200
005447 :  ,ATX,SYM
005448 :  ,XTA,BL
005449 :  RW,VJM,POSITION
005450 :  ,XTA,POS
005451 :  RW,VTM,ER
005452 :  ,UJ,WRT
005453 :  POSITION:,BSS,
005454 :  ,A+X,POS
005455 :  ,ATX,POS
005456 :  ,X-A,=200
005457 :  ,U1A,PRINT
005458 :  ,XTA,SYM
005459 :  WRT:,BSS,
005460 :  IW,V1M,EW
005461 :  IW,VTM,-48
005462 :  ,ATX,SYM
005463 :  ,XTA,
005464 :  ,XTS,SYM
005465 :  EW:,BSS,
005466 :  IW,UTM,8
005467 :  IW,ASN,64
005468 :  15,AOX,
005469 :  15,ATX,
005470 :  RW,UJ,
005471 :  INF:JW,Z00,
005472 :  15,Z00,-1
005473 :  ,BSS,1
005474 :  CONSYM1:,BSS,
005475 :  ,ISO,6H014'214''201''204'
005476 :  ,ISO,6H56+'205''206''212'
005477 :  CONSYM2:,BSS,
005478 :  ,ISO,4HS00'310'
005479 :  TABL:,GOST,4H'231''212''133''133'
005480 :  ,GOST,4H'133''024''135''133'
005481 :  ,GOST,4H'133''231''133''310'
005482 :  ,GOST,4H'117''116''231''214'
005483 :  ,GOST,4H'205''204''201''032'
005484 :  ,GOST,4H'020''021''131''206'
005485 :  ,GOST,4H'033''124''136''034'
005486 :  ,GOST,4H'123''120''125''122'
005487 :  ,GOST,4H'034''134''133'
005488 :  ,GOST,4H'137''121''126''127'
005489 :  ,GOST,4H+*)(
005490 :  ,GOST,4H/.-,
005491 :  ,GOST,4H3210
005492 :  ,GOST,4H7654
005493 :  ,GOST,4H'026':98
005494 :  ,GOST,4H'21''036'='035'
005495 :  ,GOST,4HCBA'136'
005496 :  ,GOST,4HGFED
005497 :  ,GOST,4HKJIH
005498 :  ,GOST,4HONML
005499 :  ,GOST,4HSRQP
005500 :  ,GOST,4HWVUT
005501 :  ,GOST,4H'027'ZYX
005502 :  ,GOST,4H'132''130''030''115'
005503 :  ,GOST,4HЦБAЮ
005504 :  ,GOST,4HГФEД
005505 :  ,GOST,4HKЙИX
005506 :  ,GOST,4HOHMЛ
005507 :  ,GOST,4HCPЯП
005508 :  ,GOST,4HBЖYT
005509 :  ,GOST,4HШЗЫЬ
005510 :  ,GOST,4H'133'ЧЩЭ
005511 :  SYM:,BSS,1
005512 :  POS:,BSS,1
005513 :  W:,BSS,1
005514 :  CONT:,BSS,1
005515 :  R:,BSS,1
005516 :  SB:,BSS,1
005517 :  ,END,
005518 :       PRINT8*     :,NAME,
005519 :       PRINT8      :,SUBP,
005520 :                         ,ITS ,12
005521 :                         ,ITS ,3
005522 :                         ,ITS ,4
005523 :                         ,ITS ,1
005524 :                         ,ITS ,2
005525 :                         ,ITS ,13
005526 :                   15    ,XTS ,77770B
005527 :                         ,ATI ,1
005528 :                   15    ,XTA ,77771B
005529 :                         ,ATI ,14
005530 :                   15    ,MTJ ,12
005531 :                   15    ,MTJ ,3
005532 :                         ,XTA ,
005533 :                   3     ,ATX ,
005534 :                   15    ,UTM, 00026B
005535 :                   4     ,VTM, 77730B
005536 :                   2     ,VTM, 00050B
005537 :                   13    ,VJM,      *00021B
005538 :                         ,ITS ,12
005539 :                         ,ITS ,3
005540 :                   14    ,VTM, 00001B
005541 :                         ,ITS ,14
005542 :                   13    ,VJM, PRINT8
005543 :                   12    ,MTJ ,15
005544 :                   15    ,XTA ,
005545 :                         ,STI ,13
005546 :                         ,STI ,2
005547 :                         ,STI ,1
005548 :                         ,STI ,4
005549 :                         ,STI ,3
005550 :                         ,STI ,12
005551 :                   15    ,UTM, 77776B
005552 :                   13    ,UJ,  00000B
005553 :      *00021B:     1     ,XTA ,
005554 :                   2     ,ASN ,24
005555 :                         ,UTC,      *00045B
005556 :                         ,AAX ,
005557 :                   2     ,V1M,      *00025B
005558 :                   1     ,UTM, 00001B
005559 :                   2     ,VTM, 00060B
005560 :                         ,UTC, 00000B
005561 :      *00025B:     2     ,UTM, 77770B
005562 :                   13    ,UZA, 00000B
005563 :                         ,NTR ,10
005564 :                         ,UZA,      *00034B
005565 :                   4     ,ASN ,104
005566 :                   3     ,AOX ,
005567 :                   3     ,ATX ,
005568 :                   4     ,V1M,      *00033B
005569 :                   3     ,UTM, 00001B
005570 :                         ,XTA ,
005571 :                   3     ,ATX ,
005572 :                   4     ,VTM, 77720B
005573 :      *00033B:     4     ,UTM, 00010B
005574 :                         ,UJ,       *00021B
005575 :      *00034B:           ,ITS ,1
005576 :                         ,ITS ,2
005577 :                         ,ITS ,13
005578 :                   15    ,XTS ,77774B
005579 :                         ,ASN ,104
005580 :                         ,ATI ,1
005581 :                   14    ,UTC, 00000B
005582 :                   1     ,UTC, 77600B
005583 :                   1     ,VTM, 00000B
005584 :                   2     ,VTM, 00050B
005585 :                   13    ,VJM,      *00021B
005586 :                   15    ,XTA ,
005587 :                         ,STI ,13
005588 :                         ,STI ,2
005589 :                         ,STI ,1
005590 :                         ,UJ,       *00021B
005591 :                         ,UTC, 00000B
005592 : CONST
005593 :      *00045B:,OCT,7760  0000  0000  0000
005594 :                   ,END,
005595 :  PRINT8//:,NAME,. Ф И A Э
005596 :  *REGIM//:LC,BLOCK,REGIM
005597 :  *4:,BLOCK,.       .CTAЛA OЧEHЬ
005598 :  ,CONT,B       .XOPOШA
005599 :  ,CONT,M
005600 :  ,CONT,LR
005601 :  ,CONT,LW
005602 :  ,CONT,RW
005603 :  ,CONT,RS
005604 :  ,CONT,WS
005605 :  ,CONT,WW
005606 :  ,CONT,SYM
005607 :  ,CONT,RN
005608 :  ,CONT,MV.
005609 : C**************************************
005610 : C    OPГAHИЗAЦИЯ ПEЧATИ "БИЛИCTOM".
005611 : C    ДOПOЛHИTEЛЬHЫE BXOДЫ БEЗ ПAPAMET-
005612 : C    POB:
005613 : C    PRINT8//+1  -  ДOПEЧATKA БYФEPA,
005614 : C    PRINT8//+2  -  HOBAЯ CTPAHИЦA,
005615 : C    PRINT8//+3  -  HOBЫЙ ЛИCT AЦПY,
005616 : C
005617 : C    BЫЗOB ПO OCHOBHOMY BXOДY:
005618 : C         14, VTM ,A HAЧAЛA
005619 : C           , ITS ,14      .
005620 : C         14, VTM ,A KOHЦA .
005621 : C           , ITS ,14      .
005622 : C         14, VTM ,N ПOЗИЦИИ.(ПPЯMO B AДPECE !)
005623 : C           , ITS ,14      .
005624 : C           , CALL,PRINT8//.
005625 : C
005626 : C
005627 : C
005628 : C    БAЙTЫ '200+N' B MACCИBE ПPИBOДЯT K
005629 : C         ПOЯBЛEHИЮ N ПPOБEЛOB HA ПEЧATИ,
005630 : C    БAЙTЫ '000','001','002','177' -
005631 : C       "OTCYTCTBИE KOДA"(ЗAПOЛHИTEЛИ),
005632 : C    БAЙTЫ '003','012' - ПPИЗHAKИ
005633 : C          KOHЦA TEKCTA.
005634 : C          ECЛИ HYЖHO OKOHЧИTЬ ПEЧATЬ
005635 : C          PAHЬШE, ЧEM ДOCTИГHETCЯ
005636 : C          A KOHЦA.
005637 : C    БAЙT  '007' - ПPИЗHAK ПOBTOPEHИЯ
005638 : C         ПPEДЫДYЩEГO CИMBOЛA.
005639 : C         KOЛИЧECTBO ПOBTOPEHИЙ (N-1!)
005640 : C         ЗAДAETCЯ B CЛEДYЮЩEM БAЙTE.
005641 : C
005642 : C    HAПPИMEP:
005643 : C         KOMБИHAЦИЯ  *'007'1
005644 : C         BЫЗOBET HA ПEЧATИ CTPOKY ИЗ
005645 : C         ПЯTИДECЯTИ (10) ЗBEЗДOЧEK.
005646 : C****************************************
005647 :  *C :,BSS,.COЧИHEHИE Б.Б.
005648 :  :B,BAS,*C.БAЗA
005649 :   :,UJ,PRINT.ПEЧATЬ
005650 :   ,Z00,LBUF .HACTPOЙKA
005651 :  :14,VTM,INIT.ДOCБPOC
005652 :  ,UJ,SAVE.
005653 :  :14,VTM,PAGE.HOBAЯ
005654 :  ,UJ,SAVE.CTPAHИЦA
005655 :  :14,VTM,LIST.HOBЫЙ
005656 :  ,UJ,SAVE.ЛИCT AЦПY
005657 :  :14,VTM,SHAKE.ДOCБPOC
005658 :  ,UJ,SAVE.SPEC
005659 :  INIT:B,UTC,4.
005660 :  13,VJM,.ДOПEЧATKA
005661 :  Э0000:,BSS,.ЭKOHOMИЯ
005662 :  ,XTA,LBUF+1.
005663 :  ,ATX,N STR.
005664 :  ,XTA,=1.
005665 :  ,ATX,P/F.
005666 :  ,ATX,FIRST
005667 :  ,XTA,
005668 :  ,ATX,/2.
005669 :  B,UJ,RETURN-*C.ПOЛOBИHЫ
005670 : C****************************************
005671 : C     HOBЫЙ ЛИCT AЦПY (ДЛЯ BOЛKOBA A.И.)
005672 : C****************************************
005673 :  LIST:B,UIA,PAGE-*C.MOHOЛИCT
005674 :  ,XTA,/2.
005675 :  ,ATI,M.
005676 :  13,VJM,*C+4.BEPETEHOB
005677 :  M,VIM,RETURN
005678 :  ,XTA,.
005679 :  ,ATX,/2.
005680 :  ,ATX,P/F
005681 :  ,XTA,L BUF.
005682 :  ,ATX,N STR.
005683 :  ,UJ,PAGE*.
005684 : C****************************************
005685 : C     HOBAЯ CTPAHИЦA
005686 : C****************************************
005687 :  PAGE:B,UZA,BIL-*C.БИЛИCT
005688 :  PAGE*:M,VTM,CNP.
005689 :  Э0001:,BSS,.
005690 :  ,PRINT,INF.
005691 :  B,UJ,RETURN-*C.MOHOЛИCT
005692 :  BIL:,BSS,.ИДET БИЛИCT
005693 :   ,XTA,N STR.
005694 :  B,WTC,P/F-*C.
005695 :  ,AEX,L BUF.YЖE И TAK
005696 :  B,UZA,RETURN-*C.HA HAЧAЛE
005697 :  BIL2:,XTA,N STR.
005698 :  14,VJM,MUI.
005699 :  ,XTA,=1.
005700 :  ,A-X,N STR.
005701 :  ,ATI,13.CЧETЧИK
005702 :  ,XTA,/2.
005703 :  B,UIA,SHAKE1-*C.2-Я ПOЛOBИHA
005704 :  L/M:14,VTM,-9.
005705 :  ,XTA,BLANK.
005706 :  M/14:M,UTC,.
005707 :  14,ATX,9.
005708 :  14,VLM,M/14.
005709 :  ,AOX,CONAOX
005710 :  ,AEX,CONAEX.
005711 :  M,ATX,10.KOHEЦ
005712 :  M,UTM,-11.
005713 :  13,VLM,L/M.POCПИCЬ '017'
005714 :   ,UJ,L/RET. !!!
005715 :  SHAKE1:M,XTA,10
005716 :  ,AOX,CONAOX+1.
005717 :  ,AEX,CONAEX+1.
005718 :  M,ATX,10.
005719 :  ,PRINT,INF
005720 :  M,UTM,-11.
005721 :  13,VLM,SHAKE1.ДOПEЧATKA
005722 : C                          .2-Й ПOЛOBИHЫ
005723 :  ,UJ,L/RET.
005724 : C****************************************
005725 : C     ИCЧИCЛEHИE БYФEPA ПEЧATИ
005726 : C****************************************
005727 :  MUI:,BSS,.
005728 :   ,A*X,K11.
005729 :  ,YTA,64.
005730 :  ,ATI,M
005731 :  M,UTM,BUF-11.
005732 :  14,UJ,.
005733 : C
005734 : C
005735 : C****************************************
005736 : C    ДOCБPOC БYФEPA ПEЧATИ
005737 : C****************************************
005738 :  SHAKE:,BSS,.
005739 :  B,UIA,RETURN-*C.HET БИЛИCTA
005740 :  ,XTA,/2.
005741 :  B,UIA,BIL2-*C.HE ПYCTO
005742 :  B,WTC,P/F-*C.
005743 :  14,VTM,L BUF.
005744 :  14,XTA,.
005745 :  ,AEX,N STR.
005746 :  B,UZA,RETURN-*C.ПYCTO
005747 :  14,XTA,.
005748 :  ,X-A,N STR.
005749 :  ,ATI,13.
005750 :  13,UTM,1.
005751 :  14,XTA,.
005752 :  14,VJM,MUI.
005753 :  B,UJ,SHAKE1-*C.
005754 :  SAVE:15,ATX,.
005755 :  ,RTE,177B.
005756 :  ,ITS,14.
005757 :  14,VTM,-10.
005758 :  S/I:14,ITS,14.
005759 :  14,VLM,S/I.
005760 :  ,NTR,3
005761 :  ,XTA,REGIM.
005762 :  ,ASN,64+47.
005763 :  B,VTM,*C.
005764 :  15,WTC,-11.
005765 :  ,UJ,.
005766 :  RETURN:14,VTM,10
005767 :  15,XTA,.
005768 :  R/I:14,STI,3
005769 :  14,UTM,-1.
005770 :  14,VIM,R/I.
005771 :  15,XTR,.
005772 :  15,XTA,.
005773 :  13,UJ,.
005774 :  GIVNS:,BSS,
005775 :  ,*65,766B
005776 :  ,AEX,=6HOC ДYБ
005777 :   ,U1A,NFF. - ДИCПAK
005778 :   ,*65,764B. BEPCИЯ DD
005779 :   ,NTR,6
005780 :   ,A-X,=R1.02
005781 :   ,U1A,NFF. - CTAPЫЙ DD
005782 :  ,*50,70100B+100B
005783 :  ,AAX,=10 0000
005784 :  ,U1A,NFF
005785 :  ,*50,70100B+126B
005786 :  ,NTR,6
005787 :  ,AOX,=I0
005788 :  ,A+X,=I0
005789 :  ,ATX,FIRST
005790 :  ,A/X,=R72.0
005791 :  ,NTR,7
005792 :  ,A+X,=I0
005793 :  ,NTR,6
005794 :  ,A*X,=R72.0
005795 :  ,NTR,7
005796 :  ,X-A,FIRST
005797 :  ,A-X,=I6
005798 :  :,UZA,*+1
005799 :   ,XTA,=I1
005800 :  :,X-A,=I66
005801 :  ,AAX,=377
005802 :  ,ATX,LBUF+1
005803 :  ,ATX,NSTR
005804 :  NFF:,XTA,
005805 :  ,ATX,FIRST
005806 :   ,NTR,7
005807 :  ,UJ,CPR
005808 : C*****************************************
005809 : C
005810 : C
005811 : C    OБPAБOTKA TEKCTOBOЙ CTPOKИ
005812 : C
005813 : C              * * *
005814 : C
005815 : C    ECЛИ N ПOЗИЦИИ TAKOЙ, ЧTO CAMЫЙ
005816 : C    ПEPBЫЙ CИMBOЛ OKAЗЫBAETCЯ ЗA EE
005817 : C   ПPEДEЛAMИ -  ПEЧATAETCЯ ПYCTAЯ
005818 : C   CTPOKA.
005819 : C    ECЛИ BECЬ TEKCT B OTBEДEHHOE
005820 : C    CTAHДAPTHO MECTO HE ЛEЗET,TO
005821 : C    ИЗЛИШKИ TEKCTA OБPEЗAЮTCЯ !!!
005822 : C
005823 : C
005824 : C****************************************
005825 :  PRINT:,BSS,..................
005826 :  ,ITS,13.
005827 :  13,VTM,ДOБOP.
005828 :  14,VJM,SAVE.
005829 :  ,ATI,RN
005830 :  ,XTA,FIRST
005831 :  ,U1A,GIVNS
005832 :  CPR:,BSS,
005833 :  RN,UTC,CONLS
005834 :   ,XTA,.
005835 :  15,A-X,-14.N ПOЗИЦ.
005836 :  ,UZA,P*1.XOPOШИЙ
005837 :  ,XTA,.
005838 :  P*1:,ATI,LW.
005839 :  RN,VIM,P*4
005840 :  ,XTA,NSTR.
005841 :  14,VJM,MUI.
005842 :  M,MTJ,13.
005843 :  ,XTA,/2.
005844 :  ,ATI,LR.
005845 :  LR,VZM,P*2.
005846 :  13,UTM,11.2-Я ПOЛOBИHA
005847 :  P*2:14,VTM,-10
005848 :  ,XTA,BLANK.
005849 :  P*3:14,UTC,10
005850 :  13,ATX,.
005851 :  14,VLM,P*3.
005852 :  13,XTA,10
005853 :  LR,UTC,CONAOX
005854 :  ,AOX,
005855 :  LR,UTC,CONAEX
005856 :  ,AEX,
005857 :  13,ATX,10
005858 :  ,UJ,P*6.
005859 :  P*4:14,VTM,-21
005860 :  ,XTA,BLANK
005861 :  P*5:14,UTC,21
005862 :  ,ATX,BUF
005863 :  14,VLM,P*5.
005864 :  ,AOX,CONAOX+1
005865 :  ,AEX,CONAEX+1
005866 :  ,ATX,BUF+21
005867 :  13,VTM,BUF.
005868 :  13,MTJ,M.
005869 :  P*6:LW,VZM,EOT.
005870 :  15,XTA,-14.N ПOЗ
005871 :  ,A*X,K1/6.
005872 :  15,ATX,.
005873 :  ,YTA,64.
005874 :  ,A*X,K8
005875 :  ,ATI,WS
005876 :  WS,UTM,-48
005877 :  15,WTC,.
005878 :  13,UTM,.
005879 :  15,XTA,-16.A HAЧ
005880 :  ,ATI,RW.
005881 :  15,X-A,-15.A KOH
005882 :  ,A*X,K6
005883 :  ,YTA,64.
005884 :  ,ATI,6.
005885 :  6,UTM,6
005886 :  ,MTJ,SYM.
005887 :  RS,VTM,48
005888 :  R*S:LW,VZM,EOT.
005889 :  12,VZM,R*1.
005890 :  12,UTM,-1.
005891 :  ,UJ,W*R.
005892 :  R*1:WW,VJM,R*2.
005893 :  12,VZM,R*1.
005894 :  ,AAX,=200
005895 :  ,UIA,R*4.
005896 :  ,ITA,SYM.
005897 :  ,A*X,K1/6.
005898 :  15,ATX,.
005899 :  ,YTA,64.
005900 :  ,A*X,K8.
005901 :  ,ATI,WW.
005902 :  15,WTC,.
005903 :  ,XTA,ГOCTЫ.
005904 :  11,ASN,64.
005905 :  ,AAX,=377
005906 :  ,ATX,GOST.
005907 :  ,AAX,=200
005908 :  ,UIA,R*5.CПEЦ KOД
005909 :   ,MTJ,SYM.
005910 :  W*R:,XTA,=377
005911 :   WS,UTM,8.
005912 :  WS,ASN,64.
005913 :  15,ATX,
005914 :  13,AOX,
005915 :  15,AEX,.
005916 :  ,XTS,GOST.
005917 :   ,ATX,OLDG.ДЛЯ БYДYЩEГO
005918 :  WS,ASN,64.
005919 :  15,AOX,.
005920 :  13,ATX,.
005921 :  WS,VIM,W*1.
005922 :  WS,VTM,-48
005923 :  13,UTM,1.
005924 :  W*1:LW,UTM,-1.
005925 :  ,UJ,R*S.
005926 :  R*2:LR,VZM,EOT.
005927 :     LR,UTM,-1.
005928 :  RS,UTM,-8
005929 :  8,XTA,.
005930 :  RS,ASN,64.
005931 :  ,AAX,=377.
005932 :  ,ATI,SYM.
005933 :  WW,UTC,
005934 :  RS,VIM,.
005935 :  RS,VTM,48.
005936 :  RW,UTM,1.
005937 :  WW,UJ,.
005938 :  R*4:,XTA,=17
005939 :  ,ATX,GOST
005940 :  12,UTM,-200B.
005941 :  ,UJ,R*S.
005942 :  R*5:,WTC,GOST.
005943 :  ,UJ,*-177B.
005944 :  :,UJ,R*1.ПYCTЫШKИ
005945 :  :,UJ,EOT.KOHЦЫ
005946 :  :,XTA,OLDG.
005947 :  ,ATX,GOST.
005948 :  WW,VTM,R*S.
005949 :  ,UJ,R*2.ПOBTOP
005950 :  EOT:,XTA,*REGIM//.
005951 :  ,ASN,64+47
005952 :  B,UIA,Э0001-*C.
005953 :  ,XTA,/2.
005954 :  ,UZA,NO*64.
005955 :  ,PRINT,INF.
005956 :  NO*64:,XTA,N STR
005957 :  ,ARX,=377
005958 :  ,AAX,=377
005959 :  ,ATX,N STR.
005960 :  B,UIA,RETURN-*C.
005961 :  L/RET:,BSS,. !!!
005962 :  ,XTA,/2.
005963 :  ,AEX,=1
005964 :  ,ATX,/2.
005965 :  ,U1A,L/RET2
005966 :  M,VTM,CNP
005967 :  ,PRINT,INF
005968 :  L/RET2:,BSS,
005969 :  ,WTC,P/F.
005970 :  ,XTA,L BUF.
005971 :  ,ATX,N STR.
005972 :  ,XTA,/2
005973 :  ,AAX,P/F.
005974 :  ,AEX,P/F.
005975 :  ,ATX,P/F.
005976 :  B,UJ,RETURN-*C.
005977 :  ДOБOP:,ATI,13.
005978 :  15,UTM,-3.
005979 :  15,XTA,.
005980 :  13,UJ,.
005981 :  INF:M,,.
005982 :  M,,.
005983 :  :,000,.
005984 :  8,,.
005985 :  FIRST:,LOG,1
005986 :  /2:,OCT,
005987 :  P/F:,LOG,1
005988 : C
005989 : C--- ЧИCЛO CTPOK B CTPAHИЦE ---
005990 : C
005991 :  PL:,EQU,66
005992 :  L BUF:,,
005993 :  ,Z00,PL
005994 :  ,,
005995 :  ,Z00,PL-1
005996 :  NSTR:,,
005997 :  ,Z00,PL-1
005998 :  CONLS:,LOG,77
005999 :       :,LOG,177
006000 :  CONAOX:,GOST,6H0000'377''377'
006001 :        :,GOST,6H000'377''377''377'
006002 :  CONAEX:,GOST,6H0000'177''276'
006003 :        :,GOST,6H000'146''360''360'
006004 :  K11  :,OCT,4000 0000 0000 0013
006005 :  K6   :,OCT,4000 0000 0000 0006
006006 :  K8   :,OCT,4000 0000 0000 0060
006007 :  K1/6 :,OCT,4002 5252 5252 5253
006008 :  BLANK:,GOST,6H
006009 :  CNP:,GOST,2H'201''231'
006010 :  ГOCTЫ:,GOST,6H'135''133''201''200''200''200'
006011 :  ,GOST,6H'133''201''133''133''202''024'
006012 :  ,CONT,6H'200''032''117''116''200''133'
006013 :  ,CONT,6H'020''021''131''200''200''200'
006014 :  ,CONT,6H'125''122''033''124''136''034'
006015 :  ,CONT,6H'034''134''133''017''123''120'
006016 :  ,CONT,6H)('137''121''126''127'
006017 :  ,CONT,6H/.-,+*
006018 :  ,CONT,6H543210
006019 :  ,CONT,6H'26':9876
006020 :  ,CONT,6HA'136''21''36'='35'
006021 :  ,CONT,6HGFEDCB
006022 :  ,CONT,6HMLKJIH
006023 :  ,CONT,6HSRQPON
006024 :  ,CONT,6HYXWVUT
006025 :  ,CONT,6H'132''130''30''115''27'Z
006026 :  ,CONT,6HEДЦБAЮ
006027 :  ,CONT,6HKЙИXГФ
006028 :  ,CONT,6HЯПOHMЛ
006029 :  ,CONT,6HBЖYTCP
006030 :  ,CONT,6HЩЭШЗЫЬ
006031 :  ,CONT,6H'200''200''200''200''200'Ч
006032 :  GOST:,BSS,1.
006033 :  OLDG:,BSS,1.
006034 :  BUF :,BSS,737
006035 :  ,END,PRINT8//.
006036 :   PRTLINE*:,NAME,
006037 :   PRTLINE/:,SUBP,
006038 :   PRINT8  :,SUBP,
006039 :   PRTADR/:,LC,1
006040 : C
006041 : C      ПOДПPOГPAMMA TPEБYETCЯ ПPИ CЧETE
006042 : C      B БИБЛИOTEKE ECTЬ П/П C TEM ЖE
006043 : C      ИMEHEM, HO BЫXOДЯЩAЯ HA PRINT8//
006044 : C      (TPEБYETCЯ ПPИ CБOPKE TPAHCЛЯTOPA)
006045 : C
006046 :   ,UTC,APRIN
006047 :   ,XTS,
006048 :   ,UTC,PRTADR/
006049 :   ,STX,
006050 :   ,UJ,PRTLINE/
006051 :   APRIN:,Z00,
006052 :   ,Z00,PRINT8
006053 :   ,END,
006054 :  PT6/G:,NAME,.CAXAPOB Б Б
006055 :  A/I0:,LC,1
006056 :  ,ITS,7
006057 :  7,BASE,INF
006058 :  ,STX,ИP7
006059 :  ,ATX,ARRAY.
006060 :  ,AAX,=77600.
006061 :  ,STX,FLAG*
006062 :  ,ATX,WORD.
006063 :  ,ITA,13
006064 :  ,ITS,6
006065 :  ,ITS,
006066 :  14,VTM,-1
006067 :  *C1:,XTA,
006068 :  14,UTC,BUF+1
006069 :  ,ATX,
006070 :  13,VTM,-48
006071 :  *C2:,XTA,WORD
006072 :  ,ASN,64-6
006073 :  ,ATX,WORD
006074 :  ,YTA,
006075 :   ,AOX,A/I0
006076 :  ,NTR,2
006077 :  ,E+N,64+3
006078 :  15,ATX,
006079 :  ,A/X,=R48.0
006080 :  ,NTR,3
006081 :   ,A+X,A/I0
006082 :  ,ATI,6
006083 :  ,NTR,2
006084 :  ,A*X,=R48.0
006085 :  15,A-X,
006086 :  ,NTR,3
006087 :   ,A+X,A/I0
006088 :  6,UTC,TABLE*6
006089 :  ,XTS,
006090 :  15,WTC,
006091 :  ,ASN,64
006092 :  ,AAX,=:776
006093 :  13,ASN,64+48
006094 :  14,UTC,BUF+1
006095 :  ,AOX,
006096 :  14,UTC,BUF+1
006097 :  ,ATX,
006098 :  13,UTM,8
006099 :  13,VIM,*C2
006100 :  14,VLM,*C1
006101 :  ,XTA,FLAG*
006102 :  ,UIA,TTTT
006103 :  ,XTA,ARRAY
006104 :  ,ASN,64-36
006105 :  ,AOX,=4000 0000
006106 :  ,ATX,INF+1
006107 :  ,PRINT,INF
006108 :  EXIT:,XTA,ИP7
006109 :  ,STI,7
006110 :  ,STI,6
006111 :  ,STI,13
006112 :  ,NTR,6
006113 :  13,UJ,
006114 :  TTTT:,XTA,BUF
006115 :  ,WTC,ARRAY
006116 :  ,ATX,
006117 :  ,XTA,BUF+1
006118 :  ,WTC,ARRAY
006119 :  ,ATX,+1
006120 :  ,UJ,EXIT
006121 :  INF:, ,BUF
006122 :  , ,BUF
006123 :  FLAG*:,OCT,
006124 :  TABLE*6:,GOST,20H $БЦДФГИ'16''126'*ЙЛЯЖ/0123
006125 :   ,CONT,16H456789Ь'37'П-+ЫЗABC
006126 :  ,CONT,20HDEFGHIJKLMNOPQRSTUVW
006127 :  ,CONT,8HXYZШЭЩЧЮ
006128 :  BUF:,GOST,13H000000000000'231'
006129 :  ИP7  :,BSS,1
006130 :  WORD :,BSS,1
006131 :  ARRAY:,BSS,1
006132 :  ,END,PT6/G
006133 :  PUNCTABL:  ,NAME,                        .
006134 :          :  ,OCT  ,
006135 :          :  ,OCT  ,
006136 :          :  ,OCT  ,
006137 :          :  ,OCT  ,
006138 :          :  ,OCT,0000 1774 1044 1044      .Б,1
006139 :          :  ,OCT,1044 1044 1044 0704      .Б,2
006140 :          :  ,OCT,0000 1774 1000 1000      .Ц,1
006141 :          :  ,OCT,1000 1000 1774 1000      .Ц,2
006142 :          :  ,OCT,1000 1740 1020 1010      .Д,1
006143 :          :  ,OCT,1004 1004 1004 1774      .Д,2
006144 :          :  ,OCT,0000 0360 0410 0410      .Ф,1
006145 :          :  ,OCT,1774 0410 0410 0360      .Ф.2
006146 :          :  ,OCT,0000 1774 0004 0004      .Г,1
006147 :          :  ,OCT,0004 0004 0004 0004      .Г,2
006148 :          :  ,OCT,0000 1774 0400 0200      .И,1
006149 :          :  ,OCT,0100 0040 0020 1774      .И,2
006150 :          :  ,OCT  ,
006151 :          :  ,OCT  ,
006152 :          :  ,OCT  ,
006153 :          :  ,OCT  ,
006154 :          :  ,OCT,0000 0000 0520 0340      .*,1
006155 :          :  ,OCT,0760 0340 0520 0000      .*,2
006156 :          :  ,OCT,0000 1774 0400 0204      .Й,1
006157 :          :  ,OCT,0104 0044 0020 1774      .Й,2
006158 :          :  ,OCT,0000 1740 0020 0010      .Л,1
006159 :          :  ,OCT,0004 0004 0004 1774      .Л,2
006160 :          :  ,OCT,0000 1070 0504 0304      .Я,1
006161 :          :  ,OCT,0104 0104 0104 1774      .Я,2
006162 :          :  ,OCT,0000 1014 0420 0240      .Ж,1
006163 :          :  ,OCT,1774 0240 0420 1014      .Ж,2
006164 :          :  ,OCT,0000 0000 0400 0200      ./,1
006165 :          :  ,OCT,0100 0040 0020 0000      ./,2
006166 :          :  ,OCT,0000 0770 1004 1004      .0,1
006167 :          :  ,OCT,1004 1004 1004 0770      .0,2
006168 :          :  ,OCT,0000 0000 1020 1010      .1,1
006169 :          :  ,OCT,1774 1000 1000 0000      .1,2
006170 :          :  ,OCT,0000 1030 1004 1404      .2,1
006171 :          :  ,OCT,1004 1204 1004 1170      .2,2
006172 :          :  ,OCT,0000 0404 1004 1044      .3,1
006173 :          :  ,OCT,1044 1024 1114 0604      .3,2
006174 :          :  ,OCT,0000 0100 0140 0134      .4,1
006175 :          :  ,OCT,0100 0100 0100 1774      .4,2
006176 :          :  ,OCT,0000 0474 1044 1044      .5,1
006177 :          :  ,OCT,1044 1044 1044 0704      .5,2
006178 :          :  ,OCT,0000 0760 1050 1044      .6,1
006179 :          :  ,OCT,1044 1044 1044 0710      .6,2
006180 :          :  ,OCT,0000 1004 0404 0304      .7,1
006181 :          :  ,OCT,0104 0144 0024 0014      .7,2
006182 :          :  ,OCT,0000 0730 1044 1044      .8,1
006183 :          :  ,OCT,1044 1044 1044 0730      .8,2
006184 :          :  ,OCT,0000 0470 1104 1104      .9,1
006185 :          :  ,OCT,1104 1104 1104 0770      .9,2
006186 :          :  ,OCT,0000 1774 1040 1040      .Ь,1
006187 :          :  ,OCT,1040 1040 1040 0700      .Ь,2
006188 :          :  ,OCT  ,
006189 :          :  ,OCT  ,
006190 :          :  ,OCT,0000 1774 0004 0004      .П,1
006191 :          :  ,OCT,0004 0004 0004 1774      .П,2
006192 :          :  ,OCT  ,
006193 :          :  ,OCT  ,                       .
006194 :          :  ,OCT  ,
006195 :          :  ,OCT  ,
006196 :          :  ,OCT,0000 1774 1040 1040      .Ы,1
006197 :          :  ,OCT,1040 0700 0000 1774      .Ы,2
006198 :          :  ,OCT,0000 0610 1004 1044      .З,1
006199 :          :  ,OCT,1044 1044 1044 0730      .З,2
006200 :          :  ,OCT  ,0000 1740 0220 0210    .A1
006201 :          :  ,OCT  ,0204 0204 0204 1774    .A2
006202 :          :  ,OCT  ,0000 1774 1044 1044    .B1
006203 :          :  ,OCT  ,1044 1044 1044 0730    .B2
006204 :          :  ,OCT  ,0000 0770 1004 1004    .C1
006205 :          :  ,OCT  ,1004 1004 1004 0610    .C2
006206 :          :  ,OCT  ,0000 1774 1004 1004    .D1
006207 :          :  ,OCT  ,1004 1010 0420 0340    .D2
006208 :          :  ,OCT  ,0000 1774 1104 1104    .E1
006209 :          :  ,OCT  ,1104 1104 1004 1000    .E2
006210 :          :  ,OCT  ,0000 1774 0104 0104    .F1
006211 :          :  ,OCT  ,0104 0104 0104 0004    .F2
006212 :          :  ,OCT  ,0000 0770 1004 1004    .G1
006213 :          :  ,OCT  ,1104 1104 1104 0710    .G2
006214 :          :  ,OCT  ,0000 1774 0100 0100    .H1
006215 :          :  ,OCT  ,0100 0100 0100 1774    .H2
006216 :          :  ,OCT  ,0000 0000 1000 1004    .I1
006217 :          :  ,OCT  ,1774 1004 1000 0000    .I2
006218 :          :  ,OCT  ,0000 0600 1000 1000    .J1
006219 :          :  ,OCT  ,1000 1004 1004 0774    .J2
006220 :          :  ,OCT  ,0000 1774 0040 0040    .K1
006221 :          :  ,OCT  ,0040 0120 0210 1404    .K2
006222 :          :  ,OCT  ,0000 1774 1000 1000    .L1
006223 :          :  ,OCT  ,1000 1000 1000 1000    .L2
006224 :          :  ,OCT  ,0000 1774 0010 0060    .M1
006225 :          :  ,OCT  ,0300 0060 0010 1774    .M2
006226 :          :  ,OCT  ,0000 1774 0020 0040    .N1
006227 :          :  ,OCT  ,0100 0200 0400 1774    .N2
006228 :          :  ,OCT  ,0000 0770 1004 1004    .O1
006229 :          :  ,OCT  ,1004 1004 1004 0770    .O2
006230 :          :  ,OCT  ,0000 1774 0104 0104    .P1
006231 :          :  ,OCT  ,0104 0104 0104 0070    .P2
006232 :          :  ,OCT  ,0000 0360 0410 1004    .Q1
006233 :          :  ,OCT  ,1204 1204 0410 1360    .Q2
006234 :          :  ,OCT  ,0000 1774 0104 0104    .R1
006235 :          :  ,OCT  ,0104 0304 0504 1070    .R2
006236 :          :  ,OCT  ,0000 0400 1030 1044    .S1
006237 :          :  ,OCT  ,1044 1044 1044 0710    .S2
006238 :          :  ,OCT  ,0000 0004 0004 0004    .T1
006239 :          :  ,OCT  ,1774 0004 0004 0004    .T2
006240 :          :  ,OCT  ,0000 0374 0400 1000    .U1
006241 :          :  ,OCT  ,1000 1000 0400 0374    .U2
006242 :          :  ,OCT  ,0000 0014 0060 0300    .V1
006243 :          :  ,OCT  ,1400 0300 0060 0014    .V2
006244 :          :  ,OCT  ,0000 0374 1400 0400    .W1
006245 :          :  ,OCT  ,0300 0400 1400 0374    .W2
006246 :          :  ,OCT  ,0000 1404 0210 0120    .X1
006247 :          :  ,OCT  ,0040 0120 0210 1404    .X2
006248 :          :  ,OCT  ,0000 0014 0020 0040    .Y1
006249 :          :  ,OCT  ,1700 0040 0020 0014    .Y2
006250 :          :  ,OCT  ,0000 1404 1204 1104    .Z1
006251 :          :  ,OCT  ,1044 1024 1014 1004    .Z2
006252 :          :  ,OCT,0000 1774 1000 1000      .Ш,1
006253 :          :  ,OCT,1770 1000 1000 1774      .Ш,2
006254 :          :  ,OCT,0000 1004 1044 1044      .Э,1
006255 :          :  ,OCT,1044 1044 1044 0770      .Э,2
006256 :          :  ,OCT,0000 1774 1000 1770      .Щ,1
006257 :          :  ,OCT,1770 1000 1774 1000      .Щ,2
006258 :          :  ,OCT,0000 0074 0100 0100      .Ч,1
006259 :          :  ,OCT,0100 0100 0100 1774      .Ч,2
006260 :          :  ,OCT,0000 1774 0100 0770      .Ю,1
006261 :          :  ,OCT,1004 1004 1004 0770      .Ю,2
006262 :     ,END,
006263 :       R*PO*I      :,NAME,
006264 :                         ,ATI ,12
006265 :                   12    ,XTA ,
006266 :                   12    ,VTM,      *00022B
006267 :                   12    ,ATX ,
006268 :                         ,RTE ,00177B
006269 :                   14    ,XTS ,
006270 :                   12    ,AAX ,00003B
006271 :                   12    ,ATX ,00001B
006272 :                         ,NTR ,3
006273 :                         ,X-A ,
006274 :                         ,NTR ,18
006275 :                         ,U1A,      *00010B
006276 :                   12    ,ATX ,00001B
006277 :                   12    ,XTA ,00005B
006278 :                   12    ,A/X ,
006279 :                   12    ,ATX ,
006280 :      *00010B:     12    ,XTA ,00005B
006281 :                   12    ,ATX ,00002B
006282 :      *00011B:     12    ,XTA ,00001B
006283 :                   12    ,AAX ,00004B
006284 :                         ,UZA,      *00014B
006285 :                   12    ,XTA ,00002B
006286 :                   12    ,A*X ,
006287 :                   12    ,ATX ,00002B
006288 :      *00014B:     12    ,XTA ,00001B
006289 :                         ,ASN ,65
006290 :                   12    ,ATX ,00001B
006291 :                         ,UZA,      *00020B
006292 :                   12    ,XTA ,
006293 :                   12    ,A*X ,
006294 :                   12    ,ATX ,
006295 :                         ,UJ,       *00011B
006296 :      *00020B:     15    ,XTR ,
006297 :                   12    ,XTA ,00002B
006298 :                   13    ,UJ,  00000B
006299 :                         ,UTC, 00000B
006300 : CONST
006301 :      *00022B:,OCT,0000  0000  0000  0000
006302 :      *00023B:,OCT,0000  0000  0000  0000
006303 :      *00024B:,OCT,0000  0000  0000  0000
006304 :      *00025B:,OCT,0037  7777  7777  7777
006305 :      *00026B:,OCT,0000  0000  0000  0001
006306 :      *00027B:,OCT,4050  0000  0000  0000
006307 :                   ,END,
006308 :       R*ST*I      :,NAME,
006309 :                   12    ,VTM,      *00006B
006310 :                   12    ,ATX ,
006311 :                         ,RTE ,00177B
006312 :                   12    ,XTS ,
006313 :                   12    ,ATX ,
006314 :                         ,NTR ,3
006315 :                   12    ,AMX ,00001B
006316 :                   12    ,AVX ,
006317 :                   14    ,ATX ,
006318 :                   15    ,XTR ,
006319 :                   13    ,UJ,  00000B
006320 :                         ,UTC, 00000B
006321 : CONST
006322 :      *00006B:,OCT,0000  0000  0000  0000
006323 :      *00007B:,OCT,6400  0000  0000  0000
006324 :                   ,END,
006325 :  R*TO*INT:,NAME,
006326 :  A/I0:,LC,1.
006327 :  ,UTC,1.БЛOKИP.
006328 :  R*TO*I:,ENTRY,.
006329 :  14,VTM,CONST.OKPYГЛ.
006330 :  15,ATX,.
006331 :  ,RTE,177B.
006332 :  15,XTS,-2.
006333 :  ,NTR,18.
006334 :  15,AVX,-2.
006335 :  14,XTS,.
006336 :  15,E+X,-1.
006337 :  15,A+X,.
006338 :  ,NTR,3.
006339 :  ,A+X,A/I0.
006340 :  15,AVX,-2.
006341 :  15,XTR,.
006342 :  15,UTM,-1.
006343 :  13,UJ,.
006344 :  CONST:,OCT,4000 0000 0000 0004
006345 :  ,OCT,4000 0000 0000 0000
006346 :  ,END,R*TO*INT.
006347 :       RASPAK12    :,NAME,
006348 :      *00000B:     15    ,ATX ,
006349 :                         ,ASN ,66
006350 :                         ,STI ,14
006351 :                         ,UTC,      *00006B
006352 :                         ,AAX ,
006353 :                         ,ATI ,12
006354 :                   14    ,XTA ,
006355 :                   12    ,UTC,      *00007B
006356 :                         ,APX ,
006357 :                         ,ASN ,100
006358 :                   13    ,UJ,  00000B
006359 :                         ,UTC, 00000B
006360 : CONST
006361 :      *00006B:,OCT,0000  0000  0000  0003
006362 :      *00007B:,OCT,7777  0000  0000  0000
006363 :      *00010B:,OCT,0000  7777  0000  0000
006364 :      *00011B:,OCT,0000  0000  7777  0000
006365 :      *00012B:,OCT,0000  0000  0000  7777
006366 :                   ,END,
006367 :       RASPAK8     :,NAME,
006368 :       C           :,EQU,           *00012B
006369 :  GAK/3:,EQU,     *00012B  -        *00012B
006370 :  GAK/5:,EQU,     *00013B  -        *00012B
006371 :  GAK/7:,EQU,     *00014B  -        *00012B
006372 :  GAL/1:,EQU,     *00015B  -        *00012B
006373 :                   14    ,VTM,      *00012B
006374 :                         ,NTR ,0
006375 :                   14    ,A*X , GAK/3
006376 :                   15    ,ATX ,
006377 :                   14    ,A/X , GAK/5
006378 :                         ,NTR ,3
006379 :                   14    ,A+X , GAK/7
006380 :                         ,ATI ,12
006381 :                         ,NTR ,2
006382 :                   14    ,A*X , GAK/5
006383 :                   15    ,A-X ,
006384 :                         ,NTR ,3
006385 :                   14    ,A+X , GAK/7
006386 :                         ,ATI ,11
006387 :                   12    ,XTA ,
006388 :                   11    ,ASN ,104
006389 :                   14    ,AAX , GAL/1
006390 :                         ,NTR ,2
006391 :                   13    ,UJ,  00000B
006392 :                         ,UTC, 00000B
006393 : CONST
006394 :      *00012B:,OCT,6400  0000  0000  0010
006395 :      *00013B:,OCT,4314  0000  0000  0000
006396 :      *00014B:,OCT,6400  0000  0000  0000
006397 :      *00015B:,OCT,0000  0000  0000  0377
006398 :                   ,END,
006399 :   READ*:,NAME,
006400 :   MONREAD*:,SUBP,
006401 :   READ*ERR:,SUBP,
006402 :   MONCARD*:,LC,25
006403 :   C:,ATI,8
006404 :   11,BASE,C
006405 :   ,XTA,MONCARD*
006406 :   ,AEX,*ENDFILE
006407 :   ,UZA,ENDINF
006408 :   ,XTA,MONCARD*
006409 :   ,AEX,*NAME
006410 :   ,UZA,ENDINF
006411 :   ,XTA,MONCARD*
006412 :   ,AAX,MSMB
006413 :   ,UZA,READ*ERR
006414 :   9,VTM,MONCARD*
006415 :   ,ITA,13
006416 :   ,XTS,
006417 :   KLOOP:9,XTA,
006418 :   8,ATX,
006419 :   ,AAX,CEMASK
006420 :   ,AEX,CESTR
006421 :   8,UTM,1
006422 :   9,UTM,1
006423 :   ,UIA,KLOOP
006424 :   ,CALL,MONREAD*
006425 :   ,STX,
006426 :   ,STI,13
006427 :   13,UJ,
006428 :   ENDINF:14,VTM,MONCARD*
006429 :   ,ITS,14
006430 :   14,UTM,13
006431 :   ,ITS,14
006432 :   ,XTS,
006433 :   ,CALL,PRINT8
006434 :   ,CALL,STOP*
006435 :   *END FILE:,ISO,6H*END F
006436 :   *NAME:,ISO,6H*NAME
006437 :   MSMB:,OCT,776
006438 :   CEMASK:,LOG,377
006439 :   CESTR:,LOG,12
006440 :   ,END,
006441 :  :,SUBP,READLIBT
006442 :    'DECLARATION'
006443 :  :,ARRAY, NTFORRLS
006444 :  ,LC,1
006445 :          :   ,ARRAY, WBEGFLAG             .
006446 :             ,LC,  2                       .
006447 :          :   ,EQUAL, WORDADR              .
006448 :             ,   , WBEGFLAG+1              .
006449 :          :   ,ARRAY, BUFFER               .
006450 :             ,PP   ,1024                   .
006451 :    'INSTRUCTION'                                       AFTER 2RA7
006452 :              ,ITS, 7                      .
006453 :             7,VTM, L                      .
006454 :             7,ATX,                        .
006455 :  14,VTM,NTFORRLS
006456 :  14,XTA,
006457 :  7,AAX,CRESPAGE-L
006458 :  7,ATX,ZONA-L
006459 :  14,AEX,
006460 :  ,NTR,3
006461 :  7,A-X,=0000 0000 0021 0000-L
006462 :  7,AOX,=0000 0000 0000 7740-L
006463 :  7,ATX,CTAPE-L
006464 :  ,NTR, 2
006465 :            14,VTM, BUFFER                 .
006466 :              ,ITA, 14                     .
006467 :             7,ATX, ABUF-L                 .
006468 :              ,UTC, WBEGFLAG               .
006469 :              ,XTA,                        .
006470 :              ,UZA, BEGSKIP                .
006471 :             7,ATX, ENTRYFLG-L             .
006472 :              ,XTA,                        .
006473 :             7,ATX, PARFLAG-L              .
006474 :              ,UTC, WBEGFLAG               .
006475 :              ,ATX,                        .
006476 :  BEGSKIP :  7,XTA, ENTRYFLG-L             .
006477 :              ,UZA, READWORD               .
006478 :             7,XTA, PARFLAG-L              .
006479 :              ,U1A, PAR1                   .
006480 :  EXIT1   :   ,WTC, WORDADR                .
006481 :              ,XTA,                        .
006482 :  EXIT    :  7,XTS, PARFLAG-L              .
006483 :             7,AEX, C1-L                   .
006484 :             7,STX, PARFLAG-L              .
006485 :             7,XTS,                        .
006486 :              ,STI, 7                      .
006487 :            13,UJ,                         .
006488 :  PAR1    :   ,WTC, WORDADR                .
006489 :              ,XTA, 1                      .
006490 :             7,AOX, CENTR-L                .
006491 :              ,UTC, WORDADR                .
006492 :              ,XTS,                        .
006493 :             7,ARX, CM2-L                  .
006494 :              ,UTC, WORDADR                .
006495 :              ,ATX,                        .
006496 :              ,WTC, WORDADR                .
006497 :              ,XTA, 3                      .
006498 :             7,AAX, LIBMASK-L              .
006499 :             7,ATX, LIBCODE-L              .
006500 :              ,UZA, L1                     .
006501 :              ,XTA,                        .
006502 :             7,ATX, ENTRYFLG-L             .
006503 :             7,ATX, ADRWORD-L              .                           RA
006504 :  L1      :   ,STX,                        .
006505 :              ,UJ,  EXIT                   .
006506 :  READDR1 :  7,XTA, LIBCODE-L              .
006507 :              ,UJ,  LIBR                   .
006508 :  RETLIBR :  7,ARX, ABUF-L                 .
006509 :             7,ATX, ADRWORD-L              .                          RA5
006510 :             7,XTA, LIBCODE-L              .
006511 :             7,ARX, C1-L                   .
006512 :             7,ATX, LIBCODE-L              .
006513 :  READWORD:  7,XTA, ADRWORD-L              .                           RA
006514 :             7,AEX, ABUF-L                 .
006515 :              ,UZA, EXIT2                  .
006516 :             7,AAX, CRESPAGE-L             .
006517 :              ,UZA, READDR1                .
006518 :  EXIT2   :  7,XTA, ADRWORD-L              .                           RA
006519 :             7,ARX, C1-L                   .
006520 :             7,ATX, ADRWORD-L                                        3RA6
006521 :             ,WTC,ADRWORD                  .
006522 :             ,XTA,-1                       .                     5RA65
006523 :              ,UJ,  EXIT                   .                 BEFORE RA66
006524 :  LIBR    :  7,ATX, LR-L                   .
006525 :             7,AAX, M3L-L                  .
006526 :              ,ASN, 64-30                  .
006527 :             7,ARX, LR-L                   .
006528 :             7,AAX, MESD-L                 .
006529 :             7,AUX, MUNP-L                 .
006530 :  7,ARX, CTAPE-L
006531 :             7,AOX, CRED-L                 .
006532 :             7,XTS, ABUF-L                 .
006533 :              ,ASN, 64-20                  .
006534 :            15,AOX,                        .
006535 :  7,ARX,ZONA-L
006536 :             7,ATX, AEX-L                  .
006537 :  7,AEX,AEX1-L
006538 :  ,UZA,LIBR1
006539 :  7,XTA,AEX-L
006540 :  7,ATX,AEX1-L
006541 :             7,070, AEX-L                  .
006542 :  LIBR1:7,XTA,LR-L
006543 :             7,AAX, M3L-L                  .
006544 :              ,U1A, EX2                    .
006545 :             7,XTA, LR-L                   .
006546 :             7,AAX, MSEC-L                 .
006547 :              ,ASN, 64+36
006548 :              ,UJ,  RETLIBR                .
006549 :  EX2     :   ,XTA,                        .
006550 :              ,UJ,  RETLIBR                .
006551 :    'CONST'
006552 :  L       :   ,OCT,                        .
006553 :  PARFLAG :   ,OCT,                        .
006554 :  ENTRYFLG:   ,OCT,                        .
006555 :  C1      :   ,OCT, 0000 0000 0000 0001    .
006556 :  CENTR   :   ,OCT, 2                      .
006557 :  LIBMASK :   ,OCT, 5777 7700 0000 0000    .
006558 :  CM2     :   ,OCT, 7777 7777 7777 7775    .
006559 :  LIBCODE :   ,OCT,                        .
006560 :  CRESPAGE:   ,OCT, 0000 0000 0000 1777    .
006561 :  LR      :   ,OCT,                        .
006562 :  M3L     :   ,OCT, 0000 0000 0007 7777    .
006563 :  MSEC    :   ,OCT, 1740 0000 0000 0000
006564 :  MESD    :   ,OCT, 4037 7700              .
006565 :  CRED    :   ,OCT, 0010                   .
006566 :  AEX     :   ,OCT,                        .
006567 :  ABUF     :  ,OCT,                        .
006568 :  ADRWORD :   ,OCT,                        .
006569 :  CTAPE:,OCT,0000 0000 0045 7740
006570 :  MUNP:,OCT,0007 7400 0060 0777
006571 :  AEX1:,OCT,
006572 :  ZONA:,BSS,1
006573 :     ,END,
006574 :          :   ,SUBP, READSTAR              .
006575 :    'DECLARATION'
006576 :          :   ,ARRAY, WBEGFLAG             .
006577 :             ,LC,  2                       .
006578 :          :   ,EQUAL, WORDADR              .
006579 :             ,   , WBEGFLAG+1              .
006580 :          :   ,ARRAY, BUFFER               .
006581 :             ,PP   ,1024                   .
006582 :    'INSTRUCTION'                                       AFTER 2RA7
006583 :              ,ITS, 7                      .
006584 :             7,VTM, L                      .
006585 :             7,ATX,                        .
006586 :            14,VTM, BUFFER                 .
006587 :              ,ITA, 14                     .
006588 :             7,ATX, ABUF-L                 .
006589 :              ,UTC, WBEGFLAG               .
006590 :              ,XTA,                        .
006591 :              ,UZA, BEGSKIP                .
006592 :             7,ATX, ENTRYFLG-L             .
006593 :              ,XTA,                        .
006594 :             7,ATX, PARFLAG-L              .
006595 :              ,UTC, WBEGFLAG               .
006596 :              ,ATX,                        .
006597 :  BEGSKIP :  7,XTA, ENTRYFLG-L             .
006598 :              ,UZA, READWORD               .
006599 :             7,XTA, PARFLAG-L              .
006600 :              ,U1A, PAR1                   .
006601 :  EXIT1   :   ,WTC, WORDADR                .
006602 :              ,XTA,                        .
006603 :  EXIT    :  7,XTS, PARFLAG-L              .
006604 :             7,AEX, C1-L                   .
006605 :             7,STX, PARFLAG-L              .
006606 :             7,XTS,                        .
006607 :              ,STI, 7                      .
006608 :            13,UJ,                         .
006609 :  PAR1    :   ,WTC, WORDADR                .
006610 :              ,XTA, 1                      .
006611 :             7,AOX, CENTR-L                .
006612 :              ,UTC, WORDADR                .
006613 :              ,XTS,                        .
006614 :             7,ARX, CM2-L                  .
006615 :              ,UTC, WORDADR                .
006616 :              ,ATX,                        .
006617 :              ,WTC, WORDADR                .
006618 :              ,XTA, 3                      .
006619 :             7,AAX, LIBMASK-L              .
006620 :             7,ATX, LIBCODE-L              .
006621 :              ,UZA, L1                     .
006622 :              ,XTA,                        .
006623 :             7,ATX, ENTRYFLG-L             .
006624 :             7,ATX, ADRWORD-L              .                           RA
006625 :  L1      :   ,STX,                        .
006626 :              ,UJ,  EXIT                   .
006627 :  READDR1 :  7,XTA, LIBCODE-L              .
006628 :              ,UJ,  LIBR                   .
006629 :  RETLIBR :  7,ARX, ABUF-L                 .
006630 :             7,ATX, ADRWORD-L              .                          RA5
006631 :             7,XTA, LIBCODE-L              .
006632 :             7,ARX, C1-L                   .
006633 :             7,ATX, LIBCODE-L              .
006634 :  READWORD:  7,XTA, ADRWORD-L              .                           RA
006635 :             7,AEX, ABUF-L                 .
006636 :              ,UZA, EXIT2                  .
006637 :             7,AAX, CRESPAGE-L             .
006638 :              ,UZA, READDR1                .
006639 :  EXIT2   :  7,XTA, ADRWORD-L              .                           RA
006640 :             7,ARX, C1-L                   .
006641 :             7,ATX, ADRWORD-L                                        3RA6
006642 :             ,WTC,ADRWORD                  .
006643 :             ,XTA,-1                       .                     5RA65
006644 :              ,UJ,  EXIT                   .                 BEFORE RA66
006645 :  LIBR    :  7,ATX, LR-L                   .
006646 :             7,AAX, M3L-L                  .
006647 :              ,ASN, 64-30                  .
006648 :             7,ARX, LR-L                   .
006649 :             7,AAX, MESD-L                 .
006650 :             7,AUX, MUNP-L                 .
006651 :             7,AOX, CRED-L                 .
006652 :             7,XTS, ABUF-L                 .
006653 :              ,ASN, 64-20                  .
006654 :            15,AOX,                        .
006655 :             7,ATX, AEX-L                  .
006656 :             7,070, AEX-L                  .
006657 :          :  7,XTA, LR-L              .
006658 :             7,AAX, M3L-L                  .
006659 :              ,U1A, EX2                    .
006660 :             7,XTA, LR-L                   .
006661 :             7,AAX, MSEC-L                 .
006662 :              ,ASN, 64+36
006663 :              ,UJ,  RETLIBR                .
006664 :  EX2     :   ,XTA,                        .
006665 :              ,UJ,  RETLIBR                .
006666 :    'CONST'
006667 :  L       :   ,OCT,                        .
006668 :  PARFLAG :   ,OCT,                        .
006669 :  ENTRYFLG:   ,OCT,                        .
006670 :  C1      :   ,OCT, 0000 0000 0000 0001    .
006671 :  CENTR   :   ,OCT, 2                      .
006672 :  LIBMASK :   ,OCT, 5777 7700 0000 0000    .
006673 :  CM2     :   ,OCT, 7777 7777 7777 7775    .
006674 :  LIBCODE :   ,OCT,                        .
006675 :  CRESPAGE:   ,OCT, 0000 0000 0000 1777    .
006676 :  LR      :   ,OCT,                        .
006677 :  M3L     :   ,OCT, 0000 0000 0007 7777    .
006678 :  MUNP    :   ,OCT, 0007 7400 0077 0037    .
006679 :  MSEC    :   ,OCT, 1740 0000 0000 0000
006680 :  MESD    :   ,OCT, 4037 7700              .
006681 :  CRED    :   ,OCT, 0010                   .
006682 :  AEX     :   ,OCT,                        .
006683 :  ABUF     :  ,OCT,                        .
006684 :  ADRWORD :   ,OCT,                        .
006685 :     ,END,
006686 :  READVOLK:,NAME,
006687 :  MONCARD*:,LC,
006688 :  KCOUNT*:,LC,
006689 :  ARREAD*:,LC,
006690 :  ,ITA,7
006691 :  ,ITS,13
006692 :  ,XTS,
006693 :  7,BASE,*
006694 :             ,XTA,MONCARD*+1
006695 :  ,AEX,=3HOLD
006696 :             ,UZA,READOLD
006697 :             ,XTA,KCOUNT*
006698 :             ,ATX,ARREAD*
006699 :             ,XTA,KCOUNT*+1
006700 :             ,ATX,ARREAD*+1
006701 :             ,XTA,
006702 :             ,ATX,KCOUNT*
006703 :  ,XTA,=1
006704 :             ,ATX,KCOUNT*+2
006705 :  10,VTM,
006706 :  ,CALL,PARAMT*
006707 :  11,VZM,READ//
006708 :  9,VTM,
006709 :  10,VTM,4
006710 :  ,CALL,PARAMT*
006711 :  14,V1M,MIST
006712 :  12,VZM,MIST
006713 :  ,ASN,64-30
006714 :  ,UJ,READ/
006715 :  READ//:,BSS,
006716 :  POIN:,XTA,MONCARD*+2
006717 :  ,AEX,=H
006718 :  ,UZA,STANDA
006719 :  ,APX,=6H'7''7''7''7''7'''
006720 :  ,XTS,MONCARD*+1
006721 :  ,AEX,=4HDRUM
006722 :  ,UZA,PDRUM
006723 :  ,STX,
006724 :  ,ASN,64+33
006725 :  ,ATI,14
006726 :  ,UJ,PFIN
006727 :  PDRUM:,STX,
006728 :  14,VTM,05 000B
006729 :  ,AEX,=2000 0000 0000 0000
006730 :  ,UZA,PFIN
006731 :  SDRUM:14,VTM,03 000B
006732 :  ,UJ,PFIN
006733 :  STANDA:14,VTM,31 001B
006734 :  ,XTA,MONCARD*+1
006735 :  ,AEX,=4HDRUM
006736 :  ,UZA,SDRUM
006737 :  PFIN:,BSS,
006738 :  ,ITA,14
006739 :  ,ASN,64-33
006740 :  ,AUX,=7707 7700 0000 0000
006741 :  READ/:,BSS,
006742 :  ,ARX,=7777 7700 0000 0000
006743 :  ,ASN,64+30
006744 :  ,AOX,=:001
006745 :  GREAD   :  ,ATX,KCOUNT*+1
006746 :  ,UJ,RETURN
006747 :  READOLD :  ,BSS,
006748 :             ,XTA,ARREAD*+1
006749 :             ,UZA,WRUSE
006750 :             ,XTA,ARREAD*
006751 :             ,ATX,KCOUNT*
006752 :             ,XTA,ARREAD*+1
006753 :             ,ATX,KCOUNT*+1
006754 :  ,XTA,=1
006755 :             ,ATX,KCOUNT*+2
006756 :  ,XTA,
006757 :  ,ATX,ARREAD*
006758 :  ,ATX,ARREAD*+1
006759 :  ,UJ,RETURN
006760 :  WRUSE   :  ,BSS,
006761 :  MIST:,BSS,
006762 :  ,STX,
006763 :  ,STI,13
006764 :  ,ATI,7
006765 :  ,ITA,13
006766 :  13,UJ,
006767 :  RETURN:,STX,
006768 :  ,STI,13
006769 :  ,ATI,7
006770 :  ,XTA,
006771 :  13,UJ,
006772 :  ,END,
006773 :  REAL:,NAME,
006774 : C BЗЯTИE ДEЙCTBИTEЛЬHOЙ ЧACTИ KOMПЛ.4ИCЛA
006775 :  ,ATI,14
006776 :  14,XTA,
006777 :  13,UJ,
006778 :  AIMAG:,ENTRY,
006779 : C BЗЯTИE KOЭФФИЦИEHTA ПPИ I B KOMПЛ. 4ИCЛE
006780 :  ,ATI,14
006781 :  14,XTA,1
006782 :  13,UJ,
006783 :  ,END,
006784 :  RECARD:,NAME,
006785 :  *REGIM//:,LC,1
006786 :  FOLZON:,SUBP,
006787 :  REWORD/:,PC,1024
006788 :  XREAD/:,LC,2
006789 :  *0:,BLOCK,BEG(0),POINT0(0),POINT,DINF
006790 : C------RBEGIN(YK)-ПOДГOTOBKA K ЧTEHИЮ
006791 : C      REWORD(AW)-ЧTEHИE CЛOBA
006792 : C      RECARD(AC)-ЧTEHИE KAPTЫ
006793 : C      FOLZON(AS)-OБPAБOTKA KAPTЫ,
006794 : C      HAЧИHAЮЩEЙCЯ C CИMBOЛA '377'
006795 : C------XAPAKTEPИCTИKA XREAD/:
006796 : C      ПEPBOE CЛOBO:
006797 : C      1-15-TEK.AДPEC CЛOBA B БYФEPE
006798 : C      16-33-HAЧAЛЬHOE ЗHAЧEHИE YK ЧTEHИЯ
006799 : C      35=1-KAPTA B COSY
006800 : C      36=1-ЗAKAЗ HA PAБOTY C COSY
006801 : C      B HECTAHДAPTHOM CEPBИCE PEДAKTOPA
006802 : C      BTOPOE CЛOBO:
006803 : C      ИHФOPMAЦИOHHOE CЛOBO TEK.OБMEHA
006804 :  C:,UTC,R
006805 :  ,ATX,
006806 :  ,ATI,9
006807 :  ,ITA,13
006808 :  ,ITS,7
006809 :  7,BASE,C
006810 :  ,ITS,
006811 :  10,VTM,-13
006812 :  ,ITS,9
006813 :  ,CALL,REWORD
006814 :  9,XTA,
006815 :  ,AAX,=:776
006816 :  ,AEX,=:776
006817 :  ,U1A,CYC1
006818 :  ,ITS,10
006819 :  ,ITS,9
006820 :  ,ITS,9
006821 :  ,CALL,FOLZON
006822 :  ,STI,9
006823 :  ,STI,10
006824 :  CYC:,ITS,9
006825 :  ,CALL,REWORD
006826 :  CYC1:,BSS,
006827 :  9,XTA,
006828 :  ,AAX,=377
006829 :  ,AEX,=012
006830 :  ,UZA,FIN
006831 :  9,UTM,1
006832 :  10,VLM,CYC
006833 :  ERROR:,BSS,
006834 :  12,XTA,
006835 :  ,ATX,ER
006836 :  12,XTA,1
006837 :  ,ATX,ER+1
006838 :  ,XTA,*REGIM//
006839 :  ,AAX,=77777
006840 :  ,UZA,PRI
006841 :  ,WTC,*REGIM//
006842 :  13,VJM,
006843 :  PRI:,PRINT,INFP
006844 :  ,CALL,STOP*
006845 :  FIN:12,XTA,BEG
006846 :  ,AAX,=2000 0000 0000.COSY
006847 :  ,U1A,RET
006848 :  ,XTS,R
006849 :  ,CALL,COSYTISO
006850 :  RET:,STI,
006851 :  ,STI,7
006852 :  ,STI,13
006853 :  13,UJ,
006854 :  REWORD:,ENTRY,
006855 :  12,VTM,XREAD/
006856 :  :,ATI,11
006857 :  8,BASE,C
006858 :  12,XTA,BEG
006859 :  ,AAX,=1777
006860 :  ,U1A,NOBEG
006861 :  12,TAPE,DINF
006862 :  NOBEG:12,WTC,POINT
006863 :  ,XTA,
006864 :  11,ATX,
006865 :  12,XTA,POINT
006866 :  ,AAX,=1777
006867 :  ,AEX,=1777
006868 :  ,UIA,COR
006869 :  13,MTJ,8
006870 :  12,XTA,DINF
006871 :  ,CALL,DRUMTAP*
006872 :  12,ATX,DINF
006873 :  12,XTA,BEG
006874 :  ,UTC,=1777
006875 :  ,AEX,
006876 :  12,STX,BEG
006877 :  8,UJ,
006878 :  COR:12,XTA,POINT
006879 :  ,ARX,VPT
006880 :  12,STX,BEG
006881 :  13,UJ,
006882 :  RBEGIN:,ENTRY,
006883 :  12,VTM,XREAD/
006884 :  11,VTM,REWORD/
006885 :  ,ATI,14
006886 :  ,ITA,11
006887 :  12,ATX,POINT0
006888 :  ,ASN,64-20
006889 :  14,AOX,
006890 :  ,UTC,=:001
006891 :  ,ARX,
006892 :  12,ATX,DINF
006893 :  14,XTA,
006894 :  ,ASN,64-15
006895 :  12,AOX,BEG
006896 :  12,STX,BEG
006897 :  13,UJ,
006898 :  FOLZON:,ENTRY,
006899 :  12,VTM,XREAD/
006900 :  :8,VTM,C
006901 :  ,UJ,ERROR
006902 :  VPT:,LOG,1
006903 :  INFP:,Z00,ERTEXT
006904 :  ,Z00,ER+1
006905 :  :,Z00,
006906 :  ,015,
006907 :  :2,015,16
006908 :  15,021,1
006909 :  ERTEXT:,BSS,
006910 :  ,GOST,14HNONTEXT CARD-'231'
006911 :  ER:,BSS,2
006912 :  R:,OCT,
006913 :  ,END,
006914 :       REVCARD     :,NAME,
006915 :  GAK/7:,EQU,     *00021B  -        *00000B
006916 :  GAL/1:,EQU,     *00023B  -        *00000B
006917 :  GAL/5:,EQU,     *00022B  -        *00000B
006918 :  GAL/8:,EQU,     *00024B  -        *00000B
006919 :      *00000B:     8     ,VTM,      *00000B
006920 :                         ,STI ,10
006921 :                         ,ATI ,11
006922 :                         ,XTA ,
006923 :                   14    ,VTM, 77751B
006924 :                         ,UTC, 00000B
006925 :      *00003B:     11    ,UTC, 00027B
006926 :                   14    ,ATX ,
006927 :                   14    ,VLM,      *00003B
006928 :                   12    ,VTM, 77777B
006929 :      *00005B:     8     ,XTA , GAK/7
006930 :                   8     ,ATX , GAL/1
006931 :      *00006B:     9     ,VTM, 77765B
006932 :                   8     ,XTA , GAL/5
006933 :      *00007B:     8     ,ATX , GAL/8
006934 :                   10    ,XTA ,
006935 :                   8     ,APX , GAL/8
006936 :                   8     ,AUX , GAL/1
006937 :                   11    ,AOX ,
006938 :                   11    ,ATX ,
006939 :                   11    ,UTM, 00002B
006940 :                   8     ,XTA , GAL/8
006941 :                         ,ASN ,65
006942 :                   9     ,VLM,      *00007B
006943 :                   11    ,UTM, 77750B
006944 :                   10    ,UTM, 00001B
006945 :                   8     ,XTA , GAL/1
006946 :                         ,ASN ,68
006947 :                   8     ,ATX , GAL/1
006948 :                         ,U1A,      *00006B
006949 :                   11    ,UTM, 00001B
006950 :                   12    ,VLM,      *00005B
006951 :                         ,STX ,
006952 :                   13    ,UJ,  00000B
006953 : CONST
006954 :      *00021B:,OCT,0017  0000  0000  0000
006955 :      *00022B:,OCT,4000  4000  4000  4000
006956 :      *00023B:,OCT,0000  0000  0000  0000
006957 :      *00024B:,OCT,0000  0000  0000  0000
006958 :                   ,END,
006959 :  RLIBLIS:,NAME,.NEW TEXT,VOLKOV
006960 :  LIBLIST:,SUBP,
006961 :  NTFORRLS:,LC,1
006962 :  NOLISFL*:,LC,1
006963 :  MONCARD*:,LC,14
006964 :  MONCARD/:,LC,14
006965 :  READLIBT:,SUBP,
006966 :  CATALOG:,PP,2048
006967 :  WBEGFLAG:,LC,2
006968 :  GAK/7:,EQU,WBEGFLAG+1
006969 :  GAL/1:,EQU,CATALOG+2
006970 :  WRITLIBR:,SUBP,
006971 :  PRINT8:,SUBP,
006972 :  BASE:14,VTM,-13
006973 :  ,UTC,
006974 :  IR:14,ITS,14.REGS TO STAC
006975 :  14,VLM,IR
006976 :  7,BASE,BASE
006977 :  1,VTM,1
006978 :  TRANL:,UTC,LIBLIST
006979 :  1,XTA,-1
006980 :   1,ATX,OWNLIST-1
006981 :  ,UZA,GP
006982 :  1,VLM,TRANL
006983 :  GP:,BSS,
006984 :  10,VTM,
006985 :  ,CALL,PARAMT*
006986 :  ,XTA,=66 0000
006987 :  ,ATX,NTF
006988 :  11,V1M,CONT
006989 :  ,XTA,MONCARD/+1
006990 :  ,AAX,=6H'0''0''0''0''377''377'
006991 :  ,AEX,=6H'0''0''0''0'
006992 :  ,UZA,CONT1
006993 :  ,APX,=6H'''''''''7''7'
006994 :  ,ASN,64+30
006995 :  ,ATX,NTF
006996 :  ,UJ,CONT1
006997 :  CONT:10,VTM,4
006998 :  9,VTM,NTF
006999 :  ,CALL,PARAMT*
007000 :  14,V1M,RETURN
007001 :  CONT1:,BSS,
007002 :  1,VTM,
007003 :  11,UTM,-54B
007004 :  11,VIM,ETRAN
007005 :  NEPUSTO:11,MTJ,3
007006 :  1,UTM,1
007007 :  FRCARD:10,VTM,3
007008 :  9,VTM,
007009 :  ,CALL,PARAMT*
007010 :  1,ATX,OWNLIST-1
007011 :  11,UTM,-54B
007012 :  12,VIM,NEPUSTO
007013 :  3,VIM,EPAR
007014 :  ,CALL,MONREAD*
007015 :  10,VTM,9
007016 :  14,VTM,-13
007017 :  :14,XTA,MONCARD*+13
007018 :   14,ATX,MONCARD/+13
007019 :   14,VLM,*-1
007020 :  6,VTM,MONCARD*
007021 :  9,VTM,
007022 :  ,CALL,PARAMT*
007023 :  ,ITS,6
007024 :  6,UTM,13
007025 :  ,ITS,6
007026 :  ,XTS,=1
007027 :  13,VTM,FRCARD
007028 :  ,UJ,PRINT8
007029 :  EPAR:,XTA,
007030 :  1,ATX,OWNLIST-1
007031 :  ETRAN:,XTA,NOLISFL*
007032 :  ,U1A,NOPRIN
007033 :  ,PRINT,PRI1
007034 :  NOPRIN:,XTA,NTF
007035 :  ,UTC,NTFORRLS
007036 :  ,ATX,
007037 :  ,AAX,=6000
007038 :  ,UIA,COPYCARD
007039 :  ,XTA,NTF
007040 :  ,ARX,=10 0000
007041 :  ,ASN,64+18
007042 :  ,UIA,COPYCARD
007043 :  ,XTA,NTF
007044 :  14,VTM,CATALOG
007045 :  ,ITS,14
007046 :  ,ASN,64-20
007047 :  15,AOX,
007048 :  ,AOX,PR
007049 :  ,ATX,INFR.INF FOR CATAL READ
007050 :  ,TAPE,INFR.PART1
007051 :  ,XTA,INFR
007052 :  ,ARX,PLUS1
007053 :  ,ATX,INFR
007054 :  ,TAPE,INFR
007055 :  1,VTM,2
007056 :  EC:,UTC,GAL/1
007057 :  1,XTA,-1
007058 :  ,AEX,PCON
007059 :  ,UZA,EC1.END CATAL
007060 :  1,UTM,2
007061 :  ,UJ,EC
007062 :  EC1:1,UTM,-2
007063 :  1,VZM,GIVNAM
007064 :  1,MTJ,4
007065 :  4,UTM,-2
007066 :  ,UTC,GAL/1
007067 :  4,XTA,
007068 :  ,ATX,CNAME
007069 :  2,VTM,OWNLIST
007070 :  2,XTA,
007071 :  ,UZA,FPRINT.END LIBLIST
007072 :  EC2:2,XTA,
007073 :  ,UZA,EC1
007074 :  ,AEX,CNAME
007075 :  ,UZA,AEX.EQU NAME
007076 :  2,UTM,1
007077 :  ,UJ,EC2
007078 :  EC3:2,UTM,1
007079 :  AEX:2,XTA,1
007080 :  2,ATX,
007081 :  ,AEX,
007082 :  ,U1A,EC3
007083 :  ,XTA,NOLISFL*
007084 :  ,U1A,A1
007085 :  ,XTS,CNAME
007086 :  14,VTM,24
007087 :  ,ITS,14
007088 :   ,CALL,PT6/G
007089 :  A1:,UTC,GAL/1
007090 :  4,XTA,3
007091 :  ,AAX,YML
007092 :  ,U1A,A2.YES YML
007093 :  4,UTM,2
007094 :  ,UJ,A1
007095 :  A2:2,VTM,GAL/1
007096 :  4,J+M,2
007097 :  ,ITA,2
007098 :  ,UTC,GAK/7
007099 :  ,ATX,
007100 :  14,VTM,READLIBT
007101 :  ,XTA,PCON
007102 :  ,UTC,WBEGFLAG
007103 :  ,ATX,
007104 :  ,ITS,14
007105 :  ,CALL,WRITLIBR
007106 :  ,STX,
007107 :  ,UJ,EC1
007108 :  GIVNAM:2,VTM,OWNLIST
007109 :  2,XTA,
007110 :  ,UZA,FPRINT
007111 :  ,PRINT,PRI2
007112 :  2,XTA,
007113 :  GIV1:15,ATX,
007114 :  14,VTM,34
007115 :  ,ITS,14
007116 :   ,CALL,PT6/G
007117 :  2,UTM,1
007118 :  2,XTA,
007119 :  ,UZA,FPRINT
007120 :  ,UJ,GIV1
007121 :  FPRINT:,XTA,NOLISFL*
007122 :  ,U1A,NOP
007123 :  ,PRINT,PRI3
007124 :  NOP:,BSS,
007125 :  ,XTA,47B
007126 :  ,AAX,NONP
007127 :  14,VTM,CATALOG
007128 :  ,ITS,14
007129 :  ,ASN,64-20
007130 :  15,AOX,
007131 :  ,ATX,CNAME
007132 :  ,DRUM,CNAME.CATAL TO DRUM
007133 :  ,XTA,CNAME
007134 :  ,ARX,PLUS1
007135 :  ,ATX,CNAME
007136 :  ,DRUM,CNAME
007137 :  1,VTM,GAL/1
007138 :  K:1,XTA,.FOR LIBLIST
007139 :  ,AEX,LIBLI
007140 :  ,UZA,K1
007141 :  1,XTA,1
007142 :  ,AEX,PCON
007143 :  ,UZA,M
007144 :  1,UTM,2
007145 :  ,UJ,K
007146 :  K1:1,XTA,1.YML FOR LIBLIST
007147 :  ,AAX,YML
007148 :  ,UZA,L
007149 :  1,XTA,3
007150 :  ,AAX,YML
007151 :  ,U1A,L
007152 :  1,XTA,1
007153 :  ,AAX,CL
007154 :  1,AOX,3
007155 :  1,ATX,3
007156 :  L:1,XTA,2
007157 :  1,ATX,
007158 :  1,XTA,3
007159 :  1,ATX,1
007160 :  ,AEX,PCON
007161 :  ,UZA,L1
007162 :  1,UTM,2
007163 :  ,UJ,L
007164 :  L1:1,ATX,3
007165 :  M:,XTA,50B
007166 :  ,AAX,NONP
007167 :  14,VTM,CATALOG
007168 :  ,ITS,14
007169 :  ,ASN,64-20
007170 :  15,AOX,
007171 :  ,ATX,CNAME
007172 :  ,DRUM,CNAME
007173 :  ,XTA,CNAME
007174 :  ,ARX,PLUS1
007175 :  ,ATX,CNAME
007176 :  ,DRUM,CNAME
007177 :  RETURN:,BSS,
007178 :  ,STX,
007179 :  14,VTM,13
007180 :  RET:14,STI,
007181 :  14,UTM,-1
007182 :  14,V1M,RET
007183 :  13,UJ,
007184 :   COPYCARD:12,VTM,TER
007185 :   ,*64,PRIN12
007186 :   ,SJ,
007187 :   PRIN12:12,Z00,
007188 :   12,Z00,
007189 :   ,010,
007190 :   9,,
007191 :  PRI1:,Z00,TEX1
007192 :  ,Z00,TEX1
007193 :  ,010,
007194 :  9,,
007195 :  PRI2:,Z00,TEX2
007196 :  ,Z00,TEX2
007197 :  ,010,
007198 :  9,,
007199 :  PRI3:,Z00,TEX3
007200 :  ,Z00,TEX3
007201 :  ,010,
007202 :  9,,
007203 :  NTF:,OCT,0010 0000 0066 0000
007204 :  INFR:,BSS,1
007205 :  PLUS1:,OCT,0000 0100 0000 0001
007206 :  PCON:,OCT,7777 7777 7777 7777
007207 :  CNAME:,BSS,1
007208 :  TEX1:,GOST,12H*****LIBLIST
007209 :  ,CONT,12H************
007210 :  ,CONT,12H***********'231'
007211 :  TEX2:,GOST,12HFOLLOWING SU
007212 :  ,CONT,12HBROUTINES AB
007213 :  ,CONT,13HSENT ON TAPE'231'
007214 :  TEX3:,GOST,12H*****END OF
007215 :  ,CONT,12HLIBLIST*****
007216 :  ,CONT,12H***********'231'
007217 :   TER:,GOST,24HOШИБKA B ИHФOPMAЦИИ'231'
007218 :  YML:,OCT,5777 7700 0000 0000
007219 :  CL:,OCT,7777 7777 7770 0000
007220 :  LIBLI:,TEXT,8HLIBLIST
007221 :  NONP:,OCT,7777 4077 7777 7777
007222 :  PR:,OCT,0010 0000 0000 0000
007223 :  OWNLIST:,BSS,1000
007224 :  ,END,
007225 :  SETFTN:,NAME,. Ф И A Э
007226 :  RM*:,LC,.
007227 :  *REGIM//:,LC,1.ДOБABKA
007228 :  NOLISFL*:,LC,1.
007229 :  PUNCHFL*:,LC,1.
007230 :  FULISFL*:,LC,1.
007231 :  CHECKFL*:,LC,1.
007232 :  FLAGS:  ,LC,1
007233 :  MONCARD/:,LC,14.
007234 :  *MONCRD*:,LC,12.
007235 :  XPARAMT*:,LC,4.
007236 :  MIST10* :,LC,3.
007237 :  OUT*OUT :,LC,1
007238 :  STARSTA :,LC,1
007239 :  PARAMT* :,SUBP,.
007240 : C**************************************
007241 : C   YHИBEPCAЛЬHAЯ ПOДПPOГPAMMA ДЛЯ
007242 : C   ПPOCTAHOBKИ И CHЯTИЯ ФЛAГOB  B
007243 : C   MOHИTOPHOЙ CИCTEME   "ДYБHA" .
007244 : C      (HEPEЗИДEHT STAR*)
007245 : C   PAБOTAET C 48-17PP *REGIM//  И
007246 : C   C 15-1PP   FLAGS
007247 : C***************************************
007248 :  *C:,ITS,13.
007249 :  ,ITS,7
007250 :  ,ITS,
007251 :  7,BASE,*C
007252 :   ,ATX,ZAG1.+
007253 :  ,ATX,OШИБKИ
007254 :  ,XTA,C177*
007255 :   ,ATX,OUT*OUT.
007256 :   10,VTM,.YCTAHOBKA
007257 :  ,CALL,PARAMT*.
007258 :  11,VIM,PAR*GIV.
007259 :  15,XTA,-3.CYMMATOP HA BXOДE
007260 :  ,ATI,14.  -"- .EQ.0.OR.NE.0
007261 :  14,VIM,PU1
007262 :  ,XTA,=22
007263 :  ,AOX,FLAGS.
007264 :  ,ATX,FLAGS.
007265 :  PU1:14,VIM,PU2.
007266 :  ,UTC,1.
007267 :  PU2:,XTA,ALL.
007268 :   ,ATX,ZAG1.
007269 :  PAR*GIV:11,VZM,BCЯKA.
007270 :  14,VTM,-6.
007271 :  :14,UTC,XPARAMT*
007272 :    ,XTA,6.
007273 :   14,UTC,SPAR
007274 :   ,ATX,6.
007275 :  14,VLM,PAR*GIV+1.
007276 :  10,VTM,1.TEKCT
007277 :  ,CALL,PARAMT*
007278 :  14,VZM,NO MIS.
007279 :  MISTAKE:14,VTM,-6.
007280 :  :14,UTC,SPAR.
007281 :  ,XTA,6.
007282 :  14,UTC,XPARAMT*
007283 :  ,ATX,6.
007284 :  14,VLM,MISTAKE+1.
007285 :  ,XTA,BT.
007286 :  ,ATX,STARSTA
007287 :  ,ATX,OШИБKИ.
007288 :  10,VTM,2.
007289 :  ,CALL,PARAMT*
007290 :  ,XTA,.
007291 :  ,ATX,STARSTA
007292 :  ,UJ,PAR*GIV
007293 :  NO MIS:14,VTM,BT-ET.
007294 :  :7,XTA,*MONCRD*-*C.
007295 :  14,UTC,ET
007296 :  ,AEX,.
007297 :  ,ASN,64+24
007298 :  ,UZA,XOP*ПAP.
007299 :  14,VLM,NO MIS+1
007300 :  ,UJ,MISTAKE.HET TAKOГO
007301 :  XOP*ПAP:,XTA,=:4
007302 :  ,UTC,ET-BT
007303 :  14,ASN,64.
007304 :  ,UIA,NO PUS.
007305 :  ,UTC,*MONCRD*
007306 :  ,XTA,1.
007307 :  ,UZA,MISTAKE.
007308 :  7,ATX,*MONCRD*-*C.
007309 :  ,XTA,
007310 :  ,UTC,*MONCRD*
007311 :  ,ATX,1.
007312 :  ,UJ,NO MIS.
007313 :  NO PUS:,BSS,.
007314 :  ,AOX,ZAG1.
007315 :  ,ATX,ZAG1.
007316 :  ,UJ,PAR*GIV.
007317 :  BCЯKA:,XTA,OШИБKИ.
007318 :  ,UZA,HE БЫЛO.
007319 :  14,VTM,-13.
007320 :  L/S/:14,UTC,MONCARD/.
007321 :  ,XTA,13.
007322 :  ,AEX,=6H******
007323 :  ,ARX,C177*
007324 :  ,AAX,C200*
007325 :  ,ASN,64+7
007326 :  ,ARX,C177*
007327 :  14,UTC,MONCARD/
007328 :  ,AAX,13.
007329 :  ,AOX,=6H
007330 :  14,UTC,MONCARD/
007331 :  ,ATX,13
007332 :  14,VLM,L/S/.
007333 :  14,VTM,MONCARD/
007334 :  ,XTA,=6HERRORS
007335 :  14,ATX,.
007336 :  ,ITS,14.
007337 :  14,UTM,13
007338 :  ,ITS,14
007339 :  ,ITS,
007340 :  ,CALL,PRINT8.
007341 :  HE БЫЛO:,BSS,.
007342 :  ,XTA,ZAG1.
007343 :  ,ATX,ZAG2.
007344 :  ,AAX,=77777
007345 :  ,ATX,ZAG1.
007346 :  ,AOX,FLAGS
007347 :  ,ATX,FLAGS.
007348 :  ,XTA,ZAG2.
007349 :  ,AAX,=7777 7777 7700 0000
007350 :  ,ATX,ZAG2.
007351 :  ,AOX,*REGIM//.
007352 :  ,ATX,*REGIM//.
007353 :  15,XTA,-3
007354 :  ,UIA,*EXIT
007355 :  ,XTA,*REGIM//.
007356 :  ,AEX,ZAG2.
007357 :  ,ATX,*REGIM//.
007358 :  ,XTA,FLAGS
007359 :  ,AEX,ZAG1.
007360 :  ,ATX,FLAGS.
007361 :  *EXIT:,BSS,.
007362 :  ,XTA,FLAGS.
007363 :  ,AAX,=2.*STANDART
007364 :  ,UZA,NO*STA
007365 :  ,XTA,
007366 :  ,ATX,NOLISFL*.
007367 :  ,ATX,FULISFL*.
007368 :  NO*STA:,XTA,FLAGS.
007369 :  ,AAX,=100
007370 :  ,AEX,=100
007371 :  ,ASN,64+6
007372 :  ,ATX,CHECKFL*.
007373 :  ,XTA,FLAGS.
007374 :  ,ASN,64+5
007375 :  ,AAX,=1
007376 :  ,ATX,PUNCHFL*
007377 :  ,XTA,FLAGS.
007378 :  ,AAX,=1
007379 :  ,ATX,NOLISFL*
007380 :  ,XTA,FLAGS.
007381 :  ,ASN,64+2
007382 :  ,AAX,=1
007383 :  ,ATX,FULISFL*
007384 :  ,XTA,*REGIM//.
007385 :  ,APX,=:004
007386 :  ,AUX,=:2.
007387 :  ,XTS,RM*+3.
007388 :  ,AOX,=:2
007389 :  ,AEX,=:2.
007390 :  15,AOX,.
007391 :  ,STX,RM*+3.
007392 :  ,STI,7
007393 :  ,STI,13.
007394 :  ,NTR,6
007395 :  13,UJ,.
007396 :  BT:,BSS,.
007397 : C***   PAЗPЯДЫ  *REGIM//   ***
007398 :  48,ISO,6HONE
007399 :  47,ISO,6HLONG
007400 :  46,ISO,6H46
007401 :  45,ISO,6H45
007402 :  44,ISO,6H44
007403 :  43,ISO,6HMODUL
007404 :  42,ISO,6HDEBUGE
007405 :  41,ISO,6H41
007406 :  40,ISO,6H40
007407 :  39,ISO,6H39
007408 :  38,ISO,6H38
007409 :  37,ISO,6H37
007410 :  36,ISO,6H36
007411 :  35,ISO,6H35
007412 :  34,ISO,6H34
007413 :  33,ISO,6H33
007414 :  32,ISO,6H32
007415 :  31,ISO,6H31
007416 :  30,ISO,6H30
007417 :  29,ISO,6H29
007418 :  28,ISO,6H28
007419 :  27,ISO,6H27
007420 :  26,ISO,6H26
007421 :  25,ISO,6H25
007422 :  24,ISO,6HOLD OV
007423 :  23,ISO,6H23
007424 :  22,ISO,6H/CHECK
007425 :  21,ISO,6H21
007426 :  20,ISO,6H2/OPTI
007427 :  19,ISO,6HLIBRAR
007428 :  18,ISO,6H'43'
007429 :  17,ISO,6H'43'
007430 :  16,OCT,1423 3040 1002 0040
007431 : C***      PAЗPЯДЫ  FLAGS    ***
007432 :  15,ISO,6HDLIST
007433 :  14,ISO,6HEIGHT
007434 :  13,ISO,6HREPLAC
007435 :  12,ISO,6HCALL
007436 :  11,ISO,6HLABEL
007437 :  10,ISO,6HCOMPUT
007438 :   9,ISO,6HINDEX
007439 :   8,ISO,6HCONDIT
007440 :   7,ISO,6HOPTIMI
007441 :   6,ISO,6HPUNCH
007442 :   5,ISO,6HBINARY
007443 :   4,ISO,6HASSEMB
007444 :   3,ISO,6HFULL L
007445 :   2,ISO,6HSTANDA
007446 :   1,ISO,6HNO LIS
007447 :    ,ISO,6HCHECK
007448 :    ,ISO,6HPRINT
007449 :  ET:,ISO,6HTRACE
007450 :  ALL:,LOG,77612
007451 :  ,OCT,6100 0000 4105 7655
007452 :  C177*:,GOST,6H'177''177''177''177''177''177'
007453 :  C200*:,GOST,6H'200''200''200''200''200''200'
007454 :  ZAG1:,BSS,1.
007455 :  ZAG2:,BSS,1.
007456 :  OШИБKИ:,BSS,1.
007457 :  SPAR:,BSS,10.
007458 :  ,END,
007459 :  SIGN:,NAME,.
007460 :  SIGN:,BSS,.
007461 :  R*TO*INT:,SUBP,.
007462 :  15,WTC,.
007463 :  ,XTS,.
007464 :  ,NTR,18
007465 :  15,ATX,.
007466 :  15,AVX,.
007467 :  15,WTC,.
007468 :  ,AVX,.
007469 :  13,UJ,.
007470 :  ISIGN:,ENTRY,.
007471 :  13,MTJ,12.
007472 :  13,VJM,SIGN.
007473 :  *12:,BSS,.
007474 :  12,MTJ,13.
007475 :  ,UJ,R*TO*INT.
007476 :  IDIM:,ENTRY,.
007477 :  13,MTJ,12.
007478 :  13,VTM,*12.
007479 :  DIM:,ENTRY,.
007480 :  ,ATI,14.
007481 :  14,XTA,.
007482 :  ,NTR,18.
007483 :  15,WTC,.
007484 :  ,X-A,.
007485 :  13,UZA,.
007486 :  ,XTA,.
007487 :  13,UJ,.
007488 :  ,END,SIGN    .
007489 :   SIN*:,NAME,
007490 :   14,VTM,1
007491 :   ,UJ,*50
007492 :   COS*:,ENTRY,
007493 :   14,VTM,2
007494 :   ,UJ,*50
007495 :   SQRT*:,ENTRY,
007496 :   14,VTM,
007497 :   ,UJ,*50
007498 :   LN*:,ENTRY,
007499 :   14,VTM,5
007500 :   ,UJ,*50
007501 :   ARCTAN*:,ENTRY,
007502 :   14,VTM,3
007503 :   ,UJ,*50
007504 :   EXP*:,ENTRY,
007505 :   14,VTM,6
007506 :   *50:,A+X,
007507 :   14,*50,
007508 :   13,UJ,
007509 :  ,END,
007510 :       SNGL        :,NAME,
007511 :       D*LO*I      :,SUBP,
007512 :       R*ST*D      :,SUBP,
007513 :       D*LO*D      :,SUBP,
007514 :   ACC*:,LC,1
007515 :   D*ST*R:,SUBP,
007516 :   ,ATI,14
007517 :   14,XTA,1
007518 :   ,ATX,ACC*
007519 :   14,XTA,
007520 :   14,VTM,
007521 :   ,UJ,D*ST*R
007522 :  IDINT:,ENTRY,
007523 :                   12    ,VJM,      *00013B
007524 :                   13    ,VJM, D*LO*I
007525 :                         ,UJ,       *00016B
007526 :  DBLE:,ENTRY,
007527 :                   12    ,VJM,      *00013B
007528 :                   14    ,XTA ,
007529 :                   11    ,MTJ ,14
007530 :                   13    ,VJM, R*ST*D
007531 :                   11    ,MTJ ,14
007532 :                   13    ,VJM, D*LO*D
007533 :                         ,UJ,       *00016B
007534 :                         ,UTC, 00000B
007535 :      *00013B:     11    ,VTM,      *00020B
007536 :                         ,ATI ,14
007537 :                         ,ITA ,13
007538 :                   15    ,ATX ,
007539 :                   12    ,UJ,  00000B
007540 :                         ,UTC, 00000B
007541 :      *00016B:     11    ,STX ,
007542 :                         ,ATI ,8
007543 :                   11    ,XTA ,
007544 :                   8     ,UJ,  00000B
007545 :  *00020B:,BSS,2
007546 :                   ,END,
007547 :  STAPUNCH:  ,NAME,                        .
007548 :  REVCARD :  ,SUBP,                        .
007549 :  DIREVERS:  ,EQU,REVCARD                  .
007550 :  BIGBUF:,PC,811.
007551 :  C:,BSS,.
007552 :   :,UJ,PERCOSY1
007553 :   :,UJ,PERSTAN1
007554 :   :,UJ,PERCOSY2
007555 :   :,UJ,PERCOSY3
007556 :   PERCOSY1:,BSS,
007557 :  8,BASE,C.
007558 :  ,XTS,=5000 0000 0000 0000.
007559 :  ,STX,NCARD.
007560 :  ,UJ,ENTRY1.
007561 :   PERSTAN1:,BSS,
007562 :  8,BASE,C.
007563 :  ,XTS,=1200 0000 0000 0000.
007564 :  ,STX,NCARD.
007565 :  ENTRY1:,BSS,.
007566 :            8 ,  ATX  , NAME-C             .
007567 :           14,VTM,-3                       .
007568 :  CYCL1   : 14,UTC,BEGSMBUF+3              .
007569 :              ,  XTA  ,                     .
007570 :           14,UTC,ADRSMBUF+3               .
007571 :              ,  ATX  ,                     .
007572 :            14,  VLM  ,CYCL1                .
007573 :             ,XTA,                         .
007574 :            8,ATX,FULBI-C                  .
007575 :              ,  ITA  ,13                   .
007576 :  8,ATX,FULSM-C
007577 :             8,  XTS  ,NAME-C               .
007578 :            14,  VTM  ,SMALLBUF+3           .
007579 :              ,  ITS  ,14                   .
007580 :             ,CALL,BIGPUNCH                .
007581 :             ,STI,11                       .
007582 :             8,  VTM  ,C                    .
007583 :  ,UJ,OUTFULSM
007584 :   PERCOSY3:,BSS,
007585 :   PERSTAN3:,BSS,
007586 :  ENTRY3  :  8,  VTM  ,C                    .
007587 :  8,XTA,UN1-C
007588 :             8,  ATX  ,ENDPRISN-C           .
007589 :  8,ATX,FULSM-C
007590 :             8,  AOX  ,SMALLBUF-C           .
007591 :             8,  ATX  ,SMALLBUF-C           .
007592 :  13,MTJ,11
007593 :  ,UJ,OUTFULSM
007594 :   PERCOSY2:,BSS,
007595 :   PERSTAN2:,BSS,
007596 :   PERSTANE:,BSS,
007597 :  ENTRY2  :  8,  VTM  ,C                    .
007598 :  11,VJM,OUTFULSM
007599 :              ,  WTC  ,ADRSMBUF             .
007600 :              ,  ATX  ,                     .
007601 :             8,  XTA  ,ADRSMBUF-C           .
007602 :             8,  ARX  ,CONSMBUF-C           .
007603 :             8,  ATX  ,ADRSMBUF-C           .
007604 :              ,  U1A  ,QUQU                 .
007605 :  8,ATX,FULSM-C
007606 :  ,UJ,QUQU
007607 :  OUTFULSM:,ITS,11
007608 :  8,ATX,I11-C
007609 :  8,XTA,FULSM-C
007610 :  ,U1A,AGA
007611 :  ,STX,
007612 :  11,UJ,
007613 :  AGA     :  9,  VTM  ,-7                   .
007614 :             8,  XTA  ,BEGPOR-C             .
007615 :             8,  AEX  ,CONPO-C              .
007616 :              ,  ASN  ,64-2                 .
007617 :             8,  ATX  ,POINTR-C             .
007618 :             8,  XTA  ,BEGPOW-C             .
007619 :             8,  AEX  ,CONPO-C              .
007620 :              ,  ASN  ,64-2                 .
007621 :             8,  ATX  ,POINTW-C             .
007622 :              ,  ITA  ,13                   .
007623 :              ,  XTS  ,                     .
007624 :  CYCL2   : 10,  VTM  ,-7                   .
007625 :  CYCL3   :  8,  XTS  ,POINTR-C             .
007626 :             ,CALL,RASPAK12                .
007627 :          :  8,  XTS  ,POINTW-C             .
007628 :             ,CALL,ZAPAK12                 .
007629 :          :  8,  XTA  ,POINTR-C             .
007630 :             8,  ARX  ,UN1-C                .
007631 :             8,  ATX  ,POINTR-C             .
007632 :             8,  XTA  ,POINTW-C             .
007633 :             8,  ARX  ,UN1-C                .
007634 :             8,  ATX  ,POINTW-C             .
007635 :            10,  VLM  ,CYCL3                .
007636 :              ,  XTS  ,                     .
007637 :             8,  XTS  ,POINTW-C             .
007638 :             ,CALL,ZAPAK12                 .
007639 :          : 8,XTA,POINTW-C                 .
007640 :             8,  ARX  ,UN1-C                .
007641 :             8,  ATX  ,POINTW-C             .
007642 :             9,  VLM  ,CYCL2                .
007643 :            14,  VTM  ,SMALLBUF             .
007644 :              ,  ITA  ,14                   .
007645 :             ,CALL,CHECKSUM                .
007646 :          :  ,ASN,64-12                     .
007647 :             8,  AOX  ,SMALLBUF-C           .
007648 :             8,  ATX  ,SMALLBUF-C           .
007649 :           11,VJM,OUTFULBI                 .
007650 :             8,  XTS  ,ADRBIBUF-C           .
007651 :            14,  VTM  ,SMALLBUF             .
007652 :              ,  ITS  ,14                   .
007653 :            13,  VJM  ,DIREVERS             .
007654 :          :   ,  WTC  ,ADRBIBUF            .
007655 :           14,  VTM  ,                     .
007656 :           14,  MTJ  ,13                   .
007657 :           13,  UTM  ,22                   .
007658 :           12,  VTM  ,-5                    .
007659 :   LOOP   : 14,  XTA  ,                     .
007660 :           13,  XTS  ,                      .
007661 :           14,  STX  ,                      .
007662 :           13,  ATX  ,                      .
007663 :           14,  XTA  ,1                     .
007664 :           13,  XTS  ,1                     .
007665 :           14,  STX  ,1                     .
007666 :           13,  ATX  ,1                     .
007667 :           14,  UTM  ,2                     .
007668 :           13,  UTM  ,-2                    .
007669 :           12,  VLM  ,LOOP                  .
007670 :            8,  VTM  ,C                     .
007671 :            8,XTA,SMALLBUF-C               .
007672 :            8,ARX,UN25-C                   .
007673 :             8,  AAX  ,LMASK12-C            .
007674 :            8,ATX,SMALLBUF-C               .
007675 :            8,XTA,ENDPRISN-C               .
007676 :             ,UZA,NORET                    .
007677 :  11,VTM,RETURN*
007678 :  8,XTA,ADRBIBUF-C
007679 :  8,ARX,CONBIBUF-C
007680 :  8,ATX,ADRBIBUF-C
007681 :             , UJ,PUNCH
007682 :  NORET   :14,VTM,-17
007683 :             ,XTA,                         .
007684 :          :14,UTC,SMALLBUF+18              .
007685 :             ,ATX,                         .
007686 :           14,VLM,*-1                      .
007687 :  8,ATX,FULSM-C
007688 :            8,XTA,BEGSMBUF-C               .
007689 :            8,ATX,ADRSMBUF-C               .
007690 :            8,XTA,ADRBIBUF-C               .
007691 :            8,ARX,CONBIBUF-C               .
007692 :            8,ATX,ADRBIBUF-C               .
007693 :  ,U1A,RETURN*
007694 :            8,ATX,FULBI-C                  .
007695 :  RETURN*:,STX,
007696 :  ,STI,13
007697 :  ,WTC,I11
007698 :  ,UJ,
007699 :  RETURN  :  ,STX,                         .
007700 :             ,ATI,13                       .
007701 :  QUQU    :  ,STX,                         .
007702 :           13, UJ,                         .
007703 :  OUTFULBI: 8,XTA,FULBI-C                  .
007704 :           11,UZA,                         .
007705 :  PUNCH   : 9,VTM,BIGBUF                   .
007706 :             ,WTC,ADRBIBUF                 .
007707 :           10,VTM,-1                       .
007708 :             ,XTA,                           .
007709 :           10,ATX,1                          .
007710 :           10,ATX,2                          .
007711 :            8,*71,INFPUNCH-C               .
007712 :            8,  XTA  ,BEGBIBUF-C            .
007713 :            8,  ATX  ,ADRBIBUF-C            .
007714 :             ,XTA,                         .
007715 :            8,ATX,FULBI-C                  .
007716 :           11, UJ,                         .
007717 :  BEGPOR  : 3,100,                         .
007718 :             ,UTC,SMALLBUF+3               .
007719 :  BEGPOW  : 3,100,                         .
007720 :             ,UTC,SMALLBUF+1               .
007721 :  BEGSMBUF: 4,   ,                         .
007722 :             ,UTC,SMALLBUF+3               .
007723 :  BEGBIBUF:  ,   ,                         .
007724 :             ,UTC,BIGBUF                   .
007725 :          'CONST'
007726 :  ZERO    :  ,OCT  ,                       .
007727 :  NCARD   :  ,OCT  ,1200 0000 0000 0000    .
007728 :  ADRSMBUF:  ,OCT  ,                       .
007729 :  ADRBIBUF:  ,OCT  ,                       .
007730 :  ENDPRISN:  ,OCT  ,                       .
007731 :  SMALLBUF:  ,BSS,19                       .
007732 :  NAME    :  ,BSS,1                        .
007733 :  MASK12  :  ,OCT  ,0000 0000 0000 7777    .
007734 :  CONSMBUF:  ,OCT  ,0100 0000 0000 0001    .
007735 :  CONPO   :  ,OCT  ,0000 0000 0220 0000    .
007736 :  POINTR  :  ,OCT  ,                       .
007737 :  POINTW  :  ,OCT  ,                       .
007738 :  UN1     :  ,OCT  ,0000 0000 0000 0001    .
007739 :   UN25   :   ,  OCT  ,00000001             .
007740 :   LMASK12:   ,  OCT  ,7777777700007777     .
007741 :  CONBIBUF:  ,OCT  ,0100 0000 0000 0030    .
007742 :  INFPUNCH:  ,OCT  ,4410 0000 5010 0000    .
007743 :  WMEM    :  ,OCT,                         .
007744 :  FULBI   :  ,OCT,
007745 :  FULSM:,OCT,
007746 :  I11:,OCT,
007747 :             ,END, SUBRPOUTINE FOR PUNCH ST. AR.
007748 :  STAR*:,NAME,.HOBЫE TP-PЫ
007749 :  FLAGS:,LC,1.DDR
007750 :  MONCARD*:  ,LC ,25
007751 :  MAIN*   : ,LC, 1
007752 :  MONITOR*:  ,SUBP,
007753 :   /VERE/:LC,BLOCK,(2),M/STL,A/CAT,INF0
007754 :  LOADANGO:  ,SUBP,
007755 :  TRNAME*:,LC,1.
007756 :  KCOUNT* :  ,LC,4
007757 :  FULISFL*:  ,LC ,1
007758 :  NOLISFL*:  ,LC ,1
007759 :  LOADLIS*:  ,LC,1
007760 :  PUNCHFL*:  ,LC ,1
007761 :  RM*     :   , LC , 5
007762 :  CHECKFL*:,LC,1
007763 :  PERMCAT:,EQU,=14 0000 0021 0007
007764 :  SAV*47:LC,BLOCK,SAVEFC
007765 :  MONCARD/:,LC,14
007766 :  *REGIM//:,LC,
007767 :  INFLOAD*:LC,BLOCK,ALOAD,INFLOA,INFMD,INFDM
007768 :  STATIC*:,LC,1
007769 :  SYS P:,WEQ,43B
007770 :  NSP:,P/P,(SYSP)16
007771 :  /MONTRAN:,SUBP,
007772 :  S:,EQU,/MONTRAN
007773 :  EXECUT:,SUBP,
007774 :             ,UJ ,CHEKMORE+1
007775 :  PRINT   :14,VTM,MONCARD*
007776 :             ,ITS,14
007777 :           14,UTM,13
007778 :             ,ITS,14
007779 :             ,ITS,
007780 :             ,CALL,PRINT8
007781 :   DISP:4,IFSEQ,D,DD**
007782 :   14,VTM,MONCARD*
007783 :   ,ITA,14.ЗAПИCЬ
007784 :   ,AOX,=7770 0000 7760 0000.B
007785 :   ,*72,4.DAYFILE
007786 :  14,VTM,-13
007787 :  13,VTM,CHEKMORE
007788 :  SS:14,XTA,MONCARD*+13
007789 :  14,ATX,MONCARD/+13
007790 :  14,VLM,SS
007791 :           14,VTM,*NAME-*ENDFILE
007792 :  CHLIST:14,XTA,*ENDFILE
007793 :             ,AEX,MONCARD/
007794 :           14,UZA,ESWITCH
007795 :           14,VLM,CHLIST
007796 :  ILCA:,XTS,TRNSLTOR
007797 :  ,CALL,LOADANGO
007798 :  ,XTA,TRNAME*
007799 :  ,AEX,MDLCARD*
007800 :  ,UIA,CHEKMORE.HE MADLEN3
007801 :  ,XTS,TRNAME*
007802 :  ,CALL,LOADANGO
007803 :  CHEKMORE:  ,CALL,MONREAD*
007804 :             ,XTA,MONCARD*
007805 :  ,AAX,MSMB
007806 :  ,UZA,WRLIB
007807 :  ,AEX,STAR
007808 :             ,U1A,MONITOR*+1
007809 :             ,UJ ,PRINT
007810 :  SETLIST:,BSS,.DDR
007811 :  ,XTS,FLAGS.
007812 :  ,AAX,=7
007813 :  ,AEX,FLAGS.
007814 :  15,AOX,.
007815 :  ,ATX,FLAGS.
007816 :  ,AAX,C1.
007817 :  ,ATX,NOLISFL*.
007818 :  ,XTA,FLAGS.
007819 :  ,ASN,64+2
007820 :  ,AAX,C1.
007821 :  ,ATX,FULISFL*.
007822 :  ,UJ,CHEKMORE.
007823 :  NOLODL:,ATX,LOADLIS*
007824 :             , UJ,CHEKMORE
007825 :  OPT:,BSS,.DDR
007826 :  ,XTS,FLAGS.
007827 :  ,AOX,=100
007828 :  15,AEX,-1.
007829 :  ,STX,FLAGS.
007830 :  ,ASN,64+6.
007831 :  ,ATX,CHECKFL*.
007832 :  ,UJ,CHEKMORE
007833 :  SYSTLODL:,BSS,
007834 :  ,ASN,64-1
007835 :  ,ASN,64+1
007836 :            ,ATX,44B
007837 :             ,UJ ,CHEKMORE
007838 :  WRLIB:,XTS,STWRIT*
007839 :  ,UJ,EDITE
007840 :  SAVBLIST:,XTS,43B
007841 :  ,ASN,64-20
007842 :  ,AOX,=7 0000
007843 :  ,ATX,BIW
007844 :  ,DRUM,BIW
007845 :  ,STX,
007846 :  13,UJ,
007847 :  RETBLIST:,XTA,43B
007848 :  ,ASN,64-20
007849 :  ,AOX,=10 0000 0007 0000
007850 :  ,ATX,BIW
007851 :  ,DRUM,BIW
007852 :  RETBLIS1:,XTA,SAVEFC
007853 :  ,UJ,GLIB1
007854 :  SAVBLIS1:,XTS,47B
007855 :  ,ATX,SAVEFC
007856 :             ,XTA,RM*+4
007857 :             ,STX,47B
007858 :           11, UJ,
007859 :  GLIB:11,VTM,SAVBLIST
007860 :  13,VJM,SAVBLIS1
007861 :  ,CALL,LOADANGO
007862 :  11,VTM,CHEKMORE
007863 :  ,UJ,RETBLIST
007864 :  MAIN:,BSS,
007865 :             ,ATX,MAIN*
007866 :   ,AEX,PRIORCOD
007867 :   ,U1A,CHEKMORE
007868 :  DISP:,IFSEQ,D,DD**
007869 :   ,*63,502B
007870 :   ,ATI,14
007871 :   14,*63,
007872 :   ,ASN,64-6
007873 :   ,YTA,
007874 :   ,ATI,13
007875 :   ,*63,760B
007876 :   ,ATI,14
007877 :   14,*63,
007878 :   15,ATX,
007879 :   ,*63,762B
007880 :   ,AAX,=77777
007881 :   ,NTR,3
007882 :   15,A-X,
007883 :   ,ATI,14
007884 :   ,XTA,=4.***
007885 :   14,UTC,.***
007886 :   13,*52,.***
007887 :   DISP:,ENDIF,
007888 :             , UJ,CHEKMORE
007889 :  READ    :  ,BSS,
007890 :  ,XTS,READVOLK
007891 :  ,CALL,LOADANGO
007892 :  ,UZA,CHEKMORE
007893 :  WRUSE   :  ,BSS,
007894 :  13,VTM,USTEXT
007895 :  ,*64,INFP
007896 :  ,UJ,CHEKMORE
007897 :  EDIT:,XTA,MONCARD/+1
007898 :  ,AEX,=6HDRUM
007899 :  ,UZA,OLD ED.
007900 :  ,XTA,MONCARD/+1
007901 :  ,AEX,=6HTAPE
007902 :  ,UZA,OLD ED.
007903 :  EDIT/81:,XTA,47B.
007904 :  ,ATX,SAVEFC.
007905 :  ,CALL,MONREAD*.
007906 :  ,XTS,EDIT***.
007907 :  EDICA:13,VTM,GIV76.
007908 :  ,UJ,LOADANGO.
007909 :  OLD ED:,BSS,.CTAPЫЙ PEД-TOP
007910 :  11,VTM,MONREAD*.
007911 :  13,VJM,SAVBLIS1.
007912 :  ,XTS,EDIT*
007913 :  ,UJ,EDICA.
007914 :  EDITE:13,VTM,CHEKMORE+1
007915 :  ,UJ,LOADANGO
007916 :  CALL:14,VTM,MONCARD/
007917 :   ,ITA,14
007918 : C  ПPOГPAMMЫ, BЫЗЫBAEMЫE ПO *CALL:
007919 : C  MOЖHO OФOPMЛЯTЬ KAK FUNCTION ... (KAP)
007920 : C  ГДE KAP - TEKCT KAPTЫ BЫЗOBA
007921 : C  (ДЛЯ TEX, KTO HE ЛЮБИT PARAMT*)
007922 :   ,XTS,
007923 :           11,VJM,IDENT6
007924 :             ,CALL,MONREAD*
007925 :  ,XTA,IDI
007926 :   ,CALL,LOADANGO
007927 :   GIV76:,BSS,
007928 :  DISP: 2,IFSEQ,D,DD**
007929 :   ,XTA,= 3053 4576 1634 0112
007930 :   ,*76,1
007931 :   11,VTM,CHEKMORE+1
007932 :             , UJ,RETBLIS1
007933 :  IDENT6:,BSS,
007934 :  12,VTM,-8
007935 :  13,VTM,-10
007936 :  ,XTA,
007937 :  ,ATX,IDI
007938 :  CYCY:13,XTA,MONCARD/+11
007939 :  ,ATX,RR1
007940 :  CYC:,XTA,RR1
007941 :  ,UZA,CYFE
007942 :  ,ASN,64-8
007943 :  ,ATX,RR1
007944 :  ,YTA,
007945 :  ,ATI,14
007946 :  14,UTM,-40B
007947 :  14,VZM,CYC
007948 :  ,ITA,14
007949 :  15,ATX,
007950 :  ,ASN,64+3
007951 :  ,ATI,14
007952 :  ,ASN,64-3
007953 :  15,AEX,
007954 :  ,ATX,RR2
007955 :  14,XTA,IS/TE
007956 :  ,WTC,RR2
007957 :  14,VTM,
007958 :  CYK:14,VZM,CYKI
007959 :  ,ASN,64-6
007960 :  14,UTM,-1
007961 :  ,UJ,CYK
007962 :  CYKI:,ASN,64+42
007963 :  ,UZA,CY
007964 :  ,XTS,IDI
007965 :  ,ASN,64-6
007966 :  15,AOX,
007967 :  ,ATX,IDI
007968 :  12,UTM,1
007969 :  12,VIM,CYC
007970 :  CY:,XTA,IDI
007971 :  ,UZA,WRUSE
007972 :  :,AAX,=:77
007973 :  ,UIA,CYFI
007974 :  ,XTA,IDI
007975 :  ,ASN,64-6
007976 :  ,ATX,IDI
007977 :  ,UJ,CY+1
007978 :  CYFI:,XTA,IDI
007979 :  11,UJ,
007980 :  CYFE :13,VLM,CYCY
007981 :  ,UJ,CY
007982 :  PERMLIB:,ASN,64-20
007983 :  ,AOX,PERMCAT
007984 :  ,ATX,60B
007985 :  11,VTM,CHEKMORE
007986 :  GLIB1:,XTS,43B
007987 :  ,ASN,64-20
007988 :  15,AEX,-1
007989 :  ,AAX,=3700 0000 0000
007990 :  15,AEX,
007991 :  ,ATX,47B
007992 :  11,UJ,
007993 : C
007994 : C     C COXPAHEHИEM OБM.ЛИCTA
007995 :   :,ATX,KCOUNT*+2
007996 :   TAPCOP T:,BSS,
007997 :   11,VTM,LOADANGO
007998 :   13,VJM,SAVBLIS1
007999 :   11,VTM,CHEKMORE
008000 :   ,UJ,RETBLIS1
008001 :  TEMPO:,XTS,*REGIM//
008002 :  ,AAX,=2 00000
008003 :  ,UZA,TEMPO2
008004 :  ,XTA,INFMD
008005 :  ,AAX,=3700 0000 0000
008006 :  ,AOX,=4010 0003 0000 0201
008007 :  TEMPO*:,ATX,INFLOA
008008 :  ,ATX,STATIC*
008009 :  ,ATX,A/CAT
008010 :  ,UZA,TEMPO1
008011 :  ,XTA,INFLOA
008012 :  ,ARX,=100
008013 :  ,ATX,A/CAT
008014 :  ,UJ,TEMPO1
008015 :  TEMPO2:,XTA,*REGIM//
008016 :  ,AAX,=4 00000
008017 :  ,AEX,=4 00000
008018 :  ,UZA,TEMPO*
008019 :  TEMPO1:,XTA,
008020 :  ,STX,S-10B
008021 :  ,ATX,60B
008022 :  ,ATX,47B
008023 :  13,VTM,MONITOR*+1                          -
008024 :  ,UJ,MONREAD*
008025 :  PERMA:,BSS,
008026 :   15,ATX,
008027 :  ,ASN,64-20
008028 :  ,AOX,=0014 0000 0021 0001
008029 :  ,ATX,RR2
008030 :  ,DRUM,RR2
008031 :  ,XTA,
008032 :  ,ATX,A/CAT
008033 :   ,WTC,43B
008034 :   ,XTA,1400B
008035 :  ,AEX,MONAM
008036 :  ,UIA,PERMA1
008037 :  ,XTA,INFMD
008038 :  ,AAX,=3700 0000 0000
008039 :  ,AOX,=4014 0003 0021 0101
008040 :  ,UJ,PERMA2
008041 :  PERMA1:,XTA,
008042 :  PERMA2:,ATX,INFLOA
008043 :  ,XTA,STATIC*
008044 :  ,UZA,PERMA3
008045 :  ,XTA,INFLOA
008046 :  ,ATX,STATIC*
008047 :  PERMA3:,BSS,
008048 :  ,XTA,=1
008049 :  ,STX,KCOUNT*+2
008050 :  ,UJ,PERMLIB
008051 :  DOS:,BSS,
008052 :   ,CALL,MONREAD*
008053 :  ,XTS,DOS/IAE
008054 :  ,CALL,LOADANGO
008055 :  11,VTM,CHEKMORE+1
008056 :  ,UJ,RETBLIS1
008057 :  NEWLIST:,PRINT,INFP
008058 :  ,UJ,CHEKMORE
008059 :  SWITCH  :  ,UJ ,ILCA
008060 :  :,XTA,=4.*FULL LIST
008061 :  ,UJ,SETLIST.
008062 :  :,XTA,C1.*NO LIST
008063 :  ,UJ,SETLIST.
008064 :  :,XTA,=2.
008065 :  ,UJ,SETLIST.*STANDARD
008066 :  :,XTA,C1
008067 :  ,UJ,NOLODL
008068 :  :,XTA,44B
008069 :  ,UJ,SYSTLODL
008070 :  :,XTA,C1
008071 :  :,ATX,PUNCHFL*
008072 :  ,UJ,CHEKMORE
008073 :  :13,VTM,EXECUT
008074 :  ,UJ,MONREAD*
008075 :  :,XTS,LIBDRUM
008076 :  ,UJ,GLIB
008077 :  :,XTS,LIBTAPE
008078 :  ,UJ,GLIB
008079 :  :,XTS,LIBDELET
008080 :  ,UJ,GLIB
008081 :  :,XTS,LISTLIBT
008082 :  ,UJ,GLIB
008083 :  :,XTS,CATALOG*
008084 :  ,UJ,GLIB
008085 :  :,XTS,LIBRPN*
008086 :  ,UJ,GLIB
008087 :  :11,VTM,MAIN
008088 :  ,UJ,IDENT6
008089 :          :  , UJ,READ
008090 :          :  , UJ,EDIT
008091 :  :,UJ,EDIT/81.*EDIT:<ПAPAMETPЫ>
008092 :  :11,VTM,CALL
008093 :  ,UJ,SAVBLIS1
008094 :  :,XTS,LIBDRUM*
008095 :  ,UJ,GLIB
008096 :  :,XTS,RLIBLIS
008097 :  ,UJ,GLIB
008098 :   :,XTA,43B
008099 :  ,UJ,PERMA
008100 :  :,XTA,RM*+4
008101 :  ,UJ,TEMPO
008102 :  :,UJ,CHEKMORE
008103 :  :,XTS,TAPES*
008104 :  ,UJ,LOADANGO
008105 :  :,XTS,COPYTADR
008106 :  ,UJ,LOADANGO
008107 :  :13,VTM,NL
008108 :  ,UJ,NEWLIST
008109 :  :,XTA,
008110 :  ,UJ,OPT
008111 :  ,XTA,=100.*NO OPT
008112 :  ,UJ,OPT
008113 :  :,XTS,MOVETAPE
008114 :  ,UJ,LOADANGO
008115 :  :,XTS,MOVETAPE
008116 :  ,UJ,LOADANGO
008117 :  :,XTS,TABYNAM*
008118 :  ,UJ,LOADANGO
008119 :  :,XTS,COPYTADR
008120 :  ,UJ,LOADANGO
008121 :  :,UJ,READ
008122 :  :11,VTM,DOS
008123 :  ,UJ,SAVBLIS1
008124 :   :,XTS,FIXDRUM
008125 :   ,UJ,LOADANGO
008126 :  :,XTS,LIBRARY/.*LIBRARY:N1,...
008127 :  ,UJ,TAPCOPT.
008128 :  :,XTS,NEWLIBR/.*NEW LIBRARY:...
008129 :  ,UJ,TAPCOPT.
008130 :  :,XTA,=17 7777.*CHECK
008131 :  :,XTS,SETFTN.*NO CHECK
008132 :   ,UJ ,TAPCOPT
008133 :  :,XTS,SOS/SYS.*SOS
008134 :   ,UJ ,TAPCOPT-1.COXP.OБM.ЛИCT!
008135 :   :,XTS,TAKETD
008136 :   ,UJ,LOADANGO
008137 :  ESWITCH:,CALL,STOP*
008138 :  INFP:13,,
008139 :  13,,
008140 :  :,,
008141 :  8,,
008142 :  NL:,GOST,2H'201''231'
008143 :  *NAME:,BSS,
008144 :  ,ISO,6H*NAME
008145 :  ,ISO,6H*FULL
008146 :  ,ISO,6H*NO LI
008147 :  ,ISO,6H*STAND
008148 :  ,ISO,6H*NO LO
008149 :  ,ISO,6H*SYSTE
008150 :  ,ISO,6H*PUNCH
008151 :  ,ISO,6H*NO PU
008152 :  ,ISO,6H*EXECU
008153 :  ,ISO,6H*PERSO
008154 :  ,ISO,6H*TO PE
008155 :  ,ISO,6H*EXCLU
008156 :  ,ISO,6H*TO CO
008157 :  ,ISO,6H*CATAL
008158 :  ,ISO,6H*LIBPU
008159 :  ,ISO,6H*MAIN
008160 :  ,ISO,6H*READ
008161 :  ,ISO,6H*EDIT
008162 :  ,ISO,6H*EDIT:
008163 :  ,ISO,6H*CALL
008164 :  ,ISO,6H*COMMO
008165 :  ,ISO,6H*LIBLI
008166 :  ,ISO,6H*PERMA
008167 :  ,ISO,6H*TEMPO
008168 :  ,ISO,6H*
008169 :  ,ISO,6H*TAPES
008170 :  ,ISO,6H*COPY
008171 :  ,ISO,6H*1
008172 :  ,ISO,6H*OPTIM
008173 :  ,ISO,6H*NO OP
008174 :  ,ISO,5H*MOVE
008175 :  ,ISO,6H*MOVE:
008176 :  ,ISO,6H*TABLE
008177 :  ,ISO,6H*COPY:
008178 :  ,ISO,6H*READ:
008179 :  ,ISO,6H*DOS
008180 :  ,ISO,6H*FIX D
008181 :  ,ISO,6H*LIBRA.
008182 :  ,ISO,6H*NEW L
008183 :  :,ISO,6H*CHECK
008184 :  ,ISO,6H*NO CH
008185 :  ,ISO,6H*SOS
008186 :   ,ISO,6H*TAKE
008187 :  *ENDFILE:  ,8TEXT,''*END F''
008188 :  USTEXT:,GOST,18H'212''200'+-HE HA MECTE'133''231'
008189 :  STAR    :  ,8TEXT,''*''
008190 :  DOS/IAE:,TEXT,8HDOS/IAE
008191 :  READ VOLK:,TEXT,8HREADVOLK
008192 :  STWRIT* :  ,6TEXT,''STWRIT*''
008193 :  TAPES*:,TEXT,8HTAPES*
008194 :  COPYTADR:,TEXT,8HCOPYTADR
008195 :  MOVETAPE:,TEXT,8HMOVETAPE
008196 :  TABYNAM*:,TEXT,8HTABYNAM*
008197 :   FIXDRUM:,TEXT,8HFIXDRUM
008198 :  LIBRARY/:,TEXT,8HLIBRARY/.
008199 :  NEWLIBR/:,TEXT,8HNEWLIBR/.
008200 :  SETFTN:,TEXT,8HSETFTN
008201 :  SOS/SYS:,TEXT,8HSOS/SYS
008202 :  TRNSLTOR:,TEXT,8HTRNSLTOR
008203 :   TAKETD:,TEXT,8HTAKETAPE
008204 :   PRIORCOD:,TEXT,8HSYSTEM
008205 :  MDLCARD*:,TEXT,8HMDLCARD*
008206 :  C1      :  ,OCT,0000 0000 0000 0001
008207 :  MSMB    :  ,OCT,7760 0000 0000 0000
008208 :  LIBTAPE :   ,6TEXT, ''LIBTAPE ''
008209 :  LIBDRUM :   ,6TEXT, ''LIBDRUM ''
008210 :  LISTLIBT:   ,6TEXT, ''LISTLIBT''
008211 :  CATALOG*:   ,6TEXT, ''CATALOG*''
008212 :  LIBDRUM*:   ,6TEXT, ''LIBDRUM*''
008213 :  LIBDELET:   ,6TEXT, ''LIBDELET''
008214 :  LIBRPN* :   ,6TEXT, ''LIBRPN*''
008215 :  EDIT*:,TEXT,8HSTA*R
008216 :  RLIBLIS:,TEXT,8HRLIBLIS
008217 :  MONAM:,TEXT,8HMONITOR*
008218 :  EDIT***:,TEXT,8HEDIT***
008219 :  IS/TE:,BSS,
008220 :  40,TEXT,8H
008221 :  50,TEXT,8H  *    /
008222 :  60,TEXT,8H01234567
008223 :  70,TEXT,8H89
008224 :  00,TEXT,8H ABCDEFG
008225 :  10,TEXT,8HHIJKLMNO
008226 :  20,TEXT,8HPQRSTUVW
008227 :  30,TEXT,8HXYZ
008228 :  40,TEXT,8HЮ БЦД ФГ
008229 :  50,TEXT,8H ИЙ Л
008230 :  60,TEXT,8HПЯ    Ж
008231 :  70,TEXT,8HЬЫЗШЭЩЧ
008232 :  IDI:,BSS,1
008233 :  BIW:,BSS,1
008234 :  RR1:,BSS,1
008235 :  RR2:,BSS,1
008236 :             ,END,
008237 :   STOP*:,NAME,
008238 :   TAPELIST:LC,BLOCK,(1),SC/UNIT
008239 :   MONITOR*:,SUBP,
008240 :   LOADCALL:,SUBP,
008241 :   JSTOP:,EQU,MONITOR*+4
008242 :   JSTAR:,EQU,MONITOR*+1
008243 :   RM*:,LC,
008244 :   WS:,EQU,1.ЭKOHOMИЯ
008245 : C---
008246 :  *C:13,VTM,JSTOP.HA KOHEЦ
008247 :  ,UJ,B
008248 : C---
008249 :   EXIT:,ENTRY,
008250 :  13,VTM,.
008251 :  B:12,BASE,*C.
008252 :  13,VIM,TO STOP.
008253 :  13,VTM,JSTAR.HA STAR*
008254 :   ,XTA,60B.PEЖИM
008255 :   ,ATX,47B."PERMANENT"
008256 :   ,XTA,44B.
008257 :   ,AOX,=:4."NO LOAD"
008258 :   ,ATX,44B
008259 :  TO STOP:,BSS,.
008260 :   ,XTA,17B
008261 :   ,ATX,WS+1
008262 :   ,XTA,RM*+2.BOCCT.
008263 :   ,ATI,15.MAГAЗИHA
008264 :   ,ARX,INF67
008265 :   ,ATX,WS
008266 :   ,XTA,M67
008267 :   ,CTX,17B.HAДO
008268 :   ,UJ,17B.TAK
008269 : C---
008270 :  M67:,*67,WS.
008271 :   INF67:,Z00,RET67.BOCCT.
008272 :   8,Z37,-1.ЗAЩИTЫ
008273 :   RET67:,XTA,WS+1
008274 :   ,ATX,17B
008275 : C---
008276 :  DISP: 2,IFSEQ,D,DD**
008277 :   ,XTA,=3053 4576 1634 0112
008278 :   ,*76,1.PAЗPEШEHИE
008279 : C---
008280 :   ,XTA,SC/UNIT.ДOCБPOC
008281 :   ,AAX,=:77777 00000 2
008282 :   13,UZA,.HE HYЖEH
008283 :   ,XTS,
008284 :   ,XTS,F NAME
008285 :   ,XTS,
008286 :   ,UJ,LOADCALL
008287 :   F NAME:,TEXT,8HFMRTAPE
008288 :   ,END,
008289 :  STPUNCH*:  ,NAME,
008290 :  ENTRY*  :  ,LC ,1
008291 :   STAPUNCH:,SUBP,
008292 :   STAPUNCH:I,BLOCK,PERCOSY1,PERSTAN1
008293 :   ,CONT,PERCOSY2(0),PERSTAN2(0),PERSTANE
008294 :   ,CONT,PERCOSY3(0),PERSTAN3
008295 :  BEGIN   :  ,BSS,
008296 :             ,ITA,13
008297 :             ,ITS,7
008298 :             ,ITS,6
008299 :             ,ITS,5
008300 :             ,ITS,4
008301 :             ,XTS,
008302 :            7,VTM,C
008303 :             ,*70,46B
008304 :            6,VTM,ENTRY*
008305 :            6,XTS,                         .
008306 :   13,VJM,PERSTAN1
008307 :  LOOPE   : 6,XTA,
008308 :             ,UZA,ELOOPE
008309 :            6,XTS,
008310 :   13,VJM,PERSTAN2
008311 :            6,XTS,1
008312 :   13,VJM,PERSTAN2
008313 :            6,UTM,2
008314 :             , UJ,LOOPE
008315 :  ELOOPE  :  ,XTA,43B
008316 :             ,ASN,64-20
008317 :            6,AOX,1
008318 :            7,ATX,DINF-C
008319 :            7,*70,DINF-C
008320 :             ,XTA,
008321 :            7,ATX,SSTR-C
008322 :            6,VTM,-9
008323 :  LOOPST  :  ,WTC,43B                        .
008324 :            6,XTA,9
008325 :            6,UTM,3
008326 :            6,VZM,LOOPST1
008327 :            7,ARX,SSTR-C
008328 :            7,ATX,SSTR-C
008329 :  LOOPST1 : 6,UTM,-3
008330 :            6,VLM,LOOPST
008331 :             ,WTC,SSTR
008332 :            6,VTM,10
008333 :  LOOP1   : 5,VTM,-1023
008334 :             ,WTC,43B
008335 :            4,VTM,
008336 :  LOOP2   : 6,VZM,END                        .
008337 :            6,UTM,-1                         .
008338 :            4,XTS,                           .
008339 :   13,VJM,PERSTAN2
008340 :            4,UTM,1                          .
008341 :            5,VLM,LOOP2                      .
008342 :            7,XTA,DINF-C                     .
008343 :            7,ARX,UN1-C                      .
008344 :            7,ATX,DINF-C                     .
008345 :            7,*70,DINF-C                     .
008346 :             , UJ,LOOP1                      .
008347 :  END     :  ,BSS,                         .
008348 :   13,VJM,PERSTAN3
008349 :             ,*70,45B                        .
008350 :   ,ATX,
008351 :             ,STX,                           .
008352 :             ,STI,4                          .
008353 :             ,STI,5                          .
008354 :             ,STI,6                          .
008355 :             ,STI,7                          .
008356 :             ,STI,13                         .
008357 :           13, UJ,                           .
008358 :  C       :  ,BSS,                           .
008359 :  UN1     :  ,LOG,1                          .
008360 :  DINF    :  ,OCT,                           .
008361 :  SSTR    :  ,OCT,                           .
008362 :             ,END, PUNCH FOR STANDART ARRAY
008363 :  STWRIT* :  ,NAME,
008364 :  STRBFF* :  ,PC ,1024
008365 :  KCOUNT* :  ,LC ,4
008366 :  MONCARD*:  ,LC ,25                       .
008367 :  WRITLIBR:  ,SUBP,
008368 :  RM*     :  ,LC ,5
008369 :   LOADLIS*:,LC,1
008370 :  C       :  ,BSS,                         .
008371 :             ,ITS,13
008372 :             ,ITS,7
008373 :            7,VTM,C
008374 :             ,XTS,KCOUNT*+3
008375 :            7,ATX,R-C
008376 :             ,XTA,47B
008377 :            7,ATX,SAVE-C
008378 :             ,XTA,RM*+4
008379 :             ,ATX,47B
008380 :           14,VTM,STRBFF*
008381 :             ,ITA,14
008382 :             ,ATX,KCOUNT*+3
008383 :             ,ATX,KCOUNT*+2
008384 :  REPEAT  :  ,XTA,MONCARD*
008385 :            7,ATX,COUNT-C
008386 :            7,AAX,MSMB-C
008387 :             ,U1A,EXIT
008388 :             ,XTA,MONCARD*
008389 :            7,AEX,CSTAND-C
008390 :             ,U1A,ERROR
008391 :   ,ATX,MCT
008392 :           14,VTM,READWORD
008393 :             ,ITA,14
008394 :   ,ATX,REGM
008395 :           13,VJM,WRITLIBR+1
008396 :             ,CALL,MONREAD*
008397 :   ,XTA,LOADLIS*
008398 :   ,U1A,REPEAT
008399 :    ,XTS,TA
008400 :   14,VTM,TAB
008401 :   ,ITS,14
008402 :    ,CALL,PT6/G
008403 :  ,XTA,TAB+1
008404 :    ,AEX,G017
008405 :   ,AOX,G231
008406 :  ,ATX,TAB+1
008407 :   8,VTM,2
008408 :   ,XTA,MCT
008409 :   ,ATI,9
008410 :   9,UTM,-2
008411 :   9,VZM,PRT
008412 :   DECODE:9,XTS,TA
008413 :   8,UTC,TAB
008414 :   14,VTM,
008415 :   ,ITS,14
008416 :    ,CALL,PT6/G
008417 :   8,XTA,TAB+1
008418 :    ,AEX,G017
008419 :   ,AOX,G231
008420 :    8,ATX,TAB+1
008421 :   8,UTM,2
008422 :   9,UTM,-2
008423 :   9,VIM,DECODE
008424 :   PRT:,PRINT,INF
008425 :             ,UJ ,REPEAT
008426 :  READWORD:  ,WTC,COUNT
008427 :           14,VTM,-1
008428 :           14,VZM,NEWCARD
008429 :           14,UTM,2
008430 :             ,ITS,14
008431 :             ,UTC,COUNT
008432 :             ,ATX,
008433 :   ,XTA,REGM
008434 :   ,UZA,NOENTRY
008435 :           14,XTA,MONCARD*+15
008436 :    ,AAX,MSMB
008437 :   ,UZA,SETREGM
008438 :   14,XTA,MONCARD*+15
008439 :   ,WTC,MCT
008440 :   ,ATX,TA
008441 :   ,XTA,MCT
008442 :    ,ARX,C1
008443 :   ,ATX,MCT
008444 :   NOENTRY:14,XTA,MONCARD*+15
008445 :           13,UJ ,
008446 :   SETREGM:,ATX,REGM
008447 :   ,UJ,NOENTRY
008448 :  NEWCARD :  ,ITS,13
008449 :             ,CALL,MONREAD*
008450 :             ,XTS,MONCARD*
008451 :             ,UTC,COUNT
008452 :             ,STX,
008453 :             ,STI,13
008454 :             ,UJ ,READWORD
008455 :  EXIT    : 7,XTA,R-C
008456 :             ,ATX,KCOUNT*+3
008457 :            7,XTA,SAVE-C
008458 :             ,ATX,47B
008459 :             ,STX,KCOUNT*+2
008460 :             ,STI,7
008461 :             ,STI,13
008462 :           13,UJ ,
008463 :  ERROR   :14,VTM,TEXT
008464 :             ,ITS,14
008465 :           14,UTM,4
008466 :             ,ITS,14
008467 :             ,XTS,
008468 :             ,CALL,PRINT8
008469 :             ,CALL,STOP*
008470 :   INF:,UTC,HEAD
008471 :   8,UTC,TAB-2
008472 :    ,    ,
008473 :   ,012,10
008474 :   ,012,
008475 :   8,012,9
008476 :    C1:,LOG,1
008477 :   G017:,LOG,017
008478 :    G231:,LOG,231
008479 :  MSMB    :  ,OCT,776
008480 :  CSTAND  :  ,LOG,77761
008481 :   MCT:,OCT,
008482 :   REGM:,OCT,
008483 :  R       :  ,OCT,
008484 :  COUNT   :  ,OCT,
008485 :  SAVE    :  ,OCT,
008486 :  TEXT    :  ,8TEXT,''ILLEGA''
008487 :          :  ,8TEXT,''L PLAC''
008488 :             ,8TEXT,''E FOR ''
008489 :             ,8TEXT,''BINARY ''
008490 :             ,8TEXT,'' CARD ''
008491 :   HEAD:,GOST,7H*-CM-*'231'
008492 :   TAB:,BSS,40
008493 :   TA:,BSS,40
008494 :             ,END,
008495 :  SUBPERR*:,NAME,
008496 :  PRINT8 :,SUBP ,
008497 :  STOP* : , SUBP ,
008498 :           14 , UJ    , 1
008499 :  PRINT82 :  , ENTRY ,
008500 :  BCDPUN2 :  , ENTRY ,
008501 :  PT8PUN  :  , ENTRY ,
008502 :  PT5PUN  :  , ENTRY ,
008503 :  BINPUN  :   , ENTRY ,
008504 :  PT8BNP  :   , ENTRY ,
008505 :  PT5BNP  :   , ENTRY ,
008506 :  TTPRINT :   , ENTRY ,
008507 :  14, VTM, TXT
008508 :  , ITS , 14
008509 :  14, UTM , 7
008510 :  , ITS , 14
008511 :  ,XTS ,
008512 :  , CALL , PRINT8
008513 :  , UJ,STOP*
008514 :  TXT : , ISO, 30H ERROR IN IN OUT-PUT
008515 :  ,END,
008516 :       SYSBACK*    :,NAME,
008517 :       LOADCALL    :,SUBP,
008518 :       RM*         :,LC, 00003B
008519 :  GAK/3:,EQU,RM*           +   00002B
008520 :                         ,WTC,  GAK/3
008521 :                   15    ,VTM, 77777B
008522 :                         ,ITA ,15
008523 :                         ,UTC,      *00007B
008524 :                         ,AOX ,
008525 :                         ,UTC,      *00007B
008526 :                         ,CTX ,
008527 :                   15    ,UTM, 00001B
008528 :                   12    ,XTS ,
008529 :                         ,XTS ,
008530 :                         ,UTC,      *00007B
008531 :                         ,*67 ,
008532 :      *00007B:            ,Z00,LOADCALL
008533 :                   9     ,ATX ,
008534 :                   ,END,
008535 :  TABYNAM*:,NAME,.VOLKOV A.I.
008536 :  MONITOR*:,SUBP,
008537 :  PRINT8:,SUBP,
008538 :  RM*:,LC,5
008539 :  ENTRY*:,LC,4
008540 :  MONCARD*:,LC,14
008541 :  MONCARD/:,LC,14
008542 :  OUT*OUT:,LC,1
008543 :  *MONCRD*:,LC,12
008544 :  A//:,PC,1024
008545 :  MIST10*:LC,BLOCK,MIS,CODE,NUMB
008546 :  1,BASE,*
008547 :  13,MTJ,7
008548 :  6,VTM,
008549 :  3,VTM,ENTRY*
008550 :  ,ITA,3
008551 :  ,UTC,OUT*OUT
008552 :  ,ATX,
008553 :  5,VJM,CARD0
008554 :  11,UTM,-072B
008555 :  11,VZM,C
008556 :  5,VJM,PLOXOG
008557 :  C:4,V1M,C1
008558 :  C0:5,VTM,FIN
008559 :  ,UJ,PLOXOG
008560 :  C1:10,VTM,3
008561 :  ,CALL,PARAMT*
008562 :  11,MTJ,4
008563 :  14,VZM,C2
008564 :  C1/:5,VTM,C3
008565 :  ,UJ,PLOXID
008566 :  C2:12,VZM,C1/
008567 :  C3:4,VZM,C0
008568 :  ,XTA,
008569 :  ,ATX,*MONCRD*+2
008570 :  14,VTM,*MONCRD*
008571 :  ,ITS,14
008572 :  14,VTM,PLI1
008573 :  ,ITS,14
008574 :  ,CALL,ISO*TEX*
008575 :  ,XTA,PLI1
008576 :  3,ATX,
008577 :  ,XTA,=:2
008578 :  3,ATX,1
008579 :  3,UTM,2
008580 :  4,MTJ,11
008581 :  11,UTM,-54B.,
008582 :  11,VZM,C1
008583 :  4,MTJ,11
008584 :  11,UTM,-050B.(
008585 :  11,V1M,C0
008586 :  ,XTA,
008587 :  3,ATX,
008588 :  ,XTA,=0010 0000 0010 0000
008589 :  3,ATX,1
008590 :  14,VTM,-9
008591 :  2,VTM,A//
008592 :  ,XTA,
008593 :  CY1:2,ATX,
008594 :  2,UTM,1
008595 :  14,VLM,CY1
008596 :  14,VTM,A//
008597 :  ,XTA,=1
008598 :  14,ATX,
008599 :  14,ATX,7
008600 :  CYKL:4,V1M,CY
008601 :  5,VJM,CARD
008602 :  4,V1M,CY
008603 :  CYK:5,VTM,CYKL
008604 :  ,UJ,PLOXOG
008605 :  CY:10,VTM,3
008606 :  ,CALL,PARAMT*
008607 :  11,MTJ,4
008608 :  14,VZM,CY2
008609 :  CY/:5,VTM,CY3
008610 :  ,UJ,PLOXID
008611 :  CY2:12,V1M,CY3
008612 :  4,VZM,CYKL
008613 :  ,UJ,CY/
008614 :  CY3:,XTA,
008615 :  ,ATX,*MONCRD*+2
008616 :  14,VTM,*MONCRD*
008617 :  ,ITS,14
008618 :  14,VTM,PLI1
008619 :  ,ITS,14
008620 :  ,CALL,ISO*TEX*
008621 :  ,XTA,PLI1
008622 :  2,ATX,
008623 :  2,UTM,1
008624 :  14,VTM,A//+7
008625 :  14,XTA,
008626 :  ,ARX,=1
008627 :  14,ATX,
008628 :  4,MTJ,11
008629 :  11,UTM,-054B.,
008630 :  11,VZM,CYKL
008631 :  4,MTJ,11
008632 :  11,UTM,-051B.)
008633 :  11,VZM,FIN
008634 :  4,VZM,CYKL
008635 :  ,UJ,CYK
008636 :  FIN:,XTA,.,OCT,
008637 :  2,ATX,
008638 :  ,XTA,ENTRY*
008639 :  2,ATX,1.NAME
008640 :  ,CALL,MONREAD*
008641 :  7,UTC,
008642 :  6,V1M,
008643 :  14,VTM,A//
008644 :  ,ITA,14
008645 :  ,ASN,64+10
008646 :  ,ASN,64-30
008647 :  ,AOX,=0000 0000 0010 0000
008648 :  ,ATX,PLI1
008649 :  ,DRUM,PLI1
008650 :  FIN1:,UJ,MONITOR*+2
008651 :  CARD:,CALL,MONREAD*
008652 :  14,VTM,MONCARD*
008653 :  ,ITS,14
008654 :  14,UTM,13
008655 :  ,ITS,14
008656 :  ,ITS,
008657 :  ,CALL,PRINT8
008658 :  14,VTM,-13
008659 :  SS:14,XTA,MONCARD*+13
008660 :  14,ATX,MONCARD/+13
008661 :  14,VLM,SS
008662 :  10,VTM,9
008663 :  ,UJ,CARD0/
008664 :  CARD0:10,VTM,
008665 :  CARD0/:,BSS,
008666 :  ,CALL,PARAMT*
008667 :  11,MTJ,4
008668 :  5,UJ,
008669 :  PRINTE:,ITS,14
008670 :  ,ITS,12
008671 :  14,VTM,10
008672 :  ,ITS,14
008673 :  ,UJ,PRINT8
008674 :  PLOXID:14,VTM,PLI
008675 :  :14,MTJ,12
008676 :  12,UTM,5
008677 :  ,XTA,
008678 :  ,UTC,MIS
008679 :  ,ATX,
008680 :  ,XTA,*MONCRD*
008681 :  12,ATX,-1
008682 :  ,XTA,*MONCRD*+1
008683 :  12,ATX,
008684 :  13,VJM,PRINTE
008685 :  6,UTM,1
008686 :  6,MTJ,14
008687 :  14,UTM,-20
008688 :  5,UTC,
008689 :  14,V1M,
008690 :  14,VTM,MNO
008691 :  12,VTM,MNO+2
008692 :  13,VJM,PRINTE
008693 :  ,SJ,
008694 :  PLOXOG:14,VTM,PLO
008695 :  ,UJ,PLOXID+1
008696 :  MNO:,ISO,18H---MHOГO OШИБOK---
008697 :  PLO:,ISO,24HПЛOXOЙ OГPAHИЧИTEЛЬ ЗA
008698 :  ,BSS,2
008699 :  PLI:,ISO,24HПЛOXOЙ ИДEHTИФИKATOP
008700 :  PLI1:,BSS,3
008701 :  ,END,
008702 :  TAN:,NAME,.
008703 :  ,ATI,14.
008704 :  ,NTR,2.
008705 :  14,XTS,.
008706 :  ,*51,1.COS
008707 :  15,WTC,.
008708 :  ,XTS,.
008709 :  ,*51,.
008710 :  15,A/X,
008711 :  13,UJ,.
008712 :  ASIN:,ENTRY,.
008713 :  ASIN:,ATI,14
008714 :  14,XTA,
008715 :  ,*54,
008716 :  13,UJ,.
008717 :  ACOS:,ENTRY,.
008718 :  ,ATI,14.
008719 :  14,XTA,.
008720 :  ,*54,
008721 :  ,UTC,=R1.570796326795
008722 :  ,X-A,
008723 :  13,UJ,.
008724 :  ,END,TAN
008725 :       TANH        :,NAME,
008726 :                         ,ATI ,8
008727 :                   9     ,VTM,      *00013B
008728 :                   8     ,XTA ,
008729 :                         ,AMX ,
008730 :                   9     ,A-X ,00001B
008731 :                         ,UZA,      *00011B
008732 :                   8     ,XTA ,
008733 :                         ,*56 ,
008734 :                   9     ,ATX ,00002B
008735 :                   9     ,XTA ,
008736 :                   9     ,A/X ,00002B
008737 :                   9     ,ATX ,00003B
008738 :                   9     ,A+X ,00002B
008739 :                   9     ,ATX ,00004B
008740 :                   9     ,XTA ,00002B
008741 :                   9     ,A-X ,00003B
008742 :                   9     ,A/X ,00004B
008743 :                   13    ,UJ,  00000B
008744 :      *00011B:     9     ,XTA ,
008745 :                   8     ,AVX ,
008746 :                   13    ,UJ,  00000B
008747 :                         ,UTC, 00000B
008748 : CONST
008749 :      *00013B:,OCT,4050  0000  0000  0000
008750 :      *00014B:,OCT,4217  0000  0000  0000
008751 :      *00015B:,OCT,0000  0000  0000  0000
008752 :      *00016B:,OCT,0000  0000  0000  0000
008753 :      *00017B:,OCT,0000  0000  0000  0000
008754 :                   ,END,
008755 :  TAPES*:,NAME,
008756 :  STOP*:,SUBP,
008757 :  PARAMT*:,SUBP,
008758 :  OUT*OUT:,LC,1
008759 :  A/T MT:,EQU,504B
008760 :  A/Г Y C:,EQU,502B
008761 :  *C:,ITS,13
008762 :  ,ITS,7
008763 :  ,ITS,6
008764 :  ,ITS,
008765 :  7,BASE,*C
008766 :  DISP:,IFSEQ,D,DD**
008767 :  ,*63,A/Г Y C
008768 :  ,ATI,14
008769 :  14,*63,
008770 :  ,ASN,64+42
008771 :  ,ASN,64-42.BЫД. ДHЗ
008772 :  ,ATX,Д H З
008773 :  ,*63,A/T MT
008774 :  ,ATX,TMT
008775 :  ,XTA,=10 0000
008776 :  ,UTC,OUT*OUT
008777 :  ,ATX,
008778 :  ,MTJ,10
008779 :  13,VJM,PARAMT*
008780 :  11,VZM,ERR0
008781 :  9,VTM,YK1
008782 :  10,VTM,4
008783 :  13,VJM,PARAMT*
008784 :  14,VIM,ERR0
008785 :  ,ARX,=10 0000
008786 :  ,AEX,=40 0000
008787 :  ,AAX,=40 4000
008788 :  ,UIA,ERR0
008789 :  9,VTM,YK1+1
008790 :  10,VTM,4
008791 :  13,VJM,PARAMT*
008792 :  ,UZA,DELT
008793 :  ,ARX,=10 0000
008794 :  ,AEX,=40 0000
008795 :  ,AAX,=40 4000
008796 :  ,UIA,ERR0
008797 :  6,VTM,-1
008798 :  *5:,BSS,
008799 :  6,UTC,YK1+1
008800 :  ,XTA,
008801 :  13,VJM,SER MT.ИЩEM MЛ.
008802 :  ,ITA,12
008803 :  6,UTC,P+1
008804 :  ,ATX,
008805 :  6,VLM,*5
008806 :  ,XTA,YK1
008807 :  ,AEX,YK1+1
008808 :  ,ATX,P+2
008809 :  ,XTA,P
008810 :  ,AOX,P+1
008811 :  ,UZA,ERR2.OБE MЛ OTC.
008812 :  6,VTM,-1
008813 :  *666:6,UTC,P+1
008814 :  ,XTA,
008815 :  ,UZA,MЛ HET
008816 :  ,ATI,12
008817 :  6,UTC,YK1+1
008818 :  ,XTA,
008819 :  ,AEX,P+2
008820 :  13,VJM,PUTMT
008821 :  MЛ HET:6,VLM,*666
008822 :  RET:,STI,.BOЗBPAT
008823 :  ,STI,6
008824 :  ,STI,7
008825 :  ,STI,13
008826 :  13,UJ,
008827 :  DELT:,XTA,YK1
008828 :  13,VJM,SER MT
008829 :  ,UIA,ERR1
008830 :  ,XTA,YK1
008831 :  13,VTM,RET
008832 :  ,UJ,PUT MT
008833 :  SER MT:12,VTM,-48
008834 :  ,APX,=77 0000
008835 :  ,ASN,64+6
008836 :  ,AOX,Д H З
008837 :  ,ATX,ET
008838 :  *11:,WTC,TMT
008839 :  12,*63,32
008840 :  ,AEX,ET
008841 :  ,ASN,64+36
008842 :  13,UZA,
008843 :  12,UTM,1
008844 :  12,VIM,*11
008845 :  13,UJ,
008846 :  PUT MT:,APX,=77 3777
008847 :  ,AUX,=:0077 0000 3777
008848 :  15,ATX,
008849 :  ,WTC,TMT
008850 :  12,UTM,32
008851 :  12,*63,
008852 :  ,AAX,=7700 7777 0000 7777
008853 :  15,AOX,
008854 :  ,*76,FIS
008855 :  FIS:12,ATX,
008856 :  13,MTJ,32B
008857 :  ,MTJ,27B
008858 :  2,IJ,.ЗAПИCAЛИ ДEЛЬTY
008859 :  DISP:  ,ENDIF,
008860 :  ERR0:6,VTM,ERR
008861 :  ,UJ,PRINT
008862 :  ERR1:6,VTM,MЛA
008863 :  ,UJ,PRINT
008864 :  ERR2:6,VTM,MЛ2
008865 :  PRINT:,*64,IP
008866 :  ,UJ,STOP*
008867 :  IP:6,,
008868 :  6,,
008869 :  :,001,
008870 :  8,,
008871 :  MЛ2:,GOST,24HOБE MЛ OTCYTCTBYЮT '133''231'
008872 :  MЛA:,GOST,18HMЛ OTCYTCTBYET '133''231'
008873 :  ERR:,GOST,24HOШИБ. YПPABЛ. KAPTA '133''231'
008874 :  Д H З:,BSS,1
008875 :  TMT:,BSS,1
008876 :  YK1:,BSS,2
008877 :  P:,BSS,3
008878 :  ET:,BSS,1
008879 :  ,END,TAPES*
008880 :  TERIN:,NAME,
008881 : C
008882 : C**************** E.Д.ФEДЮHЬKИH 10/05/79
008883 : C*
008884 : C*   SUBROUTINE TERIN(TEXT,N)
008885 : C*
008886 : C*   BBOДИT B MACCИB TEXT CTPOKY C ЭKPA-
008887 : C*   HA ДИCПЛEЯ. B N ЗACЫЛAETCЯ ЧИCЛO
008888 : C*   BBEДEHHЫX CЛOB. CTPOKA B MACCИBE
008889 : C*   TEXT ДOПOЛHЯETCЯ ПPOБEЛAMИ ДO ЦEЛO-
008890 : C*   ГO ЧИCЛA CЛOB.
008891 : C*
008892 : C*   OБPAЩEHИE CTAHДAPTHOE. ИCПOЛЬЗYЮTCЯ
008893 : C*   БEЗ BOCCTAHOBЛEHИЯ PEГИCTPЫ 10-12 И
008894 : C*   14. PEЖИM HOPMAЛИЗAЦИИ И OKPYГЛEHИЯ
008895 : C*   HE MEHЯETCЯ. HA BЫXOДE - ЛOГИЧECKAЯ
008896 : C*   ГPYППA.
008897 : C*
008898 : C
008899 :  15,MTJ,11
008900 :  ,ITS,
008901 :  11,ATX,24
008902 :  12,VTM,377B
008903 :  10,VTM,INF
008904 :  10,*71,
008905 :  :11,WTC,-1
008906 :  15,VTM,
008907 :  ,ITA,12
008908 :  11,ATX,-1
008909 :  11,MTJ,12
008910 :  14,VTM,
008911 :  :12,XTA,1
008912 :  12,UTM,1
008913 :  ,UZA,CONT
008914 :  15,ATX,
008915 :  11,AAX,-1
008916 :  14,UTM,1
008917 :  ,U1A,*-3
008918 :  15,XTA,-1
008919 :  10,ARX,2
008920 :  15,AEX,-1
008921 :  ,ASN,64+3
008922 :  10,AAX,1
008923 :  15,AEX,
008924 :  15,ATX,
008925 :  CONT:,ITA,14
008926 :  10,AEX,3
008927 :  11,UTM,-1
008928 :  11,MTJ,15
008929 :  11,WTC,1
008930 :  ,ATX,
008931 :  15,XTA,
008932 :  13,UJ,
008933 :  INF:11,Z06,1
008934 :  ,,
008935 :  ,ISO,6H
008936 :  ,OCT,7777777777777776
008937 :  ,OCT,64
008938 :  ,END,
008939 :  TEROUT:,NAME,
008940 : C
008941 : C**************** E.Д.ФEДЮHЬKИH 10/05/79
008942 : C*
008943 : C*   SUBROUTINE TEROUT(TEXT,N)
008944 : C*
008945 : C*   BЫBOДИT HA ДИCПЛEЙ N CЛOB MACCИBA
008946 : C*   TEXT. XBOCTOBЫE ПPOБEЛЫ ИГHOPИPY-
008947 : C*   ЮTCЯ.
008948 : C*
008949 : C*   OБPAЩEHИE CTAHДAPTHOE. ИCПOЛЬЗYЮTCЯ
008950 : C*   БEЗ BOCCTAHOBЛEHИЯ 11,12 И 14 PEГИ-
008951 : C*   CTPЫ. PEЖИM HOPMAЛИЗAЦИИ И OKPYГЛE-
008952 : C*   HИЯ HE MEHЯETCЯ. HA BЫXOДE - ЛOГИ-
008953 : C*   ЧECKAЯ ГPYППA.
008954 : C*
008955 : C
008956 :  ,STI,14
008957 :  ,ATI,12
008958 :  14,WTC,
008959 :  14,VTM,
008960 :  15,MTJ,11
008961 :  14,VZM,ZERO
008962 :  :12,XTS,
008963 :  14,UTM,-1
008964 :  12,UTM,1
008965 :  14,V1M,*-1
008966 :  15,ATX,
008967 :  14,VTM,INF
008968 :  :14,XTA,1
008969 :  15,AEX,
008970 :  ,UZA,*-1
008971 :  15,ATX,1
008972 :  14,ARX,2
008973 :  15,AEX,1
008974 :  ,ASN,64+3
008975 :  14,AAX,1
008976 :  15,UTM,1
008977 :  15,AEX,
008978 :  CONT:,XTS,
008979 :  15,ATX,
008980 :  11,MTJ,15
008981 :  14,*71,
008982 :  :15,XTA,
008983 :  13,UJ,
008984 :   ZERO:14,VJM,CONT
008985 :  INF:11,Z04,1
008986 :  ,,
008987 :  ,ISO,6H
008988 :  ,OCT,7777777777777776
008989 :  ,END,
008990 :       TRANS8/6    :,NAME,
008991 :      *00000B:           ,XTS ,
008992 :                         ,ITS ,1
008993 :                         ,ITS ,2
008994 :                         ,ITS ,3
008995 :                   15    ,ATX ,
008996 :                   14    ,VTM, 00000B
008997 :                   2     ,VTM,      *00000B
008998 :                         ,UTC, 00000B
008999 :      *00004B:     15    ,XTA ,77773B
009000 :                         ,UTC,      *00024B
009001 :                   14    ,ASX ,
009002 :                   2     ,AAX ,00063B
009003 :                         ,UZA,      *00017B
009004 :                         ,ATI ,1
009005 :                         ,ASN ,67
009006 :                         ,ATI ,3
009007 :                         ,UTC,      *00042B
009008 :                   3     ,XTA ,
009009 :                         ,ITS ,1
009010 :                         ,ASN ,19
009011 :                         ,ASN ,109
009012 :                         ,STI ,3
009013 :                         ,UTC,      *00032B
009014 :                   3     ,ASX ,
009015 :                   2     ,AAX ,00062B
009016 :                         ,UTC,      *00042B
009017 :                   14    ,ASX ,
009018 :                   15    ,AOX ,77774B
009019 :                   15    ,ATX ,77774B
009020 :                         ,UTC, 00000B
009021 :      *00017B:     14    ,UTM, 77773B
009022 :                   14    ,VZM,      *00021B
009023 :                   14    ,UTM, 00006B
009024 :                         ,UJ,       *00004B
009025 :      *00021B:     15    ,XTA ,
009026 :                         ,STI ,3
009027 :                         ,STI ,2
009028 :                         ,STI ,1
009029 :                   15    ,UTM, 77777B
009030 :                   13    ,UJ,  00000B
009031 : CONST
009032 :      *00024B:,OCT,4000  0000  0000  0000
009033 :      *00025B:,OCT,4400  0000  0000  0000
009034 :      *00026B:,OCT,5000  0000  0000  0000
009035 :      *00027B:,OCT,5400  0000  0000  0000
009036 :      *00030B:,OCT,6000  0000  0000  0000
009037 :      *00031B:,OCT,6400  0000  0000  0000
009038 :      *00032B:,OCT,4000  0000  0000  0000
009039 :      *00033B:,OCT,4300  0000  0000  0000
009040 :      *00034B:,OCT,4600  0000  0000  0000
009041 :      *00035B:,OCT,5100  0000  0000  0000
009042 :      *00036B:,OCT,5400  0000  0000  0000
009043 :      *00037B:,OCT,5700  0000  0000  0000
009044 :      *00040B:,OCT,6200  0000  0000  0000
009045 :      *00041B:,OCT,6500  0000  0000  0000
009046 :      *00042B:,OCT,3200  0000  0000  0000
009047 :      *00043B:,OCT,2700  0000  0000  0000
009048 :      *00044B:,OCT,2400  0000  0000  0000
009049 :      *00045B:,OCT,2100  0000  0000  0000
009050 :      *00046B:,OCT,1600  0000  0000  0000
009051 :      *00047B:,OCT,1300  0000  0000  0000
009052 :      *00050B:,OCT,2726  2524  2322  2120
009053 :   ,LOG,3130
009054 :      *00052B:,OCT,4746  4544  4342  4100
009055 :      *00053B:,OCT,5756  5554  5352  5150
009056 :      *00054B:,OCT,6766  6564  6362  6160
009057 :   ,LOG,72 7170
009058 :   ,TEXT,8HГФEДЦБAЮ.147-140
009059 :   ,TEXT,8HOHMЛKЙИX.157-150
009060 :   ,TEXT,8HBЖYTCPЯП.167-160
009061 :   ,TEXT,8H ЧЩЭШЗЫЬ.177-170
009062 :      *00062B:,OCT,0000  0000  0000  0077
009063 :      *00063B:,OCT,0000  0000  0000  0377
009064 :                   ,END,
009065 :  TRNSLTOR:,NAME,.01.03.80
009066 : C*
009067 : C*****   ЗAKAЗ ЯЗЫKOB ПPOГPAMMИPOBAHИЯ
009068 : C*****   HACTPOЙKA HA TPAHCЛЯTOPЫ
009069 : C*
009070 : C*       16.12.83.  Д.CEPKИH
009071 : C*   ПPИ ПOMOЩИ YK *TRANS ПPOИЗBOДИTCЯ ПOДKЛЮЧEHИE
009072 : C*   ДOП.TPAHCЛЯTOPOB K CИCTEME.
009073 : C*   YK *TRANS ИMEET BИД:
009074 : C*      *TRANS<PAЗД-ЛЬ1><ИMЯ TP-PA><PAЗД-ЛЬ2>[<YK.MЛ>]
009075 : C*   PAЗДEЛИTEЛЬ1 OПPEДEЛЯET ПOPЯДOK
009076 : C*   ПPOCMOTPA CTATИЧECKИX БИБЛИOTEK,
009077 : C*   MOЖET БЫTЬ  -  ИЛИ  :
009078 : C*      :  CHAЧAЛA HYЖHЫЙ PAЗДEЛ<ИMЯ TP-PA>
009079 : C*   ИЩETCЯ B OCHOBHOЙ CTATИKE,ЗATEM
009080 : C*   B ДOПOЛHИTEЛЬHOЙ,ЗATEM B ЛИЧHOM
009081 : C*   OVERLAY ПO AДPECY<YK.MЛ>;
009082 : C*      -  ПOИCK HYЖHOГO PAЗДEЛA ПPOИCXOДИT
009083 : C*   B ПOPЯДKE,OБPATHOM ПPEДЫДYЩEMY.
009084 : C*   PAЗДEЛИTEЛЬ2 OПPEДEЛЯET TИП ЛИЧHOЙ
009085 : C*   БИБЛИOTEKИ,ГДE HAXOДИTCЯ TPAHCЛЯTOP,
009086 : C*   MOЖET БЫTЬ  .  ИЛИ  ,
009087 : C*      ,  CTATИЧECKAЯ БИБЛИOTEKA;
009088 : C*      .  BPEMEHHAЯ БИБЛИOTEKA(ДИHAMИЧECKAЯ).
009089 : C*   B ПOCЛEДHEM CЛYЧAE <PAЗДEЛИTEЛЬ1>
009090 : C*   MOЖET БЫTЬ ЛЮБЫM ИЗ ПEPEЧИCЛEHHЫX.
009091 : C*           П P И M E P Ы :
009092 : C*           ===============
009093 : C*   *TRANS:FORTRAN  -  TP-P C ИMEHEM
009094 : C*   ГOЛOBHOГO PAЗДEЛA "FORTRAN" BЫБEPETCЯ
009095 : C*   ИЗ OCHOBHOЙ CTATИKИ.
009096 : C*   *TRANS-FORTRAN  -  TOT ЖE TP-P
009097 : C*   BЫБEPETCЯ ИЗ ДOП.CTATИKИ.
009098 : C*   *TRANS-FORTRAN,67200  -  TP-P BЫБEPETCЯ
009099 : C*   ИЗ OVERLAY,HAXOДЯЩEЙCЯ HA HOCИTEЛE,
009100 : C*   ЗAKAЗAHHOM ПOД MAT.HOMEPOM 67 C 200-Й ЗOHЫ.
009101 : C*   *TRANS:FORTRAN.  -  TP-P BЫБEPETCЯ
009102 : C*   ИЗ BPEMEHHOЙ БИБЛИOTEKИ И ЗAГPYЗИTCЯ
009103 : C*   ДИHAMИЧECKИ.
009104 : C*
009105 : C*
009106 :  /TRANS/:,SUBP,
009107 :  INFLOAD*:LC,BLOCK,(1),INFLOA
009108 :  STATIC*:,LC,
009109 :  MONITOR*:,SUBP,
009110 :  ENTRAN:,EQU,MONITOR*+1
009111 :  TR*CALL:,SUBP,
009112 :  /VERE/:LC,BLOCK,SAVL,S/CALL
009113 :  ,CONT,(2),INF0
009114 :  SAV/47:LC,BLOCK,SAV47.
009115 :  MONCARD*:LC,BLOCK,MC*
009116 :  TRNAME*:,LC,
009117 :  RM*:,LC,
009118 :  LHEAP*:LC,BLOCK,FLG
009119 :  M1000:,EQU,=2 00000
009120 :  OUT*OUT:,LC,1
009121 :  MINUS:,EQU,55B
009122 :  POINT:,EQU,56B
009123 :  CATLG:PC,BLOCK,CAT(2048)
009124 :  NP:,P/P,(CAT)16
009125 :  *WSTOR:I,BLOCK,
009126 :  ,CONT,I13,PRIOR,X1,STRN
009127 :  ,CONT,INFD,INFT
009128 :  *5:,BLOCK,I,T,R,C,L
009129 : C
009130 : C******************************************
009131 : C
009132 :  ,ITA,13
009133 :  ,ATX,I13
009134 :  ,ATX,OUT*OUT
009135 :  ,XTA,FLG.ЗAГPYЗKA
009136 :  ,AOX,M1000.TPAHCЛЯTOPA
009137 :  ,AEX,M1000.CЛEДOM ЗA
009138 :  ,ATX,FLG. /MONTRAN
009139 :  I,VTM,/TRANS/.CПИCOK KAPT
009140 :  T,VTM,1-3.TИП TPAHCЛЯTOPA
009141 :  ,XTA,MC*
009142 :  ,AEX,*TRANS
009143 :  ,UZA,TRANS
009144 : C.....   ПOИCK YПPABЛЯЮЩEЙ KAPTЫ ЯЗЫKA
009145 :  SEA:I,XTA,
009146 :  ,UZA,E SEA
009147 :  ,AEX,MC*
009148 :  ,UZA,TR/IS
009149 :  I,UTM,2
009150 :  ,UJ,SEA
009151 :  E SEA:I,UTM,1
009152 :  T,VLM,SEA
009153 : C=====   TPAHCЛЯTOP HE OБHAPYЖEH
009154 :  14,VTM,BADC
009155 :  ,XTA,TRNAME*
009156 :  ,AEX,BEMSH*
009157 :   ,U1A,ERROR
009158 :   ,XTA,FLG
009159 :   ,AOX,M1000
009160 :   ,ATX,FLG
009161 :   ,UJ,TR*CALL
009162 : C-----   HAШЛИ YПP.KAPTY ЯЗЫKA
009163 :  TR/IS:,ATX,S/CALL
009164 :  ,ATX,PRIOR
009165 :  I,XTA,1.ИMЯ TPAHCЛЯTOPA
009166 :  ,ATX,TRNAME*
009167 : C-----   ПEPEKЛЮЧATEЛЬ TИПOB TPAHCЛЯTOPOB
009168 :  T,UJ,SWT+2
009169 :  SWT:,BSS,
009170 :  :R,VTM,TYP 0
009171 :  ,UJ,RET/L10
009172 :  SWT1:,AEX,PASCAL*
009173 :  ,UJ,TYP 1
009174 :  SWT2:R,VTM,TYP 2
009175 :  ,UJ,RET/L10
009176 : C-----   OTKAЗ OT LIBRARY-22
009177 :  RET/L10:,XTA,SAVL
009178 :  R,UZA,
009179 :  ,ATX,73B
009180 :  ,XTA,
009181 :  ,ATX,SAVL
009182 :  R,UJ,
009183 :  TYP 0:,BSS,
009184 : C=====   CTAT.TPAHCЛЯTOP ПOCT.БИБЛИOTEKИ
009185 :  ,XTA,INFLOA
009186 :  ,ATX,STATIC*
009187 :  ,XTA,TRNAME*
009188 :  ,AEX,ALGOL
009189 :  ,U1A,T0/1
009190 :   ,FUN,75B.KOM.CBEPTKA
009191 :  T0/1:,XTA,SAV47
009192 :  ,UZA,RET
009193 :  ,ATX,47B
009194 :  ,UJ,RET
009195 :  TYP 1:,BSS,
009196 : C=====  ДИHAMИЧECKИЙ TPAHCЛЯTOP
009197 :  ,U1A,T1/1
009198 :  ,CALL,ALLMEMOR
009199 : C.....   ДИHAMИЧECKAЯ ЗAГPYЗKA
009200 :  T1/1:,XTA,
009201 :  ,ATX,STATIC*
009202 :  ,ATX,S/CALL
009203 : C.....   ЗAГPYЗKA ИЗ BPEM.БИБЛИOTEKИ
009204 :  ,XTA,SAV47
009205 :  ,U1A,T1/2
009206 :  ,XTA,47B
009207 :  ,ATX,SAV47
009208 :  T1/2:,BSS,
009209 :   ,XTA,RM*+4
009210 :   ,ATX,47B
009211 : C.....   ЗAKAЗ LIBRARY - 21,22
009212 :  ,XTA,SAVL
009213 :  ,U1A,T1/3.= YЖE БЫЛ
009214 :  ,XTA,73B
009215 :  ,ATX,SAVL
009216 :  ,ASN,64-6
009217 :  ,AEX,NLIBTR
009218 :  ,ATX,73B
009219 :  T1/3:,BSS,
009220 :  PUT 1000:,XTA,M1000.
009221 :  ,AOX,FLG
009222 :  ,ATX,FLG
009223 :  ,UJ,RET
009224 :  TYP 2:,BSS,
009225 : C=====   CTAT.TPAHCЛЯTOP ИЗ ДOП.БИБЛИOTEKИ
009226 :  I,UTM,2
009227 :  I,XTA,
009228 :  ,U1A,TYP 2
009229 : C-----   ПOИCK ЛEHTЫ / MOДEЛИ
009230 :  13,VTM,50B
009231 :  SE/MT:I,XTA,1
009232 :  13,*57,
009233 :  ,ATI,14
009234 :  14,VZM,ABS/MT
009235 :  ,AAX,B47*46
009236 :  ,UZA,MT/IS
009237 :  ABS/MT:13,VTM,400B
009238 :  ,*57,3
009239 :  ,UJ,SE/MT
009240 : C-----   ЛEHTY / MOДEЛЬ  HAШЛИ
009241 :  MT/IS:,ITA,14
009242 :  ,ARX,C50
009243 :  ,AAX,B7
009244 :  ,AEX,B7
009245 :  ,ASN,64-39
009246 :  ,ITS,14
009247 :  ,ASN,64-12
009248 :  15,AOX,
009249 :  ,AOX,B40*39
009250 :  I,XTS,2
009251 :  ,AAX,C1777
009252 :  15,AOX,
009253 :  F*INF0:,BSS,
009254 :  ,AOX,PRIOR
009255 :  ,ATX,INF0
009256 :   ,XTA,INFLOA
009257 :   ,ATX,STATIC*
009258 :  ,XTA,TRNAME*
009259 :  ,AEX,FOREX
009260 :  ,UZA,PUT 1000
009261 :  ,UJ,RET
009262 : C
009263 : C*****   OПPEДEЛEHИE ДOПOЛH.TPAHCЛЯTOPOB
009264 : C
009265 :  TRANS:,BSS,
009266 :  ,ATX,S/CALL
009267 :  ,ATX,PRIOR
009268 :  10,VTM,0
009269 :  ,CALL,PARAMT*.HACTPOЙKA
009270 :  14,V1M,OШФOPM
009271 :  11,VZM,ALIBPR
009272 :  11,UTM,-MINUS
009273 :  11,V1M,TRA
009274 :  ,XTA,B19
009275 :  ,ATX,PRIOR
009276 :  TRA:,BSS,
009277 :  ,XTA,TRNAME*
009278 :  ,ATX,STRN
009279 :  10,VTM,3
009280 :  ,CALL,PARAMT*.ЗA ИMEHEM
009281 :  14,V1M,OШИMЯ
009282 :  ,ATX,TRNAME*
009283 :  11,VZM,TR1.БИБ.CTAT.TPAH.
009284 :  11,UTM,-POINT
009285 :  11,VZM,SWT1.= ДИHAM.TPAHCЛ
009286 : C-----   ЗAKAЗAHA ПEPEПИCЬ TPAHCЛЯTOPA
009287 :  9,VTM,INFT
009288 :  10,VTM,4
009289 :  ,CALL,PARAMT*.ЗA YKAЗAT.
009290 : C.....   ПPOBEPKИ YKAЗATEЛЯ
009291 :  14,V1M,OШYKAЗ
009292 :  12,VZM,OШYKAЗ
009293 :  ,ARX,C500000
009294 :  ,AEX,B19
009295 :  ,AAX,CH/IW
009296 :  ,U1A,OШ YKAЗ
009297 : C.....   ЧTEHИE KATAЛOГA OVERLAY
009298 :  ,XTA,/NP
009299 :  ,AOX,INFT
009300 :  ,ATX,INFT
009301 :  ,TAPE,INFT
009302 : C.....   ПPOBEPKИ KATAЛOГA
009303 :  C,VTM,CAT
009304 :  C,XTA,1762B
009305 :  ,AEX,CODE
009306 :  ,U1A,OШKAT.HET KATAЛOГA
009307 :  C,XTA,1023
009308 :  ,UZA,MAЛMЛ.БЫЛO MAЛO MЛ
009309 : C.....   ПEЧATЬ KOMMEHTAPИЯ OVERLAY
009310 :  14,VTM,CAT+1762B
009311 :  ,ITS,14
009312 :  14,UTM,12
009313 :  ,ITS,14
009314 :  14,VTM,21
009315 :  ,ITS,14
009316 :  C,VTM,CAT-2
009317 :  ,CALL,PRINT8
009318 : C.....   HACTPOЙKA KATAЛOГA HA MБ-25
009319 :  F*25:C,UTM,2
009320 :  C,XTA,1
009321 :  ,AAX,B45/1
009322 :  ,ATX,X1
009323 :  ,ARX,C/25
009324 :  C,ATX,1
009325 :  C,XTA,
009326 :  ,U1A,F*25
009327 : C.....   A HET ЛИ YЖE KATAЛOГA HA MБ
009328 :  C,VTM,CAT-2
009329 :  ,DRUM,RD/25
009330 :  CH*25:C,UTM,2
009331 :  C,XTA,
009332 :  C,AEX,1024
009333 :  ,U1A,CAT/ABS
009334 :  C,XTA,1
009335 :  C,AEX,1025
009336 :  ,U1A,CAT/ABS
009337 :  C,XTA,
009338 :  ,U1A,CH*25
009339 :  ,UJ,PUT*25.KAT. YЖE БЫЛ
009340 :  CAT/ABS:,XTA,X1
009341 :  ,ASN,64+35
009342 :  ,ATI,L
009343 :  ,DRUM,WD/25.ЗAПИCЬ KATAЛOГA
009344 :  ,XTA,WD/25
009345 :  ,ATX,INFD
009346 : C.....   ПEPEПИCЬ БИБЛИOTEKИ
009347 :  T*LIB:,XTA,B1
009348 :  ,ARX,INFT
009349 :  ,ATX,INFT
009350 :  ,XTA,INFD
009351 :  ,ARX,C7741
009352 :  ,AAX,MSK
009353 :  ,ATX,INFD
009354 :  ,TAPE,INFT
009355 :  ,DRUM,INFD
009356 :  L,UTM,-1
009357 :  L,V1M,T*LIB
009358 : C.....   ПOДKЛЮЧEHИE БИБЛИOTEKИ 25
009359 :  PUT*25:,BSS,
009360 :  R,VJM,RET/L10
009361 :  ,XTA,INF/25
009362 :  ,UJ,F*INF0
009363 : C-----   KOHEЦ ПEPEПИCИ БИБЛИOTEKИ
009364 :  ALIBPR:,XTA,B19
009365 :  ,ATX,PRIOR
009366 : C.....   CTATИЧECKИЙ TPAHCЛЯTOP
009367 :  TR1:I,XTA,
009368 :  ,UZA,TR2
009369 :  I,UTM,2
009370 :  ,UJ,TR1
009371 :  TR2:I,UTM,1
009372 :  T,VLM,TR1
009373 :  I,UTM,-3
009374 :  ,UJ,SWT2
009375 :  /NP:,010,NP
009376 :  ,,
009377 :  RD/25:,010,NP+64
009378 :  ,025,
009379 :  WD/25:,,NP
009380 :  ,025,
009381 : C
009382 : C=====   Д И A Г H O C T И K A
009383 : C
009384 :  OШФOPM:14,VTM,ERF
009385 :  ,UJ,ERROR
009386 :  OШИMЯ:14,VTM,ERN
009387 :  ,UJ,ERROR
009388 :  OШYKAЗ:14,VTM,TOШYK
009389 :  ,UJ,ERROR1
009390 :  OШKAT:14,VTM,TOШK
009391 :  ,UJ,ERROR1
009392 :  MAЛMЛ:14,VTM,TMAЛ
009393 :  ,UJ,ERROR1
009394 :  ERROR1:,XTA,STRN
009395 :  ,ATX,TRNAME*
009396 :  ERROR:,ITS,14
009397 :  14,UTM,4
009398 :  ,ITS,14
009399 :  14,VTM,
009400 :  ,ITS,14
009401 :  ,CALL,PRINT8
009402 : C=====   B O З B P A T
009403 :  RET:,WTC,I13
009404 :  ,UJ,
009405 :  BADC:,ISO,30H ***  ПЛOXAЯ YПPABЛЯЮЩAЯ KAPTA
009406 :  ERF :,ISO,30H ***  HEПPABИЛЬHЫЙ ФOPMAT YK
009407 :  ERN :,ISO,30H ***  ПЛOXOE ИMЯ TPAHCЛЯTOPA
009408 :  TOШYK:,ISO,30H ***  OШИБKA B YKAЗATEЛE
009409 :  TOШK :,ISO,30H ***  ПЛOXOЙ OV.-KATAЛOГ
009410 :  TMAЛ:,ISO, 30H ***  ПPИ CБOPKE MAЛO MЛ
009411 :  CODE:,ISO,6HOVERLA
009412 :  ,BSS,
009413 :  *TRANS:,ISO,6H*TRANS
009414 :  ALGOL:,TEXT,8HALGOL
009415 :  BEMSH*:,TEXT,8HBEMSH*
009416 :  FOREX:,TEXT,8HINITFOR*
009417 :  PASCAL*:,TEXT,8HPASCAL*
009418 :  ,BSS,
009419 :  MSK:,OCT,7777 7777 7777 0037
009420 :  B47*46:,OCT,3
009421 :  C/25:,OCT,252
009422 :  B45/1:,OCT,0777 7777 7777 7777
009423 :  B40*39:,OCT,0014
009424 :  INF/25:,LOG,10 0000 0025 0000
009425 :  CH/IW:,LOG,100 6000
009426 :  B19:,LOG,100 0000
009427 :  C500000:,LOG,500000
009428 :  C7741:,LOG,7741
009429 :  C1777:,LOG,1777
009430 :  B7:,LOG,100
009431 :  C50:,LOG,50
009432 :  B1:,LOG,1
009433 : C.....   HOMEP БИБЛИOTEKИ TPAHCЛЯTOPOB
009434 :  NLIBTR:,LOG,321
009435 :  ,BSS,
009436 :  *WSTOR:,BSS,6
009437 :  ,END,
009438 :  TTPRINT:,NAME,.16.09.81, PTИ
009439 : C   П/П HИЖHEГO YPOBHЯ ДЛЯ BЫBOДA HA TEPMИ-
009440 : C   HAЛ ПO ФOPTPAHHOMY OПEPATOPY WRITE(54,)
009441 :  B:,BSS,
009442 :  ,STI,
009443 :  ,STI,14.AKOH
009444 :  ,UTC,H
009445 :  ,STX,.AHAЧ
009446 :  ,ITA,8
009447 :  ,ITS,9
009448 :  ,ITS,13
009449 :  14,XTS,
009450 :  8,BASE,B
009451 :  13,VTM,BLANKS
009452 : C   YДAЛEHИE KOДA '012' B KOHЦE TEKCTA
009453 :  ,AEX,=6H'12''12''12''12''12''12'
009454 :  SUB:,BSS,
009455 :  ,ARX,=6H'177''177''177''177''177''177'
009456 :  ,AAX,=6H'200''200''200''200''200''200'
009457 :  ,ASN,64+7
009458 :  ,ARX,=6H'177''177''177''177''177''177'
009459 :  ,AEX,=6H'177''177''177''177''177''177'
009460 :  14,AAX,
009461 :  13,UJ,
009462 :  BLANKS:,BSS,
009463 :  14,ATX,
009464 : C     ЗAMEHA ПPOБEЛOB HA  0-БAЙTЫ
009465 :  ,AEX,=H
009466 :  13,VJM,SUB
009467 :  ,U1A,OUT.ECTЬ HEПPOБEЛЫ
009468 :  14,ATX,
009469 :  *1:14,UTM,-1
009470 :  14,XTA,
009471 :  ,AEX,=H
009472 :  ,U1A,*2
009473 :  14,ATX,
009474 :  ,UJ,*1
009475 :  *2:,BSS,
009476 : C     YДAЛEHИE ПPOБEЛOB B XBOCTE CЛOBA
009477 :  ,ARX,=7777 7777 7777 7776
009478 :  ,ASN,64+2
009479 :  ,AAX,=H
009480 :  14,AEX,
009481 :  14,ATX,
009482 :  OUT:,BSS,
009483 : C.....
009484 :  ,WTC,H
009485 :  9,VTM,
009486 :  ,*71,IW71
009487 :  ,STI,
009488 :  ,STI,13
009489 :  ,STI,9
009490 :  ,ATI,8
009491 :  13,UJ,
009492 :  H:,BSS,1
009493 :  IW71:9,040,
009494 :  ,,
009495 :  ,END,
009496 :  TTREAD:,NAME,
009497 : C   П/П  HИЖHEГO YPOBHЯ ДЛЯ BBOДA ПO
009498 : C   ФOPTPAHHOMY OПEPATOPY READ C TEP-
009499 : C   MИHAЛA.
009500 :  B:,BSS,
009501 :  ,STI,14
009502 :  ,ITA,8
009503 :  ,ITS,9
009504 :  ,ITS,13
009505 :  8,BASE,B
009506 :  ,ITS,
009507 :  15,MTJ,9.БYФEP BBOДA!
009508 :  14,MTJ,15.AHAЧ!
009509 : C   BBOД CTPOKИ C TEPMИHAЛA
009510 :  ,*71,IW71
009511 :  14,VTM,-12
009512 :  *1:,BSS,
009513 :  14,UTC,12
009514 :  9,XTA,
009515 :  15,ATX,
009516 :  ,AAX,=377
009517 :  ,UZA,ZERO
009518 :  14,VLM,*1
009519 :  9,XTA,13
009520 :  13,VJM,GIVBLANK
009521 :  END:,BSS,
009522 :  ,AAX,=6H'377''377'''''''''
009523 :  ,AOX,EW
009524 :  15,ATX,
009525 :  9,MTJ,15
009526 :  ,STI,
009527 :  ,STI,13
009528 :  ,STI,9
009529 :  ,ATI,8
009530 :  13,UJ,.CIAO!
009531 :  ZERO:,BSS,
009532 :  15,XTA,
009533 :  13,VJM,GIVBLANK
009534 :  15,ATX,
009535 :  ,XTA,=H
009536 :  *2:,BSS,
009537 :  15,ATX,
009538 :  14,VLM,*2
009539 :  ,UJ,END
009540 :  GIVBLANK:,BSS,
009541 : C   ЗAMEHA BCEX  0-БAЙTOB  ПPOБEЛAMИ
009542 :  ,ATX,W
009543 :  ,ARX,=6H'177''177''177''177''177''177'
009544 :  ,ASN,64+2
009545 :  ,AAX,=H
009546 :  ,AEX,=H
009547 :  ,AOX,W
009548 :  13,UJ,
009549 : C..........................................
009550 :  W:,BSS,1
009551 :  IW71:9,060,
009552 :  ,,
009553 :  EW:,ISO,6H''''   '12'
009554 :  ,END,
009555 :       VDSQRT     :,NAME,
009556 :       D*LO*D     :,SUBP,
009557 :       D*ST*D     :,SUBP,
009558 :       D*MU*D     :,SUBP,
009559 :                  ,STI,  8
009560 :                  ,STI,  9
009561 :                  ,ITA,  13
009562 :             15 ,  ATX,
009563 :              9 ,  MTJ,  14
009564 :             13 ,  VJM,  D*LO*D
009565 :             14 ,  VTM,   BSS+ 0001B
009566 :             13 ,  VJM,  D*ST*D
009567 :             10 ,  VTM,   BSS+ 0001B
009568 :             10 ,  XTA,
009569 :                  ,ASN,  72B
009570 :                  ,ARX,
009571 :                  ,UIA,   *00012
009572 :             14 ,  VTM,   BSS+ 0001B
009573 :             13 ,  VJM,  D*LO*D
009574 :             14 ,  VTM,   *00023
009575 :             13 ,  VJM,  D*MU*D
009576 :             14 ,  VTM,   BSS+ 0001B
009577 :             13 ,  VJM,  D*ST*D
009578 :  *00012    :10 ,  XTA,
009579 :                  ,ASN,  77B
009580 :                  ,ASN,  152B
009581 :                  ,UTC,   *00021
009582 :                  ,AOX,
009583 :             10 ,  XTS,  1
009584 :                  ,ASN,  150B
009585 :                  ,ASN,  73B
009586 :             15 ,  AOX,
009587 :              8 ,  ATX,
009588 :             15 ,  XTA,
009589 :                  ,ATI,  13
009590 :             13 ,   UJ,
009591 :                  ,UTC,
009592 :  *00021    :,INT,              0
009593 :             ,     LOG,  00100000 00000000.
009594 :  *00023    :,REAL, 2.00000000000E+00
009595 :  BSS       :     ,BSS,  3
009596 :       ,END,
009597 :  WRCARD:,NAME,
009598 :  WRWORD/:,PC,1024
009599 :  XWRITE/:,LC,2
009600 :  *0:,BLOCK,BEG(0),POINT0(0),POINT,DINF
009601 : C------WBEGIN(YK)-ПOДГOTOBKA K ЗAПИCИ
009602 : C      WRWORD(AW)-ЗAПИCЬ CЛOBA
009603 : C     WRCARD(AC)-ЗAПИCЬ KAPTЫ
009604 : C      WRIEND-KOHEЦ ЗAПИCИ
009605 : C------XAPAKTEPИCTИKA XWRITE/:
009606 : C      ПEPBOE CЛOBO:
009607 : C      1-15-TEK.AДPEC CЛOBA B БYФEPE
009608 : C     16-33-HAЧAЛЬHOE ЗHAЧEHИE YK ЧTEHИЯ
009609 : C     34=1-БЛOKИPOBKA ЗAПИCИ
009610 : C      35=1-KAPTA B COSY
009611 : C      BTOPOE CЛOBO:
009612 : C     ИHФOPMAЦИOHHOE CЛOBO TEK.OБMEHA
009613 :  C:,BSS,
009614 :  ,UTC,R
009615 :  ,ATX,
009616 :  ,ATI,9
009617 :  ,ITA,13
009618 :  ,ITS,7
009619 :  7,BASE,C
009620 :  14,VTM,XWRITE/
009621 :  14,XTS,
009622 :  ,AAX,=2000 0000 0000.C0SY
009623 :  ,U1A,K
009624 :  ,XTS,R
009625 :  9,VTM,RR
009626 :  ,ITS,9
009627 :  ,CALL,ISOTCOSY
009628 :  K:9,XTA,
009629 :  ,AEX,=6H*READ'201'
009630 :  ,U1A,KYK
009631 :  9,XTA,1
009632 :  ,AEX,=3HOLD
009633 :  ,ASN,64+24
009634 :  ,U1A,KYK
009635 :  ,XTA,=5H*READ
009636 :  9,ATX,
009637 :  KYK:,ITS,9
009638 :  ,CALL,WRWORD
009639 :  9,XTA,
009640 :  9,UTM,1
009641 :  ,AAX,=377
009642 :  ,AEX,=012
009643 :  ,U1A,KYK
009644 :  RET:,STI,
009645 :  ,STI,7
009646 :  ,STI,13
009647 :  13,UJ,
009648 :  WRWORD:,ENTRY,
009649 :  12,VTM,XWRITE/
009650 :  :,ATI,11
009651 :  8,BASE,C
009652 :  12,XTA,BEG
009653 :  ,AAX,=1 0000 0000 0000.NO WR
009654 :  ,UIA,NO/
009655 :  11,XTA,
009656 :  12,WTC,POINT
009657 :  ,ATX,
009658 :  NO/:,BSS,
009659 :  12,XTA,POINT
009660 :  ,AAX,=1777
009661 :  ,AEX,=1777
009662 :  ,UZA,COR
009663 :  12,XTA,POINT
009664 :  ,ARX,VPT
009665 :  12,STX,POINT
009666 :  13,UJ,
009667 :  COR:12,XTA,BEG
009668 :  ,AAX,=1 0000 0000 0000.N0 WR
009669 :  ,U1A,NO
009670 :  12,TAPE,DINF
009671 :  NO:,BSS,
009672 :  12,XTA,BEG
009673 :  ,AEX,=1777
009674 :  12,ATX,BEG
009675 :  12,XTA,DINF
009676 :  13,MTJ,8
009677 :  ,CALL,DRUMTAP*
009678 :  12,STX,DINF
009679 :  8,UJ,
009680 :  WBEGIN:,ENTRY,
009681 :  12,VTM,XWRITE/
009682 :  11,VTM,WRWORD/
009683 :  ,ATI,14
009684 :  ,ITA,11
009685 :  12,ATX,POINT0
009686 :  ,ASN,64-20
009687 :  14,AOX,
009688 :  12,ATX,DINF
009689 :  14,XTA,
009690 :  ,ASN,64-15
009691 :  12,AOX,BEG
009692 :  12,STX,BEG
009693 :  13,UJ,
009694 :  WRIEND:,ENTRY,
009695 :  12,VTM,XWRITE/
009696 :  ENTR/:,XTS,
009697 :  8,BASE,C
009698 :  ,UJ,COR
009699 :   VPT:,LOG,1
009700 :  R:,BSS,1
009701 :  RR:,BSS,14
009702 :  ,END,
009703 :  WRITLIBR:,NAME,
009704 :  CATALOG:,PC,8192
009705 :  PRINT8:,SUBP,
009706 :  LDRM:,EQU,54B.CBOБ.B TEMPO
009707 :  LEX:,EQU,43B .AДPEC OБM.ЛИCTA
009708 :  ARD:,EQU,47B .ЧTEHИE CAT.TL
009709 :  AWRT:,EQU,50B.ЗAПИCЬ CAT.TL
009710 :  L       :12,VTM,1                        .ENTRY FOR COMPACT STRUCTURE
009711 :             ,UJ ,*+2                      .
009712 :          :12,VTM,                         .ENTRY FOR NONCOMPACT STRUCTUR
009713 :          :   ,ATI, 11                     .
009714 :              ,ITA, 1                      .
009715 :            14,VTM, -12                    .
009716 :          :14,ITS,14                       .
009717 :           14,VLM,*                        .
009718 :            7,VTM,L                        .
009719 :           11,MTJ,6                        .ADDRESS OF READ-SUBPR.
009720 :              ,ITA, 12                     .
009721 :             7,ATX, STR3-L                 .
009722 :              ,XTA, AWRT
009723 :             7,AAX, NLIS-L
009724 :              5,VTM, CATALOG
009725 :              ,ITS, 5
009726 :              ,ASN, 64-20
009727 :             15,AOX,
009728 :             7,ATX, WCAT-L
009729 :             7,AOX, CRED-L
009730 :              7,ATX, RCAT-L
009731 : C.....   ЧИCTKA 7-И ЛИCTOB CATALOG(БEЗ 1-ГO)
009732 :  14,VTM,1-16000B
009733 :  ,XTA,
009734 :  :,UTC,CATALOG+17777B
009735 :  14,ATX,
009736 :  14,VLM,*-1
009737 : C.....   ЧTEHИE KATAЛOГA TEMPO
009738 :  7,XTA,RCAT-L
009739 :  13,VJM,RDWRCTL
009740 :  ,UZA,CHKENTRY
009741 : C.....   ИCПOPЧEH KATAЛOГ TEMPORARY LIBRARY
009742 : C        (HA 8 ЛИCTAX HET ПP-KA KOHЦA KATAЛ.)
009743 :  14,VTM,TER4
009744 :  ,UJ,PLER
009745 :  CHKENTRY: 6,UTC,                         .
009746 :           13,VJM,                         .READ WORD OF STANDART ARRAY
009747 :          : 7,STX,STR-L                    .
009748 :            6,UTC,                         .
009749 :           13,VJM,                         .
009750 :          : 7,STX,STR1-L                   .
009751 :             7,XTA, STR1-L                 .
009752 :            7,AAX,MSTR-L                   .
009753 :             ,UZA,CHEKSTR                  .IF NOT ENTRY
009754 :             7,XTA, STR1-L                 .
009755 :            7,AAX,MA3L-L                   .CLEAN ENTRY MARK OF ENTRY ADR
009756 :            7,ATX,STR1-L                   .
009757 :           12,VJM,WRTENTRY                 .WRITE ENTRY TO CATALOG
009758 :          :  ,UJ ,CHKENTRY                 .
009759 :  CHEKSTR :  6,UTC,                        .                  AFTER 5WL37
009760 :           13,VJM,                         .READ 3D WORD OF STRUCTURE
009761 :          : 7,STX,STR2-L                   .
009762 :             7,XTA, STR3-L                 .
009763 :              ,U1A, SHORTSTR               .
009764 :            5,VTM,-6                       .READ LONG STRUCTURE AND
009765 :          : 6,UTC,                         .CONVERT TO COMPACT MODE
009766 :           13,VJM,                         .
009767 :          : 5,UTC,6                        .
009768 :            7,STX,STR3-L                   .
009769 :            5,VLM,*-2                      .
009770 :            7,XTS,STR3-L                   .CALLS
009771 :             ,ASN,64-12                    .
009772 :            7,AOX,STR2-L                   .PAREXPS
009773 :             ,ASN,64-12                    .
009774 :            7,AOX,STR1-L                   .DECLARATIONS
009775 :             ,ASN,64-12                    .
009776 :            7,AOX,STR-L                    .HEAD
009777 :            7,XTS,STR4-L                   .LONG IDENTIFIERS
009778 :             ,ASN,64-15                    .
009779 :            7,AOX,STR10-L                  .DATA
009780 :             ,ASN,64-15                    .
009781 :            7,AOX,STR11-L                  .SET DATA
009782 :            7,XTS,STR7-L                   .ABS CONSTANTS
009783 :             ,ASN,64-15                    .
009784 :           7,AOX,STR6-L                    PAR.CONSTS
009785 :             ,ASN,64-15                    .
009786 :            7,AOX,STR5-L                   .INSTRUCTIONS
009787 :            7,STX,STR2-L                   .
009788 :            7,STX,STR1-L                   .
009789 :            7,STX,STR-L                    .
009790 :  SHORTSTR: 7,XTA,STR-L                    .
009791 :            7,AAX,MA4S-L                   .HEAD
009792 :            7,XTS,STR-L                    .
009793 :             ,ASN,64+12                    .
009794 :            7,AAX,MA4S-L                   .DECLARATIONS
009795 :           15,ARX,                         .
009796 :            7,XTS,STR-L                    .
009797 :             ,ASN,64+24                    .
009798 :            7,AAX,MA4S-L                   .PAR.EXPRETIONS
009799 :           15,ARX,                         .
009800 :            7,XTS,STR-L                    .
009801 :             ,ASN,64+36                    .
009802 :            7,AAX,MA4S-L                   .CALLS
009803 :           15,ARX,                         .
009804 :            7,XTS,STR1-L                   .
009805 :             ,ASN,64+30                    .
009806 :            7,AAX,MA3L-L                   .LONG IDENTIFIERS
009807 :           15,ARX,                         .
009808 :            7,ATX,N3-L                     .LENGTH OF DEF.TABLE
009809 :            7,AAX,MDEFOVL-L                .
009810 :             ,U1A,LER1                     .INCORRECT LENGTH OF DEFTABLE
009811 :            7,XTA,STR1-L                   .
009812 :            7,AAX, MA3L-L
009813 :            7,XTS,STR1-L                   .
009814 :             ,ASN,64+15                    .
009815 :            7,AAX,MA3L-L                   .DATA
009816 :           15,ARX,                         .
009817 :            7,ATX,N2-L                     .SET DATA + DATA
009818 :            7,XTA,STR2-L                   .
009819 :            7,AAX,MA3L-L                   .INSTRUCTIONS
009820 :            7,XTS,STR2-L                   .
009821 :             ,ASN,64+15                    .
009822 :            7,AAX,MA3L-L                   .PAR.CONSTS
009823 :            7,STX,LBSS-L                   .
009824 :            7,XTS,STR2-L                   .
009825 :             ,ASN,64+30                    .
009826 :            7,AAX,MA3L-L                   .ABS.CONSTS
009827 :           15,ARX,                         .
009828 :            7,ATX,LBOD-L                   .LENGTH OF PROGRAM BODY
009829 :            7,ARX,C2-L                     .+2(1 WORD OF BODY,AT LEAST,
009830 :            7,ATX,R-L                      .MUST BE ON SAME PAGE,THAT
009831 :            7,AAX,MSEG-L                   .DEF.TABLE
009832 :            7,ATX,K-L                      .NUMBER OF SEGMENTS IN BODY
009833 :            7,XTA,R-L                      .
009834 :            7,AAX,MRES-L                   .LENGTH OF NOT FULL SEGMENT
009835 :            7,ARX,N3-L                     .
009836 :            7,ARX,N2-L                     .
009837 :            7,AAX,MSEG-L                   .
009838 :            7,ATX,L1-L                     .NUMBER OF SEGM.IN DEF.TABLE-1
009839 :            7,ARX,K-L                      .
009840 :            7,ATX,K1-L                     .FULL NUMBER OF SEGM.-1
009841 :          :  ,XTA,LDRM                     .
009842 :            7,ARX,K1-L                     .
009843 :            7,ARX,C1S-L                    .
009844 :            7,ATX,DRLT-L                   .BEG.FOR ANOTHER SUBPROGRAM
009845 :            7,XTA,DRMX-L                   .
009846 :            7,AEX,CAL7-L                   .
009847 :            7,ARX,DRLT-L                   .
009848 :            7,AAX,MSGN-L                   .
009849 :             ,UZA,LER2                     .OVERFLOW OF LIBRERY
009850 :            7,XTA,LBOD-L                   .
009851 :            7,ARX,N2-L                     .
009852 :            7,ATX,N-L                      .
009853 :            7,ARX,N3-L                     .
009854 :            7,ATX,NPR-L                    .LENGTH OF INF.FOR LIBRERY
009855 :             ,ITA,                         .
009856 :            7,ATX,PRCT-L                   .CLEAN COUNTER OF WORDS
009857 :             ,XTA,LDRM                     .FREE PLACE ON DRUM
009858 :            7,ATX,DRMC-L                   .FOR WRITING ON DRUM
009859 :            7,AAX,MSGP-L                   .NUMER OF PAGE-SEGMENT
009860 :            7,ATX,NSEG-L                   .BEGINNING ON PAGE
009861 :              ,UZA, BGLP
009862 :            14,VJM, ACLC
009863 :             7,XTA, ADR-L
009864 :             7,AOX, CRED-L
009865 :             7,ATX, ADR-L
009866 :             7,*70, ADR-L
009867 :  BGLP    : 7,XTA,NSEG-L                   .
009868 :            7,ARX,C3-L                     .
009869 :            7,ATX,PGCT-L                   .SET COUNT FOR PAGE
009870 :          :  ,WTC,LEX                      .
009871 :            5,VTM,                         .
009872 :           14,VTM,-2                       .
009873 :  STOSTR  :14,UTC,2                        .
009874 :            7,XTA,STR-L                    .WRITE STRUCTURE ON EXCHANGE
009875 :            7,WTC,PGCT-L                   .PAGE
009876 :           14,UTC,                         .
009877 :            5,ATX,-1                       .
009878 :           14,VLM,STOSTR                   .
009879 :  BGL1    : 6,UTC,                         .
009880 :           13,VJM,                         .READ WORD
009881 :            7,WTC,PGCT-L                   .
009882 :            5,STX,                         .STORE WORD
009883 :            7,XTA,PRCT-L                   .
009884 :             7,AEX, N-L
009885 :               ,U1A, *+2
009886 :              ,WTC, PGCT
009887 :              5,XTA,
009888 :              7,ATX, STR-L
009889 :          :  7,XTA, PRCT-L
009890 :            7,ARX,C1-L                     .
009891 :            7,ATX,PRCT-L                   .
009892 :            7,AEX,NPR-L                    .
009893 :             ,UZA,ENPR                     .IF END OF STAND. ARRAY
009894 :            7,XTA,PGCT-L                   .
009895 :            7,ARX,C1-L                     .
009896 :            7,ATX,PGCT-L                   .
009897 :            7,AAX,MOVL-L                   .
009898 :             ,UZA,BGL1                     .IF PAGE NOT FULL
009899 :  WRPG    :  ,XTA,                         .
009900 :            7,ATX,PGCT-L                   .
009901 :           14,VJM,ACLC                     .
009902 :           : 7,*70, ADR-L
009903 :          : 7,XTA,DRMC-L                   .
009904 :            7,ARX,C1P-L                    .PREPARE WRITING OF NEXT PAGE
009905 :            7,ATX,DRMC-L                   .
009906 :             ,UJ ,BGL1                     .CONTINUATION
009907 :  ACLC    : 7,XTA,DRMC-L                   .PREPARES INF.WORD FOR DRUM
009908 :             ,ASN,64-27                    .
009909 :            7,AUX,MUNP-L                   .
009910 :               ,XTS, LEX
009911 :             ,ASN,64-20                    .
009912 :           15,AOX,                         .
009913 :             7,ATX, ADR-L
009914 :           14,UJ ,                         .
009915 :  ENPR    :14,VJM,ACLC                     .
009916 :          :  7,*70, ADR-L
009917 :             ,XTA,LDRM                     .
009918 :            7,AAX,MSGP-L                   .NUMER OF SEGMENT
009919 :              ,ASN, 64-16
009920 :             ,AOX,LDRM                     .
009921 :            7,AAX,MOVL-L                   .
009922 :             ,ASN,64-20                    .
009923 :             7,AOX, CCAT-L
009924 :              7,XTS, LBOD-L
009925 :            7,ARX,LBSS-L                   .
009926 :              ,ASN, 64-15
009927 :           15,AOX,                         .48-46 45-42  41-31 30-16 15-1
009928 :            7,ATX,STR1-L                   .  6   NSEGM NDR,TR LBODY ENTR
009929 :            7,XTA,DRLT-L                   .
009930 :             ,ATX,LDRM                     .FREE PLACE ON DRUM
009931 :           12,VJM,WRTENTRY                 .WRITE MANE ENTRY TO CATALOG
009932 :          :  ,ITA,                         .
009933 :            7,ATX,ERRFL-L                  .
009934 :  EXIT:,BSS,
009935 :  7,XTA,ERRFL-L .ПOCЛE OШИБOK
009936 :  ,U1A,RESI .HE CБPACЫBAEM
009937 :  7,XTA,WCAT-L
009938 :  5,VTM,CATALOG
009939 :  13,VJM,RDWRCTL
009940 :  RESI    :   ,STI,
009941 :           14,VTM,13                       .RESET INDEXIS
009942 :          :14,UTM,-1                       .
009943 :           14,STI,1                        .
009944 :           14,V1M,*-1                      .
009945 :             ,UTC,ERRFL                    .
009946 :             ,XTS,                         .ERROR FLAG TO MAGAZINE
009947 :           13,UJ ,                         .EXIT
009948 :  WRTENTRY:  5,VTM, CATALOG+2
009949 :  5,XTA,8189
009950 :            7,AEX,CAL7-L                   .
009951 :             ,UZA,LER3                     .OVERFLOW OF CATALOG
009952 :  14,VTM,17 774B
009953 :          : 5,UTC,                         .SHIFT CATALOG
009954 :           14,XTA,-1                       .
009955 :            5,UTC,                         .
009956 :           14,ATX,1                        .
009957 :           14,UTM,-1                       .
009958 :           14,V1M,*-2                      .
009959 :            7,XTA,STR-L                    .
009960 :            5,ATX,                         .WRITE NAME OF ENTRY
009961 :            7,XTA,STR1-L                   .
009962 :            5,ATX,1                        .WRITE INF.WORD OF ENTRY
009963 :              ,ITS, 1                      .
009964 :              ,XTS,                        .
009965 :             5,MTJ, 1                      .
009966 :             1,UTM, 2                      .
009967 :  CHKCYCL :  5,XTA,                        .
009968 :             1,AEX,                        .
009969 :              ,UZA, ENTRYCH                .
009970 :             1,XTA, 1                      .
009971 :             7,AEX, CAL7-L                 .
009972 :              ,UZA, CHKEXIT                .
009973 :             1,UTM, 2                      .
009974 :              ,UJ,  CHKCYCL                .
009975 :  CHKEXIT :   ,STX,                        .
009976 :              ,STI, 1                      .
009977 :              ,UJ , WRTENT1                .
009978 :  ENTRYCH :  1,XTA, 1                      .
009979 :             7,AAX, LIBMASK-L              .
009980 :              ,UZA, REWRCAT                .
009981 :             1,XTA, 3                      .
009982 :             7,AAX, LIBMASK-L              .
009983 :              ,U1A, REWRCAT                .
009984 :             1,XTA, 1                      .
009985 :             7,AAX, MA3LC-L                .
009986 :             1,AOX, 3                      .
009987 :             1,ATX, 3                      .
009988 :  REWRCAT :  1,XTA, 2                      .
009989 :             1,ATX,                        .
009990 :             1,XTA, 3                      .
009991 :             1,ATX, 1                      .
009992 :             7,AEX, CAL7-L                 .
009993 :              ,UZA, CHKEXIT1               .
009994 :             1,UTM, 2                      .
009995 :              ,UJ,  REWRCAT                .
009996 :  CHKEXIT1:  1,ATX, 3                      .
009997 :              ,UJ,  CHKEXIT                .
009998 :  WRTENT1 :   ,XTA, LDRM                   .               AFTER 35WL253
009999 :            5,ATX, -2
010000 :          :12,UJ ,                         .
010001 :  LER1    :14,VTM,TER1                     .
010002 :             ,UJ ,PLER                     .
010003 :  LER2    :14,VTM,TER2                     .
010004 :             ,UJ ,PLER                     .
010005 :  LER3    :14,VTM,TER3                     .
010006 :             ,UJ ,PLER                     .
010007 :  PLER    : 7,XTA,C1-L                     .
010008 :            7,ATX,ERRFL-L                  .
010009 :             ,ITS,14                       .
010010 :           14,UTM,4                        .
010011 :             ,ITS,14                       .
010012 :             ,ITS,                         .
010013 :           13,VTM,EXIT                     .
010014 :             ,UJ ,PRINT8                   .
010015 :  RDWRCTL:,BSS,
010016 : C.....   П/П ЧTEHИЯ-ЗAПИCИ KATAЛOГA TL
010017 : C     C KOHTPOЛEM HA KOHEЦ KATAЛOГA (BCE 7)
010018 : C     HA CYMM.= HAЧ.CЛOBO ЧTEHИЯ(ЗAПИCИ) Э70
010019 : C         ИP5 = CATALOG
010020 :  7,ATX,WL-L
010021 :  8,VTM,1-8.ЗOH KATAЛOГA
010022 :  NEXT:,BSS,
010023 :  10,VTM,-511
010024 :  7,*70,WL-L
010025 :  ECTL:5,XTA,1
010026 :  7,AEX,CAL7-L
010027 :  13,UZA,.HAШЛИ KOHEЦ
010028 :  5,UTM,2
010029 :  10,VLM,ECTL
010030 : C.....   HA ЭTOM ЛИCTE KOHЦA HET,БEPEM CЛEД.
010031 :  7,XTA,WL-L
010032 :  7,ARX,CEX-L
010033 :  7,ATX,WL-L
010034 :  8,VLM,NEXT
010035 : C.....   HET KOHЦA BO BCEX 8 ЛИCTAX KATAЛOГA
010036 :  13,UJ,.HE 0 HA CYMM.
010037 :  TER1:,ISO,30HПEPEПOЛHEHA TAБЛИЦA OПИCAHИЙ
010038 :  TER2:,ISO,30HПEPEПOЛHEHИE БИБЛИOTEKИ
010039 :  TER3:,ISO,30HПEPEПOЛHEHИE KATAЛOГA
010040 :  TER4:,ISO,30HИCПOPЧEH KATAЛOГ TEMPORARY LIB
010041 :  MA4S    :  ,OCT,0000 0000 0000 7777      .
010042 :  MA3L    :  ,OCT,0000 0000 0007 7777      .
010043 :  C2      :  ,OCT,0000 0000 0000 0002      .
010044 :  MSGP    :   ,OCT, 0000 0000 0000 1740
010045 :  MSEG    :   ,OCT, 0000 0000 7777 7740
010046 :  MRES    :   ,OCT, 0000 0000 0000 0037
010047 :  MOVL    :  ,OCT,7777 7777 7777 6000      .
010048 :  CAL7    :  ,OCT,7777 7777 7777 7777      .
010049 :  C1S     :   ,OCT, 0000 0000 0000 0040
010050 :  MSGN    :  ,OCT,4000 0000 0000 0000      .
010051 :  C3      :  ,OCT,0000 0000 0000 0003      .
010052 :  CRED    :  ,OCT,0010 0000 0000 0000      .
010053 :  C1      :  ,OCT,0000 0000 0000 0001      .
010054 :  C1P     :  ,OCT,0000 0000 0000 2000      .
010055 :  MUNP    :   ,OCT, 0000 0000 0077 0037
010056 :  MNPG    :  ,OCT,0000 0000 0000 1777      .
010057 :  CCAT    :   ,OCT, 2000 0000 0000 0000
010058 :  DRMX    :  ,OCT,0000 0000 0300 0000      .
010059 :  MSTR    :  ,OCT,7000 0000 0000 0000      .
010060 :  MA3LC   :   ,OCT, 7777 7777 7770 0000    .
010061 :  LIBMASK :   ,OCT, 5777 7700 0000 0000    .
010062 :  MDEFOVL :  ,OCT, 7777 7777 7777 4000     .
010063 :  N3      :   ,OCT,                        .              AFTER 5WL306
010064 :  N2      :  ,OCT,                         .
010065 :  LBOD    :  ,OCT,                         .
010066 :  LBSS    :  ,OCT,                         .
010067 :  R       :  ,OCT,                         .
010068 :  K       :  ,OCT,                         .
010069 :  L1      :  ,OCT,                         .
010070 :  K1      :  ,OCT,                         .
010071 :  DRLT    :  ,OCT,                         .
010072 :  N       :  ,OCT,                         .
010073 :  NPR     :  ,OCT,                         .
010074 :  PRCT    :  ,OCT,                         .
010075 :  DRMC    :  ,OCT,                         .
010076 :  NSEG    :  ,OCT,                         .
010077 :  ADR     :  ,OCT,                         .
010078 :  PGCT    :  ,OCT,                         .
010079 :  ERRFL   :  ,OCT,                         .
010080 :  STR     :  ,OCT,                         .
010081 :  STR1    :  ,OCT,                         .
010082 :  STR2    :  ,OCT,                         .
010083 :  STR3    :  ,OCT,                         .
010084 :  STR4    :  ,OCT,                         .
010085 :  STR5    :  ,OCT,                         .
010086 :  STR6    :  ,OCT,                         .
010087 :  STR7    :  ,OCT,                         .
010088 :  STR10   :  ,OCT,                         .
010089 :  STR11   :  ,OCT,                         .
010090 :  CEX     :   ,OCT, 0000 0100 0000 0001
010091 :  WL      :   ,OCT,
010092 :  NLIS    :   ,OCT, 7777 4077 7777 7777
010093 :  WCAT    :   ,OCT,
010094 :  RCAT    :   ,OCT,
010095 :     ,END,
010096 :       ZAPAK12     :,NAME,
010097 :      *00000B:     15    ,ATX ,
010098 :                         ,ASN ,66
010099 :                         ,STI ,14
010100 :                         ,UTC,      *00010B
010101 :                         ,AAX ,
010102 :                         ,STI ,12
010103 :                   12    ,UTM,      *00011B
010104 :                         ,ASN ,28
010105 :                   12    ,AUX ,
010106 :                   14    ,XTS ,
010107 :                   12    ,AAX ,
010108 :                   14    ,AEX ,
010109 :                   15    ,AOX ,
010110 :                   14    ,STX ,
010111 :                   13    ,UJ,  00000B
010112 :      *00010B:,OCT,0000  0000  0000  0003
010113 :      *00011B:,OCT,7777  0000  0000  0000
010114 :      *00012B:,OCT,0000  7777  0000  0000
010115 :      *00013B:,OCT,0000  0000  7777  0000
010116 :      *00014B:,OCT,0000  0000  0000  7777
010117 :                   ,END,
010118 :       ZAPAK8      :,NAME,
010119 :       C           :,EQU,           *00014B
010120 :  GAK/3:,EQU,     *00014B  -        *00014B
010121 :  GAK/5:,EQU,     *00015B  -        *00014B
010122 :  GAK/7:,EQU,     *00016B  -        *00014B
010123 :  GAL/1:,EQU,     *00017B  -        *00014B
010124 :                   14    ,VTM,      *00014B
010125 :                         ,NTR ,0
010126 :                   14    ,A*X , GAK/3
010127 :                   15    ,ATX ,
010128 :                   14    ,A/X , GAK/5
010129 :                         ,NTR ,3
010130 :                   14    ,A+X , GAK/7
010131 :                         ,ATI ,12
010132 :                         ,NTR ,2
010133 :                   14    ,A*X , GAK/5
010134 :                   15    ,X-A ,
010135 :                         ,NTR ,3
010136 :                   14    ,A+X , GAK/7
010137 :                         ,STI ,11
010138 :                   11    ,ASN ,24
010139 :                   14    ,XTS , GAL/1
010140 :                   11    ,ASN ,24
010141 :                   12    ,AAX ,
010142 :                   12    ,AEX ,
010143 :                   15    ,AOX ,
010144 :                   12    ,STX ,
010145 :                         ,NTR ,2
010146 :                   13    ,UJ,  00000B
010147 :      *00014B:,OCT,6400  0000  0000  0010
010148 :      *00015B:,OCT,4314  0000  0000  0000
010149 :      *00016B:,OCT,6400  0000  0000  0000
010150 :      *00017B:,OCT,0000  0000  0000  0377
010151 :                   ,END,
010152 :  PARAMT*:,NAME,
010153 :  ,SPACE,62
010154 : C**************************************************************
010155 : C*                                                            *
010156 : C*     BЫДEЛEHИE И OБPAБOTKA ПAPAMETPOB  C TEKCTOBЫX KAPT     *
010157 : C*     --------------------------------------------------     *
010158 : C*    ИCXOДHAЯ TEKCTOBAЯ KAPTA (B ПPOГPAMME  PACCMATPИBAЮTCЯ  *
010159 : C*  TOЛЬKO ПEPBЫE 72 CИMBOЛA) ДOЛЖHA БЫTЬ ПOMEЩEHA B MACCИB:  *
010160 : C*      MONCARD/:,LC,14                                       *
010161 : C*  ДЛЯ YПPABЛЯЮЩИX KAPT MOHИTOPHOЙ CИCTEMЫ ЭTA PAБOTA BЫПO-  *
010162 : C*  ЛHЯETCЯ PAЗДEЛOM   STAR* , A ДЛЯ ПPOЧИX CЛYЧAEB ЗAПИCЬ B  *
010163 : C*  MONCARD/   HYЖHO ПPEДYCMOTPETЬ B BЫЗЫBAЮЩEЙ ПPOГPAMME.    *
010164 : C*    XAPAKTEP OБPAБOTKИ ЗABИCИT OT COДEPЖИMOГO I10 , ЗAДAH-  *
010165 : C*  HOГO ПPИ OБPAЩEHИИ (TИП PAБOTЫ):                          *
010166 : C*  0  - HAЧAЛЬHAЯ YCTAHOBKA ЗA ПEPBЫЙ PAЗДEЛИTEЛЬ (9+6);     *
010167 : C*  1  - BЫДEЛEHИE TEKCTOBOГO  ПAPAMETPA  (ПPOБEЛ - ЗHAЧAЩИЙ  *
010168 : C*       CИMBOЛ);                                             *
010169 : C*  2  - CPABHEHИE C ЭTAЛOHOM;                                *
010170 : C*  3  - OБPAБOTKA ИДEHTИФИKATOPA;                            *
010171 : C*  4  - OБPAБOTKA YKAЗATEЛЯ MЛ/MД/MБ;                        *
010172 : C*  5  - OБPAБOTKA ЦEЛOГO BOCЬMEPИЧHOГO ЧИCЛA;                *
010173 : C*  6  - BЫДEЛEHИE TEKCTOBOГO  ПAPAMETPA (CTAHДAPTHЫЙ PEЖИM,  *
010174 : C*       ПPOБEЛЫ ИГHOPИPYЮTCЯ);                               *
010175 : C*  7  - OБPAБOTKA ЦEЛOГO ДECЯTИЧHOГO ЧИCЛA;                  *
010176 : C*  8  - BЫДEЛEHИE TEKCTOBOГO  ПAPAMETPA ДO ЗAДAHHOГO PAЗДE-  *
010177 : C*       ЛИTEЛЯ (ПPOБEЛЫ ИГHOPИPYЮTCЯ);                       *
010178 : C*  9  - HAЧAЛЬHAЯ YCTAHOBKA K ПEPBOMY CИMBOЛY KAPTЫ;         *
010179 : C*  10 - BOЗBPAT K ПOCЛEДHEMY OБPAБOTAHHOMY ПAPAMETPY;        *
010180 : C*    BЫДEЛEHHЫЙ  ПAPAMETP B KOДE ISO  (БEЗ ПPOБEЛOB,  KPOME  *
010181 : C*  TИП=1) ПOMEЩAETCЯ B MACCИB:                               *
010182 : C*      *MONCRD*:,LC,12                                       *
010183 : C*  OCTATOK MACCИBA ЗAПOЛHЯETCЯ ПPOБEЛAMИ.                    *
010184 : C*    ДOПOЛHИTEЛЬHAЯ ИHФOPMAЦИЯ ПOMEЩAETCЯ (B ФOPMATE LOG) B  *
010185 : C*  MACCИB:                                                   *
010186 : C*      MIST10*:LC,BLOCK,MIS,CODE,NUMB                        *
010187 : C*  A TAKЖE ДYБЛИPYETCЯ B ИHДEKC-PEГИCTPAX:                   *
010188 : C*  CЛOBO ИP COДEPЖИMOE                                       *
010189 : C*   MIS  14  1 - OШИБKA B TEKYЩEM ПAPAMETPE                  *
010190 : C*            0 - HET OШИБOK                                  *
010191 : C*   CODE 11  KOД PAЗДEЛИTEЛЯ (ISO), ИЛИ HYЛЬ ПPИ BЫXOДE HA   *
010192 : C*            KOHEЦ KAPTЫ (72-Я ПOЗИЦИЯ), ПPИ ЭTOM TEPЯETCЯ   *
010193 : C*            ИHФOPMAЦИЯ O KOДE И HAЛИЧИИ PAЗДEЛИTEЛЯ B ПO-   *
010194 : C*            CЛEДHEЙ ПOЗИЦИИ KAPTЫ.                          *
010195 : C*   NUMB 12  KOЛИЧECTBO CИMBOЛOB ПAPAMETPA, ПEPEПИCAHHЫX B   *
010196 : C*            *MONCRD*                                        *
010197 : C*    ПPИ OБHAPYЖEHИИ OШИБKИ B ПAPAMETPE ПEЧATAETCЯ ДИAГHOC-  *
010198 : C*  TИKA:                                                     *
010199 : C*      'OШИБKA B ЗAДAHИИ ПAPAMETPA'                          *
010200 : C*  И ЗATEM ПEPBOE CЛOBO MACCИBA  *MONCRD*. ЭTY ПEЧATЬ MOЖHO  *
010201 : C*  ЗAБЛOKИPOBATЬ, ЗAДAB HEHYЛEBOЙ KOД B MACCИBE:             *
010202 : C*      OUT*OUT:,LC,1                                         *
010203 : C*    KOГДA PEЗYЛЬTAT OБPAБOTKИ YMEЩAETCЯ B OДHO CЛOBO (TИП=  *
010204 : C*  2,3,4,5,7),  OH  BЫДAETCЯ  HA  CYMMATOP.  KPOME TOГO ДЛЯ  *
010205 : C*  TИП=4,5,7  PEЗYЛЬTAT  ЗAПИCЫBAETCЯ ПO AДPECY,  ЗAДAHHOMY  *
010206 : C*  ПPИ OБPAЩEHИИ B I9.                                       *
010207 : C*    ECЛИ ПPИ OБPAЩEHИИ ЗAДAH HEHYЛEBOЙ KOД B MACCИBE:       *
010208 : C*      STARSTA:,LC,1                                         *
010209 : C*  TO KAЖДЫЙ CИMBOЛ ПAPAMETPA B ИCXOДHOЙ KAPTE, ЗA ИCKЛЮЧE-  *
010210 : C*  HИEM HEЗHAЧAЩИX ПPOБEЛOB, ЗAMEHЯETCЯ CИMBOЛOM '*'.        *
010211 : C*    BOЗBPAT ИЗ ПOДПPOГPAMMЫ ПPOИЗBOДИTCЯ ПO I13. ECЛИ CYM-  *
010212 : C*  MATOP HE ИCПOЛЬЗYETCЯ ДЛЯ BЫДAЧИ PEЗYЛЬTATA,  OH OБHYЛЯ-  *
010213 : C*  ETCЯ. ECЛИ HE БЫЛO ПEЧATИ ДИAГHOCTИKИ,  TO XPAHЯЩИE PAЗ-  *
010214 : C*  PЯДЫ PAY COXPAHЯЮTCЯ, ИHAЧE PRINT8 YCTAHOBИT CTAHДAPTHЫЙ  *
010215 :  ,SPACE,63
010216 : C*  PAY. YCTAHABЛИBAETCЯ ПPИЗHAK ЛOГИЧECKOЙ ГPYППЫ.  BCE ИH-  *
010217 : C*  ДEKCHЫE PEГИCTPЫ, HE ИCПOЛЬЗYЮЩИECЯ ДЛЯ BЫДAЧИ PEЗYЛЬTA-  *
010218 : C*  TOB, COXPAHЯЮTCЯ.                                         *
010219 : C*                                                            *
010220 : C*                     ПOДPOБHOE OПИCAHИE                     *
010221 : C*                     ------------------                     *
010222 : C*    1. HAЧAЛЬHAЯ YCTAHOBKA (TИП=9).                         *
010223 : C*  C ПOMOЩЬЮ П/П SIMFAIL* B MACCИBE:                         *
010224 : C*      XPARAMT*:,LC,4                                        *
010225 : C*  ФOPMИPYETCЯ  XAPAKTEPИCTИKA MACCИBA  MONCARD/  (ПPИ ЭTOM  *
010226 : C*  YKAЗATEЛЬ TEKYЩEЙ ПOЗИЦИИ  YCTAHABЛИBAETCЯ B HAЧAЛO KAP-  *
010227 : C*  TЫ), YCTAHABЛИBAЮTCЯ:                                     *
010228 : C*      CODE=1 , NUMB=MIS=0 .                                 *
010229 : C*    2. ПAPAMETP-TEKCT (TИП=0,1,2,3,6,8).                    *
010230 : C*    2.1. TИП=6.                                             *
010231 : C*  ECЛИ HA BXOДE CODE=0 (БЫЛ KOHEЦ KAPTЫ), BЫXOД C NUMB=0 ,  *
010232 : C*  ИHAЧE  ПOCЛEДOBATEЛЬHO  ПPOCMATPИBAЮTCЯ CИMBOЛЫ ИCXOДHOЙ  *
010233 : C*  KAPTЫ HAЧИHAЯ C TEKYЩEЙ ПOЗИЦИИ ДO ПEPBOГO  PAЗДEЛИTEЛЯ,  *
010234 : C*  A ECЛИ EГO HET, TO ДO KOHЦA KAPTЫ, И ПEPEПИCЫBAЮTCЯ, ИC-  *
010235 : C*  KЛЮЧAЯ ПPOБEЛЫ, B  *MONCRD* . YKAЗATEЛЬ  TEKYЩEЙ ПOЗИЦИИ  *
010236 : C*  YCTAHABЛИBAETCЯ ЗA PAЗДEЛИTEЛЬ. PAЗДEЛИTEЛЬ OПPEДEЛЯETCЯ  *
010237 : C*  ПO ИHФOPMAЦИИ ЗAДAHHOЙ B ПOДПPOГPAMME-TAБЛИЦE TABSIMIS .  *
010238 : C*    2.2. TИП=0.                                             *
010239 : C*  BЫПOЛHЯETCЯ (1.), A ЗATEM (2.1.).                         *
010240 : C*    2.3. TИП=1.                                             *
010241 : C*  TO ЖE, ЧTO И (2.1.), HO B  *MONCRD* ПEPEПИCЫBAЮTCЯ TAKЖE  *
010242 : C*  И ПPOБEЛЫ.                                                *
010243 : C*    2.4. TИП=2.                                             *
010244 : C*  ПPИ OБPAЩEHИИ  HA CYMMATOPE HYЖHO  ЗAДATЬ ЭTAЛOH  (ШECTЬ  *
010245 : C*  CИMBOЛOB B KOДE ISO). BЫПOЛHЯETCЯ (2.1.), И ECЛИ  NUMB=0  *
010246 : C*  ИЛИ NUMB>6, BЫXOД C CYMMATOPOM PABHЫM 1 (ПPИ NUMB>6 ФИK-  *
010247 : C*  CИPYETCЯ OШИБKA), ИHAЧE  OШИБKA ФИKCИPYETCЯ ПPИ HECOBПA-  *
010248 : C*  ДEHИИ ПAPAMETPA C ЭTAЛOHOM, A PEЗYЛЬTAT  CPABHEHИЯ BЫДA-  *
010249 : C*  ETCЯ HA CYMMATOP.                                         *
010250 : C*    2.5. TИП=3.                                             *
010251 : C*  BЫПOЛHЯETCЯ (2.1.). ECЛИ NUMB>8  ИЛИ ПEPBЫЙ CИMBOЛ ПAPA-  *
010252 : C*  METPA HE БYKBA, ФИKCИPYETCЯ OШИБKA. ПEPBЫE BOCEMЬ CИMBO-  *
010253 : C*  ЛOB ПAPAMETPA B KOДE TEXT BЫДAЮTCЯ HA CYMMATOP.           *
010254 : C*    2.6. TИП=8.                                             *
010255 : C*  ПPИ OБPAЩEHИИ ДOПOЛHИTEЛЬHO HYЖHO ЗAДATЬ B I11 KOД OДHO-  *
010256 : C*  ГO ИЗ PAЗДEЛИTEЛEЙ, A HA CYMMATOPE  ПPEДEЛЬHYЮ ДЛИHY ПA-  *
010257 : C*  PAMETPA  (B  ФOPMATE  LOG).  BЫПOЛHЯETCЯ  PAБOTA  KAK  B  *
010258 : C*  (2.1.), HO PAЗДEЛИTEЛEM CЧИTAETCЯ TOЛЬKO ЗAДAHHЫЙ. KPOME  *
010259 : C*  TOГO, ПPИ ДЛИHE ПAPAMETPA БOЛЬШEЙ ПPEДEЛЬHOЙ ФИKCИPYETCЯ  *
010260 : C*  OШИБKA.                                                   *
010261 : C*    3. ПAPAMETP-ЧИCЛO (TИП=4,5,7).                          *
010262 : C*  ПPИ OБPAЩEHИИ B I9 ЗAДAETCЯ AДPEC ДЛЯ ЗAПИCИ PEЗYЛЬTATA.  *
010263 : C*  BЫПOЛHЯETCЯ (2.1.), И ECЛИ  NUMB=0 ИЛИ  NUMB>6 , BЫXOД C  *
010264 : C*  HYЛEBЫM  CYMMATOPOM  (ПPИ  NUMB>6  ФИKCИPKETCЯ  OШИБKA),  *
010265 : C*  ИHAЧE BЫПOЛHЯETCЯ ПPEOБPAЗOBAHИE ПAPAMETPA B ФOPMAT LOG,  *
010266 : C*  И ECЛИ ПPИ ЭTOM HE ЗAФИKCИPOBAHO OШИБOK, PEЗYЛЬTAT BЫДA-  *
010267 : C*  ETCЯ HA CYMMATOP И ЗAПИCЫBAETCЯ ПO ЗAДAHHOMY AДPECY. ПPИ  *
010268 : C*  HAЛИЧИИ OШИБOK BЫXOД C HYЛEBЫM CYMMATOPOM.                *
010269 : C*    3.1. TИП=4.                                             *
010270 : C*  YKAЗATEЛИ MЛ/MД/MБ ПPEOБPAЗYЮTCЯ B EДИHЫЙ ФOPMAT:         *
010271 : C*  PAЗPЯДЫ COДEPЖИMOE                                        *
010272 : C*   18-13  M = M2 M1                                         *
010273 : C*          MATEMATИЧECKИЙ HOMEP YCTPOЙCTBA                   *
010274 : C*   12-1   Z = Z4 Z3 Z2 Z1                                   *
010275 : C*          HOMEP ЗOHЫ/TPAKTA                                 *
010276 : C*  ГДE: MI ,ZI - ЦИФPЫ BOCЬMEPИЧHOЙ ЗAПИCИ.                  *
010277 :  ,SPACE,35
010278 : C*  ДOПYCKAЮTCЯ CЛEДYЮЩИE TИПЫ YKAЗATEЛEЙ:                    *
010279 : C*  NUMB   ПAPAMETP              ПPEOБPAЗOBAHИE  OГPAHИЧEHИЯ  *
010280 : C*   6  <M2><M1><Z4><Z3><Z2><Z1>                  Z4<4        *
010281 : C*   5  <M2><M1><Z3><Z2><Z1>     Z4=0                         *
010282 : C*   3  F<F2><F1>                M=F2*10+F1+40B   M>40B       *
010283 : C*                               Z=0              M<60B       *
010284 : C*   2  F<F1>                    M=F1+40B , Z=0   M>40B       *
010285 : C*      <M2><M1>                 Z=0                          *
010286 : C*   1  1                        M=3 , Z=0                    *
010287 : C*      2                        M=5 , Z=0                    *
010288 : C*  ЗДECЬ: <MI> , <ZI>  - TEKCTOBЫE ПPEДCTABЛEHИЯ BOCЬMEPИЧ-  *
010289 : C*  HЫX, <FI>  - ДECЯTИЧHЫX ЦИФP.                             *
010290 : C*  ECЛИ ПAPAMETP HE OTHOCИTCЯ K OДHOMY ИЗ ПEPEЧИCЛEHHЫX TИ-  *
010291 : C*  ПOB ИЛИ HE YДOBЛETBOPЯET YKAЗAHHЫM OГPAHИЧEHИЯM,  ФИKCИ-  *
010292 : C*  PYETCЯ OШИБKA.                                            *
010293 : C*    3.2. TИП=5.                                             *
010294 : C*  ECЛИ HE BCE CИMBOЛЫ ПAPAMETPA - BOCЬMEPИЧHЫE ЦИФPЫ, ФИK-  *
010295 : C*  CИPYETCЯ OШИБKA.  PEЗYЛЬTAT - MAШИHHOE ПPEДCTABЛEHИE BЫ-  *
010296 : C*  ДEЛEHOOГO BOCЬMEPИЧHOГO ЧИCЛA.                            *
010297 : C*    3.3. TИП=7.                                             *
010298 : C*  ECЛИ HE BCE CИMBOЛЫ ПAPAMETPA - ЦИФPЫ, ФИKCИPYETCЯ OШИБ-  *
010299 : C*  KA. PEЗYЛЬTAT - MAШИHHOE ПPEДCTABЛEHИE BЫДEЛEHHOГO ДECЯ-  *
010300 : C*  TИЧHOГO ЧИCЛA.                                            *
010301 : C*    4. TИП=10.                                              *
010302 : C*  ПEPEД BЫДEЛEHИEM KAЖДOГO ПAPAMETPA XAPAKTEPИCTИKA MACCИ-  *
010303 : C*  BA  MONCARD/  KOПИPYETCЯ B MACИИB:                        *
010304 : C*      XPARAMT/:,LC,4                                        *
010305 : C*  ПPИ OБPAЩEHИИ C TИП=10  ПPOИCXOДИT BOCCTAHOBЛEHИE XAPAK-  *
010306 : C*  TEPИCTИKИ ИЗ ЭTOГO MACCИBA,  ЧTO ПOЗBOЛЯET  ЗATEM ПOBTO-  *
010307 : C*  PИTЬ OБPAБOTKY ЭTOГO ПAPAMETPA, ECЛИ ДOПYCKAЮTCЯ AЛЬTEP-  *
010308 : C*  HATИBHЫE TИПЫ. OДHAKO   MIST10*  HE BOCCTAHABЛИBAETCЯ, И  *
010309 : C*  ЭTO, B ЧACTHOCTИ, HE ПOЗBOЛЯET ПOBTOPИTЬ  OБPAБOTKY ПOC-  *
010310 : C*  ЛEДHEГO ПAPAMETPA HA KAPTE.                               *
010311 : C*                                                            *
010312 : C**************************************************************
010313 :  TABSIMIS:,SUBP,
010314 :  SIMFAIL*:,SUBP,
010315 :  SIMFAIL*:I,BLOCK,DO*LENT,(1),M1,W*P1,R*P1
010316 :  ,CONT,(6),L1*TO*L2
010317 :  STARSTA:,LC,1
010318 :  OUT*OUT:,LC,1
010319 :  MONCARD/:,LC,14
010320 :  *MONCRD*:LC,BLOCK,MK(12)
010321 :  MIST10*:,LC,3
010322 :  *0:,BLOCK,MIS,CODE,NUMB
010323 :  XPARAMT*:LC,BLOCK,PX(4)
010324 :  XPARAMT/:LC,BLOCK,PX/(4).KOПИЯ XAP-KИ
010325 : C
010326 : C          OПИCAHИЯ PEГИCTPOB
010327 : C      ЛOKAЛЬHЫE +
010328 : C      ИHTEPФEЙC C ПOЛЬЗOBATEЛEM
010329 :   *3:,BLOCK,B,I,P,H,R
010330 :  *10:,BLOCK,T,(1),N,J,M,S
010331 : C      ИHTEPФEЙC C SIMFAIL*
010332 :   *8:,BLOCK,X,A,L,F,(2),C
010333 : C
010334 : C      ЯЧEЙKИ MAГAЗИHA
010335 :  S*AC:,EQU,B-M-2
010336 :  S*AC:I,BLOCK,(1),S*M,(1),S*N
010337 :        ,CONT ,S*D,S*T,S*V
010338 : C
010339 : C------------
010340 : C
010341 :  BEG:,ITS,M
010342 :  M,VTM,M-B
010343 :  :M,UTM,-1
010344 :  M,ITS,B
010345 :  M,V1M,*-1
010346 :  B,BASE,BEG
010347 :  I,VTM,MIST10*
010348 :  P,VTM,1
010349 :  H,VTM,
010350 :  S,XTS,S*AC
010351 :  S,ATX,S*M
010352 :  T,MTJ,M
010353 :  M,UTM,15-ESWI+SWIT
010354 :   ,ITA,M
010355 :  S,AOX,S*T
010356 :   ,ASN,64+4
010357 :  I,ATX,MIS
010358 :  I,ATX,NUMB
010359 :  S,ATX,S*AC
010360 :  T,UZA,SWIT
010361 :  M,VTM,TEXA
010362 :   ,ITS,M
010363 :  M,UTM,5
010364 :   ,ITS,M
010365 :   ,XTS,=1
010366 :   ,CALL,PRINT8
010367 :   ,SJ ,
010368 :  SWIT:J,VTM,HOL.TИП=0
010369 :    ,UJ ,INIT.HACTPOЙKA
010370 :  :P,VTM,0.TИП=1
010371 :    ,UJ ,HOL.TEKCT C ПPOБ.
010372 :  :R,VTM,AEX.TИП=2
010373 :    ,UJ ,TEXT.CPABHEHИE
010374 :  :H,VTM,-48.TИП=3
010375 :    ,UJ ,IDE.ИДEHTИФИKATOP
010376 :  :R,VTM,MЛMБ.TИП=4
010377 :    ,UJ ,TEXT.YKAЗATEЛЬ
010378 :  :R,VTM,OCT.TИП=5
010379 :    ,UJ ,TEXT.BOCЬM.ЧИCЛO
010380 :  HOL:R,VTM,RET.TИП=6
010381 :    ,UJ ,TEXT.TEKCT БEЗ ПP.
010382 :  :R,VTM,INT.TИП=7
010383 :    ,UJ ,TEXT.ЦEЛOE
010384 :  :P,VTM,2.TИП=8
010385 :    ,UJ ,HOLS.ПOИCK CИMBOЛA
010386 :  :J,VTM,RET.TИП=9
010387 :    ,UJ ,INIT.HAЧ.YCTAHOBKA
010388 :  ESWI:X,VTM,PX/.TИП=10
010389 :  A,VTM,PX.BOЗBPAT HA 1 ПAP
010390 :  J,VTM,RET
010391 :   ,UJ ,L1*TO*L2
010392 : C------
010393 :  INIT:,XTA,=1
010394 :  I,ATX,CODE
010395 :  X,VTM,PX
010396 :  A,VTM,MONCARD/
010397 :  L,VTM,12
010398 :   ,UJ ,DO*LENT
010399 : C------
010400 :  TEXT:,BSS,
010401 :  X,VTM,PXX
010402 :  A,VTM,MK
010403 :  L,VTM,12
010404 :  J,VJM,DO*LENT
010405 :  M,VTM,-11
010406 :   ,XTA,=6H
010407 :  :A,ATX,
010408 :  A,UTM,1
010409 :  M,VLM,*-1
010410 :  X,VTM,PX.KOПИP.
010411 :  A,VTM,PX/.XAP-KИ
010412 :  J,VJM,L1*TO*L2
010413 :  I,XTA,CODE
010414 :   ,UZA,TERET
010415 :  TE1:X,VTM,PX
010416 :  J,VJM,R*P1
010417 :   ,ITA,F
010418 :  S,ATX,S*N
010419 :  J,VTM,TERET
010420 :  C,UTC,TABSIMIS
010421 :   ,XTA,
010422 :   ,ASN,64+45
010423 :   ,AEX,=1
010424 :   ,U1A,TEB
010425 :  P,MTJ,N
010426 :  N,UTM,-2
010427 :  N,V1M,TED
010428 :   ,ITA,C
010429 :  S,AEX,S*D
010430 :   ,UZA,TED
010431 :  TEB:P,VZM,TEP
010432 :  C,MTJ,N
010433 :  N,UTM,-40B
010434 :  N,VZM,TEN
010435 :  TEP:H,VZM,TEW
010436 :  C,UTC,TABSIMIS
010437 :   ,XTA,
010438 :   ,ASN,64-3
010439 :  H,ASN,64+48
010440 :  H,UTM,6
010441 :  S,AEX,S*AC
010442 :  S,ATX,S*AC
010443 :  TEW:X,VTM,PXX
010444 :  J,VJM,W*P1
010445 :   ,XTA,=1
010446 :  I,ARX,NUMB
010447 :  I,ATX,NUMB
010448 :   ,UTC,STARSTA
010449 :   ,XTA,
010450 :   ,UZA,TEN
010451 :  X,VTM,PX
010452 :  J,VJM,M1
010453 :  C,VTM,52B.*
010454 :  J,VJM,W*P1
010455 :  TEN:J,VTM,TE1
010456 :   ,UJ ,TEE
010457 :  TED:,ITA,C
010458 :  I,ATX,CODE
010459 :  TEE:S,XTA,S*N
010460 :  J,UZA,
010461 :   ,XTA,
010462 :  I,ATX,CODE
010463 :  TERET:M,VTM,MK
010464 :  I,XTA,NUMB
010465 :   ,ATI,N
010466 :  R,UJ ,
010467 : C------
010468 :  AEX:,BSS,
010469 :   ,XTS,=1
010470 :  S,STX,S*AC-1
010471 :  N,VZM,RET
010472 :   ,ARX,=1
010473 :   ,ASN,64+3
010474 :   ,U1A,MIST
010475 :  M,XTA,
010476 :  S,AEX,S*M
010477 :  S,ATX,S*AC
010478 :   ,UJ ,ZRET
010479 : C------
010480 :  IDE:,BSS,
010481 :  R,VJM,TEXT
010482 :  N,VZM,RET
010483 :   ,ARX,=7
010484 :   ,ASN,64+4
010485 :   ,U1A,MIST
010486 :  M,XTA,
010487 :   ,ASN,64+40
010488 :   ,ATI,N
010489 :  N,UTC,TABSIMIS
010490 :   ,XTA,
010491 :   ,ASN,64+45
010492 :  ZRET:,UZA,RET
010493 :   ,UJ ,MIST
010494 : C------
010495 :  MЛMБ:,BSS,
010496 :  N,VZM,RET
010497 :  M,XTA,
010498 :   ,AEX,=6H000000
010499 :  N,UTM,-6
010500 :  N,VLM,UN5
010501 :  MUN:S,ATX,S*M
010502 :   ,AAX,=6H'370''370''374''370''370''370'
010503 :  PAK:,U1A,MIST
010504 :  S,XTA,S*M
010505 :   ,APX,=6H'7''7''7''7''7''7'
010506 :   ,ASN,64+30
010507 :  SVA:S,ATX,S*AC
010508 :  S,WTC,S*V
010509 :   ,ATX,
010510 :   ,UJ ,RET
010511 :  UN5:J,VTM,MIST
010512 :  N,VLM,UN3
010513 :   ,AUX,=6H'377''377''''377''377''377'
010514 :   ,UJ ,MUN
010515 :  UN3:N,UTM,1
010516 :  N,VLM,UN2
010517 :  LUN:M,XTA,
010518 :   ,AEX,=3HF00
010519 :   ,ASN,64-8
010520 :  S,ATX,S*M
010521 :   ,YTA,
010522 :  J,U1A,
010523 :  S,XTA,S*M
010524 :  N,ASN,64+32
010525 :   ,UZA,MIST
010526 :  S,ATX,S*M
010527 : C-  CЛEДYЮЩИE ДBE KOMAHДЫ HE HYЖHЫ, TAK KAK
010528 : C-  KOДЫ 72-77 ПPИHAДЛEЖAT PAЗДEЛИTEЛЯM
010529 : C-          , ARX ,=6
010530 : C-         S, AOX ,S*M
010531 :   ,AAX,=6H'''''''''376''360'
010532 :   ,U1A,MIST
010533 :  S,XTA,S*M
010534 :   ,ASN,64+4
010535 :  :,UZA,*+1
010536 :   ,XTA,=12
010537 :  S,ARX,S*M
010538 :   ,AAX,=377
010539 :  S,ATX,S*M
010540 :   ,ASN,64+4
010541 :   ,U1A,MIST
010542 :   ,XTA,=37
010543 :  UNA:,ARX,=1
010544 :  S,ARX,S*M
010545 :   ,ASN,64-12
010546 :   ,UJ ,SVA
010547 :  UN2:N,VLM,UN1
010548 :  N,VTM,8
010549 :  J,VTM,MU2
010550 :   ,UJ ,LUN
010551 :  MU2:M,XTA,
010552 :   ,AEX,=2H00
010553 :   ,UJ ,MUN
010554 :  UN1:N,V1M,MIST
010555 :   ,ASN,64+39
010556 :  S,ATX,S*M.2,4
010557 :   ,ASN,64+1.1,2
010558 :   ,ARX,=377.0,1
010559 :   ,AAX,=376
010560 :   ,UZA,UNA
010561 :   ,UJ ,MIST
010562 : C------
010563 :  OCT:,BSS,
010564 :  J,VJM,DIG
010565 :   ,AAX,=6H'370''370''370''370''370''370'
010566 :   ,UJ ,PAK
010567 : C------
010568 :  INT:,BSS,
010569 :  J,VJM,DIG
010570 : C-  И ЭTИ ДBOE TOЖE ЛИШHИE
010571 : C-          , ARX ,=6H'6''6''6''6''6''6'
010572 : C-          , AOX ,S*M
010573 :   ,AAX,=6H'360''360''360''360''360''360'
010574 :   ,U1A,MIST
010575 :  INB:S,ATX,S*N
010576 :   ,ASN,64-2
010577 :  S,ARX,S*N
010578 :   ,ASN,64-1
010579 :  S,XTS,S*M-1
010580 :  L,ASN,64-8
010581 :   ,AAX,=377
010582 :  S,ARX,
010583 :  L,UTM,-8
010584 :  L,V1M,INB
010585 :   ,UJ ,SVA
010586 :  DIG:N,VZM,RET
010587 :   ,ARX,=1
010588 :   ,ASN,64+3
010589 :   ,U1A,MIST
010590 :   ,ITA,N
010591 :   ,ASN,64-3
010592 :   ,ATI,L.10-60B
010593 :   ,AEX,=377
010594 :   ,ATI,N.367-317B
010595 :  M,XTA,
010596 :   ,AEX,=6H000000
010597 :  N,ASN,64-317B
010598 :  S,ATX,S*M
010599 :  J,UJ ,
010600 : C------
010601 :  HOLS:,BSS,
010602 :  R,VJM,TEXT
010603 :   ,AEX,=377
010604 :   ,ARX,=1.400B-NUMB
010605 :  S,ARX,S*M
010606 :   ,ASN,64+8
010607 :   ,U1A,RET
010608 : C------
010609 :  MIST:,BSS,
010610 :   ,XTA,=1
010611 :  I,ATX,MIS
010612 :   ,UTC,OUT*OUT
010613 :   ,XTA,
010614 :   ,U1A,RET
010615 :  M,XTA,
010616 :   ,ATX,TEX2
010617 :  M,VTM,TEX1
010618 :   ,ITS,M
010619 :  M,VTM,TEX2
010620 :   ,ITS,M
010621 :   ,XTS,=1
010622 :   ,CALL,PRINT8
010623 : C------
010624 :  RET:,BSS,
010625 :  I,XTA,MIS
010626 :  S,ATX,S*M
010627 :  I,XTA,CODE
010628 :  S,ATX,S*D
010629 :  I,XTA,NUMB
010630 :  S,STX,S*N
010631 :  M,VTM,B-J
010632 :  :M,STI,J
010633 :  M,VLM,*
010634 :   ,STI,M
010635 :  J,UJ ,
010636 : C------------
010637 :  TEXA:,ISO,34HHEПPABИЛЬHOE ИCПOЛЬЗOBAHИE PARAMT*
010638 :  TEX1:,ISO,26HOШИБKA B ЗAДAHИИ ПAPAMETPA
010639 :  TEX2:,OCT,
010640 :  PXX:,BSS,4
010641 :  ,END,
010642 :  SIMFAIL*:,NAME,
010643 :  ,SPACE,62
010644 : C**************************************************************
010645 : C*                                                            *
010646 : C*       HAБOP ПPOЦEДYP ДЛЯ PAБOTЫ CO CTPOKAMИ CИMBOЛOB       *
010647 : C*       ----------------------------------------------       *
010648 : C*    HAЗOBEM CTPOKOЙ CИMBOЛOB ПOCЛEДOBATEЛЬHOCTЬ:            *
010649 : C*     S(1),S(2),...,S(N)                                     *
010650 : C*  ГДE: S(I)  -  BOCЬMИPAЗPЯДHЫE CИMBOЛЫ (000..377B),        *
010651 : C*       N=6*L -  ДЛИHA CTPOKИ (KPATHAЯ ШECTИ).               *
010652 : C*  ПPEДПOЛAГAETCЯ, ЧTO ПOCЛEДOBATEЛЬHOCTЬ YПAKOBAHA  (B ПO-  *
010653 : C*  PЯДKE CЛEДOBAHИЯ CИMBOЛOB) B MACCИBE  A  ИЗ  L  CЛOB, ПO  *
010654 : C*  ШECTЬ CИMBOЛOB B CЛOBE, HAЧИHAЯ CO CTAPШИX PAЗPЯДOB.      *
010655 : C*    ДЛЯ PAБOTЫ CO CTPOKOЙ HEOБXOДИMO TAKЖE BЫДEЛИTЬ MACCИB  *
010656 : C*  X  ИЗ ЧETЫPEX CЛOB (XAPAKTEPИCTИKA),  B KOTOPOM XPAHЯTCЯ  *
010657 : C*  ПAPAMETPЫ, OПИCЫBAЮЩИE COCTOЯHИE CTPOKИ.                  *
010658 : C*      EГO CTPYKTYPA TAKOBA:                                 *
010659 : C*  CЛOBO  PAЗPЯДЫ  COДEPЖИMOE                                *
010660 : C*    0     15-1     A                                        *
010661 : C*                   HAЧAЛЬHЫЙ AДPEC CTPOKИ                   *
010662 : C*    1     15-1     K=(I-1)/6                                *
010663 : C*                   OTHOCИTEЛЬHЫЙ AДPEC CЛOBA, COДEPЖAЩEГO   *
010664 : C*                   TEKYЩИЙ CИMBOЛ                           *
010665 : C*    2      7-1     ((K+1)*6-I)*8                            *
010666 : C*                   KOHCTAHTA ДЛЯ CДBИГA TEKYЩEГO CИMBOЛA B  *
010667 : C*                   B MЛAДШИE PAЗPЯДЫ CЛOBA (0..50B)         *
010668 : C*    3     15-1     A+L                                      *
010669 : C*                   KOHEЧHЫЙ AДPEC CTPOKИ+1                  *
010670 : C*  ГДE: I  -  YKAЗATEЛЬ TEKYЩEЙ ПOЗИЦИИ B CTPOKE (1..N)      *
010671 : C*                                                            *
010672 : C*                      OПИCAHИE ПPOЦEДYP                     *
010673 : C*                      -----------------                     *
010674 : C*    B BЫЗЫBAЮЩEЙ ПPOГPAMME ЭTИ ПPOЦEДYPЫ MOЖHO OПИCATЬ      *
010675 : C*  CЛEДYЮЩИM OБPAЗOM:                                        *
010676 : C*    SIMFAIL*: , SUBP,                                       *
010677 : C*    SIMFAIL*:I,BLOCK,DO*LENT  ,P1  ,M1                      *
010678 : C*              , CONT,W*P1,R*P1,W*M1,R*M1                    *
010679 : C*              , CONT,P1*W,P1*R,M1*W,M1*R                    *
010680 : C*              , CONT,L1*TO*L2 ,R   ,W                       *
010681 : C*              , CONT,L2*TO*L1                               *
010682 : C*  OБPAЩEHИE K HИM ПPOИЗBOДИTCЯ C BOЗBPATOM ПO I13,          *
010683 : C*  ПAPAMETPЫ ЗAДAЮTCЯ HA ИHДEKC-PEГИCTPAX,                   *
010684 : C*  ПPИЧEM B I8 BCEГДA ЗAДAETCЯ AДPEC XAPAKTEPИCTИKИ.         *
010685 : C*  BCEГДA COXPAHЯETCЯ CYMMATOP И XPAHЯЩИE PAЗPЯДЫ PAY,       *
010686 : C*  YCTAHABЛИBAETCЯ ПPИЗHAK ЛOГИЧECKOЙ ГPYППЫ.                *
010687 : C*  ECЛИ CПEЦИAЛЬHO HE OГOBOPEHO, ПOPTИTCЯ I12.               *
010688 : C*    1. DO*LENT  -  YCTAHOBOЧHOE OБPAЩEHИE                   *
010689 : C*  ФOPMИPYET XAPAKTEPИCTИKY CTPOKИ ПEPEД HAЧAЛOM PAБOTЫ,     *
010690 : C*  YCTAHABЛИBAET I=1.                                        *
010691 : C*  B I9   -  ЗAДAETCЯ A                                      *
010692 : C*    I10  -  ЗAДAETCЯ L                                      *
010693 : C*  ПPИMEP OБPAЩEHИЯ:                                         *
010694 : C*             8, VTM ,X                                      *
010695 : C*             9, VTM ,A                                      *
010696 : C*            10, VTM ,L                                      *
010697 : C*            13, VJM ,DO*LENT                                *
010698 : C*    2. KOПИPOBAHИE XAPAKTEPИCTИK                            *
010699 : C*  B I9   -  ЗAДAETCЯ AДPEC XAPAKTEPИCTИKИ  X2               *
010700 : C*  COXPAHЯETCЯ I12.                                          *
010701 : C*    2.1. L1*TO*L2  -  KOПИPYET X B X2                       *
010702 : C*  ПPИMEP OБPAЩEHИЯ:                                         *
010703 : C*             8, VTM ,X                                      *
010704 : C*             9, VTM ,X2                                     *
010705 : C*            13, VJM ,L1*TO*L2                               *
010706 :  ,SPACE,30
010707 : C*    2.2. L2*TO*L1  -  KOПИPYET X2 B X                       *
010708 : C*  ПPИMEP OБPAЩEHИЯ:                                         *
010709 : C*             8, VTM ,X                                      *
010710 : C*             9, VTM ,X2                                     *
010711 : C*            13, VJM ,L2*TO*L1                               *
010712 : C*    3. ЧTEHИE И ЗAПИCЬ TEKYЩEГO CИMBOЛA                     *
010713 : C*  B I14  -  CИMBOЛ                                          *
010714 : C*  ПOPTЯTCЯ I9,I10                                           *
010715 : C*    3.1. R  -  ЧTEHИE CИMBOЛA ИЗ TEKYЩEЙ ПOЗИЦИИ            *
010716 : C*               И ЗAПИCЬ B I14                               *
010717 : C*    3.2. W  -  ЗAПИCЬ CИMBOЛA ИЗ I14 B TEKYЩYЮ ПOЗИЦИЮ      *
010718 : C*    4. ИЗMEHEHИE YKAЗATEЛЯ TEKYЩEЙ ПOЗИЦИИ                  *
010719 : C*  ПPИ ПOПЫTKE BЫЙTИ ЗA ГPAHИЦЫ CTPOKИ YCTAHABЛИBAETCЯ       *
010720 : C*  I11=1 (ИHAЧE I11=0)                                       *
010721 : C*  ПOPTИTCЯ I9                                               *
010722 : C*    4.1. P1  -  YBEЛИЧEHИE YKAЗATEЛЯ HA EДИHИЦY             *
010723 : C*  ECЛИ B PEЗYЛЬTATE I CTAЛ PABHЫM N+1, BЫXOД C I11=1        *
010724 : C*    4.2. M1  -  YMEHЬШEHИE YKAЗATEЛЯ HA EДИHИЦY             *
010725 : C*  ECЛИ I БЫЛ PABEH 1, YCTAHABЛИBAET I=6, И BЫXOД C I11=1    *
010726 : C*    5. KOMПOЗИЦИИ OПEPAЦИЙ TИПA 3 И 4                       *
010727 : C*    5.1. R*P1  -  BЫПOЛHЯET  R  , A ЗATEM  P1               *
010728 : C*    5.2. R*M1  -  BЫПOЛHЯET  R  , A ЗATEM  M1               *
010729 : C*    5.3. W*P1  -  BЫПOЛHЯET  W  , A ЗATEM  P1               *
010730 : C*    5.4. W*M1  -  BЫПOЛHЯET  W  , A ЗATEM  M1               *
010731 : C*    5.5. P1*R  -  BЫПOЛHЯET  P1 , A ЗATEM  R                *
010732 : C*    5.6. M1*R  -  BЫПOЛHЯET  M1 , A ЗATEM  R                *
010733 : C*    5.7. P1*W  -  BЫПOЛHЯET  P1 , И ECЛИ I11=0 , TO  W      *
010734 : C*    5.8. M1*W  -  BЫПOЛHЯET  M1 , И ECЛИ I11=0 , TO  W      *
010735 : C*                                                            *
010736 : C**************************************************************
010737 : C      XAPAKTEPИCTИKA
010738 :  *0:,BLOCK,A*,K*,H*,L*
010739 : C      PEГИCTPЫ
010740 :  *8:,BLOCK,X,A,L,F,B,J,C,S
010741 : C
010742 : C------------
010743 : C
010744 :  B,BASE,C377
010745 :   ,UJ ,DO*LENT
010746 :  P1:B,VTM,C377
010747 :   ,UJ ,P1*
010748 :  M1:B,VTM,C377
010749 :   ,UJ ,M1*
010750 :  :,ITS,J
010751 :   ,UJ ,W*P1
010752 :  :,ITS,J
010753 :   ,UJ ,R*P1
010754 :  :,ITS,J
010755 :   ,UJ ,W*M1
010756 :  :,ITS,J
010757 :   ,UJ ,R*M1
010758 :  :,ITS,J
010759 :   ,UJ ,P1*W
010760 :  :,ITS,J
010761 :   ,UJ ,P1*R
010762 :  :,ITS,J
010763 :   ,UJ ,M1*W
010764 :  :,ITS,J
010765 :   ,UJ ,M1*R
010766 :  :X,XTS,L*
010767 :   ,UJ ,L1*TO*L2
010768 :  R:L,VTM,R*
010769 :   ,UJ ,GET*KH
010770 :  W:L,VTM,W*
010771 :   ,UJ ,GET*KH
010772 :  :A,XTS,A*.L2*TO*L1
010773 : C------
010774 :  X,ATX,A*
010775 :  A,XTA,K*
010776 :  X,ATX,K*
010777 :  A,XTA,H*
010778 :  X,ATX,H*
010779 :  A,XTA,L*
010780 :   ,UJ ,RT*1
010781 : C------
010782 :  DO*LENT:,ITS,A
010783 :  X,ATX,A*
010784 :   ,XTA,
010785 :  X,ATX,K*
010786 :   ,XTA,=50
010787 :  X,ATX,H*
010788 :   ,ITA,L
010789 :  X,ARX,A*
010790 :  RT*1:X,STX,L*
010791 :  J,UJ ,
010792 : C------
010793 :  W*P1:J,VTM,*P1
010794 :   ,UJ ,W
010795 :  R*P1:J,VJM,R
010796 :  *P1:,STI,J
010797 :  P1*:X,XTS,H*
010798 :   ,ATI,A
010799 :  A,UTM,-8
010800 :   ,U1A,RT*2
010801 :  A,VTM,40
010802 :   ,XTA,=1
010803 :  X,ARX,K*
010804 :  X,ATX,K*
010805 :  X,ARX,A*
010806 :  X,AEX,L*
010807 :   ,U1A,RT*2
010808 :   ,UTC,1
010809 :  RT*2:F,VTM,
010810 :   ,ITA,A
010811 :  X,STX,H*
010812 :  J,UJ ,
010813 : C------
010814 :  W*M1:J,VTM,*M1
010815 :   ,UJ ,W
010816 :  R*M1:J,VJM,R
010817 :  *M1:,STI,J
010818 :  M1*:X,XTS,H*
010819 :   ,ATI,A
010820 :   ,AEX,=50
010821 :  A,UTM,8
010822 :   ,U1A,RT*2
010823 :  X,ATX,H*
010824 :  X,XTA,K*
010825 :   ,UZA,OUT
010826 :   ,ARX,=77777
010827 :   ,AAX,=77777
010828 :  X,STX,K*
010829 :   ,UJ ,RT*3
010830 :  OUT:,STX,
010831 :   ,UTC,1
010832 :  RT*3:F,VTM,
010833 :  J,UJ ,
010834 : C------
010835 :  M1*W:J,VTM,*W
010836 :   ,UJ ,M1
010837 :  P1*W:J,VJM,P1
010838 :  *W:,STI,J
010839 :  F,VZM,W
010840 :  J,UJ ,
010841 :  M1*R:J,VTM,*R
010842 :   ,UJ ,M1
010843 :  P1*R:J,VJM,P1
010844 :  *R:,STI,J
010845 :   ,UJ ,R
010846 : C------
010847 :  GET*KH:B,VTM,C377
010848 :  X,XTS,A*
010849 :  X,ARX,K*
010850 :   ,ATI,A
010851 :  X,XTA,H*
010852 :  L,UJ ,
010853 : C------
010854 :  R*:,ATI,L
010855 :  A,XTA,
010856 :  L,ASN,64
010857 :   ,AAX,C377
010858 :   ,STI,C
010859 :  J,UJ ,
010860 : C------
010861 :  W*:,AEX,=77777
010862 :   ,ATI,L
010863 :   ,XTA,C377
010864 :  L,ASN,64+1
010865 :  A,AAX,
010866 :  A,AEX,
010867 :   ,ITS,C
010868 :  L,ASN,64+1
010869 :  S,AEX,
010870 :   ,UJ ,RT*4
010871 : C------
010872 :  L1*TO*L2:A,ATX,L*
010873 :  X,XTA,H*
010874 :  A,ATX,H*
010875 :  X,XTA,K*
010876 :  A,ATX,K*
010877 :  X,XTA,A*
010878 :  RT*4:A,STX,.A*
010879 :  J,UJ ,
010880 : C------------
010881 :  C377:,LOG,377
010882 :   ,END,
010883 :  TABSIMIS:,NAME,. 10.04.73
010884 :  ,SPACE,17
010885 : C**************************************************************
010886 : C*                                                            *
010887 : C*      TAБЛИЦA XAPAKTEPИCTИK CИMBOЛOB ДЛЯ KOДИPOBKИ ISO      *
010888 : C*      ------------------------------------------------      *
010889 : C*   XAPAKTEPИCTИKA CИMBOЛA C KOДOM I (0..177B) COДEPЖИTCЯ B  *
010890 : C*  I-TOM CЛOBE TAБЛИЦЫ, B CЛEДYЮЩEM ФOPMATE:                 *
010891 : C*  PAЗPЯДЫ  COДEPЖИMOE                                       *
010892 : C*   48-46    TИП CИMBOЛA:                                    *
010893 : C*            0 - БYKBA ( + CИMBOЛЫ '*' ,'/' И ПPOБEЛ)        *
010894 : C*            2 - BOCЬMEPИЧHAЯ ЦИФPA (0..7)                   *
010895 : C*            3 - ДECЯTИЧHAЯ ЦИФPA (8,9)                      *
010896 : C*            1 - PAЗДEЛИTEЛЬ (BCE OCTAЛЬHЫE CИMBOЛЫ)         *
010897 : C*   45-40    KOД TEXT, ECЛИ OH CYЩECTBYET ДЛЯ ДAHHOГO CИM-   *
010898 : C*            BOЛA, ИHAЧE HYЛЬ                                *
010899 : C*   39-1     HYЛЬ (И ЭTO CYЩECTBEHHO)                        *
010900 : C*                                                            *
010901 : C**************************************************************
010902 : C-------------------------------------------
010903 : C        KOД  :     :KOД  :
010904 : C        ISO  :  TИП:TEXT : CИMBOЛ
010905 : C-------------------------------------------
010906 :  00,OCT,1
010907 :  01,OCT,1
010908 :  02,OCT,1
010909 :  03,OCT,1
010910 :  04,OCT,1
010911 :  05,OCT,1
010912 :  06,OCT,1
010913 :  07,OCT,1
010914 :  10,OCT,1
010915 :  11,OCT,1
010916 :  12,OCT,1
010917 :  13,OCT,1
010918 :  14,OCT,1
010919 :  15,OCT,1
010920 :  16,OCT,1
010921 :  17,OCT,1
010922 :  20,OCT,1
010923 :  21,OCT,1
010924 :  22,OCT,1
010925 :  23,OCT,1
010926 :  24,OCT,1
010927 :  25,OCT,1
010928 :  26,OCT,1
010929 :  27,OCT,1
010930 :  30,OCT,1
010931 :  31,OCT,1
010932 :  32,OCT,1
010933 :  33,OCT,1
010934 :  34,OCT,1
010935 :  35,OCT,1
010936 :  36,OCT,1
010937 :  37,OCT,1
010938 :  TABSIM:40,OCT,.ПPOБEЛ
010939 :  41,OCT,1.BOCKЛ.ЗH
010940 :  42,OCT,1.KABЫЧKA
010941 :  43,OCT,1.HEPABHO
010942 :  44,OCT,1.ДOЛЛAP
010943 :  45,OCT,1.%
010944 :  46,OCT,1.&
010945 :  47,OCT,1.' (AПOCTPOФ)
010946 :  50,OCT,1.(
010947 :  51,OCT,1.)
010948 : C-------------------------------------------
010949 : C        KOД  :     :KOД  :
010950 : C        ISO  :  TИП:TEXT : CИMBOЛ
010951 : C-------------------------------------------
010952 :  SIM*:52,OCT,0 12.*
010953 :  53,OCT,1 36.+
010954 :  54,OCT,1.,
010955 :  55,OCT,1 35.-
010956 :  56,OCT,1.TOЧKA
010957 :  SIM/:57,OCT,0 17./
010958 :  60,OCT,2 20.0
010959 :  61,OCT,2 21.1
010960 :  62,OCT,2 22.2
010961 :  63,OCT,2 23.3
010962 :  64,OCT,2 24.4
010963 :  65,OCT,2 25.5
010964 :  66,OCT,2 26.6
010965 :  67,OCT,2 27.7
010966 :  70,OCT,3 30.8
010967 :  71,OCT,3 31.9
010968 :  72,OCT,1.:
010969 :  73,OCT,1.;
010970 :  74,OCT,1.<
010971 :  75,OCT,1.=
010972 :  76,OCT,1.>
010973 :  77,OCT,1.BOПP.ЗH
010974 :  100,OCT,1.@ (AT)
010975 :  101,OCT,0 41.A
010976 :  102,OCT,0 42.B
010977 :  103,OCT,0 43.C
010978 :  104,OCT,0 44.D
010979 :  105,OCT,0 45.E
010980 :  106,OCT,0 46.F
010981 :  107,OCT,0 47.G
010982 :  110,OCT,0 50.H
010983 :  111,OCT,0 51.I
010984 :  112,OCT,0 52.J
010985 :  113,OCT,0 53.K
010986 :  114,OCT,0 54.L
010987 :  115,OCT,0 55.M
010988 :  116,OCT,0 56.N
010989 :  117,OCT,0 57.O
010990 :  120,OCT,0 60.P
010991 :  121,OCT,0 61.Q
010992 :  122,OCT,0 62.R
010993 :  123,OCT,0 63.S
010994 :  124,OCT,0 64.T
010995 :  125,OCT,0 65.U
010996 :  126,OCT,0 66.V
010997 :  127,OCT,0 67.W
010998 :  130,OCT,0 70.X
010999 :  131,OCT,0 71.Y
011000 :  132,OCT,0 72.Z
011001 :  133,OCT,1.[
011002 :  134,OCT,1.OБP.KOCAЯ
011003 :  135,OCT,1.]
011004 :  136,OCT,1.HAДЧEPK.
011005 :  137,OCT,1.ПOДЧEPK.
011006 :  140,OCT,0 77.Ю
011007 :  141,OCT,1
011008 :  142,OCT,0 02.Б
011009 :  143,OCT,0 03.Ц
011010 :  144,OCT,0 04.Д
011011 :  145,OCT,1
011012 : C-------------------------------------------
011013 : C        KOД  :     :KOД  :
011014 : C        ISO  :  TИП:TEXT : CИMBOЛ
011015 : C-------------------------------------------
011016 :  146,OCT,0 05.Ф
011017 :  147,OCT,0 06.Г
011018 :  150,OCT,1
011019 :  151,OCT,0 07.И
011020 :  152,OCT,0 13.Й
011021 :  153,OCT,1
011022 :  154,OCT,0 14.Л
011023 :  155,OCT,1
011024 :  156,OCT,1
011025 :  157,OCT,1
011026 :  160,OCT,0 34.П
011027 :  161,OCT,0 15.Я
011028 :  162,OCT,1
011029 :  163,OCT,1
011030 :  164,OCT,1
011031 :  165,OCT,1
011032 :  166,OCT,0 16.Ж
011033 :  167,OCT,1
011034 :  170,OCT,0 32.Ь
011035 :  171,OCT,0 37.Ы
011036 :  172,OCT,0 40.З
011037 :  173,OCT,0 73.Ш
011038 :  174,OCT,0 74.Э
011039 :  175,OCT,0 75.Щ
011040 :  176,OCT,0 76.Ч
011041 :  177,OCT,1
011042 :  ,END,
000001 :  REDUOL:,NAME,
000002 :  'DECLARATION'
000003 :  *REGIM//:,LC,1.HAXOДИTCЯ B PEЗИДEHTE MOHИTOPA,OCTOPOЖHO C ПEPECБOPKOЙ!
000004 :  AC:,EQU,8.10B
000005 :  AS:,EQU,9.11B
000006 :  AU:,EQU,10.12B
000007 :  MODE:,EQU,11.13B
000008 :  REGIM:,EQU,12.14B
000009 :  SECENTRY:,EQU,13.15B
000010 :  ALF1:,EQU,14.16B
000011 :  ALF2:,EQU,15.17B
000012 :  OVFLJUMP:,EQU,16.20B
000013 :  IR1:,EQU,17.21B
000014 :  IR12:,EQU,28.34B
000015 :  IR13:,EQU,29.35B
000016 :  NDEF:,EQU,30.36B
000017 :  NSET:,EQU,31.37B
000018 :  NDATA:,EQU,32.40B
000019 :  R:,EQU,33.41B
000020 :  DEFADR:,EQU,34.42B
000021 :  LEX:,EQU,35.43B
000022 :  ALFA:,EQU,36.44B
000023 :  RLEX:,EQU,37.45B
000024 :  WLEX:,EQU,38.46B
000025 :  RCAT:,EQU,39.47B
000026 :  WCAT:,EQU,40.50B
000027 :  LI14:,EQU,41.51B
000028 :  LR:,EQU,42.52B
000029 :  AEX:,EQU,43.53B
000030 :  CARWORD:,EQU,46.56B
000031 :  AR:,EQU,47.57B
000032 :  RLOAD:,EQU,49.61B
000033 :  QNAME:,EQU,50.60B
000034 :  RELADR:,EQU,51.63B
000035 :  ERROFLAG:,EQU,52.64B
000036 :  LOCSUBP:,EQU,53.65B
000037 :  INFSUB:,EQU,2.2B
000038 :  TYPE:,EQU,3.3B
000039 :  STR:,EQU,4.4B
000040 :  STR1:,EQU,5.5B
000041 :  STR2:,EQU,6.6B
000042 :  NBODY:,EQU,7.7B
000043 :  LOCLIST:,EQU,ADRCAT
000044 :  BITDEBUG:,EQU,CFIX
000045 :  WORDADR:,EQU,54.66B
000046 :  PR:,EQU,55.67B
000047 :  ASUBP:,EQU,57.71B
000048 :  SHIFR:,EQU,58.72B
000049 :  RETN:,EQU,LI14.
000050 :  C2000:,EQU,C1PAGE.
000051 :  CCOMN:,EQU,INFMSK.
000052 :  AP:,EQU,ALF2.
000053 :  ADATA:,EQU,56.70B
000054 :  LOADLIBR:,EQU,60
000055 :  LTAPES:,EQU,59
000056 :  NAME:,EQU,QNAME.
000057 :  INSTR:,EQU,TYPE.
000058 :  MENDATA:,EQU,NBODY.
000059 :  MENDSUB:,EQU,NDATA.
000060 :  ADR:,EQU,INFSUB.
000061 :  ADR1:,EQU,TYPE.
000062 :  ADR2:,EQU,STR.
000063 :  ADR3:,EQU,STR1.
000064 :  ADR4:,EQU,STR2.
000065 :  MRESPAGE:,EQU,C1777.
000066 :  DTUPFLG:,EQU,PR.ЗAПPET DATA UP
000067 :  CSUBP:,EQU,LIBMASK.
000068 :  WRITFIX:,EQU,ERROFIX.
000069 :  READFIXS:,EQU,ERROFIX.
000070 :  SKIPFIXS:,EQU,ERROFIX.
000071 :  STORFIXS:,EQU,ERROFIX.
000072 :  PUTFIXS:,EQU,ERROFIX.
000073 :  CHEKFIXS:,EQU,ERROFIX.
000074 :  PAREXP:,EQU,ERROFIX.
000075 :  PWORD:,EQU,ERROFIX.
000076 :  BSHIFT:,EQU,W+1776B.HAЧAЛO CДBИГAEMЫX AДPECOB
000077 :  CSHIFT:,EQU,W+1777B.HAЧAЛO CДBИHYTЫX AДPECOB
000078 : C         ЭTИ KOHCTAHTЫ ЗAHOCЯTCЯ ПPИ
000079 : C         *CALL LDSHIFT ДЛЯ ЗAГPYЗKИ HA
000080 : C         HOPMAЛЬHO HEДOCTYПHЫE AДPECA
000081 : C
000082 :  'INSTRUCTION'                            .
000083 :   W:,BSS,
000084 :             ,ITA,                         .
000085 :           11,VTM,-2                       .
000086 :  PACKDIST:  ,ASN,64-15                    .
000087 :           11,XTS,AU                       .
000088 :  ,AAX,M3L.
000089 :           15,AOX,                         .
000090 :  11,VLM,PACKDIST.
000091 :  ,ATX,ALF1.
000092 :  ,XTA,LISTEND.
000093 :  ,A-X,BEGLIST.
000094 :             ,ASN,64-30                    .
000095 :             ,AOX,AP                       .
000096 :  ,XTS,LISTMAX
000097 :  ,X-A,LOCLIST
000098 :  ,ASN,64-15
000099 :  15,AOX,
000100 :             ,ATX,ALF2                     .
000101 :  ,XTA,ERROFLAG.
000102 :  ,U1A,CHEKLIBS.
000103 :  ,XTA,ALFA
000104 :  ,ARX,
000105 :  ,UZA,SKIPPR.
000106 :  PRINT:,BSS,
000107 :  ,XTA,*REGIM//
000108 :  ,ASN,64+47
000109 :  ,ATI,8
000110 :  ,XTA,LISTEND
000111 :  ,A-X,SECLIST.
000112 :  ,ASN,64+2
000113 :  8,V1M,P2
000114 :  ,ASN,64+1.EЩE HA 2,ECЛИ B 4 CTOЛБЦA
000115 :  P2:,BSS,
000116 :  ,ARX,C1.
000117 :  ,ATI,7.ЧИCЛO CTPOK HA ПEЧATИ
000118 :  ,ASN,64-1
000119 :  ,ATX,DPRINT.ШAГ BЫБOPKИ ИЗ CПИCKA ЗAГPYЗKИ
000120 :  ,PRINT,INF64
000121 :  LP1:,XTA,SECLIST.
000122 :  ,ATX,WORDADR.
000123 :  9,VTM,BUF.
000124 :  8,VTM,-1
000125 :  ,XTA,*REGIM//
000126 :  ,ARX,
000127 :  ,UZA,LP2
000128 :  8,VTM,-3.ECЛИ B 4 CTOЛБЦA
000129 :  LP2:,XTA,WORDADR.
000130 :  ,A-X,LISTEND.
000131 :  ,UIA,LP20
000132 : C  CHENDL:  , BSS ,
000133 :  ,AOX,
000134 :  ,UIA,LP2E
000135 :  ,XTA,FRIE
000136 :  5,VTM,ALF1
000137 :  ,UJ,LP2F
000138 :  LP20:,BSS,
000139 :  ,XTA,WORDADR
000140 :  14,VJM,READWORD.
000141 :  ,AAX,MSKER.
000142 :  ,AEX,ERROR.
000143 :  ,UZA,SETERROR.
000144 :  5,XTA,.
000145 :  LP2F:9,ATX,1
000146 :  5,XTA,1
000147 :  ,ASN,64-33
000148 :  ,AUX,CUA.
000149 :  ,AOX,C50.
000150 :  9,ATX,2
000151 :  5,XTA,1
000152 :  ,AAX,LIBMASK.
000153 :  ,U1A,LP2E.
000154 :  9,XTA,2
000155 :  ,AOX,CC.
000156 :  9,ATX,2
000157 :  5,XTA,1
000158 :  ,AAX,INFMSK.
000159 :  ,U1A,LP2E.
000160 :  ,XTA,CTE.
000161 :  9,ARX,2
000162 :  9,ATX,2
000163 :  LP2E:9,UTM,4
000164 :  ,XTA,WORDADR
000165 :  ,ARX,DPRINT.
000166 :  ,ATX,WORDADR
000167 :  8,VLM,LP2.
000168 :  14,VJM,PRINT6.
000169 :  ,XTA,SECLIST.
000170 :  ,ARX,C2.
000171 :  ,ATX,SECLIST.
000172 :  7,UTM,-1.
000173 :  7,V1M,LP1.
000174 :  ,PRINT,INF64
000175 :  SKIPPR:,BSS,.
000176 :  DRUMOFF:,XTA,NLIST.
000177 :             ,ATI,1                        .
000178 :  ,A-X,NCAT.
000179 :             ,ATI,10                       .
000180 :  WRLIST:1,XTA,TCOR.
000181 :  ,AAX,CHM.IF IN MEMORY(BY IR IN TCOR)
000182 :  ,UZA,WSK.
000183 :  1,XTA,TCOR.
000184 :  ,AAX,CMEM.
000185 :  ,ITS,1.
000186 :             ,ASN,64-34                    .
000187 :           15,ARX,                         .
000188 :  ,AAX,CMEM.
000189 :  ,ATX,FRIEPAGE.
000190 :  14,VJM,SETFDR.
000191 :  :14,VJM,WRITDRUM.
000192 :  WSK:1,UTM,1.
000193 :           10,UTM,1                        .
000194 :  10,V1M,WRLIST.
000195 :  ,WTC,NPRT.
000196 :  1,VTM,.
000197 :  RDMEM:1,XTA,TCOR.
000198 :  ,AAX,CHM.
000199 :  ,U1A,RSK.
000200 :  ,ITA,1.
000201 :    ,ASN,64-34                             .
000202 :  ,ATX,FRIEPAGE.
000203 : C    CHFM:  , BSS ,
000204 :  14,VJM,SETFDR
000205 :  ,XTA,FRIEDR
000206 :  ,UIA,RDM
000207 :  ,ITA,1
000208 :  ,ASN,64-10
000209 :  ,A-X,C8
000210 :  ,XTS,AP
000211 :  ,AAX,M3L
000212 :  15,X-A,
000213 :  ,UZA,RSK
000214 :  14,VTM,RSK
000215 :  ,UJ,ZEROS1
000216 :  RDM:14,VJM,READDRUM.
000217 :  RSK:1,UTM,-1.
000218 :  1,V1M,RDMEM.
000219 :  EXIT:1,VTM,-2.
000220 :  STORETN:1,XTA,CRETN+2
000221 :  1,CTX,RETN+2.
000222 :  :1,VLM,STORETN.
000223 :  ,XTA,MODE.
000224 :  ,UZA,SKM1.
000225 :  ,ATI,14
000226 :  ,XTA,ALF1
000227 :  14,ATX,5
000228 :  ,XTA,ALF2
000229 :  14,ATX,6
000230 :  ,XTA,SECENTRY
000231 :  14,ATX,7
000232 :  ,XTA,SRETN.
000233 :  ,CTX,RETN+2.
000234 :  SKM1:,XTA,ERROFLAG
000235 :  ,UZA,SKM2.
000236 : C ERROCOND: , BSS ,
000237 :  ,XTA,ABSFLG
000238 :  ,UIA,SRCHLIBS
000239 :  ,XTA,ERETN
000240 :  ,CTX,RETN+2
000241 :  SKM2:14,VTM,-12
000242 :  RESTIRS :14,XTA,IR13                     .
000243 :           14,ATI,13                       .
000244 :  14,VLM,RESTIRS.
000245 :  ,UJ,RETN.
000246 :  CRETN:,DRUM,RLEX.
000247 :          :  ,STX,                         .
000248 :             ,XTR,REGIM                    .
000249 :             ,WTC,SECENTRY                 .
000250 :             ,UJ ,                         .
000251 :  SRETN   :13,UJ ,                         .
000252 :  ERETN:,UJ,OVFLJUMP
000253 :  SETERROR:5,XTA,
000254 :  ,AAX,M37.
000255 :  ,ATI,14
000256 :  14,UTM,ERTEXTS.
000257 :  14,XTA,.
000258 :  9,ATX,
000259 :  14,XTA,1
000260 :  9,ATX,1
000261 :  14,XTA,2
000262 :  9,ATX,2
000263 :  5,XTA,
000264 :  ,AAX,M40.
000265 :  ,U1A,LP2E.
000266 :  5,XTA,1
000267 :  9,ATX,2
000268 :  ,UJ,LP2E.
000269 :  PRINT6:,ITA,14
000270 :  ,ITS,
000271 :  1,VTM,-14
000272 :  11,VTM,BUF.
000273 :  10,VTM,BUFP.
000274 :  4,VTM,-5.
000275 :  RW:2,VTM,-7
000276 :  RS:11,XTA,
000277 :  ,ASN,64-6
000278 :  11,ATX,
000279 :  ,YTA,
000280 :  5,VTM,-1
000281 :  SCHA:,ATI,14
000282 :  ,A-X,C6.
000283 :  5,UTM,1
000284 :  ,UZA,SCHA.
000285 :  5,XTA,TABL.
000286 :  14,UTM,1.
000287 :  SCHS:,ASN,64-8
000288 :  14,UTM,-1
000289 :  14,V1M,SCHS.
000290 :  ,YTA,.
000291 :  10,XTS,
000292 :  ,ASN,64-8
000293 :  15,AOX,
000294 :  10,ATX,
000295 :  4,VLM,ENDRS.
000296 :  10,UTM,1.
000297 :  4,VTM,-5
000298 :  ENDRS:2,VLM,RS.
000299 :  11,UTM,1.
000300 :  1,VLM,RW.
000301 :  ,XTA,CSTR.
000302 :  10,ATX,.
000303 :  ,PRINT,INF6.
000304 :  ,XTA,
000305 :  14,VTM,-14
000306 :  :11,UTM,-1
000307 :  11,ATX,
000308 :  14,VLM,*-1.
000309 :  15,WTC,.
000310 :  ,UJ,
000311 :  INF6:10,Z00,-20
000312 :  10,Z00,-20
000313 :  ,001,
000314 :  8,,
000315 :  INF64:,Z00,LAB
000316 :  ,Z00,LAB
000317 :  ,000,
000318 :  8,000,
000319 :  BUFP:,BSS,6
000320 :  BUF:,BSS,15
000321 :  TABL    :  ,OCT,0365 5441 1542 2064      .
000322 :          :  ,OCT,1062 4022 0461 4451      .
000323 :          :  ,OCT,1263 7046 0300 0001      .
000324 :          :  ,OCT,0040 1404 0120 3007      .
000325 :          :  ,OCT,0200 4473 0542 7413      .
000326 :          :  ,OCT,0243 5047 1002 1061      .
000327 :          :  ,OCT,1762 2500 2022 6502      .
000328 :          :  ,OCT,2062 5104 1304 2456      .
000329 :          :  ,OCT,1404 3107 2203 1111      .
000330 :          :  ,OCT,2244 5465 1464 6070      .
000331 :          :  ,OCT,1703 4467 1720 7414      .
000332 :  CSTR:,OCT,462
000333 :  MSKER:,OCT,7777 7777 7777 7700
000334 :  CC:,TEXT,2H C
000335 :  LAB:,GOST,2H'34''231'
000336 :  CTE:,OCT,0002
000337 :  C6:,LOG,6
000338 :  DPRINT:,OCT,
000339 :  ERTEXTS:,BSS,
000340 :  ,TEXT,8H ДЛИHHЫЙ
000341 :  ,TEXT,8H CПИCOK
000342 :  ,TEXT,8H   ДЛИHH
000343 :  ,TEXT,8HЫЙ AДPEC
000344 :  ,TEXT,8H   OTCYT
000345 :  ,TEXT,8HCTBYET
000346 :  ,TEXT,8H HEBEPHA
000347 :  ,TEXT,8HЯ ДЛИHA
000348 :  ,TEXT,8H ПEPEПOЛ
000349 :  ,TEXT,8HHEHИE S-
000350 :  ,TEXT,8HOБЛACTИ
000351 :  ,TEXT,8H     MAЛ
000352 :  ,TEXT,8HO ПAMЯTИ
000353 :  ,TEXT,8H
000354 :  ,TEXT,8H OШИБKA
000355 :  ,TEXT,8HB PACCЫЛ
000356 :  ,TEXT,8HKE DATA
000357 :  ,TEXT,8H ИCПOPЧE
000358 :  ,TEXT,8HHA ПOДПP
000359 :  ,TEXT,8HOГPAMMA
000360 :  ,TEXT,8H ПЛOXAЯ
000361 :  ,TEXT,8HЭKBИBAЛE
000362 :  ,TEXT,8HHTHOCTЬ
000363 :  FRIE:,TEXT,8HCBOБOДHO
000364 :  CHEKLIBS:,XTA,ABSFLG.
000365 :  ,UZA,PRINT.
000366 :  ,XTA,LTAPES.
000367 :  ,UZA,NOSRCH.
000368 :  ,XTA,TYESFLG.
000369 :  ,U1A,SKIPPR.
000370 :  NOSRCH:,ATX,ABSFLG.
000371 :  ,UJ,PRINT.
000372 :  SRCHLIBS:,DRUM,WPAGE1.
000373 :  13,VTM,W.ДЛЯ LOADLIBR
000374 :  ,DRUM,LOADLIBR.
000375 :  ,UJ,2000B.
000376 :  WPAGE1:,OCT,00000100 00200026.
000377 :  L       :14,VTM,                         .
000378 :  :14,VZM,MNENT.
000379 :  ,UJ,RELOAD.
000380 :  SPENT:,WTC,ALF2
000381 :  14,VTM,-10B
000382 :  14,ATX,
000383 :  ,XTA,ALF1
000384 :  14,ATX,1
000385 : C
000386 : C   БЫЛO:   ,XTA,ALF2
000387 : C
000388 : C.....   CПACAEM ФЛAГ ЗAПPETA PACCЫЛKИ
000389 : C        B BЫШECTOЯЩИE PAЗДEЛЫ (ДЛЯ OVERLAY)
000390 :  ,XTA,RETN
000391 :  ,ATX,DTUPFLG
000392 :  14,XTA,
000393 :  MNENT:15,ATX,
000394 :             ,RTE,177B                     .
000395 :             ,ATX,REGIM                    .
000396 :             ,ITA,14                       .
000397 :             ,ATX,MODE                     .
000398 :  14,VTM,-12
000399 :  *1:14,ITA,13
000400 :  14,ATX,IR13
000401 :  14,VLM,*1.
000402 :  ,NTR,3
000403 :  LOAD:,WTC,BSHIFT
000404 :  3,VTM,.HE 0- PAБOTA LDSHIFT
000405 :             ,XTA,SHIFR                    .
000406 :  ,UZA,SKIPSH.
000407 :  ,XTA,CWRITW.
000408 :  ,CTX,WRITW1.
000409 :  SKIPSH:,BSS,.
000410 :             ,XTA,                         .WITTHOUT FIXES
000411 :  ,ATX,HEADFLAG.
000412 :             ,ATX,ERROFLAG                 .
000413 :  ,ATX,MEMERFLG.
000414 :  ,XTA,OVFLJUMP
000415 :  ,ATX,SAVE20.
000416 :  ,XTA,ERJUMP.
000417 :  ,CTX,OVFLJUMP
000418 :             ,XTA,ALF1                     .
000419 :             ,ASN,64-3                     .
000420 :           11,VTM,-2                       .
000421 :    ,*75,0000B
000422 :  STORDIST:  ,ASN,64-15                    .
000423 :           15,ATX,                         .
000424 :             ,YTA,                         .
000425 :           11,STX,AU                       .AC,AS,AU
000426 :  11,VLM,STORDIST.
000427 :  ,XTA,ALF2.
000428 :             ,ASN,64+30                    .
000429 :  ,ARX,BEGLIST.
000430 :  ,ATX,LISTEND.
000431 :  ,ATX,AQUEUE.
000432 :  ,ATX,SECLIST.
000433 :  ,XTA,ALF2
000434 :  ,ASN,64+15
000435 :  ,AAX,M3L
000436 :  ,X-A,LOCLIST
000437 :  ,ATX,LISTMAX
000438 :  ,XTA,ALF2
000439 :  ,AAX,M3L
000440 :  ,ATX,AP
000441 :  ,XTA,AU.
000442 :  ,ATI,2.
000443 :  ,XTA,AP.
000444 :  NZERO:,A-X,AU.
000445 :  ,ATI,4.
000446 :  14,VTM,INITDRUM.
000447 :  ,X-A,.
000448 :  ,AAX,MP.
000449 :  ,UZA,ZEROSET.
000450 :  ,XTA,AP.
000451 :  ,AAX,MP.
000452 :  ,ARX,C1777.
000453 :  ,ATI,2.
000454 :  ,X-A,AP.
000455 :  ,ATI,4.
000456 :  14,VTM,CLAU.
000457 :  ,UJ,ZEROSET.
000458 :  BEGLOAD:,STX,.
000459 :             ,ATX,QNAME                    .
000460 :  ,XTA,TSUBP.
000461 :             ,ATX,TYPE                     .
000462 :             ,ATX,CARWORD                  .
000463 :  ,XTA,
000464 :  ,XTS,
000465 :  14,VJM,INCRAP.
000466 :  14,VJM,CHEKLIST.
000467 :  :,XTA,ENTRY.
000468 :  ,ATX,SECENTRY.ENTRY IN SECTION.ORIG A IN M
000469 :  LOADQUE:,XTA,C1.
000470 :  ,ATX,NFIXP1.
000471 :  ,XTA,
000472 :  ,ATX,GLOBASE.
000473 :  CHEKQUE:,XTA,AQUEUE.
000474 :  ,AEX,LISTEND.
000475 :  ,UZA,ENDLOAD.
000476 :  ,XTA,AQUEUE.
000477 :  14,VJM,READWORD.
000478 :  :,ATX,NAME.
000479 :            5,XTA,1                        .
000480 :  ,ATX,LIBCODE.
000481 :  ,AAX,M3L.
000482 :             ,ATX,LOCSUBP                  .
000483 :  ,XTA,AQUEUE.
000484 :  ,ARX,C2.
000485 :  ,ATX,AQUEUE.
000486 :  PTSKIP  : 5,XTA,1                        .
000487 :  ,AAX,CSUBP.
000488 :  ,UZA,CHEKQUE.
000489 :            5,XTA,1                        .
000490 :  ,AAX,CFIX.
000491 :  ,U1A,PUTFIXS.
000492 :  HCH:,XTA,HEADFLAG.
000493 :  ,U1A,HEADSKIP.
000494 :  14,VJM,LIBR.
000495 :  HEADSKIP:10,VTM,-2.
000496 :  READSTR:14,VJM,READWP1.
000497 :  :10,STX,STR+2.
000498 :  10,VLM,READSTR.
000499 :  ,ITA,.
000500 :  ,ATX,HEADFLAG.
000501 :             ,ITA,                         .
000502 :             ,ATX,NBODY                    .
000503 :             ,XTA,STR+2                    .FOR BSS
000504 :             ,ASN,64+30                    .
000505 :             ,ARX,STR+2                    .
000506 :  ,AAX,M3L.
000507 :             ,ATX,NBODY                    .INSTR.+CONST.
000508 :             ,XTA,LOCSUBP                  .
000509 :  ,AAX,M3L.
000510 :             ,ATX,LOCSUBP                  .
000511 :          :10,VTM,-2                       .
000512 :             ,XTA,STR+1                    .
000513 :  CTDATA  :15,ATX,                         .
000514 :  ,AAX,M3L.
000515 :           10,ATX,NSET+2                   .3 CELLS: NSET,NDATA,R
000516 :             ,STX,NDEF                     .
000517 :             ,ASN,64+15                    .
000518 :  10,VLM,CTDATA.
000519 :  ,XTA,RELADR.
000520 :             ,ATX,ASUBP                    .
000521 :             ,ARX,NBODY                    .
000522 :             ,ATX,ADATA                    .
000523 :             ,ARX,NDATA                    .
000524 :             ,ARX,NSET                     .
000525 :             ,ATX,DEFADR                   .
000526 :  ,ARX,NFIXP1.
000527 :             ,ATX,RELADR                   .
000528 :  ,XTA,CDEFADR.
000529 :             ,ATX,WORDADR                  .
000530 :             ,XTS,LOCSUBP                  .
000531 :  14,VJM,WRITWP1.WRITE MAINE ENTRY ADDRESS
000532 :  :,XTA,STR.AND INCREASE WORDADR BY 1
000533 :  ,AAX,M4S.
000534 :             ,ATI,10                       .
000535 :  14,VTM,ERSKIP.
000536 :  ,AEX,NFIXP1.SETSFIRSTQUE
000537 :  ,U1A,ERROFIX.INCORRECT FIX NUMBER
000538 :  ERSKIP:10,UTM,-1.
000539 :  10,V1M,WRITFIX.IF THERE ARE FIXES
000540 :  CHEKDEF:,XTA,STR.SHIFT M,READFIXS AND SAVE A
000541 :             ,ASN,64+12                    .
000542 :  ,AAX,M4S.NUMBER OF DEFINITIONS
000543 :             ,ATI,10                       .
000544 :  PRODEF:10,VZM,FORMINST.
000545 :  14,VJM,READWP1.
000546 :  :,ATX,CARWORD.
000547 :           10,UTM,-1                       .
000548 :  ,AAX,MTYPE.
000549 :             ,ASN,64+18                    .
000550 :             ,STX,TYPE                     .
000551 :  14,VJM,PUTNAME
000552 :  ,STX,QNAME
000553 :             ,WTC,TYPE                     .
000554 :  ,UJ,SWITCH.
000555 :  SWITCH:,UJ,PROCOMP.
000556 :  :,UJ,PROLAB.
000557 :  :,UJ,PROLOC.
000558 :  :,UJ,PROSUBP.
000559 :  :,UJ,PROS.
000560 :  :,UJ,PROP.
000561 :  :,UJ,PROP.
000562 :  :,UJ,PROC.
000563 :  PROLAB:14,VJM,REDLOWA.
000564 :  :,ARX,LOCSUBP.
000565 :  ,AAX,M3L.
000566 :  LDSH/WEX:,BSS,
000567 :  14,VTM,PROWEX
000568 :  3,V1M,LDSHIFT
000569 :  PROWEX:14,VJM,WRITWP1.
000570 :  14,VJM,LOCNAM
000571 :  :,UJ,PRODEF.
000572 :  PROCOMP:,XTA,CARWORD.
000573 :  ,AAX,CIDENT.
000574 :  ,U1A,PROEQU.IF EQUALITY
000575 :             ,XTA,CARWORD                  .PROCESS COMPAUND ADDRESS
000576 :             ,ASN,64+24                    .
000577 :  PROEQU  :  ,ATX,R                        .
000578 :  ,AAX,C4000.
000579 :  ,UZA,REDUSKIP.
000580 :             ,XTA,R                        .
000581 :  ,AAX,C3777.
000582 :  ,ARX,CDEFADR.
000583 :  14,VJM,READWORD.
000584 :  :,ATX,R.
000585 :  REDUSKIP:14,VJM,REDLOWA.
000586 :  ,XTS,CARWORD.
000587 :  ,AAX,CS.
000588 :  ,U1A,PROMUDIV.
000589 :             ,STX,                         .
000590 :          :  ,ARX,R                        .
000591 :  ,AAX,M3L.
000592 :  ,UJ,PROWEX.
000593 :  PROMUDIV:  ,XTA,CARWORD                  .
000594 :  ,AAX,CIDENT.
000595 :  ,U1A,PROWEQ.
000596 :  ,XTA,CARWORD
000597 :  ,AAX,CMINUS.
000598 :  ,U1A,PRODIV.
000599 :             ,XTA,R                        .
000600 :  ,AOX,CEXP.
000601 :           15,A*X,                         .
000602 :             ,YTA,64-40                    .
000603 :  ,AAX,M3L.
000604 :  ,UJ,PROWEX.
000605 :  PRODIV  :  ,STX,                         .
000606 :  ,AOX,CEXP.
000607 :             ,NTR,2                        .
000608 :             ,A+X,                         .
000609 :             ,XTS,R                        .
000610 :  ,AOX,CEXP.
000611 :             ,A+X,                         .
000612 :           15,A/X,                         .
000613 :             ,NTR,3                        .
000614 :  ,A+X,CEXP.
000615 :  ,AAX,M3L.
000616 :  ,UJ,PROWEX.
000617 :  PROWEQ: 15,XTA,-1
000618 :  ,A-X,AP
000619 :  ,UZA,ERWEQ.
000620 :  ,STX,
000621 :  14,VJM,READWORD.
000622 :  ,AAX,M3L.
000623 :  ,UJ,PROWEX.
000624 :  ERWEQ:14,VJM,ERROEQ.
000625 :  ,STX,.
000626 :  ,XTA,
000627 :  ,UJ,PROWEX.
000628 :  PROS    :  ,XTA,CARWORD                  .
000629 :  ,AAX,CSP.
000630 :  ,U1A,ERRORSC.
000631 :  PROLS   :  ,XTS,AS                       .ADDRESS OF ARRAY
000632 :  14,VJM,REDLOWA.
000633 :  :,ARX,AS.
000634 :             ,STX,AS                       .
000635 :             ,XTS,AC                       .
000636 :  ,AEX,CAL7.
000637 :             ,ARX,AS                       .
000638 :  ,UZA,SETS.
000639 :  14,VJM,ERRORSC.
000640 :  SETS:,STX,.ADDRESS OF ARRAY TO A
000641 :  ,UJ,PROWEX.
000642 :  PROP:14,VJM,REDLOWA.SHIFTS MAGAZINE
000643 :  :,XTS,CARWORD.
000644 :  ,AAX,CSP.MARK OF SEC.OR PAGE ARRAY
000645 :  14,VJM,INCRAP.CORRECTING OF P-FIELD
000646 :  :,UJ,PROWEX.ADDR OF ARRAY IN A
000647 :  PROC:14,VJM,PUTNAME.
000648 :  :14,VJM,REDLOWA.
000649 :  :,ASN,64-15.SHIFT IN POSITION N BODY
000650 :             ,STX,INFSUB                   .LENGTH
000651 :             ,ATX,QNAME                    .
000652 :  ,AEX,/GLOBAS/.
000653 :  ,U1A,SKIPBAS.
000654 :  ,XTA,INFSUB
000655 :  ,ASN,64-5
000656 :  ,ATX,GLOBASE.
000657 :  ,UJ,PROWEX.
000658 :  SKIPBAS:,BSS,
000659 :  14,VJM,CHEKLIST.PROCESS AS SUBPROGRAM
000660 :  :,XTA,ENTRY.
000661 :  ,UJ,LDSH/WEX
000662 :  PROLOC:,XTS,CARWORD.
000663 :  ,AAX,CS.
000664 :  ,U1A,PROPAR.
000665 :             ,STX,                         .
000666 :  ,UJ,PROSUBP.
000667 :  PROPAR  :  ,STX,                         .
000668 :  14,VJM,REDLOWA.
000669 :  ,AOX,CEXT.
000670 :  ,UJ,PROWEX.
000671 :  PROSUBP:,XTS,C1.
000672 :  ,STX,NFIXP1.
000673 :  14,VJM,REDLOWA.
000674 :  :,U1A,READFIXS.IF THERE ARE FIXES
000675 :  ,STX,.RESET MAG
000676 :  PRONAME:14,VJM,PUTNAME.
000677 :  ,ATX,QNAME.
000678 : C     ПPИЗHAK OTЛAДKИ ПPOCTABЛЯETCЯ
000679 : C     ПPИ  *CALL DEBUGER
000680 :  ,UJ,SKIPBAS
000681 :  READWP1:,XTS,RELADR.
000682 :           15,ATX,                         .
000683 :  ,ARX,C1.
000684 :             ,STX,RELADR                   .
000685 :  ,UJ,READWORD.
000686 :  WRITWP1:  ,XTS,WORDADR                   .
000687 :           15,ATX,                         .
000688 :  ,ARX,C1.
000689 :             ,STX,WORDADR                  .
000690 :  ,UJ,WRITWORD.RETURN IN IR14
000691 :  REDLOWA :14,MTJ,9                        .
000692 :             ,XTS,CARWORD                  .
000693 :  ,AAX,M3L.
000694 :            ,XTS,CARWORD                   .
000695 :  ,AAX,CDEF.
000696 :  ,UZA,CHMINUS.
000697 :             ,STX,                         .
000698 :  ,ARX,CDEFADR.
000699 :  14,VJM,READWORD.
000700 :  :,XTS,.
000701 :  CHMINUS :  ,XTA,CARWORD                  .
000702 :  ,AAX,CMINUS.
000703 :  ,UZA,LOWEX.
000704 :             ,XTA,CARWORD                  .FOR * /
000705 :  ,AAX,CS.
000706 :  ,U1A,LOWEX.
000707 :             ,STX,                         .
000708 :  ,AEX,M3L.
000709 :  ,ARX,C1.
000710 :  ,AAX,M3L.
000711 :             ,XTS,                         .
000712 :  LOWEX   :  ,STX,                         .
000713 :            9,UJ ,                         .RETURN FROM REDLOWA
000714 :  INCRAP  :14,MTJ,9                        .SUBP. FOR CORRECTING P-FIELD
000715 :  ,UZA,PROLP.IF NOT PAGE ARRAY
000716 :  ,XTS,C1777.
000717 :  ,STX,CSMSK.
000718 :  ,AAX,CS.
000719 :  ,UZA,IFNOTS.
000720 :  ,XTA,C377.
000721 :  ,ATX,CSMSK.
000722 :  IFNOTS  :  ,BSS,                         .
000723 :             ,XTA,AP                       .
000724 :  ,AAX,CSMSK.
000725 :  ,UZA,PROLP.
000726 :             ,XTS,AP                       .
000727 :  ,ARX,CSMSK.
000728 :  ,ARX,C1.
000729 :           15,AEX,                         .
000730 :             ,ATX,AP                       .
000731 :  PROLP   :  ,STX,                         .
000732 :             ,ATX,AR                       .ARRAY LENGTH
000733 :             ,XTA,AP                       .
000734 :           15,ATX,                         .
000735 :             ,ARX,AR                       .
000736 :             ,ATX,AP                       .
000737 :             ,XTA,AU                       .
000738 :  ,AEX,CAL7.
000739 :             ,ARX,AP                       .
000740 :  ,UZA,SETP.
000741 :  14,VJM,ERRORPU.
000742 :  SETP:,STX,.ADDRESS OF ARRAY TO A FROM M
000743 :            9,UJ ,                         .RETURN FROM INCRAP
000744 :  PUTNAME :  ,XTS,CARWORD                  .
000745 :  ,AAX,CIDENT
000746 :  14,UZA,
000747 :  ,XTA,CARWORD
000748 :  ,AAX,CLONGNAM.
000749 :  ,U1A,LONGNAME.
000750 :             ,XTA,CARWORD                  .
000751 :  ,AAX,UPPERMSK.
000752 :           14,UJ ,                         .RETURN FROM PUTNAME
000753 :  CHEKLIST:  ,ITA,14                       .
000754 :  ,ATX,LISTRETN.
000755 :  ,XTA,BEGLIST.
000756 :  ,ATX,LB.
000757 :  ,XTA,LISTEND.
000758 :  ,ATX,LE.
000759 :  14,VJM,SLIST.
000760 :  :,XTA,AQ.
000761 :  ,UZA,COMPLECT.
000762 :  CHEKINF:5,XTA,1.
000763 :             ,ATX,R                        .
000764 :  ,AAX,M3L.
000765 :  ,ATX,ENTRY.
000766 :            5,XTA,1                        .
000767 :  ,AAX,CFIX.
000768 :  ,U1A,CHEKFIXS.
000769 :             ,XTA,TYPE                     .
000770 :  ,AEX,TCOMN.
000771 :  ,U1A,ELSKIP.
000772 :  CHPAGE  :  ,XTA,CARWORD                  .
000773 :  ,AAX,CSP.
000774 :  ,UZA,CHLENGTH.
000775 :             ,XTA,R                        .
000776 :  ,AAX,C377.
000777 :  ,U1A,ERLG.
000778 :             ,XTA,CARWORD                  .
000779 :  ,AAX,CS.
000780 :  ,U1A,CHLENGTH.
000781 :             ,XTA,R                        .
000782 :  ,AAX,C1777.
000783 :  ,U1A,ERLG.
000784 :  CHLENGTH:  ,XTA,R                        .
000785 :             ,ASN,64+15                    .
000786 :  ,AAX,M3L.
000787 :  ,AEX,CAL7.
000788 :             ,XTS,INFSUB                   .
000789 :             ,ASN,64+15                    .
000790 :  ,AAX,M3L.
000791 :           15,ARX,                         .
000792 :  ,UZA,ELSKIP.
000793 :  ERLG:14,VJM,ERLENGTH.
000794 :  ELSKIP:,XTA,R.
000795 :  EXLIST:,WTC,LISTRETN.
000796 :             ,UJ ,                         .RETURN FROM CHEKLIST
000797 :  COMPLECT:  ,XTA,TYPE                     .
000798 :  ,AEX,TCOMN.
000799 :  ,U1A,SUBP.
000800 :             ,XTA,QNAME                    .
000801 :  ,AAX,MSKSMB.
000802 :  ,AEX,CSTAR.
000803 :  ,U1A,COMMON.
000804 :  SUBP:,XTA,ADRCAT.
000805 :  ,ATX,LB.
000806 :  ,XTA,CATEND.
000807 :  ,ATX,LE.
000808 :  14,VJM,SLIST.
000809 :  :,XTA,AQ.
000810 :  ,U1A,COMPL.
000811 :  ,XTA,TYPE.
000812 :  ,AEX,TCOMN.
000813 :  ,UZA,COMMON.
000814 :  14,VTM,EXLIST.
000815 :  ,UJ,NAMEERRO.
000816 :  COMPL:,ARX,C1.
000817 :  ,ATX,AQ.
000818 :  5,XTS,1.
000819 :  ,AAX,M3L.
000820 :  ,ATX,ENTRY.
000821 :            5,UTM,1                        .
000822 :            5,XTA,                         .
000823 :  ,UJ,SKR.
000824 :  CMPLP   :15,ATX,                         .
000825 :  14,VJM,READWORD.
000826 :  SKR:,AAX,INFMSK.
000827 :  ,U1A,STORINF.IF MAIN ENTRY
000828 :  ,STX,.
000829 :  ,A-X,C2.
000830 :  ,ATX,AQ.
000831 :  ,UJ,CMPLP.SEARCH MAIN ENTRY
000832 :  STORINF :  ,XTA,TYPE                     .
000833 :  ,AEX,TCOMN.
000834 :  ,UZA,BDATA.
000835 :  STINF   : 5,XTA,                         .
000836 :  STINF1:,AAX,FCLEAN.
000837 :             ,ATX,INFSUB                   .
000838 :             ,ASN,64+15                    .
000839 :  ,AAX,M3L.
000840 :             ,STX,R                        .RESET MAGAZINE
000841 :  ,UJ,STOLIST.
000842 :  BDATA   : 5,XTA,                         .
000843 :             ,ASN,64+15                    .
000844 :  ,AAX,M3L.
000845 :             ,XTS,INFSUB                   .
000846 :             ,ASN,64+15                    .
000847 :  ,AAX,M3L.
000848 :           15,A-X,                         .
000849 :  ,U1A,STINF.
000850 :             ,ASN,64-15                    .
000851 :            5,ARX,                         .
000852 :  ,UJ,STINF1.
000853 :  COMMON  :  ,XTA,                         .
000854 :  ,ATX,ENTRY.
000855 :             ,XTA,INFSUB                   .
000856 :             ,ASN,64+15                    .
000857 :             ,ATX,R                        .LENGTH
000858 :  ,UJ,STL1.
000859 :  STOLIST :  5,XTA,-1                      .
000860 :             ,ATX,QNAME                    .
000861 :  STL1    :  ,XTA,R                        .
000862 :             ,XTS,CARWORD                  .
000863 :  ,AAX,CSP.
000864 :  14,VJM,INCRAP.
000865 :  :,ATX,R.ADDRESS
000866 :  ,A+X,ENTRY.
000867 :  ,ATX,ENTRY.
000868 :             ,XTA,INFSUB                   .
000869 :  ,AAX,M3L.
000870 :  ,UZA,ZEROENTR.
000871 :             ,XTA,INFSUB                   .
000872 :  ,AAX,MLOCN.ERASE ENTRY
000873 :             ,AOX,R                        .
000874 :  ,XTS,PB.PHIZICAL BEGIN
000875 :  14,VJM,WRITWEND.
000876 :  :,XTA,INFSUB.
000877 :  ,AAX,M3L.
000878 :             ,ATX,INFSUB                   .
000879 :  ZEROENTR:  ,XTA,INFSUB                   .
000880 :             ,ARX,R                        .
000881 :             ,XTS,QNAME                    .
000882 :  SLP:14,VJM,WRITWEND.
000883 :  ,XTA,INFSUB.
000884 :  ,UZA,EXLIST.
000885 :  ,XTA,AQ.
000886 :  ,A+X,C2.
000887 :  ,ATX,AQ.
000888 :  14,VJM,READWORD.
000889 :  :,AAX,INFMSK.
000890 :  ,U1A,EXLIST.
000891 :  5,XTA,.
000892 :             ,ARX,R                        .
000893 :            5,XTS,-1                       .
000894 :  ,UJ,SLP.
000895 :  WRITWEND:  ,ITS,14                       .
000896 :  ,ATX,WREX.
000897 :  ,XTA,LISTEND.
000898 :  ,A-X,LISTMAX.
000899 :  ,U1A,ERWRSKIP.
000900 :  14,VJM,ERROLIST.
000901 :  ERWRSKIP:,XTA,LISTEND.
000902 :  15,ATX,.
000903 :  ,ARX,C2.
000904 :  ,STX,LISTEND.
000905 :  ERWRSK2:,BSS,
000906 :  14,VJM,WRITWORD.
000907 :  :5,ATX,1.
000908 :  ,WTC,WREX.
000909 :             ,UJ ,                         .
000910 :  LOCNAM:,BSS,
000911 :  ,XTA,QNAME
000912 :  14,UZA,
000913 :  ,XTA,ALFA
000914 :  ,AAX,BITDEBUG
000915 :  14,UZA,
000916 :  ,XTA,TYPE
000917 :  ,AEX,TSUBP
000918 :  14,UZA,
000919 :  ,XTA,TYPE
000920 :  ,AEX,TCOMN
000921 :  14,UZA,
000922 : C        ИДEHTИФИKATOPЫ ПOПAДAЮT B CПИCOK
000923 : C        ЛOKAЛЬHЫX ИДEHTИФИKATOPOB, ECЛИ
000924 : C        ECTЬ ПPИЗHAK OTЛAДKИ И OHИ HE
000925 : C        COMMON ИЛИ SUBP.
000926 :  ,XTA,AQUEUE
000927 :  ,A-X,BEGLIST
000928 :  ,A-X,C2
000929 :  ,ASN,64-6
000930 :  ,AOX,TYPE
000931 :  ,ASN,64-18
000932 :  5,AOX,
000933 :  ,XTS,QNAME
000934 :  ,ITS,14
000935 :  ,ATX,WREX
000936 :  ,XTA,LISTEND
000937 :  ,A-X,LISTMAX
000938 :  ,UZA,ERWRSKIP-1
000939 :  ,XTA,LISTMAX
000940 :  ,A-X,C2
000941 :  ,ATX,LISTMAX
000942 :  ,UJ,ERWRSK2
000943 :   LDSHIFT:,ATX,R
000944 :  ,A-X,BSHIFT
000945 :  ,U1A,NOSHIFT
000946 :  ,A+X,CSHIFT
000947 :  ,AAX,M3L
000948 :  14,UJ,
000949 :  NOSHIFT:,XTA,R
000950 :  14,UJ,
000951 :  SLIST   :  ,ITS,14                       .
000952 :             ,ITS,11                       .
000953 :             ,XTS,                         .
000954 :  ,ATX,AQ.
000955 :  ,XTA,LB.
000956 :  SLEND:,A-X,LE.
000957 :  ,UZA,SEX.
000958 :           15,ATX,                         .
000959 :             ,ASN,64+1                     .
000960 :             ,STI,11                       .
000961 :  ,A+X,C2000.
000962 :  ,UZA,ENDJP.
000963 :           11,VTM,-512                     .
000964 :  ENDJP:,XTA,LB.
000965 :  14,VJM,READWORD.
000966 :  :11,UTM,1.
000967 :             ,AEX,QNAME                    .
000968 :  LSTLP:,UZA,NORMEX.
000969 :            5,XTA,2                        .
000970 :             ,AEX,QNAME                    .
000971 :            5,UTM,2                        .
000972 :  11,VLM,LSTLP.
000973 :  ,XTA,LB.
000974 :  ,A+X,C2000.
000975 :  ,ATX,LB.
000976 :  ,UJ,SLEND.
000977 :  NORMEX:,ITA,1.
000978 :             ,ASN,64-10                    .
000979 :             ,ITS,5                        .
000980 :  ,AAX,C1777.
000981 :           15,AOX,                         .
000982 :  ,ATX,AQ.
000983 :  SEX     :  ,STX,                         .
000984 :             ,STI,11                       .
000985 :             ,STI,14                       .
000986 :           14,UJ ,                         .
000987 :  FORMINST:,XTA,MEMERFLG.
000988 :  ,U1A,LOADQUE.
000989 :  ,XTA,STR
000990 :             ,ASN,64+24                    .
000991 :  ,AAX,M4S.
000992 :  ,U1A,PAREXP.
000993 :            ,XTA,STR2                      .
000994 :             ,ATI,10                       .NUMBER OF INSTR. WORDS
000995 :             ,XTA,LOCSUBP                  .
000996 :             ,ATX,WORDADR                  .
000997 :             ,XTA,ASUBP                    .
000998 :             ,ATX,RELADR                   .
000999 :  WORDLOOP:10,VZM,SETDATA.
001000 :  14,VJM,READWP1.
001001 :  :,STX,CARWORD.
001002 :            9,VTM,-1                       .
001003 :  INSTLOOP:  ,XTA,CARWORD                  .
001004 :             ,ASN,64+24                    .
001005 :             ,ATX,INSTR                    .
001006 :  ,AAX,CLADR.
001007 :  ,U1A,LINSTR.IF LONG ADDRESS
001008 :  ,XTA,INSTR
001009 :  ,AAX,C4000
001010 :  ,U1A,MDEF
001011 :             ,XTA,INSTR                    .
001012 :  ,AAX,CMADR.
001013 :  ,U1A,CHEKGLOB.
001014 :  ,UJ,ENDINSTR
001015 :  MDEF:,BSS,
001016 :             ,XTA,INSTR                    .
001017 :  ,AAX,C3777.
001018 :  ,ARX,CDEFADR.
001019 :  14,VJM,READWORD.
001020 :  ,ARX,.
001021 :  ,UZA,SETADR.
001022 :          :  ,ATX,R                        .
001023 :             ,XTA,INSTR                    .
001024 :  ,AAX,CMADR.
001025 :  ,UZA,MSKIP.IF PLUS ADDRESS
001026 :             ,XTA,R                        .
001027 :  ,ARX,C70000.
001028 :             ,ATX,R                        .
001029 :  MSKIP   :  ,XTA,INSTR                    .
001030 :  ,AAX,MSINSTR.
001031 :             ,ATX,INSTR                    .
001032 :             ,XTA,R                        .
001033 :  ,AAX,C10000.
001034 :  ,ASN,64-6
001035 :             ,AOX,INSTR                    .
001036 :             ,XTS,R                        .
001037 :  ,AAX,M4S.
001038 :           15,AOX,                         .
001039 :             ,ATX,INSTR                    .
001040 :             ,XTA,R                        .
001041 :  ,ARX,C10000.
001042 :  ,AAX,C60000.
001043 :  ,UZA,ENDINSTR.
001044 :  ,ATX,R
001045 :  14,VTM,ENDINSTR.
001046 :  ,XTA,GLOBASE.
001047 :  ,UZA,ERROADR.
001048 :  ,XTA,INSTR.
001049 :  ,AAX,CMOD.
001050 :  ,U1A,ERROADR.
001051 :  ,XTA,R
001052 :  ,ASN,64-7
001053 :  ,ARX,GLOBASE.
001054 :  ,ARX,CM1M.
001055 :  ,AOX,INSTR
001056 :  ,ATX,INSTR
001057 :  ENDINSTR:,XTA,CARWORD
001058 :  ,ASN,64-24
001059 :  ,AOX,INSTR
001060 :  ,ATX,CARWORD
001061 :  9,VLM,INSTLOOP
001062 :  ,XTS,CARWORD
001063 :  10,UTM,-1
001064 :  14,VJM,WRITWP1
001065 :  ,UJ,WORDLOOP
001066 :  CHEKGLOB:,XTA,GLOBASE.
001067 :  ,UZA,ENDINSTR
001068 :  ,AAX,M4S.
001069 :  ,XTA,INSTR
001070 :  ,AAX,C3777.
001071 :  ,ARX,LOCSUBP
001072 :  ,ATX,R
001073 :  ,UJ,MSKIP.
001074 :  LINSTR  :  ,XTA,INSTR                    .
001075 :  ,AAX,C40000.
001076 :  ,UZA,ENDINSTR.IF CONSTANT ADDRESS
001077 :             ,XTA,INSTR                    .
001078 :  ,AAX,C74000.
001079 :  ,AEX,C74000.
001080 :  ,UZA,CONVADR.IF CONVENTIONAL ADDRESS
001081 :             ,XTA,INSTR                    .
001082 :  ,AAX,M37777.
001083 :             ,ARX,LOCSUBP                  .
001084 :  14,VTM,SETADR
001085 :  3,V1M,LDSHIFT
001086 :  SETADR:,AAX,M3L.
001087 :             ,XTS,INSTR                    .
001088 :  ,AAX,MLINSTR.
001089 :           15,AOX,                         .
001090 :             ,ATX,INSTR                    .
001091 :  ,UJ,ENDINSTR.
001092 :  CONVADR :  ,XTA,INSTR                    .
001093 :  ,AAX,C3777.
001094 :  ,ARX,CDEFADR.
001095 :  14,VJM,READWORD.
001096 :  :,UJ,SETADR.
001097 :  SETDATA:,BSS,.
001098 :  REWCON  :  ,XTA,RELADR                   .
001099 :             ,AEX,ADATA                    .
001100 :  ,UZA,CHEKSET.
001101 :  14,VJM,READWP1.
001102 :  :14,VJM,WRITWP1.
001103 :  :,UJ,REWCON.
001104 :  CHEKSET:,XTA,RELADR.
001105 :             ,ARX,NDATA                    .
001106 :             ,ATX,RELADR                   .
001107 :             ,XTA,NSET                     .
001108 :             ,ATI,10                       .
001109 :             ,XTA,LOCSUBP                  .
001110 :             ,ARX,NBODY                    .
001111 :  ,ARX,CM1.
001112 :             ,ATX,MENDATA                  .
001113 :  ,AEX,CAL7.
001114 :             ,XTS,MENDATA                  .
001115 :             ,ARX,NDATA                    .
001116 :  ,AEX,CAL7.
001117 :             ,STX,MENDATA                  .
001118 :             ,ATX,MENDSUB                  .
001119 :  PUTSET:10,VZM,LOADQUE.PUT NEW SUBROUTINE
001120 :  14,VJM,READWP1.
001121 :  :,STX,CARWORD.
001122 :            9,VTM,-3                       .
001123 :  DATLP   :  ,XTA,CARWORD                  .
001124 :  ,AAX,C3777.
001125 :            9,ATX,ADR4                     .
001126 :             ,XTA,CARWORD                  .
001127 :  ,AAX,C4000.
001128 :  ,UZA,SKIPCONV.
001129 :            9,XTA,ADR4                     .
001130 :  ,ARX,CDEFADR.
001131 :  14,VJM,READWORD.
001132 :  :,AAX,M3L.
001133 :  9,ATX,ADR4.
001134 :  3,VZM,SKIPCONV
001135 : C        OБPATHЫЙ CДBИГ AДPECOB ПOCЛE LDSHIFT
001136 : C        ДЛЯ ПPABИЛЬHOЙ PACCЫЛKИ ДAHHЫX.
001137 :  ,A-X,CSHIFT
001138 :  ,U1A,SKIPCONV
001139 :  ,A+X,BSHIFT
001140 :  ,AAX,M3L
001141 :  9,ATX,ADR4
001142 :  SKIPCONV:  ,XTA,CARWORD                  .
001143 :             ,ASN,64+12                    .
001144 :             ,ATX,CARWORD                  .
001145 :  9,VLM,DATLP.
001146 :  ,XTA,ADR2.
001147 :             ,ATI,8                        .
001148 :  SETLOOP:8,VZM,ESETLP.
001149 :  ,XTA,ADR4.
001150 :             ,XTS,ADR3                     .
001151 :             ,STX,ADR                      .
001152 :             ,ATI,9                        .
001153 :  DATLOOP:9,VZM,EDATLP.
001154 :  ,XTA,DTUPFLG.PRELOAD FLAG
001155 :  ,U1A,CHAPD.
001156 :  ,XTA,MODE
001157 :  ,A-X,ADR1
001158 :  ,UZA,ERD.
001159 :  CHAPD:,XTA,ADR1
001160 :  ,A-X,AP
001161 :  ,UZA,ERD.
001162 :  ,XTA,ADR1
001163 :  ,A-X,AMIN.
001164 :  ,U1A,ERD.
001165 :             ,XTA,ADR                      .
001166 :             ,ATX,ADR2                     .
001167 :             ,ARX,MENDATA                  .
001168 :  ,UZA,SERSKIP.
001169 :  ERD:14,VJM,ERROSET.
001170 :  ,STX,.
001171 :  ,UJ,SKWRDAT.
001172 :  SERSKIP :  ,XTA,ADR                      .
001173 :             ,ARX,MENDSUB                  .
001174 :  ,UZA,DATSKIP.IF NOT DATA
001175 :  ,ARX,CM1.
001176 :             ,ARX,ADATA                    .
001177 :             ,ATX,ADR2                     .
001178 :  DATSKIP :  ,XTA,ADR2                     .
001179 :  14,VJM,READWORD.
001180 :  :,XTS,ADR1.
001181 :  14,VJM,WRITWORD.
001182 :  SKWRDAT:,XTS,ADR1.
001183 :  ,ARX,C1.
001184 :             ,ATX,ADR1                     .
001185 :             ,XTA,ADR                      .
001186 :  ,ARX,C1.
001187 :             ,ATX,ADR                      .
001188 :            9,UTM,-1                       .
001189 :  ,UJ,DATLOOP.
001190 :  EDATLP  : 8,UTM,-1                       .
001191 :  ,UJ,SETLOOP.
001192 :  ESETLP  :10,UTM,-1                       .
001193 :  ,UJ,PUTSET.
001194 :  ERRORSC:6,VTM,50B.
001195 :  ,XTA,
001196 :  FATER:14,VJM,WRITER
001197 :  ENDLOAD:,XTA,SAVE20.
001198 :  ,CTX,OVFLJUMP
001199 :  ,XTA,C8.
001200 :  ,XTS,
001201 :  14,VJM,INCRAP.
001202 :  ,ATX,AP.
001203 :   ,*75,0001B
001204 :  READEND:,XTA,RLOAD
001205 :  ,ARX,C4U1L.
001206 :  ,ATX,R
001207 :  ,DRUM,R
001208 :  ,UJ,W.HA ПPOЧИTAHHЫЙ CEГMEHT
001209 :  ERROLIST:,XTS,ERROR.
001210 :  ,XTS,LISTEND.
001211 :  ,ATX,ERROFLAG
001212 :  ,ARX,CM2.
001213 :  14,VJM,WRITWORD.
001214 :  ,XTA,.
001215 :  5,ATX,1
001216 :  ,UJ,ENDLOAD.
001217 :  NAMEERRO:6,VTM,4
001218 :  ,XTA,QNAME
001219 :  ,ATX,ABSFLG.
001220 :  ,UJ,WRITER.
001221 :  ERLENGTH:6,VTM,6
001222 :  ,XTA,QNAME
001223 :  ,UJ,WRITER.
001224 :  ERRORPU:,XTA,MEMERFLG.
001225 :  14,U1A,.
001226 :  6,VTM,13B
001227 :  ,XTA,
001228 :  ,XTA,C1.
001229 :  ,ATX,MEMERFLG.
001230 :  ,XTA,AU
001231 :  ,UJ,WRITADER.
001232 :  ERROSET:6,VTM,56B
001233 :  ,UJ,WRITER.
001234 :  ERROFIX:6,VTM,61B
001235 :  ,UJ,FATER.
001236 :  ERROEQ:6,VTM,64B
001237 :  ,UJ,WRITER.
001238 :  ERROADR:6,VTM,2."ДЛИHHЫЙ AДPEC"
001239 :  ,XTA,WORDADR
001240 :  WRITADER:,ASN,64-33
001241 :  ,AUX,CUA.
001242 :  ,AOX,B00000.
001243 :  WRITER:,ATX,ERN.
001244 :  ,ITA,14
001245 :  ,XTS,ERN.
001246 :  ,ITS,6
001247 :  ,AOX,ERROR.
001248 :  ,ATX,ER.
001249 :  14,VJM,WRITWEND.
001250 :  ,XTA,AQUEUE.
001251 :  ,ARX,C2.
001252 :  ,ATX,AQUEUE.
001253 :  LPWRER:,ATX,RER.
001254 :  ,ARX,CM2.
001255 :  ,A-X,LISTEND.
001256 :  ,UZA,ENDWRER.
001257 :  ,XTA,ER.
001258 :  ,XTS,ERN.
001259 :  ,XTS,RER.
001260 :  ,ARX,CM2.
001261 :  14,VJM,READWORD.
001262 :  ,ATX,ER.
001263 :  5,XTA,1.
001264 :  ,STX,ERN.
001265 :  5,STX,1
001266 :  5,ATX,
001267 :  ,XTA,RER.
001268 :  ,ARX,C2.
001269 :  ,UJ,LPWRER.
001270 :  ENDWRER:,XTA,C1.
001271 :  ,ATX,ERROFLAG
001272 :  15,WTC,
001273 :  ,UJ,
001274 :  ERJUMP:13,VTM,W
001275 :  ,UJ,ERROFIX.
001276 :  LIBR:,ITA,14
001277 :  ,XTS,LIBCODE.
001278 :  ,AAX,MSEG.
001279 :             ,ASN,64+36                    .
001280 :  ,ARX,LIBADR.
001281 :             ,ATX,RELADR                   .
001282 :  ,XTA,LIBCODE.
001283 :  ,APX,MPACKSA.PACK SUBP.ADDRESS
001284 :             ,ASN,64+36                    .FORMAL P.NUMER TO LOW D.
001285 :  ,ATX,LIBSHIFT.
001286 :            4,VTM,TCORLIB-TCOREND          .
001287 :  RELLOOP:4,XTA,TCOREND.
001288 :  ,AAX,CEXT.
001289 :  ,U1A,SKEXTSET.
001290 :  ,XTA,EXTJUMP.
001291 :  4,CTX,TCOREND.
001292 :  SKEXTSET:4,VLM,RELLOOP.
001293 :  ,WTC,NPRT.
001294 :  4,VTM,.
001295 :  DECLOOP : 4,UTM,-1                       .
001296 :  4,XTA,TPRT.
001297 :  ,AAX,LOWMSK.
001298 :  ,A-X,LIBSHIFT.
001299 :  ,U1A,DECLOOPE.
001300 :  ,A+X,NLIBR.
001301 :  ,ATI,2.FORMAL NUMER OF PAGE
001302 :  ,X-A,NLIBRMAX.
001303 :  ,U1A,DECLOOPE.
001304 :  4,XTA,TPRT.
001305 :  ,AAX,CMEM.MEMORY PLACE
001306 :  ,ITS,2.
001307 :             ,ASN,64-34                    .
001308 :  ,AAX,CMEM.
001309 :           15,X-A,                         .
001310 :  ,AAX,CMEM.
001311 :             ,ITS,4                        .
001312 :  ,A-X,IPRT.
001313 :  ,UZA,ZPRT.
001314 :  ,XTA,NORMJUMP.
001315 :           15,AOX,                         .
001316 :  ,UJ,SETJUMP.
001317 :  ZPRT:,XTA,PRTJUMP.
001318 :           15,AOX,                         .
001319 :  SETJUMP:2,CTX,TCOR.
001320 :  DECLOOPE:4,V1M,DECLOOP.
001321 :  ,STX,.
001322 :             ,ATI,14                       .
001323 :           14,UJ ,                         .
001324 :  LONGNAME:,XTA,CARWORD
001325 :  ,ASN,64+24
001326 :  ,AAX,C3777
001327 :  ,ARX,DEFADR.BOЗBPAT B И14
001328 :  READWORD:6,VJM,GIVPLACE.
001329 :  :5,XTA,.
001330 :           14,UJ ,                         .
001331 :  WRITWORD:14,MTJ,7                        .
001332 :  6,VJM,GIVPLACE.
001333 :  WRITW1:5,STX,.
001334 :  5,CTX,.
001335 :          :  ,STX,                         .
001336 :            7,UJ ,                         .
001337 :  CHEKPAGE:  ,ITA,14                       .
001338 :             ,ITS,2                        .
001339 :             ,ITS,4                        .
001340 :             ,ITS,1                        .
001341 :  ,A-X,NDEFPLAC.
001342 :  ,U1A,CLEANPAG.IF MEMORY PAGE
001343 :  READPAGE:  ,ITA,1                        .
001344 :  ,ATX,RWR.INF FOR SETFDR
001345 :  14,VJM,SETFDR.GIVES 0 FRIEDR FOR EMPTY PAGE
001346 :  :,ITA,1.
001347 :             ,ASN,64-34                    .
001348 :  ,AAX,CMEM.
001349 :  ,X-A,FRIEPAGE.
001350 :  ,AAX,CMEM.
001351 :  ,AOX,PRTJUMP.
001352 :  1,CTX,TCOR.NOW PAGE IN MEMORY
001353 :  :,XTA,FRIEDR.
001354 :  ,UZA,ZEROS.
001355 :  14,VJM,READDRUM.
001356 :  SKIPREDR:,ITA,1.
001357 :  ,A-X,NLIST.
001358 :  ,UZA,SKIPERAS.IF CONSTANT TRACK
001359 :  ,XTA,FRIEDR.
001360 :  ,WTC,NFRIEDR.
001361 :  ,ATX,TFRIEDR.NEW TRACK IS EMPTY
001362 :  ,XTA,NFRIEDR.
001363 :  ,ARX,C1.
001364 :  ,ATX,NFRIEDR.
001365 :  SKIPERAS:,WTC,NPRT.
001366 :           14,VTM,-2                       .
001367 :  ,XTA,NFRIEDR.
001368 :  ,A-X,C2.
001369 :  ,UZA,CLEANP.IF 2 TRACKS ARE FREE
001370 :  SERCHC:14,XTA,TPRT.
001371 :  ,AAX,LOWMSK.
001372 :  ,A-X,NLIST.
001373 :  14,UTM,-1.
001374 :  ,U1A,SERCHC.IF NOT CONST.TRACK
001375 :           14,UTM,1                        .
001376 :  CLEANP  :  ,ITA,14                       .
001377 :  ,ATX,PRT.NUMER OF STRING FOR CHAINGING
001378 :  ,XTA,FRIEPAGE.
001379 :  14,XTS,TPRT.OLD FRIEPAGE TO MAGAZINE
001380 :  ,AAX,CMEM.
001381 :  ,ATX,FRIEPAGE.
001382 :  14,XTA,TPRT.
001383 :  ,AAX,LOWMSK.
001384 :  ,ATX,RWR.F.NUMER OF ERASED PAGE
001385 :  ,ATI,2.
001386 :             ,ITA,1                        .
001387 :  ,A-X,NLIBR.
001388 :  ,U1A,*5.
001389 :  ,A+X,LIBSHIFT.
001390 :  ,UJ,*6.
001391 :  *5      :  ,ITA,1                        .
001392 :  *6      :15,AOX,                         .
001393 :  14,ATX,TPRT.
001394 :  ,XTA,.
001395 :  ,ATX,FRIEDR.
001396 :  CHEKLIBR:,XTA,RWR.
001397 :  ,A-X,NLIBR.
001398 :  ,U1A,CHEKCAT.
001399 :  ,XTA,RWR.
001400 :  ,A-X,LIBSHIFT.
001401 :  ,U1A,ERASPRT.
001402 :  ,A+X,NLIBR.
001403 :             ,ATI,2                        .
001404 :  ,X-A,NLIBRMAX.
001405 :  ,U1A,ERASPRT.
001406 :  ,UJ,SETDRUM.
001407 :  CHEKCAT:,XTA,RWR.
001408 :  ,A-X,NCAT.
001409 :  ,UZA,SETDRUM.
001410 : C   CHFM1:  , BSS ,
001411 :  ,WTC,RWR
001412 :  ,XTA,TCOR
001413 :  ,AEX,FRIMEMJ
001414 :  ,UZA,SETDRUM
001415 :  CHEKLST:,XTA,RWR.
001416 :  ,A-X,NLIST.
001417 :  ,U1A,SCHFRDR.
001418 :  ,A+X,LSTDRUM.
001419 :  ,ATX,FRIEDR.
001420 :  ,UJ,WRITDR.LIST DRUM IS FIXED
001421 :  SCHFRDR:,XTA,NFRIEDR.
001422 :  ,A-X,C1.
001423 :  ,ATX,NFRIEDR.
001424 :             ,ATI,14                       .
001425 :  14,XTA,TFRIEDR-1.
001426 :  ,ATX,FRIEDR.TRACK FOR WRITING PAGE
001427 :  14,XTA,TFRIEDR.
001428 :  14,ATX,TFRIEDR-1.
001429 :  WRITDR:14,VJM,WRITDRUM.
001430 :  SETDRUM:,XTA,FRIEDR.FOR LIST DRUM IS WRITING W105
001431 :  ,AOX,EXTJUMP.BUT IT IS NOT NECESSARY
001432 :  2,CTX,TCOR.NOW PAGE ON DRUM
001433 :  ERASPRT:2,VTM,TCORV-TCOREND.
001434 :  ERASLP:2,XTA,TCOREND.
001435 :  ,AAX,CPRT.D23
001436 :  ,UZA,ERLPEND.
001437 :  2,XTA,TCOREND.
001438 :  ,AAX,UPPERMSK.
001439 :  ,AOX,PRTJUMP.
001440 :  2,CTX,TCOREND.
001441 :  ERLPEND:2,VLM,ERASLP.
001442 :  ,XTA,.
001443 :  ,ATX,IPRT.
001444 :  ,XTA,NORMJUMP.
001445 :  ,WTC,NPRT.
001446 :  ,CTX,TCORV.
001447 :  :,STX,.
001448 :             ,STI,4                        .
001449 :             ,STI,2                        .
001450 :             ,ATI,14                       .
001451 :  1,UJ,TCOR.
001452 :  SETPRT:,ITA,14.
001453 :             ,ITS,1                        .
001454 :  ,ATX,S2.
001455 :  ,A-X,NLIBR.
001456 :  ,U1A,*4.
001457 :  ,A+X,LIBSHIFT.
001458 :  ,ATX,S2.
001459 :  *4      :  ,ITA,5                        .
001460 :             ,ASN,64+10                    .
001461 :             ,ASN,64-34                    .
001462 :  ,AOX,S2.
001463 :  ,ATX,S2.
001464 :  ,ATX,S1.
001465 :  ,XTA,IPRT.
001466 :             ,ATI,14                       .
001467 :  ,A+X,C1.
001468 :  ,ATX,IPRT.
001469 :  LSET:14,XTA,TPRT.
001470 :  ,XTS,S2.
001471 :  14,STX,TPRT.
001472 :  ,ATX,S2.
001473 :  ,AEX,S1.
001474 :  14,UTM,1.
001475 :  ,U1A,LSET.
001476 :  1,XTA,TCOR.
001477 :  ,AAX,UPPERMSK.
001478 :  ,AOX,NORMJUMP.
001479 :  1,CTX,TCOR.
001480 :  :,STX,.
001481 :             ,ATI,14                       .
001482 :            6,UJ ,                         .RETURN
001483 :  CLEANPAG:  ,ITA,1                        .
001484 :             ,ASN,64-34                    .
001485 :  ,AEX,FRIEPAGE.
001486 :  ,UZA,READPAGE.
001487 :           4,VTM,                          .
001488 :  CLOOP:4,XTA,TPRT.
001489 :  ,AAX,CMEM.
001490 :  ,ASN,64+34.
001491 :             ,ITS,1                        .
001492 :           15,AEX,                         .
001493 :  ,UZA,SEND.
001494 :           4,UTM,1                         .
001495 :  ,UJ,CLOOP.
001496 :  SEND:,XTA,FRIEPAGE.
001497 :             ,ASN,64+24                    .
001498 :             ,ATI,2                        .
001499 :  4,XTA,TPRT.
001500 :  ,AAX,LOWMSK.
001501 :  ,ATX,S1.
001502 :  ,AOX,FRIEPAGE.
001503 :  4,ATX,TPRT.
001504 :  ,ATI,14.
001505 :             ,ITA,14                       .
001506 :  ,A-X,NLIBR.
001507 :  ,U1A,*7.
001508 :             ,ITA,14                       .
001509 :  ,A-X,LIBSHIFT.
001510 :  ,U1A,CHGFRP.
001511 :  ,A+X,NLIBR.
001512 :             ,ATI,14                       .
001513 :  ,X-A,NLIBRMAX.
001514 :  ,U1A,CHGFRP.
001515 :  *7      :  ,ITA,14                       .
001516 :             ,ASN,64-34                    .
001517 :  ,AAX,CMEM.
001518 :  ,X-A,FRIEPAGE.
001519 :  ,AAX,CMEM.
001520 :           14,UTC,                         .
001521 :  ,XTS,TCOR.
001522 :  ,AAX,MEMCLEAN.
001523 :           15,AOX,                         .
001524 :           14,UTC,                         .
001525 :  ,CTX,TCOR.
001526 :  CHGFRP:,ITA,1.
001527 :             ,ASN,64-34                    .
001528 :  ,ATX,FRIEPAGE.
001529 :             ,ASN,64+24                    .
001530 :             ,ATI,4                        .
001531 :           14,VTM,-1023                    .
001532 :  RESELL  : 4,XTA,                         .
001533 :            2,ATX,                         .
001534 :            4,UTM,1                        .
001535 :            2,UTM,1                        .
001536 :  14,VLM,RESELL.
001537 :  ,UJ,READPAGE.
001538 :  SETFDR:,ITA,1.
001539 :  ,A-X,NLIBR.
001540 :  ,U1A,CHCAT.
001541 :  ,A+X,LIBSHIFT.
001542 :  ,UJ,STFEX.
001543 :  CHCAT   :  ,ITA,1                        .
001544 :  ,A-X,NCAT.
001545 :  ,U1A,CHLST.
001546 :  ,A+X,CATDRUM.
001547 :  ,UJ,STFEX.
001548 :  CHLST   :  ,ITA,1                        .
001549 :  ,A-X,NLIST.
001550 :  ,U1A,CHWAR.
001551 :  ,A+X,LSTDRUM.
001552 :  ,UJ,STFEX.
001553 :  CHWAR:1,XTA,TCOR.
001554 :  ,AAX,LOWMSK.
001555 :  STFEX:,ATX,FRIEDR.
001556 :  14,UJ,.
001557 :  READDRUM:  ,ITS,14                       .
001558 :            ,ITS,                          .
001559 :  14,VJM,FA.
001560 :  :,A+X,CREAD.
001561 :  ,ATX,AEX.
001562 :  ,DRUM,AEX.
001563 :          :  ,STX,                         .
001564 :             ,STI,14                       .
001565 :           14,UJ ,                         .
001566 :  WRITDRUM:  ,ITS,14                       .
001567 :             ,ITS,                         .
001568 :  14,VJM,FA.
001569 :  :,DRUM,AEX.
001570 :          :  ,STX,                         .
001571 :             ,STI,14                       .
001572 :           14,UJ ,                         .
001573 :  FA:,XTA,FRIEDR.
001574 :             ,ASN,64-36                    .
001575 :  ,AUX,CUNPDRUM.
001576 :             ,ATX,AEX                      .
001577 :  ,XTA,FRIEDR.
001578 :  ,AAX,C33777.
001579 :  ,A-X,BTAPE.
001580 :  ,U1A,SKTP.
001581 :  ,XTA,FRIEDR.
001582 :  ,A+X,CTAPE.
001583 :             ,ASN,64-24                    .
001584 :  ,AUX,CUNPTAPE.
001585 :             ,ATX,AEX                      .
001586 :  SKTP:,XTA,FRIEPAGE.
001587 :             ,ASN,64+4                     .
001588 :             ,AOX,AEX                      .
001589 :             ,ATX,AEX                      .
001590 :           14,UJ ,                         .
001591 :  INITDRUM:  ,XTA,ALFA                     .
001592 :             ,ASN,64+25                    .
001593 :  ,AAX,M37.
001594 :  ,ARX,CM1.
001595 :  ,ATX,NPRT.
001596 :  ,XTA,CNFRIEDR.
001597 :  ,ATX,NFRIEDR.
001598 :  ,XTA,CGIVPLAC.
001599 :  ,CTX,GIVPLACE.
001600 :  ,XTA,CGIVPLC1
001601 :  ,CTX,GIVPLAC1.
001602 :  :,XTA,RCAT.
001603 :  ,APX,MPDRUM.
001604 :             ,ASN,64+36                    .
001605 :  ,ATX,CATDRUM.
001606 :            4,VTM,                         .COMPUTE INITIAL PRIORYTY
001607 :             ,XTA,AP                       .
001608 :  ,AAX,M3L.
001609 :             ,ASN,64+10                    .NUMER OF P-PAGE
001610 :  ,U1A,*8.
001611 :  ,UJ,CL1P.
001612 :  *8      :  ,ATI,1                        .
001613 :             ,ATI,2                        .
001614 :  ,X-A,NPRT.
001615 :  ,UZA,UPPLOOP.
001616 :  ,XTA,NPRT.
001617 :             ,ATI,1                        .
001618 :             ,ATI,2                        .
001619 :  UPPLOOP : 2,UTM,1                        .
001620 :             ,ITA,2                        .
001621 :  ,X-A,NPRT.
001622 :  ,U1A,LOWLOOP.
001623 :  ,XTA,FRIMEMJ.
001624 : C    FCOR:  , BSS ,
001625 :  2,CTX,TCOR
001626 :  ,ITA,2
001627 :  ,ASN,64-34
001628 :  UP1:,ITS,2
001629 :           15,AOX,                         .
001630 :  4,ATX,TPRT.
001631 :  4,UTM,1.
001632 :  ,UJ,UPPLOOP.
001633 :  LOWLOOP :  ,ITA,1                        .
001634 :  LOWLOOP1:,ARX,CM1.
001635 :  ,UZA,CLP.
001636 :             ,ATI,2                        .
001637 :             ,ASN,64-34                    .
001638 :             ,ITS,2                        .
001639 :           15,AOX,                         .
001640 :  4,ATX,TPRT.
001641 :  4,UTM,1.
001642 :            ,ITA,2                         .
001643 :  ,UJ,LOWLOOP1.
001644 :  CLP     :  ,ITS,1                        .
001645 :             ,ASN,64-34                    .
001646 :  ,ATX,FRIEPAGE.
001647 :             ,ASN,64+24                    .
001648 :             ,ATI,5                        .
001649 :             ,XTS,                         .
001650 :             ,XTS,                         .
001651 :  6,VJM,SKIPERAS.
001652 :  :,XTA,ADRCAT.
001653 :  ,ATX,CATEND.
001654 :  SCHCEND:,ARX,C1.
001655 :  14,VJM,READWORD.
001656 :  :,AEX,CAL7.
001657 :  ,UZA,BEGLOAD.
001658 :  ,XTA,CATEND.
001659 :  ,A+X,C2.
001660 :  ,ATX,CATEND.
001661 :  ,UJ,SCHCEND.
001662 :  CWRITW:,STX,.
001663 :            5,ATX,                         .
001664 :  CGIVPLAC:  ,ATI,5                        .
001665 :             ,ASN,64+10                    .
001666 :  CGIVPLC1:  ,ATI,1                        .
001667 :  1,UJ,TCOR.
001668 :  NORMJUMP: 5,UTM,                         .
001669 :            6,UJ ,                         .
001670 :  PRTJUMP : 5,UTM,                         .
001671 :  ,UJ,SETPRT.
001672 :  EXTJUMP:,UJ,CHEKPAGE.
001673 :             ,   ,                         .
001674 :  TCOR    : 5,UTM,                         .
001675 :            6,UJ ,                         .
001676 :  TCORV   : 5,UTM,                         .
001677 :  ,UJ,SETPRT.
001678 :          : 5,UTM,                         .
001679 :  ,UJ,SETPRT.
001680 :          : 5,UTM,                         .
001681 :  ,UJ,SETPRT.
001682 :          : 5,UTM,                         .
001683 :  ,UJ,SETPRT.
001684 :          : 5,UTM,                         .
001685 :  ,UJ,SETPRT.
001686 :          : 5,UTM,                         .
001687 :  ,UJ,SETPRT.
001688 :          : 5,UTM,                         .
001689 :  ,UJ,SETPRT.
001690 :          : 5,UTM,                         .
001691 :  ,UJ,SETPRT.
001692 :          : 5,UTM,                         .
001693 :  ,UJ,SETPRT.
001694 :          : 5,UTM,                         .
001695 :  ,UJ,SETPRT.
001696 :          : 5,UTM,                         .
001697 :  ,UJ,SETPRT.
001698 :          : 5,UTM,                         .
001699 :  ,UJ,SETPRT.
001700 :          : 5,UTM,                         .
001701 :  ,UJ,SETPRT.
001702 :          : 5,UTM,                         .
001703 :  ,UJ,SETPRT.
001704 :          : 5,UTM,                         .
001705 :  ,UJ,SETPRT.
001706 :          : 5,UTM,                         .
001707 :  ,UJ,SETPRT.
001708 :          : 5,UTM,                         .
001709 :  ,UJ,SETPRT.
001710 :          : 5,UTM,                         .
001711 :  ,UJ,SETPRT.
001712 :          : 5,UTM,                         .
001713 :  ,UJ,SETPRT.
001714 :          : 5,UTM,                         .
001715 :  ,UJ,SETPRT.
001716 :          : 5,UTM,                         .
001717 :  ,UJ,SETPRT.
001718 :          : 5,UTM,                         .
001719 :  ,UJ,SETPRT.
001720 :          : 5,UTM,                         .
001721 :  ,UJ,SETPRT.
001722 :          : 5,UTM,                         .
001723 :  ,UJ,SETPRT.
001724 :          : 5,UTM,                         .
001725 :  ,UJ,SETPRT.
001726 :          : 5,UTM,                         .
001727 :  ,UJ,SETPRT.
001728 :          : 5,UTM,                         .
001729 :  ,UJ,SETPRT.
001730 :          : 5,UTM,                         .
001731 :  ,UJ,SETPRT.
001732 :          : 5,UTM,                         .
001733 :  ,UJ,SETPRT.
001734 :          : 5,UTM,                         .
001735 :  ,UJ,SETPRT.
001736 :          : 5,UTM,                         .
001737 :  ,UJ,SETPRT.
001738 :  :,UJ,CHEKPAGE.
001739 :             ,   ,                         .
001740 :  :,UJ,CHEKPAGE.
001741 :             ,   ,                         .
001742 :  :,UJ,CHEKPAGE.
001743 :             ,   ,                         .
001744 :  :,UJ,CHEKPAGE.
001745 :             ,   ,                         .
001746 :  :,UJ,CHEKPAGE.
001747 :             ,   ,                         .
001748 :  :,UJ,CHEKPAGE.
001749 :             ,   ,                         .
001750 :  :,UJ,CHEKPAGE
001751 :  ,,.
001752 :  :,UJ,CHEKPAGE
001753 :  ,,.
001754 :  :,UJ,CHEKPAGE
001755 :  ,,.
001756 :  :,UJ,CHEKPAGE
001757 :  ,,.
001758 :  :,UJ,CHEKPAGE
001759 :  ,,.
001760 :  :,UJ,CHEKPAGE
001761 :  ,,.
001762 :  :,UJ,CHEKPAGE
001763 :  ,,.
001764 :  :,UJ,CHEKPAGE
001765 :  ,,.
001766 :  :,UJ,CHEKPAGE
001767 :  ,,.
001768 :  :,UJ,CHEKPAGE
001769 :  ,,.
001770 :  :,UJ,CHEKPAGE
001771 :  ,,.
001772 :  :,UJ,CHEKPAGE
001773 :  ,,.
001774 :  TCORLIB:,UJ,CHEKPAGE.
001775 :             ,   ,                         .
001776 :  :,UJ,CHEKPAGE.
001777 :             ,   ,                         .
001778 :  :,UJ,CHEKPAGE.
001779 :             ,   ,                         .
001780 :  :,UJ,CHEKPAGE.
001781 :             ,   ,                         .
001782 :  :,UJ,CHEKPAGE.
001783 :             ,   ,                         .
001784 :  :,UJ,CHEKPAGE.
001785 :             ,   ,                         .
001786 :  :,UJ,CHEKPAGE.
001787 :             ,   ,                         .
001788 :  :,UJ,CHEKPAGE.
001789 :             ,   ,                         .
001790 :  :,UJ,CHEKPAGE.
001791 :             ,   ,                         .
001792 :  :,UJ,CHEKPAGE.
001793 :             ,   ,                         .
001794 :  :,UJ,CHEKPAGE.
001795 :             ,   ,                         .
001796 :  :,UJ,CHEKPAGE.
001797 :             ,   ,                         .
001798 :  :,UJ,CHEKPAGE.
001799 :             ,   ,                         .
001800 :  :,UJ,CHEKPAGE.
001801 :             ,   ,                         .
001802 :  :,UJ,CHEKPAGE.
001803 :             ,   ,                         .
001804 :  :,UJ,CHEKPAGE.
001805 :             ,   ,                         .
001806 :  :,UJ,CHEKPAGE.
001807 :             ,   ,                         .
001808 :  :,UJ,CHEKPAGE.
001809 :             ,   ,                         .
001810 :  :,UJ,CHEKPAGE.
001811 :             ,   ,                         .
001812 :  :,UJ,CHEKPAGE.
001813 :             ,   ,                         .
001814 :  :,UJ,CHEKPAGE.
001815 :             ,   ,                         .
001816 :  :,UJ,CHEKPAGE.
001817 :             ,   ,                         .
001818 :  :,UJ,CHEKPAGE.
001819 :             ,   ,                         .
001820 :  :,UJ,CHEKPAGE.
001821 :             ,   ,                         .
001822 :  :,UJ,CHEKPAGE.
001823 :             ,   ,                         .
001824 :  :,UJ,CHEKPAGE.
001825 :             ,   ,                         .
001826 :  :,UJ,CHEKPAGE.
001827 :             ,   ,                         .
001828 :  :,UJ,CHEKPAGE.
001829 :             ,   ,                         .
001830 :  :,UJ,CHEKPAGE.
001831 :             ,   ,                         .
001832 :  :,UJ,CHEKPAGE.
001833 :             ,   ,                         .
001834 :  :,UJ,CHEKPAGE.
001835 :             ,   ,                         .
001836 :  :,UJ,CHEKPAGE.
001837 :             ,   ,                         .
001838 :  :,UJ,CHEKPAGE.
001839 :             ,   ,                         .
001840 :  :,UJ,CHEKPAGE.
001841 :             ,   ,                         .
001842 :  TCOREND:,UJ,CHEKPAGE.
001843 :             ,   ,                         .
001844 :  'CONST'
001845 :  C1      :  ,OCT,0000 0000 0000 0001      .
001846 :  M3L     :  ,OCT,0000 0000 0007 7777      .
001847 :  CM1     :  ,OCT,7777 7777 7777 7776      .
001848 :  LIBMASK :  ,OCT,5777 7700 0000 0000      .
001849 :  M4S     :  ,OCT,0000 0000 0000 7777      .
001850 :  MTYPE   :  ,OCT,0000 0000 0700 0000      .
001851 :  C1777   :  ,OCT,0000 0000 0000 1777      .
001852 :  C4000   :  ,OCT,0000 0000 0000 4000      .
001853 :  C1PAGE  :  ,OCT,0000 0000 0000 2000      .
001854 :  CAL7    :  ,OCT,7777 7777 7777 7777      .
001855 :  C3777   :  ,OCT,0000 0000 0000 3777      .
001856 :  CDEF    :  ,OCT,0000 0000 0010 0000      .
001857 :  CMINUS  :  ,OCT,0000 0000 0020 0000      .
001858 :  CLONGNAM:  ,OCT,0000 0000 2000 0000      .
001859 :  UPPERMSK:  ,OCT,7777 7777 0000 0000      .
001860 :  CFIX    :  ,OCT,2000 0000 0000 0000      .FIX MARK IN QUEUE
001861 :  CMADR   :  ,OCT,0000 0000 0100 0000      .MINUS ADR INSTR
001862 :  C70000  :  ,OCT,0000 0000 0007 0000      .
001863 :  CLADR   :  ,OCT,0000 0000 0200 0000      .
001864 :  MSINSTR :  ,OCT,0000 0000 7677 0000      .
001865 :  C40000  :  ,OCT,0000 0000 0004 0000      .
001866 :  C74000  :  ,OCT,0000 0000 0007 4000      .
001867 :  M37777  :  ,OCT,0000 0000 0003 7777      .
001868 :  MLINSTR :  ,OCT,0000 0000 7770 0000      .
001869 :  CIDENT  :  ,OCT,0000 0000 4000 0000      .
001870 :  MLOCN   :  ,OCT,7777 7777 7770 0000      .
001871 :  M37     :  ,OCT,0000 0000 0000 0037      .
001872 :  TSUBP   :  ,OCT,0000 0000 0000 0003      .
001873 :  C2      :  ,OCT,0000 0000 0000 0002      .
001874 :  CM1M:,OCT,7777 7777 7377 7777
001875 :   B 00000:,TEXT,8H B 00000
001876 :  TCOMN   :  ,OCT,0000 0000 0000 0007      .
001877 :  PB      :  ,OCT,0000 0000 0000 6042      .PB IN 6TEXT
001878 :  CEXT    :  ,OCT,4000 0000 0000 0000      .
001879 :  C377    :  ,OCT,0000 0000 0000 0377      .
001880 :  CSP     :  ,OCT,0000 0000 1040 0000      .
001881 :  CEXP    :  ,OCT,6400 0000 0000 0000      .
001882 :  MSKSMB  :  ,OCT,7700 0000 0000 0000      .
001883 :  CSTAR   :  ,6TEXT,''*''                  .
001884 :  INFMSK  :  ,OCT,5777 7777 7770 0000      .
001885 :  MPACKSA :  ,OCT,4037 7700 0000 0000      .
001886 :  MSEG    :  ,OCT,1740 0000 0000 0000      .
001887 :  CREAD   :  ,OCT,0010 0000 0000 0000      .
001888 :  CMEM    :  ,OCT,0007 7777 0000 0000      .
001889 :  MEMCLEAN:  ,OCT,7770 0000 7777 7777      .
001890 :  CHM:,OCT,2000 0000
001891 :  LOWMSK  :  ,OCT,0000 0000 7777 7777      .
001892 :  CS:,BSS,
001893 :  CPRT    :  ,OCT,0000 0000 1000 0000      .
001894 :  MPDRUM:,BSS,
001895 :  CUNPDRUM:  ,OCT,0004 0000 0077 0037      .
001896 :  BTAPE   :  ,OCT,0000 0000 0000 1400      .
001897 :  CTAPE   :  ,OCT,0000 0000 0000 3000      .
001898 :  CUNPTAPE:  ,OCT,0004 0000 0077 0777      .
001899 :  C33777  :  ,OCT,0000 0000 0003 3777      .
001900 :  FCLEAN  :  ,OCT,5777 7777 7777 7777      .
001901 :  CDEFADR :  ,OCT,0000 0000 0010 0000      .
001902 :  M40:,BSS,
001903 :  NDEFPLAC:  ,OCT,0000 0000 0000 0040      .
001904 :  BEGLIST :  ,OCT,0000 0000 0010 4000      .
001905 :  NLIST   :  ,OCT,0000 0000 0000 0042      .
001906 :  LISTMAX :  ,OCT,0000 0000 0010 6000      .
001907 :  ADRCAT:,OCT,0000 0000 0012 4000
001908 :  NCAT:,OCT,0000 0000 0000 0052
001909 :  LIBADR:,OCT,0000 0000 0014 4000
001910 :  NLIBR:,OCT,0000 0000 0000 0062
001911 :  NLIBRMAX:,OCT,0000 0000 0000 0124
001912 :  LSTDRUM :  ,OCT,0000 0000 0000 1000      .
001913 :  AMIN:,LOG,100
001914 :  C8:,LOG,10
001915 :  CNFRIEDR:  ,OCT,0000 0000 0000 0012      .
001916 :  PRT     :  ,OCT,                         .
001917 :  TFRIEDR :  ,OCT,0000 0000 0000 1037      .
001918 :          :  ,OCT,0000 0000 0000 1036      .
001919 :          :  ,OCT,0000 0000 0000 1035      .
001920 :          :  ,OCT,0000 0000 0000 1034      .
001921 :          :  ,OCT,0000 0000 0000 1033      .
001922 :          :  ,OCT,0000 0000 0000 1032      .
001923 :          :  ,OCT,0000 0000 0000 1031      .
001924 :          :  ,OCT,0000 0000 0000 1030      .
001925 :          :  ,OCT,0000 0000 0000 1027      .
001926 :          :  ,OCT,0000 0000 0000 1026      .
001927 :          :  ,OCT,0000 0000 0000 1025      .
001928 :          :  ,OCT,0000 0000 0000 1024      .
001929 :          :  ,OCT,0000 0000 0000 1023      .
001930 :          :  ,OCT,0000 0000 0000 1022      .
001931 :          :  ,OCT,0000 0000 0000 1021      .
001932 :          :  ,OCT,0000 0000 0000 1020      .
001933 :  ERROR:,TEXT,8H ERROR
001934 :  /GLOBAS/:,TEXT,4H1970.
001935 :  CM2:,OCT,7777 7777 7777 7775
001936 :  C10000:,LOG,10000
001937 :  C60000:,LOG,60000
001938 :  CMOD:,LOG,74000000
001939 :  C4U1L:,OCT,4000 0000 0000 0001
001940 :  CUA:,LOG,707070707
001941 :  C50:,LOG,2020202020
001942 :  SAVE20:,OCT,
001943 :  SECLIST:,OCT,
001944 :  GLOBASE:,OCT,
001945 :  MEMERFLG:,OCT,
001946 :  ERN:,OCT,
001947 :  ER:,OCT,
001948 :  RER:,OCT,
001949 :  CSMSK   :  ,OCT,                         .
001950 :  WREX    :  ,OCT,                         .
001951 :  LISTEND :  ,OCT,                         .
001952 :  AQUEUE  :  ,OCT,                         .
001953 :  NFRIEDR :  ,OCT,                         .
001954 :  CATDRUM :  ,OCT,                         .
001955 :  ENTRY   :  ,OCT,                         .
001956 :  NFIXP1  :  ,OCT,                         .
001957 :  LIBCODE :  ,OCT,                         .
001958 :  CATEND  :  ,OCT,                         .
001959 :  HEADFLAG:  ,OCT,                         .
001960 :  LISTRETN:  ,OCT,                         .
001961 :  LB      :  ,OCT,                         .
001962 :  LE      :  ,OCT,                         .
001963 :  AQ      :  ,OCT,                         .
001964 :  LIBSHIFT:  ,OCT,                         .
001965 :  IPRT    :  ,OCT,                         .
001966 :  RWR     :  ,OCT,                         .
001967 :  FRIEDR  :  ,OCT,                         .
001968 :  FRIEPAGE:  ,OCT,                         .
001969 :  S2      :  ,OCT,                         .
001970 :  S1      :  ,OCT,                         .
001971 :  GIVPLACE:  ,OCT,                         .
001972 :  GIVPLAC1:  ,OCT,                         .
001973 :  NPRT    :  ,OCT,                         .
001974 :  TPRT    :  ,OCT,                         .
001975 :  ,BSS,40B
001976 :  CLAU:,XTA,AU.
001977 :  ,ATI,2.
001978 :  ,AAX,MP.
001979 :  ,UJ,NZERO.
001980 :  ZEROSET:,ITA,.
001981 :  4,UTC,.
001982 :  2,ATX,.
001983 :  4,VLM,ZEROSET.
001984 :  14,UJ,.
001985 :  ZEROS:14,VTM,SKIPERAS.
001986 :  ZEROS1:,XTA,FRIEPAGE.
001987 :  ,ASN,64+24.
001988 :  ZEROS2:,ATI,2.
001989 :  2,UTM,1023.
001990 :  4,VTM,-1023.
001991 :  ,UJ,ZEROSET.
001992 :  FRIMEM:,ITA,14.
001993 :  ,ITS,2.
001994 :  ,ITS,4.
001995 :  ,XTS,PRTJUMP.
001996 :  1,CTX,TCOR.
001997 :  ,ITA,1.
001998 :  ,ASN,64-10.
001999 :  14,VTM,ERASPRT.
002000 :  ,UJ,ZEROS2.
002001 :  FRIMEMJ:,UJ,FRIMEM.
002002 :  ,,.
002003 :  MP:,LOG,76000.
002004 :  CL1P:,BSS,.
002005 :  ,XTA,FRIMEMJ.
002006 :  ,CTX,TCORV.
002007 :  ,XTA,C1.
002008 :  ,UJ,*8.
002009 :  RELOAD:,ATX,TYESFLG.
002010 :  ,DRUM,RPAGE1.
002011 :  ,WTC,MODE.
002012 :  14,VTM,.
002013 :  14,XTA,.
002014 :  14,XTS,1.
002015 :  ,ATX,ALF1.
002016 :  14,XTA,2.
002017 :  ,ARX,C8.
002018 :  ,ATX,ALF2.
002019 :  ,NTR,3
002020 :  ,UJ,LOAD.
002021 :  ABSFLG:,LOG,0.
002022 :  TYESFLG:,LOG,1.
002023 :  RPAGE1:,OCT,00100100 00200026.
002024 :  ,END,
