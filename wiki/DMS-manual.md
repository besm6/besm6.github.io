# DMS - Диалоговая Мониторная Система

```
*CALL PUTFLAG*
6
*EDIT
*W:1
*(((
*     =$FILE:H,$DMS       =======================
*     $DMS                =======================
!H    **      HAЧAЛO PAБOTЫ  ШKOЛЫ 'DMS'    ***
     ДЛЯ ПPOCMOTPA HA ЭKPAHE VT ЛЮБOГO PAЗДEЛA ИЗ KATAЛOГA
     PAЗДEЛOB ШKOЛЫ DMS HEOБXOДИMO:
     ПOCЛE ДИPEKTИBЫ DMS И OTBETA HA *NAME:... И  *PASS:..
     B ПPИГЛAШEHИИ  *JSPC: ЗAMEHИTЬ  JSPC HA H:$NAME   ИЛИ
     H:$VT,  ИЛИ   H:$SPC  И T.Д.  И HAЖATЬ KHOПKY  <ETX>.

  --------------------------------------------------------
       K A T A Л O Г   P A З Д E Л O B  ШKOЛЫ  DMS
  --------------------------------------------------------
   PAЗДEЛ     OПИCAHИE  ДИPEKTИB
  --------------------------------------------------------
   $NAME      *NAME,*PASS
   $VT        ЗHAKOMCTBO C PAБOTOЙ KЛABИATYPЫ VT-340
   $SPC       *JSPC,*JLIB,*ESPC,*ELIB,*EFILE,*SFILE,*WFILE
   $COPY      *COPY,*PACK,*RPACK,*A,*DISPL,*DUMP,*MERGE
   $CAT       *JCAT,*FREE,*CFILE,*DFILE
   $EDIT      *EDIT,*L,*P,*RN,*DL,*I,*M,*FIND,*N,*E
   $EXEC      *EXEC,*PRINT,*PUNCH,*T
   $JFILE     ЗAПИCЬ B ФAЙЛЫ БИБ.DMS TEKCTOB C П/K
   $FILE      ДOCTYП K ФAЙЛAM БИБ.DMS C K/K
   $END       *END,*WAIT
   $ALL       OCHOBHЫE ПOHЯTИЯ CИCTEMЫ DMS

*     ----------------------------------------------------

$NAME **    ИДEHTИФИKAЦИЯ ПOЛЬЗOBATEЛЯ И EГO ЗAДAHИЯ    **
 MDMS                  BЫЗOB CИCTEMЫ 'DMS' C VT
                       'DMS'-ДИAЛOГOBAЯ MOHИTOPHAЯ CИCTEMA

 *NAME:1111            ЗAДAHИE ШИФPA ПOЛЬЗOBATEЛЯ
  *NAME:1111,IBM,30    ЗAДAHИE ШИФPA,ПAPOЛЯ И HOMEPA TEMЫ = 30
  *NAME:1111,IBM,,21  ЗAДAHИE ШИФPA, ПAPOЛЯ И HAЧAЛЬHOГO
                       HOMEPA BAPИAHTA ЗAПYCKA ЗAДAHИЙ.

*PASS:IBM              ЗAДAHИE ПAPOЛЯ

           I---------------------------------------------I
           I  *   - ПPИГЛAШEHИE HA BBOД ДИPEKTИBЫ.       I
           I---------------------------------------------I

*     ----------------------------------------------------

$VT     **     ЗHAKOMCTBO C KЛABИATYPOЙ  VT-340         **

 KЛABИШИ PEЖИMA PAБOTЫ:
            OFF LINE  - OTCYTCTBYET CBЯЗЬ MEЖДY  ЭBM И  VT
                       ИHФOPMAЦИЮ HA ЭKPAHE MOЖHO PEДAKTИ-
                       POBATЬ, ИCПOЛЬЗYЯ BCЮ KЛABИATYPY VT
                SEND  - COДEPЖИMOE  ЭKPAHA  ПEPEHOCИTCЯ  B
                       /OДHOCT.PEЖ./  ЭBM C ABTOMATИЧECKИM
                       ПEPEXOДOM B PEЖИM  ON LINE
             ON LINE  - CBЯЗЬ C ЭBM
  B ПPABOM HИЖHEM YГЛY KЛABИATYPЫ VT-340 HAXOДЯTCЯ KЛABИШИ
CO  CTPEЛOЧKAMИ,   KOTOPЫE   OБOЗHAЧAЮT  ДBИЖEHИE  KYPCOPA
BПPABO, BЛEBO, BHИЗ, BBEPX.

KЛABИШИ:
            IC И  DC  - BCTABKA И ИCKЛЮЧEHИE CИMBOЛA.
            IL И  DL  - BCTABKA И ИCKЛЮЧEHИE CTPOKИ.
            REPT      - KЛABИШA ПOBTOPИTEЛЬ.
            LINE FEED - ПEPEBOД KYPCOPA HA HOBYЮ CTPOKY.
            HOME      - ПEPEBOД KYPCOPA B HAЧAЛO ЭKPAHA.
            SHIFT     - ПEЧATЬ BEPXHИX CИMBOЛOB KЛABИATYPЫ
            ETX       - ЗABEPШEHИE   BBOДA CTPOKИ, ИЛИ
                        PEДAKTИPOBAHИЯ CTPOKИ ИЛИ ЭKPAHA.
   ПPИ OДHOBPEMEHHOM HAЖATИИ KЛABИШИ  <REPT>  И KAKOЙ-ЛИБO
   ИЗ KЛABИШ ЦИФPOBOГO ИЛИ БYKBEHHOГO PEГИCTPA  KЛABИATYPЫ
   ПИШETCЯ ЭTOT CИMBOЛ.

*     ----------------------------------------------------

$SPC  **     OПEPEДEЛEHИE ПPOCTAHCTB И БИБЛИOTEK        **

*JSPC:2967,,6000       OПPEДEЛEHИE PAБOЧEГO ПPOCTPAHCTBA
 *JSPC:2967,,6000,LIBR OПPEД. PAБ.ПPOCTP. И БИБ. BMECTE

*ESPC:2967,,6000       OПPEДEЛEHИE BHEШHEГO ПPOCTPAHCTBA

*ELIB:LIBR             OПPEДEЛEHИE BHEШHEЙ БИБЛИOTEKИ

*EFILE:SUN             OПPEДEЛEHИE BHEШHEГO ФAЙЛA

*SFILE:905,,100,10     ПOДKЛЮЧEHИE MЛ 0905
                       /C 100 ПO 107 ЗOHY, BCEГO 10 ЗOH/
 *SFILE:*              OTKAЗ OT PAHEE OПPEДEЛEH. *SFILE...

*WFILE:906,,30,20      ПOДKЛЮЧEHИE MЛ 906 ДЛЯ ЗAПИCИ
                       C 30 ПO 47 ЗOHY

*     ----------------------------------------------------

$COPY  **     KOПИPOBAHИE И ПPEOБPAЗOBAHИE ФAЙЛOB       **

*COPY:*SFILE,*WFILE,N  KOПИPOB.  N ЗOH  ИЗ *SFILE B *WFILE
  *COPY:FI,FO,0        KOПИPOBAHИE FI B FO ПO CTPOKAM
  *COPY:FI,FO          KOПИPOBAHИE ПO ЗOHAM
 *COPY:FILE4,*SFILE    KOПИPOBAHИE FILE4 (S-TИПA) B *SFILE
 *COPY:*SFILE,F2       KOПИPOBAHИE *SFILE  B HOBЫЙ  F2(S)

*RPACK:FILE2,FILE3     PACПAKOBKA  C ПPEOБPAЗOBAHИEM
                       FILE2(S)  B  FILE3(I)

 *RPACK:*SFILE,FILE3   PACПAKOBKA  C ПPEOБPAЗOBAHИEM
                       *SFILE    B  FILE3(I)

*PACK:FI,FS            YПAKOBKA FI(I) B FS(S)  /C  ДATOЙ/
 *PACK:FI,FS,0         -*--*--*--*--*--*--*--  /БEЗ ДATЫ/
 *PACK:FIL3,*SFILE     YПAKOBKA FIL3(I) B *SFILE/C ДATOЙ/

*DISPL:*SFILE          ПPOCMOTP ФAЙЛA S-TИПA HA ЭKPAHE  VT
 *DISPL:111100         ПPOCMOTP ФAЙЛA 111100 HA VT /ПOCЛE
                       CЧETA ПO ДИP.*EXEC FI,1  ,CM.$EXEC/
 *DISPL:FA,I,N1,N2     ПPOCMOTP ФAЙЛA FA C N1 ПO N2 CTPOKY
                       I-TИП ФAЙЛA(MOЖET БЫTЬ OПYЩEH)

*DUMP:FF,,NZ,N1,N2     БИHAPHЫЙ ПPOCMOTP ФAЙЛA  FF C N1 ПO
                       N2 CЛOBO B ЗOHE NZ
 *DUMP:FI,I,NZ,N1,N2   TEKCTOBOЙ ПPOCMOTP ФAЙЛA FI C N1 ПO
                       N2 CЛOBO B ЗOHE NZ
*MERGE:                ЗAKPЫTA

*     ----------------------------------------------------

$CAT  **     PAБOTA C KATAЛOГOM     БИБЛИOTEKИ         ***

*JCAT                  BЫДAЧA KPATKOГO KATAЛOГA  PAБ. БИБ.
 *JCAT:,F              BЫДAЧA ПOЛHOГO KATAЛOГA   PAБ. БИБ.
 *JCAT:,I              BЫДAЧA KATAЛOГA ФAЙЛOB  I-TИПA
 *JCAT,,S              BЫДAЧA KATAЛOГA ФAЙЛOB  S-TИПA
 *JCAT:AB,F            BЫДAЧA ПOЛHOЙ ИHФOPMAЦИИ O ФAЙЛE AB

*FREE                  OПPOC ЧИCЛA CBOБOДHЫX ЗOH B  БИБ.

*CFILE:FIL,I           COЗДAHИE HOBOГO ФAЙЛA FIL  I-TИПA
 *CFILE:PMON,S*B       CMEHA TИПA ФAЙЛA PMON  /S HA B/
 *CFILE:PMON,B*B,1     ДOБABЛEHИE K ФAЙЛY PMON 1 ЗOHЫ

*DFILE:FILEN           YHИЧTOЖEHИE ФAЙЛA FILEN ИЗ PAБ. БИБ

*     ----------------------------------------------------

$EDIT   **       PAБOTA   C   PEДAKTOPOM                **

                 I---------------------------------------I
                 I  *      - ПPИГЛAШEHИE                 I
                 I---------------------------------------I

*EDIT FILL,NEW         OБPAЩEHИE K PEДAKTOPY. OБPAЗOBAHИE
                       HOBOГO ФAЙЛA FILL ДЛЯ PAБOTЫ C HИM
 *EDIT FILE,OLD        OБPAЩEHИE K PEДAKTOPY ДЛЯ PAБOTЫ
                       C ФAЙЛOM FILE, KOTOPЫЙ OCTAЛCЯ
                       ЗAHЯTЫM B PEЗYЛЬTATE CБOЯ
 *EDIT FILE,SAVE       OБPAЩEHИE K PEДAKTOPY ДЛЯ BOCCTAHOBЛEHИЯ
                       CЛYЧAЙHO ИCKЛЮЧEHHOГO ФAЙЛA И COЗДAHHOГO
                       ЗATEM ПO ДИPEKTИBE *CFILE ИЛИ B PEЖИME
                       AДMИHИCTPATOPA ПO ДИPEKTИBE *ZFILE
 *EDIT FILE3           OБPAЩEHИE K PEДAKTOPY ДЛЯ PAБOTЫ  C
                       ФAЙЛOM FILE3
*I:N                   BBOД CTPOK B PEДAKTИPYEMЫЙ ФAЙЛ
                       HAЧИHAЯ C HOMEPA   N  C ШAГOM  10
 *I:N1,K               BBOД CTPOK C HOMEPA N1, C ШAГOM K
 *L                    CTPAHИЧHЫЙ ПPOCMOTP ФAЙЛA C EГO
                       HAЧAЛA  ДO KOHЦA /CTPAHИЦA - ЭKPAH/
                       C PEДAKTИPOBAHИEM CTPOK HA ЭKPAHE,
                       BKЛЮЧAЯ HOMEPA CTPOK
 *L:N1,N2              ПPOCMOTP CTPOK ФAЙЛA   C   N1 ПO N2
 *LE:N,K               ПPOCMOTP CTPOK C HOMEPA     N1=NK-N
                       ДO  N2=NK-N+K, ГДE NK- HOMEP ПOCЛEД.
                       CTPOKИ. ECЛИ K=0, TO C N1 ДO KOHЦA
*P                     CTPAHИЧHЫЙ ПPOCMOTP ФAЙЛA C EГO
                       HAЧAЛA  ДO KOHЦA  /CTPAHИЦA-ЭKPAH/
                       C PEДAKTИPOBAHИEM  CTPOK HA ЭKPAHE
 *P:K                  PEДAKTИPOBAHИE K-OЙ CTPOKИ ФAЙЛA
 *P:K1,K2              ПPOCMOTP И PEДAKTИPOBAHИE CTPOK:
                       ECЛИ K2=0  -TO C K1 ДO KOHЦA ФAЙЛA
                       ECЛИ K2>K1 -TO C K1 ДO K2
                       ECЛИ K2<K1 -TO C K1 ДO K=K1+K2
 *PE:K3,K4             ПPOCMOTP И PEДAKT. CTPOK  /C KOHЦA/
                       C HOMEPA  K1=NK-K3, ДO  K2=NK-K3+K4
                       ГДE NK-HOMEP ПOCЛEДHEЙ CTPOKИ ФAЙЛA
                       ECЛИ K4=0, TO C K1 ДO KOHЦA ФAЙЛA
*RN                    ПEPEHYMEPAЦИЯ ФAЙЛA C ШAГOM 10
  *RN:KK               ПEPEHYMEPAЦИЯ ФAЙЛA C ШAГOM KK
  *RN:K,IB,IE,INB      ПEPEHYMEPAЦИЯ C ШAГOM K CO
                       CTPOKИ IB ДO CTPOKИ IE
                       INB-HOBЫЙ HOMEP CTPOKИ IB
*DL:N1,N2              YHИЧTOЖEHИE CTPOK C N1 ПO N2
*M:F,N1,N2             ПEPEHOC ИЗ ФAЙЛA F CTPOK C N1 ПO N2
                       B KOHEЦ PEДAKTИPYAMOГO
*N:NS                  YCTAHOBИTЬ HAЧAЛO ПOИKA C NS CTPOKИ
*FIND <TEST>           YCTAHOBИTЬ OБPAЗ <TEST>
 *L:N1,N2,F,12         BЫДATЬ HA ЭKPAH B PEЖИME ПOИCKA 12 CTPOK
 *FIND                 BЫДATЬ TEKCT ПOИCKOBOГO OБPAЗA
*N                     BЫДATЬ 3 ЧИCЛA: N-CTPOKИ, HAЙДEHHOЙ
                       ПO ДИPEKTИBE  *FIND И HOMEPA
                       ПEPBOЙ И ПOCЛEДHEЙ CTPOKИ  TEKCTA,
                        BЫДABAEMOГO ПEPEД ЭTИM  HA ЭKPAHTE
*E                     BЫXOД ИЗ PEЖИMA PEДAKTИPOBAHИЯ

*     ----------------------------------------------------

$EXEC ** ЗAПYCK ЗAДAHИЙ HA CЧET B MOHИT. CИCTEME  MC-80 **

               I-----------------------------------------I
   BHИMAHИE:   I  CИCTEMA PEЗEPBИPYET ДЛЯ CEБЯ CЛEДYЮЩИE I
               I  MAT. HOMEPA MЛ,MД:  30, 36, 40,        I
               I  A TAKЖE 36 ЛИCT OПEPATИBHOЙ ПAMЯTИ,    I
               I  HE ИCПOЛЬЗYЙTE ИX B ЗAДAHИЯX !!!       I
               I-----------------------------------------I

1. K ЗAДAHИЮ HA CЧET MOЖHO ПOДKЛЮЧATЬ ДO  8  DMS-ФAЙЛOB,
HAXOДЯЩИXCЯ KAK B TEKYЩEЙ БИБЛИOTEKE (JLIB), TAK И BO
BHEШHEЙ (ELIB),  C ПOMOЩЬЮ CTPOK-CПEЦKOMMEHTAPИEB:

 *     $PERS:PMON,B,57 ПOДKЛЮЧEHИE ИЗ  TEKYЩEЙ  БИБ. ФAЙЛA
                       PMON (B-TИПA) ПOД  MAT. HOMEPOM 57;
                       ECЛИ TИП ФAЙЛA OПYЩEH, KOHTPOЛЬ  HA
                       ЭTOT ATPИБYT OTKЛЮЧAETCЯ, HO  B
                       БИБЛИOTEKE ИMЯ ФAЙЛA HE ДOЛЖHO
                       ПOBTOPЯTЬCЯ. ECЛИ OПYЩEH MAT.HOMEP,
                       TO OH YCTAHABЛИBAETCЯ PABHЫM  40.
 *     $ELIB:W,PM,67110  ПOДKЛЮЧEHИE BHEШHEЙ БИБЛИOTEKИ  W
                       ИЗ BHEШHEГO ПPOCTPAHCTBA C 110 ЗOHЫ
                       HA MД/MЛ, ЗAKAЗAHHOЙ ПOД MAT.HOM 67
 *     $EFILE:R1,S,31  ПOДKЛЮЧEHИE ФAЙЛA   R1 (S-TИПA) ПOД
                       MAT.HOM 31 ИЗ ПOДKЛ. PAHEE БИБ. W
 *     $EFILE:R2,B,41  ПOДKЛЮЧEHИE ФAЙЛA R2 (B-TИПA) ПOД
 *     $COMM:*-C,YES   ЗAMEHA B KAPTE CПEЦKOMMEHTAPИЙ
                       CИMBOЛA '*' HA 'C' БEЗ ИCKЛЮЧEHИЯ
                       ИЗ BXOДHOГO ПOTOKA.
 *     $COMM:*-*,NO    C ИCKЛЮЧEHИEM ИЗ BЫXOДHOГO ПOTOKA
                       MAT.HOM.41 ИЗ ПOДKЛ. PAHEE БИБ. W
2. B ЗAДAЧY ПOCЛE ЗAKAЗA PECYPCOB MOЖHO  BCTABЛЯTЬ ФAЙЛЫ
И ПOДФAЙЛЫ C ПOMOЩЬЮ CTPOK-CПEЦKOMMEHTAPИEB BИДA:

  *     $FILE:NF       BCTABKA BCEГO ФAЙЛA NF
  *     $FILE:NF,$SUB  BCTABKA ПOДФAЙЛA $SUB ИЗ ФAЙЛA NF

   ИMEHA ПOДФAЙЛOB ДOЛЖHЫ HAЧИHATЬCЯ C CИMB. '$', TEKCT
 ПOДФAЙЛA  B ФAЙЛE  OБPAMЛЯETCЯ ДBYMЯ CTPOKAMИ -
 CПEЦKOMMEHTAPИЯMИ BИДA:
   *     $SUB
   *     $END FILE
  B ПOДФAЙЛE HE ДOЛЖHO БЫTЬ П/K *END FILE !!!

3.ЗAДAHИE MOЖHO 'ЗAMOPOЗИTЬ' HA BBOДHOM БYФEPE
 B ЗAДAHИE MOЖHO BKЛЮЧИTЬ YKAЗAHИE OБ OЖИДAHИИ OKOHЧAHИЯ
ДPYГOЙ ЗAДAЧИ. ДЛЯ ЭTOГO  HEOБXOДИMO  BKЛЮЧИTЬ B ПACПOPT
ЗAДAHИЯ ДИPEKTИBY:

*WAIT:<ШИФP OЖИДAEMOЙ OKOHЧAHИЯ ЗAДAЧИ>
      ПPИ ЗAПYCKE ИЗ DMS ШИФP - 12 ЦИФP - ФOPMИPYETCЯ TAK:
         <AHП><HT><KT><KБ><HB> ,                      ГДE
 <AHП> - (0001 - 9999)
  <HT> - ДBE ЦИФPЫ BЫЧИCЛЯЮTCЯ ПO ФOPMYЛE: HT=(NM-1)*12+NT
         ГДE NM - HOMEP ЭBM БЭCM-6, NT - MAT. HOMEP
         TEPMИHAЛA (1-12), C KOTOPOГO ИДET CEAHC CBЯЗИ,
 <KT>  - KOД TEMЫ (00-99)
 <KБ>  - KOД БЮДЖETA, (ПOKA 99)
 <HB>  - HOMEP BAPИAHTA ЗAПYCKA ЗAДAHИЯ (00-99)

*EXEC FF               ЗAПYCK HA CЧET FF C BЫДAЧEЙ HA AЦПY
 *EXEC FI,2            ЗAПYCK HA CЧET FI C ПEPEXBATOM
                       PEЗYЛЬTATOB CЧETA B ФAЙЛ  S-TИПA
                       ДИHHOЙ B 2 ЗOHЫ  C ИMEHEM 111100
                       /O0-TEKYЩИЙ BAPИAHT CЧETA AHП=1111)
                       ФAЙЛ ФOPMИPYETCЯ B PAБ.БИБ. B
                       MOMEHT BЫXOДA ЗAДAЧИ HA CЧET.
                       ПEPEД ЗAПYCKOM TAKOЙ ЗAДAЧИ
                       HEOБXOДИMO YБEДИTЬCЯ B HAЛИЧИИ NZ
                       CBOБOДHЫX ЗOH ПO ДИPEKTИBE: *FREE
*PRINT FIL3            BЫДAЧA HA AЦПY COДEPЖAHИЯ  FIL3
 *PRINT FIL3,B         BЫДAЧA HA AЦПY FIL3 B BИДE БИЛИCTA
 *PRINT FIL3,R         BЫДAЧA HA AYПY FIL3,B KOTOPOM
                       ЗAПИCAH TEKCT ПO KAPTE *RESPON:...
 *PRINT FIL3,BR        BЫДAЧA ФAЙЛA ABTOПEPEXBATA B БИЛИCTE
*PUNCH FILE3           BЫДAЧA  HA ПEPФOPAЦИЮ (B KOДE IBM)
*T                     OПPOC COCTOЯHИЯ ЗAПYЩEHHOЙ ЗAДAЧИ
*T 11                  OПPOC COCTOЯHИЯ ЗAДAЧИ C HOM.BAP.11
*T:,20                 OПPOC COCTOЯHИЯ ЗAПYЩEHHOЙ ЗAДAЧИ C
                       YCTAHOBKOЙ HOBOГO HOMEPA HAЧAЛЬHOГO
                       BAPИAHTA
*TASK:101119009920     OПPOC COCTOЯHИЯ PEШAEMOЙ ЗAДAЧИ ПO
                       EE ПOЛHOMY ШИФPY
*     $JFILE
*      ----------------------------------------------------

$JFILE  ** MAKET KOЛOДЫ ДЛЯ ЗAПИCИ B БИБ.DMS TEKCTOB C П/K

 *NAME:...
 *PASS:...
 *FILE:TISS,36100,W,001111 ЗAKAЗ HA ПOДKЛ. ПPOCTPAHCTBA DMS
                       PACПOЛOЖEHHOГO B ФAЙЛE MC-80  'TISS'
                       C 100 ЗOHЫ (BЛAДEЛEЦ  AHП=1111)
 *JLIB:MODEL,PR        ЗAKAЗ HA ПOДKЛ. БИБ.MODEL(ПAPOЛЬ PR)
 *JFILE:DD             DD  - ФAЙЛ  B KOTOPЫЙ БYДET ЗAПИCAH
                       HИЖECЛEДYЮЩИЙ TEKCT C KOЛOДЫ П/K
 <KOЛOДA П/K>          БEЗ П/K *NAME:.. И    *END FILE !!!
 //////                ПPИЗHAK KOHЦA П/K
 *END FILE
 <ДK>

*     ----------------------------------------------------

$FILE  ** MAKET KOЛOДЫ ДЛЯ ДOCTYПA K ФAЙЛAM БИБ.DMS C П/K


 *NAME:...
 *PASS:...
 *FILE:TISS,36100,W,001111 ЗAKAЗ HA ПOДKЛ. ПPOCTPAHCTBA
                       DMS ИЗ ФAЙЛA MC-80 'TISS' C 100 ЗOH
 < ЗAKAЗЫ PECYPCOB TИПA >:
 <*TIME:...>
 <*MEMO:...>
 <*DRUM:...>
 <*TAPE:...>
 <*FILE:...>
 *DUMON$FILE:DD,PAPT1,MODEL,PR   ЗAПYCK  ЗAДAHИЯ HA CЧET
                       ИЗ ПOДФAЙЛA PART1, ФAЙЛA DD ИЗ
                       БИБ.MODEL(ПAPOЛЬ PR), ПOДKЛЮЧEHHOЙ
                       K ЗAДAЧE ПOД MAT.HOMEPOM 36,
                       ФAЙЛ DD HE ДOЛЖEH COДEPЖATЬ BЫШE
                       YKAЗAHHЫE  ЗAKAЗЫ PECYPCOB.
 *DUMON$FILE:DD,,MODEL,PR  ПPИ ПYCTOM YKAЗATEЛE ПOДФAЙЛA
                       ЗAПYCKAETCЯ BECЬ ФAЙЛ DD
 *END FILE
 <ДK>

*     $END FILE
*     ----------------------------------------------------

$END   **         BЫXOД ИЗ CИCTEMЫ 'DMS'                **

 *WAIT                 BPEMEHHЫЙ BЫXOД ИЗ CИCTEMЫ 'DMS'
 *END                  KOHEЦ PAБOTЫ CИCTEMЫ 'DMS'

*     $END FILE

*     $ALL        =============================

$ALL  **    OCHOBHЫE ПOHЯTИЯ CИCTEMЫ  DMS B  MC-80.   **

 -------------------------------------------------------

1)  ИMEHHOBAHHЫЙ ФAЙЛ CИCTEMЫ MOHИTOP-80 - PECYPC HA MD,
OБ'EMA FZ ЗOH (B ИHT. 1-1777), BЫДEЛЯEMЫЙ AДMИHИCTPAЦИEЙ
AДMИHИCTPAЦИEЙ  ПOЛЬЗOBATEЛЮ BЦ. ДOCTYП
K HEMY OCYЩECTBЛЯETCЯ ПO EГO ИMEHИ
И ПAPOЛЮ ИЗ ЗAДAHИЙ (ЗAПYCKAEMЫX B ПAKETHOM,
ДИAЛOГOBЫM, TEPMИHAЛЬHOM PEЖИME) ПO ДИPEKTИBE
BИДA:
*FILE:TISS,66,W,1111  - ДOCTYП K ФAЙЛY TISS, AHП=1111

2)  ПPOCTPAHCTBO DMS - YЧACTOK ФAЙЛA MC-80
(OБ'EMOM B FZ ЗOH) - ПOДГOTOBЛEHHOE
AДMИHИCTPATOPOM DMS ДЛЯ PAБOTЫ C HИM ЧEPEЗ DMS.
 ДOCTYП K HEMY OCYЩECTBЛЯETCЯ ПO ИMEHИ
И ПAPOЛЮ ФAЙЛA MC-80 C YKAЗAHИEM HOMEPA
HAЧAЛЬHOЙ ЗOHЫ ПPOCTPAHCTBA ПO ДИPEKTИBAM
BИДA:
*JSPC:TISS,1111 PAБOЧEE ПPOCTPAHCTBO  DMS
                PACПOЛOЖEHO C 0  ЗOHЫ ФAЙЛA TISS
*ESPC:INT,1111,120 BHEШHEE ПPOCTPAHCTBO
                PACПOЛOЖEHO C 120 ЗOHЫ ФAЙЛA INT

3)  БИБЛИOTEKИ DMS - ИMEHOBAHHЫE PECYPCЫ B
ПPOCTPAHCTBE  DMS (C BOЗMOЖHЫM ПAPOЛEM), BЫДE-
ЛЯEMЫE AДMИHИCTPAЦЫEЙ ДЛЯ OДHOГO ПOЛЬЗOBATEЛЯ.
ДOCTYП K HИM OCYЩECTBЛЯETCЯ ПO ДИPEKTИBAM BИДA:
*JLIB:MODEL,PR
*ELIB:LINTER

4)  ИMEHOBAHHЫЙ ФAЙЛ DMS - OCHOBHAЯ EДИHИЦA ДЛЯ
XPAHEHИЯ B БИБЛИOTEKE DMS TEKCTOBOЙ ИHФOPMAЦИИ (I ИЛИ
 S-TИПA), БИБЛИOTEK CTAHДAPTHЫX ПOДПPOГPAMM,
БИБЛИOTEK PAЗДEЛOB, ФOPTPAHHЫX KAHAЛOB И T.Д.(B-TИПA).
 ЭTИ ФAЙЛЫ COЗДAЮTCЯ ПOЛЬЗOBATEЛЯMИ B ПPOЦECCE
ИX PAБOTЫ ПO ДИPEKTИBAM DMS. ATPИБYTЫ ИX

5)  ПOДФAЙЛ TEKCTOBOГO ФAЙЛA (I ИЛИ S-TИПA) -
ЧACTЬ TEKCTA, ЗAKЛЮЧEHHOГO MEЖДY HAЧAЛЬHOЙ И
KOHEЧHOЙ  CTPOKAMИ CПEЦKOMMEHTAPИЯMИ:

 *     $PART1
   ........
 *     $END FILE

B HAЧAЛЬHOЙ CTPOKE $PART1 - ИMЯ ПOДФAЙЛA, A CИMBOЛ $ -
OБЯЗATEЛЬHЫЙ HAЧAЛЬHЫЙ CИMBOЛ ЛЮБOГO ИMEHИ ПOДФAЙЛA.

6)  ФPAГMEHT  TEKCTOBOГO ФAЙЛA (I ИЛИ S-TИПA) -
ЧACTЬ TEKCTA, HAЧИHAЮЩEГOCЯ ПOCЛE CПEЦKOMMEHTAPИЯ  BИДA:
 *     $DECK D1
KOHEЦ ФPAГMEHTA - ЛИБO HAЧAЛO CЛEДYЮЩEГO, ЛИБO
ПPИЗHAK KOHЦA ФPAГMEHTOB:
 *     $DECK $END

  ---------------------------
TAKИM OБPAЗOM, OБЩYЮ CTPYKTYPY TEKCTOBOГO
ФAЙЛA DD (I ИЛИ S-TИПA) C OБЩEЙ ЧACTЬЮ,
ПOДФAЙЛAMИ $PART1 (C ФPAГMEHTAMИ D11 И D12) И
$PART2 (C ФPAГMEHTAMИ D21 И D22) MOЖHO ПPEДCTABИTЬ
CЛEДYЮЩИM OБPAЗOM:

 < TEKCT  DD >
 *     $PART1             HAЧAЛO PART1
 < TEKCT  $PART1>
 *     $DECK D11          HAЧAЛO ФPAГMEHTA D11
 <TEKCT ФPAГMEHTA D11>
 *     $DECK D12          HAЧAЛO ФPAГMEHTA D12
 < TEKCT ФPAГMEHTA D12>
 *     $DECK $END         KOHEЦ ФPAГMEHTOB  PART1
 *     $END FILE          KOHEЦ  PART1
 *     $PART2             HAЧAЛO PART2
 <TEKCT $PART2 >
 *     $DECK D21          HAЧAЛO ФPAГMEHTA D21
 <TEKCT ФPAГMEHTA D21 >
 *     $DECK D22          HAЧAЛO ФPAГMEHTA D22
 <TEKCT ФPAГMEHTA D22 >
 *     $DECK $END         KOHEЦ ФPAГMEHTOB PART2
 *     $END FILE          KOHEЦ PART2
 *END FILE                KOHEЦ ФAЙЛA DD

-  BOЗMOЖEH ДOCTYП KO BCEMY ФAЙЛY DD (PEЖИM 'CЧET ИЗ DMS' И
   'BCTABKA');
-  BOЗMOЖEH ДOCTYП K KAЖДOMY ПOДФAЙЛY PART1 И PART2
  (B ЭTИX ЖE PEЖИMAX).
-  BOЗMOЖEH ДOCTYП K KAЖДOMY ФPAГMEHTY D11, D12, D21, D22
 (TOЛЬKO B PEЖИME 'BCTABKA').


**********************************
  METOДЫ ДOCTYПA K ФAЙЛAM,ПOДФAЙЛAM,ФPAГMEHTAM
 ----------------------------------------------

1)  ДOCTYП K TEKCTOBЫM ФAЙЛAM И ПOДФAЙЛAM B PEЖИME
'CЧET ИЗ DMS' OCYЩECTBЛЯETCЯ ПO ДИPEKTИBAM BИДA:
*EXEC:DD          - ЗAПYCK HA CЧET ФAЙЛA DD ИЗ TEKYЩEЙ БИБ.
*EXEC:DD,$PART1 - ЗAПYCK ПOДФAЙЛA $PART1 ФAЙЛA DD

2)  ДOCTYП K TEKCTOBЫM ФAЙЛAM И ПOДФAЙЛAM  B CAMИX
ЗAДAHИЯX HA CЧET B PEЖИME 'BCTABKA' OCYЩECTBЛЯETCЯ
ПO CПEЦKOMMEHTAPИЯM BИДA:
 *     $FILE:DD,,MODEL,PR   BCTABKA BCEГO ФAЙЛA DD
 *     $FILE:DD,$PART1      BCTABKA ПOДФAЙЛA $PART1

3)  ДOCTYП K ФPAГMEHTAM BOЗMOЖEH ЛИШЬ ИЗ ЗAДAHИЙ
B PEЖИME 'BCTABKA' ПO CПEЦKOMMEHTAPИЯM BИДA:
 *     $FILE:DD,D1

4). ДOCTYП K ФAЙЛAM DMS  B ЗAДAHИЯX HA CЧET C
ПOMOЩЬЮ MATEMATИЧECKИX HOMEPOB OCYЩECTBЛЯETCЯ
ПO CПEЦKOMMEHTAPИЯM BИДA:
 *     $PERS:PMON,,67       ПOДKЛ. ПEPCOH. БИБ. PMON
 *     $EFILE:TEXT11,S,31   ПOДKЛ.ФAЙЛA TEXT11

*      $END FILE
*)))
*EE
*READ OLD
*END FILE
```
