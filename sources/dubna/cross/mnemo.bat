*NАМЕ МНЕМОКОДЫ
*TIME:24.00
*DISC:705/SYSTEM,WORKIN
*FILE:LCROSS,67,W
*FILE:ASSEM,30
*ТАРЕ:13/МКВ,64,W
*DISC:667/SYSTEM,MD
*FILE:MADLEN,55,W
*EXPRESS
*PAGE:99,LIST
*LIBRA:23
*
*             ФАЙЛ "ASSEM"
*           0 - ОПИСАНИЕ АССЕМБЛЕРА   (*READ)
*          20 - ОПИСАНИЕ МОДУЛЯ ЗАГРУЗКИ  (ТЕР)
*          27 - РЕДАКЦИЯ НА ЗАГРУЗЧИК
*          30 - ТЕКСТЫ ЗАГРУЗЧИКА
*          40 - ОПИСАНИЕ ЭМУЛЯТОРА   (ТЕР)
*          50 - ТЕКСТЫ АССЕМБЛЕРА
*         116 - МНЕМОКОДЫ МКБ-8601   (ТЕР)
*
*PERSO:67
*NО LIST
*EDIT
* RО
* W : 1
 MNEMOTXT:,NАМЕ,
 LST:,LIST,FС
 MNEMOCOD:,SKIP,
;
;         С Т А Р Ы Й   Р Е Ж И М   Б Э С М - 6
;
;             1. ЭКСТРАКОДЫ
;
 *50:050 U
 *51:051 U
 *52:052 U
 *53:053 U
 *54:054 U
 *55:055 U
 *56:056 U
 *57:057 U
 *60 060 U
 *61:061 U
 *62:062 U
 *63:063 U
 *64:064 U
 *65:065 U
 *66:066 U
 *67:067 U
 *70:070 U
 *71:071 U
 *72:072 U
 *73:073 U
 *74:074 U
 *75:075 U
 *76:076 U
 *77:077 U
;
;      2.  КОМАНДЫ ПЕРЕСЫЛОК:
;
АТХ 000 Х
SТХ 001 XS
XTS 003 XS
ХТА 010 Х
;
;      3.  АРИФМЕТИКА С ПЛАВ.ЗАПЯТОЙ
;
А+Х 345 Х  ; БЫЛО 004
А-Х 346 Х  ; БЫЛО 005
Х-А 347 Х  ; БЫЛО 006
АМХ 350 Х  ; 007
AVX 351 Х  ; 014
А/Х 016 Х
А*Х 352 Х  ; 017
Е+Х 024 Х
Е-Х 025 Х
Е+N 034 NС  ; БЫЛО 034 N
Е-N 035 NС  ;      035 N
;
;      4.  ЛОГИЧЕСКИЕ КОМАНДЫ:
;
ААХ 011 Х
АЕХ 012 Х
ARX 013 Х
АОХ 015 Х
АРХ 020 Х
AUX 021 Х
АСХ 022 Х
ANX 023 Х
ASX 026 Х
ASN 026 NС  ; БЫЛО 036 N
;
;         5. ИНДЕКСНЫЕ КОМАНДЫ:
;
ATI 040 IС  ; БЫЛО 040 I
STI 041 ISC ;      041 IS
IТА 200 IС  ;      042 I
ITS 042 ISC ;      043 IS
MTJ 044 IМС ;      044 IМ
J+М 045 IМС ;      045 IМ
UТС 220 U
WТС 073 Х   ;      230 Х
VТМ 074 UМ  ;      240 UМ
UТМ 075 UМ  ;      250 UМ
;
;         6. КОМАНДЫ ПЕРЕДАЧИ УПРАВЛЕНИЯ:
;
UZA 353 XJ  ; БЫЛО 260 XJ
U1А 354 XJ  ;      270 XJ
UIA 354 XJ  ;      270 XJ
JМР 030 XJ  ;      300 XJ
VJM:031 XJM ;      310 XJM
JMEQ 040 XJM ;     340 XJM
JMNE 041 XJM ;     350 XJM
VLM 033 XJM ;      370 XJM
;
;         7. РАБОТА С РЕГИСТРОМ РЕЖИМОВ:
;
XTR 333 XR   ; БЫЛО 027 XR
NTR 202 NRC  ;      037 NR
RТЕ 203 NRC  ;      030 NR
;
;
;           Н О В Ы Й    Р Е Ж И М :
;
;        1. НЕПОСРЕДСТВЕННЫЙ ОПЕРАНД:
;
А+U 201 U
А-U 202 U
U-А 203 U
;     ИЗЬЯТО:       AMU 204 U
UТА 110 U
AAU 111 U
AEU 112 U
;     ИЗЬЯТО:       ARU 113 U
;     ИЗЬЯТО:       AVU 205 U
AOU 115 U
А/U 206 U
А*U 207 U
;     ИЗЬЯТО:       APU 120 U
;     ИЗЬЯТО:       AUU 121 U
ACU 122 U
ANU 123 U
UTS 103 US      ; БЫЛО 211 US
UТУ 210 U
;
;         2. РАБОТА С   Р М Р :
;
УТА 201 NС   ; БЫЛО 031 N
ХТУ 224 Х
УАХ 225 Х
УЕХ 226 Х
УОХ 227 Х
УТХ 230 Х
ААУ 204 UС
АЕУ 205 UС
АОУ 206 UС
;
;         3. ИНДЕКСНЫЕ КОМАНДЫ:
;
MCJ 220 CIM
MSN 221 CNM
J-М 047 CIM
;      ИЗЬЯТО:      М-J 223 CIM
VТМН 240 UМ
VTMQ 241 UМ
VТМВ 242 UМ
VTMF 243 UМ
UTCS 244 U
МРАС 224 CI
MUNP 225 CI
А+I 242 CI
А-I 243 CI
I-А 244 CI
;      ИЗЬЯТО:      AMI 245 CI
;      ИЗЬЯТО:      AVI 246 CI
А/I 247 CI
А*I 250 CI
AAI 251 CI
AEI 252 CI
AOI 253 CI
;
;        4. ПЕРЕДАЧИ УПРАВЛЕНИЯ:
;
JMGE 042 XJM
JMLT 043 XJM
JMLE 044 XJM
JMGT 045 XJM
VRM 251 XJM
UZAS 252 XJS
U1AS 253 XJS
JOVR 254 XJ
JCRY 255 XJ
JAGT 256 XJ
JAGE 257 XJ
JALT 260 XJ
JALE 261 XJ
JAEQ 262 XJ
JANE 263 XJ
JMPS:264 XJM
RETS 207 UС
;
;         5. ЦЕЛАЯ 64-РАЗРЯДНАЯ АРИФМЕТИКА
;
ASNA 226 CN
А+L 231 Х
А-L 232 Х
L-А 233 Х
;       ИЗЬЯТО:     AML 234 Х
;       ИЗЬЯТО:     AVL 235 Х
А/L 236 Х
А*L 237 Х
;
;          6. ОПЕРАЦИИ С ПОЛУСЛОВАМИ:
;
АТН 266 Н
SТН 267 HS
НТА 270 Н
HTS 271 HS
А+Н 272 Н
А-Н 273 Н
Н-А 274 Н
;        ИЗЬЯТО:   АМН 275 Н
;        ИЗЬЯТО:   AVH 276 Н
А/Н 277 Н
А*Н 300 Н
ААН 301 Н
АЕН 302 Н
АОН 303 Н
;
;          7. ОПЕРАЦИИ НАД ЧЕТВЕРТИНКАМИ:
;
ATQ 304 Q
STQ 305 QS
QТА 306 Q
QTS 307 QS
AEQ 310 Q
AAQ 311 Q
AOQ 312 Q
;
;         8. ОПЕРАЦИИ НАД БАЙТАМИ:
;
АТВ 313 В
SТВ 314 BS
ВТА 315 В
BTS 316 BS
АЕВ 317 В
ААВ 320 В
АОВ 321 В
;
;         9. ОПЕРАЦИИ НАД БИТАМИ:
;
ATF 322 F
STF 323 FS
FТА 324 F
FTS 325 FS
AEF 326 F
AAF 327 F
AOF 330 F
SETF 331 F
CLRF 332 F
EXTF 241 CN
NТА 260 CN
NTS 261 CNS
AEN 262 CN
AAN 263 CN
AON 264 CN
;
;         10. АРИФМЕТИКА С ДВОЙНОЙ ТОЧНОСТЬЮ:
;
ATD 334 D
DТА 335 D
А+D 336 D
А-D 337 D
D-А 340 D
;       ИЗЬЯТО:      AMD 341 D
;       ИЗЬЯТО:      AVD 342 D
А/D 343 D
А*D 344 D
;
;         11. ПРИВИЛЕГИРОВАННЫЕ КОМАНДЫ:
;
RMOD 360 Р
WMOD 361 Р
IJP 375 РС
SETR 362 PUX
CLRR 363 PUX
SMON 370 PUX
CMON 371 PUX
SEMR 364 РХ
SEMW 367 РХ
ТТХ 365 ХР
ТТА 366 ХР
RЕТ 376 СР
HLT 377 СР
REF 377 Р      ; ДЛЯ ОРГАНИЗАЦИИ ССЫЛОК
;
;         12. ЭКСТРАКОДЫ :
;
FUN 200 U      ; ДЛЯ МКБ
CFUN 000 CU    ; ДЛЯ ЭЛЬБРУС-Б
;
;       И ЕЩЕ ОСТАЛИСЬ НЕОПИСАННЫМИ КОМАНДЫ
;       ЭЛЬБРУС-Б,   ПРИДУМАННЫЕ М.Г.ЧАЙКОВСКИМ
;
 MNEMOCOD:,ENDIF,
 ,LIST,*
 ,END,MNEMOTXT
*ЕЕ
*EDIT
*R:1
*W:2
*U:MNEMO
*ЕЕ
*ASSEM
*READ:2
*ТО PERSO:67,30, КРОСС СИСТЕМА МКБ
*ТО PERSO:64,30, КРОСС СИСТЕМА МКБ
*NО LIST
*NО LOAD
*FTN
      SUBROUTINE PUTSTR(STR,L,LSMALL)
      COMMON /INDEX/ NS
      DО 1 I=1,L
      LЕТ=IGETS(STR,I-1)
      IF(I.LЕ.LSMALL) LЕТ=LЕТ.OR.40В
      CALL IPUTSL(LЕТ,NS)
  1   NS=NS+1
      CALL IPUTSL(15В,NS)
      CALL IPUTSL(12В,NS+1)
      NS=NS+2
      RETURN
      END
      PROGRAMMA
      COMMON /MNORD/ М(512)
      COMMON /PROTO/ PROTO(40)
      COMMON /INDEX/ NS
      DАТА NS/0/
      DIMENSION LIN(14)
      DАТА LIN/14(' ')/
С----------
      CALL PUTSTR('STRUCT MNEM FAR МС = Ш',22,21)
      CALL IPUTS('""""""',LIN,6)
      CALL IPUTS('""""""',LIN,6+65)
      CALL IPUTS(',,,,,,',LIN,6+66)
      NL=0
      DО 2 I=1,512 $ IF(М(I).NЕ.' >*<') GОТО 33
      М(I)=I-1
      N1=М(I).SHIFT.4.AND.15.OR.0
      N2=М(I).AND.15.OR.0
      J=М(I) $ М(I)='<FF>'
      IF(J.GТ.255) М(I)='(FF)'
      IF(N1.GТ.9) N1=N1+7
      IF(N2.GТ.9) N2=N2+7
      CALL IPUTS(N1+48,М(I),1)
      CALL IPUTS(N2+48,М(I),2)
  33   DО 3 J=1,4
      LЕТ=IGETS(М(I),J-1)
      NL=NL+1
  3   CALL IPUTS(LЕТ,LIN,6+NL)
      IF(NL.NЕ.64) GОТО 2
      NL=0 $ IF(I.EQ.512) CALL IPUTS(32,LIN,6+66)
      CALL PUTSTR(LIN,73,0)
  2   CONTINUE
      CALL PUTSTR('Щ;',2,0)
      CALL IPUTSL(32В,NS)
      CALL IPUTSL(0,NS+1)
      J=JEXTRA(0000 3700 0055 0300В, 70В, 0)
      CALL DIALOG('ТАБЛИЦА МНЕМОКОДОВ СФОРМИРОВАНА.',36,-1)
      CALL DIALOG('ДЛЯ ПЕРЕНОСА НА IВМ/РС КОМАНДА: ',36,-1)
      CALL DIALOG('  ТУРЕ Е:GАТЕ.DАТ > MNEM.С      ',36,-1)
      END
*EXECUTE
*END F
