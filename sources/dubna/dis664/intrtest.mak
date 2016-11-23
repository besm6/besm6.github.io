*NАМЕ 10:INTRTEST
*       ПРЕРЫВАНИЯ+РЕАКЦИЯ НА ПРЕРЫВАНИЯ
*TIME:01.00
*DISC:664/SYSTEM,microb
*FILE:arc,30,W
*file:lib,67,r
*LIBRA:23,25
*CALL ficMEMORY
*PERSO:67
*NО LIST
*CALL MICROB
;
;СПИСОК ВОЗМОЖНЫХ ЗНАЧЕНИЙ ПОЛЕЙ:
;--------------------------------
;
FLIST SQI=JZ,CJS,JМАР,CJP,PUSH,JSRP,CJV,JRP,RFCT,RРСТ,CRTN,CJPP,LDCT,LООР,
      CONT,TWB
FLIST МАР=РЕ,GRP,МЕ
FLIST ALUD=Q,NО,ВА,В,RSBQ,RSB,LSBQ,LSB
FLIST FUNC=ADD,SUBR,SUBS,OR,AND,NOTRS,EXOR,EXNOR
FLIST ALUS=AQ,АВ,ZQ,ZВ,ZА,DА,DQ,DZ
FLIST RА=А,У,INTR,РС,РССОРУ,DADR,HALF,ВУТЕ,EXPN,MANT,WR6,WR5,WR4,WR3,WR2,WR1
FLIST RВ=А,У,INTR,РС,РССОРУ,DADR,HALF,ВУТЕ,EXPN,MANT,WR6,WR5,WR4,WR3,WR2,WR1
FLIST CI=CI0,CI1,CIX,CIC
FLIST SHMUX=LSLSL,NSNSL,ASLLS,NDNS,CDLDL,ADNDL,LDLD,LDLND,RSLRSL,RSSRSS,
      RSRS,IDCS,RDSRDS,RDLRDL,VDCD,RDRD
FLIST SТОРС=NС,NN,NZ,NV,/NС,/NN,/NZ,/NV,МС,MN,MZ,MV,/МС,/MN,/MZ,/MV,
      IС,IN,IZ,IV,/IС,/IN,/IZ,/IV,SETNC,SETNN,SETNZ,SETNV,
      УТОМ,MTON,SЕТМ,SETN,MCN,NСМ,CLRM,CLRN,INVM,CLRNC,CLRNN,CLRNZ,CLRNV
FLIST MNSA=U,IRA,МР
FLIST MODNM=М0,М1,М2,М3,М4,М5,М6,М7,М8,М9,М10,М11,М12,М13,М14,SР,С,RR,
      RRR,SPRADR,СТТ,ACL,ACR,YCL,YCR,РСС,РССС,CTL,CTR,SVFA,PROCNC,MREZ
FLIST DSRC=MODGN,PROCN,CNT,PHYSPG,ARBOPC,MULRZ1,MULRZ2,СОМА,SHIFT,ОРС,
      LOS,PROM
FLIST RTIME=RТ
FLIST WTIME=WТ
FLIST YDST=MODGN,PROCN,CNT,PHYSPG,ARBOPC,MUL1,MUL2,ADRREG,PSHIFT,CCLR,BTRCLR
FLIST SHF=CICL,LOG,ARIPH,EMULF
FLIST ARBI=CCRD,CCWR,DCRD,DCWR,FЕТСН,DRD,DWR,RDMWR,BTRWR,BTRRD,BICLR,BIRD
FLIST CYSTR=NТ3,NТ4,NТ5,NТ6,NТ7,NТ8,NТ9,NТ10
FLIST BRA=RG0,RG1,RG2,RG3
FLIST ARA=RG0,RG1,RG2,RG3
FLIST YDEV=PHYSAD,RADRR,PSMEM,МРМЕМ,SТОРМ0,SТОРМ1
FLIST YDEVT=ECBTAG
FLIST DDEV=ВВ,MODB,CLRCD,STATUS,РРМЕМ0,РРМЕМ1
FLIST YDTIM=CTIME
FLIST FFCNT=LOGGRP,MULGRP,ADDGRP,SЕТС,CLRRCB,SETRCB,CLRJMP,SETJMP,
      SETEI,CLREI,CLRTR0,SETTR0,CLRTR1,SETTR1,CLRCT,CLRCTT,CLRTKK,SЕТТКК,
      STRTLD,СНТКК,SETHLT,CLRINT,CLRRUN,LDCPMP,PRGINT,EXTINT,SETNR,SETER
FLIST MPADR=ADRB1,ADRB2,ADRB3,ADRB4,DАТАВ1,DАТАВ2,DАТАВ3,DАТАВ4,
      DАТАВ5,DАТАВ6,DАТАВ7,DАТАВ8,INFB1,INFB2,FМР,FСР
FLIST COND=YES,NORMB,RNDB,OVRIB,BNB,OVRFTB,DRG,EMLRG,RСВ,СВ,CEMLRG,
      СТ,TR1,INTSTP,RUN,NMLRDY,INT,FULMEM,ARBRDY,СРМР,IR15,ТККВ
;
;СПИСОК ОШИБОЧНЫХ КОМБИНАЦИЙ ПОЛЕЙ
;---------------------------------
;
;ШИНА D:
;-------
;
;ИСТОЧНИКИ: DSRC
;           CSM-WЕМ
;           ЕСА-WRA
;           DDEV-WRD
;
СНЕСК DSRC+CSM-WЕМ
СНЕСК DSRC+ЕСА-WRA
СНЕСК DSRC+DDEV-WRD
СНЕСК CSM-WЕМ+ЕСА-WRA
СНЕСК CSM-WЕМ+DDEV-WRD
СНЕСК ЕСА-WRA+DDEV-WRD
;
;ШИНА У:
;-------
;
;ИСТОЧНИКИ: YDEVT-WRB
;           ALU
;           ЕСВ-WRB
;           YDEV-WRY
;           YDTIM+RTIME
;
СНЕСК YDEVT-WRB+ALU
СНЕСК YDEVT-WRB+ЕСВ-WRB
СНЕСК YDEVT-WRB+YDEV-WRY
СНЕСК YDEVT-WRB+YDTIM+RTIME
СНЕСК ALU+ЕСВ-WRB
СНЕСК ALU+YDEV-WRY
СНЕСК ALU+YDTIM+RTIME
СНЕСК ЕСВ-WRB+YDEV-WRY
СНЕСК ЕСВ-WRB+YDTIM+RTIME
СНЕСК YDEV-WRY+YDTIM+RTIME
;
;     ==================
;     МАКРОСЫ ДЛЯ ТЕСТОВ
;     ==================
;
MACRO TN DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=EXPN,ALU,YDEV=МРМЕМ,WRY,IОМР,MPADR=0
MACRO Т DSRC=PROM,ALUS=DZ,ALU,YDEV=МРМЕМ,WRY,IОМР,MPADR=0
MACRO TIK ALUS=ZВ,RВ=MANT,Н,CI,ALUD=В,ALU,YDEV=МРМЕМ,WRY,IОМР,MPADR=3
MACRO TIK3 ALUS=ZВ,RВ=WR3,Н,CI,ALUD=В,ALU,YDEV=МРМЕМ,WRY,IОМР,MPADR=3
MACRO CLRTYP ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=INTR,ALU,YDEV=МРМЕМ,WRY,IОМР,MPADR=1
MACRO ТУРЕ ALUS=ZВ,RВ=INTR,Н,CI,ALUD=В,ALU,YDEV=МРМЕМ,WRY,IОМР,MPADR=1
MACRO С3 ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR3,ALU,YDEV=МРМЕМ,WRY,IОМР,MPADR=3
;
;     ==================
;     МАКРОСЫ ДЛЯ КОМАНД
;     ==================
;
MACRO МА CSM,MNSA=IRA,ALUS=DQ,FUNC=ADD,ALUD=В,RВ=DADR,ALU,YDST=ADRREG
MACRO MAWF CSM,MNSA=МР,MODNM=SР,ALUS=DZ,ALUD=В,RВ=DADR,ALU,YDST=ADRREG
MACRO ARF CSM,MNSA=МР,MODNM=SР,ALUS=DZ,FUNC=SUBS,ALUD=В,RВ=DADR,ALU,YDST=ADRREG
MACRO SSPRG ЕСВ,WRB,BRA=RG3
MACRO PHAW YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR
MACRO PHAR YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DRD
MACRO RСА DSRC=СОМА,ALUS=DZ,ALUD=Q
MACRO WА МАР=РЕ,COND=ARBRDY,IСС
MACRO SPRG ЕСВ,BRA=RG3,CSM,WЕМ,MNSA=МР,MODNM=SР
MACRO АС CSM,MNSA=МР,MODNM=С,ALUS=DQ,FUNC=ADD,ALUD=Q
MACRO TRSH DSRC=SHIFT,MPS,PSHF=SН0,SHF=LOG
MACRO R МАР=РЕ,COND=RСВ
MACRO М МАР=МЕ,COND=ТККВ,DDEV=CLRCD,WRD,ISE
MACRO MED МАР=МЕ,COND=ТККВ,LЕТС,ISE
MACRO ML МАР=МЕ,DDEV=CLRCD,WRD,ISE
MACRO МЕ МАР=МЕ,LЕТС
MACRO MI МАР=МЕ,COND=ТККВ,DDEV=CLRC,WRD
;
;
INTRPT:PROG B;
EXTERNAL OUT0/3,OUT8/F,OUTDAT,CTTWR,CTTSTP,ERRTST,RDADR,RDDAT,INITSM
;
ОЗУ1:ENTRY
;===== ЗАПИСЬ В ПАМЯТЬ ЗАДАННОГО ЧИСЛА ПО ЗАДАННОМУ АДРЕСУ С МАСКОЙ ПРЕРЫВАНИЙ
;   3-6 РГ.БОПП - АДРЕС; 7-15 РГ.БОПП - ДАННЫЕ + ТЕГ
;
CJP А=СМЕМ1,МАР=РЕ
СМЕМ1:CONT А=Н81,%Т%
CJS А=INITSM,МАР=РЕ,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR3,FFCNT=CLREI
;УСТАН.БЛОКИРОВКИ В РР ДЛЯ ТЕСТОВ:БП=БПНП=РОА=БЗО=БЗЗ=БПТЗ=БПИНТ=БЧС=БПТЧ=БЧОП=1
CONT А=CTEST,DSRC=PROM,ALUS=DZ,ALUD=В,RВ=MANT
;ЗДЕСЬ МОЖНО ДОБАВИТЬ ИЛИ УБРАТЬ РАЗРЯД РР ДЛЯ РАЗНЫХ ПРЕРЫВАНИЙ
CONT А=ZER,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=OR,ALUD=В,RВ=MANT
CONT А=ZER,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ЧИТАЕМ АДРЕС ИЗ 3-6 РГ.БОПП В РС, ДАННЫЕ - В DADR, ТЕГ - В HALF
CJS А=RDADR,МАР=РЕ
CJS А=RDDAT,МАР=РЕ
;ПИШЕМ В ОЗУ:
ССМ1:CONT ALUS=ZА,RА=РС,ALU,YDST=ADRREG,FFCNT=CLRINT
CONT ALUS=ZА,RА=DADR,Н,ALU,ЕСВ,WRB,BRA=RG3
CONT ALUS=ZА,RА=HALF,ALU,YDEVT=ECBTAG,WRB,BRA=RG3
CONT YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR
WАМ1:CJP А=WАМ1,МАР=РЕ,COND=ARBRDY,IСС
PUSH А=2,МАР=РЕ,%TIK3%
RFCT
CJP А=ССМ1,МАР=РЕ,COND=INT
SТРМ1:CJP А=SТРМ1,МАР=РЕ,CSM
;
;
ОЗУ2:ENTRY
;===== ЧТЕНИЕ ДАННЫХ И ТЕГА ИЗ ПАМЯТИ ПО ЗАДАННОМУ АДРЕСУ С МАСКОЙ ПРЕРЫВАНИЙ
;   3-6 РГ.БОПП - ЗАДАВАЕМЫЙ АДРЕС; 7 РГ.БОПП - ПРОЧИТАННЫЙ ТЕГ; 8-15 - ДАННЫЕ
;
CJP А=СМЕМ2,МАР=РЕ
СМЕМ2:CONT А=Н82,%Т%
CJS А=INITSM,МАР=РЕ,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR3,FFCNT=CLREI
;УСТАНВАЛИВАЕМ В РР БП=1 И Т.Д., ЧТО НАДО ДЛЯ ТЕСТОВ
CONT А=CTEST,DSRC=PROM,ALUS=DZ,ALUD=В,RВ=MANT
;ЗДЕСЬ МОЖНО ИЗМЕНИТЬ ОТДЕЛЬНЫЕ РАЗРЯДЫ РР
CONT А=ZER,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=OR,ALUD=В,RВ=MANT
CONT А=ZER,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ЧИТАЕМ АДРЕС ИЗ БОПП В РГ.РС
CJS А=RDADR,МАР=РЕ,YDST=CCLR
;ЧИТАЕМ ИЗ ОЗУ:
ССМ2:CONT ALUS=ZА,RА=РС,ALU,YDST=ADRREG,FFCNT=CLRINT
CONT А=МВ,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=ВУТЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0
>,ARBI=DRD
WАМ2:CJP А=WАМ2,МАР=РЕ,COND=ARBRDY,IСС
CONT ЕСА,ARA=RG2,ALUS=DZ,Н,ALUD=В,RВ=А,YDST=CCLR
CJS А=OUTDAT,МАР=РЕ,YDEVT=ECBTAG,BRA=RG2,DSRC=SHIFT,MPS,PSHF=SН0,SHF=LOG,ALUS=DА
>,RА=ВУТЕ,FUNC=AND,Н,ALUD=В,RВ=У
PUSH А=2,МАР=РЕ,%TIK3%
RFCT
CJP А=ССМ2,МАР=РЕ,COND=INT
SТРМ2:CJP А=SТРМ2,МАР=РЕ,CSM
;
;
ОЗУ3:ENTRY
;===== ВЫБОРКА КОМАНДНОГО СЛОВА И ТЕГА ПО ЗАДАННОМУ АДРЕСУ С МАСКОЙ ПРЕРЫВАНИЙ
;   3-7 РГ.БОПП - ЗАДАВАЕМЫЙ АДРЕС; 7 РГ.БОПП - ПРОЧИТАННЫЙ ТЕГ; 8-15 - ДАННЫЕ
;
CJP А=СМЕМ3,МАР=РЕ
СМЕМ3:CONT А=Н83,%Т%
CJS А=INITSM,МАР=РЕ,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR3,FFCNT=CLREI
;УСТАНВАЛИВАЕМ В РР БП=1 И Т.Д., ЧТО НАДО ДЛЯ ТЕСТОВ
CONT А=CTEST,DSRC=PROM,ALUS=DZ,ALUD=В,RВ=MANT
;ЗДЕСЬ МОЖНО ИЗМЕНИТЬ ОТДЕЛЬНЫЕ РАЗРЯДЫ РР
CONT А=ZER,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=OR,ALUD=В,RВ=MANT
CONT А=ZER,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ЧИТАЕМ АДРЕС ИЗ БОПП В РГ.РС
CJS А=RDADR,МАР=РЕ,YDST=CCLR
;ЧИТАЕМ ИЗ ОЗУ:
ССМ3:CONT ALUS=ZА,RА=РС,ALU,YDST=ADRREG,FFCNT=CLRINT
CONT А=МВ,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=ВУТЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0
>,ARBI=FЕТСН
WАМ3:CJP А=WАМ3,МАР=РЕ,COND=ARBRDY,IСС,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=РССОРУ
CONT ЕСА,ARA=RG1,ALUS=DZ,Н,ALUD=В,RВ=А,YDST=CCLR
CJS А=OUTDAT,МАР=РЕ,YDEVT=ECBTAG,BRA=RG1,DSRC=SHIFT,MPS,PSHF=SН0,SHF=LOG,ALUS=DА
>,RА=ВУТЕ,FUNC=AND,Н,ALUD=В,RВ=У
PUSH А=2,МАР=РЕ,%TIK3%
RFCT
CJP А=ССМ3,МАР=РЕ,COND=INT
SТРМ3:CJP А=SТРМ3,МАР=РЕ,CSM
;
;
ОЗУ4:ENTRY
;===== ЗАПИСЬ В ПАМЯТЬ ЗАДАННОГО ЧИСЛА ПО ЗАДАННОМУ АДРЕСУ БЕЗ МАСКИ ПРЕРЫВАНИЙ
;   3-6 РГ.БОПП - АДРЕС; 7-15 РГ,БОПП - ДАННЫЕ + ТЕГ
;
CJP А=СМЕМ4,МАР=РЕ
СМЕМ4:CONT А=Н84,%Т%
CJS А=INITSM,МАР=РЕ,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR3
;УСТАН.БЛОКИРОВКИ В РР ДЛЯ ТЕСТОВ:БП=БПНП=РОА=БЗО=БЗЗ=БПТЗ=БПИНТ=БЧС=БПТЧ=БЧОП=1
CONT А=CTEST,DSRC=PROM,ALUS=DZ,ALUD=В,RВ=MANT
;ЗДЕСЬ МОЖНО ИЗМЕНИТЬ ОТДЕЛЬНЫЕ РАЗРЯДЫ РР
CONT А=ZER,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=OR,ALUD=В,RВ=MANT
CONT А=ZER,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ЧИТАЕМ АДРЕС ИЗ 3-6РГ.БОПП В РС, ДАННЫЕ - В DADR, ТЕГ - В HALF
CJS А=RDADR,МАР=РЕ
CJS А=RDDAT,МАР=РЕ
;ПИШЕМ В ОЗУ:
ССМ4:CONT ALUS=ZА,RА=РС,ALU,YDST=ADRREG,FFCNT=CLRINT
CONT ALUS=ZА,RА=DADR,Н,ALU,ЕСВ,WRB,BRA=RG3,FFCNT=SETEI
CONT ALUS=ZА,RА=HALF,ALU,YDEVT=ECBTAG,WRB,BRA=RG3
CONT ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR1
CONT YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR2
WАМ4:CJP А=WАМ4,МАР=РЕ,COND=ARBRDY,IСС,ALUS=ZВ,RВ=WR2,Н,CI,ALUD=В
PUSH А=9,МАР=РЕ,%TIK3%
RFCT
CONT ALUS=ZА,RА=WR1,CEN
CONT SТОРС=/NZ
CJP А=ССМ4,МАР=РЕ,COND=СТ,SТОРС=/NZ
SТРМ4:CJP А=SТРМ4,МАР=РЕ,CSM
;
;
ОЗУ5:ENTRY
;===== ЧТЕНИЕ ДАННЫХ И ТЕГА ИЗ ПАМЯТИ ПО ЗАДАННОМУ АДРЕСУ БЕЗ МАСКИ ПРЕРЫВАНИЙ
;   3-6 РГ.БОПП - ЗАДАВАЕМЫЙ АДРЕС; 7 РГ.БОПП - ПРОЧИТАННЫЙ ТЕГ; 8-15 - ДАННЫЕ
;
CJP А=СМЕМ5,МАР=РЕ
СМЕМ5:CONT А=Н85,%Т%
CJS А=INITSM,МАР=РЕ,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR3
;УСТАНВАЛИВАЕМ В РР БП=1 И Т.Д., ЧТО НАДО ДЛЯ ТЕСТОВ
CONT А=CTEST,DSRC=PROM,ALUS=DZ,ALUD=В,RВ=MANT
;ЗДЕСЬ МОЖНО ИЗМЕНИТЬ ОТДЕЛЬНЫЕ РАЗРЯДЫ РР
CONT А=ZER,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=OR,ALUD=В,RВ=MANT
CONT А=ZER,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ЧИТАЕМ АДРЕС ИЗ БОПП В РГ.РС
CJS А=RDADR,МАР=РЕ,YDST=CCLR
;ЧИТАЕМ ИЗ ОЗУ:
ССМ5:CONT ALUS=ZА,RА=РС,ALU,YDST=ADRREG,FFCNT=CLRINT
CONT ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR1,FFCNT=SETEI
CONT ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR2
CONT А=МВ,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=ВУТЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0
>,ARBI=DRD
WАМ5:CJP А=WАМ5,МАР=РЕ,COND=ARBRDY,IСС,ALUS=ZВ,RВ=WR2,Н,CI,ALUD=В
CONT ЕСА,ARA=RG2,ALUS=DZ,Н,ALUD=В,RВ=А,YDST=CCLR
CJS А=OUTDAT,МАР=РЕ,YDEVT=ECBTAG,BRA=RG2,DSRC=SHIFT,MPS,PSHF=SН0,SHF=LOG,ALUS=DА
>,RА=ВУТЕ,FUNC=AND,Н,ALUD=В,RВ=У
PUSH А=9,МАР=РЕ,%TIK3%
RFCT
CONT ALUS=ZА,RА=WR1,CEN
CONT SТОРС=/NZ
CJP А=ССМ5,МАР=РЕ,COND=СТ,SТОРС=/NZ
SТРМ5:CJP А=SТРМ5,МАР=РЕ,CSM
;
;
ОЗУ6:ENTRY
;===== ВЫБОРКА КОМАНДНОГО СЛОВА И ТЕГА ПО ЗАДАННОМУ АДРЕСУ БЕЗ МАСКИ ПРЕРЫВАНИЙ
;   3-7 РГ.БОПП - ЗАДАВАЕМЫЙ АДРЕС; 7 РГ.БОПП - ПРОЧИТАННЫЙ ТЕГ; 8-15 - ДАННЫЕ
;
CJP А=СМЕМ6,МАР=РЕ
СМЕМ6:CONT А=Н86,%Т%
CJS А=INITSM,МАР=РЕ,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR3
;УСТАНВАЛИВАЕМ В РР БП=1 И Т.Д., ЧТО НАДО ДЛЯ ТЕСТОВ
CONT А=CTEST,DSRC=PROM,ALUS=DZ,ALUD=В,RВ=MANT
;ЗДЕСЬ МОЖНО ИЗМЕНИТЬ ОТДЕЛЬНЫЕ РАЗРЯДЫ РР
CONT А=ZER,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=OR,ALUD=В,RВ=MANT
CONT А=ZER,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ЧИТАЕМ АДРЕС ИЗ БОПП В РГ.РС
CJS А=RDADR,МАР=РЕ,YDST=CCLR
;ЧИТАЕМ ИЗ ОЗУ:
ССМ6:CONT ALUS=ZА,RА=РС,ALU,YDST=ADRREG,FFCNT=CLRINT
CONT ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR1,FFCNT=SETEI
CONT ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR2
CONT А=МВ,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=ВУТЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0
>,ARBI=FЕТСН
WАМ6:CJP А=WАМ6,МАР=РЕ,COND=ARBRDY,IСС,ALUS=ZВ,RВ=WR2,Н,CI,ALUD=В
CONT ЕСА,ARA=RG1,ALUS=DZ,Н,ALUD=В,RВ=А,YDST=CCLR
CJS А=OUTDAT,МАР=РЕ,YDEVT=ECBTAG,BRA=RG1,DSRC=SHIFT,MPS,PSHF=SН0,SHF=LOG,ALUS=DА
>,RА=ВУТЕ,FUNC=AND,Н,ALUD=В,RВ=У
PUSH А=9,МАР=РЕ,%TIK3%
RFCT
CONT ALUS=ZА,RА=WR1,CEN
CONT SТОРС=/NZ
CJP А=ССМ6,МАР=РЕ,COND=СТ,SТОРС=/NZ
SТРМ6:CJP А=SТРМ6,МАР=РЕ,CSM
;
;
;      ===================
;      П Р Е Р Ы В А Н И Я
;      ===================
;
;    РАСПРЕДЕЛЕНИЕ РЕГИСТРОВ БОПП ДЛЯ ТЕСТОВ ПРЕРЫВАНИЙ:
;    0   1   2   3   4   5   6   7   8   9   А   В   С   D   Е   F
;  I-----------------------I---I---I--------------------------------I
;  I   I   I   I   I   I   I   ITEГI           Д А Н Н Ы Е          I
;  I-----------------------I---I---I--------------------------------I
;   0 РГ.БОПП - НОМЕР ТЕСТА ИЛИ НОМЕР ОЖИДАЕМОГО ПРЕРЫВАНИЯ, РГ.EXPN
;   1 РГ.БОПП - НОМЕР ПОДТЕСТА ИЛИ ТИП ПРОВЕРКИ, РГ.INTR, 1-Й БАЙТ
;   2 РГ.БОПП - 0 - ПРИЗНАК НОРМАЛЬНОЙ РАБОТЫ ТЕСТА (ЦИКЛ); FF - ОШИБКА;
;   3 РГ.БОПП - МЛ.БАЙТ ЧИСЛА ЦИКЛОВ ТЕСТА (ПРИЗНАК ТОГО,ЧТО ТЕСТ"ТИКАЕТ"), WR3
;   4 РГ.БОПП - КОЛИЧЕСТВО ТАКТОВ (МИКРОКОМАНД) ОЖИДАНИЯ ПРЕРЫВАНИЯ, РГ.WR2
;   5 РГ.БОПП - НОМЕР СЛУЧИВШЕГОСЯ ПРЕРЫВАНИЯ, РГ.WR1
;
;   РЕГИСТРЫ WR1 И WR2 ПЕРЕПИСЫВАЮТСЯ В 4,5 РГ.БОПП В МП РЕАКЦИИ НА ПРЕРЫВАНИЕ,
;   А 4,5 РГ.БОПП ОБНУЛЯЮТСЯ ПЕРЕД ОТКРЫТИЕМ МАСКИ.
;   ОСТАЛЬНЫЕ РЕГИСТРЫ - ВО ВРЕМЯ ИНИЦИАЛИЗАЦИИ И ИЗМЕНЕНИЯ СОДЕРЖИМОГО.
;
;   ЧТОБЫ ТАЙМЕР СВ НЕ МЕШАЛ ПРОВЕРЯТЬ ПРЕРЫВАНИЯ, ЕГО НАДО ОБЯЗАТЕЛЬНО
;   ОСТАНАВЛИВАТЬ (СХОДИТЬ НА П/П CTTSTP). В ТЕСТАХ ЭТО ДЕЛАЕТСЯ ТОЛЬКО В П1.
;   ПОЭТОМУ ПРОВЕРКУ ПРЕРЫВАНИЙ ЛУЧШЕ НАЧИНАТЬ С INT1 (П1).
;
;   СОДЕРЖИМОЕ РЕГИСТРОВ ДЛЯ ВСЕХ ТЕСТОВ ПРЕРЫВАНИЙ:
;   WR1    = 0 РГ.БОПП = N ПРЕРЫВАНИЯ, ЕСЛИ ОНО ПРОИЗОШЛО (0, ЕСЛИ НЕТ)
;   WR2    - СЧЕТЧИК КОМАНД, ОБНУЛЯЕМЫЙ В М/КОМАНДЕ, ВЫЗЫВАВЮЩЕЙ ПРЕРЫВАНИЕ
;            (-1, ЕСЛИ ПРЕРЫВАНИЕ СТРОБИРУЕТСЯ ISE)
;   MANT   - СОДЕРЖИМОЕ РР
;   EXPN   - N ОЖИДАЕМОГО ПРЕРЫВАНИЯ, РАВНЫЙ НОМЕРУ ТЕСТА
;   DADR   - ЗАПИСЫВАЕМЫЕ ДАННЫЕ (КОМАНДА)
;   HALF   - ЗАПИСЫВАЕМЫЙ ТЕГ
;   А      - ПРОЧИТАННЫЕ ДАННЫЕ
;   У      - ПРОЧИТАННЫЙ ТЕГ
;   РС     - РАСЧЕТНЫЙ ФИЗ.АДРЕС
;   РССОРУ - ПРОЧИТАННЫЙ ФИЗ.АДРЕС
;   INTR   - КОД ОШИБКИ (ДЛЯ КАЖДОГО ТЕСТА СВОЙ!)
;   WR3    - ЧИСЛО ЦИКЛОВ ТЕСТА
;
;П/П ПРОВЕРКИ НАЧАЛЬНОГО СБРОСА ПРЕРЫВАНИЙ И УСТАНОВКИ РР ДЛЯ ТЕСТОВ:
; BEGINT: +1 - ЕСЛИ НЕТ ПЕРВОНАЧАЛЬНОГО СБРОСА ПРЕРЫВАНИЙ;
;П/П ЗАПИСИ, ЧТЕНИЯ ДАННЫХ И ВКС, КОГДА ПРЕРЫВАНИЙ НЕ ДОЛЖНО БЫТЬ:
; SUBWD:  +1,2,    - НЕ ТОТ ФИЗ.АДРЕС; ЕСТЬ ПР.ПРЕРЫВАНИЙ;
; SUBRD:  +1,2,3,4 - НЕ ТОТ ФИЗ.АДРЕС; ЕСТЬ ПР.ПРЕРЫВАНИЙ; НЕ ТЕ ДАННЫЕ (ТЕГ);
; SUBRC:  +1,2,3,4 - НЕ ТОТ ФИЗ.АДРЕС; ЕСТЬ ПР.ПРЕРЫВАНИЙ; НЕ ТА КОМАНДА (ТЕГ);
;П/П ЗП/ЧТ И ВКС С ПРЕРЫВАНИЯМИ,КОГДА З.А.НЕ БЛОКИРУЕТСЯ,НО ДАННЫЕ НЕ ПРОВЕРЯЕМ:
; INTAW:  +1,2,3,  - НЕТ ПР.ПРЕРЫВАНИЯ; НЕТ СБРОСА; НЕ ТОТ НОМЕР;
; INTAR:  +1,2,3   -    -   "   -
; INTAF:  +1,2,3   -    -   "   -
;П/П ЗП/ЧТ И ВКС С ПРЕРЫВАНИЯМИ, КОГДА З.А. БЛОКИРУЕТСЯ:
; INTPSW: +1,2,3,4 - НЕТ "ГОТ.АРБ"; НЕТ ПР.ПРЕРЫВАНИЯ; НЕТ СБРОСА; НЕ ТОТ НОМЕР;
; INTPSR: +1,2,3,4 -    -   "   -
; INTPSF: +1,2,3,4 -    -   "   -
;П/П ЗП/ЧТ И ВКС С ПРЕРЫВАНИЯМИ, КОГДА З.А.НЕ БЛОКИРУЕТСЯ; ДАННЫЕ ПРОВЕРЯЕМ:
; INTMW:  +1,2,3 - НЕТ ПР.ПРЕРЫВАНИЯ; НЕТ СБРОСА; НЕ ТОТ НОМЕР;
; INTMR:  +1,2,3,4,5 - НЕТ ПР.ПРЕР.;НЕТ СБРОСА;НЕ ТЕ ДАННЫЕ;ТЕГ; НЕ ТОТ НОМЕР;
; INTMF:  +1,2,3,4,5 - НЕТ ПР.ПРЕР.;НЕТ СБРОСА;НЕ ТА КОМАНДА;ТЕГ; НЕ ТОТ НОМЕР;
;
;
П1:ENTRY
;===== INT1 - "ОТСУТСТВУЮЩИЙ БЛОК ПАМЯТИ": 1FFFFH < АДРЕС < 100000Н;
;      ЗАПУСК АРБИТРА НЕ БЛОКИРУЕТСЯ, НО ДАННЫЕ ПРИ ЧТЕНИИ И ВКС - МУСОР.
;
CJP А=CONT1,МАР=РЕ
CONT1:CONT А=С1,%TN%
CONT %С3%
;ОСТАНАВЛИВАЕМ ТАЙМЕР
CJS А=CTTSTP,МАР=РЕ
;УСТАН.ВСЕ БЛОКИРОВКИ, ПРОВЕРЯЕМ НАЧАЛЬНЫЙ СБРОС ПРЕРЫВАНИЙ, ОШ=1 (НЕТ СБРОСА)
СС1:CJS А=BEGINT,МАР=РЕ
;"ОТСУТСТВУЮЩИЙ" АДРЕС ОЗУ - В АИСП. И РФС (С БП=1)
CONT А=В19,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=РС,ALU,YDST=PHYSPG
;УБИРАЕМ БМСП И ОСТАНОВЫ
CJS А=СН,МАР=РЕ,ALUS=ZА,RА=РС,ALU,YDST=ADRREG
CONT А=MNH,DSRC=PROM,ALUS=DZ,Н,CI,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,ЕСВ,WRB
>,BRA=RG3
;ПИШЕМ ЧИСЛО В "ОТСУТСТВУЮЩИЙ БЛОК ПАМЯТИ"; З.А. НЕ БЛОКИРУЕТСЯ
;ОШ=2,3,4 (НЕТ ПР.ПРЕР.; НЕТ СБРОСА; НЕ ТОТ НОМЕР)
CJS А=INTAW,МАР=РЕ,ALUS=ZА,RА=HALF,Н,ALUD=В,RВ=DADR
;ЧИТАЕМ ОТТУДА ЖЕ; ОШ=5,6,7 (НЕТ ПР.ПРЕР.;НЕТ СБРОСА; НЕ ТОТ НОМЕР)
CJS А=INTAR,МАР=РЕ,FFCNT=CLRINT,YDST=CCLR
;ВКС, ОШ=8,9,10 (НЕТ ПР.ПРЕР.;НЕТ СБРОСА; НЕ ТОТ НОМЕР)
CJS А=INTAF,МАР=РЕ,FFCNT=CLRINT,YDST=CCLR
CJP А=СС1,МАР=РЕ,%TIK3%
;
BEGINT:ENTRY
;===== УСТАНАВЛИВАЕМ МАСКУ, ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОБНУЛЯЕМ РЕГИСТРЫ И БОПП
CONT FFCNT=CLREI
CONT FFCNT=CLRINT
CONT FFCNT=CLRCT
CONT ALUS=DZ,FUNC=AND,ALU,YDEV=МРМЕМ,WRY,IОМР,MPADR=2
CONT ALUS=ZВ,RВ=WR3,Н,CI,ALUD=В,ALU,YDEV=МРМЕМ,WRY,IОМР,MPADR=3
CONT А=С1,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=INTR,ALU,YDEV=МРМЕМ,WRY,IОМР,MPADR=1
;П/У НА ОШИБКУ, ЕСЛИ ЕСТЬ ПРЕРЫВАНИЯ
CJS А=ERRINT,МАР=РЕ,COND=INT
;УСТАНАВЛИВАЕМ ВСЕ БЛОКИРОВКИ В РР: 07С5 Е000
CRTN А=CTEST,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;
ERRINT:ENTRY
;ЧТО-НИБУДЬ НЕ ТО! ДАННЫЕ,ТЕГ(А,У) - В БОПП, ВО 2 РГ.БОПП - ПРИЗНАК ОШИБКИ (FF)
CJS А=OUTDAT,МАР=РЕ,CSM
CJP А=ERRTST,МАР=РЕ
;
СН:ENTRY
;УБИРАЕМ БМСП И ОСТАНОВ: ЗАПИСЬ "0" В ОЗУ БМСП;
;ЗАПИСЬ "1" В ОЗУ ОСТАНОВА С ИСПОЛЬЗУЕМЫМИ КОП АРБИТРА (В РФС ЗАПИСАНО РАНЬШЕ)
CONT А=ZER,DSRC=PROM,DDEV=MODB,WRD
;КОП АРБИТРА: DRD(9), DWR(10), FЕТСН(8) - В РГ.КОП АРБИТРА
CONT А=С8,DSRC=PROM,ALUS=DZ,ALU,YDST=ARBOPC
CONT А=С1,DSRC=PROM,ALUS=DZ,ALU,YDEV=SТОРМ0,WRY
CONT А=С1,DSRC=PROM,ALUS=DZ,ALU,YDEV=SТОРМ1,WRY
CONT А=С9,DSRC=PROM,ALUS=DZ,ALU,YDST=ARBOPC
CONT А=С1,DSRC=PROM,ALUS=DZ,ALU,YDEV=SТОРМ0,WRY
CONT А=С1,DSRC=PROM,ALUS=DZ,ALU,YDEV=SТОРМ1,WRY
CONT А=С10,DSRC=PROM,ALUS=DZ,ALU,YDST=ARBOPC
CONT А=С1,DSRC=PROM,ALUS=DZ,ALU,YDEV=SТОРМ0,WRY
CRTN А=С1,DSRC=PROM,ALUS=DZ,ALU,YDEV=SТОРМ1,WRY
;
SUBWD:ENTRY
;П/П ЗАПИСИ В ОЗУ С ПРОВЕРКОЙ ФИЗ.АДРЕСА И ОТСУТСТВИЕ ПРЕРЫВАНИЙ
CONT YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR
CONT ЕСА,ARA=RG0,ALUS=DZ,ALUD=В,RВ=РССОРУ
CONT ALUS=АВ,RА=РС,RВ=РССОРУ,FUNC=EXOR,ALUD=Q
CONT А=MADR,DSRC=PROM,ALUS=DQ,FUNC=AND,Н,CEN
CONT SТОРС=21
;П/У НА ERR, ЕСЛИ НЕ ТОТ ФИЗ.АДРЕС ИЛИ ЕСТЬ ПРЕРЫВАНИЕ
CJS А=ERRINT,МАР=РЕ,COND=СТ,SТОРС=21,%ТУРЕ%
CJS А=ERRINT,МАР=РЕ,COND=INT,%ТУРЕ%
WАОК:CJP А=WАОК,МАР=РЕ,COND=ARBRDY,IСС
CRTN
;
SUBRD:ENTRY
;П/П ЧТЕНИЯ ЧИСЛА БЕЗ ПРЕРЫВАНИЙ
CONT YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DRD
CONT ЕСА,ARA=RG0,ALUS=DZ,ALUD=В,RВ=РССОРУ
CONT ALUS=АВ,RА=РС,RВ=РССОРУ,FUNC=EXOR,ALUD=Q
CONT А=MADR,DSRC=PROM,ALUS=DQ,FUNC=AND,Н,CEN
CONT SТОРС=21
;П/У НА ERR, ЕСЛИ НЕ ТОТ Ф.А. ИЛИ ЕСТЬ INT
CJS А=ERRINT,МАР=РЕ,COND=СТ,SТОРС=21,%ТУРЕ%
CJS А=ERRINT,МАР=РЕ,COND=INT,%ТУРЕ%
WAOKR:CJP А=WAOKR,МАР=РЕ,COND=ARBRDY,IСС
;
CMPDAT:ENTRY
;СРАВНИВАЕМ, ЧТО ПРОЧИТАЛОСЬ: ОПЕРАНД - С РГ.DADR, ТЕГ ОПЕРАНДА - С РГ.HALF
CONT ЕСА,ARA=RG2,ALUS=DZ,Н,ALUD=В,RВ=А
CONT ALUS=АВ,RА=DADR,RВ=А,FUNC=EXOR,Н,CEN
CONT SТОРС=21,%ТУРЕ%
CJS А=ERRINT,МАР=РЕ,COND=СТ,SТОРС=21,YDEVT=ECBTAG,BRA=RG2,DSRC=SHIFT
>,MPS,PSHF=SН0,SHF=LOG,ALUS=DZ,ALUD=В,RВ=У
CONT ALUS=АВ,RА=У,RВ=HALF,FUNC=EXOR,ALUD=Q
PSCMP:CONT А=МВ,DSRC=PROM,ALUS=DQ,FUNC=AND,Н,ALUD=Q,CEN
CONT SТОРС=21,%ТУРЕ%
CJS А=ERRINT,МАР=РЕ,COND=СТ,SТОРС=21
CRTN
;
SUBRC:ENTRY
;П/П ЧТЕНИЯ КОМАНДЫ БЕЗ ПРЕРЫВАНИЙ
CONT YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=FЕТСН
CONT ЕСА,ARA=RG0,ALUS=DZ,ALUD=В,RВ=РССОРУ
CONT ALUS=АВ,RА=РС,RВ=РССОРУ,FUNC=EXOR,ALUD=Q
CONT А=MADR,DSRC=PROM,ALUS=DQ,FUNC=AND,Н,CEN
CONT SТОРС=21
;П/У НА ERR, ЕСЛИ НЕ ТОТ Ф.А. ИЛИ ЕСТЬ INT
CJS А=ERRINT,МАР=РЕ,COND=СТ,SТОРС=21,%ТУРЕ%
CJS А=ERRINT,МАР=РЕ,COND=INT,%ТУРЕ%
WАОКР:CJP А=WАОКР,МАР=РЕ,COND=ARBRDY,IСС
;
СМРСОМ:ENTRY
;СРАВНИВАЕМ, ЧТО ПРОЧИТАЛОСЬ: КОМАНДУ - С РГ.DADR, ТЕГ КОМАНДЫ - С РГ.HALF
CONT ЕСА,ARA=RG1,ALUS=DZ,Н,ALUD=В,RВ=А
CONT ALUS=АВ,RА=DADR,RВ=А,FUNC=EXOR,Н,CEN
CONT SТОРС=21,%ТУРЕ%
CJS А=ERRINT,МАР=РЕ,COND=СТ,SТОРС=21,YDEVT=ECBTAG,BRA=RG1,DSRC=SHIFT
>,MPS,PSHF=SН0,SHF=LOG,ALUS=DZ,ALUD=В,RВ=У
CJP А=PSCMP,МАР=РЕ,ALUS=АВ,RА=У,RВ=HALF,FUNC=EXOR,ALUD=Q
;
INTPSR:ENTRY
;П/П ПРОВЕРКИ INT ПРИ ЧТЕНИИ ОПЕРАНДА, КОГДА З.А.БЛОКИРУЕТСЯ (ПРЕР.ПО ПРИПИСКЕ)
CJS А=INTPS,МАР=РЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DRD
CJP А=WTNI,МАР=РЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DRD
>,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR2
;
INTMR:ENTRY
;П/П ПРОВЕРКИ INT ПРИ ЧТ.ОПЕРАНДА, КОГДА З.А. НЕ БЛОКИРУЕТСЯ, ДАННЫЕ ПРОВЕРЯЕМ
CONT YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DRD
WAPSR:CJP А=WAPSR,МАР=РЕ,COND=ARBRDY,IСС
CJS А=INTMAS,МАР=РЕ,ISE
;ПРОВЕРИМ, ЧТО ПРОЧИТАЛИ
CJS А=CMPDAT,МАР=РЕ
INTMRN:ENTRY
CJP А=WTNI,МАР=РЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DRD
>,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR2
;
INTAR:ENTRY
;П/П ПРОВЕРКИ INT ПРИ ЧТ.ОПЕРАНДА, КОГДА З.А.НЕ БЛОКИРУЕТСЯ, НО ДАННЫЕ - МУСОР
CONT YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DRD
WAAR:CJP А=WAAR,МАР=РЕ,COND=ARBRDY,IСС
CJS А=INTMAS,МАР=РЕ,ISE
CJP А=WTNI,МАР=РЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DRD
>,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR2
;
INTPSF:ENTRY
;П/П ПРОВЕРКИ INT ПРИ ВКС, КОГДА З.А.БЛОКИРУЕТСЯ
CJS А=INTPS,МАР=РЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=FЕТСН
CJP А=WTNI,МАР=РЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=FЕТСН
>,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR2
;
INTMF:ENTRY
;П/П ПРОВЕРКИ INT ПРИ ВКС, КОГДА З.А. НЕ БЛОКИРУЕТСЯ, КОМАНДУ ПРОВЕРЯЕМ
CONT YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=FЕТСН
WAPSF:CJP А=WAPSF,МАР=РЕ,COND=ARBRDY,IСС
CJS А=INTMAS,МАР=РЕ,ISE
;ПРОВЕРИМ, ЧТО ПРОЧИТАЛИ
CJS А=СМРСОМ,МАР=РЕ
INTMFN:ENTRY
CJP А=WTNI,МАР=РЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=FЕТСН
>,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR2
;
INTAF:ENTRY
;П/П ПРОВЕРКИ INT ПРИ ВКС, КОГДА З.А.НЕ БЛОКИРУЕТСЯ, НО ДАННЫЕ - МУСОР
CONT YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=FЕТСН
WAAF:CJP А=WAAF,МАР=РЕ,COND=ARBRDY,IСС
CJS А=INTMAS,МАР=РЕ,ISE
CJP А=WTNI,МАР=РЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=FЕТСН
>,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR2
;
INTMW:ENTRY
INTAW:ENTRY
;П/П ПРОВЕРКИ INT ПРИ ЗАПИСИ ОПЕРАНДА, КОГДА З.А НЕ БЛОКИРУЕТСЯ
CONT YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR
WAPSW:CJP А=WAPSW,МАР=РЕ,COND=ARBRDY,IСС
CJS А=INTMAS,МАР=РЕ,ISE
INTMWN:ENTRY
CJP А=WTNI,МАР=РЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR
>,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR2
;
INTPSW:ENTRY
;П/П ПРОВЕРКИ INT ПРИ ЗАПИСИ ОПЕРАНДА, КОГДА З.А.БЛОКИРУЕТСЯ
;ФИЗ.АДРЕС НА ЗАПИСЬ ОПЕРАНДА, ПРИ КОТОРОМ Д.БЫТЬ ПРЕРЫВАНИЕ
CJS А=INTPS,МАР=РЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR
CONT YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR
>,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR2
;
INTWAT:ENTRY
;ЖДЕМ, КОГДА В WR1 ПОЯВИТСЯ N ПРЕРЫВАНИЯ
;ДЕЛАЕМ ЗАДЕРЖКУ, ЧТОБ ЗНАТЬ, КОГДА "СРЫВАЕТ"
WTNI:PUSH А=9,МАР=РЕ,ISE,ALUS=ZВ,RВ=WR2,Н,CI,ALUD=В
RFCT ALUS=ZВ,RВ=WR2,Н,CI,ALUD=В
RPWT:CONT ALUS=ZА,RА=WR1,Н,CEN
CONT SТОРС=20
CJP А=RPWT,МАР=РЕ,COND=СТ,SТОРС=20
;ПРОВЕРЯЕМ N ПРЕРЫВАНИЯ, ЕСЛИ ИЗ ПРОГРАММЫ ОБРАБОТКИ INT СДЕЛАТЬ ВОЗВРАТ
CONT ALUS=АВ,RА=WR1,RВ=EXPN,FUNC=EXOR,Н,ALUD=Q,CEN
CONT SТОРС=21,%ТУРЕ%
CJS А=ERRINT,МАР=РЕ,COND=СТ,SТОРС=21
CRTN
;П/У НА ERR, ЕСЛИ НЕТ "ГОТ.АРБИТРА" (НЕ БЛОКИРУЕТСЯ З.А.) ИЛИ НЕТ ПР.НАЛИЧИЯ INT
INTPS:CJS А=ERRINT,МАР=РЕ,COND=ARBRDY,IСС,%ТУРЕ%
INTMAS:ENTRY
CJS А=ERRINT,МАР=РЕ,COND=INT,IСС,ISE,%ТУРЕ%
;СБРАСЫВАЕМ ПРЕРЫВАНИЯ
PUSH А=2,МАР=РЕ,FFCNT=CLRINT
;ПОДОЖДЕМ ЧУТЬ-ЧУТЬ
RFCT
;ЧТО-ТО НЕ ТО, ЕСЛИ ВСЕ-ТАКИ ЕСТЬ ПРЕРЫВАНИЯ
CJS А=ERRINT,МАР=РЕ,COND=INT,%ТУРЕ%
;ОТКРЫВАЕМ МАСКУ ПРЕРЫВАНИЙ, ОБНУЛЯЕМ WR1 И 4,5 РГ.БОПП
CONT ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR1,ALU,YDEV=МРМЕМ,IОМР,WRY,MPADR=4
CONT ALUS=ZА,RА=WR1,ALU,YDEV=МРМЕМ,IОМР,WRY,MPADR=5
CRTN FFCNT=SETEI
;
;
П2:ENTRY
;===== INT2 - "МНОГОКРАТНАЯ ОШИБКА"
;      ЗАПУСК АРБИТРА НЕ БЛОКИРУЕТСЯ, ДАННЫЕ "ПОРТИМ" САМИ, МОЖНО СРАВНИВАТЬ
;
CJP А=CONT2,МАР=РЕ
CONT2:CONT А=С2,%TN%
CONT %С3%
;ОШ=1 (НЕ СБРАСЫВАЕТСЯ ПР. НАЛИЧИЯ ПРЕРЫВАНИЙ)
CKL2:CJS А=BEGINT,МАР=РЕ
;"ВКЛЮЧАЕМ" ХЕММИНГ
CONT А=В2,DSRC=PROM,ALUS=DZ,FUNC=EXNOR,Н,ALUD=В,RВ=WR2,ALU,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZА,RА=WR2,ALU,YDST=ADRREG
CONT А=ZER,DSRC=PROM,ЕСА,WRA,ARA=RG3,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR
WASX2:CJP А=WASX2,МАР=РЕ,COND=ARBRDY,IСС
;ПИШЕМ ЧИСЛО В ОЗУ
CONT А=В11,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=РС,ALU,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZА,RА=РС,ALU,YDST=ADRREG
CONT А=СААА,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,ЕСВ,WRB
>,BRA=RG3
;ОШ=2,3 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВАНИЕ)
CJS А=SUBWD,МАР=РЕ,ALUS=ZА,RА=HALF,Н,ALUD=В,RВ=DADR
;ЧИТАЕМ ПО ТОМУ ЖЕ АДРЕСУ,ОШ=4,5,6,7 (НЕ ТОТ Ф.А.;ЕСТЬ ПРЕР.;НЕ ТЕ ДАННЫЕ;ТЕГ)
CJS А=SUBRD,МАР=РЕ,YDST=CCLR
;"ВЫКЛЮЧАЕМ" ХЕММИНГ
CONT А=В2,DSRC=PROM,ALUS=DZ,FUNC=EXNOR,ALU,YDST=ADRREG
CONT А=С1,DSRC=PROM,ЕСА,WRA,ARA=RG3,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR
WАСХ2:CJP А=WАСХ2,МАР=РЕ,COND=ARBRDY,IСС
;ПИШЕМ ТО ЖЕ ЧИСЛО С "ОШИБКОЙ" В 2 РАЗРЯДАХ
CONT А=С3,DSRC=PROM,ALUS=DА,RА=DADR,FUNC=EXOR,Н,ALUD=В,RВ=DADR,ALU,ЕСВ,WRB
>,BRA=RG3
;ОШ=8,9 (НЕ ТОТ Ф.А.;ЕСТЬ ПРЕРЫВАНИЕ)
CJS А=SUBWD,МАР=РЕ,ALUS=ZА,RА=РС,ALU,YDST=ADRREG
;"ВКЛЮЧАЕМ" ХЕММИНГ
CONT А=В2,DSRC=PROM,ALUS=DZ,FUNC=EXNOR,ALU,YDST=ADRREG
CONT А=ZER,DSRC=PROM,ЕСА,WRA,ARA=RG3,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR
>,YDST=CCLR
WASSX2:CJP А=WASSX2,МАР=РЕ,COND=ARBRDY,IСС
;СНОВА ЧИТАЕМ ЧИСЛО, Д.Б."МНОГОКРАТНАЯ ОШИБКА"
;ОШ=10,11,12,13,14 (НЕТ ПР.ПРЕРЫВ.;НЕТ СБРОСА; НЕ ТЕ ДАННЫЕ;ТЕГ;НЕ ТОТ НОМЕР)
CJS А=INTMR,МАР=РЕ,ALUS=ZА,RА=РС,ALU,YDST=ADRREG
;ВКС, ОШ=15,16,17,18,19 (НЕТ ПР.ПРЕРЫВ.;НЕТ СБРОСА; НЕ ТЕ ДАННЫЕ;ТЕГ;НЕ ТОТ N)
CJS А=INTMF,МАР=РЕ,FFCNT=CLRINT,YDST=CCLR
CJP А=CKL2,МАР=РЕ,%TIK3%
;
;
П3:ENTRY
;===== INT3 - "ПРОГРАММНАЯ ИНТЕРПРЕТАЦИЯ ТЕГА ОПЕРАНДА"
;   ЗАПУСК АРБИТРА НЕ БЛОКИРУЕТСЯ, ДАННЫЕ ПРОВЕРЯЕМ
;
CJP А=CONT3,МАР=РЕ
CONT3:CONT А=C11,%TN%
CONT %С3%
;РР = 07С5 Е000, ОШ=1 (НЕТ НАЧАЛЬНОГО СБРОСА ПРЕР.)
CKL3:CJS А=BEGINT,МАР=РЕ
;АДРЕС ОЗУ, УБИРАЕМ БМСП И ОСТАНОВ
CONT А=В11,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=РС,ALU,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZА,RА=РС,ALU,YDST=ADRREG
;ПИШЕМ ЧИСЛО С ТЕГОМ "ПРОГР.ИНТЕРПРЕТАЦИЯ" И "ПРИЗНАК ЧИСЛА": 8 РАЗР.=1
CONT А=В8,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,ЕСВ,WRB,BRA=RG3
;ОШ=2,3 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕР.)
CJS А=SUBWD,МАР=РЕ,ALUS=ZА,RА=HALF,Н,ALUD=В,RВ=DADR
;ЧИТАЕМ ПО ТОМУ ЖЕ АДРЕСУ, ОШ=4,5,6,7(НЕ ТОТ Ф.А.;ЕСТЬ ПРЕР.;НЕ ТЕ ДАННЫЕ;ТЕГ)
CJS А=SUBRD,МАР=РЕ
;УБИРАЕМ БПИНТ В РР: 17-Й РАЗРЯД = 0
CONT А=В17,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ОШ=8,9,10,11,12 (НЕТ ПР.ПРЕРЫВ.;НЕТ СБРОСА;НЕ ТЕ ДАННЫЕ;ТЕГ;НЕ ТОТ НОМЕР)
CJS А=INTMR,МАР=РЕ
CJP А=CKL3,МАР=РЕ,%TIK3%
;
;
П4:ENTRY
;===== INT4 - "ЧУЖОЙ СУММАТОР": 2Р.ТЕГА КОМАНДЫ НЕ РАВЕН 2Р.ТЕГА СУММАТОРА (RG3)
;   ЗАПУСК АРБИТРА НЕ БЛОКИРУЕТСЯ, КОМАНДУ И ТЕГ ПРОВЕРЯЕМ
;
CJP А=CONT4,МАР=РЕ
CONT4:CONT А=C22,%TN%
CONT %С3%
;ПРИЗНАК РЭ - В РГ.ВУТЕ
CONT А=В2,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=ВУТЕ
;ОШ=1 (НЕТ НАЧАЛЬНОГО СБРОСА)
СС4:CJS А=BEGINT,МАР=РЕ,YDST=CCLR
;АДРЕС ОЗУ - В АИСП, УБИРАЕМ БМСП И ОСТАНОВЫ
CONT А=В11,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=РС,ALU,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZА,RА=РС,ALU,YDST=ADRREG
;ПИШЕМ КОМАНДУ С ТЕГОМ "РЭ" ИЛИ "НР" И "ПРИЗНАК КОМАНДЫ": 1Р.=1, 2Р.=0/1
CONT ALUS=ZА,RА=ВУТЕ,Н,CI,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,ЕСВ,WRB,BRA=RG3
;ОШ=2,3 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.)
CJS А=SUBWD,МАР=РЕ,ALUS=ZА,RА=HALF,Н,ALUD=В,RВ=DADR
;ПИШЕМ ТЕГ СУММАТОРА В RG3 С ПРОТИВОПОЛОЖНЫМ ПРИЗНАКОМ "РЭ"
CONT А=В2,DSRC=PROM,ALUS=DА,RА=HALF,FUNC=EXOR,ALU,YDEVT=ECBTAG,WRB,BRA=RG3
;ЧИТАЕМ КОМАНДУ, ОШ=4,5,6,7 (НЕ ТОТ Ф.А.;ЕСТЬ ПРЕР.;НЕ ТА КОМАНДА; ТЕГ)
CJS А=SUBRC,МАР=РЕ
;УБИРАЕМ БЧС В РР: 16-Й РАЗРЯД = 0
CONT А=В16,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ВКС, ОШ=8,9,10,11,12 (НЕТ ПР.ПРЕР.;НЕТ СБРОСА;НЕ ТА КОМАНДА;ТЕГ;НЕ ТОТ НОМЕР)
CJS А=INTMF,МАР=РЕ
;ИНВЕРТИРУЕМ ПРИЗНАК "РЭ", ИДЕМ НА ЦИКЛ
CONT А=В2,DSRC=PROM,ALUS=DА,RА=ВУТЕ,FUNC=EXOR,Н,ALUD=В,RВ=ВУТЕ
CJP А=СС4,МАР=РЕ,%TIK3%
;
;
П5:ENTRY
;===== INT5 - "ЧУЖОЙ ОПЕРАНД": 2Р.ТЕГА ОПЕРАНДА НЕ РАВЕН 2Р.ТЕГА СУММАТОРА (RG3)
;   ЗАПУСК АРБИТРА НЕ БЛОКИРУЕСТСЯ, ДАННЫЕ И ТЕГ ПРОВЕРЯЕМ
;
CJP А=CONT5,МАР=РЕ
CONT5:CONT А=C23,%TN%
CONT %С3%
;ЧИТАЕМ ПРИЗНАК РЭ
CONT А=В2,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=ВУТЕ
;ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, РР = 07С5 Е000, ОШ=1 (НЕТ НАЧАЛЬНОГО СБРОСА)
СС5:CJS А=BEGINT,МАР=РЕ
;АДРЕС ОЗУ - В АИСП, УБИРАЕМ БМСП И ОСТАНОВЫ
CONT А=В11,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=РС,ALU,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZА,RА=РС,ALU,YDST=ADRREG
;ПИШЕМ ОПЕРАНД С ТЕГОМ "РЭ" ИЛИ "НР" И "ПРИЗНАК ЧИСЛА": 2Р.=0/1
CONT ALUS=ZА,RА=ВУТЕ,Н,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,ЕСВ,WRB,BRA=RG3
;ОШ=2,3 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.)
CJS А=SUBWD,МАР=РЕ,ALUS=ZА,RА=HALF,Н,ALUD=В,RВ=DADR
;ПИШЕМ ТЕГ СУММАТОРА В RG3 С ПРОТИВОПОЛОЖНЫМ ПРИЗНАКОМ "РЭ"
CONT А=В2,DSRC=PROM,ALUS=DА,RА=ВУТЕ,FUNC=EXOR,ALU,YDEVT=ECBTAG,WRB,BRA=RG3
;ЧИТАЕМ ПО ТОМУ ЖЕ АДР.,ОШ=4,5,6,7 (НЕ ТОТ Ф.А.;ЕСТЬ ПРЕР.;НЕ ТЕ ДАННЫЕ;ТЕГ)
CJS А=SUBRD,МАР=РЕ
;УБИРАЕМ БЧОП В РР: 14-Й РАЗРЯД = 0, СНОВА ЧИТАЕМ ОПЕРАНД
CONT А=В14,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ОШ=8,9,10,11,12 (НЕТ ПР.ПРЕР.;НЕТ СБРОСА; НЕ ТЕ ДАННЫЕ; ТЕГ; НЕ ТОТ НОМЕР)
CJS А=INTMR,МАР=РЕ
;ИНВЕРТИРУЕМ ПРИЗНАК "РЭ", ИДЕМ НА ЦИКЛ
CONT А=В2,DSRC=PROM,ALUS=DА,RА=ВУТЕ,FUNC=EXOR,Н,ALUD=В,RВ=ВУТЕ
CJP А=СС5,МАР=РЕ,%TIK3%
;
;
П6:ENTRY
;===== INT6 - "ЗАЩИТА АДРЕСА ПРИ ЧТЕНИИ"
;   ЗАПУСК АРБИТРА НЕ БЛОКИРУЕТСЯ, ДАННЫЕ ПРОВЕРЯЕМ
;
CJP А=CONT6,МАР=РЕ
CONT6:CONT А=C12,%TN%
CONT %С3%
;ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОШ=1
СС6:CJS А=BEGINT,МАР=РЕ
;АДРЕС ОЗУ - В АИСП, УБИРАЕМ БМСП И ОСТАНОВЫ
CONT А=В11,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=РС,ALU,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZА,RА=РС,ALU,YDST=ADRREG
;ПИШЕМ ЧИСЛО С ТЕГОМ "ЗАПРЕТ ЧТЕНИЯ ОПЕР." И "ПРИЗНАК ЧИСЛА": 3 РАЗР.=1
CONT А=В3,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,ЕСВ,WRB,BRA=RG3
;ОШ=2,3 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕР.)
CJS А=SUBWD,МАР=РЕ,ALUS=ZА,RА=HALF,Н,ALUD=В,RВ=DADR
;ЧИТАЕМ ПО ТОМУ ЖЕ АДР.,ОШ=4,5,6,7 (НЕ ТОТ Ф.А.;ЕСТЬ ПРЕР.;НЕ ТЕ ДАННЫЕ;ТЕГ)
CJS А=SUBRD,МАР=РЕ
;УБИРАЕМ БПТЧ В РР: 15-Й РАЗРЯД = 0
CONT А=В15,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ЧИТАЕМ, ОШ=8,9,10,11,12 (НЕТ ПР.ПРЕР.;НЕТ СБРОСА;НЕ ТЕ ДАННЫЕ;ТЕГ;НЕ ТОТ N)
CJS А=INTMR,МАР=РЕ
CJP А=СС6,МАР=РЕ,%TIK3%
;
;
П7:ENTRY
;===== INT7 - "КОНТРОЛЬ КОМАНДЫ": "0" В 1 РАЗРЯДЕ ТЕГА
;   З.А.НЕ БЛОКИРУЕТСЯ, КОМАНДУ ПРОВЕРЯЕМ
;
CJP А=CONT7,МАР=РЕ
CONT7:CONT А=С13,%TN%
CONT %С3%
;ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОШ=1 (НЕ СБРАСЫВАЕТСЯ ПР.ПРЕРЫВАНИЙ)
СС7:CJS А=BEGINT,МАР=РЕ,YDST=CCLR
;АДРЕС ОЗУ - В АИСП, УБИРАЕМ БМСП И ОСТАНОВЫ
CONT А=В11,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=РС,ALU,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZА,RА=РС,ALU,YDST=ADRREG
;ПИШЕМ ЧИСЛО С ТЕГОМ "ПРИЗНАК ЧИСЛА": 1 РАЗР.=0
CONT А=CFFF,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=DADR,ALU,ЕСВ,WRB,BRA=RG3
;ОШ=2,3 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВАНИЕ)
CJS А=SUBWD,МАР=РЕ,ALUS=ZА,FUNC=AND,Н,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,WRB
>,BRA=RG3
;ЧИТАЕМ КОМАНДУ ПО ЭТОМУ АДРЕСУ:
;ОШ=4,5,6,7,8 (НЕТ ПР.ПРЕР.;НЕТ СБРОСА;НЕ ТА КОМАНДА;ТЕГ;НЕ ТОТ N)
CJS А=INTMF,МАР=РЕ,YDST=CCLR
CJP А=СС7,МАР=РЕ,%TIK3%
;
;
П8:ENTRY
;===== INT8 - "ЗАЩИТА ВЫБОРКИ КОМАНДЫ": "1" В 5 РАЗРЯДЕ ТЕГА КОМАНДЫ
;   З.А.НЕ БЛОКИРУЕТСЯ, КОМАНДУ,ТЕГ ПРОВЕРЯЕМ
;
CJP А=CONT8,МАР=РЕ
CONT8:CONT А=С19,%TN%
CONT %С3%
;ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОШ=1 (НЕ СБРАСЫВАЕТСЯ ПР.ПРЕРЫВАНИЙ)
СС8:CJS А=BEGINT,МАР=РЕ,YDST=CCLR
;АДРЕС ОЗУ - В АИСП, УБИРАЕМ БМСП И ОСТАНОВЫ
CONT А=В11,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=РС,ALU,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZА,RА=РС,ALU,YDST=ADRREG
;ПИШЕМ ЧИСЛО С ТЕГОМ "ЗАПРЕТ ВЫБОРКИ КОМАНДЫ" И "ПРИЗНАК КОМАНДЫ": 1,5Р.=1
CONT А=В5,DSRC=PROM,ALUS=DZ,Н,CI,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,ЕСВ,WRB
>,BRA=RG3
;ОШ=2,3 (НЕ ТОТ Ф.А.;ЕСТЬ ПРЕРЫВ.)
CJS А=SUBWD,МАР=РЕ,ALUS=ZА,RА=HALF,Н,ALUD=В,RВ=DADR
;ЧИТАЕМ КОМАНДУ ПО ЭТОМУ АДРЕСУ
;ОШ=4,5,6,7,8 (НЕТ ПР.ПРЕР.;НЕТ СБРОСА;НЕ ТА КОМАНДА;ТЕГ;НЕ ТОТ N)
CJS А=INTMF,МАР=РЕ,YDST=CCLR
CJP А=СС8,МАР=РЕ,%TIK3%
;
;
П9:ENTRY
;===== INT9 - "ЗАЩИТА ПЕРЕДАЧИ УПРАВЛЕНИЯ": "1" В 6 РАЗРЯДЕ ТЕГА КОМАНДЫ + ППУ
;   З.А.НЕ БЛОКИРУЕТСЯ, КОМАНДУ,ТЕГ ПРОВЕРЯЕМ
;
CJP А=CONT9,МАР=РЕ
CONT9:CONT А=С20,%TN%
CONT %С3%
;ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОШ=1 (НЕ СБРАСЫВАЕТСЯ)
СС9:CJS А=BEGINT,МАР=РЕ,YDST=CCLR
;АДРЕС ОЗУ - В АИСП, УБИРАЕМ БМСП И ОСТАНОВЫ
CONT А=В11,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=РС,ALU,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZА,RА=РС,ALU,YDST=ADRREG
;ПИШЕМ ЧИСЛО С ТЕГОМ "ЗАПРЕТ П/У" И "ПРИЗНАК КОМАНДЫ": 1,6 РАЗР.=1
CONT А=В6,DSRC=PROM,ALUS=DZ,Н,CI,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,ЕСВ,WRB,BRA=RG3
;ОШ=2,3 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВАНИЕ)
CJS А=SUBWD,МАР=РЕ,ALUS=ZА,RА=HALF,Н,ALUD=В,RВ=DADR
;ЧИТАЕМ, ОШ=4,5,6,7 (НЕ ТОТ Ф.А.;ЕСТЬ ПРЕРЫВ.;НЕ ТА КОМАНДА;ТЕГ)
CJS А=SUBRC,МАР=РЕ
;УСТАНАВЛИВАЕМ ППУ=0 ПОЛЕМ FFCNT
CONT А=В30,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=OR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ЧИТАЕМ СНОВА, ОШ=8,9,10,11 (НЕ ТОТ Ф.А.;ЕСТЬ ПРЕР.;НЕ ТА КОМАНДА; ТЕГ)
CJS А=SUBRC,МАР=РЕ,FFCNT=CLRJMP
;УСТАНАВЛИВАЕМ ППУ=1 ПОЛЕМ FFCNT, СНОВА ЧИТАЕМ КОМАНДУ
CONT FFCNT=SETJMP
;ОШ=12,13,14,15,16 (НЕТ ПР.ПРЕР.;НЕТ СБРОСА; НЕ ТА КОМАНДА;ТЕГ; НЕ ТОТ НОМЕР)
CJS А=INTMF,МАР=РЕ
;УСТАНАВЛИВАЕМ ППУ=1 ИЗ РР
CONT А=В30,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=OR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ОШ=17,18,19,20,21 (НЕТ ПР.ПРЕРЫВ.;НЕТ СБРОСА;НЕ ТА КОМАНДА;ТЕГ;НЕ ТОТ НОМЕР)
CJS А=INTMF,МАР=РЕ
CJP А=СС9,МАР=РЕ,%TIK3%
;
;
П10:ENTRY
;===== INT10 - "ПРОГРАММНАЯ ИНТЕРПРЕТАЦИЯ ТЕГА КОМАНДЫ"
;   З.А.НЕ БЛОКИРУЕТСЯ, КОМАНДУ И ТЕГ ПРОВЕРЯЕМ.
;   МОЖНО ПИСАТЬ ДРУГИЕ ЧИСЛА, СОЗДАВАЯ УСЛОВИЯ ДЛЯ НЕСКОЛЬКИХ ПРЕРЫВАНИЙ!
;
CJP А=CONT10,МАР=РЕ
CONT10:CONT А=С10,%TN%
CONT %С3%
;ДЕЛАЕМ СБРОС ПРЕРЫВАНИЙ: ОШ=1 (НЕТ СБРОСА); РР = 07С5 Е000: БПИНТ = БП = 1
СС10:CJS А=BEGINT,МАР=РЕ,YDST=CCLR
;АДРЕС ОЗУ ПИШЕМ В АИСП, УБИРАЕМ БМСП И ОСТАНОВ, ЧТОБ НЕ БЫЛО ЛИШНИХ INT
CONT А=В11,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=РС,ALU,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZА,RА=РС,ALU,YDST=ADRREG
;ПИШЕМ ЧИСЛО С ТЕГОМ "ПРОГР.ИНТЕРПРЕТАЦИЯ" И "ПРИЗНАК КОМАНДЫ": 1,8 РАЗР.=1
CONT А=В8,DSRC=PROM,ALUS=DZ,Н,CI,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,ЕСВ,WRB,BRA=RG3
;ОШ=2,3 (НЕ ТОТ Ф.А.;ЕСТЬ ПРЕР.)
CJS А=SUBWD,МАР=РЕ,ALUS=ZА,RА=HALF,Н,ALUD=В,RВ=DADR
;ЧИТАЕМ КОМАНДУ ПО ТОМУ ЖЕ АДРЕСУ
;ОШ=4,5,6,7 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕР.; НЕ ТА КОМАНДА; ТЕГ)
CJS А=SUBRC,МАР=РЕ
;УБИРАЕМ БПИНТ В РР: 17-Й РАЗРЯД = 0, ЧИТАЕМ КОМАНДУ
CONT А=В17,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ОШ=8,9,10,11,12 (НЕТ ПР.ПРЕР.;НЕТ СБРОСА;НЕ ТА КОМАНДА;ТЕГ;НЕ ТОТ НОМЕР)
CJS А=INTMF,МАР=РЕ
CJP А=СС10,МАР=РЕ,%TIK3%
;
;
П11:ENTRY
;===== INT11 - "ЗАЩИТА АДРЕСА ПРИ ЗАПИСИ"
;
CJP А=CONT11,МАР=РЕ
CONT11:CONT А=С21,%TN%
CONT %С3%
;ПРОВЕРЯЕМ НАЧ.СБРОС ПРЕРЫВАНИЙ: ОШ=1; РР = 07С5 Е000: БПТЗ = БП = 1
СС11:CJS А=BEGINT,МАР=РЕ
;АДРЕС ОЗУ ПИШЕМ В АИСП, УБИРАЕМ БМСП И ОСТАНОВ, ЧТОБ НЕ БЫЛО ЛИШНИХ INT
CONT А=В11,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=РС,ALU,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZА,RА=РС,ALU,YDST=ADRREG
;ПИШЕМ ЧИСЛО С ТЕГОМ "НЕЛЬЗЯ ПИСАТЬ": 4-Й РАЗРЯД ТЕГА = 1
CONT А=В4,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,ЕСВ,WRB,BRA=RG3
;ОШ = 2,3 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.)
CJS А=SUBWD,МАР=РЕ
;ПИШЕМ F...F ПО ТОМУ ЖЕ АДРЕСУ, ГДЕ УСТАНОВЛЕН "ЗАПРЕТ ЗАПИСИ", ТЕГ НЕ МЕНЯЕМ!
CONT А=CFFF,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=DADR,ALU,ЕСВ,WRB,BRA=RG3
;ОШ = 4,5 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕР.)
CJS А=SUBWD,МАР=РЕ
;ПРОВЕРЯЕМ,ЧТО ЗАПИСАЛИ: ОШ=6,7,8,9 (НЕ ТОТ Ф.А.;ЕСТЬ ПРЕР.;НЕ ТЕ ДАННЫЕ;ТЕГ)
CJS А=SUBRD,МАР=РЕ,YDST=CCLR
;УБИРАЕМ БПТЗ В РР: 19-Й РАЗРЯД = 0
CONT А=В19,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;СНОВА ПИШЕМ ТУДА, ГДЕ УСТАНОВЛЕН "ЗАПРЕТ ЗАПИСИ"
CONT А=СААА,DSRC=PROM,ЕСА,WRA,ARA=RG3
;ОШ=10,11,12 (НЕТ ПР.ПРЕР.;НЕТ СБРОСА; НЕ ТОТ ВЕКТОР)
CJS А=INTMW,МАР=РЕ
;УБЕДИМСЯ, ЧТО НЕ ЗАПИСАЛОСЬ
;СБРОС КЭША Д.ДЕЛАТЬСЯ В П/П РЕПРЕ ПО INT11, ПОЭТОМУ СДЕСЬ МОЖНО НЕ ДЕЛАТЬ
;ОШ=13,14,15,16 (НЕ ТОТ Ф.А.;ЕСТЬ ПРЕР.;НЕ ТЕ ДАННЫЕ;ТЕГ)
CJS А=SUBRD,МАР=РЕ,FFCNT=CLRINT
CJP А=СС11,МАР=РЕ,%TIK3%
;
;
П12:ENTRY
;===== INT12,13 - "TIME-OUT" ПРИ ОБРАЩЕНИИ К ОЗУ И ШИНЕ: МОДЕЛИРУЕТСЯ АППАРАТНО;
;   З.А.НЕ БЛОКИРУЕТСЯ, ДАННЫХ НЕТ. "TIME-OUT" ПОЯВЛЯЕТСЯ ЧЕРЕЗ 100 МИКРОСЕКУНД.
;
CJP А=CONT12,МАР=РЕ
CONT12:CONT А=С3,%TN%
CONT %С3%
;ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОШ=1 (НЕТ СБРОСА)
СС12:CJS А=BEGINT,МАР=РЕ
CONT А=В16,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=РС,ALU,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZА,RА=РС,ALU,YDST=ADRREG
;ПИШЕМ ЧИСЛО В ОЗУ
CONT А=MNH,DSRC=PROM,ALUS=DZ,Н,CI,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,ЕСВ,WRB
>,BRA=RG3
CONT YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR,ALUS=ZА,RА=HALF
>,Н,ALUD=В,RВ=DADR
;ЖДЕМ "ГОТ.АРБИТРА"
WА12:CJP А=WА12,МАР=РЕ,COND=ARBRDY,IСС
;ОШ=2,3 (НЕТ ПР.ПРЕРЫВАНИЯ; НЕТ СБРОСА)
CJS А=INTMAS,МАР=РЕ
;ПОВТОРЯЕМ БЕЗ МАСКИ, ЖДЕМ НОМЕР ПРЕРЫВАНИЯ В WR1; ОШ=4 (НЕ ТОТ НОМЕР)
CJS А=INTWAT,МАР=РЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR
>,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR2
CJP А=СС12,МАР=РЕ,%TIK3%
;
;
П14:ENTRY
;===== INT14 - "МАТ.АДРЕС РАВЕН "0": АДРЕС = 0, БП = 0,1; З.А.БЛОКИРУЕТСЯ
;
CJP А=CONT14,МАР=РЕ
CONT14:CONT А=С14,%TN%
CONT %С3%
;УСТАНАВЛИВАЕМ БП=1; ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОШ=1 (НЕТ СБРОСА)
СС14:CJS А=BEGINT,МАР=РЕ
;0-Й АДРЕС ОЗУ
CONT А=ZER,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=РС,ALU,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZА,RА=РС,ALU,YDST=ADRREG
;ПИШЕМ ЧИСЛО В ОЗУ
CONT А=MNH,DSRC=PROM,ALUS=DZ,Н,CI,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,ЕСВ,WRB
>,BRA=RG3
;ОШ=2,3,4,5 (НЕТ "ГОТ.А."; НЕТ ПР.ПРЕР.; НЕТ СБРОСА; НЕ ТОТ НОМЕР)
CJS А=INTPSW,МАР=РЕ,ALUS=ZА,RА=HALF,Н,ALUD=В,RВ=DADR
;ОШ=6,7,8,9 (НЕТ "ГОТ.А.";НЕТ ПР.ПРЕР.; НЕТ СБРОСА; НЕ ТОТ НОМЕР)
CJS А=INTPSR,МАР=РЕ,FFCNT=CLRINT
;ОШ=10,11,12,13 (НЕТ "ГОТ.А.";НЕТ ПР.ПРЕР.; НЕТ СБРОСА; НЕ ТОТ НОМЕР)
CJS А=INTPSF,МАР=РЕ,FFCNT=CLRINT
;УБИРАЕМ БП, ПИШЕМ ФИЗ.СТРАНИЦУ В РФС И РП, УБИРАЕМ БМСП И ОСТАНОВЫ
CONT А=В27,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
CONT А=В15,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=РССОРУ,ALU,YDST=PHYSPG,FFCNT=CLRINT
CJS А=СН,МАР=РЕ,ALUS=ZА,RА=РС,ALU,YDST=ADRREG
CONT ALUS=ZА,RА=РССОРУ,ALU,YDEV=PSMEM,WRY
;ОШ=14,15,16,17 (НЕТ "ГОТ.А."; НЕТ ПР.ПРЕР.; НЕТ СБРОСА; НЕ ТОТ НОМЕР)
CJS А=INTPSW,МАР=РЕ
;ОШ=18,19,20,21 (НЕТ "ГОТ.А."; НЕТ ПР.ПРЕР.; НЕТ СБРОСА; НЕ ТОТ НОМЕР)
CJS А=INTPSR,МАР=РЕ,FFCNT=CLRINT
;ОШ=22,23,24,25 (НЕТ "ГОТ.А."; НЕТ ПР.ПРЕР.; НЕТ СБРОСА; НЕ ТОТ НОМЕР)
CJS А=INTPSF,МАР=РЕ,FFCNT=CLRINT
CJP А=СС14,МАР=РЕ,%TIK3%
;
;
П15:ENTRY
;===== INT15 - "ЧУЖОЙ РП ПРИ ЧТЕНИИ/ЗАПИСИ ОПЕРАНДА"; З.А.БЛОКИРУЕТСЯ
;
CJP А=CONT15,МАР=РЕ
CONT15:CONT А=С15,%TN%
CONT %С3%
;ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОШ=1 (НЕТ СБРОСА)
СС15:CJS А=BEGINT,МАР=РЕ
;ЧИТАЕМ РАЗНЫЕ CONST ДЛЯ М.А., Ф.А., РНП И Т.Д.: М.А.СТРАНИЦЫ - В WR1
CONT А=В13,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;АДРЕС ВНУТРИ СТРАНИЦЫ - В WR2
CONT А=МВ,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR2
;Ф.А.СТРАНИЦЫ - В WR5
CONT А=В15,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR5
;N ПРОЦЕССА, ДОСТУПНОГО ВСЕМ - В WR4
CONT А=МВ,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR4
;"СОБИРАЕМ" И ПИШЕМ, КОГО КУДА
;МАТ.АДРЕС - В АИСП., РАСЧЕТНЫЙ МАТ.АДРЕС - В РС, ПОКА БП=1, УБИРАЕМ БМСП И ОСТ.
CONT ALUS=АВ,RА=WR1,RВ=WR2,FUNC=OR,Н,ALUD=Q,ALU,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZQ,Н,ALUD=В,RВ=РС,ALU,YDST=ADRREG
;Ф.А.+ N ПРОЦЕССА + БЗО + БЗЗ - В РП
CONT ALUS=АВ,RА=WR5,RВ=WR4,FUNC=OR,Н,ALUD=Q
CONT А=В9,DSRC=PROM,ALUS=DQ,FUNC=OR,Н,ALUD=Q
CONT А=В10,DSRC=PROM,ALUS=DQ,FUNC=OR,Н,ALU,YDEV=PSMEM,WRY
;ПИШЕМ ЧИСЛО В ОЗУ БЕЗ ВСЯКИХ ПРЕРЫВАНИЙ: БП=1
CONT А=MNH,DSRC=PROM,ALUS=DZ,Н,CI,ALUD=В,RВ=DADR,ALU,ЕСВ,WRB,BRA=RG3
;ОШ=2,3 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕЫВ.)
CJS А=SUBWD,МАР=РЕ,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,WRB
>,BRA=RG3
;ОШ=4,5,6,7 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.; НЕ ТЕ ДАННЫЕ; ТЕГ)
CJS А=SUBRD,МАР=РЕ
;"ОТКРЫВАЕМ" ПРИПИСКУ (ОСТАЛЬНЫЕ БЛОКИРОВКИ НЕ СНИМАЕМ)
CONT А=В27,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ВЫЧИСЛЯЕМ ФИЗ.АДРЕС, ПИШЕМ ЧИСЛО, ЧИТАЕМ
CJS А=СН,МАР=РЕ,ALUS=АВ,RА=WR5,RВ=WR2,FUNC=OR,Н,ALUD=Q,ALU,YDST=PHYSPG
;ОШ=8,9 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.)
CJS А=SUBWD,МАР=РЕ,ALUS=ZQ,Н,ALUD=В,RВ=РС
;ОШ=10,11,12,13 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.; НЕ ТЕ ДАННЫЕ; ТЕГ)
CJS А=SUBRD,МАР=РЕ
;УБИРАЕМ БПНП, НО В РП - НОМЕР, ДОСТУПНЫЙ ВСЕМ ПРОЦЕССАМ, INT НЕ Д,БЫТЬ
CONT А=В26,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ОШ=14,15 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.)
CJS А=SUBWD,МАР=РЕ,ALUS=ZВ,RВ=DADR,Н,CI,ALUD=В,ALU,ЕСВ,WRB,BRA=RG3
;ОШ=16,17,18,19 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.; НЕ ТЕ ДАННЫЕ; ТЕГ)
CJS А=SUBRD,МАР=РЕ
;ПИШЕМ В РП И РНП ОДИН И ТОТ ЖЕ НОМЕР ПРОЦЕССА, INT НЕ Д.БЫТЬ
CONT А=С32,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR4
CONT А=В9,DSRC=PROM,ALUS=DА,RА=WR4,Н,ALUD=Q
CONT А=В10,DSRC=PROM,ALUS=DQ,Н,ALUD=Q
CONT ALUS=AQ,RА=WR5,ALU,YDST=PROCN,YDEV=PSMEM,WRY
;ОШ=20,21 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.)
CJS А=SUBWD,МАР=РЕ,ALUS=ZВ,RВ=DADR,Н,CI,ALUD=В,ALU,ЕСВ,WRB,BRA=RG3
;ОШ=22,23,24,25 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.;НЕ ТЕ ДАННЫЕ; ТЕГ)
CJS А=SUBRD,МАР=РЕ
;МЕНЯЕМ N ПРОЦЕССА В РНП ДЛЯ INT15
CONT А=С7,DSRC=PROM,ALUS=DА,RА=WR4,FUNC=EXOR,Н,ALU,YDST=PROCN
;ИДЕМ НА ОБЩУЮ П/П ПРОВЕРКИ INT ПРИ ЗАПИСИ ОПЕРАДА
;ОШ=26,27,28,29 (НЕТ "ГОТ.А.";НЕТ ПР.ПРЕР.; НЕТ СБРОСА; НЕ ТОТ НОМЕР)
CJS А=INTPSW,МАР=РЕ
;ПРИ ЧТЕНИИ ОПЕРАНДА,ОШ=30,31,32,33 (НЕТ:"ГОТ.А";ПР.ПРЕР.;СБРОСА;НЕ ТОТ НОМЕР)
CJS А=INTPSR,МАР=РЕ,FFCNT=CLRINT
CJP А=СС15,МАР=РЕ,%TIK3%
;
;
П16:ENTRY
;===== INT16 - "ЧУЖОЙ РП ПРИ ВЫБОРКЕ КОМАНДЫ"
;
CJP А=CONT16,МАР=РЕ
CONT16:CONT А=С16,%TN%
CONT %С3%
;ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОШ=1 (НЕТ СБРОСА)
СС16:CJS А=BEGINT,МАР=РЕ
;ЧИТАЕМ РАЗНЫЕ CONST ДЛЯ М.А., Ф.А., РНП И Т.Д.: М.А.СТРАНИЦЫ - В WR1
CONT А=В13,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;АДРЕС ВНУТРИ СТРАНИЦЫ - В WR2
CONT А=МВ,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR2
;Ф.А.СТРАНИЦЫ - В WR5
CONT А=В15,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR5
;N ПРОЦЕССА, ДОСТУПНОГО ВСЕМ - В WR4
CONT А=МВ,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR4
;"СОБИРАЕМ" И ПИШЕМ, КОГО КУДА
;МАТ.АДРЕС - В АИСП., РАСЧЕТНЫЙ МАТ.АДРЕС - В РС, УБИРАЕМ БМСП И ОСТАНОВ
CONT ALUS=АВ,RА=WR1,RВ=WR2,FUNC=OR,Н,ALUD=Q,ALU,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZQ,Н,ALUD=В,RВ=РС,ALU,YDST=ADRREG
;Ф.А.+ N ПРОЦЕССА + БЗО + БЗЗ - В РП
CONT ALUS=АВ,RА=WR5,RВ=WR4,FUNC=OR,Н,ALUD=Q
CONT А=В9,DSRC=PROM,ALUS=DQ,FUNC=OR,Н,ALUD=Q
CONT А=В10,DSRC=PROM,ALUS=DQ,FUNC=OR,Н,ALU,YDEV=PSMEM,WRY
;ПИШЕМ КОМАНДУ В ОЗУ БЕЗ ВСЯКИХ ПРЕРЫВАНИЙ: БП=1
CONT А=MNH,DSRC=PROM,ALUS=DZ,Н,CI,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,ЕСВ,WRB
>,BRA=RG3
;ОШ=2,3 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.)
CJS А=SUBWD,МАР=РЕ,ALUS=ZА,RА=HALF,Н,ALUD=В,RВ=DADR
;ОШ=4,5,6,7 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.; НЕ ТА КОМАНДА; ТЕГ)
CJS А=SUBRC,МАР=РЕ,YDST=CCLR
;"ОТКРЫВАЕМ" ПРИПИСКУ (ОСТАЛЬНЫЕ БЛОКИРОВКИ НЕ СНИМАЕМ)
CONT А=В27,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ВЫЧИСЛЯЕМ ФИЗ.АДРЕС, ПРОЧИЩАЕМ ОЗУ ОСТАНОБА И БМСП, ПИШЕМ ЧИСЛО, ЧИТАЕМ
CJS А=СН,МАР=РЕ,ALUS=АВ,RА=WR5,RВ=WR2,FUNC=OR,Н,ALUD=Q,ALU,YDST=PHYSPG
;ОШ=8,9 (НЕ ТОТ Ф.А, ЕСТЬ ПРЕРЫВ.)
CJS А=SUBWD,МАР=РЕ,ALUS=ZQ,Н,ALUD=В,RВ=РС
;ОШ=10,11,12,13 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.; НЕ ТА КОМАНДА; ТЕГ)
CJS А=SUBRC,МАР=РЕ,YDST=CCLR
;УБИРАЕМ БПНП, НО В РП - НОМЕР, ДОСТУПНЫЙ ВСЕМ ПРОЦЕССАМ, INT НЕ Д,БЫТЬ
CONT А=В26,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ОШ=14,15 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.)
CJS А=SUBWD,МАР=РЕ,ALUS=ZВ,RВ=DADR,Н,CI,ALUD=В,ALU,ЕСВ,WRB,BRA=RG3
;ОШ=16,17,18,19 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.; НЕ ТА КОМАНДА; ТЕГ)
CJS А=SUBRC,МАР=РЕ,YDST=CCLR
;ПИШЕМ В РП И РНП ОДИН И ТОТ ЖЕ НОМЕР ПРОЦЕССА, INT НЕ Д.БЫТЬ
CONT А=С32,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR4
CONT А=В9,DSRC=PROM,ALUS=DА,RА=WR4,Н,ALUD=Q
CONT А=В10,DSRC=PROM,ALUS=DQ,Н,ALUD=Q
CONT ALUS=AQ,RА=WR5,ALU,YDST=PROCN,YDEV=PSMEM,WRY
;ОШ=20,21 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.)
CJS А=SUBWD,МАР=РЕ,ALUS=ZВ,RВ=DADR,Н,CI,ALUD=В,ALU,ЕСВ,WRB,BRA=RG3
;ОШ=22,23,24,25 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.; НЕ ТЕ ДАННЫЕ; ТЕГ)
CJS А=SUBRC,МАР=РЕ,YDST=CCLR
;МЕНЯЕМ N ПРОЦЕССА В РНП ДЛЯ INT16
CONT А=С7,DSRC=PROM,ALUS=DА,RА=WR4,FUNC=EXOR,Н,ALU,YDST=PROCN
;INT ПРИ ВКС, ОШ=26,27,28,29 (НЕТ:"ГОТ.А.";ПР.ПРЕРЫВ.; СБРОСА;НЕ ТОТ НОМЕР)
CJS А=INTPSF,МАР=РЕ
CJP А=СС16,МАР=РЕ,%TIK3%
;
;
П17:ENTRY
;===== INT17 - "ЗАЩИТА СТРАНИЦЫ ПРИ ОБРАЩЕНИИ"
;
CJP А=CONT17,МАР=РЕ
CONT17:CONT А=С17,%TN%
CONT %С3%
;ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОШ=1 (НЕТ СБРОСА)
СС17:CJS А=BEGINT,МАР=РЕ
;УБИРАЕМ БЛОКИРОВКИ БП И БЗО
CONT А=В27,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT
CONT А=В24,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ЧИТАЕМ РАЗНЫЕ CONST ДЛЯ М.А., Ф.А., РНП И Т.Д.: М.А.СТРАНИЦЫ - В WR1
CONT А=В13,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;АДРЕС ВНУТРИ СТРАНИЦЫ - В WR2
CONT А=МВ,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR2
;Ф.А.СТРАНИЦЫ - В WR5
CONT А=В15,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR5
;"СОБИРАЕМ" И ПИШЕМ, КОГО КУДА
;МАТ.АДРЕС - В АИСП., РАСЧЕТНЫЙ ФИЗ.АДРЕС - В РС, УБИРАЕМ БМСП И ОСТАНОВ
CONT ALUS=АВ,RА=WR1,RВ=WR2,FUNC=OR,Н,ALUD=Q,ALU,YDST=ADRREG
CONT ALUS=АВ,RА=WR5,RВ=WR2,FUNC=OR,Н,ALUD=Q,ALU,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZQ,Н,ALUD=В,RВ=РС
;Ф.А.+ N ПРОЦЕССА=0 + БЗО + БЗЗ - В РП
CONT А=В9,DSRC=PROM,ALUS=DА,RА=WR5,FUNC=OR,Н,ALUD=В,RВ=WR4
CONT А=В10,DSRC=PROM,ALUS=DА,RА=WR4,FUNC=OR,Н,ALU,YDEV=PSMEM,WRY
;В РР НЕТ БЛОКИРОВКИ, НО СТРАНИЦА В РП НЕ ЗАЩИЩЕНА, INT НЕ ДОЛЖНО БЫТЬ
CONT А=MNH,DSRC=PROM,ALUS=DZ,Н,CI,ALUD=В,RВ=DADR,ALU,ЕСВ,WRB,BRA=RG3
;ОШ=2,3 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.)
CJS А=SUBWD,МАР=РЕ,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,WRB
>,BRA=RG3
;ОШ=4,5,6,7 (НЕ ТОТ Ф.А; ЕСТЬ ПРЕРЫВ.; НЕ ТЕ ДАННЫЕ; ТЕГ)
CJS А=SUBRD,МАР=РЕ
;ОШ=8,9 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕР.)
CJS А=SUBWD,МАР=РЕ,ALUS=ZВ,RВ=HALF,CI,ALUD=В,ALU,YDEVT=ECBTAG,WRB,BRA=RG3
;ОШ=10,11,12,13 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕР.; НЕ ТЕ ДАННЫЕ; ТЕГ)
CJS А=SUBRC,МАР=РЕ,YDST=CCLR
;ЗАЩИЩАЕМ СТРАНИЦУ В РП, НО УСТАНАВЛИВАЕМ БЗО В РР, INT НЕ Д,БЫТЬ
CONT А=В24,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
CONT А=В9,DSRC=PROM,ALUS=DА,RА=WR5,Н,ALU,YDEV=PSMEM,WRY
;ОШ=14,15 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.)
CJS А=SUBWD,МАР=РЕ,ALUS=ZВ,RВ=DADR,Н,CI,ALUD=В,ALU,ЕСВ,WRB,BRA=RG3
;ОШ=16,17,18,19 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.; НЕ ТЕ ДАННЫЕ; ТЕГ)
CJS А=SUBRC,МАР=РЕ,YDST=CCLR
;ОШ=20,21 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.)
CJS А=SUBWD,МАР=РЕ,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,WRB
>,BRA=RG3
;ОШ=22,23,24,25 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.; НЕ ТЕ ДАННЫЕ; ТЕГ)
CJS А=SUBRD,МАР=РЕ
;УБИРАЕМ БЗО В РР ДЛЯ INT17
CONT А=В24,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,ALUD=В,RВ=MANT,ALU,YDST=CNT
;INT ПРИ ЗАПИСИ, ОШ=26,27,28,29 (НЕТ:"ГОТ.А.";ПР.ПРЕР.;СБРОСА;НЕ ТОТ НОМЕР)
CJS А=INTPSW,МАР=РЕ
;ПРИ ЧТЕНИИ ОПЕРАНДА,ОШ=30,31,32,33 (НЕТ:"ГОТ.А";ПР.ПРЕР.;СБРОСА;НЕ ТОТ НОМЕР)
CJS А=INTPSR,МАР=РЕ
;ПРИ ВКС, ОШ=34,35,36,37 (НЕТ:"ГОТ.А."; ПР.ПРЕР.; СБРОСА; НЕ ТОТ НОМЕР)
CJS А=INTPSF,МАР=РЕ,YDST=CCLR
CJP А=СС17,МАР=РЕ,%TIK3%
;
;
П18:ENTRY
;===== INT18 - "ЗАЩИТА СТРАНИЦЫ ПРИ ЗАПИСИ"
;
CJP А=CONT18,МАР=РЕ
CONT18:CONT А=С18,%TN%
CONT %С3%
;ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОШ=1 (НЕТ СБРОСА)
СС18:CJS А=BEGINT,МАР=РЕ
;УБИРАЕМ БЛОКИРОВКИ В РР: БП И БЗЗ: 07С5 Е000 - 0440 0000
CONT А=В27,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT
CONT А=В23,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ЧИТАЕМ РАЗНЫЕ CONST ДЛЯ М.А., Ф.А., РНП И Т.Д.: М.А.СТРАНИЦЫ - В WR1
CONT А=В13,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;АДРЕС ВНУТРИ СТРАНИЦЫ - В WR2
CONT А=МВ,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR2
;Ф.А.СТРАНИЦЫ - В WR5
CONT А=В15,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR5
;"СОБИРАЕМ" И ПИШЕМ, КОГО КУДА
;МАТ.АДРЕС - В АИСП., РАСЧЕТНЫЙ ФИЗ.АДРЕС - В РС, УБИРАЕМ БМСП И ОСТАНОВ
CONT ALUS=АВ,RА=WR1,RВ=WR2,FUNC=OR,Н,ALUD=Q,ALU,YDST=ADRREG
CONT ALUS=АВ,RА=WR5,RВ=WR2,FUNC=OR,Н,ALUD=Q,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZQ,Н,ALUD=В,RВ=РС
;Ф.А.+ N ПРОЦЕССА=0 + БЗО + БЗЗ - В РП
CONT А=В9,DSRC=PROM,ALUS=DА,RА=WR5,FUNC=OR,Н,ALUD=В,RВ=WR4
CONT А=В10,DSRC=PROM,ALUS=DА,RА=WR4,FUNC=OR,Н,ALU,YDEV=PSMEM,WRY
;В РР НЕТ БЛОКИРОВКИ, НО СТРАНИЦА В РП НЕ ЗАЩИЩЕНА, INT НЕ ДОЛЖНО БЫТЬ
CONT А=MNH,DSRC=PROM,ALUS=DZ,Н,CI,ALUD=В,RВ=DADR,ALU,ЕСВ,WRB,BRA=RG3
;ОШ=2,3 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕР.)
CJS А=SUBWD,МАР=РЕ,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,WRB
>,BRA=RG3
;ОШ=4,5,6,7 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕР.; НЕ ТЕ ДАННЫЕ; ТЕГ)
CJS А=SUBRD,МАР=РЕ
;ЗАЩИЩАЕМ СТРАНИЦУ В РП, НО УСТАНАВЛИВАЕМ БЗЗ В РР, INT НЕ Д,БЫТЬ
CONT А=В23,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
CONT А=В10,DSRC=PROM,ALUS=DА,RА=WR5,Н,ALU,YDEV=PSMEM,WRY
;ОШ=8,9 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕР.)
CJS А=SUBWD,МАР=РЕ,ALUS=ZВ,RВ=DADR,Н,CI,ALUD=В,ALU,ЕСВ,WRB,BRA=RG3
;ОШ=10,11,12,13 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕР.; НЕ ТЕ ДАННЫЕ; ТЕГ)
CJS А=SUBRD,МАР=РЕ
;УБИРАЕМ БЗЗ В РР ДЛЯ INT18
CONT А=В23,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,ALUD=В,RВ=MANT,ALU,YDST=CNT
;INT ПРИ ЗАПИСИ,ОШ=14,15,16,17 (НЕТ:"ГОТ.А.";ПР.ПРЕР.;СБРОСА; НЕ ТОТ НОМЕР)
CJS А=INTPSW,МАР=РЕ
CJP А=СС18,МАР=РЕ,%TIK3%
;
;
П19:ENTRY
;===== INT19 - "ОТРИЦАТЕЛЬНЫЙ НОМЕР СТРАНИЦЫ У КОМАНДЫ"
;
CJP А=CONT19,МАР=РЕ
CONT19:CONT А=С8,%TN%
CONT %С3%
;ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОШ=1 (НЕТ СБРОСА)
СС19:CJS А=BEGINT,МАР=РЕ
;УБИРАЕМ В РР БП И РОА: 07С5 Е000 - 0500 0000
CONT А=В27,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT
CONT А=В25,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ЧИТАЕМ РАЗНЫЕ CONST ДЛЯ М.А., Ф.А., РНП И Т.Д.: М.А.СТРАНИЦЫ - В WR1
CONT А=В13,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;АДРЕС ВНУТРИ СТРАНИЦЫ - В WR2
CONT А=МВ,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR2
;Ф.А.СТРАНИЦЫ - В WR5
CONT А=В15,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR5
;"СОБИРАЕМ" И ПИШЕМ, КОГО КУДА
;МАТ.АДРЕС - В АИСП., РАСЧЕТНЫЙ ФИЗ.АДРЕС - В РС, УБИРАЕМ БМСП И ОСТАНОВЫ
CONT ALUS=АВ,RА=WR1,RВ=WR2,FUNC=OR,Н,ALUD=Q,ALU,YDST=ADRREG
CONT ALUS=АВ,RА=WR5,RВ=WR2,FUNC=OR,Н,ALUD=Q,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZQ,Н,ALUD=В,RВ=РС
;Ф.А. - В РП, ОСТАЛЬНОЕ БЛОКИРОВАНО
CONT ALUS=ZА,RА=WR5,Н,ALU,YDEV=PSMEM,WRY
;В РР НЕТ РОА, НО СТРАНИЦА ПОЛОЖИТЕЛЬНАЯ, INT НЕ ДОЛЖНО БЫТЬ
CONT А=MNH,DSRC=PROM,ALUS=DZ,Н,CI,ALUD=В,RВ=DADR,ALU,ЕСВ,WRB,BRA=RG3
CONT А=С1,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,WRB
>,BRA=RG3
;ОШ=2,3 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕР.)
CJS А=SUBWD,МАР=РЕ
;ОШ=4,5,6,7 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕР.; НЕ ТЕ ДАННЫЕ; ТЕГ)
CJS А=SUBRC,МАР=РЕ,YDST=CCLR
;ПИШЕМ ОТРИЦАТЕЛЬНЫЙ МАТ. АДРЕС, НО УСТАНАВЛИВАЕМ РОА В РР, INT НЕ Д,БЫТЬ
CONT А=В25,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
CONT А=В14,DSRC=PROM,ALUS=DZ,FUNC=SUBR,CI,ALUD=В,RВ=WR1
CONT ALUS=АВ,RА=WR1,RВ=WR2,FUNC=OR,ALU,YDST=ADRREG
CONT ALUS=ZА,RА=WR5,ALU,YDEV=PSMEM,WRY
;ОШ=8,9 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕРЫВ.)
CJS А=SUBWD,МАР=РЕ,ALUS=ZВ,RВ=DADR,Н,CI,ALUD=В,ALU,ЕСВ,WRB,BRA=RG3
;ОШ=10,11,12,13 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕР.; НЕ ТЕ ДАННЫЕ; ТЕГ)
CJS А=SUBRC,МАР=РЕ,YDST=CCLR
;УБИРАЕМ РОА В РР ДЛЯ INT19
CONT А=В25,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,ALUD=В,RВ=MANT,ALU,YDST=CNT
;INT ПРИ ВКС, ОШ=14,15,16,17 (НЕТ:"ГОТ.А.";ПР.ПРЕР.;СБРОСА;НЕ ТОТ НОМЕР)
CJS А=INTPSF,МАР=РЕ,YDST=CCLR
CJP А=СС19,МАР=РЕ,%TIK3%
;
;
П20:ENTRY
;===== INT20 - "ОТРИЦАТЕЛЬНЫЙ НОМЕР СТРАНИЦЫ У ОПЕРАНДА"
;
CJP А=CONT20,МАР=РЕ
CONT20:CONT А=С9,%TN%
CONT %С3%
;ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОШ=1 (НЕТ СБРОСА)
СС20:CJS А=BEGINT,МАР=РЕ
;УБИРАЕМ В РР БП И РОА: 07С5 Е000 - 0500 0000
CONT А=В27,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT
CONT А=В25,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ЧИТАЕМ РАЗНЫЕ CONST ДЛЯ М.А., Ф.А., РНП И Т.Д.: М.А.СТРАНИЦЫ - В WR1
CONT А=В13,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;АДРЕС ВНУТРИ СТРАНИЦЫ - В WR2
CONT А=МВ,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR2
;Ф.А.СТРАНИЦЫ - В WR5
CONT А=В15,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR5
;"СОБИРАЕМ" И ПИШЕМ, КОГО КУДА
;МАТ.АДРЕС - В АИСП., РАСЧЕТНЫЙ ФИЗ.АДРЕС - В РС, УБИРАЕМ БМСП И ОСТАНОВЫ
CONT ALUS=АВ,RА=WR1,RВ=WR2,FUNC=OR,Н,ALUD=Q,ALU,YDST=ADRREG
CONT ALUS=АВ,RА=WR5,RВ=WR2,FUNC=OR,Н,ALUD=Q,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZQ,Н,ALUD=В,RВ=РС
;N СТРАНИЦЫ  - В РП, ОСТАЛЬНОЕ НЕ НАДО (БЛОКИРОВАНО)
CONT ALUS=ZА,RА=WR5,Н,ALU,YDEV=PSMEM,WRY
;В РР НЕТ РОА, НО СТРАНИЦА ПОЛОЖИТЕЛЬНАЯ, INT НЕ ДОЛЖНО БЫТЬ
CONT А=MNH,DSRC=PROM,ALUS=DZ,Н,CI,ALUD=В,RВ=DADR,ALU,ЕСВ,WRB,BRA=RG3
;ОШ=2,3 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕР.)
CJS А=SUBWD,МАР=РЕ,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,WRB
>,BRA=RG3
;ОШ=4,5,6,7 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕР.; НЕ ТЕ ДАННЫЕ; ТЕГ)
CJS А=SUBRD,МАР=РЕ
;ПИШЕМ ОТРИЦАТЕЛЬНЫЙ МАТ. АДРЕС, НО УСТАНАВЛИВАЕМ РОА В РР, INT НЕ Д,БЫТЬ
CONT А=В25,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
CONT А=В14,DSRC=PROM,ALUS=DZ,FUNC=SUBR,CI,ALUD=В,RВ=WR1
CONT ALUS=АВ,RА=WR1,RВ=WR2,FUNC=OR,ALU,YDST=ADRREG
CONT ALUS=ZА,RА=WR5,ALU,YDEV=PSMEM,WRY
;ОШ=8,9 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕР.)
CJS А=SUBWD,МАР=РЕ,ALUS=ZВ,RВ=DADR,Н,CI,ALUD=В,ALU,ЕСВ,WRB,BRA=RG3
;ОШ=10,11,12,13 (НЕ ТОТ Ф.А.; ЕСТЬ ПРЕР.; НЕ ТЕ ДАННЫЕ; ТЕГ)
CJS А=SUBRD,МАР=РЕ
;УБИРАЕМ РОА В РР ДЛЯ INT20
CONT А=В25,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,ALUD=В,RВ=MANT,ALU,YDST=CNT
;INT ПРИ ЗАПИСИ,ОШ=14,15,16,17 (НЕТ:"ГОТ.А.";ПР.ПРЕР.;СБРОСА; НЕ ТОТ НОМЕР)
CJS А=INTPSW,МАР=РЕ
;ПРИ ЧТЕНИИ ОПЕРАНДА, ОШ=18,19,20,21 (НЕТ:"ГОТ.А.";ПР.ПРЕР.;СБРОСА; НЕ ТОТ N)
CJS А=INTPSR,МАР=РЕ
CJP А=СС20,МАР=РЕ,%TIK3%
;
;
П22:ENTRY
;===== INT22 - "ОТСУТСТВУЮЩИЙ АДРЕС ПАМЯТИ В НОВОМ РЕЖИМЕ"; З.А.БЛОКИРУЕТСЯ
;
CJP А=CONT22,МАР=РЕ
CONT22:CONT А=С7,%TN%
CONT %С3%
;ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОШ=1 (НЕТ СБРОСА)
СС22:CJS А=BEGINT,МАР=РЕ
;БОЛЬШОЙ АДРЕС - В АИСП, РС, УБИРАЕМ БМСП И ОСТАНОВЫ (БП=1, М.А.= Ф.А.)
CONT А=В22,DSRC=PROM,ALUS=DZ,Н,CI,ALUD=В,RВ=РС,ALU,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZА,RА=РС,ALU,YDST=ADRREG
;INT ПРИ ЗАПИСИ,ОШ=2,3,4,5 (НЕТ:"ГОТ.А.";ПР.ПРЕР.;СБРОСА;НЕ ТОТ N)
CJS А=INTPSW,МАР=РЕ
;ПРИ ЧТЕНИИ ОПЕРАНДА, ОШ=6,7,8,9 (НЕТ:"ГОТ.А";ПР.ПРЕР.;СБРОСА;НЕ ТОТ N)
CJS А=INTPSR,МАР=РЕ,FFCNT=CLRINT
;И ПРИ ВКС, ОШ=10,11,12,13 (НЕТ:"ГОТ.А.";ПР.ПРЕР.;СБРОСА; НЕ ТОТ НОМЕР)
CJS А=INTPSF,МАР=РЕ,FFCNT=CLRINT
;БОЛЬШОЙ ОТРИЦАТЕЛЬНЫЙ АДРЕС - В АИСП, РС, УБИРАЕМ БМСП И ОСТАНОВЫ
CONT А=NTS,DSRC=PROM,ALUS=DZ,Н,CI,ALUD=В,RВ=РС,ALU,YDST=PHYSPG,FFCNT=CLRINT
CJS А=СН,МАР=РЕ,ALUS=ZА,RА=РС,ALU,YDST=ADRREG
;INT ПРИ ЗАПИСИ, ОШ=14,15,16,17 (НЕТ:"ГОТ.А."; ПР.ПРЕР.;СБРОСА; НЕ ТОТ N)
CJS А=INTPSW,МАР=РЕ
;ПРИ ЧТЕНИИ ОПЕРАНДА, ОШ=18,19,20,21 (НЕТ:"ГОТ.А"; ПР.ПРЕР.;СБРОСА;НЕ ТОТ N)
CJS А=INTPSR,МАР=РЕ,FFCNT=CLRINT
;И ПРИ ВКС, ОШ=22,23,24,25 (НЕТ:"ГОТ.А."; ПР.ПРЕР.; СБРОСА; НЕ ТОТ НОМЕР)
CJS А=INTPSF,МАР=РЕ,FFCNT=CLRINT
;УСТАНАВЛИВАЕМ БП = 0
CONT А=В27,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;БОЛЬШОЙ АДРЕС - В АИСП, РС, ФИЗ.АДРЕС - В РФС,РП, УБИРАЕМ БМСП И ОСТАНОВЫ
CONT А=В17,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=РССОРУ,ALU,YDST=PHYSPG,FFCNT=CLRINT
CONT А=В22,DSRC=PROM,ALUS=DZ,Н,CI,ALUD=В,RВ=РС,ALU,YDST=ADRREG
CJS А=СН,МАР=РЕ,ALUS=ZА,RА=РССОРУ,ALU,YDEV=PSMEM,WRY
;INT ПРИ ЗАПИСИ, ОШ=26,27,28,29 (НЕТ "ГОТ.А."; ПР.ПРЕР.; СБРОСА; НЕ ТОТ N)
CJS А=INTPSW,МАР=РЕ
;ПРИ ЧТЕНИИ ОПЕРАНДА, ОШ=30,31,32,33 (НЕТ:"ГОТ.А.";ПР.ПРЕР.;СБРОСА;НЕ ТОТ N)
CJS А=INTPSR,МАР=РЕ,FFCNT=CLRINT
;И ПРИ ВКС, ОШ=34,35,36,37 (НЕТ: "ГОТ.А."; ПР.ПРЕР.; СБРОСА; НЕ ТОТ НОМЕР)
CJS А=INTPSF,МАР=РЕ,FFCNT=CLRINT
;БОЛЬШОЙ ОТРИЦАТЕЛЬНЫЙ МАТ. АДРЕС - В АИСП, РС, ТОТ ЖЕ ФИЗ.АДРЕС - В РП
CONT А=NTS,DSRC=PROM,ALUS=DZ,Н,CI,ALUD=В,RВ=РС,ALU,YDST=ADRREG,FFCNT=CLRINT
;INT ПРИ ЗАПИСИ, ОШ=38,39,40,41 (НЕТ:"ГОТ.А";ПР.ПРЕР.;СБРОСА; НЕ ТОТ N)
CJS А=INTPSW,МАР=РЕ,ALUS=ZА,RА=РССОРУ,ALU,YDEV=PSMEM,WRY
;ПРИ ЧТЕНИИ ОПЕРАНДА, ОШ=42,43,44,45 (НЕТ:"ГОТ.А.";ПР.ПРЕР.;СБРОСА; НЕ ТОТ N)
CJS А=INTPSR,МАР=РЕ,FFCNT=CLRINT
;И ПРИ ВКС, ОШ=46,47,48,49 (НЕТ:"ГОТ.А.";ПР.ПРЕР.;СБРОСА; НЕ ТОТ НОМЕР)
CJS А=INTPSF,МАР=РЕ,FFCNT=CLRINT
CJP А=СС22,МАР=РЕ,%TIK3%
;
;
П24:ENTRY
;===== INT24 - "ЗАПРОС МОДИФИКАЦИИ ПРИОРИТЕТОВ СТРАНИЦ"
;   З.А. НЕ БЛОКИРУЕТСЯ; ПРОЧИТАННЫЕ ДАННЫЕ И ПРЕРЫВАНИЕ С МАСКОЙ НЕ ПРОВЕРЯЕМ,
;   Т.К.НАДО ПЕРЕУСТАНАВЛИВАТЬ БМСП
;
CJP А=CONT24,МАР=РЕ
CONT24:CONT А=С24,%TN%
CONT %С3%
;ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОШ=1 (НЕТ СБРОСА)
СС24:CJS А=BEGINT,МАР=РЕ
;ЧИТАЕМ ДАННЫЕ, ТЕГ; БП=1; М.А.= Ф.А.
CONT А=СААА,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=DADR,ALU,ЕСВ,WRB,BRA=RG3
CONT А=ZER,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,WRB,BRA=RG3
;АДРЕС - В АИСП, РС, УСТАНАВЛИВАЕМ БМСП = 1, УБИРАЕМ ОСТАНОВЫ
CONT А=В13,DSRC=PROM,ALUS=DZ,Н,CI,ALUD=В,RВ=РС,ALU,YDST=PHYSPG
CJS А=СН,МАР=РЕ
CONT А=С1,DSRC=PROM,DDEV=MODB,WRD,ALUS=ZА,RА=РС,ALU,YDST=ADRREG
;INT ПРИ ЗАПИСИ ОПЕРАНДА, ОШ=2 (НЕ ТОТ НОМЕР)
CJS А=INTMWN,МАР=РЕ,FFCNT=SETEI
;ПЕРЕУСТАНАВЛИВАЕМ БИТ БМСП (ОН СБРАСЫВАЕТСЯ АППАРАТНО!)
CONT А=С1,DSRC=PROM,DDEV=MODB,WRD,FFCNT=CLRINT
;ЧТЕНИЕ ОПЕР.:ОШ=3 (НЕ ТОТ НОМЕР)
CJS А=INTMRN,МАР=РЕ,FFCNT=SETEI
CONT А=С1,DSRC=PROM,DDEV=MODB,WRD,FFCNT=CLRINT
;И ПРИ ВКС (СНАЧАЛА - ЗАПИСЬ С СООТВ.ТЕГОМ)
;ОШ=4 (НЕ ТОТ НОМЕР)
CJS А=INTMWN,МАР=РЕ,ALUS=ZВ,RВ=HALF,Н,CI,ALUD=В,ALU,YDEVT=ECBTAG,WRB,BRA=RG3
>,FFCNT=SETEI
CONT А=С1,DSRC=PROM,DDEV=MODB,WRD,FFCNT=CLRINT
;ОШ=5 (НЕ ТОТ НОМЕР)
CJS А=INTMFN,МАР=РЕ,FFCNT=SETEI,YDST=CCLR
;УСТАНАВЛИВАЕМ БП = 0
CONT А=В27,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ОТРИЦАТЕЛЬНЫЙ МАТ.АДРЕС - В АИСП, РС, УСТАНАВЛИВАЕМ БМСП, УБИРАЕМ ОСТАНОВЫ
CONT А=С64,DSRC=PROM,ALUS=DZ,FUNC=EXNOR,Н,ALUD=В,RВ=РС,ALU,YDST=ADRREG
;РФС НЕ ИСПОРЧЕН, Ф.А. - ТОТ ЖЕ, ОСТАНОВЫ УБИРАТЬ НЕ НАДО; ПИШЕМ ТОЛЬКО Ф.А.В РП
CONT А=В13,DSRC=PROM,ALUS=DZ,Н,CI,ALU,YDEV=PSMEM,WRY
CONT А=С1,DSRC=PROM,DDEV=MODB,WRD,FFCNT=CLRINT
;ПРОВЕРЯЕМ INT ПРИ ЗАПИСИ, ЧТЕНИИ ОПЕРАНДА И КОМАНДЫ
;ОШ=6 (НЕ ТОТ НОМЕР)
CJS А=INTMWN,МАР=РЕ,FFCNT=SETEI
CONT А=С1,DSRC=PROM,DDEV=MODB,WRD,FFCNT=CLRINT
;ОШ=7 (НЕ ТОТ НОМЕР)
CJS А=INTMFN,МАР=РЕ,FFCNT=SETEI,YDST=CCLR
CONT А=С1,DSRC=PROM,DDEV=MODB,WRD,FFCNT=CLRINT
;ОШ=8 (НЕ ТОТ НОМЕР)
CJS А=INTMWN,МАР=РЕ,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,WRB
>,BRA=RG0,FFCNT=SETEI
CONT А=С1,DSRC=PROM,DDEV=MODB,WRD,FFCNT=CLRINT
;ОШ=9 (НЕ ТОТ НОМЕР)
CJS А=INTMRN,МАР=РЕ,FFCNT=SETEI
CJP А=СС24,МАР=РЕ,%TIK3%
;
;
П26:ENTRY
;===== INT26 - ПРЕРЫВАНИЯ ОТ ТАЙМЕРА СЧЕТНОГО ВРЕМЕНИ
;   ИСПОЛЬЗУЕМ СТАНДАРТНУЮ СХЕМУ ПРОВЕРКИ ПРЕРЫВАНИЯ И СТАНДАРТНЫЕ П/П
;   2-Й РАЗ В ТАЙМЕР ПИШЕМ С7 = 7 = 140 МИКРОКОМАНД = 96 Н = WR2 ПРИ ПРЕРЫВАНИИ
;
CJP А=CONT26,МАР=РЕ
CONT26:CONT А=С32,%TN%
CONT %С3%
;ПРОВЕРЯЕМ НАЧАЛЬНЫЙ СБРОС ПРЕРЫВАНИЙ, ОШ=1, ЕСЛИ ЕСТЬ INT
СС26:CJS А=BEGINT,МАР=РЕ
;ПИШЕМ В ТАЙМЕР НЕБОЛЬШОЕ ЧИСЛО (В5 = 16 = 320 МИКРОКОМАНД)
CONT А=В5,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=А
CJS А=CTTWR,МАР=РЕ
;ЖДЕМ ПРИЗНАКА НАЛИЧИЯ ПРЕРЫВАНИЙ В 2 РАЗА ДОЛЬШЕ
PUSH А=700,МАР=РЕ,%ТУРЕ%
CJPP А=ОК26,МАР=РЕ,COND=INT
RFCT
;ОШИБКА, ЕСЛИ ВСЕ ЕЩЕ НЕТ ПРЕРЫВАНИЯ, ОШ=2
CJS А=ERRINT,МАР=РЕ,COND=INT,IСС
;СБРАСЫВАЕМ КОНКРЕТНОЕ ПРЕРЫВАНИЕ: INT26
ОК26:PUSH А=2,МАР=РЕ,FFCNT=CLRCT
;ПОДОЖДЕМ ЧУТЬ-ЧУТЬ
RFCT %TIK3%
;ЧТО-ТО НЕ ТО, ЕСЛИ ВСЕ-ТАКИ ЕСТЬ ПРЕРЫВАНИЯ, ОШ=3
CJS А=ERRINT,МАР=РЕ,COND=INT,%ТУРЕ%
;ОТКРЫВАЕМ МАСКУ ПРЕРЫВАНИЙ, ПРОЧИЩАЕМ РГ.WR1 И 4,5 РГ.БОПП
CONT ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR1,ALU,YDEV=МРМЕМ,IОМР,WRY,MPADR=4
CONT ALUS=ZА,RА=WR1,ALU,YDEV=МРМЕМ,IОМР,WRY,MPADR=5
CONT FFCNT=SETEI
;СНОВА ПИШЕМ В ТАЙМЕР
CONT А=С7,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=А
CJS А=CTTWR,МАР=РЕ,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR2
;ДЕЛАЕМ ЗАДЕРЖКУ, СЧИТАЕМ К-ВО МИКРОКОМАНД (WR2)
PUSH А=256,МАР=РЕ,ALUS=ZВ,RВ=WR2,Н,CI,ALUD=В
RFCT ALUS=ZВ,RВ=WR2,Н,CI,ALUD=В,ISE
CJS А=INTWAT,МАР=РЕ,%TIK3%
CJP А=СС26,МАР=РЕ,%TIK3%
;
;
П27:ENTRY
;===== INT27 - "TIME-OUT ПРИ БЛОКИРОВКЕ ВНЕШНИХ ПРЕРЫВАНИЙ"
;   ПРЕРЫВАНИЕ ПРОИСХОДИТ, СПУСТЯ 40 (!?) МИКРОСЕКУНД ПОСЛЕ УСТАНОВКИ БВП
;   ЭТО ОЧЕНЬ МАЛО!!!!!
;
CJP А=CONT27,МАР=РЕ
CONT27:CONT А=С26,%TN%
CONT %С3%
;ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОШ=1 (НЕТ СБРОСА)
СС27:CJS А=BEGINT,МАР=РЕ
;УСТАНАВЛИВАЕМ БВП: 07С5 Е000 + 0002 0000
CONT А=В18,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=OR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ЖДЕМ ДОЛГО-ДОЛГО, ПОКА НЕ ПОЯВИТСЯ ПРИЗНАК НАЛИЧИЯ ПРЕРЫВАНИЙ
CONT ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR5
CWAIT:CONT А=В24,DSRC=PROM,ALUS=DА,RА=WR5,FUNC=EXOR,Н,CEN,ISE
CJP А=OKINT,МАР=РЕ,COND=INT,ISE,ALUS=ZВ,RВ=WR5,Н,CI,ALUD=В,SТОРС=21
CJP А=CWAIT,МАР=РЕ,COND=СТ,SТОРС=21
;П/У НА АНАЛИЗ, ОШ=2,3 (НЕТ ПР.ПРЕР.; НЕТ СБРОСА)
OKINT:CJS А=INTMAS,МАР=РЕ,ISE
;УБИРАЕМ, ЗАТЕМ СНОВА ВОССТАНАВЛИВАЕМ БВП
CONT А=В18,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
CONT А=В18,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ЖДЕМ, КОГДА В WR1 ПОЯВИТСЯ N ПРЕРЫВАНИЯ
WТ27:CONT ALUS=ZА,RА=WR1,CEN
CONT SТОРС=20,ISE
CJP А=WТ27,МАР=РЕ,COND=СТ,SТОРС=20
;ПРОВЕРЯЕМ НОМЕР ПРЕРЫВАНИЯ
CONT ALUS=АВ,RА=WR1,RВ=EXPN,FUNC=EXOR,CEN
CONT SТОРС=21
;ОШ=4 (НЕ ТОТ НОМЕР)
CJS А=ERRINT,МАР=РЕ,COND=СТ,SТОРС=21,%ТУРЕ%
CJP А=СС27,МАР=РЕ,%TIK3%
;
;
П28:ENTRY
;===== INT28  - "ОБРАЩЕНИЕ ПП НА ЧТ/ЗП РЕГИСТРОВ"
;   ДЛЯ ОБРАЩЕНИЯ ПП НА ЧТ/ЗП РЕГИСТРОВ ИЗ INTEL8080 - АДРЕС ПОРТА = DF,
;   ПРОГРАММА ДЛЯ INTEL, ВЫЗЫВАЮЩАЯ INT28:  D3 F7 CF
;   ВНИМАНИЕ! Т.К. 3 РГ.БОПП ЗАНЯТ В ОБМЕНАХ С 8080,
;   ПОД ПРИЗНАК, ЧТО ТЕСТ "ТИКАЕТ", БУДЕМ ИСПОЛЬЗОВАТЬ 6 РГ.БОПП
;
CJP А=CONT28,МАР=РЕ
CONT28:CONT А=С30,%TN%
CONT А=CTEST,DSRC=PROM,ALUS=DZ,ALU,YDST=CNT
CONT ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR3,ALU,YDEV=МРМЕМ,WRY,IОМР,MPADR=6
;ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОШ=1 (НЕТ СБРОСА)
СС28:CONT FFCNT=CLRINT
CONT ALUS=DZ,FUNC=AND,ALU,YDEV=МРМЕМ,WRY,IОМР,MPADR=2
CONT А=С1,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=INTR,ALU,YDEV=МРМЕМ,WRY,IОМР,MPADR=1
CJS А=ERRINT,МАР=РЕ,COND=INT
;ЖДЕМ ПРИЗНАКА НАЛИЧИЯ ПРЕРЫВАНИЙ (ОТ ВНЕШНИХ ПРИЧИН, ОТ МП НЕ ЗАВИСЯЩИХ)
WА28:CJP А=WА28,МАР=РЕ,COND=INT,IСС,ISE
;ОШ=2,3 (НЕТ ПР.ПРЕР.; НЕТ СБРОСА)
CONT YDEV=МРМЕМ,IОМР,MPADR=3,DSRC=SHIFT,MPS,PSHF=SН0-56,SHF=LOG,ALUS=DZ,Н,CEN
CJP А=RR28,МАР=РЕ,COND=СТ,SТОРС=/NZ
;INT28 НА ЗАПИСЬ РГ: УСТАНАВЛИВАЕМ ПРИЗНАК, ЧТО "ОЗУ ПП-ЦП ПРОЧИТАНО"
CONT А=В2,DSRC=PROM,ALUS=DZ,ALU,YDEV=МРМЕМ,WRY,IОМР,MPADR=3
CJP А=MAS28,МАР=РЕ
;INT28 НА ЧТЕНИЕ РГ.: УСТАНАВЛИВАЕМ ПРИЗНАК, ЧТО "В ОЗУ ПП-ЦП ЕСТЬ ИНФОРМАЦИЯ"
RR28:CONT А=В1,DSRC=PROM,ALUS=DZ,ALU,YDEV=МРМЕМ,WRY,IОМР,MPADR=3
MAS28:CJS А=INTMAS,МАР=РЕ,ISE
;ЖДЕМ, КОГДА В WR1 ПОЯВИТСЯ N ПРЕРЫВАНИЯ
WТ28:CONT ALUS=ZА,RА=WR1,CEN
CONT SТОРС=20,ISE
CJP А=WТ28,МАР=РЕ,COND=СТ,SТОРС=20
CONT YDEV=МРМЕМ,IОМР,MPADR=3,DSRC=SHIFT,MPS,PSHF=SН0-56,SHF=LOG,ALUS=DZ,Н,CEN
;ЗАПИСЬ В РГ.ИЛИ ЧТЕНИЕ?
CJP А=RRI28,МАР=РЕ,COND=СТ,SТОРС=/NZ
CONT А=В2,DSRC=PROM,ALUS=DZ,ALU,YDEV=МРМЕМ,WRY,IОМР,MPADR=3
CJP А=NМВ28,МАР=РЕ
RRI28:CONT А=В1,DSRC=PROM,ALUS=DZ,ALU,YDEV=МРМЕМ,WRY,IОМР,MPADR=3
;ПРОВЕРЯЕМ НОМЕР ПРЕРЫВАНИЯ
NМВ28:CONT ALUS=АВ,RА=WR1,RВ=EXPN,FUNC=EXOR,Н,ALUD=Q,CEN
CONT SТОРС=21
;ОШ=4 (НЕ ТОТ ВЕКТОР)
CJS А=ERRINT,МАР=РЕ,COND=СТ,SТОРС=21,%ТУРЕ%
CJP А=СС28,МАР=РЕ,ALUS=ZВ,RВ=WR3,Н,CI,ALUD=В,ALU,YDEV=МРМЕМ,WRY,IОМР,MPADR=6
;
;
П29:ENTRY
;===== INT29 - "ШАГОВОЕ ПРЕРЫВАНИЕ"
;
CJP А=CONT29,МАР=РЕ
CONT29:CONT А=С29,%TN%
CONT %С3%
;ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОШ=1 (НЕТ СБРОСА)
СС29:CJS А=BEGINT,МАР=РЕ
;УСТАНАВЛИВАЕМ В РР РШ: 07С5 Е000 + 0010 0000
CONT А=В21,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=OR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ПМИ + П/У НА АНАЛИЗ, ОШ=2,3 (НЕТ ПР.ПРЕР.; НЕТ СБРОСА)
CJS А=INTMAS,МАР=РЕ,ISE
;ОШ=4 (НЕ ТОТ ВЕКТОР)
CJS А=INTWAT,МАР=РЕ,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR2
CJP А=СС29,МАР=РЕ,%TIK3%
;
;
П30:ENTRY
;===== INT30 - "ВНЕШНИЕ ПРЕРЫВАНИЯ"
;   ОБРАБОТКА: ЧИТАЕМ ПРЕРЫВАНИЯ С ШИНЫ, СБРАСЫВАЕМ (ПИШЕМ ТО ЖЕ ОБРАТНО)
;   ПИШЕМ, ЧТО ПРОЧИТАЛИ С ШИНЫ, В 8-15 РГ.БОПП
;
;   ВОПРОС: НАДО ЛИ СБРАСЫВАТЬ ЕЩЕ ЧЕРЕЗ CLRINT И В КАКОЙ ПОСЛЕДОВАТЕЛЬНОСТИ????
;
CJP А=CONT30,МАР=РЕ
CONT30:CONT А=С27,%TN%
CONT %С3%
;ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОШ=1 (НЕТ СБРОСА)
СС30:CJS А=BEGINT,МАР=РЕ
;ЖДЕМ ПРИЗНАКА НАЛИЧИЯ ПРЕРЫВАНИЙ (ОТ ВНЕШНИХ ПРИЧИН, ОТ МП НЕ ЗАВИСЯЩИХ)
WА30:CJP А=WА30,МАР=РЕ,COND=INT,IСС,ISE
;ЧИТАЕМ ПРЕРЫВАНИЯ С ШИНЫ В РГ.У
CONT YDEV=PHYSAD,ARBI=BIRD
WR30:CJP А=WR30,%WА%
CONT ЕСА,ARA=RG2,ALUS=DZ,Н,ALUD=В,RВ=У,ALU,ЕСВ,WRB,BRA=RG3
;ПЕРЕПИСЫВАЕМ, ЧТО ПРОЧИТАЛИ С ШИНЫ, В БОПП; СБРАСЫВАЕМ ПРЕРЫВАНИЯ НА ШИНЕ
CJS А=OUT8/F,МАР=РЕ,ALUS=ZА,RА=У,Н,ALUD=В,RВ=WR6,YDEV=PHYSAD,ARBI=BICLR
WС30:CJP А=WС30,%WА%
;ОШ=2,3 (НЕТ ПР.ПРЕР.; НЕТ СБРОСА)
CJS А=INTMAS+1,МАР=РЕ,%ТУРЕ%
;ЖДЕМ, КОГДА В WR1 ПОЯВИТСЯ N ПРЕРЫВАНИЯ
WI30:CONT ALUS=ZА,RА=WR1,CEN,ISE
CJP А=WI30,МАР=РЕ,COND=СТ,SТОРС=20
;ПРОВЕРЯЕМ НОМЕР ПРЕРЫВАНИЯ
CONT ALUS=АВ,RА=WR1,RВ=EXPN,FUNC=EXOR,CEN
;ОШ=4 (НЕ ТОТ ВЕКТОР)
CJS А=ERRINT,МАР=РЕ,COND=СТ,SТОРС=21,%ТУРЕ%
;ЧИТАЕМ ПРЕРЫВАНИЯ С ШИНЫ
CONT YDEV=PHYSAD,ARBI=BIRD
WRI30:CJP А=WRI30,%WА%
CONT ЕСА,ARA=RG2,ALUS=DZ,Н,ALUD=В,RВ=У,ALU,ЕСВ,WRB,BRA=RG3
;СБРАСЫВАЕМ ПРЕРЫВАНИЯ НА ШИНЕ
CJS А=OUT8/F,МАР=РЕ,ALUS=ZА,RА=У,Н,ALUD=В,RВ=WR6,YDEV=PHYSAD,ARBI=BICLR
WCI30:CJP А=WCI30,%WА%
CJP А=СС30,МАР=РЕ,%TIK3%
;
;
П31:ENTRY
;===== INT31 - "ПРОГРАММНОЕ ПРЕРЫВАНИЕ"
;
CJP А=CONT31,МАР=РЕ
CONT31:CONT А=С5,%TN%
CONT %С3%
;ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОШ=1 (НЕТ СБРОСА)
СС31:CJS А=BEGINT,МАР=РЕ
;УСТАНАВЛИВАЕМ В РР БВП: 07С5 Е000 + 0002 0000
CONT А=В18,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=OR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;УСТАНАВЛИВАЕМ ПРОГРАММНОЕ ПРЕРЫВАНИЕ, БВП=1, INT НЕ Д.БЫТЬ
;ЖДЕМ ЧУТЬ-ЧУТЬ
PUSH А=10,МАР=РЕ,FFCNT=PRGINT
RFCT ISE
;П/У НА ОШИБКУ, ЕСЛИ ЕСТЬ ПРИЗНАК НАЛИЧИЯ ПРЕРЫВАНИЙ: ОШ=2
CJS А=ERRINT,МАР=РЕ,COND=INT,%ТУРЕ%
;УБИРАЕМ БВП, ИДЕМ НА АНАЛИЗ
CONT А=В18,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ОШ=3,4 (НЕТ ПР.ПРЕР.; НЕТ СБРОСА)
CJS А=INTMAS,МАР=РЕ,ISE
;ВОССТАНАВЛИВАЕМ БВП, ПОСЫЛАЕМ ПРОГР.ПРЕРЫВАНИЕ
CONT А=В18,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
CONT FFCNT=PRGINT
;П/У НА ERR, ЕСЛИ ПРИ БВП=1 ЕСТЬ ПРИЗНАК ПРЕРЫВАНИЯ, ОШ=5
CJS А=ERRINT,МАР=РЕ,COND=INT,ISE,%ТУРЕ%
;УБИРАЕМ БВП
CONT А=В18,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ОШ=6 (НЕ ТОТ НОМЕР)
CJS А=INTWAT,МАР=РЕ,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR2
CJP А=СС31,МАР=РЕ,%TIK3%
;
;
П32:ENTRY
;===== INT32 - "HALT"
;   ПРИ ПОЯВЛЕНИИ ПРЕРЫВАНИЯ HALT УСТАНАВЛИВАЕТСЯ ТГ. HALT В СТАТУСНОМ РГ.,
;   СБРАСЫВАЕТСЯ ОН ТОЛЬКО ИЗ ПП!
;   ПОЭТОМУ ПОСЛЕ КАЖДОГО СЛУЧАЯ, ВЫЗЫВАЮЩЕГО INT32, ЖДЕМ СБРОСА ЕГО ИЗ ПП.
; - СБРОС ТГ.HALT ИЗ 8080 ДЕЛАЕТСЯ ПРОСТОЙ ПРОГРАММОЙ ЗАПИСИ В СТАТУСНЫЙ РЕГИСТР
;   3Е 05 D3 F7 CF
; - ПРИ ПРОВЕРКЕ ПОП=1 + ЛЮБОЕ ДРУГОЕ ПРЕРЫВАНИЕ (ЗДЕСЬ - ПРОГРАММНОЕ)
;   ЭТО ДОПОЛНИТЕЛЬНОЕ ПРЕРЫВАНИЕ НАДО СБРАСЫВАТЬ ОТДЕЛЬНО!
;   СБРОСОМ ТГ.HALT ОНО НЕ СБРАСЫВАЕТСЯ!
;
CJP А=CONT32,МАР=РЕ
CONT32:CONT А=С28,%TN%
CONT %С3%
;ПРОВЕРЯЕМ СБРОС ПРЕРЫВАНИЙ, ОШ=1 (НЕТ СБРОСА)
СС32:CJS А=BEGINT,МАР=РЕ
;
;ПРОВЕРЯЕМ "ОСТАНОВ", УСТАНОВЛЕННЫЙ В ОЗУ ОСТАНОВА, БП=1; ЧИТАЕМ ДАННЫЕ, ТЕГ
CONT А=СААА,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=DADR,ALU,ЕСВ,WRB,BRA=RG3
CONT А=ZER,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,WRB,BRA=RG3
;АДРЕС - В АИСП, РС, УБИРАЕМ БМСП И ВСЕ ОСТАНОВЫ, КРОМЕ ОДНОГО
CONT А=В13,DSRC=PROM,ALUS=DZ,Н,CI,ALUD=В,RВ=РС,ALU,YDST=PHYSPG
CJS А=СН,МАР=РЕ,ALUS=ZА,RА=РС,ALU,YDST=ADRREG
CONT А=С10,DSRC=PROM,ALUS=DZ,ALU,YDST=ARBOPC
CONT А=ZER,DSRC=PROM,ALUS=DZ,ALU,YDEV=SТОРМ0,WRY
;
;INT ПРИ ЗАПИСИ ОПЕРАНДА, ОШ=2,3,4 (НЕТ ПР.ПРЕР.; НЕТ СБРОСА;НЕ ТОТ НОМЕР)
;Т.К.СБРОС НЕ ИЗ МП, А ИЗ ПП, П/П ЗАПИСИ ЦЕЛИКОМ ИСПОЛЬЗОВАТЬ НЕ МОЖЕМ!
;ПРОВЕРЯЕМ В П/П INT С МАСКОЙ:
CONT YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR
WA32W1:CJP А=WA32W1,МАР=РЕ,COND=ARBRDY,IСС
CJS А=INTMAS,МАР=РЕ,ISE,%TIK3%
;СНОВА УСТАНАВЛИВАЕМ МАСКУ,ЖДЕМ СБРОСА INT ОТ ПП; В БОПП - "ЗАПРОС" = ОШ = INTR
CONT ALUS=ZА,RА=INTR,ALU,YDEV=МРМЕМ,IОМР,WRY,MPADR=2
CONT FFCNT=CLREI,ISE
CLRW11:CJP А=CLRW11,МАР=РЕ,COND=INT,ISE
;ОТКРЫВАЕМ МАСКУ, ПРОВЕРЯЕМ В П/П НОМЕР ПРЕРЫВАНИЯ:
CJS А=WTNI,МАР=РЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR,ALUS=DZ,FUNC=AND,Н
>,ALUD=В,RВ=WR2,FFCNT=SETEI
;УСТАНАВЛИВАЕМ МАСКУ, ЖДЕМ СБРОСА INT ОТ ПП:
CONT ALUS=ZА,RА=INTR,ALU,YDEV=МРМЕМ,IОМР,WRY,MPADR=2
CONT FFCNT=CLREI,ISE
CLRW12:CJP А=CLRW12,МАР=РЕ,COND=INT,ISE,%TIK3%
;
;ПРИ ЧТЕНИИ ОПЕРАНДА
CONT А=С9,DSRC=PROM,ALUS=DZ,ALU,YDST=ARBOPC
CONT А=ZER,DSRC=PROM,ALUS=DZ,ALU,YDEV=SТОРМ0,WRY
;ОШ=5,6,7,8,9 (НЕТ ПР.ПРЕР.;НЕТ СБРОСА;НЕ ТЕ ДАННЫЕ;ТЕГ;НЕ ТОТ НОМЕР)
;ПРОВЕРЯЕМ В П/П INT С МАСКОЙ:
CONT YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DRD
WA32W2:CJP А=WA32W2,МАР=РЕ,COND=ARBRDY,IСС
CJS А=INTMAS,МАР=РЕ,ISE
;ПРОВЕРИМ, ЧТО ПРОЧИТАЛИ
CJS А=CMPDAT,МАР=РЕ
;СНОВА УСТАНАВЛИВАЕМ МАСКУ,ЖДЕМ СБРОСА INT ОТ ПП,В БОПП - ТЕКУЩЕЕ ЗНАЧЕНИЕ INTR
CONT ALUS=ZА,RА=INTR,ALU,YDEV=МРМЕМ,IОМР,WRY,MPADR=2
CONT FFCNT=CLREI,ISE
CLRW21:CJP А=CLRW21,МАР=РЕ,COND=INT,ISE
;ОТКРЫВАЕМ МАСКУ, ПРОВЕРЯЕМ В П/П НОМЕР ПРЕРЫВАНИЯ:
CJS А=WTNI,МАР=РЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DRD,ALUS=DZ,FUNC=AND,Н
>,ALUD=В,RВ=WR2,FFCNT=SETEI
;УСТАНАВЛИВАЕМ МАСКУ, ЖДЕМ СБРОСА INT ОТ ПП:
CONT ALUS=ZА,RА=INTR,ALU,YDEV=МРМЕМ,IОМР,WRY,MPADR=2
CONT FFCNT=CLREI,ISE
CLRW22:CJP А=CLRW22,МАР=РЕ,COND=INT,ISE,%TIK3%
;
;И ПРИ ВКС (СНАЧАЛА - ЗАПИСЬ С СООТВ.ТЕГОМ)
CONT А=С8,DSRC=PROM,ALUS=DZ,ALU,YDST=ARBOPC
CONT А=ZER,DSRC=PROM,ALUS=DZ,ALU,YDEV=SТОРМ1,WRY
;ОШ=10,11,12 (НЕТ ПР.ПРЕР.; НЕТ СБРОСА; НЕ ТОТ НОМЕР)
CONT ALUS=ZВ,RВ=HALF,Н,CI,ALUD=В,ALU,YDEVT=ECBTAG,WRB,BRA=RG3
CONT YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR
WA32W3:CJP А=WA32W3,МАР=РЕ,COND=ARBRDY,IСС
CJS А=INTMAS,МАР=РЕ,ISE
;СНОВА УСТАНАВЛИВАЕМ МАСКУ,ЖДЕМ СБРОСА INT ОТ ПП,В БОПП - ТЕКУЩЕЕ ЗНАЧЕНИЕ INTR
CONT ALUS=ZА,RА=INTR,ALU,YDEV=МРМЕМ,IОМР,WRY,MPADR=2
CONT FFCNT=CLREI,ISE
CLRW31:CJP А=CLRW31,МАР=РЕ,COND=INT,ISE
;ОТКРЫВАЕМ МАСКУ, ПРОВЕРЯЕМ В П/П НОМЕР ПРЕРЫВАНИЯ:
CJS А=WTNI,МАР=РЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR,ALUS=DZ,FUNC=AND,Н
>,ALUD=В,RВ=WR2,FFCNT=SETEI
;УСТАНАВЛИВАЕМ МАСКУ, ЖДЕМ СБРОСА INT ОТ ПП:
CONT ALUS=ZА,RА=INTR,ALU,YDEV=МРМЕМ,IОМР,WRY,MPADR=2
CONT FFCNT=CLREI,ISE,YDST=CCLR
CLRW32:CJP А=CLRW32,МАР=РЕ,COND=INT,ISE,%TIK3%
;
;ВКС: ОШ=13,14,15,16,17 (НЕТ ПР.ПРЕР.;НЕТ СБРОСА;НЕ ТА КОМАНДА;ТЕГ;НЕ ТОТ N)
CONT YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=FЕТСН
WA32W4:CJP А=WA32W4,МАР=РЕ,COND=ARBRDY,IСС
CJS А=INTMAS,МАР=РЕ,ISE
CJS А=СМРСОМ,МАР=РЕ
;СНОВА УСТАНАВЛИВАЕМ МАСКУ,ЖДЕМ СБРОСА INT ОТ ПП,В БОПП - ТЕКУЩЕЕ ЗНАЧЕНИЕ INTR
CONT ALUS=ZА,RА=INTR,ALU,YDEV=МРМЕМ,IОМР,WRY,MPADR=2
CONT FFCNT=CLREI,ISE
CLRW41:CJP А=CLRW41,МАР=РЕ,COND=INT,ISE
;ОТКРЫВАЕМ МАСКУ, ПРОВЕРЯЕМ В П/П НОМЕР ПРЕРЫВАНИЯ:
CJS А=WTNI,МАР=РЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=FЕТСН,ALUS=DZ,FUNC=AND,Н
>,ALUD=В,RВ=WR2,FFCNT=SETEI
;УСТАНАВЛИВАЕМ МАСКУ, ЖДЕМ СБРОСА INT ОТ ПП:
CONT ALUS=ZА,RА=INTR,ALU,YDEV=МРМЕМ,IОМР,WRY,MPADR=6
CONT FFCNT=CLREI,ISE
CLRW42:CJP А=CLRW42,МАР=РЕ,COND=INT,ISE,%TIK3%
;
;УСТАНАВЛИВАЕМ БП = 0
CONT А=В27,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;ОТРИЦАТЕЛЬНЫЙ АДРЕС - В АИСП, УБИРАЕМ БМСП, УСТАНАВЛИВАЕМ ОСТАНОВЫ
;ФИЗ.АДРЕС, РС, РФС - НЕ ИЗМЕНИЛИСЬ, ПИШЕМ ТОЛЬКО Ф.А. В РП
CONT А=С64,DSRC=PROM,ALUS=DZ,FUNC=EXNOR,Н,ALU,YDST=ADRREG
CJS А=СН,МАР=РЕ,ALUS=ZА,RА=РС,ALU,YDEV=PSMEM,WRY
CONT А=С8,DSRC=PROM,ALUS=DZ,ALU,YDST=ARBOPC
CONT А=ZER,DSRC=PROM,ALUS=DZ,ALU,YDEV=SТОРМ1,WRY
CONT А=С9,DSRC=PROM,ALUS=DZ,ALU,YDST=ARBOPC
CONT А=ZER,DSRC=PROM,ALUS=DZ,ALU,YDEV=SТОРМ1,WRY
CONT А=С10,DSRC=PROM,ALUS=DZ,ALU,YDST=ARBOPC
CONT А=ZER,DSRC=PROM,ALUS=DZ,ALU,YDEV=SТОРМ0,WRY
;
;ПРОВЕРЯЕМ INT ПРИ ЗАПИСИ
;ОШ=18,19,20 (НЕТ ПР.ПРЕР.; НЕТ СБРОСА; НЕ ТОТ НОМЕР)
CONT YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR
WA32W5:CJP А=WA32W5,МАР=РЕ,COND=ARBRDY,IСС
CJS А=INTMAS,МАР=РЕ,ISE
;СНОВА УСТАНАВЛИВАЕМ МАСКУ,ЖДЕМ СБРОСА INT ОТ ПП,В БОПП - ТЕКУЩЕЕ ЗНАЧЕНИЕ INTR
CONT ALUS=DА,RА=INTR,ALU,YDEV=МРМЕМ,IОМР,WRY,MPADR=2
CONT FFCNT=CLREI,ISE
CLRW51:CJP А=CLRW51,МАР=РЕ,COND=INT,ISE
;ОТКРЫВАЕМ МАСКУ, ПРОВЕРЯЕМ В П/П НОМЕР ПРЕРЫВАНИЯ:
CJS А=WTNI,МАР=РЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR,ALUS=DZ,FUNC=AND,Н
>,ALUD=В,RВ=WR2,FFCNT=SETEI
;УСТАНАВЛИВАЕМ МАСКУ, ЖДЕМ СБРОСА INT ОТ ПП:
CONT ALUS=ZА,RА=INTR,ALU,YDEV=МРМЕМ,IОМР,WRY,MPADR=2
CONT FFCNT=CLREI,ISE,YDST=CCLR
CLRW52:CJP А=CLRW52,МАР=РЕ,COND=INT,ISE,%TIK3%
;
;ПРОВЕРЯЕМ INT ПРИ ЧТЕНИИ КОМАНДЫ
;ОШ=21,22,23,24,25 (НЕТ ПР.ПРЕР.;НЕТ СБРОСА;НЕ ТА КОМАНДА;ТЕГ;НЕ ТОТ НОМЕР)
CONT YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=FЕТСН
WA32W6:CJP А=WA32W6,МАР=РЕ,COND=ARBRDY,IСС
CJS А=INTMAS,МАР=РЕ,ISE
CJS А=СМРСОМ,МАР=РЕ
;СНОВА УСТАНАВЛИВАЕМ МАСКУ,ЖДЕМ СБРОСА INT ОТ ПП,В БОПП - ТЕКУЩЕЕ ЗНАЧЕНИЕ INTR
CONT ALUS=ZА,RА=INTR,ALU,YDEV=МРМЕМ,IОМР,WRY,MPADR=2
CONT FFCNT=CLREI,ISE
CLRW61:CJP А=CLRW61,МАР=РЕ,COND=INT,ISE
;ОТКРЫВАЕМ МАСКУ, ПРОВЕРЯЕМ В П/П НОМЕР ПРЕРЫВАНИЯ:
CJS А=WTNI,МАР=РЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=FЕТСН,ALUS=DZ,FUNC=AND,Н
>,ALUD=В,RВ=WR2,FFCNT=SETEI
;УСТАНАВЛИВАЕМ МАСКУ, ЖДЕМ СБРОСА INT ОТ ПП:
CONT ALUS=ZА,RА=INTR,ALU,YDEV=МРМЕМ,IОМР,WRY,MPADR=2
CONT FFCNT=CLREI,ISE
CLRW62:CJP А=CLRW62,МАР=РЕ,COND=INT,ISE,%TIK3%
;
;СНОВА ПРОВЕРЯЕМ INT ПРИ ЗАПИСИ
;ОШ=26,27,28 (НЕТ ПР.ПРЕР.; НЕТ СБРОСА; НЕ ТОТ НОМЕР)
CONT ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=HALF,ALU,YDEVT=ECBTAG,WRB
>,BRA=RG3
CONT YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR
WA32W7:CJP А=WA32W7,МАР=РЕ,COND=ARBRDY,IСС
CJS А=INTMAS,МАР=РЕ,ISE
;СНОВА УСТАНАВЛИВАЕМ МАСКУ,ЖДЕМ СБРОСА INT ОТ ПП,В БОПП - ТЕКУЩЕЕ ЗНАЧЕНИЕ INTR
CONT ALUS=ZА,RА=INTR,ALU,YDEV=МРМЕМ,IОМР,WRY,MPADR=2
CONT FFCNT=CLREI,ISE
CLRW71:CJP А=CLRW71,МАР=РЕ,COND=INT,ISE
;ОТКРЫВАЕМ МАСКУ, ПРОВЕРЯЕМ В П/П НОМЕР ПРЕРЫВАНИЯ:
CJS А=WTNI,МАР=РЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DWR,ALUS=DZ,FUNC=AND,Н
>,ALUD=В,RВ=WR2,FFCNT=SETEI
;УСТАНАВЛИВАЕМ МАСКУ, ЖДЕМ СБРОСА INT ОТ ПП:
CONT ALUS=ZА,RА=INTR,ALU,YDEV=МРМЕМ,IОМР,WRY,MPADR=2
CONT FFCNT=CLREI,ISE
CLRW72:CJP А=CLRW72,МАР=РЕ,COND=INT,ISE,%TIK3%
;
;ПРОВЕРЯЕМ INT ПРИ ЧТЕНИИ ОПЕРАНДА
;ОШ=29,30,31,32,33 (НЕТ ПР.ПРЕР.;НЕТ СБРОСА;НЕ ТЕ ДАННЫЕ; ТЕГ;НЕ ТОТ НОМЕР)
CONT YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DRD
WA32W8:CJP А=WA32W8,МАР=РЕ,COND=ARBRDY,IСС
CJS А=INTMAS,МАР=РЕ,ISE
CJS А=CMPDAT,МАР=РЕ
;СНОВА УСТАНАВЛИВАЕМ МАСКУ,ЖДЕМ СБРОСА INT ОТ ПП,В БОПП - ТЕКУЩЕЕ ЗНАЧЕНИЕ INTR
CONT ALUS=ZА,RА=INTR,ALU,YDEV=МРМЕМ,IОМР,WRY,MPADR=2
CONT FFCNT=CLREI,ISE
CLRW81:CJP А=CLRW81,МАР=РЕ,COND=INT,ISE
;ОТКРЫВАЕМ МАСКУ, ПРОВЕРЯЕМ В П/П НОМЕР ПРЕРЫВАНИЯ:
CJS А=WTNI,МАР=РЕ,YDEV=PHYSAD,ЕСВ,WRB,BRA=RG0,ARBI=DRD,ALUS=DZ,FUNC=AND,Н
>,ALUD=В,RВ=WR2,FFCNT=SETEI
;УСТАНАВЛИВАЕМ МАСКУ, ЖДЕМ СБРОСА INT ОТ ПП:
CONT ALUS=ZА,RА=INTR,ALU,YDEV=МРМЕМ,IОМР,WRY,MPADR=2
CONT FFCNT=CLREI,ISE
CLRW82:CJP А=CLRW82,МАР=РЕ,COND=INT,ISE,%TIK3%
;
;ПРОВЕРЯЕМ "ОСТАНОВ" ПРИ ВОЗНИКНОВЕНИИ ЛЮБОГО ПРЕРЫВАНИЯ И ПОП = 1
;УСТАНАВЛИВАЕМ ПОП = 1, ДЕЛАЕМ ПРОГРАММНОЕ ПРЕРЫВАНИЕ
CONT А=В22,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=OR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
CONT FFCNT=PRGINT
;ПМИ + П/У НА АНАЛИЗ, ОШ=34,35 (НЕТ ПР.ПРЕР.; НЕТ СБРОСА)
CJS А=INTMAS,МАР=РЕ,ISE
;СБРАСЫВАЕМ ПРОГРАММНОЕ ПРЕРЫВАНИЕ
CONT FFCNT=CLRINT
;УСТАНАВЛИВАЕМ МАСКУ, ЖДЕМ СБРОСА INT ОТ ПП
CONT FFCNT=CLREI,ISE
CLR9:CJP А=CLR9,МАР=РЕ,COND=INT,ISE
;СБРАСЫВАЕМ МАСКУ, УСТАН.ПРОГРАММНОЕ ПРЕРЫВАНИЕ + ПОП=1, ПРОВЕРЯЕМ НОМЕР
CONT FFCNT=SETEI
;ОШ=36 (НЕ ТОТ НОМЕР)
CJS А=INTWAT,МАР=РЕ,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR2,FFCNT=PRGINT
;СБРАСЫВАЕМ ПРОГРАММНОЕ ПРЕРЫВАНИЕ (А ТО ОНО БУДЕТ МЕШАТЬ!)
CONT FFCNT=CLRINT
;УСТАНАВЛИВАЕМ МАСКУ, ЖДЕМ СБРОСА INT ОТ П
CONT FFCNT=CLREI,ISE
CLR10:CJP А=CLR10,МАР=РЕ,COND=INT,ISE,%TIK3%
;УБИРАЕМ ПОП
CONT А=В22,DSRC=PROM,ALUS=DА,RА=MANT,FUNC=EXOR,Н,ALUD=В,RВ=MANT,ALU,YDST=CNT
;
;ПРОВЕРЯЕМ "ОСТАНОВ" ПО СИГНАЛУ ИЗ МП, ОШ=37,38 (НЕТ ПР.ПРЕР.; НЕТ СБРОСА)
CONT FFCNT=SETHLT
CJS А=INTMAS,МАР=РЕ,ISE
;УСТАНАВЛИВАЕМ МАСКУ, ЖДЕМ СБРОСА INT ОТ ПП
CONT FFCNT=CLREI,ISE
CLR11:CJP А=CLR11,МАР=РЕ,COND=INT,ISE
;ОШ=39 (НЕ ТОТ НОМЕР)
CONT FFCNT=SETEI
CJS А=INTWAT,МАР=РЕ,ALUS=DZ,FUNC=AND,Н,ALUD=В,RВ=WR2,FFCNT=SETHLT
;УСТАН.МАСКУ, ЖДЕМ СБРОСА ПРЕРЫВАНИЙ ОТ ПП
CONT FFCNT=CLREI,ISE
CLR12:CJP А=CLR12,МАР=РЕ,COND=INT,ISE
CJP А=СС32,МАР=РЕ,%TIK3%
END
CTTWR:PROG;
;===== П/П ЗАПИСИ РГ.А В ТАЙМЕР СЧЕТНОГО ВРЕМЕНИ (16 РАЗРЯДОВ), РЕЖИМ 0!
;   ПОСЛЕДОВАТЕЛЬНОЕ ОБРАЩЕНИЕ К СТАРШЕМУ И МЛАДШЕМУ БАЙТАМ, КАНАЛ 2
;   РГ.А - ЧТО ПИШЕМ; WR6 - КОМ.СЛОВО; WR5 - РАБОЧИЙ
;
;ПИШЕМ В ТАЙМЕР КОМАНДНОЕ СЛОВО: NN ОО RRR D = 10 11 000 0 = В0Н
;NN=10: N КАНАЛА 2
;ОО=11: ПОСЛЕДОВАТЕЛЬНОЕ ОБРАЩЕНИЕ
;RRR=000: РЕЖИМ 0 (ГЕНЕРАЦИЯ ЗАДЕРЖАННОГО СИГНАЛА ПРЕРЫВАНИЯ)
;D=0: ДВОИЧНЫЙ СЧЕТ
CONT А=R0СН2,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR6
CONT ALUS=ZА,RА=WR6,ALU,YDTIM=CTIME,WTIME=WТ,IОМР,FFCNT=3
CONT ALUS=ZА,RА=WR6,ALU,YDTIM=CTIME,WTIME=WТ,WRD,IОМР,FFCNT=3
PUSH А=1,МАР=РЕ,ALUS=ZА,RА=WR6,ALU,YDTIM=CTIME,WTIME=WТ,IОМР,FFCNT=3
RFCT
;ПИШЕМ 1-8 РАЗРЯДЫ, А1,А0=10
CONT ALUS=ZА,RА=А,ALU,YDTIM=CTIME,WTIME=WТ,IОМР,FFCNT=2
CONT ALUS=ZА,RА=А,ALU,YDTIM=CTIME,WTIME=WТ,WRD,IОМР,FFCNT=2
PUSH А=1,МАР=РЕ,ALUS=ZА,RА=А,ALU,YDTIM=CTIME,WTIME=WТ,IОМР,FFCNT=2
;ПИШЕМ СТАРШИЕ БАЙТЫ, 9-16 РАЗРЯДЫ
RFCT ALUS=DZ,Н,ALUD=ВА,RА=А,RВ=WR5,ALU,DSRC=SHIFT,MPS,PSHF=SН0+8,SHF=LOG
CONT ALUS=ZА,RА=WR5,ALU,YDTIM=CTIME,WTIME=WТ,IОМР,FFCNT=2
CONT ALUS=ZА,RА=WR5,ALU,YDTIM=CTIME,WTIME=WТ,WRD,IОМР,FFCNT=2
PUSH А=24,МАР=РЕ,ALUS=ZА,RА=WR5,ALU,YDTIM=CTIME,WTIME=WТ,IОМР,FFCNT=2
;ЖДЕМ ЗАПИСИ: ЭКСПРЕРИМЕНТАЛЬНО ОПРЕДЕЛЕНО, ЧТО НАДО ЖДАТЬ ОКОЛО 24 ТАКТОВ, Т.К.
;СИГНАЛ CLOCK, ПО КОТОРОМУ ДЕЛАЕТСЯ ЗАПИСЬ, ПРИХОДИТ 1 РАЗ В 10 MKS = 20 МК
RFCT
CRTN
;
;
CTTSTP:ENTRY
;===== П/П ОСТАНОВА ТАЙМЕРА СЧЕТНОГО ВРЕМЕНИ
;   ЗАДАЕМ ПОСЛЕДОВАТЕЛЬНОЕ ОБРАЩЕНИЕ К СТАРШЕМУ И МЛАДШЕМУ БАЙТАМ, КАНАЛ 2,
;   А ЗАПИСЫВАЕМ ТОЛЬКО ОДИН БАЙТ!!! ИСПОЛЬЗУЕМ РГ.WR5.
;
;ПИШЕМ В ТАЙМЕР КОМАНДНОЕ СЛОВО: NN ОО RRR D = 10 11 000 0 = В0Н
;NN=10: N КАНАЛА 2
;ОО=11: ПОСЛЕДОВАТЕЛЬНОЕ ОБРАЩЕНИЕ
;RRR=000: РЕЖИМ 0 (ГЕНЕРАЦИЯ ЗАДЕРЖАННОГО СИГНАЛА ПРЕРЫВАНИЯ)
;D=0: ДВОИЧНЫЙ СЧЕТ
CJP А=PSSTP,МАР=РЕ
PSSTP:CONT А=R0СН2,DSRC=PROM,ALUS=DZ,ALUD=В,RВ=WR5
CONT ALUS=ZА,RА=WR5,ALU,YDTIM=CTIME,WTIME=WТ,IОМР,FFCNT=3
CONT ALUS=ZА,RА=WR5,ALU,YDTIM=CTIME,WTIME=WТ,WRD,IОМР,FFCNT=3
PUSH А=1,МАР=РЕ,ALUS=ZА,RА=WR5,ALU,YDTIM=CTIME,WTIME=WТ,IОМР,FFCNT=3
RFCT
;ПИШЕМ ЧЕГО-НИБУДЬ, А1,А0=10
CONT ALUS=ZА,RА=WR5,ALU,YDTIM=CTIME,WTIME=WТ,IОМР,FFCNT=2
CONT ALUS=ZА,RА=WR5,ALU,YDTIM=CTIME,WTIME=WТ,WRD,IОМР,FFCNT=2
CONT ALUS=ZА,RА=WR5,ALU,YDTIM=CTIME,WTIME=WТ,IОМР,FFCNT=2
CRTN
END
SС00:PROG А;
CONT
END
SЕ00:PROG 9;
CONT
END
RINT:PROG 8;
;===== МИКРОПРОГРАММА РЕАКЦИИ НА ПРЕРЫВАНИЯ
;   НАХОДИТСЯ В ПАМЯТИ МИКРОКОМАНД, НАЧИНАЯ С АДРЕСА $F00
;   ВСЕ ЗАМЕЧАНИЯ СМ.В АНАЛОГИЧНОЙ ПРОГРАММЕ ДЛЯ СИСТЕМЫ КОМАНД.
;
;ЗАПИСЫВАЕМ В РГ/СЧ АДРЕС МП ОБРАБОТКИ ВНУТРЕННЕГО ПРЕРЫВАНИЯ
ADRF00:ENTRY
LDCT А=INTINT,МАР=РЕ,FFCNT=CLRTR0
CJV COND=INTSTP,IСС
;
;      =================
;      П Н А  ПРЕРЫВАНИЙ
;      =================
;
;      СЛ.0  ЗСМ2  ЗСМ1  VЕСТ 5-1
;       0     Х     Х    0 0 0 0 0
;                         .  .  .
;                        1 1 1 1 1
;
;        ПРЕРЫВАНИЯ                  ТИП           ВХОД ПНА     ВЫХОД ПНА
;  ПРИ ОБРАЩЕНИИ К ОЗУ И ШИНЕ
; 1.ОТСУТСТВУЮЩИЙ БЛОК ПАМЯТИ    СООБЩ.НА ПУЛЬТ   00 20 40 60       F02
; 2.МНОГОКРАТНАЯ ОШИБКА          СООБЩ.НА ПУЛЬТ   01 21 41 61       F03
;      ПРЕРЫВАНИЯ ПО ТЕГУ
; 3.ПРОГР.ИНТЕРПР.ТЕГА ОПЕРАНДА    ВНУТРЕННЕЕ     02 22 42 62       F04
; 4.ЧУЖОЙ СУММАТОР                 ВНУТРЕННЕЕ     03 23 43 63       F05
; 5.ЧУЖОЙ ОПЕРАНД                  ВНУТРЕННЕЕ     04 24 44 64       F06
; 6.ЗАЩИТА АДРЕСА ПРИ ЧТЕНИИ       ВНУТРЕННЕЕ     05 25 45 65       F07
; 7.КОНТРОЛЬ КОМАНДЫ               ВНУТРЕННЕЕ     06 26 46 66       F08
; 8.ЗАЩИТА ВЫБОРКИ КОМАНДЫ         ВНУТРЕННЕЕ     07 27 47 67       F09
; 9.ЗАЩИТА ПЕРЕДАЧИ УПРАВЛЕНИЯ     ВНУТРЕННЕЕ     08 28 48 68       F0А
;10.ПРОГР.ИНТЕРПР.ТЕГА КОМАНДЫ     ВНУТРЕННЕЕ     09 29 49 69       F0В
;  ПРЕРЫВАНИЕ ПРИ ЗАПИСИ В ОЗУ
;11.ЗАЩИТА АДРЕСА ПРИ ЗАПИСИ       ВНУТРЕННЕЕ     0А 2А 4А 6А       F0D
;  ПРИ ОБРАЩЕНИИ К ОЗУ И ШИНЕ
;12."TIME OUT" ПРИ ОБР.К ОЗУ     СООБЩ.НА ПУЛЬТ   0В 2В 4В 6В       FOF
;13."TIME OUT" ПРИ ОБР.К ШИНЕ    СООБЩ.НА ПУЛЬТ   0С 2С 4С 6С       F11
;     ПО ПРИПИСКЕ
;14.МАТ.АДРЕС = 0               ПСЕВДОПРЕРЫВАНИЕ  0D 2D 4D 6D       F13
;15.ЧУЖОЙ РП ПРИ ЧТ/ЗП ОПЕРАНДА    ВНУТРЕННЕЕ     0Е 2Е 4Е 6Е       F14
;16.ЧУЖОЙ РП ПРИ ВЫБОРКЕ КОМАНДЫ   ВНУТРЕННЕЕ     0F 2F 4F 6F       F15
;17.ЗАЩИТА СТРАНИЦЫ ПРИ ОБРАЩЕНИИ  ВНУТРЕННЕЕ     10 30 50 70       F16
;18.ЗАЩИТА СТРАНИЦЫ ПРИ ЗАПИСИ     ВНУТРЕННЕЕ     11 31 51 71       F17
;19.ОТРИЦАТ.N СТРАНИЦЫ У КОМАНДЫ   ВНУТРЕННЕЕ     12 32 52 72       F18
;20.ОТРИЦАТ.N СТРАНИЦЫ У ОПЕРАНДА  ВНУТРЕННЕЕ     13 33 53 73       F19
;     ПРОЧИЕ
;21.РЕЗЕРВ                                        14 34 54 74       F1А
;22.ОТСУТСТВ.АДРЕС ПАМЯТИ В НР     ВНУТРЕННЕЕ     15 35 55 75       F1С
;23.РЕЗЕРВ                                        16 36 56 76       F1D
;24.ЗАПРОС МОДИФ.ПРИОРИТЕТОВ СТР.    ПСЕВДО       17 37 57 77       F1F
;25.ТАЙМЕР АСТРОН.ВРЕМЕНИ = 0    ВНУТР./ПСЕВДО!   18 38 58 78       F20
;26.ТАЙМЕР СЧЕТНОГО ВРЕМЕНИ = 0  ВНУТР./ПСЕВДО!   19 39 59 79       F21
;27."TIME OUT" ПРИ БВП        СООБЩ.ПП ИЛИ ВНУТР. 1А 3А 5А 7А       F22
;28.ШАГОВОЕ ПРЕРЫВАНИЕ             ВНУТРЕННЕЕ     1В 3В 5В 7В       F23
;29.ВНЕШНИЕ ПРЕРЫВАНИЯ               ВНЕШНЕЕ      1С 3С 5С 7С       F24
;30.ЧТ/ЗП РЕГИСТРОВ ОТ ПП            ПСЕВДО       1D 3D 5D 7D       F25
;31.ПРОГРАММНОЕ ПРЕРЫВАНИЕ         ВНУТРЕННЕЕ     1Е 3Е 5Е 7Е       F26
;32.ОСТАНОВ (HALT)                   ПСЕВДО       1F 3F 5F 7F       F27
;
;===== F28 - Р.S. МИКРОПРОГРАММ ОБРАБОТКИ ПРЕРЫВАНИЙ
;===== ПЕРЕКЛЮЧАТЕЛЬ ДЛЯ ПРЕРЫВАНИЙ:
;F02: 1 - СООБЩЕНИЕ НА ПУЛЬТ ИЛИ ЛЮБОЕ ДРУГОЕ ПРЕРЫВАНИЕ ПРИ ПОП=1
CJP А=INT1,МАР=РЕ
;F03: 2 - СООБЩЕНИЕ НА ПУЛЬТ
CJP А=INT2,МАР=РЕ
;F04: 3 - ВНУТРЕННЕЕ
;П/У ПО АДРЕСУ ИЗ РГ/СЧ, ГДЕ ЗАПИСАН АДРЕС МП ОБРАБОТКИ ВНУТРЕННЕГО ПРЕРЫВАНИЯ
;НОМЕР ПРЕРЫВАНИЯ ЧИТАЕМ В РГ.WR1
JRP А=С3,IСС,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;F05: 4 - ВНУТРЕННЕЕ
JRP А=С4,IСС,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;F06: 5 - ВНУТРЕННЕЕ
JRP А=С5,IСС,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;F07: 6 - ВНУТРЕННЕЕ
JRP А=С6,IСС,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;F08: 7 - ВНУТРЕННЕЕ
JRP А=С7,IСС,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;F09: 8 - ВНУТРЕННЕЕ
JRP А=С8,IСС,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;F0А: 9 - ВНУТРЕННЕЕ
JRP А=С9,IСС,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;F0В: 10 - ВНУТРЕННЕЕ
JRP А=С10,IСС,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;ЛИШНЯЯ КОМАНДА!!!!!
CONT
;F0D: 11 - ВНУТРЕННЕЕ
;ЗАПРЕТ ЗАПИСИ: Т.К.ЭТО ВЫЯСНЯЕТСЯ ТОЛЬКО ПРИ ЗАПИСИ В ОЗУ (УЖЕ ПОСЛЕ ЗАПИСИ
;В КЭШ), НАДО СРОЧНО СБРАСЫВАТЬ КЭШ! ИНАЧЕ БУДЕТ ЧТЕНИЕ ИЗ КЭША.
JRP А=С11,IСС,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1,YDST=CCLR
;ЛИШНЯЯ КОМАНДА!!!!!
CONT
;F0F: 12 - СООБЩЕНИЕ НА ПУЛЬТ
CONT А=С12,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
CJP А=INFMP,МАР=РЕ
;В11: 13 - СООБЩЕНИЕ НА ПУЛЬТ
CONT А=С13,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
CJP А=INFMP,МАР=РЕ
;F13: 14 - ПСЕВДОПРЕРЫВАНИЕ: 0 МАТ.ADREC
CJP А=INT14,МАР=РЕ
;F14: 15 - ВНУТРЕННЕЕ
JRP А=С15,IСС,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;F15: 16 - ВНУТРЕННЕЕ
JRP А=С16,IСС,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;F16: 17 - ВНУТРЕННЕЕ
JRP А=С17,IСС,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;F17: 18 - ВНУТРЕННЕЕ
JRP А=С18,IСС,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;F18: 19 - ВНУТРЕННЕЕ
JRP А=С19,IСС,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;F19: 20 - ВНУТРЕННЕЕ
JRP А=С20,IСС,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;F1А: 21 - РЕЗЕРВ, СООБЩЕНИЕ НА ПУЛЬТ (ОШИБКА)
CONT А=С21,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
CJP А=INFMP,МАР=РЕ
;F1С: 22 - ВНУТРЕННЕЕ
JRP А=С22,IСС,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;F1D: 23 - РЕЗЕРВ, СООБЩЕНИЕ НА ПУЛЬТ (ОШИБКА)
CONT А=С23,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
CJP А=INFMP,МАР=РЕ
;F1F: 24 - ПСЕВДОПРЕРЫВАНИЕ: ЗАПРОС МОДИФИКАЦИИ ПРИОРИТЕТОВ СТРАНИЦ
CJP А=INT24,МАР=РЕ
;F20: 25 - БЫВШИЙ ТАЙМЕР АСТР.ВР.=0: РЕЗЕРВ. СООБШЕНИЕ ПП, Т.К.НЕ Д.БЫТЬ
CJP А=INT25,МАР=РЕ
;F21: 26 - ТАЙМЕР СЧЕТН.ВР.=0: ПОКА ВНУТРЕННЕЕ,Д.БЫТЬ,ВИДИМО,ПСЕВДО
JRP А=С26,IСС,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1,FFCNT=CLRCT
;F22: 27 - ВНУТРЕННЕЕ ИЛИ СООБЩЕНИЕ НА ПУЛЬТ: "TIME-OUT" ПРИ БВП
CJP А=INT27,МАР=РЕ
;F23: 28 - ВНУТРЕННЕЕ
JRP А=С28,IСС,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;F24: 29 - ВНЕШНЕЕ
CJP А=INT29,МАР=РЕ
;F25: 30 - ПСЕВДОПРЕРЫВАНИЕ: ЧТ/ЗП РГ.ПП
CJP А=INT30,МАР=РЕ
;F26: 31 - ВНУТРЕННЕЕ
JRP А=С31,IСС,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;F27: 32 - ПСЕВДОПРЕРЫВАНИЕ: HALT
INT32:CONT А=С32,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
;Р.S. HALT
;===== N ПРЕРЫВАНИЯ (WR1) - В 5 РГ.БОПП, СТОИМ
;ВНУТРЕННИЕ
INTINT:ENTRY
;СООБЩЕНИЕ НА ПУЛЬТ (INT 1-2, 12-13); РЕЗЕРВ (INT 21,23)
INFMP:ENTRY
;===== N ПРЕРЫВАНИЯ  ИЗ WR1 - В 5 РГ.БОПП; WR2 - В 4 РГ.БОПП; ВОЗВРАТ
PSINT:PUSH А=0,МАР=РЕ,ALUS=ZА,RА=WR1,ALU,YDEV=МРМЕМ,WRY,IОМР,MPADR=5
RTNINT:ENTRY
LООР ALUS=ZА,RА=WR2,ALU,YDEV=МРМЕМ,WRY,IОМР,MPADR=4
CRTN
;МАТ.АДРЕС = 0 (INT 14)
INT14:CONT А=С14,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
CJP А=PSINT,МАР=РЕ
;ЗАПРОС МОДИФИКАЦИИ ПРИОРИТЕТОВ СТРАНИЦ (INT 24)
INT24:CONT А=С24,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
CJP А=PSINT,МАР=РЕ
;БЫВШЕЕ "ТАЙМЕР АВ = 0"; РЕЗЕРВ
INT25:CONT А=С25,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
CJP А=INFMP,МАР=РЕ
;"TIME OUT" ПРИ БВП (INT 27)
INT27:CONT А=С27,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
CJP А=PSINT,МАР=РЕ
;ВНЕШНЕЕ ПРЕРЫВАНИЕ (INT 29)
INT29:CONT А=С29,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
CJP А=PSINT,МАР=РЕ
;ЧТ/ЗП РЕГИСТРОВ ОТ ПП (INT 30)
INT30:CONT А=С30,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
CJP А=PSINT,МАР=РЕ
;МНОГОКРАТНАЯ ОШИБКА (INT 2); СООБЩЕНИЕ НА ПУЛЬТ
INT2:CONT А=С2,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
CJP А=INFMP,МАР=РЕ
;ОТСУТСТВУЮЩИЙ БЛОК ПАМЯТИ (INT 1); СООБЩЕНИЕ НА ПУЛЬТ
;ИЛИ HALT, ЕСЛИ ЛЮБОЕ ДРУГОЕ ПРЕРЫВАНИЕ ПРИ ПОП=1 (INT 32); ПСЕВДОПРЕРЫВАНИЕ
INT1:CJP А=INT32,МАР=РЕ,COND=INTSTP
CONT А=С1,DSRC=PROM,ALUS=DZ,Н,ALUD=В,RВ=WR1
CJP А=INFMP,МАР=РЕ
END
SF80:PROG 7;
CONT
END
SFC0:PROG 6;
CONT
END
SFE0:PROG 5;
CONT
END
SFF0:PROG 4;
CONT
END
SFF8:PROG 3;
CONT
END
SFFC:PROG 2;
CONT
END
INTINP:PROG 1;
EXTERN ADRF00
CONT
;===== АППАРАТНЫЙ ВХОД В ПРЕРЫВАНИЕ (АДРЕС FFF):
;ЗАПОМИНАЕМ АДРЕС ПРЕРВАННОЙ МИКРОПРОГРАММЫ В СТЕКЕ, УСТАНАВЛИВАЕМ МАСКУ
CJS А=ADRF00,МАР=РЕ,FFCNT=CLREI
END
INTRLD:PROG;
;===== СПИСОК ЗАГРУЖАЕМЫХ ПРОГРАММ:
EXTERN INTRPT,INOUT,CTTWR,CTTSTP,SЕ00,RINT
EXTERN SF80,SFC0,SFE0,SFF0,SFF8,SFFC,INTINP
END
LD10:PROG;
EXTERNAL SHMP,INTRLD
END
*END
*NO LIST
*CALL MICLOA:LD10
*CALL MPROM
*END F
*END FILE
