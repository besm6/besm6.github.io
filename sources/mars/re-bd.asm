        macro
&l      err     &txt
&l      мода    BASE
        уиа     tx&sndx.(М15)
        пб      a&sndx
tx&sndx текст   п&txt
        конд    m40b'172'
a&sndx  пв      '76005'(М16)
        пб      '76002'
        mend
* prep проверяет корректность сис. листа, устанавливает
* М16, опционально устанавливает дескрипторы и ключ
* (должен быть ненулевой)
        macro
&l      prep    &err,&b
&l      мода    erased
        уиа     (М16)
        мод     sysadr
        сч      1
        нтж     bandit
        пе      errmsg
        счи     М4
        мод     sysadr
        уиа     (M4)
        зп      Msavm4(M4)
        мода    &err
        уиа     (М16)
        счи     М16
        зп      Meradr(M4)
        aif     (&b eq '').ex
        сч      Mcurds(M4)
        зп      Madesc(M4)
        сч      &b
        по      errmsg
        зп      Mkey(M4)
.ex     mend
* opt проверяет командную строку: даются адрес перехода по концу параметров
* и опционально ожидаемый тип параметра
        macro
&l      opt     &err,&typ
&l      мода    BASE
        пв      getopt(М16)
        сч      (М2)
        по      &err
        aif    (&typ eq '').ex
        сч      cmdtyp
        нтж     &typ
.ex     mend
* декодирование типов параметров
        macro
        case    &a,&b,&c,&d
        aif     (&a eq '').ex
        сч      cmdtyp
        нтж     &a(1)
        по      &a(2)
        case    &b,&c,&d
.ex     mend
* декодирование CMD по таблице
        macro
&l      decod   &tab,&err
&l      мода    BASE
        уиа     &tab.(М1)
lp&sndx сч      (М1)
        по      &err
        нтж     CMD
        по      gd&sndx
        слиа    2(М1)
        пб      lp&sndx
gd&sndx мод     1(М1)
        пб      BASE
        mend
* чтение системного листа и установка адреса буфера        
        macro
&l      rdsys   &adr
&l      уии     М16(М4)
        сч      sy&sndx
        зп      my&sndx
        сч      syspag
        сда     64-30
        или     my&sndx
        зп      my&sndx
        Э70     my&sndx
        пб      go&sndx
sy&sndx конд    в'0010000000400027'
my&sndx пам     1
go&sndx сч      syspag
        сда     64-10
        уи      М4
        зп      sysadr
        сч      tabpag
        сда     64-10
        зп      Mbdtab(M4)
        сч      &adr
        сда     64-10
        зп      Mbdbuf(M4)
        mend
* оператор switch-case        
        macro
        swcnt    &a,&b,&c,&d,&e,&f,&g,&h
        aif      (&a eq '').ex
        пб       &a
        мода
        swcnt   &b,&c,&d,&e,&f,&g,&h
.ex     mend        
        macro
&l      swtch   &a,&b,&c,&d,&e,&f,&g,&h,&i
&l      мод     &a
        пб      *+1
        пам     0
        пб      &b
        мода
        swcnt   &c,&d,&e,&f,&g,&h,&i        
        mend
* передача параметров в системный лист        
        macro
        args    &a,&b,&c,&d,&e
        aif     (&a eq '').ex
        сч      &a(1)
        зп      &a(2)(M4)
        args    &b,&c,&d,&e
.ex     mend        
BD      СТАРТ   '1'
BASE    equ     BD-1
        употр   BASE(М13)
        пам     1
        конд    в'54001011' ???
        конд    п'ВD    '
        уиа     '72000'(M13)
        пб      START
        пам     2
        уиа     (M13)
        уиа     '35072'(М3)
        уиа     '76001'(М1)
        уиа     (М2)
copy    мода    '1777'
        сч      (М2)
        мода    '67777'
        Э75     (М2)
        слиа    -1(М2)
        мода    BASE
        цикл    copy(М1)
        уиа     '66000'(M13)
        пб      START
START   сч      dfltad
        зп      sysadr
        мода    BASE
        уиа     badarg(М12)
        пв      '76016'(М11)
        мод     '76015'
        сч      3
        уи      М2
        сч      
        зп      Nopt
        слиа    -1(М2)
        мода    BASE    
        пв      getopt(М16)
        пб      A00047
cmdtyp  пам     1
CMD     пам     1
Ttext   конд    в'5'
Ttype4  конд    в'4'
        конд    в'3'
Toctal  конд    в'2'
        конд    в'1'
Nopt    пам     1
getopt  слиа    1(М2)
        сч      (М2)
        сда     64-3
        счмр    
        зп      cmdtyp
        сч      Nopt
        слц     E1
        зп      Nopt
        мод     (М2)
        сч      
        зп      CMD
        сч      cmdtyp
        нтж     Ttext
        пе      A00046
        сч      CMD
        зп      cmd5
A00046  пб      (М16)
A00047  opt     noargs,Ttext
        пе      wrong
        decod   BDdec,wrong
wrong   err     'ВD <НЕ ТО>'
noargs  err     'ВD, А ДАЛЬШЕ'
badarg  err     'НЕПР. СПИСОК ПАРАМЕТРОВ ДЛЯ ВD'
cmd5    пам     1
switch  пам     1
errmsg  сда     64-30
        уии     М15(М16)
        пв      '76005'(М16)
        пб      '76002'
        конд    п'ОШ.N= '
        пам     1
        конд    m40b'172'
NЕW     сч      B4
        зп      switch
        мода    BASE
        уиа     wrong(М5)
        мода    BASE
        уиа     bufnam(M4)
        мода    BASE
        пв      NEWarg(М3)
A00124  opt     sw1,Ttext
        пе      badext
        мода    BASE
        уиа     A00124(М5)
        decod   NEWdec,badext
badext  err     'НЕПР. ДОПОЛНЕНИЕ'
nooct   err     'НЕТ ВОСЬМЕРИЧНОГО НОМЕРА'
СИС     мода    BASE
        уиа     syspag(М6)
getval  opt     nooct,Toctal
        пе      nooct
        сч      CMD
        зп      (М6)
        пб      (М5)
ТАБ     мода    BASE
        уиа     tabpag(М6)
        пб      getval
rdex23  rdsys   memadr
        пб      exec23
erased  текст   п'СТЕРТА БД-СИ'
        конд    п'СТЕМА0'в'172'
exec23  prep    errmsg,bufnam
        сч      maxzon
        сда     64-6
        зп      Mmyloc(M4)
        сч      maxzon
        сда     64-18
        или     bufloc
        зп      Marg(M4)
        мода    (M4)
        пв      '23'(М16)
        пб      '76002'
ОРЕN    opt     wrong
        сч      CMD
        зп      bufnam
        пб      rdex16
СЛО     opt     nooct
        сч      B2
        зп      swtch2
        сч      CMD
        зп      EOT2
        пб      (М5)
swtch2  конд    в'1'
rdex16  rdsys   bufpag
        prep    errmsg,bufnam
        мода    (M4)
        пв      '16'(М16)
        пб      '76002'
РUТ     сч      E1
        зп      switch
        пб      OP
DЕLЕТЕ  сч      B2
        зп      switch
        пб      OP
МОDIFУ  сч      B3
        зп      switch
        пб      OP
GЕТ     сч      
        зп      switch
OP      opt     wrong
        сч      CMD
        зп      bufnam
        opt     sw1
        case    (Toctal,A00307),(Ttext,A00314)
A00307  сч      CMD
        зп      ПАМ
        opt     sw1
        case    (Ttext,A00314)
        пб      badext
A00314  мода    BASE
        уиа     A00307+1(М5)
        decod   GETdec,A00322
A00322  пб      badext
БУФ     мода    BASE
        уиа     bufpag(М6)
        пб      getval
ДЛВ     мода    BASE
        уиа     LEN(М6)
        сч      memadr
        зп      swtch2
        пб      getval
СИМ     мода    BASE
        уиа     EOT1(М6)
        сч      E1
        зп      swtch2
        пб      getval
sw1     swtch   switch,exec32,sw2,exec21,exec22,rdex23,rdex16
exit    пб      '76002'
exec32  prep    errmsg,bufnam
        args    (0,Mmylen),(ПАМ,Mmyloc)
        мода    (M4)
        пв      '20'(М16)
        пб      exit
sw2     swtch   swtch2,exec17,exec33
        prep    errmsg,bufnam
        args    (ПАМ,Mmyloc),(LEN,Mmylen),(EOT2,Marg)
        сч      E16
        мода    (M4)
        пв      '33'(М16)
        пб      exit
exec33  prep    errmsg,bufnam
        args    (ПАМ,Mmyloc),(LEN,Mmylen),(EOT1,Marg)
        сч      
        мода    (M4)
        пв      '33'(М16)
        пб      exit
exec17  prep    errmsg,bufnam
        args    (ПАМ,Mmyloc),(LEN,Mmylen)
        мода    (M4)
        пв      '17'(М16)
        пб      exit
exec21  prep    errmsg,bufnam
        мода    (M4)
        пв      '21'(М16)
        пб      exit
exec22  prep    errmsg,bufnam
        args    (ПАМ,Mmyloc),(0,Mmylen)
        мода    (M4)
        пв      '22'(M4) should be (M16) ???
        пб      exit
BDdec   конд    п'NЕW   '
        конд    a(NEW)
        конд    п'ОРЕN  '
        конд    a(OPEN)
        конд    п'ОВТАIN'
        конд    a(GET)
        конд    п'ОВТ   '
        конд    a(GET)
        конд    п'РUТ   '
        конд    a(PUT)
        конд    п'DЕLЕТЕ'
        конд    a(DELETE)
        конд    п'DЕL   '
        конд    a(DELETE)
        конд    п'МОDIFУ'
        конд    a(MODIFY)
        конд    п'МОD   '
        конд    a(MODIFY)
        конд    п'GЕТ   '
        конд    a(GET)
        конд    п'ИНФ   '
        конд    a(ИНФ)
        конд    п'КТЛ   '
        конд    a(КТЛ)
        пам     1
NEWdec  конд    п'СИС   '
        конд    a(СИС)
        конд    п'ТАБ   '
        конд    a(ТАБ)
        конд    п'БУФ   '
        конд    a(БУФ)
        пам     1
GETdec  конд    п'ДЛВ   '
        конд    a(ДЛВ)
        конд    п'СИМ   '
        конд    a(СИМ)
        конд    п'СЛО   '
        конд    a(СЛО)
        пам     1
bufnam  конд    п'БУФ   '
bufloc  конд    в'270000'
maxzon  конд    в'20'
syspag  конд    в'36'
tabpag  конд    в'34'
bufpag  конд    в'33'
ПАМ     конд    в'10000' область для текста в памяти
EOT1    конд    m8b'172'b'175'
EOT2    конд    m8b'172'b'175'
LEN     пам     1
NEWarg  opt     (М5)
        case    (Ttext,A00574),(Toctal,A00610),(Ttype4,A00620)
badarr  err     'ДЛЯ ВD НЕПР. ЗАДАН МАССИВ'
A00574  сч      '1770'(М10)
        зп      -8
        сч      CMD
        зп      -7
        сч      memadr
        сда     64-10
        зп      -6
        сч      D01022
        зп      -5
        сч      '76000'
        зп      -4
        пв      '76006'(М11)
        уии     М15(М1)
        пб      wrexit
unusd1  сч      -3
        зп      1(M4)
        сч      -5
        зп      2(M4)
        сч      1(M4)
        и       E18T1
        зп      1(M4)
        сч      CMD
        зп      (M4)
        пб      (М3)
A00610  сч      BOCMP
        зп      (M4)
        сч      CMD
        зп      1(M4)
        opt     badarr,Toctal
        пе      badarr
        мода    
        сч      CMD
        зп      2(M4)
        пб      (М3)
A00620  сч      CMD
        нтж     B26
        по      (М3)
        пб      badarr
unusd2  opt     (М5)
        case    (Ttext,A00574),(Toctal,A00610),(Ttype4,(M3))
        пб      badarr
КТЛ     opt     A00723
        case    (Ttext,A00635)
        пб      badext
A00635  decod   KTLdec,bad1
bad1    пб      badext
FULL    prep    errmsg
        args    (0,Marg)
        мода    (M4)
        пв      '34'(М16)
        мода    BASE
        уиа     A00664-6(М15)
        пб      A00664
        текст   п'ИМЯ         ДЛИНА  ШИФР ДАТА'
        конд    м40в'172'
A00664  пв      '76005'(М16)
A00665  prep    errmsg
        сч      Mcurds(M4)
        зп      Madesc(M4)
        сч      Mcurky(M4)
        пе      A00700
        мода    Mcurky+1(M4)
        уиа     (М16)
        сч      B21
        пб      errmsg
A00700  зп      Mkey(M4)
        args    (B3,Marg),(0,Mmylen),(memadr,Mmyloc)
        мода    (M4)
        пв      '20'(М16)
        уи      М15
        пв      '76005'(М16)
        prep    exit
        args    (B3,Marg)
        мода    (M4)
        пв      '34'(М16)
        пб      A00665
KTLdec  конд    п'ПОЛН  '
afull   конд    A(FULL)
        конд    в'0'
A00723  prep    errmsg
        args    (0,Marg)        
        мода    (M4)
        пв      '34'(М16)
floop   зп      fname
        мода    fname
        уиа     (М15)
        пв      '76005'(М16)
        prep    exit
        args    (B3,Marg)
        мода    (M4)
        пв      '34'(М16)
        пб      floop
ИНФ     opt     badext
        case    (Ttext,header)
        пб      badext
header  мода    BASE
        уиа     header+2(М15)
        пб      exec20
        текст   п'ИМЯ         ДЛИНА  ШИФР ДАТА'
        конд    м40в'172'
exec20  пв      '76005'(М16)
        prep    errmsg,CMD
        args    (B3,Marg),(0,Mmylen),(memadr,Mmyloc)
        мода    (M4)
        пв      '20'(М16)
        уи      М15
        пв      '76005'(М16)
        пб      '76002'
wrexit  пв      '76005'(М16)
        пб      '76002'
fname   пам     1
        конд    м40в'172'
dfltad  конд    в'74000'
sysadr  конд    п'БАН0КА'
E1      конд    п'000001'
B4      конд    п'000004'
memadr  пам     1
bandit  конд    п'БАНДИТ'
B2      конд    п'000002'
B3      конд    п'000003'
E16     конд    в'100000'
D01022  конд    в'0000000000000020'
E18T1   конд    в'777777'
BOCMP   конд    п'ВОСЬМР'
B26     конд    в'26'
B21     конд    в'21'

Mcurds  equ '05'
Mbdbuf  equ '06'
Mkey    equ '10'
Meradr  equ '11'
Madesc  equ '12'
Mmyloc  equ '13'
Mbdtab  equ '14'
Mmylen  equ '15'
Msavm4  equ '24'
Marg    equ '27'
Mcurky  equ '37'

M1      equ     1
M2      equ     2
M3      equ     3
M4      equ     4
M5      equ     5
M6      equ     6
M10     equ     8
M11     equ     9
M12     equ     10
M13     equ     11
M15     equ     13
M16     equ     14
        ФИНИШ
