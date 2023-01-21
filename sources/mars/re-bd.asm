        macro
&l      err     &txt
&l      мода    BASE
        уиа     tx&sndx.(М15)
        пб      a&sndx
tx&sndx текст   п&txt
        конд    m40b'172'
a&sndx  пв      write(М16)
        пб      pult
        mend
* prep проверяет корректность сис. листа, устанавливает
* М4 на него, опционально устанавливает дескрипторы и ключ
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
        сч      cmdtyp
        нтж     &a(1)
        по      &a(2)
        aif     (&b eq '').ex
        case    &b,&c,&d
.ex     mend
* декодирование CMD по таблице
        macro
&l      decod   &tab
&l      мода    BASE
        уиа     &tab.(М1)
lp&sndx сч      (М1)
        по      gd&sndx+1
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
        сч      &a(1)
        зп      &a(2)(M4)
        aif     (&b eq '').ex
        args    &b,&c,&d,&e
.ex     mend
* таблица операций
        macro
&lab    tabl    &a,&b,&c,&d,&e,&f,&g,&h,&i,&j,&k,&l,&m,&n
&lab    текст   п'&a(1)'
        конд    а(&a(n'&a))
        aif     (&b eq '').ex
        tabl    &b,&c,&d,&e,&f,&g,&h,&i,&j,&k,&l,&m,&n
.ex     mend
* оператор присваивания
        macro
&a      :=      &b
        сч      &b(1)
        aif     (n'&b lt 3).x
        &b(2)   &b(3)
        aif     (n'&b eq 3).x
        &b(4)   &b(5)
.x      зп      &a
        mend
* вызов БАНДИТ        
        macro
&l      bandt   &a,&b
        aif (&b eq '').x
        сч      &b
.x      мода    (M4)
        пв      &a.(М16)
        mend
* печать заголовка         
        macro
&l      headr
&l      мода    BASE
        уиа     hd&sndx.(М15)
        пб      wr&sndx
hd&sndx текст   п'ИМЯ         ДЛИНА  ШИФР ДАТА'
        конд    м40в'172'
wr&sndx пв      write(М16)
        mend
* чтение восьмеричного аргумента, опц. уст. флага
        macro
&l      getv    &dest,&flag        
&l      мода    BASE
        уиа     &dest.(М6)
        aif     (&flag eq '').x
        сч      &flag
        зп      flag
.x      пб      getval
        mend
BD      СТАРТ   '1'
BASE    equ     BD-1
        употр   BASE(М13)
        пам     1
        конд    в'54001011' ???
        конд    п'ВD    '
entry4  уиа     '72000'(M13)
        пб      START
        пам     2
entry7  уиа     (M13)
        уиа     '35072'(М3) ЫЫЫ
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
START   ноп
sysadr  :=      dfltad
        мода    BASE
        уиа     badarg(М12)
        пв      parse(М11)
        мод     argv
        сч      3
        уи      М2
Nopt    :=      0
        слиа    -1(М2)
        мода    BASE    
        пв      getopt(М16)
        пб      НАЧАЛО
cmdtyp  пам     1
CMD     пам     1
Ttext   конд    в'5'
Tchar   конд    в'4'
        конд    в'3'
Toctal  конд    в'2'
        конд    в'1'
Nopt    пам     1
getopt  слиа    1(М2)
cmdtyp  :=      ((M2) сда 64-3 счмр)   
Nopt    :=      (Nopt слц Е1)       
        мод     (М2)
        сч
        зп      CMD
        сч      cmdtyp
        нтж     Ttext
        пе      nottxt
cmd5    :=      CMD
nottxt  пб      (М16)
НАЧАЛО  opt     noargs,Ttext
        пе      wrong
        decod   BDdec
wrong   err     'ВD <НЕ ТО>'
noargs  err     'ВD, А ДАЛЬШЕ'
badarg  err     'НЕПР. СПИСОК ПАРАМЕТРОВ ДЛЯ ВD'
cmd5    пам     1
switch  пам     1
errmsg  сда     64-30
        уии     М15(М16)
        пв      write(М16)
        пб      pult
        конд    п'ОШ.N= '
        пам     1
        конд    m40b'172'
NЕW     ноп
switch  :=      B4
        мода    BASE
        уиа     wrong(М5)
        мода    BASE
        уиа     bufnam(M4)
        мода    BASE
        пв      NEWarg(М3)
newopt  opt     sw1,Ttext
        пе      badext
        мода    BASE
        уиа     newopt(М5)
        decod   NEWdec
badext  err     'НЕПР. ДОПОЛНЕНИЕ'
nooct   err     'НЕТ ВОСЬМЕРИЧНОГО НОМЕРА'
СИС     мода    BASE
        уиа     syspag(М6)
getval  opt     nooct,Toctal
        пе      nooct
(М6)    :=      CMD
        пб      (М5)
ТАБ     getv    tabpag
neww    rdsys   B0
        пб      donew
erased  текст   п'СТЕРТА БД-СИ'
        конд    п'СТЕМА0'в'172'
donew   prep    errmsg,bufnam
Mmyloc(M4) :=   (maxzon сда 64-6)
Marg(M4) :=     (maxzon сда 64-18 или bufloc)
        bandt   '23'
        пб      pult
ОРЕN    opt     wrong
bufnam  :=      CMD
        пб      openn
СЛО     opt     nooct
flag    :=      B2
EOT2    :=      CMD
        пб      (М5)
flag    конд    в'1'
openn   rdsys   bufpag
        prep    errmsg,bufnam
        bandt  '16'
        пб      pult
РUТ     ноп
switch  :=      E1
        пб      OP
DЕLЕТЕ  ноп
switch  :=      B2
        пб      OP
МОDIFУ  ноп
switch  :=      B3
        пб      OP
GЕТ     ноп
switch  :=      0
OP      opt     wrong
bufnam  :=      CMD
        opt     sw1
        case    (Toctal,octopt),(Ttext,txtopt)
octopt  ноп
ПАМ     :=      CMD
more    opt     sw1
        case    (Ttext,txtopt)
        пб      badext
txtopt  мода    BASE
        уиа     more(М5)
        decod   optdec
        пб      badext
БУФ     getv    bufpag
ДЛВ     getv    LEN,B0
СИМ     getv    EOT1,E1
sw1     swtch   switch,gett,putt,delet,modif,neww,openn
exit    пб      pult
gett    prep    errmsg,bufnam
        args    (0,Mmylen),(ПАМ,Mmyloc)
        bandt   '20'
        пб      exit
putt    swtch   flag,dlv,sym
        prep    errmsg,bufnam  <- СЛО
        args    (ПАМ,Mmyloc),(LEN,Mmylen),(EOT2,Marg)
        bandt   '33',E16
        пб      exit
sym     prep    errmsg,bufnam <- по умолчанию
        args    (ПАМ,Mmyloc),(LEN,Mmylen),(EOT1,Marg)
        bandt   '33',0
        пб      exit
dlv     prep    errmsg,bufnam <- длина указана
        args    (ПАМ,Mmyloc),(LEN,Mmylen)
        bandt   '17'
        пб      exit
delet   prep    errmsg,bufnam
        bandt   '21'
        пб      exit
modif   prep    errmsg,bufnam
        args    (ПАМ,Mmyloc),(0,Mmylen)
        мода    (M4)
        пв      '22'(M4) should be (M16) ???
        пб      exit
BDdec   tabl    NEW,OPEN,(OBTAIN,GET),(OBT,GET),PUT,DELETE
        tabl    (DЕL,DELETE),MODIFY,(MOD,MODIFY),GET,ИНФ,КТЛ
        пам     1
NEWdec  tabl    СИС,ТАБ,БУФ
        пам     1
optdec  tabl    ДЛВ,СИМ,СЛО
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
        case    (Ttext,filenm),(Toctal,nuzzzz),(Tchar,delim)
badarr  err     'ДЛЯ ВD НЕПР. ЗАДАН МАССИВ'
filenm  ноп
'77770' :=      ('1770'(М10)) д.б. 57770, имя текущего архива
'77771' :=      CMD           имя файла
'77772' :=      (B0 сда 64-10) как бы адрес считывания
'77773' :=      B20           макс длина
'77774' :=      '76000'       признак фиктивности
        пв      ffind(М11)
        уии     М15(М1)
        пб      wrexit неудача
* ffind возвращается через слово при успехе        
succes  ноп     , успех нахождения файла
1(M4)   :=      '77775'            ИС для Э70
2(M4)   :=      '77773'            длина
1(M4)   :=      (1(M4) и E18T1)    оставляем нузззз
0(M4)   :=      CMD                имя файла
        пб      (М3)
nuzzzz  ноп
(M4)    :=      BOCMP
1(M4)   :=      CMD
        opt     badarr,Toctal
        пе      badarr
        мода    
2(M4)   :=      CMD
        пб      (М3)        
delim   сч      CMD
        нтж     semi
        по      (М3)
        пб      badarr
unusd2  opt     (М5)
        case    (Ttext,filenm),(Toctal,nuzzzz),(Tchar,(M3))
        пб      badarr
КТЛ     opt     catal
        case    (Ttext,ctlarg)
        пб      badext
ctlarg  decod   KTLdec
        пб      badext
FULL    prep    errmsg
        args    (0,Marg)
        bandt   '34'
        headr
floop1  prep    errmsg
Madesc(M4) :=   (Mcurds(M4))
        сч      Mcurky(M4)
        пе      notlst
        мода    Mcurky+1(M4)
        уиа     (М16)
        сч      B21
        пб      errmsg
notlst  зп      Mkey(M4)
        args    (B3,Marg),(0,Mmylen),(B0,Mmyloc)
        bandt   '20'
        уи      М15
        пв      write(М16)
        prep    exit
        args    (B3,Marg)
        bandt   '34'
        пб      floop1
KTLdec  tabl    (ПОЛН,FULL)
        пам     1
catal   prep    errmsg
        args    (0,Marg)        
        bandt   '34'
floop   зп      fname
        мода    fname
        уиа     (М15)
        пв      write(М16)
        prep    exit
        args    (B3,Marg)
        bandt   '34'
        пб      floop
ИНФ     opt     badext
        case    (Ttext,inf)
        пб      badext
inf     headr
        prep    errmsg,CMD
        args    (B3,Marg),(0,Mmylen),(B0,Mmyloc)
        bandt   '20'
        уи      М15
        пв      write(М16)
        пб      pult
wrexit  пв      write(М16)
        пб      pult
fname   пам     1
        конд    м40в'172'
dfltad  конд    в'74000'
sysadr  конд    п'БАН0КА'
E1      конд    п'1'
B4      конд    п'4'
B0      конд    в'0'
bandit  конд    п'БАНДИТ'
B2      конд    п'2'
B3      конд    п'3'
E16     конд    в'100000'
B20     конд    в'20'
E18T1   конд    в'777777'
BOCMP   конд    п'ВОСЬМР'
semi    конд    п';'
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

argv    equ     '76015'
parse   equ     '76016'
write   equ     '76005'
ffind   equ     '76006'
pult    equ     '76002'
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
