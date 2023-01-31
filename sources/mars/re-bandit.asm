        macro
&l      пино    &a,&r
&l      utc     &a
        v1m     &r
        mend
        macro
&l      call    &a,&r
&l      мода    &a
        пв      &r
        mend
БАНДИТ	СТАРТ	1
	употр	бандит-1(4)
	конд	п'БАНДИТ'
	конд	в'1162'
nuzzzz	пам	1
rtlen	пам	1
curdsc	пам	1
bdbuf	пам	2
key	пам	1
erradr	пам	1
adescr	пам	1
myloc	пам	1
bdtab	пам	1
mylen	пам	1
     	счи	М16
	пб	open
	счи	М16
	пб	putfix
	счи	М16
	пб	get
	счи	М16
	пб	delete
	счи	М16
	пб	modif
	счи	М16
	пб	newbd
savm4	пам	1
frebas	пам	1
root	пам	1
arg	пам	1
	конд	п'171167'
	счи	М16
	мода	rdroot
	уиа	(М16)
	пб	init
	зп	tmp1
	пб	putvar
	сч	tablen
	пб	info
recdsc	пам	1
addr	пам	1
curkey	пам	1
	текст	п'НЕТ ТЕКУЩЕЙ ЗАПИСИ' не используется
arch	конд	в'1400030'    главный каталог архива БД
	счи	М16
	пб	entr44
info	по	empty
	счи	М16
	call	init,(М16)
	мод	arg
	пб	first
first	сч	base
	пб	setadr
k1	сч	tablen
	пб	пиши
k2	сч	recdsc
	пб	пиши
k3	сч	curkey
	пб	A00062
k4	сч	key
	по	notfnd
	мод	base
	уиа	(М16)
loop1	сч	(М16)
	по	notfnd
	нтж	key
	по	fnd
	мода	loop1
	цикл	(М16)
fnd	счи	М16
	пб	setadr
A00062	по	noprev
	сч	addr
	сда	64+24
	пе	пиши
	мод	addr
	сч	1
	сда	64+20
	пе	пиши
	сч	addr
	слц	B2
	зп	addr
	сч	base
	слц	tablen
	нтж	addr
	по	nomore
A00072	мод	addr
	сч	1
	зп	recdsc
	мод	addr
	сч	
	зп	key
	зп	curkey
	пб	rtexit
nomore	зп	recdsc
	пб	nonext
setadr	зп	addr
	пб	A00072
пиши	по	noprev
* знаем, куда писать, но не знаем, сколько
putvar	счи	М16
	call	init,(М16)
	счи	М6
	зп	savm6
	сч	myloc
	слц	mylen
	зп	mylast
	уиа	'72000'(М16)
	сч	tmp1
	сда	64+15
	пе	loop4
	мода	
	сч	arg
	сда	64+8
	пе	twochr
	сч	arg
twochr	сда	64-40
	зп	tmp2
	уиа	-4(М5)
размнж	сда	64+8
	или	tmp2
	мода	размнж
	цикл	(М5)
	зп	tmp2
A00117	уиа	-5(М5)
	мод	mylast
	сч	'6000'(М16)
	нтж	tmp2
A00121	сда	64+40
	уи	М6
	мода	goteol
	пио	(М6)
	счмр	
A00124	мода	A00121
	цикл	(М5)
A00125	мода	A00117
	цикл	(М16)
	пб	noendl
goteol	счмр	
	нтж	tmp2
	зп	D00234
	сч	arg
	сда	64+8
	по	A00142
	сч	arg
	сда	64-40
	мода	A00137
	пио	(М5)
	нтж	D00234
	сда	64+40
	по	A00142
	сч	D00234
	пб	A00124
A00137	мод	mylast
	мода	'6001'(М16)
	нтж	
	сда	64+40
	пе	A00125
	слиа	1(М16)
A00142	слиа	'6001'(М16)
	счи	М16
	слц	mylen
	зп	mylen
	сч	
	мод	tmp1
	пб	here0
here0	зп	arg
	пб	alloc2
here1	зп	arg
	пб	newbd
loop4	мод	mylast
	сч	'6000'(М16)
	нтж	arg
	по	A00142
	мода	loop4
	цикл	(М16)
	пб	noewrd
getelt	сч	adescr
	по	zerkey
	мод	adescr
	сч	
rdelt	зп	descr
	и	E10T1
	зп	curzon
	пе	chkzon
	сч	bdtab
	зп	curbuf
	пб	A00172
chkzon	мод	bdbuf
	нтж	2
	по	samzon
	сч	bdbuf
	сда	64-20
	или	curzon
	или	E40
	слц	nuzzzz
	зп	ИСЭ70
	Э70	ИСЭ70
samzon	сч	bdbuf
	зп	curbuf
	мод	curbuf
	сч	
	нтж	root
	пе	err2
A00172	сч	descr
	и	E20T11
	сда	64+10
	зп	mylast
	слц	curbuf
	зп	tmp1
	мод	tmp1
	сч	
	по	norecd
	зп	tmp2
	зп	chain
	и	E10T1
	слц	curbuf
	зп	adata
	сч	tmp2
	и	E20T11
	сда	64+10
	зп	itmlen
	пб	(М5)
curbuf	пам	1
длина	пам	1
mylast	пам	1
base	пам	3
ИСЭ70	пам	1
tmp1	пам	1
tablen	пам	1
descr	пам	1
curzon	пам	2
B1777	конд	в'1777'
E21T11	конд	в'7776000'
E20T11	конд	в'3776000'
offset	пам	1
itmlen	пам	1
adata	пам	1
retadr	пам	1
savrk	пам	1
savm5	пам	1
tmp2	пам	1
e70msk	пам	1
goto	пам	1
D00234	пам	2
NU40	конд	м12в'40'
NU57	конд	м12в'57'
V2048	конд	х'2048'
err	сч	
	зп	curkey
	зп	arg
	счи	М16
	уии	М5(М16)
	сли	М16(М5)
	сли	М16(М5)
	сли	М16(М4)
	слиа	errbas(М16)
	мод	erradr
	пб	
zerkey	уиа	1(М16)
	пб	err
err2	уиа	2(М16)
	пб	err
norecd	уиа	3(М16)
	пб	err
nofile	уиа	4(М16)
	пб	err
corr1p	уиа	5(М16)
	пб	err
overfl	уиа	6(М16)
	пб	err
err7	уиа	7(М16)
	пб	err
notfnd	уиа	8(М16)
	пб	err
presnt	уиа	9(М16)
	пб	err
noendl	уиа	10(М16)
	пб	err
noewrd	уиа	11(М16)
	пб	err
toolng	уиа	12(М16)
	пб	err
corupt	уиа	13(М16)
	пб	err
empty	уиа	14(М16)
	пб	err
noprev	уиа	15(М16)
	пб	err
nonext	уиа	16(М16)
	пб	err
	уиа	21(М16)
	пб	err
fail	уиа	20(М16)
	пб	err
open	call	enter,(М16)
	мода	open2
	уиа	(М5)
chkdsk	сч	NU40
	Э50	'105'
	нтж	V2048
	по	is2048  пульт на 2048
	сч	arch
	зп	root    пользуемся НУ 40
	пб	chkcnt
is2048	сч	arch
	зп	root
	или	NU57    меняем на НУ 57
	пб	chkcnt
open2	счи	М4
	слц	aroot
	зп	myloc
	call	getget,(М16)
rdroot	сч	root
	мода	exit
	уиа	(М5)
	пб	chkcnt
chkcnt	и	E18T1
	зп	nuzzzz
	сч	root
	сда	64+18
	зп	rtlen
	сч	bdtab
	зп	curbuf
	сда	64-20
	слц	nuzzzz
	зп	e70msk
	или	E40
	зп	ИСЭ70
	мод	bdtab
	уиа	(М16)
	сч	(М16)
	нтж	root
	или	2(М16)
	по	хорошо  ключ совпадает и слово 2 = 0
	Э70	ИСЭ70   перечитываем
	мод	bdtab
	уиа	(М16)
	сч	(М16)
	нтж	root
	или	2(М16)
	пе	corr1p  испорчен каталог
хорошо	зп	curzon
	сч	3(М16)
	и	B1777
	слц	bdtab
	зп	base
	сч	rtlen
	нтж	E10T1
	слц	E1
	слц	bdtab
	зп	frebas
	мод	frebas
	сч	-1
	зп	tablen
	пб	(М5)
flush	Э70	e70msk
rtexit	мод	goto
	уиа	(М5)
	пино	(M5),(М5)
exit	рж	savrk
	мод	savm6
	уиа	(М6)
	мода	
	мод	savm5
	уиа	(М5)
	уии	М16(М4)
	мод	savm4
	уиа	(M4)
	мод	retadr(М16)
	пб	
delete	call	init,(М16)
	сч	key
	по	delcur
	мод	base
	уиа	(М16)
A00346	сч	(М16)
	по	err7
	нтж	key
	по	found
	мода	A00346
	цикл	(М16)
found	сч	1(М16)
	зп	descr
shrink	сч	2(М16)
	зп	(М16)
	слиа	1(М16)
	пе	shrink
	сч	tablen
	вч	B2
	зп	tablen
	мод	frebas
	зп	-1
	сч	descr
	мода	flush
	уиа	(М6)
delet0	call	rdelt,(М5)
delet1	сч	itmlen
	сда	64-10
	мод	curbuf
	слц	1
	зп	D00234
	и	B1777
	уи	М16
	сч	adata
	вч	curbuf
	зп	offset
	слиа	-2(М16)
loop3	мод	curbuf
	сч	2(М16)
	по	stays
	и	B1777
	вч	offset
	по	stays
	мод	curbuf
	сч	2(М16)
	слц	itmlen
	мод	curbuf
	зп	2(М16)
stays	слиа	-1(М16)
	пино	loop3,(М16)
	сч	D00234
	и	E21T11
	сда	64+10
	слц	curbuf
	зп	offset
	вч	itmlen
	зп	tmp2
	вчоб	adata
	уи	М16
	счи	М16
	по	nomove
mvloop	мод	tmp2
	сч	-1(М16)
	мод	offset
	зп	-1(М16)
	слиа	-1(М16)
	пино	mvloop,(М16)
nomove	сч	D00234
	и	B1777
	нтж	mylast
	по	A00432
	сч	
	мод	tmp1
	зп	
	сч	D00234
	или	E22
A00415	мод	curbuf
	зп	1
	сч	frebas
	уи	М16
	мод	curzon
	сч	(М16)
	слц	itmlen
	мод	curzon
	зп	(М16)
	сч	curbuf
	сда	64-20
	или	curzon
	слц	nuzzzz
	зп	ИСЭ70
	Э70	ИСЭ70
	сч	chain
A00426	сда	64+20
	пе	delet0
	зп	curdsc
	пб	(М6)
delcur	мода	delet1
	уиа	(М5)
	пб	getelt
A00432	сч	itmlen
	слц	E1
	зп	itmlen
	сч	D00234
	вч	E1
	пб	A00415
enter	зп	retadr
	счи	М5
	зп	savm5
	уии	М5(М16)
	пб	A00446
init	зп	retadr
	счи	М5
	зп	savm5
	мод	bdtab
	сч	
	нтж	root
	уии	М5(М16)
	по	A00446
	сч	e70msk
	или	E40
	зп	ИСЭ70
	Э70	ИСЭ70
A00446	счрж	
	зп	savrk
	ржа	'3'
	счи	М6
	зп	savm6
	сч	
	зп	goto
	пб	(М5)
alloc	счи	М16
	зп	goto
	пб	alloc2
putfix	call	init,(М16)
alloc2	уиа	(М6)
	сч	
	зп	D01037
	мода	
	сч	frebas
	уи	М16
	мод	curzon
	сч	(М16)
	зп	mylast
	сч	mylen
	зп	origln
	слц	E1
	зп	mylen
	вч	mylast
	по	notfit
getzon	мод	curbuf
	уиа	(М5)
	сч	(М5)
	нтж	root
	по	same
rdzon	сч	curbuf
	сда	64-20
	или	curzon
	или	E40
	слц	nuzzzz
	зп	ИСЭ70
	Э70	ИСЭ70
	сч	(М5)
	нтж	root
	пе	corupt
same	сч	mylen
	сда	64-10
	зп	D00234
	сч	1(М5)
	вч	D00234
	зп	D00234
	и	E21T11
	сда	64+10
	зп	tmp2
	слц	curbuf
	зп	tmp1
	сч	mylen
	уи	М16
	пино	A00510,(М6)
	слиа	-1(М16)
	сч	tmp1
	слц	E1
	зп	tmp1
A00510	счи	М16
	по	cidone
ciloop	мод	myloc
	сч	-1(М16)
	мод	tmp1
	зп	-1(М16)
	слиа	-1(М16)
	пино	ciloop,(М16)
cidone	сч	mylen
	сда	64-10
	или	tmp2
	или	D01037
	зп	tmp2
	сч	1(М5)
	и	E22
	по	A00530
	сч	D00234
	и	B1777
	уи	М16
	слиа	-2(М16)
A00523	мод	curbuf
	сч	2(М16)
	по	A00640
	слиа	-1(М16)
	пино	A00523,(М16)
	сч	D00234
	нтж	E22
	зп	D00234
A00530	сч	D00234
	слц	E1
	зп	1(М5)
	и	B1777
	зп	D00234
	слц	curbuf
	зп	offset
	сч	tmp2
	мод	offset
	зп	
	сч	mylen
	слц	E1
	зп	mylen
	сч	D00234
A00537	сда	64-10
	или	curzon
	зп	descr
	мод	frebas
	уиа	(М16)
	мод	curzon
	сч	(М16)
	вч	mylen
	мод	curzon
	зп	(М16)
	мода	date
	пио	(М6)
	сч	curbuf
	сда	64-20
	или	curzon
	слц	nuzzzz
	зп	ИСЭ70
	Э70	ИСЭ70
	сч	descr
	сда	64-20
	зп	D01037
	сч	D00637
	зп	mylen
A00553	слиа	-1(М6)
	пино	fragm,(М6)
	мода	overfl
	уиа	(М6)
	сч	D01037
	пб	A00426
A00557	сч	1(М16)
	пб	fail
origln	пам	1
datmsk	конд	в'0000377774000000'
date	Э50	'114'
	сбр	datmsk
	зп	mylast
	Э50	'100'
	сда	64+30
	сда	64-15
	или	mylast
	или	origln
	мод	tmp1
	зп	-1
	сч	curbuf
	сда	64-20
	или	curzon
	слц	nuzzzz
	зп	ИСЭ70
	Э70	ИСЭ70
	сч	key
	пе	ищи
	сч	descr
	зп	curdsc
	мод	adescr
	зп	
	пб	flush
********************        
нашли	сч	descr
	мода	presnt
	уиа	(М6)
	пб	delet0
ищи	мод	base
	уиа	(М16)
ищем	сч	(М16)
	по	нету
	нтж	key
	по	нашли
	мода	ищем
	цикл	(М16)
нету	счи	М16
	слц	B2
	нтж	frebas
	по	A00557
	сч	key
	зп	(М16)
	сч	descr
	зп	1(М16)
	зп	recdsc
	сч	tablen
	слц	B2
	зп	tablen
	мод	frebas
	зп	-1
	сч	
	зп	2(М16)
	пб	flush
long	сч	myloc
	слц	mylen
	вч	E1
	зп	myloc
	мод	rtlen
	уиа	(М6)
fragm	мод	frebas
	сч	-1(М6)
	вч	B2
	пе	A00553
	слц	E1
	зп	mylast
	слиа	-1(М6)
	вч	mylen
	пе	A00631
	сч	myloc
	вч	mylen
	слц	E1
	зп	myloc
	счи	М6
	уиа	(М6)
	пб	A00653
A00631	сч	mylen
	вч	mylast
	зп	D00637
	сч	mylast
	зп	mylen
	вчоб	myloc
	зп	myloc
	счи	М6
	слиа	1(М6)
	пб	A00653
savm6	пам	1
D00637	пам	1
A00640	сч	tmp2
	мод	curbuf
	зп	2(М16)
	сч	D00234
	зп	1(М5)
	счи	М16
	слц	B2
	пб	A00537
notfit	сч	mylen
	вч	B1775
	по	long
	уиа	-1(М5)
loop2	слиа	1(М5)
	счи	М5
	нтж	rtlen
	по	long
	сч	mylen
	мод	frebas
	вч	(М5)
	по	loop2
	счи	М5
A00653	зп	curzon
	пе	zonne0
	сч	bdtab
	зп	curbuf
	пб	getzon
zonne0	сч	bdbuf
	зп	curbuf
	уи	М5
	сч	2(М5)
	нтж	curzon
	пе	rdzon
	пб	getzon
modif	call	init,(М16)
	счи	М6
	зп	savm6
newbd	call	enter,(М16)
	мода	newbd2
	уиа	(М5)
	пб	chkdsk
newbd2	сч	myloc
	зп	длина
	сда	64+10
	по	меньше
	сч	B1000
	зп	длина
меньше	счи	М4
	слц	aarg
	зп	myloc
	сч	E1
	зп	mylen
	call	alloc,(М16)
	сч	bdtab
	уи	М16
	сда	64-20
	или	arg
	или	lenmsk
	нтж	lenmsk
	зп	ИСЭ70
	сч	arg
	зп	(М16)
	сда	64+18
	зп	mylast
	слц	длина
	зп	tmp1
	вчоб	B2000
	зп	tmp2
	сда	64-10
	или	B3
	зп	1(М16)
	сч	tmp1
	сда	64-10
	или	tmp2
	зп	3(М16)
	сч	
	зп	arg
	зп	2(М16)
	мод	tmp2
	зп	(М16)
	счи	М16
	слц	B2000
	вч	mylast
	мод	mylast
	уиа	(М16)
	зп	tmp2
	сч	B1775
роспис	мод	tmp2
	зп	-1(М16)
	слиа	-1(М16)
	пино	роспис,(М16)
	вч	tmp1
	вч	E1
	мод	tmp2
	зп	
	сч	
	мод	tmp2
	зп	-1
	Э70	ИСЭ70
	сч	word1
	мод	bdtab
	зп	1
wrzon	сч	mylast
	слц	E15T1
	нтж	E16
	по	done
	зп	mylast
	мод	bdtab
	зп	2
	слц	ИСЭ70
	зп	tmp1
	Э70	tmp1
	пб	wrzon
done	пб	exit
********************
getget	счи	М16
	зп	goto
	пб      get1
get	call	init,(М16)
get1	сч	key
	по	nokey
	мод	base
	уиа	(М16)
chkkey	сч	(М16)
	по	notfnd
	нтж	key
	по	found2
	мода	chkkey
	цикл	(М16)
found2	сч	1(М16)
	зп	curdsc
	мода	decode
	уиа	(М5)
	пб	rdelt
nokey	call	getelt,(М5)
decode	сч	arg
	и	B3
	по	decod0
	уи	М5
	сч	
	зп	arg
	мод	adata
	сч	
	мода	decod1
	пб	-1(М5)
decod1	рзб	dtmsk1
	пб	rtexit
	и	E15T1
	пб	rtexit
	зп	mylast
	рзб	dtmsk2
	или	EOT
	зп	datbuf
	сч	mylast
	сда	64-33
	рзб	dtmsk3
	или	sp
	зп	lenbuf
	сч	mylast
	сда	64-15
	рзб	uidmsk
	или	spsp
	зп	uidbuf
	сч	key
	зп	nambuf
	уии	М16(М4)
	слиа	nambuf(М16)
	счи	М16
	пб	rtexit
decod0	сч	mylen
	пе	chklen
docopy	сч	adata
	слц	E1
	зп	adata
	сч	itmlen
	вч	E1
	зп	itmlen
next	уи	М16
	счи	М16
	по	A01006
cpyout	мод	adata
	сч	-1(М16)
	мод	myloc
	зп	-1(М16)
	слиа	-1(М16)
	пино	cpyout,(М16)
A01006	сч	myloc
	слц	itmlen
	зп	myloc
	сч	chain
	и	E40T21
	по	rtexit
	сда	64+20
	мода	next
	уиа	(М5)
	пб	rdelt
chklen	мод	adata
	сч	
	и	E15T1
	вчоб	mylen
	по	docopy
	пб	toolng
entr44	call	init,(М16)
	сч	adescr
	call    rdelt,(М5)
	сч	adescr
	и	E40T21
	по	A01026
	сч	adata
	слц	E1
	зп	adata
	сч	itmlen
	вч	E1
	зп	itmlen
A01026	сч	itmlen
	уи	М16
	счи	М16
	по	A01034
coloop	мод	adata
	сч	-1(М16)
	мод	myloc
	зп	-1(М16)
	слиа	-1(М16)
	пино	coloop,(М16)
A01034	сч	chain
	и	E40T21
	сда	64+20
	зп	adescr
	пб	rtexit
D01037	пам	1
chain	пам	1
nambuf	пам	1
	конд	п'      '
lenbuf	пам	1
uidbuf	пам	1
datbuf	пам	1
	конд	m40в'33'
E40T21	конд	в'0017777774000000'
errbas	пам	3
ET1	текст	п'НУЛЕВОЙ УКЛЮ'
	конд	п'Ч    0'в'172'
ET2	текст	п'В БД ЗАТЕРТ '
	конд	п'ЛИСТ 0'в'172'
ET3	текст	п'ЭТА ЗАПИСЬ У'
	конд	п'БРАНА0'в'172'
ET4	текст	п'НЕТ ТАКОЙ БД'
	конд	п'     0'в'172'
ET5	текст	п'ЗАТЕРТ 1Й ЛИ'
	конд	п'СТ БД0'в'172'
ET6	текст	п'ПЕРЕПОЛНЕНИЕ'
	конд	п' БД  0'в'172'
ET7	текст	п'НЕТ ТАКОЙ ЗА'
	конд	п'ПИСИ 0'в'172'
ET8	текст	п'ТАКОЙ ЗАПИСИ'
	конд	п' НЕТ 0'в'172'
ET9	текст	п'ЭТО ИМЯ УЖЕ '
	конд	п'ЕСТЬ 0'в'172'
ET10	текст	п'НЕТ СИМВОЛА-'
	конд	п'КОНЦА0'в'172'
ET11	текст	п'НЕТ СЛОВА-КО'
	конд	п'НЦА  0'в'172'
ET12	текст	п'ДЛИНА ЗАПИСИ'
	конд	п'>ДЛВ 0'в'172'
ET13	текст	п'В БД ЗАТЕРТ '
	конд	п'ЛИСТ 0'в'172'
ET14	текст	п'В КТЛ НЕТ ЗА'
	конд	п'ПИСЕЙ0'в'172'
ET15	текст	п'НЕТ ПРЕД ЗАП'
	конд	п'ИСИ  0'в'172'
ET16	текст	п'НЕТ СЛЕД ЗАП'
	конд	п'ИСИ  0'в'172'
B2	конд	п'2'
E10T1	конд	в'1777'
E40	конд	m39в'1'
aroot	конд	a(root)
E18T1	конд	в'777777'
E1	конд	в'1'
E22	конд	m21в'1'
B1775	конд	в'1775'
B1000	конд	в'1000'
aarg	конд	a(arg)
lenmsk	конд	в'1777000000'
B2000	конд	в'2000'
B3	конд	в'3'
word1	конд	м20в'1'в'2'
E15T1	конд	в'77777'
E16	конд	m15в'1'
dtmsk1	конд	п'03 1  '
dtmsk2	конд	п'3 1  0'
EOT	конд	в'172'
dtmsk3	конд	п'777770'
sp	конд	п' '
uidmsk	конд	п'0    0'
spsp	конд	п' 0000 '
	пам	385
	конд	в'0000000000001777'
	пам	3
	конд	в'0600005600000000'
	пам	3
	конд	в'0640006200000000'
	пам	3
	конд	в'0720007000000000'
M16     equ     '16'
M6      equ     6
M5      equ     5
M4      equ     4
	ФИНИШ
