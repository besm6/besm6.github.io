
# Диалоговая система Джин

Джин исполняет следующие приказы

```
00. СПИсок (исполняется только в Школе)
01. ИНФормация  13. ВЦП         25. БЮДжет
02. ШКОла       14. ВРЕмя       26. ВОПрос
03. ПОЧта       15. ДАТа        27. БИБлиотека
04. НОВости     16. КАЛендарь   28. ГРАфика
05. БЭСм        17. ПЕЧать      29. АРИфмометр
06. ТЕРминалы   18. ЭКРан       30. АДМинистратор
07. ЛЕНты       19. ИДИ         31. ИГРа
08. ДИСки       20. ПАУза       32. РЕДактор
09. КТО         21. ВЫЗов       33. СВЯзь
10. ГДЕ         22. СЕТь        34. ЖУРнал
11. ШИФр        23. ПАКет       35. СИСтема
12. ПАРоль      24. СТАтистика  36. КОНец
                                37. ВЫХод
```

Имена приказов можно сокращать до трех букв. Наберите приказ (или
его номер), потом сделайте исполнение - нажмите (ЕТХ) и вы получите
описание приказа и его операндов.

# Приказ 'информация'

По приказу 'информация' выдается список приказов сервисной диалоговой
системы 'Джин' с кратким пояснением назначения каждого приказа и структуры операндов.
В прямых скобках показаны необязательные операнды.

Некоторые общие сведения о Джине:

* Практическая работа над системой Джин начата 12.01.77.
* На 03.07.83 Джин содержит около 156 (дес) листов готовых программ и 18 листов служебной информации (в том числе 2 листа почты).

Джин содержит следующие блоки:

|   а) | пуск   | основной блок      | 11212 |
|   б) | стат   | блок статистики    | 11313 |
|   в) | школа  | блок для обучения  | 11704 |
|   г) | мост   | блок-переключатель | 07416 |
|  *д) | вопрос | блок вопросов      | 12536 |
|  *е) | линг   | блок лингвистики   | 13274 |
|  *ж) | синоп  | блок синоптика     | 22073 |
|  *з) | журнал | блок протокола     | 17074 |
|  *и) | редак  | блок редактора     | 21131 |
|  *к) | игра   | блок игр           | 71446 |
|  *л) | сист   | системный блок     | 13433 |
|  *м) | админ  | служебный блок     | 30506 |
|  *н) | связь  | блок связи         | 17222 |
|  *о) | библ   | блок библиотеки    | 42444 |
|  *п) | граф   | графический блок   | 03036 |
|  *р) | ариф   | арифметич. блок    | 24555 |
|  *с) | план   | блок планирования  | 37464 |
|   т) | тираж  | распечатка инстр.  | 04455 |
|      |        | Всего              | 470071 |

Звездочкой отмечены автономные блоки. В этих блоках действуют
особые правила и, в частности, имеются свои приказы 'ИНФ' и 'ШКО'.
Объем блоков указан в машинных словах в восьмеричной системе.

Пожелания и замечания о работе Джина следует направлять почтой
на имя администратора (шифр 4177).

# Приказ 'школа'

По приказу 'школа' исполнение приказов отменяется и заменяется
их объяснением.
Форма обращения к приказу (знаком минус отделена необязательная
часть имени приказа):

    ШКО-ла  [ <список приказов> ]

Здесь <список приказов> есть последовательность приказов, перечисленных
через запятую или через знак минус. Приказы можно задавать в буквенной
или цифровой форме. Перечисление через знак минус понимается от-до,
включительно.

Если операнд приказа школа не указан, то по умолчанию исполняется
приказ школы 'список'. Если же список приказов задан, то после выдачи
описания всех заказанных приказов автоматически происходит выход
из школы.

# Приказ 'почта'

По приказу 'почта' можно отправить письмо любому пользователю
БЭСМ-6 и просмотреть пришедшую почту.

## 1. Отправка почты

Форма обращения к приказу в этом случае следующая:

    ПОЧ-та <шифр> [ <число дней хранения> ] [ уведомить ]

Здесь <шифр> - четырехзначный шифр адресата в смысле ОС Диспак.
Если вы не знаете шифра адресата, воспользуйтесь приказом 'шифр'.
Число дней хранения письма - необязательный параметр. При его отсутствии,
по умолчанию, принимается 7 дней.

Если при отправке почты заказано уведомление, то после прочтения
письма адресатом в адрес отправителя автоматически направляется
письмо-уведомление о вручении.

Максимальный объем письма - 56 машинных слов. По истечении срока
хранения сохранность письма не гарантируется.

Рекомендуется набирать текст письма в режиме 'оff linе', используя
для перехода на новую строку клавишу 'linе fееd'. Закончив набор,
нажмите клавишу 'ЕТХ', переведите маркер на первую строку письма
и, нажав клавишу 'sеnd', передайте весь текст в машину.

## 2. Получение почты

Форма обращения в этом случае следующая:

    ПОЧ-ту  ДАЙ  [ <номер письма>  ]

Здесь <номер письма> необязательный параметр, по умолчанию, равный
единице.

Отметим, что Джин сразу уведомляет пользователя о наличии почты,
но для прочтения ее нужно запросить. Почту можно запрашивать и так:

    ДАЙ  ПОЧту  [ <номер письма> ]

# Приказ 'новости'

По приказу 'новости' (операндов нет) выдается информация о текущих
системных изменениях, дополнениях (если они есть) и других событиях,
которые могут представлять интерес для пользователей БЭСМ-6.

# Приказ 'БЭСМ'

Приказ 'бэсм'  служит для выяснения текущего состояния машины.
Форма обращения:

    БЭС-м  [ <список параметров> ]

Здесь список параметров - перечисление через запятую запрашиваемых
параметров. Параметры могут быть:

 * РЕС-урсы - выдается число (8) свободных листов ОЗУ, число свободных трактов (в том числе на МБ и на МД), число свободных кусков на рабочем диске.
 * РЕЖ-имы - выдаются установленные режимы (отладочный, счетный, режимы 'ква' или 'мин').
 * ВЫВ-од  - выдаются сведения о текущем состоянии выводных устройств и числе зон, подготовленных для вывода.
 * ВСЕ - заказываются сразу все параметры.

# Приказ 'терминалы'

По приказу 'терминалы' выдается информация о сети терминалов и
их занятости в настоящий момент.
Форма обращения:

    ТЕР-миналы  [ ЗАН-ятые, СВО-бодные, ВСЕ ]

Если операнд приказа не задан, то выдается информация о свободных
в настоящий момент терминалах. Указав операнд, можно получить сведения
о занятых или всех терминалах. Если некоторый терминал не присутствует
ни в списке занятых, ни в списке свободных терминалов, то это означает,
что ОС Диспак в настоящий момент этот терминал не обслуживает.

# Приказ 'ленты'

По приказу 'ленты' выдается информация о магнитофонах и установленных
на них бобинах.
Форма обращения:

    ЛЕН-ты  [ <номер бобины> ]

Если <номер бобины> не указан, то выдается информация о занятости
всех магнитофонов и стоящих на них бобинах. Если около номера бобины
стоит звездочка, то это означает, что данный номер бобины присвоен
с пульта оператором.

Если <номер бобины> указан, то выдается подробная информация только
об этой бобине.

# Приказ 'диски'

По приказу 'диски' (операндов нет) выдается информация о дисководах
и установленных на них пакетах. Указывается тип пакета: системный,
общий или индивидуальный. Для несистемных пакетов указывается число
задач, работающих в настоящий момент с этим пакетом.

# Приказ 'кто'

По приказу 'кто' выдается фамилия пользователя по его шифру.
Форма обращения:

    КТО  <шифр>

Здесь <шифр> - обязательный операнд, это четырехзначный шифр пользователя
в смысле ОС Диспак. Результат - фамилия пользователя в том виде,
как она занесена в список фамилий. Фамилия пользователя занимает
2 машинных слова. Окончания длинных фамилий или инициалы из-за этого
могут отсутствовать. Если обнаружатся ошибки в фамилии - обратитесь
к старшему оператору.

Имеются еще два варианта приказа 'кто': ```КТО Я``` и ```КТО ТЫ```.

# Приказ 'где'

По приказу 'где' выдается текущее или последнее местонахождение
пользователя. Форма обращения:

    ГДЕ  <шифр 4 знака>

Имеется вариант приказа: ```ГДЕ ВСЕ```.

# Приказ 'шифр'

По приказу 'шифр' по фамилии пользователя определяется и выдается
его шифр в ОС Диспак.
Форма обращения:

    ШИФ-р  [ <фамилия> ] [ <номер лаборатории> ]

Фамилия задается в полной или сокращенной форме без инициалов
в именительном падеже. Указание номера лаборатории (первые две цифры
шифра) сокращает объем поиска. Если номер лаборатории не указан,
поиск будет производиться среди всех пользователей. Если фамилия
задана сокращенно - начальными буквами, то будут выданы все пользователи,
фамилии которых начинаются так же. Если фамилия не задана совсем,
получим всех пользователей указанной лаборатории.

# Приказ 'пароль'

По приказу 'пароль' устанавливается и снимается пароль пользователя
джина. Форма обращения:

    ПАР-оль  [<пароль>]

Здесь <пароль> - любые шесть символов или менее. В составе пароля
не должно быть пробелов. Если <пароль> не задан, то пароль снимается.
Пароль необходим, чтобы никто не мог прочесть вашу почту, не мог
отправить почту от вашего имени и др.

# Приказ 'ВЦП'

По приказу 'вцп' выдается использованное или оставшееся время
в задаче 'джин', которая вас обслуживает. Отметим здесь, что в стандартном
паспорте задачи 'джин' заказывается 21 секунда.
Форма обращения:

    ВЦП  [ ИСП, ОСТ ]

Если операнд приказа не задан, выдается оставшееся ВЦП. Если операнд
задан - заметьте, что косых скобок нет и, значит, операнд должен
даваться в том виде, как он записан - выдается использованное время
в секундах и сотых долях секунды.

# Приказ 'время'

По приказу 'время' выдается текущее машинное время БЭСМ-6. Оно
может несколько отличаться от истинного астрономического времени.
Если время БЭСМ-6 сильно отличается от истинного - свяжитесь с оператором
и потребуйте внесения поправки.

# Приказ 'дата'

Форма обращения:

    ДАТ-а  [ <дата> ]

Если <дата> не заказана, то выдается текущая дата и день недели
по данным ОС Диспак. Если обнаружится несоответствие - срочно обратитесь
к старшему оператору.

Если <дата> заказана, то выдается соответствующий ей дань недели. <Дата> задается в виде: ДД.ММ.ГГ

# Приказ 'календарь'

По приказу 'календарь' выдается календарь на три месяца, ближайшие
к указанной дате. Календарь верен для всех дат двадцатого столетия.
Дореволюционные даты даются по новому стилю. Заказанная дата отмечается
на календаре круглыми скобками.
Форма обращения:

    КАЛ-ендарь  [ <дата> ]

Под ближайшими понимаются текущий, предыдущий и последующий месяцы.
Если дата не указана, то принимается текущая дата.

Дата задается в виде:  ДД.ММ.ГГ

Нули можно не указывать.

Программу для этого приказа разработала О.А.Черненко.

# Приказ 'печать'

По приказу 'печать' можно включить и выключить печать АЦПУ. Если
приказ печать ни разу не исполнялся, то задача джин работает инкогнито.
Форма обращения:

    ПЕЧ-ать  [ ДА, НЕТ ]

Явно заданный операнд соответственно включает или выключает печать
протокола. Если операнд приказа не задан, то состояние печати меняется
на противоположное.

Под печатью протокола понимается печать приказов пользователя
и ответов Джина.

Приказ 'печать' не зависит от приказа 'экран'.

# Приказ 'экран'

Приказом 'экран' можно включить и выключить выдачу ответов джина
на терминал.
Форма обращения:

    ЭКР-ан  [ ДА, НЕТ ]

Явно заданный операнд соответственно включает и выключает выдачу
ответов на терминал. Если операнд приказа не задан, то состояние
выдачи на терминал меняется на противоположное.

Приказ 'экран' не зависит от приказа 'печать'. Если, например,
последовательно подать приказы 'печать да' и 'экран нет', то ответы
Джина не будут видны на экране, но будут печататься.

# Приказ 'иди'

Приказ 'иди' регулирует шаг вывода строк на терминал. Если на
терминал выдается длинный текст, то после некоторого числа строк
(шага) выдача прекращается с индикацией  'идти?'. Быстрочитающих
пользователей остановки вывода с индикацией  'идти'  сильно нервируют.
Этим пользователям можно рекомендовать приказ 'иди' со следующей
формой обращения:

    ИДИ  [<шаг выдачи строк>]

Здесь <шаг выдачи строк> - любое десятичное целое в диапазоне
от 2 до 999. Задав большой шаг, получим вывод практически без остановок.
Если <шаг> не указан, то устанавливается шаг=14. По умолчанию значение
шага также равно 14.

После индикации 'идти?' можно дать (етх) - тогда выдача будет
продолжена, или набрать литеру 'Н' и тогда исполнение приказа будет
прекращено. В ряде случаев исполнение приказа можно прервать, нажав
клавишу 'DЕL'.

# Приказ 'пауза'

Приказ 'пауза' служит для освобождения терминала на некоторое
время для других работ.
Форма обращения:

    ПАУ-за  [ <интервал времени> [ <терминал> ] ]

где <интервал времени> - это промежуток времени, на который джин
освободит терминал. Если интервал не указан, то принимается интервал
в 1 минуту.

Интервал задается в минутах и секундах. Разделитель - точка. Нули
можно не указывать. Примеры:

    ПАУ     75     (на 1 минуту 15 секунд)
    ПАУ   3.30     (на 3 минуты 30 секунд)
    ПАУЗА   2.     (на 2 минуты)

По прошествии заданного времени джин пытается захватить оставленный (по
умолчанию) или заказанный <терминал>. Если в течение 5 минут это
не удается, то джин уходит совсем. Длительность паузы ограничена
5 минутами. <Терминал> возврата задается как в приказах 'сет' и
'выз'.

Джина можно отозвать с паузы до истечения заказанного интервала
времени набрав приказ Диспака:

    ПОД <номер счетного канала>

# Приказ 'вызов'

Приказ 'вызов' служит для установления связи между терминалами.
Форма обращения:

    ВЫЗ-ов  <список терминалов>  [ / <интервал времени> ]

Здесь <список терминалов> - перечисление (через запятую) восьмеричных
номеров терминалов, номеров комнат или фамилий ответственных или
'все'. Связь устанавливается только с одним из заказанных терминалов
- любым из свободных. Если вызывающий желает установить связь с
операторским терминалом немедленно, то он должен указать его в списке
явно - 'оператор'.

После получения приглашения следует набрать 'вызов' - первую фразу,
которая будет передаваться на указанные терминалы. Например, 'вызываю
григорьева'. Связь считается установленной, если получен хотя бы
один ответ. После установления связи оба абонента становятся равноправными.

Указатель =-> приглашает к набору текста для передачи. Если текст
начинается со знака доллара, то после передачи текста разговор прекращается.
Любая посылка после указателя (хх), располагающегося в правой части
экрана, прерывает ожидание ответа. Здесь хх - номер терминала, на
который было передано сообщение.

Ожидание каждого ответа прерывается после истечения <интервала
времени>. Форма задания интервала описана в приказе 'пауза'.

# Приказ 'сеть'

Приказ 'сеть' дает возможность выдать некоторое сообщение на все
свободные терминалы сети терминалов института.
Форма обращения:

    СЕТь  <список терминалов>  [ / <интервал времени> ]

Здесь <список терминалов> - перечисление (через запятую) восьмеричных
номеров терминалов, номеров комнат или фамилий ответственных или
'все'. Если отправитель желает передать сообщение на операторский
терминал немедленно, то он должен указать его в списке явно.

Пример: ```СЕТЬ ОПЕР,ВСЕ (ЕТХ)```

Имеются две модификации приказа сеть:

 * Интервал времени задан. Интервал задается в виде: час.мин.сек (примеры в приказе 'пауза'). Интервал времени отделяется от списка терминалов косой чертой (знаком деления). При заданном интервале времени Джин освобождает терминал отправителя и пытается выдать сообщение на свободные терминалы из числа заказанных. Попытки делаются через каждые 2 секунды. Если сообщение уже передано всем адресатам или истек заданный интервал времени, то работа джина заканчивается (без возврата к отправителю).
 * Интервал времени не задан. В этом случае передача ведется с уведомлением отправителя о результатах. По каждому заказанному терминалу джин сообщает передано ли сообщение или абонент занят. Если среди заказанных терминалов имеются занятые, то джин запрашивает отправителя, делать ли еще попытки. Ответом пользователя может быть: 'да', 'нет', 'срочно' или <интервал времени>. Если требуется срочная выдача, то сообщение выдается на занятые терминалы. Если отправитель задает интервал времени, то работа джина продолжается как сказано выше в пункте а.

Если отправитель все же хочет знать результаты работы джина после
его ухода, то перед приказом 'сеть' нужно включить печать протокола
и результаты посмотреть по выдаче задачи на АЦПУ.

Примечание: при наборе сообщения для перехода на новую строку
используйте клавишу 'linе fееd'. При передаче на явно заданный операторский
терминал длина строки не должна преышать 48 знаков.

# Приказ 'пакет'

Приказ 'пакет' дает возможность получить информацию о каталоге
ввода БЭСМ-6.
Форма обращения:

    ПАК-ет  [<шифр>,] [ОЧЕ, ЖДУ, РЕШ, ВСЕ]

где:

 * шифр - начальные знаки шифра (от 1 до 6 цифр),
 * оче - задачи, стоящие в очереди на включение в решение,
 * жду - задачи ожидающие вызова или окончания других задач,
 * реш - задачи в решении,
 * все - все задачи.

# Приказ 'статистика'

С помощью этого приказа можно получить различные статистические
сведения о работе Джина.
Форма обращения:

    СТА-тистка   <вид статистики>

Вид статистики может быть одним из пяти возможных:

 * ТЕР-миналы      - для статистики терминалов
 * ПРИ-казы <имя блока> [УПОр] - для статистики приказов
 * ПОЧ-та          - для статистики почты.
 * ВАХ-та [<шифр>] - для статистики пользователей
 * СУМ-марная      - для суммарной статистики.

Статистика терминалов собирается следующим образом.
С каждым терминалом связано определенное место в зоне статистики
(на диске). При каждом вызове джина отмечается терминал вызова (вторая
колонка данных в статистике терминалов). Отмечаются также все другие
занятые в это мремя терминалы (первая колонка данных). Поэтому первая
колонка данных статистики по терминалам дает сведения только об
относительной загруженности терминалов.

Статистика приказов дает возможность судить о том, насколько часто
используются отдельные приказы, дает возможность оценить степень
полезности системы и ее блоков. Именем блока может быть 'джин',
'редактор', 'связь', 'игра', 'библиотека' или 'все'.

Если заказано упорядочение, то статистика упорядочивается в порядке
убывания частоты использования приказов.
Статистика почты дает возможность проверить степень загруженности
почты, узнать прочтено ли отправленное вами письмо и др.

Статистика по вахте (входная область Джина) дает возможность ответа
на большой круг вопросов (когда был пользователь, насколько он активен
и др.) для получения сведений по отдельному лицу или группе лиц.
После 'вах' можно задать четырехзначный шифр (или короче).

Мерой активности пользователя является число вызовов Джина (число
сеансов). Список пользователей иногда упорядочивается по активности.
один сеанс в текущем месяце приравнивается к десяти сеансам в прошедших
месяцах.

Суммарная статистика дает суммарное потраченное ВЦП и суммарное
время сеансов по диалоговой системе 'Джин' и ее автономным блокам.

# Приказ 'бюджет'

Приказ 'бюджет' дает возможность просмотреть состояние бюджета
(по ОС Диспак) подразделений. Можно узнать выделенный, использованный
и оставшийся бюджет.
Форма обращения:

    БЮД-жет  [ <лаб>, ВСЕ, ВЫД, ИСП,ОСТ, %, ВРЕ, ЭКС ]

где <лаб> - номер подразделения. Если номер подразделения указан,
то выдается только бюджет этого подразделения. Если указан операнд
'все', выдаются бюджеты всех подразделений. Если операнд <лаб> не
указан, то выдается бюджет того подразделения, к которому принадлежит
пользователь. Все виды бюджета можно заказать одновременно (перечислять
через запятую). По умолчанию выдается оставшийся бюджет. Если указан
'%', то использованный и оставшийся бюджеты даются в процентах к
выделенному.

Параметр 'время' означает заказ выдачи установленных ограничений
на время счета экспрессных, лимитных и дежурных задач.

Параметр 'экс' означает заказ выдачи шифров экспрессных задач.

# Приказ 'вопрос'

По этому приказу (операндов нет) происходит переход в автономный
блок вопросов. В блоке 'вопрос' действует разговорный режим - вопросы
и приказы задаются в произвольной форме. Синоним приказа 'воп' -
'джи'.

В блоке вопросов имеются свои приказы 'инф' и 'шко'. В частности
в блоке вопросов можно запросить правила работы.

# Приказ 'библиотека'

По этому приказу происходит переход в блок библиотеки, со своим
набором приказов.

В блоке имеются приказы 'выставка', 'журналы', 'препринты', 'авторефераты',
'сигнал', 'заказ', 'юмор', 'должники'.

В разработке блока приняли участие А.А.Григорьев и Г.П.Моисеева.

# Приказ 'графика'

По приказу 'графика' происходит переход в одноименный автономный
блок. С помощью графического блока производится управление подчиненными
задачами систем 'графал' и 'графор'.

Приказом 'выброс' графического блока можно выбросить свою задачу
из каталога ввода.

# Приказ 'арифмометр'

По приказу 'ариф' происходит переход в арифметический блок.
Подробнее смотрите в информаторе и школе блока.

# Приказ 'администратор'

По этому приказу происходит переход в служебный блок администратора
Джина с ограниченным доступом. Назначение блока: контроль за работой
системы, обнуление статистики, снятие паролей у лиц, которые их
забыли, исключение выбывших пользователей, замена шифров, упорядочение
пользователей по их активности, занесение новостей и др.

# Приказ 'игра'
По этому приказу происходит переход в автономный блок игр
Форма обращения:

    ИГР-а   [ <вид игры> ]

где  <вид игры> : LАN-ding, ШАШ-ки, КРЕ-стики, КАЛ-ах, КУБ-ик, БЕГ-а,
ЛОТ-о, ГЛИ-ссада, ИНС-пектор.

Адаптация игры в посадку на луну (lаnding) для БЭСМ-6 С.И.Пирина
и А.Н.Бирюкова. Вариант игры в переводе на русский язык (посадка)
В.В.Кобелева.

Игру во французские шашки запрограммировал Ю.В.Кашаев.

По приказу кре-стики производится разметка экрана терминала для
игры в крестики-нолики. По окончании разметки джин освобождает терминал
и оканчивает задачу.

Игры 'калах' и 'кубик' запрограммировал В.В.Кобелев.

Автор игры 'бега' (ипподром) - В.Н.Пильщиков. Адаптировал игру
для джина В.В.Долматов.

Игра 'лото' сходна с игрой в кости. Авторы - А.А.Григорьев, А.К.Рыжаков,
В.В.Тихорский.

Игра 'глиссада' (автор - П.Р.Шумилов) имитирует посадку самолета
в аэропорту.

Игру-детектив 'инспектор' адаптировал и переработал В.Г.Кузнецов.

Имеются ограничения на вход в блок игр - в рабочее время входить
в блок игр обычно не разрешается. В субботу и воскресенье вход свободный.
Игра на деньги в любое время запрещена.

# Приказ 'редактор'

Приказ 'редактор' передает управление в автономный блок редактора,
предназначенный для работы с магнитными лентами и дисками.
Форма обращения:

    РЕД-актор  [<приказ блока>]

Основные приказы блока: считай, найди, покажи, замени, запиши,
сравни и перепиши. Подробности смотрите в информаторе и школе блока.

# Приказ 'связь'

По приказу 'связь' происходит переход в автономный блок связи.
С помощью приказов этого блока производится обмен информацией между
дисками связи, ДИМОНом и личными лентами пользователей.

Можно также просмотреть каталоги дисков связи.

# Приказ 'журнал'

По приказу 'журнал' происходит переход в автономный блок рабочего
журнала БЭСМ-6. Через блок журнала можно просмотреть текущую и накопленную
статистику по задачам.  Подробнее смотрите в информаторе и школе
блока.

# Приказ 'система'

По приказу 'система' происходит переход в (автономный) системный
блок. Этот блок предназначен для системных программистов и любознательных
пользователей. Приказы блока дают возможность проводить анализ работы
ОС Диспак.

Подробнее смотрите в информаторе и школе блока.

# Приказ 'конец'

Приказ 'конец' операндов не имеет. По этому приказу работа джина
оканчивается.

Для выхода из школы наберите приказ 'выход'.

# Приказ 'выход'

Приказ 'выход' может быть исполнен на любом уровне приказов. Производится
переход к приказам более высокого уровня.