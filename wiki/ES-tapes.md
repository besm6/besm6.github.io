
# Инструкция по работе с ЕС-лентами

Экстракод:

    070 А

где А - адрес слова.

# Структура информационного слова

Разряд 48 - L - длина зоны:

  * 1 - короткая зона 256 слов
  * 0 - длинная зона 1024 слова

Разряды 42:40 - код операции:

  * 0 - записать зону
  * 1 - прочитать зону
  * 2 - пропустить зону вперёд
  * 3 - пропустить зону назад
  * 4 - пропустить файл вперёд до ближайшего ленточного маркера (ТМ), лента остановится за ТМ
  * 5 - пропустить файл назад, лента останавливается после ТМ
  * 6 - перемотать магнитную ленту к началу
  * 7 - запись ТМ

Разряды 35:31 - номер листа ОЗУ.

Разряды 26:25 - номер абзаца (при L=1).

Разряд 24 - P - плотность записи:

  * 0 - высокая плотность 32 бит/мм
  * 1 - низкая плотность 8 бит/мм

Разряды 18:13 - виртуальный номер магнитной ленты.

На СВС разряд 29=1 - признак обмена произвольной длиной.
В индекс-регистре 14 находится длина в байтах.

Поле номера зоны в разрядах 12:1 игнорируется.

Переключить плотность с высокой на низкую можно только на маркере начала ленты.
Переключить с низкой на высокую - только после перезагрузки ленты на магнитофоне.

На экстракод выделяется 5 секунд (10 метров ленты).
Если нужно больше - повторить экстракод.

# Ответ

Ответ возвращается в индекс-регистре 14:

Разряды 15:6 - номер последнего слова в листе (абзаце), участвовавшего в обмене
(при коде операции 0 или 1). Если при обмене листом последнее слово участвовало, то 0.

Разряды 5:1 - статус:

  * 0 - всё хорошо
  * 1 - лента в маркере начала
  * 2 - лента в маркере конца
  * 3 - очередная зона в файле отсутствует, просмотр файла окончен, лента находится за ТМ
  * 4 - на участке длиной 10м нет требуемого элемента информации (зоны или ТМ)
  * 5 - зона на ленте длиннее, чем требовалось прочитать
  * 6 - магнитофон выключен или не определено местоположение ленты
  * 7 - лента не считывается, операция не выполнена, лента находится за ТМ

При благополучной перемотке в начало индекс-регистр 14 устанавливается в 1.