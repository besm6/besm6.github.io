
# Работа с симулятором SIMH

[Проект SIMH](https://ru.wikipedia.org/wiki/SIMH) представляет собой семейство симуляторов
исторических компьютеров 60х-80х годов. Основной сайт:
[simh.trailing-edge.com](http://simh.trailing-edge.com/). Сборка и запуск SIMH для БЭСМ-6
описаны [на отдельной странице](Building).

Все симуляторы семейства SIMH имеют похожий командный интерфейс.
Имеется [общее описание в формате PDF](http://simh.trailing-edge.com/pdf/simh_doc.pdf).
Здесь мы кратко опишем основные приёмы работы с симулятором.

## Вызов симулятора

Бинарный файл симулятора имеет имя `besm6` для ЭВМ БЭСМ-6 и `svs` для ЭВМ СВС.

При вызове без параметров симулятор вызывается в диологовом режиме.
Командой `help` можно получить список команд и информацию о командах.
Выйти можно командой `q`. Например:
```
$ besm6

БЭСМ-6 simulator V4.0-0 Beta        git commit id: 0169032c
sim> help
Help is available for the following commands:

    !              ASSERT         ASSIGN         ATTACH         BOOT
    BREAK          CALL           CAT            CD             CONTINUE
    DEASSIGN       DEPOSIT        DETACH         DIR            DO
    DUMP           ECHO           EVALUATE       EXAMINE        EXIT
    EXPECT         GO             GOTO           HELP           IDEPOSIT
    IEXAMINE       IF             IGNORE         LOAD           LS
    NEXT           NOBREAK        NOEXPECT       ON             PROCEED
    PWD            RESET          RESTORE        RETURN         RUN
    SAVE           SCREENSHOT     SEND           SET            SHIFT
    SHOW           STEP           TYPE
sim> q
Goodbye
```

При вызове симулятор ищет и выполняет скрипт инициализации `$HOME/simh.ini`, а при его отсутствии
`./simh.ini`. Общий вид вызова следующий:

```
    besm6 [-флаги] [файл параметры...]
```

Если задано имя файла, он выполняется (с параметрами) после скрипта инициализации.
Затем симулятор переходит в интерактивный режим.

Флаги:
 * -v - печатать команды скрипта перед выполнением
 * -e - прервать выполнение скрипта при ошибке команды
 * -q - сообщать менее подробную информацию о выполнении команд, а также не печатать имя и версию симулятора при старте
 * -o - вызываемый скрипт будет наследовать состояние `on` из вызывающего скрипта

## Примеры скриптов

Пример 1: запуск теста арифметического устройства. Файл `test_alu.b6` содержит код теста.
```
set mmu cache
load test_alu.b6
go 32000
```

Пример 2: запуск операционной системы Диспак. Из файла `boot_dispak.b6` считывается код
первичного загрузчика. В файлах `sbor2048.bin` и `sbor2053.bin` должны находиться
образы системных дисков. Для дополнительного временного диска будет создан файл `2052.bin`.
Для барабанов будут созданы файлы `drum1x.bin` и `drum2x.bin`.
Вывод АЦПУ будет направлен в файл `output.txt`.
Протокол консольного терминала пишется в файл `tty1.txt`.
```
set cpu idle
attach -e disk0 sbor2048.bin
attach -e disk7 sbor2053.bin
attach -n disk6 2052.bin
attach -n drum0 drum1x.bin
attach -n drum1 drum2x.bin
attach -n prn0 output.txt
attach tty1 console
set tty1 qwerty,authbs
set -n tty1 log=tty1.txt
load boot_dispak.b6
run 2000
```

## Содержимое памяти

TODO: описать применение команд `d` и `ex`.

## Команды симулятора

 * quit
 * set
 * !
 * echo
 * attach
 * do
 * load
 * dump
 * reset
 * examine
 * deposit
 * go
 * step
 * run
 * break

TODO
