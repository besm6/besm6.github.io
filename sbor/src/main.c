/*г==================================================================¬
    "emd.c"   -  Emulator of MD for BESM-6
     Release  -  22.08'99 ... 19.07'2000
  L==================================================================-*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <bios.h>
#include <time.h>
#include <conio.h>
#include <graph.h>
#include <ctype.h>

#include "emd.h"

void main( int n, char *v[], char **envp)
{
	UI i, nf = 0;
	UI rsya = 0, rsoa = 0;		// added to rsy, rso
	char TST[16] = "Режим теста УВУ";

	/* Выборка параметра - имя exe/файла */
	strcpy (cpfn, v[0]);		// "Exe_File_Name"
   if(cpfn[0]=='D') l1[0]='E';
/* man(); */ if(krdf_e(1)) { printf("\n\t Excuse me please...");
         _getch(); goto E; }
   mdcheck(); // опознание пакетов на устройствах
   drtime(7); // '7' - font size
 for(;;) {
 G: // Ожидание <ПКА>:
  pus = _inpw(P_R2); GM = getGM;
  if(GM) // ГМ - 5-th bit from Ikonnikov
   { clnPKA;   // гашение <ПКА>
     clnHBSL;  // гашение <ХБСл>
     lin=pus=portinp=dir=pmd=mod=kom=0; drtime(7);
     if(g) { g=nf=0; GD=r49; mdcheck(); } // Гашение тестового режима
     nf++; if(nf>1000) goto E; // 26.05'2000
           else        goto G;
   }
 if(getPKA) { portinp = getKUS; // <~KUSj> (1-12) bits
  if(!dir) { if((portinp&077)==050) { clnPKA; goto G; }
             else // if(portinp&0400) // запрос ?
              { dir = 1; clnPKA; nf = 0; drtime(7);
scan = pimp = 0; // Проверка клавиатуры:
  if(_kbhit()) { pimp = _getch(); // Service ?
  if(pimp==0x00||pimp==0xe0) { pimp=_getch(); scan=1; }
               } else goto P;
   if(!scan) {
   if(pimp=='U'||pimp=='u') { // if(lin) goto P;
 B: CDBW1; stp(20,34); // Включение/Выключение устройства
  _outtext("Задайте N линейки (0,1): ");
   scanf("%u",&lin);     if(lin>1) { _outtext("Ошибка: N > 1");
  _outtext(ent); _getch(); goto B; } stp(20,36);
  _outtext("Задайте N устройства (0-7): ");
   scanf("%u",&mod);     if(mod>7) { _outtext("Ошибка: N > 7");
  _outtext(ent); _getch(); goto B; }
         unit(); CDBW1;
      if(!opros) mdcheck(); } // опознание пакетов
             } // end of "if(!scan)"
P:   if((portinp&07400)==01400) lin = portinp&03;
     setPRER; } // "if(portinp&0400)"
   goto G; } // "if(!dir)"
  else { // "if(dir)"
         // опрос РС (1-12)р.р.:
    if(portinp==011)
     { if(!lin) { if(pmd&rsy) setB(pmd|GD|rsya); else setB(rsya); }
       else { if(pmd&rsy1) setB(pmd|GD|rsya); else setB(rsya); } }
         // опрос РС (13-24)р.р.:
    if(portinp==031) setB(rso|rsoa);
    if(portinp==014) { // Режим т.УВУ
        setIM; orsy1=orsy=rsy1=rsy=GD=0;
         if(!g) { g=1; dr_un(0);
       _setcolor(12); _moveto(64*8,5*8); _outgtext(TST);
                }    }
          clnPKA; // гашение <ПКА>
   switch(portinp&06000) // bits 12,11
    {
     case 02000:  // гашение РМ либо выбор ...
                  // номера устройства - позиционно:
                 pmd = portinp&0377; break;
     case 00000: kom = portinp&077; // прием типа команды
      switch(kom)
       {
        case  01: // сброс на 0-й цилиндр
        case  02: // подвод
        case  03: // чтение
        case  04: // запись
        case  06: // сравнение
                  break;
        case  07: // чтение заголовка:
                  for(i=0;i<10;i++) drtime(7);
                  rdhd(); setPRER; goto G;
        case 010: // гашение РС
                  rsoa = rsya = 0; break;
        case 050: // освобождение направления
                  dir=0; setPRER; goto G;
         default: break;
       }
             break;
     case 04000: // прием непрерывного...
     case 06000: // ...адреса дорожки.
         adr = portinp&03777;
      if(adr>2029) { rsoa=02000; rsya=04000; // ОшБ и АВ
                     break; }
                 for(i=0; i<5; i++) drtime(7); // для сл/слов
      switch(kom)
       {
        case 00: // тоже подвод !
        case 02: // подвод у нас всегда окончен
                 mod_n(); // определяем N модуля
                _itoa(adr>>1,zone,10);
                 if(!lin) { strcpy(zones[mod],zone); mcol[mod] = 14; }
                 else { strcpy(zones1[mod],zone); mcol1[mod] = 14; }
                  dr_un(mod+1);
                 setPRER; break;
        case 03: // адрес для чтения
                 rdmd(); setPRER; break; // вызов функции чтения
        case 04: // адрес для записи
                 wrmd(); setPRER; break; // вызов функции записи
        case 06: // адрес для команды сравнения
                 cmpr(); setPRER; break; // вызов функции сравнения
        default: setB(0); rsoa=04000; rsya=04000; // ОшПК и АВ
  sprintf(buf,"Принят адрес %.4o для команды %.3o...",adr,kom);
      stp(20,58); _outtext(buf); break; //  докладываю: "ложняк" !
       }
     default: break;
    } // "switch(portinp&06400)"
       } // "if(dir)"
            } // "if(getPKA)"
         } // end of "for(;;)"
 E: _clearscreen(_GCLEARSCREEN); _fcloseall();
    _setvideomode(_DEFAULTMODE);
} // "main" end
