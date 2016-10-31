/*г========================================================¬
  * "emd.h" - Header for "emd.c" - 31.08'99 ... 21.08'2000 *
  L========================================================-*/

#define TWO 2 // - for two lines


#define stp(x,y) _settextposition(y,x)
#define CDBW {_setcolor(0);_rectangle(_GFILLINTERIOR,60,250,580,480);}
#define CDBW1 {_setcolor(0);_rectangle(_GFILLINTERIOR,0,250,640,310);}
#define WCLN {_setcolor(0);_rectangle(_GFILLINTERIOR,0,0,640,480);}
#define UC unsigned char

 typedef unsigned int UI;

#ifdef TWO
 UC l1[10] = "d:\\emd\\"; // 1-st line
#else
 UC l1[10] = "c:\\emd1\\"; // 1-st line
#endif
 UC cpfn[30]; // name of "exe.file"
 UC ent[21] = ". Нажмите <Enter>...";
 UC disk[8][5] = {"0","0","0","0","0","0","0","0",};  // 0-th line
 UC zones[8][5] = {"4","4","4","4","4","4","4","4",}; // нач.зона
 UI mcol[8] = {15,15,15,15,15,15,15,15,}; // цвет зоны по устр-вам
 UC disk1[8][5] = {"0","0","0","0","0","0","0","0",}; // 1-sl line
 UC zones1[8][5] = {"4","4","4","4","4","4","4","4",}; // нач.зона
 UI mcol1[8] = {15,15,15,15,15,15,15,15,}; // цвет зоны по устр-вам
 UI pimp,scan; // - для опроса клавиатуры в "main()"
 UI pack; // - номер пакета при разметке в авт. режиме
 UI dir;  // - был запрос; гасится при освобождении - '050'
 UI lin;  // - N линейки
 UI mod;  // - N модуля
 UI pmd;  // - N модуля позиционный
 UI kom;  // - тип команды
 UI adr;  // - непрерывный адрес дорожки
 UI portinp,portout; // see in "getUS()"
 UI rsy;  // 1-12 р.р. РС: 1-8 bits - КП(0-7), 9-й - ГД.
 UI rsy1;  // 1-12 р.р. РС: 1-8 bits - КП(0-7), 9-й - ГД.
 UI rso = 01500; // 12-24 р.р. РС: 22-НВМ, 21-ВКЛ, 19-1рРЦГ(?)
 UC opros; // Check all packs, if == '1'
 UC GM; // <ГМ> = '1' от  'У0'(^^^)
 UI GD = 0400; // 9-th bit in "rsy" - ГД

#define ZN 1015 // Число физ.зон на пакете
#define TR 2580 // Число слогов на дорожке

 UI DB[TR*2]; // Data_Buffer_for_Read of a zone
 UI DW[TR*2+2]; // Data_Buffer_for_Write of a zone
 UI pus; // для опроса входного порта
 UI g; // Test YBY flag
 UI r49 = 0400; // 49-й контр.разряд
 UI key = 0170707; // ключ разметки
 UI slim,d,s,t,cnts; // for all cases
 UC pr[5],pw[5],zone[5],nmod[3],fz[18];
 UC slesh[2]="\\",nu[2]="0";
 UC buf[80],buft[12];

/* for "hdr_emd()" in "emd.fns" */
#define XU 3
#define YU 16
#define SU 9
#define SL 20
#define DU 30

 FILE *fptr;

#include <dos.h> // for "man()"

/* For connect with YBY - 05.09'99 ... 09.06'2000 */

#define P_R1 0x360 // <KYCj> - R;
#define P_W6 0x360 // уст-ка реж.<ИМ> - W;
#define P_W1 0x36A // <Бj> - W.
#define P_R2 0x362 // <ПКА>,<ХБСл>,<ОП>,<~РБУС>,<ГМ>: - R;
#define P_W2 0x362 // гашение <ПКА>  - W.
#define P_W3 0x364 // гашение <ХБСл> - W.
#define P_W4 0x366 // <СИМД> - W (~0.2мксек).
#define P_W5 0x368 // <Прер> - W (~0.2мксек).

#define getPKA  pus&001
#define getHBSL _inpw(P_R2)&002
#define getOP   _inpw(P_R2)&004
#define getRBUS _inpw(P_R2)&010
#define getGM   pus&020
#define getKUS (~_inpw(P_R1))&07777
#define setB(j) _outpw(P_W1,(j)&07777)
#define setSIMD _outpw(P_W4,1)
#define setPRER _outpw(P_W5,1)
#define clnPKA  _outpw(P_W2,0)
#define clnHBSL _outpw(P_W3,0)
#define setIM   _outpw(P_W6,1)

#include "autonom.fns"
#include "emd.fns"
#include "emd.cmd"
