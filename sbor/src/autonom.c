/*г========================================================¬
   "autonom.fns" - Вспомогатальные служебные функции
    "int ug(void)" - в верхнем регистре _getche()
    "void unit(void)" - Вкл/выкл пакет на устройстве
    "void dr_un(int m)"  - Засветка N пакетов и тек.зон
    "void greb(int u)" - 'гребёнка' -> в слоги УВУ/БЭСМ
   Realease - 30.08'99 ... 19.07'2000
  L========================================================-*/

int ug(void){ d=_getche(); if(isalpha(d))
 { s = (isupper(d)) ? d : _toupper(d); return s; } else return d; }


UI orsy,orsy1;
void unit()
{
#define NU (01<<mod)
 B: stp(20,38);
 if(!lin) { if( (!(rsy&NU) && !(orsy&NU)) )
    { _outtext("Пакета на устройстве нет");
      _outtext(ent); _getch(); return; } // pack is not
   if(rsy&NU) _outtext("Выключить "); else _outtext("Включить ");
  goto U; }
 else   { if( (!(rsy1&NU) && !(orsy1&NU)) )
    { _outtext("Пакета на устройстве нет");
      _outtext(ent); _getch(); return; } // pack is not
   if(rsy1&NU) _outtext("Выключить "); else _outtext("Включить ");
        }
 U: sprintf(buf,"устройство N°%u%u ? (Y/N) : ",lin,mod);
  _outtext(buf); d=ug(); if(d=='N') return; if(d!='Y') goto B;
 if(!lin) { if(rsy&NU) {rsy&=~NU; orsy|=NU;}
            else {rsy|=NU; orsy&=~NU;}     }
 else   { if(rsy1&NU) {rsy1&=~NU; orsy1|=NU;}
          else {rsy1|=NU; orsy1&=~NU;}      }
   opros=0;
} // "unit()"


void dr_un(int m)
{
 register i,j;
 if(!m) { WCLN;
 for(i=0; i<8; i++) // для 0-й линейки
  {
   if((rsy>>i)&01) { _setcolor(3); _rectangle(_GFILLINTERIOR,
    (XU+i*SU)*8+2,(YU)*8+2,(XU+(i+1)*SU-1)*8-2,(YU+SU)*8-2); }
        _setcolor(15); _moveto((XU+i*SU+2)*8+4,(YU-2)*8);
    j = atoi(disk[i]);  if(j<2048) goto M;
   _outgtext(disk[i]); _moveto((XU+i*SU+2)*8+4,(YU+SU+2)*8);
   j = atoi(zones[i]); sprintf(zone,"%.4o",(j-4)&07777);
   _setcolor(mcol[i]); _outgtext(zone);
  M:    _setcolor(14); _moveto((XU+SU*2)*8, (YU-5)*8);
   _outgtext("П А К Е Т Ы   Н А   Д И С К О В О Д А Х :");
   _moveto((XU+i*SU+1)*8,(YU+SU/2-2)*8); _outgtext("Устр-во");
   _moveto((XU+i*SU+2)*8,(YU+SU/2+1)*8);
    sprintf(buf," 0.%i",i); _outgtext(buf); _rectangle(_GBORDER,
           (XU+i*SU)*8, (YU)*8, (XU+(i+1)*SU-1)*8, (YU+SU)*8);
  }   _moveto((XU+i*SU-1)*8,(YU+SU+2)*8); _outgtext("<зоны>");

 for(i=0; i<8; i++) // для 1-й линейки
  {
   if((rsy1>>i)&01) { _setcolor(3); _rectangle(_GFILLINTERIOR,
    (XU+i*SU)*8+2,(YU+DU)*8+2,(XU+(i+1)*SU-1)*8-2,(YU+DU+SU)*8-2); }
        _setcolor(15); _moveto((XU+i*SU+2)*8+4,(YU+DU-2)*8);
    j = atoi(disk1[i]);  if(j<2048) goto M1;
   _outgtext(disk1[i]); _moveto((XU+i*SU+2)*8+4,(YU+DU+SU+2)*8);
   j = atoi(zones1[i]); sprintf(zone,"%.4o",(j-4)&07777);
   _setcolor(mcol1[i]); _outgtext(zone);
  M1:   _setcolor(14); _moveto((XU+SU*2)*8, (YU+DU-5)*8);
   _outgtext("П А К Е Т Ы   Н А   Д И С К О В О Д А Х :");
   _moveto((XU+i*SU+1)*8,(YU+DU+SU/2-2)*8); _outgtext("Устр-во");
   _moveto((XU+i*SU+2)*8,(YU+DU+SU/2+1)*8);
    sprintf(buf," 1.%i",i); _outgtext(buf); _rectangle(_GBORDER,
           (XU+i*SU)*8, (YU+DU)*8, (XU+(i+1)*SU-1)*8, (YU+DU+SU)*8);
  }   _moveto((XU+i*SU-1)*8,(YU+DU+SU+2)*8); _outgtext("<зоны>");
        } // "if(!m)"
  else   {
   if(!lin) { // для 0-й линейки
    if((m>8)||(m<0)) return; i=m-1; j=atoi(zones[i]);
     _setcolor(0);  _rectangle(_GFILLINTERIOR,
   (XU+i*SU+2)*8+4,(YU+SU+1)*8,(XU+i*SU+2+5)*8,(YU+SU+4)*8);
   _setcolor(mcol[i]); _moveto((XU+i*SU+2)*8+4,(YU+SU+2)*8);
    sprintf(zone,"%.4o",(j-4)&07777); _outgtext(zone);
   _setcolor(15); _moveto((XU+i*SU+2)*8+4,(YU-2)*8); _outgtext(disk[i]);
            } else { // для 1-й линейки
    if((m>8)||(m<0)) return; i=m-1; j=atoi(zones1[i]);
     _setcolor(0);  _rectangle(_GFILLINTERIOR,
   (XU+i*SU+2)*8+4,(YU+DU+SU+1)*8,(XU+i*SU+2+5)*8,(YU+DU+SU+4)*8);
   _setcolor(mcol1[i]); _moveto((XU+i*SU+2)*8+4,(YU+DU+SU+2)*8);
    sprintf(zone,"%.4o",(j-4)&07777); _outgtext(zone);
   _setcolor(15); _moveto((XU+i*SU+2)*8+4,(YU+DU-2)*8); _outgtext(disk1[i]);
                   } // "else"
         } // "else"
} // "dr_un()"



/* Функция "гребёнка" для подыгрывания УВУ.
 * 'u' = 0 - // БЭСМ -> УВУ
 * 'u' = 1 - // УВУ -> БЭСМ
 * Release - 26.08'99 ... 13.10'99 */
void greb(int u)
{register i,j;
 UI c[5],d[5];
 for(i=0; i<TR*2/5; i++) // по словам БЭСМ
 { // Формирование 5-ти слогов для каждого слова:
  for(j=0;j<5;j++) {c[j]=0; d[j]=DB[i*5+j];}
  for(j=0;j<5;j++) // по слогам
   {
 if(!u)  // БЭСМ -> УВУ
  {c[j]|=((d[0]>>(9-j))&01)<<9; c[j]|=((d[0]>>(4-j))&01)<<8;
   c[j]|=((d[1]>>(9-j))&01)<<7; c[j]|=((d[1]>>(4-j))&01)<<6;
   c[j]|=((d[2]>>(9-j))&01)<<5; c[j]|=((d[2]>>(4-j))&01)<<4;
   c[j]|=((d[3]>>(9-j))&01)<<3; c[j]|=((d[3]>>(4-j))&01)<<2;
   c[j]|=((d[4]>>(9-j))&01)<<1; c[j]|=((d[4]>>(4-j))&01)<<0;
  } else // УВУ -> БЭСМ
  {c[0]|=((d[j]>>9)&01)<<(9-j); c[0]|=((d[j]>>8)&01)<<(4-j);
   c[1]|=((d[j]>>7)&01)<<(9-j); c[1]|=((d[j]>>6)&01)<<(4-j);
   c[2]|=((d[j]>>5)&01)<<(9-j); c[2]|=((d[j]>>4)&01)<<(4-j);
   c[3]|=((d[j]>>3)&01)<<(9-j); c[3]|=((d[j]>>2)&01)<<(4-j);
   c[4]|=((d[j]>>1)&01)<<(9-j); c[4]|=((d[j]>>0)&01)<<(4-j);
  }} // "for(j)"
 for(j=0;j<5;j++) DB[i*5+j]=c[j]; // new structure fixup
 } // "for(i)"
} // "greb()"
