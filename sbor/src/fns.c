/*г==================================================================¬
   "emd.fns" - Служебные функции для "main()":
   "int how_file(char *f_n,char *mode)" - File open
    "int video(int nf)"  - Fonts setup
    "int pal_file(void)" - Palette setup
    "int krdf_e(int nf)" - Graph_mode set from "main()"
    "void gotoxy (char x, char y)" - set text position
    "void hdr_emd(void)" - Заставка продукта "ЭМУЛЯТОР"
    "void drtime(int w)" - Draw_of_Time_value & "БЭСМ"
    "void mdcheck(void)" - опознание пакетов на устр-вах
    Release - 15.06'99 ... 19.07'2000
  L==================================================================-*/


/* Открытие файла "*f_n" в режиме "*mode" */
int how_file(char *f_n, char *mode)
{if((fptr=fopen(f_n,mode))==NULL) return 1; return 0;};

int video(int nf)
{
// unsigned char _far *option = "t'tms rmn'h5w5b";
 UC *option[6]={"n1","n2","n3","n4","n5","n6"};
 UC *fondir = "*.fon";
 _setvideomoderows(_VRES16COLOR,25);
 if( _registerfonts(fondir) <= 0 )
  { printf("\n%s: No such file or directory \n",fondir); goto R; }
 if ( _setfont(option[nf]) < 0 )
  { printf("\n Нет шрифта \"%.18s\" в файле \"%s\" \n",option[nf],fondir);
 R:  _getch(); return 1; }
 _setvideomoderows(_VRES16COLOR,60); return 0;
}; // "video()"


int pal_file()
{
 # define MAXCOL 16
 long pal[MAXCOL];
 UC spalsim[13]="dtk_sim5.pal"; // Файл с палитрой
 UC n; // Чтение палитры из файла:
  if(how_file(spalsim,"r")) { perror(spalsim); return 1; }
  for(n=0; n<MAXCOL; n++) fscanf(fptr,"%lx ",&pal[n]);
  _remapallpalette(pal); fclose(fptr); return 0;
}; // "pal_file()"


int krdf_e(int nf)
{
 int h=0;
  h  =  video(nf); // Чтение файла с графич. фонтами
  h += pal_file(); // Чтение файла с палитрой
  return h;
}; // "krdf()"








/* Set text cursor position - Kalinin */
void gotoxy (char x, char y)
{
 _asm {
 mov dh,y
 mov dl,x
 mov bh,0
 mov ah,2
 int 10H
      }
}; // "gotoxy()"



void hdr_emd(void)
{static UI clr=10;
 _setcolor(clr); clr++; if(clr>15) clr=0;
 _moveto((XU+SU*2-4)*8+4, (YU-11)*8);
 _outgtext("Э М У Л Я Т О Р   М А Г Н И Т Н Ы Х   Д И С К О В");
 _setcolor(clr+1); _rectangle(_GBORDER,
  (XU+SU*2-5)*8,(YU-13)*8+6,(XU+SU*2+40)*8+4,(YU-8)*8-4 );
 _moveto(8,8); _outgtext(cpfn);
} // "hdr_emd()"



// Draw of time_field
void drtime(int w) {
#define PT 35 // X_time_position_at_screen
 static char sbuft,buft[12],w1=8;
 _strtime(buft); if(w) w1=w;  if(buft[7] != sbuft)
 {_setcolor(3); _rectangle(_GFILLINTERIOR,PT*8-1,8-1,(PT+w)*8,8*2+3);
  _moveto(PT*8,8);  _setcolor(14);  sbuft = buft[7]; _outgtext(buft);
   hdr_emd(); // Look at me please how I am wounderfull !
 } if(GM) _setcolor(12); else _setcolor(10);
   _moveto(597,8); _outgtext("БЭСМ");
} // "drtime()"



/* Опознание пакетов по принципу БЭСМ - по 4-й зоне */
void mdcheck(void)
{register i,j;
 UC fz1[4];
 UC z4[7]="\\0004";
           rsy = 0;
 for(j=0;j<8;j++) {_itoa(j,fz,10); strcat(fz,z4);
  if(how_file(fz,"rb")) continue;
  fread(DB,2,TR*2,fptr); fclose(fptr); greb(1); // to BESM-codes
  i=(DB[1*5+3]&01777)>>2; i|=(DB[1*5+2]&017)<<8; // N pack
 _itoa(i,disk[j],10); if(!g) rsy|=01<<j; } rsy &= ~orsy;
           rsy1 = 0;
 for(j=0;j<8;j++) {_itoa(j,fz1,10); strcpy(fz,l1);
  strcat(fz,fz1);  strcat(fz,z4);
  if(how_file(fz,"rb")) continue;
  fread(DB,2,TR*2,fptr); fclose(fptr); greb(1); // to BESM-codes
  i=(DB[1*5+3]&01777)>>2; i|=(DB[1*5+2]&017)<<8; // N pack
 _itoa(i,disk1[j],10); if(!g) rsy1|=01<<j; } rsy1 &= ~orsy1;
  opros=1; if(!g) dr_un(0);
} // "mdcheck()"
