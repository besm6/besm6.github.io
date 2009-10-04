/*г========================================================¬
  *  "emd.cmd" - Real mode of 03,04,06,07 - commands
      "void nozone(void)" - 'Не найден файл для зоны'
      "void mod_n(void)" - позиц. N модуля -> в цифровой
      "int zon_p(char*)" - zone_path for 'rdmd/wrmd'
      "void rdmd(void)" - read with MD
      "void wrmd(void)" - write on MD
      "void cmpr(void)" - zones compare
      "void rdhd(void)" - head/dor_read
  *  Release - 24.08'99 ... 19.07'2000
  L========================================================-*/


void nozone(int z){ stp(20,55); _outtext("Не найден файл зоны ");
 sprintf(buft,"%.4o",z); _outtext(buft); _outtext(ent); _getch(); }


void mod_n(void)
{ // определяем N модуля
 UI i;
 UC md[8]={0,1,2,3, 4, 5, 6,  7};
 UC pm[8]={1,2,4,8,16,32,64,128};
     mod=0; for(i=0; i<8; i++)
   { if(pmd==pm[i]) { mod=md[i]; break; } else continue; }
}


int zon_p(char *m)
{
 UI i;
 mod_n();
  _itoa(mod,nmod,10);
   if(!lin) strcpy(fz,nmod); else { strcpy(fz,l1); strcat(fz,nmod); }
   strcat(fz,slesh); i=adr>>01;
  if(!(i/1000)) strcat(fz,nu); if(!(i/100)) strcat(fz,nu);
  if(!(i/10)) strcat(fz,nu); _itoa(i,zone,10); strcat(fz,zone);
  if(how_file(fz,m)) { nozone(i); return 1; }; return 0;
}

/*
 * Драйвер чтения зоны - операция '03'
 */
void rdmd (void)
{
	register i;

	if (zon_p ("rb"))
		return;
	fread (DB, 2, TR*2, fptr);
	fclose (fptr);
	if (! lin) {
		strcpy (zones[mod], zone);
		mcol[mod] = 10;
	} else {
		strcpy (zones1[mod], zone);
		mcol1[mod] = 10;
	}
	dr_un (mod+1);
	t=0;
	if (adr & 01)
		t = TR;			// 2-nd half of zone
B:	for (i=0; i<TR; i++) {
		setB (DB [t+i] & 01777);
		setSIMD;
W:		if (getHBSL)
			clnHBSL;	// гаш. <ХБСл>
		else
			goto W;		// wait of HBSL
		if (! (getRBUS))
			return;		// сл/слова
	} // "for(i)"
	if (getRBUS)			// (RBUS&&(!(OP))
		if (! t) {
			t = TR;
			goto B;		// to 2-nd half
		}
} // "rdmd()"





void wrmd(void) /* Драйвер записи зоны - операция '04'*/
{
	register i;

	if (zon_p ("wb"))
		return;
	t = 0;
	if (adr & 01)
		t = TR;				// 2-nd half of zone. Приём 0-го слога:
A:	for (i=0; i<TR; i++) {
		setSIMD;
W:		if (getHBSL == 0)
			goto W;
		DW [t+i] = getKUS & 01777;	// прием i-го слога из УВУ
		clnHBSL;
		if (! (getRBUS))
			goto Q;			// sl/words (?)
	} // "for(i)"
	if (getRBUS) {
		if (! t) {
			t = TR;
			goto A;
		}
	} else {
Q:		fwrite (DW, 2, TR*2, fptr);
		fclose(fptr);
		if (! lin) {
			strcpy (zones[mod], zone);
			mcol [mod] = 12;
		} else {
			strcpy (zones1[mod], zone);
			mcol1 [mod] = 12;
		}
		dr_un (mod+1);
        }
} // "wrmd()"


/* Драйвер команды сравнения - '06' */
void cmpr(void)
{
 register i;
   if(zon_p("rb")) goto R;
   fread(DB,2,TR*2,fptr); fclose(fptr);
   t=0; if(adr&01) t=TR; // 2-nd half of zone only
 A: for(i=0; i<TR; i++) { setSIMD;
 W: if(getHBSL==0) goto W;
    DW[t+i]=getKUS&01777; // прием слога из УВУ
    clnHBSL; if(!(getRBUS)) goto Q; // sl/words
                        } // "for(i)"
  if(getRBUS) { if(!t) { t=TR; goto A; } } // 2-nd half
  else {
 Q:     i=adr>>1; _itoa(i,zone,10);
    if(!lin) { strcpy(zones[mod],zone); mcol[mod]=13; }
    else { strcpy(zones1[mod],zone); mcol1[mod]=13; } dr_un(mod+1);
       }
 R: return;
}; // "cmpr()"


/*  Формирование заголовков дорожек и выдача их в УВУ для операции '07' */
void rdhd(void)
{
 register i,ks,ch=0;
 UI nam=0404; // Addr_Marker for normal tracks
 UI ram=0406; // Addr_Marker for reserve tracks
 UI ksn=07777; // к.с.а. для 0-й дорожки
 UI ksr=07730; // к.с.а. для 200-й дорожки и далее
#define NA 2000 // Max_Address of normal tracks
 UI HDR[SL];    // Slogs for YBY

  for(i=0; i<SL; i++) HDR[i] = 0;
 // 1-st address:
  HDR[0] = (adr<NA) ? nam : ram; // slog 0
  ch = (adr%10) | ((adr/10)<<4); // N cyl/head
  HDR[1] = (ch>>3) & 0777;
  HDR[2] = (ch<<6) & 0777;
  ks = (adr<NA) ? ksn : ksr;
  ks ^= (ch>>8); ks ^= ((ch&0377)<<4);
  HDR[2] |= (ks>>10)&03;
  HDR[3]  = (ks>>01)&0777;
  HDR[4]  = (ks&01)<<8;
 // 2-nd address: miss 42-8=34 null bits (= 9*3 + 7 bits)
 // and then doubling with shift 1-st address
  for(i=0; i<5; i++) {
   HDR[i+8] |= (HDR[i]>>7)&03;
   HDR[i+9]  = (HDR[i]<<2)&0777;
                     }
 // 20 slogs are ready for transmission to YBY
   for(i=0; i<SL; i++)
    { setB(HDR[i]&0777); setSIMD;
 W:  if(getHBSL) clnHBSL; // гаш. <ХБСл>
      else goto W;  // wait of HBSL
     if(!(getRBUS)) goto R;
    } // "for(i)"
 R: drtime(7); setPRER;
}; // "rdhd()"
