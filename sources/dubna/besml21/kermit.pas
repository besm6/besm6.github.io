*NAME K
*TIME:01.00
*EXPRESS
*PAGE:999,LIST
*DISC:664/SYSTEM,KADANS
*FILE:ORB,67,W
*LIBRA:23,24
*CALL ALLMEMORY
*PERSO:67100
*NO LIST
*PASCAL
 (*=P-,T-*)
 PROGRAM KERMB6 (INPUT,OUTPUT,TERM,TERM1,CZONA,
                PPUT,PREAD,ANS,INFFIL,SETSTA);
 (*-----------------------------------------*)
 CONST
   ZONILGIS = 6144;
   MAXCHARKOD = 128;
   CHARSOH = '_001';
   CHAREOL = '_015';        (*  CTRM  *)
   CHARLF  = '_012';        (*  LF *)
   CHAROCTL = '_043';       (* SIMBOLIS '' *)
   BACKSPACE = '_010';       (*SIMBOLIS LEFT*)
   BLANKAS = '_040';          (*SIMBOLIS TARPAS*)
   BLANKAI = '      ';
   MAXTRY = 5;    (*MAX BAD TRIES*)
   (* BESM-6 KERMIT PARAMETERS*)
   MAXLEN = 72; (*MAX LENGTH OF PACKETS*)
   KORTILG = 83;
   TIMEOUT = 20;
   TIMEIN = 20;
   NPAD = 0;
   PADC = 32;
   EOL = 13;     (*ASCII CARRIAGE RETURN - CR*)
   MDNESILGIS = 40; (*FAILO UZSAKYMO ILGIS*)
 TYPE
   MJMDNESTYPE = PACKED ARRAY [1..MDNESILGIS] OF CHAR;
   REPORTTYPE = PACKED ARRAY [1..40] OF CHAR;
   STRINGTYPE = PACKED ARRAY [1..84] OF CHAR;
   ILGSTRTYPE = PACKED ARRAY [1..400] OF CHAR;
   FDBTYPE = RECORD
     INFNES : ARRAY [1..3] OF INTEGER;
     KLAIDOS : ARRAY [1..3] OF ALFA;
     REZRW : PACKED CHAR;
     INFNES8 : INTEGER;
   END;
   FINFTYPE = RECORD
     MJMDNESEJAS : MJMDNESTYPE;
     MJMDPAVILGIS : INTEGER;
     NAMEFILE : PACKED ARRAY [1..12] OF CHAR;
     LENGTHNAME,
     MJKODAS,    (*PADARYTAS IS MJMATNR IR PRADZONA*)
     MJMATNR,
     PRADZONA,
     ZONUSK      : INTEGER;
   END;
   PTYPE = RECORD
     LENDATA : INTEGER;
     SEQPACK : INTEGER;
     TYPEPACK : CHAR;
     DATAPACK : ILGSTRTYPE
   END;
   STATUSTYPE = RECORD
     KCONEC,                (*AR SUJUNGTA SU LINIJA       *)
     TABULIACIJA : BOOLEAN; (*AR TABULIACIJOS ZENKLA KEISTI I TARPUS*)
     OUTTAIMER,
     INTAIMER : INTEGER;
     PARITY : (EVEN,SPASE,MARK,ODD,NON);
     FAILORUSIS : (TEKSTAS,BINARINIS);
   END;
   ZONTIPAS = PACKED ARRAY [1..ZONILGIS] OF CHAR;
 VAR
   TERM,TERM1 : INTEGER; (*TERMINALO NR. PERDAVIMUI*)
   PPUT,PREAD : PTYPE;
   ANS : BOOLEAN;
   (*INFORMACIJA APIE FAILA,ESANTI ANT MJ/MD*)
   INFFIL : FINFTYPE;
   CZONA : ZONTIPAS;
   SETSTA : STATUSTYPE;
 (*-------------------------------------*)
 (*=F+*) PROCEDURE WZ(VAR I : INTEGER); FORTRAN;
 (*=F+*) PROCEDURE TAKTAP(VAR I : INTEGER); FORTRAN;
 (*=F+*) FUNCTION JFOCC(VAR I : INTEGER): INTEGER; FORTRAN;
 (*=F+*) FUNCTION ITCONB(VAR I : INTEGER): INTEGER; FORTRAN;
 (*=F+*) FUNCTION ITDISB(VAR I : INTEGER): INTEGER; FORTRAN;
 (*=F+*) FUNCTION ITOUTB(VAR NOMTERMIN : INTEGER;VAR STR : STRINGTYPE;
           VAR LEN,TIMEOUTAS : INTEGER) : INTEGER; FORTRAN;
 (*=F+*) FUNCTION ITINPB(VAR NOMTERMIN : INTEGER;VAR STR : STRINGTYPE;
           VAR LEN,TIMEOUTAS : INTEGER) : INTEGER; FORTRAN;
 (*=F+*) PROCEDURE WAITNS(VAR I : INTEGER); FORTRAN;
 (*=F+*) PROCEDURE KTEROU(VAR I : INTEGER); FORTRAN;
 (*=F+*) PROCEDURE KTERIN(VAR I : INTEGER); FORTRAN;
 PROCEDURE IVSTEI (VAR I :INTEGER); EXTERNAL;
 PROCEDURE I10I8 (VAR I10,I8 :INTEGER); EXTERNAL;
 PROCEDURE EDITRE(VAR MATNR,PRZON,ERR : INTEGER);
                                      EXTERNAL;
 PROCEDURE RETSAL(VAR ERR:INTEGER); EXTERNAL;
 PROCEDURE SALTRE(VAR I,J,K : INTEGER); EXTERNAL;
 PROCEDURE MTKREZ(VAR MREZI,TREZI : INTEGER;VAR KOMSIMB :
                  ALFA;VAR IFERR : INTEGER); EXTERNAL;
 PROCEDURE IVIDEN(VAR A:ALFA; VAR I,J : INTEGER); EXTERNAL;
 PROCEDURE IVZODI(VAR A:PACKED ARRAY [1..12] OF CHAR;VAR I:INTEGER;
                  VAR B:ALFA;VAR J:INTEGER); EXTERNAL;
 PROCEDURE IVASSK(VAR I,J,K,L : INTEGER); EXTERNAL;
 PROCEDURE IVSVIN(VAR SK,MINN,MAX,IV,ERR : INTEGER);
                                   EXTERNAL;
 PROCEDURE IVTRTX(VAR TEXT:MJMDNESTYPE;VAR SIMBSK:INTEGER;
                  VAR SIMBRINK:ALFA;VAR IVESTASIMB:INTEGER);
                                   EXTERNAL;
 PROCEDURE ZENATG; EXTERNAL;
 PROCEDURE IVEIL; EXTERNAL;
 FUNCTION ZENKLA : CHAR; EXTERNAL;
 FUNCTION IVEOL : BOOLEAN; EXTERNAL;
 FUNCTION IVEOF : BOOLEAN; EXTERNAL;
 FUNCTION MAKECH(T : INTEGER) : CHAR; EXTERNAL;
 FUNCTION UNCHAR(VAR T : CHAR) : INTEGER; EXTERNAL;
 FUNCTION CHECKS (VAR ST : ILGSTRTYPE; VAR FIRST,LAST : INTEGER) :
                                INTEGER;  EXTERNAL;
 (*-------------------------------------------------*)
 PROCEDURE KERMIT;
 (*-------------*)
 VAR
   ERRORTIME : INTEGER;  (*KLAIDA PAGAL TAIMERI*)
   (*PRANESIMAMS SKIRTAS MASYVAS IR JO ILGIS*)
   REPORT : REPORTTYPE;
   LENREPORT : INTEGER;
   (*REZIMU VALDYMO MASYVAI TERMINALAMS N1 IR N2*)
   TERM2 : INTEGER;
   (*APSIKEITIMO PAKETO STATUSAS*)
   STATUS : CHAR;
   (*ESANT STATUS = 'R' , NEGALIMA PRADETI PIRMAM SIUSTI*)
   LAUKTI :BOOLEAN;
   (*BANDYMO NUMERIS*)
   TRY,OLDTRY : INTEGER;
   (*PAKETO NUMERIS*)
   OLDSEQ,SEQ,NEWSEQ : INTEGER;
   (*KINTAMIEJI FAILO EILUTES FORMAVIMUI IS DUOMENU
     IR DUOMENU PAKETO FORMAVIMUI IS FAILO EILUCIU*)
   KIEKISVESTA,  (*KIEK EILUTES SIMBOLIU JAU ISVESTA I MJ/MD*)
   EILSIMB : INTEGER; (*KIEK NEPAKUOTU SIMBOLIU NUSKAITYTA  *)
                      (*      RASYMUI I MJ/MD               *)
   GLOBSIMBNR : INTEGER;
   TARPAS : INTEGER;
   CTRM : BOOLEAN;
   (*KINTAMIEJI IVESTO PAKETO ANALIZEI*)
   STRING : ILGSTRTYPE;
   LEN,
   NOMCHAR : INTEGER;
   INPUTSTR,            (*AR JAU IVESTA EILUTE*)
   FLAGINPUT : BOOLEAN; (*AR GALIMA VESTI NAUJA EILUTE*)
 (*----------------------------------------*)
 PROCEDURE DELBUF;
 (*---------------*)
 VAR
   I : INTEGER;
 BEGIN
 REPEAT
 UNTIL 0 = ITINPB(0,STRING,I,3)
 END;
 (*------------------------------------------*)
 PROCEDURE GROBK(VAR IFERR : INTEGER);
 (*----------------------------------*)
 VAR
   BUFPAVADINIMAS : MJMDNESTYPE;
   FDB : FDBTYPE;
   SIMBRINK : ALFA;
   I,J,SIMBSK,IVESTASIMB : INTEGER;
 BEGIN (*GROBK*)
 J := 0;
 REPEAT
   J := J + 1;
   IFERR := 0;
   SIMBRINK := BLANKAI;
   REPORT := 'ENTER BUFFER NAME                       ';
   LENREPORT :=17;
   SIMBSK := MDNESILGIS;
   KTEROU(REPORT,LENREPORT);
   IVTRTX(BUFPAVADINIMAS,SIMBSK,SIMBRINK,IVESTASIMB);
   FOR I := IVESTASIMB + 1 TO MDNESILGIS DO
     BUFPAVADINIMAS[I] := BLANKAS;
   IF IVESTASIMB = 0 THEN
     IFERR := 111
   ELSE
     BEGIN
     WITH FDB DO
       BEGIN
       FOR I := 1 TO 3 DO
         INFNES[I] := 0;
       FOR I := 1 TO 3 DO
         KLAIDOS[I] := BLANKAI;
       REZRW := 'R';
       INFNES8 := 0;
       END;
     IFERR := JFOCC(BUFPAVADINIMAS,FDB);
     IF IFERR <> 0 THEN
       BEGIN
       KTEROU(FDB.KLAIDOS,18)
       END
     ELSE
       WITH INFFIL DO
         BEGIN
         IF MJMDPAVILGIS <> 0 THEN
           TAKTAP(MJMATNR);
         MJMDNESEJAS := BUFPAVADINIMAS;
         MJMDPAVILGIS := IVESTASIMB;
         MJMATNR := FDB.INFNES[2]
         END
     END
 UNTIL (IFERR = 0) OR (J = 4);
 IF IFERR = 0 THEN
   BEGIN
   REPORT := 'O''KEY !!!                               ';
   LENREPORT := 9;
   (*=F+*) KTEROU(REPORT,LENREPORT);
   END
 END; (*GROBK*)
 (*-------------------------------------------*)
 PROCEDURE ZONENO(VAR ERROR : INTEGER);
 (*----------------------------------*)
 VAR
   IFERR,
   NO,I,L,K,J : INTEGER;
 BEGIN (*ZONENO*)
 WITH INFFIL DO
   BEGIN
   ERROR := 0;
   IF MJMDPAVILGIS = 0 THEN
     GROBK(ERROR);
   IF ERROR = 0 THEN
     BEGIN
     REPORT := 'ZONE NUMBER:                            ';
     LENREPORT := 13;
     (*=F+*) KTEROU(REPORT,LENREPORT);
     I := 0;
     IVASSK(PRADZONA,I,J,ERROR);
     IF (ERROR = 0) AND (PRADZONA >= 0) AND
        (PRADZONA <= 1743B) THEN
       MJKODAS := MJMATNR*10000B + PRADZONA
     ELSE
       ERROR := 1000;
     IF ERROR <> 0 THEN
       BEGIN
       REPORT := 'INVALID NUMBER                          ';
       LENREPORT := 14;
       (*=F+*) KTEROU(REPORT,LENREPORT);
       END
     END
   END (*WITH*)
 END; (*ZONENO*)
(*--------------------------------------*)
PROCEDURE BAIKEILUT;
VAR
  LIKOPOZ,TARPUSK,LIEKA,I : INTEGER;
  TARPUKODAS : CHAR;
  TARPINTI,VISKAS : BOOLEAN;
BEGIN
IF TARPAS > MAXCHARKOD THEN
  BEGIN
  GLOBSIMBNR := GLOBSIMBNR + 1;
  CZONA[GLOBSIMBNR] := CHR(TARPAS);
  IF GLOBSIMBNR = ZONILGIS THEN
    WITH INFFIL DO
      BEGIN
      WZ(MJKODAS,CZONA);
      GLOBSIMBNR := 0;
      MJKODAS := MJKODAS + 1;
      ZONUSK := ZONUSK + 1;
      END;
  KIEKISVESTA := KIEKISVESTA + 1;
  TARPAS := MAXCHARKOD
  END;
LIKOPOZ := KORTILG - EILSIMB + 1;
IF LIKOPOZ > 0 THEN
  BEGIN
  TARPUSK := MAXCHARKOD + LIKOPOZ;
  TARPUKODAS := CHR(TARPUSK);
  GLOBSIMBNR := GLOBSIMBNR + 1;
  CZONA[GLOBSIMBNR] := TARPUKODAS;
  IF GLOBSIMBNR = ZONILGIS THEN
    WITH INFFIL DO
      BEGIN
      WZ(MJKODAS,CZONA);
      GLOBSIMBNR := 0;
      MJKODAS := MJKODAS + 1;
      ZONUSK := ZONUSK + 1;
      END;
  KIEKISVESTA := KIEKISVESTA + 1;
  END;
GLOBSIMBNR := GLOBSIMBNR + 1;
CZONA[GLOBSIMBNR] := '_012';
IF GLOBSIMBNR = ZONILGIS THEN
  WITH INFFIL DO
    BEGIN
    WZ(MJKODAS,CZONA);
    GLOBSIMBNR := 0;
    MJKODAS := MJKODAS + 1;
    ZONUSK := ZONUSK + 1;
    END;
KIEKISVESTA := KIEKISVESTA + 1;
LIEKA := 6 - KIEKISVESTA MOD 6; TARPINTI := TRUE;
VISKAS := FALSE;
IF LIEKA = 6 THEN
  BEGIN VISKAS := TRUE; TARPINTI := FALSE END
ELSE
  IF LIEKA = 1 THEN TARPINTI := FALSE
  ELSE LIEKA := LIEKA - 1;
IF TARPINTI THEN
  FOR I := 1 TO LIEKA DO
    BEGIN
    GLOBSIMBNR := GLOBSIMBNR + 1;
    CZONA[GLOBSIMBNR] := '_040';
    IF GLOBSIMBNR = ZONILGIS THEN
      WITH INFFIL DO
        BEGIN
        WZ(MJKODAS,CZONA);
        GLOBSIMBNR := 0;
        MJKODAS := MJKODAS + 1;
        ZONUSK := ZONUSK + 1;
        END;
    END;
IF NOT VISKAS THEN
  BEGIN
  GLOBSIMBNR := GLOBSIMBNR + 1;
  CZONA[GLOBSIMBNR] := '_012';
  IF GLOBSIMBNR = ZONILGIS THEN
    WITH INFFIL DO
      BEGIN
      WZ(MJKODAS,CZONA);
      GLOBSIMBNR := 0;
      MJKODAS := MJKODAS + 1;
      ZONUSK := ZONUSK + 1;
      END;
  END;
EILSIMB := 0;  KIEKISVESTA := 0;
END;
 PROCEDURE ISVSIMB(VAR SIMB : CHAR);
 (*PROCEDURA ISVEDA VIENA SIMBOLI I ISORINE ATMINTI*)
 VAR
   I : INTEGER;
 BEGIN (*ISVSIMB*)
 EILSIMB := EILSIMB + 1;
 IF SIMB = BLANKAS THEN
   TARPAS := TARPAS + 1
 ELSE
   IF (SIMB = '_011') THEN   (*SIMB=HT*)
     BEGIN
     I := 8 - (EILSIMB - 1) MOD 8;
     TARPAS := TARPAS + I;
     EILSIMB := EILSIMB + I - 1
     END
 ELSE
   BEGIN
   IF TARPAS > MAXCHARKOD THEN
     BEGIN
     GLOBSIMBNR := GLOBSIMBNR + 1;
     CZONA[GLOBSIMBNR] := CHR(TARPAS);
     IF GLOBSIMBNR = ZONILGIS THEN
       WITH INFFIL DO
         BEGIN
         WZ(MJKODAS,CZONA);
         GLOBSIMBNR := 0;
         MJKODAS := MJKODAS + 1;
         ZONUSK := ZONUSK + 1;
         END;
     GLOBSIMBNR := GLOBSIMBNR + 1;
     CZONA[GLOBSIMBNR] := SIMB;
     IF GLOBSIMBNR = ZONILGIS THEN
       WITH INFFIL DO
         BEGIN
         WZ(MJKODAS,CZONA);
         GLOBSIMBNR := 0;
         MJKODAS := MJKODAS + 1;
         ZONUSK := ZONUSK + 1;
         END;
     KIEKISVESTA := KIEKISVESTA + 2;
     TARPAS := MAXCHARKOD
     END
   ELSE
     BEGIN
     GLOBSIMBNR := GLOBSIMBNR + 1;
     CZONA[GLOBSIMBNR] := SIMB;
     IF GLOBSIMBNR = ZONILGIS THEN
       WITH INFFIL DO
         BEGIN
         WZ(MJKODAS,CZONA);
         GLOBSIMBNR := 0;
         MJKODAS := MJKODAS + 1;
         ZONUSK := ZONUSK + 1;
         END;
     KIEKISVESTA := KIEKISVESTA + 1
     END;
   END
 END; (*ISVSIMB*)
 PROCEDURE CLOSEFILE;
 (*----------------*)
 VAR
   I : INTEGER;
   SIMB : CHAR;
 BEGIN (*CLOSEFILE*)
 IF EILSIMB > 0 THEN
   BAIKEILUT;
 REPORT := '*READ OLD                               ';
 FOR I := 1 TO 5 DO
   BEGIN
   SIMB := REPORT[I];
   ISVSIMB(SIMB)
   END;
 EILSIMB := EILSIMB + 1;
 GLOBSIMBNR := GLOBSIMBNR + 1;
 CZONA[GLOBSIMBNR] := '_040';
 IF GLOBSIMBNR = ZONILGIS THEN
   WITH INFFIL DO
     BEGIN
     WZ(MJKODAS,CZONA);
     GLOBSIMBNR := 0;
     MJKODAS := MJKODAS + 1;
     ZONUSK := ZONUSK + 1;
     END;
 KIEKISVESTA := KIEKISVESTA + 1;
 FOR I := 7 TO 9 DO
   BEGIN
   SIMB := REPORT[I];
   ISVSIMB(SIMB)
   END;
 BAIKEILUT;
 REPORT := '*END FILE                               ';
 FOR I := 1 TO 4 DO
   BEGIN
   SIMB := REPORT[I];
   ISVSIMB(SIMB)
   END;
 EILSIMB := EILSIMB + 1;
 GLOBSIMBNR := GLOBSIMBNR + 1;
 CZONA[GLOBSIMBNR] := '_040';
 IF GLOBSIMBNR = ZONILGIS THEN
   WITH INFFIL DO
     BEGIN
     WZ(MJKODAS,CZONA);
     GLOBSIMBNR := 0;
     MJKODAS := MJKODAS + 1;
     ZONUSK := ZONUSK + 1;
     END;
 KIEKISVESTA := KIEKISVESTA + 1;
 FOR I := 6 TO 9 DO
   BEGIN
   SIMB := REPORT[I];
   ISVSIMB(SIMB)
   END;
 BAIKEILUT;
 IF GLOBSIMBNR <> 0 THEN
   WITH INFFIL DO
     BEGIN
     WZ(MJKODAS,CZONA);
     ZONUSK := ZONUSK + 1;
     END;
 END; (*CLOSEFILE*)
 (*---------------------------------------*)
 PROCEDURE PRSTR;
 (*PRINT STRING IN FILE*)
 VAR
   I,J,K : INTEGER;
   SIMB : CHAR;
 BEGIN (*PRSTR*)
 I := 0;
 WITH PREAD DO
 WHILE I < LENDATA DO
   BEGIN
   I := I + 1;
   SIMB := DATAPACK[I];
   IF SIMB = CHAROCTL THEN
     BEGIN
     I := I + 1;
     SIMB := DATAPACK[I];
     IF SIMB = CHAROCTL THEN
       ISVSIMB(SIMB)
     ELSE
       IF SIMB = 'M' THEN  CTRM := TRUE
       ELSE
         IF (SIMB = 'J') AND CTRM THEN
           BEGIN
           CTRM := FALSE;
           BAIKEILUT
           END
         ELSE
           BEGIN
           SIMB := CHR (ORD(SIMB)-64);
           ISVSIMB(SIMB)
           END
     END
   ELSE
     BEGIN
     ISVSIMB(SIMB);
     IF EILSIMB >= KORTILG THEN
       BAIKEILUT
     END
   END (*WHILE*)
 END;   (*PRSTR*)
 (*--------------------------------------------*)
 PROCEDURE GETSTR(VAR ENDFILE : BOOLEAN);
 (*GET STRING OUT OF FILE WITH NAME =NAMEFILE=*)
 VAR
   LFILGIS,MAXDATALEN : INTEGER;
   SIMB : CHAR;
   NESUFORMUOTA : BOOLEAN;
 BEGIN (*GETSTR*)
 ENDFILE := FALSE; MAXDATALEN := MAXLEN - 5;(*MAX.DUOM.ILGIS*)
 LFILGIS := MAXDATALEN - 5;
 WITH PPUT DO
   BEGIN
   TYPEPACK := 'D';
   LENDATA := 0;  NESUFORMUOTA := TRUE;
   WHILE NESUFORMUOTA AND (LENDATA < MAXDATALEN) DO
     BEGIN
     SIMB := ZENKLA;
     IF IVEOF THEN
       BEGIN
       ENDFILE := TRUE;
       NESUFORMUOTA := FALSE;
       END
     ELSE (*NE FAILO GALAS*)
       IF IVEOL THEN
         BEGIN
         IF LENDATA <= LFILGIS THEN
           BEGIN
           DATAPACK[LENDATA + 1] := SIMB;
           DATAPACK[LENDATA + 2] := CHAROCTL;
           DATAPACK[LENDATA + 3] := 'M';
           DATAPACK[LENDATA + 4] := CHAROCTL;
           DATAPACK[LENDATA + 5] := 'J';
           LENDATA := LENDATA + 5;
           SIMB := ZENKLA
           END
         ELSE
           BEGIN
           NESUFORMUOTA := FALSE;
           ZENATG;
           END
         END
       ELSE (*IVEOL = FALSE*)
         IF (SIMB = CHAROCTL) OR (ORD(SIMB) < 32) THEN
           BEGIN
           IF LENDATA <= (MAXDATALEN - 2) THEN
             BEGIN
             DATAPACK[LENDATA + 1] := CHAROCTL;
             IF SIMB <> CHAROCTL THEN SIMB := CHR(ORD(SIMB)+64);
             DATAPACK[LENDATA + 2] := SIMB;
             LENDATA := LENDATA + 2;
             END
           ELSE
             BEGIN
             NESUFORMUOTA := FALSE;
             ZENATG;
             END
           END
         ELSE (*PAPRASTAS SIMBOLIS*)
           BEGIN
           LENDATA := LENDATA + 1;
           DATAPACK[LENDATA] := SIMB;
           END;
     END; (*WHILE*)
   END; (*WITH*)
 END; (*GETSTR*)
 (*---------------------------------------------------*)
 PROCEDURE PACIN;
 (*-------------------------------------*)
 VAR
   I : INTEGER;
 BEGIN (*PACIN*)
 ERRORTIME := 0;
 WITH SETSTA DO
   BEGIN
   IF 0 = ITINPB(0,STRING,LEN,INTAIMER) THEN
     BEGIN
     ERRORTIME := 333;
     LEN := 0
     END;
   IF (FAILORUSIS = TEKSTAS) OR (PARITY <> NON) THEN
     FOR I := 1 TO LEN DO
     STRING[I] := CHR(ORD(STRING[I]) MOD 128);
   INPUTSTR := TRUE;
   NOMCHAR := 0
   END (*WITH*)
 END;  (*PACIN*)
 (*-----------------------------*)
 PROCEDURE PUTPACK;
 (*----------------*)
 VAR
   I,FIRST,LAST : INTEGER;
 BEGIN (*PUTPACK*)
 WITH PPUT DO
   BEGIN
   STRING [1] := CHARSOH;
   STRING [2] := MAKECH(LENDATA + 3);
   STRING [3] := MAKECH(SEQ);
   STRING [4] := TYPEPACK;
   IF LENDATA > 0 THEN
     FOR I := 1 TO LENDATA DO
       STRING [I+4] := DATAPACK[I];
   FIRST := 2;
   LAST := LENDATA + 4;
   I := CHECKS(STRING,FIRST,LAST);
   STRING [LENDATA + 6] := CHAREOL;
   STRING [LENDATA + 5] := MAKECH(I);
   LEN := LENDATA + 6;
   END; (*WITH*)
 WITH SETSTA DO
   I := ITOUTB(0,STRING,LEN,OUTTAIMER)
 END;   (*PUTPACK*)
 (*------------------------------*)
 PROCEDURE GETCH (VAR S : CHAR);
 (*----------------------------*)
 VAR
   I : INTEGER;
   C : CHAR;
 BEGIN (*GETCH*)
 IF NOT INPUTSTR AND FLAGINPUT THEN
   BEGIN
   PACIN;
   END;
 IF NOMCHAR >= LEN THEN
   BEGIN
   I := 0;
   INPUTSTR := FALSE
   END
 ELSE
   BEGIN
   NOMCHAR := NOMCHAR + 1;
   C := STRING [NOMCHAR];
   I := ORD(C);
   END;
 S := CHR(I);
 END;     (*GETCH*)
 (*--------------------------------------*)
 PROCEDURE NEWR;
 (*TRIE OF READ NEW PACKET*)
 BEGIN (*NEWR*)
 TRY := 0;  OLDTRY := 0;
 SEQ := NEWSEQ;
 NEWSEQ := (NEWSEQ + 1) MOD 64
 END;   (*NEWR*)
 (*--------------------------------------*)
 PROCEDURE NEWS;
 (*TRIE OF SEND NEW PACKET*)
 BEGIN (*NEWS*)
 TRY := 0;  OLDTRY := 0;
 OLDSEQ := SEQ;
 SEQ := (SEQ + 1) MOD 64
 END;   (*NEWS*)
 (*--------------------------------------*)
 PROCEDURE NEXTTRY;
 (*---------------*)
 BEGIN (*NEXTTRY*)
 TRY := TRY + 1;  DELBUF;
 IF TRY > MAXTRY THEN STATUS := 'A'
 END;   (*NEXTTRY*)
 (*---------------------------------------*)
 PROCEDURE NEXTOLD;
 (*---------------*)
 BEGIN (*NEXTOLD*)
 OLDTRY := OLDTRY + 1;  DELBUF;
 IF OLDTRY > MAXTRY THEN STATUS := 'A'
 END;   (*NEXTOLD*)
 PROCEDURE RPACK;
 (*--------------*)
 VAR
   S : CHAR;
   I,FIRST,LAST : INTEGER;
 BEGIN (*RPACK*)
 INPUTSTR := FALSE;
 FLAGINPUT := TRUE;
 IF NOT LAUKTI THEN PUTPACK;
 WITH PREAD DO
   BEGIN
   REPEAT
     GETCH(S);
   UNTIL (S = CHARSOH) OR (ERRORTIME <> 0);
   FLAGINPUT := FALSE;
   IF ERRORTIME <> 0 THEN
     WHILE (S <> CHARSOH) AND (S <> '_000') DO
       GETCH(S);
   IF S = CHARSOH THEN
     BEGIN
     GETCH(S);
     LENDATA := UNCHAR(S) - 3;
     FIRST := NOMCHAR;
     LAST := FIRST + LENDATA + 2;
     GETCH(S);
     SEQPACK := UNCHAR(S);
     GETCH(S);
     TYPEPACK := S;
     FOR I := 1 TO LENDATA DO
       BEGIN
       GETCH(S);
       DATAPACK [I] := S
       END;
     GETCH(S);
     I := CHECKS(STRING,FIRST,LAST);
     ANS := I = UNCHAR(S)
     END
   ELSE
     ANS := FALSE
   END;
 END;   (*RPACK*)
 (*----------------------------------------------*)
 FUNCTION RACK : BOOLEAN;
 (*READ ACK OR NAK*)
 VAR
   VALUEACK : BOOLEAN;
 BEGIN (*RACK*)
 VALUEACK := FALSE;
 RPACK;
 WITH PREAD DO
   BEGIN
   IF ANS THEN
     BEGIN
     IF ((TYPEPACK = 'Y') AND (SEQPACK = SEQ)) OR
        ((TYPEPACK = 'N') AND
        (SEQPACK = (SEQ + 1) MOD 64)) THEN
       BEGIN
       NEWS;
       VALUEACK := TRUE
       END
     ELSE
       IF (TYPEPACK = 'N') THEN  NEXTTRY
       ELSE  STATUS := 'A'
     END
   ELSE
     NEXTTRY
   END; (*WITH*)
 RACK := VALUEACK
 END; (*RACK*)
 (*--------------------------------------------*)
 FUNCTION RACKIN : BOOLEAN;
 (*READ ACK WITH SEND-INIT PACKET*)
 VAR
   VALUEACKIN : BOOLEAN;
 BEGIN (*RACKIN*)
 VALUEACKIN := FALSE;
 RPACK;
 WITH PREAD DO
   BEGIN
   IF ANS THEN
     BEGIN
     IF (TYPEPACK = 'Y') AND (LENDATA >= 6) AND
                               (SEQPACK = 0) THEN
       BEGIN
       (*WORK WITH DATA OF SEND-INIT PACKET*)
       (*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*)
       NEWS;
       VALUEACKIN := TRUE;
       END
     ELSE
       IF (TYPEPACK = 'N') OR (TYPEPACK = 'Y') THEN
         NEXTTRY
       ELSE  STATUS := 'A'
     END
   ELSE
     NEXTTRY
   END; (*WITH*)
 RACKIN := VALUEACKIN
 END; (*RACKIN*)
 (*--------------------------------------*)
 PROCEDURE SNAK;
 (* SEND NAK *)
 BEGIN (*SNAK*)
 WITH PPUT DO
   BEGIN
   LENDATA := 0;
   TYPEPACK := 'N'
   END
 END; (*SNAK*)
 (*--------------------------------------*)
 PROCEDURE SACK;
 (* SEND ACK *)
 BEGIN (*SACK*)
 WITH PPUT DO
   BEGIN
   LENDATA := 0;
   TYPEPACK := 'Y'
   END
 END;  (*SACK*)
 (*-----------------------------------*)
 PROCEDURE SPAR;
 (*-------------*)
 (*DATE OF SEND-INIT PACKET*)
 (*------------------------*)
 BEGIN (*SPAR*)
 WITH PPUT DO
   BEGIN
   DATAPACK [1] := MAKECH(MAXLEN);
   DATAPACK [2] := MAKECH(TIMEOUT);
   DATAPACK [3] := MAKECH(NPAD);
   DATAPACK [4] := MAKECH(PADC);
   DATAPACK [5] := MAKECH(EOL);
   DATAPACK [6] := CHAROCTL
   END (*WITH*)
 END;   (*SPAR*)
 (*---------------------------------------*)
 PROCEDURE SACKIN;
 (* SEND ACK WITH SEND-INIT PACKET*)
 BEGIN (*SACKIN*)
 SPAR;
 WITH PPUT DO
   BEGIN
   LENDATA := 6;
   TYPEPACK := 'Y'
   END
 END; (*SACKIN*)
 (*--------------------------------------*)
 PROCEDURE RDATA;
 (* READ PACKET WITH DATA *)
 VAR
   I : INTEGER;
 BEGIN (*RDATA*)
 RPACK;
 WITH PREAD DO
 IF ANS THEN
   BEGIN
   IF (TYPEPACK <> 'F') AND (TYPEPACK <> 'D') AND
                            (TYPEPACK <> 'Z') THEN
     STATUS := 'A'
   ELSE
    CASE TYPEPACK OF
     'F' :  BEGIN  SACK; NEXTOLD END;
     'D' :  IF NEWSEQ = SEQPACK THEN
              BEGIN
              SACK;  NEWR;
              (*----------------------------------*)
              (*  WORK WITH DATA                  *)
              PRSTR;
              (*----------------------------------*)
              END
            ELSE
              BEGIN  SACK;  NEXTOLD  END;
     'Z' :  BEGIN
            SACK;  NEWR;
            (*---------------------------------*)
            (*WORK WITH DATA OF LAST PACKET    *)
            (*---------------------------------*)
            STATUS := 'F'
            END;
     END (*CASE*)
   END
 ELSE
   BEGIN  SNAK;  NEXTTRY  END
 END; (*RDATA*)
 (*---------------------------------------*)
 PROCEDURE RFNAME;
 (* READ FILE NAME *)
 VAR
   I,J : INTEGER;
   MARKER : ALFA;
   SIMB : CHAR;
 BEGIN (*RFNAME*)
 RPACK;
 WITH PREAD DO
 IF ANS THEN
   BEGIN
   IF (TYPEPACK <> 'C') AND (TYPEPACK <> 'F') AND
      (TYPEPACK <> 'Z') AND (TYPEPACK <> 'B') THEN
     STATUS := 'A'
   ELSE
   CASE TYPEPACK OF
     'S' :  BEGIN  SACKIN;  NEXTOLD  END;
     'F' :  BEGIN
            SACK; NEWR;
            STATUS := 'D';
            (*--------------------------------*)
            (*WORK WITH FILE NAME             *)
            (*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*)
            (*--------------------------------*)
            END;
     'Z' :  BEGIN
            SACK;  NEXTOLD;
            END;
     'B' :  WITH SETSTA DO
            BEGIN
            SACK; NEWR;
            I := INTAIMER;
            INTAIMER := 3;
            REPEAT
              RPACK
            UNTIL ERRORTIME <> 0;
            INTAIMER := I;
            STATUS := 'C'
            END;
   END  (*CASE*)
   END
 ELSE
   BEGIN  SNAK;  NEXTTRY END
 END; (*RFNAME*)
 (*--------------------------------------*)
 PROCEDURE RINIT;
 (* READ SEND-INIT PACKET *)
 BEGIN (*RINIT*)
 RPACK;
 WITH PREAD DO
   BEGIN
   LAUKTI := FALSE;
   IF ANS THEN
     BEGIN
     IF TYPEPACK = 'S' THEN
       BEGIN
       SACKIN;
       STATUS := 'F';
       NEWR
       END
     ELSE STATUS := 'A'
     END
   ELSE
     BEGIN
     SNAK;  NEXTTRY;
     END
   END (*WITH*)
 END; (*RINIT*)
 (*----------------------------------------*)
 PROCEDURE SBREAK;
 (*SEND BREAK*)
 BEGIN (*SBREAK*)
 WITH PPUT DO
   BEGIN
   LENDATA := 0;
   TYPEPACK := 'B'
   END;
 REPEAT
   IF RACK THEN STATUS := 'C'
 UNTIL (STATUS = 'A') OR (STATUS = 'C')
 END; (*SBREAK*)
 (*-----------------------------------------*)
 PROCEDURE SEOF;
 (*SEND END OF FILE*)
 BEGIN (*SEOF*)
 WITH PPUT DO
   BEGIN
   LENDATA := 0;
   TYPEPACK := 'Z'
   END;
 REPEAT
   IF RACK THEN  STATUS := 'B'
 UNTIL(STATUS = 'A') OR (STATUS = 'B')
 END; (*SEOF*)
 (*--------------------------------------*)
 PROCEDURE SDATA;
 (*SEND DATA OF FILE*)
 VAR
   PERDUOTA,ENDFILE : BOOLEAN;
 BEGIN (*SDATA*)
 (*GET STRING OF FILE*)
 GETSTR(ENDFILE);
 REPEAT
   PERDUOTA := RACK
 UNTIL (STATUS = 'A') OR PERDUOTA;
 IF PERDUOTA AND ENDFILE THEN
   STATUS := 'Z';
 END; (*SDATA*)
 (*-----------------------------------------------*)
 PROCEDURE SFNAME;
 (*SEND FILE NAME*)
 VAR
   I : INTEGER;
 BEGIN (*SFNAME*)
 WITH PPUT,INFFIL DO
   BEGIN
   FOR I := 1 TO LENGTHNAME DO
     DATAPACK[I] := NAMEFILE[I];
   LENDATA := LENGTHNAME;
   TYPEPACK := 'F'
   END; (*WITH*)
 REPEAT
   IF RACK THEN  STATUS := 'D'
 UNTIL (STATUS = 'A') OR (STATUS = 'D')
 END; (*SFNAME*)
 (*------------------------------------------------*)
 PROCEDURE SINIT;
 (*SEND SEND-INIT PACKET*)
 BEGIN (*SINIT*)
 SPAR;
 WITH PPUT DO
   BEGIN
   LENDATA := 6;
   TYPEPACK := 'S'
   END;
 REPEAT
   IF RACKIN THEN  STATUS := 'F'
 UNTIL (STATUS = 'A') OR (STATUS = 'F');
 END; (*SINIT*)
 PROCEDURE RFILES;
 (*--------------*)
 VAR
   I,J,IFERR : INTEGER;
   GOAL : BOOLEAN;
 BEGIN (*RFILES*)
 STATUS := 'R';
 ZONENO(IFERR);
 IF IFERR = 0 THEN
   BEGIN
   TARPAS := MAXCHARKOD;
   INFFIL.ZONUSK := 0;
   KIEKISVESTA := 0;   EILSIMB := 0;
   GLOBSIMBNR := 0;
   CTRM := FALSE;
   TRY := 0;  OLDTRY := 0;
   SEQ := 0;  NEWSEQ := 0;
   LAUKTI := TRUE;
   LENREPORT := 18;
   REPORT := 'RECEIVE WAITING...                      ';
   (*=F+*) KTEROU(REPORT,LENREPORT);
   GOAL := FALSE;
   REPEAT
   CASE STATUS OF
     'R' : RINIT;  (*READ SEND-INIT PACKET*)
     'F' : RFNAME; (*READ FILE-HEADER PACKET*)
     'D' : RDATA;  (*READ FILE-DATA PACKET*)
     'C' : BEGIN   (*COMPLETE NORMALLY*)
           GOAL := TRUE
           END;
     'A' : BEGIN   (*PROCESS ABORTED*)
           DELBUF;
           GOAL := TRUE
           END;
   END (*CASE*)
   UNTIL GOAL;
   CLOSEFILE;
   IF STATUS = 'C' THEN
     BEGIN
     REPORT := 'COMPLETED NORMALLY                      '
     END
   ELSE
     BEGIN
     REPORT := 'PROCESS ABORTED                         '
     END;
   LENREPORT := 18;
   (*=F+*) KTEROU(REPORT,LENREPORT);
   REPORT := 'WRITED      ZONES                       ';
   I10I8(INFFIL.ZONUSK,I);
   J := 11;
   REPEAT
     REPORT[J] := CHR(ORD('0') + (I MOD 10));
     I := I DIV 10;
     J := J - 1;
   UNTIL I = 0;
   LENREPORT := 17;
   (*=F+*) KTEROU(REPORT,LENREPORT);
   END
 END;  (*RFILES*)
 (*-----------------------------------------*)
 PROCEDURE SFILES;
 (*-----------------*)
 VAR
   GOAL : BOOLEAN;
   MREZI,TREZI,
   IFERR,I : INTEGER;
   RINKINYS : ALFA;
 BEGIN (*SFILES*)
 STATUS := 'S';
 TRY := 0;  OLDTRY := 0;
 SEQ := 0;  NEWSEQ := 0;
 REPORT := 'ENTER FILE NAME:                        ';
 LENREPORT := 16;
 (*=F+*) KTEROU(REPORT,LENREPORT);
 I := 12;   RINKINYS := '.     ';
 IVZODI(INFFIL.NAMEFILE,I,RINKINYS,INFFIL.LENGTHNAME);
 ZONENO(IFERR);
 IF IFERR = 0 THEN
   BEGIN
   WITH INFFIL DO
     EDITRE(MJMATNR,PRADZONA,IFERR);
   MREZI := 1;  TREZI := 2;  RINKINYS := BLANKAI;
   MTKREZ(MREZI,TREZI,RINKINYS,IFERR);
   REPORT := 'WAITING 10 SECOND BEFORE SENDING        ';
   LENREPORT := 32;
   (*=F+*) KTEROU(REPORT,LENREPORT);
   I := 10;   WAITNS(I);
   GOAL := FALSE;
   REPEAT
     CASE STATUS OF
       'S' : SINIT;       (*SEND SEND-INIT PACKET*)
       'F' : SFNAME;      (*SEND FILE NAME*)
       'D' : SDATA;       (*SEND DATA OF FILE*)
       'Z' : SEOF;        (*SEND END OF FILE*)
       'B' : SBREAK;      (*SEND BREAK*)
       'C',               (*COMPLETE NORMALLY*)
       'A' : BEGIN        (*PROCESS ABORTED*)
             GOAL := TRUE;
             DELBUF
             END
     END (*CASE*)
   UNTIL GOAL;
   RETSAL(IFERR);
   IF STATUS = 'C' THEN
     REPORT := 'COMPLETED NORMALLY                      '
   ELSE
     REPORT := 'PROCESS ABORTED                         ';
   LENREPORT := 18;
   (*=F+*) KTEROU(REPORT,LENREPORT);
   END
 END; (*SFILES*)
 (*-------------------------------------------*)
 PROCEDURE DISCON;
 (*--------------------*)
 VAR
   I : INTEGER;
 BEGIN (*DISCON*)
 WITH SETSTA DO
 IF KCONEC THEN
   BEGIN
   IF ITDISB(TERM2) = 0 THEN
     KCONEC := FALSE;
   REPORT := 'DISCONECTED                             ';
   LENREPORT := 11;
   END
 ELSE
   BEGIN
   REPORT := 'NON CONECTED                            ';
   LENREPORT := 12;
   END;
 (*=F+*) KTEROU(REPORT,LENREPORT);
 END; (*DISCON*)
 PROCEDURE CONECT;
 (*-----------------*)
 BEGIN (*CONECT*)
 WITH SETSTA DO
   IF KCONEC THEN
     BEGIN
     REPORT := 'CONECTED                                ';
     REPORT[10] := CHR(ORD('0') + (TERM2 DIV 10));
     REPORT[11] := CHR(ORD('0') + (TERM2 MOD 10));
     LENREPORT := 11;
     (*=F+*) KTEROU(REPORT,LENREPORT);
     END
   ELSE
     BEGIN
     REPORT := 'NON CONECTED                            ';
     LENREPORT := 12;
     (*=F+*) KTEROU(REPORT,LENREPORT);
     END
 END; (*CONECT*)
 PROCEDURE SPAUSDINTI;
 (*-------------------*)
 VAR
   I,J,IFERR,MREZI,TREZI : INTEGER;
   SIMB : CHAR;
   RINKINYS : ALFA;
   BAIGTI : BOOLEAN;
 BEGIN (*SPAUSDINTI*)
 ZONENO(IFERR);
 IF IFERR = 0 THEN
   BEGIN
   WITH INFFIL DO
     EDITRE(MJMATNR,PRADZONA,IFERR);
   MREZI := 1;  TREZI := 2;  RINKINYS := BLANKAI;
   MTKREZ(MREZI,TREZI,RINKINYS,IFERR);
   I := 0;  BAIGTI := FALSE;
   REPEAT
     I := (I + 1) MOD 10; (*SKAICIUOJA EILUTES*)
     LEN := 0;  SIMB := ZENKLA;
     IF NOT IVEOF THEN
       BEGIN
       ZENATG;
       REPEAT
         SIMB := ZENKLA;
         LEN := LEN + 1;
         STRING[LEN] := SIMB;
       UNTIL IVEOL;
       SIMB := ZENKLA
       END
     ELSE
       BAIGTI := TRUE;
     IF LEN <> 0 THEN
       BEGIN
       (*=F+*) KTEROU(STRING,LEN);
       END;
     IF NOT BAIGTI AND (I = 0) THEN
       BEGIN
       STRING[1] := CHAROCTL;  STRING[2] := BACKSPACE;
       LEN := 2;
 (*=F+*) J := ITOUTB(0,STRING,LEN,15);
 (*=F+*) J := ITINPB(0,STRING,LEN,0);
       BAIGTI := LEN <> 0;
       STRING[1] := CHARLF;
 (*=F+*) J := ITOUTB(0,STRING,1,15)
       END
    UNTIL BAIGTI;
    RETSAL (IFERR)
   END
 END; (*SPAUSDINTI*)
 (*------------------------------------------*)
 PROCEDURE HELPAS;
 (*---------------*)
 VAR
   I : INTEGER;
 BEGIN (*HELPAS*)
 I := 9;
 REPORT := '  M E N U                               ';
 KTEROU(REPORT,I);
 I := 11;
 REPORT := '  ---------                             ';
 KTEROU(REPORT,I);
 I := 3;
 REPORT := 'SET                                     ';
 KTEROU(REPORT,I);
 I := 4;
 REPORT := 'SHOW                                    ';
 KTEROU(REPORT,I);
 I := 6;
 REPORT := 'S[END]                                  ';
 KTEROU(REPORT,I);
 I := 8;
 REPORT := 'R[ECEIV]                                ';
 KTEROU(REPORT,I);
 I := 6;
 REPORT := 'T[YPE]                                  ';
 KTEROU(REPORT,I);
 I := 28;
 REPORT := 'BUF[FER] - ENTER BUFFER NAME            ';
 KTEROU(REPORT,I);
 I := 6;
 REPORT := 'H[ELP]                                  ';
 KTEROU(REPORT,I);
 I := 6;
 REPORT := 'Q[UIT]                                  ';
 KTEROU(REPORT,I);
 END; (*HELPAS*)
 PROCEDURE SHOWPAR;
 (*-----------*)
 VAR
   I,J,N,MIN,MAX,TERMNR,IFERR : INTEGER;
   RAKTAS : ALFA;
   ERROR : BOOLEAN;
 BEGIN (*SETPAR*)
 WITH SETSTA DO
   BEGIN
   IF KCONEC THEN
     BEGIN
     REPORT := 'LINE                                    ';
     REPORT[14] := CHR(ORD('0') + (TERM2 DIV 10));
     REPORT[15] := CHR(ORD('0') + (TERM2 MOD 10));
     LENREPORT := 15;
     (*=F+*) KTEROU(REPORT,LENREPORT);
     END
   ELSE
     BEGIN
     LENREPORT := 16;
     REPORT := 'LINE         OFF                        ';
     (*=F+*) KTEROU(REPORT,LENREPORT);
     END;
   IF TABULIACIJA THEN
     BEGIN
     LENREPORT := 15;
     REPORT := 'TABULATION   ON                         ';
     (*=F+*) KTEROU(REPORT,LENREPORT);
     END
   ELSE
     BEGIN
     LENREPORT := 16;
     REPORT := 'TABULATION   OFF                        ';
     (*=F+*) KTEROU(REPORT,LENREPORT);
     END;
   REPORT := 'TIMEOUT                                 ';
   REPORT[14] := CHR(ORD('0') + (INTAIMER DIV 10));
   REPORT[15] := CHR(ORD('0') + (INTAIMER MOD 10));
   LENREPORT := 15;
   (*=F+*) KTEROU(REPORT,LENREPORT);
   LENREPORT := 19;
   REPORT := 'PARITY                                  ';
   CASE PARITY OF
     EVEN   : RAKTAS := 'EVEN  ';
     SPASE  : RAKTAS := 'SPASE ';
     MARK   : RAKTAS := 'MARK  ';
     ODD    : RAKTAS := 'ODD   ';
     NON    : RAKTAS := 'NON   ';
   END;
   FOR I := 14 TO 19 DO
     REPORT[I] := RAKTAS[I-13];
   (*=F+*) KTEROU(REPORT,LENREPORT);
   LENREPORT := 19;
   REPORT := 'FILE                                    ';
   CASE FAILORUSIS OF
     TEKSTAS  : RAKTAS := 'TEXT  ';
     BINARINIS: RAKTAS := 'BINARY';
   END;
   FOR I := 14 TO 19 DO
     REPORT[I] := RAKTAS[I-13];
   (*=F+*) KTEROU(REPORT,LENREPORT);
   LENREPORT := 12;
   REPORT := 'BUFFER NAME:                            ';
   (*=F+*) KTEROU(REPORT,LENREPORT);
   WITH INFFIL DO
     IF MJMDPAVILGIS <> 0 THEN
       (*=F+*) KTEROU(MJMDNESEJAS,MJMDPAVILGIS);
   END
 END; (*SHOWPAR*)
 PROCEDURE SETPAR;
 (*-----------*)
 VAR
   I,J,N,MIN,MAX,TERMNR,IFERR : INTEGER;
   RAKTAS : ALFA;
   ERROR : BOOLEAN;
 BEGIN (*SETPAR*)
 ERROR := FALSE OR IVEOL;
 WITH SETSTA DO
 IF NOT ERROR THEN
   BEGIN
   RAKTAS := BLANKAI;  I := 6;
   IVIDEN(RAKTAS,I,J);
   CASE RAKTAS OF
     'TAB   ',
     'TABULI',
     'T     ' :    BEGIN
                   IF IVEOL THEN
                     TABULIACIJA := NOT TABULIACIJA
                   ELSE
                     BEGIN
                     RAKTAS := BLANKAI; I := 6;
                     IVIDEN(RAKTAS,I,J);
                     CASE RAKTAS OF
                       'OFF   ': TABULIACIJA := TRUE;
                       'ON    ': TABULIACIJA := FALSE;
                       END
                     END
                   END;
     'LINE  ' :    BEGIN
                   IF IVEOL THEN
                     BEGIN
                     ERROR := TRUE
                     END
                   ELSE
                     BEGIN
                     MIN := 1;  MAX := 54;
                     IVSVIN(TERMNR,MIN,MAX,I,IFERR);
                     IF IFERR = 0 THEN
                       BEGIN
                       TERM2 := TERMNR;
                       N := 2;
                       I := 0;
                       REPEAT
                         I := I + 1;
                         IF ITCONB(TERM2) = 0 THEN
                           WAITNS(N)
                         ELSE
                           IF TERM2 <> TERM1 THEN   KCONEC := TRUE
                       UNTIL (I = 3) OR KCONEC;
                       IF KCONEC THEN
                         BEGIN
                         TERM := TERM2;
                         LENREPORT := 8;
   REPORT := 'CONECTED                                ';
                         (*=F+*) KTEROU(REPORT,LENREPORT);
                         END
                       ELSE
                         BEGIN
                         LENREPORT := 9;
   REPORT := 'LINE BUSY                               ';
                         (*=F+*) KTEROU(REPORT,LENREPORT);
                         END
                       END
                     ELSE  (*IFERR <> 0*)
                       BEGIN
                         LENREPORT := 18;
   REPORT := 'INVALID LINE NUMBER                     ';
                         (*=F+*) KTEROU(REPORT,LENREPORT);
                       END
                     END
                   END;
     'TIMEOU' :    BEGIN
                   IF IVEOL THEN
                     BEGIN
                     ERROR := TRUE
                     END
                   ELSE
                     BEGIN
                     MIN := 1;  MAX := 30;
                     IVSVIN(N,MIN,MAX,I,IFERR);
                     IF IFERR = 0 THEN
                       BEGIN
                       INTAIMER := N;
                       END
                     END
                   END;
     'PAR   ',
     'PARITY' :    BEGIN
                   IF IVEOL THEN
                     ERROR := TRUE
                   ELSE
                     BEGIN
                     RAKTAS := BLANKAI; I := 6;
                     IVIDEN(RAKTAS,I,J);
                     CASE RAKTAS OF
                       'E     ',
                       'EVEN  ': PARITY := EVEN;
                       'SP    ',
                       'SPASE ': PARITY := SPASE;
                       'MARK  ': PARITY := MARK;
                       'ODD   ': PARITY := ODD;
                       'NON   ': PARITY := NON;
                       END
                     END
                   END;
     'FIL   ',
     'FILE  ' :    BEGIN
                   IF IVEOL THEN
                     ERROR := TRUE
                   ELSE
                     BEGIN
                     RAKTAS := BLANKAI; I := 6;
                     IVIDEN(RAKTAS,I,J);
                     CASE RAKTAS OF
                       'TEXT  ': FAILORUSIS := TEKSTAS;
                       'BIN   ',
                       'BINARY': FAILORUSIS := BINARINIS;
                       END
                     END
                   END;
   END; (*CASE*)
   END;
   IF ERROR THEN
     BEGIN
     LENREPORT := 17;
     REPORT := 'INVALID PARAMETER                       ';
     (*=F+*) KTEROU(REPORT,LENREPORT);
     END
 END; (*SETPAR*)
 (*----------------------------------------*)
 PROCEDURE INIKER(VAR ERR : INTEGER);
 (*-------------------*)
 VAR
   I,J,K : INTEGER;
 BEGIN (*INIKER*)
 ERR := 0;
 I := -1;
 TERM1 := ITCONB(I);   TERM := TERM1;
 TERM2 := TERM1;
 IF TERM = 0 THEN
   ERR := 1000  (*NESUJUNGE*)
 ELSE
   BEGIN
   LENREPORT := 39;
   REPORT := ' ************************************** ';
   (*=F+*) KTEROU(REPORT,LENREPORT);
   REPORT := ' *     K E R M I T  -  B E S M 6      * ';
   (*=F+*) KTEROU(REPORT,LENREPORT);
   REPORT := ' * VERSION 1.0  DUBNA - VILNIUS 1988  * ';
   (*=F+*) KTEROU(REPORT,LENREPORT);
   REPORT := ' *- - - - - - - - - - - - - - - - - - * ';
   (*=F+*) KTEROU(REPORT,LENREPORT);
   REPORT := ' ************************************** ';
   (*=F+*) KTEROU(REPORT,LENREPORT);
   ERRORTIME := 0;
   I := 0;
   IVSTEI (I);
   I := 1;  J := 0;
   SALTRE(I,J,K);
   WITH SETSTA DO
     BEGIN
     TABULIACIJA := FALSE;
     KCONEC := FALSE;
     INTAIMER := TIMEIN;
     OUTTAIMER := TIMEOUT;
     PARITY := EVEN;
     FAILORUSIS := TEKSTAS;
     END;
   WITH INFFIL DO
     BEGIN
     MJMDPAVILGIS := 0
     END;
   END
 END; (*INIKER*)
 (*----------------------------------------------*)
 PROCEDURE MAINKER;
 (*---------------*)
 VAR
   ORDER : ALFA;
   I,J,L,ERR : INTEGER;
   GOAL : BOOLEAN;
 BEGIN (*MAINKER*)
 INIKER(ERR);
 GOAL := FALSE;
 IF ERR = 0 THEN
 REPEAT
   REPORT := 'KERMIT-BESM>                            ';
   LENREPORT := 13;
   (*=F+*) KTEROU(REPORT,LENREPORT);
   I := 6;  ORDER := BLANKAI;
   IVEIL;
   IVIDEN(ORDER,I,J);
   IF (ORDER = 'C     ') OR (ORDER = 'CONECT') OR
      (ORDER = 'D     ') OR (ORDER = 'DISCON') OR
      (ORDER = 'S     ') OR (ORDER = 'SEND  ') OR
      (ORDER = 'R     ') OR (ORDER = 'REC   ') OR
      (ORDER = 'RECEIV') OR (ORDER = 'K     ') OR
      (ORDER = 'END   ') OR (ORDER = 'QUIT  ') OR
      (ORDER = 'Q     ') OR (ORDER = 'TYPE  ') OR
      (ORDER = 'T     ') OR (ORDER = 'SET   ') OR
      (ORDER = 'BUFFER') OR (ORDER = 'BUF   ') OR
      (ORDER = 'STA   ') OR (ORDER = 'SHOW  ') OR
      (ORDER = 'H     ') OR (ORDER = 'HELPAS') OR
      (ORDER = 'HELP  ') THEN
   CASE ORDER OF
     'C     ',
     'CONECT'  : BEGIN
                 CONECT
                 END;
     'D     ',
     'DISCON'  : BEGIN
                 DISCON
                 END;
     'SEND  ',
     'S     '  : BEGIN
                 SFILES
                 END;
     'REC   ',
     'R     ',
     'RECEIV'  : BEGIN
                 RFILES
                 END;
     'BUF   ',
     'BUFFER'  : BEGIN
                 GROBK(ERR);
                 END;
     'K     ',
     'END   ',
     'Q     ',
     'QUIT  '  : BEGIN
        REPORT := '     GOOD BYE                           ';
                 LENREPORT := 13;
         (*=F+*) KTEROU(REPORT,LENREPORT);
                 GOAL := TRUE
                 END;
     'T     ',
     'TYPE  ' : BEGIN
                SPAUSDINTI
                END;
     'SHOW  ',
     'STA   ' : SHOWPAR;
     'SET   ' : SETPAR;
     'H     ',
     'HELP  ',
     'HELPAS' : HELPAS;
     END (*CASE*)
   ELSE
     BEGIN
     REPORT := 'INVALID COMMAND                         ';
     LENREPORT := 16;
     (*=F+*) KTEROU(REPORT,LENREPORT);
     ERR := (ERR + 1) MOD 5;
     IF ERR = 0 THEN
       HELPAS
     END
 UNTIL GOAL
 END; (*MAINKER*)
 (*---------------------------------*)
 BEGIN (*KERMIT*)
 MAINKER
 END;  (*KERMIT*)
 (*----------------------------------------------*)
 BEGIN
   KERMIT
 END.
*PASCAL
(*=P-,T-*)
 PROGRAM UTILIT (INPUT,OUTPUT);
 (*--------------------------*)
 TYPE
   STRINGTYPE = PACKED ARRAY [1..200] OF 0..255;
 (*=E+*)
 FUNCTION CHECKS (VAR STRING : STRINGTYPE;
             VAR FIRST,LAST : INTEGER) : INTEGER;
 VAR
   S,INDEX : INTEGER;
 BEGIN (*CHECKS*)
 S := 0;
 FOR INDEX := FIRST TO LAST DO
   S := S + STRING[INDEX];
 S := S MOD 256;
 S := (S + (S DIV 64)) MOD 64;
 CHECKS := S
 END; (*CHECKS*)
 (*------------------------------*)
 FUNCTION MAKECH(X : INTEGER) : CHAR;
 (*--------------------------------*)
 BEGIN (*MAKECH*)
 MAKECH := CHR(X + 32)
 END; (*MAKECH*)
 (*------------------------------------------------*)
 FUNCTION UNCHAR(VAR X : CHAR) : INTEGER;
 (*------------------------------------*)
 BEGIN (*UNCHAR*)
 UNCHAR := ORD(X) - 32
 END; (*UNCHAR*)
 (*--------------------------------------------*)
 PROCEDURE I10I8(VAR I10,I8 : INTEGER);
 (*----------------------------------*)
 VAR
   I10DUB,J,LAIPSNIS : INTEGER;
 (*-------------------*)
 BEGIN (*I10I8*)
 I10DUB := I10; I8 := 0; LAIPSNIS := 1;
 REPEAT
   I8 := (I10DUB MOD 8) * LAIPSNIS + I8;
   I10DUB := I10DUB DIV 8;
   LAIPSNIS := LAIPSNIS * 10;
 UNTIL I10DUB = 0
 END;  (*I10I8*)
 (*--------------------------------------------*)
 BEGIN (* UTILIT *)
 END.
*FTN
      SUBROUTINE KTERIN(M,L)
      INTEGER TIM
      DATA INIT/0/
      DATA TIM/0/
      IF(INIT.NE.0) GOTO 7
      CALL IPUTS(15B,INIT,0)
      CALL IPUTS(12B,INIT,1)
      CALL IPUTS('     -',INIT,2)
      CALL IPUTS(10B,INIT,3)
  7   K=ITOUTB(0,INIT,4,TIM)
  1   K=ITINPB(0,M,LL,TIM)
      IF(K.EQ.0) GOTO 1
      K=ITOUTB(0,M,LL,TIM)
      K=ITOUTB(0,INIT,2,TIM)
      L=LL
      RETURN
      END
      SUBROUTINE KTEROU(M,L)
      DATA INI/0/
      IF(INI.NE.0) GOTO 8
      CALL IPUTS(15B,INI,0)
      CALL IPUTS(12B,INI,1)
   8  K=ITOUTB(0,M,L,5)
      K=ITOUTB(0,INI,2,5)
      RETURN
      END
*ASSEM
  RZ:,NAME,
C*
C***    SUBROUTINE RZ( MTZZZZ, M )
C*      SUBROUTINE WZ( MTZZZZ, M )
C*
C*      ЧTEHИE/ЗAПИCЬ OДHOЙ ЗOHЫ MЛ/MД
C*      ЗДECЬ MTZZZZ - BOCЬMEPИЧHЫЙ HOMEP
C*      YCTPOЙCTBA И ЗOHЫ (HAПPИMEP: 670011B)
C*      M - MACCИB ДЛИHOЙ 1024 CЛOBA.
C*
  :,UTC,=:0010
  WZ:,ENTRY,
  9,VTM,
  8,BASE,*
   ,STI,10.MACCИB
  ,STI,11.AДPEC
C---
  12,VTM,-1023
  10,UTM,-1024
  TR1:12,XTA,-1.ЗAMEHA
  10,XTS,1024.COДEPЖИMOГO
  12,STX,-1.OБMEHHOГO ЛИCTA
  10,ATX,1024.И MACCИBA
  10,UTM,1. ЮЗEPA
  12,VLM,TR1
C---
  11,XTA,
  ,AAX,=777777
  ,AOX,=:0000 3700
  9,AOX,
  15,ATX,1
  15,*70,1. OБMEH
C---
  12,VTM,-1023
  TR2:12,XTA,-1
  10,XTS,.OБPATHAЯ
  12,STX,-1.ЗAMEHA
  10,ATX,
  10,UTM,1
  12,VLM,TR2
  13,UJ,
  ,END,
*PASCAL
 (*=P-,T-,S7*)
 PROGRAM IBRAND1 (INPUT,OUTPUT,CIBREZ,CIBMEM,CZONA,
                  CBFINF,CIBIVE,CIBEIL,CIBDER,CIBSAL);
 (*----------*)
                                                   (*  *)
  CONST
MAXEILUTE = 80;
BLANKAI = '      ';
TARNINF = '$';
DEREILUTE = '$$$   ';
DERNUIMTI = '$$$*  ';
IGNPRAD = '$$$+++';
IGNGAL = '$$$---';
MAXRAIDKOD = 128;
IVSALTPABZEN = '_004';
EILUTPABZEN = '_005';
TEXTPABZEN = '_006';
STKOMENT = '[**]  ';
DERINILGIS = 20;
DERPARILGIS = 3;
MEMILGIS = 15000;
BLANK = ' ';
PAKBLANK = '_201';
ALFAILGIS = 6;
STEKGYLIS = 5;
EKRANGAL = '*END F';   (*TURI BUTI *E N D   F*)
  TYPE
SALTIPAS = (EDITFILE,SCREEN,SCREENSLAPT,INPUTFILE,PASCFILE,
            FORTFILE,MEMREC,MEMORY,BINFILE);
TARPUTIPAS = (SUSPAUSTI,PAKUOTI,REIKSMINIAI);
MAKETIPAS = (INFORM,SUPRADZIA,PILNA);
EILRANGE = 1..MAXEILUTE;
REZTIPAS =
    RECORD
      SALTINIS : SALTIPAS;
      EILILGIS : INTEGER;
      EILFORMA : MAKETIPAS;
      KOKIETARP : TARPUTIPAS;
      KOMENT : ALFA;
      IKIOBJEKTO :BOOLEAN
    END;
TIPBINFILINF =
    RECORD
      MJMDKOD,
      MJMDMATNR,
      MJMDPRADZ,
      MJMDEILZ,
      MJMDZONSK : INTEGER;
    END;
MEMOTIPAS = PACKED ARRAY [1..MEMILGIS] OF CHAR;
PMEMOTIPAS = ^ MEMOTIPAS;
MEMTIPAS =
    RECORD
      PTOMEMORY : PMEMOTIPAS;
      OAPIRMSIMB : INTEGER;
      OAGALSIMB : INTEGER;
      OAEILINSIMB : INTEGER;
      OAEILPAB : CHAR;
      OAPABSIMB : CHAR;
    END;
EILTIPAS = PACKED ARRAY [EILRANGE] OF CHAR;
DERINTIPAS = PACKED ARRAY [1..DERINILGIS] OF 0..15;
IVEDIRASTIP =
    RECORD
      VISONUSKAITE,
      KIEKEILUT : INTEGER;
      EILUTE : EILTIPAS;
      PIRMSIMB,
      GALSIMB,
      EILINSIMB : INTEGER;
      EILGALAS,
      TEXTGALAS,
      IVSALTGALAS,
      DUOKEIL,
      YRAKOMGAL : BOOLEAN;
    END;
INFAPIESALTINI = RECORD
  SVISONUSKAITE,
  SKIEKEILUT : INTEGER;
  SCIBREZ : REZTIPAS;
  SCIBDER : DERINTIPAS;
  CASE SALTIPAS OF
    MEMREC,
    MEMORY : (SCIBMEM : MEMTIPAS)
  END;
STSALTTYPE = RECORD
  STEKVIRSUNE : 0..STEKGYLIS;
  YRASTEKAS :BOOLEAN;
  STSALTINF : ARRAY [1..STEKGYLIS] OF INFAPIESALTINI
  END;
ZONTIPAS = PACKED ARRAY [1..6144] OF CHAR;
  VAR
  CIBREZ : REZTIPAS;
  CIBMEM : MEMTIPAS;
  CIBIVE : IVEDIRASTIP;
  CIBEIL : EILTIPAS;
  CIBDER : DERINTIPAS;
  CIBSAL : STSALTTYPE;
  CZONA : ZONTIPAS;
  CBFINF : TIPBINFILINF;
  (*=F+*)
  PROCEDURE  KTERIN (VAR MAS :EILTIPAS; VAR LEN :INTEGER);FORTRAN;
  (*=F+*)
  PROCEDURE  RECARD (VAR MAS :EILTIPAS);FORTRAN;
  (*=F+*)
  PROCEDURE  RZ (VAR MTZZZZ:INTEGER; VAR ZON :ZONTIPAS);FORTRAN;
    (*=D4*)
PROCEDURE SKAITO;
(*-------------*)
VAR I, J, K :INTEGER;
    SCRGALAS, SIMBWORD, ZODIS : ALFA;
    BAIK :BOOLEAN;
    SIMBDARB :CHAR;
                                           (*  *)
BEGIN (* SKAITO *)
  WITH CIBIVE, CIBREZ DO
    BEGIN
      FOR I := 1 TO EILILGIS DO  CIBEIL [I] := BLANK;
      CASE SALTINIS OF
        INPUTFILE:
          IF EOF(INPUT) THEN IVSALTGALAS := TRUE
          ELSE READLN(CIBEIL);
        SCREENSLAPT,
        SCREEN : BEGIN(*SKAITYMO IS EKRANO PROCEDURA*)
                 (*=F+*) KTERIN(CZONA,I);
                 IF I=0 THEN
                   TEXTGALAS := TRUE
                 ELSE
                   BEGIN
                   IF I > EILILGIS THEN I := EILILGIS
                   ELSE
                     FOR J := I + 1 TO EILILGIS DO
                       CIBEIL[J] := BLANK;
                   FOR J := 1 TO I DO
                     CIBEIL[J] := CZONA[J];
                   FOR I:=1 TO ALFAILGIS DO
                     SCRGALAS[I] := CIBEIL[I];
                   IF SCRGALAS = EKRANGAL THEN
                     IVSALTGALAS := TRUE
                   END
                 END;
        EDITFILE: (* SKAITYMAS EDIT FILE IS MJ *)
          BEGIN
          RECARD(CIBEIL);
          IF CIBEIL [1] = '*' THEN
            BEGIN
            FOR I := 1 TO ALFAILGIS DO
              ZODIS[I] := CIBEIL[I];
            IF ZODIS = '*READ ' THEN
              BEGIN
              FOR I := 1 TO ALFAILGIS DO
                ZODIS[I] := CIBEIL[I+6];
              IVSALTGALAS := ZODIS = 'OLD   '
              END
            END
          END;
        BINFILE : (*SKAITYMAS BINARINIO FAILO IS MJ/MD*)
          BEGIN
          END;
        PASCFILE : (*SKAITYMAS IS PASCAL FAILO*)
          BEGIN
          END;
        FORTFILE : BEGIN(*SKAITYMAS IS FORTRAN FAILO*)END;
        MEMREC : (*PERSIUNTIMO PROCEDURA*)
          BEGIN
            WITH CIBMEM DO
              BEGIN
              IF OAEILINSIMB >= OAPIRMSIMB THEN
                IVSALTGALAS := (OAEILINSIMB = OAGALSIMB) OR
                      ( (PTOMEMORY^ [OAEILINSIMB] = OAPABSIMB)
                               AND   (OAPABSIMB <> ' ') );
              IF NOT IVSALTGALAS THEN
                BEGIN
                  I := 0;  BAIK := FALSE;
                  WHILE (I < EILILGIS) AND (NOT BAIK) AND
                             (OAEILINSIMB < OAGALSIMB) DO
                    BEGIN
                      I := I+1;
                      OAEILINSIMB := OAEILINSIMB+1;
                      SIMBDARB := PTOMEMORY^ [OAEILINSIMB];
                      CIBEIL[I] := SIMBDARB;
                      BAIK := ((SIMBDARB = OAPABSIMB) OR
                               (SIMBDARB = OAEILPAB)) AND
                               (SIMBDARB <> BLANK);
                    END (*WHILE*)
                END;
              END; (*WITH*)
          END;
      END;  (* CASE *)
      VISONUSKAIT := VISONUSKAIT + 1
    END;  (* WITH *)
  (*=Z-*)
  IF (CIBDER [1] > 3) AND NOT CIBIVE.IVSALTGALAS THEN
    WITH CIBREZ DO
      BEGIN
        WRITE(' --- SKAITO. IVESTA EILUTE IS ');
        CASE SALTINIS OF
          INPUTFILE  : WRITE('INPUTFILE');
          SCREEN     : WRITE('SCREEN');
          EDITFILE   : WRITE('EDITFILE');
          PASCFILE   : WRITE('PASCFILE');
          FORTFILE   : WRITE('FORTFILE');
          MEMREC     : WRITE('MEMREC');
          MEMORY     : WRITE('MEMORY');
          BINFILE    : WRITE('BINFILE')
        END;  (* CASE *)
        WRITELN(' ILGIO ',EILILGIS:1,' NUM. ',CIBIVE.VISONUSKAIT:1);
        WRITE (' =>');
        FOR I := 1 TO EILILGIS DO WRITE(CIBEIL [I]:1);
        WRITELN ('<=')
      END  (* WITH *)
  (*=Z+*)
END;  (* SKAITO *)
   (*=E+*)
    PROCEDURE PRITEX(VAR TEXTMAS:PACKED ARRAY [1..15000] OF CHAR;
                     VAR PIRMSIMB , MASILGIS, PRADTARP, SIMBEILUT,
                     ISNAUJOS, TARPREZ :INTEGER);
    VAR
      EILSKAICIUS,I,EILNR,K,J,SIMBMAS,
      L,NR :INTEGER;
      SIMB:CHAR;
    BEGIN
      IF (TARPREZ<0) OR (TARPREZ>3) THEN
        TARPREZ:=0;
      SIMBMAS:=PIRMSIMB-1;
      IF SIMBEILUT <= 0 THEN SIMBEILUT:=67;
      IF (MASILGIS > 0) AND (PIRMSIMB <= MASILGIS) THEN
        EILSKAICIUS:=(MASILGIS-PIRMSIMB) DIV SIMBEILUT + 1
      ELSE
        EILSKAICIUS:=0;
      IF ISNAUJOS = 1 THEN WRITELN;
      FOR I:=1 TO EILSKAICIUS DO
        BEGIN
          EILNR:=I*SIMBEILUT+PIRMSIMB-1;
          WHILE (SIMBMAS<EILNR) AND (SIMBMAS<MASILGIS) DO
            BEGIN
              K:=1;
              FOR J:=1 TO PRADTARP DO
                WRITE(' ');
              WHILE (K<=SIMBEILUT) AND (SIMBMAS<MASILGIS) DO
                BEGIN
                  SIMBMAS:=SIMBMAS+1;
                  SIMB:=TEXTMAS[SIMBMAS];
                  NR:=ORD(SIMB);
                  IF NR>MAXRAIDKOD THEN
                    CASE TARPREZ OF
                      0 : BEGIN
                           WRITE('.',NR:3,'.');
                            K:=K+5
                          END;
                      1 : BEGIN
                            WRITE(' ');
                            K:=K+1
                          END;
                      2 : BEGIN
                            L:=NR-MAXRAIDKOD;
                            FOR J:=1 TO L DO
                             WRITE(' ');
                            K:=K+L
                          END;
                    END (*CASE*)
                  ELSE
                    BEGIN
                      K:=K+1;
                      WRITE(SIMB:1)
                    END
              END; (*WHILE*)
              IF SIMBMAS <> MASILGIS THEN WRITELN
            END (*WHILE*)
       END (*FOR*)
    END; (*PRITEX*)
PROCEDURE IVEIL;
(*------------*)
VAR
  ILGIS, PIREIKSM, GALREIKSM, SKAITMSK, DERPARSK,
  I, J, L, K, IEIL : INTEGER;
  SIMB, KOMPIRPR, KOMANTPR, KOMPIRGAL, KOMANTGAL : CHAR;
  GERAEILUTE, PRALEILUT, SKAITMUO :BOOLEAN;
  TARNEILTIP : ALFA;
  MAS : ARRAY[1..2] OF INTEGER;
                                                 (*  *)
BEGIN  (* IVEIL *)
  WITH CIBIVE, CIBREZ DO
    BEGIN
      DUOKEIL := FALSE;
      IF NOT IVSALTGALAS THEN
        BEGIN
          GERAEILUTE := FALSE; PRALEILUT := FALSE;TEXTGALAS:=FALSE;
          WHILE NOT (GERAEILUTE OR IVSALTGALAS OR TEXTGALAS) DO
            BEGIN
              SKAITO;
              IF NOT (IVSALTGALAS OR TEXTGALAS) THEN
                BEGIN
                IF (CIBEIL [1] = TARNINF) AND
                   (CIBEIL [2] = TARNINF) AND
                   (CIBEIL [3] = TARNINF) THEN
 BEGIN
  TARNEILTIP := BLANKAI; I := 0;
   REPEAT
     I := I + 1;
     SIMB := CIBEIL [I];
     TARNEILTIP[I] := SIMB
   UNTIL (I >= ALFAILGIS) OR (SIMB = BLANK);
   IF (TARNEILTIP = DERNUIMTI) AND (NOT PRALEILUT) THEN
     BEGIN
       FOR I := 1 TO DERINILGIS DO CIBDER [I] := 0;
       WRITELN(' >>>DERIN OUT')
     END
   ELSE
     IF (TARNEILTIP = DEREILUTE) AND (NOT PRALEILUT) THEN
 BEGIN
   MAS[1] := 0;  MAS[2] := 0;  K := 4;        DERPARSK := 0;
   REPEAT
     SKAITMSK := 0;  J := 0;
     REPEAT
       K := K + 1;
       SIMB := CIBEIL [K];
       L := ORD(SIMB) - ORD('0');
       SKAITMUO := L IN [0..9]
     UNTIL (SKAITMUO) OR (K >= EILILGIS);
     IF SKAITMUO THEN
       BEGIN
         REPEAT
           SKAITMSK := SKAITMSK + 1;
           J := J*10 + L;
           K := K + 1;
           SIMB := CIBEIL [K];
           L := ORD(SIMB) - ORD('0');
           SKAITMUO := L IN [0..9]
         UNTIL (NOT SKAITMUO) OR (SKAITMSK >= DERPARILGIS)
                              OR (K >= EILILGIS);
         IF SKAITMUO AND (K < EILILGIS) THEN
            REPEAT
              K := K + 1;
              SIMB := CIBEIL [K];
              L := ORD(SIMB) - ORD('0');
              SKAITMUO := L IN [0..9]
            UNTIL NOT SKAITMUO OR (K >= EILILGIS);
          DERPARSK := DERPARSK + 1;
          MAS[DERPARSK] := J
        END
    UNTIL (DERPARSK >= 2) OR (K>= EILILGIS);
    IF DERPARSK >= 1 THEN
      IF (MAS [1] >= 1) AND (MAS [1] <= DERINILGIS) THEN
        BEGIN
          CIBDER [MAS [1]] := MAS [2];
          WRITELN(' >>>DERIN [',MAS [1]:1,'] = ',MAS [2]:1)
        END
 END
     ELSE
       IF TARNEILTIP = IGNPRAD THEN PRALEILUT := TRUE
       ELSE
         IF TARNEILTIP = IGNGAL THEN PRALEILUT := FALSE
 END
                ELSE
                  IF NOT PRALEILUT THEN
BEGIN
  I := 0; ILGIS := EILILGIS - 1; PIREIKSM := 0; GALREIKSM := 0;
  KOMPIRPR := KOMENT [1]; KOMANTPR := KOMENT [2];
  KOMPIRGAL := KOMENT [3]; KOMANTGAL := KOMENT [4];
  IEIL := 0;
  REPEAT
WHILE (NOT YRAKOMGAL) AND (I < ILGIS) DO
  BEGIN
    I := I + 1;
    IF CIBEIL [I] = KOMPIRGAL THEN
      IF CIBEIL [I + 1] = KOMANTGAL THEN
        BEGIN
          YRAKOMGAL := TRUE;
          I := I + 1
        END
  END;
    IF I < EILILGIS THEN
      BEGIN
        I := I + 1; SIMB := CIBEIL [I];
(*       TAI REIKALINGA NORINT, JOG BUTU KOMENTARAI
        IF SIMB = KOMPIRPR THEN
          IF CIBEIL [I + 1] = KOMANTPR THEN
            BEGIN
              YRAKOMGAL := FALSE;
              I := I + 1
            END;
 *)
        IF YRAKOMGAL THEN
          BEGIN
            IF IEIL = 0 THEN
              BEGIN
                IEIL := IEIL + 1;
                IF SIMB <> BLANK THEN
                  BEGIN
                    PIREIKSM := IEIL;
                    GALREIKSM := PIREIKSM
                  END
                ELSE
                  IF KOKIETARPAI = PAKUOTI THEN
                    SIMB := PAKBLANK
              END
            ELSE
              IF SIMB = BLANK THEN
                CASE KOKIETARPAI OF
                  SUSPAUSTI:
                    IF EILUTE [IEIL] <> BLANK THEN IEIL := IEIL + 1;
                  PAKUOTI:
                    IF ORD(EILUTE [IEIL]) > MAXRAIDKOD THEN
                      SIMB := CHR(ORD(EILUTE [IEIL]) + 1)
                    ELSE
                      BEGIN
                        IEIL := IEIL + 1;
                        SIMB := PAKBLANK
                      END;
                  REIKSMINIAI:
                    IEIL := IEIL + 1
                END  (* CASE *)
              ELSE
                BEGIN
                  IEIL := IEIL + 1;
                  GALREIKSM := IEIL;
                  IF PIREIKSM = 0 THEN PIREIKSM := IEIL
                END;
            EILUTE [IEIL] := SIMB
          END
      END
  UNTIL (I >= EILILGIS);
IF PIREIKSM > 0 THEN
  BEGIN
    GERAEILUTE := TRUE;
    KIEKEILUT := KIEKEILUT + 1;
    CASE EILFORMA OF
      INFORM : BEGIN
                 PIRMSIMB := PIREIKSM;        GALSIMB := GALREIKSM
               END;
      SUPRADZIA : BEGIN
                    PIRMSIMB := 1;  GALSIMB := GALREIKSM
                  END;
      PILNA : BEGIN
                PIRMSIMB := 1;        GALSIMB := IEIL
              END
    END;  (* CASE *)
    EILINSIMB := PIRMSIMB - 1;
    EILGALAS := FALSE
  END
END
                END
            END
        END
    END;  (* WITH *)
  (*=Z-*)
  IF CIBDER [1] > 1 THEN
WITH CIBIVE, CIBREZ DO
  BEGIN
    IF CIBDER[1] > 2 THEN
      BEGIN
        WRITE(' ---IVEIL.');
        WRITE(' VISONUSKAITE=',VISONUSKAITE:1,
              ' KIEKEILUT=',KIEKEILUT:1,
              ' PIRMSIMB=',PIRMSIMB:1,
              ' GALSIMB=',GALSIMB:1,
              ' EILINSIMB=',EILINSIMB:1);
        WRITELN;
        WRITE('          ');
        WRITE(' EILGALAS=',EILGALAS:1,
              ' IVSALTGALAS=',IVSALTGALAS:1,
              ' TEXTGALAS=',TEXTGALAS:1,
              ' DUOKEIL=',DUOKEIL:1,
              ' YRAKOMGAL=',YRAKOMGAL:1);
      END;
    WRITELN;
    IF PIRMSIMB > 0 THEN
      BEGIN
        WRITE(' =>');
        I:=0; J:=0; K:=0; L:=0;
        (*=C-*) PRITEX(EILUTE,PIRMSIMB,GALSIMB,I,J,K,L); (*=C+*)
        WRITELN('<=')
      END;
  END;        (* WITH *)
  (*=Z+*)
END;  (* IVEIL *)
FUNCTION ZENKLA : CHAR;
(*-----------*)
VAR SIMB : CHAR;
BEGIN  (*ZENKLA*)
  WITH CIBIVE DO
    BEGIN
      IF DUOKEIL THEN IVEIL;
      IF IVSALTGALAS THEN
        SIMB := IVSALTPABZEN
      ELSE
        IF TEXTGALAS THEN
          BEGIN
            SIMB:=TEXTPABZEN;
            DUOKEIL:=TRUE
          END
        ELSE
          IF EILGALAS THEN
            BEGIN
              SIMB := EILUTPABZEN;
              DUOKEIL := TRUE
            END
          ELSE
            BEGIN
              EILINSIMB := EILINSIMB + 1;
              IF CIBREZ.SALTINIS = MEMORY THEN
                WITH CIBMEM DO
                  BEGIN
                    SIMB := PTOMEMORY^ [EILINSIMB];
                    IF ((SIMB = OAPABSIMB) AND
                           (OAPABSIMB <> BLANK)) OR
                             (EILINSIMB > OAGALSIMB) THEN
                      BEGIN
                        SIMB := IVSALTPABZEN;
                        IVSALTGALAS := TRUE
                      END
                  END (*WITH*)
              ELSE
                IF CIBREZ.SALTINIS = BINFILE THEN
                  WITH CIBMEM DO
                    BEGIN
                    SIMB := PTOMEMORY^ [EILINSIMB];
                    IF (EILINSIMB = OAGALSIMB) THEN
                      WITH CBFINF DO
                        BEGIN
                        IF MJMDEILZ < MJMDZONSK THEN
                          BEGIN
                          MJMDEILZ := MJMDEILZ + 1;
                          MJMDKOD := MJMDKOD + 1;
                          RZ(MJMDKOD,CZONA);
                          EILINSIMB := 0;
                          END
                        END
                    ELSE
                      IF (EILINSIMB > OAGALSIMB) THEN
                        BEGIN
                        SIMB := IVSALTPABZEN;
                        IVSALTGALAS := TRUE
                        END
                    END (*WITH*)
              ELSE (*SALTINIS <> MEMORY*)
                BEGIN
                  SIMB := EILUTE [EILINSIMB];
                  EILGALAS := EILINSIMB = GALSIMB
                END
            END
    END; (*WITH*)
  ZENKLA := SIMB;
  (*Z-*)
  IF CIBDER [2] > 1 THEN
    WITH CIBIVE DO
      BEGIN
        WRITE(' ---ZENKLA.');
        WRITE(' EILINSIMB=',EILINSIMB:1);
        WRITE (' ORD(SIMB) = ',ORD(SIMB):1);
        WRITE(' SIMB =>',SIMB:1,'<=');        WRITELN
      END;  (*WITH*)
  (*Z+*)
END;  (*ZENKLA*)
 FUNCTION ZENSIM : CHAR;
 VAR
   SIMB : CHAR;
 BEGIN (*ZENSIM*)
 SIMB := ZENKLA;
 WHILE (SIMB = BLANK) OR (ORD(SIMB) > MAXRAIDKOD)
      OR (SIMB = EILUTPABZEN) DO
   SIMB := ZENKLA;
 ZENSIM := SIMB
 END; (*ZENSIM*)
PROCEDURE ZENATG;
(*------------*)
BEGIN  (* ZENATG *)
  WITH CIBIVE,CIBMEM,CBFINF DO
    BEGIN
    IF CIBREZ.SALTINIS = BINFILE THEN
      IF EILINSIMB < PIRMSIMB THEN
        IF MJMDEILZ > 1 THEN
          BEGIN
          MJMDKOD := MJMDKOD - 1;
          MJMDEILZ := MJMDEILZ - 1;
          (*=F+*) RZ(MJMDKOD,CZONA);
          EILINSIMB := GALSIMB
          END;
    IF NOT (IVSALTGALAS OR (EILINSIMB < PIRMSIMB)
            OR TEXTGALAS)  THEN
      BEGIN
        IF DUOKEIL THEN DUOKEIL := FALSE
        ELSE
          BEGIN
            IF EILGALAS THEN EILGALAS := FALSE;
            EILINSIMB := EILINSIMB - 1
          END
      END
    END (*WITH*)
END;  (* ZENATG *)
PROCEDURE IEILPR;
(*------------*)
BEGIN  (* IEILPR *)
  WITH CIBIVE,CIBMEM DO
   IF NOT (IVSALTGALAS OR TEXTGALAS) THEN
    BEGIN
      EILINSIMB := PIRMSIMB - 1;
      EILGALAS := FALSE;
      DUOKEIL := FALSE
    END
END;  (* IEILPR *)
PROCEDURE IEILGA;
(*------------*)
BEGIN  (* IEILGA *)
  WITH CIBIVE,CIBREZ DO
    IF NOT (SALTINIS = MEMORY) THEN
    BEGIN
      EILINSIMB := GALSIMB;
      EILGALAS := TRUE;
      DUOKEIL := TRUE
    END
END;  (* IEILGA *)
FUNCTION IVEOF : BOOLEAN ;
(*----------------------*)
BEGIN
  WITH CIBIVE DO
    BEGIN
      IVEOF:=IVSALTGALAS
    END
END; (*IVEOF*)
FUNCTION IVEOT : BOOLEAN ;
(*----------------------*)
BEGIN
  WITH CIBIVE DO
    BEGIN
      IVEOT:=TEXTGALAS OR IVSALTGALAS
    END
END; (*IVEOT*)
FUNCTION IVEOL : BOOLEAN ;
(*----------------------*)
BEGIN
  WITH CIBIVE DO
    BEGIN
      IVEOL:=EILGALAS
    END
END; (*IVEOL*)
 BEGIN (* PROG. IBRAND1 *) END.
*PASCAL
 (*=P-,T-,S7*)
 PROGRAM IBRAND2 (INPUT,OUTPUT,CIBREZ,CIBMEM,CBFINF,
                  CIBIVE,CIBEIL,CIBDER,CIBSAL,CZONA);
 (*----------*)
                                                   (*  *)
  CONST
MAXEILUTE = 80;
BLANKAI = '      ';
TARNINF = '$';
DEREILUTE = '$$$   ';
DERNUIMTI = '$$$*  ';
IGNPRAD = '$$$+++';
IGNGAL = '$$$---';
MAXRAIDKOD = 128;
IVSALTPABZEN = '_004';
EILUTPABZEN = '_005';
TEXTPABZEN = '_006';
STKOMENT = '[**]  ';
DERINILGIS = 20;
DERPARILGIS = 3;
MEMILGIS = 15000;
BLANK = ' ';
PAKBLANK = '_201';
ALFAILGIS = 6;
STEKGYLIS = 5;
EKRANGAL = '*END F';   (*TURI BUTI *E N D   F*)
  TYPE
SALTIPAS = (EDITFILE,SCREEN,SCREENSLAPT,INPUTFILE,PASCFILE,
            FORTFILE,MEMREC,MEMORY,BINFILE);
TARPUTIPAS = (SUSPAUSTI,PAKUOTI,REIKSMINIAI);
MAKETIPAS = (INFORM,SUPRADZIA,PILNA);
EILRANGE = 1..MAXEILUTE;
REZTIPAS =
    RECORD
      SALTINIS : SALTIPAS;
      EILILGIS : INTEGER;
      EILFORMA : MAKETIPAS;
      KOKIETARP : TARPUTIPAS;
      KOMENT : ALFA;
      IKIOBJEKTO :BOOLEAN
    END;
TIPBINFILINF =
    RECORD
      MJMDKOD,
      MJMDMATNR,
      MJMDPRADZ,
      MJMDEILZ,
      MJMDZONSK : INTEGER;
    END;
MEMOTIPAS = PACKED ARRAY [1..MEMILGIS] OF CHAR;
PMEMOTIPAS = ^ MEMOTIPAS;
MEMTIPAS =
    RECORD
      PTOMEMORY : PMEMOTIPAS;
      OAPIRMSIMB : INTEGER;
      OAGALSIMB : INTEGER;
      OAEILINSIMB : INTEGER;
      OAEILPAB : CHAR;
      OAPABSIMB : CHAR;
    END;
EILTIPAS = PACKED ARRAY [EILRANGE] OF CHAR;
DERINTIPAS = PACKED ARRAY [1..DERINILGIS] OF 0..15;
IVEDIRASTIP =
    RECORD
      VISONUSKAITE,
      KIEKEILUT : INTEGER;
      EILUTE : EILTIPAS;
      PIRMSIMB,
      GALSIMB,
      EILINSIMB : INTEGER;
      EILGALAS,
      TEXTGALAS,
      IVSALTGALAS,
      DUOKEIL,
      YRAKOMGAL : BOOLEAN;
    END;
INFAPIESALTINI = RECORD
  SVISONUSKAITE,
  SKIEKEILUT : INTEGER;
  SCIBREZ : REZTIPAS;
  SCIBDER : DERINTIPAS;
  CASE SALTIPAS OF
    MEMREC,
    MEMORY : (SCIBMEM : MEMTIPAS)
  END;
STSALTTYPE = RECORD
  STEKVIRSUNE : 0..STEKGYLIS;
  YRASTEKAS :BOOLEAN;
  STSALTINF : ARRAY [1..STEKGYLIS] OF INFAPIESALTINI
  END;
ZONTIPAS = ARRAY [1..1024] OF ALFA;
  VAR
  CIBREZ : REZTIPAS;
  CIBMEM : MEMTIPAS;
  CIBIVE : IVEDIRASTIP;
  CIBEIL : EILTIPAS;
  CIBDER : DERINTIPAS;
  CIBSAL : STSALTTYPE;
  CZONA : ZONTIPAS;
  CBFINF : TIPBINFILINF;
  (*=F+*)
  PROCEDURE RBEGIN(VAR FAILOVIETA : INTEGER);FORTRAN;
  (*=F+*)
  PROCEDURE  RZ (VAR MTZZZZ:INTEGER; VAR ZON :ZONTIPAS);FORTRAN;
   (*=D4*)
   (*=E+*)
 PROCEDURE IVKLAI (VAR KLNR : INTEGER);
(*-------------*)
   VAR        I, J :INTEGER;
 (*=F+*) PROCEDURE LOADGO (VAR I : INTEGER); FORTRAN;
 BEGIN        (* IVKLAI *)
   WRITE (' ***** OШИБKA BBOДA', KLNR : 4);
   CASE KLNR OF
     1 : WRITE ('  BCTPETИЛCЯ KOHEЦ ФAЙЛA');
     2 : WRITE ('  HET ИCTOЧHИKA');
     4 : WRITE ('  HEДOПYCTИMЫЙ ПAPAMETP ПPИ OПPEДEЛEHИИ',
                 ' ИCTOЧHИKA HA MЛ/MД');
     5 : WRITE ('  HEДOПYCTИMЫЙ PEЖИM -MAKET CTPOKИ-');
     6 : WRITE ('  HEДOПYCTИMЫЙ PEЖИM -OБPAБOTKA ПPOБEЛOB-');
     7 : WRITE ('  HEДOПYCTИMЫЙ -ИCTOЧHИK-');
     8 : WRITE ('  ПEPEПOЛHEH CTEK ИCTOЧHИKOB');
     9 : WRITE ('  HET CTEKA ИCTOЧHИKOB');
    20 : WRITE ('  ДЛИHHOE ЧИCЛO');
    21 : WRITE ('  HET ЦИФP');
    22 : WRITE ('  ЧИCЛO MEHЬШE ДAHHOГO');
    23 : WRITE ('  ЧИCЛO БOЛЬШE ДAHHOГO');
    24 : WRITE ('  HET OTДEЛИTEЛЯ');
    25 : WRITE ('  HET COCTABHOЙ ЧACTИ ДATЫ');
    26 : WRITE ('  HET ДATЫ');
    27 : WRITE ('  TAKAЯ ДATA HE CYЩECTBYET');
    28 : WRITE ('  ПЛOXOE BOCMEPИЧHOE ЧИCЛO');
    31 : WRITE ('  ДЛИHHAЯ ЦEЛAЯ ЧACTЬ');
    32 : WRITE ('  HET B ЧИCЛE TOЧKИ');
    33 : WRITE ('  ДЛИHHAЯ ДPOБHAЯ ЧACTЬ')
   END;
   WRITELN
 END;  (* IVKLAI *)
 PROCEDURE OIVREZ (VAR REZ :INTEGER);
 (*------------*)
 BEGIN (* OIVREZ *)
   CIBREZ.IKIOBJEKTO := REZ = 0;
   (*=Z-*)
   IF CIBDER [2] > 0 THEN
     WRITELN (' --- OIVREZ. OBJEKTU IVEDIMO REZIMAS =',
       CIBREZ.IKIOBJEKTO)
   (*=Z+*)
 END; (* OIVREZ *)
 PROCEDURE MTKREZ (VAR MREZI,TREZI :INTEGER; VAR KOMSIMB :ALFA;
 (*-------------*) VAR IFERR :INTEGER);
   VAR I :INTEGER;
 BEGIN (* MTKREZ *)
   IFERR := 0;
   IF (MREZI < 0) OR (MREZI > 3) THEN
     BEGIN IFERR := 5;        IVKLAI (IFERR) END;
   IF (TREZI < 0) OR (TREZI > 3) THEN
     BEGIN IFERR := 6;        IVKLAI (IFERR) END;
   IF IFERR = 0 THEN
     WITH  CIBREZ  DO
       IF NOT (SALTINIS IN [MEMORY,BINFILE]) THEN
       BEGIN
         CASE MREZI OF
           0: EILFORMA := INFORM;
           1: EILFORMA := SUPRADZIA;
           2: EILFORMA := PILNA
         END;
         CASE TREZI OF
           0: KOKIETARPAI := SUSPAUSTI;
           1: KOKIETARPAI := PAKUOTI;
           2: KOKIETARPAI := REIKSMINIAI
         END;
         IF KOMSIMB=BLANKAI THEN
           KOMENT := STKOMENT
         ELSE
           KOMENT := KOMSIMB;
         I := 0;  OIVREZ (I);
         CIBIVE.DUOKEIL := TRUE;
         (*=Z-*)
         IF CIBDER [2] > 0 THEN
           WRITELN (' --- MTKREZ. LOK. REZIMAI.  EIL. FORMATAS =',
             ORD(EILFORMA):2,'TARPAI =',ORD(KOKIETARPAI):2,
             '        KOMENTARAI =',KOMENT:6,'=')
         (*=Z+*)
       END
 END; (* MTKREZ *)
 PROCEDURE IVSTEI (VAR DARBTIP :INTEGER);
 (*-------------------*)
 BEGIN (*IVSTEI*)
   WITH CIBSAL DO
     BEGIN
     STEKVIRSUNE := 0;
     IF DARBTIP = 0 THEN
       YRASTEKAS := TRUE
     ELSE
       YRASTEKAS := FALSE
     END
 END; (*IVSTEI*)
 PROCEDURE IVINIC;
 (*-------------*)
   VAR I :INTEGER;
 BEGIN (* IVINIC *)
   FOR I := 1 TO DERINILGIS DO CIBDER [I] := 0;
   WITH CIBIVE DO
     BEGIN
       VISONUSKAITE := 0;  KIEKEILUT := 0;
       PIRMSIMB := 0;  GALSIMB := 0;
       EILINSIMB := 1;        EILGALAS := TRUE;
       TEXTGALAS:=FALSE;
       IVSALTGALAS := FALSE;
       DUOKEIL := TRUE;  YRAKOMGAL := TRUE;
     END;
  WITH CIBMEM DO
    BEGIN
       OAPIRMSIMB:=0;
       OAGALSIMB:=0;
       OAEILINSIMB:=1;
     END;
 END; (* IVINIC *)
 PROCEDURE SALTRE(VAR SALTNR, EILGIS, IFERR : INTEGER);
 (*-------------*)
 VAR I,J :INTEGER; TARP :ALFA;
 BEGIN (*SALTRE*)
   IFERR := 0;
   IF (SALTNR < 0) OR (SALTNR > 8) THEN
     IFERR := 7;
   WITH CIBSAL DO
   IF YRASTEKAS AND (IFERR = 0) THEN
   IF STEKVIRSUNE = 0 THEN
     STEKVIRSUNE := STEKVIRSUNE + 1
   ELSE
   IF STEKVIRSUNE <= STEKGYLIS THEN
     BEGIN
     IF NOT (CIBREZ.SALTINIS IN
          [BINFILE,EDITFILE,PASCFILE,FORTFILE]) THEN
     WITH STSALTINF[STEKVIRSUNE],CIBIVE DO
       BEGIN
       SVISONUSKAITE := VISONUSKAITE;
       SKIEKEILUT := KIEKEILUT;
       SCIBREZ := CIBREZ;
       SCIBDER := CIBDER;
       IF CIBREZ.SALTINIS IN [MEMREC,MEMORY] THEN
         BEGIN
         IF CIBREZ.SALTINIS = MEMORY THEN
           BEGIN
           CIBMEM.OAPIRMSIMB := EILINSIMB + 1;
           CIBMEM.OAEILINSIMB := EILINSIMB
           END;
         SCIBMEM := CIBMEM
         END;
       STEKVIRSUNE := STEKVIRSUNE + 1
       END
     END
   ELSE
     IFERR := 8;
   IF IFERR = 0 THEN
     WITH CIBREZ DO
       BEGIN
         IVINIC;
         CASE SALTNR OF
           0: SALTINIS := INPUTFILE;
           1: SALTINIS := SCREEN;
           2: SALTINIS := SCREENSLAPT;
           3: SALTINIS := EDITFILE;
           4: SALTINIS := PASCFILE;
           5: SALTINIS := FORTFILE;
           6: SALTINIS := MEMREC;
           7: SALTINIS := MEMORY;
           8: SALTINIS := BINFILE;
         END;
         IF (EILGIS < 0) OR (EILGIS > MAXEILUTE) THEN
           IFERR := 11
         ELSE
           BEGIN
             IF EILGIS = 0 THEN
               CASE SALTINIS OF
                 EDITFILE: EILILGIS:=72;
                 SCREENSLAPT,
                 SCREEN:  BEGIN
                          EILILGIS:=78;
                          END;
                 INPUTFILE: EILILGIS:=80;
                 PASCFILE:  EILILGIS:=72;
                 FORTFILE:  EILILGIS:=80;
                 BINFILE,
                 MEMORY: BEGIN        END;
               END
             ELSE
               EILILGIS:=EILGIS;
             I := 0;  J := 0;  TARP := BLANKAI;
             MTKREZ(I,J,TARP,IFERR);
             OIVREZ(I);
           END;
         (*=Z-*)
         IF CIBDER [2] > 0 THEN
           WRITELN (' --- SALTRE. GLOB. REZIMAI.  SALTINIS = ',
             ORD(SALTINIS):2,' EILUTES ILGIS =',EILILGIS:4)
         (*=Z+*)
       END;
   IF IFERR <> 0 THEN IVKLAI (IFERR)
 END; (*SALTRE*)
 PROCEDURE STANDR;
   VAR I,J,K :INTEGER;
  BEGIN (* STANDR *)
    I := 0;  J := 0;
    IVSTEI (I);
    SALTRE(I,J,K)
  END; (* STANDR *)
 PROCEDURE MEMORE(VAR SALTPRA,SIMBPIRM,SIMBGAL : INTEGER;
                  VAR MEMEILPAB,MEMPAB : CHAR;
                  VAR EILGIS : INTEGER);
   VAR
     I,K : INTEGER;
   BEGIN
     WITH CIBMEM,CIBIVE,CIBREZ DO
       BEGIN
         IF EILGIS = 0 THEN
             I:=7
         ELSE I:=6;
         SALTRE(I,EILGIS,K);
         IF EILGIS = 0 THEN  EILILGIS := SIMBGAL;
         PTOMEMORY := PTR(SALTPRA);
         OAPIRMSIMB := SIMBPIRM;
         OAGALSIMB := SIMBGAL;
         OAEILPAB := MEMEILPAB;
         OAPABSIMB := MEMPAB;
         OAEILINSIMB := SIMBPIRM - 1;
         IF SALTINIS = MEMORY THEN
           BEGIN
             DUOKEIL := FALSE;        EILGALAS := FALSE;
             EILINSIMB := OAEILINSIMB;
             PIRMSIMB := OAPIRMSIMB;
             GALSIMB := OAGALSIMB
           END;
         (*=Z-*)
         IF CIBDER[1] > 0 THEN
           BEGIN
             WRITELN(' ---MEMORE.SALTINIO ADRESAS =',
               SALTPRA:4,'PIRM.SIMB =',SIMBPIRM:1,
               'GALUT.SIMB. =',SIMBGAL:1);
             WRITELN(' EILUTES GALAS=',MEMEILPAB:1,
               '=  MEMORE GALAS=',MEMPAB:1,'=')
           END
         (*=Z+*)
       END (*WITH*)
     END; (*MEMORE*)
 PROCEDURE EDITRE(VAR MATNR,PRADZ,IFERR : INTEGER);
 VAR
   I,J,K,FAILOVIETA : INTEGER;
 BEGIN (*EDITRE*)
 IFERR := 0;
 IF (MATNR >= 0) AND (MATNR <= 67B) AND
    (MATNR <> 50B) AND (MATNR <> 51B) AND
    (PRADZ >= 0) AND (PRADZ <= 1743B)  THEN
   BEGIN
   FAILOVIETA := MATNR * 10000B + PRADZ;
   RBEGIN(FAILOVIETA);
   I := 3;  J := 0;
   SALTRE(I,J,IFERR)
   END
 ELSE
   BEGIN
   IFERR := 4;
   IVKLAI(IFERR)
   END
 END; (*EDITRE*)
 PROCEDURE BFILRE(VAR MATNR,PRADZ,ZONSK,IFERR : INTEGER);
 VAR
   I,J,K,L,FAILOVIETA : INTEGER;
   S1,S2 : CHAR;
 BEGIN (*EDITRE*)
 IFERR := 0;
 IF (MATNR >= 0) AND (MATNR <= 67B) AND
    (MATNR <> 50B) AND (MATNR <> 51B) AND
    (PRADZ >= 0) AND ((PRADZ + ZONSK) <= 1743B) THEN
   BEGIN
   WITH CBFINF DO
     BEGIN
     MJMDKOD := MATNR*10000B + PRADZ;
     MJMDMATNR := MATNR;
     MJMDPRADZ := PRADZ;
     MJMDZONSK := ZONSK;
     MJMDEILZ := 1;
     END;
   FAILOVIETA := MATNR * 10000B + PRADZ;
   (*=F+*) RZ (FAILOVIETA,CZONA);
   I := ORD(REF(CZONA));  J := 1;  K := 1024*ALFAILGIS;
   S1 := BLANK;  S2 := BLANK; L := 0;
   MEMORE(I,J,K,S1,S2,L);
   CIBREZ.SALTINIS := BINFILE;
   END
 ELSE
   BEGIN
   IFERR := 4;
   IVKLAI(IFERR)
   END
 END; (*EDITRE*)
 PROCEDURE RETSAL (VAR IFERR :INTEGER);
 (*-------------------*)
 BEGIN (*RETSAL*)
 IFERR := 0;
 WITH CIBSAL DO
   BEGIN
   IF YRASTEKAS THEN
   IF (STEKVIRSUNE > 1) AND (STEKVIRSUNE <= STEKGYLIS) THEN
     BEGIN
     IVINIC;
     STEKVIRSUNE := STEKVIRSUNE - 1;
     WITH STSALTINF [STEKVIRSUNE], CIBIVE DO
       BEGIN
       VISONUSKAITE := SVISONUSKAITE;
       KIEKEILUT := SKIEKEILUT;
       CIBREZ := SCIBREZ;
       CIBDER := SCIBDER;
       IF CIBREZ.SALTINIS IN [MEMREC,MEMORY] THEN
         CIBMEM := SCIBMEM;
       END
     END
   ELSE
     IFERR := 2;
   END; (*WITH*)
 IF IFERR = 0 THEN
   WITH CIBREZ,CIBMEM,CIBIVE DO
   IF SALTINIS = MEMORY THEN
     BEGIN
     DUOKEIL := FALSE;        EILGALAS := FALSE;
     EILINSIMB := OAEILINSIMB;
     PIRMSIMB := OAPIRMSIMB;
     GALSIMB := OAGALSIMB
     END;
   IF IFERR <> 0 THEN IVKLAI (IFERR)
 END; (*RETSAL*)
   PROCEDURE IVDREZ(VAR I,J : INTEGER);
     BEGIN
       CIBDER [I] := J
     END;
 BEGIN (* PROG. IBRAND2 *)
 END.
*PASCAL
(*=P-,T-*)
 PROGRAM IVFUNK (INPUT,OUTPUT,CIBREZ,CIBEIL,CIBDER);
 (*----------*)
  CONST
MAXEILUTE = 80;
BLANKAI = '      ';
MAXRAIDKOD = 128;
DERINILGIS = 20;
MAXTEXT = 1300;
BLANK = ' ';
PAKBLANK = '_201';
MAXSVILGIS = 11;
MAXREILGIS = 12;
STZODSIMB = '-     ';
ALFAILGIS = 6;
EILUTPABZEN = '_005';
IVSALTPABZEN = '_004';
TEXTPABZEN = '_006';
  TYPE
SALTIPAS = (EDITFILE,SCREEN,SCREENSLAPT,INPUTFILE,PASCFILE,
            FORTFILE,MEMREC,MEMORY,BINFILE);
TARPUTIPAS = (SUSPAUSTI,PAKUOTI,REIKSMINIAI);
MAKETIPAS = (INFORM,SUPRADZIA,PILNA);
EILRANGE = 1..MAXEILUTE;
REZTIPAS =
    RECORD
      SALTINIS : SALTIPAS;
      EILILGIS : EILRANGE;
      EILFORMA : MAKETIPAS;
      KOKIETARP : TARPUTIPAS;
      KOMENT : ALFA;
      IKIOBJEKTO :BOOLEAN
    END;
SIMBTIPAS = (RAIDE,SKAITMUO,TARPAS,EGALAS,FGALAS,TGALAS,KITI);
EILTIPAS = PACKED ARRAY [EILRANGE] OF CHAR;
TEXTTIPAS = PACKED ARRAY [1..MAXTEXT] OF CHAR;
DERINTIPAS = PACKED ARRAY [1..DERINILGIS] OF 0..15;
  VAR
  CIBREZ : REZTIPAS;
  CIBEIL : EILTIPAS;
  CIBDER : DERINTIPAS;
 (*=F+*) PROCEDURE PRITEX(VAR TEXTMAS:TEXTTIPAS;
             VAR PIRMSIMB,MASILGIS,PRADTARP,SIMBEILUT,ISNAUJOS,
             TARPREZ:INTEGER); FORTRAN;
 PROCEDURE IVEIL; EXTERNAL;
 FUNCTION ZENKLA : CHAR; EXTERNAL;
 PROCEDURE ZENATG; EXTERNAL;
 FUNCTION IVEOT : BOOLEAN; EXTERNAL;
 PROCEDURE IVKLAI(VAR KLNR : INTEGER); EXTERNAL;
    (*=D4*)
 FUNCTION ZKLASE (VAR SIMB :CHAR) :SIMBTIPAS;
 (*------------*)
     VAR INUM :INTEGER;  ZK :SIMBTIPAS;
 BEGIN (* ZKLASE *)
   INUM := ORD(SIMB);
   IF (SIMB = BLANK) OR (INUM > MAXRAIDKOD) THEN
     ZK := TARPAS
   ELSE
     IF (SIMB >= 'A') AND (INUM <= MAXRAIDKOD) THEN
       ZK := RAIDE
     ELSE
       IF (INUM - ORD('0')) IN [0..9] THEN
         ZK := SKAITMUO
       ELSE
         IF SIMB = EILUTPABZEN THEN
           ZK := EGALAS
         ELSE
           IF SIMB=TEXTPABZEN THEN
             ZK:=TGALAS
           ELSE
             IF SIMB = IVSALTPABZEN THEN
               ZK := FGALAS
             ELSE
               ZK := KITI;
   ZKLASE := ZK;
   (*=Z-*)
   IF CIBDER [3] > 4 THEN
     WRITELN (' --- ZKLASE. SIMBOLIO =>',SIMB:1,'<=  KLASE =',
       ORD(ZK):2)
   (*=Z+*)
 END; (* ZKLASE *)
 FUNCTION INALFA (VAR SIMB :CHAR; VAR SIMRINK :ALFA) :BOOLEAN;
 (*------------*)
   VAR I :INTEGER;  RASTA :BOOLEAN;
 BEGIN (* INALFA *)
   I := 1;  RASTA := FALSE;
   WHILE (I <= ALFAILGIS) AND (NOT RASTA) AND (SIMRINK[I] <> BLANK) DO
     BEGIN
       IF SIMB = SIMRINK[I] THEN RASTA := TRUE;
       I:=I + 1;
     END;
   INALFA := RASTA;
   (*=Z-*)
   IF CIBDER [3] > 2 THEN
     WRITELN (' --- INALFA.  SIMBOLIS =>',SIMB:1,'<=  RINKINYJE =',
       SIMRINK:6,'=  INALFA = ',RASTA)
   (*=Z+*)
 END; (* INALFA *)
 PROCEDURE IGTARP;
 (*--------------*)
   VAR SIMB : CHAR;
 BEGIN (* IGTARP *)
   SIMB := ZENKLA;
   WHILE (SIMB = BLANK) OR (ORD(SIMB) > MAXRAIDKOD)
         OR (SIMB = EILUTPABZEN) DO
     SIMB := ZENKLA;
   ZENATG
 END; (* IGTARP *)
 FUNCTION IKIOBJ (VAR SIMRINK :ALFA; VAR SIMKLAS :SET OF SIMBTIPAS):
                                              BOOLEAN;
 (*------------*)
   VAR  SIMB :CHAR;  SKLAS :SIMBTIPAS;  FUNREI :BOOLEAN;
        SIMBAIBE:SET OF SIMBTIPAS;
 BEGIN (* IKIOBJ *)
   (*=Z-*)
   IF CIBDER [3] > 0 THEN
     BEGIN
       WRITE (' --- IKIOBJ.  OBJ. PRADZIA : SIMBRINKINYS =',
         SIMRINK:6,'=  ARBA  TIPAI =');
       FOR SKLAS := RAIDE TO KITI DO
         IF SKLAS IN SIMKLAS THEN  WRITE (ORD(SKLAS):2);
       WRITELN
     END;
   (*=Z+*)
   WITH CIBREZ DO
     IF IKIOBJEKTO THEN
       BEGIN
   IF (SIMRINK <> BLANKAI) OR (SIMKLAS <> [ ]) THEN
     BEGIN
       SIMBAIBE:=SIMKLAS+[TGALAS,FGALAS] - [EGALAS];
       REPEAT
         SIMB := ZENKLA;  SKLAS := ZKLASE(SIMB);
       UNTIL INALFA(SIMB, SIMRINK) OR (SKLAS IN SIMBAIBE);
       ZENATG
     END
     END
   ELSE
     IGTARP;
   FUNREI := NOT IVEOT; IKIOBJ := FUNREI;
   (*=Z-*)
   IF CIBDER [3] > 0 THEN
     BEGIN
       WRITE (' ':10,'PRALEISTA IKI SIMBOLIO =>',SIMB:1,'<=');
       WRITELN ('  IKIOBJ =',FUNREI)
     END;
   (*=Z+*)
 END; (*IKIOBJ *)
   (*=E+*)
 PROCEDURE IVZODI (VAR ZODIS: EILTIPAS;     VAR SIMBSK: INTEGER;
                   VAR SIMBRINK :ALFA; VAR IVESTASIMB: INTEGER);
 VAR RINKINYS: ALFA; SIMKLA: SIMBTIPAS; SIMB: CHAR;
     REIKSIMB,I:INTEGER;
     SKLAS: SET OF SIMBTIPAS;
   BEGIN I:= 0;
     RINKINYS :=BLANKAI ; IVESTASIMB:=0;
     SKLAS := [ RAIDE ];
     IF IKIOBJ (RINKINYS,SKLAS) THEN
       BEGIN
       SIMB:=ZENKLA;
       IF SIMBRINK= BLANKAI      THEN RINKINYS:= STZODSIMB
                                 ELSE RINKINYS:= SIMBRINK;
       IF SIMBSK>CIBREZ.EILILGIS THEN REIKSIMB:= CIBREZ.EILILGIS
                                 ELSE REIKSIMB:= SIMBSK;
                              SIMKLA:= ZKLASE(SIMB);
       IF SIMKLA= RAIDE THEN
        WHILE (SIMKLA IN [RAIDE,SKAITMUO]) OR INALFA(SIMB,RINKINYS) DO
           BEGIN I:= I + 1;
                 IF I<= REIKSIMB THEN ZODIS[I]:= SIMB;
                 SIMB:= ZENKLA; SIMKLA:=ZKLASE(SIMB);
           END;
         IVESTASIMB:= REIKSIMB;
        IF I<= REIKSIMB THEN IVESTASIMB:= I;
      ZENATG
     END;
 (*=Z-*) IF CIBDER[5]> 0 THEN BEGIN WRITE(' --- IVZODI.  ZODIS =>');
                                FOR I:= 1 TO IVESTASIMB  DO
                                  WRITE (ZODIS[I]:1);
                                WRITELN('<=');
                              END;
 (*=Z+*)
   END;
 PROCEDURE IVIDEN (VAR IDENT: EILTIPAS; VAR SIMBSK: INTEGER ;
                                    VAR IVESTASIMB: INTEGER);
 VAR SIMKLA: SIMBTIPAS; SIMB: CHAR; REIKSIMB,I: INTEGER;
 SKLAS : SET OF SIMBTIPAS;  RINKINYS :ALFA;
   BEGIN I:= 0;
     RINKINYS :=BLANKAI ; IVESTASIMB:=0;
     SKLAS := [ RAIDE ];
     IF IKIOBJ (RINKINYS,SKLAS) THEN
       BEGIN
         SIMB:=ZENKLA;
     IF SIMBSK>CIBREZ.EILILGIS THEN REIKSIMB:= CIBREZ.EILILGIS
                                ELSE REIKSIMB:= SIMBSK;
        SIMKLA:= ZKLASE(SIMB);
     IF SIMKLA= RAIDE THEN
        WHILE SIMKLA IN [RAIDE,SKAITMUO] DO
           BEGIN I:= I + 1;
                 IF I<= REIKSIMB THEN IDENT[I]:= SIMB;
                 SIMB:= ZENKLA; SIMKLA:=ZKLASE(SIMB);
           END;
         IVESTASIMB:= REIKSIMB;
        IF I<= REIKSIMB THEN IVESTASIMB:= I;
      ZENATG
     END;
     (*=Z-*) IF CIBDER[5]> 0 THEN BEGIN WRITE(' --- IVIDEN.IDEN =>');
                                   FOR I:=1 TO IVESTASIMB DO
                                     WRITE(IDENT[I]:1);
                                   WRITELN('<=');
                                 END;
     (*=Z+*)
   END;
 PROCEDURE IVTRTX (VAR TEXT: EILTIPAS;     VAR SIMBSK: INTEGER ;
                   VAR SIMBRINK:ALFA;  VAR IVESTASIMB:INTEGER);
 VAR
   SIMB: CHAR; REIKSIMB ,I,K,L,J,T: INTEGER; SIMTIP :SIMBTIPAS;
   TARPAI : ALFA;  AIBETARPO : SET OF SIMBTIPAS;
   BEGIN I:= 0;IVESTASIMB:=0;
     TARPAI := BLANKAI;
     AIBETARPO := [RAIDE..KITI] - [TARPAS];
     IF IKIOBJ(TARPAI,AIBETARPO) AND (SIMBSK > 0) THEN
       BEGIN
       SIMB:=ZENKLA;  SIMTIP := ZKLASE(SIMB);
     IF SIMBSK> CIBREZ.EILILGIS THEN REIKSIMB:= CIBREZ.EILILGIS
                                ELSE REIKSIMB:= SIMBSK;
        WHILE (I < REIKSIMB) AND  NOT ( INALFA(SIMB,SIMBRINK)
            OR (SIMTIP = EGALAS)) DO
           BEGIN I:= I + 1;
                 TEXT[I]:= SIMB;
                 SIMB:= ZENKLA;  SIMTIP := ZKLASE(SIMB)
           END;
        IVESTASIMB:= I;
        IF NOT INALFA(SIMB,SIMBRINK) THEN  ZENATG;
      END; (*IKIOBJ*)
 (*=Z-*) IF CIBDER[5]> 0 THEN
         BEGIN
           WRITE(' --- IVTRTX.');
           WRITELN('TEKSTO ILGIS=',IVESTASIMB:4,'<=',SIMBSK:4);
           WRITE(' =>');
           T:=1; I:=0; K:=0; L:=0; J:=0;
           (*=F+*) PRITEX(TEXT,T,IVESTASIMB,I,L,J,K);
           WRITELN('<=')
         END;
 (*=Z+*)
   END;
     PROCEDURE IVASSK(VAR ASTSKAICIUS:INTEGER;VAR SYMBSK:INTEGER;
                       VAR IVSIMB: INTEGER; VAR  ERRNR: INTEGER);
       VAR
         I,J,K: INTEGER;
         NEG,PRALEIST: BOOLEAN;
         SIMB : CHAR ;
         OBJ:SET OF SIMBTIPAS;
         SIMRINK:ALFA;
       BEGIN
         I := 0;IVSIMB:=0;
         ASTSKAICIUS := 0;
         ERRNR := 0;
         NEG := FALSE;
         K := SYMBSK-MAXSVILGIS;
         J:=MAXSVILGIS;
         PRALEIST:=TRUE;
         IF (SYMBSK<=MAXSVILGIS) AND (SYMBSK<>0)
           THEN
             BEGIN
               J := SYMBSK;
               PRALEIST:=FALSE
             END;
         OBJ:=[SKAITMUO];
         SIMRINK:='-     ';
         IF NOT IKIOBJ(SIMRINK,OBJ) THEN
           ERRNR:=21 (*NEIVESTAS ASTUNTAINIS SVEIKAS SKAICIUS*)
         ELSE
           BEGIN
             SIMB:=ZENKLA;
             IF SIMB= '-' THEN
               BEGIN
                 NEG:=TRUE;
                 SIMB:=ZENKLA
               END;
             WHILE (ZKLASE(SIMB) = SKAITMUO) AND (I<J) DO
               BEGIN
                 I := I+1;
                 K := ORD(SIMB) - ORD('0');
                 IF (K >= 0) AND (K <= 9) THEN
                   ASTSKAICIUS := ASTSKAICIUS*8+K
                 ELSE
                   ERRNR := 28; (*NE ASTUNTAINIS SKAITMUO*)
                 SIMB:=ZENKLA
               END;
             IF (ZKLASE(SIMB) = SKAITMUO) AND PRALEIST THEN
                 BEGIN
                   ERRNR:=20; (*ILGAS SVEIKAS SKAICIUS*)
                   WHILE (ZKLASE(SIMB) = SKAITMUO) DO
                     SIMB:=ZENKLA
                 END;
             ZENATG;
             IF NEG THEN ASTSKAICIUS := (-ASTSKAICIUS);
             IVSIMB := I
            END;
         (*=Z-*)
         IF CIBDER [5] > 0
           THEN
             BEGIN
          WRITELN (' ---IVASSK. SV.AST.SKAICIUS : ',ASTSKAICIUS:1,
                      '  IS  ',IVSIMB:1,' <= ',J:1,'  SKAITMENU')
             END;
         (*=Z+*)
         IF ERRNR <> 0 THEN IVKLAI(ERRNR);
       END; (* IVSVSK *)
     PROCEDURE IVSVSK(VAR SKAICIUS: INTEGER;VAR SYMBSK: INTEGER;
                       VAR IVSIMB: INTEGER; VAR  ERRNR: INTEGER);
       VAR
         I,J,K: INTEGER;
         NEG,PRALEIST: BOOLEAN;
         SIMB : CHAR ;
         OBJ:SET OF SIMBTIPAS;
         SIMRINK:ALFA;
       BEGIN
         I := 0;IVSIMB:=0;
         SKAICIUS := 0;
         ERRNR := 0;
         NEG := FALSE;
         K := SYMBSK-MAXSVILGIS;
         J:=MAXSVILGIS;
         PRALEIST:=TRUE;
         IF (SYMBSK<=MAXSVILGIS) AND (SYMBSK<>0)
           THEN
             BEGIN
               J := SYMBSK;
               PRALEIST:=FALSE
             END;
         OBJ:=[SKAITMUO];
         SIMRINK:='-     ';
         IF NOT IKIOBJ(SIMRINK,OBJ) THEN
           ERRNR:=21 (*NEIVESTAS SVEIKAS SKAICIUS*)
         ELSE
           BEGIN
             SIMB:=ZENKLA;
             IF SIMB= '-' THEN
               BEGIN
                 NEG:=TRUE;
                 SIMB:=ZENKLA
               END;
             WHILE (ZKLASE(SIMB) = SKAITMUO) AND (I<J) DO
               BEGIN
                 I := I+1;
                 SKAICIUS := SKAICIUS*10+ORD(SIMB)-ORD('0');
                 SIMB:=ZENKLA
               END;
             IF (ZKLASE(SIMB) = SKAITMUO) AND PRALEIST THEN
                 BEGIN
                   ERRNR:=20; (*ILGAS SVEIKAS SKAICIUS*)
                   WHILE (ZKLASE(SIMB) = SKAITMUO) DO
                     SIMB:=ZENKLA
                 END;
             ZENATG;
             IF NEG THEN SKAICIUS := (-SKAICIUS);
             IVSIMB := I
            END;
         (*=Z-*)
         IF CIBDER [5] > 0
           THEN
             BEGIN
               WRITELN (' ---IVSVSK. SV. SKAICIUS : ',SKAICIUS:1,
                       '  IS  ',IVSIMB:1,' <= ',J:1,'  SKAITMENU')
             END;
         (*=Z+*)
         IF ERRNR <> 0 THEN IVKLAI(ERRNR);
       END; (* IVSVSK *)
     PROCEDURE IVSVIN(VAR SKAICIUS: INTEGER;VAR MIN: INTEGER ;
           VAR MAX:INTEGER; VAR IVSIMB: INTEGER; VAR  ERRNR: INTEGER);
       VAR
         I: INTEGER;
         SYMBSK,SK: INTEGER;
         NEG: BOOLEAN;
         SIMB : CHAR ;
       BEGIN (*IVSVIN*)
         I := 0;
         SKAICIUS := 0;
         (*=Z-*)
         IF CIBDER [5] > 0
           THEN
             BEGIN
               WRITELN (' ---IVSVIN. SKAICIUS IS INTERVALO [ ',
                    MIN:1,' *** ',MAX:1,' ]')
             END;
         (*=Z+*)
         SYMBSK:=0;
         IVSVSK(SKAICIUS,SYMBSK,IVSIMB,ERRNR);
         IF ERRNR = 0
           THEN
             BEGIN
               IF SKAICIUS < MIN
                 THEN
                   ERRNR := 22;
               IF (SKAICIUS > MAX)
                 THEN
                   ERRNR := 23
             END;
        IF ERRNR <> 0 THEN IVKLAI(ERRNR)
       END; (* IVSVIN *)
     PROCEDURE IVRESK (VAR SREAL: REAL; VAR KLAIDA : INTEGER );
       VAR
         ERR,SVD,SVDNR: INTEGER;
         TRDILGIS,TRD,TRDNR,J: INTEGER;
         ILGTRD,ILGSVD:INTEGER;
         REZ:INTEGER;
         M: REAL;
         NEIG,VESTITRD,RADAU : BOOLEAN;
         SIMB:CHAR;
         OBJ:SET OF SIMBTIPAS;
         SIMRINK:ALFA;
       BEGIN
         SREAL:=0.0; KLAIDA := 0;
         SVD :=0; TRD :=0;
         ILGTRD:=0; ILGSVD:=0;
         M:=0.0;
         VESTITRD:=FALSE;
         NEIG := FALSE;
         RADAU:=FALSE;
         (*=Z-*)
         REZ := CIBDER[5];
         IF CIBDER[5] < 2 THEN
           CIBDER[5]:=0;
         (*=Z+*)
         OBJ:=[SKAITMUO];
         SIMRINK:='-.    ';
         IF NOT IKIOBJ(SIMRINK,OBJ) THEN
           KLAIDA:=21 (*NEIVESTAS SKAICIUS*)
         ELSE
           BEGIN
             SIMB := ZENKLA;
             IF SIMB= '-' THEN
               BEGIN
                 NEIG:=TRUE;
                 SIMB:=ZENKLA
               END;
             IF SIMB='.' THEN
               VESTITRD:=TRUE;
             IF ZKLASE(SIMB)=SKAITMUO THEN
               BEGIN
                 ZENATG;
                 RADAU:=TRUE;
                 IVSVSK(SVD,ILGSVD,SVDNR,ERR);
                 IF ERR<>0 THEN KLAIDA:=31;(*ILGA SVEIKOJI DALIS*)
                 SIMB:=ZENKLA;
                 IF SIMB <> '.' THEN
                   BEGIN
                     KLAIDA := 32; (*NERA TASKO*)
                     ZENATG
                   END
                 ELSE
                   VESTITRD:=TRUE
             END;
         IF (KLAIDA<>32) AND VESTITRD THEN
           BEGIN
             SIMB:=ZENKLA;
             IF ZKLASE(SIMB)=SKAITMUO THEN
               BEGIN
                 ZENATG;
                 RADAU:=TRUE;
                 IVSVSK(TRD,ILGTRD,TRDNR,ERR);
                 IF (ERR<>0) AND (KLAIDA<>31) THEN
                   KLAIDA:=33 (*ILGA TRUPMENINE DALIS*)
               END
             ELSE
               ZENATG
           END;
         IF NOT RADAU THEN KLAIDA:=21;(*NERA SKAITMENU*)
         IF (KLAIDA = 0) OR (KLAIDA = 33)
           THEN
             BEGIN
               TRDILGIS:=MAXREILGIS-SVDNR;
               IF TRDNR>TRDILGIS THEN
                 BEGIN
                   FOR J:=1 TO TRDNR-TRDILGIS DO
                     TRD:=TRD DIV 10;
                   TRDNR:=TRDILGIS
                 END;
               M:= TRD;
               FOR J := 1 TO TRDNR DO
                 M := M/10;
               SREAL := SVD+M;
               IF NEIG THEN SREAL := (-SREAL)
             END
             END;
         (*=Z-*)
         CIBDER[5]:=REZ;
         IF CIBDER [5] > 0
           THEN
             BEGIN
               J:=SVDNR;
               TRDNR:=TRDNR+20;
               WRITELN (' ---IVRESK. IVESTAS REALUS SKAICIUS : ',
                           SREAL:J:TRDNR)
             END;
         (*=Z+*)
         IF KLAIDA <> 0 THEN IVKLAI(KLAIDA)
   END; (* IVRESK *)
    PROCEDURE IVDATA(VAR DKODAS:INTEGER;VAR LIETRUS:INTEGER;
                     VAR KLAIDA:INTEGER);
    VAR
      J,I,L,IVSIMB,REZ :INTEGER;
      SIMB:CHAR;
      DAT:ARRAY [1..3] OF INTEGER;
      SYMBSK:INTEGER;
      OBJ:SET OF SIMBTIPAS;
      SIMRINK:ALFA;
    BEGIN
      J:=1;
      KLAIDA:=0;
      DKODAS:=0;
      SYMBSK := 0;
      (*=Z-*)
      REZ := CIBDER[5];
      IF CIBDER[5] < 2 THEN
        CIBDER[5] := 0;
      (*=Z+*)
      OBJ:=[SKAITMUO];
      SIMRINK:=BLANKAI;
      IF NOT IKIOBJ(SIMRINK,OBJ) THEN
        KLAIDA:=21 (* NEIVESTA DATA *)
      ELSE
        BEGIN
          SIMB := ZENKLA;
          IF ZKLASE(SIMB) <> SKAITMUO THEN
            KLAIDA:=26
          ELSE
            ZENATG;
          WHILE (J <= 3) AND (KLAIDA = 0) DO
            BEGIN
              IVSVSK(DAT[J],SYMBSK,IVSIMB,KLAIDA);
              IF J < 3 THEN
                BEGIN
                  SIMB:=ZENKLA;
                  IF (SIMB='.') OR (SIMB='/') THEN
                    BEGIN
                      SIMB:=ZENKLA;
                      IF ZKLASE(SIMB) <> SKAITMUO
                        THEN KLAIDA:=25
                    END
                  ELSE KLAIDA:=24;
                  ZENATG
                END;
              J:=J+1
            END;
          IF KLAIDA = 0 THEN
            BEGIN
              IF LIETRUS > 0 THEN
                BEGIN
                  L:=DAT[1];
                  DAT[1]:=DAT[3];
                  DAT[3]:=L;
                END;
            IF (DAT[1] >= 0) AND (DAT[1] <= 99) THEN
              DAT[1]:=1900+DAT[1];
            IF     ( (DAT[1] < 1800) OR (DAT[1] > 2100)
                OR   (DAT[2] < 1   ) OR (DAT[2] >   12)
                OR   (DAT[3] < 1   ) OR (DAT[3] >   31) ) THEN
              KLAIDA:=27
          END
        END;
      IF KLAIDA=0 THEN
        DKODAS:=DAT[3]+100*DAT[2]+10000*DAT[1]
        ELSE
          IVKLAI(KLAIDA);
      (*=Z-*)
      CIBDER[5]:=REZ;
      IF CIBDER[5]>0
        THEN
          WRITELN (' ---IVDATA. DATOS KODAS : ',DKODAS:1 )
      (*=Z+*)
    END; (*IVDATA*)
  PROCEDURE IVILTX(VAR ILGTXT:TEXTTIPAS;VAR SIMBSK:INTEGER;
                      VAR PABSIMB:ALFA;VAR IVESTASIMB:INTEGER);
    VAR
      I,K,L,J,T :INTEGER;
      LF,TARPLF:BOOLEAN;
      TARPAI:ALFA;
      SIMKLA:SIMBTIPAS;
      AIBETARPO:SET OF SIMBTIPAS;
      SIMB:CHAR;
  BEGIN
    LF:=FALSE;
    TARPLF:=FALSE;
    I:=0;IVESTASIMB:=0;
    IF SIMBSK>MAXTEXT THEN
      SIMBSK:=MAXTEXT;
    TARPAI := BLANKAI;
    AIBETARPO := [RAIDE..KITI] - [TARPAS];
    IF IKIOBJ(TARPAI,AIBETARPO) AND (SIMBSK > 0) THEN
    BEGIN
      SIMB:=ZENKLA;
    SIMKLA:=ZKLASE(SIMB);
    WHILE NOT INALFA(SIMB,PABSIMB) AND (I<SIMBSK)
        AND NOT (SIMKLA IN [FGALAS,TGALAS]) DO
      BEGIN
        IF SIMKLA <> EGALAS THEN
          BEGIN
            IF SIMKLA <> TARPAS THEN
              I:=I+1
            ELSE (*TARPAS*)
              BEGIN
                IF (NOT LF) THEN
                  I:=I+1;
                IF TARPLF THEN
                  BEGIN
                    CASE CIBREZ.KOKIETARP OF
                      SUSPAUSTI:
                        BEGIN
                        END;
                      PAKUOTI:
            SIMB:=CHR(ORD(SIMB)+ORD(ILGTXT[I])-MAXRAIDKOD);
                      REIKSMINIAI:
                        I:=I+1
                    END  (*CASE*)
                  END
              END;
            LF:=FALSE;
            TARPLF:=FALSE
          END
        ELSE (*EILUTES GALAS-LF*)
          BEGIN
            LF:=TRUE;
            SIMB:=ILGTXT[I];
            IF ZKLASE(SIMB)=TARPAS THEN
              BEGIN
                TARPLF:=TRUE
              END
            ELSE (*PRIES LF NEBUVO TARPO*)
              BEGIN
                I:=I+1;
                CASE CIBREZ.KOKIETARP OF
                  SUSPAUSTI,REIKSMINIAI:
                    SIMB:=BLANK;
                  PAKUOTI:
                    SIMB:=CHR(MAXRAIDKOD+1)
                END (*CASE*)
              END
          END;
        ILGTXT[I]:=SIMB;
            SIMB:=ZENKLA;
            SIMKLA:=ZKLASE(SIMB)
      END; (*WHILE*)
          IF (NOT TARPLF) AND LF THEN
        I:=I-1;
      IVESTASIMB:=I;
      IF NOT (INALFA(SIMB,PABSIMB)) THEN ZENATG;
      END; (*IKIOBJ*)
      (*=Z-*)
      IF CIBDER[5]>0 THEN
        BEGIN
          WRITELN(' ---IVILTX. IVESTAS ILGAS TEKSTAS.TEKSTO ILGIS=',
                  IVESTASIMB:4,'<=',SIMBSK:4);
          WRITE ('  =>');
          T:=1; I:=0; K:=0; L:=0; J:=0;
          (*=F+*) PRITEX(ILGTXT,T,IVESTASIMB,I,K,L,J);
          WRITELN ('<=')
        END;
      (*=Z+*)
    END;(*IVILTX*)
 BEGIN (* PROG. IVFUNK *)
 END.
*TO PERSO:670100,17,*KERMIT*
*END FILE
