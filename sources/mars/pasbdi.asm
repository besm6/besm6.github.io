          PASBDI:,NAME,DTRAN  /01.06.84/
          P/HT:,SUBP,
          *ERSTOP*:,LC,1
          *BDV*:,LC,1
          *BDC*:LC,BLOCK,CW70,OPEN,DEL,NEW,PUT,GET,MOD
          *BDVECT*:,LC,168
          *BDERRN*:,LC,1
          P/BDSYS:,PC,1024
          P/BDTAB:,PC,1024
          P/BDBUF:,PC,1024
          GAL/3:,EQU,P/BDSYS+3
          P/PRINT:,SUBP,
          PASBDE:,SUBP,
          GAM/2:,EQU,P/BDSYS+2
          PASBDI:,SUBP,
          9,VTM,P/BDSYS
          9,XTA,1
          13,U1A,
          ,ITA,9
          ,ASN,64-20
          10,VTM,CW70
          10,AOX,
          10,ATX,
          10,*70,
          10,VTM,*BDVECT*
          ,ITA,10
          9,ATX,3
          ,UTC,*BDV*
          ,ATX,
          15,MTJ,12
          ,XTA,
          10,MTJ,15
          14,VTM,77531B
          *0012B:15,ATX,
          14,VLM,*0012B
          12,MTJ,15
          14,VTM,P/BDTAB
          ,ITA,14
          10,ATX,17B
          14,VTM,P/BDBUF
          ,ITA,14
          10,ATX,16B
          14,VTM,ERRHANDL
          ,ITA,14
          10,ATX,11B
          13,UJ,
          ERRHANDL:,UTC,*BDERRN*
          ,ATX,
          ,ASN,64-42
          14,VTM,*0054B
          14,AUX,2
          14,ARX,3
          10,VTM,*0041B
          10,ATX,2
          ,ITA,13
          ,ASN,64-33
          14,AUX,
          14,ARX,1
          10,ATX,
          ,WTC,GAL/3
          ,XTA,5
          ,ASN,64-30
          14,AUX,4
          14,ARX,5
          10,ATX,6
          ,WTC,GAL/3
          ,XTA,10B
          11,VTM,*0053B
          11,ATX,
          ,ITA,13
          15,ATX,
          13,VJM,P/PRINT
          ,UTC,*ERSTOP*
          ,XTA,
          ,U1A,P/HT
          13,VJM,PASBDE
          15,WTC,
          ,UJ,
          *0041B:,BSS,1
          ,ISO,6HB ADR,
          ,BSS,1
          ,ISO,18H DATA BASE ERROR,
          ,BSS,1
          ,ISO,18H MICRO RUN, NAME=
          *0053B:,BSS,1
          *0054B:,ISO,6H′′′7′′7′′7′′7′′7′
          ,ISO,6H 00000
          ,ISO,6H′′′′′′′′′7′′7′
          ,ISO,6H    00
          ,ISO,6H′7′′7′′7′′7′′7′′7′
          ,ISO,6H000000
         C===========================================
          PASBD:,ENTRY,
         C===========================================
          *0062B:13,MTJ,14
          ,ITA,4
          4,VTM,P/BDSYS
          4,ATX,33B
          ,UTC,*BDV*
          ,XTA,
          ,UTC,P/BDSYS
          ,ATX,3
          ,UJ,GAM/2
         C===========================================
          PAIB:,ENTRY,
         C===========================================
          ,ITA,13
          ,ITS,14
          ,ITS,11
          15,ATX,
          13,VJM,PASBDI
          ,WTC,*BDV*
          9,VTM,
          15,XTA,-5
          ,UTC,*0117B.=3777 7777 7777 7777
          ,AAX,
          9,ATX,10B
          15,XTA,-4
          ,UTC,*0122B.=7 7777
          ,AAX,
          9,ATX,15B
          15,XTA,-1
          9,ATX,13B
          15,WTC,-2
          ,UTC,OPEN
          ,XTA,
          9,ATX,3
          ,U1A,*0114B
          ,ITA,15
          9,ATX,13B
          15,WTC,-1
          ,XTA,
          ,ASN,64-1
          ,ASN,64+1
          9,XTS,10B
          ,ASN,64-10
          ,UTC,*0116B.=2
          ,XTS,
          9,ATX,15B
          ,UTC,NEWD1
          ,XTA,
          9,ATX,3
          13,VJM,*0062B
          ,UTC,NEWD2
          ,XTA,
          9,ATX,3
          15,UTM,-2
          *0114B:15,WTC,-3
          13,VTM,
          15,UTM,-5
          ,UJ,*0062B
          *0116B:,LOG,2
          *0117B:,LOG,3777 7777 7777 7777
          NEWD1:,LOG,26 2115 1131
          NEWD2:,LOG,1012 1411
          *0122B:,LOG,7 7777
          ,DATA,
          /0123B:,LOG,10 0000 0040 0027
          OPEND:,LOG,25 1214 1131
          DELD:,LOG,2723 1411
          ,LOG,
          PUTD:,LOG,2621 1511
          GETD:,LOG,22 1411
          MODD:,LOG,20 4026 2100 1511
          7,SET,/0123B
          1,,CW70
          ,END,
