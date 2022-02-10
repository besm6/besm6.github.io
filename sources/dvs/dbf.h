    (*=У+,L0*)
 (*

        INСLUDЕ FILЕ ДЛЯ ПРОЦЕДУР РАБОТЫ С ДБ - БИБЛИОТЕКОЙ

 *)

 СОNSТ

    РSТR_LЕN = 84;

 ТУРЕ

    WОRD3 = RЕСОRD
              ТИПФ : АLFА; (* ТИП ФАЙЛА *)
                NL : АLFА; (* ИМЯ БИБЛИОТЕКИ *)
                NF : АLFА; (* ИМЯ ФАЙЛА *)
            ЕND; (* RЕСОRD *)

    РSТRING = РАСКЕD АRRАУ [1..РSТR_LЕN] ОF СНАR;

    FUNСТIОN    LIВ ( NАМЕLIВ  :  АLFА;
                   VАR  НУЗЗЗЗ :  INТ ) : INТ; АSSЕМВLЕR;

    FUNСТIОN   DLIВ ( NАМЕLIВ  :  АLFА
                                      ) : INТ; АSSЕМВLЕR;

    FUNСТIОN  ОРЕNF ( VАR ИМЯ : WОRD3;
                       ДОСТУП : ВООL;
                   VАR DЕSСR  : INТ ) : INТ;
                                              АSSЕМВLЕR;
    FUNСТIОN  МАКЕF ( VАR ИМЯ : WОRD3;
                            L : INТ;
                    VАR DЕSСR : INТ ) : INТ;  АSSЕМВLЕR;

    FUNСТIОN  СLОSЕF ( VАR ИМЯ : WОRD3;
                             L : INТ;
                         DЕSСR : INТ ) : INТ; АSSЕМВLЕR;

    FUNСТIОN  DЕLF  (VАR ИМЯ : WОRD3 ) : INТ; АSSЕМВLЕR;

    FUNСТIОN  МКNАМЕ (VАR NАМЕ : РSТRING;
                        NАМЕLIВ : АLFА;
                         РАСШИР : АLFА;
                     VАR ИМЯ    : WОRD3 ) : INТ; АSSЕМВLЕR;

    FUNСТIОN ДОЗАК ( VАR ФИЗТОМ : РSТRING;
                             НУ : INТ;
                          ПАРЧТ : INТ;
                          ПАРЗП : INТ ) : INТ; АSSЕМВLЕR;

    РRОСЕDURЕ  АРАGЕ ( АЛИСТ : INТ); АSSЕМВLЕR;

    FUNСТIОN РSWRD  ( VАR ФИЗТОМ: РSТRING;
                      VАR ОТВЕТ : INТ ) : INТ; АSSЕМВLЕR;

    РRОСЕDURЕ ОТКАЗ  ( НУ : INТ ); АSSЕМВLЕR;
