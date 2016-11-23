*NAME INTRSP
*TIME:24.00
*EXPRESS
*NO LOAD
      PROGRAMMA
      DIMENSION M1(9),M2(9),M3(9),M4(9),M5(9)
      DATA M1/54HPROGRAMMA                                             /
      DATA M2/54HDIMENSION M1(9),M2(9),M3(9),M4(9),M5(9)               /
      DATA M3/54HPRINT M4,M1,M2,1,M1,2,M2,3,M3,4,M4,5,M5,M3,M5         /
      DATA M4/54H(2(6X,9A6/)5(6X,6HDATA M,I1,4H/54H,9A6,1H//)(6X,9A6)) /
      DATA M5/54HEND                                                   /
      PRINT M4,M1,M2,1,M1,2,M2,3,M3,4,M4,5,M5,M3,M5
      END
*EXECUTE
*END F
