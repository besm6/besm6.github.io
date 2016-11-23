*NAME N
*TIME:24.00
*no load
      programma
      dimension k(14)
      do  100 i=1,100
 100  print 101,i
      stop
 101  format(i5,1x,100(1h*))
      read 2,k
  2   format(13a6,a2)
      read 10,n,b $ a=0.
  10  format(i2,f10.4)
      print 1
  1   format(' hello')
      print 3,k,a,b
  3   format(2x,14a6//' sin**2+cos**2 TABLE from ',f7.4,' to ',
     *   f7.4/)
      do 20 i=1,n
      x=a+(i-1)*((b-a)/(n-1))
c     s=sin(x) $ c=cos(x)
      s=x $ c=1.-x**2/2.
      s2=s**2+c**2
  20   print 21,i,x,s,c,s2
  21  format(i3,' : x=',f8.4,3f18.10)
      end
*execute
  A.P.Sapoinikov  and  T.F.Sapoinikova
12 3.1416
*END F
