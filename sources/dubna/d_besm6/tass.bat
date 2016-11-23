*name assem
*time:24.00
*disc:667/system,md
*file:madlen,67,r
*assem
  program:,name,
 1,vtm,-1
 :1,xta,*+1
 1,uj,*+1
  ,*70,=0010 0100 0067 0000
  ,*70,=0000 0100 0067 0000
  ,sj,
  ,end,
*execute
*madlen,l=r.
 program:,name,
  ,*64,p
  ,sj,
  p:,z00,t
  ,z00,t
  ,003,
  8,,
  t:,gost,12h privet !'231'
  ,end,
*execute
*end f
