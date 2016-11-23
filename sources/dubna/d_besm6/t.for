*name n
*ftn
      program t
      dimension a(10000)
      print 11
      do 1 i=1,10000
  1   a(i)=sin(i+0.)
      print 12
 11   format(' начало.')
 12   format(' конец.')     
      end
*execute
1
*end file      
      end do
      call b(a)
      print *,' 1000 sin '
      do i=1,1000
        a(i)=sqrt(i+0.)
      end do
      call timere (isec,mil,mic)
      call b(a)
      print *,' 1000 sqrt ',isec+.001*(mil+.001*mic)
      call timers
      do i=1,1000
        a(i)=tan(i*0.001)
      end do
      call timere (isec,mil,mic)
      call b(a)
      print *,' 1000 tan ',isec+.001*(mil+.001*mic)
      call timers
      do i=1,1000
        a(i)=exp(i*0.001)
      end do
      call timere (isec,mil,mic)
      call b(a)
      print *,' 1000 exp ',isec+.001*(mil+.001*mic)
      end

      subroutine b(a)
      dimension a(*)

      end
