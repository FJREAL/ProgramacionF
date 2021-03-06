program taylor

  implicit none
integer :: i, npts
real :: x, costaylor, tantaylor, dx, sintaylor, f_x, g_x, h_x
real, parameter :: pi = 4.0 * atan (1.0)
 

  print *,  'Dame el número de puntos en el intervalo npts= '
  read(*,*) npts

   dx = (2.0 * pi) /float(npts)
   write(*,*) 'dx= ', dx
 

  x = 0.0

  open(unit=11, file='sintaylor.dat')

   do i = 1, npts+1, 1

    x = dx * float(i-1)

    sintaylor = x-(x**3/6.0)

    write (11,*), x, sintaylor

 enddo

 close(11)

  open(unit=11, file='costaylor.dat')

   do i = 1, npts+1, 1

    x = dx * float(i-1)

    costaylor = 1 - (x**2/2.0) + 0

    write (11,*), x, costaylor

 enddo

 close(11)

   open(unit=11, file='tantaylor.dat')

   do i = 1, npts+1, 1

    x = dx * float(i-1)

    tantaylor = (x**3/3.0) + 0 + x

    write (11,*), x, tantaylor

 enddo

 close(11)

 open(unit=11, file='seno.dat')

  do i = 1 , npts+1, 1 

     x = dx * float(i-1)

     f_x = sin(x)

     write(11,*) , x , f_x
     
  enddo

close(11)

open(unit=11, file='coseno.dat')

  do i = 1, npts+1, 1 

     x = dx * float(i-1)

     g_x = cos(x)

     write(11,*) , x , g_x

     enddo
 close(11)

open(unit=11, file='tangente.dat')

  do i = 1, npts+1, 1 

     x = dx * float(i-1)

     h_x = tan(x)

     write(11,*) , x , h_x

     enddo
 close(11)

 endprogram taylor
