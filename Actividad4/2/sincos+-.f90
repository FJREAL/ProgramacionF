program sincosmasmenos

implicit none
integer :: i, npts
real :: x, f_x, g_x, h_x, i_x, dx
real, parameter :: pi = 4.0 * atan (1.0)

print *, 'Este programa nos dice si las funciones sin(-x) y cos(-x) so&
     &n iguales a -sin(x) y -cos(x) respectivamente. Si no los son, el&
     & programa nos lo indicara'

print *,  'Dame el número de puntos en el intervalo npts= '
  read(*,*) npts

   dx = (2.0 * pi) /float(npts)

   write(*,*) 'dx= ', dx

   x = 0.0
   
  open(unit=11, file='sin4.dat')

  do i = 1 , npts+1, 1

     x = dx * float(i-1)

     f_x =  sin(-x)

     write(11,*) , x, f_x

  enddo

  close(11)

  open(unit=11, file='-sin4.dat')

  do i=1, npts+1, 1

     x= dx*float(i-1)

     g_x = -sin(x)

     write(11,*), x, g_x
  enddo

  if(f_x == g_x) write(*,*) 'sin(-x) y -sin(x) son iguales'
  if(f_x .NE. g_x) write(*,*) 'sin(-x) y -sin(x) NO son iguales'
     close(11)

open(unit=11, file='cos4.dat')

  do i = 1 , npts+1, 1

     x = dx * float(i-1)

     h_x =  cos(-x)

     write(11,*) , x, h_x

  enddo

  close(11)

  open(unit=11, file='-cos4.dat')

  do i=1, npts+1, 1

     x= dx*float(i-1)

     i_x = -cos(x)

     write(11,*), x, i_x

  enddo

  if(h_x == i_x) write(*,*) 'cos(-x) y -cos(x) son iguales'
  if(h_x .NE. i_x) write(*,*) 'cos(-x) y -cos(x) NO son iguales'
  close(11)

 end program sincosmasmenos
