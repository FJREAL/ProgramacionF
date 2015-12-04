program arctan

implicit none
integer :: i, npts
real :: x, f_x, g_x, dx, sintaylor
real, parameter :: pi = 4.0 * atan (1.0)
real, parameter :: epsilon = 1.0E-6
 

  print *,  'Dame el número de puntos en el intervalo npts= '
  read(*,*) npts

   dx = (2.0 * pi) /float(npts)
   write(*,*) 'dx= ', dx
 

  x = 0.0

 open(unit=11, file='arctan.dat')

  do i = 1 , npts+1, 1 

     x = dx * float(i-1)

     f_x = 4.0*atan(x)

     write(11,*) , x , f_x

  enddo

  close(11)

  if(f_x == pi) write(*,*) 'Cuando x=', x, 'f_x es igual a pi'
    
end program arctan
