program sin3x

implicit none
integer :: i, npts
real :: x, f_x,g_x, dx
real, parameter :: pi = 4.0 * atan (1.0)
real, parameter :: epsilon = 0.9, alpha = 1.1

print *,  'Dame el número de puntos en el intervalo npts= '
  read(*,*) npts

   dx = (2.0 * pi) /float(npts)
   write(*,*) 'dx= ', dx
 

  x = 0.0

 open(unit=11, file='sin3x.dat')

  do i = 1 , npts+1, 1 

     x = dx * float(i-1)

     f_x = sin(3*x)

     write(11,*) x , f_x

  enddo
  
 close(11)

  open(unit=11, file='4sinx.dat')

  do i = 1 , npts+1, 1 

     x = dx * float(i-1)

     g_x = -(4*(sin(x))**3) + (3*sin(x))

     write(11,*) x , g_x

  enddo
  
 close(11)
   if(g_x == f_x) write (*,*) '4Sin(x) = -4Sin**3(x) + 3Sin(x)'
   
end program sin3x  
