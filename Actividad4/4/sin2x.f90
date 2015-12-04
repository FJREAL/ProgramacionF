program sin2x

implicit none
integer :: i, npts
real :: x, f_x, g_x, h_x, dx
real, parameter :: pi = 4.0 * atan (1.0)
real, parameter :: epsilon = 0.9, alpha = 1.1

print *, 'Este programa se encarga de confirmar si el seno cuadrado de&
     & x mas el coseno cuadrado de x es igual a uno. Si esto es asi, e&
     &l programa nos lo dirá'
print *,  'Dame el número de puntos en el intervalo npts= '
  read(*,*) npts

   dx = (2.0 * pi) /float(npts)
   write(*,*) 'dx= ', dx
 

  x = 0.0

 open(unit=11, file='sin2x.dat')

  do i = 1 , npts+1, 1 

     x = dx * float(i-1)

     f_x = sin(2*x)

     write(11,*) x , f_x

  enddo
  
 close(11)

 open(unit=11, file='2sinx.dat')

  do i = 1 , npts+1, 1 

     x = dx * float(i-1)

     g_x = 2*sin(x)*cos(x)

     write(11,*) x , g_x

  enddo
  
 close(11)

 open(unit=11, file='2tanx.dat')

  do i = 1 , npts+1, 1 

     x = dx * float(i-1)

     h_x = (2*tan(x))/(1 + (tan(x))**2)
     
     write(11,*) x , h_x

  enddo
  
  close(11)
   
end program sin2x
  
