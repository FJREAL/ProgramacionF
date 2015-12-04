program TierraMarte
! definimos las variables
  IMPLICIT NONE
INTEGER :: i
COMPLEX :: r_T, r_M, D
INTEGER :: t
REAL, parameter :: pi = 4.0 * atan (1.0), dm=1.523662
COMPLEX, parameter :: c = (0.0,1.0)
REAL :: Distancia, dx

print *, 'Dame t:'
read *, t

dx= 0.05
open(unit=11, file='TM.dat')
do i=1, t, 1

   
   r_T=(cexp(2.0*pi*c*dx*float(i)))

   r_M= ((cexp(2.0*pi*c*(dm**(3/2))*dx*float(i)))*dm)

   D= r_T - r_M

  Distancia= REAL(csqrt(D*conjg(D)))
   print *, 'La distancia del Sol a la tierra en el tiempo:  ',i, 'es:&
        &', Distancia,'UA'
   write(11,*) i, Distancia
enddo

end program TierraMarte

