Program Parte1

   use modulo_precision
   use Modulo_Metodos
   use modulo_funciones
    
   Implicit none
    
   real(kind=rd)                            :: Integral
   real(kind=rd), allocatable, dimension(:) :: vt, vz, vv, vF
   character(*), parameter                  :: FileName = 'Datos/salida.dat'
   integer(kind=id) :: n, i

   call LoadData3(FileName, vt, vz, vv)
   n = size(vt, kind=id) - 1_id

   allocate(vF(0:n))
   do i = 0, n 
      vF(i) =  - m * g  - c * vv(i)
   end do 

   call Trapecio_No_equispaciado(vz, vF, Integral)
   print*, 'Trabajo con F(z)dz         = ', Integral
      
End program Parte1
    
