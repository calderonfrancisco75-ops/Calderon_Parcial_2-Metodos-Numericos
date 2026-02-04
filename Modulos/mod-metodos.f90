Module Modulo_Metodos
  
  use modulo_precision
  implicit none
  
  contains
 !_________________________________________________________Simpson____________________________________________________________________
 
  subroutine simpson ( integ ,a ,b , nn , funcion) 
    implicit none
    
    integer , intent (in) :: nn
    real (kind=wp), intent (in) :: a , b
    real (kind=wp), intent (out) :: integ
    real ( kind = wp ) :: h , integ1 , integ2 , x, funcion
    integer :: j
    
    if (2*((nn+1)/2) == (nn+1) ) then

      write (* ,*) " error subroutine simpson : "
      write (* ,*) " el numero nn de intervalos debe ser par "
      integ = 0.0_wp
      
    else
      
      h = (b - a)/( 1.0_wp *nn ) 
      integ1 = 0._wp

      do j = 1 , nn-1 , 2 

        x = a + j * h
        integ1 = integ1 + funcion(x)

      end do
    
      integ2 = 0._wp
      
      do j = 2 , nn-2 , 2

        x = a + j * h
        integ2 = integ2 + funcion(x)
    
      end do

      integ = h *((funcion(a) + funcion(b)) + 4._wp * integ1 + 2._wp * integ2 )/3._wp
      
    end if
    
    end subroutine simpson
    
 !_________________________________________Trapecio no equispaciado______________________________________________________________

    
  Subroutine Trapecio_No_equispaciado(v1, v2, Integral)

    real(rd), intent(out)   :: Integral
    integer(id)             :: i, n
    real(rd), dimension(0:) :: v1, v2

    n = size(v1, kind=id) -1_id

    Integral = 0.d0
    do i=0, n - 1_id
      Integral = Integral + ( v1(i+1) - v1(i) ) * ( v2(i+1) + v2(i)) / 2.d0
    end do

  end subroutine Trapecio_No_equispaciado

  !__________________________ Derivadas Numéricas ________________________!

  subroutine dydx_NoEquiespaciado(vx, vy, vd)
    real(kind=rd), dimension(0:), intent(in) :: vx, vy
    real(kind=rd), allocatable, dimension(:), intent(out) :: vd
    integer(kind=id) :: i, n 
    n = size(vx, kind=id) - 1_id
    allocate(vd(0:n))

    vd(0) = d2Pt1er(vx, vy, 0_id)
    vd(n) = d2PtUlt(vx, vy, n)

    do i = 1, n-1_id
    
      if ((i==1).or.(i==n-1)) then
        vd(i) = d3PtCen(vx, vy, i)
      else if ((1<i).and.(i<n-1)) then
        vd(i) = d5PtCen(vx, vy, i)
      end if 

    end do

  end subroutine
  
  function d2Pt1er(vx, vy, i)
    implicit none 
    real(kind=rd), dimension(0:), intent(in) :: vx, vy 
    integer(kind=id), intent(in) :: i 
    real(kind=rd) :: d2Pt1er
    d2Pt1er = ( vy(i+1) - vy(i) ) / ( vx(i+1) - vx(i) )
  end function

  function d2PtUlt(vx, vy, i)
    implicit none 
    real(kind=rd), dimension(0:), intent(in) :: vx, vy 
    integer(kind=id), intent(in) :: i 
    real(kind=rd) :: d2PtUlt
    d2PtUlt = ( vy(i) - vy(i-1) ) / ( vx(i) - vx(i-1) )
  end function

  function d3PtCen(vx, vy, i)
    implicit none 
    real(kind=rd), dimension(0:), intent(in) :: vx, vy 
    integer(kind=id), intent(in) :: i 
    real(kind=rd) :: d3PtCen
    d3PtCen = ( vy(i+1) - vy(i-1) ) / ( vx(i+1) - vx(i-1) )
  end function

  function d5PtCen(vx, vy, i)
    implicit none 
    real(kind=rd), dimension(0:), intent(in) :: vx, vy 
    integer(kind=id), intent(in) :: i 
    real(kind=rd) :: d5PtCen
    d5PtCen = ( vy(i-2) - 8*vy(i-1) + 8*vy(i+1) - vy(i+2)) / ( 3 * (vx(i+2) - vx(i-2)))
  end function

  !_____________________________ Interpolación de Lagrange __________________________!

  function PL(vx, vy, x)
    real(kind=rd), intent(in) :: x
    real(kind=rd), dimension(0:) :: vx, vy
    real(kind=rd) :: PL, P
    call lagrange(vx, vy, x, P)
    PL = P 
  end function

  subroutine lagrange(vx, vy, x, P)

    real(kind=rd), intent(in), dimension(0:) :: vx, vy
    real(kind=rd), intent(in) :: x
    integer(kind=id) :: i, k, n
    real(kind=rd) :: L
    real(kind=rd), intent(out) :: P

    n = size(vx, kind=id) - 1_id

    P = 0.0_wp

    do k = 0, n

        L = 1.0_wp

        do i = 0, n 

            if( i /= k ) then
                L = L * ( ( x - vx(i) ) / ( vx(k) - vx(i) ) )
            end if

        end do

        P = P + L * vy(k)

    end do

  end subroutine

  !_________________________________Subrutinas útiles______________________________!

  Subroutine LoadData3(FileName, v1, v2, v3)   

    implicit none
  
    real(kind=rd), allocatable, dimension(:) :: v1, v2, v3
    character(*), intent(in)                 :: FileName
    integer(kind=id) :: n, S, j, i, io_stat

    call CountData(FileName, S)
    n = S - 2_id ! Le resto la primera fila, y uno más para empezar desde 0
    allocate(v1(0:n), v2(0:n), v3(0:n))

    open(unit=122, file=FileName)

    j = 0

    do i = 1, S
      read(122,*,iostat=io_stat) v1(i-1-j), v2(i-1-j), v3(i-1-j)
      if (io_stat/=0) then
        j = j + 1_id
      end if 
    end do

    close(122)

  end subroutine

  Subroutine LoadData2(FileName, v1, v2)   

    implicit none
  
    real(kind=rd), allocatable, dimension(:) :: v1, v2
    character(*), intent(in)                 :: FileName
    integer(kind=id) :: n, S, j, i, io_stat

    call CountData(FileName, S)
    n = S - 2_id ! Le resto la primera fila, y uno más para empezar desde 0
    allocate(v1(0:n), v2(0:n))

    open(unit=122, file=FileName)

    j = 0

    do i = 1, S
      read(122,*,iostat=io_stat) v1(i-1-j), v2(i-1-j)
      if (io_stat/=0) then
        j = j + 1_id
      end if 
    end do

    close(122)

  end subroutine

  subroutine CountData(FileName, DataSize)

    implicit none

    character(*), intent(in)        :: FileName
    integer(kind=id), intent(out)   :: DataSize
    integer(kind=is)                :: Unidad, EOF  
    real(kind=rs)                   :: x
  
    open(unit=unidad, file=FileName)

    DataSize = 0  
    do
      read(unidad, *, iostat = EOF ) x
      if (EOF == iostat_end) exit
      DataSize = DataSize + 1_id  
    end do
  
    close(unidad)

  end subroutine
    
End Module
    
