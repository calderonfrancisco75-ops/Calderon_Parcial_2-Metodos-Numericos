module modulo_funciones
    use modulo_precision

    implicit none 

    real(kind=rd), parameter :: a = 3.14392_wp, g = 9.67979_wp
    real(kind=rd), parameter :: m = 0.001, c = m / a

    contains

    function z(t)
       implicit none 
       real(kind=rd), intent(in) :: t 
       real(kind=rd) :: z
       z = - g * a * t + g * a**2 * ( 1 - exp(-t / a))
    end function

    function v(t)
        implicit none 
        real(kind=rd), intent(in) :: t 
        real(kind=rd) :: v
        v = g * a * (exp ( -t / a) - 1)
    end function

    function Ft(t)
        implicit none 
        real(kind=rd), intent(in) :: t 
        real(kind=rd) :: Ft
        Ft = - m * g  - c * v(t)
    end function

    function Fv(t)
        implicit none 
        real(kind=rd), intent(in) :: t 
        real(kind=rd) :: Fv
        Fv = Ft(t) * v(t)
    end function

    function Ke(v)
        implicit none 
        real(kind=rd), intent(in) :: v 
        real(kind=rd) :: Ke
        Ke = 0.5 * m * v**2
    end function    

end module 