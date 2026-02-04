program Parte2

    use modulo_precision
    use Modulo_Metodos
    use modulo_funciones
     
    Implicit none

    real(kind=rd), allocatable, dimension(:)    :: vt, vz, vv
    character(*), parameter                     :: FileName = 'Datos/datos.dat'
    integer(kind=id)                            :: n, i, fu
    real(kind=rd)                               :: h, W

    !____________________ 2B _____________________!

    call LoadData2(FileName, vt, vz)
    call dydx_NoEquiespaciado(vt, vz, vv)
    n = size(vt, kind=id)-1_id

    open(newunit=fu, file='Datos/salida.dat')
    write(fu,*) '         t[s]                      z[m]                      v[m/s]'
    do i = 0, n
        write(fu,*) vt(i), vz(i), vv(i)
    end do
    close(fu)

    !_____________________ 2G _______________________! 

    call simpson ( W, vt(0), vt(n), 100, Fv )
    print*, 'Trabajo con F(t)v(t)dt     = ', W

    !_____________________ 2I _______________________!

    print*, 'Energía cinética final     = ', Ke(v(vt(n)))

    !_____________________ 2J _______________________!

    h = (vt(n) - vt(0)) / 99 

    open(newunit=fu, file='Datos/pol_lagrange.dat')
    write(fu,*) '         t[s]                      z[m]'
    do i = 0, 99
        write(fu,*) vt(0) + i * h, PL(vt, vz, vt(0) + i * h)
    end do
    close(fu)

end program Parte2

