set title "Posición en funcion del tiempo (medidas, ajuste e interpolación)"

set xlabel 'Tiempo [s]'
set ylabel 'Posición [m]'

set grid

set term png enhanced size 1280,960 font 'Ubuntu,27'
set output "../Graficos/figura1j.png"

g = 9.67979
a = 3.14392
set dummy x

plot "../Datos/pol_lagrange.dat" u 1:2 t " Interpolacion " w lines lw 2, \
     "../Datos/datos.dat" u 1:2 t " Z₁(t) " w points ps 2 pt 5, \
     (-g)*a*x + (g*(a**2))*(1 - exp(- x /a)) title " Ajuste " w lines lw 2 