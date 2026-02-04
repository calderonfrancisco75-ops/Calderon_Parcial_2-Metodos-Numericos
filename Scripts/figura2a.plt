set title "Posición en funcion del tiempo"

set xlabel 'Tiempo [s]'
set ylabel 'Posición [m]'

###luego fijo la densidad de puntos al graficador

set samples 500

set grid

set term png enhanced size 1280,960 font 'Ubuntu,27'
set output "../Graficos/figura2a.png"


plot "../Datos/datos.dat" u 1:2 t " Z₁(t) " w points ps 2 pt 5
