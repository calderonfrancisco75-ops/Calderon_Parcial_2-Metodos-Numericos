set title "Velocidad en función de la posición"

set xlabel 'Posición [m]'
set ylabel 'Velocidad [m/s]'

###luego fijo la densidad de puntos al graficador

set samples 500

set grid

set term png enhanced size 1280,960 font 'Ubuntu,27'
set output "../Graficos/figura1d.png"


plot "../Datos/salida.dat" u 2:3 t " v₁(t) " w points ps 2 pt 5