set title "Velocidad en función del tiempo"

set xlabel 'Tiempo [s]'
set ylabel 'Velocidad [m/s]'

###luego fijo la densidad de puntos al graficador

set samples 500

set grid

set term png enhanced size 1280,960 font 'Ubuntu,27'
set output "../Graficos/figura2c.png"


plot "../Datos/salida.dat" u 1:3 t " v₁(t) " w points ps 2 pt 5
