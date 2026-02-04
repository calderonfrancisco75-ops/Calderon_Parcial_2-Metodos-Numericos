#Defino a t como la variable independiente
set dummy t 

# Definimos la función a ajustar
Z(t) = - g*a*t + (g*(a**2))*(1 - exp(-t/a))  

# Estimacion inicial de los parámetros
a = 5
g = 10


# Realización del Ajuste
# Nuestro conjunto de datos están guardado en el archivo tipo texto llamado "medidas.txt"
set fit logfile "../Datos/figura1f.log"
fit Z(t) "../Datos/datos.dat" every ::1 via a, g



############################## GRAFICO #################

set term png enhanced size 1280,960 font 'Ubuntu,27'
set output '../Graficos/figura1f.png'


set title "Grafico del Ajuste de la Función de movimiento"

set ylabel 'Posición (metros)'
set xlabel 'Tiempo (segundos)'

set grid

plot "../Datos/datos.dat" every ::1 using 1:2  w points pt 6 ps 2 title "Mediciones" , Z(t) title "Curva de Ajuste"

