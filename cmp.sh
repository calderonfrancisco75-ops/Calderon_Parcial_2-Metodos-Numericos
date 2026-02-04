gfortran Modulos/mod-prec.f90 \
         Modulos/mod-metodos.f90 \
         Modulos/mod-func.f90 \
         parcial2-p1.f90 -o P1.x
gfortran Modulos/mod-prec.f90 \
         Modulos/mod-metodos.f90 \
         Modulos/mod-func.f90 \
         parcial2-p2.f90 -o P2.x
./P1.x
./P2.x
cd Scripts
gnuplot figura2a.plt \
        figura2c.plt \
        figura1d.plt \
        figura1f.plt \
        figura1j.plt 
cd ..
rm  P1.x P2.x \
    modulo_funciones.mod \
    modulo_metodos.mod \
    modulo_precision.mod
