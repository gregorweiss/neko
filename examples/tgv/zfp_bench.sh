#! /bin/bash

export JSON_Fortran_CFLAGS="-I/home/gregor/Documents/CEEC/git/json-fortran-build/jsonfortran-gnu-8.3.0/lib"
export JSON_Fortran_LIBS="-L/home/gregor/Documents/CEEC/git/json-fortran-build/jsonfortran-gnu-8.3.0/lib -ljsonfortran"
export PKG_CONFIG_PATH=/home/gregor/Documents/CEEC/git/json-fortran-build/jsonfortran-gnu-8.3.0/lib/pkgconfig
export LD_LIBRARY_PATH=/home/gregor/Documents/CEEC/git/json-fortran-build/jsonfortran-gnu-8.3.0/lib:${LD_LIBRARY_PATH}
export LD_LIBRARY_PATH=/home/gregor/CEEC/BigWhoop/BigWhoop/lib:${LD_LIBRARY_PATH}
export LD_LIBRARY_PATH=/home/gregor/CEEC/local/adios2/lib:${LD_LIBRARY_PATH}
export PATH=/home/gregor/CEEC/local/adios2/bin:${PATH}
export PATH=/home/gregor/Documents/CEEC/git/neko-build/bin:${PATH}

makeneko tgv.f90

rm -f zfp_benchmark.dat
touch zfp_benchmark.dat
for RATE in 32 16 8 4 2 1 0.5 0.25 0.125 #0.0625 0.03125 0.015625 0.0078125 0.00390625
do
  sed "s/RATE/${RATE}/g" RATE.xml > adios2.xml
  sed -i "s/COMPRESSOR/zfp/g" adios2.xml
  echo "rate ${RATE}" >> zfp_benchmark.dat
  mpirun -n 1 ./neko tgv.case > output.dat
  tail -n 12 output.dat | grep "PSNR\|MSE" >> zfp_benchmark.dat
  rm adios2.xml output.dat field0.f* bdry0.*
done
