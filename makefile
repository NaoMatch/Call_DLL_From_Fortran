run:
	gfortran -c test.f90
	gfortran -shared -o test.so test.o
	gfortran -o example example.f90 -ldl
	./example