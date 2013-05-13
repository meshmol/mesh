norm : main.o cell.o list.o compute.o function.o
	gcc  -pg -O2 main.o cell.o list.o compute.o function.o

main.o : main.c norm.h list.c
	gcc  -pg -O2  main.c

cell.o : cell.c norm.h
	gcc -pg  -O2  cell.c

list.o : list.c norm.h
	gcc -pg   -O2 list.c

compute.o : compute.c norm.h
	gcc -pg  -O2  compute.c

function.o : function.c norm.h
	gcc -pg  -O2  function.c
