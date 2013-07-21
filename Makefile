norm : main.o cell.o list.o compute.o function.o r7rs.o
	gcc -O4 -Wall main.o cell.o list.o compute.o function.o r7rs.o  -o mesh

main.o : main.c norm.h
	gcc -O4 -Wall -c main.c

cell.o : cell.c norm.h
	gcc -O4 -Wall -c cell.c

list.o : list.c norm.h
	gcc -O4 -Wall -c list.c

compute.o : compute.c norm.h
	gcc -O4 -Wall -c compute.c

function.o : function.c norm.h
	gcc -O4 -Wall -c function.c

r7rs.o : r7rs.c norm.h
	gcc -O4 -Wall -c r7rs.c


