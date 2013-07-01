#include <omp.h>
#include <stdio.h>

int main(){
int i;
	#pragma omp for
    for(i=0; i<1000; i++)
        	printf("%d\n", i);	
    
    
    #pragma omp parallel
    printf("hello world!\n");
	return(0);
}