#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <setjmp.h>
#include <math.h>
#include "norm.h"

extern cell memory[];
extern jmp_buf toplevel;


//--------------リスト操作---------------------
//エラー処理なし。(cdr '()) = '() 内部処理で使う。
int car(int lis){
	return(GET_CAR(lis));
}

int cdr(int lis){
	return(GET_CDR(lis));
}

//エラー処理あり。Schemeレベルで使う。
int scm_car(int lis){
	if(pairp(lis))
    	return(GET_CAR(lis));
    else
    	exception("car", NOT_PAIR, lis);
    return(undef);
}

int scm_cdr(int lis){
	if(pairp(lis))
    	return(GET_CDR(lis));
    else
    	exception("cdr", NOT_PAIR, lis);
    return(undef);
}

int caar(int lis){
	return(car(car(lis)));
}

int cdar(int lis){
	return(cdr(car(lis)));
}

int cadar(int lis){
	return(car(cdr(car(lis))));
}



int cddr(int lis){
	return(cdr(cdr(lis)));
}

int cdddr(int lis){
	return(cdr(cdr(cdr(lis))));
}

int cdadr(int lis){
	return(cdr(car(cdr(lis))));
}


int cadr(int lis){
	return(car(cdr(lis)));
}

int caddr(int lis){
	return(car(cdr(cdr(lis))));
}

int caadr(int lis){
	return(car(car(cdr(lis))));
}

int cadddr(int lis){
	return(car(cdr(cdr(cdr(lis)))));
}


int cons(int car, int cdr){
	int addr;

    addr = freshcell();
   	SET_TAG(addr,LIS);
    SET_CAR(addr,car);
    SET_CDR(addr,cdr);
    return(addr);
}

int assq(int obj, int lis){
	while(!nullp(lis))
    	if(eqp(obj,caar(lis)))
        	return(car(lis));
        else
        	lis = cdr(lis);
    return(BOOLF);
}

int assv(int obj, int lis){
	while(!nullp(lis))
    	if(eqvp(obj,caar(lis)))
        	return(car(lis));
        else
        	lis = cdr(lis);
    return(BOOLF);
}

int assoc(int obj, int lis){
	while(!nullp(lis))
    	if(equalp(obj,caar(lis)))
        	return(car(lis));
        else
        	lis = cdr(lis);
    return(BOOLF);
}

int memq(int obj, int lis){
	while(!nullp(lis))
    	if(eqp(obj,car(lis)))
        	return(lis);
        else
        	lis = cdr(lis);
    return(BOOLF);
}

int memv(int obj, int lis){
	while(!nullp(lis))
    	if(eqvp(obj,car(lis)))
        	return(lis);
        else
        	lis = cdr(lis);
    return(BOOLF);
}

int member(int obj, int lis){
	while(!nullp(lis))
    	if(equalp(obj,car(lis)))
        	return(lis);
        else
        	lis = cdr(lis);
    return(BOOLF);
}


int listtail(int lis, int n){
	while(!(nullp(lis))){
    	if(n == 0)
        	return(lis);
        else{
        	lis = cdr(lis);
            n--;
        }
    }
    if(n == 0)
    	return(NIL);
    else
    	return(BOOLF);
}

int listref(int lis, int n){
	while(!(nullp(lis))){
    	if(n == 0)
        	return(car(lis));
        else{
        	lis = cdr(lis);
            n--;
        }
    }
    return(BOOLF);
}

int drop(int lis, int n){
	while(!(nullp(lis))){
    	if(n == 0)
        	return(lis);
        else{
        	lis = cdr(lis);
            n--;
        }
    }
    return(undef);
}

int length(int lis){
	int len = 0;
    
    while(!(nullp(lis))){
    	len++;
        lis = cdr(lis);
    }
    return(len);
} 

int list(int arglist){
	if(nullp(arglist))
    	return(make_NIL());
    else
    	return(cons(car(arglist),list(cdr(arglist))));    
}

int list1(int x){
	return(cons(x,NIL));
}

int list2(int x, int y){
	return(cons(x,cons(y,NIL)));
}

int list3(int x, int y, int z){
	return(cons(x,cons(y,cons(z,NIL))));
}

int list4(int x1, int x2, int x3, int x4){
	return(cons(x1,cons(x2,cons(x3,cons(x4,NIL)))));
}

int list5(int x1, int x2, int x3, int x4, int x5){
	return(cons(x1,cons(x2,cons(x3,cons(x4,cons(x5,NIL))))));
}

int list6(int x1, int x2, int x3, int x4, int x5, int x6){
	return(cons(x1,cons(x2,cons(x3,cons(x4,cons(x5,cons(x6,NIL)))))));
}

int list7(int x1, int x2, int x3, int x4, int x5, int x6, int x7){
	return(cons(x1,cons(x2,cons(x3,cons(x4,cons(x5,cons(x6,cons(x7,NIL))))))));
}

int list8(int x1, int x2, int x3, int x4, int x5, int x6, int x7, int x8){
	return(cons(x1,cons(x2,cons(x3,cons(x4,cons(x5,cons(x6,cons(x7,cons(x8,NIL)))))))));
}

int listcopy(int lis){
	if(nullp(lis))
    	return(NIL);
    else
    	return(cons(car(lis),listcopy(cdr(lis))));
}

int reverse(int lis){
	int addr;
    
    addr = NIL;
	while(!(nullp(lis))){
    	addr = cons(car(lis),addr);
        lis = cdr(lis);
    }
	return(addr);
}

int reverse2(int lis){
	int x,addr;
    
    addr = NIL;
	while(!(nullp(lis))){
        x = cdr(lis);
        SET_CDR(lis,addr);
        addr = lis;
        lis = x;
    }
	return(addr);
}

int	last(int lis){
    
    while(!nullp(cdr(lis)))
    	lis = cdr(lis);
    
    return(car(lis));
}

int butlast(int lis){
	int res;
    
    res = NIL;
    while(!nullp(cdr(lis))){
    	res = cons(car(lis),res);
        lis = cdr(lis);
    }
    return(reverse2(res));
}
	

int atomp(int x){
    if(numberp(x) || symbolp(x) || charp(x) || stringp(x) || booleanp(x) || identifierp(x)
                  || IS_SYNCLO(x) || IS_INF(x) || IS_NAN(x))
    	return(1);
    else
    	return(0);
}


int symbolp(int x){
    if(0 <= x && x <= CELLSIZE){
    	if(IS_SYMBOL(x))
    		return(1);
   		else
    		return(0);
    }
    else
    	return(0);
}

//nilを空リストと解釈している。
int listp(int x){
	if(0 <= x && x <= CELLSIZE){	
    	if(IS_LIST(x) && (!(improperp(x))))
    		return(1);
    	else
    	if(IS_NIL(x))
    		return(1);
    	else
    		return(0);
    }
    else
    	return(0);
}

int stringp(int x){
	if(0 <= x && x <= CELLSIZE){
		if(IS_STRING(x))
    		return(1);
    	else
    		return(0);
    }
    else
    	return(0);
}

int charp(int x){
	if(0 <= x && x <= CELLSIZE){
		if(IS_CHARACTER(x))
    		return(1);
    	else
    		return(0);
    	}
    	else
    		return(0);
}

int improperp(int x){
	if(0 <= x && x <= CELLSIZE){
		while(!(nullp(x))){
    		if(atomp(cdr(x)))
        		return(1);
        	x = cdr(x);
    	}
    	return(0);
    }
    else
    	return(0);
}

int pairp(int x){
	if(0 <= x && x <= CELLSIZE){
		if(IS_LIST(x))
    		return(1);
    	else
    		return(0);
    }
    else
    	return(0);
}

int nullp(int x){
	if(0 <= x && x <= CELLSIZE){
		if(IS_NIL(x))
    		return(1);
    	else
    		return(0);
    }
    else
    	return(0);
}

int booleanp(int x){
	if(0 <= x && x <= CELLSIZE){
		if(IS_BOOL(x))
    		return(1);
    	else
    		return(0);
    }
    else
    	return(0);
}

int procedurep(int x){
	if(0 <= x && x <= CELLSIZE){
		if((IS_SUBR(x)) || (IS_CLOSURE(x)) || (IS_CONT(x)))
    		return(1);
    	else
    		return(0);
    }
    else
    	return(0);
}

int multvalp(int x){
	if(0 <= x && x <= CELLSIZE){
		if(IS_MULTVAL(x))
    		return(1);
    	else
    		return(0);
    }
    else
    	return(0);
}

int vectorp(int x){
	if(0 <= x && x <= CELLSIZE){
		if(IS_VECTOR(x))
    		return(1);
    	else
    		return(0);
    }
    else
    	return(0);
}

int bytevectorp(int x){
	if(0 <= x && x <= CELLSIZE){
		if(IS_U8VECTOR(x))
    		return(1);
    	else
    		return(0);
    }
    else
    	return(0);
}

int identifierp(int x){
	if(0 <= x && x <= CELLSIZE){
		if(IS_IDENTIFIER(x))
    		return(1);
    	else
    		return(0);
    }
    else
    	return(0);
}

int syntactic_closurep(int x){
	if(0 <= x && x <= CELLSIZE){
		if(IS_SYNCLO(x))
    		return(1);
    	else
    		return(0);
    }
    else
    	return(0);

}

int equalp(int x1, int x2){
	int i,n;
    
	if(integerp(x1) && integerp(x2)){
     	if(x1 == x2)
    		return(1);
        else
        	return(0);
    }
	if(nullp(x1) && nullp(x2))
    	return(1);
    if((nullp(x1) && !nullp(x2)) || (!nullp(x1) && nullp(x2)))
    	return(0);
	if(numberp(x1) && numberp(x2) && numeqp(x1,x2))
    	return(1);
    if(vectorp(x1) && vectorp(x2)){
    	if(vector_length(x1) != vector_length(x2))
        	return(0);
        n = vector_length(x1);
        for(i=0; i<n; i++)
        	if(!eqvp(GET_VEC_ELT(x1,i),GET_VEC_ELT(x2,i)))
            	return(0);
        return(1);	
    }
    if(bytevectorp(x1) && bytevectorp(x2)){
    	if(vector_length(x1) != vector_length(x2))
        	return(0);
        n = vector_length(x1);
        for(i=0; i<n; i++)
        	if(GET_U8VEC_ELT(x1,i) != GET_U8VEC_ELT(x2,i))
            	return(0);
        return(1);	
    }
	if(atomp(x1) && atomp(x2))
    	return(eqvp(x1,x2));
    if(equalp(car(x1),car(x2)))
    		return(equalp(cdr(x1),cdr(x2)));
    else
    	return(0);
}
    	



int subrp(int x){
	if(0 <= x && x <= CELLSIZE){
		if(IS_SUBR(x))
    		return(1);
    	else
    		return(0);
    }
    else
    	return(0);
}

int closurep(int x){
	if(0 <= x && x <= CELLSIZE){
		if(IS_CLOSURE(x))
    		return(1);
    	else
    		return(0);
    }
    else
    	return(0);
}

int continuationp(int x){
	if(0 <= x && x <= CELLSIZE){
		if(IS_CONT(x))
    		return(1);
    	else
    		return(0);
    }
    else
    	return(0);
}

//マクロの名前かどうか？

int macro_namep(int sym){
	int addr;
    
    if(symbolp(sym) && IS_MACRO(GET_CAR(sym)))
    	return(1);
    else
    if(identifierp(sym)){
    	addr = identifier_to_symbol(sym);
        if(IS_MACRO(GET_CAR(addr)))
        	return(1);
        else
        	return(0);
    }
    else
    	return(0);
}


int hygienic_namep(int sym){
	int addr;
    
    if(symbolp(sym) && IS_HYGIENIC(GET_CAR(sym)))
    	return(1);
    else
    if(identifierp(sym)){
    	addr = identifier_to_symbol(sym);
        if(IS_HYGIENIC(GET_CAR(addr)))
        	return(1);
        else
        	return(0);
    }
    else
    	return(0);
}


int macrop(int x){
	if(0 <= x && x <= CELLSIZE){
		if(IS_MACRO(x))
    		return(1);
    	else
    		return(0);
    }
    else
    	return(0);
}

int setcar(int x, int y){
	SET_CAR(x,y);
    return(x);
}

int setcdr(int x, int y){
	SET_CDR(x,y);
    return(x);
}

int append(int x, int y){
	if(nullp(x))
    	return(y);
    else
    	return(cons(car(x),append(cdr(x),y)));
}

int append2(int x, int y){
	int res;
    
    res = x;
    while(!(nullp(cdr(x))))
    	x = cdr(x);
    
    setcdr(x,y);
    return(res);
}

