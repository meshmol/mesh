//Normal Scheme 


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <complex.h>
#include <limits.h>
#include <setjmp.h>
#include <time.h>
#include <windows.h>
#include <tchar.h>
#include "norm.h"

 

int toupper(int c);
int tolower(int c);
int islower(int c);
int isupper(int c);
int isalpha(int c);
int isdigit(int c);
jmp_buf loadloop;

extern cell memory[];
extern int emem1[];
extern int emem2[];
extern jmp_buf toplevel;
extern clock_t gctime;
extern clock_t start_time;
extern clock_t end_time;
extern FILE *input_port;
extern FILE *output_port;
extern int loadflag;
extern int contflag;
extern token stok;
extern int genint;
extern int compflag;
extern int gbcflag;
extern int asmflag;
extern int profflag;
extern int stepflag;
extern int contflag;
extern int debugflag;
extern int step_on;
extern int prof_on;
extern int head;
extern int tail;
extern int code[];
extern int stack[];
extern int pc;
extern int sp;
extern int env;
extern int n_args;
extern int code_pointer[1000][2];
extern int code_pointer_end;
extern int trace_list;
extern int module_table[MODULESIZE][2];
extern int module_table_end;
extern int current_module;
extern int back_trace[TRACE_DEPTH][2];
extern int back_trace_end;
extern int command_line_count;
extern char *command_line[10];

extern int s_head;
extern int s_tail;
extern int s_code[];
extern int s_stack[];
extern int s_pc;
extern int s_sp;
extern int s_env;
extern int s_n_args;
extern int s_code_pointer[1000][2];
extern int s_code_pointer_end;

//--------リスト-----
int f_car(int n){
	int arg;
    
    arg = pop_s();
    return(scm_car(arg));
}

int f_cdr(int n){
	int arg;
    
	arg = pop_s();
    return(scm_cdr(arg));
}

int f_cons(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
	return(cons(arg1,arg2));
}

int f_caar(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("caar",NOT_PAIR,arg);
    return(scm_car(scm_car(arg)));
}

int f_cdar(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("cdar",NOT_PAIR,arg);
    return(scm_cdr(scm_car(arg)));
}

int f_cddr(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("cddr",NOT_PAIR,arg);
    return(scm_cdr(scm_cdr(arg)));
}

int f_cadr(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("cadr",NOT_PAIR,arg);
    return(scm_car(scm_cdr(arg)));
}


int f_caaar(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("caaar",NOT_PAIR,arg);
    return(scm_car(scm_car(scm_car(arg))));
}

int f_cdaar(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("cdaar",NOT_PAIR,arg);
    return(scm_cdr(scm_car(scm_car(arg))));
}

int f_cadar(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("cadar",NOT_PAIR,arg);
    return(scm_car(scm_cdr(scm_car(arg))));
}

int f_caadr(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("caadr",NOT_PAIR,arg);
    return(scm_car(scm_car(scm_cdr(arg))));
}

int f_cddar(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("cddar",NOT_PAIR,arg);
    return(scm_cdr(scm_cdr(scm_car(arg))));
}

int f_caddr(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("caddr",NOT_PAIR,arg);
    return(scm_car(scm_cdr(scm_cdr(arg))));
}

int f_cdadr(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("cdadr",NOT_PAIR,arg);
    return(scm_cdr(scm_car(scm_cdr(arg))));
}


int f_cdddr(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("cdddr",NOT_PAIR,arg);
    return(scm_cdr(scm_cdr(scm_cdr(arg))));
}

int f_caaaar(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("caaaar",NOT_PAIR,arg);
    return(scm_car(scm_car(scm_car(scm_car(arg)))));
}

int f_cdaaar(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("cdaaar",NOT_PAIR,arg);
    return(scm_cdr(scm_car(scm_car(scm_car(arg)))));
}

int f_cadaar(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("cadaar",NOT_PAIR,arg);
    return(scm_car(scm_cdr(scm_car(scm_car(arg)))));
}

int f_caadar(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("caadar",NOT_PAIR,arg);
    return(scm_car(scm_car(scm_cdr(scm_car(arg)))));
}

int f_caaadr(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("caaadr",NOT_PAIR,arg);
    return(scm_car(scm_car(scm_car(scm_cdr(arg)))));
}

int f_cddaar(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("cddaar",NOT_PAIR,arg);
    return(scm_cdr(scm_cdr(scm_car(scm_car(arg)))));
}

int f_caddar(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("caddar",NOT_PAIR,arg);
    return(scm_car(scm_cdr(scm_cdr(scm_car(arg)))));
}

int f_caaddr(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("caaddr",NOT_PAIR,arg);
    return(scm_car(scm_car(scm_cdr(scm_cdr(arg)))));
}

int f_cdaadr(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("cdaadr",NOT_PAIR,arg);
    return(scm_cdr(scm_car(scm_car(scm_cdr(arg)))));
}

int f_cdadar(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("cdadar",NOT_PAIR,arg);
    return(scm_cdr(scm_car(scm_cdr(scm_car(arg)))));
}

int f_cadadr(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("cadadr",NOT_PAIR,arg);
    return(scm_car(scm_cdr(scm_car(scm_cdr(arg)))));
}

int f_cadddr(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("cadddr",NOT_PAIR,arg);
    return(scm_car(scm_cdr(scm_cdr(scm_cdr(arg)))));
}

int f_cdaddr(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("cdaddr",NOT_PAIR,arg);
    return(scm_cdr(scm_car(scm_cdr(scm_cdr(arg)))));
}

int f_cddadr(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("cddadr",NOT_PAIR,arg);
    return(scm_cdr(scm_cdr(scm_car(scm_cdr(arg)))));
}

int f_cdddar(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("cdddar",NOT_PAIR,arg);
    return(scm_cdr(scm_cdr(scm_cdr(scm_car(arg)))));
}

int f_cddddr(int n){
	int arg;
    
    arg = pop_s();
    if(!pairp(arg))
    	exception("cddddr",NOT_PAIR,arg);
    return(scm_cdr(scm_cdr(scm_cdr(scm_cdr(arg)))));
}

int f_assq(int n){
	int arg1,arg2,res;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!pairp(arg2) && !nullp(arg2))
    	exception("assq",NOT_LIST, arg2);
    res = assq(arg1,arg2);
    return(res);
}

int f_assv(int n){
	int arg1,arg2,res;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!pairp(arg2) && !nullp(arg2))
    	exception("assv",NOT_LIST, arg2);
    res = assv(arg1,arg2);
    return(res);
}

int f_assoc(int n){
	int arg1,arg2,res;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!pairp(arg2) && !nullp(arg2))
    	exception("assoc",NOT_LIST, arg2);
    res = assoc(arg1,arg2);
    return(res);
}


int f_memq(int n){
	int arg1,arg2,res;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!listp(arg2))
    	exception("memq",NOT_LIST, arg2);
    res = memq(arg1,arg2);
    return(res);
}

int f_memv(int n){
	int arg1,arg2,res;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!listp(arg2))
    	exception("memv",NOT_LIST, arg2);
    res = memv(arg1,arg2);
    return(res);
}

int f_member(int n){
	int arg1,arg2,res;
   	
    arg2 = pop_s();
    arg1 = pop_s();
    if(!listp(arg2))
    	exception("member",NOT_LIST, arg2);
    res = member(arg1,arg2);
    return(res);
}

	

int f_reverse(int n){
	int arg;
    
    arg = pop_s();
    if(!listp(arg))
    	exception("reverse",NOT_LIST, arg);
    return(reverse(arg));
}

//破壊的リバース
int f_reverse2(int n){
	int arg;
	
    arg = pop_s();
    if(!listp(arg))
    	exception("reverse!",NOT_LIST, arg);
    return(reverse2(arg));
}

int f_setcar(int n){
	int arg1,arg2;
	
    arg2 = pop_s();
    arg1 = pop_s();
    if(!pairp(arg1))
    	exception("set-car!",NOT_PAIR, arg1);
	return(setcar(arg1,arg2));
}    

int f_setcdr(int n){
	int arg1,arg2;
 	
    arg2 = pop_s();
    arg1 = pop_s();
    if(!pairp(arg1))
    	exception("set-cdr!",NOT_PAIR, arg1);
	return(setcdr(arg1,arg2));
}


int f_append(int n){
	int arg1,arg2;
	
    arg2 = pop_s();
    n--;
    if(!pairp(arg2) && !nullp(arg2))
    	exception("append", NOT_LIST, arg2);

    while(n > 0){
    	arg1 = pop_s();
        n--;
        if(!listp(arg1) && !nullp(arg1))
        	exception("append", NOT_LIST, arg1);
        arg2 = append(arg1,arg2);
    }
    return(arg2);
}

int f_append2(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    return(append2(arg1,arg2));
}

int f_list(int n){
	int res;
	
    res = NIL;
    while(n > 0){
    	res = cons(pop_s(),res);
        n--;
    }
	return(res);
}

int f_makelist(int n){
	int arg1,arg2,m,res;
    
    if(n == 2){
    	arg2 = pop_s();
    	arg1 = pop_s();
    }
    else{
    	arg1 = pop_s();
        arg2 = NIL;
    }
    if(!integerp(arg1))
    	exception("make-list", NOT_INTEGER, arg1);
    m = get_int(arg1);
    res = NIL;
    while(m > 0){
    	res = cons(arg2,res);
        m--;
    }
    return(res);
}	    

int f_length(int n){
	int arg,l;
    
    arg = pop_s();
    if(!(listp(arg)))
    	exception("length", NOT_PAIR, arg);
    l = length(arg);
    return(make_int(l));
}

int f_pair_length(int n){
	int arg,l;
    
    arg = pop_s();
    if(!(pairp(arg)))
    	exception("pair-length", NOT_PAIR, arg);
    l = length(arg);
    return(make_int(l));
}

int f_listtail(int n){
	int arg1,arg2,m;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!IS_INTEGER(arg2) || negativep(arg2))
    	exception("list-ref",NOT_EXACT,arg2);
    m = GET_INT(arg2);
    return(listtail(arg1,m));
}


int f_listref(int n){
	int arg1,arg2,m;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!IS_INTEGER(arg2) || negativep(arg2))
    	exception("list-ref",NOT_EXACT,arg2);
    m = GET_INT(arg2);
    return(listref(arg1,m));
}

int f_listset(int n){
	int arg1,arg2,arg3,i,l,m,before,after,temp;
    
    arg3 = pop_s();
    arg2 = pop_s();
    arg1 = pop_s();
    if(!pairp(arg1))
    	exception("list-set!", NOT_PAIR, arg1);
    if(GET_AUX(arg1) == 1)
    	exception("list-set!", IMMUTABLE_OBJ, arg1);
    if(!IS_INTEGER(arg2) || negativep(arg2))
    	exception("list-set!",NOT_EXACT,arg2);
    m = get_int(arg2);
    l= length(arg1);
    if(m >= l)
    	exception("list-set!",OUT_OF_RANGE, arg2);
    
    if(m == 0)
    	arg1 = cons(arg3,cdr(arg1));
    else{
    	temp = arg1;
    	for(i=1; i<l; i++){
    		before = temp;
        	after = cddr(temp);
        	if(i == m){
            	SET_CDR(before,cons(arg3,after));	
            }
            temp = cdr(temp);	
       } 		
    }
    return(arg1);
}

int f_last(int n){
	int arg;
    
    arg = pop_s();
    return(last(arg));
}

int	f_butlast(int n){
	int arg;
    
    arg = pop_s();
    return(butlast(arg));
}


//----------述語-----------------
int f_nullp(int n){
	int arg;
    
	arg = pop_s();
	if(nullp(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_listp(int n){
	int arg;
    
    arg = pop_s();
    if(listp(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_pairp(int n){
	int arg;
    
    arg = pop_s();
    if(pairp(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_boolp(int n){
	int arg;
    
    arg = pop_s();
    if(!IS_INTEGER(arg) && IS_BOOL(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_symbolp(int n){
	int arg;
    
    arg = pop_s();
    if(symbolp(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_procedurep(int n){
	int arg;
    
    arg = pop_s();
    if(procedurep(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_atomp(int n){
	int arg;
    
    arg = pop_s();
    if(atomp(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_eqp(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(eqp(arg1,arg2))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_eqvp(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(eqvp(arg1,arg2))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_equalp(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    
    if(equalp(arg1,arg2))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_numberp(int n){
	int arg;
    
    arg = pop_s();
    if(numberp(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

//浮動小数点数であっても数学上の整数なら#tを返す。
int f_integerp(int n){
	int arg;
    double x;
    
    arg = pop_s();
    if(integerp(arg) || bignump(arg))
    	return(BOOLT);
    else
    if(floatp(arg)){
    	x = GET_FLT(arg);
        if(ceil(x) == floor(x))
        	return(BOOLT);
        else
        	return(BOOLF);
    } 
    else
    	return(BOOLF);
}

int f_realp(int n){
	int arg;
    
    arg = pop_s();
	if(realp(arg))
    	return(BOOLT);
    else
    	return(BOOLF);	
}

int f_rationalp(int n){
	int arg;
    
    arg = pop_s();
	if(rationalp(arg) || integerp(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

//Simpleでは複素数をこえるものは扱わないのでnumber? == complex?
int f_complexp(int n){
	int arg;
    
    arg = pop_s();
	if(numberp(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int	f_exactp(int n){
	int arg;
    
    arg = pop_s();
    if(exactp(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int	f_inexactp(int n){
	int arg;
    
    arg = pop_s();
    if(inexactp(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_stringp(int n){
	int arg;
    
    arg = pop_s();
    if(stringp(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int	f_characterp(int n){
	int arg;
    
    arg = pop_s();
    if(charp(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_bignump(int n){
	int arg;
    
    arg = pop_s();
    if(!IS_INTEGER(arg) && IS_BIGNUM(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
    
}

int	f_vectorp(int n){
	int arg;
    
    arg = pop_s();
    if(IS_VECTOR(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_macrop(int n){
	int arg;
    
    arg = pop_s();
    if(IS_MACRO(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_macro_namep(int n){
	int arg;
    
    arg = pop_s();
    if(macro_namep(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}



int identifier_to_symbol(int x){
	
    return(make_sym(GET_NAME(x)));
}

int f_hygienicp(int n){
	int arg;
    
    arg = pop_s();
    if(IS_HYGIENIC(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_hygienic_namep(int n){
	int arg;
    
    arg = pop_s();
    if(hygienic_namep(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

//-------ベクタ-----------------
int f_make_vector(int n){
	int arg1,arg2;
    
    if(n == 2){
    	arg2 = pop_s();
    	arg1 = pop_s();
	}
    else{
    	arg1 = pop_s();
    	arg2 = undef;
    }
     if(!IS_INTEGER(arg1) || negativep(arg1))
    	exception("make-vector",NOT_EXACT, arg1);    
    
    return(make_vector(get_int(arg1),arg2));
}

int f_make_bytevector(int n){
	int arg1,arg2;
    unsigned char v;
    
    if(n == 2){
    	arg2 = pop_s();
    	arg1 = pop_s();
        if(!integerp(arg2) && !charp(arg2))
        	exception("make-bytevector", ILLEGAL_ARGUMENT, arg2);
	}
    else{
    	arg1 = pop_s();
    	arg2 = make_int(0);
    }    
    
    if(integerp(arg2))
    	v = (unsigned char)get_int(arg2);
    else
    	v = (unsigned char)GET_CHAR(arg2);
        
    return(make_u8vector(get_int(arg1),v));
}

int f_vector_set(int n){
	int arg1,arg2,arg3;
    
    
    arg3 = pop_s();
    arg2 = pop_s();
    arg1 = pop_s();
    if(!vectorp(arg1))
    	exception("vector-set!",NOT_VECTOR, arg1);
    if(GET_AUX(arg1) == 1)
    	exception("vector-set",IMMUTABLE_OBJ, arg1);
    if(!IS_INTEGER(arg2) || negativep(arg2))
    	exception("vector-set!",NOT_EXACT, arg2);
    
    vector_set(arg1,get_int(arg2),arg3);
    return(undef);
}

int f_bytevector_u8_set(int n){
	int arg1,arg2,arg3;
    unsigned char b;
    
    
    arg3 = pop_s();
    arg2 = pop_s();
    arg1 = pop_s();
    if(!bytevectorp(arg1))
    	exception("bytevector-u8-set",NOT_BYTE_VECTOR, arg1);
	if(GET_AUX(arg1) == 1)
    	exception("bytevector-u8-set",IMMUTABLE_OBJ, arg1);
    if(!IS_INTEGER(arg2) || negativep(arg2))
    	exception("bytevector-u8-set!",NOT_EXACT, arg2);
    if(!integerp(arg3) && !charp(arg3))
    	exception("bytevector-u8-set!",ILLEGAL_ARGUMENT, arg3);
    
    if(integerp(arg3))
    	b = (unsigned char)get_int(arg3);
    else
    	b = (unsigned char)GET_CHAR(arg3);
    
    u8vector_set(arg1,get_int(arg2),b);
    return(undef);
}

int f_vector_ref(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!vectorp(arg1))
    	exception("vector-ref", NOT_VECTOR, arg1);
	if(!IS_INTEGER(arg2) || negativep(arg2))
    	exception("vector-ref",NOT_EXACT, arg2);
    if(get_int(arg2) >= vector_length(arg1))
    	exception("vector-ref", OUT_OF_RANGE,arg2);
    
    return(vector_ref(arg1,get_int(arg2)));
}

int f_bytevector_u8_ref(int n){
	int arg1,arg2,res;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!bytevectorp(arg1))
    	exception("bytevector-u8-ref", NOT_BYTE_VECTOR, arg1);
	if(!IS_INTEGER(arg2) || negativep(arg2))
    	exception("bytevector-u8-ref",NOT_EXACT, arg2);
    if(get_int(arg2) >= vector_length(arg1))
    	exception("bytevector-u8-ref", OUT_OF_RANGE,arg2);
    
    res = make_int((int)u8vector_ref(arg1,get_int(arg2)));
    return(res);
}



int f_vector(int n){
	int i,res;
    
    res = make_vector(n,NIL);
    for(i=n; i>0; i--)
    	vector_set(res,i-1,pop_s());
    
	return(res);
}

int f_bytevector(int n){
	int arg,i,res;
    unsigned char v;
    
    res = make_u8vector(n,0);
    for(i=n; i>0; i--){
    	arg = pop_s();
        if(integerp(arg))
        	v = (unsigned char)get_int(arg);
        else
        	v = (unsigned char)GET_CHAR(arg);
            
        u8vector_set(res,i-1,v);
    }
    return(res);
}

int f_vector_length(int n){
	int arg;
    
    arg = pop_s();
    if(!vectorp(arg))
    	exception("vector-length", NOT_VECTOR, arg);
    return(make_int(vector_length(arg)));
}

int f_bytevector_length(int n){
	int arg;
    
    arg = pop_s();
    if(!bytevectorp(arg))
    	exception("bytevector-length", NOT_BYTE_VECTOR, arg);
    return(make_int(vector_length(arg)));
}

int f_vector_fill(int n){
	int arg1,arg2,len,i;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!vectorp(arg1))
    	exception("vector-fill!", NOT_VECTOR, arg1);
        
    len = vector_length(arg1);
    for(i=0; i<len; i++)
    	vector_set(arg1,i,arg2);
    
    return(undef);
}

int f_vector_to_list(int n){
	int arg,res,i;
    
    arg = pop_s();
    if(!vectorp(arg))
    	exception("vector->list", NOT_VECTOR, arg);
    i = vector_length(arg) - 1;
    res = NIL;
    while(i >= 0){
    	res = cons(vector_ref(arg,i),res);
        i--;
    }
    return(res);   
}

int	f_list_to_vector(int n){
	int arg,res,i;
    
    arg = pop_s();
    if(!listp(arg))
    	exception("list->vector", NOT_LIST, arg);
    res = make_vector(length(arg),undef);
    i = 0;
    while(!nullp(arg)){
    	vector_set(res,i,car(arg));
        arg = cdr(arg);
        i++;
    }
    return(res);
}

int f_bytevector_copy(int n){
	int arg1,arg2,arg3,start,end,i,j,res;
    
    
    if(n == 1){
    	arg1 = pop_s();
    	if(!bytevectorp(arg1))
    		exception("bytevector-copy", NOT_BYTE_VECTOR, arg1);
        start = 0;
        end = vector_length(arg1);
    }
    else if(n == 2){
    	arg2 = pop_s();
        arg1 = pop_s();
        if(!bytevectorp(arg1))
    		exception("bytevector-copy", NOT_BYTE_VECTOR, arg1);
        if(!IS_INTEGER(arg2) || negativep(arg2))
    		exception("bytevector-copy",NOT_EXACT, arg2);
        start = get_int(arg2);
        end = vector_length(arg1);
    }
    else{
    	arg3 = pop_s();
        arg2 = pop_s();
        arg1 = pop_s();
        if(!bytevectorp(arg1))
    		exception("bytevector-copy", NOT_BYTE_VECTOR, arg1);
        if(!IS_INTEGER(arg2) || negativep(arg2))
    		exception("bytevector-copy",NOT_EXACT, arg2);
        if(!IS_INTEGER(arg3) || negativep(arg3))
    		exception("bytevector-copy",NOT_EXACT, arg3);
        start = get_int(arg2);
        end = get_int(arg3);
    }
    
    res = make_u8vector(end-start, 0);
    j = 0;
    for(i=start; i<end; i++){
    	u8vector_set(res,j,u8vector_ref(arg1,i));
        j++;
    }
    return(res);
}

int f_bytevector_copy2(int n){
	int arg1,arg2,arg3,arg4,arg5,at,start,end,i,j,res;
    
    
    if(n == 3){
    	arg3 = pop_s();
        arg2 = pop_s();
    	arg1 = pop_s();
    	if(!bytevectorp(arg1))
    		exception("bytevector-copy!", NOT_BYTE_VECTOR, arg1);
        if(!IS_INTEGER(arg2) || negativep(arg2))
    		exception("bytevector-copy!", NOT_EXACT, arg2);
        if(!bytevectorp(arg3))
    		exception("bytevector-copy!", NOT_BYTE_VECTOR, arg3);
        at = get_int(arg2);
        start = 0;
        end = vector_length(arg3);
    }
    else if(n == 4){
    	arg4 = pop_s();
        arg3 = pop_s();
    	arg2 = pop_s();
        arg1 = pop_s();
        if(!bytevectorp(arg1))
    		exception("bytevector-copy!", NOT_BYTE_VECTOR, arg1);
        if(!IS_INTEGER(arg2) || negativep(arg2))
    		exception("bytevector-copy!",NOT_EXACT, arg2);
        if(!bytevectorp(arg3))
    		exception("bytevector-copy!", NOT_BYTE_VECTOR, arg3);
        if(!IS_INTEGER(arg4) || negativep(arg4))
    		exception("bytevector-copy!",NOT_EXACT, arg4);
        at = get_int(arg2);
        start = get_int(arg3);
        end = vector_length(arg4);
    }
    else{
    	arg5 = pop_s();
        arg4 = pop_s();
    	arg3 = pop_s();
        arg2 = pop_s();
        arg1 = pop_s();
        if(!bytevectorp(arg1))
    		exception("bytevector-copy!", NOT_BYTE_VECTOR, arg1);
        if(!IS_INTEGER(arg2) || negativep(arg2))
    		exception("bytevector-copy!",NOT_EXACT, arg2);
        if(!bytevectorp(arg3))
    		exception("bytevector-copy!", NOT_BYTE_VECTOR, arg3);
        if(!IS_INTEGER(arg4) || negativep(arg4))
    		exception("bytevector-copy!",NOT_EXACT, arg4);
        if(!IS_INTEGER(arg5) || negativep(arg5))
    		exception("bytevector-copy!",NOT_EXACT, arg5);
        at = get_int(arg2);
        start = get_int(arg4);
        end = get_int(arg5);
    }
    
    res = arg1;
    j = at;
    for(i=start; i<end; i++){
    	u8vector_set(res,j,u8vector_ref(arg3,i));
        j++;
    }
    return(res);
}



int f_bytevector_append(int n){
	int arg,args,i,l,m,res;
    
    args = NIL;
    l = 0;
    for(i=0; i<n; i++){
    	arg = pop_s();
		if(!bytevectorp(arg))
        	exception("bytevector-append", NOT_BYTE_VECTOR, arg);
    	args = cons(arg,args);
        l = l + vector_length(arg);
    }
    
    res = make_u8vector(l,0);
    l = 0;
    while(!nullp(args)){
    	arg = car(args);
        m = vector_length(arg);
		args = cdr(args);
        for(i=0; i<m; i++){
        	u8vector_set(res,l,u8vector_ref(arg,i));
       		l++;
        }
    }
    return(res);
}

//-----------文字------------------------------
int f_char_eqp(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!charp(arg1))
    	exception("char=?", NOT_CHAR, arg1);
    if(!charp(arg2))
    	exception("char=?", NOT_CHAR, arg2);
    if(GET_CHAR(arg1) == GET_CHAR(arg2))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_char_ci_eqp(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!charp(arg1))
    	exception("char-ci=?", NOT_CHAR, arg1);
    if(!charp(arg2))
    	exception("char-ci=?", NOT_CHAR, arg2);
    if(toupper(GET_CHAR(arg1)) == toupper(GET_CHAR(arg2)))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_char_greaterp(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!charp(arg1))
    	exception("char>?", NOT_CHAR, arg1);
    if(!charp(arg2))
    	exception("char>?", NOT_CHAR, arg2);
    if(GET_CHAR(arg1) > GET_CHAR(arg2))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_char_ci_greaterp(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!charp(arg1))
    	exception("char-ci>?", NOT_CHAR, arg1);
    if(!charp(arg2))
    	exception("char-ci>?", NOT_CHAR, arg2);
    if(toupper(GET_CHAR(arg1)) > toupper(GET_CHAR(arg2)))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_char_eqgreaterp(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!charp(arg1))
    	exception("char>=?", NOT_CHAR, arg1);
    if(!charp(arg2))
    	exception("char>=?", NOT_CHAR, arg2);
    if(GET_CHAR(arg1) >= GET_CHAR(arg2))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_char_ci_eqgreaterp(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!charp(arg1))
    	exception("char-ci>=?", NOT_CHAR, arg1);
    if(!charp(arg2))
    	exception("char-ci>=?", NOT_CHAR, arg2);
    if(toupper(GET_CHAR(arg1)) >= toupper(GET_CHAR(arg2)))
    	return(BOOLT);
    else
    	return(BOOLF);
}


int f_char_smallerp(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!charp(arg1))
    	exception("char<?", NOT_CHAR, arg1);
    if(!charp(arg2))
    	exception("char<?", NOT_CHAR, arg2);
    if(GET_CHAR(arg1) < GET_CHAR(arg2))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_char_ci_smallerp(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!charp(arg1))
    	exception("char-ci<?", NOT_CHAR, arg1);
    if(!charp(arg2))
    	exception("char-ci<?", NOT_CHAR, arg2);
    if(toupper(GET_CHAR(arg1)) < toupper(GET_CHAR(arg2)))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_char_eqsmallerp(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!charp(arg1))
    	exception("char<=?", NOT_CHAR, arg1);
    if(!charp(arg2))
    	exception("char<=?", NOT_CHAR, arg2);
    if(GET_CHAR(arg1) <= GET_CHAR(arg2))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_char_ci_eqsmallerp(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!charp(arg1))
    	exception("char-ci<=?", NOT_CHAR, arg1);
    if(!charp(arg2))
    	exception("char-ci<=?", NOT_CHAR, arg2);
    if(toupper(GET_CHAR(arg1)) <= toupper(GET_CHAR(arg2)))
    	return(BOOLT);
    else
    	return(BOOLF);
}


int f_char_alphabeticp(int n){
	int arg;
    
    arg = pop_s();
    if(!charp(arg))
    	exception("char-alphabetic?", NOT_CHAR, arg);
    if(charp(arg) && isalpha(GET_CHAR(arg)))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_char_numericp(int n){
	int arg;
    
    arg = pop_s();
    if(!charp(arg))
    	exception("char-numeric?", NOT_CHAR, arg);
    if(charp(arg) && isdigit(GET_CHAR(arg)))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_char_whitespacep(int n){
	int arg;
    
    arg = pop_s();
    if(!charp(arg))
    	exception("char-whitespace?", NOT_CHAR, arg);
    if(charp(arg) && GET_CHAR(arg) == SPACE)
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_char_upper_casep(int n){
	int arg;
    
    arg = pop_s();
    if(!charp(arg))
    	exception("char-upper-case?", NOT_CHAR, arg);
    if(charp(arg) && isupper(GET_CHAR(arg)))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_char_lower_casep(int n){
	int arg;
    
    arg = pop_s();
    if(!charp(arg))
    	exception("char-lower-case?", NOT_CHAR, arg);
    if(charp(arg) && islower(GET_CHAR(arg)))
    	return(BOOLT);
    else
    	return(BOOLF);
}


int f_char_to_integer(int n){
	int arg;
	
    arg = pop_s();
    if(!charp(arg))
    	exception("char->integer", NOT_CHAR, arg);
    return(make_int((int)GET_CHAR(arg)));
}

int f_integer_to_char(int n){
	int arg,res;
    
    arg = pop_s();
    if(!integerp(arg))
    	exception("integer->char", NOT_INTEGER, arg);
    res = make_char(" ");
    SET_CHAR(res,(char)get_int(arg));
    return(res);
}

int f_char_upcase(int n){
	int arg,res;
	
    arg = pop_s();
    if(!charp(arg))
    	exception("char-upcase", NOT_CHAR, arg);
    res = make_char(" ");
	SET_CHAR(res,toupper(GET_CHAR(arg)));
    return(res);
}


int f_char_downcase(int n){
	int arg,res;
	
    arg = pop_s();
    if(!charp(arg))
    	exception("char-downcase", NOT_CHAR, arg);
    res = make_char(" ");
	SET_CHAR(res,tolower(GET_CHAR(arg)));
    return(res);
}

//---------文字列---------------------
int f_string_eqp(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!stringp(arg1))
    	exception("string=?", NOT_STRING, arg1);
    if(!stringp(arg2))
    	exception("string=?", NOT_STRING, arg2);
    if(SAME_NAME(arg1,arg2))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_string_ci_eqp(int n){
	int arg1,arg2,i;
    char str1[SYMSIZE],str2[SYMSIZE];
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!stringp(arg1))
    	exception("string-ci=?", NOT_STRING, arg1);
    if(!stringp(arg2))
    	exception("string-ci=?", NOT_STRING, arg2);
    strcpy(str1,GET_NAME(arg1));
    strcpy(str2,GET_NAME(arg2));
    i = 0;
    while(str1[i] != NUL){
    	str1[i] = toupper(str1[i]);
        i++;
    }
    i = 0;
    while(str2[i] != NUL){
    	str2[i] = toupper(str2[i]);
        i++;
    }
    if(strcmp(str1,str2) == 0)
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_string_greaterp(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!stringp(arg1))
    	exception("string>?", NOT_STRING, arg1);
    if(!stringp(arg2))
    	exception("string>?", NOT_STRING, arg2);
    if(GREATER_NAME(arg1,arg2))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_string_ci_greaterp(int n){
	int arg1,arg2,i;
    char str1[SYMSIZE],str2[SYMSIZE];
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!stringp(arg1))
    	exception("string-ci>?", NOT_STRING, arg1);
    if(!stringp(arg2))
    	exception("string-ci>?", NOT_STRING, arg2);
    strcpy(str1,GET_NAME(arg1));
    strcpy(str2,GET_NAME(arg2));
    i = 0;
    while(str1[i] != NUL){
    	str1[i] = toupper(str1[i]);
        i++;
    }
    i = 0;
    while(str2[i] != NUL){
    	str2[i] = toupper(str2[i]);
        i++;
    }
    if(strcmp(str1,str2) > 0)
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_string_eqgreaterp(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!stringp(arg1))
    	exception("string>=?", NOT_STRING, arg1);
    if(!stringp(arg2))
    	exception("string>=?", NOT_STRING, arg2);
    if(GREATER_NAME(arg1,arg2) || SAME_NAME(arg1,arg2))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_string_ci_eqgreaterp(int n){
	int arg1,arg2,i;
    char str1[SYMSIZE],str2[SYMSIZE];
    
    arg1 = pop_s();
    arg2 = pop_s();
    if(!stringp(arg1))
    	exception("string>=?", NOT_STRING, arg1);
    if(!stringp(arg2))
    	exception("string=?", NOT_STRING, arg2);
    strcpy(str1,GET_NAME(arg1));
    strcpy(str2,GET_NAME(arg2));
    i = 0;
    while(str1[i] != NUL){
    	str1[i] = toupper(str1[i]);
        i++;
    }
    i = 0;
    while(str2[i] != NUL){
    	str2[i] = toupper(str2[i]);
        i++;
    }
    if(strcmp(str1,str2) >= 0)
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_string_smallerp(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!stringp(arg1))
    	exception("string<?", NOT_STRING, arg1);
    if(!stringp(arg2))
    	exception("string=<", NOT_STRING, arg2);
    if(SMALLER_NAME(arg1,arg2))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_string_ci_smallerp(int n){
	int arg1,arg2,i;
    char str1[SYMSIZE],str2[SYMSIZE];
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!stringp(arg1))
    	exception("string-ci<?", NOT_STRING, arg1);
    if(!stringp(arg2))
    	exception("string-ci<?", NOT_STRING, arg2);
    strcpy(str1,GET_NAME(arg1));
    strcpy(str2,GET_NAME(arg2));
    i = 0;
    while(str1[i] != NUL){
    	str1[i] = toupper(str1[i]);
        i++;
    }
    i = 0;
    while(str2[i] != NUL){
    	str2[i] = toupper(str2[i]);
        i++;
    }
    if(strcmp(str1,str2) < 0)
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_string_eqsmallerp(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!stringp(arg1))
    	exception("string<=?", NOT_STRING, arg1);
    if(!stringp(arg2))
    	exception("string<=?", NOT_STRING, arg2);
    if(SMALLER_NAME(arg1,arg2) || SAME_NAME(arg1,arg2))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_string_ci_eqsmallerp(int n){
	int arg1,arg2,i;
    char str1[SYMSIZE],str2[SYMSIZE];
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!stringp(arg1))
    	exception("string-ci<=?", NOT_STRING, arg1);
    if(!stringp(arg2))
    	exception("string-ci<=?", NOT_STRING, arg2);
    strcpy(str1,GET_NAME(arg1));
    strcpy(str2,GET_NAME(arg2));
    i = 0;
    while(str1[i] != NUL){
    	str1[i] = toupper(str1[i]);
        i++;
    }
    i = 0;
    while(str2[i] != NUL){
    	str2[i] = toupper(str2[i]);
        i++;
    }
    if(strcmp(str1,str2) <= 0)
    	return(BOOLT);
    else
    	return(BOOLF);
}


int	f_string_append(int n){
	int arg1,arg2;
    char str1[SYMSIZE],str2[SYMSIZE];
    
    arg2 = pop_s();
    n--;
    if(nullp(arg2))
    	return(make_str(""));
    if(!stringp(arg2))
    	exception("string-append", NOT_STRING, arg2);
    strcpy(str2,GET_NAME(arg2));
    while(n != 0){
    	arg1 = pop_s();
        n--;
        if(!stringp(arg1))
        	exception("string-append", NOT_STRING, arg1);
    	strcpy(str1,GET_NAME(arg1));
    	strcat(str1,str2);
        strcpy(str2,str1);
    }
    return(make_str(str1));
}

//以下まだ直していない。
int f_number_to_string(int n){
	int arg1,arg2,d,x;
    char str1[SYMSIZE],str2[2],str3[SYMSIZE],str4[2];
    
    if(n == 2){
    	arg2 = pop_s();
    	arg1 = pop_s();
    }
    else{
    	arg2 = NIL;
        arg1 = pop_s();
    }
    
    if(!numberp(arg1))
    	exception("number->string", NOT_NUMBER, arg1);
    if(!nullp(arg2) && !integerp(arg2))
    	exception("number->string", NOT_INTEGER, arg2);
    if(nullp(arg2))
    	d = 10;
    else
    	d = get_int(arg2);
    
    if(integerp(arg1))
    	itoa(get_int(arg1),str1,d);
    
    //不完全、１０進数のみ。
    if(bignump(arg1)){
    	x = reverse(arg1);
        str1[0] = NUL;
        itoa(get_int(car(x)),str3,10);
        strcat(str1,str3);
        x = cdr(x);
        while(!nullp(x)){
        	sprintf(str3,"%09d",get_int(car(x)));
            strcat(str1,str3);
            x = cdr(x); 
        }
        return(make_str(str1));
    }
    
    if(floatp(arg1))
    	sprintf(str1,"%0.16g",GET_FLT(arg1));
    
    if(rationalp(arg1)){
    	itoa(GET_CAR(arg1),str1,d);
        itoa(GET_CDR(arg1),str3,d);
        str2[0] = '/'; str2[1] = NUL;
        strcat(str1,str2);
        strcat(str1,str3);
    }
    
    if(complexp(arg1)){
    	sprintf(str1,"%f",GET_FLT(arg1));
        sprintf(str3,"%f",GET_IMAG_FLT(arg1));
        str4[0] = 'i'; str4[1] = NUL;
    	if(GET_IMAG_FLT(arg1) < 0){
        	strcat(str1,str3);
        	strcat(str1,str4);
        }
        else{
        	str2[0] = '+'; str2[1] = NUL;
            strcat(str1,str2);
            strcat(str1,str3);
            strcat(str1,str4);
        }
    }   	
    return(make_str(str1));
}

int f_string_to_number(int n){
	int arg1,arg2,d;
    char *e;
    
    if(n == 2){
    	arg2 = pop_s();
    	arg1 = pop_s();
    }
    else{
    	arg2 = NIL;
        arg1 = pop_s();
    }
    if(!stringp(arg1))
    	exception("string->number", NOT_STRING, arg1);
    if(!nullp(arg2) && !integerp(arg2))
    	exception("string->number", NOT_INTEGER, arg2);
	if(nullp(arg2))
    	d = 10;
    else
    	d = get_int(arg2);
    
    strcpy(stok.buf,GET_NAME(arg1));
    
	if(strcmp(stok.buf,"") == 0)
    	return(BOOLF);
        
    if(bignumtoken(stok.buf)){
    	if(d == 10)
			return(make_big(stok.buf));
    	else
        	return(BOOLF);
    }
    
    if(inttoken(stok.buf))
    	return(make_int(strtol(stok.buf,&e,d)));
    
    if(flttoken(stok.buf)){
    	if(d == 10)
    		return(make_flt(atof(stok.buf)));
    	else
        	return(BOOLF);
    }
    
    if(rattoken(stok.buf))
    	return(make_rat(strtol(stok.before,&e,d), strtol(stok.after,&e,d)));
            
    if(comptoken(stok.buf)){
    	if(d == 10)
			return(make_comp(atof(stok.before), atof(stok.after)));
        else
        	return(BOOLF);
	}
    
    return(BOOLF);
}

int f_string_to_symbol(int n){
	int arg,res;
    
    arg = pop_s();
    if(!stringp(arg))
    	exception("string->symbol", NOT_STRING, arg);
    
    res = make_sym(GET_NAME(arg));
    return(res);
}

int f_symbol_to_string(int n){
	int arg,res;
    
    arg = pop_s();
    if(!symbolp(arg))
    	exception("symbol->string", NOT_SYMBOL, arg);
    
    res = make_str(GET_NAME(arg));
    return(res);
}

int f_string_length(int n){
	int arg;
    
    arg = pop_s();
    if(!stringp(arg))
    	exception("string-length", NOT_STRING, arg);
    
    return(make_int(strlen(GET_NAME(arg))));
}

int f_make_string(int n){
	int arg1,arg2,i,j;
    char c;
    char str[SYMSIZE];
    
    if(n != 1 && n !=2)
    	exception("make-string", INCORRECT_ARG_CNT, NIL);
    if(n == 2){
    	arg2 = pop_s();
    	arg1 = pop_s();
    }
    else{
    	arg2 = NIL;
        arg1 = pop_s();
    }
    
    if(!integerp(arg1))
    	exception("make-string", NOT_INTEGER, arg1);
    if(!charp(arg2) && !nullp(arg2))
    	exception("make-string", NOT_CHAR, arg2);
 	
    i = get_int(arg1);
    
    if(nullp(arg2))
    	c = SPACE;
    else
    	c = GET_CHAR(arg2);
    
    str[0] = NUL;
    for(j=0; j<i; j++)
    	str[j] = c;
    str[i] = NUL;
    return(make_str(str));	
}

int f_string(int n){
	int arg;
    char str[SYMSIZE];
    
    str[n] = NUL;
    while(n != 0){
    	arg = pop_s();
        n--;
        if(!charp(arg))
        	exception("string", NOT_CHAR, arg);
        str[n] = GET_CHAR(arg);
    }
    return(make_str(str));
}

int	f_string_ref(int n){
	int arg1,arg2,i,res;
    char c;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!stringp(arg1))
    	exception("string-ref", NOT_STRING, arg1);
    if(!integerp(arg2))
    	exception("string-ref", NOT_INTEGER, arg2);
    
    i = get_int(arg2);
    res = make_char(" ");
    c = STRING_REF(arg1,i);
    SET_CHAR(res,c);
    return(res);
}

int f_string_set(int n){
	int arg1,arg2,arg3,i;
    char c;
    
    arg3 = pop_s();
    arg2 = pop_s();
    arg1 = pop_s();
    if(!stringp(arg1))
    	exception("string-set", NOT_STRING, arg1);
    if(GET_AUX(arg1) == 1)
    	exception("string-set",IMMUTABLE_OBJ, arg1);
    if(!integerp(arg2))
    	exception("string-set", NOT_INTEGER, arg2);
    if(!charp(arg3))
    	exception("string-set", NOT_CHAR, arg2);
    
    i = get_int(arg2);
    c = GET_CHAR(arg3);
    STRING_SET(arg1,i,c);
    return(undef);
}

int f_substring(int n){
	int arg1,arg2,arg3,i,j,start,end,res;
    
    arg3 = pop_s();
    arg2 = pop_s();
    arg1 = pop_s();
	if(!stringp(arg1))
    	exception("substring", NOT_STRING, arg1);
    if(!integerp(arg2))
    	exception("substring", NOT_INTEGER, arg2);
    if(!integerp(arg3))
    	exception("substring", NOT_INTEGER, arg3);
    start = get_int(arg2);
    end   = get_int(arg3);
    if(end > strlen(GET_NAME(arg1)))
    	exception("substring",ILLEGAL_ARGUMENT, arg3);
    if(start < 0)
    	exception("substring",ILLEGAL_ARGUMENT, arg1);
    if(start > end)
    	exception("substring",ILLEGAL_ARGUMENT, arg2);
    i = 0;
    res = make_str("");
    for(j = start; j < end; j++){
    	STRING_SET(res,i,STRING_REF(arg1,j));
        i++;
    }
    STRING_SET(res,i,NUL);
    return(res);
}

int f_string_to_list(int n){
	int arg,i,chr,res;
    char c;
    
    arg = pop_s();
    if(!stringp(arg))
    	exception("string->list", NOT_STRING, arg);
    
    res = NIL;
    i = strlen(GET_NAME(arg)) - 1;
    while(i >= 0){
		c = STRING_REF(arg,i);
    	chr = make_char(" ");
        SET_CHAR(chr,c);
        res = cons(chr,res);
        i--;    
    }
    return(res);	
}

int f_list_to_string(int n){
	int arg,i,chr,res;
    char c;
    
	arg = pop_s();
    res = make_str("");
    i = 0;
    while(!nullp(arg)){
    	chr = car(arg);
        if(!charp(chr))
        	exception("list->string", NOT_CHAR, chr);
        c = GET_CHAR(chr);
        STRING_SET(res,i,c);
        i++;
        arg = cdr(arg);
    }
	STRING_SET(res,i,NUL);
    return(res);    
}

int f_string_copy(int n){
	int arg,res;
    
    arg = pop_s();
    if(!stringp(arg))
    	exception("string-copy", NOT_STRING, arg);
    res = make_str(GET_NAME(arg));
    return(res);
}

int f_string_fill(int n){
	int arg1,arg2,i;
    char c;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!stringp(arg1))
    	exception("string-fill", NOT_STRING, arg1);
    if(!charp(arg2))
    	exception("string-fill", NOT_CHAR, arg2);
    c = GET_CHAR(arg2);
    i = strlen(GET_NAME(arg1)) - 1;
    while(i >= 0){
    	STRING_SET(arg1,i,c);
        i--;
    }
    return(undef);
}
//---------数値計算--------------------
int f_plus(int n){
	int arg,res,i=0;
    
    if(n == 2 && integerp(stack[sp-1]) && integerp(stack[sp-2])){
    	arg = pop_s();
        res = pop_s();

    	i = GET_INT(arg) + GET_INT(res);
        if(SMALL_INT_MIN < i && i < SMALL_INT_MAX)
        	return(make_int(i));
        else
        	return(big_plus(inttobignum(arg),inttobignum(res)));
    }
    
    if(n == 0)
    	return(make_int(0));
    res = pop_s();
    n--;
    if(!numberp(res))
    	exception("+", NOT_NUMBER, res);
    
    while(n > 0){
    	arg = pop_s();
        n--;
        if(!numberp(arg))
    		exception("+", NOT_NUMBER, arg);
        
        res = plus(arg,res);
    }
	return(res);   
}

int f_minus(int n){
	int arg,res;
    
    if(n == 2 && integerp(stack[sp-1]) && integerp(stack[sp-2])){
    	arg = pop_s();
        res = pop_s();
        return(minus(res,arg));
    }
    
    res = pop_s();
    n--;
    if(!numberp(res))
    	exception("-", NOT_NUMBER, res);
        
    if(n == 0)
    	return(mult(res,make_int(-1)));
    while(n > 1){
    	arg = pop_s();
        n--;
        if(!numberp(arg))
    		exception("-", NOT_NUMBER, arg);
        res = plus(arg,res);
    }
    arg = pop_s();
    if(!numberp(arg))
    	exception("-", NOT_NUMBER, arg);
    res = minus(arg,res);
	return(res);   
}

int f_mult(int n){
	int arg,res;
    
	if(n == 0)
    	return(make_int(1));
    
    res = pop_s();
    n--;
    
    if(!numberp(res))
    	exception("*", NOT_NUMBER, res);
        
    while(n > 0){
    	arg = pop_s();
        n--;
        if(!numberp(arg))
    		exception("*", NOT_NUMBER, arg);
        res = mult(arg,res);
    }
	return(res);   
}

int f_div(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    n--;
    if(!numberp(arg2))
    	exception("/", NOT_NUMBER, arg2);

	if(n == 0){
		if(integerp(arg2) && zerop(arg2))
        	exception("/", DIVIDE_ZERO, arg2);
        else
    		return(divide(make_int(1),arg2));
    }
    if(n == 1){
    	if(integerp(arg2) && zerop(arg2))
        	exception("/", DIVIDE_ZERO, arg2);
        else
    		return(divide(pop_s(),arg2));
    
    }
        
    while(n > 1){
    	arg1 = pop_s();
        n--;	
        if(!numberp(arg1))
    		exception("/", NOT_NUMBER, arg1);
        if(integerp(arg1) && zerop(arg1))
        	exception("/", DIVIDE_ZERO, arg1);
        
        arg2 = mult(arg1,arg2);
    }
    arg1 = pop_s();
	return(divide(arg1,arg2));   
}

int f_smallerp(int n){
	int arg1,arg2;
    
    if(n == 2 && integerp(stack[sp-1]) && integerp(stack[sp-1])){
    	arg2 = pop_s();
        arg1 = pop_s();
        if(arg1 < arg2)
        	return(BOOLT);
        else
        	return(BOOLF);	
    }
    arg2 = pop_s();
    n--;
    if(!numberp(arg2))
    	exception("<", NOT_NUMBER, arg2);

    while(n > 0){
    	arg1 = pop_s();
        n--;
        if(!numberp(arg1))
        	exception("<", NOT_NUMBER, arg1);
    	if(!smallerp(arg1,arg2))
    		return(BOOLF);
        arg2 = arg1;
    	}
    return(BOOLT);
}


int f_eqsmallerp(int n){
	int arg1,arg2;
    
    if(n == 2 && integerp(stack[sp-1]) && integerp(stack[sp-1])){
    	arg2 = pop_s();
        arg1 = pop_s();
        if(arg1 <= arg2)
        	return(BOOLT);
        else
        	return(BOOLF);	
    }
    arg2 = pop_s();
	n--;
    if(!numberp(arg2))
    	exception("<=", NOT_NUMBER, arg2);
    
    while(n > 0){
    	arg1 = pop_s();
        n--;
        if(!numberp(arg1))
        	exception("<=", NOT_NUMBER, arg1);
    	if(!eqsmallerp(arg1,arg2))
    		return(BOOLF);
        arg2 = arg1;
    	}
    return(BOOLT);
}

int f_greaterp(int n){
	int arg1,arg2;
    
    if(n == 2 && integerp(stack[sp-1]) && integerp(stack[sp-1])){
    	arg2 = pop_s();
        arg1 = pop_s();
        if(arg1 > arg2)
        	return(BOOLT);
        else
        	return(BOOLF);	
    }
    arg2 = pop_s();
	n--;
    if(!numberp(arg2))
    	exception(">", NOT_NUMBER, arg2);
    
    while(n > 0){
    	arg1 = pop_s();
        n--;
        if(!numberp(arg1))
        	exception(">", NOT_NUMBER, arg1);
    	if(!greaterp(arg1,arg2))
    		return(BOOLF);
        arg2 = arg1;
    	}
    return(BOOLT);
}

int f_eqgreaterp(int n){
	int arg1,arg2;
    
    if(n == 2 && integerp(stack[sp-1]) && integerp(stack[sp-1])){
    	arg2 = pop_s();
        arg1 = pop_s();
        if(arg1 >= arg2)
        	return(BOOLT);
        else
        	return(BOOLF);	
    }
    arg2 = pop_s();
    n--;
    if(!numberp(arg2))
    	exception(">=", NOT_NUMBER, arg2);
    
    while(n > 0){
    	arg1 = pop_s();
        n--;
        if(!numberp(arg1))
        	exception(">=", NOT_NUMBER, arg1);
    	if(!eqgreaterp(arg1,arg2))
    		return(BOOLF);
        arg2 = arg1;
    	}
    return(BOOLT);
}

int f_numeqp(int n){
	int arg1,arg2;
    
    if(n == 2 && integerp(stack[sp-1]) && integerp(stack[sp-1])){
    	arg2 = pop_s();
        arg1 = pop_s();
        if(arg1 == arg2)
        	return(BOOLT);
        else
        	return(BOOLF);	
    }
    arg2 = pop_s();
    n--;
    if(!(numberp(arg2)))
    	exception("=", NOT_NUMBER, arg2);
    
    while(n > 0){
    	arg1 = pop_s();
        n--;
        if(!numberp(arg1))
        	exception("=", NOT_NUMBER, arg1);
        if(!(numeqp(arg1,arg2)))
        	return(BOOLF);
        
        arg2 = arg1;
    }
    return(BOOLT);
}

int f_oddp(int n){
	int arg,x;
    
    arg = pop_s();
    if(!numberp(arg))
    	exception("odd?", NOT_NUMBER, arg);
    
    if(integerp(arg)){
    	x = abs(GET_INT(arg));
        if(x % 2 == 1)
        	return(BOOLT);
        else	
        	return(BOOLF);
    }
    if(bignump(arg)){
    	arg = car(arg);
    	x = GET_INT(arg);
        if(x % 2 == 1)
        	return(BOOLT);
        else
        	return(BOOLF);
    }
    else
    	return(BOOLF);
}

int f_evenp(int n){
	int arg,x;
    
    arg = pop_s();
    if(!numberp(arg))
    	exception("even?", NOT_NUMBER, arg);
    
    if(integerp(arg)){
    	x = abs(GET_INT(arg));
        if(x % 2 == 0)
        	return(BOOLT);
        else	
        	return(BOOLF);
    }
    if(bignump(arg)){
    	arg = car(arg);
    	x = GET_INT(arg);
        if(x % 2 == 0)
        	return(BOOLT);
        else
        	return(BOOLF);
    }
    else
    	return(BOOLF);
}

int f_positivep(int n){
	int arg;
    
    arg = pop_s();
    if(!realp(arg))
    	exception("positive?", NOT_REAL, arg);
    
    if(positivep(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_negativep(int n){
	int arg;
    
    arg = pop_s();
    if(complexp(arg))
    	exception("negative?", NOT_REAL, arg);
    
   	if(negativep(arg))
   		return(BOOLT);
    else
    	return(BOOLF);
}

int f_abs(int n){
	int arg;
    
    arg = pop_s();
    return(s_abs(arg));
}

int f_max(int n){
	int arg1,arg2;
    
    if(n == 0)
    	return(NIL);
    if(n == 1)
    	return(pop_s());
    
    arg2 = pop_s();
    n--;
    while(n > 0){
    	arg1 = pop_s();
        n--;
        if(greaterp(arg1,arg2))
        	arg2 = arg1;
    }
    return(arg2);  
}

int f_min(int n){
	int arg1,arg2;
    
    if(n == 0)
    	return(NIL);
    if(n == 1)
    	return(pop_s());
    
    arg2 = pop_s();
    n--;
    while(n > 0){
    	arg1 = pop_s();
        n--;
        if(smallerp(arg1,arg2))
        	arg2 = arg1;
    }
    return(arg2);  
}

int	f_remainder(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    
    if(!mathematical_integerp(arg1))
        exception("remainder",NOT_INTEGER,arg1);

    if(!mathematical_integerp(arg2))
        exception("remainder",NOT_INTEGER,arg2);
    
    if(exactp(arg1) && exactp(arg2))
    	return(s_remainder(arg1,arg2));
    else{
    	arg1 = inexact_to_exact(arg1);
    	arg2 = inexact_to_exact(arg2);
    	return(exact_to_inexact(s_remainder(arg1,arg2)));
	}
}

int f_modulo(int n){
	int arg1,arg2,res;
    
    arg2 = pop_s();
    arg1 = pop_s();
    
    if(!mathematical_integerp(arg1))
        exception("modulo",NOT_INTEGER,arg1);

    if(!mathematical_integerp(arg2))
        exception("modulo",NOT_INTEGER,arg2);
    
    
    if(exactp(arg1) && exactp(arg2)){
    	arg1 = inexact_to_exact(arg1);
   		arg2 = inexact_to_exact(arg2);
    }
    
    if(positivep(arg1) && positivep(arg2))
		res = s_remainder(arg1,arg2);	
    else if(positivep(arg1) && negativep(arg2))
        res = plus(s_remainder(arg1,s_abs(arg2)),arg2);
    else if(negativep(arg1) && positivep(arg2))
    	res = minus(arg2,s_remainder(s_abs(arg1),arg2));
    else
    	res = mult(s_remainder(s_abs(arg1),s_abs(arg2)),make_int(-1));
        
    if(exactp(arg1) && exactp(arg2))
    	return(res);
    else
    	return(exact_to_inexact(res));
}

int	f_quotient(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(zerop(arg2))
    	exception("quotient", DIVIDE_ZERO, NIL);
    
	if(exactp(arg1) && exactp(arg2))
    	return(quotient(arg1,arg2));
    else{
    	arg1 = inexact_to_exact(arg1);
        arg2 = inexact_to_exact(arg2);
        return(exact_to_inexact(quotient(arg1,arg2)));
    }
}

int f_gcd(int n){
	int arg1,arg2;
    
	if(n == 0)
    	return(make_int(0));
    
    arg2 = pop_s();
    if(!mathematical_integerp(arg2))
        exception("gcd",NOT_INTEGER,arg2);
    if(n == 1)
    	return(arg2);
	
    arg1 = pop_s();
    if(!mathematical_integerp(arg1))
        exception("gcd",NOT_INTEGER,arg1);
    
    if(exactp(arg1) && exactp(arg2))
    	return(gcd(arg1,arg2));
    else{
        arg1 = inexact_to_exact(arg1);
    	arg2 = inexact_to_exact(arg2);
    	return(exact_to_inexact(gcd(arg1,arg2)));
	}
}

int f_lcm(int n){
	int arg1,arg2;
    
    if(n == 0)
    	return(make_int(1));
        
    arg2 = pop_s();
    if(!mathematical_integerp(arg2))
        exception("lcm",NOT_INTEGER,arg2);
    if(n == 1)
    	return(arg2);
	
    arg1 = pop_s();
    if(!mathematical_integerp(arg1))
        exception("lcm",NOT_INTEGER,arg1);
    
    if(exactp(arg1) && exactp(arg2))
    	return(lcm(arg1,arg2));
    else{
    	arg1 = inexact_to_exact(arg1);
    	arg2 = inexact_to_exact(arg2);
    	return(exact_to_inexact(lcm(arg1,arg2)));
    }
}

int f_floor(int n){
	int arg,arg1;
    
    arg = pop_s();
    if(complexp(arg) || !numberp(arg))
    	exception("floor", NOT_REAL, arg);
        
    if(integerp(arg))
    	return(arg);
    if(bignump(arg))
    	return(arg);
    arg1 = exact_to_inexact(arg);
    if(rationalp(arg))
    	return(make_int((int)floor(GET_FLT(arg1))));
    if(floatp(arg))
    	return(make_flt(floor(GET_FLT(arg1))));
    return(undef);
}

int f_ceiling(int n){
	int arg,arg1;
    
    arg = pop_s();
    if(complexp(arg) || !numberp(arg))
    	exception("ceiling", NOT_REAL, arg);
        
    if(integerp(arg))
    	return(arg);
    if(bignump(arg))
    	return(arg);
    arg1 = exact_to_inexact(arg);
    if(rationalp(arg))
    	return(make_int((int)ceil(GET_FLT(arg1))));
    if(floatp(arg))
    	return(make_flt(ceil(GET_FLT(arg1))));
    return(undef);
}

int f_truncate(int n){
	int arg,arg1;
    double x;
    
    arg = pop_s();
    if(complexp(arg) || !numberp(arg))
    	exception("truncate", NOT_REAL, arg);
    
    if(integerp(arg))
    	return(arg);
    if(bignump(arg))
    	return(arg);
    arg1 = exact_to_inexact(arg);
    x = GET_FLT(arg1);
    if(rationalp(arg)){
    	if(x >= 0)
    		return(make_int((int)floor(x)));
    	else
    		return(make_int((int)ceil(x)));
    }
    if(floatp(arg)){
    	if(x >= 0)
    		return(make_flt(floor(x)));
    	else
    		return(make_flt(ceil(x)));
    }
    return(undef);
}


int f_round(int lvar){
	int arg,arg1;
    
    arg = pop_s();
    if(complexp(arg) || !numberp(arg))
    	exception("round", NOT_REAL, arg);
    
    if(integerp(arg))
    	return(arg);
    if(bignump(arg))
    	return(arg);
    arg1 = exact_to_inexact(arg);
	if(rationalp(arg))
    	return(make_int((int)round(GET_FLT(arg1))));
    if(floatp(arg))
    	return(make_flt(round(GET_FLT(arg1))));
    return(undef);
}

int f_numerator(int n){
	int arg;
    
    arg = pop_s();
    if(!realp(arg))
    	exception("numerator", NOT_REAL, arg);
    
    if(floatp(arg)){
    	arg = flttorat(arg);
    	return(make_flt((double)GET_CAR(arg)));
    }
    return(make_int(GET_CAR(arg)));
}

int f_denominator(int n){
	int arg;
    
    arg = pop_s();
    if(!realp(arg))
    	exception("denominator", NOT_REAL, arg);
    if(integerp(arg))
    	return(make_int(1));
    
    if(floatp(arg)){
    	arg = flttorat(arg);
		return(make_flt((double)GET_CDR(arg)));
    }
    return(make_int(GET_CDR(arg)));
}

int f_sin(int n){
	int arg;
    double x,x1,y,y1;
	double complex z,z1;
    
    arg = pop_s();
    
	if(!numberp(arg))
    	exception("sin", NOT_NUMBER, arg);
    
    if(realp(arg))
    	return(make_flt(sin(GET_FLT(exact_to_inexact(arg)))));
    if(complexp(arg)){
    	x = GET_REAL_FLT(arg);
        y = GET_IMAG_FLT(arg);
        z = x+y*I;
        z1 = csin(z);
        x1 = creal(z1);
        y1 = cimag(z1);
        return(make_comp(x1,y1));
    }
    return(undef);
}

int f_asin(int n){
	int arg;
    double x,x1,y,y1;
	double complex z,z1;
    
    arg = pop_s();
    
    if(!numberp(arg))
    	exception("asin", NOT_NUMBER, arg);
    
    if(realp(arg))
    	return(make_flt(asin(GET_FLT(exact_to_inexact(arg)))));
    if(complexp(arg)){
    	x = GET_REAL_FLT(arg);
        y = GET_IMAG_FLT(arg);
        z = x+y*I;
        z1 = casin(z);
        x1 = creal(z1);
        y1 = cimag(z1);
        return(make_comp(x1,y1));
    }
    return(undef);
}


int f_cos(int n){
	int arg;
    double x,x1,y,y1;
	double complex z,z1;
    
    arg = pop_s();
    
    if(!numberp(arg))
    	exception("cos", NOT_NUMBER, arg);
    
    if(realp(arg))
    	return(make_flt(cos(GET_FLT(exact_to_inexact(arg)))));
    if(complexp(arg)){
    	x = GET_REAL_FLT(arg);
        y = GET_IMAG_FLT(arg);
        z = x+y*I;
        z1 = ccos(z);
        x1 = creal(z1);
        y1 = cimag(z1);
        return(make_comp(x1,y1));
    }
    return(undef);
}

int f_acos(int n){
	int arg;
    double x,x1,y,y1;
	double complex z,z1;
    
    arg = pop_s();
    if(!numberp(arg))
    	exception("acos", NOT_NUMBER, arg);
    
    if(realp(arg))
    	return(make_flt(acos(GET_FLT(exact_to_inexact(arg)))));
    if(complexp(arg)){
    	x = GET_REAL_FLT(arg);
        y = GET_IMAG_FLT(arg);
        z = x+y*I;
        z1 = cacos(z);
        x1 = creal(z1);
        y1 = cimag(z1);
        return(make_comp(x1,y1));
    }
    return(undef);
}

int	f_tan(int n){
	int arg;
    double x,x1,y,y1;
	double complex z,z1;
    
    arg = pop_s();
    if(!numberp(arg))
    	exception("tan", NOT_NUMBER, arg);
    
    if(realp(arg))
    	return(make_flt(tan(GET_FLT(exact_to_inexact(arg)))));
    if(complexp(arg)){
    	x = GET_REAL_FLT(arg);
        y = GET_IMAG_FLT(arg);
        z = x+y*I;
        z1 = ctan(z);
        x1 = creal(z1);
        y1 = cimag(z1);
        return(make_comp(x1,y1));
    }
    return(undef);
}

double argz(double x, double y){
	double pi = 3.141592653589793;
    
	if(x == 0 && y == 0)
    	return(0);
	if(x > 0)
    	return(atan(y/x));
    if(x == 0 && y > 0)
    	return(pi/2);
    if(x == 0 && y < 0)
    	return(-pi/2);
    if(x < 0)
    	return(atan(y/x)+pi);
    return(0);
}

int f_atan(int n){
	int arg1,arg2;
    double x,x1,y,y1;
	double complex z,z1;
    
    if(n == 1){
		arg1 = pop_s();
        if(!numberp(arg1))
    		exception("atan", NOT_NUMBER, arg1);
    	
        if(realp(arg1))
    		return(make_flt(atan(GET_FLT(exact_to_inexact(arg1)))));
    	if(complexp(arg1)){
    		x = GET_REAL_FLT(arg1);
        	y = GET_IMAG_FLT(arg1);
        	z = x+y*I;
        	z1 = catan(z);
        	x1 = creal(z1);
        	y1 = cimag(z1);
        	return(make_comp(x1,y1));
    	}
    }
    else{
    	arg2 = pop_s();
        arg1 = pop_s();
    	y = GET_FLT(exact_to_inexact(arg1));
        x = GET_FLT(exact_to_inexact(arg2));
        return(make_flt(argz(x,y)));
    }
    return(undef);   
}


int f_log(int n){
	int arg;
    double x,x1,y,y1;
	double complex z,z1;
    
    arg = pop_s();
    if(!numberp(arg))
    	exception("log", NOT_NUMBER, arg);
    
    if(realp(arg) && GET_FLT(exact_to_inexact(arg)) > 0)
    	return(make_flt(log(GET_FLT(exact_to_inexact(arg)))));
    else{
		if(realp(arg))
        	arg = realtocomp(arg);
    	x = GET_REAL_FLT(arg);
        y = GET_IMAG_FLT(arg);
        z = x+y*I;
        z1 = clog(z);
        x1 = creal(z1);
        y1 = cimag(z1);
        return(make_comp(x1,y1));
    }
    return(undef);
}


int f_exp(int n){
	int arg;
    double x,x1,y,y1;
	double complex z,z1;
    
    arg = pop_s();
    if(!numberp(arg))
    	exception("exp", NOT_NUMBER, arg);
    
    if(realp(arg))
    	return(make_flt(exp(GET_FLT(exact_to_inexact(arg)))));
    if(complexp(arg)){
    	x = GET_REAL_FLT(arg);
        y = GET_IMAG_FLT(arg);
        z = x+y*I;
        z1 = cexp(z);
        x1 = creal(z1);
        y1 = cimag(z1);
        return(make_comp(x1,y1));
    }
    return(undef);
}	

int f_sqrt(int n){
	int arg;
    double x,x1,y,y1;
	double complex z,z1;
    
    arg = pop_s();
    
    if(!numberp(arg))
    	exception("sqrt", NOT_NUMBER, arg);
    
    if(realp(arg) && !negativep(arg))
    	return(make_flt(sqrt(GET_FLT(exact_to_inexact(arg)))));
    if(realp(arg) && negativep(arg)){
    	x = GET_FLT(exact_to_inexact(arg));
        y = 0;
        z = x+y*I;
        z1 = csqrt(z);
        x1 = creal(z1);
        y1 = cimag(z1);
        return(make_comp(x1,y1));
    }	
    if(complexp(arg)){
    	x = GET_REAL_FLT(arg);
        y = GET_IMAG_FLT(arg);
        z = x+y*I;
        z1 = csqrt(z);
        x1 = creal(z1);
        y1 = cimag(z1);
        return(make_comp(x1,y1));
    }
    return(undef);
}

int f_expt(int n){
	int arg1,arg2;
    double x,x1,x2,y,y1,y2;
	double complex z,z1,z2;
	
    
    arg2 = pop_s();
    arg1 = pop_s();
    
    if(!numberp(arg1))
    	exception("expt", NOT_NUMBER, arg1);
    
	if(numberp(arg2) && bignump(arg2))
    	exception("expt", TOO_BIG, arg2);
    
	if((integerp(arg1) || bignump(arg1)) && integerp(arg2) && get_int(arg2) == 0)
    	return(make_int(1));
    
    if((integerp(arg1) || bignump(arg1)) && integerp(arg2) && get_int(arg2) > 0)
    	return(expt(arg1,get_int(arg2)));
    
    if((integerp(arg1) || floatp(arg1) || rationalp(arg1)) &&
       (integerp(arg2) || floatp(arg2) || rationalp(arg2))){
       
    	arg1 = exact_to_inexact(arg1);
        arg2 = exact_to_inexact(arg2);
        x = GET_FLT(arg1);
        y = GET_FLT(arg2);
        z = pow(x,y);
        return(make_flt(z));
    }
    
    if(realp(arg1))
    	arg1 = realtocomp(arg1);
    x = GET_REAL_FLT(arg1);
    y = GET_IMAG_FLT(arg1);
	z = x+y*I;
    if(realp(arg2))
    	arg2 = realtocomp(arg2);
    x1 = GET_REAL_FLT(arg2);
    y1 = GET_IMAG_FLT(arg2);
    z1 = x1+y1*I;
    z2 = cpow(z,z1);
    x2 = creal(z2);
    y2 = cimag(z2);
    return(make_comp(x2,y2));
}

int square(int x){
	return(mult(x,x));
}

//xにはセルとしての整数、ｙはｃ言語の非負整数。
int expt(int x, int y){
	if(y == 1)
    	return(x);
    else
    if((y % 2) == 0)
    	return(square(expt(x,y/2)));
    else
    	return(mult(x,expt(x,y-1)));
}

int f_zerop(int n){
	int arg;
    
    arg = pop_s();
    
    if(!numberp(arg))
    	exception("zero?", NOT_NUMBER, arg);
        
    if(zerop(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}


int f_realpart(int n){
	int arg;
    double x;
    
    arg = pop_s();
    
    if(!numberp(arg))
    	exception("real-part", NOT_NUMBER, arg);

    if(complexp(arg)){
    	x = GET_REAL_FLT(arg);
        return(make_flt(x));
    }
    if(realp(arg))
    	return(arg);
    
    return(undef);    	
}

int f_imagpart(int n){
	int arg;
    double x;
    
    arg = pop_s();
    
    if(!numberp(arg))
    	exception("imag-part", NOT_NUMBER, arg);
    
    if(realp(arg))
    	return(make_int(0));
    else{
    	x = GET_IMAG_FLT(arg);
    	return(make_flt(x));
    }
}

int f_magnitude(int n){
	int arg;
    double x,y;
    
    arg = pop_s();
    
    if(!numberp(arg))
    	exception("magnitude", NOT_REAL, arg);
    
    if(realp(arg))
    	return(s_abs(arg));
   
    if(complexp(arg)){
    	x = GET_REAL_FLT(arg);
    	y = GET_IMAG_FLT(arg);
    	return(make_flt(sqrt(x*x + y*y)));
    }
    return(undef);
}

int f_angle(int n){
	int arg;
    double x,y;
    
    arg = pop_s();
    
    if(!numberp(arg))
    	exception("angle", NOT_NUMBER, NIL);
    if(complexp(arg)){
    	x = GET_REAL_FLT(arg);
        y = GET_IMAG_FLT(arg);
    	return(make_flt(argz(x,y)));
    }
    else
    	return(make_int(0));
}

int f_make_rectangular(int n){
	int arg1,arg2;
    double x,y;
    
	arg2 = pop_s();
    arg1 = pop_s();
    if(!realp(arg1))
    	exception("make-rectangular", NOT_REAL, arg1);
    if(!realp(arg2))
    	exception("make-rectangular", NOT_REAL, arg2);
    
    arg1 = exact_to_inexact(arg1);
    arg2 = exact_to_inexact(arg2);
    x = GET_FLT(arg1);
    y = GET_FLT(arg2);
    return(make_comp(x,y));
}

int f_make_polar(int n){
	int arg1,arg2;
    double r,s,x,y;
    
	arg2 = pop_s();
    arg1 = pop_s();
    if(!realp(arg1))
    	exception("make-polar", NOT_REAL, arg1);
    if(!realp(arg2))
    	exception("make-polar", NOT_REAL, arg2);
    
    arg1 = exact_to_inexact(arg1);
    arg2 = exact_to_inexact(arg2);
    r = GET_FLT(arg1);
    s = GET_FLT(arg2);
    x = r * cos(s);
    y = r * sin(s);
    return(make_comp(x,y));
}


int	f_exact_inexact(int n){
	int arg;
    
    arg = pop_s();
    if(!numberp(arg))
    	exception("exact->inexact", NOT_NUMBER, arg);
    
    return(exact_to_inexact(arg));
}

int	f_inexact_exact(int n){
	int arg;
    
    arg = pop_s();
    if(!numberp(arg))
    	exception("inexact->exact", NOT_NUMBER, arg);

	return(inexact_to_exact(arg));
}

//-------真偽値-----------------

int f_not(int n){
	int arg;

    arg = pop_s();

    if(arg == BOOLF)
    	return(BOOLT);
    else
    	return(BOOLF);
}

//-------入力-----------------------
int f_read(int n){
	int arg,res;
    
    
    if(n == 1){
    	arg = pop_s();
    	input_port = GET_PORT(arg);
    }
	res = read();
    input_port = stdin;
    return(res);
}

int f_read_char(int n){
	int arg,res;
    char c;
    
    
    if(n == 1){
    	arg = pop_s();
    	input_port = GET_PORT(arg);
    }
    c = getc(input_port);
    input_port = stdin;
    if(c == EOF)
    	return(end_of_file);
    else{
    	res = make_char(" ");
        SET_CHAR(res,c);
        return(res);
    }
}

int f_peek_char(int n){
	int arg,res;
    char c;
    
    if(n == 0){
    	arg = pop_s();
    	input_port = GET_PORT(arg);
    }
    c = getc(input_port);
    ungetc(c,input_port);
    input_port = stdin;
    if(c == EOF)
    	return(end_of_file);
    else{
    	res = make_char(" ");
        SET_CHAR(res,c);
        return(res);
    }
}

int f_read_line(int n){
	char str[BUFSIZE],c;
    int pos;
    
    pos = 0;
    c = getc(input_port);
    while((c != EOL) && (c != EOF)){
    	str[pos] = c;
        pos++;
        c = getc(input_port);	
    }
	str[pos] = NUL;
    return(make_str(str));
}

int f_read_string(int n){
	char str[BUFSIZE],c;
    int arg,pos,m;
    
    arg = pop_s();
    if(!IS_INTEGER(arg) || negativep(arg))
    	exception("read-string",NOT_EXACT,arg);
    
    m = get_int(arg);    
    pos = 0;
    c = getc(input_port);
    while((c != EOF) && (m > 0)){
    	str[pos] = c;
        pos++;
        m--;
        c = getc(input_port);	
    }
	str[pos] = NUL;
    return(make_str(str));
}

//実装困難につき常時#tを返すこととする。
int	f_char_readyp(int n){
	return(BOOLT);
}

int f_load(int n){
	int arg,sexp,l,i;
    
    arg = pop_s();
    input_port = fopen(GET_NAME(arg),"r");
    if(input_port == NULL)
    	exception("load", CANT_OPEN, arg);
    
	s_pc = pc;
    s_sp = sp;
    s_env = env;
    s_n_args = n_args;
    s_head = head;
    s_tail = tail;
    s_code_pointer_end = code_pointer_end;
    for(i=0; i<code_pointer_end; i++){
    	s_code_pointer[i][0] = code_pointer[i][0];
        s_code_pointer[i][1] = code_pointer[i][1];
    }
    for(i=0; i<tail; i++)
    	s_code[i] = code[i];
    
    for(i=0; i<sp; i++)
    	s_stack[i] = stack[i];
    
    loadflag = 1;
    l = strlen(GET_NAME(arg));
    if(STRING_REF(arg,l-2) == '.' && STRING_REF(arg,l-1) == 'o'){
    	while(1){
        	sexp = read();
        	if(sexp == end_of_file)
        		break;
			pc = 0;
            tail = 0;
        	list_to_code(sexp);
        	vm1();
    	} 	
    }
    else{
    	int ret = setjmp(loadloop);   
    	while(1){
			if(ret == 1){
            	contflag = 0;
            }
        	sexp = read();
        	if(sexp == end_of_file)
        		break;
        
        	pc = 0;
        	tail = 0;
        	env = make_env(0,0);
        	eval(sexp);   
    	}
    }
    fclose(input_port);
    loadflag = 0;
    
    pc = s_pc;
    sp = s_sp;
    env = s_env;
    n_args = s_n_args;
    head = s_head;
    tail = s_tail;
    code_pointer_end = s_code_pointer_end;
    for(i=0; i<s_code_pointer_end; i++){
    	code_pointer[i][0] = s_code_pointer[i][0];
        code_pointer[i][1] = s_code_pointer[i][1];
    }
    for(i=0; i<tail; i++)
    	code[i] = s_code[i];
    
    for(i=0; i<sp; i++)
    	stack[i] = s_stack[i];
    
    if(contflag){
    	longjmp(toplevel,1);	
    }
    return(BOOLT);
}


int	f_open_input_file(int n){
	int arg;
    FILE *port;
    
    arg = pop_s();
    if(!stringp(arg))
    	exception("open-input", NOT_STRING, arg);
        
    port = fopen(GET_NAME(arg),"r");
    if(port == NULL)
    	exception("open-input", CANT_OPEN, arg);
    loadflag = 1;
    return(make_port(port,0));
}

int f_input_portp(int n){
	int arg;
    
    arg = pop_s();
    if(GET_TAG(arg) == PRT && GET_CDR(arg) == 0)
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_close_input_port(int n){
	int arg;
    
    arg = pop_s();
    fclose(GET_PORT(arg));
    loadflag = 0;
    return(undef);
}

int f_eof_objectp(int n){
	int arg;
    
    arg = pop_s();
    if(GET_TAG(arg) == EOFO)
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_current_input_port(int n){
	
    return(make_port(input_port,0));
}

//-------出力---------------
int	f_open_output_file(int n){
	int arg;
    FILE *port;
    
    arg = pop_s();
	if(!stringp(arg))
    	exception("open-output", NOT_STRING, arg);
        
    port = fopen(GET_NAME(arg),"w");
    if(port == NULL)
    	exception("open-output", CANT_OPEN,arg);
    loadflag = 1;
    return(make_port(port,1));
}

int f_close_output_port(int n){
	int arg;
    
    arg = pop_s();
    fclose(GET_PORT(arg));
    loadflag = 0;
    return(undef);
}

int f_output_portp(int n){
	int arg;
    
    arg = pop_s();
    if(GET_TAG(arg) == PRT && GET_CDR(arg) == 1)
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_current_output_port(int n){

    return(make_port(output_port, 1));    
}

int f_newline(int n){
	int arg;
    
    if(n == 0)	
		printf("\n");
    else{
    	arg = pop_s();
    	output_port = GET_PORT(arg);
    	fprintf(output_port, "\n");
    }
    return(undef);
}

void display_str(int x){
	char buf[SYMSIZE];
    int pos;
    char c;
    
    strcpy(buf,GET_NAME(x));
    pos = 0;
    c = buf[pos];
    while(c != NUL){
    	switch(c){
        	case '\\':	
            	pos++;
            	c = buf[pos];
                switch(c){
        			case 'n':	fprintf(output_port,"\n"); goto next;
            		case 'r':	fprintf(output_port,"\r"); goto next;
            		case 't':	fprintf(output_port,"\t"); goto next;
                }
        	default:	fprintf(output_port,"%c", c);
        }
        next:
        c = buf[++pos];
    }
}

int output_portp(int x){
	if(GET_TAG(x) == PRT && GET_CDR(x) == 1)
    	return(1);
    else
    	return(0);
}

int f_display(int n){
	int arg1,arg2;
    
    if(n == 1){
    	arg2 = NIL;
        arg1 = pop_s();
    }
    else{	
    	arg2 = pop_s();
    	arg1 = pop_s();
	}
    if(!nullp(arg2) && !output_portp(arg2))
    	exception("display", NOT_PORT,arg2);
    if(!nullp(arg2))
    	output_port = GET_PORT(arg2);
        
    if(stringp(arg1))
    	display_str(arg1);
    else
    if(charp(arg1))
    	fprintf(output_port, "%c", GET_CHAR(arg1));
    else
		print(arg1);
        
    output_port = stdout;
    return(undef);
}

int f_write(int n){
	int arg1,arg2;
    
    if(n == 1){
    	arg2 = NIL;
        arg1 = pop_s();
    }
    else{
    	arg2 = pop_s();
    	arg1 = pop_s();
    }
    if(!nullp(arg2) && !output_portp(arg2))
    	exception("write", NOT_PORT,arg2);
	if(!nullp(arg2))
		output_port = GET_PORT(arg2);
    print(arg1);
    output_port = stdout;
    return(undef);
}

int f_write_char(int n){
	int arg1,arg2;
    char* c;
    
    if(n == 1){
    	arg2 = NIL;
        arg1 = pop_s();
    }
    else{
    	arg2 = pop_s();
    	arg1 = pop_s();
    }
    
    if(!charp(arg1))
    	exception("write-char", NOT_CHAR, arg1);
    if(!nullp(arg2) && !output_portp(arg2))
    	exception("write-char", NOT_PORT,arg2);
    if(!nullp(arg2))
    	output_port = GET_PORT(arg2);
        
    c = GET_NAME(arg1);
    fputs(c, output_port);
	fflush(output_port);
    output_port = stdout;
    return(undef);
}

// --------------制御------------------------
int f_exit(int n){
	longjmp(toplevel,2);
}




//------------------------------
int	f_gensym(int n){
	int arg,res;
    char str1[SYMSIZE],str2[10];
    
    if(n == 0)
    	arg = NIL;
    else
        arg = pop_s();
    
    if(!nullp(arg) && !stringp(arg))
    	exception("gensym", NOT_STRING, arg);
	
    if(nullp(arg))
    	strcpy(str1,"#:G");
    else{
    	strcpy(str1,"#:");
    	strcat(str1,GET_NAME(arg));
    }
    itoa(genint,str2,10);
    genint++;
    strcat(str1,str2);
    res = make_sym(str1);
    return(res);      
}

int f_apply(int n){
	int proc,args,i,code,m,savepc,savesp,res;
    
    args = pop_s();
    if(!listp(args))
    	exception("apply", NOT_LIST, args);
    for(i=n-2; i>0; i--)
    	args = cons(pop_s(),args);
   
	proc = pop_s();
	if(!closurep(proc) && !subrp(proc) &&!continuationp(proc))
    	exception("apply",NOT_PROCEDURE,proc);
        
    savepc = pc;
	savesp = sp;
    //スタックが底をついた状態でSPを保持。
    m = 0;
    while(!nullp(args)){
    	push_s(car(args));
        args = cdr(args);
        m++;
    }
    if(closurep(proc)){
    	push_s(proc);
    	code = cons(make_int(13),
                cons(make_int(m),
                 cons(make_int(33),NIL))); //((call n)(pause))
    }
    else if(subrp(proc)){
    	code = cons(make_int(17),
                cons(make_int(proc),
                 cons(make_int(m),
                  cons(make_int(33),NIL)))); //((call proc n)(pause))
    }
    else{
		//continuation
    	push_s(proc);
    	code = cons(make_int(13),
                cons(make_int(m),
                 cons(make_int(1),NIL))); //((call n)(halt))
    }
    list_to_code(code);
    pc = head;
    if(!debugflag)
    	res = vm1();
    else
    	res = vm2();
    
    if(contflag && continuationp(proc)){
    	print(res);
        printf("\n");
        if(loadflag)
        	longjmp(loadloop,1);
        else
			longjmp(toplevel,1);
    }        
    pc = savepc;
    sp = savesp;
    return(res);
}

int f_primitive_name_p(int n){
	int arg;
    
    arg = pop_s();
    arg = GET_CAR(arg);
    if(subrp(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_macroexpand1(int n){
	int arg;
    
    arg = pop_s();
    if(!macro_namep(car(arg)) && !hygienic_namep(car(arg)))
    	exception("macroexpand", NOT_MACRO, arg);
	
    return(macroexpand1(arg));
}

int macroexpand1(int arg){
	int clo,m,code,new_expr,savepc,savesp,saveenv;
    
	//伝統的マクロの場合
    if(macro_namep(car(arg))){
    	savepc = pc;
    	savesp = sp;
        saveenv = env;
    	clo = GET_CAR(GET_CAR(car(arg)));
        arg = cdr(arg);
        
        m = 0;

        while(!nullp(arg)){
        	push_s(car(arg));
            m++;
            arg = cdr(arg);	
        }
        push_s(clo);
        code = list3(make_int(13),make_int(m),make_int(33));
		//((call m)(pause)) 
        list_to_code(code);
        pc = head; 
        new_expr = vm1();
        pc = savepc;
    	sp = savesp;
        env = saveenv;

    }
    
    else if(hygienic_namep(car(arg))){
    	savepc = pc;
    	savesp = sp;
        saveenv = env;
    	clo = GET_CAR(GET_CAR(car(arg)));
        
        push_s(arg);
        push_s(clo);
        code = list3(make_int(13),make_int(1),make_int(33));
		//((call 1)(pause)) 
        list_to_code(code);
        pc = head;
        new_expr = vm1();
        pc = savepc;
    	sp = savesp;
        env = saveenv;
    }
    else
    	new_expr = arg;
    
    return(new_expr);
}

int f_macroexpand(int n){
	int arg;
    
    arg = pop_s();
	return(macroexpand(arg));
}

int macroexpand(int x){
    
	if(nullp(x))
    	return(NIL);
    else if(atomp(x))
    	return(x);
    else if(vectorp(x))
    	return(x);
	else if(bytevectorp(x))
    	return(x);
    else if(eqp(car(x),quote))
    	return(x);
    else if(macro_namep(car(x)))
    	return(macroexpand(macroexpand1(x)));
    else if(hygienic_namep(car(x)))
    	return(macroexpand(macroexpand1(x)));
    else if(atomp(car(x)))
    	return(cons(car(x),macroexpand(cdr(x))));
    else
    	return(cons(macroexpand(car(x)),macroexpand(cdr(x))));
}

int	f_addr(int n){
	int arg;
    
    arg = pop_s();
    return(make_int(arg));
}

int f_entity_addr(int n){
	int arg;
    
    arg = pop_s();
    if(car(arg) == undef){
    	exception("", UNBOUND_VARIABLE, arg);
    	return(undef);
    }
    else
    	return(make_int(car(arg)));
}

int f_undefined(int n){
	
    return(undef);
}

int f_step(int n){
	int arg;
    
    arg = pop_s();
    if(!booleanp(arg))
    	exception("step",NOT_BOOL,arg);
    
    if(arg == BOOLT)
    	step_on = 1;
    else
    	step_on = 0;
    
    return(arg);
}

int f_vm2_step(int n){
	int arg,res,i;
    
    arg = pop_s();
    
    s_pc = pc;
    s_sp = sp;
    s_env = env;
    s_n_args = n_args;
    s_head = head;
    s_tail = tail;
    s_code_pointer_end = code_pointer_end;
    for(i=0; i<code_pointer_end; i++){
    	s_code_pointer[i][0] = code_pointer[i][0];
        s_code_pointer[i][1] = code_pointer[i][1];
    }
    for(i=0; i<tail; i++)
    	s_code[i] = code[i];
    
    for(i=0; i<sp; i++)
    	s_stack[i] = stack[i];
    
    
    head = 0;
    tail = 0;
    list_to_code(arg);
    pc = 0;
    sp = 0;
    stepflag = 1;
    res = vm2();
    stepflag = 0;
    
    pc = s_pc;
    sp = s_sp;
    env = s_env;
    n_args = s_n_args;
    head = s_head;
    tail = s_tail;
    code_pointer_end = s_code_pointer_end;
    for(i=0; i<s_code_pointer_end; i++){
    	code_pointer[i][0] = s_code_pointer[i][0];
        code_pointer[i][1] = s_code_pointer[i][1];
    }
    for(i=0; i<tail; i++)
    	code[i] = s_code[i];
    
    for(i=0; i<sp; i++)
    	stack[i] = s_stack[i];
    

    return(res);
}

int f_vm1(int n){
	int arg,res,savepc,savesp,saveenv;
    
    arg = pop_s();
    
	savepc = pc;
    savesp = sp;
    saveenv = env;
    
    list_to_code(arg);
    pc = head;
    stepflag = 0;
    res = vm1();
    
    pc = savepc;
    sp = savesp;
    env = saveenv;
    return(res);
}



int f_vm2(int n){
	int arg,res,i;
    
    arg = pop_s();
    
    s_pc = pc;
    s_sp = sp;
    s_env = env;
    s_n_args = n_args;
    s_head = head;
    s_tail = tail;
    s_code_pointer_end = code_pointer_end;
    for(i=0; i<code_pointer_end; i++){
    	s_code_pointer[i][0] = code_pointer[i][0];
        s_code_pointer[i][1] = code_pointer[i][1];
    }
    for(i=0; i<tail; i++)
    	s_code[i] = code[i];
    
    for(i=0; i<sp; i++)
    	s_stack[i] = stack[i];
    
    
    head = 0;
    tail = 0;
    list_to_code(arg);
    pc = 0;
    sp = 0;
    stepflag = 0;
    res = vm2();
    
    pc = s_pc;
    sp = s_sp;
    env = s_env;
    n_args = s_n_args;
    head = s_head;
    tail = s_tail;
    code_pointer_end = s_code_pointer_end;
    for(i=0; i<s_code_pointer_end; i++){
    	code_pointer[i][0] = s_code_pointer[i][0];
        code_pointer[i][1] = s_code_pointer[i][1];
    }
    for(i=0; i<tail; i++)
    	code[i] = s_code[i];
    
    for(i=0; i<sp; i++)
    	stack[i] = s_stack[i];

    return(res);
}



int f_dump(int n){
	int arg1,arg2,x,y;
    
    if(n == 2){
    	arg2 = pop_s();
        arg1 = pop_s();
        x = get_int(arg1);
        y = get_int(arg2);
    }
    else {
    	arg1 = pop_s();
        x = get_int(arg1);
        y = x + 10;
    }	
 	memorydump(x,y);
    return(undef);   	
}

int f_addr_prt(int n){
	int arg,x;
    
    arg = pop_s();
    x = get_int(arg);
    print(x);
    printf("\n");
    return(undef);
}

int f_gbc(int n){
	int arg;
    
    if(n == 0){
    	gbc();
        gctime = 0;
    }
    else {
    	arg = pop_s();    
    	if(arg == BOOLT)
    		gbcflag = 1;
    	if(arg == BOOLF)
    		gbcflag = 0;
    }
    
    return(undef);     
}

int f_room(int n){
	printf("cell_free     = %d\n",cell_free);
    printf("cell_heap_p   = %d\n",cell_heap_p);
	printf("dyna_env_p1   = %d\n",dyna_env_p1);
    printf("dyna_env_p2   = %d\n",dyna_env_p2);
    return(undef);
}

int f_freecell(int n){
	
    return(make_int(cell_free));
}

int f_vmcode(int n){
	int arg,code,i,res;
    
    arg = pop_s();
	
    if(!closurep(arg) && !macrop(arg))
    	exception("vmcode",NOT_CLOSURE,arg);
    if(closurep(arg))
    	code = GET_CAR(arg);
    else
    	code = GET_CAR(GET_CAR(arg));
        
    i = GET_CDR(code);
    res = NIL;
    while(i > 0){
    	res = cons(make_int(GET_VEC_ELT(code,(i-1))),res);
        i--;
    }
    return(res);
}

int f_env(int n){
	int arg;
    
    arg = pop_s();
    print(GET_CDR(arg));
    return(undef);
}

int f_timer_set(int n){
	
    start_time = clock();
    return(undef);
}

int f_timer_get(int n){
	clock_t end_time;
    double time;
    
    end_time = clock();
    time = (double)(end_time - start_time)/ CLOCKS_PER_SEC;
	time = round(time*1000)/1000;
    return(make_flt(time));
}

int f_timer_gbc(int n){
	double time;
    
    time = (double)gctime/ CLOCKS_PER_SEC;
    time = round(time*1000)/1000;
    return(make_flt(time));
}

int f_current_second(int n){
	time_t t;
    double s;
    
    t = time(NULL);
    s = (double)t;
    return(make_flt(s));
}

int f_current_jiffy(int n){
	clock_t t1;
    int t2;
    
    t1 = clock();
    t2 = (int)t1;
    return(make_int(t2));
}

int f_jiffies_per_second(int n){
	int t;
    
    t = (int)CLOCKS_PER_SEC;
    return(make_int(t));
}

int f_eval(int n){
	int arg;
    
    arg = pop_s();
    return(eval(arg));
}

int f_error(int n){
	int arg1,arg2,i;
    
    arg2 = NIL;
    while(n > 1){
    	arg2 = cons(pop_s(),arg2);
        n--;
    }
    arg1 = pop_s();
    if(!stringp(arg1))
    	exception("error", NOT_STRING, arg1);
    
	printf("Exception: ");
    display_str(arg1);
    printf(" ");
    while(!nullp(arg2)){
    	print(car(arg2));
        printf(" ");
        arg2 = cdr(arg2);
    }
    printf("\n");
	//load中ならクローズしてトップレベルへ復帰。
    if(loadflag == 1)
    	fclose(input_port);
    //クロージャに記憶した命令列の開始アドレスを消去。
    for(i=0; i< code_pointer_end; i++){
    	SET_AUX(code_pointer[i][0] , -1);
    }
    code_pointer_end = 0; 
    longjmp(toplevel,1);
}

int f_flush(int n){
	
    fflush(stdout);
    return(undef);
}

int f_flush_output_port(int n){
	int arg;
    
    if(n == 0)
    	fflush(stdout);
    else{
    	arg = pop_s();
        if(!IS_PORT(arg))
        	exception("flush-output-port", NOT_PORT, arg);
        fflush(GET_PORT(arg));
    }
    return(undef);
}

int f_set_trace(int n){
	int i,fn;
    
    if(n == 0)
    	return(trace_list);
    else{
    	for(i=n; i>0; i--){
    		fn = pop_s();
            if(!symbolp(fn))
            	exception("trace",NOT_SYMBOL,fn);
            if(memq(fn,trace_list) == BOOLF){
        		SET_CDR(fn,1);
        		trace_list = cons(fn,trace_list);
            }
    	}
    	return(undef);
    }
}

int f_set_untrace(int n){
	int i,fn,addr;
    
    if(n == 0){
    	while(!nullp(trace_list)){
        	SET_CDR(car(trace_list),0);
            trace_list = cdr(trace_list);
        }
    }
    else{
    	for(i=n; i>0; i--){
        	fn = pop_s();
            SET_CDR(fn,0);
     		addr = trace_list;
            while(!nullp(addr)){
            	if(eqp(fn,car(addr))){
                	SET_CDR(addr,cdr(addr));
                	break;
                }
                addr = cdr(addr);
            }       
    	}
    }
    return(undef);
}



int f_current_module(int n){

	return(module_table[current_module][0]);
}


int f_debug(int n){
	int arg;
    
    arg = pop_s();
    if(!booleanp(arg))
    	exception("debug", NOT_BOOL, arg);
    
    if(arg == BOOLT)
    	debugflag = 1;
    else{
    	debugflag = 0;
        prof_on = 0;
        step_on = 0;
    }
    return(arg);
}

int f_prof(int n){
	int arg;
    
	arg = pop_s();
    if(!booleanp(arg))
    	exception("profile", NOT_BOOL, arg);
    
    if(arg == BOOLT)
    	prof_on = 1;
    else
    	prof_on = 0;
    
    return(arg);
}

int f_edump(int n){
	int arg,x,i;
    
    arg = pop_s();
    x = get_int(arg);
    
    for(i=x; i<x+10; i++){
    	printf("[");
        print(emem1[i]);
        printf("]");
    }
    return(BOOLT);
}

int f_lambda_asm(int n){
	int m,code,machine,res,savepc,savesp,saveenv;
    
    code = pop_s();
    m = pop_s();
    if(!integerp(m))
    	exception("lambda/asm", NOT_INTEGER, m);
    
    if(!listp(code))
    	exception("lambda/asm", NOT_CLOSURE, code);
    
	savepc = pc;
    savesp = sp;
    saveenv = env;
    
    res = make_clo();
    SET_ARGS_CNT(res,get_int(m));
    machine = cons(make_int(2),
               cons(code,
                cons(make_int(4),
                 cons(make_sym("assemble"),
                  cons(make_int(13),
                   cons(make_int(1),
                    cons(make_int(1),NIL)))))));
    list_to_code(machine);
    pc = head;
    code = vm1();
    SET_CAR(res,list_to_code_obj(code));
    SET_CDR(res,env);
    
    pc = savepc;
    sp = savesp;
    env = saveenv;
    return(res);
}


int f_values(int n){
	int lis,i;
    
    if(n == 0)
    	//０個の場合には空集合を返す。
    	return(empty_set);
    else if(n == 1)
        //単数の場合にはそれそのものを返す。
    	return(pop_s());
    else{
    	lis = NIL;
    	for(i=0; i<n; i++)
    		lis = cons(pop_s(),lis);
    
    	return(make_multiple_values(lis));
    }
}

int f_sys_cont_room(int n){
	int clo,cont,cont_code,cont_stack,cont_env,saveenv;
    
    clo = pop_s();
    
    saveenv = env;
    env = cdr(clo);
    cont = get_lvar(1,0);
    cont_code = GET_CAR(cont);
    cont_stack = GET_CDR(cont);
    cont_env = car(GET_AUX(cont));
    
    printf("code size  %d\n",GET_VEC_ELT(cont_code,3));
    printf("stack size %d\n", GET_CDR(cont_stack));
    print(cont_env);
    env = saveenv;
    return(undef);
}

int f_make_syntactic_closure(int lvar){
	int env,fv,expr,res;
    
    expr = pop_s();
    fv = pop_s();
    env = pop_s();
    
    if(IS_SYNCLO(expr))
    	return(expr);
    
    res = freshcell();
    SET_TAG(res,SYNCLO);
    SET_CAR(res,expr);
    SET_CDR(res,env);
    SET_AUX(res,fv);
    return(res);	
}

int f_syntactic_closure_expr(int n){
	int arg;
    
    arg = pop_s();
    if(!syntactic_closurep(arg))
    	exception("syntactic-closure-expr", NOT_SYNTACTIC, arg);
    
    return(GET_CAR(arg));
}

int f_syntactic_closure_env(int n){
	int arg;
    
    arg = pop_s();
    if(!syntactic_closurep(arg))
    	exception("syntactic-closure-env", NOT_SYNTACTIC, arg);
    
    return(GET_CDR(arg));
}

int f_syntactic_closure_freevar(int n){
	int arg;
    
    arg = pop_s();
    if(!syntactic_closurep(arg))
    	exception("syntactic-closure-freevar", NOT_SYNTACTIC, arg);
    
    return(GET_AUX(arg));
}

int f_symbol_to_identifier(int n){
	int arg;
    
    arg = pop_s();
    
    if(!symbolp(arg))
    	exception("symbol->identifier",NOT_SYMBOL,arg);
    
    return(make_ident(GET_NAME(arg)));
}

int f_identifier_to_symbol(int n){
	int arg;
    
    arg = pop_s();
    
    if(!identifierp(arg))
    	exception("identifier->symbol",NOT_IDENTIFIER,arg);
    
    return(make_sym(GET_NAME(arg)));
}

int f_syntactic_closurep(int n){
	int arg;
    
    arg = pop_s();
    if(syntactic_closurep(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_identifierp(int n){
	int arg;
    
    arg = pop_s();
    if(identifierp(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_identifier_bind(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!identifierp(arg1))
    	exception("identifier-bind!", NOT_IDENTIFIER, arg1);
    
    SET_AUX(arg1,arg2);
    return(arg1);
}

int f_identifier_freep(int n){
	int arg;
    
    arg = pop_s();
    
    if(identifierp(arg) && GET_AUX(arg) == undef)
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_identifier_boundp(int n){
	int arg;
    
    arg = pop_s();
    
    if(identifierp(arg) && GET_AUX(arg) == NIL)
    	return(BOOLF);
    else
    	return(BOOLT);
}

int f_global_boundp(int n){
	int arg;
    
    arg = pop_s();
    
    if(!symbolp(arg))
    	exception("global-bound?", NOT_SYMBOL, arg);
    
    if(GET_CAR(arg) == undef)
    	return(BOOLF);
    else
    	return(BOOLT);
}

int f_identifier_bound(int n){
	int arg;
    
    arg = pop_s();
    if(!identifierp(arg))
    	exception("identifier-bound", NOT_IDENTIFIER, arg);
    
    return(GET_AUX(arg));
}

int f_identifier_variable(int n){
	int arg;
    
    arg = pop_s();
    if(!identifierp(arg))
    	exception("identifier-variable!", NOT_IDENTIFIER, arg);
    
    SET_CAR(arg,1);
    return(arg);
}

int f_identifier_variablep(int n){
	int arg;
    
    arg = pop_s();
    if(identifierp(arg) && (GET_CAR(arg) == 1))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_identifier_ellipsis(int n){
	int arg;
    
    arg = pop_s();
    if(!identifierp(arg))
    	exception("identifier-ellipsis!", NOT_IDENTIFIER, arg);
    
    SET_CAR(arg,2);
    return(arg);
}

int f_identifier_ellipsisp(int n){
	int arg;
    
    arg = pop_s();
    if(identifierp(arg) && (GET_CAR(arg) == 2))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_inspect(int n){
	int sexp,i;
    
    while(1){
    	printf("inspect> ");
        fflush(stdout);
        sexp = read();
        
        if(eqvp(sexp,make_sym("q")))
        	return(undef);
    	else if(eqvp(sexp,make_sym("b"))){
        	printf("back trace\n");
    		for(i=0; i<back_trace_end; i++){
				printf("[%d] ", i - back_trace_end);
        		print(cons(back_trace[i][0],back_trace[i][1]));
            	printf("\n");
        	}
        }
        	
    }
}

int f_exact_integerp(int n){
	int arg;
    
    arg = pop_s();
    
    if(IS_INTEGER(arg) || IS_BIGNUM(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_file_existsp(int n){
	int arg;
    FILE *port;
    
    arg = pop_s();
    if(!stringp(arg))
    	exception("file-exists?",NOT_STRING, arg);
    
    port = fopen(GET_NAME(arg),"r");
    if(port != NULL){
    	fclose(port);
        return(BOOLT);
    }
    else
    	return(BOOLF);
}

int f_system(int n){
	int arg;
    
    arg = pop_s();
    if(!stringp(arg))
    	exception("system", NOT_STRING, arg);
    
    system(GET_NAME(arg));
    return(undef);
}

int f_infinityp(int n){
	int arg;
    
    arg = pop_s();
    if(infinityp(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_finityp(int n){
	int arg;
    
    arg = pop_s();
    if(numberp(arg) && !infinityp(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_nanp(int n){
	int arg;
    
    arg = pop_s();
    if(nanp(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_square(int n){
	int arg;
    
    arg = pop_s();
    if(!numberp(arg))
    	exception("square",NOT_NUMBER,arg);
    
    return(mult(arg,arg));
}

int	f_bytevectorp(int n){
	int arg;
    
    arg = pop_s();
    if(IS_U8VECTOR(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_command_line(int n){
	int i,res;
    
    res = NIL;
    if(command_line_count == 1)
    	res = make_str(command_line[0]); 
    else{
    	for(i=command_line_count; i>0; i--)
        	res = cons(make_str(command_line[i-1]),res);
    }
    return(res);
}

int f_get_environment_variable(int n){
	int arg,res;
    DWORD dwResult;
    TCHAR val[2048];

    arg = pop_s();
    if(!stringp(arg))
    	exception("get-environment-variable", NOT_STRING, arg);

    dwResult = GetEnvironmentVariable(_T(GET_NAME(arg)),val,sizeof(val));
    if(dwResult != 0){
        res = make_str((char*)val);
    }
    else
    	res = BOOLF;
    
	return(res);    
}

int f_get_environment_variables(int n){
	int i,res;
    char *envstr, varname[128];
    LPTSTR l_str;
    DWORD dwResult;
    TCHAR val[2048];
    
    res = NIL;
    envstr = GetEnvironmentStrings();
    
    l_str = envstr;
	//最初の3個は意味のないデータなので飛ばす。
    for(i=0; i<3; i++){	
    	while (*l_str != NUL) l_str++;
        	l_str++;
    }
    while (1){
    	if (*l_str == NUL) break; 
        i = 0;
        while(*l_str != '='){
        	varname[i] = *l_str;
            l_str++;
            i++;
        }
        varname[i] = NUL;	
    
        dwResult = GetEnvironmentVariable(_T(varname),val,sizeof(val));
        if(dwResult != 0){
        	res = cons(cons(make_str(varname),make_str((char*)val)),res);
    	}
    	else
    		res = cons(cons(make_str(varname),BOOLF),res);
        
        while (*l_str != NUL) l_str++;
        l_str++;
    
    }
	
    FreeEnvironmentStrings(envstr);
    return(reverse(res));
	
}

int f_get_car(int n){
	int arg;
    
    arg = pop_s();
    
    return(GET_CAR(arg));
}

int f_make_record(int n){
	int arg1,arg2;
    
    if(n == 2){
    	arg2 = pop_s();
        arg1 = pop_s();
    }
    else{
    	arg2 = undef;
        arg1 = pop_s();
    }
    if(!IS_INTEGER(arg1) || negativep(arg1))
    	exception("make-record",NOT_EXACT, arg1);  
    return(make_record(get_int(arg1),arg2));	
}

int f_recordp(int n){
	int arg;
    
    arg = pop_s();
    if(IS_RECORD(arg))
    	return(BOOLT);
    else
    	return(BOOLF);
}

int f_record_set(int n){
	int arg1,arg2,arg3;
    
    
    arg3 = pop_s();
    arg2 = pop_s();
    arg1 = pop_s();
    if(!IS_RECORD(arg1))
    	exception("record-set!",NOT_RECORD, arg1);
    if(GET_AUX(arg1) == 1)
    	exception("record-set",IMMUTABLE_OBJ, arg1);
    if(!IS_INTEGER(arg2) || negativep(arg2))
    	exception("record-set!",NOT_EXACT, arg2);
    
    record_set(arg1,get_int(arg2),arg3);
    return(undef);
}

int f_record_ref(int n){
	int arg1,arg2;
    
    arg2 = pop_s();
    arg1 = pop_s();
    if(!IS_RECORD(arg1))
    	exception("record-ref", NOT_RECORD, arg1);
	if(!IS_INTEGER(arg2) || negativep(arg2))
    	exception("record-ref",NOT_EXACT, arg2);
    if(get_int(arg2) >= record_length(arg1))
    	exception("record-ref", OUT_OF_RANGE,arg2);
    
    return(record_ref(arg1,get_int(arg2)));
}

int f_sleep(int n){
	int arg;
    
    arg = pop_s();
    if(!IS_INTEGER(arg) || negativep(arg))
    	exception("sleep",NOT_EXACT, arg);
    
    Sleep(get_int(arg));
    return(undef);
}

//subrを環境に登録する。
void defsubr(char *name, int func){
	int sym,val;
    
    sym = make_sym(name);
    val = freshcell();
    SET_TAG(val,SUBR);
    SET_SUBR(val,func);
    SET_NAME(val,name);
    SET_CAR(sym,val);
}

//特殊形式を環境に登録する。
void defsyntax(char *name){
	int sym,val;
    
    sym = make_sym(name);
    val = freshcell();
    SET_TAG(val,SYNT);
    SET_NAME(val,name);
    SET_CAR(sym,val);
}

void initsubr(void){
	defsubr("car",(int)f_car);
    defsubr("cdr",(int)f_cdr);
    defsubr("cons",(int)f_cons);
    defsubr("caar",(int)f_caar);
    defsubr("cdar",(int)f_cdar);
    defsubr("cddr",(int)f_cddr);
    defsubr("cadr",(int)f_cadr);
    defsubr("caaar",(int)f_caaar);
    defsubr("cdaar",(int)f_cdaar);
    defsubr("cadar",(int)f_cadar);
    defsubr("caadr",(int)f_caadr);
    defsubr("cddar",(int)f_cddar);
    defsubr("caddr",(int)f_caddr);
    defsubr("cdadr",(int)f_cdadr);
    defsubr("cdddr",(int)f_cdddr);
    defsubr("caaaar",(int)f_caaaar);
    defsubr("cdaaar",(int)f_cdaaar);
    defsubr("cadaar",(int)f_cadaar);
    defsubr("caadar",(int)f_caadar);
    defsubr("caaadr",(int)f_caaadr);
    defsubr("cddaar",(int)f_cddaar);
    defsubr("caddar",(int)f_caddar);
    defsubr("caaddr",(int)f_caaddr);
    defsubr("cdaadr",(int)f_cdaadr);
    defsubr("cdadar",(int)f_cdadar);
    defsubr("cadddr",(int)f_cadddr);
    defsubr("cdaddr",(int)f_cdaddr);
    defsubr("cddadr",(int)f_cddadr);
    defsubr("cdddar",(int)f_cdddar);
    defsubr("cddddr",(int)f_cddddr);
    defsubr("assq",(int)f_assq);
    defsubr("assv",(int)f_assv);
    defsubr("assoc",(int)f_assoc);
    defsubr("memq",(int)f_memq);
    defsubr("memv",(int)f_memv);
    defsubr("member",(int)f_member);
    defsubr("reverse",(int)f_reverse);
    defsubr("reverse!",(int)f_reverse2);
    defsubr("list-tail",(int)f_listtail);
    defsubr("list-ref",(int)f_listref);
    defsubr("list-set!",(int)f_listset);
    defsubr("append",(int)f_append);
    defsubr("append!",(int)f_append2);
    defsubr("set-car!",(int)f_setcar);
    defsubr("set-cdr!",(int)f_setcdr);
    defsubr("list",(int)f_list);
    defsubr("make-list",(int)f_makelist);
    defsubr("length",(int)f_length);
    defsubr("pair-length",(int)f_pair_length);
    defsubr("last",(int)f_last);
    defsubr("butlast",(int)f_butlast);
    defsubr("newline",(int)f_newline);
    defsubr("display",(int)f_display);
    defsubr("write",(int)f_write);
    defsubr("write-char",(int)f_write_char);
	defsubr("null?",(int)f_nullp);
    defsubr("list?",(int)f_listp);
    defsubr("pair?",(int)f_pairp);
    defsubr("atom?",(int)f_atomp);
    defsubr("eq?",(int)f_eqp);
    defsubr("eqv?",(int)f_eqvp);
    defsubr("equal?",(int)f_equalp);
    defsubr("boolean?",(int)f_boolp);
    defsubr("procedure?",(int)f_procedurep);
    defsubr("number?",(int)f_numberp);
    defsubr("integer?",(int)f_integerp);
    defsubr("real?",(int)f_realp);
    defsubr("rational?",(int)f_rationalp);
    defsubr("complex?",(int)f_complexp);
    defsubr("exact?",(int)f_exactp);
    defsubr("inexact?",(int)f_inexactp);
    defsubr("symbol?",(int)f_symbolp);
    defsubr("string?",(int)f_stringp);
    defsubr("char?",(int)f_characterp);
    defsubr("bignum?",(int)f_bignump);
    defsubr("vector?",(int)f_vectorp);
    defsubr("macro?",(int)f_macrop);
	defsubr("macro-name?",(int)f_macro_namep);
    defsubr("hygienic?",(int)f_hygienicp);
    defsubr("hygienic-name?",(int)f_hygienic_namep);
    defsubr("zero?",(int)f_zerop);
    defsubr("+",(int)f_plus);
    defsubr("-",(int)f_minus);
    defsubr("*",(int)f_mult);
    defsubr("/",(int)f_div);
    defsubr("<",(int)f_smallerp);
    defsubr("<=",(int)f_eqsmallerp);
    defsubr(">",(int)f_greaterp);
    defsubr(">=",(int)f_eqgreaterp);
    defsubr("=",(int)f_numeqp);
	defsubr("sin",(int)f_sin);
    defsubr("cos",(int)f_cos);
    defsubr("tan",(int)f_tan);
    defsubr("asin",(int)f_asin);
    defsubr("acos",(int)f_acos);
    defsubr("atan",(int)f_atan);
    defsubr("log",(int)f_log);
    defsubr("exp",(int)f_exp);
    defsubr("sqrt",(int)f_sqrt);
    defsubr("expt",(int)f_expt);
    defsubr("not",(int)f_not);
    defsubr("odd?",(int)f_oddp);
    defsubr("even?",(int)f_evenp);
    defsubr("floor",(int)f_floor);
    defsubr("ceiling",(int)f_ceiling);
    defsubr("truncate",(int)f_truncate);
    defsubr("round",(int)f_round);
    defsubr("numerator",(int)f_numerator);
    defsubr("denominator",(int)f_denominator);
    defsubr("positive?",(int)f_positivep);
    defsubr("negative?",(int)f_negativep);
    defsubr("abs",(int)f_abs);
    defsubr("max",(int)f_max);
    defsubr("min",(int)f_min);
    defsubr("real-part",(int)f_realpart);
    defsubr("imag-part",(int)f_imagpart);
    defsubr("magnitude",(int)f_magnitude);
    defsubr("angle",(int)f_angle);
    defsubr("make-rectangular",(int)f_make_rectangular);
    defsubr("make-polar",(int)f_make_polar);
    defsubr("exact->inexact",(int)f_exact_inexact);
    defsubr("inexact",(int)f_exact_inexact);
	defsubr("inexact->exact",(int)f_inexact_exact);
    defsubr("exact",(int)f_inexact_exact);
    defsubr("remainder",(int)f_remainder);
	defsubr("modulo",(int)f_modulo);
    defsubr("quotient",(int)f_quotient);
    defsubr("gcd",(int)f_gcd);
    defsubr("lcm",(int)f_lcm);
    defsubr("char=?",(int)f_char_eqp);
    defsubr("char>?",(int)f_char_greaterp);
    defsubr("char>=?",(int)f_char_eqgreaterp);
    defsubr("char<?",(int)f_char_smallerp);
    defsubr("char<=?",(int)f_char_eqsmallerp);
    defsubr("char-ci=?",(int)f_char_ci_eqp);
    defsubr("char-ci>?",(int)f_char_ci_greaterp);
    defsubr("char-ci>=?",(int)f_char_ci_eqgreaterp);
    defsubr("char-ci<?",(int)f_char_ci_smallerp);
    defsubr("char-ci<=?",(int)f_char_ci_eqsmallerp);
    defsubr("char-alphabetic?",(int)f_char_alphabeticp);
	defsubr("char-numeric?",(int)f_char_numericp);
    defsubr("char-whitespace?",(int)f_char_whitespacep);
    defsubr("char-upper-case?",(int)f_char_upper_casep);
    defsubr("char-lower-case?",(int)f_char_lower_casep);
    defsubr("char->integer",(int)f_char_to_integer);
    defsubr("integer->char",(int)f_integer_to_char);
    defsubr("char-upcase",(int)f_char_upcase);
    defsubr("char-downcase",(int)f_char_downcase);
    defsubr("string-append",(int)f_string_append);
    defsubr("number->string",(int)f_number_to_string);
    defsubr("string->number",(int)f_string_to_number);
    defsubr("string=?",(int)f_string_eqp);
    defsubr("string>?",(int)f_string_greaterp);
    defsubr("string>=?",(int)f_string_eqgreaterp);
    defsubr("string<?",(int)f_string_smallerp);
    defsubr("string<=?",(int)f_string_eqsmallerp);
    defsubr("string-ci=?",(int)f_string_ci_eqp);
    defsubr("string-ci>?",(int)f_string_ci_greaterp);
    defsubr("string-ci>=?",(int)f_string_ci_eqgreaterp);
    defsubr("string-ci<?",(int)f_string_ci_smallerp);
    defsubr("string-ci<=?",(int)f_string_ci_eqsmallerp);
    defsubr("string->symbol",(int)f_string_to_symbol);
    defsubr("symbol->string",(int)f_symbol_to_string);
    defsubr("string-length",(int)f_string_length);
    defsubr("make-string",(int)f_make_string);
    defsubr("string",(int)f_string);
    defsubr("string-ref",(int)f_string_ref);
    defsubr("string-set!",(int)f_string_set);
    defsubr("substring",(int)f_substring);
    defsubr("string->list",(int)f_string_to_list);
    defsubr("list->string",(int)f_list_to_string);
    defsubr("string-copy",(int)f_string_copy);
    defsubr("string-fill!",(int)f_string_fill);
    defsubr("make-vector",(int)f_make_vector);
    defsubr("vector-set!",(int)f_vector_set);
    defsubr("vector",(int)f_vector);
    defsubr("vector-ref",(int)f_vector_ref);
    defsubr("vector-length",(int)f_vector_length);
    defsubr("vector-fill!",(int)f_vector_fill);
    defsubr("vector->list",(int)f_vector_to_list);
    defsubr("list->vector",(int)f_list_to_vector);
    defsubr("read",(int)f_read);
    defsubr("load",(int)f_load);
    defsubr("open-input-file",(int)f_open_input_file);
    defsubr("open-output-file",(int)f_open_output_file);
    defsubr("close-input-port",(int)f_close_input_port);
    defsubr("close-output-port",(int)f_close_output_port);
    defsubr("eof-object?",(int)f_eof_objectp);
    defsubr("input-port?",(int)f_input_portp);
    defsubr("output-port?",(int)f_output_portp);
    defsubr("current-input-port",(int)f_current_input_port);
    defsubr("current-output-port",(int)f_current_output_port);
    defsubr("read-char",(int)f_read_char);
    defsubr("peek-char",(int)f_peek_char);
    defsubr("read-line",(int)f_read_line);
    defsubr("read-string",(int)f_read_string);
    defsubr("char-ready?",(int)f_char_readyp);
    defsubr("lambda/asm",(int)f_lambda_asm);
    defsubr("exit",(int)f_exit);
    defsubr("gensym",(int)f_gensym);
	defsubr("apply",(int)f_apply);
    defsubr("primitive-name?",(int)f_primitive_name_p);
    defsubr("macroexpand-1",(int)f_macroexpand1);
    defsubr("macroexpand",(int)f_macroexpand);
    defsubr("addr",(int)f_addr);
    defsubr("entity-addr",(int)f_entity_addr);
    defsubr("undefined",(int)f_undefined);
    defsubr("step",(int)f_step);
    defsubr("vm2-step",(int)f_vm2_step);
    defsubr("vm1",(int)f_vm1);
    defsubr("vm2",(int)f_vm2);
    defsubr("dump",(int)f_dump);
    defsubr("addr-prt",(int)f_addr_prt);
    defsubr("gbc",(int)f_gbc);
    defsubr("room",(int)f_room);
    defsubr("freecell",(int)f_freecell);
    defsubr("sys-code",(int)f_vmcode);
    defsubr("sys-env",(int)f_env);
    defsubr("sys-timer-set",(int)f_timer_set);
    defsubr("sys-timer-get",(int)f_timer_get);
    defsubr("sys-timer-gbc",(int)f_timer_gbc);
    defsubr("current-second",(int)f_current_second);
    defsubr("current-jiffy",(int)f_current_jiffy);
    defsubr("jiffies-per-second",(int)f_jiffies_per_second);
    defsubr("eval",(int)f_eval);
    defsubr("error",(int)f_error);
    defsubr("flush",(int)f_flush);
    defsubr("sys-set-trace",(int)f_set_trace);
    defsubr("sys-set-untrace",(int)f_set_untrace);
    defsubr("debug",(int)f_debug);
    defsubr("profiler",(int)f_prof);
    defsubr("current-module",(int)f_current_module);
    defsubr("values",(int)f_values);
	defsubr("sys-cont-room",(int)f_sys_cont_room);
    defsubr("make-syntactic-closure",(int)f_make_syntactic_closure);
    defsubr("syntactic-closure-expr",(int)f_syntactic_closure_expr);
    defsubr("syntactic-closure-env",(int)f_syntactic_closure_env);
    defsubr("syntactic-closure-freevar",(int)f_syntactic_closure_freevar);
    defsubr("symbol->identifier",(int)f_symbol_to_identifier);
    defsubr("identifier->symbol",(int)f_identifier_to_symbol);
    defsubr("syntactic-closure?",(int)f_syntactic_closurep);
    defsubr("identifier?",(int)f_identifierp);
    defsubr("identifier-bind!",(int)f_identifier_bind);
    defsubr("identifier-free?",(int)f_identifier_freep);
    defsubr("identifier-bound?",(int)f_identifier_boundp);
    defsubr("identifier-bound",(int)f_identifier_bound);
    defsubr("identifier-variable!",(int)f_identifier_variable);
    defsubr("identifier-variable?",(int)f_identifier_variablep);
    defsubr("identifier-ellipsis!",(int)f_identifier_ellipsis);
    defsubr("identifier-ellipsis?",(int)f_identifier_ellipsisp);
    defsubr("global-bound?",(int)f_global_boundp);
    defsubr("inspect",(int)f_inspect);
    defsubr("exact-integer?",(int)f_exact_integerp);
    defsubr("file-exists?",(int)f_file_existsp);
    defsubr("system",(int)f_system);
    defsubr("infinity?",(int)f_infinityp);
    defsubr("finity?",(int)f_finityp);
    defsubr("nan?",(int)f_nanp);
    defsubr("square",(int)f_square);
    defsubr("bytevector?",(int)f_bytevectorp);
    defsubr("make-bytevector",(int)f_make_bytevector);
    defsubr("bytevector",(int)f_bytevector);
    defsubr("bytevector-u8-set!",(int)f_bytevector_u8_set);
    defsubr("bytevector-u8-ref",(int)f_bytevector_u8_ref);
    defsubr("bytevector-copy",(int)f_bytevector_copy);
	defsubr("bytevector-copy!",(int)f_bytevector_copy2);
    defsubr("bytevector-append",(int)f_bytevector_append);
    defsubr("command-line",(int)f_command_line);
    defsubr("get-environment-variable",(int)f_get_environment_variable);
	defsubr("get-environment-variables",(int)f_get_environment_variables);
    defsubr("get-car",(int)f_get_car);
    defsubr("make-record",(int)f_make_record);
    defsubr("record?",(int)f_recordp);
    defsubr("record-set!",(int)f_record_set);
    defsubr("record-ref",(int)f_record_ref);
    defsubr("sleep",(int)f_sleep);
    
}

void initsyntax(void){
	defsyntax("quote");
    defsyntax("begin");
    defsyntax("set!");
	defsyntax("if");
	defsyntax("lambda");
    defsyntax("define");
    defsyntax("define-macro");
    defsyntax("define-syntax");
    defsyntax("define-library");
    defsyntax("export");
    defsyntax("import");
    defsyntax("syntax-rules");
    defsyntax("syntax-error");
}