#include <setjmp.h>
#include <stdlib.h>
#include <string.h>
#include "norm.h"

int tolower(int c);


extern cell memory[];
extern int emem1[];
extern jmp_buf toplevel;
extern int pc;
extern int sp;
extern int env;
extern int head;
extern int tail;
extern int code[];
extern int stack[];
extern int code_pointer_end;
extern int code_pointer[1000][2];
extern int module_table[100][2];
extern int module_table_end;
extern int current_module;
extern int cont_count;



//---------cell initilize--------------------
void initcell(void){
	int addr,x,y;
    
    for(addr=0; addr <= CELLSIZE; addr++){
    	memory[addr].flag = FRE;
        SET_CDR(addr,addr+1);
    }
    
    cell_heap_p = 0;
    cell_free = CELLSIZE;
    
    for(x=0; x<HASHTBSIZE; x++)
    	for(y=0; y<MODULESIZE; y++) 
    		cell_hash_table[x][y] = NIL;
    
    //0address=nil, 1address=#t, 2address=#f
    make_NIL();
    make_bool("#t");
    make_bool("#f");
    make_inf("+inf.0");
    make_inf("-inf.0");
    make_nan("+nan.0");
    make_nan("-nan.0");
    undef	= make_sym("undef");
	end_of_file = make_sym("end_of_file");
    SET_TAG(end_of_file,EOFO);
	quote	= make_sym("quote");
    quasiquote = make_sym("quasiquote");
    unquote	= make_sym("unquote");
    unquote_splicing = make_sym("unquote-splicing");
    empty_set = make_empty_set(); //空集合　多値で利用する。
}

void initmodule(void){
	
    module_table[0][0] = list2(make_sym("normal"),make_sym("user"));
    module_table[1][0] = list2(make_sym("normal"),make_sym("system"));
    module_table[2][0] = list2(make_sym("normal"),make_sym("compile"));
    module_table[3][0] = list2(make_sym("scheme"),make_sym("base"));
    module_table[4][0] = list2(make_sym("scheme"),make_sym("inexact"));
    module_table[5][0] = list2(make_sym("scheme"),make_sym("complex"));
	module_table[6][0] = list2(make_sym("scheme"),make_sym("divistion"));
    module_table[7][0] = list2(make_sym("scheme"),make_sym("lazy"));
    module_table[8][0] = list2(make_sym("scheme"),make_sym("case-lambda"));
    module_table[9][0] = list2(make_sym("scheme"),make_sym("eval"));
    module_table[10][0] = list2(make_sym("scheme"),make_sym("repl"));
    module_table[11][0] = list2(make_sym("scheme"),make_sym("process-context"));
    module_table[12][0] = list2(make_sym("scheme"),make_sym("load"));
    module_table[13][0] = list2(make_sym("scheme"),make_sym("file"));
    module_table[14][0] = list2(make_sym("scheme"),make_sym("read"));
    module_table[15][0] = list2(make_sym("scheme"),make_sym("write"));
    module_table[16][0] = list2(make_sym("scheme"),make_sym("char"));
    module_table[17][0] = list3(make_sym("scheme"),make_sym("char"),make_sym("normalization"));
    module_table[18][0] = list2(make_sym("scheme"),make_sym("time"));
    module_table[19][0] = list2(make_sym("scheme"),make_sym("r5rs"));
    
    module_table_end = 19;
}


int freshcell(void){
	int res;
    
    res = cell_heap_p;
    cell_heap_p =GET_CDR(cell_heap_p);
    SET_CDR(res,0);
    cell_free--;
    
    if(cell_free == 0)
    	exception("", LACK_CELL, NIL);
        
    return(res);
}

int get_tag(int addr){
	if(IS_INTEGER(addr))
    	return(INTN);
    else
    	return(memory[addr].tag);
}

int make_NIL(void){
	int addr;
    
    addr = freshcell();
    SET_TAG(addr,BIGBANG);
    return(addr);
}

int make_bool(char *name){
	int addr;
    
    addr = freshcell();
    SET_TAG(addr,BOL);
    SET_NAME(addr,name);
    return(addr);
}

int returnbool(char *name){
	if(name[1] == 't' || name[1] == 'T')
    	return(BOOLT);
    if(name[1] == 'f' || name[1] == 'F')
    	return(BOOLF);
    return(undef);
}

int returninf(char *name){
	if(name[0] == '+')
    	return(PINF);
    if(name[0] == '-')
    	return(MINF);
	
    return(undef);    
}

int returnnan(char *name){
	if(name[0] == '+')
    	return(PNAN);
    if(name[0] == '-')
    	return(MNAN);

    return(undef);
}

int make_inf(char *name){
	int addr;
    
    addr = freshcell();
    SET_TAG(addr,INF);
    SET_NAME(addr,name);
    return(addr);
}

int make_nan(char *name){
	int addr;
    
    addr = freshcell();
    SET_TAG(addr,NANN);
    SET_NAME(addr,name);
    return(addr);
}

int make_int(int intn){
	int addr;
    
	if(intn < 0)
    	return(intn);
    else{
    	addr = INT_FLAG | intn;
        return(addr);
    }
}

int addr_to_int(int addr){
	if(addr < 0)
    	return(addr);
    else
        return(INT_MASK & addr);
}


//bignum -999999999~999999999
int make_big(char *bignum){
	char integer[15];
    int i,j,res,sign;
    

    i = 0;
    while(1){
    	if(bignum[i] == NUL)
        	return(make_int(0));
    	else
        if(bignum[i] != '0')
        	break;
        else
        	i++;
    }
    
    if(bignum[0] == '-')
    	sign = -1;
    else
    	sign = 1;
        
    res = make_NIL();
    i = laststr(bignum);
    while(i >= 0){
    	if(i > 9){
        	for(j=8; j>=0; j--){
            	integer[j] = bignum[i];
                i--;
        	}
            integer[9] = NUL;
        	res = cons(make_int(atoi(integer)*sign),res);
        }
        else{
        	if(bignum[0] == '+' || bignum[0] == '-'){
            	integer[i+1] = NUL;
            	while(i >= 0){
                	integer[i] = bignum[i];
                    i--;
                }
                res = cons(make_int(atoi(integer)),res);
            }
            else
            if(i == 9){
            	for(j=8; j>=0; j--){
                	integer[j] =bignum[i];
                    i--;
                }
                integer[9] = NUL;
                res = cons(make_int(atoi(integer)*sign),res);
            }
            else{
            	integer[i+1] = NUL;
            	while(i >= 0){
                	integer[i] = bignum[i];
                    i--;
                }
                res = cons(make_int(atoi(integer)*sign),res);
            }
        }	
    }
    res = reverse2(res);
    SET_TAG(res,BIG);
    return(res);    
}


int make_flt(double floatn){
	int addr;
    
    addr = freshcell();
    SET_TAG(addr,FLTN);
    SET_FLT(addr,floatn);
    return(addr);
}

int norm_rat(int x){
	int n,m,u;
    
    n = GET_CAR(x);
    m = GET_CDR(x);
    
    if(n == 0)
    	return(make_int(0));
        	
    u = int_gcd(abs(n),abs(m));
    n = n/u;
    m = m/u;
    if((n % m) == 0){
    	return(make_int(n/m));
    }
    else{	
    	SET_CAR(x,n);
    	SET_CDR(x,m);
        return(x);
    }
}


int make_rat(int n, int d){
	int addr;
    
    addr = freshcell();
    SET_TAG(addr,RAT);
    SET_CAR(addr,n);
    SET_CDR(addr,d);
    return(norm_rat(addr));
}

void norm_comp(int x){
	double r;
	
	if(GET_IMAG_FLT(x) == (double)0){
    	r = GET_REAL_FLT(x);
        SET_TAG(x,FLTN);
        SET_FLT(x,r);
    }       
}


int make_comp(double real, double imag){
	int addr;
    
    addr = freshcell();
    SET_TAG(addr,COMP);
    SET_CAR(addr,make_flt(real));
    SET_CDR(addr,make_flt(imag));
    norm_comp(addr);
    return(addr);
}

//inf,nanがある場合に使用する。
int make_comp1(int x, int y){
	int addr;
    
    addr = freshcell();
    SET_TAG(addr,COMP);
    SET_CAR(addr,x);
    SET_CDR(addr,y);
    return(addr);
}

int hash(char *name){
	int res;
    
    res = 0;
    while(*name != NUL){
    	res = res + (int)*name;
        name++;
    }
    return(res % HASHTBSIZE);
}

int findsym(char *name, int index){
	int addr;
    
    addr = cell_hash_table[index][current_module];
    
    while(addr != NIL){
    	if(strcmp(name,GET_NAME(car(addr))) == 0)
        	return(car(addr));
        else
        	addr = cdr(addr);
    }
    return(0);
}

int addsym(char *name, int index){
	int addr,res;
    
    addr = cell_hash_table[index][current_module];
    addr = cons(res=make_sym1(name),addr);
    cell_hash_table[index][current_module] = addr;
    return(res);
}



int make_sym(char *name){
	int index,res;
    
    index = hash(name);
    if((res=findsym(name, index)) != 0)
    	return(res);
    else
    	return(addsym(name,index));
}

int make_sym1(char *name){
	int addr;
    
    addr = freshcell();
    SET_TAG(addr,SYM);
    SET_NAME(addr,name);
    SET_CAR(addr,undef);
    SET_CDR(addr,0);
    SET_AUX(addr,-1);
    return(addr);
}

int make_clo(void){
	int addr;
    
    addr = freshcell();
    SET_TAG(addr,CLOS);
    SET_AUX(addr,-1);
    return(addr);
}

int make_macro(void){
	int addr;
    
    addr = freshcell();
    SET_TAG(addr,MAC);
    return(addr);
}

int make_hygienic(void){
	int addr;
    
    addr = freshcell();
    SET_TAG(addr,HYG);
    return(addr);
}

int make_synclo(char *name){
	int addr;
    
    addr = freshcell();
    SET_TAG(addr,SYNT);
    SET_NAME(addr,name);
    return(addr);
}

int make_cont(void){
	int addr;
    
    addr = freshcell();
    SET_TAG(addr,HCONT);
    return(addr);
}

int make_str(char *name){
	int addr;
    
    addr = freshcell();
    SET_TAG(addr,STR);
    SET_NAME(addr,name);
    return(addr);
}

int make_char(char *name){
	int addr,pos;
    char c, low_name[SYMSIZE];
	
    c = NUL;
    pos = 0;
    while(name[pos] != NUL){
    	low_name[pos] = tolower(name[pos]);
    	pos++;
    }
    low_name[pos] = NUL;
    
    if(strcmp(low_name,"space") == 0)
    	c = SPACE;
    else
    if(strcmp(low_name,"return") == 0)
    	c = RET;
    else
    if(strcmp(low_name,"newline") == 0)
    	c = EOL;
    else
    if(strcmp(low_name,"tab") == 0)
    	c = TAB;
    else 
    	if(strlen(name) != 1)
        	exception("make_char", NOT_CHAR, make_str(name));
        else
        	c = name[0];
    
    addr = freshcell();
    SET_TAG(addr,CHR);
    SET_CHAR(addr,c);
    return(addr);
}

//type 0=input_port, 1=output_port
int make_port(FILE *port, int type){
	int addr;
    
    addr = freshcell();
    SET_TAG(addr,PRT);
    SET_PORT(addr,port);
	SET_CDR(addr,type);
    return(addr);
}

int make_vector(int n, int obj){
	int res,i, *vec;
    
    res = freshcell();
	vec = (int *)malloc(sizeof(int)*n);
    if(vec == NULL)
    	exception("make_vector", MALLOC_OVERF, NIL);
    else
    	SET_VEC(res,vec);
    for(i=0; i<n; i++)
    	SET_VEC_ELT(res,i,obj);
    SET_TAG(res,VEC);
    SET_CDR(res,n);
    return(res);
}

int make_u8vector(int n, unsigned char obj){
	int res,i;
    unsigned char *u8vec;
    
    res = freshcell();
	u8vec = (unsigned char *)malloc(sizeof(unsigned char)*n);
    if(u8vec == NULL)
    	exception("make_u8vector", MALLOC_OVERF, NIL);
    else
    	SET_U8VEC(res,u8vec);
    for(i=0; i<n; i++)
    	SET_U8VEC_ELT(res,i,obj);
    SET_TAG(res,U8VEC);
    SET_CDR(res,n);
    return(res);
}

void vector_set(int v, int n, int obj){
	
    SET_VEC_ELT(v,n,obj);
}

void u8vector_set(int v, int n, unsigned char obj){
	
    SET_U8VEC_ELT(v,n,obj);
}

int vector_ref(int v, int n){
	
    return(GET_VEC_ELT(v,n));
}

unsigned char u8vector_ref(int v, int n){
	
    return(GET_U8VEC_ELT(v,n));
}

int vector_length(int v){
	
    return(GET_CDR(v));
}

int vector(int lis){
	int len,i,res;
    
    len = length(lis);
    i = 0;
    res = make_vector(len,undef);
    while(!nullp(lis)){
    	vector_set(res,i,car(lis));
        i++;
        lis = cdr(lis);
    }
    
    return(res);
}

int u8vector(int lis){
	int len,i,elt,res;
    unsigned char v;
    
    len = length(lis);
    i = 0;
    v = 0;
    res = make_u8vector(len,undef);
    while(!nullp(lis)){
    	elt = car(lis);
        if(integerp(elt))
        	v = (unsigned char)get_int(elt);
        else if(charp(elt))
        	v = (unsigned char)GET_CHAR(elt);
        else
        	exception("make_u8vector", ILLEGAL_ARGUMENT, elt);
		
    	u8vector_set(res,i,v);
        i++;
        lis = cdr(lis);
    }
    return(res);
}

int make_ident(char *name){
	int addr;
    
    addr = freshcell();
    SET_TAG(addr,IDNT);
    SET_NAME(addr,name);
    SET_AUX(addr,NIL);
    return(addr);
}


//動的にm行*n列　行列を生成する。要素は０から始まることに注意。
int make_env(int m, int n){
	int addr,x;
    
    addr = freshcell();
    SET_TAG(addr,ENV);
    if(m == 0 && n == 0)
    	goto exit;
            
    x = n + 1;
    SET_CAR(addr,ealloc(x));
       	
    exit:
    SET_CDR(addr,m);
    SET_AUX(addr,n);   
    return(addr);
}

int make_code(int i){
	int addr, *vec;
    
    addr = freshcell();
    SET_TAG(addr,CODE);
	vec = (int *)malloc(sizeof(int)*i);
    if(vec == NULL)
    	exception("make_code", MALLOC_OVERF, NIL);
    else
    	SET_VEC(addr,vec);
        
	SET_CDR(addr,i);
	return(addr);
}

int make_stack(void){
	int addr,i, *vec;
    
    //スタックの上２つは除いて保存する。環境と戻りアドレス。
    addr = freshcell();
    SET_TAG(addr,STACK);
	vec = (int *)malloc(sizeof(int)*(sp - 2));
    if(vec == NULL)
    	exception("make_stack", MALLOC_OVERF, NIL);
    else
    	SET_VEC(addr,vec);
        
	for(i=0; i<(sp - 2); i++)
    	SET_VEC_ELT(addr,i,stack[i]);
	SET_CDR(addr,sp-2);
    return(addr);
}

int make_memory(void){
	int addr,i,j,*vec,end;
    
    /*
    継続用のmemoryオブジェクトに割り当てられた動的配列が1千万を超えたときに
    GCを起動する。(ctak 10 5 0)では正常に動くが(ctak 12 6 0)では動作しない。
    おそらくmallocの内部的な問題ではないかと思う。
    */
    if(cont_count > 10000000){
		gbc();
    }
    
    addr = freshcell();
    SET_TAG(addr,MEM);
    end = code_pointer[0][1];
    vec = (int *)malloc(sizeof(int)*(4 + (code_pointer_end * 2) + end));
	cont_count = cont_count + 4 + (code_pointer_end * 2) + end;
    if(vec == NULL)
    	exception("make_memory", MALLOC_OVERF, NIL);
    else
    	SET_VEC(addr,vec);
    
    SET_VEC_ELT(addr,0,SECOND_STACK);
    SET_VEC_ELT(addr,1,code_pointer_end);
    SET_VEC_ELT(addr,2,head);
    SET_VEC_ELT(addr,3,tail);
    j = 4;
    for(i=0; i<code_pointer_end; i++){
    	SET_VEC_ELT(addr,j,code_pointer[i][0]);	
    	SET_VEC_ELT(addr,j+1,code_pointer[i][1]);
        j = j + 2;
    }
    for(i=0; i<end; i++){
    	SET_VEC_ELT(addr,j,code[i]);
        j++;
    }
    cont_count++;
    //printf("%d\n", cont_count);
	
    return(addr);
}

int get_int(int x){
	return(GET_INT(x));
}

int make_multiple_values(int lis){
	int addr,n,i,v, *vec;
    
    n = length(lis);
    addr = freshcell();
    SET_TAG(addr,MUL);
    vec = (int *)malloc(sizeof(int)*(1 + n));
    if(vec == NULL)
    	exception("make_multiple_values", MALLOC_OVERF, NIL);
    else
    	SET_VEC(addr,vec);
    
    SET_VEC_ELT(addr,0,n);
    for(i=1; i<=n; i++){
    	v = car(lis);
    	SET_VEC_ELT(addr,i,v);
        lis = cdr(lis);
    }
    return(addr);
}

int make_empty_set(void){
	int addr;
    
    addr = freshcell();
    SET_TAG(addr,EMPSET);
    return(addr);
}



//----------operate environment------------------------

int get_lvar(int i, int j){
	int orgenv;
    
    orgenv = env;
    while(i >= 0){
    	if(i == 0)
        	return(GET_ENV_VEC_ELT(orgenv,j));
        else{
        	orgenv = GET_ENV_ORG(orgenv);
            i--;	
        }
    }
    return(undef);
}


void set_lvar(int i, int j, int val){
	int orgenv;
	
    orgenv = env;
    while(i >= 0){
		if(i == 0){
        	SET_ENV_VEC_ELT(orgenv,j,val);
            return;
        }
        else{
        	orgenv = GET_ENV_ORG(orgenv);
            i--;
        }
    }
}

//シンボルをカレントモジュールでリメイクしつつコピーする。
int remake(int x){
	
    if(nullp(x))
    	return(x);
    else if(symbolp(x))
    	return(make_sym(GET_NAME(x)));
    else if(atomp(x))
    	return(x);
    else if(vectorp(x))
    	return(x);
    else
    	return(cons(remake(car(x)),remake(cdr(x))));
}