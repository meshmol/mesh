// Normal Scheme compiler

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <setjmp.h>
#include <windows.h>
#include <signal.h>
#include <math.h>
#include <time.h>
#include "norm.h"


cell memory[CELLSIZE];
token stok = {GO,OTHER};
jmp_buf toplevel;
int asmflag = 0;
int gbcflag = 0;
int loadflag = 0;
int profflag = 0;
int stepflag = 0;
int contflag = 0;
int exitflag = 0;
int initflag = 1;
int debugflag = 0;
int safeflag = 0;
int caseflag = 0; //no-fold-case default
int step_on = 0;
int prof_on = 0;
clock_t gctime;
clock_t start_time;
clock_t end_time;
FILE *input_port;
FILE *output_port;
int genint = 1;

int pc; // program counter
int sp; //stack pointer
int env; //environment
int n_args; 
int head; //head of code address
int tail; //tail ofcode address 
int code[CODESIZE];
int stack[STACKSIZE];
int emem1[ENVSIZE];
int emem2[ENVSIZE2];
int code_pointer[CLOSIZE][2]; 
int code_pointer_end = 0;
int trace_list = NIL; //closures to trace
int current_module = 2; //(normal compile)
int module_table[MODULESIZE][2]; //[0]= symbol, [1]=export_list
int module_table_end;
int export_id;
int export_sym;
int export_rename;
int back_trace[TRACE_DEPTH][2];
int back_trace_end = 0;
int cont_count = 0;


//------vm2起動時のデータ保存用------

int s_pc; // program counter
int s_sp; //stack pointer
int s_env; //environment
int s_n_args; 
int s_head; //head of code address
int s_tail; //tail ofcode address 
int s_code[CODESIZE];
int s_stack[STACKSIZE];
int s_code_pointer[CLOSIZE][2]; 
int s_code_pointer_end = 0;

//------main-----------------------
int main( int argc, char *argv[] ){
	int addr,sexp,i;
    char szPath[128],szDrive[4],szDir[128],szFileName[32],szExt[4];
    char initfile[128],compfile[128],normfile[128],kidsfile[128];
    char *p;
	

    printf("Scheme compiler Normal Ver 2013.5.11 (written by Kenichi.Sasagawa)\n");
    initcell();
    initsubr();
    initsyntax();
    initmodule();
    init_r7rs();
    SetConsoleCtrlHandler(NULL, FALSE ); //CTRL+Cを有効にする。
    SetConsoleCtrlHandler(CtrlHandler, TRUE );
    //初期化ファイルのフルpathを生成
    memset(szPath,0,sizeof(szPath));
	memset(szDrive,0,sizeof(szDrive));
	memset(szDir,0,sizeof(szDir));
	memset(szExt,0,sizeof(szExt));
    memset(initfile,0,sizeof(initfile));
    memset(compfile,0,sizeof(compfile));
    memset(normfile,0,sizeof(normfile));
    memset(kidsfile,0,sizeof(kidsfile));
    GetModuleFileName(NULL, szPath, sizeof(szPath));
	_splitpath(szPath, szDrive, szDir, szFileName, szExt);
	
    _makepath(initfile,szDrive, szDir, "initlib", "o"); 
    _makepath(compfile,szDrive, szDir, "ncomp", "o");
    for(i=0; i<argc; i++){
    	p=argv[i];
        	if(*p=='-'){  /* オプションの引数 */
        		p++;
                switch(*p){
                	case '2': /* -T(tail compiler2) */
						_makepath(compfile,szDrive, szDir, "xcomp", "o"); 
                    	break;
                	case '1': /* -R(recursive compiler1) */
                    	_makepath(compfile,szDrive, szDir, "ncomp", "o");
                    	break;
                    case 'S': /*セーフモード*/
                    	safeflag = 1;
                        break;
                }
           }
       }
    _makepath(normfile,szDrive, szDir, "normmacs", "o");
    _makepath(kidsfile,szDrive, szDir, "kids", "scm"); 
    
    
    int ret = setjmp(toplevel);
    fflush(stdin);
    
    //----初期ファイルを読み込む-----------
    if(initflag == 1 && safeflag == 0){
    	pc = 0;
        sp = 0;
        tail = 0;
        stepflag = 0;
        back_trace_end = 0;
        push_s(make_str(compfile));
        f_load(1);
		//push_s(make_str(normfile));
        //f_load(1);
		//最低限必要なcompile,assemble,compile-fileを(normal user)にexportする。
        export_id = list3(make_sym("compile"),make_sym("assemble"),make_sym("compile-file"));
        current_module = 0;
        while(!nullp(export_id)){
        		export_sym = car(export_id);
            	SET_CAR(remake(export_sym),GET_CAR(export_sym));
            	export_id = cdr(export_id);
        }
        push_s(make_str(initfile));
        f_load(1);
        //push_s(make_str(kidsfile));
        //f_load(1);
        initflag = 0;
    }    
    
    repl:
	
    if(ret == 0)
    	while(1){
            input_port = stdin;
            output_port = stdout;
            loadflag = 0;
            contflag = 0;
        
            if(!debugflag)
            	printf("norm> ");
            else
            	printf("debug> ");
            fflush(stdout);
            sexp = read();
            
            pc = 0;
        	sp = 0;
            env = make_env(0,0);
			tail = 0;
			gctime = 0;
            if(!safeflag){
            	print(eval(sexp));
            }
            else{
            	if(eqvp(sexp,make_sym("q")))
                	return 0;
            	list_to_code(sexp);
                stepflag = 1;
                print(vm2());
            }
            printf("\n"); fflush(stdout);
		
        }
    else
    	if(ret == 1){
        	ret = 0;
            goto repl;
        }
        else{
    		for(addr=0; addr<= CELLSIZE; addr++)
    			free(memory[addr].name);
			printf("- good bye. -\n");
			return 0;
        }
}
//-----eval-------------------
int eval(int x){
	int arg1,arg2,res,savehead,savetail,savepc,savesp;
    
	savehead = head;
    savetail = tail;
    savepc = pc;
    savesp = sp;
    
	//(set! arg1 (asm (compile x)))相当のVM機械語を生成。
    
    arg1 = cons(make_int(2),
    	cons(x,
         cons(make_int(4),
          cons(make_sym("compile"),
           cons(make_int(13),
             cons(make_int(1),
              cons(make_int(4),
               cons(make_sym("assemble"),
                cons(make_int(13),
                 cons(make_int(1),
                  cons(make_int(1),NIL)))))))))));	
        	
    //アセンブルしてVM機械語を得る。
    list_to_code(arg1);
    pc = head;
    arg2 = vm1();
        
         
    //その機械語をコード領域に展開してVM実行。
    head = savehead;
    tail = savetail;
    list_to_code(arg2);
    pc = head;
    
    if(!debugflag)
    	res = vm1();
    else{
    	back_trace_end = 0;
		if(prof_on)
        	profflag = 1;
        if(step_on)
        	stepflag = 1;
		res = vm2();
        profflag = 0;
        stepflag = 0;
    }
	pc = savepc;
    sp = savesp;  
    return(res);
}


//-----ctrl+C,D---------------
BOOL WINAPI CtrlHandler(DWORD CtrlEvent){
	switch(CtrlEvent){
		case CTRL_C_EVENT:		exitflag = 1;
        						return TRUE;
        case CTRL_BREAK_EVENT:	exitflag = 2;
        						return TRUE;
        default:        		return FALSE;
    }
}


//---------error-----------

void exception(char *fn, int code, int arg){
	int i;
    
	switch(code){
    	case UNBOUND_VARIABLE:	printf("Exception: variable ");
        						print(arg); printf(" is not bound\n");
                                break;
		case CANT_READ:			printf("Exception: expr ");
        						print(arg); printf(" is not readable\n");
                                break;
        case CANT_OPEN:			printf("Exception in %s: ",fn);
        						print(arg); printf(" is not exist\n");
                                break;
        case VM_ILLEGAL_CODE:	printf("Exception in VM :");
        						print(arg); printf(" is illegal op code\n");
                                break;
        case LACK_CELL:			printf("Exception: lack of cell\n");
								gbc();
        						break;
        case LACK_VEC:			printf("Exception: lack of vector\n");
        						break;
    	case NOT_PAIR:			printf("Exception in %s: ", fn);
        						print(arg); printf(" is not pair\n");
                                break;
        case NOT_NUMBER:		printf("Exception in %s: ", fn);
        						print(arg); printf(" is not a number\n");
                                break;
        case NOT_INTEGER:		printf("Exception in %s: ", fn);
        						print(arg); printf(" is not an integer\n");
                                break;
        case NOT_EXACT:			printf("Exception in %s: ", fn);
        						print(arg); printf(" is not an exact nonnegative number\n");
                                break;                        
        case NOT_REAL:			printf("Exception in %s: ", fn);
        						print(arg); printf(" is not a real number\n");
                                break;
        case NOT_SYMBOL:		printf("Exception in %s: ", fn);
        						print(arg); printf(" is not a symbol\n");
                                break;
        case NOT_STRING:		printf("Exception in %s: ", fn);
        						print(arg); printf(" is not a string\n");
                                break;
        case NOT_CHAR:			printf("Exception in %s: ", fn);
        						print(arg); printf(" is not a character\n");
                                break;
        case NOT_PORT:			printf("Exception in %s: ", fn);
        						print(arg); printf(" is not a port\n");
                                break;
        case NOT_PROCEDURE:		printf("Exception in %s: ", fn);
        						print(arg); printf(" is not a procedure\n");
                                break;
        case NOT_CLOSURE:		printf("Exception in %s: ", fn);
        						print(arg); printf(" is not a closure\n");
                                break;
        case NOT_MACRO:			printf("Exception in %s: ", fn);
        						print(arg); printf(" is not a macro\n");
                                break;
        case NOT_HYGIENIC:		printf("Exception in %s: ", fn);
        						print(arg); printf(" is not a hygienic\n");
                                break;
        case NOT_LIST:			printf("Exception in %s: ", fn);
        						print(arg); printf(" is not a list\n");
                                break;
        case NOT_BOOL:			printf("Exception in %s: ", fn);
        						print(arg); printf(" is not a boolean\n");
                                break;
        case NOT_IDENTIFIER:	printf("Exception in %s: ", fn);
        						print(arg); printf(" is not a identifier\n");
                                break;
        case NOT_SYNTACTIC:		printf("Exception in %s: ", fn);
        						print(arg); printf(" is not a syntactic closure\n");
                                break;
        case INCORRECT_ARG_CNT:	printf("Exception: incorrect argument count in call (%s)\n", fn);
        						break;
        case ILLEGAL_ARGUMENT:	printf("Exception in %s: ", fn);
        						print(arg); printf(" is illegal argument\n");
                                break;
        case DIVIDE_ZERO:		printf("Exception in %s: divide by zero\n", fn);
        						break;
        case TOO_BIG:			printf("Exception in %s: ", fn);
        						print(arg); printf(" is too big number\n");
                                break;
        case INVALID_SYNTAX:	printf("Exception: invalid syntax ");
        						print(arg); printf("\n");
                                break;
        case INVALID_APP:		printf("Exception: invalid application ");
        						print(arg); printf("\n");
                                break;
        case EXTRA_PAREN:		printf("Exception: extra close parenthesis\n");
        						break;
        case STACK_OVERF:		printf("Exception: stack overflow\n");
        						break;
        case CODE_OVERF:		printf("Exception: code overflow\n");
        						break;
		case NOT_MODULE:		printf("Exception in %s: not exist module ", fn);
        						print(arg);
								printf("\n");
        						break;
        case MODULE_OVERF:		printf("Exception: module overflow\n");
        						break;
        case CLOSURE_OVERF:		printf("Exception: closure overflow\n");
        						break;
        case ILLEGAL_VMCODE:	printf("Exception: illegal VM code pc=%d \n", pc);
        						break;
        case MALLOC_OVERF:		printf("Exception in %s: memory allocation over flow\n", fn);
        						break;
        case NOT_EXIST_LIB:		printf("Exception in import: not exist library ");
        						print(arg);
                                printf("\n");
        						break;				
    }
    //ステップ実行中止
    stepflag = 0;
    
    //デバッグモードならバックトレースを出力
    if(debugflag){
    	printf("back trace\n");
    	for(i=0; i<back_trace_end; i++){
			printf("[%d] ", i - back_trace_end);
        	print(cons(back_trace[i][0],back_trace[i][1]));
            printf("\n");
        }
    }
    	
    //load中ならクローズしてトップレベルへ復帰。
    if(loadflag == 1)
    	fclose(input_port);
	
    //クロージャに記憶した命令列の開始アドレスを消去。
    for(i=0; i< code_pointer_end; i++){
            SET_AUX(code_pointer[i][0] , -1);
        }
    code_pointer_end = 0;
	
    //モジュールを(normal user)に復帰
    current_module = 0;
    
	longjmp(toplevel,1);
}


//----------VM1----------------------------
int vm1(void){
	int arg,res,clo,m,n,o,i,j,x,y,z,size,clo_code,cont,cont_code,cont_stack,
    	new_env,env_i,env_j,new_env_i,new_env_j,savepc,savesp,saveenv;
    char str[SYMSIZE];
	
	
	static const void *JUMPTABLE[] = 
    {&&CASE_NOP,  //0
    &&CASE_HALT,  //1
    &&CASE_CONST, //2
    &&CASE_LVAR,  //3
    &&CASE_GVAR,  //4
    &&CASE_LSET,  //5
    &&CASE_GSET,  //6
    &&CASE_POP,   //7
    &&CASE_JUMP,  //8
	&&CASE_TJUMP, //9
    &&CASE_FJUMP, //10
    &&CASE_RETURN,//11
    &&CASE_ARGS,  //12
    &&CASE_CALL,  //13
    &&CASE_CALLJ, //14
    &&CASE_FN,    //15
    &&CASE_SAVE,  //16
    &&CASE_PRIM,  //17
    &&CASE_DEF,   //18
    &&CASE_DEFM,  //19
    &&CASE_DEFH,  //20
    &&CASE_NEQP,  //21
    &&CASE_SMLP,  //22
    &&CASE_ESMLP, //23
    &&CASE_GRTP,  //24
    &&CASE_EGRTP, //25
    &&CASE_ZEROP, //26
    &&CASE_ADD1,  //27
    &&CASE_SUB1,  //28
    &&CASE_ADD2,  //29
    &&CASE_SUB2,  //30
    &&CASE_GREF,  //31
    &&CASE_CATCH, //32
    &&CASE_PAUSE, //33
    &&CASE_CAR,   //34
    &&CASE_CDR,   //35
    &&CASE_CONS,  //36
    &&CASE_ADAPT, //37
    &&CASE_DEFLIB,//38
    &&CASE_EXPLIB,//39
    &&CASE_IMPLIB //40
    };
	
	env_i = GET_CDR(env);
    env_j = GET_AUX(env);
    n_args = 0;
    goto *JUMPTABLE[code[pc]];
    
    CASE_NOP:
        pc = pc + 1;
        goto *JUMPTABLE[code[pc]];
    
    CASE_HALT:
    	for(i=0; i< code_pointer_end; i++){
        	SET_AUX(code_pointer[i][0], -1);
        }
        code_pointer_end = 0;
    	return(TOP_STACK);
        
    CASE_CONST:
    	push_s(ARG1);
        pc = pc + 2;
		goto *JUMPTABLE[code[pc]];
        
    CASE_LVAR:
    	if(ARG1 == 0)
    		push_s(GET_ENV_VEC_ELT(env,ARG2));
    	else
        	push_s(get_lvar(ARG1, ARG2));
    
        pc = pc + 3;
        goto *JUMPTABLE[code[pc]];
        
    CASE_GVAR:
    	arg = ARG1;
    	res = GET_CAR(arg);
        
        if(res == undef)
            	exception("", UNBOUND_VARIABLE, arg);
        
        push_s(res);
        
        //クロージャの場合にはgref命令に置換する。
        if(IS_CLOSURE(res)){
        	code[pc] = 31; //(gref addr)
        	code[pc+1] = res;
        }
        
        pc = pc + 2;
        goto *JUMPTABLE[code[pc]];

    CASE_LSET:
		res = POP_S;
        set_lvar(ARG1,ARG2,res);
        push_s(res);
        pc = pc + 3;
        goto *JUMPTABLE[code[pc]];
        
    CASE_GSET:
        x = POP_S;
        arg = ARG1;

		if(GET_CAR(arg) != undef)
    		SET_CAR(arg,x);
    	else
        	exception("", UNBOUND_VARIABLE, arg);
       	
        push_s(x);
        pc = pc + 2;
        goto *JUMPTABLE[code[pc]];
    
    CASE_POP:
    	pop_s();
        pc = pc + 1;
        goto *JUMPTABLE[code[pc]];
        
    CASE_JUMP:
        pc = pc + ARG1;
        goto *JUMPTABLE[code[pc]];
	
    
    CASE_TJUMP:
        if(POP_S != BOOLF)
        	pc = pc + ARG1;
        else
        	pc = pc + 2;
        
        goto *JUMPTABLE[code[pc]];
        	
        
    CASE_FJUMP:
        if(POP_S == BOOLF)
        	pc = pc + ARG1;
        else
        	pc = pc + 2;
        
        goto *JUMPTABLE[code[pc]];
        
    CASE_RETURN:
        pc = THIRD_STACK;
        env = SECOND_STACK;
        stack[sp-3] = stack[sp-1];
        sp = sp - 2;
        goto *JUMPTABLE[code[pc]];
    	
    CASE_ARGS:
    	check_ctrl();
    	m = ARG1;
        n = abs(m);
		
        
		new_env_i = env_i+1;
        if(n > env_j)
        	new_env_j = n;
        else
        	new_env_j = env_j;
        
        new_env = make_env(new_env_i,new_env_j);
		
        
        //コピー元の環境のアドレスを記憶する。
        SET_ENV_ORG(new_env,env);
        
        
        //通常の引数の場合。
        if(m >= 0){
			for(j=n-1; j>=0; j--) 
                SET_ENV_VEC_ELT(new_env, j ,pop_s());        
		}
        //余剰引数の場合。
		else{
        	x = count_stack() - (n-1);//剰余引数の個数
            //余剰引数をリストにまとめる。
			arg = NIL;
            for(i=x; i>0; i--)
            	arg = cons(POP_S,arg);
            SET_ENV_MAT_ELT(new_env, 0 , n-1, arg);
            n--;
            //余剰以外の引数を環境にセットする。
            for(j=n-1; j>=0; j--) 
                SET_ENV_VEC_ELT(new_env, j ,pop_s()); 	
        }
        	
        env = new_env;
        env_i = new_env_i;
        env_j = new_env_j;
        pc = pc + 2;
        goto *JUMPTABLE[code[pc]];
    
    
    
    CASE_CALL:
		check_ctrl();
        n = ARG1;
    	clo = POP_S;
        
		if(closurep(clo)){
        	m = GET_ARGS_CNT(clo);
            if(((m >=0 ) && m != n) ||
               ( m < 0 && n < abs(m)-1))
            	exception(GET_NAME(clo),INCORRECT_ARG_CNT,NIL);
            
        	//まだ展開されていない命令列の場合
			if((x=GET_AUX(clo)) == -1){
                clo_code = GET_CAR(clo);
                size = GET_CDR(clo_code);
                for(i=0; i<size; i++)
                	code[i+tail] = GET_VEC_ELT(clo_code,i);
                head = tail;
                tail = tail+size;
                SET_AUX(clo,head);
                code_pointer[code_pointer_end][0] = clo;
                code_pointer[code_pointer_end][1] = head;
                code_pointer_end++;
                if(code_pointer_end > CLOSIZE)
                	exception("call", CLOSURE_OVERF,NIL);
            	insert_stack(env,pc+2,n);
                env = GET_CDR(clo);
                env_i = GET_CDR(env);
   				env_j = GET_AUX(env);
            	pc = head;
        	}
            //既に展開されている命令列の場合。
        	else{
                i = sp-1;
    			for(j=n; j>0; j--){
    				stack[i+2] = stack[i];
        			i--;
    			}
    			stack[sp-n] = pc+2;
    			stack[sp-n+1] = env;
    			sp = sp + 2; 
				//insert_stack(env,pc+2,n);
                env = GET_CDR(clo);
                env_i = GET_CDR(env);
    			env_j = GET_AUX(env);
            	pc = x;
        	}
        	goto *JUMPTABLE[code[pc]];
        }
        if(continuationp(clo)){
			//継続から渡された引数
            if(n == 0)
            	arg = empty_set;
            else if (n == 1)
            	arg = pop_s();
            else{
            	arg = NIL;
            	for(i=0; i<n; i++)
            		arg = cons(pop_s(),arg);
            }
                
            contflag = 1;
            //メモリ展開情報のクリア
            for(i=0; i< code_pointer_end; i++){
        		SET_AUX(code_pointer[i][0], -1);
        	}
        	code_pointer_end = 0;

 			
            //継続の保持する命令列を復元
        	cont_code = GET_CAR(clo);
            pc = GET_VEC_ELT(cont_code,0); //pcなどを復元
            code_pointer_end = GET_VEC_ELT(cont_code,1);
            head = GET_VEC_ELT(cont_code,2);
            tail = GET_VEC_ELT(cont_code,3);
            
            
            j = 4;
            for(i=0; i<code_pointer_end; i++){
            	code_pointer[i][0] = GET_VEC_ELT(cont_code,j);    //closure
            	code_pointer[i][1] = GET_VEC_ELT(cont_code,j+1);  //start address
				SET_AUX(code_pointer[i][0],code_pointer[i][1]);
                x = code_pointer[i][0];
                clo_code = GET_CAR(x);
                size = GET_CDR(clo_code);
                y = code_pointer[i][1];
                for(z=0; z<size; z++)
                	code[z+y] = GET_VEC_ELT(clo_code,z);   
                j = j + 2;
			}
            y = code_pointer[0][1];
            for(i=0; i<y; i++){
            	code[i] = GET_VEC_ELT(cont_code,j);
                j++;
            }
            
            //継続の保持するスタックを復元
            cont_stack = GET_CDR(clo);
			m = GET_CDR(cont_stack);
            for(i=0; i<m; i++)
            	stack[i] = GET_VEC_ELT(cont_stack,i);
            sp = m;
            
            //継続に渡された引数をスタックトップへ
            if(n == 0 || n == 1)
            	push_s(arg);
            else
            	push_s(make_multiple_values(arg));

			
            //継続の保持する環境を復元
            env = car(GET_AUX(clo));
            current_module = cadr(GET_AUX(clo));
            goto *JUMPTABLE[code[pc]];	
        }
        if(subrp(clo)){
        	pc = pc + 2;
        	push_s(((GET_SUBR(clo))(n)));
            goto *JUMPTABLE[code[pc]];  
        }   
        	
        
    CASE_CALLJ:
		check_ctrl();
        n = ARG1;
    	clo = POP_S;
        
        if((x=GET_AUX(clo)) == -1){
			clo_code = GET_CAR(clo);
            size = GET_CDR(clo_code);
            for(i=0; i<size; i++)
            	code[i+tail] = GET_VEC_ELT(clo_code,i);
            head = tail;
            tail = tail+size;
            SET_AUX(clo,head);
            code_pointer[code_pointer_end][0] = clo;
            code_pointer[code_pointer_end][1] = head;
            code_pointer_end++;
			if(code_pointer_end > CLOSIZE)
            	exception("callj", CLOSURE_OVERF,NIL);
            env = GET_CDR(clo);
            env_i = GET_CDR(env);
    		env_j = GET_AUX(env);
            pc = head;
        }
        else{
        	env = GET_CDR(clo);
            env_i = GET_CDR(env);
    		env_j = GET_AUX(env);
            pc = x;
        }
        goto *JUMPTABLE[code[pc]];  
        
    
    CASE_FN:
        n = ARG1;
        arg = ARG2;
        res = make_clo();
        if(IS_LIST(arg))
        	clo_code = list_to_code_obj(arg);
        else
        	clo_code = arg;
        SET_CAR(res,clo_code);
        SET_CDR(res,env);
        SET_ARGS_CNT(res,n);
        push_s(res);
        pc = pc + 3;
        goto *JUMPTABLE[code[pc]];
        
    CASE_SAVE:
		push_s(ARG1);
    	push_s(env);
        pc = pc + 2;
        goto *JUMPTABLE[code[pc]];
    
    CASE_PRIM:
		check_ctrl();
        x = ARG1;
        if(symbolp(x)){
        	clo = car(x);
            if(clo == undef) exception("", UNBOUND_VARIABLE, x);
        }
        else
        	clo = x;
        n = ARG2;
    	pc = pc + 3;
        push_s(((GET_SUBR(clo))(n)));
    	goto *JUMPTABLE[code[pc]];  
	
    CASE_DEF:
        arg = ARG1;
        if(closurep(TOP_STACK)){
        	clo = TOP_STACK;
            strcpy(str,GET_NAME(arg));
            SET_NAME(clo,str);
        }
        
		SET_CAR(arg,pop_s());
        
        push_s(arg);
        pc = pc + 2;
		goto *JUMPTABLE[code[pc]];
    	
    CASE_DEFM:
        arg = ARG1;
        clo = make_macro();
        SET_CAR(clo,pop_s());
        SET_CAR(arg,clo);
        strcpy(str,GET_NAME(arg));
        SET_NAME(clo,str);
        push_s(arg);
        pc = pc + 2;
    	goto *JUMPTABLE[code[pc]];
        
    CASE_DEFH:
        arg = ARG1; //symbol
        new_env = ARG2; //comp-env
        clo = make_hygienic();
        SET_CAR(clo,pop_s());
        SET_CDR(clo,env); //vm-env
        SET_AUX(clo,new_env);
        SET_CAR(arg,clo);
        strcpy(str,GET_NAME(arg));
        SET_NAME(clo,str);
        push_s(arg);
        pc = pc + 3;
    	goto *JUMPTABLE[code[pc]];
    
    CASE_NEQP:
    	y = pop_s();
        x = pop_s();

    	if(IS_INTEGER(x) && IS_INTEGER(y)){
     		if(x == y)
    			push_s(BOOLT);
        	else
        		push_s(BOOLF);
    	}
		else{
        	if(numeqp(x,y))
            	push_s(BOOLT);
            else
            	push_s(BOOLF);
        }
        
    	pc = pc + 1;
        goto *JUMPTABLE[code[pc]];
    
    CASE_SMLP:
    	y = pop_s();
        x = pop_s();

    	if(IS_INTEGER(x) && IS_INTEGER(y)){
     		if(x < y)
    			push_s(BOOLT);
        	else
        		push_s(BOOLF);
    	}
		else{
        	if(smallerp(x,y))
            	push_s(BOOLT);
            else
            	push_s(BOOLF);
        }
        
    	pc = pc + 1;
        goto *JUMPTABLE[code[pc]];
    
    CASE_ESMLP:
    	y = pop_s();
        x = pop_s();

    	if(IS_INTEGER(x) && IS_INTEGER(y)){
     		if(x <= y)
    			push_s(BOOLT);
        	else
        		push_s(BOOLF);
    	}
		else{
        	if(eqsmallerp(x,y))
            	push_s(BOOLT);
            else
            	push_s(BOOLF);
        }
        
    	pc = pc + 1;
        goto *JUMPTABLE[code[pc]];
        
    
    CASE_GRTP:
    	y = pop_s();
        x = pop_s();

    	if(IS_INTEGER(x) && IS_INTEGER(y)){
     		if(x > y)
    			push_s(BOOLT);
        	else
        		push_s(BOOLF);
    	}
		else{
        	if(greaterp(x,y))
            	push_s(BOOLT);
            else
            	push_s(BOOLF);
        }
        
    	pc = pc + 1;
        goto *JUMPTABLE[code[pc]];
    	
    
    CASE_EGRTP:
    	y = pop_s();
        x = pop_s();

    	if(IS_INTEGER(x) && IS_INTEGER(y)){
     		if(x >= y)
    			push_s(BOOLT);
        	else
        		push_s(BOOLF);
    	}
		else{
        	if(eqgreaterp(x,y))
            	push_s(BOOLT);
            else
            	push_s(BOOLF);
        }
        
    	pc = pc + 1;
        goto *JUMPTABLE[code[pc]];
        
    	
    
    CASE_ZEROP:
        x = pop_s();

    	if(IS_INTEGER(x)){
     		if(get_int(x) == 0)
    			push_s(BOOLT);
        	else
        		push_s(BOOLF);
    	}
		else{
        	if(zerop(x))
            	push_s(BOOLT);
            else
            	push_s(BOOLF);
        }
        
    	pc = pc + 1;
        goto *JUMPTABLE[code[pc]];
    	
        
    CASE_ADD1:
    	arg = pop_s();
    	if(IS_INTEGER(arg)){
    		x = get_int(arg);
    		x++;
    		push_s(make_int(x));
		}
    	else{
    		if(!numberp(arg))
        		exception("add1",NOT_NUMBER,arg);
    		push_s(plus(arg,make_int(1)));
    	}
    	pc = pc + 1;
        goto *JUMPTABLE[code[pc]];
    
    CASE_SUB1:
    	arg = pop_s();
    	if(IS_INTEGER(arg)){
    		x = get_int(arg);
    		x--;
    		push_s(make_int(x));
		}
    	else{
    		if(!numberp(arg))
        		exception("sub1",NOT_NUMBER,arg);
    		push_s(minus(arg,make_int(1)));
    	}
    	pc = pc + 1;
        goto *JUMPTABLE[code[pc]];
    
    CASE_ADD2:
    	arg = pop_s();
    	if(IS_INTEGER(arg)){
    		x = get_int(arg);
    		x = x+2;
    		push_s(make_int(x));
		}
    	else{
    		if(!numberp(arg))
        		exception("add2",NOT_NUMBER,arg);
    		push_s(plus(arg,make_int(2)));
    	}
    	pc = pc + 1;
        goto *JUMPTABLE[code[pc]];
    
    CASE_SUB2:
    	arg = pop_s();
    	if(IS_INTEGER(arg)){
    		x = get_int(arg);
    		x = x-2;
    		push_s(make_int(x));
		}
    	else{
    		if(!numberp(arg))
        		exception("add2",NOT_NUMBER,arg);
    		push_s(minus(arg,make_int(2)));
    	}
    	pc = pc + 1;
        goto *JUMPTABLE[code[pc]];
    
    CASE_GREF:
		push_s(ARG1);
        pc = pc + 2;
        goto *JUMPTABLE[code[pc]];
    
    CASE_CATCH:
    	cont_code = make_memory();
    	cont_stack = make_stack();
        cont = make_cont();
        SET_CAR(cont,cont_code);
        SET_CDR(cont,cont_stack);
        SET_AUX(cont,cons(env,cons(current_module,NIL)));
    	push_s(cont);
        pc = pc + 1;
    	goto *JUMPTABLE[code[pc]];
    
    CASE_PAUSE:
    	return(TOP_STACK);
    
    CASE_CAR:
    	x = pop_s();
        if(pairp(x))
    		push_s(GET_CAR(x));
    	else
    		exception("car", NOT_PAIR, x);
      	pc = pc + 1;
        goto *JUMPTABLE[code[pc]];
    
    CASE_CDR:
        x = pop_s();
        if(pairp(x))
    		push_s(GET_CDR(x));
    	else
    		exception("cdr", NOT_PAIR, x);
      	pc = pc + 1;
        goto *JUMPTABLE[code[pc]];
    
    CASE_CONS:
        y = pop_s();
        x = pop_s();
        push_s(cons(x,y));
      	pc = pc + 1;
        goto *JUMPTABLE[code[pc]];
    
    CASE_ADAPT:
    	arg = pop_s();
    
		if(GET_TAG(arg) == MUL){
    		m = GET_VEC_ELT(arg,0);
    		for(i=1; i<=m; i++){
    			x = GET_VEC_ELT(arg,i);
        	push_s(x);
    		}
    	}
    	else if(arg == empty_set){
    		m = 0;
    	}
    	else{
    		m = 1;
			push_s(arg);
    	}
        
		code[pc + 5] = m;
        pc = pc + 1;
        goto *JUMPTABLE[code[pc]];
    
    CASE_DEFLIB:
    	export_id = NIL;
        export_rename = NIL;
        x = ARG1; //name
        y = ARG2; //body
        for(i=0; i<module_table_end; i++)
        	if(equalp(module_table[i][0],x))
            	goto deflib_exit;
        
        module_table[i][0] = x;
        module_table_end++;
        
        deflib_exit:
        current_module = i;
        y = remake(y);
		
        while(!nullp(y)){
        	current_module = 0;
        	arg = cons(make_int(2),
    		       cons(car(y),
                    cons(make_int(4),
                     cons(make_sym("compile"),
                      cons(make_int(13),
                       cons(make_int(1),
                        cons(make_int(4),
                         cons(make_sym("assemble"),
                          cons(make_int(13),
                           cons(make_int(1),
                            cons(make_int(1),NIL)))))))))));
        
        	savepc = pc;
        	savesp = sp;
        	saveenv = env;
            list_to_code(arg);
            pc = head;
        	m = vm1();
        	list_to_code(m);
            pc = head;
            current_module = i;
        	vm1();
        	pc = savepc;
        	sp = savesp;
        	env = saveenv;
            y = cdr(y);
        }
        while(!nullp(export_rename)){
        	m = car(export_rename);
            n = car(m);
            o = car(cdr(m));
            SET_CAR(o,GET_CAR(n));
            export_id = cons(o,export_id);
            export_rename = cdr(export_rename);
        }
        module_table[i][1] = export_id;
        
		y = module_table[i][1];
        while(!nullp(y)){
        	m = car(y);
        	if(GET_CAR(m) == undef)
            	exception("", UNBOUND_VARIABLE, m);
        	y = cdr(y);
        }
        current_module = 0;
        pc = pc + 3;
        goto *JUMPTABLE[code[pc]];
    
    CASE_EXPLIB:
        x = ARG1; //id-list
        while(!nullp(x)){
        	if(eqp(car(car(x)),make_sym("rename")))
        		export_rename = cons(cdr(car(x)),export_rename);
        	else
        		export_id = cons(car(x),export_id);
            
            x = cdr(x);
        }
        push_s(undef);
        pc = pc + 2;
        goto *JUMPTABLE[code[pc]];
    
    CASE_IMPLIB:
        x = ARG1; //<name idintifier>
        while(!nullp(x)){
        	m = car(x);
        	for(i=0; i<module_table_end; i++)
        		if(equalp(module_table[i][0],m))
            		goto implib_exit;
        
        	exception("", NOT_EXIST_LIB, m);
        
        	implib_exit:
        	y = module_table[i][1]; //export-list
        
        	while(!nullp(y)){
        		n = car(y);
            	SET_CAR(remake(n),GET_CAR(n));
            	y = cdr(y);
        	}
            x = cdr(x);
        }
        push_s(undef);
        pc = pc + 2;
        goto *JUMPTABLE[code[pc]];
   	
}
//----------VM2--------------------------

int vm2(void){
	int arg,res,clo,m,n,o,i,j,x,y,z,size,clo_code,cont,cont_code,cont_stack,
    	new_env,env_i,env_j,new_env_i,new_env_j,trace,level,savepc,savesp,saveenv;
    char str[SYMSIZE];
	prof prof_dt[OPCODE];
    clock_t start,end;
	double total,average;
    
	static const void *JUMPTABLE[] = 
    {&&CASE_NOP,  //0
    &&CASE_HALT,  //1
    &&CASE_CONST, //2
    &&CASE_LVAR,  //3
    &&CASE_GVAR,  //4
    &&CASE_LSET,  //5
    &&CASE_GSET,  //6
    &&CASE_POP,   //7
    &&CASE_JUMP,  //8
	&&CASE_TJUMP, //9
    &&CASE_FJUMP, //10
    &&CASE_RETURN,//11
    &&CASE_ARGS,  //12
    &&CASE_CALL,  //13
    &&CASE_CALLJ, //14
    &&CASE_FN,    //15
    &&CASE_SAVE,  //16
    &&CASE_PRIM,  //17
    &&CASE_DEF,   //18
    &&CASE_DEFM,  //19
    &&CASE_DEFH,  //20
	&&CASE_NEQP,  //21
    &&CASE_SMLP,  //22
    &&CASE_ESMLP, //23
    &&CASE_GRTP,  //24
    &&CASE_EGRTP, //25
    &&CASE_ZEROP, //26
    &&CASE_ADD1,  //27
    &&CASE_SUB1,  //28
    &&CASE_ADD2,  //29
    &&CASE_SUB2,   //30
	&&CASE_GREF,   //31
    &&CASE_CATCH,  //32
	&&CASE_PAUSE,  //33
    &&CASE_CAR,    //34
    &&CASE_CDR,    //35
    &&CASE_CONS,   //36
    &&CASE_ADAPT,  //37
    &&CASE_DEFLIB, //38
    &&CASE_EXPLIB, //39
    &&CASE_IMPLIB  //40
};
    
    prof_dt[0].name = "nop    ";
    prof_dt[1].name = "halt   ";
    prof_dt[2].name = "const  ";
    prof_dt[3].name = "lvar   ";
    prof_dt[4].name = "gvar   ";
    prof_dt[5].name = "lset   ";
    prof_dt[6].name = "gset   ";
    prof_dt[7].name = "pop    ";
    prof_dt[8].name = "jump   ";
    prof_dt[9].name = "tjump  ";
    prof_dt[10].name = "fjump  ";
    prof_dt[11].name = "return ";
    prof_dt[12].name = "args   ";
    prof_dt[13].name = "call   ";
    prof_dt[14].name = "callj  ";
    prof_dt[15].name = "fn     ";
    prof_dt[16].name = "save   ";
    prof_dt[17].name = "prim   ";
    prof_dt[18].name = "def    ";
    prof_dt[19].name = "defm   ";
    prof_dt[20].name = "defh   ";
    prof_dt[21].name = "neqp   ";
    prof_dt[22].name = "smlp   ";
    prof_dt[23].name = "esmlp  ";
    prof_dt[24].name = "grtp   ";
    prof_dt[25].name = "egrtp  ";
    prof_dt[26].name = "zerop  ";
    prof_dt[27].name = "add1   ";
    prof_dt[28].name = "sub1   ";
    prof_dt[29].name = "add2   ";
    prof_dt[30].name = "sub2   ";
	prof_dt[31].name = "gref   ";
    prof_dt[32].name = "catch  ";
    prof_dt[33].name = "pause  ";
    prof_dt[34].name = "car    ";
    prof_dt[35].name = "cdr    ";
    prof_dt[36].name = "cons   ";
    prof_dt[37].name = "adapt  ";
    prof_dt[38].name = "deflib ";
    prof_dt[39].name = "explib ";
    prof_dt[40].name = "implib ";
    
    for(i=0; i<OPCODE; i++){
    	prof_dt[i].count = 0;
        prof_dt[i].time = 0;
    }
	
	env_i = GET_CDR(env);
    env_j = GET_AUX(env);
    n_args = 0;
    trace = 0;
    level = 0;
    
    goto *JUMPTABLE[code[pc]];
    
    CASE_NOP:
		step();
        start = clock();
        pc = pc + 1;
        end = clock();
        prof_dt[0].count++;
        prof_dt[0].time = prof_dt[0].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
    
    CASE_HALT:
    	step();
        start = clock();
        
    	for(i=0; i< code_pointer_end; i++){
            SET_AUX(code_pointer[i][0] , -1);
        }
        code_pointer_end = 0;
        		 
        end = clock();
        prof_dt[1].count++;
        prof_dt[1].time = prof_dt[1].time + (end - start);
        
        if(profflag){
        	printf("inst    total(s) average(s)         count\n");
        	for(i=0; i<OPCODE; i++){
            	total = (double)(double)prof_dt[i].time / CLOCKS_PER_SEC;
                if(prof_dt[i].count != 0)
                	average = total / (double)prof_dt[i].count;
                else
                	average = 0;
        		printf("%s %f %1.16f %d \n", prof_dt[i].name,
                        	          total,
                     				  average,
                                      prof_dt[i].count);
        	}
        }    
    	return(TOP_STACK);
    
    CASE_CONST:
    	step();
        start = clock();
        
    	push_s(ARG1);
        pc = pc + 2;
        
        end = clock();
        prof_dt[2].count++;
        prof_dt[2].time = prof_dt[2].time + (end - start);
        VM_ERR_CHK;
		goto *JUMPTABLE[code[pc]];
        
    CASE_LVAR:
    	step();
        start = clock();
        
		if(ARG1 == 0)
    		push_s(GET_ENV_VEC_ELT(env,ARG2));
    	else
        	push_s(get_lvar(ARG1, ARG2));
    
        pc = pc + 3;

        end = clock();
        prof_dt[3].count++;
        prof_dt[3].time = prof_dt[3].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
        
    CASE_GVAR:
    	step();
        start = clock();
        
    	arg = ARG1;
        if(GET_CDR(arg) == 1)
        	trace = 1;
    	res = GET_CAR(arg);
        
        if(res == undef)
        	exception("", UNBOUND_VARIABLE, arg);
        
		push_s(res);
        
        if(GET_CDR(arg) == 0 && IS_CLOSURE(arg)){
        //トレースがとれなくなるのでトレース中はgrefは使わない。
        	code[pc] = 31; //(gref addr);
        	code[pc+1] = res;
        }
        
        
        pc = pc + 2;
        
        end = clock();
        prof_dt[4].count++;
        prof_dt[4].time = prof_dt[4].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];

    CASE_LSET:
    	step();
        start = clock();
        
		res = POP_S;
        set_lvar(ARG1,ARG2,res);
        push_s(res);
        pc = pc + 3;
        
        end = clock();
        prof_dt[5].count++;
        prof_dt[5].time = prof_dt[5].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
        
    CASE_GSET:
    	step();
        start = clock();
        
        x = POP_S;
        arg = ARG1;
        
        if(GET_CAR(arg) != undef)
    		SET_CAR(arg,x);
    	else
        	exception("", UNBOUND_VARIABLE, arg);
        
        push_s(x);
        pc = pc + 2;
        
        end = clock();
        prof_dt[6].count++;
        prof_dt[6].time = prof_dt[6].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
    
    CASE_POP:
    	step();
        start = clock();
        
    	pop_s();
        pc = pc + 1;
        
        end = clock();
        prof_dt[7].count++;
        prof_dt[7].time = prof_dt[7].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
        
    CASE_JUMP:
    	step();
        start = clock();
        
        pc = pc + ARG1;
        
        end = clock();
        prof_dt[8].count++;
        prof_dt[8].time = prof_dt[8].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
	
    
    CASE_TJUMP:
    	step();
        start = clock();
        
        if(POP_S != BOOLF)
        	pc = pc + ARG1;
        else
        	pc = pc + 2;
        
        end = clock();
        prof_dt[9].count++;
        prof_dt[9].time = prof_dt[9].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
        	
        
    CASE_FJUMP:
    	step();
        start = clock();
        
        if(POP_S == BOOLF)
        	pc = pc + ARG1;
        else{
        	//pc = pc + 2;
            pc++; pc++;
        }
        
        end = clock();
        prof_dt[10].count++;
        prof_dt[10].time = prof_dt[10].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
        
    CASE_RETURN:
    	step();
        start = clock();
        
        if(level > 0){
			level--;
			for(i=0; i<level; i++){
				if(i >= 10){
                	printf("[%d]",level);
                    break;
                }	
            	else if(i % 2 == 0)
                	printf("|");
                else
                	printf(" ");
                }
			print(stack[sp-1]);
			printf("\n");
        }
        pc = THIRD_STACK;
        env = SECOND_STACK;
        stack[sp-3] = stack[sp-1];
        sp = sp - 2;
        
        end = clock();
        prof_dt[11].count++;
        prof_dt[11].time = prof_dt[11].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
    	
    CASE_ARGS:
    	check_ctrl();
    	step();
        start = clock();
        
    	m = ARG1;
        n = abs(m);
		
    	new_env_i = env_i+1;
        if(n > env_j)
        	new_env_j = n;
        else
        	new_env_j = env_j;
        
        new_env = make_env(new_env_i,new_env_j);
		
        
        //コピー元の環境のアドレスを記憶する。
        SET_ENV_ORG(new_env,env);
        
        
        //通常の引数の場合。
        if(m >= 0){
        	for(j=n-1; j>=0; j--) 
                SET_ENV_VEC_ELT(new_env, j ,pop_s());        
		}
        //余剰引数の場合。
		else{
        	x = count_stack() - (n-1);//剰余引数の個数
            //余剰引数をリストにまとめる。
			arg = NIL;
            for(i=x; i>0; i--)
            	arg = cons(POP_S,arg);
            SET_ENV_MAT_ELT(new_env, 0 , n-1, arg);
            n--;
            //余剰以外の引数を環境にセットする。
            for(j=n-1; j>=0; j--)
            	SET_ENV_VEC_ELT(new_env, j ,pop_s());	
        }
        	
        env = new_env;
        env_i = new_env_i;
        env_j = new_env_j;
        pc = pc + 2;
        
        end = clock();
        prof_dt[12].count++;
        prof_dt[12].time = prof_dt[12].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
    
    
    
    CASE_CALL:
    	step();
        
		check_ctrl();        
        n = ARG1;
    	clo = POP_S;
        push_back_trace(clo, stack_to_list(n));
        start = clock();
        
		if(closurep(clo)){
        	if(trace == 1){
                for(i=0; i<=level; i++){
					if(i >= 10){
                    	printf("[%d]", level);
                    	break;
                    }
                	else if(i % 2 == 0)
                    	printf("|");
                    else
                    	printf(" ");
                	}
				printf("(%s", GET_NAME(clo));
				for(i=n; i>0; i--){
                	printf(" ");
                	print(stack[sp-i]);
                    }
                printf(")\n");
                level++;
            	trace = 0;
            }
        	m = GET_ARGS_CNT(clo);
            if(((m >=0 ) && m != n) ||
               ( m < 0 && n < abs(m)-1))
            	exception(GET_NAME(clo),INCORRECT_ARG_CNT,NIL);
            
        	//まだ展開されていない命令列の場合
			if((x=GET_AUX(clo)) == -1){
                clo_code = GET_CAR(clo);
                size = GET_CDR(clo_code);
                for(i=0; i<size; i++)
                	code[i+tail] = GET_VEC_ELT(clo_code,i);
                head = tail;
                tail = tail+size;
                SET_AUX(clo,head);
                code_pointer[code_pointer_end][0] = clo;
                code_pointer[code_pointer_end][1] = head;
                code_pointer_end++;
                if(code_pointer_end > CLOSIZE)
                	exception("call", CLOSURE_OVERF,NIL);
            	insert_stack(env,pc+2,n);
                env = GET_CDR(clo);
                env_j = GET_AUX(env);
				env_i = GET_CDR(env);
            	pc = head;
        	}
            //既に展開されている命令列の場合。
        	else{
                insert_stack(env,pc+2,n);
                env = GET_CDR(clo);
                env_j = GET_AUX(env);
				env_i = GET_CDR(env);
            	pc = x;
        	}
        }
        if(continuationp(clo)){
			//継続から渡された引数
            if(n == 0)
            	arg = empty_set;
            else if (n == 1)
            	arg = pop_s();
            else{
            	arg = NIL;
            	for(i=0; i<n; i++)
            		arg = cons(pop_s(),arg);
            }
                
            contflag = 1;
            //メモリ展開情報のクリア
            for(i=0; i< code_pointer_end; i++){
        		SET_AUX(code_pointer[i][0], -1);
        	}
        	code_pointer_end = 0;
	
            
            //継続の保持する命令列を復元
        	cont_code = GET_CAR(clo);
            pc = GET_VEC_ELT(cont_code,0); //pcなどを復元
            code_pointer_end = GET_VEC_ELT(cont_code,1);
            head = GET_VEC_ELT(cont_code,2);
            tail = GET_VEC_ELT(cont_code,3);
            
            
            j = 4;
            for(i=0; i<code_pointer_end; i++){
            	code_pointer[i][0] = GET_VEC_ELT(cont_code,j);    //closure
            	code_pointer[i][1] = GET_VEC_ELT(cont_code,j+1);  //start address
				SET_AUX(code_pointer[i][0],code_pointer[i][1]);
   			    x = code_pointer[i][0];
                clo_code = GET_CAR(x);
                size = GET_CDR(clo_code);
                y = code_pointer[i][1];
                for(z=0; z<size; z++)
                	code[z+y] = GET_VEC_ELT(clo_code,z);   
                j = j + 2;
			}
            y = code_pointer[0][1];
            for(i=0; i<y; i++){
            	code[i] = GET_VEC_ELT(cont_code,j);
                j++;
            }
            
            //継続の保持するスタックを復元
            cont_stack = GET_CDR(clo);
			m = GET_CDR(cont_stack);
            for(i=0; i<m; i++)
            	stack[i] = GET_VEC_ELT(cont_stack,i);
            sp = m;
            
            //継続に渡された引数をスタックトップへ
            if(n == 0 || n == 1)
            	push_s(arg);
            else
            	push_s(make_multiple_values(arg));
            	
            
            
            //継続の保持する環境を復元
            env = car(GET_AUX(clo));
            current_module = cadr(GET_AUX(clo));
    
        }
        if(subrp(clo)){
        	pc = pc + 2;
        	push_s(((GET_SUBR(clo))(n))); 
        }
        end = clock();
        prof_dt[13].count++;
        prof_dt[13].time = prof_dt[13].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];    
        	
        
    CASE_CALLJ:
    	step();
        
		check_ctrl();
    	n = ARG1;
    	clo = POP_S;
        push_back_trace(clo, stack_to_list(n));
        start = clock();
        
		if(trace == 1){
        	for(i=0; i<=level; i++){
				if(i >= 10){
                	printf("[%d]", level);
                    break;
                }
                else if(i % 2 == 0)
                	printf("|");
                else
                	printf(" ");
            }
			printf("(%s", GET_NAME(clo));
			for(i=n; i>0; i--){
            	printf(" ");
                print(stack[sp-i]);
            }
            printf(")\n");
            trace = 0;
        }
        if((x=GET_AUX(clo)) == -1){
			clo_code = GET_CAR(clo);
            size = GET_CDR(clo_code);
            for(i=0; i<size; i++)
            	code[i+tail] = GET_VEC_ELT(clo_code,i);
            head = tail;
            tail = tail+size;
            SET_AUX(clo,head);
            code_pointer[code_pointer_end][0] = clo;
            code_pointer[code_pointer_end][1] = head;
            code_pointer_end++;
            if(code_pointer_end > CLOSIZE)
            	exception("callj", CLOSURE_OVERF,NIL);
            env = GET_CDR(clo);
            env_j = GET_AUX(env);
			env_i = GET_CDR(env);
            pc = head;
        }
        else{
        	env = GET_CDR(clo);
            env_j = GET_AUX(env);
			env_i = GET_CDR(env);
            pc = x;
        }
        
        end = clock();
        prof_dt[14].count++;
        prof_dt[14].time = prof_dt[14].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];  
        
    
    CASE_FN:
    	step();
        start = clock();
        
        n = ARG1;
        arg = ARG2;
        res = make_clo();
        if(IS_LIST(arg))
        	clo_code = list_to_code_obj(arg);
        else
        	clo_code = arg;
        SET_CAR(res,clo_code);
        SET_CDR(res,env);
        SET_ARGS_CNT(res,n);
        push_s(res);
        pc = pc + 3;
        
        end = clock();
        prof_dt[15].count++;
        prof_dt[15].time = prof_dt[15].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
        
    CASE_SAVE:
    	step();
        start = clock();
        
		push_s(ARG1);
    	push_s(env);
        pc = pc + 2;
        
        end = clock();
        prof_dt[16].count++;
        prof_dt[16].time = prof_dt[16].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
    
    CASE_PRIM:
    	check_ctrl();
    	step();
        start = clock();
        
        x = ARG1;
        if(symbolp(x)){
        	clo = car(x);
            if(clo == undef) exception("", UNBOUND_VARIABLE, x);
        }
        else
        	clo = x;
        
        n = ARG2;
        push_back_trace(clo, stack_to_list(n));
    	pc = pc + 3;
        push_s(((GET_SUBR(clo))(n)));
        
        end = clock();
        prof_dt[17].count++;
        prof_dt[17].time = prof_dt[17].time + (end - start);
        VM_ERR_CHK;
    	goto *JUMPTABLE[code[pc]];  
	
    CASE_DEF:
    	step();
        start = clock();
        
        arg = ARG1;
        if(closurep(TOP_STACK)){
        	clo = TOP_STACK;
            strcpy(str,GET_NAME(arg));
            SET_NAME(clo,str);
        }
        
        SET_CAR(arg,pop_s());
        
        push_s(arg);
        pc = pc + 2;
        
        end = clock();
        prof_dt[18].count++;
        prof_dt[18].time = prof_dt[18].time + (end - start);
        VM_ERR_CHK;
		goto *JUMPTABLE[code[pc]];
    	
    CASE_DEFM:
    	step();
        start = clock();
        
        arg = ARG1;
        clo = make_macro();
        SET_CAR(clo,pop_s());
        SET_CAR(arg,clo);
        strcpy(str,GET_NAME(arg));
        SET_NAME(clo,str);
        push_s(arg);
        pc = pc + 2;
        
        end = clock();
        prof_dt[19].count++;
        prof_dt[19].time = prof_dt[19].time + (end - start);
        VM_ERR_CHK;
    	goto *JUMPTABLE[code[pc]];
        
    CASE_DEFH:
    	step();
        start = clock();
        
        arg = ARG1; //symbol
        new_env = ARG2; //comp-env
        clo = make_hygienic();
        SET_CAR(clo,pop_s());
        SET_CDR(clo,env); //vm-env
        SET_AUX(clo,new_env);
        SET_CAR(arg,clo);
        strcpy(str,GET_NAME(arg));
        SET_NAME(clo,str);
        push_s(arg);
        pc = pc + 3;
        
        end = clock();
        prof_dt[20].count++;
        prof_dt[20].time = prof_dt[20].time + (end - start);
        VM_ERR_CHK;
    	goto *JUMPTABLE[code[pc]];
        
    CASE_NEQP:
    	step();
        start = clock();
        
    	y = pop_s();
        x = pop_s();

    	if(IS_INTEGER(x) && IS_INTEGER(y)){
     		if(x == y)
    			push_s(BOOLT);
        	else
        		push_s(BOOLF);
    	}
		else{
        	if(numeqp(x,y))
            	push_s(BOOLT);
            else
            	push_s(BOOLF);
        }
        
    	pc = pc + 1;
        
        end = clock();
        prof_dt[21].count++;
        prof_dt[21].time = prof_dt[21].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
    
    CASE_SMLP:
    	step();
        start = clock();
        
    	y = pop_s();
        x = pop_s();

    	if(IS_INTEGER(x) && IS_INTEGER(y)){
     		if(x < y)
    			push_s(BOOLT);
        	else
        		push_s(BOOLF);
    	}
		else{
        	if(smallerp(x,y))
            	push_s(BOOLT);
            else
            	push_s(BOOLF);
        }
        
    	pc = pc + 1;
        
        end = clock();
        prof_dt[22].count++;
        prof_dt[22].time = prof_dt[22].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
    
    CASE_ESMLP:
    	step();
        start = clock();
        
    	y = pop_s();
        x = pop_s();

    	if(IS_INTEGER(x) && IS_INTEGER(y)){
     		if(x <= y)
    			push_s(BOOLT);
        	else
        		push_s(BOOLF);
    	}
		else{
        	if(eqsmallerp(x,y))
            	push_s(BOOLT);
            else
            	push_s(BOOLF);
        }
        
    	pc = pc + 1;
        
        end = clock();
        prof_dt[23].count++;
        prof_dt[23].time = prof_dt[23].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
        
    
    CASE_GRTP:
    	step();
        start = clock();
        
    	y = pop_s();
        x = pop_s();

    	if(IS_INTEGER(x) && IS_INTEGER(y)){
     		if(x > y)
    			push_s(BOOLT);
        	else
        		push_s(BOOLF);
    	}
		else{
        	if(greaterp(x,y))
            	push_s(BOOLT);
            else
            	push_s(BOOLF);
        }
        
    	pc = pc + 1;
        
        end = clock();
        prof_dt[24].count++;
        prof_dt[24].time = prof_dt[24].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
    	
    
    CASE_EGRTP:
    	step();
        start = clock();
        
    	y = pop_s();
        x = pop_s();

    	if(IS_INTEGER(x) && IS_INTEGER(y)){
     		if(x >= y)
    			push_s(BOOLT);
        	else
        		push_s(BOOLF);
    	}
		else{
        	if(eqgreaterp(x,y))
            	push_s(BOOLT);
            else
            	push_s(BOOLF);
        }
        
    	pc = pc + 1;
        
        end = clock();
        prof_dt[25].count++;
        prof_dt[25].time = prof_dt[25].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
        
    	
    
    CASE_ZEROP:
    	step();
        start = clock();
        
        x = pop_s();

    	if(IS_INTEGER(x)){
     		if(get_int(x) == 0)
    			push_s(BOOLT);
        	else
        		push_s(BOOLF);
    	}
		else{
        	if(zerop(x))
            	push_s(BOOLT);
            else
            	push_s(BOOLF);
        }
        
    	pc = pc + 1;
        
        end = clock();
        prof_dt[26].count++;
        prof_dt[26].time = prof_dt[26].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
    	
        
    CASE_ADD1:
    	step();
        start = clock();
        
    	arg = pop_s();
    	if(IS_INTEGER(arg)){
    		x = get_int(arg);
    		x++;
    		push_s(make_int(x));
		}
    	else{
    		if(!numberp(arg))
        		exception("add1",NOT_NUMBER,arg);
    		push_s(plus(arg,make_int(1)));
    	}
    	pc = pc + 1;
        
        end = clock();
        prof_dt[27].count++;
        prof_dt[27].time = prof_dt[27].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
    
    CASE_SUB1:
    	step();
        start = clock();
        
    	arg = pop_s();
    	if(IS_INTEGER(arg)){
    		x = get_int(arg);
    		x--;
    		push_s(make_int(x));
		}
    	else{
    		if(!numberp(arg))
        		exception("sub1",NOT_NUMBER,arg);
    		push_s(minus(arg,make_int(1)));
    	}
    	pc = pc + 1;
        
        end = clock();
        prof_dt[28].count++;
        prof_dt[28].time = prof_dt[28].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
    
    CASE_ADD2:
    	step();
        start = clock();
        
    	arg = pop_s();
    	if(IS_INTEGER(arg)){
    		x = get_int(arg);
    		x = x+2;
    		push_s(make_int(x));
		}
    	else{
    		if(!numberp(arg))
        		exception("add2",NOT_NUMBER,arg);
    		push_s(plus(arg,make_int(2)));
    	}
    	pc = pc + 1;
        
        end = clock();
        prof_dt[29].count++;
        prof_dt[29].time = prof_dt[29].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
    
    CASE_SUB2:
    	step();
        start = clock();
        
    	arg = pop_s();
    	if(IS_INTEGER(arg)){
    		x = get_int(arg);
    		x = x-2;
    		push_s(make_int(x));
		}
    	else{
    		if(!numberp(arg))
        		exception("add2",NOT_NUMBER,arg);
    		push_s(minus(arg,make_int(2)));
    	}
    	pc = pc + 1;
        
        end = clock();
        prof_dt[30].count++;
        prof_dt[30].time = prof_dt[30].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
    
    CASE_GREF:
    	step();
        start = clock();
        
        push_s(ARG1);
        pc = pc + 2;
        
        end = clock();
        prof_dt[31].count++;
        prof_dt[31].time = prof_dt[31].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
    
    CASE_CATCH:
    	step();
        start = clock();
    
    	cont_code = make_memory();
    	cont_stack = make_stack();
        cont = make_cont();
		SET_CAR(cont,cont_code);
        SET_CDR(cont,cont_stack);
        SET_AUX(cont,cons(env,cons(current_module,NIL)));
    	push_s(cont);
        pc = pc + 1;
        
        end = clock();
        prof_dt[32].count++;
        prof_dt[32].time = prof_dt[32].time + (end - start);
        VM_ERR_CHK;
    	goto *JUMPTABLE[code[pc]];
    
    CASE_PAUSE:
    	step();
        start = clock();
        
    	end = clock();
        prof_dt[33].count++;
        prof_dt[33].time = prof_dt[33].time + (end - start);
        VM_ERR_CHK;
    	return(TOP_STACK);
    
    CASE_CAR:
    	step();
        start = clock();
        
        x = pop_s();
        if(pairp(x))
    		push_s(GET_CAR(x));
    	else
    		exception("car", NOT_PAIR, x);
      	pc = pc + 1;
        
    	end = clock();
        prof_dt[34].count++;
        prof_dt[34].time = prof_dt[34].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
    
    CASE_CDR:
    	step();
        start = clock();
        
        x = pop_s();
        if(pairp(x))
    		push_s(GET_CDR(x));
    	else
    		exception("cdr", NOT_PAIR, x);
      	pc = pc + 1;
       
    	end = clock();
        prof_dt[35].count++;
        prof_dt[35].time = prof_dt[35].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
    
    CASE_CONS:
    	step();
        start = clock();
        
        y = pop_s();
        x = pop_s();
        push_s(cons(x,y));
      	pc = pc + 1;
        
    	end = clock();
        prof_dt[36].count++;
        prof_dt[36].time = prof_dt[36].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
    
    CASE_ADAPT:
    	step();
        start = clock();
        
    	arg = pop_s();
    
		if(GET_TAG(arg) == MUL){
    		m = GET_VEC_ELT(arg,0);
    		for(i=1; i<=m; i++){
    			x = GET_VEC_ELT(arg,i);
        	push_s(x);
    		}
    	}
    	else if(arg == empty_set){
    		m = 0;
    	}
    	else{
    		m = 1;
			push_s(arg);
    	}
		code[pc + 5] = m;
        pc = pc + 1;
        end = clock();
        prof_dt[37].count++;
        prof_dt[37].time = prof_dt[37].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
        
    CASE_DEFLIB:
     	step();
        start = clock();
        
        export_id = NIL;
        export_rename = NIL;
        x = ARG1; //name
        y = ARG2; //body
        for(i=0; i<module_table_end; i++)
        	if(equalp(module_table[i][0],x))
            	goto deflib_exit;
        
        module_table[i][0] = x;
        module_table_end++;
        
        deflib_exit:
        current_module = i;
        y = remake(y);
		
        while(!nullp(y)){
        	current_module = 0;
        	arg = cons(make_int(2),
    		       cons(car(y),
                    cons(make_int(4),
                     cons(make_sym("compile"),
                      cons(make_int(13),
                       cons(make_int(1),
                        cons(make_int(4),
                         cons(make_sym("assemble"),
                          cons(make_int(13),
                           cons(make_int(1),
                            cons(make_int(1),NIL)))))))))));
        
        	savepc = pc;
        	savesp = sp;
        	saveenv = env;
            list_to_code(arg);
            pc = head;
        	m = vm1();
        	list_to_code(m);
            pc = head;
            current_module = i;
        	vm1();
        	pc = savepc;
        	sp = savesp;
        	env = saveenv;
            y = cdr(y);
        }
        while(!nullp(export_rename)){
        	m = car(export_rename);
            n = car(m);
            o = car(cdr(m));
            SET_CAR(o,GET_CAR(n));
            export_id = cons(o,export_id);
            export_rename = cdr(export_rename);
        }
        module_table[i][1] = export_id;
        
		y = module_table[i][1];
        while(!nullp(y)){
        	m = car(y);
        	if(GET_CAR(m) == undef)
            	exception("", UNBOUND_VARIABLE, m);
        	y = cdr(y);
        }
        current_module = 0;
        pc = pc + 3;
        
        end = clock();
        prof_dt[38].count++;
        prof_dt[38].time = prof_dt[38].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
    
    CASE_EXPLIB:
    	step();
        start = clock();
        
        x = ARG1; //id-list
        while(!nullp(x)){
        	if(eqp(car(car(x)),make_sym("rename")))
        		export_rename = cons(cdr(car(x)),export_rename);
        	else
        		export_id = cons(car(x),export_id);
            
            x = cdr(x);
        }
        push_s(undef);
        pc = pc + 2;
        
        end = clock();
        prof_dt[39].count++;
        prof_dt[39].time = prof_dt[39].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
    
    CASE_IMPLIB:
    	step();
        start = clock();
        
        x = ARG1; //<name idintifier>
        while(!nullp(x)){
        	m = car(x);
        	for(i=0; i<module_table_end; i++)
        		if(equalp(module_table[i][0],m))
            		goto implib_exit;
        
        	exception("", NOT_EXIST_LIB, m);
        
        	implib_exit:
        	y = module_table[i][1]; //export-list
        
        	while(!nullp(y)){
        		n = car(y);
            	SET_CAR(remake(n),GET_CAR(n));
            	y = cdr(y);
        	}
            x = cdr(x);
        }
        push_s(undef);
        pc = pc + 2;
        
        end = clock();
        prof_dt[40].count++;
        prof_dt[40].time = prof_dt[40].time + (end - start);
        VM_ERR_CHK;
        goto *JUMPTABLE[code[pc]];
}


//--------------------------------------
void push_s(int x){
	stack[sp] = x;
    sp++;
    if(sp > STACKSIZE)
     	exception("",STACK_OVERF,NIL);
}

int pop_s(void){
	sp--;
	return(stack[sp]);
}


void insert_stack(int env, int pc, int n){
	int i,j;
    
    i = sp-1;
    for(j=n; j>0; j--){
    	stack[i+2] = stack[i];
        i--;
    }
    stack[sp-n] = pc;
    stack[sp-n+1] = env;
    sp = sp + 2;	
}


void list_to_code(int lis){
    int addr,i;
    
    head = tail;
    addr = tail;
    while(!nullp(lis)){
    	i = get_int(car(lis));
        switch(i){	
			case 1:	code[addr] = i;
            		break;
            case 2:	code[addr] = i;
            		addr++;
                    lis = cdr(lis);
                    code[addr] = car(lis);
                    break;
            case 3:	code[addr] = i;
            		addr++;
                    lis = cdr(lis);
                    code[addr] = get_int(car(lis));
                    addr++;
                    lis = cdr(lis);
                    code[addr] = get_int(car(lis));
                    break;
            case 4:	code[addr] = i;
            		addr++;
                    lis = cdr(lis);
                    code[addr] = car(lis);
                    break;
            case 5:	code[addr] = i;
            		addr++;
                    lis = cdr(lis);
                    code[addr] = get_int(car(lis));
                    addr++;
                    code[addr] = get_int(car(lis));
                    break;
            case 6:	code[addr] = i;
            		addr++;
                    lis = cdr(lis);
                    code[addr] = car(lis);
                    break;
            case 7:	code[addr] = i;
            		break;
            case 8:
            case 9:
            case 10:code[addr] = i;
            		addr++;
                    lis = cdr(lis);
                    code[addr] = get_int(car(lis));
                    break;
            case 11:code[addr] = i;
            		break;
            case 12:code[addr] = i;
            		addr++;
                    lis = cdr(lis);
                    code[addr] = get_int(car(lis));
                    break;
            case 13:
            case 14:code[addr] = i;
            		addr++;
                    lis = cdr(lis);
                    code[addr] = get_int(car(lis));
                    break;
            case 15:code[addr] = i;
            		addr++;
                    lis = cdr(lis);
                    code[addr] = get_int(car(lis));
                    addr++;
                    lis = cdr(lis);
                    code[addr] = car(lis);
                    break;
            case 16:code[addr] = i;
            		addr++;
                    lis = cdr(lis);
                    code[addr] = get_int(car(lis));
                    break;
            case 17:code[addr] = i;
            		addr++;
                    lis = cdr(lis);
                    if(symbolp(car(lis)))
                    	code[addr] = car(lis);
                    else
                    	code[addr] = get_int(car(lis));
                    addr++;
                    lis = cdr(lis);
                    code[addr] = get_int(car(lis));
                    break;
            case 18:
            case 19:code[addr] = i;
            		addr++;
                    lis = cdr(lis);
                    code[addr] = car(lis);
                    break;
            case 20:code[addr] = i;
            		addr++;
                    lis = cdr(lis);
                    code[addr] = car(lis);
                    addr++;
                    lis = cdr(lis);
                    code[addr] = get_int(car(lis));
        			break;
            case 21:
            case 22:
            case 23:
            case 24:
            case 25:
            case 26:
            case 27:
            case 28:
            case 29:
            case 30:code[addr] = i;
                    break;
            case 31:code[addr] = i;
            		addr++;
                    lis = cdr(lis);
                    code[addr] = car(lis);
                    break;
            case 32:code[addr] = i;
                    break;
            case 33:code[addr] = i;
            		break;
            case 34:
            case 35:
            case 36:code[addr] = i;
                    break;
            case 37:code[addr] = i;
            		break;
            case 38:code[addr] = i;
            		addr++;
                    lis = cdr(lis);
                    code[addr] = car(lis);
                    addr++;
                    lis = cdr(lis);
                    code[addr] = car(lis);
                    break;
            case 39:
            case 40:code[addr] = i;
            		addr++;
                    lis = cdr(lis);
                    code[addr] = car(lis);
                    break;  
        }
		addr++;
        if(addr > CODESIZE)
        	exception("",CODE_OVERF,NIL);
        lis = cdr(lis);
    }
    tail = addr;
}


int list_to_code_obj(int lis){
	int i,x,code_obj;

	code_obj = make_code(length(lis));
    i = 0;
    while(!nullp(lis)){
   	x = get_int(car(lis));
    switch(x){	
		case 1:	SET_VEC_ELT(code_obj,i,x);        		
    			break;
    	case 2:	SET_VEC_ELT(code_obj,i,x);
    			i++;
        		lis = cdr(lis);
        		SET_VEC_ELT(code_obj,i,car(lis));
        		break;
    	case 3:	SET_VEC_ELT(code_obj,i,x);
    			i++;
        		lis = cdr(lis);
        		SET_VEC_ELT(code_obj,i,get_int(car(lis)));
        		i++;
        		lis = cdr(lis);
        		SET_VEC_ELT(code_obj,i,get_int(car(lis)));
        		break;
    	case 4:	SET_VEC_ELT(code_obj,i,x);
    			i++;
        		lis = cdr(lis);
        		SET_VEC_ELT(code_obj,i,car(lis));
        		break;
    	case 5:	SET_VEC_ELT(code_obj,i,x);
    			i++;
        		lis = cdr(lis);
        		SET_VEC_ELT(code_obj,i,get_int(car(lis)));
        		i++;
				lis = cdr(lis);
        		SET_VEC_ELT(code_obj,i,get_int(car(lis)));
        		break;
    	case 6:	SET_VEC_ELT(code_obj,i,x);
    			i++;
        		lis = cdr(lis);
        		SET_VEC_ELT(code_obj,i,car(lis));
        		break;
    	case 7:	SET_VEC_ELT(code_obj,i,x);
    			break;
    	case 8:
    	case 9:
   		case 10:SET_VEC_ELT(code_obj,i,x);
    			i++;
        		lis = cdr(lis);
        		SET_VEC_ELT(code_obj,i,get_int(car(lis)));
        		break;
    	case 11:SET_VEC_ELT(code_obj,i,x);
    			break;
    	case 12:SET_VEC_ELT(code_obj,i,x);
    			i++;
        		lis = cdr(lis);
        		SET_VEC_ELT(code_obj,i,get_int(car(lis)));
        		break;
    	case 13:
    	case 14:SET_VEC_ELT(code_obj,i,x);
    			i++;
        		lis = cdr(lis);
        		SET_VEC_ELT(code_obj,i,get_int(car(lis)));
        		break;
    	case 15:SET_VEC_ELT(code_obj,i,x);
    			i++;
        		lis = cdr(lis);
        		SET_VEC_ELT(code_obj,i,get_int(car(lis)));
        		i++;
        		lis = cdr(lis);
       			SET_VEC_ELT(code_obj,i,list_to_code_obj(car(lis)));
        		break;
    	case 16:SET_VEC_ELT(code_obj,i,x);
    			i++;
        		lis = cdr(lis);
        		SET_VEC_ELT(code_obj,i,get_int(car(lis)));
        		break;
    	case 17:SET_VEC_ELT(code_obj,i,x);
    			i++;
        		lis = cdr(lis);
                if(symbolp(car(lis)))
                	SET_VEC_ELT(code_obj,i,car(lis));
                else
        			SET_VEC_ELT(code_obj,i,get_int(car(lis)));
        		i++;
        		lis = cdr(lis);
        		SET_VEC_ELT(code_obj,i,get_int(car(lis)));
        		break;
    	case 18:
    	case 19:SET_VEC_ELT(code_obj,i,x);
    			i++;
            	lis = cdr(lis);
            	SET_VEC_ELT(code_obj,i,car(lis));
            	break;
        case 20:SET_VEC_ELT(code_obj,i,x);
        		i++;
            	lis = cdr(lis);
            	SET_VEC_ELT(code_obj,i,car(lis));
            	i++;
            	lis = cdr(lis);
            	SET_VEC_ELT(code_obj,i,get_int(car(lis)));
        		break;   
        case 21:
        case 22:
        case 23:
        case 24:
        case 25:
        case 26:
        case 27:
        case 28:
        case 29:
        case 30:SET_VEC_ELT(code_obj,i,x);
        		break;
        case 31:SET_VEC_ELT(code_obj,i,x);
        		i++;
                lis = cdr(lis);
                SET_VEC_ELT(code_obj,i,car(lis));
                break;
        case 32:SET_VEC_ELT(code_obj,i,x);
        		break;
        case 33:SET_VEC_ELT(code_obj,i,x);
        		break;
        case 34:
        case 35:
        case 36:SET_VEC_ELT(code_obj,i,x);
                break;
        case 37:SET_VEC_ELT(code_obj,i,x);
        		break;
        case 38:SET_VEC_ELT(code_obj,i,x);
        		i++;
        		lis = cdr(lis);
        		SET_VEC_ELT(code_obj,i,car(lis));
                i++;
        		lis = cdr(lis);
        		SET_VEC_ELT(code_obj,i,car(lis));
        		break;
        case 39:
        case 40:SET_VEC_ELT(code_obj,i,x);
        		i++;
        		lis = cdr(lis);
        		SET_VEC_ELT(code_obj,i,car(lis));
                break;
        }
		i++;
        lis = cdr(lis);
    }
    return(code_obj);
}


int count_stack(void){
	int i,count;
    
    i = sp -1;
    count = 0;
    while(i > 0 && (GET_TAG(stack[i]) != ENV)){
    	i--;
        count++;
    }
    return(count);   
}

int find_code_pointer(int addr){
	int i;
    
    for(i=0; i<code_pointer_end; i++)
    	if(code_pointer[i][0] == addr)
        	return(code_pointer[i][1]);
    
    return(-1);
}

void step(void){
	int i,j,size_n,size_m,addr;
    char c;
    
    if(stepflag == 0)
    	return;

    printf("pc: %d \n" ,pc);
    addr = pc;
    for(i=0; i<3; i++){
    	printf("code[%d] ",addr);
        addr = disasm(addr);
    }
	
	printf("sp: %d \n", sp);
	for(i=0; i<sp; i++){
    	printf("stack[%d] ", i);
        if(GET_TAG(stack[i+1]) == ENV && (i+1<sp))
        	printf("%d", stack[i]); //戻りアドレスの場合
        else
        	print(stack[i]);
        printf("\n");
	}
    printf("env: \n");
	size_n = GET_AUX(env);
    size_m = GET_CDR(env);
    for(i=0; i<size_m ; i++){
		for(j=0; j<size_n; j++){
        	printf("[");
            print(GET_ENV_MAT_ELT(env,i,j));
        	printf("]");
        }
        printf("\n");
    }
            
	printf("> ");
	fflush(stdout);
    fflush(stdin);
	c = getc(stdin);
	if(c == 'q' || c == 'Q'){
    	stepflag = 0;
        longjmp(toplevel,1);	
    }
    if(c == 'c'){
    	for(i=0; i<=50; i++)
    		printf("[%d]", code[i]);
        printf("\n");
    }
}

int disasm(int addr){
	int op;
    
    op = code[addr];
    switch(op){
    	case 0:	printf("nop\n");
        		break;
        case 1: printf("halt\n");
        		break;
        case 2: printf("const ");
        		print(code[addr+1]);
                printf("\n");
                addr++;
                break;
        case 3: printf("lvar %d %d\n", code[addr+1], code[addr+2]);
                addr = addr + 2;
                break;
        case 4:	printf("gvar ");
				print(code[addr+1]);
                printf("\n");
        		break;
        case 5: printf("lset %d %d\n", code[addr+1], code[addr+2]);
                addr = addr + 2;
                break;
        case 6:	printf("gset\n");
        		print(code[addr+1]);
                printf("\n");
        		break;
        case 7:	printf("pop\n");
        		break;
        case 8: printf("jump %d\n", code[addr+1]);
        		addr++;
        		break;
        case 9:	printf("tump %d\n", code[addr+1]);
        		addr++;
        		break;
        case 10:printf("fump %d\n", code[addr+1]);
        		addr++;
        		break;
        case 11:printf("return\n");
        		break;
    	case 12:printf("args %d\n", code[addr+1]);
        		addr++;
        		break;
        case 13:printf("call %d\n", code[addr+1]);
        		addr++;
        		break;
        case 14:printf("callj %d\n", code[addr+1]);
        		addr++;
        		break;
        case 15:printf("fn %d <code>\n", code[addr+1]);
                addr = addr + 2;
        		break;
        case 16:printf("save %d", code[addr+1]);
        		break;
        case 17:printf("prim ");
        		print(code[addr+1]);
                printf(" ");
                printf("%d\n", code[addr+2]);
                addr = addr + 2;
        		break;
        case 18:printf("def ");
        		print(code[addr+1]);
                printf("\n");
                break;
        case 19:printf("defm ");
        		print(code[addr+1]);
                printf("\n");
                break;
        case 20:printf("defh ");
        		print(code[addr+1]);
                printf(" ");
                print(code[addr+2]);
                printf("\n");
                addr = addr + 2;
                break;
        case 21:printf("neqp\n");
        		break;
        case 22:printf("smlp\n");
        		break;
        case 23:printf("esmlp\n");
        		break;
        case 24:printf("grtp\n");
        		break;
        case 25:printf("egrtp\n");
        		break;
        case 26:printf("zerop\n");
        		break;
        case 27:printf("add1\n");
        		break;
        case 28:printf("sub1\n");
        		break;
        case 29:printf("add2\n");
        		break;
        case 30:printf("sub2\n");
        		break;
        case 31:printf("gref ");
        		print(code[addr+1]);
                printf("\n");
                addr++;
                break;
        case 32:printf("catch\n");
        		break;
        case 33:printf("pause\n");
        		break;
        case 34:printf("car\n");
        		break;
        case 35:printf("cdr\n");
        		break;
        case 36:printf("cons\n");
        		break;
		case 37:printf("adapt\n");
        		break;
        case 38:printf("deflib ");
        		print(code[addr+1]);
                printf(" ");
                print(code[addr+2]);
                printf("\n");
                addr = addr + 2;
                break;
        case 39:printf("explib ");
        		print(code[addr+1]);
                printf("\n");
                addr++;
                break;
        case 40:printf("implib ");
        		print(code[addr+1]);
                printf("\n");
                addr++;
                break;
    }
    addr++;
    return(addr);
}

int stack_to_list(int n){
	int res,i;
    
    res = NIL;
    for(i=0; i<n; i++){
    	res = cons(stack[sp-1-i],res);
    }
    return(res);
}

void push_back_trace(int proc, int args){
	int i;
    
    if(back_trace_end < TRACE_DEPTH){
    	back_trace[back_trace_end][0] = proc;
        back_trace[back_trace_end][1] = args;
        back_trace_end++;
    }
    else{
    	for(i=0; i<TRACE_DEPTH-1; i++){
        	back_trace[i][0] = back_trace[i+1][0];
            back_trace[i][1] = back_trace[i+1][1];
        }
        back_trace[i][0] = proc;
        back_trace[i][1] = args;
    }	
}

//-------read()--------
int read(void){
	char *e;
    double r,s,x,y;
    int tag;
                
    gettoken();
    tag = (int)stok.type;
    switch(tag){
    	case FILEEND:	return(end_of_file);
    	case INTEGER:	if(stok.exact == YES)
        					return(make_int(atoi(stok.buf)));
        				else
                        	return(exact_to_inexact(make_int(atoi(stok.buf))));
        case BIGNUM:	if(stok.exact == YES)
        					return(make_big(stok.buf));
        				else
                        	return(make_flt(bignumtofloat(make_big(stok.buf))));
        case BINARY:	return(make_int((int)strtol(stok.buf,&e,2)));
        case OCTAL:		return(make_int((int)strtol(stok.buf,&e,8)));
        case DECNUM:	return(make_int((int)strtol(stok.buf,&e,10)));
        case HEXNUM: 	return(make_int((int)strtol(stok.buf,&e,16)));
        case EXPTNUM:	return(make_flt(atof(stok.buf)));	
        case FLOAT_N:	if(stok.exact == NO)
        					return(make_flt(atof(stok.buf)));
        				else
                        	return(inexact_to_exact(make_flt(atof(stok.buf))));
        case RATIONAL:	if(stok.exact == YES)
        					return(make_rat(atoi(stok.before),atoi(stok.after)));
        				else
                        	return(exact_to_inexact(make_rat(atoi(stok.before),atoi(stok.after))));
        case COMPLEX:	if(stok.ctype == RECTANGLER){
        					x = BOOLF;
                            y = BOOLF;
        					if(strcmp(stok.before,"+inf.0") ==0)
                            	x = PINF;
                            if(strcmp(stok.before,"-inf.0") ==0)
                            	x = MINF;
                            if(strcmp(stok.before,"+nan.0") ==0)
                            	x = PNAN;
                            if(strcmp(stok.before,"-nan.0") ==0)
                            	x = MNAN;
                            if(strcmp(stok.after,"+inf.0") ==0)
                            	y = PINF;
                            if(strcmp(stok.after,"-inf.0") ==0)
                            	y = MINF;
                            if(strcmp(stok.after,"+nan.0") ==0)
                            	y = PNAN;
                            if(strcmp(stok.after,"-nan.0") ==0)
                            	y = MNAN;
                            if(x == BOOLF)
                            	x = make_flt(atof(stok.before));
                            if(y == BOOLF)
                            	y = make_flt(atof(stok.after));
                            
                            return(make_comp1(x,y));
        				}
                        //POLAR
                        else{
                        	r = atof(stok.before);
    						s = atof(stok.after);
    						x = r * cos(s);
    						y = r * sin(s);
    						return(make_comp(x,y));
                        }
        case SYMBOL:	return(make_sym(stok.buf));
        case SBOOL:		return(returnbool(stok.buf));
        case INFINITY_NUMBER:
        				return(returninf(stok.buf));
        case NOT_A_NUMBER:
        				return(returnnan(stok.buf));
        case STRING:	return(make_str(stok.buf));
        case CHARACTER:	return(make_char(stok.buf));
        case QUOTE:		return(cons(quote, cons(read(),NIL)));
        case QUASIQUOTE:return(cons(quasiquote,cons(read(),NIL)));
        case UNQUOTE:  {gettoken();
        				switch(stok.type){
                        	case SPLICING:	return(cons(unquote_splicing,cons(read(),NIL)));
                            default:   {stok.flag = BACK;
                            			return(cons(unquote,cons(read(),NIL)));}
                        }}
        case LPAREN:	return(readlist());
        case VECTOR:	return(vector(readlist()));
        case U8VECTOR:	return(u8vector(readlist()));
        case RPAREN:	exception("",EXTRA_PAREN, NIL);
    }				
    exception("",CANT_READ,make_str(stok.buf));
    return(undef);
}

int readlist(void){
    int car_elt,cdr_elt;
    
    gettoken();      
    if(stok.type == RPAREN)
    	return(NIL);
    else
    if(stok.type == DOT){
    	cdr_elt = read();
        if(atomp(cdr_elt) 
        	|| (pairp(cdr_elt) && eqp(car(cdr_elt),unquote)) 
        	|| (pairp(cdr_elt) && eqp(car(cdr_elt),unquote_splicing)))
        	gettoken();
        return(cdr_elt);
    }
    else{
    	stok.flag = BACK;
    	car_elt = read();
        cdr_elt = readlist();
        return(cons(car_elt,cdr_elt));
    }
}

int issymch(char c){
	switch(c){
    	case '!':
        case '?':
        case '+':
        case '-':
        case '*':
        case '/':
        case '=':
        case '<':
        case '>': 
        case '_':
        case '.': 
        case ':': 
        case '#': 
        case '$': 
        case '@':
        case '%':
        case '&':
        case '~':
        case '^': return(1);
        default:  return(0);
    }
}  



void gettoken(void){
	char c, directive[20];
    int pos,i;
    
    if(stok.flag == BACK){
    	stok.flag = GO;
        return;
    }
    
    if(stok.ch == ')'){
    	stok.type = RPAREN;
        stok.ch = NUL;
        return;
    }
    
    if(stok.ch == '('){
    	stok.type = LPAREN;
        stok.ch = NUL;
        return;
    }
	
    if(caseflag)
		c = tolower(getc(input_port));
    else
    	c = getc(input_port);
    
 	skip:
    //スペース等のスキップ
    while((c == SPACE) || (c == EOL) || (c == TAB))
    	c = getc(input_port);
    
    //コメント行のスキップ
    //コメント行の行末にEOFがあった場合にはFILEENDを返す。
    if(c == ';'){
    	while(!(c == EOL)){
        	c = getc(input_port);
        	if(c == EOF){
    			stok.type = FILEEND;
                return;
            }
        }
        goto skip;
    }
    
    
    //ファイルの終端ならFILEENDを返す。
    if(c == EOF){
    	stok.type = FILEEND;
    	return;
    }
    
    switch(c){
    	case '(':	stok.type = LPAREN; break;
        case ')':	stok.type = RPAREN; break;
        case '\'':	stok.type = QUOTE; break;
        case '`':	stok.type = QUASIQUOTE; break;
        case ',':  	stok.type = UNQUOTE; break;
        case '@':	stok.type = SPLICING; break;
        case '.':  {c = getc(input_port);
        			//後ろが空白のdotはdot-pairと解釈
        			if(c == SPACE){
        				stok.type = DOT;
                        break;
                    }
                    //それ以外はシンボルと解釈
        			else{
                    	stok.buf[0] = '.';
                        stok.buf[1] = c;
                        pos = 2;
                        goto dot_exception;
                    }
                    }	
        case '"':  {c = getc(input_port);
        			pos = 0;
               		while(c != '"'){
                    	switch(c){
                    		case EOL:	stok.buf[pos++] = '\\';
                            			stok.buf[pos++] = 'n'; break;
                            case RET:	stok.buf[pos++] = '\\';
                            			stok.buf[pos++] = 'r'; break;
                            case TAB:	stok.buf[pos++] = '\\';
                            			stok.buf[pos++] = 't'; break;
                            case '\\':	stok.buf[pos++] = c;
                            			c = getc(input_port);
                                        stok.buf[pos++] = c; break;
                            default:	stok.buf[pos++] = c;
                        }
                        c = getc(input_port);
                    }
                    stok.buf[pos] = NUL;
                    stok.type = STRING;
                    break;
                    }
        case '#':  {c = getc(input_port);
        			if(c == '(' ){
                    	stok.type = VECTOR;
                        break;
                    }
                    if(c == 'u'){
                    	c = getc(input_port);
                        if(c == '8'){
                        	c = getc(input_port);
                            if(c == '(')
                        		stok.type = U8VECTOR;
                            else{
                            	ungetc(c, input_port);
                        		c = '8';
                                ungetc(c, input_port);
                        		c = 'u';
                            }
                        }        		
                        else{
                        	ungetc(c, input_port);
                        	c = 'u';
                        }
                        break;	
                    }
                    if(c == '\\'){
                		pos = 0;
                    	c = getc(input_port);
                        stok.buf[pos++] = c;
                        while(((c=getc(input_port)) != EOL) && (pos < BUFSIZE) && 
            					(c != SPACE) && (c != '(') && (c != ')'))
            				stok.buf[pos++] = c;
            
            			stok.buf[pos] = NUL;
                        stok.type = CHARACTER;
            			stok.ch = c;
                        break;
                    }
                    if(c == '|'){
                        reskip:
                    	c = getc(input_port);
                    	while(c != '|'){
                        	c = getc(input_port);
                        }
                        c = getc(input_port);
                        if(c == '#'){
                        		c = getc(input_port);
                            	goto skip;
                        }
                        else
                        		goto reskip;
                    }
                    if(c == '!'){
                    	i = 0;
                        c = getc(input_port);
                        while((c != SPACE) && (c != EOL) && (c != TAB) && (c != EOF)){
                            directive[i] = c;
                            i++;
                            c = getc(input_port);
                        }
                        directive[i] = NUL;
                        if(strcmp(directive,"fold-case") == 0)
                        	caseflag = 1;
                        else if(strcmp(directive,"no-fold-case") == 0)
                        	caseflag = 0;
                            
                        c = getc(input_port);
                        goto skip;
                    }
                    ungetc(c, input_port);
                    c = '#';
                    }		
        default: {
        	pos = 0; stok.buf[pos++] = c;
            dot_exception:
        	while(((c=getc(input_port)) != EOL) && (pos < BUFSIZE) && 
            		(c != SPACE) && (c != '(') && (c != ')')){
            	if(caseflag)
                	stok.buf[pos++] = tolower(c);
                else
                	stok.buf[pos++] = c;
            }
            
            stok.buf[pos] = NUL;
            stok.ch = c;
        	
            if(inftoken(stok.buf)){
            	stok.type = INFINITY_NUMBER;
                break;
            }
            if(nantoken(stok.buf)){
            	stok.type = NOT_A_NUMBER;
                break;
            }
            
            //先に複素数の判定をしないと-1-2iのようなケースでシンボルになってしまう。
            if(comptoken(stok.buf)){
            	stok.type = COMPLEX;
                stok.ctype = RECTANGLER;
                break;
            }
            if(polar_comptoken(stok.buf)){
            	stok.type = COMPLEX;
                stok.ctype = POLAR;
                break;
            }
            //先にbignumの判定をする。inttoken()は桁数の判定をしていない。
            if(bignumtoken(stok.buf)){
            	stok.type = BIGNUM;
                break;
            }
            if(exact_bignumtoken(stok.buf)){
            	stok.type = BIGNUM;
                stok.exact = YES;
                break;
            }
            if(inexact_bignumtoken(stok.buf)){
            	stok.type = BIGNUM;
                stok.exact = NO;
                break;
            }
            if(inttoken(stok.buf)){
            	stok.type = INTEGER;
                stok.exact = YES;
                break;
            }
            if(exact_inttoken(stok.buf)){
            	stok.type = INTEGER;
                stok.exact = YES;
                break;
            }
            if(inexact_inttoken(stok.buf)){
            	stok.type = INTEGER;
                stok.exact = NO;
                break;
            }
            if(flttoken(stok.buf)){
            	stok.type = FLOAT_N;
                stok.exact = NO;
                break;
            }
            if(exact_flttoken(stok.buf)){
            	stok.type = FLOAT_N;
                stok.exact = YES;
                break;
            }
            if(inexact_flttoken(stok.buf)){
            	stok.type = FLOAT_N;
                stok.exact = NO;
                break;
            }
            if(rattoken(stok.buf)){
            	stok.type = RATIONAL;
                stok.exact = YES;
                break;
            }
            if(exact_rattoken(stok.buf)){
            	stok.type = RATIONAL;
                stok.exact = YES;
                break;
            }
            if(inexact_rattoken(stok.buf)){
            	stok.type = RATIONAL;
                stok.exact = NO;
                break;
            }
            if(bintoken(stok.buf)){
            	stok.type = BINARY;
                break;
            }
            if(octtoken(stok.buf)){
            	stok.type = OCTAL;
                break;
            }
    		if(dectoken(stok.buf)){
            	stok.type = DECNUM;
                break;
            }
            if(hextoken(stok.buf)){
            	stok.type = HEXNUM;
                break;
            }
            if(expttoken(stok.buf)){
            	stok.type = EXPTNUM;
                break;
            }
            if(booltoken(stok.buf)){
            	stok.type = SBOOL;
                break;
            }
            if(symtoken(stok.buf)){
            	stok.type = SYMBOL;
                break;
            }	
            stok.type = OTHER;  
        }
    }
}


septoken separater(char buf[], char sep){
	int i,j;
    char c;
    septoken res;
    
    res.sepch = NUL;
    res.before[0] = buf[0];
    i = 1; j = 1; 
    while((c=buf[i]) != NUL)
    	if(c == sep){
        	res.before[j] = NUL;
            res.sepch = sep;
        	i++;
            j = 0;
            while((c=buf[i]) != NUL){
            	res.after[j] = c;
                i++;j++;
            }
            res.after[j] = NUL;
        }     
        else{
        	res.before[j] = c;
        	i++; j++;
        }
    return(res);
}	

void insertstr(char ch, char buf[]){
	int i;
    
    i = laststr(buf)+1;
    while(i >= 0){
    	buf[i+1] = buf[i];
        i--;
    }
    buf[0] = ch;
}

    
   
int laststr(char buf[]){
	int i;
    
    i = 0;
    while(buf[i] != NUL)
    	i++;
    return(i-1);
}



//文字の#\a -> a 　#\を取り除く
void dropchar(char buf[]){
	int i,j;
    
    j = laststr(buf);
    for(i=2; i<=j; i++)
    	buf[i-2] = buf[i];
    buf[i-2] = NUL;
}

//符号あり整数 桁数は判定していない。
int inttoken(char buf[]){
	int i;
    char c;
    
    if(((buf[0] == '+') || (buf[0] == '-'))){
		if(buf[1] == NUL)
        	return(0); // case {+,-} => symbol
    	i = 1;
    	while((c=buf[i]) != NUL)
        	if(isdigit(c))
            	i++;  // case {+123..., -123...}
            else
            	return(0);
    }
    else {       
    	i = 0;    // {1234...}
    	while((c=buf[i]) != NUL)
    		if(isdigit(c))
        		i++;
        	else 
        		return(0);
    }
    return(1);
}

// 正確な整数
int exact_inttoken(char buf[]){
    char buf1[BUFSIZE];
    
    if(buf[0] == '#' && buf[1] == 'e'){
    	strcpy(buf1,buf);
    	dropchar(buf1);
        if(inttoken(buf1)){
        	dropchar(buf);
    		return(1);
        }
        else
        	return(0);
	}
    else
    	return(0);
}

// 非正確な整数
int inexact_inttoken(char buf[]){
    char buf1[BUFSIZE];
    
    if(buf[0] == '#' && buf[1] == 'i'){
    	strcpy(buf1,buf);
    	dropchar(buf1);
        if(inttoken(buf1)){
        	dropchar(buf);
    		return(1);
        }
        else
        	return(0);
	}
    else
    	return(0);
}

//符号なし整数
int inttoken_nsgn(char buf[]){
	int i;
    char c;
    
    i = 0;
	while((c=buf[i]) != NUL)
    	if(isdigit(c))
        	i++;
        else 
        	return(0);
    return(1);
}

//bignum
//符号を含めないで９ケタ
int bignumtoken(char buf[]){
	int i;
    char c;
    
    if(((buf[0] == '+') || (buf[0] == '-'))){
		if(buf[1] == NUL)
        	return(0); // case {+,-} => symbol
    	i = 1;
    	while((c=buf[i]) != NUL)
        	if(isdigit(c))
            	i++;  // case {+123..., -123...}
            else
            	return(0);
        if(strlen(buf) <= 10)
        	return(0); //case not bignum 
    }
    else {       
    	i = 0;    // {1234...}
    	while((c=buf[i]) != NUL)
    		if(isdigit(c))
        		i++;
        	else 
        		return(0);
        if(strlen(buf) <= 9)
        	return(0); //case not bignum
    }
    return(1); //bignum
}

// 正確な巨大整数
int exact_bignumtoken(char buf[]){
    char buf1[BUFSIZE];
    
    if(buf[0] == '#' && buf[1] == 'e'){
    	strcpy(buf1,buf);
    	dropchar(buf1);
        if(bignumtoken(buf1)){
        	dropchar(buf);
    		return(1);
        }
        else
        	return(0);
	}
    else
    	return(0);
}

// 非正確な巨大整数
int inexact_bignumtoken(char buf[]){
    char buf1[BUFSIZE];
    
    if(buf[0] == '#' && buf[1] == 'i'){
    	strcpy(buf1,buf);
    	dropchar(buf1);
        if(bignumtoken(buf1)){
        	dropchar(buf);
    		return(1);
        }
        else
        	return(0);
	}
    else
    	return(0);
}

int rattoken(char buf[]){
	septoken tok;
    
    tok = separater(buf, '/');
    
    if(tok.sepch == NUL)
    	return(0);
    
    if(inttoken(tok.before) && inttoken_nsgn(tok.after)){
    	strcpy(stok.before, tok.before);
       	strcpy(stok.after, tok.after);
       	return(1);
    }
    else
    	return(0);
}

int exact_rattoken(char buf[]){
    char buf1[BUFSIZE];
    
    if(buf[0] == '#' && buf[1] == 'e'){
    	strcpy(buf1,buf);
    	dropchar(buf1);
        if(rattoken(buf1)){
        	dropchar(buf);
    		return(1);
        }
        else
        	return(0);
	}
    else
    	return(0);
}

int inexact_rattoken(char buf[]){
    char buf1[BUFSIZE];
    
    if(buf[0] == '#' && buf[1] == 'i'){
    	strcpy(buf1,buf);
    	dropchar(buf1);
        if(rattoken(buf1)){
        	dropchar(buf);
    		return(1);
        }
        else
        	return(0);
	}
    else
    	return(0);
}

int bintoken(char buf[]){
	int i;
    char c;
    
    if(!(buf[0] == '#' && (buf[1] == 'b' || buf[1] == 'B')))
    	return(0);
    
    i = 2;
    while((c=buf[i]) != NUL)
    	if(c == '0' || c == '1')
        	i++;
        else
        	return(0);
    
    dropchar(buf);
    return(1);
}


int flttoken(char buf[]){
    septoken tok;
    char bufcp[SYMSIZE];
    
    if(buf[0] == '.'){
    	strcpy(bufcp,buf);
    	insertstr('0',bufcp);
        if(flttoken(bufcp))
        	return(1);
    }
    
    tok = separater(buf, '.'); 
    
    if(tok.sepch == NUL)
    	return(0);

    if(inttoken(tok.before) && inttoken_nsgn(tok.after))
    	return(1);
    else
    if((strcmp(tok.before,"-") == 0) && inttoken_nsgn(tok.after))
    	return(1);
    else
    	return(0);
}

int exact_flttoken(char buf[]){
    char buf1[BUFSIZE];
    
    if(buf[0] == '#' && buf[1] == 'e'){
    	strcpy(buf1,buf);
    	dropchar(buf1);
        if(flttoken(buf1)){
        	dropchar(buf);
    		return(1);
        }
        else
        	return(0);
	}
    else
    	return(0);
}

int inexact_flttoken(char buf[]){
    char buf1[BUFSIZE];
    
    if(buf[0] == '#' && buf[1] == 'i'){
    	strcpy(buf1,buf);
    	dropchar(buf1);
        if(flttoken(buf1)){
        	dropchar(buf);
    		return(1);
        }
        else
        	return(0);
	}
    else
    	return(0);
}

int comptoken(char buf[]){
	septoken tok;
    int lastindex;
    
    lastindex = laststr(buf);
    
    if(buf[lastindex] != 'i')
    	return(0);
    
    // +i -> 0+1i
    if(buf[0] == '+' && buf[1] == 'i' && buf[2] == NUL){
    	strcpy(stok.before,"0");
        strcpy(stok.after,"1");
        return(1);
    }
    
    // -i -> 0-1i
    if(buf[0] == '-' && buf[1] == 'i' && buf[2] == NUL){
    	strcpy(stok.before,"0");
        strcpy(stok.after,"-1");
        return(1);
    }
         
    buf[lastindex] = NUL; // 虚数単位のiを削除する。
    
    tok = separater(buf, '+');
    
    if(tok.sepch == NUL)
    	goto minus;
    
    insertstr('+', tok.after);
    
    //1+iのような場合
    if((inttoken(tok.before) || flttoken(tok.before)) &&
    	tok.after[1] == NUL){
    	strcpy(stok.before, tok.before);
        stok.after[0] = '1';
        stok.after[1] = NUL;
    	return(1);
    }
	
    //n+inf.0 , inf.0+n , n+nan.0 , nan.0+n などの場合
	if(((inttoken(tok.before) || flttoken(tok.before)) &&
    	(inftoken(tok.after)  || nantoken(tok.after)))
        ||
       ((inftoken(tok.before) || nantoken(tok.before)) &&
       	(inttoken(tok.after)  || flttoken(tok.after)))
        ||
       ((inftoken(tok.before) || nantoken(tok.before)) &&
        (inftoken(tok.after)  || nantoken(tok.after)))){
        
        strcpy(stok.before, tok.before);
       	strcpy(stok.after, tok.after);
       	return(1);
    }
    
    //通常のn+miの場合
    if((inttoken(tok.before)  || flttoken(tok.before)) &&
    	(inttoken(tok.after) || flttoken(tok.after))){
    	strcpy(stok.before, tok.before);
       	strcpy(stok.after, tok.after);
       	return(1);
    }
    
    minus: //例 1-2i
    
    tok = separater(buf, '-');
    
    if(tok.sepch == NUL)
    	goto failexit;
    
    insertstr('-', tok.after);
    
    //1-iのような場合
    if((inttoken(tok.before) || flttoken(tok.before)) &&
    	tok.after[1] == NUL){
    	strcpy(stok.before, tok.before);
        stok.after[0] = '-';
        stok.after[1] = '1';
        stok.after[2] = NUL;
    	return(1);
    }
    
    
    //n-inf.0 , inf.0-n , n-nan.0 , nan.0-n などの場合
	if(((inttoken(tok.before) || flttoken(tok.before)) &&
    	(inftoken(tok.after)  || nantoken(tok.after)))
        ||
       ((inftoken(tok.before) || nantoken(tok.before)) &&
       	(inttoken(tok.after)  || flttoken(tok.after)))
        ||
       ((inftoken(tok.before) || nantoken(tok.before)) &&
        (inftoken(tok.after)  || nantoken(tok.after)))){
        
        strcpy(stok.before, tok.before);
       	strcpy(stok.after, tok.after);
       	return(1);
    }
        
    //通常のn-miの場合
    if((inttoken(tok.before)  || flttoken(tok.before)) &&
    	(inttoken(tok.after) || flttoken(tok.after))){
       	strcpy(stok.before, tok.before);
       	strcpy(stok.after, tok.after);
       	return(1);
    }
    else
    	goto failexit;
    
    failexit: //複素数ではない場合
    
    buf[lastindex] = 'i'; //削除したiを復元
    return(0);
}

int polar_comptoken(char buf[]){
	septoken tok;
    
    tok = separater(buf, '@');
    
    if(tok.sepch == NUL)
    	return(0);
    
    if((inttoken(tok.before)  || flttoken(tok.before)) &&
    	(inttoken(tok.after) || flttoken(tok.after))){
    	strcpy(stok.before, tok.before);
       	strcpy(stok.after, tok.after);
       	return(1);
    }
    return(0);
}

int octtoken(char buf[]){
	int i;
    char c;
    
    if(!(buf[0] == '#' && (buf[1] == 'o' || buf[1] == 'O')))
    	return(0);
    
    i = 2;
    while((c=buf[i]) != NUL)
    	if(c == '0' || c == '1' || c == '2' || c == '3' || c == '4' ||
           c == '5' || c == '6' || c == '7')
        	i++;
        else
        	return(0);
    
    dropchar(buf);
    return(1);
}

int dectoken(char buf[]){
	int i;
    char c;
    
    if(!(buf[0] == '#' && (buf[1] == 'd' || buf[1] == 'D')))
    	return(0);
    
    i = 2;
    while((c=buf[i]) != NUL)
    	if(isdigit(c))
        	i++;
        else
        	return(0);
    
    dropchar(buf);
    return(1);
}

int hextoken(char buf[]){
	int i;
    char c;
    
    if(!(buf[0] == '#' && (buf[1] == 'x' || buf[1] == 'X')))
    	return(0);
    
    i = 2;
    while((c=buf[i]) != NUL)
    	if(isdigit(c) ||
         c == 'A' || c == 'B' || c == 'C' || c == 'D' || c == 'E' || c == 'F' ||
         c == 'a' || c == 'b' || c == 'c' || c == 'd' || c == 'e' || c == 'f')
        	i++;
        else
        	return(0);
    
    dropchar(buf);
    return(1);
}

int expttoken(char buf[]){
	septoken tok;
    
    tok = separater(buf, 'e');
    if(tok.sepch == NUL)
    	return(0);
    
    if((inttoken(tok.before)  || flttoken(tok.before)) &&
    	inttoken(tok.after)){

       	return(1);
    }
    else
    	return(0);
}

int symtoken(char buf[]){
	int i;
    char c;
    
    //printf("%s\n", buf);
    
    i = 0;
    while((c=buf[i]) != NUL)
    	if((isalpha(c)) || (isdigit(c)) || (issymch(c)))
        	i++;
        else 
        	return(0);
    
    return(1);
}

int booltoken(char buf[]){
	if(!(buf[0] == '#'))
    	return(0);
    if(!(buf[1] == 't' || buf[1] == 'T' 
    	|| buf[1] == 'f' || buf[1] == 'F'))
        return(0);
    if(!(buf[2] == NUL))
    	return(0);
    
    return(1);
}

int inftoken(char buf[]){
	
    if(strcmp(buf,"+inf.0") == 0)
     	return(1);
    else if(strcmp(buf,"-inf.0") == 0)
    	return(1);
    else
    	return(0);
}

int nantoken(char buf[]){
	
    if(strcmp(buf,"+nan.0") == 0)
    	return(1);
    else if(strcmp(buf,"-nan.0") == 0)
    	return(1);
    else
    	return(0);
}

int charcmp(char buf[], char cmp[]){
	int i;
    
    i = 2;
    while(buf[i] != NUL){
    	if(buf[i] == cmp[i-2])
        	i++;
        else
        	return(0);
    }
    if(cmp[i-2] == NUL)
        return(1);
    else
    	return(0);
}



//-------internal define--------------
//internl-defineをletrecに置き換える。
               
int define_to_letrec(int lis){
	int e;
    
    if(definep(lis)){
    	e = formal_define(lis);
        return(list3(car(e),cadr(e),replace(caddr(e))));
    }
    else
    	return(replace(lis));
    
}

//(define ...)かどうか？
int definep(x){
	if(listp(x) && eqvp(car(x),make_sym("define")))
    	return(1);
    else
    	return(0);
    
    return(undef);
}

//letrecに置き換える。
int replace(int lis){
    
    //lambda,let,let*,letrecの場合
	if(listp(lis) && 
      (eqp(car(lis),make_sym("lambda")) || eqp(car(lis),make_sym("let")) ||
       eqp(car(lis),make_sym("letrec")) || eqp(car(lis),make_sym("let*"))))
        return(cons(car(lis),cons(cadr(lis),replace1(cddr(lis)))));
    else
    	return(lis);
    
    return(undef);
}

//式を定義部と本体に分けたものを受け取りletrecに置き換える。
int replace1(int lis){
        int e,body,res;
    
    e = replace2(lis);
    if(nullp(car(e))){
        body = cdr(e);
        res = NIL;
        while(!nullp(body)){
        	res = cons(replace(car(body)),res);
        	body = cdr(body);
        }
        return(reverse(res));
    }
    else
        return(list1(cons(make_sym("letrec"),cons(reverse(car(e)),cdr(e)))));
    
    return(undef);
}


//式を定義部と本体に分離
int replace2(int lis){
                
	return(replace3(lis,NIL));
}

//上記の本体。
int replace3(int lis, int def){
	int e;
    
    if(definep(car(lis))){
    	e = formal_define(car(lis));
        return(replace3(cdr(lis),cons(list2(cadr(e),replace(caddr(e))),def)));
    }
    else
    	return(cons(def,lis));
    
    return(undef);
    	
}

//MITスタイルの定義式を正式な(define sym (lambda (x) ...)に変換。
int formal_define(int lis){
	if(atomp(cadr(lis)))
    	return(lis);
    else
    	return(list3(car(lis),caadr(lis),cons(make_sym("lambda"),cons(cdadr(lis),cddr(lis)))));
}


//-------quasi->procedure-----------------
int quasi_to_procedure(int lis){
	if(nullp(lis))
    	return(NIL);
    if(atomp(lis))
    	return(lis);
    if(vectorp(lis))
    	return(lis);
    if(bytevectorp(lis))
    	return(lis);
    if(car(lis) == quasiquote)
    	return(transfer(cadr(lis),1));
    else
    	return(cons(quasi_to_procedure(car(lis)),quasi_to_procedure(cdr(lis))));
}

int transfer(int lis, int n){
                           
    if(pairp(lis))
    	if(pairp(car(lis))){
    		if(caar(lis) == unquote)
        		return(list3(make_sym("cons"),uq_transfer(cadar(lis),n-1),transfer(cdr(lis),n)));
    		if(caar(lis) == unquote_splicing)
        		return(list3(make_sym("append"),uqs_transfer(cadar(lis),n-1),transfer(cdr(lis),n)));
            if(caar(lis) == quasiquote)
            	return(list3(make_sym("cons"),
                			list3(make_sym("list"),
                					list2(quote,quasiquote),
                            		transfer(cadar(lis),n+1)),
                            transfer(cdr(lis),n)));
        	else
        		return(list3(make_sym("cons"),transfer(car(lis),n),transfer(cdr(lis),n)));
    	}
    	else
            if(car(lis) == unquote)
            	return(cadr(lis));
            else       
        		return(list3(make_sym("cons"),list2(quote,car(lis)),transfer(cdr(lis),n)));
    else
    	return(list2(quote,lis));     	
}

int uq_transfer1(int lis, int n){
	if(nullp(lis))
    	return(NIL);
    if(atomp(lis))
    	return(lis);
	if(pairp(lis) && (car(lis) == unquote))
    	if(n == 1)
            return(eval(cadr(lis)));
        else
        	return(list2(unquote,uq_transfer1(cadr(lis),n-1)));
    else
    	return(cons(uq_transfer1(car(lis),n),uq_transfer1(cdr(lis),n)));
}

int uq_transfer(int lis, int n){
    
    if(pairp(lis)){
    	if(car(lis) == unquote)
        	return(list3(make_sym("list"),list2(quote,unquote),uq_transfer(cadr(lis),n-1)));
        if(car(lis) == unquote_splicing)
        	return(list3(make_sym("list"),list2(quote,unquote_splicing),uq_transfer(cadr(lis),n-1)));	
    	if(car(lis) == quasiquote)
        	return(transfer(cadr(lis),n+1));
        if(n > 0){
            return(list3(make_sym("list"),
            			list2(quote,unquote),
                        list2(quote,uq_transfer1(lis,n))));
        }
        else
        	return(cons(uq_transfer(car(lis),n),uq_transfer(cdr(lis),n)));
    }
    else
    	if(n == 0)
    		return(lis);
        else
        	return(list2(quote,lis));
}

int uqs_transfer1(int lis, int n){
	if(nullp(lis))
    	return(NIL);
    if(atomp(lis))
    	return(lis);
	if(pairp(lis) && (car(lis) == unquote_splicing))
    	if(n == 1)
            return(cdr(eval(cadr(lis))));
        else
        	return(list2(unquote_splicing,uqs_transfer1(cadr(lis),n-1)));
    else
    	return(cons(uqs_transfer1(car(lis),n),uqs_transfer1(cdr(lis),n)));
}
                
                
int uqs_transfer(int lis, int n){
    
    if(pairp(lis)){
    	if(car(lis) == unquote)
        	return(list3(make_sym("list"),list2(quote,unquote),uq_transfer(cadr(lis),n-1)));
        if(car(lis) == unquote_splicing)
        	return(list3(make_sym("list"),list2(quote,unquote),uqs_transfer(cadr(lis),n-1)));	
    	if(car(lis) == quasiquote)
        	return(transfer(cadr(lis),n+1));
        if(n > 0)
        	return(list3(make_sym("list"),
            			list2(quote,unquote_splicing),
                        list2(quote,uqs_transfer1(lis,n))));
        else
        	return(cons(uqs_transfer(car(lis),n),uqs_transfer(cdr(lis),n)));
    }
    else
    	if(n == 0)
    		return(lis);
        else
        	return(list2(quote,lis));
}

//-----print------------------                           
void print(int x){
	char c;
    int tag,i,j,m,n;
    

    tag = (int)GET_TAG(x);
	switch(tag){
    	case EMP:	fprintf(output_port, "EMP"); break;
    	case BIGBANG:
        			fprintf(output_port, "()"); break;
        case EMPSET:fprintf(output_port, "#<empty-set>"); break;
    	case INTN:	fprintf(output_port, "%d", GET_INT(x)); break;
        case BIG:	printbig(x); break;
        case FLTN:	printflt(GET_FLT(x)); break;
        case RAT:  	fprintf(output_port, "%d", GET_CAR(x));
        			fprintf(output_port, "/");
                    fprintf(output_port, "%d", GET_CDR(x));
                    break;
                    
        case COMP: 	if(floatp(GET_CAR(x)))
        				printflt(GET_REAL_FLT(x));
                    else
                    	print(GET_CAR(x));
                    if(floatp(GET_CDR(x))){
                    	if(GET_IMAG_FLT(x) >= 0)
                    		fprintf(output_port, "+");
                        printflt(GET_IMAG_FLT(x)); 
                    }
                    else
                    	print(GET_CDR(x));
                    fprintf(output_port, "i");
                    break;
        			
        case SYM:   if(x == undef)
                    	fprintf(output_port, "#<undef>");
        			else
                    if(inttoken(GET_NAME(x)) ||
                       flttoken(GET_NAME(x)) ||
                       rattoken(GET_NAME(x)) ||
                       comptoken(GET_NAME(x)))
                   		fprintf(output_port, "|%s|",GET_NAME(x));
                    else
        				fprintf(output_port, "%s", GET_NAME(x)); 
               		break;
        			
        case BOL:	fprintf(output_port, "%s", GET_NAME(x)); break;
        case INF:	fprintf(output_port, "%s", GET_NAME(x)); break;
        case NANN:	fprintf(output_port, "%s", GET_NAME(x)); break;
        case STR:  	fprintf(output_port, "\"");
        			fprintf(output_port, "%s", GET_NAME(x));
                    fprintf(output_port, "\""); break;
                	
        case CHR:  	fprintf(output_port, "%c%c", '#', '\\');
        			c = GET_CHAR(x);
                    if(c == SPACE)
                    	fprintf(output_port, "space");
                    else
                    if(c == RET)
                    	fprintf(output_port, "\r");
                    else
                    if(c == EOL)
                    	fprintf(output_port, "\n");
                    else
                    if(c == TAB)
                    	fprintf(output_port, "\t");
                    else
        				fprintf(output_port, "%c", GET_CHAR(x))
                    ; break;
                    
        case SUBR:	fprintf(output_port, "#<subr %s>", GET_NAME(x)); break;
        case SYNT:	fprintf(output_port, "#<syntax %s>", GET_NAME(x)); break;
        case CLOS:	fprintf(output_port, "#<closure %s>", GET_NAME(x)); break;
        case HCONT:	fprintf(output_port, "#<continuation>"); break;
        case MAC:	fprintf(output_port, "#<macro %s>", GET_NAME(x)); break;
        case HYG:	fprintf(output_port, "#<hygienic %s>", GET_NAME(x)); break;
        case LIS:  	if(eqp(car(x),quote)){
        				fprintf(output_port, "'");
                        print(cadr(x));
                        break;
                    }
        			if(eqp(car(x),quasiquote)){
        				fprintf(output_port, "`");
                        print(cadr(x));
                        break;
                    }
                    if(eqp(car(x),unquote)){
        				fprintf(output_port, ",");
                        print(cadr(x));
                        break;
                    }
                    if(eqp(car(x),unquote_splicing)){
        				fprintf(output_port, ",@");
                        print(cadr(x));
                        break;
                    }
                    else{	
        				printlist(x);
                    	break;
                    }
        case VEC:  	printvec(x);
        			break;
        case U8VEC:	print_u8vec(x);
        			break;
        case MUL:  	n = GET_VEC_ELT(x,0);
        			for(i=1; i<n; i++){
                    	print(GET_VEC_ELT(x,i));
                    	fprintf(output_port, "\n");
        			}
                    print(GET_VEC_ELT(x,i));
                    break;   
        case PRT:	fprintf(output_port, "#<port>"); break;
        case EOFO:	fprintf(output_port, "#<eof>"); break;
        case IDNT:	fprintf(output_port, "#<id %s,", GET_NAME(x));
        			print(GET_AUX(x));
                    fprintf(output_port, ">"); break;
        case SYNCLO:
        			fprintf(output_port, "#<synclo "); print(GET_CAR(x)); 
                    fprintf(output_port, ">"); break;
        case ENV:	fprintf(output_port, "<env>\n"); 
                    m = GET_CDR(x);
                   	n = GET_AUX(x);
                    for(i=0; i<m; i++){
                    	for(j=0; j<n; j++){
                        	printf("[");
                            print(GET_ENV_VEC_ELT(x,j));
                            printf("]");
                        }
                    	printf("\n");
                        x = GET_ENV_ORG(x);
                    }    
                    break;
        case CODE:	fprintf(output_port, "<code>");
        			n = GET_CDR(x);
                    for(i=0; i<n; i++){
                        printf("[%d]", GET_VEC_ELT(x,i));
                    }
        			printf("\n");
                    break;
        case STACK:	fprintf(output_port, "<stack>"); break;
    }
}

//浮動小数点数で整数の場合に桁数を調整して表示                            
void printflt(double x){
	if(x - ceil(x) != 0)
    	fprintf(output_port, "%0.16g", x);
    else
    	fprintf(output_port, "%0.1f", x);
}

void printbig(int x){
	int y;
    
    y = reverse(x);
    fprintf(output_port, "%d",GET_INT(car(y)));
    y = cdr(y);
    while(!nullp(y)){
    	fprintf(output_port, "%09d",abs(GET_INT(car(y))));
        y = cdr(y);
    }
}

void printlist1(int x){
	if(IS_NIL(x))
    	fprintf(output_port, ")");
    else
    if((!(pairp(cdr(x)))) && (! (nullp(cdr(x))))){
    	print(car(x));
        fprintf(output_port, " . ");
        print(cdr(x));
        fprintf(output_port, ")");
    }
    else {
    	print(GET_CAR(x));    
        if(! (IS_NIL(GET_CDR(x))))
        	fprintf(output_port, " ");
       	printlist1(GET_CDR(x));
    }
}

void printlist(int x){
	fprintf(output_port, "(");
    printlist1(x);
}

void printvec(int x){
	int len,i;
    
    fprintf(output_port, "#(");
    len = GET_CDR(x);
    for(i=0; i<len-1; i++){
    	print(GET_VEC_ELT(x,i));
        fprintf(output_port, " ");
    }
    print(GET_VEC_ELT(x,i));
    fprintf(output_port, ")");
}

void print_u8vec(int x){
	int len,i;
    
    fprintf(output_port, "#u8(");
    len = GET_CDR(x);
    for(i=0; i<len-1; i++){
    	printf("%d ", GET_U8VEC_ELT(x,i));
    }
    printf("%d", GET_U8VEC_ELT(x,i));
    fprintf(output_port, ")");
    
}
                            
//-------デバッグ用------------------  
void cellprint(int addr);

//ヒープダンプ	
void memorydump(int start, int end){
	int i;
    
    printf("addr      car     cdr     tag    val\n");
    for(i=start; i<= end; i++)
    	cellprint(i);
    
}

  
void cellprint(int addr){
	int flag,tag;
    
	printf("%07d ", addr);
    flag = (int)GET_FLAG(addr);
	switch(flag){
    	case FRE:	printf("F "); break;
        case USE:	printf("U "); break;
    }
    printf("%07d ", GET_CAR(addr));
    printf("%07d ", GET_CDR(addr));
    
    tag = (int)GET_TAG(addr);
	switch(tag){
    	case BIGBANG: 
        			printf("BigBang"); break;
    	case EMP:	printf("Emp    "); break;
        case INTN:	printf("Int    "); print(addr); break;
        case FLTN:	printf("Flt    "); print(addr); break;
        case COMP:	printf("Com    "); print(addr); break;
        case SYM:	printf("Sym    "); print(addr); printf(" = "); print(GET_CAR(addr)); break;
        case STR:	printf("Str    "); print(addr); break;
        case CHR:	printf("Chr    "); print(addr); break;
        case LIS:	printf("Lis    "); break;
        case BOL:	printf("Bol    "); print(addr); break;
        case INF:	printf("Inf    "); print(addr); break;
        case NANN:	printf("Nan    "); print(addr); break;
        case BIG:	printf("Big    "); printf("%d",GET_INT(addr)); break;
        case RAT:	printf("Rat    "); print(addr); break;
        case SUBR:	printf("Sub    "); break;
        case CLOS:	printf("Clo    "); break;
		case MAC:	printf("Mac    "); break;
        case HYG:	printf("Hyg    "); break;
        case HCONT:	printf("Cnt    "); print(GET_CDR(addr)); break;
        case MUL:	printf("Mul    "); break;
        case VEC:	printf("Vec    "); break;
        case ELT:	printf("Elt    "); break;
        case IDNT:	printf("Idnt   "); break;
        case SYNCLO:printf("SynClo "); break;
        case ENV:	printf("Env    "); break;
        case CODE:	printf("Code   "); break;
        case STACK:	printf("Stack  "); break;
    }
    printf("\n");
}   


//---------ガベージコレクション-----------

void gbc(void){
	register int addr;
    clock_t gctime1,gctime2;
	
    if(gbcflag == 1){
    	printf("enter GBC free= %d\n", cell_free); fflush(stdout);
    }
	gctime1 = clock();
    cont_count = 0;
	
    gbcmark();
    gbcsweep();
    cell_free = 0;
    for(addr=0; addr <= CELLSIZE; addr++)
    	if(IS_EMPTY(addr))
        	cell_free++;
    
    gctime2 = clock();
    gctime = gctime + (gctime2 - gctime1);
    
    if(gbcflag == 1){
		printf("exit  GBC free= %d\n", cell_free); fflush(stdout);
	}
}

           
void markcell(int addr){
	int n,i,j,x,y,tag;
    
    //小整数の場合はマークする必要なし。
    if(addr < 0 || addr > CELLSIZE){    
    	return;
    }	
	if(IS_USE(addr))
    	return;
	SET_FLAG_USE(addr);
    tag = (int)GET_TAG(addr);
    switch(tag){
        case FLTN:
        case RAT:
        case STR:
        case CHR:	return;
        case COMP:	markcell(car(addr));
        			markcell(cdr(addr));
                    return;
        case SYM:	markcell(GET_CAR(addr));
        case SUBR:
        case PRT:	return;
        case BIG:	markcell(car(addr));
        			markcell(cdr(addr));
                    return;
        case LIS:   markcell(car(addr));
					markcell(cdr(addr));
        			return;
    	case CLOS:	markcell(GET_CAR(addr));
    				markcell(GET_CDR(addr));
        			return;
		case HCONT:	markcell(GET_CAR(addr));
        			markcell(GET_CDR(addr));
                    markcell(car(GET_AUX(addr)));
        			return;
    	case MAC:	markcell(GET_CAR(addr));
    				return;	
    	case VEC:	n = vector_length(addr);
        			for(i=0; i<n; i++){
          				x = GET_VEC_ELT(addr,i);
            			markcell(x);
                    }
                    return;
        case MUL:	markcell(car(addr));
        			markcell(cdr(addr));
                    return;
        case ENV:	j = GET_AUX(addr);
                    y = j + 1;
                    
                    for(x=0; x<j; x++)
                    	markcell(GET_ENV_VEC_ELT(addr,x));
          			
          			markcell(GET_ENV_ORG(addr));
                    n = dyna_env_cpy(GET_CAR(addr),y);
                    SET_CAR(addr,n);
                    return;
        case CODE:	i = GET_CDR(addr);		
                    x = 0;
                    while(x<i){
                    	y = GET_VEC_ELT(addr,x);
                        switch(y){
                        	case 0:		x = x + 1;
          								break;
                        	case 1:		x = x + 1;
                            			break;
                        	case 2:		markcell(GET_VEC_ELT(addr,x+1));
                            			x = x + 2;
                                    	break;
                        	case 3:		x = x + 3;
                            			break;
                            case 4:		x = x + 2;
                            			break;
                            case 5:		x = x + 3;
                            			break;
                            case 6:		x = x + 2;
                            			break;
                            case 7:		x = x + 1;
                            			break;
                            case 8:		x = x + 2;
                            			break;
                            case 9:		x = x + 2;
                            			break;
                            case 10:	x = x + 2;
                            			break;
                            case 11:	x = x + 1;
                            			break;
                            case 12:	x = x + 2;
                            			break;
                            case 13:	x = x + 2;
                            			break;
                            case 14:	x = x + 2;
										break ;
                        	case 15:	markcell(GET_VEC_ELT(addr,x+2));
                                    	x = x + 3;
                                        break;
          					case 16:	x = x + 2;
                            			break;
                            case 17:	x = x + 3;
                            			break;
                            case 18:	x = x + 2;
                            			break;
                            case 19:	x = x + 2;
                            			break;
                        	case 20:	x = x + 3;
                            			break;
                            case 21:
                            case 22:
                            case 23:
                            case 24:
                            case 25:
                            case 26:
                            case 27:
                            case 28:
                            case 29:
                            case 30:	x = x + 1;
                            			break;
                            case 31:	x = x + 2;
                            			break;
                            case 32:	x = x + 1;
                            			break;
                            case 33:	x = x + 1;
                            case 34:
                            case 35:
                            case 36:
                            case 37:	x = x + 1;
                            			break;
                            case 38:	x = x + 3;
                            			break;
                            case 39:
                            case 40:	x = x + 2;
                            			break;
                        }
                    }
                    return;
        case STACK:	i = GET_CDR(addr);
        			for(x=0; x<i; x++)
                    	markcell(GET_VEC_ELT(addr,x));
                    return;
        
        case MEM:	i = GET_VEC_ELT(addr,1);
        			for(x=0; x<i; x = x+2)
                    	markcell(GET_VEC_ELT(addr,x+4));
                    return;
        
    }
}



void gbcmark(void){
	int register addr,i;
    int x,y;
    
    dyna_env_p2 = 0;
    
    SET_FLAG_USE(0);  //NILをuse状態にする。
    SET_FLAG_USE(1);  //#t
    SET_FLAG_USE(2);  //#f
    SET_FLAG_USE(15); //0
    SET_FLAG_USE(16); //1
    SET_FLAG_USE(17); //2
    SET_FLAG_USE(18); //-1
    SET_FLAG_USE(19); //3
    SET_FLAG_USE(20); //#\L
    SET_FLAG_USE(21); //"L"
    
    //現状の環境をマーク
    markcell(env);
    
    //トレースリストをマーク
    markcell(trace_list);
    
    //スタックからつながっているcellをマーク。
    for(i=sp-1; i>=0; i--){
    	addr = stack[i];
        markcell(addr);
    }
    //命令列からつながっているcellをマーク。
    for(i=tail-1; i>=0; i--){
    	addr = stack[i];
        markcell(addr);
    }
    
    //大域定義シンボルをマーク。
    for(x=0; x<HASHTBSIZE; x++)
    	for(y=0; y<module_table_end; y++){
    		addr = cell_hash_table[x][y];	           
    		while(!(nullp(addr))){
        		//printf("%d ", addr);
    			SET_FLAG_USE(addr);
    			markcell(car(addr));
        		addr = cdr(addr);
    	}
    }
    
    //ライブラリ名のリストおよびexportされたリストをマーク。
    for(x=0; x<module_table_end; x++){
    	markcell(module_table[x][0]);	
    	markcell(module_table[x][1]);
    }
    //emem2 からemem1にコピー
    for(x=0; x<dyna_env_p2; x++){
    	emem1[x] = emem2[x];
        SET_FLAG_USE(emem1[x]);
    }
    dyna_env_p1 = dyna_env_p2;
}

void gbcsweep(void){
	int addr;
    
    //セル領域のGC
    addr = 0;
    while(addr < CELLSIZE){
    	
        if(IS_USE(addr)){
        	SET_FLAG_FREE(addr);
        }
        else{
        	clrcell(addr);
            SET_CDR(addr,cell_heap_p);
            cell_heap_p = addr;
        }
        addr++;
    }          
}

void clrcell(int addr){
	int tag;
              
	if((tag=GET_TAG(addr)) == SYM || tag == STR || tag == CHR || tag == CLOS){
    	free(memory[addr].name);
    	memory[addr].name = NULL;
    }
	
    
    if(tag == CODE || tag == STACK || tag == VEC || tag == MEM){
        free(memory[addr].val.car.dyna_vec);
    }
    
    
    SET_TAG(addr,EMP);
    SET_CAR(addr,0);
    SET_AUX(addr,0);
}

//自由セルが一定数を下回った場合にはgbcを起動する。
void check_ctrl(void){
	//ctrl+c がかかっているときにはトップレベルに戻る。
	if(exitflag == 1){
    	exitflag = 0;
        longjmp(toplevel,1);
    }
    //ctrl+d がかかっているときには処理系を終了する。
    if(exitflag == 2)
    	longjmp(toplevel,2);
	if(cell_free < FREESIZE)
    	gbc();
}


int ealloc(int n){
	int res;
    
    res = dyna_env_p1;
    dyna_env_p1 = dyna_env_p1 + n;
    if(dyna_env_p1 > ENVSIZE)
    	exception("ealloc", MALLOC_OVERF, NIL);
        
    return(res);
}

int dyna_env_cpy(int p, int n){
	int i,res;
    
    res = dyna_env_p2;
    for(i=0; i<n; i++){
    	emem2[dyna_env_p2] = emem1[p];
    	dyna_env_p2++;
        p++;
        if(dyna_env_p2 > ENVSIZE2)
        	exception("dyna_env_copy", MALLOC_OVERF, NIL);
    }
    return(res); 
}