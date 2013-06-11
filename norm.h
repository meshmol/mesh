// normal header

#include <stdio.h>
#include <windows.h>
#include <time.h>

#define CELLSIZE 	20000000
#define FREESIZE	50000
#define SYMSIZE		64
#define CHARSIZE	1
#define BUFSIZE 	1024
#define CODESIZE	80000000
#define	STACKSIZE	200000
#define ENVSIZE		80000000
#define ENVSIZE2	ENVSIZE / 10
#define CLOSIZE		10000000
#define NIL			0
#define BOOLT		1
#define BOOLF		2
#define PINF		3
#define MINF		4
#define PNAN		5
#define MNAN		6
#define BIGNUM_BASE	1000000000
#define HASHTBSIZE	100
#define MODULESIZE	100
#define TRACE_DEPTH	10
#define INT_FLAG	1073741824 //#b1000000000000000000000000000000
#define INT_MASK	1073741823 //#b0111111111111111111111111111111
#define SMALL_INT_MAX		1000000000
#define SMALL_INT_MIN		-1000000000
		

//ポインタ
int cell_heap_p;
int cell_free;
int cell_hash_table[HASHTBSIZE][MODULESIZE];
int dyna_env_p1;
int dyna_env_p2;


typedef enum flag 	{FRE,USE} flag;

#define EMP 	0
#define BIGBANG 1
#define INTN 	2
#define FLTN 	3
#define COMP	4
#define BIG 	5
#define RAT 	6
#define SYM 	7
#define LIS 	8
#define VEC 	9
#define ELT 	10
#define BOL 	11
#define STR 	12
#define CHR 	13
#define SUBR 	14
#define SYNT 	15
#define CLOS 	16
#define HCONT 	17
#define SCONT 	18
#define MAC 	19
#define MUL 	20
#define PRT 	21
#define EOFO 	22
#define IDNT 	23
#define SYNCLO 	24
#define HYG 	25
#define CODE 	26
#define STACK 	27
#define ENV 	28
#define MEM 	29
#define REF 	30
#define EMPSET 	31
#define INF 	32
#define NANN 	33
#define U8VEC	34
#define RECORD	35		


/*
BIGBANG	虚無 空リストを表す。常に0番地に唯一存在する。
EMP		空セル
INTN	整数
FLTN	浮動小数点数
COMP	複素数
BIG		多倍長整数
RAT		分数
SYM		シンボル
LIS		リスト
VEC		ベクター
ELT		ベクターの要素
BOL		真偽値
STR		文字列
CHR		文字
SUBR	組込関数
SYNT	特殊形式
CLOS	クロージャー
SCONT	スタック上の継続
HCONT   ヒープ上の継続
MAC		マクロ
HYG		hygienic-macro
MUL		多値
PRT		ポート
EOFO	end_of_file obj
VEC		ベクタ
IDNT	識別子identifier
SYNCLO	syntactic-closure
ENV		配列型の局所環境
CODE    配列型の命令列
STACK   配列型のスタック
MEM		継続用のメモリ保存データ
REF		Prolog変数
EMPSET  空集合
INF		無限
NANN	not a number for Normal
U8VEC	byte vector
RECORD	record型
*/


struct cell {
	union{
    	double fltnum;
        struct {
        	union{
            	int intnum;
                int 	( *subr) ();
                FILE	*port;
                int		*dyna_vec;
                unsigned char *u8_dyna_vec;
            } car;
            union{
            	int intnum;
            } cdr;
        };
    } val;
    int		aux;
	char	tag;
    flag	flag;
	char 	*name;
    char 	args_cnt;
};
typedef struct cell cell;

//プロファイルのデータ蓄積用
struct prof{
	char 	*name;
    int 	count;
    clock_t time;
};
typedef struct prof prof;


typedef enum toktype 	{LPAREN,RPAREN,LBRAKET,RBRAKET,QUOTE,QUASIQUOTE,UNQUOTE,SPLICING,VECTOR,
						DOT,INTEGER,FLOAT_N,RATIONAL,BIGNUM,BINARY,OCTAL,DECNUM,HEXNUM,EXPTNUM,
                        COMPLEX,SYMBOL,SBOOL,STRING,INFINITY_NUMBER,NOT_A_NUMBER,U8VECTOR,
                        CHARACTER,FILEEND,OTHER} toktype;
typedef enum backtrack 	{GO,BACK} backtrack;
typedef enum exactness	{YES,NO} exactness;
typedef enum comptype	{RECTANGLER,POLAR} comptype;

struct token {
	char ch;
    backtrack flag;
	toktype type;
    exactness exact;
    comptype ctype;
    char buf[BUFSIZE];
    char before[BUFSIZE];
    char after[BUFSIZE];
};

struct septoken {
	char sepch;
    char before[BUFSIZE];
    char after[BUFSIZE];
};

typedef struct token token;
typedef struct septoken septoken;

//エラーコード
#define UNBOUND_VARIABLE	1
#define CANT_READ			2
#define ILLEGAL_ARGUMENT	3
#define CANT_OPEN			4
#define LACK_CELL			6
#define LACK_VEC			7
#define VM_ILLEGAL_CODE		8
#define INVALID_SYNTAX		9
#define INVALID_APP			10
#define NOT_PAIR			11
#define NOT_SYMBOL			12
#define NOT_NUMBER			13
#define NOT_REAL			14
#define NOT_INTEGER			15
#define NOT_FLOAT			16
#define NOT_RATIONAL		17
#define NOT_COMPLEX			18
#define NOT_CHAR			19
#define NOT_STRING			20
#define NOT_VECTOR			21
#define NOT_MULTIVAL		22
#define NOT_MACRO			23
#define NOT_PORT			24
#define NOT_PROCEDURE		25
#define NOT_CLOSURE			26
#define NOT_LIST			27
#define NOT_BOOL			28
#define NOT_IDENTIFIER		29
#define INCORRECT_ARG_CNT	30
#define DIVIDE_ZERO			31
#define TOO_BIG				32
#define NOT_HYGIENIC		33
#define NOT_SYNTACTIC		34
#define EXTRA_PAREN			35
#define STACK_OVERF			36
#define CODE_OVERF			37
#define NOT_MODULE			38
#define MODULE_OVERF		39
#define CLOSURE_OVERF		40
#define NOT_EXACT			41
#define ILLEGAL_VMCODE		42
#define MALLOC_OVERF		43
#define NOT_EXIST_LIB		44
#define NOT_BYTE_VECTOR		45
#define IMMUTABLE_OBJ		46
#define OUT_OF_RANGE		47

#define EOL		'\n'
#define RET		'\r'
#define TAB		'\t'
#define SPACE	' '
#define ESCAPE	033
#define NUL		'\0'

int quote,quasiquote,unquote,unquote_splicing,undef,end_of_file,empty_set;


#define GET_TAG(addr)		get_tag(addr)
#define GET_INT(addr)		addr_to_int(addr)
#define IS_INTEGER(addr)	(INT_FLAG & addr)
#define GET_INT_ADDR(n)		(INT_FALG | n)
#define GET_AUX(addr)		memory[addr].aux
#define SET_AUX(addr,x)		memory[addr].aux = x

#define GET_FLAG(addr)		memory[addr].flag
#define GET_NAME(addr)		memory[addr].name
#define GET_CHAR(addr)		memory[addr].name[0] 
#define GET_FLT(addr)		memory[addr].val.fltnum
#define GET_CAR(addr)		memory[addr].val.car.intnum
#define	GET_CDR(addr)		memory[addr].val.cdr.intnum
#define GET_SUBR(addr)		memory[addr].val.car.subr
#define GET_REAL_FLT(addr)	GET_FLT(memory[addr].val.car.intnum)
#define GET_IMAG_FLT(addr)	GET_FLT(memory[addr].val.cdr.intnum)
#define GET_PORT(addr)		memory[addr].val.car.port
#define GET_ARGS_CNT(addr)	memory[addr].args_cnt
#define STRING_REF(addr,k)	memory[addr].name[k]
#define STRING_SET(addr,k,c)	memory[addr].name[k] = c
#define SET_TAG(addr,x)		memory[addr].tag = x
#define SET_FLAG_FREE(addr)	memory[addr].flag = FRE
#define SET_FLAG_USE(addr)	memory[addr].flag = USE
#define SET_NAME(addr,x)	memory[addr].name = (char *)malloc(strlen(x)+1); strcpy(memory[addr].name,x);
#define SET_CHAR(addr,x)	memory[addr].name = (char *)malloc(CHARSIZE); memory[addr].name[0] = x; memory[addr].name[1] = NUL;
#define SET_FLT(addr,x) 	memory[addr].val.fltnum = x
#define SET_REAL_FLT(addr,x) 	memory[addr].val.car.intnum = make_flt(x)
#define SET_IMAG_FLT(addr,x) 	memory[addr].val.cdr.intnum = make_flt(x)
#define SET_CAR(addr,x)		memory[addr].val.car.intnum = x
#define SET_CDR(addr,x)		memory[addr].val.cdr.intnum = x
#define SET_SUBR(addr,x)	memory[addr].val.car.subr = (int (*)())x
#define SET_PORT(addr,x)	memory[addr].val.car.port = x
#define SET_ARGS_CNT(addr,x)	memory[addr].args_cnt = x
#define IS_EMPTY(addr)		memory[addr].tag	== EMP
#define IS_FREE(addr)		memory[addr].flag == FRE
#define IS_USE(addr)		memory[addr].flag == USE	
#define IS_SYMBOL(addr)		memory[addr].tag == SYM
#define IS_STRING(addr)		memory[addr].tag == STR
#define IS_FLOAT(addr)		memory[addr].tag == FLTN
#define IS_RATIONAL(addr)	memory[addr].tag == RAT
#define IS_BIGNUM(addr)		memory[addr].tag == BIG
#define IS_COMPLEX(addr)	memory[addr].tag == COMP
#define IS_LIST(addr)		memory[addr].tag == LIS
#define IS_VECTOR(addr)		memory[addr].tag == VEC
#define IS_U8VECTOR(addr)	memory[addr].tag == U8VEC
#define IS_NIL(addr)		memory[addr].tag == BIGBANG
#define IS_SUBR(addr)		memory[addr].tag == SUBR
#define IS_CHARACTER(addr)	memory[addr].tag == CHR
#define IS_CLOSURE(addr)	memory[addr].tag == CLOS
#define IS_CONT(addr)		memory[addr].tag == HCONT
#define IS_MACRO(addr)		memory[addr].tag == MAC
#define IS_HYGIENIC(addr)	memory[addr].tag == HYG
#define IS_BOOL(addr)		memory[addr].tag == BOL
#define IS_T(addr)			memory[addr].tag == BOL && HAS_NAME(addr,"#t")
#define IS_F(addr)			memory[addr].tag == BOL && HAS_NAME(addr,"#f")
#define IS_MULTVAL(addr)	memory[addr].tag == MUL
#define IS_SAME_TYPE(x,y)	memory[x].tag == memory[y].tag
#define IS_IDENTIFIER(addr)	memory[addr].tag == IDNT
#define IS_SYNCLO(addr)		memory[addr].tag == SYNCLO
#define IS_ENV(addr)		memory[addr].tag == ENV
#define IS_CODE(addr)		memory[addr].tag == CODE
#define IS_PORT(addr)		memory[addr].tag == PRT
#define IS_INF(addr)		(addr == PINF || addr == MINF)
#define IS_NAN(addr)		(addr == PNAN || addr == MNAN)
#define HAS_NAME(addr,x)	strcmp(memory[addr].name,x) == 0
#define SAME_NAME(addr1,addr2) strcmp(memory[addr1].name, memory[addr2].name) == 0
#define GREATER_NAME(addr1,addr2) strcmp(memory[addr1].name, memory[addr2].name) > 0
#define SMALLER_NAME(addr1,addr2) strcmp(memory[addr1].name, memory[addr2].name) < 0
#define EQUAL_STR(x,y)		strcmp(x,y) == 0
#define DEBUG				printf("debug\n"); longjmp(toplevel,1);
#define POPC(x)				x = GET_CAR(C); C = GET_CDR(C)
#define PUSHS(x)			S = cons(x,S)
#define GET_INT_CAR(x)		memory[memory[x].car.car].car.intnum
#define GET_INT_CDR(x)		memory[memory[x].cdr.cdr].car.intnum


#define GET_VEC_ELT(addr,i)			memory[addr].val.car.dyna_vec[i]
#define SET_VEC_ELT(addr,i,x)		memory[addr].val.car.dyna_vec[i] = x
#define SET_VEC(addr,x)				memory[addr].val.car.dyna_vec = x
#define GET_U8VEC_ELT(addr,i)		memory[addr].val.car.u8_dyna_vec[i]
#define SET_U8VEC_ELT(addr,i,x)		memory[addr].val.car.u8_dyna_vec[i] = x
#define SET_U8VEC(addr,x)			memory[addr].val.car.u8_dyna_vec = x

#define GET_ENV_MAT_ELT(addr,i,j)	emem1[memory[addr].val.car.intnum + 1 + (i)*(memory[addr].aux) + (j)]
#define SET_ENV_MAT_ELT(addr,i,j,x)	emem1[memory[addr].val.car.intnum + 1 + (i)*(memory[addr].aux) + (j)] = (x)
#define GET_ENV_ORG(addr)			emem1[memory[addr].val.car.intnum]
#define SET_ENV_ORG(addr,x)			emem1[memory[addr].val.car.intnum] = x
#define GET_ENV_VEC_ELT(addr,i)		emem1[memory[addr].val.car.intnum + 1 + (i)]
#define SET_ENV_VEC_ELT(addr,i,x)	emem1[memory[addr].val.car.intnum + 1 + (i)] = (x)

#define ARG1					code[pc+1]
#define ARG2					code[pc+2]
#define ARG3					code[pc+3]
#define PUSH_S(x)				stack[sp++] = x
#define POP_S					stack[--sp]
#define TOP_STACK				stack[sp-1]
#define SECOND_STACK			stack[sp-2]
#define THIRD_STACK				stack[sp-3]

#define OPCODE 41
#define VM_ERR_CHK				if(code[pc] < 1 || code[pc] >= OPCODE) exception("vm2", ILLEGAL_VMCODE, NIL)

//main.c
int eval(int x);
void exception(char *fn, int code, int arg);
BOOL WINAPI CtrlHandler(DWORD CtrlEvent);
int vm1(void);
int vm2(void);
void print(int x);
int read(void);
int readlist(void);
void push_s(int x);
int pop_s(void);
void insert_stack(int env, int pc, int n);
int find_code_pointer(int addr);
void step(void);
int disasm(int addr);
int stack_to_list(int n);
void push_back_trace(int proc, int args);
void list_to_code(int x);
int list_to_code_obj(int lis);
int count_stack(void);
void check_ctrl(void);
int issymch(char c);
void gettoken(void);
septoken separater(char buf[], char sep);
void insertstr(char ch, char buf[]);
int laststr(char buf[]);
void dropchar(char buf[]);
int inttoken(char buf[]);
int exact_inttoken(char buf[]);
int inexact_inttoken(char buf[]);
int inttoken_nsgn(char buf[]);
int bignumtoken(char buf[]);
int exact_bignumtoken(char buf[]);
int inexact_bignumtoken(char buf[]);
int rattoken(char buf[]);
int exact_rattoken(char buf[]);
int inexact_rattoken(char buf[]);
int bintoken(char buf[]);
int flttoken(char buf[]);
int exact_flttoken(char buf[]);
int inexact_flttoken(char buf[]);
int comptoken(char buf[]);
int polar_comptoken(char buf[]);
int octtoken(char buf[]);
int dectoken(char buf[]);
int hextoken(char buf[]);
int expttoken(char buf[]);
int symtoken(char buf[]);
int booltoken(char buf[]);
int inftoken(char buf[]);
int nantoken(char buf[]);
int charcmp(char buf[], char cmp[]);
void printflt(double x);
void printbig(int x);
void printlist1(int x);
void printlist(int x);
void printvec(int x);
void print_u8vec(int x);
void print_record(int x);
void memorydump(int start, int end);
void gbc(void);
void markcell(int addr);
void gbcmark(void);
void gbcsweep(void);
void clrcell(int addr);
int ealloc(int n);
int dyna_env_cpy(int p, int n);

//list.c
int car(int lis);
int cdr(int lis);
int scm_car(int lis);
int scm_cdr(int lis);
int caar(int lis);
int cdar(int lis);
int cadar(int lis);
int cddr(int lis);
int cdddr(int lis);
int cdadr(int lis);
int cadr(int lis);
int caddr(int lis);
int caadr(int lis);
int cadddr(int lis);
int cons(int car, int cdr);
int assq(int obj, int lis);
int assv(int obj, int lis);
int assoc(int obj, int lis);
int memq(int obj, int lis);
int memv(int obj, int lis);
int member(int obj, int lis);
int listtail(int lis, int n);
int listref(int lis, int n);
int drop(int lis, int n);
int length(int lis);
int list(int arglist);
int list1(int x);
int list2(int x, int y);
int list3(int x, int y, int z);
int list4(int x1, int x2, int x3, int x4);
int list5(int x1, int x2, int x3, int x4, int x5);
int list6(int x1, int x2, int x3, int x4, int x5, int x6);
int list7(int x1, int x2, int x3, int x4, int x5, int x6, int x7);
int list8(int x1, int x2, int x3, int x4, int x5, int x6, int x7, int x8);
int listcopy(int lis);
int reverse(int lis);
int reverse2(int lis);
int	last(int lis);
int butlast(int lis);
int atomp(int x);
int integerp(int x);
int mathematical_integerp(int x);
int positivep(int x);
int negativep(int x);
int exactp(int x);
int inexactp(int x);
int bignump(int x);
int floatp(int x);
int numberp(int x);
int realp(int x);
int rationalp(int x);
int complexp(int x);
int symbolp(int x);
int listp(int x);
int stringp(int x);
int charp(int x);
int improperp(int x);
int pairp(int x);
int nullp(int x);
int booleanp(int x);
int procedurep(int x);
int multvalp(int x);
int vectorp(int x);
int bytevectorp(int x);
int identifierp(int x);
int syntactic_closurep(int x);
int equalp(int x1, int x2);
int subrp(int x);
int closurep(int x);
int continuationp(int x);
int macro_namep(int sym);
int hygienic_namep(int sym);
int macrop(int x);
int setcar(int x, int y);
int setcdr(int x, int y);
int append(int x, int y);
int append2(int x, int y);

//compute.c
int integerp(int x);
int mathematical_integerp(int x);
int positivep(int x);
int negativep(int x);
int exactp(int x);
int inexactp(int x);
int bignump(int x);
int floatp(int x);
int numberp(int x);
int realp(int x);
int rationalp(int x);
int infinityp(int x);
int nanp(int x);
int complexp(int x);
int positive_zerop(int x);
int negative_zerop(int x);
int zerop(int x);
int numeqp(int x, int y);
int eqp(int x1, int x2);
int eqvp(int x1, int x2);
int smallerp(int x1, int x2);
int eqsmallerp(int x1, int x2);
int greaterp(int x1, int x2);
int eqgreaterp(int x1, int x2);
double bignumtofloat(int x);
int exact_to_inexact(int x);
int flttorat(int x);
int flttobig(int x);
int inexact_to_exact(int x);
int realtocomp(int x);
int inverse(int x);
int plus(int arg1, int arg2);
int minus(int arg1, int arg2);
int mult(int arg1, int arg2);
int divide(int arg1, int arg2);
int s_remainder(int x, int y);
int quotient(int x, int y);
int int_gcd(int x, int y);
int gcd(int x, int y);
int int_lcm(int m, int n);
int lcm(int x, int y);
int s_abs(int x);
int big_plus(int arg1, int arg2);
int big_plus1(int arg1, int arg2);
int big_rev(int arg);
int big_minus(int arg1, int arg2);
int big_minus1(int arg1, int arg2);
int big_mult(int arg1, int arg2);
int	big_mult1(int arg1, int arg2);
int big_int_mult(int arg1, int arg2);
int	big_quotient(int arg1, int arg2);
int	big_quotient1(int arg1, int arg2);
int big_int_quotient(int arg1, int arg2);
int norm_bignum(int arg);
int inttobignum(int x);
int bignumtoint(int x);
int big_eqp(int arg1, int arg2);
int big_greaterp(int arg1, int arg2);
int big_smallerp(int arg1, int arg2);
int big_integerizep(int x);
int big_positivep(int x);
int big_negativep(int x);
int	big_abs(int x);
int	big_sift(int x, int n);
int	big_int_remainder(int x, int y);

//cell.c
void initcell(void);
void initmodule(void);
int freshcell(void);
int get_tag(int addr);
int make_NIL(void);
int make_bool(char *name);
int returnbool(char *name);
int returninf(char *name);
int returnnan(char *name);
int make_inf(char *name);
int make_nan(char *name);
int make_int(int intn);
int addr_to_int(int addr);
int make_big(char *bignum);
int make_flt(double floatn);
int norm_rat(int x);
int make_rat(int n, int d);
void norm_comp(int x);
int make_comp(double real, double imag);
int make_comp1(int x, int y);
int hash(char *name);
int findsym(char *name, int index);
int addsym(char *name, int index);
int addsym1(int x, int index);
int make_sym(char *name);
int make_sym1(char *name);
int make_clo(void);
int make_macro(void);
int make_hygienic(void);
int make_synclo(char *name);
int make_cont(void);
int make_str(char *name);
int make_char(char *name);
int make_port(FILE *port, int type);
int make_vector(int n, int obj);
int make_u8vector(int n, unsigned char obj);
int make_record(int n, int obj);
int record_length(int v);
void record_set(int v, int n, int obj);
int record_ref(int v, int n);
void vector_set(int v, int n, int obj);
void u8vector_set(int v, int n, unsigned char obj);
int vector_ref(int v, int n);
unsigned char u8vector_ref(int v, int n);
int vector_length(int v);
int vector(int lis);
int u8vector(int lis);
int make_ident(char *name);
int make_env(int i, int j);
int make_code(int i);
int make_stack(void);
int make_memory(void);
int make_multiple_values(int lis);
int make_empty_set(void);
int get_int(int x);
int get_lvar(int i, int j);
void set_lvar(int i, int j, int val);
int remake(int x);

//function.c
int f_car(int n);
int f_cdr(int n);
int f_cons(int n);
int f_caar(int n);
int f_cdar(int n);
int f_cddr(int n);
int f_cadr(int n);
int f_caaar(int n);
int f_cdaar(int n);
int f_cadar(int n);
int f_caadr(int n);
int f_cddar(int n);
int f_caddr(int n);
int f_cdadr(int n);
int f_cdddr(int n);
int f_caaaar(int n);
int f_cdaaar(int n);
int f_cadaar(int n);
int f_caadar(int n);
int f_caaadr(int n);
int f_cddaar(int n);
int f_caddar(int n);
int f_caaddr(int n);
int f_cdaadr(int n);
int f_cdadar(int n);
int f_cadadr(int n);
int f_cadddr(int n);
int f_cdaddr(int n);
int f_cddadr(int n);
int f_cdddar(int n);
int f_cddddr(int n);
int f_assq(int n);
int f_assv(int n);
int f_assoc(int n);
int f_memq(int n);
int f_memv(int n);
int f_member(int n);
int f_reverse(int n);
int f_reverse2(int n);
int f_setcar(int n);
int f_setcdr(int n);
int f_append(int n);
int f_append2(int n);
int f_list(int n);
int f_makelist(int n);
int f_length(int n);
int f_pair_length(int n);
int f_listtail(int n);
int f_listref(int n);
int f_listset(int n);
int f_last(int n);
int	f_butlast(int n);
int f_nullp(int n);
int f_listp(int n);
int f_pairp(int n);
int f_boolp(int n);
int f_symbolp(int n);
int f_procedurep(int n);
int f_atomp(int n);
int f_eqp(int n);
int f_eqvp(int n);
int f_equalp(int n);
int f_numberp(int n);
int f_integerp(int n);
int f_realp(int n);
int f_rationalp(int n);
int f_complexp(int n);
int	f_exactp(int n);
int	f_inexactp(int n);
int f_stringp(int n);
int	f_characterp(int n);
int f_bignump(int n);
int	f_vectorp(int n);
int f_macrop(int n);
int f_macro_namep(int n);
int identifier_to_symbol(int x);
int f_hygienicp(int n);
int f_hygienic_namep(int n);
int f_make_vector(int n);
int f_vector_set(int n);
int f_vector_ref(int n);
int f_vector(int n);
int f_vector_length(int n);
int f_vector_fill(int n);
int f_vector_to_list(int n);
int	f_list_to_vector(int n);
int f_char_eqp(int n);
int f_char_ci_eqp(int n);
int f_char_greaterp(int n);
int f_char_ci_greaterp(int n);
int f_char_eqgreaterp(int n);
int f_char_ci_eqgreaterp(int n);
int f_char_smallerp(int n);
int f_char_ci_smallerp(int n);
int f_char_eqsmallerp(int n);
int f_char_ci_eqsmallerp(int n);
int f_char_alphabeticp(int n);
int f_char_numericp(int n);
int f_char_whitespacep(int n);
int f_char_upper_casep(int n);
int f_char_lower_casep(int n);
int f_char_to_integer(int n);
int f_integer_to_char(int n);
int f_char_upcase(int n);
int f_char_downcase(int n);
int f_string_eqp(int n);
int f_string_ci_eqp(int n);
int f_string_greaterp(int n);
int f_string_ci_greaterp(int n);
int f_string_eqgreaterp(int n);
int f_string_ci_eqgreaterp(int n);
int f_string_smallerp(int n);
int f_string_ci_smallerp(int n);
int f_string_eqsmallerp(int n);
int f_string_ci_eqsmallerp(int n);
int	f_string_append(int n);
int f_number_to_string(int n);
int f_string_to_number(int n);
int f_string_to_symbol(int n);
int f_symbol_to_string(int n);
int f_string_length(int n);
int f_make_string(int n);
int f_string(int n);
int	f_string_ref(int n);
int f_string_set(int n);
int f_substring(int n);
int f_string_to_list(int n);
int f_list_to_string(int n);
int f_string_copy(int n);
int f_string_fill(int n);
int f_plus(int n);
int f_minus(int n);
int f_mult(int n);
int f_div(int n);
int f_smallerp(int n);
int f_eqsmallerp(int n);
int f_greaterp(int n);
int f_eqgreaterp(int n);
int f_numeqp(int n);
int f_oddp(int n);
int f_evenp(int n);
int f_positivep(int n);
int f_negativep(int n);
int f_abs(int n);
int f_max(int n);
int f_min(int n);
int	f_remainder(int n);
int f_modulo(int n);
int	f_quotient(int n);
int f_gcd(int n);
int f_lcm(int n);
int f_floor(int n);
int f_ceiling(int n);
int f_truncate(int n);
int f_round(int lvar);
int f_numerator(int n);
int f_denominator(int n);
int f_sin(int n);
int f_asin(int n);
int f_cos(int n);
int f_acos(int n);
int	f_tan(int n);
double argz(double x, double y);
int f_atan(int n);
int f_log(int n);
int f_exp(int n);
int f_sqrt(int n);
int f_expt(int n);
int square(int x);
int expt(int x, int y);
int f_zerop(int n);
int f_realpart(int n);
int f_imagpart(int n);
int f_magnitude(int n);
int f_angle(int n);
int f_make_rectangular(int n);
int f_make_polar(int n);
int	f_exact_inexact(int n);
int	f_inexact_exact(int n);
int f_not(int n);
int f_read(int n);
int f_read_char(int n);
int f_peek_char(int n);
int f_read_line(int n);
int f_read_string(int n);
int	f_char_readyp(int n);
int f_load(int n);
int	f_open_input_file(int n);
int f_input_portp(int n);
int f_close_input_port(int n);
int f_eof_objectp(int n);
int f_current_input_port(int n);
int	f_open_output_file(int n);
int f_close_output_port(int n);
int f_output_portp(int n);
int f_current_output_port(int n);
int f_newline(int n);
void display_str(int x);
int output_portp(int x);
int f_display(int n);
int f_write(int n);
int f_write_char(int n);
int f_exit(int n);
int f_call_cc(int n);
int	f_gensym(int n);
int f_apply(int n);
int f_primitive_name_p(int n);
int f_macroexpand1(int n);
int macroexpand1(int x);
int f_macroexpand(int n);
int macroexpand(int x);
int	f_addr(int n);
int f_undefined(int n);
int f_vm1_step(int n);
int f_vm2_step(int n);
int f_vm1(int n);
int f_dump(int n);
int f_addr_prt(int n);
int f_entity_addr(int n);
int f_gbc(int n);
int f_room(int n);
int f_freecell(int n);
int f_vmcode(int n);
int f_timer_set(int n);
int f_timer_get(int n);
int f_timer_gbc(int n);
int f_current_second(int n);
int f_current_jiffy(int n);
int f_jiffies_per_second(int n);
int f_eval(int n);
int f_inspect(int n);
int f_env(int n);
int f_step(int n);
int f_vm2(int n);
int f_lambda_asm(int n);
int f_error(int n);
int f_flush(int n);
int f_flush_output_port(int n);
int f_set_trace(int n);
int f_set_untrace(int n);
int f_debug(int n);
int f_prof(int n);
int f_current_module(int n);
int f_values(int n);
int f_sys_cont_room(int n);
int f_make_syntactic_closure(int n);
int f_syntactic_closure_expr(int n);
int f_syntactic_closure_env(int n);
int f_syntactic_closure_freevar(int n);
int f_symbol_to_identifier(int n);
int f_identifier_to_symbol(int n);
int f_syntactic_closurep(int n);
int f_identifierp(int n);
int f_identifier_bind(int n);
int f_identifier_freep(int n);
int f_identifier_boundp(int n);
int f_identifier_bound(int n);
int f_identifier_variable(int n);
int f_identifier_variablep(int n);
int f_identifier_ellipsis(int n);
int f_identifier_ellipsisp(int n);
int f_global_boundp(int n);
int f_exact_integerp(int n);
int f_file_existsp(int n);
int f_system(int n);
int f_infinityp(int n);
int f_finityp(int n);
int f_nanp(int n);
int f_square(int n);
int f_bytevectorp(int n);
int f_make_bytevector(int n);
int f_bytevector(int n);
int f_bytevector_length(int n);
int f_bytevector_u8_set(int n);
int f_bytevector_u8_ref(int n);
int f_bytevector_copy(int n);
int f_bytevector_copy2(int n);
int f_bytevector_append(int n);
int f_command_line(int n);
int f_get_environment_variable(int n);
int f_get_environment_variables(int n);
int f_get_car(int n);
int f_make_record(int n);

void defsubr(char *name, int func);
void defsyntax(char *name);
void initsubr(void);
void initsyntax(void);

//r7rs.c
void init_r7rs(void);

