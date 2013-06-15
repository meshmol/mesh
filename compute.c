
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <setjmp.h>
#include <limits.h>
#include <math.h>
#include "norm.h"

extern cell memory[];
extern jmp_buf toplevel;


//-------------------------------------------------
int integerp(int x){
	if(IS_INTEGER(x))
    	return(1);
    else
    	return(0);
}

int mathematical_integerp(int x){
	double f;
    
	if(IS_INTEGER(x))
    	return(1);
    else if(IS_BIGNUM(x))
    	return(1);
    else{
    	f = GET_FLT(x);
        if(IS_FLOAT(x) && (ceil(f) == floor(f)))
        	return(1);
    }
    return(0);
}

int positivep(int x){
	
    if(integerp(x) && get_int(x) > 0)
        return(1);
    else if(bignump(x) && big_positivep(x))
        return(1);
    else if(floatp(x) && GET_FLT(x) > 0)
    	return(1);
    else if(rationalp(x) && GET_CAR(x) > 0)
    	return(1);
    else
    	return(0);
}

int negativep(int x){
	
    if(integerp(x) && get_int(x) < 0)
    	return(1);
    else if(bignump(x) && big_negativep(x))
    	return(1);
    else if(floatp(x) && GET_FLT(x) < 0)
    	return(1);
    else if(rationalp(x) && GET_CAR(x) < 0)
    	return(1);
    else
    	return(0);
}

int exactp(int x){
	if(integerp(x) || bignump(x) || rationalp(x))
    	return(1);
    else
    	return(0);
}

int inexactp(int x){
	if(exactp(x))
    	return(0);
    else
    	return(1);
}

int bignump(int x){
	if(!IS_INTEGER(x) && IS_BIGNUM(x))
    	return(1);
    else
    	return(0);
}

int floatp(int x){
	if(!IS_INTEGER(x) && IS_FLOAT(x))
    	return(1);
    else
    	return(0);
}	

int numberp(int x){	
    if(IS_INTEGER(x) || (IS_FLOAT(x)) || IS_RATIONAL(x) || IS_BIGNUM(x) 
    	|| IS_COMPLEX(x) || IS_INF(x) || IS_NAN(x))
    	return(1);
    else
    	return(0);
}

int realp(int x){
	if(IS_INTEGER(x) || (IS_FLOAT(x)) || IS_RATIONAL(x) || IS_BIGNUM(x)
    	|| IS_INF(x) || IS_NAN(x))
    	return(1);
    else
    if(IS_COMPLEX(x) && (GET_IMAG_FLT(x) == 0))
    	return(1);
    else
    	return(0);
}

int rationalp(int x){
	if(!IS_INTEGER(x) && IS_RATIONAL(x))
    	return(1);
    else
    	return(0);
}

int infinityp(int x){
	
    if(IS_INF(x))
    	return(1);
    else if(IS_COMPLEX(x) && (IS_INF(GET_CAR(x) || IS_INF(GET_CDR(x)))))
    	return(1);
    else
    	return(0);
}

int nanp(int x){
	
    if(IS_NAN(x))
    	return(1);
    else if(IS_COMPLEX(x) && (IS_NAN(GET_CAR(x)) || IS_NAN(GET_CDR(x))))
    	return(1);
    else
    	return(0);
}

int complexp(int x){
	if(!IS_INTEGER(x) && IS_COMPLEX(x))
    	return(1);
    else
    	return(0);
}

int positive_zerop(int x){
	double m,n;
    
    if(!IS_FLOAT(x))
    	return(0);
    if(!zerop(x))
    	return(0);
    m = GET_FLT(x);
    n = 1 / m;
    if(n > 0)
    	return(1);
    else
    	return(0);
	
}

int negative_zerop(int x){
	double m,n;
    
    if(!IS_FLOAT(x))
    	return(0);
    if(!zerop(x))
    	return(0);
    m = GET_FLT(x);
    n = 1 / m;
    if(n < 0)
    	return(1);
    else
    	return(0);
    	
}

//比較
int zerop(int x){
	int arg;
    
    if(integerp(x) && get_int(x) == 0)
    	return(1);
    
	arg = realtocomp(x);
    if(GET_REAL_FLT(arg) == 0 && GET_IMAG_FLT(arg) == 0)
    	return(1);
    else
    	return(0);
}


int numeqp(int x, int y){
	int arg1,arg2;
    //整数型であってアドレスが同じならば等しい。
    if(IS_INTEGER(x) && IS_INTEGER(y)){
     	if(x == y)
    		return(1);
        else
        	return(0);
    }
	//bignumであって全部のセルが同じ数ならば等しい。
    if(bignump(x) && bignump(y)){
    	if(big_eqp(x,y))
        	return(1);
        else
        	return(0);
    }
	//複素数同市の場合には実部、虚部が等しいならば等しい。
    if(complexp(x) && complexp(y)){
    	if(GET_REAL_FLT(x) == GET_REAL_FLT(y) &&
           GET_IMAG_FLT(x) == GET_IMAG_FLT(y))
        	return(1);
        else
        	return(0);
    }
    //inf.nanの場合にはセルアドレスが同じならば等しい。
    if((infinityp(x) || nanp(x)) && (infinityp(y) || nanp(y))){
    	if(x == y)
        	return(1);
		else if(car(x) == car(y) && cdr(x) == cdr(y)) //複素数の場合
        	return(1);
        else
        	return(0);
    }
    //そうでなければ複素数に変換して実部、虚部ともに等しいならば等しい。	
    arg1 = realtocomp(x);
    arg2 = realtocomp(y);
    if((GET_REAL_FLT(arg1) == GET_REAL_FLT(arg2)) &&
       (GET_IMAG_FLT(arg1) == GET_IMAG_FLT(arg2)))
       	return(1);
    else
    	return(0);
}

int eqp(int x1, int x2){
	//番地が同じなら等しい
    if(x1 == x2)
    	return(1);
    if(!integerp(x1) && !integerp(x2)){
	//数値であって＝ならば等しい
    	if(numberp(x1) && numberp(x2) && IS_SAME_TYPE(x1,x2) && (numeqp(x1,x2)))
        	return(1);
    	else
    		return(0);
    }
    else
    	return(0);
}


int eqvp(int x1, int x2){
	//番地が同じなら等しい。
    if(x1 == x2)
    	return(1);
	//印字名が同じ文字列なら等しい
    if(stringp(x1) && stringp(x2) && (SAME_NAME(x1,x2)))
		return(1);
    //印字名が同じ文字なら等しい。
    if(charp(x1) && charp(x2) && (SAME_NAME(x1,x2)))
    	return(1);
    //数値であって＝ならば等しい
    if(numberp(x1) && numberp(x2) && IS_SAME_TYPE(x1,x2) && (numeqp(x1,x2)))
        return(1);
    //シンボルあるいは識別子であって印字名が同じなら等しい。（コンパイラの都合）
    if((symbolp(x1) || IS_IDENTIFIER(x1)) && (symbolp(x2) || IS_IDENTIFIER(x2)) 
    	&& (SAME_NAME(x1,x2)))
        return(1);
    
    else
    	return(0);

}


int smallerp(int x1, int x2){
	int arg1,arg2;
	if(IS_INTEGER(x1) && IS_INTEGER(x2)){
    	if(x1 < x2)
    		return(1);
        else
        	return(0);
    }
    if(integerp(x1) && bignump(x2))
    	return(1);

	if(bignump(x1) && bignump(x2)){
    	if(big_smallerp(x1,x2))
    		return(1);
        else
        	return(0);
    }
    if(realp(x1) && realp(x2)){
    	arg1 = exact_to_inexact(x1);
        arg2 = exact_to_inexact(x2);
        if(GET_FLT(arg1) < GET_FLT(arg2))
        	return(1);
    }
    if(complexp(x1))
    	exception("<",ILLEGAL_ARGUMENT, x1);
    if(complexp(x2))
    	exception("<", ILLEGAL_ARGUMENT, x2);
    
    return(0);

}

int eqsmallerp(int x1, int x2){
	int arg1,arg2;
	if(IS_INTEGER(x1) && IS_INTEGER(x2)){
    	if(x1 <= x2)
    		return(1);
        else
        	return(0);
    }
    if(integerp(x1) && bignump(x2))
    	return(1);
    if(bignump(x1) && bignump(x2)){
    	 if(big_smallerp(x1,x2) || big_eqp(x1,x2))
    		return(1);
         else
         	return(0);
    }
    if(realp(x1) && realp(x2)){
    	arg1 = exact_to_inexact(x1);
        arg2 = exact_to_inexact(x2);
        if(GET_FLT(arg1) <= GET_FLT(arg2))
        	return(1);
    }
    if(complexp(x1))
    	exception("<=", ILLEGAL_ARGUMENT, x1);
    if(complexp(x2))
    	exception("<=", ILLEGAL_ARGUMENT, x2);
    return(0);
}

int greaterp(int x1, int x2){
	if(smallerp(x2,x1))
    	return(1);
    else
    	return(0);
}

int eqgreaterp(int x1, int x2){
	if(eqsmallerp(x2,x1))
    	return(1);
    else
    	return(0);
}

//-------------------------------------------------
//数値型変換
double bignumtofloat(int x){
	double val;
    int i,sgn,rev;
    
    rev = reverse(x);
	val = sgn = GET_INT(car(rev));
    rev = cdr(rev);
    while(!nullp(rev)){
    	i = GET_INT(car(rev));
        if(sgn > 0)
    		val = val * BIGNUM_BASE + (double)i;
        else
        	val = val * BIGNUM_BASE - (double)i;
        rev = cdr(rev);
    }
    return(val);
}



int exact_to_inexact(int x){
	int m,n,res,tag;
    double val;
    
    tag = GET_TAG(x);
    switch(tag){
    	case INTN: 	res = freshcell();
    				SET_TAG(res,FLTN);
        			val = (double)GET_INT(x);
        			SET_FLT(res,val);
        			return(res);
        case BIG:  	res = freshcell();
    				SET_TAG(res,FLTN);
        			val = bignumtofloat(x);
        			SET_FLT(res,val);
                    return(res);
        case FLTN: 	return(x);
        case RAT:  	res = freshcell();
    				SET_TAG(res,FLTN);
        			n = GET_CAR(x);
        			m = GET_CDR(x);
                    val = (double)n/m;
                    SET_FLT(res,val);
                    return(res);
    
        case COMP: 	return(x);
    }
    return(undef);
}

int flttorat(int x){
	double flt;
    int numer,denom,digit;
    
    flt = GET_FLT(x);
    denom = 1;
    digit = 0;
    while((fabs(flt) - floor(fabs(flt))) > 0.000000001){
    	if(digit > 8)
        	break;
        flt = flt * 10;
        denom = denom * 10;
        digit++;
    }
    numer = (int)floor(flt);
    return(make_rat(numer,denom));
}

int flttobig(int x){
	double flt;
    int big,res;
    
    flt = GET_FLT(x);
    res = NIL;
    while(fabs(flt) >= BIGNUM_BASE){
    	big = (int)fmod(fabs(flt),BIGNUM_BASE);
        if(flt >= 0)
        	flt = floor(flt / BIGNUM_BASE);
        else
        	flt = ceil(flt / BIGNUM_BASE);
    	res = cons(make_int(big),res);    
    }
    res = cons(make_int((int)flt),res);
    res = reverse2(res);
    SET_TAG(res,BIG);
    return(res);  
}

int inexact_to_exact(x){
	int tag;
    
    tag = GET_TAG(x);
	switch(tag){
    	case INTN:
        case BIG:
        case RAT:
        case COMP:	return(x);
        case FLTN:	if(fabs(GET_FLT(x)) < BIGNUM_BASE)
        				return(flttorat(x));
        			else
                    	return(flttobig(x));
    }
    return(undef);
}

int realtocomp(int x){
	int m,n,res,tag;
    double val;
    
	res = make_comp(0,0);
    tag = GET_TAG(x);
	switch(tag){
    	case INTN: {SET_REAL_FLT(res,(double)GET_INT(x));
                    SET_IMAG_FLT(res,0);
                    break;}
        case BIG:  {val = bignumtofloat(x);
        			SET_REAL_FLT(res,val);
                    SET_IMAG_FLT(res,0);
                    break;}
        case FLTN: {SET_REAL_FLT(res,GET_FLT(x));
        			SET_IMAG_FLT(res,0);
        			break;}
        case RAT:  {n = GET_CAR(x);
        			m = GET_CDR(x);
                    val = (double)n/m;
                    SET_REAL_FLT(res,val);
                    SET_IMAG_FLT(res,0);
                    break;}
        case COMP:	return(x);
	}
    return(res);
}

int inverse(int x){
	int tag;
    
    tag = GET_TAG(x);
    switch(tag){
    	case INTN:	mult(make_int(-1),x);
        case FLTN:	mult(make_flt(-1.0),x);
        case RAT:	mult(make_int(-1),x);
        case BIG:	mult(make_int(-1),x);
        case NANN:	if(x == PNAN)
        				return(MNAN);
        			else
                    	return(PNAN);
        case INF:	if(x == PINF)
        				return(MINF);
        			else
                    	return(PINF);
    }
    return(undef);
}


//-----------------------------------
//四則演算

int plus(int arg1, int arg2){
	int n,m,s,t,u,tag1,tag2,i;
    double x1,y1,x2,y2;
    
    if(integerp(arg1) && integerp(arg2)){
    	i = GET_INT(arg1) + GET_INT(arg2);
        if(SMALL_INT_MIN < i && i < SMALL_INT_MAX)
        	return(make_int(i));
        else
        	return(big_plus(inttobignum(arg1),inttobignum(arg2)));    
    }

    tag1 = GET_TAG(arg1);
    tag2 = GET_TAG(arg2);
	switch(tag1){
    	case INTN:
        	switch(tag2){
                case FLTN: {n = GET_INT(arg1);
                			x1 = (double)n;
                            y1 = GET_FLT(arg2);
                            return(make_flt(x1+y1));}
                            
                case RAT:  {n = GET_INT(arg1);
                			s = GET_CAR(arg2);
                            t = GET_CDR(arg2);
                            return(make_rat(n*t+s,t));}
                            
                case BIG:  return(big_plus(inttobignum(arg1),arg2));
                
                case COMP: {n = GET_INT(arg1);
                			x1 = (double)n;
                            s = BOOLF;
                            t = BOOLF;
                            if(infinityp(car(arg2)) || nanp(car(arg2))){
                            	if(nanp(car(arg2)))
                                	s = PNAN;
                                else
                            		s = car(arg2);
                            }
                            if(infinityp(cdr(arg2)) || nanp(cdr(arg2))){
                            	if(nanp(cdr(arg2)))
                                	t = PNAN;
                                else
                                	t = cdr(arg2);
                            }
                            if(s == BOOLF){
                            	x2 = GET_REAL_FLT(arg2);
                                s = make_flt(x1+x2);
                            }
                            if(t == BOOLF){	
                            	y2 = GET_IMAG_FLT(arg2);
                                t = make_flt(y2);
                			}
                            return(make_comp1(s,t));}
                
                case INF:	return(arg2);
                case NANN:	return(PNAN); 
            }
        case BIG:
        	switch(tag2){
            	case INTN:	if(get_int(arg2) != 0)
                				return(big_plus(arg1,inttobignum(arg2)));
                			else
                            	return(arg1);
                                
            	case BIG: 	return(big_plus(arg1,arg2));
            }
        case FLTN:
        	switch(tag2){
            	case INTN: {x1 = GET_FLT(arg1);
                			s = GET_INT(arg2);
                            x2 = (double)s;
                            return(make_flt(x1+x2));}
                case FLTN: {x1 = GET_FLT(arg1);
                			x2 = GET_FLT(arg2);
                            return(make_flt(x1+x2));}
                case RAT:  {x1 = GET_FLT(arg1);
                			s = GET_CAR(arg2);
                            t = GET_CDR(arg2);
                            x2 = (double)s/t;
                            return(make_flt(x1+x2));}
                case BIG:	return(BOOLF);
                case COMP: {x1 = GET_FLT(arg1);
                			s = BOOLF;
                            t = BOOLF;
                			if(infinityp(car(arg2)) || nanp(car(arg2))){
                            	if(nanp(car(arg2)))
                                	s = PNAN;
                                else
                            		s = car(arg2);
                            }
                            if(infinityp(cdr(arg2)) || nanp(cdr(arg2))){
                            	if(nanp(cdr(arg2)))
                                	t = PNAN;
                                else
                                	t = cdr(arg2);
                            }
                            if(s == BOOLF){
                            	x2 = GET_REAL_FLT(arg2);
                                s = make_flt(x1+x2);
                            }
                            if(t == BOOLF){	
                            	y2 = GET_IMAG_FLT(arg2);
                                t = make_flt(y2);
                			}
                            return(make_comp1(s,t));}
                
                case INF:	return(arg2);
                
                case NANN:	return(PNAN);    
                            
        	}
        case RAT:
        	switch(tag2){
            	case INTN: {n = GET_CAR(arg1);
                			m = GET_CDR(arg1);
                            s = GET_INT(arg2);
                            return(make_rat(n+s*m,m));}
                case FLTN: {n = GET_CAR(arg1);
                			m = GET_CDR(arg1);
                            x1 = (double)n/m;
                            x2 = GET_FLT(arg2);
                            return(make_flt(x1+x2));}
                case RAT:  {n = GET_CAR(arg1);
                			m = GET_CDR(arg1);
                            s = GET_CAR(arg2);
                            t = GET_CDR(arg2);
                            u = int_lcm(m,t);
                            n = n*(u/m);
                            s = s*(u/t);
                            return(make_rat(n+s,u));}
                case BIG:	return(BOOLF);
                case COMP: {n = GET_CAR(arg1);
                			m = GET_CDR(arg1);
                            x1 = (double)n/m;
                            x2 = GET_REAL_FLT(arg2);
                            y2 = GET_IMAG_FLT(arg2);
                            return(make_comp(x1+x2,y2));}
        }
        case COMP:
        	switch(tag2){
            	case INTN: {x1 = GET_FLT(arg1);
                			y1 = GET_IMAG_FLT(arg1);
                            n = GET_INT(arg2);
                            x2 = (double)n;
                            return(make_comp(x1+x2,y1));}
                            
                case FLTN: {x1 = GET_FLT(arg1);
                			y1 = GET_IMAG_FLT(arg1);
                            x2 = GET_FLT(arg2);
                            return(make_comp(x1+x2,y1));}
                            
                case RAT:  {x1 = GET_FLT(arg1);
                			y1 = GET_IMAG_FLT(arg1);
                            s = GET_CAR(arg2);
                            t = GET_CDR(arg2);
                            x2 = (double)s/t;
                            return(make_comp(x1+x2,y1));}
                            
                case BIG:	return(BOOLF);
                
                case COMP: {x1 = GET_REAL_FLT(arg1);
                			y1 = GET_IMAG_FLT(arg1);
                            x2 = GET_REAL_FLT(arg2);
                            y2 = GET_IMAG_FLT(arg2);
                            return(make_comp(x1+x2,y1+y2));}
            }
        case INF:
        	switch(tag2){
            	case INTN:
                case FLTN:
                case RAT:
                case BIG:
                case INF:	if(arg1 == arg2)
                				return(arg1);
                			else
                            	return(PNAN);
                case NANN:	return(PNAN);
                case COMP: {y2 = GET_IMAG_FLT(arg2);
                            return(make_comp1(arg1,make_flt(y2)));}
            }
        case NANN:
        	switch(tag2){
            	case INTN:
                case FLTN:
                case RAT:
                case BIG:
                case INF:	return(arg1);
                case NANN:	return(PNAN);
                case COMP: {y2 = GET_IMAG_FLT(arg2);
                            return(make_comp1(PNAN,make_flt(y2)));}               
    		}
    }
    return(undef);
}


int minus(int arg1, int arg2){
	int n,m,s,t,u,tag1,tag2,i;
    double x1,y1,x2,y2;
    
    if(integerp(arg1) && integerp(arg2)){
    	i = GET_INT(arg1) - GET_INT(arg2);
        if(SMALL_INT_MIN < i && i < SMALL_INT_MAX)
        	return(make_int(i));
        else
        	return(big_minus(inttobignum(arg1),inttobignum(arg2)));    
    }
    	
    tag1 = GET_TAG(arg1);
    tag2 = GET_TAG(arg2);
	switch(tag1){
    	case INTN:
        	switch(tag2){
                case FLTN: {n = GET_INT(arg1);
                			x1 = (double)n;
                            y1 = GET_FLT(arg2);
                            return(make_flt(x1-y1));}
                
                case RAT:  {n = GET_INT(arg1);
                			s = GET_CAR(arg2);
                            t = GET_CDR(arg2);
                            return(make_rat(n*t-s,t));}
				
                case BIG:  return(big_minus(inttobignum(arg1),arg2));
                
                case COMP: {n = GET_INT(arg1);
                			x1 = (double)n;
                            x2 = GET_REAL_FLT(arg2);
                            y2 = GET_IMAG_FLT(arg2);
                            return(make_comp(x1-x2,-y2));}
                
                case INF:	return(inverse(arg2));
                case NANN:	return(PNAN);    
            }
        case BIG:
        	switch(tag2){
            	case INTN:	if(get_int(arg2) != 0)
                				return(big_minus(arg1,inttobignum(arg2)));
                			else
                            	return(arg1);
                                
            	case BIG:	return(big_minus(arg1,arg2));
            }
        case FLTN:
        	switch(tag2){
            	case INTN: {x1 = GET_FLT(arg1);
                			s = GET_INT(arg2);
                            x2 = (double)s;
                            return(make_flt(x1-x2));}
                case FLTN: {x1 = GET_FLT(arg1);
                			x2 = GET_FLT(arg2);
                            return(make_flt(x1-x2));}
                case RAT:  {x1 = GET_FLT(arg1);
                			s = GET_CAR(arg2);
                            t = GET_CDR(arg2);
                            x2 = (double)s/t;
                            return(make_flt(x1-x2));}
                case BIG:	return(BOOLF);
                case COMP: {x1 = GET_REAL_FLT(arg1);
                			x2 = GET_REAL_FLT(arg2);
                            y2 = GET_IMAG_FLT(arg2);
                            return(make_comp(x1-x2,-y2));}
        	}
        case RAT:
        	switch(tag2){
            	case INTN: {n = GET_CAR(arg1);
                			m = GET_CDR(arg1);
                            s = GET_INT(arg2);
                            return(make_rat(n-s*m,m));}
                case FLTN: {n = GET_CAR(arg1);
                			m = GET_CDR(arg1);
                            x1 = (double)n/m;
                            x2 = GET_FLT(arg2);
                            return(make_flt(x1-x2));}
                case RAT:  {n = GET_CAR(arg1);
                			m = GET_CDR(arg1);
                            s = GET_CAR(arg2);
                            t = GET_CDR(arg2);
                            u = int_lcm(m,t);
                            n = n*(u/m);
                            s = s*(u/t);
                            return(make_rat(n-s,u));}
                case BIG:	return(BOOLF);
                case COMP: {n = GET_CAR(arg1);
                			m = GET_CDR(arg1);
                            x1 = (double)n/m;
                            x2 = GET_REAL_FLT(arg2);
                            y2 = GET_IMAG_FLT(arg2);
                            return(make_comp(x1-x2,y2));}
        }
        case COMP:
        	switch(tag2){
            	case INTN: {x1 = GET_REAL_FLT(arg1);
                			y1 = GET_IMAG_FLT(arg1);
                            n = GET_INT(arg2);
                            x2 = (double)n;
                            y2 = 0;
                            return(make_comp(x1-x2,y2));}
                case FLTN: {x1 = GET_FLT(arg1);
                			y1 = GET_IMAG_FLT(arg1);
                            x2 = GET_FLT(arg2);
                            return(make_comp(x1-x2,y1));}
                case RAT:  {x1 = GET_FLT(arg1);
                			y1 = GET_IMAG_FLT(arg1);
                            s = GET_CAR(arg2);
                            t = GET_CDR(arg2);
                            x2 = (double)s/t;
                            return(make_comp(x1-x2,y1));}
                case BIG:	return(BOOLF);
                case COMP: {x1 = GET_REAL_FLT(arg1);
                			y1 = GET_IMAG_FLT(arg1);
                            x2 = GET_REAL_FLT(arg2);
                            y2 = GET_IMAG_FLT(arg2);
                            return(make_comp(x1-x2,y1-y2));}
            }
        case INF:
        	switch(tag2){
            	case INTN:
                case FLTN:
                case RAT:
                case BIG:
                case INF:	if(arg1 != arg2)
                				return(arg1);
                			else
                            	return(PNAN);
                case NANN:	return(PNAN);
                case COMP: {y2 = GET_IMAG_FLT(arg2);
                            return(make_comp1(arg1,make_flt(-y2)));}
            }
        case NANN:
        	switch(tag2){
            	case INTN:
                case FLTN:
                case RAT:
                case BIG:
                case INF:	return(arg1);
                case NANN:	return(PNAN);
                case COMP: {y2 = GET_IMAG_FLT(arg2);
                            return(make_comp1(arg1,make_flt(-y2)));}               
    		}               
    }
    return(undef); 
}


int mult(int arg1, int arg2){
	int n,m,s,t,tag1,tag2;
    long long int l,l1,l2;
    double x1,y1,x2,y2;
    
    tag1 = GET_TAG(arg1);
    tag2 = GET_TAG(arg2);
    
	switch(tag1){
    	case INTN:
        	switch(tag2){
        		case INTN: {l1 = (long long int)GET_INT(arg1);
                			l2 = (long long int)GET_INT(arg2);
                            l = l1 * l2;
                            if(l < SMALL_INT_MAX && l > SMALL_INT_MIN)
                            	return(make_int((int)l));
                            else
                            	return(big_mult(inttobignum(arg1),inttobignum(arg2))); }
                                
                case FLTN: {n = GET_INT(arg1);
                			x1 = (double)n;
                            y1 = GET_FLT(arg2);
                            return(make_flt(x1*y1));}
                            
                case RAT:  {n = GET_INT(arg1);
                			s = GET_CAR(arg2);
                            t = GET_CDR(arg2);
                            return(make_rat(n*s,t));}
                            
                case BIG:   if(get_int(arg1) != 0)
                				return(big_int_mult(arg2,arg1));
                			else
                            	return(arg1); //int 0
            
                case COMP: {n = GET_INT(arg1);
                			x1 = (double)n;
                            x2 = GET_REAL_FLT(arg2);
                            y2 = GET_IMAG_FLT(arg2);
                            return(make_comp(x1*x2,x1*y2));}   
            }
        case BIG:
        	switch(tag2){
            	case INTN:	if(get_int(arg2) != 0)
                				return(big_int_mult(arg1,arg2));
                			else
                            	return(arg2); //int 0
                                
            	case BIG:	return(big_mult(arg1,arg2));
            }
        
        case FLTN:
        	switch(tag2){
            	case INTN: {x1 = GET_FLT(arg1);
                			s = GET_INT(arg2);
                            x2 = (double)s;
                            return(make_flt(x1*x2));}
                case FLTN: {x1 = GET_FLT(arg1);
                			x2 = GET_FLT(arg2);
                            return(make_flt(x1*x2));}
                case RAT:  {x1 = GET_FLT(arg1);
                			s = GET_CAR(arg2);
                            t = GET_CDR(arg2);
                            x2 = (double)s/t;
                            return(make_flt(x1*x2));}
                case BIG:	return(BOOLF);
                case COMP: {x1 = GET_REAL_FLT(arg1);
                			x2 = GET_REAL_FLT(arg2);
                            y2 = GET_IMAG_FLT(arg2);
                            return(make_comp(x1*x2,x1*y2));}
        	}
        case RAT:
        	switch(tag2){
            	case INTN: {n = GET_CAR(arg1);
                			m = GET_CDR(arg1);
                            s = GET_INT(arg2);
                            return(make_rat(n*s,m));}
                case FLTN: {n = GET_CAR(arg1);
                			m = GET_CDR(arg1);
                            x1 = (double)n/m;
                            x2 = GET_FLT(arg2);
                            return(make_flt(x1*x2));}
                case RAT:  {n = GET_CAR(arg1);
                			m = GET_CDR(arg1);
                            s = GET_CAR(arg2);
                            t = GET_CDR(arg2);
                            return(make_rat(n*s,m*t));}
                case BIG:	return(BOOLF);
                case COMP: {n = GET_CAR(arg1);
                			m = GET_CDR(arg1);
                            x1 = (double)n/m;
                            x2 = GET_REAL_FLT(arg2);
                            y2 = GET_IMAG_FLT(arg2);
                            return(make_comp(x1*x2,x1*y2));}
        }
        case COMP:
        	switch(tag2){
            	case INTN: {x1 = GET_REAL_FLT(arg1);
                			y1 = GET_IMAG_FLT(arg1);
                            n = GET_INT(arg2);
                            x2 = (double)n;
                            return(make_comp(x1*x2,y1*x2));}
                case FLTN: {x1 = GET_REAL_FLT(arg1);
                			y1 = GET_IMAG_FLT(arg1);
                            x2 = GET_FLT(arg2);
                            return(make_comp(x1*x2,y1*x2));}
                case RAT:  {x1 = GET_FLT(arg1);
                			y1 = GET_IMAG_FLT(arg1);
                            s = GET_CAR(arg2);
                            t = GET_CDR(arg2);
                            x2 = (double)s/t;
                            return(make_comp(x1*x2,y1*x2));}
                case BIG:	return(BOOLF);
                case COMP: {x1 = GET_REAL_FLT(arg1);
                			y1 = GET_IMAG_FLT(arg1);
                            x2 = GET_REAL_FLT(arg2);
                            y2 = GET_IMAG_FLT(arg2);
                            return(make_comp(x1*x2-y1*y2,x1*y2+y1*x2));}
            }
        case INF:
        	switch(tag2){
            	case INTN:
                case FLTN:
                case RAT:
                case BIG:
                case INF:	if(arg1 == arg2)
                				return(PINF);
                			else
                            	return(MINF);
                case NANN:	return(PNAN);
                case COMP: 	return(make_comp1(arg1,arg1));
            }
        case NANN:
        	switch(tag2){
            	case INTN:
                case FLTN:
                case RAT:
                case BIG:
                case INF:	return(arg1);
                case NANN:	return(PNAN);
                case COMP: 	return(make_comp1(PNAN,PNAN));               
    		}                       
    }
    return(undef);  
}


int divide(int arg1, int arg2){
	int n,m,s,t,tag1,tag2;
    double x1,y1,x2,y2;
    
    tag1 = GET_TAG(arg1);
    tag2 = GET_TAG(arg2); 
	switch(tag1){
    	case INTN:
        	switch(tag2){
        		case INTN: {n = GET_INT(arg1); 
                			s = GET_INT(arg2);
                            return(make_rat(n,s));}
                case FLTN: {if(positivep(arg1) && positive_zerop(arg2))
                            	return(PINF); //+inf.0
                			if(negativep(arg1) && positive_zerop(arg2))
                            	return(MINF); //-inf.0
                            if(positivep(arg1) && negative_zerop(arg2))
                            	return(MINF); //-inf.0
                            if(negativep(arg1) && negative_zerop(arg2))
                            	return(PINF); //+inf.0
                            if(zerop(arg1) && zerop(arg2))
                            	return(PNAN); //+nan.0
                			n = GET_INT(arg1);
                			x1 = (double)n;
                            y1 = GET_FLT(arg2);
                            return(make_flt(x1/y1));}
                case RAT:  {n = GET_INT(arg1);
                			s = GET_CAR(arg2);
                            t = GET_CDR(arg2);
                            return(make_rat(n*t,s));}
                case BIG:   exception("/", ILLEGAL_ARGUMENT, arg2);
            
                case COMP: {n = GET_INT(arg1);
                			x1 = (double)n;
                            y1 = 0;
                            x2 = GET_REAL_FLT(arg2);
                            y2 = GET_IMAG_FLT(arg2);
							return(make_comp((x1*x2+y1*y2)/(x2*x2+y2*y2),(y1*x2-x1*y2)/(x2*x2+y2*y2)));}
                
                case INF:	if((positivep(arg1) && arg2 == PINF) || (negativep(arg1) && arg2 == MINF))
                				return(make_flt(0.0));
                			else
                            	return(make_flt(-0.0));
                
                case NANN:	return(PNAN);
   
            }
		case BIG:
        	switch(tag2){
            	case INTN:	exception("/", ILLEGAL_ARGUMENT, arg1);
                case BIG:	exception("/", ILLEGAL_ARGUMENT, arg1);
        
        	}
        case FLTN:
        	switch(tag2){
            	case INTN: {x1 = GET_FLT(arg1);
                			s = GET_INT(arg2);
                            x2 = (double)s;
                            return(make_flt(x1/x2));}
                case FLTN: {if(positivep(arg1) && positive_zerop(arg2))
                                return(PINF); //+inf.0
                			if(negativep(arg1) && positive_zerop(arg2))
                            	return(MINF); //-inf.0
                            if(positivep(arg1) && negative_zerop(arg2))
                            	return(MINF); //-inf.0
                            if(negativep(arg1) && negative_zerop(arg2))
                            	return(PINF); //+inf.0
                            if(zerop(arg1) && zerop(arg2))
                            	return(PNAN); //+nan.0
                			x1 = GET_FLT(arg1);
                			x2 = GET_FLT(arg2);
                            return(make_flt(x1/x2));}
                case RAT:  {x1 = GET_FLT(arg1);
                			s = GET_CAR(arg2);
                            t = GET_CDR(arg2);
                            x2 = (double)s/t;
                            return(make_flt(x1/x2));}
                case BIG:	return(BOOLF);
                case COMP: {x1 = GET_FLT(arg1);
                			y1 = 0;
                			x2 = GET_REAL_FLT(arg2);
                            y2 = GET_IMAG_FLT(arg2);
                            return(make_comp((x1*x2+y1*y2)/(x2*x2+y2*y2),(y1*x2-x1*y2)/(x2*x2+y2*y2)));}
                            
        	}
        case RAT:
        	switch(tag2){
            	case INTN: {n = GET_CAR(arg1);
                			m = GET_CDR(arg1);
                            s = GET_INT(arg2);
                            return(make_rat(n,m*s));}
                case FLTN: {n = GET_CAR(arg1);
                			m = GET_CDR(arg1);
                            x1 = (double)n/m;
                            x2 = GET_FLT(arg2);
                            return(make_flt(x1/x2));}
                case RAT:  {n = GET_CAR(arg1);
                			m = GET_CDR(arg1);
                            s = GET_CAR(arg2);
                            t = GET_CDR(arg2);
                            return(make_rat(n*t,m*s));}
                case BIG:	return(BOOLF);
                case COMP: {n = GET_CAR(arg1);
                			m = GET_CDR(arg1);
                            x1 = (double)n/m;
                            y1 = 0;
                            x2 = GET_REAL_FLT(arg2);
                            y2 = GET_IMAG_FLT(arg2);
                            return(make_comp((x1*x2+y1*y2)/(x2*x2+y2*y2),(y1*x2-x1*y2)/(x2*x2+y2*y2)));}
                            
        }
        
        case COMP:
        	switch(tag2){
            	case INTN: {x1 = GET_REAL_FLT(arg1);
                			y1 = GET_IMAG_FLT(arg1);
                            n = GET_INT(arg2);
                            x2 = (double)n;
                            return(make_comp(x1/x2,y1/x2));}
                case FLTN: {if(zerop(arg1) && zerop(arg2))
                				return(PNAN);
                			if(zerop(arg2))
                            	return(make_comp1(PINF,PINF));
                			x1 = GET_REAL_FLT(arg1);
                			y1 = GET_IMAG_FLT(arg1);
                            x2 = GET_FLT(arg2);
                            return(make_comp(x1/x2,y1/x2));}
                case RAT:  {x1 = GET_FLT(arg1);
                			y1 = GET_IMAG_FLT(arg1);
                            s = GET_CAR(arg2);
                            t = GET_CDR(arg2);
                            x2 = (double)s/t;
                            return(make_comp(x1/x2,y1/x2));}
                case BIG:	return(BOOLF);
                case COMP: {x1 = GET_REAL_FLT(arg1);
                			y1 = GET_IMAG_FLT(arg1);
                            x2 = GET_REAL_FLT(arg2);
                            y2 = GET_IMAG_FLT(arg2);
                            return(make_comp((x1*x2+y1*y2)/(x2*x2+y2*y2),(y1*x2-x1*y2)/(x2*x2+y2*y2)));}
            }
        case INF:
        	switch(tag2){
            	case INTN:
                case RAT:
                case BIG:	if(positivep(arg2))
                            	return(arg1);
                            else
                            	return(inverse(arg1));
                case FLTN:	if(positive_zerop(arg2))
                				return(PINF);
                			else if(negative_zerop(arg2))
                            	return(MINF);
                            else if(positivep(arg2))
                            	return(arg1);
                            else
                            	return(inverse(arg1));
                				
                case INF:	if(arg1 == arg2)
                				return(PINF);
                			else
                            	return(MINF);
                case NANN:	return(PNAN);
                case COMP: 	return(make_comp1(arg1,arg1));
            }
        case NANN:
        	switch(tag2){
            	case INTN:
                case FLTN:
                case RAT:
                case BIG:
                case INF:	return(arg1);
                case NANN:	return(PNAN);
                case COMP: 	return(make_comp1(PNAN,PNAN));               
    		}                 
    }
    return(undef);  
}



int s_remainder(int x, int y){
	if(integerp(x) && integerp(y))
    	return(make_int(get_int(x) % get_int(y)));
    if(bignump(x) && integerp(y))
    	return(big_int_remainder(x,y));
    if(integerp(x) && bignump(y))
    	return(x);
	if(bignump(x) && bignump(y))
    	return(minus(x,mult(quotient(x,y),y)));
    
    exception("remainder", ILLEGAL_ARGUMENT, NIL);
    return(undef);
}


int quotient(int x, int y){
	if(integerp(x) && integerp(y))
    	return(make_int(get_int(x) / get_int(y)));
    
    if(bignump(x) && integerp(y)){
    	if(big_positivep(x))
    		return(big_int_quotient(x,y));
        else
        	return(big_rev(big_int_quotient(big_rev(x),y)));
    }
    
    if(bignump(x) && bignump(y))
    	return(big_quotient(x,y));
    
    if(integerp(x) && bignump(y))
    	return(make_int(0));
    
    exception("quotient", ILLEGAL_ARGUMENT, NIL);
    return(undef);
}

//整数の最大公約数
int int_gcd(int x, int y){
	int r;
	
    if(y == 0)
    	return(x);
        
	while(y != 0){
		r = x % y;
        x = y;
        y = r;
	}
	return(x);
}

//bignumを含む整数の最大公約数
int gcd(int x, int y){
	int r;
    
	if(integerp(x) && integerp(y))
    	return(make_int(abs(int_gcd(get_int(x),get_int(y)))));
    
    if(floatp(x) && integerp(y))
    	return(make_flt((double)abs(int_gcd((int)GET_FLT(x),get_int(y)))));
    
    if(integerp(x) && floatp(y))
    	return(make_flt((double)abs(int_gcd(get_int(x),(int)GET_FLT(y)))));
    
    if(floatp(x) && floatp(y))
    	return(make_flt((double)abs(int_gcd(GET_FLT(x),(int)GET_FLT(y)))));
    
    while(!zerop(y)){
    	r = s_remainder(x,y);
        x = y;
        y = r;
        
    }
    return(s_abs(x));
}

//整数の場合の最大公約数
int int_lcm(int m, int n){
	if (m == 0 || n == 0)
		return(0);
        
	return ((m / int_gcd(m, n)) * n);
}
//bignumを含む整数の最大公約数
int lcm(int x, int y){
	int g,d,res;
	if(integerp(x) && integerp(y) &&
    	abs(get_int(x)) < 10000 && abs(get_int(y)) < 10000)// なぜならx,y < sqrt(BIGNUM_BASE)
    	return(make_int(abs(int_lcm(get_int(x),get_int(y)))));
    
    if(floatp(x) && integerp(y))
    	return(make_flt((double)abs(int_lcm((int)GET_FLT(x),get_int(y)))));
    
    if(integerp(x) && floatp(y))
    	return(make_flt((double)abs(int_lcm(get_int(x),(int)GET_FLT(y)))));
    
    if(floatp(x) && floatp(y))
    	return(make_flt((double)abs(int_lcm(GET_FLT(x),(int)GET_FLT(y)))));
    
    else{
    	g = gcd(x,y);
        d = quotient(s_abs(x),g);
        res = mult(d,s_abs(y));
    	return(res); 
    }   
}

int s_abs(int x){
    
	if(integerp(x))
    	return(make_int(abs(get_int(x))));
    if(bignump(x)){
    	if(big_positivep(x))
        	return(x);
        if(big_negativep(x))
        	return(big_rev(x));
    }
    if(floatp(x)){
        return(make_flt(fabs(GET_FLT(x))));
    }
    if(rationalp(x))
    	return(make_rat(abs(GET_CAR(x)),GET_CDR(x)));
    exception("abs", ILLEGAL_ARGUMENT, x);
    return(undef);
}


//---------bignum型の演算----------------
int big_plus(int arg1, int arg2){
	int res;
    if(big_positivep(arg1) && big_negativep(arg2))
    	return(big_minus(arg1,big_rev(arg2)));
    if(big_negativep(arg1) && big_positivep(arg2))
    	return(big_minus(arg2,big_rev(arg1)));
    
    res = big_plus1(arg1,arg2);
    if(big_integerizep(res))
    	return(bignumtoint(res));
    else
    	return(res);
}



//条件 arg1>0 arg2>0
int big_plus1(int arg1, int arg2){
	int x,y,z,c,q,res;
    
    res = NIL;
    c = 0;
    while(1){
    	if(nullp(arg1) && nullp(arg2)){
        	if(c != 0)
        		res = reverse(cons(make_int(c),res));
            else
            	res = reverse(res);
            SET_TAG(res,BIG);
            return(res);
        }
       	
        if(nullp(arg1) && !nullp(arg2)){
        	x = 0;
            y = GET_INT(car(arg2));
        }
        else if(!nullp(arg1) && nullp(arg2)){
        	x = GET_INT(car(arg1));
        	y = 0;
        }
        else {
        	x = GET_INT(car(arg1));
        	y = GET_INT(car(arg2));
        }
        
        z = x + y + c;
        c = z / BIGNUM_BASE;
        q = z - (c * BIGNUM_BASE);
        res = cons(make_int(q),res);
        arg1 = cdr(arg1);
        arg2 = cdr(arg2);
    }
}


//符号反転
int big_rev(int arg){
	int res;
    
    res = NIL;
    while(!nullp(arg)){
        res = cons(make_int(get_int(car(arg)) * -1),res);
        arg = cdr(arg);
    }
    res = reverse2(res);
    SET_TAG(res,BIG);
    return(res);
}

int big_minus(int arg1, int arg2){
	int x,y,res;
    
    res = undef;
    
    if(big_positivep(arg1) && big_negativep(arg2))
    	return(big_plus(arg1,big_rev(arg2)));
    if(big_negativep(arg1) && big_positivep(arg2))
    	return(big_rev(big_plus(big_rev(arg1),arg2)));
    if(big_positivep(arg1) && big_positivep(arg2)){
		if(big_greaterp(arg1,arg2))
    		res = big_minus1(arg1,arg2);
    	else
    		res = big_rev(big_minus1(arg2,arg1));
    }
            
    if(big_negativep(arg1) && big_negativep(arg1)){
    	x = big_rev(arg1);
        y = big_rev(arg2);
        if(big_greaterp(x,y))
    		res = big_rev(big_minus1(x,y));
    	else
    		res = (big_minus1(y,x));
    }
    

    if(big_integerizep(res))
        	return(bignumtoint(res));
        else
        	return(res); 
}

// 条件　arg1>0 arg2>0 arg1>arg2
int big_minus1(int arg1, int arg2){
	int x,y,z,c,res;
    
    res = NIL;
    c = 0;
    while(1){
        if(nullp(arg2)){
        	while(!nullp(arg1)){
        		x = get_int(car(arg1));
                if((x + c) < 0){
                	x = x + BIGNUM_BASE + c;
                    c = -1;
                	res = cons(make_int(x),res);
                }
                else{
                    x = x + c;
                    c = 0;
                    res = cons(make_int(x),res);
                }
                arg1 = cdr(arg1);
            }
            res= norm_bignum(reverse(res));
            SET_TAG(res,BIG);
            return(res);
    	}
        x = get_int(car(arg1));
        y = get_int(car(arg2));
        if((x + c - y) < 0){
        	z = (x + BIGNUM_BASE + c) - y;
            c = -1;
        }
        else{
        	z = (x + c) - y;
        	c = 0;
        }
        res = cons(make_int(z),res);
        arg1 = cdr(arg1);
        arg2 = cdr(arg2);
    }
}

//bignumとbignumとの乗算
int big_mult(int arg1, int arg2){
	//被乗数の方が大きい方がメモリ効率がいいため。
	if(big_greaterp(arg1,arg2))
    	return(big_mult1(arg1,arg2));
    else
    	return(big_mult1(arg2,arg1));
}

int	big_mult1(int arg1, int arg2){
	int org_arg1,n,acc,res;
    long long int l,l1,l2,c,r;
    
    org_arg1 = arg1;
    n = 0;
    res = list1(make_int(0));
    acc = NIL;
    while(!nullp(arg2)){
    	l2 = (long long int)get_int(car(arg2));
        c = 0;
        while(!nullp(arg1)){
        	l1 = (long long int)get_int(car(arg1));
            l = l1 * l2 + c;
            c = l / BIGNUM_BASE;
            r = l % BIGNUM_BASE;
            acc = cons(make_int((int)r),acc);
            arg1 = cdr(arg1);
    	}
        if(c != 0)
        	acc = cons(make_int((int)c),acc);
        
        acc = big_sift(reverse2(acc),n);
        res = big_plus1(res,acc);
        n++;
        acc = NIL;
        arg1 = org_arg1;
        arg2 = cdr(arg2);
    }
    SET_TAG(res,BIG);
    return(res);
}

//bignum(arg1)とint(arg2)との乗算
int big_int_mult(int arg1, int arg2){
	int res;
    long long int l,l1,l2,c,r;
    
    res = NIL;
    l2 = (long long int)get_int(arg2);
    c = 0;
    while(!nullp(arg1)){
    	l1 = (long long int)get_int(car(arg1));
        l = l1 * l2 + c;
        c = l / BIGNUM_BASE;
        r = l % BIGNUM_BASE;
        res = cons(make_int((int)r),res);
        arg1 = cdr(arg1);
    }
    if(c != 0)
    	res = cons(make_int((int)c),res);
    
    res = reverse2(res);
    SET_TAG(res,BIG);
    return(res);
}


//クヌース　第4巻　88ページ参照
int	big_quotient(int arg1, int arg2){
	if(big_positivep(arg1) && big_positivep(arg2))
    	return(big_quotient1(arg1,arg2));    
    if(big_positivep(arg1) && big_negativep(arg2))
    	return(big_rev(big_quotient(arg1,big_rev(arg2))));
    if(big_negativep(arg1) && big_positivep(arg2))
    	return(big_rev(big_quotient(big_rev(arg1),arg2)));
    if(big_negativep(arg1) && big_negativep(arg2))
    	return(big_quotient(big_rev(arg1),big_rev(arg2)));
    return(undef);
}

//被除数bignumと除数bignumの場合の商
int	big_quotient1(int arg1, int arg2){
	int s,ds,p,res; //s=sift桁合わせ, ds除数,p=plus足し戻し
    long long int d,u,v,q;
    
    //被除数が除数より小さければ０とする。
    if(smallerp(arg1,arg2))
    	return(make_int(0));
    
    //被除数が除数の2倍より小さければ１とする。
    //商が1になる場合には定理は成り立たないはず。
    s = mult(arg2,make_int(2));
    if(smallerp(arg1,s))
    	return(make_int(1));
    
    res = NIL;
    //除数が小さければ定理が成り立つように大きくする。
    v = (long long int)get_int(last(arg2));
    if(v < (BIGNUM_BASE / 2)){
        	d = BIGNUM_BASE / (v + 1);
        	arg1 = mult(arg1,make_int((int)d));
            arg2 = mult(arg2,make_int((int)d));
        }
    	
    
    while(1){
        u = (long long int)get_int(last(arg1)) * BIGNUM_BASE +
            	(long long int)get_int(last(butlast(arg1)));
        v = (long long int)get_int(last(arg2));
        //q = min(u/v,BIGNUM_BASE-1)
        q = u / v;
        if(q > BIGNUM_BASE-1)
        	q = BIGNUM_BASE-1;
        
        ds = mult(arg2,make_int((int)q));
        s = length(arg1) - length(ds);
        ds = big_sift(ds,s);
        p = big_sift(arg2,s); 
        arg1 = minus(arg1,ds);
        
        
        if(big_negativep(arg1)){
        	arg1 = plus(arg1,p);
            q--;
        }
        if(big_negativep(arg1)){
        	arg1 = plus(arg1,p);
            q--;
        }
        
        res = cons(make_int((int)q),res);
        
        if(smallerp(arg1,arg2))
        	break;
              
    }
    SET_TAG(res,BIG);
    if(big_integerizep(res))
    	return(bignumtoint(res));
    else
    	return(res);
}

//被除数bignumと除数intの場合の商
int big_int_quotient(int arg1, int arg2){
	int res;
    long long int u,v,q,r;
    
    res = NIL;
	
    v = (long long int)get_int(arg2);
    if(v == 0)
    	exception("quotient", ILLEGAL_ARGUMENT, arg2);
	
    u = (long long int)get_int(last(arg1));
    if(u < v){
    	r = u;
        arg1 = butlast(arg1);
    }
    else
    	r = 0;
    
    while(!nullp(arg1)){
    	u = (long long int)get_int(last(arg1));
        q = (r * BIGNUM_BASE + u) / v;
        r = (r * BIGNUM_BASE + u) % v;
        res = cons(make_int((int)q),res);
        arg1 = butlast(arg1);
    }

    SET_TAG(res,BIG);
    if(big_integerizep(res)){
    	return(bignumtoint(res));
	}
    return(res);	
}

//上位桁の０をカットする。
int norm_bignum(int arg){
	int car_addr,lis,res;
    
	res = NIL;
    lis = reverse2(arg);
	while(!nullp(lis)){
    	car_addr = car(lis);
        if(GET_INT(car_addr) != 0){
        	res = reverse2(lis);
            SET_TAG(res,BIG);	
            return(res);
        }
        else
        	lis = cdr(lis);	
    }
    return(make_int(0));
}

int inttobignum(int x){
	int res;
    
    res = list1(make_int(get_int(x)));
    SET_TAG(res,BIG);
    return(res);    
}

int bignumtoint(int x){
	
    return(make_int(get_int(car(x))));	
}

//比較
int big_eqp(int arg1, int arg2){
	int l1,l2;
    
    l1 = length(arg1);
    l2 = length(arg2);
    

	if(l1 != l2)
    	return(0);
    else{
    	while(!nullp(arg1)){
        	if(get_int(car(arg1)) != get_int(car(arg2)))
    			return(0);
            
            arg1 = cdr(arg1);
            arg2 = cdr(arg2);
        }
    }	
    return(1);	
}

int big_greaterp(int arg1, int arg2){
	int l1,l2,a1,a2;
    
    l1 = length(arg1);
    l2 = length(arg2);
    

	if(l1 > l2)
    	return(1);
    else
    if(l1 == l2){
    	a1 = reverse(arg1);
    	a2 = reverse(arg2);
    	while(!nullp(a1)){
        	if(get_int(car(a1)) > get_int(car(a2)))
    			return(1);
            if(get_int(car(a1)) < get_int(car(a2)))
    			return(0);
            a1 = cdr(a1);
            a2 = cdr(a2);
        }
        return(0);
    }	
    else
    	return(0);	
}

int big_smallerp(int arg1, int arg2){
	int l1,l2,a1,a2;
    
    l1 = length(arg1);
    l2 = length(arg2);
    

	if(l1 < l2)
    	return(1);
    else
    if(l1 == l2){
    	a1 = reverse(arg1);
    	a2 = reverse(arg2);
    	while(!nullp(a1)){
        	if(get_int(car(a1)) < get_int(car(a2)))
    			return(1);
            
            if(get_int(car(a1)) > get_int(car(a2)))
            	return(0);
                
            a1 = cdr(a1);
            a2 = cdr(a2);
        }
        return(0);
    }	
    else
    	return(0);	
}



//int型に変換可能か
int big_integerizep(int x){
    int y;
    
    y = cdr(x);
    while(!nullp(y)){
    	if(get_int(car(y)) != 0)
        	return(0);
        else
        	y = cdr(y);
    }
    return(1);	
}

//プラスか
int big_positivep(int x){

    while(!nullp(cdr(x)))
    	x = cdr(x);
    
	if(get_int(car(x)) > 0)
    	return(1);
    else
    	return(0);
}
//マイナスか
int big_negativep(int x){
	
    while(!nullp(cdr(x)))
    	x = cdr(x);
    
	if(get_int(car(x)) < 0)
    	return(1);
    else
    	return(0);
}

int	big_abs(int x){
	if(big_positivep(x))
    	return(x);
    else
    	return(big_rev(x));
}



//ｎ個分０のセルを付加。ビットシフトのような感じ。
int	big_sift(int x, int n){
	int res;
    
    if(n == 0)
    	return(x);
        
    res = x;
    while(n > 0){
    	res = cons(make_int(0),res);
        n--;
    }
    SET_TAG(res,BIG);
    return(res);
}

//bignumとintとの剰余を計算する。
int	big_int_remainder(int x, int y){
	int big,sign1,sign2;
    long long int i,j,r;
    
    if(big_negativep(x)){
    	big = reverse(big_rev(x));
    	sign1 = -1;
    }
    else{
    	big = reverse(x);
    	sign1 = 1;
    }
    
    j = get_int(y);
    if(j < 0){
    	j = abs(j);
        sign2 = -1;
    }
    else{
    	sign2 = 1;
    }
    	
    r = 0;
    
    while(!nullp(big)){
    	i = get_int(car(big));
        i = i + r * BIGNUM_BASE;
        if(i >= j)
        	r = i % j;
        else
        	r = i + r;
        big = cdr(big);
    }
    return(make_int((int)r*sign1*sign2));
}



