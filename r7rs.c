#include "norm.h"

extern int current_module;


void init_r7rs(void){
	
    current_module = 1; //(normal system)
	defsubr("pair-length",(int)f_pair_length);
    defsubr("last",(int)f_last);
    defsubr("butlast",(int)f_butlast);
    defsubr("sys-code",(int)f_vmcode);
    defsubr("sys-env",(int)f_env);
    defsubr("sys-timer-set",(int)f_timer_set);
    defsubr("sys-timer-get",(int)f_timer_get);
    defsubr("sys-timer-gbc",(int)f_timer_gbc);
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
    defsubr("room",(int)f_room);
    defsubr("macro?",(int)f_macrop);
    defsubr("macro-name?",(int)f_macro_namep);
    defsubr("hygienic?",(int)f_hygienicp);
    defsubr("hygienic-name?",(int)f_hygienic_namep);
    defsubr("gensym",(int)f_gensym);
    defsubr("flush",(int)f_flush);
    defsubr("sys-set-trace",(int)f_set_trace);
    defsubr("sys-set-untrace",(int)f_set_untrace);
    defsubr("transfer",(int)f_transfer);
    defsubr("debug",(int)f_debug);
    defsubr("profiler",(int)f_prof);
    defsubr("current-module",(int)f_current_module);
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
    defsubr("global-bound?",(int)f_global_boundp);
    defsubr("inspect",(int)f_inspect);
    defsubr("lambda/asm",(int)f_lambda_asm);
    defsubr("system",(int)f_system);
    defsubr("get-car",(int)f_get_car);
    

	current_module = 3; //(scheme base)
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
    defsubr("newline",(int)f_newline);
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
    defsubr("exact->inexact",(int)f_exact_inexact);
    defsubr("inexact",(int)f_exact_inexact);
	defsubr("inexact->exact",(int)f_inexact_exact);
    defsubr("exact",(int)f_inexact_exact);
    defsubr("remainder",(int)f_remainder);
	defsubr("modulo",(int)f_modulo);
    defsubr("quotient",(int)f_quotient);
    defsubr("gcd",(int)f_gcd);
    defsubr("lcm",(int)f_lcm);
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
	defsubr("apply",(int)f_apply);
    defsubr("gbc",(int)f_gbc);
    defsubr("values",(int)f_values);
    defsubr("exact-integer?",(int)f_exact_integerp);
	defsubr("error",(int)f_error);
    defsubr("flush-output-port",(int)f_flush_output_port);
    defsubr("square",(int)f_square);
    defsubr("bytevector?",(int)f_bytevectorp);
    defsubr("make-bytevector",(int)f_make_bytevector);
    defsubr("bytevector",(int)f_bytevector);
    defsubr("bytevector-length",(int)f_bytevector_length);
    defsubr("bytevector-u8-set!",(int)f_bytevector_u8_set);
    defsubr("bytevector-u8-ref",(int)f_bytevector_u8_ref);
    defsubr("bytevector-copy",(int)f_bytevector_copy);
    defsubr("bytevector-copy!",(int)f_bytevector_copy2);
    defsubr("bytevector-append",(int)f_bytevector_append);
	
    current_module = 4; //(scheme inexact)
    defsubr("sin",(int)f_sin);
    defsubr("cos",(int)f_cos);
    defsubr("tan",(int)f_tan);
    defsubr("asin",(int)f_asin);
    defsubr("acos",(int)f_acos);
    defsubr("atan",(int)f_atan);
    defsubr("log",(int)f_log);
    defsubr("exp",(int)f_exp);
    defsubr("sqrt",(int)f_sqrt);
    defsubr("infinity?",(int)f_infinityp);
    defsubr("finity?",(int)f_finityp);
    defsubr("nan?",(int)f_nanp);
    
    current_module = 5; //(scheme complex)
    defsubr("real-part",(int)f_realpart);
    defsubr("imag-part",(int)f_imagpart);
    defsubr("magnitude",(int)f_magnitude);
    defsubr("angle",(int)f_angle);
    defsubr("make-rectangular",(int)f_make_rectangular);
    defsubr("make-polar",(int)f_make_polar);
    
    current_module = 9;
    defsubr("eval",(int)f_eval);
    
    current_module = 11; //(scheme process-context)
    defsubr("command-line",(int)f_command_line);
    defsubr("get-environment-variable",(int)f_get_environment_variable);
    defsubr("get-environment-variables",(int)f_get_environment_variables);
	defsubr("exit",(int)f_exit);
    
    current_module = 12; //(scheme load)
    defsubr("load",(int)f_load);
    
    current_module = 13; //(scheme file)
    defsubr("file-exists?",(int)f_file_existsp);
    
    current_module = 14; //(scheme read)
    defsubr("read",(int)f_read);
    
    current_module = 15; //(scheme write)
    defsubr("display",(int)f_display);
    defsubr("write",(int)f_write);
    
    current_module = 16; //(scheme char)
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
    
    current_module = 18; //(scheme time)
    defsubr("current-second",(int)f_current_second);
    defsubr("current-jiffy",(int)f_current_jiffy);
    defsubr("jiffies-per-second",(int)f_jiffies_per_second);
	
    current_module = 0; //user
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
    
    current_module = 2;
}