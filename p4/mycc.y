/*
Price, Chris, Gorana, Lian
GCSC 554
File: mycc.y
*/

%{

#include "lex.yy.h"
#include "global.h"

#define MAXFUN 100
#define MAXFLD 100

static struct ClassFile cf;

/* stacks of symbol tables and offsets, depth is just 2 in C (global/local) */
static Table *tblptr[2];
static int offset[2];

/* stack pointers (index into tblptr[] and offset[]) */
static int tblsp = -1;
static int offsp = -1;

/* stack operations */
#define top_tblptr	(tblptr[tblsp])
#define top_offset	(offset[offsp])
#define push_tblptr(t)	(tblptr[++tblsp] = t)
#define push_offset(n)	(offset[++offsp] = n)
#define pop_tblptr	(tblsp--)
#define pop_offset	(offsp--)

/* flag to indicate we are compiling main's body (to differentiate 'return') */
static int is_in_main = 0;

%}

/* declare YYSTYPE attribute types of tokens and nonterminals */
%union
{ Symbol *sym;  /* token value yylval.sym is the symbol table entry of an ID */
  unsigned num; /* token value yylval.num is the value of an int constant */
  float flt;    /* token value yylval.flt is the value of a float constant */
  char *str;    /* token value yylval.str is the value of a string constant */
  unsigned loc; /* location of instruction to backpatch */
  Type typ;	/* type descriptor */
  Expr exp;
  Entry *ent;
}

/* declare ID token and its attribute type */
%token <sym> ID

/* Declare INT tokens (8 bit, 16 bit, 32 bit) and their attribute type 'num' */
%token <num> INT8 INT16 INT32

/* Declare FLT token for literal floats */
%token <flt> FLT

/* Declare STR token for literal strings */
%token <str> STR

/* declare tokens for keywords */
/* Note: install_id() returns Symbol* for keywords and identifiers */
%token <sym> BREAK CHAR DO ELSE FLOAT FOR IF INT MAIN RETURN VOID WHILE

/* declare operator tokens */
%right '=' PA NA TA DA MA AA XA OA LA RA
%left OR
%left AN
%left '|'
%left '^'
%left '&'
%left EQ NE LE '<' GE '>'
%left LS RS
%left '+' '-'
%left '*' '/' '%'
%right '!' '~'
%left PP NN 
%left '.' AR

/* Declare attribute types for marker nonterminals, such as K L M and N */
/* TODO: TO BE COMPLETED WITH ADDITIONAL NONMARKERS AS NECESSARY */
%type <loc> K L M N P B

%type <typ> type list args

%type <num> ptr

%%

prog	: Mprog exts	{ addwidth(top_tblptr, top_offset);
			  pop_tblptr;
			  pop_offset;
			}
	;

Mprog	: /* empty */	{ push_tblptr(mktable(NULL));
			  push_offset(0);
			}
	;

exts	: exts func
	| exts decl
	| /* empty */
	;

func	: MAIN '(' ')' Mmain block
			{ // need a temporary table pointer
			  Table *table;
			  // the type of main is a JVM type descriptor
			  Type type = mkfun("[Ljava/lang/String;", "V");
			  // emit the epilogue part of main()
			  emit3(getstatic, constant_pool_add_Fieldref(&cf, "java/lang/System", "out", "Ljava/io/PrintStream;"));
			  emit(iload_2);
			  emit3(invokevirtual, constant_pool_add_Methodref(&cf, "java/io/PrintStream", "println", "(I)V"));
			  emit(return_);
			  // method has public access and is static
			  cf.methods[cf.method_count].access = (enum access_flags)(ACC_PUBLIC | ACC_STATIC);
			  // method name is "main"
			  cf.methods[cf.method_count].name = "main";
			  // method descriptor of "void main(String[] arg)"
			  cf.methods[cf.method_count].descriptor = type;
			  // local variables
			  cf.methods[cf.method_count].max_locals = top_offset;
			  // max operand stack size of this method
			  cf.methods[cf.method_count].max_stack = 100;
			  // length of bytecode is in the emitter's pc variable
			  cf.methods[cf.method_count].code_length = pc;
			  // must copy code to make it persistent
			  cf.methods[cf.method_count].code = copy_code();
			  if (!cf.methods[cf.method_count].code)
				error("Out of memory");
			  // advance to next method to store in method array
			  cf.method_count++;
			  if (cf.method_count > MAXFUN)
			  	error("Max number of functions exceeded");
			  // add width information to table
			  addwidth(top_tblptr, top_offset);
			  // need this table of locals for enterproc
			  table = top_tblptr;
			  // exit the local scope by popping
			  pop_tblptr;
			  pop_offset;
			  // enter the function in the global table
			  enterproc(top_tblptr, $1, type, table);
			}
	| type ID '(' Margs args ')' block
			{ /* TASK 3: TO BE COMPLETED */
			}
	;

Mmain	:		{ int label1, label2;
			  Table *table;
			  // create new table for local scope of main()
			  table = mktable(top_tblptr);
			  // push it to create new scope
			  push_tblptr(table);
			  // for main(), we must start with offset 3 in the local variables of the frame
			  push_offset(3);
			  // init code block to store stmts
			  init_code();
			  // emit the prologue part of main()
			  emit(aload_0);
			  emit(arraylength);
			  emit2(newarray, T_INT);
			  emit(astore_1);
			  emit(iconst_0);
			  emit(istore_2);
			  label1 = pc;
			  emit(iload_2);
			  emit(aload_0);
			  emit(arraylength);
			  label2 = pc;
			  emit3(if_icmpge, PAD);
			  emit(aload_1);
			  emit(iload_2);
			  emit(aload_0);
			  emit(iload_2);
			  emit(aaload);
			  emit3(invokestatic, constant_pool_add_Methodref(&cf, "java/lang/Integer", "parseInt", "(Ljava/lang/String;)I"));
			  emit(iastore);
			  emit32(iinc, 2, 1);
			  emit3(goto_, label1 - pc);
			  backpatch(label2, pc - label2);
			  // global flag to indicate we're in main()
			  is_in_main = 1;
			}
	;

Margs	:		{  Table *table;
			  table = mktable(top_tblptr);
			  push_tblptr(table);
			  push_offset(0);
			  init_code();
			  is_in_main = 0;
			}
	;

block	: '{' decls stmts '}'
	;

decls	: decls decl
	| /* empty */
	;

decl	: list ';'
	;

type	: VOID		{ $$ = mkvoid(); }
	| INT		{ $$ = mkint(); }
	| FLOAT		{ $$ = mkfloat(); }
	| CHAR		{ $$ = mkchar(); }
	;

args	: args ',' type ID
			{ if ($4 && ischar($3))
				enter(top_tblptr, $5, mkstr(), top_offset++);
			  else
				enter(top_tblptr, $5, $3, top_offset++);
			  $$ = mkpair($1, $3);
			}
	| type ID	{ if ($2 && ischar($1))
				enter(top_tblptr, $3, mkstr(), top_offset++);
			  else
				enter(top_tblptr, $3, $1, top_offset++);
			  $$ = $1;
			}
	;

list	: list ',' ID
			{ /* TASK 1 and 4: TO BE COMPLETED */
			  /* $1 is the type */
			if (top_tblptr -> level == 0) {
				cf.fields[cf.field_count].access = ACC_STATIC;
            			cf.fields[cf.field_count].name = $3->lexptr;
            			cf.fields[cf.field_count].descriptor = $1;
            			cf.field_count++;
           			enter(top_tblptr, $3, $1, constant_pool_add_Fieldref(&cf, cf.name, $3->lexptr, $1));
           		
			} else if(top_tblptr -> level > 0){
         			enter(top_tblptr, $3, $1, top_offset++);
         		}
        		
			$$ = $1;
		}
	| type ID	
		{
			if (top_tblptr -> level == 0){
				cf.fields[cf.field_count].access = ACC_STATIC;
            			cf.fields[cf.field_count].name = $2->lexptr;
            			cf.fields[cf.field_count].descriptor = $1;
            			cf.field_count++;
            			enter(top_tblptr, $2, $1, constant_pool_add_Fieldref(&cf, cf.name, $2->lexptr, $1));

			  } else if(top_tblptr -> level > 0) {
			  	enter(top_tblptr, $2, $1, top_offset++);
			  	
			  }
			  $$ = $1;
	;

ptr	: /* empty */	{ $$ = 0; }
	| '*'		{ $$ = 1; }
	;

stmts   : stmts stmt
        | /* empty */
        ;

/* TASK 1: TO BE COMPLETED: */
stmt    : ';'
        | expr ';'      { emit(pop); }
        | IF '(' expr ')' M L stmt
                        { 
			if (!$3.type) {
			  	backpatchlist($3.truelist, $6);
				backpatchlist($3.falselist, pc);
			  } else if (!isint($3.type)) {
			  	error("Type error");
			  }
			  backpatch($5, pc - $5);
			}
        | IF '(' expr ')' M L stmt ELSE N L stmt
                {
			if (!$3.type) {
				backpatchlist($3.truelist, $6);
				backpatchlist($3.falselist, $10);
			} else if (!isint($3.type)) {
				error("Type error IFELSE");
			}
			backpatch($5, $10 - $5);
			backpatch($9, pc - $5);
		}

	| WHILE '(' L expr ')' M L stmt N
                {
			if (!$4.type) {
				backpatchlist($4.truelist, $7);
				backpatchlist($4.falselist, pc);
			} else if (!isint($4.type)) {
				error("Type error WHILE");
			}
			backpatch($6, pc - $6);
			backpatch($9, $3 - $9);
		}

        | DO L stmt WHILE '(' expr ')' K ';'
		{
			if (!$6.type) {
				backpatchlist($6.truelist, $2);
				backpatchlist($6.falselist, pc);
			} else if (!isint($6.type)) {
				error("Type error DOWHILE");
			}
			backpatch($8,$2-$8);
		}
        | FOR '(' Pexpr ';' L expr M N ';' L Pexpr N ')' B L stmt N
                        { if (!$6.type)
			  {	backpatchlist($6.truelist, $15);
				backpatchlist($6.falselist, pc);
			  }
			  else if (!isint($6.type))
			  	error("Type error");
                          backpatch($7, pc - $7);
                          backpatch($8, $15 - $8);
			  backpatch($12, $5 - $12);
			  backpatch($17, $10 - $17);
			  // backpatch goto of break statement
			  backpatchlist(breaks[level--], pc);
			}
        | RETURN expr ';'
                        { coerce1(&$2, ret_type);
			  if (is_in_main) {
				emit(istore_2);
			  } else {
			  	if (isint(ret_type)) {
					emit(ireturn);
			  	} else if (isfloat(ret_type)) {
					emit(freturn);
			  	} else {
					error("Type error");
			        }
			  }
			}
	| BREAK ';'	{ /* BREAK is optional to implement (see Pr3) */
			  error("break not implemented");
			}
        | '{' stmts '}'
        | error ';'     { yyerrok; }
        ;

exprs	: exprs ',' expr
			{ decircuit(&$3); }
	| expr		{ decircuit(&$1); }
	;

/* TASK 1: TO BE COMPLETED (use pr3 code, then work on assign operators): */
expr    : ID   '=' expr { $$ = emitas($1, &$3, nop, nop); }
        | ID   PA  expr { $$ = emitas($1, &$3, iadd, fadd); }
        | ID   NA  expr { $$ = emitas($1, &$3, isub, fsub); }
        | ID   TA  expr { $$ = emitas($1, &$3, imul, fmul); }
        | ID   DA  expr { $$ = emitas($1, &$3, idiv, fdiv); }
        | ID   MA  expr { $$ = emitas($1, &$3, irem, nop); }
        | ID   AA  expr { $$ = emitas($1, &$3, iand, nop); }
        | ID   XA  expr { $$ = emitas($1, &$3, ixor, nop); }
        | ID   OA  expr { $$ = emitas($1, &$3, ior, nop); }
        | ID   LA  expr { $$ = emitas($1, &$3, ishl, nop); }
        | ID   RA  expr { $$ = emitas($1, &$3, ishr, nop); }
        | expr OR L expr
			{ $$.type = NULL;
			  if ($1.type && $4.type)
			  {	// both operands are non-short-circuit
			  	if (isint($1.type) && isint($4.type)) {
			  		emit3(ifeq, 5);
			  		emit(pop);
					emit(iconst_1);
					$$ = circuit(&$4);
			  	} else {
					error("Type error");
				}
			  } else if ($4.type) {
			  	Expr e = circuit(&$4);
			  	$$.truelist = mergelist($1.truelist, e.truelist);
				backpatchlist($1.falselist, $3);
				$$.falselist = e.falselist;
			  } else if ($1.type) {
			  	Expr e = circuit(&$1);
			  	$$.truelist = mergelist(e.truelist, $4.truelist);
				backpatchlist(e.falselist, $3);
				$$.falselist = $4.falselist;
			  } else {
			  	$$.truelist = mergelist($1.truelist, $4.truelist);
				backpatchlist($1.falselist, $3);
				$$.falselist = $4.falselist;
			  }
			}
        | expr AN  expr { error("&& operator not implemented"); }
        | expr '|' expr { error("| operator not implemented"); }
        | expr '^' expr { error("^ operator not implemented"); }
        | expr '&' expr { error("& operator not implemented"); }
        | expr EQ  expr { error("== operator not implemented"); }
        | expr NE  expr { error("!= operator not implemented"); }
        | expr '<' expr { error("< operator not implemented"); }
        | expr '>' expr { error("> operator not implemented"); }
        | expr LE  expr { $$.type = widen(&$1, &$3);
			  if (isint($$.type)) {
			  	$$.truelist = makelist(pc);
			  	emit3(if_icmple, 0);
			  } else if (isfloat($$.type)) {
			  	emit(fcmpg);
			  	$$.truelist = makelist(pc);
			  	emit3(ifeq, 0);
			  }
			  $$.falselist = makelist(pc);
			  emit3(goto_, 0);
			  $$.type = NULL;
			}
        | expr GE  expr { error(">= operator not implemented"); }
        | expr LS  expr { error("<< operator not implemented"); }
        | expr RS  expr { error(">> operator not implemented"); }
        | expr '+' expr { error("+ operator not implemented"); }
        | expr '-' expr { error("- operator not implemented"); }
        | expr '*' expr { error("* operator not implemented"); }
        | expr '/' expr { error("/ operator not implemented"); }
        | expr '%' expr { error("% operator not implemented"); }
        | '!' expr      { error("! operator not implemented"); }
        | '~' expr      { error("~ operator not implemented"); }
        | '+' expr %prec '!'
                        { $$ = $2; }
        | '-' expr %prec '!'
                        { $$.truelist = $$.falselist = NULL;
			  $$.type = decircuit(&$2);
			  if (isint($$.type))
				emit(ineg);
			  else if (isfloat($$.type))
				emit(fneg);
			  else
			  	error("Type error");
			}
        | '(' expr ')'
        | '$' INT8      { // check that we are in main()
			  if (is_in_main)
			  {	emit(aload_1);
			  	emit2(bipush, $2);
			  	emit(iaload);
			  }
			  else
			  	error("invalid use of $# in function");
			}
        | PP ID         { error("pre ++ operator not implemented"); }
        | NN ID         { error("pre -- operator not implemented"); }
        | ID PP         { error("post ++ operator not implemented"); }
        | ID NN         { error("post -- operator not implemented"); }
        | ID            { error("variable use not implemented"); }
        | INT8          { emit2(bipush, $1); }
        | INT16         { emit3(sipush, $1); }
        | INT32         { emit2(ldc, constant_pool_add_Integer(&cf, $1)); }
	| FLT		{ emit2(ldc, constant_pool_add_Float(&cf, $1)); }
	| STR		{ emit2(ldc, constant_pool_add_String(&cf, constant_pool_add_Utf8(&cf, $1))); }
	| ID '(' exprs ')'
			{ /* TASK 3: TO BE COMPLETED */
			  error("function call not implemented");
			}
        ;

K       : /* empty */   { $$ = pc; emit3(ifne, 0); }
        ;

L       : /* empty */   { $$ = pc; }
        ;

M       : /* empty */   { $$ = pc;	/* location of inst. to backpatch */
			  emit3(ifeq, 0);
			}
        ;

N       : /* empty */   { $$ = pc;	/* location of inst. to backpatch */
			  emit3(goto_, 0);
			}
        ;

P       : /* empty */   { emit(pop); }
        ;

%%

int main(int argc, char **argv)
{
	// init the compiler
	init();

	// set up a new class file structure
	init_ClassFile(&cf);

	// class has public access
	cf.access = ACC_PUBLIC;

	// class name is "Code"
	cf.name = "Code";

	// field counter (incremented for each field we add)
	cf.field_count = 0;

	// method counter (incremented for each method we add)
	cf.method_count = 0;

	// allocate an array of MAXFLD fields
	cf.fields = (struct FieldInfo*)malloc(MAXFLD * sizeof(struct FieldInfo));

	// allocate an array of MAXFUN methods
	cf.methods = (struct MethodInfo*)malloc(MAXFUN * sizeof(struct MethodInfo));

	if (!cf.methods)
		error("Out of memory");

	if (argc > 1)
		if (!(yyin = fopen(argv[1], "r")))
			error("Cannot open file for reading");

	if (yyparse() || errnum > 0)
		error("Compilation errors: class file not saved");

	fprintf(stderr, "Compilation successful: saving %s.class\n", cf.name);

	// save class file
	save_classFile(&cf);

	return 0;
}
/* Coerce value on top of stack */
static Type coerce1(Expr *expr, Type type) {
    Type conv;
    Type rc;

    if (iseq(expr->type, type)) {
	rc = type;
    } else {
	conv = decircuit(expr);
	if (isint(conv) && isfloat(type)) {
	    emit(i2f);
	} else if (isfloat(conv) && isint(type)) {
	    emit(f2i);
	}
	rc = type;
    }

    return rc;
}

/* Coerce value under top of stack */
static Type coerce2(Expr *expr, Type type) {
    Type conv;
    Type rc;

    if (iseq(expr->type, type)) {
	rc = type;
    } else {
	if (expr->type) {
	    emit(swap);
	}
	conv = coerce1(expr, type);
	emit(swap);
	rc = conv;
    }

    return rc;
}

/* Convert integer to short-circuit, no change when already short circuit */
static Expr circuit(Expr *expr) {
    Expr result;

    if (!expr->type) {
	result = *expr;
    } else if (isint(expr->type)) {
	result.falselist = makelist(pc);
	emit3(ifeq, 0);
	result.truelist = makelist(pc);
	emit3(goto_, 0);
    } else {
	error("Type error");
    }

    result.type = expr->type;

    return result;
}

/* Convert short-circuit logic to push of int 0 or 1 by backpatching */
static Type decircuit(Expr *expr) {
    Type rc;
    if (expr->type) {
	rc = expr->type;
    } else {
	backpatchlist(expr->falselist, pc);
	emit(iconst_0);
	emit3(goto_, 4);
	backpatchlist(expr->truelist, pc);
	emit(iconst_1);

	rc = mkint();
    }

    return rc;
}

/* Coerce and return the wider type (int or float) of two types */
static Type widen(Expr *expr1, Expr *expr2) {
    Type type;

    if (isfloat(expr1->type)) {
	type = expr1->type;
    } else if (isfloat(expr2->type)) {
	type = expr2->type;
    } else {
	type = mkint();
    }

    coerce1(expr2, type);
    coerce2(expr1, type);

    return type;
}

/* Emit float/integer operation */
static Expr emitop(Expr *expr1, Expr *expr2, int iop, int fop) {
    Expr result;

    result.truelist = result.falselist = NULL;
    result.type = widen(expr1, expr2);

    if (isint(result.type)) {
	emit(iop);
    } else if (isfloat(result.type) && fop != nop) {
	emit(fop);
    } else {
	error("Type error");
    }

    return result;
}

/* Emit float/integer assignment operation */
static Expr emitas(Symbol *sym, Expr *expr, int iop, int fop) {
    Expr result;
    int place;
    Type type;

    result.truelist = result.falselist = NULL;

    place = getplace(top_tblptr, sym);
    type = gettype(top_tblptr, sym);
    result.type = coerce1(expr, type);
    if (getlevel(top_tblptr, sym) == 0) {
    	if (iop != nop) {
	    emit3(getstatic, place);
	    emit(swap);
	    if (isint(result.type)) {
		emit(iop);
	    } else if (isfloat(result.type) && fop != nop) {
		emit(fop);
	    } else {
		error("Type error");
	    }
	}
	emit(dup);
	emit3(putstatic, place);
    } else if (isint(result.type)) {
    	if (iop != nop) {
	    emit2(iload, place);
	    emit(swap);
	    emit(iop);
	}
	emit(dup);
	emit2(istore, place);
    } else if (isfloat(result.type) && (iop == nop || fop != nop)) {
    	if (iop != nop) {
	    emit2(fload, place);
	    emit(swap);
	    emit(fop);
	}
	emit(dup);
	emit2(fstore, place);
    } else {
	error("Type error");
    }

    return result;
} 
