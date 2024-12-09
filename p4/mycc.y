%{
/* 
Price, Chris, Gorana, Lian
GCSC554
Project: p4
File: mycc.y
*/

#include "lex.yy.h"
#include "global.h"

#define MAXFUN 100
#define MAXFLD 100

#define MAXLEVEL 10  // Adjust based on the maximum nesting level of loops
static Backpatchlist *breaks[MAXLEVEL];
static int level = -1;  // Used to track the current loop level


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

/* Stores the type that a particular method returns */
static Type ret_type;

/* Coerce value on top of stack */
static Type coerce1(Expr *expr, Type type);
/* Coerce value under top of stack */
static Type coerce2(Expr *expr, Type type);

/* Convert integer to short-circuit, no change when already short circuit */
static Expr circuit(Expr *expr);
/* Convert short-circuit logic to push an int 0 or 1 by backpatching */
static Type decircuit(Expr *expr);
/* Coerce and return the wider type (int or float) of two types */
static Type widen(Expr *expr1, Expr *expr2);
/* Emit float/integer operation */
static Expr emitop(Expr *expr1, Expr *expr2, int iop, int fop);
/* Emit float/integer assignment operation */
static Expr emitas(Symbol *sym, Expr *expr, int iop, int fop);
/*emit assignment ops for increment assign(id++ and variants) prepost variable  0 = pre, 1 = post*/
static Expr emitinc(Symbol *sym,int prepost,int iop,int fop);
static Expr emitcmp(Expr *exp1, Expr *exp2, int icmp);

%}

/* declare YYSTYPE attribute types of tokens and nonterminals */
%union
{ 
  Symbol *sym;  /* token value yylval.sym is the symbol table entry of an ID */
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
%type <exp> expr Pexpr

%type <loc> K L M N P B

/*declare attribute types for non-terminals*/
%type <typ> type list args 

/*implementation of recursion Head parsing*/
%type <ent> head ftype

%type <num> ptr

%%

prog	: Mprog exts	{ 
			  addwidth(top_tblptr, top_offset);
			  pop_tblptr;
			  pop_offset;
			}
	;

Mprog	: /* empty */	{ 
			  push_tblptr(mktable(NULL));
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
	| head block	
		{
			  emit(return_);
			  // length of bytecode is in the emitter's pc variable
			  cf.methods[cf.method_count].code_length = pc;
			  // must copy code to make it persistent
			  cf.methods[cf.method_count].code = copy_code();
			  if (!cf.methods[cf.method_count].code) {
				error("Out of memory");
			  }
			  // max operand stack size of this method
			  cf.methods[cf.method_count].max_stack = 100;
			  // local variables
			  cf.methods[cf.method_count].max_locals = top_offset;
			  // advance to next method to store in method array
			  cf.method_count++;
			  if (cf.method_count > MAXFUN) {
			  	error("Max number of functions exceeded");
			  }
			  // complete the enterproc table pointer
			  $1->table = top_tblptr;
			  // add width information to table
			  addwidth(top_tblptr, top_offset);
			  // exit the local scope by popping
			  pop_tblptr;
			  pop_offset;
		}
	;



head	: ftype  Margs args ')'
		{ // the type of the function is a JVM type descriptor
			Type type = mkfun($3, ret_type);
			  // method has public access and is static
			  cf.methods[cf.method_count].access = ACC_PUBLIC | ACC_STATIC;
			  // method name (from symbol table)
			  cf.methods[cf.method_count].name = $1->sym -> lexptr;
			  // method descriptor
			  cf.methods[cf.method_count].descriptor = type;
			  // enter the incomplete function in the global table
			  $$ = enterproc(top_tblptr, $1->sym, type, NULL);
			  ret_type = type;
 			
		}
	;

ftype   : type ID '(' 
		{
			ret_type = $1;
			$$ ->sym = $2;
		}
	;

Mmain	:	{ 
			  int label1, label2;
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
			  ret_type = mkint();
		}
	;

Margs	:	{ 
			   Table *table;
			  // create new table for function args and locals
			  table = mktable(top_tblptr);
			  // push it to create new scope
			  push_tblptr(table);
			  // start with local variable 0
			  push_offset(0);
			  // init code block to store stmts
			  init_code();
			  // global flag to indicate we're not in main()
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

args : args ',' type ID
       { 
           // Check if the type is 'char', and if true, treat it as a 'string'
           if ($4 && ischar($3))
               enter(top_tblptr, $4, mkstr(), top_offset++);
           else
               enter(top_tblptr, $4, $3, top_offset++);
           // Combine the existing argument list with the new type
           $$ = mkpair($1, $3);
       }
    | type ID
       { 
           // Check if the type is 'char', and if true, treat it as a 'string'
           if ($2 && ischar($1))
               enter(top_tblptr, $2, mkstr(), top_offset++);
           else
               enter(top_tblptr, $2, $1, top_offset++);
           // Set the return value to the type
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
           		
			} else {
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

			  } else {
			  	enter(top_tblptr, $2, $1, top_offset++);
			  }
			  $$ = $1;
		}
	;

stmts   : stmts stmt
        | /* empty */
        ;

ptr	: /* empty */	{ $$ = 0; }
	| '*'		{ $$ = 1; }
	;		

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
				error("Type error");
			}
			backpatch($5, $10 - $5);
			backpatch($9, pc - $5);
			}


	| WHILE '(' L expr ')' M B L stmt N
			{
			    if (!$4.type) {
			        backpatchlist($4.truelist, $8);
			        backpatchlist($4.falselist, pc);
			    } else if (!isint($4.type)) {
			        error("Type error");
			    }
			
			    backpatch($6, pc - $6);
			    backpatch($10, $3 - $10);
			
			    // Backpatch `break` statements for this level
			    backpatchlist(breaks[level--], pc);
			}

        | DO B L stmt WHILE '(' expr ')' K ';'
		{
			if (!$7.type) {
				backpatchlist($7.truelist, $3);
				backpatchlist($7.falselist, pc);
			} else if (!isint($7.type)) {
				error("Type error");
			}
			backpatch($9,$3-$9);
			// Backpatch `break` statements for this level
			backpatchlist(breaks[level--], pc);
		}

        | FOR '(' Pexpr ';' L expr M N ';' L Pexpr N ')' B L stmt N
        	{
			if (!$6.type) {
				backpatchlist($6.truelist, $15);
				backpatchlist($6.falselist, pc);
			} else if (!isint($6.type)) {
			  	error("Type error");
			}
            backpatch($7, pc - $7);
            backpatch($8, $15 - $8);
			backpatch($12, $5 - $12);
			backpatch($17, $10 - $17);
			// backpatch goto of break statement
			backpatchlist(breaks[level--], pc);
		}
                        
        | RETURN expr ';'
        	{ 
            coerce1(&$2, ret_type);
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
		
		| BREAK ';' {
			if (level < 0) {
				error("break statement not within loop");
			} else {
				// Add the current program counter to the `breaks[level]` list
				breaks[level] = mergelist(breaks[level], makelist(pc));
				
				// Emit a placeholder instruction for the break
				emit2(goto_, 0);  // `emit2` assumes the instruction is a two-byte opcode
			}
		}


        | '{' stmts '}'
        | error ';'     { yyerrok; }
        ;

exprs	: exprs ',' expr
		{
			decircuit(&$3);
		}
		
	| expr		
		{
			decircuit(&$1); 
		}
	;

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
	// This rule handles the logical OR (||) operator
	| expr OR L expr {
	    $$.type = NULL;  // Initialize the type of the result as NULL
	
	    // Check if both operands have types
	    if ($1.type && $4.type) {
	        // Both operands are non-short-circuiting
	        if (isint($1.type) && isint($4.type)) {  // Check if both operands are integers
	            // Emit an instruction that will jump if the first operand is equal to zero
	            emit3(ifeq, 5);  
	            emit(pop);  // Pop the result of the first operand (as it's not used after evaluation)
	            emit(iconst_1);  // Push 1 to the stack (since OR short-circuits to 1 if the first operand is true)
	            $$ = circuit(&$4);  // Process the second operand using the `circuit` function
	        } else {
	            error("Type error");  // Throw a type error if the operands are not of the expected type
	        }
	    } else if ($4.type) {
	        // If only the second operand has a type, handle it as the primary condition
	        Expr e = circuit(&$4);  // Evaluate the second operand using `circuit`
	        $$ .truelist = mergelist($1.truelist, e.truelist);  // Merge the true lists of both operands
	        backpatchlist($1.falselist, $3);  // Backpatch the false list of the first operand
	        $$ .falselist = e.falselist;  // Set the false list of the result to the false list of the second operand
	    } else if ($1.type) {
	        // If only the first operand has a type, handle it as the primary condition
	        Expr e = circuit(&$1);  // Evaluate the first operand using `circuit`
	        $$ .truelist = mergelist(e.truelist, $4.truelist);  // Merge the true lists of both operands
	        backpatchlist(e.falselist, $3);  // Backpatch the false list of the first operand
	        $$ .falselist = $4.falselist;  // Set the false list of the result to the false list of the second operand
	    } else {
	        // If neither operand has a type, merge both true and false lists
	        $$ .truelist = mergelist($1.truelist, $4.truelist);  // Merge true lists from both operands
	        backpatchlist($1.falselist, $3);  // Backpatch the false list of the first operand
	        $$ .falselist = $4.falselist;  // Set the false list of the result to the false list of the second operand
	    }
	}
	
	| expr AN L expr
		{
			// Set head expr to be short-circuited
			$$.type = NULL;
			// If both exprs are not short-circuited
			if ($1.type && $4.type) {
				// If both exprs are ints
				if (isint($1.type) && isint($4.type)) {
					// If expr2 if true, branch ahead 5 (keep expr1)
					emit3(ifne, 5);
					// Else, if expr2 was false, entire expr is false so pop and push 0
					emit(pop);
					emit(iconst_0);
					// Make expr head be the short-circuited Expr for expr2
					$$ = circuit(&$4);
				} else {
					error("Type error");
				}
			// If expr1 is already short-circuited
			} else if ($4.type) {
				// Make expr2 short-circuited
				Expr e = circuit(&$4);
				// Add expr2's pc to expr1's falselist 
				$$.falselist = mergelist($1.falselist, e.falselist);
				// When expr1 is true, check expr2
				backpatchlist($1.truelist, $3);
				// Set head to go wherever expr2 goes to when true
				$$.truelist = e.truelist;
			// If expr2 is already short-circuited
			} else if ($1.type) {
				// Short-circuit expr1
				Expr e = circuit(&$1);
				// Add expr2's falselist to expr1's falselist
				$$.falselist = mergelist(e.falselist, $4.falselist);
				// If expr1 is true, go to expr 2
				backpatchlist(e.truelist, $3);
				$$.truelist = $4.truelist;
			// If both are already short-circuited
			} else {
				$$.falselist = mergelist($1.falselist, $4.falselist);
				backpatchlist($1.truelist, $3);
				$$.truelist = $4.truelist;
			}
		}

        | expr '|' expr {
			if(isint($1.type) && isint($3.type)) {
				emit(ior);
			} else {
				error("Type error");			}
			$$.type = $3.type;
		}

        | expr '^' expr {
			if(isint($1.type) && isint($3.type)) {
				emit(ixor);
			} else {
				error("Type error");
			}
			$$.type = $3.type;
		}

        | expr '&' expr {
			if(isint($1.type) && isint($3.type)) {
				emit(iand);
			} else {
				error("Type error");
			}
			$$.type = $3.type;
	    }

        | expr EQ  expr { 
				emitcmp(&$1, &$3, if_icmpeq); 
			}
        | expr NE  expr {
				emitcmp(&$1, &$3, if_icmpne);
			}
        | expr '<' expr { 
				emitcmp(&$1, &$3, if_icmplt);
			}
        | expr '>' expr {
				emitcmp(&$1, &$3, if_icmpgt);
			}
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
        | expr GE  expr {
				emitcmp(&$1, &$3, if_icmpge);
			}
        | expr LS  expr { $$ = emitop(&$1,&$3,ishl,nop); } 
        | expr RS  expr { $$ = emitop(&$1,&$3,ishr,nop); }
        | expr '+' expr { $$ = emitop(&$1, &$3, iadd, fadd); } 
        | expr '-' expr { $$ = emitop(&$1,&$3,isub,fsub); }
        | expr '*' expr { $$ = emitop(&$1,&$3,imul,fmul); }
        | expr '/' expr { $$ = emitop(&$1,&$3,idiv,fdiv); }
        | expr '%' expr { $$ = emitop(&$1,&$3,irem,nop); }
        | '!' expr
		{
			$$.type = NULL;
			// If expr1 is not short-circuited
			if ($2.type) {
				if (isint($2.type)) {
					emit(ineg);
					emit(iconst_1);
					emit(iadd);
					$$ = $2;
				} else {
					error("Type error");
				}
			} else {
				$$.falselist = $2.truelist;
				$$.truelist = $2.falselist;
			}
		}
        | '~' expr {
			if(isint($2.type)) {
				emit(ineg);
				emit(iconst_1);
				emit(isub);
			} else {
				error("Type error");
			}
		}

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
        | PP ID         { $$ = emitinc($2,0,iadd,fadd); }
        | NN ID         { $$ = emitinc($2,0,isub,fsub); }
        | ID PP         { $$ = emitinc($1,1,iadd,fadd); }
        | ID NN         { $$ = emitinc($1,1,isub,fsub);}
        | ID
		{
        		int place = getplace(top_tblptr, $1);
    			Type type = gettype(top_tblptr, $1);
    			if(isint(type)) {
    				if(getlevel(top_tblptr,$1) == 0) {
    					emit3(getstatic, place);
    				} else {
    				  	emit2(iload,place);
    				} 
    			} else if (isfloat(type)){
    				if(getlevel(top_tblptr,$1) == 0){
    					emit3(getstatic, place);
    				} else { 
    					emit2(fload,place);
    				}
    			}
    			$$.type = type;
        	}
		| INT8          { emit2(bipush, $1); }
		| INT16         { emit3(sipush, $1); }
		| INT32         { emit2(ldc, constant_pool_add_Integer(&cf, $1)); }
		| FLT		{ emit2(ldc, constant_pool_add_Float(&cf, $1)); }
		| STR		{ emit2(ldc, constant_pool_add_String(&cf, constant_pool_add_Utf8(&cf, $1))); }
		| ID '(' exprs ')'
			{
				Type type = gettype(top_tblptr, $1);
				$$.type = mkret(type);
				if ($$.type && getlevel(top_tblptr, $1) == 0) {
					emit3(invokestatic, constant_pool_add_Methodref(&cf, cf.name, $1->lexptr, type));
				} else {
					error("Invalid function call");
				}
				$$.truelist = $$.falselist = NULL;
			}
		;

Pexpr   : expr P { $$ = $1; }
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
B       : /* empty */   {  }
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
    
    // If both type's are not null and are the same, return the type
    if (iseq(expr->type, type)) {
	rc = type;
    // If they are not the same type
    } else {
    	// Have short-circuiting jump to here and set conv to the type
	conv = decircuit(expr);
	// If the decircuited expr is an int and the passed type is float
	if (isint(conv) && isfloat(type)) {
	    // Convert the top of the stack (whatever expr's truth/falseness was) to a float
	    emit(i2f);
	// If the decircuited expr is an float and the passed type is int
	} else if (isfloat(conv) && isint(type)) {
	    // Convert the top of the stack to be an int
	    emit(f2i);
	}
	rc = type;

    }
    
    // Return the passed type
    return rc;
}

/* Coerce value under top of stack */
static Type coerce2(Expr *expr, Type type) {
    Type conv;
    Type rc;

    // If both type's are not null and are the same, return the type
    if (iseq(expr->type, type)) {
	rc = type;
    // If they are not the same type
    } else {
    	// If expr is not short-circuited
	if (expr->type) {
	    // Get the second-to-top element be at the top
	    emit(swap);
	}
	// Coerce the new top
	conv = coerce1(expr, type);
	// Put it back in its place
	emit(swap);
	rc = conv;
    }

    // Return the passed type
    return rc;
}

/* Convert integer to short-circuit, no change when already short circuit */
static Expr circuit(Expr *expr) {
    Expr result;

    // If expr is already short-circuited, just return the dereferenced expr
    if (!expr->type) {
	result = *expr;
    // If it has not yet been short-circuited and it is an int
    } else if (isint(expr->type)) {
    	// Make a new backpatch list that holds the current pc
	result.falselist = makelist(pc);
	// Make a goto for when it is false
	emit3(ifeq, 0);
	// Make a goto for when it is true;
	// if it was false, it would have branched at ifeq
	result.truelist = makelist(pc);
	emit3(goto_, 0);
    } else {
	error("Type error curcuit");
    }

    result.type = expr->type;

    return result;
}

/* Convert short-circuit logic to push an int 0 or 1 by backpatching */
/* If the passed expr is a non-short-circuited value, the expr has already */
/* been emitted, so just return the type; otherwise, backpatch all the truelist */
/* and falselist gotos to come here and emit 1 or 0 accordingly */
static Type decircuit(Expr *expr) {
    Type rc;
    // If the passed expr's type is not null
    if (expr->type) {
    	// Return the type
	rc = expr->type;
    // If it is currently short-circuited (type is null)
    } else {
    	// Backpatch each goto on the falselist to go here
	backpatchlist(expr->falselist, pc);
	// Push a 0 on the stack (false)
	emit(iconst_0);
	// Skip the iconst 1
	emit3(goto_, 4);
	// Make all the gotos go here
	backpatchlist(expr->truelist, pc);
	// Push a true
	emit(iconst_1);

	// return Type int ("I")
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
	error("Type error operation");
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
		error("Type error assign globe");
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
	error("Type error assign loc");
    }

    return result;
}

/*emit assignment ops for increment assign(id++ and variants) prepost variable  0 = pre, 1 = post*/
static Expr emitinc(Symbol *sym, int prepost, int iop, int fop) {
    Expr result;
    int place;
    Type type;

    result.truelist = result.falselist = NULL;

    place = getplace(top_tblptr, sym);
    type = gettype(top_tblptr, sym);

    result.type = type;
    /*check if pre/post inc*/
    if (prepost == 0) {
    /*check if global level or not */
    	if (getlevel(top_tblptr, sym) == 0) {
    		if (isint(result.type) && iop != nop){
    			emit(iconst_1);
    			emit3(getstatic,place);
    			emit(swap);
    			emit(iop);
    		} else if (isfloat(result.type) && fop != nop){
    			emit(fconst_1);
    			emit3(getstatic,place);
    			emit(swap);
    			emit(fop);
    		} else {
			error("Type error INC");
	    	}
	
	emit(dup);
	emit3(putstatic, place);
	/*if not global check type to see what kind of inc/dec*/
    	} else if (isint(result.type) && iop !=nop) {
    		emit(iconst_1);
	    	emit2(iload, place);
	    	emit(swap);
	    	emit(iop);
		emit(dup);
		emit2(istore, place);
    	} else if (isfloat(result.type) && fop != nop) {
    	    	emit(fconst_1);
	    	emit2(fload, place);
	    	emit(swap);
	    	emit(fop);
		emit(dup);
		emit2(fstore, place);
    	} else {
	error("Type error INC");
    	}
    	/*this is if post inc/dec*/
    } else if (prepost == 1) {
    /*check if global */
    	if (getlevel(top_tblptr, sym) == 0) {
    		if (isint(result.type) && iop != nop){
    			emit3(getstatic,place);
    			emit(dup);
    			emit(iconst_1);
    			emit(swap);
    			emit(iop);
    		} else if (isfloat(result.type) && fop != nop){
    			emit3(getstatic,place);
    			emit(dup);
    			emit(fconst_1);
    			emit(swap);
    			emit(fop);
    		} else {
			error("Type error INC");
	    	}
	
	emit3(putstatic, place);
    	} else if (isint(result.type) && iop !=nop) {
    		
	    	emit2(iload, place);
	    	emit(dup);
	    	emit(iconst_1);
	    	emit(swap);
	    	emit(iop);
		emit2(istore, place);
    	} else if (isfloat(result.type) && fop != nop) {
    	    	
	    	emit2(fload, place);
	    	emit(dup);
	    	emit(fconst_1);
	    	emit(swap);
	    	emit(fop);
		emit2(fstore, place);
    	} else {
		error("Type error INC");
    	}
    } else {
    	error("Type error INC");
    }


    return result;
}

static Expr emitcmp(Expr *exp1, Expr *exp2, int icmp) {
	Expr result;
	/*set truelist and falselist to null*/
    	result.truelist = result.falselist = NULL;
    	/*widen the result to fit expr and check if int and compare
    	if it isnt error*/
    	result.type = widen(exp1, exp2);
	Type t = widen(exp1, exp2);
	if (isint(t)) {
		emit3(icmp, 7);
		emit(iconst_0);
		emit3(goto_, 4);
		emit(iconst_1);
	} else if (isfloat(t)) {
		error("Type error");
	}
	return result;
}
