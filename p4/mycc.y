/*
Price, Chris, Gorana, Lian
GCSC 554
Project: P4
File: mycc.y
*/

%{

#include "lex.yy.h"
#include "global.h"

#define MAXLEVEL 10  // Adjust based on the maximum nesting level of loops
static List *breaks[MAXLEVEL];
static int level = -1;  // Used to track the current loop level

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
%token <sym> BREAK CHAR DO ELSE FLOAT FOR IF INT MAIN RETURN VOID WHILE TRUE FALSE

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

%type <typ> type list args

%type <num> ptr

%type <Table *> func

%type <ent> head ftype



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
	| head block    
			{ emit(return_);
                          // length of bytecode is in the emitter's pc variable
                          cf.methods[cf.method_count].code_length = pc;
                          // must copy code to make it persistent
                          cf.methods[cf.method_count].code = copy_code();
                          if (!cf.methods[cf.method_count].code)
                                error("Out of memory");
                          // max operand stack size of this method
                          cf.methods[cf.method_count].max_stack = 100;
                          // local variables
                          cf.methods[cf.method_count].max_locals = top_offset;
                          // advance to next method to store in method array
                          cf.method_count++;
                          if (cf.method_count > MAXFUN)
                                error("Max number of functions exceeded");
                          // complete the enterproc table pointer
                          $1->table = top_tblptr;
                          // add width information to table
                          addwidth(top_tblptr, top_offset);
                          // exit the local scope by popping
                          pop_tblptr;
                          pop_offset;
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
head    : ftype ID '(' Margs args ')'
                        { // the type of the function is a JVM type descriptor
                          Type type = mkfun($5, $1);
                          // method has public access and is static
                          cf.methods[cf.method_count].access = ACC_PUBLIC | ACC_STATIC;
                          // method name (from symbol table)
                          cf.methods[cf.method_count].name = $2->lexptr;
                          // method descriptor
                          cf.methods[cf.method_count].descriptor = type;
                          // enter the incomplete function in the global table
                          $$ = enterproc(snd_tblptr, $2, type, NULL);
                          ret_type = $1;
                        }
        ;
ftype   : type ID '(' 
		{
			ret_type = $1;
			$$ ->sym = $2;
		}
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

	| WHILE '(' L expr ')' M B L stmt N
			{
			    if (!$4.type) {
			        backpatchlist($4.truelist, $8);
			        backpatchlist($4.falselist, pc);
			    } else if (!isint($4.type)) {
			        error("Type error WHILE");
			    }
			
			    backpatch($6, pc - $6);
			    backpatch($10, $3 - $10);
			
			    // Backpatch `break` statements for this level
			    backpatchlist(breaks[level--], pc);
			}



        | DO B L stmt WHILE '(' expr ')' K ';'
			{
			    if (!$6.type) {
			        backpatchlist($6.truelist, $2);
			        backpatchlist($6.falselist, pc);
			    } else if (!isint($6.type)) {
			        error("Type error DOWHILE");
			    }
			
			    backpatch($8, $2 - $8);
			
			    // Backpatch `break` statements for this level
			    backpatchlist(breaks[level--], pc);
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
	| BREAK ';' 
		{
		    // Ensure `break` is used inside a loop
		    if (level < 0) {
		        error("break statement not within loop");
		    } else {
		        // Add the current program counter to the `breaks[level]` list
		        breaks[level] = mergelist(breaks[level], makelist(pc));
		
		        // Emit a placeholder instruction for the break 
		        emit(JUMP, 0);  // `emit` is a function that emits bytecode instructions
		    }
		}

        | '{' stmts '}'
        | error ';'     { yyerrok; }
        ;

exprs	: exprs ',' expr
			{ decircuit(&$3); }
	| expr		{ decircuit(&$1); }
	;

/* TASK 1: TO BE COMPLETED (use pr3 code, then work on assign operators): */
expr    : ID   '=' expr { emit(dup); emit2(istore, $1->localvar); }
        // ID += expr: Load the current value of ID, add expr, and store the result back into ID
        | ID   PA  expr { emit2(iload , $1->localvar); emit(iadd); emit(dup); emit2(istore, $1->localvar); }
        // ID -= expr: Load the current value of ID, subtract expr, and store the result back into ID
        | ID   NA  expr { emit2(iload , $1->localvar); emit(swap); emit(isub); emit(dup); emit2(istore, $1->localvar); }
        // ID *= expr: Load the current value of ID, multiply by expr, and store the result back into ID
        | ID   TA  expr { emit2(iload , $1->localvar); emit(imul); emit(dup); emit2(istore, $1->localvar); }
        // ID /= expr: Load the current value of ID, divide by expr, and store the result back into ID
        | ID   DA  expr { emit2(iload , $1->localvar); emit(swap); emit(idiv); emit(dup); emit2(istore, $1->localvar); }
        // ID %= expr: Load the current value of ID, calculate the remainder with expr, and store the result back into ID
        | ID   MA  expr { emit2(iload , $1->localvar); emit(swap); emit(irem); emit(dup); emit2(istore, $1->localvar); }
        // ID &= expr: Perform bitwise AND on ID and expr, store the result back into ID
        | ID   AA  expr { emit2(iload , $1->localvar); emit(iand); emit(dup); emit2(istore, $1->localvar); }
        // ID ^= expr: Perform bitwise XOR on ID and expr, store the result back into ID
        | ID   XA  expr { emit2(iload , $1->localvar); emit(ixor); emit(dup); emit2(istore, $1->localvar); }
        // ID |= expr: Perform bitwise OR on ID and expr, store the result back into ID
        | ID   OA  expr { emit2(iload , $1->localvar); emit(ior); emit(dup); emit2(istore, $1->localvar); }
        // ID <<= expr: Perform bitwise left shift on ID by expr, store the result back into ID
        | ID   LA  expr { emit2(iload , $1->localvar); emit(swap); emit(ishl); emit(dup); emit2(istore, $1->localvar); }
        // ID >>= expr: Perform bitwise right shift on ID by expr, store the result back into ID
        | ID   RA  expr { emit2(iload , $1->localvar); emit(swap); emit(ishr); emit(dup); emit2(istore, $1->localvar); }
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
	
	// This rule handles the logical AND (&&) operator
	| expr AN expr {
	    $$.type = NULL;  // Initialize the type of the result as NULL
	
	    // Check if both operands have types
	    if ($1.type && $4.type) {
	        if (isint($1.type) && isint($4.type)) {  // Check if both operands are integers
	            // Emit an instruction that will jump if the first operand is not equal to zero
	            emit3(ifne, 5);  
	            emit(pop);  // Pop the result of the first operand (as it's not used after evaluation)
	            emit(iconst_0);  // Push 0 to the stack (since AND short-circuits to 0 if the first operand is false)
	            $$ = circuit(&$4);  // Process the second operand using the `circuit` function
	        } else {
	            error("Type error");  // Throw a type error if the operands are not of the expected type
	        }
	    } else if ($4.type) {
	        // If only the second operand has a type, handle it as the primary condition
	        Expr e = circuit(&$4);  // Evaluate the second operand using `circuit`
	        $$ .falselist = mergelist($1.falselist, e.falselist);  // Merge the false lists of both operands
	        backpatchlist($1.truelist, $3);  // Backpatch the true list of the first operand
	        $$ .truelist = e.truelist;  // Set the true list of the result to the true list of the second operand
	    } else if ($1.type) {
	        // If only the first operand has a type, handle it as the primary condition
	        Expr e = circuit(&$1);  // Evaluate the first operand using `circuit`
	        $$ .falselist = mergelist(e.falselist, $4.falselist);  // Merge the false lists of both operands
	        backpatchlist(e.truelist, $3);  // Backpatch the true list of the first operand
	        $$ .truelist = $4.truelist;  // Set the true list of the result to the true list of the second operand
	    } else {
	        // If neither operand has a type, merge both false and true lists
	        $$ .falselist = mergelist($1.falselist, $4.falselist);  // Merge false lists from both operands
	        backpatchlist($1.truelist, $3);  // Backpatch the true list of the first operand
	        $$ .truelist = $4.truelist;  // Set the true list of the result to the true list of the second operand
	    }
	}

        | expr '|' expr {
	    if ($1.type && $3.type) {
	        // Both operands must be integers for bitwise OR
	        if (isint($1.type) && isint($3.type)) {
	            emit2(iload, $1.localvar);  // Load the first operand
	            emit2(iload, $3.localvar);  // Load the second operand
	            emit(ior);  // Bitwise OR operation
	            emit(dup);  // Duplicate the result
	            emit2(istore, $1.localvar);  // Store the result back in the first operand
	            $$ = $1;  // The result of the operation is stored in $1
	        } else {
	            error("Type error: operands must be integers for bitwise OR");
	        }
	    } else {
	        error("Type error: both operands must have a type for bitwise OR");
	    }
	}

        | expr '^' expr {
	    if ($1.type && $3.type) {
	        // Both operands must be integers for bitwise XOR
	        if (isint($1.type) && isint($3.type)) {
	            emit2(iload, $1.localvar);  // Load the first operand
	            emit2(iload, $3.localvar);  // Load the second operand
	            emit(ixor);  // Bitwise XOR operation
	            emit(dup);  // Duplicate the result
	            emit2(istore, $1.localvar);  // Store the result back in the first operand
	            $$ = $1;  // The result of the operation is stored in $1
	        } else {
	            error("Type error: operands must be integers for bitwise XOR");
	        }
	    } else {
	        error("Type error: both operands must have a type for bitwise XOR");
	    }
	}

        | expr '&' expr {
	    if ($1.type && $3.type) {
	        // Both operands must be integers for bitwise AND
	        if (isint($1.type) && isint($3.type)) {
	            emit2(iload, $1.localvar);  // Load the first operand
	            emit2(iload, $3.localvar);  // Load the second operand
	            emit(iand);  // Bitwise AND operation
	            emit(dup);  // Duplicate the result
	            emit2(istore, $1.localvar);  // Store the result back in the first operand
	            $$ = $1;  // The result of the operation is stored in $1
	        } else {
	            error("Type error: operands must be integers for bitwise AND");
	        }
	    } else {
	        error("Type error: both operands must have a type for bitwise AND");
	    }
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
	    if ($2.type) {
	        // Operand must be an integer for bitwise NOT
	        if (isint($2.type)) {
	            emit2(iload, $2.localvar);  // Load the operand
	            emit(ineg);  // Perform bitwise negation (bitwise NOT is equivalent to negation in this context)
	            $$ = $2;  // The result is stored in $2 (the operand)
	        } else {
	            error("Type error: operand must be an integer for bitwise NOT");
	        }
	    } else {
	        error("Type error: operand must have a type for bitwise NOT");
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

B 	: /* empty */ { }
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
