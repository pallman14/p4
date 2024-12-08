/*
Price, Chris, Gorana, Lian
GCSC554
Project: P4
File: global.h
*/

#ifndef GLOBAL_H
#define GLOBAL_H

#include "javaclass.h"  // Include definitions related to Java class handling
#include "bytecode.h"   // Include definitions for bytecode operations

/* 
  Structure representing a symbol in the compiler.
  Typically used to store identifiers and related information.
*/
struct Symbol
{
    const char *lexptr;  // Pointer to the lexeme (name of the symbol)
    int token;           // Token associated with the symbol (e.g., identifier token)
    int localvar;        // (Deprecated) Previously used for local variable tracking
};

typedef struct Symbol Symbol;  // Typedef for convenience

/*
  Type descriptor used in the JVM.
  Represents the type of a symbol or expression.
 */
typedef const char *Type;

/* Structure representing an entry in a symbol table. 
Each entry stores information about a symbol, including its type, position, and scope. */
typedef struct Entry
{
    struct Entry *next;   // Pointer to the next entry in the linked list or NULL
    Symbol *sym;          // Pointer to the associated symbol (identifier)
    Type type;            // Type descriptor of the symbol
    int place;            // Place or position (e.g., local variable index or memory offset)
    struct Table *table;  // Pointer to a table (used when the entry represents a function)
} Entry;

/*
  Structure representing a symbol table.
  Maintains a list of symbols within a particular scope (global or local).
 */
typedef struct Table
{
    struct Table *prev;   // Pointer to the previous (parent) table for nested scopes
    struct Entry *list;   // Linked list of entries in the current table
    int width;            // Cumulative width of entries (e.g., total memory used)
    int level;            // Scope level: 0 for global, 1 for local
} Table;

/*
  Structure representing an expression.
  Stores information for expressions with backpatching and type data.
 */
typedef struct Expr
{
    Backpatchlist *truelist;  // List of locations to backpatch if the expression evaluates to true
    Backpatchlist *falselist; // List of locations to backpatch if the expression evaluates to false
    Type type;                // Type of the expression; NULL for short-circuit evaluation
} Expr;

/*
  Structure representing a backpatch list.
  Used to keep track of locations that need to be backpatched.
 */
typedef struct Backpatchlist
{
    struct Backpatchlist *next;  // Pointer to the next node in the backpatch list
    int loc;                     // Location to be backpatched (e.g., program counter)
} Backpatchlist;

//Function declarations for symbol table operations.
/* Searches for a symbol in the symbol table. 
  Parameters:
    const char *name - The name of the symbol to look up.
  Returns:
    A pointer to the `Symbol` if found; otherwise, NULL.
 */
extern Symbol *lookup(const char* name);

/* 
  Inserts a new symbol into the symbol table.
  Parameters:
    const char *name - The name of the symbol to insert.
    int token        - The token associated with the symbol.
  Returns:
    A pointer to the newly inserted `Symbol`.
 */
extern Symbol *insert(const char* name, int token);

// create a new table and link to previous table in hierarchy:
extern Table *mktable(Table *prev);
// print table
extern void dumptable(Table *table);
// add width information to table:
extern void addwidth(Table *table, int width);
// enter a declaration to the table for symbol with type and place:
extern Entry *enter(Table *table, Symbol *sym, Type type, int place);
// enter a procedure (function) to the table:
extern Entry *enterproc(Table *table, Symbol *sym, Type type, Table *newtable);
// find the level at which the variable is declared (0=global, 1=local, -1 when not found):
extern int getlevel(Table *table, Symbol *sym);
// find the type of a variable (NULL when not found):
extern Type gettype(Table *table, Symbol *sym);
// find the place of a variable:
extern int getplace(Table *table, Symbol *sym);

// return a new function type:
extern Type mkfun(Type args, Type result);
// return a pair of types to construct argument list for functions:
extern Type mkpair(Type type1, Type type2);
// return void type:
extern Type mkvoid();
// return int type:
extern Type mkint();
// return char type:
extern Type mkchar();
// return string type:
extern Type mkstr();
// return float type:
extern Type mkfloat();
// if type is function, get return type (or NULL):
extern Type mkret(Type type);

// check if type is void:
extern int isvoid(Type type);
// check if type is int:
extern int isint(Type type);
// check if type is float:
extern int isfloat(Type type);
// check if type is char:
extern int ischar(Type type);
// check if type is string:
extern int isstr(Type type);
// check if two types are equal:
extern int iseq(Type type1, Type type2);

extern void init(void);
extern void error(const char*);

extern char *yytext;
extern int yylineno;

extern int yylex();
extern int yyparse();
extern int yyerror(const char*);

extern int errnum;

#endif
