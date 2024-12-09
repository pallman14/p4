/* 
Price, Chris, Gorana, Lian
GCSC554
Project: p4
File: global.h
*/

#ifndef GLOBAL_H
#define GLOBAL_H

#include "javaclass.h"
#include "bytecode.h"

/* 
This code defines several structures and functions for symbol table management,
type handling, and backpatching, used in a compiler or interpreter context.
*/

/* Structure representing a symbol with its lexical representation and token type */
struct Symbol
{
	const char *lexptr; // Pointer to the lexeme (identifier or literal).
	int token;          // Token type associated with the symbol.
};

typedef struct Symbol Symbol;

/* Type descriptor for the JVM, represented as a string */
typedef const char *Type; // A string representing the type (e.g., int, float, char).

/* Structure representing an entry in the symbol table */
typedef struct Entry
{
	struct Entry *next;  // Pointer to the next entry in the linked list (or NULL if last).
	Symbol *sym;         // Pointer to the symbol (identifier).
	Type type;           // Type of the symbol (e.g., int, float).
	int place;           // Place or location information (e.g., memory or register).
	struct Table *table; // Pointer to a nested table (used for functions/procedures).
} Entry;

/* Structure representing a symbol table */
typedef struct Table
{
	struct Table *prev; // Pointer to the previous (parent) table in the hierarchy.
	struct Entry *list; // Linked list of entries in the current table.
	int width;          // Total memory width occupied by entries in the table.
	int level;          // Scope level (0 for global, 1 for local).
} Table;

/* Structure representing a backpatching list for handling control flow */
typedef struct Backpatchlist
{
	struct Backpatchlist *next; // Pointer to the next node in the backpatching list.
	int loc;                    // Location information for backpatching.
} Backpatchlist;

/* Structure representing an expression with associated backpatching lists */
typedef struct Expr
{
	Backpatchlist *truelist; // List of locations to backpatch for the "true" condition.
	Backpatchlist *falselist; // List of locations to backpatch for the "false" condition.
	Type type;               // Type of the expression (NULL for short-circuit evaluation).
} Expr;

/* Function declarations for symbol table operations */

// Lookup a symbol in the table by its lexeme.
extern Symbol *lookup(const char*);

// Insert a new symbol into the table with a given token type.
extern Symbol *insert(const char*, int);

/* Table management */

// Create a new table linked to the previous table in the hierarchy.
extern Table *mktable(Table *prev);

// Print the contents of the given symbol table.
extern void dumptable(Table *table);

// Add memory width information to the given table.
extern void addwidth(Table *table, int width);

// Add a declaration to the table with symbol, type, and place.
extern Entry *enter(Table *table, Symbol *sym, Type type, int place);

// Add a procedure (function) declaration to the table.
extern Entry *enterproc(Table *table, Symbol *sym, Type type, Table *newtable);

// Determine the level (scope) where a variable is declared (0 for global, 1 for local, -1 if not found).
extern int getlevel(Table *table, Symbol *sym);

// Retrieve the type of a variable (returns NULL if not found).
extern Type gettype(Table *table, Symbol *sym);

// Retrieve the location (place) of a variable.
extern int getplace(Table *table, Symbol *sym);

/* Type handling */

// Create a function type descriptor with argument types and a return type.
extern Type mkfun(Type args, Type result);

// Create a pair of types (used for constructing argument lists).
extern Type mkpair(Type type1, Type type2);

// Return a "void" type descriptor.
extern Type mkvoid();

// Return an "int" type descriptor.
extern Type mkint();

// Return a "char" type descriptor.
extern Type mkchar();

// Return a "string" type descriptor.
extern Type mkstr();

// Return a "float" type descriptor.
extern Type mkfloat();

// Retrieve the return type of a function type (returns NULL if not a function).
extern Type mkret(Type type);

/* Type checking */

// Check if a type is "void".
extern int isvoid(Type type);

// Check if a type is "int".
extern int isint(Type type);

// Check if a type is "float".
extern int isfloat(Type type);

// Check if a type is "char".
extern int ischar(Type type);

// Check if a type is "string".
extern int isstr(Type type);

// Check if two types are equivalent.
extern int iseq(Type type1, Type type2);

/* Backpatching utilities */

// Create a new backpatching list with a single node at the given location.
extern Backpatchlist *makelist(unsigned loc);

// Perform backpatching by setting all locations in the list to the specified location.
extern void backpatchlist(Backpatchlist *list, int loc);

// Merge two backpatching lists and return the combined list.
extern Backpatchlist *mergelist(Backpatchlist *list1, Backpatchlist *list2);

/* Initialization and error handling */

// Initialize the compiler or interpreter state.
extern void init(void);

// Report an error with a specified message.
extern void error(const char*);

/* Flex/Bison utility variables and functions */

extern char *yytext;   // The current token text.
extern int yylineno;   // The current line number in the input.

extern int yylex();    // The lexer function.
extern int yyparse();  // The parser function.
extern int yyerror(const char*); // Error reporting function for the parser.

extern int errnum;     // Counter for the number of errors encountered.

#endif // End of header file
