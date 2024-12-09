/* 
Price, Chris, Gorana, Lian
GCSC554
Project: p4
File: mycc.l
*/

%option noyywrap
%option yylineno
%option nounistd
%option nounput
%option noinput
%option always-interactive

%{
#include "global.h"
#include "mycc.h"

static int install_id();
static int install_int();
static int install_oct();
static int install_hex();
static int install_chr();
static int install_fp();
static int install_str();
%}

digit           [0-9]
alpha           [a-zA-Z_]
id              {alpha}({alpha}|{digit})*
int             {digit}+
oct             0[0-7]+
hex             0[xX][0-9a-fA-F]+
chr             '(\\'|[^'\n])*'
fp              {int}(\.{int}([Ee][+-]?{int})?|(\.{int})?[Ee][+-]?{int})
str             \"(\\\"|\\\n|[^"])*\"

%%

[ \t\v\n\f\r]                  /* skip white space */
\/\/.*						   /* ignore inline comments */
\/\*([^*]|(\*+[^*/]))*\*+\/    /* ignore multiline comments */
{id}                           { return install_id(); }
{oct}                          { return install_oct(); }
{hex}                          { return install_hex(); }
{int}                          { return install_int(); }
{chr}                          { return install_chr(); }
{fp}                           { return install_fp(); }
{str}                          { return install_str(); }
"+="                           { return PA; }
"-="                           { return NA; }
"*="                           { return TA; }
"/="                           { return DA; }
"%="                           { return MA; }
"&="                           { return AA; }
"^="                           { return XA; }
"|="                           { return OA; }
"<<="                          { return LA; }
">>="                          { return RA; }
"||"                           { return OR; }
"&&"                           { return AN; }
"=="                           { return EQ; }
"!="                           { return NE; }
"<="                           { return LE; }
">="                           { return GE; }
"<<"                           { return LS; }
">>"                           { return RS; }
"++"                           { return PP; }
"--"                           { return NN; }
"->"                           { return AR; }
"<%"                           { return '{'; }
"%>"                           { return '}'; }
"<:"                           { return '['; }
":>"                           { return ']'; }
[!$%&()*+,\-./:;<=>?\[\]^{|}~] { return yytext[0]; }
.                              { yyerror("Unknown character ignored"); }

%%

// Function implementations for processing specific tokens

/* install_id - Adds an identifier to the symbol table if not already present. */
static int install_id()
{
	yylval.sym = lookup(yytext); // Look up identifier in the symbol table.

	if (!yylval.sym)             // If not found, insert it.
	{
		yylval.sym = insert(yytext, ID);
		/*	yylval.sym->localvar = localvar++; */
	}

	return yylval.sym->token;    // Return the token type.
}

/* install_int - Converts a decimal integer to INT8, INT16, or INT32 based on size. */
static int install_int()
{
	sscanf(yytext, "%u", &yylval.num); // Convert string to unsigned integer.

	if (yylval.num < 128)
		return INT8;

	if (yylval.num < 32368)
		return INT16;

	return INT32;
}

/* install_oct - Converts an octal integer to INT8, INT16, or INT32. */
static int install_oct()
{
	sscanf(yytext, "%o", &yylval.num); // Convert string to octal integer.

	if (yylval.num < 128)
		return INT8;

	if (yylval.num < 32368)
		return INT16;

	return INT32;
}

/* install_hex - Converts a hexadecimal integer to INT8, INT16, or INT32. */
static int install_hex()
{
	sscanf(yytext, "%x", &yylval.num); // Convert string to hexadecimal integer.

	if (yylval.num < 128)
		return INT8;

	if (yylval.num < 32368)
		return INT16;

	return INT32;
}

/* install_chr - Converts an ASCII character to INT8. */
static int install_chr()
{
	char c;

	sscanf(yytext + 1, "%c", &c);  // Extract character from input.
	yylval.num = (int)c;          // Store character as integer.

	return INT8;
}

/* install_fp - Converts a floating-point literal to a float (FLT). */
static int install_fp()
{
	sscanf(yytext, "%g", &yylval.flt); // Convert string to floating-point value.

	return FLT;
}

/* install_str - Duplicates a string literal, removing the enclosing quotes. */
static int install_str()
{
	yylval.str = (char*)malloc(yyleng - 1); // Allocate memory for string.

	if (yylval.str)
	{
		strcpy(yylval.str, yytext + 1);       // Copy string without the first quote.
		yylval.str[yyleng - 2] = '\0';       // Remove the last quote.
	}

	return STR;
}
