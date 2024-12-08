/* 
Price, Chris, Gorana, Lian
GCSC554
Project: P4
File: symbol.c
*/

#include "global.h"  

// Define maximum sizes for the string buffer and the symbol table
#define STRMAX 999    
#define SYMMAX 100    

// Global counter to track the total number of characters used in the string buffer
int charCnt = 0;  

// Global symbol table: an array of `Symbol` structures with a maximum capacity of SYMMAX
struct Symbol symTable[SYMMAX];  

// Global variable to track the index of the last entry in the symbol table
int lastEntry = 0;  

/*
  Function: lookup
  - Searches for a symbol in the symbol table by its lexeme.
  Parameters:
    s - A pointer to the string (lexeme) to look up.
  Returns:
    A pointer to the `Symbol` if found; otherwise, `NULL`.
  Description:
    - Iterates through the symbol table from the beginning to `lastEntry`.
    - Compares each symbol's `lexptr` with the provided string `s`.
    - If a match is found, returns a pointer to the corresponding `Symbol`.
    - If no match is found, returns `NULL`.
 */
Symbol *lookup(const char *s)
{
    int p;                // Loop variable for iterating through the symbol table
    Symbol *rc = NULL;    // Pointer to store the result (initially NULL)
	
    for (p = 0; p < lastEntry; p++) {
        // Compare the lexeme of the current entry with the input string
        if (strcmp(symTable[p].lexptr, s) == 0) {
            rc = &symTable[p];  // Found a match; store the pointer to the symbol
        }
    }
    return rc;  // Return the result (either a valid symbol pointer or NULL)
}

/*
  Function: insert
  - Inserts a new symbol into the symbol table.
  Parameters:
    s   - A pointer to the string (lexeme) representing the symbol.
    tok - The token associated with the symbol.
  Returns:
    A pointer to the newly inserted `Symbol`.
  Description:
    - Checks if the symbol table has reached its capacity (`SYMMAX`).
    - Checks if there is enough space left in the string buffer (`STRMAX`) to store the new lexeme.
    - Allocates memory for the lexeme and copies the string `s` into the new symbol's `lexptr`.
    - Updates the token and increments the global counters (`charCnt` and `lastEntry`).
 */
Symbol *insert(const char *s, int tok)
{
    int lexLen = strlen(s) + 1;  // Length of the lexeme plus one for the null terminator

    // Check if the symbol table is full
    if (lastEntry + 1 == SYMMAX) {
        error("Symbol table full");  // Handle the error when the symbol table is full
    }
    // Check if there is enough space in the string buffer
    else if (charCnt + lexLen >= STRMAX) {
        error("Too many ID characters");  // Handle the error when the string buffer is full
    }
    // Store the token in the current symbol table entry
    symTable[lastEntry].token = tok;
	
    // Allocate memory for the lexeme and copy the string `s` into the new symbol's `lexptr`
    symTable[lastEntry].lexptr = (char *)malloc(lexLen);
    strcpy((char *)symTable[lastEntry].lexptr, s);
	
    // Update the global character counter
    charCnt += lexLen;

    // Increment the `lastEntry` to point to the next available slot in the symbol table
    lastEntry++;

    // Return a pointer to the newly inserted symbol
    return &symTable[lastEntry - 1];
}
