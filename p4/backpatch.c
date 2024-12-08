#include "global.h"

/* TO BE COMPLETED: implement backpatch operations on lists */

void backpatchlist(Backpatchlist *list, int target) {
    Backpatchlist *current = list;
    
    while (current != NULL) {
        // Calculate the relative offset
        int offset = target - current->loc;
        
        // Apply the backpatch operation with the calculated offset
        if (backpatch(current->loc, offset) != 0) {
            error("Backpatch failed");
            return;
        }
        
        // Move to the next node in the list
        current = current->next;
    }
}
