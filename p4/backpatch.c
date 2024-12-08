/* Price, Chris, Gorana, Lian
GCSC 554
Project: P4
File: Backpatch.c
*/

#include "global.h"

/* 
 * Creates a new node in a backpatch list with a specified location.
 *
 * Parameters:
 *   loc - The location to be stored in the new backpatch list node.
 *
 * Returns:
 *   A pointer to the newly created `Backpatchlist` node.
 *
 * Description:
 *   - Allocates memory for a new `Backpatchlist` node.
 *   - Initializes the `loc` field with the provided location.
 *   - Sets the `next` pointer to `NULL` to mark the end of the list.
 *   - Returns a pointer to the newly allocated node.
 */
Backpatchlist *makelist(unsigned loc) {
  Backpatchlist *bpl = (Backpatchlist*)malloc(sizeof(Backpatchlist)); // Allocate memory for a new node
  bpl->next = NULL;  // Initialize the next pointer to NULL
  bpl->loc = loc;    // Set the location field with the provided loc value
  return bpl;        // Return the pointer to the newly created node
}


/* 
 * Applies backpatching to a list of backpatch locations.
 *
 * Parameters:
 *   list - A pointer to the head of the `Backpatchlist`.
 *   loc  - The target location to which each entry in the list should be backpatched.
 *
 * Description:
 *   - Iterates through the linked list of backpatch nodes.
 *   - For each node, calculates the relative offset by subtracting the node's location (`temp->loc`) from `loc`.
 *   - Calls the `backpatch` function to apply the backpatch operation using the node's location and the calculated offset.
 *   - Moves to the next node until the end of the list is reached.
 */
void backpatchlist(Backpatchlist *list, int loc) {
  Backpatchlist *temp = list;  // Start at the head of the list
  while (temp) {
    // Perform backpatching: set the target location relative to the node's location
    backpatch(temp->loc, loc - temp->loc);
    temp = temp->next;  // Move to the next node in the list
  }
}

/* 
 * Merges two backpatch lists into a single list.
 *
 * Parameters:
 *   list1 - A pointer to the head of the first `Backpatchlist`.
 *   list2 - A pointer to the head of the second `Backpatchlist`.
 *
 * Returns:
 *   A pointer to the head of the merged `Backpatchlist`.
 *
 * Description:
 *   - If `list1` is not `NULL`, appends `list2` to the end of `list1`:
 *     - Iterates through `list1` to find the last node.
 *     - Sets the `next` pointer of the last node to `list2`.
 *     - Returns the head of `list1`.
 *   - If `list1` is `NULL`, returns `list2` as the head of the merged list.
 */
Backpatchlist *mergelist(Backpatchlist *list1, Backpatchlist *list2) {
  Backpatchlist *mergeable = list2;  // Default return value is list2 if list1 is NULL
  if (list1 != NULL) {
    mergeable = list1;  // Start merging from list1
    // Iterate to the end of list1 to find the last node
    while (mergeable->next) {
      mergeable = mergeable->next;
    }
    // Append list2 to the end of list1
    mergeable->next = list2;
  }

  return mergeable;  // Return the head of the merged list
}
