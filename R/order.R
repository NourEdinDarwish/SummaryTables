#'Track Variable Insertion Order Across Multiple Listboxes
#'
#'Preserves the historical cross-listbox insertion order using a type-slot
#'pattern. The state records which listbox (type) each variable came from.
#'Within-listbox reordering is respected: variables fill their type's slots in
#'the current listbox order. New variables are appended at the end.
#'
#'@param savedState List with `vars` (character) and `types` (integer) from a
#'  previous call, or NULL on first run. Each type index corresponds to the
#'  position of its listbox in `...`.
#'@param ... Character vectors of current listbox contents, in a fixed order
#'  (e.g., categorical first, continuous second). The position determines the
#'  type index stored in `types`.
#'@return List with `vars` (ordered variable names) and `types` (matching type
#'  indices).
trackVariableOrder <- function(savedState, ...) {
  currentLists <- list(...)

  if (is.null(savedState)) {
    savedState <- list(vars = character(), types = integer())
  }

  orderedVars <- character()
  orderedTypes <- integer()

  for (i in seq_along(savedState$types)) {
    listIndex <- savedState$types[i]
    if (length(currentLists[[listIndex]]) > 0) {
      orderedVars <- c(orderedVars, currentLists[[listIndex]][1])
      orderedTypes <- c(orderedTypes, listIndex)
      currentLists[[listIndex]] <- currentLists[[listIndex]][-1]
    }
  }

  # Append remaining new variables (not in previous state)
  remainingVars <- unlist(currentLists)
  remainingListIndices <- rep(seq_along(currentLists), lengths(currentLists))
  orderedVars <- c(orderedVars, remainingVars)
  orderedTypes <- c(orderedTypes, remainingListIndices)

  list(vars = orderedVars, types = orderedTypes)
}
