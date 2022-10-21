# @name nin
# @title Not in operator
# @description Checks if x is in y
# @param x a value to check
# @param y a value or vector of values to check against x
# @return Boolean value, true if x is not in y; false otherwise
# Removed rdoxygen2 comments in 0.8.0 to prevent documentation of
# this *internal* function.
"%nin%" <- function(x, y) {!(x %in% y)}

# @name waiver
# @title Used in place of null or empty values to function parameters
# @return An empty data structure of class waiver
waiver <- function() structure(list(), class = "waiver")

# @name is.waive
# @title Checks if a parameter contains the default value, waiver()
# @param x A parameter to the function
# @return True if
is.waive <- function(x) inherits(x, "waiver")
