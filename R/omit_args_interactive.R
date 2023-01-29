#' Omit Arguments Interactively
#'
#' This function allows the user to interactively select one or more arguments
#' of a given function by letter.
#'
#' @param func A function.
#' @return
#' A character vector with the selected argument names.
#' If no arguments are selected or the function has no arguments, returns `NULL`.
#'
#' @examples
#'
#' my_function <- function(arg1, arg2, arg3) {
#'   return(c(arg1, arg2, arg3))
#' }
#'
#' omit_args_interactive(my_function)
#'
#' # Output:
#' # a. arg1
#' # b. arg2
#' # c. arg3
#' # Choose one or more arguments to omit.
#' # Enter the argument letter(s) or none:
#' # a
#' # [1] "arg1"
#'
#' omit_args_interactive(function(a, b) {})
#'
#' # Output:
#' # a. a
#' # b. b
#' # Choose one or more arguments to omit.
#' # Enter the argument letter(s) or none:
#' # ab
#' # [1] "a" "b"
#'
#' omit_args_interactive(function() {})
#'
#' # Output:
#' # [1] NULL
#' @export
omit_args_interactive <- function(func) {

  # Get the arguments of the input function
  args <- formals(func)

  # Remove the "..." argument from the list of arguments
  args <- args[names(args) != "..."]

  # If the function has no arguments, return NULL
  if (length(args) == 0) return(NULL)

  # Assign each argument in the list a lowercase letter
  arg_letters <- letters[seq_len(length(args))]


  # Print the arguments with letters
  for (i in seq_along(args)) {
    cat(sprintf("%s. %s\n", arg_letters[i], names(args)[i]))
  }

  message("Choose one or more arguments to omit.")

  # Prompt the user to select one or more arguments by letter
  user_input <- readline("Enter the argument letter(s) or none: \n")
  if (user_input == "") return(NULL)

  # Split the user input into separate numbers
  selected_arg_letters <- as.character(unlist(
    stringr::str_extract_all(string = user_input, pattern = "[a-z]{1}")
  ))

  # Check if any of the letters in are not in the possible arg_letters
  invalid_letters <- setdiff(selected_arg_letters, arg_letters)
  if (length(invalid_letters) > 0) {
    invalid_letters <- paste(invalid_letters, collapse = ", ")
    stop(sprintf("Invalid letter(s) entered: %s", invalid_letters))
  }

  # Return the selected argument names
  return(names(args)[match(selected_arg_letters, arg_letters)])
}
