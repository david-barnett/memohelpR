#' Memoise a Function in the Project Directory
#'
#' The function memoise_fun_in_proj takes in a function func and returns a memoised version of the specified function,
#' with the cache written to disk in a specified folder within the project folder.
#'
#' @param func a function that needs to be memoised
#' @param omit_args Names of arguments to ignore when calculating hash
#' @param proj_dir
#' the project directory where the memoised function and its cache will be stored
#' @param memo_dir
#' the subdirectory within the project directory to store the memoised function
#' @param cache_dir
#' the subdirectory within the memo_dir to store the cache
#' @param logfile
#' the file name of the log file to store cache activities (made in memo_dir)
#' @param evict the cache eviction strategy to be used (default: "lru")
#' @param ... additional arguments to be passed to cachem::cache_disk
#'
#' @return a memoised version of the input function,
#' with the cache written to disk in the specified folder.
#'
#' @examples
#' lm_memo <- memoise_fun_in_proj(lm)
#' @export
memoise_fun_in_proj <- function(func,
                                omit_args = NULL,
                                proj_dir = here::here(),
                                memo_dir = "memoise",
                                cache_dir = "cache",
                                logfile = "log.txt",
                                evict = "lru",
                                ...) {

  func_name <- deparse(substitute(func))
  stopifnot(is.function(func))

  # remove package name
  func_name <- gsub(pattern = "^.+[:]{2}", replacement = "", x = func_name)

  # specify memoise function directory
  mem_fun_dir <- file.path(proj_dir, memo_dir, func_name)

  # check if this is probably running for the first time or not
  if (!dir.exists(mem_fun_dir)) {

    # Print the current working directory
    message("Project working directory, from here(): \n", proj_dir, "\n")

    if (!interactive()) stop ("Run this function interactively the 1st time")

    # Ask the user if this is the project directory where they want to continue
    user_choice <- readline(prompt = paste(
      "Is this the project dir where you want to create a memoized copy of",
      func_name, "? (y/n) \n"
    ))

    if (tolower(user_choice) != "y") {
      stop("Exiting. Ensure desired output of here() before calling again.")
    }

  }

  # Set the cache_dir name
  cache_dir <- file.path(mem_fun_dir, "cache")

  # Create the cache_dir
  if (!dir.exists(cache_dir)) {
    message("Creating cache dir at: ./", cache_dir)
    dir.create(cache_dir, recursive = TRUE)
  }

  # create disk cache object
  cache <- cachem::cache_disk(
    dir = cache_dir, logfile = file.path(mem_fun_dir, logfile), evict = evict, ...
  )

  return(memoise::memoise(func, omit_args = omit_args, cache = cache))
}
