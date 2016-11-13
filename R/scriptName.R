#' The name of the current R script.
#'
#' The name of the current R script as taken from R's command-line arguments,
#' e.g. when the script is run by Rscript. More specifically, the name of the
#' script is extracted from the --file= argument.
#' @seealso \code{\link{commandArgs}}
#' @return a character vector containing the name of the current R script, or
#'  NA if no --file= argument can be found
#' @export
scriptName <- function() {
  re <- "^--file=(.+)$"
  for (arg in commandArgs()) {
    if (regexpr(re, arg) != -1)
      return(gsub(re, arg, replacement="\\1"))
  }
  NA
}
