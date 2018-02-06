#' Is a character column really numeric?
#'
#' This function does a brute force test to see whether a column of a data frame
#' that is of class `character` is really a column of `numeric` values. This
#' function can be used with calls to `dplyr::mutate_if` and
#' `dplyr::summarize_if`.
#'
#' @param x A generic vector
#'
#' @return A boolean whether the vector is numeric or not
#' @export
#'
#' @examples
#'
is_char_num <- function(x){
  suppressWarnings(all(!is.na(as.numeric(x))))
}
