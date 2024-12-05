
#' @title Complete Experimental Design
#' @description This function generates a complete factorial experimental design.
#' @param varlist A list where each element represents a factor.
#'   Each element can be:
#'   - A positive integer (representing the number of levels for a factor).
#'   - A vector (representing the specific levels of a factor).
#'   - A data frame (representing a complex factor with multiple columns).
#' @param numbering Logical, whether to include experiment numbers (default = TRUE).
#' @param size Integer, representing the number of repetitions for each combination (default = 1).
#' @return A data frame containing the full factorial design.
#' @examples
#' complete.design(list(f1 = 3, f2 = c("a", "b"), f3 = 4))
#' complete.design(list(f1 = 3, f2 = c("a", "b"),
#'   f3 = data.frame(fa = c(0.01, 0.05, 0.1), fb = c("x", "x", "y"))), size = 2)
#' @export
#' Suggests: knitr, rmarkdown
#‘ VignetteBuilder: knitr
