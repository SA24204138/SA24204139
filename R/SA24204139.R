
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
complete.design <- function(varlist, numbering = TRUE, size = 1) {
  nf <- length(varlist)
  vl <- vector(mode = "list", length = nf)
  tf <- character(nf)
  for (i in seq_along(varlist)) {
    if (length(varlist[[i]]) == 1) {
      if (varlist[[i]] == as.integer(varlist[[i]]) && varlist[[i]] > 0) {
        vl[[i]] <- factor(seq(varlist[[i]]))
        tf[i] <- "factor"
      } else {
        stop("非法输入！")
      }
    } else if (is.data.frame(varlist[[i]])) {
      vl[[i]] <- seq(nrow(varlist[[i]]))
      tf[i] <- "df"
    } else {
      stopifnot(!any(duplicated(varlist[[i]])))
      vl[[i]] <- factor(varlist[[i]])
      tf[i] <- "factor"
    }
  }
  nlev <- vapply(vl, length, 1)
  ne <- prod(nlev) * size
  nr <- rev(cumprod(rev(c(nlev, size))))[-1]
  nc <- ne / nr
  dl <- list()
  for (i in seq_along(vl)) {
    idl <- as.integer(gl(nlev[i], nr[i], ne))
    if (tf[i] == "factor") {
      dl[[i]] <- data.frame(factor(idl, labels = levels(vl[[i]])))
      names(dl[[i]]) <- names(varlist)[[i]]
    } else {
      dl[[i]] <- varlist[[i]][idl, ]
    }
  }
  d <- dl[[1]]
  if (nf > 1) {
    for (i in seq(2, nf)) {
      d <- cbind(d, dl[[i]])
    }
  }
  
  if (numbering) {
    d <- cbind(d, data.frame(expNumber = rep(seq(ne / size), each = size)))
  }
  
  d
}

# Test cases
test1 <- function() {
  complete.design(list(f1 = 3, f2 = c("a", "b"), f3 = 4))
}
print(head(test1()))

test2 <- function() {
  complete.design(list(f1 = 3, f2 = c("a", "b"), f3 = 4), size = 2)
}
print(head(test2()))

test3 <- function() {
  complete.design(list(
    f1 = 3,
    f2 = c("a", "b"),
    f3 = data.frame(
      fa = c(0.01, 0.05, 0.1),
      fb = c("x", "x", "y")
    )
  ), size = 2)
}
print(head(test3()))