#' Get the mode of a vector.
#'
#' The mode of a vector is the value that appears most often.
#'
#' @param x A vector.
#'
#' @return The mode of \code{x}.
#' @export
#'
#' @examples
#' mode(c("D", "A", "B", "B", "C"))
#'
#'
#' # In case of 2 or more modi, the function returns the modus that appears
#' # first:
#' mode(c(4, 9, 1, 2, 9, 1))
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
