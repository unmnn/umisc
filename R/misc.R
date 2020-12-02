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


#' Capitalizes all letters in a character string until a separator character
#'
#' Returns original character string if it does not contain the separator
#' character.
#'
#' @param char A character string.
#' @param sep The separator character.
#'
#' @return The modified character string.
#' @export
#'
#' @examples
#' char <- "adsl_depression"
#' capitalize_prefix(char)
capitalize_prefix <- function(char, sep = "_") {
  s <- strsplit(char, "_")
  vapply(s, function(x){
    if(length(x) == 1) {
      x
    } else {
      paste0(toupper(x[1]), "_", x[2])
    }
  },
  FUN.VALUE = character(1),
  USE.NAMES = FALSE
  )
}
