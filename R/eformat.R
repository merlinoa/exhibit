#' Default formats for the exhibit function
#' 
#' \code{eformat} defines certain default ways to format
#' objects created by functions in the exhibit function.
#' 
#' @param object output of exhibit function to format
#' 
#' @export
eformat <- function(object) UseMethod("eformat")

#'@method eformat exhibit_ata
#'@export
eformat.exhibit_ata <- function(object) {
  format(object, digits = 3, nsmall = 3)
}


#'@method eformat exhibit_triangle
#'@export
eformat.exhibit_triangle <- function(object) {
  format(round(object, 0), big.mark = ",")
}

#'@method eformat exhibit_glmReserve
#'@export
eformat.exhibit_glmReserve <- function(object) {
  object[, c(1, 3, 4, 5)] <- format(round(object[, c(1, 3, 4, 5)], 0), 
                                    big.mark = ",")
  object[, c(2, 6)] <- format(object[, c(2, 6)], digits = 3, nsmall = 3)
  object
}

#'@method eformat exhibit_BootChainLadder
#'@export
eformat.exhibit_BootChainLadder <- function(object) {
  format(round(object, 0),big.mark = ",")
}

#'@method eformat exhibit_MackChainLadder
#'@export
eformat.exhibit_MackChainLadder <- function(object) {
  object[, c(1, 3:5)] <- format(round(object[, c(1, 3:5)], 0), big.mark = ",")
  object[, 2] <- format(object[, 2, drop = FALSE], nsmall = 3, digits = 3)
  object
}