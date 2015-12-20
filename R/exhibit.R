#' Generic function for creating tabular data for report output
#' 
#' \code{exhibit} defines certain default ways to arrange 
#' objects returned by \code{ChainLadder} and other
#' packages for presentation in a report.  
#' 
#' @param object object to turn into exhibit
#' @param ... additional arguments
#' 
#' @export
exhibit <- function(object, ...) UseMethod("exhibit")