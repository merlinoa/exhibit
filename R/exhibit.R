#' The exhibit package.
#' 
#' @docType package
#' @name exhibit
#' @import lossdb
#' @import ChainLadder
NULL


#' Generic similar to summary
#' 
#' \code{exhibit} defines certain default ways to arrange 
#' objects returned by the package \code{ChainLadder} package
#' and \code{lossdb} package functions for presentation in a report.  
#' 
#' @param object object to turn into exhibit
#' @param ... additional arguments
#' 
#' @export
exhibit <- function(odject, ...) UseMethod("exhibit")