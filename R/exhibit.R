#' The exhibit package.
#' 
#' @docType package
#' @name exhibit
#' @import lossdb
#' @import ChainLadder
#' 
#' @details Many packages in R return list data structures that contain
#' a lot more information than is usually needed.  \code{exhibit} returns
#' a summary of this output as a data frame that can be easily displayed as
#' a table in a pdf or html document. 
#' 
NULL


#' Generic similar to summary but always creates a data frame
#' 
#' \code{exhibit} defines certain default ways to arrange 
#' objects returned by the package \code{ChainLadder} package
#' and \code{lossdb} package functions for presentation in a report.  
#' 
#' @param object object to turn into exhibit
#' @param format whether or not to use default `exhibit` format
#' @param ... additional arguments
#' 
 
#' 
#' @export
exhibit <- function(oject, format = TRUE, ...) UseMethod("exhibit")