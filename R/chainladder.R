#' Return a triangle for age to age development factors
#' 
#' @param ata object of class ata generated from \code{ChainLadder} package
#' @param selection optional selected development factors
#' @param tail_column optional column for development factor of 
#' most mature age to ultimate
#' 
#' @method exhibit ata
#' 
#' @export
#' @examples
#' tri <- as.triangle(recovery_ldf, origin = "origin", 
#'                    dev = "dev", value = "paid_loss_only")
#' dev_tri <- ata(tri)
#' exhibit(dev_tri, selection = c(1.9, 1.2, 1.13, 1.075, NA))
#' 
#' # with tail factor selected
#' exhibit(dev_tri, selection = c(1.9, 1.2, 1.13, 1.075, 1.1))
exhibit.ata <- function(object, selection = NULL, tail_column = FALSE) {
  
  # extract development table from ata object
  xhbt <- as.data.frame(object[1:nrow(object), 1:ncol(object)])
  
  if (tail_column) {
    # create name for final column
    final_col_name <- paste0(ncol(xhbt) + 1, "-Ult.")
    
    # create final column
    final_col <- data.frame(rep(NA, times = nrow(xhbt)))
    names(final_col) <- final_col_name
    
    # add final column to exhibit
    xhbt <- cbind(xhbt, final_col)
    xhbt <- rbind(xhbt, 
                  smpl = c(attr(object, "smpl"), NA),
                  wtd = c(attr(object, "vwtd"), NA),
                  sel = selection)
  } else {
    xhbt <- rbind(xhbt,
                  smpl = c(attr(object, "smpl")),
                  wtd = c(attr(object, "vwtd")),
                  sel = selection)
  }
  
  # format the values for presentation
  format(round(xhbt, 3), digits = 3, nsmall = 3)
}

#' Returns a cleaner development triangle for use in reports
#' 
#' @param object object of class triangle generated from \code{ChainLadder} package
#' 
#' @method exhibit triangle
#' 
#' @export
#' 
#' @examples
#' tri <- as.triangle(recovery_ldf, origin = "origin", 
#'                    dev = "dev", value = "paid_loss_only")
#'                    
#' exhibit(tri)
exhibit.triangle <- function(object) {
  xhbt <- format(object[1:nrow(object), 1:ncol(object)], big.mark = ",")
  xhbt <- as.data.frame(xhbt)
  names(xhbt) <- attr(object, "dimnames")[[2]]
  xhbt
}


#' Returns glmReserve summary information in a data frame
#'
#' @param object object of class glmReserve generate from \code{ChainLadder} package
#' 
#' @method exhibit glmReserve
#' 
#' @export
#' 
#' @examples
#' tri <- as.triangle(recovery_ldf, origin = "origin", 
#'                    dev = "dev", value = "paid_loss_only")
#'                    
#' glm_object <- glmReserve(tri)
#' 
#' exhibit(glm_object)
exhibit.glmReserve <- function(object) {
  xhbt <- object$summary
  latest_first_ay <- object$Triangle[1, ncol(object$Triangle)]
  first_ay <- c("Latest" = latest_first_ay, 1, "Ultimate" = latest_first_ay, "IBNR" = 0, "S.E." = NA, "CV" = NA)
  
  # create copy of provided totals row
  totals_p <- xhbt[nrow(xhbt), ]
  
  # combine provided summary with most developed accident year.  Total row not included.
  xhbt <- rbind(first_ay, xhbt[1:(nrow(xhbt) - 1), ])
  
  # create totals row that includes data from first accident year
  totals <- c(sum(xhbt[, "Latest"]), sum(xhbt[, "Latest"]) / sum(xhbt[, "Ultimate"]), totals_p[3:6])
  totals <- as.data.frame(totals)
  names(totals) <- names(xhbt)
  xhbt <- rbind(xhbt, totals)

  # format columns
  xhbt[, c(1, 3, 4, 5)] <- format(round(xhbt[, c(1, 3, 4, 5)], 0), big.mark = ",")
  xhbt[, c(2, 6)] <- format(xhbt[, c(2, 6)], nsmall = 3, digits = 3)
  
  # set rownames
  rownames(xhbt) <- c(as.character(as.numeric(rownames(xhbt)[2]) - 1), rownames(xhbt)[2:(length(rownames(xhbt)) - 1)],
                      "totals:")
  xhbt
}