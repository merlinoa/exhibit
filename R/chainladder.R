#' Returns an age to age development triangle matrix
#' 
#' @param ata object of class ata from the \code{ChainLadder} package
#' @param selection Selected development factors.  Should be supplied
#' as a numeric vector of the same length as the number of development periods
#' in the ata development triangle or the number of development periods + 1 
#' (the + 1 will be used as the tail factor).
#' 
#' @method exhibit ata
#' 
#' @import ChainLadder
#' 
#' @export
#' @examples
#' # make sure to load the ChainLadder package with
#' # library(ChainLadder)
#' dev_tri <- ata(RAA)
#' 
#' # No selected development factors
#' exhibit(dev_tri)
#' 
#' # with selected development factors
#' exhibit(dev_tri, selection = c(3.0, 1.65, 1.30, 1.2, 1.12, 1.05, 1.035, 1.015, 1.01))
#' 
#' # with selected development factors includeing tail factor
#' exhibit(dev_tri, selection = c(3.0, 1.65, 1.30, 1.2, 1.12, 1.05, 1.035, 1.015, 1.01, 1.05))
exhibit.ata <- function(object, 
                        selection = NULL) {
  
  # extract development table from ata object
  xhbt <- object[1:nrow(object), 1:ncol(object)]
  
  if (is.null(selection)) {
    xhbt <- rbind(xhbt,
                  Simple = c(attr(object, "smpl")),
                  Weighted = c(attr(object, "vwtd")))
  } else {
    if (length(selection) == (ncol(xhbt) + 1)) {
      # create name for final column
      final_col_name <- paste0(ncol(xhbt) + 1, "-Ult.")
    
      # create final column
      final_col <- rep(NA, times = nrow(xhbt))
    
      # add final column to exhibit
      xhbt <- cbind(xhbt, final_col)
      colnames(xhbt)[ncol(xhbt)] <- final_col_name
    
      xhbt <- rbind(xhbt, 
                    Simple = c(attr(object, "smpl"), NA),
                    Weighted = c(attr(object, "vwtd"), NA),
                    Selected = selection)
    } else if (length(selection) == ncol(xhbt)) {
      xhbt <- rbind(xhbt,
                    Simple = c(attr(object, "smpl")),
                    Weighted = c(attr(object, "vwtd")),
                    Selected = selection)
      } else {
        stop("the `selection` parameter must be a vector of length ncol(`object`) or 
             (ncol(`object`) + 1)")
    }
  }
  
  xhbt
}

#' Returns a development triangle as a matrix
#' 
#' @param object object of class triangle generated from \code{ChainLadder} package
#' 
#' @method exhibit triangle
#' 
#' @import ChainLadder
#' 
#' @export
#' 
#' @examples                    
#' exhibit(RAA)
exhibit.triangle <- function(object) {
  
  # extract relevant data from triangle
  xhbt <- object[1:nrow(object), 1:ncol(object)]
  colnames(xhbt) <- attr(object, "dimnames")[[2]]
  
  xhbt
}


#' Returns glmReserve summary information as a data frame
#'
#' @param object object of class glmReserve generated from \code{ChainLadder} package
#' 
#' @method exhibit glmReserve
#' 
#' @import ChainLadder
#' 
#' @export
#' 
#' @examples                    
#' glm_object <- glmReserve(GenIns)
#' 
#' exhibit(glm_object)
exhibit.glmReserve <- function(object) {
  xhbt <- object$summary
  
  # extract latest development of first origin year
  latest_first_ay <- object$Triangle[1, ncol(object$Triangle)]
  first_ay <- c("Latest" = latest_first_ay, 1, "Ultimate" = latest_first_ay, 
                "IBNR" = 0, "S.E." = NA, "CV" = NA)
  
  # create copy of provided totals row
  totals_p <- xhbt[nrow(xhbt), ]
  
  # combine provided summary with most developed accident year.  Total row not included.
  xhbt <- rbind(first_ay, xhbt[1:(nrow(xhbt) - 1), ])
  
  # create totals row that includes data from first accident year
  totals <- c(sum(xhbt[, "Latest"]), sum(xhbt[, "Latest"]) / sum(xhbt[, "Ultimate"]), 
              sum(xhbt[, "Ultimate"]), totals_p[4:6])
  totals <- as.data.frame(totals)
  names(totals) <- names(xhbt)
  xhbt <- rbind(xhbt, totals)
  
  rownames(xhbt) <- c(as.character(as.numeric(rownames(xhbt)[2]) - 1), 
                      rownames(xhbt)[2:(length(rownames(xhbt)) - 1)],
                      "totals:")
  
  xhbt
}

#' Returns BootChainLadder summary information as a data frame
#'
#' @param object object of class BootChainLadder generated from \code{ChainLadder} package
#' 
#' @import ChainLadder
#' 
#' @method exhibit BootChainLadder
#' 
#' @export
#' 
#' @examples          
#' boot_object <- BootChainLadder(RAA)
#' 
#' exhibit(boot_object)
exhibit.BootChainLadder <- function(object) {
  # use first object in generic summary
  xhbt <- summary(object)[[1]]
  
  # retreive totals
  totals <- as.data.frame(t(summary(object)[[2]]))
  names(totals) <- names(xhbt)
  
  # combine data frames
  xhbt <- rbind(xhbt, totals)
  
  xhbt
}

#' Returns MackChainLadder summary information in a data frame
#'
#' @param object object of class MackChainLadder generated from \code{ChainLadder} package
#' 
#' @import ChainLadder
#' 
#' @method exhibit MackChainLadder
#' 
#' @export
#' 
#' @examples
#' mack_object <- MackChainLadder(RAA)
#' 
#' exhibit(mack_object)
exhibit.MackChainLadder <- function(object) {
  # use first object in generic summary
  xhbt <- summary(object)[[1]][, 1:5]
  
  # retreive totals
  totals <- as.data.frame(t(summary(object)[[2]]))
  totals <- totals[, 1:5]
  names(totals) <- names(xhbt)
  
  # combine data frames
  xhbt <- rbind(xhbt, totals)
  
  xhbt
}