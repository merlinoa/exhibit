#' Return a triangle for age to age development factors
#' 
#' @param ata object of class ata generated from \code{ChainLadder} package
#' @param selection optional selected development factors.  Should be supplied
#' as a numeric vector of the same length as the number of development periods
#' in the ata development triengle or the number of developments + 1 in the ata
#' development triengle, with the + 1 for the tail factor.  The tail factor can be
#' used to project future development for the most mature origin years.
#' 
#' @method exhibit ata
#' 
#' @export
#' @examples
#' tri <- as.triangle(ldf_data, origin = "origin", 
#'                    dev = "dev", value = "paid_loss_only")
#' dev_tri <- ata(tri)
#' exhibit(dev_tri, selection = c(1.9, 1.2, 1.13, 1.075))
#' 
#' # with tail factor selected
#' exhibit(dev_tri, selection = c(1.9, 1.2, 1.13, 1.075, 1.1))
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

#' Returns a cleaner development triangle for use in reports
#' 
#' @param object object of class triangle generated from \code{ChainLadder} package
#' @param eformat logical; whether or not to use default exhibit format
#' 
#' @method exhibit triangle
#' 
#' @export
#' 
#' @examples
#' tri <- as.triangle(ldf_data, origin = "origin", 
#'                    dev = "dev", value = "paid_loss_only")
#'                    
#' exhibit(tri)
exhibit.triangle <- function(object) {
  
  # extract relevant data from triangle
  xhbt <- object[1:nrow(object), 1:ncol(object)]
  names(xhbt) <- attr(object, "dimnames")[[2]]
  
  xhbt
}


#' Returns glmReserve summary information in a data frame
#'
#' @param object object of class glmReserve generated from \code{ChainLadder} package
#' @param eformat logical; whether or not to use default exhibit format
#' 
#' @method exhibit glmReserve
#' 
#' @export
#' 
#' @examples
#' tri <- as.triangle(ldf_data, origin = "origin", 
#'                    dev = "dev", value = "paid_loss_only")
#'                    
#' glm_object <- glmReserve(tri)
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

#' Returns BootChainLadder summary information in a data frame
#'
#' @param object object of class BootChainLadder generated from \code{ChainLadder} package
#' @param eformat logical; whether or not to use default exhibit format
#' 
#' @method exhibit BootChainLadder
#' 
#' @export
#' 
#' @examples
#' tri <- as.triangle(ldf_data, origin = "origin", 
#'                    dev = "dev", value = "paid_loss_only")
#'                    
#' boot_object <- BootChainLadder(tri)
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
#' @param eformat whether or not to use default exhibit format
#' 
#' @method exhibit MackChainLadder
#' 
#' @export
#' 
#' @examples
#' tri <- as.triangle(ldf_data, origin = "origin", 
#'                    dev = "dev", value = "paid_loss_only")
#'                    
#' mack_object <- MackChainLadder(tri)
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