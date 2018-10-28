#' @title Plotting a circle
#' @description Plotting a circle
#' @param center A \code{vector} of dimension 2 used to denote the center
#' of the circle (default: \code{center = c(0,0)}).
#' @param radius A \code{numeric} (positive) value used to indicate the radius of the
#' circle (default: \code{raduis = 1}).
#' @param nb_step A \code{numeric} value used the number of points used to
#' construct the circle (default: \code{nb_step = 300}).
#' @param col A \code{numeric} or \code{string} used to denote the color of
#'  the border of the circle (default: \code{col = "darkblue"})
#' @param fill A \code{numeric} or \code{string} used to denote the number
#' the color for filling the circle. The default, \code{NULL}, is to leave
#' circle unfilled.
#' @param lty A \code{numeric} value used the denote the line type used for
#' the border of the circle (default: \code{lty = 1}). See \code{?par} for
#' more details.
#' @author Stephane Guerrier
#' @export
#' @examples
#' # Example 1
#' plot(NA, xlim = c(-1.1,1.1), ylim = c(-1.1,1.1), xlab = "x", ylab = "y")
#' grid()
#' make_square(col = "blue")
#' make_circle(col = "red", lty = 2)
#'
#' # Example 2
#' cols = hcl(h = seq(15, 375, length = 4), l = 65, c = 100)[1:3]
#' cols_trans = hcl(h = seq(15, 375, length = 4), l = 65, c = 100, alpha = 0.15)[1:3]
#' plot(NA, xlim = c(-310, 310), ylim = c(-310, 310), xlab = "x", ylab = "y")
#' grid()
#' make_circle(fill = "lightgrey", radius = 200, lty = 1)
#' make_circle(radius = 20, center = c(-300, 300), col = cols[1], fill = cols[1])
#' make_circle(radius = 20, center = c(300, 300), col =cols[2], fill = cols[2])
#' make_circle(radius = 20, center = c(0, -300), col = cols[3], fill = cols[3])
#' make_circle(radius = 449.2136, center = c(-300, 300),
#'             col = cols[1], fill = cols_trans[1], lty = 2)
#' make_circle(radius = 284.8427, center = c(300, 300),
#'             col = cols[2], fill = cols_trans[2], lty = 2)
#' make_circle(radius = 414.3106, center = c(0, -300),
#'             col = cols[3], fill = cols_trans[3], lty = 2)
make_circle = function(center = c(0,0), radius = 1,
                       nb_step = 300, col = "darkblue",
                       fill = NULL, lty = 1){
  if (!(radius > 0) || is.na(radius) || is.null(radius)){
    stop("The parameter radius must be positive.")
  }

  if (!(nb_step > 10) || is.na(nb_step) || is.null(nb_step)){
    stop("The parameter nb_step must be larger than 10.")
  }

  if (!(length(center) == 2) || is.na(center) || is.null(center)){
    stop("The parameter center must a vector of lenght 2.")
  }

  if (is.na(col) || is.null(col)){
    stop("The parameter col cannot be NA or NULL.")
  }

  if (!is.numeric(lty) || lty <= 0 || is.na(col) || is.null(col)){
    stop("The parameter lty must be an integer positive value.")
  }

  my_points = seq(from = 0, to = 2*pi, length.out = nb_step)
  coords = cbind(radius*cos(my_points) + center[1], radius*sin(my_points) + center[2])
  lines(coords, col = col, lty = lty)
  if (!is.null(fill)){
    polygon(c(coords[,1]), c(coords[,2]), col = fill, border = NA)
  }
}




#' @title Plotting a square
#' @description Plotting a square
#' @param center A \code{vector} of dimension 2 used to denote the center
#' of the square (default: \code{center = c(0,0)}).
#' @param side A \code{numeric} (positive) value used to indicate the length of one
#' side of the square (default: \code{side = 2}).
#' @param col A \code{numeric} or \code{string} used to denote the color of
#'  the border of the square (default: \code{col = "darkblue"})
#' @param fill A \code{numeric} or \code{string} used to denote the number
#' the color for filling the square The default, \code{NULL}, is to leave
#' circle unfilled.
#' @param lty A \code{numeric} value used the denote the line type used for
#' the border of the square (default: \code{lty = 1}). See \code{?par} for
#' more details.
#' @author Stephane Guerrier
#' @export
#' @examples
#' cols = hcl(h = seq(15, 375, length = 4), l = 65, c = 100)[1:3]
#' cols_trans = hcl(h = seq(15, 375, length = 4), l = 65, c = 100, alpha = 0.15)[1:3]
#' plot(NA, xlim = c(-110, 110), ylim = c(-110, 110), xlab = "x", ylab = "y")
#' grid()
#' make_square(center = c(-50,50), side = 100, col = cols[1], lty = 4)
#' make_square(center = c(0,0), side = 100, col = cols[2], fill = cols_trans[2], lty = 2)
#' make_square(center = c(50,-50), side = 100, col = cols_trans[3], fill = cols_trans[3])
#' make_square(center = c(50,-50), side = 100, col = cols[1], fill = cols_trans[3])
#' make_square(center = c(0,0), side = 150, col = cols_trans[1], fill = cols_trans[1])
make_square = function(center = c(0,0), side = 2, col = "darkblue",
                       fill = NULL, lty = 1){

  if (!(side > 0) || is.na(side) || is.null(side)){
    stop("The parameter side must be positive.")
  }

  if (!(length(center) == 2) || is.na(center) || is.null(center)){
    stop("The parameter center must a vector of lenght 2.")
  }

  if (is.na(col) || is.null(col)){
    stop("The parameter col cannot be NA or NULL.")
  }

  if (!is.numeric(lty) || lty <= 0 || is.na(col) || is.null(col)){
    stop("The parameter lty must be an integer positive value.")
  }

  bottom_left = center - side/2
  lines(c(bottom_left[1], bottom_left[1]), c(bottom_left[2] + side, bottom_left[2]),
        col = col, lty = lty)
  lines(c(bottom_left[1] + side, bottom_left[1]), c(bottom_left[2] + side, bottom_left[2] + side),
        col = col, lty = lty)
  lines(c(bottom_left[1] + side, bottom_left[1] + side), c(bottom_left[2], bottom_left[2] + side),
        col = col, lty = lty)
  lines(c(bottom_left[1], bottom_left[1] + side), c(bottom_left[2], bottom_left[2]),
        col = col, lty = lty)
  if (!is.null(fill)){
    polygon(c(bottom_left[1], bottom_left[1]+side, bottom_left[1]+side, bottom_left[1]),
            c(bottom_left[2], bottom_left[2], bottom_left[2]+side, bottom_left[2]+side),
            border = NA, col = fill)
  }
}
