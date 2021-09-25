#' Fill Missing values from another vector
#'
#' Fill `NA`s values from `x` using values from `fill` that in the same location.
#' Vector `x` and `fill` must have the same length.
#'
#' @param x Vector to fill `NA`
#' @param fill A Vector whose elements will be retrieved to fill in `x`
#'
#' @return A Vector with original element from `x` and filled `NA` from `fill`
#' @export
#' @examples
#'
#' v1 <- c(NA, NA, "a", NA, "b") # Trust v1
#' v2 <- c(NA, "c", "d","k", NA)
#' fill_na(v1, v2)
#'
fill_na <- function(x, fill) {

  if(length(x) != length(fill)) stop("`x` and `fill` must have the same length", call. = F)

  from_fill_lgl <- is.na(x) & !is.na(fill) # Select these element from fill
  from_x_lgl <- !from_fill_lgl  # Select these element from x

  z <- vector(class(c(x,fill)), length = length(x))
  z[from_fill_lgl] <- fill[from_fill_lgl]
  z[from_x_lgl] <- x[from_x_lgl]
  z

}
