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


#' Calculate Completeness of Data
#'
#' Calculate proportion of completeness (no `NA`) of the element in the data frame or list
#'
#' @param x list or data.frame
#' @param negate `TRUE` to return proportion of missing data
#'
#' @return A vector of proportion of completeness (not missing)
#' @export
#' @examples
#' df <- data.frame(x = c(1, NA, 2),
#'                  y = c(NA, "a", "b"),
#'                  z = 1:3)
#'
#' complete_rate(df)
complete_rate <- function(x,
                          negate = FALSE
) {
  stopifnot(is.list(x))
  res <- vapply(x, function(x) mean(!is.na(x)), numeric(1))
  if(negate) return(1 - res)
  res
}
