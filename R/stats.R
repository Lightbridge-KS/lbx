#' Compute the Mode of a Numeric Vector
#'
#' This function calculates the mode (the value with the highest frequency) of a numeric vector.
#' It supports handling of missing values through the `na.rm` argument.
#'
#' @param x A numeric vector for which the mode is to be calculated.
#' @param na.rm A logical value indicating whether to remove \code{NA} values
#'        before calculating the mode. Default is \code{TRUE}.
#'
#' @return A numeric vector containing the mode(s) of the input vector.
#'         If the input vector is empty or only contains \code{NA} values,
#'         the function returns \code{NA}.
#'         If there are multiple modes (a tie), the function returns all modes as a vector.
#'
#' @examples
#' # Basic usage
#' compute_mode(c(1, 2, 2, 3, 4, 4, 4, 5))
#'
#' # Handle missing values
#' compute_mode(c(1, 2, 2, NA, 3, 4, 4, 4, 5, NA))
#'
#' # Include \code{NA} values in mode calculation
#' compute_mode(c(1, 2, 2, NA, 3, 4, 4, 4, 5, NA), na.rm = FALSE)
#'
#' # Edge cases
#' compute_mode(numeric(0))  # Returns NA for empty vector
#' compute_mode(c(NA, NA, NA), na.rm = TRUE)  # Returns NA for all NAs
#'
#' @export
compute_mode <- function(x, na.rm = TRUE) {
  # Handle NA values based on na.rm
  if (na.rm) {
    x <- stats::na.omit(x)  # Remove NA values
  }

  if (length(x) == 0) {
    return(NA)  # Return NA if the vector is empty after removing NAs
  }

  # Create a table of counts
  freq_table <- table(x)

  # Identify the value(s) with the maximum frequency
  mode_values <- as.numeric(names(freq_table[freq_table == max(freq_table)]))

  return(mode_values)
}
