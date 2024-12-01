#' Compute Custom Summary Statistics for One or More Variables
#'
#' @description
#' The `get_custom_summary_stats()` function computes a set of summary statistics
#' (e.g., count, mean, standard deviation, interquartile range, median, skewness, and kurtosis)
#' for one or more variables in a given data frame. Users can optionally specify which statistics
#' to display.
#'
#' @param df A `data.frame` or `tibble`. The input dataset.
#' @param ... One or more variable names (unquoted or quoted) for which the summary statistics
#'   will be computed.
#' @param show A character vector specifying which summary statistics to include in the output.
#'   Possible values are `"n"`, `"mean"`, `"sd"`, `"iqr"`, `"median"`, `"skewness"`,
#'   and `"kurtosis"`. If `NULL` (default), all statistics are included.
#'
#' @details
#' This function uses `dplyr::across()` to apply multiple summary functions dynamically
#' to the specified variables. Summary statistics are computed using base R and
#' the `moments` package for skewness and kurtosis. The resulting summary statistics
#' are named dynamically as `<variable>_<statistic>`.
#'
#' @return
#' A `tibble` containing the computed summary statistics. Each statistic is presented
#' as a column with names formatted as `<variable>_<statistic>`.
#'
#' @examples
#' library(dplyr)
#' library(moments)
#'
#' # Example data
#' df <- tibble::tibble(
#'   Score = c(85, 90, 78, NA, 92),
#'   Age = c(25, 30, 35, 40, 45)
#' )
#'
#' # Compute all summary statistics for `Score` and `Age`
#' get_custom_summary_stats(df, Score, Age)
#'
#' # Compute only mean and standard deviation for `Score`
#' get_custom_summary_stats(df, Score, show = c("mean", "sd"))
#'
#' @export
get_custom_summary_stats <- function(df, ..., show = NULL) {

  vars <- rlang::ensyms(...) # Capture one or more variable names
  vars_str <- purrr::map_chr(vars, rlang::as_string)

  .fns_custom_summary_stats <- list(
    n = ~ sum(!is.na(.)),
    mean = ~ mean(., na.rm = TRUE),
    sd = ~ sd(., na.rm = TRUE),
    iqr = ~ IQR(., na.rm = TRUE),
    median = ~ median(., na.rm = TRUE),
    skewness = ~ moments::skewness(., na.rm = TRUE),
    kurtosis = ~ moments::kurtosis(., na.rm = TRUE)
  )

  if (!is.null(show)) {
    .fns_custom_summary_stats <- .fns_custom_summary_stats[show]
  }

  dplyr::summarise(df, dplyr::across(
    .cols = c(vars_str),
    .fns = .fns_custom_summary_stats,
    .names = "{.col}_{.fn}"
  ))

}


