# Count variable to a display table ---------------------------------------


#' Count variable and display to table
#'
#' This function count 1 variable and display the output as count and percentage.
#' Also add "Total" at last row.
#'
#' @param x A data.frame
#' @param var (Quote or Unquoted) Variable to count
#' @param sort Output sorted or not
#' @param name Name of column that store the "Count".
#' @param add_total Add "Total" at last row or not
#'
#' @return A data.frame
#' @export
#'
#' @examples
#' count_var_table(iris, Species)
count_var_table <- function(x,
                            var,
                            sort = TRUE,
                            name = "Count",
                            add_total = TRUE
) {

  var <- dplyr::ensym(var)

  n <- rlang::ensym(name)
  prop_expr <- dplyr::expr(!!n/sum(!!n) )

  df <- x %>%
    dplyr::count(!!var, name = name, sort = sort) %>%
    dplyr::mutate(Prop =!!prop_expr,
                  Percent = scales::percent(Prop)
    )
  ## Compute Summary
  tot_count <- sum(dplyr::pull(df, !!n), na.rm = T)
  tot_pc <- scales::percent(sum(dplyr::pull(df, Prop), na.rm = T))
  ## Add Total at last row
  if (add_total) {
    df <- df %>% dplyr::rows_insert(
      tibble::tibble(
        !!var := "Total",
        !!n := tot_count,
        Percent = tot_pc
      ),
      by = as.character(var)
    )
  }

  df %>%
    dplyr::select(-Prop)

}

