
# Count Each Variables ----------------------------------------------------


#' Count Each Variables to List of Data Frames
#'
#' Count all or many variables in a data.frame (depth 1 level) and display the results as list of data.frame.
#' This function intended for interactive use.
#'
#' @param x A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param ... Variables to count (one level); If not supply, count all variables.
#' @param sort If `TRUE`, will show the largest groups at the top.
#' @param name The name of the new column in the output.
#' @param prop If `TRUE` calculate proprotion for each count.
#'
#' @return A named list of data.frame
#' @export
#'
#' @examples
#' # Count All variables
#' count_vars(iris)
#' # Count Specified variables
#' count_vars(iris, Species, Sepal.Length)
count_vars <- function(x, ... ,
                       sort = TRUE,
                       name = NULL,
                       prop = FALSE
) {

  dot <- rlang::enexprs(...)

  vars_chr <- if(!rlang::is_empty(dot)){
    # If supply ... as variable
    chr <- purrr::map_chr(dot, rlang::as_string)
    stats::setNames(chr, chr)
  }else{
    # If not supply ... count all varibles in data.frame
    stats::setNames(names(x), names(x))
  }

  ls_counted <- vars_chr %>% purrr::map(
    ~dplyr::count(x, dplyr::across(.x), sort = sort, name = name)
  )

  if(prop){ # Add Proprotion
    n <- ifelse(is.null(name), rlang::expr(n), rlang::ensym(name))
    prop_expr <- rlang::expr((!!n)/sum(!!n))
    ls_counted %>%
      purrr::map(~dplyr::mutate(.x, prop := !!prop_expr))
  }else{
    ls_counted
  }

}


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
    df <- df %>%
      dplyr::mutate(!!var := as.character(!!var)) %>%
      dplyr::rows_insert(
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

# Count variables for Plotting Easier --------------------------------------


#' Count variable for plotting
#'
#' This function count variables, do `fct_reorder` by that count, extract percentage and construct `labels` column
#' ready to be plotted.
#'
#' @param x A data.frame
#' @param ... (Quoted or Unquoted) Variables to count
#' @param var_desc If `TRUE` levels of the variable will be reorder in descending order according to
#'   count of that variable
#' @param label_number Construction of `labels` column in the output
#' * "percent": use `%` (eg. 60%)
#' * "count": count as integer (eg. 6)
#' * "both": use `%` and count in parenthesis  (eg. 60% (6))
#' @param lump_prop Proportion that lump levels to "Other" (passed to `fct_lump` at `prop` argument)
#' @param other_level Name of the "Other" level
#'
#' @return A data.frame with "n", "prop", "pc", "labels" column
#' @export
#'
#' @examples
#' count_vars_plot(iris, Species)
count_vars_plot <- function(x,
                            ...,
                            var_desc = TRUE,
                            label_number = c("percent","count","both"),
                            lump_prop = 0,
                            other_level = "Other"
) {

  label_number <- match.arg(label_number)
  vars <- dplyr::ensyms(..., .named = TRUE)

  labels_expr <- switch (label_number,
                         "percent" = { dplyr::expr(pc) },
                         "count" = {dplyr::expr(n)},
                         "both" = {dplyr::expr(glue::glue("{pc} ({n})"))}
  )
  ## Lump Factor
  if (lump_prop != 0) {
    x <- x %>%
      dplyr::mutate(
        dplyr::across(c(!!!vars), ~forcats::fct_lump(.x, prop = lump_prop,
                                                     other_level = other_level)))

  }

  x %>%
    dplyr::count(!!!vars, sort = TRUE) %>%
    dplyr::mutate(prop = n/sum(n),
                  percent = prop*100,
                  pc = paste0(round(percent, 1),"%"),
                  labels = !!labels_expr) %>%
    dplyr::mutate(dplyr::across(c(!!!vars), ~forcats::fct_reorder(.x, prop, .desc = var_desc)))
}



# Count Variables to Plot Bar Graph with Facet ----------------------------



#' Count Variables to Plot Bar Graph with Facet
#'
#' A Helper that count variable in each group of `var_facet` do extract percentage and construct `labels` column
#' ready to be plotted.
#'
#' @param x A data.frame
#' @param var (Quoted or Unquoted) Variable to count
#' @param var_facet (Quoted or Unquoted) Variable intended to get facet wrapped (grouping variable)
#' @param label_number Construction of `labels` column in the output
#' * "percent": use `%` (eg. 60%)
#' * "count": count as integer (eg. 6)
#' * "both": use `%` and count in parenthesis  (eg. 60% (6))
#'
#' @return A data.frame
#' @export
#'
#' @examples
#' count_vars_plot_facet(mtcars, cyl, gear)
count_vars_plot_facet <- function(x,
                                  var,
                                  var_facet,
                                  label_number = c("percent","count","both")
) {

  label_number <- match.arg(label_number)
  var <- dplyr::ensym(var)
  var_facet <- dplyr::ensym(var_facet)

  labels_expr <- switch (label_number,
                         "percent" = { dplyr::expr(pc) },
                         "count" = {dplyr::expr(n)},
                         "both" = {dplyr::expr(glue::glue("{pc} ({n})"))}
  )

  x %>%
    dplyr::count(!!var_facet, !!var) %>%
    dplyr::group_by(!!var_facet) %>%
    dplyr::mutate(prop = n/sum(n),
                  percent = prop*100,
                  pc = paste0(round(percent, 1),"%"),
                  labels = !!labels_expr) %>%
    dplyr::ungroup()
}


