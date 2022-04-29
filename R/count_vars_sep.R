


# Separate and count variables to list of DF --------------------------------------------

#' Separate and Count Variables to DF
#'
#' Separate texts in a variables using a separator `sep` into rows, trim white space,
#' counts each variables, and return list of counted data frames.
#'
#'
#' @param data A Data Frame
#' @param vars (Character) A Character of variables to count. If `NULL`, count all variables in the data frame.
#' @param sep A separator of texts
#' @param convert If `TRUE` will automatically run [type.convert()] on the key column. This is useful if the column types are actually numeric, integer, or logical.
#' @param sort If `TRUE`, will show the largest groups at the top.
#'
#' @return a named list of data frames (named as counted variable names)
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = 1:3,
#'   y = c("a", "d,e,f", "g,h"),
#'   z = c("1", "2, 3, 4", "5, 6, 7")
#'  )
#' count_vars_sep(df, sep = ",")
count_vars_sep <- function(data,
                           vars = NULL,
                           sep = ",",
                           convert = FALSE,
                           sort = TRUE
) {

  if(is.null(vars)){
    vars <- names(data)
  }

  ls <- purrr::map(vars,
                   ~count_var_sep(data, !!.x, convert = convert, sep = sep)
  )

  names(ls) <- vars
  ls
}

# Separate and count variable to DF --------------------------------------------

#' Separate and Count Variable to DF
#'
#' Separate texts in a variable from `sep` into rows, trim white space,
#' and counts.
#'
#' @param data A Data Frame
#' @param var (data masking) a variable to count
#' @param sep A separator of texts
#' @param convert If `TRUE` will automatically run [type.convert()] on the key column. This is useful if the column types are actually numeric, integer, or logical.
#' @param sort If `TRUE`, will show the largest groups at the top.
#'
#' @return a data frame
#' @noRd
#'
#' @examples
#' df <- data.frame(
#'   x = 1:3,
#'   y = c("a", "d,e,f", "g,h"),
#'   z = c("1", "2, 3, 4", "5, 6, 7")
#'  )
#' count_var_sep(df, z, sep = ",")
count_var_sep <- function(data,
                          var,
                          sep = ",",
                          convert = FALSE,
                          sort = TRUE
) {

  var <- dplyr::ensym(var)

  df_sep <- data %>%
    tidyr::separate_rows(!!var, sep = sep) %>%
    dplyr::mutate(!!var := trimws(!!var))

  if(convert){
    df_sep <- df_sep %>%
      dplyr::mutate(!!var := type.convert(!!var, as.is = TRUE))
  }

  df_sep %>%
    dplyr::count(!!var, sort = sort)

}
