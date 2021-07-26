

# Plot Pie Chart ----------------------------------------------------------

#' Plot Pie Chart
#'
#' High level constructor of pie chart using `ggplot2` to display count of 1 variable.
#'
#' @param data A data.frame
#' @param var (Quote or Unquote) Categorical variables to plot
#' @param lump_prop Proportion that lump levels to "Other" (passed to `fct_lump` at `prop` argument)
#' @param other_level Name of the "Other" level
#' @param polar_start The `start` arg of the `coord_polar()`
#' @param polar_direction The `direction` arg of the `coord_polar()`: 1 for clockwise rotation from high-to-low count.
#' @param polar_clip The `clip` arg of the `coord_polar()`
#' @param label_geom (A Character) Indicate function of label geom
#' * "label": `geom_label`
#' * "label_repel": `ggrepel::geom_label_repel`
#' * "text": `geom_text`
#' * "text_repel": `ggrepel::geom_text_repel`
#' @param label_number (A Character) How to display the label?
#' * "percent": use `%` (eg. 60%)
#' * "count": count as integer (eg. 6)
#' * "both": use `%` and count in parenthesis  (eg. 60% (6))
#' @param color_label (A Character) Color for label
#' @param ... Passed to selected function of `label_geom`
#'
#' @return  A Plot class `gg` and `ggplot`
#' @export
#'
#' @examples
#' plot_pie(mtcars, cyl)
plot_pie <- function(data = NULL,
                     var,
                     lump_prop = 0,
                     other_level = "Other",
                     # Pie
                     polar_start = 0,
                     polar_direction = 1,
                     polar_clip = "on",
                     # Label
                     label_geom = c("label","label_repel", "text", "text_repel"),
                     label_number = c("percent","count","both"),
                     color_label = "black",
                     ... # To geom_fun
) {

  label_geom <- match.arg(label_geom)
  label_number <- match.arg(label_number)
  var <- dplyr::ensym(var)

  # If variable is not factor
  if(!is.factor(dplyr::pull(data, !!var))){
    data <- data %>% dplyr::mutate(!!var := factor(!!var))
  }

  geom_fun <- switch (label_geom,
                      "label" = { ggplot2::geom_label },
                      "label_repel" = { ggrepel::geom_label_repel },
                      "text" = { ggplot2::geom_text },
                      "text_repel" = { ggrepel::geom_text_repel}
  )

  data2 <- data %>% count_vars_plot(!!var,
                                    var_desc = FALSE,
                                    label_number = label_number,
                                    lump_prop = lump_prop,
                                    other_level = other_level)

  data2 %>%
    ggplot2::ggplot(ggplot2::aes(x = "", y = prop, fill = !!var)) +
    # Bargraph
    ggplot2::geom_col() +
    # Wrap to Pie
    ggplot2::coord_polar(theta = "y",
                         start = polar_start,
                         direction = polar_direction,
                         clip = polar_clip) +
    # Lable Percent
    geom_fun(
      ggplot2::aes(label = labels),
      position = ggplot2::position_stack(vjust = 0.5),
      color = color_label,
      show.legend = FALSE,
      ...
    ) +
    ggplot2::theme_void()

}

# Plot Simple Bar graph ----------------------------------------------------


#' Plot Counted Bar Graph
#'
#' Plot counted of 1 variable using bar graph
#'
#' @param data A data.frame
#' @param var (Quote or Unquote) Categorical variables to plot
#' @param var_desc If `TRUE` levels of the variable will be reorder in descending order according to
#'   count of that variable
#' @param lump_prop Proportion that lump levels to "Other" (passed to `fct_lump` at `prop` argument)
#' @param other_level Name of the "Other" level
#' @param alpha An alpha of `geom_col`
#' @param stat (A Character) How y-axis computed ?
#' * "percent": as percentage with `%`
#' * "prop": as proportion
#' * "count": as count
#' @param label_geom (A Character) Indicate function of label geom
#' * "label": `geom_label`
#' * "label_repel": `ggrepel::geom_label_repel`
#' * "text": `geom_text`
#' * "text_repel": `ggrepel::geom_text_repel`
#' @param label_number (A Character) How to display the label?
#' * "percent": use `%` (eg. 60%)
#' * "count": count as integer (eg. 6)
#' * "both": use `%` and count in parenthesis  (eg. 60% (6))
#' @param ... Passed to selected function of `label_geom`
#'
#' @return  A Plot class `gg` and `ggplot`
#' @export
#'
#' @examples
#' plot_bar(mtcars, cyl)
plot_bar <- function(data,
                     var,
                     var_desc = TRUE,
                     lump_prop = 0,
                     other_level = "Other",
                     # geom_col
                     alpha = 0.8,
                     # Label
                     stat = c("percent","prop", "count"),
                     label_geom = c("label","label_repel", "text", "text_repel"),
                     label_number = c("percent","count","both"),
                     ... # Passed to geom_fun
) {

  stat <- match.arg(stat)
  label_number <- match.arg(label_number)
  label_geom <- match.arg(label_geom)

  var <- dplyr::ensym(var)
  # If variable is not factor
  if(!is.factor(dplyr::pull(data, !!var))){
    data <- data %>% dplyr::mutate(!!var := factor(!!var))
  }
  # Stats of y-axis
  y_expr <- switch (stat,
                    "percent" = { dplyr::expr(percent) },
                    "prop" = { dplyr::expr(prop) },
                    "count" = { dplyr::expr(n) }
  )
  # Label of y-axis
  y_labels_fun <- switch (stat,
                          "percent" = { function(x) paste0(x, "%") },
                          "prop" = {function(x) x },
                          "count" = {function(x) x}
  )

  geom_fun <- switch (label_geom,
                      "label" = { ggplot2::geom_label },
                      "label_repel" = { ggrepel::geom_label_repel },
                      "text" = { ggplot2::geom_text },
                      "text_repel" = { ggrepel::geom_text_repel}
  )

  data2 <- data %>% count_vars_plot(!!var,
                                    var_desc = var_desc,
                                    label_number = label_number,
                                    lump_prop = lump_prop,
                                    other_level = other_level)
  data2 %>%
    ggplot2::ggplot(ggplot2::aes(!!var, !!y_expr, fill = !!var)) +
    ggplot2::geom_col(ggplot2::aes(color = !!var), show.legend = FALSE, alpha = alpha) +
    geom_fun(ggplot2::aes(label = labels),
             show.legend = FALSE,
             ...
    ) +
    ggplot2::scale_y_continuous(labels = y_labels_fun)

}


# Plot bar graph with facet wrap ------------------------------------------

#' Plot Bar Graph with Facet Wrap
#'
#' Plot count of 2 variables (2D) into bar graph with facet wrap.
#'
#' @param data A data.frame
#' @param var (Quoted or Unquoted) Variable to count in each facet
#' @param var_facet (Quoted or Unquoted) Variable intended to get facet wrapped (grouping variable)
#' @param stat (A Character) How y-axis computed ?
#' * "percent": as percentage with `%`
#' * "prop": as proportion
#' * "count": as count
#' @param label_geom (A Character) Indicate function of label geom
#' * "label": `geom_label`
#' * "label_repel": `ggrepel::geom_label_repel`
#' * "text": `geom_text`
#' * "text_repel": `ggrepel::geom_text_repel`
#' @param label_number (A Character) How to display the label?
#' * "percent": use `%` (eg. 60%)
#' * "count": count as integer (eg. 6)
#' * "both": use `%` and count in parenthesis  (eg. 60% (6))
#' @param ... Passed to selected function of `label_geom`
#'
#' @return  A Plot class `gg` and `ggplot`
#' @export
#'
#' @examples
#' plot_bars_facet2D(mtcars, cyl, gear, color = "white")
plot_bars_facet2D <- function(data,
                              var,
                              var_facet,
                              # Label
                              stat = c("percent","prop", "count"),
                              label_geom = c("label","label_repel", "text", "text_repel"),
                              label_number = c("percent", "count", "both"),
                              ... # To label_geom
) {
  var <- dplyr::ensym(var)
  var_facet <- dplyr::ensym(var_facet)
  stat <- match.arg(stat)
  label_number <- match.arg(label_number)
  label_geom <- match.arg(label_geom)

  # Stats of y-axis
  y_expr <- switch (stat,
                    "percent" = { dplyr::expr(percent) },
                    "prop" = { dplyr::expr(prop) },
                    "count" = { dplyr::expr(n) }
  )
  # Label of y-axis
  y_labels_fun <- switch (stat,
                          "percent" = { function(x) paste0(x, "%") },
                          "prop" = {function(x) x },
                          "count" = {function(x) x}
  )

  geom_fun <- switch (label_geom,
                      "label" = { ggplot2::geom_label },
                      "label_repel" = { ggrepel::geom_label_repel },
                      "text" = { ggplot2::geom_text },
                      "text_repel" = { ggrepel::geom_text_repel}
  )

  data2 <- count_vars_plot_facet(
    data,
    var = !!var,
    var_facet = !!var_facet,
    label_number = label_number
  )

  data2 %>%
    ggplot2::ggplot(ggplot2::aes(!!var, !!y_expr, fill = !!var)) +
    ggplot2::geom_col(ggplot2::aes(color = !!var), alpha = 0.8, show.legend = F) +
    geom_fun(ggplot2::aes(label = labels),
             show.legend = FALSE, ...
    ) +
    ggplot2::scale_y_continuous(labels = y_labels_fun) +
    ggplot2::facet_wrap(dplyr::vars(!!var_facet))
}


