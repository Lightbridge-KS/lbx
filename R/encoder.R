#' Encode Vector
#'
#' @description Match and Encode vector
#' @param x (vector) Input data to be matched.
#' @param match (vector) Value to be matched against `x`'s element
#' @param encode (vector) Encoding vector same length as `match`
#' @param nomatch_na (Logical) Control behavior of non-matching elements, one of:
#' * \strong{`TRUE`:} (default) return `NA` for non-matching elements.
#' * \strong{`FALSE`:} return original elements of `x`.
#'
#' @return Encoded vector
#' @export
#'
#' @examples
#' encoder(c("a","b","d"), c("a","b","c"), c("A","B","C"))
#' encoder(c("a","b","d"), c("a","b","c"), c("A","B","C"), nomatch_na = FALSE)
encoder <- function(x, # Any vector
                    match,
                    encode = match, # Encode that pair with match
                    nomatch_na = TRUE
) {

  if(length(match) != length(encode)) stop("`match` and `encode` must have same length", call. = F)

  index <- match(x, match)
  encoded_may_NA <- encode[index]

  if(!is.logical(nomatch_na)) stop("`nomatch_na` must be `TRUE` or `FALSE`.")

  out <- if (nomatch_na) {
    # `NA` for non-matching element
    encoded_may_NA
  } else {
    # Return original element in `x` for non-matching element
    encoded_may_NA[is.na(encoded_may_NA)] <- x[is.na(encoded_may_NA)]
    encoded <- encoded_may_NA
    encoded
  }
  out
}
