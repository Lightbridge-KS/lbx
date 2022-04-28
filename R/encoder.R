#' Encode Vector
#'
#' @description Match and Encode vector
#' @param x (vector) Input data to be matched.
#' @param match (vector) Value to be matched against `x`'s element
#' @param encode (vector) Encoding vector same length as `match`
#' @param nomatch_ok (Logical) Control behavior of non-matching elements, one of:
#' * \strong{`NA`:} (default) return `NA` for non-matching elements.
#' * \strong{`TRUE`:} return original element of `x`.
#' * \strong{`FALSE`:} throw an error and message which elements in `x` not found in `match`.
#'
#' @return Encoded vector
#' @export
#'
#' @examples
#' encoder(c("a","b","d"), c("a","b","c"), c("A","B","C"))
#' encoder(c("a","b","d"), c("a","b","c"), c("A","B","C"), nomatch_ok = TRUE)
encoder <- function(x, # Any vector
                    match,
                    encode = match, # Encode that pair with match
                    nomatch_ok = NA
) {

  if(length(match) != length(encode)) stop("`match` and `encode` must have same length", call. = F)

  index <- match(x, match)
  encoded_may_NA <- encode[index]

  if(!is.logical(nomatch_ok)) stop("`nomatch_ok` must be one of: `NA`, `TRUE`, `FALSE`.")

  out <- if (is.na(nomatch_ok)) {
    # `NA` for non-matching element
    encoded_may_NA
  } else if(nomatch_ok) {
    # Return original element in `x` for non-matching element
    encoded_may_NA[is.na(encoded_may_NA)] <- x[is.na(encoded_may_NA)]
    encoded <- encoded_may_NA
    encoded
  } else {
    # Throw error and message which element in `x` not found in `match`
    non_match <- unique(x[is.na(encoded_may_NA)])
    on.exit(lapply(non_match, message))
    stop("Element(s) in `x` not found in `match`:", call. = FALSE)
  }

  out
}
