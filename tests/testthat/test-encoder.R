
test_that("encoder() works", {

  x <- c("a","b", "b", "d")
  match <- c("a","b","c")
  encode <- c("A","B","C")

  # No match to `NA`
  expect_equal(encoder(x, match, encode),c("A", "B", "B", NA))
  # No match to original
  expect_equal(encoder(x, match, encode, nomatch_na = FALSE), c("A", "B", "B", "d"))

  # Error for Invalid `nomatch_na`
  expect_error(encoder(x, match, encode, nomatch_na = "Something"), "`nomatch_na`")
  expect_error(encoder(x, match, encode, nomatch_na = 1), "`nomatch_na`")

})
