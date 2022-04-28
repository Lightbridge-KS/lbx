
test_that("encoder() works", {

  x <- c("a","b", "b", "d")
  match <- c("a","b","c")
  encode <- c("A","B","C")

  # No match to `NA`
  expect_equal(encoder(x, match, encode),c("A", "B", "B", NA))
  # No match to original
  expect_equal(encoder(x, match, encode, nomatch_ok = TRUE), c("A", "B", "B", "d"))
  # No match throw Error
  expect_error(encoder(x, match, encode, nomatch_ok = FALSE))

  # Error for Invalid `nomatch_ok`
  expect_error(encoder(x, match, encode, nomatch_ok = "Something"), "`nomatch_ok`")
  expect_error(encoder(x, match, encode, nomatch_ok = 1), "`nomatch_ok`")

})
