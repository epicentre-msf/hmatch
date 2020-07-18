test_that("hmatch_tokens works as expected", {

  raw <- data.frame(
    id = 1:2,
    x = c("United States", "China")
  )

  ref <- data.frame(
    x = c("People's Republic of China", "United States of America")
  )

  m <- hmatch_tokens(raw, ref, type = "inner")
  expect_equal(m$id, c(1, 2))
})
