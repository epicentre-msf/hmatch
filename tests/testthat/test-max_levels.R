test_that("max_levels works as expected", {

  x <- data.frame(
    id = 1:5,
    col1 = c("a", "a", NA , "a", NA),
    col2 = c("a", "a", "a", NA , NA),
    col3 = c(NA , "a", "a", NA , NA),
    test = rep(TRUE, 5),
    stringsAsFactors = FALSE
  )


  ml1 <- max_levels(x, pattern = "^col")
  expect_is(ml1, "integer")
  expect_equal(ml1, c(2, 3, 3, 1, 0))

  ml2 <- max_levels(x, by = c("col1", "col2", "col3"))
  expect_equal(ml1, c(2, 3, 3, 1, 0))

  ml3 <- max_levels(x, by = c("col1", "col2", "col3"), type = "name")
  expect_equal(ml3, c("col2", "col3", "col3", "col1", NA))


  # test sort
  xrev <- x[,5:1]

  ml4 <- max_levels(xrev, pattern = "^col")
  expect_equal(ml4, c(3, 3, 2, 3, 0))

  ml5 <- max_levels(xrev, pattern = "^col", sort = TRUE)
  expect_equal(ml5, c(2, 3, 3, 1, 0))


  # test match all columns
  ml6 <- max_levels(x)
  expect_equal(ml6, c(5, 5, 5, 5, 5))

})

