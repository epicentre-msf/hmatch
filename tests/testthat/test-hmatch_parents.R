test_that("hmatch_parents works as expected", {

  ## basic functionality
  raw <- data.frame(
    x1 = c("g1", "g1", "g1", "g2", "g2", "g2", "g3"),
    x2 = c("a", "b", "c", "d", "e", "f", "g"),
    stringsAsFactors = FALSE
  )

  ref <- data.frame(
    x1 = c("group1", "group1", "group1", "group2", "group2", "group2", "group3"),
    x2 = c("a", "b", "cc", "d", "e", "f", "blah"),
    stringsAsFactors = FALSE
  )

  m1 <- hmatch_parents(
    raw,
    ref,
    level = 1,
    min_matches = 1,
    type = "left"
  )

  expect_equal(m1$x1, c("g1", "g2", "g3"))
  expect_equal(m1$ref_x1, c("group1", "group2", NA))
  expect_equal(m1$n_child_match, c(2, 3, NA))

  m2 <- hmatch_parents(
    raw,
    ref,
    level = 1,
    min_matches = 1,
    type = "inner"
  )

  expect_equal(m2$x1, c("g1", "g2"))
  expect_equal(m2$ref_x1, c("group1", "group2"))
  expect_equal(m2$n_child_match, c(2, 3))


  ## test errors due to level argument not in range
  expect_error(hmatch_parents(raw, ref, level = 0))
  expect_error(hmatch_parents(raw, ref, level = 2))
  expect_error(hmatch_parents(raw, ref, level = "x2"))
  expect_error(hmatch_parents(raw, ref, level = "blah"))
})

