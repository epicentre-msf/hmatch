test_that("separate_hcode works as expected", {

  dat1 <- data.frame(
    row = 1:4,
    pcode = c("can", "can__on", NA_character_, "can__sk__moose_jaw")
  )

  x1 <- separate_hcode(dat1, col = "pcode", into = paste0("pcode", 1:3))

  expect_s3_class(x1, "data.frame")
  expect_named(x1, c("row", "pcode", paste0("pcode", 1:3)))
  expect_equal(x1$pcode1, c("can", "can", NA, "can"))
  expect_equal(x1$pcode2, c(NA, "can__on", NA, "can__sk"))
  expect_equal(x1$pcode3, c(NA, NA, NA, "can__sk__moose_jaw"))

  x2 <- separate_hcode(dat1, col = "pcode", into = paste0("pcode", 1:2), extra = "drop")
  expect_named(x2, c("row", "pcode", paste0("pcode", 1:2)))
  expect_equal(x2$pcode1, c("can", "can", NA, "can"))
  expect_equal(x2$pcode2, c(NA, "can__on", NA, "can__sk"))

  x3 <- separate_hcode(dat1, col = "pcode", into = paste0("pcode", 1:3), remove = TRUE)
  expect_named(x3, c("row", paste0("pcode", 1:3)))

  # test arg extra
  expect_warning(separate_hcode(dat1, col = "pcode", into = paste0("pcode", 1:2), extra = "warn"))
  expect_silent(separate_hcode(dat1, col = "pcode", into = paste0("pcode", 1:2), extra = "drop"))

  # test with input with 0 rows
  dat2 <- data.frame(
    row = integer(0),
    pcode = character(0)
  )

  x4 <- separate_hcode(dat2, col = "pcode", into = paste0("pcode", 1:3))
  expect_named(x4, c("row", "pcode", paste0("pcode", 1:3)))
  expect_equal(nrow(x4), 0L)

})

