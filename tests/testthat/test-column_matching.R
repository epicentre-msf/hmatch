test_that("column matching works as expected", {


  ## test low-level select functions
  cols_by <- c("adm0", "adm1", "adm2")

  x1 <- select_pattern(ne_raw, "adm")
  x2 <- select_by(ne_raw, cols_by)

  expect_equal(x1, cols_by)
  expect_equal(x2, cols_by)

  expect_error(select_pattern(ne_raw, "blah"))
  expect_error(select_by(ne_raw, c("adm1", "blah")))


  ## test select_columns
  x1 <- select_columns(ne_raw, pattern = "adm")
  x2 <- select_columns(ne_raw, by = cols_by)
  x3 <- select_columns(ne_raw, allow_both_null = TRUE)

  expect_equal(x1, cols_by)
  expect_equal(x2, cols_by)
  expect_equal(x3, names(ne_raw))


  ## testing column matching within hmatch()
  cols_by <- c("adm0", "adm1", "adm2")

  m1 <- hmatch(ne_raw, ne_ref, type = "inner")
  m2 <- hmatch(ne_raw, ne_ref, pattern = "^adm", type = "inner")
  m3 <- hmatch(ne_raw, ne_ref, by = cols_by, type = "inner")

  expect_equal(m1, m2)
  expect_equal(m2, m3)

  expect_error(hmatch(ne_raw, ne_ref, pattern = "blah"))
  expect_error(hmatch(ne_raw, ne_ref, by = c("adm1", "blah")))
  expect_warning(hmatch(ne_raw, ne_ref, pattern = "adm", by = cols_by))


  ## testing column matching with different names in raw vs ref
  ne_ref2 <- setNames(ne_ref, c("level", "geo0", "geo1", "geo2", "hcode"))

  by_raw <- c("adm0", "adm1", "adm2")
  by_ref <- c("geo0", "geo1", "geo2")

  m4 <- hmatch(ne_raw, ne_ref2, pattern = "^adm", pattern_ref = "^geo", type = "inner")
  m5 <- hmatch(ne_raw, ne_ref2, by = by_raw, by_ref = by_ref, type = "inner")

  expect_equal(m4, m5)
  expect_error(hmatch(ne_raw, ne_ref2, type = "inner")) # no common colnames


  ## test column matching for manual join
  ne_man <- data.frame(
    adm0 = NA_character_,
    adm1 = NA_character_,
    adm2 = "NJ_Bergen",
    hcode = "211",
    stringsAsFactors = FALSE
  )

  m6 <- hmatch_manual(ne_raw, ne_ref, ne_man, code_col = "hcode")
  m7 <- hmatch_manual(ne_raw, ne_ref2, ne_man, pattern = "adm", pattern_ref = "geo", code_col = "hcode")
  m8 <- hmatch_manual(ne_raw, ne_ref2, ne_man, by = by_raw, by_ref = by_ref, code_col = "hcode")

  expect_equal(m6$hcode, m7$hcode)
  expect_equal(m7$hcode, m8$hcode)
})

