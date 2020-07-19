test_that("hmatch_manual works correctly", {


  ne_man <- data.frame(
    adm0 = NA_character_,
    adm1 = NA_character_,
    adm2 = "Bergen, N.J.",
    hcode = "211",
    stringsAsFactors = FALSE
  )

  ## test join types
  mi <- hmatch_manual(ne_raw, ne_ref, ne_man, code_col = "hcode", type = "inner")
  expect_equal(nrow(mi), 1L)

  ml <- hmatch_manual(ne_raw, ne_ref, ne_man, code_col = "hcode", type = "left")
  expect_equal(nrow(ml), nrow(ne_raw))


  ## check works with 0 rows
  m0 <- hmatch_manual(ne_raw, ne_ref, ne_man, code_col = "hcode")

  m1 <- hmatch_manual(ne_raw[0,], ne_ref, ne_man, code_col = "hcode")
  expect_equal(nrow(m1), 0L)
  expect_equal(names(m1), names(m0))

  m2 <- hmatch_manual(ne_raw, ne_ref[0,], ne_man, code_col = "hcode")
  expect_equal(nrow(m2), nrow(m0))
  expect_equal(names(m2), names(m0))

  m3 <- hmatch_manual(ne_raw, ne_ref, ne_man[0,], code_col = "hcode")
  expect_equal(nrow(m3), nrow(m0))
  expect_equal(names(m3), names(m0))
})

