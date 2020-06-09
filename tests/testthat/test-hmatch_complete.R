test_that("hmatch_complete", {

  ## test basic match types
  m_complete_l <- hmatch_complete(ne_raw, ne_ref, type = "left")
  expect_equal(nrow(m_complete_l), nrow(ne_raw))

  m_complete_i <- hmatch_complete(ne_raw, ne_ref, type = "inner")
  expect_lte(nrow(m_complete_i), nrow(ne_raw))
  expect_true(all(!is.na(m_complete_i$hcode)))

  m_complete_a <- hmatch_complete(ne_raw, ne_ref, type = "anti")
  expect_lte(nrow(m_complete_a), nrow(ne_raw))
  expect_equal(names(m_complete_a), names(ne_raw))

  ## test column-matching
  by_ref <- c("adm0", "adm1", "adm2")
  by_raw <- c("adm_country", "adm_state", "adm_county")
  ne_raw_named <- stats::setNames(ne_raw, by_raw)

  m1 <- hmatch_complete(ne_raw_named, ne_ref, pattern = "^adm")
  m2 <- hmatch_complete(ne_raw_named, ne_ref, by = by_raw, by_ref = by_ref)

  expect_identical(m1, m2)


})

