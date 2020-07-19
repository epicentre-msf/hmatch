test_that("String standardization", {

  # convert raw to lowercase so no complete matches (if no standardization)
  ne_ref_upr <- ne_ref
  ne_ref_upr$adm0 <- toupper(ne_ref_upr$adm0)
  ne_ref_upr$adm1 <- toupper(ne_ref_upr$adm1)
  ne_ref_upr$adm2 <- toupper(ne_ref_upr$adm2)

  ne_ref_lwr <- ne_ref
  ne_ref_lwr$adm0 <- tolower(ne_ref_lwr$adm0)
  ne_ref_lwr$adm1 <- tolower(ne_ref_lwr$adm1)
  ne_ref_lwr$adm2 <- tolower(ne_ref_lwr$adm2)

  # test that no-standardization works (std_fn = NULL)
  # expect no matches if ref date all-uppercase
  m_complete_no_std <- hmatch(ne_ref_upr, ne_ref_lwr, type = "inner", std_fn = NULL)
  expect_equal(nrow(m_complete_no_std), 0L)

  m_partial_no_std <- hmatch(ne_ref_upr, ne_ref_lwr, type = "inner", std_fn = NULL)
  expect_equal(nrow(m_partial_no_std), 0L)

  m_best_no_std <- hmatch_settle(ne_ref_upr, ne_ref_lwr, std_fn = NULL)
  expect_true(all(is.na(m_best_no_std$ref_hcode)))

  m_comp_no_std <- hmatch_composite(ne_ref_upr, ne_ref_lwr, std_fn = NULL)
  expect_true(all(is.na(m_comp_no_std$ref_hcode)))
})

