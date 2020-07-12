test_that("String standardization", {

  # convert raw to lowercase so no complete matches (if no standardization)
  ne_raw_upr <- ne_raw
  ne_raw_upr$adm0 <- toupper(ne_raw_upr$adm0)
  ne_raw_upr$adm1 <- toupper(ne_raw_upr$adm1)
  ne_raw_upr$adm2 <- toupper(ne_raw_upr$adm2)

  # test that no-standardization works (std_fn = NULL)
  # expect no matches if raw date all-uppercase
  m_complete_no_std <- hmatch_complete(ne_raw_upr, ne_ref, type = "inner", std_fn = NULL)
  expect_equal(nrow(m_complete_no_std), 0L)

  m_partial_no_std <- hmatch_partial(ne_raw_upr, ne_ref, type = "inner", std_fn = NULL)
  expect_equal(nrow(m_partial_no_std), 0L)

  m_best_no_std <- hmatch_best(ne_raw_upr, ne_ref, std_fn = NULL)
  expect_true(all(is.na(m_best_no_std$match_type)))

  m_comp_no_std <- hmatch_composite(ne_raw_upr, ne_ref, std_fn = NULL)
  expect_true(all(is.na(m_comp_no_std$match_type)))
})

