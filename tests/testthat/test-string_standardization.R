test_that("String standardization", {


  # convert raw to lowercase so no exact matches (if no standardization)
  drc_raw_upr <- drc_raw
  drc_raw_upr$adm1 <- toupper(drc_raw_upr$adm1)
  drc_raw_upr$adm2 <- toupper(drc_raw_upr$adm2)
  drc_raw_upr$adm3 <- toupper(drc_raw_upr$adm3)
  drc_raw_upr$adm4 <- toupper(drc_raw_upr$adm4)

  # test that no-standardization works (std_fn = NULL)
  # expect no matches if raw date all-uppercase
  m_exact_no_std <- hmatch_exact(drc_raw_upr, drc_ref, type = "inner", std_fn = NULL)
  expect_equal(nrow(m_exact_no_std), 0L)

  m_partial_no_std <- hmatch_partial(drc_raw_upr, drc_ref, type = "inner", std_fn = NULL)
  expect_equal(nrow(m_partial_no_std), 0L)

  m_best_no_std <- hmatch_best(drc_raw_upr, drc_ref, std_fn = NULL)
  expect_true(all(is.na(m_best_no_std$match_type)))

  m_comp_no_std <- hmatch(drc_raw_upr, drc_ref, std_fn = NULL)
  expect_true(all(is.na(m_comp_no_std$match_type)))
})

