test_that("Basic functionality", {

  ## exact join
  m_exact_i <- hmatch_exact(drc_raw, drc_ref, type = "inner")
  expect_is(m_exact_i, "data.frame")
  expect_lte(nrow(m_exact_i), nrow(drc_raw))

  m_exact_l <- hmatch_exact(drc_raw, drc_ref, type = "left")
  expect_equal(nrow(m_exact_l), nrow(drc_raw))

  ## manual join
  m_manual_i <- hmatch_manual(drc_raw, drc_ref, drc_man, code_col = "pcode", type = "inner")
  expect_is(m_manual_i, "data.frame")
  expect_lte(nrow(m_manual_i), nrow(drc_raw))

  m_manual_l <- hmatch_manual(drc_raw, drc_ref, drc_man, code_col = "pcode", type = "left")
  expect_equal(nrow(m_manual_l), nrow(drc_raw))

  ## partial join
  m_partial_i <- hmatch_partial(drc_raw, drc_ref, type = "inner")
  expect_is(m_partial_i, "data.frame")
  expect_lte(nrow(m_partial_i), nrow(drc_raw))

  m_partial_l <- hmatch_partial(drc_raw, drc_ref, type = "left")
  expect_gte(nrow(m_partial_l), nrow(drc_raw))

  ## fuzzy join
  m_fuzzy_i <- hmatch_partial(drc_raw, drc_ref, type = "inner", fuzzy = TRUE, max_dist = 2)
  expect_is(m_fuzzy_i, "data.frame")
  expect_lte(nrow(m_fuzzy_i), nrow(drc_raw))

  m_fuzzy_l <- hmatch_partial(drc_raw, drc_ref, type = "left", fuzzy = TRUE, max_dist = 2)
  expect_gte(nrow(m_fuzzy_l), nrow(drc_raw))

  ## best join (no manual)
  m_best <- hmatch_best(drc_raw, drc_ref, fuzzy = TRUE)
  expect_is(m_best, "data.frame")
  expect_equal(nrow(m_best), nrow(drc_raw))

  ## best join (with manual)
  m_best_man <- hmatch_best(drc_raw, drc_ref, drc_man, code_col = "pcode", fuzzy = TRUE)
  expect_is(m_best_man, "data.frame")
  expect_equal(nrow(m_best_man), nrow(drc_raw))

})

