test_that("Basic functionality", {

  ## exact join
  m_exact_i <- hmatch_exact(ne_raw, ne_ref, type = "inner")
  expect_is(m_exact_i, "data.frame")
  expect_lte(nrow(m_exact_i), nrow(ne_raw))

  m_exact_l <- hmatch_exact(ne_raw, ne_ref, type = "left")
  expect_equal(nrow(m_exact_l), nrow(ne_raw))

  ## manual join
  ne_ref_code <- ne_ref
  ne_ref_code$pcode <- hcodes_str(ne_ref_code, "adm")

  ne_man <- data.frame(adm0 = NA_character_,
                       adm1 = NA_character_,
                       adm2 = "NJ_Bergen",
                       pcode = "united_states__new_jersey__bergen",
                       stringsAsFactors = FALSE)

  m_manual_i <- hmatch_manual(ne_raw, ne_ref_code, ne_man, code_col = "pcode", type = "inner")
  expect_is(m_manual_i, "data.frame")
  expect_lte(nrow(m_manual_i), nrow(ne_raw))

  m_manual_l <- hmatch_manual(ne_raw, ne_ref_code, ne_man, code_col = "pcode", type = "left")
  expect_equal(nrow(m_manual_l), nrow(ne_raw))

  ## partial join
  m_partial_i <- hmatch_partial(ne_raw, ne_ref, type = "inner")
  expect_is(m_partial_i, "data.frame")
  expect_lte(nrow(m_partial_i), nrow(ne_raw))

  m_partial_l <- hmatch_partial(ne_raw, ne_ref, type = "left")
  expect_gte(nrow(m_partial_l), nrow(ne_raw))

  ## fuzzy join
  m_fuzzy_i <- hmatch_partial(ne_raw, ne_ref, type = "inner", fuzzy = TRUE, max_dist = 2)
  expect_is(m_fuzzy_i, "data.frame")
  expect_lte(nrow(m_fuzzy_i), nrow(ne_raw))

  m_fuzzy_l <- hmatch_partial(ne_raw, ne_ref, type = "left", fuzzy = TRUE, max_dist = 2)
  expect_gte(nrow(m_fuzzy_l), nrow(ne_raw))

  ## rolling join
  m_best_i <- hmatch_best(ne_raw, ne_ref, type = "inner")
  expect_is(m_best_i, "data.frame")
  expect_lte(nrow(m_best_i), nrow(ne_raw))

  m_best_l <- hmatch_best(ne_raw, ne_ref, type = "left")
  expect_gte(nrow(m_best_l), nrow(ne_raw))

  ## composite hmatch (no manual)
  m_comp <- hmatch(ne_raw, ne_ref, fuzzy = TRUE)
  expect_is(m_comp, "data.frame")
  expect_equal(nrow(m_comp), nrow(ne_raw))

  ## composite hmatch (with manual)
  m_comp_man <- hmatch(ne_raw, ne_ref_code, ne_man, code_col = "pcode", fuzzy = TRUE)
  expect_is(m_comp_man, "data.frame")
  expect_equal(nrow(m_comp_man), nrow(ne_raw))

  ## composite hmatch (all match exactly)
  ne_raw_test <- ne_ref_code[,grepl("adm", names(ne_ref_code))]
  m_comp_exact <- hmatch(ne_raw_test, ne_ref_code, ne_man, pattern_raw = "^adm", code_col = "pcode", fuzzy = TRUE)
  expect_equal(nrow(m_comp_exact), nrow(ne_ref))
  expect_true(all(m_comp_exact$match_type == "exact"))
})

