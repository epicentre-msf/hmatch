test_that("hmatch_manual works correctly", {


  ne_ref_code <- ne_ref
  ne_ref_code$pcode <- hcodes_str(ne_ref_code, "adm")

  ne_man <- data.frame(
    adm0 = NA_character_,
    adm1 = NA_character_,
    adm2 = "NJ_Bergen",
    pcode = "united_states__new_jersey__bergen",
    stringsAsFactors = FALSE
  )

  m_manual_i <- hmatch_manual(ne_raw, ne_ref_code, ne_man, code_col = "pcode", type = "inner")
  expect_is(m_manual_i, "data.frame")
  expect_lte(nrow(m_manual_i), nrow(ne_raw))

  m_manual_l <- hmatch_manual(ne_raw, ne_ref_code, ne_man, code_col = "pcode", type = "left")
  expect_equal(nrow(m_manual_l), nrow(ne_raw))
})

