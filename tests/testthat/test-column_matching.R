test_that("Column matching works", {

  ne_ref_r <- setNames(ne_ref, c("level", "geo0", "geo1", "geo2"))

  by <- setNames(c("geo0", "geo1", "geo2"),
                 c("adm0", "adm1", "adm2"))

  ## complete join
  m_complete_0 <- hmatch_complete(ne_raw, ne_ref, type = "inner")

  m_complete_r1 <- hmatch_complete(ne_raw,
                                   ne_ref_r,
                                   pattern_raw = "^adm",
                                   pattern_ref = "^geo",
                                   type = "inner")

  m_complete_r2 <- hmatch_complete(ne_raw,
                                   ne_ref_r,
                                   by = by,
                                   type = "inner")

  expect_equal(nrow(m_complete_0), nrow(m_complete_r1))
  expect_equal(nrow(m_complete_0), nrow(m_complete_r2))


  ## manual join
  ne_ref_code <- ne_ref
  ne_ref_code$pcode <- hcodes_str(ne_ref_code, "^adm")
  ne_ref_r$pcode <- hcodes_str(ne_ref_r, pattern = "^geo")

  ne_man <- data.frame(adm0 = NA_character_,
                       adm1 = NA_character_,
                       adm2 = "NJ_Bergen",
                       pcode = "united_states__new_jersey__bergen",
                       stringsAsFactors = FALSE)

  m_manual_0 <- hmatch_manual(ne_raw,
                              ne_ref_code,
                              ne_man,
                              code_col = "pcode",
                              type = "inner")

  m_manual_r1 <- hmatch_manual(ne_raw,
                               ne_ref_r,
                               ne_man,
                               pattern_raw = "^adm",
                               pattern_ref = "^geo",
                               code_col = "pcode",
                               type = "inner")

  m_manual_r2 <- hmatch_manual(ne_raw,
                               ne_ref_r,
                               ne_man,
                               by = by,
                               code_col = "pcode",
                               type = "inner")

  expect_equal(nrow(m_manual_0), nrow(m_manual_r1))
  expect_equal(nrow(m_manual_0), nrow(m_manual_r2))

})

