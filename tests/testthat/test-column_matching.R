test_that("Column matching works", {

  drc_ref_r <- setNames(drc_ref, c("level", "geo1", "geo2", "geo3", "geo4"))

  by <- setNames(c("geo1", "geo2", "geo3", "geo4"),
                 c("adm1", "adm2", "adm3", "adm4"))

  ## exact join
  m_exact_0 <- hmatch_exact(drc_raw, drc_ref, type = "inner")

  m_exact_r1 <- hmatch_exact(drc_raw,
                             drc_ref_r,
                             pattern_raw = "^adm",
                             pattern_ref = "^geo",
                             type = "inner")

  m_exact_r2 <- hmatch_exact(drc_raw,
                             drc_ref_r,
                             by = by,
                             type = "inner")

  expect_equal(nrow(m_exact_0), nrow(m_exact_r1))
  expect_equal(nrow(m_exact_0), nrow(m_exact_r2))


  ## manual join
  drc_ref_code <- drc_ref
  drc_ref_code$pcode <- hcodes_str(drc_ref_code, "^adm")
  drc_ref_r$pcode <- hcodes_str(drc_ref_r, pattern = "^geo")

  m_manual_0 <- hmatch_manual(drc_raw,
                              drc_ref_code,
                              drc_man,
                              code_col = "pcode",
                              type = "inner")

  m_manual_r1 <- hmatch_manual(drc_raw,
                               drc_ref_r,
                               drc_man,
                               pattern_raw = "^adm",
                               pattern_ref = "^geo",
                               code_col = "pcode",
                               type = "inner")

  m_manual_r2 <- hmatch_manual(drc_raw,
                               drc_ref_r,
                               drc_man,
                               by = by,
                               code_col = "pcode",
                               type = "inner")

  expect_equal(nrow(m_manual_0), nrow(m_manual_r1))
  expect_equal(nrow(m_manual_0), nrow(m_manual_r2))

})

